;;; ess-plot.el --- Display ESS plots in a dedicated window -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023-2025 Dennie te Molder

;; Author: Dennie te Molder
;; Created: 30-8-2023
;; Version: 0.2.1
;; URL: https://github.com/DennieTeMolder/ess-plot
;; Package-Requires: ((emacs "26.1") (ess "18.10.1"))
;; Keywords: tools ESS R plot dedicated window

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; When not using `ess-plot-on-startup-h', call M-x `ess-plot-toggle' to start
;; redirecting plots for the current process. Gg-plots should be rendered
;; automatically, but base-R plots require calling M-x `ess-plot-show' or
;; 'dev.flush()' in R to render the plot to the window. Plots are displayed in
;; PNG format thus plot history can be navigated using `image-mode' bindings
;; (i.e. `image-previous-file'). Calling M-x `ess-plot-hide' hides the plot
;; window until a new plot is generated. If the plot window was closed call M-x
;; `ess-plot-show' to re-display the last plot. Calling `ess-plot-toggle' again
;; stops plots from being redirected. You can customise how the plots are
;; displayed by changing `ess-plot-display-function'. To change the plot size
;; and resolution modify options(plot.width, plot.height, plot.res, plot.units)
;; inside of the R process.
;;
;; Current limitations:
;;  - Only implemented for the R dialect (help is welcome for others)
;;
;; Alternatives:
;; - httpgd (R package) & `xwidget-webkit-browse-url': This does provide a more
;;   RStudio like experience, allowing the user to zoom and export plots.
;;   However, in contrast to ESS-plot, this combination requires Emacs to
;;   be compiled with xwidget support and requires manual installation of a
;;   package into the R enviroment.
;;
;; Development:
;; TODO add Emacs cmd to change plot width/height
;;
;;; Code:
(require 'ess-inf)
(require 'filenotify)

;;* Variables
(defvar ess-plot-dir
  (expand-file-name "ess_plot/" temporary-file-directory)
  "Folder for storing plots that are to be displayed in Emacs.
Will be created if it doesn't exist.")

(defvar ess-plot-window-show-on-startup nil
  "Controls whether `ess-plot-start' immediately creates the plot window.
If nil, the plot window will be created when a new plot is rendered.
Also indirectly affects `ess-plot-toggle' and `ess-plot-on-startup-h'.")

(defvar ess-plot-display-function #'ess-plot-display-default
  "Function used to display new plot buffers.
The function should take the buffer to display as the only argument and
should return the window used to display the buffer. Example values
include: `ess-plot-display-default' or `display-buffer'.")

(defvar ess-plot-placeholder-name "*R plot*"
  "Name of the placeholder plot buffer.")

(defvar ess-plot-buffer-modes '(image-mode dired-mode)
  "Modes used by `ess-plot-buffer-p' to discern buffers managed by ess-plot.")

(defvar ess-plot-mask-functions-p t
  "Whether to mask specific R functions for enhanced UX.
With this enabled you can call the built-in functions dev.flush() in
place of .ess_plot_show() and options() in place of .ess_plot_options(),
which enables you to control ess-plot using code that can be run by non
ess-plot users. Furthermore, this setting also enables automatic
rendering of ggplots and synchronises plot dimensions with ggsave() and
png/jpeg/bmp/tiff/svg(). Dimensions for pdf() are always synchronised
when using .ess_plot_options().")

(defvar ess-plot--source-dir
  (file-name-directory (file-truename (or load-file-name buffer-file-name)))
  "Source directory containing ess-plot.el(c) and the dir/ folder.")

;; NOTE In Emacs29+ `file-notify-descriptors' is cleared when the dir is deleted
(defvar ess-plot--descriptor nil
  "File notify descriptor watching `ess-plot-dir'.")

(defvar ess-plot--file-last nil
  "Most recent ESS plot file.")

;;* Predicate functions
(defun ess-plot-loaded-p ()
  "Non-nil if ESSR_plot is attached to `ess-local-process-name'."
  (and (ess-process-live-p)
       (string= "R" (ess-get-process-variable 'ess-dialect))
       (ess-boolean-command "'ESSR_plot' %in% search()\n")))

(defun ess-plot-file-p (file)
  "Return non-nil if FILE is an ESS plot."
  (when ess-plot-dir (string-prefix-p ess-plot-dir file)))

(defun ess-plot-buffer-p (&optional buf)
  "Return BUF if it displays an ESS plot. Defaults to `current-buffer'."
  (with-current-buffer (or buf (current-buffer))
    (when (or (string= (buffer-name) ess-plot-placeholder-name)
              (and default-directory
                   (string= ess-plot-dir default-directory)
                   (derived-mode-p ess-plot-buffer-modes)))
      (current-buffer))))

;;* Buffer management
(defun ess-plot--placeholder ()
  "Return the placeholder buffer based on `ess-plot-placeholder-name'."
  (with-current-buffer (get-buffer-create ess-plot-placeholder-name)
    (setq-local default-directory ess-plot-dir)
    (current-buffer)))

(defun ess-plot-buffers ()
  "Return a list of buffers associated with an ESS plot."
  (let (plot-bufs)
    (dolist (buf (buffer-list) plot-bufs)
      (when (ess-plot-buffer-p buf)
        (push buf plot-bufs)))))

(defun ess-plot-cleanup-buffers (&optional kill-visible)
  "Kill all unmodified buffers dedicated to ESS plot files.
The visible plot buffers are only killed if KILL-VISIBLE is t."
  (dolist (buf (ess-plot-buffers))
    (let ((win (get-buffer-window buf)))
      (and win kill-visible (delete-window win))
      (unless (or (buffer-modified-p buf)
                  (and win (not kill-visible)))
        (kill-buffer buf)))))

(defun ess-plot-visible-process-buffer ()
  "Return the first visible ESS process buffer or nil."
  (cl-some (lambda (win) (with-selected-window win
                           (when-let ((buf (ess-get-current-process-buffer)))
                             (get-buffer-window buf))))
           (window-list-1 nil 'ignore-minibuffer 'visible)))

;;* Window management
(defun ess-plot--window-search-list (&optional frame)
  "Return the non-minibuffer windows of all visible frames in a consistent order.
The list always starts with the first window of FRAME.
Defaults to the first visible frame."
  (let ((first-window (frame-root-window (or frame (car (visible-frame-list))))))
    (while (and first-window (not (window-live-p first-window)))
      (setq first-window (window-child first-window)))
    (window-list-1 first-window 'ignore-minibuffer 'visible)))

(defun ess-plot-window ()
  "Return the first window currently displaying ESS plots."
  (cl-some (lambda (win) (and (ess-plot-buffer-p (window-buffer win)) win))
           (ess-plot--window-search-list)))

(defun ess-plot-display-default (buf)
  "Display BUF in `ess-plot-window', else split `ess-plot-visible-process-buffer'.
If both are nil `display-buffer' is used as fallback."
  (let ((win (ess-plot-window)))
    (and (not win)
         (setq win (ess-plot-visible-process-buffer))
         (if (caar (window--subtree (window-parent win)))
             (setq win (split-window-right nil win))
           (setq win (split-window-below nil win))))
    (if win
        (with-selected-window win
          (get-buffer-window (pop-to-buffer-same-window buf)))
      (display-buffer buf))))

(defun ess-plot--display (buf)
  "Call `ess-plot-display-function' and `image-transform-fit-both' on BUF."
  (prog1 (funcall ess-plot-display-function buf)
    (when (fboundp 'image-transform-fit-both)
      (with-current-buffer buf
        (when (eq major-mode 'image-mode)
          (image-transform-fit-both))))))

(defun ess-plot--show-last (&optional show-placeholder)
  "Display `ess-plot--file-last' in `ess-plot-window' creating it if needed.
If SHOW-PLACEHOLDER is non-nil, `ess-plot--placeholder' is shown if
`ess-plot--file-last' is nil."
  (if ess-plot--file-last
      (ess-plot--display (find-file-noselect ess-plot--file-last))
    (when show-placeholder
      (ess-plot--display (ess-plot--placeholder)))))

;;* File watcher
(defun ess-plot--file-notify-open (event)
  "Display the .png file created by EVENT in `ess-plot-window'."
  (when (and (eq 'created (nth 1 event))
             (string= (file-name-extension (nth 2 event)) "png"))
    (ess-plot--display (find-file-noselect (nth 2 event)))
    (setq ess-plot--file-last (nth 2 event))
    (when (ess-plot-cleanup-buffers)
      (message "ESS-plot: updated plot"))))

(defun ess-plot--watch-dir (dir)
  "Call `file-notify-add-watch' for change on DIR w/ `ess-plot--file-notify-open'."
  (file-notify-add-watch (file-name-as-directory dir)
                         '(change)
                         #'ess-plot--file-notify-open))

(defun ess-plot--watcher-start ()
  "Start the file watcher for `ess-plot-dir' that will display new plots."
  (unless ess-plot-dir
    (error "`ess-plot-dir' is unset"))
  (unless ess-plot--descriptor
    (make-directory ess-plot-dir t)
    (setq ess-plot--descriptor (ess-plot--watch-dir ess-plot-dir))))

(defun ess-plot--watcher-stop ()
  "Stop file watcher corresponding to `ess-plot--descriptor' and `ess-plot-dir'."
  (when ess-plot--descriptor
    (file-notify-rm-watch ess-plot--descriptor)
    (setq ess-plot--descriptor nil))
  (ess-plot-cleanup-buffers 'kill-visible))

;;* State management
(defun ess-plot--kill-buffer-h ()
  "Call `ess-plot--watcher-stop' if no other buffers have this hook.
Intended for `kill-buffer-hook'."
  (when ess-plot--descriptor
    (unless (cl-some (lambda (buf)
                       (memq 'ess-plot--kill-buffer-h
                             (buffer-local-value 'kill-buffer-hook buf)))
                     (remq (current-buffer) (buffer-list)))
      (ess-plot--watcher-stop))))

;; REVIEW Can we add remote support like in `ess-r-load-ESSR'?
(defun ess-plot--load ()
  "Attach ess-plot to `ess-local-process-name' and start redirecting plots."
  (unless (ess-plot-loaded-p)
    (unless (string= "R" (ess-get-process-variable 'ess-dialect))
      (error "ESS-plot currently only supports the 'R' dialect"))
    (let ((cmd (format (concat "options(ess_plot.mask_functions=%s)\n"
                               "local(source('%s', local=TRUE))\n")
                       (if (ess-get-process-variable 'ess-plot-mask-functions-p)
                           "TRUE" "FALSE")
                       (expand-file-name "dir/ess-plot.R" ess-plot--source-dir))))
      (ess-eval-linewise cmd "Attaching ESS-plot functions" nil nil 'wait-last-prompt))
    (unless (ess-plot-loaded-p)
      (error "ESS-plot: failed to load R code into process: %s"
             ess-local-process-name))))

(defun ess-plot--unload ()
  "Detach ess-plot from `ess-local-process-name' and stop redirecting plots."
  (when (ess-plot-loaded-p)
    (ess-eval-linewise ".ess_plot_env_teardown(detach = TRUE)\n"
                       "Detaching ESS-plot functions" nil nil 'wait-last-prompt)
    (when (ess-plot-loaded-p)
      (user-error "ESS-plot: failed to unload R code from process: %s"
                  ess-local-process-name))))

;;* User facing functions
;;;###autoload
(defun ess-plot-start ()
  "Start displaying plots inside of Emacs for `ess-local-process-name'."
  (interactive)
  (ess-force-buffer-current)
  (ess-plot--load)
  (ess-plot--watcher-start)
  (with-current-buffer (ess-get-current-process-buffer)
    (add-hook 'kill-buffer-hook #'ess-plot--kill-buffer-h nil 'local))
  (ess-eval-linewise (format ".ess_plot_start('%s')\n" ess-plot-dir)
                     "Redirecting plots to Emacs")
  (when ess-plot-window-show-on-startup
    (ess-plot--show-last 'show-placeholder))
  (message "ESS-plot: started displaying plots")
  ess-plot-dir)

(defun ess-plot-stop ()
  "Stop displaying plots inside of Emacs for `ess-local-process-name'."
  (interactive)
  (ess-force-buffer-current)
  (ess-plot--unload)
  (message "ESS-plot: stopped displaying plots"))

;;;###autoload
(defun ess-plot-toggle ()
  "Toggle displaying ESS plots inside of Emacs."
  (interactive)
  (ess-force-buffer-current)
  (if (ess-plot-loaded-p)
      (ess-plot-stop)
    (ess-plot-start)))

;;;###autoload
(defun ess-plot-on-startup-h ()
  "Hook function for `ess-r-post-run-hook' to start plot redirection to Emacs."
  (when ess-plot-dir
    (ess-plot-start)))

;;;###autoload
(defun ess-plot-show ()
  "Force the current plot to be drawn on screen."
  (interactive)
  (ess-force-buffer-current)
  (unless (ess-plot-loaded-p)
    (user-error "ESS-plot: not loaded in process '%s', call M-x ess-plot-toggle"
                ess-local-process-name))
  (ess-eval-linewise ".ess_plot_show()\n" "M-x ess-plot-show" nil nil 'wait-last-prompt)
  (unless (ess-plot--show-last)
    (user-error "ESS-plot: no plots to display")))

;;;###autoload
(defun ess-plot-hide ()
  "Hide the current plot window by killing it, also cleans-up all plot buffers."
  (interactive)
  (ess-plot-cleanup-buffers 'kill-visible))

;;;###autoload
(defun ess-plot-window-here (&optional window no-kill)
  "Turn WINDOW into an `ess-plot-window'. Defaults to selected window.
Unless NO-KILL is non-nil the other plot windows are killed."
  (interactive)
  (unless no-kill
    (ess-plot-cleanup-buffers 'kill-visible))
  (with-selected-window (or window (selected-window))
    (let ((ess-plot-display-function #'pop-to-buffer-same-window))
      (ess-plot--show-last 'show-placeholder))))

(provide 'ess-plot)

;;; ess-plot.el ends here
