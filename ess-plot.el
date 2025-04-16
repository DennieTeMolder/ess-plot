;;; ess-plot.el --- Display ESS plots in a dedicated window -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023-2025 Dennie te Molder

;; Author: Dennie te Molder
;; Created: 30-8-2023
;; Version: 0.2.0
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
;; NOTE In Emacs29+ `file-notify-descriptors' is cleared when the dir is deleted
(defvar ess-plot-dir
  (expand-file-name "ess_plot/" temporary-file-directory)
  "Folder for storing plots that are to be displayed.
Will be created it it doesn't exist.")

(defvar ess-plot-window-show-on-startup nil
  "Controls weather `ess-plot-toggle' will trigger `ess-plot-show'.")

(defvar ess-plot-display-function #'ess-plot-display-default
  "Function used to display new plots. See `ess-plot-display-default'.")

(defvar ess-plot-placeholder-name "*R plot*"
  "Name of the placeholder plot buffer.")

(defvar ess-plot-buffer-modes '(image-mode dired-mode)
  "Modes used by `ess-plot-buffer-p' to discern buffers managed by ess-plot.")

(defvar ess-plot--source-dir
  (file-name-directory (file-truename (or load-file-name buffer-file-name)))
  "Directory containing ess-plot.el(c) and the dir/ folder.")

(defvar ess-plot--descriptor nil
  "File notify descriptor watching the plot folder.")

(defvar ess-plot--file-last nil
  "Most recent ESS plot file.")

;;* Predicate functions
(defun ess-plot-loaded-p ()
  "Non-nil if ESSR_plot is attached to `ess-local-process-name'."
  (and (ess-process-live-p)
       (ess-boolean-command "'ESSR_plot' %in% search()\n")))

(defun ess-plot-file-p (file)
  "Return non-nil if FILE is an ESS plot."
  (when ess-plot-dir (string-prefix-p ess-plot-dir file)))

(defun ess-plot-buffer-p (&optional buf)
  "Return BUF if it displays an ESS plot. Defaults to `current-buffer'."
  (with-current-buffer (or buf (current-buffer))
    (and default-directory
         (cl-some #'derived-mode-p ess-plot-buffer-modes)
         (equal ess-plot-dir (file-truename default-directory)))))

;;* Buffer management
(defun ess-plot-buffers ()
  "Return a list of buffers associated with an ESS plot."
  (let (plot-bufs)
    (dolist (buf (buffer-list))
      (when (ess-plot-buffer-p buf)
        (push buf plot-bufs)))
    (when-let ((placeholder (get-buffer ess-plot-placeholder-name)))
      (cl-pushnew placeholder plot-bufs))
    plot-bufs))

(defun ess-plot-cleanup-buffers (&optional kill-visible)
  "Kill all unmodified buffers dedicated to ESS plot files.
The visible plot buffers are only killed if KILL-VISIBLE is t."
  (dolist (buf (ess-plot-buffers))
    (let ((win (get-buffer-window buf)))
      (and win kill-visible (delete-window win))
      (unless (or (buffer-modified-p buf)
                  (and win (not kill-visible)))
        (kill-buffer buf)))))

;;* Window management
(defun ess-plot-window ()
  "Return the window currently displaying ESS plots."
  (cl-some #'get-buffer-window (ess-plot-buffers)))

(defun ess-plot-visible-process-buffer ()
  "Return the first visible ESS process buffer or nil."
  (cl-some (lambda (win) (with-selected-window win
                           (when-let ((buf (ess-get-current-process-buffer)))
                             (get-buffer-window buf))))
           (window-list-1 nil 'ignore-minibuffer (selected-frame))))

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

(defun ess-plot--placeholder ()
  "Return the placeholder buffer based on `ess-plot-placeholder-name'."
  (with-current-buffer (get-buffer-create ess-plot-placeholder-name)
    (setq-local default-directory ess-plot-dir)
    (current-buffer)))

(defun ess-plot--show-last ()
  "Display `ess-plot--file-last' in `ess-plot-window' creating it if needed."
  (when ess-plot--file-last
    (funcall ess-plot-display-function
             (find-file-noselect ess-plot--file-last))))

;;* File watcher
(defun ess-plot--file-notify-open (event)
  "Display the .png file created by EVENT in `ess-plot-window'."
  (when (and (eq 'created (nth 1 event))
             (string= (file-name-extension (nth 2 event)) "png"))
    (funcall ess-plot-display-function
             (find-file-noselect (nth 2 event)))
    (setq ess-plot--file-last (nth 2 event))
    (when (ess-plot-cleanup-buffers)
      (message "ESS-plot: updated plot"))))

(defun ess-plot--watch-dir (dir)
  "Call `file-notify-add-watch' for change on DIR w/ `ess-plot--file-notify-open'."
  (file-notify-add-watch (file-name-as-directory dir)
                         '(change)
                         #'ess-plot--file-notify-open))

(defun ess-plot-watcher-start ()
  "Start the file watcher for `ess-plot-dir' that will display new plots."
  (interactive)
  (unless ess-plot--descriptor
    (make-directory ess-plot-dir t)
    (setq ess-plot--descriptor (ess-plot--watch-dir ess-plot-dir))))

(defun ess-plot-watcher-stop ()
  "Stop file watcher corresponding to `ess-plot--descriptor'."
  (interactive)
  (when ess-plot--descriptor
    (file-notify-rm-watch ess-plot--descriptor)
    (setq ess-plot--descriptor nil))
  (ess-plot-cleanup-buffers 'kill-visible))

;;* State management
;; REVIEW Can we add remote support like in `ess-r-load-ESSR'?
(defun ess-plot--load ()
  "Attatch ess-plot to `ess-local-process-name' and start redirecting plots."
  (unless ess-plot-dir
    (error "`ess-plot-dir' is unset."))
  (unless (string= "R" (ess-get-process-variable 'ess-dialect))
    (error "ESS-plot currently only supports the 'R' dialect"))
  (unless (ess-plot-loaded-p)
    (let* ((r-src (expand-file-name "dir/ess-plot.R" ess-plot--source-dir))
           (cmd (format "local(source('%s', local = TRUE))\n" r-src)))
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
  (ess-plot-watcher-start)
  (ess-plot--load)
  (ess-eval-linewise (format ".ess_plot_start('%s')\n" ess-plot-dir)
                     "Redirecting plots to Emacs")
  (when ess-plot-window-show-on-startup
    (or (ess-plot--show-last)
        (funcall ess-plot-display-function
                 (ess-plot--placeholder))))
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
  "Toggle displaying ESS plots inside of Emacs.
If STARTUP is non-nil plotting will never be deactivate."
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
  (ess-eval-linewise "dev.flush()\n" nil nil nil 'wait-last-prompt)
  (unless (ess-plot--show-last)
    (user-error "ESS-plot: no plots to display")))

;;;###autoload
(defun ess-plot-hide ()
  "Hide the current plot window by killing it, also cleans-up all plot buffers."
  (interactive)
  (ess-plot-cleanup-buffers 'kill-visible))

(provide 'ess-plot)

;;; ess-plot.el ends here
