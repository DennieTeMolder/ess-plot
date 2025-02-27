;;; ess-plot.el --- Display ESS plots in a dedicated window -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023-2025 Dennie te Molder

;; Author: Dennie te Molder
;; Created: 30-8-2023
;; Version: 0.1.2
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
;; Call M-x `ess-plot-toggle' to start redirecting plots. Gg-plots should be
;; rendered automatically, but base-R plots require calling 'dev.flush()' in R
;; to render the plot to the window. If the plot window was closed call M-x
;; `ess-plot-show' to redisplay the last plot. Plots are displayed in PNG format
;; thus plot history can be navigated using `image-mode' bindings (i.e.
;; `image-previous-file'). Calling `ess-plot-toggle' again stops plots from
;; being redirected and closes the plot window. You can customize how the plot
;; window is created and positioned by changing `ess-plot-window-create-function'.
;;
;; Call M-x `ess-plot-toggle' to start redirecting plots. Gg-plots should be
;; rendered automatically, but base-R plots require calling either M-x
;; `ess-plot-show' in Emacs or 'dev.flush()' in R to render the plot to the
;; window. If the plot window was closed, calling M-x `ess-plot-show' will cause
;; the latest plot to be redisplayed. Plots are displayed in PNG format thus
;; plot history can be navigated using `image-mode' bindings (i.e. M-x
;; `image-previous-file'). Calling M-x `ess-plot-toggle' again stops plots from
;; being redirected and closes the plot window. It is recommended to create
;; bindings for `ess-plot-toggle', `ess-plot-show', and optionally
;; `ess-plot-hide'.
;;
;; Current limitations:
;;  - After reloading the process the user needs to call `ess-plot-toggle'
;;    twice to restore functionality
;;  - Can only be active for one process at a time
;;  - Only implemented for the R dialect (help is welcome for others)
;;
;; Alternatives:
;; - httpgd (R package) & `xwidget-webkit-browse-url': This does provide a more
;;   RStudio like experience, allowing the user to zoom and export plots.
;;   However, in contrast to ESS-plot, this combination requires Emacs to
;;   be compiled with xwidget support and forces the user to install additional
;;   packages into their environment.
;;
;; Development:
;; TODO coordinate file naming through Emacs
;; TODO multi-process support
;; TODO attach on startup
;; TODO handle `inferior-ess-reload'
;; TODO add Emacs cmd to change plot width/height
;;
;;; Code:
(require 'ess-inf)
(require 'filenotify)

;;* Variables
(defvar ess-plot-window-show-on-startup t
  "Controls weather `ess-plot-toggle' will trigger `ess-plot-show'.")

(defvar ess-plot-window-create-function #'ess-plot-window-create-default
  "Function used to create the plot window if none is visible.
The `selected-window' after calling this function is used to open plot files.")

(defvar ess-plot-placeholder-name "*R plot*"
  "Name of the placeholder plot buffer.")

(defvar ess-plot-buffer-modes '(image-mode dired-mode)
  "Modes used by `ess-plot-buffer-p' to discern buffers managed by ess-plot.")

(defvar ess-plot--source-dir
  (file-name-directory (file-truename (or load-file-name buffer-file-name)))
  "Directory containing ess-plot.el(c) and the dir/ folder.")

(defvar ess-plot--process-name nil
  "ESS process for which plots are currently being displayed.")

(defvar ess-plot--descriptor nil
  "File notify descriptor watching the plot folder.")

;; NOTE In Emacs29+ `file-notify-descriptors' is cleared when the dir is deleted
(defvar ess-plot--dir nil
  "Folder being watched by `ess-plot--descriptor'.")

;;* Predicate functions
(defun ess-plot-loaded-p (&optional proc-name)
  "Non-nil if ESSR_plot is attached to PROC-NAME.
Defaults to `ess-plot--process-name'."
  (or proc-name (setq proc-name ess-plot--process-name))
  (when-let ((ess-local-process-name proc-name))
    (when (ess-process-live-p)
      (ess-boolean-command "'ESSR_plot' %in% search()\n"))))

(defun ess-plot-current-p ()
  "Non-nil if the dev.cur() of `ess-plot--process-name' is ESSR-plot."
  (when (ess-plot-loaded-p)
    (let ((ess-local-process-name ess-plot--process-name))
      (ess-boolean-command ".ess_plot_is_current()\n"))))

(defun ess-plot-process-dir ()
  "Directory targeted by `ess-plot--process-name' to output plots."
  (when (ess-plot-loaded-p)
    (let ((ess-local-process-name ess-plot--process-name))
      (file-truename (file-name-as-directory
                      (car (ess-get-words-from-vector ".ESS_PLOT_DIR.\n")))))))

(defun ess-plot-file-p (file)
  "Return non-nil if FILE is an ESS plot."
  (when ess-plot--dir (string-prefix-p ess-plot--dir file)))

(defun ess-plot-buffer-p (&optional buf)
  "Return BUF if it displays an ESS plot. Defaults to `current-buffer'."
  (with-current-buffer (or buf (current-buffer))
    (and default-directory
         (equal ess-plot--dir (file-truename default-directory))
         (cl-some #'derived-mode-p ess-plot-buffer-modes))))

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

;;;###autoload
(defun ess-plot-split-window-rational (&optional window size w/h-ratio pixelwise)
  "Split WINDOW based on width to height ratio (including margins/fringes/bars).
When W/H is lower then W/H-RATIO split below, else split right.
WINDOW, SIZE, and PIXELWISE are passed on to `split-window'"
  (interactive)
  (or w/h-ratio (setq w/h-ratio 1.5))
  (let ((side (if (< (window-pixel-width) (* w/h-ratio (window-pixel-height)))
                  'below 'right)))
    (funcall (if (called-interactively-p 'any) #'select-window #'identity)
             (split-window window size side pixelwise))))

(defun ess-plot-window-create-default ()
  "Use `ess-plot-split-window-rational' on the window of `ess-plot--process-name'.
If it is not visible split the current window instead."
  (when-let ((is-ess-proc (assoc ess-plot--process-name ess-process-name-list))
             (win (get-buffer-window (ess-get-process-buffer ess-plot--process-name))))
    (select-window win))
  (select-window (ess-plot-split-window-rational)))

(defun ess-plot--window-force ()
  "Return window for displaying ESS plot files, create if it does not exist."
  (or (ess-plot-window)
      (save-selected-window
        (funcall ess-plot-window-create-function)
        (switch-to-buffer (get-buffer-create ess-plot-placeholder-name))
        (setq-local default-directory ess-plot--dir)
        (selected-window))))

(defun ess-plot-file-last ()
  "The most recent plot outputted by `ess-plot-process-name."
  (when (ess-plot-loaded-p)
    (let ((ess-local-process-name ess-plot--process-name))
      (car (ess-get-words-from-vector ".ess_plot_file_last()\n")))))

(defun ess-plot--show-last ()
  "Display `ess-plot-file-last' in `ess-plot-window' creating it if needed."
  (when-let ((last-plot (ess-plot-file-last)))
    (save-selected-window
      (select-window (ess-plot--window-force))
      (find-file last-plot))))

;;* File watcher
(defun ess-plot--file-notify-open (event)
  "Display the .png file created by EVENT in `ess-plot-window'."
  (when (and (eq 'created (nth 1 event))
             (string= (file-name-extension (nth 2 event)) "png"))
    (save-selected-window
      (select-window (ess-plot--window-force))
      (find-file (nth 2 event)))
    (when (ess-plot-cleanup-buffers)
      (message "ESS-plot: updated plot"))))

(defun ess-plot--watch-dir (dir)
  "Call `file-notify-add-watch' for change on DIR w/ `ess-plot--file-notify-open'."
  (file-notify-add-watch (file-name-as-directory dir)
                         '(change)
                         #'ess-plot--file-notify-open))

;;* State management
(defun ess-plot-sync ()
  "Synchronise `ess-plot--dir' with `ess-plot-process-dir'."
  (let ((proc-dir (ess-plot-process-dir)))
    (if (and proc-dir ess-plot--dir (string= proc-dir ess-plot--dir))
        (ess-plot-cleanup-buffers)
      (when ess-plot--descriptor
        (ess-plot-cleanup-buffers 'kill-visible)
        (file-notify-rm-watch ess-plot--descriptor)
        (setq ess-plot--descriptor nil
              ess-plot--dir nil))
      (when proc-dir
        (setq ess-plot--descriptor (ess-plot--watch-dir proc-dir)
              ess-plot--dir proc-dir)))
    ess-plot--dir))

;; REVIEW Can we add remote support like in `ess-r-load-ESSR'?
(defun ess-plot--load ()
  "Load the ESSR-plot functions into the current process."
  (unless (ess-plot-loaded-p)
    (ess-force-buffer-current)
    (unless (string= "R" (ess-get-process-variable 'ess-dialect))
      (user-error "ESS-plot currently only supports the 'R' dialect"))
    (let* ((r-src (expand-file-name "dir/ess-plot.R" ess-plot--source-dir))
           (cmd (format "local(source('%s', local = TRUE))\n" r-src)))
      (ess-eval-linewise cmd "Attaching ESS-plot functions" nil nil 'wait-last-prompt)
      (unless (ess-plot-loaded-p ess-current-process-name)
        (user-error "ESS-plot: failed to load R code into process: %s"
                    ess-current-process-name))
      (setq ess-plot--process-name ess-current-process-name)))
  (ess-plot-sync)
  ess-plot--process-name)

(defun ess-plot--unload ()
  "Unload the ESSR-plot functions from the current process."
  (when (ess-plot-loaded-p)
    (let ((ess-local-process-name ess-plot--process-name))
      (ess-eval-linewise ".ess_plot_env_teardown(detach = TRUE)\n"
                         "Detaching ESS-plot functions" nil nil 'wait-last-prompt))
    (when (ess-plot-loaded-p)
      (user-error "ESS-plot: failed to unload R code from process: %s"
                  ess-plot--process-name)))
  (setq ess-plot--process-name nil)
  (ess-plot-sync))

;;* User facing functions
;;;###autoload
(defun ess-plot-toggle ()
  "Toggle displaying ESS plots."
  (interactive)
  (if (not ess-plot--process-name)
      (progn
        (ess-plot--load)
        (when ess-plot-window-show-on-startup
          (unless (ess-plot--show-last) (ess-plot--window-force)))
        (message "ESS-plot: started displaying plots"))
    (ess-plot--unload)
    (message "ESS-plot: stopped displaying plots")))

;;;###autoload
(defun ess-plot-show ()
  "Force the current plot to be drawn on screen."
  (interactive)
  (ess-force-buffer-current)
  (unless (ess-plot-loaded-p ess-current-process-name)
    (user-error "ESS-plot: not loaded in process '%s', call M-x ess-plot-toggle"
                ess-current-process-name))
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
