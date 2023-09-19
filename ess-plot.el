;;; ess-plot.el --- Display ESS plots in a dedicated window -*- lexical-binding: t; -*-
;;;
;; Copyright (C) 2023  Dennie te Molder

;; Author: Dennie te Molder
;; Created: 30-8-2023
;; Version: 0.1.0
;; URL: https://github.com/DennieTeMolder/ess-plot
;; Package-Requires: ((emacs "26.1") (ess "18.10.1"))
;; Keywords: tools ESS frames plot

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
;; Call M-x `ess-plot-toggle' to start redirecting plots.
;; Gg-plots should be redirected automatically, base-R plots require calling
;; 'dev.flush()' to push the plot to the window. If the plot window was closed
;; call M-x `ess-plot-show' to redisplay the last plot. Plots are displayed in
;; PNG format thus plot history can be navigated using `image-mode' bindings
;; (i.e. `image-previous-file').
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
;; TODO handle `inferior-ess-reload'
;; TODO add Emacs cmd to change plot width/height
;;
;;; Code:
(require 'ess-inf)
(require 'filenotify)

(defvar ess-plot-window-create-function #'ess-plot-window-create-default
  "Function used to create the plot window if none is visible.
The `selected-window' after calling this function is used to open plot files.")

(defvar ess-plot-placeholder-name "*R plot*"
  "Name of the placeholder plot buffer.")

(defvar ess-plot--source-dir
  (file-name-directory (file-truename (or load-file-name buffer-file-name)))
  "Directory containing ess-plot.el(c) and other source code.")

(defvar ess-plot--process-name nil
  "ESS process for which plots are currently being displayed.")

(defvar ess-plot--descriptor nil
  "File notify descriptor watching the plot folder.")

;; NOTE In Emacs29+ `file-notify-descriptors' is cleared when the dir is deleted
(defvar ess-plot--dir nil
  "Folder being watched by `ess-plot--descriptor'.")

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
      (file-name-as-directory (car (ess-get-words-from-vector ".ESS_PLOT_DIR.\n"))))))

(defun ess-plot-file-p (file)
  "Return non-nil if FILE is an ESS plot."
  (when ess-plot--dir (file-in-directory-p file ess-plot--dir)))

(defun ess-plot-buffer-p (&optional buf)
  "Return BUF if it displays an ESS plot. Defaults to `current-buffer'."
  (or buf (setq buf (current-buffer)))
  (when-let ((file (buffer-file-name buf)))
    (when (ess-plot-file-p file) buf)))

(defun ess-plot-buffers ()
  "Return a list of buffers associated with an ESS plot."
  (delq nil (cons (get-buffer ess-plot-placeholder-name)
                  (mapcar #'ess-plot-buffer-p (buffer-list)))))

(defun ess-plot-file-last ()
  "The most recent plot outputted by `ess-plot-process-name."
  (when (ess-plot-loaded-p)
    (let ((ess-local-process-name ess-plot--process-name))
      (car (ess-get-words-from-vector ".ess_plot_file_last()\n")))))

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
  "Use `ess-plot-split-window-rational' to split `ess-switch-to-ESS'."
  (unless (eq (ess-get-process-buffer ess-plot--process-name)
              (current-buffer))
    (ess-switch-to-ESS nil))
  (select-window (ess-plot-split-window-rational)))

(defun ess-plot--window-force ()
  "Return window for displaying ESS plot files, create if it does not exist."
  (or (ess-plot-window)
      (save-selected-window
        (funcall ess-plot-window-create-function)
        (if-let ((last-plot (ess-plot-file-last)))
            (find-file last-plot)
          (switch-to-buffer (generate-new-buffer ess-plot-placeholder-name))
          (setq-local default-directory ess-plot--dir))
        (selected-window))))

(defun ess-plot-cleanup-buffers (&optional kill-visible)
  "Kill all unmodified buffers dedicated to ESS plot files.
Only kill visible plot buffers if KILL-VISIBLE is t."
  (when-let ((bufs (ess-plot-buffers)))
    (mapc (lambda (buf)
            (unless (buffer-modified-p buf)
              (if-let ((win (get-buffer-window buf)))
                  (when kill-visible
                    (delete-window win)
                    (kill-buffer buf))
                (kill-buffer buf))))
          bufs)))

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

(defun ess-plot-sync ()
  "Synchronise `ess-plot--dir' with `ess-plot-process-dir'."
  (let ((proc-dir (ess-plot-process-dir)))
    (if (and proc-dir ess-plot--dir (string= proc-dir ess-plot--dir))
        (progn (ess-plot-cleanup-buffers) ess-plot--dir)
      (when ess-plot--descriptor
        (ess-plot-cleanup-buffers 'kill-visible)
        (file-notify-rm-watch ess-plot--descriptor)
        (setq ess-plot--descriptor nil
              ess-plot--dir nil))
      (when proc-dir
        (setq ess-plot--descriptor (ess-plot--watch-dir proc-dir)
              ess-plot--dir proc-dir)))))

;; REVIEW Can we add remote support like in `ess-r-load-ESSR'?
(defun ess-plot--load ()
  "Load the ESSR-plot functions into the current process."
  (unless (ess-plot-loaded-p)
    (ess-force-buffer-current)
    (unless (string= "R" (ess-get-process-variable 'ess-dialect))
      (user-error "ESS-plot currently only supports the 'R' dialect"))
    (let* ((r-src (expand-file-name "etc/ess-plot.R" ess-plot--source-dir))
           (cmd (format "local(source('%s', local = new.env()))\n" r-src)))
      (ess-eval-linewise cmd "Attaching ESS-plot functions" nil nil 'wait-last-prompt)
      (unless (ess-plot-loaded-p ess-current-process-name)
        (user-error "ESS-plot: failed to load R code into process: %s"
                    ess-current-process-name))
      (setq ess-plot--process-name ess-current-process-name)))
  (ess-plot-sync)
  (ess-plot--window-force)
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

;;;###autoload
(defun ess-plot-toggle ()
  "Toggle displaying ESS plots."
  (interactive)
  (if (not ess-plot--process-name)
      (progn (ess-plot--load)
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
  (ess-plot--window-force))

(provide 'ess-plot)

;;; ess-plot.el ends here
