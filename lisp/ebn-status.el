;;; ebn-status --- Status and system monitor for Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar ebn/status--buffer "*status*")
(defvar ebn/status--timer nil)
(defvar ebn/status--on nil)

(defvar ebn/status--week-days
  [ "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" ])

(defun ebn/status--time->date-string (time)
  (format "%s, %s"
	  (elt ebn/status--week-days (string-to-number (format-time-string "%u" time)))
	  (format-time-string "%B %d, %Y" time)))

(defcustom ebn/status-timer-interval 60
  "How often should the time be updates"
  :group 'ebn/status)

(defface ebn/status-current-time-face
  '((t (:foreground "#7ce3b3" :height 1500 :inherit bold)))
  "Face used for time.")

(defun ebn/status--date ()
  "Returns formatted string with date and time"
  (let* ((now (current-time))
	 (date (ebn/status--time->date-string now))
	 (time (format-time-string "%H:%M")))
    
    (format "                        %s\n    %s"
	    (propertize date 'face '(:foreground "#7ce3b3"))
	    (concat
	     (propertize time 'face '(:inherit ebn/status-current-time-face))))))


(cl-defmacro ebn/with-status-buffer (buffer &rest form)
  (declare (indent 0))
  `(with-current-buffer ,buffer
     (read-only-mode -1)
     ,@form
     (read-only-mode 1)))

(defun ebn/status--timer-start (handler)
  "Start a timer that calls HANDLER on every interval"
  (run-at-time t ebn/status-timer-interval handler))

(defun ebn/status--update (&rest args)
  (with-current-buffer ebn/status--buffer
    (read-only-mode -1)
    (undo-boundary)
    (erase-buffer)
    (insert (ebn/status--date))
    (undo-boundary)
    (read-only-mode 1)))

(defun ebn/status--stop ()
  (interactive)
  (ignore-error (cancel-timer 'ebn/status--timer))
  (setq ebn/status--timer nil)
  (kill-buffer ebn/status--buffer)
  (setq ebn/status--on nil))

(defun ebn/status--start ()
  (with-current-buffer (get-buffer-create ebn/status--buffer)
    (setq cursor-type nil)
    (switch-to-buffer ebn/status--buffer)
    (ebn/status--update)
    (unless ebn/status--timer
      (setq ebn/status--timer (ebn/status--timer-start #'ebn/status--update)))
    (setq ebn/status--on t)))

(defun ebn/status ()
  (interactive)
  (if ebn/status--on (ebn/status--stop) (ebn/status--start)))

;;; ebn-status ends here
