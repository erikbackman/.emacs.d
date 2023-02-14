;;; ebn-course --- Activate/Deactivate my courses -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'dash)

(defun ebn/do-symlink (target link)
  (let ((display-buffer-alist
	 '(("\\`\\*Async Shell Command\\*\\'" display-buffer-no-window))))
    (async-shell-command (format "ln -s %s %s" target link))))

(defun ebn/select-course ()
  (interactive)
  (let* ((active-dir
	  "~/Dropbox/courses/active")
	 (all-dir
	  "~/Dropbox/courses/all")
	 (paths
	  (directory-files all-dir t "[^\\.]"))
	 (courses
	  (-zip (-map (lambda (x) (file-name-base x)) paths) paths))
	 (selected
	  (assoc (completing-read "Course:" courses) courses))
	 (action (completing-read "Action:" '(activate deactivate))))
    
    (pcase action
      ("activate"
       (ebn/do-symlink (cdr selected) (format "%s/%s" active-dir (car selected))))
      ("deactivate"
       (let ((link (format "%s/%s" active-dir (car selected))))
	 (when (file-symlink-p link)
	   (async-shell-command (format "rm %s" link))))))))

(provide 'ebn-course)
