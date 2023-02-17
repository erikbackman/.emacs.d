;;; ebn-nav --- Navigation functions -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'dash)

(defun ebn/nav--match-points (regexp)
  (let ((points '()))
    (save-excursion
      (while (re-search-forward regexp (window-end) t)
	(push (match-beginning 0) points)))
    points))

(defun ebn/nav--generate-keys ()
  ;; TODO: Generate the keys for real..
  (mapcar (lambda (p) (format "%s%s" (car p) (cadr p)))  
	  '((a s) (s a)
	    (a d) (d a)
	    (s d) (d s)
	    (d f) (f d)
	    (j k) (k j)
	    (k l) (l k)
	    (a l) (l a)
	    (s k) (k s)
	    (f j) (j f))))

(defun ebn/nav--read-chars (n)
  (let ((echo-keystrokes 0)
	(inp '()))
    (dotimes (c n)
      (setq inp (-snoc inp (read-char))))
    inp))

(defun ebn/nav-goto-char ()
  (interactive)
  (let ((points (ebn/nav--match-points
		 (read-string "regexp: "))))

    (when points
      (let ((keys (ebn/nav--generate-keys))
	    (overlays '()))
	
	(dolist (p points)
	  (let ((overlay (make-overlay p (+ 2 p)))
		(key (pop keys)))
	    
	    (push `(,key . ,overlay) overlays)
	    (overlay-put overlay 'face '(:background "red" :foreground "white"))
	    (overlay-put overlay 'display key)))

	(let* ((inp (concat (ebn/nav--read-chars 2)))
	       (pos (overlay-start (assoc-default inp overlays))))
	  (when pos (goto-char pos)))
	(remove-overlays)))))

(provide 'ebn-nav)
;;; ebn-nav ends here
