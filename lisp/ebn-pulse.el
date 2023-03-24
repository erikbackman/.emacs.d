;;; ebn-pulse --- Highlight current line when switching to another window -*- lexical-binding: t -*-

;;; Commentary:
;; TODO: ignore minibuffer and such

;;; Code:

(defun ebn/pulse-do-pulse (arg)
  (pulse-momentary-highlight-one-line))

;;;###autoload
(define-minor-mode ebn/pulse-minor-mode
  "Pulse highlight on window changes."
  :global t
  :lighter nil
  :init-value nil
  (if (not ebn/pulse-minor-mode)
      (progn (setq window-selection-change-functions
		   (delete 'ebn/pulse-do-pulse window-selection-change-functions)))
    (progn
      (add-to-list 'window-selection-change-functions #'ebn/pulse-do-pulse))))

;;; ebn-pulse ends here
