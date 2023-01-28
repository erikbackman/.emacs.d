;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
		     '(("src" "#+begin_src $1\n$0\n#+end_src" "src" nil nil nil "/home/ebn/.emacs.d/etc/yasnippet/snippets/org-mode/src" nil nil)
		       ("mk" "\\\\($0 \\\\)\n" "Inline Math" nil
			("math")
			((yas-after-exit-snippet-hook #'org-edit-special))
			"/home/ebn/.emacs.d/etc/yasnippet/snippets/org-mode/inline_math" nil nil)
		       ("gp" "#+begin_src gnuplot :file plot.svg\n  set terminal svg enhanced\n  set output \"plot.svg\"\n$0\n#+end_src" "gp" nil nil
			((yas-after-exit-snippet-hook #'org-edit-special))
			"/home/ebn/.emacs.d/etc/yasnippet/snippets/org-mode/gp" nil nil)
		       ("ma" "\\[ `(save-excursion (previous-line)(make-string (current-indentation) ?\\s))`$0 \\]\n" "org-math" nil nil
			((yas-after-exit-snippet-hook #'org-edit-special))
			"/home/ebn/.emacs.d/etc/yasnippet/snippets/org-mode/display_math" nil nil)))


;;; Do not edit! File generated at Sun Jan 22 13:17:00 2023
