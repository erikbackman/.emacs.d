;;; Package Management
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

;;; Misc settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(global-so-long-mode 1)
(show-paren-mode 1)
(setq blink-cursor-blinks 2)

;;; Functions
(defun ebn/--setup-variable-fonts ()
  (interactive)
  (set-face-attribute
   'variable-pitch nil
   :font "Sarasa Mono CL-12")

  (set-face-attribute
   'fixed-pitch nil
   :font "Sarasa Mono CL-12"))

(defun ebn/su-find-this-file ()
  (interactive)
  (find-file (format "/su::%s" (buffer-file-name))))

;;; Built-in Packages
(use-package emacs
  :custom
  (delete-selection-mode t)
  :bind
  (:map lisp-interaction-mode-map
	("C-<return>" . eval-print-last-sexp))
  (:map global-map
	("C-8" . backward-list)
	("C-9" . forward-list)
	("C-f" . forward-word)
	("C-b" . backward-word)
	("M-1" . delete-other-frames)
	("M-2" . make-frame)
	("M-3" . delete-frame)
	("s-r" . replace-string)
	("M-z" . zap-up-to-char)
	("C-c t l" . display-line-numbers-mode)
	("C-<return>" . mark-sexp)
	("C-x C-b" . ibuffer-other-window)
	("C-x k" . kill-current-buffer)
	("C-x ;" . comment-line)
	("C-c r" . recentf-open-files)))

(use-package dired
  :ensure nil
  :config
  (require 'dired-x)
  (setq dired-recursive-copies t
	dired-recursive-deletes t
	dired-dwim-target t
	dired-omit-files "^\\..*$"
	delete-by-moving-to-trash t)
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  :bind*
  ("C-x d" . dired)
  ("C-x C-d" . dired-other-window)
  (:map dired-mode-map
	(")" . dired-omit-mode)
	("q" . (lambda () (interactive) (quit-window t)))
	("e" . wdired-change-to-wdired-mode)))

(use-package outline
  :ensure nil
  :commands 'outline-minor-mode
  :bind
  ("C-c t o" . outline-minor-mode)
  ("C-<tab>" . outline-cycle))

(use-package save-hist
  :ensure nil
  :defer 10
  :init
  (savehist-mode 1)
  :config
  (setq history-length 10))

(cl-defmacro ebn/def-repeat-map (name &key keys exit-with)
  (declare (indent 0))
  (let ((def-repeat-map-result nil))
    (when exit-with
      (push `(define-key ,name ,(kbd exit-with) #'keyboard-quit)
	    def-repeat-map-result))
    (dolist (key (map-pairs keys))
      (push `(define-key ,name ,(car key) ,(cdr key))
	    def-repeat-map-result)
      (push `(put ,(cdr key) 'repeat-map ',name)
	    def-repeat-map-result))
    `(progn
       (defvar ,name (make-sparse-keymap))
       ,@def-repeat-map-result)))

(use-package repeat
  :ensure nil
  :init
  (repeat-mode 1)
  :config
  (ebn/def-repeat-map to-word-repeat-map
		      :keys ("f" #'forward-to-word
			     "b" #'backward-to-word)
		      :exit-with "RET")

  (ebn/def-repeat-map forward-word-repeat-map
		      :keys ("f" #'forward-word
			     "b" #'backward-word)
		      :exit-with "RET")

  (ebn/def-repeat-map capitalize-word-repeat-map
		      :keys ("c" #'capitalize-word))

  (ebn/def-repeat-map downcase-word-repeat-map
		      :keys ("l" #'downcase-word))

  (ebn/def-repeat-map mark-sexp-repeat-map
		      :keys ([return] #'mark-sexp))
  
  :bind (:map isearch-mode-map
	      ("<down>" . #'isearch-repeat-forward)
	      ("<up>" . #'isearch-repeat-backward)))

(use-package erc
  :commands erc-tls
  :config
  (setq erc-server "irc.libera.chat"
	erc-port 6697
	erc-autojoin-channels-alist
	'((".*" "#emacs" "#systemcrafters" "#gentoo"))
	erc-hide-list '("JOIN" "PART" "QUIT")
        erc-nick "ebn"
	erc-prompt-for-password nil)
  (setq auth-sources '("~/.authinfo.gpg"))
  (set-face-attribute 'erc-prompt-face nil :background nil :foreground "#000")
  (setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))))

(use-package mindre-theme
  :ensure nil
  :load-path "themes/"
  :custom
  (mindre-use-more-fading nil)
  :config
  (load-theme 'mindre t))

;;; Backups
(use-package no-littering
  :ensure t
  :init
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq no-littering-etc-directory
	(expand-file-name "config/" user-emacs-directory))
  (setq no-littering-var-directory
	(expand-file-name "data/" user-emacs-directory))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;;; Completion
(use-package corfu
  :custom
  (corfu-auto-delay 0.2)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-commit-predicate nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-echo-documentation nil)
  :init
  (global-corfu-mode))

(use-package corfu-doc
  :ensure t
  :config
  (add-hook 'corfu-mode-hook #'corfu-doc-mode)
  :bind (:map corfu-map
	      ("M-d" . corfu-doc-toggle)))

(use-package vertico
  :ensure
  :init
  (vertico-mode))

(use-package orderless
  :commands (orderless)
  :custom (completion-styles '(orderless flex)))

(use-package consult
  :ensure t
  :init
  (setq consult-preview-key nil)
  (recentf-mode)
  :bind
  (:map global-map
	("C-c r" . consult-recent-file)
	("C-c f" . consult-ripgrep)
	("C-c l" . consult-line)
	("C-c i" . consult-imenu)
	("C-x b" . consult-buffer)
	("C-c x" . consult-complex-command))
  (:map comint-mode-map
	("C-c C-l" . consult-history)))

(use-package cape
  :after corfu
  :bind (("C-c p i" . cape-ispell)
	 ("C-c p d" . cape-dabbrev)
	 ("C-c p f" . cape-file)
	 ("s-7" . cape-file)
	 ("C-c p l" . cape-line)
	 ("C-c p \\" . cape-tex))
  :config
  (setq-local completion-at-point-functions
              (list (cape-super-capf
		     #'cape-dabbrev
		     ;;#'cape-dict
		     #'cape-keyword
		     #'cape-symbol)))

  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file))

;;; Org
(use-package org
  :defer t
  :commands (org-agenda
	     org-capture
	     org-cdlatex-mode)
  :init (progn
	  (defun ebn/diary-last-day-of-month (date)
	    "Return `t` if DATE is the last day of the month."
	    (let* ((day (calendar-extract-day date))
		   (month (calendar-extract-month date))
		   (year (calendar-extract-year date))
		   (last-day-of-month
		    (calendar-last-day-of-month month year)))
	      (= day last-day-of-month))))

  :config (progn
	    (require 'org-mouse)
	    (defun ebn/org-eval-block ()
	      "Wrapper around org-ctrl-c-ctrl-c that previews latex."
	      (interactive)
	      (org-ctrl-c-ctrl-c)
	      (when (string= "sage" (plist-get (cadr (org-element-at-point)) :language))
		(org-latex-preview)))

	    ;; Options
	    (setq org-startup-indented t
		  org-startup-with-latex-preview t
		  org-pretty-entities t
		  org-startup-with-inline-images t
		  org-ellipsis " …"
  		  org-export-preserve-breaks t
		  org-highlight-latex-and-related '(native)
		  org-src-fontify-natively t
		  org-fontify-quote-and-verse-blocks t
		  org-startup-folded t
		  org-cycle-separator-lines 2
		  org-catch-invisible-edits 'error
		  org-ctrl-k-protect-subtree t
		  org-image-actual-width nil
		  org-return-follows-link t
		  org-hide-emphasis-markers t
		  org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
		  org-latex-listings 'minted
		  org-latex-packages-alist '(("" "minted"))
		  org-latex-tables-centered t
		  org-insert-heading-respect-content t
		  org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
				      (sequence "BACKLOG(b)" "ACTIVE(a)"
						"REVIEW(v)" "WAIT(w@/!)" "HOLD(h)"
						"|" "DELEGATED(D)" "CANCELLED(c)"))
		  org-agenda-current-time-string "← now ─────────────────"
		  org-latex-pdf-process
		  ;; The reason why this is a list is that it usually takes several
		  ;; runs of ‘pdflatex’, maybe mixed with a call to ‘bibtex’.  Org
		  ;; does not have a clever mechanism to detect which of these
		  ;; commands have to be run to get to a stable result, and it also
		  ;; does not do any error checking.
		  '("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
		    "biber %b"
		    "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
		    "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

	    ;; active Babel languages
	    (org-babel-do-load-languages
	     'org-babel-load-languages
	     '((maxima . t)
	       (julia-vterm . t)))
	    (setq org-confirm-babel-evaluate nil)
	    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)   
	    (add-hook 'org-mode-hook 'org-display-inline-images)
	    
	    ;; Org-agenda
	    (setq org-agenda-files '("gtd.org" "someday.org" "tickler.org")
		  org-agenda-include-diary t
		  org-capture-templates
		  '(("i" "Inbox" entry (file "~/org/inbox.org"))
		    ("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
		     "* TODO %?\n  %i\n  %a")
		    ("s" "Someday" entry (file "~/org/someday.org")
		     "* TODO %?\n  %i\n  %a")
		    ("r" "Roam node" function #'org-roam-capture)
		    ("j" "Journal: Today" function #'org-roam-dailies-capture-today)
		    ("J" "Journal: Tomorrow" function #'org-roam-dailies-capture-tomorrow)
		    ("d" "Journal: Date" function #'org-roam-dailies-capture-date))
		  org-refile-targets
		  '((nil :maxlevel . 3)
		    (org-agenda-files :maxlevel . 3)))

	    (defun ebn/rectangle-number-lines ()
	      (interactive)
	      (rectangle-number-lines (region-beginning) (region-end) 1 "%s. ")))

  :bind*
  (:map org-mode-map
	("C-<return>" . org-meta-return)
	("C-c h" . consult-org-heading)
	("C-j" . join-line)
	("C-<tab>" . org-cycle))
  (:map global-map
	("C-c n n" . org-capture)
	("C-c n a" . org-agenda))

  :hook ((org-mode . (lambda ()
		       (setq line-spacing .2)
		       (setq cursor-type 'box)
		       (org-cdlatex-mode)
		       (ebn/--setup-variable-fonts)))))

(use-package org-modern)

(use-package org-roam
  :defer t
  :commands (org-roam-node-find org-roam-capture)

  :init
  (setq org-roam-v2-ack t) ;; Disable v2-migration-prompt

  :custom
  (org-roam-directory "~/org/org-roam")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   `(("d" "default" plain "%?"
      :if-new (file+head
	       "%<%Y%m%d%H%M%S>-${slug}.org"
	       ,(let ((options '("#+options: _:{}"
				 "#+options: ^:{}"
				 "#+startup: latexpreview"
				 "#+startup: entitiespretty"
				 "#+startup: inlineimages"
				 "#+title: ${title}")))
		  (mapconcat 'identity options "\n")))
      :unnarrowed t)))

  (org-roam-node-display-template "${title}")
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n c" . org-roam-capture))
  :config
  (org-roam-db-autosync-mode))

;;; LaTeX and math
(use-package gnuplot-mode)
(use-package maxima
  :init
  (add-hook 'maxima-mode-hook #'maxima-hook-function)
  (add-hook 'maxima-inferior-mode-hook #'maxima-hook-function)
  (setq maxima-display-maxima-buffer nil)
  :mode ("\\.mac\\'" . maxima-mode)
  :interpreter ("maxima" . maxima-mode))

(use-package julia-mode
  :ensure
  :config
  (use-package julia-repl)
  (use-package julia-vterm)
  (use-package ob-julia-vterm)
  (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
  (defalias 'org-babel-variable-assignments:julia 'org-babel-variable-assignments:julia-vterm)
  (setq julia-program "julia1.7"))

(use-package cdlatex)

;;; Lisp
(use-package paredit
  :init
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  :bind (:map paredit-mode-map
	      ("M-<left>" . paredit-backward-barf-sexp)
	      ("M-<right>" . paredit-forward-barf-sexp)
	      ("M-7" . paredit-wrap-curly)
	      ("M-8" . paredit-wrap-round)
	      ("C-8" . paredit-backward)
	      ("C-9" . paredit-forward)))

(use-package racket-mode
  :config
  (defun setup-racket-eldoc ()
    (eldoc-mode +1)
    (setq eldoc-documentation-function #'racket-xp-eldoc-function))

  (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
  (add-hook 'racket-mode-hook      #'setup-racket-eldoc)
  (add-hook 'racket-mode-hook      #'racket-xp-mode)
  (add-hook 'racket-mode-hook      #'paredit-mode))

;;; Misc
(use-package vterm)
(use-package rainbow-mode)

(use-package tempel
  :custom
  (tempel-trigger-prefix "<")
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package sv-kalender
  :ensure nil
  :commands 'org-agenda
  :load-path "lisp/")

(use-package pdf-tools
  :defer t
  :commands (pdf-view-mode pdf-tools-install)
  :config
  (pdf-tools-install))
