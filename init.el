;;; Package Management
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq package-native-compile t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

;;; Misc settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      blink-cursor-blinks 2)
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Functions
(defun ebn/--setup-variable-fonts ()
  (interactive)
  (set-face-attribute 'variable-pitch nil :font "Sarasa Mono CL-12")
  (set-face-attribute 'fixed-pitch nil :font "Sarasa Mono CL-12"))

(defun ebn/su-find-this-file ()
  (interactive)
  (find-file (format "/su::%s" (buffer-file-name))))

(defvar ebn/prev-buffer nil)

(defun ebn/cycle-buffer ()
  (interactive)
  (when-let ((b ebn/prev-buffer))
    (switch-to-buffer b)))

(advice-add 'switch-to-buffer
	    :before (lambda (&rest r) (setq ebn/prev-buffer (current-buffer))))

;;; Built-in Packages
(use-package emacs
  :custom
  (delete-selection-mode t)
  (show-paren-mode 1)
  (global-prettify-symbols-mode t)
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda ()
				    (setq-local page-delimiter ";;;")))
  :bind
  (:map global-map
	("C-8" . backward-list)
	("C-9" . forward-list)
	("C-f" . forward-word)
	("C-b" . backward-word)
	("M-1" . delete-other-frames)
	("M-2" . make-frame)
	("M-3" . delete-frame)
	("M-j" . join-line)
	("s-r" . replace-string)
	("M-z" . zap-up-to-char)
	("C-c t l" . display-line-numbers-mode)
	("C-<return>" . mark-sexp)
	("C-x C-b" . ibuffer-other-window)
	("C-x k" . kill-current-buffer)
	("C-x ;" . comment-line)
	("s-ö" . ebn/cycle-buffer)
	("C-c r" . recentf-open-files)))

(use-package so-long
  :defer 10
  :init
  (global-so-long-mode 1))

(use-package proced
  :commands proced
  :config
  (setq-default proced-format 'medium))

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
	("-" . dired-up-directory)
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
  ;(setq auth-sources '("~/.authinfo.gpg"))
  (setq auth-sources '(password-store))
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
  (recentf-mode)
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
  :bind*
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

  :config
  (require 'org-mouse)
  (plist-put org-format-latex-options :scale 1.5)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((maxima . t)
     (julia . t)
     (haskell . t)))
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (add-hook 'org-mode-hook 'org-display-inline-images)
  ;; Options
  :custom
  (org-confirm-babel-evaluate nil)
  (org-startup-indented t)
  (org-startup-with-latex-preview t)
  (org-pretty-entities t)
  (org-startup-with-inline-images t)
  (org-ellipsis " …")
  (org-export-preserve-breaks t)
  (org-highlight-latex-and-related '(native))
  (org-src-fontify-natively t)
  (org-fontify-quote-and-verse-blocks t)
  (org-startup-folded t)
  (org-cycle-separator-lines 2)
  (org-catch-invisible-edits 'error)
  (org-ctrl-k-protect-subtree t)
  (org-image-actual-width nil)
  (org-return-follows-link t)
  (org-hide-emphasis-markers t)
  (org-latex-listings 'minted)
  (org-latex-packages-alist '(("" "minted")))
  (org-latex-tables-centered t)
  (org-insert-heading-respect-content t)
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
		       (sequence "BACKLOG(b)" "ACTIVE(a)"
				 "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)"
				 "|" "DELEGATED(D)" "CANCELLED(c)")))
  (org-agenda-current-time-string "← now ─────────────────")
  (org-agenda-files '("gtd.org" "someday.org" "tickler.org"))
  (org-capture-templates
   '(("i" "Inbox" entry (file "~/org/inbox.org"))
     ("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
      "* TODO %?\n  %i\n  %a")
     ("s" "Someday" entry (file "~/org/someday.org")
      "* TODO %?\n  %i\n  %a")
     ("r" "Roam node" function #'org-roam-capture)
     ("j" "Journal: Today" function #'org-roam-dailies-capture-today)
     ("J" "Journal: Tomorrow" function #'org-roam-dailies-capture-tomorrow)
     ("d" "Journal: Date" function #'org-roam-dailies-capture-date)))
  (org-latex-pdf-process
   ;; The reason why this is a list is that it usually takes several
   ;; runs of ‘pdflatex’, maybe mixed with a call to ‘bibtex’.  Org
   ;; does not have a clever mechanism to detect which of these
   ;; commands have to be run to get to a stable result, and it also
   ;; does not do any error checking.
   '("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "biber %b"
     "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  :bind*
  (:map org-mode-map
	("C-<return>" . org-meta-return)
	("C-c h" . consult-org-heading)
	("C-<tab>" . org-cycle))
  (:map global-map
	("C-c n n" . org-capture)
	("C-c n a" . org-agenda))

  :hook ((org-mode . (lambda ()
		       (setq line-spacing .2)
		       (setq cursor-type 'box)
		       (org-cdlatex-mode)
		       (ebn/--setup-variable-fonts)))))

(use-package org-modern
  :custom
  (org-modern-block nil)
  (org-modern-table nil)
  :hook (org-mode . org-modern-mode))

(use-package org-roam
  :defer t
  :commands (org-roam-node-find org-roam-capture)

  :init
  (setq org-roam-v2-ack t) ;; Disable v2-migration-prompt

  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))

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
  (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
  (defalias 'org-babel-variable-assignments:julia 'org-babel-variable-assignments:julia-vterm)
  (setq julia-program "julia1.7"))

(use-package julia-snail
  :ensure t
  :custom
  (julia-snail-extensions '(ob-julia))
  :hook (julia-mode . julia-snail-mode))

(use-package julia-vterm :disabled)
(use-package ob-julia-vterm :disabled)

(use-package julia-repl
  :disabled
  :ensure t
  :hook (julia-mode . julia-repl-mode)

  :init
  (setenv "JULIA_NUM_THREADS" "8")

  :config
  ;; Set the terminal backend
  (julia-repl-set-terminal-backend 'vterm)
  
  ;; Keybindings for quickly sending code to the REPL
  (define-key julia-repl-mode-map (kbd "<C-RET>") 'my/julia-repl-send-cell)
  (define-key julia-repl-mode-map (kbd "<M-RET>") 'julia-repl-send-line)
  (define-key julia-repl-mode-map (kbd "<S-return>") 'julia-repl-send-buffer))

(use-package cdlatex)

;;; Lisp
(use-package paredit
  :init
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  :config (define-key paredit-mode-map [remap paredit-newline] nil)
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

;;; Haskell
(use-package haskell-mode
  :custom
  (haskell-font-lock-symbols t)
  (haskell-tags-on-save t)
  :hook
  (haskell-mode . interactive-haskell-mode))

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
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (add-hook 'org-mode-hook 'tempel-setup-capf))

(use-package sv-kalender
  :ensure nil
  :commands 'org-agenda
  :load-path "lisp/")

(use-package pdf-tools
  :defer t
  :commands (pdf-view-mode pdf-tools-install doc-view-mode)
  :config
  (pdf-tools-install))

;;; Rest/WIP
(with-eval-after-load "re-builder"
  (progn
    (defvar my/re-builder-positions nil
      "Store point and region bounds before calling re-builder")
    (advice-add 're-builder
		:before
		(defun my/re-builder-save-state (&rest _)
		  "Save into `my/re-builder-positions' the point and region
positions before calling `re-builder'."
		  (setq my/re-builder-positions
			(cons (point)
                              (when (region-active-p)
				(list (region-beginning)
                                      (region-end)))))))

    (defun reb-replace-regexp (&optional delimited)
      "Run `query-replace-regexp' with the contents of re-builder. With
non-nil optional argument DELIMITED, only replace matches
surrounded by word boundaries."
      (interactive "P")
      (reb-update-regexp)
      (let* ((re (reb-target-binding reb-regexp))
             (replacement (query-replace-read-to
			   re
			   (concat "Query replace"
				   (if current-prefix-arg
                                       (if (eq current-prefix-arg '-) " backward" " word")
                                     "")
				   " regexp"
				   (if (with-selected-window reb-target-window
					 (region-active-p))
				       " in region" ""))
			   t))
             (pnt (car my/re-builder-positions))
             (beg (cadr my/re-builder-positions))
             (end (caddr my/re-builder-positions)))
	(with-selected-window reb-target-window
	  (goto-char pnt)
	  (setq my/re-builder-positions nil)
	  (reb-quit)
	  (query-replace-regexp re replacement delimited beg end))))

    (define-key reb-mode-map (kbd "RET") #'reb-replace-regexp)
    (define-key reb-lisp-mode-map (kbd "RET") #'reb-replace-regexp)
    (global-set-key (kbd "C-M-%") #'re-builder)))

(use-package olivetti
  :defer t
  :commands 'olivetti-mode
  :custom
  (olivetti-body-width 100))

(use-package eix
  :ensure nil
  :load-path "lisp/")

(use-package package-lint)
