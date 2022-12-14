(add-hook 'after-init-hook (lambda () (list-bookmarks)))
(setq initial-buffer-choice (lambda nil (get-buffer "*Bookmark List*")))

;;; Package Management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq package-native-compile t)

;;; use-package is built-in to Emacs 29.0.60
;;; I should remove this eventually.
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
  (set-face-attribute 'variable-pitch nil :font "JuliaMono-12")
  (set-face-attribute 'fixed-pitch nil :font "JuliaMono-12"))

(defun ebn/su-find-this-file ()
  (interactive)
  (let ((cur-buffer (current-buffer)))
    (kill-buffer cur-buffer)
    (find-file (format "/su::%s" (buffer-file-name cur-buffer)))))

(defun ebn/bury-scratch-buffer ()
  (if (string= (buffer-name) "*scratch*")
      (ignore (bury-buffer))
    t))
(add-hook 'kill-buffer-query-functions 'ebn/bury-scratch-buffer)

;;; Built-in Packages
(use-package emacs
  :custom
  (delete-selection-mode t)
  (show-paren-mode 1)
  (global-prettify-symbols-mode t)
  (winner-mode 1)
  (midnight-mode t)
  (eshell-destroy-buffer-when-process-dies t)
  (tab-always-indent 'complete)
  :init
  (put 'narrow-to-region 'disabled nil)
  (when (version<= "29" emacs-version)
    (define-key minibuffer-mode-map (kbd "C-n") 'minibuffer-next-completion)
    (define-key minibuffer-mode-map (kbd "C-p") 'minibuffer-previous-completion)
    (define-key completion-in-region-mode-map (kbd "C-n") 'minibuffer-next-completion)
    (define-key completion-in-region-mode-map (kbd "C-p") 'minibuffer-previous-completion)
    (setq completions-format 'one-column)
    (setq completions-header-format nil)
    (setq completions-max-height 20)
    (setq completion-auto-select 'second-tab)
    (setq minibuffer-completion-auto-choose t))
  
  :bind
  (:map global-map
	("C-8" . backward-list)
	("C-9" . forward-list)
	("C-f" . forward-word)
	("C-b" . backward-word)
	("M-f" . forward-word)
	("M-1" . delete-other-frames)
	("M-2" . make-frame)
	("M-3" . delete-frame)
	("s-3" . delete-frame)
	("M-j" . join-line)
	("s-r" . replace-string)
	("M-z" . zap-up-to-char)
	("M-c" . capitalize-dwim)
	("M-g" . consult-goto-line)
	("C-c t l" . display-line-numbers-mode)
	("C-h ," . xref-find-definitions)
	("C-<return>" . mark-sexp)
	("C-x C-b" . ibuffer-other-window)
	("C-x k" . kill-current-buffer)
	("C-x ;" . comment-line)
	("s-??" . mode-line-other-buffer)
	("C-c t c" . calc)
	("C-c t p" . proced)
	("C-<tab>" . hippie-expand)
	("C-c w u" . winner-undo)
	("s-<up>" . scroll-other-window-down)
	("s-<down>" . scroll-other-window)
	("C-," . completion-at-point)))

(use-package eshell
  :commands (eshell)
  :preface
  (require 'esh-mode)
  :custom
  (eshell-destroy-buffer-when-process-dies t)
  :config  
  (defun eshell-load-bash-aliases ()
    "Read Bash aliases and add them to the list of eshell aliases."
    (with-temp-buffer
      (call-process "bash" nil '(t nil) nil "-ci" "alias")
      (goto-char (point-min))
      (while (re-search-forward "alias \\(.+\\)='\\(.+\\)'$" nil t)
        (eshell/alias (match-string 1) (match-string 2)))))
  (defalias 'openo 'find-file-other-frame)
  (defun eshell/ff (file) (find-file file))
  :hook
  (eshell-mode . paredit-mode)
  (eshell-mode . eshell-load-bash-aliases)
  
  :bind
  ("C-c t e" . eshell)
  (:map eshell-mode-map
	("C-l" . (lambda () (interactive)
		   (eshell/clear-scrollback)
		   (eshell-emit-prompt)))))

(use-package proced
  :commands proced
  :config
  (setq-default proced-format 'medium))

(use-package dired
  :ensure nil
  :config
  (require 'dired-x)
  (defun ebn/dired-copy-file-name (&optional ARG)
    (interactive)
    (let ((fn (dired-get-filename)))
      (kill-new fn)
      (message "%s saved to kill-ring." fn)))
  (setq dired-recursive-copies t
	dired-recursive-deletes t
	dired-dwim-target t
	dired-omit-files "^\\..*$\\|^_.*$"
	delete-by-moving-to-trash t)

  (add-hook 'dired-mode-hook #'dired-omit-mode)
  :bind*
  ("C-x d" . dired)
  ("C-x C-d" . dired-other-window)
  (:map dired-mode-map
	(")" . dired-omit-mode)
	("-" . dired-up-directory)
	("q" . (lambda () (interactive) (quit-window t)))
	("e" . wdired-change-to-wdired-mode)
	("w" . ebn/dired-copy-file-name)))

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
  :custom
  (repeat-echo-function #'ignore)
  (repeat-exit-timeout nil)
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
  
  (ebn/def-repeat-map backward-up-list-repeat-map
		      :keys ([up] #'backward-up-list))

  (ebn/def-repeat-map minibuffer-next-repeat-map
		      :keys ("n" #'minibuffer-next-completion
			     "p" #'minibuffer-previous-completion)
		      ))
 
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
  (set-face-attribute 'erc-prompt-face nil :background nil :foreground "#ae95c7")
  (setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))))

(use-package mindre-theme
  :ensure nil
  :load-path "themes/"
  :custom
  (mindre-use-more-fading nil)
  (mindre-use-more-bold nil)
  (mindre-use-faded-lisp-parens t)
  :custom-face
  (mindre-heading-1 ((t (:height 1.2))))
  :config
  (load-theme 'mindre-dark t))

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
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

;;; Completion
(use-package orderless
  :commands (orderless)
  :custom (completion-styles '(orderless flex)))

(use-package consult
  :ensure t
  :custom
  (consult-preview-key nil)
  (recentf-mode t)
  :config
  (add-hook 'buffer-list-update-hook #'recentf-track-opened-file)
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

;;; Org
(use-package org
  :defer t
  :commands (org-agenda
	     org-capture
	     org-cdlatex-mode)
  :config
  (require 'org-mouse)

  (defun ebn/org-cdlatex-tab ()
    (interactive)
    (if (eobp) (org-edit-src-exit)
      (cdlatex-tab)))

  (defun ebn/diary-last-day-of-month (date)
    "Return `t` if DATE is the last day of the month."
    (let* ((day (calendar-extract-day date))
	   (month (calendar-extract-month date))
	   (year (calendar-extract-year date))
	   (last-day-of-month
	    (calendar-last-day-of-month month year)))
      (= day last-day-of-month)))
  
  (plist-put org-format-latex-options :scale 1.7)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((maxima . t)
     (julia . t)
     (haskell . t)))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (add-hook 'org-mode-hook 'org-display-inline-images)
  ;; Options
  :custom
  (org-confirm-babel-evaluate nil)
  (org-confirm-babel-evaluate nil)
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-startup-with-inline-images t)
  (org-ellipsis " ???")
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
  (org-hide-leading-stars t)
  (org-log-repeat nil)
  (org-log-done nil)
  (org-latex-listings 'minted)
  (org-latex-packages-alist '(("" "minted")))
  (org-latex-tables-centered t)
  (org-insert-heading-respect-content t)
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
		       (sequence "BACKLOG(b)" "ACTIVE(a)"
				 "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)"
				 "|" "DELEGATED(D)" "CANCELLED(c)")))
  (org-agenda-current-time-string "??? now ???????????????????????????")
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
  :bind*
  (:map org-mode-map
	("C-<return>" . org-meta-return)
	("C-c h" . consult-org-heading)
	("C-<tab>" . hippie-expand)
	("C-c e" . org-latex-export-to-pdf))
  (:map global-map
	("C-c n n" . org-capture)
	("C-c n a" . org-agenda))

  :hook ((org-mode . (lambda ()
		       (setq line-spacing nil)
		       (setq cursor-type 'box)
		       (org-cdlatex-mode)
		       (ebn/--setup-variable-fonts)))))

(use-package ebn-org-latex
  :load-path "lisp/"
  :after org)

(use-package org-transclusion)
(use-package org-drill)

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
(use-package gnuplot)
(use-package gnuplot-mode)
(use-package maxima
  :init
  (add-hook 'maxima-mode-hook #'maxima-hook-function)
  (add-hook 'maxima-inferior-mode-hook #'maxima-hook-function)
  (setq maxima-display-maxima-buffer nil)
  :mode ("\\.mac\\'" . maxima-mode)
  :interpreter ("maxima" . maxima-mode))

(use-package julia-mode
  :init
  (setenv "JULIA_NUM_THREADS" "8")
  :config
  (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
  (defalias 'org-babel-variable-assignments:julia 'org-babel-variable-assignments:julia-vterm)
  (setq julia-program "julia1.7"))

(use-package julia-snail
  :ensure t
  :custom
  (julia-snail-extensions '(ob-julia))
  (julia-snail-repl-display-eval-results nil)
  :hook (julia-mode . julia-snail-mode))

(use-package cdlatex
  :hook
  (LaTeX-mode . cdlatex-mode)
  (LaTeX-mode . yas-minor-mode-on)
  :bind*
  (:map LaTeX-mode-map
	("C-c t m" . cdlatex-mode))
  (:map org-src-mode-map
	("<tab>" . ebn/org-cdlatex-tab)))

(use-package org-auctex
  :load-path "lisp/"
  :disabled
  :custom
  (preview-scale-function 1.7)
  :config
  :hook (org-mode . org-auctex-mode))

;;; Lisp
(use-package paredit
  :init
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  :config
  (define-key paredit-mode-map [remap paredit-newline] nil)
  :bind (:map paredit-mode-map
	      ("C-<left>" . paredit-backward-slurp-sexp)
	      ("M-<left>" . paredit-backward-barf-sexp)
	      ("M-<right>" . paredit-forward-barf-sexp)
	      ("M-7" . paredit-wrap-curly)
	      ("M-8" . paredit-wrap-square)
	      ("C-8" . paredit-backward)
	      ("C-9" . paredit-forward)))

(use-package racket-mode
  :ensure t
  :config
  (add-hook 'racket-xp-mode-hook
            (lambda ()
              (remove-hook 'pre-redisplay-functions
                           #'racket-xp-pre-redisplay
                           t)))
  (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
  (add-hook 'racket-mode-hook      #'racket-xp-mode)
  (add-hook 'racket-mode-hook      #'paredit-mode))

;;; Haskell
(use-package haskell-mode
  :config
  (defun ebn/haskell-mode-setup ()
    (interactive-haskell-mode)
    (haskell-indent-mode))
  (defun haskell-mode-after-save-handler ()
      (ignore-errors (haskell-process-reload)))
  :custom
  (haskell-font-lock-symbols t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-tags-on-save t)
  :bind
  ("C-h t" . haskell-mode-show-type-at)
  (:map haskell-mode-map
	("RET" . electric-newline-and-maybe-indent))
  :hook
  (haskell-mode . ebn/haskell-mode-setup))

;;; Mail
(use-package notmuch
  :ensure nil
  :init
  (setq epg-pinentry-mode 'loopback)
  :custom
  (notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "s")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "kau" :query "tag:kau" :key "k")
     (:name "today" :query "date:today" :key "t")
     (:name "study" :query "tag:study" :key "S")
     (:name "deleted" :query "tag:deleted" :key "D")))
  :bind
  ("C-c m" . notmuch)
  (:map notmuch-search-mode-map
	("<mouse-8>" . notmuch-bury-or-kill-this-buffer)
	("d" . (lambda ()
		 (interactive)
		 (notmuch-search-add-tag '("+deleted"))))))

(use-package smtpmail
  :config
  (setq smtpmail-default-smtp-server "smtp.mailbox.org"
	smtpmail-smtp-server "smtp.mailbox.org"
	smtpmail-stream-type 'ssl
	smtpmail-smtp-service 465
	smtpmail-queue-mail nil))

(use-package sendmail
  :config
  (setq send-mail-function 'smtpmail-send-it))

(use-package org-notmuch
  :load-path "lisp/")
;;; Misc
(use-package elfeed
  :defer t
  :commands (efleed elfeed-update)
  :custom
  (elfeed-feeds '(("https://www.gentoo.org/feeds/news.xml" gentoo)
		  ("https://sachachua.com/blog/feed/" emacs))))

(use-package vterm)
(use-package rainbow-mode)

(use-package sv-kalender
  :ensure nil
  :commands 'org-agenda
  :load-path "lisp/")

(use-package pdf-tools
  :ensure t
  :commands (pdf-view-mode pdf-tools-install doc-view-mode)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
	TeX-source-correlate-start-server t)

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  :hook
  (pdf-view-moe . auto-revert-mode)
  :config
  (pdf-tools-install))

;;; Rest/WIP
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package yasnippet
  :custom
  (yas-snippet-dirs '("/home/ebn/.emacs.d/etc/yasnippet/snippets"))
  :config
  (with-eval-after-load 'warnings
    (cl-pushnew '(yasnippet backquote-change) warning-suppress-types
		:test 'equal))
  (yas-global-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package koka-mode
  :load-path "lisp/"
  :mode "\\.kk\\'"
  :hook (koka-mode . (lambda ()
		       (interactive)
		       (setq-local
			indent-tabs-mode nil
			tab-width 2
			tab-stop-list '(2 4)
			tab-always-indent t
			indent-line-function 'insert-tab))))

(use-package rust-mode)
(use-package package-lint)
(use-package keycast)
(use-package embark-consult)
(use-package wgrep)

(use-package avy
  :commands (avy-goto-char-timer)
  :bind ("C-??" . avy-goto-char-timer))

(use-package tree-sitter
  :ensure nil
  :config
  (tree-sitter-require 'haskell)
  (tree-sitter-require 'rust)
  (tree-sitter-require 'python)
  (global-tree-sitter-mode)
  :hook
  (haskell-mode . tree-sitter-hl-mode)
  (rust-mode . tree-sitter-hl-mode))


