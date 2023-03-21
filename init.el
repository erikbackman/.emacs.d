;; Package Management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq package-native-compile t)

;;; use-package is built-in to Emacs 29.0.60
;;; I should remove this eventually.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)              
  (package-install 'use-package))
(defalias 'yes-or-no-p 'y-or-n-p)

(setq use-package-always-ensure t)

;;; Misc settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      blink-cursor-blinks 2)

;; Treesitter grammars
(setq treesit-language-source-alist
      '((haskell . ("https://github.com/tree-sitter/tree-sitter-haskell"))))

(defun ebn/bury-scratch-buffer ()
  (if (string= (buffer-name) "*scratch*")
      (ignore (bury-buffer))
    t))
(add-hook 'kill-buffer-query-functions 'ebn/bury-scratch-buffer)

;;; Built-in Packages
(use-package emacs
  :custom
  (show-paren-mode 1)
  (global-prettify-symbols-mode t)
  (tab-always-indent 'complete)
  (proced-format 'medium)
  (blink-cursor-mode nil)
  :init
  (put 'narrow-to-region 'disabled nil)
  (global-unset-key (kbd "C-x C-p")) ; UNBIND THE BANE OF MY EXISTENCE!
  (global-unset-key (kbd "C-z"))     ; and this...
  (global-unset-key (kbd "C-x C-z")) ; and this....

  (unless (version< emacs-version "29.0")
    (setq completion-auto-help 'visible
          completion-auto-select 'second-tab
          completion-show-help nil
          completions-sort nil
          completions-header-format nil))
  
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
	("s-8" . split-window-below)
	("s-9" . split-window-right)
	("s-0" . delete-window)
	("M-j" . join-line)
	("s-r" . replace-regexp)
	("M-z" . zap-up-to-char)
	("M-c" . capitalize-dwim)
	("M-u" . upcase-dwim)
	("C-c t l" . display-line-numbers-mode)
	("C-c t s" . flyspell-mode)
	("C-h ," . xref-find-definitions)
	("C-<return>" . mark-sexp)
	("C-x C-b" . ibuffer-other-window)
	("C-x k" . kill-current-buffer)
	("C-x ;" . comment-line)
	("s-ö" . mode-line-other-buffer)
	("C-c t c" . calc)
	("C-c t p" . proced)
	("C-<tab>" . hippie-expand)
	("s-<up>" . scroll-other-window-down)
	("s-<down>" . scroll-other-window)
	("C-c <prior>" . beginning-of-buffer)
	("C-c <next>" . end-of-buffer)
	("C-x f" . find-file-other-window)
	("C-1" . (lambda () (interactive) (bookmark-set "1")))
	("C-2" . (lambda () (interactive) (bookmark-set "2")))
	("C-<f1>" . (lambda () (interactive) (bookmark-jump "1")))
	("C-<f2>" . (lambda () (interactive) (bookmark-jump "2")))
	("C-x K" . kill-buffer-and-window)))

(use-package delsel
  :ensure nil
  :commands (set-mark-command mark-sexp)
  :init
  (delete-selection-mode 1))

(use-package window
  :ensure nil
  :config
  (setq display-buffer-alist
	`(((derived-mode . process-menu-mode)
	   (display-buffer-in-side-window)
           (dedicated . t)
           (side . bottom)
           (slot . 0))
	  ((derived-mode . messages-buffer-mode)
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 1))
	  ((derived-mode . proced-mode)
	   (display-buffer-full-frame)))))

(use-package winner
  :ensure nil
  :defer 2
  :config
  (winner-mode)
  :bind
  ("C-0" . winner-undo))

(use-package repeat
  :ensure nil
  :defer 2
  :config
  (repeat-mode t)
  :custom
  (repeat-echo-function #'ignore)
  (repeat-exit-timeout nil)
  :bind
  (:repeat-map my/to-word-repeat-map
	       ("f" . forward-to-word)
	       ("b" . backward-to-word))
  (:repeat-map my/forward-word-repeat-map
	       ("f" . forward-word)
	       ("b" . backward-word))
  (:repeat-map my/move-by-sexp-repeat-map
	       ("f" . forward-sexp)
	       ("b" . backward-sexp))
  (:repeat-map my/backward-up-list-repeat-map
	       ([up] . backward-up-list)))

(use-package eshell
  :commands (eshell)
  :custom
  (eshell-destroy-buffer-when-process-dies t)
  :config
  (require 'esh-mode)
  (defun ebn/setup-eshell ()
    (interactive)
    (with-temp-buffer
      (call-process "bash" nil '(t nil) nil "-ci" "alias")
      (goto-char (point-min))
      (while (re-search-forward "alias \\(.+\\)='\\(.+\\)'$" nil t)
        (eshell/alias (match-string 1) (match-string 2)))))
  :hook
  (eshell-mode . ebn/setup-eshell)
  
  :bind
  ("C-c t e" . eshell)
  (:map eshell-mode-map
	("C-l" . (lambda () (interactive)
		   (eshell/clear-scrollback)
		   (eshell-emit-prompt)))))

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
	dired-isearch-filenames 'dwim
	delete-by-moving-to-trash t)

  (setq dired-guess-shell-alist-user
	'(("\\.mp4\\'" "mpv")
	  ("\\.pdf\\'" "mupdf")))
  
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  :bind*
  ("C-x d" . dired)
  ("C-x C-d" . dired-other-window)
  (:map dired-mode-map
	(")" . dired-omit-mode)
	("-" . dired-up-directory)
	("q" . (lambda () (interactive) (quit-window t)))
	("e" . wdired-change-to-wdired-mode)
	("w" . ebn/dired-copy-file-name)
	("?" . dired-create-empty-file)))

(use-package save-hist
  :ensure nil
  :defer 10
  :init
  (savehist-mode 1)
  :config
  (setq history-length 20))
 
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
  (setq erc-prompt (lambda () (concat "[" (buffer-name) "]")))
  :hook (erc-mode . ebn/set-small-font))

(use-package mindre-dark-theme
  :ensure nil
  :load-path "themes/"
  :custom
  (mindre-use-more-fading nil)
  (mindre-use-faded-lisp-parens t)
  :custom-face
  (mindre-heading-1 ((t (:height 1.2))))
  :config
  (load-theme 'mindre-dark t))

;;; Backups
(use-package no-littering
  :preface
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq no-littering-etc-directory
	(expand-file-name "etc/" user-emacs-directory))
  (setq no-littering-var-directory
	(expand-file-name "var/" user-emacs-directory))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

;;; Completion
(use-package vertico
  :config
  (vertico-mode))

(use-package orderless
  :custom (completion-styles '(orderless)))

(use-package consult
  :defer nil
  :config
  (setq recentf-exclude '("/tmp/" "/su" "/etc/")
	recentf-auto-cleanup 'never
	consult-preview-key nil)
  (recentf-mode 1)

  (add-hook 'buffer-list-update-hook #'recentf-track-opened-file)
  :init
  (setq completion-in-region-function
	(lambda (&rest args)
	  (apply (if vertico-mode
		     #'consult-completion-in-region
		   #'completion--in-region)
		 args)))
  :bind
  (:map global-map
	("C-c r" . consult-recent-file)
	("C-c f" . consult-ripgrep)
	("C-c l" . consult-line)
	("C-c i" . consult-imenu)
	("C-x b" . consult-buffer)
	("C-c x" . consult-complex-command)
	("C-c j" . consult-global-mark)
	("C-c b" . consult-bookmark)))

;;; Org
(use-package org
  :pin gnu
  :defer t
  :commands (org-agenda
	     org-capture
	     org-cdlatex-mode)
  :config
  (require 'org-mouse)

  (defun ebn/org-cdlatex-tab ()
    (interactive)
    (if (eobp) (progn
		 (org-edit-src-exit)
		 (org-latex-preview))
      (cdlatex-tab)))

  (defun ebn/diary-last-day-of-month (date)
    "Return `t` if DATE is the last day of the month."
    (let* ((day (calendar-extract-day date))
	   (month (calendar-extract-month date))
	   (year (calendar-extract-year date))
	   (last-day-of-month
	    (calendar-last-day-of-month month year)))
      (= day last-day-of-month)))
  
  (plist-put org-format-latex-options :scale 1.5)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((maxima . t)
     (julia . t)
     (dot . t)
     (haskell . t)
     (gnuplot . t)))

  (setq org-latex-listings 'minted
	org-latex-packages-alist '(("" "minted"))
	org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (setq org-latex-minted-langs
	'((emacs-lisp "common-lisp")
	  (shell-script "bash")
	  (julia "julia")))
  
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (add-hook 'org-mode-hook 'org-display-inline-images)
  :custom
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate nil)
  (org-log-into-drawer t)
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-startup-with-inline-images t)
  (org-export-preserve-breaks t)
  (org-highlight-latex-and-related '(native))
  (org-src-fontify-natively t)
  (org-fontify-quote-and-verse-blocks t)
  (org-startup-folded t)
  (org-cycle-separator-lines 2)
  (org-image-actual-width nil)
  (org-return-follows-link t)
  (org-hide-leading-stars t)
  (org-cycle-separator-lines 2)
  (org-latex-src-block-backend 'minted)
  (org-latex-packages-alist '(("" "minted")))
  (org-latex-tables-centered t)
  (org-preview-latex-image-directory "~/.emacs.d/var/org/latex-preview/")
  (org-insert-heading-respect-content t)
  (org-agenda-tags-column 80)
  (org-agenda-show-inherited-tags nil)
  (org-agenda-remove-tags t)
  (org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %LOCATION")
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
		       (sequence "BACKLOG(b)" "ACTIVE(a)"
				 "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)"
				 "|" "DELEGATED(D)" "CANCELLED(c)")))
  (org-agenda-current-time-string "← now ─────────")
  (org-agenda-files `("gtd.org" "someday.org" "tickler.org"
		      ,@(directory-files-recursively "~/Dropbox/courses/active/" ".org" t nil t)))
  (org-capture-templates
   '(("i" "Inbox" entry (file "~/org/inbox.org"))
     ("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
      "* TODO %?\n  %i\n  %a")
     ("s" "Someday" entry (file "~/org/someday.org")
      "* TODO %?\n  %i\n  %a")))
  :bind*
  (:map org-mode-map
	("C-<return>" . org-meta-return)
	("C-c h" . consult-org-heading)
	("C-<tab>" . hippie-expand)
	("C-c C-<up>" . org-promote-subtree)
	("C-c C-<down>" . org-demote-subtree)
	("C-c 1" . org-toggle-narrow-to-subtree)
	("C-c t t" . org-transclusion-add))
  (:map global-map
	("C-c n n" . org-capture)
	("C-c n a" . org-agenda)
	("C-c S" . org-store-link))

  :hook (org-mode . org-cdlatex-mode))

(use-package ebn-org-latex
  :load-path "lisp/"
  :after org)

(use-package org-transclusion :defer t)

(use-package org-modern
  :disabled
  :custom
  (org-modern-block nil)
  (org-modern-table nil)
  :config
  (global-org-modern-mode)
  :hook (org-mode . org-modern-mode))

(use-package org-roam
  :disabled
  :defer t
  :commands (org-roam-node-find org-roam-capture)
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
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n c" . org-roam-capture))
  :config
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

;;; LaTeX and math
(use-package gnuplot :mode ("\\.gp\\'"))
(use-package maxima
  :config
  (setq imaxima-fnt-size "Large")
  :hook
  (maxima-mode-hook . maxima-hook-function)
  (maxima-inferior-mode-hook . maxima-hook-function)
  :interpreter ("maxima" . maxima-mode))

(use-package julia-mode
  :mode ("\\.jl\\'" . julia-mode)
  :init
  (setenv "JULIA_NUM_THREADS" "8")
  :config
  (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
  (defalias 'org-babel-variable-assignments:julia 'org-babel-variable-assignments:julia-vterm))

(use-package julia-snail
  :after julia-mode
  :custom
  (julia-snail-extensions '(ob-julia))
  (julia-snail-repl-display-eval-results nil)
  :hook (julia-mode . julia-snail-mode))

(use-package tex
  :ensure auctex
  :config
  ;; (add-to-list 'TeX-expand-list
  ;; 	       '("%(mode)"
  ;; 		 (lambda () (if TeX-interactive-mode "" " -interaction=batchmode"))))
  :hook
  (LaTeX-mode . turn-on-auto-fill))

(use-package cdlatex
  :hook
  (LaTeX-mode . cdlatex-mode)
  (LaTeX-mode . yas-minor-mode-on)
  :bind
  (:map org-src-mode-map
	("<tab>" . ebn/org-cdlatex-tab)))

(use-package laas
  :hook (LaTeX-mode . laas-mode)
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math
                    "supp" "\\supp"
                    "On" "O(n)"
                    "O1" "O(1)"
                    "Olog" "O(\\log n)"
                    "Olon" "O(n \\log n)"
		    "fx" "f(x)"
		    "gx" "g(x)"
		    "xt" "x(t)"
		    "yt" "y(t)"
		    "zt" "z(t)"
		    ":::" "\\vdots"
		    "intab" "\int_{a}^{b}"
		    "vp" "\\vec{v_{p}}"
		    "up" "\\vec{u_{p}}"
		    "vn" "\\vec{v_{n}}"
		    "un" "\\vec{u_{n}}"
                    ;; bind to functions!
                    "Sum" (lambda () (interactive)
                            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
                    "Span" (lambda () (interactive)
                             (yas-expand-snippet "\\Span($1)$0"))
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

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
  :config
  (defun ebn/setup-racket-mode ()
    (interactive)
    (racket-unicode-input-method-enable)
    (racket-xp-mode)
    (remove-hook 'pre-redisplay-functions
		 #'racket-xp-pre-redisplay
		 t)
    (paredit-mode))
  :hook
  (racket-mode . ebn/setup-racket-mode)
  (racket-repl-mode . racket-unicode-input-method-enable))

;;; Haskell
(use-package haskell-mode
  :config
  (defun ebn/haskell-mode-setup ()
    (interactive-haskell-mode)
    (haskell-indent-mode))
  (defun haskell-mode-after-save-handler ()
      (ignore-errors (haskell-process-reload)))
  :bind
  ("C-h t" . haskell-mode-show-type-at)
  (:map haskell-mode-map
	("RET" . electric-newline-and-maybe-indent)
	("C-M-e" . forward-sentence))
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
  ("C-c t m" . notmuch)
  (:map notmuch-search-mode-map
	("<mouse-8>" . notmuch-bury-or-kill-this-buffer)
	("d" . (lambda ()
		 (interactive)
		 (notmuch-search-add-tag '("+deleted"))))))

(use-package smtpmail
  :after notmuch
  :config
  (setq smtpmail-default-smtp-server "smtp.mailbox.org"
	smtpmail-smtp-server "smtp.mailbox.org"
	smtpmail-stream-type 'ssl
	smtpmail-smtp-service 465
	smtpmail-queue-mail nil))

(use-package sendmail
  :after notmuch
  :config
  (setq send-mail-function 'smtpmail-send-it))

;;; Misc
(use-package elfeed
  :defer t
  :commands (efleed elfeed-update)
  :custom
  (elfeed-feeds '(("https://www.gentoo.org/feeds/news.xml" gentoo)
		  ("https://sachachua.com/blog/feed/" emacs))))

(use-package rainbow-mode :defer t)

(use-package pdf-tools
  :commands (pdf-view-mode pdf-tools-install doc-view-mode)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
	TeX-source-correlate-start-server t)

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  :hook
  (pdf-view-mode . auto-revert-mode)
  :config
  (pdf-tools-install))

;;; Rest/WIP
(use-package marginalia :init (marginalia-mode))

(use-package yasnippet
  :defer 5
  :config
  (with-eval-after-load 'warnings
    (cl-pushnew '(yasnippet backquote-change) warning-suppress-types
		:test 'equal))
  (yas-global-mode)
  (yas-reload-all))

(use-package wgrep
  :commands (occur embark-act grep))

(use-package avy
  :commands
  (avy-goto-char-timer avy-goto-line avy-move-line)
  :custom
  (avy-all-windows 't)
  :bind
  ("C-ö" . avy-goto-char-timer)
  ("M-g" . avy-goto-line)
  ("C-c m" . avy-move-line)
  (:map isearch-mode-map
	("C-ö" . avy-isearch)))

(use-package ebn-course
  :load-path "lisp/"
  :commands (ebn/select-course))

(use-package org-drill
  :commands (org-drill))
