;(require 'ebn-exwm)

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

(setq use-package-always-ensure t)
(setq use-package-always-defer nil)

(use-package nordic-night-theme
  :custom-face
  (mode-line ((t :box (:line-width 2 :style released-button))))
  (mode-line-inactive ((t :box (:line-width 2 :style released-button))))
  (org-hide ((t :background "#121212")))
  :config
  (load-theme 'nordic-night t))

;;; Misc settings
(setq confirm-kill-emacs 'y-or-n-p)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq blink-cursor-blinks 2)
(setq-default line-spacing 0.1)
(setq tab-bar-show nil)

;; Treesitter grammars
(setq treesit-language-source-alist
      '((haskell . ("https://github.com/tree-sitter/tree-sitter-haskell"))
	(c . ("https://github.com/tree-sitter/tree-sitter-c"))))

;;; Functions
(defvar ebn/toggle--buffers-alist
  '(("*Messages*" . view-echo-area-messages)))

(defun ebn/toggle-buffer-window (buffer-name)
  (if-let ((win (get-buffer-window buffer-name)))
      (delete-window win)
    (funcall (assoc-default buffer-name ebn/toggle--buffers-alist))))

(defun ebn/toggle-messages ()
  (interactive)
  (ebn/toggle-buffer-window "*Messages*"))

;;; Built-in Packages
(use-package emacs
  :custom
  (show-paren-mode 1)
  (global-prettify-symbols-mode t)
  (tab-always-indent 'complete)
  (proced-format-alist '((ebn pid pcpu pmem comm) (ebn-verbose pid pcpu pmem (args comm))))
  (proced-format 'ebn)
  (blink-cursor-mode nil)
  (fringe-mode '(1 . 1))
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (calc-display-trail t)
  (comint-prompt-read-only t)
  ;;(page-delimiter "\\#| ")
  (grep-command "grep -E --color=auto -nH --null -e ")
  :init
  (put 'narrow-to-region 'disabled nil)
  (global-unset-key (kbd "C-x C-p")) ; UNBIND THE BANE OF MY EXISTENCE!
  (global-unset-key (kbd "C-z"))     ; and this...
  (global-unset-key (kbd "C-x C-z")) ; and this....

  :bind
  (:map global-map
	("C-8" . backward-list)
	("C-9" . forward-list)
	("C-f" . forward-word)
	("C-b" . backward-word)
	("s-l" . windmove-right)
	("s-j" . windmove-down)
	("s-h" . windmove-left)
	("s-k" . windmove-up)
	("M-j" . join-line)
	("s-r" . replace-regexp)
	("M-z" . zap-up-to-char)
	("M-c" . capitalize-dwim)
	("M-u" . upcase-dwim)
	("C-h ," . xref-find-definitions)
	("C-h e" . ebn/toggle-messages)
	("C-<return>" . mark-sexp)
	("C-x C-b" . ibuffer-other-window)
	("C-x k" . kill-current-buffer)
	("C-c ;" . comment-line)
	("C-c C-;" . comment-line)
	("s-ö" . mode-line-other-buffer)
	("C-c t c" . calc)
	("C-c C-t C-c" . calc)
	("C-c t p" . proced)
	("C-<tab>" . hippie-expand)
	("s-<up>" . scroll-other-window-down)
	("s-<down>" . scroll-other-window)
	("C-c <prior>" . beginning-of-buffer)
	("C-c <next>" . end-of-buffer)
	("<prior>" . backward-page)
	("<next>" . forward-page)
	("C-x f" . find-file-other-window)
	("C-c s" . scratch-buffer)
	("C-x K" . kill-buffer-and-window)
	("C-c j" . jump-to-register)))

(use-package shell
  :bind (:map comint-mode-map
	      ("C-l" . comint-clear-buffer)))

(use-package delsel
  :commands (set-mark-command mark-sexp)
  :init
  (delete-selection-mode 1))

(use-package window
  :ensure nil
  :config
  (setq display-buffer-alist
	`(((derived-mode . messages-buffer-mode)
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 1)))))

(use-package winner
  :defer 2
  :config
  (winner-mode)
  :bind
  ("C-0" . winner-undo))

(use-package repeat
  :defer 2
  :config
  (repeat-mode 1)
  :custom
  (repeat-echo-function #'ignore)
  (repeat-exit-timeout nil))

(use-package eshell
  :preface
  (require 'esh-mode)
  (require 'em-smart)
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)
  :custom
  (eshell-destroy-buffer-when-process-dies t)
  :config
  (defun ebn/setup-eshell ()
    (interactive)
    (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
    (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show")))
  
  (defalias 'f 'find-file)
  (defun eshell/gs () (vc-diff))
  (defun eshell/ll () (funcall #'eshell/ls '-l))
  (defun eshell/la () (funcall #'eshell/ls '-la))
  (defun ebn/eshell-clear ()
    (interactive)
    (eshell/clear-scrollback)
    (eshell-emit-prompt))
  :hook
  (eshell-mode . ebn/setup-eshell)
  :bind
  ("C-c t e" . eshell)
  (:map eshell-mode-map ("C-l" . ebn/eshell-clear)))

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
	(list `(,(rx (or ".mp4'" ".mkv") eos) "mpv &>/dev/null")
	      '("\\.pdf\\'" "mupdf &>/dev/null")))
  
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
	("C-c t i" . image-dired)
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
	erc-prompt-for-password nil
	erc-track-enable-keybindings t)
  (setq auth-sources '("~/.authinfo.gpg"))
  (set-face-attribute 'erc-prompt-face nil :background nil :foreground "#ae95c7")
  (setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))))

(use-package dictionary
  :custom
  (dictionary-server "dict.org"))

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
  :demand
  :config
  (vertico-mode))

(use-package orderless
  :after vertico
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :demand
  :config
  (setq recentf-exclude '("/tmp/" "/su" "/etc/")
	recentf-auto-cleanup 'never
	consult-preview-key nil)
  (recentf-mode 1)
  (add-hook 'buffer-list-update-hook #'recentf-track-opened-file)
  :bind
  (:map global-map
	("C-c r" . consult-recent-file)
	("C-c f" . consult-git-grep)
	("C-c l" . consult-line)
	("C-c i" . consult-imenu)
	("M-g i" . consult-imenu)
	("C-c o" . consult-outline)
	("M-g o" . consult-outline)
	("C-x b" . consult-buffer)
	("C-x p b" . consult-project-buffer)
	("C-x B" . consult-buffer-other-window)
	("C-c x" . consult-complex-command)
	("C-c h" . consult-history)
	("M-g g" . consult-imenu)
	("C-c b" . consult-bookmark)))

(use-package corfu
  :custom
  (corfu-auto-delay 0.2)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-commit-predicate nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-echo-documentation nil)
  :hook
  (prog-mode . corfu-mode)
  (sly-mrepl-mode . corfu-mode))

;;; Org
(use-package org
  :ensure cdlatex
  :commands (org-agenda
	     org-capture
	     org-cdlatex-mode)
  :config
  (require 'org-mouse)

  (set-face-attribute 'fixed-pitch nil :font "Fantasque Sans Mono")
  (defun ebn/org-cdlatex-tab ()
    (interactive)
    (if (eobp) (progn
		 (org-edit-src-exit)
		 (org-latex-preview))
      (cdlatex-tab)))
  
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
  (org-refile-targets '((nil :maxlevel . 9)
			(org-agenda-files :maxlevel . 9)))
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path t)
  (org-archive-location "~/org/archived/%s_archive::")
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
  (org-agenda-files `("gtd.org" "someday.org"
		      ,@(directory-files-recursively "~/Dropbox/courses/active/" ".org" t nil t)))
  (org-capture-templates
   '(("i" "Inbox" entry (file "~/org/inbox.org"))
     ("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
      "* TODO %?\n  %i\n  %a")
     ("s" "Someday" entry (file "~/org/someday.org")
      "* TODO %?\n  %i\n  %a")))
  :bind
  (:map org-src-mode-map
	("<tab>" . ebn/org-cdlatex-tab))
  (:map org-mode-map
	("C-c h" . consult-org-heading)
	("C-<return>" . org-meta-return)
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

;;; LaTeX and math
(use-package gnuplot :mode ("\\.gp\\'"))

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
  :init (require 'cdlatex)
  :custom
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-source-correlate-start-server t)
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  :hook
  (LaTeX-mode . turn-on-auto-fill))

(use-package cdlatex
  :init (require 'yasnippet)
  :hook
  (LaTeX-mode . cdlatex-mode)
  (LaTeX-mode . yas-minor-mode-on))

(use-package laas
  :hook (LaTeX-mode . laas-mode)
  :config
  (aas-set-snippets
   'laas-mode
   :cond #'texmathp
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

   "Sum" (lambda () (interactive)
           (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
   "Span" (lambda () (interactive)
            (yas-expand-snippet "\\Span($1)$0"))
   ;; add accent snippets
   :cond #'laas-object-on-left-condition
   "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

;;; Lisp
(font-lock-add-keywords
 'lisp-mode '(("\\;; \\(TODO\\) " 1 'font-lock-warning-face prepend)))

(use-package paredit
  :config
  (define-key paredit-mode-map [remap paredit-newline] nil)
  :hook
  (lisp-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode)
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
  (racket-repl-mode . racket-unicode-input-method-enable)
  :custom-face
  (racket-xp-unused-face ((t (:inherit default :strike-through nil)))))

;;; Common Lisp
(use-package sly
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl --dynamic-space-size 4GB")

  (defun ebn/asdf-load-current-system ()
    (interactive)
    (when-let ((files (directory-files (project-root (project-current)) nil ".*\.\.asd")))
      (sly-asdf-load-system (file-name-base (car files)))))

  (defun ebn/sly-eval-replace ()
  (interactive)
  (sly-eval-async `(slynk:eval-and-grab-output ,(sly-last-expression))
    (lambda (result)
      (cl-destructuring-bind (output value) result
        (backward-kill-sexp)
	(insert (string-trim-right value " (.+)"))))))
  
  (defun ebn/setup-sly ()
    (interactive)
    (bind-key (kbd "C-<up>")
	      #'comint-previous-input 'sly-mrepl-mode-map)
    (bind-key (kbd "C-<down>")
	      #'comint-next-input 'sly-mrepl-mode-map))
  :bind
  ("C-c ," . sly-asdf-load-system)
  (:map sly-mode-map
	("C-c s" . sly-scratch))
  (:map lisp-mode-map
	("C-x p c" . ebn/asdf-load-current-system)
	("<f5>" . ebn/asdf-load-current-system)
	("C-c e" . ebn/sly-eval-replace))
  :hook (sly-mrepl . ebn/setup-sly))

;;; Haskell
(use-package haskell-mode
  :config
  (defun ebn/haskell-mode-setup ()
    (interactive-haskell-mode)
    (haskell-indent-mode 1))
  (defun haskell-mode-after-save-handler ()
    (ignore-errors (haskell-process-reload)))
  :custom
  (haskell-font-lock-symbols t)
  :bind
  ("C-h t" . haskell-mode-show-type-at)
  (:map haskell-mode-map
	("RET" . electric-newline-and-maybe-indent)
	("C-M-e" . forward-sentence)
	("C-x `" . haskell-goto-next-error))
  :hook
  (haskell-mode . ebn/haskell-mode-setup))

;;; Mail
(use-package notmuch
  :ensure nil
  :init
  (setq epg-pinentry-mode 'loopback)
  :config
  (remove-hook 'notmuch-show-hook #'notmuch-show-turn-on-visual-line-mode)
  (remove-hook 'notmuch-search-hook #'notmuch-hl-line-mode)
  :custom
  (notmuch-show-logo nil)
  (notmuch-search-oldest-first nil)
  (notmuch-hello-recent-searches-max 0)
  (notmuch-hello-sections
   '(notmuch-hello-insert-header
     notmuch-hello-insert-saved-searches
     notmuch-hello-insert-recent-searches
     notmuch-hello-insert-alltags))
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
  (setq send-mail-function 'smtpmail-send-it)
  (setq user-mail-address "contact@ebackman.net")
  (setq user-full-name "Erik Bäckman"))

(use-package rainbow-mode :defer t)

(use-package pdf-tools
  :commands (pdf-view-mode pdf-tools-install doc-view-mode)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (pdf-view-midnight-invert nil)
  (pdf-view-midnight-colors '("#e4e4ef" . "#151617"))
  :hook
  (pdf-view-mode . auto-revert-mode)
  :config
  (setq TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
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
  :commands (grep project-find-regexp))

(use-package avy
  :commands
  (avy-goto-char-timer avy-goto-line avy-move-line)
  :custom
  (avy-all-windows 't)
  :bind
  ("C-ö" . avy-goto-char-timer)
  ("M-G" . goto-line)
  ("C-c m" . avy-move-line)
  ("C-c L" . avy-copy-line)
  (:map isearch-mode-map
	("C-ö" . avy-isearch)))

(use-package ebn-course
  :load-path "lisp/"
  :commands (ebn/select-course))

(use-package org-drill
  :commands (org-drill))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))
