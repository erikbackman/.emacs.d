;; Package managment
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq package-native-compile t)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)

;; Path
(add-to-list 'exec-path "/home/ebn/.local/bin")
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;; Basic settings
(setq custom-file "~/.emacs.d/custom.el")
(setq completion-auto-select 'second-tab)
(setq dired-dwim-target t)
(setopt compilation-scroll-output t)
(setq dired-listing-switches "-alh")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setopt initial-scratch-message nil)
;; IDO
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq-default confirm-nonexistent-file-or-buffer nil)
;;
(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers 'relative)))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(delete-selection-mode)
(repeat-mode)

;; Functions
(defun my-view-messages ()
  "Toggle message buffer."
  (interactive)
  (if-let ((w (get-buffer-window "*Messages*")))
      (delete-window w)
    (view-echo-area-messages)))

(defun my-find-definitions ()
  (interactive)
  (call-interactively 'xref-find-definitions))

;; Global keybinds
(defmacro gbind (key command)
  "Globally map `KEY' to `COMMAND'."
  `(global-set-key ,(kbd key) ,command))

(unbind-key (kbd "C-z"))
(gbind "M-j" #'join-line)
(gbind "C-x k" #'kill-current-buffer)
(gbind "C-x C-b" #'ibuffer)
(gbind "C-<return>" #'mark-sexp)
(gbind "C-8" #'backward-list)
(gbind "C-9" #'forward-list)
(gbind "C-<prior>" #'beginning-of-buffer)
(gbind "C-<next>" #'end-of-buffer)
(gbind "C-h e" #'my-view-messages)
(gbind "C-c t c" #'calc)
(gbind "C-<tab>" #'hippie-expand)
(gbind "C-ö" 'my-find-definitions)
(gbind "M-z" 'zap-up-to-char)

;;; Package configuration
;;;

(use-package ebn-citrus-theme
  :demand
  :load-path "lisp/"
  :config (load-theme 'ebn-citrus t))

(use-package comint
  :ensure nil
  :bind (:map comint-mode-map ("C-l" . #'comint-clear-buffer)))

(use-package completion-preview
  :demand t
  :load-path "lisp/"
  :init
  (require 'completion-preview)
  :bind
  ("C-," . #'completion-preview-prev-candidate)
  ("C-." . #'completion-preview-next-candidate)
  :config
  (completion-preview-mode))

(use-package cape
  :config
  (defun init-cape ()
    (interactive)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (completion-preview-mode))
  :hook ((zig-mode . init-cape)
	 (eshell-mode . init-cape)
	 (emacs-lisp-mode . init-cape)))

(use-package pdf-tools
  :commands (pdf-view-mode pdf-tools-install doc-view-mode)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook
  (pdf-view-mode . auto-revert-mode)
  :config
  (setq TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (pdf-loader-install))

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
  :hook
  (LaTeX-mode . cdlatex-mode))

;; Fix GPG with Emacs 29
(setenv "GPG_AGENT_INFO" nil)
(fset 'epg-wait-for-status 'ignore)

(setq auth-sources '("~/authinfo.gpg"))

(use-package notmuch
  :commands (notmuch)
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
  (setq user-mail-address (auth-source-pass-get "Username" "mail/mailbox"))
  (setq user-full-name (auth-source-pass-get "Fullname" "mail/mailbox")))

(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((julia . t) (R . t) (latex . t)))
  :custom
  (org-latex-listings 'minted)
  (org-confirm-babel-evaluate nil)
  (org-latex-pdf-process
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (org-src-fontify-natively t)
  (org-default-notes-file "/home/ebn/org/notes.org")
  (org-agenda-files '("notes.org" "life.org" "study.org"))
  (org-capture-templates
   '( ("t" "Todo" entry (file+headline "life.org" "Tasks")
       "* TODO %?\n %i\n")
      ("s" "Study Todo" entry (file+headline "study.org" "Tasks")
       "* TODO %? %^G\n DEADLINE:%^t\n %i\n")) )
  :bind
  (:map global-map
	("C-c n n" . #'org-capture)
	("C-c n a" . #'org-agenda)))

(use-package paredit
  :hook
  (lisp-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode)
  (scheme-mode . paredit-mode)
  :bind (:map lisp-mode-map
	      ("C-8" . #'paredit-backward)
	      ("C-9" . #'paredit-forward)
	      ("s-u" . #'paredit-backward-up)
	      ("s-n" . #'paredit-forward-down)))

(use-package julia-mode
  :hook (julia-mode . julia-repl-mode))

(use-package julia-repl
  :init
  (setenv "JULIA_NUM_THREADS" "8")
  :config
  (julia-repl-set-terminal-backend 'vterm))

(use-package zig-mode
  :config
  (require 'semantic/symref/grep)
  (add-to-list 'semantic-symref-filepattern-alist '(zig-mode "*.zig"))
  :bind (:map zig-mode-map ("C-c C-c" . #'recompile)))

;; (add-to-list 'eshell-modules-list 'eshell-tramp) ; use eshell-sudo
