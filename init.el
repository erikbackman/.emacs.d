;; Package managment
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq package-native-compile t)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)

;; Basic settings
(setq custom-file "~/.emacs.d/custom.el")
(setq tab-always-indent 'complete)
(setq completion-auto-select 'second-tab)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq prettify-symbols-mode t)
(setq dired-dwim-target t)
(setopt compilation-scroll-output t)
(setq dired-listing-switches "-alh")

(delete-selection-mode)
(repeat-mode)

(add-to-list 'exec-path "/home/ebn/.local/bin")

;; Functions
(defun my-view-messages ()
  "Toggle message buffer."
  (interactive)
  (if-let ((w (get-buffer-window "*Messages*")))
      (delete-window w)
    (view-echo-area-messages)))

;; Global keybinds
(defmacro gbind (key command)
  "Globally map `KEY' to `COMMAND'."
  `(global-set-key ,(kbd key) ,command))

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

;;; Package configuration
;;;

(use-package mindre-dark-theme
  :demand
  :load-path "lisp/"
  :config (load-theme 'mindre-dark t))

(use-package comint
  :ensure nil
  :bind (:map comint-mode-map ("C-l" . #'comint-clear-buffer)))

(use-package abbrev
  :ensure nil
  :bind ("C-<tab>" . expand-abbrev))

(use-package vertico :demand :config (vertico-mode))
(use-package consult
  :config
  (setq consult-preview-key nil)
  :bind
  ("C-c l" . #'consult-line)
  ("C-ö" . #'consult-goto-line)
  ("C-c g" . #'consult-imenu))

(use-package orderless
  :custom
  (completion-styles '(orderless basic flex))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :demand
  :custom
  (corfu-auto t)
  :config
  (global-corfu-mode))

(use-package cape
  :config
  (defun init-cape ()
    (interactive)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file))
  :hook (zig-mode . init-cape))

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
   '((julia . t)
     (R . t)
     (latex . t)))
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
  :bind (:map global-map
	      ("C-8" . #'paredit-backward)
	      ("C-9" . #'paredit-forward)
	      ("s-u" . #'paredit-backward-up)
	      ("s-n" . #'paredit-forward-down)))

(use-package sly
  :config
  (setq sly-default-lisp 'roswell
	sly-lisp-implementations
	`((roswell ("ros" "-Q" "run" "dynamic-space-size=4GB"))))
  (defun my-setup-sly-repl ()
    (interactive)
    (define-key sly-mrepl-mode-map (kbd "C-<up>") #'comint-previous-input)
    (define-key sly-mrepl-mode-map (kbd "C-<down>") #'comint-next-input))
  (add-hook 'sly-mrepl-mode-hook #'my-setup-sly-repl))

(use-package julia-mode
  :config
  (add-to-list
   'exec-path
   "/home/ebn/.julia/juliaup/julia-1.9.4+0.x64.linux.gnu/bin/")
  :hook (julia-mode . julia-repl-mode))

(use-package julia-repl
  :init
  (setenv "JULIA_NUM_THREADS" "8")
  :config
  (julia-repl-set-terminal-backend 'vterm))

(use-package zig-mode
  :config
  (add-to-list 'exec-path "/home/ebn/opt/zig/"))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))
