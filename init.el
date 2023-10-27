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

(delete-selection-mode)
(repeat-mode)

;; Functions
(defun my-view-messages ()
  (interactive)
  (if-let ((w (get-buffer-window "*Messages*")))
      (delete-window w)
    (view-echo-area-messages)))

;; Global keybinds
(global-set-key (kbd "M-j") #'join-line)
(global-set-key (kbd "C-x k") #'kill-current-buffer)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-<return>") #'mark-sexp)
(global-set-key (kbd "C-8") #'backward-list)
(global-set-key (kbd "C-9") #'forward-list)
(global-set-key (kbd "C-<prior>") #'beginning-of-buffer)
(global-set-key (kbd "C-<next>") #'end-of-buffer)
(global-set-key (kbd "C-h e") #'my-view-messages)

;;; Package configuration
;;;

(use-package kaolin-themes
  :demand
  :config (load-theme 'kaolin-temple t))

(use-package vertico :demand :config (vertico-mode))
(use-package consult
  :config
  (setq consult-preview-key nil)
  :bind
  ("M-g g" . #'consult-goto-line)
  ("C-c l" . #'consult-line))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :demand
  :custom
  (corfu-auto t)
  :config
  (global-corfu-mode))

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
  :commands (notmuch)
  :after notmuch
  :config
  (setq smtpmail-default-smtp-server "smtp.mailbox.org"
	smtpmail-smtp-server "smtp.mailbox.org"
	smtpmail-stream-type 'ssl
	smtpmail-smtp-service 465
	smtpmail-queue-mail nil))

(use-package sendmail
  :commands (notmuch)
  :after notmuch
  :config
  (setq send-mail-function 'smtpmail-send-it)
  (setq user-mail-address (auth-source-pass-get "Username" "mail/mailbox"))
  (setq user-full-name (auth-source-pass-get "Fullname" "mail/mailbox")))

(use-package org
  :config
  (setq org-default-notes-file "/home/ebn/org/notes.org")
  (setq org-agenda-files '("notes.org" "life.org" "study.org"))
  (setq org-capture-templates
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
  (emacs-lisp-mode . paredit-mode))

(use-package sly
  :config
  (setq inferior-lisp-program
	"/home/ebn/.roswell/impls/x86-64/linux/sbcl-bin/2.3.9/bin/sbcl --dynamic-space-size 4GB"))
