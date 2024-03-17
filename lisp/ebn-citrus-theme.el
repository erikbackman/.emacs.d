;;; ebn-citrus-theme.el --- some description -*- lexical-binding: t -*-

;; Author: Erik Bäckman <contact@ebackman.net>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: faces
;; Homepage:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------

;;; Commentary:

;;; Code:

(deftheme ebn-citrus "ebn-citrus theme")

(let ((bg "#1d2021")
      (fg "#f9f4e8")
      (red+1 "#E9B0B0")
      (red "#D9A0A0")
      (red-1 "#C99090")
      (green "#7F9F7F")
      (green+1 "#8ca98c")
      (green+2 "#acd2ac")
      (green+3 "#7ce3b3")
      (cyan "medium aquamarine")
      (blue+1 "#9CC7FB")
      (blue "#99DDE0")
      (blue-1 "#89C5C8")
      (blue-2 "#79ADB0")
      (blue-3 "#699598")
      (blue-4 "#597D80")
      (blue-5 "#436D6D")
      (orange "#ffa263")
      (yellow "#ffd787")
      (yellow-1 "#c7ae95")
      (yellow-2 "#a5997d"))
  (custom-theme-set-faces
   'ebn-citrus
   ;; Basic
   `(default ((t (:background ,bg :foreground ,fg))))
   `(link ((t :foreground ,blue :underline t)))
   `(highlight ((t (:background "gray22"))))
   `(region ((t (:background "gray30"))))
   ;; --- Font lock ----------------------------------------------------
   `(font-lock-comment-face ((t (:foreground ,green+2))))
   `(font-lock-doc-face ((t (:foreground ,yellow-2))))
   `(font-lock-string-face ((t (:foreground "RosyBrown1"))))
   `(font-lock-constant-face ((t (:foreground ,orange))))
   `(font-lock-type-face ((t (:foreground ,orange))))
   `(font-lock-keyword-face ((t (:foreground ,yellow :bold t))))
   `(font-lock-warning-face ((t (:foreground ,red))))
   `(font-lock-function-name-face ((t (:inhert default))))
   `(font-lock-variable-name-face ((t (:inherit default))))
   `(font-lock-builtin-face ((t (:foreground ,yellow))))
   `(font-lock-warning-face ((t (:foreground ,red))))

   ;;  --- UI ----------------------------------------------------
   `(mode-line ((t (:background "grey20" :foreground ,fg
				:box (:color "#030303" :line-width 1)))))
   
   `(mode-line-inactive ((t (:background "gray15" :foreground "gray50"
					 :box (:color "#030303" :line-width 1)))))
   `(minibuffer-prompt ((t (:inherit default))))

   ;; IDO
   `(ido-first-match ((t (:foreground ,yellow))))
   `(ido-only-match ((t (:foreground "medium aquamarine"))))
   `(ido-subdir ((t (:foreground ,blue))))

   ;; ORG
   `(org-agenda-done ((t (:foreground ,green+2))))
   `(org-agenda-structure ((t (:foreground ,fg))))
   `(org-agenda-date-today ((t (:foreground ,yellow))))
   `(org-agenda-date-weekend-today ((t (:foreground ,yellow :slant italic))))
   `(org-code ((t (:inherit fixed-pitch))))

   ;; --- Windows divider ----------------------------------------------
   `(window-divider ((t (:foreground ,bg))))
   '(window-divider-first-pixel ((t (:inherit window-divider))))
   '(window-divider-last-pixel ((t (:inherit window-divider))))
   `(vertical-border ((t (:foreground "gray20"))))

   ;; Mail
   `(message-header-name ((t (:foreground ,yellow))))
   `(message-header-subject ((t (:foreground ,orange))))
   `(message-header-to ((t (:foreground ,fg))))
   `(message-header-other ((t (:foreground ,fg))))
   `(notmuch-message-summary-face ((t (:inhert default))))
   ;; Dired
   `(dired-directory ((t (:foreground ,blue :bold t))))
   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ebn-citrus)

;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:
