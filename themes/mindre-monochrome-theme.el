;;; mindre-monochrome-theme.el --- A dark monochrome theme.

;; Author: Erik Bäckman
;; URL: 
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces, theme, prose

;;; Commentary:

;;; Code:

(defgroup mindre-monochrome nil
 "Customizations to change the behavior of mindre-monochrome"
 :group 'faces
 :prefix "mindre-"
 :tag "Mindre")

(deftheme mindre-monochrome
  "Mindre monochrome theme")

(let ((fg "#c1c1c1")
      (bg "#000000")
      (emph "#eeeeee")
      (sep "#444444")
      (hlt "#121212")
      (bg-hlt "#191919")
      (muted "#333333")
      (meta "#e5e5e5")
      (link "#d2d2d2")
      (link-underline "#d2d2d2")
      (vlink-underline "#ffffff")
      (header "#b6b6b6")
      (button "#aaaaaa")
      (glyph "#e8e8e8")
      (cursor "#dedede")
      (paren-match-bg "#8b8b8b")
      (paren-match-fg "#ffffff")
      (search-fg "#ffffff")
      (search-bg "#9e9e9e")
      (search-fail-bg "#d9d9d9")
      (tooltip-fg "#111111")
      (tooltip-bg "#bababa")
      (shadow "#999999")
      (secondary-bg "#000000")
      (trailing-bg "#b2b2b2")
      (fci "#dedede")
      (lazy-hlt-fg "#000000")
      (lazy-hlt-bg "#ffffff")
      (evil-rep-fg "#ffffff")
      (evil-rep-bg "#3e3e3e")
      (mode-line-fg "#d8d8d8")
      (header-line-bg "#111111")
      (mode-line-hlt "#000000")
      (mode-line-inactive "#888888")
      (error "#989898")
      (builtin "#aaaaaa")
      (string "#9b8d7f")
      (function-name "#9e9e9e")
      (keyword "#999999")
      (constant "#aaaaaa")
      (type "#bababa")
      (variable "#5f8787")
      (org-meta "#b9b9b9")
      (org-document-info "#b9b9b9")
      (org-table "#161616")
      (org-quote-fg "#f0f0f0")
      (org-quote-bg "#161616")
      (org-date "#b9b9b9")
      (org-title "#939393")
      (org-title-underline "#939393")
      (org-checkbox "#999999")
      (org-scheduled "#dddddd")
      (org-scheduled-today "#ffffff")
      (org-done "#717171")
      (org-todo "#7f7f7f")
      (org-tag "#aaaaaa")
      (org-block-line "#060606")
      (org-block-bg "#161616")
      (org-agenda-structure-fg "#aaaaaa")
      (org-agenda-structure-bg "#111111")
      (org-agenda-today-fg "#dddddd")
      (org-agenda-today-bg "#000000")
      (org-special-keyword "#777777")
      (org-sched-prev "#dadada")
      (org-agenda-done "#bfbfbf")
      (hl-line "#1e1e1e")
      (linum-hlt "#bbbbbb")
      (linum "#555555")
      (markdown-markup "#787878")
      (markdown-metadata "#777777")
      (markdown-language "#b0b0b0")
      (markdown-list "#ffffff")
      (markdown-code-bg "#161616")
      (markdown-pre-bg "#161616")
      (markdown-header-delimiter "#787878")
      (imenu "#b6b6b6"))
  (custom-theme-set-faces
   'mindre-monochrome
   `(default ((t (:background ,bg :foreground ,fg))))
   `(italic ((t (:foreground ,emph :slant italic))))
   `(highlight ((t (:background ,hlt :overline nil))))
   `(region ((t (:background "#132020"))))
   `(fringe ((t (:background ,bg))))
   `(button ((t (:inherit default :foreground ,button))))
   `(escape-glyph ((t (:foreground ,glyph))))
   `(link ((t (:underline (:color ,link-underline :style line) :foreground ,link))))
   `(link-visited ((t (:inherit link :foreground ,link :underline (:color ,vlink-underline :style line)))))
   `(cursor ((t (:background ,cursor))))
   `(show-paren-match ((t (:background ,paren-match-fg :foreground ,paren-match-bg))))
   `(isearch ((t (:foreground ,search-fg :background ,search-bg))))
   `(isearch-fail ((t (:background ,search-fail-bg))))
   `(query-replace ((t (:inherit isearch))))
   `(tooltip ((t (:inherit default :foreground ,tooltip-fg :background ,tooltip-bg))))
   `(shadow ((t (:foreground ,shadow))))
   `(secondary-selection ((t (:background ,secondary-bg))))
   `(trailing-whitespace ((t (:background ,trailing-bg))))
   `(lazy-highlight ((t (:foreground ,lazy-hlt-fg :background ,lazy-hlt-bg))))
   `(next-error ((t (:inherit region))))
   `(window-divider ((t (:background ,sep :foreground ,sep))))
   `(vertical-border ((t (:background ,sep :foreground ,sep))))
   `(evil-ex-substitute-replacement ((t (:foreground ,evil-rep-fg :background ,evil-rep-bg :underline nil))))
   `(minibuffer-prompt ((t (:weight bold :foreground ,meta))))
   `(mode-line ((t (:foreground ,mode-line-fg :background ,bg :overline ,sep :box (:line-width 3 :color ,bg)))))
   `(header-line ((t (:overline nil :background ,header-line-bg :box (:line-width 3 :color ,header-line-bg) :underline ,sep :inherit mode-line))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-highlight ((t (:background ,mode-line-hlt))))
   `(mode-line-inactive ((t (:inherit mode-line :background ,bg :foreground ,mode-line-inactive :box (:color ,bg :line-width 3)))))
   `(error ((t (:foreground ,error ))))
   `(dired-directory ((t (:inherit nil :foreground ,fg :bold t))))
   `(font-lock-comment-face ((t (:foreground ,muted ))))
   `(font-lock-builtin-face ((t (:foreground ,builtin ))))
   `(font-lock-string-face ((t (:foreground ,string))))
   `(font-lock-function-name-face ((t (:foreground ,function-name))))
   `(font-lock-keyword-face ((t (:foreground ,keyword))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face ((t (:foreground ,constant))))
   `(font-lock-doc-face ((t (:inherit font-lock-string-face))))
   `(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit bold))))
   `(font-lock-regexp-grouping-construct ((t (:inherit bold))))
   `(font-lock-type-face ((t (:foreground ,type ))))
   `(font-lock-variable-name-face ((t (:foreground ,variable))))
   `(font-lock-warning-face ((t (:inherit error))))
   `(org-level-1 ((t (:inherit default :foreground ,header))))
   `(org-level-2 ((t (:inherit default :foreground ,header))))
   `(org-level-3 ((t (:inherit default :foreground ,header))))
   `(org-level-4 ((t (:inherit default :foreground ,header))))
   `(org-level-5 ((t (:inherit default :foreground ,header))))
   `(org-level-6 ((t (:inherit default :foreground ,header))))
   `(org-level-7 ((t (:inherit default :foreground ,header))))
   `(org-level-8 ((t (:inherit default :foreground ,header))))
   `(org-meta-line ((t (:foreground ,org-meta))))
   `(org-document-info-keyword ((t (:foreground ,org-document-info))))
   `(org-document-info ((t (:inherit default :foreground ,org-document-info))))
   `(org-verbatim ((t ())))
   `(org-code ((t ())))
   `(org-table ((t (:background ,org-table))))
   `(org-formula ((t (:inherit org-table))))
   `(org-verse ((t (:inherit default :foreground ,org-quote-fg :background ,org-quote-bg))))
   `(org-quote ((t (:inherit default :foreground ,org-quote-fg :background ,org-quote-bg))))
   `(org-hide ((t (:foreground ,bg))))
   `(org-indent ((t (:inherit org-hide))))
   `(org-date ((t (:foreground ,org-date :underline nil))))
   `(org-document-title ((t (:inherit default :foreground ,org-title))))
   `(org-checkbox ((t (:weight bold :foreground ,org-checkbox))))
   `(org-done ((t (:foreground ,org-done))))
   `(org-todo ((t (:foreground ,org-todo))))
   `(org-tag ((t (:foreground ,org-tag))))
   `(org-block-begin-line ((t (:background ,org-block-line))))
   `(org-block-end-line ((t (:background ,org-block-line))))
   `(org-block ((t (:background ,org-block-bg ))))
   `(org-priority ((t (:weight normal))))
   `(org-agenda-structure ((t (:foreground ,org-agenda-structure-fg :background ,bg :box (:line-width 3 :color ,bg) :underline ,org-agenda-structure-bg))))
   `(org-scheduled ((t (:foreground ,org-scheduled))))
   `(org-scheduled-today ((t (:foreground ,org-scheduled-today))))
   `(org-agenda-date-weekend ((t (:inherit org-agenda-structure))))
   `(org-agenda-date-today ((t (:box (:line-width 3 :color ,org-agenda-today-bg) :foreground ,org-agenda-today-fg :background ,org-agenda-today-bg))))
   `(org-special-keyword ((t (:foreground ,org-special-keyword))))
   `(org-scheduled-previously ((t (:foreground ,org-sched-prev))))
   `(org-agenda-done ((t (:foreground ,org-agenda-done))))
   `(org-footnote ((t (:foreground ,link))))
   `(hl-line ((t (:background ,hl-line))))
   `(linum-highlight-face ((t (:foreground ,linum-hlt))))
   `(linum ((t (:foreground ,linum))))
   `(line-number ((t (:foreground ,linum))))
   `(line-number-current-line ((t (:foreground ,linum-hlt))))
   `(markdown-header-face-1 ((t (:foreground ,header :inherit default))))
   `(markdown-header-face-2 ((t (:foreground ,header :inherit default))))
   `(markdown-header-face-3 ((t (:foreground ,header :inherit default))))
   `(markdown-header-face-4 ((t (:foreground ,header :inherit default))))
   `(markdown-header-face-5 ((t (:foreground ,header :inherit default))))
   `(markdown-header-face-6 ((t (:foreground ,header :inherit default))))
   `(markdown-header-face-7 ((t (:foreground ,header :inherit default))))
   `(markdown-header-face-8 ((t (:foreground ,header :inherit default))))
   `(markdown-markup-face ((t (:foreground ,markdown-markup))))
   `(markdown-inline-code-face ((t ())))
   `(markdown-metadata-key-face ((t (:foreground ,markdown-metadata))))
   `(markdown-metadata-value-face ((t (:foreground ,fg))))
   `(markdown-language-keyword-face ((t (:foreground ,markdown-language))))
   `(markdown-list-face ((t (:foreground ,markdown-list))))
   `(markdown-code-face ((t (:foreground ,fg :background ,markdown-code-bg))))
   `(markdown-pre-face ((t (:color ,fg :background ,markdown-pre-bg))))
   `(markdown-header-delimiter-face ((t (:foreground ,markdown-header-delimiter))))
   `(markdown-header-rule-face ((t (:foreground ,markdown-header-delimiter))))
   `(markdown-url-face ((t (:foreground ,link))))
   `(imenu-list-entry-face-0 ((t (:foreground ,imenu))))
   `(imenu-list-entry-face-1 ((t (:foreground ,imenu))))
   `(imenu-list-entry-face-2 ((t (:foreground ,imenu))))
   `(imenu-list-entry-face-3 ((t (:foreground ,imenu))))
   `(imenu-list-entry-face-4 ((t (:foreground ,imenu))))
   `(imenu-list-entry-face-5 ((t (:foreground ,imenu))))
   `(ein:cell-input-area ((t (:background ,org-block-bg))))
   `(ein:cell-input-prompt ((t (:foreground ,org-tag :background ,bg))))
   `(ein:cell-output-prompt ((t (:foreground ,org-tag :background ,bg))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "gray80"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "gray70"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "gray60"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "gray50"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "gray40"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "gray30"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "gray20"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "gray10"))))
   `(erc-prompt-face ((t (:foreground ,variable :background nil))))
   `(erc-notice-face ((t (:foreground ,string))))
   `(sly-mrepl-output-face ((t (:foreground "dark sea green")))))
  )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

(provide 'mindre-monochrome)
;;; mindre-monochrome-theme ends here
