;; Extended to suit my needs.

;;; mustang-theme.el --- port of vim's mustang theme
;; Author: martin haesler
;; URL: http://github.com/mswift42/mustang-theme
;; Version: 20130920.939
;;; X-Original-Version: 0.3

;; original vim theme by Henrique C.Alves
;;(http://hcalves.deviantart.com/art/Mustang-Vim-Colorspcheme-98974484)

(deftheme mustang-netrom)

(custom-theme-set-faces
  'mustang-netrom 
        '(default ((t (:background "#202020" :foreground "#e2e2e5"))))
        '(font-lock-builtin-face ((t (:foreground "#808080"))))
        '(region ((t (:background "#3c414c" :foreground "#faf4c6"))))
        '(highlight ((t (:background "#3c414c"))))
        '(hl-line ((t (:background "#393939"))))
	'(fringe ((t (:background "#232323" :foreground "#cfbfad"))))
	;'(cursor ((t (:background "#626262"))))
        '(cursor ((t (:background "#bb7600"))))
        '(show-paren-match-face ((t (:background "#ff9800" :foreground "#000000"))))
        '(isearch ((t (:bold t :foreground "#202020" :background "#e2e2e5"))))
        '(mode-line ((t (:background "#151515" :foreground "#808080" :box (:line-width 1 :color "#000000" :style released-button) :weight bold))))
        '(mode-line-inactive ((t (:background "#303030" :foreground "#696969" :box (:line-width 1 :color "#000000" :style released-button)))))
        '(mode-line-buffer-id ((t (:bold t :foreground "#ff9800" :background "#202020"))))
        '(minibuffer-prompt ((t (:bold t :foreground "#708090"))))
        '(default-italic ((t (:italic t))))
	'(font-lock-negation-char-face ((t (:foreground "#ff9800"))))
        ;'(font-lock-comment-delimiter-face ((t (:foreground "#808080"))))
        '(font-lock-comment-delimiter-face ((t (:foreground "#908080"))))
	;'(font-lock-comment-face ((t (:foreground "#808080"))))
        '(font-lock-comment-face ((t (:foreground "#908080"))))
	'(font-lock-constant-face ((t (:foreground "#ff9800"))))
        '(font-lock-doc-face ((t (:foreground "#7e8aa2"))))
        '(font-lock-function-name-face ((t (:foreground "#ffffff"))))
        '(font-lock-keyword-face ((t (:bold t :foreground "#808080"))))
        '(font-lock-preprocessor-face ((t (:foreground "#ff9800"))))
        '(font-lock-reference-face ((t (:bold t :foreground "#808bed"))))
        '(font-lock-string-face ((t (:foreground "#b1d631"))))
        '(font-lock-type-face ((t (:foreground "#7e8aa2"))))
        '(font-lock-variable-name-face ((t (:foreground "#ff9800"))))
        '(font-lock-warning-face ((t (:foreground "#ffffff" :background "#ff6523"))))
	'(link ((t (:foreground "#ff9800"))))
        '(company-tooltip ((t (:background "#282828" :foreground "#ffffff"))))
        '(company-tooltip-common ((t (:background "#282828" :foreground "#ff9800"))))
        '(company-tooltip-selection ((t (:background "#353535" :foreground "#ffffff"))))
        '(company-tooltip-common-selection ((t (:background "#353535" :foreground "#b1d631"))))
        '(company-scrollbar-bg ((t (:background "#282828"))))
        '(company-scrollbar-fg ((t (:background "#808080"))))
        '(helm-candidate-number ((t (:background "#202020" :foreground "#ff9800"))))
        '(helm-selection ((t (:background "#282828" :foreground "#b1d631" :underline nil :weight bold))))
        '(helm-source-header ((t (:background "#353535" :foreground "white" :weight bold :height 1.3 :family "Sans Serif"))))
        '(helm-match ((t (:foreground "#ff9800"))))
        '(helm-swoop-target-line-block-face ((t nil)))
        '(helm-swoop-target-line-face ((t (:background "#3c414c"))))
        '(helm-swoop-target-word-face ((t (:background "#b1d631" :foreground "#000000"))))
        '(highlight-current-line-face ((t (:background "#282828"))))
        '(linum ((t (:inherit (shadow default) :foreground "#696969" :height 0.9))))
        '(linum-highlight-face ((t (:inherit linum :foreground "#b1d631"))))
        '(anzu-mode-line ((t (:foreground "#b1d631" :weight bold))))
        '(anzu-replace-to ((t (:foreground "#ff9800" :slant italic :weight bold))))
        '(semantic-highlight-func-current-tag-face ((t (:background "#353535" :weight bold))))
        '(sml/global ((t :foreground "gray50" :inverse-video nil)))
        '(sml/prefix ((t :inherit sml/global :foreground "#bf6000")))
        '(sml/read-only ((t :inherit sml/not-modified :foreground "DeepSkyBlue")))
        '(sml/col-number ((t (:inherit sml/global :foreground "#808bed"))))
        '(sml/filename ((t (:inherit sml/global :foreground "#ff9800" :weight bold))))
        '(sml/modes ((t (:inherit sml/global :foreground "#b1d631"))))
        '(sml/position-percentage ((t (:foreground "#df9f2d" :weight normal))))
        '(diff-added ((t (:background "#224422"))))
        '(diff-removed ((t (:background "#442222"))))
        '(diff-file-header ((t (:foreground "#ff9800"))))
        '(diff-hunk-header ((t (:foreground "#b1d631"))))
        '(magit-item-highlight ((t (:background "#303030"))))
        '(magit-tag ((t (:background "#343434" :foreground "#b1d613" :box 1))))
        '(magit-log-head-label-tags ((t (:inherit magit-tag))))
        '(magit-log-reflog-label-commit ((t (:inherit magit-tag))))
        '(magit-log-reflog-label-merge ((t (:inherit magit-tag))))
        '(magit-log-author ((t (:foreground "#ff9800"))))
        '(magit-log-sha1 ((t (:foreground "#b1d631"))))
        '(git-commit-nonempty-second-line-face ((t nil)))
        '(git-commit-overlong-summary-face ((t nil)))
	'(org-hide ((t (:foreground "#708090"))))
        '(org-level-1 ((t (:bold t :foreground "#808080" :height 1.1))))
        '(org-level-2 ((t (:bold nil :foreground "#7e8aa2" :height 1.1))))
        '(org-level-3 ((t (:bold t :foreground "#df9f2d" :height 1.1))))
        '(org-level-4 ((t (:bold nil :foreground "#af4f4b" :height 1.0))))
        '(org-date ((t (:underline t :foreground "#f0ad6d") :height 1.1)))
        '(org-footnote  ((t (:underline t :foreground "#ad600b"))))
        '(org-link ((t (:underline t :foreground "#ff9800" ))))
        '(org-special-keyword ((t (:foreground "#ff9800"))))
        '(org-verbatim ((t (:foreground "#eeeeec" :underline t :slant italic))))
        '(org-block ((t (:foreground "#7e8aa2"))))
        '(org-quote ((t (:inherit org-block :slant italic))))
        '(org-verse ((t (:inherit org-block :slant italic))))
        '(org-todo ((t (:bold t :foreground "#ffffff"))))
        '(org-done ((t (:bold t :foreground "#708090"))))
        '(org-warning ((t (:underline t :foreground "#ff0000"))))
        '(org-agenda-structure ((t (:weight bold :foreground "#df9f2d"))))
        '(org-agenda-date ((t (:foreground "#ff9800" :height 1.2))))
        '(org-agenda-date-weekend ((t (:weight normal :foreground "#808bed"))))
        '(org-agenda-date-today ((t (:weight bold :foreground "#ff9800" :height 1.4))))
	'(org-scheduled ((t (:foreground "#eeeeec"))))
	'(font-latex-bold-face ((t (:foreground "#cd8b00"))))
	'(font-latex-italic-face ((t (:foreground "#808bed" :italic t))))
	'(font-latex-string-face ((t (:foreground "#708090"))))
	'(font-latex-match-reference-keywords ((t (:foreground "#708090"))))
	'(font-latex-match-variable-keywords ((t (:foreground "#708090"))))
	'(ido-only-match ((t (:foreground "#ff9800"))))
	'(org-sexp-date ((t (:foreground "#808080"))))
	'(ido-first-match ((t (:foreground "#b1d631"))))
	'(gnus-header-content ((t (:foreground "#ff9810"))))
	'(gnus-header-from ((t (:foreground "#f0e16a"))))
	'(gnus-header-name ((t (:foreground "#ff9800"))))
	'(gnus-header-subject ((t (:foreground "#ff8800"))))
	'(mu4e-view-url-number-face ((t (:foreground "#7e8aa2"))))
	'(mu4e-cited-1-face ((t (:foreground "#df9f2d"))))
	'(mu4e-cited-7-face ((t (:foreground "#808bed"))))
	'(slime-repl-inputed-output-face ((t (:foreground "#ff9800")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'mustang-netrom)

;;; mustang-netrom-theme.el ends here
