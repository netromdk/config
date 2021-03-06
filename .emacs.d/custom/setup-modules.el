;;;;;;;;; IDO/FLX

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; Disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;;;;;;;;; SPELLING

;; Set aspell as spell program
(setq ispell-program-name "aspell")

;; Speed up aspell: ultra | fast | normal
(setq ispell-extra-args '("--sug-mode=normal"))

;; Flyspell activation for text mode
;(add-hook 'text-mode-hook
;          (lambda () (flyspell-mode t)))

;; Remove Flyspell from some sub modes of text mode
;(dolist (hook '(change-log-mode-hook
;                log-edit-mode-hook))
;  (add-hook hook (lambda () (flyspell-mode -1))))

(defun da-spell ()
  "Set ispell to use Danish dictionary (globally)"
  (interactive)
  (ispell-change-dictionary "dansk" "global"))

(defun en-spell ()
  "Set ispell to use English dictionary (globally)"
  (interactive)
  (ispell-change-dictionary "english" "global"))

(defalias 'sb 'ispell-buffer)

;;;;;;;;; C & C++

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq tab-width 2)
            (setq c-basic-offset tab-width)
            (setq indent-tabs-mode nil)))

;; indent on CR
(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

;; Open .h files in c++ mode.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;;;;;;;; Text

;; set auto-fill-mode and org minor modes for lists and tables.
(add-hook 'text-mode-hook
          (lambda ()
            (auto-fill-mode t)
            (orgstruct-mode t)
            (orgtbl-mode t)))

;;;;;;;;; (La)TeX

;; set auto-fill-mode
(add-hook 'latex-mode-hook
          (lambda () (auto-fill-mode t)))
(setenv "TEXINPUTS" ".:~/latex/:")

;;;;;;;;; CSS-mode

(setq auto-mode-alist (append '(("\\.css$" . css-mode)
                                ("\\.style$" . css-mode))
                              auto-mode-alist))

;;;;;;;;; PHP-mode

(require 'php-mode)

;;;;;;;;; CMake

(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;;;;;;;;; JavaScript

(add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))

;;;;;;;;; SH

(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2
                  sh-indentation 2)))

;;;;;;;;; CEDET

;; (require 'cc-mode)
;; (require 'semantic)
;; (require 'semantic/ia)

;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
;; (global-semantic-idle-summary-mode 0)
;; (global-semantic-idle-completions-mode 0)
;; (global-semantic-idle-breadcrumbs-mode 0)
;; (global-semantic-stickyfunc-mode 1)
;; (global-semantic-highlight-func-mode 1)
;; (global-semantic-highlight-edits-mode 0)
;; (global-semantic-decoration-mode 0)
;; (global-semantic-show-parser-state-mode 1)

;; (semantic-add-system-include "/opt/local/include")

;; (defun custom-cedet-hook ()
;;   (local-set-key [(control return)] 'semantic-ia-complete-symbol)
;;   (local-set-key "\C-cc" 'semantic-complete-analyze-inline)
;;   (local-set-key "\C-cr" 'semantic-complete-analyze-and-replace)
;;   (local-set-key "\C-ci" 'semantic-decoration-all-include-summary)
;;   (local-set-key "\C-cj" 'semantic-ia-fast-jump)
;;   (local-set-key "\C-cs" 'semantic-symref)
;;   ;(local-set-key "." 'semantic-complete-self-insert)
;;   ;(local-set-key ">" 'semantic-complete-self-insert)
;; )
;; (add-hook 'c-mode-common-hook 'custom-cedet-hook)

;; ; Allow Semanticdb use databases generated by global (gtags).
;; ;(semanticdb-enable-gnu-global-databases 'c-mode)
;; ;(semanticdb-enable-gnu-global-databases 'c++-mode)

;; (semantic-mode 1)

;;;;;;;;; Company ("complete-anything") 

(require 'company)

(add-to-list 'company-backends 'company-c-headers)
(add-hook 'after-init-hook 'global-company-mode)

;; Use C-tab globally for company-complete but use C-tab for gtags
;; completion locally for C like modes, but in that case keep C-return
;; with the default completion.
(global-set-key (kbd "C-<tab>") 'company-complete)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (local-set-key (kbd "C-<tab>") 'company-gtags)
              (local-set-key (kbd "C-<return>") 'company-complete))))

(company-mode 1)

;;;;;;;;; CEDET utility stuff

;; (defvar c-files-regex ".*\\.\\(c\\|cc\\|cpp\\|h\\|hpp\\)"
;;   "A regular expression to match any c/c++ related files under a directory")

;; (defvar qt-files-regex "Q.*"
;;   "A regular expression to match any c/c++ related files under a directory")
 
;; (defun my-semantic-parse-dir (root regex)
;;   "
;;    This function is an attempt of mine to force semantic to
;;    parse all source files under a root directory. Arguments:
;;    -- root: The full path to the root directory
;;    -- regex: A regular expression against which to match all files in the directory
;;   "
;;   (let (
;;         ;;make sure that root has a trailing slash and is a dir
;;         (root (file-name-as-directory root))
;;         (files (directory-files root t ))
;;        )
;;     ;; remove current dir and parent dir from list
;;     (setq files (delete (format "%s." root) files))
;;     (setq files (delete (format "%s.." root) files))
;;     ;; remove any known version control directories 
;;     (setq files (delete (format "%s.git" root) files))
;;     (setq files (delete (format "%s.hg" root) files))
;;     (while files
;;       (setq file (pop files))
;;       (if (not(file-accessible-directory-p file))
;;           ;;if it's a file that matches the regex we seek
;;           (progn (when (string-match-p regex file)
;;                    (save-excursion
;;                      (semanticdb-file-table-object file))
;;            ))
;;           ;;else if it's a directory
;;           (my-semantic-parse-dir file regex)
;;       )
;;      )
;;   )
;; )
 
;; (defun my-semantic-parse-current-dir (regex)
;;   "Parses all files under the current directory matching regex
;;   "
;;   (my-semantic-parse-dir (file-name-directory(buffer-file-name)) regex)
;; )
 
;; (defun lk-parse-curdir-c ()
;;   "Parses all the c/c++ related files under the current directory
;;    and inputs their data into semantic
;;   "
;;   (interactive)
;;   (my-semantic-parse-current-dir c-files-regex)
;; )
 
;; (defun lk-parse-dir-c (dir)
;;   "Prompts the user for a directory and parses all c/c++ related
;;    files under the directory
;;   "
;;   (interactive (list (read-directory-name "Provide the directory to search in:")))
;;   (my-semantic-parse-dir (expand-file-name dir) c-files-regex)
;; ) 

;; (defun lk-parse-dir-qt (dir)
;;   "Prompts the user for a directory and parses all QT c/c++
;;    related files under the directory
;;   "
;;   (interactive (list (read-directory-name "Provide the directory to search in:")))
;;   (my-semantic-parse-dir (expand-file-name dir) qt-files-regex)
;; )

;;;;;;;;; Recent files mode

(recentf-mode 1)
(global-set-key "\C-xr" 'recentf-open-files)

;;;;;;;;; Multiple cursors

(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m") 'mc/mark-all-like-this)

;;;;;;;;; Magit mode

(setq magit-revert-item-confirm t)
(setq magit-save-some-buffers t)
(setq magit-auto-revert-mode nil) ;; Do _not_ auto-revert!
(setq magit-revert-buffers t)
(setq magit-last-seen-setup-instructions "1.4.0") ;; Silence latest info.
(setq magit-push-always-verify nil) ;; Only ask when upstream is not conf'ed!

(global-set-key (kbd "C-x g") 'magit-status)

;;;;;;;;; FIC mode (marks TODO, FIXME etc. clearly)

(setq fic-background-color "#ff9800")
(setq fic-foreground-color "#000000")
(setq fic-highlighted-words (quote ("FIXME" "TODO" "BUG" "KLUDGE" "TEMP")))

(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)

;;;;;;;;; Highlight current line mode

(require 'highlight-current-line)
(highlight-current-line-minor-mode)
(highlight-current-line-on t)

;;;;;;;;; hlinum-mode - highlights current line number in margin

(require 'hlinum)
(hlinum-activate)

;; Does not show the line number mode per default unless goto-line is used,
;; which will enable it only while choosinga line to jump to. Defined in
;; "setup-general.el".

;;;;;;;;; Rainbow mode - highlights hexcolors, like #aabbcc and Red

(add-hook 'prog-mode-hook 'rainbow-mode)

;;;;;;;;; Ace jump mode

;; Jump to symbol
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Jump back to before taking an ace jump
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back" t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;;;;;;;; Helm

(setq helm-candidate-number-limit 100)
(setq helm-display-source-at-screen-top t)
(setq helm-exit-idle-delay 0)
(setq helm-full-frame nil)
(setq helm-buffers-fuzzy-matching t)
(setq helm-ff-file-name-history-use-recentf t)
(setq helm-split-window-default-side (quote below))
(setq helm-reuse-last-window-split-state nil)
(setq helm-split-window-in-side-p t) ;; split in same window
(setq helm-quick-update t) ;; don't show invisible candidates

(setq helm-swoop-split-direction (quote split-window-vertically))
(setq helm-swoop-split-with-multiple-windows t)

(setq helm-gtags-maximum-candidates 1000)

;; Use flx for better search results.
(helm-flx-mode +1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)

;; Enhance the help menu using helm functionality.
(define-key 'help-command (kbd "a") 'helm-apropos)
(define-key 'help-command (kbd "r") 'helm-info-emacs)
(define-key 'help-command (kbd "C-l") 'helm-locate-library)
(define-key 'help-command (kbd "SPC") 'helm-all-mark-rings)

;; Activate helm-swoop on isearch results.
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key isearch-mode-map (kbd "M-I") 'helm-multi-swoop-all-from-isearch)

;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; Set key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)))

;;;;;;;;; objc-mode

(setq auto-mode-alist (append '(("\\.mm$" . objc-mode)) auto-mode-alist))

;;;;;;;;; anzu-mode - always shows the number of matches for searches

(defalias 'qrr 'anzu-query-replace-regexp)

;; Don't add to modeline because spaceline will show anzu.
(setq anzu-cons-mode-line-p nil)

;; Deactivate region, if any, when using anzu replace functionality because it's
;; hard to see the search results with an active region as well.
(setq anzu-deactivate-region t)

;; Change the mode-line text summary of search/replace results.
(defun msk-anzu-update-func (here total)
  (when anzu--state
    (let ((status (cl-case anzu--state
                    (search (format "%d/%d" here total))
                    (replace-query (format "%d replaces" total))
                    (replace (format "%d/%d" here total)))))
      (propertize status 'face 'anzu-mode-line))))
(setq anzu-mode-line-update-function #'msk-anzu-update-func)

(global-anzu-mode t)

;;;;;;;;; window-numbering mode

(require 'window-numbering)

(window-numbering-mode t)

;;;;;;;;; smart-mode-line

;; (setq sml/numbers-separator " ")

;; (sml/setup)

;; (add-to-list 'sml/replacer-regexp-list '("/Volumes/Luxion/" ":LUX:") t)
;; (add-to-list 'sml/prefix-face-list '(":LUX:" sml/git))

;; (add-to-list 'sml/replacer-regexp-list '("/Volumes/Burator/" ":B:") t)
;; (add-to-list 'sml/prefix-face-list '(":B:" sml/git))

;; (add-to-list 'sml/replacer-regexp-list '(".*[Ss]vn" ":SVN:") t)
;; (add-to-list 'sml/prefix-face-list '(":SVN:" sml/git))

;;;;;;;;; spaceline

(require 'spaceline-config)
(spaceline-spacemacs-theme)

;; Don't show unicode window numbers because they are too small to be seen fast
;; and clearly.
(setq spaceline-window-numbers-unicode nil)

(setq spaceline-minor-modes-separator " ")

(spaceline-toggle-process-on)
(spaceline-toggle-selection-info-on)
(spaceline-toggle-hud-off)

;;;;;;;;; smartparens mode

(require 'smartparens-config)
(show-smartparens-global-mode 1)
(smartparens-global-mode 1)

;;;;;;;;; zygospore - restore buffers

(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;;;;;;;;; on-screen mode to track reading markers

(setq on-screen-highlight-method (quote fringe))

(global-on-screen-mode 1)

;;;;;;;;; saveplace - saves the buffer positions and restores them

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saveplace.txt")

;;;;;;;;; Show empty lines at the end like Vim

(global-vim-empty-lines-mode)

;;;;;;;; dash-at-point

(global-set-key "\C-cd" 'dash-at-point)

(add-to-list 'dash-at-point-mode-alist '(c-mode . "c,manpages"))
(add-to-list 'dash-at-point-mode-alist '(c++-mode . "cpp,qt,c,manpages,lux"))
(add-to-list 'dash-at-point-mode-alist '(python-mode . "py,flask"))
(add-to-list 'dash-at-point-mode-alist '(cmake-mode . "cmake"))
(add-to-list 'dash-at-point-mode-alist '(js-mode . "js"))

;;;;;;;; helm-ag

(setq helm-ag-base-command "ag --nocolor --nogroup --smart-case --stats")

;;;;;;;;; markdown-mode

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Turn off auto-fill-mode beacuse markdown is sensitive about newlines.
(add-hook 'markdown-mode-hook
          (lambda ()
            (auto-fill-mode 0)
            (visual-line-mode t)))

;;;;;;;;; define-word

;; Show definitions of a word at point.
(global-set-key (kbd "C-c ?") 'define-word-at-point)

;;;;;;;;; expand-region

;; Expand region to select text intelligently.
(global-set-key (kbd "C-<") 'er/expand-region)
(global-set-key (kbd "C->") 'er/contract-region)

;;;;;;;;; whitespace

;; Visualize certain like space at end of line and trailing characters after
;; fill column.
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))

;; Enable whitespace for programming modes.
(add-hook 'prog-mode-hook 'whitespace-mode)

;;;;;;;; rainbow-delimiters

;; For programming modes, show delimiters with variying colors to easily
;; distinguish between them.
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;;;;;;; diminish

(require 'diminish)

(eval-after-load "anzu"
  '(diminish 'anzu-mode))

(eval-after-load "vim-empty-lines-mode"
  '(diminish 'vim-empty-lines-mode))

(eval-after-load "abbrev"
  '(diminish 'abbrev-mode "Abv"))

(eval-after-load "fic-mode"
  '(diminish 'fic-mode))

(eval-after-load "company"
  '(diminish 'company-mode "Comp"))

(eval-after-load "smartparens"
  '(diminish 'smartparens-mode))

(eval-after-load "org-table"
  '(diminish 'orgtbl-mode "OrgT"))

(eval-after-load "org"
  '(diminish 'orgstruct-mode "OrgS"))

(eval-after-load "rainbow-mode"
  '(diminish 'rainbow-mode))

(eval-after-load "helm-gtags"
  '(diminish 'helm-gtags-mode "HGt"))


(provide 'setup-modules)
