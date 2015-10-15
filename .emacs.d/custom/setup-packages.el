;;;;;;;;; Packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; The list of packages that should be automatically installed for
;; this configuration.
(setq pkgs (list
            'ace-jump-mode
            'anzu
            'cmake-mode
            'company
            'company-c-headers
            'dash-at-point
            'define-word
            'diminish
            'expand-region
            'fic-mode
            'flx-ido
            'flx-isearch
            'gitconfig-mode
            'gitignore-mode
            'helm
            'helm-ag
            'helm-flx
            'helm-gtags
            'helm-package
            'helm-swoop
            'highlight-current-line
            'hlinum
            'magit
            'markdown-mode
            'mu4e-maildirs-extension
            'multiple-cursors
            'on-screen
            'package-safe-delete
            'php-mode
            'rainbow-mode
            ;;'smart-mode-line
            'smartparens
            'spaceline
            'vim-empty-lines-mode
            'window-numbering
            'zygospore
            ))

;; Refresh package archive if a package is to be installed or it's not
;; in the archive list.
(setq n 0)
(dolist (pkg pkgs)
  (unless (or (package-installed-p pkg)
              (assoc pkg package-archive-contents))
    (setq n (+ n 1))))
(when (> n 0)
  (package-refresh-contents))

;; Go through the list and install those that aren't installed.
(dolist (pkg pkgs)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Aliases for viewing packages.
(defalias 'lp 'package-list-packages)
(defalias 'lpn 'package-list-packages-no-fetch)


(provide 'setup-packages)
