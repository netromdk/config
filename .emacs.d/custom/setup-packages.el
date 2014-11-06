;;;;;;;;; Packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; The list of packages that should be automatically installed for
;; this configuration.
(setq pkgs (list
            'ace-jump-mode
            'ack
            'anzu
            'cmake-mode
            'company
            'company-c-headers
            'fic-mode
            'git-commit-mode
            'git-rebase-mode
            'gitconfig-mode
            'gitignore-mode
            'helm
            'helm-gtags
            'helm-package
            'helm-swoop
            'highlight-current-line
            'hlinum
            'magit
            'magit-svn
            'multiple-cursors
            'on-screen
            'package-safe-delete
            'rainbow-mode
            'smart-mode-line
            'smartparens
            'zygospore))

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


(provide 'setup-packages)
