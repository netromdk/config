;;;;;;;;; COMMON CONFIGURATIONS

(column-number-mode t)                  ;; show current column
(menu-bar-mode -1)                      ;; don't show menu-bar
(tool-bar-mode -1)                      ;; same for the toolbar
(scroll-bar-mode -1)                    ;; .. and for the scrollbar
(setq inhibit-startup-message t)        ;; dont show the GNU splash screen
(transient-mark-mode t)                 ;; show selection from mark
(mouse-avoidance-mode 'jump)            ;; jump mouse away when typing
(auto-compression-mode t)               ;; browse compressed archives
(put 'upcase-region 'disabled nil)      ;; enable ``upcase-region''
(global-font-lock-mode t)               ;; syntax highlight
(setq-default indent-tabs-mode nil)     ;; use spaces instead of tabs
(fset 'yes-or-no-p 'y-or-n-p)           ;; use 'y' instead of 'yes' etc.

;; Prefer newest version of a file, especially for compiled files this is
;; useful.
(setq load-prefer-newer t)

;; Garbage collect at every 20 MB allocated instead of the default 8 MB. This
;; speeds up various things.
(setq gc-cons-threshold 20000000)

;; Set fill column to 80.
(setq-default fill-column 80)
(add-hook 'auto-fill-mode-hook
          (lambda () (set-fill-column 80)))

;; Disable visible bell because it looks ugly, but that makes the
;; audible bell and therefore we replace it with a
;; background/foreground color "flash".
(setq visible-bell nil)
(setq ring-bell-function
      `(lambda ()
         (let ((old (face-foreground 'default)))
           (set-face-foreground 'default (face-background 'default))
           (set-face-foreground 'default old))))

;; Use internal 'ls' to avoid the '--dired' argument warning when the
;; system 'ls' command doesn't support it.
(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

;; Show line number mode only while using goto-line.
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (let ((line-numbers-off-p (not linum-mode)))
    (unwind-protect
        (progn (when line-numbers-off-p
                 (linum-mode 1))
               (call-interactively 'goto-line))
      (when line-numbers-off-p
        (linum-mode -1)))))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;;;;;;;;; Scratch buffer

(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message ";; Scratch buffer..

")

;;;;;;;;; EMAIL

(setq user-mail-address "msk@nullpointer.dk")

;;;;;;;;; UTF-8 ENCODING

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;;;;;;; OS SPECIFIC

;; OSX
(if (or (eq window-system 'ns) (eq window-system 'mac))
    (progn
      ;; avoid, e.g., hiding with M-h etc. (Carbon Emacs specific)
      ;(setq mac-pass-command-to-system nil)

      ;; Let command be meta and alt be alt.
      (setq mac-option-key-is-meta nil)
      (setq mac-command-key-is-meta t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)))

;; X11
(if (eq window-system 'x)
    (set-default-font "-misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-15"))


(provide 'setup-general)
