;; ------------------------
;;   Emacs Configurations
;; ------------------------

;; Copyright (C) 2007-2014 - Morten Slot Kristensen
;;
;; I got inspiration to some of the tweaks elsewhere. One of those are
;; Ian Zerny. If I have wrongfully forgotten to give credit to the
;; original source of something please do inform me of the mistake.
;;
;; This file is free software. You may redistribute it and/or modify
;; it under the terms of the GNU General Public License, version 2 or
;; later as published by the Free Software Foundation.
;;
;; The file is distributed AS IS and WITHOUT ANY WARRANTY. I hope you
;; will find it useful and I welcome feedback and
;; modifications/improvements.

;; Add the ~/.emacs.d dir to the loadpath
(setq load-path (cons "~/.emacs.d/" load-path))

;;;;;;;;; COMMON CONFIGURATIONS

(show-paren-mode t)                     ;; show matching parenthesis
(column-number-mode t)                  ;; show current column
(menu-bar-mode -1)                      ;; don't show menu-bar
(tool-bar-mode -1)                      ;; same for the toolbar
(scroll-bar-mode -1)                    ;; .. and for the scrollbar
(setq inhibit-startup-message t)        ;; dont show the GNU splash screen
(transient-mark-mode t)                 ;; show selection from mark
(mouse-avoidance-mode 'jump)            ;; jump mouse away when typing
(setq visible-bell nil)                 ;; turn off bip warnings
(auto-compression-mode t)               ;; browse tar archives
(put 'upcase-region 'disabled nil)      ;; enable ``upcase-region''
(global-font-lock-mode t)               ;; syntax highlight
(setq-default indent-tabs-mode nil)     ;; use spaces instead of tabs
(fset 'yes-or-no-p 'y-or-n-p)           ;; use 'y' instead of 'yes' etc.

;;;;;;;;; MAC OS X SPECIFIC

(if (or (eq window-system 'ns) (eq window-system 'mac))
    (progn
      ;; avoid, e.g., hiding with M-h etc. (Carbon Emacs specific)
      ;(setq mac-pass-command-to-system nil)

      ;; Let command be meta and alt be alt.
      (setq mac-option-key-is-meta nil)
      (setq mac-command-key-is-meta t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)))

;;;;;;;;; UTF-8 ENCODING

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;;;;;;; CUSTOM COLORS & FONTS

(set-background-color "black")
(set-foreground-color "white")
(set-cursor-color "white")

;; under X11
(if (eq window-system 'x)
    (set-default-font "-misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-15"))

;;;;;;;;; CUSTOM KEYBINDINGS

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-n") 'scroll-up-one-line)
(global-set-key (kbd "M-p") 'scroll-down-one-line)
(global-set-key (kbd "M-P") 'previous-user-buffer) 
(global-set-key (kbd "M-N") 'next-user-buffer) 
(global-set-key (kbd "C-c o") 'ff-find-other-file)

;;;;;;;;; CUSTOM ALIASES

(defalias 'qrr 'query-replace-regexp)
(defalias 'sb 'ispell-buffer)
(defalias 'rp 'run-petite-chez-scheme)

;;;;;;;;; FUNCTIONS

;; convert current buffer to unix EOLs
(defun to-unix-eol ()
  "Change current buffer's line ending to unix convention."
  (interactive)
  (progn
    (set-buffer-file-coding-system 'unix) ; or 'mac or 'dos
    (save-buffer)))

(defun open-with-finder ()
  "Show current buffer-file, or directory if in Dired-mode, in
  Finder (OSX specific)."
  (interactive)
  (if (eq 'dired-mode major-mode)
      (shell-command "open .")
    (shell-command (concat "open -R '" (concat buffer-file-name "'")))))

;; goto next user buffer (no *Messages*, *eshell* etc.)
(defun next-user-buffer ()
  "Switch to next buffer in cyclic order. User buffers are those
  not starting with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name))
                (< i 50)) ; we need to have some maximum..
      (setq i (1+ i))
      (next-buffer))))

;; goto previous user buffer (no *Messages*, *eshell* etc.)
(defun previous-user-buffer ()
  "Switch to previous buffer in cyclic order. User buffers are
  those not starting with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name))
                (< i 50)) ; we need to have some maximum..
      (setq i (1+ i))
      (previous-buffer))))

;; Reload the .emacs conf-file
(defun reload-conf ()
  "Reloads ~/.emacs"
  (interactive)
  (load-file "~/.emacs"))

;; Opens (finds) .emacs in another buffer
(defun open-conf () 
  "Opens ~/.emacs for editing"
  (interactive)
  (find-file-other-window "~/.emacs"))

;; Scroll one line up / down
(defun scroll-down-one-line ()
  "Scrolls down one line"
  (interactive)
  (scroll-down 2))

(defun scroll-up-one-line ()
  "Scrolls up one line"
  (interactive)
  (scroll-up 2))

;; Set exec path to be the same as the one from the shell
(defun set-exec-path-from-shell-path()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
  This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-path)

;;;;;;;;; Packages

(require 'package)
(add-to-list 'package-archives 
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;;;;;;;;; Luxion related

(defun wrap-luxion-count (start end)
  "Determines the longest horizontal stretch of the function selected."
  (progn
    (goto-char start)
    (let (max)
      (progn 
        (setq max 0)
        (while (search-forward "\n" end 't)
          (progn
            ;; Move back once to get the correct column. The cursor is
            ;; on the first char on the next line after searching for
            ;; "\n".
            (backward-char) 
            (if (> (current-column) max)
                (setq max (current-column)))
            (forward-char))))
      ;; Subtract three to compensate for "// ".
      (- max 3))))

;; Luxion: Wraps // ***.. before and after a function (the region
;; selected). Place region at the start of the function and end it
;; after "{" on the next line.
(defun wrap-luxion-function (start end)
 "Put comments around Luxion function."
 (interactive "r")    
 (let (count str)
   (progn
     (save-excursion 
       (setq count (wrap-luxion-count start end))
       (setq str (concat "// " (make-string count ?*) "\n"))
       (goto-char end) 
       (insert str)
       (goto-char start) 
       (insert str)))))

(global-set-key (kbd "C-M-l") 'wrap-luxion-function) 

;;;;;;;;; EMAIL

(setq user-mail-address "msk@nullpointer.dk")

;;;;;;;;; IDO

(ido-mode t)
(setq ido-enable-flex-matching t)

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

;; Change to danish dict
(defun da-spell ()
  "Set ispell to use Danish dictionary"
  (interactive)
  (ispell-change-dictionary "dansk"))

;; Change to english dict
(defun en-spell ()
  "Set ispell to use English dictionary"
  (interactive)
  (ispell-change-dictionary "english"))

;;;;;;;;; C & C++

(add-hook 'c-mode-common-hook
          (lambda ()
            (flyspell-prog-mode)
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

;; set auto-fill-mode
(add-hook 'text-mode-hook
          (lambda () (auto-fill-mode t)))

;;;;;;;;; (La)TeX

;; set auto-fill-mode
(add-hook 'latex-mode-hook
          (lambda () (auto-fill-mode t)))
(setenv "TEXINPUTS" ".:~/latex/:")

;;;;;;;;; Ruby

(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))

;;;;;;;;; SML

;(autoload 'sml-mode "sml-mode" () t)
(autoload 'sml-mode "sml-mode-color" () t)
(setq auto-mode-alist (cons '("\\.sml$" . sml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sig$" . sml-mode) auto-mode-alist))

;;;;;;;;; Scheme

;; Petite-chez-scheme-mode
(autoload 'petite-chez-scheme-mode "petite-chez-scheme-mode-color" () t)
(setq auto-mode-alist
      (cons '("\\.scm$" . petite-chez-scheme-mode)
	    (cons '("\\.sim$" . petite-chez-scheme-mode)
		  auto-mode-alist)))

(defun run-petite-chez-scheme ()
  "Run an inferior Petite Chez Scheme process, input and output via buffer *Petite Chez Scheme*."
  (interactive)
  (require 'comint)
  (pop-to-buffer
    (make-comint "Petite Chez Scheme"
                 "/bin/sh"
                 nil
                 "-c"
                 "/usr/bin/petite"))
  ;(inferior-scheme-mode)
  (make-local-variable 'comint-prompt-regexp)
  (setq comint-prompt-regexp "> ")
  (setq mode-name "Inferior Petite Chez Scheme"))

;;;;;;;;; CSS-mode

(autoload 'css-mode "css-mode" "CSS mode." t)
(setq auto-mode-alist (append '(("\\.css$" . css-mode)) auto-mode-alist))

;;;;;;;;; Diff-mode

(autoload 'diff-mode "diff-mode" "Diff major mode" t)
(add-to-list 'auto-mode-alist '("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode))

;;;;;;;;; Apache-mode

(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;;;;;;;;; XSL mode

(autoload 'xsl-mode "xslide" "Major mode for XSL stylesheets." t)

;; Turn on font lock when in XSL mode
(add-hook 'xsl-mode-hook
          'turn-on-font-lock)

(setq auto-mode-alist
      (append
       (list
        '("\\.fo" . xsl-mode)
        '("\\.xsl" . xsl-mode))
       auto-mode-alist))

;;;;;;;;; DTD mode

(autoload 'dtd-mode "tdtd" "Major mode for SGML and XML DTDs." t)
(autoload 'dtd-etags "tdtd"
  "Execute etags on FILESPEC and match on DTD-specific regular expressions."
  t)
(autoload 'dtd-grep "tdtd" "Grep for PATTERN in files matching FILESPEC." t)

;; Turn on font lock when in DTD mode
(add-hook 'dtd-mode-hooks
          'turn-on-font-lock)

(setq auto-mode-alist
      (append
       (list
        '("\\.dcl$" . dtd-mode)
        '("\\.dec$" . dtd-mode)
        '("\\.dtd$" . dtd-mode)
        '("\\.ele$" . dtd-mode)
        '("\\.ent$" . dtd-mode)
        '("\\.mod$" . dtd-mode))
       auto-mode-alist))

;;;;;;;;; Csharp-mode

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;;;;;;;;; Haskell-mode

(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.[hg]s$"  . haskell-mode)
                ("\\.hi$"     . haskell-mode)
                ("\\.l[hg]s$" . literate-haskell-mode))))
(autoload 'haskell-mode "haskell-mode"
   "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
   "Major mode for editing literate Haskell scripts." t)

;;;;;;;;; PHP-mode

(autoload 'php-mode "php-mode" "Mode for editing PHP source files")
(add-to-list 'auto-mode-alist '("\\.\\(inc\\|php[s34]?\\)" . php-mode))

;;;;;;;;; D-mode

(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

;;;;;;;;; Ediff hacks

;; (defun my-ediff-quit ()
;;   "My custom ediff quit function: closes ediff frame and hides
;;    all other buffers than the selected one."
;;   (interactive)
;;   ;; closes ediff external frame
;;   (delete-frame)
;;   ;; hides all other buffers than the selected one (the one at the
;;   ;; buttom of the screen normally)
;;   (delete-other-windows))

;; ;; since 'q' crashes X when in ediff mode this one overrides the alias
;; ;; to run my own custom function which simply closes the external
;; ;; ediff frame.
;; (defalias 'ediff-quit 'my-ediff-quit)

;;;;;;;;; dOvs hacks

(defun path-parent (path)
  (let ((new-path (mapconcat 'identity
                             (butlast (split-string path "/"))
                             "/")))
    (if (equal (substring path 0 1) "/")
        (concat "/" new-path)
        new-path)))

(defun dovs-find-make-path (path)
  (cond ((or (equal "/" path) (equal "" path)) nil)
        ((file-exists-p (concat path "/Makefile")) path)
        (t (dovs-find-make-path (path-parent path)))))


(defun dovs-compile (&optional target)
  "Compile function that searches recursively backwards for a
   Makefile to invoke. If no file is found invokes `compile' in
   the default manner."
  (interactive)
  (let ((make-path (dovs-find-make-path (path-parent (buffer-file-name)))))
    (if make-path
	(let ((my-buf (current-buffer))
	      (com-buf (compile (concat "cd " make-path " && make " target))))
	  (switch-to-buffer-other-window com-buf)
	  (end-of-buffer)
	  (switch-to-buffer-other-window my-buf))
        ;; default to ordinary compile
        (call-interactively 'compile))))

(let ((binding
       (lambda ()
	 (local-set-key [(control c) (control v)]
			(lambda () (interactive) (dovs-compile "joos2")))
	 (local-set-key [(control c) (control j)]
			(lambda () (interactive) (dovs-compile "onlyjoos2")))
;;	 (local-set-key [(control c) (control t)]
;;			(lambda () (interactive) (dovs-compile "test")))
;;	 (local-set-key [(control c) (control o)]
;;			(lambda () (interactive) (dovs-compile "onlytest")))
;;	 (local-set-key [(control c) (control u)]
;;			(lambda () (interactive) (dovs-compile "onlyjoos2test")))
	 (local-set-key [(control c) (control t)]
			(lambda () (interactive) (dovs-compile "test2")))
	 (local-set-key [(control c) (control o)]
			(lambda () (interactive) (dovs-compile "onlytest2")))
	 (local-set-key [(control c) (control u)]
			(lambda () (interactive) (dovs-compile "onlyjoos2test2")))
	 (local-set-key [(control c) (control p)]
			(lambda () (interactive) (dovs-compile "parser")))
	 (local-set-key [(control c) (control d)]
			(lambda () (interactive) (dovs-compile "doc"))))))
  (progn (add-hook 'java-mode-hook binding)))


;;;;;;;;; erlang-mode

(require 'erlang-start)

;;;;;;;;; OCaml mode (TuaReg)

(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
;(autoload 'tuareg-imenu-set-imenu "tuareg-imenu" 
;  "Configuration of imenu for tuareg" t) 
;(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist 
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

;;;;;;;;; MIX / MIXAL

(autoload 'mixal-mode "mixal-mode" t)
(add-to-list 'auto-mode-alist '("\\.mixal\\'" . mixal-mode))
(autoload 'mixvm "mixvm" "mixvm/gud interaction" t)

;;;;;;;;; CMake

(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;;;;;;;;; JavaScript

(add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))

;;;;;;;;; CEDET

(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-idle-completions-mode 0)
(global-semantic-idle-breadcrumbs-mode 0)
(global-semantic-stickyfunc-mode 1)
(global-semantic-highlight-func-mode 1)
(global-semantic-highlight-edits-mode 0)
(global-semantic-decoration-mode 0)
(global-semantic-show-parser-state-mode 1)

(semantic-add-system-include "/opt/local/include")

(defun custom-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-cc" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cr" 'semantic-complete-analyze-and-replace)
  (local-set-key "\C-ci" 'semantic-decoration-all-include-summary)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cs" 'semantic-symref)
  ;(local-set-key "." 'semantic-complete-self-insert)
  ;(local-set-key ">" 'semantic-complete-self-insert)
)
(add-hook 'c-mode-common-hook 'custom-cedet-hook)

(semantic-mode 1)

;;;;;;;;; Company ("complete-anything") 

(require 'company)

(add-to-list 'company-backends 'company-c-headers)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-<tab>") 'company-complete)

(company-mode 1)

;;;;;;;;; CEDET utility stuff

(defvar c-files-regex ".*\\.\\(c\\|cc\\|cpp\\|h\\|hpp\\)"
  "A regular expression to match any c/c++ related files under a directory")

(defvar qt-files-regex "Q.*"
  "A regular expression to match any c/c++ related files under a directory")
 
(defun my-semantic-parse-dir (root regex)
  "
   This function is an attempt of mine to force semantic to
   parse all source files under a root directory. Arguments:
   -- root: The full path to the root directory
   -- regex: A regular expression against which to match all files in the directory
  "
  (let (
        ;;make sure that root has a trailing slash and is a dir
        (root (file-name-as-directory root))
        (files (directory-files root t ))
       )
    ;; remove current dir and parent dir from list
    (setq files (delete (format "%s." root) files))
    (setq files (delete (format "%s.." root) files))
    ;; remove any known version control directories 
    (setq files (delete (format "%s.git" root) files))
    (setq files (delete (format "%s.hg" root) files))
    (while files
      (setq file (pop files))
      (if (not(file-accessible-directory-p file))
          ;;if it's a file that matches the regex we seek
          (progn (when (string-match-p regex file)
                   (save-excursion
                     (semanticdb-file-table-object file))
           ))
          ;;else if it's a directory
          (my-semantic-parse-dir file regex)
      )
     )
  )
)
 
(defun my-semantic-parse-current-dir (regex)
  "Parses all files under the current directory matching regex
  "
  (my-semantic-parse-dir (file-name-directory(buffer-file-name)) regex)
)
 
(defun lk-parse-curdir-c ()
  "Parses all the c/c++ related files under the current directory
   and inputs their data into semantic
  "
  (interactive)
  (my-semantic-parse-current-dir c-files-regex)
)
 
(defun lk-parse-dir-c (dir)
  "Prompts the user for a directory and parses all c/c++ related
   files under the directory
  "
  (interactive (list (read-directory-name "Provide the directory to search in:")))
  (my-semantic-parse-dir (expand-file-name dir) c-files-regex)
) 

(defun lk-parse-dir-qt (dir)
  "Prompts the user for a directory and parses all QT c/c++
   related files under the directory
  "
  (interactive (list (read-directory-name "Provide the directory to search in:")))
  (my-semantic-parse-dir (expand-file-name dir) qt-files-regex)
) 

;;;;;;;;; Recent files mode

(recentf-mode 1)
(global-set-key "\C-xr" 'recentf-open-files)
(add-to-list 'recentf-exclude "ido.last")

;;;;;;;;; Multiple cursors

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;;;;;;; Nyan mode

(require 'nyan-mode)
(nyan-mode)

;;;;;;;;; Magit mode

(require 'magit-svn)
(require 'magit)
(global-set-key "\C-xg" 'magit-status)
(add-hook 'magit-mode-hook 'magit-load-config-extensions)

;;;;;;;;; FIC mode (marks TODO, FIXME etc. clearly)

(require 'fic-mode)
(add-hook 'c-mode-common-hook 'fic-mode)
