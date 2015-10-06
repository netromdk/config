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

;; set auto-fill-mode
(add-hook 'text-mode-hook
          (lambda () (auto-fill-mode t)))

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

(autoload 'php-mode "php-mode" "Mode for editing PHP source files")
(add-to-list 'auto-mode-alist '("\\.\\(inc\\|php[s34]?\\)" . php-mode))

;;;;;;;;; CMake

(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;;;;;;;;; JavaScript

(add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))

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

;; Turn on general lines in left margin (built-in package).
(global-linum-mode t)

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

(defalias 'lp 'helm-list-elisp-packages)

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

(global-anzu-mode t)

;;;;;;;;; smart-mode-line

(setq sml/numbers-separator " ")

(sml/setup)

(add-to-list 'sml/replacer-regexp-list '("/Volumes/Luxion/" ":LUX:") t)
(add-to-list 'sml/prefix-face-list '(":LUX:" sml/git))

(add-to-list 'sml/replacer-regexp-list '("/Volumes/Burator/" ":B:") t)
(add-to-list 'sml/prefix-face-list '(":B:" sml/git))

(add-to-list 'sml/replacer-regexp-list '(".*[Ss]vn" ":SVN:") t)
(add-to-list 'sml/prefix-face-list '(":SVN:" sml/git))

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

;;;;;;;;; whitespace

;; Visualize certain like space at end of line and trailing characters after
;; fill column.
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;;;;;;;; dash-at-point

(global-set-key "\C-cd" 'dash-at-point)

(add-to-list 'dash-at-point-mode-alist '(c-mode . "c,manpages"))
(add-to-list 'dash-at-point-mode-alist '(c++-mode . "cpp,qt,c,manpages,lux"))
(add-to-list 'dash-at-point-mode-alist '(python-mode . "py,flask"))
(add-to-list 'dash-at-point-mode-alist '(cmake-mode . "cmake"))
(add-to-list 'dash-at-point-mode-alist '(js-mode . "js"))

;;;;;;;; helm-ag

(setq helm-ag-base-command "ag --nocolor --nogroup --smart-case --stats")

;;;;;;;; mu4e

(require 'mu4e)

;; Set as default MUA.
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-maildir (expand-file-name "~/Maildir"))

(setq mu4e-drafts-folder  "/[Gmail].Drafts")
(setq mu4e-sent-folder    "/[Gmail].Sent Mail")
(setq mu4e-trash-folder   "/[Gmail].Bin")
(setq mu4e-refile-folder  "/[Gmail].All Mail")
(setq mu4e-attachment-dir "~/Downloads")

;; Don't save message to Sent Messages, GMail/IMAP will take care of this.
(setq mu4e-sent-messages-behavior 'delete)

;; Allow for updating mail using 'U' in the main view.
(setq mu4e-get-mail-command "offlineimap")

(setq mu4e-user-mail-address-list
      '("msk@nullpointer.dk"
        "ontherenth@gmail.com"
        "morten@luxion.com"))

;; First time it will ask for user and pass, then save it to .authinfo
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; Add confirmation before sending emails.
(add-hook 'message-send-hook
          (lambda ()
            (unless (yes-or-no-p "Sure you want to send this?")
              (signal 'quit nil))))

;; Don't keep message buffers around after sending a message.
(setq message-kill-buffer-on-exit t)

;; Skip duplicates when the Message-Id is the same (typically happens with gmail
;; with labels).
(setq mu4e-headers-skip-duplicates t)

;; Number of headers to show while viewing an email.
(setq mu4e-headers-visible-lines 20)

;; Use imagemagick, if available.
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; Define actions to view in browser with "ai".
(add-to-list 'mu4e-headers-actions
             '("in browser" . mu4e-action-view-in-browser) t)
(add-to-list 'mu4e-view-actions
             '("in browser" . mu4e-action-view-in-browser) t)

;; Convert org mode to HTML automatically.
(setq org-mu4e-convert-to-html t)

;; Need this to render HTML e-mails properly.
(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)

;; Some link navigation with tab and backtab.
(add-hook 'mu4e-view-mode-hook
          (lambda()
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))

;; Show addresses in addition to names.
(setq mu4e-view-show-addresses t)

;; Show images inline.
(setq mu4e-view-show-images t)

;; Show unicode characters.
(setq mu4e-use-fancy-chars t)

;; Set appropriate time format.
(setq mu4e-headers-time-format "%H:%M:%S")

;; Set appropriate date format.
(setq mu4e-headers-date-format "%a %d/%m %Y")

;; Set the headers and their column sizes.
(setq mu4e-headers-fields
      '((:human-date . 15) ;; Shows time for today and date for otherwise.
        (:flags . 6)
        (:size . 6)
        (:from-or-to . 22)
        (:subject . nil)))

;; Set the header fields to show when viewing emails.
(setq mu4e-view-fields
      '(:from :to :cc :bcc :subject :flags :date :maildir :mailing-list :tags :attachments :signature :decryption :size))

;; Set bookmarks on the front.
(setq mu4e-bookmarks
      '(("flag:unread AND NOT flag:trashed" "Unread messages" 117)
        ("date:today..now" "Today's messages" 116)
        ("flag:replied AND date:today..now" "Replied today" 114)
        ("date:7d..now" "Last 7 days" 119)
        ("flag:attach" "Messages with attachments" 97)
        ("mime:image/*" "Messages with images" 112)
        ("size:5M..500M" "Big messages" 98)
        ("html" "HTML messages" 104)
        ("text" "Text messages" 120)))

(defun mu4e-in-new-frame ()
  "Start mu4e in new frame."
  (interactive)
  (select-frame (make-frame))
  (mu4e))

;; Define each account.
(defun msk-mu4e-msk()
  (interactive)
  (da-spell)
  (message "Personal account: msk@nullpointer.dk")
  (setq user-mail-address "msk@nullpointer.dk"
        user-full-name "Morten Kristensen"
        mu4e-compose-signature
        '(concat "Mvh. /Best regards\n"
                 "Morten Kristensen\n")
        mu4e-maildir-shortcuts
        '(("/INBOX" . ?i)
          ("/[Gmail].All Mail" . ?A)
          ("/[Gmail].Sent Mail" . ?S)
          ("/me/@msk" . ?m)
          ("/me.@ontherenth" . ?g)
          ("/me.e-boks" . ?e)
          ("/me.receipts" . ?r)
          ("/bujinkan.dojo" . ?d)
          ("/bujinkan.e-boks" . ?b)
          ("/bujinkan.kenkon" . ?k))))

(defun msk-mu4e-ontherenth()
  (interactive)
  (da-spell)
  (message "Personal account: ontherenth@gmail.com")
  (setq user-mail-address "ontherenth@gmail.com"
        user-full-name "Morten Kristensen"
        mu4e-compose-signature
        '(concat "Mvh. /Best regards\n"
                 "Morten Kristensen\n")
        mu4e-maildir-shortcuts
        '(("/INBOX" . ?i)
          ("/[Gmail].All Mail" . ?A)
          ("/[Gmail].Sent Mail" . ?S)
          ("/me.@msk" . ?m)
          ("/me.@ontherenth" . ?g)
          ("/me.e-boks" . ?e)
          ("/me.receipts" . ?r)
          ("/bujinkan.dojo" . ?d)
          ("/bujinkan.e-boks" . ?b)
          ("/bujinkan.kenkon" . ?k))))

(defun msk-mu4e-luxion()
  (interactive)
  (en-spell)
  (message "Work account: morten@luxion.com")
  (setq user-mail-address "morten@luxion.com"
        user-full-name "Morten Kristensen"
        mu4e-compose-signature
        '(concat "Mvh. /Best regards\n"
                 "Morten Kristensen\n\n"
                 "Software Engineer\n"
                 "Luxion ApS\n")
        mu4e-maildir-shortcuts
        '(("/INBOX" . ?i)
          ("/[Gmail].All Mail" . ?A)
          ("/[Gmail].Sent Mail" . ?S)
          ("/lux" . ?l)
          ("/lux.commits" . ?c)
          ("/lux.redmine" . ?r)
          ("/lux.velux" . ?v)
          ("/lux.@dev" . ?d)
          ("/lux.@aarhus" . ?å)
          ("/lux.@all" . ?a)
          ("/lux.@cloud" . ?u))))

;; Switch between active accounts.
(define-key mu4e-main-mode-map (kbd "1") 'msk-mu4e-msk)
(define-key mu4e-main-mode-map (kbd "2") 'msk-mu4e-ontherenth)
(define-key mu4e-main-mode-map (kbd "3") 'msk-mu4e-luxion)
(define-key mu4e-headers-mode-map (kbd "1") 'msk-mu4e-msk)
(define-key mu4e-headers-mode-map (kbd "2") 'msk-mu4e-ontherenth)
(define-key mu4e-headers-mode-map (kbd "3") 'msk-mu4e-luxion)

;; Check if addresses are used in to, cc or bcc fields.
(defun msk-mu4e-is-message-to (msg rx)
  "Check if to, cc or bcc field in MSG has any address in RX."
  (or (mu4e-message-contact-field-matches msg :to rx)
      (mu4e-message-contact-field-matches msg :cc rx)
      (mu4e-message-contact-field-matches msg :bcc rx)))

;; Set replying identity from to, cc or bcc fields.
(defun msk-mu4e-set-from-address ()
  "Set current identity based on to, cc, bcc of original."
  (let ((msg mu4e-compose-parent-message))
    (if msg
        (cond
         ((msk-mu4e-is-message-to msg (list "msk@nullpointer.dk"))
          (msk-mu4e-msk))
         ((msk-mu4e-is-message-to msg (list "ontherenth@gmail.com"))
          (msk-mu4e-ontherenth))
         ((msk-mu4e-is-message-to msg (list "morten@luxion.com"))
          (msk-mu4e-luxion))))))

(add-hook 'mu4e-compose-pre-hook 'msk-mu4e-set-from-address)

;; Enable to compose an email with attachments from dired mode. Go into dired
;; somewhere, mark files to attach using 'm' and then do C-c RET C-a to compose
;; new email with those files as attachments.
(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; Choose default account.
(msk-mu4e-msk)

;;;;;;;; mu4e-maildir-shortcuts

(require 'mu4e-maildirs-extension)
(mu4e-maildirs-extension)


(provide 'setup-modules)
