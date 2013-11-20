(setq load-path (cons "/users/courses/dProgSprog/emacs" load-path))

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
                 "/usr/local/bin/petite"))
  ;(inferior-scheme-mode)
  (make-local-variable 'comint-prompt-regexp)
  (setq comint-prompt-regexp "> ")
  (setq mode-name "Inferior Petite Chez Scheme"))



