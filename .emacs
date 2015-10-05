;; ------------------------
;;   Emacs Configurations
;; ------------------------

;; Add the ~/.emacs.d dir to the loadpath
(add-to-list 'load-path (expand-file-name "~/.emacs.d") t)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/custom") t)

(require 'setup-general)
(require 'setup-packages)

;;;;;;;;; Additional customizations

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212121" "#CC5542" "#ff9800" "#b1d631" "#5180b3" "#DC8CC3" "#9b55c3" "#bdbdb3"])
 '(custom-enabled-themes (quote (mustang-netrom)))
 '(custom-safe-themes
   (quote
    ("a479d98dbdfaa2880d8c81014f926acd14c286f5b0f4a657507e63f0577a4844" default)))
 '(fill-column 80)
 '(recentf-exclude (quote ("ido.last" ".emacs.d/saveplace.txt")))
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(vc-annotate-background "#3b3b3b")
 '(vc-annotate-color-map
   (quote
    ((20 . "#dd5542")
     (40 . "#CC5542")
     (60 . "#fb8512")
     (80 . "#baba36")
     (100 . "#bdbc61")
     (120 . "#7d7c61")
     (140 . "#6abd50")
     (160 . "#6aaf50")
     (180 . "#6aa350")
     (200 . "#6a9550")
     (220 . "#6a8550")
     (240 . "#6a7550")
     (260 . "#9b55c3")
     (280 . "#6CA0A3")
     (300 . "#528fd1")
     (320 . "#5180b3")
     (340 . "#6380b3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'setup-funcs)
(require 'setup-luxion)
(require 'setup-modules)
