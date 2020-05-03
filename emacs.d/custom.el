;; Put your personal user configuration in this file.

;; To require addional packages add them to 'package-selected-packages, e.g.
;; (add-to-list 'package-selected-packages 'ess)
;; will ensure that the ess package is installed the next time Emacs starts.



;; Don't remove this:
(unless (every 'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (package-install-selected-packages))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
