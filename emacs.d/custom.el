(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.cache/emacs-backups/" t))))
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.cache/emacs-backups"))))
 '(create-lockfiles nil)
 '(custom-file "/home/riccardo/.emacs.d/custom.el")
 '(default-input-method (quote russian-computer))
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(inhibit-splash-screen t)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote fundamental-mode))
 '(initial-scratch-message "")
 '(package-last-refresh-date "2020-05-03T12:15")
 '(package-selected-packages
   (quote
    (multi-term treemacs-magit separedit edit-indirect clang-format lsp-ui lsp-mode iedit expand-region mc-extras multiple-cursors magit yasnippet undo-tree company-posframe company ivy-posframe counsel ivy flx selected paredit geiser editorconfig vterm lua-mode fennel-mode cider clojure-mode yaml-mode cmake-mode racket-mode toml-mode rust-mode markdown-mode ox-hugo minions treemacs doom-modeline solaire-mode doom-themes all-the-icons use-package)))
 '(user-full-name "Andrey Orst")
 '(user-mail-address "andreyorst@gmail.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-deprecated-face ((t (:inherit smerge-upper))))
 '(cider-error-highlight-face ((t (:inherit flymake-error))))
 '(cider-fragile-button-face ((t (:box (:line-width -1 :color nil :style nil) :inherit (font-lock-warning-face)))))
 '(cider-fringe-face ((t (:inherit flymake-warning))))
 '(cider-instrumented-face ((t (:box (:line-width -1 :color "#ff6c6b" :style nil)))))
 '(parinfer--error-face ((t (:inherit (flymake-error))))))
