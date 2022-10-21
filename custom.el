(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(cider-repl-display-help-banner nil t)
 '(cider-repl-tab-command nil t)
 '(confirm-kill-processes nil)
 '(create-lockfiles nil)
 '(custom-file "~/.emacs.d/custom.el")
 '(default-input-method 'russian-computer)
 '(geiser-active-implementations '(guile))
 '(geiser-default-implementation 'guile)
 '(inhibit-startup-screen t nil nil "Customized with use-package startup")
 '(initial-major-mode 'fundamental-mode nil nil "Customized with use-package startup")
 '(initial-scratch-message "" nil nil "Customized with use-package startup")
 '(lsp-completion-provider t)
 '(lsp-enable-symbol-highlighting nil)
 '(lsp-keymap-prefix "C-c l")
 '(lsp-rust-clippy-preference "on")
 '(lsp-rust-server 'rust-analyzer)
 '(lsp-ui-doc-border "#1d2026")
 '(lsp-ui-doc-delay 0.7)
 '(lsp-ui-imenu-enable nil)
 '(lsp-ui-sideline-enable nil)
 '(nrepl-hide-special-buffers t t)
 '(package-last-refresh-date "2022-10-19T16:46")
 '(package-selected-packages
   '(sly jupyter poly-R poly-markdown slime ein pyvenv ess lsp-python-ms which-key dap-PYTHON dap-python dap-mode lsp-treemacs lsp-ivy evil multi-term treemacs-magit separedit edit-indirect clang-format lsp-ui lsp-mode iedit expand-region mc-extras multiple-cursors magit yasnippet undo-tree company-posframe company ivy-posframe counsel ivy flx selected paredit geiser editorconfig vterm lua-mode fennel-mode cider clojure-mode yaml-mode cmake-mode racket-mode toml-mode rust-mode markdown-mode ox-hugo minions treemacs doom-modeline solaire-mode doom-themes all-the-icons use-package))
 '(parinfer-extensions '(defaults pretty-parens smart-tab smart-yank) t)
 '(solaire-mode-real-buffer-fn 'aorst/real-buffer-p)
 '(user-full-name "Riccardo Pinosio")
 '(user-mail-address "rpinosio@gmail.com"))
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
