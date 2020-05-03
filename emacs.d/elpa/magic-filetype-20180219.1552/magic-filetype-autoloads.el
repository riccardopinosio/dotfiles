;;; magic-filetype-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "magic-filetype" "magic-filetype.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from magic-filetype.el

(autoload 'magic-filetype-major-mode-from-language-name "magic-filetype" "\
Invoke `major-mode' from `LANG-NAME'.

\(fn LANG-NAME)" t nil)

(autoload 'magic-filetype-vim-filetype-magic-mode "magic-filetype" "\
Invoke `major-mode' by Vim-style `FT' file header.

\(fn &optional FT)" t nil)

(autoload 'magic-filetype-enable-vim-filetype "magic-filetype" "\
Turn on magic-mode by Vim-style file header.

\(fn &optional FORCE)" t nil)

(autoload 'magic-filetype-major-mode-of "magic-filetype" "\
Get MAJOR-MODE from `LANG-NAME'.

\(fn LANG-NAME)" nil nil)

(autoload 'magic-filetype-set-auto-mode "magic-filetype" "\
Set `auto-mode-alist' by `LANG-NAME'.

\(fn LANG-NAME)" nil nil)

(autoload 'magic-filetype-reload-major-mode "magic-filetype" "\
Reload current major mode.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magic-filetype" '("magic-filetype-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magic-filetype-autoloads.el ends here
