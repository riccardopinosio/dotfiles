;;; simple-httpd-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "simple-httpd" "../../../../../../.emacs.d/elpa/simple-httpd-20191103.1446/simple-httpd.el"
;;;;;;  "4bab2bddb7d86f6cbc13ed2272f6b9c8")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/simple-httpd-20191103.1446/simple-httpd.el

(autoload 'httpd-start "simple-httpd" "\
Start the web server process. If the server is already
running, this will restart the server. There is only one server
instance per Emacs instance.

\(fn)" t nil)

(autoload 'httpd-stop "simple-httpd" "\
Stop the web server if it is currently running, otherwise do nothing.

\(fn)" t nil)

(autoload 'httpd-running-p "simple-httpd" "\
Return non-nil if the simple-httpd server is running.

\(fn)" nil nil)

(autoload 'httpd-serve-directory "simple-httpd" "\
Start the web server with given `directory' as `httpd-root'.

\(fn DIRECTORY)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "simple-httpd"
;;;;;;  "../../../../../../.emacs.d/elpa/simple-httpd-20191103.1446/simple-httpd.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/simple-httpd-20191103.1446/simple-httpd.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "simple-httpd" '("httpd" "defservlet" "with-httpd-buffer")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/elpa/simple-httpd-20191103.1446/simple-httpd-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/elpa/simple-httpd-20191103.1446/simple-httpd.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; simple-httpd-autoloads.el ends here
