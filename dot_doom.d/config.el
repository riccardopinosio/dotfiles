;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Riccardo Pinosio"
      user-mail-address "rpinosio@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/braindump/")

(after! org
  :custom
  (org-link-set-parameters "zotero" :follow
                           (lambda (zpath)
                             (browse-url
                              (format "zotero:%s" zpath)
                              )))
  )

(after! org-roam
  :ensure t
  :custom
  (setq org-roam-directory (file-truename "~/braindump"))
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))))
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n<t")
           :unnarrowed t)
          ("m" "meeting" plain "%?"
           :target (file+head "meetings/%<%Y%m%d%H%M%S>-${slug}.org"
                              ":PROPERTIES:\n:project: fill\n:people: fill\n:END:\n#+title: ${title} %<%Y-%m-%d>\n#+filetags:")
           :unnarrowed t)
          ("t" "main" plain "%?"
           :target (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags:")
           :unnarrowed t)
          ))
  :config
  (org-roam-db-autosync-enable)
  )

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; CITATIONS

(after! oc
(setq org-cite-global-bibliography '("~/braindump/references.bib")))

;;(defun riccardo/format-citar-file (file)
;;  (let* ((parts (split-string file ";"))
;;        (ret (cl-loop for str in parts
;;                       ;;if (string-suffix-p ".pdf" str)
;;                       collect str)))
;;    (setq paths "")
;;    (dolist (path ret) (setq paths (concat paths " " "[[" path "]]")))
;;    paths))

;; Use `citar' with `org-cite'
;;

(use-package! citar
  :after oc
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography '("~/braindump/references.bib"))
  (citar-org-roam-note-title-template "${author} - ${title}\npdf: ${file}")
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
  )

(defadvice! riccardo/citar-file-trust-zotero (oldfun &rest r)
  "Leave Zotero-generated file paths alone, especially zotero://..."
  :around '(citar-file-open citar-file--find-files-in-dirs)
  (cl-letf (((symbol-function 'file-exists-p) #'always)
            ((symbol-function 'expand-file-name) (lambda (first &rest _) first)))
    (apply oldfun r)))

(after! citar
  (add-to-list 'citar-file-open-functions '("pdf" . citar-file-open-external)))

;; link zotero annotations

(defun riccardo/grab-reference-items (citekey item-list)
  (with-temp-buffer
    (insert-file-contents (nth 0 citar-bibliography))
    (let* ((citekey-start (search-forward citekey nil nil))
           (citekey-start (if citekey-start (line-beginning-position)
                            nil))
           (citekey-end (if citekey-start (search-forward "@" nil nil)
                          nil)))
      (mapcar (lambda (item-name)
                (let* (
                       (item-start (if citekey-start (progn (goto-char citekey-start)
                                                            (search-forward (format "%s = {" item-name)
                                                                            citekey-end t))
                                     nil
                                     ))
                       (item-end (if item-start (progn (up-list) (point)) nil)))
                  (if item-start
                      (buffer-substring-no-properties item-start item-end)
                    "")))
              item-list)
    )))

(defun riccardo/parse-annotation-par (el)
  (let* ((match (string-match ".*(\\(.*\\),\\(.*\\),.*p.\s\\([0-9]+\\))\\(.*\\)" el))
        (components (when match
                      (list :author (match-string 1 el)
                            :year (match-string 2 el)
                            :page (match-string 3 el)
                            :content (match-string 4 el)
                            :quote (let ((match (string-match ".*``\\(.*\\)''" el)))
                                     (when match (match-string 1 el)))
                            )
                      )))
    components
    )
  )

(defun riccardo/parse-annotation-components (annotation-section url)
  (let* ((match (string-match ".*(\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\),\s\\(.+\\))" annotation-section))
         (annotation-attributes (list :year (match-string 3 annotation-section)
                                :month (match-string 2 annotation-section)
                                :day (match-string 1 annotation-section)
                                :timestamp (match-string 4 annotation-section)))
         (url (replace-regexp-in-string "?.*" "" url))
         )
    (setq annotation-pars (remq nil (mapcar #'riccardo/parse-annotation-par
                                            (split-string annotation-section "\par[\s\n]+"))))
    (setq annotation-pars (mapconcat (lambda (el)
                             (let* ((quote (plist-get el :quote))
                                    (page (plist-get el :page))
                                    (content (plist-get el :content))
                                    (url (concat url (format "?p%s" page))))
                               (concat (format "*** [[%s][note p%s]]\n"
                                               url
                                               page)
                                       (when quote
                                         (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n" quote))
                                       (when content content))))
                           annotation-pars "\n"))
    ;; format note for display
    (format "** (%s-%s-%s), %s:\n%s"
            (plist-get annotation-attributes :year)
            (plist-get annotation-attributes :month)
            (plist-get annotation-attributes :day)
            (plist-get annotation-attributes :timestamp)
            annotation-pars
            )
    ))


(defun riccardo/parse-zotero-notes (notes url)
  "tst"
  (let* ((annotation-sections (split-string notes "\section"))
        (annotation-sections (seq-filter (lambda (el) (string-match-p (regexp-quote "Annotation")
                                                          el)) annotation-sections))
        (notes-parsed (mapcar (lambda (el) (riccardo/parse-annotation-components el url)) annotation-sections)))
    (mapconcat 'identity notes-parsed "\n")
    )
  )

(defun riccardo/create-zotero-notes (notes file-url)
  (let* ((notes (riccardo/parse-zotero-notes notes file-url))
         (data (org-element-parse-buffer 'greater-elements))
         (begin-zotero-headline (org-element-map data 'headline
                          (lambda (el)
                            (and (string= "zotero notes" (org-element-property :raw-value el))
                            (org-element-property :begin el))
                            )
                          nil t))
         (end-zotero-headline (org-element-map data 'headline
                          (lambda (el)
                            (and (string= "zotero notes" (org-element-property :raw-value el))
                            (org-element-property :end el))
                            )
                          nil t)))
  (if begin-zotero-headline (delete-region begin-zotero-headline end-zotero-headline))
  (save-excursion
    (goto-char (point-max))
    (insert "* zotero notes\n" notes))
  )
  )

(defun riccardo/update-references ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((start-reference (search-forward ":ROAM_REFS: @" nil t))
           (end-reference (progn
                            (forward-word)
                            (point))))
      (if (and start-reference end-reference)
          (let* ((citekey (buffer-substring-no-properties start-reference end-reference))
                 (zotero-notes (riccardo/grab-reference-items citekey '(note file)))
                 )
            (riccardo/create-zotero-notes (nth 0 zotero-notes) (nth 1 zotero-notes))
                 )))))

(add-hook 'find-file-hook #'riccardo/update-references)
(add-hook 'after-save-hook #'riccardo/update-references)

;; deal with zotero notes

;; OTHER PERSONAL FUNCTIONS
;;


(defun riccardo/paste-excursion (tag)
  (interactive)
  (save-excursion
    (insert "* TODO  :" tag ":")
    )
  (forward-char 7)
  (evil-insert-state)
  )


(defhydra riccardo/todo (nil nil :foreign-keys nil :hint nil :exit t)
  "
Select todo type:
----------------------
_a_ life admin
_l_ learning
_r_ research
_w_ work
_s_ systems

_q_ quit"
  ("q" nil)
  ("a" (riccardo/paste-excursion "admin"))
  ("l" (riccardo/paste-excursion "learning"))
  ("r" (riccardo/paste-excursion "research"))
  ("w" (riccardo/paste-excursion "work"))
  ("s" (riccardo/paste-excursion "systems"))
  )

(defhydra riccardo/daily (nil nil :foreign-keys nil :hint nil :exit t)
  "
Select todo type:
----------------------
_t_ go to today

_q_ quit"
  ("q" nil)
  ("t" org-roam-dailies-goto-today)
  )

(map! :leader :desc "create todo" "t" #'riccardo/todo/body)
(map! :leader :desc "dailies" "d" #'riccardo/daily/body)

(defun riccardo/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
