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
          ("a" "article" plain "%?"
           :target (file+head "articles/${title}-${slug}.org"
                              "#+title: ${title}\n#+filetags: articles")
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

;; CITATIONS AND ZOTERO NOTES

(after! oc
(setq org-cite-global-bibliography '("~/braindump/references.bib")))

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

(defun riccardo/parse-annotation-par (el)
  "given a single \par annotation, extract its attributes"
  (let* ((match (string-match ".*(\\(.*\\),\\(.*\\),.*p.\s\\([0-9]+\\))\\(.*\\)" el))
         (components (when match
                       (list :author (match-string 1 el)
                             :year (match-string 2 el)
                             :page (match-string 3 el)
                             :content (match-string 4 el)
                             :quote (let ((match (string-match ".*``\\(.*\\)''" el)))
                                      (when match (match-string 1 el)))))))
    components))

(defun riccardo/parse-annotation-components (annotation-section url)
  "given an annotation with multiple notes separated by \par elements, extract the time information of the
   annotation and, for each \par note, its attributes, among which its content"
  (let* ((match (string-match ".*(\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\),\s\\(.+\\))" annotation-section))
         (annotation-attributes (list :year (match-string 3 annotation-section)
                                      :month (match-string 2 annotation-section)
                                      :day (match-string 1 annotation-section)
                                      :timestamp (match-string 4 annotation-section)))
         (url (replace-regexp-in-string "?.*" "" url)))

    (setq annotation-pars (remq nil (mapcar #'riccardo/parse-annotation-par (split-string annotation-section "\par[\s\n]+"))))
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
            annotation-pars)))


(defun riccardo/parse-zotero-notes (notes url)
  "Given a string with the zotero notes, split it into sections (one per annotation) and parse it"
  (let* ((annotation-sections (split-string notes "\section"))
         (annotation-sections (seq-filter (lambda (el) (string-match-p (regexp-quote "Annotation")
                                                                       el)) annotation-sections))
         (notes-parsed (mapcar (lambda (el) (riccardo/parse-annotation-components el url)) annotation-sections)))
    (mapconcat 'identity notes-parsed "\n")))

(defun riccardo/create-zotero-notes (notes file-url)
  "create a string with the zotero notes and insert it"
  (let* ((notes (riccardo/parse-zotero-notes notes file-url)) ;; parse the notes
         (data (org-element-parse-buffer 'greater-elements))
         (zotero-headline-bounds (org-element-map data 'headline
                          (lambda (el)
                            (and (string= "zotero notes" (org-element-property :raw-value el))
                            (list (org-element-property :begin el) (org-element-property :end el))))
                          nil t)))
  (if (nth 0 zotero-headline-bounds) (delete-region (nth 0 zotero-headline-bounds) (nth 1 zotero-headline-bounds)))
  (save-excursion
    (goto-char (point-max))
    (insert "* zotero notes\n" notes)
    )))

(defun riccardo/grab-reference-items (citekey item-list)
  "Retrieve the items in item-list from the citekey"
  (with-temp-buffer
    (mapc (lambda (reference-file) (insert-file-contents reference-file))  citar-bibliography)
    (let* ((citekey-start (search-forward citekey nil nil))
           (citekey-start (if citekey-start (line-beginning-position) nil))
           (citekey-end (if citekey-start (search-forward "@" nil nil) nil)))
      (mapcar (lambda (item-name)
                (let* ((item-start (if citekey-start (progn (goto-char citekey-start)
                                                            (search-forward (format "%s = {" item-name)
                                                                            citekey-end t))
                                     nil))
                       (item-end (if item-start (progn (up-list) (point)) nil)))
                  (if item-start
                      (buffer-substring-no-properties item-start item-end)
                    "")))
              item-list))))

(defun riccardo/update-zotero-notes ()
  "Update the ORG headline with the zotero notes for this reference"
  (interactive)
  (when (get-buffer-window)
    (save-excursion
      (goto-char (point-min))
      (let* ((start-reference (search-forward ":ROAM_REFS: @" nil t))
             (end-reference (progn
                              (forward-word)
                              (point))))
        (if (and start-reference end-reference)
            (let* ((citekey (buffer-substring-no-properties start-reference end-reference))
                   (zotero-notes (riccardo/grab-reference-items citekey '(note file))))
              (riccardo/create-zotero-notes (nth 0 zotero-notes) (nth 1 zotero-notes))))))))

(add-hook 'find-file-hook #'riccardo/update-zotero-notes)
(add-hook 'after-save-hook #'riccardo/update-zotero-notes)


;; QUARTO AND ORG MODE

(setq blog-folder "~/repositories/misc/website/blog/")

(defun org-qmd-format-tags (tags)
  "format filetags for inclusion in qmd front-matter"
  (let ((tags-list (split-string tags " ")))
    (format "tags: %s"
            (concat "[" (mapconcat 'identity tags-list ", ") "]"))))

(defun org-qmd-get-lang-name (lang)
  (cond ((string= lang "emacs-lisp") "commonlisp")
        (t lang)))

(defun org-qmd-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into Github Flavored Markdown
format. CONTENTS is nil.  INFO is a plist used as a communication
channel. Lifted from org-gfm."
  (let* ((lang (org-qmd-get-lang-name (org-element-property :language src-block)))
         (code (org-export-format-code-default src-block info))
         (prefix (concat "```" lang "\n"))
         (suffix "```"))
    (concat prefix code suffix)))


(defun org-qmd-format-attr-org (attr-org)
  (mapconcat (lambda (el)
               (let* ((parts (split-string el " "))
                      (key (substring (nth 0 parts) 1))
                      (val (nth 1 parts)))
                 (cond ((string= key "width") (format "%s=%s" key val))
                       (t "")))) attr-org " "))

(defun org-qmd-link (link desc info)
  (let* ((parent (org-element-property :parent link))
         (attr-org (org-element-property :attr_org parent))
         (trans-link (org-md-link link desc info)))
    (if attr-org
        (concat trans-link "{" (org-qmd-format-attr-org attr-org) "}") ;; parse and format the attr_org
      trans-link)))

(defun org-qmd-template (contents info)
  "add front matter to the markdown file"
  (let* ((front-matter-properties (mapcar (lambda (el)
                                            (cons (substring el 1)
                                                  (org-export-data (plist-get info (intern el)) info)))
                                          '(":title"
                                            ":author"
                                            ":date"
                                            ":description"
                                            ":filetags")))
         (front-matter-lines (mapcar (lambda (el)
                                       (cond ((string= (car el) "filetags") (org-qmd-format-tags (cdr el)))
                                             (t (format "%s: %s" (car el) (cdr el)))))
                                     front-matter-properties)))
    (concat "---\n" (mapconcat 'identity front-matter-lines "\n") "\n---\n" contents)))

(defun qmd-export ()
  "export org to qmd in a buffer"
  (interactive)
  (org-export-to-buffer 'qmd "*qmd*"))

(defun org-qmd-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Github Flavored Markdown file.
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".qmd" subtreep)))
    (org-export-to-file 'qmd outfile async subtreep visible-only)))

(defun org-qmd-remap-urls (data backend info)
"Remap the url for the image links"
(if (string-match-p (regexp-quote "[img]") data)
    (let* ((url (string-match ".*(\\(.*\\))" data))
           (url-match (match-string 1 data))
           (file-name (file-name-nondirectory url-match))
           (remapped-file-name (concat "../images/" file-name)))
      (replace-regexp-in-string "(.*)" (format "(%s)" remapped-file-name) data))
    data))

(defun org-qmd-publish-to-qmd (plist filename pub-dir)
  "Publish an org file to Markdown.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  (org-publish-org-to 'qmd filename ".qmd" plist pub-dir))

(defun org-qmd-publish-to-qmp-remap (plist filename pub-dir)
  "Publish to markdown but remap urls"
  (let ((org-export-filter-link-functions (list 'org-qmd-remap-urls)))
    (org-qmd-publish-to-qmd plist filename pub-dir)))

;; setup org mode

(require 'ox)

(setq org-directory "~/braindump/")

(after! org
  :custom
  (org-link-set-parameters "zotero" :follow
                           (lambda (zpath)
                             (browse-url
                              (format "zotero:%s" zpath))))
  (org-export-define-derived-backend 'qmd 'md
  :filters-alist '((:filter-parse-tree . org-md-separate-elements))
  :translate-alist '((inner-template . org-qmd-template)
                     (src-block . org-qmd-src-block)
                     (link . org-qmd-link)
                     ))
  (setq org-publish-project-alist
      '(("blog-posts"
         :base-directory "~/braindump/articles"
         :publishing-function org-qmd-publish-to-qmd
         :publishing-directory "~/repositories/misc/website/blog/posts"
         :section-numbers nil
         :with-toc nil)
        ("blog-images"
         :base-directory "~/braindump/images"
         :base-extension "png\\|jpg"
         :publishing-directory "~/repositories/misc/website/blog/images"
         :publishing-function org-publish-attachment)
        ("blog" :components("blog-posts" "blog-images")))))

;; org agenda
(after! org
  :custom
  (setq org-agenda-files (directory-files-recursively "~/braindump/" "\\.org$")))
(after! org-ql
  :custom
  (setq org-ql-views '(("Todos" :buffers-files org-agenda-files
                        :query (todo)
                        :sort (date)
                        :super-groups org-super-agenda-groups
                        :title "Todos")))
  (setq org-super-agenda-groups '((:auto-tags t))))


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
Select action:
----------------------
_o_ todo overview
_a_ insert todo life admin
_l_ insert todo learning
_r_ insert todo research
_w_ insert todo work
_s_ insert todo systems

_q_ quit"
  ("q" nil)
  ("o" (org-ql-view "Todos"))
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

(require 'quarto-mode)

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
