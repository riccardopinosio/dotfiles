;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;

;; allow loading of lisp code from .doom.d
(add-to-list 'load-path "~/.doom.d")

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
           :target (file+head "articles/${slug}.org"
                              "#+title: ${title}\n#+filetags: articles\n#+AUTHOR: Riccardo Pinosio\n#+DATE: %<%Y-%m-%d>\n#+DESCRIPTION: description")
           :unnarrowed t)
          ))
  :config
  (org-roam-db-autosync-enable))

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

;; Use `citar' with `org-cite'
;;

(after! oc
  (setq org-cite-global-bibliography '("~/braindump/references.bib")))

(use-package! citar
  :after oc
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography '("~/braindump/references.bib"))
  (citar-org-roam-note-title-template "${author} - ${title}\npdf: ${file}")
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

;; LINK ZOTERO NOTES TO ORG-ROAM VIA CITAR

(load "org-roam-zotero-notes")


;; QUARTO AND ORG MODE

(setq blog-folder "~/repositories/misc/website/blog/")

(defun org-qmd-format-categories (categories)
  "format filetags for inclusion in qmd front-matter"
  (let ((category-list (split-string categories)))
    (format "categories: %s"
            (concat "[" (mapconcat 'identity category-list ", ") "]"))))

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

(defun org-qmd-get-path-from-link (link)
  (let ((match (string-match ".*(\\(.*\\))" link)))
    (match-string 1 link)))

(defun org-qmd-link-type (link-type-org path)
  (let ((ext (file-name-extension path)))
    (cond ((and (string= link-type-org "file") (member ext '("png" "jpeg" "jpg" "gif"))) "image" )
          (t "unknown"))))

(defun org-qmd-format-image-link (link parent)
  (let* ((attr-org (org-element-property :attr_org parent))
         (caption (org-element-property :caption parent))
         (caption (if caption
                      (nth 0 (nth 0 (nth 0 caption)))
                    ""))
         )
    (format "![%s](%s){%s}" caption
            (org-element-property :path link)
            (org-qmd-format-attr-org attr-org))))

(defun org-qmd-link (link desc info)
  (let* ((parent (org-element-property :parent link))
         (link-type-org (org-element-property :type link))
         (path (org-element-property :path link))
         (link-type (org-qmd-link-type link-type-org path)))
    (cond ((string= link-type "image") (org-qmd-format-image-link link parent))
          (t (org-md-link link desc info)))))

(defun org-qmd-template (contents info)
  "add front matter to the markdown file"
  (let* ((front-matter-properties (mapcar (lambda (el)
                                            (cons (substring el 1)
                                                  (org-export-data (plist-get info (intern el)) info)))
                                          '(":title"
                                            ":author"
                                            ":date"
                                            ":description"
                                            ":category")))
         (front-matter-lines (mapcar (lambda (el)
                                       (cond ((string= (car el) "category") (org-qmd-format-categories (cdr el)))
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
  (let* ((path (org-qmd-get-path-from-link data))
         (extension (file-name-extension path))
         (file-name (file-name-nondirectory path)))
    (cond ((member extension '("png" "jpeg" "gif"))
           (replace-regexp-in-string "(.*)" (format "(%s)" (concat "../assets/" file-name)) data))
          (t data))))

(defun org-qmd-publish-to-qmd (plist filename pub-dir)
  "Publish an org file to Markdown.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  (org-publish-org-to 'qmd filename ".qmd" plist pub-dir))

(defun org-qmd-publish-to-qmd-remap (plist filename pub-dir)
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
                       (link . org-qmd-link))
    :options-alist '((:category "CATEGORY" nil nil t)))

  (setq org-publish-project-alist
        '(("blog-posts"
           :base-directory "~/braindump/articles"
           :publishing-function org-qmd-publish-to-qmd-remap
           :publishing-directory "~/repositories/web/blog/posts"
           :section-numbers nil
           :with-toc nil)
          ("blog-images"
           :base-directory "~/braindump/images"
           :base-extension "png\\|jpg\\|gif"
           :publishing-directory "~/repositories/web/blog/assets"
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

(defun riccardo/export-org-roam-md ()
  "useful function to export an org roam to markdown
where the filename is the org note slug"
  (interactive)
  (let* ((filename (org-md-export-to-markdown))
         (slug (car (last (split-string filename "-")))))
    (rename-file filename (format "~/braindump/exports/%s" slug))))


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
