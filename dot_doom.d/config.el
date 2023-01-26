;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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
  (setq org-startup-folded t)
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
                               ":PROPERTIES:\n:project: fill\n:people: fill\n:END:\n#+title: ${title}\n#+filetags:")
            :unnarrowed t)
            ("t" "main" plain "%?"
            :target (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org"
                               "#+title: ${title}\n#+filetags:")
            :unnarrowed t)
                ("r" "reference" plain "%?"
                :if-new
                (file+head "reference/${title}.org" "#+title: ${title}\n")
                :immediate-finish t
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
(setq org-cite-global-bibliography '("~/braindump/references.bib"))
  )


;; Use `citar' with `org-cite'
;;
(use-package! citar
  :after oc
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography '("~/braindump/references.bib"))
;;  (citar-org-roam-note-title-template "${author}")
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
  )


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
