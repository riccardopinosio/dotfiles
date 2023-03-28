;;; orgconfig.el -*- lexical-binding: t; -*-

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
