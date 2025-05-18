;;; mainspring-notes.el --- Interface for notes. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jonathan Hope

;; Author: Jonathan Hope <jhope@theflatfield.net>
;; Version: 1.0
;; Keywords: notes denote
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Interface for notes.
;; Built on denote and consult.

;;; Code:

(eval-when-compile (require 'json))
(unless (require 'denote nil 'noerror)
  (message "denote is required"))
(eval-when-compile (require 'denote))
(unless (require 'consult nil 'noerror)
  (message "denote is required"))
(eval-when-compile (require 'consult))

(defgroup mainspring-notes nil
  "Interface for notes"
  :group 'extensions)

(defface mainspring-notes-id-face
  '()
  "Face used for ID part of candidate."
  :group 'mainspring-notes-id)

(defface mainspring-notes-title-face
  '()
  "Face used for title part of candidate."
  :group 'mainspring-notes-title)

(defun mainspring-query-notes (query max)
  "Uses `rg' to search the notes in `denote-directory.';
Only org/markdown/text files with a denote identifier in their name are searched.
The search is  performed case insensitive on the title in the front matter.
At most `max' number of results are returned.
Requires `rg' to be in the PATH."
  (split-string
   (shell-command-to-string
    (format "rg -l -i '^\\#\\+title:.*%s.*' %s -g '*.{org,md,txt}' | rg '[0-9]{8}T[0-9]{6}' | head -n %d" query (denote-directory) max))))

(defun mainspring-get-notes-meta (notes)
  "Uses `rg' to fetch the metadata for the notes in paths `notes'.
The metadata returned  is title, identifier, and filests, all of whih must be in the front matter.
The metadata is returned as a hash table.
Requires `rg' and `jq' to be in the PATH.
Requires that Emacs be compiled with JSON support."
  (if (not notes) 
    '() 
    (let ((notes-with-meta ()))
      (maphash
       (lambda (k v) (push (list k v) notes-with-meta))
       (json-parse-string
        (shell-command-to-string
         (concat 
          (format "rg --json '(^\\#\\+title:|^\\#\\+identifier:|^\\#\\+filetags:)' %s"  (string-join notes " "))
          " | "
          "rg '\"type\":\"match\"'"
          " | "
          "jq -s 'reduce .[] as $item ({}; ($item.data.path.text) as $path "
          "| ($item.data.submatches[0].match.text | sub(\":$\"; \"\") | sub(\"^#\\\\+\"; \"\")) as $key "
          "| ($item.data.lines.text | sub(\"(^#\\\\+title:\\\\s+|^#\\\\+identifier:\\\\s+|^#\\\\+filetags:\\\\s+)\"; \"\") | sub(\"\\\\s$\"; \"\")) as $value "
          "| .[$path] += {($key): $value})'"))))
      notes-with-meta)))

(defun mainspring-new-note (cand)
  "Create a note with `denote'."
  (interactive "fNote: ")
  (denote cand (denote-keywords-prompt)))

(defun mainspring-note-items (input)
  "`consult' source items for notes.
This must be an async source because it is calling out to `rg' and `jq'."
  (mapcar (lambda (note)
            (list (let* ((meta (car (cdr note)))
                         (title (gethash "title" meta))
                         (id (gethash "identifier" meta)))
                    (concat
                     (propertize id 'face 'mainspring-notes-id-face)
                     " "
                     (propertize title 'face 'mainspring-notes-title-face)))
                  (car note)))
          (mainspring-get-notes-meta
           (mainspring-query-notes input 50))))

;;;###autoload
(defun mainspring-consult-notes ()
  "`consult' the available notes."
  (interactive)
  (consult--multi (list
                   (list :async (consult--dynamic-collection 'mainspring-note-items)
                         :action (lambda (cand) (find-file (car cand)))
                         :new #'mainspring-new-note
                         :require-match nil))
                  :prompt "Notes: "))

(provide 'mainspring-notes)
;;; mainspring-notes.el ends here
