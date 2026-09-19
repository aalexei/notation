;;; citar-notation.el --- Citar notes-source integration for notation -*- lexical-binding: t; -*-

;; Author: You
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (citar "1.0") (notation "0.1"))
;; Keywords: bib, notation

;;; Commentary:
;;
;; Registers `notation' as a Citar notes source, following the same
;; :name/:items/:hasitems/:open/:create contract used by
;; `citar-org-roam' and `citar-denote' (see
;; `citar-register-notes-source').
;;
;; Unlike citar-denote's default mechanism -- which stores a
;; citekey in a `#+reference:' front-matter line and finds it by
;; searching file content -- this integration stores the citekey as
;; a notation ALIAS (the "==alias" component of a note's file name),
;; and resolves it via `notation-all-aliases'. Since discovery only
;; ever looks at file names, not file contents, checking "does this
;; citekey have a note" never needs to read a single file's body:
;; the whole index falls out of one ripgrep filename scan
;; (`notation--find-note-files', via `notation-all-aliases').
;;
;; This means the association between a note and its citekey depends
;; entirely on the ALIAS the note's file name carries. If you already
;; have bibliographic notes whose citekey lives only in a
;; `#+reference:'-style front-matter line (e.g. from citar-denote's
;; default behaviour, carried over unchanged by a migration that only
;; touched file names), they won't be found here until that citekey
;; is added as a proper alias -- a rename, not a content edit.
;; `notation-doctor' will flag it if the same alias ever ends up on
;; more than one note.
;;
;; Setup:
;;
;;   (require 'citar-notation)
;;   (citar-notation-mode)
;;
;; This registers notation as Citar's active notes source in place of
;; whatever was configured before (e.g. `citar-denote-mode' or
;; `citar-org-roam-mode' -- only one Citar notes source is active at
;; a time). `M-x citar-notation-mode' again turns it off and restores
;; whatever was active previously.

;;; Code:

(require 'citar)
(require 'notation)

(defgroup citar-notation nil
  "Citar notes-source integration for notation."
  :group 'notation
  :prefix "citar-notation-")

(defcustom citar-notation-tag "bib"
  "Tag applied to new bibliographic notes created via Citar."
  :type 'string
  :group 'citar-notation)

(defcustom citar-notation-subdir "references"
  "Subdirectory (relative to `notation-directory') new bibliographic
notes are filed under. Set to nil to file them at the root instead."
  :type '(choice (const :tag "Notation root" nil) string)
  :group 'citar-notation)

;;;###autoload
(defun citar-notation--get-notes (&optional citekeys)
  "Return a hash table of notation note files associated with CITEKEYS.
If CITEKEYS is omitted, return every alias found on any notation
note (mirroring `citar-denote--get-notes'), not just ones Citar
currently knows about -- harmless, since Citar only ever looks up
keys it already has."
  (let ((all (notation-all-aliases)))
    (if (null citekeys)
        all
      (let ((table (make-hash-table :test #'equal)))
        (dolist (key citekeys)
          (when-let ((files (gethash key all)))
            (puthash key files table)))
        table))))

;;;###autoload
(defun citar-notation--has-notes ()
  "Return a predicate testing whether a citekey has a notation note.
See the docstring of `citar-has-notes' for the expected shape: this
builds the alias index once and returns a closure over it, rather
than re-scanning per candidate."
  (let ((notes (citar-notation--get-notes)))
    (unless (hash-table-empty-p notes)
      (lambda (citekey) (and (gethash citekey notes) t)))))

;;;###autoload
(defun citar-notation--create-note (citekey &optional _entry)
  "Create a notation note for CITEKEY.
CITEKEY is stored as the note's alias, so `citar-notation--get-notes'
resolves it afterwards. Prompts for a title, pre-filled from the
bibliography entry's title when available. Filed under
`citar-notation-subdir' and tagged with `citar-notation-tag'."
  (notation-create-note
   (read-string "Title: " (or (citar-get-value "title" citekey) citekey))
   (list citar-notation-tag)
   (list citekey)
   citar-notation-subdir
   notation-default-extension))

(defconst citar-notation-config
  (list :name "Notation"
        :category 'file
        :items #'citar-notation--get-notes
        :hasitems #'citar-notation--has-notes
        :open #'find-file
        :create #'citar-notation--create-note)
  "Tells Citar to use notation as its notes source.
Mirrors the shape of `citar-denote-config' and `citar-org-roam's own
registration.")

(defvar citar-notes-source)

(defvar citar-notation--orig-source nil
  "The `citar-notes-source' value from before `citar-notation-mode' was
enabled, so it can be restored on disable.")

(defun citar-notation-setup ()
  "Register notation as Citar's active notes source."
  (setq citar-notation--orig-source citar-notes-source)
  (citar-register-notes-source 'citar-notation-source citar-notation-config)
  (setq citar-notes-source 'citar-notation-source))

(defun citar-notation-reset ()
  "Restore whatever Citar notes source was active before
`citar-notation-mode' was enabled."
  (setq citar-notes-source citar-notation--orig-source)
  (citar-remove-notes-source 'citar-notation-source))

;;;###autoload
(define-minor-mode citar-notation-mode
  "Toggle integration between Citar and notation.
Only one Citar notes source can be active at a time -- enabling this
replaces whatever was configured before (e.g. `citar-denote-mode'),
and disabling it restores that prior source rather than leaving
Citar with no notes source at all."
  :global t
  :group 'citar-notation
  :lighter " citar-notation"
  (if citar-notation-mode
      (citar-notation-setup)
    (citar-notation-reset)))

(provide 'citar-notation)
;;; citar-notation.el ends here
