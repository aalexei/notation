;;; notation.el --- Computable, denote-like notes, one directory per note -*- lexical-binding: t; -*-

;; Author: You
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: outlines, files, convenience

;;; Commentary:
;;
;; Notation is a minimal note-taking system inspired by `denote', but
;; where each note lives in its own directory rather than as a bare
;; file. The per-note directory gives each note room to hold code,
;; data, and generated output alongside the note text itself -- the
;; goal is notes you can compute on, not just read.
;;
;; Layout:
;;
;;   notation-directory/
;;     20260910143022/
;;       __my-first-note--emacs-notes.org
;;     20260910151101/
;;       __another-note.org
;;
;; - The directory name is a 14-digit timestamp: YYYYMMDDHHMMSS
;; - The file inside follows: __TITLE-SLUG--TAG1-TAG2.org
;;   (tags and the leading "--" are omitted if there are no tags)
;;
;; Keeping each note in its own directory leaves room to drop
;; attachments (images, PDFs, data files) alongside the note itself
;; without cluttering a single flat notes directory.
;;
;; Entry points:
;;   M-x notation-new-note
;;   M-x notation-find-note
;;   M-x notation-rename-note

;;; Code:

(require 'seq)
(require 'subr-x)

;;; Customization

(defgroup notation nil
  "Denote-like notes, one directory per note."
  :group 'files
  :prefix "notation-")

(defcustom notation-directory (expand-file-name "~/notes/")
  "Root directory holding all note directories."
  :type 'directory
  :group 'notation)

(defcustom notation-file-extension ".org"
  "File extension used for the main note file."
  :type 'string
  :group 'notation)

(defcustom notation-id-format "%Y%m%d%H%M%S"
  "`format-time-string' format used for note directory names.
Must always expand to 14 digits for `notation-id-regexp' to match."
  :type 'string
  :group 'notation)

(defconst notation-id-regexp "\\`[0-9]\\{14\\}\\'"
  "Regexp matching a valid notation note-id directory name.")

;;; Internal helpers

(defun notation--ensure-root ()
  "Make sure `notation-directory' exists."
  (unless (file-directory-p notation-directory)
    (make-directory notation-directory t)))

(defun notation--new-id ()
  "Return a fresh 14-digit timestamp id, guaranteed unique on disk."
  (notation--ensure-root)
  (let ((id (format-time-string notation-id-format)))
    ;; Guard against creating two notes within the same second.
    (while (file-directory-p (expand-file-name id notation-directory))
      (sleep-for 0 1000) ;; wait 1s
      (setq id (format-time-string notation-id-format)))
    id))

(defun notation--slug (title)
  "Turn TITLE into a filename-safe slug."
  (let* ((down (downcase title))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "-" down)))
    (string-trim slug "-+" "-+")))

(defun notation--tags-to-list (tags-string)
  "Split TAGS-STRING (comma or space separated) into a list of slugs."
  (thread-last
    (split-string tags-string "[,\s]+" t "\s+")
    (mapcar #'notation--slug)
    (delete "")))

(defun notation--file-name (title tags)
  "Build the note file name for TITLE and TAGS (a list of strings)."
  (let* ((slug (notation--slug title))
         (tag-part (if tags
                       (concat "--" (mapconcat #'identity tags "-"))
                     "")))
    (concat "__" slug tag-part notation-file-extension)))

(defun notation--note-dirs ()
  "Return a list of absolute paths to all note directories, newest first."
  (notation--ensure-root)
  (thread-last
    (directory-files notation-directory nil notation-id-regexp)
    (sort (lambda (a b) (string> a b)))
    (mapcar (lambda (id) (expand-file-name id notation-directory)))))

(defun notation--main-file-in-dir (dir)
  "Return the path to the main note file inside DIR, or nil."
  (car (directory-files dir t "\\`__.*\\..+\\'")))

(defun notation--parse-file-name (file)
  "Return a plist (:title TITLE :tags TAGS) parsed from FILE's name."
  (let* ((base (file-name-base file))
         ;; strip leading "__"
         (body (if (string-prefix-p "__" base) (substring base 2) base))
         (parts (split-string body "--" t)))
    (list :title (or (car parts) body)
          :tags (if (cadr parts) (split-string (cadr parts) "-" t) nil))))

(defun notation--all-notes ()
  "Return an alist of (DISPLAY . FILE-PATH) for every existing note."
  (let (result)
    (dolist (dir (notation--note-dirs))
      (let ((file (notation--main-file-in-dir dir)))
        (when file
          (let* ((id (file-name-nondirectory (directory-file-name dir)))
                 (parsed (notation--parse-file-name file))
                 (title (plist-get parsed :title))
                 (tags (plist-get parsed :tags))
                 (display (format "%s  %s%s"
                                   id
                                   title
                                   (if tags
                                       (format "  [%s]" (mapconcat #'identity tags " "))
                                     ""))))
            (push (cons display file) result)))))
    (nreverse result)))

(defun notation--insert-front-matter (title tags)
  "Insert denote-style front matter for TITLE and TAGS at point."
  (insert "#+title:      " title "\n")
  (insert "#+date:       " (format-time-string "%Y-%m-%d %H:%M") "\n")
  (insert "#+filetags:   " (if tags
                                (concat ":" (mapconcat #'identity tags ":") ":")
                              "")
          "\n\n"))

;;; Commands

;;;###autoload
(defun notation-new-note (title tags)
  "Create a new note titled TITLE with TAGS.
TAGS is read as a comma/space separated string and split into slugs.
Creates a new timestamped directory under `notation-directory',
writes the main note file inside it, and opens it."
  (interactive
   (list (read-string "Title: ")
         (read-string "Tags (comma/space separated, optional): ")))
  (when (string-empty-p (string-trim title))
    (user-error "Title must not be empty"))
  (let* ((id (notation--new-id))
         (dir (expand-file-name id notation-directory))
         (tag-list (notation--tags-to-list tags))
         (file-name (notation--file-name title tag-list))
         (path (expand-file-name file-name dir)))
    (make-directory dir t)
    (find-file path)
    (notation--insert-front-matter title tag-list)
    (save-buffer)
    (message "Created note %s" id)))

;;;###autoload
(defun notation-find-note ()
  "Prompt for an existing note and open its main file."
  (interactive)
  (let ((notes (notation--all-notes)))
    (unless notes
      (user-error "No notes found in %s" notation-directory))
    (let* ((choice (completing-read "Note: " (mapcar #'car notes) nil t))
           (file (cdr (assoc choice notes))))
      (find-file file))))

;;;###autoload
(defun notation-rename-note ()
  "Rename the note file visited by the current buffer.
Prompts for a new title and tags, keeping the note's directory
(and therefore its id/timestamp) unchanged."
  (interactive)
  (let* ((file (buffer-file-name))
         (dir (and file (file-name-directory file))))
    (unless (and file
                 (string-match-p notation-id-regexp
                                  (file-name-nondirectory
                                   (directory-file-name dir))))
      (user-error "Current buffer is not visiting a notation note"))
    (let* ((parsed (notation--parse-file-name file))
           (title (read-string "Title: " (plist-get parsed :title)))
           (tags (read-string "Tags: "
                               (mapconcat #'identity (plist-get parsed :tags) " ")))
           (tag-list (notation--tags-to-list tags))
           (new-name (notation--file-name title tag-list))
           (new-path (expand-file-name new-name dir)))
      (rename-file file new-path)
      (set-visited-file-name new-path t t)
      (message "Renamed to %s" new-name))))

(provide 'notation)
;;; notation.el ends here
