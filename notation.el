;;; notation.el --- Computable, denote-like notes, one directory per note -*- lexical-binding: t; -*-

;; Author: You
;; Version: 0.2
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
;; A note is identified purely structurally: a directory whose name
;; is a 14-digit timestamp (YYYYMMDDHHMMSS), containing a file whose
;; name starts with "__". Everything else -- where that directory
;; sits, what it's called, what the file's extension is -- is free.
;; This means notes can be filed into arbitrary subdirectories of
;; `notation-directory' (by topic, by project, however you like)
;; without breaking discovery.
;;
;; Layout example:
;;
;;   notation-directory/
;;     projects/emacs/
;;       20260910143022/
;;         __config_notes--emacs==wip=urgent.org
;;     journal/2026/
;;       20260911090000/
;;         __morning_pages.md
;;     20260912101500/
;;       __scan_of_receipt.pdf
;;
;; File name anatomy: __TITLE[--TAG1-TAG2...][==KEY1=KEY2...].EXT
;;
;;   - "__"    marks the file as a note's main file
;;   - TITLE   a slug of the title, words joined by "_"
;;   - "--"    introduces tags, individual tags joined by "-"
;;   - "=="    introduces keywords, individual keywords joined by "="
;;   - EXT     anything: org, md, pdf, ipynb, txt, ...
;;
;; Tags and keywords are both optional, and both mean "a short label
;; attached to the note" -- the distinction is left to you (e.g. tags
;; for topic, keywords for status/workflow), but they're kept in
;; separate namespaces so you can search/filter on either.
;;
;; Discovery of notes is done with ripgrep (`rg'), searched by
;; filename glob rather than by walking the directory tree from
;; Emacs Lisp, so it stays fast even with deep or large note trees.
;; Ripgrep must be installed and on `exec-path'.
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
  "Root directory holding all notes.
Notes may be organized into arbitrary subdirectories underneath it."
  :type 'directory
  :group 'notation)

(defcustom notation-default-extension "org"
  "Default file extension (without the dot) offered for new notes."
  :type 'string
  :group 'notation)

(defcustom notation-id-format "%Y%m%d%H%M%S"
  "`format-time-string' format used for note directory names.
Must always expand to 14 digits for `notation-id-regexp' to match."
  :type 'string
  :group 'notation)

(defcustom notation-rg-executable "rg"
  "Name or path of the ripgrep executable used for note discovery."
  :type 'string
  :group 'notation)

(defconst notation-id-regexp "\\`[0-9]\\{14\\}\\'"
  "Regexp matching a valid notation note-id directory name.")

(defconst notation-marker-regexp "\\`__"
  "Regexp matching the leading marker of a note's main file name.")

;;; Internal helpers: ids, slugs, file names

(defun notation--ensure-root ()
  "Make sure `notation-directory' exists."
  (unless (file-directory-p notation-directory)
    (make-directory notation-directory t)))

(defun notation--new-id ()
  "Return a fresh 14-digit timestamp id, guaranteed unique on disk."
  (notation--ensure-root)
  (let ((id (format-time-string notation-id-format)))
    ;; Guard against creating two notes within the same second.
    ;; Uniqueness only needs to hold within the target directory, but
    ;; checking globally is simpler and the collision is already rare.
    (while (notation--id-in-use-p id)
      (sleep-for 0 1000) ;; wait 1s
      (setq id (format-time-string notation-id-format)))
    id))

(defun notation--id-in-use-p (id)
  "Return non-nil if ID already names a note directory anywhere."
  (seq-some (lambda (file)
              (string= id (notation--id-from-file file)))
            (notation--find-note-files)))

(defun notation--slug (title)
  "Turn TITLE into a filename-safe slug, words joined by underscores."
  (let* ((down (downcase title))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "_" down)))
    (string-trim slug "_+" "_+")))

(defun notation--token (s)
  "Turn S into a single bare alphanumeric token (for a tag or keyword)."
  (replace-regexp-in-string "[^a-z0-9]+" "" (downcase s)))

(defun notation--tokens-from-string (s)
  "Split S (comma/space separated) into a list of slug tokens."
  (thread-last
    (split-string s "[,\s]+" t "\s+")
    (mapcar #'notation--token)
    (delete "")))

(defun notation--file-name (title tags keywords extension)
  "Build a note file name from TITLE, TAGS, KEYWORDS and EXTENSION.
TAGS and KEYWORDS are lists of bare tokens (see `notation--token').
EXTENSION is given without a leading dot."
  (let* ((slug (notation--slug title))
         (tag-part (if tags (concat "--" (mapconcat #'identity tags "-")) ""))
         (key-part (if keywords (concat "==" (mapconcat #'identity keywords "=")) "")))
    (concat "__" slug tag-part key-part "." extension)))

(defun notation--parse-file-name (file)
  "Return a plist (:title :tags :keywords) parsed from FILE's name."
  (let* ((base (file-name-base file))
         (body (if (string-match notation-marker-regexp base)
                   (substring base (match-end 0))
                 base))
         keywords tags title)
    ;; "==" (keyword marker) and "--" (tag marker) can each only occur
    ;; once, as markers -- title/tag/keyword tokens never contain "="
    ;; or "-" themselves -- so a single search for each is unambiguous.
    (when (string-match "==\\(.+\\)\\'" body)
      (setq keywords (split-string (match-string 1 body) "=" t))
      (setq body (substring body 0 (match-beginning 0))))
    (when (string-match "--\\(.+\\)\\'" body)
      (setq tags (split-string (match-string 1 body) "-" t))
      (setq body (substring body 0 (match-beginning 0))))
    (setq title body)
    (list :title title :tags tags :keywords keywords)))

;;; Internal helpers: discovery via ripgrep

(defun notation--check-rg ()
  "Signal a user-error if ripgrep isn't available."
  (unless (executable-find notation-rg-executable)
    (user-error "Ripgrep (%s) not found; install it or set `notation-rg-executable'"
                notation-rg-executable)))

(defun notation--find-note-files ()
  "Return absolute paths of all note main files under `notation-directory'.
A candidate file is any file whose name starts with \"__\"; results
are then filtered down to those directly inside a 14-digit id
directory, which is what actually makes something a note."
  (notation--ensure-root)
  (notation--check-rg)
  (let* ((default-directory notation-directory)
         (output
          (with-temp-buffer
            (let ((status (call-process notation-rg-executable nil t nil
                                         "--files" "--hidden" "--no-messages"
                                         "--no-ignore-vcs"
                                         "-g" "__*")))
              ;; rg exits 1 when it simply found nothing; only treat
              ;; other non-zero statuses as real errors.
              (unless (memq status '(0 1))
                (error "ripgrep failed: %s" (string-trim (buffer-string))))
              (buffer-string)))))
    (thread-last
      (split-string output "\n" t)
      (mapcar (lambda (rel) (expand-file-name rel notation-directory)))
      (seq-filter #'notation--note-file-p))))

(defun notation--note-file-p (file)
  "Return non-nil if FILE is a valid note main file.
That means it lives directly inside a directory named with a
14-digit id."
  (let ((parent (file-name-nondirectory
                 (directory-file-name (file-name-directory file)))))
    (string-match-p notation-id-regexp parent)))

(defun notation--id-from-file (file)
  "Return the 14-digit id of the note directory containing FILE."
  (file-name-nondirectory (directory-file-name (file-name-directory file))))

(defun notation--subdir-from-file (file)
  "Return FILE's path relative to `notation-directory', excluding the
id directory and file name -- i.e. the arbitrary filing subdirectory
the note lives under, or nil if it's directly under the root."
  (let* ((id-dir (directory-file-name (file-name-directory file)))
         (container (file-name-directory id-dir))
         (rel (file-relative-name container notation-directory)))
    (unless (member rel '("./" "."))
      (directory-file-name rel))))

(defun notation--all-notes ()
  "Return an alist of (DISPLAY . FILE-PATH) for every existing note."
  (thread-last
    (notation--find-note-files)
    (mapcar
     (lambda (file)
       (let* ((id (notation--id-from-file file))
              (subdir (notation--subdir-from-file file))
              (parsed (notation--parse-file-name file))
              (title (plist-get parsed :title))
              (tags (plist-get parsed :tags))
              (keywords (plist-get parsed :keywords))
              (display
               (format "%s  %s%s%s%s"
                       id
                       (if subdir (format "%s/  " subdir) "")
                       title
                       (if tags (format "  [%s]" (mapconcat #'identity tags " ")) "")
                       (if keywords (format "  {%s}" (mapconcat #'identity keywords " ")) ""))))
         (cons display file))))
    (sort (lambda (a b) (string> (car a) (car b))))))

(defun notation--suggest-subdir ()
  "Suggest a default subdirectory for a new note.
If the current buffer is visiting a file under `notation-directory',
suggest the filing path it lives under -- i.e. the same directory a
sibling note would go in. If that file itself sits directly inside a
note's own 14-digit id directory, that id directory is dropped from
the suggestion, since a new note shouldn't be nested inside another
note's directory. Returns nil (no suggestion) if the current buffer
isn't inside `notation-directory' at all."
  (let ((file (buffer-file-name)))
    (when (and file (file-in-directory-p file notation-directory))
      (let* ((dir (file-name-directory file))
             (rel (file-relative-name dir notation-directory)))
        (unless (member rel '("./" "." ""))
          (let* ((rel (directory-file-name rel))
                 (parts (split-string rel "/" t)))
            (when (and parts (string-match-p notation-id-regexp (car (last parts))))
              (setq parts (butlast parts)))
            (when parts
              (mapconcat #'identity parts "/"))))))))

(defun notation--insert-front-matter (title tags keywords)
  "Insert denote-style front matter for TITLE, TAGS and KEYWORDS at point."
  (insert "#+title:      " title "\n")
  (insert "#+date:       " (format-time-string "%Y-%m-%d %H:%M") "\n")
  (insert "#+filetags:   " (if tags (concat ":" (mapconcat #'identity tags ":") ":") "") "\n")
  (insert "#+keywords:   " (if keywords (mapconcat #'identity keywords " ") "") "\n\n"))

;;; Commands

;;;###autoload
(defun notation-new-note (title tags keywords subdir extension)
  "Create a new note titled TITLE with TAGS and KEYWORDS.
TAGS and KEYWORDS are read as comma/space separated strings and each
split into bare tokens. SUBDIR, if non-empty, is a subdirectory path
(relative to `notation-directory') the note's id directory is placed
under, letting notes be organized arbitrarily. When invoked from a
buffer already visiting a file under `notation-directory', SUBDIR is
pre-filled with that file's filing directory, so creating a note
while looking at a related one defaults to filing it alongside.
EXTENSION (without a leading dot) controls the main file's type --
org, md, pdf, ipynb, or anything else.

Creates a new timestamped id directory, writes the main note file
inside it, and opens it."
  (interactive
   (list (read-string "Title: ")
         (read-string "Tags (comma/space separated, optional): ")
         (read-string "Keywords (comma/space separated, optional): ")
         (read-string "Subdirectory (optional, e.g. projects/emacs): "
                      (notation--suggest-subdir))
         (read-string (format "Extension (default %s): " notation-default-extension)
                      nil nil notation-default-extension)))
  (when (string-empty-p (string-trim title))
    (user-error "Title must not be empty"))
  (let* ((id (notation--new-id))
         (base-dir (if (string-empty-p (string-trim subdir))
                       notation-directory
                     (expand-file-name (string-trim subdir) notation-directory)))
         (dir (expand-file-name id base-dir))
         (tag-list (notation--tokens-from-string tags))
         (keyword-list (notation--tokens-from-string keywords))
         (ext (string-trim (string-remove-prefix "." extension)))
         (file-name (notation--file-name title tag-list keyword-list ext))
         (path (expand-file-name file-name dir)))
    (make-directory dir t)
    (find-file path)
    (when (member ext '("org" "md" "markdown" "txt"))
      (notation--insert-front-matter title tag-list keyword-list))
    (save-buffer)
    (message "Created note %s" id)))

;;;###autoload
(defun notation-find-note ()
  "Prompt for an existing note (via ripgrep) and open its main file."
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
Prompts for a new title, tags and keywords, keeping the note's
directory (and therefore its id/timestamp and location) unchanged."
  (interactive)
  (let* ((file (buffer-file-name))
         (dir (and file (file-name-directory file))))
    (unless (and file (notation--note-file-p file))
      (user-error "Current buffer is not visiting a notation note"))
    (let* ((parsed (notation--parse-file-name file))
           (title (read-string "Title: "
                                (replace-regexp-in-string "_" " " (plist-get parsed :title))))
           (tags (read-string "Tags: "
                               (mapconcat #'identity (plist-get parsed :tags) " ")))
           (keywords (read-string "Keywords: "
                                   (mapconcat #'identity (plist-get parsed :keywords) " ")))
           (tag-list (notation--tokens-from-string tags))
           (keyword-list (notation--tokens-from-string keywords))
           (ext (file-name-extension file))
           (new-name (notation--file-name title tag-list keyword-list ext))
           (new-path (expand-file-name new-name dir)))
      (rename-file file new-path)
      (set-visited-file-name new-path t t)
      (message "Renamed to %s" new-name))))

(provide 'notation)
;;; notation.el ends here
