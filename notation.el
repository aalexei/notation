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
;; File name anatomy: __TITLE[--TAG1-TAG2...][==ALIAS1=ALIAS2...].EXT
;;
;;   - "__"    marks the file as a note's main file
;;   - TITLE   a slug of the title, words joined by "_"
;;   - "--"    introduces tags, individual tags joined by "-"
;;   - "=="    introduces aliases, individual aliases joined by "="
;;   - EXT     anything: org, md, pdf, ipynb, txt, ...
;;
;; Tags and aliases are both optional. Tags are topical labels; an
;; alias is a user-supplied alternate name the note can also be
;; found by (e.g. an old title, an abbreviation, a nickname). They're
;; kept in separate namespaces so you can search/filter on either.
;;
;; Discovery of notes is done with ripgrep (`rg'), searched by
;; filename glob rather than by walking the directory tree from
;; Emacs Lisp, so it stays fast even with deep or large note trees.
;; Ripgrep must be installed and on `exec-path'.
;;
;; Notes can link to each other with a plain-text reference of the
;; form "notation:ID" -- always the 14-digit id, never an alias,
;; since aliases aren't guaranteed unique and a link has to resolve
;; to exactly one note. Because the reference is plain text, it works
;; the same way regardless of file type: Org gets a native, clickable
;; link type; anywhere else, `notation-follow-link-at-point' finds
;; and follows the "notation:ID" text near point directly. A note's
;; own directory must contain exactly one "__" file for a link to it
;; to resolve; this is enforced at follow time, and `notation-doctor'
;; audits the whole tree for that (and other) invariant violations.
;;
;; Entry points:
;;   M-x notation-new-note
;;   M-x notation-find-note
;;   M-x notation-rename-note
;;   M-x notation-insert-link
;;   M-x notation-follow-link-at-point
;;   M-x notation-backlinks
;;   M-x notation-doctor

;;; Code:

(require 'seq)
(require 'subr-x)

;;; Customization

(defgroup notation nil
  "Denote-like notes, one directory per note."
  :group 'files
  :prefix "notation-")

(defcustom notation-directory (expand-file-name "~/notation/")
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
  "Turn S into a single bare alphanumeric token (for a tag or alias)."
  (replace-regexp-in-string "[^a-z0-9]+" "" (downcase s)))

(defun notation--tokens-from-string (s)
  "Split S (comma/space separated) into a list of slug tokens."
  (thread-last
    (split-string s "[,\s]+" t "\s+")
    (mapcar #'notation--token)
    (delete "")))

(defun notation--file-name (title tags aliases extension)
  "Build a note file name from TITLE, TAGS, ALIASES and EXTENSION.
TAGS and ALIASES are lists of bare tokens (see `notation--token').
EXTENSION is given without a leading dot."
  (let* ((slug (notation--slug title))
         (tag-part (if tags (concat "--" (mapconcat #'identity tags "-")) ""))
         (alias-part (if aliases (concat "==" (mapconcat #'identity aliases "=")) "")))
    (concat "__" slug tag-part alias-part "." extension)))

(defun notation--parse-file-name (file)
  "Return a plist (:title :tags :aliases) parsed from FILE's name."
  (let* ((base (file-name-base file))
         (body (if (string-match notation-marker-regexp base)
                   (substring base (match-end 0))
                 base))
         aliases tags title)
    ;; "==" (alias marker) and "--" (tag marker) can each only occur
    ;; once, as markers -- title/tag/alias tokens never contain "="
    ;; or "-" themselves -- so a single search for each is unambiguous.
    (when (string-match "==\\(.+\\)\\'" body)
      (setq aliases (split-string (match-string 1 body) "=" t))
      (setq body (substring body 0 (match-beginning 0))))
    (when (string-match "--\\(.+\\)\\'" body)
      (setq tags (split-string (match-string 1 body) "-" t))
      (setq body (substring body 0 (match-beginning 0))))
    (setq title body)
    (list :title title :tags tags :aliases aliases)))

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
  (let ((entries
         (mapcar
          (lambda (file)
            (let* ((id (notation--id-from-file file))
                   (subdir (notation--subdir-from-file file))
                   (parsed (notation--parse-file-name file))
                   (title (plist-get parsed :title))
                   (tags (plist-get parsed :tags))
                   (aliases (plist-get parsed :aliases))
                   (display
                    (format "%s  %s%s%s%s"
                            id
                            (if subdir (format "%s/  " subdir) "")
                            title
                            (if tags (format "  [%s]" (mapconcat #'identity tags " ")) "")
                            (if aliases (format "  {%s}" (mapconcat #'identity aliases " ")) ""))))
              (cons display file)))
          (notation--find-note-files))))
    ;; NOTE: deliberately not `thread-last' here -- `sort' takes
    ;; (SEQUENCE PREDICATE), but `thread-last' appends the threaded
    ;; value as the *last* argument of each form, which would call
    ;; `sort' as (sort PREDICATE SEQUENCE) -- backwards.
    (sort entries (lambda (a b) (string> (car a) (car b))))))

;;; Internal helpers: links

(defun notation--files-for-id (id)
  "Return the list of \"__\" files that live in a note directory named ID.
Searched directly with ripgrep (rather than filtering the full note
list), so resolving a single link stays cheap regardless of corpus
size. Ordinarily this should be a list of exactly one file; anything
else means the one-main-file-per-directory invariant is broken."
  (notation--ensure-root)
  (notation--check-rg)
  (let* ((default-directory notation-directory)
         (glob (format "**/%s/__*" id))
         (output
          (with-temp-buffer
            (let ((status (call-process notation-rg-executable nil t nil
                                         "--files" "--hidden" "--no-messages"
                                         "--no-ignore-vcs"
                                         "-g" glob)))
              (unless (memq status '(0 1))
                (error "ripgrep failed: %s" (string-trim (buffer-string))))
              (buffer-string)))))
    (thread-last
      (split-string output "\n" t)
      (mapcar (lambda (rel) (expand-file-name rel notation-directory))))))

(defun notation-resolve-id (id)
  "Return the absolute path of the note file whose directory is ID.
Signals a `user-error' if ID isn't a valid id or no such note exists,
or a hard `error' if ID's directory contains more than one \"__\"
file, since that violates the one-main-file-per-note invariant a
link's resolution depends on."
  (unless (string-match-p notation-id-regexp id)
    (user-error "\"%s\" is not a valid 14-digit note id" id))
  (let ((files (notation--files-for-id id)))
    (cond
     ((null files) (user-error "No note found with id %s" id))
     ((cdr files)
      (error "Note directory %s contains multiple \"__\" files: %s"
             id (mapconcat #'identity files ", ")))
     (t (car files)))))

(defun notation--link-id-at-point ()
  "Return the id referenced by a \"notation:ID\" text near point, or nil.
Scans the current line for occurrences of the pattern; if point
falls inside one, that one wins, otherwise a single unambiguous match
on the line is used as a fallback."
  (save-excursion
    (let ((line-end (line-end-position))
          (pt (point))
          (matches nil)
          (contained nil))
      (beginning-of-line)
      (while (re-search-forward "notation:\\([0-9]\\{14\\}\\)" line-end t)
        (let ((beg (match-beginning 0))
              (end (match-end 0))
              (id (match-string 1)))
          (push id matches)
          (when (and (<= beg pt) (<= pt end))
            (setq contained id))))
      (or contained
          (and (= (length matches) 1) (car matches))))))

(defun notation--search-text (needle)
  "Return note main files under `notation-directory' containing NEEDLE.
NEEDLE is matched literally (not as a regexp) via ripgrep's content
search. Results are filtered down to actual note main files, so a
stray match inside some other file (an attachment, a cache file)
doesn't get treated as a backlink."
  (notation--ensure-root)
  (notation--check-rg)
  (let* ((default-directory notation-directory)
         (output
          (with-temp-buffer
            (let ((status (call-process notation-rg-executable nil t nil
                                         "--files-with-matches" "--hidden" "--no-messages"
                                         "--no-ignore-vcs" "--fixed-strings"
                                         needle)))
              (unless (memq status '(0 1))
                (error "ripgrep failed: %s" (string-trim (buffer-string))))
              (buffer-string)))))
    (thread-last
      (split-string output "\n" t)
      (mapcar (lambda (rel) (expand-file-name rel notation-directory)))
      (seq-filter #'notation--note-file-p))))

(defun notation--all-id-dirs-with-files ()
  "Return an alist of (ID-DIR . FILES) for every id-shaped directory.
Unlike `notation--find-note-files', this lists *every* directory
under `notation-directory' whose name matches `notation-id-regexp',
along with everything it directly contains -- including directories
that have no \"__\" file, or more than one -- so `notation-doctor'
can detect problems that well-formed note discovery would otherwise
just silently skip."
  (notation--ensure-root)
  (notation--check-rg)
  (let* ((default-directory notation-directory)
         (output
          (with-temp-buffer
            (let ((status (call-process notation-rg-executable nil t nil
                                         "--files" "--hidden" "--no-messages"
                                         "--no-ignore-vcs")))
              (unless (memq status '(0 1))
                (error "ripgrep failed: %s" (string-trim (buffer-string))))
              (buffer-string))))
         (files (mapcar (lambda (rel) (expand-file-name rel notation-directory))
                         (split-string output "\n" t)))
         (table (make-hash-table :test #'equal)))
    (dolist (file files)
      (let* ((dir (directory-file-name (file-name-directory file)))
             (parent-name (file-name-nondirectory dir)))
        (when (string-match-p notation-id-regexp parent-name)
          (puthash dir (cons file (gethash dir table)) table))))
    (let (result)
      (maphash (lambda (dir dir-files) (push (cons dir dir-files) result)) table)
      result)))

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

(defun notation--insert-front-matter (title tags aliases)
  "Insert denote-style front matter for TITLE, TAGS and ALIASES at point."
  (insert "#+title:      " title "\n")
  (insert "#+date:       " (format-time-string "%Y-%m-%d %H:%M") "\n")
  (insert "#+filetags:   " (if tags (concat ":" (mapconcat #'identity tags ":") ":") "") "\n")
  (insert "#+aliases:    " (if aliases (mapconcat #'identity aliases " ") "") "\n\n"))

;;; Commands

;;;###autoload
(defun notation-new-note (title tags aliases subdir extension)
  "Create a new note titled TITLE with TAGS and ALIASES.
TAGS and ALIASES are read as comma/space separated strings and each
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
         (read-string "Aliases (comma/space separated, optional): ")
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
         (alias-list (notation--tokens-from-string aliases))
         (ext (string-trim (string-remove-prefix "." extension)))
         (file-name (notation--file-name title tag-list alias-list ext))
         (path (expand-file-name file-name dir)))
    (make-directory dir t)
    (find-file path)
    (when (member ext '("org" "md" "markdown" "txt"))
      (notation--insert-front-matter title tag-list alias-list))
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
Prompts for a new title, tags and aliases, keeping the note's
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
           (aliases (read-string "Aliases: "
                                   (mapconcat #'identity (plist-get parsed :aliases) " ")))
           (tag-list (notation--tokens-from-string tags))
           (alias-list (notation--tokens-from-string aliases))
           (ext (file-name-extension file))
           (new-name (notation--file-name title tag-list alias-list ext))
           (new-path (expand-file-name new-name dir)))
      (rename-file file new-path)
      (set-visited-file-name new-path t t)
      (message "Renamed to %s" new-name))))

;;;###autoload
(defun notation-insert-link ()
  "Search for a note and insert a link to it at point.
The inserted link always encodes only the note's id -- never an
alias, since aliases aren't guaranteed unique. Where the surrounding
format supports a visible description, the note's title is used for
that, purely for readability: it is never consulted when the link is
later followed, so it can go stale harmlessly if the note is renamed.

Link syntax adapts to the current major mode: a native Org link in
Org buffers, Markdown link syntax in Markdown buffers, and a plain
\"notation:ID (Title)\" form everywhere else."
  (interactive)
  (let ((notes (notation--all-notes)))
    (unless notes
      (user-error "No notes found in %s" notation-directory))
    (let* ((choice (completing-read "Link to note: " (mapcar #'car notes) nil t))
           (file (cdr (assoc choice notes)))
           (id (notation--id-from-file file))
           (parsed (notation--parse-file-name file))
           (title (replace-regexp-in-string "_" " " (plist-get parsed :title))))
      (insert
       (cond
        ((derived-mode-p 'org-mode)
         (format "[[notation:%s][%s]]" id title))
        ((derived-mode-p 'markdown-mode)
         (format "[%s](notation:%s)" title id))
        (t (format "notation:%s (%s)" id title)))))))

;;;###autoload
(defun notation-follow-link-at-point ()
  "Open the note referenced by a \"notation:ID\" link at point.
Works in any buffer or major mode, since the link is just text -- it
finds the id near point directly rather than relying on a mode's own
link-following machinery (Org links are additionally handled
natively; see the \"notation\" link type registered below)."
  (interactive)
  (let ((id (notation--link-id-at-point)))
    (unless id
      (user-error "No notation link at point"))
    (find-file (notation-resolve-id id))))

;;;###autoload
(defun notation-backlinks (&optional id)
  "Show notes that link to the note with ID.
With no ID and the current buffer visiting a note, uses that note's
id; otherwise prompts for one. Backlinks are found by searching the
whole `notation-directory' tree for the literal text \"notation:ID\"
across note main files; binary files such as PDFs are naturally
excluded since ripgrep only content-searches text."
  (interactive)
  (let* ((current-id (and (buffer-file-name)
                           (notation--note-file-p (buffer-file-name))
                           (notation--id-from-file (buffer-file-name))))
         (id (or id current-id (read-string "Note id to find backlinks for: "))))
    (unless (string-match-p notation-id-regexp id)
      (user-error "\"%s\" is not a valid 14-digit note id" id))
    (let* ((hits (notation--search-text (format "notation:%s" id)))
           (hits (seq-remove (lambda (f) (string= (notation--id-from-file f) id)) hits)))
      (cond
       ((null hits) (message "No backlinks found for %s" id))
       ((null (cdr hits)) (find-file (car hits)))
       (t
        (let* ((alist
                (mapcar
                 (lambda (f)
                   (let* ((fid (notation--id-from-file f))
                          (subdir (notation--subdir-from-file f))
                          (title (plist-get (notation--parse-file-name f) :title)))
                     (cons (format "%s  %s%s" fid (if subdir (format "%s/  " subdir) "") title) f)))
                 hits))
               (choice (completing-read (format "Backlinks to %s: " id)
                                         (mapcar #'car alist) nil t)))
          (find-file (cdr (assoc choice alist)))))))))

;;;###autoload
(defun notation-doctor ()
  "Audit all note directories under `notation-directory' for problems.
Checks that every id-shaped directory contains exactly one \"__\"
file -- the invariant that note discovery and link resolution both
depend on -- and that no 14-digit id is reused across more than one
directory. Results are shown in a `*notation-doctor*' buffer; if
everything checks out, reports so in the echo area instead."
  (interactive)
  (let* ((id-dirs (notation--all-id-dirs-with-files))
         (problems nil)
         (id-locations (make-hash-table :test #'equal)))
    (dolist (entry id-dirs)
      (let* ((dir (car entry))
             (files (cdr entry))
             (main-files (seq-filter
                          (lambda (f) (string-match-p notation-marker-regexp
                                                       (file-name-nondirectory f)))
                          files))
             (id (file-name-nondirectory dir)))
        (puthash id (cons dir (gethash id id-locations)) id-locations)
        (cond
         ((null main-files)
          (push (format "%s -- no \"__\" file (invisible to note discovery)" dir)
                problems))
         ((cdr main-files)
          (push (format "%s -- %d \"__\" files, expected exactly 1: %s"
                        dir (length main-files)
                        (mapconcat #'file-name-nondirectory main-files ", "))
                problems)))))
    (maphash
     (lambda (id dirs)
       (when (cdr dirs)
         (push (format "id %s reused by %d directories: %s"
                        id (length dirs) (mapconcat #'identity dirs ", "))
               problems)))
     id-locations)
    (if (null problems)
        (message "notation-doctor: checked %d note director%s under %s, no problems found"
                  (length id-dirs) (if (= (length id-dirs) 1) "y" "ies")
                  notation-directory)
      (setq problems (nreverse problems))
      (with-current-buffer (get-buffer-create "*notation-doctor*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "notation-doctor: %d problem%s found under %s\n\n"
                           (length problems) (if (= (length problems) 1) "" "s")
                           notation-directory))
          (dolist (p problems)
            (insert "- " p "\n")))
        (goto-char (point-min))
        (special-mode)
        (display-buffer (current-buffer)))
      (message "notation-doctor: %d problem%s found, see *notation-doctor*"
                (length problems) (if (= (length problems) 1) "" "s")))))

;;; Org integration

(with-eval-after-load 'org
  (org-link-set-parameters
   "notation"
   :follow (lambda (id &rest _) (find-file (notation-resolve-id id)))
   :face 'org-link))

(provide 'notation)
;;; notation.el ends here
