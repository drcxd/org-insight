;;; org-insight.el --- Recursive grouped search with live whole-file preview -*- lexical-binding: t; -*-
;;
;; Author: Your Name <you@example.com>
;; Maintainer: Your Name <you@example.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, search, convenience
;; URL: https://example.com/org-insight
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; org-insight provides a self-contained recursive search UI that:
;; - Groups matches by file (header shows Org #+TITLE when present; otherwise file name)
;; - Results buffer with navigation:
;;     n/p : next/previous match
;;     N/P : next/previous file group
;;     RET : visit match (optional)
;;     q   : quit results AND close the preview window (buffer kept)
;; - Preview shows the WHOLE file in a side window (without visiting the file),
;;   highlights the match line, highlights keyword(s) in a distinct face,
;;   and recenters the match line to the middle of the window.
;; - Helper command to search a preconfigured directory and only prompt for the keyword.
;;
;; Usage:
;;   M-x org-insight
;;   (setq org-insight-default-directory "~/org")
;;   M-x org-insight-in-default-directory
;;
;;; License:
;; MIT
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; -----------------------------------------------------------------------------
;; Customization
;; -----------------------------------------------------------------------------

(defgroup org-insight nil
  "Recursive grouped search with live non-visiting preview."
  :group 'tools)

(defcustom org-insight-use-regexp t
  "If non-nil, treat input keyword as a regexp; otherwise match literally."
  :type 'boolean :group 'org-insight)

(defcustom org-insight-default-directory nil
  "Default directory for `org-insight-in-default-directory'.
If nil, the command will signal an error until you set this variable."
  :type 'directory :group 'org-insight)

(defcustom org-insight-ignore-file-predicate
  (lambda (path)
    "Return non-nil to skip PATH. Default: skip VCS dirs & common binaries."
    (let ((case-fold-search t))
      (or (string-match-p "/\\.\\(git\\|hg\\|svn\\)/" path)
          (string-match-p "\\.\\(png\\|jpg\\|jpeg\\|gif\\|bmp\\|pdf\\|zip\\|tar\\|gz\\|7z\\|exe\\|dll\\)$" path))))
  "Predicate called with absolute PATH; return non-nil to ignore it."
  :type 'function :group 'org-insight)

;; -----------------------------------------------------------------------------
;; Data structures & buffer-locals
;; -----------------------------------------------------------------------------

(cl-defstruct org-insight-item file line text)

(defvar-local org-insight--groups nil
  "List of file groups for rendering: plist (:file FILE :display NAME :items (ITEM ...)).")

(defvar-local org-insight--group-positions nil
  "List of (FILE . MARKER) of group header start positions, in render order.")

(defvar-local org-insight--preview-keyword nil)
(defvar-local org-insight--preview-regexp-p nil)
(defvar-local org-insight--last-preview-key nil)

;; -----------------------------------------------------------------------------
;; Mode & keymap
;; -----------------------------------------------------------------------------

(defvar org-insight-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'org-insight-next-result)
    (define-key map (kbd "p") #'org-insight-previous-result)
    (define-key map (kbd "N") #'org-insight-next-file)
    (define-key map (kbd "P") #'org-insight-previous-file)
    (define-key map (kbd "RET") #'org-insight-visit)
    (define-key map (kbd "q") #'org-insight-quit)
    map))

(define-derived-mode org-insight-mode special-mode "OrgInsight"
  "Mode for org-insight results.
Move with n/p for matches, N/P for files. Point movement previews the
target line without visiting the file. RET visits the match (optional)."
  (setq buffer-read-only t)
  (hl-line-mode 1)
  (add-hook 'post-command-hook #'org-insight--maybe-preview nil t))

;; -----------------------------------------------------------------------------
;; Core helpers
;; -----------------------------------------------------------------------------

(defun org-insight--directory-files-recursively (dir)
  "Return list of files under DIR honoring `org-insight-ignore-file-predicate'."
  (let (out)
    (dolist (f (directory-files-recursively dir ".*" nil))
      (unless (funcall org-insight-ignore-file-predicate f)
        (push f out)))
    (nreverse out)))

(defun org-insight--collect-lines (file matcher-fn)
  "Return list of (org-insight-item ...) matches in FILE using MATCHER-FN(line)->bool."
  (let (acc)
    (when (file-regular-p file)
      (let ((tmp (generate-new-buffer " *org-insight-scan*")))
        (unwind-protect
            (with-current-buffer tmp
              (insert-file-contents file)
              (goto-char (point-min))
              (let ((ln 1))
                (while (not (eobp))
                  (let ((text (buffer-substring-no-properties
                               (line-beginning-position) (line-end-position))))
                    (when (funcall matcher-fn text)
                      (push (make-org-insight-item :file file :line ln :text text) acc)))
                  (forward-line 1)
                  (setq ln (1+ ln)))))
          (kill-buffer tmp))))
    (nreverse acc)))

(defun org-insight--org-file-title (file)
  "Return #+TITLE from Org FILE (case-insensitive), or nil if absent.
Reads up to first ~8KB for speed."
  (when (string-match-p "\\.org\\'" file)
    (with-temp-buffer
      (insert-file-contents file nil 0 8192)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (when (re-search-forward "^#\\+title:\\s-*\\(.+\\)$" nil t)
          (string-trim (match-string 1)))))))

(defun org-insight--default-display-name (file)
  "Return display name for FILE: Org #+TITLE, else file basename."
  (or (org-insight--org-file-title file)
      (file-name-nondirectory file)))

;; -----------------------------------------------------------------------------
;; Rendering
;; -----------------------------------------------------------------------------

(defun org-insight--render (file-groups keyword dir regexp-p)
  "Show grouped results in a fresh buffer.
FILE-GROUPS is a list of plists: (:file FILE :display NAME :items (org-insight-item ...))."
  (let* ((buf (get-buffer-create "*Org Insight*"))
         (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (org-insight-mode)
      (setq org-insight--groups file-groups
            org-insight--preview-keyword keyword
            org-insight--preview-regexp-p regexp-p
            org-insight--group-positions nil)
      (insert (propertize (format "org-insight: \"%s\" in %s  [%s]\n\n"
                                  keyword (abbreviate-file-name dir)
                                  (if regexp-p "regexp" "literal"))
                          'face '(:height 1.0 :weight bold)))
      (let (groups)
        (dolist (grp file-groups)
          (let* ((file (plist-get grp :file))
                 (display (plist-get grp :display))
                 (items (plist-get grp :items))
                 (hdr-start (point)))
            ;; Header line
            (insert (propertize (format "%s  (%d)\n" display (length items))
                                'face '(:weight bold :height 1.0)))
            (add-text-properties hdr-start (line-end-position)
                                 (list 'org-insight-category t
                                       'org-insight-category-name display
                                       'org-insight-category-file file))
            (push (cons file (copy-marker hdr-start t)) groups)
            ;; Entries
            (dolist (it items)
              (let ((start (point)))
                (insert (format "  %s:%d: %s\n"
                                (abbreviate-file-name file)
                                (org-insight-item-line it)
                                (org-insight-item-text it)))
                (add-text-properties
                 start (point)
                 (list 'org-insight-file file
                       'org-insight-line (org-insight-item-line it)
                       'mouse-face 'highlight
                       'help-echo file))))
            (insert "\n")))
        (setq org-insight--group-positions (nreverse groups)))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;; -----------------------------------------------------------------------------
;; Preview (whole file, recenter match in middle, not visiting)
;; -----------------------------------------------------------------------------

(defun org-insight--get-props-at-point ()
  "Return (FILE LINE) from text properties at point, or nil."
  (let ((file (get-text-property (line-beginning-position) 'org-insight-file))
        (line (get-text-property (line-beginning-position) 'org-insight-line)))
    (when (and file line) (list file line))))

(defun org-insight--maybe-preview ()
  "When point is on a result line, show preview without visiting file."
  (pcase (org-insight--get-props-at-point)
    (`(,file ,line)
     (let ((key (cons file line)))
       (unless (equal key org-insight--last-preview-key)
         (setq org-insight--last-preview-key key)
         (org-insight--show-preview file line org-insight--preview-keyword org-insight--preview-regexp-p))))
    (_ (setq org-insight--last-preview-key nil))))

(defun org-insight--show-preview (file line keyword regexp-p)
  "Show the whole FILE in *Org Insight Preview* and recenter LINE to middle.
Highlights LINE (background) and KEYWORD matches (distinct face)."
  (let* ((buf (get-buffer-create "*Org Insight Preview*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remove-overlays (point-min) (point-max) 'org-insight--temp t)
        (insert-file-contents file)
        ;; Highlight the target line
        (goto-char (point-min))
        (forward-line (1- line))
        (let* ((lb (line-beginning-position))
               (le (line-end-position))
               (line-ov (make-overlay lb le)))
          (overlay-put line-ov 'face '(:inherit highlight :weight bold))
          (overlay-put line-ov 'org-insight--temp t))
        ;; Highlight keyword occurrences
        (when (and keyword (not (string-empty-p keyword)))
          (let ((pattern (if regexp-p keyword (regexp-quote keyword))))
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward pattern nil t)
                (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                  (overlay-put ov 'face '(:foreground "orange" :weight bold))
                  (overlay-put ov 'org-insight--temp t))))))

        (view-mode 1)))
    ;; Display in side window and recenter
    (let ((win (display-buffer-in-side-window
                buf '((side . right) (slot . 0) (window-width . 0.45)))))
      (when (window-live-p win)
        (with-selected-window win
          (goto-char (point-min))
          (forward-line (1- line))
          (recenter '(middle)))))))

;; -----------------------------------------------------------------------------
;; Movement (results and file groups)
;; -----------------------------------------------------------------------------

(defun org-insight-next-result ()
  "Move point to the next search result line (skip headers/blank)."
  (interactive)
  (let* ((start (min (1+ (line-end-position)) (point-max)))
         (pos (text-property-not-all start (point-max) 'org-insight-line nil)))
    (if pos
        (progn (goto-char pos) (beginning-of-line))
      (message "No more results"))))

(defun org-insight-previous-result ()
  "Move point to the previous search result line (skip headers/blank)."
  (interactive)
  (let ((orig (point)) (found nil))
    (forward-line -1)
    (while (and (not found) (> (point) (point-min)))
      (if (get-text-property (line-beginning-position) 'org-insight-line)
          (setq found t)
        (forward-line -1)))
    (if found
        (beginning-of-line)
      (goto-char orig)
      (message "No previous results"))))

(defun org-insight--first-result-after-point ()
  "Move to the first result line at or after point; return point or nil."
  (let* ((start (line-beginning-position))
         (pos (text-property-not-all start (point-max) 'org-insight-line nil)))
    (when pos (goto-char pos) (beginning-of-line) pos)))

(defun org-insight--group-count () (length org-insight--group-positions))

(defun org-insight--current-group-index ()
  "Return index of the group that current point belongs to, or nil if before first."
  (let* ((pos (point))
         (idx nil)
         (i 0))
    (dolist (g org-insight--group-positions idx)
      (when (<= (marker-position (cdr g)) pos)
        (setq idx i))
      (setq i (1+ i)))))

(defun org-insight--goto-group-index (i)
  "Go to group I (0-based) and land on its first result line."
  (let ((n (org-insight--group-count)))
    (cond
     ((or (null i) (< i 0) (>= i n))
      (message "No such file group"))
     (t
      (let* ((g (nth i org-insight--group-positions))
             (hdr (marker-position (cdr g))))
        (goto-char hdr)
        (forward-line 1)
        (or (org-insight--first-result-after-point)
            (goto-char hdr)))))))

(defun org-insight-next-file ()
  "Jump to the next file/category group; land on its first result."
  (interactive)
  (let* ((cur (org-insight--current-group-index))
         (next (if (null cur) 0 (1+ cur))))
    (if (>= next (org-insight--group-count))
        (message "No more file groups")
      (org-insight--goto-group-index next))))

(defun org-insight-previous-file ()
  "Jump to the previous file/category group; land on its first result."
  (interactive)
  (let ((cur (org-insight--current-group-index)))
    (cond
     ((or (null cur) (= cur 0))
      (message "No previous file group"))
     (t (org-insight--goto-group-index (1- cur))))))

;; -----------------------------------------------------------------------------
;; Actions
;; -----------------------------------------------------------------------------

(defun org-insight-visit ()
  "Visit the file/line at point (opens real file buffer)."
  (interactive)
  (pcase (org-insight--get-props-at-point)
    (`(,file ,line)
     (find-file file)
     (goto-char (point-min))
     (forward-line (1- line))
     (recenter))))

(defun org-insight-quit ()
  "Quit the org-insight results window and close the preview window if present.
Keeps the preview buffer around; only removes its window."
  (interactive)
  (when-let* ((pb (get-buffer "*Org Insight Preview*"))
              (pw (get-buffer-window pb t)))
    (when (window-live-p pw)
      (ignore-errors (delete-window pw))))
  (quit-window))

;; -----------------------------------------------------------------------------
;; Entry points
;; -----------------------------------------------------------------------------

;;;###autoload
(defun org-insight (dir keyword &optional display-fn)
  "Search DIR recursively for lines containing KEYWORD and show grouped results.
KEYWORD is treated as a regexp when `org-insight-use-regexp' is non-nil,
otherwise matched literally.

DISPLAY-FN is (lambda FILE -> DISPLAY-NAME). If nil, defaults to
`org-insight--default-display-name'."
  (interactive "DDirectory: \nsKeyword: ")
  (let* ((regexp-p org-insight-use-regexp)
         (match-fn (if regexp-p
                       (lambda (line) (ignore-errors (string-match-p keyword line)))
                     (let ((needle (regexp-quote keyword)))
                       (lambda (line) (string-match-p needle line)))))
         (disp-fn (or display-fn #'org-insight--default-display-name))
         (files (org-insight--directory-files-recursively (expand-file-name dir)))
         ;; Build table keyed by FILE → (DISPLAY . ITEMS)
         (table (make-hash-table :test 'equal)))
    (dolist (f files)
      (let ((items (org-insight--collect-lines f match-fn)))
        (when items
          (let* ((bucket (gethash f table))
                 (disp (or (car-safe bucket) (funcall disp-fn f)))
                 (old  (cdr-safe bucket)))
            (puthash f (cons disp (append old items)) table)))))
    ;; Convert to plist list
    (let (alist)
      (maphash (lambda (file pair)
                 (push (list :file file :display (car pair) :items (cdr pair)) alist))
               table)
      ;; Sort by display name then file path for stability
      (setq alist (sort alist
                        (lambda (a b)
                          (let ((da (plist-get a :display))
                                (db (plist-get b :display)))
                            (if (string= da db)
                                (string< (plist-get a :file) (plist-get b :file))
                              (string< da db))))))
      (org-insight--render alist keyword dir regexp-p))))

;;;###autoload
(defun org-insight-in-default-directory (keyword)
  "Search `org-insight-default-directory' recursively for KEYWORD.
Prompts only for the keyword; the directory is taken from `org-insight-default-directory'."
  (interactive "sKeyword: ")
  (unless org-insight-default-directory
    (user-error "Please set `org-insight-default-directory' first"))
  (org-insight org-insight-default-directory keyword))

(provide 'org-insight)
;;; org-insight.el ends here
