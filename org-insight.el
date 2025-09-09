;;; org-insight.el --- Grouped recursive search with live preview (debounced minibuffer) -*- lexical-binding: t; -*-
;;
;; Author: Your Name <you@example.com>
;; Maintainer: Your Name <you@example.com>
;; Version: 1.8
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, search, convenience
;; URL: https://example.com/org-insight
;;
;;; Commentary:
;;
;; - Grouped results by file (uses Org #+TITLE when present; otherwise filename)
;; - Results buffer: n/p (matches), N/P (files), RET (visit), q (quit & close preview)
;; - Whole-file preview (non-visiting) on the right; match line highlighted; keywords colored
;; - Multiple keywords (space/comma-separated); AND/OR toggle with C-c C-o
;; - Backends: ripgrep, GNU grep, or pure Elisp
;; - Live preview while typing in the minibuffer:
;;   * The preview window/frame is created BEFORE the minibuffer prompt.
;;   * During typing we only UPDATE the existing preview buffer (no new windows).
;;   * Updates are **debounced** via `run-with-idle-timer` (default 1.0s idle).
;;
;;; License: MIT
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
  "If non-nil, treat input keyword(s) as regexp; otherwise match literally."
  :type 'boolean :group 'org-insight)

(defcustom org-insight-default-directory nil
  "Default directory used by `org-insight'.
If nil, you will be prompted for a directory."
  :type 'directory :group 'org-insight)

(defcustom org-insight-ignore-file-predicate
  (lambda (path)
    "Return non-nil to skip PATH. Default ignores VCS dirs and common binaries."
    (let ((case-fold-search t))
      (or (string-match-p "/\\.\\(git\\|hg\\|svn\\)/" path)
          (string-match-p "\\.\\(png\\|jpg\\|jpeg\\|gif\\|bmp\\|pdf\\|zip\\|tar\\|gz\\|7z\\|exe\\|dll\\)$" path))))
  "Predicate called with absolute PATH; return non-nil to ignore it."
  :type 'function :group 'org-insight)

(defcustom org-insight-backend
  (if (executable-find "rg") 'ripgrep 'emacs)
  "Backend to use for the final search: 'ripgrep, 'grep, or 'emacs."
  :type '(choice (const :tag "ripgrep (rg)" ripgrep)
                 (const :tag "GNU grep" grep)
                 (const :tag "Built-in (Elisp)" emacs))
  :group 'org-insight)

(defcustom org-insight-ripgrep-extra-args '("--hidden" "--no-heading" "--color=never")
  "Extra args for ripgrep. `org-insight' always adds --line-number and --with-filename."
  :type '(repeat string) :group 'org-insight)

(defcustom org-insight-grep-extra-args '("-R" "-n" "-H" "-I")
  "Extra args for grep. Adds -E for regexp or -F for literal automatically."
  :type '(repeat string) :group 'org-insight)

(defcustom org-insight-default-operator 'or
  "Default boolean operator for multiple keywords: 'or or 'and."
  :type '(choice (const :tag "OR" or) (const :tag "AND" and))
  :group 'org-insight)

(defcustom org-insight-crm-separator "[ \t\n\r,]+"
  "Regexp separator for splitting multiple keywords (spaces/commas by default)."
  :type 'string :group 'org-insight)

;; Live preview options ---------------------------------------------------------

(defcustom org-insight-live-preview t
  "If non-nil, show a live preview during minibuffer input."
  :type 'boolean :group 'org-insight)

(defcustom org-insight-live-preview-backend 'emacs
  "Backend used while typing for live preview (recommended: 'emacs)."
  :type '(choice (const :tag "Built-in (Elisp)" emacs)
                 (const :tag "ripgrep (rg)" ripgrep)
                 (const :tag "GNU grep" grep))
  :group 'org-insight)

(defcustom org-insight-live-preview-debounce 1.0
  "Seconds of idle time before the live preview updates in the minibuffer.
Set to a larger value for slower disks or very large trees."
  :type 'number :group 'org-insight)

(defcustom org-insight-live-preview-min-chars 1
  "Minimal input length to start live preview."
  :type 'integer :group 'org-insight)

(defcustom org-insight-live-preview-max-files 10
  "Max number of file groups to display in the live preview."
  :type 'integer :group 'org-insight)

(defcustom org-insight-live-preview-max-lines-per-file 3
  "Max number of matching lines per file to display in the live preview."
  :type 'integer :group 'org-insight)

(defcustom org-insight-live-preview-close-on-exit t
  "Close the live preview window/frame when the minibuffer exits."
  :type 'boolean :group 'org-insight)

(defcustom org-insight-live-preview-side 'bottom
  "Where to show the live preview side window."
  :type '(choice (const bottom) (const right)) :group 'org-insight)

(defcustom org-insight-live-preview-display 'side
  "How to show the live preview: 'side (side window) or 'frame (small frame)."
  :type '(choice (const side) (const frame))
  :group 'org-insight)

(defface org-insight-live-header-face
  '((t :inherit shadow :height 0.95))
  "Face for the live preview header line."
  :group 'org-insight)

;; History for keyword prompts
(defvar org-insight-keyword-history nil
  "Input history for org-insight keyword prompts.")

;; -----------------------------------------------------------------------------
;; Data structures & buffer-locals
;; -----------------------------------------------------------------------------

(cl-defstruct org-insight-item file line text)

(defvar-local org-insight--groups nil)
(defvar-local org-insight--group-positions nil)
(defvar-local org-insight--preview-keywords nil)   ;; list of strings
(defvar-local org-insight--preview-regexp-p nil)
(defvar-local org-insight--last-preview-key nil)

(defvar org-insight--live-buffer-name "*Org Insight Live*")
(defvar org-insight--live-frame nil)               ;; only when display='frame

;; Minibuffer locals for live update
(defvar-local org-insight--entry-operator org-insight-default-operator)
(defvar-local org-insight--live-kick-fn nil)
(defvar-local org-insight--live-idle-timer nil
  "Idle timer used to debounce live preview while typing in the minibuffer.")
(defvar-local org-insight--live-schedule-fn nil
  "Buffer-local closure used to debounce live preview in the minibuffer.")

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
  "Results for org-insight. n/p: matches, N/P: files. RET: visit. q: quit & close preview."
  (setq buffer-read-only t)
  (hl-line-mode 1)
  (add-hook 'post-command-hook #'org-insight--maybe-preview nil t)
  (add-hook 'kill-buffer-hook #'org-insight--close-preview nil t))

;; -----------------------------------------------------------------------------
;; Helpers: keywords, files, operator toggle
;; -----------------------------------------------------------------------------

(defun org-insight--split-keywords (input)
  "Split INPUT into non-empty keywords. Splits on commas/whitespace."
  (let* ((trimmed (string-trim input)))
    (and (not (string-empty-p trimmed))
         (split-string trimmed org-insight-crm-separator t))))

(defun org-insight--directory-files-recursively (dir)
  "Return list of files under DIR, honoring `org-insight-ignore-file-predicate'."
  (let (out)
    (dolist (f (directory-files-recursively dir ".*" nil))
      (unless (funcall org-insight-ignore-file-predicate f)
        (push f out)))
    (nreverse out)))

(defun org-insight--operator-label (&optional op)
  (if (eq (or op org-insight--entry-operator) 'and) "AND" "OR"))

(defun org-insight--toggle-operator-during-entry ()
  "Toggle AND/OR during keyword entry; force a debounced live refresh."
  (interactive)
  (setq org-insight--entry-operator
        (if (eq org-insight--entry-operator 'and) 'or 'and))
  (when (minibufferp)
    (minibuffer-message " Operator: %s" (org-insight--operator-label))
    (when (functionp org-insight--live-kick-fn)
      (funcall org-insight--live-kick-fn))))

;; -----------------------------------------------------------------------------
;; Backends (single keyword helpers + collectors)
;; -----------------------------------------------------------------------------

(defun org-insight--collect-lines-elisp (files matcher-fn)
  "Collect matches across FILES using MATCHER-FN(line)->bool (pure Elisp)."
  (let (items)
    (dolist (file files)
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
                        (push (make-org-insight-item :file file :line ln :text text) items)))
                    (forward-line 1)
                    (setq ln (1+ ln)))))
            (kill-buffer tmp)))))
    (nreverse items)))

(defun org-insight--parse-lno-line (s)
  "Parse \"FILE:LINE:TEXT\" robustly (supports Windows drive letters)."
  (cond
   ;; C:\path\to\file:123:the line...
   ((string-match "^\\([a-zA-Z]:[^:\n]*\\):\\([0-9]+\\):\\(.*\\)$" s)
    (list (match-string 1 s)
          (string-to-number (match-string 2 s))
          (match-string 3 s)))
   ;; /path/to/file:123:the line...
   ((string-match "^\\([^:\n]+\\):\\([0-9]+\\):\\(.*\\)$" s)
    (list (match-string 1 s)
          (string-to-number (match-string 2 s))
          (match-string 3 s)))
   (t nil)))

(defun org-insight--call-process-lines (program args dir)
  "Run PROGRAM with ARGS in DIR, return list of output lines."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (with-temp-buffer
      (let ((status (apply #'process-file program nil (current-buffer) nil args)))
        (cond
         ((or (eq status 0) (eq status 1))
          (split-string (buffer-string) "\n" t))
         (t (error "%s failed with exit code %s\n%s" program status (buffer-string))))))))

(defun org-insight--collect-lines-ripgrep (dir keyword regexp-p)
  (unless (executable-find "rg")
    (error "ripgrep (rg) not found; set backend to 'grep or 'emacs"))
  (let* ((args (append org-insight-ripgrep-extra-args
                       '("--with-filename" "--line-number")
                       (unless regexp-p '("-F"))
                       (list "--" keyword ".")))
         (lines (org-insight--call-process-lines "rg" args dir))
         items)
    (dolist (ln lines)
      (pcase (org-insight--parse-lno-line ln)
        (`(,file ,lno ,text)
         (let ((abs (expand-file-name file dir)))
           (unless (funcall org-insight-ignore-file-predicate abs)
             (push (make-org-insight-item :file abs :line lno :text text) items))))))
    (nreverse items)))

(defun org-insight--collect-lines-grep (dir keyword regexp-p)
  (unless (executable-find "grep")
    (error "grep not found; set backend to 'ripgrep or 'emacs"))
  (let* ((mode-flag (if regexp-p "-E" "-F"))
         (args (append org-insight-grep-extra-args (list mode-flag "--" keyword ".")))
         (lines (org-insight--call-process-lines "grep" args dir))
         items)
    (dolist (ln lines)
      (pcase (org-insight--parse-lno-line ln)
        (`(,file ,lno ,text)
         (let ((abs (expand-file-name file dir)))
           (unless (funcall org-insight-ignore-file-predicate abs)
             (push (make-org-insight-item :file abs :line lno :text text) items))))))
    (nreverse items)))

(defun org-insight--collect-one (backend dir keyword regexp-p)
  "Collect items for a SINGLE KEYWORD using BACKEND in DIR."
  (pcase backend
    ('ripgrep (org-insight--collect-lines-ripgrep dir keyword regexp-p))
    ('grep    (org-insight--collect-lines-grep    dir keyword regexp-p))
    ('emacs   (let* ((files (org-insight--directory-files-recursively (expand-file-name dir)))
                     (match-fn (if regexp-p
                                   (lambda (line) (ignore-errors (string-match-p keyword line)))
                                 (let ((needle (regexp-quote keyword)))
                                   (lambda (line) (string-match-p needle line))))))
                (org-insight--collect-lines-elisp files match-fn)))
    (_ (user-error "Unknown `org-insight-backend': %S" backend))))

;; -----------------------------------------------------------------------------
;; Combining items (AND/OR)
;; -----------------------------------------------------------------------------

(defun org-insight--key (item)
  (format "%s:%d" (org-insight-item-file item) (org-insight-item-line item)))

(defun org-insight--combine-items (lists operator)
  "Combine LISTS of items by per-line OPERATOR ('or or 'and)."
  (let ((n (length lists)))
    (pcase operator
      ('or
       (let ((seen (make-hash-table :test 'equal)) out)
         (dolist (lst lists)
           (dolist (it lst)
             (let ((k (org-insight--key it)))
               (unless (gethash k seen)
                 (puthash k it seen)
                 (push it out)))))
         (nreverse out)))
      ('and
       (let ((count (make-hash-table :test 'equal))
             (store (make-hash-table :test 'equal)))
         (dolist (lst lists)
           (let ((seen-one (make-hash-table :test 'equal)))
             (dolist (it lst)
               (let ((k (org-insight--key it)))
                 (unless (gethash k seen-one)
                   (puthash k t seen-one)
                   (puthash k it store)
                   (puthash k (1+ (gethash k count 0)) count))))))
         (let (out)
           (maphash (lambda (k c)
                      (when (= c n) (push (gethash k store) out)))
                    count)
           (setq out (sort out
                           (lambda (a b)
                             (let ((fa (org-insight-item-file a))
                                   (fb (org-insight-item-file b))
                                   (la (org-insight-item-line a))
                                   (lb (org-insight-item-line b)))
                               (if (string= fa fb) (< la lb) (string< fa fb))))))
           out)))
      (_ (user-error "Unknown operator: %S" operator)))))

;; -----------------------------------------------------------------------------
;; Org helpers & rendering (main results)
;; -----------------------------------------------------------------------------

(defun org-insight--org-file-title (file)
  "Return #+TITLE from Org FILE (case-insensitive), or nil."
  (when (string-match-p "\\.org\\'" file)
    (with-temp-buffer
      (insert-file-contents file nil 0 8192)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (when (re-search-forward "^#\\+title:\\s-*\\(.+\\)$" nil t)
          (string-trim (match-string 1)))))))

(defun org-insight--default-display-name (file)
  (or (org-insight--org-file-title file)
      (file-name-nondirectory file)))

(defun org-insight--group-items-by-file (items display-fn)
  "Return an alist of plist groups (:file :display :items) from ITEMS."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (it items)
      (let* ((file (org-insight-item-file it))
             (bucket (gethash file table))
             (disp (or (car-safe bucket) (funcall display-fn file)))
             (old  (cdr-safe bucket)))
        (puthash file (cons disp (append old (list it))) table)))
    (let (alist)
      (maphash (lambda (file pair)
                 (push (list :file file :display (car pair) :items (cdr pair)) alist))
               table)
      (sort alist
            (lambda (a b)
              (let ((da (plist-get a :display))
                    (db (plist-get b :display)))
                (if (string= da db)
                    (string< (plist-get a :file) (plist-get b :file))
                  (string< da db))))))))

(defun org-insight--render (file-groups header-text dir regexp-p keywords)
  "Show grouped results. KEYWORDS is a list for preview highlighting."
  (let* ((buf (get-buffer-create "*Org Insight*"))
         (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (org-insight-mode)
      (setq org-insight--groups file-groups
            org-insight--preview-keywords keywords
            org-insight--preview-regexp-p regexp-p
            org-insight--group-positions nil)
      (insert (propertize (format "org-insight: \"%s\" in %s  [%s]\n\n"
                                  header-text (abbreviate-file-name dir)
                                  (if regexp-p "regexp" "literal"))
                          'face '(:height 1.0 :weight bold)))
      (let (groups)
        (dolist (grp file-groups)
          (let* ((file (plist-get grp :file))
                 (display (plist-get grp :display))
                 (items   (plist-get grp :items))
                 (hdr-start (point)))
            (insert (propertize (format "%s  (%d)\n" display (length items))
                                'face '(:weight bold :height 1.0)))
            (add-text-properties hdr-start (line-end-position)
                                 (list 'org-insight-category t
                                       'org-insight-category-name display
                                       'org-insight-category-file file))
            (push (cons file (copy-marker hdr-start t)) groups)
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
;; Whole-file line preview (for selected entry)
;; -----------------------------------------------------------------------------

(defun org-insight--get-props-at-point ()
  (let ((file (get-text-property (line-beginning-position) 'org-insight-file))
        (line (get-text-property (line-beginning-position) 'org-insight-line)))
    (when (and file line) (list file line))))

(defun org-insight--maybe-preview ()
  (pcase (org-insight--get-props-at-point)
    (`(,file ,line)
     (let ((key (cons file line)))
       (unless (equal key org-insight--last-preview-key)
         (setq org-insight--last-preview-key key)
         (org-insight--show-preview file line org-insight--preview-keywords org-insight--preview-regexp-p))))
    (_ (setq org-insight--last-preview-key nil))))

(defun org-insight--show-preview (file line keywords regexp-p)
  (let* ((buf (get-buffer-create "*Org Insight Preview*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remove-overlays (point-min) (point-max) 'org-insight--temp t)
        (insert-file-contents file)
        (goto-char (point-min))
        (forward-line (1- line))
        (let* ((lb (line-beginning-position))
               (le (line-end-position))
               (line-ov (make-overlay lb le)))
          (overlay-put line-ov 'face '(:inherit highlight :weight bold))
          (overlay-put line-ov 'org-insight--temp t))
        (when (and keywords (listp keywords))
          (save-excursion
            (dolist (kw keywords)
              (goto-char (point-min))
              (let ((pattern (if regexp-p kw (regexp-quote kw))))
                (while (re-search-forward pattern nil t)
                  (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                    (overlay-put ov 'face '(:foreground "orange" :weight bold))
                    (overlay-put ov 'org-insight--temp t)))))))
        (view-mode 1)))
    (let ((win (display-buffer-in-side-window
                buf '((side . right) (slot . 0) (window-width . 0.45)))))
      (when (window-live-p win)
        (with-selected-window win
          (goto-char (point-min))
          (forward-line (1- line))
          (recenter '(middle)))))))

;; -----------------------------------------------------------------------------
;; Movement (results & file groups)
;; -----------------------------------------------------------------------------

(defun org-insight-next-result () (interactive)
       (let* ((start (min (1+ (line-end-position)) (point-max)))
              (pos (text-property-not-all start (point-max) 'org-insight-line nil)))
         (if pos (goto-char (progn (goto-char pos) (line-beginning-position)))
           (message "No more results"))))

(defun org-insight-previous-result () (interactive)
       (let ((orig (point)) (found nil))
         (forward-line -1)
         (while (and (not found) (> (point) (point-min)))
           (if (get-text-property (line-beginning-position) 'org-insight-line)
               (setq found t)
             (forward-line -1)))
         (if found (beginning-of-line) (goto-char orig) (message "No previous results"))))

(defun org-insight--first-result-after-point ()
  (let* ((start (line-beginning-position))
         (pos (text-property-not-all start (point-max) 'org-insight-line nil)))
    (when pos (goto-char pos) (beginning-of-line) pos)))

(defun org-insight--group-count () (length org-insight--group-positions))

(defun org-insight--current-group-index ()
  (let* ((pos (point)) (idx nil) (i 0))
    (dolist (g org-insight--group-positions idx)
      (when (<= (marker-position (cdr g)) pos) (setq idx i))
      (setq i (1+ i)))))

(defun org-insight--goto-group-index (i)
  (let ((n (org-insight--group-count)))
    (cond
     ((or (null i) (< i 0) (>= i n)) (message "No such file group"))
     (t
      (let* ((g (nth i org-insight--group-positions))
             (hdr (marker-position (cdr g))))
        (goto-char hdr)
        (forward-line 1)
        (or (org-insight--first-result-after-point) (goto-char hdr)))))))

(defun org-insight-next-file () (interactive)
       (let* ((cur (org-insight--current-group-index))
              (next (if (null cur) 0 (1+ cur))))
         (if (>= next (org-insight--group-count))
             (message "No more file groups")
           (org-insight--goto-group-index next))))

(defun org-insight-previous-file () (interactive)
       (let ((cur (org-insight--current-group-index)))
         (if (or (null cur) (= cur 0))
             (message "No previous file group")
           (org-insight--goto-group-index (1- cur)))))

;; -----------------------------------------------------------------------------
;; Actions
;; -----------------------------------------------------------------------------

(defun org-insight-visit () (interactive)
       (pcase (org-insight--get-props-at-point)
         (`(,file ,line)
          (find-file file)
          (goto-char (point-min))
          (forward-line (1- line))
          (recenter))))

(defun org-insight-quit () (interactive)
       (org-insight--close-preview)
       (org-insight--close-live)
       (quit-window))

;; -----------------------------------------------------------------------------
;; Core runner
;; -----------------------------------------------------------------------------

(defun org-insight--run (dir keywords operator regexp-p display-fn)
  "Execute search and render results."
  (let* ((backend org-insight-backend)
         (lists (mapcar (lambda (kw)
                          (org-insight--collect-one backend dir kw regexp-p))
                        keywords))
         (items (org-insight--combine-items lists operator))
         (groups (org-insight--group-items-by-file items display-fn))
         (header-str (if (= (length keywords) 1)
                         (car keywords)
                       (mapconcat #'identity
                                  keywords
                                  (if (eq operator 'and) " & " " | ")))))
    (org-insight--render groups header-str dir regexp-p keywords)))

;; -----------------------------------------------------------------------------
;; Live preview (render buffer only) and setup helpers
;; -----------------------------------------------------------------------------

(defun org-insight--render-live-buffer (groups header dir)
  "Update the contents of the live preview buffer ONLY (no window ops)."
  (let* ((buf (get-buffer-create org-insight--live-buffer-name))
         (inhibit-read-only t))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (propertize
               (format "org-insight live: \"%s\" in %s  [%s]\n\n"
                       (if (and header (not (string-empty-p header))) header "—")
                       (abbreviate-file-name dir)
                       (if org-insight-use-regexp "regexp" "literal"))
               'face 'org-insight-live-header-face))
      (let ((files 0))
        (dolist (grp groups)
          (when (< files org-insight-live-preview-max-files)
            (setq files (1+ files))
            (let* ((file (plist-get grp :file))
                   (display (plist-get grp :display))
                   (items   (plist-get grp :items))
                   (take    (cl-subseq items 0 (min (length items)
                                                    org-insight-live-preview-max-lines-per-file))))
              (insert (propertize (format "%s  (%d)\n" display (length items))
                                  'face '(:weight bold)))
              (dolist (it take)
                (insert (format "  %s:%d: %s\n"
                                (abbreviate-file-name file)
                                (org-insight-item-line it)
                                (org-insight-item-text it))))
              (when (< (length take) (length items))
                (insert (format "  ... and %d more lines\n"
                                (- (length items) (length take)))))
              (insert "\n"))))
        (setq buffer-read-only t)))))

(defun org-insight--ensure-live-visible ()
  "Ensure the live preview buffer is visible BEFORE minibuffer starts."
  (let ((buf (get-buffer-create org-insight--live-buffer-name)))
    (pcase org-insight-live-preview-display
      ('side
       (or (get-buffer-window buf t)
           (display-buffer-in-side-window
            buf (if (eq org-insight-live-preview-side 'right)
                    '((side . right) (slot . 1) (window-width . 0.4))
                  '((side . bottom) (slot . 1) (window-height . 0.33))))))
      ('frame
       (unless (and (frame-live-p org-insight--live-frame)
                    (get-buffer-window buf org-insight--live-frame))
         (setq org-insight--live-frame
               (make-frame '((name . "Org Insight Live")
                             (minibuffer . nil) (width . 100) (height . 18)
                             (unsplittable . t) (visibility . t)
                             (auto-raise . nil) (auto-lower . nil))))
         (with-selected-frame org-insight--live-frame
           (display-buffer buf))))
      ;; Initialize with an empty header so users see it's on
      (org-insight--render-live-buffer nil "—" (abbreviate-file-name default-directory)))))

(defun org-insight--close-live ()
  "Close the live preview window/frame (if any)."
  (when-let* ((b (get-buffer org-insight--live-buffer-name))
              (w (get-buffer-window b t)))
    (when (window-live-p w) (ignore-errors (delete-window w))))
  (when (and (frame-live-p org-insight--live-frame)
             (equal (frame-parameter org-insight--live-frame 'name) "Org Insight Live"))
    (ignore-errors (delete-frame org-insight--live-frame))
    (setq org-insight--live-frame nil)))

(defun org-insight--close-preview ()
  "Close the results preview side window (\"*Org Insight Preview*\") if visible."
  (when-let* ((buf (get-buffer "*Org Insight Preview*"))
              (win (get-buffer-window buf t)))
    (when (window-live-p win)
      (ignore-errors (delete-window win)))))

;; -----------------------------------------------------------------------------
;; Interactive entry (minibuffer; debounced via buffer-local closure)
;; -----------------------------------------------------------------------------

;;;###autoload
(defun org-insight (&optional display-fn)
  "Search recursively with grouped results and whole-file preview.

- Type keywords separated by spaces/commas.
- Toggle AND/OR with C-c C-o (message shows current operator).
- Live preview updates after an idle delay; the preview window is created *before* prompting.

Uses `org-insight-default-directory` if set; otherwise prompts for a directory.

Optional DISPLAY-FN is (lambda FILE -> DISPLAY-NAME)."
  (interactive)
  (let* ((dir (or org-insight-default-directory
                  (read-directory-name "Directory: ")))
         (regexp-p org-insight-use-regexp)
         (disp-fn (or display-fn #'org-insight--default-display-name))
         (local-map (let ((m (make-sparse-keymap)))
                      (set-keymap-parent m minibuffer-local-map)
                      (define-key m (kbd "C-c C-o") #'org-insight--toggle-operator-during-entry)
                      m))
         ;; Pre-create the live preview view (window or frame) in the chosen dir
         (_pre (when org-insight-live-preview
                 (let ((default-directory dir))
                   (org-insight--ensure-live-visible))))
         (prompt "Keywords: "))
    (let* ((input (unwind-protect (minibuffer-with-setup-hook
                                      (lambda ()
                                        (use-local-map local-map)
                                        (setq-local org-insight--entry-operator org-insight-default-operator)
                                        (when org-insight-live-preview
                                          (let* ((mb (current-buffer)))
                                            ;; Called by the idle timer to compute & render preview NOW.
                                            (cl-labels
                                                ((refresh-now ()
                                                   (when (buffer-live-p mb)
                                                     (with-current-buffer mb
                                                       (let* ((s (buffer-substring-no-properties
                                                                  (minibuffer-prompt-end) (point-max)))
                                                              (trim (string-trim s)))
                                                         (if (< (length trim) org-insight-live-preview-min-chars)
                                                             (org-insight--render-live-buffer nil "—" dir)
                                                           (let* ((kws (org-insight--split-keywords trim)))
                                                             (if (null kws)
                                                                 (org-insight--render-live-buffer nil "—" dir)
                                                               (let* ((op (if (> (length kws) 1)
                                                                              org-insight--entry-operator
                                                                            org-insight-default-operator))
                                                                      (backend org-insight-live-preview-backend)
                                                                      (lists (mapcar (lambda (kw)
                                                                                       (org-insight--collect-one backend dir kw regexp-p))
                                                                                     kws))
                                                                      (items  (org-insight--combine-items lists op))
                                                                      (groups (org-insight--group-items-by-file
                                                                               items (or disp-fn #'org-insight--default-display-name)))
                                                                      (header (if (= (length kws) 1)
                                                                                  (car kws)
                                                                                (mapconcat #'identity
                                                                                           kws
                                                                                           (if (eq op 'and) " & " " | ")))))
                                                                 (org-insight--render-live-buffer groups header dir))))))))))
                                              ;; A closure we can safely add/remove from hooks:
                                              (setq-local org-insight--live-schedule-fn
                                                          (lambda (&rest _)
                                                            (when (timerp org-insight--live-idle-timer)
                                                              (cancel-timer org-insight--live-idle-timer))
                                                            (setq org-insight--live-idle-timer
                                                                  (run-with-idle-timer
                                                                   org-insight-live-preview-debounce
                                                                   nil
                                                                   (lambda ()
                                                                     (when (buffer-live-p mb)
                                                                       (with-current-buffer mb
                                                                         (refresh-now))))))))
                                              ;; Expose manual kick for C-c C-o
                                              (setq-local org-insight--live-kick-fn
                                                          (lambda () (funcall org-insight--live-schedule-fn)))
                                              ;; On any edit, just reschedule the idle timer.
                                              (add-hook 'post-self-insert-hook org-insight--live-schedule-fn nil t)
                                              (add-hook 'post-command-hook      org-insight--live-schedule-fn nil t)
                                              ;; Cleanup on exit: remove hooks and cancel timer.
                                              (let (cleanup)
                                                (setq cleanup
                                                      (lambda ()
                                                        (when org-insight-live-preview-close-on-exit
                                                          (org-insight--close-live))
                                                        (when (timerp org-insight--live-idle-timer)
                                                          (cancel-timer org-insight--live-idle-timer))
                                                        (remove-hook 'post-self-insert-hook org-insight--live-schedule-fn t)
                                                        (remove-hook 'post-command-hook      org-insight--live-schedule-fn t)
                                                        (setq org-insight--live-idle-timer nil
                                                              org-insight--live-schedule-fn nil
                                                              org-insight--live-kick-fn nil)
                                                        (remove-hook 'minibuffer-exit-hook cleanup)))
                                                (add-hook 'minibuffer-exit-hook cleanup))))))
                                    (read-from-minibuffer prompt
                                                          nil nil nil 'org-insight-keyword-history))
                    (org-insight--close-live)))
           (keywords (or (org-insight--split-keywords input)
                         (user-error "No keywords provided")))
           (operator (if (> (length keywords) 1)
                         org-insight--entry-operator
                       org-insight-default-operator)))
      (org-insight--run dir keywords operator regexp-p disp-fn))))

(provide 'org-insight)
;;; org-insight.el ends here
