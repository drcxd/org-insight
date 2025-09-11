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

(defcustom org-insight-min-input-chars 2
  "Minimal input length before any search or live preview occurs.
Increasing this helps keep minibuffer typing responsive."
  :type 'integer :group 'org-insight)

(define-obsolete-variable-alias
  'org-insight-live-preview-min-chars 'org-insight-min-input-chars "0.2")

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
  "How to show the live preview:
- 'side     : show in a side window (right/bottom, depending on `org-insight-live-preview-side`)
- 'replace  : replace the current window (restore it on minibuffer exit)
- 'frame    : show in a small separate frame."
  :type '(choice
          (const :tag "Side window" side)
          (const :tag "Replace current window" replace)
          (const :tag "Own frame" frame))
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
(defvar org-insight--live-replaced-window nil
  "Window that was replaced by the live preview in 'replace mode.")
(defvar org-insight--live-replaced-buffer nil
  "Buffer previously shown in the replaced window.")
(defvar org-insight--live-replaced-point nil
  "Point in the previously shown buffer when it was replaced.")

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
  "Collect matches using ripgrep in DIR for KEYWORD.
Respects smart-case and avoids truncating long lines."
  (unless (executable-find "rg")
    (error "ripgrep (rg) not found; set backend to 'grep or 'emacs"))
  (let* ((args (append org-insight-ripgrep-extra-args
                       ;; Always ask for file, line; never truncate long lines; smart-case.
                       '("--with-filename" "--line-number" "--max-columns=0" "-S")
                       ;; Use fixed string unless regexp requested.
                       (unless regexp-p '("-F"))
                       ;; Pattern and search root.
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
  "Collect matches using GNU grep in DIR for KEYWORD.
Honors `case-fold-search' for case-insensitive search."
  (unless (executable-find "grep")
    (error "grep not found; set backend to 'ripgrep or 'emacs"))
  (let* ((mode-flag (if regexp-p "-E" "-F"))
         ;; If the user typed all-lowercase and Emacs would fold case, use -i.
         (need-ci (and case-fold-search
                       (string= keyword (downcase keyword))))
         (args (append org-insight-grep-extra-args
                       (list mode-flag)
                       (when need-ci '("-i"))
                       (list "--" keyword ".")))
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
         ;; Ensure deterministic order: by file, then line.
         (setq out (sort out
                         (lambda (a b)
                           (let ((fa (org-insight-item-file a))
                                 (fb (org-insight-item-file b))
                                 (la (org-insight-item-line a))
                                 (lb (org-insight-item-line b)))
                             (if (string= fa fb) (< la lb) (string< fa fb))))))
         out))
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

;;;; ----------------------------------------------------------------------
;;;; Vertico integration
;;;; ----------------------------------------------------------------------

(defvar org-insight--calling-window nil
  "The window that was selected when the Vertico session started.")

(defun org-insight--visit-file-line-in-window (file line window)
  "Show FILE and move to LINE in WINDOW, ignoring display rules."
  (let ((buf (find-file-noselect file)))
    (when (window-live-p window)
      (select-window window))
    (set-window-buffer window buf)
    (with-selected-window window
      (goto-char (point-min))
      (forward-line (max 0 (1- line)))
      (recenter))))

(defvar org-insight--cand-map nil
  "Hash table mapping displayed candidate text to (FILE . LINE) for the active session.")

(defun org-insight--unwrap-candidate (cand)
  "Strip the artificial prefix \"… ⇨ \" from CAND, returning the displayed text."
  (if (string-match "\\`.*?⇨ \\(.*\\)\\'" cand)
      (match-string 1 cand)
    cand))

(defgroup org-insight-vertico nil
  "Vertico-based UI for org-insight."
  :group 'org-insight)

(defcustom org-insight-use-vertico t
  "If non-nil and Vertico is available, use Vertico-based UI for `org-insight`."
  :type 'boolean :group 'org-insight-vertico)

(defcustom org-insight-vertico-preview-side 'right
  "Which side to show the live preview window."
  :type '(choice (const left) (const right) (const above) (const below))
  :group 'org-insight-vertico)

(defcustom org-insight-vertico-preview-width 0.5
  "Width/height (fraction) of the preview side window."
  :type '(choice number (const 0.5)) :group 'org-insight-vertico)

(defvar-local org-insight--vertico-last-preview nil
  "Cache of the last previewed (FILE . LINE) during a Vertico session.")

(defvar org-insight--current-dir nil
  "Directory used for the ongoing Vertico search session.")

(defvar org-insight--current-keywords nil
  "Keywords list parsed from current minibuffer input; used for preview highlighting.")

;; --- Utility: split keywords, respecting quotes (\"like this\" or 'like this')
(defun org-insight--split-keywords-quoted (s)
  "Split S into keywords, respecting double/single quoted phrases."
  (let ((pos 0) out)
    (while (and s (< pos (length s)))
      (cond
       ((string-match "\\s-*\"\\([^\"]+\\)\"" s pos)
        (setq out (cons (match-string 1 s) out)
              pos (match-end 0)))
       ((string-match "\\s-*'\\([^']+\\)'" s pos)
        (setq out (cons (match-string 1 s) out)
              pos (match-end 0)))
       ((string-match "\\s-*\\([^ \t\n\r,]+\\)" s pos)
        (setq out (cons (match-string 1 s) out)
              pos (match-end 0)))
       (t (setq pos (1+ pos)))))
    (nreverse out)))

;; --- Build candidates from current input

;; Stable flatten: keep first-seen file order; sort lines ascending within each file
(defun org-insight--sort-items-by-file-and-line (items)
  "Return ITEMS ordered by first-seen file, with each file's items sorted by line asc."
  (let ((table (make-hash-table :test 'equal))
        files-order)
    (dolist (it items)
      (let* ((f (org-insight-item-file it))
             (bucket (gethash f table)))
        (unless bucket
          (setq bucket '())
          (puthash f bucket table)
          (push f files-order))
        (puthash f (cons it (gethash f table)) table)))
    (setq files-order (nreverse files-order))
    (let (out)
      (dolist (f files-order)
        (let* ((bucket (nreverse (gethash f table))))
          ;; sort lines ascending within this file
          (setq bucket (sort bucket (lambda (a b)
                                      (< (org-insight-item-line a)
                                         (org-insight-item-line b)))))
          (setq out (nconc out bucket))))
      out)))

(defun org-insight--items-for-input (dir input)
  "Return list of `org-insight-item' for DIR and minibuffer INPUT,
stable-sorted by (FILE order of first occurrence, then LINE asc).
No search is performed when INPUT is shorter than
`org-insight-min-input-chars'."
  (let* ((trimmed (string-trim input)))
    (if (< (length trimmed) org-insight-min-input-chars)
        (progn
          (setq org-insight--current-keywords nil)
          nil)
      (let* ((kws (org-insight--split-keywords-quoted trimmed))
             (backend (or (and (boundp 'org-insight-backend) org-insight-backend) 'ripgrep))
             (regexp-p (and (boundp 'org-insight-regexp-p) org-insight-regexp-p))
             (collector
              (pcase backend
                ('ripgrep #'org-insight--collect-lines-ripgrep)
                ('grep    #'org-insight--collect-lines-grep)
                (_       #'org-insight--collect-lines-elisp)))
             (seen (make-hash-table :test 'equal))
             acc)
        (setq org-insight--current-keywords kws)
        ;; Gather and dedupe by (file . line)
        (dolist (kw kws)
          (dolist (it (funcall collector dir kw regexp-p))
            (let* ((file (org-insight-item-file it))
                   (line (org-insight-item-line it))
                   (key  (cons file line)))
              (unless (gethash key seen)
                (puthash key t seen)
                (push it acc)))))
        ;; Stable order: by first-seen file, then line asc
        (org-insight--sort-items-by-file-and-line (nreverse acc))))))

;; --- Candidate formatting
(defun org-insight--format-candidate (it input)
  "Format a single ITEM for display as \"LINE: MATCH\" (no filename),
styling the LINE: prefix with `org-insight-lineno-face`."
  (let* ((file (org-insight-item-file it))
         (line (org-insight-item-line it))
         (txt  (org-insight-item-text it))
         (linostr (propertize (number-to-string line) 'face 'org-insight-lineno-face))
         (colon   (propertize ":" 'face 'org-insight-lineno-face))
         (disp (concat linostr colon " " txt))
         ;; Unique key ties the display back to its source even if DISP duplicates
         (key  (concat (expand-file-name file) ":" (number-to-string line))))
    (org-insight--display-wrapped
     input disp key
     'org-insight-file file
     'org-insight-line line
     'mouse-face 'highlight)))

(defun org-insight--annotation (cand)
  "Annotation function: show base file name."
  (let* ((file (get-text-property 0 'org-insight-file cand)))
    (when file
      (concat "  [" (file-name-nondirectory file) "]"))))

(defun org-insight--group (cand transform)
  "Group candidates by file; prefer Org #+title if present.
When TRANSFORM is non-nil, return CAND unchanged."
  (if transform
      cand
    (let* ((file (or (get-text-property 0 'org-insight-file cand)
                     (and (hash-table-p org-insight--cand-map)
                          (or (car-safe (gethash cand org-insight--cand-map))
                              (let* ((key (and (stringp cand)
                                               (if (string-match "\\`.*?⇨ \\(.*\\)\\'" cand)
                                                   (match-string 1 cand)
                                                 cand))))
                                (and key (car-safe (gethash key org-insight--cand-map)))))))))
      (org-insight--file-group-name file))))

(defun org-insight--table-with-metadata (table metadata)
  "Wrap completion TABLE so that (ACTION 'metadata) returns METADATA."
  (lambda (string pred action)
    (if (eq action 'metadata)
        metadata
      (complete-with-action action table string pred))))

;; Force prefix-match compatibility, and keep a unique hidden key.
;; The *real* candidate string becomes: "INPUT ⇨ DISP \x1F KEY"
;; but the minibuffer *displays* only DISP via the `display` property.
(defun org-insight--display-wrapped (input disp &optional key &rest props)
  "Return a string that STARTS WITH INPUT but DISPLAYS as DISP.
KEY (optional) is appended to the real string (hidden by `display`) to keep it unique.
Additional PROPS are added as text properties."
  (let* ((real (concat input " ⇨ " disp (if key (concat "\x1F" key) "")))
         (s (apply #'propertize real 'display disp props)))
    s))

;; --- Dynamic completion table
(defun org-insight--completion-table (dir)
  "A completion table that searches DIR from minibuffer input.
Populates `org-insight--cand-map` keyed by the REAL candidate string."
  (let ((gen (lambda (input)
               (let ((trimmed (string-trim input)))
                 (when (hash-table-p org-insight--cand-map)
                   (clrhash org-insight--cand-map))
                 (mapcar
                  (lambda (it)
                    (let* ((file (org-insight-item-file it))
                           (line (org-insight-item-line it))
                           (txt  (org-insight-item-text it))
                           ;; display as \"LINE: text\" (no filename), with a styled LINE:
                           (linostr (propertize (number-to-string line) 'face 'org-insight-lineno-face))
                           (colon   (propertize ":" 'face 'org-insight-lineno-face))
                           (disp (concat linostr colon " " txt))
                           (key  (concat (expand-file-name file) ":" (number-to-string line)))
                           (wrapped (org-insight--display-wrapped
                                     trimmed disp key
                                     'org-insight-file file
                                     'org-insight-line line
                                     'mouse-face 'highlight)))
                      ;; Map REAL candidate -> (FILE . LINE)
                      (when (hash-table-p org-insight--cand-map)
                        (puthash wrapped (cons file line) org-insight--cand-map)
                        ;; lenient fallback key
                        (puthash disp (cons file line) org-insight--cand-map))
                      wrapped))
                  (org-insight--items-for-input dir trimmed))))))
    (org-insight--table-with-metadata
     (completion-table-dynamic gen t)
     '(metadata
       (category . org-insight)
       (group-function . org-insight--group)
       ;; CRITICAL: keep the generator order
       (display-sort-function . identity)
       (cycle-sort-function . identity)))))

(defconst org-insight--preview-buffer-name "*Org Insight Preview*")

(defun org-insight--get-preview-buffer ()
  "Return the single, reusable preview buffer (not visiting any file)."
  (or (get-buffer org-insight--preview-buffer-name)
      (let ((buf (get-buffer-create org-insight--preview-buffer-name)))
        (with-current-buffer buf
          (setq-local buffer-read-only t)
          (setq-local buffer-undo-list t)   ;; disable undo growth
          (setq-local truncate-lines t))
        buf)))

(defun org-insight--preview-load-file-contents (buf file)
  "Load FILE's contents into BUF (scratch), set mode, and record current file."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-file-contents file nil nil nil) ; do not visit
      ;; Set major mode by pretending to be FILE just for mode detection
      (let ((buffer-file-name file))
        (set-auto-mode))
      (setq-local buffer-file-name nil)
      (setq-local org-insight--preview-current-file file)
      (goto-char (point-min)))))

;; --- Preview helper (reuses your highlight/overlay logic if present)
(defun org-insight--preview-file-line (file line)
  "Show FILE in the reusable side-window and center LINE.
Reloads content only when FILE differs from what's currently shown.
Also restores keyword highlighting."
  (when (and file (integerp line))
    (let* ((buf (org-insight--get-preview-buffer))
           (win (display-buffer-in-side-window
                 buf `((side . ,org-insight-vertico-preview-side)
                       (window-width . ,org-insight-vertico-preview-width)
                       (slot . 0)
                       (window-parameters . ((no-other-window . t)
                                             (no-delete-other-windows . t)))))))
      ;; Make sure Emacs never reuses this window for the visited file.
      (when (window-live-p win)
        (set-window-dedicated-p win t))
      ;; Reload contents if needed.
      (with-current-buffer buf
        (unless (and (boundp 'org-insight--preview-current-file)
                     (stringp org-insight--preview-current-file)
                     (string-equal org-insight--preview-current-file file))
          (org-insight--preview-load-file-contents buf file)))
      ;; Jump, center, and (re)highlight.
      (with-selected-window win
        (goto-char (point-min))
        (forward-line (max 0 (1- line)))
        (recenter)
        (cond
         ((and (fboundp 'org-insight--clear-preview-overlays)
               (fboundp 'org-insight--highlight-keywords-in-buffer)
               (boundp 'org-insight--current-keywords))
          (org-insight--clear-preview-overlays)
          (org-insight--highlight-keywords-in-buffer org-insight--current-keywords))
         (t
          (org-insight--preview-highlight-keywords
           org-insight--current-keywords
           (and (boundp 'org-insight-regexp-p) org-insight-regexp-p))))))))

;; --- Live preview on candidate move (minibuffer local)
(defun org-insight--vertico-preview-post-command ()
  "Minibuffer-local hook to preview current Vertico candidate."
  (when (and (bound-and-true-p vertico-mode)
             (boundp 'vertico--index)
             (>= vertico--index 0))
    (let* ((cand (ignore-errors (funcall (intern "vertico--candidate"))))
           (pair (and cand (hash-table-p org-insight--cand-map)
                      (gethash cand org-insight--cand-map)))
           ;; Fallback to unwrapped display text only if needed
           (pair (or pair
                     (let* ((key (and cand (org-insight--unwrap-candidate cand))))
                       (and key (gethash key org-insight--cand-map)))))
           (file (car-safe pair))
           (line (cdr-safe pair)))
      (when (and file line)
        (setq org-insight--vertico-last-preview (cons file line))
        (org-insight--preview-file-line file line)))))

;; One reusable preview buffer (already defined earlier)
(defconst org-insight--preview-buffer-name "*Org Insight Preview*")

;; Track what file is currently displayed in the preview buffer
(defvar-local org-insight--preview-current-file nil)
(defvar-local org-insight--preview-overlays nil)

;; A face for keyword highlights (keeps theme-friendly defaults)
(defface org-insight-highlight-face
  '((t :inherit highlight :weight bold))
  "Face for keyword highlights in the preview buffer.")

(defun org-insight--preview-clear-overlays ()
  "Clear temp highlight overlays in the preview buffer."
  (when org-insight--preview-overlays
    (mapc #'delete-overlay org-insight--preview-overlays)
    (setq org-insight--preview-overlays nil)))

(defun org-insight--preview-highlight-keywords (keywords regexp-p)
  "Highlight KEYWORDS in current buffer. Respect REGEXP-P for regex vs literal."
  (org-insight--preview-clear-overlays)
  (when (and keywords (consp keywords))
    (save-excursion
      (dolist (kw keywords)
        (let ((re (if regexp-p kw (regexp-quote kw))))
          (goto-char (point-min))
          (while (re-search-forward re nil t)
            (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put ov 'face 'org-insight-highlight-face)
              (push ov org-insight--preview-overlays))))))))

(defun org-insight--minibuffer-setup ()
  "Setup live preview and keybindings during `org-insight` minibuffer session.
Also force match highlighting to render in pure black for this minibuffer only."
  (add-hook 'post-command-hook #'org-insight--vertico-preview-post-command nil t)
  ;; Bind RET locally for visit
  (let* ((parent (current-local-map))
         (map (make-sparse-keymap)))
    (set-keymap-parent map parent)
    (define-key map (kbd "RET") #'org-insight--vertico-accept-visit)
    (define-key map [return]    #'org-insight--vertico-accept-visit)
    (use-local-map map))

  ;; Keep our candidate order
  (when (boundp 'vertico-sort-function)
    (setq-local vertico-sort-function nil))
  (when (boundp 'vertico-group-sort-function)
    (setq-local vertico-group-sort-function nil))

  ;; ----- Make matches black (local to minibuffer) -----
  (when (boundp 'orderless-match-faces)
    (setq-local orderless-match-faces
                (mapcar (lambda (_i) 'org-insight-match-face)
                        (number-sequence 0 9)))
    (when (boundp 'orderless-highlight-matches)
      (setq-local orderless-highlight-matches t)))
  (let ((faces-to-remap
         '(completions-common-part
           completions-first-difference
           orderless-match-face-0 orderless-match-face-1
           orderless-match-face-2 orderless-match-face-3
           orderless-match-face-4 orderless-match-face-5
           orderless-match-face-6 orderless-match-face-7
           orderless-match-face-8 orderless-match-face-9)))
    (dolist (ff faces-to-remap)
      (when (facep ff)
        (face-remap-add-relative ff 'org-insight-match-face)))))

(defun org-insight--visit-candidate (cand &optional window)
  "Visit CAND by opening file and jumping to line in WINDOW (default: selected)."
  (let* ((prop-file (and (stringp cand) (get-text-property 0 'org-insight-file cand)))
         (prop-line (and (stringp cand) (get-text-property 0 'org-insight-line cand)))
         (key (and (stringp cand) (org-insight--unwrap-candidate cand)))
         (pair (and (not prop-file) key (hash-table-p org-insight--cand-map)
                    (gethash key org-insight--cand-map)))
         (file (or prop-file (car-safe pair)))
         (line (or prop-line (cdr-safe pair))))
    (unless (and file line)
      (user-error "Invalid selection: missing file/line properties"))
    (org-insight--visit-file-line-in-window file line (or window (selected-window)))))

(defvar org-insight--visit-triggered nil
  "Non-nil while a Vertico accept has visited a candidate.")

(defun org-insight--vertico-accept-visit ()
  "Visit the highlighted candidate in the original window and exit minibuffer."
  (interactive)
  (let* ((cand (when (and (boundp 'vertico--index) (>= vertico--index 0))
                 (ignore-errors (funcall (intern "vertico--candidate")))))
         (pair (and cand (hash-table-p org-insight--cand-map)
                    (gethash cand org-insight--cand-map)))
         (pair (or pair
                   (let* ((key (and cand (org-insight--unwrap-candidate cand))))
                     (and key (gethash key org-insight--cand-map)))))
         (file (car-safe pair))
         (line (cdr-safe pair))
         (target (or org-insight--calling-window
                     (and (fboundp 'minibuffer-selected-window)
                          (minibuffer-selected-window))
                     (selected-window))))
    (unless (and file line)
      (user-error "Invalid selection: missing file/line (no mapping for candidate)"))
    (setq org-insight--visit-triggered t)
    (run-at-time 0 nil #'org-insight--visit-file-line-in-window file line target)
    (exit-minibuffer)))

;; ---------- Org title lookup (fast, no visiting) ----------
(defvar org-insight--title-cache nil
  "Per-session cache mapping absolute file paths to display titles for grouping.")

(defun org-insight--org-file-title-fast (file)
  "Return #+title of FILE if it's an .org file and has a title, else nil.
Reads only the first ~8KB without visiting the file."
  (when (and (stringp file)
             (string-equal (downcase (or (file-name-extension file) "")) "org")
             (file-readable-p file))
    (or (and (hash-table-p org-insight--title-cache)
             (gethash file org-insight--title-cache))
        (let ((title nil))
          (with-temp-buffer
            ;; Read only the head of the file; enough for keywords.
            (insert-file-contents file nil 0 8192)
            (goto-char (point-min))
            (let ((case-fold-search t))
              (when (re-search-forward "^[ \t]*#\\+title:[ \t]*\\(.*\\)\\s-*$" nil t)
                (setq title (string-trim (match-string 1))))))
          (when (and (hash-table-p org-insight--title-cache) title)
            (puthash file title org-insight--title-cache))
          title))))

(defun org-insight--file-group-name (file)
  "Return the display name for grouping: Org #+title or file base name."
  (or (org-insight--org-file-title-fast file)
      (file-name-nondirectory (or file ""))))

(defface org-insight-lineno-face
  '((t :inherit shadow :weight semibold))
  "Face used for the \"LINE:\" prefix in Org Insight candidates.")

(defface org-insight-match-face
  '((t :foreground "black"))
  "Face used to render matched parts of candidates in the Vertico list.")

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
      ('replace
       ;; Remember exactly what we're replacing so we can restore on exit.
       (unless (and (window-live-p org-insight--live-replaced-window)
                    (buffer-live-p org-insight--live-replaced-buffer))
         (setq org-insight--live-replaced-window (selected-window)
               org-insight--live-replaced-buffer (window-buffer (selected-window))
               org-insight--live-replaced-point  (window-point  (selected-window))))
       ;; Show the live preview buffer in the same window (make it “large”).
       (let ((win (or org-insight--live-replaced-window (selected-window))))
         (set-window-buffer win buf)
         (select-window win)))
      ;; Initialize with an empty header so users see it's on
      (org-insight--render-live-buffer nil "—" (abbreviate-file-name default-directory)))))

(defun org-insight--close-live ()
  "Close the live preview window/frame (if any), cancel timer, and
restore windows when 'replace was used."
  ;; 1) Cancel debounce timer if present
  (when (and (boundp 'org-insight--live-idle-timer)
             (timerp org-insight--live-idle-timer))
    (cancel-timer org-insight--live-idle-timer))
  (setq org-insight--live-idle-timer nil)

  ;; 2) Close depending on display mode
  (pcase org-insight-live-preview-display
    ('replace
     (let* ((live-buf (or (and (boundp 'org-insight--live-buffer-name)
                               (get-buffer org-insight--live-buffer-name))
                          (get-buffer "*Org Insight Live*"))))
       ;; If we still own the window we replaced, restore its original buffer & point.
       (when (and (window-live-p org-insight--live-replaced-window)
                  (buffer-live-p org-insight--live-replaced-buffer))
         (with-selected-window org-insight--live-replaced-window
           (when (buffer-live-p org-insight--live-replaced-buffer)
             (set-window-buffer nil org-insight--live-replaced-buffer))
           (when (and org-insight--live-replaced-point
                      (buffer-live-p org-insight--live-replaced-buffer))
             (set-window-point nil org-insight--live-replaced-point))))
       ;; Fallback: if the live buffer is visible anywhere, bury it or replace it.
       (when (buffer-live-p live-buf)
         (dolist (w (get-buffer-window-list live-buf t t))
           (when (window-live-p w)
             (if (buffer-live-p org-insight--live-replaced-buffer)
                 (set-window-buffer w org-insight--live-replaced-buffer)
               (quit-window nil w))))
         (bury-buffer live-buf)))
     ;; Clear state for the next session.
     (setq org-insight--live-replaced-window nil
           org-insight--live-replaced-buffer nil
           org-insight--live-replaced-point  nil))
    ('side
     (when-let* ((b (get-buffer org-insight--live-buffer-name))
                 (w (get-buffer-window b t)))
       (when (window-live-p w)
         (ignore-errors (delete-window w)))))
    ('frame
     (when (and (frame-live-p org-insight--live-frame)
                (equal (frame-parameter org-insight--live-frame 'name) "Org Insight Live"))
       (ignore-errors (delete-frame org-insight--live-frame)))
     (setq org-insight--live-frame nil))
    (_ nil)))

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
(defun org-insight (&optional dir)
  "Search org/plaintext files and jump to results.

Uses Vertico when available and `org-insight-use-vertico' is non-nil.
Does NOT prompt for a directory; reuses `org-insight-default-directory'."
  (interactive)
  (require 'subr-x)
  (let* ((base (or dir
                   (and (boundp 'org-insight-default-directory)
                        org-insight-default-directory)
                   (ignore-errors (locate-dominating-file default-directory ".git"))
                   default-directory))
         (use-vertico (and (boundp 'org-insight-use-vertico)
                           org-insight-use-vertico
                           (featurep 'vertico))))
    (if (not use-vertico)
        (if (fboundp 'org-insight--legacy-ui)
            (org-insight--legacy-ui base)
          (user-error "Vertico not available and no legacy UI function `org-insight--legacy-ui' found."))
      (let* ((org-insight--current-dir base)
             (minibuffer-allow-text-properties t)
             (table (org-insight--completion-table base))
             (prompt (format "Org Insight (%s): " (abbreviate-file-name base))))
        ;; NEW: per-session caches
        (setq org-insight--cand-map   (make-hash-table :test 'equal)
              org-insight--title-cache (make-hash-table :test 'equal))
        (let ((org-insight--visit-triggered nil)
              (org-insight--calling-window (selected-window)))
          (minibuffer-with-setup-hook #'org-insight--minibuffer-setup
            (let ((selection (completing-read prompt table nil t "")))
              (unless org-insight--visit-triggered
                (when (and selection (not (string-empty-p selection)))
                  (let* ((key (if (string-match "\\`.*?⇨ \\(.*\\)\\'" selection)
                                  (match-string 1 selection)
                                selection))
                         (pair (and key (gethash key org-insight--cand-map)))
                         (file (car-safe pair))
                         (line (cdr-safe pair))
                         (target (or org-insight--calling-window
                                     (and (fboundp 'minibuffer-selected-window)
                                          (minibuffer-selected-window))
                                     (selected-window))))
                    (when (and file line)
                      (run-at-time 0 nil
                                   #'org-insight--visit-file-line-in-window
                                   file line target))))))))))))

(provide 'org-insight)
;;; org-insight.el ends here
