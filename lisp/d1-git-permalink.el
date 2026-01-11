;;; d1-git-permalink.el --- Generate permalinks to git hosting platforms  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; Generates permanent URLs (permalinks) to files in git repositories.
;; Opens files at specific lines or line ranges on GitHub, GitLab, Codeberg, etc.
;;
;; Features:
;; - Uses commit SHA for permanent, stable URLs
;; - Supports line numbers and line ranges (with region selection)
;; - Warns when file has uncommitted changes
;; - Extensible: easy to add support for new git hosting platforms
;;
;; Usage:
;;   M-x d1-git-browse-permalink    - Open permalink in browser (C-c g o)
;;   M-x d1-git-copy-permalink      - Copy permalink to kill ring
;;   C-u M-x d1-git-browse-permalink - Copy instead of opening
;;
;; Adding support for new git hosts:
;; Add a new entry to `d1-git-host-configs' with:
;;   :name        - Human-readable name (e.g., "Bitbucket")
;;   :match       - Regex to match the host (e.g., "bitbucket\\.org")
;;   :file-path   - Format string for file URLs (e.g., "%s/src/%s/%s")
;;   :line-anchor - Function to format line anchors
;;                  (use `d1--format-line-anchor-default' for most hosts,
;;                   or `d1--format-line-anchor-gitlab-style' for GitLab-like format,
;;                   or create a custom function)
;;

;;; Code:

;;; Configuration

(defgroup d1-git-permalink nil
  "Generate permalinks to git hosting platforms."
  :group 'tools
  :prefix "d1-git-")

;;; Git Host Configurations
;; Each host configuration is a plist with:
;;   :name        - Human-readable name
;;   :match       - Regex pattern to match the host
;;   :file-path   - Format string for file URLs (args: base-url commit-sha relative-path)
;;   :line-anchor - Function to format line anchors (args: line-range)

(defvar d1-git-host-configs
  '((:name "GitHub"
     :match "github\\.com"
     :file-path "%s/blob/%s/%s"
     :line-anchor d1--format-line-anchor-default)

    (:name "GitLab"
     :match "gitlab\\.com"
     :file-path "%s/blob/%s/%s"
     :line-anchor d1--format-line-anchor-gitlab-style)

    (:name "Codeberg"
     :match "codeberg\\.org"
     :file-path "%s/src/commit/%s/%s"
     :line-anchor d1--format-line-anchor-default))
  "Configuration for supported git hosting platforms.
Each entry is a plist defining how to build URLs for that platform.")


;;; Line Anchor Formatters

(defun d1--format-line-anchor-with-format (line-range range-format)
  "Format line anchor using the specified RANGE-FORMAT.
LINE-RANGE can be a cons cell (START . END), a number, or nil.
RANGE-FORMAT is a format string for the range (e.g., \"L%d-L%d\" or \"L%d-%d\")."
  (cond
   ((null line-range) "")
   ((and (consp line-range) (= (car line-range) (cdr line-range)))
    (format "#L%d" (car line-range)))
   ((consp line-range)
    (format (concat "#" range-format) (car line-range) (cdr line-range)))
   ((numberp line-range)
    (format "#L%d" line-range))))

(defun d1--format-line-anchor-default (line-range)
  "Format line anchor using the standard format (L10-L20).
Used by most git hosts including GitHub, Codeberg, SourceHut, etc.
LINE-RANGE can be a cons cell (START . END), a number, or nil."
  (d1--format-line-anchor-with-format line-range "L%d-L%d"))

(defun d1--format-line-anchor-gitlab-style (line-range)
  "Format line anchor using GitLab-style format (L10-20 without second L).
LINE-RANGE can be a cons cell (START . END), a number, or nil."
  (d1--format-line-anchor-with-format line-range "L%d-%d"))


;;; Helper Functions

(defun d1--find-host-config (host)
  "Find the host configuration for HOST.
Returns the matching config plist or nil if not found."
  (seq-find (lambda (config)
              (string-match-p (plist-get config :match) host))
            d1-git-host-configs))

(defun d1--git-get-remote-url ()
  "Get the URL for the 'origin' remote.
Returns nil if not in a git repository or 'origin' remote doesn't exist."
  (when-let* ((root (vc-root-dir))
              (default-directory root))
    (condition-case nil
        (string-trim
         (shell-command-to-string "git remote get-url origin"))
      (error nil))))

(defun d1--git-get-relative-path ()
  "Get the current buffer's file path relative to the git repository root.
Returns nil if not in a file buffer or not in a git repository."
  (when-let* ((file (buffer-file-name))
              (root (vc-root-dir)))
    (file-relative-name file root)))

(defun d1--git-get-current-commit ()
  "Get the current commit SHA.
Returns nil if not in a git repository."
  (when-let* ((root (vc-root-dir))
              (default-directory root))
    (condition-case nil
        (string-trim
         (shell-command-to-string "git rev-parse HEAD"))
      (error nil))))

(defun d1--git-has-uncommitted-changes ()
  "Check if the current file or repository has uncommitted changes.
Returns t if there are uncommitted changes, nil otherwise."
  (when-let* ((root (vc-root-dir))
              (default-directory root))
    (condition-case nil
        (not (zerop (call-process "git" nil nil nil
                                  "diff-index" "--quiet" "HEAD" "--")))
      (error nil))))

(defun d1--git-get-line-range ()
  "Get the current line or line range if a region is selected.
Returns a cons cell (START . END) where START and END are line numbers.
If no region is active, both START and END are the current line number.
Returns nil if not in a file buffer."
  (when (buffer-file-name)
    (if (use-region-p)
        (let ((start (line-number-at-pos (region-beginning)))
              (end (line-number-at-pos (region-end))))
          ;; Ensure start <= end
          (cons (min start end) (max start end)))
      ;; No region, just current line
      (let ((current-line (line-number-at-pos)))
        (cons current-line current-line)))))

(defun d1--parse-git-remote-url (remote-url)
  "Parse REMOTE-URL and return a cons cell (HOST . REPO-PATH).
Handles both SSH (git@host:path) and HTTPS (https://host/path) formats.
Strips .git suffix if present.
Returns nil if unable to parse."
  (let* ((url (string-trim remote-url))
         ;; Strip .git suffix if present
         (url (if (string-suffix-p ".git" url)
                  (substring url 0 -4)
                url))
         (host nil)
         (path nil))

    ;; Parse SSH format: git@host:user/repo
    (if (string-match "^git@\\([^:]+\\):\\(.+\\)$" url)
        (setq host (match-string 1 url)
              path (match-string 2 url))
      ;; Parse HTTPS format: https://host/user/repo
      (when (string-match "^https://\\([^/]+\\)/\\(.+\\)$" url)
        (setq host (match-string 1 url)
              path (match-string 2 url))))

    (when (and host path)
      (cons host path))))

(defun d1--build-file-url (host repo-path commit-sha relative-path)
  "Build file URL for HOST and REPO-PATH using COMMIT-SHA and RELATIVE-PATH.
Returns the file URL without line anchors.
Uses host configuration from `d1-git-host-configs'."
  (let ((config (d1--find-host-config host)))
    (unless config
      (error "Unsupported git hosting platform: %s" host))

    (let ((base-url (format "https://%s/%s" host repo-path))
          (file-path-template (plist-get config :file-path)))
      (format file-path-template base-url commit-sha relative-path))))

(defun d1--format-line-anchor (host line-range)
  "Format line anchor for HOST and LINE-RANGE.
Uses host configuration from `d1-git-host-configs'.
LINE-RANGE can be:
  - A cons cell (START . END) for a line range
  - A single number for a single line
  - nil for no line number
Returns the anchor string (e.g., \"#L10-L20\") or empty string if no range."
  (let ((config (d1--find-host-config host)))
    (unless config
      (error "Unsupported git hosting platform: %s" host))

    (let ((formatter (plist-get config :line-anchor)))
      (funcall formatter line-range))))

(defun d1--git-remote-to-permalink-url (remote-url &optional relative-path commit-sha line-range)
  "Convert REMOTE-URL to a web browsable permalink URL.
Supports GitHub, GitLab, and Codeberg.
If RELATIVE-PATH is provided, appends the path to the URL.
If COMMIT-SHA is provided, uses it to create a permalink.
If LINE-RANGE is provided, appends the line anchor to the URL.
  LINE-RANGE can be:
  - A cons cell (START . END) for a line range
  - A single number for a single line (backward compatibility)
  - nil for no line number
Signals an error for unsupported platforms."
  (let ((parsed (d1--parse-git-remote-url remote-url)))
    (unless parsed
      (error "Unable to parse git remote URL: %s" remote-url))

    (let ((host (car parsed))
          (repo-path (cdr parsed)))
      (if relative-path
          ;; Build file URL with optional line anchor
          (let ((file-url (d1--build-file-url host repo-path commit-sha relative-path))
                (anchor (d1--format-line-anchor host line-range)))
            (concat file-url anchor))
        ;; Just return repository root
        (format "https://%s/%s" host repo-path)))))


;;; Public Commands

(defun d1--git-permalink-action (url line-range has-changes action-fn action-verb)
  "Perform ACTION-FN on URL and display a message with ACTION-VERB.
LINE-RANGE is used to format range information in the message.
HAS-CHANGES indicates if there are uncommitted changes.
ACTION-FN is called with URL as argument (e.g., `kill-new' or `browse-url').
ACTION-VERB is used in the message (e.g., \"Copied\" or \"Opening\")."
  (funcall action-fn url)
  (let ((range-info (when (and line-range (consp line-range))
                      (if (= (car line-range) (cdr line-range))
                          (format " (line %d)" (car line-range))
                        (format " (lines %d-%d)" (car line-range) (cdr line-range))))))
    (if has-changes
        (message "Warning: Uncommitted changes. %s permalink to last commit%s: %s"
                 action-verb (or range-info "") url)
      (message "%s permalink%s: %s" action-verb (or range-info "") url))))

(defun d1--git-get-permalink-url ()
  "Get the permalink URL for the current buffer or repository.
Returns the URL string.
Signals user-error if not in a git repository or no origin remote found."
  (unless (vc-root-dir)
    (user-error "Not in a git repository"))

  (let ((remote-url (d1--git-get-remote-url)))
    (unless remote-url
      (user-error "No 'origin' remote found"))

    (let* ((relative-path (d1--git-get-relative-path))
           (commit-sha (d1--git-get-current-commit))
           (line-range (d1--git-get-line-range))
           (has-changes (d1--git-has-uncommitted-changes))
           (url (d1--git-remote-to-permalink-url remote-url relative-path commit-sha line-range)))
      (list url line-range has-changes))))

(defun d1-git-copy-permalink ()
  "Copy the git permalink to the kill ring.
Copies a permanent URL to the 'origin' remote using the current commit SHA.
When viewing a file buffer, copies the current file's path at the current
line number, or line range if a region is selected.  Otherwise, copies the
repository root URL.

Warns if the file has uncommitted changes, as the URL will point to the
last committed version.

Supports GitHub, GitLab, and Codeberg."
  (interactive)
  (let* ((result (d1--git-get-permalink-url))
         (url (nth 0 result))
         (line-range (nth 1 result))
         (has-changes (nth 2 result)))
    (d1--git-permalink-action url line-range has-changes #'kill-new "Copied")))

(defun d1-git-browse-permalink (&optional copy-only)
  "Open the git permalink in the browser.
Opens a permanent URL to the 'origin' remote using the current commit SHA.
When viewing a file buffer, opens the current file's path at the current
line number, or line range if a region is selected.  Otherwise, opens the
repository root.

With prefix argument COPY-ONLY, copy the URL to the kill ring instead of
opening it in the browser (calls `d1-git-copy-permalink').

Warns if the file has uncommitted changes, as the URL will point to the
last committed version.

Supports GitHub, GitLab, and Codeberg."
  (interactive "P")
  (if copy-only
      (d1-git-copy-permalink)
    (let* ((result (d1--git-get-permalink-url))
           (url (nth 0 result))
           (line-range (nth 1 result))
           (has-changes (nth 2 result)))
      (d1--git-permalink-action url line-range has-changes #'browse-url "Opening"))))

(provide 'd1-git-permalink)
;;; d1-git-permalink.el ends here
