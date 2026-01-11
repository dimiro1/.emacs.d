;;; d1-git.el --- Git integration with Magit  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; Git integration using Magit, a powerful Git interface for Emacs.
;; Provides comprehensive git operations with an intuitive interface.
;;
;; Additional features:
;; - d1-git-browse-remote: Open the current git repository or file in the browser
;;   Supports GitHub, GitLab, and Codeberg.  Opens the current file's path at the
;;   current line number when viewing a file buffer, or the repository root
;;   otherwise.  Uses the current branch name when available.
;;

;;; Code:

;;; Magit Configuration
(use-package magit
  :ensure t
  :custom
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers 'dontask)
  (magit-auto-revert-mode t)
  (magit-no-confirm '(stage-all-changes unstage-all-changes))
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  :bind
  ("C-x g" . magit-status)
  ("C-c g o" . d1-git-browse-remote))

;;; Git Remote Browser Functions

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

(defun d1--git-get-current-branch ()
  "Get the current git branch name.
Returns nil if not in a git repository or unable to determine branch."
  (when-let* ((root (vc-root-dir))
              (default-directory root))
    (condition-case nil
        (let ((branch (string-trim
                       (shell-command-to-string "git branch --show-current"))))
          ;; Return nil if branch is empty string
          (when (and branch (not (string-empty-p branch)))
            branch))
      (error nil))))

(defun d1--git-get-current-line ()
  "Get the current line number.
Returns nil if not in a file buffer."
  (when (buffer-file-name)
    (line-number-at-pos)))

(defun d1--git-remote-to-browse-url (remote-url &optional relative-path branch line-number)
  "Convert REMOTE-URL to a web browsable URL.
Supports GitHub, GitLab, and Codeberg.
If RELATIVE-PATH is provided, appends the path to the URL.
If BRANCH is provided, uses that branch name instead of HEAD.
If LINE-NUMBER is provided, appends the line anchor to the URL.
Signals an error for unsupported platforms."
  (let* ((url (string-trim remote-url))
         ;; Strip .git suffix if present
         (url (if (string-suffix-p ".git" url)
                  (substring url 0 -4)
                url))
         (host nil)
         (path nil)
         ;; Use branch if provided and non-empty, otherwise use HEAD
         (ref (if (and branch (not (string-empty-p branch)))
                  branch
                "HEAD")))

    ;; Parse SSH format: git@host:user/repo
    (if (string-match "^git@\\([^:]+\\):\\(.+\\)$" url)
        (setq host (match-string 1 url)
              path (match-string 2 url))
      ;; Parse HTTPS format: https://host/user/repo
      (when (string-match "^https://\\([^/]+\\)/\\(.+\\)$" url)
        (setq host (match-string 1 url)
              path (match-string 2 url))))

    (unless (and host path)
      (error "Unable to parse git remote URL: %s" remote-url))

    ;; Build the web URL based on the hosting platform
    (let ((base-url (format "https://%s/%s" host path)))
      (cond
       ;; GitHub and GitLab use the same format
       ((or (string-match-p "github\\.com" host)
            (string-match-p "gitlab\\.com" host))
        (if relative-path
            (let ((file-url (format "%s/blob/%s/%s" base-url ref relative-path)))
              (if line-number
                  (format "%s#L%d" file-url line-number)
                file-url))
          base-url))

       ;; Codeberg uses a different format
       ((string-match-p "codeberg\\.org" host)
        (if relative-path
            (let ((file-url (format "%s/src/branch/%s/%s" base-url ref relative-path)))
              (if line-number
                  (format "%s#L%d" file-url line-number)
                file-url))
          base-url))

       ;; Unsupported platform
       (t
        (error "Unsupported git hosting platform: %s" host))))))

(defun d1-git-browse-remote ()
  "Open the git remote URL in the browser.
Opens the 'origin' remote.  When viewing a file buffer, opens the current
file's path on the hosting platform at the current line number.  Otherwise,
opens the repository root.

Uses the current branch name when available, otherwise falls back to HEAD.

Supports GitHub, GitLab, and Codeberg."
  (interactive)
  (unless (vc-root-dir)
    (user-error "Not in a git repository"))

  (let ((remote-url (d1--git-get-remote-url)))
    (unless remote-url
      (user-error "No 'origin' remote found"))

    (let* ((relative-path (d1--git-get-relative-path))
           (branch (d1--git-get-current-branch))
           (line-number (d1--git-get-current-line))
           (browse-url (d1--git-remote-to-browse-url remote-url relative-path branch line-number)))
      (browse-url browse-url)
      (message "Opening: %s" browse-url))))

(provide 'd1-git)
;;; d1-git.el ends here
