;;; d1-git-permalink-test.el --- Tests for d1-git-permalink.el -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Tests for d1-git-permalink.el
;;

;;; Code:

(require 'ert)
(require 'd1-git-permalink)

;;; Test URL Parsing

(ert-deftest d1-git-permalink-test-parse-git-remote-url-ssh ()
  "Test parsing SSH-style git remote URLs."
  (should (equal '("github.com" . "user/repo")
                 (d1--parse-git-remote-url "git@github.com:user/repo")))
  (should (equal '("github.com" . "user/repo")
                 (d1--parse-git-remote-url "git@github.com:user/repo.git")))
  (should (equal '("gitlab.com" . "org/project")
                 (d1--parse-git-remote-url "git@gitlab.com:org/project.git")))
  (should (equal '("codeberg.org" . "user/repo")
                 (d1--parse-git-remote-url "git@codeberg.org:user/repo"))))

(ert-deftest d1-git-permalink-test-parse-git-remote-url-https ()
  "Test parsing HTTPS git remote URLs."
  (should (equal '("github.com" . "user/repo")
                 (d1--parse-git-remote-url "https://github.com/user/repo")))
  (should (equal '("github.com" . "user/repo")
                 (d1--parse-git-remote-url "https://github.com/user/repo.git")))
  (should (equal '("gitlab.com" . "org/project")
                 (d1--parse-git-remote-url "https://gitlab.com/org/project.git")))
  (should (equal '("codeberg.org" . "user/repo")
                 (d1--parse-git-remote-url "https://codeberg.org/user/repo"))))

(ert-deftest d1-git-permalink-test-parse-git-remote-url-invalid ()
  "Test parsing invalid git remote URLs."
  (should (null (d1--parse-git-remote-url "")))
  (should (null (d1--parse-git-remote-url "not-a-url")))
  (should (null (d1--parse-git-remote-url "ftp://example.com/repo"))))

;;; Test Host Configuration

(ert-deftest d1-git-permalink-test-find-host-config ()
  "Test finding host configurations."
  (should (equal "GitHub"
                 (plist-get (d1--find-host-config "github.com") :name)))
  (should (equal "GitLab"
                 (plist-get (d1--find-host-config "gitlab.com") :name)))
  (should (equal "Codeberg"
                 (plist-get (d1--find-host-config "codeberg.org") :name)))
  (should (null (d1--find-host-config "unknown-host.com"))))

;;; Test Line Anchor Formatting with Host

(ert-deftest d1-git-permalink-test-format-line-anchor-github ()
  "Test formatting line anchors for GitHub."
  (should (equal "#L10"
                 (d1--format-line-anchor "github.com" 10)))
  (should (equal "#L10-L20"
                 (d1--format-line-anchor "github.com" '(10 . 20))))
  (should (equal ""
                 (d1--format-line-anchor "github.com" nil))))

(ert-deftest d1-git-permalink-test-format-line-anchor-gitlab ()
  "Test formatting line anchors for GitLab."
  (should (equal "#L10"
                 (d1--format-line-anchor "gitlab.com" 10)))
  (should (equal "#L10-20"
                 (d1--format-line-anchor "gitlab.com" '(10 . 20))))
  (should (equal ""
                 (d1--format-line-anchor "gitlab.com" nil))))

(ert-deftest d1-git-permalink-test-format-line-anchor-unsupported ()
  "Test formatting line anchors for unsupported hosts."
  (should-error (d1--format-line-anchor "unsupported.com" 10)))

;;; Test URL Building

(ert-deftest d1-git-permalink-test-build-file-url-github ()
  "Test building GitHub file URLs."
  (should (equal "https://github.com/user/repo/blob/abc123/path/to/file.el"
                 (d1--build-file-url "github.com" "user/repo" "abc123" "path/to/file.el"))))

(ert-deftest d1-git-permalink-test-build-file-url-gitlab ()
  "Test building GitLab file URLs."
  (should (equal "https://gitlab.com/org/project/blob/def456/src/main.js"
                 (d1--build-file-url "gitlab.com" "org/project" "def456" "src/main.js"))))

(ert-deftest d1-git-permalink-test-build-file-url-codeberg ()
  "Test building Codeberg file URLs."
  (should (equal "https://codeberg.org/user/repo/src/commit/xyz789/README.md"
                 (d1--build-file-url "codeberg.org" "user/repo" "xyz789" "README.md"))))

(ert-deftest d1-git-permalink-test-build-file-url-unsupported ()
  "Test building URLs for unsupported hosts."
  (should-error (d1--build-file-url "unsupported.com" "user/repo" "abc123" "file.txt")))

;;; Test Full URL Generation

(ert-deftest d1-git-permalink-test-remote-to-permalink-github ()
  "Test converting GitHub remote URL to permalink."
  ;; Repository root
  (should (equal "https://github.com/user/repo"
                 (d1--git-remote-to-permalink-url "git@github.com:user/repo.git")))
  
  ;; File without line number
  (should (equal "https://github.com/user/repo/blob/abc123/file.el"
                 (d1--git-remote-to-permalink-url "git@github.com:user/repo.git"
                                                   "file.el" "abc123" nil)))
  
  ;; File with single line
  (should (equal "https://github.com/user/repo/blob/abc123/file.el#L10"
                 (d1--git-remote-to-permalink-url "git@github.com:user/repo.git"
                                                   "file.el" "abc123" 10)))
  
  ;; File with line range
  (should (equal "https://github.com/user/repo/blob/abc123/file.el#L10-L20"
                 (d1--git-remote-to-permalink-url "git@github.com:user/repo.git"
                                                   "file.el" "abc123" '(10 . 20)))))

(ert-deftest d1-git-permalink-test-remote-to-permalink-gitlab ()
  "Test converting GitLab remote URL to permalink."
  ;; Repository root
  (should (equal "https://gitlab.com/org/project"
                 (d1--git-remote-to-permalink-url "https://gitlab.com/org/project.git")))
  
  ;; File with GitLab-style line range
  (should (equal "https://gitlab.com/org/project/blob/def456/src/main.js#L5-15"
                 (d1--git-remote-to-permalink-url "https://gitlab.com/org/project.git"
                                                   "src/main.js" "def456" '(5 . 15)))))

(ert-deftest d1-git-permalink-test-remote-to-permalink-codeberg ()
  "Test converting Codeberg remote URL to permalink."
  ;; Repository root
  (should (equal "https://codeberg.org/user/repo"
                 (d1--git-remote-to-permalink-url "git@codeberg.org:user/repo")))
  
  ;; File with line number
  (should (equal "https://codeberg.org/user/repo/src/commit/xyz789/README.md#L42"
                 (d1--git-remote-to-permalink-url "git@codeberg.org:user/repo"
                                                   "README.md" "xyz789" 42))))

(ert-deftest d1-git-permalink-test-remote-to-permalink-invalid ()
  "Test handling invalid remote URLs."
  (should-error (d1--git-remote-to-permalink-url "not-a-url")))

;;; Test Line Range Detection
;; Note: These tests use temp buffers to simulate file buffers

(ert-deftest d1-git-permalink-test-get-line-range-no-region ()
  "Test getting line range without region selection."
  (with-temp-buffer
    ;; Create a temporary file so buffer-file-name returns non-nil
    (let ((temp-file (make-temp-file "d1-git-test")))
      (unwind-protect
          (progn
            (set-visited-file-name temp-file t)
            (insert "line 1\n")
            (insert "line 2\n")
            (insert "line 3\n")
            (goto-char (point-min))
            (forward-line 1)  ;; Go to line 2
            (should (equal '(2 . 2) (d1--git-get-line-range))))
        ;; Cleanup
        (set-buffer-modified-p nil)
        (kill-buffer)
        (delete-file temp-file)))))

(ert-deftest d1-git-permalink-test-get-line-range-with-region ()
  "Test getting line range with region selection."
  (with-temp-buffer
    ;; Create a temporary file so buffer-file-name returns non-nil
    (let ((temp-file (make-temp-file "d1-git-test")))
      (unwind-protect
          (progn
            (set-visited-file-name temp-file t)
            (insert "line 1\n")
            (insert "line 2\n")
            (insert "line 3\n")
            (insert "line 4\n")
            (goto-char (point-min))
            (forward-line 1)  ;; Line 2
            (set-mark (point))
            (forward-line 2)  ;; Line 4
            (transient-mark-mode 1)
            (activate-mark)
            (let ((range (d1--git-get-line-range)))
              (should (equal '(2 . 4) range))))
        ;; Cleanup
        (set-buffer-modified-p nil)
        (kill-buffer)
        (delete-file temp-file)))))

(ert-deftest d1-git-permalink-test-get-line-range-no-file ()
  "Test getting line range in non-file buffer."
  (with-temp-buffer
    (insert "some text")
    (should (null (d1--git-get-line-range)))))

;;; Test Edge Cases

(ert-deftest d1-git-permalink-test-url-with-spaces ()
  "Test handling file paths with spaces.
Note: The current implementation does not URL-encode spaces in file paths."
  (should (equal "https://github.com/user/repo/blob/abc123/path/to/file with spaces.el"
                 (d1--build-file-url "github.com" "user/repo" "abc123" "path/to/file with spaces.el"))))

(ert-deftest d1-git-permalink-test-url-with-special-chars ()
  "Test handling file paths with special characters."
  (should (equal "https://github.com/user/repo/blob/abc123/path/to/file-name_v2.el"
                 (d1--build-file-url "github.com" "user/repo" "abc123" "path/to/file-name_v2.el"))))

(ert-deftest d1-git-permalink-test-same-line-range ()
  "Test handling line ranges where start equals end."
  (should (equal "#L10"
                 (d1--format-line-anchor-default '(10 . 10))))
  (should (equal "#L1"
                 (d1--format-line-anchor-default '(1 . 1)))))

;;; Test Helper Functions

(ert-deftest d1-git-permalink-test-format-line-anchor-with-format ()
  "Test the generic line anchor formatter."
  ;; Test nil
  (should (equal ""
                 (d1--format-line-anchor-with-format nil "L%d-L%d")))
  
  ;; Test single line (as number)
  (should (equal "#L10"
                 (d1--format-line-anchor-with-format 10 "L%d-L%d")))
  
  ;; Test same line range
  (should (equal "#L10"
                 (d1--format-line-anchor-with-format '(10 . 10) "L%d-L%d")))
  
  ;; Test different line range formats
  (should (equal "#L10-L20"
                 (d1--format-line-anchor-with-format '(10 . 20) "L%d-L%d")))
  (should (equal "#L10-20"
                 (d1--format-line-anchor-with-format '(10 . 20) "L%d-%d"))))

(provide 'd1-git-permalink-test)
;;; d1-git-permalink-test.el ends here
