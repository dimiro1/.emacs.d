;;; d1-homebrew-test.el --- Tests for d1-homebrew.el -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Tests for d1-homebrew.el
;; Uses cl-letf to mock shell commands and processes.
;;

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'd1-homebrew)

;;; Test Package Validation

(ert-deftest d1-homebrew-test-valid-package-p-valid ()
  "Test validation of valid package plists."
  (should (d1--homebrew-valid-package-p '(:name "Git" :formula "git")))
  (should (d1--homebrew-valid-package-p '(:name "Ghostty" :formula "ghostty" :cask t)))
  (should (d1--homebrew-valid-package-p '(:name "Custom" :formula "pkg" :tap "user/repo")))
  (should (d1--homebrew-valid-package-p '(:name "Full" :formula "pkg" :cask t :tap "user/repo" :command "brew install special"))))

(ert-deftest d1-homebrew-test-valid-package-p-invalid ()
  "Test validation of invalid package plists."
  ;; Missing :name
  (should-not (d1--homebrew-valid-package-p '(:formula "git")))
  ;; Missing :formula
  (should-not (d1--homebrew-valid-package-p '(:name "Git")))
  ;; Empty :name
  (should-not (d1--homebrew-valid-package-p '(:name "" :formula "git")))
  ;; Empty :formula
  (should-not (d1--homebrew-valid-package-p '(:name "Git" :formula "")))
  ;; Not a list
  (should-not (d1--homebrew-valid-package-p "not a list"))
  ;; nil
  (should-not (d1--homebrew-valid-package-p nil))
  ;; Non-string values
  (should-not (d1--homebrew-valid-package-p '(:name 123 :formula "git")))
  (should-not (d1--homebrew-valid-package-p '(:name "Git" :formula 456))))

;;; Test Status Management

(ert-deftest d1-homebrew-test-status-set-and-get ()
  "Test setting and getting package status."
  (let ((d1--homebrew-package-states (make-hash-table :test 'equal)))
    ;; Default status should be pending
    (should (eq 'pending (d1--homebrew-get-status "nonexistent")))
    ;; Set and retrieve various statuses
    (d1--homebrew-set-status "git" 'installed)
    (should (eq 'installed (d1--homebrew-get-status "git")))
    (d1--homebrew-set-status "ripgrep" 'checking)
    (should (eq 'checking (d1--homebrew-get-status "ripgrep")))
    (d1--homebrew-set-status "failed-pkg" 'failed)
    (should (eq 'failed (d1--homebrew-get-status "failed-pkg")))))

;;; Test Status String Formatting

(ert-deftest d1-homebrew-test-status-string ()
  "Test status string generation with faces."
  (should (equal "pending" (substring-no-properties (d1--homebrew-status-string 'pending))))
  (should (equal "checking" (substring-no-properties (d1--homebrew-status-string 'checking))))
  (should (equal "installing" (substring-no-properties (d1--homebrew-status-string 'installing))))
  (should (equal "installed" (substring-no-properties (d1--homebrew-status-string 'installed))))
  (should (equal "FAILED" (substring-no-properties (d1--homebrew-status-string 'failed))))
  (should (equal "unknown" (substring-no-properties (d1--homebrew-status-string 'invalid-status)))))

(ert-deftest d1-homebrew-test-status-string-faces ()
  "Test that status strings have correct faces."
  (should (eq 'd1-homebrew-pending (get-text-property 0 'face (d1--homebrew-status-string 'pending))))
  (should (eq 'd1-homebrew-installed (get-text-property 0 'face (d1--homebrew-status-string 'installed))))
  (should (eq 'd1-homebrew-failed (get-text-property 0 'face (d1--homebrew-status-string 'failed)))))

;;; Test Homebrew Check Functions (with mocking)

(ert-deftest d1-homebrew-test-installed-p-formula-installed ()
  "Test checking installed formula."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "git")))
    (should (d1--homebrew-installed-p "git"))))

(ert-deftest d1-homebrew-test-installed-p-formula-not-installed ()
  "Test checking non-installed formula."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "")))
    (should-not (d1--homebrew-installed-p "nonexistent"))))

(ert-deftest d1-homebrew-test-installed-p-cask-installed ()
  "Test checking installed cask."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (cmd)
               (if (string-match-p "--cask" cmd)
                   "ghostty"
                 ""))))
    (should (d1--homebrew-installed-p "ghostty" t))))

(ert-deftest d1-homebrew-test-installed-p-cask-not-installed ()
  "Test checking non-installed cask."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "   \n  ")))
    (should-not (d1--homebrew-installed-p "nonexistent" t))))

(ert-deftest d1-homebrew-test-tap-exists-p-exists ()
  "Test checking existing tap."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "yes")))
    (should (d1--homebrew-tap-exists-p "homebrew/cask"))))

(ert-deftest d1-homebrew-test-tap-exists-p-not-exists ()
  "Test checking non-existing tap."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "")))
    (should-not (d1--homebrew-tap-exists-p "nonexistent/tap"))))

;;; Test Build Entries

(ert-deftest d1-homebrew-test-build-entries-basic ()
  "Test building entries for tabulated list."
  (let ((d1-homebrew-packages '((:name "Git" :formula "git")
                                (:name "Ripgrep" :formula "ripgrep")))
        (d1--homebrew-package-states (make-hash-table :test 'equal)))
    (d1--homebrew-set-status "git" 'installed)
    (d1--homebrew-set-status "ripgrep" 'pending)
    (let ((entries (d1--homebrew-build-entries)))
      (should (= 2 (length entries)))
      ;; Check first entry
      (should (equal "git" (car (nth 0 entries))))
      (should (equal "Git" (aref (cadr (nth 0 entries)) 0)))
      ;; Check second entry
      (should (equal "ripgrep" (car (nth 1 entries))))
      (should (equal "Ripgrep" (aref (cadr (nth 1 entries)) 0))))))

(ert-deftest d1-homebrew-test-build-entries-with-cask ()
  "Test building entries for cask packages."
  (let ((d1-homebrew-packages '((:name "Ghostty" :formula "ghostty" :cask t)))
        (d1--homebrew-package-states (make-hash-table :test 'equal)))
    (let ((entries (d1--homebrew-build-entries)))
      (should (= 1 (length entries)))
      ;; Check formula display includes (cask)
      (should (string-match-p "(cask)" (aref (cadr (nth 0 entries)) 2))))))

(ert-deftest d1-homebrew-test-build-entries-with-tap ()
  "Test building entries for packages with taps."
  (let ((d1-homebrew-packages '((:name "Custom" :formula "pkg" :tap "user/repo")))
        (d1--homebrew-package-states (make-hash-table :test 'equal)))
    (let ((entries (d1--homebrew-build-entries)))
      (should (= 1 (length entries)))
      ;; Check formula display includes tap
      (should (string-match-p "\\[user/repo\\]" (aref (cadr (nth 0 entries)) 2))))))

(ert-deftest d1-homebrew-test-build-entries-filters-invalid ()
  "Test that invalid packages are filtered out."
  (let ((d1-homebrew-packages '((:name "Valid" :formula "valid")
                                (:name "" :formula "empty-name")
                                (:formula "no-name")
                                (:name "No Formula")
                                (:name "Also Valid" :formula "also-valid")))
        (d1--homebrew-package-states (make-hash-table :test 'equal)))
    (let ((entries (d1--homebrew-build-entries)))
      ;; Only 2 valid packages should be included
      (should (= 2 (length entries)))
      (should (equal "valid" (car (nth 0 entries))))
      (should (equal "also-valid" (car (nth 1 entries)))))))

;;; Test Shell Quote Argument Usage

(ert-deftest d1-homebrew-test-shell-quote-argument-in-installed-p ()
  "Test that shell-quote-argument is used in installed-p."
  (let ((quoted-formula nil))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (cmd)
                 (setq quoted-formula cmd)
                 "")))
      (d1--homebrew-installed-p "test;rm -rf /")
      ;; The dangerous characters should be quoted (escaped with backslash)
      (should (string-match-p "test" quoted-formula))
      ;; Semicolon should be escaped (preceded by backslash)
      (should (string-match-p "\\\\;" quoted-formula)))))

;;; Test Installation Queue

(ert-deftest d1-homebrew-test-install-queue-management ()
  "Test installation queue state management."
  (let ((d1--homebrew-install-queue nil)
        (d1--homebrew-current-install nil))
    ;; Initially empty
    (should (null d1--homebrew-install-queue))
    (should (null d1--homebrew-current-install))
    ;; Add to queue
    (let ((pkg '(:name "Test" :formula "test")))
      (add-to-list 'd1--homebrew-install-queue pkg t)
      (should (= 1 (length d1--homebrew-install-queue)))
      ;; Pop from queue
      (let ((next (pop d1--homebrew-install-queue)))
        (should (equal pkg next))
        (should (null d1--homebrew-install-queue))))))

;;; Test Face Definitions

(ert-deftest d1-homebrew-test-faces-defined ()
  "Test that all required faces are defined."
  (should (facep 'd1-homebrew-pending))
  (should (facep 'd1-homebrew-checking))
  (should (facep 'd1-homebrew-installing))
  (should (facep 'd1-homebrew-installed))
  (should (facep 'd1-homebrew-failed)))

;;; Test Mode Setup

(ert-deftest d1-homebrew-test-mode-keymap ()
  "Test that mode keymap has expected bindings."
  (should (eq 'd1-homebrew-install-at-point (lookup-key d1-homebrew-mode-map (kbd "RET"))))
  (should (eq 'd1-homebrew-install-all-pending (lookup-key d1-homebrew-mode-map (kbd "i"))))
  (should (eq 'd1-homebrew-info-at-point (lookup-key d1-homebrew-mode-map (kbd "I"))))
  (should (eq 'd1-homebrew-refresh-status (lookup-key d1-homebrew-mode-map (kbd "r"))))
  (should (eq 'revert-buffer (lookup-key d1-homebrew-mode-map (kbd "g"))))
  (should (eq 'quit-window (lookup-key d1-homebrew-mode-map (kbd "q")))))

(provide 'd1-homebrew-test)
;;; d1-homebrew-test.el ends here
