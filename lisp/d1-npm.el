;;; d1-npm.el --- Run npm scripts interactively  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; Simple npm script runner with project detection and marginalia support.
;; Features automatic package.json discovery, interactive script selection,
;; and optional argument passing.
;;
;; Usage: M-x d1-npm-run
;;        C-u M-x d1-npm-run (to add custom arguments)
;;

;;; Code:

;;; Internal Functions

(defun d1--npm-find-project-root (&optional project-path)
  "Returns the current project dir or PROJECT-PATH if provided."
  (if project-path
      (expand-file-name project-path)
    (when-let* ((project (project-current))
                (project-dir (project-root project)))
      project-dir)))

(defun d1--npm-find-package-json (&optional project-path)
  "Returns the package.json file path, optionally for PROJECT-PATH."
  (when-let* ((project-root (d1--npm-find-project-root project-path))
              (package-json-path (expand-file-name "package.json" project-root)))
    (when (file-exists-p package-json-path)
      package-json-path)))

(defun d1--npm-read-package-json (&optional project-path)
  "Read and parse the package.json file for current project or PROJECT-PATH."
  (when-let* ((package-json-path (d1--npm-find-package-json project-path)))
    (with-temp-buffer
      (insert-file-contents package-json-path)
      (goto-char (point-min))
      (json-parse-buffer :object-type 'alist))))

(defun d1--npm-get-scripts (&optional project-path)
  "Returns the scripts section from a package.json from the current project or PROJECT-PATH."
  (when-let* ((json-contents (d1--npm-read-package-json project-path))
			  (scripts (alist-get 'scripts json-contents)))
	scripts))

;;; Public API

;;;###autoload
(defun d1-npm-run (&optional script-args)
  "Run an npm script interactively.

Prompts for script selection from package.json in the current project.
With prefix argument SCRIPT-ARGS, prompts for additional arguments."
  (interactive "P")
  (when-let* ((scripts (d1--npm-get-scripts))
              (selected (completing-read "NPM Script: " scripts)))
    (let* ((args (when script-args
                   (read-string "Additional arguments: ")))
           (command (if (and args (not (string-empty-p args)))
                        (format "npm run %s -- %s" selected args)
                      (format "npm run %s" selected)))
		   (buffer-name (format "*npm run %s*" selected)))
      (async-shell-command command buffer-name)
      (message "Running: %s" command))))

;;; Marginalia Integration
;; Display npm script commands in minibuffer annotations

(defun d1-npm-annotate (cand)
  "Annotate npm script with its command CAND is the selected candidate."
  (when-let* ((scripts (d1--npm-get-scripts))
              (command (alist-get cand scripts nil nil #'string=)))
    (marginalia--fields
     ((marginalia--truncate command 80)
      :face 'marginalia-documentation))))

;; Optional marginalia configuration (add to your init.el):
;;
;; (with-eval-after-load 'marginalia
;;   (add-to-list 'marginalia-annotator-registry
;;                '(npm-script d1-npm-annotate builtin none))
;;   (add-to-list 'marginalia-command-categories
;;                '(d1-npm-run . npm-script)))

;;; Configuration Example
;; To use this package, add to your init.el:
;;
;; (require 'd1-npm)
;; (global-set-key (kbd "C-c n") 'd1-npm-run)
;;
;; For marginalia integration (optional):
;; (with-eval-after-load 'marginalia
;;   (add-to-list 'marginalia-annotator-registry
;;                '(npm-script d1-npm-annotate builtin none))
;;   (add-to-list 'marginalia-command-categories
;;                '(d1-npm-run . npm-script)))

(provide 'd1-npm)

;;; d1-npm.el ends here.
