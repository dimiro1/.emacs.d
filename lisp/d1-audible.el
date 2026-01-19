;;; d1-audible.el --- Audible AAX to M4B conversion  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; Convert Audible AAX audiobook files to M4B format.
;; Activation bytes are fetched automatically from an online API.
;;

;;; Code:

(defun d1--aax-get-checksum (aax-file)
  "Get the checksum from AAX-FILE using ffprobe."
  (with-temp-buffer
    (call-process "ffprobe" nil '(t t) nil (expand-file-name aax-file))
    (goto-char (point-min))
    (when (re-search-forward "file checksum == \\([a-fA-F0-9]+\\)" nil t)
      (match-string 1))))

(defun d1--aax-get-activation-bytes (checksum callback)
  "Fetch activation bytes for CHECKSUM and call CALLBACK with result."
  (let ((url (format "https://aaxapiserverfunction20220831180001.azurewebsites.net/api/v2/Activation/%s?JonasIstCool=true" checksum)))
    (url-retrieve
     url
     (lambda (_status)
       (goto-char url-http-end-of-headers)
       (let* ((json-object-type 'alist)
              (response (json-read))
              (success (alist-get 'success response))
              (activation-bytes (alist-get 'activationBytes response)))
         (if success
             (funcall callback activation-bytes)
           (error "Failed to get activation bytes"))))
     nil t t)))

(defun d1--aax-convert (aax-file activation-bytes)
  "Convert AAX-FILE to M4B using ACTIVATION-BYTES."
  (let* ((output-file (concat (file-name-sans-extension aax-file) ".m4b"))
         (buf (get-buffer-create "*AAX Conversion*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "Converting: %s\n" (file-name-nondirectory aax-file)))
      (insert (format "Output: %s\n\n" (file-name-nondirectory output-file))))
    (display-buffer buf '(display-buffer-at-bottom (window-height . 10)))
    (make-process
     :name "aax-to-m4b"
     :buffer buf
     :command (list "ffmpeg" "-hide_banner" "-nostats" "-loglevel" "error"
                    "-y" "-activation_bytes" activation-bytes
                    "-i" (expand-file-name aax-file)
                    "-codec" "copy" (expand-file-name output-file))
     :sentinel (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-max))
                     (insert (if (= 0 (process-exit-status proc))
                                 (propertize "Conversion complete!\n" 'face 'success)
                               (propertize "Conversion failed!\n" 'face 'error)))))))))

;;;###autoload
(defun d1-convert-aax-to-m4b (aax-file)
  "Convert AAX-FILE to M4B format."
  (interactive "fAAX file: ")
  (unless (string-suffix-p ".aax" aax-file t)
    (user-error "File must have .aax extension"))
  (let ((checksum (d1--aax-get-checksum aax-file)))
    (unless checksum
      (user-error "Could not extract checksum from AAX file"))
    (message "Fetching activation bytes for checksum: %s" checksum)
    (d1--aax-get-activation-bytes
     checksum
     (lambda (activation-bytes)
       (d1--aax-convert aax-file activation-bytes)))))

;;;###autoload
(defun d1-dired-convert-aax-to-m4b ()
  "Convert marked AAX files or file at point to M4B in Dired."
  (interactive)
  (let ((files (dired-get-marked-files nil nil
                                       (lambda (f)
                                         (string-suffix-p ".aax" f t)))))
    (if (null files)
        (user-error "No AAX files selected")
      (dolist (file files)
        (d1-convert-aax-to-m4b file)))))

(provide 'd1-audible)
;;; d1-audible.el ends here
