;;; d1-german-gender.el --- Check German word gender using Ollama  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; Uses Ollama to determine the grammatical gender of German words.
;; Displays the article (der/die/das), plural form, and explanation.
;;
;; Usage: M-x d1-german-gender or M-x d1-german-gender-at-point
;;

;;; Code:

;;; Configuration

(defgroup d1-german-gender nil
  "German word gender lookup using Ollama."
  :group 'tools
  :prefix "d1-german-gender-")

(defcustom d1-german-gender-model "gpt-oss"
  "Ollama model to use for German gender queries."
  :type 'string
  :group 'd1-german-gender)

(defcustom d1-german-gender-url "http://localhost:11434"
  "Ollama API base URL."
  :type 'string
  :group 'd1-german-gender)

(defcustom d1-german-gender-debug nil
  "When non-nil, log raw AI responses to debug buffer."
  :type 'boolean
  :group 'd1-german-gender)

(defvar d1--german-gender-buffer-name "*German Gender*"
  "Name of the buffer for German gender results.")

(defvar d1--german-gender-debug-buffer-name "*German Gender Debug*"
  "Name of the buffer for debug output.")


;;; Faces

(defface d1-german-gender-masculine
  '((((class color) (background dark))
     :foreground "#6aa4ff" :weight bold)
    (((class color) (background light))
     :foreground "#0055cc" :weight bold))
  "Face for masculine gender (der)."
  :group 'd1-german-gender)

(defface d1-german-gender-feminine
  '((((class color) (background dark))
     :foreground "#ff7eb3" :weight bold)
    (((class color) (background light))
     :foreground "#cc0066" :weight bold))
  "Face for feminine gender (die)."
  :group 'd1-german-gender)

(defface d1-german-gender-neuter
  '((((class color) (background dark))
     :foreground "#f0c674" :weight bold)
    (((class color) (background light))
     :foreground "#996600" :weight bold))
  "Face for neuter gender (das)."
  :group 'd1-german-gender)

(defface d1-german-gender-word
  '((t :height 2.0 :weight bold))
  "Face for the word being looked up."
  :group 'd1-german-gender)

(defface d1-german-gender-entry
  '((t :height 1.4))
  "Face for singular/plural entries."
  :group 'd1-german-gender)

(defface d1-german-gender-label
  '((((class color) (background dark))
     :foreground "#888888")
    (((class color) (background light))
     :foreground "#666666"))
  "Face for labels like 'Singular:', 'Plural:'."
  :group 'd1-german-gender)

(defface d1-german-gender-translation
  '((((class color) (background dark))
     :foreground "#a9a9a9" :slant italic)
    (((class color) (background light))
     :foreground "#555555" :slant italic))
  "Face for English translation."
  :group 'd1-german-gender)


;;; Helper Functions

(defun d1--german-gender-http-get (endpoint)
  "Make a synchronous GET request to ENDPOINT.
Returns parsed JSON as alist, or nil on error."
  (condition-case nil
      (let ((buffer (url-retrieve-synchronously
                     (concat d1-german-gender-url endpoint)
                     t t 5)))
        (when buffer
          (unwind-protect
              (with-current-buffer buffer
                (goto-char url-http-end-of-headers)
                (json-parse-string
                 (buffer-substring-no-properties (point) (point-max))
                 :object-type 'alist))
            (kill-buffer buffer))))
    (error nil)))

(defun d1--german-gender-http-post (endpoint data callback)
  "Make an async POST request to ENDPOINT with DATA.
CALLBACK is called with parsed JSON response or nil on error."
  (let* ((url (concat d1-german-gender-url endpoint))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode data) 'utf-8)))
    (url-retrieve
     url
     (lambda (status)
       (let ((response nil))
         (unless (plist-get status :error)
           (goto-char url-http-end-of-headers)
           (condition-case nil
               (setq response (json-parse-string
                               (buffer-substring-no-properties (point) (point-max))
                               :object-type 'alist))
             (error nil)))
         (kill-buffer (current-buffer))
         (funcall callback response)))
     nil t)))

(defun d1--german-gender-ollama-running-p ()
  "Check if Ollama server is running."
  (not (null (d1--german-gender-http-get "/api/tags"))))

(defun d1--german-gender-get-installed-models ()
  "Get list of installed Ollama models."
  (let ((response (d1--german-gender-http-get "/api/tags")))
    (when response
      (mapcar (lambda (m) (cdr (assoc 'name m)))
              (cdr (assoc 'models response))))))

(defun d1--german-gender-model-installed-p (model)
  "Check if MODEL is installed in Ollama."
  (let ((installed (d1--german-gender-get-installed-models)))
    (or (member model installed)
        (member (concat model ":latest") installed))))

(defun d1--german-gender-ensure-model ()
  "Ensure Ollama is running and the configured model is available.
Returns t if ready, nil otherwise."
  (cond
   ((not (d1--german-gender-ollama-running-p))
    (message "Ollama is not running. Start it with: ollama serve")
    nil)
   ((not (d1--german-gender-model-installed-p d1-german-gender-model))
    (message "Model '%s' not found. Install it with: ollama pull %s"
             d1-german-gender-model d1-german-gender-model)
    nil)
   (t t)))

(defun d1--german-gender-request (prompt callback)
  "Send PROMPT to Ollama asynchronously.
CALLBACK is called with the response text when complete, or nil on error."
  (d1--german-gender-http-post
   "/api/generate"
   `((model . ,d1-german-gender-model)
     (prompt . ,prompt)
     (stream . :json-false)
     (context . []))
   (lambda (response)
     (let ((result (cdr (assoc 'response response)))
           (error-msg (cdr (assoc 'error response))))
       (cond
        (error-msg (message "Ollama error: %s" error-msg) (funcall callback nil))
        (result (funcall callback result))
        (t (message "Error: No response from Ollama") (funcall callback nil)))))))

(defun d1--german-gender-parse-json (response)
  "Parse JSON from RESPONSE string.
Returns alist with keys: corrected, article, singular, plural, english, portuguese, explanation, error."
  (condition-case nil
      (let* ((json-str (if (string-match "```json?\\s-*\n?\\([^`]+\\)```" response)
                           (match-string 1 response)
                         (if (string-match "\\({[^}]+}\\)" response)
                             (match-string 1 response)
                           response)))
             (cleaned (string-trim json-str)))
        (json-parse-string cleaned :object-type 'alist))
    (error nil)))

(defun d1--german-gender-article-to-gender (article)
  "Convert ARTICLE string to gender symbol."
  (pcase (downcase (string-trim article))
    ("der" 'masculine)
    ("die" 'feminine)
    ("das" 'neuter)
    (_ nil)))

(defun d1--german-gender-face-for-gender (gender)
  "Return the face for GENDER symbol."
  (pcase gender
    ('masculine 'd1-german-gender-masculine)
    ('feminine 'd1-german-gender-feminine)
    ('neuter 'd1-german-gender-neuter)
    (_ 'default)))

(defun d1--german-gender-display (word response)
  "Display RESPONSE for WORD in a dedicated buffer."
  (when d1-german-gender-debug
    (with-current-buffer (get-buffer-create d1--german-gender-debug-buffer-name)
      (erase-buffer)
      (insert response)
      (goto-char (point-min))))
  (let* ((data (d1--german-gender-parse-json response))
         (article (cdr (assoc 'article data)))
         (singular (cdr (assoc 'singular data)))
         (plural (cdr (assoc 'plural data)))
         (nominative (cdr (assoc 'nominative data)))
         (nominative-example (cdr (assoc 'nominative_example data)))
         (accusative (cdr (assoc 'accusative data)))
         (accusative-example (cdr (assoc 'accusative_example data)))
         (dative (cdr (assoc 'dative data)))
         (dative-example (cdr (assoc 'dative_example data)))
         (genitive (cdr (assoc 'genitive data)))
         (genitive-example (cdr (assoc 'genitive_example data)))
         (english (cdr (assoc 'english data)))
         (portuguese (cdr (assoc 'portuguese data)))
         (explanation (cdr (assoc 'explanation data)))
         (corrected-raw (cdr (assoc 'corrected data)))
         (corrected (when (stringp corrected-raw) corrected-raw))
         (error-raw (cdr (assoc 'error data)))
         (error-msg (when (stringp error-raw) error-raw))
         (gender (when article (d1--german-gender-article-to-gender article)))
         (face (d1--german-gender-face-for-gender gender))
         (display-word (or corrected word))
         (buf (get-buffer-create d1--german-gender-buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (if (or error-msg (not data))
          ;; Error or invalid response
          (progn
            (insert (propertize word 'face 'd1-german-gender-word))
            (insert "\n")
            (insert (make-string (length word) ?─))
            (insert "\n\n")
            (insert (or error-msg "Could not parse response. Enable debug mode to see raw output.")))
        ;; Valid response - display formatted
        ;; Show correction if word was misspelled
        (when (and corrected (not (string= (downcase corrected) (downcase word))))
          (insert (propertize word 'face '(:strike-through t :inherit shadow)))
          (insert (propertize " → " 'face 'shadow)))
        (insert (propertize display-word 'face (list face 'd1-german-gender-word)))
        (insert "\n")
        (insert (propertize (make-string (length display-word) ?─) 'face face))
        (insert "\n\n")
        ;; Singular
        (insert (propertize "Singular  " 'face '(d1-german-gender-label d1-german-gender-entry)))
        (insert (propertize (or article "?") 'face (list face 'd1-german-gender-entry)))
        (insert (propertize " " 'face 'd1-german-gender-entry))
        (insert (propertize (or singular word) 'face 'd1-german-gender-entry))
        (insert "\n")
        ;; Plural
        (insert (propertize "Plural    " 'face '(d1-german-gender-label d1-german-gender-entry)))
        (insert (propertize "die " 'face 'd1-german-gender-entry))
        (insert (propertize (or plural "—") 'face 'd1-german-gender-entry))
        (insert "\n\n")
        ;; Cases
        (when (or nominative accusative dative genitive)
          (insert (propertize "Cases\n" 'face '(d1-german-gender-label d1-german-gender-entry)))
          (when nominative
            (insert (propertize "  Nominativ  " 'face 'd1-german-gender-label))
            (insert (propertize nominative 'face 'd1-german-gender-entry))
            (insert "\n")
            (when nominative-example
              (insert (propertize "             " 'face 'd1-german-gender-label))
              (insert (propertize nominative-example 'face 'shadow))
              (insert "\n")))
          (when accusative
            (insert (propertize "  Akkusativ  " 'face 'd1-german-gender-label))
            (insert (propertize accusative 'face 'd1-german-gender-entry))
            (insert "\n")
            (when accusative-example
              (insert (propertize "             " 'face 'd1-german-gender-label))
              (insert (propertize accusative-example 'face 'shadow))
              (insert "\n")))
          (when dative
            (insert (propertize "  Dativ      " 'face 'd1-german-gender-label))
            (insert (propertize dative 'face 'd1-german-gender-entry))
            (insert "\n")
            (when dative-example
              (insert (propertize "             " 'face 'd1-german-gender-label))
              (insert (propertize dative-example 'face 'shadow))
              (insert "\n")))
          (when genitive
            (insert (propertize "  Genitiv    " 'face 'd1-german-gender-label))
            (insert (propertize genitive 'face 'd1-german-gender-entry))
            (insert "\n")
            (when genitive-example
              (insert (propertize "             " 'face 'd1-german-gender-label))
              (insert (propertize genitive-example 'face 'shadow))
              (insert "\n")))
          (insert "\n"))
        ;; Translations
        (insert (propertize "English   " 'face '(d1-german-gender-label d1-german-gender-entry)))
        (insert (propertize (or english "—") 'face '(d1-german-gender-translation d1-german-gender-entry)))
        (insert "\n")
        (insert (propertize "Português " 'face '(d1-german-gender-label d1-german-gender-entry)))
        (insert (propertize (or portuguese "—") 'face '(d1-german-gender-translation d1-german-gender-entry)))
        (insert "\n\n")
        ;; Explanation
        (when explanation
          (insert (propertize explanation 'face 'shadow))))
      (insert "\n")
      (goto-char (point-min))
      (read-only-mode 1)
      (local-set-key (kbd "q") 'quit-window))
    (display-buffer buf '(display-buffer-pop-up-window))))


;;; Public Commands

(defun d1-german-gender (word)
  "Check the grammatical gender of German WORD.
Displays singular, plural, English translation, and explanation."
  (interactive "sGerman word: ")
  (when (d1--german-gender-ensure-model)
    (let ((prompt (format "You are a German language expert. Determine the grammatical gender of: \"%s\"

If the word is misspelled, correct it and include the correction.

Reply with ONLY this JSON (no markdown, no explanation outside JSON):
{
  \"corrected\": \"corrected spelling if misspelled, or null if correct\",
  \"article\": \"der or die or das\",
  \"singular\": \"word in singular form\",
  \"plural\": \"word in plural form\",
  \"nominative\": \"article + word\",
  \"nominative_example\": \"short example sentence\",
  \"accusative\": \"article + word\",
  \"accusative_example\": \"short example sentence\",
  \"dative\": \"article + word\",
  \"dative_example\": \"short example sentence\",
  \"genitive\": \"article + word\",
  \"genitive_example\": \"short example sentence\",
  \"english\": \"English translation\",
  \"portuguese\": \"Brazilian Portuguese translation\",
  \"explanation\": \"Brief explanation of the gender pattern\"
}

If the word is not a German noun, reply: {\"error\": \"reason\"}"
                          word)))
      (message "Checking gender of '%s'..." word)
      (d1--german-gender-request prompt
                                 (lambda (response)
                                   (if response
                                       (d1--german-gender-display word response)
                                     (message "No response received")))))))

(defun d1-german-gender-at-point ()
  "Check the grammatical gender of the German word at point.
If no word at point, prompts for input."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (if word
        (d1-german-gender word)
      (call-interactively #'d1-german-gender))))

(provide 'd1-german-gender)
;;; d1-german-gender.el ends here
