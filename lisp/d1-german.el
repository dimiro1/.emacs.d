;;; d1-german.el --- German language utilities using Ollama  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; Uses Ollama to provide German language utilities:
;; - Check grammatical gender of German nouns (der/die/das)
;; - Show verb conjugations across major tenses
;; - Display declension, examples, and translations
;;
;; Usage:
;;   M-x d1-german-gender or M-x d1-german-gender-at-point
;;   M-x d1-german-verb or M-x d1-german-verb-at-point
;;

;;; Code:

;;; Configuration

(defgroup d1-german nil
  "German language utilities using Ollama."
  :group 'tools
  :prefix "d1-german-")

(defcustom d1-german-model "gpt-oss"
  "Ollama model to use for German language queries."
  :type 'string
  :group 'd1-german)

(defcustom d1-german-url "http://localhost:11434"
  "Ollama API base URL."
  :type 'string
  :group 'd1-german)

(defcustom d1-german-debug nil
  "When non-nil, log raw AI responses to debug buffer."
  :type 'boolean
  :group 'd1-german)

(defvar d1--german-buffer-name "*German Gender*"
  "Name of the buffer for German gender results.")

(defvar d1--german-verb-buffer-name "*German Verb*"
  "Name of the buffer for German verb conjugation results.")

(defvar d1--german-debug-buffer-name "*German Debug*"
  "Name of the buffer for debug output.")


;;; Faces

;; Gender-related faces
(defface d1-german-gender-masculine
  '((((class color) (background dark))
     :foreground "#6aa4ff" :weight bold)
    (((class color) (background light))
     :foreground "#0055cc" :weight bold))
  "Face for masculine gender (der)."
  :group 'd1-german)

(defface d1-german-gender-feminine
  '((((class color) (background dark))
     :foreground "#ff7eb3" :weight bold)
    (((class color) (background light))
     :foreground "#cc0066" :weight bold))
  "Face for feminine gender (die)."
  :group 'd1-german)

(defface d1-german-gender-neuter
  '((((class color) (background dark))
     :foreground "#f0c674" :weight bold)
    (((class color) (background light))
     :foreground "#996600" :weight bold))
  "Face for neuter gender (das)."
  :group 'd1-german)

(defface d1-german-gender-word
  '((t :height 2.0 :weight bold))
  "Face for the word being looked up."
  :group 'd1-german)

(defface d1-german-gender-entry
  '((t :height 1.4))
  "Face for singular/plural entries."
  :group 'd1-german)

(defface d1-german-gender-label
  '((((class color) (background dark))
     :foreground "#888888")
    (((class color) (background light))
     :foreground "#666666"))
  "Face for labels like 'Singular:', 'Plural:'."
  :group 'd1-german)

(defface d1-german-gender-translation
  '((((class color) (background dark))
     :foreground "#a9a9a9" :slant italic)
    (((class color) (background light))
     :foreground "#555555" :slant italic))
  "Face for English translation."
  :group 'd1-german)

;; Verb conjugation faces
(defface d1-german-verb-infinitive
  '((((class color) (background dark))
     :foreground "#50fa7b" :height 1.8 :weight bold)
    (((class color) (background light))
     :foreground "#228b22" :height 1.8 :weight bold))
  "Face for verb infinitive form."
  :group 'd1-german)

(defface d1-german-verb-tense-header
  '((((class color) (background dark))
     :foreground "#8be9fd" :height 1.3 :weight bold)
    (((class color) (background light))
     :foreground "#1e90ff" :height 1.3 :weight bold))
  "Face for tense headers (Präsens, Präteritum, etc.)."
  :group 'd1-german)

(defface d1-german-verb-pronoun
  '((((class color) (background dark))
     :foreground "#888888" :weight normal)
    (((class color) (background light))
     :foreground "#666666" :weight normal))
  "Face for pronouns (ich, du, er/sie/es, etc.)."
  :group 'd1-german)

(defface d1-german-verb-form
  '((t :height 1.1 :weight normal))
  "Face for conjugated verb forms."
  :group 'd1-german)

(defface d1-german-verb-auxiliary
  '((((class color) (background dark))
     :foreground "#bd93f9" :slant italic)
    (((class color) (background light))
     :foreground "#9370db" :slant italic))
  "Face for auxiliary verbs (haben/sein in Perfekt)."
  :group 'd1-german)


;;; Helper Functions

(defun d1--german-http-get (endpoint)
  "Make a synchronous GET request to ENDPOINT.
Returns parsed JSON as alist, or nil on error."
  (condition-case nil
      (let ((buffer (url-retrieve-synchronously
                     (concat d1-german-url endpoint)
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

(defun d1--german-http-post (endpoint data callback)
  "Make an async POST request to ENDPOINT with DATA.
CALLBACK is called with parsed JSON response or nil on error."
  (let* ((url (concat d1-german-url endpoint))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode data) 'utf-8)))
    (url-retrieve
     url
     (lambda (status)
       (let ((response nil))
         (unwind-protect
             (progn
               (unless (plist-get status :error)
                 (goto-char url-http-end-of-headers)
                 (condition-case nil
                     (setq response (json-parse-string
                                     (buffer-substring-no-properties (point) (point-max))
                                     :object-type 'alist))
                   (error nil)))
               (funcall callback response))
           (kill-buffer (current-buffer)))))
     nil t)))

(defun d1--german-ollama-running-p ()
  "Check if Ollama server is running."
  (not (null (d1--german-http-get "/api/tags"))))

(defun d1--german-get-installed-models ()
  "Get list of installed Ollama models."
  (let ((response (d1--german-http-get "/api/tags")))
    (when response
      (mapcar (lambda (m) (cdr (assoc 'name m)))
              (cdr (assoc 'models response))))))

(defun d1--german-model-installed-p (model)
  "Check if MODEL is installed in Ollama."
  (let ((installed (d1--german-get-installed-models)))
    (or (member model installed)
        (member (concat model ":latest") installed))))

(defun d1--german-ensure-model ()
  "Ensure Ollama is running and the configured model is available.
Returns t if ready, nil otherwise."
  (cond
   ((not (d1--german-ollama-running-p))
    (message "Ollama não está rodando. Inicie com: ollama serve")
    nil)
   ((not (d1--german-model-installed-p d1-german-model))
    (message "Modelo '%s' não encontrado. Instale com: ollama pull %s"
             d1-german-model d1-german-model)
    nil)
   (t t)))

(defun d1--german-request (prompt callback)
  "Send PROMPT to Ollama asynchronously.
CALLBACK is called with the response text when complete, or nil on error."
  (d1--german-http-post
   "/api/generate"
   `((model . ,d1-german-model)
     (prompt . ,prompt)
     (stream . :json-false)
     (context . []))
   (lambda (response)
     (let ((result (cdr (assoc 'response response)))
           (error-msg (cdr (assoc 'error response))))
       (cond
        (error-msg (message "Erro do Ollama: %s" error-msg) (funcall callback nil))
        (result (funcall callback result))
        (t (message "Erro: Sem resposta do Ollama") (funcall callback nil)))))))

(defun d1--german-parse-json (response)
  "Parse JSON from RESPONSE string.
Returns alist with keys: corrected, article, singular, plural, english, portuguese, explanation, error."
  (condition-case err
      (let* ((json-str (if (string-match "```json?\\s-*\n?\\([^`]+\\)```" response)
                           (match-string 1 response)
                         ;; Try to extract JSON object - look for first { to last }
                         (if (string-match "\\({.*}\\)" response)
                             (match-string 1 response)
                           response)))
             (cleaned (string-trim json-str)))
        (json-parse-string cleaned :object-type 'alist))
    (error 
     (when d1-german-debug
       (message "JSON parse error: %S" err))
     nil)))

(defun d1--german-article-to-gender (article)
  "Convert ARTICLE string to gender symbol."
  (pcase (downcase (string-trim article))
    ("der" 'masculine)
    ("die" 'feminine)
    ("das" 'neuter)
    (_ nil)))

(defun d1--german-face-for-gender (gender)
  "Return the face for GENDER symbol."
  (pcase gender
    ('masculine 'd1-german-gender-masculine)
    ('feminine 'd1-german-gender-feminine)
    ('neuter 'd1-german-gender-neuter)
    (_ 'default)))

(defun d1--german-display-gender (word response)
  "Display RESPONSE for WORD in a dedicated buffer."
  (when d1-german-debug
    (with-current-buffer (get-buffer-create d1--german-debug-buffer-name)
      (erase-buffer)
      (insert response)
      (goto-char (point-min))))
  (let* ((data (d1--german-parse-json response))
         (article (cdr (assoc 'article data)))
         (singular (cdr (assoc 'singular data)))
         (plural (cdr (assoc 'plural data)))
         (nominative (cdr (assoc 'nominative data)))
         (nominative-example (cdr (assoc 'nominative_example data)))
         (nominative-example-pt (cdr (assoc 'nominative_example_pt data)))
         (accusative (cdr (assoc 'accusative data)))
         (accusative-example (cdr (assoc 'accusative_example data)))
         (accusative-example-pt (cdr (assoc 'accusative_example_pt data)))
         (dative (cdr (assoc 'dative data)))
         (dative-example (cdr (assoc 'dative_example data)))
         (dative-example-pt (cdr (assoc 'dative_example_pt data)))
         (genitive (cdr (assoc 'genitive data)))
         (genitive-example (cdr (assoc 'genitive_example data)))
         (genitive-example-pt (cdr (assoc 'genitive_example_pt data)))
         (english (cdr (assoc 'english data)))
         (portuguese (cdr (assoc 'portuguese data)))
         (explanation (cdr (assoc 'explanation data)))
         (corrected-raw (cdr (assoc 'corrected data)))
         (corrected (when (stringp corrected-raw) corrected-raw))
         (error-raw (cdr (assoc 'error data)))
         (error-msg (when (stringp error-raw) error-raw))
         (gender (when article (d1--german-article-to-gender article)))
         (face (d1--german-face-for-gender gender))
         (display-word (or corrected word))
         (buf (get-buffer-create d1--german-buffer-name)))
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
            (insert (or error-msg "Não foi possível processar a resposta. Ative o modo debug para ver a saída completa.")))
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
            (insert "\n")
            (insert (propertize "  Nominativ " 'face 'd1-german-gender-label))
            (insert (propertize "(Quem faz a ação?)\n" 'face 'shadow))
            (insert (propertize "  " 'face 'd1-german-gender-label))
            (insert (propertize nominative 'face 'd1-german-gender-entry))
            (insert "\n")
            (when nominative-example
              (insert (propertize "    " 'face 'd1-german-gender-label))
              (insert (propertize nominative-example 'face 'shadow))
              (insert "\n")
              (when nominative-example-pt
                (insert (propertize "    " 'face 'd1-german-gender-label))
                (insert (propertize nominative-example-pt 'face '(d1-german-gender-translation)))
                (insert "\n"))))
          (when accusative
            (insert "\n")
            (insert (propertize "  Akkusativ " 'face 'd1-german-gender-label))
            (insert (propertize "(O quê? / Quem sofre a ação?)\n" 'face 'shadow))
            (insert (propertize "  " 'face 'd1-german-gender-label))
            (insert (propertize accusative 'face 'd1-german-gender-entry))
            (insert "\n")
            (when accusative-example
              (insert (propertize "    " 'face 'd1-german-gender-label))
              (insert (propertize accusative-example 'face 'shadow))
              (insert "\n")
              (when accusative-example-pt
                (insert (propertize "    " 'face 'd1-german-gender-label))
                (insert (propertize accusative-example-pt 'face '(d1-german-gender-translation)))
                (insert "\n"))))
          (when dative
            (insert "\n")
            (insert (propertize "  Dativ     " 'face 'd1-german-gender-label))
            (insert (propertize "(Pra quem? / A quem?)\n" 'face 'shadow))
            (insert (propertize "  " 'face 'd1-german-gender-label))
            (insert (propertize dative 'face 'd1-german-gender-entry))
            (insert "\n")
            (when dative-example
              (insert (propertize "    " 'face 'd1-german-gender-label))
              (insert (propertize dative-example 'face 'shadow))
              (insert "\n")
              (when dative-example-pt
                (insert (propertize "    " 'face 'd1-german-gender-label))
                (insert (propertize dative-example-pt 'face '(d1-german-gender-translation)))
                (insert "\n"))))
          (when genitive
            (insert "\n")
            (insert (propertize "  Genitiv   " 'face 'd1-german-gender-label))
            (insert (propertize "(De quem? / Posse)\n" 'face 'shadow))
            (insert (propertize "  " 'face 'd1-german-gender-label))
            (insert (propertize genitive 'face 'd1-german-gender-entry))
            (insert "\n")
            (when genitive-example
              (insert (propertize "    " 'face 'd1-german-gender-label))
              (insert (propertize genitive-example 'face 'shadow))
              (insert "\n")
              (when genitive-example-pt
                (insert (propertize "    " 'face 'd1-german-gender-label))
                (insert (propertize genitive-example-pt 'face '(d1-german-gender-translation)))
                (insert "\n"))))
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

(defun d1--german-display-verb (verb response)
  "Display conjugation RESPONSE for VERB in a dedicated buffer."
  (when d1-german-debug
    (with-current-buffer (get-buffer-create d1--german-debug-buffer-name)
      (erase-buffer)
      (insert response)
      (goto-char (point-min))))
  (let* ((data (d1--german-parse-json response))
         (infinitive (cdr (assoc 'infinitive data)))
         (verb-type (cdr (assoc 'verb_type data)))
         (english (cdr (assoc 'english data)))
         (portuguese (cdr (assoc 'portuguese data)))
         (präsens (cdr (assoc 'präsens data)))
         (präsens-example (cdr (assoc 'präsens_example data)))
         (präsens-example-pt (cdr (assoc 'präsens_example_pt data)))
         (präteritum (cdr (assoc 'präteritum data)))
         (präteritum-example (cdr (assoc 'präteritum_example data)))
         (präteritum-example-pt (cdr (assoc 'präteritum_example_pt data)))
         (perfekt (cdr (assoc 'perfekt data)))
         (perfekt-example (cdr (assoc 'perfekt_example data)))
         (perfekt-example-pt (cdr (assoc 'perfekt_example_pt data)))
         (futur (cdr (assoc 'futur data)))
         (futur-example (cdr (assoc 'futur_example data)))
         (futur-example-pt (cdr (assoc 'futur_example_pt data)))
         (explanation (cdr (assoc 'explanation data)))
         (error-raw (cdr (assoc 'error data)))
         (error-msg (when (stringp error-raw) error-raw))
         (buf (get-buffer-create d1--german-verb-buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (if (or error-msg (not data))
          ;; Error or invalid response
          (progn
            (insert (propertize verb 'face 'd1-german-verb-infinitive))
            (insert "\n")
            (insert (make-string (length verb) ?─))
            (insert "\n\n")
            (insert (or error-msg "Não foi possível processar a resposta. Ative o modo debug para ver a saída completa.")))
        ;; Valid response - display formatted
        (insert (propertize (or infinitive verb) 'face 'd1-german-verb-infinitive))
        (insert "\n")
        (insert (propertize (make-string (length (or infinitive verb)) ?─) 'face 'd1-german-verb-infinitive))
        (insert "\n\n")
        ;; Verb type and translations
        (when verb-type
          (insert (propertize "Tipo      " 'face 'd1-german-gender-label))
          (insert (propertize verb-type 'face 'shadow))
          (insert "\n"))
        (insert (propertize "English   " 'face 'd1-german-gender-label))
        (insert (propertize (or english "—") 'face 'd1-german-gender-translation))
        (insert "\n")
        (insert (propertize "Português " 'face 'd1-german-gender-label))
        (insert (propertize (or portuguese "—") 'face 'd1-german-gender-translation))
        (insert "\n\n")
        ;; Präsens
        (when präsens
          (insert (propertize "Präsens (Presente)\n" 'face 'd1-german-verb-tense-header))
          (insert (propertize "  ich         " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'ich präsens)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  du          " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'du präsens)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  er/sie/es   " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'er_sie_es präsens)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  wir         " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'wir präsens)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  ihr         " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'ihr präsens)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  sie/Sie     " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'sie_Sie präsens)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (when präsens-example
            (insert (propertize "  → " 'face 'd1-german-gender-label))
            (insert (propertize präsens-example 'face 'shadow))
            (insert "\n")
            (when präsens-example-pt
              (insert (propertize "    " 'face 'd1-german-gender-label))
              (insert (propertize präsens-example-pt 'face 'd1-german-gender-translation))
              (insert "\n")))
          (insert "\n"))
        ;; Präteritum
        (when präteritum
          (insert (propertize "Präteritum (Passado Simples)\n" 'face 'd1-german-verb-tense-header))
          (insert (propertize "  ich         " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'ich präteritum)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  du          " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'du präteritum)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  er/sie/es   " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'er_sie_es präteritum)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  wir         " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'wir präteritum)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  ihr         " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'ihr präteritum)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  sie/Sie     " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'sie_Sie präteritum)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (when präteritum-example
            (insert (propertize "  → " 'face 'd1-german-gender-label))
            (insert (propertize präteritum-example 'face 'shadow))
            (insert "\n")
            (when präteritum-example-pt
              (insert (propertize "    " 'face 'd1-german-gender-label))
              (insert (propertize präteritum-example-pt 'face 'd1-german-gender-translation))
              (insert "\n")))
          (insert "\n"))
        ;; Perfekt
        (when perfekt
          (insert (propertize "Perfekt (Pretérito Perfeito)\n" 'face 'd1-german-verb-tense-header))
          (let ((auxiliary (cdr (assoc 'auxiliary perfekt)))
                (partizip (cdr (assoc 'partizip perfekt))))
            (when (and auxiliary partizip)
              (insert (propertize "  Auxiliar: " 'face 'd1-german-gender-label))
              (insert (propertize auxiliary 'face 'd1-german-verb-auxiliary))
              (insert (propertize " + " 'face 'd1-german-gender-label))
              (insert (propertize partizip 'face 'd1-german-verb-form))
              (insert "\n\n")))
          (insert (propertize "  ich         " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'ich perfekt)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  du          " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'du perfekt)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  er/sie/es   " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'er_sie_es perfekt)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  wir         " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'wir perfekt)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  ihr         " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'ihr perfekt)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  sie/Sie     " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'sie_Sie perfekt)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (when perfekt-example
            (insert (propertize "  → " 'face 'd1-german-gender-label))
            (insert (propertize perfekt-example 'face 'shadow))
            (insert "\n")
            (when perfekt-example-pt
              (insert (propertize "    " 'face 'd1-german-gender-label))
              (insert (propertize perfekt-example-pt 'face 'd1-german-gender-translation))
              (insert "\n")))
          (insert "\n"))
        ;; Futur
        (when futur
          (insert (propertize "Futur I (Futuro)\n" 'face 'd1-german-verb-tense-header))
          (insert (propertize "  ich         " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'ich futur)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  du          " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'du futur)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  er/sie/es   " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'er_sie_es futur)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  wir         " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'wir futur)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  ihr         " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'ihr futur)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (insert (propertize "  sie/Sie     " 'face 'd1-german-verb-pronoun))
          (insert (propertize (or (cdr (assoc 'sie_Sie futur)) "—") 'face 'd1-german-verb-form))
          (insert "\n")
          (when futur-example
            (insert (propertize "  → " 'face 'd1-german-gender-label))
            (insert (propertize futur-example 'face 'shadow))
            (insert "\n")
            (when futur-example-pt
              (insert (propertize "    " 'face 'd1-german-gender-label))
              (insert (propertize futur-example-pt 'face 'd1-german-gender-translation))
              (insert "\n")))
          (insert "\n"))
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
  (when (d1--german-ensure-model)
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
  \"nominative_example_pt\": \"Brazilian Portuguese translation of example\",
  \"accusative\": \"article + word\",
  \"accusative_example\": \"short example sentence\",
  \"accusative_example_pt\": \"Brazilian Portuguese translation of example\",
  \"dative\": \"article + word\",
  \"dative_example\": \"short example sentence\",
  \"dative_example_pt\": \"Brazilian Portuguese translation of example\",
  \"genitive\": \"article + word\",
  \"genitive_example\": \"short example sentence\",
  \"genitive_example_pt\": \"Brazilian Portuguese translation of example\",
  \"english\": \"English translation\",
  \"portuguese\": \"Brazilian Portuguese translation\",
  \"explanation\": \"Brief explanation of why this German word has this gender, written in Brazilian Portuguese\"
}

If the word is not a German noun, reply: {\"error\": \"reason in Brazilian Portuguese\"}"
                          word)))
      (message "Verificando gênero de '%s'..." word)
      (d1--german-request prompt
                          (lambda (response)
                            (if response
                                (d1--german-display-gender word response)
                              (message "Nenhuma resposta recebida")))))))

(defun d1-german-gender-at-point ()
  "Check the grammatical gender of the German word at point.
If no word at point, prompts for input."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (if word
        (d1-german-gender word)
      (call-interactively #'d1-german-gender))))

(defun d1-german-verb (verb)
  "Show conjugation of German VERB across major tenses.
Displays Präsens, Präteritum, Perfekt, and Futur I."
  (interactive "sGerman verb: ")
  (when (d1--german-ensure-model)
    (let ((prompt (format "You are a German language expert. Conjugate the verb: \"%s\"

Reply with ONLY this JSON (no markdown, no explanation outside JSON):
{
  \"infinitive\": \"verb in infinitive form\",
  \"verb_type\": \"regular or irregular\",
  \"english\": \"English translation with 'to'\",
  \"portuguese\": \"Brazilian Portuguese translation\",
  \"präsens\": {
    \"ich\": \"conjugated form\",
    \"du\": \"conjugated form\",
    \"er_sie_es\": \"conjugated form\",
    \"wir\": \"conjugated form\",
    \"ihr\": \"conjugated form\",
    \"sie_Sie\": \"conjugated form\"
  },
  \"präsens_example\": \"short example sentence in Präsens\",
  \"präsens_example_pt\": \"Brazilian Portuguese translation\",
  \"präteritum\": {
    \"ich\": \"conjugated form\",
    \"du\": \"conjugated form\",
    \"er_sie_es\": \"conjugated form\",
    \"wir\": \"conjugated form\",
    \"ihr\": \"conjugated form\",
    \"sie_Sie\": \"conjugated form\"
  },
  \"präteritum_example\": \"short example sentence in Präteritum\",
  \"präteritum_example_pt\": \"Brazilian Portuguese translation\",
  \"perfekt\": {
    \"auxiliary\": \"haben or sein\",
    \"partizip\": \"past participle\",
    \"ich\": \"auxiliary + partizip\",
    \"du\": \"auxiliary + partizip\",
    \"er_sie_es\": \"auxiliary + partizip\",
    \"wir\": \"auxiliary + partizip\",
    \"ihr\": \"auxiliary + partizip\",
    \"sie_Sie\": \"auxiliary + partizip\"
  },
  \"perfekt_example\": \"short example sentence in Perfekt\",
  \"perfekt_example_pt\": \"Brazilian Portuguese translation\",
  \"futur\": {
    \"ich\": \"werde + infinitive\",
    \"du\": \"wirst + infinitive\",
    \"er_sie_es\": \"wird + infinitive\",
    \"wir\": \"werden + infinitive\",
    \"ihr\": \"werdet + infinitive\",
    \"sie_Sie\": \"werden + infinitive\"
  },
  \"futur_example\": \"short example sentence in Futur\",
  \"futur_example_pt\": \"Brazilian Portuguese translation\",
  \"explanation\": \"Brief explanation about the verb usage and conjugation patterns in Brazilian Portuguese\"
}

If not a valid German verb, reply: {\"error\": \"reason in Brazilian Portuguese\"}"
                          verb)))
      (message "Verificando conjugação de '%s'..." verb)
      (d1--german-request prompt
                          (lambda (response)
                            (if response
                                (d1--german-display-verb verb response)
                              (message "Nenhuma resposta recebida")))))))

(defun d1-german-verb-at-point ()
  "Show conjugation of German verb at point.
If no word at point, prompts for input."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (if word
        (d1-german-verb word)
      (call-interactively #'d1-german-verb))))

(provide 'd1-german)
;;; d1-german.el ends here
