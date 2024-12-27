;;; Copilot Configuration
;; Copilot for Emacs integrates GitHub Copilot, an AI-powered code completion tool.
;; It provides context-aware suggestions as you type, helping you write code faster.
(use-package copilot
  :straight (:host github
             :repo "copilot-emacs/copilot.el"
             :files ("*.el")) ;; Install from the official GitHub repository
  :init (copilot-mode)
  :bind
  (:map copilot-completion-map
        ("<tab>"   . copilot-accept-completion)           ;; Accept the current suggestion
        ("TAB"     . copilot-accept-completion)           ;; Accept the current suggestion (alternative binding)
        ("C-TAB"   . copilot-accept-completion-by-word)   ;; Accept the suggestion word by word
        ("C-<tab>" . copilot-accept-completion-by-word))) ;; Same as above, alternate binding

;;; GPTel Configuration
;; GPTel enables interaction with Large Language Models (LLMs) directly within Emacs.
;; It supports ChatGPT and other LLM services, allowing AI-assisted conversations.
(use-package gptel
  :config
  ;; Set the GPT model to use
  (setopt gptel-model 'claude-3-5-sonnet-20241022)

  ;; Adding the Anthropic API key to the macOS Keychain
  ;;
  ;; This configuration fetches the Anthropic API key from the macOS Keychain.
  ;; To add the key to the keychain:
  ;; 1. Open the Terminal.
  ;; 2. Run the following command:
  ;;
  ;;    security add-internet-password -a "Bearer" -s "https://api.anthropic.com" -w "<YOUR_API_KEY>"
  ;;
  ;; Replace `<YOUR_API_KEY>` with your actual Anthropic API key.
  ;; Note: This is the same password used by the Zed editor for accessing the Anthropic API.

  ;; Function to retrieve the API key from the macOS Keychain
  (defun get-anthropic-password ()
    "Retrieve the Anthropic API key from the macOS Keychain."
    (let ((entry (nth 0 (auth-source-search
                         :host "https://api.anthropic.com"
                         :user "Bearer"
                         :max 1))))
      (when entry
        (funcall (plist-get entry :secret)))))

  ;; Configure gptel to use the key retrieved from the macOS Keychain
  (setopt gptel-backend
          (gptel-make-anthropic "Claude"
            :stream t
            :key (or (get-anthropic-password)
                     (error "Anthropic API key not found in macOS Keychain")))))

(provide 'my-llm)
