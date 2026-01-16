;;; d1-ai.el --- AI coding agent integrations  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; Configuration for AI coding agents and assistants.
;; Provides integrations for tools like Claude Code, Gemini CLI, etc.
;;

;;; Code:

;; Agent Shell - Native agentic integrations for Claude Code, Gemini CLI, etc.
;; Provides: Interactive shell buffers for AI coding agents
;; Requires: shell-maker, acp
(use-package agent-shell
  :commands (agent-shell
             agent-shell-new-shell
             agent-shell-toggle
             agent-shell-prompt-compose))

(provide 'd1-ai)
;;; d1-ai.el ends here
