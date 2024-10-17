;;; package --- ai-client configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package aider
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  (setq aider-args '("--model" "ollama/deepseek-coder-v2"))
  (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434")
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

(provide 'init-ai)
;;; init-ai.el ends here
