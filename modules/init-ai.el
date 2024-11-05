;;; package --- ai-client configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package aider
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :bind (("C-c a" . aider-transient-menu))
  :config
  (setq aider-args '("--model" "ollama/deepseek-coder-v2"))
  (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434"))

(provide 'init-ai)
;;; init-ai.el ends here
