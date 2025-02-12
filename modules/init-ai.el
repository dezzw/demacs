;;; package --- ai-client configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (use-package aider
;;   :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
;;   :bind (("C-c a" . aider-transient-menu))
;;   :config
;;   (setq aider-args '("--model" "ollama_chat/deepseek-r1:8b"))
;;   (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434"))

(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs")
  :bind (("C-c a" . aidermacs-transient-menu))
  :custom
  (aidermacs-args '("--model" "ollama_chat/deepseek-r1:8b"))
  (aidermacs-backend 'vterm)
  :config
  (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434"))

(provide 'init-ai)
;;; init-ai.el ends here
