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
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :bind (("C-c a" . aidermacs-transient-menu))
  :custom
  (aidermacs-args '("--model" "ollama_chat/deepseek-r1:8b"))
  (aidermacs-backend 'vterm)
  :config
  (aidermacs-setup-minor-mode)
  (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434"))

(use-package gptel
  :straight t
  :config
  (gptel-make-ollama "Ollama"             ;Any name of your choosing
    :host "localhost:11434"               ;Where it's running
    :stream t                             ;Stream responses
    :models '(deepseek-r1:8b)))          ;List of models

(provide 'init-ai)
;;; init-ai.el ends here
