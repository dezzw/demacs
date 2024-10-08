;;; -*- lexical-binding: t -*-

(use-package yasnippet
  :straight t
  :config
  (use-package yasnippet-snippets :straight t)
  (yas-reload-all))

;; lsp-bridge
(use-package lsp-bridge
  :straight nil
  :custom
  (lsp-bridge-enable-completion-in-minibuffer t)
  (lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
  (lsp-bridge-signature-show-with-frame-position "top-right")
  (lsp-bridge-enable-with-tramp t)
  (lsp-bridge-enable-hover-diagnostic t)

  (acm-candidate-match-function 'orderless-flex)
  (acm-backend-yas-match-by-trigger-keyword t)
  (acm-enable-capf t)
  (acm-enable-tabnine t)
  ;; (acm-enable-tempel t)
  ;; (acm-enable-codeium t)
  ;; (acm-enable-citre t)
  (lsp-bridge-enable-mode-line nil)
  ;; :bind (:map acm-mode-map
  ;; 	      ("C-n" . acm-select-next)
  ;; 	      ("C-p" . acm-select-prev))
  :config
  (yas-global-mode 1)
  (global-lsp-bridge-mode)

  (setq lsp-bridge-get-project-path-by-filepath
	(lambda (filepath)
          (when (locate-dominating-file filepath ".envrc")
	    (expand-file-name (locate-dominating-file filepath ".envrc"))))))

(provide 'init-lspbridge)
