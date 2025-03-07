;;; init-lspbridge.el --- lsp bridge related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package yasnippet
  :straight t
  :after lsp-bridge
  :config
  (use-package yasnippet-snippets :straight t)
  (yas-reload-all))

;; lsp-bridge
(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
			 :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
			 :build (:not compile))
  :defer 0.5
  :custom
  (lsp-bridge-python-command "python-for-lsp-bridge")
  
  (lsp-bridge-enable-completion-in-minibuffer t)
  (lsp-bridge-enable-with-tramp t)
  ;; ui configuration
  (lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
  (lsp-bridge-signature-show-with-frame-position "top-right")
  (lsp-bridge-enable-mode-line nil)
  (lsp-bridge-enable-hover-diagnostic t)
  (lsp-bridge-enable-inlay-hint t)
  (lsp-bridge-enable-hover-diagnostic t)
  (lsp-bridge-enable-org-babel t)

  ;; acm configuration
  (acm-candidate-match-function 'orderless-flex)
  (acm-backend-yas-match-by-trigger-keyword t)
  (acm-enable-capf t)
  (acm-enable-tabnine t)

  ;;lsp-server configuartion
  (lsp-bridge-nix-lsp-server 'nixd)
  ;; (acm-enable-tempel t)
  ;; (acm-enable-codeium t)
  ;; (acm-enable-citre t)
  ;; :bind (:map acm-mode-map
  ;; 	      ("C-n" . acm-select-next)
  ;; 	      ("C-p" . acm-select-prev))
  :config
  (add-to-list 'acm-backend-capf-mode-list 'clojure-mode)
  (add-to-list 'acm-backend-capf-mode-list 'clojurescript-mode)
  
  (setq lsp-bridge-get-project-path-by-filepath
	(lambda (filepath)
          (or (when-let* ((project (project-current nil (file-name-directory filepath)))
                          (root (project-root project)))
		(expand-file-name root))
              (file-name-directory filepath))))

  (yas-global-mode 1)
  (global-lsp-bridge-mode))

;; (use-package flymake-bridge
;;   :straight '(:type git :host github :repo "liuyinz/flymake-bridge")
;;   :hook (lsp-bridge-mode . flymake-bridge-setup))


(provide 'init-lspbridge)
;;; init-lspbridge.el ends here
