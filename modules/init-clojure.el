
(use-package clojure-mode
  :hook ((clojure-mode clojurescript-mode clojurec-mode) .
	 (lambda ()
	   (progn
	     (rainbow-delimiters-mode)
	     (hs-minor-mode)
	     (aggressive-indent-mode)))))
	     ;; (setq-local completion-at-point-functions
	     ;; 		 (list (cape-capf-super #'cider-complete-at-point #'lsp-completion-at-point)))
	     ;; (lsp-deferred)))))

(use-package cider
  :hook (cider-repl-mode . rainbow-delimiters-mode)
  :custom
  (cider-eldoc-display-for-symbol-at-point nil)
  (cider-font-lock-dynamically nil))

(use-package babashka
  :commands (babashka-command))

(use-package neil
  :commands (neil-find-clojure-package)
  :config
  (setq neil-prompt-for-version-p nil
        neil-inject-dep-to-project-p t))

(use-package jet
  :commands (jet))

(provide 'init-clojure)
