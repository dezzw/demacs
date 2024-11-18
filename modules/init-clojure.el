;;; init-clojure.el --- clojure related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(use-package clojure-mode
  :straight t
  :mode ("\\.clj\\'" "\\.cljs\\'" "\\.cljc\\'")
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
  :straight t
  :after clojure-mode
  :hook (cider-repl-mode . rainbow-delimiters-mode)
  :custom
  (cider-eldoc-display-for-symbol-at-point nil)
  (cider-font-lock-dynamically nil))

(use-package babashka
  :straight t
  :commands (babashka-command))

(use-package neil
  :straight t
  :commands (neil-find-clojure-package)
  :config
  (setq neil-prompt-for-version-p nil
        neil-inject-dep-to-project-p t))

(use-package jet
  :straight t
  :commands (jet))

(provide 'init-clojure)
;;; init-clojure.el ends here
