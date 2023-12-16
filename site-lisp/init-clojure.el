
(use-package clojure-mode
  :hook (((clojure-mode clojurescript-mode) . rainbow-delimiters-mode)
	 ((clojure-mode clojurescript-mode) . hs-minor-mode)
	 ((clojure-mode clojurescript-mode) . aggressive-indent-mode)))

(use-package cider)
  ;; :hook (cider-repl-mode . rainbow-delimiters-mode))

(use-package neil
  :commands (neil-find-clojure-package)
  :config
  (setq neil-prompt-for-version-p nil
        neil-inject-dep-to-project-p t))

(use-package jet
  :commands (jet))

(provide 'init-clojure)
