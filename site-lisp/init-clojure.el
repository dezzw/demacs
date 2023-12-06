
(use-package clojure-mode
  :hook (clojure-mode . rainbow-delimiters-mode))

(use-package cider-mode)
  ;; :hook (cider-repl-mode . rainbow-delimiters-mode))

(use-package neil
  :commands (neil-find-clojure-package)
  :config
  (setq neil-prompt-for-version-p nil
        neil-inject-dep-to-project-p t))

(use-package jet
  :commands (jet))

(provide 'init-clojure)
