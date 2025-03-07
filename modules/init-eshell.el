;;; init-eshell.el --- eshell related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package eshell
  ;; :ensure nil
  :commands (eshell)
  :config
  (setq eshell-directory-name (concat user-emacs-directory "eshell/"))
  
  (if (executable-find "exa")
      (defalias 'eshell/ls 'exa)))

(use-package eshell-prompt-extras
  :straight t
  :after esh-opt
  :config
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package eshell-up
  :straight t
  :after esh-mode
  :custom
  (eshell-up-ignore-case nil))

(use-package eshell-syntax-highlighting
  :straight t
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-z
  :straight t
  :after esh-mode)

(use-package esh-help
  :straight t
  :after esh-mode
  :config
  (setup-esh-help-eldoc))



(use-package eat
  :straight '(eat :type git
		  :host codeberg
		  :repo "akib/emacs-eat"
		  :files ("*.el" ("term" "term/*.el") "*.texi"
			  "*.ti" ("terminfo/e" "terminfo/e/*")
			  ("terminfo/65" "terminfo/65/*")
			  ("integration" "integration/*")
			  (:exclude ".dir-locals.el" "*-tests.el")))
  :commands (eat eshell)
  :config
  ;; For `eat-eshell-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-mode)

  ;; For `eat-eshell-visual-command-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(provide 'init-eshell)
;;; init-eshell.el ends here
