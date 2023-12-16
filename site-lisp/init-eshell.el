;;; -*- lexical-binding: t -*-

(use-package eshell
  :straight nil
  :commands (eshell)
  :config
  (setq eshell-directory-name "~/.dotfiles/Emacs/eshell/")
  
  (if (executable-find "exa")
      (defalias 'eshell/ls 'exa)))

(use-package eshell-prompt-extras
  :after esh-opt
  :config
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package eshell-up
  :after esh-mode
  :custom
  (eshell-up-ignore-case nil))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-z
  :after esh-mode)

(use-package esh-help
  :after esh-mode
  :config
  (setup-esh-help-eldoc))

(provide 'init-eshell)
