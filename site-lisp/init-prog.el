;;; -*- lexical-binding: t -*-

(use-package elec-pair
  :ensure nil
  :config (electric-pair-mode))

(use-package electric
  :ensure nil
  :config (electric-indent-mode))

(use-package rainbow-mode)

(use-package rainbow-delimiters)

(use-package aggressive-indent)

(use-package vdiff
  :commands (vdiff-buffer))

(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)))

;;; lsp config
;; (require 'init-lspbridge)
(require 'init-eglot)

;;; try out both developing documents
(use-package devdocs
  :straight t
  :commands (devdocs-lookup))

(use-package dash-at-point
  :straight t
  :commands (dash-at-point))

(provide 'init-prog)
