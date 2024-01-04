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
  :mode (("\\.http\\'" . restclient-mode)))

(provide 'init-prog)
