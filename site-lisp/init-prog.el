;;; -*- lexical-binding: t -*-

(use-package elec-pair
  :straight nil
  :config (electric-pair-mode))

(use-package electric
  :straight nil
  :config (electric-indent-mode))

(use-package rainbow-mode)

(use-package rainbow-delimiters)

(use-package aggressive-indent)

(use-package vdiff
  :commands (vdiff-buffer))

(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)))

(provide 'init-prog)
