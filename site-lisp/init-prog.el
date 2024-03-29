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
  :mode (("\\.rest\\'" . restclient-mode)))

;;; try out both developing documents
(use-package devdocs
  :straight t
  :commands (devdocs-lookup))

(use-package dash-at-point
  :straight t
  :commands (dash-at-point))

(use-package compile
  :defer t
  :hook ((compilation-filter . ansi-color-compilation-filter))
  :bind (("C-x C-m" . recompile))
  :config
  (setopt compilation-scroll-output t)
  (setopt compilation-ask-about-save nil)
  (require 'ansi-color))

(provide 'init-prog)
