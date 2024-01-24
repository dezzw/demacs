;;; -*- lexical-binding: t -*-

(use-package rust-mode
  :straight t)

(use-package cargo
  :straight t
  :hook (rust-mode . cargo-minor-mode))

(use-package rust-playground
  :straight t)

(provide 'init-rust)
