;;; -*- lexical-binding: t -*-

(use-package rust-mode
  :straight t
  :mode ("\\.rs\\'" . rust-mode))

(use-package cargo
  :straight t
  :hook (rust-mode . cargo-minor-mode))


(provide 'init-rust)
