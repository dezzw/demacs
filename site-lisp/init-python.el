;;; -*- lexical-binding: t; -*-
(with-eval-after-load 'python-mode
  (with-eval-after-load 'lsp-bridge-mode
      (electric-indent-local-mode t)))

(provide 'init-python)
