;;; -*- lexical-binding: t -*-

(use-package envrc
  :hook (after-init . dw/maybe-enable-envrc-global-mode)
  :config
  (defun dw/maybe-enable-envrc-global-mode ()
    "Enable `envrc-global-mode' if `direnv' is installed."
    (when (executable-find "direnv")
      (envrc-global-mode)))

  (with-eval-after-load 'envrc
    (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)))

(provide 'init-direnv)
