;;; -*- lexical-binding: t -*-
(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (use-package yasnippet-snippets)
  (yas-reload-all))

(use-package tempel)

(provide 'init-snippets)
