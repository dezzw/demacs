;;; -*- lexical-binding: t -*-

(use-package tempel
  :disabled
  :straight t
  :bind (:map tempel-map
              ("TAB" . tempel-next)
              ("S-<tab>" . tempel-previous))
  :hook (((prog-mode text-mode) . +tempel-setup-capf)
         ((prog-mode text-mode) . tempel-abbrev-mode))
  :custom
  (tempel-trigger-prefix "\\")
  :config
  (defun +tempel-setup-capf ()
    (push #'tempel-complete completion-at-point-functions)))


(use-package tempel-collection
  :disabled
  :straight t
  :after tempel)

(provide 'init-snippets)
