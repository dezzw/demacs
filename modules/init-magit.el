;;; -*- lexical-binding: t -*-

(use-package magit
  :straight t
  :commands (magit magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-delta
  :straight t
  :hook magit-mode)

(provide 'init-magit)
