;;; -*- lexical-binding: t -*-

(use-package magit
  :commands (magit magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-delta
  :hook magit-mode)

(provide 'init-magit)
