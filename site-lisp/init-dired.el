;;; -*- lexical-binding: t -*-

(use-package dired
  :straight nil
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alGh")
  (dired-recursive-copies 'always)
  (dired-mouse-drag-files t)                   ; added in Emacs 29
  (mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  (dired-kill-when-opening-new-dired-buffer t)
  (delete-by-moving-to-trash t))

(use-package dired-single
  :commands (dired-single-buffer dired-single-up-directory))

(use-package dired-hide-dotfiles
  :hook dired-mode)


(use-package dirvish
  :commands (dirvish)
  :config
  (dirvish-override-dired-mode))

(provide 'init-dired)
