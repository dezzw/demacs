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
  :hook dired-mode
  :bind (:map dired-mode-map
	      ("." . dired-hide-dotfiles-mode)))


(use-package dirvish
  :straight '(dirvish :type git :repo "hlissner/dirvish" :depth full)
  :hook (dirvish-setup . dirvish-emerge-mode)
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "~/UTM/"                       "UTM")
     ("t" "~/Desktop/Test/"             "Language Test")))
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (vc-info yank index)))
  (setq dirvish-attributes
        '(nerd-icons vc-state git-msg))
  (setq delete-by-moving-to-trash t))
  

(provide 'init-dired)
