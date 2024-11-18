;;; -*- lexical-binding: t -*-

(use-package citre
  :straight t
  :bind (:map citre-mode-map
	 ("C-x c j" . citre-jump)
	 ("C-x c J" . citre-jump-back)
	 ("C-x c p" . citre-ace-peek)
	 ("C-x c u" . citre-update-this-tags-file))
  :config
  (require 'citre-config)
  (setq
   ;; citre-readtags-program "/etc/profiles/per-user/dez/bin/readtags"
   ;; citre-ctags-program "/etc/profiles/per-user/dez/bin/ctags"
   ;; Set this if you want to always use one location to create a tags file.
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t))

(provide 'init-ctags)
