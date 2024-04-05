;;; -*- lexical-binding: t; -*-
(use-package treesit
  :straight nil
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :mode (("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode)
	 ("\\.go\\'" . go-ts-mode)
	 ("/go\\.mod\\'" . go-mod-ts-mode)
	 ("\\.rs\\'" . rust-ts-mode)
	 ("\\.ts\\'" . typescript-ts-mode)
	 ("\\.tsx\\'" . tsx-ts-mode)
	 ("\\.y[a]?ml\\'" . yaml-ts-mode)
	 ("\\.zig\\'" . zig-ts-mode))
  :config (setq treesit-font-lock-level 4)
  :init
  (setq major-mode-remap-alist
	'((sh-mode         . bash-ts-mode)
	  (c-mode          . c-ts-mode)
	  (c++-mode        . c++-ts-mode)
	  (c-or-c++-mode   . c-or-c++-ts-mode)
	  (css-mode        . css-ts-mode)
	  (js-mode         . js-ts-mode)
	  (java-mode       . java-ts-mode)
	  (js-json-mode    . json-ts-mode)
	  (makefile-mode   . cmake-ts-mode)
	  (python-mode     . python-ts-mode)
	  (ruby-mode       . ruby-ts-mode)
	  (conf-toml-mode  . toml-ts-mode))))

(provide 'init-treesit)
