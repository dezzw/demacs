;;; -*- lexical-binding: t -*-

(use-package lua-ts-mode
  :straight nil
  :mode "\\.lua\\'"
  :config
  (let* ((root (project-root (project-current)))
	 (dev-dir (expand-file-name (concat root ".devenv/profile/")))
	 (bin-path (concat dev-dir "bin/lua-language-server"))
	 (main-path (concat dev-dir "share/lua-language-server/main.lua")))
    (setq lsp-clients-lua-language-server-bin bin-path)
    (setq lsp-clients-lua-language-server-main-location main-path)))

(use-package fennel-mode
  :straight t
  :mode "\\.fnl\\'")

(provide 'init-lua)
