;;; -*- lexical-binding: t -*-

;; (add-hook 'c-ts-mode-hook 'lsp)
;; (add-hook 'c++-ts-mode-hook 'lsp)

;; (when (featurep 'lsp-mode)
;;   (with-eval-after-load 'lsp-mode
;;     (setq lsp-clients-clangd-args '("--header-insertion-decorators=0"
;;                                   "--clang-tidy"
;;                                   "--enable-config"))))

(provide 'init-cc)
