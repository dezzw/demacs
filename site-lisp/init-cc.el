;;; -*- lexical-binding: t -*-

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(when (featurep 'lsp-mode)
  (with-eval-after-load 'lsp-mode
      (setq lsp-clients-clangd-args '("--header-insertion=never"
                                      "--clang-tidy"
                                      "--enable-config"))))

(provide 'init-cc)
