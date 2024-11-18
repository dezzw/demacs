;;; -*- lexical-binding: t -*-

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :custom
  (markdown-command "multimarkdown"))

(provide 'init-markdown)
