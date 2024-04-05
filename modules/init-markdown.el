;;; -*- lexical-binding: t -*-

(use-package markdown-mode
  :mode "\\.md\\'"
  :custom
  (markdown-command "multimarkdown"))

(provide 'init-markdown)
