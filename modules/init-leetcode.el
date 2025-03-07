;;; init-leetcode.el --- leetcode related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package leetcode
  :straight '(:type git :host github :repo "ginqi7/leetcode-emacs")
  :commands (leetcode-list-all)
  :custom
  (leetcode-language "java"))

(use-package ctable
  :straight t)

(provide 'init-leetcode)
;;; init-leetcode.el ends here
