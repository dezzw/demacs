;;; init-tools.el --- useful tools -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(use-package atomic-chrome
  :straight t
  :defer t
  :config
  (atomic-chrome-start-server))


(provide 'init-tools)
;;; init-tools.el ends here
