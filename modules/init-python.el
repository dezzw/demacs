;;; init-python.el --- python related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(use-package python
  :after python-ts-mode
  :custom
  (python-indent-guess-indent-offset nil)
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4))


(provide 'init-python)
;;; init-python.el ends here
