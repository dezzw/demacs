;;; init-cc.el --- c-family related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package cmake-mode
  :straight t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(provide 'init-cc)
;;; init-cc.el ends here
