;;; package --- jupyter related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(use-package jupyter
  :straight t
  :commands (jupyter-run-repl jupyter-connect-repl))

(use-package ein
  :straight t
  :commands (ein:run ein:login)
  :custom
  (ein:output-area-inlined-image t))

(provide 'init-jupyter)
;;; init-jupyter.el ends here
