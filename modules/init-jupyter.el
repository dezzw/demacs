;;; package --- jupyter related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(use-package jupyter
  :straight t
  :commands (jupyter-run-repl jupyter-connect-repl))

;; (use-package ein
;;   :straight t
;;   :commands (ein:run ein:login)
;;   :custom
;;   (ein:output-area-inlined-image t))

;; (use-package code-cells
;;   :straight t
;;   :hook (python-mode . code-cells-mode-maybe))

(use-package drepl
  :straight t)

(use-package ein
  :straight t
  :commands (ein:run ein:login)
  :custom
  (ein:output-area-inlined-images t)
  (ein:jupyter-server-use-subcommand "server"))

(use-package csv-mode
  :straight t
  :hook (csv-mode . csv-align-mode)
  :custom
  (csv-separators '("," ";" ":")))

(provide 'init-jupyter)
;;; init-jupyter.el ends here
