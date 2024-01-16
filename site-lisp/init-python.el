;;; -*- lexical-binding: t; -*-

(use-package jupyter
  :commands (jupyter-run-repl jupyter-connect-repl))

(use-package ein
  :commands (ein:run ein:login)
  :custom
  (ein:output-area-inlined-image t))

(provide 'init-python)
