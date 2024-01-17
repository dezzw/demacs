;;; -*- lexical-binding: t; -*-

(use-package python
  :straight nil
  :custom
  (python-indent-guess-indent-offset nil)
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4))

(use-package jupyter
  :commands (jupyter-run-repl jupyter-connect-repl))

(use-package ein
  :commands (ein:run ein:login)
  :custom
  (ein:output-area-inlined-image t))

(provide 'init-python)
