;;; -*- lexical-binding: t; -*-
 (setup python-mode
   (:with-mode python-ts-mode
     (:hook electric-indent-local-mode))
   (:hook electric-indent-local-mode))

(provide 'init-python)
