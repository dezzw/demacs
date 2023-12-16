;;; -*- lexical-binding: t -*-

(straight-use-package 'auctex)
(with-eval-after-load 'auctex
  (defun remove-tex-trash()
    (interactive)
    (let ((current-directory default-directory)
	  (extensions '("\\.log\\'" "\\.out\\'" "\\.aux\\'")))
      (dolist (ext extensions)
	(dolist (file (directory-files current-directory nil ext))
	  (delete-file (concat current-directory file)))))))

(use-package cdlatex
  :hook
  ((Tex-latex-mode . #'turn-on-cdlatex)
   (org-mode . org-cdlatex-mode)))

(use-package pdf-tools
  :straight nil
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(provide 'init-tex)
