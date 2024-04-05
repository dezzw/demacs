;;; -*- lexical-binding: t -*-

(use-package tex
  :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :custom
  (TeX-parse-self t) ; parse on load
  (TeX-auto-save t)  ; parse on save
  ;; Use hidden directories for AUCTeX files.
  (TeX-auto-local ".auctex-auto")
  (TeX-style-local ".auctex-style")
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctex
			       )      ;; Don't start the Emacs server when correlating sources.
  (TeX-source-correlate-start-server nil)
  ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
  (TeX-electric-sub-and-superscript t)
  ;; Just save, don't ask before each compilation.
  (TeX-save-query nil)

  :config
  (with-eval-after-load 'auctex
    (defun remove-tex-trash()
      (interactive)
      (let ((current-directory default-directory)
	    (extensions '("\\.log\\'" "\\.out\\'" "\\.aux\\'")))
	(dolist (ext extensions)
	  (dolist (file (directory-files current-directory nil ext))
	    (delete-file (concat current-directory file))))))))

(use-package cdlatex
  :hook (LaTeX-mode . cdlatex-mode)
  :hook (org-mode . org-cdlatex-mode)
  :config
  ;; Use \( ... \) instead of $ ... $.
  (setq cdlatex-use-dollar-to-ensure-math nil))

(use-package pdf-tools
  :straight nil
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(provide 'init-tex)
