;;; -*- lexical-binding: t -*-

(use-package tex
  :straight auctex
  ;; :mode ("\\.tex\\'" . TeX-tex-mode)
  :custom
  (TeX-parse-self t) ; parse on load
  (TeX-auto-save t)  ; parse on save
  ;; Use hidden directories for AUCTeX files.
  (TeX-auto-local ".auctex-auto")
  (TeX-style-local ".auctex-style")
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctex
			       )      ;; Don't start the Emacs server when correlating sources.
  (TeX-clean-confirm nil)
  (TeX-source-correlate-start-server nil)
  ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
  (TeX-electric-sub-and-superscript t)
  ;; Just save, don't ask before each compilation.
  (TeX-save-query nil)

  :config
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
  (when (featurep :system 'macos)
    ;; PDF Tools isn't in `TeX-view-program-list-builtin' on macs.
    (add-to-list 'TeX-view-program-list '("PDF Tools" TeX-pdf-tools-sync-view)))

  (defun dw/clean-latex-files ()
    "Clean auxiliary LaTeX files and the .auctex-auto folder."
    (interactive)
  (TeX-clean) ;; Clean TeX files
  (let ((auto-dir (concat (file-name-directory buffer-file-name) ".auctex-auto")))
    (when (file-directory-p auto-dir)
      (delete-directory auto-dir t))))
  
  (defun dw/auto-compile-tex ()
    "Automatically compile TeX file after saving."
    (when (eq major-mode 'LaTeX-mode)
      (TeX-command "LaTeX" 'TeX-master-file)))

  ;; Add the function to the after-save-hook
  (add-hook 'after-save-hook (lambda ()
			       (dw/auto-compile-tex)
			       (dw/clean-latex-files))))


(use-package cdlatex
  :hook (LaTeX-mode . cdlatex-mode)
  :hook (org-mode . org-cdlatex-mode)
  :config
  ;; Use \( ... \) instead of $ ... $.
  (setq cdlatex-use-dollar-to-ensure-math nil))

(use-package pdf-tools
  :straight nil
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :commands
  (TeX-pdf-tools-sync-view)
  :config
  ;; Update PDF buffers after successful LaTeX runs.
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package saveplace-pdf-view
  :straight t
  :config
  (save-place-mode 1))

(provide 'init-tex)
