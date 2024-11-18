;;; init-prog.el --- prog mode related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package elec-pair
  :straight nil
  :hook ((prog-mode conf-mode yaml-mode org-mode markdown-mode minibuffer-mode) . electric-pair-mode))

(use-package electric
  :straight nil
  :config (electric-indent-mode))

(use-package rainbow-mode
  :straight t)

(use-package rainbow-delimiters
  :straight t)

(use-package aggressive-indent
  :straight t)

(use-package vdiff
  :straight t
  :commands (vdiff-buffer))

(use-package restclient
  :straight t
  :mode (("\\.rest\\'" . restclient-mode)))

;;; try out both developing documents
(use-package devdocs
  :straight t
  :commands (devdocs-lookup))

(use-package dash-at-point
  :straight t
  :commands (dash-at-point))

(use-package compile
  :defer t
  :hook ((compilation-filter . ansi-color-compilation-filter))
  :bind (("C-x C-m" . recompile))
  :config
  (setopt compilation-scroll-output t)
  (setopt compilation-ask-about-save nil)
  (require 'ansi-color))

(use-package tabnine
  :straight t
  ;; :diminish "⌬"
  :defer t
  :custom
  (tabnine-wait 1)
  (tabnine-minimum-prefix-length 0)
  :hook (kill-emacs . tabnine-kill-process))
  ;; :config
  ;; (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
  ;; ;; (add-to-list 'nerd-icons-corfu-mapping `(tabnine ,(nerd-icons-codicon "nf-cod-hubot") :face font-lock-warning-face) t)
  ;; (tabnine-start-process)
  ;; :bind
  ;; (:map  tabnine-completion-map
  ;; 	 ("<tab>" . tabnine-accept-completion)
  ;; 	 ("TAB" . tabnine-accept-completion)
  ;; 	 ("M-f" . tabnine-accept-completion-by-word)
  ;; 	 ("M-<return>" . tabnine-accept-completion-by-line)
  ;; 	 ("C-g" . tabnine-clear-overlay)
  ;; 	 ("M-[" . tabnine-previous-completion)
;; 	 ("M-]" . tabnine-next-completion)))

(use-package leetcode
  :straight t
  :commands (leetcode)
  :hook (leetcode-solution-mode .
          (lambda() (flymake-mode -1)))
  :custom
  (leetcode-prefer-language "swift")
  (leetcode-prefer-sql "mysql")
  (leetcode-save-solutions t)
  (leetcode-directory "~/Documents/Leetcode")
  )

(use-package jinx
  :straight t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))


(provide 'init-prog)
;;; init-prog.el ends here
