;;; init-prog.el --- prog mode related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package elec-pair
  :straight nil
  :hook ((prog-mode conf-mode yaml-mode org-mode markdown-mode minibuffer-mode) . electric-pair-mode))

(use-package electric
  :straight nil
  :config (electric-indent-mode))

(use-package indent-bars
  :straight t
  :hook (prog-mode . indent-bars-mode)
  :custom
  (indent-bars-prefer-character t))

;; (use-package rainbow-mode
;;   :straight t)

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode))

(use-package colorful-mode
  :straight t
  :hook (prog-mode text-mode)
  :custom
  (colorful-use-prefix t))


;; (use-package prism
;;   :straight t)

(use-package aggressive-indent
  :straight t
  :hook (python-mode emacs-lisp-mode clojure-mode clojurescript-mode fennel-mode))

(use-package paren
  :custom-face (show-paren-match ((t (:foreground "SpringGreen3" :underline t :weight bold))))
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen t
        show-paren-delay 0.2))

(use-package highlight-parentheses
  :straight t
  :hook ((minibuffer-setup . highlight-parentheses-minibuffer-setup)
         (prog-mode . highlight-parentheses-mode))
  :config
  (setq highlight-parentheses-colors '("firebrick1" "firebrick3" "orange1" "orange3")
        highlight-parentheses-attributes '((:underline t) (:underline t) (:underline t))
        highlight-parentheses-delay 0.2)
  )

(use-package hl-line
  :hook (after-init . global-hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil)
  ;; Highlight starts from EOL, to avoid conflicts with other overlays
  (setq hl-line-range-function (lambda () (cons (line-end-position)
                                           (line-beginning-position 2)))))

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

(use-package jinx
  :straight t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package eee
  :straight '(:type git :host github :repo "eval-exec/eee.el"
                    :files (:defaults "*.el" "*.sh"))
  :config
  (setq ee-terminal-command "wezterm"))

(use-package editorconfig
  :demand t
  :config
  (defun oxcl/update-indent-bars-with-editorconfig (size)
    (when (bound-and-true-p indent-bars-mode)
      (setq indent-bars-spacing-override size)
      (indent-bars-reset)))
  (dolist (_mode editorconfig-indentation-alist)
    (let ((_varlist (cdr _mode)))
      (setcdr _mode (append '((_ . oxcl/update-indent-bars-with-editorconfig))
                            (if (listp _varlist) _varlist `(,_varlist))))))
  (editorconfig-mode 1))


(provide 'init-prog)
;;; init-prog.el ends here
