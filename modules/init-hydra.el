;; init-hydra.el --- Initialize hydra configurations.	-*- lexical-binding: t -*-

(use-package hydra
  :hook (emacs-lisp-mode . hydra-add-imenu)
  :init
  (setq hydra-hint-display-type 'posframe)

  (with-eval-after-load 'posframe
    (defun hydra-set-posframe-show-params ()
      "Set hydra-posframe style."
      (setq hydra-posframe-show-params
            `(:left-fringe 8
			   :right-fringe 8
			   :internal-border-width 2
			   ;; :internal-border-color ,(face-background 'posframe-border nil t)
			   :background-color ,(face-background 'tooltip nil t)
			   :foreground-color ,(face-foreground 'tooltip nil t)
			   :lines-truncate t
			   :poshandler posframe-poshandler-frame-center-near-bottom)))
    (hydra-set-posframe-show-params)
    (add-hook 'after-load-theme-hook #'hydra-set-posframe-show-params t)))

(use-package pretty-hydra
  :demand t
  :custom (pretty-hydra-default-title-body-format-spec " %s%s")
  :bind ("<f6>" . toggles-hydra/body)
  :hook (emacs-lisp-mode . (lambda ()
                             (add-to-list
                              'imenu-generic-expression
                              '("Hydras"
                                "^.*(\\(pretty-hydra-define\\) \\([a-zA-Z-]+\\)"
                                2))))
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:inherit highlight :reverse-video t)))
          (height (or height 1.2))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "nerd-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face)))))

(provide 'init-hydra)
