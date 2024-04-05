;; ;; [Eglot] LSP support
;; (use-package eglot
;;   :hook (python-ts-mode . eglot-ensure)
;;   :bind (:map eglot-mode-map
;;          ("M-<return>" . eglot-code-actions))
;;   :config
;;   (setq eglot-events-buffer-size 0
;;         eglot-connect-timeout 10
;;         eglot-autoshutdown t
;;         eglot-report-progress 'messages)

  ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; eglot has it's own strategy by default
  ;; (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly
  ;;             completion-at-point-functions (cl-nsubst
  ;;                                            (cape-capf-noninterruptible
  ;;                                             (cape-capf-buster #'eglot-completion-at-point
  ;;                                                               #'string-prefix-p))
  ;;                                            'eglot-completion-at-point
  ;;                                            completion-at-point-functions))

  ;; we call eldoc manually by C-h .
  ;; (add-hook! eglot-managed-mode-hook
  ;;   (defun +eglot-disable-eldoc-mode ()
  ;;     (when (eglot-managed-p)
  ;;       (eldoc-mode -1))))
  ;; )


(use-package eglot-tempel
  :after (eglot tempel)
  :init
  (eglot-tempel-mode))


(use-package eglot-x
  :straight (:host github :repo "nemethf/eglot-x")
  :hook (eglot-managed-mode . eglot-x-setup))

(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode))


;; [Eldoc]
(use-package eldoc
  :straight nil
  :bind (("C-h ." . eldoc))
  :config
  (setq eldoc-echo-area-display-truncation-message t
        eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil
        eglot-extend-to-xref t))


;; [consult-eglot] Eglot support for consult
(use-package consult-eglot
  :after consult eglot
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] . consult-eglot-symbols)))

(provide 'init-eglot)
