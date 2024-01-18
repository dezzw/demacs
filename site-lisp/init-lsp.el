;;; -*- lexical-binding: t -*-
;;; LSP
;; Should boost performance with lsp
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(use-package lsp-mode
  :hook ((python-mode web-mode js2-mode typescript-ts-mode tsx-ts-mode rjsx-mode) . lsp)
  :bind ((:map lsp-mode-map
               ("M-<return>" . lsp-execute-code-action)))
  :commands (lsp lsp-deferred)
  :init
  ;; (setenv "LSP_USE_PLISTS" "1")
  ;; Increase the amount of data emacs reads from processes
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-clients-clangd-args '("--header-insertion-decorators=0"
                                  "--clang-tidy"
                                  "--enable-config"))
  ;; Disable features that have great potential to be slow.
  (setq lsp-enable-folding nil
        lsp-enable-text-document-color nil)
  ;; Reduce unexpected modifications to code
  (setq lsp-enable-on-type-formatting nil)
  ;; Make breadcrumbs opt-in; they're redundant with the modeline and imenu
  (setq lsp-headerline-breadcrumb-enable nil)

  ;; General lsp-mode settings
  (setq lsp-completion-provider :none
        lsp-enable-snippet nil
        lsp-enable-indentation nil
	lsp-idle-delay 0.500
        lsp-keymap-prefix "C-x L")
  ;; to enable the lenses
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'lsp-completion-mode-hook
            (lambda ()
              (setf (alist-get 'lsp-capf completion-category-defaults)
                    '((styles . (orderless flex)))))))
  ;; :config
  ;; (defun dw/with-lsp-completion()
  ;;   (setq-local completion-at-point-functions
  ;; 		(list (cape-capf-buster
  ;; 		       (cape-super-capf
  ;; 			#'lsp-completion-at-point
  ;; 			#'tempel-complete
  ;; 			#'cape-dabbrev
  ;; 			#'tabnine-completion-at-point)))))
  ;; (add-hook 'lsp-completion-mode-hook #'dw/with-lsp-completion))

(use-package lsp-tailwindcss
  :commands (lsp lsp-deferred lsp-restart-workspace)
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  (setq lsp-tailwindcss-major-modes '(tsx-ts-mode rjsx-mode web-mode css-mode)))

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(use-package lsp-haskell
  :hook (haskell-mode . lsp-deferred))

(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :config
  (require 'lsp-java-boot)
  
  ;; to enable the lenses
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode))

(use-package lsp-pyright
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp-deferred))))  ; or lsp-deferred

(use-package lsp-ui
  :after lsp-mode
  :init
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 72         ; 150 (default) is too wide
        lsp-ui-doc-delay 0.75           ; 0.2 (default) is too naggy
  	;; lsp-ui doc
  	lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover
  	lsp-ui-doc-show-with-cursor t
	;; lsp-ui sideline
  	lsp-ui-sideline-show-hover nil
	lsp-ui-sideline-show-code-actions t))

;;; Debugging
(use-package dap-mode
  :commands (dap-debug dap-debug-last)
  :bind (:map dap-mode-map
	      ("C-x D D" . dap-debug)
	      ("C-x D d" . dap-debug-last))
  :config
  (with-eval-after-load 'python-mode
    (require 'dap-python)
    ;; if you installed debugpy, you need to set this
    ;; https://github.com/emacs-lsp/dap-mode/issues/306
    (setq dap-python-debugger 'debugpy))

  (with-eval-after-load 'c++-mode
    (require 'dap-gdb-lldb)
    (dap-gdb-lldb-setup))
  (setq dap-auto-configure-features '(sessions locals controls tooltip)))

(provide 'init-lsp)
