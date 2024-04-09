;;; -*- lexical-binding: t -*-
;;; LSP
;; Should boost performance with lsp
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(use-package lsp-mode
  :bind ((:map lsp-mode-map
               ("M-<return>" . lsp-execute-code-action)))
  :commands (lsp lsp-deferred)
  :init
  ;; Increase the amount of data emacs reads from processes
  (setq read-process-output-max (* 1024 1024))
  
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
	lsp-semantic-tokens-enable t
        lsp-enable-indentation nil
	lsp-idle-delay 0.500
        lsp-keymap-prefix "C-x L")
  
  ;; to enable the lenses
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'lsp-completion-mode-hook
            (lambda ()
              (setf (alist-get 'lsp-capf completion-category-defaults)
                    '((styles . (orderless flex))))))
    
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
	 (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
			 (fboundp 'json-parse-buffer))
                  'json-parse-buffer
		'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
	orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(use-package lsp-tailwindcss
  :after (:all lsp (:any lsp-tailwindcss-major-modes))
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  (setq lsp-tailwindcss-major-modes
	(append '(tsx-ts-mode) lsp-tailwindcss-major-modes)))

(use-package lsp-sourcekit
  :hook (swift-mode . lsp-deferred)
  :config
  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(use-package lsp-haskell
  :hook (haskell-mode . lsp-deferred))

(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :config
  (require 'lsp-java-boot)
  
  ;; to enable the lenses
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode))

(use-package lsp-pyright
  :hook ((python-mode python-ts-mode) . (lambda ()
					  (require 'lsp-pyright)
					  (lsp-deferred))))

(use-package lsp-ui
  :after lsp-mode
  :bind
  (:map lsp-ui-mode-map
	([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	([remap xref-find-references] . lsp-ui-peek-find-references))
  :init
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 72         ; 150 (default) is too wide
        lsp-ui-doc-delay 0.75           ; 0.2 (default) is too naggy
  	;; lsp-ui doc
  	lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover
  	lsp-ui-doc-show-with-cursor t
	;; lsp-ui sideline
  	lsp-ui-sideline-show-hover nil
	lsp-ui-sideline-show-code-actions nil
	;; lsp signature
	lsp-signature-render-documentation nil
	))

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

(use-package flycheck)

(use-package consult-lsp
  :commands (consult-lsp-symbols consult-lsp-diagnostics))

(provide 'init-lsp)
