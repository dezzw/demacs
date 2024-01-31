;;; -*- lexical-binding: t -*-

;;
;; Speed up startup
;;

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "test"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

;; (defun add-subdirs-to-load-path (&rest _)
;;   "Add subdirectories to `load-path'.
;;    Don't put large files in `site-lisp' directory, e.g. EAF.
;;    Otherwise the startup will be very slow."
;;   (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
;;     (normal-top-level-add-subdirs-to-load-path)))

;; (advice-add #'package-initialize :after #'add-subdirs-to-load-path)a

(update-load-path)

(require 'init-util)
(require 'init-package)

(require 'init-base)
(require 'init-hydra)

(require 'init-font)
(require 'init-ui)

(require 'init-edit)
(require 'init-helpful)
(require 'init-completion)

(require 'init-wm)

(require 'init-org)
(require 'init-dired)

;; prog
(require 'init-direnv)
(require 'init-prog)
(require 'init-dap)
;;; lsp config
;; (require 'init-lspbridge)
;; (require 'init-eglot)
(require 'init-lsp)
(require 'init-ctags)

;;; lang
(require 'init-treesit)
(require 'init-web)
(require 'init-python)
(require 'init-clojure)
(require 'init-clisp)
(require 'init-gdscript)
(require 'init-markdown)
(require 'init-docker)
(require 'init-haskell)
(require 'init-nix)
(require 'init-swift)
(require 'init-zig)
(require 'init-rust)
(require 'init-tex)
(require 'init-db)

;; terms
(require 'init-vterm)
(require 'init-eshell)

;; magit
(require 'init-magit)
;; (require 'init-telega)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-day))
 '(custom-safe-themes
   '("04aa1c3ccaee1cc2b93b246c6fbcd597f7e6832a97aaeac7e5891e6863236f9f"
     "76ddb2e196c6ba8f380c23d169cf2c8f561fd2013ad54b987c516d3cabc00216"
     "6bdc4e5f585bb4a500ea38f563ecf126570b9ab3be0598bdf607034bb07a8875"
     "6fc9e40b4375d9d8d0d9521505849ab4d04220ed470db0b78b700230da0a86c1"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
