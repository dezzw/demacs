;;; -*- lexical-binding: t -*-

(use-package web-mode
  :mode "\\.html?\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))


;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
;;(use-package impatient-mode
;;  :commands (impatient-mode))

;;(use-package skewer-mode
;;  :commands (skewer-mode))

(defun dw/set-js-indentation ()
  (setq-default js-indent-level 2)
  (setq-default tab-width 2))

(use-package js2-mode
  :mode "\\.c?js\\'"
  :hook
  (js2-mode . dw/set-js-indentation)
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil))


(use-package rjsx-mode
  :mode "\\.jsx\\'")

(use-package add-node-modules-path
  :hook
  ((js2-mode . add-node-modules-path)
   (typescript-ts-mode . add-node-modules-path)
   (tsx-ts-mode . add-node-modules-path)
   (rjsx-mode . add-node-modules-path)))

(defun dw/format-prettier()
  (interactive)
  (shell-command
   (format "yarn prettier --write %s" 
           (buffer-file-name))))

;; not good enough for use
;; (use-package auto-rename-tag
;;   :hook ((web-mode rjsx-mode tsx-ts-mode) . auto-rename-tag-mode))

;; (use-package scss-mode
;;   :mode "\\.scss\\'"
;;   :custom
;;   (scss-compile-at-save t)
;;   (scss-output-directory "../css")
;;   (scss-sass-command "sass --no-source-map"))

(use-package svelte-mode
  :mode "\\.svelte\\'")

(use-package emmet-mode
  :hook ((web-mode css-ts-mode css-mode js2-mode rjsx-mode tsx-ts-mode) . emmet-mode)
  :config
  (add-to-list 'emmet-jsx-major-modes 'tsx-ts-mode))

(provide 'init-web)
