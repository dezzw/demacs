;;; early-init.el --- early init file -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Desmond Wang
;;
;; Author: Desmond Wang <dw@dezzw.com>
;; Maintainer: Desmond Wang <dw@dezzw.com>
;; Created: August 06, 2023
;; Modified: August 06, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/early-init
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;

;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

(require 'cl-lib)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; `use-package' is builtin since 29.
;; It must be set before loading `use-package'.
(setq use-package-enable-imenu-support t)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)
(push '(undecorated . t) default-frame-alist)
;; (push '(ns-transparent-titlebar . t) default-frame-alist)
;; (push '(ns-appearance . dark) default-frame-alist)

(setq make-backup-files       nil
      auto-save-default       nil
      inhibit-startup-message t
      inhibit-splash-screen   t
      ring-bell-function      'ignore
      tab-bar-mode 1)


(pixel-scroll-precision-mode 1)


;; (when (featurep 'ns)
;;   (push '(ns-transparent-titlebar . t) default-frame-alist))
(setq-default mode-line-format nil)

(defconst IS-MAC (eq system-type 'darwin))


(if IS-MAC
    (progn
      (setq frame-resize-pixelwise  t)
      (menu-bar-mode t)))
