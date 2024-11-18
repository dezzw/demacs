;;; -*- lexical-binding: t -*-

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :custom
  (custom-safe-themes t))

(defun dw/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ;; ('light (load-theme 'modus-operandi t))
    ;; ('dark (load-theme 'modus-vivendi t))))
    ('light (load-theme 'sanityinc-tomorrow-day))
    ('dark (load-theme 'sanityinc-tomorrow-night))))

(add-hook 'ns-system-appearance-change-functions #'dw/apply-theme)

(use-package nerd-icons
  :straight t
  :defer t)

(use-package svg-tag-mode
  :straight t
  :defer t)

(use-package doom-modeline
  :straight t
  :hook
  ((after-init . doom-modeline-mode)
   (doom-modeline-mode . size-indication-mode)
   (doom-modeline-mode . column-number-mode))
  :config
  (unless after-init-time
    ;; prevent flash of unstyled modeline at startup
    (setq-default mode-line-format nil))
  ;; Set these early so they don't trigger variable watchers
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-lsp nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        ;; Only show file encoding if it's non-UTF-8 and different line endings
        ;; than the current OSes preference
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-default-eol-type 0))

;; Enable liner number
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook
		pdf-view-mode-hook
                xwidget-webkit-mode-hook
                eaf-mode-hook
                doc-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package transwin
  :straight t
  :config
  (setq transwin-delta-alpha 5)
  (setq transwin-parameter-alpha 'alpha-background)
  :bind
  ("C-M-=" . transwin-inc)
  ("C-M--" . transwin-dec)
  ("C-M-0" . transwin-toggle))

;; (use-package hl-todo
;;   :config
;;   (setq hl-todo-keyword-faces
;;         '(("TODO"   . "#61d290")
;; 	  ("IMPLEMENT" . "#61d290")
;;           ("FIXME"  . "#FF0000")
;;           ("DEBUG"  . "#A020F0")
;;           ("NEXT" . "#FF4500")
;;           ("UNCHECK"   . "#1E90FF")))
;;   (global-hl-todo-mode))

(use-package diff-hl
  :straight t
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)))

;; Child frame
(when (childframe-workable-p)
  (use-package posframe
    :straight t
    :hook (after-load-theme . posframe-delete-all)
    :init
    (defface posframe-border
      `((t (:inherit region)))
      "Face used by the `posframe' border."
      :group 'posframe)
    (defvar posframe-border-width 2
      "Default posframe border width.")
    :config
    (with-no-warnings
      (defun my-posframe--prettify-frame (&rest _)
        (set-face-background 'fringe nil posframe--frame))
      (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

      (defun posframe-poshandler-frame-center-near-bottom (info)
        (cons (/ (- (plist-get info :parent-frame-width)
                    (plist-get info :posframe-width))
                 2)
              (/ (+ (plist-get info :parent-frame-height)
                    (* 2 (plist-get info :font-height)))
                 2))))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
		(setq doom-modeline-icon t)
		(with-selected-frame frame
		  (dw/set-fonts))))
  (if (display-graphic-p)
      (dw/set-fonts)))

(provide 'init-ui)
