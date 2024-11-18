;;; -*- lexical-binding: t -*-

;; Copy from https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-shell.el
;; Better term
;; @see https://github.com/akermu/emacs-libvterm#installation
(use-package vterm
  :straight nil
  :commands (vterm vterm-posframe-toggle)
  :bind ("C-c `" . vterm-posframe-toggle)
  :custom
  (vterm-max-scrollback 10000)
  :config
  (with-no-warnings
    (when (and (fboundp #'posframe-workable-p)
	       (posframe-workable-p))
      (defvar vterm-posframe--frame nil)
      (defun vterm-posframe-toggle ()
        "Toggle `vterm' child frame."
        (interactive)
        (let ((buffer (vterm--internal #'ignore 100))
              (width  (max 80 (/ (frame-width) 2)))
              (height (/ (frame-height) 2)))
          (if (frame-live-p vterm-posframe--frame)
              (progn
                (posframe-delete-frame buffer)
                (setq vterm-posframe--frame nil))
            (setq vterm-posframe--frame
                  (posframe-show
                   buffer
                   :poshandler #'posframe-poshandler-frame-center
                   :left-fringe 8
                   :right-fringe 8
                   :width width
                   :height height
                   :min-width width
                   :min-height height
                   :internal-border-width 3
                   :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                   :background-color (face-background 'tooltip nil t)
                   :accept-focus t))))))))

(use-package multi-vterm
  :straight t
  :commands (multi-vterm))

(use-package vterm-toggle
  :straight t
  :commands  (vterm-toggle-cd))

(provide 'init-vterm)
