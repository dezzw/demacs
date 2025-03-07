;;; init-base.el --- base related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(when (not (fboundp 'igc-stats))
  (use-package gcmh
    :diminish
    :straight t
    :hook (emacs-startup . gcmh-mode)))


(use-package emacs
  :straight nil
  :custom
  (scroll-preserve-screen-position 'always)
  (truncate-partial-width-windows nil)
  (use-short-answers t)
  (frame-resize-pixelwise t)
  (create-lockfiles nil)
  (auto-save-default nil)
  (make-backup-files nil)
  (global-auto-revert-mode 1)
  (delete-selection-mode t)
  (auto-save-visited-interval 1.1)
  (auto-save-visited-predicate
   (lambda () (and (not (buffer-live-p (get-buffer " *vundo tree*")))
                   (not (string-suffix-p "gpg" (file-name-extension (buffer-name)) t))
                   (not (eq (buffer-base-buffer (get-buffer (concat "CAPTURE-" (buffer-name))))
                            (current-buffer)))
                   (or (not (boundp 'corfu--total)) (zerop corfu--total))
                   (or (not (boundp 'yas--active-snippets)) (not yas--active-snippets)))))
  (display-fill-column-indicator-character ?\u254e)
  :hook ((prog-mode . display-fill-column-indicator-mode)
         ((prog-mode text-mode) . indicate-buffer-boundaries-left)
         (after-init . auto-save-visited-mode))
  :config
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left)))

(use-package recentf
  :straight nil
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

(use-package midnight
  :when (daemonp)
  :custom
  (midnight-period 7200)
  :config
  (midnight-mode))

(provide 'init-base)
;;; init-base.el ends here
