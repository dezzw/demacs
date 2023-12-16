;;; -*- lexical-binding: t -*-

(use-package gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB

(use-package exec-path-from-shell
    :init (exec-path-from-shell-initialize))

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
  :straight nil
  :when (daemonp)
  :custom
  (midnight-period 7200)
  :config
  (midnight-mode))

(use-package savehist
  :straight nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(provide 'init-base)
