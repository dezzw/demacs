;;; init-env.el --- system environment related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(defconst +env-file (concat user-emacs-directory ".env"))

(defun +load-env-file (file &optional noerror)
  "Read and set envvars from FILE.
If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
unreadable. Returns the names of envvars that were changed."
  (if (not (file-readable-p file))
      (unless noerror
        (signal 'file-error (list "Couldn't read envvar file" file)))
    (let (envvars environment)
      (with-temp-buffer
        (save-excursion
          (insert "\n")
          (insert-file-contents file))
        (while (re-search-forward "\n *\\([^#= \n]*\\)=" nil t)
          (push (match-string 1) envvars)
          (push (buffer-substring
                 (match-beginning 1)
                 (1- (or (save-excursion
                           (when (re-search-forward "^\\([^= ]+\\)=" nil t)
                             (line-beginning-position)))
                         (point-max))))
                environment)))
      (when environment
        (setq process-environment
              (append (nreverse environment) process-environment)
              exec-path
              (if (member "PATH" envvars)
                  (append (split-string (getenv "PATH") path-separator t)
                          (list exec-directory))
                exec-path)
              shell-file-name
              (if (member "SHELL" envvars)
                  (or (getenv "SHELL") shell-file-name)
                shell-file-name))
        envvars))))

(when (and (or (display-graphic-p))
           (file-exists-p +env-file))
  (+load-env-file +env-file))

;; (use-package exec-path-from-shell
;;   :straight t
;;   :init
;;   (setq exec-path-from-shell-variables '("PATH" "MANPATH")
;;           exec-path-from-shell-arguments '("-l"))
;;   (exec-path-from-shell-initialize))

(use-package envrc
  :straight t
  :hook (after-init . dw/maybe-enable-envrc-global-mode)
  :config
  (defun dw/maybe-enable-envrc-global-mode ()
    "Enable `envrc-global-mode' if `direnv' is installed."
    (when (executable-find "direnv")
      (envrc-global-mode)))

  (with-eval-after-load 'envrc
    (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)))

(provide 'init-env)
;;; init-env.el ends here
