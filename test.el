(defun dw/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :demand t
  :hook (evil-mode . 'dw/evil-hook)
  :init
  ;; Pre-load configuration
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)
  :bind

  :config
  ;; Activate the Evil
  (evil-mode 1)

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; Clear the binding of C-k so that it doesn't conflict with Corfu
  (define-key evil-insert-state-map (kbd "C-k") nil)

  (evil-set-initial-state 'messages-buffer-mode 'normal))

;; (use-package evil-nerd-commenter
;;   :commands (evilnc-comment-or-uncomment-lines)
;;   :bind
;;   ("M-;" . 'evilnc-comment-or-uncomment-lines))

(use-package evil-escape
  :after evil
  :custom
  (evil-escape-key-sequence "jk")
  :config
  (evil-escape-mode 1))

(use-package evil-visualstar
  :defer 2
  :config
  (global-evil-visualstar-mode))

(use-package evil-surround
  :defer 2
  :config
  (global-evil-surround-mode 1))

(use-package evil-multiedit
  :defer 2
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-mc
  :commands (evil-mc-make-cursor-here
             evil-mc-make-all-cursors
             evil-mc-pause-cursors
             evil-mc-resume-cursors
             evil-mc-make-and-goto-first-cursor
             evil-mc-make-and-goto-last-cursor
             evil-mc-make-cursor-in-visual-selection-beg
             evil-mc-make-cursor-in-visual-selection-end
             evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-make-cursor-at-pos
             evil-mc-has-cursors-p
             evil-mc-make-and-goto-next-cursor
             evil-mc-skip-and-goto-next-cursor
             evil-mc-make-and-goto-prev-cursor
             evil-mc-skip-and-goto-prev-cursor
             evil-mc-make-and-goto-next-match
             evil-mc-skip-and-goto-next-match
             evil-mc-skip-and-goto-next-match
             evil-mc-make-and-goto-prev-match
             evil-mc-skip-and-goto-prev-match)
  :config
  (global-evil-mc-mode  1))

(use-package evil-matchit
  :defer 2
  :config
  (global-evil-matchit-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-tex
  :hook (LaTeX-mode org-mode))


(use-package general)

(general-evil-setup t)

(general-create-definer dw/leader-key-def
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

(general-create-definer dw/ctrl-c-keys
  :prefix "C-c")

(general-define-key
 :states '(normal)
 "r" 'evil-redo
 "Q" "@q"
 "gJ" 'jester/evil-join-no-whitespace)

(dw/leader-key-def
 "SPC" 'execute-extended-command
 "f" 'find-file
 "b" 'consult-buffer
 "d" 'consult-dir
 "a" 'org-agenda)
