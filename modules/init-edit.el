;;; init-edit --- editing related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(setq undo-limit 67108864) ; 64mb.
(setq undo-strong-limit 100663296) ; 96mb.
(setq undo-outer-limit 1006632960) ; 960mb.

(use-package vundo
  :straight t
  :commands (vundo))

(use-package undo-fu
  :straight t
  :bind*
  (("s-z" . undo-fu-only-undo)
   ("s-Z" . undo-fu-only-redo)))

(use-package undo-fu-session
  :straight t
  :after undo-fu
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))


;; Alternatives to [hungry-delete]
(setq backward-delete-char-untabify-method 'hungry)
;; override all minor modes
;; (bind-key* "DEL" 'backward-delete-char-untabify)

;; [puni]
(use-package puni
  :straight t
  :hook ((prog-mode sgml-mode nxml-mode LaTeX-mode eval-expression-minibuffer-setup) . puni-mode)
  :bind (:map puni-mode-map
              ("DEL" . +puni-hungry-delete))
  :config
  (defun +puni-hungry-delete ()
    (interactive)
    (if (looking-back "^[[:blank:]]+")
        (let* ((puni-mode nil)
               (original-func (key-binding (kbd "DEL"))))
          ;; original-func is what `DEL' would be if puni-mode were disabled
          (if (eq original-func 'delete-backward-char)
              (backward-delete-char-untabify 1)
            (call-interactively original-func)))
      (puni-backward-delete-char))))

(use-package embrace
  :straight t
  :after meow
  ;; :bind* ("C-c ." . embrace-commander)
  :hook (org-mode . embrace-org-mode-hook))

(use-package avy
  :straight t
  :after meow
  :custom
  (avy-timeout-seconds 0.4)
  (avy-all-windows nil)
  (avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))

(use-package meow
  :straight t
  :hook (after-init . meow-global-mode)
  :demand t
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (setq-default meow-replace-state-name-list '((normal . "N")
                                               (motion . "M")
                                               (keypad . "K")
                                               (insert . "I")
                                               (beacon . "B")))
  (setq meow-use-clipboard t)

  ;; [motion]
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))

  ;; [leader]
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)

   ;; common commands 
   '("a" . org-agenda)
   '("f" . find-file)
   '("b" . consult-buffer)
   '("k" . kill-buffer)
   '("d" . dirvish)
   )

  ;; [normal]
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   ;; '("e" . meow-next-word)
   ;; '("E" . meow-next-symbol)
   '("e" . embrace-add)
   '("E" . embrace-commander)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . undo-redo)
   '("s" . meow-kill)
   '("t" . avy-goto-char-timer)
   '("u" . undo-fu-only-undo)
   '("U" . undo-fu-only-redo)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("V" . meow-line) ; temp balance behaviour
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))

  (dolist
      (state
       '((View-mode . normal)
         (comint-mode . normal)         ; IELM
         (fundamental-mode . normal)
         (message-mode . normal)
         (emacs-lisp-mode . normal)
         (eshell-mode . insert)
         (shell-mode . insert)
         (term-mode . insert)
         (vterm-mode . insert)
         (help-mode . normal)
         (vundo-mode . motion)))
    (add-to-list 'meow-mode-state-list state))

  (setq meow-two-char-escape-sequence "jk")
  (setq meow-two-char-escape-delay 0.5)

  (defun meow--two-char-exit-insert-state (s)
    (when (meow-insert-mode-p)
      (let ((modified (buffer-modified-p)))
	(insert (elt s 0))
	(let* ((second-char (elt s 1))
               (event
		(if defining-kbd-macro
                    (read-event nil nil)
		  (read-event nil nil meow-two-char-escape-delay))))
          (when event
            (if (and (characterp event) (= event second-char))
		(progn
                  (backward-delete-char 1)
                  (set-buffer-modified-p modified)
                  (meow--execute-kbd-macro "<escape>"))
              (push event unread-command-events)))))))

  (defun meow-two-char-exit-insert-state ()
    (interactive)
    (meow--two-char-exit-insert-state meow-two-char-escape-sequence))

  (define-key meow-insert-state-keymap (substring meow-two-char-escape-sequence 0 1)
	      #'meow-two-char-exit-insert-state)
  (if (featurep 'corfu)
      (add-hook 'meow-insert-exit-hook 'corfu-quit)))

(use-package meow-tree-sitter
  :straight t
  :after meow
  :config
  (meow-tree-sitter-register-defaults))

(provide 'init-edit)
;;; init-edit.el ends here
