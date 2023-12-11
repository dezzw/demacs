;; Copy from https://github.com/willbush/system/blob/master/emacs/early-init.el
(defconst IS-GUI (or (display-graphic-p) (and (daemonp) (not (string= (daemonp) "tty")))))
(defconst IS-TTY (or (not (display-graphic-p)) (and (daemonp) (string= (daemonp) "tty"))))

(setq native-comp-deferred-compilation-deny-list nil)

;; (add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(setq use-package-verbose t)
(when (daemonp)
  (setq use-package-always-demand t))

;; (require 'init-package)

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)
;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;; (use-package doom-themes
;;   :config
;;   (load-theme 'doom-one t))

(load-theme 'modus-vivendi t)

(use-package doom-modeline
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
        doom-modeline-default-eol-type
        (cond (IS-MAC 2)
              (0))))

(use-package gcmh
  :hook (after-init . gcmh-mode)
  :custom
  ;; doom is using 0.5, default is 15s
  (gcmh-idle-delay 0.5)
  ;; 16 MB
  (gcmh-high-cons-threshold (* 16 1024 1024)))

(use-package exec-path-from-shell
  :demand t
  :custom
  (exec-path-from-shell-arguments '("-l"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; Editing utils
(use-package emacs
  ;; :straight nil
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
  ;; :straight nil
  :custom
  (recentf-max-saved-items 1000)
  (recentf-exclude `("/tmp/" "/ssh:" ,(concat user-emacs-directory "lib/.*-autoloads\\.el\\'")))
  :config
  (recentf-mode))

(use-package midnight
  ;; :straight nil
  :defer t
  :custom
  (midnight-period 7200)
  :config
  (midnight-mode))

(use-package posframe)

(use-package nerd-icons)

(use-package vundo
  :commands (vundo))

(use-package hungry-delete
  :hook (prog-mode . hungry-delete-mode)
  :custom
  (hungry-delete-join-reluctantly t))

(use-package avy
  :bind ("C-," . avy-goto-char-timer)
  :custom
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  (avy-timeout-seconds 0.3))


(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

(use-package evil-nerd-commenter
  :commands (evilnc-comment-or-uncomment-lines)
  :bind
  ("M-;" . 'evilnc-comment-or-uncomment-lines))

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
  :hook (TeX-latex-mode))


(use-package general
  :config
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
    "a" 'org-agenda))


;; Set default font
(defun dw/set-fonts()
  (interactive)
  (set-face-attribute 'default nil
                      :font "Liga SFMono Nerd Font"
                      ;; :font "JetBrainsMono Nerd Font"
                      :weight 'regular
                      :height 140)

  ;; Set the fixed pitch face
  ;; (set-face-attribute 'fixed-pitch nil
  ;; 		    :font "Operator Mono SSm Lig"
  ;; 		    :weight 'light
  ;; 		    :height 140)

  ;; Set the variable pitch face
  ;; (set-face-attribute 'variable-pitch nil
  ;; 		    :font "Operator Mono SSm Lig"
  ;; 		    :height 140
  ;; 		    :weight 'light)
  )

(use-package ligature
  :defer 0.5
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; (use-package unicode-fonts
;;   :defer 0.5
;;   :config
;;   (unicode-fonts-setup))

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

(use-package hl-todo
  :defer t
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#61d290")
	  ("IMPLEMENT" . "#61d290")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("NEXT" . "#FF4500")
          ("UNCHECK"   . "#1E90FF")))
  (global-hl-todo-mode))

;; (use-package diff-hl
;;   :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
;;          (after-init . global-diff-hl-mode)
;;          (dired-mode . diff-hl-dired-mode)))

(use-package vdiff
  :commands (vdiff-buffer))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
		(setq doom-modeline-icon t)
		(with-selected-frame frame
		  (dw/set-fonts))))
  (if (display-graphic-p)
      (dw/set-fonts)))

(use-package beframe
  :when (daemonp)
  :config
  (setq beframe-global-buffers '("*scratch*" "*Messages*"))
  (beframe-mode 1)
  (defvar consult-buffer-sources)
  (declare-function consult--buffer-state "consult")

  (with-eval-after-load 'consult
    (defface beframe-buffer
      '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defvar beframe-consult-source
      `( :name     "Frame-specific buffers (current frame)"
         :narrow   ?F
         :category buffer
         :face     beframe-buffer
         :history  beframe-history
         :items    ,#'beframe-buffer-names
         :action   ,#'switch-to-buffer
         :state    ,#'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'beframe-consult-source))

  (defun my/beframe-items (&optional frame)
    (beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))
  )

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package winner
  ;; :straight nil
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config (winner-mode 1))

(use-package popper
  :bind (:map popper-mode-map
         ("C-h z"       . popper-toggle)
         ("C-<tab>"     . popper-cycle)
         ("C-M-<tab>"   . popper-toggle-type))
  :hook (emacs-startup . popper-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
	  helpful-mode
          compilation-mode
          ;; "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
          "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
          )))

(use-package dired-single
  :commands (dired-single-buffer dired-single-up-directory))

(use-package dired-hide-dotfiles
  :hook dired-mode)

(use-package dired
  ;; :straight nil
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alGh")
  (dired-recursive-copies 'always)
  (dired-mouse-drag-files t)                   ; added in Emacs 29
  (mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  (dired-kill-when-opening-new-dired-buffer t)
  (delete-by-moving-to-trash t)
  :config
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-single-up-directory
      "H" 'dired-omit-mode
      "l" 'dired-single-buffer
      "." 'dired-hide-dotfiles-mode)))

(use-package dirvish
  :commands (dirvish)
  :config
  (dirvish-override-dired-mode))

(defun dw/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  ;; (auto-fill-mode 0)
  (setq evil-auto-indent nil)
  (visual-line-mode 1))

(use-package org
  :defer t
  :hook (org-mode . dw/org-mode-setup)
  :config
  (setq org-html-head-include-default-style nil
        ;; org-ellipsis " ▾"
        org-adapt-indentation t
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)

  (setq org-html-htmlize-output-type nil)

  ;; config for images in org
  (auto-image-file-mode t)
  (setq org-image-actual-width nil)
  ;; default image width
  (setq org-image-actual-width '(300))

  (setq org-export-with-sub-superscripts nil)

  ;; Since we don't want to disable org-confirm-babel-evaluate all
  ;; of the time, do it around the after-save-hook
  (defun dw/org-babel-tangle-dont-ask ()
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))
  
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dw/org-babel-tangle-dont-ask
						'run-at-end 'only-in-org-mode))))

;; change bullets for headings
(use-package org-superstar
  :hook org-mode
  :custom
  (org-superstar-remove-leading-stars t))

(use-package visual-fill-column
  :hook org-mode
  :custom
  (visual-fill-column-width 110)
  (visual-fill-column-center-text t))

(use-package valign
  :hook org-mode)

(use-package org-appear
  :hook org-mode)

;; (use-package org-modern-indent
;;   :after org
;;   :config
;;   (add-hook 'org-mode-hook #'org-modern-indent-mode))

(with-eval-after-load "org-export-dispatch"
  ;; Edited from http://emacs.stackexchange.com/a/9838
  (defun dw/org-html-wrap-blocks-in-code (src backend info)
    "Wrap a source block in <pre><code class=\"lang\">.</code></pre>"
    (when (org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string
       "\\(</pre>\\)" "</code>\n\\1"
       (replace-regexp-in-string "<pre class=\"src src-\\([^\"]*?\\)\">"
                                 "<pre>\n<code class=\"\\1\">" src))))

  (require 'ox-html)

  (add-to-list 'org-export-filter-src-block-functions
               'dw/org-html-wrap-blocks-in-code)
  )

(with-eval-after-load "ob"
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (shell . t)
     (python . t)))

  (setq org-confirm-babel-evaluate nil))

(with-eval-after-load "org"
  ;; Custom TODO states and Agendas
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "TBA(b)" "|" "DONE(d!)")))

  (setq org-tag-alist
        '((:startgroup)
          ;; Put mutually exclusive tags here
          (:endgroup)
          ("review" . ?r)
          ("assignment" . ?a)
          ("lab" . ?l)
          ("test" . ?t)
          ("quiz" . ?q)
          ("pratice" . ?p)
          ("emacs" . ?e)
          ("note" . ?n)
          ("idea" . ?i))))

(if IS-MAC
    (setq org-agenda-files '("~/Documents/Org/Planner")))

(use-package org-super-agenda
  :hook org-agenda-mode
  ;; :commands (org-agenda)
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-include-diary t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-log-done 'time
        org-log-into-drawer t
        org-agenda-start-with-log-mode t)

  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)

                        (org-super-agenda-groups
                         '((:name "Today"
				  :time-grid t
				  :date today
				  :scheduled today
				  :order 1)
                           (:name "Due Soon"
				  :deadline future
				  :order 2)
                           (:discard (:anything t))))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Overdue"
				   :deadline past
				   :order 1)
                            (:name "Assignments"
				   :tag "assignment"
				   :order 2)
                            (:name "Labs"
				   :tag "lab"
				   :order 3)
                            (:name "Quizs"
				   :tag "quiz"
				   :order 4)
                            (:name "Tests/Exam"
				   :tag "test"
				   :order  5)
                            (:name "Projects"
				   :tag "Project"
				   :order 14)
                            (:name "Emacs"
				   :tag "Emacs"
				   :order 13)
                            (:discard (:anything t)))))))))))

;; Refiling
(setq org-refile-targets
      '(("~/Documents/Org/Planner/Archive.org" :maxlevel . 1)))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; Capture Templates
(defun dw/read-file-as-string (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(setq org-capture-templates
      `(("t" "Tasks / Projects")
        ("tt" "Task" entry (file+olp "~/Documents/Org/Planner/Tasks.org" "Inbox")
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)))

(use-package org-roam
  :bind
  (("C-c o l" . org-roam-buffer-toggle)
   ("C-c o f" . org-roam-node-find)
   ("C-c o g" . org-roam-graph)
   ("C-c o i" . org-roam-node-insert)
   ("C-c o c" . org-roam-capture))
  :custom
  (org-roam-directory "~/Documents/Org/Notes")
  (org-roam-database-connecter 'splite-builtin)
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)

  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :commands (org-roam-ui-open)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; (use-package org-inline-anim
;;   :commands (org-inline-anim-animate)
;;   :hook org-mode)

;; (use-package org-imagine
;;   :after org
;;   :config
;;   (setq
;;     org-imagine-cache-dir "./.org-imagine"
;;     org-imagine-is-overwrite nil))

(use-package org-download
  :hook (org-mode . org-download-enable)
  :custom
  (org-download-image-dir "./images/"))

;; Enable vertico
(use-package vertico
  :demand t
  :custom
  (vertico-cycle t)
  (enable-recursive-minibuffers t)
  :config
  (vertico-mode))

;; Configure directory extension.
(use-package vertico-directory
  ;; :straight nil
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  ;; :straight nil
  :init
  (savehist-mode)
  :config
  (setq history-length 25))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Example configuration for Consult
(use-package consult
  :defer 0.5
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c r" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-search-program 'ripgrep
        xref-history-storage 'xref-window-local-history
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<"))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package marginalia
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package embark
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim)
  ("C-h b" . embark-bindings)
  ("C-h B" . embark-bindings-at-point)
  ("M-n" . embark-next-symbol)
  ("M-p" . embark-previous-symbol)
  ("C-h E" . embark-on-last-message)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-prompter 'embark-completing-read-prompter)
  :custom
  (embark-quit-after-action nil)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :config
  (setq embark-candidate-collectors
        (cl-substitute 'embark-sorted-minibuffer-candidates
                       'embark-minibuffer-candidates
                       embark-candidate-collectors))
  (defun embark-on-last-message (arg)
    "Act on the last message displayed in the echo area."
    (interactive "P")
    (with-current-buffer "*Messages*"
      (goto-char (1- (point-max)))
      (embark-act arg))))

;; (use-package consult-applemusic
;;   :commands (consult-applemusic-playlists applemusic-toggle-play))

;; (use-package corfu
;;   :demand t
;;   :bind (:map corfu-map
;;               ("M-SPC"      . corfu-insert-separator)
;;               ("TAB"        . corfu-next)
;;               ([tab]        . corfu-next)
;;               ("S-TAB"      . corfu-previous)
;;               ([backtab]    . corfu-previous)
;;               ("S-<return>" . corfu-insert)
;;               ("RET"        . nil))
;;   :custom
;;   (corfu-cycle t)
;;   (corfu-auto t)
;;   (corfu-auto-prefix 2)
;;   (corfu-auto-delay 0)
;;   (corfu-preselect 'prompt)
;;   (corfu-preselect-first nil)
;;   (corfu-on-exact-match nil)
;;   (corfu-popupinfo-delay '(0.5 . 0.2))
;;   :config
;;   (global-corfu-mode)
;;   ;; (corfu-history-mode)
;;   ;; (corfu-popupinfo-mode)

;;   (add-hook 'eshell-mode-hook
;;             (lambda () (setq-local corfu-quit-at-boundary t
;;                               corfu-quit-no-match t
;;                               corfu-auto nil)
;;               (corfu-mode))))

;; (use-package cape
;;   :custom
;;   (cape-dabbrev-min-length 3)
;;   :config
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-file))

;; (use-package tabnine
;;   ;; :hook (prog-mode . tabnine-mode)
;;   :hook (kill-emacs . tabnine-kill-process)
;;   :config
;;   (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point))

;; (use-package kind-icon
;;   :custom
;;   (kind-icon-default-face 'corfu-default)
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
;;   (setq kind-icon-use-icons nil)
;;   (setq kind-icon-mapping
;;         `(
;;           (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
;;           (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
;;           (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
;;           (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
;;           (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
;;           (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
;;           (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
;;           (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
;;           (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
;;           (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
;;           (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
;;           (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
;;           (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
;;           (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
;;           (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
;;           (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
;;           (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
;;           (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
;;           (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
;;           (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
;;           (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
;;           (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
;;           (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
;;           (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
;;           (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
;;           (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
;;           (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
;;           (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
;;           (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
;;           (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
;;           (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
;;           (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
;;           (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
;;           (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
;;           (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
;;           (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)
;;           (tabnine ,(nerd-icons-codicon "nf-cod-hubot") :face font-lock-warning-face))))

(use-package tempel)
;; :custom
;; (tempel-path (expand-file-name "templates" user-emacs-directory)))

;;   :init
;;   (defun tempel-setup-capf ()
;;     ;; Add the Tempel Capf to `completion-at-point-functions'.
;;     ;; `tempel-expand' only triggers on exact matches. Alternatively use
;;     ;; `tempel-complete' if you want to see all matches, but then you
;;     ;; should also configure `tempel-trigger-prefix', such that Tempel
;;     ;; does not trigger too often when you don't expect it. NOTE: We add
;;     ;; `tempel-expand' *before* the main programming mode Capf, such
;;     ;; that it will be tried first.
;;     (setq-local completion-at-point-functions
;;                 (cons #'tempel-complete
;;                       completion-at-point-functions)))
;;   (add-hook 'prog-mode-hook 'tempel-setup-capf)
;;   (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package citre
  :disabled
  :bind (:map citre-mode-map
	 ("C-x c j" . citre-jump)
	 ("C-x c J" . citre-jump-back)
	 ("C-x c p" . citre-ace-peek)
	 ("C-x c u" . citre-update-this-tags-file))
  :init
  (require 'citre)
  (require 'citre-config)
  :config
  (setq
   citre-readtags-program "/etc/profiles/per-user/dez/bin/readtags"
   citre-ctags-program "/etc/profiles/per-user/dez/bin/ctags"
   ;; Set this if you want to always use one location to create a tags file.
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package elec-pair
  ;; :straight nil
  :config (electric-pair-mode))

(use-package electric
  ;; :straight nil
  :config (electric-indent-mode))

(use-package rainbow-delimiters
  :hook emacs-lisp-mode)

;; (use-package highlight-indent-guides
;;   :defer 0.5
;;   :hook prog-mode
;;   :custom
  ;; (highlight-indent-guides-auto-enabled nil)
;;   (highlight-indent-guides-delay 0)
;;   (highlight-indent-guides-method 'character))
  ;; :config
  ;; (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  ;; (set-face-background 'highlight-indent-guides-even-face "dimgray")
  ;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray"))

;; (use-package aggressive-indent
;;   :hook (emacs-lisp-mode lisp-mode python-ts-mode))

(use-package rainbow-mode
  :hook (web-mode js2-mode emacs-lisp-mode))

(use-package format-all
  :hook prog-mode)

(with-eval-after-load 'prog-mode
  (add-hook #'prog-mode-hook 'hs-minor-mode))

(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)))


;; lsp-bridge
(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (use-package yasnippet-snippets)
  (yas-reload-all))

(use-package lsp-bridge
  ;; :straight nil
  :custom
  (lsp-bridge-enable-completion-in-minibuffer t)
  (lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
  (lsp-bridge-signature-show-with-frame-position "top-right")
  (lsp-bridge-enable-with-tramp t)
  (lsp-bridge-enable-hover-diagnostic t)

  (acm-candidate-match-function 'orderless-flex)
  (acm-backend-yas-match-by-trigger-keyword t)
  (acm-enable-tabnine t)
  (acm-enable-tempel t)
  ;; (acm-enable-codeium t)
  ;; (acm-enable-citre t)
  (lsp-bridge-enable-mode-line nil)
  ;; :bind (:map acm-mode-map
  ;; 	      ("C-n" . acm-select-next)
  ;; 	      ("C-p" . acm-select-prev))
  :config
  (global-lsp-bridge-mode)

  (setq lsp-bridge-get-project-path-by-filepath
	(lambda (filepath)
          (when (locate-dominating-file filepath ".envrc")
	    (expand-file-name (locate-dominating-file filepath ".envrc"))))))

(use-package dape
  :commands (dape dape-toggle-breakpoint)
  :custom
  (dape-key-prefix "\C-x\C-a")
  (dape-repl-use-shorthand t)
  :config
  (add-to-list 'dape-configs
               `(debugpy
		 modes (python-ts-mode python-mode)
		 command "python3"
		 command-args ("-m" "debugpy.adapter")
		 :type "executable"
		 :request "launch"
		 :cwd dape-cwd-fn
		 :program dape-find-file-buffer-default)))

(use-package jupyter
 :commands (jupyter-run-repl jupyter-connect-repl))

(use-package ein
 :commands (ein:run ein:login))

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

;; (setq c-default-style "gnu")


(use-package nix-mode
  :mode "\\.nix\\'")

;; (use-package nixos-options
;;   :after nix-mode)

;; (use-package nix-sandbox
;;   :after nix-mode)

;; (use-package nix-update
;;   :after nix-mode)

;;        (:file-match "\\.lisp\\'"))

(use-package zig-mode
  :mode "\\.zig\\'")

(use-package sly
  :mode "\\.lisp\\'")

(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package tex
  ;; :straight nil
  :ensure auctex
  :config
  (defun remove-tex-trash()
    (interactive)
    (let ((current-directory default-directory)
	  (extensions '("\\.log\\'" "\\.out\\'" "\\.aux\\'")))
      (dolist (ext extensions)
	(dolist (file (directory-files current-directory nil ext))
	  (delete-file (concat current-directory file)))))))

(use-package cdlatex
  :hook
  ((Tex-latex-mode . #'turn-on-cdlatex)
   (org-mode . org-cdlatex-mode)))

(use-package markdown-mode
  :mode "\\.md\\'"
  :custom
  (markdown-command "multimarkdown"))

(use-package edit-indirect
  :disabled
  :after markdown-mode)

(use-package gdscript-mode
  :mode "\\.gd\\'")

(use-package swift-mode
  :mode "\\.swift\\'"
  :config
  (defun xcode-build()
    (interactive)
    (shell-command-to-string
     "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'build targetProject' -e 'end tell'"))
  (defun xcode-run()
    (interactive)
    (shell-command-to-string
     "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'run targetProject' -e 'end tell'"))
  (defun xcode-test()
    (interactive)
    (shell-command-to-string
     "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'test targetProject' -e 'end tell'"))
  (defun xcode-open-current-file()
    (interactive)
    (shell-command-to-string
     (concat "open -a \"/Applications/Xcode.app\" " (buffer-file-name)))))

;; (use-package flycheck-swift
;;   :after swift-mode
;;   :config
;;   (eval-after-load 'flycheck '(flycheck-swift-setup)))

(use-package zig-mode
  :mode "\\.zig\\'")


(use-package docker
  :bind ("C-c d" . docker))

;; (use-package flycheck
;;   :hook (lsp-mode . flycheck-mode)
;;   :config
;;   (setq flycheck-emacs-lisp-load-path 'inherit)

;;   ;; Rerunning checks on every newline is a mote excessive.
;;   (delq 'new-line flycheck-check-syntax-automatically)
;;   ;; And don't recheck on idle as often
;;   (setq flycheck-idle-change-delay 1.0)

;;   ;; For the above functionality, check syntax in a buffer that you switched to
;;   ;; only briefly. This allows "refreshing" the syntax check state for several
;;   ;; buffers quickly after e.g. changing a config file.
;;   (setq flycheck-buffer-switch-check-intermediate-buffers t)

;;   ;; Display errors a little quicker (default is 0.9s)
;;   (setq flycheck-display-errors-delay 0.25))

;;; LSP
;; Should boost performance with lsp
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;; (use-package lsp-mode
;;   :hook ((python-mode web-mode js2-mode typescript-ts-mode tsx-ts-mode rjsx-mode) . lsp)
;;   :bind ((:map lsp-mode-map
;;                ("M-<return>" . lsp-execute-code-action)))
;;   :commands (lsp lsp-deferred)
;;   :init
;;   (setenv "LSP_USE_PLISTS" "1")
;;   ;; Increase the amount of data emacs reads from processes
;;   (setq read-process-output-max (* 1024 1024))
;;   (setq lsp-clients-clangd-args '("--header-insertion-decorators=0"
;;                                   "--clang-tidy"
;;                                   "--enable-config"))
;;   ;; Disable features that have great potential to be slow.
;;   (setq lsp-enable-folding nil
;;         lsp-enable-text-document-color nil)
;;   ;; Reduce unexpected modifications to code
;;   (setq lsp-enable-on-type-formatting nil)
;;   ;; Make breadcrumbs opt-in; they're redundant with the modeline and imenu
;;   (setq lsp-headerline-breadcrumb-enable nil)

;;   ;; General lsp-mode settings
;;   (setq lsp-completion-provider :none
;;         lsp-enable-snippet nil
;;         lsp-enable-indentation nil
;;         lsp-idle-delay 0.500
;;         lsp-keymap-prefix "C-x L")
;;   ;; to enable the lenses
;;   (add-hook 'lsp-mode-hook #'lsp-lens-mode)
;;   (add-hook 'lsp-completion-mode-hook
;;             (lambda ()
;;               (setf (alist-get 'lsp-capf completion-category-defaults)
;;                     '((styles . (orderless flex))))))
;;   :config
;;   (defun dw/with-lsp-completion()
;;     (setq-local completion-at-point-functions
;;                 (list (cape-capf-buster
;;                        (cape-super-capf
;;                         #'lsp-completion-at-point
;;                         #'tempel-complete
;;                         #'cape-dabbrev
;;                         #'tabnine-completion-at-point)))))
;;   (add-hook 'lsp-completion-mode-hook #'dw/with-lsp-completion))

;; (use-package lsp-tailwindcss
;;   :commands (lsp lsp-deferred lsp-restart-workspace)
;;   :init
;;   (setq lsp-tailwindcss-add-on-mode t)
;;   (setq lsp-tailwindcss-major-modes '(tsx-ts-mode rjsx-mode web-mode css-mode)))

;; (use-package lsp-sourcekit
;;   :after lsp-mode
;;   :config
;;   (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

;; (use-package lsp-haskell
;;   :hook (haskell-mode . lsp-deferred))

;; (use-package lsp-java
;;   :hook (java-mode . lsp-deferred)
;;   :config
;;   (require 'lsp-java-boot)

;;   ;; to enable the lenses
;;   (add-hook 'lsp-mode-hook #'lsp-lens-mode)
;;   (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode))

;; (use-package lsp-pyright
;;   :hook (python-ts-mode . (lambda ()
;;                             (require 'lsp-pyright)
;;                             (lsp-deferred))))  ; or lsp-deferred

;; (use-package lsp-ui
;;   :after lsp-mode
;;   :init
;;   (setq lsp-ui-doc-max-height 8
;;         lsp-ui-doc-max-width 72         ; 150 (default) is too wide
;;         lsp-ui-doc-delay 0.75           ; 0.2 (default) is too naggy
;;         ;; lsp-ui doc
;;         lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover
;;         lsp-ui-doc-show-with-cursor t
;;         ;; lsp-ui sideline
;;         lsp-ui-sideline-show-hover nil
;;         lsp-ui-sideline-show-code-actions t))

;; ;;; Debugging
;; (use-package dap-mode
;;   :commands (dap-debug dap-debug-last)
;;   :bind (:map dap-mode-map
;;               ("C-x D D" . dap-debug)
;;               ("C-x D d" . dap-debug-last))
;;   :config
;;   (with-eval-after-load 'python-mode
;;     (require 'dap-python)
;;     ;; if you installed debugpy, you need to set this
;;     ;; https://github.com/emacs-lsp/dap-mode/issues/306
;;     (setq dap-python-debugger 'debugpy))

;;   (with-eval-after-load 'c++-mode
;;     (require 'dap-gdb-lldb)
;;     (dap-gdb-lldb-setup))
;;   (setq dap-auto-configure-features '(sessions locals controls tooltip)))

(use-package eglot
  ;; :straight nil
  :commands (eglot eglot-ensure)
  :custom
  (eglot-inlay-hints-mode nil)
  (eglot-events-buffer-size 0)
  (eldoc-idle-delay 1))

(use-package emmet-mode
  :when (daemonp)
  :hook ((web-mode css-ts-mode css-mode js2-mode rjsx-mode tsx-ts-mode) . emmet-mode)
  :custom
  ()
  :config
  (add-to-list 'emmet-jsx-major-modes 'tsx-ts-mode))

(use-package envrc
  :hook (after-init . dw/maybe-enable-envrc-global-mode)
  :config
  (defun dw/maybe-enable-envrc-global-mode ()
    "Enable `envrc-global-mode' if `direnv' is installed."
    (when (executable-find "direnv")
      (envrc-global-mode)))

  (with-eval-after-load 'envrc
    (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)))

;; Copy from https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-shell.el
;; Better term
;; @see https://github.com/akermu/emacs-libvterm#installation
(use-package vterm
  :commands (vterm vterm-posframe-toggle)
  :bind ("C-c `" . vterm-posframe-toggle)
  :custom
  (vterm-max-scrollback 10000)
  :config
  (with-no-warnings
    (when (posframe-workable-p)
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
                   :accept-focus t)))))))

  (dw/leader-key-def
    "'" 'vterm-posframe-toggle))

(use-package multi-vterm
  :commands (multi-vterm))

(use-package vterm-toggle
  :commands  (vterm-toggle-cd))

(use-package eshell
  ;; :straight nil
  :commands (eshell)
  :config
  (setq eshell-directory-name "~/.dotfiles/Emacs/eshell/")
  
  (if (executable-find "exa")
      (defalias 'eshell/ls 'exa)))

(use-package eshell-prompt-extras
  :after esh-opt
  :config
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package eshell-up
  :after esh-mode
  :custom
  (eshell-up-ignore-case nil))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-z
  :after esh-mode)

(use-package esh-help
  :after esh-mode
  :config
  (setup-esh-help-eldoc))

;; (use-package eat
;; 		  :host codeberg
;; 		  :repo "akib/emacs-eat"
;; 		  :files ("*.el" ("term" "term/*.el") "*.texi"
;; 			  "*.ti" ("terminfo/e" "terminfo/e/*")
;; 			  ("terminfo/65" "terminfo/65/*")
;; 			  ("integration" "integration/*")
;; 			  (:exclude ".dir-locals.el" "*-tests.el")))
;;   :hook (eshell-mode . eat-eshell-mode))

(use-package magit
  :commands (magit magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-delta
  :hook magit-mode)

(use-package password-store
  :commands (password-store-copy)
  :custom
  (password-store-password-length 12))

(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))


(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq tramp-default-method "ssh"
      tramp-default-user "wangpe90"
      tramp-default-host "dh2026pc25.utm.utoronto.ca")
(setq tramp-auto-save-directory temporary-file-directory
      backup-directory-alist (list (cons tramp-file-name-regexp nil)))

(setq erc-server "irc.libera.chat"
      erc-nick "dezzw"    ; Change this!
      erc-user-full-name "Desmond Wang"  ; And this!
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))
      erc-kill-buffer-on-part t
      erc-auto-query 'bury)

(use-package circe
  :commands (circe)
  :config
  (setq circe-network-options
        '(("irc.libera.chat"
           :tls t
           :port 6697
           :nick "dezzw"
           :sasl-username "dezzw"
           :sasl-password "Irc0x577063"
           :channels ("#emacs-circe")))))

;; (require 'init-tabbar)

;;; lang
;; (require 'init-treesit)
;; (require 'init-python)
;; (require 'init-clojure)
