;;; package --- org-mode related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(use-package org
  :defer
  :straight t
  :hook
  ((org-mode . org-indent-mode)
   (org-mode . visual-line-mode))
  :config
  (setq org-html-head-include-default-style nil
        org-adapt-indentation t
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)

  ;; Since we don't want to disable org-confirm-babel-evaluate all
  ;; of the time, do it around the after-save-hook
  (defun dw/org-babel-tangle-dont-ask ()
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))
  
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dw/org-babel-tangle-dont-ask
						'run-at-end 'only-in-org-mode)))
  (if (eq system-type 'darwin)
      (setq org-agenda-files '("~/Documents/Org/Planner")))
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
          ("idea" . ?i)))
  )

(use-package org-contrib
  :straight t)

(use-package org-modern
  :straight t
  :hook org-mode
  :custom
  (org-modern-fold-stars '(("▶" . "▼") ("▷" . "▽") ("▹" . "▿") ("▸" . "▾") ("⯈" . "⯆"))))

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package visual-fill-column
  :hook org-mode
  :custom
  (visual-fill-column-width 110)
  (visual-fill-column-center-text t))

(use-package valign
  :hook org-mode)

(use-package org-appear
  :hook org-mode)

(use-package org-tidy
  :hook
  (org-mode . org-tidy-mode)
  :custom
  (org-tidy-properties-style 'fringe))

(with-eval-after-load "ob"
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (shell . t)
     (python . t)
     (sql . t)
     (jupyter . t)))

  (setq org-confirm-babel-evaluate nil))

(use-package org-super-agenda
  :hook org-agenda-mode
  :commands (org-agenda)
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
  :straight t
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

;; (use-package org-download
;;   :hook (org-mode . org-download-enable)
;;   :custom
;;   (org-download-image-dir "./images/"))

;; (use-package org-imagine
;;   :straight 

(defun org-insert-image ()
  "insert a image from clipboard"
  (interactive)
  (let* ((path (concat default-directory "img/"))
	 (image-file (concat
		      path
		      (buffer-name)
		      (format-time-string "_%Y%m%d_%H%M%S.png"))))
    (if (not (file-exists-p path))
	(mkdir path))
    (do-applescript (concat
		     "set the_path to \"" image-file "\" \n"
		     "set png_data to the clipboard as «class PNGf» \n"
		     "set the_file to open for access (POSIX file the_path as string) with write permission \n"
		     "write png_data to the_file \n"
		     "close access the_file"))
    ;; (shell-command (concat "pngpaste " image-file))
    (org-insert-link nil
		     (concat "file:" image-file)
		     "")
    (message image-file))
  (org-display-inline-images))

(provide 'init-org)
;;; init-org.el ends here
