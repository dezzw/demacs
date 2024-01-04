;;; -*- lexical-binding: t -*-

(defun dw/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  ;; (auto-fill-mode 0)
  (setq evil-auto-indent nil)
  (visual-line-mode 1))

(use-package org
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

(if (eq system-type 'darwin)
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

(use-package org-download
  :hook (org-mode . org-download-enable)
  :custom
  (org-download-image-dir "./images/"))

(provide 'init-org)