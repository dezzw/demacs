;;; init-project.el --- python related -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(use-package project
  :config
  (setq project-vc-extra-root-markers '("CMakeLists.txt" ".project")))

  ;; (defun my-project-try-local (dir)
  ;;   "Determine if DIR is a project root using `project-vc-extra-root-markers'."
  ;;   (interactive)
  ;;   (if-let ((root (seq-some (lambda (marker)
  ;;                              (locate-dominating-file dir marker))
  ;;                            project-vc-extra-root-markers)))
  ;;       (cons 'transient root)))

  ;; (defun my-project-try-vc-with-local-priority (dir)
  ;;   "Try to find a project root in DIR, prioritizing local markers over VC."
  ;;   (interactive)
  ;;   (or (my-project-try-local dir)
  ;;       (project-try-vc dir))))

  ;; (advice-add 'project-try-vc :override #'my-project-try-vc-with-local-priority))

;;; use project-x to enhance the project.el
;; (use-package project-x
;;   :straight (project-x :type git :host github :repo "karthink/project-x")
;;   :after project
;;   :custom
;;   (project-x-local-identifier
;;    '("package.json" "deps.edn" "CMakeLists.txt" "project.clj" ".envrc" ".tags" ".project"))
;;   :config
;;   (project-x-mode 1))

(provide 'init-project)
;;; init-project.el ends here
