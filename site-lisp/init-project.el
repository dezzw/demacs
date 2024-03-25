;;; -*- lexical-binding: t -*-

;; (defun ct/dir-contains-project-marker (dir)
;;   "Checks if `.project' file is present in directory at DIR path."
;;   (interactive)
;;   (let ((project-marker-path (file-name-concat dir ".tags")))
;;     (when (file-exists-p project-marker-path)
;;        dir)))
;; (customize-set-variable 'project-find-functions
;;                         (list #'project-try-vc
;;                               #'ct/dir-contains-project-marker))

;;; use project-x to enhance the project.el
(use-package project-x
  :straight (project-x :type git :host github :repo "karthink/project-x")
  :after project
  :custom
  (project-x-local-identifier
   '("package.json" "deps.edn" "project.clj" ".envrc" ".tags" ".project")) 
  :config
  ;; (setq project-x-save-interval 600)    ;Save project state every 10 min
  (project-x-mode 1))

(provide 'init-project)
