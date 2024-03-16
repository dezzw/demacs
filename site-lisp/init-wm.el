;;; -*- lexical-binding: t -*-

(use-package tab-bar
  :straight nil
  :when (display-graphic-p)
  :hook (window-setup . tab-bar-mode)
  ;; :custom (tab-bar-new-tab-choice "*scratch*")
  :config
  (defun eli/tab-bar-svg-padding (width string)
    (let* ((style svg-lib-style-default)
         (margin      (plist-get style :margin))
         (txt-char-width  (window-font-width nil 'fixed-pitch))
         (tag-width (- width (* margin txt-char-width)))
         (padding (- (/ tag-width txt-char-width) (length string))))
      padding))

  (defun dw/tab-bar-tab-name-with-svg (tab i)
    (let* ((current-p (eq (car tab) 'current-tab))
           (name (concat (if tab-bar-tab-hints (format "%d " i) "")
			 (alist-get 'name tab)))
           (padding (plist-get svg-lib-style-default :padding)))
      (propertize
       name
       'display
       (svg-tag-make
	name
	:face (if (eq (car tab) 'current-tab) 'tab-bar-tab 'tab-bar-tab-inactive)
	:inverse t :margin 0 :radius 6 :padding padding))))

  (setq tab-bar-tab-name-format-function #'dw/tab-bar-tab-name-with-svg))


(use-package tabspaces
  :straight (:type git :host github :repo "mclear-tools/tabspaces")
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup. 
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  ;; sessions
  (tabspaces-session t)
  ;; (tabspaces-session-auto-restore t)
  
  :config
  ;; Filter Buffers for Consult-Buffer
  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
      (list :name     "Workspace Buffer"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
				  :predicate #'tabspaces--local-buffer-p
				  :sort 'visibility
				  :as #'buffer-name)))

      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace)))

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package winner
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

(provide 'init-wm)
