;;; init-telega.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Configuration for telega using use-package.
;;; Code: 

(defun +telega-webpage-open-url-in-xwidget ()
  (interactive)
  (let ((entry-link
         (if (eq major-mode 'telega-chat-mode)
             (telega-url-at-point))))
    (xwidget-webkit-browse-url entry-link)))

;; 补全
;; (defun +telega-completion-setup ()
;;   (make-variable-buffer-local 'completion-at-point-functions)
;;   (setq completion-at-point-functions
;;         (append (mapcar #'cape-company-to-capf telega-company-backends)
;;                 completion-at-point-functions))
;;   (corfu-mode 1))

(defun +telega-save-file-to-clipboard (msg)
  "Save file at point to clipboard.
NOTE: macOS only."
  (interactive (list (telega-msg-for-interactive)))
  (let ((file (telega-msg--content-file msg)))
    (unless file
      (user-error "No file associated with message"))
    (telega-file--download file 32
      (lambda (dfile)
        (telega-msg-redisplay msg)
        (message "Wait for downloading to finish…")
        (when (telega-file--downloaded-p dfile)
          (let* ((fpath (telega--tl-get dfile :local :path)))
            (shell-command (format "osascript -e 'set the clipboard to POSIX file \"%s\"'" fpath))
            (message (format "File saved to clipboard: %s" fpath))))))))

(defun +telega-msg-save-to-cloud-copyleft (msg)
  "Save messages's MSG media content to a file.
     If MSG is an animation message, then possibly add animation to
     the saved animations list."
  (interactive (list (telega-msg-for-interactive)))
  (let ((file (telega-msg--content-file msg)))
    (unless file
      (user-error "No file associated with message"))
    (telega-file--download file 32
      (lambda (dfile)
        (telega-msg-redisplay msg)
        (when (telega-file--downloaded-p dfile)
          ;; TODO: This might be executed in process filter, so
          ;; pressing C-g will trigger "error in process filter: Quit"
          ;; Need to execute this outside of process filter
          (let* ((fpath (telega--tl-get dfile :local :path))
                 (fname (file-name-nondirectory fpath)))
            (telega--sendMessage
             (telega-chat-me)
             (list :@type "inputMessageDocument"
                   :document (telega-chatbuf--gen-input-file
                              fpath 'Document)
                   :caption (telega-fmt-text "#copyleft")
                   :disable_content_type_detection nil))
            (message (format "Saved to cloud: %s" fname))))))))

(use-package telega
  :straight '(:type git :host github :repo "LuciusChen/telega.el"
		    :files (:defaults "contrib/*.el" "etc" "server" "Makefile"))
  :defer t
  :custom
  (telega-avatar-workaround-gaps-for '(return t))
  (telega-video-player-command "iina")
  (telega-notifications-mode 1)
  (telega-notifications-msg-temex '(and (not outgoing)
                                        (not (chat (or (type channel))))
                                        (contains "[eE]macs\\|@wpcdes")))

  :bind (:map telega-prefix-map
              ("C-c t" . telega-prefix-map)
              ("p" . telega-chatbuf-filter-search)
              ("d" . telega-chat-remove-member)
              ("h" . telega-notifications-history)
              ("x" . telega-chatbuf-thread-cancel))
  :init
  ;; Ensure global keybinding is functional
  (global-set-key (kbd "C-c t") telega-prefix-map)
  :config
  ;;       telega-open-file-function 'org-open-file
  ;;       telega-chat-fill-column 90
  ;;       telega-sticker-size '(6 . 24)
  ;;       telega-translate-to-language-by-default "zh"
  ;;       telega-msg-save-dir "~/Downloads"
  ;;       telega-chat-input-markups '("markdown2" "org")
  ;;       telega-autoplay-mode 1
  ;;       telega-url-shorten-regexps
  ;;       (list `(too-long-link
  ;;               :regexp "^\$begin:math:text$https?://\\$end:math:text$\$begin:math:text$.\\\\{55\\\\}\\$end:math:text$.*?$"
  ;;               :symbol ,(nerd-icons-faicon "nf-fa-link")
  ;;               :replace " \\1\\2..."))
  ;;       telega-root-default-view-function 'telega-view-folders
  ;;       telega-root-keep-cursor 'track
  ;;       telega-root-show-avatars nil
  ;;       telega-root-buffer-name "*Telega Root*"
  ;;       telega-filters-custom nil
  ;;       telega-root-fill-column 70
  ;;       telega-filter-custom-show-folders nil
  ;;       telega-chat-folders-insexp nil
  ;;       telega-symbol-verified (nerd-icons-codicon "nf-cod-verified_filled" :face 'telega-blue)
  ;;       telega-symbol-vertical-bar "│"
  ;;       telega-symbol-flames (nerd-icons-mdicon "nf-md-delete_clock")
  ;;       telega-symbol-checkmark (nerd-icons-mdicon "nf-md-check")
  ;;       telega-symbol-heavy-checkmark (nerd-icons-codicon "nf-cod-check_all"))

  ;; Add hooks for telega modes
  ;; (add-hook 'telega-chat-mode-hook
  ;;           (lambda ()
  ;;             (electric-pair-local-mode -1)))
  ;; (add-hook 'telega-msg-ignore-predicates
  ;;           (telega-match-gen-predicate 'msg '(sender is-blocked)))

  ;; Telega-specific modes
  (global-telega-url-shorten-mode 1)
  (global-telega-mnz-mode 1)

  ;; macOS-specific settings
  (define-key telega-msg-button-map (kbd "C") '+telega-save-file-to-clipboard)
  (define-key telega-msg-button-map (kbd "s") '+telega-msg-save-to-cloud-copyleft)
  (setq telega-symbol-forum (nerd-icons-mdicon "nf-md-format_list_text")))


(provide 'init-telega)
;;; init-telega.el ends here
