(use-package tramp
  :straight nil
  :config
  ;; Enable full-featured Dirvish over TRAMP on certain connections
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1.
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:YOUR_HOSTNAME:")
                     "direct-async-process" t))
  ;; Tips to speed up connections
  (setq tramp-verbose 0)
  (setq tramp-chunksize 2000)
  (setq tramp-use-ssh-controlmaster-options nil))
