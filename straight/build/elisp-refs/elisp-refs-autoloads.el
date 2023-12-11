;;; elisp-refs-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:



;;; Generated autoloads from elisp-refs.el

(autoload 'elisp-refs-function "elisp-refs" "\
Display all the references to function SYMBOL, in all loaded
elisp files.

If called with a prefix, prompt for a directory to limit the search.

This searches for functions, not macros. For that, see
`elisp-refs-macro'.

(fn SYMBOL &optional PATH-PREFIX)" t)
(autoload 'elisp-refs-macro "elisp-refs" "\
Display all the references to macro SYMBOL, in all loaded
elisp files.

If called with a prefix, prompt for a directory to limit the search.

This searches for macros, not functions. For that, see
`elisp-refs-function'.

(fn SYMBOL &optional PATH-PREFIX)" t)
(autoload 'elisp-refs-special "elisp-refs" "\
Display all the references to special form SYMBOL, in all loaded
elisp files.

If called with a prefix, prompt for a directory to limit the search.

(fn SYMBOL &optional PATH-PREFIX)" t)
(autoload 'elisp-refs-variable "elisp-refs" "\
Display all the references to variable SYMBOL, in all loaded
elisp files.

If called with a prefix, prompt for a directory to limit the search.

(fn SYMBOL &optional PATH-PREFIX)" t)
(autoload 'elisp-refs-symbol "elisp-refs" "\
Display all the references to SYMBOL in all loaded elisp files.

If called with a prefix, prompt for a directory to limit the
search.

(fn SYMBOL &optional PATH-PREFIX)" t)
(register-definition-prefixes "elisp-refs" '("elisp-"))

;;; End of scraped data

(provide 'elisp-refs-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; elisp-refs-autoloads.el ends here
