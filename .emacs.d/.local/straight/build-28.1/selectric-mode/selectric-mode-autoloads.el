;;; selectric-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "selectric-mode" "selectric-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from selectric-mode.el

(defvar selectric-mode nil "\
Non-nil if Selectric mode is enabled.
See the `selectric-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `selectric-mode'.")

(custom-autoload 'selectric-mode "selectric-mode" nil)

(autoload 'selectric-mode "selectric-mode" "\
Toggle Selectric mode.
Interactively with no argument, this command toggles the mode.  A
positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

This is a minor mode.  If called interactively, toggle the
`Selectric mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='selectric-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

When Selectric mode is enabled, your Emacs will sound like an IBM
Selectric typewriter.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "selectric-mode" '("selectric-"))

;;;***

(provide 'selectric-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; selectric-mode-autoloads.el ends here
