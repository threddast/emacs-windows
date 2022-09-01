;;; centered-window-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "centered-window" "centered-window.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from centered-window.el

(autoload 'centered-window-mode-toggle "centered-window" nil nil nil)

(defvar centered-window-mode nil "\
Non-nil if Centered-Window mode is enabled.
See the `centered-window-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `centered-window-mode'.")

(custom-autoload 'centered-window-mode "centered-window" nil)

(autoload 'centered-window-mode "centered-window" "\
Minor mode to center text on the current buffer

This is a minor mode.  If called interactively, toggle the
`Centered-Window mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='centered-window-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "centered-window" '("cwm-"))

;;;***

(provide 'centered-window-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; centered-window-autoloads.el ends here
