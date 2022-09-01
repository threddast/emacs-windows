;;; math-at-point-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "math-at-point" "math-at-point.el" (0 0 0 0))
;;; Generated autoloads from math-at-point.el

(autoload 'math-at-point-simple "math-at-point" "\
Evaluate the simple math expression at point with `calc-eval'.

A simple math expression consists of decimal numbers, and the
operations +, -, *, /, and ^, and can be interspersed with
whitespace. A simple math expression cannot contain parens. The
whole expression must on the current line.

The result is displayed in the minibuffer and copied into the
kill ring so that it can be pasted with ``yank''. If the point is
not inside a simple math expression, then instead run
``quick-calc''.

If optional prefix argument INSERT is provided,
then insert the evaluation result after the expression, prefixed
by \"=\". If there was already a previous result, then replace
it.

\(fn &optional INSERT)" t nil)

(autoload 'math-at-point "math-at-point" "\
Evaluate the math expression at point with `calc-eval'.

A math expression consists of decimal numbers, the operations +,
-, *, /, ^, and parentheses, and can be interspersed with
whitespace. The whole expression must be fully contained in the
current line. If the point is inside a LaTeX math fragment, then
the math expression can also contain LaTeX syntax.

The result is displayed in the minibuffer and copied into the
kill ring (so that it can be pasted with ``yank''). If the point
is not within a math expression, then instead run `quick-calc'.

If optional prefix argument INSERT is provided, then insert the
evaluation result after the expression, prefixed by \"=\". If
there was already a previous result, then replace it.

\(fn &optional INSERT)" t nil)

(autoload 'math-at-point-expression "math-at-point" "\
Evaluate the math expression at point with `calc-eval'.

A math expression consists of decimal numbers, the operations +,
-, *, /, ^, and parentheses, and can be interspersed with
whitespace. The whole expression must be fully contained in the
current line.

The result is displayed in the minibuffer and copied into the
kill ring (so that it can be pasted with ``yank''). If the point
is not within a math expression, then instead run `quick-calc'.

If optional prefix argument INSERT is provided, then insert the
evaluation result after the expression, prefixed by \"=\". If
there was already a previous result, then replace it.

\(fn &optional INSERT)" t nil)

(autoload 'math-at-point-latex "math-at-point" "\
Evaluate the LaTeX math expression at point with `calc-eval'.

The result is displayed in the minibuffer and copied into the
kill ring (so that it can be pasted with ``yank''). If the point
is not within a LaTeX math expression, then instead run `quick-calc'.

If optional prefix argument INSERT is provided, then insert the
evaluation result after the expression, prefixed by \"=\". If
there was already a previous result, then replace it.

Optional argument PARAMS should contains a cons cell with the
left delimiter of the LaTeX fragment and its position. If PARAMS
isn't provided, it is set to the output
of (org-inside-LaTeX-fragment-p).

\(fn &optional INSERT PARAMS)" t nil)

(register-definition-prefixes "math-at-point" '("map-"))

;;;***

(provide 'math-at-point-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; math-at-point-autoloads.el ends here
