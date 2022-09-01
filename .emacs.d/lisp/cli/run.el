;;; lisp/cli/run.el --- launching Emacs in a sandbox -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;
;;; Variables

;; None yet!


;;
;;; Commands

(defcli! run
    (;; TODO Implement sandbox functionality post-3.0
     ;; (daemon?     ("--daemon"))
     ;; (window-type ("--gui" "--tty"))
     ;; (version     ("--with-emacs" version))
     ;; (doomversion ("--with-doom" version))
     ;; (profile     ("--profile" name))
     (repl?  ("--repl") "Launch an elisp REPL")
     ;; &multiple
     ;; (calls  ("-f" "--funcall" fn))
     ;; (loads  ("-l" "--load" file))
     ;; (evals  (     "--eval" form))
     &context context
     &input input
     &rest args)
  "Launch Doom Emacs or an Emacs sandbox

Opens from bin/doom's parent directory.

Keep in mind there is some overhead opening Doom this way. For the best
performance, it is best to run Doom out of ~/.config/emacs or ~/.emacs.d."
  :benchmark nil
  ;; TODO Implement sandbox functionality post-3.0
  ;; (when version
  ;;   (unless (executable-find "nix-shell")
  ;;     (user-error "--emacs option is not supported without nix"))
  ;;   ...)
  (if repl?
      (if input
          ;; Evaluate piped-in text directly, if given.
          (eval (read input) t)
        (doom-run-repl context))
    ;; TODO Does this work on Windows?
    (let* ((tempdir      (doom-path (temporary-file-directory) "doom.run"))
           (tempemacsdir (doom-path tempdir ".emacs.d")))
      (delete-directory tempdir t)
      (make-directory tempemacsdir t)
      (with-temp-file (doom-path tempemacsdir "early-init.el")
        (prin1 `(progn
                  (setenv "HOME" ,(getenv "HOME"))
                  (setq user-emacs-directory ,doom-emacs-dir)
                  (load-file ,(doom-path doom-emacs-dir "early-init.el")))
               (current-buffer)))
      (exit! (format "HOME=%S %s %s"
                     tempdir
                     invocation-name
                     (combine-and-quote-strings args))))))


;;
;;; Helpers

(defun doom-run-repl (context)
  "Launch a rudimentary Elisp REPL."
  ;; I wrote this for fun; not with any serious intention of adding a
  ;; fully-fledged REPL to the Doom CLI. Still, I occasionally need to check
  ;; something, and once this has nix integration and can sandbox Emacs versions
  ;; separately, it may be useful for quick tests and demos.
  (let (form)
    (while (setq form (read-from-minibuffer "(elisp) $ "))
      (when (member form '(":quit" ":q"))
        (print! "\nGoodbye!")
        (exit! 0))
      (let (debug-on-error)
        (condition-case e
            (print! "%S" (eval (read form) t))
          (error
           (let* ((n 0)
                  (frame (backtrace-frame n))
                  (frame-list nil)
                  (in-program-stack t))
             (while frame
               (when in-program-stack
                 (push (cdr frame) frame-list))
               ;; (when (eq (elt frame 1) 'doom-run-repl)
               ;;   (setq in-program-stack t))
               (when (eq (elt frame 1) 'doom-run-repl)
                 (setq in-program-stack nil))
               (setq n (1+ n)
                     frame (backtrace-frame n)))
             (let* ((depth doom-cli-backtrace-depth)
                    (print-escape-newlines t))
               (print! (error "There was an unexpected error"))
               (print-group!
                (print! "%s %s" (bold "Message:") (error-message-string e))
                (print! "%s %S" (bold "Details:") (cdr e))))))))
      (terpri))))

(provide 'doom-cli-run)
;;; run.el ends here
