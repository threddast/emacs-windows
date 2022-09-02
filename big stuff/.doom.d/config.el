;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Erik Giorgis"
      user-mail-address "giorgiserik@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-nord)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/roam/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq auto-save-default t
      make-backup-files t
      confirm-kill-emacs nil
      org-habit-show-habits-only-for-today nil
      fancy-splash-image (expand-file-name "images/doom_256x256.png" doom-private-dir)
      inhibit-x-resources t
      )

(global-auto-revert-mode)

;; theme
(defun synchronize-theme ()
  (let* ((light-theme 'doom-nord-light)
         (dark-theme 'doom-nord)
         (start-time-light-theme 5)
         (end-time-light-theme 18)
         (hour (string-to-number (substring (current-time-string) 11 13)))
         (next-theme (if (member hour (number-sequence start-time-light-theme end-time-light-theme))
                         light-theme dark-theme)))
    (when (not (equal doom-theme next-theme))
      (setq doom-theme next-theme)
      (load-theme next-theme t))))

(run-with-timer 0 900 'synchronize-theme)

(require 'json)
(defun threddast/get-secret (key)
  "Return the value of the json file secret for key"
  (cdr (assoc key (json-read-file (expand-file-name "secrets/secrets.json" doom-private-dir))))
  )

(setq initial-buffer-choice "~/roam/0-inbox/inbox.org")

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/roam")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
    '(("d" "default" plain "%?"
      :target (file+head "0-inbox/${slug}.org"
                         "#+title: ${title}\n\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
    '(("d" "default" plain "%?"
       :target (file+head "%<%Y-%m-%d>.org"
                          "#+title: %<%Y-%m-%d>\n\n")
       :unarrowed t)))
  (org-roam-dailies-directory "~/roam/5-logs/daily"))

(defun threddast/print-journal-template (date)
  "Returns the daily journal if the date is a weekday, weekly journal if date is Sunday"
  (let ((year  (string-to-number (substring date 0 4)))
        (month (string-to-number (substring date 5 7)))
        (day   (string-to-number (substring date 8))))
    (concat "#+title: " date "\n"
            (f-read-text "~/roam/templates/daily.org")
            (if (eq (calendar-day-of-week (list month day year)) 0)
              (f-read-text "~/roam/templates/weekly.org") nil))))

(defun threddast/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("t" "TODO" plain "* TODO %?"
                                  :target (file+head "0-inbox/inbox.org" "Inbox\n"))
                                  ("b" "BUY" plain "* BUY %?"
                                  :target (file+head "0-inbox/inbox.org" "Inbox\n"))
                                  ("i" "IDEA" plain "* IDEA %?"
                                  :target (file+head "0-inbox/inbox.org" "Inbox\n"))
                                  )))

(map! :leader
      (:prefix-map ("r" . "roam")
       (:desc "Insert node"       "i" #'org-roam-node-insert
        :desc "Find node"         "f" #'org-roam-node-find
        :desc "Today's journal"   "t" #'org-roam-dailies-goto-today
        :desc "Journal goto date" "d" #'org-roam-dailies-goto-date
        :desc "Capture to inbox"  "c" #'threddast/org-roam-capture-inbox
        :desc "Open agenda menu"  "a" #'org-agenda
        )))

(setq org-download-method 'directory)

(setq +latex-viewers '(evince))

(after! org
  (use-package! ox-extra
    :config
    (ox-extras-activate '(latex-header-blocks ignore-headlines))))

(after! org
  ;; Import ox-latex to get org-latex-classes and other funcitonality
  ;; for exporting to LaTeX from org
  (use-package! ox-latex
    :init
    ;; code here will run immediately
    :config
    ;; code here will run after the package is loaded
    (setq org-latex-pdf-process
          '("pdflatex -interaction nonstopmode -output-directory %o %f"
            "bibtex %b"
            "pdflatex -interaction nonstopmode -output-directory %o %f"
            "pdflatex -interaction nonstopmode -output-directory %o %f"))
    (setq org-latex-with-hyperref nil) ;; stop org adding hypersetup{author..} to latex export
    ;; (setq org-latex-prefer-user-labels t)

    ;; deleted unwanted file extensions after latexMK
    (setq org-latex-logfiles-extensions
          (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist")))

    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil)))
)

;; custom keybindings for noter
(map! :leader
      (:prefix ("n")
       (:desc "Insert note at point" "i" #'org-noter-insert-note)))
;; prevent from collapsing everything
(use-package org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; I want to see the whole file
   org-noter-hide-other t
   ;; I want to not open a new frame every time
   org-noter-always-create-frame nil
   )
  )
;; for lone truncation
(defun my/no-op (&rest args))
(advice-add 'org-noter--set-notes-scroll :override 'my/no-op)
;; noter pdf-tools
(use-package! org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package! org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
     With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(add-hook 'org-mode-hook
 #'(lambda ()
    (local-set-key [C-M-up] (quote org-table-move-single-cell-up))
    (local-set-key [C-M-down] (quote org-table-move-single-cell-down))
    (local-set-key [C-M-left] (quote org-table-move-single-cell-left))
    (local-set-key [C-M-right] (quote org-table-move-single-cell-right))))

(defun org-table-swap-cells (i1 j1 i2 j2)
  "Swap two cells"
  (let ((c1 (org-table-get i1 j1))
  (c2 (org-table-get i2 j2)))
    (org-table-put i1 j1 c2)
    (org-table-put i2 j2 c1)
    (org-table-align)))

(defun org-table-move-single-cell (direction)
  "Move the current cell in a cardinal direction according to the
  parameter symbol: 'up 'down 'left 'right. Swaps contents of
  adjacent cell with current one."
  (unless (org-at-table-p)
    (error "No table at point"))
  (let ((di 0) (dj 0))
    (cond ((equal direction 'up) (setq di -1))
          ((equal direction 'down) (setq di +1))
          ((equal direction 'left) (setq dj -1))
          ((equal direction 'right) (setq dj +1))
          (t (error "Not a valid direction, must be up down left right")))
    (let* ((i1 (org-table-current-line))
           (j1 (org-table-current-column))
           (i2 (+ i1 di))
           (j2 (+ j1 dj)))
      (org-table-swap-cells i1 j1 i2 j2)
      (org-table-goto-line i2)
      (org-table-goto-column j2))))

(defun org-table-move-single-cell-up ()
  "Move a single cell up in a table; swap with anything in target cell"
  (interactive)
  (org-table-move-single-cell 'up))

(defun org-table-move-single-cell-down ()
  "Move a single cell down in a table; swap with anything in target cell"
  (interactive)
  (org-table-move-single-cell 'down))

(defun org-table-move-single-cell-left ()
  "Move a single cell left in a table; swap with anything in target cell"
  (interactive)
  (org-table-move-single-cell 'left))

(defun org-table-move-single-cell-right ()
  "Move a single cell right in a table; swap with anything in target cell"
  (interactive)
  (org-table-move-single-cell 'right))

;; org-modern enable as minor mode
(global-org-modern-mode)

;; org-modern minimal design
 ;; Minimal UI
 ;; (package-initialize)
 ;; (menu-bar-mode -1)
 ;; (tool-bar-mode -1)
 ;; (scroll-bar-mode -1)

;; Choose some fonts
(set-face-attribute 'org-drawer nil :height 0.8)
(set-face-attribute 'org-document-title nil :height 1.5)
(use-package doom-themes
  :custom-face
  (org-ellipsis ((t (:height 0.8 :inherit 'shadow))))
  ;;(org-level-1 ((t (:inherit outline-1 :height 1.5))))
  )
 ;; (set-face-attribute 'default nil :family "???")
 ;; (set-face-attribute 'variable-pitch nil :family "???")
 ;; (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

;; Add frame borders and window dividers
 ;;(modify-all-frames-parameters
 ;; '((right-divider-width . 40)
 ;;   (internal-border-width . 40)))
 ;;(dolist (face '(window-divider
 ;;                window-divider-first-pixel
 ;;                window-divider-last-pixel))
 ;;  (face-spec-reset-face face)
 ;;  (set-face-foreground face (face-attribute 'default :background)))
 ;;(set-face-background 'fringe (face-attribute 'default :background))
 (setq
  ;; Edit settings
  org-auto-align-tags nil
  org-tags-column 0
  org-catch-invisible-edits 'show-and-error
  org-special-ctrl-a/e t
  org-insert-heading-respect-content t
  org-ellipsis  " ⬎ " ;; ⤵ ▼ 
  ;; Org styling, hide markup etc.
  org-hide-emphasis-markers t
  org-pretty-entities t
  org-cycle-separator-lines -1
  org-modern-todo nil
  org-fontify-done-headline nil
)



(setq org-modern-star '("◉" "○" "◈" "◇"))

(setq org-modern-todo-faces
      '(("WAIT" :background "#AC4426" :foreground "#ECEFF4" :weight bold)
        ("NEXT" :background "#398EAC" :foreground "#ECEFF4" :weight bold)
        ("TODO" :background "#4F894C" :foreground "#ECEFF4" :weight bold)
        ("SDAY" :background "#B48EAD" :foreground "#ECEFF4" :weight bold :height 1.0)
        ))
;; (setq org-todo-keyword-faces
;;  '(("TODO" . ,(doom-color 'green))
;;    ("NEXT" . ,(doom-color 'cyan))
;;    ("WAIT" . ,(doom-color 'orange))
;;    ("SDAY" . ,(doom-color 'magenta))
;;  ))

(define-key evil-window-map "j" 'evil-window-left)
(define-key evil-window-map "J" 'evil-window-move-far-left)
(define-key evil-motion-state-map "j" 'evil-backward-char)
(define-key evil-motion-state-map "J" 'evil-window-top)

(define-key evil-window-map "h" 'evil-window-down)
(define-key evil-window-map "H" 'evil-window-move-very-bottom)
(define-key evil-motion-state-map "h" 'evil-next-line)

(map! :leader
      (:prefix-map ("k" . "clock")
       (:desc "Org clock-in" "i" #'org-clock-in
        :desc "Org clock-out" "o" #'org-clock-out
        :desc "Org clock goto" "g" #'org-clock-goto)))

;; to get this to work I needed to install alsa-utils: aplay is used to play the sound
(setq org-clock-sound (expand-file-name "sounds/school_bell.wav" doom-private-dir))

 (require 'org-auto-tangle)
 (add-hook 'org-mode-hook 'org-auto-tangle-mode)

(setq cwm-incremental-padding t)
(setq cwm-use-vertical-padding t)
(setq cwm-incremental-padding-% 5)
(setq cwm-frame-internal-border 50)
(setq cwm-centered-window-width 3000)
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)
(setq indicate-empty-lines nil)
(defun threddast/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children)
  (org-latex-preview)
  )

(defun threddast/org-present-hook ()
  ;(setq-local face-remapping-alist '((header-line (:height 1.5) variable-pitch)))
  ;;                                    (header-line (:height 4.5) variable-pitch)
  ;;                                    (org-code (:height 1.55) org-code)
  ;;                                    (org-verbatim (:height 1.55) org-verbatim)
  ;;                                    (org-block (:height 1.25) org-block)
  ;;                                    (org-block-begin-line (:height 0.7) org-block)))
  (set-face-attribute 'header-line t :background "#ffffff")
  (setq header-line-format " ")
  (org-display-inline-images)
  (centered-window-mode 1)
  (display-line-numbers-mode 0)
  (org-present-hide-cursor)
  (threddast/org-present-prepare-slide))

(defun threddast/org-present-quit-hook ()
 ; (setq-local face-remapping-alist '((default variable-pitch default)))
  (org-present-show-cursor)
  (setq header-line-format nil)
  (centered-window-mode 0)
)

(defun threddast/org-present-prev ()
  (interactive)
  (org-present-prev)
  (threddast/org-present-prepare-slide))

(defun threddast/org-present-next ()
  (interactive)
  (org-present-next)
  (threddast/org-present-prepare-slide))

(use-package org-present
  :bind (:map org-present-mode-keymap
         ("[right]" . nil)
         ("[left]" . nil)
         ("C-<down>" . org-present-hide-cursor)
         ("C-<up>" . org-present-show-cursor)
         ("C-<right>" . threddast/org-present-next)
         ("C-<left>" . threddast/org-present-prev))
  :hook (;(org-present-mode . evil-normalize-keymaps)
         (org-present-mode . threddast/org-present-hook)
         (org-present-mode-quit . threddast/org-present-quit-hook)))

;; (add-hook 'cdlatex-mode-hook
;;  (lambda () (when (eq major-mode 'org-mode)
;;   (make-local-variable 'org-pretty-entities-include-sub-superscripts)
;;    (setq org-pretty-entities-include-sub-superscripts nil))))
(setq org-pretty-entities-include-sub-superscripts nil)

(setq yas-snippet-dirs '("~/.doom.d/snippets"))
(expand-file-name "snippets" doom-private-dir)
