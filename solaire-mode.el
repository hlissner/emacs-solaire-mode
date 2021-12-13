;;; solaire-mode.el --- make certain buffers grossly incandescent -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2021 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <contact@henrik.io>
;; Created: Jun 3, 2017
;; Modified: December 13, 2021
;; Version: 2.0.4
;; Keywords: dim bright window buffer faces
;; Homepage: https://github.com/hlissner/emacs-solaire-mode
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; `solaire-mode' is inspired by editors who visually distinguish code-editing
;; windows from sidebars, popups, terminals, ecetera. It changes the background
;; of file-visiting buffers (and certain aspects of the UI) to make them easier
;; to distinguish from other, not-so-important buffers.
;;
;; Praise the sun.
;;
;;; Installation
;;
;; M-x package-install RET solaire-mode
;;
;;   (require 'solaire-mode)
;;   (solaire-global-mode +1)
;;
;; And to unconditionally darken certain buffers:
;;
;;   (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
;;
;;; Code:

(require 'cl-lib)
(require 'face-remap)

(defgroup solaire-mode nil
  "Options for solaire-mode."
  :group 'faces)


;;
;;; Faces

;;;###autoload
(defface solaire-default-face '((t :inherit default))
  "Alternative version of the `default' face."
  :group 'solaire-mode)

(defface solaire-fringe-face '((t :inherit solaire-default-face))
  "Alternative version of the `fringe' face."
  :group 'solaire-mode)

(defface solaire-line-number-face
  `((t :inherit (,(if (boundp 'display-line-numbers) 'line-number 'linum)
                 solaire-default-face)))
  "Alternative face for `line-number'.
Used by native line numbers in Emacs 26+ and `linum'."
  :group 'solaire-mode)

(defface solaire-hl-line-face '((t :inherit hl-line))
  "Alternative face for the current line, highlighted by `hl-line'."
  :group 'solaire-mode)

(defface solaire-org-hide-face '((t :inherit org-hide))
  "Alternative face for `org-hide'.
Used to camoflauge the leading asterixes in `org-mode' when
`org-hide-leading-stars' is non-nil."
  :group 'solaire-mode)

(defface solaire-region-face '((t :inherit region))
  "Alternative face for `region' (the active selection)."
  :group 'solaire-mode)

(defface solaire-mode-line-face '((t :inherit mode-line))
  "Alternative face for the `mode-line' face."
  :group 'solaire-mode)

(defface solaire-mode-line-active-face '((t :inherit mode-line-active))
  "Alternative face for the `mode-line-active' face (Emacs 29+)."
  :group 'solaire-mode)

(defface solaire-mode-line-inactive-face '((t :inherit mode-line-inactive))
  "Alternative face for the `mode-line-inactive' face."
  :group 'solaire-mode)

(defface solaire-header-line-face '((t :inherit header-line))
  "Alternative face for the `header-line' face."
  :group 'solaire-mode)


;;
;;; Options

(defcustom solaire-mode-real-buffer-fn #'solaire-mode-real-buffer-p
  "The function that determines buffer eligability for `solaire-mode'.

Should accept one argument: the buffer and return truthy for buffers where
`solaire-mode' should *not* be activated."
  :group 'solaire-mode
  :type 'function)

(defcustom solaire-mode-remap-alist
  `((default                    . solaire-default-face)
    (hl-line                    . solaire-hl-line-face)
    (region                     . solaire-region-face)
    (org-hide                   . solaire-org-hide-face)
    (org-indent                 . solaire-org-hide-face)
    (linum                      . solaire-line-number-face)
    (line-number                . solaire-line-number-face)
    (header-line                . solaire-header-line-face)
    (mode-line                  . solaire-mode-line-face)
    (mode-line-active           . solaire-mode-line-active-face)
    (mode-line-inactive         . solaire-mode-line-inactive-face)
    (highlight-indentation-face . solaire-hl-line-face)
    ,@(unless (version<= emacs-version "26")
        '((fringe               . solaire-fringe-face))))
  "An alist of faces to remap when enabling `solaire-mode'."
  :group 'solaire-mode
  :type '(repeat (cons (face :tag "Source face")
                       (face :tag "Destination face"))))

(defcustom solaire-mode-swap-alist (copy-sequence solaire-mode-remap-alist)
  "An alist of faces to swap.

This is used when the current theme is in `solaire-mode-themes-to-face-swap'.

The CAR is the package to wait for before performing the swap.
The CDR is a list of cons cells mapping the original face to the new face to
remap them to."
  :group 'solaire-mode
  :type '(repeat (cons (face :tag "Source face")
                       (face :tag "Destination face"))))

(defcustom solaire-mode-themes-to-face-swap '()
  "A list of themes to automatically swap bg faces for.

Each rule can be a regexp string (tested against the name of the theme being
loaded), the name of a theme (symbol), or a function predicate that takes one
argument (the currently loaded theme) and returns true if that theme's bg faces
should be swapped.

If the regexp or symbol matches the current theme (or the function returns
non-nil), `solaire-mode-swap-faces-maybe' is used."
  :group 'solaire-mode
  :type '(repeat (or symbol regexp function)))

(defvar solaire-mode--supported-p nil)
(defvar solaire-mode--swapped-p nil)
(defvar solaire-mode--theme nil)
(defvar-local solaire-mode--remaps nil)


;;
;;; Helpers

(defun solaire-mode-real-buffer-p ()
  "Return t if the current buffer is a real (file-visiting) buffer."
  (buffer-file-name (buffer-base-buffer)))

(defun solaire-mode--swap-faces (src-face dest-face)
  "Swap SRC-FACE's spec with DEST-FACE's.
If FEATURE is t, swap FACES immediately."
  (let* ((custom--inhibit-theme-enable nil)
         (theme-settings (get solaire-mode--theme 'theme-settings))
         (spec1 (cl-loop for spec in theme-settings
                         for face = (nth 1 spec)
                         if (eq face src-face)
                         return (nth 3 spec)))
         (spec2 (cl-loop for spec in theme-settings
                         for face = (nth 1 spec)
                         if (eq face dest-face)
                         return (nth 3 spec))))
    (when (and spec1 spec2)
      (custom-theme-set-faces
       'solaire-swapped-faces-theme
       `(,src-face ,spec2)
       `(,dest-face ,spec1)))))

(defun solaire-mode-swap-faces-maybe ()
  "Globally swap the current theme's background faces.

See `solaire-mode-swap-alist' for list of faces that are swapped.
See `solaire-mode-themes-to-face-swap' for themes where faces will be swapped."
  (when (and solaire-mode--supported-p
             (null solaire-mode--swapped-p)
             (cl-find-if
              (lambda (rule)
                (cond ((functionp rule)
                       (funcall rule solaire-mode--theme))
                      ((stringp rule)
                       (string-match-p rule (symbol-name solaire-mode--theme)))
                      ((symbolp rule)
                       (eq solaire-mode--theme rule))))
              solaire-mode-themes-to-face-swap))
    (let ((swap-theme 'solaire-swapped-faces-theme))
      (custom-declare-theme swap-theme nil)
      (put swap-theme 'theme-settings nil)
      (dolist (faces solaire-mode-swap-alist)
        (when (cdr faces)
          (solaire-mode--swap-faces (car faces) (cdr faces))))
      (enable-theme swap-theme)
      (setq solaire-mode--swapped-p t))))


;;
;;; `solaire-mode' / `solaire-global-mode'

;;;###autoload
(define-minor-mode solaire-mode
  "Make current buffer a different color so others can be grossly incandescent.

Remaps faces in `solaire-mode-remap-alist', then runs `solaire-mode-hook', where
additional mode-specific fixes may live. Lastly, adjusts the fringes for the
current frame."
  :lighter "" ; should be obvious it's on
  :init-value nil
  ;; Don't kick in if the current theme doesn't support solaire-mode.
  (if (not solaire-mode--supported-p)
      (setq solaire-mode nil)
    (mapc #'face-remap-remove-relative solaire-mode--remaps)
    (setq solaire-mode--remaps nil)
    (when solaire-mode
      (dolist (remap solaire-mode-remap-alist)
        (when (cdr remap)
          (push (face-remap-add-relative (car remap) (cdr remap))
                solaire-mode--remaps))))))

;;;###autoload
(define-globalized-minor-mode solaire-global-mode solaire-mode turn-on-solaire-mode)

;;;###autoload
(defun turn-on-solaire-mode (&rest _)
  "Conditionally enable `solaire-mode' in the current buffer.

Does nothing if the current buffer doesn't satisfy the function in
`solaire-mode-real-buffer-fn'."
  (interactive)
  (and (not solaire-mode)
       (or (minibufferp)
           (not (funcall solaire-mode-real-buffer-fn)))
       (solaire-mode +1)))

;;;###autoload
(defun turn-off-solaire-mode (&rest _)
  "Disable `solaire-mode' in the current buffer."
  (interactive)
  (when solaire-mode
    (solaire-mode -1)))

;;;###autoload
(defun solaire-mode-reset (&rest _)
  "Reset `solaire-mode' in all buffers where it is enabled.

Use this in case solaire-mode has caused some sort of problem, e.g. after
changing themes.  are more prelevant in Emacs 25 and 26, but far less so in 27+;
particularly where the fringe is concerned."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (solaire-mode-reset-buffer))))

;;;###autoload
(defun solaire-mode-reset-buffer ()
  "Reset `solaire-mode' incurrent buffer.

See `solaire-mode-reset' for details."
  (when solaire-mode
    (solaire-mode -1)
    (solaire-mode +1)))


;;
;;; Bootstrap

;;;###autoload
(progn
  ;; The `progn' assures the whole form is inserted into the package's autoloads
  ;; verbatim, rather than as `autoload' forms, so that this `load-theme' advice
  ;; is active as soon as possible.
  (defun solaire-mode--prepare-for-theme-a (theme &rest _)
    "Prepare solaire-mode for THEME.
Meant to be used as a `load-theme' advice."
    (when (and
           ;; Make sure THEME is a real theme (not a psuedo theme like
           ;; `solaire-swapped-faces-theme').
           (get theme 'theme-feature)
           ;; And that it's been successfully enabled.
           (memq theme custom-enabled-themes))
      (setq solaire-mode--supported-p
            (ignore-errors
              (let ((default1 (face-background 'default nil t))
                    (default2 (face-background 'solaire-default-face nil t)))
                (and default1
                     default2
                     (not (equal default1 default2)))))
            solaire-mode--swapped-p nil
            solaire-mode--theme theme)  ; reset swap
      (when (bound-and-true-p solaire-global-mode)
        (if solaire-mode--supported-p
            (solaire-mode-swap-faces-maybe)
          (solaire-global-mode -1)))))
  (advice-add #'load-theme :after #'solaire-mode--prepare-for-theme-a))

(defvar evil-buffer-regexps)
(defun solaire-mode-fix-minibuffer (&optional unset)
  "Create minibuffer/echo area buffers to enable `solaire-mode' in them.

If UNSET, resets these buffers instead.

Emacs will always display one of *Minibuf-N* or *Echo Area N* (where X is 0 or
1) in the minibuffer area. If these buffers don't exist OR they exist and are
empty, they will be transparent, showing the (incorrect) background color of
`default', but we want it to display `solaire-default-face' instead, so we
create these buffers early and insert whitespace in them."
  (dolist (buf '(" *Minibuf-0*" " *Minibuf-1*"
                 " *Echo Area 0*" " *Echo Area 1*"))
    (with-current-buffer (get-buffer-create buf)
      (if (or unset (not solaire-global-mode))
          (solaire-mode -1)
        ;; HACK Fix #41: evil initializes itself in these buffers, whether or
        ;;      not `evil-want-minibuffer' or `evil-collection-setup-minibuffer'
        ;;      is non-nil, which is unwanted.
        (setq-local evil-buffer-regexps '((".")))
        ;; Minibuffers must be non-empty for solaire-default-face to apply to
        ;; the whole line (otherwise it terminates at BOL).
        (when (= (buffer-size) 0)
          (insert " "))
        ;; Don't allow users to kill these buffers, as it destroys the hack
        (add-hook 'kill-buffer-query-functions #'ignore nil 'local)
        (solaire-mode +1)))))

;; Make sure the minibuffer always has solaire-mode active in it.
(add-hook 'solaire-global-mode-hook #'solaire-mode-fix-minibuffer)

;; Give `solaire-global-mode' a change to swap faces as early as possible.
(add-hook 'solaire-global-mode-hook #'solaire-mode-swap-faces-maybe)

(defun solaire-mode--auto-detect-theme ()
  "Initialize solaire-mode for a recently enabled theme.

This is only necessary if `solaire-mode--prepare-for-theme-a' wasn't executed
when the user loaded their latest theme. E.g. the user loads this package
without its autoloads. Normally, you shouldn't directly call this."
  (unless solaire-mode--theme
    (let ((theme
           (cl-find-if (lambda (th) (get th 'theme-feature))
                       custom-enabled-themes)))
      (and (symbolp theme)
           (custom-theme-enabled-p theme)
           (solaire-mode--prepare-for-theme-a theme)))))

(solaire-mode--auto-detect-theme)


;;
;;; Package compatibility hackery

(with-no-warnings  ; Shush, byte-compiler! Trust in Solaire.
  (when (< emacs-major-version 27)
    ;;; Fixes for Emacs <=26 fringes
    ;; HACK The fringe cannot have a buffer-local remapping on Emacs <= 26, so
    ;;      we jump through hoops to reset it (globally) whenever it is likely
    ;;      that the fringe has lost its background color.
    (advice-add #'load-theme :after #'solaire-mode-reset)

    ;; HACK The fringe can become unstyled when deleting or refocusing frames.
    (add-hook 'focus-in-hook #'solaire-mode-reset)

    ;; HACK Hide the fringe in the minibuffer or which-key frames, since it
    ;;      serves no purpose there, and its incorrect color stands out as ugly.
    (defun solaire-mode--hide-fringes-in-minibuffer-h (&rest _)
      "Hide the fringe in the minibuffer.
A global fringe color means the minibuffer (with its fringes) will always stand
out, so we remove them (in which-key popups too).

Only necessary for Emacs <= 26."
      (when (assq 'fringe solaire-mode-remap-alist)
        (set-window-fringes (minibuffer-window) 0 0 nil)))

    (defun solaire-mode--hide-fringes-in-which-key-buffer (&rest _)
      "Hide the minibuffer in the `which-key' window.
Only necessary for Emacs <= 26."
      (when (assq 'fringe solaire-mode-remap-alist)
        (solaire-mode--hide-fringes-in-minibuffer-h)
        (set-window-fringes (get-buffer-window which-key--buffer) 0 0 nil)))

    (add-hook 'solaire-mode-hook #'solaire-mode--hide-fringes-in-minibuffer-h)
    (add-hook 'minibuffer-setup-hook #'solaire-mode--hide-fringes-in-minibuffer-h)
    (add-hook 'window-configuration-change-hook #'solaire-mode--hide-fringes-in-minibuffer-h)
    (with-eval-after-load 'which-key
      (advice-add 'which-key--show-buffer-side-window
                  :after #'solaire-mode--hide-fringes-in-which-key-buffer))

    ;; Our last ditch effort to keep fringes in line (they occasionally forget
    ;; their color due to face shenanigans on Emacs 26).
    (defun solaire-mode-fix-fringe ()
      "Toggle the `fringe's new background.
If ARG is non-nil, match `solaire-fringe-face's background, otherwise
`default's.

Only necessary for Emacs <= 26."
      (when (assq 'fringe solaire-mode-remap-alist)
        (set-face-background
         'fringe
         (if solaire-mode
             (face-background 'default nil t)
           (unless (cl-find-if (apply-partially #'buffer-local-value 'solaire-mode)
                               (buffer-list))
             (face-background 'solaire-fringe-face nil t))))))
    (add-hook 'solaire-mode-hook #'solaire-mode-fix-fringe)

    ;;; `hl-line'
    ;; HACK On Emacs <=26, when point is on the last (or second to last) line
    ;;      and solaire-mode is remapping the hl-line face, hl-line's highlight
    ;;      bleeds into the rest of the window after eob. On Emacs 27 this no
    ;;      longer happens. This tries to fix it for 26 users, but it imposes
    ;;      another problem: the 2nd-to-last line will only be highlighted up to
    ;;      the (n-1)th character, but I think that is the lesser evil.
    (defun solaire-mode-hl-line-range-fn ()
      "Adjust `hl-line's overlay to avoid EOB.

This is needed to avoid a face-remap bug in Emacs 26 and earlier that causes
hl-line's overlay to spill out into the rest of the window."
      (if solaire-mode
          (let ((bol (line-beginning-position))
                (eol (line-end-position))
                (eob (point-max)))
            (cond ((= bol eob)
                   nil)
                  ((= (1+ eol) eob)
                   (cons bol (1- eob)))
                  ((or (= eol eob) (eobp))
                   (cons bol eol))
                  ((cons bol (line-beginning-position 2)))))
        (cons (line-beginning-position)
              (line-beginning-position 2))))
    (setq hl-line-range-function #'solaire-mode-hl-line-range-fn))

  ;; which-key and transient create their popups in splits and in
  ;; fundamental-mode, which `solaire-global-mode' can't reach. This fixes that.
  (defun solaire-mode--enable-if-global ()
    "Activate `solaire-mode' in buffer if `solaire-global-mode' is active."
    (when solaire-global-mode
      (solaire-mode +1)))

  ;;; which-key
  (with-eval-after-load 'which-key
    (add-hook 'which-key-init-buffer-hook #'solaire-mode--enable-if-global))

  ;;; transient
  (with-eval-after-load 'transient
    (advice-add #'transient--insert-groups :before #'solaire-mode--enable-if-global))

  ;;; image
  (with-eval-after-load 'image
    (define-advice create-image (:filter-return (image) fix-background-color)
      "Create IMAGE with the correct (solaire-ized) background color."
      (when solaire-mode
        (plist-put (cdr image)
                   :background (face-background 'solaire-default-face
                                                nil t)))
      image))

  ;;; mini-modeline
  (with-eval-after-load 'mini-modeline
    (define-advice mini-modeline--set-buffer-face (:after (&rest _) fix-mismatched-fringe)
      "Fix the mismatched fringe in the `mini-modeline' minibuffer.
Meant to be used as `:after' advice for `mini-modeline--set-buffer-face'."
      (when solaire-mode
        (push (face-remap-add-relative 'fringe mini-modeline-face-attr)
              solaire-mode--remaps))))

  ;;; persp-mode
  (with-eval-after-load 'persp-mode
    (define-advice persp-load-state-from-file (:after (&rest _) restore-solaire-mode)
      "Restore `solaire-mode' in new `persp-mode' sessions."
      (dolist (buf (persp-buffer-list))
        (with-current-buffer buf
          (turn-on-solaire-mode)))))

  ;;; term / ansi-term
  (defun solaire-mode--fix-term-mode ()
    "Replace `term' with `solaire-default-face' in `ansi-term-color-vector'.
Meant to fix mismatched background on text in term buffers (should be used on
`solaire-mode-hook')."
    (when (derived-mode-p 'term-mode)
      (if (not solaire-mode)
          (kill-local-variable 'ansi-term-color-vector)
        (make-local-variable 'ansi-term-color-vector)
        (setf (elt ansi-term-color-vector 0) 'solaire-default-face))))
  (add-hook 'solaire-mode-hook #'solaire-mode--fix-term-mode)

  ;;; mixed-pitch / variable-pitch
  ;; Reset solaire-mode so its remappings assimilate the remappings done by
  ;; `mixed-pitch-mode' and `variable-pitch-mode'.
  (add-hook 'mixed-pitch-mode-hook #'solaire-mode-reset-buffer)
  (add-hook 'buffer-face-mode-hook #'solaire-mode-reset-buffer)

  ;;; `revert-buffer'
  ;; Don't let revert-buffer disable `solaire-mode's effects
  (put (defvar solaire-mode--last-state nil) 'permanent-local t)

  (defun solaire-mode--remember ()
    "Remember `solaire-mode's state for `solaire-mode--restore' to restore it."
    (setq-local solaire-mode--last-state solaire-mode))
  (add-hook 'before-revert-hook #'solaire-mode--remember)

  (defun solaire-mode--restore ()
    "Restore `solaire-mode' if it was enabled."
    (when (and solaire-mode--last-state (not solaire-mode))
      (solaire-mode +1)
      (kill-local-variable 'solaire-mode--last-state)))
  (add-hook 'after-revert-hook #'solaire-mode--restore)

  ;;; `map-y-or-n-p'
  ;; HACK Due to `map-y-or-n-p's unfortunate implementation, an ugly white
  ;;      background is visible from EOL to EOW when the minibuffer's background
  ;;      is remapped. This function is used in some visible places, like when
  ;;      prompting to save buffers when quitting Emacs.
  ;;
  ;;      This doesn't totally fix the issue, but makes it more tolerable (by
  ;;      using `default's unremapped background).
  (define-advice map-y-or-n-p (:around (orig-fn &rest args) fix-background)
    "Fix ugly white background in `map-y-or-n-p' due to `solaire-mode'.
ORIG-FN is `map-y-or-n-p' and ARGS are its arguments."
    (unwind-protect
        (progn
          (solaire-mode-fix-minibuffer 'unset)
          (apply orig-fn args))
      (solaire-mode-fix-minibuffer))))

(provide 'solaire-mode)
;;; solaire-mode.el ends here
