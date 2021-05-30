;;; solaire-mode.el --- make certain buffers grossly incandescent -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2021 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: Jun 03, 2017
;; Modified: May 21, 2021
;; Version: 1.1.5
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
(defface solaire-default-face '((t (:inherit default)))
  "Alternative version of the `default' face."
  :group 'solaire-mode)

(defface solaire-fringe-face '((t (:inherit solaire-default-face)))
  "Alternative version of the `fringe' face."
  :group 'solaire-mode)

(defface solaire-line-number-face
  `((t (:inherit (,(if (boundp 'display-line-numbers) 'line-number 'linum)
                  solaire-default-face))))
  "Alternative face for `line-number'.
Used by native line numbers in Emacs 26+ and `linum'."
  :group 'solaire-mode)

(defface solaire-hl-line-face '((t (:inherit hl-line)))
  "Alternative face for the current line, highlighted by `hl-line'."
  :group 'solaire-mode)

(defface solaire-org-hide-face '((t (:inherit org-hide)))
  "Alternative face for `org-hide'.
Used to camoflauge the leading asterixes in `org-mode' when
`org-hide-leading-stars' is non-nil."
  :group 'solaire-mode)

(defface solaire-mode-line-face '((t (:inherit mode-line)))
  "Alternative face for the `mode-line' face."
  :group 'solaire-mode)

(defface solaire-mode-line-inactive-face '((t (:inherit mode-line-inactive)))
  "Alternative face for the `mode-line-inactive' face."
  :group 'solaire-mode)

(defface solaire-header-line-face '((t (:inherit header-line)))
  "Alternative face for the `header-line' face."
  :group 'solaire-mode)


;;
;;; Options

(defcustom solaire-mode-real-buffer-fn #'solaire-mode-real-buffer-p
  "The function that determines buffer eligability for `solaire-mode'.

Should accept one argument: the buffer."
  :group 'solaire-mode
  :type 'function)

(defcustom solaire-mode-remap-headerline t
  "If non-nil, remap the `header-line' face as well."
  :group 'solaire-mode
  :type 'boolean)

(defcustom solaire-mode-remap-modeline t
  "If non-nil, remap mode-line faces as well.

Solaire-mode can conflict with certain mode-line plugins, like powerline and
telephone-line, so it's best to simply turn this off for those plugins."
  :group 'solaire-mode
  :type 'boolean)

(defcustom solaire-mode-remap-line-numbers nil
  "If non-nil, remap line number faces as well.

Canonically, the `linum' and `line-number' faces should inherit from `default'
and have no `:background' property; this prevents mismatched backgrounds when
solaire-mode is active. If your theme doesn't do this, set this to non-nil and
line number faces will be remapped to `solaire-line-number-face'."
  :group 'solaire-mode
  :type 'boolean)

(defcustom solaire-mode-remap-fringe
  (eval-when-compile (not (version<= emacs-version "26")))
  "If non-nil, buffer-locally remap the fringe face, otherwise do so globally."
  :group 'solaire-mode
  :type 'boolean)

(defcustom solaire-mode-remap-alist
  '(((default solaire-default-face)                       . t)
    ((hl-line solaire-hl-line-face)                       . (bound-and-true-p hl-line-mode))
    ((org-hide solaire-org-hide-face)                     . (derived-mode-p 'org-mode))
    ((org-indent solaire-org-hide-face)                   . (derived-mode-p 'org-mode))
    ((fringe solaire-fringe-face)                         . (and solaire-mode-remap-fringe (display-graphic-p)))
    ((linum solaire-line-number-face)                     . solaire-mode-remap-line-numbers)
    ((line-number solaire-line-number-face)               . solaire-mode-remap-line-numbers)
    ((header-line solaire-header-line-face)               . solaire-mode-remap-headerline)
    ((mode-line solaire-mode-line-face)                   . solaire-mode-remap-modeline)
    ((mode-line-inactive solaire-mode-line-inactive-face) . solaire-mode-remap-modeline)
    ((highlight-indentation-face solaire-hl-line-face)    . (featurep 'highlight-indentation)))
  "An alist of faces to remap when enabling `solaire-mode'."
  :group 'solaire-mode
  :type '(list face))

(defcustom solaire-mode-swap-alist
  '((t (default . solaire-default-face)
       (mode-line . solaire-mode-line-face)
       (mode-line-inactive . solaire-mode-line-inactive-face)
       (header-line . solaire-header-line-face)
       (fringe . solaire-fringe-face))
    (hl-line (hl-line . solaire-hl-line-face))
    (org-faces (org-hide . solaire-org-hide-face)))
  "An alist of faces to swap.

This is used when `solaire-mode-auto-swap-bg' is non-nil and the current theme
is in `solaire-mode-themes-to-face-swap'.

The CAR is the package to wait for before performing the swap.
The CDR is a list of cons cells mapping the original face to the new face to
remap them to."
  :group 'solaire-mode
  :type '(list face))

(defcustom solaire-mode-auto-swap-bg t
  "If non-nil, swap the backgrounds of faces and their solaire counterparts.

How solaire-mode works is it remaps many faces to solaire-mode counterparts. In
order to make file-visiting buffers \"brighter\", it remaps `default' with
`solaire-default-face', and has to assume that the latter has the brighter
:background. Or more specifically, it is assumed that `default' will be the
\"darker\" face. Since this isn't always the case, set this to non-nil so these
faces are swapped each time a theme is loaded."
  :group 'solaire-mode
  :type 'boolean)

(defcustom solaire-mode-themes-to-face-swap '()
  "A list of themes to automatically swap bg faces for.

Each rule can be a regexp string (tested against the name of the theme being
loaded), the name of a theme (symbol), or a function predicate that takes one
argument (the currently loaded theme) and returns true if that theme's bg faces
should be swapped.

If the regexp or symbol matches the current theme (or the function returns
non-nil), `solaire-mode-swap-faces-maybe' is used.

If `solaire-mode-auto-swap-bg' is nil, this variable is ignored."
  :group 'solaire-mode
  :type '(repeat (or symbol regexp function)))

(defvar solaire-mode--supported-p nil)
(defvar solaire-mode--swapped-faces nil)
(defvar solaire-mode--theme nil)
(defvar-local solaire-mode--remaps nil)


;;
;;; Helpers

(defun solaire-mode-real-buffer-p ()
  "Return t if the current buffer is a real (file-visiting) buffer."
  (buffer-file-name (buffer-base-buffer)))

(defun solaire-mode--swap-faces (feature faces)
  "Swap the face specs of FACES when FEATURE is available.

If FEATURE is t, swap FACES immediately."
  (with-eval-after-load
      (if (eq feature t)
          'solaire-mode  ; load immediately
        feature)
    (dolist (pair faces)
      (let ((face1 (car pair))
            (face2 (cdr pair)))
        (unless (memq face1 solaire-mode--swapped-faces)
          (push face1 solaire-mode--swapped-faces)
          (let* ((custom--inhibit-theme-enable nil)
                 (theme-settings (get solaire-mode--theme 'theme-settings))
                 (spec1 (cl-loop for spec in theme-settings
                                 for face = (nth 1 spec)
                                 if (eq face face1)
                                 return (nth 3 spec)))
                 (spec2 (cl-loop for spec in theme-settings
                                 for face = (nth 1 spec)
                                 if (eq face face2)
                                 return (nth 3 spec))))
            (when (and spec1 spec2)
              (custom-theme-set-faces
               'solaire-swap-bg-theme
               `(,face1 ,spec2)
               `(,face2 ,spec1)))))))))

(defun solaire-mode-swap-faces-maybe ()
  "Globally swap the current theme's background faces.

See `solaire-mode-swap-alist' for list of faces that are swapped.
See `solaire-mode-themes-to-face-swap' for themes where faces will e swapped.
Does nothing if `solaire-mode-auto-swap-bg' is nil."
  (when (and solaire-mode--supported-p
             solaire-mode-auto-swap-bg
             (null solaire-mode--swapped-faces)
             (cl-find-if
              (lambda (rule)
                (cond ((functionp rule)
                       (funcall rule solaire-mode--theme))
                      ((stringp rule)
                       (string-match-p rule (symbol-name solaire-mode--theme)))
                      ((symbolp rule)
                       (eq solaire-mode--theme rule))))
              solaire-mode-themes-to-face-swap))
    (push t solaire-mode--swapped-faces)
    (let ((swap-theme 'solaire-swap-bg-theme))
      (custom-declare-theme swap-theme nil)
      (put swap-theme 'theme-settings nil)
      (dolist (swap solaire-mode-swap-alist)
        (solaire-mode--swap-faces (car swap) (cdr swap)))
      (enable-theme swap-theme))))


;;
;;; `solaire-mode' / `solaire-global-mode'

;;;###autoload
(define-minor-mode solaire-mode
  "Make current buffer a different color to make others grossly incandescent.

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
        (when (eval (cdr remap))
          (push (apply #'face-remap-add-relative (car remap))
                solaire-mode--remaps)))
      ;; Update the fringe, in case it was remapped. We don't cycle
      ;; `fringe-mode' because it affects all frames, which is overkill.
      (and (bound-and-true-p fringe-mode)
           (display-graphic-p)
           solaire-mode-remap-fringe
           (modify-frame-parameters
            nil (list (cons 'left-fringe
                            (if (consp fringe-mode)
                                (car fringe-mode)
                              fringe-mode))
                      (cons 'right-fringe
                            (if (consp fringe-mode)
                                (cdr fringe-mode)
                              fringe-mode))))))))

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
  "Reset all buffers with `solaire-mode' enabled.

Use this in case solaire-mode has screwed up colors in your frame, e.g. when
changing themes. Issues will be more prelevant in Emacs 25 and 26, but far less
so in 27+; particularly where the fringe is concerned."
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
;;; Compatibility fixes

(declare-function persp-buffer-list "persp-mode")
(declare-function persp-load-state-from-file "persp-mode")
(declare-function transient--insert-groups "transient")
(defvar evil-buffer-regexps)
(defvar hl-line-range-function)
(defvar which-key--buffer)

(defun solaire-mode-fix-minibuffer (&optional unset)
  "Create minibuffer/echo area buffers and insert whitespace into them.

If UNSET, kill these buffers instead.

Emacs will always display one of *Minibuf-X* or *Echo Area X* (where X is 0 or
1) in the minibuffer area. If these buffers don't exist OR they exist and are
empty, they will be transparent, showing the background color of `default', but
`solaire-mode' wants it to display `solaire-default-face' instead.

To do this, we create these buffers early and insert whitespace into them."
  (dolist (buf '(" *Minibuf-0*" " *Minibuf-1*"
                 " *Echo Area 0*" " *Echo Area 1*"))
    (with-current-buffer (get-buffer-create buf)
      (if (or unset (not solaire-global-mode))
          (let (kill-buffer-query-functions)
            (kill-buffer buf))
        ;; HACK Fix #41: evil initializes itself in these buffers, whether or not
        ;;      `evil-want-minibuffer' (or `evil-collection-setup-minibuffer') is
        ;;      non-nil, which is unwanted.
        (setq-local evil-buffer-regexps '((".")))
        (when (= (buffer-size) 0)
          (insert " "))
        ;; Don't allow users to kill these buffers, as it destroys the illusion
        (add-hook 'kill-buffer-query-functions #'ignore nil 'local)
        (solaire-mode (if solaire-global-mode +1 -1))))))

(defun solaire-mode-fix-term-mode ()
  "Replace `term' face with `solaire-default-face' in `ansi-term-color-vector'.

Meant to fix mismatched background on text in term buffers (should be used on
`solaire-mode-hook')."
  (when (derived-mode-p 'term-mode)
    (if (not solaire-mode)
        (kill-local-variable 'ansi-term-color-vector)
      (make-local-variable 'ansi-term-color-vector)
      (setf (elt ansi-term-color-vector 0) 'solaire-default-face))))

(defun solaire-mode-fix-fringe ()
  "Toggle the `fringe's new background.
If ARG is non-nil, match `solaire-fringe-face's background, otherwise
`default's."
  (when solaire-mode-remap-fringe
    (set-face-background
     'fringe
     (if solaire-mode
         (face-background 'default)
       (unless (cl-find-if (apply-partially #'buffer-local-value 'solaire-mode)
                           (buffer-list))
         (face-background 'solaire-fringe-face nil t))))))

(defun solaire-mode-fix-create-image-bg-a (image)
  "Create IMAGE with the correct (solaire-ized) background color."
  (when solaire-mode
    (plist-put (cdr image)
               :background (face-background 'solaire-default-face
                                            nil t)))
  image)

(defun solaire-mode-fix-persp-mode-buffers-a (&rest _)
  "Restore `solaire-mode' in new `persp-mode' sessions."
  (dolist (buf (persp-buffer-list))
    (with-current-buffer buf
      (turn-on-solaire-mode))))

(defun solaire-mode--hide-fringes-in-minibuffer-h (&rest _)
  "Hide the fringe in the minibuffer.
A global fringe color means the minibuffer (with its fringes) will always stand
out, so we remove them (in which-key popups too)."
  (when solaire-mode-remap-fringe
    (set-window-fringes (minibuffer-window) 0 0 nil)))

(defun solaire-mode--hide-fringes-in-which-key-buffer-a (&rest _)
  "Hide the minibuffer in the `which-key' window."
  (when solaire-mode-remap-fringe
    (solaire-mode--hide-fringes-in-minibuffer-h)
    (set-window-fringes (get-buffer-window which-key--buffer) 0 0 nil)))

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

(defun solaire-mode--enable-if-global ()
  "Activate `solaire-mode' in buffer if `solaire-global-mode' is active."
  (when solaire-global-mode
    (solaire-mode +1)))

(defvar solaire-mode--last-state nil)
(put 'solaire-mode--last-state 'permanent-local t)
(defun solaire-mode--remember ()
  "Remember `solaire-mode's state for `solaire-mode--restore' to restore it."
  (setq-local solaire-mode--last-state solaire-mode))
(defun solaire-mode--restore ()
  "Restore `solaire-mode' if it was enabled."
  (when (and solaire-mode--last-state (not solaire-mode))
    (solaire-mode +1)
    (kill-local-variable 'solaire-mode--last-state)))


;;
;;; Side effects

;; face-remap is buggy in Emacs 26 and older. Upgrades people!
(when (< emacs-major-version 27)
  ;; HACK The fringe cannot have a buffer-local remapping on Emacs <= 26, so we
  ;;      jump through hoops to reset it (globally) whenever it is likely that
  ;;      the fringe will have lost its background color.
  (advice-add #'load-theme :after #'solaire-mode-reset)

  ;; Don't complain about `focus-in-hook' being obsolete.
  (with-no-warnings
    ;; HACK Fringe can become unstyled when deleting or refocusing frames
    (add-hook 'focus-in-hook #'solaire-mode-reset))

  ;; HACK Hide the fringe in the minibuffer or which-key frames, since it serves
  ;;      no purpose there, and its incorrect color stands out as ugly.
  (add-hook 'solaire-mode-hook #'solaire-mode--hide-fringes-in-minibuffer-h)
  (add-hook 'minibuffer-setup-hook #'solaire-mode--hide-fringes-in-minibuffer-h)
  (add-hook 'window-configuration-change-hook #'solaire-mode--hide-fringes-in-minibuffer-h)
  (with-eval-after-load 'which-key
    (advice-add 'which-key--show-buffer-side-window
                :after #'solaire-mode--hide-fringes-in-which-key-buffer-a))

  ;; Our last ditch effort to keep fringes in line (they occasionally forget
  ;; their color due to face shenanigans on Emacs 26).
  (add-hook 'solaire-mode-hook #'solaire-mode-fix-fringe)

  ;; HACK On Emacs <=26, when point is on the last (or second to last) line and
  ;;      solaire-mode is remapping the hl-line face, hl-line's highlight bleeds
  ;;      into the rest of the window after eob. On Emacs 27 this no longer
  ;;      happens. This tries to fix it for 26 users, but it imposes another
  ;;      problem: the 2nd-to-last line will only be highlighted up to the
  ;;      (n-1)th character, but I think that is the lesser evil.
  (setq hl-line-range-function #'solaire-mode-hl-line-range-fn))

;; HACK Restore solaire-mode in persp-mode buffers when a perspective is loaded.
(with-eval-after-load 'persp-mode
  (advice-add #'persp-load-state-from-file :after #'solaire-mode-fix-persp-mode-buffers-a))

;; which-key and transient create their popups in split windows/frames, in
;; fundamental-mode, which `solaire-global-mode' doesn't reach. This fixes that.
(with-eval-after-load 'which-key
  (add-hook 'which-key-init-buffer-hook #'solaire-mode--enable-if-global))

(with-eval-after-load 'transient
  (advice-add #'transient--insert-groups :before #'solaire-mode--enable-if-global))

;; Ensure inline images are created with the correct background color.
(with-eval-after-load 'image
  (advice-add #'create-image :filter-return #'solaire-mode-fix-create-image-bg-a))

;; Fix background color for text in `term'/`ansi-term'
(add-hook 'solaire-mode-hook #'solaire-mode-fix-term-mode)

;; Reset solaire-mode so its remappings assimilate the remappings done by
;; `mixed-pitch-mode' and `variable-pitch-mode'.
(add-hook 'mixed-pitch-mode-hook #'solaire-mode-reset-buffer)
(add-hook 'buffer-face-mode-hook #'solaire-mode-reset-buffer)

;; Don't let revert-buffer disable `solaire-mode's effects
(add-hook 'before-revert-hook #'solaire-mode--remember)
(add-hook 'after-revert-hook #'solaire-mode--restore)

;; Make sure the minibuffer always has solaire-mode active in it
(add-hook 'solaire-global-mode-hook #'solaire-mode-fix-minibuffer)

;; HACK Due to `map-y-or-n-p's unfortunate implementation, an ugly white
;;      background is visible from EOL to EOW when the minibuffer's background
;;      is remapped. This function is used in some visible places, like when
;;      prompting to save buffers when quitting Emacs.
;;
;;      This doesn't totally fix the issue, but makes it more tolerable (by
;;      using `default's unremapped background).
(defun solaire-mode-fix-map-y-or-n-p-a (orig-fn &rest args)
  "Advice to fix ugly white background in `map-y-or-n-p' due to `solaire-mode'.
ORIG-FN is `map-y-or-n-p' and ARGS are its arguments."
  (unwind-protect
      (progn
        (solaire-mode-fix-minibuffer 'unset)
        (apply orig-fn args))
    (solaire-mode-fix-minibuffer)))
(advice-add #'map-y-or-n-p :around #'solaire-mode-fix-map-y-or-n-p-a)


;;
;;; Bootstrap

;;;###autoload
(progn
  (defun solaire-mode--prepare-for-theme-a (theme &rest _)
    "Prepare solaire-mode for THEME.
Meant to be used as a `load-theme' advice."
    (when (and
           ;; Make sure THEME is a real theme (not a psuedo theme like
           ;; `solaire-swap-bg-theme').
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
            solaire-mode--swapped-faces nil
            solaire-mode--theme theme)  ; reset swap
      (when (bound-and-true-p solaire-global-mode)
        (if solaire-mode--supported-p
            (solaire-mode-swap-faces-maybe)
          (solaire-global-mode -1)))))
  (advice-add #'load-theme :after #'solaire-mode--prepare-for-theme-a))

(defun solaire-mode--prepare-for-active-theme ()
  "Check if a theme has been loaded before `solaire-global-mode' was activated."
  (let ((theme
         (or solaire-mode--theme
             ;; This is only necessary if the user has loaded the package
             ;; without its autoloads, which is unusual, but may be necessary
             ;; for testing (e.g. in a sandbox).
             (cl-find-if (lambda (th) (get th 'theme-feature))
                         custom-enabled-themes))))
    (and (symbolp theme)
         (custom-theme-enabled-p theme)
         (solaire-mode--prepare-for-theme-a theme))))

(add-hook 'solaire-global-mode-hook #'solaire-mode--prepare-for-active-theme)

;; Give `solaire-global-mode' a change to swap faces as early as possible.
(add-hook 'solaire-global-mode-hook #'solaire-mode-swap-faces-maybe 'append)

(provide 'solaire-mode)
;;; solaire-mode.el ends here
