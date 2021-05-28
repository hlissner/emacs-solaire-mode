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

(defvar evil-buffer-regexps)

(defgroup solaire-mode nil
  "Options for solaire-mode."
  :group 'faces)


;;
;;; Faces

(defface solaire-default-face '((t (:inherit default)))
  "Alternative version of the `default' face."
  :group 'solaire-mode)

(defface solaire-fringe-face '((t (:inherit solaire-default-face)))
  "Alternative version of the `fringe' face."
  :group 'solaire-mode)

(defface solaire-minibuffer-face '((t (:inherit solaire-default-face)))
  "Alternative face for the minibuffer. See `solaire-mode-in-minibuffer'."
  :group 'solaire-mode)

(defface solaire-line-number-face
  `((t (:inherit (,(if (boundp 'display-line-numbers) 'line-number 'linum)
                  solaire-default-face))))
  "Alternative face for `line-number' (native line numbers in Emacs 26+) and
`linum'."
  :group 'solaire-mode)

(defface solaire-hl-line-face '((t (:inherit hl-line)))
  "Alternative face for the current line, highlighted by `hl-line'."
  :group 'solaire-mode)

(defface solaire-org-hide-face '((t (:inherit org-hide)))
  "Alternative face for `org-hide', which is used to camoflauge the leading
asterixes in `org-mode' when `org-hide-leading-stars' is non-nil."
  :group 'solaire-mode)

(defface solaire-mode-line-face '((t (:inherit mode-line)))
  "Alternative face for the mode line."
  :group 'solaire-mode)

(defface solaire-mode-line-inactive-face '((t (:inherit mode-line-inactive)))
  "Alternative face for the inactive mode line."
  :group 'solaire-mode)

(defface solaire-header-line-face '((t (:inherit header-line)))
  "Alternative face for the header line."
  :group 'solaire-mode)


;;
;;; Plugin faces

;; Forward-define these so I don't have to load their packages to manipulate
;; them (in `solaire-mode--swap-bg-faces-maybe').

(defface hl-line
  '((t :inherit highlight :extend t))
  "Default face for highlighting the current line in Hl-Line mode."
  :version "22.1"
  :group 'hl-line)


;;
;;; Options

(defcustom solaire-mode-real-buffer-fn #'solaire-mode--real-buffer-p
  "The function that determines buffer eligability for `solaire-mode'.

Should accept one argument: the buffer."
  :group 'solaire-mode
  :type 'function)

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
  "If non-nil, remap the fringe using `face-remap', otherwise change the face globally."
  :group 'solaire-mode
  :type 'boolean)

(defcustom solaire-mode-remap-alist
  '(((default solaire-default-face)                       . t)
    ((hl-line solaire-hl-line-face)                       . t)
    ((org-hide solaire-org-hide-face)                     . t)
    ((org-indent solaire-org-hide-face)                   . t)
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

(defcustom solaire-mode-themes-to-face-swap '()
  "A list of rules that determine if the bg faces should be swapped for the
current theme.

Each rule can be a regexp string (tested against the name of the theme being
loaded), the name of a theme (symbol), or a function that takes one argument:
the currently loaded theme.

If the regexp or symbol matches the current theme (or the function returns
non-nil), `solaire-mode--swap-bg-faces-maybe' is used."
  :group 'solaire-mode
  :type '(repeat (or symbol regexp function)))


;;
;;; Helpers

(defvar solaire-mode--bg-swapped nil)

(defun solaire-mode--real-buffer-p ()
  "Return t if the BUF is a file-visiting buffer."
  (buffer-file-name (buffer-base-buffer)))

(defun solaire-mode--swap-bg-faces-p ()
  "Return t if the current buffer is in `solaire-mode-theme-whitelist'."
  (let ((themes
         (cl-remove-if-not (lambda (x) (get x 'theme-feature))
                           custom-enabled-themes)))
    (and themes
         solaire-mode-auto-swap-bg
         (not solaire-mode--bg-swapped)
         (cl-find-if (lambda (rule)
                       (cl-find-if
                        (cond ((functionp rule) rule)
                              ((stringp rule)
                               (lambda (theme)
                                 (string-match-p rule (symbol-name theme))))
                              ((symbolp rule)
                               (apply-partially #'eq rule)))
                        themes))
                     solaire-mode-themes-to-face-swap))))

(defun solaire-mode--swap-faces (face1 face2 &optional prop)
  (let* ((prop (or prop :background))
         (color (face-attribute face1 prop nil)))
    (custom-theme-set-faces
     'solaire-swap-bg-theme
     `(,face1 ((t (,prop ,(face-attribute face2 prop nil)))))
     `(,face2 ((t (,prop ,color)))))))

(defvar ansi-color-names-vector)
(defun solaire-mode--swap-bg-faces-maybe ()
  "Swap the backgrounds of the following faces:

+ `default' <-> `solaire-default-face'
+ `hl-line' <-> `solaire-hl-line-face'

This is necessary for themes in the doom-themes package."
  (when (solaire-mode--swap-bg-faces-p)
    (let ((swap-theme 'solaire-swap-bg-theme))
      (custom-declare-theme swap-theme nil)
      (when (custom-theme-enabled-p swap-theme)
        (disable-theme swap-theme))
      (put swap-theme 'theme-settings nil)
      (solaire-mode--swap-faces 'default 'solaire-default-face)
      (solaire-mode--swap-faces 'hl-line 'solaire-hl-line-face)
      (enable-theme swap-theme)
      (setq solaire-mode--bg-swapped t))))

(defvar-local solaire-mode--remap-cookies nil)
;;;###autoload
(define-minor-mode solaire-mode
  "Make source buffers grossly incandescent by remapping common faces (see
`solaire-mode-remap-alist') to their solaire-mode variants."
  :lighter "" ; should be obvious it's on
  :init-value nil
  (if (null (face-background 'solaire-default-face))
      (setq solaire-mode nil)
    (solaire-mode--swap-bg-faces-maybe)
    (mapc #'face-remap-remove-relative solaire-mode--remap-cookies)
    (when solaire-mode
      (setq solaire-mode--remap-cookies
            (cl-loop for (map . pred) in (copy-sequence solaire-mode-remap-alist)
                     if (eval pred)
                     collect (apply #'face-remap-add-relative map)))
      ;; Update the fringe, in case it was remapped. We don't cycle
      ;; `fringe-mode' because it affects all frames, which is overkill.
      (when (and (bound-and-true-p fringe-mode)
                 (display-graphic-p)
                 solaire-mode-remap-fringe)
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
(defun turn-on-solaire-mode ()
  "Conditionally enable `solaire-mode' in the current buffer.

Does nothing if the current buffer doesn't satisfy the function in
`solaire-mode-real-buffer-fn'."
  (interactive)
  (and (not solaire-mode)
       (or (minibufferp)
           (not (funcall solaire-mode-real-buffer-fn)))
       (solaire-mode +1)))

;;;###autoload
(defun turn-off-solaire-mode ()
  "Disable `solaire-mode' in the current buffer."
  (interactive)
  (when solaire-mode
    (solaire-mode -1)))

;;;###autoload
(defun solaire-mode-in-minibuffer ()
  "Highlight the minibuffer whenever it is active."
  (with-selected-window (minibuffer-window)
    (setq-local face-remapping-alist
                (append face-remapping-alist '((default solaire-minibuffer-face))))))

;;;###autoload
(defun solaire-mode-reset (&rest _)
  "Reset all buffers with `solaire-mode' enabled.

The purpose for this is to reset faces that cannot be buffer-local such as the
fringe, which can be changed by loading a new theme or opening an Emacs client
frame with a different display (via emacsclient)."
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

(defun solaire-mode-swap-bg-faces-maybe-a (theme &rest _)
  "Swap bg colors of `default' & `solaire-default-face' when THEME is loaded."
  (and
   ;; Make sure THEME is a real theme (not a psuedo theme like `use-package' or
   ;; `solaire-swap-bg-theme').
   (get theme 'theme-feature)
   ;; Avoid `cl-lib' here, since we can't assume cl-lib will be available when
   ;; the autoloads file is read.
   (catch 'return
     ;; If THEME was successfully activated by `load-theme', it should be the
     ;; first real theme in `custom-enabled-themes'.
     (dolist (th custom-enabled-themes)
       (when (get th 'theme-feature)
         (throw 'return (eq th theme)))))
   ;; Then let 'er rip!
   (solaire-mode--swap-bg-faces-maybe)))
(advice-add #'load-theme :after #'solaire-mode-swap-bg-faces-maybe-a)

(defun solaire-mode-setup-minibuffer ()
  "Creates up minibuffer/echo area buffers and inserts whitespace in them.

Emacs will always display one of *Minibuf-X* or *Echo Area X* (where X is 0 or
1) in the minibuffer area. If these buffers don't exist OR they exist and are
empty, they will be transparent, showing the background color of `default', but
`solaire-mode' wants it to display `solaire-default-face' instead.

To do this, we create these buffers early and insert whitespace into them."
  (dolist (buf '(" *Minibuf-0*" " *Minibuf-1*"
                 " *Echo Area 0*" " *Echo Area 1*"))
    (with-current-buffer (get-buffer-create buf)
      ;; HACK Fix #41: evil initializes itself in these buffers, whether or not
      ;;      `evil-want-minibuffer' (or `evil-collection-setup-minibuffer') is
      ;;      non-nil, which is unwanted.
      (setq-local evil-buffer-regexps '((".")))
      (if solaire-global-mode
          (when (= (buffer-size) 0)
            (insert " "))
        (erase-buffer))
      ;; Don't allow users to kill these buffers, as it destroys the illusion
      (add-hook 'kill-buffer-query-functions #'ignore nil 'local)
      (solaire-mode (if solaire-global-mode +1 -1)))))
(add-hook 'solaire-global-mode-hook #'solaire-mode-setup-minibuffer)


;;
;;; Compatibility

(defun solaire-mode-fix-term-mode ()
  "Replace `term' face with `solaire-default-face' in `ansi-term-color-vector'.

Meant to fix mismatched background on text in term buffers (should be used on
`solaire-mode-hook')."
  (when (and  (derived-mode-p 'term-mode))
    (if (not solaire-mode)
        (kill-local-variable 'ansi-term-color-vector)
      (make-local-variable 'ansi-term-color-vector)
      (setf (elt ansi-term-color-vector 0) 'solaire-default-face))))
(add-hook 'solaire-mode-hook #'solaire-mode-fix-term-mode)

(when (< emacs-major-version 27)
  ;; HACK The fringe cannot have a buffer-local remapping on Emacs <= 26, so we
  ;;      jump through hoops to reset it (globally) whenever it is likely that
  ;;      the fringe will have lost its background color.
  (advice-add #'load-theme :after #'solaire-mode-reset)

  ;; Fringe can become unstyled when deleting or refocusing frames
  (add-hook 'focus-in-hook #'solaire-mode-reset)

  ;; A global fringe color means the minibuffer (with its fringes) will always
  ;; stand out, so we remove them (in which-key popups too).
  (defun solaire-mode-disable-fringes-in-minibuffer-h (&rest _)
    (set-window-fringes (minibuffer-window) 0 0 nil))
  (add-hook 'solaire-mode-hook #'solaire-mode-disable-fringes-in-minibuffer-h)
  (add-hook 'minibuffer-setup-hook #'solaire-mode-disable-fringes-in-minibuffer-h)
  (add-hook 'window-configuration-change-hook #'solaire-mode-disable-fringes-in-minibuffer-h)

  (with-eval-after-load 'which-key
    (defun solaire-mode--no-fringes-in-which-key-buffer-a (&rest _)
      (solaire-mode-disable-fringes-in-minibuffer-h)
      (set-window-fringes (get-buffer-window which-key--buffer) 0 0 nil))
    (advice-add 'which-key--show-buffer-side-window
                :after #'solaire-mode--no-fringes-in-which-key-buffer-a))

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
  (add-hook 'solaire-mode-hook #'solaire-mode-fix-fringe)


  ;; HACK On Emacs <=26, when point is on the last (or second to last) line and
  ;;      solaire-mode is remapping the hl-line face, hl-line's highlight bleeds
  ;;      into the rest of the window after eob. On Emacs 27 this no longer
  ;;      happens. This tries to fix it for 26 users, but it imposes another
  ;;      problem: the 2nd-to-last line will only be highlighted up to the
  ;;      (n-1)th character, but I think that is the lesser evil.
  (defun solaire-mode--hl-line-range-fn ()
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
  (setq hl-line-range-function #'solaire-mode--hl-line-range-fn))

(defun solaire-mode-create-image-with-correct-bg-a (image)
  (when (bound-and-true-p solaire-mode)
    (plist-put (cdr image) :background (face-background 'solaire-default-face nil t)))
  image)
(advice-add #'create-image :filter-return #'solaire-mode-create-image-with-correct-bg-a)

(with-eval-after-load 'persp-mode
  (defun solaire-mode-restore-persp-mode-buffers (&rest _)
    "Restore `solaire-mode' in buffers when `persp-mode' loads a session."
    (dolist (buf (persp-buffer-list))
      (with-current-buffer buf
        (turn-on-solaire-mode))))
  (advice-add #'persp-load-state-from-file :after #'solaire-mode-restore-persp-mode-buffers))



;;
;;; Side effects

(add-hook 'after-revert-hook #'turn-on-solaire-mode)

;; Reset solaire-mode so its remappings assimilate the remappings done by
;; `mixed-pitch-mode' and `variable-pitch-mode'.
(add-hook 'mixed-pitch-mode-hook #'solaire-mode-reset-buffer)
(add-hook 'buffer-face-mode-hook #'solaire-mode-reset-buffer)

;; Make sure solaire-mode has enough chances to swap the bg faces.
(add-hook 'solaire-global-mode-hook #'solaire-mode--swap-bg-faces-maybe)

(provide 'solaire-mode)
;;; solaire-mode.el ends here
