;;; solaire-mode.el --- make certain buffers grossly incandescent -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2020 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: Jun 03, 2017
;; Modified: August 11, 2020
;; Version: 1.1.4
;; Keywords: dim bright window buffer faces
;; Homepage: https://github.com/hlissner/emacs-solaire-mode
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
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
;;
;; Brighten buffers that represent real files:
;;
;;   (add-hook 'change-major-mode-hook #'turn-on-solaire-mode)
;;
;; If you use auto-revert-mode:
;;
;;   (add-hook 'after-revert-hook #'turn-on-solaire-mode)
;;
;; And to unconditionally brighten certain buffers:
;;
;;   (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
;;
;; You can do similar with the minibuffer when it is active:
;;
;;   (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
;;
;;; Code:

(require 'cl-lib)

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
  `((t (:inherit (,(if (boundp 'display-line-numbers) 'line-number 'linum) solaire-default-face))))
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

(defcustom solaire-mode-themes-to-face-swap '("^doom-")
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
(defvar solaire-mode--current-theme nil)

(defun solaire-mode--real-buffer-p ()
  "Return t if the BUF is a file-visiting buffer."
  (buffer-file-name (buffer-base-buffer)))

(defun solaire-mode--swap-bg-faces-p ()
  "Return t if the current buffer is in `solaire-mode-theme-whitelist'."
  (and solaire-mode--current-theme
       solaire-mode-auto-swap-bg
       (not solaire-mode--bg-swapped)
       (cl-loop for rule in solaire-mode-themes-to-face-swap
                if (cond ((functionp rule) (funcall rule solaire-mode--current-theme))
                         ((stringp rule) (string-match-p rule (symbol-name solaire-mode--current-theme)))
                         ((symbolp rule) (eq solaire-mode--current-theme rule)))
                return t)))

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
      (let ((default-bg (face-background 'default))
            (theme solaire-mode--current-theme))
        (with-eval-after-load 'ansi-color
          (when (eq theme solaire-mode--current-theme)
            (when (stringp default-bg)
              (setf (aref ansi-color-names-vector 0) default-bg))))
        (enable-theme swap-theme)
        (setq solaire-mode--bg-swapped t)))))


(defvar-local solaire-mode--remap-cookies nil)
;;;###autoload
(define-minor-mode solaire-mode
  "Make source buffers grossly incandescent by remapping common faces (see
`solaire-mode-remap-alist') to their solaire-mode variants."
  :lighter "" ; should be obvious it's on
  :init-value nil
  (solaire-mode--swap-bg-faces-maybe)
  (unless solaire-mode-remap-fringe
    (solaire-mode-reset-fringe-face solaire-mode))
  (mapc #'face-remap-remove-relative solaire-mode--remap-cookies)
  (when solaire-mode
    (and (setq solaire-mode--remap-cookies
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

(defun solaire-mode-reset-fringe-face (arg)
  "Toggle the `fringe's new background.

This is only necessary for Emacs 26 and below. Emacs 27 and above support
remapping the fringe buffer-locally.

If ARG is non-nil, match `solaire-fringe-face's background, otherwise
`default's."
  (set-face-background
   'fringe
   (if (or (null arg) (eq arg -1))
       (unless (cl-loop for buf in (buffer-list)
                        if (buffer-local-value 'solaire-mode buf)
                        return t)
         (face-background 'default))
     (face-background 'solaire-fringe-face nil t))))

;;;###autoload
(define-globalized-minor-mode solaire-global-mode solaire-mode turn-on-solaire-mode)

;;;###autoload
(defun turn-on-solaire-mode ()
  "Conditionally enable `solaire-mode' in the current buffer.

Does nothing if the current buffer doesn't satisfy the function in
`solaire-mode-real-buffer-fn'."
  (interactive)
  (when (and (not solaire-mode)
             (not (minibufferp))
             (funcall solaire-mode-real-buffer-fn))
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
      (when solaire-mode
        (solaire-mode -1)
        (solaire-mode +1)))))

;;;###autoload
(advice-add #'load-theme :before
            (lambda (theme &optional _ no-enable)
              (unless no-enable
                (setq solaire-mode--current-theme theme))))

;;;###autoload
(advice-add #'load-theme :after
            (lambda (theme &rest _)
              ;; `load-theme' doesn't always enable THEME
              (when (memq theme custom-enabled-themes)
                (setq solaire-mode--bg-swapped nil)
                ;; If solaire was already loaded and you've changed the theme
                ;; post-startup, solaire-mode wouldn't reset its swapped bg
                ;; faces. This prevents that:
                (when (featurep 'solaire-mode)
                  (solaire-mode--swap-bg-faces-maybe)))))

(add-hook 'solaire-global-mode-hook #'solaire-mode--swap-bg-faces-maybe)


;;
;;; Compatibility

(defun solaire-mode--fix-org-latex-bg (orig-fn attr)
  "Advice for background color mismatch in org latex previews.
Only works if :background is set to `default' in `org-format-latex-options'."
  (turn-on-solaire-mode)
  (if solaire-mode
      (cl-letf*
          ((face-attribute (symbol-function #'face-attribute))
           ((symbol-function #'face-attribute)
            (lambda (_face attr &optional frame _inherit)
              (car (delq 'unspecified
                         (mapcar (lambda (face)
                                   (funcall face-attribute face attr frame nil))
                                 '(solaire-default-face default)))))))
        (funcall orig-fn attr))
    (funcall orig-fn attr)))
(advice-add #'org-latex-color  :around #'solaire-mode--fix-org-latex-bg)
(advice-add #'org-dvipng-color :around #'solaire-mode--fix-org-latex-bg)


;;; Deprecated

;;;###autoload
(defun solaire-mode-swap-bg ()
  "Does nothing. Set `solaire-mode-auto-swap-bg' instead."
  (declare (obsolete solaire-mode-auto-swap-bg "1.1.4"))
  (solaire-mode--swap-bg-faces-maybe))


;;; Support for other packages

;;;###autoload
(defun solaire-mode-restore-persp-mode-buffers (&rest _)
  "Restore `solaire-mode' in buffers when `persp-mode' loads a session."
  (dolist (buf (persp-buffer-list))
    (with-current-buffer buf
      (turn-on-solaire-mode))))

(provide 'solaire-mode)
;;; solaire-mode.el ends here
