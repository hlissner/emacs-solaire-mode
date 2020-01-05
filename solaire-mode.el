;;; solaire-mode.el --- make certain buffers grossly incandescent
;;
;; Copyright (C) 2017-2020 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: Jun 03, 2017
;; Modified: April 24, 2019
;; Version: 1.0.9
;; Keywords: dim bright window buffer faces
;; Homepage: https://github.com/hlissner/emacs-solaire-mode
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; `soliare-mode' is inspired by editors who visually distinguish code-editing
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
(defcustom solaire-mode-real-buffer-fn #'solaire-mode--real-buffer-p
  "The function that determines buffer eligability for `solaire-mode'.

Should accept one argument: the buffer."
  :group 'solaire-mode
  :type 'function)

(defcustom solaire-mode-auto-swap-bg nil
  "If non-nil, swap the backgrounds of faces and their solaire counterparts.

How solaire-mode works is it remaps many faces to solaire-mode counterparts. In
order to make file-visiting buffers \"brighter\", it remaps `default' with
`solaire-default-face', and has to assume that the latter has the brighter
:background. Or more specifically, it is assumed that `default' will be the
\"darker\" face. Since this isn't always the case, it is sometimes necessary to
call `solaire-mode-swap-bg' to swap them.

Setting this to non-nil will do so automatically when a theme is loaded.

See `solaire-mode-swap-bg' for specifics."
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

(defcustom solaire-mode-remap-fringe t
  "If non-nil, change the background of the fringe."
  :group 'solaire-mode
  :type 'boolean)

(defcustom solaire-mode-remap-alist
  '(((default solaire-default-face)                       . t)
    ((hl-line solaire-hl-line-face)                       . t)
    ((org-hide solaire-org-hide-face)                     . t)
    ((org-indent solaire-org-hide-face)                   . t)
    ((linum solaire-line-number-face)                     . solaire-mode-remap-line-numbers)
    ((line-number solaire-line-number-face)               . solaire-mode-remap-line-numbers)
    ((header-line solaire-mode-line-face)                 . solaire-mode-remap-headerline)
    ((mode-line solaire-mode-line-face)                   . solaire-mode-remap-modeline)
    ((mode-line-inactive solaire-mode-line-inactive-face) . solaire-mode-remap-modeline)
    ((highlight-indentation-face solaire-hl-line-face)    . (featurep 'highlight-indentation)))
  "An alist of faces to remap when enabling `solaire-mode'."
  :group 'solaire-mode
  :type '(list face))

(defvar solaire-mode--pending-bg-swap nil)

(defun solaire-mode--real-buffer-p ()
  "Return t if the BUF is a file-visiting buffer."
  buffer-file-name)

(defvar-local solaire-mode--remap-cookies nil)
;;;###autoload
(define-minor-mode solaire-mode
  "Make source buffers grossly incandescent by remapping common faces (see
`solaire-mode-remap-alist') to their solaire-mode variants."
  :lighter "" ; should be obvious it's on
  :init-value nil
  (when solaire-mode--pending-bg-swap
    (solaire-mode-swap-bg))
  (mapc #'face-remap-remove-relative solaire-mode--remap-cookies)
  (if (not solaire-mode)
      (when (and solaire-mode-remap-fringe
                 (not (cl-loop for buf in (buffer-list)
                               when (buffer-local-value 'solaire-mode buf)
                               return t)))
        (set-face-background 'fringe (face-background 'default)))
    (when solaire-mode-remap-fringe
      (set-face-background 'fringe (face-background 'solaire-fringe-face nil t)))
    (setq solaire-mode--remap-cookies
          (cl-loop for (map . pred) in (copy-sequence solaire-mode-remap-alist)
                   if (eval pred)
                   collect (apply #'face-remap-add-relative map)))))

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

(defun solaire-mode--swap (face1 face2 &optional prop)
  (let* ((prop (or prop :background))
         (color (face-attribute face1 prop)))
    (set-face-attribute face1 nil prop (face-attribute face2 prop))
    (set-face-attribute face2 nil prop color)))

;;;###autoload
(defun solaire-mode-swap-bg ()
  "Swap the backgrounds of the following faces:

+ `default' <-> `solaire-default-face'
+ `hl-line' <-> `solaire-hl-line-face'
+ `org-hide' <-> `solaire-org-hide-face'

This is necessary for themes in the doom-themes package."
  (when (or (null solaire-mode-auto-swap-bg)
            solaire-mode--pending-bg-swap)
    (solaire-mode--swap 'default 'solaire-default-face)
    (with-eval-after-load 'hl-line
      (solaire-mode--swap 'hl-line 'solaire-hl-line-face))
    (with-eval-after-load 'org
      (solaire-mode--swap 'org-hide 'solaire-org-hide-face :foreground))
    (with-eval-after-load 'ansi-color
      (let ((color (face-background 'default)))
        (when (stringp color)
          (setf (aref ansi-color-names-vector 0) color))))
    (setq solaire-mode--pending-bg-swap nil)))

;;;###autoload
(defun solaire-mode-fix-latex-preview-background ()
  "Fixes `org-mode' to display latex previews with the correct background."
  ;; Fix #24
  (when (eq major-mode 'org-mode)
    (setq-default
     org-format-latex-options
     (plist-put org-format-latex-options
                :background
                (face-background (or (cadr (assq 'default face-remapping-alist))
                                     'default)
                                 nil t)))))
(add-hook 'solaire-mode-hook #'solaire-mode-fix-latex-preview-background)

;;;###autoload
(defun solaire-mode-restore-persp-mode-buffers (&rest _)
  "Restore `solaire-mode' in buffers when `persp-mode' loads a session."
  (dolist (buf (persp-buffer-list))
    (with-current-buffer buf
      (turn-on-solaire-mode))))

;;;###autoload
(advice-add #'load-theme :after
            (lambda (&rest _)
              (setq solaire-mode--pending-bg-swap
                    (bound-and-true-p solaire-mode-auto-swap-bg))))

(provide 'solaire-mode)
;;; solaire-mode.el ends here
