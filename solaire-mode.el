;;; solaire-mode.el --- make certain buffers grossly incandescent
;;
;; Copyright (C) 2017 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: Jun 03, 2017
;; Modified: Jun 04, 2017
;; Version: 1.0.0
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
;;   (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
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
  :group 'faces) ; FIXME :group

(defface solaire-default-face '((t (:inherit default)))
  "Alternative version of the `default' face."
  :group 'solaire-mode)

(defface solaire-minibuffer-face '((t (:inherit solaire-default-face)))
  "Alternative face for the minibuffer. See `solaire-mode-in-minibuffer'."
  :group 'solaire-mode)

(defface solaire-linum-face '((t (:inherit linum)))
  "Alternative face for `linum-mode' (and `nlinum-mode')."
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

;;
(defcustom solaire-mode-real-buffer-fn #'solaire-mode--real-buffer-fn
  "The function that determines buffer eligability for `solaire-mode'.

Should accept one argument: the buffer."
  :group 'solaire-mode
  :type 'function)

(defcustom solaire-mode-remap-modeline t
  "If non-nil, remap mode-line faces as well.

Solaire-mode can conflict with certain mode-line plugins, like powerline and
telephone-line, so it's best to simply turn this off for those plugins."
  :group 'solaire-mode
  :type 'boolean)

(defcustom solaire-mode-remap-faces
  '((default solaire-default-face)
    (hl-line solaire-hl-line-face)
    (linum solaire-linum-face)
    (org-hide solaire-org-hide-face)
    (mode-line solaire-mode-line-face)
    (mode-line-inactive solaire-mode-line-inactive-face))
  "An alist of faces to remap when enabling `solaire-mode'."
  :group 'solaire-mode
  :type '(list face))

(defun solaire-mode--real-buffer-fn (buf)
  "Return t if the current buffer BUF represents a real file."
  buffer-file-name)

;;;###autoload
(define-minor-mode solaire-mode
  "Make source buffers grossly incandescent by remapping common faces (see
`solaire-mode-remap-faces') to their solaire-mode variants."
  :lighter "" ; should be obvious it's on
  :init-value nil
  ;; Don't reset remapped faces on `kill-all-local-variables'
  (make-variable-buffer-local 'face-remapping-alist)
  (put 'face-remapping-alist 'permanent-local solaire-mode)
  (if solaire-mode
      (progn
        (set-face-background 'fringe (face-background 'solaire-default-face))
        (setq face-remapping-alist (append solaire-mode-remap-faces face-remapping-alist))
        (unless solaire-mode-remap-modeline
          (dolist (fc '(mode-line mode-line-inactive) solaire-mode-remap-faces)
            (setq face-remapping-alist
                  (assq-delete-all fc solaire-mode-remap-faces)))))
    (dolist (remap solaire-mode-remap-faces)
      (setq face-remapping-alist (delete remap face-remapping-alist)))
    (unless (cl-loop for buf in (buffer-list)
                     when (buffer-local-value 'solaire-mode buf)
                     return t)
      (set-face-background 'fringe (face-background 'default)))))

;;;###autoload
(defun turn-on-solaire-mode ()
  "Enable `solaire-mode' in the current buffer.

Does nothing if it doesn't represent a real, file-visiting buffer (see
`solaire-mode-real-buffer-fn')."
  (when (and (not solaire-mode)
             (funcall solaire-mode-real-buffer-fn (current-buffer)))
    (solaire-mode +1)))

;;;###autoload
(defun turn-off-solaire-mode ()
  "Disable `solaire-mode' in the current buffer."
  (when solaire-mode
    (solaire-mode -1)))

;;;###autoload
(defun solaire-mode-in-minibuffer ()
  "Highlight the minibuffer whenever it is active."
  (with-selected-window (minibuffer-window)
    (setq-local face-remapping-alist
                (append face-remapping-alist '((default solaire-minibuffer-face))))))

;;;###autoload
(defun solaire-mode-reset ()
  "Reset all buffers with `solaire-mode' enabled."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when solaire-mode
        (solaire-mode -1)
        (solaire-mode +1)))))

;;;###autoload
(defun solaire-mode-restore-persp-mode-buffers (&rest _)
  "Restore `solaire-mode' in buffers when `persp-mode' loads a session."
  (dolist (buf (persp-buffer-list))
    (with-current-buffer buf
      (turn-on-solaire-mode))))

(defun solaire-mode--face-remap-add-relative (orig-fn &rest args)
  "Minimize interference from other themes, functions and/or packages trying to
remap their own faces (like `text-scale-set')."
  (when solaire-mode
    (let ((remap (assq (nth 0 args) face-remapping-alist)))
      (when remap (setf (nth 0 args) (cadr remap)))))
  (apply orig-fn args))
(advice-add 'face-remap-add-relative :around #'solaire-mode--face-remap-add-relative)

(provide 'solaire-mode)
;;; solaire-mode.el ends here
