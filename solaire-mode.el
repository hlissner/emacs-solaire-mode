;;; solaire-mode.el --- make certain buffers grossly incandescent
;;
;; Copyright (C) 2017 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: Jun 03, 2017
;; Modified: Jun 03, 2017
;; Version: 1.0.0
;; Keywords: dim bright window buffer
;; Homepage: https://github.com/hlissner/emacs-solaire-mode
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; `solaire-mode` changes the background of file-visiting buffers (and certain
;; aspects of the UI) to make them easier to distinguish from transient,
;; temporary or special buffers.
;;
;;; Installation
;;
;; M-x package-install RET solaire-mode
;;
;;   (require 'solaire-mode)
;;
;;   ;; brighten buffers that represent real files:
;;   (add-hook 'find-file-hook #'turn-on-solaire-mode)
;;
;;   ;; ...if you use auto-revert-mode:
;;   (add-hook 'after-revert-hook #'turn-on-solaire-mode)
;;
;;   ;; to unconditionally brighten certain buffers:
;;   (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
;;
;;; Code:

(require 'cl-lib)

(defgroup solaire-mode nil
  "Options for solaire-mode."
  :group 'faces)

(defface solaire-default-face '((t (:inherit default)))
  "Alternative version of the `default' face."
  :group 'solaire-mode)

(defface solaire-minibuffer-face '((t (:inherit solaire-default-face)))
  "Alternative face for the minibuffer. See `solaire-mode-on-minibuffer'."
  :group 'solaire-mode)

(defface solaire-linum-face '((t (:inherit linum)))
  "Alternative face for `linum-mode' (and `nlinum-mode')."
  :group 'solaire-mode)

(defface solaire-linum-highlight-face '((t (:inherit linum)))
  "Alternative face for highlighted line numbers."
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
  "The function used to determine whether the current buffer should be affected
or not. Should accept one argument: the buffer.

See `solaire-mode--real-buffer-fn' (the default function)."
  :group 'solaire-mode
  :type 'function)

(defcustom solaire-mode-remap-modeline t
  "If non-nil, remap mode-line faces as well. Solaire-mode can conflict with
certain mode-line plugins, like powerline and telephone-line, so it's best to
simply turn this off for those plugins."
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
  "Returns t if the current buffer represents a real file."
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
        (set-face-background 'fringe (face-background 'bright-default-face))
        (setq face-remapping-alist (append solaire-mode-remapped-faces face-remapping-alist))
        (unless solaire-mode-remap-modeline
          (dolist (fc '(mode-line mode-line-inactive) solaire-mode-remap-faces)
            (setq face-remapping-alist
                  (assq-delete-all fc solaire-mode-remap-faces)))))
    (dolist (remap solaire-mode-remapped-faces)
      (setq face-remapping-alist (delete remap face-remapping-alist)))
    (unless (cl-some (lambda (buf) (buffer-local-value 'solaire-mode buf))
                     (buffer-list))
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
(defun solaire-mode-on-minibuffer ()
  "Highlight the minibuffer whenever it is active."
  (with-selected-window (minibuffer-window)
    (setq-local face-remapping-alist
                (append face-remapping-alist '((default doom-minibuffer-active))))))

;;;###autoload
(defun solaire-mode-reset ()
  "Resets all buffers with `solaire-mode' enabled."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when solaire-mode
        (solaire-mode -1)
        (solaire-mode +1)))))

;; Fixes an issue with persp-mode where solaire-mode is forgotten when loading a
;; perspective from file.
(eval-after-load "persp-mode"
  (lambda ()
    (defun solaire-mode--reload-buffers (&rest _)
      "Restore `solaire-mode' in buffers when `persp-mode' loads a session."
      (dolist (buf (persp-buffer-list))
        (with-current-buffer buf
          (turn-on-solaire-mode))))
    (advice-add #'persp-load-state-from-file :after #'solaire-mode--reload-buffers)))

(provide 'solaire-mode)
;;; solaire-mode.el ends here
