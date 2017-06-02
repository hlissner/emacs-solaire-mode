;;; solaire-mode.el --- make source buffers grossly incandescent
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
;; TODO
;;
;;; Code:

(require 'cl-lib)

(defgroup solaire-mode nil
  "Options for solaire-mode."
  :group 'faces)

(defface solaire-default-face '((t (:inherit default)))
  "TODO"
  :group 'solaire-mode)

(defface solaire-minibuffer-face '((t (:inherit solaire-default-face)))
  "TODO"
  :group 'solaire-mode)

(defface solaire-linum-face '((t (:inherit linum)))
  "TODO"
  :group 'solaire-mode)

(defface solaire-linum-highlight-face '((t (:inherit linum)))
  "TODO"
  :group 'solaire-mode)

(defface solaire-hl-line-face '((t (:inherit hl-line)))
  "TODO"
  :group 'solaire-mode)

(defface solaire-org-hide-face '((t (:inherit org-hide)))
  "TODO"
  :group 'solaire-mode)

(defface solaire-mode-line-face '((t (:inherit mode-line)))
  "TODO"
  :group 'solaire-mode)

(defface solaire-mode-line-inactive-face '((t (:inherit mode-line-inactive)))
  "TODO"
  :group 'solaire-mode)

(defcustom solaire-mode-real-buffer-fn #'solaire-mode--real-buffer-fn
  "The function used to determine whether the current buffer should be
brightened or not."
  :group 'solaire-mode
  :type 'function)

(defcustom solaire-mode-remap-modeline t
  "If non-nil, remap mode-line faces as well. This may conflict with certain
mode-line plugins, like powerline and telephone-line."
  :group 'solaire-mode
  :type 'boolean)

(defcustom solaire-mode-remap-faces
  '((default solaire-default-face)
    (hl-line solaire-hl-line-face)
    (linum solaire-linum-face)
    (mode-line solaire-mode-line-face)
    (mode-line-inactive solaire-mode-line-inactive-face)
    (org-hide solaire-org-hide-face))
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
(defun solaire-mode-maybe ()
  "Enable `solaire-mode' in the current buffer.

Does nothing if it doesn't represent a real, file-visiting buffer."
  (when (and (not solaire-mode)
             (funcall solaire-mode-real-buffer-fn (current-buffer)))
    (solaire-mode +1)))

;;;###autoload
(defun solaire-mode-on-minibuffer ()
  "Highlight the minibuffer whenever it is in use."
  (with-selected-window (minibuffer-window)
    (setq-local face-remapping-alist
                (append face-remapping-alist '((default doom-minibuffer-active))))))

;;;###autoload
(defun solaire-mode-reset ()
  "Resets all buffers with `solaire-mode' enabled.")

(provide 'solaire-mode)
;;; solaire-mode.el ends here
