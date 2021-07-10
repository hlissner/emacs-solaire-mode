;;; solaire-mode-test.el --- unit tests -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'solaire-mode)


;;
;;; Helpers

(defmacro deftest (name face-specs &rest body)
  "A wrapper around `ert-deftest'.

NAME is the name of the test.
FACE-SPECS are a list of face attributes to create a temporary theme with. This
theme is enalbed during BODY.

After BODY runs, any side-effects of enabling `solaire-mode' are cleaned up."
  (declare (indent 2) (doc-string 3))
  (let ((theme (make-symbol "solaire-mode-theme"))
        (docstring (if (stringp (car body)) (pop body))))
    `(ert-deftest ,name ()
       ,docstring
       (with-temp-buffer
         (deftheme ,theme)
         (apply #'custom-theme-set-faces ',theme (list ,@face-specs))
         (unwind-protect
             (progn
               (enable-theme ',theme)
               (solaire-mode--prepare-for-theme-a ',theme)
               ,@body)
           (when solaire-mode
             (solaire-mode -1))
           (when solaire-mode--theme
             ;; Disable and unregister theme
             (disable-theme solaire-mode--theme)
             (put solaire-mode--theme 'theme-feature nil)
             (setq custom-known-themes (delq solaire-mode--theme custom-known-themes)))
           (setq solaire-mode--supported-p nil
                 solaire-mode--swapped-p nil
                 solaire-mode--theme nil
                 solaire-mode--remaps nil))))))


;;
;;; Tests

(deftest activates-on-supported-theme
    ('(default ((t (:background "#000000"))))
     '(solaire-default-face ((t (:background "#222222")))))
  (solaire-mode +1)
  (should solaire-mode))

(deftest activates-on-unsupported-theme ()
  (solaire-mode +1)
  (should-not solaire-mode))

(deftest real-buffer-predicate ()
  (with-current-buffer (find-file-noselect "solaire-mode.el")
    (should (solaire-mode-real-buffer-p)))
  (with-current-buffer (dired-noselect ".")
    (should-not (solaire-mode-real-buffer-p))))

(deftest activates-in-unreal-buffers
    ('(default ((t (:background "#000000"))))
     '(solaire-default-face ((t (:background "#222222")))))
  (with-temp-buffer
    (let ((solaire-mode-real-buffer-fn (lambda () t)))
      (turn-on-solaire-mode))
    (should-not solaire-mode))
  (with-temp-buffer
    (let ((solaire-mode-real-buffer-fn (lambda () nil)))
      (turn-on-solaire-mode))
    (should solaire-mode)))

(deftest remaps-faces-buffer-locally
    ('(default                         ((t (:background "#000000"))))
     '(solaire-default-face            ((t (:background "#222222"))))
     '(mode-line                       ((t (:background "#111111"))))
     '(solaire-mode-line-face          ((t (:background "#222222"))))
     '(mode-line-inactive              ((t (:background "#333333"))))
     '(solaire-mode-line-inactive-face ((t (:background "#444444"))))
     '(header-line                     ((t (:background "blue"))))
     '(solaire-header-line-face        ((t (:background "red")))))
  (let ((solaire-mode-remap-alist
         '((default            . solaire-default-face)
           (mode-line          . solaire-mode-line-face)
           (mode-line-inactive . solaire-mode-line-inactive-face)
           (header-line))))
    (with-temp-buffer
      (solaire-mode +1)
      (dolist (remap solaire-mode-remap-alist)
        (if (cdr remap)
            (should (eq (cdr remap) (cadr (assq (car remap) face-remapping-alist))))
          (should-not (assq (car remap) face-remapping-alist))))
      ;; and cleans up after itself
      (solaire-mode -1)
      (should (null face-remapping-alist)))))

(deftest swaps-faces-globally
  ('(default                         ((t (:background "#000000"))))
   '(solaire-default-face            ((t (:background "#222222"))))
   '(mode-line                       ((t (:background "#111111"))))
   '(solaire-mode-line-face          ((t (:background "#222222"))))
   '(mode-line-inactive              ((t (:background "#333333"))))
   '(solaire-mode-line-inactive-face ((t (:background "#444444"))))
   '(header-line                     ((t (:background "blue"))))
   '(solaire-header-line-face        ((t (:background "red")))))
  (let ((solaire-mode-swap-alist
         '((default            . solaire-default-face)
           (mode-line          . solaire-mode-line-face)
           (header-line)))
        (dont-swap-alist
         '((header-line        . solaire-header-line-face)
           (mode-line-inactive . solaire-mode-line-inactive-face)))
        old-colors)
    (dolist (swap (append solaire-mode-swap-alist dont-swap-alist))
      (setf (alist-get (car swap) old-colors) (face-background (car swap))
            (alist-get (cdr swap) old-colors)
            (if (cdr swap)
                (face-background (cdr swap)))))
    (let ((solaire-mode-themes-to-face-swap ()))
      (should-not (solaire-mode-swap-faces-maybe))
      (dolist (swap solaire-mode-swap-alist)
        (when (cdr swap)
          (should-not (equal (face-background (car swap))
                             (alist-get (cdr swap) old-colors)))
          (should-not (equal (face-background (cdr swap))
                             (alist-get (car swap) old-colors))))))
    (let ((solaire-mode-themes-to-face-swap '(".")))
      (should (solaire-mode-swap-faces-maybe))
      (dolist (swap solaire-mode-swap-alist)
        (when (cdr swap)
          (should (equal (face-background (car swap))
                         (alist-get (cdr swap) old-colors)))
          (should (equal (face-background (cdr swap))
                         (alist-get (car swap) old-colors)))))
      ;; These shouldn't have changed
      (dolist (swap dont-swap-alist)
        (should (equal (face-background (car swap))
                       (alist-get (car swap) old-colors)))
        (should (equal (face-background (cdr swap))
                       (alist-get (cdr swap) old-colors))))
      ;; But don't swap the same theme more than once
      (should-not (solaire-mode-swap-faces-maybe)))))

;;; solaire-mode-test.el ends here
