![Release tag](https://img.shields.io/github/tag/hlissner/emacs-solaire-mode.svg?label=release&style=flat-square)
[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![MELPA](http://melpa.org/packages/solaire-mode-badge.svg)](http://melpa.org/#/solaire-mode)

# Solaire mode

If only certain buffers could be so grossly incandescent.

<a href="https://winkla12.deviantart.com/art/Grossly-Incandescent-438305072">
  <img src="/../screenshots/praise.jpg" width="100%" />
</a>

`solaire-mode` is an aesthetic plugin that helps visually distinguish
file-visiting windows from other types of windows (like popups or sidebars) by
giving them a slightly different -- often brighter -- background.

Praise the sun.

**Note:**
+ Uses `face-remapping-alist`, which other plugins may overwrite.
+ Tested mainly on Emacs 25.1+
+ This was once a part of [doom-themes] as `doom-buffer-mode`
+ Try jumping.

## Screenshot

![solaire-mode at work](/../screenshots/screenshot.png)

## Install

Solaire-mode is available on MELPA: `M-x package-install RET solaire-mode`

### Doom Emacs

Doom installs this package as part of the `:ui doom` module. No additional
configuration is needed.


## Configuration

`solaire-mode` (or `solaire-global-mode`) must be activated before your theme is
loaded:

```emacs-lisp
(solaire-global-mode +1)

(load-theme 'my-theme t)
```

Here are some example `use-package` configs for `solaire-mode`:

```emacs-lisp
;; A simple config:
(use-package solaire-mode
  :hook (after-init . solaire-global-mode))


;; A more complex, more lazy-loaded config
(use-package solaire-mode
  ;; Ensure solaire-mode is running in all solaire-mode buffers
  :hook (change-major-mode . turn-on-solaire-mode)
  ;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
  ;; itself off every time Emacs reverts the file
  :hook (after-revert . turn-on-solaire-mode)
  ;; To enable solaire-mode unconditionally for certain modes:
  :hook (ediff-prepare-buffer . solaire-mode)
  ;; Highlight the minibuffer when it is activated:
  :hook (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  ;; The bright and dark background colors are automatically swapped the first 
  ;; time solaire-mode is activated. Namely, the backgrounds of the `default` and
  ;; `solaire-default-face` faces are swapped. This is done because the colors 
  ;; are usually the wrong way around. If you don't want this, you can disable it:
  (setq solaire-mode-auto-swap-bg nil)

  (solaire-global-mode +1))
```


## Configuration
+ By default, `solaire-mode`'s effects will be invisible. Its faces must be
  defined:

  + `solaire-default-face`
  + `solaire-minibuffer-face`
  + `solaire-line-number-face`
  + `solaire-hl-line-face`
  + `solaire-org-hide-face`
  + `solaire-mode-line-face`
  + `solaire-mode-line-inactive-face`

+ What faces get remapped is controlled by `solaire-mode-remap-faces` and
  `solaire-mode-remap-modeline`.

  By default, these faces are affected:

  + `default`
  + `hl-line`
  + `linum`
  + `org-hide`
  + `mode-line`
  + `mode-line-inactive`

+ The function in `solaire-mode-real-buffer-fn` determines if a buffer should be
  brightened or not.

+ `solaire-mode-remap-fringe` controls whether the fringe's background is
  changed (and maintained) when solaire-mode is active. Setting it to `nil` will
  disable this behavior. To change what background it is changed to, modify the
  `solaire-fringe-face` face's `:background`.

## Themes that support solaire-mode out of the box
  The only (known) themes to support solaire-mode are:

  + [doom-themes]
  + [spacemacs-theme]
  + [wilmersdorf-theme]

## Jolly cooperation with other plugins
+ Latex previews in org-mode may have a mismatched background color. You can fix
  this by setting the `:background` property in `org-format-latex-options` to
  `'default`:
  
  ```elisp
  (with-eval-after-load 'org
    (plist-put org-format-latex-options :background 'default))
  ```
+ By default, `solaire-mode` remaps the mode-line faces. This interferes with
  certain mode-line packages like telephone-line or powerline. You can undo this
  with:

  ```emacs-lisp
  (setq solaire-mode-remap-modeline nil)
  ```
+ When `persp-mode` loads a perspective from file, it doesn't restore
  solaire-mode. The function `solaire-mode-restore-persp-mode-buffers` is
  available for this:

  ```emacs-lisp
  (advice-add #'persp-load-state-from-file :after #'solaire-mode-restore-persp-mode-buffers)
  ```
+ Don't trust Patches!


[doom-themes]: https://github.com/hlissner/emacs-doom-themes
[spacemacs-theme]: https://github.com/nashamri/spacemacs-theme
[wilmersdorf-theme]: https://github.com/ianpan870102/wilmersdorf-emacs-theme
