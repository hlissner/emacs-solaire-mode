[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![MELPA](http://melpa.org/packages/solaire-mode-badge.svg)](http://melpa.org/#/solaire-mode)
![solaire-mode](https://img.shields.io/badge/solaire--mode-v1.0.1-blue.svg)

# Solaire mode

If only certain buffers could be so grossly incandescent.

<a href="http://winkla12.deviantart.com/art/Grossly-Incandescent-438305072">
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

`M-x package-install RET solaire-mode`

```emacs-lisp
(require 'solaire-mode)

;; brighten buffers (that represent real files)
(add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)

;; ...if you use auto-revert-mode:
(add-hook 'after-revert-hook #'turn-on-solaire-mode)

;; You can do similar with the minibuffer when it is activated:
(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

;; To enable solaire-mode unconditionally for certain modes:
(add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
```

## Configuration
+ By default, `solaire-mode`'s effects will be invisible. Its faces must be
  defined:

  + `solaire-default-face`
  + `solaire-minibuffer-face`
  + `solaire-linum-face`
  + `solaire-linum-highlight-face`
  + `solaire-hl-line-face`
  + `solaire-org-hide-face`
  + `solaire-mode-line-face`
  + `solaire-mode-line-inactive-face`

  The only theme(s) that supports this, that I know of, is [doom-themes].

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

## Jolly cooperation with other plugins
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
