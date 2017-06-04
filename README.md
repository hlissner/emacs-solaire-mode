[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![MELPA](http://melpa.org/packages/solaire-mode-badge.svg)](http://melpa.org/#/solaire-mode)

# Solaire mode

If only certain buffers could be so grossly incandescent.

<a href="http://winkla12.deviantart.com/art/Grossly-Incandescent-438305072">
  <img src="praise.jpg" width="100%" />
</a>

`soliare-mode` is inspired by editors who visually distinguish code-editing
windows from sidebars, popups, terminals, ecetera. It changes the background of
file-visiting buffers (and certain aspects of the UI) to make them easier to
distinguish from other, not-so-important buffers.

Praise the sun.

**Note:**
+ Uses `face-remapping-alist`, which other plugins may overwrite.
+ Tested mainly on Emacs 25.1+
+ This was once a part of [doom-themes] as `doom-buffer-mode`
+ Try jumping.

## Install

`M-x package-install RET solaire-mode`

```emacs-lisp
(require 'solaire-mode)

;; brighten buffers that represent real files, and ensure solaire-mode persists
;; across major-mode changes.
(add-hook 'find-file-hook #'turn-on-solaire-mode)
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

  You can change what `solaire-mode` considers a "real" file by changing
  `solaire-mode-real-buffer-fn` (a function that determines if a buffer should
  be brightened or not).

## Known conflicts
+ By default, `solaire-mode` remaps the mode-line faces. This interferes with
  certain mode-line packages like telephone-line or powerline. You can undo this
  with:

  ```emacs-lisp
  (setq solaire-mode-remap-modeline nil)
  ```
+ Don't trust Patches!


[doom-themes]: https://github.com/hlissner/emacs-doom-themes
