[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![MELPA](http://melpa.org/packages/solaire-mode-badge.svg)](http://melpa.org/#/solaire-mode)

# Solaire mode

If only certain buffers could be so grossly incandescent.

[![Praise the sun](praise.jpg)](http://winkla12.deviantart.com/art/Grossly-Incandescent-438305072)

By default, `solaire-mode` changes the background of file-visiting buffers (and
certain aspects of the UI), making them easier to distinguish from transient,
temporary or special buffers.

**Note:**
+ Uses `face-remapping-alist`, which other plugins may overwrite.
+ Tested mainly on Emacs 25.1+
+ This was once a part of [doom-themes] as `doom-buffer-mode`

## Install

`M-x package-install RET solaire-mode`

```emacs-lisp
(require 'solaire-mode)

;; brighten buffers that represent real files:
(add-hook 'find-file-hook #'solaire-mode-maybe)

;; ...if you use auto-revert-mode:
(add-hook 'after-revert-hook #'solaire-mode-maybe)

;; to unconditionally brighten certain buffers:
(add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
```

## Configuration

What faces get remapped is controlled by `solaire-mode-remap-faces` and
`solaire-mode-remap-modeline`.

By default, these faces are affected:

+ `default`
+ `hl-line`
+ `linum`
+ `org-hide`
+ `mode-line`
+ `mode-line-inactive`

You can change what `solaire-mode` considers a "real" file by changing
`solaire-mode-real-buffer-fn` (a function that determines if a buffer should be
brightened or not).

## Known conflicts
+ By default, `solaire-mode` remaps the mode-line faces. This interferes with
  certain mode-line packages like telephone-line or powerline. You can undo this
  with:

  ```emacs-lisp
  (setq solaire-mode-remap-modeline nil)
  ```


[doom-themes]: https://github.com/hlissner/emacs-doom-theme
