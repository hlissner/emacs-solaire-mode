[![Made with Doom Emacs](https://img.shields.io/badge/Made_with-Doom_Emacs-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)](https://github.com/hlissner/doom-emacs)
![Release tag](https://img.shields.io/github/tag/hlissner/emacs-solaire-mode.svg?label=release&style=flat-square)
[![MELPA](http://melpa.org/packages/solaire-mode-badge.svg)](http://melpa.org/#/solaire-mode)
![Build status](https://img.shields.io/github/workflow/status/hlissner/emacs-solaire-mode/CI/master?style=flat-square)

# Solaire mode
If only certain buffers could be so grossly incandescent.

<a href="https://winkla12.deviantart.com/art/Grossly-Incandescent-438305072">
  <img src="/../screenshots/praise.jpg" width="100%" />
</a>

`solaire-mode` is an aesthetic plugin designed to visually distinguish "real"
buffers (i.e. file-visiting code buffers where you do most of your work) from
"unreal" buffers (like popups, sidebars, log buffers, terminals, etc) by giving
the latter a slightly different -- often darker -- background:

![solaire-mode at work](/../screenshots/screenshot.png)

This plugin accomplishes this by doing two things when activated:

+ `solaire-mode` buffer-locally remaps all the faces in
  `solaire-mode-remap-alist`. e.g. `default` <-> `solaire-default-face`, and
  `mode-line` <-> `solaire-mode-line-face`
  
  The latter faces are the (presumably) "adjusted" faces.

+ `solaire-global-mode` will globally swap faces in `solaire-mode-swap-alist` IF
  your active theme is in `solaire-mode-themes-to-face-swap`. Use this if you
  think a theme has its designated solaire-*-face colors the wrong way around.
  
Praise the sun.

**Note:**
+ Uses `face-remapping-alist`, which other plugins may overwrite.
+ Tested mainly on Emacs 25.1+
+ Works in GUI and terminal Emacs (with themes that support it).
+ [Try jumping.](https://www.youtube.com/watch?v=-ZGlaAxB7nI)


## Install
Solaire-mode is available on MELPA: `M-x package-install RET solaire-mode`

### Doom Emacs
Doom installs this package as part of the `:ui doom` module. No additional
configuration is needed.


## Configuration
Simply activate `solaire-global-mode`:

```emacs-lisp
(solaire-global-mode +1)
```

### Settings
+ `solaire-mode-real-buffer-fn` (default: `solaire-mode-real-buffer-p`): The
  predicate function used to determine if a buffer is "real" or not. It takes no
  arguments and must return truthy for buffers where `solaire-mode` should *not*
  be activated in.
+ `solaire-mode-remap-alist`: An alist mapping original faces to replacement
  faces, which will be buffer-locally remapped in any buffer `solaire-mode` is
  enabled in.
+ `solaire-mode-swap-alist`: An alist mapping original faces to replacement
  faces, which will be swapped globally if current theme is in
  `solaire-mode-themes-to-face-swap`.
+ `solaire-mode-themes-to-face-swap` (default: `()`): A list of themes to swap
  faces in `solaire-mode-swap-alist` for. Can be symbols or regexps.

### Jolly cooperation with other plugins
+ By default, `solaire-mode` remaps the mode-line faces. This interferes with
  certain mode-line packages like telephone-line or powerline, but works fine
  for doom-modeline. To disable this behavior use:

  ```elisp
  (dolist (face '(mode-line mode-line-inactive))
    (setf (alist-get face solaire-mode-remap-modeline) nil))
  ```
+ It is up to themes to decide whether unreal buffers (i.e. non-file-visiting)
  are lighter or darker than real buffers. If you don't like their arrangement,
  add that theme to `solaire-mode-themes-to-face-swap`:

  ```elisp
  (add-to-list 'solaire-mode-themes-to-face-swap 'doom-vibrant)
  ```
  
  Then solaire-mode will swap all the faces in `solaire-mode-swap-alist` (for
  example, swapping the `default` face with `solaire-default-face`).
  
  You can add a regexp to `solaire-mode-themes-to-face-swap` to affect a family
  of themes, e.g.

  ```elisp
  (add-to-list 'solaire-mode-themes-to-face-swap "^doom-")
  ```

  
## Theme support for solaire-mode
`solaire-mode` will disable itself if the active theme doesn't support it, but
to ensure support a theme must at least change `solaire-default-face`'s
`:background`. My recommendation is that it is be *slightly* darker or lighter
than `default`'s background.

For example `doom-one` (seen in the screenshot above) prefers
`solaire-default-face` be darker than `default`:

![doom-one screenshot](https://raw.githubusercontent.com/hlissner/emacs-doom-themes/screenshots/doom-one.png)

Others, like doom-vibrant prefer the opposite:

![doom-vibrant screenshot](https://raw.githubusercontent.com/hlissner/emacs-doom-themes/screenshots/doom-vibrant.png)

For full support, themes can customize the following faces:

| Face                            | Remapped face        |
|---------------------------------|----------------------|
| solaire-default-face            | default              |
| solaire-fringe-face             | fringe               |
| solaire-line-number-face        | line-number          |
| solaire-hl-line-face            | hl-line              |
| solaire-org-hide-face           | org-hide, org-indent |
| solaire-mode-line-face          | mode-line            |
| solaire-mode-line-inactive-face | mode-line-inactive   |
| solaire-header-line-face        | header-line          |

## Themes that support solaire-mode out of the box
The only (known) themes to support solaire-mode are:

+ [doom-themes]
+ [nano-theme]
+ [modus-themes]
+ [parchment]
+ [spacemacs-theme]
+ [vscode-dark-plus-theme]
+ [wilmersdorf-theme]

If you know of more, feel free to PR them.


[doom-themes]: https://github.com/hlissner/emacs-doom-themes
[modus-themes]: https://gitlab.com/protesilaos/modus-themes
[nano-theme]: https://github.com/404cn/nano-theme.el
[parchment]: https://github.com/ajgrf/parchment
[spacemacs-theme]: https://github.com/nashamri/spacemacs-theme
[vscode-dark-plus-theme]: https://github.com/ianpan870102/vscode-dark-plus-emacs-theme
[wilmersdorf-theme]: https://github.com/ianpan870102/wilmersdorf-emacs-theme
