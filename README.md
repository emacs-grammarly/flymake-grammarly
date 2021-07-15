[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/flymake-grammarly-badge.svg)](https://melpa.org/#/flymake-grammarly)
[![MELPA Stable](https://stable.melpa.org/packages/flymake-grammarly-badge.svg)](https://stable.melpa.org/#/flymake-grammarly)
[![CI](https://github.com/emacs-grammarly/flymake-grammarly/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-grammarly/flymake-grammarly/actions/workflows/test.yml)

<img align="right" src="./etc/logo.png" with="138" height="46">

# flymake-grammarly
> Flymake support for Grammarly.

<p align="center">
  <img src="./etc/screenshot.png" width="629" height="225"/>
<p>

## Usage

To enable this package, simply add loading to your config like the code below.

```el
(require 'flymake-grammarly)
```

You can generally call `flymake-grammarly-load` to any `mode-hook` for
grammarly to start working.

```el
(add-hook 'text-mode-hook 'flymake-grammarly-load)
(add-hook 'latex-mode-hook 'flymake-grammarly-load)
(add-hook 'org-mode-hook 'flymake-grammarly-load)
(add-hook 'markdown-mode-hook 'flymake-grammarly-load)
```

otherwise you can call `flymake-grammarly-maybe-load` like the snippt below.

```el
(add-hook 'find-file-hook 'flymake-grammarly-maybe-load)
```

If you encounter the performance issue, try raise `flymake-grammarly-check-time` higher.
The request will be send by this time everytime the buffer has changed.

```el
(setq flymake-grammarly-check-time 0.8)
```

## Todo List

- [ ] Strip only text data, if other data like `# header` or `> quote` will return nothing.
- [ ] Usable but not fast enough, really depends on Grammarly's analyzer.

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
