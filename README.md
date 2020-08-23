[![Build Status](https://travis-ci.com/jcs-elpa/flymake-grammarly.svg?branch=master)](https://travis-ci.com/jcs-elpa/flymake-grammarly)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# flymake-grammarly
> Grammarly support for Flymake.

<p align="center">
  <img src="./etc/screenshot.png" width="309" height="360"/>
<p>

## Usage

To enable this package, simply add loading to your config like the code below.

```el
(require 'flymake-grammarly)
```

You can generally call `'flymake-grammarly-load` to any `mode-hook` for
grammarly to start working.

```el
(add-hook 'text-mode-hook 'flymake-grammarly-load)
(add-hook 'latex-mode-hook 'flymake-grammarly-load)
(add-hook 'org-mode-hook 'flymake-grammarly-load)
(add-hook 'markdown-mode-hook 'flymake-grammarly-load)
```

otherwise you can call `'flymake-grammarly-maybe-load` like the snippt below.

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
