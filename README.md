This is forked from https://github.com/mutbuerger/iPython-mode.el

# iPython-mode

![iPython-mode](https://raw.github.com/mutbuerger/iPython-mode.el/master/iPython-mode.gif "iPython-mode showcase")

This quick hack provides a minor-mode and tries to emulate my R/[ESS](http://ess.r-project.org/) workflow of coding in a [Python-mode](https://launchpad.net/python-mode) buffer sending lines, blocks and buffers bit by bit to the interpreter. So it's more or less a compilation of key settings, hooks and a few functions to achieve a more consistent workflow when switching between R and Python code repeatedly.

# Installation

1. Download iPython-mode.el
2. Specify `load-path` in `init.el`:

``` common-lisp
(add-to-list 'load-path "~/path/to/iPython-mode")
(require iPython-mode)
```

# Usage

<kbd>C-c C-j</kbd> ... execute line and show interpreter in other window

<kbd>C-c C-c</kbd> ... execute active region or buffer

<kbd>C-c C-z</kbd> ... switch to interpreter | back to code

[Jedi](http://tkf.github.io/emacs-jedi/) autocompletion and [YaSnippet](https://github.com/capitaomorte/yasnippet) (python-mode) are enabled both in Python-mode and the iPython buffer. 
