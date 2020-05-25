[![MELPA](https://melpa.org/packages/mermaid-mode-badge.svg)](https://melpa.org/#/mermaid-mode)

# mermaid-mode

Emacs major mode for working with [mermaid graphs](https://mermaidjs.github.io/)

![Screenshot](./assets/screenshot.jpg "Screenshot")

[![Thumbnail](./assets/thumbnail.png)](https://vimeo.com/414458581 "Screencast")

## Installation

1. Install from Melpa or load the `mermaid-mode.el` file
1. Install `mmdc` binary from the [mermaid.cli project](https://github.com/mermaidjs/mermaid.cli) if you plan to compile graphs in Emacs

## Usage

Currently supporting flow charts and sequence diagrams with syntax coloring and indentation.

```text
C-c C-d c - compile current file to an image
C-c C-d f - compile given file to an image
C-c C-d b - compile current buffer to an image
C-c C-d r - compile current region to an image
C-c C-d o - open in the live editor
C-c C-d d - open the official doc
```
Note: `C-d` is inspired by `mermai(d)` (`C-m` was/is obviously taken).

Note: All compile commands will open the output in a buffer to view the resulting image.

## Customization

You can specify the location of `mmdc` with the variable `mermaid-mmdc-location`, the default assumes you have the binary in your `PATH` (and for that you probably want/need to install [`mermaid.cli`](https://github.com/mermaidjs/mermaid.cli)).

By default `mmdc` will compile to png format. You can change that by setting the variable `mermaid-output-format`.

By default `mermaid-tmp-dir` points to `\tmp\`. Feel free to set it to a more appropriate location that works for you (e.g. on windows).

## Bugs & Issues

Feel free to open an issue!
