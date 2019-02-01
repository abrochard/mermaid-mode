# mermaid-mode
Emacs major mode for working with [mermaid graphs](https://mermaidjs.github.io/)

## Installation
Load the `mermaid-mode.el` file, and install `mmdc` binary from the [mermaid.cli project](https://github.com/mermaidjs/mermaid.cli) if you plan to compile graphs in Emacs.

## Usage
Currently supporting flow charts and sequence diagrams with syntax coloring and indentation.

```
C-c C-c to compile to an image
C-c C-o to open in the live editor
C-c C-h to open the official doc
```

## Customization
By default `mmdc` will compile to png format. You can change that by setting the variable `mermaid-output-format`.

## Bugs & Issues
- fix indentation for `else` statement
