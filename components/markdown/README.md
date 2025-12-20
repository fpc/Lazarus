## Markdown control for Lazarus

### Intro

This directory contains a control that displays markdown.
It parses and renders markdown directly, there is no conversion to HTML.

All standard markdown is supported, see the demo.

The markdown panel automatically adjusts to the height needed to display the
markdown.

Images are fetched through a callback. clicks on hyperlinks are also handled
through an event.

You can select the text, and the Ctrl-C or Ctrl-Insert will copy the
selection to the keyboard.

### Installation
The main package is lazmarkdown.lpk

The package installs the control on the component palette, under tab Misc.

The sources for the markdown parser are copied from FPC trunk to Src_3_2.
Updates will be copied over as long as FPC trunk is not released.

# Demo

The demo can be compiled as-is; you do not have to install the package
first. But you must have opened it so lazarus knows where to find it.

### Todo
- Canvas - Some optimization: do not draw beyond size of canvas
- Copy to clipboard as html ?
- Mouse events.
- No autosize of control ?
- Make selection optional ?

### Component palette icons.
The markdown icons were taken from Icons8:
https://icons8.com/icon/121792/markdown
