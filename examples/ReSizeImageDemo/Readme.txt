Resizing Example

LCL demo of redraw event order for images, panels and forms that should work
for all widget sets eg Windows, qt, OSX, GTK etc.

ClientWidth and ClientHeight rather than Width and Height are used so that the code works across widget sets eg Windows, qt, OSX, GTK etc because Height and Width do not allow for widget set and theme borders.

Checkboxes are used to call procedures for each of the redraw events. Each procedure writes to the canvas. They overwrite each other as they are called and the firing order is shown in a memo.

Repaint or Invalidate are not needed for resizing events.
