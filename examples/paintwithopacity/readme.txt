Paint with opacity by Željan Rikalo
===================================

Demonstrates the per-DC drawing opacity API added to the LCL:

  WidgetSet.SetDCOpacity(DC: HDC; AOpacity: Byte); // 0 = transparent, 255 = opaque
  WidgetSet.GetDCOpacity(DC: HDC): Byte; // defaults to 255

Drag the slider to change the opacity used while painting the PaintBox.
The same opacity is applied to:

  - vector shapes (the coloured bars and the overlapping circles)
  - text (the WATERMARK caption)
  - images (the right-hand picture, drawn once normal and once with opacity)

The grid lists each widgetset and whether it supports DC opacity:

  gtk3, qt, qt5, qt6   - Yes
  cocoa                - in progress
  win32                - planned
  gtk2                 - no (needs heavy rework of TGtkDeviceContext)

The label shows which widgetset the running binary was built with.

opacity_test.png is loaded from the binary's directory and drawn as the
sample image. If it is missing, a generated pattern bitmap is used instead.

Requires Lazarus trunk 4.99 (https://gitlab.com/freepascal.org/lazarus/lazarus/-/commit/c124d5fd6f2d582ba8f5b511032053f55f6e2e9d)
or Lazarus 5.0 (the SetDCOpacity/GetDCOpacity methods do not exist in earlier releases).
