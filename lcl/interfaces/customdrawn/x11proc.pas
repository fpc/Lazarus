unit x11proc;

{$mode objfpc}{$H+}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  fpimage, fpcanvas,
  X, XLib,
  // Custom Drawn Canvas
  IntfGraphics, lazcanvas,
  //
  GraphType, Controls, LCLMessageGlue, WSControls, LCLType, LCLProc;

type
  TX11WindowInfo = class
  public
    Window: X.TWindow;
    LCLControl: TWinControl;
    // Used and valid only during event processing
    XEvent: PXEvent;
    // X11 extra objects
    Attr: XLib.TXWindowAttributes;
    Colormap: TColormap;
    GC: TGC;
    ColorDepth: Byte;
    // painting objects
    Image: TLazIntfImage;
    Canvas: TLazCanvas;
  end;

implementation

end.

