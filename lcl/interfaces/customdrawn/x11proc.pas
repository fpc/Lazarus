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
    XEvent: PXEvent;
    Image: TLazIntfImage;
    Canvas: TLazCanvas;
  end;

implementation

end.

