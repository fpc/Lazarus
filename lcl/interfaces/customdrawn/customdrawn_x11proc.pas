unit customdrawn_x11proc;

{$mode objfpc}{$H+}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  fpimage, fpcanvas, ctypes,
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

function XButtonToMouseButton(const XButton: cint; var MouseButton: TMouseButton): Boolean;

implementation

{ Returns True if the button is indeed a mouse button
  and False if it's the mouse wheel }
function XButtonToMouseButton(const XButton: cint; var MouseButton: TMouseButton): Boolean;
const
  ButtonTable: array[1..3] of TMouseButton = (mbLeft, mbMiddle, mbRight);
begin
  Result := False;

  if (XButton > 3) or (XButton < 1) then Exit;

  MouseButton := ButtonTable[XButton];

  Result := True;
end;

end.

