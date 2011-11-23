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

function RectToXRect(const ARect: TRect): TXRectangle;
function XRectToRect(const ARect: TXRectangle): TRect;
function XButtonToMouseButton(const XButton: cint; var MouseButton: TMouseButton): Boolean;
function GetXEventName(Event: LongInt): String;

implementation

function RectToXRect(const ARect: TRect): TXRectangle;
begin
  Result.x      := ARect.Left;
  Result.y      := ARect.Top;
  Result.width  := ARect.Right - ARect.Left;
  Result.height := ARect.Bottom - ARect.Top;
end;

function XRectToRect(const ARect: TXRectangle): TRect;
begin
  Result.Left   := ARect.x;
  Result.Top    := ARect.y;
  Result.Right  := ARect.x + ARect.width;
  Result.Bottom := ARect.y + ARect.height;
end;

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

function GetXEventName(Event: LongInt): String;
const
  EventNames: array[2..34] of String = (
    'KeyPress', 'KeyRelease', 'ButtonPress', 'ButtonRelease', 'MotionNotify',
    'EnterNotify', 'LeaveNotify', 'FocusIn', 'FocusOut', 'KeymapNotify',
    'Expose', 'GraphicsExpose', 'NoExpose', 'VisibilityNotify', 'CreateNotify',
    'DestroyNotify', 'UnmapNotify', 'MapNotify', 'MapRequest', 'ReparentNotify',
    'ConfigureNotify', 'ConfigureRequest', 'GravityNotify', 'ResizeRequest',
    'CirculateNotify', 'CirculateRequest', 'PropertyNotify', 'SelectionClear',
    'SelectionRequest', 'SelectionNotify', 'ColormapNotify', 'ClientMessage',
    'MappingNotify');
begin
  if (Event >= Low(EventNames)) and (Event <= High(EventNames)) then
    Result := EventNames[Event]
  else
    Result := '#' + IntToStr(Event);
end;

end.

