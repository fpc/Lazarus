{
 *****************************************************************************
 *                              QtObjects.pas                                *
 *                              --------------                               *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit qtobjects;

{$mode delphi}{$H+}

interface

uses
  // Bindings
  qt4,
  // Free Pascal
  Classes, SysUtils, Types,
  // LCL
  Forms, Controls, LCLType, LCLProc, Menus;

type
  { TQtAction }

  TQtAction = class(TObject)
  private
  public
    Handle: QActionH;
    MenuItem: TMenuItem;
  public
    constructor Create(const AHandle: QActionH);
    destructor Destroy; override;
  public
    procedure SlotTriggered(checked: Boolean = False); cdecl;
  public
    procedure setChecked(p1: Boolean);
    procedure setEnabled(p1: Boolean);
    procedure setVisible(p1: Boolean);
  end;

  { TQtImage }

  TQtImage = class(TObject)
  public
    Handle: QImageH;
    Data: PByte;
    DataSize: Cardinal;
  public
    constructor Create(vHandle: QImageH); overload;
    constructor Create(Adata: PByte; width: Integer; height: Integer; format: QImageFormat; size: PtrInt); overload;
    destructor Destroy; override;
  public
    function height: Integer;
    function width: Integer;
    function numBytes: Integer;
  end;

  { TQtFont }

  TQtFont = class(TObject)
  private
  public
    Widget: QFontH;
  public
    constructor Create(CreateHandle: Boolean); virtual;
    destructor Destroy; override;
  public
    function pointSize: Integer;
    procedure setPointSize(p1: Integer);
    function pixelSize: Integer;
    procedure setPixelSize(p1: Integer);
    function weight: Integer;
    procedure setWeight(p1: Integer);
    procedure setBold(p1: Boolean);
    procedure setItalic(b: Boolean);
    procedure setUnderline(p1: Boolean);
    procedure setStrikeOut(p1: Boolean);
    procedure setRawName(p1: string);
    procedure setFamily(p1: string);
  end;

  { TQtFontMetrics }

  TQtFontMetrics = class(TObject)
  private
  public
    Widget: QFontMetricsH;
  public
    constructor Create(Parent: QFontH); virtual;
    destructor Destroy; override;
  public
    function height: Integer;
  end;

  { TQtBrush }

  TQtBrush = class(TObject)
  private
  public
    Widget: QBrushH;
  public
    constructor Create(CreateHandle: Boolean); virtual;
    destructor Destroy; override;
    procedure setStyle(style: QtBrushStyle);
  end;

  { TQtDeviceContext }

  TQtDeviceContext = class(TObject)
  private
  public
    Widget: QPainterH;
    Parent: QWidgetH;
    Origin: TPoint;
    vBrush: TQtBrush;
    vFont: TQtFont;
    vImage: QImageH;
  public
    constructor Create(WidgetHandle: HWND); virtual;
    destructor Destroy; override;
  public
    procedure drawRect(x1: Integer; y1: Integer; w: Integer; h: Integer);
    procedure drawText(x: Integer; y: Integer; s: PWideString);
    procedure drawLine(x1: Integer; y1: Integer; x2: Integer; y2: Integer);
    procedure drawEllipse(x: Integer; y: Integer; w: Integer; h: Integer);
    procedure setBrushOrigin(x, y: Integer);
    procedure brushOrigin(retval: PPoint);
    function font: TQtFont;
    procedure setFont(f: TQtFont);
    function brush: TQtBrush;
    procedure setBrush(brush: TQtBrush);
    procedure drawImage(targetRect: PRect; image: QImageH; sourceRect: PRect; flags: QtImageConversionFlags = QtAutoColor);
  end;
  
implementation

uses qtwidgets;

{ TQtAction }

constructor TQtAction.Create(const AHandle: QActionH);
begin
  Handle := AHandle;
end;

destructor TQtAction.Destroy;
begin
  inherited Destroy;
end;

procedure TQtAction.SlotTriggered(checked: Boolean); cdecl;
begin
  if Assigned(MenuItem) and Assigned(MenuItem.OnClick) then MenuItem.OnClick(Self);
end;

procedure TQtAction.setChecked(p1: Boolean);
begin
  QAction_setChecked(Handle, p1);
end;

procedure TQtAction.setEnabled(p1: Boolean);
begin
  QAction_setEnabled(Handle, p1);
end;

procedure TQtAction.setVisible(p1: Boolean);
begin
  QAction_setVisible(Handle, p1);
end;

{ TQtImage }

{------------------------------------------------------------------------------
  Method: TQtImage.Create

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtImage.Create(vHandle: QImageH);
begin
  Handle := vHandle;
  
  DataSize := 0;
end;

{------------------------------------------------------------------------------
  Method: TQtImage.Create

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtImage.Create(Adata: PByte; width: Integer; height: Integer; format: QImageFormat; size: PtrInt);
begin
  DataSize := size;

  GetMem(Data, DataSize);
  
//  FillChar(Data^, DataSize, #0);

  Move(Adata^, Data^, DataSize);

  Handle := QImage_create(Data, width, height, format);

  {$ifdef VerboseQt}
    WriteLn('TQtImage.Create Size:', dbgs(size), ' Result:', dbgs(Handle));
  {$endif}
end;

{------------------------------------------------------------------------------
  Method: TQtImage.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TQtImage.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtImage.Destroy Handle:', dbgs(Handle), ' DataSize', dbgs(DataSize));
  {$endif}

  if Handle <> nil then QImage_destroy(Handle);
  
  if DataSize > 0 then FreeMem(Data, DataSize);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TQtImage.height
  Params:  None
  Returns: The height of the image
 ------------------------------------------------------------------------------}
function TQtImage.height: Integer;
begin
  Result := QImage_height(Handle);
end;

{------------------------------------------------------------------------------
  Method: TQtImage.width
  Params:  None
  Returns: The width of the image
 ------------------------------------------------------------------------------}
function TQtImage.width: Integer;
begin
  Result := QImage_width(Handle);
end;

{------------------------------------------------------------------------------
  Method: TQtImage.numBytes
  Params:  None
  Returns: The number of bytes the image occupies in memory
 ------------------------------------------------------------------------------}
function TQtImage.numBytes: Integer;
begin
  Result := QImage_numBytes(Handle);
end;

{ TQtFont }

{------------------------------------------------------------------------------
  Function: TQtFont.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtFont.Create(CreateHandle: Boolean);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtFont.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}

  if CreateHandle then Widget := QFont_create;
end;

{------------------------------------------------------------------------------
  Function: TQtFont.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtFont.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtFont.Destroy');
  {$endif}

  if Widget <> nil then QFont_destroy(Widget);

  inherited Destroy;
end;

function TQtFont.pointSize: Integer;
begin
  Result := QFont_pointSize(Widget);
end;

procedure TQtFont.setPointSize(p1: Integer);
begin
  QFont_setPointSize(Widget, p1);
end;

function TQtFont.pixelSize: Integer;
begin
  Result := QFont_pixelSize(Widget);
end;

procedure TQtFont.setPixelSize(p1: Integer);
begin
  QFont_setPixelSize(Widget, p1);
end;

function TQtFont.weight: Integer;
begin
  Result := QFont_weight(Widget);
end;

procedure TQtFont.setWeight(p1: Integer);
begin
  QFont_setWeight(Widget, p1);
end;

procedure TQtFont.setBold(p1: Boolean);
begin
  QFont_setBold(Widget, p1);
end;

procedure TQtFont.setItalic(b: Boolean);
begin
  QFont_setItalic(Widget, b);
end;

procedure TQtFont.setUnderline(p1: Boolean);
begin
  QFont_setUnderline(Widget, p1);
end;

procedure TQtFont.setStrikeOut(p1: Boolean);
begin
  QFont_setStrikeOut(Widget, p1);
end;

procedure TQtFont.setRawName(p1: string);
var
  Str: WideString;
begin
  Str := WideString(p1);

  QFont_setRawName(Widget, @Str);
end;

procedure TQtFont.setFamily(p1: string);
var
  Str: WideString;
begin
  Str := WideString(p1);

  QFont_setFamily(Widget, @Str);
end;

{ TQtFontMetrics }

constructor TQtFontMetrics.Create(Parent: QFontH);
begin
  Widget := QFontMetrics_create(Parent);
end;

destructor TQtFontMetrics.Destroy;
begin
  QFontMetrics_destroy(Widget);

  inherited Destroy;
end;

function TQtFontMetrics.height: Integer;
begin
  Result := QFontMetrics_height(Widget);
end;

{ TQtBrush }

{------------------------------------------------------------------------------
  Function: TQtBrush.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtBrush.Create(CreateHandle: Boolean);
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtBrush.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}

  if CreateHandle then Widget := QBrush_create;
end;

{------------------------------------------------------------------------------
  Function: TQtBrush.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtBrush.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtBrush.Destroy');
  {$endif}

  QBrush_destroy(Widget);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtBrush.setStyle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtBrush.setStyle(style: QtBrushStyle);
begin
  QBrush_setStyle(Widget, style);
end;

{ TQtDeviceContext }

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtDeviceContext.Create(WidgetHandle: HWND);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtDeviceContext.Create ( WidgetHandle: ' + IntToStr(WidgetHandle) + ' )');
  {$endif}

  if WidgetHandle = 0 then
  begin
    Parent := nil;
    Widget := QPainter_create
  end
  else
  begin
    Parent := TQtMainWindow(WidgetHandle).Widget;
    Widget := QPainter_create(QWidget_to_QPaintDevice(Parent));
  end;

  vBrush := TQtBrush.Create(False);
  vFont := TQtFont.Create(False);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtDeviceContext.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtDeviceContext.Destroy');
  {$endif}

  vBrush.Widget := nil;
  vBrush.Free;
  vFont.Widget := nil;
  vFont.Free;
  
//  if vImage <> nil then NilAndFree(vImage);

  QPainter_destroy(Widget);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawRect
  Params:  None
  Returns: Nothing

  Draws a rectangle. Helper function of winapi.Rectangle
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawRect(x1: Integer; y1: Integer; w: Integer; h: Integer);
begin
  QPainter_drawRect(Widget, x1, y1, w, h);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawText
  Params:  None
  Returns: Nothing

  Draws a Text. Helper function of winapi.TextOut
  
  Qt does not draw the text starting at Y position and downwards, like LCL.

  Instead, Y becomes the baseline for the text and it´s drawn upwards.
  
  To get a correct behavior we need to sum the text´s height to the Y coordinate.
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawText(x: Integer; y: Integer; s: PWideString);
var
  QtFontMetrics: TQtFontMetrics;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtDeviceContext.drawText TargetX: ', dbgs(Origin.X + X),
     ' TargetY: ', dbgs(Origin.Y + Y));
  {$endif}

  QtFontMetrics := TQtFontMetrics.Create(Font.Widget);
  try
    QPainter_drawText(Widget, Origin.X + x, Origin.Y + y + QtFontMetrics.height, s);

    {$ifdef VerboseQt}
      WriteLn(' Font metrics height: ', dbgs(QtFontMetrics.height));
    {$endif}
  finally
    QtFontMetrics.Free;
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawLine
  Params:  None
  Returns: Nothing

  Draws a Text. Helper function for winapi.LineTo
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawLine(x1: Integer; y1: Integer; x2: Integer; y2: Integer);
begin
  QPainter_drawLine(Widget, x1, y1, x2, y2);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawEllipse
  Params:  None
  Returns: Nothing

  Draws a ellipse. Helper function for winapi.Ellipse
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawEllipse(x: Integer; y: Integer; w: Integer; h: Integer);
begin
  QPainter_drawEllipse(Widget, x, y, w, h);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setBrushOrigin
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setBrushOrigin(x, y: Integer);
begin
  QPainter_setBrushOrigin(Widget, x, y);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.brushOrigin
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.brushOrigin(retval: PPoint);
begin
  QPainter_brushOrigin(Widget, retval);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.font
  Params:  None
  Returns: The current font object of the DC
 ------------------------------------------------------------------------------}
function TQtDeviceContext.font: TQtFont;
begin
  vFont.Widget := QPainter_font(Widget);

  Result := vFont;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setFont
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setFont(f: TQtFont);
begin
  if (f.Widget <> nil) and (Widget <> nil) and (Parent <> nil) then
   QPainter_setFont(Widget, QFontH(f.Widget));
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.brush
  Params:  None
  Returns: The current brush object of the DC
 ------------------------------------------------------------------------------}
function TQtDeviceContext.brush: TQtBrush;
begin
  vBrush.Widget := QPainter_brush(Widget);

  Result := vBrush;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setBrush
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setBrush(brush: TQtBrush);
begin
  if (brush.Widget <> nil) and (Widget <> nil) then QPainter_setBrush(Widget, brush.Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawImage
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawImage(targetRect: PRect;
  image: QImageH; sourceRect: PRect; flags: QtImageConversionFlags = QtAutoColor);
begin
  QPainter_drawImage(Widget, targetRect, image, sourceRect, flags);
end;

end.

