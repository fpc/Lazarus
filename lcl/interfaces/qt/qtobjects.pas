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
  LCLType, Menus, LCLProc;

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
  public
    constructor Create(vHandle: QImageH); overload;
    constructor Create(Adata: PByte; width: Integer; height: Integer; format: QImageFormat); overload;
    destructor Destroy; override;
  public
    function height: Integer;
    function width: Integer;
    function bits: PByte;
    function numBytes: Integer;
  end;

  { TQtFont }

  TQtFont = class(TObject)
  private
  public
    Widget: QFontH;
    Angle: Integer;
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
    procedure family(retval: PWideString);
    function fixedPitch: Boolean;
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
    function width(p1: PWideString): Integer;
    function ascent: Integer;
    function descent: Integer;
    function leading: Integer;
    function maxWidth: Integer;
    function charWidth(str: WideString; pos: Integer): Integer;
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

  { TQtPen }

  TQtPen = class(TObject)
  private
  public
    Widget: QPenH;
  public
    constructor Create(CreateHandle: Boolean); virtual;
    destructor Destroy; override;
    function Width: Integer;
    function Style: QtPenStyle;
    procedure setStyle(style: QtPenStyle);
    procedure setBrush(brush: QBrushH);
    procedure setWidth(p1: Integer);
    procedure setColor(p1: TQColor);
  end;


  { TQtRegion }

  TQtRegion = class(TObject)
  private
  public
    Widget: QRegionH;
  public
    constructor Create(CreateHandle: Boolean); virtual; overload;
    constructor Create(CreateHandle: Boolean; X1,Y1,X2,Y2: Integer); virtual; overload;
    destructor Destroy; override;
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
    vPen: TQtPen;
    vRegion: TQtRegion;
  public
    constructor Create(WidgetHandle: THandle); virtual;
    destructor Destroy; override;
  public
    procedure drawPoint(x1: Integer; y1: Integer);
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
    function  pen: TQtPen;
    procedure setPen(pen: TQtPen);
    function region: TQtRegion;
    procedure setRegion(region: TQtRegion); 
    procedure drawImage(targetRect: PRect; image: QImageH; sourceRect: PRect; flags: QtImageConversionFlags = QtAutoColor);
    procedure rotate(a: Double);
    procedure save;
    procedure restore;
    procedure translate(dx: Double; dy: Double);
  end;
  
  { TQtPixmap }

  TQtPixmap = class(TObject)
  private
  public
    Handle: QPixmapH;
  public
    constructor Create(p1: PSize); virtual;
    destructor Destroy; override;
  public
    procedure grabWindow(p1: Cardinal; x: Integer = 0; y: Integer = 0; w: Integer = -1; h: Integer = -1);
    procedure toImage(retval: QImageH);
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
end;

{------------------------------------------------------------------------------
  Method: TQtImage.Create

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtImage.Create(Adata: PByte; width: Integer; height: Integer; format: QImageFormat);
begin
  Handle := QImage_create(AData, width, height, format);

  {$ifdef VerboseQt}
    WriteLn('TQtImage.Create Result:', PtrInt(Handle));
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
    WriteLn('TQtImage.Destroy Handle:', dbgs(Handle));
  {$endif}

  if Handle <> nil then QImage_destroy(Handle);

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
  Method: TQtImage.bits
  Params:  None
  Returns: The internal array of bits of the image
 ------------------------------------------------------------------------------}
function TQtImage.bits: PByte;
begin
  Result := QImage_bits(Handle);
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
  Str := UTF8Decode(p1);

  QFont_setRawName(Widget, @Str);
end;

procedure TQtFont.setFamily(p1: string);
var
  Str: WideString;
begin
  Str := UTF8Decode(p1);

  QFont_setFamily(Widget, @Str);
end;

procedure TQtFont.family(retval: PWideString);
begin
  QFont_family(Widget, retval);
end;

function TQtFont.fixedPitch: Boolean;
begin
  Result := QFont_fixedPitch(Widget);
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

function TQtFontMetrics.width(p1: PWideString): Integer;
begin
  Result := QFontMetrics_width(Widget, p1);
end;

function TQtFontMetrics.ascent: Integer;
begin
  Result := QFontMetrics_ascent(Widget);
end;

function TQtFontMetrics.descent: Integer;
begin
  Result := QFontMetrics_descent(Widget);
end;

function TQtFontMetrics.leading: Integer;
begin
  Result := QFontMetrics_leading(Widget);
end;

function TQtFontMetrics.maxWidth: Integer;
begin
  Result := QFontMetrics_maxWidth(Widget);
end;

function TQtFontMetrics.charWidth(str: WideString; pos: Integer): Integer;
begin
  Result := QFontMetrics_charWidth(Widget, @str, pos);
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

{ TQtPen }

{------------------------------------------------------------------------------
  Function: TQtPen.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtPen.Create(CreateHandle: Boolean);
var
   MyColor: TQColor;
begin

  {$ifdef VerboseQt}
    WriteLn('TQtPen.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}
  
  if CreateHandle then Widget := QPen_create;
end;

{------------------------------------------------------------------------------
  Function: TQtPen.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtPen.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtPen.Destroy');
  {$endif}

  QPen_destroy(Widget);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtPen.setBrush
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}

procedure TQtPen.setBrush(brush: QBrushH);
begin
  QPen_setBrush(Widget, brush);
end;

{------------------------------------------------------------------------------
  Function: TQtPen.setStyle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtPen.setStyle(style: QtPenStyle);
begin
  QPen_setStyle(Widget, style);
end;

{------------------------------------------------------------------------------
  Function: TQtPen.setWidth
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtPen.setWidth(p1: Integer);
begin
  QPen_setWidth(Widget, p1);
end;


{------------------------------------------------------------------------------
  Function: TQtPen.Style
  Params:  None
  Returns: QPenStyle
 ------------------------------------------------------------------------------}
function TQtPen.Style: QtPenStyle;
begin
  Result := QPen_Style(Widget);
end;


{------------------------------------------------------------------------------
  Function: TQtPen.Width
  Params:  None
  Returns: integer , width of current pen

 ------------------------------------------------------------------------------}
function TQtPen.Width: Integer;
begin
  Result := QPen_Width(Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtPen.setColor
  Params:  p1: TQColor
  Returns: Nothing
  Setting pen color. 
 ------------------------------------------------------------------------------}
procedure TQtPen.setColor(p1: TQColor);
var
  p2: TQColor;
begin
  QColor_fromRGB(@p2,p1.r,p1.g,p1.b,p1.Alpha);
  QPen_setColor(Widget, @p2);
end;


{ TQtRegion }

{------------------------------------------------------------------------------
  Function: TQtRegion.Create
  Params:  CreateHandle: Boolean
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtRegion.Create(CreateHandle: Boolean);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtRegion.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}

  // Creates the widget
  if CreateHandle then Widget := QRegion_create;
end;

{------------------------------------------------------------------------------
  Function: TQtRegion.Create (CreateRectRgn)
  Params:  CreateHandle: Boolean; X1,Y1,X2,Y2:Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtRegion.Create(CreateHandle: Boolean; X1,Y1,X2,Y2:Integer);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtRegion.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}

  // Creates the widget
  if CreateHandle then Widget := QRegion_create(X1,Y1,X2,Y2);
end;

{------------------------------------------------------------------------------
  Function: TQtRegion.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtRegion.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtRegion.Destroy');
  {$endif}

  QRegion_destroy(Widget);

  inherited Destroy;
end;


{ TQtDeviceContext }

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtDeviceContext.Create(WidgetHandle: THandle);
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
  vPen  := TQtPen.Create(False);
  vRegion := TQtRegion.Create(False);
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
  vPen.Widget := nil;
  vPen.Free;
  vRegion.Widget := nil;
  vRegion.Free;
  
  if vImage <> nil then QImage_destroy(vImage);

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
    Write('TQtDeviceContext.drawText TargetX: ', (Origin.X + X), ' TargetY: ', (Origin.Y + Y));
  {$endif}

  QtFontMetrics := TQtFontMetrics.Create(Font.Widget);
  try
    Save;

    translate(Origin.X + x, Origin.Y + y + QtFontMetrics.height);

    Rotate(-0.1 * vFont.Angle);

    QPainter_drawText(Widget, 0, 0, s);

    Restore;
    
    {$ifdef VerboseQt}
      WriteLn(' Font metrics height: ', QtFontMetrics.height, ' Angle: ', Round(0.1 * vFont.Angle));
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
  Function: TQtDeviceContext.drawPoint
  Params:  x1,y1 : Integer
  Returns: Nothing

  Draws a point. Helper function of winapi. DrawFocusRect
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawPoint(x1: Integer; y1: Integer);
begin
    QPainter_drawPoint(Widget, x1, y1);
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
  begin
    QPainter_setFont(Widget, QFontH(f.Widget));
    
    vFont.Angle := f.Angle;
  end;
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
  Function: TQtDeviceContext.pen
  Params:  None
  Returns: The current pen object of the DC
 ------------------------------------------------------------------------------}
function TQtDeviceContext.pen: TQtPen;
begin
  vPen.Widget := QPainter_pen(Widget);
  Result := vPen;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setPen
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setPen(pen: TQtPen);
begin
  if (pen.Widget <> nil) and (Widget <> nil) then QPainter_setPen(Widget, pen.Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.region
  Params:  None
  Returns: The current clip region
 ------------------------------------------------------------------------------}
function TQtDeviceContext.region: TQtRegion;
begin
  QPainter_clipRegion(Widget,  vRegion.Widget);
  Result := vRegion;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setRegion
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setRegion(region: TQtRegion);
begin
  if (region.Widget <> nil) and (Widget <> nil) then QPainter_setClipRegion(Widget, Region.Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawImage
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawImage(targetRect: PRect;
  image: QImageH; sourceRect: PRect; flags: QtImageConversionFlags = QtAutoColor);
var
  LocalRect: TRect;
begin
  LocalRect := targetRect^;
  
  LocalRect.Left := LocalRect.Left + Origin.X;

  LocalRect.Top := LocalRect.Top + Origin.Y;

  QPainter_drawImage(Widget, PRect(@LocalRect), image, sourceRect, flags);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.rotate
  Params:  None
  Returns: Nothing
  
  Rotates the coordinate system
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.rotate(a: Double);
begin
  QPainter_rotate(Widget, a);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.save
  Params:  None
  Returns: Nothing
  
  Saves the state of the canvas
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.save;
begin
  QPainter_save(Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.restore
  Params:  None
  Returns: Nothing
  
  Restores the state of the canvas
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.restore;
begin
  QPainter_restore(Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.translate
  Params:  None
  Returns: Nothing
  
  Tranlates the coordinate system
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.translate(dx: Double; dy: Double);
begin
  QPainter_translate(Widget, dx, dy);
end;

{ TQtPixmap }

constructor TQtPixmap.Create(p1: PSize);
begin
  Handle := QPixmap_create(p1);
end;

destructor TQtPixmap.Destroy;
begin
  if handle <> nil then QPixmap_destroy(handle);

  inherited Destroy;
end;

procedure TQtPixmap.grabWindow(p1: Cardinal; x: Integer; y: Integer; w: Integer; h: Integer);
begin
  QPixmap_grabWindow(Handle, p1, x, y, w, h);
end;

procedure TQtPixmap.toImage(retval: QImageH);
begin
  QPixmap_toImage(Handle, retval);
end;

end.

