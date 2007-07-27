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

{$mode objfpc}{$H+}

interface

uses
  // Bindings
{$ifdef USE_QT_4_3}
  qt43,
{$else}
  qt4,
{$endif}
  // Free Pascal
  Classes, SysUtils, Types,
  // LCL
  LCLType, Menus, LCLProc, Graphics;

type
  // forward declarations
  TQtImage = class;

  { TQtObject }
  TQtObject = class(TObject)
  private
    FUpdateCount: Integer;
  public
    FEventHook: QObject_hookH;
    TheObject: QObjectH;
    // TODO: base virtual constructor with initialization
    destructor Destroy; override;
  public
    procedure AttachEvents; virtual;
    procedure DetachEvents; virtual;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; virtual; abstract;
  public
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function InUpdate: Boolean;
  end;

  { TQtResource }

  TQtResource = class(TObject)
  public
    Owner: TObject;
  end;

  { TQtAction }

  TQtAction = class(TObject)
  private
    FIcon: QIconH;
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
    procedure setCheckable(p1: Boolean);
    procedure setEnabled(p1: Boolean);
    procedure setIcon(const AIcon: QIconH);
    procedure setImage(const AImage: TQtImage);
    procedure setVisible(p1: Boolean);
  end;

  { TQtImage }

  TQtImage = class(TObject)
  private
    FData: PByte;
    FDataOwner: Boolean;
  public
    Handle: QImageH;
  public
    constructor Create(vHandle: QImageH); overload;
    constructor Create(Adata: PByte; width: Integer; height: Integer; format: QImageFormat; const ADataOwner: Boolean = False); overload;
    destructor Destroy; override;
    function AsIcon(AMode: QIconMode = QIconNormal; AState: QIconState = QIconOff): QIconH;
    function AsPixmap: QPixmapH;
  public
    function height: Integer;
    function width: Integer;
    function bits: PByte;
    function numBytes: Integer;
  end;

  { TQtFont }

  TQtFont = class(TQtResource)
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

  TQtBrush = class(TQtResource)
  private
  public
    Widget: QBrushH;
    constructor Create(CreateHandle: Boolean); virtual;
    destructor Destroy; override;
    procedure setStyle(style: QtBrushStyle);
  end;

  { TQtPen }

  TQtPen = class(TQtResource)
  private
  public
    Widget: QPenH;
    constructor Create(CreateHandle: Boolean); virtual;
    destructor Destroy; override;
  public
    function Width: Integer;
    function Style: QtPenStyle;
    procedure setStyle(AStyle: QtPenStyle);
    procedure setBrush(brush: QBrushH);
    procedure setWidth(p1: Integer);
    procedure setColor(p1: TQColor);
  end;


  { TQtRegion }

  TQtRegion = class(TQtResource)
  private
  public
    Widget: QRegionH;
    constructor Create(CreateHandle: Boolean); virtual; overload;
    constructor Create(CreateHandle: Boolean; X1,Y1,X2,Y2: Integer;
      Const RegionType: QRegionRegionType = QRegionRectangle); virtual; overload;
    constructor Create(CreateHandle: Boolean; Poly: QPolygonH;
      Const Fill: QtFillRule = QtWindingFill); virtual; overload;
    destructor Destroy; override;
    function GetRegionType: integer;
  end;

  // NOTE: PQtDCData was a pointer to a structure with QPainter information
  //       about current state, currently this functionality is implemented
  //       using native functions qpainter_save and qpainter_restore. If in
  //       future it needs to save/restore aditional information, PQtDCData
  //       should point to a structure holding the additional information.
  //       see SaveDC and RestoreDC for more information.
  //       for example: what about textcolor, it's currently not saved....
  PQtDCData = pointer;

  { TQtDeviceContext }

  TQtDeviceContext = class(TObject)
  private
    SelFont: TQTFont;
    SelBrush: TQTBrush;
    SelPen: TQtPen;
    PenColor: TQColor;
    procedure RestorePenColor;
    procedure RestoreTextColor;
  public
    { public fields }
    Widget: QPainterH;
    Parent: QWidgetH;
    ParentPixmap: QPixmapH;
    vBrush: TQtBrush;
    vFont: TQtFont;
    vImage: QImageH;
    vPen: TQtPen;
    vRegion: TQtRegion;
    vBackgroundBrush: TQtBrush;
    vClipRect: PRect;         // is the cliprect paint event give to us
    vClipRectDirty: boolean;  // false=paint cliprect is still valid
    vTextColor: TColor;
  public
    { Our own functions }
    constructor Create(AWidget: QWidgetH; Const APaintEvent: Boolean = False); virtual;
    destructor Destroy; override;
    function CreateDCData: PQtDCDATA;
    function RestoreDCData(DCData: PQtDCData): boolean;
    procedure DebugClipRect(const msg: string);
    procedure setImage(AImage: TQtImage);
    procedure CorrectCoordinates(var ARect: TRect);
  public
    { Qt functions }
    procedure drawPoint(x1: Integer; y1: Integer);
    procedure drawRect(x1: Integer; y1: Integer; w: Integer; h: Integer);
    procedure drawText(x: Integer; y: Integer; s: PWideString); overload;
    procedure DrawText(x,y,w,h,flags: Integer; s:PWideString); overload;
    procedure drawLine(x1: Integer; y1: Integer; x2: Integer; y2: Integer);
    procedure drawEllipse(x: Integer; y: Integer; w: Integer; h: Integer);
    procedure fillRect(ARect: PRect; ABrush: QBrushH); overload;
    procedure fillRect(x, y, w, h: Integer; ABrush: QBrushH); overload;
    procedure fillRect(x, y, w, h: Integer); overload;

    procedure setBrushOrigin(x, y: Integer);
    procedure brushOrigin(retval: PPoint);
    function font: TQtFont;
    procedure setFont(f: TQtFont);
    function brush: TQtBrush;
    procedure setBrush(ABrush: TQtBrush);
    function BackgroundBrush: TQtBrush;
    function  pen: TQtPen;
    procedure setPen(APen: TQtPen);
    function  SetBkColor(Color: TcolorRef): TColorRef;
    function  SetBkMode(BkMode: Integer): Integer;
    function getRegionType(ARegion: QRegionH): integer;
    function region: TQtRegion;
    procedure setRegion(ARegion: TQtRegion);
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
    class procedure fromImage(retval: QPixmapH; image: QImageH; flags: QtImageConversionFlags = QtAutoColor);
  end;
  
  { TQtSystemTrayIcon }

  TQtSystemTrayIcon = class(TObject)
  private
  public
    Handle: QSystemTrayIconH;
  public
    constructor Create(vIcon: QIconH); virtual;
    destructor Destroy; override;
  public
    procedure setContextMenu(menu: QMenuH);
    procedure setIcon(icon: QIconH);
    procedure setToolTip(tip: WideString);
    procedure show;
    procedure hide;
  end;
  
  { TQtButtonGroup }
  
  TQtButtonGroup = class(TObject)
  private
  public
    constructor Create(AParent: QObjectH); virtual;
    destructor Destroy; override;
    Handle: QButtonGroupH;
  public
    procedure AddButton(AButton: QAbstractButtonH); overload;
    procedure AddButton(AButton: QAbstractButtonH; Id: Integer); overload;
    function ButtonFromId(id: Integer): QAbstractButtonH;
    procedure RemoveButton(AButton: QAbstractButtonH);
    function GetExclusive: Boolean;
    procedure SetExclusive(AExclusive: Boolean);
    procedure SignalButtonClicked(AButton: QAbstractButtonH); cdecl;
  end;
  
  { TQtClipboard }
  
  TQtClipboard = class(TQtObject)
  private
    FClipBoardFormats: TStringList;
    FOnClipBoardRequest: TClipboardRequestEvent;
  public
    constructor Create;
    destructor Destroy; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;

    function Clipboard: QClipboardH; inline;
    
    function getMimeData(AMode: QClipboardMode): QMimeDataH;
    procedure setMimeData(AMimeData: QMimeDataH; AMode: QClipboardMode);
    procedure Clear(AMode: QClipboardMode);
    
    function FormatToMimeType(AFormat: TClipboardFormat): String;
    function RegisterFormat(AMimeType: String): TClipboardFormat;
    function GetData(ClipboardType: TClipboardType; FormatID: TClipboardFormat;
      Stream: TStream): boolean;
    function GetFormats(ClipboardType: TClipboardType; var Count: integer;
      var List: PClipboardFormat): boolean;
    function GetOwnerShip(ClipboardType: TClipboardType; OnRequestProc: TClipboardRequestEvent;
      FormatCount: integer; Formats: PClipboardFormat): boolean;
  end;

  procedure TQColorToColorRef(const AColor: TQColor; out AColorRef: TColorRef);
  procedure ColorRefToTQColor(const AColorRef: TColorRef; var AColor:TQColor);
  procedure DebugRegion(const msg: string; Rgn: QRegionH);
  function Clipboard: TQtClipboard;

implementation
const
  ClipbBoardTypeToQtClipboard: array[TClipboardType] of QClipboardMode =
  (
{ctPrimarySelection  } QClipboardSelection,
{ctSecondarySelection} QClipboardSelection,
{ctClipboard         } QClipboardClipboard
  );

var
  FClipboard: TQtClipboard = nil;
  
{ TQtObject }

destructor TQtObject.Destroy;
begin
  if TheObject <> nil then
  begin
    DetachEvents;
    QObject_destroy(TheObject);
    TheObject := nil;
  end;
  inherited Destroy;
end;

procedure TQtObject.AttachEvents;
var
  Method: TMethod;
begin
  FEventHook := QObject_hook_create(TheObject);
  TEventFilterMethod(Method) := @EventFilter;
  QObject_hook_hook_events(FEventHook, Method);
end;

procedure TQtObject.DetachEvents;
begin
  if FEventHook <> nil then
  begin
    QObject_hook_destroy(FEventHook);
    FEventHook := nil;
  end;
end;

procedure TQtObject.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TQtObject.EndUpdate;
begin
  if FUpdateCount > 0 then
    dec(FUpdateCount);
end;

function TQtObject.InUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

{ TQtAction }

{------------------------------------------------------------------------------
  Method: TQtAction.Create

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtAction.Create(const AHandle: QActionH);
begin
  Handle := AHandle;
  FIcon := nil;
end;

{------------------------------------------------------------------------------
  Method: TQtAction.Destroy

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TQtAction.Destroy;
begin
  if FIcon <> nil then
    QIcon_destroy(FIcon);

  if Handle <> nil then
    QAction_destroy(Handle);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TQtAction.SlotTriggered

  Callback for menu item click
 ------------------------------------------------------------------------------}
procedure TQtAction.SlotTriggered(checked: Boolean); cdecl;
begin
  if Assigned(MenuItem) and Assigned(MenuItem.OnClick) then
   MenuItem.OnClick(Self.MenuItem);
end;

{------------------------------------------------------------------------------
  Method: TQtAction.setChecked

  Checks or unchecks a menu entry
  
  To mimic the behavior LCL should have we added code to handle
 setCheckable automatically
 ------------------------------------------------------------------------------}
procedure TQtAction.setChecked(p1: Boolean);
begin
  if p1 then setCheckable(True)
  else setCheckable(False);

  QAction_setChecked(Handle, p1);
end;

{------------------------------------------------------------------------------
  Method: TQtAction.setCheckable

  Set's if a menu can be checked. Is false by default
 ------------------------------------------------------------------------------}
procedure TQtAction.setCheckable(p1: Boolean);
begin
  QAction_setCheckable(Handle, p1);
end;

{------------------------------------------------------------------------------
  Method: TQtAction.setEnabled
 ------------------------------------------------------------------------------}
procedure TQtAction.setEnabled(p1: Boolean);
begin
  QAction_setEnabled(Handle, p1);
end;

procedure TQtAction.setIcon(const AIcon: QIconH);
begin
  QAction_setIcon(Handle, AIcon);
end;

procedure TQtAction.setImage(const AImage: TQtImage);
begin
  if FIcon <> nil then
  begin
    QIcon_destroy(FIcon);
    FIcon := nil;
  end;
  
  if AImage <> nil then
    FIcon := AImage.AsIcon()
  else
    FIcon := QIcon_create();

  setIcon(FIcon);
end;

{------------------------------------------------------------------------------
  Method: TQtAction.setVisible
 ------------------------------------------------------------------------------}
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
  FData := nil;
  FDataOwner := False;
end;

{------------------------------------------------------------------------------
  Method: TQtImage.Create

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtImage.Create(Adata: PByte; width: Integer; height: Integer;
  format: QImageFormat; const ADataOwner: Boolean = False);
begin

  FData := AData;
  FDataOwner := ADataOwner;

  if FData = nil then
    Handle := QImage_create(width, height, format)
  else
    Handle := QImage_create(FData, width, height, format);
    
  {$ifdef VerboseQt}
    WriteLn('TQtImage.Create Result:', dbghex(PtrInt(Handle)));
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

  if Handle <> nil then
    QImage_destroy(Handle);
  if (FDataOwner) and (FData <> nil) then 
    FreeMem(FData);

  inherited Destroy;
end;

function TQtImage.AsIcon(AMode: QIconMode = QIconNormal; AState: QIconState = QIconOff): QIconH;
var
  APixmap: QPixmapH;
begin
  APixmap := AsPixmap;
  Result := QIcon_create();
  if Result <> nil then
    QIcon_addPixmap(Result, APixmap, AMode, AState);
  QPixmap_destroy(APixmap);
end;

function TQtImage.AsPixmap: QPixmapH;
begin
  Result := QPixmap_create();
  QPixmap_fromImage(Result, Handle);
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
begin
  {$ifdef VerboseQt}
    WriteLn('TQtPen.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}
  
  if CreateHandle then
    Widget := QPen_create;
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
procedure TQtPen.setStyle(AStyle: QtPenStyle);
begin
  QPen_setStyle(Widget, AStyle);
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
constructor TQtRegion.Create(CreateHandle: Boolean; X1,Y1,X2,Y2:Integer;
  Const RegionType: QRegionRegionType = QRegionRectangle);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtRegion.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}

  // Creates the widget
  if CreateHandle then Widget := QRegion_create(X1,Y1,X2,Y2, RegionType);
end;

constructor TQtRegion.Create(CreateHandle: Boolean; Poly: QPolygonH;
  Const Fill: QtFillRule = QtWindingFill);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtRegion.Create polyrgn CreateHandle: ', dbgs(CreateHandle));
  {$endif}
  if CreateHandle then Widget := QRegion_create(Poly, Fill);
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

function TQtRegion.GetRegionType: integer;
var
  R: TRect;
begin
  try
    if QRegion_isEmpty(Widget) then
      Result := NULLREGION
    else
    begin
      QRegion_boundingRect(Widget, @R);
      if QRegion_contains(Widget, PRect(@R)) then
        Result := SIMPLEREGION
      else
        Result := COMPLEXREGION;
    end;
  except
    Result := ERROR;
  end;
end;

{ TQtDeviceContext }

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtDeviceContext.Create(AWidget: QWidgetH; Const APaintEvent: Boolean = False);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtDeviceContext.Create ( WidgetHandle: ', dbghex(WidgetHandle), ' FromPaintEvent:',BoolToStr(FromPaintEvent),' )');
  {$endif}

  {NOTE FOR QT DEVELOPERS: Whenever you call TQtDeviceContext.Create() outside
   of TQtWidgetSet.BeginPaint() SET APaintEvent TO FALSE !}
  if AWidget = nil then
  begin
    ParentPixmap := QPixmap_Create(10,10);
    Widget := QPainter_Create(QPaintDeviceH(ParentPixmap));
  end
  else
  begin
    Parent := AWidget;
    if not APaintEvent then
    begin
      ParentPixmap := QPixmap_Create(QWidget_width(Parent), QWidget_height(Parent));
      Widget := QPainter_create(QPaintDeviceH(ParentPixmap));
    end
    else
      Widget := QPainter_create(QWidget_to_QPaintDevice(Parent));
  end;
  vBrush := TQtBrush.Create(False);
  vBrush.Owner := Self;
  vFont := TQtFont.Create(False);
  vFont.Owner := Self;;
  vPen := TQtPen.Create(False);
  vPen.Owner := Self;
  vRegion := TQtRegion.Create(False);
  vRegion.Owner := Self;
  vBackgroundBrush := TQtBrush.Create(False);
  vBackgroundBrush.Owner := Self;
  vTextColor := ColorToRGB(clWindowText);
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

  if (vClipRect <> nil) then
    dispose(vClipRect);

  vBrush.Widget := nil;
  vBrush.Free;
  vFont.Widget := nil;
  vFont.Free;
  vPen.Widget := nil;
  vPen.Free;
  vRegion.Widget := nil;
  vRegion.Free;
  vBackgroundBrush.Widget := nil;
  vBackgroundBrush.Free;

  if vImage <> nil then
    QImage_destroy(vImage);

  if Widget <> nil then
    QPainter_destroy(Widget);

  if ParentPixmap <> nil then
    QPixmap_destroy(ParentPixmap);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.DebugClipRect
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.DebugClipRect(const msg: string);
var
  Rgn: QRegionH;
  ok: boolean;
begin
  ok := QPainter_hasClipping(Widget);
  Write(Msg, 'DC: HasClipping=', ok);
  
  if Ok then
  begin
    Rgn := QRegion_Create;
    QPainter_ClipRegion(Widget, Rgn);
    DebugRegion('', Rgn);
    QRegion_Destroy(Rgn);
  end
  else
    WriteLn;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setImage
  Params:  None
  Returns: Nothing
  
  This function will destroy the previous DC handle and generate
 a new one based on an image. This is necessary because when painting
 is being done to a TBitmap, LCL will create a completely abstract DC,
 using GetDC(0), and latter use SelectObject to associate that DC
 with the Image.
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setImage(AImage: TQtImage);
begin
  {$ifdef VerboseQt}
  writeln('TQtDeviceContext.setImage() ');
  {$endif}
  vImage := AImage.Handle;
  
  QPainter_destroy(Widget);

  Widget := QPainter_Create(vImage);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.CorrectCoordinates
  Params:  None
  Returns: Nothing
  
  If you draw an image with negative coordinates
 (for example x: -50 y: -50 w: 100 h: 100), the result is not well
 defined in Qt, and could well be: (x: 0 y: 0 w: 100 h: 100)
  This method corrects the coordinates, cutting the result, so we draw:
 (x: 0 y: 0 w: 50 h: 50)
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.CorrectCoordinates(var ARect: TRect);
begin
  if ARect.Left < 0 then ARect.Left := 0;

  if ARect.Top < 0 then ARect.Top := 0;

{  if ARect.Right > MaxRight then ARect.Right := MaxRight;

  if ARect.Bottom > MaxBottom then ARect.Bottom := MaxBottom;}
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.CreateDCData
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtDeviceContext.CreateDCData: PQtDCDATA;
begin
  {$ifdef VerboseQt}
  writeln('TQtDeviceContext.CreateDCData() ');
  {$endif}
  Qpainter_save(Widget);
  result := nil; // doesn't matter;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.RestoreDCData
  Params:  DCData, dummy in current implementation
  Returns: true if QPainter state was successfuly restored
 ------------------------------------------------------------------------------}
function TQtDeviceContext.RestoreDCData(DCData: PQtDCData):boolean;
begin
  {$ifdef VerboseQt}
  writeln('TQtDeviceContext.RestoreDCData() ');
  {$endif}
  QPainter_restore(Widget);
  result:=true;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.RestorePenColor
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.RestorePenColor;
begin
  {$ifdef VerboseQt}
  writeln('TQtDeviceContext.RestorePenColor() ');
  {$endif}
  Qpainter_setPen(Widget, @PenColor);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.RestoreTextColor
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.RestoreTextColor;
var
  CurPen: QPenH;
  TxtColor: TQColor;
begin
  {$ifdef VerboseQt}
  writeln('TQtDeviceContext.RestoreTextColor() ');
  {$endif}
  CurPen := QPainter_Pen(Widget);
  QPen_color(CurPen, @PenColor);
  TxtColor := PenColor;
  ColorRefToTQColor(vTextColor, TxtColor);
  Qpainter_setPen(Widget, @txtColor);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawRect
  Params:  None
  Returns: Nothing

  Draws a rectangle. Helper function of winapi.Rectangle
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawRect(x1: Integer; y1: Integer; w: Integer; h: Integer);
begin
  {$ifdef VerboseQt}
  writeln('TQtDeviceContext.drawRect() x1: ',x1,' y1: ',y1,' w: ',w,' h: ',h);
  {$endif}
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
  Write('TQtDeviceContext.drawText TargetX: ', X, ' TargetY: ', Y);
  {$endif}

  QtFontMetrics := TQtFontMetrics.Create(Font.Widget);
  try

    Save;
    
    translate(x, y);
    Rotate(-0.1 * vFont.Angle);
    
    RestoreTextColor;
    
    QPainter_drawText(Widget, 0, QtFontMetrics.ascent, s);
    
    RestorePenColor;
    
    Restore;
    
    {$ifdef VerboseQt}
    WriteLn(' Font metrics height: ', QtFontMetrics.height, ' Angle: ',
      Round(0.1 * vFont.Angle));
    {$endif}
  finally
    QtFontMetrics.Free;
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.DrawText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.DrawText(x, y, w, h, flags: Integer; s: PWideString);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.drawText x: ', X, ' Y: ', Y,' w: ',w,' h: ',h);
  {$endif}
  RestoreTextColor;

  QPainter_DrawText(Widget, x, y, w, h, Flags, s);

  RestorePenColor;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawLine
  Params:  None
  Returns: Nothing

  Draws a Text. Helper function for winapi.LineTo
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawLine(x1: Integer; y1: Integer; x2: Integer; y2: Integer);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.drawLine x1: ', X1, ' Y1: ', Y1,' x2: ',x2,' y2: ',y2);
  {$endif}
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

procedure TQtDeviceContext.fillRect(ARect: PRect; ABrush: QBrushH);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.fillRect() from PRect');
  {$endif}
  QPainter_fillRect(Widget, ARect, ABrush);
end;

procedure TQtDeviceContext.fillRect(x, y, w, h: Integer; ABrush: QBrushH);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.fillRect() x: ',x,' y: ',y,' w: ',w,' h: ',h);
  {$endif}
  QPainter_fillRect(Widget, x, y, w, h, ABrush);
end;

procedure TQtDeviceContext.fillRect(x, y, w, h: Integer);
begin
  fillRect(x, y, w, h, BackgroundBrush.Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawPoint
  Params:  x1,y1 : Integer
  Returns: Nothing

  Draws a point. Helper function of winapi. DrawFocusRect
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawPoint(x1: Integer; y1: Integer);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.drawPoint() x1: ',x1,' y1: ',y1);
  {$endif}
  QPainter_drawPoint(Widget, x1, y1);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setBrushOrigin
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setBrushOrigin(x, y: Integer);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.setBrushOrigin() x: ',x,' y: ',y);
  {$endif}
  QPainter_setBrushOrigin(Widget, x, y);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.brushOrigin
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.brushOrigin(retval: PPoint);
var
  QtPoint: TQtPoint;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.brushOrigin() ');
  {$endif}

  QPainter_brushOrigin(Widget, @QtPoint);
  retval^.x := QtPoint.x;
  retval^.y := QtPoint.y;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.font
  Params:  None
  Returns: The current font object of the DC
 ------------------------------------------------------------------------------}
function TQtDeviceContext.font: TQtFont;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.font()');
  {$endif}

  vFont.Widget := QPainter_font(Widget);
  if SelFont=nil then
    Result := vFont
  else
    Result := SelFont;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setFont
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setFont(f: TQtFont);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.setFont() ');
  {$endif}
  SelFont := F;
  if (f.Widget <> nil) and (Widget <> nil) {and (Parent <> nil)} then
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
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.brush() ');
  {$endif}
  vBrush.Widget := QPainter_brush(Widget);
  if SelBrush=nil then
    Result := vBrush
  else
    Result := SelBrush;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setBrush
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setBrush(ABrush: TQtBrush);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.setBrush() ');
  {$endif}
  SelBrush := ABrush;
  if (ABrush.Widget <> nil) and (Widget <> nil) then
    QPainter_setBrush(Widget, ABrush.Widget);
end;

function TQtDeviceContext.BackgroundBrush: TQtBrush;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.backgroundBrush() ');
  {$endif}
  vBackgroundBrush.Widget := QPainter_background(Widget);
  result := vBackGroundBrush;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.pen
  Params:  None
  Returns: The current pen object of the DC
 ------------------------------------------------------------------------------}
function TQtDeviceContext.pen: TQtPen;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.pen() ');
  {$endif}
  vPen.Widget := QPainter_pen(Widget);
  if SelPen=nil then
    Result := vPen
  else
    Result := SelPen;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setPen
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setPen(APen: TQtPen);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.setPen() ');
  {$endif}
  SelPen := APen;
  if (APen.Widget <> nil) and (Widget <> nil) then
    QPainter_setPen(Widget, APen.Widget);
end;

procedure TQColorToColorRef(const AColor: TQColor; out AColorRef: TColorRef);
begin
  AColorRef:=(( AColor.r shr 8) and $FF)
       or (AColor.g  and $ff00)
       or ((AColor.b  shl 8) and $ff0000);
end;

procedure ColorRefToTQColor(const AColorRef: TColorRef; var AColor:TQColor);
begin
  AColor.r:=(AColorRef and $ff);
  AColor.r:=AColor.r+(AColor.r shl 8);
  AColor.g:=(AColorRef and $ff00);
  AColor.g:=AColor.g+(AColor.g shr 8);
  AColor.b:=(AColorRef and $ff0000) shr 8;
  AColor.b:=AColor.b+(AColor.b shr 8);
end;

procedure DebugRegion(const msg: string; Rgn: QRegionH);
var
  R: TRect;
  ok: boolean;
begin
  Write(Msg);
  ok := QRegion_isEmpty(Rgn);
  QRegion_BoundingRect(Rgn, @R);
  WriteLn(' Empty=',Ok,' Rect=', dbgs(R));
end;

function Clipboard: TQtClipboard;
begin
  if FClipboard = nil then
    FClipboard := TQtClipboard.Create;
  Result := FClipboard;
end;

function TQtDeviceContext.SetBkColor(Color: TcolorRef): TColorRef;
var
  ABrush: QBrushH;
  NColor: TQColor;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.setBKColor() ');
  {$endif}
  result := CLR_INVALID;
  ABrush := BackgroundBrush.Widget;
  if ABrush<>nil then
  begin
    NColor := QBrush_Color(aBrush)^;
    TQColorToColorRef(NColor, Result);
    ColorRefToTQColor(ColorToRGB(Color), NColor);
    QBrush_setColor(ABrush, @NColor);
  end;
end;

function TQtDeviceContext.SetBkMode(BkMode: Integer): Integer;
var
  Mode: QtBGMode;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.setBKMode() ');
  {$endif}
  result := 0;
  if Widget <> nil then
  begin
    Mode := QPainter_BackgroundMode(Widget);
    if Mode = QtOpaqueMode then
      result := OPAQUE
    else
      result := TRANSPARENT;

    if BkMode = OPAQUE then
      Mode := QtOpaqueMode
    else
      Mode := QtTransparentMode;
    QPainter_SetBackgroundMode(Widget, Mode);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.getRegionType
  Params:  QRegionH
  Returns: Region type
 ------------------------------------------------------------------------------}
function TQtDeviceContext.getRegionType(ARegion: QRegionH): integer;
var
  R: TRect;
begin
  try
    if QRegion_isEmpty(ARegion) then
      Result := NULLREGION
    else
    begin
      QRegion_boundingRect(ARegion, @R);
      if QRegion_contains(ARegion, PRect(@R)) then
        Result := SIMPLEREGION
      else
        Result := COMPLEXREGION;
    end;
  except
    Result := ERROR;
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.region
  Params:  None
  Returns: The current clip region
 ------------------------------------------------------------------------------}
function TQtDeviceContext.region: TQtRegion;
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.region() ');
  {$endif}
  if vRegion.Widget=nil then
    vRegion.Widget := QRegion_Create();
    
  QPainter_clipRegion(Widget,  vRegion.Widget);
  Result := vRegion;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setRegion
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setRegion(ARegion: TQtRegion);
begin
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.setRegion() ');
  {$endif}
  if (ARegion.Widget <> nil) and (Widget <> nil) then
    QPainter_setClipRegion(Widget, ARegion.Widget);
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
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.drawImage() ');
  {$endif}
  LocalRect := targetRect^;
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
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.rotate() ');
  {$endif}
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
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.save() ');
  {$endif}
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
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.restore() ');
  {$endif}
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
  {$ifdef VerboseQt}
  Write('TQtDeviceContext.translate() ');
  {$endif}
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

class procedure TQtPixmap.fromImage(retval: QPixmapH; image: QImageH; flags: QtImageConversionFlags = QtAutoColor);
begin
  QPixmap_fromImage(retval, image, flags);
end;

{ TQtSystemTrayIcon }

constructor TQtSystemTrayIcon.Create(vIcon: QIconH);
begin
  inherited Create;

  handle := QSystemTrayIcon_create(vicon, nil);
end;

destructor TQtSystemTrayIcon.Destroy;
begin
  QSystemTrayIcon_destroy(handle);

  inherited Destroy;
end;

procedure TQtSystemTrayIcon.setContextMenu(menu: QMenuH);
begin
  QSystemTrayIcon_setContextMenu(handle, menu);
end;

procedure TQtSystemTrayIcon.setIcon(icon: QIconH);
begin
  QSystemTrayIcon_setIcon(handle, icon);
end;

procedure TQtSystemTrayIcon.setToolTip(tip: WideString);
begin
  QSystemTrayIcon_setToolTip(handle, @tip)
end;

procedure TQtSystemTrayIcon.show;
begin
  QSystemTrayIcon_show(handle);
end;

procedure TQtSystemTrayIcon.hide;
begin
  QSystemTrayIcon_hide(handle);
end;

{ TQtButtonGroup }

constructor TQtButtonGroup.Create(AParent: QObjectH);
begin
  inherited Create;

  Handle := QButtonGroup_create(AParent);
end;

destructor TQtButtonGroup.Destroy;
begin
  QButtonGroup_destroy(Handle);
  inherited Destroy;
end;

procedure TQtButtonGroup.AddButton(AButton: QAbstractButtonH); overload;
begin
  QButtonGroup_addButton(Handle, AButton);
end;

procedure TQtButtonGroup.AddButton(AButton: QAbstractButtonH; id: Integer); overload;
begin
  QButtonGroup_addButton(Handle, AButton, id);
end;

function TQtButtonGroup.ButtonFromId(id: Integer): QAbstractButtonH;
begin
  Result := QButtonGroup_button(Handle, id);
end;

procedure TQtButtonGroup.RemoveButton(AButton: QAbstractButtonH);
begin
  QButtonGroup_removeButton(Handle, AButton);
end;

procedure TQtButtonGroup.SetExclusive(AExclusive: Boolean);
begin
  QButtonGroup_setExclusive(Handle, AExclusive);
end;

function TQtButtonGroup.GetExclusive: Boolean;
begin
  Result := QButtonGroup_exclusive(Handle);
end;

procedure TQtButtonGroup.SignalButtonClicked(AButton: QAbstractButtonH); cdecl;
begin
  {todo}
end;

{ TQtClipboard }

constructor TQtClipboard.Create;
begin
  FOnClipBoardRequest := nil;
  FClipBoardFormats := TStringList.Create;
  FClipBoardFormats.Add('foo'); // 0 is reserved
  TheObject := QApplication_clipBoard;
  AttachEvents;
end;

destructor TQtClipboard.Destroy;
begin
  DetachEvents;
  FClipBoardFormats.Free;
  inherited Destroy;
end;

function TQtClipboard.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result := False;

  if QEvent_type(Event) = QEventClipboard then
  begin
    Result := True;

    QEvent_accept(Event);

    // Clipboard is changed, but we have no ability at moment to pass that info
    // to LCL since LCL has no support for that event
  end;
end;

function TQtClipboard.Clipboard: QClipboardH;
begin
  Result := QClipboardH(TheObject);
end;

function TQtClipboard.getMimeData(AMode: QClipboardMode): QMimeDataH;
begin
  Result := QClipboard_mimeData(Clipboard, AMode);
end;

procedure TQtClipboard.setMimeData(AMimeData: QMimeDataH; AMode: QClipboardMode);
begin
  QClipboard_setMimeData(Clipboard, AMimeData, AMode);
end;

procedure TQtClipboard.Clear(AMode: QClipboardMode);
begin
  QClipboard_clear(ClipBoard, AMode);
end;

function TQtClipboard.FormatToMimeType(AFormat: TClipboardFormat): String;
begin
  if FClipBoardFormats.Count > Integer(AFormat) then
    Result := FClipBoardFormats[AFormat]
  else
    Result := '';
end;

function TQtClipboard.RegisterFormat(AMimeType: String): TClipboardFormat;
var
  Index: Integer;
begin
  Index := FClipBoardFormats.IndexOf(AMimeType);
  if Index < 0 then
    Index := FClipBoardFormats.Add(AMimeType);
  Result := Index;
end;

function TQtClipboard.GetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean;
var
  QtMimeData: QMimeDataH;
  MimeType: WideString;
  Data: QByteArrayH;
  p: PAnsiChar;
  s: Integer;
begin
  Result := False;
  QtMimeData := getMimeData(ClipbBoardTypeToQtClipboard[ClipBoardType]);
  MimeType := FormatToMimeType(FormatID);
  Data := QByteArray_create();
  QMimeData_data(QtMimeData, Data, @MimeType);
  s := QByteArray_size(Data);
  p := QByteArray_data(Data);
  Stream.Write(p^, s);
  // what to do with p? FreeMem or nothing?
  QByteArray_destroy(Data);
  Result := True;
end;

function TQtClipboard.GetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;
var
  QtMimeData: QMimeDataH;
  QtList: QStringListH;
  i: Integer;
  Str: WideString;
begin
  Result := False;
  Count := 0;
  List := nil;

  QtMimeData := getMimeData(ClipbBoardTypeToQtClipboard[ClipBoardType]);

  QtList := QStringList_create;
  QMimeData_formats(QtMimeData, QtList);

  try
    Count := QStringList_size(QtList);
    GetMem(List, Count * SizeOf(TClipboardFormat));

    for i := 0 to Count - 1 do
    begin
      QStringList_at(QtList, @Str, i);
      Str := UTF8Encode(Str);
      List[i] := RegisterFormat(Str);
    end;

    Result := Count > 0;

  finally
    QStringList_destroy(QtList);
  end;
end;

function TQtClipboard.GetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent; FormatCount: integer;
  Formats: PClipboardFormat): boolean;

  procedure PutOnClipBoard;
  var
    MimeType: WideString;
    MimeData: QMimeDataH;
    Data: QByteArrayH;
    DataStream: TMemoryStream;
    I: Integer;
    P: PChar = 'test';
  begin
    MimeData := QMimeData_create();
    DataStream := TMemoryStream.Create;
    
    for I := 0 to FormatCount - 1 do
    begin
      DataStream.Size := 0;
      DataStream.Position := 0;

      MimeType := FormatToMimeType(Formats[I]);
      FOnClipBoardRequest(Formats[I], DataStream);

      Data := QByteArray_create(PAnsiChar(DataStream.Memory), DataStream.Size);
      QMimeData_setData(MimeData, @MimeType, Data);
      QByteArray_destroy(Data);
    end;

    DataStream.Free;
    setMimeData(MimeData, ClipbBoardTypeToQtClipboard[ClipBoardType]);
    // do not destroy MimeData!!!
  end;
begin
  Result := False;
  if (FormatCount = 0) or (OnRequestProc = nil) then
  begin
  { The LCL indicates it doesn't have the clipboard data anymore
    and the interface can't use the OnRequestProc anymore.}
    FOnClipBoardRequest := nil;
    Result := True;
  end else
  begin
  { clear OnClipBoardRequest to prevent destroying the LCL clipboard,
    when emptying the clipboard}
    FOnClipBoardRequest := nil;
    Clear(ClipbBoardTypeToQtClipboard[ClipBoardType]);
    FOnClipBoardRequest := OnRequestProc;
    PutOnClipBoard;
    Result := True;
  end;
end;

end.

