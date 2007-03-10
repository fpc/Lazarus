{ $Id$
                    -----------------------------------------
                    carbondef.pp  -  Type & Const definitions
                    -----------------------------------------

 @created(Wed Aug 26st WET 2005)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains type & const definitions needed in the Carbon <-> LCL interface

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


unit CarbonDef;

{$mode objfpc}{$H+}

interface

uses
  WSLCLClasses, LCLClasses,
  LCLType, LCLProc, Math, Classes, Graphics, Controls,
  FPCMacOSAll, CarbonUtils;

const
  DEFAULT_CFSTRING_ENCODING = kCFStringEncodingUTF8;

var
  LAZARUS_FOURCC: FourCharCode; // = 'Laz ';
  WIDGETINFO_FOURCC: FourCharCode; // = 'WInf';

type
  TCarbonControlContext = class;

  TCarbonWidgetType = (cwtWindowRef, cwtControlRef, cwtUnknown);

  (* Info needed by the API of a HWND (=Widget) *)

  { TCarbonWidgetInfo }
  TCarbonWidgetInfo = class
  private
    FProperties: TStringList;
    function GetProperty(AIndex: String): Pointer;
    procedure SetProperty(AIndex: String; const AValue: Pointer);
  public
    LCLObject: TObject;               // The object which created this widget
    CGContext: CGContextRef;          // Quartz 2D CGContext filled on paint event
    Context: TCarbonControlContext;   // Carbon control context
    Widget: Pointer;                  // Reference to the Carbon window or control
    WidgetType: TCarbonWidgetType;
    WSClass: TWSLCLComponentClass;    // The Widgetsetclass for this info
    DataOwner: Boolean;               // Set if the UserData should be freed when the info is freed
    UserData: Pointer;
  public
    constructor Create(AWidget: Pointer; AObject: TLCLComponent; TheType: TCarbonWidgetType);
    constructor CreateForControl(AWidget: Pointer; AObject: TLCLComponent);
    constructor CreateForWindow(AWidget: Pointer; AObject: TLCLComponent);
    
    destructor Destroy; override;
  public
    property Properties[AIndex: String]: Pointer read GetProperty write SetProperty;
  end;
  
type
  TCarbonWSEventHandlerProc = function (ANextHandler: EventHandlerCallRef;
    AEvent: EventRef;
    AInfo: TCarbonWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}

type
  TEventInt = packed record
    case integer of
    1: (Chars: array[0..4] of char);
    2: (Int: FPCMacOSAll.UInt32);
  end;
  
const
  LCLCarbonEventClass    = 'Laz ';
  LCLCarbonEventKindWake = 'Wake';
  LCLCarbonEventKindMain = 'Main';
  
  CursorToThemeCursor: array[crLow..crHigh] of ThemeCursor =
    ({crSizeSE      } kThemeResizeLeftCursor, {!!}
     {crSizeS       } kThemeResizeLeftCursor, {!!}
     {crSizeSW      } kThemeResizeRightCursor, {!!}
     {crSizeE       } kThemeResizeLeftCursor,
     {crSizeW       } kThemeResizeRightCursor,
     {crSizeNE      } kThemeResizeLeftCursor, {!!}
     {crSizeN       } kThemeResizeRightCursor, {!!}
     {crSizeNW      } kThemeResizeRightCursor, {!!}
     {crSizeAll     } kThemeResizeLeftRightCursor, {!!}
     {crHandPoint   } kThemePointingHandCursor,
     {crHelp        } kThemeContextualMenuArrowCursor, {!!}
     {crAppStart    } kThemeSpinningCursor, {!!}
     {crNo          } kThemeArrowCursor, {!!}
     {crSQLWait     } kThemeSpinningCursor, {!!}
     {crMultiDrag   } kThemeCopyArrowCursor, {!!}
     {crVSplit      } kThemeResizeLeftRightCursor,
     {crHSplit      } kThemeResizeLeftRightCursor, {!!}
     {crNoDrop      } kThemeArrowCursor, {!!}
     {crDrag        } kThemeCopyArrowCursor,
     {crHourGlass   } kThemeSpinningCursor,
     {crUpArrow     } kThemeArrowCursor, {!!}
     {crSizeWE      } kThemeResizeLeftRightCursor,
     {crSizeNWSE    } kThemeResizeLeftRightCursor, {!!}
     {crSizeNS      } kThemeResizeLeftRightCursor, {!!}
     {crSizeNESW    } kThemeResizeLeftRightCursor, {!!}
     {undefined     } kThemeArrowCursor, {!!}
     {crIBeam       } kThemeIBeamCursor,
     {crCross       } kThemeCrossCursor,
     {crArrow       } kThemeArrowCursor,
     {crNone        } kThemeArrowCursor,
     {crDefault     } kThemeArrowCursor);
  
type
  TCarbonDeviceContext = class;

  { TCarbonGDIObject }

  TCarbonGDIObject = class
  end;

  { TCarbonFont }

  TCarbonFont = class(TCarbonGDIObject)
  private
    FStyle: ATSUStyle;
  public
    constructor Create; // default system font
    constructor Create(ALogFont: TLogFont; AFaceName: String);
    destructor Destroy; override;
  public
    property Style: ATSUStyle read FStyle;
  end;

  { TCarbonColorObject }

  TCarbonColorObject = class(TCarbonGDIObject)
  private
    FR, FG, FB: Byte;
    FA: Boolean; // alpha: True - solid, False - clear
  public
    constructor Create(const AColor: TColor; ASolid: Boolean);
    procedure SetColor(const AColor: TColor; ASolid: Boolean);
    procedure GetRGBA(AROP2: Integer; out AR, AG, AB, AA: Single);
  end;

  { TCarbonBrush }

  TCarbonBrush = class(TCarbonColorObject)
  private
    FCGPattern: CGPatternRef; // TODO
  public
    constructor Create; // create default brush
    constructor Create(ALogBrush: TLogBrush);
    procedure Apply(ADC: TCarbonDeviceContext; UseROP2: Boolean = True);
  end;

const
  CarbonDashStyle: Array [0..1] of Single = (4, 2);
  CarbonDotStyle: Array [0..1] of Single = (2, 2);
  CarbonDashDotStyle: Array [0..3] of Single = (4, 2, 2, 2);
  CarbonDashDotDotStyle: Array [0..5] of Single = (4, 2, 2, 2, 2, 2);

type

  { TCarbonPen }

  TCarbonPen = class(TCarbonColorObject)
  private
    FWidth: Integer;
    FStyle: LongWord;
   public
    constructor Create; // create default pen
    constructor Create(ALogPen: TLogPen);
    procedure Apply(ADC: TCarbonDeviceContext; UseROP2: Boolean = True);
  end;
  
  { TCarbonBitmap }

  TCarbonBitmap = class(TCarbonGDIObject)
  private
    FData: Pointer;
    FDataOwner: Boolean;
    FDataSize: Integer;
    FCGDataProvider: CGDataProviderRef;
    FCGImage: CGImageRef;
    function GetBitsPerComponent: Integer;
    function GetBytesPerRow: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
  public
    constructor Create(AWidth, AHeight, ABitsPerPixel: Integer; AData: Pointer);
    destructor Destroy; override;
  public
    property BitsPerComponent: Integer read GetBitsPerComponent;
    property BytesPerRow: Integer read GetBytesPerRow;
    property CGImage: CGImageRef read FCGImage;
    property Data: Pointer read FData;
    property DataSize: Integer read FDataSize;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;
  
  { TCarbonDeviceContext }

  TCarbonDeviceContext = class
  private
    FCurrentFont: TCarbonFont;
    FCurrentBrush: TCarbonBrush;
    FCurrentPen: TCarbonPen;

    FBkColor: TColor;
    FBkMode: Integer;
    FBkBrush: TCarbonBrush;

    FTextColor: TColor;
    FTextPen: TCarbonPen;

    FROP2: Integer;
    FPenPos: TPoint;
    
    procedure SetBkColor(const AValue: TColor);
    procedure SetBkMode(const AValue: Integer);
    procedure SetCurrentBrush(const AValue: TCarbonBrush);
    procedure SetCurrentFont(const AValue: TCarbonFont);
    procedure SetCurrentPen(const AValue: TCarbonPen);
    procedure SetROP2(const AValue: Integer);
    procedure SetTextColor(const AValue: TColor);
  protected
    function GetCGContext: CGContextRef; virtual; abstract;
    function GetSize: TPoint; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Reset; virtual;
    procedure Update; virtual;
  public
    property CGContext: CGContextRef read GetCGContext;
    property Size: TPoint read GetSize;
    
    property CurrentFont: TCarbonFont read FCurrentFont write SetCurrentFont;
    property CurrentBrush: TCarbonBrush read FCurrentBrush write SetCurrentBrush;
    property CurrentPen: TCarbonPen read FCurrentPen write SetCurrentPen;
    
    property BkColor: TColor read FBkColor write SetBkColor;
    property BkMode: Integer read FBkMode write SetBkMode;
    property BkBrush: TCarbonBrush read FBkBrush;

    property TextColor: TColor read FTextColor write SetTextColor;
    property TextPen: TCarbonPen read FTextPen;

    property ROP2: Integer read FROP2 write SetROP2;
    property PenPos: TPoint read FPenPos write FPenPos;
  end;
  
  { TCarbonScreenContext }

  TCarbonScreenContext = class(TCarbonDeviceContext)
  protected
    function GetCGContext: CGContextRef; override; // TODO
    function GetSize: TPoint; override;
  public
    constructor Create;
  end;

  { TCarbonControlContext }

  TCarbonControlContext = class(TCarbonDeviceContext)
  private
    FInfo: TCarbonWidgetInfo;    // owner
  protected
    function GetCGContext: CGContextRef; override;
    function GetSize: TPoint; override;
  public
    constructor Create(AInfo: TCarbonWidgetInfo);
  end;
  
  { TCarbonBitmapContext }

  TCarbonBitmapContext = class(TCarbonDeviceContext)
  private
    FBitmap: TCarbonBitmap;
    FBitmapContext: CGContextRef;
    procedure SetBitmap(const AValue: TCarbonBitmap);
  protected
    function GetCGContext: CGContextRef; override;
    function GetSize: TPoint; override;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property Bitmap: TCarbonBitmap read FBitmap write SetBitmap;
  end;
  // TODO: TCarbonPrinterContext
  
var
  DefaultTextStyle: ATSUStyle; // default Carbon text style
  RGBColorSpace: CGColorSpaceRef; // global RGB color space
  
  StockSystemFont: TCarbonFont;
  StockNullBrush: TCarbonBrush;
  

implementation

uses
  CarbonProc;
  
{ TCarbonWidgetInfo }

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetInfo.GetProperty
  Params:  AIndex - Property name
  Returns: Property data, nil if the property is not listed

  Returns the specified property data or nil if the property is not listed
 ------------------------------------------------------------------------------}
function TCarbonWidgetInfo.GetProperty(AIndex: String): Pointer;
var
  I: Integer;
begin
  if FProperties <> nil then
  begin
    I := FProperties.IndexOf(AIndex);
    
    if I >= 0 then // the property is listed
    begin
      Result := FProperties.Objects[I];
      Exit;
    end;
  end;
  Result := nil;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetInfo.SetProperty
  Params:  AIndex - Property name
           AValue - Property data, nil means remove the property
  Returns: Nothing

  Sets the specified property data or removes the property
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetInfo.SetProperty(AIndex: String; const AValue: Pointer);
var
  I: Integer;
begin
  if FProperties = nil then
  begin
    if AValue = nil then Exit;
    // create string list for storing properties
    FProperties := TStringList.Create;
    FProperties.Sorted := True; // to enable binary searching
  end;
  
  I := FProperties.IndexOf(AIndex);
  if I >= 0 then // the property is listed -> update or remove if AValue = nil
  begin
    if AValue = nil then
    begin
      FProperties.Delete(I);
      if FProperties.Count = 0 then
      begin
        FProperties.Free; // free if the list is clear
        FProperties := nil;
      end;
    end
    else FProperties.Objects[I] := TObject(AValue);
  end
  else // the property is not listed -> add
  begin
    FProperties.AddObject(AIndex, TObject(AValue));
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetInfo.Create
  Params:  AWidget - Pointer to widget
           AObject - LCL object
           TheType - Type of Carbon widget (Control or Window)
  Returns: The Carbon widget info

  Creates basic info for the specified widget and LCL object
 ------------------------------------------------------------------------------}
constructor TCarbonWidgetInfo.Create(AWidget: Pointer; AObject: TLCLComponent;
  TheType: TCarbonWidgetType);
begin
  LCLObject := AObject;
  CGContext := nil;
  Widget := AWidget;
  WSClass := AObject.WidgetSetClass;
  WidgetType := TheType;
  UserData := nil;
  DataOwner := False;
  FProperties := nil;
  Context := TCarbonControlContext.Create(Self);
  
  case TheType of
    cwtControlRef: SetControlProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self);
    cwtWindowRef : SetWindowProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self);
  else 
    DebugLn('[TCarbonWidgetInfo.Create] ***WARNING! Unknown widget type***');
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetInfo.CreateForControl
  Params:  AWidget - Pointer to control widget
           AObject - LCL object
  Returns: The Carbon Control widget info

  Creates basic info for specified control widget and LCL object
 ------------------------------------------------------------------------------}
constructor TCarbonWidgetInfo.CreateForControl(AWidget: Pointer; AObject: TLCLComponent);
begin
  Create(AWidget, AObject, cwtControlRef);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetInfo.CreateForWindow
  Params:  AWidget - Pointer to window widget
           AObject - LCL object
  Returns: The Carbon window widget info

  Creates basic info for specified window widget and LCL object
 ------------------------------------------------------------------------------}
constructor TCarbonWidgetInfo.CreateForWindow(AWidget: Pointer; AObject: TLCLComponent);
begin
  Create(AWidget, AObject, cwtWindowRef);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetInfo.Destroy
  Returns: Nothing

  Frees the widget info
 ------------------------------------------------------------------------------}
destructor TCarbonWidgetInfo.Destroy;
begin
  case WidgetType of
    cwtControlRef: RemoveControlProperty(Widget, LAZARUS_FOURCC, WIDGETINFO_FOURCC);
    cwtWindowRef : RemoveWindowProperty(Widget, LAZARUS_FOURCC, WIDGETINFO_FOURCC);
  else Debugln('[TCarbonWidgetInfo.Destroy] ***WARNING! Unknown widget type***');
  end;

  if (UserData <> nil) and DataOwner then
  begin
    System.FreeMem(UserData);
    UserData := nil;
  end;
  
  Context.Free;
  FProperties.Free;

  inherited Destroy;
end;

{ TCarbonFont }

constructor TCarbonFont.Create;
begin
  FStyle := DefaultTextStyle;
end;

constructor TCarbonFont.Create(ALogFont: TLogFont; AFaceName: String);
var
  Attr: ATSUAttributeTag;
  M: ATSUTextMeasurement;
  B: Boolean;
  S: ByteCount;
  A: ATSUAttributeValuePtr;
  ID: ATSUFontID;
begin
  ATSUCreateStyle(FStyle);
  
  ID := FindCarbonFontID(AFaceName);
  
  if ID <> 0 then
  begin
    Attr := kATSUFontTag;
    A := @ID;
    S := SizeOf(ID);
    ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);
  end;
  
  if ALogFont.lfHeight <> 0 then
  begin
    Attr := kATSUSizeTag;
    M := Abs(ALogFont.lfHeight) shl 16;
    A := @M;
    S := SizeOf(M);
    ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);
  end;
  
  if ALogFont.lfEscapement <> 0 then
  begin
    Attr := kATSULineRotationTag;
    M := (ALogFont.lfEscapement shl 16) div 10;
    A := @M;
    S := SizeOf(M);
    ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);
  end;
  
  if ALogFont.lfWeight > FW_NORMAL then
  begin
    Attr := kATSUQDBoldfaceTag;
    B := True;
    A := @B;
    S := SizeOf(B);
    ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);
  end;
  
  if ALogFont.lfItalic > 0 then
  begin
    Attr := kATSUQDItalicTag;
    B := True;
    A := @B;
    S := SizeOf(B);
    ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);
  end;
  
  if ALogFont.lfUnderline > 0 then
  begin
    Attr := kATSUQDUnderlineTag;
    B := True;
    A := @B;
    S := SizeOf(B);
    ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);
  end;
  
  if ALogFont.lfStrikeOut > 0 then
  begin
    Attr := kATSUStyleStrikeThroughTag;
    B := True;
    A := @B;
    S := SizeOf(B);
    ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);
  end;
end;

destructor TCarbonFont.Destroy;
begin
  if FStyle <> DefaultTextStyle then ATSUDisposeStyle(FStyle);

  inherited;
end;

{ TCarbonDeviceContext }

procedure TCarbonDeviceContext.SetBkColor(const AValue: TColor);
begin
  if FBkColor <> AValue then
  begin
    FBkColor := AValue;
    FBkBrush.SetColor(ColorToRGB(AValue), BkMode = OPAQUE);
  end;
end;

procedure TCarbonDeviceContext.SetBkMode(const AValue: Integer);
begin
  if FBkMode <> AValue then
  begin
    FBkMode := AValue;
    FBkBrush.SetColor(ColorToRGB(BkColor), FBkMode = OPAQUE);
  end;
end;

procedure TCarbonDeviceContext.SetCurrentBrush(const AValue: TCarbonBrush);
begin
  if FCurrentBrush <> AValue then
  begin
    FCurrentBrush := AValue;
    FCurrentBrush.Apply(Self);
  end;
end;

procedure TCarbonDeviceContext.SetCurrentFont(const AValue: TCarbonFont);
begin
  if FCurrentFont <> AValue then
  begin
    FCurrentFont := AValue;
  end;
end;

procedure TCarbonDeviceContext.SetCurrentPen(const AValue: TCarbonPen);
begin
  if FCurrentPen <> AValue then
  begin
    FCurrentPen := AValue;
    FCurrentPen.Apply(Self);
  end;
end;

procedure TCarbonDeviceContext.SetROP2(const AValue: Integer);
begin
  if FROP2 <> AValue then
  begin
    FROP2 := AValue;
    CurrentPen.Apply(Self);
    CurrentBrush.Apply(Self);
  end;
end;

procedure TCarbonDeviceContext.SetTextColor(const AValue: TColor);
begin
  if FTextColor <> AValue then
  begin
    FTextColor := AValue;
    TextPen.SetColor(ColorToRGB(AValue), False);
  end;
end;

constructor TCarbonDeviceContext.Create;
begin
  FBkBrush := TCarbonBrush.Create;
  FTextPen := TCarbonPen.Create;
end;

destructor TCarbonDeviceContext.Destroy;
begin
  BkBrush.Free;
  TextPen.Free;

  inherited Destroy;
end;

procedure TCarbonDeviceContext.Reset;
begin
  CurrentFont := nil;

  PenPos.x := 0;
  PenPos.y := 0;

  // create brush for bk color and mode
  FBkColor := clNone;
  FBkMode := TRANSPARENT;

  // create pen for text color
  FTextColor := clNone;

  // set raster operation to copy
  FROP2 := R2_COPYPEN;

  // set initial pen and brush
  FCurrentPen := nil;
  FCurrentBrush := nil;
  
  Update;
end;

procedure TCarbonDeviceContext.Update;
begin
  if CGContext <> nil then
  begin
    {$IFDEF VerbosePaint}
    DebugLn('TCarbonDeviceContext.Update');
    {$ENDIF}
    // disable anti-aliasing
    CGContextSetShouldAntialias(CGContext, 0);
  end;
end;

{ TCarbonScreenContext }

function TCarbonScreenContext.GetCGContext: CGContextRef;
begin
  Result := nil;
end;

function TCarbonScreenContext.GetSize: TPoint;
begin
  Result.X := CGDisplayPixelsWide(CGMainDisplayID);
  Result.Y := CGDisplayPixelsHigh(CGMainDisplayID);
end;

constructor TCarbonScreenContext.Create;
begin
  inherited Create;
  
  Reset;
end;

{ TCarbonControlContext }

function TCarbonControlContext.GetCGContext: CGContextRef;
begin
  Result := FInfo.CGContext
end;

function TCarbonControlContext.GetSize: TPoint;
begin
  Result.X := (FInfo.LCLObject as TControl).ClientWidth;
  Result.Y := (FInfo.LCLObject as TControl).ClientHeight;
end;

constructor TCarbonControlContext.Create(AInfo: TCarbonWidgetInfo);
begin
  inherited Create;
  
  FInfo := AInfo;
  Reset;
end;

{ TCarbonBitmapContext }

procedure TCarbonBitmapContext.SetBitmap(const AValue: TCarbonBitmap);
begin
  if FBitmap <> AValue then
  begin
    FBitmap := AValue;
    
    if FBitmapContext <> nil then CGContextRelease(FBitmapContext);

    if FBitmap = nil then
    begin
      FBitmapContext := nil;
      Exit;
    end;
    
    // create CGBitmapContext
    FBitmapContext := CGBitmapContextCreate(FBitmap.Data, FBitmap.Width,
      FBitmap.Height, FBitmap.BitsPerComponent, FBitmap.BytesPerRow, RGBColorSpace,
      kCGImageAlphaNoneSkipLast);
    
    Reset;
  end;
end;

function TCarbonBitmapContext.GetCGContext: CGContextRef;
begin
  if FBitmap <> nil then
  begin
    Result := FBitmapContext;
  end
  else
    Result := nil;
end;

function TCarbonBitmapContext.GetSize: TPoint;
begin
  if FBitmap <> nil then
  begin
    Result.X := FBitmap.Width;
    Result.Y := FBitmap.Height;
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

constructor TCarbonBitmapContext.Create;
begin
  FBitmap := nil;
end;

destructor TCarbonBitmapContext.Destroy;
begin
  if FBitmapContext <> nil then CGContextRelease(FBitmapContext);
  
  inherited Destroy;
end;

{ TCarbonColorObject }

constructor TCarbonColorObject.Create(const AColor: TColor; ASolid: Boolean);
begin
  SetColor(AColor, ASolid);
end;

procedure TCarbonColorObject.SetColor(const AColor: TColor; ASolid: Boolean);
begin
  RedGreenBlue(ColorToRGB(AColor), FR, FG, FB);
  FA := ASolid;
end;

procedure TCarbonColorObject.GetRGBA(AROP2: Integer; out AR, AG, AB, AA: Single);
begin
  case AROP2 of
    R2_BLACK:
    begin
      AR := 0;
      AG := 0;
      AB := 0;
      AA := Byte(FA);
    end;
    R2_WHITE:
    begin
      AR := 1;
      AG := 1;
      AB := 1;
      AA := Byte(FA);
    end;
    R2_NOP:
    begin
      AR := 1;
      AG := 1;
      AB := 1;
      AA := 0;
    end;
    R2_NOT:
    begin
      AR := 1;
      AG := 1;
      AB := 1;
      AA := Byte(FA);
    end;
    R2_NOTCOPYPEN:
    begin
      AR := (255 - FR) / 255;
      AG := (255 - FG) / 255;
      AB := (255 - FB) / 255;
      AA := Byte(FA);
    end;
  else // copy
    begin
      AR := FR / 255;
      AG := FG / 255;
      AB := FB / 255;
      AA := Byte(FA);
    end;
  end;
end;

{ TCarbonBrush }

constructor TCarbonBrush.Create;
begin
  inherited Create(clWhite, False);
end;

constructor TCarbonBrush.Create(ALogBrush: TLogBrush);
begin
  case ALogBrush.lbStyle of
    BS_SOLID,
    BS_HATCHED..BS_MONOPATTERN:
      begin
        SetColor(ColorToRGB(ALogBrush.lbColor), True);
        // TODO: patterns
      end;
    else
      SetColor(ColorToRGB(ALogBrush.lbColor), False);
  end;
end;

procedure TCarbonBrush.Apply(ADC: TCarbonDeviceContext; UseROP2: Boolean);
var
  AR, AG, AB, AA: Single;
  AROP2: Integer;
begin
  if ADC = nil then Exit;
  
  if UseROP2 then AROP2 := ADC.ROP2
  else AROP2 := R2_COPYPEN;

  GetRGBA(AROP2, AR, AG, AB, AA);
  
  if AROP2 <> R2_NOT then
    CGContextSetBlendMode(ADC.CGContext, kCGBlendModeNormal)
  else
    CGContextSetBlendMode(ADC.CGContext, kCGBlendModeDifference);

  CGContextSetRGBFillColor(ADC.CGContext, AR, AG, AB, AA);
end;

{ TCarbonPen }

constructor TCarbonPen.Create;
begin
  inherited Create(clBlack, True);
  FStyle := PS_SOLID;
  FWidth := 1;
end;

constructor TCarbonPen.Create(ALogPen: TLogPen);
begin
  case ALogPen.lopnStyle of
    PS_SOLID..PS_DASHDOTDOT,
    PS_INSIDEFRAME:
      begin
        SetColor(ColorToRGB(ALogPen.lopnColor), True);
        FWidth := Max(1, ALogPen.lopnWidth.x);
      end;
    else
    begin
      SetColor(ColorToRGB(ALogPen.lopnColor), True);
      FWidth := 1;
    end;
  end;
  
  FStyle := ALogPen.lopnStyle;
end;

procedure TCarbonPen.Apply(ADC: TCarbonDeviceContext; UseROP2: Boolean);
var
  AR, AG, AB, AA: Single;
  AROP2: Integer;
begin
  if ADC = nil then Exit;

  if UseROP2 then AROP2 := ADC.ROP2
  else AROP2 := R2_COPYPEN;

  GetRGBA(AROP2, AR, AG, AB, AA);
  
  if AROP2 <> R2_NOT then
    CGContextSetBlendMode(ADC.CGContext, kCGBlendModeNormal)
  else
    CGContextSetBlendMode(ADC.CGContext, kCGBlendModeDifference);
    
  CGContextSetRGBStrokeColor(ADC.CGContext, AR, AG, AB, AA);
  CGContextSetLineWidth(ADC.CGContext, FWidth);
  
  case FStyle of
    PS_DASH: CGContextSetLineDash(ADC.CGContext, 0, @CarbonDashStyle[0],
      Length(CarbonDashStyle));
    PS_DOT: CGContextSetLineDash(ADC.CGContext, 0, @CarbonDotStyle[0],
      Length(CarbonDotStyle));
    PS_DASHDOT: CGContextSetLineDash(ADC.CGContext, 0, @CarbonDashDotStyle[0],
      Length(CarbonDashDotStyle));
    PS_DASHDOTDOT: CGContextSetLineDash(ADC.CGContext, 0, @CarbonDashDotDotStyle[0],
      Length(CarbonDashDotDotStyle));
  else
    CGContextSetLineDash(ADC.CGContext, 0, nil, 0);
  end;
end;

{ TCarbonBitmap }

function TCarbonBitmap.GetHeight: Integer;
begin
  Result := CGImageGetHeight(FCGImage);
end;

function TCarbonBitmap.GetBitsPerComponent: Integer;
begin
  Result := CGImageGetBitsPerComponent(FCGImage);
end;

function TCarbonBitmap.GetBytesPerRow: Integer;
begin
  Result := CGImageGetBytesPerRow(FCGImage);
end;

function TCarbonBitmap.GetWidth: Integer;
begin
  Result := CGImageGetWidth(FCGImage);
end;

constructor TCarbonBitmap.Create(AWidth, AHeight, ABitsPerPixel: Integer;
  AData: Pointer);
var
  DataRowSize: Integer;
begin
  if AWidth < 1 then AWidth := 1;
  if AHeight < 1 then AHeight := 1;
  
  // TODO: use AData if assigned, enable more pixel formats
  ABitsPerPixel := 32; // RGBA-32 format
  // 16-bytes align for best performance
  DataRowSize := ((AWidth * ABitsPerPixel shr 3) shl 4 + $F) shr 4;
  FDataSize := DataRowSize * AHeight;
  System.GetMem(FData, FDataSize);
  FDataOwner := True;
    
  FCGDataProvider := CGDataProviderCreateWithData(nil, FData, FDataSize, nil);
  FCGImage := CGImageCreate(AWidth, AHeight, ABitsPerPixel shr 2, ABitsPerPixel,
    DataRowSize, RGBColorSpace, kCGImageAlphaLast,
    FCGDataProvider, nil, 0, kCGRenderingIntentDefault);
end;

destructor TCarbonBitmap.Destroy;
begin
  CGImageRelease(FCGImage);
  CGDataProviderRelease(FCGDataProvider);
  if FDataOwner then System.FreeMem(FData);

  inherited Destroy;
end;

var
  LogBrush: TLogBrush;

initialization
  LAZARUS_FOURCC := MakeFourCC('Laz ');
  WIDGETINFO_FOURCC := MakeFourCC('WInf');
  
  ATSUCreateStyle(DefaultTextStyle);
  RGBColorSpace := CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
  
  StockSystemFont := TCarbonFont.Create;
  
  LogBrush.lbStyle := BS_NULL;
  LogBrush.lbColor := 0;
  StockNullBrush := TCarbonBrush.Create(LogBrush);
  
finalization
  StockNullBrush.Free;
  StockSystemFont.Free;
  
  ATSUDisposeStyle(DefaultTextStyle);
  CGColorSpaceRelease(RGBColorSpace);

end.
