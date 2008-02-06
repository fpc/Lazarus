{ $Id$
                  ------------------------------------------
                  carbongdiobjects.pp  -  Carbon GDI objects
                  ------------------------------------------

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
unit CarbonGDIObjects;

{$mode objfpc}{$H+}

interface

// debugging defines
{$I carbondebug.inc}

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math,
 // carbon bindings
  FPCMacOSAll,
 // LCL
  LCLProc, LCLType, Graphics, Controls, Forms,
 // LCL Carbon
 {$ifdef DebugBitmaps}
  CarbonDebug,
 {$endif}
  CarbonDef;

type

  { TCarbonGDIObject }

  TCarbonGDIObject = class
  private
    FSelCount: Integer;
    FGlobal: Boolean;
  public
    constructor Create(AGlobal: Boolean);
    
    procedure Select;
    procedure Unselect;
    
    property Global: Boolean read FGlobal;
    property SelCount: Integer read FSelCount;
  end;
  
  { TCarbonRegion }

  TCarbonRegion = class(TCarbonGDIObject)
  private
    FShape: HIShapeRef;
  public
    constructor Create;
    constructor Create(const X1, Y1, X2, Y2: Integer);
    constructor Create(Points: PPoint; NumPts: Integer; FillMode: Integer);
    destructor Destroy; override;
    
    procedure Apply(ADC: TCarbonContext);
    function GetBounds: TRect;
    function GetType: Integer;
    function ContainsPoint(const P: TPoint): Boolean;
  public
    property Shape: HIShapeRef read FShape;
  end;
  
  TCarbonFont = class;
  
  { TCarbonTextLayout }

  TCarbonTextLayout = class
  private
    FTextBefore: ATSUTextMeasurement;
    FTextAfter: ATSUTextMeasurement;
    FAscent: ATSUTextMeasurement;
    FDescent: ATSUTextMeasurement;
    FLineRotation: Fixed;
  public
    procedure Apply(ADC: TCarbonContext); virtual; abstract;
    function Draw(X, Y: Integer): Boolean; virtual; abstract;
    procedure Release; virtual;

    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetDrawBounds(X, Y: Integer): CGRect;

    property TextBefore: ATSUTextMeasurement read FTextBefore;
    property TextAfter: ATSUTextMeasurement read FTextAfter;
    property Ascent: ATSUTextMeasurement read FAscent;
    property Descent: ATSUTextMeasurement read FDescent;
  end;
  
  { TCarbonTextLayoutBuffer }

  TCarbonTextLayoutBuffer = class(TCarbonTextLayout)
  private
    FLayout: ATSUTextLayout;
    FTextBuffer: WideString;
    FDC: TCarbonContext;
  public
    constructor Create(const Text: String; Font: TCarbonFont; TextFractional: Boolean);
    procedure Apply(ADC: TCarbonContext); override;
    function Draw(X, Y: Integer): Boolean; override;
    procedure Release; override;

    property Layout: ATSUTextLayout read FLayout;
    property TextBuffer: WideString read FTextBuffer;
  end;
  
  { TCarbonTextLayoutArray }

  TCarbonTextLayoutArray = class(TCarbonTextLayout)
  private
    FText: String;
    FFont: TCarbonFont;
  public
    constructor Create(const Text: String; Font: TCarbonFont);
    procedure Apply(ADC: TCarbonContext); override;
    function Draw(X, Y: Integer): Boolean; override;
  end;

  { TCarbonFont }

  TCarbonFont = class(TCarbonGDIObject)
  private
    FStyle: ATSUStyle;
    FLineRotation: Fixed;
    FCachedLayouts: Array of TCarbonTextLayoutBuffer;
  public
    constructor Create(AGlobal: Boolean); // default system font
    constructor Create(ALogFont: TLogFont; const AFaceName: String);
    function CreateStyle(ALogFont: TLogFont; const AFaceName: String): ATSUStyle;
    destructor Destroy; override;
    procedure SetColor(AColor: TColor);
    
    function CreateTextLayout(const Text: String; TextFractional: Boolean): TCarbonTextLayout;
  public
    property LineRotation: Fixed read FLineRotation;
    property Style: ATSUStyle read FStyle;
  end;

  { TCarbonColorObject }

  TCarbonColorObject = class(TCarbonGDIObject)
  private
    FR, FG, FB: Byte;
    FA: Boolean; // alpha: True - solid, False - clear
  public
    constructor Create(const AColor: TColor; ASolid, AGlobal: Boolean);
    procedure SetColor(const AColor: TColor; ASolid: Boolean);
    procedure GetRGBA(AROP2: Integer; out AR, AG, AB, AA: Single);
    function CreateCGColor: CGColorRef;
    
    property Solid: Boolean read FA;
  end;

  { TCarbonBrush }

  TCarbonBrush = class(TCarbonColorObject)
  private
    FCGPattern: CGPatternRef; // TODO
  public
    constructor Create(AGlobal: Boolean); // create default brush
    constructor Create(ALogBrush: TLogBrush);
    procedure Apply(ADC: TCarbonContext; UseROP2: Boolean = True);
  end;

const
  // Paul Ishenin:
  // pen shapes are compared with windows shapes and now a bit to bit equal
  // please dont remove multiplier, maybe we will change it later
  osx_pen_multiplier = 3;
  CarbonDashStyle: Array [0..1] of Single = (6*osx_pen_multiplier, 2*osx_pen_multiplier);
  CarbonDotStyle: Array [0..1] of Single = (1*osx_pen_multiplier, 1*osx_pen_multiplier);
  CarbonDashDotStyle: Array [0..3] of Single = (3*osx_pen_multiplier, 2*osx_pen_multiplier, 1*osx_pen_multiplier, 2*osx_pen_multiplier);
  CarbonDashDotDotStyle: Array [0..5] of Single = (3*osx_pen_multiplier, 1*osx_pen_multiplier, 1*osx_pen_multiplier, 1*osx_pen_multiplier, 1*osx_pen_multiplier, 1*osx_pen_multiplier);

type

  { TCarbonPen }

  TCarbonPen = class(TCarbonColorObject)
  private
    FWidth: Integer;
    FStyle: LongWord;
   public
    constructor Create(AGlobal: Boolean); // create default pen
    constructor Create(ALogPen: TLogPen);
    procedure Apply(ADC: TCarbonContext; UseROP2: Boolean = True);
  end;

  { TCarbonBitmap }
  
  TCarbonBitmapAlignment = (
    cbaByte,  // each line starts at byte boundary.
    cbaWord,  // each line starts at word (16bit) boundary
    cbaDWord, // each line starts at double word (32bit) boundary
    cbaQWord, // each line starts at quad word (64bit) boundary
    cbaDQWord // each line starts at double quad word (128bit) boundary
  );
  
  TCarbonBitmapType = (
    cbtMono,  // mask or mono bitmap
    cbtGray,  // grayscale bitmap
    cbtRGB,   // color bitmap 8-8-8 R-G-B
    cbtARGB,  // color bitmap with alpha channel 8-8-8-8 A-R-G-B
    cbtBGR,   // color bitmap 8-8-8 B-G-R (windows compatible)
    cbtBGRA   // color bitmap with alpha channel 8-8-8-8 B-G-R-A (windows compatible)
  );
const
  cbtMask = cbtMono;
    

type
  TCarbonBitmap = class(TCarbonGDIObject)
  private
    FData: Pointer;
    FAlignment: TCarbonBitmapAlignment;
    FFreeData: Boolean;
    FDataSize: Integer;
    FBytesPerRow: Integer;
    FDepth: Byte;
    FBitsPerPixel: Byte;
    FWidth: Integer;
    FHeight: Integer;
    FType: TCarbonBitmapType;
    FCGImage: CGImageRef;
    function GetBitsPerComponent: Integer;
    function GetColorSpace: CGColorSpaceRef;
    function GetInfo: CGBitmapInfo;
  public
    constructor Create(AWidth, AHeight, ADepth, ABitsPerPixel: Integer;
      AAlignment: TCarbonBitmapAlignment; AType: TCarbonBitmapType;
      AData: Pointer; ACopyData: Boolean = True);
    constructor Create(ABitmap: TCarbonBitmap);
    destructor Destroy; override;
    procedure Update;
    function CreateSubImage(const ARect: TRect): CGImageRef;
    function CreateMaskedImage(AMask: TCarbonBitmap): CGImageRef;
    function CreateMaskedImage(AMask: TCarbonBitmap; const ARect: TRect): CGImageRef;
  public
    property BitsPerComponent: Integer read GetBitsPerComponent;
    property BytesPerRow: Integer read FBytesPerRow;
    property CGImage: CGImageRef read FCGImage;
    property ColorSpace: CGColorSpaceRef read GetColorSpace;
    property Data: Pointer read FData;
    property DataSize: Integer read FDataSize;
    property Depth: Byte read FDepth;
    property Info: CGBitmapInfo read GetInfo;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

const
  kThemeUndefCursor = ThemeCursor(-1); // undefined mac theme cursor
  
  CursorToThemeCursor: array[crLow..crHigh] of ThemeCursor =
    ({crSizeSE      } kThemeResizeRightCursor, {!!}
     {crSizeS       } kThemeResizeDownCursor,
     {crSizeSW      } kThemeResizeLeftCursor, {!!}
     {crSizeE       } kThemeResizeRightCursor,
     {crSizeW       } kThemeResizeLeftCursor,
     {crSizeNE      } kThemeResizeRightCursor, {!!}
     {crSizeN       } kThemeResizeUpCursor,
     {crSizeNW      } kThemeResizeLeftCursor, {!!}
     {crSizeAll     } kThemeUndefCursor,           // will be loaded from resource
     {crHandPoint   } kThemePointingHandCursor,
     {crHelp        } kThemeUndefCursor,           // will be loaded from resource
     {crAppStart    } kThemeSpinningCursor,
     {crNo          } kThemeUndefCursor,
     {crSQLWait     } kThemeUndefCursor,           // will be loaded from resource
     {crMultiDrag   } kThemeUndefCursor,           // will be loaded from resource
     {crVSplit      } kThemeResizeUpDownCursor,
     {crHSplit      } kThemeResizeLeftRightCursor,
     {crNoDrop      } kThemeNotAllowedCursor, 
     {crDrag        } kThemeCopyArrowCursor,
     {crHourGlass   } kThemeSpinningCursor,
     {crUpArrow     } kThemeUndefCursor,           // will be loaded from resource
     {crSizeWE      } kThemeResizeLeftRightCursor,
     {crSizeNWSE    } kThemeResizeLeftRightCursor, {!!}
     {crSizeNS      } kThemeResizeUpDownCursor, {!!}
     {crSizeNESW    } kThemeResizeLeftRightCursor, {!!}
     {undefined     } kThemeArrowCursor, {!!}
     {crIBeam       } kThemeIBeamCursor,
     {crCross       } kThemeCrossCursor,
     {crArrow       } kThemeArrowCursor,
     {crNone        } kThemeUndefCursor,
     {crDefault     } kThemeArrowCursor);

type
  TCarbonCursorType =
  (
    cctUnknown,    // undefined
    cctQDHardware, // QuickDraw hardware cursor
    cctQDColor,    // QuickDraw Color cursor
    cctTheme,      // theme cursor
    cctAnimated,   // animated theme cursor
    cctWait        // special wait cursor
  );
  { TCarbonCursor }
  
  TCarbonCursor = class(TCarbonGDIObject)
  private
    FCursorType: TCarbonCursorType;
    FDefault: Boolean;
    FThemeCursor: ThemeCursor;
    // animation
    FAnimationStep: Integer;
    FTaskID: MPTaskID;
    // color cursors
    FQDColorCursorHandle: CCrsrHandle;
    FQDHardwareCursorName: String;
    FPixmapHandle: PixmapHandle;
    procedure CreateThread;
    procedure DestroyThread;
  protected
    procedure CreateHardwareCursor(ABitmap: TCarbonBitmap; AHotSpot: Point);
    procedure CreateColorCursor(ABitmap: TCarbonBitmap; AHotSpot: Point);
  public
    constructor Create;
    constructor CreateFromInfo(AInfo: PIconInfo);
    constructor CreateThemed(AThemeCursor: ThemeCursor; ADefault: Boolean = False);
    destructor Destroy; override;

    procedure Install;
    procedure UnInstall;
    function StepAnimation: Boolean;
    class function HardwareCursorsSupported: Boolean;
  public
    property CursorType: TCarbonCursorType read FCursorType;
    property Default: Boolean read FDefault;
  end;
  
function CheckGDIObject(const GDIObject: HGDIOBJ; const AMethodName: String; AParamName: String = ''): Boolean;
function CheckBitmap(const Bitmap: HBITMAP; const AMethodName: String; AParamName: String = ''): Boolean;
function CheckCursor(const Cursor: HCURSOR; const AMethodName: String; AParamName: String = ''): Boolean;


var
  StockSystemFont: TCarbonFont;
  StockNullBrush: TCarbonBrush;
  WhiteBrush: TCarbonBrush;
  BlackPen: TCarbonPen;
  
  DefaultFont: TCarbonFont;
  DefaultBrush: TCarbonBrush;
  DefaultPen: TCarbonPen;

  DefaultBitmap: TCarbonBitmap; // 1 x 1 bitmap for default context
  
implementation

uses
  CarbonProc, CarbonCanvas, CarbonDbgConsts;
  
const
  BITMAPINFOMAP: array[TCarbonBitmapType] of CGBitmapInfo = (
    {cbtMask} kCGImageAlphaNone,
    {cbtGray} kCGImageAlphaNone,
    {cbtRGB}  kCGImageAlphaNone,
    {cbtARGB} kCGImageAlphaFirst,
    {cbtBGR}  kCGImageAlphaNone  or kCGBitmapByteOrder32Little,
    {cbtBGRA} kCGImageAlphaFirst or kCGBitmapByteOrder32Little
  );

  
{------------------------------------------------------------------------------
  Name:    CheckGDIObject
  Params:  GDIObject   - Handle to a GDI Object (TCarbonFont, ...)
           AMethodName - Method name
           AParamName  - Param name
  Returns: If the GDIObject is valid

  Remark: All handles for GDI objects must be pascal objects so we can
 distinguish between them
 ------------------------------------------------------------------------------}
function CheckGDIObject(const GDIObject: HGDIOBJ; const AMethodName: String;
  AParamName: String): Boolean;
begin
  if TObject(GDIObject) is TCarbonGDIObject then Result := True
  else
  begin
    Result := False;
    
    if Pos('.', AMethodName) = 0 then
      DebugLn(SCarbonWSPrefix + AMethodName + ' Error - invalid GDIObject ' +
        AParamName + ' = ' + DbgS(GDIObject) + '!')
    else
      DebugLn(AMethodName + ' Error - invalid GDIObject ' + AParamName + ' = ' +
        DbgS(GDIObject) + '!');
  end;
end;

{------------------------------------------------------------------------------
  Name:    CheckBitmap
  Params:  Bitmap      - Handle to a bitmap (TCarbonBitmap)
           AMethodName - Method name
           AParamName  - Param name
  Returns: If the bitmap is valid
 ------------------------------------------------------------------------------}
function CheckBitmap(const Bitmap: HBITMAP; const AMethodName: String;
  AParamName: String): Boolean;
begin
  if TObject(Bitmap) is TCarbonBitmap then Result := True
  else
  begin
    Result := False;
    
    if Pos('.', AMethodName) = 0 then
      DebugLn(SCarbonWSPrefix + AMethodName + ' Error - invalid bitmap ' +
        AParamName + ' = ' + DbgS(Bitmap) + '!')
    else
      DebugLn(AMethodName + ' Error - invalid bitmap ' + AParamName + ' = ' +
        DbgS(Bitmap) + '!');
  end;
end;

{------------------------------------------------------------------------------
  Name:    CheckCursor
  Params:  Cursor      - Handle to a cursor (TCarbonCursor)
           AMethodName - Method name
           AParamName  - Param name
  Returns: If the cursor is valid
 ------------------------------------------------------------------------------}
function CheckCursor(const Cursor: HCURSOR; const AMethodName: String;
  AParamName: String): Boolean;
begin
  if TObject(Cursor) is TCarbonCursor then Result := True
  else
  begin
    Result := False;

    if Pos('.', AMethodName) = 0 then
      DebugLn(SCarbonWSPrefix + AMethodName + ' Error - invalid cursor ' +
        AParamName + ' = ' + DbgS(Cursor) + '!')
    else
      DebugLn(AMethodName + ' Error - invalid cursor ' + AParamName + ' = ' +
        DbgS(Cursor) + '!');
  end;
end;

type
  THardwareCursorsAvailability =
  (
    hcaUndef,
    hcaAvailable,
    hcaUnavailable
  );

const
// missed error codes
  kQDNoColorHWCursorSupport = -3951;
  kQDCursorAlreadyRegistered = -3952;
  kQDCursorNotRegistered = -3953;
  kQDCorruptPICTDataErr = -3954;
  
  kThemeCursorAnimationDelay = 70;
  LazarusCursorInfix = '_lazarus_';

var
  MHardwareCursorsSupported: THardwareCursorsAvailability = hcaUndef;

{------------------------------------------------------------------------------
  Name: AnimationCursorHandler
  Handles cursor animation steps
 ------------------------------------------------------------------------------}
function AnimationCursorHandler(parameter: UnivPtr): OSStatus;
  {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  Result := noErr;
  while True do
  begin
    if TCarbonCursor(parameter).StepAnimation then
      Sleep(kThemeCursorAnimationDelay) else
      break;
  end;
end;

{ TCarbonGDIObject }

{------------------------------------------------------------------------------
  Method:  TCarbonGDIObject.Create
  Params:  AGlobal - Global

  Creates custom GDI object
 ------------------------------------------------------------------------------}
constructor TCarbonGDIObject.Create(AGlobal: Boolean);
begin
  FSelCount := 0;
  FGlobal := AGlobal;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonGDIObject.Select

  Selects custom GDI object
 ------------------------------------------------------------------------------}
procedure TCarbonGDIObject.Select;
begin
  if FGlobal then Exit;
  Inc(FSelCount);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonGDIObject.Unselect

  Unselects custom GDI object
 ------------------------------------------------------------------------------}
procedure TCarbonGDIObject.Unselect;
begin
  if FGlobal then Exit;
  if FSelCount > 0 then
    Dec(FSelCount)
  else
  begin
    DebugLn('TCarbonGDIObject.Unselect Error - ', DbgSName(Self), ' SelCount = ',
      DbgS(FSelCount), '!');
  end;
end;

{ TCarbonRegion }

{------------------------------------------------------------------------------
  Method:  TCarbonRegion.Create

  Creates a new empty Carbon region
 ------------------------------------------------------------------------------}
constructor TCarbonRegion.Create;
begin
  inherited Create(False);
  
  FShape := HIShapeCreateEmpty;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonRegion.Create
  Params:  X1, Y1, X2, Y2 - Region bounding rectangle

  Creates a new rectangular Carbon region
 ------------------------------------------------------------------------------}
constructor TCarbonRegion.Create(const X1, Y1, X2, Y2: Integer);
begin
  inherited Create(False);
  
  FShape := HIShapeCreateWithRect(GetCGRectSorted(X1, Y1, X2, Y2));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonRegion.Create
  Params:  Points   - Pointer to array of polygon points
           NumPts   - Number of points passed
           FillMode - Filling mode

  Creates a new polygonal Carbon region from the specified points
 ------------------------------------------------------------------------------}
constructor TCarbonRegion.Create(Points: PPoint; NumPts: Integer;
  FillMode: Integer);
var
  Bounds: TRect;
  Context: CGContextRef;
  W, H: Integer;
  Data: Pointer;
  PData: PByte;
  P: PPoint;
  I: Integer;
  X, Y, SX: Integer;
  LC, C: Byte;
  //Line: String;
  
  function GetPolygonBounds: TRect;
  var
    I: Integer;
  begin
    P := Points;
    Result := Classes.Rect(P^.X, P^.Y, P^.X, P^.Y);
    for I := 1 to NumPts - 1 do
    begin
      Inc(P);
      if P^.X < Result.Left then Result.Left := P^.X;
      if P^.X > Result.Right then Result.Right := P^.X;
      if P^.Y < Result.Top then Result.Top := P^.Y;
      if P^.Y > Result.Bottom then Result.Bottom := P^.Y;
    end;
  end;
  
  procedure AddPart(X1, X2, Y: Integer);
  var
    R: HIShapeRef;
  begin
    //DebugLn('AddPart:' + DbgS(X1) + ' - ' + DbgS(X2) + ', ' + DbgS(Y));
        
    R := HIShapeCreateWithRect(GetCGRect(X1, Y, X2, Y + 1));
    OSError(HIShapeUnion(FShape, R, FShape),
      Self, 'Create polygonal', 'HIShapeUnion');
    CFRelease(R);
  end;
  
begin
  inherited Create(False);
  
(*
  The passed polygon is drawed into grayscale context, the region is constructed
  per rows from rectangles of drawed polygon parts.
  *)
  
  FShape := HIShapeCreateMutable;
  
  if (NumPts <= 2) or (Points = nil) then Exit;
  Bounds := GetPolygonBounds;
  DebugLn('TCarbonRegion.Create Bounds:' + DbgS(Bounds));
  W := Bounds.Right - Bounds.Left + 2;
  H := Bounds.Bottom - Bounds.Top + 2;
  
  if (W <= 0) or (H <= 0) then Exit;
  
  System.GetMem(Data, W * H);
  System.FillChar(Data^, W * H, 0); // clear bitmap context data to black
  try
    Context := CGBitmapContextCreate(Data, W, H, 8, W, GrayColorSpace,
      kCGImageAlphaNone);
    try
      CGContextSetShouldAntialias(Context, 0); // disable anti-aliasing
      CGContextSetGrayFillColor(Context, 1.0, 1.0); // draw white polygon
      
      P := Points;
      CGContextBeginPath(Context);
      CGContextMoveToPoint(Context, P^.X, P^.Y);

      for I := 1 to NumPts - 1 do
      begin
        Inc(P);
        CGContextAddLineToPoint(Context, P^.X, P^.Y);
      end;
      
      CGContextClosePath(Context);
      
      if FillMode = ALTERNATE then
        CGContextEOFillPath(Context)
      else
        CGContextFillPath(Context);

      //SetLength(Line, W);

      PData := Data;
      for Y := 0 to Pred(H) do
      begin
        LC := 0; // edge is black
        for X := 0 to Pred(W) do
        begin
          C := PData^;
          //Line[X + 1] := Chr(Ord('0') + C div 255);
          
          if (C = $FF) and (LC = 0) then
            SX := X; // start of painted row part
          if (C = 0) and (LC = $FF) then
            // end of painted row part (SX, X)
            AddPart(SX, X,  Pred(H) - Y);
          
          LC := C;
          Inc(PData);
        end;
        //DebugLn(DbgS(Pred(H) - Y) + ':' + Line);
      end;

    finally
      CGContextRelease(Context);
    end;
  finally
    System.FreeMem(Data);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonRegion.Destroy

  Destroys Carbon region
 ------------------------------------------------------------------------------}
destructor TCarbonRegion.Destroy;
begin
  CFRelease(FShape);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonRegion.Apply
  Params:  ADC - Context to apply to

  Applies region to the specified context
  Note: Clipping region is only reducing
 ------------------------------------------------------------------------------}
procedure TCarbonRegion.Apply(ADC: TCarbonContext);
begin
  if ADC = nil then Exit;
  if ADC.CGContext = nil then Exit;
  
  if OSError(HIShapeReplacePathInCGContext(FShape, ADC.CGContext),
    Self, 'Apply', 'HIShapeReplacePathInCGContext') then Exit;
    
  CGContextClip(ADC.CGContext);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonRegion.GetBounds
  Returns: The bounding box of Carbon region
 ------------------------------------------------------------------------------}
function TCarbonRegion.GetBounds: TRect;
var
  R: HIRect;
begin
  if HIShapeGetBounds(FShape, R) = nil then
  begin
    DebugLn('TCarbonRegion.GetBounds Error!');
    Exit;
  end;
  
  Result := CGRectToRect(R);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonRegion.GetType
  Returns: The type of Carbon region
 ------------------------------------------------------------------------------}
function TCarbonRegion.GetType: Integer;
begin
  Result := ERROR;
  if HIShapeIsEmpty(FShape) then Result := NULLREGION
  else
    if HIShapeIsRectangular(FShape) then Result := SIMPLEREGION
    else Result := COMPLEXREGION;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonRegion.ContainsPoint
  Params:  P - Point
  Returns: If the specified point lies in Carbon region
 ------------------------------------------------------------------------------}
function TCarbonRegion.ContainsPoint(const P: TPoint): Boolean;
begin
  Result := HIShapeContainsPoint(FShape, PointToHIPoint(P));
end;

{ TCarbonTextLayout }

procedure TCarbonTextLayout.Release;
begin
  Free;
end;

function TCarbonTextLayout.GetHeight: Integer;
begin
  Result := RoundFixed(Descent + Ascent);
end;

function TCarbonTextLayout.GetWidth: Integer;
begin
  Result := RoundFixed(TextAfter - TextBefore);
end;

function TCarbonTextLayout.GetDrawBounds(X, Y: Integer): CGRect;
begin
  Result := GetCGRectSorted(X - RoundFixed(FTextBefore),
    -Y, X + RoundFixed(FTextAfter), -Y - RoundFixed(FAscent + FDescent));
end;

{ TCarbonTextLayoutBuffer }

{------------------------------------------------------------------------------
  Method:  TCarbonTextLayoutBuffer.Create
  Params:  Text           - UTF-8 text
           Font           - Text font
           TextFractional

  Creates new Carbon text layout with buffer
 ------------------------------------------------------------------------------}
constructor TCarbonTextLayoutBuffer.Create(const Text: String; Font: TCarbonFont; TextFractional: Boolean);
var
  TextStyle: ATSUStyle;
  TextLength: LongWord;
  Tag: ATSUAttributeTag;
  DataSize: ByteCount;
  Options: ATSLineLayoutOptions;
  PValue: ATSUAttributeValuePtr;
begin
  // keep copy of text
  FTextBuffer := UTF8ToUTF16(Text);
  
  TextStyle := Font.Style;

  // create text layout
  TextLength := kATSUToTextEnd;
  if OSError(ATSUCreateTextLayoutWithTextPtr(ConstUniCharArrayPtr(@FTextBuffer[1]),
      kATSUFromTextBeginning, kATSUToTextEnd, Length(FTextBuffer), 1, @TextLength,
      @TextStyle, FLayout), Self, SCreate, 'ATSUCreateTextLayoutWithTextPtr') then Exit;
      
  // set layout line orientation
  Tag := kATSULineRotationTag;
  DataSize := SizeOf(Fixed);

  FLineRotation := Font.LineRotation;
  PValue := @(FLineRotation);
  if OSError(ATSUSetLayoutControls(FLayout, 1, @Tag, @DataSize, @PValue),
    Self, SCreate, 'ATSUSetLayoutControls', 'LineRotation') then Exit;
    
  if not TextFractional then
  begin
    // disable fractional positions of glyphs in layout
    Tag := kATSULineLayoutOptionsTag;
    DataSize := SizeOf(ATSLineLayoutOptions);

    Options := kATSLineFractDisable or kATSLineDisableAutoAdjustDisplayPos or
      kATSLineDisableAllLayoutOperations or kATSLineUseDeviceMetrics;
    PValue := @Options;
    if OSError(ATSUSetLayoutControls(FLayout, 1, @Tag, @DataSize, @PValue),
      Self, SCreate, 'ATSUSetLayoutControls', 'LineLayoutOptions') then Exit;
  end;
  
  FDC := nil;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTextLayoutBuffer.Apply
  Params:  ADC     - Context to apply to

  Applies text layout to the specified context
 ------------------------------------------------------------------------------}
procedure TCarbonTextLayoutBuffer.Apply(ADC: TCarbonContext);
var
  Tag: ATSUAttributeTag;
  DataSize: ByteCount;
  PValue: ATSUAttributeValuePtr;
begin
  if FDC = ADC then Exit;
  FDC := ADC;

  // set layout context
  Tag := kATSUCGContextTag;
  DataSize := SizeOf(CGContextRef);

  PValue := @(ADC.CGContext);
  if OSError(ATSUSetLayoutControls(FLayout, 1, @Tag, @DataSize, @PValue),
    Self, 'Apply', 'ATSUSetLayoutControls', 'CGContext') then Exit;
    
  // get text ascent
  if OSError(
    ATSUGetUnjustifiedBounds(FLayout, kATSUFromTextBeginning, kATSUToTextEnd,
      FTextBefore, FTextAfter, FAscent, FDescent),
    Self, 'Apply', SGetUnjustifiedBounds) then Exit;
end;

function TCarbonTextLayoutBuffer.Draw(X, Y: Integer): Boolean;
var
  MX, MY: ATSUTextMeasurement;
  A: Single;
begin
  Result := False;
  
  if FLineRotation <> 0 then
  begin
    A := FLineRotation * (PI / ($10000 * 180));
    MX := Round(Ascent * Sin(A));
    MY := Round(Ascent - Ascent * Cos(A));
  end
  else
  begin
    MX := 0;
    MY := 0;
  end;

  if OSError(ATSUDrawText(FLayout, kATSUFromTextBeginning, kATSUToTextEnd,
      X shl 16 - FTextBefore + MX, -(Y shl 16) - FAscent + MY),
    Self, 'Draw', 'ATSUDrawText') then Exit;
    
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTextLayoutBuffer.Release

  Releases text layout
 ------------------------------------------------------------------------------}
procedure TCarbonTextLayoutBuffer.Release;
begin
  if FLayout <> nil then
    OSError(ATSUDisposeTextLayout(FLayout), Self, 'Release', 'ATSUDisposeTextLayout');

  inherited;
end;

{ TCarbonTextLayoutArray }

{------------------------------------------------------------------------------
  Method:  TCarbonTextLayoutArray.Create
  Params:  Text           - UTF-8 text
           Font           - Text font

  Creates new Carbon text layout array
 ------------------------------------------------------------------------------}
constructor TCarbonTextLayoutArray.Create(const Text: String; Font: TCarbonFont);
begin
  FText := Text;
  FFont := Font;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTextLayoutArray.Apply
  Params:  ADC     - Context to apply to

  Applies text layout to the specified context
 ------------------------------------------------------------------------------}
procedure TCarbonTextLayoutArray.Apply(ADC: TCarbonContext);
var
  I: Integer;
begin
  FAscent := 0;
  FDescent := 0;
  FTextBefore := 0;
  FTextAfter := 0;
  
  for I := 1 to Length(FText) do
  begin
    FFont.FCachedLayouts[Ord(FText[I])].Apply(ADC);
    
    if I = 1 then
    begin
      FAscent := FFont.FCachedLayouts[Ord(FText[1])].FAscent;
      FDescent := FFont.FCachedLayouts[Ord(FText[1])].FDescent;
      FTextBefore := FFont.FCachedLayouts[Ord(FText[1])].FTextBefore;
      FTextAfter := FTextBefore;
    end;
    FTextAfter := FTextAfter + Long2Fix(FFont.FCachedLayouts[Ord(FText[I])].GetWidth);
  end;
end;

function TCarbonTextLayoutArray.Draw(X, Y: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  
  for I := 1 to Length(FText) do
  begin
    Result := FFont.FCachedLayouts[Ord(FText[I])].Draw(X, Y);
    Inc(X, FFont.FCachedLayouts[Ord(FText[I])].GetWidth);
  end;

  Result := True;
end;

{ TCarbonFont }

{------------------------------------------------------------------------------
  Method:  TCarbonFont.Create
  Params:  AGlobal

  Creates default Carbon font
 ------------------------------------------------------------------------------}
constructor TCarbonFont.Create(AGlobal: Boolean);
begin
  inherited Create(AGlobal);
  
  FStyle := DefaultTextStyle;
  FLineRotation := 0;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonFont.Create
  Params:  ALogFont  - Font characteristics
           AFaceName - Name of the font

  Creates Carbon font with the specified name and characteristics
 ------------------------------------------------------------------------------}
constructor TCarbonFont.Create(ALogFont: TLogFont; const AFaceName: String);
begin
  inherited Create(False);
  
  FStyle := CreateStyle(ALogFont, AFaceName);
  
  // applied when creating text layout
  FLineRotation := (ALogFont.lfEscapement shl 16) div 10;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonFont.CreateStyle
  Params:  ALogFont  - Font characteristics
           AFaceName - Name of the font
  Returns: ATSUStyle for the specified font name and characteristics
 ------------------------------------------------------------------------------}
function TCarbonFont.CreateStyle(ALogFont: TLogFont; const AFaceName: String): ATSUStyle;
var
  Attr: ATSUAttributeTag;
  M: ATSUTextMeasurement;
  B: Boolean;
  S: ByteCount;
  A: ATSUAttributeValuePtr;
  ID: ATSUFontID;
const
  SSetAttrs = 'ATSUSetAttributes';
  SName = 'CreateStyle';
begin
  inherited Create(False);

  OSError(ATSUCreateStyle(Result), Self, SName, SCreateStyle);

  ID := FindCarbonFontID(AFaceName);

  if ID <> 0 then
  begin
    Attr := kATSUFontTag;
    A := @ID;
    S := SizeOf(ID);
    OSError(ATSUSetAttributes(Result, 1, @Attr, @S, @A), Self, SName,
      SSetAttrs, 'kATSUFontTag');
  end;

  if ALogFont.lfHeight <> 0 then
  begin
    Attr := kATSUSizeTag;
    M := Abs(ALogFont.lfHeight) shl 16;
    A := @M;
    S := SizeOf(M);
    OSError(ATSUSetAttributes(Result, 1, @Attr, @S, @A), Self, SName,
      SSetAttrs, 'kATSUSizeTag');
  end;

  if ALogFont.lfWeight > FW_NORMAL then
  begin
    Attr := kATSUQDBoldfaceTag;
    B := True;
    A := @B;
    S := SizeOf(B);
    OSError(ATSUSetAttributes(Result, 1, @Attr, @S, @A), Self, SName,
      SSetAttrs, 'kATSUQDBoldfaceTag');
  end;

  if ALogFont.lfItalic > 0 then
  begin
    Attr := kATSUQDItalicTag;
    B := True;
    A := @B;
    S := SizeOf(B);
    OSError(ATSUSetAttributes(Result, 1, @Attr, @S, @A), Self, SName, SSetAttrs,
      'kATSUQDItalicTag');
  end;

  if ALogFont.lfUnderline > 0 then
  begin
    Attr := kATSUQDUnderlineTag;
    B := True;
    A := @B;
    S := SizeOf(B);
    OSError(ATSUSetAttributes(Result, 1, @Attr, @S, @A), Self, SName,
      SSetAttrs, 'kATSUQDUnderlineTag');
  end;

  if ALogFont.lfStrikeOut > 0 then
  begin
    Attr := kATSUStyleStrikeThroughTag;
    B := True;
    A := @B;
    S := SizeOf(B);
    OSError(ATSUSetAttributes(Result, 1, @Attr, @S, @A), Self, SName,
      SSetAttrs, 'kATSUStyleStrikeThroughTag');
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonFont.Destroy

  Frees Carbon font
 ------------------------------------------------------------------------------}
destructor TCarbonFont.Destroy;
var
  I: Integer;
begin
  if FStyle <> DefaultTextStyle then
    OSError(ATSUDisposeStyle(FStyle), Self, SDestroy, SCreateStyle);
  for I := 0 to High(FCachedLayouts) do
    if FCachedLayouts[I] <> nil then FCachedLayouts[I].Release;

  inherited;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonFont.SetColor
  Params:  AColor  - Font color

  Chnage font style color
 ------------------------------------------------------------------------------}
procedure TCarbonFont.SetColor(AColor: TColor);
var
  Attr: ATSUAttributeTag;
  S: ByteCount;
  A: ATSUAttributeValuePtr;
  C: RGBColor;
begin
  C := ColorToRGBColor(AColor);

  Attr := kATSUColorTag;
  A := @C;
  S := SizeOf(C);
  OSError(ATSUSetAttributes(Style, 1, @Attr, @S, @A), Self, SShowModal,
    'ATSUSetAttributes', 'kATSUSizeTag');
end;

function TCarbonFont.CreateTextLayout(const Text: String;
  TextFractional: Boolean): TCarbonTextLayout;
  
  function IsTextASCII: Boolean;
  var
    I: Integer;
    C: Byte;
  begin
    Result := False;

    for I := 1 to Length(Text) do
    begin
      C := Ord(Text[I]);
      if (C > 127) or (C = 10) or (C = 13) then Exit;
    end;
    
    Result := True;
  end;
var
  I, J, L: Integer;
  C: Byte;
begin
  if (FLineRotation <> 0) or TextFractional or not IsTextASCII then
    Result := TCarbonTextLayoutBuffer.Create(Text, Self, TextFractional)
  else
  begin
    for I := 1 to Length(Text) do
    begin
      C := Ord(Text[I]);
      if C > High(FCachedLayouts) then
      begin
        L := Length(FCachedLayouts);
        SetLength(FCachedLayouts, C + 1);
        for J := L to C do FCachedLayouts[J] := nil;
      end;
      
      if FCachedLayouts[C] = nil then
        FCachedLayouts[C] := TCarbonTextLayoutBuffer.Create(Text[I], Self, TextFractional);
    end;
    
    Result := TCarbontextLayoutArray.Create(Text, Self);
  end;
end;

{ TCarbonColorObject }

{------------------------------------------------------------------------------
  Method:  TCarbonColorObject.Create
  Params:  AColor  - Color
           ASolid  - Opacity
           AGlobal - Global
          
  Creates Carbon color object
 ------------------------------------------------------------------------------}
constructor TCarbonColorObject.Create(const AColor: TColor; ASolid, AGlobal: Boolean);
begin
  inherited Create(AGlobal);
  
  SetColor(AColor, ASolid);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonColorObject.SetColor
  Params:  AColor - Color
           ASolid - Opacity

  Sets the color and opacity
 ------------------------------------------------------------------------------}
procedure TCarbonColorObject.SetColor(const AColor: TColor; ASolid: Boolean);
begin
  RedGreenBlue(ColorToRGB(AColor), FR, FG, FB);
  FA := ASolid;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonColorObject.GetRGBA
  Params:  AROP2 - Binary raster operation
           AR, AG, AB, AA - Red, green, blue, alpha component of color

  Gets the individual color components according to the binary raster operation
 ------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------
  Method:  TCarbonColorObject.CreateCGColor
  Returns: CGColor
 ------------------------------------------------------------------------------}
function TCarbonColorObject.CreateCGColor: CGColorRef;
var
  F: Array [0..3] of Single;
begin
  F[0] := FR / 255;
  F[1] := FG / 255;
  F[2] := FB / 255;
  F[3] := Byte(FA);
  
  Result := CGColorCreate(RGBColorSpace, @F[0]);
end;

{ TCarbonBrush }

{------------------------------------------------------------------------------
  Method:  TCarbonBrush.Create
  Params:  AGlobal

  Creates default Carbon brush
 ------------------------------------------------------------------------------}
constructor TCarbonBrush.Create(AGlobal: Boolean);
begin
  inherited Create(clWhite, True, AGlobal);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBrush.Create
  Params:  ALogBrush - Brush characteristics

  Creates Carbon brush with the specified characteristics
 ------------------------------------------------------------------------------}
constructor TCarbonBrush.Create(ALogBrush: TLogBrush);
begin
  case ALogBrush.lbStyle of
    BS_SOLID,
    BS_HATCHED..BS_MONOPATTERN:
      begin
        inherited Create(ColorToRGB(ALogBrush.lbColor), True, False);
        // TODO: patterns
      end;
    else
      inherited Create(ColorToRGB(ALogBrush.lbColor), False, False);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBrush.Apply
  Params:  ADC     - Context to apply to
           UseROP2 - Consider binary raster operation?

  Applies brush to the specified context
 ------------------------------------------------------------------------------}
procedure TCarbonBrush.Apply(ADC: TCarbonContext; UseROP2: Boolean);
var
  AR, AG, AB, AA: Single;
  AROP2: Integer;
begin
  if ADC = nil then Exit;
  if ADC.CGContext = nil then Exit;

  if UseROP2 then AROP2 := (ADC as TCarbonDeviceContext).ROP2
  else AROP2 := R2_COPYPEN;

  GetRGBA(AROP2, AR, AG, AB, AA);

  if AROP2 <> R2_NOT then
    CGContextSetBlendMode(ADC.CGContext, kCGBlendModeNormal)
  else
    CGContextSetBlendMode(ADC.CGContext, kCGBlendModeDifference);

  CGContextSetRGBFillColor(ADC.CGContext, AR, AG, AB, AA);
end;

{ TCarbonPen }

{------------------------------------------------------------------------------
  Method:  TCarbonPen.Create
  Params:  AGlobal

  Creates default Carbon pen
 ------------------------------------------------------------------------------}
constructor TCarbonPen.Create(AGlobal: Boolean);
begin
  inherited Create(clBlack, True, AGlobal);
  FStyle := PS_SOLID;
  FWidth := 1;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonPen.Create
  Params:  ALogPen - Pen characteristics

  Creates Carbon pen with the specified characteristics
 ------------------------------------------------------------------------------}
constructor TCarbonPen.Create(ALogPen: TLogPen);
begin
  case ALogPen.lopnStyle of
    PS_SOLID..PS_DASHDOTDOT,
    PS_INSIDEFRAME:
      begin
        inherited Create(ColorToRGB(ALogPen.lopnColor), True, False);
        FWidth := Max(1, ALogPen.lopnWidth.x);
      end;
    else
    begin
      inherited Create(ColorToRGB(ALogPen.lopnColor), False, False);
      FWidth := 1;
    end;
  end;

  FStyle := ALogPen.lopnStyle;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonPen.Apply
  Params:  ADC     - Context to apply to
           UseROP2 - Consider binary raster operation?

  Applies pen to the specified context
 ------------------------------------------------------------------------------}
procedure TCarbonPen.Apply(ADC: TCarbonContext; UseROP2: Boolean);
var
  AR, AG, AB, AA: Single;
  AROP2: Integer;
begin
  if ADC = nil then Exit;
  if ADC.CGContext = nil then Exit;

  if UseROP2 then AROP2 := (ADC as TCarbonDeviceContext).ROP2
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

{------------------------------------------------------------------------------
  Method:  TCarbonBitmap.GetBitsPerComponent
  Returns: Bitmap bits per component
 ------------------------------------------------------------------------------}
function TCarbonBitmap.GetBitsPerComponent: Integer;
begin
  case FType of
    cbtMask,
    cbtGray: Result := FDepth;
    cbtRGB,
    cbtBGR:  Result := FDepth div 3;
    cbtARGB,
    cbtBGRA: Result := FDepth shr 2;
  else
    Result := 0;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBitmap.GetColorSpace
  Returns: The colorspace for this type of bitmap
 ------------------------------------------------------------------------------}
function TCarbonBitmap.GetColorSpace: CGColorSpaceRef;
begin
  if FType in [cbtMask, cbtGray]
  then Result := GrayColorSpace
  else Result := RGBColorSpace
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBitmap.GetInfo
  Returns: The CGBitmapInfo for this type of bitmap
 ------------------------------------------------------------------------------}
function TCarbonBitmap.GetInfo: CGBitmapInfo;
begin
  Result := BITMAPINFOMAP[FType];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBitmap.Create
  Params:  AWidth        - Bitmap width
           AHeight       - Bitmap height
           ADepth        - Significant bits per pixel
           ABitsPerPixel - The number of allocated bits per pixel (can be larger than depth)
           AAlignment    - Alignment of the data for each row
           ABytesPerRow  - The number of bytes between rows
           ACopyData     - Copy supplied bitmap data (OPTIONAL)
  
  Creates Carbon bitmap with the specified characteristics
 ------------------------------------------------------------------------------}
constructor TCarbonBitmap.Create(AWidth, AHeight, ADepth, ABitsPerPixel: Integer;
  AAlignment: TCarbonBitmapAlignment; AType: TCarbonBitmapType; AData: Pointer;
  ACopyData: Boolean);
const
  ALIGNBITS: array[TCarbonBitmapAlignment] of Integer = (0, 1, 3, 7, $F);
var
  M: Integer;
begin
  inherited Create(False);
  
  FCGImage := nil;
  
  if AWidth < 1 then AWidth := 1;
  if AHeight < 1 then AHeight := 1;
  FWidth := AWidth;
  FHeight := AHeight;
  FDepth := ADepth;
  FBitsPerPixel := ABitsPerPixel;
  FType := AType;
  FAlignment := AAlignment;

  FBytesPerRow := ((AWidth * ABitsPerPixel) + 7) shr 3;
  M := FBytesPerRow and ALIGNBITS[AAlignment];
  if M <> 0 then Inc(FBytesPerRow, ALIGNBITS[AAlignment] + 1 - M);

  FDataSize := FBytesPerRow * FHeight;
  
  if (AData = nil) or ACopyData then
  begin
    System.GetMem(FData, FDataSize);
    FFreeData := True;
    if AData <> nil then
      System.Move(AData^, FData^, FDataSize) // copy data
    else
      FillDWord(FData^, FDataSize shr 2, 0); // clear bitmap
  end
  else
  begin
    FData := AData;
    FFreeData := False;
  end;
//DebugLn(Format('TCarbonBitmap.Create %d x %d Data: %d RowSize: %d Size: %d',
//  [AWidth, AHeight, Integer(AData), DataRowSize, FDataSize]));

  Update;
  
  //DbgDumpImage(FCGImage, 'TCarbonBitmap.Create');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBitmap.Create
  Params:  ABitmap - Source bitmap

  Creates Carbon bitmap as a copy of specified bitmap
 ------------------------------------------------------------------------------}
constructor TCarbonBitmap.Create(ABitmap: TCarbonBitmap);
begin
  Create(ABitmap.Width, ABitmap.Height, ABitmap.Depth, ABitmap.FBitsPerPixel,
    ABitmap.FAlignment, ABitmap.FType, ABitmap.Data);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBitmap.Destroy

  Frees Carbon bitmap
 ------------------------------------------------------------------------------}
destructor TCarbonBitmap.Destroy;
begin
  CGImageRelease(FCGImage);
  if FFreeData then System.FreeMem(FData);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBitmap.Update

  Updates Carbon bitmap
 ------------------------------------------------------------------------------}
procedure TCarbonBitmap.Update;
var
  CGDataProvider: CGDataProviderRef;
begin
  if FData = nil then Exit;
  if FCGImage <> nil then CGImageRelease(FCGImage);

  CGDataProvider := CGDataProviderCreateWithData(nil, FData, FDataSize, nil);
  try
    if FType = cbtMask
    then FCGImage := CGImageMaskCreate(FWidth, FHeight, GetBitsPerComponent,
           FBitsPerPixel, FBytesPerRow, CGDataProvider, nil, 0)
    else FCGImage := CGImageCreate(FWidth, FHeight, GetBitsPerComponent,
           FBitsPerPixel, FBytesPerRow, GetColorSpace, BITMAPINFOMAP[FType],
           CGDataProvider, nil, 0, kCGRenderingIntentDefault);
  finally
    CGDataProviderRelease(CGDataProvider);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBitmap.CreateSubImage
  Returns: New image ref to portion of image data according to the rect
 ------------------------------------------------------------------------------}
function TCarbonBitmap.CreateSubImage(const ARect: TRect): CGImageRef;
begin
  if CGImage = nil then Result := nil
  else Result := CGImageCreateWithImageInRect(CGImage, RectToCGRect(ARect));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBitmap.CreateMaskedImage
  Returns: New image ref to masked image data
 ------------------------------------------------------------------------------}
function TCarbonBitmap.CreateMaskedImage(AMask: TCarbonBitmap): CGImageRef;
begin
  Result := CreateMaskedImage(AMask, Classes.Rect(0, 0, Width, Height));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBitmap.CreateMaskedImage
  Returns: New image ref to portion of masked image data according to the rect
 ------------------------------------------------------------------------------}
function TCarbonBitmap.CreateMaskedImage(AMask: TCarbonBitmap;
  const ARect: TRect): CGImageRef;
var
  CGSubImage: CGImageRef;
begin
  Result := nil;
  if CGImage = nil then Exit;
  if (AMask <> nil) and (AMask.CGImage <> nil) then
  begin
    CGSubImage := CreateSubImage(ARect);
    try
      Result := CGImageCreateWithMask(CGSubImage, AMask.CGImage);
    finally
      CGImageRelease(CGSubImage);
    end;
  end
  else
    Result := CreateSubImage(ARect);
end;

{ TCarbonCursor }

{------------------------------------------------------------------------------
  Method:  TCarbonCursor.Create

  Creates Carbon cursor
 ------------------------------------------------------------------------------}
constructor TCarbonCursor.Create;
begin
  inherited Create(False);

  FCursorType := cctUnknown;
  FThemeCursor := 0;
  FAnimationStep := 0;
  FQDHardwareCursorName := '';
  FPixmapHandle := nil;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCursor.CreateThread

  Creates cursor animation thread
 ------------------------------------------------------------------------------}
procedure TCarbonCursor.CreateThread;
begin
  FTaskID := nil;
  OSError(MPCreateTask(@AnimationCursorHandler, Self, 0, nil, nil, nil, 0, @FTaskID),
    Self, 'CreateThread', 'MPCreateTask');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCursor.DestroyThread

  Destroys cursor animation thread
 ------------------------------------------------------------------------------}
procedure TCarbonCursor.DestroyThread;
begin
  OSError(MPTerminateTask(FTaskID, noErr), Self, 'DestroyThread', 'MPTerminateTask');
  FTaskID := nil;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCursor.CreateHardwareCursor
  Params:  ABitmap  - Cursor image
           AHotSpot - Hot spot position

  Creates new hardware cursor
 ------------------------------------------------------------------------------}
procedure TCarbonCursor.CreateHardwareCursor(ABitmap: TCarbonBitmap; AHotSpot: Point);
var
  B: Rect;
begin
  FCursorType := cctQDHardware;

  B.top := 0;
  B.left := 0;
  B.bottom := ABitmap.Height;
  B.right := ABitmap.Width;

  FPixmapHandle := PixMapHandle(NewHandleClear(SizeOf(PixMap)));
  // tell that this is pixmap (bit 15 := 1)
  FPixmapHandle^^.rowBytes := ABitmap.BytesPerRow or $8000;
  FPixmapHandle^^.bounds := B;
  FPixmapHandle^^.pmVersion := 0;
  FPixmapHandle^^.packType := 0;
  FPixmapHandle^^.packSize := 0;
  FPixmapHandle^^.hRes := $00480000; // 72 dpi
  FPixmapHandle^^.vRes := $00480000; // 72 dpi
  FPixmapHandle^^.pixelType := RGBDirect;
  FPixmapHandle^^.cmpSize := ABitmap.BitsPerComponent;
  FPixmapHandle^^.cmpCount := ABitmap.Depth div FPixmapHandle^^.cmpSize;  // $AARRGGBB
  FPixmapHandle^^.pixelSize := ABitmap.Depth; // depth
  FPixmapHandle^^.pmTable := nil;
  FPixmapHandle^^.baseAddr := Ptr(ABitmap.Data);

  FQDHardwareCursorName := Application.Title + LazarusCursorInfix + IntToStr(Integer(Self));
  OSError(
    QDRegisterNamedPixMapCursor(FPixmapHandle, nil, AHotSpot, PChar(FQDHardwareCursorName)),
    Self, 'CreateHardwareCursor', 'QDRegisterNamedPixMapCursor');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCursor.CreateColorCursor
  Params:  ABitmap  - Cursor image
           AHotSpot - Hot spot position

  Creates new color cursor
 ------------------------------------------------------------------------------}
procedure TCarbonCursor.CreateColorCursor(ABitmap: TCarbonBitmap; AHotSpot: Point);
var
  Bounds: Rect;
  i, j, rowBytes: integer;
  SrcRowPtr, SrcPtr, DstRowPtr: PByte;
  RowMask, RowData, Bit: UInt16;
begin
  FCursorType := cctQDColor;
  Bounds.top := 0;
  Bounds.left := 0;
  Bounds.bottom := 16;
  Bounds.right := 16;

  FPixmapHandle := PixMapHandle(NewHandleClear(SizeOf(PixMap)));
  FPixmapHandle^^.baseAddr := nil;
  FPixmapHandle^^.bounds := Bounds;
  // tell that this is pixmap (bit 15 := 1)
  FPixmapHandle^^.pmVersion := 0;
  FPixmapHandle^^.packType := 0;
  FPixmapHandle^^.packSize := 0;
  FPixmapHandle^^.hRes := $00480000; // 72 dpi
  FPixmapHandle^^.vRes := $00480000; // 72 dpi
  FPixmapHandle^^.pixelType := RGBDirect;
  FPixmapHandle^^.cmpSize := ABitmap.BitsPerComponent;
  FPixmapHandle^^.cmpCount := ABitmap.Depth div FPixmapHandle^^.cmpSize;  // $AARRGGBB
  FPixmapHandle^^.pixelSize := ABitmap.Depth; // depth
  rowBytes := FPixmapHandle^^.Bounds.right * (FPixmapHandle^^.pixelSize shr 3);
  FPixmapHandle^^.rowBytes := rowBytes or $8000;
  FPixmapHandle^^.pmTable := nil;

  // create cursor handle
  FQDColorCursorHandle := CCrsrHandle(NewHandleClear(SizeOf(CCrsr)));
  FQDColorCursorHandle^^.crsrType := SInt16($8001); // color cursor ($8000 - bw)
  FQDColorCursorHandle^^.crsrMap := FPixmapHandle;
  FQDColorCursorHandle^^.crsrXData := nil;
  FQDColorCursorHandle^^.crsrXValid := 0;
  FQDColorCursorHandle^^.crsrXHandle := nil;
  FQDColorCursorHandle^^.crsrHotspot.h := Min(15, AHotSpot.h);
  FQDColorCursorHandle^^.crsrHotspot.v := Min(15, AHotSpot.v);
  FQDColorCursorHandle^^.crsrXTable := 0;
  FQDColorCursorHandle^^.crsrID := GetCTSeed;

  // handle for data with size = rowBytes * height
  FQDColorCursorHandle^^.crsrData := NewHandleClear(rowBytes * FPixmapHandle^^.bounds.bottom);

  // fill cursor bitmap and mask
  SrcRowPtr := ABitmap.Data;
  DstRowPtr := PByte(FQDColorCursorHandle^^.crsrData^);
  for i := 0 to 15 do
  begin
    RowMask := 0;
    RowData := 0;
    Bit := $8000;
    SrcPtr := SrcRowPtr;
    System.Move(SrcPtr^, DstRowPtr^, 16 * 4);
    for j := 0 to 15 do
    begin
      // check alpha
      if SrcPtr[0] and $FF = 0 then
        RowData := RowData or Bit
      else
        RowMask := RowMask or Bit;

      Bit := Bit shr 1;
      Inc(SrcPtr, 4);
    end;
 {$IFDEF ENDIAN_BIG}
    FQDColorCursorHandle^^.crsrMask[i] := RowMask;
    FQDColorCursorHandle^^.crsr1Data[i] := RowData;
 {$ELSE}
    FQDColorCursorHandle^^.crsrMask[i] := CFSwapInt16(RowMask);
    FQDColorCursorHandle^^.crsr1Data[i] := CFSwapInt16(RowData);
 {$ENDIF}
    Inc(SrcRowPtr, ABitmap.BytesPerRow);
    Inc(DstRowPtr, rowBytes);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCursor.CreateFromInfo
  Params:  AInfo - Cusrsor info

  Creates new cursor from the specified info
 ------------------------------------------------------------------------------}
constructor TCarbonCursor.CreateFromInfo(AInfo: PIconInfo);
var
  AHotspot: Point;
begin
  Create;

  if (AInfo^.hbmColor = 0) or not (TObject(AInfo^.hbmColor) is TCarbonBitmap) then
    Exit;

  AHotspot.h := AInfo^.xHotspot;
  AHotspot.v := AInfo^.yHotspot;
  
  if HardwareCursorsSupported then
    CreateHardwareCursor(TCarbonBitmap(AInfo^.hbmColor), AHotSpot) else
    CreateColorCursor(TCarbonBitmap(AInfo^.hbmColor), AHotSpot);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCursor.CreateThemed
  Params:  AThemeCursor - Theme cursor kind

  Creates new theme cursor
 ------------------------------------------------------------------------------}
constructor TCarbonCursor.CreateThemed(AThemeCursor: ThemeCursor;
  ADefault: Boolean);
const
  kThemeCursorTypeMap: array[kThemeArrowCursor..22] of TCarbonCursorType =
  (
    cctTheme,    // kThemeArrowCursor
    cctTheme,    // kThemeCopyArrowCursor
    cctTheme,    // kThemeAliasArrowCursor
    cctTheme,    // kThemeContextualMenuArrowCursor
    cctTheme,    // kThemeIBeamCursor
    cctTheme,    // kThemeCrossCursor
    cctTheme,    // kThemePlusCursor
    cctAnimated, // kThemeWatchCursor
    cctTheme,    // kThemeClosedHandCursor
    cctTheme,    // kThemeOpenHandCursor
    cctTheme,    // kThemePointingHandCursor
    cctAnimated, // kThemeCountingUpHandCursor
    cctAnimated, // kThemeCountingDownHandCursor
    cctAnimated, // kThemeCountingUpAndDownHandCursor
    cctWait,     // kThemeSpinningCursor (!!! obsolte and thats why we should use wait instead)
    cctTheme,    // kThemeResizeLeftCursor
    cctTheme,    // kThemeResizeRightCursor
    cctTheme,    // kThemeResizeLeftRightCursor
    cctTheme,    // kThemeNotAllowedCursor
    cctTheme,    // kThemeResizeUpCursor
    cctTheme,    // kThemeResizeDownCursor
    cctTheme,    // kThemeResizeUpDownCursor
    cctTheme     // kThemePoofCursor
  );
begin
  Create;
  FDefault := ADefault;
  FThemeCursor := AThemeCursor;
  if (AThemeCursor >= Low(kThemeCursorTypeMap)) and
     (AThemeCursor <= High(kThemeCursorTypeMap)) then
    FCursorType := kThemeCursorTypeMap[FThemeCursor] else
    FCursorType := cctTheme;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCursor.Destroy

  Frees Carbon cursor
 ------------------------------------------------------------------------------}
destructor TCarbonCursor.Destroy;
begin
  UnInstall;
  
  case CursorType of
    cctQDHardware:
      if FQDHardwareCursorName <> '' then
      begin
        OSError(QDUnregisterNamedPixmapCursor(PChar(FQDHardwareCursorName)),
          Self, SDestroy, 'QDUnregisterNamedPixmapCursor');
        
        FPixmapHandle^^.baseAddr := nil;
        DisposePixMap(FPixmapHandle);
      end;
    cctQDColor:
      DisposeCCursor(FQDColorCursorHandle);  // suppose pixmap will be disposed too
  end;
  
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCursor.Install

  Installs Carbon cursor
 ------------------------------------------------------------------------------}
procedure TCarbonCursor.Install;
const
  SName = 'Install';
begin
  {$IFDEF VerboseCursor}
    DebugLn('TCarbonCursor.Install type: ', DbgS(Ord(CursorType)));
  {$ENDIF}
  
  case CursorType of
    cctQDHardware:
      if FQDHardwareCursorName <> '' then
        OSError(QDSetNamedPixmapCursor(PChar(FQDHardwareCursorName)),
          Self, SName, 'QDSetNamedPixmapCursor');
    cctQDColor:
      SetCCursor(FQDColorCursorHandle);
    cctTheme:
      OSError(SetThemeCursor(FThemeCursor),
        Self, SName, 'SetThemeCursor');
    cctAnimated:
      begin
        FAnimationStep := 0;
        CreateThread;
      end;
    cctWait:
      QDDisplayWaitCursor(True);
    else
      DebugLn('[TCarbonCursor.Apply] !!! Unknown cursor type');
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCursor.UnInstall

  Uninstalls Carbon cursor
 ------------------------------------------------------------------------------}
procedure TCarbonCursor.UnInstall;
begin
  case CursorType of
    cctWait: QDDisplayWaitCursor(False);
    cctAnimated: DestroyThread;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCursor.StepAnimation
  Returns: If the function succeeds

  Steps Carbon cursor animation
 ------------------------------------------------------------------------------}
function TCarbonCursor.StepAnimation: Boolean;
begin
  Result := SetAnimatedThemeCursor(FThemeCursor, FAnimationStep) <> themeBadCursorIndexErr;
  if Result then
  begin
    inc(FAnimationStep);
  end else
  begin
    FCursorType := cctTheme;
    SetThemeCursor(FThemeCursor);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCursor.HardwareCursorsSupported
  Returns: If hardware cursors are supported
 ------------------------------------------------------------------------------}
class function TCarbonCursor.HardwareCursorsSupported: Boolean;
var
  P: Point;
  ATestCursorName: String;
  ATempPixmap: PixmapHandle;
begin
  if MHardwareCursorsSupported = hcaUndef then
  begin
    ATestCursorName := Application.Title + LazarusCursorInfix + 'test';
    P.v := 0;
    P.h := 0;
    
    ATempPixmap := PixMapHandle(NewHandleClear(SizeOf(PixMap)));
    if QDRegisterNamedPixMapCursor(ATempPixmap, nil, P, PChar(ATestCursorName)) = kQDNoColorHWCursorSupport then
      MHardwareCursorsSupported := hcaUnavailable else
      MHardwareCursorsSupported := hcaAvailable;
    QDUnregisterNamedPixmapCursor(PChar(ATestCursorName));
    DisposePixMap(ATempPixmap);
  end;
  Result := MHardwareCursorsSupported = hcaAvailable;
end;


var
  LogBrush: TLogBrush;


initialization

  InitCursor;

  StockSystemFont := TCarbonFont.Create(True);

  LogBrush.lbStyle := BS_NULL;
  LogBrush.lbColor := 0;
  StockNullBrush := TCarbonBrush.Create(LogBrush);

  WhiteBrush := TCarbonBrush.Create(True);
  BlackPen := TCarbonPen.Create(True);
  
  DefaultFont := TCarbonFont.Create(True);

  DefaultBrush := TCarbonBrush.Create(True);
  DefaultPen := TCarbonPen.Create(True);
  
  DefaultContext := TCarbonBitmapContext.Create;
  DefaultBitmap := TCarbonBitmap.Create(1, 1, 32, 32, cbaDQWord, cbtARGB, nil);
  DefaultContext.Bitmap := DefaultBitmap;
  
  ScreenContext := TCarbonScreenContext.Create;
  ScreenContext.CGContext := DefaultContext.CGContext; // workaround

finalization
  DefaultContext.Free;
  ScreenContext.Free;
  
  DefaultBrush.Free;
  DefaultPen.Free;
  
  DefaultFont.Free;
  
  BlackPen.Free;
  WhiteBrush.Free;

  StockNullBrush.Free;
  StockSystemFont.Free;
  
  DefaultBitmap.Free;

end.
