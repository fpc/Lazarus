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

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math,
 // carbon bindings
  FPCMacOSAll,
 // LCL
  LCLProc, LCLType, Graphics, Controls, Forms,
 // LCL Carbon
  CarbonDef;

type

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
    procedure Apply(ADC: TCarbonContext; UseROP2: Boolean = True);
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
    procedure Apply(ADC: TCarbonContext; UseROP2: Boolean = True);
  end;

  { TCarbonBitmap }

  TCarbonBitmap = class(TCarbonGDIObject)
  private
    FData: Pointer;
    FARGBData: Pointer;
    FDataSize: Integer;
    FBytesPerRow: Integer;
    FBitsPerPixel: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FCGImage: CGImageRef;
    function GetARGBData: Pointer;
    function GetBitsPerComponent: Integer;
  public
    constructor Create(AWidth, AHeight, ABitsPerPixel: Integer; AData: Pointer);
    destructor Destroy; override;
    procedure Update;
  public
    property BitsPerComponent: Integer read GetBitsPerComponent;
    property BytesPerRow: Integer read FBytesPerRow;
    property CGImage: CGImageRef read FCGImage;
    property Data: Pointer read FData;
    property DataSize: Integer read FDataSize;
    property RGBAData: Pointer read FData;
    property ARGBData: Pointer read GetARGBData;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

const
  kThemeUndefCursor = ThemeCursor(-1); // undefined mac theme cursor
  
  CursorToThemeCursor: array[crLow..crHigh] of ThemeCursor =
    ({crSizeSE      } kThemeResizeLeftCursor, {!!}
     {crSizeS       } kThemeResizeDownCursor,
     {crSizeSW      } kThemeResizeRightCursor, {!!}
     {crSizeE       } kThemeResizeLeftCursor,
     {crSizeW       } kThemeResizeRightCursor,
     {crSizeNE      } kThemeResizeLeftCursor, {!!}
     {crSizeN       } kThemeResizeUpCursor,
     {crSizeNW      } kThemeResizeRightCursor, {!!}
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
     {crSizeNS      } kThemeResizeLeftRightCursor, {!!}
     {crSizeNESW    } kThemeResizeLeftRightCursor, {!!}
     {undefined     } kThemeArrowCursor, {!!}
     {crIBeam       } kThemeIBeamCursor,
     {crCross       } kThemeCrossCursor,
     {crArrow       } kThemeArrowCursor,
     {crNone        } kThemeArrowCursor,
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
    constructor CreateThemed(AThemeCursor: ThemeCursor);
    destructor Destroy; override;

    procedure Install;
    procedure UnInstall;
    function StepAnimation: Boolean;
    class function HardwareCursorsSupported: Boolean;
  public
    property CursorType: TCarbonCursorType read FCursorType;
  end;
  
var
  DefaultTextStyle: ATSUStyle; // default Carbon text style
  RGBColorSpace: CGColorSpaceRef; // global RGB color space

  StockSystemFont: TCarbonFont;
  StockNullBrush: TCarbonBrush;
  WhiteBrush: TCarbonBrush;
  BlackPen: TCarbonPen;

implementation

uses
  CarbonProc, CarbonCanvas;

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
  inherited Create(clWhite, True);
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

procedure TCarbonBrush.Apply(ADC: TCarbonContext; UseROP2: Boolean);
var
  AR, AG, AB, AA: Single;
  AROP2: Integer;
begin
  if ADC = nil then Exit;

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
      SetColor(ColorToRGB(ALogPen.lopnColor), False);
      FWidth := 1;
    end;
  end;

  FStyle := ALogPen.lopnStyle;
end;

procedure TCarbonPen.Apply(ADC: TCarbonContext; UseROP2: Boolean);
var
  AR, AG, AB, AA: Single;
  AROP2: Integer;
begin
  if ADC = nil then Exit;

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

function TCarbonBitmap.GetBitsPerComponent: Integer;
begin
  Result := CGImageGetBitsPerComponent(FCGImage);
end;

function TCarbonBitmap.GetARGBData: Pointer;
var
  i, j: integer;
  RowPtr, BytePtr: PByte;
begin
  if (FData = nil) or (FARGBData <> nil)
  then begin
    Result := FARGBData;
    Exit;
  end;

  // Since we need to shift from $RRGGBBAA to $AARRGGBB we move the data with
  // one byte offset so that the RRGGBB part is in place
  System.GetMem(FARGBData, FDataSize + 1);
  System.Move(FData^, PByte(FARGBData)[1], FDataSize);

  // now only the Alpha part needs to get inplace
  RowPtr := FARGBData;
  for i := 0 to FHeight - 1 do
  begin
    BytePtr := RowPtr;
    for j := 0 to FWidth - 1 do
    begin
      BytePtr[0] := BytePtr[4];
      Inc(BytePtr, 4);
    end;
    Inc(RowPtr, FBytesPerRow);
  end;

  Result := FARGBData;
end;

constructor TCarbonBitmap.Create(AWidth, AHeight, ABitsPerPixel: Integer;
  AData: Pointer);
begin
  FCGImage := nil;
  
  if AWidth < 1 then AWidth := 1;
  if AHeight < 1 then AHeight := 1;
  FWidth := AWidth;
  FHeight := AHeight;

  // TODO: enable more pixel formats
  FBitsPerPixel := 32; // RGBA-32 format
  // 128bit align for best performance
  FBytesPerRow := ((FWidth * FBitsPerPixel + 127) and not Cardinal(127)) shr 3;

  FDataSize := FBytesPerRow * FHeight;
  System.GetMem(FData, FDataSize);
  if AData <> nil then System.Move(AData^, FData^, FDataSize); // copy data

//DebugLn(Format('TCarbonBitmap.Create %d x %d Data: %d RowSize: %d Size: %d',
//  [AWidth, AHeight, Integer(AData), DataRowSize, FDataSize]));

  Update;
end;

destructor TCarbonBitmap.Destroy;
begin
  CGImageRelease(FCGImage);
  System.FreeMem(FData);
  System.FreeMem(FARGBData);

  inherited Destroy;
end;

procedure TCarbonBitmap.Update;
var
  CGDataProvider: CGDataProviderRef;
begin
  if FData = nil then Exit;
  if FCGImage <> nil then CGImageRelease(FCGImage);
  
  CGDataProvider := CGDataProviderCreateWithData(nil, FData, FDataSize, nil);
  try
    FCGImage := CGImageCreate(FWidth, FHeight, FBitsPerPixel shr 2, FBitsPerPixel,
      FBytesPerRow, RGBColorSpace, kCGImageAlphaLast,
      CGDataProvider, nil, 0, kCGRenderingIntentDefault);
  finally
    CGDataProviderRelease(CGDataProvider);
  end;
end;

{ TCarbonCursor }

constructor TCarbonCursor.Create;
begin
  FCursorType := cctUnknown;
  FThemeCursor := 0;
  FAnimationStep := 0;
  FQDHardwareCursorName := '';
  FPixmapHandle := nil;
end;

procedure TCarbonCursor.CreateThread;
begin
  FTaskID := nil;
  MPCreateTask(@AnimationCursorHandler, Self, 0, nil, nil, nil, 0, @FTaskID);
end;

procedure TCarbonCursor.DestroyThread;
begin
  MPTerminateTask(FTaskID, noErr);
  FTaskID := nil;
end;

procedure TCarbonCursor.CreateHardwareCursor(ABitmap: TCarbonBitmap; AHotSpot: Point);
var
  B: Rect;
  Status: OSStatus;
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
  FPixmapHandle^^.cmpCount := 4;  // $AARRGGBB
  FPixmapHandle^^.cmpSize := ABitmap.BitsPerComponent;
  FPixmapHandle^^.pixelSize := FPixmapHandle^^.cmpCount * FPixmapHandle^^.cmpSize; // depth
  FPixmapHandle^^.pmTable := nil;
  FPixmapHandle^^.baseAddr := Ptr(ABitmap.ARGBData);

  FQDHardwareCursorName := Application.Title + LazarusCursorInfix + IntToStr(Integer(Self));
  Status := QDRegisterNamedPixMapCursor(FPixmapHandle, nil, AHotSpot, PChar(FQDHardwareCursorName));
  if Status <> noErr then
  begin
    DebugLn('[TCarbonCursor.CreateFromInfo] - Hardware cursor error: ', IntToStr(Status));
  end;
end;

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
  FPixmapHandle^^.cmpCount := 4;  // RGBA
  FPixmapHandle^^.cmpSize := ABitmap.BitsPerComponent;
  FPixmapHandle^^.pixelSize := FPixmapHandle^^.cmpCount * FPixmapHandle^^.cmpSize; // depth
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
  SrcRowPtr := ABitmap.ARGBData;
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

constructor TCarbonCursor.CreateFromInfo(AInfo: PIconInfo);
var
  AHotspot: Point;
begin
  Create;

  if (AInfo^.hbmColor = 0) or not (TObject(AInfo^.hbmColor) is TCarbonBitmap) then
    exit;

  AHotspot.h := AInfo^.xHotspot;
  AHotspot.v := AInfo^.yHotspot;
  
  if HardwareCursorsSupported then
    CreateHardwareCursor(TCarbonBitmap(AInfo^.hbmColor), AHotSpot) else
    CreateColorCursor(TCarbonBitmap(AInfo^.hbmColor), AHotSpot);
end;

constructor TCarbonCursor.CreateThemed(AThemeCursor: ThemeCursor);
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
  FThemeCursor := AThemeCursor;
  if (AThemeCursor >= Low(kThemeCursorTypeMap)) and
     (AThemeCursor <= High(kThemeCursorTypeMap)) then
    FCursorType := kThemeCursorTypeMap[FThemeCursor] else
    FCursorType := cctTheme;
end;

destructor TCarbonCursor.Destroy;
begin
  UnInstall;
  case CursorType of
    cctQDHardware:
      if FQDHardwareCursorName <> '' then
      begin
        QDUnregisterNamedPixmapCursor(PChar(FQDHardwareCursorName));
        FPixmapHandle^^.baseAddr := nil;
        DisposePixMap(FPixmapHandle);
      end;
    cctQDColor:
      DisposeCCursor(FQDColorCursorHandle);  // suppose pixmap will be disposed too
  end;
  inherited Destroy;
end;

procedure TCarbonCursor.Install;
begin
  DebugLn('TCarbonCursor.Install type: ', IntToStr(Ord(CursorType)));
  case CursorType of
    cctQDHardware:
      if FQDHardwareCursorName <> '' then
        QDSetNamedPixmapCursor(PChar(FQDHardwareCursorName));
    cctQDColor:
     SetCCursor(FQDColorCursorHandle);
    cctTheme:
      SetThemeCursor(FThemeCursor);
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

procedure TCarbonCursor.UnInstall;
begin
  case CursorType of
    cctWait: QDDisplayWaitCursor(False);
    cctAnimated: DestroyThread;
  end;
end;

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

  ATSUCreateStyle(DefaultTextStyle);
  RGBColorSpace := CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);

  StockSystemFont := TCarbonFont.Create;

  LogBrush.lbStyle := BS_NULL;
  LogBrush.lbColor := 0;
  StockNullBrush := TCarbonBrush.Create(LogBrush);

  WhiteBrush := TCarbonBrush.Create;
  BlackPen := TCarbonPen.Create;

finalization
  BlackPen.Free;
  WhiteBrush.Free;

  StockNullBrush.Free;
  StockSystemFont.Free;

  ATSUDisposeStyle(DefaultTextStyle);
  CGColorSpaceRelease(RGBColorSpace);

end.
