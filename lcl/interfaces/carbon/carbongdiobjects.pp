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
    FDataSize: Integer;
    FBytesPerRow: Integer;
    FBitsPerPixel: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FCGImage: CGImageRef;
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
     {crSizeNS      } kThemeResizeUpDownCursor,
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
    cctUnknown,  // undefined
    cctImage,    // from image
    cctTheme,    // theme cursor
    cctAnimated, // animated theme cursor
    cctWait      // special wait cursor
  );

  { TCarbonCursor }
  
  TCarbonCursor = class(TCarbonGDIObject)
  private
    // TODO: cursor from image

    FCursorType: TCarbonCursorType;
    FThemeCursor: ThemeCursor;
    FAnimationStep: Integer;
    FTaskID: MPTaskID;

    procedure CreateThread;
    procedure DestroyThread;
  public
    constructor Create;
    constructor CreateFromInfo(AInfo: PIconInfo);
    constructor CreateThemed(AThemeCursor: ThemeCursor);
    destructor Destroy; override;

    procedure Install;
    procedure UnInstall;
    function StepAnimation: Boolean;
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

const
  kThemeCursorAnimationDelay = 70;
  
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

  FDataSize := FBytesPerRow * FWidth;
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

constructor TCarbonCursor.CreateFromInfo(AInfo: PIconInfo);
begin
  // TODO:
  Create;
  FCursorType := cctImage;
end;

constructor TCarbonCursor.CreateThemed(AThemeCursor: ThemeCursor);
const
  kThemeCursorTypeMap: array[kThemeArrowCursor..22] of TCarbonCursorType =
  (
    cctTheme,    // kThemeArrowCursor
    cctTheme,    // kThemeCopyArrowCursor
    cctTheme,    // 2
    cctTheme,    // 3
    cctTheme,    // 4
    cctTheme,    // 5
    cctTheme,    // 6
    cctAnimated, // kThemeWatchCursor
    cctTheme,    // 8
    cctTheme,    // 9
    cctTheme,    // 10
    cctAnimated, // kThemeCountingUpHandCursor
    cctAnimated, // kThemeCountingDownHandCursor
    cctAnimated, // kThemeCountingUpAndDownHandCursor
    cctWait,     // kThemeSpinningCursor (obsolte and thats why we should use wait instead)
    cctTheme,    // 15
    cctTheme,    // 16
    cctTheme,    // 17
    cctTheme,    // 18
    cctTheme,    // 19
    cctTheme,    // 20
    cctTheme,    // 21
    cctTheme     // kThemeProofCursor
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
  if CursorType = cctImage then
  begin
    // TODO:
  end;
  inherited Destroy;
end;

procedure TCarbonCursor.Install;
begin
  if CursorType = cctImage then
  begin
    // TODO : SetCCursor();
  end else
  if CursorType = cctTheme then
  begin
    SetThemeCursor(FThemeCursor);
  end else
  if CursorType = cctAnimated then
  begin
    FAnimationStep := 0;
    CreateThread;
  end else
  if CursorType = cctWait then
  begin
    QDDisplayWaitCursor(True);
  end else
    DebugLn('[TCarbonCursor.Apply] !!! Unknown cursor type');
end;

procedure TCarbonCursor.UnInstall;
begin
  if CursorType = cctWait then
    QDDisplayWaitCursor(False) else
  if CursorType = cctAnimated then
  begin
    DestroyThread;
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


var
  LogBrush: TLogBrush;

initialization

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
