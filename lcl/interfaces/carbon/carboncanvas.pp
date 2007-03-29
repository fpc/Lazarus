{ $Id$
                  -----------------------------------------
                  carboncanvas.pp  -  Carbon device context
                  -----------------------------------------

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
unit CarbonCanvas;

{$mode objfpc}{$H+}

interface

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math, Contnrs,
 // carbon bindings
  FPCMacOSAll,
 // LCL
  LCLProc, LCLType, Graphics, Controls, Forms,
 // LCL Carbon
  CarbonDef, CarbonGDIObjects;

type
  TCarbonDCData = class
    CurrentFont: TCarbonFont;
    CurrentBrush: TCarbonBrush;
    CurrentPen: TCarbonPen;

    BkColor: TColor;
    BkMode: Integer;
    BkBrush: TCarbonBrush;

    TextColor: TColor;
    TextBrush: TCarbonBrush;

    ROP2: Integer;
    PenPos: TPoint;
  end;

  { TCarbonDeviceContext }

  TCarbonDeviceContext = class(TCarbonContext)
  private
    FCurrentFont: TCarbonFont;
    FCurrentBrush: TCarbonBrush;
    FCurrentPen: TCarbonPen;

    FBkColor: TColor;
    FBkMode: Integer;
    FBkBrush: TCarbonBrush;

    FTextColor: TColor;
    FTextBrush: TCarbonBrush; // text color is fill color

    FROP2: Integer;
    FPenPos: TPoint;
    
    FSavedDCList: TFPObjectList;

    procedure SetBkColor(const AValue: TColor);
    procedure SetBkMode(const AValue: Integer);
    procedure SetCurrentBrush(const AValue: TCarbonBrush);
    procedure SetCurrentFont(const AValue: TCarbonFont);
    procedure SetCurrentPen(const AValue: TCarbonPen);
    procedure SetROP2(const AValue: Integer);
    procedure SetTextColor(const AValue: TColor);
  protected
    function GetSize: TPoint; virtual; abstract;
    function SaveDCData: TCarbonDCData; virtual;
    procedure RestoreDCData(const AData: TCarbonDCData); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset; override;
    
    function SaveDC: Integer;
    function RestoreDC(ASavedDC: Integer): Boolean;
    
    function BeginTextRender(AStr: PChar; ACount: Integer; out ALayout: ATSUTextLayout): Boolean;
    procedure EndTextRender(var ALayout: ATSUTextLayout);
    
    procedure SetAntialiasing(AValue: Boolean);
  public
    property Size: TPoint read GetSize;

    property CurrentFont: TCarbonFont read FCurrentFont write SetCurrentFont;
    property CurrentBrush: TCarbonBrush read FCurrentBrush write SetCurrentBrush;
    property CurrentPen: TCarbonPen read FCurrentPen write SetCurrentPen;

    property BkColor: TColor read FBkColor write SetBkColor;
    property BkMode: Integer read FBkMode write SetBkMode;
    property BkBrush: TCarbonBrush read FBkBrush;

    property TextColor: TColor read FTextColor write SetTextColor;
    property TextBrush: TCarbonBrush read FTextBrush;

    property ROP2: Integer read FROP2 write SetROP2;
    property PenPos: TPoint read FPenPos write FPenPos;
  end;

  { TCarbonScreenContext }

  TCarbonScreenContext = class(TCarbonDeviceContext)
  protected
    function GetSize: TPoint; override;
  public
    constructor Create;
  end;

  { TCarbonControlContext }

  TCarbonControlContext = class(TCarbonDeviceContext)
  private
    FOwner: TCarbonWidget;    // owner
  protected
    function GetSize: TPoint; override;
  public
    constructor Create(AOwner: TCarbonWidget);
  end;

  { TCarbonBitmapContext }

  TCarbonBitmapContext = class(TCarbonDeviceContext)
  private
    FBitmap: TCarbonBitmap;
  protected
    function GetSize: TPoint; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset; override;
  public
    function GetBitmap: TCarbonBitmap;
    procedure SetBitmap(const AValue: TCarbonBitmap);
  end;
  
  // TODO: TCarbonPrinterContext

implementation

uses CarbonProc;

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
    TextBrush.SetColor(ColorToRGB(AValue), True);
  end;
end;

constructor TCarbonDeviceContext.Create;
begin
  FBkBrush := TCarbonBrush.Create;
  FTextBrush := TCarbonBrush.Create;
end;

destructor TCarbonDeviceContext.Destroy;
begin
  BkBrush.Free;
  TextBrush.Free;
  
  FSavedDCList.Free;

  inherited Destroy;
end;

procedure TCarbonDeviceContext.Reset;
begin
  CurrentFont := nil;

  PenPos.x := 0;
  PenPos.y := 0;

  // create brush for bk color and mode
  FBkColor := clWhite;
  FBkMode := TRANSPARENT;
  FBkBrush.SetColor(clWhite, False);

  // create brush for text color
  FTextColor := clBlack;
  FTextBrush.SetColor(clBlack, True);

  // set raster operation to copy
  FROP2 := R2_COPYPEN;

  // set initial pen and brush
  FCurrentPen := BlackPen;
  FCurrentBrush := WhiteBrush;

  if CGContext <> nil then
  begin
    {$IFDEF VerbosePaint}
    DebugLn('TCarbonDeviceContext.Reset set defaults');
    {$ENDIF}
    // disable anti-aliasing
    CGContextSetShouldAntialias(CGContext, 0);
    CGContextSetBlendMode(CGContext, kCGBlendModeNormal);
    
    CGContextSetRGBFillColor(CGContext, 1, 1, 1, 1);
    CGContextSetRGBStrokeColor(CGContext, 0, 0, 0, 1);
    CGContextSetLineWidth(CGContext, 1);
    CGContextSetLineDash(CGContext, 0, nil, 0);
  end;
end;

function TCarbonDeviceContext.SaveDC: Integer;
begin
  Result := 0;
  if CGContext = nil then
  begin
    DebugLn('TCarbonDeviceContext.SaveDC CGContext is nil!');
    Exit;
  end;
  
  if FSavedDCList = nil then FSavedDCList := TFPObjectList.Create(True);
  
  CGContextSaveGState(CGContext);
  Result := FSavedDCList.Add(SaveDCData) + 1;
  DebugLn('TCarbonDeviceContext.SaveDC Result: ', DbgS(Result));
end;

function TCarbonDeviceContext.RestoreDC(ASavedDC: Integer): Boolean;
begin
  Result := False;
  if (FSavedDCList = nil) or (ASavedDC <= 0) or (ASavedDC > FSavedDCList.Count) then
  begin
    DebugLn(Format('TCarbonDeviceContext.RestoreDC SavedDC %d is not valid!', [ASavedDC]));
    Exit;
  end;
  
  if FSavedDCList.Count > ASavedDC then
    DebugLn(Format('TCarbonDeviceContext.RestoreDC Warning: SaveDC - RestoreDC' +
      ' not used in pair, skipped %d saved states!', [FSavedDCList.Count - ASavedDC]));
    
  while FSavedDCList.Count > ASavedDC do
  begin
    CGContextRestoreGState(CGContext);
    FSavedDCList.Delete(FSavedDCList.Count - 1);
  end;
  
  DebugLn('TCarbonDeviceContext.RestoreDC SavedDC: ', DbgS(ASavedDC));
  
  CGContextRestoreGState(CGContext);
  RestoreDCData(TCarbonDCData(FSavedDCList[ASavedDC - 1]));
  FSavedDCList.Delete(ASavedDC - 1);
  Result := True;
  
  DebugLn('TCarbonDeviceContext.RestoreDC End');
  
  if FSavedDCList.Count = 0 then FreeAndNil(FSavedDCList);
end;

function TCarbonDeviceContext.SaveDCData: TCarbonDCData;
begin
  Result := TCarbonDCData.Create;
  
  Result.CurrentFont := FCurrentFont;
  Result.CurrentBrush := FCurrentBrush;
  Result.CurrentPen := FCurrentPen;

  Result.BkColor := FBkColor;
  Result.BkMode := FBkMode;
  Result.BkBrush := FBkBrush;

  Result.TextColor := FTextColor;
  Result.TextBrush := FTextBrush;

  Result.ROP2 := FROP2;
  Result.PenPos := FPenPos;
end;

procedure TCarbonDeviceContext.RestoreDCData(const AData: TCarbonDCData);
begin
  FCurrentFont := AData.CurrentFont;
  FCurrentBrush := AData.CurrentBrush;
  FCurrentPen := AData.CurrentPen;

  FBkColor := AData.BkColor;
  FBkMode := AData.BkMode;
  FBkBrush := AData.BkBrush;

  FTextColor := AData.TextColor;
  FTextBrush := AData.TextBrush;

  FROP2 := AData.ROP2;
  FPenPos := AData.PenPos;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.BeginTextRender
  Params:  AStr    - UTF8 string to render
           ACount  - Count of chars to render
           ALayout - ATSU layout
  Returns: If the function suceeds

  Creates the ATSU text layout for the specified text and manages the device
  context to render the text.
  NOTE: Coordination system is set upside-down!
 ------------------------------------------------------------------------------}
function TCarbonDeviceContext.BeginTextRender(AStr: PChar; ACount: Integer; out
  ALayout: ATSUTextLayout): Boolean;
var
  TextStyle: ATSUStyle;
  TextLength: LongWord;
  S: String;
  W: WideString;
  Tag: ATSUAttributeTag;
  DataSize: ByteCount;
  PContext: ATSUAttributeValuePtr;
begin
  Result := False;

  // save context
  CGContextSaveGState(CGContext);

  // change coordination system
  CGContextScaleCTM(CGContext, 1, -1);
  CGContextTranslateCTM(CGContext, 0, 0);

  // convert UTF-8 string to UTF-16 string
  if ACount < 0 then S := AStr
  else S := Copy(AStr, 1, ACount);
  W := UTF8ToUTF16(S);

  if not (CurrentFont is TCarbonFont) then
    TextStyle := DefaultTextStyle
  else
    TextStyle := CurrentFont.Style;

  // create text layout
  TextLength := kATSUToTextEnd;
  if ATSUCreateTextLayoutWithTextPtr(ConstUniCharArrayPtr(@W[1]),
    kATSUFromTextBeginning, kATSUToTextEnd, Length(W), 1, @TextLength, @TextStyle,
    ALayout) = noErr then
  begin
    // set layout context
    Tag := kATSUCGContextTag;
    DataSize := SizeOf(CGContextRef);

    PContext := @CGContext;
    Result := ATSUSetLayoutControls(ALayout, 1, @Tag, @DataSize, @PContext) = noErr;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.EndTextRender
  Params:  ALayout - ATSU layout
  Returns: Nothing

  Frees the ATSU text layout and manages the device
  context to render ordinary graphic
 ------------------------------------------------------------------------------}
procedure TCarbonDeviceContext.EndTextRender(var ALayout: ATSUTextLayout);
begin
  // restore context
  CGContextRestoreGState(CGContext);

  if ALayout <> nil then ATSUDisposeTextLayout(ALayout);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.SetAntialiasing
  Params:  AValue - If should antialias

  Sets whether device context should antialias
 ------------------------------------------------------------------------------}
procedure TCarbonDeviceContext.SetAntialiasing(AValue: Boolean);
begin
  CGContextSetShouldAntialias(CGContext, CBool(AValue));
end;

{ TCarbonScreenContext }

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

function TCarbonControlContext.GetSize: TPoint;
begin
  Result.X := (FOwner.LCLObject as TControl).ClientWidth;
  Result.Y := (FOwner.LCLObject as TControl).ClientHeight;
end;

constructor TCarbonControlContext.Create(AOwner: TCarbonWidget);
begin
  inherited Create;

  FOwner := AOwner;
  Reset;
end;

{ TCarbonBitmapContext }

procedure TCarbonBitmapContext.SetBitmap(const AValue: TCarbonBitmap);
begin
  //DebugLn('TCarbonBitmapContext.SetBitmap');
  if FBitmap <> AValue then
  begin
    FBitmap := AValue;
    Reset;
  end;
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
  inherited Create;
  FBitmap := nil;
end;

destructor TCarbonBitmapContext.Destroy;
begin
  if CGContext <> nil then CGContextRelease(CGContext);

  inherited Destroy;
end;

procedure TCarbonBitmapContext.Reset;
begin
  if CGContext <> nil then CGContextRelease(CGContext);

  if FBitmap = nil then
    CGContext := nil
  else
  begin
    // create CGBitmapContext
    CGContext := CGBitmapContextCreate(FBitmap.Data, FBitmap.Width,
      FBitmap.Height, FBitmap.BitsPerComponent, FBitmap.BytesPerRow, RGBColorSpace,
      kCGImageAlphaNoneSkipLast);

    // flip and offset CTM to upper left corner
    CGContextTranslateCTM(CGContext, 0, FBitmap.Height);
    CGContextScaleCTM(CGContext, 1, -1);
  end;

  inherited Reset;
end;

function TCarbonBitmapContext.GetBitmap: TCarbonBitmap;
begin
  if FBitmap = nil then Result := nil
  else
  begin
    // update bitmap to reflect changes made to canvas
    FBitmap.Update;
    Result := FBitmap;
  end;
end;

end.
