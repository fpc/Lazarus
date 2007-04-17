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

// debugging defines
{$I carbondebug.inc}

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
  // device context data for SaveDC/RestoreDC
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
    constructor Create; // TODO
  end;

  { TCarbonControlContext }

  TCarbonControlContext = class(TCarbonDeviceContext)
  private
    FOwner: TCarbonWidget;    // owner widget
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
  
function CheckDC(const DC: HDC; const AMethodName: String; AParamName: String = ''): Boolean;

var
  // context for calculating text parameters for invisible controls
  DefaultContext: TCarbonBitmapContext;
  ScreenContext: TCarbonScreenContext;

implementation

uses CarbonProc, CarbonDbgConsts;

{------------------------------------------------------------------------------
  Name:    CheckDC
  Params:  DC          - Handle to a device context (TCarbonDeviceContext)
           AMethodName - Method name
           AParamName  - Param name
  Returns: If the DC is valid
 ------------------------------------------------------------------------------}
function CheckDC(const DC: HDC; const AMethodName: String;
  AParamName: String): Boolean;
begin
  if TObject(DC) is TCarbonDeviceContext then Result := True
  else
  begin
    Result := False;
    
    if Pos('.', AMethodName) = 0 then
      DebugLn(SCarbonWSPrefix + AMethodName + ' Error - invalid DC ' +
        AParamName + ' = ' + DbgS(DC) + '!')
    else
      DebugLn(AMethodName + ' Error - invalid DC ' + AParamName + ' = ' +
        DbgS(DC) + '!');
  end;
end;

{ TCarbonDeviceContext }

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.SetBkColor
  Params:  AValue - New background color

  Sets the background color
 ------------------------------------------------------------------------------}
procedure TCarbonDeviceContext.SetBkColor(const AValue: TColor);
begin
  if FBkColor <> AValue then
  begin
    FBkColor := AValue;
    FBkBrush.SetColor(ColorToRGB(AValue), BkMode = OPAQUE);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.SetBkMode
  Params:  AValue - New background mode (OPAQUE, TRANSPARENT)

  Sets the background mode
 ------------------------------------------------------------------------------}
procedure TCarbonDeviceContext.SetBkMode(const AValue: Integer);
begin
  if FBkMode <> AValue then
  begin
    FBkMode := AValue;
    FBkBrush.SetColor(ColorToRGB(BkColor), FBkMode = OPAQUE);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.SetCurrentBrush
  Params:  AValue - New brush

  Sets the current brush
 ------------------------------------------------------------------------------}
procedure TCarbonDeviceContext.SetCurrentBrush(const AValue: TCarbonBrush);
begin
  if FCurrentBrush <> AValue then
  begin
    FCurrentBrush := AValue;
    FCurrentBrush.Apply(Self);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.SetCurrentFont
  Params:  AValue - New font

  Sets the current font
 ------------------------------------------------------------------------------}
procedure TCarbonDeviceContext.SetCurrentFont(const AValue: TCarbonFont);
begin
  if FCurrentFont <> AValue then
  begin
    FCurrentFont := AValue;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.SetCurrentPen
  Params:  AValue - New pen

  Sets the current pen
 ------------------------------------------------------------------------------}
procedure TCarbonDeviceContext.SetCurrentPen(const AValue: TCarbonPen);
begin
  if FCurrentPen <> AValue then
  begin
    FCurrentPen := AValue;
    FCurrentPen.Apply(Self);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.SetROP2
  Params:  AValue - New binary raster operation

  Sets the binary raster operation
 ------------------------------------------------------------------------------}
procedure TCarbonDeviceContext.SetROP2(const AValue: Integer);
begin
  if FROP2 <> AValue then
  begin
    FROP2 := AValue;
    CurrentPen.Apply(Self);
    CurrentBrush.Apply(Self);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.SetTextColor
  Params:  AValue - New text color

  Sets the text color
 ------------------------------------------------------------------------------}
procedure TCarbonDeviceContext.SetTextColor(const AValue: TColor);
begin
  if FTextColor <> AValue then
  begin
    FTextColor := AValue;
    TextBrush.SetColor(ColorToRGB(AValue), True);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.Create

  Creates new Carbon device context
 ------------------------------------------------------------------------------}
constructor TCarbonDeviceContext.Create;
begin
  FBkBrush := TCarbonBrush.Create;
  FTextBrush := TCarbonBrush.Create;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.Destroy

  Frees Carbon device context
 ------------------------------------------------------------------------------}
destructor TCarbonDeviceContext.Destroy;
begin
  BkBrush.Free;
  TextBrush.Free;
  
  FSavedDCList.Free;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.Reset

  Resets the device context properties to defaults (pen, brush, ...)
 ------------------------------------------------------------------------------}
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
    {$IFDEF VerboseCanvas}
      DebugLn('TCarbonDeviceContext.Reset set defaults');
    {$ENDIF}
    
    // enable anti-aliasing
    CGContextSetShouldAntialias(CGContext, 1);
    CGContextSetBlendMode(CGContext, kCGBlendModeNormal);
    
    CGContextSetRGBFillColor(CGContext, 1, 1, 1, 1);
    CGContextSetRGBStrokeColor(CGContext, 0, 0, 0, 1);
    CGContextSetLineWidth(CGContext, 1);
    CGContextSetLineDash(CGContext, 0, nil, 0);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.SaveDC
  Returns: Index of saved device context state
  
  Note: must be used in pair with RestoreDC!
 ------------------------------------------------------------------------------}
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
  
  {$IFDEF VerboseCanvas}
    DebugLn('TCarbonDeviceContext.SaveDC Result: ', DbgS(Result));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.RestoreDC
  Params:  ASavedDC - Index of saved device context
  Returns: If the function succeeds
  
  Restores the previously saved state of device context
 ------------------------------------------------------------------------------}
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
  
  {$IFDEF VerboseCanvas}
    DebugLn('TCarbonDeviceContext.RestoreDC SavedDC: ', DbgS(ASavedDC));
  {$ENDIF}
  
  CGContextRestoreGState(CGContext);
  RestoreDCData(TCarbonDCData(FSavedDCList[ASavedDC - 1]));
  FSavedDCList.Delete(ASavedDC - 1);
  Result := True;
  
  {$IFDEF VerboseCanvas}
    DebugLn('TCarbonDeviceContext.RestoreDC End');
  {$ENDIF}
  
  if FSavedDCList.Count = 0 then FreeAndNil(FSavedDCList);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.SaveDCData
  Returns: The device context data
 ------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.RestoreDCData
  Params:  AData - Device context data
  
  Restores device context data
 ------------------------------------------------------------------------------}
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
const
  SName = 'BeginTextRender';
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
  if OSError(ATSUCreateTextLayoutWithTextPtr(ConstUniCharArrayPtr(@W[1]),
      kATSUFromTextBeginning, kATSUToTextEnd, Length(W), 1, @TextLength,
      @TextStyle, ALayout), Self, SName, 'ATSUCreateTextLayoutWithTextPtr') then Exit;
      
  // set layout context
  Tag := kATSUCGContextTag;
  DataSize := SizeOf(CGContextRef);

  PContext := @CGContext;
  if OSError(ATSUSetLayoutControls(ALayout, 1, @Tag, @DataSize, @PContext),
    Self, SName, 'ATSUSetLayoutControls') then Exit;
    
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDeviceContext.EndTextRender
  Params:  ALayout - ATSU layout

  Frees the ATSU text layout and manages the device
  context to render ordinary graphic
 ------------------------------------------------------------------------------}
procedure TCarbonDeviceContext.EndTextRender(var ALayout: ATSUTextLayout);
begin
  // restore context
  CGContextRestoreGState(CGContext);

  if ALayout <> nil then
    OSError(ATSUDisposeTextLayout(ALayout), Self, 'EndTextRender', 'ATSUDisposeTextLayout');
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

{------------------------------------------------------------------------------
  Method:  TCarbonScreenContext.GetSize
  Returns: Size of screen context
 ------------------------------------------------------------------------------}
function TCarbonScreenContext.GetSize: TPoint;
begin
  Result.X := CGDisplayPixelsWide(CGMainDisplayID);
  Result.Y := CGDisplayPixelsHigh(CGMainDisplayID);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonScreenContext.Create

  Creates new screen context
 ------------------------------------------------------------------------------}
constructor TCarbonScreenContext.Create;
begin
  inherited Create;

  Reset;
end;

{ TCarbonControlContext }

{------------------------------------------------------------------------------
  Method:  TCarbonControlContext.GetSize
  Returns: Size of control context
 ------------------------------------------------------------------------------}
function TCarbonControlContext.GetSize: TPoint;
begin
  Result.X := (FOwner.LCLObject as TControl).ClientWidth;
  Result.Y := (FOwner.LCLObject as TControl).ClientHeight;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControlContext.Create
  Params:  AOwner - Context widget

  Creates new control context
 ------------------------------------------------------------------------------}
constructor TCarbonControlContext.Create(AOwner: TCarbonWidget);
begin
  inherited Create;

  FOwner := AOwner;
  Reset;
end;

{ TCarbonBitmapContext }

{------------------------------------------------------------------------------
  Method:  TCarbonBitmapContext.SetBitmap
  Params:  AValue - New bitmap

  Sets the bitmap
 ------------------------------------------------------------------------------}
procedure TCarbonBitmapContext.SetBitmap(const AValue: TCarbonBitmap);
begin
  //DebugLn('TCarbonBitmapContext.SetBitmap');
  if FBitmap <> AValue then
  begin
    FBitmap := AValue;
    Reset;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBitmapContext.GetSize
  Returns: Size of bitmap context
 ------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------
  Method:  TCarbonBitmapContext.Create

  Creates new bitmap context
 ------------------------------------------------------------------------------}
constructor TCarbonBitmapContext.Create;
begin
  inherited Create;
  FBitmap := nil;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBitmapContext.Destroy

  Frees bitmap context
 ------------------------------------------------------------------------------}
destructor TCarbonBitmapContext.Destroy;
begin
  if CGContext <> nil then CGContextRelease(CGContext);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBitmapContext.Reset

  Resets the bitmap context properties to defaults (pen, brush, ...)
 ------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------
  Method:  TCarbonBitmapContext.GetBitmap
  Returns: The bitmap of bitmap context
 ------------------------------------------------------------------------------}
function TCarbonBitmapContext.GetBitmap: TCarbonBitmap;
begin
  if FBitmap = nil then Result := nil
  else
  begin
    // update bitmap to reflect changes made via canvas
    FBitmap.Update;
    Result := FBitmap;
  end;
end;

end.
