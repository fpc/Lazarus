{ $Id$
                  ----------------------------------------
                  carbonproc.pp  -  Carbon interface procs
                  ----------------------------------------

 @created(Wed Aug 26st WET 2005)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains procedures/functions needed for the Carbon <-> LCL interface
 Common carbon untilities (usable by other projects) go to CarbonUtils

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

unit CarbonProc;
{$mode objfpc}{$H+}

interface

uses
  FPCMacOSAll, Classes, LCLType, LCLProc, LCLClasses, LMessages,
  Controls, Forms, Avl_Tree, SysUtils, Graphics, Math,
  CarbonDef;
  

type
  TConvertResult = (trNoError, trNullSrc, trNullDest, trDestExhausted,
    trInvalidChar, trUnfinishedChar);

  TConvertOption = (toInvalidCharError, toInvalidCharToSymbol,
    toUnfinishedCharError, toUnfinishedCharToSymbol);
  TConvertOptions = set of TConvertOption;
  
function UTF8ToUTF16(const S: UTF8String): WideString;

function GetCtrlWidgetInfo(AWidget: Pointer): TCarbonWidgetInfo;
function GetWndWidgetInfo(AWidget: Pointer): TCarbonWidgetInfo;
function GetWidgetInfo(AWidget: Pointer): TCarbonWidgetInfo;
function GetWidgetType(AWidget: Pointer): TCarbonWidgetType;
function GetWidgetType(AWidget: Pointer; var AInfo: TCarbonWidgetInfo): TCarbonWidgetType;

function GetTopParentWindow(AWinControl: TWinControl): WindowRef;
function GetCarbonLocalWindowRect(Handle: hwnd; var ARect: TRect; Info: TCarbonWidgetInfo = nil): Boolean;
function GetCarbonClientRect(Handle: hwnd; var ARect: TRect; Info: TCarbonWidgetInfo = nil): Boolean;

procedure InvalidateCarbonControl(AControl: HWnd); inline;
function GetCarbonWindowContent(AWindow: HWnd): HIViewRef; inline;

function FindCarbonFontID(const FontName: String): ATSUFontID;

function GetEditControlText(AControl: HWnd; var AText: String): Boolean;
function SetEditControlText(AControl: HWnd; const AText: String): Boolean;

function GetEditControlSelStart(AControl: HWnd; var ASelStart: Integer): Boolean;
function GetEditControlSelLength(AControl: HWnd; var ASelLength: Integer): Boolean;
function SetEditControlSelStart(AControl: HWnd; ASelStart: Integer): Boolean;
function SetEditControlSelLength(AControl: HWnd; ASelLength: Integer): Boolean;

function BeginTextRender(DC: TCarbonDeviceContext; AStr: PChar; ACount: Integer; out ALayout: ATSUTextLayout): Boolean;
procedure EndTextRender(DC: TCarbonDeviceContext; var ALayout: ATSUTextLayout);

function RegisterEventHandler(AHandler: TCarbonWSEventHandlerProc): EventHandlerUPP;
procedure UnRegisterEventHandler(AHandler: TCarbonWSEventHandlerProc);

procedure CreateCarbonString(const S: String; out AString: CFStringRef); inline;
procedure FreeCarbonString(var AString: CFStringRef); inline;
function CarbonStringToString(AString: CFStringRef): String;

function GetCarbonRect(Left, Top, Width, Height: Integer): FPCMacOSAll.Rect;
function GetCarbonRect(const ARect: TRect): FPCMacOSAll.Rect;
function ParamsToCarbonRect(const AParams: TCreateParams): FPCMacOSAll.Rect;
function GetCGRect(X1, Y1, X2, Y2: Integer): CGRect;
function RectToCGRect(const ARect: TRect): CGRect;
function CGRectToRect(const ARect: CGRect): TRect;
function ParamsToHIRect(const AParams: TCreateParams): HIRect;
function CarbonRectToRect(const ARect: FPCMacOSAll.Rect): TRect;

function ColorToCarbonColor(const AColor: TColor): RGBColor;
function CarbonColorToColor(const AColor: RGBColor): TColor; inline;
function CreateCGColor(const AColor: TColor): CGColorRef;

function Dbgs(const ARect: FPCMacOSAll.Rect): string; overload;
function Dbgs(const AColor: FPCMacOSAll.RGBColor): string; overload;

implementation

uses CarbonInt;

{------------------------------------------------------------------------------
  Name:    ConvertUTF8ToUTF16
  Params:  Dest                - Pointer to destination string
           DestWideCharCount   - Wide char count allocated in destination string
           Src                 - Pointer to source string
           SrcCharCount        - Char count allocated in source string
           Options             - Conversion options, if none is set, both
             invalid and unfinished UTF-8 chars are skipped

             toInvalidCharError       - Stop on invalid UTF-8 char and report
                                      error
             toInvalidCharToSymbol    - Replace invalid UTF-8 chars with '?'
             toUnfinishedCharError    - Stop on unfinished UTF-8 char and report
                                      error
             toUnfinishedCharToSymbol - Replace unfinished UTF-8 char with '?'

           ActualWideCharCount - Actual wide char count converted from source
                               string to destination string
  Returns:
    trNoError        - The string was successfully converted without
                     any error
    trNullSrc        - Pointer to source string is nil
    trNullDest       - Pointer to destination string is nil
    trDestExhausted  - Destination buffer size is not big enough to hold
                     converted string
    trInvalidChar    - Invalid UTF-8 char has occured
    trUnfinishedChar - Unfinished UTF-8 char has occured

  Converts the specified UTF-8 encoded string to UTF-16 encoded
 ------------------------------------------------------------------------------}
function ConvertUTF8ToUTF16(Dest: PWideChar; DestWideCharCount: SizeUInt;
  Src: PChar; SrcCharCount: SizeUInt; Options: TConvertOptions;
  out ActualWideCharCount: SizeUInt): TConvertResult;
var
  DestI, SrcI: SizeUInt;
  B1, B2, B3, B4: Byte;
  W: Word;
  C: Cardinal;

  function UnfinishedCharError: Boolean;
  begin
    if toUnfinishedCharToSymbol in Options then
    begin
      Dest[DestI] := System.WideChar('?');
      Inc(DestI);
      Result := False;
    end
    else
      if toUnfinishedCharError in Options then
      begin
        ConvertUTF8ToUTF16 := trUnfinishedChar;
        Result := True;
      end
      else Result := False;
  end;

  function InvalidCharError(Count: Integer): Boolean; inline;
  begin
    if toInvalidCharToSymbol in Options then
    begin
      Dest[DestI] := System.WideChar('?');
      Inc(DestI);
      Dec(SrcI, Count);
      Result := False;
    end
    else
      if toInvalidCharError in Options then
      begin
        ConvertUTF8ToUTF16 := trUnfinishedChar;
        Result := True;
      end
      else
      begin
        Dec(SrcI, Count);
        Result := False;
      end;
  end;

begin
  ActualWideCharCount := 0;

  if not Assigned(Src) then
  begin
    Result := trNullSrc;
    Exit;
  end;

  if not Assigned(Dest) then
  begin
    Result := trNullDest;
    Exit;
  end;
  SrcI := 0;
  DestI := 0;

  while (DestI < DestWideCharCount) and (SrcI < SrcCharCount) do
  begin
    B1 := Byte(Src[SrcI]);
    Inc(SrcI);

    if B1 < 128 then // single byte UTF-8 char
    begin
      Dest[DestI] := System.WideChar(B1);
      Inc(DestI);
    end
    else
    begin
      if SrcI >= SrcCharCount then
        if UnfinishedCharError then Exit
        else Break;

      B2 := Byte(Src[SrcI]);
      Inc(SrcI);

      if (B1 and %11100000) = %11000000 then // double byte UTF-8 char
      begin
        if (B2 and %11000000) = %10000000 then
        begin
          Dest[DestI] := System.WideChar(((B1 and %00011111) shl 6) or (B2 and %00111111));
          Inc(DestI);
        end
        else // invalid character, assume single byte UTF-8 char
          if InvalidCharError(1) then Exit;
      end
      else
      begin
        if SrcI >= SrcCharCount then
          if UnfinishedCharError then Exit
          else Break;

        B3 := Byte(Src[SrcI]);
        Inc(SrcI);

        if (B1 and %11110000) = %11100000 then // triple byte UTF-8 char
        begin
          if ((B2 and %11000000) = %10000000) and ((B3 and %11000000) = %10000000) then
          begin
            W := ((B1 and %00011111) shl 12) or ((B2 and %00111111) shl 6) or (B3 and %00111111);
            if W < $D800 then // to single wide char UTF-16 char
            begin
              Dest[DestI] := System.WideChar(W);
              Inc(DestI);
            end
            else // to double wide char UTF-16 char
            begin
              Dest[DestI] := System.WideChar($D800 or (W shr 10));
              Inc(DestI);
              if DestI >= DestWideCharCount then Break;
              Dest[DestI] := System.WideChar($DC00 or (W and %0000001111111111));
              Inc(DestI);
            end;
          end
          else // invalid character, assume single byte UTF-8 char
            if InvalidCharError(2) then Exit;
        end
        else
        begin
          if SrcI >= SrcCharCount then
            if UnfinishedCharError then Exit
            else Break;

          B4 := Byte(Src[SrcI]);
          Inc(SrcI);

          if ((B1 and %11111000) = %11110000) and ((B2 and %11000000) = %10000000)
            and ((B3 and %11000000) = %10000000) and ((B4 and %11000000) = %10000000) then
          begin // 4 byte UTF-8 char
            C := ((B1 and %00011111) shl 18) or ((B2 and %00111111) shl 12)
              or ((B3 and %00111111) shl 6)  or (B4 and %00111111);
            // to double wide char UTF-16 char
            Dest[DestI] := System.WideChar($D800 or (C shr 10));
            Inc(DestI);
            if DestI >= DestWideCharCount then Break;
            Dest[DestI] := System.WideChar($DC00 or (C and %0000001111111111));
            Inc(DestI);
          end
          else // invalid character, assume single byte UTF-8 char
            if InvalidCharError(3) then Exit;
        end;
      end;
    end;
  end;

  if DestI >= DestWideCharCount then
  begin
    DestI := DestWideCharCount - 1;
    Result := trDestExhausted;
  end
  else
    Result := trNoError;

  Dest[DestI] := #0;
  ActualWideCharCount := DestI + 1;
end;

{------------------------------------------------------------------------------
  Name:    UTF8ToUTF16
  Params:  S - Source UTF-8 string
  Returns: UTF-16 encoded string

  Converts the specified UTF-8 encoded string to UTF-16 encoded
 ------------------------------------------------------------------------------}
function UTF8ToUTF16(const S: UTF8String): WideString;
var
  L: SizeUInt;
  R: WideString;
begin
  Result := '';
  if S = '' then Exit;

  SetLength(R, Length(S)); // bytes of UTF-8 string >= wide chars of UTF-16
  if ConvertUTF8ToUTF16(PWideChar(R), Length(R) + 1, PChar(S), Length(S),
    [toInvalidCharToSymbol], L) = trNoError then
  begin
    SetLength(R, L - 1);
    Result := R;
  end;
end;

{------------------------------------------------------------------------------
  Name:    GetCtrlWidgetInfo
  Params:  AWidget - Pointer to control widget
  Returns: The Carbon control widget info

  Retrieves basic info for specified control widget
 ------------------------------------------------------------------------------}
function GetCtrlWidgetInfo(AWidget: Pointer): TCarbonWidgetInfo;
begin
  if GetControlProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC,
    SizeOf(TCarbonWidgetInfo), nil, @Result) <> noErr then Result := nil;
end;

{------------------------------------------------------------------------------
  Name:    GetWndWidgetInfo
  Params:  AWidget - Pointer to window widget
  Returns: The Carbon window widget info

  Retrieves basic info for specified window widget
 ------------------------------------------------------------------------------}
function GetWndWidgetInfo(AWidget: Pointer): TCarbonWidgetInfo;
begin
  if GetWindowProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC,
    SizeOf(TCarbonWidgetInfo), nil, @Result) <> noErr then Result := nil;
end;

{------------------------------------------------------------------------------
  Name:    GetWidgetInfo
  Params:  AWidget - Pointer to widget
  Returns: The Carbon widget info

  Retrieves basic info for specified widget (control or window)
 ------------------------------------------------------------------------------}
function GetWidgetInfo(AWidget: Pointer): TCarbonWidgetInfo;
begin
  if AWidget = nil then
  begin
    Result := nil;
    Exit;
  end;
  
  if IsValidControlHandle(AWidget) then Result := GetCtrlWidgetInfo(AWidget)
  else
    // there is no (cheap) check for windows so assume a window
    // when it is not a control.
    Result := GetWndWidgetInfo(AWidget);
end;

{------------------------------------------------------------------------------
  Name:    GetWidgetType
  Params:  AWidget - Pointer to widget
  Returns: Widget type

  Retrieves the type of specified widget (Control or Window)
 ------------------------------------------------------------------------------}
function GetWidgetType(AWidget: Pointer): TCarbonWidgetType;
var
  AInfo: TCarbonWidgetInfo;
begin
  Result := cwtUnknown;
  
  AInfo := GetWidgetInfo(AWidget);
  if AInfo = nil then Exit;
  
  Result := AInfo.WidgetType;
end;

{------------------------------------------------------------------------------
  Name:    GetWidgetType
  Params:  AWidget - Pointer to widget
           AInfo   - Carbon widget info
  Returns: Widget type

  Retrieves the type of specified widget (Control or Window) according to
  passed info. If info is not set then it is retrieved from widget.
 ------------------------------------------------------------------------------}
function GetWidgetType(AWidget: Pointer; var AInfo: TCarbonWidgetInfo): TCarbonWidgetType;
begin
  Result := cwtUnknown;
  if AInfo = nil then AInfo := GetWidgetInfo(AWidget);
  if AInfo = nil then Exit;

  Result := AInfo.WidgetType;
end;

//=====================================================
// UPP mamanger
//=====================================================
type
  TUPPAVLTreeNode = class(TAVLTreeNode)
  public
    UPP: EventHandlerUPP;
    RefCount: Integer;
    procedure Clear; reintroduce; // not overridable, so reintroduce since we only will call this clear
    destructor Destroy; override;
  end;

var
  UPPTree: TAVLTree = nil;

procedure TUPPAVLTreeNode.Clear;
begin
  if UPP <> nil
  then begin
    DisposeEventHandlerUPP(UPP);
    UPP := nil;
  end;
  inherited Clear;
end;

destructor TUPPAVLTreeNode.Destroy;
begin
  if UPP <> nil
  then begin
    DisposeEventHandlerUPP(UPP);
    UPP := nil;
  end;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Name:    GetTopParentWindow
  Params:  AWinControl - Window control
  Returns: Window reference

  Retrieves the parent window reference of the control
 ------------------------------------------------------------------------------}
function GetTopParentWindow(AWinControl: TWinControl): WindowRef;
var
  Window: TControl;
begin
  if AWinControl = nil then
  begin
    Result := nil;
    Exit;
  end;
   
  Window := AWinControl.GetTopParent;

  if Window is TCustomForm then Result := WindowRef((Window as TCustomForm).Handle)
  else Result := nil;
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonLocalWindowRect
  Params:  Handle - Handle of window
           ARect  - Record for window coordinates
           Info   - Carbon widget info (optional)
  Returns: If function succeeds

  Returns the window bounding rectangle relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function GetCarbonLocalWindowRect(Handle: hwnd; var ARect: TRect; Info: TCarbonWidgetInfo): Boolean;
var
  AWndRect: FPCMacOSAll.Rect;
begin
  Result := False;
  
  case GetWidgetType(Pointer(Handle), Info) of
  cwtWindowRef:
    Result := FPCMacOSAll.GetWindowBounds(WindowRef(Handle), kWindowStructureRgn, AWndRect) = noErr;
  cwtControlRef:
    Result := FPCMacOSAll.GetControlBounds(ControlRef(Handle), AWndRect) <> nil;
  end;
  
  if not Result then Exit;
  ARect := CarbonRectToRect(AWndRect);
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonClientRect
  Params:  Handle - Handle of window
           ARect  - Record for client area coordinates
           Info   - Carbon widget info (optional)
  Returns: If function succeeds

  Returns the window client rectangle relative to the client area parent
  window origin
 ------------------------------------------------------------------------------}
function GetCarbonClientRect(Handle: hwnd; var ARect: TRect; Info: TCarbonWidgetInfo): Boolean;
var
  AWndRect, AClientRect: FPCMacOSAll.Rect;
  OSResult: OSStatus;
  ClientRegion: FPCMacOSAll.RgnHandle;
begin
  Result := False;
  
  case GetWidgetType(Pointer(Handle), Info) of
  cwtWindowRef:
  begin
    Result := FPCMacOSAll.GetWindowBounds(WindowRef(Handle), kWindowStructureRgn, AWndRect) = noErr;
    if Result then
    begin
      Result := FPCMacOSAll.GetWindowBounds(WindowRef(Handle), kWindowContentRgn, AClientRect) = noErr;
    end;
    if Result then
    begin
      ARect.Left := AClientRect.Left - AWndRect.Left;
      ARect.Top := AClientRect.Top - AWndRect.Top;
      ARect.Right := AClientRect.Right - AWndRect.Left;
      ARect.Bottom := AClientRect.Bottom - AWndRect.Top;
    end;
  end;
  cwtControlRef:
  begin
    ClientRegion := FPCMacOSAll.NewRgn();
    try
      OSResult := GetControlRegion(ControlRef(Handle), kControlContentMetaPart,
        ClientRegion);

      if OSResult = errInvalidPartCode then
      begin
        // controls without content area have clientrect = boundsrect
        Result := FPCMacOSAll.GetControlBounds(ControlRef(Handle), AClientRect) <> nil;
        if Result then
        begin
          ARect := CarbonRectToRect(AClientRect);
          OffsetRect(ARect, -ARect.Left, -ARect.Top);
        end;
      end
      else
      begin
        Result := OSResult = noErr;

        if Result then
        begin
          Result := GetRegionBounds(ClientRegion, AClientRect) <> nil;
          if Result then ARect := CarbonRectToRect(AClientRect);
        end;
      end;
    finally
      FPCMacOSAll.DisposeRgn(ClientRegion);
    end;
  end;
  end;
end;

{------------------------------------------------------------------------------
  Name:    InvalidateCarbonControl
  Params:  AControl - Handle of control
  Returns: Nothing
  
  Invalidates specified control
 ------------------------------------------------------------------------------}
procedure InvalidateCarbonControl(AControl: HWnd);
begin
  HiViewSetNeedsDisplay(HIViewRef(AControl), True);
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonWindowContent
  Params:  AWindow - Handle of window
  Returns: Carbon window content

  Returns the Carbon window content for the specified window
 ------------------------------------------------------------------------------}
function GetCarbonWindowContent(AWindow: HWnd): HIViewRef;
begin
  if HIViewFindByID(HIViewGetRoot(WindowRef(AWindow)), kHIViewWindowContentID,
    Result) <> noErr then Result := nil;
end;

{------------------------------------------------------------------------------
  Name:    FindCarbonFontID
  Params:  FontName - The font name
  Returns: Caron font ID

  Finds ID of specified font
 ------------------------------------------------------------------------------}
function FindCarbonFontID(const FontName: String): ATSUFontID;
begin
  Result := 0;

  if (FontName <> '') and not SameText(FontName, 'default') then
  begin
    ATSUFindFontFromName(@FontName[1], Length(FontName), kFontFamilyName,
      kFontMacintoshPlatform, kFontRomanScript, kFontEnglishLanguage, Result);
  end;
end;

{------------------------------------------------------------------------------
  Name:    GetEditControlText
  Params:  AControl - Handle of edit control
           AText    - Record for text
  Returns: If the function suceeds

  Gets the text from the Carbon edit control
 ------------------------------------------------------------------------------}
function GetEditControlText(AControl: HWnd; var AText: String): Boolean;
var
  Info: TCarbonWidgetInfo;
  CFString: CFStringRef;
begin
  Result := False;
  Info := GetCtrlWidgetInfo(Pointer(AControl));
  if Info = nil then Exit;
  
  if (Info.UserData <> nil) and (PBoolean(Info.UserData)^ = True) then
  begin // IsPassword
    if GetControlData(ControlRef(AControl), kControlEntireControl,
      kControlEditTextPasswordCFStringTag, SizeOf(CFStringRef),
      @CFString, nil) <> noErr then Exit;
  end
  else
  begin
    CFString := HIViewCopyText(HIViewRef(AControl));
    if CFString = nil then Exit;
  end;
  
  try
    AText := CarbonStringToString(CFString);
    Result := True;
  finally
    FreeCarbonString(CFString);
  end;
end;

{------------------------------------------------------------------------------
  Name:    SetEditControlText
  Params:  AControl - Handle of edit control
           AText    - Text to set
  Returns:  If the function suceeds

  Sets the text of the Carbon edit control
 ------------------------------------------------------------------------------}
function SetEditControlText(AControl: HWnd; const AText: String): Boolean;
var
  Info: TCarbonWidgetInfo;
  CFString: CFStringRef;
begin
  Result := False;
  Info := GetCtrlWidgetInfo(Pointer(AControl));
  if Info = nil then Exit;

  CreateCarbonString(AText, CFString);
  try
    if (Info.UserData <> nil) and (PBoolean(Info.UserData)^ = True) then
      // IsPassword
      Result := SetControlData(ControlRef(AControl), kControlEntireControl,
        kControlEditTextPasswordCFStringTag, SizeOf(CFStringRef), @CFString) = noErr
    else
      Result := HIViewSetText(HIViewRef(AControl), CFString) = noErr;
  finally
    FreeCarbonString(CFString);
  end;
end;

{------------------------------------------------------------------------------
  Name:    GetEditControlSelStart
  Params:  AControl   - Handle of edit control
           ASelStart  - Selection start
  Returns: If the function suceeds

  Gets the selection start from the Carbon edit control
 ------------------------------------------------------------------------------}
function GetEditControlSelStart(AControl: HWnd; var ASelStart: Integer): Boolean;
var
  SelData: ControlEditTextSelectionRec;
begin
  Result := GetControlData(ControlRef(AControl), kControlEntireControl,
    kControlEditTextSelectionTag, SizeOf(ControlEditTextSelectionRec),
    @SelData, nil) = noErr;

  if Result then ASelStart := SelData.SelStart;
end;

{------------------------------------------------------------------------------
  Name:    GetEditControlSelLength
  Params:  AControl   - Handle of edit control
           ASelLength - Selection length
  Returns: If the function suceeds

  Gets the selection length from the Carbon edit control
 ------------------------------------------------------------------------------}
function GetEditControlSelLength(AControl: HWnd; var ASelLength: Integer): Boolean;
var
  SelData: ControlEditTextSelectionRec;
begin
  Result := GetControlData(ControlRef(AControl), kControlEntireControl,
    kControlEditTextSelectionTag, SizeOf(ControlEditTextSelectionRec),
    @SelData, nil) = noErr;

  if Result then ASelLength := SelData.SelEnd - SelData.SelStart;
end;

{------------------------------------------------------------------------------
  Name:    SetEditControlSelStart
  Params:  AControl   - Handle of edit control
           ASelStart  - Selection start
  Returns: If the function suceeds

  Sets the selection start of the Carbon edit control
 ------------------------------------------------------------------------------}
function SetEditControlSelStart(AControl: HWnd; ASelStart: Integer): Boolean;
var
  SelData: ControlEditTextSelectionRec;
begin
  Result := GetControlData(ControlRef(AControl), kControlEntireControl,
    kControlEditTextSelectionTag, SizeOf(ControlEditTextSelectionRec),
    @SelData, nil) = noErr;

  if Result then
  begin
    if SelData.SelStart = ASelStart then Exit;
  
    SelData.SelEnd := (SelData.SelEnd - SelData.SelStart) + ASelStart;
    SelData.SelStart := ASelStart;
    Result := SetControlData(ControlRef(AControl), kControlEntireControl,
      kControlEditTextSelectionTag, SizeOf(ControlEditTextSelectionRec),
      @SelData) = noErr;
  end;
end;

{------------------------------------------------------------------------------
  Name:    SetEditControlSelLength
  Params:  AControl   - Handle of edit control
           ASelLength - Selection length
  Returns: If the function suceeds

  Sets the selection length of the Carbon edit control
 ------------------------------------------------------------------------------}
function SetEditControlSelLength(AControl: HWnd; ASelLength: Integer): Boolean;
var
  SelData: ControlEditTextSelectionRec;
begin
  Result := GetControlData(ControlRef(AControl), kControlEntireControl,
    kControlEditTextSelectionTag, SizeOf(ControlEditTextSelectionRec),
    @SelData, nil) = noErr;

  if Result then
  begin
    if SelData.SelEnd = SelData.SelStart + ASelLength then Exit;
    
    SelData.SelEnd := SelData.SelStart + ASelLength;
    Result := SetControlData(ControlRef(AControl), kControlEntireControl,
      kControlEditTextSelectionTag, SizeOf(ControlEditTextSelectionRec),
      @SelData) = noErr;
  end;
end;

{------------------------------------------------------------------------------
  Name:    BeginTextRender
  Params:  DC      - Carbon device context
           AStr    - UTF8 string to render
           ACount  - Count of chars to render
           ALayout - ATSU layout
  Returns: If the function suceeds

  Creates the ATSU text layout for the specified text and manages the device
  context to render the text
 ------------------------------------------------------------------------------}
function BeginTextRender(DC: TCarbonDeviceContext; AStr: PChar;
  ACount: Integer; out ALayout: ATSUTextLayout): Boolean;
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
  if DC = nil then Exit;

  // save context
  CGContextSaveGState(DC.CGContext);
  
  // convert UTF-8 string to UTF-16 string
  if ACount < 0 then S := AStr
  else S := Copy(AStr, 1, ACount);
  W := UTF8ToUTF16(S);

  if not CarbonWidgetSet.IsValidGDIObject(HFONT(DC.CurrentFont)) then
    TextStyle := DefaultTextStyle
  else
    TextStyle := DC.CurrentFont.Style;
    
  // apply text color and bk color
  DC.TextPen.Apply(DC, False); // do not use ROP2
  DC.BkBrush.Apply(DC, False);

  // create text layout
  TextLength := kATSUToTextEnd;
  if ATSUCreateTextLayoutWithTextPtr(ConstUniCharArrayPtr(@W[1]),
    kATSUFromTextBeginning, kATSUToTextEnd, Length(W), 1, @TextLength, @TextStyle,
    ALayout) = noErr then
  begin
    // set layout context
    Tag := kATSUCGContextTag;
    DataSize := SizeOf(CGContextRef);

    PContext := Pointer(DC.CGContext);
    Result := ATSUSetLayoutControls(ALayout, 1, @Tag, @DataSize, @PContext) = noErr;
  end;
end;

{------------------------------------------------------------------------------
  Name:    EndTextRender
  Params:  DC      - Carbon device context
           ALayout - ATSU layout
  Returns: Nothing

  Frees the ATSU text layout and manages the device
  context to render ordinary graphic
 ------------------------------------------------------------------------------}
procedure EndTextRender(DC: TCarbonDeviceContext; var ALayout: ATSUTextLayout);
begin
  if DC <> nil then
    // restore context
    CGContextRestoreGState(DC.CGContext);
    
  if ALayout <> nil then ATSUDisposeTextLayout(ALayout);
end;

{------------------------------------------------------------------------------
  Name:    RegisterEventHandler
  Params:  AHandler - Carbon event handler procedure
  Returns: Event handler UPP

  Registers new carbon event handler procedure
 ------------------------------------------------------------------------------}
function RegisterEventHandler(AHandler: TCarbonWSEventHandlerProc): EventHandlerUPP;
var
  Node: TUPPAVLTreeNode;
begin
  if UPPTree = nil then UPPTree := TAVLTree.Create;
  
  Node := TUPPAVLTreeNode(UPPTree.Find(AHandler));
  if Node = nil then
  begin
    Node := TUPPAVLTreeNode.Create;
    Node.Data := AHandler;
    Node.UPP := NewEventHandlerUPP(EventHandlerProcPtr(AHandler));
    UPPTree.Add(Node);
  end;
  
  Inc(Node.Refcount);
  Result := Node.UPP;
end;

{------------------------------------------------------------------------------
  Name:    UnRegisterEventHandler
  Params:  AHandler - Carbon event handler procedure
  Returns: Nothing

  Unregisters event handler procedure
 ------------------------------------------------------------------------------}
procedure UnRegisterEventHandler(AHandler: TCarbonWSEventHandlerProc);
var
  Node: TUPPAVLTreeNode;
begin
  if UPPTree = nil then Exit; //???
  Node := TUPPAVLTreeNode(UPPTree.Find(AHandler));
  if Node = nil then Exit; //???
  if Node.Refcount <= 0 then
  begin
    DebugLn('[UnRegisterEventHandler] UPPInconsistency, Node.RefCount <= 0');
    Exit;
  end;

  Dec(Node.Refcount);
  if Node.Refcount > 0 then Exit;

  // Sigh !
  // there doesn't exist a light version of the avltree without buildin memmanager
  // So, just free it and "pollute" the memmanager with our classes;
  // Freeing our node is also not an option, since that would
  // corrupt the tree (no handling for that).
  // Tweaking the memmanager is also not possible since only the class is public
  // and not the manager itself.

  Node.Clear;
  UPPTree.Delete(Node);
end;

{------------------------------------------------------------------------------
  Name:    CreateCarbonString
  Params:  S       - UTF-8 string
           AString - Core Foundation string ref
  Returns: Nothing

  Creates new Core Foundation string form specified string
 ------------------------------------------------------------------------------}
procedure CreateCarbonString(const S: String; out AString: CFStringRef);
begin
  AString := CFStringCreateWithCString(nil, Pointer(PChar(S)), DEFAULT_CFSTRING_ENCODING);
end;

{------------------------------------------------------------------------------
  Name:    FreeCarbonString
  Params:  AString - Core Foundation string ref to free
  Returns: Nothing

  Frees specified Core Foundation string
 ------------------------------------------------------------------------------}
procedure FreeCarbonString(var AString: CFStringRef);
begin
  if AString <> nil then
    CFRelease(Pointer(AString));
end;

{------------------------------------------------------------------------------
  Name:    CarbonStringToString
  Params:  AString - Core Foundation string ref
  Returns: UTF-8 string

  Converts Core Foundation string to string
 ------------------------------------------------------------------------------}
function CarbonStringToString(AString: CFStringRef): String;
var
  Str: Pointer;
  StrSize: CFIndex;
begin
  if AString = nil then
  begin
    Result := '';
    Exit;
  end;

  // Try the quick way first
  Str := CFStringGetCStringPtr(AString, DEFAULT_CFSTRING_ENCODING);
  if Str <> nil then
    Result := PChar(Str)
  else
  begin
    // if that doesn't work this will
    StrSize := CFStringGetLength(AString) + 1; // + 1 for null terminator
    GetMem(Str, StrSize);
    try
      CFStringGetCString(AString, Str, StrSize, DEFAULT_CFSTRING_ENCODING);
      Result := PChar(Str);
    finally
      System.FreeMem(Str);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonRect
  Params:  Left, Top, Width, Height - coordinates
  Returns: Carbon Rect

  Carbon Rect constructor
 ------------------------------------------------------------------------------}
function GetCarbonRect(Left, Top, Width, Height: Integer): FPCMacOSAll.Rect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonRect
  Params:  ARect - Rectangle
  Returns: Carbon Rect

  Carbon Rect constructor
 ------------------------------------------------------------------------------}
function GetCarbonRect(const ARect: TRect): FPCMacOSAll.Rect;
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Right := ARect.Right;
  Result.Bottom := ARect.Bottom;
end;

{------------------------------------------------------------------------------
  Name:    ParamsToCarbonRect
  Params:  AParams - Creation parameters
  Returns: Carbon Rect

  Carbon Rect constructor from creation parameters
 ------------------------------------------------------------------------------}
function ParamsToCarbonRect(const AParams: TCreateParams): FPCMacOSAll.Rect;
begin
  Result.Left := AParams.X;
  Result.Top := AParams.Y;
  Result.Right := AParams.X + AParams.Width;
  Result.Bottom := AParams.Y + AParams.Height;
end;

{------------------------------------------------------------------------------
  Name:    RectToCGRect
  Params:  X1, Y1, X2, Y2 - Rectangle coordinates
  Returns: CGRect

  CGRect constructor, coordinates are sorted
 ------------------------------------------------------------------------------}
function GetCGRect(X1, Y1, X2, Y2: Integer): CGRect;
begin
  if X1 <= X2 then
  begin
    Result.origin.x := X1;
    Result.size.width := X2 - X1;
  end
  else
  begin
    Result.origin.x := X2;
    Result.size.width := X1 - X2;
  end;
  
  if Y1 <= Y2 then
  begin
    Result.origin.y := Y1;
    Result.size.height := Y2 - Y1;
  end
  else
  begin
    Result.origin.y := Y2;
    Result.size.height := Y1 - Y2;
  end;
end;

{------------------------------------------------------------------------------
  Name:    RectToCGRect
  Params:  ARect - Rectangle
  Returns: CGRect

  CGRect constructor
 ------------------------------------------------------------------------------}
function RectToCGRect(const ARect: TRect): CGRect;
begin
  Result.origin.x := ARect.Left;
  Result.origin.y := ARect.Top;
  Result.size.width := ARect.Right - ARect.Left;
  Result.size.height := ARect.Bottom - ARect.Top;
end;

{------------------------------------------------------------------------------
  Name:    CGRectToRect
  Params:  ARect - CGRect
  Returns: TRect

  Converts CGRect to TRect
 ------------------------------------------------------------------------------}
function CGRectToRect(const ARect: CGRect): TRect;
begin
  Result.Left := Floor(ARect.origin.x);
  Result.Top := Floor(ARect.origin.y);
  Result.Right := Ceil(ARect.origin.x + ARect.size.width);
  Result.Bottom := Ceil(ARect.origin.y + ARect.size.height);
end;

{------------------------------------------------------------------------------
  Name:    ParamsToHIRect
  Params:  AParams - Creation parameters
  Returns: HIView Rect

  HIView Rect constructor from creation parameters
 ------------------------------------------------------------------------------}
function ParamsToHIRect(const AParams: TCreateParams): HIRect;
begin
  Result.origin.x := AParams.X;
  Result.origin.y := AParams.Y;
  Result.size.width := AParams.Width;
  Result.size.height := AParams.Height;
end;

{------------------------------------------------------------------------------
  Name:    CarbonRectToRect
  Params:  ARect - Carbon Rect
  Returns: Rectangle

  Converts Carbon Rect to rectangle
 ------------------------------------------------------------------------------}
function CarbonRectToRect(const ARect: FPCMacOSAll.Rect): TRect;
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Right := ARect.Right;
  Result.Bottom := ARect.Bottom;
end;

{------------------------------------------------------------------------------
  Name:    ColorToCarbonColor
  Params:  AColor - Color
  Returns: RGBColor

  Converts the color to Carbon RGBColor
 ------------------------------------------------------------------------------}
function ColorToCarbonColor(const AColor: TColor): RGBColor;
var
  V: TColor;
begin
  V := ColorToRGB(AColor);
  
  Result.Red := Red(V);
  Result.Red := (Result.Red shl 8) or Result.Red;
  Result.Green := Green(V);
  Result.Green := (Result.Green shl 8) or Result.Green;
  Result.Blue := Blue(V);
  Result.Blue := (Result.Blue shl 8) or Result.Blue;
end;

{------------------------------------------------------------------------------
  Name:    CarbonColorToColor
  Params:  AColor - Carbon RGBColor
  Returns: Color

  Converts Carbon RGBColor to color
 ------------------------------------------------------------------------------}
function CarbonColorToColor(const AColor: RGBColor): TColor;
begin
  Result := RGBToColor(AColor.Red shr 8, AColor.Green shr 8, AColor.Blue shr 8);
end;

{------------------------------------------------------------------------------
  Name:    CreateCGColor
  Params:  AColor - Color
  Returns: CGColorRef

  Creates CGColorRef form the specified color
 ------------------------------------------------------------------------------}
function CreateCGColor(const AColor: TColor): CGColorRef;
var
  V: TColor;
  F: Array [0..3] of Single;
begin
  V := ColorToRGB(AColor);

  F[0] := Red(V) / 255;
  F[1] := Green(V) / 255;
  F[2] := Blue(V) / 255;
  F[3] := 1; // Alpha
  Result := CGColorCreate(RGBColorSpace, @F[0]);
end;

function Dbgs(const ARect: FPCMacOSAll.Rect): String;
begin
  Result:=IntToStr(ARect.left)+','+IntToStr(ARect.top)
          +','+IntToStr(ARect.right)+','+IntToStr(ARect.bottom);
end;

function Dbgs(const AColor: FPCMacOSAll.RGBColor): String;
begin
  Result := 'R: ' + IntToHex(AColor.Red, 4)
    + 'G: ' + IntToHex(AColor.Green, 4)
    + 'B: ' + IntToHex(AColor.Blue, 4);
end;

finalization
  if UPPTree <> nil
  then FreeAndNil(UPPTree);

end.
