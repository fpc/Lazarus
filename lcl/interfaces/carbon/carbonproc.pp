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
  Controls, Forms, Avl_Tree, SysUtils, Graphics, Math, GraphType,
  CarbonDef, CarbonPrivate;
  
type
  TConvertResult = (trNoError, trNullSrc, trNullDest, trDestExhausted,
    trInvalidChar, trUnfinishedChar);

  TConvertOption = (toInvalidCharError, toInvalidCharToSymbol,
    toUnfinishedCharError, toUnfinishedCharToSymbol);
  TConvertOptions = set of TConvertOption;
  
function UTF8ToUTF16(const S: UTF8String): WideString;

function AsControlRef(Handle: HWND): ControlRef; inline;
function AsWindowRef(Handle: HWND): WindowRef; inline;

function CheckWidget(const Handle: HWND; const DbgText: String; AName: String = ''): Boolean;
function CheckDC(const DC: HDC; const DbgText: String; AName: String = ''): Boolean;
function CheckGDIObject(const GDIObject: HGDIOBJ; const DbgText: String; AName: String = ''): Boolean;
function CheckBitmap(const Bitmap: HBITMAP; const DbgText: String; AName: String = ''): Boolean;

function GetCarbonWidget(AWidget: Pointer): TCarbonWidget;
function GetCarbonWindow(AWidget: WindowRef): TCarbonWindow;
function GetCarbonControl(AWidget: ControlRef): TCarbonControl;

function GetCarbonMsgKeyState: PtrInt;
function GetCarbonShiftState: TShiftState;

function FindCarbonFontID(const FontName: String): ATSUFontID;
function FontStyleToQDStyle(const AStyle: TFontStyles): FPCMacOSAll.Style;

procedure FillStandardDescription(var Desc: TRawImageDescription);

function RegisterEventHandler(AHandler: TCarbonWSEventHandlerProc): EventHandlerUPP;
procedure UnRegisterEventHandler(AHandler: TCarbonWSEventHandlerProc);

procedure CreateCFString(const S: String; out AString: CFStringRef); inline;
procedure FreeCFString(var AString: CFStringRef); inline;
function CFStringToStr(AString: CFStringRef): String;

function GetCarbonRect(Left, Top, Width, Height: Integer): FPCMacOSAll.Rect;
function GetCarbonRect(const ARect: TRect): FPCMacOSAll.Rect;
function ParamsToCarbonRect(const AParams: TCreateParams): FPCMacOSAll.Rect;

function GetCGRect(X1, Y1, X2, Y2: Integer): CGRect;
function RectToCGRect(const ARect: TRect): CGRect;
function CGRectToRect(const ARect: CGRect): TRect;

function ParamsToHIRect(const AParams: TCreateParams): HIRect;
function CarbonRectToRect(const ARect: FPCMacOSAll.Rect): TRect;

function ColorToRGBColor(const AColor: TColor): RGBColor;
function RGBColorToColor(const AColor: RGBColor): TColor; inline;
function CreateCGColor(const AColor: TColor): CGColorRef;

function Dbgs(const ARect: FPCMacOSAll.Rect): string; overload;
function Dbgs(const AColor: FPCMacOSAll.RGBColor): string; overload;

implementation

uses CarbonInt, CarbonCanvas, CarbonGDIObjects;

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
  Name:    AsControlRef
  Params:  Handle  - Handle of window control
  Returns: Carbon control
 ------------------------------------------------------------------------------}
function AsControlRef(Handle: HWND): ControlRef;
begin
  Result := ControlRef(TCarbonControl(Handle).Widget);
end;

{------------------------------------------------------------------------------
  Name:    AsControlRef
  Params:  Handle  - Handle of window
  Returns: Carbon window
 ------------------------------------------------------------------------------}
function AsWindowRef(Handle: HWND): WindowRef;
begin
  Result := WindowRef(TCarbonWindow(Handle).Widget);
end;

{------------------------------------------------------------------------------
  Name:    CheckWidget
  Params:  Handle  - Handle of window
           DbgText - Text to output on invalid DC
           Name    - Param name
  Returns: If the window is valid
 ------------------------------------------------------------------------------}
function CheckWidget(const Handle: HWND; const DbgText: String; AName: String): Boolean;
begin
  if TObject(Handle) is TCarbonWidget then Result := True
  else
  begin
    DebugLn(DbgText + Format(' error - invalid widget %s = %d!',
      [AName, Integer(Handle)]));
    Result := False;
  end;
end;

{------------------------------------------------------------------------------
  Name:    CheckDC
  Params:  DC      - Handle to a device context (TCarbonDeviceContext)
           DbgText - Text to output on invalid DC
           Name    - Param name
  Returns: If the DC is valid
 ------------------------------------------------------------------------------}
function CheckDC(const DC: HDC; const DbgText: String; AName: String): Boolean;
begin
  if TObject(DC) is TCarbonDeviceContext then Result := True
  else
  begin
    DebugLn(DbgText + Format(' error - invalid device context %s = %d!',
      [AName, Integer(DC)]));
    Result := False;
  end;
end;

{------------------------------------------------------------------------------
  Name:    CheckGDIObject
  Params:  GDIObject - handle to a GDI Object (TCarbonFont, ...)
           DbgText   - Text to output on invalid GDIObject
           Name      - Param name
  Returns: If the GDIObject is valid

  Remark: All handles for GDI objects must be pascal objects so we can
 distinguish between them
 ------------------------------------------------------------------------------}
function CheckGDIObject(const GDIObject: HGDIOBJ; const DbgText: String;
  AName: String): Boolean;
begin
  if TObject(GDIObject) is TCarbonGDIObject then Result := True
  else
  begin
    DebugLn(DbgText + Format(' error - invalid GDI object %s = %d!',
      [AName, Integer(GDIObject)]));
    Result := False;
  end;
end;

{------------------------------------------------------------------------------
  Name:    CheckBitmap
  Params:  Bitmap  - handle to a bitmap (TCarbonBitmap)
           DbgText - Text to output on invalid GDIObject
           Name    - Param name
  Returns: If the bitmap is valid
 ------------------------------------------------------------------------------}
function CheckBitmap(const Bitmap: HBITMAP; const DbgText: String;
  AName: String): Boolean;
begin
  if TObject(Bitmap) is TCarbonBitmap then Result := True
  else
  begin
    DebugLn(DbgText + Format(' error - invalid bitmap %s = %d!',
      [AName, Integer(Bitmap)]));
    Result := False;
  end;
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
  Name:    GetCarbonWidget
  Params:  AWidget - Pointer to control or window widget
  Returns: The Carbon widget

  Retrieves widget for specified Carbon control or window
 ------------------------------------------------------------------------------}
function GetCarbonWidget(AWidget: Pointer): TCarbonWidget;
begin
  if AWidget = nil then
  begin
    Result := nil;
    Exit;
  end;

  if IsValidControlHandle(AWidget) then
    Result := GetCarbonControl(ControlRef(AWidget))
  else
    // there is no (cheap) check for windows so assume a window
    // when it is not a control.
    Result := GetCarbonWindow(WindowRef(AWidget));
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonWindow
  Params:  AWidget - Pointer to window widget
  Returns: The Carbon window

  Retrieves the Carbon window for specified window widget
 ------------------------------------------------------------------------------}
function GetCarbonWindow(AWidget: WindowRef): TCarbonWindow;
begin
  if GetWindowProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC,
    SizeOf(TCarbonWidget), nil, @Result) <> noErr then Result := nil;
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonControl
  Params:  AWidget - Pointer to control widget
  Returns: The Carbon control

  Retrieves the Carbon control for specified control widget
 ------------------------------------------------------------------------------}
function GetCarbonControl(AWidget: ControlRef): TCarbonControl;
begin
  if GetControlProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC,
    SizeOf(TCarbonWidget), nil, @Result) <> noErr then Result := nil;
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonMsgKeyState
  Returns: The current state of mouse and function keys
 ------------------------------------------------------------------------------}
function GetCarbonMsgKeyState: PtrInt;
var
  Modifiers, ButtonState: UInt32;
begin
  Result := 0;

  Modifiers := GetCurrentEventKeyModifiers;  // shift, control, option, command
  ButtonState := GetCurrentEventButtonState; // Bit 0 first button (left),
   // bit 1 second (right), bit2 third (middle) ...

  if (ButtonState and 1)         > 0 then Inc(Result, MK_LButton);
  if (ButtonState and 2)         > 0 then Inc(Result, MK_RButton);
  if (ButtonState and 4)         > 0 then Inc(Result, MK_MButton);
  if (shiftKey    and Modifiers) > 0 then Inc(Result, MK_Shift);
  if (cmdKey      and Modifiers) > 0 then Inc(Result, MK_Control);

  //DebugLn('GetCarbonMsgKeyState Result=',dbgs(KeysToShiftState(Result)),' Modifiers=',hexstr(Modifiers,8),' ButtonState=',hexstr(ButtonState,8));
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonShiftState
  Returns: The current shift state of mouse and function keys
 ------------------------------------------------------------------------------}
function GetCarbonShiftState: TShiftState;
var
  Modifiers, ButtonState: UInt32;
begin
  Result := [];

  Modifiers := GetCurrentEventKeyModifiers;  // shift, control, option, command
  ButtonState := GetCurrentEventButtonState; // Bit 0 first button (left),
   // bit 1 second (right), bit2 third (middle) ...

  if (ButtonState and 1)         > 0 then Include(Result, ssLeft);
  if (ButtonState and 2)         > 0 then Include(Result, ssRight);
  if (ButtonState and 4)         > 0 then Include(Result, ssMiddle);
  if (shiftKey    and Modifiers) > 0 then Include(Result, ssShift);
  if (cmdKey      and Modifiers) > 0 then Include(Result, ssCtrl);
  if (controlKey  and Modifiers) > 0 then Include(Result, ssMeta);
  if (optionKey   and Modifiers) > 0 then Include(Result, ssAlt);
  if (alphaLock   and Modifiers) > 0 then Include(Result, ssCaps);

  //DebugLn('GetCarbonShiftState Result=',dbgs(Result),' Modifiers=',hexstr(Modifiers,8),' ButtonState=',hexstr(ButtonState,8));
end;

{------------------------------------------------------------------------------
  Name:    FindCarbonFontID
  Params:  FontName - The font name
  Returns: Carbon font ID of fotn with the specified name
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
  Name:    FontStyleToQDStyle
  Params:  AStyle - Font style
  Returns: QuickDraw Style
 ------------------------------------------------------------------------------}
function FontStyleToQDStyle(const AStyle: TFontStyles): FPCMacOSAll.Style;
begin
  Result := FPCMacOSAll.normal;
  
  if fsBold      in AStyle then Result := Result or FPCMacOSAll.bold;
  if fsItalic    in AStyle then Result := Result or FPCMacOSAll.italic;
  if fsUnderline in AStyle then Result := Result or FPCMacOSAll.underline;
  // fsStrikeOut has no counterpart?
end;

{------------------------------------------------------------------------------
  Name:    FillStandardDescription
  Params:  Desc - Raw image description
  Returns: Nothing

  Fills the raw image description with standard Carbon internal image storing
  description
 ------------------------------------------------------------------------------}
procedure FillStandardDescription(var Desc: TRawImageDescription);
begin
  FillChar(Desc, SizeOf(Desc), 0);

// $RRGGBBAA
  Desc.Format := ricfRGBA;
  Desc.HasPalette := False;
// Width and Height skipped
  Desc.PaletteColorCount := 0;
  Desc.BitOrder := riboReversedBits;
{$IFDEF ENDIAN_BIG}
  Desc.ByteOrder := riboMSBFirst;
{$ELSE}
  Desc.ByteOrder := riboLSBFirst;
{$ENDIF}
  Desc.LineOrder := riloTopToBottom;
  Desc.ColorCount := Desc.PaletteColorCount;
  Desc.BitsPerPixel := 32;
  Desc.LineEnd := rileDQWordBoundary; // 128bit aligned

  // 8-8-8-8 mode, high byte is Alpha
  Desc.RedPrec := 8;
  Desc.GreenPrec := 8;
  Desc.BluePrec := 8;
  Desc.RedShift := 24;
  Desc.GreenShift := 16;
  Desc.BlueShift := 8;
  Desc.Depth := 32;

  Desc.AlphaPrec := 8;
  Desc.AlphaSeparate := False;
  Desc.AlphaLineEnd := rileDQWordBoundary;
  Desc.AlphaShift := 0;
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
  Name:    CreateCFString
  Params:  S       - UTF-8 string
           AString - Core Foundation string ref
  Returns: Nothing

  Creates new Core Foundation string form specified string
 ------------------------------------------------------------------------------}
procedure CreateCFString(const S: String; out AString: CFStringRef);
begin
  AString := CFStringCreateWithCString(nil, Pointer(PChar(S)), DEFAULT_CFSTRING_ENCODING);
end;

{------------------------------------------------------------------------------
  Name:    FreeCFString
  Params:  AString - Core Foundation string ref to free
  Returns: Nothing

  Frees specified Core Foundation string
 ------------------------------------------------------------------------------}
procedure FreeCFString(var AString: CFStringRef);
begin
  if AString <> nil then
    CFRelease(Pointer(AString));
end;

{------------------------------------------------------------------------------
  Name:    CFStringToStr
  Params:  AString - Core Foundation string ref
  Returns: UTF-8 string

  Converts Core Foundation string to string
 ------------------------------------------------------------------------------}
function CFStringToStr(AString: CFStringRef): String;
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
  Returns: Carbon Rect from creation parameters
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
  Returns: CGRect, coordinates are sorted
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
  Returns: HIView Rect from creation parameters
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
 ------------------------------------------------------------------------------}
function CarbonRectToRect(const ARect: FPCMacOSAll.Rect): TRect;
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Right := ARect.Right;
  Result.Bottom := ARect.Bottom;
end;

{------------------------------------------------------------------------------
  Name:    ColorToRGBColor
  Params:  AColor - Color
  Returns: Carbon RGBColor
 ------------------------------------------------------------------------------}
function ColorToRGBColor(const AColor: TColor): RGBColor;
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
  Name:    RGBColorToColor
  Params:  AColor - Carbon RGBColor
  Returns: Color
 ------------------------------------------------------------------------------}
function RGBColorToColor(const AColor: RGBColor): TColor;
begin
  Result := RGBToColor(AColor.Red shr 8, AColor.Green shr 8, AColor.Blue shr 8);
end;

{------------------------------------------------------------------------------
  Name:    CreateCGColor
  Params:  AColor - Color
  Returns: CGColorRef

  Creates CGColorRef from the specified color. You are responsible for
  releasing it by CGColorRelease.
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
