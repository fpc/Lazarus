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

// debugging defines
{$I carbondebug.inc}

uses
  FPCMacOSAll,
  Classes, Types, LCLType, LCLProc, LCLClasses, LMessages,
  Controls, SysUtils, Graphics, Math, GraphType;

function OSError(AResult: OSStatus; const AMethodName, ACallName: String;
  const AText: String = ''): Boolean;
function OSError(AResult: OSStatus; const AObject: TObject; const AMethodName, ACallName: String;
  const AText: String = ''): Boolean;
function OSError(AResult: OSStatus; const AClass: TClass; const AMethodName, ACallName: String;
  const AText: String = ''): Boolean;
function OSError(AResult: OSStatus; const AObject: TObject; const AMethodName, ACallName: String;
  const AText: String; AValidResult: OSStatus): Boolean;
  
var
  DefaultTextStyle: ATSUStyle; // default Carbon text style
  RGBColorSpace: CGColorSpaceRef; // global RGB color space

{$I mackeycodes.inc}

function VirtualKeyCodeToMac(AKey: Word): Word;

function GetCarbonMsgKeyState: PtrInt;
function GetCarbonShiftState: TShiftState;
function ShiftStateToModifiers(const Shift: TShiftState): Byte;

function FindCarbonFontID(const FontName: String): ATSUFontID;
function FontStyleToQDStyle(const AStyle: TFontStyles): FPCMacOSAll.Style;

procedure FillStandardDescription(var Desc: TRawImageDescription);

const
  DEFAULT_CFSTRING_ENCODING = kCFStringEncodingUTF8;

procedure CreateCFString(const S: String; out AString: CFStringRef);
procedure FreeCFString(var AString: CFStringRef);
function CFStringToStr(AString: CFStringRef): String;

function GetCarbonRect(Left, Top, Width, Height: Integer): FPCMacOSAll.Rect;
function GetCarbonRect(const ARect: TRect): FPCMacOSAll.Rect;
function ParamsToCarbonRect(const AParams: TCreateParams): FPCMacOSAll.Rect;

type
  CGRectArray = Array of CGRect;

function ExcludeRect(const A, B: TRect): CGRectArray;
  
function GetCGRect(X1, Y1, X2, Y2: Integer): CGRect;
function GetCGRectSorted(X1, Y1, X2, Y2: Integer): CGRect;
function RectToCGRect(const ARect: TRect): CGRect;
function CGRectToRect(const ARect: CGRect): TRect;

function ParamsToHIRect(const AParams: TCreateParams): HIRect;
function CarbonRectToRect(const ARect: FPCMacOSAll.Rect): TRect;

function PointToHIPoint(const APoint: TPoint): HIPoint;
function PointToHISize(const APoint: TPoint): HISize;
function HIPointToPoint(const APoint: HIPoint): TPoint;

function ColorToRGBColor(const AColor: TColor): RGBColor;
function RGBColorToColor(const AColor: RGBColor): TColor;
function CreateCGColor(const AColor: TColor): CGColorRef;

function DbgS(const ARect: FPCMacOSAll.Rect): string; overload;
function DbgS(const AColor: FPCMacOSAll.RGBColor): string; overload;

implementation

uses CarbonDbgConsts;

{------------------------------------------------------------------------------
  Name:    OSError
  Params:  AResult     - Result of Carbon function call
           AMethodName - Parent method name
           ACallName   - The Carbon function name
           AText       - Another text useful for debugging (param value, ...)
  Returns: If an error was the result of calling the specified Carbon function
 ------------------------------------------------------------------------------}
function OSError(AResult: OSStatus; const AMethodName, ACallName: String;
  const AText: String): Boolean;
begin
  if AResult = noErr then Result := False
  else
  begin
    Result := True;
    DebugLn(AMethodName + ' Error: ' + ACallName + ' ' + AText +
      ' failed with result ' + DbgS(AResult));
  end;
end;

{------------------------------------------------------------------------------
  Name:    OSError
  Params:  AResult     - Result of Carbon function call
           AObject     - Method object
           AMethodName - Parent method name
           ACallName   - The Carbon function name
           AText       - Another text useful for debugging (param value, ...)
  Returns: If an error was the result of calling the specified Carbon function
 ------------------------------------------------------------------------------}
function OSError(AResult: OSStatus; const AObject: TObject;
  const AMethodName, ACallName: String;
  const AText: String = ''): Boolean;
begin
  if AResult = noErr then Result := False
  else
  begin
    Result := True;
    DebugLn(AObject.ClassName + '.' + AMethodName + ' Error: ' + ACallName +
      ' ' + AText + ' failed with result ' + DbgS(AResult));
  end;
end;

{------------------------------------------------------------------------------
  Name:    OSError
  Params:  AResult     - Result of Carbon function call
           AClass      - Method object
           AMethodName - Parent method name
           ACallName   - The Carbon function name
           AText       - Another text useful for debugging (param value, ...)
  Returns: If an error was the result of calling the specified Carbon function
 ------------------------------------------------------------------------------}
function OSError(AResult: OSStatus; const AClass: TClass;
  const AMethodName, ACallName: String;
  const AText: String = ''): Boolean;
begin
  if AResult = noErr then Result := False
  else
  begin
    Result := True;
    DebugLn(AClass.ClassName + '.' + AMethodName + ' Error: ' + ACallName +
      ' ' + AText + ' failed with result ' + DbgS(AResult));
  end;
end;

{------------------------------------------------------------------------------
  Name:    OSError
  Params:  AResult      - Result of Carbon function call
           AObject      - Method object
           AMethodName  - Parent method name
           ACallName    - The Carbon function name
           AText        - Another text useful for debugging (param value, ...)
           AValidResult - Another result code that is valid like noErr
  Returns: If an error was the result of calling the specified Carbon function
 ------------------------------------------------------------------------------}
function OSError(AResult: OSStatus; const AObject: TObject;
  const AMethodName, ACallName: String;
  const AText: String; AValidResult: OSStatus): Boolean;
begin
  if (AResult = noErr) or (AResult = AValidResult) then Result := False
  else
  begin
    Result := True;
    DebugLn(AObject.ClassName + '.' + AMethodName + ' Error: ' + ACallName +
      ' ' + AText + ' failed with result ' + DbgS(AResult));
  end;
end;

{------------------------------------------------------------------------------
  Name:    VirtualKeyCodeToMac
  Returns: The Mac virtual key (MK_) code for the specified virtual
  key code (VK_) or 0
 ------------------------------------------------------------------------------}
function VirtualKeyCodeToMac(AKey: Word): Word;
begin
  case AKey of
  VK_BACK      : Result := MK_BACKSPACE;
  VK_TAB       : Result := MK_TAB;
  VK_RETURN    : Result := MK_ENTER;
  VK_PAUSE     : Result := MK_PAUSE;
  VK_CAPITAL   : Result := MK_CAPSLOCK;
  VK_ESCAPE    : Result := MK_ESC;
  VK_SPACE     : Result := MK_SPACE;
  VK_PRIOR     : Result := MK_PAGUP;
  VK_NEXT      : Result := MK_PAGDN;
  VK_END       : Result := MK_END;
  VK_HOME      : Result := MK_HOME;
  VK_LEFT      : Result := MK_LEFT;
  VK_UP        : Result := MK_UP;
  VK_RIGHT     : Result := MK_RIGHT;
  VK_DOWN      : Result := MK_DOWN;
  VK_SNAPSHOT  : Result := MK_PRNSCR;
  VK_INSERT    : Result := MK_INS;
  VK_DELETE    : Result := MK_DEL;
  VK_HELP      : Result := MK_HELP;
  VK_SLEEP     : Result := MK_POWER;
  VK_NUMPAD0   : Result := MK_NUMPAD0;
  VK_NUMPAD1   : Result := MK_NUMPAD1;
  VK_NUMPAD2   : Result := MK_NUMPAD2;
  VK_NUMPAD3   : Result := MK_NUMPAD3;
  VK_NUMPAD4   : Result := MK_NUMPAD4;
  VK_NUMPAD5   : Result := MK_NUMPAD5;
  VK_NUMPAD6   : Result := MK_NUMPAD6;
  VK_NUMPAD7   : Result := MK_NUMPAD7;
  VK_NUMPAD8   : Result := MK_NUMPAD8;
  VK_NUMPAD9   : Result := MK_NUMPAD9;
  VK_MULTIPLY  : Result := MK_PADMULT;
  VK_ADD       : Result := MK_PADADD;
  VK_SEPARATOR : Result := MK_PADDEC;
  VK_SUBTRACT  : Result := MK_PADSUB;
  VK_DECIMAL   : Result := MK_PADDEC;
  VK_DIVIDE    : Result := MK_PADDIV;
  VK_F1        : Result := MK_F1;
  VK_F2        : Result := MK_F2;
  VK_F3        : Result := MK_F3;
  VK_F4        : Result := MK_F4;
  VK_F5        : Result := MK_F5;
  VK_F6        : Result := MK_F6;
  VK_F7        : Result := MK_F7;
  VK_F8        : Result := MK_F8;
  VK_F9        : Result := MK_F9;
  VK_F10       : Result := MK_F10;
  VK_F11       : Result := MK_F11;
  VK_F12       : Result := MK_F12;
  VK_F13       : Result := MK_F13;
  VK_F14       : Result := MK_F14;
  VK_F15       : Result := MK_F15;
  VK_NUMLOCK   : Result := MK_NUMLOCK;
  VK_SCROLL    : Result := MK_SCRLOCK;
  else
    Result := 0;
  end;
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
  Name:    ShiftStateToModifiers
  Params:  Shift - Shift state to convert
  Returns: The Carbon key modifiers converted from the passed shift state
 ------------------------------------------------------------------------------}
function ShiftStateToModifiers(const Shift: TShiftState): Byte;
begin
  if Shift = [ssCtrl] then
    Result := kMenuNoModifiers
  else
  begin
    if ssCtrl in Shift then Result := kMenuNoModifiers
    else Result := kMenuNoCommandModifier;
    
    if ssShift in Shift then Inc(Result, kMenuShiftModifier);
    if ssMeta  in Shift then Inc(Result, kMenuControlModifier);
    if ssAlt   in Shift then Inc(Result, kMenuOptionModifier);
  end;
end;

{------------------------------------------------------------------------------
  Name:    FindCarbonFontID
  Params:  FontName - The font name, UTF-8 encoded
  Returns: Carbon font ID of font with the specified name
 ------------------------------------------------------------------------------}
function FindCarbonFontID(const FontName: String): ATSUFontID;
begin
  Result := 0;

  if (FontName <> '') and not SameText(FontName, 'default') then
  begin
    OSError(ATSUFindFontFromName(@FontName[1], Length(FontName),
        kFontFamilyName, kFontMacintoshPlatform, kFontRomanScript,
        kFontEnglishLanguage, Result),
      'FindCarbonFontID', 'ATSUFindFontFromName');
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

  Fills the raw image description with standard Carbon internal image storing
  description
 ------------------------------------------------------------------------------}
procedure FillStandardDescription(var Desc: TRawImageDescription);
begin
  FillChar(Desc, SizeOf(Desc), 0);

  Desc.Format := ricfRGBA;
  Desc.HasPalette := False;
// Width and Height skipped
  Desc.PaletteColorCount := 0;
  Desc.ColorCount := Desc.PaletteColorCount;

  Desc.BitOrder := riboReversedBits;
  Desc.ByteOrder := riboMSBFirst;
  Desc.LineEnd := rileDQWordBoundary; // 128bit aligned
  
  Desc.AlphaSeparate := False;
  
  Desc.LineOrder := riloTopToBottom;
  Desc.BitsPerPixel := 32;
  Desc.Depth := 32;

  // 8-8-8-8 mode, $RRGGBBAA
  Desc.RedPrec := 8;
  Desc.GreenPrec := 8;
  Desc.BluePrec := 8;
  Desc.AlphaPrec := 8;

  Desc.RedShift   := 24;
  Desc.GreenShift := 16;
  Desc.BlueShift  := 08;
  Desc.AlphaShift := 00;
end;

{------------------------------------------------------------------------------
  Name:    CreateCFString
  Params:  S       - UTF-8 string
           AString - Core Foundation string ref

  Creates new Core Foundation string form specified string
 ------------------------------------------------------------------------------}
procedure CreateCFString(const S: String; out AString: CFStringRef);
begin
  AString := CFStringCreateWithCString(nil, Pointer(PChar(S)), DEFAULT_CFSTRING_ENCODING);
end;

{------------------------------------------------------------------------------
  Name:    FreeCFString
  Params:  AString - Core Foundation string ref to free

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
  Name:    ExcludeRect
  Params:  A - Source rectangle
           B - Rectangle to be excluded
  Returns: Array of CGRect, which are product of exclusion rectangle B from
  rectangle A.
  Note: The returned rectangles may overlay.
 ------------------------------------------------------------------------------}
function ExcludeRect(const A, B: TRect): CGRectArray;
begin
  SetLength(Result, 0);
  if (A.Left >= A.Right) or (A.Top >= A.Bottom) then Exit;
  
  SetLength(Result, 1);
  Result[0] := RectToCGRect(A);

  if (B.Left >= B.Right) or (B.Top >= B.Bottom) then Exit;

  if (B.Left < A.Right) and (B.Right > A.Left)
    and (B.Top < A.Bottom) and (B.Bottom > A.Top) then
  begin // rectangles have intersection
    SetLength(Result, 0);

    if B.Top > A.Top then
    begin
      SetLength(Result, Succ(Length(Result)));
      Result[High(Result)] := GetCGRect(A.Left, A.Top, A.Right, B.Top);
    end;

    if B.Bottom < A.Bottom then
    begin
      SetLength(Result, Succ(Length(Result)));
      Result[High(Result)] := GetCGRect(A.Left, B.Bottom, A.Right, A.Bottom);
    end;

    if B.Left > A.Left then
    begin
      SetLength(Result, Succ(Length(Result)));
      Result[High(Result)] := GetCGRect(A.Left, A.Top, B.Left, A.Bottom);
    end;

    if B.Right < A.Right then
    begin
      SetLength(Result, Succ(Length(Result)));
      Result[High(Result)] := GetCGRect(B.Right, A.Top, A.Right, A.Bottom);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Name:    GetCGRect
  Params:  X1, Y1, X2, Y2 - Rectangle coordinates
  Returns: CGRect
 ------------------------------------------------------------------------------}
function GetCGRect(X1, Y1, X2, Y2: Integer): CGRect;
begin
  Result.origin.x := X1;
  Result.size.width := X2 - X1;
  Result.origin.y := Y1;
  Result.size.height := Y2 - Y1;
end;

{------------------------------------------------------------------------------
  Name:    GetCGRectSorted
  Params:  X1, Y1, X2, Y2 - Rectangle coordinates
  Returns: CGRect, coordinates are sorted
 ------------------------------------------------------------------------------}
function GetCGRectSorted(X1, Y1, X2, Y2: Integer): CGRect;
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
  Name:    PointToHIPoint
  Params:  APoint - Point
  Returns: HIPoint
 ------------------------------------------------------------------------------}
function PointToHIPoint(const APoint: TPoint): HIPoint;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

{------------------------------------------------------------------------------
  Name:    PointToHISize
  Params:  APoint - Point
  Returns: HISize
 ------------------------------------------------------------------------------}
function PointToHISize(const APoint: TPoint): HISize;
begin
  Result.Width := APoint.X;
  Result.Height := APoint.Y;
end;

{------------------------------------------------------------------------------
  Name:    HIPointToPoint
  Params:  APoint - HIPoint
  Returns: Point
 ------------------------------------------------------------------------------}
function HIPointToPoint(const APoint: HIPoint): TPoint;
begin
  Result.X := Trunc(APoint.X);
  Result.Y := Trunc(APoint.Y);
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

function DbgS(const ARect: FPCMacOSAll.Rect): String;
begin
  Result := DbgS(ARect.left) + ', ' + DbgS(ARect.top)
          + ', ' + DbgS(ARect.right) + ', ' + DbgS(ARect.bottom);
end;

function DbgS(const AColor: FPCMacOSAll.RGBColor): String;
begin
  Result :=
    'R: ' + IntToHex(AColor.Red, 4) +
    ' G: ' + IntToHex(AColor.Green, 4) +
    ' B: ' + IntToHex(AColor.Blue, 4);
end;


initialization

  OSError(
    ATSUCreateStyle(DefaultTextStyle), 'CarbonProc.initialization', SCreateStyle);
  RGBColorSpace := CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);

finalization

  OSError(
    ATSUDisposeStyle(DefaultTextStyle), 'CarbonProc.finalization', SDisposeStyle);
  CGColorSpaceRelease(RGBColorSpace);
  

end.
