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
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit CarbonProc;

{$mode objfpc}{$H+}

interface

// defines
{$I carbondefines.inc}

uses
  MacOSAll,
  Classes, SysUtils, DateUtils, Types, LCLType, LCLProc,
  Controls, Forms, Graphics, Math, GraphType, StrUtils;

const
  CleanPMRect: PMRect = (top: 0; left: 0; bottom: 0; right: 0);
  CleanPMOrientation: PMOrientation = 0;

function OSError(AResult: OSStatus; const AMethodName, ACallName: String;
  const AText: String = ''): Boolean;
function OSError(AResult: OSStatus; const AObject: TObject; const AMethodName, ACallName: String;
  const AText: String = ''): Boolean;
function OSError(AResult: OSStatus; const AClass: TClass; const AMethodName, ACallName: String;
  const AText: String = ''): Boolean;
function OSError(AResult: OSStatus; const AObject: TObject; const AMethodName, ACallName: String;
  const AText: String; AValidResult: OSStatus): Boolean;
function OSError(AResult: OSStatus; const AMethodName, ACallName: String;
  const AText: String; AValidResult: OSStatus): Boolean;

var
  DefaultTextStyle: ATSUStyle; // default Carbon text style
  RGBColorSpace: CGColorSpaceRef; // global RGB color space
  GrayColorSpace: CGColorSpaceRef; // global Gray color space
  
  HIViewClassID: CFStringRef; // class CFString for HIView
  CustomControlClassID: CFStringRef; // class CFString for custom control

var
  CarbonDefaultFont     : AnsiString = '';
  CarbonDefaultMonoFont : AnsiString = 'Menlo Regular'; { Default introduced in Snow Leopard }
                                                        { TODO: Find from system }
  CarbonDefaultFontSize : Integer = 0;

{$I mackeycodes.inc}

function VirtualKeyCodeToMac(AKey: Word): Word;
function VirtualKeyCodeToCharCode(AKey: Word): Word;

function GetBorderWindowAttrs(const ABorderStyle: TFormBorderStyle;
  const ABorderIcons: TBorderIcons): WindowAttributes;

function GetCarbonMouseClickCount(AEvent: EventRef): Integer;
function GetCarbonMouseButton(AEvent: EventRef): Integer;
function GetCarbonMsgKeyState: PtrInt;
function GetCarbonShiftState: TShiftState;
function ShiftStateToModifiers(const Shift: TShiftState): Byte;

function FindCarbonFontID(const FontName: String): ATSUFontID; overload;
function FindCarbonFontID(FontName: string; var Bold, Italic: Boolean; MonoSpace: Boolean): ATSUFontID; overload;
function CarbonFontIDToFontName(ID: ATSUFontID): String;
function FindQDFontFamilyID(const FontName: String; var Family: FontFamilyID): Boolean;

function FontStyleToQDStyle(const AStyle: TFontStyles): MacOSAll.Style;
function QDStyleToFontStyle(QDStyle: Integer): TFontStyles;

procedure FillStandardDescription(out Desc: TRawImageDescription);

function GetCarbonThemeMetric(Metric: ThemeMetric; DefaultValue: Integer = 0): Integer;

function CreateCustomHIView(const ARect: HIRect; ControlStyle: TControlStyle = []): HIViewRef;

procedure SetControlViewStyle(Control: ControlRef; TinySize, SmallSize, NormalSize: Integer; ControlHeight: Boolean = True);

function CarbonHitTest(Control: ControlRef; const X,Y: integer; var part: ControlPartCode): Boolean;

const
  DEFAULT_CFSTRING_ENCODING = kCFStringEncodingUTF8;

procedure CreateCFString(const S: String; out AString: CFStringRef);
procedure CreateCFString(const Data: CFDataRef; Encoding: CFStringEncoding; out AString: CFStringRef);
procedure FreeCFString(var AString: CFStringRef);
function CFStringToStr(AString: CFStringRef; Encoding: CFStringEncoding = DEFAULT_CFSTRING_ENCODING): String;
function CFStringToData(AString: CFStringRef; Encoding: CFStringEncoding = DEFAULT_CFSTRING_ENCODING): CFDataRef;

function StringsToCFArray(S: TStrings): CFArrayRef;

function RoundFixed(const F: Fixed): Integer;

function GetCarbonRect(Left, Top, Width, Height: Integer): MacOSAll.Rect;
function GetCarbonRect(const ARect: TRect): MacOSAll.Rect;
function ParamsToCarbonRect(const AParams: TCreateParams): MacOSAll.Rect;
function ParamsToRect(const AParams: TCreateParams): TRect;

function CFDateRefToDateTime(dateRef: CFDateRef): TDateTime;

type
  CGRectArray = Array of CGRect;

function ExcludeRect(const A, B: TRect): CGRectArray;
  
function GetCGRect(X1, Y1, X2, Y2: Integer): CGRect;
function GetCGRectSorted(X1, Y1, X2, Y2: Integer): CGRect;
function RectToCGRect(const ARect: TRect): CGRect;
function CGRectToRect(const ARect: CGRect): TRect;

function ParamsToHIRect(const AParams: TCreateParams): HIRect;
function CarbonRectToRect(const ARect: MacOSAll.Rect): TRect;
function HIRectToCarbonRect(const ARect: HIRect): MacOSAll.Rect;
function SortRect(const ARect: TRect): TRect;

function PointToHIPoint(const APoint: TPoint): HIPoint;
function PointToHISize(const APoint: TPoint): HISize;
function HIPointToPoint(const APoint: HIPoint): TPoint;
function GetHIPoint(X, Y: Single): HIPoint;
function GetHISize(X, Y: Single): HISize;

function ColorToRGBColor(const AColor: TColor): RGBColor;
function RGBColorToColor(const AColor: RGBColor): TColor;
function CreateCGColor(const AColor: TColor): CGColorRef;

function DbgS(const ARect: MacOSAll.Rect): string; overload;
function DbgS(const AColor: MacOSAll.RGBColor): string; overload;
function DbgS(const APoint: HIPoint): string; overload;
function DbgS(const ASize: HISize): string; overload;

// Exception raising functions to centralize error strings
procedure RaiseCreateWidgetError(AControl: TWinControl);
procedure RaiseColorSpaceError;
procedure RaiseMemoryAllocationError;
procedure RaiseContextCreationError;

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
  Name:    OSError
  Params:  AResult      - Result of Carbon function call
           AMethodName  - Parent method name
           ACallName    - The Carbon function name
           AText        - Another text useful for debugging (param value, ...)
           AValidResult - Another result code that is valid like noErr
  Returns: If an error was the result of calling the specified Carbon function
 ------------------------------------------------------------------------------}
function OSError(AResult: OSStatus; const AMethodName, ACallName: String;
  const AText: String; AValidResult: OSStatus): Boolean;
begin
  if (AResult = noErr) or (AResult = AValidResult) then Result := False
  else
  begin
    Result := True;
    DebugLn(AMethodName + ' Error: ' + ACallName +
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
//VK_MULTIPLY  : Result := MK_PADMULT;
//VK_ADD       : Result := MK_PADADD;
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
  VK_F16       : Result := MK_F16;
  VK_F17       : Result := MK_F17;
  VK_F18       : Result := MK_F18;
  VK_F19       : Result := MK_F19;
  VK_NUMLOCK   : Result := MK_NUMLOCK;
  VK_CLEAR     : Result := MK_CLEAR;
  VK_SCROLL    : Result := MK_SCRLOCK;
  VK_SHIFT     : Result := MK_SHIFTKEY;
  VK_CONTROL   : Result := MK_COMMAND;
  VK_MENU      : Result := CarbonProc.MK_ALT; // see LCLType.MK_ALT
  VK_OEM_3     : Result := MK_TILDE;
//VK_OEM_MINUS : Result := MK_MINUS;
  VK_OEM_PLUS  : Result := MK_EQUAL;
  VK_OEM_5     : Result := MK_BACKSLASH;
  VK_OEM_4     : Result := MK_LEFTBRACKET;
  VK_OEM_6     : Result := MK_RIGHTBRACKET;
  VK_OEM_1     : Result := MK_SEMICOLON;
  VK_OEM_7     : Result := MK_QUOTE;
  VK_OEM_COMMA : Result := MK_COMMA;
//VK_OEM_PERIOD: Result := MK_PERIOD;
//VK_OEM_2     : Result := MK_SLASH;
  else
    Result := 0;
  end;
end;

{------------------------------------------------------------------------------
  Name:    VirtualKeyCodeToCharCode
  Returns: The char code for the specified virtual key or the original
  virtual key.  Must be called after VirtualKeyCodeToMac since char codes
  overlap VK_ codes.
 ------------------------------------------------------------------------------}
function VirtualKeyCodeToCharCode(AKey: Word): Word;
begin
  case AKey of
  VK_MULTIPLY  : Result := Ord('*');
  VK_ADD       : Result := Ord('+');
  VK_OEM_MINUS : Result := Ord('-');
  VK_OEM_PERIOD: Result := Ord('.');
  VK_OEM_2     : Result := Ord('/');
  else
    Result := AKey;
  end;
end;

{------------------------------------------------------------------------------
  Name:    GetBorderWindowAttrs
  Returns: Converts the form border style and icons to Carbon window attributes
 ------------------------------------------------------------------------------}
function GetBorderWindowAttrs(const ABorderStyle: TFormBorderStyle;
  const ABorderIcons: TBorderIcons): WindowAttributes;
begin
  case ABorderStyle of
  bsNone:
    Result := kWindowNoTitleBarAttribute;
  bsToolWindow, bsSingle:
    Result := kWindowCloseBoxAttribute or
      kWindowCollapseBoxAttribute;
  bsSizeable:
    Result := kWindowCloseBoxAttribute or kWindowCollapseBoxAttribute
      or kWindowFullZoomAttribute or kWindowResizableAttribute;
  bsDialog:
    Result := kWindowCloseBoxAttribute;
  bsSizeToolWin:
    Result := kWindowCloseBoxAttribute or kWindowResizableAttribute;
  else
    Result := kWindowNoAttributes;
  end;

  if biSystemMenu in ABorderIcons then
  begin
    Result := Result or kWindowCloseBoxAttribute;
    if biMinimize in ABorderIcons then
      Result := Result or kWindowCollapseBoxAttribute
    else
      Result := Result and not kWindowCollapseBoxAttribute;
    if biMaximize in ABorderIcons then
      Result := Result or kWindowFullZoomAttribute
    else
      Result := Result and not kWindowFullZoomAttribute;
  end
  else
    Result := Result and not (kWindowCloseBoxAttribute or
      kWindowCollapseBoxAttribute or kWindowFullZoomAttribute);
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonMouseClickCount
  Returns: The click count of mouse
 ------------------------------------------------------------------------------}
function GetCarbonMouseClickCount(AEvent: EventRef): Integer;
var
  ClickCount: UInt32;
const
  SName = 'CarbonWindow_MouseProc';
begin
  Result := 1;

  if OSError(
    GetEventParameter(AEvent, kEventParamClickCount, typeUInt32, nil,
      SizeOf(ClickCount), nil, @ClickCount),
    SName, SGetEvent, 'kEventParamClickCount') then Exit;

  Result := Integer(ClickCount);
  {debugln('GetClickCount ClickCount=',dbgs(ClickCount));}
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonMouseButton
  Returns: The event state of mouse
 ------------------------------------------------------------------------------}
function GetCarbonMouseButton(AEvent: EventRef): Integer;
  // 1 = left, 2 = right, 3 = middle
var
  MouseButton: EventMouseButton;
  Modifiers: UInt32;
const
  SName = 'GetCarbonMouseButton';
begin
  Result := 0;
  Modifiers := 0;

  if OSError(
    GetEventParameter(AEvent, kEventParamMouseButton, typeMouseButton, nil,
      SizeOf(MouseButton), nil, @MouseButton),
    SName, SGetEvent, 'kEventParamMouseButton', eventParameterNotFoundErr) then Exit;
  Result := MouseButton;

  if OSError(
    GetEventParameter(AEvent, kEventParamKeyModifiers, typeUInt32, nil,
      SizeOf(Modifiers), nil, @Modifiers),
    SName, SGetEvent, 'kEventParamKeyModifiers', eventParameterNotFoundErr) then Exit;
    
  if Result = 1 then
  begin
    if (Modifiers and optionKey) > 0 then
      Result := 3
    else
      if (Modifiers and controlKey) > 0 then
        Result := 2;
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
  if (controlKey  and Modifiers) > 0 then Inc(Result, MK_Control);
  if (optionKey   and Modifiers) > 0 then Inc(Result, LCLType.MK_ALT); // see CarbonProc.MK_ALT

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
  if (cmdKey      and Modifiers) > 0 then Include(Result, ssMeta);
  if (controlKey  and Modifiers) > 0 then Include(Result, ssCtrl);
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
  //if Shift = [ssAlt] then
  //  Result := kMenuNoModifiers
  //else
  //begin
    if ssMeta in Shift then
      Result := kMenuNoModifiers
    else
      Result := kMenuNoCommandModifier;

    if ssShift in Shift then Inc(Result, kMenuShiftModifier);
    if ssCtrl  in Shift then Inc(Result, kMenuControlModifier);
    if ssAlt   in Shift then Inc(Result, kMenuOptionModifier);
  //end;
end;

const
  lclFontName      = kFontFullName;
  lclFontPlatform  = kFontMacintoshPlatform;
  lclFontScript    = kFontNoScriptCode;
  lclFontLanguage  = kFontNoLanguageCode;


{------------------------------------------------------------------------------
  Name:    FindCarbonFontID
  Params:  FontName - The font name, UTF-8 encoded
  Returns: Carbon font ID of font with the specified name
 ------------------------------------------------------------------------------}
function FindCarbonFontID(const FontName: String): ATSUFontID;
var
  fn  : String;
begin
  Result := 0;

  //DebugLn('FindCarbonFontID ' + FontName);

  if IsFontNameDefault(FontName)
    then fn:=CarbonDefaultFont
    else fn:=FontName;
  if (fn <> '') then
  begin
    OSError(ATSUFindFontFromName(@fn[1], Length(fn),
        lclFontName, lclFontPlatform, lclFontScript,
        lclFontLanguage, Result),
      'FindCarbonFontID', 'ATSUFindFontFromName');
  end;
end;

{------------------------------------------------------------------------------
  Name:    FindCarbonFontID
  Params:  FontName - The font name, UTF-8 encoded
           Bold, Italic - Font style, cleared if the style was found
           MonoSpace: Indication that Fixed Pitch font is wanted.
                      Currently only implemented for font name 'default'
  Returns: Carbon font ID of font with the specified name

  Finds the font ID for the given font name.  The ATSU bold/italic styles
  are manufactured, so if possible this will match the full font name including
  styles. If a match is found it will clear Bold/Italic.
 ------------------------------------------------------------------------------}
function FindCarbonFontID(FontName: string; var Bold, Italic: Boolean; MonoSpace: Boolean): ATSUFontID;

  function FindFont(const fn: string; code: FontNameCode; out ID: ATSUFontID): Boolean;
  begin
    Result := ATSUFindFontFromName(PChar(fn), Length(fn), code,
      lclFontPlatform, lclFontScript, lclFontLanguage, ID) = noErr;
    if not Result then
      ID := 0;
  end;

const
  SRegular = ' Regular';
  SBold = ' Bold';
  SItalic = ' Italic';
  SOblique = ' Oblique';
var
  FamilyName: string;
begin
  if IsFontNameDefault(FontName) then
    if MonoSpace then
      FontName := CarbonDefaultMonoFont
    else
    FontName := CarbonDefaultFont;
  FamilyName := FontName;
  if AnsiEndsStr(SRegular, FamilyName) then
    SetLength(FamilyName, Length(FamilyName) - Length(SRegular));
  if (Bold and Italic) and
     (FindFont(FamilyName + SBold + SItalic, kFontFullName, Result) or
      FindFont(FamilyName + SBold + SOblique, kFontFullName, Result)) then begin
    Bold := False;
    Italic := False;
  end
  else if Bold and FindFont(FamilyName + SBold, kFontFullName, Result) then
    Bold := False
  else if Italic and
     (FindFont(FamilyName + SItalic, kFontFullName, Result) or
      FindFont(FamilyName + SOblique, kFontFullName, Result)) then
    Italic := False
  else if not FindFont(FontName, kFontFullName, Result) then
    FindFont(FontName, kFontFamilyName, Result)
end;

{------------------------------------------------------------------------------
  Name:    CarbonFontIDToFontName
  Params:  IS - Carbon font ID
  Returns: The font name, UTF-8 encoded
 ------------------------------------------------------------------------------}
function CarbonFontIDToFontName(ID: ATSUFontID): String;
var
  NameLength: LongWord;
  FontName: UTF8String;
const
  SName = 'CarbonFontIDToFontName';
begin
  Result := '';
  NameLength:=1024;
  
  // retrieve font name length
  if OSError(ATSUFindFontName(ID, lclFontName, lclFontPlatform,
      lclFontScript, lclFontLanguage, NameLength, nil,
    @NameLength, nil), SName, 'ATSUFindFontName', 'Length') then Exit;

  SetLength(FontName, NameLength);

  // retrieve font name
  if OSError(ATSUFindFontName(ID, lclFontName, lclFontPlatform,
      lclFontScript, lclFontLanguage, NameLength,
    @FontName[1], @NameLength, nil), SName, 'ATSUFindFontName', 'Name') then Exit;
    
  Result := FontName;
end;

{------------------------------------------------------------------------------
  Name:    CarbonFontIDToFontName
  Params:  FontName - The font name, UTF-8 encoded
  Returns: Returns QuickDraw font family ID
           The function returns true, if the font family has be found, false
           otherwise.
  Note: FMGetFontFamilyFromName is deprecated in OSX 10.4.
        There's no replacment for the function, in future OSX versions.
 ------------------------------------------------------------------------------}
function FindQDFontFamilyID(const FontName: String; var Family: FontFamilyID): Boolean;
var
  name : Str255;
begin
  name:=FontName;
  Family:=FMGetFontFamilyFromName(name);
  Result:=true;
end;

{------------------------------------------------------------------------------
  Name:    FontStyleToQDStyle
  Params:  AStyle - Font style
  Returns: QuickDraw Style
 ------------------------------------------------------------------------------}
function FontStyleToQDStyle(const AStyle: TFontStyles): MacOSAll.Style;
begin
  Result := MacOSAll.normal;
  
  if fsBold      in AStyle then Result := Result or MacOSAll.bold;
  if fsItalic    in AStyle then Result := Result or MacOSAll.italic;
  if fsUnderline in AStyle then Result := Result or MacOSAll.underline;
  // fsStrikeOut has no counterpart?
end;

{------------------------------------------------------------------------------
  Name:    QDStyleToFontStyle
  Params:  QDStyle - Quick Draw font style
  Returns: LCL Font Style
 ------------------------------------------------------------------------------}
function QDStyleToFontStyle(QDStyle: Integer): TFontStyles;
begin
  Result := [];
  if QDStyle and MacOSAll.bold > 0 then Include(Result, fsBold);
  if QDStyle and MacOSAll.italic > 0 then Include(Result, fsItalic);
  if QDStyle and MacOSAll.underline > 0 then Include(Result, fsUnderline);
end;

{------------------------------------------------------------------------------
  Name:    FillStandardDescription
  Params:  Desc - Raw image description

  Fills the raw image description with standard Carbon internal image storing
  description
 ------------------------------------------------------------------------------}
procedure FillStandardDescription(out Desc: TRawImageDescription);
begin
  Desc.Init;

  Desc.Format := ricfRGBA;
// Width and Height skipped
  Desc.PaletteColorCount := 0;

  Desc.BitOrder := riboReversedBits;
  Desc.ByteOrder := riboMSBFirst;
  Desc.LineEnd := rileDQWordBoundary; // 128bit aligned
  
  Desc.LineOrder := riloTopToBottom;
  Desc.BitsPerPixel := 32;
  Desc.Depth := 32;

  // 8-8-8-8 mode, $AARRGGBB
  Desc.RedPrec := 8;
  Desc.GreenPrec := 8;
  Desc.BluePrec := 8;
  Desc.AlphaPrec := 8;
  
  Desc.AlphaShift := 24;
  Desc.RedShift   := 16;
  Desc.GreenShift := 08;
  Desc.BlueShift  := 00;

  Desc.MaskBitOrder := riboReversedBits;
  Desc.MaskBitsPerPixel := 1;
  Desc.MaskLineEnd := rileByteBoundary;
  Desc.MaskShift := 0;
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonThemeMetric
  Params:  Metric       - Theme metric
           DefaultValue
  Returns: Theme metric value or default value if fails
 ------------------------------------------------------------------------------}
function GetCarbonThemeMetric(Metric: ThemeMetric; DefaultValue: Integer): Integer;
begin
  if OSError(GetThemeMetric(Metric, Result{%H-}),
    'GetCarbonThemeMetric', 'GetThemeMetric') then Result := DefaultValue;
end;

{------------------------------------------------------------------------------
  Name:    CreateCustomHIView
  Params:  ARect - Bounds rect
           ControlStyle
  Returns: New custom HIView
 ------------------------------------------------------------------------------}
function CreateCustomHIView(const ARect: HIRect; ControlStyle: TControlStyle): HIViewRef;
var
  Features: HIViewFeatures;
const
  SName = 'CreateCustomHIView';
begin
  Result := nil;

  if OSError(
    HIObjectCreate(CustomControlClassID, nil, HIObjectRef(Result)),
    SName, 'HIObjectCreate') then Exit;

  Features := kHIViewFeatureAllowsSubviews;
  if not (csNoFocus in ControlStyle) then
    Features := Features or kHIViewFeatureGetsFocusOnClick;

  OSError(
    HIViewChangeFeatures(Result, Features, 0),
    SName, 'HIViewChangeFeatures');
    
  OSError(HIViewSetVisible(Result, True), SName, SViewVisible);
  OSError(HIViewSetFrame(Result, ARect), SName, SViewFrame);
end;

{------------------------------------------------------------------------------
  Name:    SetControlViewSize
  Params:  Control - Mac OS Carbon control handle
           TinySize - if control size, is less or equal to TinySize then
                      tine size view style is set. The same goes with Small an
                      NormalSize. If control size is larger than control size,
                      then view style is set to Auto.
           SmallSize
           NormalSize
           ControlHeight - if Height (default) instead of Width of the control
             should be measured
 ------------------------------------------------------------------------------}
procedure SetControlViewStyle(Control: ControlRef; TinySize, SmallSize,
  NormalSize: Integer; ControlHeight: Boolean);
var
  R: MacOSAlL.Rect;
  Data: Word;
  ControlSize: Integer;
begin
  FillChar(R{%H-}, SizeOf(R), 0);
  GetControlBounds(Control, R);
  if ControlHeight then ControlSize := R.Bottom - R.Top
  else ControlSize := R.Right - R.Left;

  if ControlSize > NormalSize then Data := kControlSizeAuto
  else if ControlSize = NormalSize then Data := kControlSizeNormal
  else if ControlSize >= SmallSize then Data := kControlSizeSmall
  else if ControlSize >= TinySize then Data := kControlSizeMini
  else Data := kControlSizeAuto;

  SetControlData(Control, kControlEntireControl, kControlSizeTag, SizeOf(Data), @Data);
end;


{------------------------------------------------------------------------------
  Name:    CarbonHitTest
  Params:  Control - control to test
           x,y     - mouse coordinates in control's local coordinates
           part    - hit test result
  Returns: True - if hittest is succsefull, False - overwise

  Performs hit-test on a carbon control (hiview)
 ------------------------------------------------------------------------------}
function CarbonHitTest(Control: ControlRef; const X,Y: integer; var part: ControlPartCode): Boolean;
var
  event : EventRef;
  mp    : MacOSAll.point;
begin
  Result := false;
  if CreateEvent(kCFAllocatorDefault, kEventClassControl, kEventControlHitTest, 0, 0, event{%H-}) <> noErr then
    Exit;
  mp.h := X;
  mp.v := Y;
  SetEventParameter(event, kEventParamDirectObject, typeControlRef, sizeof(Control), @Control);
  SetEventParameter(event, kEventParamMouseLocation, typeQDPoint, sizeof(mp), @mp);
  if SendEventToEventTarget(event, GetControlEventTarget(Control))= noErr then
    Result:=GetEventParameter(event, kEventParamControlPart, typeControlPartCode, nil, sizeof(part), nil, @part)=noErr;
  ReleaseEvent(event);
end;

{------------------------------------------------------------------------------
  Name:    CreateCFString
  Params:  S       - UTF-8 string
           AString - Core Foundation string ref

  Creates new Core Foundation string from the specified string
 ------------------------------------------------------------------------------}
procedure CreateCFString(const S: String; out AString: CFStringRef);
begin
  AString := CFStringCreateWithCString(nil, Pointer(PChar(S)), DEFAULT_CFSTRING_ENCODING);
end;

{------------------------------------------------------------------------------
  Name:    CreateCFString
  Params:  Data     - CFDataRef
           Encoding - Data encoding format
           AString  - Core Foundation string ref

  Creates new Core Foundation string from the specified data and format
 ------------------------------------------------------------------------------}
procedure CreateCFString(const Data: CFDataRef; Encoding: CFStringEncoding; out
  AString: CFStringRef);
begin
  AString := nil;
  if Data = nil then Exit;
  AString := CFStringCreateWithBytes(nil, CFDataGetBytePtr(Data),
    CFDataGetLength(Data), Encoding, False);
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
  Params:  AString  - Core Foundation string ref
           Encoding - Result data encoding format
  Returns: UTF-8 string

  Converts Core Foundation string to string
 ------------------------------------------------------------------------------}
function CFStringToStr(AString: CFStringRef; Encoding: CFStringEncoding): String;
var
  Str: Pointer;
  StrSize: CFIndex;
  StrRange: CFRange;
begin
  if AString = nil then
  begin
    Result := '';
    Exit;
  end;

  // Try the quick way first
  Str := CFStringGetCStringPtr(AString, Encoding);
  if Str <> nil then
    Result := PChar(Str)
  else
  begin
    // if that doesn't work this will
    StrRange.location := 0;
    StrRange.length := CFStringGetLength(AString);
    
    CFStringGetBytes(AString, StrRange, Encoding,
      Ord('?'), False, nil, 0, StrSize{%H-});
    SetLength(Result, StrSize);

    if StrSize > 0 then
      CFStringGetBytes(AString, StrRange, Encoding,
        Ord('?'), False, @Result[1], StrSize, StrSize);
  end;
end;

{------------------------------------------------------------------------------
  Name:    CFStringToData
  Params:  AString  - Core Foundation string ref
           Encoding - Result data encoding format
  Returns: CFDataRef

  Converts Core Foundation string to data
 ------------------------------------------------------------------------------}
function CFStringToData(AString: CFStringRef; Encoding: CFStringEncoding): CFDataRef;
var
  S: String;
begin
  Result := nil;
  if AString = nil then Exit;
  S := CFStringToStr(AString, Encoding);
  
  if Length(S) > 0 then
    Result := CFDataCreate(nil, @S[1], Length(S))
  else
    Result := CFDataCreate(nil, nil, 0);
end;

{------------------------------------------------------------------------------
  Name:    StringsToCFArray
  Params:  S - Strings
  Returns: Creates CFArray from strings
 ------------------------------------------------------------------------------}
function StringsToCFArray(S: TStrings): CFArrayRef;
var
  StrArray: Array of CFStringRef;
  I: Integer;
begin
  SetLength(StrArray, S.Count);

  for I := 0 to S.Count - 1 do CreateCFString(S[I], StrArray[I]);
  
  if S.Count > 0 then
    Result := CFArrayCreate(nil, @StrArray[0], Length(StrArray), nil)
  else
    Result := CFArrayCreate(nil, nil, 0, nil);
end;

{------------------------------------------------------------------------------
  Name:    RoundFixed
  Params:  F - Fixed value
  Returns: Rounded passed fixed value
 ------------------------------------------------------------------------------}
function RoundFixed(const F: Fixed): Integer;
begin
  Result := Round(Fix2X(F));
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonRect
  Params:  Left, Top, Width, Height - Coordinates
  Returns: Carbon Rect
 ------------------------------------------------------------------------------}
function GetCarbonRect(Left, Top, Width, Height: Integer): MacOSAll.Rect;
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
function GetCarbonRect(const ARect: TRect): MacOSAll.Rect;
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
function ParamsToCarbonRect(const AParams: TCreateParams): MacOSAll.Rect;
begin
  Result.Left := AParams.X;
  Result.Top := AParams.Y;
  Result.Right := AParams.X + AParams.Width;
  Result.Bottom := AParams.Y + AParams.Height;
end;

{------------------------------------------------------------------------------
  Name:    ParamsToRect
  Params:  AParams - Creation parameters
  Returns: TRect from creation parameters
 ------------------------------------------------------------------------------}
function ParamsToRect(const AParams: TCreateParams): TRect;
begin
  Result.Left := AParams.X;
  Result.Top := AParams.Y;
  Result.Right := AParams.X + AParams.Width;
  Result.Bottom := AParams.Y + AParams.Height;
end;

function CFDateRefToDateTime(dateRef: CFDateRef): TDateTime;
var
  absTime: CFAbsoluteTime;
  gDate: CFGregorianDate;
  tz: CFTimeZoneRef;
begin
  absTime := CFDateGetAbsoluteTime(dateRef);
  tz := CFTimeZoneCopySystem;
  gDate := CFAbsoluteTimeGetGregorianDate(absTime, tz);
  CFRelease(tz);

  with gDate do
    if not TryEncodeDateTime(
              trunc(year), trunc(month), trunc(day),
              trunc(hour), trunc(minute), trunc(second), 0, result)
    then
      result := 0;

end;

{------------------------------------------------------------------------------
  Name:    ExcludeRect
  Params:  A - Source rectangle
           B - Rectangle to be excluded
  Returns: Array of CGRect, which are product of exclusion rectangle B from
  rectangle A.
  Note: The returned rectangles may overlap.
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
  if CGRectIsNull(ARect) <> 0 then
  begin // CGRect passed is invalid!
    Result.Left := 0;
    Result.Top := 0;
    Result.Right := 0;
    Result.Bottom := 0;
  end
  else
  begin
    Result.Left := Floor(ARect.origin.x);
    Result.Top := Floor(ARect.origin.y);
    Result.Right := Ceil(ARect.origin.x + ARect.size.width);
    Result.Bottom := Ceil(ARect.origin.y + ARect.size.height);
  end;
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
function CarbonRectToRect(const ARect: MacOSAll.Rect): TRect;
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Right := ARect.Right;
  Result.Bottom := ARect.Bottom;
end;

{------------------------------------------------------------------------------
  Name:    HIRectToCarbonRect
  Params:  ARect - HIRect
  Returns: Carbon Rect
 ------------------------------------------------------------------------------}
function HIRectToCarbonRect(const ARect: HIRect): MacOSAll.Rect;
begin
  if CGRectIsNull(ARect) <> 0 then
  begin // CGRect passed is invalid!
    Result.Left := 0;
    Result.Top := 0;
    Result.Right := 0;
    Result.Bottom := 0;
  end
  else
  begin
    Result.Left := Floor(ARect.origin.x);
    Result.Top := Floor(ARect.origin.y);
    Result.Right := Ceil(ARect.origin.x + ARect.size.width);
    Result.Bottom := Ceil(ARect.origin.y + ARect.size.height);
  end;
end;

function SortRect(const ARect: TRect): TRect;
begin
  if ARect.Left <= ARect.Right then
  begin
    Result.Left := ARect.Left;
    Result.Right := ARect.Right;
  end
  else
  begin
    Result.Left := ARect.Right;
    Result.Right := ARect.Left;
  end;

  if ARect.Top <= ARect.Bottom then
  begin
    Result.Top := ARect.Top;
    Result.Bottom := ARect.Bottom;
  end
  else
  begin
    Result.Top := ARect.Bottom;
    Result.Bottom := ARect.Top;
  end;
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
  Name:    GetHIPoint
  Params:  X, Y
  Returns: HIPoint
 ------------------------------------------------------------------------------}
function GetHIPoint(X, Y: Single): HIPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

{------------------------------------------------------------------------------
  Name:    GetHISize
  Params:  X, Y
  Returns: HISize
 ------------------------------------------------------------------------------}
function GetHISize(X, Y: Single): HISize;
begin
  Result.width := X;
  Result.height := Y;
end;

{------------------------------------------------------------------------------
  Name:    ColorToRGBColor
  Params:  AColor - Color
  Returns: Carbon RGBColor
 ------------------------------------------------------------------------------}
function ColorToRGBColor(const AColor: TColor): RGBColor;
var
  V: TColorRef;
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
  Result := RGBToColor((AColor.Red shr 8) and $FF, (AColor.Green shr 8) and $FF, (AColor.Blue shr 8) and $FF);
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
  V: TColorRef;
  F: Array [0..3] of Single;
begin
  V := ColorToRGB(AColor);

  F[0] := Red(V) / 255;
  F[1] := Green(V) / 255;
  F[2] := Blue(V) / 255;
  F[3] := 1; // Alpha
  Result := CGColorCreate(RGBColorSpace, @F[0]);
end;

function DbgS(const ARect: MacOSAll.Rect): String;
begin
  Result := DbgS(ARect.left) + ', ' + DbgS(ARect.top)
          + ', ' + DbgS(ARect.right) + ', ' + DbgS(ARect.bottom);
end;

function DbgS(const AColor: MacOSAll.RGBColor): String;
begin
  Result :=
    'R: ' + IntToHex(AColor.Red, 4) +
    ' G: ' + IntToHex(AColor.Green, 4) +
    ' B: ' + IntToHex(AColor.Blue, 4);
end;

function DbgS(const APoint: HIPoint): string;
begin
  Result := 'X: ' + DbgS(APoint.X) + ' Y: ' + DbgS(APoint.Y);
end;

function DbgS(const ASize: HISize): string;
begin
  Result := 'W: ' + DbgS(ASize.width) + ' H: ' + DbgS(ASize.height);
end;

{------------------------------------------------------------------------------
  Name:    RaiseCreateWidgetError
  Params:  AControl - Which control was being created

  Raises exception for widget creation error
  
  Used on CarbonPrivate
 ------------------------------------------------------------------------------}
procedure RaiseCreateWidgetError(AControl: TWinControl);
begin
  raise Exception.CreateFmt('Unable to create Carbon widget for %s: %s!',
    [AControl.Name, AControl.ClassName]);
end;

procedure RaiseColorSpaceError;
begin
  raise Exception.Create('Unable to create CGColorSpaceRef');
end;

procedure RaiseMemoryAllocationError;
begin
  raise Exception.Create('Unable to allocate memory');
end;

procedure RaiseContextCreationError;
begin
  raise Exception.Create('Unable to create CGContextRef');
end;

{------------------------------------------------------------------------------
  Name: CustomControlHandler
  Handles custom control class methods
 ------------------------------------------------------------------------------}
function CustomControlHandler(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  {%H-}AData: Pointer): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  EventClass, EventKind: LongWord;
  Part: ControlPartCode;
const
	SName = 'CustomControlHandler';
begin
  EventClass := GetEventClass(AEvent);
  EventKind := GetEventKind(AEvent);

  case EventClass of
    kEventClassHIObject:
      case EventKind of
        kEventHIObjectConstruct,
        kEventHIObjectDestruct: Result := noErr;
        kEventHIObjectInitialize: Result := CallNextEventHandler(ANextHandler, AEvent);
      end;
    kEventClassControl:
      case EventKind of
        kEventControlGetFocusPart,
        kEventControlSetFocusPart: Result := CallNextEventHandler(ANextHandler, AEvent);
        kEventControlHitTest:
          begin
            //Result := CallNextEventHandler(ANextHandler, AEvent);
            // I was not able to find what for is this workaround (r11394)
            // returning kControlEditTextPart for any customcontrol looks strange
            // It seems to interfer with what widget really hit the mouse click.
            //
            // But it breaks grid's mouse selecting when clicking (near) at
            // the borders of cells on an always show editor grid.
            //
            // Found, without it double click doesn't occur in custom controls
            // temporarily restored old behaviour affects issue 22542
            // kControlEditTextPart is an arbitrary number
            {$IFDEF VerboseMouse}
              DebugLn('CustomControlHandler HitTest');
            {$ENDIF}

            Part := kControlEditTextPart; // workaround

            Result := SetEventParameter(AEvent, kEventParamControlPart,
              typeControlPartCode, SizeOf(Part), @Part);
            OSError(Result, SName, SSetEvent);
          end;
      end;
    kEventClassTextInput: Result := noErr;
    kEventClassScrollable: Result := noErr;
  end;
end;

procedure InitDefaultFont;
var
  s   : Str255;
  st  : MacOSAll.Style;
  sz  : SInt16;
begin
  //Note: the GetThemeFont is deprecated in 10.5. CoreText functions should be used!
  MacOSAll.GetThemeFont(kThemeSystemFont, GetApplicationScript, @s, sz{%H-}, st{%H-});
  CarbonDefaultFont := s;
  CarbonDefaultFontSize := sz;
end;

var
  EventSpec: Array [0..8] of EventTypeSpec;
  CustomControlHandlerUPP: EventHandlerUPP;
  
initialization

  OSError(
    ATSUCreateStyle(DefaultTextStyle), 'CarbonProc.initialization', SCreateStyle);
  RGBColorSpace := CGColorSpaceCreateDeviceRGB;
  GrayColorSpace := CGColorSpaceCreateDeviceGray;

  EventSpec[0].eventClass := kEventClassHIObject;
  EventSpec[0].eventKind := kEventHIObjectConstruct;
  EventSpec[1].eventClass := kEventClassHIObject;
  EventSpec[1].eventKind := kEventHIObjectInitialize;
  EventSpec[2].eventClass := kEventClassHIObject;
  EventSpec[2].eventKind := kEventHIObjectDestruct;
  EventSpec[3].eventClass := kEventClassControl;
  EventSpec[3].eventKind := kEventControlHitTest;
  EventSpec[4].eventClass := kEventClassTextInput;
  EventSpec[4].eventKind := kEventTextInputUnicodeForKeyEvent;
  EventSpec[5].eventClass := kEventClassControl;
  EventSpec[5].eventKind := kEventControlGetFocusPart;
  EventSpec[6].eventClass := kEventClassControl;
  EventSpec[6].eventKind := kEventControlSetFocusPart;
  EventSpec[7].eventClass := kEventClassScrollable;
  EventSpec[7].eventKind := kEventScrollableGetInfo;
  EventSpec[8].eventClass := kEventClassScrollable;
  EventSpec[8].eventKind := kEventScrollableScrollTo;

  CustomControlHandlerUPP := NewEventHandlerUPP(EventHandlerProcPtr(@CustomControlHandler));

  CreateCFString('com.lazarus.customcontrol', CustomControlClassID);
  CreateCFString('com.apple.hiview', HIViewClassID);

  OSError(
    HIObjectRegisterSubclass(CustomControlClassID, HIViewClassID, 0,
      CustomControlHandlerUPP, Length(EventSpec), @EventSpec[0], nil, nil),
    'CarbonProc.initialization', 'HIObjectRegisterSubclass');

  InitDefaultFont;

finalization

  FreeCFString(CustomControlClassID);
  FreeCFString(HIViewClassID);
  DisposeEventHandlerUPP(CustomControlHandlerUPP);
   

  OSError(
    ATSUDisposeStyle(DefaultTextStyle), 'CarbonProc.finalization', SDisposeStyle);
  CGColorSpaceRelease(RGBColorSpace);
  CGColorSpaceRelease(GrayColorSpace);

end.


