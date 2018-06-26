unit CocoaUtils;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  classes,
  MacOSAll, CocoaAll,
  SysUtils, Types, LCLType, LCLClasses, LCLProc, menus,//LMessages,
  Controls, Forms, Graphics, Math, GraphType;

const
  LCLEventSubTypeMessage = MaxShort - 1;
var
  // post message/send message string. Created by TCocoaWidgetSet
  NSMessageWnd, NSMessageMsg, NSMessageWParam, NSMessageLParam, NSMessageResult: NSString;

type
  { NSLCLDebugExtension }

  NSLCLDebugExtension = objccategory(NSObject)
    function lclClassName: shortstring; message 'lclClassName';
  end;

const
  NSNullRect : NSRect = (origin:(x:0; y:0); size:(width:0; height:0));

function GetNSSize(width, height: CGFloat): NSSize; inline;

function GetNSPoint(x,y: single): NSPoint; inline;
function LCLToNSPoint(APt: TPoint; ParentHeight: Single): NSPoint;

function GetCGRect(x1, y1, x2, y2: Integer): CGRect; inline;
function GetCGRectSorted(X1, Y1, X2, Y2: Integer): CGRect;
function RectToCGRect(const R: TRect): CGRect;
function CGRectToRect(const c: CGRect): TRect;

function GetNSRect(x, y, width, height: Integer): NSRect; inline;
function RectToNSRect(const r: TRect): NSRect;
function NSRectToRect(const NS: NSRect): TRect;

procedure NSToLCLRect(const ns: NSRect; ParentHeight: Single; out lcl: TRect);
procedure LCLToNSRect(const lcl: TRect; ParentHeight: Single; out ns: NSRect);
function LCLCoordsToCocoa(AControl: TControl; X, Y: Integer): NSPoint;

function CreateParamsToNSRect(const params: TCreateParams): NSRect;

function NSStringUtf8(s: PChar): NSString;
function NSStringUtf8(const s: String): NSString;
function NSStringToString(ns: NSString): String;

function GetNSObjectView(obj: NSObject): NSView;
function GetNSObjectWindow(obj: NSObject): NSWindow;

procedure SetNSText(text: NSText; const s: String); inline;
function GetNSText(text: NSText): string; inline;

procedure SetNSControlValue(c: NSControl; const S: String); inline;
function GetNSControlValue(c: NSControl): String; inline;

procedure ColorToRGBFloat(cl: TColorRef; var r,g,b: Single); inline;
function RGBToColorFloat(r,g,b: Single): TColorRef; inline;
// extract ColorRef from NSColor in RGB colorspace
function NSColorToRGB(const Color: NSColor): TColorRef; inline;
// extract ColorRef from any NSColor
function NSColorToColorRef(const Color: NSColor): TColorRef;
function ColorToNSColor(const Color: TColorRef): NSColor; inline;

// the returned "Key" should not be released, as it's not memory owned
procedure ShortcutToKeyEquivalent(const AShortCut: TShortcut; out Key: NSString; out shiftKeyMask: NSUInteger);

const
  DEFAULT_CFSTRING_ENCODING = kCFStringEncodingUTF8;

function CFStringToStr(AString: CFStringRef; Encoding: CFStringEncoding = DEFAULT_CFSTRING_ENCODING): String;
function CFStringToString(AString: CFStringRef): String;

// Missing things from NSTableColumns.inc
const
  NSTableColumnAutoresizingMask = 1 shl 0;
  NSTableColumnUserResizingMask = 1 shl 1;

{$I mackeycodes.inc}

function VirtualKeyCodeToMac(AKey: Word): Word;
function VirtualKeyCodeToMacString(AKey: Word): NSString;

procedure FillStandardDescription(out Desc: TRawImageDescription);

procedure CreateCFString(const S: String; out AString: CFStringRef);
procedure CreateCFString(const Data: CFDataRef; Encoding: CFStringEncoding; out AString: CFStringRef);
procedure FreeCFString(var AString: CFStringRef);
function CFStringToData(AString: CFStringRef; Encoding: CFStringEncoding = DEFAULT_CFSTRING_ENCODING): CFDataRef;

function GetCurrentEventTime: double;
function GetMacOSXVersion: Integer;

procedure NSResponderHotKeys(trg: NSResponder; event: NSEvent; var handled: Boolean);

function DateTimeToNSDate(const aDateTime : TDateTime): NSDate;
function NSDateToDateTime(const aDateTime: NSDate): TDateTime;

implementation

procedure ColorToRGBFloat(cl: TColorRef; var r,g,b: Single); inline;
begin
  R:=(cl and $FF) / $FF;
  G:=((cl shr 8) and $FF) / $FF;
  B:=((cl shr 16) and $FF) / $FF;
end;

function RGBToColorFloat(r,g,b: Single): TColorRef; inline;
begin
  Result:=(Round(b*$FF) shl 16) or (Round(g*$FF) shl 8) or Round(r*$FF);
end;

function NSColorToRGB(const Color: NSColor): TColorRef; inline;
begin
  with Color do
    Result := RGBToColorFloat(redComponent, greenComponent, blueComponent);
end;

function NSColorToColorRef(const Color: NSColor): TColorRef;

function AverageColor(Color1, Color2: TColorRef): TColorRef; inline;
  begin
    if Color1 = Color2 then
      Result := Color1
    else
      Result :=
        (((Color1 and $FF) + (Color2 and $FF)) shr 1) and $FF or
        (((((Color1 shr 8) and $FF) + ((Color2 shr 8) and $FF)) shr 1) and $FF) shl 8 or
        (((((Color1 shr 16) and $FF) + ((Color2 shr 16) and $FF)) shr 1) and $FF) shl 16;
  end;

var
  LocalPool: NSAutoReleasePool;
  RGBColor, PatternColor: NSColor;
  ImageRep: NSImageRep;
  x, y: Integer;
begin
  LocalPool := NSAutoReleasePool.alloc.init;
  RGBColor := Color.colorUsingColorSpaceName(NSDeviceRGBColorSpace);
  // if color is a pattern it can't be converted as is to a solid color value
  if RGBColor = nil then
  begin
    PatternColor := Color.colorUsingColorSpaceName(NSPatternColorSpace);
    if PatternColor = nil then
      Result := 0
    else
    begin
      // compute an average color of the top left 2x2 rectangle
      ImageRep := PatternColor.patternImage.bestRepresentationForRect_context_hints(NSNullRect, nil, nil);
      if (ImageRep = nil) or not ImageRep.isKindOfClass(NSBitmapImageRep) then
        Result := 0
      else
      begin
        for y := 0 to ImageRep.pixelsHigh - 1 do
          for x := 0 to ImageRep.pixelsWide - 1 do
          begin
            RGBColor := NSBitmapImageRep(ImageRep).colorAtX_y(x, y).colorUsingColorSpaceName(NSCalibratedRGBColorSpace);
            if Assigned(RGBColor) then
            begin
              if (x = 0) and (y = 0) then
                Result := NSColorToRGB(RGBColor)
              else
                Result := AverageColor(Result, NSColorToRGB(RGBColor))
            end
            else
            begin
              Result := 0;
              break;
            end
          end;
      end;
    end;
  end
  else
    Result := NSColorToRGB(RGBColor);
  LocalPool.release;
end;

function ColorToNSColor(const Color: TColorRef): NSColor; inline;
begin
  Result := NSColor.colorWithDeviceRed_green_blue_alpha(
    (Color and $FF) / $FF,
    ((Color shr 8) and $FF) / $FF,
    ((Color shr 16) and $FF) / $FF, 1);
end;

procedure ShortcutToKeyEquivalent(const AShortCut: TShortcut; out Key: NSString; out shiftKeyMask: NSUInteger);
var
  w: word;
  s: TShiftState;
begin
  ShortCutToKey(AShortCut, w, s);
  key := VirtualKeyCodeToMacString(w);
  shiftKeyMask := 0;
  if ssShift in s then
    ShiftKeyMask := ShiftKeyMask + NSShiftKeyMask;
  if ssAlt in s then
    ShiftKeyMask := ShiftKeyMask + NSAlternateKeyMask;
  if ssCtrl in s then
    ShiftKeyMask := ShiftKeyMask + NSControlKeyMask;
  if ssMeta in s then
    ShiftKeyMask := ShiftKeyMask + NSCommandKeyMask;
end;

function CFStringToString(AString: CFStringRef): String;
begin
  result:=CFStringToStr(AString);
end;

// Return the content view of a given non-view or
// for a view. For Window and Box and similar containers
// it returns the content view
function GetNSObjectView(obj: NSObject): NSView;
begin
  Result := nil;
  if not Assigned(obj) then Exit;
  if obj.isKindOfClass_(NSBox) then
    Result := NSBox(obj).contentView
  else if obj.isKindOfClass_(NSView) then
    Result := NSView(obj)
  else if obj.isKindOfClass_(NSWindow) then
    Result := NSWindow(obj).contentView
  else if obj.isKindOfClass_(NSTabViewItem) then
    Result := NSTabViewItem(obj).view;
end;

function GetNSObjectWindow(obj: NSObject): NSWindow;
var
  lView: NSView;
begin
  Result := nil;
  if not Assigned(obj) then Exit;
  if obj.isKindOfClass_(NSWindow) then
    Result := NSWindow(obj)
  else
  begin
    lView := GetNSObjectView(obj);
    if lView <> nil then Result := lView.window;
  end;
end;

function GetNSSize(width, height: CGFloat): NSSize; inline;
begin
  Result.height := height;
  Result.width := width;
end;

function GetNSPoint(x, y: single): NSPoint;
begin
  Result.x := x;
  Result.y := y;
end;

function LCLToNSPoint(APt: TPoint; ParentHeight: Single): NSPoint;
begin
  Result.X := APt.X;
  Result.Y := ParentHeight - APt.Y;
end;

function GetNSRect(x, y, width, height: Integer): NSRect;
begin
  with Result do
  begin
    origin.x := x;
    origin.y := y;
    size.width := width;
    size.height := height;
  end;
end;

function GetCGRect(x1, y1, x2, y2: Integer): CGRect;
begin
  with Result do
  begin
    origin.x := x1;
    origin.y := y1;
    size.width := x2 - x1;
    size.height := y2 - y1;
  end;
end;

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

function RectToCGRect(const R: TRect): CGRect;
begin
  with R do
    Result := GetCGRect(Left, Top, Right, Bottom);
end;

function CGRectToRect(const c: CGRect): TRect;
begin
  Result.Left := Round(c.origin.x);
  Result.Top := Round(c.origin.y);
  Result.Right := Round(c.origin.x + c.size.width);
  Result.Bottom := Round(c.origin.y + c.size.height);
end;

function RectToNSRect(const r: TRect): NSRect;
begin
  with R do
    Result := GetNSRect(Left, Top, Right - Left, Bottom - Top);
end;

function NSRectToRect(const NS: NSRect): TRect;
begin
  Result.Left := Round(ns.origin.x);
  Result.Top := Round(ns.origin.y);
  Result.Right := Round(ns.origin.x + ns.size.width);
  Result.Bottom := Round(ns.origin.y + ns.size.height);
end;

procedure NSToLCLRect(const ns: NSRect; ParentHeight: Single; out lcl: TRect);
begin
  lcl.Left := Round(ns.origin.x);
  lcl.Top := Round(ParentHeight - ns.size.height - ns.origin.y);
  lcl.Right := Round(ns.origin.x + ns.size.width);
  lcl.Bottom := Round(lcl.Top + ns.size.height);
end;

procedure LCLToNSRect(const lcl: TRect; ParentHeight: Single; out ns: NSRect);
begin
  ns.origin.x:=lcl.left;
  ns.origin.y:=ParentHeight-lcl.bottom;
  ns.size.width:=lcl.Right-lcl.Left;
  ns.size.height:=lcl.Bottom-lcl.Top;
end;

function LCLCoordsToCocoa(AControl: TControl; X, Y: Integer): NSPoint;
begin
  Result.x := X;
  Result.y := Screen.Height - Y;
  if AControl <> nil then Result.y := Result.y - AControl.Height;
end;

function CreateParamsToNSRect(const params: TCreateParams): NSRect;
begin
  with params do Result:=GetNSRect(X,Y,Width,Height);
end;

function NSStringUtf8(s: PChar): NSString;
var
  cf: CFStringRef;
  r: Integer;
begin
  {NSString and CFStringRef are interchangable}
  cf := CFStringCreateWithCString(nil, S, kCFStringEncodingUTF8);
  Result := NSString(cf);
end;

function NSStringUtf8(const s: String): NSString;
var
  cf: CFStringRef;
begin
  {NSString and CFStringRef are interchangable}
  cf := CFStringCreateWithCString(nil, Pointer(PChar(S)), kCFStringEncodingUTF8);
  Result := NSString(cf);
end;

function NSStringToString(ns: NSString): String;
begin
  Result := CFStringToStr(CFStringRef(ns));
end;

procedure SetNSText(text: NSText; const s: String); inline;
var
  ns: NSString;
begin
  if Assigned(text) then
  begin
    ns := NSStringUTF8(s);
    text.setString(ns);
    ns.release;
  end;
end;

function GetNSText(text: NSText): string; inline;
begin
  if Assigned(text) then
    Result := NSStringToString(text.string_)
  else
    Result := '';
end;

procedure SetNSControlValue(c: NSControl; const S: String); inline;
var
  ns: NSString;
begin
  if Assigned(c) then
  begin
    ns := NSStringUtf8(S);
    c.setStringValue(ns);
    ns.release;
  end;
end;

function GetNSControlValue(c: NSControl): String; inline;
begin
  if Assigned(c) then
    Result := NSStringToString(c.stringValue)
  else
    Result := '';
end;


{ NSLCLDebugExtension }

function NSLCLDebugExtension.lclClassName: shortstring;
begin
  Result := NSStringToString(self.className);
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
  VK_SHIFT     : Result := MK_SHIFTKEY;
  VK_CONTROL   : Result := MK_COMMAND;
  VK_MENU      : Result := MK_ALT;
  VK_OEM_3     : Result := MK_TILDE;
  VK_OEM_MINUS : Result := MK_MINUS;
  VK_OEM_PLUS  : Result := MK_EQUAL;
  VK_OEM_5     : Result := MK_BACKSLASH;
  VK_OEM_4     : Result := MK_LEFTBRACKET;
  VK_OEM_6     : Result := MK_RIGHTBRACKET;
  VK_OEM_1     : Result := MK_SEMICOLON;
  VK_OEM_7     : Result := MK_QUOTE;
  VK_OEM_COMMA : Result := MK_COMMA;
  VK_OEM_PERIOD: Result := MK_PERIOD;
  VK_OEM_2     : Result := MK_SLASH;
  else
    Result := 0;
  end;
end;

function VirtualKeyCodeToMacString(AKey: Word): NSString;
type
  WideChar = System.WideChar;
var
  w : WideChar;
begin
  w:=#0;
  case AKey of
  VK_MULTIPLY  : w := '*';
  VK_ADD, VK_OEM_PLUS       : w := '+';
  VK_SUBTRACT, VK_OEM_MINUS : w := '-';
  VK_OEM_COMMA : w := ',';
  VK_OEM_PERIOD: w := '.';
  VK_OEM_1     : w := ';';
  VK_OEM_2     : w := '/';
  VK_OEM_3     : w := '`';
  VK_OEM_4     : w := '[';
  VK_OEM_5     : w := '\';
  VK_OEM_6     : w := ']';
  VK_OEM_7     : w := '''';
  VK_BACK      : w := WideChar(NSBackspaceCharacter);
  VK_CLEAR     : w := WideChar(NSClearDisplayFunctionKey);
  VK_PAUSE     : w := WideChar(NSPauseFunctionKey);
  VK_PRIOR     : w := WideChar(NSPageUpFunctionKey);
  VK_NEXT      : w := WideChar(NSPageDownFunctionKey);
  VK_END       : w := WideChar(NSEndFunctionKey);
  VK_HOME      : w := WideChar(NSHomeFunctionKey);
  VK_LEFT      : w := WideChar(NSLeftArrowFunctionKey);
  VK_UP        : w := WideChar(NSUpArrowFunctionKey);
  VK_RIGHT     : w := WideChar(NSRightArrowFunctionKey);
  VK_DOWN      : w := WideChar(NSDownArrowFunctionKey);
  VK_SELECT    : w := WideChar(NSSelectFunctionKey);
  VK_PRINT     : w := WideChar(NSPrintFunctionKey);
  VK_EXECUTE   : w := WideChar(NSExecuteFunctionKey);
  VK_INSERT    : w := WideChar(NSInsertFunctionKey);
  VK_DELETE    : w := WideChar(NSDeleteCharacter);
  VK_HELP      : w := WideChar(NSHelpFunctionKey);
  VK_SCROLL    : w := WideChar(NSScrollLockFunctionKey);
  VK_F1..VK_F24: w := WideChar(NSF1FunctionKey + AKey - VK_F1);
  VK_A..VK_Z   : w := WideChar(Ord('a') + AKey - VK_A);
  else
    w := WideChar(AKey and $ff);
  end;
  if w<>#0
    then Result:=NSString.stringWithCharacters_length(@w, 1)
    else Result:=NSString.string_;
end;
{------------------------------------------------------------------------------
  Name:    FillStandardDescription
  Params:  Desc - Raw image description

  Fills the raw image description with standard Cocoa internal image storing
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
      Ord('?'), False, nil, 0, StrSize);
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

function GetCurrentEventTime: double;
// returns seconds since system startup
begin
  Result := AbsoluteToDuration(UpTime) / 1000.0;
end;

function GetMacOSXVersion: Integer;
var
  lVersionNSStr: NSString;
  lVersionStr: string;
  lParser: TStringList;
  lMajor: integer = 0;
  lMinor: integer = 0;
  lFix: integer = 0;
begin
  Result := 0;
  lVersionNSStr := NSProcessInfo.processInfo.operatingSystemVersionString;
  lVersionStr := NSStringToString(lVersionNSStr);
  lParser := TStringList.Create;
  try
    lParser.Delimiter := ' ';
    lParser.DelimitedText := lVersionStr;
    if lParser.Count >= 2 then
    begin
      lVersionStr := lParser.Strings[1];
      lParser.Delimiter := '.';
      lParser.DelimitedText := lVersionStr;
      if lParser.Count = 3 then
      begin
        TryStrToInt(lParser.Strings[0], lMajor);
        TryStrToInt(lParser.Strings[1], lMinor);
        TryStrToInt(lParser.Strings[2], lFix);
      end;
    end;
  finally
    lParser.Free;
  end;
  Result := lMajor*$10000 + lMinor*$100 + lFix;
end;

procedure NSResponderHotKeys(trg: NSResponder; event: NSEvent; var handled: Boolean);
begin
  // todo: system keys could be overriden. thus need to review the current
  //       keyboard configuration first. See "Key Bindings" at
  //       https://developer.apple.com/library/content/documentation/Cocoa/Conceptual/EventOverview/TextDefaultsBindings/TextDefaultsBindings.html

  handled := false;
  if (event.type_ = NSKeyDown) then
  begin
    if ((event.modifierFlags and NSCommandKeyMask) = 0) then Exit;

    if Assigned(event.charactersIgnoringModifiers.UTF8String) then
    begin
      case event.charactersIgnoringModifiers.UTF8String^ of
        'Z': handled := NSApplication(NSApp).sendAction_to_from(objcselector('redo:'), nil, trg);
        'a': handled := NSApplication(NSApp).sendAction_to_from(objcselector('selectAll:'), nil, trg);
        'c': handled := NSApplication(NSApp).sendAction_to_from(objcselector('copy:'), nil, trg);
        'v': handled := NSApplication(NSApp).sendAction_to_from(objcselector('paste:'), nil, trg);
        'x': handled := NSApplication(NSApp).sendAction_to_from(objcselector('cut:'), nil, trg);
        'z': handled := NSApplication(NSApp).sendAction_to_from(objcselector('undo:'), nil, trg);
      end;
    end;
  end;
end;

function DateTimeToNSDate(const aDateTime : TDateTime): NSDate;
var
  ti : NSTimeInterval;
  d  : NSDate;
begin
  ti := (aDateTime - EncodeDate(2001, 1, 1)) * SecsPerDay;
  d  := NSDate.alloc.init;
  Result:= d.dateWithTimeIntervalSinceReferenceDate(ti);
end;

function NSDateToDateTime(const aDateTime: NSDate): TDateTime;
begin
  if aDateTime = nil then
  begin
    Result:= 0.0;
    Exit;
  end;
  Result:= aDateTime.timeIntervalSince1970 / SecsPerDay + EncodeDate(1970, 1, 1);
end;


end.

