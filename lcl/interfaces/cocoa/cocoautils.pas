unit CocoaUtils;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  classes,
  MacOSAll, CocoaAll, Cocoa_Extra,
  SysUtils, Types, LCLType, LCLClasses, LCLProc,
  Graphics, Math, GraphType;

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

const
  DEFAULT_CFSTRING_ENCODING = kCFStringEncodingUTF8;

function CFStringToStr(AString: CFStringRef; Encoding: CFStringEncoding = DEFAULT_CFSTRING_ENCODING): String;
function CFStringToString(AString: CFStringRef): String;

// Missing things from NSTableColumns.inc
const
  NSTableColumnAutoresizingMask = 1 shl 0;
  NSTableColumnUserResizingMask = 1 shl 1;

function VirtualKeyCodeToMacString(AKey: Word): NSString;

procedure FillStandardDescription(out Desc: TRawImageDescription);

procedure CreateCFString(const S: String; out AString: CFStringRef);
procedure CreateCFString(const Data: CFDataRef; Encoding: CFStringEncoding; out AString: CFStringRef);
procedure FreeCFString(var AString: CFStringRef);
function CFStringToData(AString: CFStringRef; Encoding: CFStringEncoding = DEFAULT_CFSTRING_ENCODING): CFDataRef;

function GetCurrentEventTime: double;
function GetMacOSXVersion: Integer;

function DateTimeToNSDate(const aDateTime : TDateTime): NSDate;
function NSDateToDateTime(const aDateTime: NSDate): TDateTime;

// The function removes single & and replaced && with &
// (removing LCL (Windows) specific caption convention
function ControlTitleToStr(const ATitle: string): String;
// The returned NSString doesn't require a release
// (it would happen in NSAutoRelease pool)
function ControlTitleToNSStr(const ATitle: string): NSString;

procedure AddLayoutToFrame(const layout: TRect; var frame: TRect);
procedure SubLayoutFromFrame(const layout: TRect; var frame: TRect);

// MacOSX Virtual Key Codes missing from MacOSAll.Events.pas
const
  kVK_SubMenu = $6E;

function MacCodeToVK(AKey: Word): Word;

function MacCharToVK(achar: unichar): Word;

procedure ApplicationWillShowModal;

const
  // Shift, Control, Alt and Command
  KeysModifiers = NSShiftKeyMask or NSControlKeyMask or NSAlternateKeyMask or NSCommandKeyMask;

function NSEventRawKeyChar(ev: NSEvent): System.WideChar;

function AllocImageRotatedByDegrees(src: NSImage; degrees: double): NSImage;
function AllocCursorFromCursorByDegrees(src: NSCursor; degrees: double): NSCursor;

type

  { TCocoaInputClient }

  TCocoaInputClient = objcclass(NSObject, NSTextInputClientProtocol)
    procedure insertText_replacementRange(aString: id; replacementRange: NSRange);
    procedure setMarkedText_selectedRange_replacementRange(aString: id; selectedRange: NSRange; replacementRange: NSRange);
    procedure unmarkText;
    function selectedRange: NSRange;
    function markedRange: NSRange;
    function hasMarkedText: LCLObjCBoolean;
    function attributedSubstringForProposedRange_actualRange(aRange: NSRange; actualRange: NSRangePointer): NSAttributedString;
    function validAttributesForMarkedText: NSArray;
    function firstRectForCharacterRange_actualRange(aRange: NSRange; actualRange: NSRangePointer): NSRect;
    function characterIndexForPoint(aPoint: NSPoint): NSUInteger;

    procedure doCommandBySelector(asel: sel); message 'doCommandBySelector:';
  end;

implementation

procedure ApplicationWillShowModal;
begin
  // Any place that would attempt to use Cocoa-native modality.
  // should call this routine, prior to the call
  // This is a workaround for AppKit drawing approaches

  // hack: it's assumed that an implicit transaction is running at the moment
  //       for versions 10.7 and later it's possible to add an end-transacion
  //       block. But since blocks are not yet, if official FPC release
  //       the approach is not used
  //
  //       the code takes care of all modal windows
  //
  //       The hack is commented out, as it doesn't work on some machines

  //if NSAppkitversionNumber >= NSAppKitVersionNumber10_12 then
    //NSAnimationContext.endGrouping;

  // If transaction is not terminated by calling "endGrouping"
  // the typical error shown is:
  //
  //  *** Terminating app due to uncaught exception 'NSGenericException',
  //  reason: '-[NSApplication runModalForWindow:] may not be invoked
  //  inside of transaction begin/commit pair, or inside of transaction
  //  commit (usually this means it was invoked inside of a view's -drawRect: method.)'
  //  terminating with uncaught exception of type NSException
  //  abort() called
end;

function MacCodeToVK(AKey: Word): Word;
begin
  case AKey of
    kVK_ANSI_A : Result :=  VK_A;
    kVK_ANSI_S : Result :=  VK_S;
    kVK_ANSI_D : Result :=  VK_D;
    kVK_ANSI_F : Result :=  VK_F;
    kVK_ANSI_H : Result :=  VK_H;
    kVK_ANSI_G : Result :=  VK_G;
    kVK_ANSI_Z : Result :=  VK_Z;
    kVK_ANSI_X : Result :=  VK_X;
    kVK_ANSI_C : Result :=  VK_C;
    kVK_ANSI_V : Result :=  VK_V;

    //kVK_ISO_Section   // ISO keyboard only

    kVK_ANSI_B : Result := VK_B;
    kVK_ANSI_Q : Result := VK_Q;
    kVK_ANSI_W : Result := VK_W;
    kVK_ANSI_E : Result := VK_E;
    kVK_ANSI_R : Result := VK_R;
    kVK_ANSI_Y : Result := VK_Y;
    kVK_ANSI_T : Result := VK_T;
    kVK_ANSI_1 : Result := VK_1;
    kVK_ANSI_2 : Result := VK_2;
    kVK_ANSI_3 : Result := VK_3;
    kVK_ANSI_4 : Result := VK_4;
    kVK_ANSI_6 : Result := VK_6;
    kVK_ANSI_5 : Result := VK_5;
    kVK_ANSI_Equal        : Result := VK_LCL_EQUAL; // aka VK_EQUAL = 187 = $BB;
    kVK_ANSI_9            : Result := VK_9;
    kVK_ANSI_7            : Result := VK_7;
    kVK_ANSI_Minus        : Result := VK_OEM_MINUS;
    kVK_ANSI_8            : Result := VK_8;
    kVK_ANSI_0            : Result := VK_0;
    kVK_ANSI_RightBracket : Result := VK_OEM_6;
    kVK_ANSI_O            : Result := VK_O;
    kVK_ANSI_U            : Result := VK_U;
    kVK_ANSI_LeftBracket  : Result := VK_LCL_OPEN_BRAKET;
    kVK_ANSI_I            : Result := VK_I;
    kVK_ANSI_P            : Result := VK_P;

    kVK_Return            : Result := VK_RETURN;

    kVK_ANSI_L            : Result := VK_L;
    kVK_ANSI_J            : Result := VK_J;
    kVK_ANSI_Quote        : Result := VK_LCL_QUOTE;
    kVK_ANSI_K            : Result := VK_K;
    kVK_ANSI_Semicolon    : Result := VK_LCL_SEMI_COMMA;
    kVK_ANSI_Backslash    : Result := VK_LCL_BACKSLASH;
    kVK_ANSI_Comma        : Result := VK_LCL_COMMA;
    kVK_ANSI_Slash        : Result := VK_LCL_SLASH;
    kVK_ANSI_N            : Result := VK_N;
    kVK_ANSI_M            : Result := VK_M;
    kVK_ANSI_Period       : Result := VK_LCL_POINT;

    kVK_Tab               : Result := VK_TAB;
    kVK_Space             : Result := VK_SPACE;

    kVK_ANSI_Grave        : Result := VK_LCL_TILDE;

    kVK_Delete            : Result := VK_BACK; // or VK_DELETE?

    kVK_Escape            : Result := VK_ESCAPE;
    kVK_Command           : Result := VK_LWIN;
    // todo: Application.ExtendedKeysSupport must be true!
    kVK_Shift             : Result := VK_LSHIFT; // VK_SHIFT?
    kVK_CapsLock          : Result := VK_CAPITAL;
    kVK_Option            : Result := VK_LMENU;
    kVK_Control           : Result := VK_LCONTROL;
    kVK_RightShift        : Result := VK_RSHIFT;
    kVK_RightOption       : Result := VK_RMENU;
    kVK_RightControl      : Result := VK_RCONTROL;
    //kVK_Function          : Result := VK_; todo:
    kVK_F17               : Result := VK_F17;

    kVK_ANSI_KeypadDecimal  : Result := VK_DECIMAL;
    kVK_ANSI_KeypadMultiply : Result := VK_MULTIPLY;
    kVK_ANSI_KeypadPlus     : Result := VK_ADD;
    kVK_ANSI_KeypadClear    : Result := VK_NUMLOCK;

    kVK_VolumeUp    : Result := VK_VOLUME_UP;
    kVK_VolumeDown  : Result := VK_VOLUME_DOWN;
    kVK_Mute        : Result := VK_VOLUME_MUTE;

    kVK_ANSI_KeypadDivide   : Result := VK_DIVIDE;
    kVK_ANSI_KeypadEnter    : Result := VK_RETURN;
    kVK_ANSI_KeypadMinus    : Result := VK_SUBTRACT;

    kVK_F18         : Result := VK_F18;
    kVK_F19         : Result := VK_F19;

    //kVK_ANSI_KeypadEquals : Result := VK_;
    kVK_ANSI_Keypad0      : Result := VK_NUMPAD0;
    kVK_ANSI_Keypad1      : Result := VK_NUMPAD1;
    kVK_ANSI_Keypad2      : Result := VK_NUMPAD2;
    kVK_ANSI_Keypad3      : Result := VK_NUMPAD3;
    kVK_ANSI_Keypad4      : Result := VK_NUMPAD4;
    kVK_ANSI_Keypad5      : Result := VK_NUMPAD5;
    kVK_ANSI_Keypad6      : Result := VK_NUMPAD6;
    kVK_ANSI_Keypad7      : Result := VK_NUMPAD7;

    kVK_F20  : Result := VK_F20;

    kVK_ANSI_Keypad8 : Result := VK_NUMPAD8;
    kVK_ANSI_Keypad9 : Result := VK_NUMPAD9;

    //kVK_JIS_Yen                   = $5D;
    //kVK_JIS_Underscore            = $5E;
    //kVK_JIS_KeypadComma           = $5F;

    kVK_F5           : Result := VK_F5;
    kVK_F6           : Result := VK_F6;
    kVK_F7           : Result := VK_F7;
    kVK_F3           : Result := VK_F3;
    kVK_F8           : Result := VK_F8;
    kVK_F9           : Result := VK_F9;

    //kVK_JIS_Eisu                  = $66;
    kVK_JIS_Kana      : Result := VK_KANA;

    kVK_F11           : Result := VK_F11;

    kVK_F13           : Result := VK_SNAPSHOT;
    kVK_F16           : Result := VK_F16;
    kVK_F14           : Result := VK_SCROLL;
    kVK_F10           : Result := VK_F10;

    kVK_SubMenu       : Result := VK_APPS;

    kVK_F12           : Result := VK_F12;
    kVK_F15           : Result := VK_PAUSE;
    kVK_Help          : Result := VK_HELP; //VK_INSERT; // todo!
    kVK_Home          : Result := VK_HOME;
    kVK_PageUp        : Result := VK_PRIOR;
    kVK_ForwardDelete : Result := VK_DELETE; // VK_BACK?
    kVK_F4            : Result := VK_F4;
    kVK_End           : Result := VK_END;
    kVK_F2            : Result := VK_F2;
    kVK_PageDown      : Result := VK_NEXT;
    kVK_F1            : Result := VK_F1;
    kVK_LeftArrow     : Result := VK_LEFT;
    kVK_RightArrow    : Result := VK_RIGHT;
    kVK_DownArrow     : Result := VK_DOWN;
    kVK_UpArrow       : Result := VK_UP;

  else
    Result := VK_UNKNOWN;
  end;
end;

function MacCharToVK(achar: unichar): Word;
var
  ch : AnsiChar;
begin
  // only handle printable characters here in Ansi range
  // all other should be taken care of in MacCodeToVk
  if (achar < 32) or (achar>127) then begin
    Result := VK_UNKNOWN;
    Exit;
  end;
  ch := AnsiChar(achar and $FF);
  case ch of
    'a'..'z': Result := VK_A+ord(ch)-ord('a');
    'A'..'Z': Result := ord(ch);
    '`','~':  Result := VK_LCL_TILDE;
    '[','{':  Result := VK_LCL_OPEN_BRAKET;
    ']','}':  Result := VK_LCL_CLOSE_BRAKET;
    '\','|':  Result := VK_LCL_BACKSLASH;
    ';',':':  Result := VK_LCL_SEMI_COMMA;
    '''','"': Result := VK_LCL_QUOTE;
    ',','<':  Result := VK_LCL_COMMA;
    '>','.':  Result := VK_LCL_POINT;
    // make sure that KeyCodes are not related to numpad
    '=','+':  Result := VK_LCL_EQUAL;
    '-','_':  Result := VK_LCL_MINUS;
    '/','?':  Result := VK_LCL_SLASH;
  else
    Result := VK_UNKNOWN;
  end;
end;

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
var
  alpha: CGFloat;
begin
  // TColorRef doesn't bear an alpha channel information.
  // Thus RGB needs to be multiplied by it.
  alpha := Color.alphaComponent;
  with Color do
    Result := RGBToColorFloat(redComponent*alpha, greenComponent*alpha, blueComponent*alpha);
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
        Result := 0; // getting rid of compiler warning
        for y := 0 to ImageRep.pixelsHigh - 1 do
          for x := 0 to ImageRep.pixelsWide - 1 do
          begin
            RGBColor := NSBitmapImageRep(ImageRep).colorAtX_y(x, y).colorUsingColorSpaceName(NSDeviceRGBColorSpace);
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
  if CGRectIsEmpty(c) <> 0 then
    Result := Rect(0,0,0,0)
  else if CGRectIsInfinite(c) <> 0 then
    Result:= Rect(Low(Integer), Low(Integer), High(Integer), High(Integer))
  else begin
  Result.Left := Round(c.origin.x);
  Result.Top := Round(c.origin.y);
  Result.Right := Round(c.origin.x + c.size.width);
  Result.Bottom := Round(c.origin.y + c.size.height);
end;
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

{ TCocoaInputClient }

procedure TCocoaInputClient.insertText_replacementRange(aString: id;
  replacementRange: NSRange);
begin

end;

procedure TCocoaInputClient.setMarkedText_selectedRange_replacementRange(
  aString: id; selectedRange: NSRange; replacementRange: NSRange);
begin

end;

procedure TCocoaInputClient.unmarkText;
begin

end;

function TCocoaInputClient.selectedRange: NSRange;
begin
  Result.location := 0;
  Result.length := 0;
end;

function TCocoaInputClient.markedRange: NSRange;
begin
  Result.location := 0;
  Result.length := 0;
end;

function TCocoaInputClient.hasMarkedText: LCLObjCBoolean;
begin
  Result := false;
end;

function TCocoaInputClient.attributedSubstringForProposedRange_actualRange(
  aRange: NSRange; actualRange: NSRangePointer): NSAttributedString;
begin
  Result := nil;
end;

function TCocoaInputClient.validAttributesForMarkedText: NSArray;
begin
  Result := nil;
end;

function TCocoaInputClient.firstRectForCharacterRange_actualRange(
  aRange: NSRange; actualRange: NSRangePointer): NSRect;
begin
  Result := NSZeroRect;
end;

function TCocoaInputClient.characterIndexForPoint(aPoint: NSPoint): NSUInteger;
begin
  Result := 0;
end;

procedure TCocoaInputClient.doCommandBySelector(asel: sel);
begin

end;


{ NSLCLDebugExtension }

function NSLCLDebugExtension.lclClassName: shortstring;
begin
  Result := NSStringToString(self.className);
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

function ControlTitleToStr(const ATitle: string): String;
begin
  Result := ATitle;
  DeleteAmpersands(Result);
end;

function ControlTitleToNSStr(const ATitle: string): NSString;
var
  t: string;
begin
  t := ControlTitleToStr(ATitle);
  if t = '' then Result:=NSString.string_ // empty string
  else Result := NSString.stringWithUTF8String( @t[1] );
end;

procedure AddLayoutToFrame(const layout: TRect; var frame: TRect);
begin
  inc(frame.Left, layout.Left);
  inc(frame.Top, layout.Top);
  inc(frame.Right, layout.Right);
  inc(frame.Bottom, layout.Bottom);
end;

procedure SubLayoutFromFrame(const layout: TRect; var frame: TRect);
begin
  dec(frame.Left, layout.Left);
  dec(frame.Top, layout.Top);
  dec(frame.Right, layout.Right);
  dec(frame.Bottom, layout.Bottom);
end;

function NSEventRawKeyChar(ev: NSEvent): System.WideChar;
var
  m : NSString;
begin
  m := ev.charactersIgnoringModifiers;
  if m.length <> 1 then
    Result := #0
  else
    Result := System.WideChar(m.characterAtIndex(0));
end;

function AllocImageRotatedByDegrees(src: NSImage; degrees: double): NSImage;
var
  imageBounds : NSRect;
  pathBounds  : NSBezierPath;
  transform   : NSAffineTransform;
  rotatedBounds : NSRect;
  rotatedImage   : NSImage;
begin
  if not Assigned(src) then
  begin
    Result := nil;
    Exit;
  end;

  // src: https://stackoverflow.com/questions/31699235/rotate-nsimage-in-swift-cocoa-mac-osx

  imageBounds.size := src.size;
  pathBounds := NSBezierPath.bezierPathWithRect(imageBounds);
  transform := NSAffineTransform.alloc.init;
  transform.rotatebyDegrees(degrees);
  pathBounds.transformUsingAffineTransform(transform);
  rotatedBounds := NSMakeRect(NSZeroPoint.x, NSZeroPoint.y, src.size.width, src.size.height );
  rotatedImage := NSImage(NSImage.alloc).initWithSize(rotatedBounds.size);

  //Center the image within the rotated bounds
  imageBounds.origin.x := NSMidX(rotatedBounds) - (NSWidth(imageBounds) / 2);
  imageBounds.origin.y := NSMidY(rotatedBounds) - (NSHeight(imageBounds) / 2);
  transform.release;

  // Start a new transform
  transform := NSAffineTransform.alloc.init;
  // Move coordinate system to the center (since we want to rotate around the center)
  transform.translateXBy_yBy(rotatedBounds.size.width / 2, rotatedBounds.size.width / 2);
  transform.rotateByDegrees(degrees);
  // Move the coordinate system bak to normal
  transform.translateXBy_yBy(-rotatedBounds.size.width / 2, -rotatedBounds.size.height / 2);
  // Draw the original image, rotated, into the new image
  rotatedImage.lockFocus;
  transform.concat();
  src.drawInRect_fromRect_operation_fraction(imageBounds, NSZeroRect, NSCompositeCopy, 1.0);
  rotatedImage.unlockFocus();
  Result := rotatedImage;

  transform.release;
end;

function AllocCursorFromCursorByDegrees(src: NSCursor; degrees: double): NSCursor;
var
  img : NSImage;
begin
  img := AllocImageRotatedByDegrees(src.image, degrees);
  //todo: a better hotspot detection
  Result := NSCursor.alloc.initWithImage_hotSpot(
    img,
    NSMakePoint(img.size.height / 2, img.size.width / 2)
  );
  img.release;
end;

end.

