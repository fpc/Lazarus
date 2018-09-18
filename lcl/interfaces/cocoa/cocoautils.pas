unit CocoaUtils;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  classes,
  MacOSAll, CocoaAll,
  SysUtils, Types, LCLType, LCLClasses, LCLProc,
  Graphics, Math, GraphType;

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

procedure NSResponderHotKeys(asender: NSResponder; event: NSEvent; var handled: Boolean; atarget: id = nil);

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

  // MacOSX Virtual Key Codes
const
  kVK_ANSI_A                    = $00;
  kVK_ANSI_S                    = $01;
  kVK_ANSI_D                    = $02;
  kVK_ANSI_F                    = $03;
  kVK_ANSI_H                    = $04;
  kVK_ANSI_G                    = $05;
  kVK_ANSI_Z                    = $06;
  kVK_ANSI_X                    = $07;
  kVK_ANSI_C                    = $08;
  kVK_ANSI_V                    = $09;

  kVK_ISO_Section               = $0A; // ISO keyboard only

  kVK_ANSI_B                    = $0B;
  kVK_ANSI_Q                    = $0C;
  kVK_ANSI_W                    = $0D;
  kVK_ANSI_E                    = $0E;
  kVK_ANSI_R                    = $0F;
  kVK_ANSI_Y                    = $10;
  kVK_ANSI_T                    = $11;
  kVK_ANSI_1                    = $12;
  kVK_ANSI_2                    = $13;
  kVK_ANSI_3                    = $14;
  kVK_ANSI_4                    = $15;
  kVK_ANSI_6                    = $16;
  kVK_ANSI_5                    = $17;
  kVK_ANSI_Equal                = $18;
  kVK_ANSI_9                    = $19;
  kVK_ANSI_7                    = $1A;
  kVK_ANSI_Minus                = $1B;
  kVK_ANSI_8                    = $1C;
  kVK_ANSI_0                    = $1D;
  kVK_ANSI_RightBracket         = $1E;
  kVK_ANSI_O                    = $1F;
  kVK_ANSI_U                    = $20;
  kVK_ANSI_LeftBracket          = $21;
  kVK_ANSI_I                    = $22;
  kVK_ANSI_P                    = $23;

  kVK_Return                    = $24;

  kVK_ANSI_L                    = $25;
  kVK_ANSI_J                    = $26;
  kVK_ANSI_Quote                = $27;
  kVK_ANSI_K                    = $28;
  kVK_ANSI_Semicolon            = $29;
  kVK_ANSI_Backslash            = $2A;
  kVK_ANSI_Comma                = $2B;
  kVK_ANSI_Slash                = $2C;
  kVK_ANSI_N                    = $2D;
  kVK_ANSI_M                    = $2E;
  kVK_ANSI_Period               = $2F;

  kVK_Tab                       = $30;
  kVK_Space                     = $31;

  kVK_ANSI_Grave                = $32;

  kVK_Delete                    = $33;

  kVK_Escape                    = $35;
  kVK_Command                   = $37;
  kVK_Shift                     = $38;
  kVK_CapsLock                  = $39;
  kVK_Option                    = $3A;
  kVK_Control                   = $3B;
  kVK_RightShift                = $3C;
  kVK_RightOption               = $3D;
  kVK_RightControl              = $3E;
  kVK_Function                  = $3F;
  kVK_F17                       = $40;

  kVK_ANSI_KeypadDecimal        = $41;
  kVK_ANSI_KeypadMultiply       = $43;
  kVK_ANSI_KeypadPlus           = $45;
  kVK_ANSI_KeypadClear          = $47; // Numlock

  kVK_VolumeUp                  = $48;
  kVK_VolumeDown                = $49;
  kVK_Mute                      = $4A;

  kVK_ANSI_KeypadDivide         = $4B;
  kVK_ANSI_KeypadEnter          = $4C;
  kVK_ANSI_KeypadMinus          = $4E;

  kVK_F18                       = $4F;
  kVK_F19                       = $50;

  kVK_ANSI_KeypadEquals         = $51;
  kVK_ANSI_Keypad0              = $52;
  kVK_ANSI_Keypad1              = $53;
  kVK_ANSI_Keypad2              = $54;
  kVK_ANSI_Keypad3              = $55;
  kVK_ANSI_Keypad4              = $56;
  kVK_ANSI_Keypad5              = $57;
  kVK_ANSI_Keypad6              = $58;
  kVK_ANSI_Keypad7              = $59;

  kVK_F20                       = $5A;

  kVK_ANSI_Keypad8              = $5B;
  kVK_ANSI_Keypad9              = $5C;

  kVK_JIS_Yen                   = $5D;
  kVK_JIS_Underscore            = $5E;
  kVK_JIS_KeypadComma           = $5F;

  kVK_F5                        = $60;
  kVK_F6                        = $61;
  kVK_F7                        = $62;
  kVK_F3                        = $63;
  kVK_F8                        = $64;
  kVK_F9                        = $65;

  kVK_JIS_Eisu                  = $66;
  kVK_JIS_Kana                  = $68;

  kVK_F11                       = $67;

  kVK_F13                       = $69; // Print Screen
  kVK_F16                       = $6A;
  kVK_F14                       = $6B; // Scroll Lock
  kVK_F10                       = $6D;

  kVK_SubMenu = $6E;

  kVK_F12                       = $6F;
  kVK_F15                       = $71; // Pause
  kVK_Help                      = $72;
  kVK_Home                      = $73;
  kVK_PageUp                    = $74;
  kVK_ForwardDelete             = $75;
  kVK_F4                        = $76;
  kVK_End                       = $77;
  kVK_F2                        = $78;
  kVK_PageDown                  = $79;
  kVK_F1                        = $7A;
  kVK_LeftArrow                 = $7B;
  kVK_RightArrow                = $7C;
  kVK_DownArrow                 = $7D;
  kVK_UpArrow                   = $7E;

function MacCodeToVK(AKey: Word): Word;

implementation

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
        Result := 0; // getting rid of compiler warning
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

procedure NSResponderHotKeys(asender: NSResponder; event: NSEvent; var handled: Boolean; atarget: id);
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
        // redo/undo are not implemented in either of TextControls?
        //'Z': handled := NSApplication(NSApp).sendAction_to_from(objcselector('redo:'), atarget, asender);
        'a': handled := NSApplication(NSApp).sendAction_to_from(objcselector('selectAll:'), atarget, asender);
        'c': handled := NSApplication(NSApp).sendAction_to_from(objcselector('copy:'), atarget, asender);
        'v': handled := NSApplication(NSApp).sendAction_to_from(objcselector('paste:'), atarget, asender);
        'x': handled := NSApplication(NSApp).sendAction_to_from(objcselector('cut:'), atarget, asender);
        //'z': handled := NSApplication(NSApp).sendAction_to_from(objcselector('undo:'), atarget, asender);
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

end.

