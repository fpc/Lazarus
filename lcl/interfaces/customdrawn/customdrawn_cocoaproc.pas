unit customdrawn_cocoaproc;

{$mode objfpc}{$H+}
{$include customdrawndefines.inc}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils, Math,
  CGGeometry,
  fpimage, fpcanvas,
  // Custom Drawn Canvas
  IntfGraphics, lazcanvas, customdrawnproc,
  // Libs
  MacOSAll, CocoaAll, CocoaUtils, CocoaGDIObjects, LazSysUtils,
  //
  LMessages, StdCtrls, LCLIntf,
  Forms, Controls, LCLMessageGlue, WSControls, LCLType, LCLProc, GraphType;

type
  TCocoaForm = objcclass;
  TCocoaCustomControl = objcclass;

  TCocoaWindow = class(TCDForm)
  public
    CocoaForm: TCocoaForm;
    ClientArea: TCocoaCustomControl;
  end;

  { TCocoaForm }

  TCocoaForm = objcclass(NSWindow, NSWindowDelegateProtocol)
  protected
    function windowShouldClose(sender : id): LongBool; message 'windowShouldClose:';
    procedure windowWillClose(notification: NSNotification); message 'windowWillClose:';
    procedure windowDidBecomeKey(notification: NSNotification); message 'windowDidBecomeKey:';
    procedure windowDidResignKey(notification: NSNotification); message 'windowDidResignKey:';
    procedure windowDidResize(notification: NSNotification); message 'windowDidResize:';
  public
    WindowHandle: TCocoaWindow;
    function acceptsFirstResponder: Boolean; override;
    // Mouse events
    procedure mouseUp(event: NSEvent); override;
    procedure mouseDown(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    // Keyboard events
    procedure keyDown(theEvent: NSEvent); override;
    procedure keyUp(theEvent: NSEvent); override;
    function MacKeyCodeToLCLKey(AKeyEvent: NSEvent; var SendKeyUpDown, SendChar: Boolean; var AUTF8Char: TUTF8Char): Word; message 'MacKeyCodeToLCLKey:sendkey:sendchar:AUTF8Char:';
    //
    function lclIsVisible: Boolean; message 'lclIsVisible';
    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:';
    procedure lclInvalidate; message 'lclInvalidate';
    procedure lclLocalToScreen(var X,Y: Integer); message 'lclLocalToScreen::';
    function lclFrame: TRect; message 'lclFrame';
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:';
    function lclClientFrame: TRect; message 'lclClientFrame';
    procedure lclSetClientFrame(const r: TRect); message 'lclSetClientFrame:';
    // callback routines
    procedure CallbackActivate; message 'CallbackActivate';
    procedure CallbackDeactivate; message 'CallbackDeactivate';
    procedure CallbackCloseQuery(var CanClose: Boolean); message 'CallbackCloseQuery:';
    procedure CallbackResize; message 'CallbackResize';
    // Accessibility
    function accessibilityAttributeValue(attribute: NSString): id; override;
  end;

  { TCocoaCustomControl }

  TCocoaCustomControl = objcclass(NSControl)
  public
    //callback  : TCommonCallback;
    WindowHandle: TCocoaWindow;
    Context : TCocoaContext;
    procedure drawRect(dirtyRect: NSRect); override;
    procedure Draw(ControlContext: NSGraphicsContext; const Abounds, dirty:NSRect); message 'draw:Context:bounds:';
  public
    // Keyboard events
    function acceptsFirstResponder: Boolean; override;
    procedure keyDown(theEvent: NSEvent); override;
    procedure keyUp(theEvent: NSEvent); override;
    //
    function lclInitWithCreateParams(const AParams: TCreateParams): id; message 'lclInitWithCreateParams:';
    //
    function lclIsVisible: Boolean; message 'lclIsVisible';
    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:';
    procedure lclInvalidate; message 'lclInvalidate';
    procedure lclLocalToScreen(var X,Y: Integer); message 'lclLocalToScreen::';
    function lclParent: id; message 'lclParent';
    function lclFrame: TRect; message 'lclFrame';
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:';
    function lclClientFrame: TRect; message 'lclClientFrame';
    // Accessibility
    class function LazRoleToCocoaRole(ALazRole: TLazAccessibilityRole): NSString; message 'LazRoleToCocoaRole:';
    function accessibilityAttributeValue(attribute: NSString): id; override;
    function accessibilityFocusedUIElement: id; override;
  end;

  TSelectionInfo = class
  public
    SelectedText: string;
    CaretPos: TPoint;
    SelLength: Integer;
    SelStart: Integer;
  end;

  TVisibleTextInfo = class
  public
    VisibleLineStart, VisibleLineCount: Integer;
    // now as a character index in relation to the entire text string
    VisibleTextStart, VisibleTextLength: Integer;
  end;

  { TCocoaAccessibleObject }

  TCocoaAccessibleObject = objcclass(NSObject)
  public
    // Accessibility
    LCLControl: TControl;
    LCLAcc: TLazAccessibleObject;
    //
    LCLInjectedControl: TCustomControl;
    LCLBaseControl: TCDBaseControl;
    procedure ReadInjectedAndBaseControl; message 'ReadInjectedAndBaseControl';
    function GetSelectedTextInfo: TSelectionInfo; message 'GetSelectedTextInfo';
    function GetVisibleTextInfo: TVisibleTextInfo; message 'GetVisibleTextInfo';
    //NSAccessibilityCategory = objccategory external (NSObject)
    function accessibilityAttributeNames: NSArray; override;
    function accessibilityAttributeValue(attribute: NSString): id; override;
    function accessibilityIsAttributeSettable(attribute: NSString): Boolean; override;
    procedure accessibilitySetValue_forAttribute(_value: id; attribute: NSString); override;
    function accessibilityParameterizedAttributeNames: NSArray; override;
    function accessibilityAttributeValue_forParameter(attribute: NSString; parameter: id): id; override;
    function accessibilityActionNames: NSArray; override;
    function accessibilityActionDescription(action: NSString): NSString; override;
    procedure accessibilityPerformAction(action: NSString); override;
    function accessibilityIsIgnored: Boolean; override;
    function accessibilityHitTest(point: NSPoint): id; override;
    function accessibilityFocusedUIElement: id; override;
    {function accessibilityIndexOfChild(child: id): NSUInteger; message 'accessibilityIndexOfChild:';
    function accessibilityArrayAttributeCount(attribute: NSString): NSUInteger; message 'accessibilityArrayAttributeCount:';
    function accessibilityArrayAttributeValues_index_maxCount(attribute: NSString; index: NSUInteger; maxCount: NSUInteger): NSArray; message 'accessibilityArrayAttributeValues:index:maxCount:';}
  end;

  { TCocoaMenu }

  TCocoaMenu = objcclass(NSMenu)
  public
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';
  end;

  { TCocoaMenuItem }

  TCocoaMenuItem = objcclass(NSMenuItem)
  public
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';
  end;

procedure SetViewDefaults(AView: NSView);

function Cocoa_RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;
function RawImage_DescriptionToBitmapType(ADesc: TRawImageDescription; out bmpType: TCocoaBitmapType): Boolean;

function CalcNSWindowStyle(const AWinControl: TWinControl; const AParams: TCreateParams): NSUInteger;
function LCLCoordToCocoa(AControl: TControl; ACoord: TPoint): NSPoint;
function CocoaCoordToLCL(AControl: TControl; ACoord: NSPoint): TPoint;

implementation

uses customdrawnwsforms, customdrawnprivate;

(*
About Mac Key codes:
unfortunately, mac key codes are keyboard specific:
that is, there is no universal VK_A, but every keyboard has its code for VK_A
Key codes depend on physical key position on the keyboard: considering a
QWERTY keyboard and an AZERTY one, keycode(Q) of first one = keycode(A) of
the second one, and so on.
For "printable" keys we can rely on kEventParamKeyMacCharCodes and
kEventParamKeyUnicodes event parameters to obtain an ascii/unicode value
that we can translate to the appropriate VK_ code
For non printable keys (Function, ins, arrow and so on...) we use the raw
keycodes, since it looks like they are constant across all keyboards

So, here are constants for non-printable keys (MK means "Mac Key").
These constants were extracted using KeyCodes program by Peter Maurer
(http://www.petermaurer.de/nasi.php?section=keycodes)

Some keys were taken from the ancient "Macintosh Toolbox Essentials", page 87
http://developer.apple.com/documentation/mac/pdf/MacintoshToolboxEssentials.pdf
*)

const
  MK_ENTER     = $24;
  MK_SPACE     = $31;
  MK_ESC       = $35;
  MK_F1        = $7A;
  MK_F2        = $78;
  MK_F3        = $63;
  MK_F4        = $76;
  MK_F5        = $60;
  MK_F6        = $61;
  MK_F7        = $62;
  MK_F8        = $64;
  MK_F9        = $65;
  MK_F10       = $6D;
  MK_F11       = $67;
  MK_F12       = $6F;
  MK_F13       = $69; MK_PRNSCR  = MK_F13;  //Print screen = F13
  MK_F14       = $6B; MK_SCRLOCK = MK_F14;  //Scroll Lock = F14
  MK_F15       = $71; MK_PAUSE   = MK_F15;  //Pause = F15
  MK_POWER     = $7F7F;
  MK_TAB       = $30;
  MK_INS       = $72; MK_HELP    = MK_INS;  //old macs call this key "help"
  MK_DEL       = $75;
  MK_HOME      = $73;
  MK_END       = $77;
  MK_PAGUP     = $74;
  MK_PAGDN     = $79;
  MK_UP        = $7E;
  MK_DOWN      = $7D;
  MK_LEFT      = $7B;
  MK_RIGHT     = $7C;
  MK_NUMLOCK   = $47;
  MK_NUMPAD0   = $52;
  MK_NUMPAD1   = $53;
  MK_NUMPAD2   = $54;
  MK_NUMPAD3   = $55;
  MK_NUMPAD4   = $56;
  MK_NUMPAD5   = $57;
  MK_NUMPAD6   = $58;
  MK_NUMPAD7   = $59;
  MK_NUMPAD8   = $5b;
  MK_NUMPAD9   = $5c;
  MK_PADEQUALS = $51; //only present in old mac keyboards?
  MK_PADDIV    = $4B;
  MK_PADMULT   = $43;
  MK_PADSUB    = $4E;
  MK_PADADD    = $45;
  MK_PADDEC    = $41;
  MK_PADENTER  = $4C; //enter on numeric keypad
  MK_BACKSPACE = $33;
  MK_CAPSLOCK  = $39;


//Modifiers codes - you'll never get these directly

  MK_SHIFTKEY  = $38;
  MK_CTRL      = $3B;
  MK_ALT       = $3A; MK_OPTION = MK_ALT;
  MK_COMMAND   = $37; MK_APPLE  = MK_COMMAND;

  MK_TILDE        = 50; // `/~ key
  MK_MINUS        = 27; // -/_ key
  MK_EQUAL        = 24; // =/+ key
  MK_BACKSLASH    = 42; // \ | key
  MK_LEFTBRACKET  = 33; // [ { key
  MK_RIGHTBRACKET = 30; // ] } key
  MK_SEMICOLON    = 41; // ; : key
  MK_QUOTE        = 39; // ' " key
  MK_COMMA        = 43; // , < key
  MK_PERIOD       = 47; // . > key
  MK_SLASH        = 44; // / ? key

function Cocoa_RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;
const
  ALIGNMAP: array[TRawImageLineEnd] of TCocoaBitmapAlignment = (cbaByte, cbaByte, cbaWord, cbaDWord, cbaQWord, cbaDQWord);
var
  ADesc: TRawImageDescription absolute ARawImage.Description;
  bmpType: TCocoaBitmapType;
begin
  Result := RawImage_DescriptionToBitmapType(ADesc, bmpType);
  if not Result then begin
    debugln(['TCarbonWidgetSet.RawImage_CreateBitmaps TODO Depth=',ADesc.Depth,' alphaprec=',ADesc.AlphaPrec,' byteorder=',ord(ADesc.ByteOrder),' alpha=',ADesc.AlphaShift,' red=',ADesc.RedShift,' green=',adesc.GreenShift,' blue=',adesc.BlueShift]);
    exit;
  end;
  ABitmap := HBITMAP(TCocoaBitmap.Create(ADesc.Width, ADesc.Height, ADesc.Depth, ADesc.BitsPerPixel, ALIGNMAP[ADesc.LineEnd], bmpType, ARawImage.Data));

  if ASkipMask or (ADesc.MaskBitsPerPixel = 0)
  then AMask := 0
  else AMask := HBITMAP(TCocoaBitmap.Create(ADesc.Width, ADesc.Height, 1, ADesc.MaskBitsPerPixel, ALIGNMAP[ADesc.MaskLineEnd], cbtMask, ARawImage.Mask));

  Result := True;
end;

function RawImage_DescriptionToBitmapType(
  ADesc: TRawImageDescription;
  out bmpType: TCocoaBitmapType): Boolean;
begin
  Result := False;

  if ADesc.Format = ricfGray
  then
  begin
    if ADesc.Depth = 1 then bmpType := cbtMono
    else bmpType := cbtGray;
  end
  else if ADesc.Depth = 1
  then bmpType := cbtMono
  else if ADesc.AlphaPrec <> 0
  then begin
    if ADesc.ByteOrder = riboMSBFirst
    then begin
      if  (ADesc.AlphaShift = 24)
      and (ADesc.RedShift   = 16)
      and (ADesc.GreenShift = 8 )
      and (ADesc.BlueShift  = 0 )
      then bmpType := cbtARGB
      else
      if  (ADesc.AlphaShift = 0)
      and (ADesc.RedShift   = 24)
      and (ADesc.GreenShift = 16 )
      and (ADesc.BlueShift  = 8 )
      then bmpType := cbtRGBA
      else
      if  (ADesc.AlphaShift = 0 )
      and (ADesc.RedShift   = 8 )
      and (ADesc.GreenShift = 16)
      and (ADesc.BlueShift  = 24)
      then bmpType := cbtBGRA
      else Exit;
    end
    else begin
      if  (ADesc.AlphaShift = 24)
      and (ADesc.RedShift   = 16)
      and (ADesc.GreenShift = 8 )
      and (ADesc.BlueShift  = 0 )
      then bmpType := cbtBGRA
      else
      if  (ADesc.AlphaShift = 0 )
      and (ADesc.RedShift   = 8 )
      and (ADesc.GreenShift = 16)
      and (ADesc.BlueShift  = 24)
      then bmpType := cbtARGB
      else
      if  (ADesc.AlphaShift = 24 )
      and (ADesc.RedShift   = 0 )
      and (ADesc.GreenShift = 8)
      and (ADesc.BlueShift  = 16)
      then bmpType := cbtRGBA
      else Exit;
    end;
  end
  else begin
    bmpType := cbtRGB;
  end;

  Result := True;
end;

function CalcNSWindowStyle(const AWinControl: TWinControl;
  const AParams: TCreateParams): NSUInteger;
var
  lForm: TCustomForm absolute AWinControl;
begin
  case lForm.BorderStyle of
    bsNone: Result := NSBorderlessWindowMask;
    bsSingle: Result := NSTitledWindowMask or NSClosableWindowMask;
    // bsToolWindow should be created by building a NSPanel instead of a NSWindow
    bsToolWindow: Result := NSTitledWindowMask or NSClosableWindowMask;
    bsSizeToolWin: Result := NSClosableWindowMask or NSResizableWindowMask;
  else
    Result := NSTitledWindowMask or NSClosableWindowMask or NSResizableWindowMask;
  end;

  if biMinimize in lForm.BorderIcons then Result := Result or NSMiniaturizableWindowMask;
end;

function LCLCoordToCocoa(AControl: TControl; ACoord: TPoint): NSPoint;
begin
  Result.x := ACoord.X;
  Result.y := Screen.Height - ACoord.Y;
  if AControl <> nil then Result.y := Result.y - AControl.Height;
end;

function CocoaCoordToLCL(AControl: TControl; ACoord: NSPoint): TPoint;
begin

end;

{ TCocoaForm }

function TCocoaForm.windowShouldClose(sender: id): LongBool;
var
  canClose : Boolean;
begin
  canClose:=true;
  CallbackCloseQuery(canClose);
  Result:=canClose;
end;

procedure TCocoaForm.windowWillClose(notification: NSNotification);
begin
  LCLSendCloseUpMsg(WindowHandle.LCLForm);
end;

procedure TCocoaForm.windowDidBecomeKey(notification: NSNotification);
begin
  CallbackActivate;
end;

procedure TCocoaForm.windowDidResignKey(notification: NSNotification);
begin
  CallbackDeactivate;
end;

procedure TCocoaForm.windowDidResize(notification: NSNotification);
begin
  CallbackResize;
end;

function TCocoaForm.acceptsFirstResponder: Boolean;
begin
  Result:=true;
end;

procedure TCocoaForm.mouseUp(event: NSEvent);
var
  mp : NSPoint;
begin
  mp:=event.locationInWindow;
  mp.y:=NSView(event.window.contentView).bounds.size.height-mp.y;
  callbackMouseUp(WindowHandle, round(mp.x), round(mp.y), mbLeft);
  inherited mouseUp(event);
end;

procedure TCocoaForm.mouseDown(event: NSEvent);
var
  mp : NSPoint;
begin
  mp:=event.locationInWindow;
  mp.y:=NSView(event.window.contentView).bounds.size.height-mp.y;
  callbackMouseDown(WindowHandle, round(mp.x), round(mp.y), mbLeft);
  inherited mouseDown(event);
end;

procedure TCocoaForm.mouseDragged(event: NSEvent);
var
  mp : NSPoint;
begin
  mp:=event.locationInWindow;
  mp.y:=NSView(event.window.contentView).bounds.size.height-mp.y;
  callbackMouseMove(WindowHandle, round(mp.x), round(mp.y));
  inherited mouseMoved(event);
end;

procedure TCocoaForm.mouseMoved(event: NSEvent);
var
  mp : NSPoint;
begin
  mp:=event.locationInWindow;
  mp.y:=NSView(event.window.contentView).bounds.size.height-mp.y;
  callbackMouseMove(WindowHandle, round(mp.x), round(mp.y));
  inherited mouseMoved(event);
end;

procedure TCocoaForm.keyDown(theEvent: NSEvent);
var
  lKey: Word;
  lUTF8Char: TUTF8Char;
  lSendKey, lSendChar: Boolean;
begin
  //inherited keyDown(theEvent); Don't call inherited or else Cocoa will think you didn't handle the event and beep on key input
  lKey := MacKeyCodeToLCLKey(theEvent, lSendKey, lSendChar, lUTF8Char);
  {$ifdef VerboseCDKeyInput}
  DebugLn(Format('[TCocoaForm] KeyDown CocoaKeyCode=%x UTF8Char=%s', [theEvent.keyCode(), lUTF8Char]));
  {$endif}
  if lSendKey then CallbackKeyDown(WindowHandle, lKey);
  if lSendChar then CallbackKeyChar(WindowHandle, 0, lUTF8Char);
end;

procedure TCocoaForm.keyUp(theEvent: NSEvent);
var
  lKey: Word;
  lUTF8Char: TUTF8Char;
  lSendKey, lSendChar: Boolean;
begin
  //inherited keyUp(theEvent); Don't call inherited or else Cocoa will think you didn't handle the event and beep on key input
  lKey := MacKeyCodeToLCLKey(theEvent, lSendKey, lSendChar, lUTF8Char);
  if lSendKey then CallbackKeyUp(WindowHandle, lKey);
end;

function TCocoaForm.MacKeyCodeToLCLKey(AKeyEvent: NSEvent; var SendKeyUpDown, SendChar: Boolean; var AUTF8Char: TUTF8Char): Word;
var
  KeyCode: Word;
  ExtendedKeysSupport: Boolean;
{  TextLen : UInt32;
  CharLen : integer;}
  characters: string;
  charactersIgnoringModifiers: string;
(*  widebuf: array[1..2] of widechar;
  U: Cardinal;
  Layout: UCKeyboardLayoutPtr;
  KeyboardLayout: KeyboardLayoutRef;*)
begin
  SendKeyUpDown := False;
  SendChar := False;
  Result := VK_UNKNOWN;
  AUTF8Char := '';
  ExtendedKeysSupport := Application.ExtendedKeysSupport;

  // Obtain data from the event
  KeyCode := AKeyEvent.keyCode();
  characters := NSStringToString(AKeyEvent.characters());
  charactersIgnoringModifiers := NSStringToString(AKeyEvent.charactersIgnoringModifiers());

//  IsSysKey:=(GetCurrentEventKeyModifiers and cmdKey)>0;

  //non-printable keys (see mackeycodes.inc)
  //for these keys, only send keydown/keyup (not char or UTF8KeyPress)
  case KeyCode of
    MK_F1       : Result:=VK_F1;
    MK_F2       : Result:=VK_F2;
    MK_F3       : Result:=VK_F3;
    MK_F4       : Result:=VK_F4;
    MK_F5       : Result:=VK_F5;
    MK_F6       : Result:=VK_F6;
    MK_F7       : Result:=VK_F7;
    MK_F8       : Result:=VK_F8;
    MK_F9       : Result:=VK_F9;
    MK_F10      : Result:=VK_F10;
    MK_F11      : Result:=VK_F11;
    MK_F12      : Result:=VK_F12;
{    MK_F13      : Result:=VK_SNAPSHOT;
    MK_F14      : Result:=VK_SCROLL;
    MK_F15      : Result:=VK_PAUSE;}
    MK_POWER    : Result:=VK_LCL_POWER;
    MK_TAB      : Result:=VK_TAB; //strangely enough, tab is "non printable"
    MK_INS      : Result:=VK_INSERT;
    MK_DEL      : Result:=VK_DELETE;
    MK_HOME     : Result:=VK_HOME;
    MK_END      : Result:=VK_END;
    MK_PAGUP    : Result:=VK_PRIOR;
    MK_PAGDN    : Result:=VK_NEXT;
    MK_UP       : Result:=VK_UP;
    MK_DOWN     : Result:=VK_DOWN;
    MK_LEFT     : Result:= VK_LEFT;
    MK_RIGHT    : Result:= VK_RIGHT;
    MK_NUMLOCK  : Result:= VK_NUMLOCK;
    MK_BACKSPACE: Result:= VK_BACK;
//    MK_CAPSLOCK : Result:= VK_CAPSLOCK; <- We don't seam to receive it at all
    //Modifiers codes - you'll never get these directly
    {MK_SHIFTKEY  = $38;
    MK_CTRL      = $3B;
    MK_ALT       = $3A; MK_OPTION = MK_ALT;
    MK_COMMAND   = $37; MK_APPLE  = MK_COMMAND;}
  end;

  if Result<>VK_UNKNOWN then
  begin
    //stop here, we won't send char or UTF8KeyPress
    SendKeyUpDown:=true;
    Exit;
  end;

  // Now other keys which might also send utf8keypress

  case KeyCode of
    MK_NUMPAD0: Result:=VK_NUMPAD0;
    MK_NUMPAD1: Result:=VK_NUMPAD1;
    MK_NUMPAD2: Result:=VK_NUMPAD2;
    MK_NUMPAD3: Result:=VK_NUMPAD3;
    MK_NUMPAD4: Result:=VK_NUMPAD4;
    MK_NUMPAD5: Result:=VK_NUMPAD5;
    MK_NUMPAD6: Result:=VK_NUMPAD6;
    MK_NUMPAD7: Result:=VK_NUMPAD7;
    MK_NUMPAD8: Result:=VK_NUMPAD8;
    MK_NUMPAD9: Result:=VK_NUMPAD9;
    MK_PADDIV  : Result:=VK_DIVIDE;
    MK_PADMULT : Result:=VK_MULTIPLY;
    MK_PADSUB  : Result:=VK_SUBTRACT;
    MK_PADADD  : Result:=VK_ADD;
    MK_PADDEC  : Result:=VK_DECIMAL;
    MK_PADENTER:
    begin
      Result:=VK_RETURN;
      AUTF8Char:=#13;
    end;
    MK_TILDE:        Result := VK_LCL_TILDE; // `/~ key
    MK_MINUS:        Result := VK_LCL_MINUS;
    MK_EQUAL:        Result := VK_LCL_EQUAL;
    MK_BACKSLASH:    Result := VK_LCL_BACKSLASH;
    MK_LEFTBRACKET:  Result := VK_LCL_OPEN_BRAKET;
    MK_RIGHTBRACKET: Result := VK_LCL_CLOSE_BRAKET;
    MK_SEMICOLON:    Result := VK_LCL_SEMI_COMMA;
    MK_QUOTE:        Result := VK_LCL_QUOTE;
    MK_COMMA:        Result := VK_LCL_COMMA;
    MK_PERIOD:       Result := VK_LCL_POINT;
    MK_SLASH:        Result := VK_LCL_SLASH;
  end;

  if Result<>VK_UNKNOWN then SendKeyUpDown:=true;

  if AUTF8Char <> '' then
  begin
    SendChar := True;
    Exit;
  end;

  if Length(charactersIgnoringModifiers) = 1 then
  begin
    case charactersIgnoringModifiers[1] of
      'a'..'z':
      begin
        Result:=VK_A+ord(charactersIgnoringModifiers[1])-ord('a');
        AUTF8Char := charactersIgnoringModifiers;
      end;
      'A'..'Z':
      begin
        Result:=ord(charactersIgnoringModifiers[1]);
        AUTF8Char := charactersIgnoringModifiers;
      end;
      #27     : Result:=VK_ESCAPE;
      #8      : Result:=VK_BACK;
      ' '     :
      begin
        Result:=VK_SPACE;
        AUTF8Char := charactersIgnoringModifiers;
      end;
      #13     : Result:=VK_RETURN;
      '0'..'9':
      begin
        Result:=VK_0+ord(charactersIgnoringModifiers[1])-ord('0');
        AUTF8Char := charactersIgnoringModifiers;
      end;
    end;
  end;

  if Result<>VK_UNKNOWN then SendKeyUpDown:=true;
  if AUTF8Char <> '' then SendChar := True;

  if Length(characters) > 1 then
  begin
    // ToDo: Merge the accents into the UTF-8 character if necessary
    AUTF8Char := characters;
    //
    SendChar := True;
  end;

  if Result = VK_UNKNOWN then
    DebugLn('[MacKeyCodeToLCLKey] Unknown Key! KeyCode=%d characters=%s charactersIgnoringModifiers=%s',
      [KeyCode, characters, charactersIgnoringModifiers]);
end;

procedure TCocoaForm.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaForm.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

function TCocoaForm.lclIsVisible:Boolean;
begin
  Result:=isVisible;
end;

procedure TCocoaForm.lclInvalidateRect(const r:TRect);
begin
  contentView.lclInvalidateRect(r);
end;

procedure TCocoaForm.lclInvalidate;
begin
  contentView.lclInvalidate;
end;

procedure TCocoaForm.lclLocalToScreen(var X,Y:Integer);
var
  f   : NSRect;
begin
  if Assigned(screen) then begin
    f:=frame;
    x:=Round(f.origin.x+x);
    y:=Round(screen.frame.size.height-f.size.height-f.origin.y);
  end;
end;

function TCocoaForm.lclFrame:TRect;
begin
  if Assigned(screen)
    then NSToLCLRect(frame, screen.frame.size.height, Result)
    else NSToLCLRect(frame, Result);
end;

procedure TCocoaForm.lclSetFrame(const r:TRect);
var
  ns: NSRect;
begin
  if Assigned(screen)
    then LCLToNSRect(r, screen.frame.size.height, ns)
    else LCLToNSRect(r, ns);
  setFrame_display(ns, isVisible);
end;

function TCocoaForm.lclClientFrame:TRect;
var
  wr  : NSRect;
  b   : CGGeometry.CGRect;
begin
  wr:=frame;
  b:=contentView.frame;
  Result.Left:=Round(b.origin.x);
  Result.Top:=Round(wr.size.height-b.origin.y);
  Result.Right:=Round(b.origin.x+b.size.width);
  Result.Bottom:=Round(Result.Top+b.size.height);
end;

procedure TCocoaForm.lclSetClientFrame(const r: TRect);
var
  ns, lAdjustedRect: NSRect;
  lTitleBarHeight: Single;
begin
  if Assigned(screen)
    then LCLToNSRect(r, screen.frame.size.height, ns)
    else LCLToNSRect(r, ns);
  // LCL coordinates are for the client area of the Window, while setFrame_display
  // includes the title bar too, so we need to consider this here, but in a clever
  // way, as to keep the window position the same
  lAdjustedRect := frameRectForContentRect(ns);
  lTitleBarHeight := lAdjustedRect.size.height - ns.size.height;
  ns.origin.y := ns.origin.y - lTitleBarHeight;
  ns.size.height := lAdjustedRect.size.height;

  setFrame_display(ns, isVisible);
end;

procedure TCocoaForm.CallbackActivate;
begin
  LCLSendActivateMsg(WindowHandle.LCLForm, WA_ACTIVE, false);
end;

procedure TCocoaForm.CallbackDeactivate;
begin
  LCLSendActivateMsg(WindowHandle.LCLForm, WA_INACTIVE, false);
end;

procedure TCocoaForm.CallbackCloseQuery(var CanClose: Boolean);
begin
  // Message results : 0 - do nothing, 1 - destroy window
  CanClose:=LCLSendCloseQueryMsg(WindowHandle.LCLForm)>0;
end;

procedure TCocoaForm.CallbackResize;
var
  sz  : NSSize;
  r   : TRect;
begin
  sz := frame.size;
  TCDWSCustomForm.GetClientBounds(TWinControl(WindowHandle.LCLForm), r);
  if Assigned(WindowHandle.LCLForm) then
    LCLSendSizeMsg(WindowHandle.LCLForm, Round(sz.width), Round(sz.height), SIZENORMAL);
end;

function TCocoaForm.accessibilityAttributeValue(attribute: NSString): id;
var
  lStrAttr: String;
  lAResult: NSArray;
  lMAResult: NSMutableArray;
begin
  Result := inherited accessibilityAttributeValue(attribute);

  {$ifdef VerboseCDAccessibility}
  //lStrAttr := NSStringToString(attribute);
  //DebugLn('[TCocoaCustomControl.accessibilityAttributeValue] attribute='+lStrAttr);
  {$endif}

// Cocoa by default merges the window and it's main subcontrol into 1 things
// And Finder and other Cocoa apps follow this, so let's do it like that too
//
// If we want to put all items inside a sub-window in the window then we could
// activate the code bellow, but then we have to solve parent missmatch errors which we get
//
// Curiously, even while not being in the list of elements,
// TCocoaCustomControl.accessibilityAttributeValue is executed
// as if it spoke for the window and not for itself, this adding the sub-window
// duplicates all out accessibility entries (appear once for the window and once for the sub-window)
(*  if attribute.isEqualToString(NSAccessibilityChildrenAttribute) then
  begin
    // Strangely Cocoa doesn't add automatically the client area to the array
    {$ifdef VerboseCDAccessibility}
    //DebugLn('[TCocoaForm.accessibilityAttributeValue] NSAccessibilityChildrenAttribute');
    {$endif}
    lAResult := NSArray(Result);
    lMAResult := lAResult.mutableCopy();
    lMAResult.addObject(WindowHandle.ClientArea);
    Result := lMAResult;
  end;  *)
end;

{ TCocoaCustomControl }

procedure TCocoaCustomControl.drawRect(dirtyRect:NSRect);
begin
  inherited drawRect(dirtyRect);
  Draw(NSGraphicsContext.currentContext, bounds, dirtyRect);
end;

procedure TCocoaCustomControl.Draw(ControlContext: NSGraphicsContext;
  const Abounds, dirty:NSRect);
var
  lWidth, lHeight: Integer;
  lBitmap, lMask: HBITMAP;
  lRawImage: TRawImage;
  AImage: TLazIntfImage;
  ACanvas: TLazCanvas;
  {$IFDEF VerboseCDPaintProfiler}
  lTimeStart, lNativeStart: TDateTime;
  {$ENDIF}
begin
  {$IFDEF VerboseCDPaintProfiler}
  lTimeStart := NowUTC();
  {$ENDIF}
  if not Assigned(Context) then Context:=TCocoaContext.Create;

  Context.ctx:=ControlContext;
  lWidth := Round(bounds.size.width);
  lHeight := Round(bounds.size.height);
  if Context.InitDraw(lWidth, lHeight) then
  begin
    // Prepare the non-native image and canvas
    UpdateControlLazImageAndCanvas(WindowHandle.Image,
      WindowHandle.Canvas, lWidth, lHeight, clfRGB24UpsideDown);
    DrawFormBackground(WindowHandle.Image, WindowHandle.Canvas, WindowHandle.LCLForm);
    WindowHandle.Canvas.NativeDC := PtrInt(Context);

    // Draw the form
    RenderForm(WindowHandle.Image, WindowHandle.Canvas, WindowHandle.LCLForm);

    {$IFDEF VerboseCDPaintProfiler}
    lNativeStart := NowUTC();
    {$ENDIF}

    // Now render it into the control
    WindowHandle.Image.GetRawImage(lRawImage);
    Cocoa_RawImage_CreateBitmaps(lRawImage, lBitmap, lMask, True);
    Context.DrawBitmap(0, 0, TCocoaBitmap(lBitmap));
  end;
  {$IFDEF VerboseCDPaintProfiler}
  DebugLn(Format('[TCocoaCustomControl.Draw] Paint LCL-CustomDrawn: %d ms Native: %d ms',
    [DateTimeToMilliseconds(lNativeStart - lTimeStart),
     DateTimeToMilliseconds(NowUTC() - lNativeStart)]));
  {$ENDIF}
end;

function TCocoaCustomControl.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

procedure TCocoaCustomControl.keyDown(theEvent: NSEvent);
begin
  //DebugLn('[TCocoaCustomControl] KeyDown='+IntToHex(theEvent.keyCode(), 4));
  WindowHandle.CocoaForm.keyDown(theEvent);
end;

procedure TCocoaCustomControl.keyUp(theEvent: NSEvent);
begin
  WindowHandle.CocoaForm.keyUp(theEvent);
end;

function RectToViewCoord(view: NSView; const r: TRect): NSRect;
var
  b: NSRect;
begin
  if not Assigned(view) then Exit;
  b:=view.bounds;
  Result.origin.x:=r.Left;
  Result.origin.y:=b.size.height-r.Top;
  Result.size.width:=r.Right-r.Left;
  Result.size.height:=r.Bottom-r.Top;
end;

function TCocoaCustomControl.lclInitWithCreateParams(const AParams:TCreateParams): id;
var
  p: NSView;
  ns: NSRect;
begin
  p:=nil;
  if (AParams.WndParent<>0) then begin
    if (NSObject(AParams.WndParent).isKindOfClass_(NSView)) then
      p:=NSView(AParams.WndParent)
    else if (NSObject(AParams.WndParent).isKindOfClass_(NSWindow)) then
      p:=NSWindow(AParams.WndParent).contentView;
  end;
  with AParams do
    if Assigned(p)
      then LCLToNSRect(Types.Bounds(X,Y,Width, Height), p.frame.size.height, ns)
      else LCLToNSRect(Types.Bounds(X,Y,Width, Height), ns);

  Result:=initWithFrame(ns);
  if not Assigned(Result) then Exit;

  if Assigned(p) then p.addSubview(Self);
  SetViewDefaults(Self);
end;

function TCocoaCustomControl.lclIsVisible:Boolean;
begin
  Result:=not isHidden;
end;

procedure TCocoaCustomControl.lclInvalidateRect(const r:TRect);
begin
  setNeedsDisplayInRect(RectToViewCoord(Self, r));
end;

procedure TCocoaCustomControl.lclInvalidate;
begin
  setNeedsDisplay_(True);
end;

procedure TCocoaCustomControl.lclLocalToScreen(var X,Y:Integer);
begin

end;

function TCocoaCustomControl.lclParent:id;
begin
  Result:=superView;
end;

function TCocoaCustomControl.lclFrame: TRect;
var
  v : NSView;
begin
  v:=superview;
  if Assigned(v)
    then NSToLCLRect(frame, v.frame.size.height, Result)
    else NSToLCLRect(frame, Result);
end;

procedure TCocoaCustomControl.lclSetFrame(const r:TRect);
var
  ns : NSRect;
begin
  if Assigned(superview)
    then LCLToNSRect(r, superview.frame.size.height, ns)
    else LCLToNSRect(r, ns);
  setFrame(ns);
end;

function TCocoaCustomControl.lclClientFrame:TRect;
var
  r: NSRect;
begin
  r:=bounds;
  Result.Left:=0;
  Result.Top:=0;
  Result.Right:=Round(r.size.width);
  Result.Bottom:=Round(r.size.height);
end;

class function TCocoaCustomControl.LazRoleToCocoaRole(
  ALazRole: TLazAccessibilityRole): NSString;
begin
  case ALazRole of
    larAnimation: Result := NSAccessibilityImageRole;
    larButton: Result := NSAccessibilityButtonRole;
    larCell: Result := NSAccessibilityCellRole;
    larChart: Result := NSAccessibilityImageRole;
    larCheckBox: Result := NSAccessibilityCheckBoxRole;
    larClock: Result := NSAccessibilityStaticTextRole;
    larColorPicker: Result := NSAccessibilityColorWellRole;
    larComboBox: Result := NSAccessibilityComboBoxRole;
    larDateField: Result := NSAccessibilityStaticTextRole;//('AXDateField');
    larGrid: Result := NSAccessibilityGridRole;
    larGroup: Result := NSAccessibilityGroupRole;
    larImage: Result := NSAccessibilityImageRole;
    larLabel: Result := NSAccessibilityStaticTextRole;
    larListBox: Result := NSAccessibilityListRole;
    larListItem: Result := NSAccessibilityStaticTextRole;
    larMenuBar: Result := NSAccessibilityMenuBarRole;
    larMenuItem: Result := NSAccessibilityMenuItemRole;
    larProgressIndicator: Result := NSAccessibilityProgressIndicatorRole;
    larRadioButton: Result := NSAccessibilityRadioButtonRole;
    larResizeGrip: Result := NSAccessibilityStaticTextRole; // VoiceOver cannot handle AXHandle in Mac 10.6, so we fallback to AXStaticText
    larScrollBar: Result := NSAccessibilityScrollBarRole;
    larSpinner: Result := NSAccessibilityIncrementorRole;
    larTabControl: Result := NSAccessibilityTabGroupRole;
    larTextEditorMultiline: Result := NSAccessibilityTextAreaRole;
    larTextEditorSingleline: Result := NSAccessibilityTextFieldRole;
    //  NSAccessibilityToolbarRole: NSString; cvar; external;
    larTrackBar: Result := NSAccessibilitySliderRole;
    larTreeView: Result := NSAccessibilityListRole;
    larTreeItem: Result := NSAccessibilityStaticTextRole;
    larWindow: Result := NSAccessibilityWindowRole;//NSAccessibilityUnknownRole;
  else
    Result := NSAccessibilityUnknownRole;
  end;
end;

function TCocoaCustomControl.accessibilityAttributeValue(attribute: NSString): id;
var
  lStrAttr: String;
  lAResult: NSArray;
  lMAResult: NSMutableArray;
  i: Integer;
  lForm: TCustomForm;
  lFormAcc, lChildAcc: TLazAccessibleObject;
  lAccObject: TCocoaAccessibleObject;
begin
  Result := inherited accessibilityAttributeValue(attribute);

  lStrAttr := NSStringToString(attribute);
  //DebugLn('[TCocoaCustomControl.accessibilityAttributeValue] attribute='+lStrAttr);

  if attribute.isEqualToString(NSAccessibilityRoleAttribute) then
  begin
    {$ifdef VerboseCDAccessibility}
    DebugLn('[TCocoaCustomControl.accessibilityAttributeValue] NSAccessibilityRoleAttribute');
    {$endif}
    lForm := WindowHandle.LCLForm;
    lFormAcc := lForm.GetAccessibleObject();
    Result := LazRoleToCocoaRole(lFormAcc.AccessibleRole);
  end
  else if attribute.isEqualToString(NSAccessibilityChildrenAttribute) then
  begin
    {$ifdef VerboseCDAccessibility}
    lStrAttr := '[TCocoaCustomControl.accessibilityAttributeValue] NSAccessibilityChildrenAttribute';
    {$endif}
    lAResult := NSArray(Result);
    lMAResult := NSMutableArray.arrayWithCapacity(0); //lAResult.mutableCopy();
    lForm := WindowHandle.LCLForm;
    lFormAcc := lForm.GetAccessibleObject();
    lChildAcc := lFormAcc.GetFirstChildAccessibleObject();
    while lChildAcc <> nil do
    begin
      lAccObject := TCocoaAccessibleObject(lChildAcc.Handle);
      if lAccObject <> nil then
      begin
        lMAResult.addObject(lAccObject);
        {$ifdef VerboseCDAccessibility}
        lStrAttr := lStrAttr + Format(' Adding object %s : %x', [lChildAcc.OwnerControl.ClassName, PtrUInt(lAccObject)]);
        {$endif}
      end;
      lChildAcc := lFormAcc.GetNextChildAccessibleObject();
    end;
    {$ifdef VerboseCDAccessibility}
    DebugLn(lStrAttr);
    {$endif}
    Result := lMAResult;
  end;
end;

function TCocoaCustomControl.accessibilityFocusedUIElement: id;
var
  lForm: TCustomForm;
//  lFormAcc: TLazAccessibleObject;
  lActiveControl: TWinControl;
begin
  Result := inherited accessibilityFocusedUIElement();

  lForm := WindowHandle.LCLForm;
//  lFormAcc := lForm.GetAccessibleObject();
  lActiveControl := lForm.ActiveControl;
  if lActiveControl = nil then lActiveControl := lForm;

  Result := id(lActiveControl.GetAccessibleObject().Handle);
end;

{ TCocoaAccessibleObject }

procedure TCocoaAccessibleObject.ReadInjectedAndBaseControl;
var
  lHandle: TCDWinControl;
begin
  if not (LCLControl is TWinControl) then Exit;

  if LCLBaseControl = nil then
  begin
    lHandle := TCDWinControl(TWinControl(LCLControl).Handle);
    LCLBaseControl := lHandle;
    if lHandle <> nil then
      LCLInjectedControl := lHandle.CDControl
    else
      LCLInjectedControl := nil;
  end;
end;

function TCocoaAccessibleObject.GetSelectedTextInfo: TSelectionInfo;
var
  lCustomEdit: TCustomEdit;
begin
  // initialization
  Result := TSelectionInfo.Create;

  if LCLControl is TCustomEdit then
  begin
    lCustomEdit := TCustomEdit(LCLControl);
    Result.SelectedText := lCustomEdit.SelText;
    Result.CaretPos := lCustomEdit.CaretPos;
    Result.SelLength := lCustomEdit.SelLength;
    Result.SelStart := lCustomEdit.SelStart;
  end;
end;

function TCocoaAccessibleObject.GetVisibleTextInfo: TVisibleTextInfo;
var
  lCustomMemo: TCustomMemo;
  lCustomEdit: TCustomEdit;
  lDC: HDC;
  lOldFont: HFont;
  lTM: TTextMetric;
  lRect: TRect;
  lLines: TStrings = nil;
  lScrollBar: TControlScrollbar = nil;
  i: Integer;
  lScrollPos, lScrollRange, lScrollPage: Integer;
  lStr: String;
begin
  // initialization
  Result := TVisibleTextInfo.Create;
  Result.VisibleLineCount := 1;

  if LCLControl is TCustomMemo then
  begin
    lCustomMemo := TCustomMemo(LCLControl);
    lDC := LCLIntf.GetDC(lCustomMemo.Handle);
    lOldFont := SelectObject(lDC, lCustomMemo.Font.Handle);
    try
      GetTextMetrics(lDC, lTM);
      lRect := LCLControl.ClientRect;
      Result.VisibleLineCount := (lRect.Bottom - lRect.Top) div
        (lTM.tmHeight + lTM.tmExternalLeading);
    finally
      SelectObject(lDC, lOldFont);
      ReleaseDC(lCustomMemo.Handle, lDC);
    end;

    lLines := lCustomMemo.Lines;
    lScrollBar := lCustomMemo.VertScrollBar;
  end;

  // With a TStrings and a ScrollBar we can get the visible range info
  if (lLines <> nil) and (lScrollBar <> nil) then
  begin
    lScrollPos := lScrollBar.Position;
    lScrollRange := lScrollBar.Range;
    lScrollPage := lScrollBar.Page;
    Result.VisibleLineStart := Round((lScrollPos / lScrollRange) * lLines.Count);
    Result.VisibleLineCount := Round((lScrollPage / lScrollRange) * lLines.Count);
    Result.VisibleLineCount := Min(Result.VisibleLineCount, lLines.Count - 1 - Result.VisibleLineStart); // sanity correction

    // using the calculated info get the character indexes
    for i := 0 to Result.VisibleLineStart + Result.VisibleLineCount do
    begin
      if i < Result.VisibleLineStart then
      begin
        lStr := lLines.Strings[i];
        Result.VisibleTextStart := Result.VisibleTextStart + UTF8Length(lStr) + 1; // +1 for the Mac line ending
      end
      else
      begin
        lStr := lLines.Strings[i];
        Result.VisibleTextLength := Result.VisibleTextLength + UTF8Length(lStr) + 1; // +1 for the Mac line ending
      end;
    end;
  end;
end;

{
  See the documentation about accessibility Roles and their required attributes:
  http://developer.apple.com/library/mac/documentation/UserExperience/Reference/Accessibility_RoleAttribute_Ref/Role.html
}
function TCocoaAccessibleObject.accessibilityAttributeNames: NSArray;
var
  lResult: NSMutableArray;
  lCocoaRole: NSString;

    // Basic elements which most roles have
  procedure AddBasicAttributes;
  begin
    lResult.addObject(NSAccessibilityDescriptionAttribute);
    lResult.addObject(NSAccessibilityEnabledAttribute);
    lResult.addObject(NSAccessibilityFocusedAttribute);
    lResult.addObject(NSAccessibilityParentAttribute);
    lResult.addObject(NSAccessibilityPositionAttribute);
    lResult.addObject(NSAccessibilityRoleAttribute);
    lResult.addObject(NSAccessibilitySizeAttribute);
    lResult.addObject(NSAccessibilityTitleAttribute);
    lResult.addObject(NSAccessibilityTopLevelUIElementAttribute);
    lResult.addObject(NSAccessibilityWindowAttribute);
  end;

begin
  {$ifdef VerboseCDAccessibility}
  DebugLn(Format('[TCocoaAccessibleObject.accessibilityAttributeNames] Self=%x', [PtrUInt(Self)]));
  {$endif}
  lResult := NSMutableArray.array_();

  lCocoaRole := TCocoaCustomControl.LazRoleToCocoaRole(LCLAcc.AccessibleRole);
  //larAnimation
  //larButton
  if lCocoaRole.isEqualToString(NSAccessibilityButtonRole) then
  begin
    AddBasicAttributes();
    lResult.addObject(NSAccessibilityRoleDescriptionAttribute);
  end
  //larCell
  //larChart
  //larCheckBox
  //larClock
  //larColorPicker
  //larComboBox
  //larDateField
  //larGrid
  //larGroup
  //larImage
  //larLabel
  else if lCocoaRole.isEqualToString(NSAccessibilityStaticTextRole) then
  begin
    AddBasicAttributes();
    lResult.addObject(NSAccessibilityValueAttribute);
  end
  //larListBox
  //larListItem
  //larMenuBar
  //larMenuItem
  //larProgressIndicator
  //larRadioButton
  //larResizeGrip
  //larScrollBar
  //larSpinner
  //larTabControl
  //larTextEditorMultiline
  //larTextEditorSingleline
  else if lCocoaRole.isEqualToString(NSAccessibilityTextFieldRole) then
  begin
    AddBasicAttributes();
    lResult.addObject(NSAccessibilityNumberOfCharactersAttribute); // Number of characters in an editable text field. Must not be settable.
    lResult.addObject(NSAccessibilitySelectedTextAttribute); // Currently selected text of a UI element. May be settable.
    lResult.addObject(NSAccessibilitySelectedTextRangeAttribute); // Position and length (in characters) of a selected portion of text in the UI element. May be settable.
    lResult.addObject(NSAccessibilityValueAttribute); // The element’s value. May be settable.
    lResult.addObject(NSAccessibilityVisibleCharacterRangeAttribute); // Range of characters that are scrolled into view in an editable text element. May be settable.
  end
  //larTrackBar
  //larTreeView
  //larTreeItem
  //larWindow
  else
  begin
    AddBasicAttributes();
  end;

  // This one we use to put LCL object and class names to help debugging =)
  lResult.addObject(NSAccessibilityUnitDescriptionAttribute);

  Result := lResult;
end;

// Note that if there is an injected control we will send information about it instead
// of about the host control
function TCocoaAccessibleObject.accessibilityAttributeValue(attribute: NSString): id;
var
  lStrAttr: String;
  lTmpStr: string;
  //
  lSize: TSize;
  lPoint: TPoint;
  lBool: Boolean;
  lInt: Integer;
  //
  lAResult: NSArray;
  lMAResult: NSMutableArray;
  lNSSize: NSSize;
  lNSPoint: NSPoint;
  lNSRange: NSRange;
  //
  i: Integer;
  lChildAcc: TLazAccessibleObject;
  lForm: TCustomForm;
  lParent: TWinControl;
  lSelInfo: TSelectionInfo;
  lVisibleTextInfo: TVisibleTextInfo;

  function GetControlText: string;
  begin
    Result := LCLControl.Caption;
  end;

begin
  //Result := inherited accessibilityAttributeValue(attribute); -> This raises errors, so don't
  Result := nil;
  ReadInjectedAndBaseControl();

  lStrAttr := NSStringToString(attribute);
  {$ifdef VerboseCDAccessibility}
  DebugLn(Format(':>[TCocoaAccessibleObject.accessibilityAttributeValue] Self=%x LCLControl=%x',
    [PtrUInt(Self), PtrUInt(LCLControl)]));
  {$endif}

  //
  // Role
  //
  if attribute.isEqualToString(NSAccessibilityRoleAttribute) then
  begin
    {$ifdef VerboseCDAccessibility}
    DebugLn(':<[TCocoaAccessibleObject.accessibilityAttributeValue] NSAccessibilityRoleAttribute');
    {$endif}
    Result := TCocoaCustomControl.LazRoleToCocoaRole(LCLAcc.AccessibleRole);
  end
  //
  // RoleDescription
  //
  else if attribute.isEqualToString(NSAccessibilityRoleDescriptionAttribute) then
  begin
    Result := NSStringUtf8(LCLControl.Caption);
  end
  //
  // Value
  //
  // The element’s value. May be settable.
  //
  else if attribute.isEqualToString(NSAccessibilityValueAttribute) then
  begin
    Result := NSStringUtf8(LCLControl.Caption);
  end
  {else if attribute = NSAccessibilityMinValueAttribute: NSString; cvar; external;
  NSAccessibilityMaxValueAttribute: NSString; cvar; external;}
  //
  // Enabled
  //
  else if attribute.isEqualToString(NSAccessibilityEnabledAttribute) then
  begin
    Result := NSNumber.numberWithBool(LCLControl.Enabled);
  end
  //
  // Focused
  //
  else if attribute.isEqualToString(NSAccessibilityFocusedAttribute) then
  begin
    if LCLControl is TWinControl then
    begin
      lBool := TWinControl(LCLControl).Focused;
      if (not lBool) and (LCLInjectedControl <> nil) then
        lBool := LCLInjectedControl.Focused;
      Result := NSNumber.numberWithBool(lBool);
    end
    else Result := NSNumber.numberWithBool(False);
  end
  //
  // Parent
  //
  else if attribute.isEqualToString(NSAccessibilityParentAttribute) or
    attribute.isEqualToString(NSAccessibilityTopLevelUIElementAttribute) then
  begin
    lParent := LCLControl.Parent;
    if lParent <> nil then
    begin
      {$ifdef VerboseCDAccessibility}
      DebugLn(Format(':<[TCocoaAccessibleObject.accessibilityAttributeValue] NSAccessibilityParentAttribute Self=%s Parent=%s',
        [LCLControl.ClassName, lParent.ClassName]));
      {$endif}
      // if the parent is a form, pass to the custom control
      if lParent is TCustomForm then
      begin
        Result := TCocoaWindow(lParent.Handle).CocoaForm;//ClientArea;
        {$ifdef VerboseCDAccessibility}
        DebugLn(':<[TCocoaAccessibleObject.accessibilityAttributeValue] Parent is TCustomForm');
        {$endif}
      end
      else
        Result := TCocoaAccessibleObject(lParent.GetAccessibleObject().Handle);
    end
    else
    begin
      {$ifdef VerboseCDAccessibility}
      DebugLn(':<[TCocoaAccessibleObject.accessibilityAttributeValue] ERROR: Parent is nil!');
      {$endif}
      Result := nil;
    end;
  end
  //
  // Children
  //
  else if attribute.isEqualToString(NSAccessibilityChildrenAttribute)
    or attribute.isEqualToString(NSAccessibilityVisibleChildrenAttribute) then
  begin
    {$ifdef VerboseCDAccessibility}
    DebugLn(':[TCocoaAccessibleObject.accessibilityAttributeValue] NSAccessibilityChildrenAttribute');
    {$endif}
    lAResult := NSArray(Result);
    lMAResult := lAResult.mutableCopy();
    //if Self.LCLInjectedControl = nil then
    for i := 0 to LCLAcc.GetChildAccessibleObjectsCount() - 1 do
    begin
      lChildAcc := LCLAcc.GetChildAccessibleObject(i);
      lMAResult.addObject(TCocoaAccessibleObject(lChildAcc.Handle));
    end;
    Result := lMAResult;
  end
  //
  // Window
  //
  else if attribute.isEqualToString(NSAccessibilityWindowAttribute) then
  begin
    lForm := Forms.GetParentForm(LCLControl);
    //Result := TCocoaAccessibleObject(lForm.GetAccessibleObject().Handle);
    Result := TCocoaWindow(lForm.Handle).CocoaForm;//ClientArea;
  end
  else if attribute.isEqualToString(NSAccessibilitySelectedChildrenAttribute) then
  begin

  end
  //
  // Position is in screen coordinates!
  //
  else if attribute.isEqualToString(NSAccessibilityPositionAttribute) then
  begin
    lPoint := LCLControl.ClientToScreen(Types.Point(0, 0));
    lNSPoint := LCLCoordToCocoa(LCLControl, lPoint);
    Result := NSValue.valueWithPoint(lNSPoint);
    {$ifdef VerboseCDAccessibility}
    DebugLn(Format(':<[TCocoaAccessibleObject.accessibilityAttributeValue] NSAccessibilityPositionAttribute Result=%d,%d', [lPoint.X, lPoint.Y]));
    {$endif}
  end
  //
  // Size
  //
  else if attribute.isEqualToString(NSAccessibilitySizeAttribute) then
  begin
    lSize := LCLAcc.Size;
    lNSSize.width := lSize.CX;
    lNSSize.height := lSize.CY;
    Result := NSValue.valueWithSize(lNSSize);
    {$ifdef VerboseCDAccessibility}
    DebugLn(Format(':<[TCocoaAccessibleObject.accessibilityAttributeValue] NSAccessibilitySizeAttribute Result=%d,%d', [lSize.CX, lSize.CY]));
    {$endif}
  end
  //
  // NumberOfCharacters
  //
  // Number of characters in an editable text field. Must not be settable.
  //
  else if attribute.isEqualToString(NSAccessibilityNumberOfCharactersAttribute) then
  begin
    lTmpStr := LCLControl.Caption;
    lInt := UTF8Length(lTmpStr);
    Result := NSNumber.numberWithInt(lInt);
    {$ifdef VerboseCDAccessibility}
    DebugLn(Format(':<[TCocoaAccessibleObject.accessibilityAttributeValue] NSAccessibilityNumberOfCharactersAttribute Text=%s Result=%d', [lTmpStr, lInt]));
    {$endif}
  end
  //
  // SelectedText
  //
  // Currently selected text of a UI element. May be settable.
  //
  else if attribute.isEqualToString(NSAccessibilitySelectedTextAttribute) then
  begin
    lSelInfo := GetSelectedTextInfo();
    try
      Result := NSStringUtf8(lSelInfo.SelectedText);
    finally
      lSelInfo.Free;
    end;
    //{$ifdef VerboseCDAccessibility}
    //DebugLn(Format(':<[TCocoaAccessibleObject.accessibilityAttributeValue] NSAccessibilitySizeAttribute Result=%d,%d', [lSize.CX, lSize.CY]));
    //{$endif}
  end
  //
  // SelectedTextRange
  //
  // Position and length (in characters) of a selected portion of text in the UI element. May be settable.
  //
  else if attribute.isEqualToString(NSAccessibilitySelectedTextRangeAttribute) then
  begin
    lSelInfo := GetSelectedTextInfo();
    try
      lNSRange.location := lSelInfo.SelStart;
      lNSRange.length := lSelInfo.SelLength;
      Result := NSValue.valueWithRange(lNSRange);
    finally
      lSelInfo.Free;
    end;
    //{$ifdef VerboseCDAccessibility}
    //DebugLn(Format(':<[TCocoaAccessibleObject.accessibilityAttributeValue] NSAccessibilitySizeAttribute Result=%d,%d', [lSize.CX, lSize.CY]));
    //{$endif}
  end
  //
  // VisibleCharacterRange
  //
  // Range of characters that are scrolled into view in an editable text element. May be settable.
  //
  else if attribute.isEqualToString(NSAccessibilityVisibleCharacterRangeAttribute) then
  begin
    lVisibleTextInfo := GetVisibleTextInfo();
    try
      lNSRange.location := lVisibleTextInfo.VisibleTextStart;
      lNSRange.length := lVisibleTextInfo.VisibleTextLength;
      Result := NSValue.valueWithRange(lNSRange);
    finally
      lVisibleTextInfo.Free;
    end;
    //{$ifdef VerboseCDAccessibility}
    //DebugLn(Format(':<[TCocoaAccessibleObject.accessibilityAttributeValue] NSAccessibilityVisibleCharacterRangeAttribute Result=%d,%d', [lSize.CX, lSize.CY]));
    //{$endif}
  end
  //
  // This one we use to put LCL object and class names to help debugging =)
  //
  else if attribute.isEqualToString(NSAccessibilityUnitDescriptionAttribute) then
  begin
    Result := NSStringUtf8(LCLControl.Name+':'+LCLControl.ClassName);
  end;
end;

function TCocoaAccessibleObject.accessibilityIsAttributeSettable(
  attribute: NSString): Boolean;
begin
  {$ifdef VerboseCDAccessibility}
  DebugLn('[TCocoaAccessibleObject.accessibilityIsAttributeSettable]');
  {$endif}
  Result := False;

  //
  // Value - The element’s value. May be settable.
  //
  if attribute.isEqualToString(NSAccessibilityValueAttribute) then
  begin
    if LCLAcc.AccessibleRole in [larComboBox, larTextEditorMultiline, larTextEditorSingleline] then
    begin
      Result := True;
    end;
  end
  //
  // Always Settable attributes
  //
  else if attribute.isEqualToString(NSAccessibilityFocusedAttribute) or
    attribute.isEqualToString(NSAccessibilitySelectedTextAttribute) or
    attribute.isEqualToString(NSAccessibilitySelectedTextRangeAttribute) or
    attribute.isEqualToString(NSAccessibilityVisibleCharacterRangeAttribute) then
  begin
    Result := True;
  end;
end;

procedure TCocoaAccessibleObject.accessibilitySetValue_forAttribute(_value: id;
  attribute: NSString);
begin
  {$ifdef VerboseCDAccessibility}
  DebugLn(':>[TCocoaAccessibleObject.accessibilitySetValue_forAttribute]');
  {$endif}
  if attribute.isEqualToString(NSAccessibilityFocusedAttribute) then
  begin
    {$ifdef VerboseCDAccessibility}
    DebugLn(':<[TCocoaAccessibleObject.accessibilitySetValue_forAttribute] Control=%s:%s Value=%d',
      [LCLControl.Name, LCLControl.ClassName, NSNumber(value).intValue()]);
    {$endif}
    if not (LCLControl is TWinControl) then Exit;
    if not NSNumber(value).boolValue() then Exit;
    if LCLInjectedControl <> nil then LCLInjectedControl.SetFocus()
    else TWinControl(LCLControl).SetFocus();
  end
  else if attribute.isEqualToString(NSAccessibilityValueAttribute) then
  begin
    LCLControl.Caption := NSStringToString(value);
  end;
end;

function TCocoaAccessibleObject.accessibilityParameterizedAttributeNames: NSArray;
var
  lResult: NSMutableArray;
  lCocoaRole: NSString;
begin
  lResult := NSMutableArray.array_();

  lCocoaRole := TCocoaCustomControl.LazRoleToCocoaRole(LCLAcc.AccessibleRole);

  Result := lResult;
end;

function TCocoaAccessibleObject.accessibilityAttributeValue_forParameter(
  attribute: NSString; parameter: id): id;
begin
  {$ifdef VerboseCDAccessibility}
  DebugLn('[TCocoaAccessibleObject.accessibilityAttributeValue_forParameter]');
  {$endif}
  Result := nil;
end;

function TCocoaAccessibleObject.accessibilityActionNames: NSArray;
var
  lResult: NSMutableArray;
  lCocoaRole: NSString;
begin
  {$ifdef VerboseCDAccessibility}
  DebugLn('[TCocoaAccessibleObject.accessibilityActionNames]');
  {$endif}
  lResult := NSMutableArray.array_();

  lCocoaRole := TCocoaCustomControl.LazRoleToCocoaRole(LCLAcc.AccessibleRole);
  if lCocoaRole.caseInsensitiveCompare(NSAccessibilityButtonRole) = NSOrderedSame then
  begin
    lResult.addObject(NSAccessibilityPressAction);
  end;

  Result := lResult;
end;

function TCocoaAccessibleObject.accessibilityActionDescription(action: NSString): NSString;
begin
  if action = NSAccessibilityPressAction then Result := NSSTR('Press')
  else Result := NSSTR('');
end;

procedure TCocoaAccessibleObject.accessibilityPerformAction(action: NSString);
begin

end;

function TCocoaAccessibleObject.accessibilityIsIgnored: Boolean;
begin
  Result := LCLAcc.AccessibleRole = larIgnore;
end;

function TCocoaAccessibleObject.accessibilityHitTest(point: NSPoint): id;
begin
  Result := inherited accessibilityHitTest(point);
end;

function TCocoaAccessibleObject.accessibilityFocusedUIElement: id;
begin
  Result := inherited accessibilityFocusedUIElement;
end;

{ TCocoaMenu }

procedure TCocoaMenu.lclItemSelected(sender: id);
begin

end;

{ TCocoaMenuItem }

procedure TCocoaMenuItem.lclItemSelected(sender: id);
begin

end;

procedure SetViewDefaults(AView:NSView);
begin
  if not Assigned(AView) then Exit;
  AView.setAutoresizingMask(NSViewMinYMargin or NSViewMaxXMargin);
end;

end.

