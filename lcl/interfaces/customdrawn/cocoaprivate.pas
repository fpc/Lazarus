unit cocoaprivate;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  CGGeometry,
  fpimage, fpcanvas,
  // Custom Drawn Canvas
  IntfGraphics, lazcanvas, customdrawnproc,
  // Libs
  MacOSAll, CocoaAll, CocoaUtils, CocoaGDIObjects, lazutf8sysutils,
  //
  Forms, Controls, LCLMessageGlue, WSControls, LCLType, LCLProc, GraphType;

type
  TCocoaForm = objcclass;

  TCocoaWindow = class(TCDForm)
  public
    CocoaForm: TCocoaForm;
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
    // callback routines
    procedure CallbackActivate; message 'CallbackActivate';
    procedure CallbackDeactivate; message 'CallbackDeactivate';
    procedure CallbackCloseQuery(var CanClose: Boolean); message 'CallbackCloseQuery:';
    procedure CallbackResize; message 'CallbackResize';
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
  end;

procedure SetViewDefaults(AView: NSView);

function Cocoa_RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;
function RawImage_DescriptionToBitmapType(ADesc: TRawImageDescription; out bmpType: TCocoaBitmapType): Boolean;

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
  inherited keyDown(theEvent);
  lKey := MacKeyCodeToLCLKey(theEvent, lSendKey, lSendChar, lUTF8Char);
  //DebugLn('KeyDown='+IntToHex(theEvent.keyCode(), 4));
  if lSendKey then CallbackKeyDown(WindowHandle, lKey);
  if lSendChar then CallbackKeyChar(WindowHandle, 0, lUTF8Char);
end;

procedure TCocoaForm.keyUp(theEvent: NSEvent);
var
  lKey: Word;
  lUTF8Char: TUTF8Char;
  lSendKey, lSendChar: Boolean;
begin
  inherited keyUp(theEvent);
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
    MK_POWER    : Result:=VK_POWER;
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
    MK_TILDE:        Result := VK_TILDE; // `/~ key
    MK_MINUS:        Result := VK_MINUS;
    MK_EQUAL:        Result := VK_LCL_EQUAL;
    MK_BACKSLASH:    Result := VK_BACKSLASH;
    MK_LEFTBRACKET:  Result := VK_OPEN_BRAKET;
    MK_RIGHTBRACKET: Result := VK_CLOSE_BRAKET;
    MK_SEMICOLON:    Result := VK_SEMI_COMMA;
    MK_QUOTE:        Result := VK_QUOTE;
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
      'a'..'z': Result:=VK_A+ord(charactersIgnoringModifiers[1])-ord('a');
      'A'..'Z': Result:=ord(charactersIgnoringModifiers[1]);
      #27     : Result:=VK_ESCAPE;
      #8      : Result:=VK_BACK;
      ' '     : Result:=VK_SPACE;
      #13     : Result:=VK_RETURN;
      '0'..'9': Result:=VK_0+ord(charactersIgnoringModifiers[1])-ord('0');
    end;
  end;

  if Result<>VK_UNKNOWN then SendKeyUpDown:=true;

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
  ns : NSREct;
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

procedure TCocoaForm.CallbackActivate;
begin
  LCLSendActivateMsg(WindowHandle.LCLForm, True, false);
end;

procedure TCocoaForm.CallbackDeactivate;
begin
  LCLSendDeactivateStartMsg(WindowHandle.LCLForm);
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
  lTimeStart: TDateTime;
  {$ENDIF}
begin
  {$IFDEF VerboseCDPaintProfiler}
  lTimeStart := NowUTC();
  {$ENDIF}
  if not Assigned(Context) then Context:=TCocoaContext.Create;

  Context.ctx:=ControlContext;
  lWidth := Round(bounds.size.width);
  lHeight := Round(bounds.size.height);
  WindowHandle.FormRealSize := Types.Size(lWidth, lHeight);
  lHeight := WindowHandle.GetFormBufferHeight(lHeight);
  if Context.InitDraw(lWidth, lHeight) then
  begin
    // Prepare the non-native image and canvas
    UpdateControlLazImageAndCanvas(WindowHandle.Image,
      WindowHandle.Canvas, lWidth, lHeight, clfRGB24UpsideDown);
    DrawFormBackground(WindowHandle.Image, WindowHandle.Canvas);
    WindowHandle.Canvas.NativeDC := PtrInt(Context);

    // Draw the form
    RenderForm(WindowHandle.Image, WindowHandle.Canvas, WindowHandle.LCLForm);

    // Now render it into the control
    WindowHandle.Image.GetRawImage(lRawImage);
    Cocoa_RawImage_CreateBitmaps(lRawImage, lBitmap, lMask, True);
    Context.DrawBitmap(0, WindowHandle.ScrollY, TCocoaBitmap(lBitmap));
  end;
  {$IFDEF VerboseCDPaintProfiler}
  DebugLn(Format('[TCocoaCustomControl.Draw] Paint duration: %d ms', [DateTimeToMilliseconds(NowUTC() - lTimeStart)]));
  {$ENDIF}
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

procedure SetViewDefaults(AView:NSView);
begin
  if not Assigned(AView) then Exit;
  AView.setAutoresizingMask(NSViewMinYMargin or NSViewMaxXMargin);
end;

end.

