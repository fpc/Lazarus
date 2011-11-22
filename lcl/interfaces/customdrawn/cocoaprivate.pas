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
  MacOSAll, CocoaAll, CocoaUtils, CocoaGDIObjects,
  //
  Controls, LCLMessageGlue, WSControls, LCLType, LCLProc, GraphType;

type
  { LCLObjectExtension }

  LCLObjectExtension = objccategory(NSObject)
    function lclIsEnabled: Boolean; message 'lclIsEnabled';
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:';
    function lclIsVisible: Boolean; message 'lclIsVisible';

    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:';
    procedure lclInvalidate; message 'lclInvalidate';
    procedure lclRelativePos(var Left, Top: Integer); message 'lclRelativePos::';
    procedure lclLocalToScreen(var X,Y: Integer); message 'lclLocalToScreen::';
    function lclParent: id; message 'lclParent';
    function lclFrame: TRect; message 'lclFrame';
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:';
    function lclClientFrame: TRect; message 'lclClientFrame';
  end;

  { LCLViewExtension }

  LCLViewExtension = objccategory(NSView)
    function lclIsVisible: Boolean; message 'lclIsVisible'; reintroduce;
    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:'; reintroduce;
    procedure lclInvalidate; message 'lclInvalidate'; reintroduce;
    procedure lclLocalToScreen(var X,Y: Integer); message 'lclLocalToScreen::'; reintroduce;
    function lclParent: id; message 'lclParent'; reintroduce;
    function lclFrame: TRect; message 'lclFrame'; reintroduce;
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:'; reintroduce;
    function lclClientFrame: TRect; message 'lclClientFrame'; reintroduce;
  end;

  { LCLControlExtension }

  LCLControlExtension = objccategory(NSControl)
    function lclIsEnabled: Boolean; message 'lclIsEnabled'; reintroduce;
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:'; reintroduce;
  end;

  { LCLWindowExtension }

  LCLWindowExtension = objccategory(NSWindow)
    function lclIsVisible: Boolean; message 'lclIsVisible'; reintroduce;
    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:'; reintroduce;
    procedure lclInvalidate; message 'lclInvalidate'; reintroduce;
    procedure lclLocalToScreen(var X,Y: Integer); message 'lclLocalToScreen::'; reintroduce;
    function lclFrame: TRect; message 'lclFrame'; reintroduce;
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:'; reintroduce;
    function lclClientFrame: TRect; message 'lclClientFrame'; reintroduce;
  end;

  { TCommonCallback }

  TCommonCallback = class(TObject)
  public
    Owner : NSObject;
    constructor Create(AOwner: NSObject);
    procedure MouseDown(x,y: Integer); virtual; abstract;
    procedure MouseUp(x,y: Integer); virtual; abstract;
    procedure MouseClick(ClickCount: Integer); virtual; abstract;
    procedure MouseMove(x,y: Integer); virtual; abstract;
    procedure Draw(ctx: NSGraphicsContext; const bounds, dirty: NSRect); virtual; abstract;
  end;

  { TWindowCallback }

  TWindowCallback = class(TObject)
  public
    Owner : NSWindow;
    constructor Create(AOwner: NSWindow);
    procedure Activate; virtual; abstract;
    procedure Deactivate; virtual; abstract;
    procedure CloseQuery(var CanClose: Boolean); virtual; abstract;
    procedure Close; virtual; abstract;
    procedure Resize; virtual; abstract;
  end;

  { TCocoaWindow }

  TCocoaWindow = objcclass(NSWindow, NSWindowDelegateProtocol)
  protected
    function windowShouldClose(sender : id): LongBool; message 'windowShouldClose:';
    procedure windowWillClose(notification: NSNotification); message 'windowWillClose:';
    procedure windowDidBecomeKey(notification: NSNotification); message 'windowDidBecomeKey:';
    procedure windowDidResignKey(notification: NSNotification); message 'windowDidResignKey:';
    procedure windowDidResize(notification: NSNotification); message 'windowDidResize:';
  public
    callback      : TCommonCallback;
    wincallback   : TWindowCallback;
    function acceptsFirstResponder: Boolean; override;
    procedure mouseUp(event: NSEvent); override;
    procedure mouseDown(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
  end;

  { TCocoaCustomControl }

  TCocoaCustomControl = objcclass(NSControl)
    callback  : TCommonCallback;
    Image: TLazIntfImage;
    Canvas: TLazCanvas;
    procedure drawRect(dirtyRect: NSRect); override;
  end;

  TLCLWindowCallback=class(TWindowCallback)
  public
    Target  : TControl;
    constructor Create(AOwner: NSWindow; ATarget: TControl);
    procedure Activate; override;
    procedure Deactivate; override;
    procedure CloseQuery(var CanClose: Boolean); override;
    procedure Close; override;
    procedure Resize; override;
  end;

  { TLCLCommonCallback }

  TLCLCommonCallback = class(TCommonCallback)
  public
    Target  : TControl;
    Context : TCocoaContext;
    constructor Create(AOwner: NSObject; ATarget: TControl);
    destructor Destroy; override;
    procedure MouseDown(x,y: Integer); override;
    procedure MouseUp(x,y: Integer); override;
    procedure MouseClick(clickCount: Integer); override;
    procedure MouseMove(x,y: Integer); override;
    procedure Draw(ControlContext: NSGraphicsContext; const bounds, dirty: NSRect); override;
  end;

  { TCocoaWSWinControl }

  TCocoaWSWinControl=class(TWSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class function GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; override;

    class function  GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class function  GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
  end;


  { TCocoaWSCustomControl }

  TCocoaWSCustomControl=class(TWSCustomControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  LCLWSViewExtension = objccategory(NSView)
    function lclInitWithCreateParams(const AParams: TCreateParams): id; message 'lclInitWithCreateParams:';
  end;

function AllocCustomControl(const AWinControl: TWinControl): TCocoaCustomControl;
procedure SetViewDefaults(AView: NSView);

function Cocoa_RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;
function RawImage_DescriptionToBitmapType(ADesc: TRawImageDescription; out bmpType: TCocoaBitmapType): Boolean;

implementation

uses customdrawnwsforms;

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

{ TCocoaWindow }

function TCocoaWindow.windowShouldClose(sender: id): LongBool;
var
  canClose : Boolean;
begin
  canClose:=true;
  wincallback.CloseQuery(canClose);
  Result:=canClose;
end;

procedure TCocoaWindow.windowWillClose(notification: NSNotification);
begin
  wincallback.Close;
end;

procedure TCocoaWindow.windowDidBecomeKey(notification: NSNotification);
begin
  wincallback.Activate;
end;

procedure TCocoaWindow.windowDidResignKey(notification: NSNotification);
begin
  wincallback.Deactivate;
end;

procedure TCocoaWindow.windowDidResize(notification: NSNotification);
begin
  wincallback.Resize;
end;

function TCocoaWindow.acceptsFirstResponder: Boolean;
begin
  Result:=true;
end;

procedure TCocoaWindow.mouseUp(event: NSEvent);
var
  mp : NSPoint;
begin
  mp:=event.locationInWindow;
  mp.y:=NSView(event.window.contentView).bounds.size.height-mp.y;
  callback.MouseUp(round(mp.x), round(mp.y));
  inherited mouseUp(event);
end;

procedure TCocoaWindow.mouseDown(event: NSEvent);
var
  mp : NSPoint;
begin
  mp:=event.locationInWindow;
  mp.y:=NSView(event.window.contentView).bounds.size.height-mp.y;
  callback.MouseDown(round(mp.x), round(mp.y));
  inherited mouseDown(event);
end;

procedure TCocoaWindow.mouseDragged(event: NSEvent);
var
  mp : NSPoint;
begin
  mp:=event.locationInWindow;
  mp.y:=NSView(event.window.contentView).bounds.size.height-mp.y;
  callback.MouseMove(round(mp.x), round(mp.y));
  inherited mouseMoved(event);
end;

procedure TCocoaWindow.mouseMoved(event: NSEvent);
var
  mp : NSPoint;
begin
  mp:=event.locationInWindow;
  mp.y:=NSView(event.window.contentView).bounds.size.height-mp.y;
  callback.MouseMove(round(mp.x), round(mp.y));
  inherited mouseMoved(event);
end;

procedure TCocoaWindow.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaWindow.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

{ TCommonCallback }

constructor TCommonCallback.Create(AOwner: NSObject);
begin
  Owner:=AOwner;
end;

{ TWindowCallback }

constructor TWindowCallback.Create(AOwner: NSWindow);
begin
  Owner:=AOwner;
end;

{ TCocoaCustomControl }

procedure TCocoaCustomControl.drawRect(dirtyRect:NSRect);
begin
  inherited drawRect(dirtyRect);
  callback.Draw(NSGraphicsContext.currentContext, bounds, dirtyRect);
end;

{ LCLObjectExtension }

function LCLObjectExtension.lclIsEnabled:Boolean;
begin
  Result:=False;
end;

procedure LCLObjectExtension.lclSetEnabled(AEnabled:Boolean);
begin
end;

function LCLObjectExtension.lclIsVisible:Boolean;
begin
  Result:=False;
end;

procedure LCLObjectExtension.lclInvalidateRect(const r:TRect);
begin

end;

procedure LCLObjectExtension.lclInvalidate;
begin

end;

procedure LCLObjectExtension.lclRelativePos(var Left,Top:Integer);
begin

end;

procedure LCLObjectExtension.lclLocalToScreen(var X,Y:Integer);
begin

end;

function LCLObjectExtension.lclParent:id;
begin
  Result:=nil;
end;

function LCLObjectExtension.lclFrame:TRect;
begin
  FillChar(Result, sizeof(Result), 0);
end;

procedure LCLObjectExtension.lclSetFrame(const r:TRect);
begin

end;

function LCLObjectExtension.lclClientFrame:TRect;
begin
  FillChar(Result, sizeof(Result), 0);
end;

{ LCLControlExtension }

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

function LCLControlExtension.lclIsEnabled:Boolean;
begin
  Result:=IsEnabled;
end;

procedure LCLControlExtension.lclSetEnabled(AEnabled:Boolean);
begin
  SetEnabled(AEnabled);
end;

function LCLViewExtension.lclIsVisible:Boolean;
begin
  Result:=not isHidden;
end;

procedure LCLViewExtension.lclInvalidateRect(const r:TRect);
begin
  setNeedsDisplayInRect(RectToViewCoord(Self, r));
end;

procedure LCLViewExtension.lclInvalidate;
begin
  setNeedsDisplay_(True);
end;

procedure LCLViewExtension.lclLocalToScreen(var X,Y:Integer);
begin

end;

function LCLViewExtension.lclParent:id;
begin
  Result:=superView;
end;

function LCLViewExtension.lclFrame: TRect;
var
  v : NSView;
begin
  v:=superview;
  if Assigned(v)
    then NSToLCLRect(frame, v.frame.size.height, Result)
    else NSToLCLRect(frame, Result);
end;

procedure LCLViewExtension.lclSetFrame(const r:TRect);
var
  ns : NSRect;
begin
  if Assigned(superview)
    then LCLToNSRect(r, superview.frame.size.height, ns)
    else LCLToNSRect(r, ns);
  setFrame(ns);
end;

function LCLViewExtension.lclClientFrame:TRect;
var
  r: NSRect;
begin
  r:=bounds;
  Result.Left:=0;
  Result.Top:=0;
  Result.Right:=Round(r.size.width);
  Result.Bottom:=Round(r.size.height);
end;

{ LCLWindowExtension }

function LCLWindowExtension.lclIsVisible:Boolean;
begin
  Result:=isVisible;
end;

procedure LCLWindowExtension.lclInvalidateRect(const r:TRect);
begin
  contentView.lclInvalidateRect(r);
end;

procedure LCLWindowExtension.lclInvalidate;
begin
  contentView.lclInvalidate;
end;

procedure LCLWindowExtension.lclLocalToScreen(var X,Y:Integer);
var
  f   : NSRect;
begin
  if Assigned(screen) then begin
    f:=frame;
    x:=Round(f.origin.x+x);
    y:=Round(screen.frame.size.height-f.size.height-f.origin.y);
  end;
end;

function LCLWindowExtension.lclFrame:TRect;
begin
  if Assigned(screen)
    then NSToLCLRect(frame, screen.frame.size.height, Result)
    else NSToLCLRect(frame, Result);
end;

procedure LCLWindowExtension.lclSetFrame(const r:TRect);
var
  ns : NSREct;
begin
  if Assigned(screen)
    then LCLToNSRect(r, screen.frame.size.height, ns)
    else LCLToNSRect(r, ns);
  setFrame_display(ns, isVisible);
end;

function LCLWindowExtension.lclClientFrame:TRect;
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

{ TLCLWindowCallback }

constructor TLCLWindowCallback.Create(AOwner: NSWindow; ATarget: TControl);
begin
  inherited Create(AOwner);
  Target:=ATarget;
end;

procedure TLCLWindowCallback.Activate;
begin
  LCLSendActivateMsg(Target, True, false);
end;

procedure TLCLWindowCallback.Deactivate;
begin
  LCLSendDeactivateStartMsg(Target);
end;

procedure TLCLWindowCallback.CloseQuery(var CanClose: Boolean);
begin
  // Message results : 0 - do nothing, 1 - destroy window
  CanClose:=LCLSendCloseQueryMsg(Target)>0;
end;

procedure TLCLWindowCallback.Close;
begin
  LCLSendCloseUpMsg(Target);
end;

procedure TLCLWindowCallback.Resize;
var
  sz  : NSSize;
  r   : TRect;
begin
  sz := Owner.frame.size;
  TCDWSCustomForm.GetClientBounds(TWinControl(Target), r);
  if Assigned(Target) then
    LCLSendSizeMsg(Target, Round(sz.width), Round(sz.height), SIZENORMAL);
end;

function AllocCustomControl(const AWinControl: TWinControl): TCocoaCustomControl;
begin
  if not Assigned(AWinControl) then begin
    Result:=nil;
    Exit;
  end;
  Result:=TCocoaCustomControl(TCocoaCustomControl.alloc).init;
  Result.callback:=TLCLCommonCallback.Create(Result, AWinControl);
end;

procedure SetViewDefaults(AView:NSView);
begin
  if not Assigned(AView) then Exit;
  AView.setAutoresizingMask(NSViewMinYMargin or NSViewMaxXMargin);
end;


{ TLCLCommonCallback }

constructor TLCLCommonCallback.Create(AOwner: NSObject; ATarget: TControl);
begin
  inherited Create(AOwner);
  Target:=ATarget;
end;

destructor TLCLCommonCallback.Destroy;
begin
  Context.Free;
  inherited Destroy;
end;

procedure TLCLCommonCallback.MouseDown(x, y: Integer);
begin
  LCLSendMouseDownMsg(Target,x,y,mbLeft, []);
end;

procedure TLCLCommonCallback.MouseUp(x, y: Integer);
begin
  LCLSendMouseUpMsg(Target,x,y,mbLeft, []);
end;

procedure TLCLCommonCallback.MouseClick(clickCount: Integer);
begin
  LCLSendClickedMsg(Target);
end;

procedure TLCLCommonCallback.MouseMove(x, y: Integer);
begin
  LCLSendMouseMoveMsg(Target, x,y, []);
end;

procedure TLCLCommonCallback.Draw(ControlContext: NSGraphicsContext;
  const bounds, dirty:NSRect);
var
  struct : TPaintStruct;
  lWidth, lHeight: Integer;
  lBitmap, lMask: HBITMAP;
  lRawImage: TRawImage;
begin
  if not Assigned(Context) then Context:=TCocoaContext.Create;

  Context.ctx:=ControlContext;
  lWidth := Round(bounds.size.width);
  lHeight := Round(bounds.size.height);
  if Context.InitDraw(lWidth, lHeight) then
  begin
    // Prepare the non-native image and canvas
    FillChar(struct, SizeOf(TPaintStruct), 0);

    UpdateControlLazImageAndCanvas(TCocoaCustomControl(Owner).Image,
      TCocoaCustomControl(Owner).Canvas, lWidth, lHeight);

    struct.hdc := HDC(TCocoaCustomControl(Owner).Canvas);

    // Send the paint message to the LCL
    {$IFDEF VerboseWinAPI}
      DebugLn(Format('[TLCLCommonCallback.Draw] OnPaint event started context: %x', [struct.hdc]));
    {$ENDIF}
    LCLSendPaintMsg(Target, struct.hdc, @struct);
    {$IFDEF VerboseWinAPI}
      DebugLn('[TLCLCommonCallback.Draw] OnPaint event ended');
    {$ENDIF}

    // Now render it into the control
    TCocoaCustomControl(Owner).Image.GetRawImage(lRawImage);
    Cocoa_RawImage_CreateBitmaps(lRawImage, lBitmap, lMask, True);
    Context.DrawBitmap(0, 0, TCocoaBitmap(lBitmap));
  end;
end;

{ TCocoaWSWinControl }

class function TCocoaWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result:=TCocoaWSCustomControl.CreateHandle(AWinControl, AParams);
end;

class procedure TCocoaWSWinControl.SetText(const AWinControl: TWinControl; const AText: String);
var
  obj : NSObject;
begin
  // sanity check
  obj:=NSObject(AWinControl.Handle);
  if not Assigned(obj) or not obj.isKindOfClass_(NSControl) then Exit;

  SetNSControlValue(NSControl(obj), AText);
end;

class function TCocoaWSWinControl.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  obj   : NSObject;
begin
  Result:=false;

  // sanity check
  obj:=NSObject(AWinControl.Handle);
  Result:=Assigned(obj) and obj.isKindOfClass_(NSControl);
  if not Result then Exit;

  AText:=GetNSControlValue(NSControl(obj));
  Result:=true;
end;

class function TCocoaWSWinControl.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;
var
  obj : NSObject;
  s   : NSString;
begin
  Result:=false;

  // sanity check
  obj:=NSObject(AWinControl.Handle);
  Result:= Assigned(obj) and obj.isKindOfClass_(NSControl);
  if not Result then Exit;

  s:=NSControl(obj).stringValue;
  if Assigned(s) then ALength:=s.length
  else ALength:=0
end;

class function TCocoaWSWinControl.GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  Result:=(AWinControl.Handle<>0);
  if not Result then Exit;
  ARect:=NSObject(AWinControl.Handle).lclClientFrame;
end;

class function TCocoaWSWinControl.GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  Result:=(AWinControl.Handle<>0);
  if not Result then Exit;
  ARect:=NSObject(AWinControl.Handle).lclClientFrame;
end;

class procedure TCocoaWSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
begin
  if (AWinControl.Handle<>0) then
    NSObject(AWinControl.Handle).lclSetFrame(Bounds(ALeft, ATop, AWidth, AHeight));
end;

{ TCocoaWSCustomControl }

class function TCocoaWSCustomControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  ctrl  : TCocoaCustomControl;
begin
  ctrl:=TCocoaCustomControl( NSView(TCocoaCustomControl.alloc).lclInitWithCreateParams(AParams));
  ctrl.callback:=TLCLCommonCallback.Create(ctrl, AWinControl);
  Result:=TLCLIntfHandle(ctrl);
end;

{ LCLWSViewExtension }

function LCLWSViewExtension.lclInitWithCreateParams(const AParams:TCreateParams): id;
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

end.

