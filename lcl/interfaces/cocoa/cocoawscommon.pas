unit CocoaWSCommon;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Types,
  MacOSAll, CocoaAll,
  Classes, Controls, SysUtils,
  //
  WSControls, LCLType, LCLProc, Forms,
  CocoaPrivate, CocoaGDIObjects, CocoaUtils, LCLMessageGlue;

type

  { LCLWSViewExtension }

  LCLWSViewExtension = objccategory(NSView)
    function lclInitWithCreateParams(const AParams: TCreateParams): id; message 'lclInitWithCreateParams:';
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
    procedure ResetCursorRects; override;
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


// Utility WS functions

function AllocCustomControl(const AWinControl: TWinControl): TCocoaCustomControl;
function EmbedInScrollView(AView: NSView): TCocoaScrollView;
procedure SetViewDefaults(AView: NSView);

implementation

function AllocCustomControl(const AWinControl: TWinControl): TCocoaCustomControl;
begin
  if not Assigned(AWinControl) then begin
    Result:=nil;
    Exit;
  end;
  Result:=TCocoaCustomControl(TCocoaCustomControl.alloc).init;
  Result.callback:=TLCLCommonCallback.Create(Result, AWinControl);
end;

function EmbedInScrollView(AView:NSView):TCocoaScrollView;
var
  r : TRect;
  p : NSView;
begin
  if not Assigned(AView) then begin
    Result:=nil;
    Exit;
  end;
  r:=AView.lclFrame;
  p:=AView.superview;
  Result:=TCocoaScrollView.alloc.initWithFrame(NSNullRect);
  if Assigned(p) then p.addSubView(Result);
  Result.lclSetFrame(r);
  Result.setDocumentView(AView);
  SetViewDefaults(Result);
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
begin
  if not Assigned(Context) then Context:=TCocoaContext.Create;

  Context.ctx:=ControlContext;
  if Context.InitDraw(Round(bounds.size.width), Round(bounds.size.height)) then
  begin
    FillChar(struct, SizeOf(TPaintStruct), 0);
    struct.hdc := HDC(Context);
    {$IFDEF VerboseWinAPI}
      DebugLn(Format('[TLCLCommonCallback.Draw] OnPaint event started context: %x', [HDC(context)]));
    {$ENDIF}
    LCLSendPaintMsg(Target, HDC(Context), @struct);
    {$IFDEF VerboseWinAPI}
      DebugLn('[TLCLCommonCallback.Draw] OnPaint event ended');
    {$ENDIF}
  end;
end;

procedure TLCLCommonCallback.ResetCursorRects;
var
  ACursor: TCursor;
  AControl: TControl;
  View: NSView;
begin
  if Owner.isKindOfClass_(NSWindow) then
    View := NSwindow(Owner).contentView
  else
  if Owner.isKindOfClass_(NSView) then
    View := NSView(Owner)
  else
    Exit;
  if not (csDesigning in Target.ComponentState) then
  begin
    ACursor := Screen.Cursor;
    if ACursor = crDefault then
    begin
      // traverse visible child controls
      ACursor := Target.Cursor;
    end;
    if ACursor <> crDefault then
      View.addCursorRect_cursor(View.visibleRect, TCocoaCursor(Screen.Cursors[ACursor]).Cursor);
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

