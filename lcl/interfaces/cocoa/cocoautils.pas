unit CocoaUtils;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  MacOSAll, CocoaAll,
  Types, LCLType;

function GetNSPoint(x,y: single): NSPoint; inline;

function GetNSRect(x, y, width, height: Integer): NSRect; inline;
function RectToNSRect(const r: TRect): NSRect;
procedure NSRectToRect(const ns: NSRect; var r: TRect);
function CreateParamsToNSRect(const params: TCreateParams): NSRect;

function NSStringUtf8(s: PChar): NSString;
function NSStringUtf8(const s: String): NSString;
function NSStringToString(ns: NSString): String;

function GetNSObjectView(obj: NSObject): NSView;
procedure AddViewToNSObject(ctrl: NSView; obj: NSObject);
procedure AddViewToNSObject(ctrl: NSView; obj: NSObject; X,Y: integer);

procedure SetViewFramePos(view: NSView; X,Y: Single; invalidateView: Boolean=false);
procedure SetViewFrame(view: NSView; X,Y,W,H: Single; invalidateView: Boolean=false);
procedure GetViewFrame(view: NSView; var FrameRect: TRect);

procedure SetNSText(text: NSText; const s: String); inline;
function GetNSText(text: NSText): string; inline;

procedure SetNSControlValue(c: NSControl; const S: String); inline;
function GetNSControlValue(c: NSControl): String; inline;

procedure LCLToCocoaRect(const lcl, parent: NSRect; var cocoa: NSRect); inline;
procedure CocoaToLCLRect(const cocoa, parent: NSRect; var lcl: NSRect); inline;

function GetNSWindowFrame(win: NSWindow; var lcl: TRect): Boolean; inline;
function GetNSViewFrame(view: NSView; var lcl: TRect): boolean; inline;

implementation

const
  DEFAULT_CFSTRING_ENCODING = kCFStringEncodingUTF8;

function CFStringToStr(AString: CFStringRef; Encoding: CFStringEncoding = DEFAULT_CFSTRING_ENCODING): String;
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

{X,Y - coordinates relative to Left-Top (LCL-style)}
procedure SetViewFramePos(view: NSView; X,Y: Single; invalidateView: Boolean);
var
  f : NSRect;
begin
  f:=view.frame;
  SetViewFrame(view,x,y,f.size.width, f.size.height, invalidateView);
end;

{X,Y - coordinates relative to Left-Top (LCL-style)}
{W,H - width and height of the NSView}
procedure SetViewFrame(view: NSView; X,Y,W,H: single; invalidateView: Boolean=false); overload;
var
  parent : NSView;
  pb, b  : NSRect;
begin
  parent:=view.superView;
  if not Assigned(parent) then Exit;
  pb:=parent.bounds;
  b:=view.frame;
  b.origin.x:=X;
  b.origin.y:=pb.size.height-y-h;
  b.size.width:=w;
  b.size.height:=h;
  view.setFrame(b);
  if invalidateView then view.setNeedsDisplay_(true);
end;

procedure GetViewFrame(view: NSView; var FrameRect: TRect);
var
  parent : NSView;
  pb, f  : NSRect;
begin
  f:=view.frame;
  parent:=view.superView;
  if Assigned(parent) then begin
    pb:=parent.bounds;
    f.origin.y:=pb.size.height-f.origin.y-f.size.height;
  end;
  NSRectToRect(f, FrameRect);
end;

function GetNSObjectView(obj: NSObject): NSView;
begin
  Result:=nil;
  if not Assigned(obj) then Exit;
  if obj.isKindOfClass_(NSView) then Result:=NSView(obj)
  else if obj.isKindOfClass_(NSWindow) then Result:=NSWindow(obj).contentView;
end;

procedure AddViewToNSObject(ctrl: NSView; obj: NSObject);
var
  view : NSView;
begin
  view:=GetNSObjectView(obj);
  if not Assigned(view) then Exit;
  view.addSubView(ctrl);
end;

procedure AddViewToNSObject(ctrl: NSView; obj: NSObject; X,Y: integer);
begin
  AddViewToNSObject(ctrl, obj);
  SetViewFramePos(ctrl, x,y);
end;

function GetNSPoint(x, y: single): NSPoint;
begin
  Result.x:=x;
  Result.y:=y;
end;

function GetNSRect(x, y, width, height: Integer): NSRect;
begin
  Result.origin.x:=x;
  Result.origin.y:=y;
  Result.size.width:=width;
  Result.size.height:=height;
end;

function RectToNSRect(const r: TRect): NSRect;
begin
  Result:=GetNSRect(r.Left,r.Top,r.Right-r.Left,r.Bottom-r.Top);
end;

procedure NSRectToRect(const ns: NSRect; var r: TRect);
begin
  r.Left:=round(ns.origin.x);
  r.Top:=round(ns.origin.y);
  r.Right:=round(ns.origin.x+ns.size.width);
  r.Bottom:=round(ns.origin.y+ns.size.height);
end;

function CreateParamsToNSRect(const params: TCreateParams): NSRect;
begin
  with params do Result:=GetNSRect(X,Y,Width,Height);
end;

function NSStringUtf8(s: PChar): NSString;
var
  cf : CFStringRef;
begin
  {NSString and CFStringRef are interchangable}
  cf:=CFStringCreateWithCString(nil, S, kCFStringEncodingUTF8);
  Result:=NSString(cf);
end;

function NSStringUtf8(const s: String): NSString;
var
  cf : CFStringRef;
begin
  {NSString and CFStringRef are interchangable}
  cf:=CFStringCreateWithCString(nil, Pointer(PChar(S)), kCFStringEncodingUTF8);
  Result:=NSString(cf);
end;

function NSStringToString(ns: NSString): String;
begin
  Result:=CFStringToStr(CFStringRef(ns));
end;

procedure SetNSText(text: NSText; const s: String); inline;
begin
  if Assigned(text) then
    text.setString(NSStringUTF8(s));
end;

function GetNSText(text: NSText): string; inline;
begin
  if Assigned(text) then
    Result := NSStringToString(text.string_)
  else
    Result:='';
end;

procedure SetNSControlValue(c: NSControl; const S: String); inline;
begin
  if Assigned(c) then
    c.setStringValue(NSStringUtf8(S));
end;

function GetNSControlValue(c: NSControl): String; inline;
begin
  if Assigned(c) then
    Result:=NSStringToString(c.stringValue)
  else
    Result:='';
end;


procedure LCLToCocoaRect(const lcl, parent: NSRect; var cocoa: NSRect);
begin
  cocoa.origin.x:=lcl.origin.x;
  cocoa.origin.y:=parent.size.height - lcl.size.height - lcl.origin.y;
  cocoa.size:=lcl.size;
end;

procedure CocoaToLCLRect(const cocoa, parent: NSRect; var lcl: NSRect);
begin
  lcl.origin.x:=cocoa.origin.x;
  lcl.origin.y:=parent.size.height-cocoa.size.height-cocoa.origin.y;
  lcl.size:=cocoa.size;
end;

function GetNSWindowFrame(win: NSWindow; var lcl: TRect): Boolean;
var
  f : NSRect;
begin
  if Assigned(win.screen) then begin
    CocoaToLCLRect( win.frame, win.screen.frame, f);
    NSRectToRect(f, lcl);
  end else
    NSRectToRect(win.frame, lcl);
  Result:=true;
end;

function GetNSViewFrame(view: NSView; var lcl: TRect): boolean; inline;
var
  f : NSRect;
begin
  if Assigned(view.superview) then begin
    CocoaToLCLRect( view.frame, view.superview.frame, f);
    NSRectToRect(f, lcl);
  end else
    NSRectToRect(view.frame, lcl);
  Result:=true;
end;

initialization


end.

