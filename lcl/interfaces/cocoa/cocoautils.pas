unit CocoaUtils;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  MacOSAll, CocoaAll,
  Types, LCLType;

function GetNSRect(x, y, width, height: Integer): NSRect; inline;
function RectToNSRect(const r: TRect): NSRect;
function CreateParamsToNSRect(const params: TCreateParams): NSRect;

function NSStringUtf8(s: PChar): NSString;
function NSStringUtf8(const s: String): NSString;
function NSStringToString(ns: NSString): String;

function GetNSObjectView(obj: NSObject): NSView;
procedure AddViewToNSObject(ctrl: NSView; obj: NSObject);
procedure AddViewToNSObject(ctrl: NSView; obj: NSObject; X,Y: integer);

function GetCocoaRect(parent: NSView; const ChildBounds: TRect): NSRect;
function GetCocoaRect(parent: NSView; ChildX, ChildY, ChildWidth, ChildHeight: Integer): NSRect;

procedure SetViewBoundsPos(view: NSView; X,Y: integer; invalidateView: Boolean=false);

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
procedure SetViewBoundsPos(view: NSView; X,Y: integer; invalidateView: Boolean);
var
  parent : NSView;
  pb, b  : NSRect;
begin
  parent:=view.superView;
  if not Assigned(parent) then Exit;
  pb:=parent.bounds;
  b:=view.frame;
  b.origin.x:=X;
  b.origin.y:=pb.size.height-y-b.size.height;
  view.setFrame(b);
  if invalidateView then view.setNeedsDisplay_(true);
end;

function GetCocoaRect(parent: NSView; ChildX, ChildY, ChildWidth, ChildHeight: Integer): NSRect;
var
  b : NSRect;
begin
  if not Assigned(parent) then begin
    Result.origin.x:=ChildX;
    Result.origin.y:=ChildY;
    Result.size.width:=ChildWidth;
    Result.size.height:=ChildHeight;
  end else begin
    b:=parent.bounds;
    Result.origin.x:=ChildX;
    Result.origin.y:=b.size.height - ChildY;
    Result.size.width:=ChildWidth;
    Result.size.height:=ChildHeight;
  end;
end;

function GetCocoaRect(parent: NSView; const ChildBounds: TRect): NSRect;
begin
  with ChildBounds do
    Result:=GetCocoaRect(parent, Left, Top, Right-Left, Bottom-Top);
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
  SetViewBoundsPos(ctrl, x,y);
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

initialization


end.

