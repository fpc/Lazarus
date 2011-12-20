unit CocoaUtils;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  MacOSAll, CocoaAll,
  Types, LCLType;

type
  { NSLCLDebugExtension }

  NSLCLDebugExtension = objccategory(NSObject)
    function lclClassName: shortstring; message 'lclClassName';
  end;

const
  NSNullRect : NSRect = (origin:(x:0; y:0); size:(width:0; height:0));

function GetNSPoint(x,y: single): NSPoint; inline;

function GetCGRect(x1, y1, x2, y2: Integer): CGRect; inline;
function RectToCGRect(const R: TRect): CGRect;
function CGRectToRect(const c: CGRect): TRect;

function GetNSRect(x, y, width, height: Integer): NSRect; inline;
function RectToNSRect(const r: TRect): NSRect;

procedure NSToLCLRect(const ns: NSRect; out lcl: TRect); overload;
procedure NSToLCLRect(const ns: NSRect; ParentHeight: Single; out lcl: TRect); overload;

procedure LCLToNSRect(const lcl: TRect; var ns: NSRect); overload;
procedure LCLToNSRect(const lcl: TRect; ParentHeight: Single; var ns: NSRect); overload;

function CreateParamsToNSRect(const params: TCreateParams): NSRect;

function NSStringUtf8(s: PChar): NSString;
function NSStringUtf8(const s: String): NSString;
function NSStringToString(ns: NSString): String;

function GetNSObjectView(obj: NSObject): NSView;
procedure AddViewToNSObject(ctrl: NSView; obj: NSObject);
procedure AddViewToNSObject(ctrl: NSView; obj: NSObject; X,Y: integer);

procedure SetNSText(text: NSText; const s: String); inline;
function GetNSText(text: NSText): string; inline;

procedure SetNSControlValue(c: NSControl; const S: String); inline;
function GetNSControlValue(c: NSControl): String; inline;

procedure ColorToRGBFloat(cl: TColorRef; var r,g,b: Single); inline;
function RGBToColorFloat(r,g,b: Single): TColorRef; inline;
function NSColorToRGB(const Color: NSColor): TColorRef; inline;

implementation

const
  DEFAULT_CFSTRING_ENCODING = kCFStringEncodingUTF8;

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
  //SetViewFramePos(ctrl, x,y);
end;

function GetNSPoint(x, y: single): NSPoint;
begin
  Result.x:=x;
  Result.y:=y;
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

function RectToCGRect(const R: TRect): CGRect;
begin
  with R do
    Result := GetCGRect(Left, Top, Right, Bottom);
end;

function CGRectToRect(const c:CGRect):TRect;
begin
  with Result do
  begin
    Left := Round(c.origin.x);
    Top := Round(c.origin.y);
    Right := Round(c.origin.x + c.size.width);
    Bottom := Round(c.origin.y + c.size.height);
  end;
end;

function RectToNSRect(const r: TRect): NSRect;
begin
  with R do
    Result := GetNSRect(Left, Top, Right - Left, Bottom - Top);
end;

procedure NSToLCLRect(const ns: NSRect; out lcl: TRect);
begin
  with lcl do
  begin
    Left := Round(ns.origin.x);
    Top := Round(ns.origin.y);
    Right := Round(ns.origin.x + ns.size.width);
    Bottom := Round(ns.origin.y + ns.size.height);
  end;
end;

procedure NSToLCLRect(const ns: NSRect; ParentHeight: Single; out lcl: TRect);
begin
  with lcl do
  begin
    Left := Round(ns.origin.x);
    Top := Round(ParentHeight - ns.size.height - ns.origin.y);
    Right := Round(ns.origin.x + ns.size.width);
    Bottom := Round(lcl.Top + ns.size.height);
  end;
end;

procedure LCLToNSRect(const lcl: TRect; var ns: NSRect); overload;
begin
  ns.origin.x:=lcl.Left;
  ns.origin.y:=lcl.Top;
  ns.size.width:=lcl.Right-lcl.Left;
  ns.size.height:=lcl.Bottom-lcl.Top;
end;

procedure LCLToNSRect(const lcl: TRect; ParentHeight: Single; var ns: NSRect); overload;
begin
  ns.origin.x:=lcl.left;
  ns.origin.y:=ParentHeight-(lcl.bottom-lcl.Top)-lcl.Top;
  ns.size.width:=lcl.Right-lcl.Left;
  ns.size.height:=lcl.Bottom-lcl.Top;
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
var
  ns : NSString;
begin
  if Assigned(text) then
  begin
    ns:=NSStringUTF8(s);
    text.setString(ns);
    ns.release;
  end;
end;

function GetNSText(text: NSText): string; inline;
begin
  if Assigned(text) then
    Result := NSStringToString(text.string_)
  else
    Result:='';
end;

procedure SetNSControlValue(c: NSControl; const S: String); inline;
var
  ns : NSString;
begin
  if Assigned(c) then
  begin
    ns:=NSStringUtf8(S);
    c.setStringValue(ns);
    ns.release;
  end;
end;

function GetNSControlValue(c: NSControl): String; inline;
begin
  if Assigned(c) then
    Result:=NSStringToString(c.stringValue)
  else
    Result:='';
end;


{ NSLCLDebugExtension }

function NSLCLDebugExtension.lclClassName: shortstring;
begin
  Result:=NSStringToString(self.className);
end;

initialization

end.

