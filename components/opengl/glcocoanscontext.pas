{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  ToDo:
    use custom pixelformat
      attributes: doublebufferd, version, ...
    SwapBuffers
    Mouse,
    SharedControl
}
unit GLCocoaNSContext;

{$mode objfpc}{$H+}
{$ModeSwitch objectivec1}

interface

uses
  Classes, SysUtils, types, CocoaWSCommon, CocoaPrivate, CocoaUtils, LCLType,
  Controls, LazLoggerBase, WSLCLClasses, gl, MacOSAll, CocoaAll;

procedure LOpenGLViewport(Left, Top, Width, Height: integer);
procedure LOpenGLSwapBuffers(Handle: HWND);
function LOpenGLMakeCurrent(Handle: HWND): boolean;
function LOpenGLReleaseContext(Handle: HWND): boolean;
procedure LOpenGLClip(Handle: HWND);
function LOpenGLCreateContext(AWinControl: TWinControl;
              {%H-}WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
              DoubleBuffered: boolean;
              MajorVersion, MinorVersion: Cardinal;
              MultiSampling, AlphaBits, DepthBits, StencilBits, AUXBuffers: Cardinal;
              const {%H-}AParams: TCreateParams): HWND;
procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);
function CreateOpenGLContextAttrList(DoubleBuffered: boolean;
              MajorVersion, MinorVersion: Cardinal;
              MultiSampling, AlphaBits, DepthBits,
              StencilBits, AUXBuffers: cardinal): NSOpenGLPixelFormatAttributePtr;

const
  // missing constants in FPC 3.1.1 rev 31197 and below
  NSOpenGLPFAProfile            = 99;
  NSOpenGLProfileLegacy         = $1000;
  NSOpenGLProfileVersion3_2Core = $3200;
  NSOpenGLProfileVersion4_1Core = $4100;

type
  TDummyNoWarnObjCNotUsed = objc.BOOL;
  TDummyNoWarnObjCBaseNotUsed = objcbase.NSInteger;

  { TCocoaOpenGLView }

  TCocoaOpenGLView = objcclass(NSOpenGLView)
  public
    Owner: TWinControl;
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    procedure drawRect(dirtyRect: NSRect); override;
    procedure dealloc; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function lclIsHandle: Boolean; override;
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;
    // key
    procedure keyDown(event: NSEvent); override;
    procedure keyUp(event: NSEvent); override;
    procedure flagsChanged(event: NSEvent); override;
    // other
    procedure resetCursorRects; override;
  end;

function GetCGLContextObj(OpenGLControlHandle: HWND): CGLContextObj;
function CreateCGLContextAttrList(DoubleBuffered: boolean;
              {$IFDEF UsesModernGL}
              MajorVersion, MinorVersion: Cardinal;
              {$ENDIF}
              MultiSampling, AlphaBits, DepthBits,
              StencilBits, AUXBuffers: cardinal): PInteger;
function IsCGLPixelFormatAvailable(Attribs: PInteger): boolean;

implementation

procedure LOpenGLViewport(Left, Top, Width, Height: integer);
begin
  glViewport(Left,Top,Width,Height);
end;

procedure LOpenGLSwapBuffers(Handle: HWND);
begin
  if Handle=0 then exit;
  glFlush();
  // ToDo
end;

function LOpenGLMakeCurrent(Handle: HWND): boolean;
begin
  if Handle=0 then exit(false);
  //CGLContext:=GetCGLContextObj(Handle);
  //Result:=CGLSetCurrentContext(CGLContext)=kCGLNoError;
  Result:=true;
end;

function LOpenGLReleaseContext(Handle: HWND): boolean;
begin
  if Handle=0 then exit(false);
  //Result:=CGLSetCurrentContext(nil)=kCGLNoError;
  Result:=true;
end;

procedure LOpenGLClip(Handle: HWND);
begin
  if Handle=0 then exit;
  // ToDo
end;

function LOpenGLCreateContext(AWinControl: TWinControl;
  WSPrivate: TWSPrivateClass; SharedControl: TWinControl; DoubleBuffered: boolean;
  MajorVersion, MinorVersion: Cardinal;
  MultiSampling, AlphaBits, DepthBits, StencilBits,
  AUXBuffers: Cardinal; const AParams: TCreateParams): HWND;
var
  View: TCocoaOpenGLView;
  Attrs: NSOpenGLPixelFormatAttributePtr;
  PixFmt: NSOpenGLPixelFormat;
  p: NSView;
  ns: NSRect;
  aNSOpenGLContext: NSOpenGLContext;
begin
  Result:=0;

  p := nil;
  if (AParams.WndParent <> 0) then
    p := CocoaUtils.GetNSObjectView(NSObject(AParams.WndParent));

  if Assigned(p) then
    LCLToNSRect(types.Bounds(AParams.X, AParams.Y, AParams.Width, AParams.Height),
      p.frame.size.height, ns)
  else
    ns := GetNSRect(AParams.X, AParams.Y, AParams.Width, AParams.Height);

  Attrs:=CreateOpenGLContextAttrList(DoubleBuffered,MajorVersion,MinorVersion,
    MultiSampling,AlphaBits,DepthBits,StencilBits,AUXBuffers);
  try
    PixFmt:=NSOpenGLPixelFormat(NSOpenGLPixelFormat.alloc).initWithAttributes(Attrs);
    aNSOpenGLContext:=NSOpenGLContext(NSOpenGLContext.alloc).initWithFormat_shareContext(PixFmt,nil);
    debugln(['LOpenGLCreateContext ',aNSOpenGLContext<>nil]);
    //View := TCocoaOpenGLView(TCocoaOpenGLView.alloc).initWithFrame_pixelFormat(ns,PixFmt);
    View := TCocoaOpenGLView(TCocoaOpenGLView.alloc).initWithFrame(ns);
    //View := TCocoaOpenGLView(TCocoaOpenGLView(TCocoaOpenGLView.alloc).lclInitWithCreateParams(AParams));
    if not Assigned(View) then Exit;
  finally
    FreeMem(Attrs);
  end;

  View.setHidden(AParams.Style and WS_VISIBLE = 0);

  if Assigned(p) then
    p.addSubview(View);
  SetViewDefaults(View);
  View.Owner:=AWinControl;
  View.callback:=TLCLCommonCallback.Create(View, AWinControl);

    //View.setPixelFormat(PixFmt);

  Result:=TLCLIntfHandle(View);
end;

procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);
begin
  // no special needed, simply release handle
  if AWinControl=nil then
    raise Exception.Create('');
end;

function CreateOpenGLContextAttrList(DoubleBuffered: boolean; MajorVersion,
  MinorVersion: Cardinal; MultiSampling, AlphaBits, DepthBits, StencilBits,
  AUXBuffers: cardinal): NSOpenGLPixelFormatAttributePtr;
var
  p: integer;

  procedure Add(i: NSOpenGLPixelFormatAttribute);
  begin
    if Result<>nil then
      Result[p]:=i;
    inc(p);
  end;

  procedure CreateList;
  begin
    //Add(NSOpenGLPFAProfile);    Add(NSOpenGLProfileLegacy);
    //Add(NSOpenGLPFAColorSize);  Add(24);
    //Add(NSOpenGLPFADepthSize);  Add(16);
    Add(NSOpenGLPFADoubleBuffer);
    Add(0);
    exit;

    if DoubleBuffered then
      Add(NSOpenGLPFADoubleBuffer);
    //if (MajorVersion>=3) and (MinorVersion>=2) then
    //   Add(NSOpenGLPFA);
    Add(NSOpenGLPFANoRecovery);
    Add(NSOpenGLPFAMaximumPolicy);
    Add(NSOpenGLPFASingleRenderer);
    Add(NSOpenGLPFAColorSize);  Add(24);
    if AlphaBits>0 then
    begin
      Add(NSOpenGLPFAAlphaSize);  Add(AlphaBits);
    end;
    if DepthBits>0 then
    begin
      Add(NSOpenGLPFADepthSize);  Add(DepthBits);
    end;
    if StencilBits>0 then
    begin
      Add(NSOpenGLPFAStencilSize);  Add(StencilBits);
    end;
    if AUXBuffers>0 then
    begin
      Add(NSOpenGLPFAAuxBuffers);  Add(AUXBuffers);
    end;
    if MultiSampling > 1 then
    begin
      Add(NSOpenGLPFAMultisample);
      Add(NSOpenGLPFASampleBuffers); Add(1);
      Add(NSOpenGLPFASamples); Add(MultiSampling);
    end;

    Add(0); // end of list
  end;

begin
  Result:=nil;
  p:=0;
  CreateList;
  GetMem(Result,SizeOf(NSOpenGLPixelFormatAttribute)*p);
  p:=0;
  CreateList;
end;

function GetCGLContextObj(OpenGLControlHandle: HWND): CGLContextObj;
var
  View: NSOpenGLView;
begin
  Result:=nil;
  if OpenGLControlHandle=0 then exit;
  View:=TCocoaOpenGLView(OpenGLControlHandle);
  Result:=CGLContextObj(View.openGLContext.CGLContextObj);
end;

function CreateCGLContextAttrList(DoubleBuffered: boolean; MultiSampling,
  AlphaBits, DepthBits, StencilBits, AUXBuffers: cardinal): PInteger;
var
  p: integer;

  procedure Add(i: integer);
  begin
    if Result<>nil then
      Result[p]:=i;
    inc(p);
  end;

  procedure CreateList;
  begin
    //Add(kCGLPFAWindow); deprecated since 10.9
    Add(kCGLPFAAccelerated);
    if DoubleBuffered then
      Add(kCGLPFADoubleBuffer);
    //if (MajorVersion>=3) and (MinorVersion>=2) then
    //   Add(kCGLOGLPVersion);
    Add(kCGLPFANoRecovery);
    Add(kCGLPFAMaximumPolicy);
    Add(kCGLPFASingleRenderer);
    if AlphaBits>0 then
    begin
      Add(kCGLPFAAlphaSize);  Add(AlphaBits);
    end;
    if DepthBits>0 then
    begin
      Add(kCGLPFADepthSize);  Add(DepthBits);
    end;
    if StencilBits>0 then
    begin
      Add(kCGLPFAStencilSize);  Add(StencilBits);
    end;
    if AUXBuffers>0 then
    begin
      //Add(kCGLPFAAuxBuffers);  Add(AUXBuffers); ToDo
    end;
    if MultiSampling > 1 then
    begin
      Add(kCGLPFASampleBuffers); Add(1);
      Add(kCGLPFASamples); Add(MultiSampling);
    end;

    Add(0); // end of list
  end;

begin
  Result:=nil;
  p:=0;
  CreateList;
  GetMem(Result,SizeOf(integer)*p);
  p:=0;
  CreateList;
end;

function IsCGLPixelFormatAvailable(Attribs: PInteger): boolean;
var
  //display: CGDirectDisplayID;
  aPixFormatObj: CGLPixelFormatObj;
  aPixObjCountAttrList: GLint;
begin
  //display := CGMainDisplayID();
  if CGLChoosePixelFormat(Attribs, @aPixFormatObj, @aPixObjCountAttrList)<>kCGLNoError
  then
    exit(false);
  if aPixFormatObj=nil then
    exit(false);
  Result:=true;
  // ToDo: free aPixFormatObj
end;

{ TCocoaOpenGLView }

function TCocoaOpenGLView.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaOpenGLView.becomeFirstResponder: Boolean;
begin
  Result:=inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaOpenGLView.resignFirstResponder: Boolean;
begin
  Result:=inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

procedure TCocoaOpenGLView.dealloc;
begin
  inherited dealloc;
end;

function TCocoaOpenGLView.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaOpenGLView.lclClearCallback;
begin
  callback := nil;
end;

function TCocoaOpenGLView.lclIsHandle: Boolean;
begin
  Result:=true;
end;

procedure TCocoaOpenGLView.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseDown(event);
end;

procedure TCocoaOpenGLView.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaOpenGLView.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaOpenGLView.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaOpenGLView.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaOpenGLView.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaOpenGLView.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaOpenGLView.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaOpenGLView.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaOpenGLView.mouseMoved(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaOpenGLView.scrollWheel(event: NSEvent);
begin
  if not Assigned(callback) or not callback.scrollWheel(event) then
    inherited scrollWheel(event);
end;

procedure TCocoaOpenGLView.keyDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.KeyEvent(event) then
    inherited keyDown(event);
end;

procedure TCocoaOpenGLView.keyUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.KeyEvent(event) then
    inherited keyUp(event);
end;

procedure TCocoaOpenGLView.flagsChanged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.KeyEvent(event) then
    inherited flagsChanged(event);
end;

procedure TCocoaOpenGLView.resetCursorRects;
begin
  if not Assigned(callback) or not callback.resetCursorRects then
    inherited resetCursorRects;
end;

procedure TCocoaOpenGLView.drawRect(dirtyRect: NSRect);
begin
  inherited drawRect(dirtyRect);
  if CheckMainThread and Assigned(callback) then
    callback.Draw(NSGraphicsContext.currentContext, bounds, dirtyRect);
end;

end.

