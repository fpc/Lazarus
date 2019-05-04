{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  ToDo:
    use custom pixelformat
      attributes: doublebufferd, version, ...
      It should work with initWithFrame_pixelFormat, but this paints nothing
    SwapBuffers - there is no function like aglSwapBuffers in CGL/NS
    Mouse:
      the TLCLCommonCallback mouse handlers check Owner.isEnabled, which
      for a NSView always returns false.
    SharedControl
}
unit GLCocoaNSContext;

{$mode objfpc}{$H+}
{$ModeSwitch objectivec1}

interface

uses
  Classes, SysUtils, types, CocoaWSCommon, CocoaPrivate, CocoaUtils, LCLType,
  LMessages, LCLMessageGlue,
  Controls, LazLoggerBase, WSLCLClasses, gl, MacOSAll, CocoaAll;

function LBackingScaleFactor(Handle: HWND): single;
procedure LSetWantsBestResolutionOpenGLSurface(const AValue: boolean; Handle: HWND);
procedure LOpenGLViewport(Handle: HWND; Left, Top, Width, Height: integer);
procedure LOpenGLSwapBuffers(Handle: HWND);
function LOpenGLMakeCurrent(Handle: HWND): boolean;
function LOpenGLReleaseContext(Handle: HWND): boolean;
procedure LOpenGLClip(Handle: HWND);
function LOpenGLCreateContext(AWinControl: TWinControl;
              {%H-}WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
              DoubleBuffered, AMacRetinaMode: boolean;
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
  NSOpenGLPFAOpenGLProfile            = 99; //cr: name changed to match https://developer.apple.com/library/mac/documentation//Cocoa/Reference/ApplicationKit/Classes/NSOpenGLPixelFormat_Class/index.html
  NSOpenGLProfileLegacy         = $1000;
  NSOpenGLProfileVersion3_2Core = $3200;
  NSOpenGLProfileVersion4_1Core = $4100; //requires OSX SDK 10.10 or later, https://github.com/google/gxui/issues/98

type
  NSOpenGLViewFix = objccategory external (NSOpenGLView)
    procedure setWantsBestResolutionOpenGLSurface(bool: NSInteger); message 'setWantsBestResolutionOpenGLSurface:';
  end;
  NSScreenFix = objccategory external (NSScreen)
     function backingScaleFactor: CGFloat ; message 'backingScaleFactor';
  end;
  TDummyNoWarnObjCNotUsed = objc.BOOL;
  TDummyNoWarnObjCBaseNotUsed = objcbase.NSInteger;

  { TCocoaOpenGLView }

  TCocoaOpenGLView = objcclass(NSOpenGLView)
  public
    Owner: TWinControl;
    //nsGL: NSOpenGLContext;
    callback: TLCLCommonCallback;
    backingScaleFactor: Single;
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function becomeFirstResponder: LCLObjCBoolean; override;
    function resignFirstResponder: LCLObjCBoolean; override;
    procedure drawRect(dirtyRect: NSRect); override;
    procedure dealloc; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function lclIsEnabled: Boolean; override;
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure rightMouseDragged(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure otherMouseDragged(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;
    // other
    procedure resetCursorRects; override;
  end;

function GetCGLContextObj(OpenGLControlHandle: HWND): CGLContextObj;
(*function CreateCGLContextAttrList(DoubleBuffered: boolean;
              {$IFDEF UsesModernGL}
              MajorVersion, MinorVersion: Cardinal;
              {$ENDIF}
              MultiSampling, AlphaBits, DepthBits,
              StencilBits, AUXBuffers: cardinal): PInteger;
function IsCGLPixelFormatAvailable(Attribs: PInteger): boolean;*)

implementation

//value > 1 if screen is scaled, e.g. default for MOST retina displays is 2
function LBackingScaleFactor(Handle: HWND): single;
begin
  result := TCocoaOpenGLView(Handle).backingScaleFactor;
end;

procedure LSetWantsBestResolutionOpenGLSurface(const AValue: boolean; Handle: HWND);
var
  View: TCocoaOpenGLView;
begin
  if Handle=0 then exit;
  View:=TCocoaOpenGLView(Handle);
  if not View.respondsToSelector(objcselector('setWantsBestResolutionOpenGLSurface:')) then exit;
  if AValue then
    View.setWantsBestResolutionOpenGLSurface(1)
  else
    View.setWantsBestResolutionOpenGLSurface(0);
  if (AValue) and (NSScreen.mainScreen.respondsToSelector(objcselector('backingScaleFactor'))) then //MacOS >=10.7
    View.backingScaleFactor := NSScreen.mainScreen.backingScaleFactor
  else
    View.backingScaleFactor := 1;
end;

procedure LOpenGLViewport(Handle: HWND; Left, Top, Width, Height: integer);
var
  View: NSOpenGLView absolute Handle;
  lFinalWidth, lFinalHeight: Integer;
begin
  lFinalWidth := Width;
  lFinalHeight := Height;
  if View <> nil then
  begin
    lFinalWidth := Round(Width * LBackingScaleFactor(Handle));
    lFinalHeight := Round(Height * LBackingScaleFactor(Handle));
  end;
  glViewport(Left,Top,lFinalWidth,lFinalHeight);
end;

procedure LOpenGLSwapBuffers(Handle: HWND);
//var
//  View: TCocoaOpenGLView;  //TCocoaOpenGLView
begin
  if Handle=0 then exit;
  glFlush();
  // View:=TCocoaOpenGLView(Handle);
  // View.nsGL.flushBuffer;
end;

function LOpenGLMakeCurrent(Handle: HWND): boolean;
var
  CGLContext: CGLContextObj;
begin
  if Handle=0 then exit(false);
  CGLContext:=GetCGLContextObj(Handle);
  Result:=CGLSetCurrentContext(CGLContext)=kCGLNoError;
end;

function LOpenGLReleaseContext(Handle: HWND): boolean;
begin
  if Handle=0 then exit(false);
  Result:=CGLSetCurrentContext(nil)=kCGLNoError;
  //Result:=true;
end;

procedure LOpenGLClip(Handle: HWND);
begin
  if Handle=0 then exit;
  // ToDo
end;

function LOpenGLCreateContext(AWinControl: TWinControl;
  WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
  DoubleBuffered, AMacRetinaMode: boolean;
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
  CGLContext: CGLContextObj;
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
  Attrs:=CreateOpenGLContextAttrList(DoubleBuffered,MajorVersion,MinorVersion, MultiSampling,AlphaBits,DepthBits,StencilBits,AUXBuffers);
  try
    PixFmt:=NSOpenGLPixelFormat(NSOpenGLPixelFormat.alloc).initWithAttributes(Attrs);
    aNSOpenGLContext:=NSOpenGLContext(NSOpenGLContext.alloc).initWithFormat_shareContext(PixFmt,nil);
    if aNSOpenGLContext  = nil then
    	debugln(['LOpenGLCreateContext Error']);
    View := TCocoaOpenGLView(TCocoaOpenGLView.alloc).initWithFrame_pixelFormat(ns,PixFmt);
    if not Assigned(View) then Exit;
  finally
    FreeMem(Attrs);
  end;
  View.setHidden(AParams.Style and WS_VISIBLE = 0);
  if Assigned(p) then
    p.addSubview(View);
  SetViewDefaults(View);
  View.Owner:=AWinControl;
  //View.nsGL := aNSOpenGLContext;
  View.callback:=TLCLCommonCallback.Create(View, AWinControl);
  LSetWantsBestResolutionOpenGLSurface(AMacRetinaMode, HWND(View));
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
procedure AddUInt32(i: NSOpenGLPixelFormatAttribute);
  begin
    if Result<>nil then
      Result[p]:=i;
    inc(p);
  end;

  procedure CreateList;
  begin
    //see https://developer.apple.com/library/mac/documentation//Cocoa/Reference/ApplicationKit/Classes/NSOpenGLPixelFormat_Class/index.html
    //AddUInt32(NSOpenGLPFAAccelerated);  // <- comment out: we can run in software if hardware is not available
    //AddUInt32(NSOpenGLPFAOpenGLProfile); //Versions beyond 'Legacy' appear to break CULL_FACE and DEPTH_BUFFER, legacy seems to be default, so comment out whole instruction
    //if (MajorVersion>=4) and (MinorVersion>=1)
    //  AddUInt32(NSOpenGLProfileVersion4_1Core);
    //else if (MajorVersion>=3) and (MinorVersion>=2) then
    //     AddUInt32(NSOpenGLProfileVersion3_2Core);
    //else
    //AddUInt32(NSOpenGLProfileLegacy); // NSOpenGLProfileLegacy is default and sufficient, later versions depend on SDK we are building against
    AddUInt32(NSOpenGLPFAOpenGLProfile);
    if (MajorVersion>=4) and (MinorVersion>=1) then
       AddUInt32(NSOpenGLProfileVersion4_1Core) //OpenGL 4.1, GLSL 4.1
    else if (MajorVersion>=3) and (MinorVersion>=2) then
        AddUInt32(NSOpenGLProfileVersion3_2Core)
    else
        AddUInt32(NSOpenGLProfileLegacy); //OpenGL 2.1, GLSL 1.2
    AddUInt32(NSOpenGLPFAColorSize); AddUInt32(24);
    if DepthBits > 0 then begin
       AddUInt32(NSOpenGLPFADepthSize);  AddUInt32(32);
    end;
    if AlphaBits>0 then begin
      AddUInt32(NSOpenGLPFAAlphaSize);  AddUInt32(AlphaBits);
    end;
    AddUInt32(NSOpenGLPFAAccelerated);
    if MultiSampling > 1 then begin
      AddUInt32(NSOpenGLPFAMultisample);
      AddUInt32(NSOpenGLPFASampleBuffers); AddUInt32(1);
      AddUInt32(NSOpenGLPFASamples); AddUInt32(MultiSampling);
    end;
    if StencilBits>0 then
    begin
      AddUInt32(NSOpenGLPFAStencilSize);  AddUInt32(StencilBits);
    end;
    if AUXBuffers>0 then
    begin
      AddUInt32(NSOpenGLPFAAuxBuffers);  AddUInt32(AUXBuffers);
    end;
    //if DoubleBuffered then //requires fix for nsGL
    //   AddUInt32(NSOpenGLPFADoubleBuffer); //this doen't work with Lazarus
    AddUInt32(NSOpenGLPFAMaximumPolicy); //allows future changes to make attributes more demanding, e.g. add multisampling


    AddUInt32(NSOpenGLPFANoRecovery); //see apple web page: "not generally useful" but might help with multisample
    AddUInt32(0); // end of list
  end;

begin
  Result:=nil;
  p:=0;
  CreateList;
  GetMem(Result,SizeOf(NSOpenGLPixelFormatAttribute)*(p+1));
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
  NSScreen.mainScreen.colorSpace;
end;

(*
//these functions are commented out: this was an attempt to use CGL, porting NSOpenGLView instead was more successful
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
end;              *)

{ TCocoaOpenGLView }

function TCocoaOpenGLView.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := True;
end;

function TCocoaOpenGLView.becomeFirstResponder: LCLObjCBoolean;
begin
  Result:=inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaOpenGLView.resignFirstResponder: LCLObjCBoolean;
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

function TCocoaOpenGLView.lclIsEnabled: Boolean;
begin
  Result := Owner.Enabled;
end;

procedure TCocoaOpenGLView.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
  begin
    // do not pass mouseDown below or it will pass it to the parent control
    // causing double events
    //inherited mouseDown(event);
  end;
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

procedure TCocoaOpenGLView.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited rightMouseDragged(event);
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

procedure TCocoaOpenGLView.otherMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited otherMouseDragged(event);
end;

procedure TCocoaOpenGLView.mouseDragged(event: NSEvent);
begin
  if Assigned(callback)
    then callback.MouseMove(event)
    else inherited mouseDragged(event);
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
  if Assigned(callback)
    then callback.scrollWheel(event)
    else inherited scrollWheel(event);
end;

procedure TCocoaOpenGLView.resetCursorRects;
begin
  if not Assigned(callback) or not callback.resetCursorRects then
    inherited resetCursorRects;
end;

procedure TCocoaOpenGLView.drawRect(dirtyRect: NSRect);
var
  ctx : NSGraphicsContext;
  PS  : TPaintStruct;
  r   : NSRect;
begin
  ctx := NSGraphicsContext.currentContext;
  inherited drawRect(dirtyRect);
  if CheckMainThread and Assigned(callback) then
  begin
    if ctx = nil then
    begin
      // In macOS 10.14 (mojave) current context is nil
      // we still can paint anything releated to OpenGL!
      // todo: consider creating a dummy context (for a bitmap)
      FillChar(PS, SizeOf(TPaintStruct), 0);
      r := frame;
      r.origin.x:=0;
      r.origin.y:=0;
      PS.hdc := HDC(0);
      PS.rcPaint := NSRectToRect(r);
      LCLSendPaintMsg(Owner, HDC(0), @PS);
    end
    else
      callback.Draw(ctx, bounds, dirtyRect);
  end;
end;

end.

