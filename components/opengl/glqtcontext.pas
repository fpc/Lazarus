{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

}
unit GLQTContext;

{$mode objfpc}{$H+}
{$LinkLib GL}
{$PACKRECORDS C}

interface

uses
  Classes, SysUtils, Controls, LCLProc, LCLType, X, XUtil, XLib, gl,
  InterfaceBase,
  WSLCLClasses,glx,
  // Bindings
  {$IFDEF LCLQt}qt4,{$ENDIF}
  {$IFDEF LCLQt5}qt5,{$ENDIF}
  qtwidgets, qtobjects, qtproc, qtint,
  QtWSControls;

// gdkgl

const
// enum _QT_GL_CONFIGS
  QT_GL_NONE                           = 0;
  QT_GL_USE_GL                         = 1;
  QT_GL_BUFFER_SIZE                    = 2;
  QT_GL_LEVEL                          = 3;
  QT_GL_RGBA                           = 4;
  QT_GL_DOUBLEBUFFER                   = 5;
  QT_GL_STEREO                         = 6;
  QT_GL_AUX_BUFFERS                    = 7;
  QT_GL_RED_SIZE                       = 8;
  QT_GL_GREEN_SIZE                     = 9;
  QT_GL_BLUE_SIZE                      = 10;
  QT_GL_ALPHA_SIZE                     = 11;
  QT_GL_DEPTH_SIZE                     = 12;
  QT_GL_STENCIL_SIZE                   = 13;
  QT_GL_ACCUM_RED_SIZE                 = 14;
  QT_GL_ACCUM_GREEN_SIZE               = 15;
  QT_GL_ACCUM_BLUE_SIZE                = 16;
  QT_GL_ACCUM_ALPHA_SIZE               = 17;

  // GLX_EXT_visual_info extension
  QT_GL_X_VISUAL_TYPE_EXT              = $22;
  QT_GL_TRANSPARENT_TYPE_EXT           = $23;
  QT_GL_TRANSPARENT_INDEX_VALUE_EXT    = $24;
  QT_GL_TRANSPARENT_RED_VALUE_EXT      = $25;
  QT_GL_TRANSPARENT_GREEN_VALUE_EXT    = $26;
  QT_GL_TRANSPARENT_BLUE_VALUE_EXT     = $27;
  QT_GL_TRANSPARENT_ALPHA_VALUE_EXT    = $28;

type
  TGLXContext = pointer;

procedure LOpenGLViewport(Handle: HWND; Left, Top, Width, Height: integer);
procedure LOpenGLSwapBuffers(Handle: HWND);
function LOpenGLMakeCurrent(Handle: HWND): boolean;
function LOpenGLReleaseContext(Handle: HWND): boolean;
function LOpenGLCreateContext(AWinControl: TWinControl;
                          WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
                          DoubleBuffered, RGBA: boolean;
                          const RedBits, GreenBits, BlueBits, MajorVersion, MinorVersion,
                          MultiSampling, AlphaBits, DepthBits, StencilBits, AUXBuffers: Cardinal;
                          const AParams: TCreateParams): HWND;

procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);
function CreateOpenGLContextAttrList(DoubleBuffered: boolean; RGBA: boolean;
             const RedBits, GreenBits, BlueBits, AlphaBits, DepthBits,
             StencilBits,  AUXBuffers: Cardinal): PInteger;


implementation

function XVisualAsString(AVisual: PVisual): string;
begin
  if AVisual=nil then begin
    Result:='nil';
  end else begin
    Result:=''
        +' bits_per_rgb='+dbgs(AVisual^.bits_per_rgb)
        +' red_mask='+hexstr(AVisual^.red_mask,8)
        +' green_mask='+hexstr(AVisual^.green_mask,8)
        +' blue_mask='+hexstr(AVisual^.blue_mask,8)
        +' map_entries='+dbgs(AVisual^.map_entries)
        +'';
  end;
end;

function XDisplayAsString(ADisplay: PDisplay): string;
begin
  if ADisplay=nil then begin
    Result:='nil';
  end else begin
    Result:=''
        +'';
  end;
end;

type

  { TQtGLWidget }

  TQtGLWidget = class(TQtWidget)
  public
    xdisplay: PDisplay;
    visual: PXVisualInfo;
    glxcontext: TGLXContext;
    ref_count: integer;
    function GetGLXDrawable: GLXDrawable;
  end;

{ TQtGLWidget }

function TQtGLWidget.GetGLXDrawable: GLXDrawable;
begin
  result := QWidget_winID(Widget);
end;

procedure LOpenGLViewport(Handle: HWND; Left, Top, Width, Height: integer);
begin
  glViewport(Left,Top,Width,Height);
end;

procedure LOpenGLSwapBuffers(Handle: HWND);
var
  Widget: TQtGLWidget;
begin
  if Handle=0 then
    RaiseGDBException('LOpenGLSwapBuffers Handle=0');

  Widget:=TQtGLWidget(Handle);
  glXSwapBuffers(Widget.xdisplay,
                 Widget.GetGLXDrawable
                 );
end;

function LOpenGLMakeCurrent(Handle: HWND): boolean;
var
  Widget: TQtGLWidget;
begin
  if Handle=0 then
    RaiseGDBException('LOpenGLSwapBuffers Handle=0');
  Result:=false;

  Widget:=TQtGLWidget(Handle);
  Result:=glXMakeCurrent(Widget.xdisplay,
                                 Widget.GetGLXDrawable,
                                 Widget.glxcontext);
end;

function LOpenGLReleaseContext(Handle: HWND): boolean;
var
  Widget: TQtGLWidget;
begin
  Result := false;
  if Handle=0 then
    RaiseGDBException('LOpenGLSwapBuffers Handle=0');

  Widget:=TQtGLWidget(Handle);
  Result := glXMakeCurrent(Widget.xdisplay, 0, nil);
end;


{function LOpenGLCreateContext(AWinControl: TWinControl;
  WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
  DoubleBuffered, RGBA: boolean;
  const MultiSampling, AlphaBits, DepthBits, StencilBits: Cardinal;
  const AParams: TCreateParams): HWND;}
function LOpenGLCreateContext(AWinControl: TWinControl;
                          WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
                          DoubleBuffered, RGBA: boolean;
                          const RedBits, GreenBits, BlueBits, MajorVersion, MinorVersion,
                          MultiSampling, AlphaBits, DepthBits, StencilBits, AUXBuffers: Cardinal;
                          const AParams: TCreateParams): HWND;

var
  AttrList: PInteger;
  NewQtWidget: TQtGLWidget;
  direct: boolean;
begin
  if WSPrivate=nil then ;
  AttrList:=CreateOpenGLContextAttrList(DoubleBuffered,RGBA,RedBits,GreenBits,BlueBits,AlphaBits,DepthBits,StencilBits,AUXBuffers);
  try
    NewQtWidget:=TQtGLWidget.Create(AWinControl,AParams);
    NewQtWidget.setAttribute(QtWA_PaintOnScreen);
    NewQtWidget.setAttribute(QtWA_NoSystemBackground);
    NewQtWidget.setAttribute(QtWA_OpaquePaintEvent);
    NewQtWidget.HasPaint:=true;
    NewQtWidget.xdisplay := QX11Info_display;
    NewQtWidget.visual:=glXChooseVisual(NewQtWidget.xdisplay,
      DefaultScreen(NewQtWidget.xdisplay), @attrList[0]);
    direct:=false;
    NewQtWidget.glxcontext := glXCreateContext(NewQtWidget.xdisplay,
                                               NewQtWidget.visual, nil, direct);
    NewQtWidget.ref_count := 1;

    NewQtWidget.AttachEvents;

    Result:=HWND(PtrUInt(Pointer(NewQtWidget)));
  finally
    FreeMem(AttrList);
  end;
end;

procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);
begin
  if not AWinControl.HandleAllocated then exit;
  // nothing to do
end;

function CreateOpenGLContextAttrList(DoubleBuffered: boolean; RGBA: boolean;
  const RedBits, GreenBits, BlueBits, AlphaBits, DepthBits, StencilBits,
  AUXBuffers: Cardinal): PInteger;
var
  p: integer;
  UseFBConfig: boolean;

  procedure Add(i: integer);
  begin
    if Result<>nil then
      Result[p]:=i;
    inc(p);
  end;

  procedure CreateList;
  begin
    p:=0;
    if UseFBConfig then begin
      Add(GLX_X_RENDERABLE); Add(1);
      Add(GLX_X_VISUAL_TYPE); Add(GLX_TRUE_COLOR);
    end;
    if DoubleBuffered then
    begin
      if UseFBConfig then
        begin Add(GLX_DOUBLEBUFFER); Add(1); end else
        Add(GLX_DOUBLEBUFFER);
    end;
    if RGBA then
    begin
      if not UseFBConfig then Add(GLX_RGBA);
      { For UseFBConfig, glXChooseFBConfig already defaults to RGBA }
    end;
    Add(GLX_RED_SIZE);  Add(RedBits);
    Add(GLX_GREEN_SIZE);  Add(GreenBits);
    Add(GLX_BLUE_SIZE);  Add(BlueBits);
    if AlphaBits>0 then
    begin
      Add(GLX_ALPHA_SIZE);  Add(AlphaBits);
    end;
    if DepthBits>0 then
    begin
      Add(GLX_DEPTH_SIZE);  Add(DepthBits);
    end;
    if StencilBits>0 then
    begin
      Add(GLX_STENCIL_SIZE);  Add(StencilBits);
    end;
    if AUXBuffers>0 then
    begin
      Add(GLX_AUX_BUFFERS);  Add(AUXBuffers);
    end;

    Add(0); { 0 = X.None (be careful: GLX_NONE is something different) }
  end;

begin
  {$IFDEF VerboseMultiSampling}
  debugln(['CreateOpenGLContextAttrList MultiSampling=',MultiSampling]);
  {$ENDIF}
  UseFBConfig := false; //GLX_version_1_3(GetDefaultXDisplay);
  Result:=nil;
  CreateList;
  GetMem(Result,SizeOf(integer)*p);
  CreateList;
end;


end.

