unit GLGtk3GlxContext;

{$mode objfpc}
{$LinkLib GL}

interface

uses
  Classes, SysUtils, ctypes, LCLProc, LCLType, X, XUtil, XLib, gl, glext,
  InterfaceBase,
  glx,
  WSLCLClasses, LCLMessageGlue,
  LMessages, glib2, gtk3int, LazGdk3, LazGtk3, gtk3widgets,
  Controls;

function LBackingScaleFactor(Handle: HWND): single;
procedure LOpenGLViewport({%H-}Handle: HWND; Left, Top, Width, Height: integer);
procedure LOpenGLSwapBuffers(Handle: HWND);
function LOpenGLMakeCurrent(Handle: HWND): boolean;
function LOpenGLReleaseContext({%H-}Handle: HWND): boolean;
function LOpenGLCreateContext(AWinControl: TWinControl;
             WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
             DoubleBuffered, RGBA, DebugContext: boolean;
             const RedBits, GreenBits, BlueBits, MajorVersion, MinorVersion,
             MultiSampling, AlphaBits, DepthBits, StencilBits, AUXBuffers: Cardinal;
             const AParams: TCreateParams): HWND;
procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);

implementation

{$assertions on}

procedure on_render(widget: PGtkWidget; context: gpointer{Pcairo_t}; data: TGtk3Widget); cdecl;
begin
  data.LCLObject.Perform(LM_PAINT, WParam(data), 0);
end;

function gtkglarea_size_allocateCB(Widget: PGtkWidget; Size: pGtkAllocation; Data: gPointer): GBoolean; cdecl;
var
  SizeMsg: TLMSize;
  GtkWidth, GtkHeight: integer;
  LCLControl: TWinControl;
begin
  Result := true;
  LCLControl:=TWinControl(Data);
  if LCLControl=nil then exit;

  gtk_widget_get_size_request(Widget, @GtkWidth, @GtkHeight);

  SizeMsg.Msg:=0;
  FillChar(SizeMsg,SizeOf(SizeMsg),0);
  with SizeMsg do
  begin
    Result := 0;
    Msg := LM_SIZE;
    SizeType := Size_SourceIsInterface;
    Width := SmallInt(GtkWidth);
    Height := SmallInt(GtkHeight);
  end;
  LCLControl.WindowProc(TLMessage(SizeMsg));
end;

function gtk_gl_area_get_error (area: PGtkGLArea): PGError; cdecl; external;

function LBackingScaleFactor(Handle: HWND): single;
var
  glarea: TGtk3GLArea absolute Handle;
begin
  // todo(ryan): get the correct screen for the handle!
  result := TGdkScreen.get_default^.get_monitor_scale_factor(0);
end;

procedure LOpenGLViewport(Handle: HWND; Left, Top, Width, Height: integer);
var
  scaleFactor: integer;
begin
  scaleFactor := RoundToInt(LBackingScaleFactor(Handle));
  glViewport(Left,Top,Width*scaleFactor,Height*scaleFactor);
end;

procedure LOpenGLSwapBuffers(Handle: HWND);
var
  glarea: TGtk3GLArea absolute Handle;
begin
  if Handle=0 then exit;
  glFlush();
end;

function LOpenGLMakeCurrent(Handle: HWND): boolean;
var
  glarea: TGtk3GLArea absolute Handle;
begin
  glarea.Widget^.realize;
  PGtkGLArea(glarea.Widget)^.make_current;
  Assert(gtk_gl_area_get_error(PGtkGLArea(glarea.Widget)) = nil, 'LOpenGLMakeCurrent failed');
  result := true;
end;

function LOpenGLReleaseContext(Handle: HWND): boolean;
var
  glarea: TGtk3GLArea absolute Handle;
begin
  // todo(ryan): is it possible to make no context current?
  result:=true;
end;

function LOpenGLCreateContext(AWinControl: TWinControl;
  WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
  DoubleBuffered, RGBA, DebugContext: boolean;
  const RedBits, GreenBits, BlueBits, MajorVersion, MinorVersion,
  MultiSampling, AlphaBits, DepthBits, StencilBits, AUXBuffers: Cardinal;
  const AParams: TCreateParams): HWND;
var
  NewWidget: TGtk3GLArea;
  glarea: PGtkGLArea;
begin
  NewWidget := TGtk3GLArea.Create(AWinControl, AParams);
  result := TLCLIntfHandle(NewWidget);
  glarea := PGtkGLArea(NewWidget.Widget);

  g_signal_connect(glarea, 'render', TGCallback(@on_render), NewWidget);
  // todo(ryan): do we need this?
  g_signal_connect_after(glarea, 'size-allocate', TGCallback(@gtkglarea_size_allocateCB), AWinControl);

  glarea^.set_auto_render(false);
  glarea^.set_required_version(MajorVersion, MinorVersion);
  glarea^.set_has_depth_buffer(DepthBits > 0);
  glarea^.set_has_alpha(AlphaBits > 0);
  glarea^.set_has_stencil_buffer(StencilBits > 0);
end;

procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);
begin
  if not AWinControl.HandleAllocated then exit;
  // nothing to do
end;

end.

