{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Provides methods and types to create gtkwidget with an opengl context.
    Works very similar to gtkglarea, but completely in FreePascal, so no libs
    needed.
}
unit OpenGLGtkWidget;

{$mode objfpc}{$H+}

interface

uses SysUtils, X, XUtil, XLib, glib, gdk, gtk, gl, nvGLX;

// gdkgl

const
// enum _GDK_GL_CONFIGS
  GDK_GL_NONE                           = 0;
  GDK_GL_USE_GL                         = 1;
  GDK_GL_BUFFER_SIZE                    = 2;
  GDK_GL_LEVEL                          = 3;
  GDK_GL_RGBA                           = 4;
  GDK_GL_DOUBLEBUFFER                   = 5;
  GDK_GL_STEREO                         = 6;
  GDK_GL_AUX_BUFFERS                    = 7;
  GDK_GL_RED_SIZE                       = 8;
  GDK_GL_GREEN_SIZE                     = 9;
  GDK_GL_BLUE_SIZE                      = 10;
  GDK_GL_ALPHA_SIZE                     = 11;
  GDK_GL_DEPTH_SIZE                     = 12;
  GDK_GL_STENCIL_SIZE                   = 13;
  GDK_GL_ACCUM_RED_SIZE                 = 14;
  GDK_GL_ACCUM_GREEN_SIZE               = 15;
  GDK_GL_ACCUM_BLUE_SIZE                = 16;
  GDK_GL_ACCUM_ALPHA_SIZE               = 17;

  // GLX_EXT_visual_info extension
  GDK_GL_X_VISUAL_TYPE_EXT              = $22;
  GDK_GL_TRANSPARENT_TYPE_EXT           = $23;
  GDK_GL_TRANSPARENT_INDEX_VALUE_EXT    = $24;
  GDK_GL_TRANSPARENT_RED_VALUE_EXT      = $25;
  GDK_GL_TRANSPARENT_GREEN_VALUE_EXT    = $26;
  GDK_GL_TRANSPARENT_BLUE_VALUE_EXT     = $27;
  GDK_GL_TRANSPARENT_ALPHA_VALUE_EXT    = $28;


type
  TGdkGLContext = record end;
  PGdkGLContext = ^TGdkGLContext;

// GLX_EXT_visual_info extension

function gdk_gl_query: boolean; cdecl;
function gdk_gl_get_info: string; cdecl;
function gdk_gl_choose_visual(attrlist: Plongint): PGdkVisual; cdecl;
function gdk_gl_get_config(visual: PGdkVisual; attrib: longint):longint; cdecl;
function gdk_gl_context_new(visual: PGdkVisual): PGdkGLContext; cdecl;
function gdk_gl_context_share_new(visual: PGdkVisual; sharelist: PGdkGLContext;
                                  direct: Integer): PGdkGLContext; cdecl;
function gdk_gl_context_attrlist_share_new(attrlist: Plongint;
               sharelist: PGdkGLContext; direct: Integer): PGdkGLContext; cdecl;
function gdk_gl_context_ref(context: PGdkGLContext): PGdkGLContext; cdecl;
procedure gdk_gl_context_unref(context:PGdkGLContext); cdecl;
function gdk_gl_make_current(drawable: PGdkDrawable;
                             context: PGdkGLContext): boolean; cdecl;
procedure gdk_gl_swap_buffers(drawable: PGdkDrawable); cdecl;
procedure gdk_gl_wait_gdk; cdecl;
procedure gdk_gl_wait_gl; cdecl;

{ glpixmap stuff  }

type
  TGdkGLPixmap = record end;
  PGdkGLPixmap = ^TGdkGLPixmap;


//function gdk_gl_pixmap_new(visual:PGdkVisual; pixmap:PGdkPixmap):PGdkGLPixmap; cdecl;external;
//function gdk_gl_pixmap_ref(glpixmap:PGdkGLPixmap):PGdkGLPixmap; cdecl;external;
//procedure gdk_gl_pixmap_unref(glpixmap:PGdkGLPixmap); cdecl;external;
//function gdk_gl_pixmap_make_current(glpixmap:PGdkGLPixmap; context:PGdkGLContext):Integer; cdecl;external;

{ fonts  }

//procedure gdk_gl_use_gdk_font(font:PGdkFont; first:longint; count:longint; list_base:longint); cdecl;external;

// gtkglarea

type
  TGtkGlAreaMakeCurrentType = boolean;

  PGtkGLArea = ^TGtkGLArea;
  TGtkGLArea = record
    darea: TGtkDrawingArea;
    glcontext: PGdkGLContext;
  end;

  PGtkGLAreaClass = ^TGtkGLAreaClass;
  TGtkGLAreaClass = record
    parent_class: TGtkDrawingAreaClass;
  end;

function GTK_TYPE_GL_AREA: TGtkType; cdecl;
function GTK_GL_AREA(obj: Pointer): PGtkGLArea; cdecl;
function GTK_GL_AREA_CLASS(klass: Pointer): PGtkGLAreaClass; cdecl;
function GTK_IS_GL_AREA(obj: Pointer): Boolean;
function GTK_IS_GL_AREA_CLASS(klass: Pointer): Boolean;

function gtk_gl_area_get_type: TGtkType; cdecl;
function gtk_gl_area_new(attrList: Plongint): PGtkWidget; cdecl;
function gtk_gl_area_share_new(attrList: Plongint; share: PGtkGLArea): PGtkWidget; cdecl;
//function gtk_gl_area_new_vargs(share:PGtkGLArea; args:array of const):PGtkWidget; cdecl;external;
//function gtk_gl_area_new_vargs(share:PGtkGLArea):PGtkWidget; cdecl;external;
function gtk_gl_area_make_current(glarea: PGtkGLArea): boolean; cdecl;
function gtk_gl_area_begingl(glarea: PGtkGLArea): boolean; cdecl;
procedure gtk_gl_area_swap_buffers(gl_area: PGtkGLArea);


implementation

var
  gl_area_type: TGtkType = 0;
  parent_class: Pointer = nil;
  SharedArea: PGtkGLArea = nil;

type
  TGdkGLContextPrivate = record
    xdisplay: PDisplay;
    glxcontext: TGLXContext;
    ref_count: guint;
  end;
  PGdkGLContextPrivate = ^TGdkGLContextPrivate;

procedure g_return_if_fail(b: boolean; const Msg: string);
begin
  if not b then raise Exception.Create(Msg);
end;

procedure g_return_if_fail(b: boolean);
begin
  g_return_if_fail(b,'');
end;

function DefaultScreen(ADisplay: PDisplay): longint;
begin
  if ADisplay=nil then ;
  Result:=gdk_screen;
end;

function g_new(BaseSize, Count: integer): Pointer;
begin
  Result:=g_malloc(BaseSize*Count);
end;

function get_xvisualinfo(visual: PGdkVisualPrivate): PXVisualInfo;
// IMPORTANT: remember to XFree returned XVisualInfo !!!
var
  vinfo_template: TXVisualInfo;
  dpy: PDisplay;
  nitems_return: integer;
  vi: PXVisualInfo;
begin
  dpy := GDK_DISPLAY;

  // 'GLX uses VisualInfo records because they uniquely identify
  // a (VisualID,screen,depth) tuple.'
  vinfo_template.visual   := GDK_VISUAL_XVISUAL(visual);
  vinfo_template.visualid := XVisualIDFromVisual(vinfo_template.visual);
  vinfo_template.depth    := visual^.visual.depth;
  vinfo_template.screen   := DefaultScreen(dpy);
  vi := XGetVisualInfo(dpy, VisualIDMask or VisualDepthMask or VisualScreenMask,
                       @vinfo_template, @nitems_return);
  // visualinfo needs to be unique
  if (vi=nil) then raise Exception.Create('');
  if (nitems_return<>1) then raise Exception.Create('');

  Result:=vi;
end;

procedure gtk_gl_area_destroy(obj: PGtkObject); cdecl;
var
  gl_area: PGtkGLArea;
begin
  g_return_if_fail (obj <>nil,'');
  g_return_if_fail (GTK_IS_GL_AREA(obj),'');

  gl_area := GTK_GL_AREA(obj);
  gdk_gl_context_unref(gl_area^.glcontext);

  if Assigned(GTK_OBJECT_CLASS(parent_class)^.destroy) then
    GTK_OBJECT_CLASS(parent_class)^.destroy(obj);
end;

procedure gtk_gl_area_class_init(klass: Pointer); cdecl;
var
  object_class: PGtkObjectClass;
begin
  parent_class := gtk_type_class(gtk_drawing_area_get_type());
  g_return_if_fail(parent_class<>nil,'gtk_gl_area_class_init parent_class=nil');
  object_class := PGtkObjectClass(klass);
  g_return_if_fail(object_class<>nil,'gtk_gl_area_class_init object_class=nil');

  object_class^.destroy := @gtk_gl_area_destroy;
end;

function gdk_gl_query: boolean; cdecl;
begin
  Result:=boolean(glXQueryExtension(GDK_DISPLAY,nil,nil)=true);
end;

function gdk_gl_get_info: string; cdecl;
begin
  Result:='Vendor: '+glXGetClientString(GDK_DISPLAY, GLX_VENDOR)+LineEnding
          +'Version: '+glXGetClientString(GDK_DISPLAY, GLX_VERSION)+LineEnding
          +'Extensions: '+glXGetClientString(GDK_DISPLAY, GLX_EXTENSIONS)+LineEnding;
end;

function gdk_gl_choose_visual(attrlist: Plongint): PGdkVisual; cdecl;
var
  dpy: PDisplay;
  vi: PXVisualInfo;
  visual: PGdkVisual;
begin
  //writeln('gdk_gl_choose_visual A ');
  if attrList=nil then begin
    Result:=nil;
    exit;
  end;

  //writeln('gdk_gl_choose_visual B ');
  dpy := GDK_DISPLAY;
  vi := glXChooseVisual(dpy,DefaultScreen(dpy), attrlist);
  if (vi=nil) then begin
    Result:=nil;
    exit;
  end;

  //writeln('gdk_gl_choose_visual C ');
  visual := gdkx_visual_get(vi^.visualid);
  XFree(vi);
  Result:=visual;
end;

function gdk_gl_get_config(visual: PGdkVisual; attrib: longint): longint; cdecl;
var
  dpy: PDisplay;
  vi: PXVisualInfo;
  value: integer;
begin
  Result:=-1;
  if visual=nil then exit;

  dpy := GDK_DISPLAY;
  
  vi := get_xvisualinfo(PGdkVisualPrivate(visual));

  if (glXGetConfig(dpy, vi, attrib, @value) = 0) then begin
    XFree(vi);
    Result:=value;
  end else
    XFree(vi);
end;

function gdk_gl_context_new(visual: PGdkVisual): PGdkGLContext; cdecl;
begin
  Result:=gdk_gl_context_share_new(visual,nil,gtk_FALSE);
end;

function gdk_gl_context_share_new(visual: PGdkVisual; sharelist: PGdkGLContext;
  direct: integer): PGdkGLContext; cdecl;
var
  dpy: PDisplay;
  vi: PXVisualInfo;
  PrivateShareList: PGdkGLContextPrivate;
  PrivateContext: PGdkGLContextPrivate;
  glxcontext: TGLXContext;
begin
  Result:=nil;
  if visual=nil then exit;

  dpy := GDK_DISPLAY;

  vi := get_xvisualinfo(PGdkVisualPrivate(visual));

  PrivateShareList:=PGdkGLContextPrivate(sharelist);
  if (sharelist<>nil) then
    glxcontext := glXCreateContext(dpy, vi, PrivateShareList^.glxcontext,
                                   direct=gtk_True)
  else
    glxcontext := glXCreateContext(dpy, vi, nil, direct=gtk_True);

  XFree(vi);
  if (glxcontext = nil) then exit;

  PrivateContext := g_new(SizeOf(TGdkGLContextPrivate), 1);
  PrivateContext^.xdisplay := dpy;
  PrivateContext^.glxcontext := glxcontext;
  PrivateContext^.ref_count := 1;

  Result := PGdkGLContext(PrivateContext);
end;

function gdk_gl_context_attrlist_share_new(attrlist: Plongint;
  sharelist: PGdkGLContext; direct: Integer): PGdkGLContext; cdecl;
var
  visual: PGdkVisual;
begin
  visual := gdk_gl_choose_visual(attrlist);
  if (visual<>nil) then
    Result:=gdk_gl_context_share_new(visual, sharelist, direct)
  else
    Result:=nil;
end;

function gdk_gl_context_ref(context: PGdkGLContext): PGdkGLContext; cdecl;
var
  PrivateContext: PGdkGLContextPrivate;
begin
  Result:=nil;
  if context=nil then exit;
  PrivateContext := PGdkGLContextPrivate(context);
  inc(PrivateContext^.ref_count);
  Result:=context;
end;

procedure gdk_gl_context_unref(context: PGdkGLContext); cdecl;
var
  PrivateContext: PGdkGLContextPrivate;
begin
  g_return_if_fail(context<>nil,'');

  PrivateContext:=PGdkGLContextPrivate(context);

  if (PrivateContext^.ref_count > 1) then
    dec(PrivateContext^.ref_count)
  else begin
    if (PrivateContext^.glxcontext = glXGetCurrentContext()) then
      glXMakeCurrent(PrivateContext^.xdisplay, None, nil);
    glXDestroyContext(PrivateContext^.xdisplay, PrivateContext^.glxcontext);
    g_free(PrivateContext);
  end;
end;

function gdk_gl_make_current(drawable: PGdkDrawable;
  context: PGdkGLContext): boolean; cdecl;
var
  PrivateContext: PGdkGLContextPrivate;
begin
  Result:=false;
  if  drawable=nil then exit;
  if context=nil then exit;
  PrivateContext := PGdkGLContextPrivate(context);

  Result:=boolean(glXMakeCurrent(PrivateContext^.xdisplay,
                         GDK_WINDOW_XWINDOW(PGdkWindowPrivate(drawable)),
                         PrivateContext^.glxcontext)=true);
end;

procedure gdk_gl_swap_buffers(drawable: PGdkDrawable); cdecl;
begin
  g_return_if_fail(drawable <> nil);

  glXSwapBuffers(GDK_WINDOW_XDISPLAY(PGdkWindowPrivate(drawable)),
                 GDK_WINDOW_XWINDOW(PGdkWindowPrivate(drawable)));
end;

procedure gdk_gl_wait_gdk; cdecl;
begin
  glXWaitX;
end;

procedure gdk_gl_wait_gl; cdecl;
begin
  glXWaitGL;
end;

procedure gtk_gl_area_init(gl_area, theClass: Pointer); cdecl;
begin
  if theClass=nil then ;
  PGtkGLArea(gl_area)^.glcontext:=nil;
  //#if GTK_CHECK_VERSION (1, 3, 1)
  //gtk_widget_set_double_buffered(PGtkWidget(gl_area),gdkFALSE);
end;

function GTK_TYPE_GL_AREA: TGtkType; cdecl;
const
  gl_area_type_name = 'GtkGLArea';
  gl_area_info: TGtkTypeInfo = (
    type_name: gl_area_type_name;
    object_size: SizeOf(TGtkGLArea);
    class_size:  SizeOf(TGtkGLAreaClass);
    class_init_func:  @gtk_gl_area_class_init;
    object_init_func: @gtk_gl_area_init;
    reserved_1: nil;
    reserved_2: nil;
    base_class_init_func: nil;
  );
begin
  if (gl_area_type=0) then begin
    gl_area_type:=gtk_type_unique(gtk_drawing_area_get_type(),@gl_area_info);
  end;
  Result:=gl_area_type;
end;

function GTK_GL_AREA(obj: Pointer): PGtkGLArea; cdecl;
begin
  g_return_if_fail(GTK_IS_GL_AREA(obj),'');
  Result:=PGtkGLArea(obj);
end;

function GTK_GL_AREA_CLASS(klass: Pointer): PGtkGLAreaClass; cdecl;
begin
  g_return_if_fail(GTK_IS_GL_AREA_CLASS(klass),'');
  Result:=PGtkGLAreaClass(klass);
end;

function GTK_IS_GL_AREA(obj: Pointer): Boolean;
begin
  // GTK_IS_GL_AREA:=GTK_CHECK_TYPE(obj,GTK_TYPE_GL_AREA);
  Result := Assigned(obj) and GTK_IS_GL_AREA_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GTK_IS_GL_AREA_CLASS(klass: Pointer): Boolean;
begin
  // GTK_IS_GL_AREA_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_GL_AREA);
  Result := Assigned(klass) and (PGtkTypeClass(klass)^.thetype = GTK_TYPE_GL_AREA);
end;

function gtk_gl_area_get_type: TGtkType; cdecl;
begin
  Result:=GTK_TYPE_GL_AREA;
end;

function gtk_gl_area_new(attrList: Plongint): PGtkWidget; cdecl;
var
  Count: Integer;
  CopyAttrList: Plongint;
  Size: Integer;
begin
  Count:=0;
  while (attrList[Count]<>GDK_GL_NONE) do inc(Count);
  inc(Count);
  Size:=SizeOf(Integer)*Count;
  GetMem(CopyAttrList,Size);
  System.Move(attrList^,CopyAttrList^,Size);
  Result:=gtk_gl_area_share_new(CopyAttrList,SharedArea);
  FreeMem(CopyAttrList);
end;

function gtk_gl_area_share_new(attrList: Plongint; share: PGtkGLArea
  ): PGtkWidget; cdecl;
var
  visual: PGdkVisual;
  sharelist: PGdkGLContext;
  glcontext: PGdkGLContext;
  gl_area: PGtkGLArea;
begin
  Result:=nil;
  //writeln('gtk_gl_area_share_new A ');
  if (share<>nil) and (not GTK_IS_GL_AREA(share)) then
    exit;
  {$IFNDEF win32}
  //writeln('gtk_gl_area_share_new B ');
  visual := gdk_gl_choose_visual(attrlist);
  if (visual = nil) then exit;
  {$ENDIF non win32}

  //writeln('gtk_gl_area_share_new C ');
  sharelist := nil;
  if share<>nil then sharelist:=share^.glcontext;
  glcontext := gdk_gl_context_share_new(visual, sharelist, gtk_TRUE);
  if (glcontext = nil) then exit;
  //writeln('gtk_gl_area_share_new D ');

  {$IFNDEF win32}
  // use colormap and visual suitable for OpenGL rendering
  gtk_widget_push_colormap(gdk_colormap_new(visual,gtk_TRUE));
  gtk_widget_push_visual(visual);
  {$ENDIF non win32}

  gl_area := gtk_type_new (gtk_gl_area_get_type);
  gl_area^.glcontext := glcontext;
  //writeln('gtk_gl_area_share_new E ',gl_area<>nil);

  {$IFNDEF win32}
  // pop back defaults
  gtk_widget_pop_visual;
  gtk_widget_pop_colormap;
  {$ENDIF non win32}
  Result:=PGtkWidget(gl_area);
end;

function gtk_gl_area_make_current(glarea: PGtkGLArea): boolean; cdecl;
begin
  Result:=false;
  if glarea=nil then exit;
  if not GTK_IS_GL_AREA(glarea) then exit;
  if not GTK_WIDGET_REALIZED(PGtkWidget(glarea)) then exit;

  Result:=gdk_gl_make_current(PGtkWidget(glarea)^.window, glarea^.glcontext);
end;

function gtk_gl_area_begingl(glarea: PGtkGLArea): boolean; cdecl;
begin
  Result:=gtk_gl_area_make_current(glarea);
end;

procedure gtk_gl_area_swap_buffers(gl_area: PGtkGLArea);
begin
  g_return_if_fail(gl_area <> nil);
  g_return_if_fail(GTK_IS_GL_AREA(gl_area));
  g_return_if_fail(GTK_WIDGET_REALIZED(PGtkWidget(gl_area)));

  gdk_gl_swap_buffers(GTK_WIDGET(gl_area)^.window);
end;

end.

