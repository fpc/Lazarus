{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

}
unit GLGtkGlxContext;

{$mode objfpc}{$H+}
{$LinkLib GL}
{$PACKRECORDS C}

{$IFNDEF VER2_4}
  {$Define UseFPGLX}
{$ENDIF}

interface

uses
  Classes, SysUtils, LCLProc, LCLType, X, XUtil, XLib, gl, InterfaceBase,
  {$IFDEF UseFPGLX}
  glx,
  {$ELSE}
  ctypes,
  {$ENDIF}
  WSLCLClasses,
  {$IFDEF LCLGTK2}
  LMessages, Gtk2Def, gdk2x, glib2, gdk2, gtk2, Gtk2Int,
  {$ENDIF}
  {$IFDEF LCLGTK}
  glib, gdk, gtk, GtkInt,
  {$ENDIF}
  Controls;

{$IFDEF UseFPGLX}
type
  TGLBool = longbool;
const
  GLXTrue:longbool = true;
  GLXFalse:longbool = false;
{$ELSE}
type
  TGLBool = cint;
const
  GLXTrue = 1;
  GLXFalse = 0;
  
  { Constants below are the same as in FPC GLX unit. }

  // GLX 1.3 and later:
  GLX_X_RENDERABLE                = $8012;
  
  // Tokens for glXChooseVisual and glXGetConfig.
  GLX_USE_GL                            = 1;
  GLX_BUFFER_SIZE                       = 2;
  GLX_LEVEL                             = 3;
  GLX_RGBA                              = 4;
  GLX_DOUBLEBUFFER                      = 5;
  GLX_STEREO                            = 6;
  GLX_AUX_BUFFERS                       = 7;
  GLX_RED_SIZE                          = 8;
  GLX_GREEN_SIZE                        = 9;
  GLX_BLUE_SIZE                         = 10;
  GLX_ALPHA_SIZE                        = 11;
  GLX_DEPTH_SIZE                        = 12;
  GLX_STENCIL_SIZE                      = 13;
  GLX_ACCUM_RED_SIZE                    = 14;
  GLX_ACCUM_GREEN_SIZE                  = 15;
  GLX_ACCUM_BLUE_SIZE                   = 16;
  GLX_ACCUM_ALPHA_SIZE                  = 17;

  // For non conformant FBConfigs
  GLX_CONFIG_CAVEAT                     = 32;
  GLX_NON_CONFORMANT_CONFIG             = 32781;

  // GLX_ARB_multisample
  GLX_SAMPLE_BUFFERS_ARB             = 100000;
  GLX_SAMPLES_ARB                    = 100001;
{$ENDIF}

type
  TGdkGLContext = record end;
  PGdkGLContext = ^TGdkGLContext;

// GLX_EXT_visual_info extension

function gdk_gl_query: boolean;
function gdk_gl_choose_visual(attrlist: Plongint): PGdkVisual;
function gdk_gl_get_config(visual: PGdkVisual; attrib: longint): longint;
function gdk_gl_context_new(visual: PGdkVisual; attrlist: PlongInt): PGdkGLContext;
function gdk_gl_context_share_new(visual: PGdkVisual; sharelist: PGdkGLContext;
                                  direct: TGLBool; attrlist: plongint): PGdkGLContext;
function gdk_gl_context_attrlist_share_new(attrlist: Plongint;
               sharelist: PGdkGLContext; direct: TGLBool): PGdkGLContext;
function gdk_gl_context_ref(context: PGdkGLContext): PGdkGLContext;
procedure gdk_gl_context_unref(context:PGdkGLContext);
function gdk_gl_make_current(drawable: PGdkDrawable;
                             context: PGdkGLContext): boolean;
procedure gdk_gl_swap_buffers(drawable: PGdkDrawable);
procedure gdk_gl_wait_gdk;
procedure gdk_gl_wait_gl;

{ glpixmap stuff  }

type
  TGdkGLPixmap = record end;
  PGdkGLPixmap = ^TGdkGLPixmap;
  TGLXContext = pointer;


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

function GTK_TYPE_GL_AREA: TGtkType;
function GTK_GL_AREA(obj: Pointer): PGtkGLArea;
function GTK_GL_AREA_CLASS(klass: Pointer): PGtkGLAreaClass;
function GTK_IS_GL_AREA(obj: Pointer): Boolean;
function GTK_IS_GL_AREA_CLASS(klass: Pointer): Boolean;

function gtk_gl_area_get_type: TGtkType;
function gtk_gl_area_new(attrList: Plongint): PGtkWidget;
function gtk_gl_area_share_new(attrList: Plongint; share: PGtkGLArea): PGtkWidget;
function gtk_gl_area_make_current(glarea: PGtkGLArea): boolean;
function gtk_gl_area_begingl(glarea: PGtkGLArea): boolean;
procedure gtk_gl_area_swap_buffers(gl_area: PGtkGLArea);


procedure LOpenGLViewport(Left, Top, Width, Height: integer);
procedure LOpenGLSwapBuffers(Handle: HWND);
function LOpenGLMakeCurrent(Handle: HWND): boolean;
function LOpenGLCreateContext(AWinControl: TWinControl;
             WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
             DoubleBuffered, RGBA: boolean;
             const RedBits, GreenBits, BlueBits,
             MultiSampling, AlphaBits, DepthBits, StencilBits, AUXBuffers: Cardinal;
             const AParams: TCreateParams): HWND;
procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);

{ Create GLX attributes list suitable for glXChooseVisual or glXChooseFBConfig.
  Note that if MultiSampling is > 1, it is expected that caller 
  already checked that GLX_ARB_multisample is available. }
function CreateOpenGLContextAttrList(DoubleBuffered: boolean;
  RGBA: boolean;
  const RedBits, GreenBits, BlueBits,
  MultiSampling, AlphaBits, DepthBits, StencilBits, AUXBuffers: Cardinal): PInteger;

implementation


var
  gl_area_type: TGtkType = 0;
  parent_class: Pointer = nil;

type
  TGdkGLContextPrivate = record
    xdisplay: PDisplay;
    glxcontext: TGLXContext;
    ref_count: gint;
  end;
  PGdkGLContextPrivate = ^TGdkGLContextPrivate;

type
  //PGLXPixmap = ^GLXPixmap;
  GLXPixmap = {%H-}TXID;

  //PGLXDrawable = ^GLXDrawable;
  GLXDrawable = {%H-}TXID;

{$IFNDEF UseFPGLX}
{ GLX 1.0 functions. }

function glXChooseVisual(dpy:PDisplay; screen:longint; attrib_list:Plongint):PXVisualInfo;cdecl;external;
procedure glXCopyContext(dpy:PDisplay; src:TGLXContext; dst:TGLXContext; mask: cardinal);cdecl;external;
function glXCreateContext(dpy:PDisplay; vis:PXVisualInfo; share_list:TGLXContext; direct:TGLBool):TGLXContext;cdecl;external;
function glXCreateGLXPixmap(dpy:PDisplay; vis:PXVisualInfo; pixmap:TPixmap):GLXPixmap;cdecl;external;
procedure glXDestroyContext(dpy:PDisplay; ctx:TGLXContext);cdecl;external;
procedure glXDestroyGLXPixmap(dpy:PDisplay; pix:GLXPixmap);cdecl;external;
function glXGetConfig(dpy:PDisplay; vis:PXVisualInfo; attrib:longint; var value:longint):longint;cdecl;external;
function glXGetCurrentContext:TGLXContext;cdecl;external;
function glXGetCurrentDrawable:GLXDrawable;cdecl;external;
function glXIsDirect(dpy:PDisplay; ctx:TGLXContext):TGLBool;cdecl;external;
function glXMakeCurrent(dpy:PDisplay; drawable:GLXDrawable; ctx:TGLXContext):TGLBool;cdecl;external;
function glXQueryExtension(dpy:PDisplay; var error_base:longint; var event_base:longint):TGLBool;cdecl;external;
function glXQueryVersion(dpy:PDisplay; major:Plongint; minor:Plongint):TGLBool;cdecl;external;
procedure glXSwapBuffers(dpy:PDisplay; drawable:GLXDrawable);cdecl;external;
procedure glXUseXFont(font:TFont; first:longint; count:longint; list_base:longint);cdecl;external;
procedure glXWaitGL;cdecl;external;
procedure glXWaitX;cdecl;external;
{$ENDIF}

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
  Result:=XDefaultScreen(ADisplay);
end;

function g_new(BaseSize, Count: integer): Pointer;
begin
  Result:=g_malloc(BaseSize*Count);
end;

function GetDefaultXDisplay: PDisplay;
begin
  Result:=GDK_DISPLAY;
end;

{$IFDEF LCLGtk2}
function GdkVisualAsString(Visual: PGdkVisual): string;
begin
  if Visual=nil then begin
    Result:='nil';
  end else begin
    with Visual^ do begin
      Result:=''
        //parent_instance : TGObject;
        +' TheType='+dbgs(ord(TheType))
        +' depth='+dbgs(depth)
        +' byte_order='+dbgs(ord(byte_order))
        +' colormap_size='+dbgs(colormap_size)
        +' bits_per_rgb='+dbgs(bits_per_rgb)
        +' red_mask='+hexstr(red_mask,8)
        +' red_shift='+dbgs(red_shift)
        +' red_prec='+dbgs(red_prec)
        +' green_mask='+hexstr(green_mask,8)
        +' green_shift='+dbgs(green_shift)
        +' green_prec='+dbgs(green_prec)
        +' blue_mask='+hexstr(blue_mask,8)
        +' blue_shift='+dbgs(blue_shift)
        +' blue_prec='+dbgs(blue_prec)
        //screen : PGdkScreen;
        +'';
    end;
  end;
end;

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
{$ENDIF}

function get_xvisualinfo(visual: PGdkVisual): PXVisualInfo;
// IMPORTANT: remember to XFree returned XVisualInfo !!!
var
  vinfo_template: TXVisualInfo;
  dpy: PDisplay;
  nitems_return: integer;
  vi: PXVisualInfo;
begin
  dpy := GetDefaultXDisplay;
  {$IFDEF Lclgtk2}
  DebugLn('get_xvisualinfo dpy=',XDisplayAsString(dpy));
  DebugLn('get_xvisualinfo visual=',GdkVisualAsString(Visual));
  RaiseGDBException('not implemented for gtk2');
  {$ENDIF}

  // 'GLX uses VisualInfo records because they uniquely identify
  // a (VisualID,screen,depth) tuple.'
  vinfo_template.bits_per_rgb:=0;
  FillChar(vinfo_template,SizeOf(vinfo_template),0);
  vinfo_template.visual   := GDK_VISUAL_XVISUAL({$IFDEF LCLGTK}
                                                PGdkVisualPrivate(visual)
                                                {$ELSE}
                                                visual
                                                {$ENDIF});
  vinfo_template.visualid := XVisualIDFromVisual(vinfo_template.visual);
  vinfo_template.depth    := PGdkVisualPrivate(visual)^.visual.depth;
  vinfo_template.screen   := DefaultScreen(GetDefaultXDisplay);
  {$IFDEF LCLGTK2}
  DebugLn('get_xvisualinfo vinfo_template.visual=',dbgs(vinfo_template.visual));
  DebugLn('get_xvisualinfo vinfo_template.visual: ',XVisualAsString(vinfo_template.visual));
  DebugLn('get_xvisualinfo vinfo_template.visualid=',dbgs(vinfo_template.visualid));
  DebugLn('get_xvisualinfo vinfo_template.depth=',dbgs(vinfo_template.depth),' GetDefaultXDisplay=',dbgs(GetDefaultXDisplay));
  DebugLn('get_xvisualinfo vinfo_template.screen=',dbgs(vinfo_template.screen));
  {$ENDIF}
  vi := XGetVisualInfo(dpy, VisualIDMask or VisualDepthMask or VisualScreenMask,
                       @vinfo_template, @nitems_return);
  DebugLn('get_xvisualinfo nitems_return=',dbgs(nitems_return));
  // visualinfo needs to be unique
  if (vi=nil) then raise Exception.Create('get_xvisualinfo vi=nil');
  if (nitems_return<>1) then raise Exception.Create('get_xvisualinfo nitems_return='+dbgs(nitems_return));

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

function gdk_gl_query: boolean;
var
  errorb: Integer = 0;
  event: Integer = 0;
begin
  Result:=boolean(glXQueryExtension(GetDefaultXDisplay, errorb, event));
end;

function gdk_gl_choose_visual(attrlist: Plongint): PGdkVisual;
var
  dpy: PDisplay;
  vi: PXVisualInfo;
  visual: PGdkVisual;
begin
  {$IFDEF lclgtk2}
  DebugLn(['gdk_gl_choose_visual not implemented yet for gtk2']);
  RaiseGDBException('');
  {$ENDIF}

  if attrList=nil then begin
    Result:=nil;
    exit;
  end;

  dpy := GetDefaultXDisplay;
  vi := glXChooseVisual(dpy,DefaultScreen(dpy), attrlist);
  if (vi=nil) then begin
    Result:=nil;
    exit;
  end;

  visual := gdkx_visual_get(vi^.visualid);
  XFree(vi);
  Result:=visual;
end;

function gdk_gl_get_config(visual: PGdkVisual; attrib: longint): longint;
var
  dpy: PDisplay;
  vi: PXVisualInfo;
  value: integer;
begin
  Result:=-1;
  if visual=nil then exit;

  dpy := GetDefaultXDisplay;

  vi := get_xvisualinfo(visual);

  value:=0;
  if (glXGetConfig(dpy, vi, attrib, value) = 0) then begin
    XFree(vi);
    Result:=value;
  end else
    XFree(vi);
end;

function gdk_gl_context_new(visual: PGdkVisual; attrlist: PlongInt): PGdkGLContext;
begin
  Result := gdk_gl_context_share_new(visual, nil, GLXFalse, attrlist);
end;

function gdk_gl_context_share_new(visual: PGdkVisual; sharelist: PGdkGLContext;
  direct: TGLBool; attrlist: plongint): PGdkGLContext;
var
  dpy: PDisplay;
  vi: PXVisualInfo;
  PrivateShareList: PGdkGLContextPrivate;
  PrivateContext: PGdkGLContextPrivate;
  glxcontext: TGLXContext;
{$IFDEF UseFPGLX}
  FBConfig: TGLXFBConfig;
  FBConfigs: PGLXFBConfig;
  FBConfigsCount: Integer;
{$ENDIF}
{$IFDEF VerboseMultiSampling}
  value: longint;
  ctxValue: longint;
{$ENDIF}

begin
  Result:=nil;

  dpy := GetDefaultXDisplay;

  {$IFDEF lclgtk2}
    if visual=nil then ;
    {$IFDEF UseFPGLX}
      FBConfigs:=nil;
      if GLX_version_1_3(dpy) then begin
        { use approach recommended since glX 1.3 }
        FBConfigsCount:=0;
        FBConfigs:=glXChooseFBConfig(dpy, DefaultScreen(dpy), @attrList[0], FBConfigsCount);
        {$IFDEF VerboseMultiSampling}
        debugln(['GLX config counts: ',FBConfigsCount]);
        {$ENDIF}
        if FBConfigsCount = 0 then
          raise Exception.Create('Could not find FB config');

        { just choose the first FB config from the FBConfigs list.
          More involved selection possible. }
        FBConfig := FBConfigs^;
        vi:=glXGetVisualFromFBConfig(dpy, FBConfig);
      end else begin
        vi:=glXChooseVisual(dpy, DefaultScreen(dpy), @attrList[0]);
      end;
    {$ELSE}
      vi:=glXChooseVisual(dpy, DefaultScreen(dpy), @attrList[0]);
    {$ENDIF}
  {$ELSE}
    if visual=nil then exit;
    vi := get_xvisualinfo(visual);
  {$ENDIF}
  if vi=nil then
    raise Exception.Create('gdk_gl_context_share_new no visual found');

  PrivateShareList:=PGdkGLContextPrivate(sharelist);

  {$IFDEF UseFPGLX}
    if GLX_version_1_3(dpy) then begin
      {$IFDEF VerboseMultiSampling}
      DebugLn('GLX Version 1.3 context creation');
      {$ENDIF}
      if (sharelist<>nil) then
        glxcontext := glXCreateNewContext(dpy, FBConfig, GLX_RGBA_TYPE,
                                          PrivateShareList^.glxcontext, true)
      else begin
        {$IFDEF VerboseMultiSampling}
        value:=0;
        debugln(['AttribValue: ',glXGetFBConfigAttrib(dpy, FBConfig, GLX_SAMPLES_ARB, value),'-',value]);
        {$ENDIF}
        glxcontext := glXCreateNewContext(dpy, FBConfig, GLX_RGBA_TYPE, nil, true);
        {$IFDEF VerboseMultiSampling}
        ctxValue:=0;
        debugln(['ContextAttrib: ',glXQueryContext(dpy, glxcontext, GLX_FBCONFIG_ID, ctxValue),'-',ctxValue]);
        {$ENDIF}
      end;
      if FBConfigs<>nil then
        XFree(FBConfigs);
    end else begin
      if (sharelist<>nil) then
        glxcontext := glXCreateContext(dpy, vi, PrivateShareList^.glxcontext,
                                       direct)
      else
        glxcontext := glXCreateContext(dpy, vi, nil, direct);
    end;
  {$ELSE}
    if (sharelist<>nil) then
      glxcontext := glXCreateContext(dpy, vi, PrivateShareList^.glxcontext,
                                     direct)
    else
      glxcontext := glXCreateContext(dpy, vi, nil, direct);
  {$ENDIF}

  XFree(vi);
  if (glxcontext = nil) then exit;

  PrivateContext := g_new(SizeOf(TGdkGLContextPrivate), 1);
  PrivateContext^.xdisplay := dpy;
  PrivateContext^.glxcontext := glxcontext;
  PrivateContext^.ref_count := 1;

  Result := PGdkGLContext(PrivateContext);
end;

function gdk_gl_context_attrlist_share_new(attrlist: Plongint;
  sharelist: PGdkGLContext; direct: TGLBool): PGdkGLContext;
var
  visual: PGdkVisual;
begin
  {$IFDEF lclgtk2}
  visual :=nil;
  Result := gdk_gl_context_share_new(visual, sharelist, direct, attrlist);
  {$ELSE}
  visual := gdk_gl_choose_visual(attrlist);
  if (visual <> nil) then
    Result := gdk_gl_context_share_new(visual, sharelist, direct, attrlist)
  else
    Result := nil;
  {$ENDIF}
end;

function gdk_gl_context_ref(context: PGdkGLContext): PGdkGLContext;
var
  PrivateContext: PGdkGLContextPrivate;
begin
  Result:=nil;
  if context=nil then exit;
  PrivateContext := PGdkGLContextPrivate(context);
  inc(PrivateContext^.ref_count);
  //DebugLn(['gdk_gl_context_ref ref_count=',PrivateContext^.ref_count]);
  Result:=context;
end;

procedure gdk_gl_context_unref(context: PGdkGLContext);
var
  PrivateContext: PGdkGLContextPrivate;
begin
  g_return_if_fail(context<>nil,'');

  PrivateContext:=PGdkGLContextPrivate(context);

  dec(PrivateContext^.ref_count);
  if (PrivateContext^.ref_count = 0) then begin
    //DebugLn(['gdk_gl_context_unref START ref_count=',PrivateContext^.ref_count]);
    if (PrivateContext^.glxcontext = glXGetCurrentContext()) then
      glXMakeCurrent(PrivateContext^.xdisplay, None, nil);
    glXDestroyContext(PrivateContext^.xdisplay, PrivateContext^.glxcontext);
    PrivateContext^.glxcontext:=nil;
    g_free(PrivateContext);
    //DebugLn(['gdk_gl_context_unref END']);
  end;
end;

function gdk_gl_make_current(drawable: PGdkDrawable;
  context: PGdkGLContext): boolean;
var
  PrivateContext: PGdkGLContextPrivate;
begin
  Result:=false;
  if  drawable=nil then exit;
  if context=nil then exit;
  PrivateContext := PGdkGLContextPrivate(context);

  Result:=boolean(glXMakeCurrent(PrivateContext^.xdisplay,
                                 {$IFDEF LCLGTK}
                                 GDK_WINDOW_XWINDOW(PGdkWindowPrivate(drawable)),
                                 {$ELSE}
                                 GDK_WINDOW_XWINDOW(drawable),
                                 {$ENDIF}
                                 PrivateContext^.glxcontext)
                                 );
end;

procedure gdk_gl_swap_buffers(drawable: PGdkDrawable);
begin
  g_return_if_fail(drawable <> nil);

  glXSwapBuffers({$IFDEF LCLGTK}
                 GDK_WINDOW_XDISPLAY(PGdkWindowPrivate(drawable)),
                 GDK_WINDOW_XWINDOW(PGdkWindowPrivate(drawable))
                 {$ELSE}
                 GDK_WINDOW_XDISPLAY(drawable),
                 GDK_WINDOW_XWINDOW(drawable)
                 {$ENDIF}
                 );
end;

procedure gdk_gl_wait_gdk;
begin
  glXWaitX;
end;

procedure gdk_gl_wait_gl;
begin
  glXWaitGL;
end;

procedure gtk_gl_area_init(
  {$IFDEF LCLGTK}
  gl_area, theClass: Pointer
  {$ELSE}
  gl_area: PGTypeInstance; theClass: gpointer
  {$ENDIF}
  ); cdecl;
begin
  if theClass=nil then ;
  //DebugLn(['gtk_gl_area_init START']);
  PGtkGLArea(gl_area)^.glcontext:=nil;
  {$IFDEF LclGtk2}
  gtk_widget_set_double_buffered(PGtkWidget(gl_area),gdkFALSE);
  GTK_WIDGET_UNSET_FLAGS(PGtkWidget(gl_area),GTK_NO_WINDOW);
  {$ENDIF}
  //DebugLn(['gtk_gl_area_init END']);
end;

function GTK_TYPE_GL_AREA: TGtkType;
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

function GTK_GL_AREA(obj: Pointer): PGtkGLArea;
begin
  g_return_if_fail(GTK_IS_GL_AREA(obj),'');
  Result:=PGtkGLArea(obj);
end;

function GTK_GL_AREA_CLASS(klass: Pointer): PGtkGLAreaClass;
begin
  g_return_if_fail(GTK_IS_GL_AREA_CLASS(klass),'');
  Result:=PGtkGLAreaClass(klass);
end;

function GTK_IS_GL_AREA(obj: Pointer): Boolean;
begin
  {$IFDEF LCLGTK}
  Result := Assigned(obj) and GTK_IS_GL_AREA_CLASS(PGtkTypeObject(obj)^.klass);
  {$ELSE}
  GTK_IS_GL_AREA:=GTK_CHECK_TYPE(obj,GTK_TYPE_GL_AREA);
  {$ENDIF}
end;

function GTK_IS_GL_AREA_CLASS(klass: Pointer): Boolean;
begin
  {$IFDEF LCLGTK}
  Result := Assigned(klass) and (PGtkTypeClass(klass)^.thetype = GTK_TYPE_GL_AREA);
  {$ELSE}
  GTK_IS_GL_AREA_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_GL_AREA);
  {$ENDIF}
end;

function gtk_gl_area_get_type: TGtkType;
begin
  Result:=GTK_TYPE_GL_AREA;
end;

function gtk_gl_area_new(attrList: Plongint): PGtkWidget;
var
  Count: Integer;
  CopyAttrList: Plongint;
  Size: Integer;
begin
  Count:=0;
  while (attrList[Count]<>0) do inc(Count);
  inc(Count);
  Size:=SizeOf(Integer)*Count;
  CopyAttrList:=nil;
  GetMem(CopyAttrList,Size);
  System.Move(attrList^,CopyAttrList^,Size);
  Result:=gtk_gl_area_share_new(CopyAttrList,nil);
  FreeMem(CopyAttrList);
end;

function gtk_gl_area_share_new(attrList: Plongint; share: PGtkGLArea
  ): PGtkWidget;
var
  visual: PGdkVisual;
  sharelist: PGdkGLContext;
  glcontext: PGdkGLContext;
  gl_area: PGtkGLArea;
begin
  Result := nil;
  //DebugLn(['gtk_gl_area_share_new START']);
  if (share <> nil) and (not GTK_IS_GL_AREA(share)) then
    exit;
  {$IFNDEF MSWindows}
  {$IFDEF lclgtk2}
  visual := nil;
  {$ELSE}
  visual := gdk_gl_choose_visual(attrlist);
  if (visual = nil) then exit;
  {$ENDIF}
  {$ENDIF non MSWindows}

  sharelist := nil;
  if share <> nil then sharelist := share^.glcontext;
  glcontext := gdk_gl_context_share_new(visual, sharelist, GLXTrue, attrlist);
  if (glcontext = nil) then exit;

  {$IFNDEF MSWindows}
  if visual <> nil then begin
    // use colormap and visual suitable for OpenGL rendering
    gtk_widget_push_colormap(gdk_colormap_new(visual, gtk_TRUE));
    gtk_widget_push_visual(visual);
  end;
  {$ENDIF non MSWindows}

  gl_area := gtk_type_new (gtk_gl_area_get_type);
  gl_area^.glcontext := glcontext;

  {$IFNDEF MSWindows}
  if visual<>nil then begin
    // pop back defaults
    gtk_widget_pop_visual;
    gtk_widget_pop_colormap;
  end;
  {$ENDIF non MSWindows}
  Result:=PGtkWidget(gl_area);
end;

function gtk_gl_area_make_current(glarea: PGtkGLArea): boolean;
begin
  Result:=false;
  if glarea=nil then exit;
  if not GTK_IS_GL_AREA(glarea) then exit;
  if not GTK_WIDGET_REALIZED(PGtkWidget(glarea)) then exit;

  //DebugLn(['gtk_gl_area_make_current START']);
  Result:=gdk_gl_make_current(PGtkWidget(glarea)^.window, glarea^.glcontext);
  //DebugLn(['gtk_gl_area_make_current END']);
end;

function gtk_gl_area_begingl(glarea: PGtkGLArea): boolean;
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

procedure LOpenGLViewport(Left, Top, Width, Height: integer);
begin
  glViewport(Left,Top,Width,Height);
end;

procedure LOpenGLSwapBuffers(Handle: HWND);
begin
  gtk_gl_area_swap_buffers({%H-}PGtkGLArea(Handle));
end;

function LOpenGLMakeCurrent(Handle: HWND): boolean;
var
  Widget: PGtkWidget;
  glarea: PGtkGLArea;
begin
  if Handle=0 then
    RaiseGDBException('LOpenGLSwapBuffers Handle=0');
  Result:=false;

  Widget:={%H-}PGtkWidget(PtrUInt(Handle));
  glarea:=PGtkGLArea(Widget);
  if not GTK_IS_GL_AREA(glarea) then
    RaiseGDBException('LOpenGLSwapBuffers not a PGtkGLArea');

  // make sure the widget is realized
  gtk_widget_realize(Widget);
  if not GTK_WIDGET_REALIZED(Widget) then exit;

  // make current
  Result:=gtk_gl_area_make_current(glarea);
end;

{$IFDEF LCLGtk2}
function gtkglarea_size_allocateCB(Widget: PGtkWidget; Size: pGtkAllocation;
  Data: gPointer): GBoolean; cdecl;
const
  CallBackDefaultReturn = {$IFDEF GTK2}false{$ELSE}true{$ENDIF};
var
  SizeMsg: TLMSize;
  GtkWidth, GtkHeight: integer;
  LCLControl: TWinControl;
begin
  Result := CallBackDefaultReturn;
  if not GTK_WIDGET_REALIZED(Widget) then begin
    // the widget is not yet realized, so this GTK resize was not a user change.
    // => ignore
    exit;
  end;
  if Size=nil then ;
  LCLControl:=TWinControl(Data);
  if LCLControl=nil then exit;
  //DebugLn(['gtkglarea_size_allocateCB ',DbgSName(LCLControl)]);

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
  //DebugLn(['gtkglarea_size_allocateCB ',GtkWidth,',',GtkHeight]);
  LCLControl.WindowProc(TLMessage(SizeMsg));
end;
{$ENDIF}

function LOpenGLCreateContextCore(AWinControl: TWinControl;
  WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
  DoubleBuffered, RGBA: boolean;
  const RedBits, GreenBits, BlueBits,
  MultiSampling, AlphaBits, DepthBits, StencilBits, AUXBuffers: Cardinal;
  const AParams: TCreateParams): HWND;
var
  NewWidget: PGtkWidget;
  SharedArea: PGtkGLArea;
  AttrList: PInteger;
begin
  if WSPrivate=nil then ;
  {$IFDEF VerboseMultiSampling}
  debugln(['LOpenGLCreateContextCore MultiSampling=',MultiSampling]);
  {$ENDIF}
  AttrList:=CreateOpenGLContextAttrList(DoubleBuffered,RGBA,RedBits,GreenBits,
    BlueBits,MultiSampling,AlphaBits,DepthBits,StencilBits,AUXBuffers);
  try
    if SharedControl<>nil then begin
      SharedArea:={%H-}PGtkGLArea(PtrUInt(SharedControl.Handle));
      if not GTK_IS_GL_AREA(SharedArea) then
        RaiseGDBException('LOpenGLCreateContext');
      NewWidget:=gtk_gl_area_share_new(AttrList,SharedArea);
    end else begin
      NewWidget:=gtk_gl_area_new(AttrList);
    end;
    Result:=HWND({%H-}PtrUInt(Pointer(NewWidget)));
    PGtkobject(NewWidget)^.flags:=PGtkobject(NewWidget)^.flags or GTK_CAN_FOCUS;
    {$IFDEF LCLGtk}
    TGTKWidgetSet(WidgetSet).FinishCreateHandle(AWinControl,NewWidget,AParams);
    {$ELSE}
    TGTK2WidgetSet(WidgetSet).FinishCreateHandle(AWinControl,NewWidget,AParams);
    g_signal_connect_after(PGtkObject(NewWidget), 'size-allocate',
                       TGTKSignalFunc(@gtkglarea_size_allocateCB), AWinControl);
    {$ENDIF}
  finally
    FreeMem(AttrList);
  end;
end;

function LOpenGLCreateContext(AWinControl: TWinControl;
  WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
  DoubleBuffered, RGBA: boolean;
  const RedBits, GreenBits, BlueBits,
  MultiSampling, AlphaBits, DepthBits, StencilBits, AUXBuffers: Cardinal;
  const AParams: TCreateParams): HWND;
begin
  {$IFDEF VerboseMultiSampling}
  debugln(['LOpenGLCreateContext MultiSampling=',MultiSampling]);
  {$ENDIF}
  if (MultiSampling > 1) and
     {$IFDEF UseFPGLX}
     GLX_ARB_multisample(GetDefaultXDisplay, DefaultScreen(GetDefaultXDisplay))
     {$ELSE}
     false { no GLX_ARB_multisample support when UseFPGLX undefined,
             it would be too convoluted to replicate GLX unit extension querying
             functionality here }
     {$ENDIF}
  then begin
    {$IFDEF VerboseMultiSampling}
    debugln(['LOpenGLCreateContext GLX_ARB_multisample succeeded']);
    {$ENDIF}
    try
      Result := LOpenGLCreateContextCore(AWinControl, WSPrivate, SharedControl,
        DoubleBuffered, RGBA, RedBits, GreenBits, BlueBits, MultiSampling,
        AlphaBits, DepthBits, StencilBits, AUXBuffers, AParams);
    except
      {$IFDEF VerboseMultiSampling}
      debugln(['LOpenGLCreateContext LOpenGLCreateContextCore failed, trying without multisampling']);
      {$ENDIF}
      { retry without MultiSampling }
      Result := LOpenGLCreateContextCore(AWinControl, WSPrivate, SharedControl,
        DoubleBuffered, RGBA, RedBits, GreenBits, BlueBits, 1, AlphaBits,
        DepthBits, StencilBits, AUXBuffers, AParams);
    end;
  end else begin
    { no multi-sampling requested (or GLX_ARB_multisample not available),
      just pass to LOpenGLCreateContextCore }
    Result := LOpenGLCreateContextCore(AWinControl, WSPrivate, SharedControl, 
      DoubleBuffered, RGBA, RedBits, GreenBits, BlueBits, MultiSampling,
      AlphaBits, DepthBits, StencilBits, AUXBuffers, AParams);
  end;
end;

procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);
begin
  if not AWinControl.HandleAllocated then exit;
  // nothing to do
end;

function CreateOpenGLContextAttrList(DoubleBuffered: boolean; RGBA: boolean;
  const RedBits, GreenBits, BlueBits, MultiSampling, AlphaBits, DepthBits,
  StencilBits, AUXBuffers: Cardinal): PInteger;
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
    if UseFBConfig then begin Add(GLX_X_RENDERABLE); Add(1); end;
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
    if MultiSampling > 1 then
    begin
      // multisampling contexts are non-conformant so only ask for those
      Add(GLX_CONFIG_CAVEAT); Add(GLX_NON_CONFORMANT_CONFIG);
      Add(GLX_SAMPLE_BUFFERS_ARB); Add(1);
      Add(GLX_SAMPLES_ARB); Add(MultiSampling);
    end;
    
    Add(0); { 0 = X.None (be careful: GLX_NONE is something different) }
  end;
  
begin
  {$IFDEF VerboseMultiSampling}
  debugln(['CreateOpenGLContextAttrList MultiSampling=',MultiSampling]);
  {$ENDIF}
  UseFBConfig := {$IFDEF UseFPGLX} GLX_version_1_3(GetDefaultXDisplay) {$else} false {$endif};
  Result:=nil;
  CreateList;
  GetMem(Result,SizeOf(integer)*p);
  CreateList;
end;

end.

