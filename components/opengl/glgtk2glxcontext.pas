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

}
unit GLGtk2GLXContext;

{$mode objfpc}{$H+}
{$LinkLib GL}
{$PACKRECORDS C}

interface

uses
  Classes, SysUtils, LCLProc, LCLType, X, XUtil, XLib, gl, InterfaceBase,
  WSLCLClasses, GtkWSControls,
  {$IFDEF LCLGTK2}
  GtkDef, glx, gdk2x, glib2, gdk2, gtk2, Gtk2Int,
  {$ENDIF}
  {$IFDEF LCLGTK}
  glib, gdk, gtk, GtkInt,
  {$ENDIF}
  Controls;

procedure LOpenGLViewport(Left, Top, Width, Height: integer);
procedure LOpenGLSwapBuffers(Handle: HWND);
function LOpenGLMakeCurrent(Handle: HWND): boolean;
function LOpenGLCreateContext(AWinControl: TWinControl;
             WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
             DoubleBuffered, RGBA: boolean): HWND;
procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);
function CreateOpenGLContextAttrList(DoubleBuffered: boolean;
  RGBA: boolean): PInteger;

type
  TWidgetSetWSWinControl = TGtkWSWinControl;

implementation

type
  TLOpenGLInfo = record
    Control: TWinControl;
    xdisplay: PDisplay;
    glxcontext: TGLXContext;
    ref_count: guint;
    AttrList: PInteger;
    Mapped: boolean;
    XWindow: x.TWindow;
  end;
  PLOpenGLInfo = ^TLOpenGLInfo;

procedure InternalResizeWnd(AWinControl: TWinControl);
var
  Info: PLOpenGLInfo;
  Widget: PGtkWidget;
begin
  if (not AWinControl.HandleAllocated) then exit;
  Widget:=PGtkWidget(AWinControl.Handle);
  Info:=PLOpenGLInfo(gtk_object_get_data(PGtkObject(Widget),'LOpenGLInfo'));

  DebugLn(['InternalResizeWnd ',dbgs(AWinControl.BoundsRect)]);
  with Info^ do begin
    if (AWinControl.Width > 0) and (AWinControl.Height > 0) then begin
      if not Mapped then begin
        Mapped:=True;
        XMapWindow(xdisplay, XWindow);
      end;
      XResizeWindow(xdisplay, XWindow, AWinControl.Width, AWinControl.Height);
    end else begin
      if Mapped then begin
        XUnmapWindow(xdisplay, XWindow);
        Mapped:=False;
      end;
    end;
  end;
end;

procedure LOpenGLViewport(Left, Top, Width, Height: integer);
begin
  glViewport(Left,Top,Width,Height);
end;

procedure LOpenGLSwapBuffers(Handle: HWND);
var
  Info: PLOpenGLInfo;
begin
  Info:=PLOpenGLInfo(gtk_object_get_data(PGtkObject(Handle),'LOpenGLInfo'));
  glXSwapBuffers(Info^.xdisplay, Info^.XWindow);
end;

function LOpenGLMakeCurrent(Handle: HWND): boolean;
var
  Info: PLOpenGLInfo;
begin
  Info:=PLOpenGLInfo(gtk_object_get_data(PGtkObject(Handle),'LOpenGLInfo'));
  Result:=glXMakeCurrent(Info^.xdisplay, Info^.XWindow, Info^.glxcontext);
  DebugLn(['LOpenGLMakeCurrent Result=',Result]);
  InternalResizeWnd(Info^.Control);
end;

function gtkRealizeAfter(Widget: PGtkWidget; Data: Pointer): GBoolean; cdecl;
var
  GdkWindow: PGdkWindow;
  CurXWindow: TXID;
  Info: PLOpenGLInfo;
  XVInfo: PXVisualInfo;
  ColorMap: TColormap;
  WinAttr: TXSetWindowAttributes;
  LCLControl: TWinControl;
begin
  Result:={$IFDEF GTK2}false{$ELSE}true{$ENDIF};

  DebugLn(['gtkRealizeAfter ']);
  // disable gtk painting
  gdk_window_set_back_pixmap(Widget^.Window, nil, GdkFalse);
  
  GdkWindow:=Widget^.window;
  CurXWindow:=gdk_window_xwindow(PGdkDrawable(GdkWindow));
  
  Info:=PLOpenGLInfo(gtk_object_get_data(PGtkObject(Widget),'LOpenGLInfo'));

  XVInfo:=glXChooseVisual(Info^.XDisplay, DefaultScreen(Info^.XDisplay), @Info^.AttrList[0]);
  ColorMap:=XCreateColormap(Info^.XDisplay, CurXWindow, XVInfo^.visual, AllocNone);

  FillChar(WinAttr, SizeOf(WinAttr), 0);
  WinAttr.event_mask:=0; //ExposureMask;
  WinAttr.colormap:=ColorMap;
  
  LCLControl:=TWinControl(Data);

  Info^.XWindow:=XCreateWindow(Info^.XDisplay, CurXWindow,
    LCLControl.Left+30, LCLControl.Top+30, LCLControl.Width, LCLControl.Height, 0,
    XVInfo^.depth, InputOutput, XVInfo^.visual, CWColormap, @WinAttr);
  {Info^.XWindow:=XCreateSimpleWindow(Info^.XDisplay, CurXWindow,
    LCLControl.Left+30, LCLControl.Top+30, LCLControl.Width, LCLControl.Height,
    0,0,0);}
  //Info^.XWindow:=CurXWindow;

  Info^.glxcontext:=glXCreateContext(Info^.XDisplay, XVInfo, nil, True);
  DebugLn(['gtkRealizeAfter ',dbgs(Info^.glxcontext),' mapped=',GTK_WIDGET_MAPPED(Widget)]);
  glXMakeCurrent(Info^.xdisplay, Info^.XWindow, Info^.glxcontext);
  InternalResizeWnd(LCLControl);
end;

function LOpenGLCreateContext(AWinControl: TWinControl;
  WSPrivate: TWSPrivateClass; SharedControl: TWinControl; DoubleBuffered,
  RGBA: boolean): HWND;
var
  NewWidget: PGtkWidget;
  Info: PLOpenGLInfo;
begin
  if WSPrivate=nil then ;
  if SharedControl<>nil then begin
    //SharedArea:=PGtkGLArea(SharedControl.Handle);
    //if not GTK_IS_GL_AREA(SharedArea) then
    //  RaiseGDBException('LOpenGLCreateContext');
    //NewWidget:=gtk_gl_area_share_new(AttrList,SharedArea);
  end else begin
    //NewWidget:=gtk_gl_area_new(AttrList);
    NewWidget:=gtk_frame_new('Test');
    {$IFDEF GTK2}
    gtk_fixed_set_has_window(PGtkFixed(Result), True);
    {$ENDIF}
    g_signal_connect_after(PGtkObject(NewWidget), 'realize',
        TGTKSignalFunc(@GTKRealizeAfter), AWinControl);
    New(Info);
    FillChar(Info^,SizeOf(Info),0);
    Info^.Control:=AWinControl;
    Info^.xdisplay:=gdk_display;
    Info^.AttrList:=CreateOpenGLContextAttrList(DoubleBuffered,RGBA);
    gtk_object_set_data(PGtkObject(NewWidget),'LOpenGLInfo',Info);
    //gtk_widget_set_double_buffered(NewWidget,false);
  end;
  Result:=HWND(NewWidget);
  PGtkobject(NewWidget)^.flags:=PGtkobject(NewWidget)^.flags or GTK_CAN_FOCUS;
  {$IFDEF LCLGtk}
  TGTKWidgetSet(WidgetSet).FinishComponentCreate(AWinControl,NewWidget);
  {$ELSE}
  TGTK2WidgetSet(WidgetSet).FinishComponentCreate(AWinControl,NewWidget);
  {$ENDIF}
end;

procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);
var
  Info: PLOpenGLInfo;
begin
  if not AWinControl.HandleAllocated then exit;
  Info:=PLOpenGLInfo(gtk_object_get_data(PGtkObject(AWinControl.Handle),'LOpenGLInfo'));
  gtk_object_set_data(PGtkObject(AWinControl.Handle),'LOpenGLInfo',nil);
  ReAllocMem(Info^.AttrList,0);
  glXDestroyContext(Info^.xdisplay, Info^.glxcontext);
  XDestroyWindow(Info^.xdisplay, Info^.XWindow);
  Dispose(Info);
end;

function CreateOpenGLContextAttrList(DoubleBuffered: boolean; RGBA: boolean
  ): PInteger;
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
    if DoubleBuffered then
      Add(GLX_DOUBLEBUFFER);
    if RGBA then
      Add(GLX_RGBA);
    Add(GLX_RED_SIZE);  Add(1);
    Add(GLX_GREEN_SIZE);  Add(1);
    Add(GLX_BLUE_SIZE);  Add(1);
    Add(GLX_DEPTH_SIZE);  Add(1);
    Add(GLX_STENCIL_SIZE); Add(1);
    Add(0);
  end;

begin
  Result:=nil;
  p:=0;
  CreateList;
  GetMem(Result,SizeOf(integer)*p);
  p:=0;
  CreateList;
end;

end.

