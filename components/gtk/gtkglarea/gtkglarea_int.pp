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

}
unit GTKGLArea_Int;

{$mode objfpc}{$H+}
{$PACKRECORDS C}

interface

uses gdk, gtk, gl;

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

function gdk_gl_query: Integer;cdecl;external;
// function gdk_gl_get_info:^char;cdecl;external;
function gdk_gl_choose_visual(attrlist:Plongint):PGdkVisual;cdecl;external;
function gdk_gl_get_config(visual:PGdkVisual; attrib:longint):longint;cdecl;external;
function gdk_gl_context_new(visual:PGdkVisual):PGdkGLContext;cdecl;external;
function gdk_gl_context_share_new(visual:PGdkVisual; sharelist:PGdkGLContext; direct:Integer):PGdkGLContext;cdecl;external;
function gdk_gl_context_attrlist_share_new(attrlist:Plongint; sharelist:PGdkGLContext; direct:Integer):PGdkGLContext;cdecl;external;
function gdk_gl_context_ref(context:PGdkGLContext):PGdkGLContext;cdecl;external;
procedure gdk_gl_context_unref(context:PGdkGLContext);cdecl;external;
function gdk_gl_make_current(drawable:PGdkDrawable; context:PGdkGLContext):Integer;cdecl;external;
procedure gdk_gl_swap_buffers(drawable:PGdkDrawable);cdecl;external;
procedure gdk_gl_wait_gdk;cdecl;external;
procedure gdk_gl_wait_gl;cdecl;external;

{ glpixmap stuff  }

type
  TGdkGLPixmap = record end;
  PGdkGLPixmap = ^TGdkGLPixmap;


function gdk_gl_pixmap_new(visual:PGdkVisual; pixmap:PGdkPixmap):PGdkGLPixmap;cdecl;external;
function gdk_gl_pixmap_ref(glpixmap:PGdkGLPixmap):PGdkGLPixmap;cdecl;external;
procedure gdk_gl_pixmap_unref(glpixmap:PGdkGLPixmap);cdecl;external;
function gdk_gl_pixmap_make_current(glpixmap:PGdkGLPixmap; context:PGdkGLContext):Integer;cdecl;external;

{ fonts  }

procedure gdk_gl_use_gdk_font(font:PGdkFont; first:longint; count:longint; list_base:longint);cdecl;external;

// gtkglarea

function GTK_TYPE_GL_AREA: TGtkType; cdecl; external 'gtkgl' name 'gtk_gl_area_get_type';
{ return type might be wrong }

{ argument types are unknown }
{ return type might be wrong }
//function GTK_GL_AREA(obj : longint) : longint;

{ argument types are unknown }
{ return type might be wrong }
//function GTK_GL_AREA_CLASS(klass : longint) : longint;

{ argument types are unknown }
{ return type might be wrong }
function GTK_IS_GL_AREA(obj : Pointer) : Boolean;

function GTK_IS_GL_AREA_CLASS(klass: Pointer): Boolean;
{ return type might be wrong }

type
  PGtkGLArea = ^TGtkGLArea;
  TGtkGLArea = record
    darea: TGtkDrawingArea;
    glcontext: PGdkGLContext;
  end;

  PGtkGLAreaClass = ^TGtkGLAreaClass;
  TGtkGLAreaClass = record
    parent_class: TGtkDrawingAreaClass;
  end;

function gtk_gl_area_get_type:TGtkType;cdecl;external;
function gtk_gl_area_new(attrList:Plongint):PGtkWidget;cdecl;external;
function gtk_gl_area_share_new(attrList:Plongint; share:PGtkGLArea):PGtkWidget;cdecl;external;
function gtk_gl_area_new_vargs(share:PGtkGLArea; args:array of const):PGtkWidget;cdecl;external;
function gtk_gl_area_new_vargs(share:PGtkGLArea):PGtkWidget;cdecl;external;
function gtk_gl_area_make_current(glarea:PGtkGLArea):Integer;cdecl;external;
function gtk_gl_area_begingl(glarea:PGtkGLArea):Integer;cdecl;external;

{ deprecated, use gtk_gl_area_make_current  }
procedure gtk_gl_area_endgl(glarea:PGtkGLArea);cdecl;external;

{ deprecated  }
procedure gtk_gl_area_swapbuffers(glarea:PGtkGLArea);cdecl;external;

{ deprecated  }
procedure gtk_gl_area_swap_buffers(glarea:PGtkGLArea);cdecl;external;

{ deprecated, use gtk_drawing_area_size()  }
procedure gtk_gl_area_size(glarea:PGtkGLArea; width:Integer; height:Integer);cdecl;external;

implementation

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
{  function GTK_GL_AREA(obj : longint) : longint;
    begin
      GTK_GL_AREA:=GTK_CHECK_CAST(obj,GTK_TYPE_GL_AREA,GtkGLArea);
    end;}

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
{  function GTK_GL_AREA_CLASS(klass : longint) : longint;
    begin
       GTK_GL_AREA_CLASS:=GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_GL_AREA,GtkGLAreaClass);
    end;}

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
  function GTK_IS_GL_AREA(obj: Pointer): Boolean;
    begin
       // GTK_IS_GL_AREA:=GTK_CHECK_TYPE(obj,GTK_TYPE_GL_AREA);
       Result := Assigned(obj) and GTK_IS_GL_AREA_CLASS(PGtkTypeObject(obj)^.klass);
    end;

{ was #define dname def_expr }
  function GTK_IS_GL_AREA_CLASS(klass: Pointer): Boolean;
      { return type might be wrong }
      begin
         // GTK_IS_GL_AREA_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_GL_AREA);
         Result := Assigned(klass) and (PGtkTypeClass(klass)^.thetype = GTK_TYPE_GL_AREA);
      end;

{$PACKRECORDS DEFAULT}

end.
