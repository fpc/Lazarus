{
  FreePascal Bindings for libGL by NVIDIA  glx.h

  Author: Mattias Gaertner

  This file is distributed under the Library GNU General Public License
  with the following modification:

  - object files and libraries linked into an application may be
    distributed without source code.

  If you didn't receive a copy of the file COPYING, contact:
        Free Software Foundation
        675 Mass Ave
        Cambridge, MA  02139
        USA
}
unit NVGLX;

{$mode objfpc}{$H+}
{$LinkLib GL}

interface

uses
  X, XLib, XUtil, NVGL;

{$PACKRECORDS C}
const
  GLX_VERSION_1_1 = 1;
  GLX_VERSION_1_2 = 1;

{ ------------------------------------------------------------------------------
   Names for attributes to glXGetConfig.
}
  { support GLX rendering  }
  GLX_USE_GL = 1;
  { depth of the color buffer  }
  GLX_BUFFER_SIZE = 2;
  { level in plane stacking  }
  GLX_LEVEL = 3;
  { true if RGBA mode  }
  GLX_RGBA = 4;
  { double buffering supported  }
  GLX_DOUBLEBUFFER = 5;
  { stereo buffering supported  }
  GLX_STEREO = 6;
  { number of aux buffers  }
  GLX_AUX_BUFFERS = 7;
  { number of red component bits  }
  GLX_RED_SIZE = 8;
  { number of green component bits  }
  GLX_GREEN_SIZE = 9;
  { number of blue component bits  }
  GLX_BLUE_SIZE = 10;
  { number of alpha component bits  }
  GLX_ALPHA_SIZE = 11;
  { number of depth bits  }
  GLX_DEPTH_SIZE = 12;
  { number of stencil bits  }
  GLX_STENCIL_SIZE = 13;
  { number of red accum bits  }
  GLX_ACCUM_RED_SIZE = 14;
  { number of green accum bits  }
  GLX_ACCUM_GREEN_SIZE = 15;
  { number of blue accum bits  }
  GLX_ACCUM_BLUE_SIZE = 16;
  { number of alpha accum bits  }
  GLX_ACCUM_ALPHA_SIZE = 17;

  { number of multisample buffers  }
  GLX_SAMPLE_BUFFERS_ARB = 100000;
  { number of multisample samples  }
  GLX_SAMPLES_ARB = 100001;


{ ------------------------------------------------------------------------------
  FBConfig-specific attributes
}

  GLX_X_VISUAL_TYPE = $22;
  { Like visual_info VISUAL_CAVEAT  }
  GLX_CONFIG_CAVEAT = $20;
  GLX_TRANSPARENT_TYPE = $23;
  GLX_TRANSPARENT_INDEX_VALUE = $24;
  GLX_TRANSPARENT_RED_VALUE = $25;
  GLX_TRANSPARENT_GREEN_VALUE = $26;
  GLX_TRANSPARENT_BLUE_VALUE = $27;
  GLX_TRANSPARENT_ALPHA_VALUE = $28;
  GLX_DRAWABLE_TYPE = $8010;
  GLX_RENDER_TYPE = $8011;
  GLX_X_RENDERABLE = $8012;
  GLX_FBCONFIG_ID = $8013;
  GLX_MAX_PBUFFER_WIDTH = $8016;
  GLX_MAX_PBUFFER_HEIGHT = $8017;
  GLX_MAX_PBUFFER_PIXELS = $8018;
  GLX_VISUAL_ID = $800B;

  GLX_DRAWABLE_TYPE_SGIX = GLX_DRAWABLE_TYPE;
  GLX_RENDER_TYPE_SGIX = GLX_RENDER_TYPE;
  GLX_X_RENDERABLE_SGIX = GLX_X_RENDERABLE;
  GLX_FBCONFIG_ID_SGIX = GLX_FBCONFIG_ID;
  GLX_MAX_PBUFFER_WIDTH_SGIX = GLX_MAX_PBUFFER_WIDTH;
  GLX_MAX_PBUFFER_HEIGHT_SGIX = GLX_MAX_PBUFFER_HEIGHT;
  GLX_MAX_PBUFFER_PIXELS_SGIX = GLX_MAX_PBUFFER_PIXELS;
  GLX_OPTIMAL_PBUFFER_WIDTH_SGIX = $8019;
  GLX_OPTIMAL_PBUFFER_HEIGHT_SGIX = $801A;


{ ------------------------------------------------------------------------------
  Error return values from glXGetConfig.  Success is indicated by
  a value of 0.
}
  { screen # is bad  }
  GLX_BAD_SCREEN = 1;
  { attribute to get is bad  }
  GLX_BAD_ATTRIBUTE = 2;
  { no glx extension on server  }
  GLX_NO_EXTENSION = 3;
  { visual # not known by GLX  }
  GLX_BAD_VISUAL = 4;
  GLX_BAD_CONTEXT = 5;
  GLX_BAD_VALUE = 6;
  GLX_BAD_ENUM = 7;


{ ------------------------------------------------------------------------------
  FBConfig attribute values
}
  { Generic "don't care" value for glX ChooseFBConfig attributes (except GLX_LEVEL) }
  GLX_DONT_CARE = $FFFFFFFF;

  { GLX_RENDER_TYPE bits  }
  GLX_RGBA_BIT = $00000001;
  GLX_COLOR_INDEX_BIT = $00000002;
  GLX_RGBA_BIT_SGIX = GLX_RGBA_BIT;
  GLX_COLOR_INDEX_BIT_SGIX = GLX_COLOR_INDEX_BIT;

  { GLX_DRAWABLE_TYPE bits  }
  GLX_WINDOW_BIT = $00000001;
  GLX_PIXMAP_BIT = $00000002;
  GLX_PBUFFER_BIT = $00000004;
  GLX_WINDOW_BIT_SGIX = GLX_WINDOW_BIT;
  GLX_PIXMAP_BIT_SGIX = GLX_PIXMAP_BIT;
  GLX_PBUFFER_BIT_SGIX = GLX_PBUFFER_BIT;

  { GLX_CONFIG_CAVEAT attribute values  }
  GLX_NONE = $8000;
  GLX_SLOW_CONFIG = $8001;
  GLX_NON_CONFORMANT_CONFIG = $800D;

  { GLX_X_VISUAL_TYPE attribute values  }
  GLX_TRUE_COLOR = $8002;
  GLX_DIRECT_COLOR = $8003;
  GLX_PSEUDO_COLOR = $8004;
  GLX_STATIC_COLOR = $8005;
  GLX_GRAY_SCALE = $8006;
  GLX_STATIC_GRAY = $8007;

  { GLX_TRANSPARENT_TYPE attribute values  }
  { #define GLX_NONE                        0x8000  }
  GLX_TRANSPARENT_RGB = $8008;
  GLX_TRANSPARENT_INDEX = $8009;

  { glXCreateGLXPbuffer attributes  }
  GLX_PRESERVED_CONTENTS = $801B;
  GLX_LARGEST_PBUFFER = $801C;
  { New for GLX 1.3  }
  GLX_PBUFFER_HEIGHT = $8040;
  { New for GLX 1.3  }
  GLX_PBUFFER_WIDTH = $8041;
  GLX_PRESERVED_CONTENTS_SGIX = GLX_PRESERVED_CONTENTS;
  GLX_LARGEST_PBUFFER_SGIX = GLX_LARGEST_PBUFFER;

  { glXQueryGLXPBuffer attributes  }
  GLX_WIDTH = $801D;
  GLX_HEIGHT = $801E;
  GLX_EVENT_MASK = $801F;
  GLX_WIDTH_SGIX = GLX_WIDTH;
  GLX_HEIGHT_SGIX = GLX_HEIGHT;
  GLX_EVENT_MASK_SGIX = GLX_EVENT_MASK;

  { glXCreateNewContext render_type attribute values  }
  GLX_RGBA_TYPE = $8014;
  GLX_COLOR_INDEX_TYPE = $8015;
  GLX_RGBA_TYPE_SGIX = GLX_RGBA_TYPE;
  GLX_COLOR_INDEX_TYPE_SGIX = GLX_COLOR_INDEX_TYPE;

  { glXQueryContext attributes  }
  { #define GLX_FBCONFIG_ID                0x8013  }
  { #define GLX_RENDER_TYPE                0x8011  }
  GLX_SCREEN = $800C;

  { glXSelectEvent event mask bits  }
  GLX_PBUFFER_CLOBBER_MASK = $08000000;
  GLX_PBUFFER_CLOBBER_MASK_SGIX = GLX_PBUFFER_CLOBBER_MASK;

  { GLXPbufferClobberEvent event_type values  }
  GLX_DAMAGED = $8020;
  GLX_SAVED = $8021;
  GLX_DAMAGED_SGIX = GLX_DAMAGED;
  GLX_SAVED_SGIX = GLX_SAVED;

  { GLXPbufferClobberEvent draw_type values  }
  GLX_WINDOW = $8022;
  GLX_PBUFFER = $8023;
  GLX_WINDOW_SGIX = GLX_WINDOW;
  GLX_PBUFFER_SGIX = GLX_PBUFFER;

  { GLXPbufferClobberEvent buffer_mask bits  }
  GLX_FRONT_LEFT_BUFFER_BIT = $00000001;
  GLX_FRONT_RIGHT_BUFFER_BIT = $00000002;
  GLX_BACK_LEFT_BUFFER_BIT = $00000004;
  GLX_BACK_RIGHT_BUFFER_BIT = $00000008;
  GLX_AUX_BUFFERS_BIT = $00000010;
  GLX_DEPTH_BUFFER_BIT = $00000020;
  GLX_STENCIL_BUFFER_BIT = $00000040;
  GLX_ACCUM_BUFFER_BIT = $00000080;
  GLX_FRONT_LEFT_BUFFER_BIT_SGIX = GLX_FRONT_LEFT_BUFFER_BIT;
  GLX_FRONT_RIGHT_BUFFER_BIT_SGIX = GLX_FRONT_RIGHT_BUFFER_BIT;
  GLX_BACK_LEFT_BUFFER_BIT_SGIX = GLX_BACK_LEFT_BUFFER_BIT;
  GLX_BACK_RIGHT_BUFFER_BIT_SGIX = GLX_BACK_RIGHT_BUFFER_BIT;
  GLX_AUX_BUFFERS_BIT_SGIX = GLX_AUX_BUFFERS_BIT;
  GLX_DEPTH_BUFFER_BIT_SGIX = GLX_DEPTH_BUFFER_BIT;
  GLX_STENCIL_BUFFER_BIT_SGIX = GLX_STENCIL_BUFFER_BIT;
  GLX_ACCUM_BUFFER_BIT_SGIX = GLX_ACCUM_BUFFER_BIT;


{ ------------------------------------------------------------------------------
   Extension return values from glXGetConfig.  These are also
   accepted as parameter values for glXChooseVisual.
}
  { visual_info extension type  }
  GLX_X_VISUAL_TYPE_EXT = $22;
  { visual_info extension  }
  GLX_TRANSPARENT_TYPE_EXT = $23;
  { visual_info extension  }
  GLX_TRANSPARENT_INDEX_VALUE_EXT = $24;
  { visual_info extension  }
  GLX_TRANSPARENT_RED_VALUE_EXT = $25;
  { visual_info extension  }
  GLX_TRANSPARENT_GREEN_VALUE_EXT = $26;
  { visual_info extension  }
  GLX_TRANSPARENT_BLUE_VALUE_EXT = $27;
  { visual_info extension  }
  GLX_TRANSPARENT_ALPHA_VALUE_EXT = $28;

  { Property values for visual_type  }
  GLX_TRUE_COLOR_EXT = $8002;
  GLX_DIRECT_COLOR_EXT = $8003;
  GLX_PSEUDO_COLOR_EXT = $8004;
  GLX_STATIC_COLOR_EXT = $8005;
  GLX_GRAY_SCALE_EXT = $8006;
  GLX_STATIC_GRAY_EXT = $8007;

  { Property values for transparent pixel  }
  GLX_NONE_EXT = $8000;
  GLX_TRANSPARENT_RGB_EXT = $8008;
  GLX_TRANSPARENT_INDEX_EXT = $8009;

  { Property values for visual_rating  }
  { visual_rating extension type  }
  GLX_VISUAL_CAVEAT_EXT = $20;
  GLX_SLOW_VISUAL_EXT = $8001;
  GLX_NON_CONFORMANT_VISUAL_EXT = $800D;


{ ------------------------------------------------------------------------------
   Names for attributes to glXGetClientString.
}
  GLX_VENDOR = $1;
  GLX_VERSION = $2;
  GLX_EXTENSIONS = $3;


{ ------------------------------------------------------------------------------
   Names for attributes to glXQueryContextInfoEXT.
}
  { id of share context  }
  GLX_SHARE_CONTEXT_EXT = $800A;
  { id of context's visual  }
  GLX_VISUAL_ID_EXT = $800B;
  { screen number  }
  GLX_SCREEN_EXT = $800C;
  { GLX Extension Strings  }
  GLX_EXT_import_context = 1;
  GLX_EXT_visual_info = 1;
  GLX_EXT_visual_rating = 1;
  GLX_SGIX_fbconfig = 1;
  GLX_SGIX_pbuffer = 1;


type
{-------------------------------------------------------------------------------
   GLX resources.
}
  TXID = dword;

  PGLXContextID = ^GLXContextID;
  GLXContextID = TXID;

  PGLXPixmap = ^GLXPixmap;
  GLXPixmap = TXID;

  PGLXDrawable = ^GLXDrawable;
  GLXDrawable = TXID;

  PGLXPbuffer = ^GLXPbuffer;
  GLXPbuffer = TXID;

  PGLXWindow = ^GLXWindow;
  GLXWindow = TXID;

  PGLXFBConfigID = ^GLXFBConfigID;
  GLXFBConfigID = TXID;
  
{-------------------------------------------------------------------------------
   GLXContext is a pointer to opaque data.
}
  TGLXContext = pointer; // nvidia knows __GLXcontextRec, but I didn't found it
  
{-------------------------------------------------------------------------------
   GLXFBConfig is a pointer to opaque data.
}
  PGLXFBConfig = ^TGLXFBConfig;
  TGLXFBConfig = pointer; // nvidia knows __GLXFBConfigRec, but I didn't find it

  PGLXFBConfigSGIX = ^TGLXFBConfigSGIX;
  TGLXFBConfigSGIX = pointer; // nvidia knows __GLXFBConfigRec, but I didn't find it
  
{------------------------------------------------------------------------------}

function glXChooseVisual(dpy:PDisplay; screen:longint; attribList:Plongint):PXVisualInfo; cdecl; external;
procedure glXCopyContext(dpy:PDisplay; src:TGLXContext; dst:TGLXContext; mask:GLuint); cdecl; external;
function glXCreateContext(dpy:PDisplay; vis:PXVisualInfo; shareList:TGLXContext; direct:TBool):TGLXContext; cdecl; external;
function glXCreateGLXPixmap(dpy:PDisplay; vis:PXVisualInfo; pixmap:TPixmap):GLXPixmap; cdecl; external;
procedure glXDestroyContext(dpy:PDisplay; ctx:TGLXContext); cdecl; external;
procedure glXDestroyGLXPixmap(dpy:PDisplay; pix:GLXPixmap); cdecl; external;
function glXGetConfig(dpy:PDisplay; vis:PXVisualInfo; attrib:longint; value:Plongint):longint; cdecl; external;
function glXGetCurrentContext:TGLXContext; cdecl; external;
function glXGetCurrentDrawable:GLXDrawable; cdecl; external;
function glXIsDirect(dpy:PDisplay; ctx:TGLXContext):TBool; cdecl; external;
function glXMakeCurrent(dpy:PDisplay; drawable:GLXDrawable; ctx:TGLXContext):TBool; cdecl; external;
function glXQueryExtension(dpy:PDisplay; errorBase:Plongint; eventBase:Plongint):TBool; cdecl; external;
function glXQueryVersion(dpy:PDisplay; major:Plongint; minor:Plongint):TBool; cdecl; external;
procedure glXSwapBuffers(dpy:PDisplay; drawable:GLXDrawable); cdecl; external;
procedure glXUseXFont(font:TFont; first:longint; count:longint; listBase:longint); cdecl; external;
procedure glXWaitGL; cdecl; external;
procedure glXWaitX; cdecl; external;
function glXGetClientString(dpy:PDisplay; name:longint):Pchar; cdecl; external;
function glXQueryServerString(dpy:PDisplay; screen:longint; name:longint):Pchar; cdecl; external;
function glXQueryExtensionsString(dpy:PDisplay; screen:longint):Pchar; cdecl; external;

{ New for GLX 1.3 ------------------------------------------------------------ }
function glXGetFBConfigs(dpy:PDisplay; screen:longint; nelements:Plongint):PGLXFBConfig; cdecl; external;
function glXChooseFBConfig(dpy:PDisplay; screen:longint; attrib_list:Plongint; nelements:Plongint):PGLXFBConfig; cdecl; external;
function glXGetFBConfigAttrib(dpy:PDisplay; config:TGLXFBConfig; attribute:longint; value:Plongint):longint; cdecl; external;
function glXGetVisualFromFBConfig(dpy:PDisplay; config:TGLXFBConfig):PXVisualInfo; cdecl; external;
function glXCreateWindow(dpy:PDisplay; config:TGLXFBConfig; win:TWindow; attrib_list:Plongint):GLXWindow; cdecl; external;
procedure glXDestroyWindow(dpy:PDisplay; win:GLXWindow); cdecl; external;
function glXCreatePixmap(dpy:PDisplay; config:TGLXFBConfig; pixmap:TPixmap; attrib_list:Plongint):GLXPixmap; cdecl; external;
procedure glXDestroyPixmap(dpy:PDisplay; pixmap:GLXPixmap); cdecl; external;
function glXCreatePbuffer(dpy:PDisplay; config:TGLXFBConfig; attrib_list:Plongint):GLXPbuffer; cdecl; external;
procedure glXDestroyPbuffer(dpy:PDisplay; pbuf:GLXPbuffer); cdecl; external;
procedure glXQueryDrawable(dpy:PDisplay; draw:GLXDrawable; attribute:longint; value:Pdword); cdecl; external;
function glXCreateNewContext(dpy:PDisplay; config:TGLXFBConfig; render_type:longint; share_list:TGLXContext; direct:TBool):TGLXContext; cdecl; external;
function glXMakeContextCurrent(display:PDisplay; draw:GLXDrawable; read:GLXDrawable; ctx:TGLXContext):TBool; cdecl; external;
function glXGetCurrentReadDrawable:GLXDrawable; cdecl; external;
function glXGetCurrentDisplay:PDisplay; cdecl; external;
function glXQueryContext(dpy:PDisplay; ctx:TGLXContext; attribute:longint; value:Plongint):longint; cdecl; external;
procedure glXSelectEvent(dpy:PDisplay; draw:GLXDrawable; event_mask:dword); cdecl; external;
procedure glXGetSelectedEvent(dpy:PDisplay; draw:GLXDrawable; event_mask:Pdword); cdecl; external;

{   Extensions  ---------------------------------------------------------------}
function glXGetContextIDEXT(ctx:TGLXContext):GLXContextID; cdecl; external;
function glXGetCurrentDrawableEXT:GLXDrawable; cdecl; external;
function glXImportContextEXT(dpy:PDisplay; contextID:GLXContextID):TGLXContext; cdecl; external;
procedure glXFreeContextEXT(dpy:PDisplay; ctx:TGLXContext); cdecl; external;
function glXQueryContextInfoEXT(dpy:PDisplay; ctx:TGLXContext; attribute:longint; value:Plongint):longint; cdecl; external;

function glXGetProcAddressARB(procName:PGLubyte): TProcedure; cdecl; external;

function glXAllocateMemoryNV(size:GLsizei; readfreq:GLfloat; writefreq:GLfloat; priority:GLfloat):pointer; cdecl; external;
procedure glXFreeMemoryNV(pointer:PGLvoid); cdecl; external;

{ Extensions added for the SGI, SGIS and SGIX extensions ----------------------}
{ Video Sync extension }
function glXGetVideoSyncSGI(count:Pdword):longint; cdecl; external;
function glXWaitVideoSyncSGI(divisor:longint; remainder:longint; count:Pdword):longint; cdecl; external;
function glXGetRefreshRateSGI(rate:Pdword):longint; cdecl; external;

{ Swap Group extension }
procedure glXJoinSwapGroupSGIX(dpy:PDisplay; drawable:GLXDrawable; member:GLXDrawable); cdecl; external;

{ Swap Barrier extension }
procedure glXBindSwapBarrierSGIX(dpy:PDisplay; drawable:GLXDrawable; barrier:longint); cdecl; external;
function glXQueryMaxSwapBarriersSGIX(dpy:PDisplay; screen:longint; max:Plongint):TBool; cdecl; external;

{ SGIX FBConfig extension }
function glXGetFBConfigAttribSGIX(dpy:PDisplay; config:TGLXFBConfigSGIX; attribute:longint; value_return:Plongint):longint; cdecl; external;
function glXChooseFBConfigSGIX(dpy:PDisplay; screen:longint; attribList:Plongint; nitems:Plongint):PGLXFBConfigSGIX; cdecl; external;
function glXCreateGLXPixmapWithConfigSGIX(dpy:PDisplay; config:TGLXFBConfigSGIX; pixmap:TPixmap; attribList:Plongint):GLXPixmap; cdecl; external;
function glXCreateContextWithConfigSGIX(dpy:PDisplay; config:TGLXFBConfigSGIX; renderType:longint; shareList:TGLXContext; allowDirect:TBool):TGLXContext; cdecl; external;
function glXGetVisualFromFBConfigSGIX(dpy:PDisplay; config:TGLXFBConfigSGIX):PXVisualInfo; cdecl; external;
function glXGetFBConfigFromVisualSGIX(dpy:PDisplay; vis:PXVisualInfo):TGLXFBConfigSGIX; cdecl; external;
function glXCreateGLXPbufferSGIX(dpy:PDisplay; config:TGLXFBConfig; width:dword; height:dword; attribList:Plongint):GLXPbuffer; cdecl; external;
procedure glXDestroyGLXPbufferSGIX(dpy:PDisplay; pbuf:GLXPbuffer); cdecl; external;
procedure glXQueryGLXPbufferSGIX(dpy:PDisplay; pbuf:GLXPbuffer; attribute:longint; value:Pdword); cdecl; external;
procedure glXSelectEventSGIX(dpy:PDisplay; drawable:GLXDrawable; mask:dword); cdecl; external;
procedure glXGetSelectedEventSGIX(dpy:PDisplay; drawable:GLXDrawable; mask:Pdword); cdecl; external;

{-------------------------------------------------------------------------------
  GLX Events
}
type
  PGLXPbufferClobberEvent = ^GLXPbufferClobberEvent;
  GLXPbufferClobberEvent = record
    event_type : longint;   { GLX_DAMAGED or GLX_SAVED  }
    draw_type : longint;    { GLX_WINDOW or GLX_PBUFFER  }
    serial : dword;         { # of last request processed by server  }
    send_event : TBool;     { true if this came for SendEvent request  }
    display : PDisplay;     { display the event was read from  }
    drawable : GLXDrawable; { XID of Drawable  }
    buffer_mask : dword;    { mask indicating which buffers are affected  }
    aux_buffer : dword;     { which aux buffer was affected  }
    x : longint;
    y : longint;
    width : longint;
    height : longint;
    count : longint;        { if nonzero, at least this many more  }
  end;

  P__GLXEvent = ^T__GLXEvent;
  T__GLXEvent = record
    case longint of
      0 : ( glxpbufferclobber : GLXPbufferClobberEvent );
      1 : ( pad : array[0..23] of longint );
    end;
  TGLXEvent = T__GLXEvent;
  PGLXEvent = ^TGLXEvent;


implementation

end.

