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
{-------------------------------------------------------------------------------
   GLX resources.
}
type
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

