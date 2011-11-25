{
** Copyright (c) 2007-2009 The Khronos Group Inc.
**
** Permission is hereby granted, free of charge, to any person obtaining a
** copy of this software and/or associated documentation files (the
** "Materials"), to deal in the Materials without restriction, including
** without limitation the rights to use, copy, modify, merge, publish,
** distribute, sublicense, and/or sell copies of the Materials, and to
** permit persons to whom the Materials are furnished to do so, subject to
** the following conditions:
**
** The above copyright notice and this permission notice shall be included
** in all copies or substantial portions of the Materials.
**
** THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
** MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
}

unit egl;

interface

uses ctypes
     {$IF DEFINED(WINDOWS)}
     ,windows
     {$ELSEIF DEFINED(ANDROID)}
     {$ELSEIF DEFINED(UNIX)}
     ,x,xlib,xutil
     {$ENDIF};

const
  LibName = 'libEGL.so';

type
  EGLBoolean = cuint;
  EGLenum = cuint;
  EGLint = cint;
  
  PEGLint = ^EGLint; 

type
  EGLConfig = Pointer;
  EGLContext = Pointer;
  EGLDisplay = Pointer;
  EGLSurface = Pointer;
  EGLClientBuffer = Pointer;

  PEGLConfig = ^EGLConfig;

{$IF defined(WINDOWS)}
  EGLNativeDisplayType = HDC;
  EGLNativePixmapType = HBITMAP;
  EGLNativeWindowType = HWINDOW;
{$ELSEIF defined(SYMBIAN)}
  EGLNativeDisplayType = cint;
  EGLNativePixmapType = Pointer;
  EGLNativeWindowType = Pointer;
{$ELSEIF defined(ANDROID)}
  EGLNativeDisplayType = Pointer;
  EGLNativePixmapType = Pointer;
  EGLNativeWindowType = Pointer;
{$ELSEIF defined(UNIX)}
  EGLNativeDisplayType = PDisplay;
  EGLNativePixmapType = TPixmap;
  EGLNativeWindowType = TWindow;
{$ENDIF}

  NativeDisplayType = EGLNativeDisplayType;
  NativePixmapType = EGLNativePixmapType;
  NativeWindowType = EGLNativeWindowType;

  (* EGL Versioning  *)

const
  EGL_VERSION_1_0 = 1;
  EGL_VERSION_1_1 = 1;
  EGL_VERSION_1_2 = 1;
  EGL_VERSION_1_3 = 1;
  EGL_VERSION_1_4 = 1;
  (* EGL Enumerants. Bitmasks and other exceptional cases aside, most
   * enums are assigned unique values starting at 0x3000.
   *)

  (* EGL aliases  *)

  EGL_FALSE = 0;
  EGL_TRUE = 1;
  (* Out-of-band handle values  *)

  EGL_DEFAULT_DISPLAY = EGLNativeDisplayType(0);
  EGL_NO_CONTEXT = EGLContext(0);
  EGL_NO_DISPLAY = EGLDisplay(0);
  EGL_NO_SURFACE = EGLSurface(0);
  (* Out-of-band attribute value  *)

  EGL_DONT_CARE = -1;
  (* Errors / GetError return values  *)

  EGL_SUCCESS = $3000;
  EGL_NOT_INITIALIZED = $3001;
  EGL_BAD_ACCESS = $3002;
  EGL_BAD_ALLOC = $3003;
  EGL_BAD_ATTRIBUTE = $3004;
  EGL_BAD_CONFIG = $3005;
  EGL_BAD_CONTEXT = $3006;
  EGL_BAD_CURRENT_SURFACE = $3007;
  EGL_BAD_DISPLAY = $3008;
  EGL_BAD_MATCH = $3009;
  EGL_BAD_NATIVE_PIXMAP = $300A;
  EGL_BAD_NATIVE_WINDOW = $300B;
  EGL_BAD_PARAMETER = $300C;
  EGL_BAD_SURFACE = $300D;
  EGL_CONTEXT_LOST = $300E;  (* EGL 1.1 - IMG_power_management  *)
  (* Reserved 0x300F-0x301F for additional errors  *)

  (* Config attributes  *)

  EGL_BUFFER_SIZE = $3020;
  EGL_ALPHA_SIZE = $3021;
  EGL_BLUE_SIZE = $3022;
  EGL_GREEN_SIZE = $3023;
  EGL_RED_SIZE = $3024;
  EGL_DEPTH_SIZE = $3025;
  EGL_STENCIL_SIZE = $3026;
  EGL_CONFIG_CAVEAT = $3027;
  EGL_CONFIG_ID = $3028;
  EGL_LEVEL = $3029;
  EGL_MAX_PBUFFER_HEIGHT = $302A;
  EGL_MAX_PBUFFER_PIXELS = $302B;
  EGL_MAX_PBUFFER_WIDTH = $302C;
  EGL_NATIVE_RENDERABLE = $302D;
  EGL_NATIVE_VISUAL_ID = $302E;
  EGL_NATIVE_VISUAL_TYPE = $302F;
  EGL_SAMPLES = $3031;
  EGL_SAMPLE_BUFFERS = $3032;
  EGL_SURFACE_TYPE = $3033;
  EGL_TRANSPARENT_TYPE = $3034;
  EGL_TRANSPARENT_BLUE_VALUE = $3035;
  EGL_TRANSPARENT_GREEN_VALUE = $3036;
  EGL_TRANSPARENT_RED_VALUE = $3037;
  EGL_NONE = $3038;  (* Attrib list terminator  *)
  EGL_BIND_TO_TEXTURE_RGB = $3039;
  EGL_BIND_TO_TEXTURE_RGBA = $303A;
  EGL_MIN_SWAP_INTERVAL = $303B;
  EGL_MAX_SWAP_INTERVAL = $303C;
  EGL_LUMINANCE_SIZE = $303D;
  EGL_ALPHA_MASK_SIZE = $303E;
  EGL_COLOR_BUFFER_TYPE = $303F;
  EGL_RENDERABLE_TYPE = $3040;
  EGL_MATCH_NATIVE_PIXMAP = $3041;  (* Pseudo-attribute (not queryable)  *)
  EGL_CONFORMANT = $3042;
  (* Reserved 0x3041-0x304F for additional config attributes  *)

  (* Config attribute values  *)

  EGL_SLOW_CONFIG = $3050;  (* EGL_CONFIG_CAVEAT value  *)
  EGL_NON_CONFORMANT_CONFIG = $3051;  (* EGL_CONFIG_CAVEAT value  *)
  EGL_TRANSPARENT_RGB = $3052;  (* EGL_TRANSPARENT_TYPE value  *)
  EGL_RGB_BUFFER = $308E;  (* EGL_COLOR_BUFFER_TYPE value  *)
  EGL_LUMINANCE_BUFFER = $308F;  (* EGL_COLOR_BUFFER_TYPE value  *)
  (* More config attribute values, for EGL_TEXTURE_FORMAT  *)

  EGL_NO_TEXTURE = $305C;
  EGL_TEXTURE_RGB = $305D;
  EGL_TEXTURE_RGBA = $305E;
  EGL_TEXTURE_2D = $305F;
  (* Config attribute mask bits  *)

  EGL_PBUFFER_BIT = $0001;  (* EGL_SURFACE_TYPE mask bits  *)
  EGL_PIXMAP_BIT = $0002;  (* EGL_SURFACE_TYPE mask bits  *)
  EGL_WINDOW_BIT = $0004;  (* EGL_SURFACE_TYPE mask bits  *)
  EGL_VG_COLORSPACE_LINEAR_BIT = $0020;  (* EGL_SURFACE_TYPE mask bits  *)
  EGL_VG_ALPHA_FORMAT_PRE_BIT = $0040;  (* EGL_SURFACE_TYPE mask bits  *)
  EGL_MULTISAMPLE_RESOLVE_BOX_BIT = $0200;  (* EGL_SURFACE_TYPE mask bits  *)
  EGL_SWAP_BEHAVIOR_PRESERVED_BIT = $0400;  (* EGL_SURFACE_TYPE mask bits  *)
  EGL_OPENGL_ES_BIT = $0001;  (* EGL_RENDERABLE_TYPE mask bits  *)
  EGL_OPENVG_BIT = $0002;  (* EGL_RENDERABLE_TYPE mask bits  *)
  EGL_OPENGL_ES2_BIT = $0004;  (* EGL_RENDERABLE_TYPE mask bits  *)
  EGL_OPENGL_BIT = $0008;  (* EGL_RENDERABLE_TYPE mask bits  *)
  (* QueryString targets  *)

  EGL_VENDOR = $3053;
  EGL_VERSION = $3054;
  EGL_EXTENSIONS = $3055;
  EGL_CLIENT_APIS = $308D;
  (* QuerySurface / SurfaceAttrib / CreatePbufferSurface targets  *)

  EGL_HEIGHT = $3056;
  EGL_WIDTH = $3057;
  EGL_LARGEST_PBUFFER = $3058;
  EGL_TEXTURE_FORMAT = $3080;
  EGL_TEXTURE_TARGET = $3081;
  EGL_MIPMAP_TEXTURE = $3082;
  EGL_MIPMAP_LEVEL = $3083;
  EGL_RENDER_BUFFER = $3086;
  EGL_VG_COLORSPACE = $3087;
  EGL_VG_ALPHA_FORMAT = $3088;
  EGL_HORIZONTAL_RESOLUTION = $3090;
  EGL_VERTICAL_RESOLUTION = $3091;
  EGL_PIXEL_ASPECT_RATIO = $3092;
  EGL_SWAP_BEHAVIOR = $3093;
  EGL_MULTISAMPLE_RESOLVE = $3099;
  (* EGL_RENDER_BUFFER values / BindTexImage / ReleaseTexImage buffer targets  *)

  EGL_BACK_BUFFER = $3084;
  EGL_SINGLE_BUFFER = $3085;
  (* OpenVG color spaces  *)

  EGL_VG_COLORSPACE_sRGB = $3089;  (* EGL_VG_COLORSPACE value  *)
  EGL_VG_COLORSPACE_LINEAR = $308A;  (* EGL_VG_COLORSPACE value  *)
  (* OpenVG alpha formats  *)

  EGL_VG_ALPHA_FORMAT_NONPRE = $308B;  (* EGL_ALPHA_FORMAT value  *)
  EGL_VG_ALPHA_FORMAT_PRE = $308C;  (* EGL_ALPHA_FORMAT value  *)
  (* Constant scale factor by which fractional display resolutions &
 * aspect ratio are scaled when queried as integer values.
  *)

  EGL_DISPLAY_SCALING = 10000;
  (* Unknown display resolution/aspect ratio  *)

  EGL_UNKNOWN = -1;
  (* Back buffer swap behaviors  *)

  EGL_BUFFER_PRESERVED = $3094;  (* EGL_SWAP_BEHAVIOR value  *)
  EGL_BUFFER_DESTROYED = $3095;  (* EGL_SWAP_BEHAVIOR value  *)
  (* CreatePbufferFromClientBuffer buffer types  *)

  EGL_OPENVG_IMAGE = $3096;
  (* QueryContext targets  *)

  EGL_CONTEXT_CLIENT_TYPE = $3097;
  (* CreateContext attributes  *)

  EGL_CONTEXT_CLIENT_VERSION = $3098;
  (* Multisample resolution behaviors  *)

  EGL_MULTISAMPLE_RESOLVE_DEFAULT = $309A;  (* EGL_MULTISAMPLE_RESOLVE value  *)
  EGL_MULTISAMPLE_RESOLVE_BOX = $309B;  (* EGL_MULTISAMPLE_RESOLVE value  *)
  (* BindAPI/QueryAPI targets  *)

  EGL_OPENGL_ES_API = $30A0;
  EGL_OPENVG_API = $30A1;
  EGL_OPENGL_API = $30A2;
  (* GetCurrentSurface targets  *)

  EGL_DRAW = $3059;
  EGL_READ = $305A;
  (* WaitNative engines  *)

  EGL_CORE_NATIVE_ENGINE = $305B;
  (* EGL 1.2 tokens renamed for consistency in EGL 1.3  *)

  EGL_COLORSPACE = EGL_VG_COLORSPACE;
  EGL_ALPHA_FORMAT = EGL_VG_ALPHA_FORMAT;
  EGL_COLORSPACE_sRGB = EGL_VG_COLORSPACE_sRGB;
  EGL_COLORSPACE_LINEAR = EGL_VG_COLORSPACE_LINEAR;
  EGL_ALPHA_FORMAT_NONPRE = EGL_VG_ALPHA_FORMAT_NONPRE;
  EGL_ALPHA_FORMAT_PRE = EGL_VG_ALPHA_FORMAT_PRE;
  (* EGL extensions must request enum blocks from the Khronos
 * API Registrar, who maintains the enumerant registry. Submit
 * a bug in Khronos Bugzilla against task "Registry".
  *)


  (* EGL Functions  *)

function eglGetError: EGLint; cdecl; external LibName;

function eglGetDisplay(display_id: EGLNativeDisplayType): EGLDisplay; cdecl; external LibName;
function eglInitialize(dpy: EGLDisplay; major, minor: PEGLint): EGLBoolean; cdecl; external LibName;
function eglTerminate(dpy: EGLDisplay): EGLBoolean; cdecl; external LibName;

function eglQueryString(dpy: EGLDisplay; name_: EGLint): Pchar; cdecl; external LibName;

function eglGetConfigs(dpy: EGLDisplay; configs: PEGLConfig; config_size: EGLint; num_config: PEGLint): EGLBoolean; cdecl; external LibName;
function eglChooseConfig(dpy: EGLDisplay; attrib_list: PEGLint; configs: PEGLConfig; config_size: EGLint; num_config: PEGLint): EGLBoolean; cdecl; external LibName;
function eglGetConfigAttrib(dpy: EGLDisplay; config: EGLConfig; attribute: EGLint; value: PEGLint): EGLBoolean; cdecl; external LibName;
function eglCreateWindowSurface(dpy: EGLDisplay; config: EGLConfig; win: EGLNativeWindowType; attrib_list: PEGLint): EGLSurface; cdecl; external LibName;
function eglCreatePbufferSurface(dpy: EGLDisplay; config: EGLConfig; attrib_list: PEGLint): EGLSurface; cdecl; external LibName;
function eglCreatePixmapSurface(dpy: EGLDisplay; config: EGLConfig; pixmap: EGLNativePixmapType; attrib_list: PEGLint): EGLSurface; cdecl; external LibName;
function eglDestroySurface(dpy: EGLDisplay; surface: EGLSurface): EGLBoolean; cdecl; external LibName;
function eglQuerySurface(dpy: EGLDisplay; surface: EGLSurface; attribute: EGLint; value: PEGLint): EGLBoolean; cdecl; external LibName;
function eglBindAPI(api: EGLenum): EGLBoolean; cdecl; external LibName;
function eglQueryAPI: EGLenum; cdecl; external LibName;
function eglWaitClient: EGLBoolean; cdecl; external LibName;
function eglReleaseThread: EGLBoolean; cdecl; external LibName;
function eglCreatePbufferFromClientBuffer(dpy: EGLDisplay; buftype: EGLenum; buffer: EGLClientBuffer; config: EGLConfig; attrib_list: PEGLint): EGLSurface; cdecl; external LibName;
function eglSurfaceAttrib(dpy: EGLDisplay; surface: EGLSurface; attribute, value: EGLint): EGLBoolean; cdecl; external LibName;
function eglBindTexImage(dpy: EGLDisplay; surface: EGLSurface; buffer: EGLint): EGLBoolean; cdecl; external LibName;
function eglReleaseTexImage(dpy: EGLDisplay; surface: EGLSurface; buffer: EGLint): EGLBoolean; cdecl; external LibName;

function eglSwapInterval(dpy: EGLDisplay; interval: EGLint): EGLBoolean; cdecl; external LibName;

function eglCreateContext(dpy: EGLDisplay; config: EGLConfig; share_context: EGLContext; attrib_list: PEGLint): EGLContext; cdecl; external LibName;
function eglDestroyContext(dpy: EGLDisplay; ctx: EGLContext): EGLBoolean; cdecl; external LibName;
function eglMakeCurrent(dpy: EGLDisplay; draw, read_: EGLSurface; ctx: EGLContext): EGLBoolean; cdecl; external LibName;
function eglGetCurrentContext: EGLContext; cdecl; external LibName;
function eglGetCurrentSurface(readdraw: EGLint): EGLSurface; cdecl; external LibName;
function eglGetCurrentDisplay: EGLDisplay; cdecl; external LibName;
function eglQueryContext(dpy: EGLDisplay; ctx: EGLContext; attribute: EGLint; value: PEGLint): EGLBoolean; cdecl; external LibName;
function eglWaitGL: EGLBoolean; cdecl; external LibName;
function eglWaitNative(engine: EGLint): EGLBoolean; cdecl; external LibName;
function eglSwapBuffers(dpy: EGLDisplay; surface: EGLSurface): EGLBoolean; cdecl; external LibName;
function eglCopyBuffers(dpy: EGLDisplay; surface: EGLSurface; target: EGLNativePixmapType): EGLBoolean; cdecl; external LibName;

(* This is a generic function pointer type, whose name indicates it must
 * be cast to the proper type *and calling convention* before use.
  *)

type
  __eglMustCastToProperFunctionPointerType = procedure; cdecl;

function eglGetProcAddress(procname: Pchar): __eglMustCastToProperFunctionPointerType; cdecl; external LibName;

implementation

end.
