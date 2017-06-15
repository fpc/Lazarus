{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

}
unit GLWin32WGLContext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LMessages, Windows, LCLProc, LCLType, gl, Forms, Controls, 
  Win32Int, WSLCLClasses, WSControls, Win32WSControls, Win32Proc, LCLMessageGlue;

procedure LOpenGLViewport(Handle: HWND; Left, Top, Width, Height: integer);
procedure LOpenGLSwapBuffers(Handle: HWND);
function LOpenGLMakeCurrent(Handle: HWND): boolean;
function LOpenGLReleaseContext(Handle: HWND): boolean;
function LOpenGLCreateContext(AWinControl: TWinControl;
                    WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
                    DoubleBuffered, RGBA, DebugContext: boolean;
                    const RedBits, GreenBits, BlueBits,
                    MultiSampling, AlphaBits, DepthBits, StencilBits, AUXBuffers: Cardinal;
                    const AParams: TCreateParams): HWND;
procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);

procedure InitWGL(RequireWGL_ARB_create_context : boolean);
procedure InitOpenGLContextGLWindowClass;


type
  TWGLControlInfo = record
    Window: HWND;
    DC: HDC;
    PixelFormat: GLUInt;
    WGLContext: HGLRC;
  end;
  PWGLControlInfo = ^TWGLControlInfo;

var
  WGLControlInfoAtom: ATOM = 0;

function AllocWGLControlInfo(Window: HWND): PWGLControlInfo;
function DisposeWGLControlInfo(Window: HWND): boolean;
function GetWGLControlInfo(Window: HWND): PWGLControlInfo;


const
  WGL_SAMPLE_BUFFERS_ARB                           = $2041;
  WGL_SAMPLES_ARB                                  = $2042;

  // WGL_ARB_pixel_format
  WGL_NUMBER_PIXEL_FORMATS_ARB                     = $2000;
  WGL_DRAW_TO_WINDOW_ARB                           = $2001;
  WGL_DRAW_TO_BITMAP_ARB                           = $2002;
  WGL_ACCELERATION_ARB                             = $2003;
  WGL_NEED_PALETTE_ARB                             = $2004;
  WGL_NEED_SYSTEM_PALETTE_ARB                      = $2005;
  WGL_SWAP_LAYER_BUFFERS_ARB                       = $2006;
  WGL_SWAP_METHOD_ARB                              = $2007;
  WGL_NUMBER_OVERLAYS_ARB                          = $2008;
  WGL_NUMBER_UNDERLAYS_ARB                         = $2009;
  WGL_TRANSPARENT_ARB                              = $200A;
  WGL_TRANSPARENT_RED_VALUE_ARB                    = $2037;
  WGL_TRANSPARENT_GREEN_VALUE_ARB                  = $2038;
  WGL_TRANSPARENT_BLUE_VALUE_ARB                   = $2039;
  WGL_TRANSPARENT_ALPHA_VALUE_ARB                  = $203A;
  WGL_TRANSPARENT_INDEX_VALUE_ARB                  = $203B;
  WGL_SHARE_DEPTH_ARB                              = $200C;
  WGL_SHARE_STENCIL_ARB                            = $200D;
  WGL_SHARE_ACCUM_ARB                              = $200E;
  WGL_SUPPORT_GDI_ARB                              = $200F;
  WGL_SUPPORT_OPENGL_ARB                           = $2010;
  WGL_DOUBLE_BUFFER_ARB                            = $2011;
  WGL_STEREO_ARB                                   = $2012;
  WGL_PIXEL_TYPE_ARB                               = $2013;
  WGL_COLOR_BITS_ARB                               = $2014;
  WGL_RED_BITS_ARB                                 = $2015;
  WGL_RED_SHIFT_ARB                                = $2016;
  WGL_GREEN_BITS_ARB                               = $2017;
  WGL_GREEN_SHIFT_ARB                              = $2018;
  WGL_BLUE_BITS_ARB                                = $2019;
  WGL_BLUE_SHIFT_ARB                               = $201A;
  WGL_ALPHA_BITS_ARB                               = $201B;
  WGL_ALPHA_SHIFT_ARB                              = $201C;
  WGL_ACCUM_BITS_ARB                               = $201D;
  WGL_ACCUM_RED_BITS_ARB                           = $201E;
  WGL_ACCUM_GREEN_BITS_ARB                         = $201F;
  WGL_ACCUM_BLUE_BITS_ARB                          = $2020;
  WGL_ACCUM_ALPHA_BITS_ARB                         = $2021;
  WGL_DEPTH_BITS_ARB                               = $2022;
  WGL_STENCIL_BITS_ARB                             = $2023;
  WGL_AUX_BUFFERS_ARB                              = $2024;
  WGL_NO_ACCELERATION_ARB                          = $2025;
  WGL_GENERIC_ACCELERATION_ARB                     = $2026;
  WGL_FULL_ACCELERATION_ARB                        = $2027;
  WGL_SWAP_EXCHANGE_ARB                            = $2028;
  WGL_SWAP_COPY_ARB                                = $2029;
  WGL_SWAP_UNDEFINED_ARB                           = $202A;
  WGL_TYPE_RGBA_ARB                                = $202B;
  WGL_TYPE_COLORINDEX_ARB                          = $202C;

  // WGL_NV_float_buffer
  WGL_FLOAT_COMPONENTS_NV                          = $20B0;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_R_NV         = $20B1;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RG_NV        = $20B2;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGB_NV       = $20B3;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGBA_NV      = $20B4;
  WGL_TEXTURE_FLOAT_R_NV                           = $20B5;
  WGL_TEXTURE_FLOAT_RG_NV                          = $20B6;
  WGL_TEXTURE_FLOAT_RGB_NV                         = $20B7;
  WGL_TEXTURE_FLOAT_RGBA_NV                        = $20B8;

  // WGL_ARB_pbuffer
type
  HPBUFFERARB = Integer;
  TGLenum = uint;

const
  WGL_DRAW_TO_PBUFFER_ARB                          = $202D;
  WGL_MAX_PBUFFER_PIXELS_ARB                       = $202E;
  WGL_MAX_PBUFFER_WIDTH_ARB                        = $202F;
  WGL_MAX_PBUFFER_HEIGHT_ARB                       = $2030;
  WGL_PBUFFER_LARGEST_ARB                          = $2033;
  WGL_PBUFFER_WIDTH_ARB                            = $2034;
  WGL_PBUFFER_HEIGHT_ARB                           = $2035;
  WGL_PBUFFER_LOST_ARB                             = $2036;

  // WGL_ARB_buffer_region
  WGL_FRONT_COLOR_BUFFER_BIT_ARB                   = $00000001;
  WGL_BACK_COLOR_BUFFER_BIT_ARB                    = $00000002;
  WGL_DEPTH_BUFFER_BIT_ARB                         = $00000004;
  WGL_STENCIL_BUFFER_BIT_ARB                       = $00000008;

  WGL_CONTEXT_FLAGS_ARB                            = $2094;
  WGL_CONTEXT_DEBUG_BIT_ARB                        = $0001;

const
  opengl32 = 'OpenGL32.dll';
  glu32 = 'GLU32.dll';

type
  PWGLSwap = ^TWGLSwap;
  _WGLSWAP = packed record
    hdc: HDC;
    uiFlags: UINT;
  end;
  TWGLSwap = _WGLSWAP;
  WGLSWAP = _WGLSWAP;

  function wglGetProcAddress(ProcName: PChar): Pointer; stdcall; external opengl32;
  function wglCopyContext(p1: HGLRC; p2: HGLRC; p3: Cardinal): BOOL; stdcall; external opengl32;
  function wglCreateContext(DC: HDC): HGLRC; stdcall; external opengl32;
  function wglCreateLayerContext(p1: HDC; p2: Integer): HGLRC; stdcall; external opengl32;
  function wglDeleteContext(p1: HGLRC): BOOL; stdcall; external opengl32;
  function wglDescribeLayerPlane(p1: HDC; p2, p3: Integer; p4: Cardinal; var p5: TLayerPlaneDescriptor): BOOL; stdcall; external opengl32;
  function wglGetCurrentContext: HGLRC; stdcall; external opengl32;
  function wglGetCurrentDC: HDC; stdcall; external opengl32;
  function wglGetLayerPaletteEntries(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall; external opengl32;
  function wglMakeCurrent(DC: HDC; p2: HGLRC): BOOL; stdcall; external opengl32;
  function wglRealizeLayerPalette(p1: HDC; p2: Integer; p3: BOOL): BOOL; stdcall; external opengl32;
  function wglSetLayerPaletteEntries(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall; external opengl32;
  function wglShareLists(p1, p2: HGLRC): BOOL; stdcall; external opengl32;
  function wglSwapLayerBuffers(p1: HDC; p2: Cardinal): BOOL; stdcall; external opengl32;
  function wglUseFontBitmapsA(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32;
  function wglUseFontOutlinesA (p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32;
  function wglUseFontBitmapsW(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32;
  function wglUseFontOutlinesW (p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32;
  function wglUseFontBitmaps(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32 name 'wglUseFontBitmapsA';
  function wglUseFontOutlines(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32 name 'wglUseFontOutlinesA';

var
  // WGL Extensions ----------------------------
  WGL_EXT_swap_control: boolean;
  WGL_ARB_multisample: boolean;
  WGL_ARB_extensions_string: boolean;
  WGL_ARB_pixel_format: boolean;
  WGL_ARB_pbuffer: boolean;
  WGL_ARB_buffer_region: boolean;
  WGL_ATI_pixel_format_float: boolean;


  // ARB wgl extensions
  wglCreateContextAttribsARB : function (DC: HDC; hShareContext:HGLRC; attribList:PInteger ):HGLRC;stdcall;
  wglGetExtensionsStringARB: function(DC: HDC): PChar; stdcall;
  wglGetPixelFormatAttribivARB: function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: TGLenum;
    const piAttributes: PGLint; piValues : PGLint) : BOOL; stdcall;
  wglGetPixelFormatAttribfvARB: function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: TGLenum;
    const piAttributes: PGLint; piValues: PGLFloat) : BOOL; stdcall;
  wglChoosePixelFormatARB: function(DC: HDC; const piAttribIList: PGLint; const pfAttribFList: PGLFloat;
    nMaxFormats: GLint; piFormats: PGLint; nNumFormats: PGLenum) : BOOL; stdcall;
  wglCreatePbufferARB: function(DC: HDC; iPixelFormat: Integer; iWidth, iHeight : Integer;
    const piAttribList: PGLint) : HPBUFFERARB; stdcall;
  wglGetPbufferDCARB: function(hPbuffer: HPBUFFERARB) : HDC; stdcall;
  wglReleasePbufferDCARB: function(hPbuffer: HPBUFFERARB; DC: HDC) : Integer; stdcall;
  wglDestroyPbufferARB: function(hPbuffer: HPBUFFERARB): BOOL; stdcall;
  wglQueryPbufferARB: function(hPbuffer: HPBUFFERARB; iAttribute : Integer;
    piValue: PGLint) : BOOL; stdcall;

  wglCreateBufferRegionARB: function(DC: HDC; iLayerPlane: Integer; uType: TGLenum) : Integer; stdcall;
  wglDeleteBufferRegionARB: procedure(hRegion: Integer); stdcall;
  wglSaveBufferRegionARB: function(hRegion: Integer; x, y, width, height: Integer): BOOL; stdcall;
  wglRestoreBufferRegionARB: function(hRegion: Integer; x, y, width, height: Integer;
    xSrc, ySrc: Integer): BOOL; stdcall;

  // non-ARB wgl extensions
  wglSwapIntervalEXT: function(interval : Integer) : BOOL; stdcall;
  wglGetSwapIntervalEXT: function : Integer; stdcall;

var
  WGLInitialized: boolean = false;
  OpenGLContextWindowClassInitialized: boolean = false;
  OpenGLContextWindowClass: WNDCLASS;

const
  DefaultOpenGLContextInitAttrList: array [0..0] of LongInt = (
    0
    );

implementation
uses glext;

function GLGetProcAddress(ProcName: PChar):Pointer;
begin
  Result := wglGetProcAddress(ProcName);
end;

procedure LOpenGLViewport(Handle: HWND; Left, Top, Width, Height: integer);
begin
  glViewport(Left,Top,Width,Height);
end;

procedure LOpenGLSwapBuffers(Handle: HWND);
var
  Info: PWGLControlInfo;
begin
  Info:=GetWGLControlInfo(Handle);
  // don't use wglSwapLayerBuffers or wglSwapBuffers!
  SwapBuffers(Info^.DC);
end;

function LOpenGLMakeCurrent(Handle: HWND): boolean;
var
  Info: PWGLControlInfo;
begin
  Info:=GetWGLControlInfo(Handle);
  Result:=wglMakeCurrent(Info^.DC,Info^.WGLContext);
end;

function LOpenGLReleaseContext(Handle: HWND): boolean;
begin
  Result:=wglMakeCurrent(0,0);
end;

function GlWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  PaintMsg   : TLMPaint;
  winctrl    : TWinControl;
begin
  case Msg of 
    WM_ERASEBKGND: begin
      Result:=0;
    end;
    WM_PAINT: begin 
      winctrl := GetWin32WindowInfo(Window)^.WinControl;
      if Assigned(winctrl) then begin
        FillChar(PaintMsg, SizeOf(PaintMsg), 0);
        PaintMsg.Msg := LM_PAINT;
        PaintMsg.DC := WParam;
        DeliverMessage(winctrl, PaintMsg);
        Result:=PaintMsg.Result;
      end else 
        Result:=WindowProc(Window, Msg, WParam, LParam);
    end;
  else
    Result:=WindowProc(Window, Msg, WParam, LParam);
  end;
end;

var
  Temp_h_GLRc: HGLRC;
  Temp_h_Dc: HDC;
  Temp_h_Wnd: HWND;

procedure LGlMsDestroyTemporaryWindow; forward;

procedure LGlMsCreateTemporaryWindow;
var
  PixelFormat: LongInt;
  pfd: PIXELFORMATDESCRIPTOR;
begin
  Temp_h_Wnd := 0;
  Temp_h_Dc := 0;
  Temp_h_GLRc := 0;

  try
    { create Temp_H_wnd }
    Temp_H_wnd := CreateWindowEx(WS_EX_APPWINDOW or WS_EX_WINDOWEDGE,
      PChar('STATIC'),
      PChar('temporary window for wgl'),
      WS_OVERLAPPEDWINDOW or WS_CLIPSIBLINGS or WS_CLIPCHILDREN,
      0, 0, 100, 100,
      0 { no parent window }, 0 { no menu }, hInstance,
      nil);
    if Temp_H_wnd=0 then
      raise Exception.Create('LGlMsCreateTemporaryWindow CreateWindowEx failed');

    { create Temp_h_Dc }
    Temp_h_Dc := GetDC(Temp_h_Wnd);
    if Temp_h_Dc=0 then
      raise Exception.Create('LGlMsCreateTemporaryWindow GetDC failed');

    { create and set PixelFormat (must support OpenGL to be able to
      later do wglCreateContext) }
    FillChar(pfd, SizeOf(pfd), 0);
    with pfd do
    begin
      nSize := SizeOf(pfd);
      nVersion := 1;
      dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
      iPixelType := PFD_TYPE_RGBA;
      iLayerType := PFD_MAIN_PLANE;
    end;
    PixelFormat := ChoosePixelFormat(Temp_h_Dc, @pfd);
    if PixelFormat = 0 then
      raise Exception.Create('LGlMsCreateTemporaryWindow ChoosePixelFormat failed');

    if not SetPixelFormat(Temp_h_Dc, PixelFormat, @pfd) then
      raise Exception.Create('LGlMsCreateTemporaryWindow SetPixelFormat failed');

    { create and make current Temp_h_GLRc }
    Temp_h_GLRc := wglCreateContext(Temp_h_Dc);
    if Temp_h_GLRc = 0 then
      raise Exception.Create('LGlMsCreateTemporaryWindow wglCreateContext failed');

    if not wglMakeCurrent(Temp_h_Dc, Temp_h_GLRc) then
      raise Exception.Create('LGlMsCreateTemporaryWindow wglMakeCurrent failed');
  except
    { make sure to finalize all partially initialized window parts }
    LGlMsDestroyTemporaryWindow;
    raise;
  end;
end;

procedure LGlMsDestroyTemporaryWindow;
begin
  if Temp_h_GLRc <> 0 then
  begin
    wglMakeCurrent(Temp_h_Dc, 0);
    wglDeleteContext(Temp_h_GLRc);
    Temp_h_GLRc := 0;
  end;

  if Temp_h_Dc <> 0 then
  begin
    ReleaseDC(Temp_h_Wnd, Temp_h_Dc);
    Temp_h_Dc := 0;
  end;

  if Temp_h_Wnd <> 0 then
  begin
    DestroyWindow(Temp_h_Wnd);
    Temp_h_Wnd := 0;
  end;
end;

function LGlMsCreateOpenGLContextAttrList(DoubleBuffered: boolean; RGBA: boolean; 
  const RedBits, GreenBits, BlueBits, MultiSampling, AlphaBits, DepthBits,
  StencilBits, AUXBuffers: Cardinal): PInteger;
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
    Add(WGL_DRAW_TO_WINDOW_ARB); Add(GL_TRUE);
    Add(WGL_SUPPORT_OPENGL_ARB); Add(GL_TRUE);
    Add(WGL_ACCELERATION_ARB); Add(WGL_FULL_ACCELERATION_ARB);
    if DoubleBuffered then
      begin Add(WGL_DOUBLE_BUFFER_ARB); Add(GL_TRUE); end;
    Add(WGL_PIXEL_TYPE_ARB);
    if RGBA then
      Add(WGL_TYPE_RGBA_ARB)
    else
      Add(WGL_TYPE_COLORINDEX_ARB);

    Add(WGL_RED_BITS_ARB);  Add(RedBits);
    Add(WGL_GREEN_BITS_ARB);  Add(GreenBits);
    Add(WGL_BLUE_BITS_ARB);  Add(BlueBits);
    Add(WGL_COLOR_BITS_ARB);  Add(RedBits+GreenBits+BlueBits);
    Add(WGL_ALPHA_BITS_ARB);  Add(AlphaBits);
    Add(WGL_DEPTH_BITS_ARB);  Add(DepthBits);
    Add(WGL_STENCIL_BITS_ARB);  Add(StencilBits);
    Add(WGL_AUX_BUFFERS_ARB);  Add(AUXBuffers);
    if MultiSampling > 1 then
    begin
      Add(WGL_SAMPLE_BUFFERS_ARB); Add(1);
      Add(WGL_SAMPLES_ARB);        Add(MultiSampling);
    end;
    Add(0); Add(0);
  end;

begin
  Result:=nil;
  p:=0;
  CreateList;
  GetMem(Result,SizeOf(integer)*p);
  p:=0;
  CreateList;
end;

function LOpenGLCreateContext(AWinControl: TWinControl;
  WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
  DoubleBuffered, RGBA, DebugContext: boolean;
  const RedBits, GreenBits, BlueBits,
  MultiSampling, AlphaBits, DepthBits, StencilBits, AUXBuffers: Cardinal;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
  pfd: PIXELFORMATDESCRIPTOR;
  Info, SharedInfo: PWGLControlInfo;

  ReturnedFormats: UINT;
  VisualAttrList: PInteger;
  VisualAttrFloat: array [0..1] of Single;
  MsInitSuccess: WINBOOL;
  FailReason : string;
  attribList : array [0..2] of Integer;
begin
  InitWGL( DebugContext );
  //InitOpenGLContextGLWindowClass;
  
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do begin
    pClassName := @ClsName;
    WindowTitle := StrCaption;
    SubClassWndProc := @GlWindowProc;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
  
  // create info
  Info:=AllocWGLControlInfo(Result);

  // create device context
  Info^.DC := GetDC(Result);
  if Info^.DC=0 then
    raise Exception.Create('LOpenGLCreateContext GetDC failed');

  // get pixelformat
  FillChar(pfd,SizeOf(pfd),0);
  with pfd do begin
    nSize:=sizeOf(pfd);
    nVersion:=1;
    dwFlags:=PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL;
    if DoubleBuffered then
      dwFlags:=dwFlags or PFD_DOUBLEBUFFER;
    if RGBA then
      iPixelType:=PFD_TYPE_RGBA
    else
      iPixelType:=PFD_TYPE_COLORINDEX;
    cColorBits:=RedBits+GreenBits+BlueBits; // color depth
    cRedBits:=RedBits;
    cGreenBits:=GreenBits;
    cBlueBits:=BlueBits;
    cAlphaBits:=AlphaBits;
    cDepthBits:=DepthBits; // Z-Buffer
    cStencilBits:=StencilBits;
    cAuxBuffers:=AUXBuffers;
    iLayerType:=PFD_MAIN_PLANE;
  end;

  MsInitSuccess := false;
  if (MultiSampling > 1) and WGL_ARB_multisample and WGL_ARB_pixel_format
    and Assigned(wglChoosePixelFormatARB) then
  begin
    VisualAttrList := LGlMsCreateOpenGLContextAttrList(DoubleBuffered, RGBA,
      RedBits, GreenBits, BlueBits, MultiSampling, AlphaBits, DepthBits,
      StencilBits, AUXBuffers);
    try
      FillChar(VisualAttrFloat, SizeOf(VisualAttrFloat), 0);
      MsInitSuccess := wglChoosePixelFormatARB(Info^.DC, PGLint(VisualAttrList),
                         @VisualAttrFloat[0], 1, @Info^.PixelFormat, @ReturnedFormats);
    finally FreeMem(VisualAttrList) end;

    if MsInitSuccess and (ReturnedFormats >= 1) then
       SetPixelFormat(Info^.DC, Info^.PixelFormat, nil)
    else
       MsInitSuccess := false;
  end;

  if not MsInitSuccess then
  begin
    Info^.PixelFormat:=ChoosePixelFormat(Info^.DC,@pfd);
    if Info^.PixelFormat=0 then
      raise Exception.Create('LOpenGLCreateContext ChoosePixelFormat failed');

    // set pixel format in device context
    if not SetPixelFormat(Info^.DC,Info^.PixelFormat,@pfd) then
      raise Exception.Create('LOpenGLCreateContext SetPixelFormat failed');
  end;

  // create WGL context
  Info^.WGLContext:=0;
  if not DebugContext then
    begin
      Info^.WGLContext:=wglCreateContext(Info^.DC);
      FailReason:='wglCreateContext failed';
    end
    else if wglCreateContextAttribsARB = nil then
    begin
      FailReason:='wglCreateContextAttribsARB not supported';
    end
    else
    begin
      // try to create debug context
      attribList[0]:=WGL_CONTEXT_FLAGS_ARB;
      attribList[1]:=WGL_CONTEXT_DEBUG_BIT_ARB;
      attribList[2]:=0;
      Info^.WGLContext:=wglCreateContextAttribsARB(Info^.DC, 0, @attribList);
      FailReason:='wglCreateContextAttribsARB failed';
    end;

  if Info^.WGLContext=0 then
    raise Exception.CreateFmt('LOpenGLCreateContext: %s', [FailReason]);

  // share context objects
  if Assigned(SharedControl) then begin
    SharedInfo:=GetWGLControlInfo(SharedControl.Handle);
    if Assigned(SharedInfo) then wglShareLists(SharedInfo^.WGLContext, Info^.WGLContext);
  end;
end;

procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);
var
  Info: PWGLControlInfo;
begin
  if not AWinControl.HandleAllocated then exit;
  Info:=GetWGLControlInfo(AWinControl.Handle);
  if Info=nil then exit;
  if wglMakeCurrent(Info^.DC,Info^.WGLContext) then begin
    wglDeleteContext(Info^.WGLContext);
    Info^.WGLContext:=0;
  end;
  if (Info^.DC<>0) then begin
    ReleaseDC(Info^.Window,Info^.DC);
  end;
  DisposeWGLControlInfo(Info^.Window);
end;

procedure InitWGL( RequireWGL_ARB_create_context : boolean );
var
  Buffer: string;

  // Checks if the given Extension string is in Buffer.
  function CheckExtension(const extension : String) : Boolean;
  begin
    Result:=(Pos(extension, Buffer)>0);
  end;

begin
  if WGLInitialized then exit;
  WGLInitialized:=true;

  try
    { to successfully use wglGetExtensionsStringARB (to query e.g. ARB_multisample,
      needed for MultiSampling), you need to have OpenGL context 
      already initialized. We create a temporary window for this purpose. }
    LGlMsCreateTemporaryWindow;

    if wglGetCurrentContext() = 0 then
      raise Exception.Create('Context is not active');

    // ARB wgl extensions
    Pointer(wglCreateContextAttribsARB) := GLGetProcAddress('wglCreateContextAttribsARB');
    Pointer(wglGetExtensionsStringARB) := GLGetProcAddress('wglGetExtensionsStringARB');
    Pointer(wglGetPixelFormatAttribivARB) := GLGetProcAddress('wglGetPixelFormatAttribivARB');
    Pointer(wglGetPixelFormatAttribfvARB) := GLGetProcAddress('wglGetPixelFormatAttribfvARB');
    Pointer(wglChoosePixelFormatARB) := GLGetProcAddress('wglChoosePixelFormatARB');

    Pointer(wglCreatePbufferARB) := GLGetProcAddress('wglCreatePbufferARB');
    Pointer(wglGetPbufferDCARB) := GLGetProcAddress('wglGetPbufferDCARB');
    Pointer(wglReleasePbufferDCARB) := GLGetProcAddress('wglReleasePbufferDCARB');
    Pointer(wglDestroyPbufferARB) := GLGetProcAddress('wglDestroyPbufferARB');
    Pointer(wglQueryPbufferARB) := GLGetProcAddress('wglQueryPbufferARB');

    Pointer(wglCreateBufferRegionARB) := GLGetProcAddress('wglCreateBufferRegionARB');
    Pointer(wglDeleteBufferRegionARB) := GLGetProcAddress('wglDeleteBufferRegionARB');
    Pointer(wglSaveBufferRegionARB) := GLGetProcAddress('wglSaveBufferRegionARB');
    Pointer(wglRestoreBufferRegionARB) := GLGetProcAddress('wglRestoreBufferRegionARB');

    // -EGG- ----------------------------
    Pointer(wglSwapIntervalEXT) := GLGetProcAddress('wglSwapIntervalEXT');
    Pointer(wglGetSwapIntervalEXT) := GLGetProcAddress('wglGetSwapIntervalEXT');

    // ARB wgl extensions
    if Assigned(wglGetExtensionsStringARB) then
    begin
      Buffer:=wglGetExtensionsStringARB(Temp_h_Dc);
      { Writeln('WGL extensions supported: ', Buffer); }
    end else
      Buffer:='';
    WGL_ARB_multisample:=CheckExtension('WGL_ARB_multisample');
    WGL_EXT_swap_control:=CheckExtension('WGL_EXT_swap_control');
    WGL_ARB_buffer_region:=CheckExtension('WGL_ARB_buffer_region');
    WGL_ARB_extensions_string:=CheckExtension('WGL_ARB_extensions_string');
    WGL_ARB_pbuffer:=CheckExtension('WGL_ARB_pbuffer ');
    WGL_ARB_pixel_format:=CheckExtension('WGL_ARB_pixel_format');
    WGL_ATI_pixel_format_float:=CheckExtension('WGL_ATI_pixel_format_float');
  except
    on E: Exception do begin
      DebugLn('InitWGL ',E.Message);
    end;
  end;

  try
    if RequireWGL_ARB_create_context then
    begin
      if wglGetExtensionsStringARB = nil then
        raise Exception.Create('InitWGL : wglGetExtensionsStringARB = nil');
      if not CheckExtension('WGL_ARB_create_context') then
      begin
        raise Exception.CreateFmt('InitWGL : WGL_ARB_create_context not found. Version %s Renderer=%s'
                                + sLineBreak + 'Extensions found:' + sLineBreak + '%s',
            [String(glGetString(GL_VERSION)), String(glGetString(GL_RENDERER)), Buffer]);
      end;
      if wglCreateContextAttribsARB = nil then
        raise Exception.Create('InitWGL : wglCreateContextAttribsARB = nil');
    end;
  finally
    LGlMsDestroyTemporaryWindow;
  end;
end;

procedure InitOpenGLContextGLWindowClass;
begin
  if OpenGLContextWindowClassInitialized then exit;
  OpenGLContextWindowClassInitialized:=true;
  with OpenGLContextWindowClass do begin
    style:=CS_HREDRAW or CS_VREDRAW or CS_OWNDC;// Redraw On Move, And Own DC For Window
    lpfnWndProc  := @WindowProc;                // WndProc Handles Messages
    cbClsExtra   := 0;                          // No Extra Window Data
    cbWndExtra   := 0;                          // No Extra Window Data
    hInstance    := System.HInstance;           // Set The Instance
    hIcon        := LoadIcon(NULL, IDI_WINLOGO);// Load The Default Icon
    hCursor      := LoadCursor(NULL, IDC_ARROW);// Load The Arrow Pointer
    hbrBackground:= NULL;                       // No Background Required For GL
    lpszMenuName := nil;                       // We Don't Want A Menu
    lpszClassName:= 'LazOpenGLContext';         // Set The Class Name
  end;
  if RegisterClass(@OpenGLContextWindowClass)=0 then
    raise Exception.Create('registering OpenGLContextWindowClass failed');
end;

function AllocWGLControlInfo(Window: HWND): PWGLControlInfo;
begin
  New(Result);
  FillChar(Result^, sizeof(Result^), 0);
  Result^.Window := Window;
  if WGLControlInfoAtom=0 then
    WGLControlInfoAtom := Windows.GlobalAddAtom('WGLControlInfo');
  Windows.SetProp(Window, PChar(PtrUInt(WGLControlInfoAtom)), PtrUInt(Result));
end;

function DisposeWGLControlInfo(Window: HWND): boolean;
var
  Info: PWGLControlInfo;
begin
  Info := PWGLControlInfo(Windows.GetProp(Window,
                                          PChar(PtrUInt(WGLControlInfoAtom))));
  Result := Windows.RemoveProp(Window, PChar(PtrUInt(WGLControlInfoAtom)))<>0;
  if Result then begin
    Dispose(Info);
  end;
end;

function GetWGLControlInfo(Window: HWND): PWGLControlInfo;
begin
  Result:=PWGLControlInfo(Windows.GetProp(Window,
                                          PChar(PtrUInt(WGLControlInfoAtom))));
end;

end.

