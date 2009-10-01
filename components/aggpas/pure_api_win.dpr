{target:win}
//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 pure_api_win;

uses
 Windows ,Messages ,

 agg_basics ,
 agg_rendering_buffer ,
 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgba ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline_p ,
 agg_render_scanlines ;

{$I agg_mode.inc }

const
 szWindowClass = 'PURE_API';
 szTitle       = 'pure_api';

{ WndProc }
function WndProc(Wnd : HWND; Msg : UINT; WPar : WParam; LPar : LParam ) : LResult; stdcall;
var
// Win32
 dc : HDC;
 ps : TPaintStruct;
 rt : TRect;

 width  ,
 height : integer;

 bmp_info : TBitmapInfo;
 mem_dc   : HDC;

 buf  : pointer;
 bmp  ,
 temp : HBitmap;

// AGG
 rbuf : rendering_buffer;
 pixf : pixel_formats;
 renb : renderer_base;
 rgba : aggclr;

 ren : renderer_scanline_aa_solid;
 ras : rasterizer_scanline_aa;
 sl  : scanline_p8;

begin
 result:=0;

 case Msg of
  WM_PAINT :
   begin
    dc:=BeginPaint(Wnd ,ps );

    if dc <> 0 then
     begin
      GetClientRect(Wnd ,rt );

      width :=rt.Right - rt.Left;
      height:=rt.Bottom - rt.Top;

     //Creating compatible DC and a bitmap to render the image
      mem_dc:=CreateCompatibleDC(dc );

      bmp_info.bmiHeader.biSize  :=sizeof(TBITMAPINFOHEADER );
      bmp_info.bmiHeader.biWidth :=width;
      bmp_info.bmiHeader.biHeight:=height;
      bmp_info.bmiHeader.biPlanes:=1;

      bmp_info.bmiHeader.biBitCount     :=32;
      bmp_info.bmiHeader.biCompression  :=BI_RGB;
      bmp_info.bmiHeader.biSizeImage    :=0;
      bmp_info.bmiHeader.biXPelsPerMeter:=0;
      bmp_info.bmiHeader.biYPelsPerMeter:=0;
      bmp_info.bmiHeader.biClrUsed      :=0;
      bmp_info.bmiHeader.biClrImportant :=0;

      buf:=NIL;
      bmp:=CreateDIBSection(mem_dc ,bmp_info ,DIB_RGB_COLORS ,buf ,0 ,0 );

     // Selecting the object before doing anything allows you
     // to use AGG together with native Windows GDI.
      temp:=SelectObject(mem_dc ,bmp );

     //============================================================
     // AGG lowest level code
      rbuf.Construct;
      rbuf.attach(buf ,width ,height ,-width * 4 ); // Use negative stride in order
                                                    // to keep Y-axis consistent with
                                                    // WinGDI, i.e., going down.

     // Pixel format and basic primitives renderer
      pixfmt_bgra32(pixf ,@rbuf );

      renb.Construct(@pixf );
      rgba.ConstrInt(255 ,255 ,255 ,127 );

      renb.clear(@rgba );

     // Scanline renderer for solid filling
      ren.Construct(@renb );

     // Rasterizer & scanline
      ras.Construct;
      sl.Construct;

     // Polygon (triangle)
      ras.move_to_d(20.7   ,34.15  );
      ras.line_to_d(398.23 ,123.43 );
      ras.line_to_d(165.45 ,401.87 );

     // Setting the attrribute (color) & Rendering
      rgba.ConstrInt(80 ,90 ,60 );
      ren.color_    (@rgba );

      render_scanlines(@ras ,@sl ,@ren );

     //------------------------------------------------------------
     // Display the image. If the image is B-G-R-A (32-bits per pixel)
     // one can use AlphaBlend instead of BitBlt. In case of AlphaBlend
     // one also should clear the image with zero alpha, i.e. rgba8(0,0,0,0)
      BitBlt(
       dc ,
       rt.left ,rt.top ,width ,height ,
       mem_dc ,0 ,0 ,
       SRCCOPY );

     // Free AGG resources
     // Pascal doesn't call object destructors when leaving the local function range,
     // thus we have to do it by hand
      rbuf.Destruct;
      ras.Destruct;
      sl.Destruct;

     // Free resources
      SelectObject(mem_dc ,temp );
      DeleteObject(bmp );
      DeleteObject(mem_dc );

      EndPaint(Wnd ,ps );

     end;

   end;

  WM_SYSKEYDOWN ,WM_KEYDOWN :
   case WPar of
    VK_F1 :
     MessageBox(
      Wnd ,
      'The AGG library is able to draw to any surface. It is achieved by using an offline'#13 +
      'bitmap (buffer), to which AGG primarily renders. Then that bitmap is blited to the'#13 +
      'GDI device context of the destination device.'#13#13 +
      'This example demonstrates that simple setup for a Windows app that paints to a GDI '#13 +
      'device context. All it needs are the CreateCompatibleBitmap(), CreateDIBSection()'#13 +
      'and BitBlt() WinAPI calls.'#0 ,
      'AGG Message' ,MB_OK );

    else
     result:=DefWindowProc(Wnd ,Msg ,WPar ,LPar );

   end;

  WM_ERASEBKGND :
   NoP;

  WM_DESTROY :
   PostQuitMessage(0 );

  else
   result:=DefWindowProc(Wnd ,Msg ,WPar ,LPar );

 end;

end;

{ MyRegisterClass }
procedure MyRegisterClass;
var
 wcex : TWndClassEx;

begin
 wcex.cbSize:=sizeof(TWndClassEx );

 wcex.style        :=CS_HREDRAW or CS_VREDRAW;
 wcex.lpfnWndProc  :=@WndProc;
 wcex.cbClsExtra   :=0;
 wcex.cbWndExtra   :=0;
 wcex.hInstance    :=HInstance;
 wcex.hIcon        :=LoadIcon(HInstance ,IDI_APPLICATION );
 wcex.hCursor      :=LoadCursor(0 ,IDC_ARROW );
 wcex.hbrBackground:=COLOR_WINDOW + 1;
 wcex.lpszMenuName :=NIL;
 wcex.lpszClassName:=szWindowClass;
 wcex.hIconSm      :=0;

 RegisterClassEx(wcex );

end;

{ InitInstance }
function InitInstance : boolean;
var
 Wnd : HWND;

begin
 Wnd:=
  CreateWindow(
   szWindowClass ,
   szTitle ,
   WS_OVERLAPPEDWINDOW ,
   integer(CW_USEDEFAULT ) ,0 ,integer(CW_USEDEFAULT ) ,0 ,
   0 ,0 ,HInstance ,NIL );

 if Wnd <> 0 then
  begin
   ShowWindow  (Wnd,SW_SHOW );
   UpdateWindow(Wnd );

   result:=true;

  end
 else
  result:=false;

end;

var
 Msg : TMsg;

BEGIN
 MyRegisterClass;

 if InitInstance then
  while GetMessage(Msg ,0 ,0 ,0 ) do
   begin
    TranslateMessage(Msg );
    DispatchMessage (Msg );

   end;

END.
