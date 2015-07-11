//----------------------------------------------------------------------------
// Anti-Grain Geometry - Version 2.4 (Public License)
// Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)
//
// Anti-Grain Geometry - Version 2.4 Release Milano 3 (AggPas 2.4 RM3)
// Pascal Port By: Milan Marusinec alias Milano
//                 milan@marusinec.sk
//                 http://www.aggpas.org
// Copyright (c) 2005-2006
//
// Permission to copy, use, modify, sell and distribute this software
// is granted provided this copyright notice appears in all copies.
// This software is provided "as is" without express or implied
// warranty, and with no claim as to its suitability for any purpose.
//
//----------------------------------------------------------------------------
// Contact: mcseem@antigrain.com
//          mcseemagg@yahoo.com
//          http://www.antigrain.com
//
//----------------------------------------------------------------------------
//
// class pixel_map
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 15.12.2005-Milano: Unit port establishment
//
{ agg_win32_bmp.pas }
unit
 agg_win32_bmp ;

INTERFACE

{$I agg_mode.inc }

uses
 Windows ,
 agg_basics ;

{ TYPES DEFINITION }
const
 org_mono8   = 8;
 org_color16 = 16;
 org_color24 = 24;
 org_color32 = 32;
 org_color48 = 48;
 org_color64 = 64;

type
 pixel_map = object
   m_bmp : PBITMAPINFO;
   m_buf : pointer;
   m_bpp : unsigned;

   m_is_internal : boolean;

   m_img_size  ,
   m_full_size : unsigned;

   constructor Construct;
   destructor  Destruct;

   procedure destroy;
   procedure create(width_ ,height_ ,org : unsigned; clear_val : unsigned = 256 );

   function  load_from_bmp(var fd : file ) : boolean; overload;
   function  load_from_bmp(filename : shortstring ) : boolean; overload;

   function  save_as_bmp(var fd : file ) : boolean; overload;
   function  save_as_bmp(filename : shortstring ) : boolean; overload;

   procedure draw(h_dc : HDC; device_rect : PRect = NIL; bmp_rect : PRect = NIL ); overload;
   procedure draw(h_dc : HDC; x ,y : int; scale : double = 1.0 ); overload;

   function  _buf : pointer;
   function  _width : unsigned;
   function  _height : unsigned;
   function  _stride : int;
   function  _bpp : unsigned;

   function  create_bitmap_info       (width_ ,height_ ,bits_per_pixel : unsigned ) : PBITMAPINFO;
   procedure create_gray_scale_palette(bmp : PBITMAPINFO );

   function  calc_full_size  (bmp : PBITMAPINFO ) : unsigned;
   function  calc_header_size(bmp : PBITMAPINFO ) : unsigned;
   function  calc_img_ptr    (bmp : PBITMAPINFO ) : unsigned;

   function  calc_palette_size(clr_used ,bits_per_pixel : unsigned ) : unsigned; overload;
   function  calc_palette_size(bmp : PBITMAPINFO ) : unsigned; overload;

   function  calc_row_len   (width_ ,bits_per_pixel : unsigned ) : unsigned;
   procedure create_from_bmp(bmp : PBITMAPINFO );

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor pixel_map.Construct;
begin
 m_bmp:=NIL;
 m_buf:=NIL;
 m_bpp:=0;

 m_is_internal:=false;

 m_img_size :=0;
 m_full_size:=0;

end;

{ DESTRUCT }
destructor pixel_map.Destruct;
begin
 destroy;

end;

{ DESTROY }
procedure pixel_map.destroy;
begin
 if (m_bmp <> NIL ) and
    m_is_internal then
  agg_freemem(pointer(m_bmp ) ,m_full_size );

 m_bmp:=NIL;
 m_buf:=NIL;

 m_is_internal:=false;

end;

{ CREATE }
procedure pixel_map.create;
begin
 destroy;

 if width_ = 0 then
  width_:=1;

 if height_ = 0 then
  height_:=1;

 m_bpp:=org;

 create_from_bmp(create_bitmap_info(width_ ,height_ ,m_bpp ) );

 create_gray_scale_palette(m_bmp );

 m_is_internal:=true;

 if clear_val <= 255 then
  fillchar(m_buf^ ,m_img_size ,clear_val );

end;

{ LOAD_FROM_BMP }
function pixel_map.load_from_bmp(var fd : file ) : boolean;
var
 sz : int;

 bmf : BITMAPFILEHEADER;
 bmi : PBITMAPINFO;

 bmp_size : unsigned;

label
 bmperr;

begin
 blockread(fd ,bmf ,sizeof(bmf ) );

 if bmf.bfType <> $4D42 then
  goto bmperr;

 bmp_size:=bmf.bfSize - sizeof(BITMAPFILEHEADER );

 agg_getmem(pointer(bmi ) ,bmp_size );
 blockread (fd ,bmi^ ,bmp_size ,sz );

 if sz <> bmp_size then
  goto bmperr;

 destroy;

 m_bpp:=bmi.bmiHeader.biBitCount;

 create_from_bmp(bmi );

 m_is_internal:=true; 

 result:=true;

 exit;

bmperr:
 if bmi <> NIL then
  agg_freemem(pointer(bmi ) ,bmp_size ); 

 result:=false;

end;

{ LOAD_FROM_BMP }
function pixel_map.load_from_bmp(filename : shortstring ) : boolean;
var
 fd  : file;
 err : integer;
 ret : boolean;

begin
{$I- }
 err:=ioresult;

 assignfile(fd ,filename );
 reset     (fd ,1 );

 err:=ioresult;
 ret:=false;

 if err = 0 then
  begin
   ret:=load_from_bmp(fd );

   close(fd );

  end;

 result:=ret;

end;

{ SAVE_AS_BMP }
function pixel_map.save_as_bmp(var fd : file ) : boolean;
var
 bmf : BITMAPFILEHEADER;

begin
 if m_bmp = NIL then
  result:=false

 else
  begin
   bmf.bfType     :=$4D42;
   bmf.bfOffBits  :=calc_header_size(m_bmp ) + sizeof(bmf );
   bmf.bfSize     :=bmf.bfOffBits + m_img_size;
   bmf.bfReserved1:=0;
   bmf.bfReserved2:=0;

   blockwrite(fd ,bmf ,sizeof(bmf ) );
   blockwrite(fd ,m_bmp^ ,m_full_size );

   result:=true;

  end;

end;

{ SAVE_AS_BMP }
function pixel_map.save_as_bmp(filename : shortstring ) : boolean;
var
 fd  : file;
 err : integer;
 ret : boolean;

begin
{$I- }
 err:=ioresult;

 assignfile(fd ,filename );
 rewrite   (fd ,1 );

 err:=ioresult;
 ret:=false;

 if err = 0 then
  begin
   ret:=save_as_bmp(fd );

   close(fd );

  end;

 result:=ret;

end;

{ DRAW }
procedure pixel_map.draw(h_dc : HDC; device_rect : PRect = NIL; bmp_rect : PRect = NIL );
var
 bmp_x ,
 bmp_y ,

 bmp_width  ,
 bmp_height ,

 dvc_x ,
 dvc_y ,

 dvc_width  ,
 dvc_height : unsigned;

 err : int;
 bok : boolean;

 compdc : HDC;
 handle ,
 backup : HBITMAP;
 bminfo : TBitmapInfo;
 buffer : pointer;

 rinc ,rgap ,size ,stride : int;

begin
 if (m_bmp = NIL ) or
    (m_buf = NIL ) then
  exit;

 bmp_x:=0;
 bmp_y:=0;

 bmp_width :=m_bmp.bmiHeader.biWidth;
 bmp_height:=m_bmp.bmiHeader.biHeight;

 dvc_x:=0;
 dvc_y:=0;

 dvc_width :=m_bmp.bmiHeader.biWidth;
 dvc_height:=m_bmp.bmiHeader.biHeight;

 if bmp_rect <> NIL then
  begin
   bmp_x     :=bmp_rect.left;
   bmp_y     :=bmp_rect.top;
   bmp_width :=bmp_rect.right  - bmp_rect.left;
   bmp_height:=bmp_rect.bottom - bmp_rect.top;

  end;

 dvc_x     :=bmp_x;
 dvc_x     :=bmp_x;
 dvc_width :=bmp_width;
 dvc_height:=bmp_height;

 if device_rect <> NIL then
  begin
   dvc_x     :=device_rect.left;
   dvc_y     :=device_rect.top;
   dvc_width :=device_rect.right  - device_rect.left;
   dvc_height:=device_rect.bottom - device_rect.top;

  end;

 if (dvc_width <> bmp_width ) or
    (dvc_height <> bmp_height ) then
  begin
   SetStretchBltMode(h_dc ,COLORONCOLOR );

   StretchDIBits(
    h_dc ,            // handle of device context
    dvc_x ,           // x-coordinate of upper-left corner of source rect.
    dvc_y ,           // y-coordinate of upper-left corner of source rect.
    dvc_width ,       // width of source rectangle
    dvc_height ,      // height of source rectangle
    bmp_x ,
    bmp_y ,           // x, y -coordinates of upper-left corner of dest. rect.
    bmp_width ,       // width of destination rectangle
    bmp_height ,      // height of destination rectangle
    m_buf ,           // address of bitmap bits
    m_bmp^ ,          // address of bitmap data
    DIB_RGB_COLORS ,  // usage
    SRCCOPY );        // raster operation code

  end
 else
  begin
   err:=SetDIBitsToDevice(
    h_dc ,            // handle to device context
    dvc_x ,           // x-coordinate of upper-left corner of
    dvc_y ,           // y-coordinate of upper-left corner of
    dvc_width ,       // source rectangle width
    dvc_height ,      // source rectangle height
    bmp_x ,           // x-coordinate of lower-left corner of
    bmp_y ,           // y-coordinate of lower-left corner of
    0 ,               // first scan line in array
    bmp_height ,      // number of scan lines
    m_buf ,           // address of array with DIB bits
    m_bmp^ ,          // address of structure with bitmap info.
    DIB_RGB_COLORS ); // RGB or palette indexes

  {hack}  
   if err = 0 then
    begin
     compdc:=CreateCompatibleDC(h_dc );

     if compdc <> 0 then
      begin
       fillchar(bminfo ,sizeof(TBitmapInfoHeader ) ,0 );

       bminfo.bmiHeader.biSize       :=m_bmp.bmiHeader.biSize;
       bminfo.bmiHeader.biCompression:=m_bmp.bmiHeader.biCompression;

       bminfo.bmiHeader.biPlanes  :=m_bmp.bmiHeader.biPlanes;
       bminfo.bmiHeader.biBitCount:=m_bmp.bmiHeader.biBitCount;

       bminfo.bmiHeader.biWidth :=m_bmp.bmiHeader.biWidth;
       bminfo.bmiHeader.biHeight:=m_bmp.bmiHeader.biHeight;

       handle:=CreateDIBSection(compdc ,bminfo ,DIB_RGB_COLORS ,buffer ,0 ,0 );
       stride:=_stride;

       rinc:=((bminfo.bmiHeader.biWidth * bminfo.bmiHeader.biBitCount + 31 ) shr 5 ) shl 2;
       rgap:=bminfo.bmiHeader.biWidth mod 4;
       size:=rinc * bminfo.bmiHeader.biHeight;

       if handle <> 0 then
        begin
         backup:=SelectObject(compdc ,handle );

         if (rinc = stride ) and
            (size = m_img_size ) then
          begin
           move(m_buf^ ,buffer^ ,size );

           bok:=BitBlt(
            h_dc ,dvc_x ,dvc_y ,dvc_width ,dvc_height ,
            compdc ,bmp_x ,bmp_y ,
            SRCCOPY );

          end
         else
          MessageBox(0 ,'Cannot draw - different format !' ,'pixel_map.draw message' ,MB_OK );

         if backup <> 0 then
          SelectObject(compdc ,backup );

         DeleteObject(handle );

        end;

       DeleteDC(compdc );

      end;

    end;

  end;

end;

{ DRAW }
procedure pixel_map.draw(h_dc : HDC; x ,y : int; scale : double = 1.0 );
var
 rect : TRect;

 width_  ,
 height_ : unsigned;

begin
 if (m_bmp = NIL ) or
    (m_buf = NIL ) then
  exit;

 width_ :=trunc(m_bmp.bmiHeader.biWidth * scale );
 height_:=trunc(m_bmp.bmiHeader.biHeight * scale );

 rect.left  :=x;
 rect.top   :=y;
 rect.right :=x + width_;
 rect.bottom:=y + height_;

 draw(h_dc ,@rect );

end;

{ _BUF }
function pixel_map._buf;
begin
 result:=m_buf;

end;

{ _WIDTH }
function pixel_map._width;
begin
 result:=m_bmp.bmiHeader.biWidth;

end;

{ _HEIGHT }
function pixel_map._height;
begin
 result:=m_bmp.bmiHeader.biHeight;

end;

{ _STRIDE }
function pixel_map._stride;
begin
 result:=calc_row_len(m_bmp.bmiHeader.biWidth ,m_bmp.bmiHeader.biBitCount );

end;

{ _BPP }
function pixel_map._bpp;
begin
 result:=m_bpp;

end;

{ CALC_FULL_SIZE }
function pixel_map.calc_full_size;
begin
 if bmp = NIL then
  result:=0
 else
  result:=
   sizeof(TBITMAPINFOHEADER ) +
   sizeof(RGBQUAD ) * calc_palette_size(bmp ) +
   bmp.bmiHeader.biSizeImage;

end;

{ CALC_HEADER_SIZE }
function pixel_map.calc_header_size;
begin
 if bmp = NIL then
  result:=0
 else
  result:=sizeof(TBITMAPINFOHEADER ) + sizeof(RGBQUAD ) * calc_palette_size(bmp );

end;

{ CALC_IMG_PTR }
function pixel_map.calc_img_ptr;
begin
 if bmp = NIL then
  result:=0
 else
  result:=ptrcomp(bmp ) + calc_header_size(bmp );

end;

{ CREATE_BITMAP_INFO }
function pixel_map.create_bitmap_info;
var
 bmp : PBITMAPINFO;

 line_len  ,
 img_size  ,
 rgb_size  ,
 full_size : unsigned;

begin
 line_len :=calc_row_len(width_ ,bits_per_pixel );
 img_size :=line_len * height_;
 rgb_size :=calc_palette_size(0 ,bits_per_pixel ) * sizeof(RGBQUAD );
 full_size:=sizeof(TBITMAPINFOHEADER ) + rgb_size + img_size;

 agg_getmem(pointer(bmp ) ,full_size );
 fillchar  (bmp^ ,full_size ,0 );

 bmp.bmiHeader.biSize         :=sizeof(TBITMAPINFOHEADER );
 bmp.bmiHeader.biWidth        :=width_;
 bmp.bmiHeader.biHeight       :=height_;
 bmp.bmiHeader.biPlanes       :=1;
 bmp.bmiHeader.biBitCount     :=bits_per_pixel;
 bmp.bmiHeader.biCompression  :=0;
 bmp.bmiHeader.biSizeImage    :=img_size;
 bmp.bmiHeader.biXPelsPerMeter:=0;
 bmp.bmiHeader.biYPelsPerMeter:=0;
 bmp.bmiHeader.biClrUsed      :=0;
 bmp.bmiHeader.biClrImportant :=0;

 result:=bmp;

end;

{ CREATE_GRAY_SCALE_PALETTE }
procedure pixel_map.create_gray_scale_palette;
var
 rgb : PRGBQUAD;

 i ,rgb_size ,
 brightness  : unsigned;

begin
 if bmp = NIL then
  exit;

 rgb_size:=calc_palette_size(bmp );

 rgb:=PRGBQUAD(ptrcomp(bmp ) + sizeof(TBITMAPINFOHEADER ) );

 if rgb_size > 0 then
  for i:=0 to rgb_size - 1 do
   begin
    brightness:=trunc((255 * i ) / (rgb_size - 1 ) );

    rgb.rgbBlue :=brightness;
    rgb.rgbGreen:=brightness;
    rgb.rgbRed  :=brightness;

    rgb.rgbReserved:=0;

    inc(ptrcomp(rgb ) ,sizeof(RGBQUAD ) );

   end;

end;

{ CALC_PALETTE_SIZE }
function pixel_map.calc_palette_size(clr_used ,bits_per_pixel : unsigned ) : unsigned;
var
 palette_size : int;

begin
 palette_size:=0;

 if bits_per_pixel <= 8 then
  begin
   palette_size:=clr_used;

   if palette_size = 0 then
    palette_size:=1 shl bits_per_pixel;

  end;

 result:=palette_size;

end;

{ CALC_PALETTE_SIZE }
function pixel_map.calc_palette_size(bmp : PBITMAPINFO ) : unsigned;
begin
 if bmp = NIL then
  result:=0
 else
  result:=calc_palette_size(bmp.bmiHeader.biClrUsed ,bmp.bmiHeader.biBitCount );

end;

{ CALC_ROW_LEN }
function pixel_map.calc_row_len;
var
 n ,k : unsigned;

begin
 n:=width_;

 case bits_per_pixel of
  1 :
   begin
    k:=n;
    n:=n shr 3;

    if k and 7 <> 0 then
     inc(n );

   end;

  4 :
   begin
    k:=n;
    n:=n shr 1;

    if k and 3 <> 0 then
     inc(n );

   end;

  8 : NoP;

  16 : n:=n * 2;
  
  24 : n:=n * 3;

  32 : n:=n * 4;

  48 : n:=n * 6;

  64 : n:=n * 8;

  else
   n:=0;

 end;

 result:=((n + 3 ) shr 2 ) shl 2;

end;

{ CREATE_FROM_BMP }
procedure pixel_map.create_from_bmp;
begin
 if bmp <> NIL then
  begin
   m_img_size:=
    calc_row_len(
     bmp.bmiHeader.biWidth ,
     bmp.bmiHeader.biBitCount ) * bmp.bmiHeader.biHeight;

   m_full_size:=calc_full_size(bmp );

   m_bmp:=bmp;
   m_buf:=pointer(calc_img_ptr(bmp ) );

  end;

end;

END.

