//----------------------------------------------------------------------------
// AGG Contribution Pack - Gradients 1 (AGG CP - Gradients 1)
// http://milan.marusinec.sk/aggcp
//
// Anti-Grain Geometry - Version 2.4 Release Milano 3 (AggPas 2.4 RM3)
// http://www.aggpas.org
//
// Contribution Created By:
//  Milan Marusinec alias Milano
//  milan@marusinec.sk
//  Copyright (c) 2007
//
// Permission to copy, use, modify, sell and distribute this software
// is granted provided this copyright notice appears in all copies.
// This software is provided "as is" without express or implied
// warranty, and with no claim as to its suitability for any purpose.
//
// [History] -----------------------------------------------------------------
//
// 17.11.2007-Milano: Establishment
//
{ agg_span_gradient_contour.pas }
unit
 agg_span_gradient_contour ;

INTERFACE

{$I agg_mode.inc }

uses
 Math ,
 agg_basics ,
 agg_color ,
 agg_span_gradient ,
 agg_path_storage ,
 agg_bounding_rect ,
 agg_conv_curve ,
 agg_conv_stroke ,
 agg_conv_transform ,
 agg_trans_affine ,
 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_renderer_primitives ,
 agg_render_scanlines ,
 agg_rasterizer_outline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,
 agg_pixfmt ,
 agg_pixfmt_gray ;

{ GLOBAL VARIABLES & CONSTANTS }
{ TYPES DEFINITION }
type
 single_ptr = ^single;

 int_array_ptr = ^int_array;
 int_array = array[0..65535 ] of int;

 single_array_ptr = ^single_array;
 single_array = array[0..65535 ] of single;

 gradient_contour = object(gradient )
  private
   m_buffer : pointer;
   m_width  ,
   m_height ,
   m_frame  : int;

   m_d1 ,
   m_d2 : double;

  public
   constructor Construct(d_1 : double = 0; d_2 : double = 100 );
   destructor  Destruct;

   function  contour_create(ps : path_storage_ptr ) : pointer;
   function  contour_width : int;
   function  contour_height : int;

   procedure d1(d : double );
   procedure d2(d : double );

   procedure frame(f : int ); overload;
   function  frame : int; overload;

   function  calculate(x ,y ,d : int ) : int; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
const
 infinity = 1e20;

{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor gradient_contour.Construct(d_1 : double = 0; d_2 : double = 100 );
begin
 m_buffer:=NIL;
 m_width :=0;
 m_height:=0;
 m_frame :=10;

 m_d1:=d_1;
 m_d2:=d_2;

end;

{ DESTRUCT }
destructor gradient_contour.Destruct;
begin
 if m_buffer <> NIL then
  agg_freemem(m_buffer ,m_width * m_height );

end;

{ SQUARE }
function square(x : int ) : int;
begin
 result:=x * x;

end;

{ DT }
// DT algorithm by: Pedro Felzenszwalb
procedure dt(spanf ,spang ,spanr : single_array_ptr; spann : int_array_ptr; length : int );
var
 k ,q : int;

 s : single;

begin
 k:=0;

 spann[0 ]:=0;
 spang[0 ]:=-infinity;
 spang[1 ]:=+infinity;

 q:=1;

 while  q <= length  - 1 do
  begin
   s:=((spanf[q ] + square(q ) ) - (spanf[spann[k ] ] + square(spann[k ] ) ) ) / (2 * q - 2 * spann[k ] );

   while s <= spang[k ] do
    begin
     dec(k );

     s:=((spanf[q ] + square(q ) ) - (spanf[spann[k ] ] + square(spann[k ] ) ) ) / (2 * q - 2 * spann[k ] );

    end;

   inc(k );

   spann[k ]:=q;
   spang[k ]:=s;

   spang[k + 1 ]:=+infinity;

   inc(q );

  end;

 k:=0;
 q:=0;

 while q <= length - 1 do
  begin
   while spang[k + 1 ] < q do
    inc(k );

   spanr[q ]:=square(q - spann[k ] ) + spanf[spann[k ] ];

   inc(q );

  end;

end;

{ CONTOUR_CREATE }
// DT algorithm by: Pedro Felzenszwalb
function gradient_contour.contour_create(ps : path_storage_ptr ) : pointer;
var
 rb : rendering_buffer;
 pf : pixel_formats;

 ras : rasterizer_outline;
 mtx : trans_affine;

 rgba : aggclr;
 renb : renderer_base;
 prim : renderer_primitives;
 conv : conv_curve;

 trans : conv_transform;

 min ,max ,scale : single;

 x1 ,y1 ,x2 ,y2 : double;

 width ,height ,length ,fcx ,fcy : int;

 buffer ,image : pointer;

 src : int8u_ptr;

 dst ,im ,spanf ,spang ,spanr : single_ptr;

 spann : int_ptr;

begin
 result:=NIL;
 buffer:=NIL;

 if ps <> NIL then
  begin
  { I. Render Black And White NonAA Stroke of the Path }
  { Path Bounding Box + Some Frame Space Around [configurable] }
   conv.Construct(ps );

   if bounding_rect_single(@conv ,0 ,@x1 ,@y1 ,@x2 ,@y2 ) then
    begin
    { Create BW Rendering Surface }
     width :=Ceil(x2 - x1 ) + m_frame * 2 + 1;
     height:=Ceil(y2 - y1 ) + m_frame * 2 + 1;

     if agg_getmem(buffer ,width * height ) then
      begin
       FillChar(buffer^ ,width * height ,255 );

      { Setup VG Engine & Render }
       rb.Construct;
       rb.attach(buffer ,width ,height ,width );

       pixfmt_gray8(pf ,@rb );

       renb.Construct(@pf );

       prim.Construct(@renb );
       ras.Construct (@prim );

       mtx.Construct;
       mtx.Translate(-x1 + m_frame ,-y1 + m_frame );

       trans.Construct(@conv ,@mtx );

       rgba.ConstrInt  (0 ,0 ,0 ,255 );
       prim.line_color_(@rgba );
       ras.add_path    (@trans );

       rb.Destruct;

      { II. Distance Transform }
      { Create Float Buffer + 0 vs infinity (1e20) assignment }
       if agg_getmem(image ,width * height * sizeof(single ) ) then
        begin
         src:=buffer;
         dst:=image;

         for fcy:=0 to height - 1 do
          for fcx:=0 to width - 1 do
           begin
            if src^ = 0 then
             dst^:=0
            else
             dst^:=infinity;

            inc(ptrcomp(src ) );
            inc(ptrcomp(dst ) ,sizeof(single ) );

           end;

        { DT of 2d }
        { SubBuff<float> max width,height }
         length:=width;

         if height > length then
          length:=height;

         spanf:=NIL;
         spang:=NIL;
         spanr:=NIL;
         spann:=NIL;

         if agg_getmem(pointer(spanf ) ,length * sizeof(single ) ) and
            agg_getmem(pointer(spang ) ,(length + 1 ) * sizeof(single ) ) and
            agg_getmem(pointer(spanr ) ,length * sizeof(single ) ) and
            agg_getmem(pointer(spann ) ,length * sizeof(int ) ) then
          begin
          { Transform along columns }
           for fcx:=0 to width - 1 do
            begin
             im :=pointer(ptrcomp(image ) + fcx * sizeof(single ) );
             dst:=spanf;

             for fcy:=0 to height - 1 do
              begin
               dst^:=im^;

               inc(ptrcomp(dst ) ,sizeof(single ) );
               inc(ptrcomp(im ) ,width * sizeof(single ) );

              end;

            { DT of 1d }
             dt(pointer(spanf ) ,pointer(spang ) ,pointer(spanr ) ,pointer(spann ) ,height );

             im :=pointer(ptrcomp(image ) + fcx * sizeof(single ) );
             dst:=spanr;

             for fcy:=0 to height - 1 do
              begin
               im^:=dst^;

               inc(ptrcomp(dst ) ,sizeof(single ) );
               inc(ptrcomp(im ) ,width * sizeof(single ) );

              end;

            end;

          { Transform along rows }
           for fcy:=0 to height - 1 do
            begin
             im :=pointer(ptrcomp(image ) + fcy * width * sizeof(single ) );
             dst:=spanf;

             for fcx:=0 to width - 1 do
              begin
               dst^:=im^;

               inc(ptrcomp(dst ) ,sizeof(single ) );
               inc(ptrcomp(im ) ,sizeof(single ) );

              end;

            { DT of 1d }
             dt(pointer(spanf ) ,pointer(spang ) ,pointer(spanr ) ,pointer(spann ) ,width );

             im :=pointer(ptrcomp(image ) + fcy * width * sizeof(single ) );
             dst:=spanr;

             for fcx:=0 to width - 1 do
              begin
               im^:=dst^;

               inc(ptrcomp(dst ) ,sizeof(single ) );
               inc(ptrcomp(im ) ,sizeof(single ) );

              end;

            end;

          { Take Square Roots, Min & Max }
           dst:=image;
           min:=Sqrt(dst^ );
           max:=min;

           for fcy:=0 to height - 1 do
            for fcx:=0 to width - 1 do
             begin
              dst^:=Sqrt(dst^ );

              if min > dst^ then
               min:=dst^;

              if max < dst^ then
               max:=dst^;

              inc(ptrcomp(dst ) ,sizeof(single ) );

             end;

          { III. Convert To Grayscale }
           if min = max then
            FillChar(buffer^ ,width * height ,0 )
           else
            begin
             scale:=255 / (max - min );

             src:=buffer;
             dst:=image;

             for fcy:=0 to height - 1 do
              for fcx:=0 to width - 1 do
               begin
                src^:=int8u(Trunc((dst^ - min ) * scale ) );

                inc(ptrcomp(src ) );
                inc(ptrcomp(dst ) ,sizeof(single ) );

               end;

            end;

          { OK }
           if m_buffer <> NIL then
            agg_freemem(m_buffer ,m_width * m_height );

           m_buffer:=buffer;
           m_width :=width;
           m_height:=height;

           buffer:=NIL;
           result:=m_buffer;

          end;

         if spanf <> NIL then
          agg_freemem(pointer(spanf ) ,length * sizeof(single ) );

         if spang <> NIL then
          agg_freemem(pointer(spang ) ,(length + 1 ) * sizeof(single ) );

         if spanr <> NIL then
          agg_freemem(pointer(spanr ) ,length * sizeof(single ) );

         if spann <> NIL then
          agg_freemem(pointer(spann ) ,length * sizeof(int ) );

         agg_freemem(image ,width * height * sizeof(single ) );

        end;

      end;

    end;

  { Free }
   conv.Destruct;

   if buffer <> NIL then
    agg_freemem(buffer ,width * height );

  end;

end;

{ CONTOUR_WIDTH }
function gradient_contour.contour_width : int;
begin
 result:=m_width;

end;

{ CONTOUR_HEIGHT }
function gradient_contour.contour_height : int;
begin
 result:=m_height;

end;

{ D1 }
procedure gradient_contour.d1(d : double );
begin
 m_d1:=d;

end;

{ D2 }
procedure gradient_contour.d2(d : double );
begin
 m_d2:=d;

end;

{ FRAME }
procedure gradient_contour.frame(f : int );
begin
 m_frame:=f;

end;

{ FRAME }
function gradient_contour.frame : int;
begin
 result:=m_frame;

end;

{ CALCULATE }
function gradient_contour.calculate(x ,y ,d : int ) : int;
var
 px ,py : int;

 pixel : int8u_ptr;

begin
 if m_buffer <> NIL then
  begin
   px:=shr_int32(x ,gradient_subpixel_shift );
   py:=shr_int32(y ,gradient_subpixel_shift );

   px:=px mod m_width;

   if px < 0 then
    px:=m_width + px;

   py:=py mod m_height;

   if py < 0 then
    py:=m_height + py;

   pixel :=int8u_ptr(ptrcomp(m_buffer ) + py * m_width + px );
   result:=Round(pixel^ * (m_d2 / 256 ) + m_d1 ) shl gradient_subpixel_shift;

  end
 else
  result:=0;

end;

END.

