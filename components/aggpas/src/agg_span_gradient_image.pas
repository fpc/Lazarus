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
// 14.11.2007-Milano: Establishment
//
{ agg_span_gradient_image.pas }
unit
 agg_span_gradient_image ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_color ,
 agg_array ,
 agg_span_gradient ,
 agg_pixfmt ,
 agg_pixfmt_rgba ,
 agg_rendering_buffer ;

{ GLOBAL VARIABLES & CONSTANTS }
{ TYPES DEFINITION }
type
 one_color_function = object(array_base )
   m_color : aggclr;

   constructor Construct;

   function  size : unsigned; virtual;
   function  entry : unsigned; virtual;
   function  array_operator(i : unsigned ) : pointer; virtual;

  end;

 gradient_image = object(gradient )
  private
   m_buffer : pointer;
   m_alocdx ,
   m_alocdy ,
   m_width  ,
   m_height : int;

   m_renbuf : rendering_buffer_ptr;
   m_pixelf : pixel_formats;

   m_color : aggclr_ptr;

   m_color_function : one_color_function;

  public
   constructor Construct;
   destructor  Destruct;

   function  image_create(width ,height : int ) : pointer;
   function  image_buffer : pointer;
   function  image_width : int;
   function  image_height : int;
   function  image_stride : int;

   function  calculate(x ,y ,d : int ) : int; virtual;

   function  pixel_format : pixel_formats_ptr;
   function  color_function : array_base_ptr;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor one_color_function.Construct;
begin
 m_color.Construct;

end;

{ SIZE }
function one_color_function.size : unsigned;
begin
 result:=1;

end;

{ ENTRY }
function one_color_function.entry : unsigned;
begin
 result:=sizeof(aggclr );

end;

{ ARRAY_OPERATOR }
function one_color_function.array_operator(i : unsigned ) : pointer;
begin
 result:=@m_color;

end;

{ CONSTRUCT }
constructor gradient_image.Construct;
begin
 m_color_function.Construct;

 m_buffer:=NIL;
 m_alocdx:=0;
 m_alocdy:=0;
 m_width :=0;
 m_height:=0;

 m_renbuf:=NIL;

 pixfmt_undefined(m_pixelf );

 m_color:=m_color_function.array_operator(0 );

end;

{ DESTRUCT }
destructor gradient_image.Destruct;
begin
 if m_buffer <> NIL then
  agg_freemem(m_buffer ,m_alocdx * m_alocdy * 4 );

 if m_renbuf <> NIL then
  dispose(m_renbuf ,Destruct );

end;

{ IMAGE_CREATE }
function gradient_image.image_create(width ,height : int ) : pointer;
var
 row  : pointer;
 rows : unsigned;

begin
 result:=NIL;

 if m_renbuf <> NIL then
  dispose(m_renbuf ,Destruct );

 m_renbuf:=NIL;

 if (width > m_alocdx ) or
    (height > m_alocdy ) then
  begin
   if m_buffer <> NIL then
    agg_freemem(m_buffer ,m_alocdx * m_alocdy * 4 );

   m_buffer:=NIL;

   if agg_getmem(m_buffer ,width * height * 4 ) then
    begin
     m_alocdx:=width;
     m_alocdy:=height;

    end
   else
    begin
     m_alocdx:=0;
     m_alocdy:=0;

    end;

  end;

 if m_buffer <> NIL then
  begin
   m_width :=width;
   m_height:=height;

   row :=m_buffer;
   rows:=height;

   while rows > 0 do
    begin
     FillChar(row^ ,m_width * 4 ,0 );

     inc(ptrcomp(row ) ,m_alocdx * 4 );
     dec(rows );

    end;

   result:=m_buffer;

  end
 else
  begin
   m_width :=0;
   m_height:=0;

  end;

end;

{ IMAGE_BUFFER }
function gradient_image.image_buffer : pointer;
begin
 result:=m_buffer;

end;

{ IMAGE_WIDTH }
function gradient_image.image_width : int;
begin
 result:=m_width;

end;

{ IMAGE_HEIGHT }
function gradient_image.image_height : int;
begin
 result:=m_height;

end;

{ IMAGE_STRIDE }
function gradient_image.image_stride : int;
begin
 result:=m_alocdx * 4;

end;

{ CALCULATE }
function gradient_image.calculate(x ,y ,d : int ) : int;
var
 px ,py : int;

 pixel : rgba8_ptr;

begin
 result:=0;

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

   pixel:=rgba8_ptr(ptrcomp(m_buffer ) + py * (m_alocdx * 4 ) + px * 4 );

   m_color.r:=pixel.r;
   m_color.g:=pixel.g;
   m_color.b:=pixel.b;
   m_color.a:=pixel.a;

  end
 else
  begin
   m_color.r:=0;
   m_color.g:=0;
   m_color.b:=0;
   m_color.a:=0;

  end;

end;

{ PIXEL_FORMAT }
function gradient_image.pixel_format : pixel_formats_ptr;
begin
 if (m_buffer <> NIL ) and
    (m_renbuf = NIL ) then
  begin
   new(m_renbuf ,Construct );

   m_renbuf.attach(m_buffer ,m_width ,m_height ,m_alocdx * 4 );
   pixfmt_rgba32  (m_pixelf ,m_renbuf );

  end;

 if m_renbuf = NIL then
  result:=NIL
 else
  result:=@m_pixelf;

end;

{ COLOR_FUNCTION }
function gradient_image.color_function : array_base_ptr;
begin
 result:=@m_color_function;

end;

END.

