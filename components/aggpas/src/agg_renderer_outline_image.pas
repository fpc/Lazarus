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
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 03.02.2006-Milano: Complete unit port
// 02.02.2006-Milano: Unit port establishment
//
{ agg_renderer_outline_image.pas }
unit
 agg_renderer_outline_image ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }

uses
 Math ,
 agg_basics ,
 agg_color ,
 agg_line_aa_basics ,
 agg_dda_line ,
 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_outline_aa ,
 agg_pattern_filters_rgba ;

{ TYPES DEFINITION }
type
 pixel_source_ptr = ^pixel_source;
 pixel_source = object
   function  _width : unsigned; virtual; abstract;
   function  _height : unsigned; virtual; abstract;

   function  _width_d : double; virtual;
   function  _height_d : double; virtual;

   function  pixel(x ,y : int ) : rgba8; virtual; abstract;

  end;

 line_image_scale = object(pixel_source )
   m_source : pixel_source_ptr;
   m_height ,
   m_scale  : double;

   constructor Construct(src : pixel_source_ptr; height_ : double );

   function  _width_d : double; virtual;
   function  _height_d : double; virtual;

   function  pixel(x ,y : int ) : rgba8; virtual;

  end;

 line_image_pattern_ptr = ^line_image_pattern; 
 line_image_pattern = object
   m_buf    : rendering_buffer;
   m_filter : pattern_filter_ptr;

   m_dilation    : unsigned;
   m_dilation_hr : int;

   m_data   : aggclr_ptr;
   m_dt_sz  ,
   m_width  ,
   m_height : unsigned;

   m_width_hr       ,
   m_half_height_hr ,
   m_offset_y_hr    : int;

   constructor Construct(filter_ : pattern_filter_ptr ); overload;
   constructor Construct(filter_ : pattern_filter_ptr; src : pixel_source_ptr ); overload;
   destructor  Destruct;

   procedure create(src : pixel_source_ptr ); virtual;

   function  pattern_width : int;
   function  line_width : int;

   procedure pixel(p : aggclr_ptr; x ,y : int ); virtual;

   procedure filter(filter_ : pattern_filter_ptr ); overload;
   function  filter : pattern_filter_ptr; overload;

  end;

 line_image_pattern_pow2 = object(line_image_pattern )
   m_mask : unsigned;

   constructor Construct(filter_ : pattern_filter_ptr ); overload;
   constructor Construct(filter_ : pattern_filter_ptr; src : pixel_source_ptr ); overload;

   procedure create(src : pixel_source_ptr ); virtual;
   procedure pixel (p : aggclr_ptr; x ,y : int ); virtual;

  end;

 distance_interpolator4 = object(distance_interpolator )
   m_dx ,
   m_dy ,

   m_dx_start ,
   m_dy_start ,
   m_dx_pict  ,
   m_dy_pict  ,
   m_dx_end   ,
   m_dy_end   ,

   m_dist       ,
   m_dist_start ,
   m_dist_pict  ,
   m_dist_end   ,

   m_len : int;

   constructor Construct; overload;
   constructor Construct(x1 ,y1 ,x2 ,y2 ,sx ,sy ,ex ,ey ,len_ : int; scale : double; x ,y : int ); overload;

   procedure inc_x_; virtual;
   procedure dec_x_; virtual;
   procedure inc_y_; virtual;
   procedure dec_y_; virtual;

   procedure inc_x(dy_ : int ); virtual;
   procedure dec_x(dy_ : int ); virtual;
   procedure inc_y(dx_ : int ); virtual;
   procedure dec_y(dx_ : int ); virtual;

   function  dist : int; virtual;
   function  dist_start : int; virtual;
   function  dist_pict : int;
   function  dist_end : int; virtual;

   function  dx : int; virtual;
   function  dy : int; virtual;
   function  dx_start : int; virtual;
   function  dy_start : int; virtual;
   function  dx_pict : int;
   function  dy_pict : int;
   function  dx_end : int; virtual;
   function  dy_end : int; virtual;
   function  len : int;

  end;

 renderer_outline_image_ptr = ^renderer_outline_image;

 line_interpolator_image = object //(line_interpolator )
   m_lp  : line_parameters_ptr;
   m_li  : dda2_line_interpolator;
   m_di  : distance_interpolator4;
   m_ren : renderer_outline_image_ptr;

   m_plen ,
   m_x    ,
   m_y    ,

   m_old_x ,
   m_old_y ,
   m_count ,
   m_width ,

   m_max_extent ,

   m_start ,
   m_step  : int;

   m_dist_pos : array[0..max_half_width + 1 - 1 ] of int;
   m_colors   : array[0..max_half_width * 2 + 4 - 1 ] of aggclr;

   constructor Construct(
                ren : renderer_outline_image_ptr;
                lp  : line_parameters_ptr;
                sx ,sy ,ex ,ey ,pattern_start : int;
                scale_x : double );

   function  step_hor : boolean;
   function  step_ver : boolean;

   function  pattern_end : int;
   function  vertical : boolean;
   function  width : int;
   function  count : int;

  end;

 renderer_outline_image = object(renderer_outline )
   m_ren     : renderer_base_ptr;
   m_pattern : line_image_pattern_ptr;
   m_start   : int;
   m_scale_x : double;

   constructor Construct(ren : renderer_base_ptr; patt : line_image_pattern_ptr );

   procedure pattern_(p : line_image_pattern_ptr );
   function  _pattern : line_image_pattern_ptr;

   procedure scale_x_(s : double );
   function  _scale_x : double;

   procedure start_x_(s : double );
   function  _start_x : double;

   function  subpixel_width : int; virtual;
   function  _pattern_width : int;

   procedure pixel(p : aggclr_ptr; x ,y : int );

   procedure blend_color_hspan(x ,y : int; len : unsigned; colors : aggclr_ptr );
   procedure blend_color_vspan(x ,y : int; len : unsigned; colors : aggclr_ptr );

   function  accurate_join_only : boolean; virtual;

   procedure semidot(cmp : cmp_function; xc1 ,yc1 ,xc2 ,yc2 : int ); virtual;

   procedure line0(lp : line_parameters_ptr ); virtual;
   procedure line1(lp : line_parameters_ptr; sx ,sy : int ); virtual;
   procedure line2(lp : line_parameters_ptr; ex ,ey : int ); virtual;
   procedure line3(lp : line_parameters_ptr; sx ,sy ,ex ,ey : int ); virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ _WIDTH_D }
function pixel_source._width_d;
begin
 result:=_width;

end;

{ _HEIGHT_D }
function pixel_source._height_d;
begin
 result:=_height;

end;

{ CONSTRUCT }
constructor line_image_scale.Construct;
begin
 m_source:=src;
 m_height:=height_;

 if height_ <> 0 then
  m_scale:=src._height / height_
 else
  m_scale:=0;

end;

{ _WIDTH_D }
function line_image_scale._width_d;
begin
 result:=m_source._width;

end;

{ _HEIGHT_D }
function line_image_scale._height_d;
begin
 result:=m_height;

end;

{ PIXEL }
function line_image_scale.pixel;
var
 src_y : double;

 h ,y1 ,y2 : int;

 pix1 ,pix2 : aggclr;

begin
 src_y:=(y + 0.5 ) * m_scale - 0.5;

 h :=trunc(m_source._height ) - 1;
 y1:=trunc(src_y );
 y2:=y1 + 1;

 if y1 < 0 then
  pix1.Construct
 else
  pix1.Construct(m_source.pixel(x ,y1 ) );

 if y2 > h then
  pix2.Construct
 else
  pix2.Construct(m_source.pixel(x ,y2 ) );

 result:=pix1.gradient8(@pix2 ,src_y - y1 );

end;

{ CONSTRUCT }
constructor line_image_pattern.Construct(filter_ : pattern_filter_ptr );
begin
 m_buf.Construct;

 m_filter:=filter_;

 m_dilation   :=filter_.dilation + 1;
 m_dilation_hr:=m_dilation shl line_subpixel_shift;

 m_data  :=0;
 m_dt_sz :=0;
 m_width :=0;
 m_height:=0;

 m_width_hr      :=0;
 m_half_height_hr:=0;
 m_offset_y_hr   :=0;

end;

{ CONSTRUCT }
constructor line_image_pattern.Construct(filter_ : pattern_filter_ptr; src : pixel_source_ptr );
begin
 m_buf.Construct;

 m_filter:=filter_;

 m_dilation   :=filter_.dilation + 1;
 m_dilation_hr:=m_dilation shl line_subpixel_shift;

 m_data  :=0;
 m_dt_sz :=0;
 m_width :=0;
 m_height:=0;

 m_width_hr      :=0;
 m_half_height_hr:=0;
 m_offset_y_hr   :=0;

 create(src );

end;

{ DESTRUCT }
destructor line_image_pattern.Destruct;
begin
 agg_freemem(pointer(m_data ) ,m_dt_sz );

 m_buf.Destruct;

end;

{ CREATE }
procedure line_image_pattern.create;
var
 x ,y ,h : unsigned;

 d1 ,d2 ,s1 ,s2 : rgba8_ptr;

begin
 m_height:=Ceil(src._height_d );
 m_width :=Ceil(src._width_d );

 m_width_hr      :=trunc(src._width_d * line_subpixel_size );
 m_half_height_hr:=trunc(src._height_d * line_subpixel_size / 2 );
 m_offset_y_hr   :=m_dilation_hr + m_half_height_hr - line_subpixel_size div 2;

 inc(m_half_height_hr ,line_subpixel_size div 2 );

 agg_freemem(pointer(m_data ) ,m_dt_sz );

 m_dt_sz:=(m_width + m_dilation * 2 ) * (m_height + m_dilation * 2 ) * sizeof(rgba8 );

 agg_getmem(pointer(m_data ) ,m_dt_sz );

 m_buf.attach(
  int8u_ptr(m_data ) ,
  m_width + m_dilation * 2 ,
  m_height + m_dilation * 2 ,
  (m_width + m_dilation * 2 ) * sizeof(rgba8 ) );

 if m_height > 0 then
  for y:=0 to m_height - 1 do
   begin
    d1:=rgba8_ptr(ptrcomp(m_buf.row(y + m_dilation ) ) + m_dilation * sizeof(rgba8 ) );

    for x:=0 to m_width - 1 do
     begin
      d1^:=src.pixel(x ,y );

      inc(ptrcomp(d1 ) ,sizeof(rgba8 ) );

     end;

   end;

 for y:=0 to m_dilation - 1 do
  begin
   d1:=rgba8_ptr(ptrcomp(m_buf.row(m_dilation + m_height + y ) ) + m_dilation * sizeof(rgba8 ) );
   d2:=rgba8_ptr(ptrcomp(m_buf.row(m_dilation - y - 1 ) ) + m_dilation * sizeof(rgba8 ) );

   for x:=0 to m_width - 1 do
    begin
     d1^.no_color;
     d2^.no_color;

     inc(ptrcomp(d1 ) ,sizeof(rgba8 ) );
     inc(ptrcomp(d2 ) ,sizeof(rgba8 ) );

    end;

  end;

 h:=m_height + m_dilation * 2;

 for y:=0 to h - 1 do
  begin
   s1:=rgba8_ptr(ptrcomp(m_buf.row(y ) ) + m_dilation * sizeof(rgba8 ) );
   s2:=rgba8_ptr(ptrcomp(m_buf.row(y ) ) + (m_dilation + m_width ) * sizeof(rgba8 ) );
   d1:=rgba8_ptr(ptrcomp(m_buf.row(y ) ) + (m_dilation + m_width ) * sizeof(rgba8 ) );
   d2:=rgba8_ptr(ptrcomp(m_buf.row(y ) ) + m_dilation * sizeof(rgba8 ) );

   for x:=0 to m_dilation - 1 do
    begin
     d1^:=s1^;

     inc(ptrcomp(d1 ) ,sizeof(rgba8 ) );
     inc(ptrcomp(s1 ) ,sizeof(rgba8 ) );
     dec(ptrcomp(d2 ) ,sizeof(rgba8 ) );
     dec(ptrcomp(s2 ) ,sizeof(rgba8 ) );

     d2^:=s2^;

    end;

  end;

end;

{ PATTERN_WIDTH }
function line_image_pattern.pattern_width;
begin
 result:=m_width_hr;

end;

{ LINE_WIDTH }
function line_image_pattern.line_width;
begin
 result:=m_half_height_hr;

end;

{ PIXEL }
procedure line_image_pattern.pixel;
begin
 m_filter.pixel_high_res(
  m_buf.rows ,
  p ,
  x mod m_width_hr + m_dilation_hr ,
  y + m_offset_y_hr );

end;

{ FILTER }
procedure line_image_pattern.filter(filter_ : pattern_filter_ptr );
begin
 m_filter:=filter_;

end;

{ FILTER }
function line_image_pattern.filter : pattern_filter_ptr;
begin
 result:=m_filter;

end;

{ CONSTRUCT }
constructor line_image_pattern_pow2.Construct(filter_ : pattern_filter_ptr );
begin
 inherited Construct(filter_ );

 m_mask:=0;

end;

{ CONSTRUCT }
constructor line_image_pattern_pow2.Construct(filter_ : pattern_filter_ptr; src : pixel_source_ptr );
begin
 inherited Construct(filter_ ,src );

 create(src );

end;

{ CREATE }
procedure line_image_pattern_pow2.create;
begin
 inherited create(src );

 m_mask:=1;

 while m_mask < m_width do
  begin
   m_mask:=m_mask shl 1;
   m_mask:=m_mask or 1;

  end;

 m_mask:=m_mask shl (line_subpixel_shift - 1 );
 m_mask:=m_mask or line_subpixel_mask;

 m_width_hr:=m_mask + 1;

end;

{ PIXEL }
procedure line_image_pattern_pow2.pixel;
begin
 m_filter.pixel_high_res(
  m_buf.rows ,
  p ,
  (x and m_mask ) + m_dilation_hr ,
  y + m_offset_y_hr );

end;

{ CONSTRUCT }
constructor distance_interpolator4.Construct;
begin
end;

{ CONSTRUCT }
constructor distance_interpolator4.Construct(x1 ,y1 ,x2 ,y2 ,sx ,sy ,ex ,ey ,len_ : int; scale : double; x ,y : int );
var
 d : double;

 dx_ ,dy_ : int;

begin
 m_dx:=x2 - x1;
 m_dy:=y2 - y1;

 m_dx_start:=line_mr(sx ) - line_mr(x1 );
 m_dy_start:=line_mr(sy ) - line_mr(y1 );
 m_dx_end  :=line_mr(ex ) - line_mr(x2 );
 m_dy_end  :=line_mr(ey ) - line_mr(y2 );

 m_dist:=
  trunc(
   (x + line_subpixel_size / 2 - x2 ) * m_dy -
   (y + line_subpixel_size / 2 - y2 ) * m_dx );

 m_dist_start:=
  (line_mr(x + line_subpixel_size div 2 ) - line_mr(sx ) ) * m_dy_start -
  (line_mr(y + line_subpixel_size div 2 ) - line_mr(sy ) ) * m_dx_start;

 m_dist_end:=
  (line_mr(x + line_subpixel_size div 2) - line_mr(ex ) ) * m_dy_end -
  (line_mr(y + line_subpixel_size div 2) - line_mr(ey ) ) * m_dx_end;

 if scale <> 0 then
  m_len:=trunc(len_ / scale )
 else
  m_len:=0; 

 d:=len_ * scale;

 if d <> 0 then
  begin
   dx_:=trunc(((x2 - x1 ) shl line_subpixel_shift ) / d );
   dy_:=trunc(((y2 - y1 ) shl line_subpixel_shift ) / d );

  end
 else
  begin
   dx_:=0;
   dy_:=0;

  end;

 m_dx_pict:=-dy_;
 m_dy_pict:=dx_;

 m_dist_pict:=
  shr_int32(
   (x + line_subpixel_size div 2 - (x1 - dy_ ) ) * m_dy_pict -
   (y + line_subpixel_size div 2 - (y1 + dx_ ) ) * m_dx_pict ,line_subpixel_shift );

 m_dx      :=m_dx shl line_subpixel_shift;
 m_dy      :=m_dy shl line_subpixel_shift;
 m_dx_start:=m_dx_start shl line_mr_subpixel_shift;
 m_dy_start:=m_dy_start shl line_mr_subpixel_shift;
 m_dx_end  :=m_dx_end shl line_mr_subpixel_shift;
 m_dy_end  :=m_dy_end shl line_mr_subpixel_shift;

end;

{ INC_X_ }
procedure distance_interpolator4.inc_x_;
begin
 inc(m_dist ,m_dy );
 inc(m_dist_start ,m_dy_start );
 inc(m_dist_pict ,m_dy_pict );
 inc(m_dist_end ,m_dy_end );

end;

{ DEC_X_ }
procedure distance_interpolator4.dec_x_;
begin
 dec(m_dist ,m_dy );
 dec(m_dist_start ,m_dy_start );
 dec(m_dist_pict ,m_dy_pict );
 dec(m_dist_end ,m_dy_end );

end;

{ INC_Y_ }
procedure distance_interpolator4.inc_y_;
begin
 dec(m_dist ,m_dx );
 dec(m_dist_start ,m_dx_start );
 dec(m_dist_pict ,m_dx_pict );
 dec(m_dist_end ,m_dx_end );

end;

{ DEC_Y_ }
procedure distance_interpolator4.dec_y_;
begin
 inc(m_dist ,m_dx );
 inc(m_dist_start ,m_dx_start );
 inc(m_dist_pict ,m_dx_pict );
 inc(m_dist_end ,m_dx_end );

end;

{ INC_X }
procedure distance_interpolator4.inc_x;
begin
 inc(m_dist ,m_dy );
 inc(m_dist_start ,m_dy_start );
 inc(m_dist_pict ,m_dy_pict );
 inc(m_dist_end ,m_dy_end );

 if dy_ > 0 then
  begin
   dec(m_dist ,m_dx );
   dec(m_dist_start ,m_dx_start );
   dec(m_dist_pict ,m_dx_pict );
   dec(m_dist_end ,m_dx_end );

  end;

 if dy_ < 0 then
  begin
   inc(m_dist ,m_dx );
   inc(m_dist_start ,m_dx_start );
   inc(m_dist_pict ,m_dx_pict );
   inc(m_dist_end ,m_dx_end );

  end;

end;

{ DEC_X }
procedure distance_interpolator4.dec_x;
begin
 dec(m_dist ,m_dy );
 dec(m_dist_start ,m_dy_start );
 dec(m_dist_pict ,m_dy_pict );
 dec(m_dist_end ,m_dy_end );

 if dy_ > 0 then
  begin
   dec(m_dist ,m_dx );
   dec(m_dist_start ,m_dx_start );
   dec(m_dist_pict ,m_dx_pict );
   dec(m_dist_end ,m_dx_end );

  end;

 if dy_ < 0 then
  begin
   inc(m_dist ,m_dx );
   inc(m_dist_start ,m_dx_start );
   inc(m_dist_pict ,m_dx_pict );
   inc(m_dist_end ,m_dx_end );

  end;

end;

{ INC_Y }
procedure distance_interpolator4.inc_y;
begin
 dec(m_dist ,m_dx );
 dec(m_dist_start ,m_dx_start );
 dec(m_dist_pict ,m_dx_pict );
 dec(m_dist_end ,m_dx_end );

 if dx_ > 0 then
  begin
   inc(m_dist ,m_dy );
   inc(m_dist_start ,m_dy_start );
   inc(m_dist_pict ,m_dy_pict );
   inc(m_dist_end ,m_dy_end );

  end;

 if dx_ < 0 then
  begin
   dec(m_dist ,m_dy );
   dec(m_dist_start ,m_dy_start );
   dec(m_dist_pict ,m_dy_pict );
   dec(m_dist_end ,m_dy_end );

  end;

end;

{ DEC_Y }
procedure distance_interpolator4.dec_y;
begin
 inc(m_dist ,m_dx );
 inc(m_dist_start ,m_dx_start );
 inc(m_dist_pict ,m_dx_pict );
 inc(m_dist_end ,m_dx_end );

 if dx_ > 0 then
  begin
   inc(m_dist ,m_dy );
   inc(m_dist_start ,m_dy_start );
   inc(m_dist_pict ,m_dy_pict );
   inc(m_dist_end ,m_dy_end );

  end;

 if dx_ < 0 then
  begin
   dec(m_dist ,m_dy );
   dec(m_dist_start ,m_dy_start );
   dec(m_dist_pict ,m_dy_pict );
   dec(m_dist_end ,m_dy_end );

  end;

end;

{ DIST }
function distance_interpolator4.dist;
begin
 result:=m_dist;

end;

{ DIST_START }
function distance_interpolator4.dist_start;
begin
 result:=m_dist_start;

end;

{ DIST_PICT }
function distance_interpolator4.dist_pict;
begin
 result:=m_dist_pict;

end;

{ DIST_END }
function distance_interpolator4.dist_end;
begin
 result:=m_dist_end;

end;

{ DX }
function distance_interpolator4.dx;
begin
 result:=m_dx;

end;

{ DY }
function distance_interpolator4.dy;
begin
 result:=m_dy;

end;

{ DX_START }
function distance_interpolator4.dx_start;
begin
 result:=m_dx_start;

end;

{ DY_START }
function distance_interpolator4.dy_start;
begin
 result:=m_dy_start;

end;

{ DX_PICT }
function distance_interpolator4.dx_pict;
begin
 result:=m_dx_pict;

end;

{ DY_PICT }
function distance_interpolator4.dy_pict;
begin
 result:=m_dy_pict;

end;

{ DX_END }
function distance_interpolator4.dx_end;
begin
 result:=m_dx_end;

end;

{ DY_END }
function distance_interpolator4.dy_end;
begin
 result:=m_dy_end;

end;

{ LEN }
function distance_interpolator4.len;
begin
 result:=m_len;

end;

{ CONSTRUCT }
constructor line_interpolator_image.Construct;
var
 i : unsigned;

 stop ,dist1_start ,dist2_start ,npix ,dx ,dy : int;

 li : dda2_line_interpolator;

begin
 m_lp:=lp;

 if lp.vertical then
  m_li.Construct(line_dbl_hr(lp.x2 - lp.x1 ) ,Abs(lp.y2 - lp.y1 ) )
 else
  m_li.Construct(line_dbl_hr(lp.y2 - lp.y1 ) ,Abs(lp.x2 - lp.x1 ) + 1 );

 m_di.Construct(
  lp.x1 ,lp.y1 ,lp.x2 ,lp.y2 ,sx ,sy ,ex ,ey ,lp.len ,scale_x ,
  lp.x1 and not line_subpixel_mask ,
  lp.y1 and not line_subpixel_mask );

 m_ren:=ren;

 m_x:=shr_int32(lp.x1 ,line_subpixel_shift );
 m_y:=shr_int32(lp.y1 ,line_subpixel_shift );

 m_old_x:=m_x;
 m_old_y:=m_y;

 if lp.vertical then
  m_count:=Abs(shr_int32(lp.y2 ,line_subpixel_shift ) - m_y )
 else
  m_count:=Abs(shr_int32(lp.x2 ,line_subpixel_shift ) - m_x );

 m_width     :=ren.subpixel_width;
 m_max_extent:=shr_int32(m_width ,line_subpixel_shift - 2 );

try
 m_start:=pattern_start + (m_max_extent + 2 ) * ren._pattern_width;
except
 m_start:=0 + (m_max_extent + 2 ) * ren._pattern_width;

end;

 m_step :=0;

 if lp.vertical then
  li.Construct(0 ,lp.dy shl line_subpixel_shift ,lp.len )
 else
  li.Construct(0 ,lp.dx shl line_subpixel_shift ,lp.len );

 stop:=m_width + line_subpixel_size * 2;
 i   :=0;

 while i < max_half_width do
  begin
   m_dist_pos[i ]:=li._y;

   if m_dist_pos[i ] >= stop then
    break;

   li.plus_operator;

   inc(i );

  end;

 m_dist_pos[i ]:=$7FFF0000;

 npix:=1;

 if lp.vertical then
  repeat
   m_li.minus_operator;

   dec(m_y ,lp.inc_ );

   m_x:=shr_int32(m_lp.x1 + m_li._y ,line_subpixel_shift );

   if lp.inc_ > 0 then
    m_di.dec_y(m_x - m_old_x )
   else
    m_di.inc_y(m_x - m_old_x );

   m_old_x:=m_x;

   dist1_start:=m_di.dist_start;
   dist2_start:=dist1_start;

   dx:=0;

   if dist1_start < 0 then
    inc(npix );

   repeat
    inc(dist1_start ,m_di.dy_start );
    dec(dist2_start ,m_di.dy_start );

    if dist1_start < 0 then
     inc(npix );

    if dist2_start < 0 then
     inc(npix );

    inc(dx );

   until m_dist_pos[dx ] > m_width;

   if npix = 0 then
    break;

   npix:=0; 

   dec(m_step );

  until m_step < -m_max_extent
 else
  repeat
   m_li.minus_operator;

   dec(m_x ,lp.inc_ );

   m_y:=shr_int32(m_lp.y1 + m_li._y ,line_subpixel_shift );

   if lp.inc_ > 0 then
    m_di.dec_x(m_y - m_old_y )
   else
    m_di.inc_x(m_y - m_old_y );

   m_old_y:=m_y;

   dist1_start:=m_di.dist_start;
   dist2_start:=dist1_start;

   dy:=0;

   if dist1_start < 0 then
    inc(npix );

   repeat
    dec(dist1_start ,m_di.dx_start );
    inc(dist2_start ,m_di.dx_start );

    if dist1_start < 0 then
     inc(npix );

    if dist2_start < 0 then
     inc(npix );

    inc(dy );

   until m_dist_pos[dy ] > m_width;

   if npix = 0 then
    break;

   npix:=0; 

   dec(m_step );

  until m_step < -m_max_extent;

 m_li.adjust_forward;

 dec(m_step ,m_max_extent );

end;

{ STEP_HOR }
function line_interpolator_image.step_hor;
var
 s1 ,s2 ,dist_start ,dist_pict ,dist_end ,dy ,dist ,npix : int;

 p0 ,p1 : aggclr_ptr;

begin
 m_li.plus_operator;

 inc(m_x ,m_lp.inc_ );

 m_y:=shr_int32(m_lp.y1 + m_li._y ,line_subpixel_shift );

 if m_lp.inc_ > 0 then
  m_di.inc_x(m_y - m_old_y )
 else
  m_di.dec_x(m_y - m_old_y );

 m_old_y:=m_y;

 s1:=m_di.dist div m_lp.len;
 s2:=-s1;

 if m_lp.inc_ < 0 then
  s1:=-s1;

 dist_start:=m_di.dist_start;
 dist_pict :=m_di.dist_pict + m_start;
 dist_end  :=m_di.dist_end;

 p0:=aggclr_ptr(ptrcomp(@m_colors[0 ] ) + (max_half_width + 2 ) * sizeof(aggclr ) );
 p1:=p0;

 npix:=0;

 p1.clear;

 if dist_end > 0 then
  begin
   if dist_start <= 0 then
    m_ren.pixel(p1 ,dist_pict ,s2 );

   inc(npix );

  end;

 inc(ptrcomp(p1 ) ,sizeof(aggclr ) );

 dy  :=1;
 dist:=m_dist_pos[dy ];

 while dist - s1 <= m_width do
  begin
   dec(dist_start ,m_di.dx_start );
   dec(dist_pict ,m_di.dx_pict );
   dec(dist_end ,m_di.dx_end );

   p1.clear();

   if (dist_end > 0 ) and
      (dist_start <= 0 ) then
    begin
     if m_lp.inc_ > 0 then
      dist:=-dist;

     m_ren.pixel(p1 ,dist_pict ,s2 - dist );

     inc(npix );

    end;

   inc(ptrcomp(p1 ) ,sizeof(aggclr ) );
   inc(dy );

   dist:=m_dist_pos[dy ];

  end;

 dy:=1;

 dist_start:=m_di.dist_start;
 dist_pict :=m_di.dist_pict + m_start;
 dist_end  :=m_di.dist_end;

 dist:=m_dist_pos[dy ];

 while dist + s1 <= m_width do
  begin
   inc(dist_start ,m_di.dx_start );
   inc(dist_pict ,m_di.dx_pict );
   inc(dist_end ,m_di.dx_end );

   dec(ptrcomp(p0 ) ,sizeof(aggclr ) );

   p0.clear;

   if (dist_end > 0 ) and
      (dist_start <= 0 ) then
    begin
     if m_lp.inc_ > 0 then
      dist:=-dist;

     m_ren.pixel(p0 ,dist_pict ,s2 + dist );

     inc(npix );

    end;

   inc(dy );

   dist:=m_dist_pos[dy ];

  end;

 m_ren.blend_color_vspan(
  m_x ,m_y - dy + 1 ,
  unsigned((ptrcomp(p1 ) - ptrcomp(p0 ) ) div sizeof(aggclr ) ) ,
  p0 );

 inc(m_step );

 result:=
  (npix <> 0 ) and
  (m_step < m_count );

end;

{ STEP_VER }
function line_interpolator_image.step_ver;
var
 s1 ,s2 ,dist_start ,dist_pict ,dist_end ,dx ,dist ,npix : int;

 p0 ,p1 : aggclr_ptr;

begin
 m_li.plus_operator;

 inc(m_y ,m_lp.inc_ );

 m_x:=shr_int32(m_lp.x1 + m_li._y ,line_subpixel_shift );

 if m_lp.inc_ > 0 then
  m_di.inc_y(m_x - m_old_x )
 else
  m_di.dec_y(m_x - m_old_x );

 m_old_x:=m_x;

 s1:=m_di.dist div  m_lp.len;
 s2:=-s1;

 if m_lp.inc_ > 0 then
  s1:=-s1;

 dist_start:=m_di.dist_start;
 dist_pict :=m_di.dist_pict + m_start;
 dist_end  :=m_di.dist_end;

 p0:=aggclr_ptr(ptrcomp(@m_colors[0 ] ) + (max_half_width + 2 ) * sizeof(aggclr ) );
 p1:=p0;

 npix:=0;

 p1.clear;

 if dist_end > 0 then
  begin
   if dist_start <= 0 then
    m_ren.pixel(p1 ,dist_pict ,s2 );

   inc(npix );

  end;

 inc(ptrcomp(p1 ) ,sizeof(aggclr ) );

 dx  :=1;
 dist:=m_dist_pos[dx ];

 while dist - s1 <= m_width do
  begin
   inc(dist_start ,m_di.dy_start );
   inc(dist_pict ,m_di.dy_pict );
   inc(dist_end ,m_di.dy_end );

   p1.clear;

   if (dist_end > 0 ) and
      (dist_start <= 0 ) then
    begin
     if m_lp.inc_ > 0 then
      dist:=-dist;

     m_ren.pixel(p1 ,dist_pict ,s2 + dist );

     inc(npix );

    end;

   inc(ptrcomp(p1 ) ,sizeof(aggclr ) );
   inc(dx );

   dist:=m_dist_pos[dx ];

  end;

 dx:=1;

 dist_start:=m_di.dist_start;
 dist_pict :=m_di.dist_pict + m_start;
 dist_end  :=m_di.dist_end;

 dist:=m_dist_pos[dx ];

 while dist + s1 <= m_width do
  begin
   dec(dist_start ,m_di.dy_start );
   dec(dist_pict ,m_di.dy_pict );
   dec(dist_end ,m_di.dy_end );

   dec(ptrcomp(p0 ) ,sizeof(aggclr ) );

   p0.clear;

   if (dist_end > 0 ) and
      (dist_start <= 0 ) then
    begin
     if m_lp.inc_ > 0 then
      dist:=-dist;

     m_ren.pixel(p0 ,dist_pict ,s2 - dist );

     inc(npix );

    end;

   inc(dx );

   dist:=m_dist_pos[dx ];

  end;

 m_ren.blend_color_hspan(
  m_x - dx + 1 ,m_y ,
  unsigned((ptrcomp(p1 ) - ptrcomp(p0 ) ) div sizeof(aggclr ) ) ,
  p0 );

 inc(m_step );

 result:=
  (npix <> 0 ) and
  (m_step < m_count );

end;

{ PATTERN_END }
function line_interpolator_image.pattern_end;
begin
 result:=m_start + m_di.len;

end;

{ VERTICAL }
function line_interpolator_image.vertical;
begin
 result:=m_lp.vertical;

end;

{ WIDTH }
function line_interpolator_image.width;
begin
 result:=m_width;

end;

{ COUNT }
function line_interpolator_image.count;
begin
 result:=m_count;

end;

{ CONSTRUCT }
constructor renderer_outline_image.Construct;
begin
 m_ren    :=ren;
 m_pattern:=patt;
 m_start  :=0;
 m_scale_x:=1.0;

end;

{ PATTERN_ }
procedure renderer_outline_image.pattern_;
begin
 m_pattern:=p;

end;

{ _PATTERN }
function renderer_outline_image._pattern;
begin
 result:=m_pattern;

end;

{ SCALE_X_ }
procedure renderer_outline_image.scale_x_;
begin
 m_scale_x:=s;

end;

{ _SCALE_X }
function renderer_outline_image._scale_x;
begin
 result:=m_scale_x;

end;

{ START_X_ }
procedure renderer_outline_image.start_x_;
begin
 m_start:=trunc(s * line_subpixel_size );

end;

{ _START_X }
function renderer_outline_image._start_x;
begin
 result:=m_start / line_subpixel_size;

end;

{ SUBPIXEL_WIDTH }
function renderer_outline_image.subpixel_width;
begin
 result:=m_pattern.line_width;

end;

{ _PATTERN_WIDTH }
function renderer_outline_image._pattern_width;
begin
 result:=m_pattern.pattern_width;

end;

{ PIXEL }
procedure renderer_outline_image.pixel;
begin
 m_pattern.pixel(p ,x ,y );

end;

{ BLEND_COLOR_HSPAN }
procedure renderer_outline_image.blend_color_hspan;
begin
 m_ren.blend_color_hspan(x ,y ,len ,colors ,NIL );

end;

{ BLEND_COLOR_VSPAN }
procedure renderer_outline_image.blend_color_vspan;
begin
 m_ren.blend_color_vspan(x ,y ,len ,colors ,NIL );

end;

{ ACCURATE_JOIN_ONLY }
function renderer_outline_image.accurate_join_only;
begin
 result:=true;

end;

{ SEMIDOT }
procedure renderer_outline_image.semidot;
begin
end;

{ LINE0 }
procedure renderer_outline_image.line0;
begin
end;

{ LINE1 }
procedure renderer_outline_image.line1;
begin
end;

{ LINE2 }
procedure renderer_outline_image.line2;
begin
end;

{ LINE3 }
procedure renderer_outline_image.line3;
var
 li : line_interpolator_image;

begin
 fix_degenerate_bisectrix_start(lp ,@sx ,@sy );
 fix_degenerate_bisectrix_end  (lp ,@ex ,@ey );

 li.Construct(@self ,lp ,sx ,sy ,ex ,ey ,m_start ,m_scale_x );

 if li.vertical then
  while li.step_ver do
 else
  while li.step_hor do;

 m_start:=li.pattern_end;

end;

END.

