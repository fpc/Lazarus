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
// 01.02.2006-Milano: Complete unit port
// 31.01.2006-Milano: Unit port establishment
//
{ agg_renderer_outline_aa.pas }
unit
 agg_renderer_outline_aa ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
 agg_basics ,
 agg_color ,
 agg_math ,
 agg_line_aa_basics ,
 agg_dda_line ,
 agg_ellipse_bresenham ,
 agg_renderer_base ,
 agg_gamma_functions ,
 agg_vertex_source ;

{ TYPES DEFINITION }
const
 max_half_width = 64;
 
 subpixel_shift = line_subpixel_shift;
 subpixel_size  = 1 shl subpixel_shift;
 subpixel_mask  = subpixel_size - 1;

 aa_shift = 8;
 aa_num   = 1 shl aa_shift;
 aa_mask  = aa_num - 1;

type
 distance_interpolator_ptr = ^distance_interpolator;
 distance_interpolator = object
   procedure inc_x_; virtual; abstract;
   procedure dec_x_; virtual; abstract;
   procedure inc_y_; virtual; abstract;
   procedure dec_y_; virtual; abstract;

   procedure inc_x(dy_ : int ); virtual; abstract;
   procedure dec_x(dy_ : int ); virtual; abstract;
   procedure inc_y(dx_ : int ); virtual; abstract;
   procedure dec_y(dx_ : int ); virtual; abstract;

   function  dist : int; virtual; abstract;
   function  dist_start : int; virtual; abstract;
   function  dist_end : int; virtual; abstract;

   function  dx : int; virtual; abstract;
   function  dy : int; virtual; abstract;
   function  dx_start : int; virtual; abstract;
   function  dy_start : int; virtual; abstract;
   function  dx_end : int; virtual; abstract;
   function  dy_end : int; virtual; abstract;

  end;

 distance_interpolator0 = object(distance_interpolator )
   m_dx   ,
   m_dy   ,
   m_dist : int;

   constructor Construct; overload;
   constructor Construct(x1 ,y1 ,x2 ,y2 ,x ,y : int ); overload;

   procedure inc_x_; virtual;
   procedure dec_x_; virtual;
   procedure inc_y_; virtual;
   procedure dec_y_; virtual;

   procedure inc_x(dy_ : int ); virtual;
   procedure dec_x(dy_ : int ); virtual;
   procedure inc_y(dx_ : int ); virtual;
   procedure dec_y(dx_ : int ); virtual;

   function  dist : int; virtual;
   function  dx : int; virtual;
   function  dy : int; virtual;

  end;

 distance_interpolator1 = object(distance_interpolator )
   m_dx   ,
   m_dy   ,
   m_dist : int;

   constructor Construct; overload;
   constructor Construct(x1 ,y1 ,x2 ,y2 ,x ,y : int ); overload;

   procedure inc_x_; virtual;
   procedure dec_x_; virtual;
   procedure inc_y_; virtual;
   procedure dec_y_; virtual;

   procedure inc_x(dy_ : int ); virtual;
   procedure dec_x(dy_ : int ); virtual;
   procedure inc_y(dx_ : int ); virtual;
   procedure dec_y(dx_ : int ); virtual;

   function  dist : int; virtual;
   function  dx : int; virtual;
   function  dy : int; virtual;

  end;

 distance_interpolator2 = object(distance_interpolator )
   m_dx ,
   m_dy ,

   m_dx_start ,
   m_dy_start ,

   m_dist ,m_dist_start : int;

   constructor Construct; overload;
   constructor Construct(x1 ,y1 ,x2 ,y2 ,sx ,sy ,x ,y : int ); overload;
   constructor Construct(x1 ,y1 ,x2 ,y2 ,ex ,ey ,x ,y ,z : int ); overload;

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
   function  dist_end : int; virtual;

   function  dx : int; virtual;
   function  dy : int; virtual;
   function  dx_start : int; virtual;
   function  dy_start : int; virtual;
   function  dx_end : int; virtual;
   function  dy_end : int; virtual;

  end;

 distance_interpolator3 = object(distance_interpolator )
   m_dx ,
   m_dy ,

   m_dx_start ,
   m_dy_start ,

   m_dx_end ,
   m_dy_end ,

   m_dist ,m_dist_start ,m_dist_end : int;

   constructor Construct; overload;
   constructor Construct(x1 ,y1 ,x2 ,y2 ,sx ,sy ,ex ,ey ,x ,y : int ); overload;

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
   function  dist_end : int; virtual;

   function  dx : int; virtual;
   function  dy : int; virtual;
   function  dx_start : int; virtual;
   function  dy_start : int; virtual;
   function  dx_end : int; virtual;
   function  dy_end : int; virtual;

  end;

 renderer_outline_aa_ptr = ^renderer_outline_aa;

 line_interpolator = object
   function  step_hor : boolean; virtual; abstract;
   function  step_ver : boolean; virtual; abstract;

   function  width : int; virtual; abstract;
   function  count : int; virtual; abstract;

  end;

 line_interpolator_aa_base = object //(line_interpolator )
   m_lp  : line_parameters_ptr;
   m_li  : dda2_line_interpolator;
   m_ren : renderer_outline_aa_ptr;

   m_len ,
   m_x   ,
   m_y   ,

   m_old_x ,
   m_old_y ,
   m_count ,
   m_width ,

   m_max_extent ,
   m_step       : int;

   m_dist   : array[0..max_half_width + 1 - 1 ] of int;
   m_covers : array[0..max_half_width * 2 + 4 - 1 ] of int8u;

   constructor Construct(ren : renderer_outline_aa_ptr; lp : line_parameters_ptr );

   function  step_hor : boolean; virtual; abstract;
   function  step_ver : boolean; virtual; abstract;

   function  step_hor_base(di : distance_interpolator_ptr ) : int;
   function  step_ver_base(di : distance_interpolator_ptr ) : int;

   function  vertical : boolean;

   function  width : int; virtual;
   function  count : int; virtual;

  end;

 line_interpolator_aa0_ptr = ^line_interpolator_aa0;
 line_interpolator_aa0 = object(line_interpolator_aa_base )
   m_di : distance_interpolator1;

   constructor Construct(ren : renderer_outline_aa_ptr; lp : line_parameters_ptr );

   function  step_hor : boolean; virtual;
   function  step_ver : boolean; virtual;

  end;

 line_interpolator_aa1_ptr = ^line_interpolator_aa1;
 line_interpolator_aa1 = object(line_interpolator_aa_base )
   m_di : distance_interpolator2;

   constructor Construct(ren : renderer_outline_aa_ptr; lp : line_parameters_ptr; sx ,sy : int );

   function  step_hor : boolean; virtual;
   function  step_ver : boolean; virtual;

  end;

 line_interpolator_aa2_ptr = ^line_interpolator_aa2; 
 line_interpolator_aa2 = object(line_interpolator_aa_base )
   m_di : distance_interpolator2;

   constructor Construct(ren : renderer_outline_aa_ptr; lp : line_parameters_ptr; ex ,ey : int );

   function  step_hor : boolean; virtual;
   function  step_ver : boolean; virtual;

  end;

 line_interpolator_aa3_ptr = ^line_interpolator_aa3; 
 line_interpolator_aa3 = object(line_interpolator_aa_base )
   m_di : distance_interpolator3;

   constructor Construct(ren : renderer_outline_aa_ptr; lp : line_parameters_ptr; sx ,sy ,ex ,ey : int );

   function  step_hor : boolean; virtual;
   function  step_ver : boolean; virtual;

  end;

 line_profile_aa_ptr = ^line_profile_aa;
 line_profile_aa = object
   m_size    : unsigned;
   m_profile : int8u_ptr;
   m_gamma   : array[0..aa_num - 1 ] of int8u;

   m_subpixel_width : int;
   m_min_width      ,
   m_smoother_width : double;

   constructor Construct; overload;
   constructor Construct(w : double; gamma_function : vertex_source_ptr ); overload;
   destructor  Destruct;

   procedure min_width_     (w : double );
   procedure smoother_width_(w : double );

   procedure gamma_(gamma_function : vertex_source_ptr );
   procedure width_(w : double );

   function  _profile_size : unsigned;
   function  _subpixel_width : int;

   function  _min_width : double;
   function  _smoother_width : double;

   function  value(dist : int ) : int8u;

   function  profile(w : double ) : int8u_ptr;
   procedure set_   (center_width ,smoother_width : double );

  end;

 cmp_function = function(d : int ) : boolean;

 renderer_outline_ptr = ^renderer_outline;
 renderer_outline = object
   procedure color_(c : aggclr_ptr ); virtual; abstract;

   function  subpixel_width : int; virtual; abstract;
   function  accurate_join_only : boolean; virtual; abstract;

   procedure semidot(cmp : cmp_function; xc1 ,yc1 ,xc2 ,yc2 : int ); virtual; abstract;

   procedure line0(lp : line_parameters_ptr ); virtual; abstract;
   procedure line1(lp : line_parameters_ptr; sx ,sy : int ); virtual; abstract;
   procedure line2(lp : line_parameters_ptr; ex ,ey : int ); virtual; abstract;
   procedure line3(lp : line_parameters_ptr; sx ,sy ,ex ,ey : int ); virtual; abstract;

  end;

 renderer_outline_aa = object(renderer_outline )
   m_ren     : renderer_base_ptr;
   m_profile : line_profile_aa_ptr;
   m_color   : aggclr;

   constructor Construct(ren : renderer_base_ptr; prof : line_profile_aa_ptr );

   procedure color_(c : aggclr_ptr ); virtual;
   function  _color : aggclr_ptr;

   procedure profile_(prof : line_profile_aa_ptr );
   function  _profile : line_profile_aa_ptr;

   function  subpixel_width : int; virtual;
   function  cover(d : int ) : int8u;

   procedure blend_solid_hspan(x ,y : int; len : unsigned; covers : int8u_ptr );
   procedure blend_solid_vspan(x ,y : int; len : unsigned; covers : int8u_ptr );

   function  accurate_join_only : boolean; virtual;

   procedure semidot_hline(cmp : cmp_function; xc1 ,yc1 ,xc2 ,yc2 ,x1 ,y1 ,x2 : int );
   procedure semidot      (cmp : cmp_function; xc1 ,yc1 ,xc2 ,yc2 : int ); virtual;

   procedure line0(lp : line_parameters_ptr ); virtual;
   procedure line1(lp : line_parameters_ptr; sx ,sy : int ); virtual;
   procedure line2(lp : line_parameters_ptr; ex ,ey : int ); virtual;
   procedure line3(lp : line_parameters_ptr; sx ,sy ,ex ,ey : int ); virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor distance_interpolator0.Construct;
begin
end;

{ CONSTRUCT }
constructor distance_interpolator0.Construct(x1 ,y1 ,x2 ,y2 ,x ,y : int );
begin
 m_dx:=line_mr(x2 ) - line_mr(x1 );
 m_dy:=line_mr(y2 ) - line_mr(y1 );

 m_dist:=
  (line_mr(x + line_subpixel_size div 2 ) - line_mr(x2 ) ) * m_dy -
  (line_mr(y + line_subpixel_size div 2 ) - line_mr(y2 ) ) * m_dx;

 m_dx:=m_dx shl line_mr_subpixel_shift;
 m_dy:=m_dy shl line_mr_subpixel_shift;

end;

{ INC_X_ }
procedure distance_interpolator0.inc_x_;
begin
 inc(m_dist ,m_dy );

end;

{ DEC_X_ }
procedure distance_interpolator0.dec_x_;
begin
 dec(m_dist ,m_dy );

end;

{ INC_Y_ }
procedure distance_interpolator0.inc_y_;
begin
 inc(m_dist ,m_dx );

end;

{ DEC_Y_ }
procedure distance_interpolator0.dec_y_;
begin
 inc(m_dist ,m_dx );

end;

{ INC_X }
procedure distance_interpolator0.inc_x(dy_ : int );
begin
 inc(m_dist ,m_dy );

 if dy_ > 0 then
  dec(m_dist ,m_dx );

 if dy_ < 0 then
  inc(m_dist ,m_dx );

end;

{ DEC_X }
procedure distance_interpolator0.dec_x(dy_ : int );
begin
 dec(m_dist ,m_dy );

 if dy_ > 0 then
  dec(m_dist ,m_dx );

 if dy_ < 0 then
  inc(m_dist ,m_dx );

end;

{ INC_Y }
procedure distance_interpolator0.inc_y(dx_ : int );
begin
 dec(m_dist ,m_dx );

 if dx_ > 0 then
  inc(m_dist ,m_dy );

 if dx_ < 0 then
  dec(m_dist ,m_dy );

end;

{ DEC_Y }
procedure distance_interpolator0.dec_y(dx_ : int );
begin
 inc(m_dist ,m_dx );

 if dx_ > 0 then
  inc(m_dist ,m_dy );

 if dx_ < 0 then
  dec(m_dist ,m_dy );

end;

{ DIST }
function distance_interpolator0.dist;
begin
 result:=m_dist;

end;

{ DX }
function distance_interpolator0.dx;
begin
 result:=m_dx;

end;

{ DY }
function distance_interpolator0.dy;
begin
 result:=m_dy;

end;

{ CONSTRUCT }
constructor distance_interpolator1.Construct;
begin
end;

{ CONSTRUCT }
constructor distance_interpolator1.Construct(x1 ,y1 ,x2 ,y2 ,x ,y : int );
begin
 m_dx:=x2 - x1;
 m_dy:=y2 - y1;

 m_dist:=
  trunc(
   (x + line_subpixel_size / 2 - x2 ) * m_dy -
   (y + line_subpixel_size / 2 - y2 ) * m_dx );

 m_dx:=m_dx shl line_subpixel_shift;
 m_dy:=m_dy shl line_subpixel_shift;

end;

{ INC_X_ }
procedure distance_interpolator1.inc_x_;
begin
 inc(m_dist ,m_dy );

end;

{ DEC_X_ }
procedure distance_interpolator1.dec_x_;
begin
 dec(m_dist ,m_dy );

end;

{ INC_Y_ }
procedure distance_interpolator1.inc_y_;
begin
 dec(m_dist ,m_dx );

end;

{ DEC_Y_ }
procedure distance_interpolator1.dec_y_;
begin
 inc(m_dist ,m_dx );

end;

{ INC_X }
procedure distance_interpolator1.inc_x(dy_ : int );
begin
 inc(m_dist ,m_dy );

 if dy_ > 0 then
  dec(m_dist ,m_dx );

 if dy_ < 0 then
  inc(m_dist ,m_dx );

end;

{ DEC_X }
procedure distance_interpolator1.dec_x(dy_ : int );
begin
 dec(m_dist ,m_dy );

 if dy_ > 0 then
  dec(m_dist ,m_dx );

 if dy_ < 0 then
  inc(m_dist ,m_dx );

end;

{ INC_Y }
procedure distance_interpolator1.inc_y(dx_ : int );
begin
 dec(m_dist ,m_dx );

 if dx_ > 0 then
  inc(m_dist ,m_dy );

 if dx_ < 0 then
  dec(m_dist ,m_dy );

end;

{ DEC_Y }
procedure distance_interpolator1.dec_y(dx_ : int );
begin
 inc(m_dist ,m_dx );

 if dx_ > 0 then
  inc(m_dist ,m_dy );

 if dx_ < 0 then
  dec(m_dist ,m_dy );

end;

{ DIST }
function distance_interpolator1.dist;
begin
 result:=m_dist;

end;

{ DX }
function distance_interpolator1.dx;
begin
 result:=m_dx;

end;

{ DY }
function distance_interpolator1.dy;
begin
 result:=m_dy;

end;

{ CONSTRUCT }
constructor distance_interpolator2.Construct;
begin
end;

{ CONSTRUCT }
constructor distance_interpolator2.Construct(x1 ,y1 ,x2 ,y2 ,sx ,sy ,x ,y : int );
begin
 m_dx:=x2 - x1;
 m_dy:=y2 - y1;

 m_dx_start:=line_mr(sx ) - line_mr(x1 );
 m_dy_start:=line_mr(sy ) - line_mr(y1 );

 m_dist:=
  trunc(
   (x + line_subpixel_size / 2 - x2 ) * m_dy -
   (y + line_subpixel_size / 2 - y2 ) * m_dx );

 m_dist_start:=
  (line_mr(x + line_subpixel_size div 2 ) - line_mr(sx ) ) * m_dy_start -
  (line_mr(y + line_subpixel_size div 2 ) - line_mr(sy ) ) * m_dx_start;

 m_dx:=m_dx shl line_subpixel_shift;
 m_dy:=m_dy shl line_subpixel_shift;

 m_dx_start:=m_dx_start shl line_mr_subpixel_shift;
 m_dy_start:=m_dy_start shl line_mr_subpixel_shift;

end;

{ CONSTRUCT }
constructor distance_interpolator2.Construct(x1 ,y1 ,x2 ,y2 ,ex ,ey ,x ,y ,z : int );
begin
 m_dx:=x2 - x1;
 m_dy:=y2 - y1;

 m_dx_start:=line_mr(ex ) - line_mr(x2 );
 m_dy_start:=line_mr(ey ) - line_mr(y2 );

 m_dist:=
  trunc(
   (x + line_subpixel_size / 2 - x2 ) * m_dy -
   (y + line_subpixel_size / 2 - y2 ) * m_dx );

 m_dist_start:=
  (line_mr(x + line_subpixel_size div 2 ) - line_mr(ex ) ) * m_dy_start -
  (line_mr(y + line_subpixel_size div 2 ) - line_mr(ey ) ) * m_dx_start;

 m_dx:=m_dx shl line_subpixel_shift;
 m_dy:=m_dy shl line_subpixel_shift;

 m_dx_start:=m_dx_start shl line_mr_subpixel_shift;
 m_dy_start:=m_dy_start shl line_mr_subpixel_shift;

end;

{ INC_X_ }
procedure distance_interpolator2.inc_x_;
begin
 inc(m_dist ,m_dy );
 inc(m_dist_start ,m_dy_start );

end;

{ DEC_X_ }
procedure distance_interpolator2.dec_x_;
begin
 dec(m_dist ,m_dy );
 dec(m_dist_start ,m_dy_start );

end;

{ INC_Y_ }
procedure distance_interpolator2.inc_y_;
begin
 dec(m_dist ,m_dx );
 dec(m_dist_start ,m_dx_start );

end;

{ DEC_Y_ }
procedure distance_interpolator2.dec_y_;
begin
 inc(m_dist ,m_dx );
 inc(m_dist_start ,m_dx_start );

end;

{ INC_X }
procedure distance_interpolator2.inc_x(dy_ : int );
begin
 inc(m_dist ,m_dy );
 inc(m_dist_start ,m_dy_start );

 if dy_ > 0 then
  begin
   dec(m_dist ,m_dx );
   dec(m_dist_start ,m_dx_start );

  end;

 if dy_ < 0 then
  begin
   inc(m_dist ,m_dx );
   inc(m_dist_start ,m_dx_start );

  end;

end;

{ DEC_X }
procedure distance_interpolator2.dec_x(dy_ : int );
begin
 dec(m_dist ,m_dy );
 dec(m_dist_start ,m_dy_start );

 if dy_ > 0 then
  begin
   dec(m_dist ,m_dx );
   dec(m_dist_start ,m_dx_start );

  end;

 if dy_ < 0 then
  begin
   inc(m_dist ,m_dx );
   inc(m_dist_start ,m_dx_start );

  end;

end;

{ INC_Y }
procedure distance_interpolator2.inc_y(dx_ : int );
begin
 dec(m_dist ,m_dx );
 dec(m_dist_start ,m_dx_start );

 if dx_ > 0 then
  begin
   inc(m_dist ,m_dy );
   inc(m_dist_start ,m_dy_start );

  end;

 if dx_ < 0 then
  begin
   dec(m_dist ,m_dy );
   dec(m_dist_start ,m_dy_start );

  end;

end;

{ DEC_Y }
procedure distance_interpolator2.dec_y(dx_ : int );
begin
 inc(m_dist ,m_dx );
 inc(m_dist_start ,m_dx_start );

 if dx_ > 0 then
  begin
   inc(m_dist ,m_dy );
   inc(m_dist_start ,m_dy_start );

  end;

 if dx_ < 0 then
  begin
   dec(m_dist ,m_dy );
   dec(m_dist_start ,m_dy_start );

  end;

end;

{ DIST }
function distance_interpolator2.dist;
begin
 result:=m_dist;

end;

{ DIST_START }
function distance_interpolator2.dist_start;
begin
 result:=m_dist_start;

end;

{ DIST_END }
function distance_interpolator2.dist_end;
begin
 result:=m_dist_start;

end;

{ DX }
function distance_interpolator2.dx;
begin
 result:=m_dx;

end;

{ DY }
function distance_interpolator2.dy;
begin
 result:=m_dy;

end;

{ DX_START }
function distance_interpolator2.dx_start;
begin
 result:=m_dx_start;

end;

{ DY_START }
function distance_interpolator2.dy_start;
begin
 result:=m_dy_start;

end;

{ DX_END }
function distance_interpolator2.dx_end;
begin
 result:=m_dx_start;

end;

{ DY_END }
function distance_interpolator2.dy_end;
begin
 result:=m_dy_start;

end;

{ CONSTRUCT }
constructor distance_interpolator3.Construct;
begin
end;

{ CONSTRUCT }
constructor distance_interpolator3.Construct(x1 ,y1 ,x2 ,y2 ,sx ,sy ,ex ,ey ,x ,y : int );
begin
 m_dx:=x2 - x1;
 m_dy:=y2 - y1;

 m_dx_start:=line_mr(sx ) - line_mr(x1 );
 m_dy_start:=line_mr(sy ) - line_mr(y1 );

 m_dx_end:=line_mr(ex ) - line_mr(x2 );
 m_dy_end:=line_mr(ey ) - line_mr(y2 );

 m_dist:=
  trunc(
   (x + line_subpixel_size / 2 - x2 ) * m_dy -
   (y + line_subpixel_size / 2 - y2 ) * m_dx );

 m_dist_start:=
  (line_mr(x + line_subpixel_size div 2 ) - line_mr(sx ) ) * m_dy_start -
  (line_mr(y + line_subpixel_size div 2 ) - line_mr(sy ) ) * m_dx_start;

 m_dist_end:=
  (line_mr(x + line_subpixel_size div 2 ) - line_mr(ex ) ) * m_dy_end -
  (line_mr(y + line_subpixel_size div 2 ) - line_mr(ey ) ) * m_dx_end;

 m_dx:=m_dx shl line_subpixel_shift;
 m_dy:=m_dy shl line_subpixel_shift;

 m_dx_start:=m_dx_start shl line_mr_subpixel_shift;
 m_dy_start:=m_dy_start shl line_mr_subpixel_shift;

 m_dx_end:=m_dx_end shl line_mr_subpixel_shift;
 m_dy_end:=m_dy_end shl line_mr_subpixel_shift;

end;

{ INC_X_ }
procedure distance_interpolator3.inc_x_;
begin
 inc(m_dist ,m_dy );
 inc(m_dist_start ,m_dy_start );
 inc(m_dist_end ,m_dy_end );

end;

{ DEC_X_ }
procedure distance_interpolator3.dec_x_;
begin
 dec(m_dist ,m_dy );
 dec(m_dist_start ,m_dy_start );
 dec(m_dist_end ,m_dy_end );

end;

{ INC_Y_ }
procedure distance_interpolator3.inc_y_;
begin
 dec(m_dist ,m_dx );
 dec(m_dist_start ,m_dx_start );
 dec(m_dist_end ,m_dx_end );

end;

{ DEC_Y_ }
procedure distance_interpolator3.dec_y_;
begin
 inc(m_dist ,m_dx );
 inc(m_dist_start ,m_dx_start );
 inc(m_dist_end ,m_dx_end );

end;

{ INC_X }
procedure distance_interpolator3.inc_x(dy_ : int );
begin
 inc(m_dist ,m_dy );
 inc(m_dist_start ,m_dy_start );
 inc(m_dist_end ,m_dy_end );

 if dy_ > 0 then
  begin
   dec(m_dist ,m_dx );
   dec(m_dist_start ,m_dx_start );
   dec(m_dist_end ,m_dx_end );

  end;

 if dy_ < 0 then
  begin
   inc(m_dist ,m_dx );
   inc(m_dist_start ,m_dx_start );
   inc(m_dist_end ,m_dx_end );

  end;

end;

{ DEC_X }
procedure distance_interpolator3.dec_x(dy_ : int );
begin
 dec(m_dist ,m_dy );
 dec(m_dist_start ,m_dy_start );
 dec(m_dist_end ,m_dy_end );

 if dy_ > 0 then
  begin
   dec(m_dist ,m_dx );
   dec(m_dist_start ,m_dx_start );
   dec(m_dist_end ,m_dx_end );

  end;

 if dy_ < 0 then
  begin
   inc(m_dist ,m_dx );
   inc(m_dist_start ,m_dx_start );
   inc(m_dist_end ,m_dx_end );

  end;

end;

{ INC_Y }
procedure distance_interpolator3.inc_y(dx_ : int );
begin
 dec(m_dist ,m_dx );
 dec(m_dist_start ,m_dx_start );
 dec(m_dist_end ,m_dx_end );

 if dx_ > 0 then
  begin
   inc(m_dist ,m_dy );
   inc(m_dist_start ,m_dy_start );
   inc(m_dist_end ,m_dy_end );

  end;

 if dx_ < 0 then
  begin
   dec(m_dist ,m_dy );
   dec(m_dist_start ,m_dy_start );
   dec(m_dist_end ,m_dy_end );

  end;

end;

{ DEC_Y }
procedure distance_interpolator3.dec_y(dx_ : int );
begin
 inc(m_dist ,m_dx );
 inc(m_dist_start ,m_dx_start );
 inc(m_dist_end ,m_dx_end );

 if dx_ > 0 then
  begin
   inc(m_dist ,m_dy );
   inc(m_dist_start ,m_dy_start );
   inc(m_dist_end ,m_dy_end );

  end;

 if dx_ < 0 then
  begin
   dec(m_dist ,m_dy );
   dec(m_dist_start ,m_dy_start );
   dec(m_dist_end ,m_dy_end );

  end;

end;

{ DIST }
function distance_interpolator3.dist;
begin
 result:=m_dist;

end;

{ DIST_START }
function distance_interpolator3.dist_start;
begin
 result:=m_dist_start;

end;

{ DIST_END }
function distance_interpolator3.dist_end;
begin
 result:=m_dist_end;

end;

{ DX }
function distance_interpolator3.dx;
begin
 result:=m_dx;

end;

{ DY }
function distance_interpolator3.dy;
begin
 result:=m_dy;

end;

{ DX_START }
function distance_interpolator3.dx_start;
begin
 result:=m_dx_start;

end;

{ DY_START }
function distance_interpolator3.dy_start;
begin
 result:=m_dy_start;

end;

{ DX_END }
function distance_interpolator3.dx_end;
begin
 result:=m_dx_end;

end;

{ DY_END }
function distance_interpolator3.dy_end;
begin
 result:=m_dy_end;

end;

{ CONSTRUCT }
constructor line_interpolator_aa_base.Construct;
var
 li : dda2_line_interpolator;
 i  : unsigned;

 stop : int;

begin
 m_lp:=lp;

 if lp.vertical then
  m_li.Construct(line_dbl_hr(lp.x2 - lp.x1 ) ,Abs(lp.y2 - lp.y1 ) )
 else
  m_li.Construct(line_dbl_hr(lp.y2 - lp.y1 ) ,Abs(lp.x2 - lp.x1) + 1 );

 m_ren:=ren;

 if lp.vertical = (lp.inc_ > 0 ) then
  m_len:=-lp.len
 else
  m_len:=lp.len;

 m_x:=shr_int32(lp.x1 ,line_subpixel_shift );
 m_y:=shr_int32(lp.y1 ,line_subpixel_shift );

 m_old_x:=m_x;
 m_old_y:=m_y;

 if lp.vertical then
  m_count:=Abs(shr_int32(lp.y2 ,line_subpixel_shift) - m_y )
 else
  m_count:=Abs(shr_int32(lp.x2 ,line_subpixel_shift) - m_x );

 m_width     :=ren.subpixel_width;
 m_max_extent:=shr_int32(m_width ,line_subpixel_shift - 2 );
 m_step      :=0;

 if lp.vertical then
  li.Construct(0 ,lp.dy shl line_subpixel_shift ,lp.len )
 else
  li.Construct(0 ,lp.dx shl line_subpixel_shift ,lp.len );

 stop:=m_width + line_subpixel_size * 2;

 i:=0;

 while i < max_half_width do
  begin
   m_dist[i ]:=li._y;

   if m_dist[i ] >= stop then
    break;

   li.plus_operator;

   inc(i );

  end;

 m_dist[i ]:=$7FFF0000;

end;

{ STEP_HOR_BASE }
function line_interpolator_aa_base.step_hor_base;
begin
 m_li.plus_operator;

 inc(m_x ,m_lp.inc_ );

 m_y:=shr_int32(m_lp.y1 + m_li._y ,line_subpixel_shift );

 if m_lp.inc_ > 0 then
  di.inc_x(m_y - m_old_y )
 else
  di.dec_x(m_y - m_old_y );

 m_old_y:=m_y;

 result:=di.dist div m_len;

end;

{ STEP_VER_BASE }
function line_interpolator_aa_base.step_ver_base;
begin
 m_li.plus_operator;

 inc(m_y ,m_lp.inc_ );

 m_x:=shr_int32(m_lp.x1 + m_li._y ,line_subpixel_shift );

 if m_lp.inc_ > 0 then
  di.inc_y(m_x - m_old_x )
 else
  di.dec_y(m_x - m_old_x );

 m_old_x:=m_x;

 result:=di.dist div m_len;

end;

{ VERTICAL }
function line_interpolator_aa_base.vertical;
begin
 result:=m_lp.vertical;

end;

{ WIDTH }
function line_interpolator_aa_base.width;
begin
 result:=m_width;

end;

{ COUNT }
function line_interpolator_aa_base.count;
begin
 result:=m_count;

end;

{ CONSTRUCT }
constructor line_interpolator_aa0.Construct;
begin
 inherited Construct(ren ,lp );

 m_di.Construct(
  lp.x1 ,lp.y1 ,lp.x2 ,lp.y2 ,
  lp.x1 and not line_subpixel_mask ,
  lp.y1 and not line_subpixel_mask );

 m_li.adjust_forward; 

end;

{ STEP_HOR }
function line_interpolator_aa0.step_hor;
var
 dist ,dy ,s1 : int;

 p0 ,p1 : int8u_ptr;

begin
 s1:=step_hor_base(@m_di );
 p0:=int8u_ptr(ptrcomp(@m_covers[0 ] ) + (max_half_width + 2 ) * sizeof(int8u ) );
 p1:=p0;

 p1^:=int8u(m_ren.cover(s1 ) );

 inc(ptrcomp(p1 ) ,sizeof(int8u ) );

 dy  :=1;
 dist:=m_dist[dy ] - s1;

 while dist <= m_width do
  begin
   p1^:=int8u(m_ren.cover(dist ) );

   inc(ptrcomp(p1 ) ,sizeof(int8u ) );
   inc(dy );

   dist:=m_dist[dy ] - s1;

  end;

 dy  :=1;
 dist:=m_dist[dy ] + s1;

 while dist <= m_width do
  begin
   dec(ptrcomp(p0 ) ,sizeof(int8u ) );

   p0^:=int8u(m_ren.cover(dist ) );

   inc(dy );

   dist:=m_dist[dy ] + s1;

  end;

 m_ren.blend_solid_vspan(
  m_x ,m_y - dy + 1 ,
  unsigned((ptrcomp(p1 ) - ptrcomp(p0 ) ) div sizeof(int8u ) ) ,
  p0 );

 inc(m_step );

 result:=m_step < m_count;

end;

{ STEP_VER }
function line_interpolator_aa0.step_ver;
var
 dist ,dx ,s1 : int;

 p0 ,p1 : int8u_ptr;

begin
 s1:=step_ver_base(@m_di );
 p0:=int8u_ptr(ptrcomp(@m_covers[0 ] ) + (max_half_width + 2 ) * sizeof(int8u ) );
 p1:=p0;

 p1^:=int8u(m_ren.cover(s1 ) );

 inc(ptrcomp(p1 ) ,sizeof(int8u ) );

 dx  :=1;
 dist:=m_dist[dx ] - s1;

 while dist <= m_width do
  begin
   p1^:=int8u(m_ren.cover(dist ) );

   inc(ptrcomp(p1 ) ,sizeof(int8u ) );
   inc(dx );

   dist:=m_dist[dx ] - s1;

  end;

 dx  :=1;
 dist:=m_dist[dx ] + s1;

 while dist <= m_width do
  begin
   dec(ptrcomp(p0 ) ,sizeof(int8u ) );

   p0^:=int8u(m_ren.cover(dist ) );

   inc(dx );

   dist:=m_dist[dx ] + s1;

  end;

 m_ren.blend_solid_hspan(
  m_x - dx + 1 ,m_y ,
  unsigned((ptrcomp(p1 ) - ptrcomp(p0 ) ) div sizeof(int8u ) ) ,
  p0 );

 inc(m_step );

 result:=m_step < m_count;

end;

{ CONSTRUCT }
constructor line_interpolator_aa1.Construct;
var
 dist1_start ,dist2_start ,npix ,dx ,dy : int;

begin
 inherited Construct(ren ,lp );

 m_di.Construct(
  lp.x1 ,lp.y1 ,lp.x2 ,lp.y2 ,sx ,sy ,
  lp.x1 and not line_subpixel_mask ,
  lp.y1 and not line_subpixel_mask );

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

   until m_dist[dx ] > m_width;

   dec(m_step );

   if npix = 0 then
    break;

   npix:=0;

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

   until m_dist[dy ] > m_width;

   dec(m_step );

   if npix = 0 then
    break;

   npix:=0; 

  until m_step < -m_max_extent;

 m_li.adjust_forward;

end;

{ STEP_HOR }
function line_interpolator_aa1.step_hor;
var
 dist_start ,dist ,dy ,s1 : int;

 p0 ,p1 : int8u_ptr;

begin
 s1:=step_hor_base(@m_di );

 dist_start:=m_di.dist_start;

 p0:=int8u_ptr(ptrcomp(@m_covers[0 ] ) + (max_half_width + 2 ) * sizeof(int8u ) );
 p1:=p0;

 p1^:=0;

 if dist_start <= 0 then
  p1^:=int8u(m_ren.cover(s1 ) );

 inc(ptrcomp(p1 ) ,sizeof(int8u ) );

 dy  :=1;
 dist:=m_dist[dy ] - s1;

 while dist <= m_width do
  begin
   dec(dist_start ,m_di.dx_start );

   p1^:=0;

   if dist_start <= 0 then
    p1^:=int8u(m_ren.cover(dist ) );

   inc(ptrcomp(p1 ) ,sizeof(int8u ) );
   inc(dy );

   dist:=m_dist[dy ] - s1;

  end;

 dy        :=1;
 dist_start:=m_di.dist_start;
 dist      :=m_dist[dy ] + s1;

 while dist <= m_width do
  begin
   inc(dist_start ,m_di.dx_start );
   dec(ptrcomp(p0 ) ,sizeof(int8u ) );

   p0^:=0;

   if dist_start <= 0 then
    p0^:=int8u(m_ren.cover(dist ) );

   inc(dy );

   dist:=m_dist[dy ] + s1;

  end;

 m_ren.blend_solid_vspan(
  m_x ,m_y - dy + 1 ,
  unsigned((ptrcomp(p1 ) - ptrcomp(p0 ) ) div sizeof(int8u ) ) ,
  p0 );

 inc(m_step );

 result:=m_step < m_count;

end;

{ STEP_VER }
function line_interpolator_aa1.step_ver;
var
 dist_start ,dist ,dx ,s1 : int;

 p0 ,p1 : int8u_ptr;

begin
 s1:=step_ver_base(@m_di );
 p0:=int8u_ptr(ptrcomp(@m_covers[0 ] ) + (max_half_width + 2 ) * sizeof(int8u ) );
 p1:=p0;

 dist_start:=m_di.dist_start;

 p1^:=0;

 if dist_start <= 0 then
  p1^:=int8u(m_ren.cover(s1 ) );

 inc(ptrcomp(p1 ) ,sizeof(int8u ) );

 dx  :=1;
 dist:=m_dist[dx ] - s1;

 while dist <= m_width do
  begin
   inc(dist_start ,m_di.dy_start );

   p1^:=0;

   if dist_start <= 0 then
    p1^:=int8u(m_ren.cover(dist ) );

   inc(ptrcomp(p1 ) ,sizeof(int8u ) );
   inc(dx );

   dist:=m_dist[dx ] - s1;

  end;

 dx        :=1;
 dist_start:=m_di.dist_start;
 dist      :=m_dist[dx ] + s1;

 while dist <= m_width do
  begin
   dec(dist_start ,m_di.dy_start );
   dec(ptrcomp(p0 ) ,sizeof(int8u ) );

   p0^:=0;

   if dist_start <= 0 then
    p0^:=int8u(m_ren.cover(dist ) );

   inc(dx );

   dist:=m_dist[dx ] + s1;

  end;

 m_ren.blend_solid_hspan(
  m_x - dx + 1 ,m_y ,
  unsigned((ptrcomp(p1 ) - ptrcomp(p0 ) ) div sizeof(int8u ) ) ,
  p0 );

 inc(m_step );

 result:=m_step < m_count;

end;

{ CONSTRUCT }
constructor line_interpolator_aa2.Construct;
begin
 inherited Construct(ren ,lp );

 m_di.Construct(
  lp.x1 ,lp.y1 ,lp.x2 ,lp.y2 ,ex ,ey ,
  lp.x1 and not line_subpixel_mask ,
  lp.y1 and not line_subpixel_mask ,
  0 );

 m_li.adjust_forward;

 dec(m_step ,m_max_extent );

end;

{ STEP_HOR }
function line_interpolator_aa2.step_hor;
var
 dist_end ,dist ,dy ,s1 ,npix : int;

 p0 ,p1 : int8u_ptr;

begin
 s1:=step_hor_base(@m_di );
 p0:=int8u_ptr(ptrcomp(@m_covers[0 ] ) + (max_half_width + 2 ) * sizeof(int8u ) );
 p1:=p0;

 dist_end:=m_di.dist_end;

 npix:=0;
 p1^ :=0;

 if dist_end > 0 then
  begin
   p1^:=int8u(m_ren.cover(s1 ) );

   inc(npix );

  end;

 inc(ptrcomp(p1 ) ,sizeof(int8u ) );

 dy  :=1;
 dist:=m_dist[dy ] - s1;

 while dist <= m_width do
  begin
   dec(dist_end ,m_di.dx_end );

   p1^:=0;

   if dist_end > 0 then
    begin
     p1^:=int8u(m_ren.cover(dist ) );

     inc(npix );

    end;

   inc(ptrcomp(p1 ) ,sizeof(int8u ) );
   inc(dy );

   dist:=m_dist[dy ] - s1;

  end;

 dy      :=1;
 dist_end:=m_di.dist_end;
 dist    :=m_dist[dy ] + s1;

 while dist <= m_width do
  begin
   inc(dist_end ,m_di.dx_end );
   dec(ptrcomp(p0 ) ,sizeof(int8u ) );

   p0^:=0;

   if dist_end > 0 then
    begin
     p0^:=int8u(m_ren.cover(dist ) );

     inc(npix );

    end;

   inc(dy );

   dist:=m_dist[dy ] + s1;

  end;

 m_ren.blend_solid_vspan(
  m_x ,m_y - dy + 1 ,
  unsigned((ptrcomp(p1 ) - ptrcomp(p0 ) ) div sizeof(int8u ) ) ,
  p0 );

 inc(m_step );

 result:=
  (npix <> 0 ) and
  (m_step < m_count );

end;

{ STEP_VER }
function line_interpolator_aa2.step_ver;
var
 dist_end ,dist ,dx ,s1 ,npix : int;

 p0 ,p1 : int8u_ptr;

begin
 s1:=step_ver_base(@m_di );
 p0:=int8u_ptr(ptrcomp(@m_covers[0 ] ) + (max_half_width + 2 ) * sizeof(int8u ) );
 p1:=p0;

 dist_end:=m_di.dist_end;

 npix:=0;
 p1^ :=0;

 if dist_end > 0 then
  begin
   p1^:=int8u(m_ren.cover(s1 ) );

   inc(npix );

  end;

 inc(ptrcomp(p1 ) ,sizeof(int8u ) );

 dx  :=1;
 dist:=m_dist[dx ] - s1;

 while dist <= m_width do
  begin
   inc(dist_end ,m_di.dy_end );

   p1^:=0;

   if dist_end > 0 then
    begin
     p1^:=int8u(m_ren.cover(dist ) );

     inc(npix );

    end;

   inc(ptrcomp(p1 ) ,sizeof(int8u ) );
   inc(dx );

   dist:=m_dist[dx ] - s1;

  end;

 dx      :=1;
 dist_end:=m_di.dist_end;
 dist    :=m_dist[dx ] + s1;

 while dist <= m_width do
  begin
   dec(dist_end ,m_di.dy_end );
   dec(ptrcomp(p0 ) ,sizeof(int8u ) );

   p0^:=0;

   if dist_end > 0 then
    begin
     p0^:=int8u(m_ren.cover(dist ) );

     inc(npix );

    end;

   inc(dx );

   dist:=m_dist[dx ] + s1;

  end;

 m_ren.blend_solid_hspan(
  m_x - dx + 1 ,m_y ,
  unsigned((ptrcomp(p1 ) - ptrcomp(p0 ) ) div sizeof(int8u ) ) ,
  p0 );

 inc(m_step );

 result:=
  (npix <> 0 ) and
  (m_step < m_count );

end;

{ CONSTRUCT }
constructor line_interpolator_aa3.Construct;
var
 dist1_start ,dist2_start ,npix ,dx ,dy : int;

begin
 inherited Construct(ren ,lp );

 m_di.Construct(
  lp.x1 ,lp.y1 ,lp.x2 ,lp.y2 ,sx ,sy ,ex ,ey ,
  lp.x1 and not line_subpixel_mask ,
  lp.y1 and not line_subpixel_mask );

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

   until m_dist[dx ] > m_width;

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

   until m_dist[dy ] > m_width;

   if npix = 0 then
    break;

   npix:=0;

   dec(m_step );

  until m_step < -m_max_extent;

 m_li.adjust_forward;

 dec(m_step ,m_max_extent );

end;

{ STEP_HOR }
function line_interpolator_aa3.step_hor;
var
 dist_start ,dist_end ,dist ,dy ,s1 ,npix : int;

 p0 ,p1 : int8u_ptr;

begin
 s1:=step_hor_base(@m_di );
 p0:=int8u_ptr(ptrcomp(@m_covers[0 ] ) + (max_half_width + 2 ) * sizeof(int8u ) );
 p1:=p0;

 dist_start:=m_di.dist_start;
 dist_end  :=m_di.dist_end;

 npix:=0;
 p1^ :=0;

 if dist_end > 0 then
  begin
   if dist_start <= 0 then
    p1^:=int8u(m_ren.cover(s1 ) );

   inc(npix );

  end;

 inc(ptrcomp(p1 ) ,sizeof(int8u ) );

 dy  :=1;
 dist:=m_dist[dy ] - s1;

 while dist <= m_width do
  begin
   dec(dist_start ,m_di.dx_start );
   dec(dist_end ,m_di.dx_end );

   p1^:=0;

   if (dist_end > 0 ) and
      (dist_start <= 0 ) then
    begin
     p1^:=int8u(m_ren.cover(dist ) );

     inc(npix );

    end;

   inc(ptrcomp(p1 ) ,sizeof(int8u ) );
   inc(dy );

   dist:=m_dist[dy ] - s1;

  end;

 dy        :=1;
 dist_start:=m_di.dist_start;
 dist_end  :=m_di.dist_end;
 dist      :=m_dist[dy ] + s1;

 while dist <= m_width do
  begin
   inc(dist_start ,m_di.dx_start );
   inc(dist_end ,m_di.dx_end );
   dec(ptrcomp(p0 ) ,sizeof(int8u ) );

   p0^:=0;

   if (dist_end > 0 ) and
      (dist_start <= 0 ) then
    begin
     p0^:=int8u(m_ren.cover(dist ) );

     inc(npix );

    end;

   inc(dy );

   dist:=m_dist[dy ] + s1;

  end;

 m_ren.blend_solid_vspan(
  m_x ,m_y - dy + 1 ,
  unsigned((ptrcomp(p1 ) - ptrcomp(p0 ) ) div sizeof(int8u ) ) ,
  p0 );

 inc(m_step );

 result:=
  (npix <> 0 ) and
  (m_step < m_count );

end;

{ STEP_VER }
function line_interpolator_aa3.step_ver;
var
 dist_start ,dist_end ,dist ,dx ,s1 ,npix : int;

 p0 ,p1 : int8u_ptr;

begin
 s1:=step_ver_base(@m_di );
 p0:=int8u_ptr(ptrcomp(@m_covers[0 ] ) + (max_half_width + 2 ) * sizeof(int8u ) );
 p1:=p0;

 dist_start:=m_di.dist_start;
 dist_end  :=m_di.dist_end;

 npix:=0;
 p1^ :=0;

 if dist_end > 0 then
  begin
   if dist_start <= 0 then
    p1^:=int8u(m_ren.cover(s1 ) );

   inc(npix );

  end;

 inc(ptrcomp(p1 ) ,sizeof(int8u ) );

 dx  :=1;
 dist:=m_dist[dx ] - s1;

 while dist <= m_width do
  begin
   inc(dist_start ,m_di.dy_start );
   inc(dist_end ,m_di.dy_end );

   p1^:=0;

   if (dist_end > 0 ) and
      (dist_start <= 0 ) then
    begin
     p1^:=int8u(m_ren.cover(dist ) );

     inc(npix );

    end;

   inc(ptrcomp(p1 ) ,sizeof(int8u ) );
   inc(dx );

   dist:=m_dist[dx ] - s1;

  end;

 dx        :=1;
 dist_start:=m_di.dist_start;
 dist_end  :=m_di.dist_end;
 dist      :=m_dist[dx ] + s1;

 while dist <= m_width do
  begin
   dec(dist_start ,m_di.dy_start );
   dec(dist_end ,m_di.dy_end );
   dec(ptrcomp(p0 ) ,sizeof(int8u ) );

   p0^:=0;

   if (dist_end > 0 ) and
      (dist_start <= 0 ) then
    begin
     p0^:=int8u(m_ren.cover(dist ) );

     inc(npix );

    end;

   inc(dx );

   dist:=m_dist[dx ] + s1;

  end;

 m_ren.blend_solid_hspan(
  m_x - dx + 1 ,m_y ,
  unsigned((ptrcomp(p1 ) - ptrcomp(p0 ) ) div sizeof(int8u ) ) ,
  p0 );

 inc(m_step );

 result:=
  (npix <> 0 ) and
  (m_step < m_count );

end;

{ CONSTRUCT }
constructor line_profile_aa.Construct;
var
 i : int;

begin
 m_size   :=0;
 m_profile:=0;

 m_subpixel_width:=0;
 m_min_width     :=1.0;
 m_smoother_width:=1.0;

 for i:=0 to aa_num - 1 do
  m_gamma[i ]:=int8u(i );

end;

{ CONSTRUCT }
constructor line_profile_aa.Construct(w : double; gamma_function : vertex_source_ptr );
begin
 m_size   :=0;
 m_profile:=0;

 m_subpixel_width:=0;
 m_min_width     :=1.0;
 m_smoother_width:=1.0;

 gamma_(gamma_function );
 width_(w );

end;

{ DESTRUCT }
destructor line_profile_aa.Destruct;
begin
 agg_freemem(pointer(m_profile ) ,m_size * sizeof(int8u ) );

end;

{ MIN_WIDTH_ }
procedure line_profile_aa.min_width_;
begin
 m_min_width:=w;

end;

{ SMOOTHER_WIDTH_ }
procedure line_profile_aa.smoother_width_;
begin
 m_smoother_width:=w;

end;

{ GAMMA_ }
procedure line_profile_aa.gamma_;
var
 i : int;

begin
 for i:=0 to aa_num - 1 do
  m_gamma[i ]:=int8u(trunc(gamma_function.func_operator_gamma(i / aa_mask ) * aa_mask + 0.5 ) );

end;

{ WIDTH_ }
procedure line_profile_aa.width_;
var
 s : double;

begin
 if w < 0.0 then
  w:=0.0;

 if w < m_smoother_width then
  w:=w + w
 else
  w:=w + m_smoother_width;

 w:=w * 0.5;
 w:=w - m_smoother_width;
 s:=m_smoother_width;

 if w < 0.0 then
  begin
   s:=s + w;
   w:=0.0;

  end;

 set_(w ,s );

end;

{ _PROFILE_SIZE }
function line_profile_aa._profile_size;
begin
 result:=m_size;

end;

{ _SUBPIXEL_WIDTH }
function line_profile_aa._subpixel_width;
begin
 result:=m_subpixel_width;

end;

{ _MIN_WIDTH }
function line_profile_aa._min_width;
begin
 result:=m_min_width;

end;

{ _SMOOTHER_WIDTH }
function line_profile_aa._smoother_width;
begin
 result:=m_smoother_width;

end;

{ VALUE }
function line_profile_aa.value;
begin
 result:=
  int8u_ptr(
   ptrcomp(m_profile ) + (dist + subpixel_size * 2 ) * sizeof(int8u ) )^;

end;

{ PROFILE }
function line_profile_aa.profile;
var
 size : unsigned;

begin
 m_subpixel_width:=trunc(w * subpixel_size );

 size:=m_subpixel_width + subpixel_size * 6;

 if size > m_size then
  begin
   agg_freemem(pointer(m_profile ) ,m_size * sizeof(int8u ) );
   agg_getmem (pointer(m_profile ) ,size * sizeof(int8u ) );

   m_size:=size;

  end;

 result:=m_profile;

end;

{ SET_ }
procedure line_profile_aa.set_;
var
 base_val ,width ,k : double;

 subpixel_center_width ,subpixel_smoother_width ,i ,val ,n_smoother : unsigned;

 ch ,ch_center ,ch_smoother : int8u_ptr;

begin
 base_val:=1.0;

 if center_width = 0.0 then
  center_width:=1.0 / subpixel_size;

 if smoother_width = 0.0 then
  smoother_width:=1.0 / subpixel_size;

 width:=center_width + smoother_width;

 if width < m_min_width then
  begin
   k:=width / m_min_width;

   base_val      :=base_val * k;
   center_width  :=center_width / k;
   smoother_width:=smoother_width / k;

  end;

 ch:=profile(center_width + smoother_width );

 subpixel_center_width  :=trunc(center_width * subpixel_size);
 subpixel_smoother_width:=trunc(smoother_width * subpixel_size);

 ch_center  :=int8u_ptr(ptrcomp(ch ) + subpixel_size * 2 * sizeof(int8u ) );
 ch_smoother:=int8u_ptr(ptrcomp(ch_center ) + subpixel_center_width * sizeof(int8u ) );

 val:=m_gamma[trunc(base_val * aa_mask ) ];

 ch:=ch_center;

 i:=0;

 while i < subpixel_center_width do
  begin
   ch^:=int8u(val );

   inc(ptrcomp(ch ) ,sizeof(int8u ) );
   inc(i );

  end;

 i:=0;

 while i < subpixel_smoother_width do
  begin
   ch_smoother^:=
    m_gamma[
     trunc(
      (base_val - base_val * (i / subpixel_smoother_width ) ) * aa_mask ) ];

   inc(ptrcomp(ch_smoother ) ,sizeof(int8u ) );
   inc(i );

  end;

 n_smoother:=
  _profile_size -
  subpixel_smoother_width -
  subpixel_center_width -
  subpixel_size * 2;

 val:=m_gamma[0 ];

 for i:=0 to n_smoother - 1 do
  begin
   ch_smoother^:=int8u(val );

   inc(ptrcomp(ch_smoother ) ,sizeof(int8u ) );

  end;

 ch:=ch_center;

 for i:=0 to subpixel_size * 2 - 1 do
  begin
   ch^:=ch_center^;

   dec(ptrcomp(ch ) ,sizeof(int8u ) );
   inc(ptrcomp(ch_center ) ,sizeof(int8u ) );

  end;

end;

{ CONSTRUCT }
constructor renderer_outline_aa.Construct;
begin
 m_ren    :=ren;
 m_profile:=prof;

 m_color.Construct;

end;

{ COLOR_ }
procedure renderer_outline_aa.color_;
begin
 m_color:=c^;

end;

{ _COLOR }
function renderer_outline_aa._color;
begin
 result:=@m_color;

end;

{ PROFILE_ }
procedure renderer_outline_aa.profile_;
begin
 m_profile:=prof;

end;

{ _PROFILE }
function renderer_outline_aa._profile;
begin
 result:=m_profile;

end;

{ SUBPIXEL_WIDTH }
function renderer_outline_aa.subpixel_width;
begin
 result:=m_profile._subpixel_width;

end;

{ COVER }
function renderer_outline_aa.cover;
begin
 result:=int8u(m_profile.value(d ) );

end;

{ BLEND_SOLID_HSPAN }
procedure renderer_outline_aa.blend_solid_hspan;
begin
 m_ren.blend_solid_hspan(x ,y ,len ,@m_color ,covers );

end;

{ BLEND_SOLID_VSPAN }
procedure renderer_outline_aa.blend_solid_vspan;
begin
 m_ren.blend_solid_vspan(x ,y ,len ,@m_color ,covers );

end;

{ ACCURATE_JOIN_ONLY }
function renderer_outline_aa.accurate_join_only;
begin
 result:=false;

end;

{ SEMIDOT_HLINE }
procedure renderer_outline_aa.semidot_hline;
var
 covers : array[0..max_half_width * 2 + 4 - 1 ] of int8u;
 p0 ,p1 : int8u_ptr;

 x ,y ,w ,x0 ,dx ,dy ,d : int;

 di : distance_interpolator0;

begin
 p0:=@covers[0 ];
 p1:=@covers[0 ];

 x:=x1 shl line_subpixel_shift;
 y:=y1 shl line_subpixel_shift;
 w:=subpixel_width;

 di.Construct(xc1 ,yc1 ,xc2 ,yc2 ,x ,y );

 inc(x ,line_subpixel_size div 2 );
 inc(y ,line_subpixel_size div 2 );

 x0:=x1;
 dx:=x - xc1;
 dy:=y - yc1;

 repeat
  d:=trunc(fast_sqrt(dx * dx + dy * dy ) );

  p1^:=0;

  if cmp(di.dist ) and
     (d <= w ) then
   p1^:=int8u(cover(d ) );

  inc(ptrcomp(p1 ) ,sizeof(int8u ) );
  inc(dx ,line_subpixel_size );

  di.inc_x_;

  inc(x1 );

 until x1 > x2;

 m_ren.blend_solid_hspan(
  x0 ,y1 ,
  unsigned((ptrcomp(p1 ) - ptrcomp(p0 ) ) div sizeof(int8u ) ) ,
  _color ,p0 );

end;

{ SEMIDOT }
procedure renderer_outline_aa.semidot;
var
 r ,dx ,dy ,dy0 ,dx0 ,x ,y : int;

 ei : ellipse_bresenham_interpolator;

begin
 r:=shr_int32(subpixel_width + line_subpixel_mask ,line_subpixel_shift);

 if r < 1 then
  r:=1;

 ei.Construct(r ,r );

 dx :=0;
 dy :=-r;
 dy0:=dy;
 dx0:=dx;
 x  :=shr_int32(xc1 ,line_subpixel_shift );
 y  :=shr_int32(yc1 ,line_subpixel_shift );

 repeat
  inc(dx ,ei._dx );
  inc(dy ,ei._dy );

  if dy <> dy0 then
   begin
    semidot_hline(cmp ,xc1 ,yc1 ,xc2 ,yc2 ,x - dx0 ,y + dy0 ,x + dx0 );
    semidot_hline(cmp ,xc1 ,yc1 ,xc2 ,yc2 ,x - dx0 ,y - dy0 ,x + dx0 );

   end;

  dx0:=dx;
  dy0:=dy;

  ei.inc_operator;

 until dy >= 0;

 semidot_hline(cmp ,xc1 ,yc1 ,xc2 ,yc2 ,x - dx0 ,y + dy0 ,x + dx0 );

end;

{ LINE0 }
procedure renderer_outline_aa.line0;
var
 li : line_interpolator_aa0;

begin
 li.Construct(@self ,lp );

 if li.count <> 0 then
  if li.vertical then
   while li.step_ver do
  else
   while li.step_hor do;

end;

{ LINE1 }
procedure renderer_outline_aa.line1;
var
 li : line_interpolator_aa1;

begin
 fix_degenerate_bisectrix_start(lp ,@sx ,@sy );

 li.Construct(@self ,lp ,sx ,sy );

 if li.vertical then
  while li.step_ver do
 else
  while li.step_hor do;

end;

{ LINE2 }
procedure renderer_outline_aa.line2;
var
 li : line_interpolator_aa2;

begin
 fix_degenerate_bisectrix_end(lp ,@ex ,@ey );

 li.Construct(@self ,lp ,ex ,ey );

 if li.vertical then
  while li.step_ver do
 else
  while li.step_hor do;

end;

{ LINE3 }
procedure renderer_outline_aa.line3;
var
 li : line_interpolator_aa3;

begin
 fix_degenerate_bisectrix_start(lp ,@sx ,@sy );
 fix_degenerate_bisectrix_end  (lp ,@ex ,@ey );

 li.Construct(@self ,lp ,sx ,sy ,ex ,ey );

 if li.vertical then
  while li.step_ver do
 else
  while li.step_hor do;

end;

END.

