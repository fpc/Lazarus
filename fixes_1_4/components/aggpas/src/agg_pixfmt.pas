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
//----------------------------------------------------------------------------
//
// This unit is originaly not the part of the AGG library.
// agg_pixfmt unit & pixel_formats object substitutes the templetized
// concept of a pixel polymorphism in c++.
//
// [Pascal Port History] -----------------------------------------------------
//
// 13.01.2006-Milano: Unit creation
//
{ agg_pixfmt.pas }
unit
 agg_pixfmt ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
 agg_basics ,
 agg_rendering_buffer ,
 agg_color ;

{ TYPES DEFINITION }
type
 pixel_formats_ptr = ^pixel_formats;

 func_blender   = procedure(this : pixel_formats_ptr; op : unsigned; p : int8u_ptr; cr ,cg ,cb ,ca ,cover : unsigned );
 func_blend_pix = procedure(this : pixel_formats_ptr; p : int8u_ptr; cr ,cg ,cb ,alpha ,cover : unsigned );

 func_copy_pixel  = procedure(this : pixel_formats_ptr; x ,y : int; c : aggclr_ptr );
 func_blend_pixel = procedure(this : pixel_formats_ptr; x ,y : int; c : aggclr_ptr; cover : int8u );

 func_pixel = function(this : pixel_formats_ptr; x ,y : int ) : aggclr;
 func_row   = function(this : pixel_formats_ptr; x ,y : int ) : row_data_type;

 func_copy_hline  = procedure(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr );
 func_copy_vline  = procedure(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr );

 func_blend_hline = procedure(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
 func_blend_vline = procedure(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );

 func_blend_solid_hspan = procedure(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
 func_blend_solid_vspan = procedure(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );

 func_copy_color_hspan = procedure(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr );
 func_copy_color_vspan = procedure(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr );

 func_blend_color_hspan = procedure(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
 func_blend_color_vspan = procedure(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );

 func_copy_from  = procedure(this : pixel_formats_ptr; from : rendering_buffer_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned );
 func_blend_from = procedure(this : pixel_formats_ptr; from : pixel_formats_ptr; psrc_ : int8u_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );

 func_blend_from_color = procedure(this : pixel_formats_ptr; from : pixel_formats_ptr; color : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
 func_blend_from_lut   = procedure(this : pixel_formats_ptr; from : pixel_formats_ptr; color_lut : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );

 func_apply_gamma    = procedure(this : pixel_formats_ptr; p : int8u_ptr );
 func_for_each_pixel = procedure(this : pixel_formats_ptr; f : func_apply_gamma );

 pixel_formats = object
   m_rbuf  : rendering_buffer_ptr;
   m_gamma ,
   m_apply : gamma_ptr;
   m_order : order_type;

   m_comp_op   ,
   m_step      ,
   m_offset    ,
   m_pix_width : unsigned;

   blender : func_blender;

   copy_pixel  : func_copy_pixel;
   blend_pixel : func_blend_pixel;

   pixel : func_pixel;
   row   : func_row;

   copy_hline : func_copy_hline;
   copy_vline : func_copy_vline;

   blend_hline : func_blend_hline;
   blend_vline : func_blend_vline;

   blend_solid_hspan : func_blend_solid_hspan;
   blend_solid_vspan : func_blend_solid_vspan;

   copy_color_hspan : func_copy_color_hspan;
   copy_color_vspan : func_copy_color_vspan;

   blend_color_hspan : func_blend_color_hspan;
   blend_color_vspan : func_blend_color_vspan;

   copy_from  : func_copy_from;
   blend_from : func_blend_from;

   blend_from_color : func_blend_from_color;
   blend_from_lut   : func_blend_from_lut;

   for_each_pixel  : func_for_each_pixel;
   gamma_dir_apply ,
   gamma_inv_apply : func_apply_gamma;

   pixel_premultiply ,
   pixel_demultiply  : func_apply_gamma;

   constructor Construct(rb : rendering_buffer_ptr; st : unsigned = 1; off : unsigned = 0 );

   function  attach (pixf : pixel_formats_ptr; x1 ,y1 ,x2 ,y2 : int ) : boolean;
   function  pix_ptr(x ,y : int ) : int8u_ptr;
   function  row_ptr(y : int ) : int8u_ptr;

   function  _width : unsigned; virtual;
   function  _height : unsigned; virtual;
   function  _stride : int;
   function  _pix_width : unsigned;

   procedure apply_gamma_dir(g : gamma_ptr; order : order_type );
   procedure apply_gamma_inv(g : gamma_ptr; order : order_type );

   procedure comp_op_(op : unsigned );
   function  _comp_op : unsigned;

   procedure premultiply;
   procedure demultiply;

  end;

 define_pixfmt         = procedure(var pixf : pixel_formats; rb : rendering_buffer_ptr );
 define_pixfmt_gamma   = procedure(var pixf : pixel_formats; rb : rendering_buffer_ptr; g : gamma_ptr );
 define_pixfmt_blender = procedure(var pixf : pixel_formats; rb : rendering_buffer_ptr; bl : func_blender; order : order_type );

 comp_op_e = (

  comp_op_clear,         //----comp_op_clear
  comp_op_src,           //----comp_op_src
  comp_op_dst,           //----comp_op_dst
  comp_op_src_over,      //----comp_op_src_over
  comp_op_dst_over,      //----comp_op_dst_over
  comp_op_src_in,        //----comp_op_src_in
  comp_op_dst_in,        //----comp_op_dst_in
  comp_op_src_out,       //----comp_op_src_out
  comp_op_dst_out,       //----comp_op_dst_out
  comp_op_src_atop,      //----comp_op_src_atop
  comp_op_dst_atop,      //----comp_op_dst_atop
  comp_op_xor,           //----comp_op_xor
  comp_op_plus,          //----comp_op_plus
  comp_op_minus,         //----comp_op_minus
  comp_op_multiply,      //----comp_op_multiply
  comp_op_screen,        //----comp_op_screen
  comp_op_overlay,       //----comp_op_overlay
  comp_op_darken,        //----comp_op_darken
  comp_op_lighten,       //----comp_op_lighten
  comp_op_color_dodge,   //----comp_op_color_dodge
  comp_op_color_burn,    //----comp_op_color_burn
  comp_op_hard_light,    //----comp_op_hard_light
  comp_op_soft_light,    //----comp_op_soft_light
  comp_op_difference,    //----comp_op_difference
  comp_op_exclusion,     //----comp_op_exclusion
  comp_op_contrast,      //----comp_op_contrast
  comp_op_invert,        //----comp_op_invert
  comp_op_invert_rgb,    //----comp_op_invert_rgb

  end_of_comp_op_e );

{ GLOBAL PROCEDURES }
 procedure pixfmt_undefined(var pixf : pixel_formats );


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor pixel_formats.Construct;
begin
 m_rbuf :=rb;
 m_gamma:=NIL;
 m_apply:=NIL;
 m_order:=bgra_order;

 m_comp_op:=3;
 m_step   :=st;
 m_offset :=off;

 m_pix_width:=0;

 blender:=NIL;

 copy_pixel :=NIL;
 blend_pixel:=NIL;

 pixel:=NIL;
 row  :=NIL;

 copy_hline:=NIL;
 copy_vline:=NIL;

 blend_hline:=NIL;
 blend_vline:=NIL;

 blend_solid_hspan:=NIL;
 blend_solid_vspan:=NIL;

 copy_color_hspan:=NIL;
 copy_color_vspan:=NIL;

 blend_color_hspan:=NIL;
 blend_color_vspan:=NIL;

 copy_from :=NIL;
 blend_from:=NIL;

 blend_from_color:=NIL;
 blend_from_lut  :=NIL;

 for_each_pixel :=NIL;
 gamma_dir_apply:=NIL;
 gamma_inv_apply:=NIL;

 pixel_premultiply:=NIL;
 pixel_demultiply :=NIL;

end;

{ ATTACH }
function pixel_formats.attach(pixf : pixel_formats_ptr; x1 ,y1 ,x2 ,y2 : int ) : boolean;
var
 r ,c : rect_i;

 stride ,y : int;

begin
 r.Construct(x1 ,y1 ,x2 ,y2 );
 c.Construct(0 ,0 ,pixf._width - 1 ,pixf._height - 1 );

 if r.clip(@c ) then
  begin
   stride:=pixf.m_rbuf._stride;

   if stride < 0  then
    y:=r.y2
   else
    y:=r.y1;

   m_rbuf.attach(
    pixf.pix_ptr(r.x1 ,y ) ,
    (r.x2 - r.x1 ) + 1 ,
    (r.y2 - r.y1 ) + 1 ,
    stride );

   result:=true;

  end
 else
  result:=false;

end;

{ PIX_PTR }
function pixel_formats.pix_ptr(x ,y : int ) : int8u_ptr;
begin
 result:=int8u_ptr(ptrcomp(m_rbuf.row(y ) ) + x * m_pix_width + m_offset );

end;

{ ROW_PTR }
function pixel_formats.row_ptr(y : int ) : int8u_ptr;
begin
 result:=m_rbuf.row(y );

end;

{ _WIDTH }
function pixel_formats._width;
begin
 result:=m_rbuf._width;

end;

{ _HEIGHT }
function pixel_formats._height;
begin
 result:=m_rbuf._height;

end;

{ _STRIDE }
function pixel_formats._stride;
begin
 result:=m_rbuf._stride;

end;

{ _PIX_WIDTH }
function pixel_formats._pix_width;
begin
 result:=m_pix_width;

end;

{ APPLY_GAMMA_DIR }
procedure pixel_formats.apply_gamma_dir;
begin
 m_apply:=g;
 m_order:=order;

 for_each_pixel(@self ,@gamma_dir_apply );

end;

{ APPLY_GAMMA_INV }
procedure pixel_formats.apply_gamma_inv;
begin
 m_apply:=g;
 m_order:=order;

 for_each_pixel(@self ,@gamma_inv_apply );

end;

{ COMP_OP_ }
procedure pixel_formats.comp_op_;
begin
 m_comp_op:=op;

end;

{ _COMP_OP }
function pixel_formats._comp_op;
begin
 result:=m_comp_op;

end;

{ PREMULTIPLY }
procedure pixel_formats.premultiply;
begin
 for_each_pixel(@self ,@pixel_premultiply );

end;

{ DEMULTIPLY }
procedure pixel_formats.demultiply;
begin
 for_each_pixel(@self ,@pixel_demultiply );

end;

{ PIXFMT_UNDEFINED }
procedure pixfmt_undefined;
begin
 pixf.Construct(NIL );

 pixf.copy_pixel :=NIL;
 pixf.blend_pixel:=NIL;

 pixf.pixel:=NIL;
 pixf.row  :=NIL;

 pixf.copy_hline:=NIL;
 pixf.copy_vline:=NIL;

 pixf.blend_hline:=NIL;
 pixf.blend_vline:=NIL;

 pixf.blend_solid_hspan:=NIL;
 pixf.blend_solid_vspan:=NIL;

 pixf.copy_color_hspan:=NIL;
 pixf.copy_color_vspan:=NIL;

 pixf.blend_color_hspan:=NIL;
 pixf.blend_color_vspan:=NIL;

 pixf.copy_from :=NIL;
 pixf.blend_from:=NIL;

 pixf.blend_from_color:=NIL;
 pixf.blend_from_lut  :=NIL;

 pixf.for_each_pixel :=NIL;
 pixf.gamma_dir_apply:=NIL;
 pixf.gamma_inv_apply:=NIL;

end;

END.

