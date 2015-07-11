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
// Adaptation for high precision colors has been sponsored by 
// Liberty Technology Systems, Inc., visit http://lib-sys.com
//
// Liberty Technology Systems, Inc. is the provider of
// PostScript and PDF technology for software developers.
//
// [Pascal Port History] -----------------------------------------------------
//
// 12.10.2007-Milano: comp_op_rgba_invert & comp_op_rgba_invert_rgb
// 08.10.2007-Milano: pixfmt_alpha_blend_rgba
// 13.09.2007-Milano: comp_op_adaptor_clip_to_dst_rgba_pre
// 23.06.2006-Milano: ptrcomp adjustments
// 18.03.2006-Milano: pf_xxx.inc completed
// 13.01.2006-Milano: rgba ,argb & abgr stuff
// 16.11.2005-Milano: Unit port establishment
//
{ agg_pixfmt_rgba.pas }
unit
 agg_pixfmt_rgba ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
 agg_basics ,
 agg_pixfmt ,
 agg_color ,
 agg_rendering_buffer ;

{ GLOBAL PROCEDURES }
 procedure pixfmt_bgra32(var pixf : pixel_formats; rb : rendering_buffer_ptr );
 procedure pixfmt_rgba32(var pixf : pixel_formats; rb : rendering_buffer_ptr );
 procedure pixfmt_argb32(var pixf : pixel_formats; rb : rendering_buffer_ptr );
 procedure pixfmt_abgr32(var pixf : pixel_formats; rb : rendering_buffer_ptr );

 procedure pixfmt_bgra32_pre(var pixf : pixel_formats; rb : rendering_buffer_ptr );
 procedure pixfmt_rgba32_pre(var pixf : pixel_formats; rb : rendering_buffer_ptr );
 procedure pixfmt_argb32_pre(var pixf : pixel_formats; rb : rendering_buffer_ptr );
 procedure pixfmt_abgr32_pre(var pixf : pixel_formats; rb : rendering_buffer_ptr );

 procedure comp_op_adaptor_rgba                (this : pixel_formats_ptr; op : unsigned; p : int8u_ptr; cr ,cg ,cb ,ca ,cover : unsigned );
 procedure comp_op_adaptor_clip_to_dst_rgba_pre(this : pixel_formats_ptr; op : unsigned; p : int8u_ptr; cr ,cg ,cb ,ca ,cover : unsigned );

 procedure pixfmt_alpha_blend_rgba (var pixf : pixel_formats; rb : rendering_buffer_ptr; order : order_type );
 procedure pixfmt_custom_blend_rgba(var pixf : pixel_formats; rb : rendering_buffer_ptr; bl : func_blender; order : order_type );


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ fmt32_row }
function fmt32_row(this : pixel_formats_ptr; x ,y : int ) : row_data_type;
begin
 result.Construct(
  x ,this._width - 1 ,
  int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * 4 * sizeof(int8u ) ) );

end;

{ fmt32_copy_from }
procedure fmt32_copy_from(this : pixel_formats_ptr; from : rendering_buffer_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned );
begin
 move(
  unsigned_ptr(ptrcomp(from.row(ysrc ) ) + xsrc * 4 )^ ,
  unsigned_ptr(ptrcomp(this.m_rbuf.row(ydst ) ) + xdst * 4 )^ ,
  len * 4 );

end;

{ order32_for_each_pixel }
procedure order32_for_each_pixel(this : pixel_formats_ptr; f : func_apply_gamma );
var
 y ,len : unsigned;

 p : int8u_ptr;

begin
 y:=0;

 while y < this._height do
  begin
   len:=this._width;

   p:=this.m_rbuf.row(y );

   repeat
    f(this ,p );

    inc(ptrcomp(p ) ,4 );
    dec(len );

   until len = 0;

   inc(y );

  end;

end;

{ order32_gamma_dir_apply }
procedure order32_gamma_dir_apply(this : pixel_formats_ptr; p : int8u_ptr );
begin
 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u(this.m_apply.dir(int8u_ptr(ptrcomp(p ) + this.m_order.R )^ ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u(this.m_apply.dir(int8u_ptr(ptrcomp(p ) + this.m_order.G )^ ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u(this.m_apply.dir(int8u_ptr(ptrcomp(p ) + this.m_order.B )^ ) );

end;

{ order32_gamma_inv_apply }
procedure order32_gamma_inv_apply(this : pixel_formats_ptr; p : int8u_ptr );
begin
 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u(this.m_apply.inv(int8u_ptr(ptrcomp(p ) + this.m_order.R )^ ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u(this.m_apply.inv(int8u_ptr(ptrcomp(p ) + this.m_order.G )^ ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u(this.m_apply.inv(int8u_ptr(ptrcomp(p ) + this.m_order.B )^ ) );

end;

{ order32_pixel_premultiply }
procedure order32_pixel_premultiply(this : pixel_formats_ptr; p : int8u_ptr );
var
 a : unsigned;

begin
 a:=int8u_ptr(ptrcomp(p ) + this.m_order.A )^;

 if a = 0 then
  begin
   int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=0;
   int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=0;
   int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=0;

  end
 else
  begin
   int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=
    int8u((int8u_ptr(ptrcomp(p ) + this.m_order.R )^ * a + base_mask ) shr base_shift );

   int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=
    int8u((int8u_ptr(ptrcomp(p ) + this.m_order.G )^ * a + base_mask ) shr base_shift );

   int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=
    int8u((int8u_ptr(ptrcomp(p ) + this.m_order.B )^ * a + base_mask ) shr base_shift );

  end;

end;

{ order32_pixel_demultiply }
procedure order32_pixel_demultiply(this : pixel_formats_ptr; p : int8u_ptr );
var
 r ,g ,b ,a : unsigned;

begin
 a:=int8u_ptr(ptrcomp(p ) + this.m_order.A )^;

 if a = 0 then
  begin
   int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=0;
   int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=0;
   int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=0;

  end
 else
  begin
   r:=(int8u_ptr(ptrcomp(p ) + this.m_order.R )^ * base_mask ) div a;
   g:=(int8u_ptr(ptrcomp(p ) + this.m_order.G )^ * base_mask ) div a;
   b:=(int8u_ptr(ptrcomp(p ) + this.m_order.B )^ * base_mask ) div a;

   if r > base_mask then
    int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=base_mask
   else
    int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=r;

   if g > base_mask then
    int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=base_mask
   else
    int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=g;

   if b > base_mask then
    int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=base_mask
   else
    int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=b;

  end;

end;

{$I pf_bgra32.inc }

{ PIXFMT_BGRA32 }
procedure pixfmt_bgra32;
begin
 pixf.Construct(rb );

 pixf.m_order:=bgra_order;

 pixf.m_pix_width:=4;

 pixf.copy_pixel :=@bgra32_copy_pixel;
 pixf.blend_pixel:=@bgra32_blend_pixel;

 pixf.pixel:=@bgra32_pixel;
 pixf.row  :=@fmt32_row;

 pixf.copy_hline:=@bgra32_copy_hline;
 pixf.copy_vline:=@bgra32_copy_vline;

 pixf.blend_hline:=@bgra32_blend_hline;
 pixf.blend_vline:=@bgra32_blend_vline;

 pixf.blend_solid_hspan:=@bgra32_blend_solid_hspan;
 pixf.blend_solid_vspan:=@bgra32_blend_solid_vspan;

 pixf.copy_color_hspan:=@bgra32_copy_color_hspan;
 pixf.copy_color_vspan:=@bgra32_copy_color_vspan;
 
 pixf.blend_color_hspan:=@bgra32_blend_color_hspan;
 pixf.blend_color_vspan:=@bgra32_blend_color_vspan;

 pixf.copy_from :=@fmt32_copy_from;
 pixf.blend_from:=@bgra32_blend_from;

 pixf.blend_from_color:=@bgra32_blend_from_color;
 pixf.blend_from_lut  :=@bgra32_blend_from_lut;
 
 pixf.for_each_pixel :=@order32_for_each_pixel;
 pixf.gamma_dir_apply:=@order32_gamma_dir_apply;
 pixf.gamma_inv_apply:=@order32_gamma_inv_apply;

 pixf.pixel_premultiply:=@order32_pixel_premultiply;
 pixf.pixel_demultiply :=@order32_pixel_demultiply;
 
end;

{$I pf_rgba32.inc }

{ PIXFMT_RGBA32 }
procedure pixfmt_rgba32;
begin
 pixf.Construct(rb );

 pixf.m_order:=rgba_order;

 pixf.m_pix_width:=4;

 pixf.copy_pixel :=@rgba32_copy_pixel;
 pixf.blend_pixel:=@rgba32_blend_pixel;

 pixf.pixel:=@rgba32_pixel;
 pixf.row  :=@fmt32_row;

 pixf.copy_hline:=@rgba32_copy_hline;
 pixf.copy_vline:=@rgba32_copy_vline;

 pixf.blend_hline:=@rgba32_blend_hline;
 pixf.blend_vline:=@rgba32_blend_vline;

 pixf.blend_solid_hspan:=@rgba32_blend_solid_hspan;
 pixf.blend_solid_vspan:=@rgba32_blend_solid_vspan;

 pixf.copy_color_hspan:=@rgba32_copy_color_hspan;
 pixf.copy_color_vspan:=@rgba32_copy_color_vspan;
 
 pixf.blend_color_hspan:=@rgba32_blend_color_hspan;
 pixf.blend_color_vspan:=@rgba32_blend_color_vspan;

 pixf.copy_from :=@fmt32_copy_from;
 pixf.blend_from:=@rgba32_blend_from;

 pixf.blend_from_color:=@rgba32_blend_from_color;
 pixf.blend_from_lut  :=@rgba32_blend_from_lut;
 
 pixf.for_each_pixel :=@order32_for_each_pixel;
 pixf.gamma_dir_apply:=@order32_gamma_dir_apply;
 pixf.gamma_inv_apply:=@order32_gamma_inv_apply;

 pixf.pixel_premultiply:=@order32_pixel_premultiply;
 pixf.pixel_demultiply :=@order32_pixel_demultiply;

end;

{$I pf_argb32.inc }

{ PIXFMT_ARGB32 }
procedure pixfmt_argb32;
begin
 pixf.Construct(rb );

 pixf.m_order:=argb_order;

 pixf.m_pix_width:=4;

 pixf.copy_pixel :=@argb32_copy_pixel;
 pixf.blend_pixel:=@argb32_blend_pixel;

 pixf.pixel:=@argb32_pixel;
 pixf.row  :=@fmt32_row;

 pixf.copy_hline:=@argb32_copy_hline;
 pixf.copy_vline:=@argb32_copy_vline;

 pixf.blend_hline:=@argb32_blend_hline;
 pixf.blend_vline:=@argb32_blend_vline;

 pixf.blend_solid_hspan:=@argb32_blend_solid_hspan;
 pixf.blend_solid_vspan:=@argb32_blend_solid_vspan;

 pixf.copy_color_hspan:=@argb32_copy_color_hspan;
 pixf.copy_color_vspan:=@argb32_copy_color_vspan;
 
 pixf.blend_color_hspan:=@argb32_blend_color_hspan;
 pixf.blend_color_vspan:=@argb32_blend_color_vspan;

 pixf.copy_from :=@fmt32_copy_from;
 pixf.blend_from:=@argb32_blend_from;

 pixf.blend_from_color:=@argb32_blend_from_color;
 pixf.blend_from_lut  :=@argb32_blend_from_lut;
 
 pixf.for_each_pixel :=@order32_for_each_pixel;
 pixf.gamma_dir_apply:=@order32_gamma_dir_apply;
 pixf.gamma_inv_apply:=@order32_gamma_inv_apply;

 pixf.pixel_premultiply:=@order32_pixel_premultiply;
 pixf.pixel_demultiply :=@order32_pixel_demultiply;
 
end;

{$I pf_abgr32.inc }

{ PIXFMT_ABGR32 }
procedure pixfmt_abgr32;
begin
 pixf.Construct(rb );

 pixf.m_order:=abgr_order;

 pixf.m_pix_width:=4;

 pixf.copy_pixel :=@abgr32_copy_pixel;
 pixf.blend_pixel:=@abgr32_blend_pixel;

 pixf.pixel:=@abgr32_pixel;
 pixf.row  :=@fmt32_row;

 pixf.copy_hline:=@abgr32_copy_hline;
 pixf.copy_vline:=@abgr32_copy_vline;

 pixf.blend_hline:=@abgr32_blend_hline;
 pixf.blend_vline:=@abgr32_blend_vline;

 pixf.blend_solid_hspan:=@abgr32_blend_solid_hspan;
 pixf.blend_solid_vspan:=@abgr32_blend_solid_vspan;

 pixf.copy_color_hspan:=@abgr32_copy_color_hspan;
 pixf.copy_color_vspan:=@abgr32_copy_color_vspan;
 
 pixf.blend_color_hspan:=@abgr32_blend_color_hspan;
 pixf.blend_color_vspan:=@abgr32_blend_color_vspan;

 pixf.copy_from :=@fmt32_copy_from;
 pixf.blend_from:=@abgr32_blend_from;

 pixf.blend_from_color:=@abgr32_blend_from_color;
 pixf.blend_from_lut  :=@abgr32_blend_from_lut;

 pixf.for_each_pixel :=@order32_for_each_pixel;
 pixf.gamma_dir_apply:=@order32_gamma_dir_apply;
 pixf.gamma_inv_apply:=@order32_gamma_inv_apply;

 pixf.pixel_premultiply:=@order32_pixel_premultiply;
 pixf.pixel_demultiply :=@order32_pixel_demultiply;
 
end;

{$I pf_bgra32_pre.inc }

{ PIXFMT_BGRA32_PRE }
procedure pixfmt_bgra32_pre;
begin
 pixf.Construct(rb );

 pixf.m_order:=bgra_order;

 pixf.m_pix_width:=4;

 pixf.copy_pixel :=@bgra32_copy_pixel;
 pixf.blend_pixel:=@bgra32_pre_blend_pixel;

 pixf.pixel:=@bgra32_pixel;
 pixf.row  :=@fmt32_row;

 pixf.copy_hline:=@bgra32_copy_hline;
 pixf.copy_vline:=@bgra32_copy_vline;

 pixf.blend_hline:=@bgra32_pre_blend_hline;
 pixf.blend_vline:=@bgra32_pre_blend_vline;

 pixf.blend_solid_hspan:=@bgra32_pre_blend_solid_hspan;
 pixf.blend_solid_vspan:=@bgra32_pre_blend_solid_vspan;

 pixf.copy_color_hspan:=bgra32_copy_color_hspan;
 pixf.copy_color_vspan:=bgra32_copy_color_vspan;
 
 pixf.blend_color_hspan:=@bgra32_pre_blend_color_hspan;
 pixf.blend_color_vspan:=@bgra32_pre_blend_color_vspan;

 pixf.copy_from :=@fmt32_copy_from;
 pixf.blend_from:=@bgra32_pre_blend_from;

 pixf.blend_from_color:=@bgra32_pre_blend_from_color;
 pixf.blend_from_lut  :=@bgra32_pre_blend_from_lut;

 pixf.for_each_pixel :=@order32_for_each_pixel;
 pixf.gamma_dir_apply:=@order32_gamma_dir_apply;
 pixf.gamma_inv_apply:=@order32_gamma_inv_apply;

 pixf.pixel_premultiply:=@order32_pixel_premultiply;
 pixf.pixel_demultiply :=@order32_pixel_demultiply;
 
end;

{$I pf_rgba32_pre.inc }

{ PIXFMT_RGBA32_PRE }
procedure pixfmt_rgba32_pre;
begin
 pixf.Construct(rb );

 pixf.m_order:=rgba_order;

 pixf.m_pix_width:=4;

 pixf.copy_pixel :=@rgba32_copy_pixel;
 pixf.blend_pixel:=@rgba32_pre_blend_pixel;

 pixf.pixel:=@rgba32_pixel;
 pixf.row  :=@fmt32_row;

 pixf.copy_hline:=@rgba32_copy_hline;
 pixf.copy_vline:=@rgba32_copy_vline;

 pixf.blend_hline:=@rgba32_pre_blend_hline;
 pixf.blend_vline:=@rgba32_pre_blend_vline;

 pixf.blend_solid_hspan:=@rgba32_pre_blend_solid_hspan;
 pixf.blend_solid_vspan:=@rgba32_pre_blend_solid_vspan;

 pixf.copy_color_hspan:=rgba32_copy_color_hspan;
 pixf.copy_color_vspan:=rgba32_copy_color_vspan;
 
 pixf.blend_color_hspan:=@rgba32_pre_blend_color_hspan;
 pixf.blend_color_vspan:=@rgba32_pre_blend_color_vspan;

 pixf.copy_from :=@fmt32_copy_from;
 pixf.blend_from:=@rgba32_pre_blend_from;

 pixf.blend_from_color:=@rgba32_pre_blend_from_color;
 pixf.blend_from_lut  :=@rgba32_pre_blend_from_lut;
 
 pixf.for_each_pixel :=@order32_for_each_pixel;
 pixf.gamma_dir_apply:=@order32_gamma_dir_apply;
 pixf.gamma_inv_apply:=@order32_gamma_inv_apply;

 pixf.pixel_premultiply:=@order32_pixel_premultiply;
 pixf.pixel_demultiply :=@order32_pixel_demultiply;
 
end;

{$I pf_argb32_pre.inc }

{ PIXFMT_ARGB32_PRE }
procedure pixfmt_argb32_pre;
begin
 pixf.Construct(rb );

 pixf.m_order:=argb_order;

 pixf.m_pix_width:=4;

 pixf.copy_pixel :=@argb32_copy_pixel;
 pixf.blend_pixel:=@argb32_pre_blend_pixel;

 pixf.pixel:=@argb32_pixel;
 pixf.row  :=@fmt32_row;

 pixf.copy_hline:=@argb32_copy_hline;
 pixf.copy_vline:=@argb32_copy_vline;

 pixf.blend_hline:=@argb32_pre_blend_hline;
 pixf.blend_vline:=@argb32_pre_blend_vline;

 pixf.blend_solid_hspan:=@argb32_pre_blend_solid_hspan;
 pixf.blend_solid_vspan:=@argb32_pre_blend_solid_vspan;

 pixf.copy_color_hspan:=argb32_copy_color_hspan;
 pixf.copy_color_vspan:=argb32_copy_color_vspan;
 
 pixf.blend_color_hspan:=@argb32_pre_blend_color_hspan;
 pixf.blend_color_vspan:=@argb32_pre_blend_color_vspan;

 pixf.copy_from :=@fmt32_copy_from;
 pixf.blend_from:=@argb32_pre_blend_from;

 pixf.blend_from_color:=@argb32_pre_blend_from_color;
 pixf.blend_from_lut  :=@argb32_pre_blend_from_lut;
 
 pixf.for_each_pixel :=@order32_for_each_pixel;
 pixf.gamma_dir_apply:=@order32_gamma_dir_apply;
 pixf.gamma_inv_apply:=@order32_gamma_inv_apply;

 pixf.pixel_premultiply:=@order32_pixel_premultiply;
 pixf.pixel_demultiply :=@order32_pixel_demultiply;
 
end;

{$I pf_abgr32_pre.inc }

{ PIXFMT_ABGR32_PRE }
procedure pixfmt_abgr32_pre;
begin
 pixf.Construct(rb );

 pixf.m_order:=abgr_order;

 pixf.m_pix_width:=4;

 pixf.copy_pixel :=@abgr32_copy_pixel;
 pixf.blend_pixel:=@abgr32_pre_blend_pixel;

 pixf.pixel:=@abgr32_pixel;
 pixf.row  :=@fmt32_row;

 pixf.copy_hline:=@abgr32_copy_hline;
 pixf.copy_vline:=@abgr32_copy_vline;

 pixf.blend_hline:=@abgr32_pre_blend_hline;
 pixf.blend_vline:=@abgr32_pre_blend_vline;

 pixf.blend_solid_hspan:=@abgr32_pre_blend_solid_hspan;
 pixf.blend_solid_vspan:=@abgr32_pre_blend_solid_vspan;

 pixf.copy_color_hspan:=abgr32_copy_color_hspan;
 pixf.copy_color_vspan:=abgr32_copy_color_vspan;
 
 pixf.blend_color_hspan:=@abgr32_pre_blend_color_hspan;
 pixf.blend_color_vspan:=@abgr32_pre_blend_color_vspan;

 pixf.copy_from :=@fmt32_copy_from;
 pixf.blend_from:=@abgr32_pre_blend_from;

 pixf.blend_from_color:=@abgr32_pre_blend_from_color;
 pixf.blend_from_lut  :=@abgr32_pre_blend_from_lut;
 
 pixf.for_each_pixel :=@order32_for_each_pixel;
 pixf.gamma_dir_apply:=@order32_gamma_dir_apply;
 pixf.gamma_inv_apply:=@order32_gamma_inv_apply;

 pixf.pixel_premultiply:=@order32_pixel_premultiply;
 pixf.pixel_demultiply :=@order32_pixel_demultiply;
 
end;

{$I pf_alpha32.inc }

{ PIXFMT_ALPHA_BLEND_RGBA }
procedure pixfmt_alpha_blend_rgba;
begin
 pixf.Construct(rb );

 pixf.m_order:=order;

 pixf.m_pix_width:=4;

 pixf.copy_pixel :=@alpha32_copy_pixel;
 pixf.blend_pixel:=@alpha32_blend_pixel;

 pixf.pixel:=@alpha32_pixel;
 pixf.row  :=@fmt32_row;

 pixf.copy_hline:=@alpha32_copy_hline;
 pixf.copy_vline:=@alpha32_copy_vline;

 pixf.blend_hline:=@alpha32_blend_hline;
 pixf.blend_vline:=@alpha32_blend_vline;

 pixf.blend_solid_hspan:=@alpha32_blend_solid_hspan;
 pixf.blend_solid_vspan:=@alpha32_blend_solid_vspan;

 pixf.copy_color_hspan:=@alpha32_copy_color_hspan;
 pixf.copy_color_vspan:=@alpha32_copy_color_vspan;
 
 pixf.blend_color_hspan:=@alpha32_blend_color_hspan;
 pixf.blend_color_vspan:=@alpha32_blend_color_vspan;

 pixf.copy_from :=@fmt32_copy_from;
 pixf.blend_from:=@alpha32_blend_from;

 pixf.blend_from_color:=@alpha32_blend_from_color;
 pixf.blend_from_lut  :=@alpha32_blend_from_lut;
 
 pixf.for_each_pixel :=@order32_for_each_pixel;
 pixf.gamma_dir_apply:=@order32_gamma_dir_apply;
 pixf.gamma_inv_apply:=@order32_gamma_inv_apply;

 pixf.pixel_premultiply:=@order32_pixel_premultiply;
 pixf.pixel_demultiply :=@order32_pixel_demultiply;
 
end;

{$I pf_cubl32.inc }

{ PIXFMT_CUSTOM_BLEND_RGBA }
procedure pixfmt_custom_blend_rgba;
begin
 pixf.Construct(rb );

 pixf.m_order:=order;
 pixf.blender:=bl;

 pixf.m_pix_width:=4;

 pixf.copy_pixel :=@cubl_copy_pixel;
 pixf.blend_pixel:=@cubl_blend_pixel;

 pixf.pixel:=@cubl_pixel;
 pixf.row  :=@fmt32_row;

 pixf.copy_hline:=@cubl_copy_hline;
 pixf.copy_vline:=@cubl_copy_vline;

 pixf.blend_hline:=@cubl_blend_hline;
 pixf.blend_vline:=@cubl_blend_vline;

 pixf.blend_solid_hspan:=@cubl_blend_solid_hspan;
 pixf.blend_solid_vspan:=@cubl_blend_solid_vspan;

 pixf.copy_color_hspan:=@cubl_copy_color_hspan;
 pixf.copy_color_vspan:=@cubl_copy_color_vspan;

 pixf.blend_color_hspan:=@cubl_blend_color_hspan;
 pixf.blend_color_vspan:=@cubl_blend_color_vspan;

 pixf.copy_from :=@fmt32_copy_from;
 pixf.blend_from:=@cubl_blend_from;

 pixf.blend_from_color:=@cubl_blend_from_color;
 pixf.blend_from_lut  :=@cubl_blend_from_lut;
 
 pixf.for_each_pixel :=@order32_for_each_pixel;
 pixf.gamma_dir_apply:=@order32_gamma_dir_apply;
 pixf.gamma_inv_apply:=@order32_gamma_inv_apply;

 pixf.pixel_premultiply:=@order32_pixel_premultiply;
 pixf.pixel_demultiply :=@order32_pixel_demultiply;
 
end;

{ comp_op_rgba_clear }
procedure comp_op_rgba_clear(this : pixel_formats_ptr; p : int8u_ptr; cr ,cg ,cb ,alpha ,cover : unsigned );
begin
 if cover < 255 then
  begin
   cover:=255 - cover;

   int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u((int8u_ptr(ptrcomp(p ) + this.m_order.R )^ * cover + 255 ) shr 8 );
   int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u((int8u_ptr(ptrcomp(p ) + this.m_order.G )^ * cover + 255 ) shr 8 );
   int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u((int8u_ptr(ptrcomp(p ) + this.m_order.B )^ * cover + 255 ) shr 8 );
   int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=int8u((int8u_ptr(ptrcomp(p ) + this.m_order.A )^ * cover + 255 ) shr 8 );

  end
 else
  begin
   int8u_ptr(ptrcomp(p ) + 0 )^:=0;
   int8u_ptr(ptrcomp(p ) + 1 )^:=0;
   int8u_ptr(ptrcomp(p ) + 2 )^:=0;
   int8u_ptr(ptrcomp(p ) + 3 )^:=0;

  end;

end;

{ comp_op_rgba_src }
procedure comp_op_rgba_src(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 alpha : unsigned;

begin
 if cover < 255 then
  begin
   alpha:=255 - cover;

   int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u(((int8u_ptr(ptrcomp(p ) + this.m_order.R )^ * alpha + 255 ) shr 8 ) + ((sr * cover + 255 ) shr 8 ) );
   int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u(((int8u_ptr(ptrcomp(p ) + this.m_order.G )^ * alpha + 255 ) shr 8 ) + ((sg * cover + 255 ) shr 8 ) );
   int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u(((int8u_ptr(ptrcomp(p ) + this.m_order.B )^ * alpha + 255 ) shr 8 ) + ((sb * cover + 255 ) shr 8 ) );
   int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=int8u(((int8u_ptr(ptrcomp(p ) + this.m_order.A )^ * alpha + 255 ) shr 8 ) + ((sa * cover + 255 ) shr 8 ) );

  end
 else
  begin
   int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=sr;
   int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=sg;
   int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=sb;
   int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=sa;

  end;

end;

{ comp_op_rgba_dst }
procedure comp_op_rgba_dst(this : pixel_formats_ptr; p : int8u_ptr; cr ,cg ,cb ,alpha ,cover : unsigned );
begin
end;

{ comp_op_rgba_src_over }
//   Dca' = Sca + Dca.(1 - Sa)
//   Da'  = Sa + Da - Sa.Da
procedure comp_op_rgba_src_over(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 s1a : unsigned;

begin
 if cover < 255 then
  begin
   sr:=(sr * cover + 255 ) shr 8;
   sg:=(sg * cover + 255 ) shr 8;
   sb:=(sb * cover + 255 ) shr 8;
   sa:=(sa * cover + 255 ) shr 8;

  end;

 s1a:=base_mask - sa;

 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=
  int8u(sr + ((int8u_ptr(ptrcomp(p ) + this.m_order.R )^ * s1a + base_mask ) shr base_shift ) );

 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=
  int8u(sg + ((int8u_ptr(ptrcomp(p ) + this.m_order.G )^ * s1a + base_mask ) shr base_shift ) );

 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=
  int8u(sb + ((int8u_ptr(ptrcomp(p ) + this.m_order.B )^ * s1a + base_mask ) shr base_shift ) );

 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=
  int8u(sa + int8u_ptr(ptrcomp(p ) + this.m_order.A )^ - ((sa * int8u_ptr(ptrcomp(p ) + this.m_order.A )^ + base_mask ) shr base_shift ) );

end;

{ comp_op_rgba_dst_over }
// Dca' = Dca + Sca.(1 - Da)
// Da'  = Sa + Da - Sa.Da
procedure comp_op_rgba_dst_over(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 d1a : unsigned;

begin
 if cover < 255 then
  begin
   sr:=(sr * cover + 255 ) shr 8;
   sg:=(sg * cover + 255 ) shr 8;
   sb:=(sb * cover + 255 ) shr 8;
   sa:=(sa * cover + 255 ) shr 8;

  end;

 d1a:=base_mask - int8u_ptr(ptrcomp(p ) + this.m_order.A )^;

 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=
  int8u(int8u_ptr(ptrcomp(p ) + this.m_order.R )^ + ((sr * d1a + base_mask ) shr base_shift ) );

 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=
  int8u(int8u_ptr(ptrcomp(p ) + this.m_order.G )^ + ((sg * d1a + base_mask ) shr base_shift ) );

 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=
  int8u(int8u_ptr(ptrcomp(p ) + this.m_order.B )^ + ((sb * d1a + base_mask ) shr base_shift ) );

 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=
  int8u(sa + int8u_ptr(ptrcomp(p ) + this.m_order.A )^ - ((sa * int8u_ptr(ptrcomp(p ) + this.m_order.A )^ + base_mask ) shr base_shift ) );

end;

{ comp_op_rgba_src_in }
// Dca' = Sca.Da
// Da'  = Sa.Da
procedure comp_op_rgba_src_in(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 da ,alpha : unsigned;

begin
 da:=int8u_ptr(ptrcomp(p ) + this.m_order.A )^;

 if cover < 255 then
  begin
   alpha:=255 - cover;

   int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=
    int8u(((int8u_ptr(ptrcomp(p ) + this.m_order.R )^ * alpha + 255 ) shr 8 ) + ((((sr * da + base_mask ) shr base_shift ) * cover + 255 ) shr 8 ) );

   int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=
    int8u(((int8u_ptr(ptrcomp(p ) + this.m_order.G )^ * alpha + 255 ) shr 8 ) + ((((sg * da + base_mask ) shr base_shift ) * cover + 255 ) shr 8 ) );

   int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=
    int8u(((int8u_ptr(ptrcomp(p ) + this.m_order.B )^ * alpha + 255 ) shr 8 ) + ((((sb * da + base_mask ) shr base_shift ) * cover + 255 ) shr 8 ) );

   int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=
    int8u(((int8u_ptr(ptrcomp(p ) + this.m_order.A )^ * alpha + 255 ) shr 8 ) + ((((sa * da + base_mask ) shr base_shift ) * cover + 255 ) shr 8 ) );

  end
 else
  begin
   int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u((sr * da + base_mask ) shr base_shift );
   int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u((sg * da + base_mask ) shr base_shift );
   int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u((sb * da + base_mask ) shr base_shift );
   int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=int8u((sa * da + base_mask ) shr base_shift );

  end;

end;

{ comp_op_rgba_dst_in }
// Dca' = Dca.Sa
// Da'  = Sa.Da
procedure comp_op_rgba_dst_in(this : pixel_formats_ptr; p : int8u_ptr; cr ,cg ,cb ,sa ,cover : unsigned );
begin
 if cover < 255 then
  sa:=base_mask - ((cover * (base_mask - sa ) + 255 ) shr 8 );

 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u((int8u_ptr(ptrcomp(p ) + this.m_order.R )^ * sa + base_mask ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u((int8u_ptr(ptrcomp(p ) + this.m_order.G )^ * sa + base_mask ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u((int8u_ptr(ptrcomp(p ) + this.m_order.B )^ * sa + base_mask ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=int8u((int8u_ptr(ptrcomp(p ) + this.m_order.A )^ * sa + base_mask ) shr base_shift );

end;

{ comp_op_rgba_src_out }
// Dca' = Sca.(1 - Da)
// Da'  = Sa.(1 - Da)
procedure comp_op_rgba_src_out(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 da ,alpha : unsigned;

begin
 da:=base_mask - int8u_ptr(ptrcomp(p ) + this.m_order.A )^;

 if cover < 255 then
  begin
   alpha:=255 - cover;

   int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=
    int8u(((int8u_ptr(ptrcomp(p ) + this.m_order.R )^ * alpha + 255 ) shr 8 ) + ((((sr * da + base_mask ) shr base_shift ) * cover + 255 ) shr 8 ) );

   int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=
    int8u(((int8u_ptr(ptrcomp(p ) + this.m_order.G )^ * alpha + 255 ) shr 8 ) + ((((sg * da + base_mask ) shr base_shift ) * cover + 255 ) shr 8 ) );

   int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=
    int8u(((int8u_ptr(ptrcomp(p ) + this.m_order.B )^ * alpha + 255 ) shr 8 ) + ((((sb * da + base_mask ) shr base_shift ) * cover + 255 ) shr 8 ) );

   int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=
    int8u(((int8u_ptr(ptrcomp(p ) + this.m_order.A )^ * alpha + 255 ) shr 8 ) + ((((sa * da + base_mask ) shr base_shift ) * cover + 255 ) shr 8 ) );

  end
 else
  begin
   int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u((sr * da + base_mask ) shr base_shift );
   int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u((sg * da + base_mask ) shr base_shift );
   int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u((sb * da + base_mask ) shr base_shift );
   int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=int8u((sa * da + base_mask ) shr base_shift );

  end;

end;

{ comp_op_rgba_dst_out }
// Dca' = Dca.(1 - Sa)
// Da'  = Da.(1 - Sa)
procedure comp_op_rgba_dst_out(this : pixel_formats_ptr; p : int8u_ptr; cr ,cg ,cb ,sa ,cover : unsigned );
begin
 if cover < 255 then
  sa:=(sa * cover + 255 ) shr 8;

 sa:=base_mask - sa;

 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u((int8u_ptr(ptrcomp(p ) + this.m_order.R )^ * sa + base_shift ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u((int8u_ptr(ptrcomp(p ) + this.m_order.G )^ * sa + base_shift ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u((int8u_ptr(ptrcomp(p ) + this.m_order.B )^ * sa + base_shift ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=int8u((int8u_ptr(ptrcomp(p ) + this.m_order.A )^ * sa + base_shift ) shr base_shift );

end;

{ comp_op_rgba_src_atop }
// Dca' = Sca.Da + Dca.(1 - Sa)
// Da'  = Da
procedure comp_op_rgba_src_atop(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 da : unsigned;

begin
 if cover < 255 then
  begin
   sr:=(sr * cover + 255 ) shr 8;
   sg:=(sg * cover + 255 ) shr 8;
   sb:=(sb * cover + 255 ) shr 8;
   sa:=(sa * cover + 255 ) shr 8;

  end;

 da:=int8u_ptr(ptrcomp(p ) + this.m_order.A )^;
 sa:=base_mask - sa;

 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u((sr * da + int8u_ptr(ptrcomp(p ) + this.m_order.R )^ * sa + base_mask ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u((sg * da + int8u_ptr(ptrcomp(p ) + this.m_order.G )^ * sa + base_mask ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u((sb * da + int8u_ptr(ptrcomp(p ) + this.m_order.B )^ * sa + base_mask ) shr base_shift );

end;

{ comp_op_rgba_dst_atop }
// Dca' = Dca.Sa + Sca.(1 - Da)
// Da'  = Sa
procedure comp_op_rgba_dst_atop(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 da ,alpha : unsigned;

begin
 da:=base_mask - int8u_ptr(ptrcomp(p ) + this.m_order.A )^;

 if cover < 255 then
  begin
   alpha:=255 - cover;

   sr:=(int8u_ptr(ptrcomp(p ) + this.m_order.R )^ * sa + sr * da + base_mask ) shr base_shift;
   sg:=(int8u_ptr(ptrcomp(p ) + this.m_order.G )^ * sa + sg * da + base_mask ) shr base_shift;
   sb:=(int8u_ptr(ptrcomp(p ) + this.m_order.B )^ * sa + sb * da + base_mask ) shr base_shift;

   int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=
    int8u(((int8u_ptr(ptrcomp(p ) + this.m_order.R )^ * alpha + 255 ) shr 8) + ((sr * cover + 255 ) shr 8 ) );

   int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=
    int8u(((int8u_ptr(ptrcomp(p ) + this.m_order.G )^ * alpha + 255 ) shr 8) + ((sg * cover + 255 ) shr 8 ) );

   int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=
    int8u(((int8u_ptr(ptrcomp(p ) + this.m_order.B )^ * alpha + 255 ) shr 8) + ((sb * cover + 255 ) shr 8 ) );

   int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=
    int8u(((int8u_ptr(ptrcomp(p ) + this.m_order.A )^ * alpha + 255 ) shr 8) + ((sa * cover + 255 ) shr 8 ) );

  end
 else
  begin
   int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u((int8u_ptr(ptrcomp(p ) + this.m_order.R )^ * sa + sr * da + base_mask ) shr base_shift );
   int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u((int8u_ptr(ptrcomp(p ) + this.m_order.G )^ * sa + sg * da + base_mask ) shr base_shift );
   int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u((int8u_ptr(ptrcomp(p ) + this.m_order.B )^ * sa + sb * da + base_mask ) shr base_shift );
   int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=int8u(sa );

  end;

end;

{ comp_op_rgba_xor }
// Dca' = Sca.(1 - Da) + Dca.(1 - Sa)
// Da'  = Sa + Da - 2.Sa.Da
procedure comp_op_rgba_xor(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 s1a ,d1a : unsigned;

begin
 if cover < 255 then
  begin
   sr:=(sr * cover + 255 ) shr 8;
   sg:=(sg * cover + 255 ) shr 8;
   sb:=(sb * cover + 255 ) shr 8;
   sa:=(sa * cover + 255 ) shr 8;

  end;

 s1a:=base_mask - sa;
 d1a:=base_mask - int8u_ptr(ptrcomp(p ) + this.m_order.A )^;

 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=
  int8u((int8u_ptr(ptrcomp(p ) + this.m_order.R )^ * s1a + sr * d1a + base_mask ) shr base_shift );

 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=
  int8u((int8u_ptr(ptrcomp(p ) + this.m_order.G )^ * s1a + sg * d1a + base_mask ) shr base_shift );

 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=
  int8u((int8u_ptr(ptrcomp(p ) + this.m_order.B )^ * s1a + sb * d1a + base_mask ) shr base_shift );

 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=
  int8u(sa + int8u_ptr(ptrcomp(p ) + this.m_order.A )^ - ((sa * int8u_ptr(ptrcomp(p ) + this.m_order.A )^ + base_mask div 2 ) shr (base_shift - 1 ) ) );

end;

{ comp_op_rgba_plus }
// Dca' = Sca + Dca
// Da'  = Sa + Da
procedure comp_op_rgba_plus(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 dr ,dg ,db ,da : unsigned;

begin
 if cover < 255 then
  begin
   sr:=(sr * cover + 255 ) shr 8;
   sg:=(sg * cover + 255 ) shr 8;
   sb:=(sb * cover + 255 ) shr 8;
   sa:=(sa * cover + 255 ) shr 8;

  end;

 dr:=int8u_ptr(ptrcomp(p ) + this.m_order.R )^ + sr;
 dg:=int8u_ptr(ptrcomp(p ) + this.m_order.G )^ + sg;
 db:=int8u_ptr(ptrcomp(p ) + this.m_order.B )^ + sb;
 da:=int8u_ptr(ptrcomp(p ) + this.m_order.A )^ + sa;

 if dr > base_mask then
  int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=base_mask
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u(dr );

 if dg > base_mask then
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=base_mask
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u(dg );

 if db > base_mask then
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=base_mask
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u(db );

 if da > base_mask then
  int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=base_mask
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=int8u(da );

end;

{ comp_op_rgba_minus }
// Dca' = Dca - Sca
// Da' = 1 - (1 - Sa).(1 - Da)
procedure comp_op_rgba_minus(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 dr ,dg ,db : unsigned;

begin
 if cover < 255 then
  begin
   sr:=(sr * cover + 255 ) shr 8;
   sg:=(sg * cover + 255 ) shr 8;
   sb:=(sb * cover + 255 ) shr 8;
   sa:=(sa * cover + 255 ) shr 8;

  end;

 dr:=int8u_ptr(ptrcomp(p ) + this.m_order.R )^ - sr;
 dg:=int8u_ptr(ptrcomp(p ) + this.m_order.G )^ - sg;
 db:=int8u_ptr(ptrcomp(p ) + this.m_order.B )^ - sb;

 if dr > base_mask then
  int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=0
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u(dr );

 if dg > base_mask then
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=0
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u(dg );

 if db > base_mask then
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=0
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u(db );

 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=
  int8u(base_mask - (((base_mask - sa ) * (base_mask - int8u_ptr(ptrcomp(p ) + this.m_order.A )^ ) + base_mask ) shr base_shift ) );

end;

{ comp_op_rgba_multiply }
// Dca' = Sca.Dca + Sca.(1 - Da) + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure comp_op_rgba_multiply(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 s1a ,d1a ,dr ,dg ,db : unsigned;

begin
 if cover < 255 then
  begin
   sr:=(sr * cover + 255 ) shr 8;
   sg:=(sg * cover + 255 ) shr 8;
   sb:=(sb * cover + 255 ) shr 8;
   sa:=(sa * cover + 255 ) shr 8;

  end;

 s1a:=base_mask - sa;
 d1a:=base_mask - int8u_ptr(ptrcomp(p ) + this.m_order.A )^;
 dr :=int8u_ptr(ptrcomp(p ) + this.m_order.R )^;
 dg :=int8u_ptr(ptrcomp(p ) + this.m_order.G )^;
 db :=int8u_ptr(ptrcomp(p ) + this.m_order.B )^;

 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u((sr * dr + sr * d1a + dr * s1a + base_mask ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u((sg * dg + sg * d1a + dg * s1a + base_mask ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u((sb * db + sb * d1a + db * s1a + base_mask ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=
  int8u(sa + int8u_ptr(ptrcomp(p ) + this.m_order.A )^ - ((sa * int8u_ptr(ptrcomp(p ) + this.m_order.A )^ + base_mask ) shr base_shift ) );

end;

{ comp_op_rgba_screen }
// Dca' = Sca + Dca - Sca.Dca
// Da'  = Sa + Da - Sa.Da
procedure comp_op_rgba_screen(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 dr ,dg ,db ,da : unsigned;

begin
 if cover < 255 then
  begin
   sr:=(sr * cover + 255 ) shr 8;
   sg:=(sg * cover + 255 ) shr 8;
   sb:=(sb * cover + 255 ) shr 8;
   sa:=(sa * cover + 255 ) shr 8;

  end;

 dr:=int8u_ptr(ptrcomp(p ) + this.m_order.R )^;
 dg:=int8u_ptr(ptrcomp(p ) + this.m_order.G )^;
 db:=int8u_ptr(ptrcomp(p ) + this.m_order.B )^;
 da:=int8u_ptr(ptrcomp(p ) + this.m_order.A )^;

 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u(sr + dr - ((sr * dr + base_mask ) shr base_shift ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u(sg + dg - ((sg * dg + base_mask ) shr base_shift ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u(sb + db - ((sb * db + base_mask ) shr base_shift ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=int8u(sa + da - ((sa * da + base_mask ) shr base_shift ) );

end;

{ comp_op_rgba_overlay }
// if 2.Dca < Da
//   Dca' = 2.Sca.Dca + Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise
//   Dca' = Sa.Da - 2.(Da - Dca).(Sa - Sca) + Sca.(1 - Da) + Dca.(1 - Sa)
//
// Da' = Sa + Da - Sa.Da
procedure comp_op_rgba_overlay(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 d1a ,s1a ,dr ,dg ,db ,da ,sada : unsigned;

begin
 if cover < 255 then
  begin
   sr:=(sr * cover + 255 ) shr 8;
   sg:=(sg * cover + 255 ) shr 8;
   sb:=(sb * cover + 255 ) shr 8;
   sa:=(sa * cover + 255 ) shr 8;

  end;

 d1a :=base_mask - int8u_ptr(ptrcomp(p ) + this.m_order.A )^;
 s1a :=base_mask - sa;
 dr  :=int8u_ptr(ptrcomp(p ) + this.m_order.R )^;
 dg  :=int8u_ptr(ptrcomp(p ) + this.m_order.G )^;
 db  :=int8u_ptr(ptrcomp(p ) + this.m_order.B )^;
 da  :=int8u_ptr(ptrcomp(p ) + this.m_order.A )^;
 sada:=sa * int8u_ptr(ptrcomp(p ) + this.m_order.A )^;

 if 2 * dr < da then
  int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u((2 * sr * dr + sr * d1a + dr * s1a ) shr base_shift )
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u((sada - 2 * (da - dr ) * (sa - sr ) + sr * d1a + dr * s1a ) shr base_shift );

 if 2 * dg < da then
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u((2 * sg * dg + sg * d1a + dg * s1a ) shr base_shift )
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u((sada - 2 * (da - dg ) * (sa - sg ) + sg * d1a + dg * s1a ) shr base_shift );

 if 2 * db < da then
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u((2 * sb * db + sb * d1a + db * s1a ) shr base_shift )
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u((sada - 2 * (da - db ) * (sa - sb ) + sb * d1a + db * s1a ) shr base_shift );

 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=int8u(sa + da - ((sa * da + base_mask ) shr base_shift ) );

end;

{ sd_min }
function sd_min(a ,b : unsigned ) : unsigned;
begin
 if a < b then
  result:=a
 else
  result:=b;

end;

{ sd_max }
function sd_max(a ,b : unsigned ) : unsigned;
begin
 if a > b then
  result:=a
 else
  result:=b;

end;

{ comp_op_rgba_darken }
// Dca' = min(Sca.Da, Dca.Sa) + Sca.(1 - Da) + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure comp_op_rgba_darken(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 d1a ,s1a ,dr ,dg ,db ,da : unsigned;

begin
 if cover < 255 then
  begin
   sr:=(sr * cover + 255 ) shr 8;
   sg:=(sg * cover + 255 ) shr 8;
   sb:=(sb * cover + 255 ) shr 8;
   sa:=(sa * cover + 255 ) shr 8;

  end;

 d1a:=base_mask - int8u_ptr(ptrcomp(p ) + this.m_order.A )^;
 s1a:=base_mask - sa;
 dr :=int8u_ptr(ptrcomp(p ) + this.m_order.R )^;
 dg :=int8u_ptr(ptrcomp(p ) + this.m_order.G )^;
 db :=int8u_ptr(ptrcomp(p ) + this.m_order.B )^;
 da :=int8u_ptr(ptrcomp(p ) + this.m_order.A )^;

 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u((sd_min(sr * da, dr * sa ) + sr * d1a + dr * s1a ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u((sd_min(sg * da, dg * sa ) + sg * d1a + dg * s1a ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u((sd_min(sb * da, db * sa ) + sb * d1a + db * s1a ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=int8u(sa + da - ((sa * da + base_mask ) shr base_shift ) );

end;

{ comp_op_rgba_lighten }
// Dca' = max(Sca.Da, Dca.Sa) + Sca.(1 - Da) + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure comp_op_rgba_lighten(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 d1a ,s1a ,dr ,dg ,db ,da : unsigned;

begin
 if cover < 255 then
  begin
   sr:=(sr * cover + 255 ) shr 8;
   sg:=(sg * cover + 255 ) shr 8;
   sb:=(sb * cover + 255 ) shr 8;
   sa:=(sa * cover + 255 ) shr 8;

  end;

 d1a:=base_mask - int8u_ptr(ptrcomp(p ) + this.m_order.A )^;
 s1a:=base_mask - sa;
 dr :=int8u_ptr(ptrcomp(p ) + this.m_order.R )^;
 dg :=int8u_ptr(ptrcomp(p ) + this.m_order.G )^;
 db :=int8u_ptr(ptrcomp(p ) + this.m_order.B )^;
 da :=int8u_ptr(ptrcomp(p ) + this.m_order.A )^;

 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u((sd_max(sr * da, dr * sa ) + sr * d1a + dr * s1a ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u((sd_max(sg * da, dg * sa ) + sg * d1a + dg * s1a ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u((sd_max(sb * da, db * sa ) + sb * d1a + db * s1a ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=int8u(sa + da - ((sa * da + base_mask ) shr base_shift ) );

end;

{ comp_op_rgba_color_dodge }
// if Sca.Da + Dca.Sa >= Sa.Da
//   Dca' = Sa.Da + Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise
//   Dca' = Dca.Sa/(1-Sca/Sa) + Sca.(1 - Da) + Dca.(1 - Sa)
//
// Da'  = Sa + Da - Sa.Da
procedure comp_op_rgba_color_dodge(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 d1a ,s1a ,dr ,dg ,db ,da : unsigned;

 drsa ,dgsa ,dbsa ,srda ,sgda ,sbda ,sada : int;

begin
 if cover < 255 then
  begin
   sr:=(sr * cover + 255 ) shr 8;
   sg:=(sg * cover + 255 ) shr 8;
   sb:=(sb * cover + 255 ) shr 8;
   sa:=(sa * cover + 255 ) shr 8;

  end;

 d1a :=base_mask - int8u_ptr(ptrcomp(p ) + this.m_order.A )^;
 s1a :=base_mask - sa;
 dr  :=int8u_ptr(ptrcomp(p ) + this.m_order.R )^;
 dg  :=int8u_ptr(ptrcomp(p ) + this.m_order.G )^;
 db  :=int8u_ptr(ptrcomp(p ) + this.m_order.B )^;
 da  :=int8u_ptr(ptrcomp(p ) + this.m_order.A )^;
 drsa:=dr * sa;
 dgsa:=dg * sa;
 dbsa:=db * sa;
 srda:=sr * da;
 sgda:=sg * da;
 sbda:=sb * da;
 sada:=sa * da;

 if srda + drsa >= sada then
  int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u(shr_int32(sada + sr * d1a + dr * s1a ,base_shift ) )
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u(drsa div (base_mask - (sr shl base_shift ) div sa ) + ((sr * d1a + dr * s1a ) shr base_shift ) );

 if  sgda + dgsa >= sada then
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u(shr_int32(sada + sg * d1a + dg * s1a ,base_shift ) )
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u(dgsa div (base_mask - (sg shl base_shift ) div sa ) + ((sg * d1a + dg * s1a ) shr base_shift ) );

 if sbda + dbsa >= sada then
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u(shr_int32(sada + sb * d1a + db * s1a ,base_shift ) )
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u(dbsa div (base_mask - (sb shl base_shift ) div sa ) + ((sb * d1a + db * s1a ) shr base_shift ) );

 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=int8u(sa + da - ((sa * da + base_mask) shr base_shift ) );

end;

{ comp_op_rgba_color_burn }
// if Sca.Da + Dca.Sa <= Sa.Da
//   Dca' = Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise
//   Dca' = Sa.(Sca.Da + Dca.Sa - Sa.Da)/Sca + Sca.(1 - Da) + Dca.(1 - Sa)
//
// Da'  = Sa + Da - Sa.Da
procedure comp_op_rgba_color_burn(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 d1a ,s1a ,dr ,dg ,db ,da : unsigned;

 drsa ,dgsa ,dbsa ,srda ,sgda ,sbda ,sada : int;

begin
 if cover < 255 then
  begin
   sr:=(sr * cover + 255 ) shr 8;
   sg:=(sg * cover + 255 ) shr 8;
   sb:=(sb * cover + 255 ) shr 8;
   sa:=(sa * cover + 255 ) shr 8;

  end;

 d1a :=base_mask - int8u_ptr(ptrcomp(p ) + this.m_order.A )^;
 s1a :=base_mask - sa;
 dr  :=int8u_ptr(ptrcomp(p ) + this.m_order.R )^;
 dg  :=int8u_ptr(ptrcomp(p ) + this.m_order.G )^;
 db  :=int8u_ptr(ptrcomp(p ) + this.m_order.B )^;
 da  :=int8u_ptr(ptrcomp(p ) + this.m_order.A )^;
 drsa:=dr * sa;
 dgsa:=dg * sa;
 dbsa:=db * sa;
 srda:=sr * da;
 sgda:=sg * da;
 sbda:=sb * da;
 sada:=sa * da;

 if srda + drsa <= sada then
  int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u((sr * d1a + dr * s1a ) shr base_shift )
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u(shr_int32(sa * (srda + drsa - sada ) div sr + sr * d1a + dr * s1a ,base_shift ) );

 if sgda + dgsa <= sada then
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u((sg * d1a + dg * s1a ) shr base_shift)
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u(shr_int32(sa * (sgda + dgsa - sada ) div sg + sg * d1a + dg * s1a ,base_shift ) );

 if sbda + dbsa <= sada then
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u((sb * d1a + db * s1a ) shr base_shift)
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u(shr_int32(sa * (sbda + dbsa - sada ) div sb + sb * d1a + db * s1a ,base_shift ) );

 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=int8u(sa + da - ((sa * da + base_mask ) shr base_shift ) );

end;

{ comp_op_rgba_hard_light }
// if 2.Sca < Sa
//    Dca' = 2.Sca.Dca + Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise
//    Dca' = Sa.Da - 2.(Da - Dca).(Sa - Sca) + Sca.(1 - Da) + Dca.(1 - Sa)
//
// Da'  = Sa + Da - Sa.Da
procedure comp_op_rgba_hard_light(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 d1a ,s1a ,dr ,dg ,db ,da ,sada : unsigned;

begin
 if cover < 255 then
  begin
   sr:=(sr * cover + 255 ) shr 8;
   sg:=(sg * cover + 255 ) shr 8;
   sb:=(sb * cover + 255 ) shr 8;
   sa:=(sa * cover + 255 ) shr 8;

  end;

 d1a :=base_mask - int8u_ptr(ptrcomp(p ) + this.m_order.A )^;
 s1a :=base_mask - sa;
 dr  :=int8u_ptr(ptrcomp(p ) + this.m_order.R )^;
 dg  :=int8u_ptr(ptrcomp(p ) + this.m_order.G )^;
 db  :=int8u_ptr(ptrcomp(p ) + this.m_order.B )^;
 da  :=int8u_ptr(ptrcomp(p ) + this.m_order.A )^;
 sada:=sa * da;

 if 2*sr < sa then
  int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u((2 * sr * dr + sr * d1a + dr * s1a ) shr base_shift )
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u((sada - 2 * (da - dr ) * (sa - sr ) + sr * d1a + dr * s1a ) shr base_shift );

 if 2*sg < sa then
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u((2 * sg * dg + sg * d1a + dg * s1a ) shr base_shift )
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u((sada - 2 * (da - dg ) * (sa - sg ) + sg * d1a + dg * s1a ) shr base_shift );

 if 2*sb < sa then
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u((2 * sb * db + sb * d1a + db * s1a ) shr base_shift )
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u((sada - 2 * (da - db ) * (sa - sb ) + sb * d1a + db * s1a ) shr base_shift );

 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=int8u(sa + da - ((sa * da + base_mask ) shr base_shift ) );

end;

{ comp_op_rgba_soft_light }
// if 2.Sca < Sa
//   Dca' = Dca.(Sa + (1 - Dca/Da).(2.Sca - Sa)) + Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise if 8.Dca <= Da
//   Dca' = Dca.(Sa + (1 - Dca/Da).(2.Sca - Sa).(3 - 8.Dca/Da)) + Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise
//   Dca' = (Dca.Sa + ((Dca/Da)^(0.5).Da - Dca).(2.Sca - Sa)) + Sca.(1 - Da) + Dca.(1 - Sa)
//
// Da'  = Sa + Da - Sa.Da
procedure comp_op_rgba_soft_light(this : pixel_formats_ptr; p : int8u_ptr; r ,g ,b ,a ,cover : unsigned );
var
 sr ,sg ,sb ,sa ,dr ,dg ,db ,da : double;

begin
 sr:=(r * cover ) / (base_mask * 255 );
 sg:=(g * cover ) / (base_mask * 255 );
 sb:=(b * cover ) / (base_mask * 255 );
 sa:=(a * cover ) / (base_mask * 255 );
 dr:=int8u_ptr(ptrcomp(p ) + this.m_order.R )^ / base_mask;
 dg:=int8u_ptr(ptrcomp(p ) + this.m_order.G )^ / base_mask;
 db:=int8u_ptr(ptrcomp(p ) + this.m_order.B )^ / base_mask;

 if int8u_ptr(ptrcomp(p ) + this.m_order.A )^ <> 0 then
  da:=int8u_ptr(ptrcomp(p ) + this.m_order.A )^ / base_mask
 else
  da:=1 / base_mask;

 if cover < 255 then
  a:=(a * cover + 255 ) shr 8;

 if 2 * sr < sa then
  dr:=dr * (sa + (1 - dr / da ) * (2 * sr - sa ) ) + sr * (1 - da ) + dr * (1 - sa )
 else
  if 8 * dr <= da then
   dr:=dr * (sa + (1 - dr / da ) * (2 * sr - sa ) * (3 - 8 * dr / da ) ) + sr * (1 - da ) + dr * (1 - sa )
  else
   dr:=(dr * sa + (Sqrt(dr / da ) * da - dr ) * (2 * sr - sa ) ) + sr * (1 - da ) + dr * (1 - sa );

 if 2 * sg < sa then
  dg:=dg * (sa + (1 - dg / da ) * (2 * sg - sa ) ) + sg * (1 - da ) + dg * (1 - sa )
 else
  if 8 * dg <= da then
   dg:=dg * (sa + (1 - dg / da ) * (2 * sg - sa ) * (3 - 8 * dg / da ) ) + sg * (1 - da ) + dg * (1 - sa )
  else
   dg:=(dg * sa + (Sqrt(dg / da ) * da - dg ) * (2 * sg - sa ) ) + sg * (1 - da ) + dg * (1 - sa );

 if 2 * sb < sa then
  db:=db * (sa + (1 - db / da ) * (2 * sb - sa ) ) + sb * (1 - da ) + db * (1 - sa )
 else
  if 8 * db <= da then
   db:=db * (sa + (1 - db / da ) * (2 * sb - sa ) * (3 - 8 * db / da ) ) + sb * (1 - da ) + db * (1 - sa )
  else
   db:=(db * sa + (Sqrt(db / da ) * da - db ) * (2 * sb - sa ) ) + sb * (1 - da ) + db * (1 - sa );

 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u(trunc(dr * base_mask ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u(trunc(dg * base_mask ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u(trunc(db * base_mask ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=
  int8u(a + int8u_ptr(ptrcomp(p ) + this.m_order.A )^ - ((a * int8u_ptr(ptrcomp(p ) + this.m_order.A )^ + base_mask ) shr base_shift ) );

end;

{ comp_op_rgba_difference }
// Dca' = Sca + Dca - 2.min(Sca.Da, Dca.Sa)
// Da'  = Sa + Da - Sa.Da
procedure comp_op_rgba_difference(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 dr ,dg ,db ,da : unsigned;

begin
 if cover < 255 then
  begin
   sr:=(sr * cover + 255 ) shr 8;
   sg:=(sg * cover + 255 ) shr 8;
   sb:=(sb * cover + 255 ) shr 8;
   sa:=(sa * cover + 255 ) shr 8;

  end;

 dr:=int8u_ptr(ptrcomp(p ) + this.m_order.R )^;
 dg:=int8u_ptr(ptrcomp(p ) + this.m_order.G )^;
 db:=int8u_ptr(ptrcomp(p ) + this.m_order.B )^;
 da:=int8u_ptr(ptrcomp(p ) + this.m_order.A )^;

 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u(sr + dr - ((2 * sd_min(sr * da ,dr * sa ) ) shr base_shift ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u(sg + dg - ((2 * sd_min(sg * da ,dg * sa ) ) shr base_shift ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u(sb + db - ((2 * sd_min(sb * da ,db * sa ) ) shr base_shift ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=int8u(sa + da - ((sa * da + base_mask ) shr base_shift ) );

end;

{ comp_op_rgba_exclusion }
// Dca' = (Sca.Da + Dca.Sa - 2.Sca.Dca) + Sca.(1 - Da) + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure comp_op_rgba_exclusion(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 d1a ,s1a ,dr ,dg ,db ,da : unsigned;

begin
 if cover < 255 then
  begin
   sr:=(sr * cover + 255 ) shr 8;
   sg:=(sg * cover + 255 ) shr 8;
   sb:=(sb * cover + 255 ) shr 8;
   sa:=(sa * cover + 255 ) shr 8;

  end;

 d1a:=base_mask - int8u_ptr(ptrcomp(p ) + this.m_order.A )^;
 s1a:=base_mask - sa;
 dr :=int8u_ptr(ptrcomp(p ) + this.m_order.R )^;
 dg :=int8u_ptr(ptrcomp(p ) + this.m_order.G )^;
 db :=int8u_ptr(ptrcomp(p ) + this.m_order.B )^;
 da :=int8u_ptr(ptrcomp(p ) + this.m_order.A )^;

 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u((sr * da + dr * sa - 2 * sr * dr + sr * d1a + dr * s1a ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u((sg * da + dg * sa - 2 * sg * dg + sg * d1a + dg * s1a ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u((sb * da + db * sa - 2 * sb * db + sb * d1a + db * s1a ) shr base_shift );
 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=int8u(sa + da - ((sa * da + base_mask ) shr base_shift ) );

end;

{ comp_op_rgba_contrast }
procedure comp_op_rgba_contrast(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 dr ,dg ,db ,da ,d2a ,r ,g ,b : int;

 s2a : unsigned;

begin
 if cover < 255 then
  begin
   sr:=(sr * cover + 255 ) shr 8;
   sg:=(sg * cover + 255 ) shr 8;
   sb:=(sb * cover + 255 ) shr 8;
   sa:=(sa * cover + 255 ) shr 8;

  end;

 dr :=int8u_ptr(ptrcomp(p ) + this.m_order.R )^;
 dg :=int8u_ptr(ptrcomp(p ) + this.m_order.G )^;
 db :=int8u_ptr(ptrcomp(p ) + this.m_order.B )^;
 da :=int8u_ptr(ptrcomp(p ) + this.m_order.A )^;
 d2a:=shr_int32(da ,1 );
 s2a:=sa shr 1;

 r:=shr_int32((dr - d2a) * ((sr - s2a ) * 2 + base_mask ) ,base_shift ) + d2a;
 g:=shr_int32((dg - d2a) * ((sg - s2a ) * 2 + base_mask ) ,base_shift ) + d2a;
 b:=shr_int32((db - d2a) * ((sb - s2a ) * 2 + base_mask ) ,base_shift ) + d2a;

 if r < 0 then
  r:=0;

 if g < 0 then
  g:=0;

 if b < 0 then
  b:=0;

 if r > da then
  int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u(trunc(da ) )
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u(trunc(r ) );

 if g > da then
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u(trunc(da ) )
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u(trunc(g ) );

 if b > da then
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u(trunc(da ) )
 else
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u(trunc(b ) );

end;

{ comp_op_rgba_invert }
// Dca' = (Da - Dca) * Sa + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure comp_op_rgba_invert(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 da ,dr ,dg ,db ,s1a : int;

begin
 sa:=(sa * cover + 255 ) shr 8;

 if sa <> 0 then
  begin
   da :=int8u_ptr(ptrcomp(p ) + this.m_order.A )^;
   dr :=shr_int32((da - int8u_ptr(ptrcomp(p ) + this.m_order.R )^ ) * sa + base_mask ,base_shift );
   dg :=shr_int32((da - int8u_ptr(ptrcomp(p ) + this.m_order.G )^ ) * sa + base_mask ,base_shift );
   db :=shr_int32((da - int8u_ptr(ptrcomp(p ) + this.m_order.B )^ ) * sa + base_mask ,base_shift );
   s1a:=base_mask - sa;

   int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=
    int8u(
     dr + shr_int32(int8u_ptr(ptrcomp(p ) + this.m_order.R )^ * s1a + base_mask ,base_shift ) );

   int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=
    int8u(
     dg + shr_int32(int8u_ptr(ptrcomp(p ) + this.m_order.G )^ * s1a + base_mask ,base_shift ) );

   int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=
    int8u(
     db + shr_int32(int8u_ptr(ptrcomp(p ) + this.m_order.B )^ * s1a + base_mask ,base_shift ) );

   int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=
     int8u(
      sa + da - shr_int32(sa * da + base_mask ,base_shift ) );

  end;

end;

{ comp_op_rgba_invert_rgb }
// Dca' = (Da - Dca) * Sca + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure comp_op_rgba_invert_rgb(this : pixel_formats_ptr; p : int8u_ptr; sr ,sg ,sb ,sa ,cover : unsigned );
var
 da ,dr ,dg ,db ,s1a : int;

begin
 if cover < 255 then
  begin
   sr:=shr_int32(sr * cover + 255 ,8 );
   sg:=shr_int32(sg * cover + 255 ,8 );
   sb:=shr_int32(sb * cover + 255 ,8 );
   sa:=shr_int32(sa * cover + 255 ,8 );

  end;

 if sa <> 0 then
  begin
   da :=int8u_ptr(ptrcomp(p ) + this.m_order.A )^;
   dr :=shr_int32((da - int8u_ptr(ptrcomp(p ) + this.m_order.R )^ ) * sr + base_mask ,base_shift );
   dg :=shr_int32((da - int8u_ptr(ptrcomp(p ) + this.m_order.G )^ ) * sg + base_mask ,base_shift );
   db :=shr_int32((da - int8u_ptr(ptrcomp(p ) + this.m_order.B )^ ) * sb + base_mask ,base_shift );
   s1a:=base_mask - sa;

   int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=
    int8u(
     dr + shr_int32(int8u_ptr(ptrcomp(p ) + this.m_order.R )^ * s1a + base_mask ,base_shift ) );

   int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=
    int8u(
     dg + shr_int32(int8u_ptr(ptrcomp(p ) + this.m_order.G )^ * s1a + base_mask ,base_shift ) );

   int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=
    int8u(
     db + shr_int32(int8u_ptr(ptrcomp(p ) + this.m_order.B )^ * s1a + base_mask ,base_shift ) );

   int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=
    int8u(
     sa + da - shr_int32(sa * da + base_mask ,base_shift ) );

  end;

end;

const
 comp_op_table_rgba : array[0..byte(end_of_comp_op_e ) - 1 ] of func_blend_pix = (

  comp_op_rgba_clear ,
  comp_op_rgba_src ,
  comp_op_rgba_dst ,
  comp_op_rgba_src_over ,
  comp_op_rgba_dst_over ,
  comp_op_rgba_src_in ,
  comp_op_rgba_dst_in ,
  comp_op_rgba_src_out ,
  comp_op_rgba_dst_out ,
  comp_op_rgba_src_atop ,
  comp_op_rgba_dst_atop ,
  comp_op_rgba_xor ,
  comp_op_rgba_plus ,
  comp_op_rgba_minus ,
  comp_op_rgba_multiply ,
  comp_op_rgba_screen ,
  comp_op_rgba_overlay ,
  comp_op_rgba_darken ,
  comp_op_rgba_lighten ,
  comp_op_rgba_color_dodge ,
  comp_op_rgba_color_burn ,
  comp_op_rgba_hard_light ,
  comp_op_rgba_soft_light ,
  comp_op_rgba_difference ,
  comp_op_rgba_exclusion ,
  comp_op_rgba_contrast ,
  comp_op_rgba_invert ,
  comp_op_rgba_invert_rgb );

{ COMP_OP_ADAPTOR_RGBA }
procedure comp_op_adaptor_rgba;
begin
 comp_op_table_rgba[op ](
  this ,p ,
  (cr * ca + base_mask ) shr base_shift ,
  (cg * ca + base_mask ) shr base_shift ,
  (cb * ca + base_mask ) shr base_shift ,
  ca ,cover );

end;

{ COMP_OP_ADAPTOR_CLIP_TO_DST_RGBA }
procedure comp_op_adaptor_clip_to_dst_rgba_pre(this : pixel_formats_ptr; op : unsigned; p : int8u_ptr; cr ,cg ,cb ,ca ,cover : unsigned );
var
 da : unsigned;

begin
 da:=int8u_ptr(ptrcomp(p ) + this.m_order.A )^;

 comp_op_table_rgba[op ](
  this ,p ,
  (cr * da + base_mask ) shr base_shift ,
  (cg * da + base_mask ) shr base_shift ,
  (cb * da + base_mask ) shr base_shift ,
  (ca * da + base_mask ) shr base_shift ,
  cover );

end;

END.

