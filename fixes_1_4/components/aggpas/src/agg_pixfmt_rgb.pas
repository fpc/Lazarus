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
// 23.06.2006-Milano: ptrcomp adjustments
// 18.03.2006-Milano: pf_xxx.inc completed
// 13.01.2006-Milano: Unit port establishment
//
{ agg_pixfmt_rgb.pas }
unit
 agg_pixfmt_rgb ;

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
 procedure pixfmt_bgr24(var pixf : pixel_formats; rb : rendering_buffer_ptr );
 procedure pixfmt_rgb24(var pixf : pixel_formats; rb : rendering_buffer_ptr );

 procedure pixfmt_bgr24_pre(var pixf : pixel_formats; rb : rendering_buffer_ptr );
 procedure pixfmt_rgb24_pre(var pixf : pixel_formats; rb : rendering_buffer_ptr );

 procedure pixfmt_bgr24_gamma(var pixf : pixel_formats; rb : rendering_buffer_ptr; g : gamma_ptr );
 procedure pixfmt_rgb24_gamma(var pixf : pixel_formats; rb : rendering_buffer_ptr; g : gamma_ptr );


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ fmt24_row }
function fmt24_row(this : pixel_formats_ptr; x ,y : int ) : row_data_type;
begin
 result.Construct(
  x ,this._width - 1 ,
  int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * 3 * sizeof(int8u ) ) );

end;

{ fmt24_copy_from }
procedure fmt24_copy_from(this : pixel_formats_ptr; from : rendering_buffer_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned );
begin
 move(
  int8u_ptr(ptrcomp(from.row(ysrc ) ) + xsrc * 3 * sizeof(int8u ) )^ ,
  int8u_ptr(ptrcomp(this.m_rbuf.row(ydst ) ) + xdst * 3 * sizeof(int8u ) )^ ,
  sizeof(int8u ) * 3 * len );

end;

{ order24_gamma_dir_apply }
procedure order24_gamma_dir_apply(this : pixel_formats; p : int8u_ptr );
begin
 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u(this.m_apply.dir(int8u_ptr(ptrcomp(p ) + this.m_order.R )^ ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u(this.m_apply.dir(int8u_ptr(ptrcomp(p ) + this.m_order.G )^ ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u(this.m_apply.dir(int8u_ptr(ptrcomp(p ) + this.m_order.B )^ ) );

end;

{ order24_gamma_inv_apply }
procedure order24_gamma_inv_apply(this : pixel_formats; p : int8u_ptr );
begin
 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=int8u(this.m_apply.inv(int8u_ptr(ptrcomp(p ) + this.m_order.R )^ ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=int8u(this.m_apply.inv(int8u_ptr(ptrcomp(p ) + this.m_order.G )^ ) );
 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=int8u(this.m_apply.inv(int8u_ptr(ptrcomp(p ) + this.m_order.B )^ ) );

end;

{ order24_for_each_pixel }
procedure order24_for_each_pixel(this : pixel_formats_ptr; f : func_apply_gamma );
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

    inc(ptrcomp(p ) ,3 );
    dec(len );

   until len = 0;

   inc(y );

  end;

end;

{$I pf_bgr24.inc }

{ PIXFMT_BGR24 }
procedure pixfmt_bgr24;
begin
 pixf.Construct(rb );

 pixf.m_order:=bgr_order;

 pixf.m_pix_width:=3;

 pixf.copy_pixel :=@bgr24_copy_pixel;
 pixf.blend_pixel:=@bgr24_blend_pixel;

 pixf.pixel:=@bgr24_pixel;
 pixf.row  :=@fmt24_row;

 pixf.copy_hline:=@bgr24_copy_hline;
 pixf.copy_vline:=@bgr24_copy_vline;

 pixf.blend_hline:=@bgr24_blend_hline;
 pixf.blend_vline:=@bgr24_blend_vline;

 pixf.blend_solid_hspan:=@bgr24_blend_solid_hspan;
 pixf.blend_solid_vspan:=@bgr24_blend_solid_vspan;

 pixf.copy_color_hspan:=@bgr24_copy_color_hspan;
 pixf.copy_color_vspan:=@bgr24_copy_color_vspan;

 pixf.blend_color_hspan:=@bgr24_blend_color_hspan;
 pixf.blend_color_vspan:=@bgr24_blend_color_vspan;

 pixf.copy_from :=@fmt24_copy_from;
 pixf.blend_from:=@bgr24_blend_from;

 pixf.blend_from_color:=@bgr24_blend_from_color;
 pixf.blend_from_lut  :=@bgr24_blend_from_lut;
 
 pixf.for_each_pixel :=@order24_for_each_pixel;
 pixf.gamma_dir_apply:=@order24_gamma_dir_apply;
 pixf.gamma_inv_apply:=@order24_gamma_inv_apply;

end;

{$I pf_rgb24.inc }

{ PIXFMT_RGB24 }
procedure pixfmt_rgb24;
begin
 pixf.Construct(rb );

 pixf.m_order:=rgb_order;

 pixf.m_pix_width:=3;

 pixf.copy_pixel :=@rgb24_copy_pixel;
 pixf.blend_pixel:=@rgb24_blend_pixel;

 pixf.pixel:=@rgb24_pixel;
 pixf.row  :=@fmt24_row;

 pixf.copy_hline:=@rgb24_copy_hline;
 pixf.copy_vline:=@rgb24_copy_vline;

 pixf.blend_hline:=@rgb24_blend_hline;
 pixf.blend_vline:=@rgb24_blend_vline;

 pixf.blend_solid_hspan:=@rgb24_blend_solid_hspan;
 pixf.blend_solid_vspan:=@rgb24_blend_solid_vspan;

 pixf.copy_color_hspan:=@rgb24_copy_color_hspan;
 pixf.copy_color_vspan:=@rgb24_copy_color_vspan;

 pixf.blend_color_hspan:=@rgb24_blend_color_hspan;
 pixf.blend_color_vspan:=@rgb24_blend_color_vspan;

 pixf.copy_from :=@fmt24_copy_from;
 pixf.blend_from:=@rgb24_blend_from;

 pixf.blend_from_color:=@rgb24_blend_from_color;
 pixf.blend_from_lut  :=@rgb24_blend_from_lut;
 
 pixf.for_each_pixel :=@order24_for_each_pixel;
 pixf.gamma_dir_apply:=@order24_gamma_dir_apply;
 pixf.gamma_inv_apply:=@order24_gamma_inv_apply;

end;

{$I pf_bgr24_pre.inc }

{ PIXFMT_BGR24_PRE }
procedure pixfmt_bgr24_pre;
begin
 pixf.Construct(rb );

 pixf.m_order:=bgr_order;

 pixf.m_pix_width:=3;

 pixf.copy_pixel :=@bgr24_copy_pixel;
 pixf.blend_pixel:=@bgr24_pre_blend_pixel;

 pixf.pixel:=@bgr24_pixel;
 pixf.row  :=@fmt24_row;

 pixf.copy_hline:=@bgr24_copy_hline;
 pixf.copy_vline:=@bgr24_copy_vline;

 pixf.blend_hline:=@bgr24_pre_blend_hline;
 pixf.blend_vline:=@bgr24_pre_blend_vline;

 pixf.blend_solid_hspan:=@bgr24_pre_blend_solid_hspan;
 pixf.blend_solid_vspan:=@bgr24_pre_blend_solid_vspan;

 pixf.copy_color_hspan:=@bgr24_copy_color_hspan;
 pixf.copy_color_vspan:=@bgr24_copy_color_vspan;

 pixf.blend_color_hspan:=@bgr24_pre_blend_color_hspan;
 pixf.blend_color_vspan:=@bgr24_pre_blend_color_vspan;

 pixf.copy_from :=@fmt24_copy_from;
 pixf.blend_from:=@bgr24_pre_blend_from;

 pixf.blend_from_color:=@bgr24_pre_blend_from_color;
 pixf.blend_from_lut  :=@bgr24_pre_blend_from_lut;

 pixf.for_each_pixel :=@order24_for_each_pixel;
 pixf.gamma_dir_apply:=@order24_gamma_dir_apply;
 pixf.gamma_inv_apply:=@order24_gamma_inv_apply;

end;

{$I pf_rgb24_pre.inc }

{ PIXFMT_RGB24_PRE }
procedure pixfmt_rgb24_pre;
begin
 pixf.Construct(rb );

 pixf.m_order:=rgb_order;

 pixf.m_pix_width:=3;

 pixf.copy_pixel :=@rgb24_copy_pixel;
 pixf.blend_pixel:=@rgb24_pre_blend_pixel;

 pixf.pixel:=@rgb24_pixel;
 pixf.row  :=@fmt24_row;

 pixf.copy_hline:=@rgb24_copy_hline;
 pixf.copy_vline:=@rgb24_copy_vline;

 pixf.blend_hline:=@rgb24_pre_blend_hline;
 pixf.blend_vline:=@rgb24_pre_blend_vline;

 pixf.blend_solid_hspan:=@rgb24_pre_blend_solid_hspan;
 pixf.blend_solid_vspan:=@rgb24_pre_blend_solid_vspan;

 pixf.copy_color_hspan:=@rgb24_copy_color_hspan;
 pixf.copy_color_vspan:=@rgb24_copy_color_vspan;
 
 pixf.blend_color_hspan:=@rgb24_pre_blend_color_hspan;
 pixf.blend_color_vspan:=@rgb24_pre_blend_color_vspan;

 pixf.copy_from :=@fmt24_copy_from;
 pixf.blend_from:=@rgb24_pre_blend_from;

 pixf.blend_from_color:=@rgb24_pre_blend_from_color;
 pixf.blend_from_lut  :=@rgb24_pre_blend_from_lut;
 
 pixf.for_each_pixel :=@order24_for_each_pixel;
 pixf.gamma_dir_apply:=@order24_gamma_dir_apply;
 pixf.gamma_inv_apply:=@order24_gamma_inv_apply;

end;

{$I pf_bgr24_gamma.inc }

{ PIXFMT_BGR24_GAMMA }
procedure pixfmt_bgr24_gamma;
begin
 pixf.Construct(rb );

 pixf.m_order:=bgr_order;
 pixf.m_gamma:=g;

 pixf.m_pix_width:=3;

 pixf.copy_pixel :=@bgr24_copy_pixel;
 pixf.blend_pixel:=@bgr24_gamma_blend_pixel;

 pixf.pixel:=@bgr24_pixel;
 pixf.row  :=@fmt24_row;

 pixf.copy_hline:=@bgr24_copy_hline;
 pixf.copy_vline:=@bgr24_copy_vline;

 pixf.blend_hline:=@bgr24_gamma_blend_hline;
 pixf.blend_vline:=@bgr24_gamma_blend_vline;

 pixf.blend_solid_hspan:=@bgr24_gamma_blend_solid_hspan;
 pixf.blend_solid_vspan:=@bgr24_gamma_blend_solid_vspan;

 pixf.copy_color_hspan:=@bgr24_copy_color_hspan;
 pixf.copy_color_vspan:=@bgr24_copy_color_vspan;
 
 pixf.blend_color_hspan:=@bgr24_gamma_blend_color_hspan;
 pixf.blend_color_vspan:=@bgr24_gamma_blend_color_vspan;

 pixf.copy_from :=@fmt24_copy_from;
 pixf.blend_from:=@bgr24_gamma_blend_from;

 pixf.blend_from_color:=@bgr24_gamma_blend_from_color;
 pixf.blend_from_lut  :=@bgr24_gamma_blend_from_lut;
 
 pixf.for_each_pixel :=@order24_for_each_pixel;
 pixf.gamma_dir_apply:=@order24_gamma_dir_apply;
 pixf.gamma_inv_apply:=@order24_gamma_inv_apply;

end;

{$I pf_rgb24_gamma.inc }

{ PIXFMT_RGB24_GAMMA }
procedure pixfmt_rgb24_gamma;
begin
 pixf.Construct(rb );

 pixf.m_order:=rgb_order;
 pixf.m_gamma:=g;

 pixf.m_pix_width:=3;

 pixf.copy_pixel :=@rgb24_copy_pixel;
 pixf.blend_pixel:=@rgb24_gamma_blend_pixel;

 pixf.pixel:=@rgb24_pixel;
 pixf.row  :=@fmt24_row;

 pixf.copy_hline:=@rgb24_copy_hline;
 pixf.copy_vline:=@rgb24_copy_vline;

 pixf.blend_hline:=@rgb24_gamma_blend_hline;
 pixf.blend_vline:=@rgb24_gamma_blend_vline;

 pixf.blend_solid_hspan:=@rgb24_gamma_blend_solid_hspan;
 pixf.blend_solid_vspan:=@rgb24_gamma_blend_solid_vspan;

 pixf.copy_color_hspan:=@rgb24_copy_color_hspan;
 pixf.copy_color_vspan:=@rgb24_copy_color_vspan;

 pixf.blend_color_hspan:=@rgb24_gamma_blend_color_hspan;
 pixf.blend_color_vspan:=@rgb24_gamma_blend_color_vspan;

 pixf.copy_from :=@fmt24_copy_from;
 pixf.blend_from:=@rgb24_gamma_blend_from;

 pixf.blend_from_color:=@rgb24_gamma_blend_from_color;
 pixf.blend_from_lut  :=@rgb24_gamma_blend_from_lut;

 pixf.for_each_pixel :=@order24_for_each_pixel;
 pixf.gamma_dir_apply:=@order24_gamma_dir_apply;
 pixf.gamma_inv_apply:=@order24_gamma_inv_apply;

end;

END.

