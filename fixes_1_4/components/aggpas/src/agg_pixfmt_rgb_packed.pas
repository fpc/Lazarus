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
{ agg_pixfmt_rgb_packed.pas }
unit
 agg_pixfmt_rgb_packed ;

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
 procedure pixfmt_rgb555(var pixf : pixel_formats; rb : rendering_buffer_ptr );
 procedure pixfmt_rgb565(var pixf : pixel_formats; rb : rendering_buffer_ptr );

 procedure pixfmt_rgb555_pre(var pixf : pixel_formats; rb : rendering_buffer_ptr );
 procedure pixfmt_rgb565_pre(var pixf : pixel_formats; rb : rendering_buffer_ptr );

 procedure pixfmt_rgb555_gamma(var pixf : pixel_formats; rb : rendering_buffer_ptr; g : gamma_ptr );
 procedure pixfmt_rgb565_gamma(var pixf : pixel_formats; rb : rendering_buffer_ptr; g : gamma_ptr );


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ fmt5x5_row }
function fmt5x5_row(this : pixel_formats_ptr; x ,y : int ) : row_data_type;
begin
 result.Construct(
  x ,this._width - 1 ,
  int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * sizeof(int16u ) ) );

end;

{$I pf_rgb555.inc }

{ PIXFMT_RGB555 }
procedure pixfmt_rgb555;
begin
 pixf.Construct(rb );

 pixf.m_pix_width:=2;

 pixf.copy_pixel :=@rgb555_copy_pixel;
 pixf.blend_pixel:=@rgb555_blend_pixel;

 pixf.pixel:=@rgb555_pixel;
 pixf.row  :=@fmt5x5_row;

 pixf.copy_hline:=@rgb555_copy_hline;
 pixf.copy_vline:=@rgb555_copy_vline;

 pixf.blend_hline:=@rgb555_blend_hline;
 pixf.blend_vline:=@rgb555_blend_vline;

 pixf.blend_solid_hspan:=@rgb555_blend_solid_hspan;
 pixf.blend_solid_vspan:=@rgb555_blend_solid_vspan;

 pixf.copy_color_hspan:=@rgb555_copy_color_hspan;
 pixf.copy_color_vspan:=@rgb555_copy_color_vspan;
 
 pixf.blend_color_hspan:=@rgb555_blend_color_hspan;
 pixf.blend_color_vspan:=@rgb555_blend_color_vspan;

 pixf.copy_from :=@rgb555_copy_from;
 pixf.blend_from:=@rgb555_blend_from;

 pixf.blend_from_color:=@rgb555_blend_from_color;
 pixf.blend_from_lut  :=@rgb555_blend_from_lut;
 
 pixf.for_each_pixel :=NIL; // not implemented in agg_pixfmt_rgb_packed.h
 pixf.gamma_dir_apply:=NIL;
 pixf.gamma_inv_apply:=NIL;

end;

{$I pf_rgb565.inc }

{ PIXFMT_RGB565 }
procedure pixfmt_rgb565;
begin
 pixf.Construct(rb );

 pixf.m_pix_width:=2;

 pixf.copy_pixel :=@rgb565_copy_pixel;
 pixf.blend_pixel:=@rgb565_blend_pixel;

 pixf.pixel:=@rgb565_pixel;
 pixf.row  :=@fmt5x5_row;

 pixf.copy_hline:=@rgb565_copy_hline;
 pixf.copy_vline:=@rgb565_copy_vline;

 pixf.blend_hline:=@rgb565_blend_hline;
 pixf.blend_vline:=@rgb565_blend_vline;

 pixf.blend_solid_hspan:=@rgb565_blend_solid_hspan;
 pixf.blend_solid_vspan:=@rgb565_blend_solid_vspan;

 pixf.copy_color_hspan:=@rgb565_copy_color_hspan;
 pixf.copy_color_vspan:=@rgb565_copy_color_vspan;
 
 pixf.blend_color_hspan:=@rgb565_blend_color_hspan;
 pixf.blend_color_vspan:=@rgb565_blend_color_vspan;

 pixf.copy_from :=@rgb565_copy_from;
 pixf.blend_from:=@rgb565_blend_from;

 pixf.blend_from_color:=@rgb565_blend_from_color;
 pixf.blend_from_lut  :=@rgb565_blend_from_lut;
 
 pixf.for_each_pixel :=NIL; // not implemented in agg_pixfmt_rgb_packed.h
 pixf.gamma_dir_apply:=NIL;
 pixf.gamma_inv_apply:=NIL;

end;

{$I pf_rgb555_pre.inc }

{ PIXFMT_RGB555_PRE }
procedure pixfmt_rgb555_pre;
begin
 pixf.Construct(rb );

 pixf.m_pix_width:=2;

 pixf.copy_pixel :=@rgb555_pre_copy_pixel;
 pixf.blend_pixel:=@rgb555_pre_blend_pixel;

 pixf.pixel:=@rgb555_pre_pixel;
 pixf.row  :=@fmt5x5_row;

 pixf.copy_hline:=@rgb555_pre_copy_hline;
 pixf.copy_vline:=@rgb555_pre_copy_vline;

 pixf.blend_hline:=@rgb555_pre_blend_hline;
 pixf.blend_vline:=@rgb555_pre_blend_vline;

 pixf.blend_solid_hspan:=@rgb555_pre_blend_solid_hspan;
 pixf.blend_solid_vspan:=@rgb555_pre_blend_solid_vspan;

 pixf.copy_color_hspan:=@rgb555_pre_copy_color_hspan;
 pixf.copy_color_vspan:=@rgb555_pre_copy_color_vspan;
 
 pixf.blend_color_hspan:=@rgb555_pre_blend_color_hspan;
 pixf.blend_color_vspan:=@rgb555_pre_blend_color_vspan;

 pixf.copy_from :=@rgb555_pre_copy_from;
 pixf.blend_from:=@rgb555_pre_blend_from;

 pixf.blend_from_color:=@rgb555_pre_blend_from_color;
 pixf.blend_from_lut  :=@rgb555_pre_blend_from_lut;
 
 pixf.for_each_pixel :=NIL; // not implemented in agg_pixfmt_rgb_packed.h
 pixf.gamma_dir_apply:=NIL;
 pixf.gamma_inv_apply:=NIL;

end;

{$I pf_rgb565_pre.inc }

{ PIXFMT_RGB565 }
procedure pixfmt_rgb565_pre;
begin
 pixf.Construct(rb );

 pixf.m_pix_width:=2;

 pixf.copy_pixel :=@rgb565_pre_copy_pixel;
 pixf.blend_pixel:=@rgb565_pre_blend_pixel;

 pixf.pixel:=@rgb565_pre_pixel;
 pixf.row  :=@fmt5x5_row;

 pixf.copy_hline:=@rgb565_pre_copy_hline;
 pixf.copy_vline:=@rgb565_pre_copy_vline;

 pixf.blend_hline:=@rgb565_pre_blend_hline;
 pixf.blend_vline:=@rgb565_pre_blend_vline;

 pixf.blend_solid_hspan:=@rgb565_pre_blend_solid_hspan;
 pixf.blend_solid_vspan:=@rgb565_pre_blend_solid_vspan;

 pixf.copy_color_hspan:=@rgb565_pre_copy_color_hspan;
 pixf.copy_color_vspan:=@rgb565_pre_copy_color_vspan;
 
 pixf.blend_color_hspan:=@rgb565_pre_blend_color_hspan;
 pixf.blend_color_vspan:=@rgb565_pre_blend_color_vspan;

 pixf.copy_from :=@rgb565_pre_copy_from;
 pixf.blend_from:=@rgb565_pre_blend_from;

 pixf.blend_from_color:=@rgb565_pre_blend_from_color;
 pixf.blend_from_lut  :=@rgb565_pre_blend_from_lut;
 
 pixf.for_each_pixel :=NIL; // not implemented in agg_pixfmt_rgb_packed.h
 pixf.gamma_dir_apply:=NIL;
 pixf.gamma_inv_apply:=NIL;

end;

{$I pf_rgb555_gamma.inc }

{ PIXFMT_RGB555_GAMMA }
procedure pixfmt_rgb555_gamma;
begin
 pixf.Construct(rb );

 pixf.m_pix_width:=2;

 pixf.m_gamma:=g;

 pixf.copy_pixel :=@rgb555_gamma_copy_pixel;
 pixf.blend_pixel:=@rgb555_gamma_blend_pixel;

 pixf.pixel:=@rgb555_gamma_pixel;
 pixf.row  :=@fmt5x5_row;

 pixf.copy_hline:=@rgb555_gamma_copy_hline;
 pixf.copy_vline:=@rgb555_gamma_copy_vline;

 pixf.blend_hline:=@rgb555_gamma_blend_hline;
 pixf.blend_vline:=@rgb555_gamma_blend_vline;

 pixf.blend_solid_hspan:=@rgb555_gamma_blend_solid_hspan;
 pixf.blend_solid_vspan:=@rgb555_gamma_blend_solid_vspan;

 pixf.copy_color_hspan:=@rgb555_gamma_copy_color_hspan;
 pixf.copy_color_vspan:=@rgb555_gamma_copy_color_vspan;
 
 pixf.blend_color_hspan:=@rgb555_gamma_blend_color_hspan;
 pixf.blend_color_vspan:=@rgb555_gamma_blend_color_vspan;

 pixf.copy_from :=@rgb555_gamma_copy_from;
 pixf.blend_from:=@rgb555_gamma_blend_from;

 pixf.blend_from_color:=@rgb555_gamma_blend_from_color;
 pixf.blend_from_lut  :=@rgb555_gamma_blend_from_lut;
 
 pixf.for_each_pixel :=NIL; // not implemented in agg_pixfmt_rgb_packed.h
 pixf.gamma_dir_apply:=NIL;
 pixf.gamma_inv_apply:=NIL;

end;

{$I pf_rgb565_gamma.inc }

{ PIXFMT_RGB565 }
procedure pixfmt_rgb565_gamma;
begin
 pixf.Construct(rb );

 pixf.m_pix_width:=2;

 pixf.m_gamma:=g;

 pixf.copy_pixel :=@rgb565_gamma_copy_pixel;
 pixf.blend_pixel:=@rgb565_gamma_blend_pixel;

 pixf.pixel:=@rgb565_gamma_pixel;
 pixf.row  :=@fmt5x5_row;

 pixf.copy_hline:=@rgb565_gamma_copy_hline;
 pixf.copy_vline:=@rgb565_gamma_copy_vline;

 pixf.blend_hline:=@rgb565_gamma_blend_hline;
 pixf.blend_vline:=@rgb565_gamma_blend_vline;

 pixf.blend_solid_hspan:=@rgb565_gamma_blend_solid_hspan;
 pixf.blend_solid_vspan:=@rgb565_gamma_blend_solid_vspan;

 pixf.copy_color_hspan:=@rgb565_gamma_copy_color_hspan;
 pixf.copy_color_vspan:=@rgb565_gamma_copy_color_vspan;

 pixf.blend_color_hspan:=@rgb565_gamma_blend_color_hspan;
 pixf.blend_color_vspan:=@rgb565_gamma_blend_color_vspan;

 pixf.copy_from :=@rgb565_gamma_copy_from;
 pixf.blend_from:=@rgb565_gamma_blend_from;

 pixf.blend_from_color:=@rgb565_gamma_blend_from_color;
 pixf.blend_from_lut  :=@rgb565_gamma_blend_from_lut;
 
 pixf.for_each_pixel :=NIL; // not implemented in agg_pixfmt_rgb_packed.h
 pixf.gamma_dir_apply:=NIL;
 pixf.gamma_inv_apply:=NIL;

end;

END.

