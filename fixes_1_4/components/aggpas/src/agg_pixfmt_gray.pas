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
// 17.03.2006-Milano: pf_gray8.inc & pf_gray8_pre.inc, finished
// 13.01.2006-Milano: Unit port establishment
//
{ agg_pixfmt_gray.pas }
unit
 agg_pixfmt_gray ;

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
 procedure pixfmt_gray8(var pixf : pixel_formats; rb : rendering_buffer_ptr );

 procedure pixfmt_gray8_bgr24r(var pixf : pixel_formats; rb : rendering_buffer_ptr );
 procedure pixfmt_gray8_bgr24g(var pixf : pixel_formats; rb : rendering_buffer_ptr );
 procedure pixfmt_gray8_bgr24b(var pixf : pixel_formats; rb : rendering_buffer_ptr );

 procedure pixfmt_gray8_pre(var pixf : pixel_formats; rb : rendering_buffer_ptr );

 procedure pixfmt_gray8_pre_bgr24r(var pixf : pixel_formats; rb : rendering_buffer_ptr );
 procedure pixfmt_gray8_pre_bgr24g(var pixf : pixel_formats; rb : rendering_buffer_ptr );
 procedure pixfmt_gray8_pre_bgr24b(var pixf : pixel_formats; rb : rendering_buffer_ptr );


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ fmt8_row }
function fmt8_row(this : pixel_formats_ptr; x ,y : int ) : row_data_type;
begin
 result.Construct(
  x ,this._width - 1 ,
  int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * this.m_step + this.m_offset ) );

end;

{ gray_gamma_dir_apply }
procedure gray_gamma_dir_apply(this : pixel_formats; p : int8u_ptr );
begin
 p^:=this.m_apply.dir(p^ );

end;

{ gray_gamma_inv_apply }
procedure gray_gamma_inv_apply(this : pixel_formats; p : int8u_ptr );
begin
 p^:=this.m_apply.inv(p^ );

end;

{ gray_for_each_pixel }
procedure gray_for_each_pixel(this : pixel_formats_ptr; f : func_apply_gamma );
var
 y ,len : unsigned;

 p : int8u_ptr;

begin
 y:=0;

 while y < this._height do
  begin
   len:=this._width;
   p  :=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + this.m_offset );

   repeat
    f(this ,p );

    inc(ptrcomp(p ) ,this.m_step );
    dec(len );

   until len = 0;

   inc(y );

  end;

end;

{$I pf_gray8.inc }

{ PIXFMT_GRAY8 }
procedure pixfmt_gray8;
begin
 pixf.Construct(rb ,1 ,0 );

 pixf.m_pix_width:=1;

 pixf.copy_pixel :=@gray8_copy_pixel;
 pixf.blend_pixel:=@gray8_blend_pixel;

 pixf.pixel:=@gray8_pixel;
 pixf.row  :=@fmt8_row;

 pixf.copy_hline:=@gray8_copy_hline;
 pixf.copy_vline:=@gray8_copy_vline;

 pixf.blend_hline:=@gray8_blend_hline;
 pixf.blend_vline:=@gray8_blend_vline;

 pixf.blend_solid_hspan:=@gray8_blend_solid_hspan;
 pixf.blend_solid_vspan:=@gray8_blend_solid_vspan;

 pixf.copy_color_hspan:=@gray8_copy_color_hspan;
 pixf.copy_color_vspan:=@gray8_copy_color_vspan;
 
 pixf.blend_color_hspan:=@gray8_blend_color_hspan;
 pixf.blend_color_vspan:=@gray8_blend_color_vspan;

 pixf.copy_from :=@gray8_copy_from;
 pixf.blend_from:=NIL; // not defined in agg_pixfmt_gray.h

 pixf.blend_from_color:=@gray8_blend_from_color;
 pixf.blend_from_lut  :=@gray8_blend_from_lut;
 
 pixf.for_each_pixel :=@gray_for_each_pixel;
 pixf.gamma_dir_apply:=@gray_gamma_dir_apply;
 pixf.gamma_inv_apply:=@gray_gamma_inv_apply;

end;

{ PIXFMT_GRAY8_BGR24R }
procedure pixfmt_gray8_bgr24r;
begin
 pixf.Construct(rb ,3 ,2 );

 pixf.m_pix_width:=3;

 pixf.copy_pixel :=@gray8_copy_pixel;
 pixf.blend_pixel:=@gray8_blend_pixel;

 pixf.pixel:=@gray8_pixel;
 pixf.row  :=@fmt8_row;

 pixf.copy_hline:=@gray8_copy_hline;
 pixf.copy_vline:=@gray8_copy_vline;

 pixf.blend_hline:=@gray8_blend_hline;
 pixf.blend_vline:=@gray8_blend_vline;

 pixf.blend_solid_hspan:=@gray8_blend_solid_hspan;
 pixf.blend_solid_vspan:=@gray8_blend_solid_vspan;

 pixf.copy_color_hspan:=@gray8_copy_color_hspan;
 pixf.copy_color_vspan:=@gray8_copy_color_vspan;
 
 pixf.blend_color_hspan:=@gray8_blend_color_hspan;
 pixf.blend_color_vspan:=@gray8_blend_color_vspan;

 pixf.copy_from :=@gray8_copy_from;
 pixf.blend_from:=NIL; // not defined in agg_pixfmt_gray.h

 pixf.blend_from_color:=@gray8_blend_from_color;
 pixf.blend_from_lut  :=@gray8_blend_from_lut;
 
 pixf.for_each_pixel :=@gray_for_each_pixel;
 pixf.gamma_dir_apply:=@gray_gamma_dir_apply;
 pixf.gamma_inv_apply:=@gray_gamma_inv_apply;

end;

{ PIXFMT_GRAY8_BGR24G }
procedure pixfmt_gray8_bgr24g;
begin
 pixf.Construct(rb ,3 ,1 );

 pixf.m_pix_width:=3;

 pixf.copy_pixel :=@gray8_copy_pixel;
 pixf.blend_pixel:=@gray8_blend_pixel;

 pixf.pixel:=@gray8_pixel;
 pixf.row  :=@fmt8_row;

 pixf.copy_hline:=@gray8_copy_hline;
 pixf.copy_vline:=@gray8_copy_vline;

 pixf.blend_hline:=@gray8_blend_hline;
 pixf.blend_vline:=@gray8_blend_vline;

 pixf.blend_solid_hspan:=@gray8_blend_solid_hspan;
 pixf.blend_solid_vspan:=@gray8_blend_solid_vspan;

 pixf.copy_color_hspan:=@gray8_copy_color_hspan;
 pixf.copy_color_vspan:=@gray8_copy_color_vspan;
 
 pixf.blend_color_hspan:=@gray8_blend_color_hspan;
 pixf.blend_color_vspan:=@gray8_blend_color_vspan;

 pixf.copy_from :=@gray8_copy_from;
 pixf.blend_from:=NIL; // not defined in agg_pixfmt_gray.h

 pixf.blend_from_color:=@gray8_blend_from_color;
 pixf.blend_from_lut  :=@gray8_blend_from_lut;
 
 pixf.for_each_pixel :=@gray_for_each_pixel;
 pixf.gamma_dir_apply:=@gray_gamma_dir_apply;
 pixf.gamma_inv_apply:=@gray_gamma_inv_apply;

end;

{ PIXFMT_GRAY8_BGR24B }
procedure pixfmt_gray8_bgr24b;
begin
 pixf.Construct(rb ,3 ,0 );

 pixf.m_pix_width:=3;

 pixf.copy_pixel :=@gray8_copy_pixel;
 pixf.blend_pixel:=@gray8_blend_pixel;

 pixf.pixel:=@gray8_pixel;
 pixf.row  :=@fmt8_row;

 pixf.copy_hline:=@gray8_copy_hline;
 pixf.copy_vline:=@gray8_copy_vline;

 pixf.blend_hline:=@gray8_blend_hline;
 pixf.blend_vline:=@gray8_blend_vline;

 pixf.blend_solid_hspan:=@gray8_blend_solid_hspan;
 pixf.blend_solid_vspan:=@gray8_blend_solid_vspan;

 pixf.copy_color_hspan:=@gray8_copy_color_hspan;
 pixf.copy_color_vspan:=@gray8_copy_color_vspan;
 
 pixf.blend_color_hspan:=@gray8_blend_color_hspan;
 pixf.blend_color_vspan:=@gray8_blend_color_vspan;

 pixf.copy_from :=@gray8_copy_from;
 pixf.blend_from:=NIL; // not defined in agg_pixfmt_gray.h

 pixf.blend_from_color:=@gray8_blend_from_color;
 pixf.blend_from_lut  :=@gray8_blend_from_lut;
 
 pixf.for_each_pixel :=@gray_for_each_pixel;
 pixf.gamma_dir_apply:=@gray_gamma_dir_apply;
 pixf.gamma_inv_apply:=@gray_gamma_inv_apply;

end;

{$I pf_gray8_pre.inc }

{ PIXFMT_GRAY8_PRE }
procedure pixfmt_gray8_pre;
begin
 pixf.Construct(rb ,1 ,0 );

 pixf.m_pix_width:=1;

 pixf.copy_pixel :=@gray8_copy_pixel;
 pixf.blend_pixel:=@gray8_pre_blend_pixel;

 pixf.pixel:=@gray8_pixel;
 pixf.row  :=@fmt8_row;

 pixf.copy_hline:=@gray8_copy_hline;
 pixf.copy_vline:=@gray8_copy_vline;

 pixf.blend_hline:=@gray8_pre_blend_hline;
 pixf.blend_vline:=@gray8_pre_blend_vline;

 pixf.blend_solid_hspan:=@gray8_pre_blend_solid_hspan;
 pixf.blend_solid_vspan:=@gray8_pre_blend_solid_vspan;

 pixf.copy_color_hspan:=@gray8_copy_color_hspan;
 pixf.copy_color_vspan:=@gray8_copy_color_vspan;
 
 pixf.blend_color_hspan:=@gray8_pre_blend_color_hspan;
 pixf.blend_color_vspan:=@gray8_pre_blend_color_vspan;

 pixf.copy_from :=@gray8_copy_from;
 pixf.blend_from:=NIL; // not defined in agg_pixfmt_gray.h

 pixf.blend_from_color:=@gray8_pre_blend_from_color;
 pixf.blend_from_lut  :=@gray8_pre_blend_from_lut;
 
 pixf.for_each_pixel :=@gray_for_each_pixel;
 pixf.gamma_dir_apply:=@gray_gamma_dir_apply;
 pixf.gamma_inv_apply:=@gray_gamma_inv_apply;

end;

{ PIXFMT_GRAY8_PRE_BGR24R }
procedure pixfmt_gray8_pre_bgr24r;
begin
 pixf.Construct(rb ,3 ,2 );

 pixf.m_pix_width:=3;

 pixf.copy_pixel :=@gray8_copy_pixel;
 pixf.blend_pixel:=@gray8_pre_blend_pixel;

 pixf.pixel:=@gray8_pixel;
 pixf.row  :=@fmt8_row;

 pixf.copy_hline:=@gray8_copy_hline;
 pixf.copy_vline:=@gray8_copy_vline;

 pixf.blend_hline:=@gray8_pre_blend_hline;
 pixf.blend_vline:=@gray8_pre_blend_vline;

 pixf.blend_solid_hspan:=@gray8_pre_blend_solid_hspan;
 pixf.blend_solid_vspan:=@gray8_pre_blend_solid_vspan;

 pixf.copy_color_hspan:=@gray8_copy_color_hspan;
 pixf.copy_color_vspan:=@gray8_copy_color_vspan;
 
 pixf.blend_color_hspan:=@gray8_pre_blend_color_hspan;
 pixf.blend_color_vspan:=@gray8_pre_blend_color_vspan;

 pixf.copy_from :=@gray8_copy_from;
 pixf.blend_from:=NIL; // not defined in agg_pixfmt_gray.h

 pixf.blend_from_color:=@gray8_pre_blend_from_color;
 pixf.blend_from_lut  :=@gray8_pre_blend_from_lut;
 
 pixf.for_each_pixel :=@gray_for_each_pixel;
 pixf.gamma_dir_apply:=@gray_gamma_dir_apply;
 pixf.gamma_inv_apply:=@gray_gamma_inv_apply;

end;

{ PIXFMT_GRAY8_PRE_BGR24G }
procedure pixfmt_gray8_pre_bgr24g;
begin
 pixf.Construct(rb ,3 ,1 );

 pixf.m_pix_width:=3;

 pixf.copy_pixel :=@gray8_copy_pixel;
 pixf.blend_pixel:=@gray8_pre_blend_pixel;

 pixf.pixel:=@gray8_pixel;
 pixf.row  :=@fmt8_row;

 pixf.copy_hline:=@gray8_copy_hline;
 pixf.copy_vline:=@gray8_copy_vline;

 pixf.blend_hline:=@gray8_pre_blend_hline;
 pixf.blend_vline:=@gray8_pre_blend_vline;

 pixf.blend_solid_hspan:=@gray8_pre_blend_solid_hspan;
 pixf.blend_solid_vspan:=@gray8_pre_blend_solid_vspan;

 pixf.copy_color_hspan:=@gray8_copy_color_hspan;
 pixf.copy_color_vspan:=@gray8_copy_color_vspan;
 
 pixf.blend_color_hspan:=@gray8_pre_blend_color_hspan;
 pixf.blend_color_vspan:=@gray8_pre_blend_color_vspan;

 pixf.copy_from :=@gray8_copy_from;
 pixf.blend_from:=NIL; // not defined in agg_pixfmt_gray.h

 pixf.blend_from_color:=@gray8_pre_blend_from_color;
 pixf.blend_from_lut  :=@gray8_pre_blend_from_lut;
 
 pixf.for_each_pixel :=@gray_for_each_pixel;
 pixf.gamma_dir_apply:=@gray_gamma_dir_apply;
 pixf.gamma_inv_apply:=@gray_gamma_inv_apply;

end;

{ PIXFMT_GRAY8_PRE_BGR24B }
procedure pixfmt_gray8_pre_bgr24b;
begin
 pixf.Construct(rb ,3 ,0 );

 pixf.m_pix_width:=3;

 pixf.copy_pixel :=@gray8_copy_pixel;
 pixf.blend_pixel:=@gray8_pre_blend_pixel;

 pixf.pixel:=@gray8_pixel;
 pixf.row  :=@fmt8_row;

 pixf.copy_hline:=@gray8_copy_hline;
 pixf.copy_vline:=@gray8_copy_vline;

 pixf.blend_hline:=@gray8_pre_blend_hline;
 pixf.blend_vline:=@gray8_pre_blend_vline;

 pixf.blend_solid_hspan:=@gray8_pre_blend_solid_hspan;
 pixf.blend_solid_vspan:=@gray8_pre_blend_solid_vspan;

 pixf.copy_color_hspan:=@gray8_copy_color_hspan;
 pixf.copy_color_vspan:=@gray8_copy_color_vspan;

 pixf.blend_color_hspan:=@gray8_pre_blend_color_hspan;
 pixf.blend_color_vspan:=@gray8_pre_blend_color_vspan;

 pixf.copy_from :=@gray8_copy_from;
 pixf.blend_from:=NIL; // not defined in agg_pixfmt_gray.h

 pixf.blend_from_color:=@gray8_pre_blend_from_color;
 pixf.blend_from_lut  :=@gray8_pre_blend_from_lut;
 
 pixf.for_each_pixel :=@gray_for_each_pixel;
 pixf.gamma_dir_apply:=@gray_gamma_dir_apply;
 pixf.gamma_inv_apply:=@gray_gamma_inv_apply;

end;

END.

