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
// 10.02.2006-Milano: Unit port establishment
//
{ agg_pixfmt_amask_adaptor.pas }
unit
 agg_pixfmt_amask_adaptor ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_color ,
 agg_rendering_buffer ,
 agg_pixfmt ,
 agg_alpha_mask_u8 ;

{ TYPES DEFINITION }
const
 span_extra_tail = 256;

type
 pixfmt_amask_adaptor_ptr = ^pixfmt_amask_adaptor;
 pixfmt_amask_adaptor = object(pixel_formats )
   m_pixf : pixel_formats_ptr;
   m_mask : alpha_mask_ptr;

   m_span    : int8u_ptr;
   m_max_len : unsigned;

   constructor Construct(pixf : pixel_formats_ptr; mask : alpha_mask_ptr );
   destructor  Destruct;

   procedure realloc_span(len : unsigned );

   procedure init_span(len : unsigned ); overload;
   procedure init_span(len : unsigned; covers : int8u_ptr ); overload;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ _copy_hline }
procedure _copy_hline(this : pixfmt_amask_adaptor_ptr; x ,y : int; len : unsigned; c : aggclr_ptr );
begin
 this.realloc_span            (len );
 this.m_mask.fill_hspan       (x ,y ,this.m_span ,len );
 this.m_pixf.blend_solid_hspan(this.m_pixf ,x ,y ,len ,c ,this.m_span );

end;

{ _blend_hline }
procedure _blend_hline(this : pixfmt_amask_adaptor_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
begin
 this.init_span               (len );
 this.m_mask.combine_hspan    (x ,y ,this.m_span ,len );
 this.m_pixf.blend_solid_hspan(this.m_pixf ,x ,y ,len ,c ,this.m_span );

end;

{ _blend_vline }
procedure _blend_vline(this : pixfmt_amask_adaptor_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
begin
 this.init_span               (len);
 this.m_mask.combine_vspan    (x ,y ,this.m_span ,len );
 this.m_pixf.blend_solid_vspan(this.m_pixf ,x ,y ,len ,c ,this.m_span );

end;

{ _blend_solid_hspan }
procedure _blend_solid_hspan(this : pixfmt_amask_adaptor_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
begin
 this.init_span               (len ,covers );
 this.m_mask.combine_hspan    (x ,y ,this.m_span ,len );
 this.m_pixf.blend_solid_hspan(this.m_pixf ,x ,y ,len ,c ,this.m_span );

end;

{ _blend_solid_vspan }
procedure _blend_solid_vspan(this : pixfmt_amask_adaptor_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
begin
 this.init_span               (len ,covers );
 this.m_mask.combine_vspan    (x ,y ,this.m_span ,len );
 this.m_pixf.blend_solid_vspan(this.m_pixf ,x ,y ,len ,c ,this.m_span );

end;

{ _blend_color_hspan }
procedure _blend_color_hspan(this : pixfmt_amask_adaptor_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
begin
 if covers <> NIL then
  begin
   this.init_span           (len ,covers );
   this.m_mask.combine_hspan(x ,y ,this.m_span ,len );

  end
 else
  begin
   this.realloc_span     (len );
   this.m_mask.fill_hspan(x ,y ,this.m_span ,len );

  end;

 this.m_pixf.blend_color_hspan(this.m_pixf ,x ,y ,len ,colors ,this.m_span ,cover );

end;

{ _blend_color_vspan }
procedure _blend_color_vspan(this : pixfmt_amask_adaptor_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
begin
 if covers <> NIL then
  begin
   this.init_span           (len ,covers );
   this.m_mask.combine_vspan(x ,y ,this.m_span ,len );

  end
 else
  begin
   this.realloc_span     (len );
   this.m_mask.fill_vspan(x ,y ,this.m_span ,len );

  end;

 this.m_pixf.blend_color_vspan(this.m_pixf ,x ,y ,len ,colors ,this.m_span ,cover );

end;

{ _blend_pixel }
procedure _blend_pixel(this : pixfmt_amask_adaptor_ptr; x ,y : int; c : pointer; cover : int8u );
begin
 this.m_pixf.blend_pixel(this.m_pixf ,x ,y ,c ,this.m_mask.combine_pixel(x ,y ,cover ) );

end;

{ CONSTRUCT }
constructor pixfmt_amask_adaptor.Construct;
begin
 inherited Construct(pixf.m_rbuf );

 m_pixf:=pixf;
 m_mask:=mask;

 m_span   :=NIL;
 m_max_len:=0;

 copy_hline :=@_copy_hline;
 blend_hline:=@_blend_hline;
 blend_vline:=@_blend_vline;

 blend_solid_hspan:=@_blend_solid_hspan;
 blend_solid_vspan:=@_blend_solid_vspan;
 blend_color_hspan:=@_blend_color_hspan;
 blend_color_vspan:=@_blend_color_vspan;

 blend_pixel:=@_blend_pixel;

end;

{ DESTRUCT }
destructor pixfmt_amask_adaptor.Destruct;
begin
 agg_freemem(pointer(m_span ) ,m_max_len * sizeof(int8u ) );

end;

{ REALLOC_SPAN }
procedure pixfmt_amask_adaptor.realloc_span;
begin
 if len > m_max_len then
  begin
   agg_freemem(pointer(m_span ) ,m_max_len * sizeof(int8u ) );

   m_max_len:=len + span_extra_tail;

   agg_getmem(pointer(m_span ) ,m_max_len * sizeof(int8u ) );

  end;

end;

{ INIT_SPAN }
procedure pixfmt_amask_adaptor.init_span(len : unsigned );
begin
 realloc_span(len );

 fillchar(m_span^ ,len * sizeof(int8u ) ,cover_full );

end;

{ INIT_SPAN }
procedure pixfmt_amask_adaptor.init_span(len : unsigned; covers : int8u_ptr );
begin
 realloc_span(len );

 move(covers^ ,m_span^ ,len * sizeof(int8u ) );

end;

END.

