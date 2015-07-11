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
//----------------------------------------------------------------------------
//
// Image transformations with filtering. Span generator base class
//
// [Pascal Port History] -----------------------------------------------------
//
// 01.03.2006-Milano: Unit port establishment
//
{ agg_span_image_filter.pas }
unit
 agg_span_image_filter ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_color ,
 agg_image_filters ,
 agg_rendering_buffer ,
 agg_span_generator ,
 agg_span_allocator ,
 agg_span_interpolator_linear ;

{ TYPES DEFINITION }
type
 span_image_filter_ptr = ^span_image_filter;
 span_image_filter = object(span_generator )
   m_src          : rendering_buffer_ptr;
   m_back_color   : aggclr;
   m_interpolator : span_interpolator_ptr;
   m_filter       : image_filter_lut_ptr;

   m_dx_dbl ,
   m_dy_dbl : double;
   m_dx_int ,
   m_dy_int : unsigned;

   constructor Construct(alloc : span_allocator_ptr ); overload;
   constructor Construct(
                alloc        : span_allocator_ptr;
                src          : rendering_buffer_ptr;
                back_color   : aggclr_ptr;
                interpolator : span_interpolator_ptr;
                filter       : image_filter_lut_ptr ); overload;

   function  _source_image : rendering_buffer_ptr;
   function  _background_color : aggclr_ptr;
   function  _filter : image_filter_lut_ptr;
   function  _interpolator : span_interpolator_ptr;

   function  filter_dx_int : int;
   function  filter_dy_int : int;
   function  filter_dx_dbl : double;
   function  filter_dy_dbl : double;

   procedure source_image_    (v : rendering_buffer_ptr );
   procedure background_color_(v : aggclr_ptr );
   procedure interpolator_    (v : span_interpolator_ptr );
   procedure filter_          (v : image_filter_lut_ptr );
   procedure filter_offset    (dx ,dy : double ); overload;
   procedure filter_offset    (d : double ); overload;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor span_image_filter.Construct(alloc : span_allocator_ptr );
begin
 inherited Construct(alloc );

 m_back_color.Construct;

 m_src         :=NIL;
 m_interpolator:=NIL;
 m_filter      :=NIL;

 m_dx_dbl:=0.0;
 m_dy_dbl:=0.0;
 m_dx_int:=0;
 m_dy_int:=0;

end;

{ CONSTRUCT }
constructor span_image_filter.Construct(
             alloc        : span_allocator_ptr;
             src          : rendering_buffer_ptr;
             back_color   : aggclr_ptr;
             interpolator : span_interpolator_ptr;
             filter       : image_filter_lut_ptr );
begin
 inherited Construct(alloc );

 m_src         :=src;
 m_back_color  :=back_color^;
 m_interpolator:=interpolator;
 m_filter      :=filter;

 m_dx_dbl:=0.5;
 m_dy_dbl:=0.5;
 m_dx_int:=image_subpixel_size div 2;
 m_dy_int:=image_subpixel_size div 2;

end;

{ _SOURCE_IMAGE }
function span_image_filter._source_image;
begin
 result:=m_src;

end;

{ _BACKGROUND_COLOR }
function span_image_filter._background_color;
begin
 result:=@m_back_color;

end;

{ _FILTER }
function span_image_filter._filter;
begin
 result:=m_filter;

end;

{ _INTERPOLATOR }
function span_image_filter._interpolator;
begin
 result:=m_interpolator;

end;

{ FILTER_DX_INT }
function span_image_filter.filter_dx_int;
begin
 result:=m_dx_int;

end;

{ FILTER_DY_INT }
function span_image_filter.filter_dy_int;
begin
 result:=m_dy_int;

end;

{ FILTER_DX_DBL }
function span_image_filter.filter_dx_dbl;
begin
 result:=m_dx_dbl;

end;

{ FILTER_DY_DBL }
function span_image_filter.filter_dy_dbl;
begin
 result:=m_dy_dbl;

end;

{ SOURCE_IMAGE_ }
procedure span_image_filter.source_image_;
begin
 m_src:=v;

end;

{ BACKGROUND_COLOR_ }
procedure span_image_filter.background_color_;
begin
 m_back_color:=v^;

end;

{ INTERPOLATOR_ }
procedure span_image_filter.interpolator_;
begin
 m_interpolator:=v;

end;

{ FILTER_ }
procedure span_image_filter.filter_;
begin
 m_filter:=v;

end;

{ FILTER_OFFSET }
procedure span_image_filter.filter_offset(dx ,dy : double );
begin
 m_dx_dbl:=dx;
 m_dy_dbl:=dy;
 m_dx_int:=trunc(dx * image_subpixel_size );
 m_dy_int:=trunc(dy * image_subpixel_size );

end;

{ FILTER_OFFSET }
procedure span_image_filter.filter_offset(d : double );
begin
 filter_offset(d ,d );

end;

END.

