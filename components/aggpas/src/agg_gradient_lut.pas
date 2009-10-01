//----------------------------------------------------------------------------
// Anti-Grain Geometry - Version 2.4 (Public License)
// Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)
//
// Anti-Grain Geometry - Version 2.4 Release Milano 3 (AggPas 2.4 RM3)
// Pascal Port By: Milan Marusinec alias Milano
//                 milan@marusinec.sk
//                 http://www.aggpas.org
// Copyright (c) 2005-2007
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
// 12.10.2007-Milano: Unit port establishment
//
{ agg_gradient_lut.pas }
unit
 agg_gradient_lut ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_array ,
 agg_dda_line ,
 agg_color ;

{ TYPES DEFINITION }
type
 gradient_lut_ptr = ^gradient_lut;
 gradient_lut = object(array_base )
  private
   m_color_lut_size : unsigned;
   m_color_profile  : pod_bvector;
   m_color_lut      : pod_array;

  public
   constructor Construct(size_ : unsigned = 256 );
   destructor  Destruct;

  // Build Gradient Lut
  // First, call remove_all(), then add_color() at least twice,
  // then build_lut(). Argument "offset" in add_color must be
  // in range [0...1] and defines a color stop as it is described
  // in SVG specification, section Gradients and Patterns.
  // The simplest linear gradient is:
  //    gradient_lut.add_color(0.0, start_color);
  //    gradient_lut.add_color(1.0, end_color);
   procedure remove_all;
   procedure add_color(offset : double; color : aggclr_ptr );
   procedure build_lut;

  // Size-index Interface. This class can be used directly as the
  // ColorF in span_gradient. All it needs is two access methods
  // size() and operator [].
   function  size : unsigned; virtual;
   function  entry : unsigned; virtual;
   function  array_operator(i : unsigned ) : pointer; virtual;

  end;

{ GLOBAL VARIABLES & CONSTANTS }
{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
type
 color_point_ptr = ^color_point;
 color_point = object
   offset : double;
   color  : aggclr;

   constructor Construct; overload;
   constructor Construct(off : double; c : aggclr_ptr ); overload;

  end;

 color_interpolator = object
   m_c1 ,
   m_c2 : aggclr;

   m_len   ,
   m_count : unsigned;

   v ,r ,g ,b ,a : dda_line_interpolator;

   m_is_gray : boolean;

   constructor Construct(c1 ,c2 : aggclr_ptr; len : unsigned; is_gray : boolean = false );

   procedure operator_inc;

   function  color : aggclr;

  end;

{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor color_point.Construct;
begin
 offset:=0;

 color.Construct;

end;

{ CONSTRUCT }
constructor color_point.Construct(off : double; c : aggclr_ptr );
begin
 offset:=off;

 color.Construct(c );

 if offset < 0.0 then
  offset:=0.0;

 if offset > 1.0 then
  offset:=1.0;

end;

{ CONSTRUCT }
constructor color_interpolator.Construct(c1 ,c2 : aggclr_ptr; len : unsigned; is_gray : boolean = false );
begin
 m_c1.Construct(c1 );
 m_c2.Construct(c2 );

 m_len  :=len;
 m_count:=0;

 m_is_gray:=is_gray;

 if m_is_gray then
  v.Construct(c1.v ,c2.v ,len ,14 )

 else
  begin
   r.Construct(c1.r ,c2.r ,len ,14 );
   g.Construct(c1.g ,c2.g ,len ,14 );
   b.Construct(c1.b ,c2.b ,len ,14 );

  end;

 a.Construct(c1.a ,c2.a ,len ,14 );

end;

{ OPERATOR_INC }
procedure color_interpolator.operator_inc;
begin
 inc(m_count );

 if m_is_gray then
  v.plus_operator

 else
  begin
   r.plus_operator;
   g.plus_operator;
   b.plus_operator;

  end;

 a.plus_operator;

end;

{ COLOR }
function color_interpolator.color : aggclr;
begin
// result:=m_c1.gradient(@m_c2 ,m_count / m_len );

 if m_is_gray then
  result.ConstrInt(r._y ,a._y )
 else
  result.ConstrInt(r._y ,g._y ,b._y ,a._y )

end;

{ CONSTRUCT }
constructor gradient_lut.Construct(size_ : unsigned = 256 );
begin
 m_color_lut_size:=size_;

 m_color_profile.Construct(sizeof(color_point ) ,4 );
 m_color_lut.Construct(sizeof(aggclr ) ,m_color_lut_size );

end;

{ DESTRUCT }
destructor gradient_lut.Destruct;
begin
 m_color_profile.Destruct;
 m_color_lut.Destruct;

end;

{ REMOVE_ALL }
procedure gradient_lut.remove_all;
begin
 m_color_profile.remove_all;

end;

{ ADD_COLOR }
procedure gradient_lut.add_color(offset : double; color : aggclr_ptr );
var
 cp : color_point;

begin
 cp.Construct(offset ,color );

 m_color_profile.add(@cp );

end;

{ offset_less }
function offset_less(a ,b : color_point_ptr ) : boolean;
begin
 result:=a.offset < b.offset;

end;

{ offset_equal }
function offset_equal(a ,b : color_point_ptr ) : boolean;
begin
 result:=a.offset = b.offset;

end;

{ BUILD_LUT }
procedure gradient_lut.build_lut;
var
 i ,start ,end_ : unsigned;

 c : aggclr;

 ci : color_interpolator;

begin
 quick_sort            (@m_color_profile ,@offset_less );
 m_color_profile.cut_at(remove_duplicates(@m_color_profile ,@offset_equal ) );

 if m_color_profile.size >= 2 then
  begin
   start:=
    uround(
     color_point_ptr(m_color_profile.array_operator(0 ) ).offset * m_color_lut_size );

   c:=color_point_ptr(m_color_profile.array_operator(0 ) ).color;
   i:=0;

   while i < start do
    begin
     aggclr_ptr(m_color_lut.array_operator(i ) )^:=c;

     inc(i );

    end;

   i:=1;

   while i < m_color_profile.size do
    begin
     end_:=uround(color_point_ptr(m_color_profile.array_operator(i ) ).offset * m_color_lut_size );

     ci.Construct(
      @color_point_ptr(m_color_profile.array_operator(i - 1 ) ).color ,
      @color_point_ptr(m_color_profile.array_operator(i ) ).color ,
      end_ - start + 1 );

     while start < end_ do
      begin
       aggclr_ptr(m_color_lut.array_operator(start ) )^:=ci.color;

       ci.operator_inc;

       inc(start );

      end;

     inc(i );

    end;

   c:=color_point_ptr(m_color_profile.last ).color;

   while end_ < m_color_lut.size do
    begin
     aggclr_ptr(m_color_lut.array_operator(end_ ) )^:=c;

     inc(end_ );

    end;

  end;

end;

{ SIZE }
function gradient_lut.size : unsigned;
begin
 result:=m_color_lut_size;

end;

{ ENTRY }
function gradient_lut.entry : unsigned;
begin
 result:=m_color_lut.m_entry_sz;

end;

{ ARRAY_OPERATOR }
function gradient_lut.array_operator(i : unsigned ) : pointer;
begin
 result:=m_color_lut.array_operator(i );

end;

END.

