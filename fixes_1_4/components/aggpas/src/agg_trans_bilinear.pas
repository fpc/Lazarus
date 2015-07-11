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
// Bilinear 2D transformations
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 07.02.2006-Milano: Unit port establishment
//
{ agg_trans_bilinear.pas }
unit
 agg_trans_bilinear ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_trans_affine ,
 agg_simul_eq ;

{ TYPES DEFINITION }
type
 iterator_x = object
   inc_x ,inc_y ,x ,y : double;

   constructor Construct; overload;
   constructor Construct(tx ,ty ,step : double; m : double_42_ptr ); overload;

   procedure inc_operator;

  end;

 trans_bilinear_ptr = ^trans_bilinear;
 trans_bilinear = object(trans_affine )
   m_valid : boolean;
   m_mtx   : array[0..3 ,0..1 ] of double;

   constructor Construct; overload;

  // Arbitrary quadrangle transformations
   constructor Construct(src ,dst : double_ptr ); overload;

  // Direct transformations
   constructor Construct(x1 ,y1 ,x2 ,y2 : double; quad : double_ptr ); overload;

  // Reverse transformations
   constructor Construct(quad : double_ptr; x1 ,y1 ,x2 ,y2 : double ); overload;

  // Set the transformations using two arbitrary quadrangles.
   procedure quad_to_quad(src ,dst : double_ptr );

  // Set the direct transformations, i.e., rectangle -> quadrangle
   procedure rect_to_quad(x1 ,y1 ,x2 ,y2 : double; quad : double_ptr );

  // Set the reverse transformations, i.e., quadrangle -> rectangle
   procedure quad_to_rect(quad : double_ptr; x1 ,y1 ,x2 ,y2 : double );

  // Check if the equations were solved successfully
   function  is_valid : boolean;

   function  begin_(x ,y ,step : double ) : iterator_x;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor iterator_x.Construct;
begin
end;

{ CONSTRUCT }
constructor iterator_x.Construct(tx ,ty ,step : double; m : double_42_ptr );
begin
 inc_x:=m^[1 ,0 ] * step * ty + m^[2 ,0 ] * step;
 inc_y:=m^[1 ,1 ] * step * ty + m^[2 ,1 ] * step;

 x:=m^[0 ,0 ] + m^[1 ,0 ] * tx * ty + m^[2 ,0 ] * tx + m^[3 ,0 ] * ty;
 y:=m^[0 ,1 ] + m^[1 ,1 ] * tx * ty + m^[2 ,1 ] * tx + m^[3 ,1 ] * ty;

end;

{ INC_OPERATOR }
procedure iterator_x.inc_operator;
begin
 x:=x + inc_x;
 y:=y + inc_y;

end;

{ _transform }
procedure _transform(this : trans_bilinear_ptr; x ,y : double_ptr );
var
 tx ,ty ,xy : double;

begin
 tx:=x^;
 ty:=y^;
 xy:=tx * ty;

 x^:=this.m_mtx[0 ,0 ] + this.m_mtx[1 ,0 ] * xy + this.m_mtx[2 ,0 ] * tx + this.m_mtx[3 ,0 ] * ty;
 y^:=this.m_mtx[0 ,1 ] + this.m_mtx[1 ,1 ] * xy + this.m_mtx[2 ,1 ] * tx + this.m_mtx[3 ,1 ] * ty;

end;

{ CONSTRUCT }
constructor trans_bilinear.Construct;
begin
 inherited Construct;

 m_valid:=false;

 transform:=@_transform;

end;

{ CONSTRUCT }
constructor trans_bilinear.Construct(src ,dst : double_ptr );
begin
 inherited Construct;

 quad_to_quad(src ,dst );

 transform:=@_transform;

end;

{ CONSTRUCT }
constructor trans_bilinear.Construct(x1 ,y1 ,x2 ,y2 : double; quad : double_ptr );
begin
 inherited Construct;

 rect_to_quad(x1 ,y1 ,x2 ,y2 ,quad );

 transform:=@_transform;

end;

{ CONSTRUCT }
constructor trans_bilinear.Construct(quad : double_ptr; x1 ,y1 ,x2 ,y2 : double );
begin
 inherited Construct;

 quad_to_rect(quad ,x1 ,y1 ,x2 ,y2 );

 transform:=@_transform;

end;

{ QUAD_TO_QUAD }
procedure trans_bilinear.quad_to_quad;
var
 left  : double_44;
 right : double_42;

 i ,ix ,iy : unsigned;

begin
 for i:=0 to 3 do
  begin
   ix:=i * 2;
   iy:=ix + 1;

   left[i ,0 ]:=1.0;
   left[i ,1 ]:=double_ptr(ptrcomp(src ) + ix * sizeof(double ) )^ * double_ptr(ptrcomp(src ) + iy * sizeof(double ) )^;
   left[i ,2 ]:=double_ptr(ptrcomp(src ) + ix * sizeof(double ) )^;
   left[i ,3 ]:=double_ptr(ptrcomp(src ) + iy * sizeof(double ) )^;

   right[i ,0 ]:=double_ptr(ptrcomp(dst ) + ix * sizeof(double ) )^;
   right[i ,1 ]:=double_ptr(ptrcomp(dst ) + iy * sizeof(double ) )^;

  end;

 m_valid:=simul_eq_solve(@left ,@right ,@m_mtx ,4 ,2 );

end;

{ RECT_TO_QUAD }
procedure trans_bilinear.rect_to_quad;
var
 src : double_8;

begin
 src[0 ]:=x1;
 src[6 ]:=x1;
 src[2 ]:=x2;
 src[4 ]:=x2;
 src[1 ]:=y1;
 src[3 ]:=y1;
 src[5 ]:=y2;
 src[7 ]:=y2;

 quad_to_quad(@src ,quad );

end;

{ QUAD_TO_RECT }
procedure trans_bilinear.quad_to_rect;
var
 dst : double_8;

begin
 dst[0 ]:=x1;
 dst[6 ]:=x1;
 dst[2 ]:=x2;
 dst[4 ]:=x2;
 dst[1 ]:=y1;
 dst[3 ]:=y1;
 dst[5 ]:=y2;
 dst[7 ]:=y2;

 quad_to_quad(quad ,@dst );

end;

{ IS_VALID }
function trans_bilinear.is_valid;
begin
 result:=m_valid;

end;

{ BEGIN_ }
function trans_bilinear.begin_;
begin
 result.Construct(x ,y ,step ,@m_mtx );

end;

END.

