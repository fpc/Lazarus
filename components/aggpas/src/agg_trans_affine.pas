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
// Affine transformation classes.
//
// [Pascal Port History] -----------------------------------------------------
//
// 26.10.2007-Milano: Reflection Transformations
// 27.09.2005-Milano: Complete unit port
//
//----------------------------------------------------------------------------
//
// Affine transformation are linear transformations in Cartesian coordinates
// (strictly speaking not only in Cartesian, but for the beginning we will
// think so). They are rotation, scaling, translation and skewing.
// After any affine transformation a line segment remains a line segment
// and it will never become a curve.
//
// There will be no math about matrix calculations, since it has been
// described many times. Ask yourself a very simple question:
// "why do we need to understand and use some matrix stuff instead of just
// rotating, scaling and so on". The answers are:
//
// 1. Any combination of transformations can be done by only 4 multiplications
//    and 4 additions in floating point.
// 2. One matrix transformation is equivalent to the number of consecutive
//    discrete transformations, i.e. the matrix "accumulates" all transformations
//    in the order of their settings. Suppose we have 4 transformations:
//       * rotate by 30 degrees,
//       * scale X to 2.0,
//       * scale Y to 1.5,
//       * move to (100, 100).
//    The result will depend on the order of these transformations,
//    and the advantage of matrix is that the sequence of discret calls:
//    rotate(30), scaleX(2.0), scaleY(1.5), move(100,100)
//    will have exactly the same result as the following matrix transformations:
//
//    affine_matrix m;
//    m *= rotate_matrix(30);
//    m *= scaleX_matrix(2.0);
//    m *= scaleY_matrix(1.5);
//    m *= move_matrix(100,100);
//
//    m.transform_my_point_at_last(x, y);
//
// What is the good of it? In real life we will set-up the matrix only once
// and then transform many points, let alone the convenience to set any
// combination of transformations.
//
// So, how to use it? Very easy - literally as it's shown above. Not quite,
// let us write a correct example:
//
// agg::trans_affine m;
// m *= agg::trans_affine_rotation(30.0 * 3.1415926 / 180.0);
// m *= agg::trans_affine_scaling(2.0, 1.5);
// m *= agg::trans_affine_translation(100.0, 100.0);
// m.transform(&x, &y);
//
// The affine matrix is all you need to perform any linear transformation,
// but all transformations have origin point (0,0). It means that we need to
// use 2 translations if we want to rotate someting around (100,100):
//
// m *= agg::trans_affine_translation(-100.0, -100.0);         // move to (0,0)
// m *= agg::trans_affine_rotation(30.0 * 3.1415926 / 180.0);  // rotate
// m *= agg::trans_affine_translation(100.0, 100.0);           // move back to (100,100)
//
{ agg_trans_affine.pas }
unit
 agg_trans_affine ;

INTERFACE

{$I agg_mode.inc }

uses
 Math ,
 agg_basics ;

{ TYPES DEFINITION }
const
 affine_epsilon = 1e-14; // About of precision of doubles

type
 trans_affine_ptr = ^trans_affine;

 proc_transform = procedure(this : trans_affine_ptr; x ,y : double_ptr );

 parallelo_ptr = ^parallelogram;
 parallelogram = array[0..5 ] of double;

 trans_affine = object
 { sx ,shy ,shx ,sy ,tx ,ty }
   m0 ,m1  ,m2  ,m3 ,m4 ,m5 : double;

   transform         ,
   transform_2x2     ,
   inverse_transform : proc_transform;

  // Construct an identity matrix - it does not transform anything
   constructor Construct; overload;

  // Construct a custom matrix. Usually used in derived classes
   constructor Construct(v0 ,v1 ,v2 ,v3 ,v4 ,v5 : double ); overload;

  // Construct a matrix to transform a parallelogram to another one
   constructor Construct(rect, parl : parallelo_ptr ); overload;

  // Construct a matrix to transform a rectangle to a parallelogram
   constructor Construct(x1 ,y1 ,x2 ,y2 : double; parl : parallelo_ptr ); overload;

  // Construct a matrix to transform a parallelogram to a rectangle
   constructor Construct(parl : parallelo_ptr; x1 ,y1 ,x2 ,y2 : double ); overload;

  // Construct a matrix with different transform function
   constructor Construct(tr : proc_transform ); overload;

  //---------------------------------- Parallelogram transformations
  // Calculate a matrix to transform a parallelogram to another one.
  // src and dst are pointers to arrays of three points
  // (double[6], x,y,...) that identify three corners of the
  // parallelograms assuming implicit fourth points.
  // There are also transformations rectangtle to parallelogram and
  // parellelogram to rectangle
   procedure parl_to_parl(src ,dst : parallelo_ptr );
   procedure rect_to_parl(x1 ,y1 ,x2 ,y2 : double; parl : parallelo_ptr );
   procedure parl_to_rect(parl : parallelo_ptr; x1 ,y1 ,x2 ,y2 : double );

  //------------------------------------------ Operations
  // Reset - actually load an identity matrix
   procedure reset; virtual;

  // Multiply matrix to another one
   procedure multiply(m : trans_affine_ptr );

  // Multiply "m" to "this" and assign the result to "this"
   procedure premultiply(m : trans_affine_ptr );

  // Multiply matrix to inverse of another one
   procedure multiply_inv(m : trans_affine_ptr );

  // Multiply inverse of "m" to "this" and assign the result to "this"
   procedure premultiply_inv(m : trans_affine_ptr );

  // Invert matrix. Do not try to invert degenerate matrices,
  // there's no check for validity. If you set scale to 0 and
  // then try to invert matrix, expect unpredictable result.
   procedure invert;

  // Mirroring around X
   procedure flip_x;

  // Mirroring around Y
   procedure flip_y;

  //------------------------------------------- Load/Store
  // Store matrix to an array [6] of double
   procedure store_to(m : parallelo_ptr );

  // Load matrix from an array [6] of double
   procedure load_from(m : parallelo_ptr );

  //-------------------------------------------- Transformations
  // Direct transformation x and y
  // see: transform : proc_transform; above

  // Direct transformation x and y, 2x2 matrix only, no translation
  // procedure transform_2x2(x ,y : double_ptr );

  // Inverse transformation x and y. It works slower than the
  // direct transformation, so if the performance is critical
  // it's better to invert() the matrix and then use transform()
  // procedure inverse_transform(x ,y : double_ptr );

  //-------------------------------------------- Auxiliary
  // Calculate the determinant of matrix
   function  determinant : double;

  // Get the average scale (by X and Y).
  // Basically used to calculate the approximation_scale when
  // decomposinting curves into line segments.
   function  scale : double; overload;

  // Check to see if it's an identity matrix
   function  is_identity(epsilon : double = affine_epsilon ) : boolean;

  // Check to see if two matrices are equal
   function  is_equal(m : trans_affine; epsilon : double = affine_epsilon ) : boolean;

  // Determine the major parameters. Use carefully considering degenerate matrices
   function  rotation : double;
   procedure translation(dx ,dy : double_ptr );
   procedure scaling    (sx ,sy : double_ptr );
   procedure scaling_abs(sx ,sy : double_ptr );

  // Trans Affine Assignations
   procedure assign    (from : trans_affine_ptr );
   procedure assign_all(from : trans_affine_ptr );

  // Direct transformations operations
   function  translate(x ,y : double ) : trans_affine_ptr;
   function  rotate   (a : double ) : trans_affine_ptr;
   function  scale    (s : double ) : trans_affine_ptr; overload;
   function  scale    (x ,y : double ) : trans_affine_ptr; overload;

  end;

//====================================================trans_affine_rotation
// Rotation matrix. sin() and cos() are calculated twice for the same angle.
// There's no harm because the performance of sin()/cos() is very good on all
// modern processors. Besides, this operation is not going to be invoked too
// often.
 trans_affine_rotation = object(trans_affine )
   constructor Construct(a : double );

  end;

//====================================================trans_affine_scaling
// Scaling matrix. sx, sy - scale coefficients by X and Y respectively
 trans_affine_scaling = object(trans_affine )
   constructor Construct(sx ,sy : double ); overload;
   constructor Construct(s : double ); overload;

  end;

//================================================trans_affine_translation
// Translation matrix
 trans_affine_translation = object(trans_affine )
   constructor Construct(tx ,ty : double );

  end;

//====================================================trans_affine_skewing
// Sckewing (shear) matrix
 trans_affine_skewing = object(trans_affine )
   constructor Construct(sx ,sy : double );

  end;

//===============================================trans_affine_line_segment
// Rotate, Scale and Translate, associating 0...dist with line segment
// x1,y1,x2,y2
 trans_affine_line_segment = object(trans_affine )
   constructor Construct(x1 ,y1 ,x2 ,y2 ,dist : double );

  end;

//============================================trans_affine_reflection_unit
// Reflection matrix. Reflect coordinates across the line through
// the origin containing the unit vector (ux, uy).
// Contributed by John Horigan
 trans_affine_reflection_unit = object(trans_affine )
   constructor Construct(ux ,uy : double );

  end;

//=================================================trans_affine_reflection
// Reflection matrix. Reflect coordinates across the line through
// the origin at the angle a or containing the non-unit vector (x, y).
// Contributed by John Horigan
 trans_affine_reflection = object(trans_affine_reflection_unit )
   constructor Construct(a : double ); overload;
   constructor Construct(x ,y : double ); overload;

  end;

{ GLOBAL PROCEDURES }
 function  is_equal_eps(v1 ,v2 ,epsilon : double ) : boolean;


IMPLEMENTATION
{ UNIT IMPLEMENTATION }
{ is_equal_eps }
function is_equal_eps;
begin
 result:=Abs(v1 - v2 ) < epsilon;

end;

{ trans_affine_transform }
procedure trans_affine_transform(this : trans_affine_ptr; x ,y : double_ptr );
var
 tx : double;

begin
 tx:=x^;
 x^:=tx * this.m0 + y^ * this.m2 + this.m4;
 y^:=tx * this.m1 + y^ * this.m3 + this.m5;

end;

{ trans_affine_transform_2x2 }
procedure trans_affine_transform_2x2(this : trans_affine_ptr; x ,y : double_ptr );
var
 tx : double;

begin
 tx:=x^;
 x^:=tx * this.m0 + y^ * this.m2;
 y^:=tx * this.m1 + y^ * this.m3;

end;

{ trans_affine_inverse_transform }
procedure trans_affine_inverse_transform(this : trans_affine_ptr; x ,y : double_ptr );
var
 d ,a ,b : double;

begin
 d:=this.determinant;
 a:=(x^ - this.m4 ) * d;
 b:=(y^ - this.m5 ) * d;

 x^:=a * this.m3 - b * this.m2;
 y^:=b * this.m0 - a * this.m1;

end;

{ CONSTRUCT }
constructor trans_affine.Construct;
begin
 m0:=1;
 m1:=0;
 m2:=0;
 m3:=1;
 m4:=0;
 m5:=0;

 transform        :=@trans_affine_transform;
 transform_2x2    :=@trans_affine_transform_2x2;
 inverse_transform:=@trans_affine_inverse_transform;

end;

{ CONSTRUCT }
constructor trans_affine.Construct(v0 ,v1 ,v2 ,v3 ,v4 ,v5 : double );
begin
 m0:=v0;
 m1:=v1;
 m2:=v2;
 m3:=v3;
 m4:=v4;
 m5:=v5;

 transform        :=@trans_affine_transform;
 transform_2x2    :=@trans_affine_transform_2x2;
 inverse_transform:=@trans_affine_inverse_transform;

end;

{ CONSTRUCT }
constructor trans_affine.Construct(rect, parl : parallelo_ptr );
begin
 parl_to_parl(rect ,parl );

 transform        :=@trans_affine_transform;
 transform_2x2    :=@trans_affine_transform_2x2;
 inverse_transform:=@trans_affine_inverse_transform;

end;

{ CONSTRUCT }
constructor trans_affine.Construct(x1 ,y1 ,x2 ,y2 : double; parl : parallelo_ptr );
begin
 rect_to_parl(x1 ,y1 ,x2 ,y2 ,parl );

 transform        :=@trans_affine_transform;
 transform_2x2    :=@trans_affine_transform_2x2;
 inverse_transform:=@trans_affine_inverse_transform;

end;

{ CONSTRUCT }
constructor trans_affine.Construct(parl : parallelo_ptr; x1 ,y1 ,x2 ,y2 : double );
begin
 parl_to_rect(parl ,x1 ,y1 ,x2 ,y2 );

 transform        :=@trans_affine_transform;
 transform_2x2    :=@trans_affine_transform_2x2;
 inverse_transform:=@trans_affine_inverse_transform;

end;

{ CONSTRUCT }
constructor trans_affine.Construct(tr : proc_transform );
begin
 m0:=1;
 m1:=0;
 m2:=0;
 m3:=1;
 m4:=0;
 m5:=0;

 transform        :=tr;
 transform_2x2    :=@trans_affine_transform_2x2;
 inverse_transform:=@trans_affine_inverse_transform;

end;

{ parl_to_parl }
procedure trans_affine.parl_to_parl;
var
 m : trans_affine;

begin
 m0:=src[2 ] - src[0 ];
 m1:=src[3 ] - src[1 ];
 m2:=src[4 ] - src[0 ];
 m3:=src[5 ] - src[1 ];
 m4:=src[0 ];
 m5:=src[1 ];

 invert;

 m.Construct(
  dst[2 ] - dst[0 ] ,
  dst[3 ] - dst[1 ] ,
  dst[4 ] - dst[0 ] ,
  dst[5 ] - dst[1 ] ,
  dst[0 ] ,
  dst[1 ] );

 multiply(@m );

end;

{ rect_to_parl }
procedure trans_affine.rect_to_parl;
var
 src : parallelogram;

begin
 src[0 ]:=x1;
 src[1 ]:=y1;
 src[2 ]:=x2;
 src[3 ]:=y1;
 src[4 ]:=x2;
 src[5 ]:=y2;

 parl_to_parl(@src ,parl );

end;

{ parl_to_rect }
procedure trans_affine.parl_to_rect;
var
 dst : parallelogram;

begin
 dst[0 ]:=x1;
 dst[1 ]:=y1;
 dst[2 ]:=x2;
 dst[3 ]:=y1;
 dst[4 ]:=x2;
 dst[5 ]:=y2;

 parl_to_parl(parl ,@dst );

end;

{ reset }
procedure trans_affine.reset;
begin
 m0:=1;
 m1:=0;
 m2:=0;
 m3:=1;
 m4:=0;
 m5:=0;

end;

{ multiply }
procedure trans_affine.multiply;
var
 t0 ,t2 ,t4 : double;

begin
 t0:=m0 * m.m0 + m1 * m.m2;
 t2:=m2 * m.m0 + m3 * m.m2;
 t4:=m4 * m.m0 + m5 * m.m2 + m.m4;
 m1:=m0 * m.m1 + m1 * m.m3;
 m3:=m2 * m.m1 + m3 * m.m3;
 m5:=m4 * m.m1 + m5 * m.m3 + m.m5;
 m0:=t0;
 m2:=t2;
 m4:=t4;

end;

{ premultiply }
procedure trans_affine.premultiply;
var
 t : trans_affine;

begin
 t.assign_all(m );

 t.multiply(@self );

 assign(@t );

end;

{ multiply_inv }
procedure trans_affine.multiply_inv;
var
 t : trans_affine;

begin
 t.assign_all(m );
 t.invert;

 multiply(@t );

end;

{ premultiply_inv }
procedure trans_affine.premultiply_inv;
var
 t : trans_affine;

begin
 t.assign_all(m );

 t.invert;
 t.multiply(@self );

 assign(@t );

end;

{ invert }
procedure trans_affine.invert;
var
 d ,t0 ,t4 : double;

begin
 d:=determinant;

 t0:= m3 * d;
 m3:= m0 * d;
 m1:=-m1 * d;
 m2:=-m2 * d;

 t4:=-m4 * t0 - m5 * m2;
 m5:=-m4 * m1 - m5 * m3;

 m0:=t0;
 m4:=t4;

end;

{ flip_x }
procedure trans_affine.flip_x;
begin
 m0:=-m0;
 m1:=-m1;
 m4:=-m4;

end;

{ flip_y }
procedure trans_affine.flip_y;
begin
 m2:=-m2;
 m3:=-m3;
 m5:=-m5;

end;

{ store_to }
procedure trans_affine.store_to;
begin
 m[0 ]:=m0;
 m[1 ]:=m1;
 m[2 ]:=m2;
 m[3 ]:=m3;
 m[4 ]:=m4;
 m[5 ]:=m5;

end;

{ load_from }
procedure trans_affine.load_from;
begin
 m0:=m[0 ];
 m1:=m[1 ];
 m2:=m[2 ];
 m3:=m[3 ];
 m4:=m[4 ];
 m5:=m[5 ];

end;

{ determinant }
function trans_affine.determinant;
begin
 try
  result:=1 / (m0 * m3 - m1 * m2 );

 except
  result:=0;

 end;

end;

{ scale }
function trans_affine.scale : double;
var
 x ,y : double;

begin
 x:=0.707106781 * m0 + 0.707106781 * m2;
 y:=0.707106781 * m1 + 0.707106781 * m3;

 result:=Sqrt(x * x + y * y );

end;

{ is_identity }
function trans_affine.is_identity;
begin
 result:=
  is_equal_eps(m0 ,1 ,epsilon ) and
  is_equal_eps(m1 ,0 ,epsilon ) and
  is_equal_eps(m2 ,0 ,epsilon ) and
  is_equal_eps(m3 ,1 ,epsilon ) and
  is_equal_eps(m4 ,0 ,epsilon ) and
  is_equal_eps(m5 ,0 ,epsilon );

end;

{ is_equal }
function trans_affine.is_equal;
begin
 result:=
  is_equal_eps(m0 ,m.m0 ,epsilon ) and
  is_equal_eps(m1 ,m.m1 ,epsilon ) and
  is_equal_eps(m2 ,m.m2 ,epsilon ) and
  is_equal_eps(m3 ,m.m3 ,epsilon ) and
  is_equal_eps(m4 ,m.m4 ,epsilon ) and
  is_equal_eps(m5 ,m.m5 ,epsilon );
  
end;

{ rotation }
function trans_affine.rotation;
var
 x1 ,y1 ,x2 ,y2 : double;

begin
 x1:=0;
 y1:=0;
 x2:=1;
 y2:=0;

 transform(@self ,@x1 ,@y1 );
 transform(@self ,@x2 ,@y2 );

 result:=ArcTan2(y2 - y1 ,x2 - x1 );

end;

{ translation }
procedure trans_affine.translation;
begin
 dx:=0;
 dy:=0;

 transform(@self ,@dx ,@dy );

end;

{ scaling }
procedure trans_affine.scaling;
var
 t : trans_affine_rotation;

 x1 ,y1 ,x2 ,y2 : double;

begin
 x1:=0;
 y1:=0;
 x2:=1;
 y2:=1;

 trans_affine(t ):=self;

 t.Construct(-rotation );

 t.transform(@self ,@x1 ,@y1 );
 t.transform(@self ,@x2 ,@y2 );

 sx^:=x2 - x1;
 sy^:=y2 - y1;

end;

{ scaling_abs }
procedure trans_affine.scaling_abs;
begin
 sx^:=Sqrt(m0 * m0 + m2 * m2 );
 sy^:=Sqrt(m1 * m1 + m3 * m3 );

end;

{ ASSIGN }
procedure trans_affine.assign;
begin
 m0:=from.m0;
 m1:=from.m1;
 m2:=from.m2;
 m3:=from.m3;
 m4:=from.m4;
 m5:=from.m5;

end;

{ ASSIGN_ALL }
procedure trans_affine.assign_all;
begin
 m0:=from.m0;
 m1:=from.m1;
 m2:=from.m2;
 m3:=from.m3;
 m4:=from.m4;
 m5:=from.m5;

 transform        :=@from.transform;
 transform_2x2    :=@from.transform_2x2;
 inverse_transform:=@from.inverse_transform;

end;

{ TRANSLATE }
function trans_affine.translate(x ,y : double ) : trans_affine_ptr;
begin
 m4:=m4 + x;
 m5:=m5 + y;

 result:=@self;

end;

{ ROTATE }
function trans_affine.rotate(a : double ) : trans_affine_ptr;
var
 ca ,sa ,t0 ,t2 ,t4 : double;

begin
 ca:=Cos(a);
 sa:=Sin(a);
 t0:=m0 * ca - m1 * sa;
 t2:=m2 * ca - m3 * sa;
 t4:=m4 * ca - m5 * sa;

 m1:=m0 * sa + m1 * ca;
 m3:=m2 * sa + m3 * ca;
 m5:=m4 * sa + m5 * ca;
 m0:=t0;
 m2:=t2;
 m4:=t4;

 result:=@self;

end;

{ SCALE }
function trans_affine.scale(s : double ) : trans_affine_ptr;
begin
 m0:=m0 * s;
 m1:=m1 * s;
 m2:=m2 * s;
 m3:=m3 * s;
 m4:=m4 * s;
 m5:=m5 * s;

 result:=@self;

end;

{ SCALE }
function trans_affine.scale(x ,y : double ) : trans_affine_ptr;
begin
 m0:=m0 * x;
 m2:=m2 * x;
 m4:=m4 * x;
 m1:=m1 * y;
 m3:=m3 * y;
 m5:=m5 * y;

 result:=@self;

end;

{ CONSTRUCT }
constructor trans_affine_rotation.Construct;
begin
 inherited Construct(Cos(a ) ,Sin(a ) ,-Sin(a ) ,Cos(a ) ,0 ,0 );

end;

{ CONSTRUCT }
constructor trans_affine_scaling.Construct(sx ,sy : double );
begin
 inherited Construct(sx ,0 ,0 ,sy ,0 ,0 );

end;

{ CONSTRUCT }
constructor trans_affine_scaling.Construct(s : double );
begin
 inherited Construct(s ,0 ,0 ,s ,0 ,0 );

end;

{ CONSTRUCT }
constructor trans_affine_translation.Construct;
begin
 inherited Construct(1 ,0 ,0 ,1 ,tx ,ty );

end;

{ CONSTRUCT }
constructor trans_affine_skewing.Construct;
begin
 inherited Construct(1 ,Tan(sy ) ,Tan(sx ) ,1 ,0 ,0 );

end;

{ CONSTRUCT }
constructor trans_affine_line_segment.Construct;
var
 dx ,dy : double;

 s : trans_affine_scaling;
 r : trans_affine_rotation;
 t : trans_affine_translation;

begin
 dx:=x2 - x1;
 dy:=y2 - y1;

 if dist > 0 then
  begin
   s.Construct(Sqrt(dx * dx + dy * dy ) / dist );

   multiply(@s );

  end;

 r.Construct(ArcTan2(dy ,dx ) );

 multiply(@r );

 t.Construct(x1 ,y1 );

 multiply(@t );

end;

{ CONSTRUCT }
constructor trans_affine_reflection_unit.Construct(ux ,uy : double );
begin
 inherited Construct(
  2.0 * ux * ux - 1.0 ,
  2.0 * ux * uy ,
  2.0 * ux * uy ,
  2.0 * uy * uy - 1.0 ,
  0.0 ,0.0 );

end;

{ CONSTRUCT }
constructor trans_affine_reflection.Construct(a : double );
begin
 inherited Construct(Cos(a ) ,Sin(a ) );

end;

{ CONSTRUCT }
constructor trans_affine_reflection.Construct(x ,y : double );
var
 nx ,ny : double;

begin
 if (x = 0 ) and
    (y = 0 ) then
  begin
   x:=0;
   y:=0;

  end
 else
  begin
   nx:=x / Sqrt(x * x + y * y );
   ny:=y / Sqrt(x * x + y * y );

  end;

 inherited Construct(nx ,ny );

end;

END.

