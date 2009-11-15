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
// Perspective 2D transformations
//
// [Pascal Port History] -----------------------------------------------------
//
// 13.11.2007-Milano: trans_perspective v 2.4
// 23.06.2006-Milano: ptrcomp adjustments
// 06.02.2006-Milano: Unit port establishment
//
{ agg_trans_perspective.pas }
unit
 agg_trans_perspective ;

INTERFACE

{$I agg_mode.inc }

uses
 Math ,
 agg_basics ,
 agg_simul_eq ,
 agg_trans_affine ;

{ TYPES DEFINITION }
type
 iterator_x23 = object
   den   ,den_step   ,
   nom_x ,nom_x_step ,
   nom_y ,nom_y_step : double;

   x ,y : double;

   constructor Construct; overload;
   constructor Construct(tx ,ty ,step : double; m : double_ptr ); overload;

   procedure inc_operator;

  end;

 trans_perspective23_ptr = ^trans_perspective23;
 trans_perspective23 = object(trans_affine )
   m_valid : boolean;
   m_mtx   : array[0..7 ] of double;

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

   function  begin_(x ,y ,step : double ) : iterator_x23;

  end;

 quadrilateral_ptr = ^quadrilateral;
 quadrilateral = array[0..8 ] of double;

 trans_perspective_ptr = ^trans_perspective;

 iterator_x = object
   den   ,den_step   ,
   nom_x ,nom_x_step ,
   nom_y ,nom_y_step : double;

   x ,y : double;

   constructor Construct; overload;
   constructor Construct(px ,py ,step : double; m : trans_perspective_ptr ); overload;

   procedure operator_inc;

  end;

 trans_perspective = object(trans_affine )
   sx ,shy ,w0 ,shx ,sy ,w1 ,tx ,ty ,w2 : double;

   transform_affine : proc_transform;

  //------------------------------------------------------- Construction
  // Identity matrix
   constructor Construct; overload;

  // Custom matrix
   constructor Construct(v0 ,v1 ,v2 ,v3 ,v4 ,v5 ,v6 ,v7 ,v8 : double ); overload;

  // Custom matrix from m[9]
   constructor Construct(m : quadrilateral_ptr ); overload;

  // From affine
   constructor Construct_af(a : trans_affine_ptr );

  // From affine
   constructor Construct(p : trans_perspective_ptr ); overload;

  // Rectangle to quadrilateral
   constructor Construct(x1 ,y1 ,x2 ,y2 : double; quad : quadrilateral_ptr ); overload;

  // Quadrilateral to rectangle
   constructor Construct(quad : quadrilateral_ptr; x1 ,y1 ,x2 ,y2 : double ); overload;

  // Arbitrary quadrilateral transformations
   constructor Construct(src ,dst : quadrilateral_ptr ); overload;

  //-------------------------------------- Quadrilateral transformations
  // The arguments are double[8] that are mapped to quadrilaterals:
  // x1,y1, x2,y2, x3,y3, x4,y4
   function  quad_to_quad(qs ,qd : quadrilateral_ptr ) : boolean;
   function  rect_to_quad(x1 ,y1 ,x2 ,y2 : double; q : quadrilateral_ptr ) : boolean;
   function  quad_to_rect(q : quadrilateral_ptr; x1 ,y1 ,x2 ,y2 : double ) : boolean;

  // Map square (0,0,1,1) to the quadrilateral and vice versa
   function  square_to_quad(q : quadrilateral_ptr ) : boolean;
   function  quad_to_square(q : quadrilateral_ptr ) : boolean;

  //--------------------------------------------------------- Operations
  // Reset - load an identity matrix
   function  reset_ : trans_perspective_ptr;

  // Invert matrix. Returns false in degenerate case
   function  invert : boolean;

  // Direct transformations operations
   function  translate(x ,y : double ) : trans_perspective_ptr;
   function  rotate   (a : double ) : trans_perspective_ptr;
   function  scale    (s : double ) : trans_perspective_ptr; overload;
   function  scale    (x ,y : double ) : trans_perspective_ptr; overload;

  // Multiply the matrix by another one
   function  multiply(a : trans_perspective_ptr ) : trans_perspective_ptr;

  // Multiply "m" by "this" and assign the result to "this"
   function  premultiply(b : trans_perspective_ptr ) : trans_perspective_ptr;

  // Multiply matrix to inverse of another one
   function  multiply_inv(m : trans_perspective_ptr ) : trans_perspective_ptr;

  // Multiply inverse of "m" by "this" and assign the result to "this"
   function  premultiply_inv(m : trans_perspective_ptr ) : trans_perspective_ptr;

  // Multiply the matrix by another one
   function  multiply_af(a : trans_affine_ptr ) : trans_perspective_ptr;

  // Multiply "m" by "this" and assign the result to "this"
   function  premultiply_af(b : trans_affine_ptr ) : trans_perspective_ptr;

  // Multiply the matrix by inverse of another one
   function  multiply_inv_af(m : trans_affine_ptr ) : trans_perspective_ptr;

  // Multiply inverse of "m" by "this" and assign the result to "this"
   function  premultiply_inv_af(m : trans_affine_ptr ) : trans_perspective_ptr;

  //--------------------------------------------------------- Load/Store
   procedure store_to (m : quadrilateral_ptr );
   function  load_from(m : quadrilateral_ptr ) : trans_perspective_ptr;

  //---------------------------------------------------------- Auxiliary
   function  from_affine(a : trans_affine_ptr ) : trans_perspective_ptr;

   function  determinant : double;
   function  determinant_reciprocal : double;

   function  is_valid   (epsilon : double = affine_epsilon ) : boolean;
   function  is_identity(epsilon : double = affine_epsilon ) : boolean;
   function  is_equal   (m : trans_perspective_ptr; epsilon : double = affine_epsilon ) : boolean;

  // Determine the major affine parameters. Use with caution
  // considering possible degenerate cases.
   function  scale : double; overload;
   function  rotation : double;

   procedure translation(dx ,dy : double_ptr );
   procedure scaling    (x ,y : double_ptr );
   procedure scaling_abs(x ,y : double_ptr );

  // private
   function  begin_(x ,y ,step : double ) : iterator_x;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor iterator_x23.Construct;
begin
end;

{ CONSTRUCT }
constructor iterator_x23.Construct(tx ,ty ,step : double; m : double_ptr );
begin
 den     :=double_ptr(ptrcomp(m ) + 6 * sizeof(double ) )^ * tx + double_ptr(ptrcomp(m ) + 7 * sizeof(double ) )^ * ty + 1.0;
 den_step:=double_ptr(ptrcomp(m ) + 6 * sizeof(double ) )^ * step;

 nom_x     :=double_ptr(ptrcomp(m ) + 0 * sizeof(double ) )^ + double_ptr(ptrcomp(m ) + 1 * sizeof(double ) )^ * tx + double_ptr(ptrcomp(m ) + 2 * sizeof(double ) )^ * ty;
 nom_x_step:=double_ptr(ptrcomp(m ) + 1 * sizeof(double ) )^ * step;

 nom_y     :=double_ptr(ptrcomp(m ) + 3 * sizeof(double ) )^ + double_ptr(ptrcomp(m ) + 4 * sizeof(double ) )^ * tx + double_ptr(ptrcomp(m ) + 5 * sizeof(double ) )^ * ty;
 nom_y_step:=double_ptr(ptrcomp(m ) + 4 * sizeof(double ) )^ * step;

 x:=nom_x / den;
 y:=nom_y / den;

end;

{ INC_OPERATOR }
procedure iterator_x23.inc_operator;
var
 d : double;

begin
 den  :=den   + den_step;
 nom_x:=nom_x + nom_x_step;
 nom_y:=nom_y + nom_y_step;

 d:=1.0 / den;
 x:=nom_x * d;
 y:=nom_y * d;

end;

{ _transform23 }
procedure _transform23(this : trans_perspective23_ptr; x ,y : double_ptr );
var
 tx ,ty ,d : double;

begin
 tx:=x^;
 ty:=y^;
 d :=1.0 / (this.m_mtx[6 ] * tx + this.m_mtx[7 ] * ty + 1.0 );

 x^:=(this.m_mtx[0 ] + this.m_mtx[1 ] * tx + this.m_mtx[2 ] * ty ) * d;
 y^:=(this.m_mtx[3 ] + this.m_mtx[4 ] * tx + this.m_mtx[5 ] * ty ) * d;

end;

{ CONSTRUCT }
constructor trans_perspective23.Construct;
begin
 inherited Construct;

 m_valid:=false;

 transform:=@_transform23;

end;

{ CONSTRUCT }
constructor trans_perspective23.Construct(src ,dst : double_ptr );
begin
 inherited Construct;

 quad_to_quad(src ,dst );

 transform:=@_transform23;

end;

{ CONSTRUCT }
constructor trans_perspective23.Construct(x1 ,y1 ,x2 ,y2 : double; quad : double_ptr );
begin
 inherited Construct;

 rect_to_quad(x1 ,y1 ,x2 ,y2 ,quad );

 transform:=@_transform23;

end;

{ CONSTRUCT }
constructor trans_perspective23.Construct(quad : double_ptr; x1 ,y1 ,x2 ,y2 : double );
begin
 inherited Construct;

 quad_to_rect(quad ,x1 ,y1 ,x2 ,y2 );

 transform:=@_transform23;

end;

{ QUAD_TO_QUAD }
procedure trans_perspective23.quad_to_quad;
var
 left  : double_88;
 right : double_81;

 i ,ix ,iy : unsigned;

begin
 for i:=0 to 3 do
  begin
   ix:=i * 2;
   iy:=ix + 1;

   left [ix ][0 ]:=1.0;
   left [ix ][1 ]:=double_ptr(ptrcomp(src ) + ix * sizeof(double ) )^;
   left [ix ][2 ]:=double_ptr(ptrcomp(src ) + iy * sizeof(double ) )^;
   left [ix ][3 ]:=0.0;
   left [ix ][4 ]:=0.0;
   left [ix ][5 ]:=0.0;
   left [ix ][6 ]:=-double_ptr(ptrcomp(src ) + ix * sizeof(double ) )^ * double_ptr(ptrcomp(dst ) + ix * sizeof(double ) )^;
   left [ix ][7 ]:=-double_ptr(ptrcomp(src ) + iy * sizeof(double ) )^ * double_ptr(ptrcomp(dst ) + ix * sizeof(double ) )^;
   right[ix ][0 ]:=double_ptr(ptrcomp(dst ) + ix * sizeof(double ) )^;

   left [iy ][0 ]:=0.0;
   left [iy ][1 ]:=0.0;
   left [iy ][2 ]:=0.0;
   left [iy ][3 ]:=1.0;
   left [iy ][4 ]:=double_ptr(ptrcomp(src ) + ix * sizeof(double ) )^;
   left [iy ][5 ]:=double_ptr(ptrcomp(src ) + iy * sizeof(double ) )^;
   left [iy ][6 ]:=-double_ptr(ptrcomp(src ) + ix * sizeof(double ) )^ * double_ptr(ptrcomp(dst ) + iy * sizeof(double ) )^;
   left [iy ][7 ]:=-double_ptr(ptrcomp(src ) + iy * sizeof(double ) )^ * double_ptr(ptrcomp(dst ) + iy * sizeof(double ) )^;
   right[iy ][0 ]:=double_ptr(ptrcomp(dst ) + iy * sizeof(double ) )^;

  end;

 m_valid:=simul_eq_solve(@left ,@right ,@m_mtx ,8 ,1 );

end;

{ RECT_TO_QUAD }
procedure trans_perspective23.rect_to_quad;
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
procedure trans_perspective23.quad_to_rect;
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
function trans_perspective23.is_valid;
begin
 result:=m_valid;

end;

{ BEGIN_ }
function trans_perspective23.begin_;
begin
 result.Construct(x ,y ,step ,@m_mtx );

end;

{ CONSTRUCT }
constructor iterator_x.Construct;
begin
end;

{ CONSTRUCT }
constructor iterator_x.Construct(px ,py ,step : double; m : trans_perspective_ptr );
begin
 den     :=px * m.w0 + py * m.w1 + m.w2;
 den_step:=m.w0 * step;

 nom_x     :=px * m.sx + py * m.shx + m.tx;
 nom_x_step:=step * m.sx;
 nom_y     :=px * m.shy + py * m.sy + m.ty;
 nom_y_step:=step * m.shy;

 x:=nom_x / den;
 y:=nom_y / den;

end;

{ OPERATOR_INC }
procedure iterator_x.operator_inc;
var
 d : double;

begin
 den:=den + den_step;

 nom_x:=nom_x + nom_x_step;
 nom_y:=nom_y + nom_y_step;

 d:=1.0 / den;
 x:=nom_x * d;
 y:=nom_y * d;

end;

{ _transform }
// Direct transformation of x and y
procedure _transform(this : trans_perspective_ptr; px ,py : double_ptr );
var
 x ,y ,m : double;

begin
 x:=px^;
 y:=py^;

 try
  m:=1.0 / (x * this.w0 + y * this.w1 + this.w2 );

 except
  m:=0;

 end;

 px^:=m * (x * this.sx  + y * this.shx + this.tx );
 py^:=m * (x * this.shy + y * this.sy  + this.ty );

end;

{ _transform_affine }
// Direct transformation of x and y, affine part only
procedure _transform_affine(this : trans_perspective_ptr; x ,y : double_ptr );
var
 tmp : double;

begin
 tmp:=x^;

 x^:=tmp * this.sx  + y^ * this.shx + this.tx;
 y^:=tmp * this.shy + y^ * this.sy  + this.ty;

end;

{ _transform_2x2 }
// Direct transformation of x and y, 2x2 matrix only, no translation
procedure _transform_2x2(this : trans_perspective_ptr; x ,y : double_ptr );
var
 tmp : double;

begin
 tmp:=x^;

 x^:=tmp * this.sx  + y^ * this.shx;
 y^:=tmp * this.shy + y^ * this.sy;

end;

{ _inverse_transform }
// Inverse transformation of x and y. It works slow because
// it explicitly inverts the matrix on every call. For massive
// operations it's better to invert() the matrix and then use
// direct transformations.
procedure _inverse_transform(this : trans_perspective_ptr; x ,y : double_ptr );
var
 t : trans_perspective;

begin
 t.Construct(this );

 if t.invert then
  t.transform(@t ,x ,y );

end;

{ CONSTRUCT }
constructor trans_perspective.Construct;
begin
 inherited Construct;

 sx :=1;
 shy:=0;
 w0 :=0;
 shx:=0;
 sy :=1;
 w1 :=0;
 tx :=0;
 ty :=0;
 w2 :=1;

 transform        :=@_transform;
 transform_2x2    :=@_transform_2x2;
 inverse_transform:=@_inverse_transform;
 transform_affine :=@_transform_affine;

end;

{ CONSTRUCT }
constructor trans_perspective.Construct(v0 ,v1 ,v2 ,v3 ,v4 ,v5 ,v6 ,v7 ,v8 : double );
begin
 inherited Construct;

 sx :=v0;
 shy:=v1;
 w0 :=v2;
 shx:=v3;
 sy :=v4;
 w1 :=v5;
 tx :=v6;
 ty :=v7;
 w2 :=v8;

 transform        :=@_transform;
 transform_2x2    :=@_transform_2x2;
 inverse_transform:=@_inverse_transform;
 transform_affine :=@_transform_affine;

end;

{ CONSTRUCT }
constructor trans_perspective.Construct(m : quadrilateral_ptr );
begin
 inherited Construct;

 sx :=m[0 ];
 shy:=m[1 ];
 w0 :=m[2 ];
 shx:=m[3 ];
 sy :=m[4 ];
 w1 :=m[5 ];
 tx :=m[6 ];
 ty :=m[7 ];
 w2 :=m[8 ];

 transform        :=@_transform;
 transform_2x2    :=@_transform_2x2;
 inverse_transform:=@_inverse_transform;
 transform_affine :=@_transform_affine;

end;

{ CONSTRUCT }
constructor trans_perspective.Construct_af(a : trans_affine_ptr );
begin
 inherited Construct;

 sx :=a.m0;
 shy:=a.m1;
 w0 :=0;
 shx:=a.m2;
 sy :=a.m3;
 w1 :=0;
 tx :=a.m4;
 ty :=a.m5;
 w2 :=1;

 transform        :=@_transform;
 transform_2x2    :=@_transform_2x2;
 inverse_transform:=@_inverse_transform;
 transform_affine :=@_transform_affine;

end;

{ CONSTRUCT }
constructor trans_perspective.Construct(p : trans_perspective_ptr );
begin
 inherited Construct;

 sx :=p.sx;
 shy:=p.shy;
 w0 :=p.w0;
 shx:=p.shx;
 sy :=p.sy;
 w1 :=p.w1;
 tx :=p.tx;
 ty :=p.ty;
 w2 :=p.w2;

 transform        :=@_transform;
 transform_2x2    :=@_transform_2x2;
 inverse_transform:=@_inverse_transform;
 transform_affine :=@_transform_affine;

end;

{ CONSTRUCT }
constructor trans_perspective.Construct(x1 ,y1 ,x2 ,y2 : double; quad : quadrilateral_ptr );
begin
 inherited Construct;

 rect_to_quad(x1 ,y1 ,x2 ,y2 ,quad );

 transform        :=@_transform;
 transform_2x2    :=@_transform_2x2;
 inverse_transform:=@_inverse_transform;
 transform_affine :=@_transform_affine;

end;

{ CONSTRUCT }
constructor trans_perspective.Construct(quad : quadrilateral_ptr; x1 ,y1 ,x2 ,y2 : double );
begin
 inherited Construct;

 quad_to_rect(quad ,x1 ,y1 ,x2 ,y2 );

 transform        :=@_transform;
 transform_2x2    :=@_transform_2x2;
 inverse_transform:=@_inverse_transform;
 transform_affine :=@_transform_affine;

end;

{ CONSTRUCT }
constructor trans_perspective.Construct(src ,dst : quadrilateral_ptr );
begin
 inherited Construct;

 quad_to_quad(src ,dst );

 transform        :=@_transform;
 transform_2x2    :=@_transform_2x2;
 inverse_transform:=@_inverse_transform;
 transform_affine :=@_transform_affine;

end;

{ QUAD_TO_QUAD }
function trans_perspective.quad_to_quad(qs ,qd : quadrilateral_ptr ) : boolean;
var
 p : trans_perspective;

begin
 p.Construct;

 if not quad_to_square(qs ) then
  begin
   result:=false;

   exit;

  end;

 if not p.square_to_quad(qd ) then
  begin
   result:=false;

   exit;

  end;

 multiply(@p );

 result:=true;

end;

{ RECT_TO_QUAD }
function trans_perspective.rect_to_quad(x1 ,y1 ,x2 ,y2 : double; q : quadrilateral_ptr ) : boolean;
var
 r : array[0..7 ] of double;

begin
 r[0 ]:=x1;
 r[6 ]:=x1;
 r[2 ]:=x2;
 r[4 ]:=x2;
 r[1 ]:=y1;
 r[3 ]:=y1;
 r[5 ]:=y2;
 r[7 ]:=y2;

 result:=quad_to_quad(@r[0 ] ,q );

end;

{ QUAD_TO_RECT }
function trans_perspective.quad_to_rect(q : quadrilateral_ptr; x1 ,y1 ,x2 ,y2 : double ) : boolean;
var
 r : array[0..7 ] of double;

begin
 r[0 ]:=x1;
 r[6 ]:=x1;
 r[2 ]:=x2;
 r[4 ]:=x2;
 r[1 ]:=y1;
 r[3 ]:=y1;
 r[5 ]:=y2;
 r[7 ]:=y2;

 result:=quad_to_quad(q ,@r[0 ] );

end;

{ SQUARE_TO_QUAD }
function trans_perspective.square_to_quad(q : quadrilateral_ptr ) : boolean;
var
 dx ,dy ,dx1 ,dy1 ,dx2 ,dy2 ,den ,u ,v : double;

begin
 dx:=q[0 ] - q[2 ] + q[4 ] - q[6 ];
 dy:=q[1 ] - q[3 ] + q[5 ] - q[7 ];

 if (dx = 0.0 ) and
    (dy = 0.0 ) then
  begin
  // Affine case (parallelogram)
   sx :=q[2 ] - q[0 ];
   shy:=q[3 ] - q[1 ];
   w0 :=0.0;
   shx:=q[4 ] - q[2 ];
   sy :=q[5 ] - q[3 ];
   w1 :=0.0;
   tx :=q[0 ];
   ty :=q[1 ];
   w2 :=1.0;

  end
 else
  begin
   dx1:=q[2 ] - q[4 ];
   dy1:=q[3 ] - q[5 ];
   dx2:=q[6 ] - q[4 ];
   dy2:=q[7 ] - q[5 ];
   den:=dx1 * dy2 - dx2 * dy1;

   if den = 0.0 then
    begin
    // Singular case
     sx :=0.0;
     shy:=0.0;
     w0 :=0.0;
     shx:=0.0;
     sy :=0.0;
     w1 :=0.0;
     tx :=0.0;
     ty :=0.0;
     w2 :=0.0;

     result:=false;

     exit;

    end;

  // General case
   u:=(dx * dy2 - dy * dx2 ) / den;
   v:=(dy * dx1 - dx * dy1 ) / den;

   sx :=q[2 ] - q[0 ] + u * q[2 ];
   shy:=q[3 ] - q[1 ] + u * q[3 ];
   w0 :=u;
   shx:=q[6 ] - q[0 ] + v * q[6 ];
   sy :=q[7 ] - q[1 ] + v * q[7 ];
   w1 :=v;
   tx :=q[0 ];
   ty :=q[1 ];
   w2 :=1.0;

  end;

 result:=true;

end;

{ QUAD_TO_SQUARE }
function trans_perspective.quad_to_square(q : quadrilateral_ptr ) : boolean;
begin
 if not square_to_quad(q ) then
  result:=false
 else
  begin
   invert;

   result:=true;

  end;

end;

{ RESET_ }
function trans_perspective.reset_ : trans_perspective_ptr;
begin
 sx :=1;
 shy:=0;
 w0 :=0;
 shx:=0;
 sy :=1;
 w1 :=0;
 tx :=0;
 ty :=0;
 w2 :=1;

 result:=@self;

end;

{ INVERT }
function trans_perspective.invert : boolean;
var
 d0 ,d1 ,d2 ,d : double;

 a : trans_perspective;

begin
 d0:=sy  * w2 - w1  * ty;
 d1:=w0  * ty - shy * w2;
 d2:=shy * w1 - w0  * sy;
 d :=sx  * d0 + shx * d1 + tx * d2;

 if d = 0.0 then
  begin
   sx :=0.0;
   shy:=0.0;
   w0 :=0.0;
   shx:=0.0;
   sy :=0.0;
   w1 :=0.0;
   tx :=0.0;
   ty :=0.0;
   w2 :=0.0;

   result:=false;

   exit;

  end;

 d:=1.0 / d;

 a.Construct(trans_perspective_ptr(@self ) );

 sx :=d * d0;
 shy:=d * d1;
 w0 :=d * d2;
 shx:=d * (a.w1  * a.tx  - a.shx * a.w2 );
 sy :=d * (a.sx  * a.w2  - a.w0  * a.tx );
 w1 :=d * (a.w0  * a.shx - a.sx  * a.w1 );
 tx :=d * (a.shx * a.ty  - a.sy  * a.tx );
 ty :=d * (a.shy * a.tx  - a.sx  * a.ty );
 w2 :=d * (a.sx  * a.sy  - a.shy * a.shx );

 result:=true;

end;

{ TRANSLATE }
function trans_perspective.translate(x ,y : double ) : trans_perspective_ptr;
begin
 tx:=tx + x;
 ty:=ty + y;

 result:=@self;

end;

{ ROTATE }
function trans_perspective.rotate(a : double ) : trans_perspective_ptr;
var
 tar : trans_affine_rotation;

begin
 tar.Construct(a );

 multiply_af(@tar );

 result:=@self;

end;

{ SCALE }
function trans_perspective.scale(s : double ) : trans_perspective_ptr;
var
 tas : trans_affine_scaling;

begin
 tas.Construct(s );

 multiply_af(@tas );

 result:=@self;

end;

{ SCALE }
function trans_perspective.scale(x ,y : double ) : trans_perspective_ptr;
var
 tas : trans_affine_scaling;

begin
 tas.Construct(x ,y );

 multiply_af(@tas );

 result:=@self;

end;

{ MULTIPLY }
function trans_perspective.multiply(a : trans_perspective_ptr ) : trans_perspective_ptr;
var
 b : trans_perspective;

begin
 b.Construct(trans_perspective_ptr(@self ) );

 sx :=a.sx  * b.sx  + a.shx * b.shy + a.tx * b.w0;
 shx:=a.sx  * b.shx + a.shx * b.sy  + a.tx * b.w1;
 tx :=a.sx  * b.tx  + a.shx * b.ty  + a.tx * b.w2;
 shy:=a.shy * b.sx  + a.sy  * b.shy + a.ty * b.w0;
 sy :=a.shy * b.shx + a.sy  * b.sy  + a.ty * b.w1;
 ty :=a.shy * b.tx  + a.sy  * b.ty  + a.ty * b.w2;
 w0 :=a.w0  * b.sx  + a.w1  * b.shy + a.w2 * b.w0;
 w1 :=a.w0  * b.shx + a.w1  * b.sy  + a.w2 * b.w1;
 w2 :=a.w0  * b.tx  + a.w1  * b.ty  + a.w2 * b.w2;

 result:=@self;

end;

{ PREMULTIPLY }
function trans_perspective.premultiply(b : trans_perspective_ptr ) : trans_perspective_ptr;
var
 a : trans_perspective;

begin
 a.Construct(trans_perspective_ptr(@self ) );

 sx :=a.sx  * b.sx  + a.shx * b.shy + a.tx * b.w0;
 shx:=a.sx  * b.shx + a.shx * b.sy  + a.tx * b.w1;
 tx :=a.sx  * b.tx  + a.shx * b.ty  + a.tx * b.w2;
 shy:=a.shy * b.sx  + a.sy  * b.shy + a.ty * b.w0;
 sy :=a.shy * b.shx + a.sy  * b.sy  + a.ty * b.w1;
 ty :=a.shy * b.tx  + a.sy  * b.ty  + a.ty * b.w2;
 w0 :=a.w0  * b.sx  + a.w1  * b.shy + a.w2 * b.w0;
 w1 :=a.w0  * b.shx + a.w1  * b.sy  + a.w2 * b.w1;
 w2 :=a.w0  * b.tx  + a.w1  * b.ty  + a.w2 * b.w2;

 result:=@self;

end;

{ MULTIPLY_INV }
function trans_perspective.multiply_inv(m : trans_perspective_ptr ) : trans_perspective_ptr;
var
 t : trans_perspective;

begin
 t.Construct(m );
 t.invert;

 result:=multiply(@t )

end;

{ PREMULTIPLY_INV }
function trans_perspective.premultiply_inv(m : trans_perspective_ptr ) : trans_perspective_ptr;
var
 t : trans_perspective;

begin
 t.Construct(m );
 t.invert;

 t.multiply(@self );

 Construct(trans_perspective_ptr(@self ) );

 result:=@self;

end;

{ MULIPLY_AF }
function trans_perspective.multiply_af(a : trans_affine_ptr ) : trans_perspective_ptr;
var
 b : trans_perspective;

begin
 b.Construct(trans_perspective_ptr(@self ) );

 sx :=a.m0 * b.sx  + a.m2 * b.shy + a.m4 * b.w0;
 shx:=a.m0 * b.shx + a.m2 * b.sy  + a.m4 * b.w1;
 tx :=a.m0 * b.tx  + a.m2 * b.ty  + a.m4 * b.w2;
 shy:=a.m1 * b.sx  + a.m3 * b.shy + a.m5 * b.w0;
 sy :=a.m1 * b.shx + a.m3 * b.sy  + a.m5 * b.w1;
 ty :=a.m1 * b.tx  + a.m3 * b.ty  + a.m5 * b.w2;

 result:=@self;

end;

{ PREMULTIPLY_AF }
function trans_perspective.premultiply_af(b : trans_affine_ptr ) : trans_perspective_ptr;
var
 a : trans_perspective;

begin
 a.Construct(trans_perspective_ptr(@self ) );

 sx :=a.sx  * b.m0 + a.shx * b.m1;
 shx:=a.sx  * b.m2 + a.shx * b.m3;
 tx :=a.sx  * b.m4 + a.shx * b.m5 + a.tx;
 shy:=a.shy * b.m0 + a.sy  * b.m1;
 sy :=a.shy * b.m2 + a.sy  * b.m3;
 ty :=a.shy * b.m4 + a.sy  * b.m5 + a.ty;
 w0 :=a.w0  * b.m0 + a.w1  * b.m1;
 w1 :=a.w0  * b.m2 + a.w1  * b.m3;
 w2 :=a.w0  * b.m4 + a.w1  * b.m5 + a.w2;

 result:=@self;

end;

{ MULTIPLY_INV_AF }
function trans_perspective.multiply_inv_af(m : trans_affine_ptr ) : trans_perspective_ptr;
var
 t : trans_affine;

begin
 t.Construct(m.m0 ,m.m1 ,m.m2 ,m.m3 ,m.m4 ,m.m5 );
 t.invert;

 result:=multiply_af(@t );

end;

{ PREMULTIPLY_INV_AF }
function trans_perspective.premultiply_inv_af(m : trans_affine_ptr ) : trans_perspective_ptr;
var
 t : trans_perspective;

begin
 t.Construct_af(m );
 t.invert;

 t.multiply(@self );

 Construct(trans_perspective_ptr(@t ) );

 result:=@self;

end;

{ STORE_TO }
procedure trans_perspective.store_to(m : quadrilateral_ptr );
begin
 m[0 ]:=sx;
 m[1 ]:=shy;
 m[2 ]:=w0;
 m[3 ]:=shx;
 m[4 ]:=sy;
 m[5 ]:=w1;
 m[6 ]:=tx;
 m[7 ]:=ty;
 m[8 ]:=w2;

end;

{ LOAD_FROM }
function trans_perspective.load_from(m : quadrilateral_ptr ) : trans_perspective_ptr;
begin
 sx :=m[0 ];
 shy:=m[1 ];
 w0 :=m[2 ];
 shx:=m[3 ];
 sy :=m[4 ];
 w1 :=m[5 ];
 tx :=m[6 ];
 ty :=m[7 ];
 w2 :=m[8 ];

end;

{ FROM_AFFINE }
function trans_perspective.from_affine(a : trans_affine_ptr ) : trans_perspective_ptr;
begin
 sx :=a.m0;
 shy:=a.m1;
 w0 :=0;
 shx:=a.m2;
 sy :=a.m3;
 w1 :=0;
 tx :=a.m4;
 ty :=a.m5;
 w2 :=1;

 result:=@self;

end;

{ DETERMINANT }
function trans_perspective.determinant : double;
begin
 result:=
  sx  * (sy  * w2 - ty  * w1 ) +
  shx * (ty  * w0 - shy * w2 ) +
  tx  * (shy * w1 - sy  * w0 );

end;

{ DETERMINANT_RECIPROCAL }
function trans_perspective.determinant_reciprocal : double;
begin
 result:=1.0 / determinant;

end;

{ IS_VALID }
function trans_perspective.is_valid(epsilon : double = affine_epsilon ) : boolean;
begin
 result:=
  (Abs(sx ) > epsilon ) and
  (Abs(sy ) > epsilon ) and
  (Abs(w2 ) > epsilon );

end;

{ IS_IDENTITY }
function trans_perspective.is_identity(epsilon : double = affine_epsilon ) : boolean;
begin
 result:=
  is_equal_eps(sx  ,1.0 ,epsilon ) and
  is_equal_eps(shy ,0.0 ,epsilon ) and
  is_equal_eps(w0  ,0.0 ,epsilon ) and
  is_equal_eps(shx ,0.0 ,epsilon ) and
  is_equal_eps(sy  ,1.0 ,epsilon ) and
  is_equal_eps(w1  ,0.0 ,epsilon ) and
  is_equal_eps(tx  ,0.0 ,epsilon ) and
  is_equal_eps(ty  ,0.0 ,epsilon ) and
  is_equal_eps(w2  ,1.0 ,epsilon );

end;

{ IS_EQUAL }
function trans_perspective.is_equal(m : trans_perspective_ptr; epsilon : double = affine_epsilon ) : boolean;
begin
 result:=
  is_equal_eps(sx  ,m.sx  ,epsilon ) and
  is_equal_eps(shy ,m.shy ,epsilon ) and
  is_equal_eps(w0  ,m.w0  ,epsilon ) and
  is_equal_eps(shx ,m.shx ,epsilon ) and
  is_equal_eps(sy  ,m.sy  ,epsilon ) and
  is_equal_eps(w1  ,m.w1  ,epsilon ) and
  is_equal_eps(tx  ,m.tx  ,epsilon ) and
  is_equal_eps(ty  ,m.ty  ,epsilon ) and
  is_equal_eps(w2  ,m.w2  ,epsilon );

end;

{ SCALE }
function trans_perspective.scale : double;
var
 x ,y : double;

begin
 x:=0.707106781 * sx  + 0.707106781 * shx;
 y:=0.707106781 * shy + 0.707106781 * sy;

 result:=Sqrt(x * x + y * y );

end;

{ ROTATION }
function trans_perspective.rotation : double;
var
 x1 ,y1 ,x2 ,y2 : double;

begin
 x1:=0.0;
 y1:=0.0;
 x2:=1.0;
 y2:=0.0;

 _transform(@self ,@x1 ,@y1 );
 _transform(@self ,@x2 ,@y2 );

 result:=ArcTan2(y2 - y1 ,x2 - x1 );

end;

{ TRANSLATION }
procedure trans_perspective.translation(dx ,dy : double_ptr );
begin
 dx^:=tx;
 dy^:=ty;

end;

{ SCALING }
procedure trans_perspective.scaling(x ,y : double_ptr );
var
 x1 ,y1 ,x2 ,y2 : double;

 t : trans_perspective;

 tar : trans_affine_rotation;

begin
 x1:= 0.0;
 y1:= 0.0;
 x2:= 1.0;
 y2:= 1.0;

 t.Construct  (trans_perspective_ptr(@self ) );
 tar.Construct(-rotation );
 t.multiply_af(@tar );

 t.transform(@t ,@x1 ,@y1 );
 t.transform(@t ,@x2 ,@y2 );

 x^:=x2 - x1;
 y^:=y2 - y1;

end;

{ SCALING_ABS }
procedure trans_perspective.scaling_abs(x ,y : double_ptr );
begin
 x^:=Sqrt(sx  * sx  + shx * shx );
 y^:=Sqrt(shy * shy + sy  * sy );

end;

{ BEGIN_ }
function trans_perspective.begin_(x ,y ,step : double ) : iterator_x;
begin
 result.Construct(x ,y ,step ,@self );

end;

END.

