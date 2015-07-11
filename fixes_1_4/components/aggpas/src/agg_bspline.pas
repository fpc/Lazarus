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
// class bspline
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 19.01.2006-Milano: Complete Unit Port
// 18.01.2006-Milano: Unit port establishment
//
{ agg_bspline.pas }
unit
 agg_bspline ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ;

{ TYPES DEFINITION }
type
//----------------------------------------------------------------bspline
// A very simple class of Bi-cubic Spline interpolation.
// First call init(num, x[], y[]) where num - number of source points,
// x, y - arrays of X and Y values respectively. Here Y must be a function
// of X. It means that all the X-coordinates must be arranged in the ascending
// order.
// Then call get(x) that calculates a value Y for the respective X.
// The class supports extrapolation, i.e. you can call get(x) where x is
// outside the given with init() X-range. Extrapolation is a simple linear
// function.
 bspline = object
   m_max ,
   m_num : int;

   m_x  ,
   m_y  ,
   m_am : double_ptr;

   m_last_idx : int;

   constructor Construct; overload;
   constructor Construct(num : int ); overload;
   constructor Construct(num : int; x ,y : double_ptr ); overload;
   destructor  Destruct;

   procedure init(max : int ); overload;
   procedure init(num : int; x ,y : double_ptr ); overload;

   procedure add_point(x ,y : double );
   procedure prepare;

   function  get         (x : double ) : double;
   function  get_stateful(x : double ) : double;

   procedure bsearch(n : int; x : double_ptr; x0 : double; i : int_ptr );

   function  extrapolation_left (x : double ) : double;
   function  extrapolation_right(x : double ) : double;

   function  interpolation(x : double; i : int ) : double;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor bspline.Construct;
begin
 m_max:=0;
 m_num:=0;

 m_x :=NIL;
 m_y :=NIL;
 m_am:=NIL;

 m_last_idx:=-1;

end;

{ CONSTRUCT }
constructor bspline.Construct(num : int );
begin
 Construct;

 init(num );

end;

{ CONSTRUCT }
constructor bspline.Construct(num : int; x ,y : double_ptr );
begin
 Construct;

 init(num ,x ,y );

end;

{ DESTRUCT }
destructor bspline.Destruct;
begin
 agg_freemem(pointer(m_am ) ,m_max * 3 * sizeof(double ) );

end;

{ INIT }
procedure bspline.init(max : int );
begin
 if (max > 2 ) and
    (max > m_max ) then
  begin
   agg_freemem(pointer(m_am ) ,m_max * 3 * sizeof(double ) );
   agg_getmem (pointer(m_am ) ,max * 3 * sizeof(double ) );

   m_max:=max;

   m_x:=double_ptr(ptrcomp(m_am ) + m_max * sizeof(double ) );
   m_y:=double_ptr(ptrcomp(m_am ) + m_max * 2 * sizeof(double ) );

  end;

 m_num     :=0;
 m_last_idx:=-1;

end;

{ INIT }
procedure bspline.init(num : int; x ,y : double_ptr );
var
 i : int;

begin
 if num > 2 then
  begin
   init(num );

   for i:=0 to num - 1 do
    begin
     add_point(x^ ,y^ );

     inc(ptrcomp(x ) ,sizeof(double ) );
     inc(ptrcomp(y ) ,sizeof(double ) );

    end;

   prepare;

  end;

 m_last_idx:=-1;

end;

{ ADD_POINT }
procedure bspline.add_point;
begin
 if m_num < m_max then
  begin
   double_ptr(ptrcomp(m_x ) + m_num * sizeof(double ) )^:=x;
   double_ptr(ptrcomp(m_y ) + m_num * sizeof(double ) )^:=y;

   inc(m_num );

  end;

end;

{ PREPARE }
procedure bspline.prepare;
var
 i ,k ,n1 ,sz   : int;
 temp ,r ,s ,al : double_ptr;
 h ,p ,d ,f ,e  : double;

begin
 if m_num > 2 then
  begin
   for k:=0 to m_num - 1 do
    double_ptr(ptrcomp(m_am ) + k * sizeof(double ) )^:=0;

   n1:=3 * m_num;
   sz:=n1;

   agg_getmem(pointer(al ) ,n1 * sizeof(double ) );

   temp:=al;

   for k:=0 to n1 - 1 do
    double_ptr(ptrcomp(temp ) + k * sizeof(double ) )^:=0;

   r:=double_ptr(ptrcomp(temp ) + m_num * sizeof(double ) );
   s:=double_ptr(ptrcomp(temp ) + m_num * 2 * sizeof(double ) );

   n1:=m_num - 1;
   d :=double_ptr(ptrcomp(m_x ) + sizeof(double ) )^ - m_x^;
   e :=(double_ptr(ptrcomp(m_y ) + sizeof(double ) )^ - m_y^ ) / d;

   k:=1;

   while k < n1 do
    begin
     h:=d;
     d:=double_ptr(ptrcomp(m_x ) + (k + 1 ) * sizeof(double ) )^ - double_ptr(ptrcomp(m_x ) + k * sizeof(double ) )^;
     f:=e;
     e:=(double_ptr(ptrcomp(m_y ) + (k + 1 ) * sizeof(double ) )^ - double_ptr(ptrcomp(m_y ) + k * sizeof(double ) )^ ) / d;

     double_ptr(ptrcomp(al ) + k * sizeof(double ) )^:=d / (d + h );
     double_ptr(ptrcomp(r ) + k * sizeof(double ) )^ :=1.0 - double_ptr(ptrcomp(al ) + k * sizeof(double ) )^;
     double_ptr(ptrcomp(s ) + k * sizeof(double ) )^ :=6.0 * (e - f ) / (h + d );

     inc(k );

    end;

   k:=1;

   while k < n1 do
    begin
     p:=1.0 / (double_ptr(ptrcomp(r ) + k * sizeof(double ) )^ * double_ptr(ptrcomp(al ) + (k - 1 ) * sizeof(double ) )^ + 2.0 );

     double_ptr(ptrcomp(al ) + k * sizeof(double ) )^:=
      double_ptr(ptrcomp(al ) + k * sizeof(double ) )^ * -p;

     double_ptr(ptrcomp(s ) + k * sizeof(double ) )^ :=
      (double_ptr(ptrcomp(s ) + k * sizeof(double ) )^ -
       double_ptr(ptrcomp(r ) + k * sizeof(double ) )^ *
       double_ptr(ptrcomp(s ) + (k - 1 ) * sizeof(double ) )^ ) * p;

     inc(k );

    end;

   double_ptr(ptrcomp(m_am ) + n1 * sizeof(double ) )^:=0.0;

   double_ptr(ptrcomp(al ) + (n1 - 1 ) * sizeof(double ) )^:=
    double_ptr(ptrcomp(s ) + (n1 - 1 ) * sizeof(double ) )^;

   double_ptr(ptrcomp(m_am ) + (n1 - 1 ) * sizeof(double ) )^:=
    double_ptr(ptrcomp(al ) + (n1 - 1 ) * sizeof(double ) )^;

   k:=n1 - 2;
   i:=0;

   while i < m_num - 2 do
    begin
     double_ptr(ptrcomp(al ) + k * sizeof(double ) )^:=
      double_ptr(ptrcomp(al ) + k * sizeof(double ) )^ *
      double_ptr(ptrcomp(al ) + (k + 1 ) * sizeof(double ) )^ +
      double_ptr(ptrcomp(s ) + k * sizeof(double ) )^;

     double_ptr(ptrcomp(m_am ) + k * sizeof(double ) )^:=
      double_ptr(ptrcomp(al ) + k * sizeof(double ) )^;

     inc(i );
     dec(k );

    end;

   agg_freemem(pointer(al ) ,sz * sizeof(double ) );

  end;

 m_last_idx:=-1;

end;

{ GET }
function bspline.get;
var
 i : int;

begin
 if m_num > 2 then
  begin
  // Extrapolation on the left
   if x < m_x^  then
    begin
     result:=extrapolation_left(x );

     exit;

    end;

  // Extrapolation on the right
   if x >= double_ptr(ptrcomp(m_x ) + (m_num - 1 ) * sizeof(double ) )^ then
    begin
     result:=extrapolation_right(x );

     exit;

    end;

  // Interpolation
   bsearch(m_num ,m_x ,x ,@i );

   result:=interpolation(x ,i );

   exit;

  end;

 result:=0.0; 

end;

{ GET_STATEFUL }
function bspline.get_stateful;
begin
 if m_num > 2 then
  begin
  // Extrapolation on the left
   if x < m_x^ then
    begin
     result:=extrapolation_left(x );

     exit;

    end;

  // Extrapolation on the right
   if x >= double_ptr(ptrcomp(m_x ) + (m_num - 1 ) * sizeof(double ) )^ then
    begin
     result:=extrapolation_right(x );

     exit;

    end;

   if m_last_idx >= 0 then
    begin
    // Check if x is not in current range
     if (x < double_ptr(ptrcomp(m_x ) + m_last_idx * sizeof(double ) )^ ) or
        (x > double_ptr(ptrcomp(m_x ) + (m_last_idx + 1 ) * sizeof(double ) )^ ) then
      // Check if x between next points (most probably)
       if (m_last_idx < m_num - 2 ) and
          (x >= double_ptr(ptrcomp(m_x ) + (m_last_idx + 1 ) * sizeof(double ) )^ ) and
          (x <= double_ptr(ptrcomp(m_x ) + (m_last_idx + 2 ) * sizeof(double ) )^ ) then
        inc(m_last_idx )
       else
        if (m_last_idx > 0 ) and
           (x >= double_ptr(ptrcomp(m_x ) + (m_last_idx - 1 ) * sizeof(double ) )^ ) and
           (x <= double_ptr(ptrcomp(m_x ) + m_last_idx * sizeof(double ) )^ ) then
        // x is between pevious points
         dec(m_last_idx )
        else
        // Else perform full search
         bsearch(m_num ,m_x ,x ,@m_last_idx );

     result:=interpolation(x ,m_last_idx );

     exit;

    end
   else
    begin
    // Interpolation
     bsearch(m_num ,m_x ,x ,@m_last_idx );

     result:=interpolation(x ,m_last_idx );

     exit;

    end;

  end;

 result:=0.0;

end;

{ BSEARCH }
procedure bspline.bsearch;
var
 j ,k : int;

begin
 j :=n - 1;
 i^:=0;

 while j - i^ > 1 do
  begin
   k:=shr_int32(i^ + j ,1 );

   if x0 < double_ptr(ptrcomp(x ) + k * sizeof(double ) )^ then
    j:=k
   else
    i^:=k;

  end;

end;

{ EXTRAPOLATION_LEFT }
function bspline.extrapolation_left;
var
 d : double;

begin
 d:=double_ptr(ptrcomp(m_x ) + sizeof(double ) )^ - m_x^;

 result:=
  (-d * double_ptr(ptrcomp(m_am ) + sizeof(double ) )^ / 6 +
   (double_ptr(ptrcomp(m_y ) + sizeof(double ) )^ - m_y^ ) / d ) *
  (x - m_x^ ) + m_y^;
  
end;

{ EXTRAPOLATION_RIGHT }
function bspline.extrapolation_right;
var
 d : double;

begin
 d:=
  double_ptr(ptrcomp(m_x ) + (m_num - 1 ) * sizeof(double ) )^ -
  double_ptr(ptrcomp(m_x ) + (m_num - 2 ) * sizeof(double ) )^;

 result:=
  (d * double_ptr(ptrcomp(m_am ) + (m_num - 2 ) * sizeof(double ) )^ / 6 +
   (double_ptr(ptrcomp(m_y ) + (m_num - 1 ) * sizeof(double ) )^ -
    double_ptr(ptrcomp(m_y ) + (m_num - 2 ) * sizeof(double ) )^ ) / d ) *
  (x - double_ptr(ptrcomp(m_x ) + (m_num - 1 ) * sizeof(double ) )^ ) +
  double_ptr(ptrcomp(m_y ) + (m_num - 1 ) * sizeof(double ) )^;

end;

{ INTERPOLATION }
function bspline.interpolation;
var
 j : int;

 d ,h ,r ,p : double;

begin
 j:=i + 1;
 d:=double_ptr(ptrcomp(m_x ) + i * sizeof(double ) )^ - double_ptr(ptrcomp(m_x ) + j * sizeof(double ) )^;
 h:=x - double_ptr(ptrcomp(m_x ) + j * sizeof(double ) )^;
 r:=double_ptr(ptrcomp(m_x ) + i * sizeof(double ) )^ - x;
 p:=d * d / 6.0;

 result:=
  (double_ptr(ptrcomp(m_am ) + j * sizeof(double ) )^ * r * r * r +
   double_ptr(ptrcomp(m_am ) + i * sizeof(double ) )^ * h * h * h ) / 6.0 / d +
  ((double_ptr(ptrcomp(m_y ) + j * sizeof(double ) )^ -
    double_ptr(ptrcomp(m_am ) + j * sizeof(double ) )^ * p ) * r +
   (double_ptr(ptrcomp(m_y ) + i * sizeof(double ) )^ -
    double_ptr(ptrcomp(m_am ) + i * sizeof(double ) )^ * p ) * h ) / d;

end;

END.

