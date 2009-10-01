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
// 23.06.2006-Milano: ptrcomp adjustments
// 06.02.2006-Milano: Unit port establishment
//
{ agg_simul_eq.pas }
unit
 agg_simul_eq ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ;

{ GLOBAL PROCEDURES }
 procedure swap_arrays   (a1 ,a2 : double_ptr; n : unsigned );
 function  matrix_pivot  (m : double_ptr; row ,Rows ,Cols : unsigned ) : int;
 function  simul_eq_solve(left ,right ,result_ : double_ptr; Size ,RightCols : unsigned ) : boolean;


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ SWAP_ARRAYS }
procedure swap_arrays;
var
 i   : unsigned;
 tmp : double;

begin
 i:=0;

 while i < n do
  begin
   tmp:=a1^;
   a1^:=a2^;
   a2^:=tmp;

   inc(ptrcomp(a1 ) ,sizeof(double ) );
   inc(ptrcomp(a2 ) ,sizeof(double ) );
   inc(i );

  end;

end;

{ MATRIX_PIVOT }
function matrix_pivot;
var
 i : unsigned;
 k : int;

 max_val ,tmp : double;

begin
 k:=row;

 max_val:=-1.0;

 i:=row;

 while i < Rows do
  begin
   tmp:=Abs(double_ptr(ptrcomp(m ) + (i * Cols + row ) * sizeof(double ) )^ );

   if (tmp > max_val ) and
      (tmp <> 0.0 ) then
    begin
     max_val:=tmp;

     k:=i;

    end;

   inc(i );

  end;

 if double_ptr(ptrcomp(m ) + (k * Cols + row ) * sizeof(double ) )^ = 0.0 then
  begin
   result:=-1;

   exit;

  end;

 if k <> row then
  begin
   swap_arrays(
    double_ptr(ptrcomp(m ) + k * Cols * sizeof(double ) ) ,
    double_ptr(ptrcomp(m ) + row * Cols * sizeof(double ) ) ,
    Cols );

   result:=k;

   exit;

  end;

 result:=0;

end;

{ SIMUL_EQ_SOLVE }
function simul_eq_solve;
var
 m : int;

 i , j , k ,adx : unsigned;

 a1  : double;
 tmp : double_ptr;

label
 return_false ,free ;

begin
// Alloc
 adx:=Size + RightCols;

 agg_getmem(pointer(tmp ) ,Size * adx * sizeof(double ) );

 for i:=0 to Size - 1 do
  begin
   for j:=0 to Size - 1 do
    double_ptr(ptrcomp(tmp ) + (i * adx + j ) * sizeof(double ) )^:=
     double_ptr(ptrcomp(left ) + (i * Size + j ) * sizeof(double ) )^;

   for j:=0 to RightCols - 1 do
    double_ptr(ptrcomp(tmp ) + (i * adx + Size + j ) * sizeof(double ) )^:=
     double_ptr(ptrcomp(right ) + (i * RightCols + j ) * sizeof(double ) )^;

  end;

 for k:=0 to Size - 1 do
  begin
   if matrix_pivot(tmp ,k ,Size ,Size + RightCols ) < 0 then
    goto return_false; // Singularity....

   a1:=double_ptr(ptrcomp(tmp ) + (k * adx + k ) * sizeof(double ) )^;
   j :=k;

   while j < Size + RightCols do
    begin
     double_ptr(ptrcomp(tmp ) + (k * adx + j ) * sizeof(double ) )^:=
      double_ptr(ptrcomp(tmp ) + (k * adx + j ) * sizeof(double ) )^ / a1;

     inc(j );

    end;

   i:=k + 1;

   while i < Size do
    begin
     a1:=double_ptr(ptrcomp(tmp ) + (i * adx + k ) * sizeof(double ) )^;
     j :=k;

     while j < Size + RightCols do
      begin
       double_ptr(ptrcomp(tmp ) + (i * adx + j ) * sizeof(double ) )^:=
        double_ptr(ptrcomp(tmp ) + (i * adx + j ) * sizeof(double ) )^ -
        a1 * double_ptr(ptrcomp(tmp ) + (k * adx + j ) * sizeof(double ) )^;

       inc(j );

      end;

     inc(i );

    end;

  end;

 for k:=0 to RightCols - 1 do
  begin
   m:=int(Size - 1 );

   while m >= 0 do
    begin
     double_ptr(ptrcomp(result_ ) + (m * RightCols + k ) * sizeof(double ) )^:=
      double_ptr(ptrcomp(tmp ) + (m * adx + Size + k ) * sizeof(double ) )^;

     j:=m + 1;

     while j < Size do
      begin
       double_ptr(ptrcomp(result_ ) + (m * RightCols + k ) * sizeof(double ) )^:=
        double_ptr(ptrcomp(result_ ) + (m * RightCols + k ) * sizeof(double ) )^ -
        (double_ptr(ptrcomp(tmp ) + (m * adx + j ) * sizeof(double ) )^ *
         double_ptr(ptrcomp(result_ ) + (j * RightCols + k ) * sizeof(double ) )^ );

       inc(j );

      end;

     dec(m );

    end;

  end;

// Done
 result:=true;

 goto free;

return_false:
 result:=false;

// Free
free:
 agg_freemem(pointer(tmp ) ,Size * adx * sizeof(double ) );

end;

END.

