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
// 31.10.2006-Milano: Unit port establishment
//
{ agg_line_aa_basics.pas }
unit
 agg_line_aa_basics ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ;

{ TYPES DEFINITION }
const
 line_subpixel_shift = 8;                         //----line_subpixel_shift
 line_subpixel_size  = 1 shl line_subpixel_shift; //----line_subpixel_size
 line_subpixel_mask  = line_subpixel_size - 1;    //----line_subpixel_mask

 line_mr_subpixel_shift = 4;                            //----line_mr_subpixel_shift
 line_mr_subpixel_size  = 1 shl line_mr_subpixel_shift; //----line_mr_subpixel_size
 line_mr_subpixel_mask  = line_mr_subpixel_size - 1;    //----line_mr_subpixel_mask

type
 line_parameters_ptr = ^line_parameters;
 line_parameters = object
   x1, y1, x2, y2, dx, dy, sx, sy : int;

   vertical : boolean;

   inc_ ,len ,octant : int;

   constructor Construct; overload;
   constructor Construct(x1_ ,y1_ ,x2_ ,y2_ ,len_ : int ); overload;

   function  orthogonal_quadrant : unsigned;
   function  diagonal_quadrant : unsigned;

   function  same_orthogonal_quadrant(lp : line_parameters_ptr ) : boolean;
   function  same_diagonal_quadrant  (lp : line_parameters_ptr ) : boolean;

  end;

{ GLOBAL PROCEDURES }
 function  line_mr(x : int ) : int;
 function  line_hr(x : int ) : int;

 function  line_dbl_hr(x : int ) : int;
 function  line_coord (x : double ) : int;

 procedure bisectrix(l1 ,l2 : line_parameters_ptr; x ,y : int_ptr );

 procedure fix_degenerate_bisectrix_start(lp : line_parameters_ptr; x ,y : int_ptr );
 procedure fix_degenerate_bisectrix_end  (lp : line_parameters_ptr; x ,y : int_ptr );


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
const
// The number of the octant is determined as a 3-bit value as follows:
// bit 0 = vertical flag
// bit 1 = sx < 0
// bit 2 = sy < 0
//
// [N] shows the number of the orthogonal quadrant
// <M> shows the number of the diagonal quadrant
//               <1>
//   [1]          |          [0]
//       . (3)011 | 001(1) .
//         .      |      .
//           .    |    .
//             .  |  .
//    (2)010     .|.     000(0)
// <2> ----------.+.----------- <0>
//    (6)110   .  |  .   100(4)
//           .    |    .
//         .      |      .
//       .        |        .
//         (7)111 | 101(5)
//   [2]          |          [3]
//               <3>
//                                                0 ,1 ,2 ,3 ,4 ,5 ,6 ,7
 s_orthogonal_quadrant : array[0..7 ] of int8u = (0 ,0 ,1 ,1 ,3 ,3 ,2 ,2 );
 s_diagonal_quadrant   : array[0..7 ] of int8u = (0 ,1 ,2 ,1 ,0 ,3 ,2 ,3 );

{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor line_parameters.Construct;
begin
end;

{ CONSTRUCT }
constructor line_parameters.Construct(x1_ ,y1_ ,x2_ ,y2_ ,len_ : int );
begin
 x1:=x1_;
 y1:=y1_;
 x2:=x2_;
 y2:=y2_;
 dx:=Abs(x2_ - x1_ );
 dy:=Abs(y2_ - y1_ );

 if x2_ > x1_ then
  sx:=1
 else
  sx:=-1;

 if y2_ > y1_ then
  sy:=1
 else
  sy:=-1;

 vertical:=dy >= dx;

 if vertical then
  inc_:=sy
 else
  inc_:=sx;

 len:=len_;

 octant:=(sy and 4 ) or (sx and 2 ) or int(vertical );

end;

{ ORTHOGONAL_QUADRANT }
function line_parameters.orthogonal_quadrant;
begin
 result:=s_orthogonal_quadrant[octant ];

end;

{ DIAGONAL_QUADRANT }
function line_parameters.diagonal_quadrant;
begin
 result:=s_diagonal_quadrant[octant ];

end;

{ SAME_ORTHOGONAL_QUADRANT }
function line_parameters.same_orthogonal_quadrant;
begin
 result:=s_orthogonal_quadrant[octant ] = s_orthogonal_quadrant[lp.octant ];

end;

{ SAME_DIAGONAL_QUADRANT }
function line_parameters.same_diagonal_quadrant;
begin
 result:=s_diagonal_quadrant[octant ] = s_diagonal_quadrant[lp.octant ];

end;

{ LINE_MR }
function line_mr;
begin
 result:=shr_int32(x ,line_subpixel_shift - line_mr_subpixel_shift ); 

end;

{ LINE_HR }
function line_hr;
begin
 result:=x shl (line_subpixel_shift - line_mr_subpixel_shift );

end;

{ LINE_DBL_HR }
function line_dbl_hr;
begin
 result:=x shl line_subpixel_shift;

end;

{ LINE_COORD }
function line_coord;
begin
 result:=trunc(x * line_subpixel_size );

end;

{ BISECTRIX }
procedure bisectrix;
var
 k ,tx ,ty ,dx ,dy : double;

begin
 k :=l2.len / l1.len;
 tx:=l2.x2 - (l2.x1 - l1.x1 ) * k;
 ty:=l2.y2 - (l2.y1 - l1.y1 ) * k;

//All bisectrices must be on the right of the line
//If the next point is on the left (l1 => l2.2)
//then the bisectix should be rotated by 180 degrees.
 if intdbl(l2.x2 - l2.x1 ) * intdbl(l2.y1 - l1.y1 ) <
    intdbl(l2.y2 - l2.y1 ) * intdbl(l2.x1 - l1.x1 ) + 100.0 then
  begin
   tx:=tx - ((tx - l2.x1 ) * 2.0 );
   ty:=ty - ((ty - l2.y1 ) * 2.0 );

  end;

// Check if the bisectrix is too short
 dx:=tx - l2.x1;
 dy:=ty - l2.y1;

 if trunc(Sqrt(dx * dx + dy * dy ) ) < line_subpixel_size then
  begin
   x^:=shr_int32(l2.x1 + l2.x1 + (l2.y1 - l1.y1 ) + (l2.y2 - l2.y1 ) ,1 );
   y^:=shr_int32(l2.y1 + l2.y1 - (l2.x1 - l1.x1 ) - (l2.x2 - l2.x1 ) ,1 );

   exit;

  end;

 x^:=trunc(tx );
 y^:=trunc(ty );

end;

{ FIX_DEGENERATE_BISECTRIX_START }
procedure fix_degenerate_bisectrix_start;
var
 d : int;

begin
 d:=trunc(
  (intdbl(x^ - lp.x2 ) * intdbl(lp.y2 - lp.y1 ) -
   intdbl(y^ - lp.y2 ) * intdbl(lp.x2 - lp.x1 ) ) / lp.len );

 if d < line_subpixel_size then
  begin
   x^:=lp.x1 + (lp.y2 - lp.y1 );
   y^:=lp.y1 - (lp.x2 - lp.x1 );

  end;

end;

{ FIX_DEGENERATE_BISECTRIX_END }
procedure fix_degenerate_bisectrix_end;
var
 d : int;

begin
 d:=trunc(
  (intdbl(x^ - lp.x2 ) * intdbl(lp.y2 - lp.y1 ) -
   intdbl(y^ - lp.y2 ) * intdbl(lp.x2 - lp.x1 ) ) / lp.len );

 if d < line_subpixel_size then
  begin
   x^:=lp.x2 + (lp.y2 - lp.y1 );
   y^:=lp.y2 - (lp.x2 - lp.x1 );

  end;

end;

END.

