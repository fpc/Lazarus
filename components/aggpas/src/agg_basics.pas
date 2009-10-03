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
// 23.06.2006-Milano: added ptrcomp type + ptrcomp adjustments
// 26.09.2005-Milano: Complete unit port
//
{ agg_basics.pas }
unit
 agg_basics ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }

uses
 Math ;

{ TYPES DEFINITION }
type

 agg_types = (

  int8_type ,
  int8u_type ,
  int16_type ,
  int16u_type ,
  int32_type ,
  int32u_type ,
  int64_type ,
  int64u_type

  );

 agg_type = (agg_int ,agg_unsigned ,agg_double ); 

 AGG_INT8  = shortint;
 AGG_INT8U = byte;

 AGG_INT16  = smallint;
 AGG_INT16U = word;

 AGG_INT32  = longint;
 AGG_INT32U = longword;

 AGG_INT64  = int64;
{$IFDEF FPC }
 AGG_INT64U = qword;
{$ELSE }
 AGG_INT64U = int64;
{$ENDIF }

 int8   = AGG_INT8;
 int8u  = AGG_INT8U;
 int16  = AGG_INT16;
 int16u = AGG_INT16U;
 int32  = AGG_INT32;
 int32u = AGG_INT32U;
 int64  = AGG_INT64;
 int64u = AGG_INT64U;

 int8_ptr   = ^int8;
 int8u_ptr  = ^int8u;
 int16_ptr  = ^int16;
 int16u_ptr = ^int16u;
 int32_ptr  = ^int32;
 int32u_ptr = ^int32u;
 int64_ptr  = ^int64;
 int64u_ptr = ^int64u;

 int8u_ptr_ptr = ^int8u_ptr;

 cover_type_ptr = ^cover_type;
 cover_type = byte;

 int_ptr = ^int;
 int = int32;

 unsigned_ptr = ^unsigned;
 unsigned = int32u;

 int8u_01_ptr = ^int8u_01;
 int8u_01 = array[0..1 ] of int8u;

 int16u_ = record
   low  ,
   high : int8u;

  end;

 int32_ = record
   low  ,
   high : int16;

  end;

 int32_int8u = record
   _0 ,_1 ,_2 ,_3 : int8u;

  end;

 int32u_ = record
   low  ,
   high : int16u;

  end;

{ To achive maximum compatiblity with older code, FPC doesn't change the size
  of predefined data types like integer, longint or word when changing from
  32 to 64 Bit. However, the size of a pointer is 8 bytes on a 64 bit
  architecture so constructs like longint(pointer(p)) are doomed to crash on
  64 bit architectures. However, to allow you to write portable code, the
  FPC system unit introduces the types PtrInt and PtrUInt which are signed
  and unsigned integer data types with the same size as a pointer.

  Keep in mind that the size change of the "pointer" type also affects record
  sizes. If you allocate records with fixed sizes, and not with new or with
  getmem (<x>,sizeof(<x>)), this will have to be fixed. }
// Pascal Pointer Computation Type
{$IFDEF CPU64 }
 ptrcomp = system.int64;

{$ELSE }
 ptrcomp = integer;

{$ENDIF }

// Pascal's pointer-in-an-array-access helper structures
 p32_ptr = ^p32;
 p32 = record
   case integer of
    1 : (ptr : pointer );

   {$IFDEF CPU64 }
    2 : (int : system.int64 );

   {$ELSE }
    2 : (int : integer );

   {$ENDIF }

  end;

 double_ptr_ptr = ^double_ptr;
 double_ptr = ^double;

 double_2_ptr = ^double_2;
 double_2     = array[0..1 ] of double;

 double_8_ptr = ^double_8;
 double_8     = array[0..7 ] of double;

 double_42_ptr = ^double_42;
 double_42     = array[0..3 ,0..1 ] of double;

 double_44_ptr = ^double_44;
 double_44     = array[0..3 ,0..3 ] of double;

 double_81_ptr = ^double_81;
 double_81     = array[0..7 ,0..0 ] of double;

 double_88_ptr = ^double_88;
 double_88     = array[0..7 ,0..7 ] of double;

 double_26_ptr = ^double_26;
 double_26     = array[0..25 ] of double;

 double_00_ptr = ^double_00;
 double_00     = array of double;

 char_ptr_ptr = ^char_ptr;
 char_ptr     = ^char;
 pointer_ptr  = ^pointer;

 gamma_ptr = ^gamma;
 gamma = object
   function dir(v : unsigned ) : unsigned; virtual; abstract;
   function inv(v : unsigned ) : unsigned; virtual; abstract;

  end;

 poly_subpixel_scale_e = int;

 filling_rule_e = (fill_non_zero ,fill_even_odd );
 
const
// cover_scale_e
 cover_shift = 8;
 cover_size  = 1 shl cover_shift;
 cover_mask  = cover_size - 1;
 cover_none  = 0;
 cover_full  = cover_mask;

 pi : double = 3.14159265358979323846;

// These constants determine the subpixel accuracy, to be more precise,
// the number of bits of the fractional part of the coordinates.
// The possible coordinate capacity in bits can be calculated by formula:
// sizeof(int) * 8 - poly_subpixel_shift, i.e, for 32-bit integers and
// 8-bits fractional part the capacity is 24 bits.
 poly_subpixel_shift = 8;                         //----poly_subpixel_shift
 poly_subpixel_scale = 1 shl poly_subpixel_shift; //----poly_subpixel_scale
 poly_subpixel_mask  = poly_subpixel_scale-1;     //----poly_subpixel_mask

// path_commands_e
 path_cmd_stop     = 0;
 path_cmd_move_to  = 1;
 path_cmd_line_to  = 2;
 path_cmd_curve3   = 3;
 path_cmd_curve4   = 4;
 path_cmd_curveN   = 5;
 path_cmd_catrom   = 6;
 path_cmd_ubspline = 7;
 path_cmd_end_poly = $0F;
 path_cmd_mask     = $0F;

// path_flags_e
 path_flags_none  = 0;
 path_flags_ccw   = $10;
 path_flags_cw    = $20;
 path_flags_close = $40;
 path_flags_mask  = $F0;

type
 rect_ptr = ^rect;
 rect = object
   x1 ,y1 ,x2 ,y2 : int;

   constructor Construct; overload;
   constructor Construct(x1_ ,y1_ ,x2_ ,y2_ : int ); overload;
   constructor Construct(r : rect_ptr ); overload;

   function  normalize : rect_ptr;
   function  clip(r : rect_ptr ) : boolean;
   function  is_valid : boolean;

  end;

 rect_d_ptr = ^rect_d;
 rect_d = object
   x1 ,y1 ,x2 ,y2 : double;

   constructor Construct; overload;
   constructor Construct(x1_ ,y1_ ,x2_ ,y2_ : double ); overload;

   function  normalize : rect_d_ptr;
   function  clip(r : rect_d_ptr ) : boolean;
   function  is_valid : boolean;

  end;

 rect_i_ptr = ^rect_i;
 rect_i = object
   x1 ,y1 ,x2 ,y2 : int;

   constructor Construct; overload;
   constructor Construct(x1_ ,y1_ ,x2_ ,y2_ : int ); overload;

   function  clip(r : rect_i_ptr ) : boolean;

  end;

 point_type_ptr = ^point_type;
 point_type = record
   x ,y : double;

  end;

 vertex_type = object
   x ,y : double;
   cmd  : byte;

   constructor Construct; overload;
   constructor Construct(x_ ,y_ : double; cmd_ : byte ); overload;

  end;

 unsigned_list_ptr = ^unsigned_list; 
 unsigned_list = object
   function  array_operator(idx : unsigned ) : unsigned; virtual; abstract;

  end;

{ GLOBAL PROCEDURES }
 function  agg_getmem (var buf : pointer; sz : unsigned ) : boolean;
 function  agg_freemem(var buf : pointer; sz : unsigned ) : boolean;

 function  deg2rad(deg : double ) : double;
 function  rad2deg(rad : double ) : double;

 procedure normalize_rect  (var this : rect );
 procedure normalize_rect_d(var this : rect_d );

 function  clip_rect  (var this : rect; r : rect_ptr ) : boolean;
 function  clip_rect_d(var this : rect_d; r : rect_d_ptr ) : boolean;

 function  is_valid_rect (var this : rect ) : boolean;
 function  is_valid_rect_d(var this : rect_d ) : boolean;

 function  intersect_rectangles (r1 ,r2 : rect_ptr ) : rect;
 function  intersect_rectangles_d(r1 ,r2 : rect_d_ptr ) : rect_d;

 function  unite_rectangles (r1 ,r2 : rect_ptr ) : rect;
 function  unite_rectangles_d(r1 ,r2 : rect_d_ptr ) : rect_d;

 function  is_vertex   (c : unsigned ) : boolean;
 function  is_drawing  (c : unsigned ) : boolean;
 function  is_stop     (c : unsigned ) : boolean;
 function  is_move     (c : unsigned ) : boolean;
 function  is_line_to  (c : unsigned ) : boolean;
 function  is_move_to  (c : unsigned ) : boolean;
 function  is_curve    (c : unsigned ) : boolean;
 function  is_curve3   (c : unsigned ) : boolean;
 function  is_curve4   (c : unsigned ) : boolean;
 function  is_end_poly (c : unsigned ) : boolean;
 function  is_close    (c : unsigned ) : boolean;
 function  is_next_poly(c : unsigned ) : boolean;
 function  is_cw       (c : unsigned ) : boolean;
 function  is_ccw      (c : unsigned ) : boolean;
 function  is_oriented (c : unsigned ) : boolean;
 function  is_closed   (c : unsigned ) : boolean;

 function  get_close_flag   (c : unsigned ) : unsigned;
 function  clear_orientation(c : unsigned ) : unsigned;
 function  get_orientation  (c : unsigned ) : unsigned;
 function  set_orientation  (c ,o : unsigned ) : unsigned;

 procedure swap_ptrs(a ,b : pointer );
 procedure sprintf  (dst : char_ptr; src : PChar; val : double );
 function  intdbl   (i : int ) : double;

 procedure srand_(seed : int );
 function  rand_ : int;

 procedure srand(seed : int );
 function  rand : int;

 function  uround(v : double ) : int;
 function  iround(v : double ) : int;

 function  saturation_iround(Limit : int; v : double ) : int;

// NoP = No Operation. It's the empty function, whose purpose is only for the
// debugging, or for the piece of code where intentionaly nothing is planned
// to be.
 procedure NoP;

// SHR for signed integers is differently implemented in pascal compilers
// than in c++ compilers. On the assembler level, c++ is using the SAR and
// pascal is using SHR. That gives completely different result, when the
// number is negative. We have to be compatible with c++ implementation,
// thus instead of directly using SHR we emulate c++ solution.
 function  shr_int8 (i ,shift : int8 ) : int8;
 function  shr_int16(i ,shift : int16 ) : int16;
 function  shr_int32(i ,shift : int ) : int;

IMPLEMENTATION
{ UNIT IMPLEMENTATION }
{ agg_getmem }
function agg_getmem;
begin
 result:=false;

 try
  getmem(buf ,sz );

  result:=true;

 except
  buf:=NIL;

 end;

end;

{ agg_freemem }
function agg_freemem;
begin
 if buf = NIL then
  result:=true

 else
  try
   freemem(buf ,sz );

   buf:=NIL;

   result:=true;

  except
   result:=false;

  end;

end;

{ deg2rad }
function deg2rad;
begin
 result:=deg * pi / 180;

end;

{ rad2deg }
function rad2deg;
begin
 result:=rad * 180 / pi;

end;

{ CONSTRUCT }
constructor rect.Construct;
begin
 x1:=0;
 y1:=0;
 x2:=0;
 y2:=0;

end;

{ CONSTRUCT }
constructor rect.Construct(x1_ ,y1_ ,x2_ ,y2_ : int );
begin
 x1:=x1_;
 y1:=y1_;
 x2:=x2_;
 y2:=y2_;

end;

{ CONSTRUCT }
constructor rect.Construct(r : rect_ptr );
begin
 x1:=r.x1;
 y1:=r.y1;
 x2:=r.x2;
 y2:=r.y2;

end;

{ NORMALIZE }
function rect.normalize;
var
 t : int;

begin
 if x1 > x2 then
  begin
   t :=x1;
   x1:=x2;
   x2:=t;

  end;

 if y1 > y2 then
  begin
   t :=y1;
   y1:=y2;
   y2:=t;

  end;

 result:=@self;

end;

{ CLIP }
function rect.clip;
begin
 if x2 > r.x2 then
  x2:=r.x2;

 if y2 > r.y2 then
  y2:=r.y2;

 if x1 < r.x1 then
  x1:=r.x1;

 if y1 < r.y1 then
  y1:=r.y1;

 result:=(x1 <= x2 ) and (y1 <= y2 );

end;

{ IS_VALID }
function rect.is_valid;
begin
 result:=(x1 <= x2 ) and (y1 <= y2 );

end;

{ CONSTRUCT }
constructor rect_d.Construct;
begin
 x1:=0;
 y1:=0;
 x2:=0;
 y2:=0;

end;

{ CONSTRUCT }
constructor rect_d.Construct(x1_ ,y1_ ,x2_ ,y2_ : double );
begin
 x1:=x1_;
 y1:=y1_;
 x2:=x2_;
 y2:=y2_;

end;

{ NORMALIZE }
function rect_d.normalize;
var
 t : double;

begin
 if x1 > x2 then
  begin
   t :=x1;
   x1:=x2;
   x2:=t;

  end;

 if y1 > y2 then
  begin
   t :=y1;
   y1:=y2;
   y2:=t;

  end;

 result:=@self;

end;

{ CLIP }
function rect_d.clip;
begin
 if x2 > r.x2 then
  x2:=r.x2;

 if y2 > r.y2 then
  y2:=r.y2;

 if x1 < r.x1 then
  x1:=r.x1;

 if y1 < r.y1 then
  y1:=r.y1;

 result:=(x1 <= x2 ) and (y1 <= y2 );

end;

{ IS_VALID }
function rect_d.is_valid;
begin
 result:=(x1 <= x2 ) and (y1 <= y2 );

end;

{ CONSTRUCT }
constructor rect_i.Construct;
begin
 x1:=0;
 y1:=0;
 x2:=0;
 y2:=0;

end;

{ CONSTRUCT }
constructor rect_i.Construct(x1_ ,y1_ ,x2_ ,y2_ : int );
begin
 x1:=x1_;
 y1:=y1_;
 x2:=x2_;
 y2:=y2_;

end;

{ CLIP }
function rect_i.clip(r : rect_i_ptr ) : boolean;
begin
 if x2 > r.x2 then
  x2:=r.x2;

 if y2 > r.y2 then
  y2:=r.y2;

 if x1 < r.x1 then
  x1:=r.x1;

 if y1 < r.y1 then
  y1:=r.y1;

 result:=(x1 <= x2 ) and (y1 <= y2 );

end;

{ CONSTRUCT }
constructor vertex_type.Construct;
begin
 x:=0;
 y:=0;

 cmd:=0;

end;

{ CONSTRUCT }
constructor vertex_type.Construct(x_ ,y_ : double; cmd_ : byte );
begin
 x:=x_;
 y:=y_;

 cmd:=cmd_;

end;

{ normalize_rect }
procedure normalize_rect(var this : rect );
var
 t : int;

begin
 if this.x1 > this.x2 then
  begin
   t      :=this.x1;
   this.x1:=this.x2;
   this.x2:=t;

  end;

 if this.y1 > this.y2 then
  begin
   t      :=this.y1;
   this.y1:=this.y2;
   this.y2:=t;

  end;

end;

{ normalize_rect_d }
procedure normalize_rect_d(var this : rect_d );
var
 t : double;

begin
 if this.x1 > this.x2 then
  begin
   t      :=this.x1;
   this.x1:=this.x2;
   this.x2:=t;

  end;

 if this.y1 > this.y2 then
  begin
   t      :=this.y1;
   this.y1:=this.y2;
   this.y2:=t;

  end;

end;

{ clip_rect }
function clip_rect(var this : rect; r : rect_ptr ) : boolean;
begin
 if this.x2 > r.x2 then
  this.x2:=r.x2;

 if this.y2 > r.y2 then
  this.y2:=r.y2;

 if this.x1 < r.x1 then
  this.x1:=r.x1;

 if this.y1 < r.y1 then
  this.y1:=r.y1;

 result:=(this.x1 <= this.x2 ) and (this.y1 <= this.y2 );

end;

{ clip_rect_d }
function clip_rect_d(var this : rect_d; r : rect_d_ptr ) : boolean;
begin
 if this.x2 > r.x2 then
  this.x2:=r.x2;

 if this.y2 > r.y2 then
  this.y2:=r.y2;

 if this.x1 < r.x1 then
  this.x1:=r.x1;

 if this.y1 < r.y1 then
  this.y1:=r.y1;

 result:=(this.x1 <= this.x2 ) and (this.y1 <= this.y2 );

end;

{ is_valid_rect }
function is_valid_rect(var this : rect ) : boolean;
begin
 result:=(this.x1 <= this.x2 ) and (this.y1 <= this.y2 );

end;

{ is_valid_rect_d }
function is_valid_rect_d(var this : rect_d ) : boolean;
begin
 result:=(this.x1 <= this.x2 ) and (this.y1 <= this.y2 );

end;

{ intersect_rectangles }
function intersect_rectangles(r1 ,r2 : rect_ptr ) : rect;
begin
 result:=r1^;

 if result.x2 > r2.x2 then
  result.x2:=r2.x2;

 if result.y2 > r2.y2 then
  result.y2:=r2.y2;

 if result.x1 < r2.x1 then
  result.x1:=r2.x1;

 if result.y1 < r2.y1 then
  result.y1:=r2.y1;

end;

{ intersect_rectangles_d }
function intersect_rectangles_d(r1 ,r2 : rect_d_ptr ) : rect_d;
begin
 result:=r1^;

 if result.x2 > r2.x2 then
  result.x2:=r2.x2;

 if result.y2 > r2.y2 then
  result.y2:=r2.y2;

 if result.x1 < r2.x1 then
  result.x1:=r2.x1;

 if result.y1 < r2.y1 then
  result.y1:=r2.y1;

end;

{ unite_rectangles }
function unite_rectangles(r1 ,r2 : rect_ptr ) : rect;
begin
 result:=r1^;

 if result.x2 < r2.x2 then
  result.x2:=r2.x2;

 if result.y2 < r2.y2 then
  result.y2:=r2.y2;

 if result.x1 > r2.x1 then
  result.x1:=r2.x1;

 if result.y1 > r2.y1 then
  result.y1:=r2.y1;

end;

{ unite_rectangles_d }
function unite_rectangles_d(r1 ,r2 : rect_d_ptr ) : rect_d;
begin
 result:=r1^;

 if result.x2 < r2.x2 then
  result.x2:=r2.x2;

 if result.y2 < r2.y2 then
  result.y2:=r2.y2;

 if result.x1 > r2.x1 then
  result.x1:=r2.x1;

 if result.y1 > r2.y1 then
  result.y1:=r2.y1;

end;

{ is_vertex }
function is_vertex;
begin
 result:=(c >= path_cmd_move_to ) and (c < path_cmd_end_poly );

end;

{ is_drawing }
function is_drawing;
begin
 result:=(c >= path_cmd_line_to ) and (c < path_cmd_end_poly );

end;

{ is_stop }
function is_stop;
begin
 result:=(c = path_cmd_stop );

end;

{ is_move }
function is_move;
begin
 result:=(c = path_cmd_move_to );

end;

{ is_line_to }
function is_line_to;
begin
 result:=(c = path_cmd_line_to );

end;

{ is_move_to }
function is_move_to;
begin
 result:=(c = path_cmd_move_to );

end;

{ is_curve }
function is_curve;
begin
 result:=(c = path_cmd_curve3 ) or (c = path_cmd_curve4 );

end;

{ is_curve3 }
function is_curve3;
begin
 result:=(c = path_cmd_curve3 );

end;

{ is_curve4 }
function is_curve4;
begin
 result:=(c = path_cmd_curve4 );

end;

{ is_end_poly }
function is_end_poly;
begin
 result:=((c and path_cmd_mask ) = path_cmd_end_poly );

end;

{ is_close }
function is_close;
begin
 result:=
  (c and not(path_flags_cw or path_flags_ccw ) ) =
  (path_cmd_end_poly or path_flags_close )

end;

{ is_next_poly }
function is_next_poly;
begin
 result:=is_stop(c ) or is_move_to(c ) or is_end_poly(c );

end;

{ is_cw }
function is_cw;
begin
 result:=not((c and path_flags_cw ) = 0 );

end;

{ is_ccw }
function is_ccw;
begin
 result:=not((c and path_flags_ccw ) = 0 );

end;

{ is_oriented }
function is_oriented;
begin
 result:=not((c and (path_flags_cw or path_flags_ccw ) ) = 0 );

end;

{ is_closed }
function is_closed;
begin
 result:=not((c and path_flags_close ) = 0 );

end;

{ get_close_flag }
function get_close_flag;
begin
 result:=c and path_flags_close;

end;

{ clear_orientation }
function clear_orientation;
begin
 result:=c and not(path_flags_cw or path_flags_ccw );

end;

{ get_orientation }
function get_orientation;
begin
 result:=c and (path_flags_cw or path_flags_ccw );

end;

{ set_orientation }
function set_orientation;
begin
 result:=clear_orientation(c ) or o;

end;

{ swap_ptrs }
procedure swap_ptrs;
var
 temp : pointer;

begin
 temp:=p32_ptr(a ).ptr;

 p32_ptr(a ).ptr:=p32_ptr(b ).ptr;
 p32_ptr(b ).ptr:=temp;

end;

{ MAKESTR }
function MakeStr(ch : char; sz : byte ) : shortstring;
begin
 result[0 ]:=char(sz );

 fillchar(result[1 ] ,sz ,ch );

end;

{ BACKLEN }
function BackLen(s : shortstring; sz : byte ) : shortstring;
type
 tSCAN = (

  SCAN_0 ,
  SCAN_1 ,SCAN_2 ,SCAN_3 ,SCAN_4 ,SCAN_5 ,SCAN_6 ,SCAN_7 ,SCAN_8 ,SCAN_9 ,
  SCAN_A ,SCAN_B ,SCAN_C ,SCAN_D ,SCAN_E ,SCAN_F ,SCAN_G ,SCAN_H ,SCAN_I ,
  SCAN_J ,SCAN_K ,SCAN_L ,SCAN_M ,SCAN_N ,SCAN_O ,SCAN_P ,SCAN_Q ,SCAN_R ,
  SCAN_S ,SCAN_T ,SCAN_U ,SCAN_V ,SCAN_W ,SCAN_X ,SCAN_Y ,SCAN_Z

  );

var
 pos ,
 wcb : byte;
 scn : tSCAN;

begin
 result:='';

 wcb:=sz;
 pos:=length(s );
 scn:=SCAN_1;

 while wcb > 0 do
  begin
   case scn of
    SCAN_1 :
     if pos > 0 then
      begin
       result:=s[pos ] + result;

       dec(pos );

      end
     else
      begin
       scn:=SCAN_2;

       result:=' ' + result;

      end;

    SCAN_2 :
     result:=' ' + result;

   end;

   dec(wcb );

  end;

end;

{ INTHEX }
function IntHex(i : int64; max : byte = 0; do_low : boolean = false ) : shortstring;
type
 tITEM = (

  ITEM_0 ,
  ITEM_1 ,ITEM_2 ,ITEM_3 ,ITEM_4 ,ITEM_5 ,ITEM_6 ,ITEM_7 ,ITEM_8 ,ITEM_9 ,
  ITEM_A ,ITEM_B ,ITEM_C ,ITEM_D ,ITEM_E ,ITEM_F ,ITEM_G ,ITEM_H ,ITEM_I ,
  ITEM_J ,ITEM_K ,ITEM_L ,ITEM_M ,ITEM_N ,ITEM_O ,ITEM_P ,ITEM_Q ,ITEM_R ,
  ITEM_S ,ITEM_T ,ITEM_U ,ITEM_V ,ITEM_W ,ITEM_X ,ITEM_Y ,ITEM_Z

  );

var
 str : shortstring;
 itm : tITEM;
 fcb : byte;

const
 low : array[0..$f ] of char = '0123456789abcdef';
 hex : array[0..$f ] of char = '0123456789ABCDEF';

begin
 if do_low then
  str:=
   low[i shr 60 and 15 ] +
   low[i shr 56 and 15 ] +
   low[i shr 52 and 15 ] +
   low[i shr 48 and 15 ] +
   low[i shr 44 and 15 ] +
   low[i shr 40 and 15 ] +
   low[i shr 36 and 15 ] +
   low[i shr 32 and 15 ] +

   low[i shr 28 and 15 ] +
   low[i shr 24 and 15 ] +
   low[i shr 20 and 15 ] +
   low[i shr 16 and 15 ] +
   low[i shr 12 and 15 ] +
   low[i shr 8 and 15 ] +
   low[i shr 4 and 15 ] +
   low[i and 15 ]
 else
  str:=
   hex[i shr 60 and 15 ] +
   hex[i shr 56 and 15 ] +
   hex[i shr 52 and 15 ] +
   hex[i shr 48 and 15 ] +
   hex[i shr 44 and 15 ] +
   hex[i shr 40 and 15 ] +
   hex[i shr 36 and 15 ] +
   hex[i shr 32 and 15 ] +

   hex[i shr 28 and 15 ] +
   hex[i shr 24 and 15 ] +
   hex[i shr 20 and 15 ] +
   hex[i shr 16 and 15 ] +
   hex[i shr 12 and 15 ] +
   hex[i shr 8 and 15 ] +
   hex[i shr 4 and 15 ] +
   hex[i and 15 ];

 if max > 0 then
  if length(str ) > max then
   result:=BackLen(str ,max )
  else
   if length(str ) < max then
    result:=MakeStr('0' ,max - length(str ) ) + str
   else
    result:=str

 else
  begin
   result:='';

   itm:=ITEM_1;

   for fcb:=1 to length(str ) do
    case itm of
     ITEM_1 :
      case str[fcb ] of
       '0' :
       else
        begin
         result:=str[fcb ];

         itm:=ITEM_2;

        end;

      end;

     ITEM_2 :
      result:=result + str[fcb ];

    end;

   if result = '' then
    result:='0';

  end;

end;

{ SPRINTF }
procedure sprintf;
type
 scan = (_string ,_flags ,_width ,_precision ,_prefix ,_type );

var
 sc : scan;
 nt ,
 fr : integer;

 get : shortstring;
 flg : char;
 dth ,
 prc ,
 err : integer;
 prf : array[0..3 ] of char;
 typ : char;

{ apply }
procedure apply;
var
 i ,x : int;

 add : shortstring;

begin
 add:='';

 case typ of
  'X' :
   begin
    if dth = 1 then
     dth:=0;

    add:=IntHex(trunc(val ) ,dth ,false );

   end;

  'x' :
   begin
    if dth = 1 then
     dth:=0;

    add:=IntHex(trunc(val ) ,dth ,true );

   end;

  's' :
   add:=PChar(trunc(val ) );

  'u' ,'d' :
   begin
    str(nt ,get );

    while length(get ) < dth do
     get:='0' + get;

    add:=get;

   end;

  'f' :
   begin
    str(nt ,get );

    while length(get ) < dth do
     get:=' ' + get;

    add:=get;

    if prc > 0 then
     begin
      x:=1;

      for i:=1 to prc do
       x:=x * 10;

      fr:=Abs(trunc(system.frac(val ) * x ) );

      str(fr ,get );

      while length(get ) < prc do
       get:='0' + get;

      add:=add + '.' + get;

     end;

    if (val < 0 ) and
       (add[1 ] <> '-' ) then
     add:='-' + add; 

   end;

 end;

 err:=0;

 while err < length(add ) do
  begin
   dst^:=add[err + 1 ];

   inc(ptrcomp(dst ) );
   inc(err );

  end;

 sc:=_string;

end;

begin
 nt:=trunc(system.int (val ) );
 fr:=trunc(system.frac(val ) );
 sc:=_string;

 flg:=#0;
 dth:=1;
 prc:=0;
 prf:=#0;
 typ:='s';

 while src^ <> #0 do
  begin
   case sc of
   { Copy Text or expect % }
    _string :
     case src^ of
      '%' :
       sc:=_flags;

      else
       begin
        dst^:=src^;

        inc(ptrcomp(dst ) );

       end;

     end;

   { Flags }
    _flags :
     case src^ of
      '-' ,'+' ,'0' ,' ' ,'#' :
       flg:=src^;

      '1'..'9' :
       begin
        get:=src^;
        sc :=_width;

       end;

      '.' :
       begin
        get:='';
        sc :=_precision;

       end;

      'h' ,'l' :
       begin
        prf[0 ]:=src^;
        prf[3 ]:=#1;

        sc:=_type;

       end;

      'I' :
       begin
        prf[0 ]:=src^;
        prf[3 ]:=#1;

        sc:=_prefix;

       end;

      'c' ,'C' ,'d' ,'i' ,'o' ,'u' ,'x' ,'X' ,'e' ,'E' ,'f' ,'g' ,'G' ,'n' ,'p' ,'s' ,'S' :
       begin
        typ:=src^;

        apply;

       end;

      else
       begin
        dst^:=src^;

        inc(ptrcomp(dst ) );

        sc:=_string;

       end;

     end;

   { Width }
    _width :
     case src^ of
      '0' ,'1'..'9' :
       get:=get + src^;

      else
       begin
        system.val(get ,dth ,err );

        case src^ of
         '.' :
          begin
           get:='';
           sc :=_precision;

          end;

         'h' ,'l' :
          begin
           prf[0 ]:=src^;
           prf[3 ]:=#1;

           sc:=_type;

          end;

         'I' :
          begin
           prf[0 ]:=src^;
           prf[3 ]:=#1;

           sc:=_prefix;

          end;

         'c' ,'C' ,'d' ,'i' ,'o' ,'u' ,'x' ,'X' ,'e' ,'E' ,'f' ,'g' ,'G' ,'n' ,'p' ,'s' ,'S' :
          begin
           typ:=src^;

           apply;

          end;

         else
          sc:=_string;

        end;

       end;

     end;

   { Precision }
    _precision :
     case src^ of
      '0' ,'1'..'9' :
       get:=get + src^;

      else
       begin
        system.val(get ,prc ,err );

        case src^ of
         'h' ,'l' :
          begin
           prf[0 ]:=src^;
           prf[3 ]:=#1;

           sc:=_type;

          end;

         'I' :
          begin
           prf[0 ]:=src^;
           prf[3 ]:=#1;

           sc:=_prefix;

          end;

         'c' ,'C' ,'d' ,'i' ,'o' ,'u' ,'x' ,'X' ,'e' ,'E' ,'f' ,'g' ,'G' ,'n' ,'p' ,'s' ,'S' :
          begin
           typ:=src^;

           apply;

          end;

         else
          sc:=_string;

        end;

       end;

     end;

   { Prefix }
    _prefix :
     if prf[3 ] = #1 then
      case src^ of
       '3' ,'6' :
        begin
         prf[1 ]:=src^;
         prf[3 ]:=#2;

        end;

       'c' ,'C' ,'d' ,'i' ,'o' ,'u' ,'x' ,'X' ,'e' ,'E' ,'f' ,'g' ,'G' ,'n' ,'p' ,'s' ,'S' :
        begin
         typ:=src^;

         apply;

        end;

       else
        sc:=_string;

      end
     else
      if prf[3 ] = #2 then
       case src^ of
        '2' ,'4' :
         begin
          prf[2 ]:=src^;
          prf[3 ]:=#3;

          sc:=_type;

         end;

        'c' ,'C' ,'d' ,'i' ,'o' ,'u' ,'x' ,'X' ,'e' ,'E' ,'f' ,'g' ,'G' ,'n' ,'p' ,'s' ,'S' :
         begin
          typ:=src^;

          apply;

         end;

        else
         sc:=_string;

       end
      else
       sc:=_string;

   { Type }
    _type :
     case src^ of
      'c' ,'C' ,'d' ,'i' ,'o' ,'u' ,'x' ,'X' ,'e' ,'E' ,'f' ,'g' ,'G' ,'n' ,'p' ,'s' ,'S' :
       begin
        typ:=src^;

        apply;

       end;

      else
       sc:=_string;

     end;

   end;

   inc(ptrcomp(src ) );

  end;

 dst^:=#0;

end;

{ INTDBL }
function intdbl;
begin
 result:=i;

end;

{ SRAND_ }
procedure srand_(seed : int );
begin
 system.RandSeed:=seed;

end;

{ RAND_ }
// Generates a pseudorandom number
function rand_ : int;
begin
 result:=system.Random($7fff )

end;

var
 g_holdrand : int = 1;

{ SRAND }
procedure srand(seed : int );
begin
 g_holdrand:=seed;

end;

{ RAND }
function rand : int;
begin
 g_holdrand:=g_holdrand * 214013 + 2531011;

 result:=(shr_int32(g_holdrand ,16 ) and $7fff );

end;

{ UROUND }
function uround(v : double ) : int;
begin
 result:=unsigned(Trunc(v + 0.5 ) );

end;

{ IROUND }
function iround(v : double ) : int;
begin
 if v < 0.0 then
  result:=int(Trunc(v - 0.5 ) )
 else
  result:=int(Trunc(v + 0.5 ) );

end;

{ SATURATION_IROUND }
function saturation_iround(Limit : int; v : double ) : int;
begin
 if v < -Limit then
  result:=-Limit
 else
  if v > Limit then
   result:=Limit
  else
   result:=iround(v );

end;

{ NoP }
procedure NoP;
begin
end;

{ SHR_INT8 }
function shr_int8;
begin
{$IFDEF AGG_CPU_386 }
 asm
  mov al ,byte ptr [i ]
  mov cl ,byte ptr [shift ]
  sar al ,cl
  mov byte ptr [result ] ,al

 end;

{$ENDIF }

{$IFDEF AGG_CPU_PPC }
 asm
  lbz   r2,i
  extsb r2,r2
  lbz   r3,shift
  extsb r3,r3
  sraw  r2,r2,r3
  extsb r2,r2
  stb   r2,result
	
 end;

{$ENDIF }

end;

{ SHR_INT16 }
function shr_int16;
begin
{$IFDEF AGG_CPU_386 }
 asm
  mov ax ,word ptr [i ]
  mov cx ,word ptr [shift ]
  sar ax ,cl
  mov word ptr [result ] ,ax

 end;

{$ENDIF }

{$IFDEF AGG_CPU_PPC }
 asm
  lha   r2,i
  lha   r3,shift
  sraw  r2,r2,r3
  extsh r2,r2
  sth   r2,result

 end;

{$ENDIF }

end;

{ SHR_INT32 }
function shr_int32;
begin
{$IFDEF AGG_CPU_386 }
 asm
  mov eax ,dword ptr [i ]
  mov ecx ,dword ptr [shift ]
  sar eax ,cl
  mov dword ptr [result ] ,eax

 end;

{$ENDIF }

{$IFDEF AGG_CPU_PPC }
 asm
  lwz  r3,i
  lwz  r2,shift
  sraw r3,r3,r2
  stw  r3,result

 end;

{$ENDIF }

end;

END.


