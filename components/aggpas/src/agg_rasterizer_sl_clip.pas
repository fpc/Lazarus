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
// 16.10.2007-Milano: Finished OK
// 15.10.2007-Milano: Unit port establishment
//
{ agg_rasterizer_sl_clip.pas }
unit
 agg_rasterizer_sl_clip ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_clip_liang_barsky ,
 agg_rasterizer_cells_aa ;

{ GLOBAL VARIABLES & CONSTANTS }
const
 poly_max_coord = (1 shl 30 ) - 1; //----poly_max_coord

 max_stack = 4;

{ TYPES DEFINITION }
type
 poly_max_coord_e = int;

 ras_conv_ptr = ^ras_conv;
 ras_conv = object
   function  mul_div(a ,b ,c : double ) : pointer; virtual; abstract;

   function  xi(v : pointer ) : int; virtual; abstract;
   function  yi(v : pointer ) : int; virtual; abstract;

   function  upscale  (v : double ) : pointer; virtual; abstract;
   function  downscale(v : int ) : pointer; virtual; abstract;

  end;

 ras_conv_int = object(ras_conv )
   result_ : array[1..max_stack ] of int;
   stack_  : int;

   constructor Construct;

   function  mul_div(a ,b ,c : double ) : pointer; virtual;

   function  xi(v : pointer ) : int; virtual;
   function  yi(v : pointer ) : int; virtual;

   function  upscale  (v : double ) : pointer; virtual;
   function  downscale(v : int ) : pointer; virtual;

  end;

 ras_conv_int_sat = object(ras_conv )
   result_ : array[1..max_stack ] of int;
   stack_  : int;

   constructor Construct;

   function  mul_div(a ,b ,c : double ) : pointer; virtual;

   function  xi(v : pointer ) : int; virtual;
   function  yi(v : pointer ) : int; virtual;

   function  upscale  (v : double ) : pointer; virtual;
   function  downscale(v : int ) : pointer; virtual;

  end;

 ras_conv_int_3x = object(ras_conv )
   result_ : array[1..max_stack ] of int;
   stack_  : int;

   constructor Construct;

   function  mul_div(a ,b ,c : double ) : pointer; virtual;

   function  xi(v : pointer ) : int; virtual;
   function  yi(v : pointer ) : int; virtual;

   function  upscale  (v : double ) : pointer; virtual;
   function  downscale(v : int ) : pointer; virtual;

  end;

 ras_conv_dbl = object(ras_conv )
   result_ : array[1..max_stack ] of double;
   stack_  : int;

   constructor Construct;

   function  mul_div(a ,b ,c : double ) : pointer; virtual;

   function  xi(v : pointer ) : int; virtual;
   function  yi(v : pointer ) : int; virtual;

   function  upscale  (v : double ) : pointer; virtual;
   function  downscale(v : int ) : pointer; virtual;

  end;

 ras_conv_dbl_3x = object(ras_conv )
   result_ : array[1..max_stack ] of double;
   stack_  : int;

   constructor Construct;

   function  mul_div(a ,b ,c : double ) : pointer; virtual;

   function  xi(v : pointer ) : int; virtual;
   function  yi(v : pointer ) : int; virtual;

   function  upscale  (v : double ) : pointer; virtual;
   function  downscale(v : int ) : pointer; virtual;

  end;

 rasterizer_sl_ptr = ^rasterizer_sl;
 rasterizer_sl = object
   procedure reset_clipping; virtual; abstract;
   procedure clip_box(x1 ,y1 ,x2 ,y2 : pointer ); virtual; abstract;
   procedure move_to (x1 ,y1 : pointer ); virtual; abstract;
   procedure line_to (ras : rasterizer_cells_aa_ptr; x2 ,y2 : pointer ); virtual; abstract;

   function  conv_type : ras_conv_ptr; virtual; abstract;

  end;

 rasterizer_sl_clip_int_ = object(rasterizer_sl )
  private
   t_conv : ras_conv_ptr;

   m_clip_box : rect;

   m_x1 ,
   m_y1 : int;

   m_f1 : unsigned;

   m_clipping : boolean;

  public
   constructor Construct(conv : ras_conv_ptr );

   procedure reset_clipping; virtual;
   procedure clip_box(x1 ,y1 ,x2 ,y2 : pointer ); virtual;
   procedure move_to (x1 ,y1 : pointer ); virtual;
   procedure line_to (ras : rasterizer_cells_aa_ptr; x2 ,y2 : pointer ); virtual;

   function  conv_type : ras_conv_ptr; virtual;

  private
   procedure line_clip_y(ras : rasterizer_cells_aa_ptr; x1 ,y1 ,x2 ,y2 : int; f1 ,f2 : unsigned );

  end;

 rasterizer_sl_clip_dbl_ = object(rasterizer_sl )
  private
   t_conv : ras_conv_ptr;

   m_clip_box : rect_d;

   m_x1 ,
   m_y1 : double;

   m_f1 : unsigned;

   m_clipping : boolean;

  public
   constructor Construct(conv : ras_conv_ptr );

   procedure reset_clipping; virtual;
   procedure clip_box(x1 ,y1 ,x2 ,y2 : pointer ); virtual;
   procedure move_to (x1 ,y1 : pointer ); virtual;
   procedure line_to (ras : rasterizer_cells_aa_ptr; x2 ,y2 : pointer ); virtual;

   function  conv_type : ras_conv_ptr; virtual;

  private
   procedure line_clip_y(ras : rasterizer_cells_aa_ptr; x1 ,y1 ,x2 ,y2 : double; f1 ,f2 : unsigned );

  end;

 rasterizer_sl_no_clip = object(rasterizer_sl )
  private
   m_x1 ,
   m_y1 : int;

   m_conv : ras_conv_int;

  public
   constructor Construct;

   procedure reset_clipping; virtual;
   procedure clip_box(x1 ,y1 ,x2 ,y2 : pointer ); virtual;
   procedure move_to (x1 ,y1 : pointer ); virtual;
   procedure line_to (ras : rasterizer_cells_aa_ptr; x2 ,y2 : pointer ); virtual;

   function  conv_type : ras_conv_ptr; virtual;

  end;

 rasterizer_sl_clip_int = object(rasterizer_sl_clip_int_ )
  private
   m_conv : ras_conv_int;

  public
   constructor Construct;

  end;

 rasterizer_sl_clip_int_sat = object(rasterizer_sl_clip_int_ )
  private
   m_conv : ras_conv_int_sat;

  public
   constructor Construct;

  end;

 rasterizer_sl_clip_int_3x = object(rasterizer_sl_clip_int_ )
  private
   m_conv : ras_conv_int_3x;

  public
   constructor Construct;

  end;

 rasterizer_sl_clip_dbl = object(rasterizer_sl_clip_dbl_ )
  private
   m_conv : ras_conv_dbl;

  public
   constructor Construct;

  end;

 rasterizer_sl_clip_dbl_3x = object(rasterizer_sl_clip_dbl_ )
  private
   m_conv : ras_conv_dbl_3x;

  public
   constructor Construct;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor ras_conv_int.Construct;
begin
 stack_:=1;

end;

{ MUL_DIV }
function ras_conv_int.mul_div(a ,b ,c : double ) : pointer;
begin
 result_[stack_ ]:=iround(a * b / c );

 result:=@result_[stack_ ];

 inc(stack_ );

 if stack_ > max_stack then
  stack_:=1;

end;

{ XI }
function ras_conv_int.xi(v : pointer ) : int;
begin
 result:=int_ptr(v )^;

end;

{ YI }
function ras_conv_int.yi(v : pointer ) : int;
begin
 result:=int_ptr(v )^;

end;

{ UPSCALE }
function ras_conv_int.upscale(v : double ) : pointer;
begin
 result_[stack_ ]:=iround(v * poly_subpixel_scale );

 result:=@result_[stack_ ];

 inc(stack_ );

 if stack_ > max_stack then
  stack_:=1;

end;

{ DOWNSCALE }
function ras_conv_int.downscale(v : int ) : pointer;
begin
 result_[stack_ ]:=v;

 result:=@result_[stack_ ];

 inc(stack_ );

 if stack_ > max_stack then
  stack_:=1;

end;

{ CONSTRUCT }
constructor ras_conv_int_sat.Construct;
begin
 stack_:=1;

end;

{ MUL_DIV }
function ras_conv_int_sat.mul_div(a ,b ,c : double ) : pointer;
begin
 result_[stack_ ]:=saturation_iround(poly_max_coord ,a * b / c );

 result:=@result_[stack_ ];

 inc(stack_ );

 if stack_ > max_stack then
  stack_:=1;

end;

{ XI }
function ras_conv_int_sat.xi(v : pointer ) : int;
begin
 result:=int_ptr(v )^;

end;

{ YI }
function ras_conv_int_sat.yi(v : pointer ) : int;
begin
 result:=int_ptr(v )^;

end;

{ UPSCALE }
function ras_conv_int_sat.upscale(v : double ) : pointer;
begin
 result_[stack_ ]:=saturation_iround(poly_max_coord ,v * poly_subpixel_scale );

 result:=@result_[stack_ ];

 inc(stack_ );

 if stack_ > max_stack then
  stack_:=1;

end;

{ DOWNSCALE }
function ras_conv_int_sat.downscale(v : int ) : pointer;
begin
 result_[stack_ ]:=v;

 result:=@result_[stack_ ];

 inc(stack_ );

 if stack_ > max_stack then
  stack_:=1;

end;

{ CONSTRUCT }
constructor ras_conv_int_3x.Construct;
begin
 stack_:=1;

end;

{ MUL_DIV }
function ras_conv_int_3x.mul_div(a ,b ,c : double ) : pointer;
begin
 result_[stack_ ]:=iround(a * b / c );

 result:=@result_[stack_ ];

 inc(stack_ );

 if stack_ > max_stack then
  stack_:=1;

end;

{ XI }
function ras_conv_int_3x.xi(v : pointer ) : int;
begin
 result:=int_ptr(v )^ * 3;

end;

{ YI }
function ras_conv_int_3x.yi(v : pointer ) : int;
begin
 result:=int_ptr(v )^;

end;

{ UPSCALE }
function ras_conv_int_3x.upscale(v : double ) : pointer;
begin
 result_[stack_ ]:=iround(v * poly_subpixel_scale );

 result:=@result_[stack_ ];

 inc(stack_ );

 if stack_ > max_stack then
  stack_:=1;

end;

{ DOWNSCALE }
function ras_conv_int_3x.downscale(v : int ) : pointer;
begin
 result_[stack_ ]:=v;

 result:=@result_[stack_ ];

 inc(stack_ );

 if stack_ > max_stack then
  stack_:=1;

end;

{ CONSTRUCT }
constructor ras_conv_dbl.Construct;
begin
 stack_:=1;

end;

{ MUL_DIV }
function ras_conv_dbl.mul_div(a ,b ,c : double ) : pointer;
begin
 result_[stack_ ]:=a * b / c;

 result:=@result_[stack_ ];

 inc(stack_ );

 if stack_ > max_stack then
  stack_:=1;

end;

{ XI }
function ras_conv_dbl.xi(v : pointer ) : int;
begin
 result:=iround(double_ptr(v )^ * poly_subpixel_scale );

end;

{ YI }
function ras_conv_dbl.yi(v : pointer ) : int;
begin
 result:=iround(double_ptr(v )^ * poly_subpixel_scale );

end;

{ UPSCALE }
function ras_conv_dbl.upscale(v : double ) : pointer;
begin
 result_[stack_ ]:=v;

 result:=@result_[stack_ ];

 inc(stack_ );

 if stack_ > max_stack then
  stack_:=1;

end;

{ DOWNSCALE }
function ras_conv_dbl.downscale(v : int ) : pointer;
begin
 result_[stack_ ]:=v / poly_subpixel_scale;

 result:=@result_[stack_ ];

 inc(stack_ );

 if stack_ > max_stack then
  stack_:=1;

end;

{ CONSTRUCT }
constructor ras_conv_dbl_3x.Construct;
begin
 stack_:=1;

end;

{ MUL_DIV }
function ras_conv_dbl_3x.mul_div(a ,b ,c : double ) : pointer;
begin
 result_[stack_ ]:=a * b / c;

 result:=@result_[stack_ ];

 inc(stack_ );

 if stack_ > max_stack then
  stack_:=1;

end;

{ XI }
function ras_conv_dbl_3x.xi(v : pointer ) : int;
begin
 result:=iround(double_ptr(v )^ * poly_subpixel_scale * 3 );

end;

{ YI }
function ras_conv_dbl_3x.yi(v : pointer ) : int;
begin
 result:=iround(double_ptr(v )^ * poly_subpixel_scale );

end;

{ UPSCALE }
function ras_conv_dbl_3x.upscale(v : double ) : pointer;
begin
 result_[stack_ ]:=v;

 result:=@result_[stack_ ];

 inc(stack_ );

 if stack_ > max_stack then
  stack_:=1;

end;

{ DOWNSCALE }
function ras_conv_dbl_3x.downscale(v : int ) : pointer;
begin
 result_[stack_ ]:=v / poly_subpixel_scale;

 result:=@result_[stack_ ];

 inc(stack_ );

 if stack_ > max_stack then
  stack_:=1;

end;

{ CONSTRUCT }
constructor rasterizer_sl_clip_int_.Construct(conv : ras_conv_ptr );
begin
 t_conv:=conv;

 m_clip_box.Construct(0 ,0 ,0 ,0 );

 m_x1:=0;
 m_y1:=0;
 m_f1:=0;

 m_clipping:=false;

end;

{ RESET_CLIPPING }
procedure rasterizer_sl_clip_int_.reset_clipping;
begin
 m_clipping:=false;

end;

{ CLIP_BOX }
procedure rasterizer_sl_clip_int_.clip_box(x1 ,y1 ,x2 ,y2 : pointer );
begin
 m_clip_box.Construct(int_ptr(x1 )^ ,int_ptr(y1 )^ ,int_ptr(x2 )^ ,int_ptr(y2 )^ );
 m_clip_box.normalize;

 m_clipping:=true;

end;

{ MOVE_TO }
procedure rasterizer_sl_clip_int_.move_to(x1 ,y1 : pointer );
begin
 m_x1:=int_ptr(x1 )^;
 m_y1:=int_ptr(y1 )^;

 if m_clipping then
  m_f1:=clipping_flags_int(int_ptr(x1 )^ ,int_ptr(y1 )^ ,@m_clip_box );

end;

{ LINE_TO }
procedure rasterizer_sl_clip_int_.line_to(ras : rasterizer_cells_aa_ptr; x2 ,y2 : pointer );
var
 f1 ,f2 ,f3 ,f4 : unsigned;
 x1 ,y1 ,y3 ,y4 : int;

begin
 if m_clipping then
  begin
   f2:=clipping_flags_int(int_ptr(x2 )^ ,int_ptr(y2 )^ ,@m_clip_box );

  // Invisible by Y
   if ((m_f1 and 10) = (f2 and 10) ) and
      (m_f1 and 10 <> 0 ) then
    begin
     m_x1:=int_ptr(x2 )^;
     m_y1:=int_ptr(y2 )^;
     m_f1:=f2;

     exit;

    end;

   x1:=m_x1;
   y1:=m_y1;
   f1:=m_f1;

   case ((f1 and 5 ) shl 1 ) or (f2 and 5 ) of
   // Visible by X
    0 : line_clip_y(ras ,x1 ,y1 ,int_ptr(x2 )^ ,int_ptr(y2 )^ ,f1 ,f2 );

   // x2 > clip.x2
    1 :
     begin
      y3:=y1 + int_ptr(t_conv.mul_div(m_clip_box.x2 - x1 ,int_ptr(y2 )^ - y1 ,int_ptr(x2 )^ - x1 ) )^;
      f3:=clipping_flags_y_int(y3 ,@m_clip_box );

      line_clip_y(ras ,x1 ,y1 ,m_clip_box.x2 ,y3 ,f1 ,f3 );
      line_clip_y(ras ,m_clip_box.x2 ,y3 ,m_clip_box.x2 ,int_ptr(y2 )^ ,f3 ,f2 );

     end;

   // x1 > clip.x2
    2 :
     begin
      y3:=y1 + int_ptr(t_conv.mul_div(m_clip_box.x2 - x1 ,int_ptr(y2 )^ - y1 ,int_ptr(x2 )^ - x1 ) )^;
      f3:=clipping_flags_y_int(y3 ,@m_clip_box );

      line_clip_y(ras ,m_clip_box.x2 ,y1 ,m_clip_box.x2 ,y3 ,f1 ,f3 );
      line_clip_y(ras ,m_clip_box.x2 ,y3 ,int_ptr(x2 )^ ,int_ptr(y2 )^ ,f3 ,f2 );

     end;

   // x1 > clip.x2 && x2 > clip.x2
    3 : line_clip_y(ras ,m_clip_box.x2 ,y1 ,m_clip_box.x2 ,int_ptr(y2 )^ ,f1 ,f2 );

   // x2 < clip.x1
    4 :
     begin
      y3:=y1 + int_ptr(t_conv.mul_div(m_clip_box.x1 - x1 ,int_ptr(y2 )^ - y1 ,int_ptr(x2 )^ - x1 ) )^;
      f3:=clipping_flags_y_int(y3 ,@m_clip_box );

      line_clip_y(ras ,x1 ,y1 ,m_clip_box.x1 ,y3 ,f1 ,f3 );
      line_clip_y(ras ,m_clip_box.x1 ,y3 ,m_clip_box.x1 ,int_ptr(y2 )^ ,f3 ,f2 );

     end;

   // x1 > clip.x2 && x2 < clip.x1
    6 :
     begin
      y3:=y1 + int_ptr(t_conv.mul_div(m_clip_box.x2 - x1 ,int_ptr(y2 )^ - y1 ,int_ptr(x2 )^ - x1 ) )^;
      y4:=y1 + int_ptr(t_conv.mul_div(m_clip_box.x1 - x1 ,int_ptr(y2 )^ - y1 ,int_ptr(x2 )^ - x1 ) )^;

      f3:=clipping_flags_y_int(y3 ,@m_clip_box );
      f4:=clipping_flags_y_int(y4 ,@m_clip_box );

      line_clip_y(ras ,m_clip_box.x2 ,y1 ,m_clip_box.x2 ,y3 ,f1 ,f3 );
      line_clip_y(ras ,m_clip_box.x2 ,y3 ,m_clip_box.x1 ,y4 ,f3 ,f4 );
      line_clip_y(ras ,m_clip_box.x1 ,y4 ,m_clip_box.x1 ,int_ptr(y2 )^ ,f4 ,f2 );

     end;

   // x1 < clip.x1
    8 :
     begin
      y3:=y1 + int_ptr(t_conv.mul_div(m_clip_box.x1 - x1 ,int_ptr(y2 )^ - y1 ,int_ptr(x2 )^ - x1 ) )^;
      f3:=clipping_flags_y_int(y3 ,@m_clip_box );

      line_clip_y(ras ,m_clip_box.x1 ,y1 ,m_clip_box.x1 ,y3 ,f1 ,f3 );
      line_clip_y(ras ,m_clip_box.x1 ,y3 ,int_ptr(x2 )^ ,int_ptr(y2 )^ ,f3 ,f2 );

     end;

   // x1 < clip.x1 && x2 > clip.x2
    9 :
     begin
      y3:=y1 + int_ptr(t_conv.mul_div(m_clip_box.x1 - x1 ,int_ptr(y2 )^ - y1 ,int_ptr(x2 )^ - x1 ) )^;
      y4:=y1 + int_ptr(t_conv.mul_div(m_clip_box.x2 - x1 ,int_ptr(y2 )^ - y1 ,int_ptr(x2 )^ - x1 ) )^;
      f3:=clipping_flags_y_int(y3 ,@m_clip_box );
      f4:=clipping_flags_y_int(y4 ,@m_clip_box );

      line_clip_y(ras ,m_clip_box.x1 ,y1 ,m_clip_box.x1 ,y3 ,f1 ,f3 );
      line_clip_y(ras ,m_clip_box.x1 ,y3 ,m_clip_box.x2 ,y4 ,f3 ,f4 );
      line_clip_y(ras ,m_clip_box.x2 ,y4 ,m_clip_box.x2 ,int_ptr(y2 )^ ,f4 ,f2 );

     end;

   // x1 < clip.x1 && x2 < clip.x1
    12 : line_clip_y(ras ,m_clip_box.x1 ,y1 ,m_clip_box.x1 ,int_ptr(y2 )^ ,f1 ,f2 );

   end;

   m_f1:=f2;

  end
 else
  ras.line(t_conv.xi(@m_x1 ) ,t_conv.yi(@m_y1 ) ,t_conv.xi(x2 ) ,t_conv.yi(y2 ) );

 m_x1:=int_ptr(x2 )^;
 m_y1:=int_ptr(y2 )^;

end;

{ CONV_TYPE }
function rasterizer_sl_clip_int_.conv_type : ras_conv_ptr;
begin
 result:=t_conv;

end;

{ LINE_CLIP_Y }
procedure rasterizer_sl_clip_int_.line_clip_y(ras : rasterizer_cells_aa_ptr; x1 ,y1 ,x2 ,y2 : int; f1 ,f2 : unsigned );
var
 tx1 ,ty1 ,tx2 ,ty2 : int;

begin
 f1:=f1 and 10;
 f2:=f2 and 10;

 if f1 or f2 = 0 then
 // Fully visible
  ras.line(t_conv.xi(@x1 ) ,t_conv.yi(@y1 ) ,t_conv.xi(@x2 ) ,t_conv.yi(@y2 ) )

 else
  begin
  // Invisible by Y
   if f1 = f2 then
    exit;

   tx1:=x1;
   ty1:=y1;
   tx2:=x2;
   ty2:=y2;

  // y1 < clip.y1
   if f1 and 8 <> 0 then
    begin
     tx1:=x1 + int_ptr(t_conv.mul_div(m_clip_box.y1 - y1 ,x2 - x1 ,y2 - y1 ) )^;
     ty1:=m_clip_box.y1;

    end;

  // y1 > clip.y2
   if f1 and 2 <> 0 then
    begin
     tx1:=x1 + int_ptr(t_conv.mul_div(m_clip_box.y2 - y1 ,x2 - x1 ,y2 - y1 ) )^;
     ty1:=m_clip_box.y2;

    end;

  // y2 < clip.y1
   if f2 and 8 <> 0 then
    begin
     tx2:=x1 + int_ptr(t_conv.mul_div(m_clip_box.y1 - y1 ,x2 - x1 ,y2 - y1 ) )^;
     ty2:=m_clip_box.y1;

    end;

  // y2 > clip.y2
   if f2 and 2 <> 0 then
    begin
     tx2:=x1 + int_ptr(t_conv.mul_div(m_clip_box.y2 - y1 ,x2 - x1 ,y2 - y1 ) )^;
     ty2:=m_clip_box.y2;

    end;

   ras.line(t_conv.xi(@tx1 ) ,t_conv.yi(@ty1 ) ,t_conv.xi(@tx2 ) ,t_conv.yi(@ty2 ) );

  end;

end;

{ CONSTRUCT }
constructor rasterizer_sl_clip_dbl_.Construct(conv : ras_conv_ptr );
begin
 t_conv:=conv;

 m_clip_box.Construct(0 ,0 ,0 ,0 );

 m_x1:=0;
 m_y1:=0;
 m_f1:=0;

 m_clipping:=false;

end;

{ RESET_CLIPPING }
procedure rasterizer_sl_clip_dbl_.reset_clipping;
begin
 m_clipping:=false;

end;

{ CLIP_BOX }
procedure rasterizer_sl_clip_dbl_.clip_box(x1 ,y1 ,x2 ,y2 : pointer );
begin
 m_clip_box.Construct(double_ptr(x1 )^ ,double_ptr(y1 )^ ,double_ptr(x2 )^ ,double_ptr(y2 )^ );
 m_clip_box.normalize;

 m_clipping:=true;

end;

{ MOVE_TO }
procedure rasterizer_sl_clip_dbl_.move_to(x1 ,y1 : pointer );
begin
 m_x1:=double_ptr(x1 )^;
 m_y1:=double_ptr(y1 )^;

 if m_clipping then
  m_f1:=clipping_flags_dbl(double_ptr(x1 )^ ,double_ptr(y1 )^ ,@m_clip_box );

end;

{ LINE_TO }
procedure rasterizer_sl_clip_dbl_.line_to(ras : rasterizer_cells_aa_ptr; x2 ,y2 : pointer );
var
 f1 ,f2 ,f3 ,f4 : unsigned;
 x1 ,y1 ,y3 ,y4 : double;

begin
 if m_clipping then
  begin
   f2:=clipping_flags_dbl(double_ptr(x2 )^ ,double_ptr(y2 )^ ,@m_clip_box );

  // Invisible by Y
   if ((m_f1 and 10) = (f2 and 10) ) and
      (m_f1 and 10 <> 0 ) then
    begin
     m_x1:=double_ptr(x2 )^;
     m_y1:=double_ptr(y2 )^;
     m_f1:=f2;

     exit;

    end;

   x1:=m_x1;
   y1:=m_y1;
   f1:=m_f1;

   case ((f1 and 5 ) shl 1 ) or (f2 and 5 ) of
   // Visible by X
    0 : line_clip_y(ras ,x1 ,y1 ,double_ptr(x2 )^ ,double_ptr(y2 )^ ,f1 ,f2 );

   // x2 > clip.x2
    1 :
     begin
      y3:=y1 + double_ptr(t_conv.mul_div(m_clip_box.x2 - x1 ,double_ptr(y2 )^ - y1 ,double_ptr(x2 )^ - x1 ) )^;
      f3:=clipping_flags_y_dbl(y3 ,@m_clip_box );

      line_clip_y(ras ,x1 ,y1 ,m_clip_box.x2 ,y3 ,f1 ,f3 );
      line_clip_y(ras ,m_clip_box.x2 ,y3 ,m_clip_box.x2 ,double_ptr(y2 )^ ,f3 ,f2 );

     end;

   // x1 > clip.x2
    2 :
     begin
      y3:=y1 + double_ptr(t_conv.mul_div(m_clip_box.x2 - x1 ,double_ptr(y2 )^ - y1 ,double_ptr(x2 )^ - x1 ) )^;
      f3:=clipping_flags_y_dbl(y3 ,@m_clip_box );

      line_clip_y(ras ,m_clip_box.x2 ,y1 ,m_clip_box.x2 ,y3 ,f1 ,f3 );
      line_clip_y(ras ,m_clip_box.x2 ,y3 ,double_ptr(x2 )^ ,double_ptr(y2 )^ ,f3 ,f2 );

     end;

   // x1 > clip.x2 && x2 > clip.x2
    3 : line_clip_y(ras ,m_clip_box.x2 ,y1 ,m_clip_box.x2 ,double_ptr(y2 )^ ,f1 ,f2 );

   // x2 < clip.x1
    4 :
     begin
      y3:=y1 + double_ptr(t_conv.mul_div(m_clip_box.x1 - x1 ,double_ptr(y2 )^ - y1 ,double_ptr(x2 )^ - x1 ) )^;
      f3:=clipping_flags_y_dbl(y3 ,@m_clip_box );

      line_clip_y(ras ,x1 ,y1 ,m_clip_box.x1 ,y3 ,f1 ,f3 );
      line_clip_y(ras ,m_clip_box.x1 ,y3 ,m_clip_box.x1 ,double_ptr(y2 )^ ,f3 ,f2 );

     end;

   // x1 > clip.x2 && x2 < clip.x1
    6 :
     begin
      y3:=y1 + double_ptr(t_conv.mul_div(m_clip_box.x2 - x1 ,double_ptr(y2 )^ - y1 ,double_ptr(x2 )^ - x1 ) )^;
      y4:=y1 + double_ptr(t_conv.mul_div(m_clip_box.x1 - x1 ,double_ptr(y2 )^ - y1 ,double_ptr(x2 )^ - x1 ) )^;

      f3:=clipping_flags_y_dbl(y3 ,@m_clip_box );
      f4:=clipping_flags_y_dbl(y4 ,@m_clip_box );

      line_clip_y(ras ,m_clip_box.x2 ,y1 ,m_clip_box.x2 ,y3 ,f1 ,f3 );
      line_clip_y(ras ,m_clip_box.x2 ,y3 ,m_clip_box.x1 ,y4 ,f3 ,f4 );
      line_clip_y(ras ,m_clip_box.x1 ,y4 ,m_clip_box.x1 ,double_ptr(y2 )^ ,f4 ,f2 );

     end;

   // x1 < clip.x1
    8 :
     begin
      y3:=y1 + double_ptr(t_conv.mul_div(m_clip_box.x1 - x1 ,double_ptr(y2 )^ - y1 ,double_ptr(x2 )^ - x1 ) )^;
      f3:=clipping_flags_y_dbl(y3 ,@m_clip_box );

      line_clip_y(ras ,m_clip_box.x1 ,y1 ,m_clip_box.x1 ,y3 ,f1 ,f3 );
      line_clip_y(ras ,m_clip_box.x1 ,y3 ,double_ptr(x2 )^ ,double_ptr(y2 )^ ,f3 ,f2 );

     end;

   // x1 < clip.x1 && x2 > clip.x2
    9 :
     begin
      y3:=y1 + double_ptr(t_conv.mul_div(m_clip_box.x1 - x1 ,double_ptr(y2 )^ - y1 ,double_ptr(x2 )^ - x1 ) )^;
      y4:=y1 + double_ptr(t_conv.mul_div(m_clip_box.x2 - x1 ,double_ptr(y2 )^ - y1 ,double_ptr(x2 )^ - x1 ) )^;
      f3:=clipping_flags_y_dbl(y3 ,@m_clip_box );
      f4:=clipping_flags_y_dbl(y4 ,@m_clip_box );

      line_clip_y(ras ,m_clip_box.x1 ,y1 ,m_clip_box.x1 ,y3 ,f1 ,f3 );
      line_clip_y(ras ,m_clip_box.x1 ,y3 ,m_clip_box.x2 ,y4 ,f3 ,f4 );
      line_clip_y(ras ,m_clip_box.x2 ,y4 ,m_clip_box.x2 ,double_ptr(y2 )^ ,f4 ,f2 );

     end;

   // x1 < clip.x1 && x2 < clip.x1
    12 : line_clip_y(ras ,m_clip_box.x1 ,y1 ,m_clip_box.x1 ,double_ptr(y2 )^ ,f1 ,f2 );

   end;

   m_f1:=f2;

  end
 else
  ras.line(t_conv.xi(@m_x1 ) ,t_conv.yi(@m_y1 ) ,t_conv.xi(x2 ) ,t_conv.yi(y2 ) );

 m_x1:=double_ptr(x2 )^;
 m_y1:=double_ptr(y2 )^;

end;

{ CONV_TYPE }
function rasterizer_sl_clip_dbl_.conv_type : ras_conv_ptr;
begin
 result:=t_conv;

end;

{ LINE_CLIP_Y }
procedure rasterizer_sl_clip_dbl_.line_clip_y(ras : rasterizer_cells_aa_ptr; x1 ,y1 ,x2 ,y2 : double; f1 ,f2 : unsigned );
var
 tx1 ,ty1 ,tx2 ,ty2 : double;

begin
 f1:=f1 and 10;
 f2:=f2 and 10;

 if f1 or f2 = 0 then
 // Fully visible
  ras.line(t_conv.xi(@x1 ) ,t_conv.yi(@y1 ) ,t_conv.xi(@x2 ) ,t_conv.yi(@y2 ) )

 else
  begin
  // Invisible by Y
   if f1 = f2 then
    exit;

   tx1:=x1;
   ty1:=y1;
   tx2:=x2;
   ty2:=y2;

  // y1 < clip.y1
   if f1 and 8 <> 0 then
    begin
     tx1:=x1 + double_ptr(t_conv.mul_div(m_clip_box.y1 - y1 ,x2 - x1 ,y2 - y1 ) )^;
     ty1:=m_clip_box.y1;

    end;

  // y1 > clip.y2
   if f1 and 2 <> 0 then
    begin
     tx1:=x1 + double_ptr(t_conv.mul_div(m_clip_box.y2 - y1 ,x2 - x1 ,y2 - y1 ) )^;
     ty1:=m_clip_box.y2;

    end;

  // y2 < clip.y1
   if f2 and 8 <> 0 then
    begin
     tx2:=x1 + double_ptr(t_conv.mul_div(m_clip_box.y1 - y1 ,x2 - x1 ,y2 - y1 ) )^;
     ty2:=m_clip_box.y1;

    end;

  // y2 > clip.y2
   if f2 and 2 <> 0 then
    begin
     tx2:=x1 + double_ptr(t_conv.mul_div(m_clip_box.y2 - y1 ,x2 - x1 ,y2 - y1 ) )^;
     ty2:=m_clip_box.y2;

    end;

   ras.line(t_conv.xi(@tx1 ) ,t_conv.yi(@ty1 ) ,t_conv.xi(@tx2 ) ,t_conv.yi(@ty2 ) );

  end;

end;

{ CONSTRUCT }
constructor rasterizer_sl_no_clip.Construct;
begin
 m_x1:=0;
 m_y1:=0;

 m_conv.Construct;

end;

{ RESET_CLIPPING }
procedure rasterizer_sl_no_clip.reset_clipping;
begin
end;

{ CLIP_BOX }
procedure rasterizer_sl_no_clip.clip_box(x1 ,y1 ,x2 ,y2 : pointer );
begin
end;

{ MOVE_TO }
procedure rasterizer_sl_no_clip.move_to(x1 ,y1 : pointer );
begin
 m_x1:=int_ptr(x1 )^;
 m_y1:=int_ptr(y1 )^;

end;

{ LINE_TO }
procedure rasterizer_sl_no_clip.line_to(ras : rasterizer_cells_aa_ptr; x2 ,y2 : pointer );
begin
 ras.line(m_x1 ,m_y1 ,int_ptr(x2 )^ ,int_ptr(y2 )^ );

 m_x1:=int_ptr(x2 )^;
 m_y1:=int_ptr(y2 )^;

end;

{ CONV_TYPE }
function rasterizer_sl_no_clip.conv_type : ras_conv_ptr;
begin
 result:=@m_conv;

end;

{ CONSTRUCT }
constructor rasterizer_sl_clip_int.Construct;
begin
 m_conv.Construct;

 inherited Construct(@m_conv );

end;

{ CONSTRUCT }
constructor rasterizer_sl_clip_int_sat.Construct;
begin
 m_conv.Construct;

 inherited Construct(@m_conv );

end;

{ CONSTRUCT }
constructor rasterizer_sl_clip_int_3x.Construct;
begin
 m_conv.Construct;

 inherited Construct(@m_conv );

end;

{ CONSTRUCT }
constructor rasterizer_sl_clip_dbl.Construct;
begin
 m_conv.Construct;

 inherited Construct(@m_conv );

end;

{ CONSTRUCT }
constructor rasterizer_sl_clip_dbl_3x.Construct;
begin
 m_conv.Construct;

 inherited Construct(@m_conv );

end;

END.

