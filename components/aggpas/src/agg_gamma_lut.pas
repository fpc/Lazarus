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
// 16.01.2006-Milano: Unit port establishment
//
{ agg_gamma_lut.pas }
unit
 agg_gamma_lut ;

INTERFACE

{$I agg_mode.inc }

uses
 Math ,
 agg_basics ,
 agg_pixfmt ;

{ TYPES DEFINITION }
type
 gamma_lut = object(gamma )
   gamma_shift ,
   gamma_size  ,
   gamma_mask  ,

   hi_res_shift ,
   hi_res_size  ,
   hi_res_mask  ,

   HiResT ,
   LoResT : unsigned;

   m_gamma : double;

   m_dir_gamma ,
   m_inv_gamma : int8u_ptr;

   constructor Construct_(GammaShift : unsigned = 8; HiResShift : unsigned = 8 );
   constructor Construct (g : double; GammaShift : unsigned = 8; HiResShift : unsigned = 8 );
   destructor  Destruct;

   procedure gamma_(g : double );
   function  _gamma : double;

   function dir(v : unsigned ) : unsigned; virtual;
   function inv(v : unsigned ) : unsigned; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor gamma_lut.Construct_;
var
 i : unsigned;

begin
 gamma_shift:=GammaShift;
 gamma_size :=1 shl gamma_shift;
 gamma_mask :=gamma_size - 1;

 hi_res_shift:=HiResShift;
 hi_res_size :=1 shl hi_res_shift;
 hi_res_mask :=hi_res_size - 1;

 HiResT:=hi_res_shift div 8;
 LoResT:=gamma_shift div 8;

 agg_getmem(pointer(m_dir_gamma ) ,gamma_size * HiResT );
 agg_getmem(pointer(m_inv_gamma ) ,hi_res_size * LoResT );
 
// dir_gamma
 for i:=0 to gamma_size - 1 do
  try
   case HiResT of
    1 :
     int8u_ptr(ptrcomp(m_dir_gamma ) + i * HiResT )^:=
      int8u(i shl (hi_res_shift - gamma_shift ) );

    2 :
     int16u_ptr(ptrcomp(m_dir_gamma ) + i * HiResT )^:=
      int16u(i shl (hi_res_shift - gamma_shift ) );

    4 :
     int32u_ptr(ptrcomp(m_dir_gamma ) + i * HiResT )^:=
      int32u(i shl (hi_res_shift - gamma_shift ) );

   end;
  except
  end;

// inv_gamma
 for i:=0 to hi_res_size - 1 do
  try
   case LoResT of
    1 :
     int8u_ptr(ptrcomp(m_inv_gamma ) + i * LoResT )^:=
      int8u(i shr (hi_res_shift - gamma_shift ) );

    2 :
     int16u_ptr(ptrcomp(m_inv_gamma ) + i * LoResT )^:=
      int16u(i shr (hi_res_shift - gamma_shift ) );

    4 :
     int32u_ptr(ptrcomp(m_inv_gamma ) + i * LoResT )^:=
      int32u(i shr (hi_res_shift - gamma_shift ) );

   end;
  except
  end;

end;

{ CONSTRUCT }
constructor gamma_lut.Construct;
begin
 gamma_shift:=GammaShift;
 gamma_size :=1 shl gamma_shift;
 gamma_mask :=gamma_size - 1;

 hi_res_shift:=HiResShift;
 hi_res_size :=1 shl hi_res_shift;
 hi_res_mask :=hi_res_size - 1;

 HiResT:=hi_res_shift div 8;
 LoResT:=gamma_shift div 8;

 m_gamma:=1;

 agg_getmem(pointer(m_dir_gamma ) ,gamma_size * HiResT );
 agg_getmem(pointer(m_inv_gamma ) ,hi_res_size * LoResT );

 gamma_(g );

end;

{ DESTRUCT }
destructor gamma_lut.Destruct;
begin
 agg_freemem(pointer(m_dir_gamma ) ,gamma_size * HiResT );
 agg_freemem(pointer(m_inv_gamma ) ,hi_res_size * LoResT );

end;

{ GAMMA_ }
procedure gamma_lut.gamma_;
var
 i : unsigned;

 inv_g : double;

begin
 m_gamma:=g;

// dir_gamma
 for i:=0 to gamma_size - 1 do
  try
   case HiResT of
    1 :
     int8u_ptr(ptrcomp(m_dir_gamma ) + i * HiResT )^:=
      int8u(trunc(Power(i / gamma_mask ,m_gamma ) * hi_res_mask + 0.5 ) );

    2 :
     int16u_ptr(ptrcomp(m_dir_gamma ) + i * HiResT )^:=
      int16u(trunc(Power(i / gamma_mask ,m_gamma ) * hi_res_mask + 0.5 ) );

    4 :
     int32u_ptr(ptrcomp(m_dir_gamma ) + i * HiResT )^:=
      int32u(trunc(Power(i / gamma_mask ,m_gamma ) * hi_res_mask + 0.5 ) );

   end;
  except
  end;

// inv_gamma
 if g = 0 then
  fillchar(m_inv_gamma^ ,hi_res_size * LoResT ,0 )

 else
  begin
   inv_g:=1 / g;

   for i:=0 to hi_res_size - 1 do
    try
     case LoResT of
      1 :
       int8u_ptr(ptrcomp(m_inv_gamma ) + i * LoResT )^:=
        int8u(trunc(Power(i / hi_res_mask ,inv_g ) * gamma_mask + 0.5 ) );

      2 :
       int16u_ptr(ptrcomp(m_inv_gamma ) + i * LoResT )^:=
        int16u(trunc(Power(i / hi_res_mask ,inv_g ) * gamma_mask + 0.5 ) );

      4 :
       int32u_ptr(ptrcomp(m_inv_gamma ) + i * LoResT )^:=
        int32u(trunc(Power(i / hi_res_mask ,inv_g ) * gamma_mask + 0.5 ) );

     end;
    except
    end;

  end;

end;

{ _GAMMA }
function gamma_lut._gamma : double;
begin
 result:=m_gamma;

end;

{ DIR }
function gamma_lut.dir;
begin
 case HiResT of
  1 :
   result:=int8u_ptr(ptrcomp(m_dir_gamma ) + v * HiResT )^;

  2 :
   result:=int16u_ptr(ptrcomp(m_dir_gamma ) + v * HiResT )^;

  4 :
   result:=int32u_ptr(ptrcomp(m_dir_gamma ) + v * HiResT )^;

  else
   result:=0;

 end;

end;

{ INV }
function gamma_lut.inv;
begin
 case LoResT of
  1 :
   result:=int8u_ptr(ptrcomp(m_inv_gamma ) + v * LoResT )^;

  2 :
   result:=int16u_ptr(ptrcomp(m_inv_gamma ) + v * LoResT )^;

  4 :
   result:=int32u_ptr(ptrcomp(m_inv_gamma ) + v * LoResT )^;

  else
   result:=0;

 end;

end;

END.

