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
// 10.02.2006-Milano: amask_no_clip_u8
// 09.02.2006-Milano: Unit port establishment
//
{ agg_alpha_mask_u8.pas }
unit
 agg_alpha_mask_u8 ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_rendering_buffer ;

{ TYPES DEFINITION }
const
 cover_shift = 8;
 cover_none  = 0;
 cover_full  = 255;

type
 func_mask_calculate = function(p : int8u_ptr ) : unsigned;

//==========================================================alpha_mask_u8
 alpha_mask_ptr = ^alpha_mask;
 alpha_mask = object
   procedure attach(rbuf : rendering_buffer_ptr ); virtual; abstract;

   function  mask_function : func_mask_calculate; virtual; abstract;

   function  pixel        (x ,y : int ) : int8u; virtual; abstract;
   function  combine_pixel(x ,y : int; val : int8u ) : int8u; virtual; abstract;

   procedure fill_hspan   (x ,y : int; dst : int8u_ptr; num_pix : int ); virtual; abstract;
   procedure combine_hspan(x ,y : int; dst : int8u_ptr; num_pix : int ); virtual; abstract;
   procedure fill_vspan   (x ,y : int; dst : int8u_ptr; num_pix : int ); virtual; abstract;
   procedure combine_vspan(x ,y : int; dst : int8u_ptr; num_pix : int ); virtual; abstract;

  end;

 alpha_mask_u8 = object(alpha_mask )
   Step   ,
   Offset : unsigned;

   m_rbuf          : rendering_buffer_ptr;
   m_mask_function : func_mask_calculate;

   constructor Construct(MaskF : func_mask_calculate; Step_ : unsigned = 1; Offset_ : unsigned = 0 ); overload;
   constructor Construct(rbuf : rendering_buffer_ptr; MaskF : func_mask_calculate; Step_ : unsigned = 1; Offset_ : unsigned = 0 ); overload;

   procedure attach(rbuf : rendering_buffer_ptr ); virtual;

   function  mask_function : func_mask_calculate; virtual;

   function  pixel        (x ,y : int ) : int8u; virtual;
   function  combine_pixel(x ,y : int; val : int8u ) : int8u; virtual;

   procedure fill_hspan   (x ,y : int; dst : int8u_ptr; num_pix : int ); virtual;
   procedure combine_hspan(x ,y : int; dst : int8u_ptr; num_pix : int ); virtual;
   procedure fill_vspan   (x ,y : int; dst : int8u_ptr; num_pix : int ); virtual;
   procedure combine_vspan(x ,y : int; dst : int8u_ptr; num_pix : int ); virtual;

  end;

 alpha_mask_gray8 = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_rgb24r = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_rgb24g = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_rgb24b = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_bgr24r = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_bgr24g = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_bgr24b = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_rgba32r = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_rgba32g = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_rgba32b = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_rgba32a = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_argb32r = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_argb32g = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_argb32b = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_argb32a = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_bgra32r = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_bgra32g = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_bgra32b = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_bgra32a = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_abgr32r = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_abgr32g = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_abgr32b = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_abgr32a = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_rgb24gray = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_bgr24gray = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_rgba32gray = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_argb32gray = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_bgra32gray = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 alpha_mask_abgr32gray = object(alpha_mask_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

//==========================================================amask_no_clip_u8
 amask_no_clip_u8 = object(alpha_mask )
   Step   ,
   Offset : unsigned;

   m_rbuf          : rendering_buffer_ptr;
   m_mask_function : func_mask_calculate;

   constructor Construct(MaskF : func_mask_calculate; Step_ : unsigned = 1; Offset_ : unsigned = 0 ); overload;
   constructor Construct(rbuf : rendering_buffer_ptr; MaskF : func_mask_calculate; Step_ : unsigned = 1; Offset_ : unsigned = 0 ); overload;

   procedure attach(rbuf : rendering_buffer_ptr ); virtual;

   function  mask_function : func_mask_calculate; virtual;

   function  pixel        (x ,y : int ) : int8u; virtual;
   function  combine_pixel(x ,y : int; val : int8u ) : int8u; virtual;

   procedure fill_hspan   (x ,y : int; dst : int8u_ptr; num_pix : int ); virtual;
   procedure combine_hspan(x ,y : int; dst : int8u_ptr; num_pix : int ); virtual;
   procedure fill_vspan   (x ,y : int; dst : int8u_ptr; num_pix : int ); virtual;
   procedure combine_vspan(x ,y : int; dst : int8u_ptr; num_pix : int ); virtual;

  end;

 amask_no_clip_gray8 = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_rgb24r = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_rgb24g = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_rgb24b = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_bgr24r = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_bgr24g = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_bgr24b = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_rgba32r = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_rgba32g = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_rgba32b = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_rgba32a = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_argb32r = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_argb32g = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_argb32b = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_argb32a = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_bgra32r = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_bgra32g = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_bgra32b = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_bgra32a = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_abgr32r = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_abgr32g = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_abgr32b = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_abgr32a = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_rgb24gray = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_bgr24gray = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_rgba32gray = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_argb32gray = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_bgra32gray = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

 amask_no_clip_abgr32gray = object(amask_no_clip_u8 )
   constructor Construct(rbuf : rendering_buffer_ptr );

  end;

{ GLOBAL PROCEDURES }
 function one_component_mask_u8  (p : int8u_ptr ) : unsigned;
 function rgb_to_gray_mask_u8_012(p : int8u_ptr ) : unsigned;
 function rgb_to_gray_mask_u8_210(p : int8u_ptr ) : unsigned;


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ ONE_COMPONENT_MASK_U8 }
function one_component_mask_u8;
begin
 result:=p^;

end;

{ RGB_TO_GRAY_MASK_U8_012 }
function rgb_to_gray_mask_u8_012;
begin
 result:=
  int8u((
   int8u_ptr(ptrcomp(p ) + 0 * sizeof(int8u ) )^ * 77 +
   int8u_ptr(ptrcomp(p ) + 1 * sizeof(int8u ) )^ * 150 +
   int8u_ptr(ptrcomp(p ) + 2 * sizeof(int8u ) )^ * 29  shr 8 ) );

end;

{ RGB_TO_GRAY_MASK_U8_210 }
function rgb_to_gray_mask_u8_210;
begin
 result:=
  int8u((
   int8u_ptr(ptrcomp(p ) + 2 * sizeof(int8u ) )^ * 77 +
   int8u_ptr(ptrcomp(p ) + 1 * sizeof(int8u ) )^ * 150 +
   int8u_ptr(ptrcomp(p ) + 0 * sizeof(int8u ) )^ * 29 shr 8 ) );

end;

{ CONSTRUCT }
constructor alpha_mask_u8.Construct(MaskF : func_mask_calculate; Step_ : unsigned = 1; Offset_ : unsigned = 0 );
begin
 Step  :=Step_;
 Offset:=Offset_;

 m_rbuf         :=NIL;
 m_mask_function:=MaskF;

end;

{ CONSTRUCT }
constructor alpha_mask_u8.Construct(rbuf : rendering_buffer_ptr; MaskF : func_mask_calculate; Step_ : unsigned = 1; Offset_ : unsigned = 0 );
begin
 Step  :=Step_;
 Offset:=Offset_;

 m_rbuf         :=rbuf;
 m_mask_function:=MaskF;

end;

{ ATTACH }
procedure alpha_mask_u8.attach;
begin
 m_rbuf:=rbuf;

end;

{ MASK_FUNCTION }
function alpha_mask_u8.mask_function;
begin
 result:=@m_mask_function;

end;

{ PIXEL }
function alpha_mask_u8.pixel;
begin
 if(x >= 0 ) and
   (y >= 0 ) and
   (x < m_rbuf._width ) and
   (y < m_rbuf._height ) then
  result:=
   int8u(m_mask_function(
    int8u_ptr(ptrcomp(m_rbuf.row(y ) ) + (x * Step + Offset ) * sizeof(int8u ) ) ) )
 else
  result:=0;

end;

{ COMBINE_PIXEL }
function alpha_mask_u8.combine_pixel;
begin
 if(x >= 0 ) and
   (y >= 0 ) and
   (x < m_rbuf._width ) and
   (y < m_rbuf._height ) then
  result:=int8u(
   (cover_full + val *
     m_mask_function(
      int8u_ptr(ptrcomp(m_rbuf.row(y ) ) + (x * Step + Offset ) * sizeof(int8u ) ) )
   ) shr cover_shift )
 else
  result:=0;

end;

{ FILL_HSPAN }
procedure alpha_mask_u8.fill_hspan;
var
 xmax ,ymax ,count ,rest : int;

 covers ,mask : int8u_ptr;

begin
 xmax:=m_rbuf._width - 1;
 ymax:=m_rbuf._height - 1;

 count :=num_pix;
 covers:=dst;

 if (y < 0 ) or
    (y > ymax ) then
  begin
   fillchar(dst^ ,num_pix * sizeof(int8u ) ,0 );

   exit;

  end;

 if x < 0 then
  begin
   inc(count ,x );

   if count <= 0 then
    begin
     fillchar(dst^ ,num_pix * sizeof(int8u ) ,0 );

     exit;

    end;

   fillchar(covers^ ,-x * sizeof(int8u ) ,0 );

   dec(covers ,x );

   x:=0;

  end;

 if x + count > xmax then
  begin
   rest:=x + count - xmax - 1;

   dec(count ,rest );

   if count <= 0 then
    begin
     fillchar(dst^ ,num_pix * sizeof(int8u ) ,0 );

     exit;

    end;

   fillchar(int8u_ptr(ptrcomp(covers ) + count * sizeof(int8u ) )^ ,rest * sizeof(int8u ) ,0 );

  end;

 mask:=int8u_ptr(ptrcomp(m_rbuf.row(y ) ) + (x * Step + Offset ) * sizeof(int8u ) );

 repeat
  covers^:=int8u(m_mask_function(mask ) );

  inc(ptrcomp(covers ) ,sizeof(int8u ) );
  inc(ptrcomp(mask ) ,Step * sizeof(int8u ) );
  dec(count );

 until count = 0;

end;

{ COMBINE_HSPAN }
procedure alpha_mask_u8.combine_hspan;
var
 xmax ,ymax ,count ,rest : int;

 covers ,mask : int8u_ptr;

begin
 xmax:=m_rbuf._width - 1;
 ymax:=m_rbuf._height - 1;

 count :=num_pix;
 covers:=dst;

 if (y < 0 ) or
    (y > ymax ) then
  begin
   fillchar(dst^ ,num_pix * sizeof(int8u ) ,0 );

   exit;

  end;

 if x < 0 then
  begin
   inc(count ,x );

   if count <= 0 then
    begin
     fillchar(dst^ ,num_pix * sizeof(int8u ) ,0 );

     exit;

    end;

   fillchar(covers^ ,-x * sizeof(int8u ) ,0 );

   dec(ptrcomp(covers ) ,x * sizeof(int8u ) );

   x:=0;

  end;

 if x + count > xmax then
  begin
   rest:=x + count - xmax - 1;

   dec(count ,rest );

   if count <= 0 then
    begin
     fillchar(dst^ ,num_pix * sizeof(int8u ) ,0 );

     exit;

    end;

   fillchar(int8u_ptr(ptrcomp(covers ) + count * sizeof(int8u ) )^ ,rest * sizeof(int8u ) ,0 );

  end;

 mask:=int8u_ptr(ptrcomp(m_rbuf.row(y ) ) + (x * Step + Offset ) * sizeof(int8u ) );

 repeat
  covers^:=
   int8u(
    (cover_full + covers^ *
     m_mask_function(mask ) ) shr cover_shift );

  inc(ptrcomp(covers ) ,sizeof(int8u ) );
  inc(mask ,Step * sizeof(int8u ) );
  dec(count );

 until count = 0;


end;

{ FILL_VSPAN }
procedure alpha_mask_u8.fill_vspan;
var
 xmax ,ymax ,count ,rest : int;

 covers ,mask : int8u_ptr;

begin
 xmax:=m_rbuf._width - 1;
 ymax:=m_rbuf._height - 1;

 count :=num_pix;
 covers:=dst;

 if (x < 0 ) or
    (x > xmax ) then
  begin
   fillchar(dst^ ,num_pix * sizeof(int8u ) ,0 );

   exit;

  end;

 if y < 0 then
  begin
   inc(count ,y );

   if count <= 0 then
    begin
     fillchar(dst^ ,num_pix * sizeof(int8u ) ,0 );

     exit;

    end;

   fillchar(covers^ ,-y * sizeof(int8u ) ,0 );

   dec(ptrcomp(covers ) ,y * sizeof(int8u ) );

   y:=0;

  end;

 if y + count > ymax then
  begin
   rest:=y + count - ymax - 1;

   dec(count ,rest );

   if count <= 0 then
    begin
     fillchar(dst^ ,num_pix * sizeof(int8u ) ,0 );

     exit;

    end;

   fillchar(int8u_ptr(ptrcomp(covers ) + count * sizeof(int8u ) )^ ,rest * sizeof(int8u ) ,0 );

  end;

 repeat
  covers^:=int8u(m_mask_function(mask ) );

  inc(ptrcomp(covers ) ,sizeof(int8u ) );
  inc(ptrcomp(mask ) ,m_rbuf._stride );
  dec(count );

 until count = 0;

end;

{ COMBINE_VSPAN }
procedure alpha_mask_u8.combine_vspan;
var
 xmax ,ymax ,count ,rest : int;

 covers ,mask : int8u_ptr;

begin
 xmax:=m_rbuf._width - 1;
 ymax:=m_rbuf._height - 1;

 count :=num_pix;
 covers:=dst;

 if (x < 0 ) or
    (x > xmax ) then
  begin
   fillchar(dst^ ,num_pix * sizeof(int8u ) ,0 );

   exit;

  end;

 if y < 0 then
  begin
   inc(count ,y );

   if count <= 0 then
    begin
     fillchar(dst^ ,num_pix * sizeof(int8u ) ,0 );

     exit;

    end;

   fillchar(covers^ ,-y * sizeof(int8u ) ,0 );

   dec(ptrcomp(covers ) ,y * sizeof(int8u ) );

   y:=0;

  end;

 if y + count > ymax then
  begin
   rest:=y + count - ymax - 1;

   dec(count ,rest );

   if count <= 0 then
    begin
     fillchar(dst^ ,num_pix * sizeof(int8u ) ,0 );

     exit;

    end;

   fillchar(int8u_ptr(ptrcomp(covers ) + count * sizeof(int8u ) )^ ,rest * sizeof(int8u ) ,0 );

  end;

 mask:=int8u_ptr(ptrcomp(m_rbuf.row(y ) ) + (x * Step + Offset ) * sizeof(int8u ) );

 repeat
  covers^:=
   int8u(
    (cover_full + covers^ *
     m_mask_function(mask ) ) shr cover_shift );

  inc(ptrcomp(covers ) ,sizeof(int8u ) );
  inc(ptrcomp(mask ) ,m_rbuf._stride );
  dec(count );

 until count = 0;

end;

{ CONSTRUCT }
constructor alpha_mask_gray8.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,1 ,0 );

end;

{ CONSTRUCT }
constructor alpha_mask_rgb24r.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,3 ,0 );

end;

{ CONSTRUCT }
constructor alpha_mask_rgb24g.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,3 ,1 );

end;

{ CONSTRUCT }
constructor alpha_mask_rgb24b.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,3 ,2 );

end;

{ CONSTRUCT }
constructor alpha_mask_bgr24r.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,3 ,2 );

end;

{ CONSTRUCT }
constructor alpha_mask_bgr24g.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,3 ,1 );

end;

{ CONSTRUCT }
constructor alpha_mask_bgr24b.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,3 ,0 );

end;

{ CONSTRUCT }
constructor alpha_mask_rgba32r.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,0 );

end;

{ CONSTRUCT }
constructor alpha_mask_rgba32g.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,1 );

end;

{ CONSTRUCT }
constructor alpha_mask_rgba32b.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,2 );

end;

{ CONSTRUCT }
constructor alpha_mask_rgba32a.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,3 );

end;

{ CONSTRUCT }
constructor alpha_mask_argb32r.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,1 );

end;

{ CONSTRUCT }
constructor alpha_mask_argb32g.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,2 );

end;

{ CONSTRUCT }
constructor alpha_mask_argb32b.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,3 );

end;

{ CONSTRUCT }
constructor alpha_mask_argb32a.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,0 );

end;

{ CONSTRUCT }
constructor alpha_mask_bgra32r.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,2 );

end;

{ CONSTRUCT }
constructor alpha_mask_bgra32g.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,1 );

end;

{ CONSTRUCT }
constructor alpha_mask_bgra32b.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,0 );

end;

{ CONSTRUCT }
constructor alpha_mask_bgra32a.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,3 );

end;

{ CONSTRUCT }
constructor alpha_mask_abgr32r.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,3 );

end;

{ CONSTRUCT }
constructor alpha_mask_abgr32g.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,2 );

end;

{ CONSTRUCT }
constructor alpha_mask_abgr32b.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,1 );

end;

{ CONSTRUCT }
constructor alpha_mask_abgr32a.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,0 );

end;

{ CONSTRUCT }
constructor alpha_mask_rgb24gray.Construct;
begin
 inherited Construct(rbuf ,@rgb_to_gray_mask_u8_012 ,3 ,0 );

end;

{ CONSTRUCT }
constructor alpha_mask_bgr24gray.Construct;
begin
 inherited Construct(rbuf ,@rgb_to_gray_mask_u8_210 ,3 ,0 );

end;

{ CONSTRUCT }
constructor alpha_mask_rgba32gray.Construct;
begin
 inherited Construct(rbuf ,@rgb_to_gray_mask_u8_012 ,4 ,0 );

end;

{ CONSTRUCT }
constructor alpha_mask_argb32gray.Construct;
begin
 inherited Construct(rbuf ,@rgb_to_gray_mask_u8_012 ,4 ,1 );

end;

{ CONSTRUCT }
constructor alpha_mask_bgra32gray.Construct;
begin
 inherited Construct(rbuf ,@rgb_to_gray_mask_u8_210 ,4 ,0 );

end;

{ CONSTRUCT }
constructor alpha_mask_abgr32gray.Construct;
begin
 inherited Construct(rbuf ,@rgb_to_gray_mask_u8_210 ,4 ,1 );

end;

{ CONSTRUCT }
constructor amask_no_clip_u8.Construct(MaskF : func_mask_calculate; Step_ : unsigned = 1; Offset_ : unsigned = 0 );
begin
 Step  :=Step_;
 Offset:=Offset_;

 m_rbuf         :=NIL;
 m_mask_function:=MaskF;

end;

{ CONSTRUCT }
constructor amask_no_clip_u8.Construct(rbuf : rendering_buffer_ptr; MaskF : func_mask_calculate; Step_ : unsigned = 1; Offset_ : unsigned = 0 );
begin
 Step  :=Step_;
 Offset:=Offset_;

 m_rbuf         :=rbuf;
 m_mask_function:=MaskF;

end;

{ ATTACH }
procedure amask_no_clip_u8.attach;
begin
 m_rbuf:=rbuf;

end;

{ MASK_FUNCTION }
function amask_no_clip_u8.mask_function;
begin
 result:=@m_mask_function;

end;

{ PIXEL }
function amask_no_clip_u8.pixel;
begin
 result:=
  int8u(m_mask_function(
   int8u_ptr(ptrcomp(m_rbuf.row(y ) ) + (x * Step + Offset ) * sizeof(int8u ) ) ) );

end;

{ COMBINE_PIXEL }
function amask_no_clip_u8.combine_pixel;
begin
 result:=int8u(
  (cover_full + val *
    m_mask_function(
     int8u_ptr(ptrcomp(m_rbuf.row(y ) ) + (x * Step + Offset ) * sizeof(int8u ) ) )
  ) shr cover_shift );

end;

{ FILL_HSPAN }
procedure amask_no_clip_u8.fill_hspan;
var
 mask : int8u_ptr;

begin
 mask:=int8u_ptr(ptrcomp(m_rbuf.row(y ) ) + (x * Step + Offset ) * sizeof(int8u ) );

 repeat
  dst^:=int8u(m_mask_function(mask ) );

  inc(ptrcomp(dst ) ,sizeof(int8u ) );
  inc(ptrcomp(mask ) ,Step * sizeof(int8u ) );
  dec(num_pix );

 until num_pix = 0;

end;

{ COMBINE_HSPAN }
procedure amask_no_clip_u8.combine_hspan;
var
 mask : int8u_ptr;

begin
 mask:=int8u_ptr(ptrcomp(m_rbuf.row(y ) ) + (x * Step + Offset ) * sizeof(int8u ) );

 repeat
  dst^:=int8u((cover_full + dst^ * m_mask_function(mask ) ) shr cover_shift );

  inc(ptrcomp(dst ) ,sizeof(int8u ) );
  inc(ptrcomp(mask ) ,Step * sizeof(int8u ) );
  dec(num_pix );

 until num_pix = 0;

end;

{ FILL_VSPAN }
procedure amask_no_clip_u8.fill_vspan;
var
 mask : int8u_ptr;

begin
 mask:=int8u_ptr(ptrcomp(m_rbuf.row(y ) ) + (x * Step + Offset ) * sizeof(int8u ) );

 repeat
  dst^:=int8u(m_mask_function(mask ) );

  inc(ptrcomp(dst ) ,sizeof(int8u ) );
  inc(ptrcomp(mask ) ,m_rbuf._stride );
  dec(num_pix );

 until num_pix = 0;

end;

{ COMBINE_VSPAN }
procedure amask_no_clip_u8.combine_vspan;
var
 mask : int8u_ptr;

begin
 mask:=int8u_ptr(ptrcomp(m_rbuf.row(y ) ) + (x * Step + Offset ) * sizeof(int8u ) );

 repeat
  dst^:=int8u((cover_full + dst^ * m_mask_function(mask ) ) shr cover_shift );

  inc(ptrcomp(dst ) ,sizeof(int8u ) );
  inc(ptrcomp(mask ) ,m_rbuf._stride );
  dec(num_pix );

 until num_pix = 0;

end;

{ CONSTRUCT }
constructor amask_no_clip_gray8.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,1 ,0 );

end;

{ CONSTRUCT }
constructor amask_no_clip_rgb24r.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,3 ,0 );

end;

{ CONSTRUCT }
constructor amask_no_clip_rgb24g.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,3 ,1 );

end;

{ CONSTRUCT }
constructor amask_no_clip_rgb24b.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,3 ,2 );

end;

{ CONSTRUCT }
constructor amask_no_clip_bgr24r.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,3 ,2 );

end;

{ CONSTRUCT }
constructor amask_no_clip_bgr24g.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,3 ,1 );

end;

{ CONSTRUCT }
constructor amask_no_clip_bgr24b.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,3 ,0 );

end;

{ CONSTRUCT }
constructor amask_no_clip_rgba32r.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,0 );

end;

{ CONSTRUCT }
constructor amask_no_clip_rgba32g.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,1 );

end;

{ CONSTRUCT }
constructor amask_no_clip_rgba32b.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,2 );

end;

{ CONSTRUCT }
constructor amask_no_clip_rgba32a.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,3 );

end;

{ CONSTRUCT }
constructor amask_no_clip_argb32r.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,1 );

end;

{ CONSTRUCT }
constructor amask_no_clip_argb32g.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,2 );

end;

{ CONSTRUCT }
constructor amask_no_clip_argb32b.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,3 );

end;

{ CONSTRUCT }
constructor amask_no_clip_argb32a.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,0 );

end;

{ CONSTRUCT }
constructor amask_no_clip_bgra32r.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,2 );

end;

{ CONSTRUCT }
constructor amask_no_clip_bgra32g.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,1 );

end;

{ CONSTRUCT }
constructor amask_no_clip_bgra32b.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,0 );

end;

{ CONSTRUCT }
constructor amask_no_clip_bgra32a.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,3 );

end;

{ CONSTRUCT }
constructor amask_no_clip_abgr32r.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,3 );

end;

{ CONSTRUCT }
constructor amask_no_clip_abgr32g.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,2 );

end;

{ CONSTRUCT }
constructor amask_no_clip_abgr32b.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,1 );

end;

{ CONSTRUCT }
constructor amask_no_clip_abgr32a.Construct;
begin
 inherited Construct(rbuf ,@one_component_mask_u8 ,4 ,0 );

end;

{ CONSTRUCT }
constructor amask_no_clip_rgb24gray.Construct;
begin
 inherited Construct(rbuf ,@rgb_to_gray_mask_u8_012 ,3 ,0 );

end;

{ CONSTRUCT }
constructor amask_no_clip_bgr24gray.Construct;
begin
 inherited Construct(rbuf ,@rgb_to_gray_mask_u8_210 ,3 ,0 );

end;

{ CONSTRUCT }
constructor amask_no_clip_rgba32gray.Construct;
begin
 inherited Construct(rbuf ,@rgb_to_gray_mask_u8_012 ,4 ,0 );

end;

{ CONSTRUCT }
constructor amask_no_clip_argb32gray.Construct;
begin
 inherited Construct(rbuf ,@rgb_to_gray_mask_u8_012 ,4 ,1 );

end;

{ CONSTRUCT }
constructor amask_no_clip_bgra32gray.Construct;
begin
 inherited Construct(rbuf ,@rgb_to_gray_mask_u8_210 ,4 ,0 );

end;

{ CONSTRUCT }
constructor amask_no_clip_abgr32gray.Construct;
begin
 inherited Construct(rbuf ,@rgb_to_gray_mask_u8_210 ,4 ,1 );

end;

END.

