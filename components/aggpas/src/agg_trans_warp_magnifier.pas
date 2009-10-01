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
// 06.20.2006-Milano: Unit port establishment
//
{ agg_trans_warp_magnifier.pas }
unit
 agg_trans_warp_magnifier ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_trans_affine ;

{ TYPES DEFINITION }
type
 trans_warp_magnifier_ptr = ^trans_warp_magnifier;
 trans_warp_magnifier = object(trans_affine )
   m_xc ,
   m_yc ,

   m_magn   ,
   m_radius : double;

   constructor Construct;

   procedure center       (x ,y : double );
   procedure magnification(m : double );
   procedure radius       (r : double );

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ _transform }
procedure _transform(this : trans_warp_magnifier_ptr; x ,y : double_ptr );
var
 dx ,dy ,r ,m : double;

begin
 dx:=x^ - this.m_xc;
 dy:=y^ - this.m_yc;
 r :=Sqrt(dx * dx + dy * dy );

 if r < this.m_radius then
  begin
   x^:=this.m_xc + dx * this.m_magn;
   y^:=this.m_yc + dy * this.m_magn;

   exit;

  end;

 m:=(r + this.m_radius * (this.m_magn - 1.0 ) ) / r;

 x^:=this.m_xc + dx * m;
 y^:=this.m_yc + dy * m;

end;

{ _inverse_transform }
procedure _inverse_transform(this : trans_warp_magnifier_ptr; x ,y : double_ptr );
var
 t : trans_warp_magnifier;

begin
 t.Construct;

 t:=this^;

 t.magnification(1.0 / this.m_magn );
 t.radius       (this.m_radius * this.m_magn );
 t.transform    (@t ,x ,y );

end;

{ CONSTRUCT }
constructor trans_warp_magnifier.Construct;
begin
 inherited Construct;

 m_xc:=0.0;
 m_yc:=0.0;

 m_magn  :=1.0;
 m_radius:=1.0;

 transform        :=@_transform;
 inverse_transform:=@_inverse_transform;

end;

{ CENTER }
procedure trans_warp_magnifier.center;
begin
 m_xc:=x;
 m_yc:=y;

end;

{ MAGNIFICATION }
procedure trans_warp_magnifier.magnification;
begin
 m_magn:=m;

end;

{ RADIUS }
procedure trans_warp_magnifier.radius;
begin
 m_radius:=r;

end;

END.

