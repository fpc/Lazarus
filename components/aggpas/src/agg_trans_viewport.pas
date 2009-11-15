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
// Viewport transformer - simple orthogonal conversions from world coordinates
//                        to screen (device) ones
//
// [Pascal Port History] -----------------------------------------------------
//
// 27.09.2005-Milano: Complete unit port
//
{ agg_trans_viewport.pas }
unit
 agg_trans_viewport ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_trans_affine ;

{ TYPES DEFINITION }
type
 aspect_ratio_e = (aspect_ratio_stretch ,aspect_ratio_meet ,aspect_ratio_slice );

//----------------------------------------------------------trans_viewport
 trans_viewport_ptr = ^trans_viewport;
 trans_viewport = object(trans_affine )
   m_world_x1 ,
   m_world_y1 ,
   m_world_x2 ,
   m_world_y2 ,

   m_device_x1 ,
   m_device_y1 ,
   m_device_x2 ,
   m_device_y2 : double;

   m_aspect   : aspect_ratio_e;
   m_is_valid : boolean;

   m_align_x ,
   m_align_y ,

   m_wx1 ,
   m_wy1 ,
   m_wx2 ,
   m_wy2 ,
   m_dx1 ,
   m_dy1 ,

   m_kx ,
   m_ky : double;

   constructor Construct;

   procedure preserve_aspect_ratio(alignx ,aligny : double; aspect : aspect_ratio_e );

   procedure device_viewport   (x1 ,y1 ,x2 ,y2 : double );
   procedure device_viewport_ex(var x1 ,y1 ,x2 ,y2 : double );

   procedure world_viewport   (x1 ,y1 ,x2 ,y2 : double );
   procedure world_viewport_ex(var x1 ,y1 ,x2 ,y2 : double );

   procedure world_viewport_actual(var x1 ,y1 ,x2 ,y2 : double );

   function  is_valid : boolean;
   function  align_x : double;
   function  align_y : double;
   function  aspect_ratio : aspect_ratio_e;

   procedure inverse_transform_scale_only(x ,y : double_ptr );

   function  device_dx : double;
   function  device_dy : double;
   function  scale_x : double;
   function  scale_y : double;
   function  scale : double;

   procedure to_affine           (mtx : trans_affine_ptr );
   procedure to_affine_scale_only(mtx : trans_affine_ptr );

   function  byte_size : unsigned;
   procedure serialize  (ptr : int8u_ptr );
   procedure deserialize(ptr : int8u_ptr);

   procedure update;

  end;

{ GLOBAL VARIABLES & CONSTANTS }
{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ UNIT IMPLEMENTATION }
{ _transform }
procedure _transform(this : trans_viewport_ptr; x ,y : double_ptr );
begin
 x^:=x^ * this.m_kx;
 y^:=y^ * this.m_ky;

end;

{ _inverse_transform }
procedure _inverse_transform(this : trans_viewport_ptr; x ,y : double_ptr );
begin
 x^:=(x^ - this.m_dx1 ) / this.m_kx + this.m_wx1;
 y^:=(y^ - this.m_dy1 ) / this.m_ky + this.m_wy1;

end;

{ CONSTRUCT }
constructor trans_viewport.Construct;
begin
 inherited Construct;

 transform        :=@_transform;
 inverse_transform:=@_inverse_transform;

 m_world_x1:=0;
 m_world_y1:=0;
 m_world_x2:=1;
 m_world_y2:=1;

 m_device_x1:=0;
 m_device_y1:=0;
 m_device_x2:=1;
 m_device_y2:=1;

 m_aspect  :=aspect_ratio_stretch;
 m_is_valid:=true;

 m_align_x:=0.5;
 m_align_y:=0.5;

 m_wx1:=0;
 m_wy1:=0;
 m_wx2:=1;
 m_wy2:=1;
 m_dx1:=0;
 m_dy1:=0;

 m_kx:=1;
 m_ky:=1;

end;

{ preserve_aspect_ratio }
procedure trans_viewport.preserve_aspect_ratio;
begin
 m_align_x:=alignx;
 m_align_y:=aligny;
 m_aspect :=aspect;

 update;

end;

{ device_viewport }
procedure trans_viewport.device_viewport;
begin
 m_device_x1:=x1;
 m_device_y1:=y1;
 m_device_x2:=x2;
 m_device_y2:=y2;

 update;

end;

{ device_viewport_ex }
procedure trans_viewport.device_viewport_ex;
begin
 x1:=m_device_x1;
 y1:=m_device_y1;
 x2:=m_device_x2;
 y2:=m_device_y2;

end;

{ world_viewport }
procedure trans_viewport.world_viewport;
begin
 m_world_x1:=x1;
 m_world_y1:=y1;
 m_world_x2:=x2;
 m_world_y2:=y2;

 update;

end;

{ world_viewport_ex }
procedure trans_viewport.world_viewport_ex;
begin
 x1:=m_world_x1;
 y1:=m_world_y1;
 x2:=m_world_x2;
 y2:=m_world_y2;

end;

{ world_viewport_actual }
procedure trans_viewport.world_viewport_actual;
begin
 x1:=m_wx1;
 y1:=m_wy1;
 x2:=m_wx2;
 y2:=m_wy2;

end;

{ is_valid }
function trans_viewport.is_valid;
begin
 result:=m_is_valid;

end;

{ align_x }
function trans_viewport.align_x;
begin
 result:=m_align_x;

end;

{ align_y }
function trans_viewport.align_y;
begin
 result:=m_align_y;

end;

{ aspect_ratio }
function trans_viewport.aspect_ratio;
begin
 result:=m_aspect;

end;

{ inverse_transform_scale_only }
procedure trans_viewport.inverse_transform_scale_only;
begin
 x^:=x^ / m_kx;
 y^:=y^ / m_ky;

end;

{ device_dx }
function trans_viewport.device_dx;
begin
 result:=m_dx1 - m_wx1 * m_kx;

end;

{ device_dy }
function trans_viewport.device_dy;
begin
 result:=m_dy1 - m_wy1 * m_ky;

end;

{ scale_x }
function trans_viewport.scale_x;
begin
 result:=m_kx;

end;

{ scale_y }
function trans_viewport.scale_y;
begin
 result:=m_ky;

end;

{ scale }
function trans_viewport.scale;
begin
 result:=(m_kx + m_ky ) * 0.5;

end;

{ to_affine }
procedure trans_viewport.to_affine;
var
 m ,
 t : trans_affine_translation;
 s : trans_affine_scaling;

begin
 m.Construct(-m_wx1 ,-m_wy1 );
 s.Construct(m_kx ,m_ky );
 m.multiply (@s );
 t.Construct(m_dx1 ,m_dy1 );
 m.multiply (@t );

 mtx.assign(@m );

end;

{ to_affine_scale_only }
procedure trans_viewport.to_affine_scale_only;
var
 s : trans_affine_scaling;

begin
 s.Construct(m_kx ,m_ky );

 mtx.assign(@s );

end;

{ byte_size }
function trans_viewport.byte_size;
begin
 result:=sizeof(self );

end;

{ serialize }
procedure trans_viewport.serialize;
begin
 move(self ,ptr^ ,sizeof(self ) ); 

end;

{ deserialize }
procedure trans_viewport.deserialize;
begin
 move(ptr^ ,self ,sizeof(self ) );

end;

{ update }
procedure trans_viewport.update;
const
 epsilon : double = 1e-30;

var
 d ,

 world_x1 ,
 world_y1 ,
 world_x2 ,
 world_y2 ,

 device_x1 ,
 device_y1 ,
 device_x2 ,
 device_y2 : double;

begin
 if (Abs(m_world_x1  - m_world_x2 )  < epsilon ) or
    (Abs(m_world_y1  - m_world_y2 )  < epsilon ) or
    (Abs(m_device_x1 - m_device_x2 ) < epsilon ) or
    (Abs(m_device_y1 - m_device_y2 ) < epsilon ) then
  begin
   m_wx1:=m_world_x1;
   m_wy1:=m_world_y1;
   m_wx2:=m_world_x1 + 1;
   m_wy2:=m_world_y2 + 1;
   m_dx1:=m_device_x1;
   m_dy1:=m_device_y1;
   m_kx :=1;
   m_ky :=1;

   m_is_valid:=false;

  end
 else
  begin
   world_x1 :=m_world_x1;
   world_y1 :=m_world_y1;
   world_x2 :=m_world_x2;
   world_y2 :=m_world_y2;
   device_x1:=m_device_x1;
   device_y1:=m_device_y1;
   device_x2:=m_device_x2;
   device_y2:=m_device_y2;

   if not (m_aspect = aspect_ratio_stretch ) then
    begin
     m_kx:=(device_x2 - device_x1 ) / (world_x2 - world_x1 );
     m_ky:=(device_y2 - device_y1 ) / (world_y2 - world_y1 );

     if (m_aspect = aspect_ratio_meet ) = (m_kx < m_ky ) then
      begin
       d:=(world_y2 - world_y1 ) * m_ky / m_kx;

       world_y1:=world_y1 + ((world_y2 - world_y1 - d ) * m_align_y );
       world_y2:=world_y1 + d;

      end
     else
      begin
       d:=(world_x2 - world_x1 ) * m_kx / m_ky;

       world_x1:=world_x1 + ((world_x2 - world_x1 - d ) * m_align_x );
       world_x2:=world_x1 + d;
       
      end;

    end;

   m_wx1:=world_x1;
   m_wy1:=world_y1;
   m_wx2:=world_x2;
   m_wy2:=world_y2;
   m_dx1:=device_x1;
   m_dy1:=device_y1;
   m_kx :=(device_x2 - device_x1 ) / (world_x2 - world_x1 );
   m_ky :=(device_y2 - device_y1 ) / (world_y2 - world_y1 );

   m_is_valid:=true;

  end;

end;

END.

