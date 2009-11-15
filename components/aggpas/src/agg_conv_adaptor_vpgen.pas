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
// 24.01.2006-Milano: Unit port establishment
//
{ agg_conv_adaptor_vpgen.pas }
unit
 agg_conv_adaptor_vpgen ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vertex_source ,
 agg_vpgen_segmentator ;

{ TYPES DEFINITION }
type
 conv_adaptor_vpgen = object(vertex_source )
   m_source  ,
   m_vpgen   : vertex_source_ptr;
   m_start_x ,
   m_start_y : double;

   m_poly_flags : unsigned;
   m_vertices   : int;

   constructor Construct(source ,gen : vertex_source_ptr );

   procedure set_source(source : vertex_source_ptr );

   function  vpgen : vpgen_segmentator_ptr;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor conv_adaptor_vpgen.Construct;
begin
 m_source :=source;
 m_vpgen  :=gen;
 m_start_x:=0;
 m_start_y:=0;

 m_poly_flags:=0;
 m_vertices  :=0;

end;

{ SET_SOURCE }
procedure conv_adaptor_vpgen.set_source;
begin
 m_source:=source;

end;

{ VPGEN }
function conv_adaptor_vpgen.vpgen;
begin
 result:=vpgen_segmentator_ptr(m_vpgen );

end;

{ REWIND }
procedure conv_adaptor_vpgen.rewind;
begin
 m_source.rewind(path_id );
 
 vpgen_segmentator_ptr(m_vpgen ).reset;

 m_start_x   :=0;
 m_start_y   :=0;
 m_poly_flags:=0;
 m_vertices  :=0;

end;

{ VERTEX }
function conv_adaptor_vpgen.vertex;
var
 cmd : unsigned;

 tx ,ty : double;

begin
 cmd:=path_cmd_stop;

 repeat
  cmd:=m_vpgen.vertex(x ,y );

  if not is_stop(cmd ) then
   break;

  if (m_poly_flags <> 0 ) and
     not vpgen_segmentator_ptr(m_vpgen )._auto_unclose then
   begin
    x^ :=0.0;
    y^ :=0.0;
    cmd:=m_poly_flags;

    m_poly_flags:=0;
    
    break;

   end;

  if m_vertices < 0 then
   begin
    if m_vertices < -1 then
     begin
      m_vertices:=0;

      result:=path_cmd_stop;

      exit;

     end;

    vpgen_segmentator_ptr(m_vpgen ).move_to(m_start_x ,m_start_y );

    m_vertices:=1;

    continue;

   end;

  cmd:=m_source.vertex(@tx ,@ty );

  if is_vertex(cmd ) then
   if is_move_to(cmd ) then
    begin
     if vpgen_segmentator_ptr(m_vpgen )._auto_close and
        (m_vertices > 2 ) then
      begin
       vpgen_segmentator_ptr(m_vpgen ).line_to(m_start_x ,m_start_y );

       m_poly_flags:=path_cmd_end_poly or path_flags_close;
       m_start_x   :=tx;
       m_start_y   :=ty;
       m_vertices  :=-1;

       continue;

      end;

     vpgen_segmentator_ptr(m_vpgen ).move_to(tx ,ty );

     m_start_x :=tx;
     m_start_y :=ty;
     m_vertices:=1;

    end
   else
    begin
     vpgen_segmentator_ptr(m_vpgen ).line_to(tx ,ty );

     inc(m_vertices );

    end
  else
   if is_end_poly(cmd ) then
    begin
     m_poly_flags:=cmd;

     if is_closed(cmd ) or
        vpgen_segmentator_ptr(m_vpgen )._auto_close then
      begin
       if vpgen_segmentator_ptr(m_vpgen )._auto_close then
        m_poly_flags:=m_poly_flags or path_flags_close;

       if m_vertices > 2 then
        vpgen_segmentator_ptr(m_vpgen ).line_to(m_start_x ,m_start_y );

       m_vertices:=0;

      end;

    end
   else
    begin
    // path_cmd_stop
     if vpgen_segmentator_ptr(m_vpgen )._auto_close and
        (m_vertices > 2 ) then
      begin
       vpgen_segmentator_ptr(m_vpgen ).line_to(m_start_x ,m_start_y );

       m_poly_flags:=path_cmd_end_poly or path_flags_close;
       m_vertices  :=-2;

       continue;

      end;

     break;

    end;

 until false;

 result:=cmd;

end;

END.

