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
// 20.12.2005-Milano: Unit port establishment
//
{ agg_conv_adaptor_vcgen.pas }
unit
 agg_conv_adaptor_vcgen ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_vertex_source ;

{ TYPES DEFINITION }
type
//------------------------------------------------------------null_markers
 null_markers_ptr = ^null_markers;
 null_markers = object(vertex_source )
   m_markers : vertex_source_ptr;

   constructor Construct;

   procedure remove_all; virtual;
   procedure add_vertex(x ,y : double; cmd : unsigned ); virtual;
   procedure prepare_src;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

   procedure set_markers(m : vertex_source_ptr );

  end;

//------------------------------------------------------conv_adaptor_vcgen
 status = (initial ,accumulate ,generate );

 conv_adaptor_vcgen_ptr = ^conv_adaptor_vcgen;
 conv_adaptor_vcgen = object(vertex_source )
   m_source    ,
   m_generator : vertex_source_ptr;
   m_markers   : null_markers;
   m_status    : status;
   m_last_cmd  : unsigned;
   m_start_x   ,
   m_start_y   : double;

   constructor Construct(source ,gen : vertex_source_ptr );

   procedure set_source (source : vertex_source_ptr );
   procedure set_markers(m : vertex_source_ptr );

   function  generator : vertex_source_ptr;
   function  markers : vertex_source_ptr;

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor null_markers.Construct;
begin
 inherited Construct;

 m_markers:=NIL;

end;

{ REMOVE_ALL }
procedure null_markers.remove_all;
begin
 if m_markers <> NIL then
  m_markers.remove_all;

end;

{ ADD_VERTEX }
procedure null_markers.add_vertex;
begin
 if m_markers <> NIL then
  m_markers.add_vertex(x ,y ,cmd );

end;

{ PREPARE_SRC }
procedure null_markers.prepare_src;
begin
end;

{ REWIND }
procedure null_markers.rewind;
begin
 if m_markers <> NIL then
  m_markers.rewind(path_id );

end;

{ VERTEX }
function null_markers.vertex;
begin
 if m_markers <> NIL then
  result:=m_markers.vertex(x ,y )
 else
  result:=path_cmd_stop;

end;

{ SET_MARKERS }
procedure null_markers.set_markers;
begin
 m_markers:=m;

end;

{ CONSTRUCT }
constructor conv_adaptor_vcgen.Construct;
begin
 inherited Construct;

 m_source:=source;
 m_status:=initial;

 m_generator:=gen;

 m_markers.Construct;

 m_last_cmd:=0;
 m_start_x :=0;
 m_start_y :=0;

end;

{ SET_SOURCE }
procedure conv_adaptor_vcgen.set_source;
begin
 m_source:=source;

end;

{ SET_MARKERS }
procedure conv_adaptor_vcgen.set_markers;
begin
 m_markers.set_markers(m );

end;

{ GENERATOR }
function conv_adaptor_vcgen.generator;
begin
 result:=m_generator;

end;

{ MARKERS }
function conv_adaptor_vcgen.markers;
begin
 if m_markers.m_markers <> NIL then
  result:=m_markers.m_markers
 else
  result:=@m_markers;

end;

{ REWIND }
procedure conv_adaptor_vcgen.rewind;
begin
 m_source.rewind(path_id );

 m_status:=initial;

end;

{ VERTEX }
function conv_adaptor_vcgen.vertex;
var
 cmd  : unsigned;
 done : boolean;

label
 _acc ,_gen ,_end ; 

begin
 cmd :=path_cmd_stop;
 done:=false;

 while not done do
  begin
   case m_status of
    initial :
     begin
      m_markers.remove_all;

      m_last_cmd:=m_source.vertex(@m_start_x ,@m_start_y );
      m_status  :=accumulate;

      goto _acc;

     end;

    accumulate :
     begin
     _acc:
      if is_stop(m_last_cmd ) then
       begin
        result:=path_cmd_stop;

        exit;

       end;

      m_generator.remove_all;
      m_generator.add_vertex(m_start_x ,m_start_y ,path_cmd_move_to );
      m_markers.add_vertex  (m_start_x ,m_start_y ,path_cmd_move_to );

      repeat
       cmd:=m_source.vertex(x ,y );

       if is_vertex(cmd ) then
        begin
         m_last_cmd:=cmd;

         if is_move_to(cmd ) then
          begin
           m_start_x:=x^;
           m_start_y:=y^;

           break;

          end;

         m_generator.add_vertex(x^ ,y^ ,cmd );
         m_markers.add_vertex(x^ ,y^ ,path_cmd_line_to );

        end
       else
        begin
         if is_stop(cmd ) then
          begin
           m_last_cmd:=path_cmd_stop;

           break;

          end;

         if is_end_poly(cmd ) then
          begin
           m_generator.add_vertex(x^ ,y^ ,cmd );

           break;

          end;

        end;

      until false;

      m_generator.rewind(0 );

      m_status:=generate;

      goto _gen;

     end;

    generate :
     begin
     _gen:
      cmd:=m_generator.vertex(x ,y );

      if is_stop(cmd ) then
       begin
        m_status:=accumulate;

        goto _end;

       end;

      done:=true; 

     end;

   end;

  _end:
  end;

 result:=cmd;

end;

END.

