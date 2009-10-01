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
// 24.04.2006-Milano: Unit port establishment
//
{ agg_svg_path_tokenizer.pas }
unit
 agg_svg_path_tokenizer ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_svg_exception ;

{ TYPES DEFINITION }
const
 s_commands   = '+-MmZzLlHhVvCcSsQqTtAaFfPp';
 s_numeric    = '.Ee0123456789';
 s_separators = ' ,'#9#10#13;

type
// SVG path tokenizer.
// Example:
//
// agg::svg::path_tokenizer tok;
//
// tok.set_str("M-122.304 84.285L-122.304 84.285 122.203 86.179 ");
// while(tok.next())
// {
//     printf("command='%c' number=%f\n",
//             tok.last_command(),
//             tok.last_number());
// }
//
// The tokenizer does all the routine job of parsing the SVG paths.
// It doesn't recognize any graphical primitives, it even doesn't know
// anything about pairs of coordinates (X,Y). The purpose of this class
// is to tokenize the numeric values and commands. SVG paths can
// have single numeric values for Horizontal or Vertical line_to commands
// as well as more than two coordinates (4 or 6) for Bezier curves
// depending on the semantics of the command.
// The behaviour is as follows:
//
// Each call to next() returns true if there's new command or new numeric
// value or false when the path ends. How to interpret the result
// depends on the sematics of the command. For example, command "C"
// (cubic Bezier curve) implies 6 floating point numbers preceded by this
// command. If the command assumes no arguments (like z or Z) the
// the last_number() values won't change, that is, last_number() always
// returns the last recognized numeric value, so does last_command().
 path_tokenizer_ptr = ^path_tokenizer;
 path_tokenizer = object
   m_separators_mask ,
   m_commands_mask   ,
   m_numeric_mask    : array[0..256 div 8 - 1 ] of char;

   m_path         : char_ptr;
   m_last_number  : double;
   m_last_command : char;

   constructor Construct;

   procedure set_path_str(str : char_ptr );

   function  next : boolean; overload;
   function  next(cmd : char ) : double; overload;

   function  last_command : char;
   function  last_number : double;

   procedure init_char_mask(mask ,char_set : char_ptr );

   function  contains_   (mask : char_ptr; c : unsigned ) : boolean;
   function  is_command  (c : unsigned ) : boolean;
   function  is_numeric  (c : unsigned ) : boolean;
   function  is_separator(c : unsigned ) : boolean;
   function  parse_number : boolean;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor path_tokenizer.Construct;
begin
 m_path        :=NIL;
 m_last_command:=#0;
 m_last_number :=0.0;

 init_char_mask(@m_commands_mask[0 ]   ,@s_commands[1 ] );
 init_char_mask(@m_numeric_mask[0 ]    ,@s_numeric[1 ] );
 init_char_mask(@m_separators_mask[0 ] ,@s_separators[1 ] );

end;

{ SET_PATH_STR }
procedure path_tokenizer.set_path_str;
begin
 m_path        :=str;
 m_last_command:=#0;
 m_last_number :=0.0;

end;

{ NEXT }
function path_tokenizer.next : boolean;
var
 buf : array[0..99 ] of char;

begin
 result:=false;

 if m_path = NIL then
  result:=false;
  
// Skip all white spaces and other garbage
 while (m_path^ <> #0 ) and
       not is_command(unsigned(m_path^ ) ) and
       not is_numeric(unsigned(m_path^ ) ) do
  begin
   if not is_separator(unsigned(m_path^ ) ) then
    begin
     sprintf(@buf[0 ] ,'path_tokenizer::next : Invalid Character %c' ,unsigned(m_path^ ) );

     raise svg_exception.Construct(PChar(@buf[0 ] ) );

    end;

   inc(ptrcomp(m_path ) );

  end;

 if m_path^ = #0 then
  exit;

 if is_command(unsigned(m_path^ ) ) then
  begin
  // Check if the command is a numeric sign character
   if (m_path^ = '-' ) or
      (m_path^ = '+' ) then
    begin
     result:=parse_number;

     exit;

    end;

   m_last_command:=m_path^;

   inc(ptrcomp(m_path ) );

   while (m_path^ <> #0 ) and
         is_separator(unsigned(m_path^ ) ) do
    inc(ptrcomp(m_path ) );

   if m_path^ = #0 then
    begin
     result:=true;

     exit;

    end;

  end;

 result:=parse_number;

end;

{ NEXT }
function path_tokenizer.next(cmd : char ) : double;
var
 buf : array[0..99 ] of char;

begin
 if not next then
  raise svg_exception.Construct(PChar('parse_path: Unexpected end of path' ) );

 if last_command <> cmd then
  begin
   sprintf(@buf[0 ] ,'parse_path: Command %c: bad or missing parameters' ,unsigned(cmd ) );

   raise svg_exception.Construct(PChar(@buf[0 ] ) );

  end;

 result:=last_number;

end;

{ LAST_COMMAND }
function path_tokenizer.last_command;
begin
 result:=m_last_command;

end;

{ LAST_NUMBER }
function path_tokenizer.last_number;
begin
 result:=m_last_number;

end;

{ INIT_CHAR_MASK }
procedure path_tokenizer.init_char_mask;
var
 c : unsigned;

begin
 fillchar(mask^ ,256 div 8 ,0 );

 while char_set^ <> #0 do
  begin
   c:=int8u_ptr(char_set )^;

   int8u_ptr(ptrcomp(mask ) + (c shr 3 ) )^:=
    int8u_ptr(ptrcomp(mask ) + (c shr 3 ) )^ or (1 shl (c and 7 ) );

   inc(ptrcomp(char_set ) );

  end;

end;

{ CONTAINS_ }
function path_tokenizer.contains_;
begin
 result:=
  (int8u_ptr(ptrcomp(mask ) + (c shr 3 ) and (256 div 8 - 1 ) )^ and
   (1 shl (c and 7 ) ) ) <> 0;

end;

{ IS_COMMAND }
function path_tokenizer.is_command;
begin
 result:=contains_(@m_commands_mask[0 ] ,c );

end;

{ IS_NUMERIC }
function path_tokenizer.is_numeric;
begin
 result:=contains_(@m_numeric_mask[0 ] ,c );

end;

{ IS_SEPARATOR }
function path_tokenizer.is_separator;
begin
 result:=contains_(@m_separators_mask[0 ] ,c );

end;

{ PARSE_NUMBER }
function path_tokenizer.parse_number;
var
 buf : array[0..255 ] of char; // Should be enough for any number

 buf_ptr : char_ptr;

begin
 buf_ptr:=@buf[0 ];

// Copy all sign characters
 while (ptrcomp(buf_ptr ) < ptrcomp(@buf[0 ] ) + 255 ) and
       ((m_path^ = '-' ) or
        (m_path^ = '+' ) ) do
  begin
   buf_ptr^:=m_path^;

   inc(ptrcomp(buf_ptr ) );
   inc(ptrcomp(m_path ) );

  end;

// Copy all numeric characters
 while (ptrcomp(buf_ptr ) < ptrcomp(@buf[0 ] ) + 255 ) and
       is_numeric(unsigned(m_path^ ) ) do
  begin
   buf_ptr^:=m_path^;

   inc(ptrcomp(buf_ptr ) );
   inc(ptrcomp(m_path ) );

  end;

 buf_ptr^:=#0;

 m_last_number:=get_double(pointer(PChar(@buf[0 ] ) ) );

 result:=true;

end;

END.

