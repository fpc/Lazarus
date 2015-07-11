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
// 17.02.2006-Milano: Unit port establishment
//
{ agg_bitset_iterator.pas }
unit
 agg_bitset_iterator ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ;

{ TYPES DEFINITION }
type
 bitset_iterator = object
   m_bits : int8u_ptr;
   m_mask : int8u;

   constructor Construct(bits : int8u_ptr; offset : unsigned = 0 );

   procedure inc_operator;

   function  bit : unsigned;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor bitset_iterator.Construct;
begin
 m_bits:=int8u_ptr(ptrcomp(bits ) + (offset shr 3) * sizeof(int8u ) );
 m_mask:=($80 shr (offset and 7 ) );

end;

{ INC_OPERATOR }
procedure bitset_iterator.inc_operator;
begin
 m_mask:=m_mask shr 1;

 if m_mask = 0 then
  begin
   inc(ptrcomp(m_bits ) ,sizeof(int8u ) );

   m_mask:=$80;

  end;

end;

{ BIT }
function bitset_iterator.bit;
begin
 result:=m_bits^ and m_mask;

end;

END.

