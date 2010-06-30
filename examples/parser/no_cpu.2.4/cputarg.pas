{
    Copyright (c) 2001-2002 by Peter Vreman

    Includes the CPU dependent target units - by DoDi.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

(* The used units initialize global references.
*)

unit cputarg;

{$i fpcdefs.inc}

interface


implementation

    uses
      systems { prevent an syntax error when nothing is included }

{**************************************
             Targets
**************************************}

    {$ifndef NOTARGETLINUX}
      ,t_linux
    {$endif}
    {$ifndef NOTARGETAMIGA}
      ,t_amiga
    {$endif}
    {$ifndef NOTARGETPALMOS}
      ,t_palmos
    {$endif}

{**************************************
          Assembler Readers
**************************************}
{$IFDEF dummy}
      //,ra68k //normally included by ra68kmot
{$ELSE}
      ,ra68kmot - not required
{$ENDIF}

{**************************************
             Assemblers
**************************************}

      ,ag68kgas //register TAssembler class
      ;

end.
