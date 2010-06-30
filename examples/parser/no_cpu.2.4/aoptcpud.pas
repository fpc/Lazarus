{
    Copyright (c) 1998-2004 by Jonas Maebe, member of the Free Pascal
    Development Team

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

(* This is the required (by?) Optimizing CPU definition.
  For an parser it has nothing to do.
*)

Unit aoptcpud;

{$i fpcdefs.inc}

  interface

{$IFDEF dummy}
{$ELSE}
    uses
      aoptda;

    type
      TAOptDFACpu = class(TAOptDFA)
      end;
{$ENDIF}

  implementation

end.
