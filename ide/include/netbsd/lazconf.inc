// included by lazconf.pp

{***************************************************************************
                               lazconf.inc

  netbsd specific include file for the IDE

 ***************************************************************************

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}

{Should become unit Unix in the future. For both Linux and FreeBSD}
IFNDEF VER1_0
  {$IFDEF VER1_1}
  unix;
  {$ELSE}
  linux;
  {$ENDIF}

function GetDefaultTargetOS: string;
begin
  Result:='netbsd';
end;

procedure GetDefaultLCLLibPaths(List: TStrings);
begin
end;

{$I ../unix/lazbaseconf.inc}

{
  $Log$
  Revision 1.11  2004/08/20 09:47:36  mattias
  added darwin libpaths to Makefile and LCL Usage lib paths

  Revision 1.10  2003/12/20 01:20:52  mattias
  splitted output directories for cross compilation

  Revision 1.9  2003/11/03 16:57:47  peter
    * change $ifdef ver1_1 to $ifndef ver1_0 so it works also with
      fpc 1.9.x

  Revision 1.8  2003/08/15 14:01:20  mattias
  combined lazconf things for unix

  Revision 1.7  2003/03/26 11:39:08  mattias
  fixed rtl include path

  Revision 1.6  2003/02/07 17:49:21  mattias
  added ReadAllLinks

  Revision 1.5  2003/02/06 20:46:51  mattias
  default fpc src dirs and clean ups

  Revision 1.4  2002/12/20 11:08:47  mattias
  method resolution clause, class ancestor find declaration, 1.1. makros

  Revision 1.3  2002/07/01 05:53:31  lazarus
  MG: improved default make path for build lazarus

  Revision 1.2  2002/05/10 06:57:51  lazarus
  MG: updated licenses

  Revision 1.1  2002/01/22 19:02:05  marco
   * copied from FreeBSD. Lazarus starts up under NetBSD, and doesn't need changes

  Revision 1.4  2001/12/16 22:24:55  lazarus
  MG: changes for new compiler 20011216

  Revision 1.3  2001/12/10 08:44:23  lazarus
  MG: added search for compiler, if not set

  Revision 1.2  2001/05/27 11:52:01  lazarus
  MG: added --primary-config-path=<filename> cmd line option

  Revision 1.1  2001/01/14 20:50:23  marco
   * Initial version. Same as Linux, and probably will be that way.

}

