{%MainUnit ../../lazconf.pp}

{***************************************************************************
                               lazconf.inc
                               
 linux specific include file for the IDE
   
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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
{For Linux specific implementations.
 General unix implementations are in ../unix/lazbaseconf.inc}
uses
  Unix;

procedure IfDirectoryExistsAdd(const Directory: string; List: TStrings);
begin
  if DirPathExists(Directory) then List.Add(Directory);
end;

procedure GetDefaultLCLLibPaths(List: TStrings);
begin
  // for SuSE
  IfDirectoryExistsAdd('/opt/gnome/lib',List);
end;

{$I ../unix/lazbaseconf.inc}

// included by lazconf.pp


