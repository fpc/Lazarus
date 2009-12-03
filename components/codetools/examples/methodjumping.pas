{ Copyright (C) 2005 Mattias Gaertner

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

  Abstract:
    Example, how to setup the codetools, load a pascal unit and jump from
    the declaration of a unit to its body.
}
program MethodJumping;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeToolManager, CodeCache, CustomCodeTool, FileProcs;
  
var
  ExpandedFilename: String;
  CodeBuf: TCodeBuffer;
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
  RevertableJump: boolean;
begin
  ExpandedFilename:=ExpandFileNameUTF8('scanexamples/methodjump1.pas');
  CodeBuf:=CodeToolBoss.LoadFile(ExpandedFilename,true,false);
  if CodeBuf=nil then
    raise Exception.Create('failed loading '+ExpandedFilename);
  if CodeToolBoss.JumpToMethod(CodeBuf,14,10,NewCode,NewX,NewY,NewTopLine,
                               RevertableJump)
  then
    writeln(NewCode.Filename,' ',NewX,',',NewY,' TopLine=',NewTopLine,
            ' RevertableJump=',RevertableJump)
  else
    writeln('Method body not found.');
end.

