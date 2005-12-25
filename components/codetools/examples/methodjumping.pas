{ Copyright (C) 2005 Mattias Gaertner

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


  Abstract:
    Example, how to setup the codetools, load a pascal unit and jump from
    the declaration of a unit to its body.
}
program MethodJumping;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeToolManager, CodeCache;
  
var
  ExpandedFilename: String;
  CodeBuf: TCodeBuffer;
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
  RevertableJump: boolean;
begin
  ExpandedFilename:=ExpandFileName('tgeneric2.pp');
  CodeBuf:=CodeToolBoss.LoadFile(ExpandedFilename,true,false);
  CodeToolBoss.JumpToMethod(CodeBuf,10,8,NewCode,NewX,NewY,NewTopLine,
                            RevertableJump);
  writeln(NewCode.Filename,' ',NewX,',',NewY,' TopLine=',NewTopLine,' RevertableJump=',RevertableJump);
end.

