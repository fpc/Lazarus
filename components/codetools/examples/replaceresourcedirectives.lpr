{
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

  Author: Mattias Gaertner

  Abstract:
    Demonstration of how to reduce IFDEFs in a source file.
}
program ReplaceResourceDirectives;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, FileProcs,
  CodeTree, DirectivesTree;
  
var
  Filename: string;
  Code: TCodeBuffer;
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
begin
  // load the file
  if ParamCount>=1 then
    Filename:=ExpandFileName(ParamStr(1))
  else
    Filename:=ExpandFileName(SetDirSeparators('scanexamples/resourcetest1.pas'));
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('loading failed '+Filename);

  if not CodeToolBoss.AddResourceDirective(Code,'*.res',false,
    '{$IFDEF SomePlatform}{$R *.res}{$ENDIF}')
  then begin
    writeln('FAILED: unable to add resource');
    if CodeToolBoss.ErrorMessage<>'' then
      writeln('CodeToolBoss.ErrorMessage=',CodeToolBoss.ErrorMessage);
    halt;
  end;

  if not CodeToolBoss.FindResourceDirective(Code,1,1,
    NewCode,NewX,NewY,NewTopLine,'',false) then
  begin
    writeln('FAILED: did not find any resource directive');
    if CodeToolBoss.ErrorMessage<>'' then
      writeln('CodeToolBoss.ErrorMessage=',CodeToolBoss.ErrorMessage);
    halt;
  end;
  
  // write the new source:
  writeln('---------BEFORE REMOVE-------------');
  writeln(Code.Source);
  writeln('-----------------------------------');

  writeln(NewCode.Filename,' X=',NewX,' Y=',NewY);
  
  if not CodeToolBoss.RemoveDirective(NewCode,NewX,NewY,true) then begin
    writeln('FAILED to remove resource directive');
    if CodeToolBoss.ErrorMessage<>'' then
      writeln('CodeToolBoss.ErrorMessage=',CodeToolBoss.ErrorMessage);
    halt;
  end;

  // write the new source:
  writeln('---------AFTER REMOVE---------------');
  writeln(Code.Source);
  writeln('-----------------------------------');
end.

