(*
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
    Demonstration of how to add the missing IFDEFs, that h2pas forgets to add
    and to remove unneeded IFDEFs.
    
  Usage:
    fixh2pasdirectives [-name1] [+name2] ... [filename]
    
  Parameters starting with - are macros that should be set to 'undefined',
  that means {$IFNDEF NAME} will be true.
  Parameters starting with + are macros that should be set to 'defined',
  that means {$IFDEF NAME} will be true.
  All other macros are treated as 'any'.
  Names are case insensitive.
  Otherwise the parameter is used a filename to load.
  Example:
  
    ./fixh2pasdirectives -ENDIAN_BIG -Debug +FPC scanexamples/missingh2pasdirectives.pas
    
    This will set ENDIAN_BIG and Debug to undefined and FPC is set to '1'.
*)
program FixH2PASDirectives;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, FileProcs,
  CodeTree, DirectivesTree;
  
var
  Filename: string;
  Code: TCodeBuffer;
  Tree: TCompilerDirectivesTree;
  Changed: Boolean;
  Pass: Integer;
  i: Integer;
  p: String;
  Undefines: TStringList;
  Defines: TStringList;
begin
  Undefines:=nil;
  Defines:=nil;
  Filename:=SetDirSeparators('scanexamples/missingh2pasdirectives.pas');
  
  // parse parameters
  for i:=1 to ParamCount do begin
    p:=ParamStr(i);
    if p='' then continue;
    if p[1]='-' then begin
      if Undefines=nil then Undefines:=TStringList.Create;
      Undefines.Add(copy(p,2,length(p)));
    end
    else if p[1]='+' then begin
      if Defines=nil then Defines:=TStringList.Create;
      Defines.Add(copy(p,2,length(p)));
    end else
      Filename:=p;
  end;
  if Undefines<>nil then begin
    writeln('Undefines: ');
    writeln(Undefines.Text);
  end;
  if Defines<>nil then begin
    writeln('Defines: ');
    writeln(Defines.Text);
  end;

  // load the file
  Filename:=ExpandFileName(Filename);
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('loading failed '+Filename);

  // parse the directives
  Tree:=TCompilerDirectivesTree.Create;
  Tree.Parse(Code,CodeToolBoss.GetNestedCommentsFlagForFile(Code.Filename));
  writeln('-----------------------------------');
  writeln('h2pas created these directives:');
  Tree.WriteDebugReport;

  // add missing directives
  Changed:=false;
  Tree.FixMissingH2PasDirectives(Changed);
  writeln('-----------------------------------');
  writeln('after adding the missing directives:');
  Tree.WriteDebugReport;
  
  // reduce directives
  Pass:=0;
  repeat
    inc(Pass);
    Changed:=false;
    Tree.ReduceCompilerDirectives(Undefines,Defines,Changed);
    if not Changed then break;
    writeln('-----------------------------------');
    writeln('after reduce number ',Pass,':');
    Tree.WriteDebugReport;
  until false;
  
  writeln('-----------------------------------');
  writeln('Source:');
  writeln(Code.Source);
end.

