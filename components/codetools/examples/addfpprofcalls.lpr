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
    Command line utility to add/remove fpprofiler calls to functions.

  Usage:
    ./addfpprofcalls -h
}
program addfpprofcalls;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, FileProcs, AVL_Tree, CodeAtom,
  BasicCodeTools, SourceChanger, CodeTree, FindDeclarationTool,
  CodeToolsStructs, PascalParserTool;

type
  TMode = (mList,mAdd,mRemove);
const
  ConfigFilename = 'codetools.config';
  EnterCall = 'SendMethodEnter';
  ExitCall = 'SendMethodExit';
var
  i: Integer;
  Param: String;
  Filename: String;
  Signatures: TStringToStringTree;
  Tool: TCodeTool;
  Mode: TMode;
  Code: TCodeBuffer;
  Node: CodeTree.TCodeTreeNode;
  Signature: String;
begin
  Mode:=mList;
  Filename:='';
  Signatures:=TStringToStringTree.Create(false);
  for i:=1 to ParamCount do begin
    Param:=ParamStrUTF8(i);
    if (Param='-h') or (Param='-?') then begin
      writeln('addfpprofcalls');
      writeln;
      writeln('List function signatures, or add or remove fpprofiler calls to selected functions.');
      writeln;
      writeln('Usage: <options> <unit file name> <function signature> <function signature> ...');
      writeln('  -h : write this help');
      writeln('  -r : remove calls instead of list');
      writeln('  -a : add calls instead of list');
      writeln;
      writeln('Example');
      writeln('  List all function signatures of unit1.pas');
      writeln('    ',ParamStrUTF8(0),' unit1.pas');
      writeln('  Add fpprofiler calls to TForm.Button1Click:');
      writeln('    ',ParamStrUTF8(0),' -a unit1.pas TForm1.Button1Click(:TObject)');
      writeln('  Remove fpprofiler calls from TForm.Button1Click:');
      writeln('    ',ParamStrUTF8(0),' -r unit1.pas TForm1.Button1Click(:TObject)');
      writeln;
      writeln('Before:');
      writeln('=======');
      writeln('procedure TMainForm.Button1Clicked(...)');
      writeln('begin');
      writeln('  // do something here');
      writeln('end;');
      writeln;
      writeln('After:');
      writeln('======');
      writeln('procedure TMainForm.Button1Clicked(...)');
      writeln('begin');
      writeln('  SendMethodEnter(''TMainForm.Button1Clicked'');');
      writeln('  // do something here');
      writeln('  SendMethodExit(''TMainForm.Button1Clicked'');');
      writeln('end;');
      Halt;
    end else if Param='-r' then
      Mode:=mRemove
    else if Param='-a' then
      Mode:=mAdd
    else if (Param='') or (Param[1]='-') then begin
      writeln('ERROR: invalid parameter ',Param);
      Halt;
    end else begin
      if Filename='' then
        Filename:=ExpandFileNameUTF8(Param)
      else
        Signatures[Param]:='1';
    end;
  end;

  CodeToolBoss.SimpleInit(ConfigFilename);

  // load the file
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('loading failed '+Filename);

  // parse the unit
  if not CodeToolBoss.Explore(Code,Tool,false) then
    raise Exception.Create('parser error');

  writeln('-----------------------------------------------');
  Node:=Tool.FindImplementationNode;
  if Node=nil then
    Node:=Tool.Tree.Root;
  while Node<>nil do begin
    if Node.Desc in (AllDefinitionSections+AllIdentifierDefinitions+[ctnInterface]) then
      Node:=Node.NextSkipChilds
    else begin
      if (Node.Desc=ctnBeginBlock) and (Node.Parent<>nil)
      and (Node.Parent.Desc=ctnProcedure) then begin
        // procedure body
        Signature:=Tool.ExtractProcHead(Node.Parent,[phpWithoutSemicolon]);
        if Mode=mList then begin
          writeln('Signature: ',Signature);
        end else if (Mode in [mAdd,mRemove]) and (Signatures[Signature]<>'')
        then begin

        end;
      end;
      Node:=Node.Next;
    end;
  end;

  if Mode in [mAdd,mRemove] then begin
    // write the new source:
    writeln('-----------------------------------');
    writeln('New source:');
    writeln(Code.Source);
    writeln('-----------------------------------');
  end;
end.

