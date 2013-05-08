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
  Classes, SysUtils, CodeCache, CodeToolManager, FileProcs, CodeAtom,
  BasicCodeTools, SourceChanger, CodeTree,
  CodeToolsStructs, PascalParserTool;

type
  TMode = (mList,mAdd,mRemove);
const
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
  Node: TCodeTreeNode;
  Signature: String;
  ProcNode: TCodeTreeNode;
  Changer: TSourceChangeCache;
  Beauty: TBeautifyCodeOptions;
  Indent: integer;
  HasEnterCall: Boolean;
  HasExitCall: Boolean;
  EnterInsertPos: Integer;
  ExitInsertPos: Integer;
  FromPos: objpas.Integer;
  ToPos: objpas.Integer;

procedure RemoveCall;
begin
  FromPos:=Tool.CurPos.StartPos;
  // read parameters
  Tool.ReadNextAtom;
  if Tool.CurPos.Flag<>cafRoundBracketOpen then
    Tool.RaiseException('( expected, but '+Tool.GetAtom+' found');
  Tool.ReadTilBracketClose(true);
  ToPos:=Tool.CurPos.EndPos;
  // read semicolon
  Tool.ReadNextAtom;
  if Tool.CurPos.Flag=cafSemicolon then
    ToPos:=Tool.CurPos.EndPos;
  // delete space and line break in front
  while Tool.Src[FromPos-1] in [' ',#9] do dec(FromPos);
  if Tool.Src[FromPos-1] in [#10,#13] then begin
    dec(FromPos);
    if (Tool.Src[FromPos-1] in [#10,#13]) and (Tool.Src[FromPos]<>Tool.Src[FromPos-1]) then
      dec(FromPos);
  end;
  // delete space behind
  while Tool.Src[ToPos] in [' ',#9] do inc(ToPos);
  if not Changer.Replace(gtNone,gtNone,FromPos,ToPos,'') then
    Exception.Create('source not writable');
end;

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

  // load the file
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('loading failed '+Filename);

  // parse the unit
  if not CodeToolBoss.Explore(Code,Tool,false) then
    raise Exception.Create('parser error');

  writeln('-----------------------------------------------');

  Changer:=CodeToolBoss.SourceChangeCache;
  Beauty:=Changer.BeautifyCodeOptions;
  Changer.MainScanner:=Tool.Scanner;

  // guess indent
  Indent:=Beauty.Indent;
  GuessIndentSize(Code.Source,Indent,Beauty.TabWidth);
  Beauty.Indent:=Indent;

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
        ProcNode:=Node.Parent;
        Signature:=Tool.ExtractProcHead(ProcNode,[phpWithoutSemicolon]);
        if Mode=mList then begin
          writeln('Signature: ',Signature);
        end else if (Mode in [mAdd,mRemove]) and (Signatures[Signature]<>'')
        then begin
          Tool.MoveCursorToNodeStart(Node);
          Tool.ReadNextAtom; // read 'begin'
          EnterInsertPos:=Tool.CurPos.EndPos;
          ExitInsertPos:=0;
          HasEnterCall:=false;
          HasExitCall:=false;
          while Tool.CurPos.StartPos<Node.EndPos do begin
            if Tool.AtomIs(EnterCall) then begin
              HasEnterCall:=true;
              if (Mode=mRemove) then begin
                // remove Enter call
                RemoveCall;
              end;
            end;
            if Tool.AtomIs(ExitCall) then begin
              HasExitCall:=true;
              if (Mode=mRemove) then begin
                // remove Exit call
                RemoveCall;
              end;
            end;
            if Tool.CurPos.Flag=cafEnd then
              ExitInsertPos:=Tool.CurPos.StartPos;
            Tool.ReadNextAtom;
          end;
          if (Mode=mAdd) then begin
            if (not HasEnterCall) then begin
              // add Enter call
              if not Changer.Replace(gtNewLine,gtNewLine,
                EnterInsertPos,EnterInsertPos,
                Beauty.GetIndentStr(Beauty.GetLineIndent(Tool.Src,Node.StartPos)+Indent)+EnterCall+'('''+Signature+''');')
              then
                Exception.Create('source not writable');
            end;
            if (not HasExitCall) then begin
              // add Exit call
              if not Changer.Replace(gtNewLine,gtNewLine,
                ExitInsertPos,ExitInsertPos,
                Beauty.GetIndentStr(Beauty.GetLineIndent(Tool.Src,Node.StartPos)+Indent)+ExitCall+'('''+Signature+''');')
              then
                Exception.Create('source not writable');
            end;
          end;
        end;
      end;
      Node:=Node.Next;
    end;
  end;

  if Mode in [mAdd,mRemove] then begin
    if not Changer.Apply then
      raise Exception.Create('source not writable');

    // write the new source:
    writeln('-----------------------------------');
    writeln('New source:');
    writeln(Code.Source);
    writeln('-----------------------------------');

    // save: if not Code.Save then ...
  end;
end.

