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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
 
  Abstract:
  
  This package (QuickFixExample.lpk) demonstrates:
    - How to write an IDE package.
      When You install it will register a Quick Fix item.
    - How to write Quick Fix item for compiler messages
      'Parameter "Sender" not used'
    - How to use the codetools to
      * parsing a unit
      * conversion of Filename,Line,Column to codetools source position
      * finding a codetools node at a cursor position
      * finding a procedure node and the begin..end node
      * creating a nice insertion position for a statement at the beginning of
        the begin..end block
      * getting the indentation of a line, so that the new line will
        work in sub procedure as well
      * inserting code with the codetools
}
unit QuickFixDemo1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Dialogs,
  CodeTree, PascalParserTool, SourceChanger, CodeCache, CodeToolManager,
  IDEMsgIntf, LazIDEIntf, IDEExternToolIntf;

type

  { TQuickFixParemNotUsedAddAssert }

  TQuickFixParemNotUsedAddAssert = class(TMsgQuickFix)
  public
    function IsApplicable(Msg: TMessageLine; out Identifier: string): boolean;
    procedure CreateMenuItems(Fixes: TMsgQuickFixes); override;
    procedure QuickFix({%H-}Fixes: TMsgQuickFixes; Msg: TMessageLine); override;
  end;

function ParseCode(Msg: TMessageLine;
  out CodeBuf: TCodeBuffer; out ACodeTool: TCodeTool): boolean;
function CaretToSourcePosition(ACodeTool: TCodeTool; CodeBuf: TCodeBuffer;
  Line, Column: integer; out CleanPos: Integer): boolean;

procedure Register;

implementation

function ParseCode(Msg: TMessageLine; out CodeBuf: TCodeBuffer; out
  ACodeTool: TCodeTool): boolean;
// commits any editor changes to the codetools, parses the unit
// and if there is a syntax error, tells the IDE jump to it
begin
  Result:=false;
  if Msg=nil then exit;
  CodeBuf:=CodeToolBoss.LoadFile(Msg.GetFullFilename,true,false);
  if CodeBuf=nil then exit;
  if not LazarusIDE.BeginCodeTools then begin
    DebugLn(['QuickFixDemo1 ParseCode failed because IDE busy']);
    exit;
  end;
  if not CodeToolBoss.Explore(CodeBuf,ACodeTool,false) then begin
    LazarusIDE.DoJumpToCodeToolBossError;
    exit;
  end;
  Result:=true;
end;

function CaretToSourcePosition(ACodeTool: TCodeTool;
  CodeBuf: TCodeBuffer; Line, Column: integer;
  out CleanPos: Integer): boolean;
// find the source position in the parsed
// The parsed source is the source combined of all include files
// stripped of irrelevant IFDEFs and parsed macros.
var
  CursorPos: TCodeXYPosition;
begin
  CursorPos.X:=Column;
  CursorPos.Y:=Line;
  CursorPos.Code:=CodeBuf;
  if ACodeTool.CaretToCleanPos(CursorPos,CleanPos)<>0 then begin
    DebugLn('QuickFixSenderParameterNotUsed invalid code position line=',dbgs(line),' col=',dbgs(Column));
    Result:=false;
  end else
    Result:=true;
end;

procedure Register;
begin
  MsgQuickFixes.RegisterQuickFix(TQuickFixParemNotUsedAddAssert.Create);
end;

{ TQuickFixParemNotUsedAddAssert }

function TQuickFixParemNotUsedAddAssert.IsApplicable(Msg: TMessageLine; out
  Identifier: string): boolean;
var
  Dummy: string;
begin
  Result:=false;
  // Check: Parameter "$1" not used
  if not IDEFPCParser.MsgLineIsId(Msg,5024,Identifier,Dummy) then
    exit;
  if not Msg.HasSourcePosition or not IsValidIdent(Identifier) then exit;
  Result:=true;
end;

procedure TQuickFixParemNotUsedAddAssert.CreateMenuItems(Fixes: TMsgQuickFixes);
var
  i: Integer;
  Msg: TMessageLine;
  Identifier: string;
begin
  for i:=0 to Fixes.LineCount-1 do begin
    Msg:=Fixes.Lines[i];
    if not IsApplicable(Msg,Identifier) then continue;
    Fixes.AddMenuItem(Self, Msg, 'Add Assert('+Identifier+'=nil);');
    exit;
  end;
end;

procedure TQuickFixParemNotUsedAddAssert.QuickFix(Fixes: TMsgQuickFixes;
  Msg: TMessageLine);
var
  Identifier: string;
  CodeBuf: TCodeBuffer;
  ACodeTool: TCodeTool;
  CleanPos: Integer;
  ProcNode: TCodeTreeNode;
  BeginNode: TCodeTreeNode;
  SourceChangeCache: TSourceChangeCache;
  InsertPos: Integer;
  Indent: Integer;
  Beauty: TBeautifyCodeOptions;
begin
  if not IsApplicable(Msg,Identifier) then exit;

  // parse the code and find the source position
  if not ParseCode(Msg,CodeBuf,ACodeTool) then exit;
  if not CaretToSourcePosition(ACodeTool,CodeBuf,Msg.Line,Msg.Column,CleanPos) then exit;

  // find procedure node
  ProcNode:=ACodeTool.FindDeepestNodeAtPos(CleanPos,false);
  if ProcNode<>nil then
    ProcNode:=ProcNode.GetNodeOfType(ctnProcedure);
  if ProcNode=nil then begin
    DebugLn('TQuickFixParemNotUsedAddAssert.QuickFix no procedure with begin..end at code position line=',dbgs(Msg.Line),' col=',dbgs(Msg.Column));
    exit;
  end;
  BeginNode:=ACodeTool.FindProcBody(ProcNode);
  if BeginNode=nil then begin
    // this procedure has no begin..end -> search corresponding pocedure node
    ProcNode:=ACodeTool.FindCorrespondingProcNode(ProcNode,[phpAddClassName,phpInUpperCase]);
    if ProcNode=nil then begin
      DebugLn('TQuickFixParemNotUsedAddAssert.QuickFix no corresponding procedure with begin..end at code position line=',dbgs(Msg.Line),' col=',dbgs(Msg.Column));
      exit;
    end;
    BeginNode:=ACodeTool.FindProcBody(ProcNode);
    if BeginNode=nil then begin
      DebugLn('TQuickFixParemNotUsedAddAssert.QuickFix corresponding procedure at code position line=',dbgs(Msg.Line),' col=',dbgs(Msg.Column),' has no begin..end');
      exit;
    end;
  end;

  // find insert postion after the 'begin' keyword
  SourceChangeCache:=CodeToolBoss.SourceChangeCache;
  SourceChangeCache.MainScanner:=ACodeTool.Scanner;
  InsertPos:=BeginNode.StartPos+length('begin');
  Beauty:=SourceChangeCache.BeautifyCodeOptions;

  // define a nice indentation
  Indent:=Beauty.GetLineIndent(ACodeTool.Src,InsertPos);
  inc(Indent,SourceChangeCache.BeautifyCodeOptions.Indent);

  // change source
  try
    if not SourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,
                              Beauty.GetIndentStr(Indent)+'Assert(Sender=nil);')
    then
      raise Exception.Create('TQuickFixParemNotUsedAddAssert.QuickFix insertion impossible');
    if not SourceChangeCache.Apply then
      raise Exception.Create('TQuickFixParemNotUsedAddAssert.QuickFix changing source failed');
    // message fixed -> clean
    Msg.Msg:='';
  except
    on E: Exception do begin
      MessageDlg('Error',E.Message,mtError,[mbCancel],0);
    end;
  end;
end;

end.

