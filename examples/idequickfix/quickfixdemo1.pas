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
  Classes, SysUtils, LCLProc, IDEMsgIntf, CodeTree, CodeAtom, BasicCodeTools,
  PascalParserTool, SourceChanger, LazIDEIntf, CodeCache, CodeToolManager,
  Dialogs;

procedure QuickFixSenderParameterNotUsed(Sender: TObject; Step: TIMQuickFixStep;
                                         Msg: TIDEMessageLine);
function GetMsgLineFilename(Msg: TIDEMessageLine;
                            out CodeBuf: TCodeBuffer): boolean;
function ParseCode(CodeBuf: TCodeBuffer; out ACodeTool: TCodeTool): boolean;
function CaretToSourcePosition(ACodeTool: TCodeTool; CodeBuf: TCodeBuffer;
  Line, Column: integer; out CleanPos: Integer): boolean;

procedure Register;

implementation

procedure QuickFixSenderParameterNotUsed(Sender: TObject;
  Step: TIMQuickFixStep; Msg: TIDEMessageLine);
// Adds a 'Assert(Sender=nil);' line at the beginning of the begin..end block
// of the procedure of the compiler messages 'Parameter "Sender" not used'.
var
  CodeBuf: TCodeBuffer;
  Line: LongInt;
  ACodeTool: TCodeTool;
  Column: LongInt;
  CleanPos: Integer;
  ProcNode: TCodeTreeNode;
  SourceChangeCache: TSourceChangeCache;
  InsertPos: Integer;
  Indent: LongInt;
  BeginNode: TCodeTreeNode;
begin
  if Step<>imqfoMenuItem then exit;

  // load the file
  if not GetMsgLineFilename(Msg,CodeBuf) then exit;

  // get line and column
  Line:=StrToIntDef(Msg.Parts.Values['Line'],0);
  Column:=StrToIntDef(Msg.Parts.Values['Column'],0);
  if (Line<1) then begin
    DebugLn('QuickFixSenderParameterNotUsed Line=',dbgs(Line));
    exit;
  end;

  // parse the code and find the source position
  if not ParseCode(CodeBuf,ACodeTool) then exit;
  if not CaretToSourcePosition(ACodeTool,CodeBuf,Line,Column,CleanPos) then exit;
  
  // find procedure node
  ProcNode:=ACodeTool.FindDeepestNodeAtPos(CleanPos,false);
  if ProcNode<>nil then
    ProcNode:=ProcNode.GetNodeOfType(ctnProcedure);
  if ProcNode=nil then begin
    DebugLn('QuickFixSenderParameterNotUsed no procedure with begin..end at code position line=',dbgs(line),' col=',dbgs(Column));
    exit;
  end;
  BeginNode:=ACodeTool.FindProcBody(ProcNode);
  if BeginNode=nil then begin
    // this procedure has no begin..end -> search corresponding pocedure node
    ProcNode:=ACodeTool.FindCorrespondingProcNode(ProcNode,[phpAddClassName,phpInUpperCase]);
    if ProcNode=nil then begin
      DebugLn('QuickFixSenderParameterNotUsed no corresponding procedure with begin..end at code position line=',dbgs(line),' col=',dbgs(Column));
      exit;
    end;
    BeginNode:=ACodeTool.FindProcBody(ProcNode);
    if BeginNode=nil then begin
      DebugLn('QuickFixSenderParameterNotUsed corresponding procedure at code position line=',dbgs(line),' col=',dbgs(Column),' has no begin..end');
      exit;
    end;
  end;

  // find insert postion after the 'begin' keyword
  SourceChangeCache:=CodeToolBoss.SourceChangeCache;
  SourceChangeCache.MainScanner:=ACodeTool.Scanner;
  InsertPos:=BeginNode.StartPos+length('begin');

  // define a nice indentation
  Indent:=GetLineIndent(ACodeTool.Src,InsertPos);
  inc(Indent,SourceChangeCache.BeautifyCodeOptions.Indent);

  // change source
  try
    if not SourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,
                                     GetIndentStr(Indent)+'Assert(Sender=nil);')
    then
      raise Exception.Create('QuickFixSenderParameterNotUsed insertion impossible');
    if not SourceChangeCache.Apply then
      raise Exception.Create('QuickFixSenderParameterNotUsed changing source failed');
    // message fixed -> clean
    Msg.Msg:='';
  except
    on E: Exception do begin
      MessageDlg('Error',E.Message,mtError,[mbCancel],0);
    end;
  end;
end;

function GetMsgLineFilename(Msg: TIDEMessageLine; out CodeBuf: TCodeBuffer
  ): boolean;
// loads the file of the message
var
  Filename: String;
begin
  Result:=false;
  CodeBuf:=nil;
  if Msg.Parts=nil then begin
    DebugLn('GetMsgLineFilename Msg.Parts=nil');
    exit;
  end;

  Filename:=Msg.Parts.Values['Filename'];
  //DebugLn('GetMsgLineFilename Filename=',Filename,' ',Msg.Parts.Text);
  CodeBuf:=CodeToolBoss.LoadFile(Filename,false,false);
  if CodeBuf=nil then begin
    DebugLn('GetMsgLineFilename Filename "',Filename,'" not found.');
    exit;
  end;
  Result:=true;
end;

function ParseCode(CodeBuf: TCodeBuffer; out ACodeTool: TCodeTool): boolean;
// commits any editor changes to the codetools, parses the unit
// and if there is a syntax error, tells the IDE jump to it
begin
  LazarusIDE.SaveSourceEditorChangesToCodeCache(nil);
  if not CodeToolBoss.Explore(CodeBuf,ACodeTool,false) then begin
    LazarusIDE.DoJumpToCodeToolBossError;
    Result:=false;
  end else begin
    Result:=true;
  end;
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
  RegisterIDEMsgQuickFix('Parameter Sender not used',
    'Quick fix: Add Assert(Sender=nil) dummy line',
    'Parameter "Sender" not used',[imqfoMenuItem],
    nil,@QuickFixSenderParameterNotUsed);
end;

end.

