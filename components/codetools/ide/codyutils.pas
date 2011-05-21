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
    Common functions.
}
unit CodyUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Controls,
  // IDEIntf
  IDEDialogs, LazIDEIntf, SrcEditorIntf,
  // codetools
  CodeAtom, FileProcs, CodeToolManager, CodeCache, SourceLog, BasicCodeTools,
  EventCodeTool, LinkScanner, PascalParserTool, CodeTree, SourceChanger,
  CodeBeautifier,
  CodyStrConsts;

type
  { TCody }

  TCody = class
  public
    procedure DecodeLoaded(Sender: TSourceLog; const Filename: string;
                           var Source, DiskEncoding, MemEncoding: string);
  end;

var
  Cody: TCody;

type
  TCUParseError = (
    cupeNoSrcEditor,
    cupeMainCodeNotFound, // the file of the unit start was not found
    cupeParseError,
    cupeCursorNotInCode, // e.g. in front of the keyword 'unit'
    cupeSuccess
    );

procedure RemoveWithBlockCmd(Sender: TObject);
procedure InsertFileAtCursor(Sender: TObject);
procedure AddCallInherited(Sender: TObject);

function ParseTilCursor(out Tool: TCodeTool; out CleanPos: integer;
   out Node: TCodeTreeNode; out ErrorHandled: boolean;
   JumpToError: boolean; CodePos: PCodeXYPosition = nil): TCUParseError;

implementation

procedure RemoveWithBlockCmd(Sender: TObject);

  procedure ErrorNotInWithVar;
  begin
    IDEMessageDialog(crsCWError,
      crsCWPleasePlaceTheCursorOfTheSourceEditorOnAWithVariab,
      mtError,[mbCancel]);
  end;

var
  SrcEdit: TSourceEditorInterface;
begin
  // commit changes form source editor to codetools
  if not LazarusIDE.BeginCodeTools then exit;
  // check context at cursor
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then begin
    ErrorNotInWithVar;
    exit;
  end;
  if not CodeToolBoss.RemoveWithBlock(SrcEdit.CodeToolsBuffer as TCodeBuffer,
    SrcEdit.CursorTextXY.X,SrcEdit.CursorTextXY.Y)
  then begin
    // syntax error or not in a class
    if CodeToolBoss.ErrorMessage<>'' then
      LazarusIDE.DoJumpToCodeToolBossError
    else
      ErrorNotInWithVar;
    exit;
  end;
end;

procedure InsertFileAtCursor(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  Filter: String;
  Filename: String;
  Code: TCodeBuffer;
  SrcEdit: TSourceEditorInterface;
begin
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then exit;

  OpenDialog:=TOpenDialog.Create(nil);
  Code:=nil;
  try
    InitIDEFileDialog(OpenDialog);
    OpenDialog.Title:=crsCUSelectFileToInsertAtCursor;
    OpenDialog.Options:=OpenDialog.Options+[ofFileMustExist];
    Filter:=crsCUPascalPasPpPasPp;
    Filter:=Format(crsCUAllFiles, [Filter, FileMask, FileMask]);
    OpenDialog.Filter:=Filter;
    if not OpenDialog.Execute then exit;
    Filename:=OpenDialog.FileName;
    if not FileIsText(Filename) then begin
      if IDEMessageDialog(crsCUWarning, crsCUTheFileSeemsToBeABinaryProceed,
        mtConfirmation,[mbOk,mbCancel])<>mrOK then exit;
    end;
    Code:=TCodeBuffer.Create;
    Code.Filename:=Filename;
    Code.OnDecodeLoaded:=@Cody.DecodeLoaded;
    if not Code.LoadFromFile(Filename) then begin
      IDEMessageDialog(crsCWError, Format(crsCUUnableToLoadFile, [Filename, #13
        , Code.LastError]),
        mtError,[mbCancel]);
      exit;
    end;

    SrcEdit.Selection:=Code.Source;
  finally
    OpenDialog.Free;
    Code.Free;
  end;
end;

procedure AddCallInherited(Sender: TObject);

  procedure ErrorNotInMethod;
  begin
    IDEMessageDialog(crsCWError,
      crsCUPleasePlaceTheCursorOfTheSourceEditorInAnImplement,
      mtError,[mbCancel]);
  end;

var
  Handled: boolean;
  Tool: TEventsCodeTool;
  CleanPos: integer;
  CursorNode: TCodeTreeNode;
  ProcNode: TCodeTreeNode;
  DeclNode: TCodeTreeNode;
  NewCode: String;
  SrcEdit: TSourceEditorInterface;
  Indent: LongInt;
  IndentContextSensitive: Boolean;
  NewIndent: TFABIndentationPolicy;
  NewLine: Boolean;
  Gap: TGapTyp;
begin
  if (ParseTilCursor(Tool,CleanPos,CursorNode,Handled,true)<>cupeSuccess)
  and not Handled then begin
    ErrorNotInMethod;
    exit;
  end;
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  try
    try
      ProcNode:=CursorNode.GetNodeOfType(ctnProcedure);
      if not Tool.NodeIsMethodBody(ProcNode) then begin
        debugln(['AddCallInherited not in a method body']);
        exit;
      end;
      // search the declaration (the header of the body may be incomplete)
      DeclNode:=Tool.FindCorrespondingProcNode(ProcNode);
      if DeclNode=nil then
        DeclNode:=ProcNode;
      Handled:=true;
      NewCode:='inherited '+Tool.ExtractProcHead(DeclNode,
        [phpWithoutClassName,phpWithParameterNames,phpWithoutParamTypes]);
      //debugln(['AddCallInherited NewCode="',NewCode,'"']);
      NewLine:=true;
      Gap:=gtNone;
      if Tool.NodeIsFunction(DeclNode) then begin
        if FindFirstNonSpaceCharInLine(Tool.Src,CleanPos)<CleanPos then begin
          // insert function behind some code
          // e.g. InheritedValue:=|
          Indent:=0;
          NewLine:=false;
        end else begin
          // store the old result value
          NewCode:='Result:='+NewCode;
        end;
      end;

      if NewLine then begin
        // auto indent
        Gap:=gtNewLine;
        Indent:=SrcEdit.CursorScreenXY.X-1;
        IndentContextSensitive:=true;
        if CodeToolBoss.Indenter.GetIndent(Tool.Src,CleanPos,
          Tool.Scanner.NestedComments,
          true,NewIndent,IndentContextSensitive,NewCode)
        and NewIndent.IndentValid then begin
          Indent:=NewIndent.Indent;
        end;
        NewCode:=GetIndentStr(Indent)+NewCode;
        CleanPos:=GetLineStartPosition(Tool.Src,CleanPos);
        //debugln(['AddCallInherited Indent=',Indent]);
      end;

      NewCode:=CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
        NewCode,Indent,[bcfDoNotIndentFirstLine],GetPosInLine(Tool.Src,CleanPos));
      CodeToolBoss.SourceChangeCache.MainScanner:=Tool.Scanner;
      if not CodeToolBoss.SourceChangeCache.Replace(Gap,Gap,CleanPos,CleanPos,NewCode)
      then begin
        debugln(['AddCallInherited CodeToolBoss.SourceChangeCache.Replace failed']);
        exit;
      end;
      if not CodeToolBoss.SourceChangeCache.Apply then begin
        debugln(['AddCallInherited CodeToolBoss.SourceChangeCache.Apply failed']);
        exit;
      end;
    except
      on e: Exception do CodeToolBoss.HandleException(e);
    end;
  finally
    // syntax error or not in a method
    if not Handled then begin
      if CodeToolBoss.ErrorMessage<>'' then
        LazarusIDE.DoJumpToCodeToolBossError
      else
        ErrorNotInMethod;
    end;
  end;
end;

function ParseTilCursor(out Tool: TCodeTool; out CleanPos: integer;
  out Node: TCodeTreeNode; out ErrorHandled: boolean;
  JumpToError: boolean; CodePos: PCodeXYPosition): TCUParseError;
var
  SrcEdit: TSourceEditorInterface;
  CursorPos: TCodeXYPosition;
begin
  Tool:=nil;
  CleanPos:=0;
  Node:=nil;
  ErrorHandled:=false;
  if CodePos<>nil then CodePos^:=CleanCodeXYPosition;
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then begin
    debugln(['CodyUtils.ParseTilCursor: no source editor']);
    exit(cupeNoSrcEditor);
  end;
  if not LazarusIDE.BeginCodeTools then exit;

  CursorPos.Code:=SrcEdit.CodeToolsBuffer as TCodeBuffer;
  CursorPos.X:=SrcEdit.CursorTextXY.X;
  CursorPos.Y:=SrcEdit.CursorTextXY.Y;
  if CodePos<>nil then
    CodePos^:=CursorPos;
  try
    if not CodeToolBoss.InitCurCodeTool(CursorPos.Code) then
      exit(cupeMainCodeNotFound);
    try
      Tool:=CodeToolBoss.CurCodeTool;
      Result:=cupeParseError;
      Tool.BuildTreeAndGetCleanPos(trTillCursor,lsrEnd,CursorPos,CleanPos,
                                   [btSetIgnoreErrorPos]);
      Node:=Tool.FindDeepestNodeAtPos(CleanPos,false);
      if Node=nil then
        exit(cupeCursorNotInCode);
      Result:=cupeSuccess;
    except
      on e: Exception do CodeToolBoss.HandleException(e);
    end;
  finally
    if (CodeToolBoss.ErrorMessage<>'') and JumpToError then begin
      ErrorHandled:=true;
      LazarusIDE.DoJumpToCodeToolBossError;
    end;
  end;
end;

{ TCody }

procedure TCody.DecodeLoaded(Sender: TSourceLog; const Filename: string;
  var Source, DiskEncoding, MemEncoding: string);
begin
  //debugln(['TCody.DecodeLoaded ',Filename]);
  if (Sender is TCodeBuffer)
  and Assigned(CodeToolBoss.SourceCache.OnDecodeLoaded) then
    CodeToolBoss.SourceCache.OnDecodeLoaded(TCodeBuffer(Sender),Filename,
      Source,DiskEncoding,MemEncoding);
end;

initialization
  Cody:=TCody.Create;
finalization
  FreeAndNil(Cody);

end.

