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
    Function to copy, cut declarations to clipboard and paste them.
}
unit CodyCopyDeclaration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  IDEDialogs, LazIDEIntf, SrcEditorIntf,
  CodeToolManager, CodeCache, CodeTree, BasicCodeTools,
  CodyUtils, CodyStrConsts;

type

  { TCodyClipboardCopyDeclaration }

  TCodyClipboardCopyDeclaration = class(TCodyClipboardSrcData)
  public
    Desc: TCodeTreeNodeDesc;
    TypeDesc: TCodeTreeNodeDesc;
    Header: string;
    Declaration: string; // declaration with comments
    BodyHeader: string; // comment above bodies
    Bodies: TStrings; // bodies with comments
    Units: TStrings;
    procedure WriteToStream(MemStream: TMemoryStream); override;
    procedure ReadFromStream(MemStream: TMemoryStream); override;
    procedure Execute(SrcEdit: TSourceEditorInterface; LogXY: TPoint); override;
  end;

procedure CopyDeclarationToClipboard(Sender: TObject);
procedure CutDeclarationToClipboard(Sender: TObject);
procedure CutCopyDeclarationToClipboard(Delete: boolean);

implementation

procedure CopyDeclarationToClipboard(Sender: TObject);
begin
  CutCopyDeclarationToClipboard(false);
end;

procedure CutDeclarationToClipboard(Sender: TObject);
begin
  CutCopyDeclarationToClipboard(true);
end;

procedure FindCommentsInFront(Tool: TCodeTool;
  CleanPos: integer; out CommentStartPos: integer);
var
  p: Integer;
begin
  CommentStartPos:=0;
  // jump to atom in front
  Tool.ReadPriorAtomSafe(CleanPos);
  if Tool.CurPos.StartPos<1 then exit;
  // skip comments belonging to prior atom
  p:=Tool.FindLineEndOrCodeAfterPosition(Tool.CurPos.EndPos);
  while (p<=Tool.SrcLen) and (Tool.Src[p] in [' ',#9,#10,#13]) do inc(p);
  if p>CleanPos then exit;
  // skip space
  //if (p^='{') or ((p^='/') and (p[1]='/'))
  //or ((p^='(') and (p[1]='*')) then begin
  //  CommentStartPos:=p-PChar(Tool.Src)+1;
  //end;
end;

procedure CutCopyDeclarationToClipboard(Delete: boolean);

  procedure ErrorNotInADeclaration;
  begin
    IDEMessageDialog(crsCWError,
      crsPleasePlaceTheCursorOfTheSourceEditorOnAnIdentifie,
      mtError,[mbCancel]);
  end;

var
  Tool: TCodeTool;
  CleanPos: integer;
  CursorNode: TCodeTreeNode;
  Handled: boolean;
  Data: TCodyClipboardCopyDeclaration;
begin
  if (ParseUnit(Tool,CleanPos,CursorNode,Handled,true)<>cupeSuccess)
  and not Handled then begin
    ErrorNotInADeclaration;
    exit;
  end;
  Data:=nil;
  try
    try
      Data:=TCodyClipboardCopyDeclaration.Create;
      if CursorNode.Parent=nil then exit;
      if CursorNode.Parent.Desc in AllDefinitionSections then begin
        if CursorNode.Desc in AllSimpleIdentifierDefinitions then begin
          // var, const, type
          if CleanPos>CursorNode.StartPos+GetIdentLen(@Tool.Src[CursorNode.StartPos])
          then
            ErrorNotInADeclaration;
          // comment in front

        end;
      end;
      if Data.Desc=ctnNone then
        ErrorNotInADeclaration;
      // todo: procedure, method, member variable, property, global property, parameter, generic
      // todo: var nodes can be grouped: var a,b: integer;
      // todo: copy comments in front, in between and behind
      // todo: copy header
      // todo: proc: copy proc body
      // todo: proc: copy body comments too
      // todo: class: copy all bodies and comments
      // todo: class: delete declaration header
      // todo: delete: when last child then delete whole section
      if CursorNode.Desc in AllSimpleIdentifierDefinitions then begin

      end;
      //Range:=trTillCursor;
      if Delete then begin

      end;
    except
      on e: Exception do CodeToolBoss.HandleException(e);
    end;
  finally
    Data.Free;
    // syntax error or not in a method
    if not Handled then begin
      if CodeToolBoss.ErrorMessage<>'' then
        LazarusIDE.DoJumpToCodeToolBossError
      else
        ErrorNotInADeclaration;
    end;
  end;
end;

{ TCodyClipboardCopyDeclaration }

procedure TCodyClipboardCopyDeclaration.WriteToStream(MemStream: TMemoryStream
  );
begin
  inherited WriteToStream(MemStream);

end;

procedure TCodyClipboardCopyDeclaration.ReadFromStream(MemStream: TMemoryStream
  );
begin
  inherited ReadFromStream(MemStream);

end;

procedure TCodyClipboardCopyDeclaration.Execute(
  SrcEdit: TSourceEditorInterface; LogXY: TPoint);
begin
  inherited Execute(SrcEdit, LogXY);
end;

end.

