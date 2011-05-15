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
    A dialog to create a variable declaration.
    It uses the identifier in the source editor to guess the name and type, so
    that the user needs only to choose where to create the var.
}
unit DeclareVarDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, ExtCtrls,
  IDEDialogs, LazIDEIntf,
  FileProcs, CodeToolManager, FindDeclarationTool, CodeTree,
  KeywordFuncLists, BasicCodeTools, CodeCompletionTool, CodeAtom,
  CodyUtils, CodyStrConsts;

type

  { TCodyDeclareVarDialog }

  TCodyDeclareVarDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    Panel1: TPanel;
    TypeEdit: TEdit;
    TypeLabel: TLabel;
    WhereRadioGroup: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
  end;


procedure ShowDeclareVariableDialog(Sender: TObject);

function CheckCreateVarFromIdentifierInSrcEdit: boolean;

implementation

procedure ShowDeclareVariableDialog(Sender: TObject);
var
  CodyDeclareVarDialog: TCodyDeclareVarDialog;
begin
  if not CheckCreateVarFromIdentifierInSrcEdit then exit;
  ShowMessage('Declare Variable dialog is not implemented yet');

  CodyDeclareVarDialog:=TCodyDeclareVarDialog.Create(nil);
  try
    CodyDeclareVarDialog.ShowModal;
  finally
    CodyDeclareVarDialog.Free;
  end;
end;

function CheckCreateVarFromIdentifierInSrcEdit: boolean;

  procedure ErrorNotAtAnIdentifier;
  begin
    IDEMessageDialog(crsCWError,
      Format(crsPleasePlaceTheCursorOfTheSourceEditorAtAnIdentifie, [#13, #13]),
      mtError,[mbCancel]);
  end;

var
  Tool: TCodeTool;
  CleanPos: integer;
  CursorNode: TCodeTreeNode;
  Handled: boolean;
  BlockNode: TCodeTreeNode;
  Node: TCodeTreeNode;
  IdentStart: integer;
  IdentEnd: integer;
  Identifier: String;
begin
  Result:=false;
  if (ParseTilCursor(Tool,CleanPos,CursorNode,Handled,true)<>cupeSuccess)
  and not Handled then begin
    ErrorNotAtAnIdentifier;
    exit;
  end;

  Handled:=false;
  try
    try
      // check if in a statement
      BlockNode:=nil;
      Node:=CursorNode;
      while (Node<>nil) do begin
        if Node.Desc=ctnBeginBlock then
          BlockNode:=Node;
        Node:=Node.Parent;
      end;
      if BlockNode=nil then begin
        // not in a statement
        debugln(['CheckCreateVarFromIdentifierInSrcEdit not on a statement']);
        ErrorNotAtAnIdentifier;
        exit;
      end;

      // check if a keyword
      GetIdentStartEndAtPosition(Tool.Src,CleanPos, IdentStart, IdentEnd);
      if IdentStart>=IdentEnd then begin
        // not on a word
        debugln(['CheckCreateVarFromIdentifierInSrcEdit not on a word']);
        ErrorNotAtAnIdentifier;
        exit;
      end;
      Identifier:=GetIdentifier(@Tool.Src[IdentStart]);
      if WordIsKeyWord.DoItCaseInsensitive(Identifier) then
      begin
        // a keyword
        debugln(['CheckCreateVarFromIdentifierInSrcEdit "',Identifier,'" is a keyword']);
        IDEMessageDialog(crsCWError,'The "'+Identifier+'" is a keyword.',mtError,[mbCancel]);
        exit;
      end;

      // ToDo: check context
      // examples:
      //   identifier:=<something>
      //   aclass.identifier:=<something>
      //   <something>:=aclass.identifier
      //   <something>:=<something>+aclass.identifier
      //   <proc>(,,aclass.identifier)
      //   for identifier in <something>

      // ToDo: check where the identifier is already defined
      // ToDo: check if the identifier is a sub identifier (e.g. A.identifier)
      // ToDo: check if it is the target of an assignment and guess the type
      // ToDo: check if it is the source of an assignment and guess the type
      // ToDo: check if it is a parameter and guess the type
      // ToDo: create the list of possible locations and notes

    except
      on e: Exception do CodeToolBoss.HandleException(e);
    end;
  finally
    // syntax error or not in a method
    if not Handled then begin
      if CodeToolBoss.ErrorMessage<>'' then
        LazarusIDE.DoJumpToCodeToolBossError
      else
        ErrorNotAtAnIdentifier;
    end;
  end;
end;

{$R *.lfm}

{ TCodyDeclareVarDialog }

procedure TCodyDeclareVarDialog.FormCreate(Sender: TObject);
begin
  Caption:='Declare a new variable';
  WhereRadioGroup.Caption:='Where';
  TypeEdit.Caption:='Type';
end;

procedure TCodyDeclareVarDialog.FormDestroy(Sender: TObject);
begin

end;

end.

