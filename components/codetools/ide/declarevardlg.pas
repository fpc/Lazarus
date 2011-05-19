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
  KeywordFuncLists, BasicCodeTools, CodeAtom,
  CodyUtils, CodyStrConsts;

type

  { TCodyDeclareVarDialog }

  TCodyDeclareVarDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    Panel1: TPanel;
    TypeEdit: TEdit;
    TypeLabel: TLabel;
    WhereRadioGroup: TRadioGroup;
    procedure OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
    CodePos: TCodeXYPosition;
    Tool: TCodeTool;
    Identifier: string;
    RecommendedType: string;
    UnitOfType: string;
    function Run: boolean;
  end;


procedure ShowDeclareVariableDialog(Sender: TObject);

function CheckCreateVarFromIdentifierInSrcEdit(out CodePos: TCodeXYPosition;
  out Tool: TCodeTool; out NewIdentifier, NewType, NewUnitName: string): boolean;

implementation

procedure ShowDeclareVariableDialog(Sender: TObject);
var
  CodyDeclareVarDialog: TCodyDeclareVarDialog;
begin
  CodyDeclareVarDialog:=TCodyDeclareVarDialog.Create(nil);
  try
    CodyDeclareVarDialog.Run;
  finally
    CodyDeclareVarDialog.Free;
  end;
end;

function CheckCreateVarFromIdentifierInSrcEdit(
  out CodePos: TCodeXYPosition; out Tool: TCodeTool;
  out NewIdentifier, NewType, NewUnitName: string): boolean;

  procedure ErrorNotAtAnIdentifier;
  begin
    IDEMessageDialog(crsCWError,
      Format(crsPleasePlaceTheCursorOfTheSourceEditorAtAnIdentifie, [#13, #13]),
      mtError,[mbCancel]);
  end;

var
  CleanPos: integer;
  CursorNode: TCodeTreeNode;
  Handled: boolean;
  IsKeyword: boolean;
  ExistingDefinition: TFindContext;
  ListOfPFindContext: TFPList;
  NewExprType: TExpressionType;
begin
  Result:=false;
  NewType:='';
  NewIdentifier:='';
  NewUnitName:='';
  CodePos:=CleanCodeXYPosition;
  Tool:=nil;
  if (ParseTilCursor(Tool,CleanPos,CursorNode,Handled,true,@CodePos)<>cupeSuccess)
  and not Handled then begin
    ErrorNotAtAnIdentifier;
    exit;
  end;

  Handled:=false;
  ListOfPFindContext:=nil;
  try
    try
      if not CodeToolBoss.GuessTypeOfIdentifier(CodePos.Code,CodePos.X,CodePos.Y,
        IsKeyword,ExistingDefinition,ListOfPFindContext,NewExprType,NewType)
      then begin
        debugln(['CheckCreateVarFromIdentifierInSrcEdit GuessTypeOfIdentifier failed']);
        exit;
      end;

      NewIdentifier:=GetIdentifier(@Tool.Src[GetIdentStartPosition(Tool.Src,CleanPos)]);
      if IsKeyword then begin
        Handled:=true;
        IDEMessageDialog('Error','"'+NewIdentifier+'" is a keyword.',mtError,[mbCancel]);
        exit;
      end;

      // check if newtype is a variable and if it is in another unit
      if (NewExprType.Desc=xtContext) then begin
        if NewExprType.Context.Tool<>Tool then
          NewUnitName:=NewExprType.Context.Tool.GetSourceName;
      end;

      Handled:=true;
      Result:=true;
    except
      on e: Exception do CodeToolBoss.HandleException(e);
    end;
  finally
    FreeListOfPFindContext(ListOfPFindContext);
    //debugln(['CheckCreateVarFromIdentifierInSrcEdit Handled=',Handled,' CTError=',CodeToolBoss.ErrorMessage]);
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
  ButtonPanel1.OKButton.OnClick:=@OKButtonClick;
end;

procedure TCodyDeclareVarDialog.OKButtonClick(Sender: TObject);
var
  NewType: TCaption;
begin
  NewType:=Trim(TypeEdit.Text);
  if NewType='' then begin
    IDEMessageDialog('Error','Please specify a type',mtError,[mbCancel]);
    exit;
  end;
  if CompareTextIgnoringSpace(NewType,RecommendedType,false)<>0 then begin
    debugln(['TCodyDeclareVarDialog.OKButtonClick using custom type "',NewType,'"']);
    UnitOfType:='';
  end;
  if not CodeToolBoss.DeclareVariable(CodePos.Code,CodePos.X,CodePos.Y,
    Identifier,RecommendedType,UnitOfType)
  then begin
    LazarusIDE.DoJumpToCodeToolBossError;
    ModalResult:=mrCancel;
    exit;
  end;
  ModalResult:=mrOk;
end;

procedure TCodyDeclareVarDialog.FormDestroy(Sender: TObject);
begin

end;

function TCodyDeclareVarDialog.Run: boolean;
begin
  Result:=false;
  if not CheckCreateVarFromIdentifierInSrcEdit(CodePos,Tool,Identifier,
    RecommendedType,UnitOfType)
  then exit;
  Caption:='Declare variable "'+Identifier+'"';
  TypeEdit.Text:=RecommendedType;
  WhereRadioGroup.Items.Add('Local variable');
  WhereRadioGroup.ItemIndex:=0;
  Result:=ShowModal=mrOk;
end;

end.

