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

  ToDo:
    - guess for in
    - guess parameter
    - guess <i>:=expression
    - Extend uses section when adding to a class
    - Fix bug: adding types with comments
    - Target implementation
    - Target interface
    - Target program
}
unit DeclareVarDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, contnrs, LResources, Forms, Controls, Graphics,
  Dialogs, ButtonPanel, StdCtrls, ExtCtrls,
  IDEDialogs, LazIDEIntf, SrcEditorIntf,
  FileProcs, CodeToolManager, FindDeclarationTool, CodeTree,
  KeywordFuncLists, BasicCodeTools, CodeAtom,
  CodyUtils, CodyStrConsts;

type

  { TCodyDeclareVarTarget }

  TCodyDeclareVarTarget = class
  public
    Tool: TFindDeclarationTool;
    NodeDesc: TCodeTreeNodeDesc;
    NodeStartPos: TCodeXYPosition;
    Visibility: TCodeTreeNodeDesc;
    Caption: string;
    constructor Create(const Context: TFindContext);
  end;

  { TCodyDeclareVarDialog }

  TCodyDeclareVarDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    Panel1: TPanel;
    TypeEdit: TEdit;
    TypeLabel: TLabel;
    WhereRadioGroup: TRadioGroup;
    RedefineLabel: TLabel;
    procedure HelpButtonClick(Sender: TObject);
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
    Targets: TObjectList; // list of TCodyDeclareVarTarget
    function Run: boolean;
  end;


procedure ShowDeclareVariableDialog(Sender: TObject);

function CheckCreateVarFromIdentifierInSrcEdit(out CodePos: TCodeXYPosition;
  out Tool: TCodeTool; out NewIdentifier, NewType, NewUnitName: string;
  out ExistingDefinition: TFindContext;
  out PossibleContexts: TFPList): boolean;

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
  out NewIdentifier, NewType, NewUnitName: string;
  out ExistingDefinition: TFindContext;
  out PossibleContexts: TFPList // list of PFindContext
  ): boolean;

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
  NewExprType: TExpressionType;
  IsSubIdentifier: boolean;
begin
  Result:=false;
  NewType:='';
  NewIdentifier:='';
  NewUnitName:='';
  CodePos:=CleanCodeXYPosition;
  Tool:=nil;
  ExistingDefinition:=CleanFindContext;
  PossibleContexts:=nil;

  if (ParseTilCursor(Tool,CleanPos,CursorNode,Handled,true,@CodePos)<>cupeSuccess)
  and not Handled then begin
    ErrorNotAtAnIdentifier;
    exit;
  end;

  Handled:=false;
  try
    try
      if not CodeToolBoss.GuessTypeOfIdentifier(CodePos.Code,CodePos.X,CodePos.Y,
        IsKeyword,IsSubIdentifier,ExistingDefinition,PossibleContexts,
        NewExprType,NewType)
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

{ TCodyDeclareVarTarget }

constructor TCodyDeclareVarTarget.Create(const Context: TFindContext);
begin
  Tool:=Context.Tool;
  NodeDesc:=Context.Node.Desc;
  Context.Tool.CleanPosToCaret(Context.Node.StartPos,NodeStartPos);
end;

{$R *.lfm}

{ TCodyDeclareVarDialog }

procedure TCodyDeclareVarDialog.FormCreate(Sender: TObject);
begin
  Targets:=TObjectList.create(true);
  Caption:='Declare a new variable';
  WhereRadioGroup.Caption:='Where';
  TypeEdit.Caption:='Type';
  ButtonPanel1.OKButton.OnClick:=@OKButtonClick;
  ButtonPanel1.HelpButton.OnClick:=@HelpButtonClick;
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
  if WhereRadioGroup.ItemIndex<0 then begin
    IDEMessageDialog('Error','Please specify a location',mtError,[mbCancel]);
    exit;
  end;
  ModalResult:=mrOk;
end;

procedure TCodyDeclareVarDialog.HelpButtonClick(Sender: TObject);
begin
  OpenCodyHelp('#Declare_Variable');
end;

procedure TCodyDeclareVarDialog.FormDestroy(Sender: TObject);
begin
  Targets.Free;
end;

function TCodyDeclareVarDialog.Run: boolean;

  procedure AddClassTarget(Context: TFindContext; Visibility: TCodeTreeNodeDesc);
  var
    Target: TCodyDeclareVarTarget;
    s: String;
  begin
    case Visibility of
    ctnClassPrivate: s:='Private';
    ctnClassProtected: s:='Protected';
    ctnClassPublic: s:='Public';
    ctnClassPublished: s:='Published';
    else exit;
    end;
    Target:=TCodyDeclareVarTarget.Create(Context);
    Target.Visibility:=Visibility;
    s:=s+' member of '+Context.Node.DescAsString+' '+
      Context.Tool.ExtractClassName(Context.Node,false,true);
    Target.Caption:=s;
    Targets.Add(Target);
  end;

var
  PossibleContexts: TFPList;
  Target: TCodyDeclareVarTarget;
  Context: TFindContext;
  i: Integer;
  OldChange: boolean;
  OldSrcEdit: TSourceEditorInterface;
  NewType: String;
  ExistingDefinition: TFindContext;
begin
  Result:=false;
  PossibleContexts:=nil;
  Targets.Clear;
  try
    if not CheckCreateVarFromIdentifierInSrcEdit(CodePos,Tool,Identifier,
      RecommendedType,UnitOfType,ExistingDefinition,PossibleContexts)
    then exit;

    if ExistingDefinition.Node<>nil then begin
      RedefineLabel.Visible:=true;
      RedefineLabel.Caption:='Already defined at '+ExistingDefinition.Tool.CleanPosToRelativeStr(
        ExistingDefinition.Node.StartPos,CodePos.Code.Filename);
    end else begin
      RedefineLabel.Visible:=false;
    end;

    if PossibleContexts<>nil then begin
      for i:=0 to PossibleContexts.Count-1 do begin
        Context:=PFindContext(PossibleContexts[i])^;
        if Context.Node.Desc=ctnProcedure then begin
          Target:=TCodyDeclareVarTarget.Create(Context);
          Target.Caption:='Local variable of '+Context.Tool.ExtractProcName(Context.Node,[]);
          Targets.Add(Target);
        end else if Context.Node.Desc in AllClassObjects then begin
          AddClassTarget(Context,ctnClassPrivate);
          AddClassTarget(Context,ctnClassProtected);
          AddClassTarget(Context,ctnClassPublic);
          AddClassTarget(Context,ctnClassPublished);
        end;
      end;
    end;
  finally
    FreeListOfPFindContext(PossibleContexts);
  end;

  if Targets.Count=0 then begin
    IDEMessageDialog('Already defined',
      'The identifier "'+Identifier+'" is already defined.',mtError,[mbCancel]);
    exit;
  end;

  Caption:='Declare variable "'+Identifier+'"';
  TypeLabel.Caption:='Type';
  TypeEdit.Text:=RecommendedType;

  for i:=0 to Targets.Count-1 do begin
    Target:=TCodyDeclareVarTarget(Targets[i]);
    WhereRadioGroup.Items.Add(Target.Caption);
  end;
  WhereRadioGroup.ItemIndex:=0;

  // show dialog as modal form
  Result:=ShowModal=mrOk;

  if Result then begin
    NewType:=Trim(TypeEdit.Text);
    Target:=TCodyDeclareVarTarget(Targets[WhereRadioGroup.ItemIndex]);

    OldChange:=LazarusIDE.OpenEditorsOnCodeToolChange;
    try
      OldSrcEdit:=SourceEditorManagerIntf.ActiveEditor;
      LazarusIDE.OpenEditorsOnCodeToolChange:=true;
      if not CodeToolBoss.DeclareVariable(Target.NodeStartPos.Code,
        Target.NodeStartPos.X,Target.NodeStartPos.Y,
        Identifier,NewType,UnitOfType,Target.Visibility)
      then begin
        debugln(['TCodyDeclareVarDialog.Run Error']);
        LazarusIDE.DoJumpToCodeToolBossError;
        ModalResult:=mrCancel;
        exit;
      end;
      if Target.NodeStartPos.Code<>CodePos.Code then begin
        // declaration was in another unit => switch back to cursor
        //debugln(['TCodyDeclareVarDialog.OKButtonClick switching back to ',OldSrcEdit.FileName]);
        SourceEditorManagerIntf.ActiveEditor:=OldSrcEdit;
      end;
    finally
      LazarusIDE.OpenEditorsOnCodeToolChange:=OldChange;
    end;
  end;
end;

end.

