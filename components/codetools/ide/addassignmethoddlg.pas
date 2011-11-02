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
    An IDE dialog to add an Assign method to a pascal class.
}
unit AddAssignMethodDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, ButtonPanel, StdCtrls, ComCtrls, Menus, AVL_Tree,
  // Codetools
  CodeCache, CodeToolManager, FileProcs, PascalParserTool,
  BasicCodeTools, CodeTree, FindDeclarationTool,
  // IDEIntf
  IDEDialogs, LazIDEIntf, SrcEditorIntf,
  // Cody
  CodyCtrls, CodyStrConsts, CodyUtils;

type

  { TAAMDItem }

  TAAMDItem = class
  public
    CodePos: TCodeXYPosition;
    Name: string;
    Desc: TCodeTreeNodeDesc;
    Visibility: TCodeTreeNodeDesc;
    WrittenByProperty: string;
    Selected: boolean;
  end;

  { TAddAssignMethodDialog }

  TAddAssignMethodDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    DeclGroupBox: TGroupBox;
    ImageList1: TImageList;
    InhCallCheckBox: TCheckBox;
    InhCallOnlyInElseCheckBox: TCheckBox;
    InheritedEdit: TEdit;
    InheritedGroupBox: TGroupBox;
    InheritedLabel: TLabel;
    InhOverrideCheckBox: TCheckBox;
    MembersGroupBox: TGroupBox;
    MembersTreeView: TCodyTreeView;
    ParamNameEdit: TEdit;
    ParamNameErrorLabel: TLabel;
    ParamNameLabel: TLabel;
    ParamTypeEdit: TEdit;
    ParamTypeErrorLabel: TLabel;
    ParamTypeLabel: TLabel;
    ProcNameEdit: TEdit;
    ProcNameErrorLabel: TLabel;
    ProcNameLabel: TLabel;
    procedure MembersTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure OkButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ParamNameEditChange(Sender: TObject);
    procedure ParamTypeEditChange(Sender: TObject);
    procedure ProcNameEditChange(Sender: TObject);
  private
    FParamChanged: boolean;
    FProcNameEditChanged: boolean;
    FChecking: Boolean;
    FIdleConnected: boolean;
    procedure SetIdleConnected(const AValue: boolean);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
  private
    ImgIDImplementation: LongInt;
    ImgIDVariable: LongInt;
    ImgIDProperty: LongInt;
    // cursor position
    FCode: TCodeBuffer;
    FX, FY: integer;
    FProcName: string;
    // items
    FItems: TAVLTree; // tree of TAAMDItem sorted for Name
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  public
    NewPos: TCodeXYPosition;
    NewTopLine: integer;
    function Init(NewCode: TCodeBuffer; NewX, NewY: integer;
                  UseInheritedParam: boolean): boolean;
  end;


procedure ShowAddAssignMethodDialog(Sender: TObject);

function CompareAAMDItemsByName(Item1, Item2: Pointer): integer;
function CompareAnsistringWithAAMDItemName(AnAnsistring, Item: Pointer): integer;

implementation

procedure ShowAddAssignMethodDialog(Sender: TObject);

  procedure ErrorNotInClass;
  begin
    IDEMessageDialog(crsCWError,
      crsCAMPleasePositionTheCursorOfTheSourceEditorInAPascalC,
      mtError,[mbCancel]);
  end;

var
  AddAssignMethodDialog: TAddAssignMethodDialog;
  SrcEdit: TSourceEditorInterface;
begin
  // commit changes form source editor to codetools
  if not LazarusIDE.BeginCodeTools then exit;
  // check context at cursor
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then begin
    ErrorNotInClass;
    exit;
  end;
  AddAssignMethodDialog:=nil;
  try
    AddAssignMethodDialog:=TAddAssignMethodDialog.Create(nil);
    with AddAssignMethodDialog do begin
      if not Init(SrcEdit.CodeToolsBuffer as TCodeBuffer,
        SrcEdit.CursorTextXY.X,SrcEdit.CursorTextXY.Y,true)
      then begin
        // syntax error or not in a class
        if CodeToolBoss.ErrorMessage<>'' then
          LazarusIDE.DoJumpToCodeToolBossError
        else
          ErrorNotInClass;
        exit;
      end;
      if ShowModal=mrOk then begin
        if NewPos.Code<>nil then
          LazarusIDE.DoOpenFileAndJumpToPos(NewPos.Code.Filename,
             Point(NewPos.X,NewPos.Y),NewTopLine,-1,-1,[]);
      end;
    end;
  finally
    AddAssignMethodDialog.Free;
  end;
end;

function CompareAAMDItemsByName(Item1, Item2: Pointer): integer;
var
  it1: TAAMDItem absolute Item1;
  it2: TAAMDItem absolute Item2;
begin
  Result:=SysUtils.CompareText(it1.Name,it2.Name);
end;

function CompareAnsistringWithAAMDItemName(AnAnsistring, Item: Pointer
  ): integer;
var
  it: TAAMDItem absolute Item;
  s: String;
begin
  s:=AnsiString(AnAnsistring);
  Result:=SysUtils.CompareText(s,it.Name);
end;

{ TAddAssignMethodDialog }

procedure TAddAssignMethodDialog.FormCreate(Sender: TObject);
begin
  FProcName:='Assign';
  FItems:=TAVLTree.Create(@CompareAAMDItemsByName);
  ImgIDImplementation := Imagelist1.AddLazarusResource('ce_implementation');
  ImgIDVariable := Imagelist1.AddLazarusResource('ce_variable');
  ImgIDProperty := Imagelist1.AddLazarusResource('ce_property');

  ProcNameEdit.Text:=FProcName;
end;

procedure TAddAssignMethodDialog.OkButtonClick(Sender: TObject);
var
  FTool: TCodeTool;
  FClassNode: TCodeTreeNode;
  FAssignDeclNode: TCodeTreeNode;
  FMemberNodeExts: TAVLTree;
  FAssignBodyNode: TCodeTreeNode;
  FInheritedDeclContext: TFindContext;
  AVLNode: TAVLTreeNode;
  NextAVLNode: TAVLTreeNode;
  NodeExt: TCodeTreeNodeExtension;
  AssignIt: Boolean;
  FoundAVLNode: TAVLTreeNode;
  Item: TAAMDItem;
  FAssignMembers: TFPList;
  i: Integer;
begin
  if (not IsValidIdent(FProcName))
  or (not IsValidIdent(ParamNameEdit.Text))
  or (not IsValidIdent(ParamTypeEdit.Text)) then exit;

  FMemberNodeExts:=nil;
  FAssignMembers:=TFPList.Create;
  try
    if not CodeToolBoss.FindAssignMethod(FCode,FX,FY,
      FTool,FClassNode,FAssignDeclNode,FMemberNodeExts,FAssignBodyNode,
      FInheritedDeclContext,FProcName)
    then begin
      // syntax error or not in a class
      if CodeToolBoss.ErrorMessage<>'' then begin
        LazarusIDE.DoJumpToCodeToolBossError;
        ModalResult:=mrCancel;
      end;
      exit;
    end;
    if FAssignDeclNode<>nil then
      exit;

    // use selected members
    if FMemberNodeExts<>nil then begin
      AVLNode:=FMemberNodeExts.FindLowest;
      while AVLNode<>nil do begin
        NextAVLNode:=FMemberNodeExts.FindSuccessor(AVLNode);
        NodeExt:=TCodeTreeNodeExtension(AVLNode.Data);
        AssignIt:=true;
        if (NodeExt.Txt='') then AssignIt:=false;
        if AssignIt then begin
          FoundAVLNode:=FItems.FindKey(PChar(NodeExt.Txt),@CompareAnsistringWithAAMDItemName);
          if FoundAVLNode<>nil then begin
            Item:=TAAMDItem(FoundAVLNode.Data);
            AssignIt:=Item.Selected;
          end;
        end;
        if AssignIt then begin
          FMemberNodeExts.Delete(AVLNode);
          FAssignMembers.Add(NodeExt);
        end;
        AVLNode:=NextAVLNode;
      end;
    end;
    //debugln(['TAddAssignMethodDialog.OkButtonClick Count=',FAssignMembers.Count]);

    if not FTool.AddAssignMethod(FClassNode,FAssignMembers,FProcName,
        ParamNameEdit.Text,ParamTypeEdit.Text,
        InhOverrideCheckBox.Checked,InhCallCheckBox.Checked,
        InhCallOnlyInElseCheckBox.Checked,
        CodeToolBoss.SourceChangeCache,
        NewPos,NewTopLine)
    then begin
      debugln(['TAddAssignMethodDialog.ButtonPanel1Click failed']);
      ModalResult:=mrCancel;
      exit;
    end;
  finally
    DisposeAVLTree(FMemberNodeExts);
    for i:=0 to FAssignMembers.Count-1 do
      TObject(FAssignMembers[i]).Free;
    FreeAndNil(FAssignMembers);
  end;
  ModalResult:=mrOk;
end;

procedure TAddAssignMethodDialog.HelpButtonClick(Sender: TObject);
begin
  OpenCodyHelp('#Add_Assign_method');
end;

procedure TAddAssignMethodDialog.MembersTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  Item: TAAMDItem;
begin
  Node:=MembersTreeView.GetNodeAt(X,Y);
  if Node=nil then exit;
  if TObject(Node.Data) is TAAMDItem then
    Item:=TAAMDItem(Node.Data)
  else
    Item:=nil;
  if Item<>nil then begin
    if Button=mbLeft then begin
      if X>Node.DisplayExpandSignRight then begin
        Item.Selected:=not Item.Selected;
        if Item.Selected then
          Node.ImageIndex:=ImgIDImplementation
        else
          Node.ImageIndex:=-1;
        Node.SelectedIndex:=Node.ImageIndex;
      end;
    end;
  end;
end;

procedure TAddAssignMethodDialog.FormDestroy(Sender: TObject);
begin
  FItems.FreeAndClear;
  FreeAndNil(FItems);
end;

procedure TAddAssignMethodDialog.ParamNameEditChange(Sender: TObject);
begin
  FParamChanged:=true;
  IdleConnected:=true;
end;

procedure TAddAssignMethodDialog.ParamTypeEditChange(Sender: TObject);
begin
  FParamChanged:=true;
  IdleConnected:=true;
end;

procedure TAddAssignMethodDialog.ProcNameEditChange(Sender: TObject);
begin
  FProcNameEditChanged:=true;
  IdleConnected:=true;
end;

procedure TAddAssignMethodDialog.SetIdleConnected(const AValue: boolean);
begin
  if FIdleConnected=AValue then exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TAddAssignMethodDialog.OnIdle(Sender: TObject; var Done: Boolean);
begin
  IdleConnected:=false;
  Init(FCode,FX,FY,FProcNameEditChanged);
  FProcNameEditChanged:=false;
end;

function TAddAssignMethodDialog.Init(NewCode: TCodeBuffer; NewX, NewY: integer;
  UseInheritedParam: boolean): boolean;
var
  FTool: TCodeTool;
  FClassNode: TCodeTreeNode;
  FAssignDeclNode: TCodeTreeNode;
  FMemberNodeExts: TAVLTree;
  FAssignBodyNode: TCodeTreeNode;
  FInheritedDeclContext: TFindContext;
  FClassName: String;
  FInheritedParamName: String;
  FInheritedParamType: String;
  FInheritedIsTPersistent: Boolean;
  InheritedClassNode: TCodeTreeNode;
  ParamNode: TCodeTreeNode;
  AVLNode: TAVLTreeNode;
  NodeExt: TCodeTreeNodeExtension;
  s: String;
  VisibilityNode: TCodeTreeNode;
  TVNode: TTreeNode;
  Item: TAAMDItem;
  NewProcName: String;
begin
  if FChecking then exit;
  FCode:=NewCode;
  FX:=NewX;
  FY:=NewY;

  FItems.FreeAndClear;

  FMemberNodeExts:=nil;
  try
    FChecking:=true;
    FTool:=nil;
    FClassNode:=nil;
    FAssignDeclNode:=nil;
    FMemberNodeExts:=nil;
    FAssignBodyNode:=nil;
    FInheritedDeclContext:=CleanFindContext;
    NewProcName:=ProcNameEdit.Text;
    if (NewProcName<>'') and IsValidIdent(NewProcName) then
      FProcName:=NewProcName;

    Result:=(FCode<>nil) and CodeToolBoss.FindAssignMethod(FCode,FX,FY,
      FTool,FClassNode,FAssignDeclNode,FMemberNodeExts,FAssignBodyNode,
      FInheritedDeclContext,FProcName);

    if FTool<>nil then
      FClassName:=FTool.ExtractClassName(FClassNode,false)
    else
      FClassName:='';

    FInheritedParamName:='Source';
    FInheritedParamType:='TObject';
    FInheritedIsTPersistent:=false;

    // check if inherited exists, if it is TPersistent.Assign and use the
    // inherited parameter name and type
    if FInheritedDeclContext.Node<>nil then begin
      InheritedClassNode:=fInheritedDeclContext.Tool.FindClassOrInterfaceNode(fInheritedDeclContext.Node);
      FInheritedIsTPersistent:=(InheritedClassNode<>nil)
        and (InheritedClassNode.Parent.Desc=ctnTypeDefinition)
        and (CompareIdentifiers('TPersistent',@fInheritedDeclContext.Tool.Src[InheritedClassNode.Parent.StartPos])=0);
      ParamNode:=fInheritedDeclContext.Tool.GetProcParamList(fInheritedDeclContext.Node);
      if ParamNode<>nil then begin
        ParamNode:=ParamNode.FirstChild;
        if ParamNode<>nil then begin
          FInheritedParamName:=fInheritedDeclContext.Tool.ExtractDefinitionName(ParamNode);
          if (ParamNode.FirstChild<>nil) and (ParamNode.FirstChild.Desc=ctnIdentifier) then
            FInheritedParamType:=GetIdentifier(@fInheritedDeclContext.Tool.Src[ParamNode.FirstChild.StartPos]);
        end;
      end;
    end;

    ButtonPanel1.OKButton.Enabled:=Result and (FMemberNodeExts<>nil)
      and (FMemberNodeExts.Count>0);
    ButtonPanel1.HelpButton.Caption:=crsHelp;
    ButtonPanel1.OKButton.Caption:=crsBTNOK;
    ButtonPanel1.CancelButton.Caption:=crsBTNCancel;
    Caption:=Format(crsCAMAddAssignMethodToClass, [FClassName]);

    DeclGroupBox.Caption:=crsCAMNewMethod;
    ProcNameLabel.Caption:=crsCAMMethodName;
    if (NewProcName='') or (not IsValidIdent(NewProcName)) then
      ProcNameErrorLabel.Caption:=crsCAMInvalidIdentifier
    else if not Result then
      ProcNameErrorLabel.Caption:=crsCAMCursorIsNotInAPascalClassDeclaration
    else if FAssignDeclNode<>nil then
      ProcNameErrorLabel.Caption:=crsCAMExistsAlready
    else
      ProcNameErrorLabel.Caption:='';

    ParamNameLabel.Caption:=crsCAMParameterName;
    if UseInheritedParam then
      ParamNameEdit.Text:=FInheritedParamName;
    if (ParamNameEdit.Text='') or not IsValidIdent(ParamNameEdit.Text) then
      ParamNameErrorLabel.Caption:=crsCAMInvalidIdentifier
    else
      ParamNameErrorLabel.Caption:='';

    ParamTypeLabel.Caption:=crsCAMParameterType;
    if UseInheritedParam then
      ParamTypeEdit.Text:=FInheritedParamType;
    if (ParamTypeEdit.Text='') or not IsValidIdent(ParamTypeEdit.Text) then
      ParamTypeErrorLabel.Caption:=crsCAMInvalidIdentifier
    else
      ParamTypeErrorLabel.Caption:='';

    // show some context information about the inherited method
    InheritedGroupBox.Caption:=crsCAMInherited;
    InhOverrideCheckBox.Caption:=crsCAMOverride;
    InhCallCheckBox.Caption:=crsCAMCallInherited;
    InhCallOnlyInElseCheckBox.Caption:=crsCAMCallInheritedOnlyIfWrongClass;
    InheritedEdit.ReadOnly:=true;
    if FInheritedDeclContext.Node<>nil then begin
      InheritedGroupBox.Enabled:=true;
      InheritedLabel.Caption:=crsCAMMethod;
      InheritedEdit.Text:=FInheritedDeclContext.Tool.ExtractProcHead(
        FInheritedDeclContext.Node,
        [phpAddClassName,phpWithDefaultValues,phpWithParameterNames]);
      InheritedEdit.Enabled:=true;
      InhOverrideCheckBox.Checked:=true;
      InhCallCheckBox.Checked:=true;
      InhCallOnlyInElseCheckBox.Enabled:=SysUtils.CompareText(FClassName,ParamTypeEdit.Text)<>0;
      InhCallOnlyInElseCheckBox.Checked:=FInheritedIsTPersistent;
    end else begin
      InheritedGroupBox.Enabled:=false;
      InheritedLabel.Caption:=crsCAMThereIsNoInheritedMethod;
      InheritedEdit.Text:='';
      InheritedEdit.Enabled:=false;
    end;

    MembersGroupBox.Caption:=crsCAMSelectMembersToAssign;
    MembersTreeView.BeginUpdate;
    MembersTreeView.Items.Clear;
    if FMemberNodeExts<>nil then begin
      AVLNode:=FMemberNodeExts.FindLowest;
      while AVLNode<>nil do begin
        NodeExt:=TCodeTreeNodeExtension(AVLNode.Data);
        Item:=TAAMDItem.Create;

        // visibility
        VisibilityNode:=NodeExt.Node.Parent;
        if VisibilityNode.Desc in AllClassSubSections then
          VisibilityNode:=VisibilityNode.Parent;
        Item.Visibility:=VisibilityNode.Desc;
        // kind
        Item.Desc:=NodeExt.Node.Desc;
        // name
        Item.Name:=NodeExt.Txt;
        // written by property
        if NodeExt.Data<>nil then
          Item.WrittenByProperty:=FTool.ExtractPropName(TCodeTreeNode(NodeExt.Data),false)
        else
          Item.Selected:=true;
        case Item.Visibility of
        ctnClassPrivate: s:=crsCAMPrivate;
        ctnClassProtected: s:=crsCAMProtected;
        ctnClassPublic: s:=crsCAMPublic;
        ctnClassPublished: s:=crsCAMPublished;
        else s:=crsCAMVisibility;
        end;
        if Item.Desc=ctnVarDefinition then
          s:=Format(crsCAMVar, [s])
        else
          s:=Format(crsCAMProperty, [s]);
        s:=s+' '+Item.Name;
        if Item.WrittenByProperty<>'' then
          s:=Format(crsCAMWrittenByProperty, [s, Item.WrittenByProperty]);
        FItems.Add(Item);
        TVNode:=MembersTreeView.Items.AddObject(nil,s,Item);
        TVNode.ImageIndex:=-1;
        if Item.Selected then
          TVNode.ImageIndex:=ImgIDImplementation;
        TVNode.SelectedIndex:=TVNode.ImageIndex;
        AVLNode:=FMemberNodeExts.FindSuccessor(AVLNode);
      end;
    end;
    MembersTreeView.EndUpdate;

  finally
    DisposeAVLTree(FMemberNodeExts);
    FChecking:=false;
  end;
end;

{$R *.lfm}

end.

