{ Copyright (C) 2005

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Lagunov Aleksey

  Abstract:
    Property Editors for TTreeView.
}

unit TreeViewPropEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Dialogs, Buttons, Controls, StdCtrls, ComCtrls,
  // IdeIntf
  PropEdits, ComponentEditors, ObjInspStrConsts, IDEImagesIntf, IDEWindowIntf;

type

  { TTreeViewItemsEditorForm }

  TTreeViewItemsEditorForm = class(TForm)
    BtnApply: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    BtnOK: TBitBtn;
    BtnSave: TButton;
    BtnNewItem: TButton;
    BtnNewSubItem: TButton;
    BtnDelete: TButton;
    BtnLoad: TButton;
    edtText: TEdit;
    edtIndexImg: TEdit;
    edtIndexSel: TEdit;
    edtIndexState: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    LabelText: TLabel;
    LabelImageIndex: TLabel;
    LabelSelectedIndex: TLabel;
    LabelStateIndex: TLabel;
    OpenDialog1: TOpenDialog;
    MoveDownBtn: TSpeedButton;
    MoveUpBtn: TSpeedButton;
    SaveDialog1: TSaveDialog;
    TreeView1: TTreeView;
    procedure BtnNewItemClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
    procedure MoveDownBtnClick(Sender: TObject);
    procedure TreeView1SelectionChanged(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure edtIndexStateEditingDone(Sender: TObject);
  private
    FTreeView: TCustomTreeView;
    FModified: Boolean;
    procedure LoadFromTree(ATreeView: TCustomTreeView);
    procedure SaveToTree;
    procedure UpdateEnabledStates;
  public
  end; 


type
  TTreeViewItemsProperty = class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function  GetAttributes: TPropertyAttributes; override;
  end;

  { TTreeViewComponentEditor }

  TTreeViewComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

{$R *.lfm}

function EditTreeView(ATreeView: TCustomTreeView):boolean;
var
  TreeViewItemsEditorForm: TTreeViewItemsEditorForm;
begin
  TreeViewItemsEditorForm:=TTreeViewItemsEditorForm.Create(Application);
  try
    TreeViewItemsEditorForm.LoadFromTree(ATreeView);
    if TreeViewItemsEditorForm.ShowModal = mrOk then
      TreeViewItemsEditorForm.SaveToTree;
    Result:=TreeViewItemsEditorForm.FModified;
  finally
    TreeViewItemsEditorForm.Free;
  end;
end;

{ TTreeViewItemsEditorForm }

procedure TTreeViewItemsEditorForm.FormCreate(Sender: TObject);
begin
  Caption := sccsTrEdtCaption;

  GroupBox1.Caption := sccsTrEdtGrpLCaption;
  BtnNewItem.Caption := sccsTrEdtNewItem;
  BtnNewSubItem.Caption := sccsTrEdtNewSubItem;
  BtnDelete.Caption := sccsTrEdtDelete;
  BtnLoad.Caption := sccsTrEdtLoad;
  BtnSave.Caption := sccsTrEdtSave;
  BtnApply.Caption := sccsTrEdtApply;
  IDEImages.AssignImage(MoveUpBtn, 'arrow_up');
  IDEImages.AssignImage(MoveDownBtn, 'arrow_down');
  MoveUpBtn.Hint:=rscdMoveUp;
  MoveDownBtn.Hint:=rscdMoveDown;

  GroupBox2.Caption := sccsTrEdtGrpRCaption;
  LabelText.Caption := sccsTrEdtLabelText;
  LabelImageIndex.Caption := sccsTrEdtLabelImageIndex;
  LabelSelectedIndex.Caption := sccsTrEdtLabelSelIndex;
  LabelStateIndex.Caption := sccsTrEdtLabelStateIndex;

  OpenDialog1.Title := sccsTrEdtOpenDialog;
  SaveDialog1.Title := sccsTrEdtSaveDialog;
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TTreeViewItemsEditorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TTreeViewItemsEditorForm.BtnNewItemClick(Sender: TObject);
var
  S: String;
begin
  S := sccsTrEdtItem + IntToStr(TreeView1.Items.Count);
  if (Sender as TComponent).Tag = 1 then
    TreeView1.Selected := TreeView1.Items.Add(TreeView1.Selected, S)
  else
    TreeView1.Selected := TreeView1.Items.AddChild(TreeView1.Selected, S);

  GroupBox2.Enabled := TreeView1.Items.Count > 0;
  
  edtText.SetFocus;
  edtText.SelectAll;
end;

procedure TTreeViewItemsEditorForm.Edit1Change(Sender: TObject);
begin
  if Assigned(TreeView1.Selected) then
    TreeView1.Selected.Text := edtText.Text;
end;

procedure TTreeViewItemsEditorForm.MoveUpBtnClick(Sender: TObject);
var
  CurNode, PrevNode: TTreeNode;
begin
  CurNode := TreeView1.Selected;      Assert(Assigned(CurNode));
  PrevNode := CurNode.GetPrevSibling; Assert(Assigned(PrevNode));
  CurNode.MoveTo(PrevNode, naInsert);
  UpdateEnabledStates;
end;

procedure TTreeViewItemsEditorForm.MoveDownBtnClick(Sender: TObject);
var
  CurNode, NextNode: TTreeNode;
begin
  CurNode := TreeView1.Selected;      Assert(Assigned(CurNode));
  NextNode := CurNode.GetNextSibling; Assert(Assigned(NextNode));
  CurNode.MoveTo(NextNode, naInsertBehind);
  UpdateEnabledStates;
end;

procedure TTreeViewItemsEditorForm.TreeView1SelectionChanged(Sender: TObject);
begin
  if Assigned(TreeView1.Selected) then
  begin
    edtText.Text := TreeView1.Selected.Text;
    edtIndexImg.Text := IntToStr(TreeView1.Selected.ImageIndex);
    edtIndexSel.Text := IntToStr(TreeView1.Selected.SelectedIndex);
    edtIndexState.Text := IntToStr(TreeView1.Selected.StateIndex);
  end;
  UpdateEnabledStates;
end;

procedure TTreeViewItemsEditorForm.btnApplyClick(Sender: TObject);
begin
  SaveToTree;
end;

procedure TTreeViewItemsEditorForm.btnDeleteClick(Sender: TObject);
var
  TempNode: TTreeNode;
begin
  if Assigned(TreeView1.Selected) then
  begin
    TempNode := TreeView1.Selected.GetNextSibling;
    if TempNode = nil then
      TempNode := TreeView1.Selected.GetPrevSibling;
    if TempNode = nil then
      TempNode := TreeView1.Selected.Parent;
    TreeView1.Items.Delete(TreeView1.Selected);
    if TempNode <> nil then
      TreeView1.Selected := TempNode;
    UpdateEnabledStates;
    TreeView1.SetFocus;
  end;
end;

procedure TTreeViewItemsEditorForm.btnLoadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    TreeView1.LoadFromFile(OpenDialog1.FileName);
    UpdateEnabledStates;
  end;
end;

procedure TTreeViewItemsEditorForm.btnSaveClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    TreeView1.SaveToFile(SaveDialog1.FileName);
end;

procedure TTreeViewItemsEditorForm.edtIndexStateEditingDone(Sender: TObject);
begin
  if Assigned(TreeView1.Selected) then
  begin
    TreeView1.Selected.ImageIndex := StrToIntDef(edtIndexImg.Text, -1);
    TreeView1.Selected.SelectedIndex := StrToIntDef(edtIndexSel.Text, -1);
    TreeView1.Selected.StateIndex := StrToIntDef(edtIndexState.Text, -1);
    
    edtIndexImg.Text := IntToStr(TreeView1.Selected.ImageIndex);
    edtIndexSel.Text := IntToStr(TreeView1.Selected.SelectedIndex);
    edtIndexState.Text := IntToStr(TreeView1.Selected.StateIndex);
  end;
end;

procedure TTreeViewItemsEditorForm.LoadFromTree(ATreeView: TCustomTreeView);
begin
  FTreeView := ATreeView;
  if Assigned(ATreeView) then
  begin
    TreeView1.Images := ATreeView.Images;
    TreeView1.StateImages := ATreeView.StateImages;
    TreeView1.Items.Assign(ATreeView.Items);
  end;
  UpdateEnabledStates;
end;

procedure TTreeViewItemsEditorForm.SaveToTree;
begin
  if Assigned(FTreeView) then
  begin
    FTreeView.Items.Assign(TreeView1.Items);
    FModified := True;
  end;
end;

procedure TTreeViewItemsEditorForm.UpdateEnabledStates;
var
  CurNode: TTreeNode;
begin
  CurNode := TreeView1.Selected;
  MoveUpBtn.Enabled := Assigned(CurNode) and Assigned(CurNode.GetPrevSibling);
  MoveDownBtn.Enabled:=Assigned(CurNode) and Assigned(CurNode.GetNextSibling);
  GroupBox2.Enabled := Assigned(CurNode);
end;


{ TTreeViewItemsProperty }

procedure TTreeViewItemsProperty.Edit;
begin
  if EditTreeView(GetComponent(0) as TCustomTreeView) then
    Modified;
end;

function TTreeViewItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly, paRevertable];
end;

{ TTreeViewComponentEditor }
procedure TTreeViewComponentEditor.ExecuteVerb(Index: Integer);
var
  Hook: TPropertyEditorHook;
begin
  If Index = 0 then
  begin
    GetHook(Hook);
    if EditTreeView(GetComponent as TCustomTreeView) then
      if Assigned(Hook) then
        Hook.Modified(Self);
  end;
end;

function TTreeViewComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  If Index = 0 then
    Result := sccsTrEdt;
end;

function TTreeViewComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

initialization
  RegisterPropertyEditor(ClassTypeInfo(TTreeNodes), TTreeView, 'Items', TTreeViewItemsProperty);
  RegisterComponentEditor(TTreeView,TTreeViewComponentEditor);
end.

