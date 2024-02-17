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
  Forms, Dialogs, Buttons, Controls, StdCtrls, ComCtrls, ImgList, Spin,
  // IdeIntf
  PropEdits, ComponentEditors, ObjInspStrConsts, IDEImagesIntf, IDEWindowIntf;

type

  { TTreeViewItemsEditorForm }

  TTreeViewItemsEditorForm = class(TForm)
    btnApply: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnSave: TButton;
    btnNewItem: TButton;
    btnNewSubItem: TButton;
    btnDelete: TButton;
    btnLoad: TButton;
    edtNodeText: TEdit;
    grpTreeEditor: TGroupBox;
    grpNodeEditor: TGroupBox;
    lblNodeText: TLabel;
    lblImageIndex: TLabel;
    lblSelectedIndex: TLabel;
    lblStateIndex: TLabel;
    dlgOpen: TOpenDialog;
    btnMoveDown: TSpeedButton;
    btnMoveUp: TSpeedButton;
    dlgSave: TSaveDialog;
    spnImageIndex: TSpinEdit;
    spnSelectedIndex: TSpinEdit;
    spnStateIndex: TSpinEdit;
    treEditor: TTreeView;
    procedure btnNewItemClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure treEditorSelectionChanged(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure spnIndexChange(Sender: TObject);
  private
    FTreeView: TCustomTreeView;
    FModified: Boolean;
    procedure LoadFromTree(ATreeView: TCustomTreeView);
    procedure SaveToTree;
    procedure UpdateEnabledStates;
    procedure UpdateImageHints;
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

  grpTreeEditor.Caption := sccsTrEdtGrpLCaption;
  btnNewItem.Caption := sccsTrEdtNewItem;
  btnNewSubItem.Caption := sccsTrEdtNewSubItem;
  btnDelete.Caption := sccsTrEdtDelete;
  btnLoad.Caption := sccsTrEdtLoad;
  btnSave.Caption := sccsTrEdtSave;
  btnApply.Caption := sccsTrEdtApply;
  IDEImages.AssignImage(btnMoveUp, 'arrow_up');
  IDEImages.AssignImage(btnMoveDown, 'arrow_down');
  btnMoveUp.Hint:=rscdMoveUp;
  btnMoveDown.Hint:=rscdMoveDown;

  grpNodeEditor.Caption := sccsTrEdtGrpRCaption;
  lblNodeText.Caption := sccsTrEdtLabelText;
  lblImageIndex.Caption := sccsTrEdtLabelImageIndex + ':';
  lblSelectedIndex.Caption := sccsTrEdtLabelSelIndex + ':';
  lblStateIndex.Caption := sccsTrEdtLabelStateIndex + ':';

  dlgOpen.Title := sccsTrEdtOpenDialog;
  dlgSave.Title := sccsTrEdtSaveDialog;
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TTreeViewItemsEditorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TTreeViewItemsEditorForm.btnNewItemClick(Sender: TObject);
var
  S: String;
begin
  S := sccsTrEdtItem + IntToStr(treEditor.Items.Count);
  if (Sender as TComponent).Tag = 1 then
    treEditor.Selected := treEditor.Items.Add(treEditor.Selected, S)
  else
    treEditor.Selected := treEditor.Items.AddChild(treEditor.Selected, S);

  grpNodeEditor.Enabled := treEditor.Items.Count > 0;
  
  edtNodeText.SetFocus;
  edtNodeText.SelectAll;
end;

procedure TTreeViewItemsEditorForm.Edit1Change(Sender: TObject);
begin
  if Assigned(treEditor.Selected) then
    treEditor.Selected.Text := edtNodeText.Text;
end;

procedure TTreeViewItemsEditorForm.btnMoveUpClick(Sender: TObject);
var
  CurNode, PrevNode: TTreeNode;
begin
  CurNode := treEditor.Selected;      Assert(Assigned(CurNode));
  PrevNode := CurNode.GetPrevSibling; Assert(Assigned(PrevNode));
  CurNode.MoveTo(PrevNode, naInsert);
  UpdateEnabledStates;
end;

procedure TTreeViewItemsEditorForm.btnMoveDownClick(Sender: TObject);
var
  CurNode, NextNode: TTreeNode;
begin
  CurNode := treEditor.Selected;      Assert(Assigned(CurNode));
  NextNode := CurNode.GetNextSibling; Assert(Assigned(NextNode));
  CurNode.MoveTo(NextNode, naInsertBehind);
  UpdateEnabledStates;
end;

procedure TTreeViewItemsEditorForm.treEditorSelectionChanged(Sender: TObject);
begin
  if Assigned(treEditor.Selected) then
  begin
    // Update node text
    edtNodeText.Text := treEditor.Selected.Text;

    // Update image indexes
    // Remove events to avoid cyclic calling
    spnImageIndex.OnChange := nil;
    spnSelectedIndex.OnChange := nil;
    spnStateIndex.OnChange := nil;
    try
      // Read the indexes of the selected item
      spnImageIndex.Value := treEditor.Selected.ImageIndex;
      spnSelectedIndex.Value := treEditor.Selected.SelectedIndex;
      spnStateIndex.Value := treEditor.Selected.StateIndex;
    finally
      // Restore events
      spnImageIndex.OnChange := @spnIndexChange;
      spnSelectedIndex.OnChange := @spnIndexChange;
      spnStateIndex.OnChange := @spnIndexChange;
    end;
    // Update hints
    UpdateImageHints;
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
  if Assigned(treEditor.Selected) then
  begin
    TempNode := treEditor.Selected.GetNextSibling;
    if TempNode = nil then
      TempNode := treEditor.Selected.GetPrevSibling;
    if TempNode = nil then
      TempNode := treEditor.Selected.Parent;
    treEditor.Items.Delete(treEditor.Selected);
    if TempNode <> nil then
      treEditor.Selected := TempNode;
    UpdateEnabledStates;
    treEditor.SetFocus;
  end;
end;

procedure TTreeViewItemsEditorForm.btnLoadClick(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    treEditor.LoadFromFile(dlgOpen.FileName);
    UpdateEnabledStates;
  end;
end;

procedure TTreeViewItemsEditorForm.btnSaveClick(Sender: TObject);
begin
  if dlgSave.Execute then
    treEditor.SaveToFile(dlgSave.FileName);
end;

procedure TTreeViewItemsEditorForm.spnIndexChange(Sender: TObject);
begin
  if Assigned(treEditor.Selected) then
  begin
    treEditor.Selected.ImageIndex := spnImageIndex.Value;
    treEditor.Selected.SelectedIndex := spnSelectedIndex.Value;
    treEditor.Selected.StateIndex := spnStateIndex.Value;

    UpdateImageHints;
  end;
end;

// Show hints and "*" in label for invalid index
procedure TTreeViewItemsEditorForm.UpdateImageHints;

  procedure UpdateImageHint(ASpinEdit: TSpinEdit;
    AIsStateImages: boolean; ALabel: TLabel; ACaption: string);
  var
    lImageList: TCustomImageList;
    lPropName: string;
  begin
    if AIsStateImages then
    begin
      lImageList := treEditor.StateImages;
      lPropName := FTreeView.Name + '.StateImages';
    end else begin
      lImageList := treEditor.Images;
      lPropName := FTreeView.Name + '.Images';
    end;

    // check valid index
    if ASpinEdit.Value >= 0 then
    begin
      // check assign
      if lImageList = nil then
      begin
        ALabel.Hint := Format(sccsTrEdtImageListNotAssigned, [lPropName]);
        ALabel.ShowHint := true;
      end else begin
        // check count
        if ASpinEdit.Value >= lImageList.Count then
        begin
          ALabel.Hint := Format(sccsTrEdtInvalidIndex, [lPropName, lImageList.Count]);
          ALabel.ShowHint := true;
        end else
          ALabel.ShowHint := false;
      end;
    end else
      aLabel.ShowHint := false;

    // show asterisk if necessary
    if ALabel.ShowHint then
      ALabel.Caption := ACaption + '*:'
    else
      ALabel.Caption := ACaption + ':';
  end;
  //
begin
  if Assigned(treEditor.Selected) then
  begin
    UpdateImageHint(spnImageIndex,    false, lblImageIndex,    sccsTrEdtLabelImageIndex);
    UpdateImageHint(spnSelectedIndex, false, lblSelectedIndex, sccsTrEdtLabelSelIndex);
    UpdateImageHint(spnStateIndex,    true,  lblStateIndex,    sccsTrEdtLabelStateIndex);
  end;
end;

procedure TTreeViewItemsEditorForm.LoadFromTree(ATreeView: TCustomTreeView);
begin
  FTreeView := ATreeView;
  if Assigned(ATreeView) then
  begin
    treEditor.Images := ATreeView.Images;
    treEditor.StateImages := ATreeView.StateImages;
    treEditor.Items.Assign(ATreeView.Items);
  end;
  UpdateEnabledStates;
end;

procedure TTreeViewItemsEditorForm.SaveToTree;
begin
  if Assigned(FTreeView) then
  begin
    FTreeView.Items.Assign(treEditor.Items);
    FModified := True;
  end;
end;

procedure TTreeViewItemsEditorForm.UpdateEnabledStates;
var
  CurNode: TTreeNode;
begin
  CurNode := treEditor.Selected;
  btnMoveUp.Enabled := Assigned(CurNode) and Assigned(CurNode.GetPrevSibling);
  btnMoveDown.Enabled:=Assigned(CurNode) and Assigned(CurNode.GetNextSibling);
  grpNodeEditor.Enabled := Assigned(CurNode);
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

