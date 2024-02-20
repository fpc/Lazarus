{ Copyright (C) 2005

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Lagunov Aleksey

  Abstract:
    Property Editor for TTreeView.
}

unit TreeViewPropEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType, Forms, Dialogs, Buttons, Controls, StdCtrls, ComCtrls, ImgList, Spin,
  ButtonPanel,
  // IdeIntf
  PropEdits, ComponentEditors, ObjInspStrConsts, IDEImagesIntf, IDEWindowIntf;

type

  { TTreeViewItemsEditorForm }

  TTreeViewItemsEditorForm = class(TForm)
    btnApply: TBitBtn;
    btnSave: TButton;
    btnNewItem: TButton;
    btnNewSubItem: TButton;
    btnDelete: TButton;
    btnLoad: TButton;
    ButtonPanel: TButtonPanel;
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
    procedure edtNodeTextChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnMoveClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure treEditorEditingEnd(Sender: TObject; Node: TTreeNode;
      Cancel: Boolean);
    procedure treEditorSelectionChanged(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure spnIndexChange(Sender: TObject);
  private
    fTreeView: TCustomTreeView;
    procedure LoadFromTree(aTreeView: TCustomTreeView);
    procedure SaveToTree;
    procedure UpdateEnabledStates;
    procedure UpdateImageHints;
    procedure FinishNodeEditing;
    procedure CancelNodeEditing;
  end;

  { TTreeViewItemsProperty }

  TTreeViewItemsProperty = class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
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

function EditTreeView(ATreeView: TCustomTreeView): boolean;
begin
  with TTreeViewItemsEditorForm.Create(Application) do
    try
      LoadFromTree(ATreeView);
      result := ShowModal = mrOK;
      if result then
        SaveToTree;
    finally
      Free;
    end;
end;

{ TTreeViewItemsEditorForm }

procedure TTreeViewItemsEditorForm.FormCreate(Sender: TObject);
begin
  { Captions }

  // form
  self    .Caption := sccsTrEdtCaption;
  btnApply.Caption := sccsTrEdtApply;
  // tree group
  grpTreeEditor.Caption := sccsTrEdtGrpLCaption;
  btnNewItem   .Caption := sccsTrEdtNewItem;
  btnNewSubItem.Caption := sccsTrEdtNewSubItem;
  btnDelete    .Caption := sccsTrEdtDelete;
  btnSave      .Caption := sccsTrEdtSave;
  btnLoad      .Caption := sccsTrEdtLoad;
  // node group
  grpNodeEditor   .Caption := sccsTrEdtGrpRCaption;
  lblNodeText     .Caption := sccsTrEdtLabelText;
  lblImageIndex   .Caption := sccsTrEdtLabelImageIndex;
  lblSelectedIndex.Caption := sccsTrEdtLabelSelIndex;
  lblStateIndex   .Caption := sccsTrEdtLabelStateIndex;
  // dialogs
  dlgOpen.Title := sccsTrEdtOpenDialog;
  dlgSave.Title := sccsTrEdtSaveDialog;

  { Images }

  IDEImages.AssignImage(btnMoveUp  , 'arrow_up');
  IDEImages.AssignImage(btnMoveDown, 'arrow_down');
  btnApply.LoadGlyphFromResource(idButtonRetry);

  { Hints }

  ButtonPanel.ShowHint := true;
  btnMoveUp  .Hint := rscdMoveUp   + LineEnding + '[Ctrl+Shift+Up]';
  btnMoveDown.Hint := rscdMoveDown + LineEnding + '[Ctrl+Shift+Down]';

  { Layout }

  IDEDialogLayoutList.ApplyLayout(self);
end;

procedure TTreeViewItemsEditorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(self);
end;

procedure TTreeViewItemsEditorForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // form actions
  if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    FinishNodeEditing;

    ModalResult := mrOK;
    Key := 0;
  end
  else if (Key = VK_RETURN) and (Shift = [ssShift]) then
  begin
    btnApplyClick(Sender);
    Key := 0;
  end
  else if (Key = VK_ESCAPE) and (Shift = []) then
  begin
    // pressing [Esc] in an open editor cancel text editing
    if treEditor.IsEditing then
      CancelNodeEditing
    else
      ModalResult := mrCancel;
    Key := 0;
  end
  // create item
  else if (Key = VK_N) and (Shift = [ssCtrl]) then
  begin
    btnNewItemClick(btnNewItem); // "Sender" is the pressed button
    Key := 0;
  end
  else if (Key = VK_N) and (Shift = [ssCtrl, ssShift]) then
  begin
    btnNewItemClick(btnNewSubItem); // "Sender" is the pressed button
    Key := 0;
  end
  // delete item
  else if (Key = VK_DELETE) and (Shift = []) then
  begin
    // check to prevent it from working in TEdit or TSpinEdit
    // or when renaming node
    if treEditor.Focused and not treEditor.IsEditing then
    begin
      btnDeleteClick(Sender);
      Key := 0;
    end;
  end
  // move item
  else if (Key = VK_DOWN) and (Shift = [ssCtrl, ssShift]) then
  begin
    btnMoveClick(btnMoveDown); // "Sender" is the pressed button
    Key := 0;
  end
  else if (Key = VK_UP) and (Shift = [ssCtrl, ssShift]) then
  begin
    btnMoveClick(btnMoveUp); // "Sender" is the pressed button
    Key := 0;
  end
  // move selection
  else if (Key in [VK_DOWN, VK_UP]) and (Shift = []) then
  begin
    if treEditor.IsEditing then
    begin
      // close text editor
      FinishNodeEditing;
      // select next
      if Key = VK_DOWN then
        treEditor.MoveToNextNode(true)
      else
        treEditor.MoveToPrevNode(true);
      // open text editor
      if Assigned(treEditor.Selected) then
        treEditor.Selected.EditText;

      Key := 0;
    end;
  end
  // save and load in/from file
  else if (Key = VK_S) and (Shift = [ssCtrl]) then
  begin
    btnSaveClick(Sender);
    Key := 0;
  end
  else if (Key in [VK_L, VK_O]) and (Shift = [ssCtrl]) then
  begin
    btnLoadClick(Sender);
    Key := 0;
  end;
end;

procedure TTreeViewItemsEditorForm.treEditorEditingEnd(Sender: TObject;
  Node: TTreeNode; Cancel: Boolean);
begin
  // this event can be fired when edtNodeText has already been released
  if csDestroying in ComponentState then exit;

  if Assigned(treEditor.Selected) then
  begin
    // remove event to avoid cyclic calling
    edtNodeText.OnChange := nil;
    try
      // update text
      edtNodeText.Text := treEditor.Selected.Text;
    finally
      // restore event
      edtNodeText.OnChange := @edtNodeTextChange;
    end;
  end;
end;

procedure TTreeViewItemsEditorForm.btnNewItemClick(Sender: TObject);
var
  lNewName: String;
begin
  FinishNodeEditing;

  lNewName := sccsTrEdtItem + IntToStr(treEditor.Items.Count);
  if Sender = btnNewItem then
    treEditor.Selected := treEditor.Items.Add(treEditor.Selected, lNewName)
  else if Sender = btnNewSubItem then
    treEditor.Selected := treEditor.Items.AddChild(treEditor.Selected, lNewName)
  else
    raise Exception.Create('[btnNewItemClick] Unknown Sender');

  UpdateEnabledStates;
end;

procedure TTreeViewItemsEditorForm.edtNodeTextChange(Sender: TObject);
begin
  if Assigned(treEditor.Selected) then
    treEditor.Selected.Text := edtNodeText.Text;
end;

procedure TTreeViewItemsEditorForm.btnMoveClick(Sender: TObject);
var
  lCurNode: TTreeNode;
begin
  FinishNodeEditing;

  lCurNode := treEditor.Selected;
  if lCurNode = nil then exit;

  if Sender = btnMoveUp then
  begin
    if lCurNode.GetPrevSibling = nil then exit;
    lCurNode.MoveTo(lCurNode.GetPrevSibling, naInsert);
  end
  else if Sender = btnMoveDown then
  begin
    if lCurNode.GetNextSibling = nil then exit;
    lCurNode.MoveTo(lCurNode.GetNextSibling, naInsertBehind);
  end else
    raise Exception.Create('[btnMoveClick] Unknown Sender');

  UpdateEnabledStates;
  treEditor.SetFocus; // return focus after button click
end;

procedure TTreeViewItemsEditorForm.treEditorSelectionChanged(Sender: TObject);
  //
  procedure UpdateImageIndexes;
  begin
    // Remove events to avoid cyclic calling
    spnImageIndex   .OnChange := nil;
    spnSelectedIndex.OnChange := nil;
    spnStateIndex   .OnChange := nil;
    try
      // Read the indexes of the selected item
      spnImageIndex   .Value := treEditor.Selected.ImageIndex;
      spnSelectedIndex.Value := treEditor.Selected.SelectedIndex;
      spnStateIndex   .Value := treEditor.Selected.StateIndex;
    finally
      // Restore events
      spnImageIndex   .OnChange := @spnIndexChange;
      spnSelectedIndex.OnChange := @spnIndexChange;
      spnStateIndex   .OnChange := @spnIndexChange;
    end;
  end;
  //
begin
  if Assigned(treEditor.Selected) then
  begin
    edtNodeText.Text := treEditor.Selected.Text;
    UpdateImageIndexes;
    UpdateImageHints;
  end;

  UpdateEnabledStates;
end;

procedure TTreeViewItemsEditorForm.btnApplyClick(Sender: TObject);
begin
  FinishNodeEditing;

  SaveToTree;
end;

procedure TTreeViewItemsEditorForm.btnDeleteClick(Sender: TObject);
var
  lNextNode: TTreeNode;
begin
  if Assigned(treEditor.Selected) then
  begin
    // find node for new selection
    lNextNode := treEditor.Selected.GetNextSibling;
    if lNextNode = nil then
      lNextNode := treEditor.Selected.GetPrevSibling;
    if lNextNode = nil then
      lNextNode := treEditor.Selected.Parent;

    // delete
    treEditor.Items.Delete(treEditor.Selected);

    // select "next" node
    if lNextNode <> nil then
      treEditor.Selected := lNextNode;

    UpdateEnabledStates;
    treEditor.SetFocus; // return focus after button click
  end;
end;

procedure TTreeViewItemsEditorForm.btnLoadClick(Sender: TObject);
begin
  FinishNodeEditing;

  if dlgOpen.Execute then
  begin
    treEditor.LoadFromFile(dlgOpen.FileName);

    treEditor.FullExpand;
    treEditor.Selected := treEditor.Items.GetFirstNode;
    treEditor.SetFocus; // return focus after button click
    UpdateEnabledStates;
  end;
end;

procedure TTreeViewItemsEditorForm.btnSaveClick(Sender: TObject);
  //
  function ImagesFound: boolean;
  var
    i: Integer;
  begin
    for i := 0 to treEditor.Items.Count - 1 do
      if (treEditor.Items[i].ImageIndex    >= 0) or
         (treEditor.Items[i].SelectedIndex >= 0) or
         (treEditor.Items[i].StateIndex    >= 0)
      then
        exit(true);
    result := false;
  end;
  //
  function ConfirmImagesLoss: boolean;
  begin
    if ImagesFound then
      result := QuestionDlg(sccsTrEdtConfirmationCaption, sccsTrEdtConfirmationImages,
        TMsgDlgType.mtConfirmation, [mrYes, sccsTrEdtYes, mrNo, sccsTrEdtNo,
        mrCancel, sccsTrEdtCancel], 0) = mrYes
    else
      result := true;
  end;
  //
  function ConfirmReplace: boolean;
  begin
    if FileExists(dlgSave.FileName) then
      result := QuestionDlg(sccsTrEdtConfirmationCaption, sccsTrEdtConfirmationReplace,
        TMsgDlgType.mtConfirmation, [mrYes, sccsTrEdtYes, mrNo, sccsTrEdtNo,
        mrCancel, sccsTrEdtCancel], 0) = mrYes
    else
      result := true;
  end;
  //
begin
  FinishNodeEditing;

  if ConfirmImagesLoss and dlgSave.Execute and ConfirmReplace then
    treEditor.SaveToFile(dlgSave.FileName);

  treEditor.SetFocus; // return focus after button click
end;

procedure TTreeViewItemsEditorForm.spnIndexChange(Sender: TObject);
begin
  if Assigned(treEditor.Selected) then
  begin
    treEditor.Selected.ImageIndex    := spnImageIndex   .Value;
    treEditor.Selected.SelectedIndex := spnSelectedIndex.Value;
    treEditor.Selected.StateIndex    := spnStateIndex   .Value;

    UpdateImageHints;
  end;
end;

// Show hints and "*" in label for invalid index
procedure TTreeViewItemsEditorForm.UpdateImageHints;

  procedure UpdateImageHint(aSpinEdit: TSpinEdit;
    aIsStateImages: boolean; aLabel: TLabel; aCaption: string);
  var
    lImageList: TCustomImageList;
    lPropName: string;
  begin
    if aIsStateImages then
    begin
      lImageList := treEditor.StateImages;
      lPropName := fTreeView.Name + '.StateImages';
    end else begin
      lImageList := treEditor.Images;
      lPropName := fTreeView.Name + '.Images';
    end;

    // check valid index
    if aSpinEdit.Value >= 0 then
    begin
      // check assign
      if lImageList = nil then
      begin
        aLabel.Hint := Format(sccsTrEdtImageListNotAssigned, [lPropName]);
        aLabel.ShowHint := true;
      end else begin
        // check count
        if aSpinEdit.Value >= lImageList.Count then
        begin
          aLabel.Hint := Format(sccsTrEdtInvalidIndex, [lPropName, lImageList.Count]);
          aLabel.ShowHint := true;
        end else
          aLabel.ShowHint := false;
      end;
    end else
      aLabel.ShowHint := false;

    // show asterisk if necessary
    if aLabel.ShowHint then
      aLabel.Caption := aCaption + '*'
    else
      aLabel.Caption := aCaption;
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

procedure TTreeViewItemsEditorForm.FinishNodeEditing;
begin
  if treEditor.Items.Count > 0 then
    treEditor.Items[0].EndEdit(false);
end;

procedure TTreeViewItemsEditorForm.CancelNodeEditing;
begin
  if treEditor.Items.Count > 0 then
    treEditor.Items[0].EndEdit(true);
end;

procedure TTreeViewItemsEditorForm.LoadFromTree(aTreeView: TCustomTreeView);
begin
  fTreeView := aTreeView;
  if Assigned(aTreeView) then
  begin
    // images
    treEditor.Images      := aTreeView.Images;
    treEditor.StateImages := aTreeView.StateImages;
    // items
    treEditor.Items.Assign(aTreeView.Items);
  end;
  treEditor.Selected := treEditor.Items.GetFirstNode;
  UpdateEnabledStates;
end;

procedure TTreeViewItemsEditorForm.SaveToTree;
begin
  if Assigned(fTreeView) then
    fTreeView.Items.Assign(treEditor.Items);
end;

procedure TTreeViewItemsEditorForm.UpdateEnabledStates;
var
  lCurNode: TTreeNode;
begin
  lCurNode := treEditor.Selected;

  // control states
  btnSave      .Enabled := treEditor.Items.Count > 0;
  btnMoveUp    .Enabled := Assigned(lCurNode) and Assigned(lCurNode.GetPrevSibling);
  btnMoveDown  .Enabled := Assigned(lCurNode) and Assigned(lCurNode.GetNextSibling);
  btnDelete    .Enabled := Assigned(lCurNode);
  grpNodeEditor.Enabled := Assigned(lCurNode);

  // clear disabled fields
  if lCurNode = nil then
  begin
    edtNodeText.Text := '';

    spnImageIndex   .Value := -1;
    spnSelectedIndex.Value := -1;
    spnStateIndex   .Value := -1;
  end;
end;

{ TTreeViewItemsProperty }

procedure TTreeViewItemsProperty.Edit;
begin
  if EditTreeView(GetComponent(0) as TCustomTreeView) then
    Modified;
end;

function TTreeViewItemsProperty.GetAttributes: TPropertyAttributes;
begin
  result := [paDialog, paReadOnly, paRevertable];
end;

{ TTreeViewComponentEditor }

procedure TTreeViewComponentEditor.ExecuteVerb(Index: Integer);
var
  Hook: TPropertyEditorHook;
begin
  if Index = 0 then
  begin
    GetHook(Hook);
    if EditTreeView(GetComponent as TCustomTreeView) then
      if Assigned(Hook) then
        Hook.Modified(self);
  end;
end;

function TTreeViewComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    result := sccsTrEdt
  else
    result := '';
end;

function TTreeViewComponentEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

initialization
  RegisterPropertyEditor(ClassTypeInfo(TTreeNodes), TTreeView, 'Items', TTreeViewItemsProperty);
  RegisterComponentEditor(TTreeView, TTreeViewComponentEditor);
end.

