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
  ButtonPanel, ExtCtrls,
  // IdeIntf
  PropEdits, ComponentEditors, ObjInspStrConsts, IDEImagesIntf, IDEWindowIntf;

type

  { TTreeViewItemsEditorForm }

  TTreeViewItemsEditorForm = class(TForm)
    btnApply: TBitBtn;
    ButtonPanel: TButtonPanel;
    lblImageIndex: TLabel;
    lblSelectedIndex: TLabel;
    lblStateIndex: TLabel;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    pnlStateIndex: TPanel;
    pnlSelectedIndex: TPanel;
    pnlImageIndex: TPanel;
    pnlImageIndexes: TPanel;
    spnImageIndex: TSpinEdit;
    spnSelectedIndex: TSpinEdit;
    spnStateIndex: TSpinEdit;
    ToolBar: TToolBar;
    tbNewItem: TToolButton;
    tbNewSubItem: TToolButton;
    tb1: TToolButton;
    tbDelete: TToolButton;
    tb2: TToolButton;
    tbRename: TToolButton;
    tb3: TToolButton;
    tbMoveDown: TToolButton;
    tbMoveUp: TToolButton;
    tb4: TToolButton;
    tbSave: TToolButton;
    tbOpen: TToolButton;
    treEditor: TTreeView;
    procedure btnApplyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure spnIndexChange(Sender: TObject);
    procedure tbDeleteClick(Sender: TObject);
    procedure tbMoveClick(Sender: TObject);
    procedure tbNewItemSubItemClick(Sender: TObject);
    procedure tbOpenClick(Sender: TObject);
    procedure tbRenameClick(Sender: TObject);
    procedure tbSaveClick(Sender: TObject);
    procedure treEditorSelectionChanged(Sender: TObject);
  private
    fModified: Boolean;
    fTreeView: TCustomTreeView;
    procedure LoadFromTree(aTreeView: TCustomTreeView);
    procedure SaveToTree;
    procedure UpdateEnabledStates;
    procedure UpdateImageHints;
    procedure FinishNodeEditing;
    procedure CancelNodeEditing;
    property Modified: Boolean read fModified write fModified;
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

{ The function returns TRUE if the tree has been changed. }
function EditTreeView(ATreeView: TCustomTreeView): boolean;
begin
  with TTreeViewItemsEditorForm.Create(Application) do
    try
      LoadFromTree(ATreeView);
      if ShowModal = mrOK then
        SaveToTree;
      Result := Modified;
    finally
      Free;
    end;
end;

{ TTreeViewItemsEditorForm }

procedure TTreeViewItemsEditorForm.FormCreate(Sender: TObject);
begin
  { Captions }

  // form
  self   .Caption := sccsTrEdtCaption;
  btnApply.Caption := sccsTrEdtApply;

  // toolbar
  ToolBar.ShowHint := true;
  tbNewItem   .Hint := sccsTrEdtNewItem    + ' [Ctrl+N]';
  tbNewSubItem.Hint := sccsTrEdtNewSubItem + ' [Ctrl+Shift+N]';
  tbDelete    .Hint := sccsTrEdtDelete     + ' [Del]';
  tbRename    .Hint := sccsTrEdtRename     + ' [F2]';
  tbMoveDown  .Hint := rscdMoveDown        + ' [Ctrl+Shift+Down]';
  tbMoveUp    .Hint := rscdMoveUp          + ' [Ctrl+Shift+Up]';
  tbSave      .Hint := sccsTrEdtSave       + ' [Ctrl+S]';
  tbOpen      .Hint := sccsTrEdtOpen       + ' [Ctrl+O]';

  // image indexes
  lblImageIndex   .Caption := sccsTrEdtLabelImageIndex + ':';
  lblSelectedIndex.Caption := sccsTrEdtLabelSelIndex   + ':';
  lblStateIndex   .Caption := sccsTrEdtLabelStateIndex + ':';

  // dialogs
  dlgOpen.Title := sccsTrEdtOpenDialog;
  dlgSave.Title := sccsTrEdtSaveDialog;

  // button panel
  ButtonPanel.ShowHint := true;

  { Images }

  btnApply.LoadGlyphFromResource(idButtonRetry);

  { Hints }

  // toolbar
  ToolBar.Images := IDEImages.Images_16;
  tbNewItem   .ImageIndex := IDEImages.LoadImage('new_item');
  tbNewSubItem.ImageIndex := IDEImages.LoadImage('new_subitem');
  tbDelete    .ImageIndex := IDEImages.LoadImage('item_delete');
  tbRename    .ImageIndex := IDEImages.LoadImage('item_rename');
  tbMoveDown  .ImageIndex := IDEImages.LoadImage('item_down');
  tbMoveUp    .ImageIndex := IDEImages.LoadImage('item_up');
  tbSave      .ImageIndex := IDEImages.LoadImage('items_save');
  tbOpen      .ImageIndex := IDEImages.LoadImage('items_load');

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
    tbNewItemSubItemClick(tbNewItem); // "Sender" is the pressed button
    Key := 0;
  end
  else if (Key = VK_N) and (Shift = [ssCtrl, ssShift]) then
  begin
    tbNewItemSubItemClick(tbNewSubItem); // "Sender" is the pressed button
    Key := 0;
  end
  // delete item
  else if (Key = VK_DELETE) and (Shift = []) then
  begin
    // check to prevent it from working in TEdit or TSpinEdit
    // or when renaming node
    if treEditor.Focused and not treEditor.IsEditing then
    begin
      tbDeleteClick(Sender);
      Key := 0;
    end;
  end
  // move item
  else if (Key = VK_DOWN) and (Shift = [ssCtrl, ssShift]) then
  begin
    tbMoveClick(tbMoveDown); // "Sender" is the pressed button
    Key := 0;
  end
  else if (Key = VK_UP) and (Shift = [ssCtrl, ssShift]) then
  begin
    tbMoveClick(tbMoveUp); // "Sender" is the pressed button
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
    tbSaveClick(Sender);
    Key := 0;
  end
  else if (Key in [VK_L, VK_O]) and (Shift = [ssCtrl]) then
  begin
    tbOpenClick(Sender);
    Key := 0;
  end;
end;

procedure TTreeViewItemsEditorForm.tbRenameClick(Sender: TObject);
begin
  treEditor.Selected.EditText;
end;

procedure TTreeViewItemsEditorForm.tbNewItemSubItemClick(Sender: TObject);
var
  lEdited: Boolean;
  lNewName: String;
begin
  lEdited := treEditor.IsEditing;
  FinishNodeEditing;

  lNewName := sccsTrEdtItem + IntToStr(treEditor.Items.Count);
  if Sender = tbNewItem then
    treEditor.Selected := treEditor.Items.Add(treEditor.Selected, lNewName)
  else if Sender = tbNewSubItem then
    treEditor.Selected := treEditor.Items.AddChild(treEditor.Selected, lNewName)
  else
    raise Exception.Create('[tbNewItemClick] Unknown Sender');

  if lEdited and Assigned(treEditor.Selected) then
  begin
    treEditor.SetFocus;
    treEditor.Selected.EditText;
  end;

  UpdateEnabledStates;
end;

procedure TTreeViewItemsEditorForm.tbMoveClick(Sender: TObject);
var
  lCurNode: TTreeNode;
begin
  FinishNodeEditing;

  lCurNode := treEditor.Selected;
  if lCurNode = nil then exit;

  if Sender = tbMoveUp then
  begin
    if lCurNode.GetPrevSibling = nil then exit;
    lCurNode.MoveTo(lCurNode.GetPrevSibling, naInsert);
  end
  else if Sender = tbMoveDown then
  begin
    if lCurNode.GetNextSibling = nil then exit;
    lCurNode.MoveTo(lCurNode.GetNextSibling, naInsertBehind);
  end else
    raise Exception.Create('[tbMoveClick] Unknown Sender');

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
    UpdateImageIndexes;
    UpdateImageHints;
  end;

  UpdateEnabledStates;
end;

procedure TTreeViewItemsEditorForm.btnApplyClick(Sender: TObject);
begin
  FinishNodeEditing;
  SaveToTree;
  fModified := true;
end;

procedure TTreeViewItemsEditorForm.tbDeleteClick(Sender: TObject);
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

procedure TTreeViewItemsEditorForm.tbOpenClick(Sender: TObject);
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

procedure TTreeViewItemsEditorForm.tbSaveClick(Sender: TObject);

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
  lAssigned: Boolean;
begin
  lCurNode := treEditor.Selected;
  lAssigned := Assigned(lCurNode);

  // control states
  pnlImageIndexes .Enabled := lAssigned;
  tbDelete        .Enabled := lAssigned;
  tbRename        .Enabled := lAssigned;
  tbMoveUp        .Enabled := lAssigned and Assigned(lCurNode.GetPrevSibling);
  tbMoveDown      .Enabled := lAssigned and Assigned(lCurNode.GetNextSibling);
  tbSave          .Enabled := treEditor.Items.Count > 0;

  // clear disabled fields
  if lCurNode = nil then
  begin
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

