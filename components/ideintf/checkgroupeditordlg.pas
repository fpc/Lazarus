{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CheckGroupEditorDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus, ComCtrls,
  ObjInspStrConsts, IDEImagesIntf, ButtonPanel;

type

  { TCheckGroupEditorDlg }

  TCheckGroupEditorDlg = class(TForm)
    DuplicateCheckBox: TCheckBox;
    ColumnsEdit: TEdit;
    FCheck: TCheckGroup;
    aCheck: TCheckGroup;
    ColumnsLabel: TLabel;
    BtnPanel: TButtonPanel;
    FPopupMenu: TPopupMenu;
    ColumnsUpDown: TUpDown;
    LabelDisable: TLabel;
    ToolBar: TToolBar;
    tbAdd: TToolButton;
    tbDelete: TToolButton;
    ToolButton3: TToolButton;
    tbUp: TToolButton;
    tbDown: TToolButton;
    ToolButton6: TToolButton;
    tbEdit: TToolButton;
    procedure AddItem(Sender:TObject);
    procedure ColumnsEditChange(Sender: TObject);
    procedure CreateItems(Sender: TObject);
    procedure DeleteItem(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ItemClick(Sender: TObject; Index: integer);
    procedure ModifyItem(Sender: TObject);
    procedure MoveDownItem(Sender: TObject);
    procedure MoveUpItem(Sender: TObject);
    procedure EnableDisable(Sender:TObject);
    procedure ApplyCheck(Sender: TObject);
  private
    FItemIndex: Integer;
    FModified: Boolean;
    procedure Change;
    procedure SetItemIndex(const AValue: Integer);
  public
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Modified: Boolean read FModified write FModified;
  end;

procedure AssignCheckGroup(dstCheck, srcCheck: TCheckGroup);

implementation

{$R *.lfm}

const
  NormalColor = clBtnFace;
  SelectedColor = clHighlight;

procedure AssignCheckGroup(dstCheck, srcCheck: TCheckGroup);
var
  i: integer;
begin
  dstCheck.Items.Assign(srcCheck.Items);
  dstCheck.Caption := srcCheck.Caption;
  dstCheck.Columns := srcCheck.Columns;
  for i := 0 to srcCheck.Items.Count - 1 do
  begin
    dstCheck.Checked[i] := srcCheck.Checked[i];
    dstCheck.CheckEnabled[i] := srcCheck.CheckEnabled[i]
  end;
end;

{ TCheckGroupEditorDlg }

procedure TCheckGroupEditorDlg.AddItem(Sender:TObject);
var 
  strItem: string;
  canAdd: boolean;
begin
  if not InputQuery(cgCheckGroupEditor, clbAdd, strItem) then
    Exit;
  canAdd := True;
  if DuplicateCheckBox.Checked then
  begin
    canAdd := (FCheck.Items.IndexOf(strItem)= -1);
    if not canAdd then
      canAdd := MessageDlg(cgCheckGroupEditor, Format(cgCheckDuplicateMsg,[strItem]), mtConfirmation, mbYesNo, 0) = mrYes;
  end;
  if canAdd then
  begin
    FCheck.Items.Add(strItem);
    Change;
  end;
end;

procedure TCheckGroupEditorDlg.ColumnsEditChange(Sender: TObject);
begin
  FCheck.Columns := ColumnsUpDown.Position;
end;

procedure TCheckGroupEditorDlg.DeleteItem(Sender:TObject);
begin
  if (FCheck.Items.Count = 0) or (ItemIndex = -1) then
    Exit;
  if MessageDlg(cgCheckGroupEditor,
    Format(clbDeleteQuest, [ItemIndex, FCheck.Items[ItemIndex]]),
    mtConfirmation, mbYesNo, 0) = mrYes then
  begin
    FCheck.Items.Delete(ItemIndex);
    if ItemIndex > FCheck.Items.Count - 1 then
      ItemIndex := FCheck.Items.Count - 1;
    if ItemIndex <> -1 then
      FCheck.Controls[ItemIndex].Color := SelectedColor;
  end;
end;

procedure TCheckGroupEditorDlg.FormCreate(Sender: TObject);
begin
  ToolBar.Images := IDEImages.Images_16;
  tbAdd.ImageIndex := IDEImages.LoadImage(16, 'laz_add');
  tbDelete.ImageIndex := IDEImages.LoadImage(16, 'laz_delete');
  tbUp.ImageIndex := IDEImages.LoadImage(16, 'arrow_up');
  tbDown.ImageIndex := IDEImages.LoadImage(16, 'arrow_down');
  tbEdit.ImageIndex := IDEImages.LoadImage(16, 'laz_edit');

  Caption := cgCheckGroupEditor;
  FItemIndex := -1;
  ColumnsLabel.Caption := cgColumns;
  DuplicateCheckBox.Caption := cgCheckDuplicate;
  LabelDisable.Caption := cgDisable;
  BtnPanel.CloseButton.Caption := sccsTrEdtApply;
  BtnPanel.CloseButton.Kind := bkCustom;
  BtnPanel.CloseButton.Glyph := nil;
  BtnPanel.CloseButton.ModalResult := mrNone;
  BtnPanel.CloseButton.OnClick := @ApplyCheck;

  tbAdd.Hint := clbAdd;
  tbDelete.Hint := clbDeleteHint;
  tbUp.Hint := clbUp;
  tbDown.Hint := clbDown;
  tbEdit.Hint := clbModify;
  Change;
end;

procedure TCheckGroupEditorDlg.MoveUpItem(Sender:TObject);
var
  itemtmp: string;
  checkedtmp: boolean;
begin
  if (FCheck.Items.Count <= 1) or (ItemIndex < 1) then
    Exit;
   //swap the caption and the checked states
  itemtmp := FCheck.Items[ItemIndex - 1];
  checkedtmp := FCheck.Checked[ItemIndex - 1];
  FCheck.Items[ItemIndex - 1] := FCheck.Items[ItemIndex];
  FCheck.Checked[ItemIndex - 1] := FCheck.Checked[ItemIndex];
  FCheck.Items[ItemIndex] := itemtmp;
  FCheck.Checked[ItemIndex] := checkedtmp;
  //swap the states enabled
  checkedtmp := FCheck.CheckEnabled[ItemIndex - 1];
  FCheck.CheckEnabled[ItemIndex - 1] := FCheck.CheckEnabled[ItemIndex];
  FCheck.CheckEnabled[ItemIndex] := checkedtmp;

  FCheck.Controls[ItemIndex].Color := NormalColor;
  ItemIndex := ItemIndex - 1;
  FCheck.Controls[ItemIndex].Color := SelectedColor;
end;

procedure TCheckGroupEditorDlg.MoveDownItem(Sender:TObject);
var
  itemtmp: string;
  checkedtmp: boolean;
begin
  if (FCheck.Items.Count <= 1) or (ItemIndex = FCheck.Items.Count-1) or (ItemIndex=-1) then
    Exit;
   //swap the caption and the checked states
  itemtmp := FCheck.Items[ItemIndex + 1];
  checkedtmp := FCheck.Checked[ItemIndex + 1];
  FCheck.Items[ItemIndex + 1] := FCheck.Items[ItemIndex];
  FCheck.Checked[ItemIndex + 1] := FCheck.Checked[ItemIndex];
  FCheck.Items[ItemIndex] := itemtmp;
  FCheck.Checked[ItemIndex] := checkedtmp;
  //swap the states enabled
  checkedtmp := FCheck.CheckEnabled[ItemIndex + 1];
  FCheck.CheckEnabled[ItemIndex + 1] := FCheck.CheckEnabled[ItemIndex];
  FCheck.CheckEnabled[ItemIndex] := checkedtmp;

  FCheck.Controls[ItemIndex].Color := NormalColor;
  ItemIndex := ItemIndex + 1;
  FCheck.Controls[ItemIndex].Color := SelectedColor;
end;

procedure TCheckGroupEditorDlg.ModifyItem(Sender:TObject);
begin
  if (FCheck.Items.Count = 0) or (ItemIndex = -1) then
    Exit;
  FCheck.Items[ItemIndex] := InputBox(cgCheckGroupEditor, clbModify, FCheck.Items[ItemIndex]);
end;

procedure TCheckGroupEditorDlg.ItemClick(Sender: TObject; Index: integer);
begin
  if ItemIndex <> -1 then
    FCheck.Controls[ItemIndex].Color := NormalColor;
  ItemIndex := Index;
  if ItemIndex <> -1 then
    FCheck.Controls[ItemIndex].Color := SelectedColor;
end;

procedure TCheckGroupEditorDlg.EnableDisable(Sender:TObject);
var i:integer;
begin
  for i:=0 to FCheck.Items.Count-1 do begin
    if (Sender=FPopupMenu.Items[i]) then
      FCheck.CheckEnabled[i]:=not FCheck.CheckEnabled[i]
  end;
end;

procedure TCheckGroupEditorDlg.CreateItems(Sender:TObject);
var
  i: integer;
begin
  FPopupMenu.Items.Clear;
  for i := 0 to FCheck.Items.Count-1 do
  begin
    FPopupMenu.Items.Add(TMenuItem.Create(self));
    FPopupMenu.Items[i].Caption := FCheck.Items[i];
    FPopupMenu.Items[i].Checked := FCheck.CheckEnabled[i];
    FPopupMenu.Items[i].OnClick := @EnableDisable;
  end;
end;

procedure TCheckGroupEditorDlg.ApplyCheck(Sender:TObject);
begin
  if Assigned(FCheck) then
  begin
    AssignCheckGroup(aCheck, FCheck);
    FModified := True;
  end;
end;

procedure TCheckGroupEditorDlg.Change;
begin
  tbDelete.Enabled := ItemIndex <> -1;
  tbEdit.Enabled := ItemIndex <> -1;
  tbUp.Enabled := (ItemIndex <> -1) and (ItemIndex > 0);
  tbDown.Enabled := (ItemIndex <> -1) and (ItemIndex < FCheck.Items.Count - 1);
end;

procedure TCheckGroupEditorDlg.SetItemIndex(const AValue: Integer);
begin
  if FItemIndex <> AValue then
  begin
    FItemIndex := AValue;
    Change;
  end;
end;

end.

