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
unit CheckListboxEditorDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CheckLst,
  ExtCtrls, Buttons, ComCtrls, ButtonPanel, IDEImagesIntf, ObjInspStrConsts,
  LCLType, ActnList;

type

  { TCheckListBoxEditorDlg }

  TCheckListBoxEditorDlg = class(TForm)
    actAdd: TAction;
    actDel: TAction;
    actEdit: TAction;
    actMoveUp: TAction;
    actMoveDown: TAction;
    ActionList1: TActionList;
    BtnPanel: TButtonPanel;
    FCheck: TCheckListBox;
    aCheck: TCheckListBox;
    ToolBar: TToolBar;
    tbAdd: TToolButton;
    tbDelete: TToolButton;
    ToolButton3: TToolButton;
    tbUp: TToolButton;
    tbDown: TToolButton;
    ToolButton6: TToolButton;
    tbEdit: TToolButton;
    procedure actAddExecute(Sender: TObject);
    procedure actDelExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure FCheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ApplyCheck(Sender: TObject);
  private
    { private declarations }
    FModified: Boolean;
    procedure Change;
  public
    { public declarations }
    property Modified: Boolean read FModified write FModified;
  end;
  
procedure AssignCheckList(dstCheck, srcCheck: TCheckListBox);

implementation

{$R *.lfm}

procedure AssignCheckList(dstCheck, srcCheck: TCheckListBox);
var
  i: integer;
begin
  dstCheck.Items.Clear;
  dstCheck.AllowGrayed := srcCheck.AllowGrayed;
  dstCheck.Items := srcCheck.Items;
  dstCheck.ItemHeight := srcCheck.ItemHeight;
  for i := 0 to srcCheck.Items.Count - 1 do
  begin
    if srcCheck.Items[i] <> dstCheck.Items[i] then
      dstCheck.Items[i] := srcCheck.Items[i];
    dstCheck.State[i] := srcCheck.State[i]
  end;
end;

{ TCheckListBoxEditorDlg }

procedure TCheckListBoxEditorDlg.actAddExecute(Sender: TObject);
var
  strItem: string;
begin
  if InputQuery(clbCheckListBoxEditor, clbAdd, strItem) then
  begin
    FCheck.Items.Add(strItem);
    Change;
  end;
end;

procedure TCheckListBoxEditorDlg.actDelExecute(Sender: TObject);
var
  OldIndex : integer;
begin
  if FCheck.ItemIndex = -1 then exit;
  if MessageDlg(clbCheckListBoxEditor, Format(clbDeleteQuest, [FCheck.ItemIndex, FCheck.Items[FCheck.ItemIndex]]),
    mtConfirmation, mbYesNo, 0) = mrYes then
  begin
    //  save old index
    OldIndex := FCheck.ItemIndex;
    FCheck.Items.Delete(FCheck.ItemIndex);
    if (FCheck.Count-1<OldIndex) then
      FCheck.ItemIndex := FCheck.Count-1
    else
      FCheck.ItemIndex := OldIndex;
    Change;
  end;
end;

procedure TCheckListBoxEditorDlg.actEditExecute(Sender: TObject);
begin
  if FCheck.ItemIndex = -1 then exit;
  FCheck.Items[FCheck.ItemIndex]:=InputBox(clbCheckListBoxEditor,clbModify,FCheck.Items[FCheck.ItemIndex]);
end;

procedure TCheckListBoxEditorDlg.actMoveDownExecute(Sender: TObject);
var
  itemtmp: string;
  checkedtmp: boolean;
begin
  if (FCheck.Items.Count<=1)or(FCheck.ItemIndex=FCheck.Items.Count-1)or(FCheck.ItemIndex=-1) then exit;
  itemtmp:=FCheck.Items[FCheck.ItemIndex+1];
  checkedtmp:=FCheck.Checked[FCheck.ItemIndex+1];
  FCheck.Items[FCheck.ItemIndex+1]:=FCheck.Items[FCheck.ItemIndex];
  FCheck.Checked[FCheck.ItemIndex+1]:=FCheck.Checked[FCheck.ItemIndex];
  FCheck.Items[FCheck.ItemIndex]:=itemtmp;
  FCheck.Checked[FCheck.ItemIndex]:=checkedtmp;
  FCheck.ItemIndex:=FCheck.ItemIndex+1;
  Change;
end;

procedure TCheckListBoxEditorDlg.actMoveUpExecute(Sender: TObject);
var
  itemtmp: string;
  checkedtmp: boolean;
begin
  if (FCheck.Items.Count<=1)or(FCheck.ItemIndex<1) then exit;
  itemtmp:=FCheck.Items[FCheck.ItemIndex-1];
  checkedtmp:=FCheck.Checked[FCheck.ItemIndex-1];
  FCheck.Items[FCheck.ItemIndex-1]:=FCheck.Items[FCheck.ItemIndex];
  FCheck.Checked[FCheck.ItemIndex-1]:=FCheck.Checked[FCheck.ItemIndex];
  FCheck.Items[FCheck.ItemIndex]:=itemtmp;
  FCheck.Checked[FCheck.ItemIndex]:=checkedtmp;
  FCheck.ItemIndex:=FCheck.ItemIndex-1;
  Change;
end;

procedure TCheckListBoxEditorDlg.FCheckClick(Sender: TObject);
begin
  Change;
end;

procedure TCheckListBoxEditorDlg.FormCreate(Sender: TObject);
begin
  ToolBar.Images := IDEImages.Images_16;
  actAdd.ImageIndex := IDEImages.LoadImage(16, 'laz_add');
  actDel.ImageIndex := IDEImages.LoadImage(16, 'laz_delete');
  actMoveUp.ImageIndex := IDEImages.LoadImage(16, 'arrow_up');
  actMoveDown.ImageIndex := IDEImages.LoadImage(16, 'arrow_down');
  actEdit.ImageIndex := IDEImages.LoadImage(16, 'laz_edit');

  Caption := clbCheckListBoxEditor;
  BtnPanel.OKButton.Caption := oisOk;
  BtnPanel.CancelButton.Caption := oisCancel;
  BtnPanel.HelpButton.Caption := cActionListEditorHelpCategory;
  BtnPanel.CloseButton.Kind := bkCustom;
  BtnPanel.CloseButton.LoadGlyphFromStock(idButtonYes);
  BtnPanel.CloseButton.Caption := sccsTrEdtApply;
  BtnPanel.CloseButton.OnClick := @ApplyCheck;

  actAdd.Hint := clbAdd;
  actDel.Hint := clbDeleteHint;
  actMoveUp.Hint := clbUp;
  actMoveDown.Hint := clbDown;
  actEdit.Hint := clbModify;

  actMoveUp.ShortCut := scCtrl or VK_UP;
  actMoveDown.ShortCut := scCtrl or VK_DOWN;
  Modified := False;
end;

procedure TCheckListBoxEditorDlg.ApplyCheck(Sender:TObject);
begin
  if Assigned(FCheck) then
  begin
    AssignCheckList(aCheck, FCheck);
    FModified := True;
  end;
end;

procedure TCheckListBoxEditorDlg.Change;
begin
  actDel.Enabled := FCheck.ItemIndex <> -1;
  actEdit.Enabled := FCheck.ItemIndex <> -1;
  actMoveUp.Enabled := (FCheck.ItemIndex <> -1) and (FCheck.ItemIndex > 0);
  actMoveDown.Enabled := (FCheck.ItemIndex <> -1) and (FCheck.ItemIndex < FCheck.Count - 1);
end;

end.

