{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CheckLst,
  ExtCtrls, Buttons, ComCtrls, IDEImagesIntf;

type

  { TCheckListBoxEditorDlg }

  TCheckListBoxEditorDlg = class(TForm)
    BtnApply: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    BtnOK: TBitBtn;
    FCheck: TCheckListBox;
    aCheck: TCheckListBox;
    FPanelOKCancel: TPanel;
    ToolBar: TToolBar;
    tbAdd: TToolButton;
    tbDelete: TToolButton;
    ToolButton3: TToolButton;
    tbUp: TToolButton;
    tbDown: TToolButton;
    ToolButton6: TToolButton;
    tbEdit: TToolButton;
    procedure AddItem(Sender: TObject);
    procedure DeleteItem(Sender: TObject);
    procedure FCheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ModifyItem(Sender: TObject);
    procedure MoveDownItem(Sender: TObject);
    procedure MoveUpItem(Sender: TObject);
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

uses ObjInspStrConsts;

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

procedure TCheckListBoxEditorDlg.AddItem(Sender:TObject);
var
  strItem: string;
begin
  if InputQuery(clbCheckListBoxEditor, clbAdd, strItem) then
    FCheck.Items.Add(strItem);
end;

procedure TCheckListBoxEditorDlg.DeleteItem(Sender:TObject);
begin
  if FCheck.ItemIndex = -1 then exit;
  if MessageDlg(clbCheckListBoxEditor,Format(clbDelete,[FCheck.ItemIndex,FCheck.Items[FCheck.ItemIndex]]),
    mtConfirmation, mbYesNo, 0) = mrYes then
    FCheck.Items.Delete(FCheck.ItemIndex);
end;

procedure TCheckListBoxEditorDlg.FCheckClick(Sender: TObject);
begin
  Change;
end;

procedure TCheckListBoxEditorDlg.FormCreate(Sender: TObject);
begin
  ToolBar.Images := IDEImages.Images_16;
  tbAdd.ImageIndex := IDEImages.LoadImage(16, 'add');
  tbDelete.ImageIndex := IDEImages.LoadImage(16, 'delete');
  tbUp.ImageIndex := IDEImages.LoadImage(16, 'arrow_up');
  tbDown.ImageIndex := IDEImages.LoadImage(16, 'arrow_down');
  tbEdit.ImageIndex := IDEImages.LoadImage(16, 'edit');

  Caption := clbCheckListBoxEditor;
  BtnOK.Caption := oisOk;
  BtnCancel.Caption := oisCancel;
  BtnHelp.Caption := cActionListEditorHelpCategory;
  
  tbAdd.Hint := oiscAdd;
  tbDelete.Hint := oiscDelete;
  tbUp.Hint := clbUp;
  tbDown.Hint := clbDown;
  tbEdit.Hint := clbModify;
  Modified := False;
end;

procedure TCheckListBoxEditorDlg.MoveUpItem(Sender:TObject);
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
  FCheck.ItemIndex:=FCheck.ItemIndex-1
end;

procedure TCheckListBoxEditorDlg.MoveDownItem(Sender:TObject);
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
  FCheck.ItemIndex:=FCheck.ItemIndex+1
end;

procedure TCheckListBoxEditorDlg.ModifyItem(Sender:TObject);
begin
  if FCheck.ItemIndex = -1 then exit;
  FCheck.Items[FCheck.ItemIndex]:=InputBox(clbCheckListBoxEditor,clbModify,FCheck.Items[FCheck.ItemIndex]);
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
  tbDelete.Enabled := FCheck.ItemIndex <> -1;
  tbEdit.Enabled := FCheck.ItemIndex <> -1;
  tbUp.Enabled := (FCheck.ItemIndex <> -1) and (FCheck.ItemIndex > 0);
  tbDown.Enabled := (FCheck.ItemIndex <> -1) and (FCheck.ItemIndex < FCheck.Count - 1);
end;

initialization
  {$I checklistboxeditordlg.lrs}

end.

