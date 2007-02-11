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
unit CheckGroupEditorDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus, ComCtrls;

type

  { TCheckGroupEditorDlg }

  TCheckGroupEditorDlg = class(TForm)
    BtnAdd: TButton;
    BtnApply: TBitBtn;
    BtnCancel: TBitBtn;
    BtnDelete: TButton;
    BtnDown: TButton;
    BtnHelp: TBitBtn;
    BtnModify: TButton;
    BtnOK: TBitBtn;
    BtnUp: TButton;
    DuplicateCheckBox: TCheckBox;
    ColumnsEdit: TEdit;
    FCheck: TCheckGroup;
    aCheck: TCheckGroup;
    FPanelButtons: TPanel;
    FPanelOKCancel: TPanel;
    ColumnsLabel: TLabel;
    LabelDisable: TLabel;
    FPopupMenu: TPopupMenu;
    ColumnsUpDown: TUpDown;
    procedure AddItem(Sender:TObject);
    procedure ColumnsEditChange(Sender: TObject);
    procedure CreateItems(Sender: TObject);
    procedure DeleteItem(Sender: TObject);
    procedure ItemClick(Sender: TObject; Index: integer);
    procedure ModifyItem(Sender: TObject);
    procedure MoveDownItem(Sender: TObject);
    procedure MoveUpItem(Sender: TObject);
    procedure EnableDisable(Sender:TObject);
    procedure ApplyCheck(Sender: TObject);
  private
    FModified: Boolean;
  public
    ItemIndex:integer;
    property Modified: Boolean read FModified write FModified;
  end;

procedure AssignCheckGroup(dstCheck, srcCheck: TCheckGroup);

implementation

uses ObjInspStrConsts;

procedure AssignCheckGroup(dstCheck, srcCheck: TCheckGroup);
var i: integer;
begin
  dstCheck.Items.Assign(srcCheck.Items);
  dstCheck.Caption:=srcCheck.Caption;
  dstCheck.Columns:=srcCheck.Columns;
  for i:=0 to srcCheck.Items.Count-1 do begin
    dstCheck.Checked[i]:=srcCheck.Checked[i];
    dstCheck.CheckEnabled[i]:=srcCheck.CheckEnabled[i]
  end;
end;

{ TCheckGroupEditorDlg }

procedure TCheckGroupEditorDlg.AddItem(Sender:TObject);
var strItem:string;
    canAdd: boolean;
begin
  if not InputQuery(clbCheckGroupEditor, clbAdd, strItem) then exit;
  canAdd:=true;
  if DuplicateCheckBox.Checked then begin
    canAdd:=(FCheck.Items.IndexOf(strItem)=-1);
    if not canAdd then
      canAdd:=MessageDlg(clbCheckGroupEditor,Format(clbCheckDuplicateMsg,[strItem]),mtConfirmation,mbYesNo,0)=mrYes;
  end;
  if canAdd then
    FCheck.Items.Add(strItem);
end;

procedure TCheckGroupEditorDlg.ColumnsEditChange(Sender: TObject);
begin
  FCheck.Columns:=ColumnsUpDown.Position;
end;

procedure TCheckGroupEditorDlg.DeleteItem(Sender:TObject);
begin
  if (FCheck.Items.Count=0)or(ItemIndex=-1) then exit;
  if MessageDlg(clbCheckGroupEditor,Format(clbDelete,[ItemIndex, FCheck.Items[ItemIndex]]),
    mtConfirmation, mbYesNo, 0)=mrYes then begin
    FCheck.Items.Delete(ItemIndex);
    if ItemIndex>FCheck.Items.Count-1 then
      ItemIndex:=FCheck.Items.Count-1;
    if ItemIndex<>-1 then
      FCheck.Controls[ItemIndex].Color:=clYellow;
  end;
end;

procedure TCheckGroupEditorDlg.MoveUpItem(Sender:TObject);
var itemtmp:string;
    checkedtmp:boolean;
begin
  if (FCheck.Items.Count<=1)or(ItemIndex<1) then exit;
   //swap the caption and the checked states
  itemtmp:=FCheck.Items[ItemIndex-1];
  checkedtmp:=FCheck.Checked[ItemIndex-1];
  FCheck.Items[ItemIndex-1]:=FCheck.Items[ItemIndex];
  FCheck.Checked[ItemIndex-1]:=FCheck.Checked[ItemIndex];
  FCheck.Items[ItemIndex]:=itemtmp;
  FCheck.Checked[ItemIndex]:=checkedtmp;
  //swap the states enabled
  checkedtmp:=FCheck.CheckEnabled[ItemIndex-1];
  FCheck.CheckEnabled[ItemIndex-1]:=FCheck.CheckEnabled[ItemIndex];
  FCheck.CheckEnabled[ItemIndex]:=checkedtmp;

  FCheck.Controls[ItemIndex].Color:=clBtnFace;
  ItemIndex:=ItemIndex-1;
  FCheck.Controls[ItemIndex].Color:=clYellow;
end;

procedure TCheckGroupEditorDlg.MoveDownItem(Sender:TObject);
var itemtmp:string;
    checkedtmp:boolean;
begin
  if (FCheck.Items.Count<=1)or(ItemIndex=FCheck.Items.Count-1)or(ItemIndex=-1) then exit;
   //swap the caption and the checked states
  itemtmp:=FCheck.Items[ItemIndex+1];
  checkedtmp:=FCheck.Checked[ItemIndex+1];
  FCheck.Items[ItemIndex+1]:=FCheck.Items[ItemIndex];
  FCheck.Checked[ItemIndex+1]:=FCheck.Checked[ItemIndex];
  FCheck.Items[ItemIndex]:=itemtmp;
  FCheck.Checked[ItemIndex]:=checkedtmp;
  //swap the states enabled
  checkedtmp:=FCheck.CheckEnabled[ItemIndex+1];
  FCheck.CheckEnabled[ItemIndex+1]:=FCheck.CheckEnabled[ItemIndex];
  FCheck.CheckEnabled[ItemIndex]:=checkedtmp;

  FCheck.Controls[ItemIndex].Color:=clBtnFace;
  ItemIndex:=ItemIndex+1;
  FCheck.Controls[ItemIndex].Color:=clYellow;
end;

procedure TCheckGroupEditorDlg.ModifyItem(Sender:TObject);
begin
  if (FCheck.Items.Count=0)or(ItemIndex=-1) then exit;
  FCheck.Items[ItemIndex]:=InputBox(clbCheckGroupEditor,clbModify,FCheck.Items[ItemIndex]);
end;

procedure TCheckGroupEditorDlg.ItemClick(Sender: TObject; Index: integer);
begin
  if ItemIndex<>-1 then
    FCheck.Controls[ItemIndex].Color:=clBtnFace;
  ItemIndex:=Index;
  if ItemIndex<>-1 then
    FCheck.Controls[ItemIndex].Color:=clYellow;
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
var i:integer;
begin
  FPopupMenu.Items.Clear;
  for i:=0 to FCheck.Items.Count-1 do begin
    FPopupMenu.Items.Add(TMenuItem.Create(self));
    FPopupMenu.Items[i].Caption:=FCheck.Items[i];
    FPopupMenu.Items[i].Checked:=FCheck.CheckEnabled[i];
    FPopupMenu.Items[i].OnClick:=@EnableDisable;
  end;;
end;

procedure TCheckGroupEditorDlg.ApplyCheck(Sender:TObject);
begin
  if Assigned(FCheck) then begin
    AssignCheckGroup(aCheck, FCheck);
    FModified := True;
  end;
end;


initialization
  {$I checkgroupeditordlg.lrs}

end.

