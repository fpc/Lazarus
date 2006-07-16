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
  ExtCtrls, Buttons;

type

  { TCheckListBoxEditorDlg }

  TCheckListBoxEditorDlg = class(TForm)
    BtnApply: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    BtnOK: TBitBtn;
    BtnAdd: TButton;
    BtnDelete: TButton;
    BtnDown: TButton;
    BtnModify: TButton;
    BtnUp: TButton;
    FCheck: TCheckListBox;
    aCheck: TCheckListBox;
    FPanelOKCancel: TPanel;
    FPanelButtons: TPanel;
    procedure AddItem(Sender: TObject);
    procedure DeleteItem(Sender: TObject);
    procedure ModifyItem(Sender: TObject);
    procedure MoveDownItem(Sender: TObject);
    procedure MoveUpItem(Sender: TObject);
    procedure ApplyCheck(Sender: TObject);
  private
    { private declarations }
    FModified: Boolean;
  public
    { public declarations }
    property Modified: Boolean read FModified write FModified;
  end;
  
procedure AssignCheck(dstCheck, srcCheck: TCheckListBox);

implementation

uses ObjInspStrConsts;

procedure AssignCheck(dstCheck, srcCheck: TCheckListBox);
var i: integer;
begin
  dstCheck.Items.Clear;
  dstCheck.Items:=srcCheck.Items;
  dstCheck.ItemHeight:=srcCheck.ItemHeight;
  for i:=0 to srcCheck.Items.Count-1 do begin
    if srcCheck.Items[i]<>dstCheck.Items[i] then
        dstCheck.Items[i]:=srcCheck.Items[i];
    dstCheck.Checked[i]:=srcCheck.Checked[i]
  end;
end;

{ TCheckListBoxEditorDlg }

procedure TCheckListBoxEditorDlg.AddItem(Sender:TObject);
var strItem:string;
begin
  if InputQuery(clbCheckListBoxEditor, clbAdd, strItem) then
    FCheck.Items.Add(strItem);
end;

procedure TCheckListBoxEditorDlg.DeleteItem(Sender:TObject);
begin
  if FCheck.ItemIndex=-1 then exit;
  if MessageDlg(clbCheckListBoxEditor,Format(clbDelete,[FCheck.ItemIndex,FCheck.Items[FCheck.ItemIndex]]),
    mtConfirmation, mbYesNo, 0)=mrYes then
    FCheck.Items.Delete(FCheck.ItemIndex);
end;

procedure TCheckListBoxEditorDlg.MoveUpItem(Sender:TObject);
var itemtmp:string;
    checkedtmp:boolean;
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
var itemtmp:string;
    checkedtmp:boolean;
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
  if FCheck.ItemIndex=-1 then exit;
  FCheck.Items[FCheck.ItemIndex]:=InputBox(clbCheckListBoxEditor,clbModify,FCheck.Items[FCheck.ItemIndex]);
end;

procedure TCheckListBoxEditorDlg.ApplyCheck(Sender:TObject);
//var aCheck:TCheckListBox; //coppola
begin
//  if FCheck.ItemIndex=-1 then exit;
  if Assigned(FCheck) then begin
    AssignCheck(aCheck, FCheck);
    FModified := True;
  end;

//  AssignCheck(aCheck, FCheck);
//  Modified; coppola
end;

initialization
  {$I checklistboxeditordlg.lrs}

end.

