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
}
unit ColumnDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources, ComCtrls,
  StdCtrls, Buttons, ExtCtrls;

type
  // TODO create more generic collection editor.
  TColumnDlg = class(TForm)
    Listbox1: TLISTBOX;
    Label1: TLABEL;
    Edit1: TEDIT;
    Label2: TLABEL;
    Edit2: TEDIT;
    Button1: TBUTTON;
    Button2: TBUTTON;
    Radiogroup1: TRADIOGROUP;
    Button3: TBUTTON;
    Button4: TBUTTON;
    btnOK : TBitBtn;
    btnCancel : TBitBtn;
    cbVisible : TCheckbox;
    cbAutoSize : TCheckBox;
  private
    { private declarations }
    FColumns: TListColumns;
    FSelectedIndex : Integer;
    procedure DisplayColumn(Value : Integer);
    procedure SetColumns(const AValue: TListColumns);
  protected
    procedure Button1OnClick(sender : TObject);
    procedure Button2OnClick(sender : TObject);
    procedure Button3OnClick(sender : TObject);
    procedure Button4OnClick(sender : TObject);
    procedure RadioGroup1OnClick(sender : TObject);
    procedure Listbox1OnClick(sender : TObject);
    Procedure Edit1OnChange(Sender : TObject);
    Procedure Edit2OnChange(Sender : TObject);
    Procedure cbVisibleOnClick(Sender : TObject);
    Procedure cbAutoSizeOnClick(Sender : TObject);
    Procedure FormOnShow(Sender : TObject);
  public
    { public declarations }
    constructor Create(AOwner : TComponent); override;
    property Columns: TListColumns read FColumns write SetColumns;
  end;


implementation

{ TColumnDlg }

constructor TColumnDlg.Create(AOwner : TComponent);
Begin
  inherited;
//  if LazarusResources.Find(Classname)=nil then
  begin
     Caption := 'Column Editor';
     Width := 400;
     Height := 340;
     OnShow := @FormOnShow;
     Position := poScreenCenter;
     Listbox1 := TListBox.Create(self);
     with Listbox1 do
       Begin
         Parent := Self;
         left := 1;
         Width := 170;
         Top := 1;
         Height := 270;
         Visible := True;
         OnClick := @Listbox1OnClick;
       end;
     Label1 := TLabel.Create(self);
     with Label1 do
       Begin
          Parent := self;
          Caption := 'Caption';
          Left := self.width div 2;
          Top := 15;
          Visible := True;
       end;

     Edit1 := TEdit.Create(self);
     with Edit1 do
       Begin
         Parent := Self;
         Text := '';
         Left := self.Width div 2;
         Height := 25;
         Top := Label1.Top+Label1.Height+5;
          Visible := True;
          OnChange := @Edit1OnChange;
       end;
         
     Label2 := TLabel.Create(self);
     with Label2 do
       Begin
          Parent := self;
          Caption := 'Width';
          Left := self.width div 2;
          Top := Edit1.Top+Edit1.Height+5;
          Visible := True;
       end;

     Edit2 := TEdit.Create(self);
     with Edit2 do
       Begin
         Parent := Self;
         Text := '';
         Left := self.Width div 2;
         Height := 25;
         Top := Label2.Top+Label2.Height+5;
         Visible := True;
         OnChange := @Edit2OnChange;
       end;
       
     RadioGroup1 := TRadioGroup.Create(self);
     with RadioGroup1 do
       Begin
         Parent := Self;
         Caption := 'Alignment';
         Left := self.Width div 2;
         Top := Edit2.Top+Edit2.Height+5;
         Visible := True;
         Columns := 3;
         Height := 50;
         Width := 200;
         Items.Add('Left');
         Items.Add('Center');
         Items.Add('Right');
         ItemIndex := 0;
         OnClick := @RadioGroup1OnClick;
       end;

     cbVisible := TCheckBox.Create(self);
     with cbVisible do
       begin
         Parent := Self;
         Visible := True;
         Caption := 'Visible';
         Left := self.width div 2;
         Top :=  RadioGroup1.Top+RadioGroup1.Height+5;
         Height := 25;
         Checked := True;
         OnClick := @cbVisibleOnClick;
       end;

     cbAutoSize := TCheckBox.Create(self);
     with cbAutoSize do
       begin
         Parent := Self;
         Visible := True;
         Caption := 'Auto Size';
         Left := self.width div 2;
         Top :=  cbVisible.Top + cbVisible.Height + 5;
         Height := 25;
         Checked := True;
         OnClick := @cbAutoSizeOnClick;
       end;

     Button1 := TButton.Create(self);
     with Button1 do
       Begin
          Parent := self;
          Caption := 'Add';
          Left := self.width div 2;
          Top := cbAutoSize.Top+cbAutoSize.Height+5;
          Visible := True;
          OnClick := @Button1OnClick;
       end;

     Button2 := TButton.Create(self);
     with Button2 do
       Begin
          Parent := self;
          Caption := 'Delete';
          Left := Button1.Left+Button1.Width+5;
          Top := Button1.Top;
          Visible := True;
          OnClick := @Button2OnClick;
       end;
       
     Button3 := TButton.Create(self);
     with Button3 do
       Begin
          Parent := self;
          Caption := 'Move up';
          Left := 5;
          Top := ListBox1.Top+Listbox1.Height+5;
          Visible := True;
          OnClick := @Button3OnClick;
       end;

     Button4 := TButton.Create(self);
     with Button4 do
       Begin
          Parent := self;
          Caption := 'Move down';
          Left := Button3.Left+Button3.Width+5;
          Top := Button3.Top;
          Visible := True;
          OnClick := @Button4OnClick;
       end;
       
     btnOK := TBitbtn.Create(self);
     with btnOK do
       Begin
          Parent := self;
          Caption := 'OK';
          Left := self.Width div 2+5;
          Top := Button3.Top;
          Visible := True;
          kind := bkOK;
       end;

     btnCancel := TBitbtn.Create(self);
     with btnCancel do
       Begin
          Parent := self;
          Caption := 'Cancel';
          Left := btnOK.left + btnOK.Width + 5;
          Top :=btnOK.top;
          Visible := True;
          Kind := bkCancel;
       end;

  end;
  FColumns := TListColumns.Create(nil);
  FSelectedIndex:= -1;
end;


procedure TColumnDlg.Button1OnClick(sender : TObject);
var
  Column : TListColumn;
Begin
  //add
  Column := FColumns.Add;
  Column.Caption := 'Caption';
  FSelectedIndex := Column.Index;
  Listbox1.Items.Add(Column.Caption);
  Listbox1.Selected[FSelectedIndex] := True;
  DisplayColumn(FSelectedIndex);
end;

procedure TColumnDlg.Listbox1OnClick(sender : TObject);
var
  I : Integer;
begin
  Edit1.ReadOnly := True;
  FSelectedIndex := -1;
  if Listbox1.SelCount = 0 then Exit;
  Edit1.ReadOnly := False;
  I := 0;
  While not Listbox1.Selected[i] do
    inc(i);
  DisplayColumn(I);
  
end;

Procedure TColumnDlg.Edit1OnChange(Sender : TObject);
Var
  ListColumn : TListColumn;
begin
  if FSelectedIndex = -1 then Exit;
  ListColumn := FColumns[FSelectedIndex];
  ListColumn.Caption := Edit1.Caption;
  Listbox1.Items[FSelectedIndex] := Edit1.Caption;
  Listbox1.Selected[FSelectedIndex] := True;
end;

Procedure TColumnDlg.Edit2OnChange(Sender : TObject);
Var
  ListColumn : TListColumn;
begin
  if FSelectedIndex = -1 then Exit;
  ListColumn := FColumns[FSelectedIndex];
  if Edit2.Caption = '' then
    ListColumn.Width := 0
    else
    try
      ListColumn.Width := StrtoInt(Edit2.Caption);
    except
        raise Exception.Create('Invalid numeric Value');
        Edit2.Caption := '0';
    end;
end;

procedure TColumnDlg.Button2OnClick(sender : TObject);
var
  Index : Integer;
begin
  //delete
  if FSelectedIndex = -1 then Exit;

  Index := FSelectedIndex;
  FSelectedIndex := -1;
  FColumns[Index].Free;
  Listbox1.Items.Delete(Index);
  if Index > 0 then
  Listbox1.Selected[Index-1] := True;
  DisplayColumn(Index-1);
end;

procedure TColumnDlg.Button3OnClick(sender : TObject);
Var
  ListColumn : TListColumn;
  Index : Integer;
begin
  //move up
  if FSelectedIndex <= 0 then Exit;
  Index := FSelectedIndex;
  FSelectedIndex := -1;
  ListColumn := FColumns[Index];
  ListColumn.Index := Index - 1;
  
  Listbox1.Items.Insert(Index-1,ListColumn.Caption);
  Listbox1.Items.Delete(Index+1);
  Listbox1.Selected[Index-1] := True;
  DisplayColumn(Index-1);
end;

procedure TColumnDlg.Button4OnClick(sender : TObject);
Var
  ListColumn : TListColumn;
  Index : Integer;
begin
  //move down
  if FSelectedIndex = -1 then Exit;
  if (FSelectedIndex >= Listbox1.Items.Count-1) then Exit;

  Index := FSelectedIndex;
  FSelectedIndex := -1;

  ListColumn := FColumns[Index];
  ListColumn.Index := Index + 1;

  Listbox1.Items.Insert(Index+2,ListColumn.Caption);
  Listbox1.Items.Delete(Index);
  Listbox1.Selected[Index+1] := True;
  DisplayColumn(Index+1);
end;

Procedure TColumnDlg.DisplayColumn(Value : Integer);
Var
  ListColumn : TListColumn;
begin
  FSelectedIndex := -1;
  if Value = -1 then exit;

  ListColumn := FColumns[Value];
  Edit1.Caption := ListColumn.Caption;
  Edit2.Caption := Inttostr(Integer(ListColumn.Width));

  case ListColumn.Alignment of
    taLeftJustify :  RadioGroup1.ItemIndex := 0;
    taCenter:        RadioGroup1.ItemIndex := 1;
    taRightJustify : RadioGroup1.ItemIndex := 2;
  end;  //case
  
  cbVisible.Checked := ListColumn.Visible;
  cbAutoSize.Checked := ListColumn.AutoSize;

  FSelectedIndex := Value;
end;

procedure TColumnDlg.SetColumns(const AValue: TListColumns);
begin
  FColumns.Assign(AValue);
end;

procedure TColumnDlg.RadioGroup1OnClick(sender : TObject);
Var
  ListColumn : TListColumn;
begin
  if FSelectedIndex = -1 then Exit;
  ListColumn := FColumns[FSelectedIndex];
  case  RadioGroup1.ItemIndex of
     0 : ListColumn.Alignment := taLeftJustify;
     1 : ListColumn.Alignment := taCenter;
     2 : ListColumn.Alignment := taRightJustify;
  end;
end;

Procedure TColumnDlg.FormOnShow(Sender : TObject);
var
  I : Integer;
begin
  //clear the listbox and display the items if any...
  Listbox1.Items.Clear;
  for I := 0 to FColumns.Count-1 do begin
    writeln('TColumnDlg.FormOnShow ',i,' "',FColumns[i].Caption,'"');
    Listbox1.Items.Add(FColumns[i].Caption);
  end;

  if Listbox1.Items.Count > 0 then
  begin
    Listbox1.Selected[0] := True;
    DisplayColumn(0);
  end;
end;

procedure TColumnDlg.cbVisibleOnClick(Sender : TObject);
begin
  if FSelectedIndex = -1 then Exit;
  FColumns[FSelectedIndex].Visible := cbVisible.Checked;
end;

procedure TColumnDlg.cbAutoSizeOnClick(Sender : TObject);
begin
  if FSelectedIndex = -1 then Exit;
  FColumns[FSelectedIndex].AutoSize := cbAutoSize.Checked;
end;

initialization
  { $I columndlg.lrs}

end.

