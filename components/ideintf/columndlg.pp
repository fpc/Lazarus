{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit ColumnDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLProc, Forms, Controls, Graphics, Dialogs, LResources,
  ComCtrls, StdCtrls, Buttons, ExtCtrls,
  // IdeIntf
  ObjInspStrConsts;

type
  // TODO create more generic collection editor.
  TColumnDlg = class(TForm)
    ColumnsListBox: TLISTBOX;
    CaptionLabel: TLABEL;
    CaptionEdit: TEDIT;
    WidthLabel: TLABEL;
    WidthEdit: TEDIT;
    AddButton: TBUTTON;
    DeleteButton: TBUTTON;
    AlignmentRadioGroup: TRADIOGROUP;
    MoveUpButton: TBUTTON;
    MoveDownButton: TBUTTON;
    btnOK : TBitBtn;
    btnCancel : TBitBtn;
    cbVisible : TCheckbox;
    cbAutoSize : TCheckBox;
  private
    FColumns: TListColumns;
    FSelectedIndex : Integer;
    procedure DisplayColumn(Value : Integer);
    procedure SetColumns(const AValue: TListColumns);
  protected
    procedure AddButtonOnClick({%H-}sender : TObject);
    procedure DeleteButtonOnClick({%H-}sender : TObject);
    procedure MoveUpButtonOnClick({%H-}sender : TObject);
    procedure MoveDownButtonOnClick({%H-}sender : TObject);
    procedure AlignmentRadioGroupOnClick({%H-}sender : TObject);
    procedure ColumnsListBoxOnClick({%H-}sender : TObject);
    Procedure CaptionEditOnChange(Sender : TObject);
    Procedure WidthEditOnChange(Sender : TObject);
    Procedure cbVisibleOnClick(Sender : TObject);
    Procedure cbAutoSizeOnClick(Sender : TObject);
    Procedure FormOnShow(Sender : TObject);
    procedure EnableComponents;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteDebugReport;
    property Columns: TListColumns read FColumns write SetColumns;
  end;


implementation

{ TColumnDlg }

constructor TColumnDlg.Create(TheOwner: TComponent);
Begin
  inherited Create(TheOwner);
  Caption := rscdColumnEditor;
  Width := 400;
  Height := 340;
  OnShow := @FormOnShow;
  Position := poScreenCenter;
  
  ColumnsListBox := TListBox.Create(self);
  with ColumnsListBox do
   Begin
     Parent := Self;
     Left := 1;
     Width := 170;
     Top := 1;
     Height := Self.ClientHeight-Top-40;
     OnClick := @ColumnsListBoxOnClick;
   end;
   
  CaptionLabel := TLabel.Create(self);
  with CaptionLabel do
   Begin
      Parent := self;
      Caption := rscdCaption;
      Left := ColumnsListBox.Left+ColumnsListBox.Width+10;
      Top := 15;
   end;

  CaptionEdit := TEdit.Create(self);
  with CaptionEdit do
   Begin
     Parent := Self;
     Text := '';
     Left := CaptionLabel.Left;
     Height := 25;
     Top := CaptionLabel.Top+CaptionLabel.Height+5;
     OnChange := @CaptionEditOnChange;
   end;

  WidthLabel := TLabel.Create(self);
  with WidthLabel do
   Begin
     Parent := self;
     Caption := rscdWidth;
     Left := CaptionLabel.Left;
     Top := CaptionEdit.Top+CaptionEdit.Height+5;
   end;

  WidthEdit := TEdit.Create(self);
  with WidthEdit do
   Begin
     Parent := Self;
     Text := '';
     Left := WidthLabel.Left;
     Height := 25;
     Top := WidthLabel.Top+WidthLabel.Height+5;
     OnChange := @WidthEditOnChange;
   end;

  AlignmentRadioGroup := TRadioGroup.Create(self);
  with AlignmentRadioGroup do
   Begin
     Parent := Self;
     Caption := rscdAlignment;
     Left := CaptionLabel.Left;
     Top := WidthEdit.Top+WidthEdit.Height+5;
     Columns := 3;
     Height := 50;
     Width := 200;
     Items.Add(rscdLeft);
     Items.Add(sccsILEdtCenter);
     Items.Add(rscdRight);
     ItemIndex := 0;
     OnClick := @AlignmentRadioGroupOnClick;
   end;

  cbVisible := TCheckBox.Create(self);
  with cbVisible do
   begin
     Parent := Self;
     Caption := rscdVisible;
     Left := CaptionLabel.Left;
     Top :=  AlignmentRadioGroup.Top+AlignmentRadioGroup.Height+5;
     Height := 25;
     Checked := True;
     OnClick := @cbVisibleOnClick;
   end;

  cbAutoSize := TCheckBox.Create(self);
  with cbAutoSize do
   begin
     Parent := Self;
     Caption := rscdAutoSize;
     Left := CaptionLabel.Left;
     Top :=  cbVisible.Top + cbVisible.Height + 5;
     Height := 25;
     Checked := True;
     OnClick := @cbAutoSizeOnClick;
   end;

  AddButton := TButton.Create(self);
  with AddButton do
   Begin
     Parent := self;
     Caption := oiColEditAdd;
     Left := CaptionLabel.Left;
     Top := cbAutoSize.Top+cbAutoSize.Height+5;
     OnClick := @AddButtonOnClick;
   end;

  DeleteButton := TButton.Create(self);
  with DeleteButton do
   Begin
     Parent := self;
     Caption := oisDelete;
     Left := AddButton.Left+AddButton.Width+5;
     Top := AddButton.Top;
     OnClick := @DeleteButtonOnClick;
   end;

  MoveUpButton := TButton.Create(self);
  with MoveUpButton do
   Begin
     Parent := self;
     Caption := rscdMoveUp;
     Left := 5;
     Top := ColumnsListBox.Top+ColumnsListBox.Height+5;
     OnClick := @MoveUpButtonOnClick;
   end;

  MoveDownButton := TButton.Create(self);
  with MoveDownButton do
   Begin
     Parent := self;
     Caption := rscdMoveDown;
     Left := MoveUpButton.Left+MoveUpButton.Width+5;
     Top := MoveUpButton.Top;
     OnClick := @MoveDownButtonOnClick;
   end;

  btnOK := TBitbtn.Create(self);
  with btnOK do
   Begin
     Parent := self;
     Caption := rscdOK;
     Left := CaptionLabel.Left;
     Top := MoveUpButton.Top;
     kind := bkOK;
   end;

  btnCancel := TBitbtn.Create(self);
  with btnCancel do
   Begin
     Parent := self;
     Caption := oiStdActDataSetCancel1Hint;
     Left := btnOK.Left + btnOK.Width + 5;
     Top := btnOK.Top;
     Kind := bkCancel;
   end;
   
  FColumns := TListColumns.Create(nil);
  FSelectedIndex:= -1;
end;

destructor TColumnDlg.Destroy;
begin
  FreeAndNil(FColumns);
  inherited Destroy;
end;

procedure TColumnDlg.AddButtonOnClick(sender : TObject);
var
  Column : TListColumn;
Begin
  //add
  Column := FColumns.Add;
  Column.Caption := rscdCaption;
  FSelectedIndex := Column.Index;
  ColumnsListBox.Items.Add(Column.Caption);
  ColumnsListBox.Selected[FSelectedIndex] := True;
  DisplayColumn(FSelectedIndex);
  //WriteDebugReport;
end;

procedure TColumnDlg.ColumnsListBoxOnClick(sender : TObject);
var
  I : Integer;
begin
  CaptionEdit.ReadOnly := True;
  FSelectedIndex := -1;
  if ColumnsListBox.SelCount = 0 then Exit;
  CaptionEdit.ReadOnly := False;
  I := 0;
  While not ColumnsListBox.Selected[i] do
    inc(i);
  DisplayColumn(I);
end;

Procedure TColumnDlg.CaptionEditOnChange(Sender : TObject);
Var
  ListColumn : TListColumn;
begin
  if FSelectedIndex = -1 then Exit;
  ListColumn := FColumns[FSelectedIndex];
  ListColumn.Caption := CaptionEdit.Caption;
  ColumnsListBox.Items[FSelectedIndex] := CaptionEdit.Caption;
  ColumnsListBox.Selected[FSelectedIndex] := True;
end;

Procedure TColumnDlg.WidthEditOnChange(Sender : TObject);
Var
  ListColumn : TListColumn;
begin
  if FSelectedIndex = -1 then Exit;
  ListColumn := FColumns[FSelectedIndex];
  if WidthEdit.Caption = '' then
    ListColumn.Width := 0
  else
    try
      ListColumn.Width := StrToInt(WidthEdit.Caption);
    except
      showmessage(rscdInvalidNumericValue);
      // revert to previous value
      WidthEdit.Caption := IntToStr(ListColumn.Width);
    end;
end;

procedure TColumnDlg.DeleteButtonOnClick(sender : TObject);
var
  Index : Integer;
begin
  //delete
  if FSelectedIndex = -1 then Exit;
  Index := FSelectedIndex;
  FSelectedIndex := -1;
  FColumns[Index].Free;
  ColumnsListBox.Items.Delete(Index);
  if Index > 0 then
  ColumnsListBox.Selected[Index-1] := True;
  DisplayColumn(Index-1);
end;

procedure TColumnDlg.MoveUpButtonOnClick(sender : TObject);
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
  
  ColumnsListBox.Items.Insert(Index-1,ListColumn.Caption);
  ColumnsListBox.Items.Delete(Index+1);
  ColumnsListBox.Selected[Index-1] := True;
  DisplayColumn(Index-1);
end;

procedure TColumnDlg.MoveDownButtonOnClick(sender : TObject);
Var
  ListColumn : TListColumn;
  Index : Integer;
begin
  //move down
  if FSelectedIndex = -1 then Exit;
  if (FSelectedIndex >= ColumnsListBox.Items.Count-1) then Exit;

  Index := FSelectedIndex;
  FSelectedIndex := -1;

  ListColumn := FColumns[Index];
  ListColumn.Index := Index + 1;

  ColumnsListBox.Items.Insert(Index+2,ListColumn.Caption);
  ColumnsListBox.Items.Delete(Index);
  ColumnsListBox.Selected[Index+1] := True;
  DisplayColumn(Index+1);
end;

Procedure TColumnDlg.DisplayColumn(Value : Integer);
Var
  ListColumn : TListColumn;
begin
  FSelectedIndex := -1;
  if Value >=0 then begin

    ListColumn := FColumns[Value];
    CaptionEdit.Caption := ListColumn.Caption;
    WidthEdit.Caption := IntToStr(Integer(ListColumn.Width));

    case ListColumn.Alignment of
      taLeftJustify :  AlignmentRadioGroup.ItemIndex := 0;
      taCenter:        AlignmentRadioGroup.ItemIndex := 1;
      taRightJustify : AlignmentRadioGroup.ItemIndex := 2;
    end;  //case

    cbVisible.Checked := ListColumn.Visible;
    cbAutoSize.Checked := ListColumn.AutoSize;
  end;

  FSelectedIndex := Value;
  EnableComponents;
end;

procedure TColumnDlg.SetColumns(const AValue: TListColumns);
begin
  FColumns.Assign(AValue);
end;

procedure TColumnDlg.AlignmentRadioGroupOnClick(sender : TObject);
Var
  ListColumn : TListColumn;
begin
  if FSelectedIndex = -1 then Exit;
  ListColumn := FColumns[FSelectedIndex];
  case  AlignmentRadioGroup.ItemIndex of
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
  ColumnsListBox.Items.Clear;
  for I := 0 to FColumns.Count-1 do begin
    DebugLn('TColumnDlg.FormOnShow ',IntToStr(i),' "',FColumns[i].Caption,'"');
    ColumnsListBox.Items.Add(FColumns[i].Caption);
  end;

  if ColumnsListBox.Items.Count > 0 then
  begin
    ColumnsListBox.Selected[0] := True;
    DisplayColumn(0);
  end;
  EnableComponents;
end;

procedure TColumnDlg.EnableComponents;
var
  AColumnIsSelected: boolean;
begin
  AColumnIsSelected:=FSelectedIndex>=0;

  CaptionEdit.Enabled:=AColumnIsSelected;
  WidthEdit.Enabled:=AColumnIsSelected;
  AlignmentRadioGroup.Enabled:=AColumnIsSelected;
end;

procedure TColumnDlg.WriteDebugReport;
var
  I: Integer;
begin
  DebugLn('TColumnDlg.WriteDebugReport: ');
  for I := 0 to ColumnsListBox.Items.Count-1 do begin
    DebugLn('ListBox: ',IntToStr(i),' "',ColumnsListBox.Items[I],'"');
  end;
  for I := 0 to FColumns.Count-1 do begin
    DebugLn('Columns: ',IntToStr(i),' "',FColumns[i].Caption,'" ');
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

end.

