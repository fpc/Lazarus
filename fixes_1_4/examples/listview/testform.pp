unit testform;

{$mode objfpc} {$H+} 

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, StdCtrls, ExtCtrls, Spin;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    ChBoxOwnerData: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    HideSelection: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    lblSetCount: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListView1: TListView;
    PageControl1: TPageControl;
    Page1: TTabSheet;
    Page2: TTabSheet;
    Page3: TTabSheet;
    OwnerDataCount: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure ChBoxOwnerDataChange(Sender: TObject);
    procedure CheckBox1Click (Sender: TObject );
    procedure CheckBox2Click (Sender: TObject );
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Change(Sender: TObject);
    procedure CheckBox7Change(Sender: TObject);
    procedure ComboBox1Change (Sender: TObject );
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure Edit10Change (Sender: TObject );
    procedure Edit11Change(Sender: TObject);
    procedure Edit1Change (Sender: TObject );
    procedure Edit2Change (Sender: TObject );
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure Edit7Change(Sender: TObject);
    procedure Edit8Change(Sender: TObject);
    procedure HideSelectionChange(Sender: TObject);
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure OwnerDataCountChange(Sender: TObject);
  private
    procedure ShowItemData;
    procedure ShowColumnData;
  protected
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  Item: TListItem;
begin
  if ListView1.OwnerData then
    // OwnerDataCount.OnChange() triggers new items count.
    OwnerDataCount.Value := ListView1.Items.Count + 1
  else
  begin
    Item := ListView1.Items.Insert(StrToIntDef(Edit1.Text, 0));
    Item.Caption := Format('Item %d', [ListView1.Items.Count]);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Item: TListItem;
  Idx: Integer;
begin
  if ListView1.OwnerData then
    OwnerDataCount.Value := ListView1.Items.Count - 1
  else
  begin
    Idx := StrToIntDef(Edit1.Text, 0);
    if (Idx < 0) or (Idx > ListView1.Items.Count - 1) then
      exit;
    Item := ListView1.Items[Idx];
    Item.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Column: TListColumn;
begin
  Column := ListView1.Columns.Add;
  Column.Caption := Format('Column %d', [ListView1.Columns.Count]);
  Column.Index := (StrToIntDef(Edit2.Text, 0));
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Column: TListColumn;
begin
  Column := ListView1.Columns[StrToIntDef(Edit2.Text, 0)];
  Column.Free;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  Item: TListItem;
  Idx: Integer;
begin
  Idx := StrToIntDef(Edit1.Text, 0);
  if (Idx < 0) or (Idx > ListView1.Items.Count - 1) then
    exit;
  Item := ListView1.Items[Idx];
  Item.Selected := True;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  Item: TListItem;
  Idx: Integer;
begin
  Idx := StrToIntDef(Edit1.Text, 0);
  if (Idx < 0) or (Idx > ListView1.Items.Count - 1) then
    exit;
  Item := ListView1.Items[Idx];
  Item.Focused := True;
end;

procedure TForm1.ChBoxOwnerDataChange(Sender: TObject);
var
  B: Boolean;
  AItem: TListItem;
begin
  B := TCheckBox(Sender).Checked;
  lblSetCount.Enabled := B;
  OwnerDataCount.Enabled := B;
  ListView1.BeginUpdate;
  try
    ListView1.Clear; // first clear all items because virtual and normal items are different classes
  finally
    ListView1.EndUpdate;
  end;
  ListView1.OwnerData := B;
  ListView1.BeginUpdate;
  try
    if B then
      ListView1.Items.Count := OwnerDataCount.Value
    else
    begin
      with ListView1 do
      begin
        AItem := Items.Add;
        AItem.Caption := 'New 1';
        AItem.ImageIndex := 0;
        AItem.StateIndex := 0;
        AItem.SubItems.Add('Sub 1');

        AItem := Items.Add;
        AItem.Caption := 'New 2';
      end;
    end;
  finally
    ListView1.EndUpdate;
  end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
var
  Column: TListColumn;
begin
  Column := ListView1.Columns[StrToIntDef(Edit2.Text, 0)];
  Column.Visible := CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
var
  Column: TListColumn;
begin
  Column := ListView1.Columns[StrToIntDef(Edit2.Text, 0)];
  Column.Autosize := CheckBox2.Checked;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  Listview1.Multiselect := CheckBox3.Checked;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  ListView1.RowSelect := CheckBox4.Checked;
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  ListView1.ShowColumnHeaders := CheckBox5.Checked;
end;

procedure TForm1.CheckBox6Change(Sender: TObject);
begin
  ListView1.AutoWidthLastColumn := TCheckBox(Sender).Checked;
end;

procedure TForm1.CheckBox7Change(Sender: TObject);
begin
  ListView1.ReadOnly := TCheckBox(Sender).Checked;
end;

procedure TForm1.ComboBox1Change (Sender: TObject );
var
  Column: TListColumn;
begin
  Column := ListView1.Columns[StrToIntDef(Edit2.Text, 0)];
  Column.Alignment := TAlignment(ComboBox1.ItemIndex);
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  ListView1.Scrollbars := TScrollStyle(ComboBox2.ItemIndex);
end;

procedure TForm1.ComboBox3Change(Sender: TObject);
begin
  ListView1.SortType := TSortType(ComboBox3.ItemIndex);
end;

procedure TForm1.ComboBox4Change(Sender: TObject);
begin
  ListView1.ViewStyle := TViewStyle(ComboBox4.ItemIndex);
end;

procedure TForm1.Edit10Change(Sender: TObject);
var
  Item: TListItem;
  n, idx: Integer;
  
begin
  idx := StrToIntDef(Edit1.Text, 0);
  if (idx < 0) or (idx > ListView1.Items.Count - 1) then
    exit;
  Item := ListView1.Items[idx];

  for n := Item.SubItems.Count to ListView1.Columns.Count - 2 do
    Item.Subitems.Add('');

  n := StrToIntDef(Edit5.Text, 0);
  if n >= ListView1.Columns.Count then Exit;
  idx := StrToIntDef(Edit10.Text, -1);
  if n = 0 then
    Item.ImageIndex := idx
  else
    Item.SubitemImages[n - 1] := idx;
end;

procedure TForm1.Edit11Change(Sender: TObject);
var
  i: Integer;
begin
  i := 0;
  if TryStrToInt(Edit11.Text, i) then
    ListView1.SortColumn := i;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  ShowItemData;
end;

procedure TForm1.Edit2Change(Sender: TObject);
begin
  ShowColumnData;
end;

procedure TForm1.Edit3Change(Sender: TObject);
var
  Column: TListColumn;
begin
  Column := ListView1.Columns[StrToIntDef(Edit2.Text, 0)];
  Column.Caption := Edit3.Text;
end;

procedure TForm1.Edit4Change(Sender: TObject);
var
  Item: TListItem;
  n: Integer;
begin
  n := StrToIntDef(Edit1.Text, 0);
  if (n < 0) or (n > ListView1.Items.Count - 1) then
    exit;
  Item := ListView1.Items[n];
  for n := Item.SubItems.Count to ListView1.Columns.Count - 2 do
    Item.Subitems.Add('');
  n := StrToIntDef(Edit5.Text, 0);
  if n >= ListView1.Columns.Count then Exit;
  if n = 0
  then Item.Caption := Edit4.Text
  else Item.Subitems[n - 1] := Edit4.Text;
end;

procedure TForm1.Edit5Change(Sender: TObject);
begin
  ShowItemData;
end;

procedure TForm1.Edit6Change(Sender: TObject);
var
  Column: TListColumn;
begin
  Column := ListView1.Columns[StrToIntDef(Edit2.Text, 0)];
  Column.Width := StrToIntDef(Edit6.Text, 0);
end;

procedure TForm1.Edit7Change (Sender: TObject );
var
  Column: TListColumn;
begin
  Column := ListView1.Columns[StrToIntDef(Edit2.Text, 0)];
  Column.MinWidth := StrToIntDef(Edit7.Text, 0);
end;

procedure TForm1.Edit8Change (Sender: TObject );
var
  Column: TListColumn;
begin
  Column := ListView1.Columns[StrToIntDef(Edit2.Text, 0)];
  Column.MaxWidth := StrToIntDef(Edit8.Text, 0);
end;

procedure TForm1.HideSelectionChange(Sender: TObject);
begin
  Listview1.HideSelection := HideSelection.Checked;
end;

procedure TForm1.ListView1ColumnClick(Sender: TObject; Column: TListColumn);
begin
  Edit2.Text := IntToStr(Column.Index);
  Edit5.Text := IntToStr(Column.Index);
end;

procedure TForm1.ListView1Data(Sender: TObject; Item: TListItem);
var
  i: Integer;
begin
  if (ListView1.ColumnCount > 0) then
  begin
    Item.Caption := 'OwnerData item '+IntToStr(Item.Index);
    Item.ImageIndex := -1;
    Item.StateIndex := -1;
    for i := 1 to ListView1.ColumnCount - 1 do
      Item.SubItems.Add('OwnerData subitem ['+IntToStr(Item.Index)+','+IntToStr(i)+']');

  end;
end;

procedure TForm1.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  subIdx: Integer;
begin
  if not Selected then Exit;
  Edit1.Text := IntToStr(Item.Index);
end;

procedure TForm1.OwnerDataCountChange(Sender: TObject);
begin
  if not ListView1.OwnerData then
    exit;
  ListView1.BeginUpdate;
  ListView1.Items.Count := TSpinEdit(Sender).Value;
  ListView1.EndUpdate;
end;

procedure TForm1.ShowItemData;
var
  Item: TListItem;
  idx: Integer;
begin
  idx := StrToIntDef(Edit1.Text, 0);
  if (idx < 0) or (idx > ListView1.Items.Count - 1) then
    exit;
  Item := ListView1.Items[idx];

  idx := StrToIntDef(Edit5.Text, 0);
  if idx = 0
  then begin
    Edit4.Text := Item.Caption;
    Edit10.Text := IntToStr(Item.ImageIndex);

    Exit;
  end;
  if Item.SubItems.Count <= idx - 1 then Exit;

  Edit4.Text := Item.SubItems[idx - 1];
  Edit10.Text := IntToStr(Item.SubItemImages[idx - 1]);
end;

procedure TForm1.ShowColumnData;
var
  Column: TListColumn;
begin
  Column := ListView1.Columns[StrToIntDef(Edit2.Text, 0)];

  Edit3.Text := Column.Caption;
  Edit6.Text := IntToStr(Column.Width);
  Edit7.Text := IntToStr(Column.MinWidth);
  Edit8.Text := IntToStr(Column.MaxWidth);
  Edit9.Text := IntToStr(Column.Imageindex);
  ComboBox1.ItemIndex := Ord(Column.Alignment);
  CheckBox1.Checked := Column.Visible;
  CheckBox2.Checked := Column.AutoSize;
end;

end.

