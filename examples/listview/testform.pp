unit testform;

{$mode objfpc} {$H+} 

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, StdCtrls, ExtCtrls;

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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure CheckBox1Click (Sender: TObject );
    procedure CheckBox2Click (Sender: TObject );
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
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
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
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
  Item := ListView1.Items.Insert(StrToIntDef(Edit1.Text, 0));
  Item.Caption := Format('Item %d', [ListView1.Items.Count]);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Item: TListItem;
begin
  Item := ListView1.Items[StrToIntDef(Edit1.Text, 0)];
  Item.Free;
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
begin
  Item := ListView1.Items[StrToIntDef(Edit1.Text, 0)];
  Item.Selected := True;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  Item: TListItem;
begin
  Item := ListView1.Items[StrToIntDef(Edit1.Text, 0)];
  Item.Focused := True;
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
  Item := ListView1.Items[StrToIntDef(Edit1.Text, 0)];

  for n := Item.SubItems.Count to ListView1.Columns.Count - 2 do
    Item.Subitems.Add('');

  n := StrToIntDef(Edit5.Text, 0);
  if n >= ListView1.Columns.Count then Exit;
  idx := StrToIntDef(Edit10.Text, -1);
  if n = 0
  then Item.ImageIndex := idx
  else Item.SubitemImages[n - 1] := idx;

end;

procedure TForm1.Edit11Change(Sender: TObject);
begin
  ListView1.SortColumn := StrToIntDef(Edit11.Text, 0);
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
  Item := ListView1.Items[StrToIntDef(Edit1.Text, 0)];
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

procedure TForm1.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  subIdx: Integer;
begin
  if not Selected then Exit;
  Edit1.Text := IntToStr(Item.Index);
end;

procedure TForm1.ShowItemData;
var
  Item: TListItem;
  idx: Integer;
begin
  Item := ListView1.Items[StrToIntDef(Edit1.Text, 0)];

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

