unit Unit1;

{$MODE ObjFpc}
{$H+}

interface

uses
  LCLIntf, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DBCtrls, ExtCtrls, DBGrids, DB, dbf, FileUtil, LResources;

type

  { TForm1 }

  TForm1 = class(TForm)
    DBEdit2: TDBEdit;
    Dbf1ADATE: TDateField;
    Dbf1AINT: TLargeintField;
    Dbf1ASTR: TStringField;
    Label1: TLabel;
    ShowLongDateCheckBox: TCheckBox;
    DataSource1: TDataSource;
    ClientDataSet1ADate: TDateField;
    ClientDataSet1AStr: TStringField;
    ClientDataSet1AInt: TLargeintField;
    Dbf1: TDbf;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DBEdit1: TDBEdit;
    Label3: TLabel;
    procedure Dbf1ADATESetText(Sender: TField; const aText: string);
    procedure Dbf1AINTGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ShowLongDateCheckBoxChange(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  strutils;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ShortDateFormat := 'd/M/yyyy';
  if not FileExistsUTF8(Dbf1.TableName) then
  begin
    Dbf1.FieldDefs.Clear;
    Dbf1.FieldDefs.Add('ADate', ftDate);
    Dbf1.FieldDefs.Add('AStr', ftString, 50);
    Dbf1.FieldDefs.Add('AInt', ftLargeint);
    Dbf1.CreateTable;
    //add some data
    Dbf1.Open;
    Dbf1.Append;
    Dbf1.FieldByName('ADate').AsString := '12/09/2003';
    Dbf1.Post;
    Dbf1.Append;
    Dbf1.FieldByName('AInt').AsInteger := 1;
    Dbf1.Post;
    Dbf1.Append;
    Dbf1.FieldByName('ADate').AsString := '12/12/1090';
    Dbf1.FieldByName('AInt').AsInteger := 30;
    Dbf1.Post;
  end
  else
    Dbf1.Open;
end;

procedure TForm1.Dbf1AINTGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if DisplayText then
  begin
    if Sender.IsNull then
      aText := '(Undefined)'
    else if Sender.AsInteger = 0 then
      aText := 'No Item'
    else if Sender.AsInteger = 1 then
      aText := 'Only One Item'
    else if Sender.AsInteger < 10 then
      aText := 'Few Itens'
    else
      aText := 'Many Itens';
  end
  else
    aText := Sender.AsString;
end;

procedure TForm1.Dbf1ADATESetText(Sender: TField; const aText: string);
var
  FixedStr: String;
begin
  //workaround to fpc bug 15039
  FixedStr := AnsiReplaceStr(aText, ' ', '');
  Sender.AsString := FixedStr;
end;

procedure TForm1.ShowLongDateCheckBoxChange(Sender: TObject);
var
  DateField: TDateTimeField;
begin
  DateField := Dbf1.FieldByName('ADate') as TDateTimeField;
  if ShowLongDateCheckBox.Checked then
    DateField.DisplayFormat := LongDateFormat
  else
    DateField.DisplayFormat := '';
end;

initialization
  {$i unit1.lrs}

end.
