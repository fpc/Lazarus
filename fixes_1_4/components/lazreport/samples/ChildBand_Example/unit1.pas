unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LR_Class, LR_DBSet, Forms, Controls, Graphics,
  Dialogs, StdCtrls, BufDataset, db;

type

  { TForm1 }

  TForm1 = class(TForm)
    BufDataset1: TBufDataset;
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    frDBDataSet1: TfrDBDataSet;
    frReport1: TfrReport;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure frReport1BeginBand(Band: TfrBand);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i:Integer;

begin
  BufDataset1.CreateDataset;
  for i:=1 to 10 do
   begin
     BufDataset1.AppendRecord([i]);
   end;
end;

procedure TForm1.frReport1BeginBand(Band: TfrBand);
begin
  if CheckBox1.Checked and (Band.Name='Child1') then
   begin
     Band.Visible:=((BufDataset1.FieldByName('FIELD1').Value mod 2)=0);
     Band.PrintChildIfNotVisible:=CheckBox2.Checked;
   end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  frReport1.ShowReport;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  CheckBox2.Visible:=CheckBox1.Checked;
end;

end.

