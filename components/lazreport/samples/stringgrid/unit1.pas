unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, LR_DSet, LR_Class;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    frReport1: TfrReport;
    Filas: TfrUserDataset;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FilasCheckEOF(Sender: TObject; var Eof: Boolean);
    procedure FilasFirst(Sender: TObject);
    procedure FilasNext(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure frReport1GetValue(const ParName: String; var ParValue: Variant);
  private
    fila : longint;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  Edit1.Text := '';
  Edit2.Text := '';
  Edit3.Text := '';
  Edit4.Text := '';
  Edit5.Text := FormatFloat('###0.00', 0.0);
  
  StringGrid1.RowCount := 1;
  
  frReport1.LoadFromFile('sample.lrf');
  
  Edit1.SetFocus;
end;

procedure TForm1.frReport1GetValue(const ParName: String; var ParValue: Variant
  );
begin
  if ParName = 'cliente' then  // customer
    ParValue := Edit1.Text;
    
  if ParName = 'direccion' then  // address
    ParValue := Edit2.Text;
    
  if ParName = 'nombre' then // product
    ParValue := StringGrid1.Cells[0, fila];
    
  if ParName = 'precio' then // price
    ParValue := StringGrid1.Cells[1, fila];
    
  if ParName = 'total' then
    ParValue := Edit5.Text;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  StringGrid1.RowCount := StringGrid1.RowCount + 1;
  StringGrid1.Cells[0, StringGrid1.RowCount - 1] := Edit3.Text;
  StringGrid1.Cells[1, StringGrid1.RowCount - 1] := FormatFloat('$ ###0.00', StrToFloat(Edit4.Text));

  Edit5.Text := FormatFloat('###0.00', StrToFloat(Edit5.Text) + StrToFloat(Edit4.Text));
  
  Edit3.Text := '';
  Edit4.Text := '';
  
  Edit3.SetFocus;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  frReport1.DesignReport;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  frReport1.ShowReport
end;

procedure TForm1.FilasCheckEOF(Sender: TObject; var Eof: Boolean);
begin
  Eof := fila > StringGrid1.RowCount - 1;
end;

procedure TForm1.FilasFirst(Sender: TObject);
begin
  fila := 1; {fila por la cual empieza. En este caso la 1 porque la 0 es para los títulos}
             {starting row. In this case 1 because row 0 it's for title}
end;

procedure TForm1.FilasNext(Sender: TObject);
begin
  Inc(fila);
end;

initialization
  {$I unit1.lrs}

end.

