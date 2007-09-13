unit cbunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, dbf, db,
  DBGrids, LR_DBSet, LR_Class, LR_BarC, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Datasource1: TDatasource;
    Dbf1: TDbf;
    DBGrid1: TDBGrid;
    frBarCodeObject1: TfrBarCodeObject;
    frDBDataSet1: TfrDBDataSet;
    frReport1: TfrReport;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  OpenDialog1.Execute;
  frReport1.FileName := OpenDialog1.FileName;
  frReport1.LoadFromFile(OpenDialog1.Filename);
  frReport1.DesignReport;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  OpenDialog1.Execute;
  frReport1.FileName := OpenDialog1.FileName;
  frReport1.LoadFromFile(OpenDialog1.Filename);
  frReport1.ShowReport;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName)+'reports';
  Dbf1.TableName := SetDirSeparators('..\editor\db\disco.dbf');
  Dbf1.Open;
end;

initialization
  {$I cbunit.lrs}

end.

