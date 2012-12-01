unit sqliteform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls;

type

  { TformSqlite }

  TformSqlite = class(TForm)
    Button1: TButton;
    SqliteDatasource: TDatasource;
    DBEdit1: TDBEdit;
    DBNavigator1: TDBNavigator;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  formSqlite: TformSqlite;

implementation

{$ifdef CPUARM}
uses sqlitejniandroid;
{$endif}

{$R *.lfm}

{ TformSqlite }

procedure TformSqlite.Button1Click(Sender: TObject);
{$ifdef CPUARM}
var
  sqlitedb: TSqliteJNIDataset;
{$endif}
begin
  {$ifdef CPUARM}
  sqlitedb := TSqliteJNIDataset.Create(Self);
  //SqliteDatasource.DataSet := sqlitedb;
  sqlitedb.FileName := '/sdcard/database.db';
  sqlitedb.TableName := 'TestTable';
  sqlitedb.FieldDefs.Add('FirstFieldStr', ftString);
  sqlitedb.FieldDefs.Add('SecondFieldInt', ftInteger);
  sqlitedb.Open();
  {$endif}
end;

end.

