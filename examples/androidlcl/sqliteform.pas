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
    btnCreateDB: TButton;
    SqliteDatasource: TDatasource;
    DBEdit1: TDBEdit;
    DBNavigator1: TDBNavigator;
    procedure btnCreateDBClick(Sender: TObject);
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
{$else}
uses sqlite3ds;
{$endif}

{$R *.lfm}

{ TformSqlite }

procedure TformSqlite.Button1Click(Sender: TObject);
var
  sqlitedb: {$ifdef CPUARM}TSqliteJNIDataset;{$else}TSqlite3Dataset;{$endif}
begin
  {$ifdef CPUARM}
  sqlitedb := TSqliteJNIDataset.Create(Self);
  sqlitedb.FileName := '/sdcard/database.db';
  {$else}
  sqlitedb := TSqlite3Dataset.Create(Self);
  sqlitedb.FileName := 'database.db';
  {$endif}
  sqlitedb.TableName := 'TestTable';
  sqlitedb.FieldDefs.Add('FirstFieldStr', ftString);
  sqlitedb.FieldDefs.Add('SecondFieldInt', ftInteger);
  sqlitedb.Open();
  SqliteDatasource.DataSet := sqlitedb;
end;

procedure TformSqlite.btnCreateDBClick(Sender: TObject);
var
  sqlitedb: {$ifdef CPUARM}TSqliteJNIDataset;{$else}TSqlite3Dataset;{$endif}
begin
  {$ifdef CPUARM}
  sqlitedb := TSqliteJNIDataset.Create(Self);
  sqlitedb.FileName := '/sdcard/database.db';
  {$else}
  sqlitedb := TSqlite3Dataset.Create(Self);
  sqlitedb.FileName := 'database.db';
  {$endif}
  //SqliteDatasource.DataSet := sqlitedb;
  sqlitedb.TableName := 'TestTable';
  sqlitedb.FieldDefs.Add('FirstFieldStr', ftString);
  sqlitedb.FieldDefs.Add('SecondFieldInt', ftInteger);
  sqlitedb.CreateTable();
  sqlitedb.Free;
end;

end.

