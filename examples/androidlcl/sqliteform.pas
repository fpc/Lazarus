unit sqliteform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls;

type

  { TformSqlite }

  TformSqlite = class(TForm)
    btnConnect: TButton;
    btnCreateDB: TButton;
    Edit1: TEdit;
    SqliteDatasource: TDatasource;
    DBEdit1: TDBEdit;
    DBNavigator1: TDBNavigator;
    procedure btnCreateDBClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
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

procedure TformSqlite.btnConnectClick(Sender: TObject);
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
  sqlitedb.SaveOnClose := True;
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

