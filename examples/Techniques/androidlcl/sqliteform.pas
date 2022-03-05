unit sqliteform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls
  {$ifdef CPUARM}
    ,sqlitejniandroid
  {$else}
    ,sqlite3ds
  {$endif}
  ;

type

  { TformSqlite }

  TformSqlite = class(TForm)
    btnConnect: TButton;
    btnSaveDB: TButton;
    btnCreateDB: TButton;
    Edit1: TEdit;
    SqliteDatasource: TDatasource;
    DBEdit1: TDBEdit;
    DBNavigator1: TDBNavigator;
    procedure btnCreateDBClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnSaveDBClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    sqlitedb: {$ifdef CPUARM}TSqliteJNIDataset;{$else}TSqlite3Dataset;{$endif}
  end;

var
  formSqlite: TformSqlite;

implementation

{$R *.lfm}

{ TformSqlite }

procedure TformSqlite.btnConnectClick(Sender: TObject);
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

procedure TformSqlite.btnSaveDBClick(Sender: TObject);
begin
  if sqlitedb = nil then Exit;
  sqlitedb.Close();
  sqlitedb.Open();
end;

procedure TformSqlite.btnCreateDBClick(Sender: TObject);
var
  lsqlitedb: {$ifdef CPUARM}TSqliteJNIDataset;{$else}TSqlite3Dataset;{$endif}
begin
  {$ifdef CPUARM}
  lsqlitedb := TSqliteJNIDataset.Create(Self);
  lsqlitedb.FileName := '/sdcard/database.db';
  {$else}
  lsqlitedb := TSqlite3Dataset.Create(Self);
  lsqlitedb.FileName := 'database.db';
  {$endif}
  //SqliteDatasource.DataSet := sqlitedb;
  lsqlitedb.TableName := 'TestTable';
  lsqlitedb.FieldDefs.Add('FirstFieldStr', ftString);
  lsqlitedb.FieldDefs.Add('SecondFieldInt', ftInteger);
  lsqlitedb.CreateTable();
  lsqlitedb.Free;
end;

end.

