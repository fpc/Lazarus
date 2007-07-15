{
 Copyright (c) 2007 by Michael Van Canneyt.

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit fpddsqldb;

{$mode objfpc}{$H+}

{$DEFINE DDFirebird}
{$DEFINE DDMYSQL}
{$DEFINE DDPOSTGRESQL}
{$DEFINE DDODBC}
// SQLite3 only for 2.3
{$IFNDEF VER2_0}
{$IFNDEF VER2_1}
{$IFNDEF VER2_2}
{$DEFINE DDSQLITE}
{$ENDIF}
{$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, sqldb, fpdatadict;

Type

  { TSQLDBDDEngine }

  TSQLDBDDEngine = Class(TFPDDEngine)
  Private
    FConn: TSQLConnection;
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; virtual; abstract;
    Function CreateSQLQuery(ADatasetOwner: TComponent) : TSQLQuery;
    Property Connection : TSQLConnection Read FConn;
  Public
    Procedure Disconnect ; override;
    Function HostSupported: Boolean; virtual;
    Function Connect(const AConnectString : String) : Boolean; override;
    Function GetTableList(List : TStrings) : Integer; override;
    Function ImportFields(Table : TDDTableDef) : Integer; override;
    Function ViewTable(Const TableName: String; DatasetOwner : TComponent) : TDataset; override;
    Function RunQuery(SQL : String) : Integer; override;
    Function CreateQuery(SQL : String; DatasetOwner : TComponent) : TDataset; override;
    Procedure SetQueryStatement(SQL : String; AQuery : TDataset); override;
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
  end;

{$ifdef DDFIREBIRD}
  { TSQLDBIBDDEngine }

  TSQLDBIBDDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function Description : string; override;
    Class function DBType : String; override;
  end;
{$ENDIF DDFIREBIRD}

{$IFDEF DDMYSQL}
  { TSQLDBMySql4DDEngine }

  TSQLDBMySql4DDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function Description : string; override;
    Class function DBType : String; override;
  end;
{$ENDIF DDMYSQL}

{$IFDEF DDPOSTGRESQL}
  { TSQLDBPostGreSQLDDEngine }

  TSQLDBPostGreSQLDDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function Description : string; override;
    Class function DBType : String; override;
  end;
{$ENDIF DDPOSTGRESQL}

{$IFDEF DDODBC}
  { TSQLDBODBCDDEngine }

  TSQLDBODBCDDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function Description : string; override;
    Class function DBType : String; override;
  end;
{$ENDIF DDODBC}

{$IFDEF DDSQLITE}
  { TSQLDBSQLite3DDEngine }

  TSQLDBSQLite3DDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function Description : string; override;
    Class function DBType : String; override;
  end;
{$ENDIF DDSQLITE}

Procedure InitSQLDBImporter;
Procedure DoneSQLDBImporter;

Const
  // used in connectionstring
  KeyHostName     = 'Host';
  KeyDatabaseName = 'Database';
  KeyUserName     = 'User';
  KeyPassword     = 'Password';
  KeyEncode       = 'Trivial';

implementation


uses
{$IFDEF DDFIREBIRD}
  ibconnection,
{$ENDIF DDFIREBIRD}
{$IFDEF DDMYSQL}
  mysql4conn,
{$ENDIF DDMYSQL}
{$IFDEF DDPOSTGRESQL}
  pqconnection,
{$ENDIF}
{$IFDEF DDODBC}
  odbcconn,
{$ENDIF}
{$IFDEF DDSQLITE}
  sqlite3conn,
{$ENDIF}
  strutils;

Resourcestring
  SErrQueryNotSQLQuery = 'Query object "%s" is not a SQL Query';
  
{ TSQLDBDDEngine }

function TSQLDBDDEngine.HostSupported: Boolean;
begin
  Result:=True;
end;

function TSQLDBDDEngine.CreateSQLQuery(ADatasetOwner: TComponent): TSQLQuery;
begin
  Result:=TSQLQuery.Create(ADatasetOwner);
  Result.DataBase:=FConn;
  Result.Transaction:=FConn.TRansaction;
end;

procedure TSQLDBDDEngine.Disconnect;
begin
  FreeAndNil(FConn);
  FConnectString:='';
  FConnected:=False;
end;

function TSQLDBDDEngine.Connect(const AConnectString: String): Boolean;

Var
  L : TStringList;
  
begin
  FConn:=CreateConnection(AConnectString);
  FConn.Transaction:=TSQLTransaction.Create(FConn);
  L:=TStringList.Create;
  Try
    L.CommaText:=AConnectString;
    If HostSupported then
      FConn.HostName:=L.Values[KeyHostName];
    FConn.DatabaseName:=L.Values[KeyDatabaseName];
    FConn.UserName:=L.Values[KeyUserName];
    FConn.Password:=XorDecode(KeyEncode,L.Values[KeyPassword]);
    FConn.LoginPrompt:=False;
    FConn.Connected:=True;
    FConnected:=True;
    FConnectString:=AConnectString;
    Result:=True;
  Finally
    L.Free;
  end;
end;

function TSQLDBDDEngine.GetTableList(List: TStrings): Integer;
begin
  FConn.GetTableNames(List,False);
end;

function TSQLDBDDEngine.ImportFields(Table: TDDTableDef): Integer;

Const
  SQL = 'SELECT * from %s where (1=0)';

Var
  Q : TSQLQuery;
  
begin
  Q:=CreateSQLQuery(Nil);
  try
    Q.Sql.Text:=Format(SQL,[Table.TableName]);
    Q.Open;
    try
      Result:=Table.ImportFromDataset(Q);
    finally
      Q.CLose;
    end;
  finally
    Q.Free;
  end;
end;

function TSQLDBDDEngine.ViewTable(const TableName: String;
  DatasetOwner: TComponent): TDataset;
  
Var
  Q : TSQLQuery;
  
begin
  Q:=CreateSQLQuery(DatasetOwner);
  Q.SQL.Text:='SELECT * FROM '+TableName;
  Result:=Q;
end;

function TSQLDBDDEngine.RunQuery(SQL: String): Integer;

Var
  Q : TSQLQuery;

begin
  Q:=CreateSQLQuery(Nil);
  Try
    Q.SQL.Text:=SQL;
    Q.ExecSQL;
    Result:=0;
  Finally
    Q.Free;
  end;
end;

function TSQLDBDDEngine.CreateQuery(SQL: String; DatasetOwner: TComponent
  ): TDataset;

Var
  Q : TSQLQuery;

begin
  Q:=CreateSQLQuery(Nil);
  Result:=Q;
  Q.SQL.Text:=SQL;
  Q.Open;
end;

procedure TSQLDBDDEngine.SetQueryStatement(SQL: String; AQuery: TDataset);
begin
  If Not (AQuery is TSQLQuery) then
    Raise EDataDict.CreateFmt(SErrQueryNotSQLQuery,[AQuery.ClassName]);
  (AQuery as TSQLQuery).SQL.Text:=SQL;
end;

class function TSQLDBDDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=[ecimport,ecViewTable, ecRunQuery];
end;

{$IFDEF DDFIREBIRD}
{ TSQLDBIBDDEngine }

function TSQLDBIBDDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:=TIBConnection.Create(Self);
end;

class function TSQLDBIBDDEngine.Description: string;
begin
  Result:='Interbase connection using SQLDB';
end;

class function TSQLDBIBDDEngine.DBType: String;
begin
  Result:='Firebird/Interbase';
end;
{$ENDIF DDFIREBIRD}

{$IFDEF DDMYSQL}
{ TSQLDBMySql4DDEngine }

function TSQLDBMySql4DDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:=mysql4conn.TMySQLConnection.Create(Self);
end;

class function TSQLDBMySql4DDEngine.Description: string;
begin
  Result:='Mysql connection using SQLDB';
end;

class function TSQLDBMySql4DDEngine.DBType: String;
begin
  Result:='MySQL 4.0';
end;
{$ENDIF DDMYSQL}

{$IFDEF DDPOSTGRESQL}

{ TSQLDBPostGreSQLDDEngine }

function TSQLDBPostGreSQLDDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:=TPQConnection.Create(Self);
end;

class function TSQLDBPostGreSQLDDEngine.Description: string;
begin
  Result:='PostGreSQL using SQLDB';
end;

class function TSQLDBPostGreSQLDDEngine.DBType: String;
begin
  Result:='PostGreSQL';
end;
{$ENDIF DDPOSTGRESQL}

{$IFDEF DDODBC}

{ TSQLDBODBCDDEngine }

function TSQLDBODBCDDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:=TODBCConnection.Create(Self);
end;

class function TSQLDBODBCDDEngine.Description: string;
begin
  Result:='ODBC connection using SQLDB';
end;

class function TSQLDBODBCDDEngine.DBType: String;
begin
  Result:='ODBC';
end;
{$ENDIF DDODBC}


{ ---------------------------------------------------------------------
  Registration/Unregistration
  ---------------------------------------------------------------------}


procedure InitSQLDBImporter;
begin
{$IFDEF DDFIREBIRD}
  RegisterDictionaryEngine(TSQLDBIBDDEngine);
{$ENDIF DDFIREBIRD}
{$IFDEF DDMYSQL}
  RegisterDictionaryEngine(TSQLDBMySQL4DDEngine);
{$ENDIF DDMYSQL}
{$IFDEF DDPOSTGRESQL}
  RegisterDictionaryEngine(TSQLDBPostGreSQLDDEngine);
{$ENDIF DDPOSTGRESQL}
{$IFDEF DDODBC}
  RegisterDictionaryEngine(TSQLDBODBCDDEngine);
{$ENDIF DDODBC}
{$IFDEF DDSQLITE}
  RegisterDictionaryEngine(TSQLDBSQLITE3DDEngine);
{$ENDIF DDSQLITE}
end;

procedure DoneSQLDBImporter;
begin
{$IFDEF DDFIREBIRD}
  UnRegisterDictionaryEngine(TSQLDBIBDDEngine);
{$ENDIF DDFIREBIRD}
{$IFDEF DDMYSQL}
  UnRegisterDictionaryEngine(TSQLDBMySQL4DDEngine);
{$ENDIF DDMYSQL}
{$IFDEF DDPOSTGRESQL}
  UnRegisterDictionaryEngine(TSQLDBPostGreSQLDDEngine);
{$ENDIF DDPOSTGRESQL}
{$IFDEF DDODBC}
  UnRegisterDictionaryEngine(TSQLDBODBCDDEngine);
{$ENDIF DDODBC}
{$IFDEF DDSQLITE}
  UnRegisterDictionaryEngine(TSQLDBSQLITE3DDEngine);
{$ENDIF DDSQLITE}
end;

{$IFDEF DDSQLITE}
{ TSQLDBSQLite3DDEngine }

function TSQLDBSQLite3DDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:=TSQLITE3Connection.Create(Self);
end;

class function TSQLDBSQLite3DDEngine.Description: string;
begin
  Result:='SQLite 3 database using SQLDB';
end;

class function TSQLDBSQLite3DDEngine.DBType: String;
begin
  Result:='SQLITE3';
end;
{$ENDIF DDSQLITE}

Initialization
  InitSQLDBImporter
Finalization
  DoneSQLDBImporter
end.

