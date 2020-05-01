unit reglddfeatures;

{
  Adding support for new connection types requires implementing a Data Dictionary for your connection type
  see fcl-db/src/datadict for many implementations.
  When done so, add the unit to the uses clause in the implementation, register it in RegisterDDEngines
  and likely add a connection callback to RegisterConnectionCallBacks.
}

{ MS-SQL server connection}
{$IF FPC_FULLVERSION>30001}
{$DEFINE HAVEMSSQLCONN}
{$ENDIF}

{ MySQL 5.6 and 5.7 connections }
{$IF FPC_FULLVERSION>30100}
{$DEFINE HAVEMYSQL5657CONN}
{$ENDIF}
{$IFDEF VER3_0_4}
{$DEFINE HAVEMYSQL5657CONN}
{$ENDIF}


{$mode objfpc}{$H+}

interface

uses
  // Data dictionary support for database types
  fpdddbf,     // DBF
  {$ifndef win64}
  fpddfb,      // Firebird
  fpddmysql40, // MySQL 4.0
  fpddmysql41, // MySQL 4.1
  fpddmysql50, // MySQL 5.0
  fpddmysql51, // MySQL 5.1
  fpddmysql55, // MySQL 5.5
  {$ifdef HAVEMYSQL5657CONN}
  fpddmysql56, // MySQL 5.6
  fpddmysql57, // MySQL 5.7
  {$endif HAVEMYSQL5657CONN}
  fpddoracle,  // Oracle
  fpddpq,      // PostgreSQL
  {$endif}
  fpddsqlite3, // SQLite 3
  fpddodbc,    // Any ODBC supported
  {$ifdef HAVEMSSQLCONN}
  fpddmssql,
  {$endif HAVEMSSQLCONN}
  // code generators
{$IF FPC_FULLVERSION>=30200}
  fpcgfieldmap,
  fpcgtypesafedataset,
{$ENDIF}
  fpcgSQLConst,
  fpcgdbcoll,
  fpcgCreateDBF,
  fpcgtiOPF,
  // data Export
  fpstdExports,
  fpxmlxsdexport,
  fpdbexport
  ;

procedure registerengines;
procedure RegisterExportFormats;

implementation

procedure RegisterExportFormats;

begin
  RegisterXMLXSDExportFormat;
  RegisterStdFormats;
end;

procedure registerengines;

begin
{$ifndef win64}
  RegisterFBDDEngine;
  RegisterMySQL40DDEngine;
  RegisterMySQL41DDEngine;
  RegisterMySQL50DDEngine;
  RegisterMySQL51DDEngine;
  RegisterMySQL55DDEngine;
{$ifdef HAVEMYSQL5657CONN}
  RegisterMySQL56DDEngine;
  RegisterMySQL57DDEngine;
{$endif}
  RegisterOracleDDEngine;
  RegisterPostgreSQLDDengine;
{$endif}
  RegisterSQLite3DDEngine;
  RegisterODBCDDengine;
{$IFDEF HAVEMSSQLCONN}
  RegisterMSSQLDDEngine;
{$ENDIF}
end;


end.

