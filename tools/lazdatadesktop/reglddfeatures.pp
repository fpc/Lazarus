unit reglddfeatures;

{
  Adding support for new connection types requires implementing a Data Dictionary for your connection type
  see fcl-db/src/datadict for many implementations.
  When done so, add the unit to the uses clause in the implementation, register it in RegisterDDEngines
  and likely add a connection callback to RegisterConnectionCallBacks.
}

{$mode objfpc}{$H+}

interface

uses
  fpdddbf,     // DBF
{$IFDEF VER3_3}
  // Data dictionary support for database types
  fpddfb,      // Firebird
  fpddmysql40, // MySQL 4.0
  fpddmysql41, // MySQL 4.1
  fpddmysql50, // MySQL 5.0
  fpddmysql51, // MySQL 5.1
  fpddmysql55, // MySQL 5.5
  fpddmysql56, // MySQL 5.6
  fpddmysql57, // MySQL 5.7
  fpddmysql80, // MySQL 8.0
  fpddoracle,  // Oracle
  fpddpq,      // PostgreSQL
  fpddsqlite3, // SQLite 3
  fpddodbc,    // Any ODBC supported
  fpddmssql,
{$ELSE}
  // Descendents for all classes
  fpddWrappers,
{$ENDIF}
  // code generators
  fpcgfieldmap,
  fpcgtypesafedataset,
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
  RegisterFBDDEngine;
  RegisterMySQL40DDEngine;
  RegisterMySQL41DDEngine;
  RegisterMySQL50DDEngine;
  RegisterMySQL51DDEngine;
  RegisterMySQL55DDEngine;
  RegisterMySQL56DDEngine;
  RegisterMySQL57DDEngine;
  RegisterMySQL80DDEngine;
  RegisterOracleDDEngine;
  RegisterPostgreSQLDDengine;
  RegisterSQLite3DDEngine;
  RegisterODBCDDengine;
  RegisterMSSQLDDEngine;
end;


end.

