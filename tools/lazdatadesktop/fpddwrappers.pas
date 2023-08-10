unit fpddwrappers;
{
This unit adds features available in 3.3.1 to the 3.2.2 version of sqldb dictionary engines.
}

{$mode ObjFPC}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils,
  db, sqldb,
  fpdatadict,
  fpddsqldb,
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
  fpddmssql;


Type


  { TSQLDBMySql40DDEngine }


  TSQLDBMySql40DDEngine = Class(fpddmysql40.TSQLDBMySql40DDEngine)
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
  end;

  { TSQLDBMySql41DDEngine }

  TSQLDBMySql41DDEngine = Class(fpddmysql41.TSQLDBMySql41DDEngine)
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
  end;

  { TSQLDBMySql5DDEngine }

  TSQLDBMySql5DDEngine = Class(fpddmysql50.TSQLDBMySql5DDEngine)
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
  end;

  { TSQLDBMySql51DDEngine }

  TSQLDBMySql51DDEngine = Class(fpddmysql51.TSQLDBMySql51DDEngine)
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
  end;

  { TSQLDBMySql55DDEngine }

  TSQLDBMySql55DDEngine = Class(fpddmysql55.TSQLDBMySql55DDEngine)
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
  end;

  { TSQLDBMySql56DDEngine }

  TSQLDBMySql56DDEngine = Class(fpddmysql56.TSQLDBMySql56DDEngine)
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
  end;

  { TSQLDBMySql56DDEngine }

  { TSQLDBMySql57DDEngine }

  TSQLDBMySql57DDEngine = Class(fpddmysql57.TSQLDBMySql57DDEngine)
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
  end;

  { TSQLDBMySql80DDEngine }

  TSQLDBMySql80DDEngine = Class(fpddmysql80.TSQLDBMySql80DDEngine)
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
  end;

  { TSQLDBFBDDEngine }

  TSQLDBFBDDEngine = Class(fpddfb.TSQLDBFBDDEngine)
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
  end;

  { TSQLDBOracleDDEngine }

  TSQLDBOracleDDEngine = Class(fpddoracle.TSQLDBOracleDDEngine)
  Public
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
  end;

  { TSQLDBPostGreSQLDDEngine }

  TSQLDBPostGreSQLDDEngine = Class(fpddpq.TSQLDBPostGreSQLDDEngine)
  Public
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
  end;

  { TSQLDBSQLite3DDEngine }

  TSQLDBSQLite3DDEngine = Class(fpddsqlite3.TSQLDBSQLite3DDEngine)
  Public
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
  end;

  { TSQLDBODBCDDEngine }

  TSQLDBODBCDDEngine = Class(fpddodbc.TSQLDBODBCDDEngine)
  Public
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
  end;

  { TFPDDMSSQLEngine }

  TFPDDMSSQLEngine = Class(fpddmssql.TSQLDBMSSQLDDEngine)
  Public
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
  end;




Procedure RegisterMySQL40DDEngine;
Procedure RegisterMySQL41DDEngine;
Procedure RegisterMySQL50DDEngine;
Procedure RegisterMySQL51DDEngine;
Procedure RegisterMySQL55DDEngine;
Procedure RegisterMySQL56DDEngine;
Procedure RegisterMySQL57DDEngine;
Procedure RegisterMySQL80DDEngine;
Procedure RegisterFBDDEngine;
Procedure RegisterOracleDDEngine;
Procedure RegisterPostgreSQLDDengine;
Procedure RegisterSQLite3DDEngine;
Procedure RegisterODBCDDengine;
Procedure RegisterMSSQLDDEngine;

implementation

procedure RegisterMySQL40DDEngine;
begin
  RegisterDictionaryEngine(TSQLDBMySQL40DDEngine);
end;

procedure RegisterMySQL41DDEngine;
begin
  RegisterDictionaryEngine(TSQLDBMySQL41DDEngine);
end;

procedure RegisterMySQL50DDEngine;
begin
  RegisterDictionaryEngine(TSQLDBMySQL5DDEngine);

end;

procedure RegisterMySQL51DDEngine;
begin
  RegisterDictionaryEngine(TSQLDBMySQL51DDEngine);

end;

procedure RegisterMySQL55DDEngine;
begin
  RegisterDictionaryEngine(TSQLDBMySQL55DDEngine);

end;

procedure RegisterMySQL56DDEngine;
begin
  RegisterDictionaryEngine(TSQLDBMySQL56DDEngine);

end;

procedure RegisterMySQL57DDEngine;
begin
  RegisterDictionaryEngine(TSQLDBMySQL57DDEngine);
end;

procedure RegisterMySQL80DDEngine;
begin
  RegisterDictionaryEngine(TSQLDBMySQL80DDEngine);
end;

procedure RegisterFBDDEngine;
begin
  RegisterDictionaryEngine(TSQLDBFBDDEngine);
end;

procedure RegisterOracleDDEngine;
begin
  RegisterDictionaryEngine(TSQLDBOracleDDEngine);
end;

procedure RegisterPostgreSQLDDengine;
begin
  RegisterDictionaryEngine(TSQLDBPostGreSQLDDEngine);
end;

procedure RegisterSQLite3DDEngine;
begin
  RegisterDictionaryEngine(TSQLDBSQLite3DDEngine);
end;

procedure RegisterODBCDDengine;
begin
  RegisterDictionaryEngine(TSQLDBODBCDDEngine);
end;

procedure RegisterMSSQLDDEngine;
begin
  RegisterDictionaryEngine(TSQLDBMSSQLDDEngine);
end;


{ TSQLDBMySql40DDEngine }

class function TSQLDBMySql40DDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=inherited EngineCapabilities;
  Result:=Result+[ecRowsAffected]
end;


{ TSQLDBMySql41DDEngine }

class function TSQLDBMySql41DDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=inherited EngineCapabilities;
  Result:=Result+[ecRowsAffected]
end;

{ TSQLDBMySql5DDEngine }

class function TSQLDBMySql5DDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=inherited EngineCapabilities;
  Result:=Result+[ecRowsAffected]
end;

{ TSQLDBMySql51DDEngine }

class function TSQLDBMySql51DDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=inherited EngineCapabilities;
  Result:=Result+[ecRowsAffected]
end;

{ TSQLDBMySql55DDEngine }

class function TSQLDBMySql55DDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=inherited EngineCapabilities;
  Result:=Result+[ecRowsAffected]
end;

{ TSQLDBMySql56DDEngine }

class function TSQLDBMySql56DDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=inherited EngineCapabilities;
  Result:=Result+[ecRowsAffected]
end;

{ TSQLDBMySql57DDEngine }

class function TSQLDBMySql57DDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=inherited EngineCapabilities;
  Result:=Result+[ecRowsAffected]
end;

{ TSQLDBMySql80DDEngine }

class function TSQLDBMySql80DDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=inherited EngineCapabilities;
  Result:=Result+[ecRowsAffected]
end;

{ TSQLDBFBDDEngine }

class function TSQLDBFBDDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=inherited EngineCapabilities;
  Result:=Result+[ecRowsAffected,ecSequences];
end;

{ TSQLDBOracleDDEngine }

class function TSQLDBOracleDDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=inherited EngineCapabilities;
  Result:=Result+[ecRowsAffected,ecSequences];
end;

{ TSQLDBPostGreSQLDDEngine }

class function TSQLDBPostGreSQLDDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=inherited EngineCapabilities;
  Result:=Result+[ecRowsAffected,ecSequences];
end;

{ TSQLDBSQLite3DDEngine }

class function TSQLDBSQLite3DDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=inherited EngineCapabilities;
  Result:=Result+[ecRowsAffected];
end;

{ TSQLDBODBCDDEngine }

class function TSQLDBODBCDDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=inherited EngineCapabilities;
  Result:=Result+[ecRowsAffected];
end;

{ TFPDDMSSQLEngine }

class function TFPDDMSSQLEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=inherited EngineCapabilities;
  Result:=Result+[ecRowsAffected,ecSequences];
end;


end.

