{  $Id$  }
{
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

  Author: Joost van der Sluis
  
  This unit registers the sqldb components of the FCL.
}
unit registersqldb;

{$mode objfpc}{$H+}
{$IFNDEF win64}
{$DEFINE HASMYSQL4CONNECTION}
{$DEFINE HASORACLECONNECTION}
{$DEFINE HASPQCONNECTION}
{$ENDIF}

{ SQLITE }
{$IFNDEF VER2_2}
{$IFNDEF VER2_0}
{$IFNDEF VER2_1}
{$DEFINE HASSQLITE3CONNECTION}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{ SQLSCRIPT }
{$IFNDEF VER2_2}
{$IFNDEF VER2_0}
{$IFNDEF VER2_1}
{$DEFINE HASSQLSCRIPT}
{$ENDIF}
{$ENDIF}
{$ENDIF}
interface

uses
  Classes, SysUtils, LResources, sqldb, ibconnection, odbcconn,
{$IFDEF HASPQCONNECTION}
  pqconnection,
{$ENDIF}
{$IFDEF HASORACLECONNECTION}
  oracleconnection,
{$ENDIF}
{$IFDEF HASMYSQL4CONNECTION}
  mysql40conn, mysql41conn,
{$ENDIF}
{$IFDEF HASSQLITE3CONNECTION}
  sqlite3conn,
{$ENDIF}
  mysql50conn,
  LazarusPackageIntf;

procedure Register;

implementation

procedure RegisterUnitSQLdb;
begin
  RegisterComponents('SQLdb',[TSQLQuery,
                              TSQLTransaction,
{$IFDEF HASSQLSCRIPT}
                              TSQLScript,
                              TSQLConnector,
{$ENDIF}
{$IFDEF HASPQCONNECTION}
                              TPQConnection,
{$ENDIF}
{$IFDEF HASORACLECONNECTION}
                              TOracleConnection,
{$ENDIF}
                              TODBCConnection,
{$IFDEF HASMYSQL4CONNECTION}
                              TMySQL40Connection,
                              TMySQL41Connection,
{$ENDIF}
{$IFDEF HASSQLITE3CONNECTION}
                              TSQLite3Connection,
{$ENDIF}
                              TMySQL50Connection,
                              TIBConnection]);
end;

procedure Register;
begin
  RegisterUnit('sqldb',@RegisterUnitSQLdb);
end;

initialization
 {$i registersqldb.lrs}

end.
