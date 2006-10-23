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
{$DEFINE HASODBCCONNECTION}
{$IFNDEF ver2_0_2}{$IFNDEF ver2_0_3}
  {$DEFINE HASMYSQL50CONNECTION}
  {$DEFINE HASORACLECONNECTION}
{$ENDIF}{$ENDIF}

interface

uses
  Classes, SysUtils, LResources, sqldb, ibconnection, pqconnection,
  oracleconnection, odbcconn,
{$IFDEF HASMYSQL50CONNECTION}
  mysql40conn, mysql41conn, mysql50conn,
{$ELSE}
  mysql4conn,
{$ENDIF}
  LazarusPackageIntf;

procedure Register;

implementation

procedure RegisterUnitSQLdb;
begin
  RegisterComponents('SQLdb',[TSQLQuery,
                              TSQLTransaction,
                              TIBConnection,
                              TODBCConnection,
{$IFDEF HASORACLECONNECTION}
                              TOracleConnection,
{$ENDIF}
{$IFDEF HASMYSQL50CONNECTION}
                              TMySQL40Connection,
                              TMySQL41Connection,
                              TMySQL50Connection,
{$ELSE}
                              TMySQLConnection,
{$ENDIF}
                              TPQConnection]);
end;

procedure Register;
begin
  RegisterUnit('sqldb',@RegisterUnitSQLdb);
end;

initialization
 {$i registersqldb.lrs}

end.
