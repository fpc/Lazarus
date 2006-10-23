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
{$IFNDEF ver2_0_0}{$IFNDEF ver2_0_1}
  {$DEFINE HASODBCCONNECTION}
{$ENDIF}{$ENDIF}
{$IFNDEF ver2_0_0}{$IFNDEF ver2_0_1}{$IFNDEF ver2_0_2}{$IFNDEF ver2_0_3}
  {$DEFINE HASMYSQL50CONNECTION}
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}

interface

uses
  Classes, SysUtils, LResources, sqldb, ibconnection, pqconnection,
{$IFDEF HASMYSQL50CONNECTION}
  mysql40conn, mysql41conn, mysql50conn,
{$ELSE}
  mysql4conn,
{$ENDIF}
{$IFDEF HASODBCCONNECTION}
  odbcconn,
{$ENDIF}
  LazarusPackageIntf;

procedure Register;

implementation

procedure RegisterUnitSQLdb;
begin
  RegisterComponents('SQLdb',[TSQLQuery,
                              TSQLTransaction,
                              TIBConnection,
{$IFDEF HASODBCCONNECTION}
                              TODBCConnection,
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
