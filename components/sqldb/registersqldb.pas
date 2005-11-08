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

interface

uses
  Classes, SysUtils, LResources, sqldb, ibconnection, pqconnection, mysql4conn,
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

			      TPQConnection,
                              TMySQLConnection]);
end;

procedure Register;
begin
  RegisterUnit('sqldb',@RegisterUnitSQLdb);
end;

initialization
 {$i registersqldb.lrs}

end.
