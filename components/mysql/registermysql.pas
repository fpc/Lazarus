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

  Author: Mattias Gaertner
  
  This unit registers the mysql components of the FCL.
}
unit RegisterMySQL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources,
  {$IFDEF MySQL3}
  MySQLDB3,
  {$ELSE}
  MySQLDB4,
  {$ENDIF}
  LazarusPackageIntf;

procedure Register;

implementation

procedure RegisterUnitMySQLDB;
begin
  RegisterComponents('MySQL',[TMySQLDatabase,TMySQLDataset]);
end;

procedure Register;
begin
  {$IFDEF MySQL3}
  RegisterUnit('MySQLDB3',@RegisterUnitMySQLDB);
  {$ELSE}
  RegisterUnit('MySQLDB4',@RegisterUnitMySQLDB);
  {$ENDIF}
end;

initialization
  {$i registermysql.lrs}

end.
