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
  Classes, SysUtils, LResources, MySQLDB, LazarusPackageIntf;

procedure Register;

implementation

procedure RegisterUnitMySQLDB;
begin
  RegisterComponents('MySQL',[TMySQLDatabase,TMySQLDataset]);
end;

procedure Register;
begin
  RegisterUnit('MySQLDB',@RegisterUnitMySQLDB);
end;

initialization
  {$i registermysql.lrs}

end.
