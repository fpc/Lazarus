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

  This unit registers the mysqlconnection components of the FCL.
}
unit registermysql4conn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, {$IFNDEF VER1_0} mysql4conn,{$ENDIF} LazarusPackageIntf;

procedure Register;

implementation

procedure RegisterUnitMySQL4Conn;
begin
{$IFNDEF VER1_0}
  RegisterComponents('SQLdb',[TMySQLConnection]);
{$ENDIF}
end;

procedure Register;
begin
  RegisterUnit('mysql4conn',@RegisterUnitMySQL4Conn);
end;

initialization
  {$i registermysql4conn.lrs}

end.

