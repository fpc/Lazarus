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

interface

uses
  Classes, SysUtils, LResources,{$IFNDEF VER1_0} sqldb,{$ENDIF} LazarusPackageIntf;

procedure Register;

implementation

procedure RegisterUnitSQLdb;
begin
{$IFNDEF VER1_0}
  RegisterComponents('SQLdb',[TSQLTransaction
                              ,TSQLQuery]);
{$ENDIF}
end;

procedure Register;
begin
  RegisterUnit('sqldb',@RegisterUnitSQLdb);
end;

initialization

end.
