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

  This unit registers the ibconnection component of the FCL.
}
unit registeribconnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources,{$IFNDEF VER1_0} ibconnection,{$ENDIF} LazarusPackageIntf;

procedure Register;

implementation

procedure RegisterUnitIBConnection;
begin
{$IFNDEF VER1_0}
  RegisterComponents('SQLdb',[TIBConnection]);
{$ENDIF}
end;

procedure Register;
begin
  RegisterUnit('ibconnection',@RegisterUnitIBConnection);
end;

initialization

end.

