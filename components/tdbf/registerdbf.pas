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

  Author: Michael Van Canneyt
  
  This unit registers the TDBF component of the FCL.
}
unit RegisterDBF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Dbf, LazarusPackageIntf;

procedure Register;

implementation

procedure RegisterUnitDBF;
begin
  RegisterComponents('Data Access',[TDbf]);
end;

procedure Register;
begin
  RegisterUnit('DBF',@RegisterUnitDBF);
end;

initialization
  {$i registerdbf.lrs}
end.
