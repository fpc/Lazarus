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
  
  This unit registers the TMemDataset components of the FCL.
}
unit registermemds;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, memds, LazarusPackageIntf;

procedure Register;

implementation

procedure RegisterUnitMemDS;
begin
  RegisterComponents('Data Access',[TMemDataset]);
end;

procedure Register;
begin
  RegisterUnit('memds',@RegisterUnitMemDS);
end;

initialization
  {$i registermemds.lrs}

end.
