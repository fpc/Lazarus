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
  
  This unit registers the interbase components of the FCL.
}
unit RegisterInterbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Interbase, LazarusPackageIntf;

procedure Register;

implementation

procedure RegisterUnitInterbase;
begin
  RegisterComponents('Interbase',[TIBDatabase,TIBTransaction,TIBQuery]);
end;

procedure Register;
begin
  RegisterUnit('Interbase',@RegisterUnitInterbase);
end;

initialization
  {$i registerinterbase.lrs}

end.
