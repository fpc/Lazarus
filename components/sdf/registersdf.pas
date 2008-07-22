{  $Id$  }
{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Michael Van Canneyt
  
  This unit registers the TSDFDataset/TFixedFormatDataset components of the FCL.
}
unit RegisterSDF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, sdfdata, LazarusPackageIntf;

procedure Register;

implementation

procedure RegisterUnitSDF;
begin
  RegisterComponents('Data Access',[TSDFDataset,TFixedFormatDataSet]);
end;

procedure Register;
begin
  RegisterUnit('sdfdata',@RegisterUnitSDF);
end;

initialization
  {$i registersdf.lrs}

end.
