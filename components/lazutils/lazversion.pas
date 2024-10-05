{
 /***************************************************************************
                               lazversion.pas
                             -------------------
                         Version numbers for Lazarus

 ***************************************************************************/

 *****************************************************************************
  This file is part of the LazUtils package

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit LazVersion;

{$mode objfpc}{$H+}

interface

type
  TLCLWidgetTypeNameEvent = function(): string;

const
  laz_major = 4;
  laz_minor = 99;
  laz_release = 0;
  laz_patch = 0;
  laz_fullversion = ((laz_major *  100 + laz_minor) * 100 + laz_release) * 100 + laz_patch;
  laz_version = '4.99.0.0';

var
  OnLCLWidgetTypeName: TLCLWidgetTypeNameEvent;  // Set by LCL

function GetLCLWidgetTypeName: string;

implementation

function GetLCLWidgetTypeName: string;
begin
  if Assigned(OnLCLWidgetTypeName) then
    Result := OnLCLWidgetTypeName()
  else
    Result := '';
end;

end.

