unit CustomDrawn_Mac;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils, Types,
  // LCL -> Use only TForm, TWinControl, TCanvas and TLazIntfImage
  Graphics, Controls, LCLType,
  //
  customdrawndrawers, customdrawn_common;

type

  { TCDDrawerMac }

  TCDDrawerMac = class(TCDDrawerCommon)
  public
  end;

implementation

initialization
  RegisterDrawer(TCDDrawerMac.Create, dsMacOSX);
end.

