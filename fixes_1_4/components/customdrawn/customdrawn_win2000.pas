unit customdrawn_win2000;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils,
  // fpimage
  fpcanvas, fpimgcanv, fpimage,
  // LCL -> Use only TForm, TWinControl, TCanvas and TLazIntfImage
  Graphics, Controls, LCLType, LCLIntf, IntfGraphics,
  //
  customdrawndrawers, customdrawn_common;

type

  { TCDDrawerWin2k }

  TCDDrawerWin2k = class(TCDDrawerCommon)
  public
  end;

implementation

initialization
  RegisterDrawer(TCDDrawerWin2k.Create, dsWin2000);
end.

