unit customdrawn_wince;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils,
  // LCL -> Use only TForm, TWinControl, TCanvas and TLazIntfImage
  Graphics, Controls, LCLType,
  // Others only for types
  StdCtrls,
  //
  customdrawndrawers, customdrawn_common;

type
  TCDDrawerWinCE = class(TCDDrawerCommon)
  public
  end;

implementation

initialization
  RegisterDrawer(TCDDrawerWinCE.Create, dsWinCE);
end.

