{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TAChartBgra;

interface

uses
  TADrawerBGRA, TABGRAUtils, TAGUIConnectorBGRA, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('TAGUIConnectorBGRA', @TAGUIConnectorBGRA.Register);
end;

initialization
  RegisterPackage('TAChartBgra', @Register);
end.
