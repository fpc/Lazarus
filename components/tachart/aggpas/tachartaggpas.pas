{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TAChartAggPas;

{$warn 5023 off : no warning about unused units}
interface

uses
  TADrawerAggPas, TAGUIConnectorAggPas, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('TAGUIConnectorAggPas', @TAGUIConnectorAggPas.Register);
end;

initialization
  RegisterPackage('TAChartAggPas', @Register);
end.
