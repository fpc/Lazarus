{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit industrial;

interface

uses
  IndAdvLed, IndLed, Sensors, AllIndustrialRegister, cyBaseLed, cyClasses,
  cyGraphics, cyTypes, LedNumber, IndGnouMeter, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllIndustrialRegister', @AllIndustrialRegister.Register);
end;

initialization
  RegisterPackage('industrial', @Register);
end.
