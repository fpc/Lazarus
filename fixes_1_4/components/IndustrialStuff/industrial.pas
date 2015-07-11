{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit industrial;

interface

uses
  IndLed, Sensors, AllIndustrialRegister, LedNumber, indGnouMeter, AdvLed, 
  indcyBaseLed, indcyClasses, indcyGraphics, indcyTypes, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllIndustrialRegister', @AllIndustrialRegister.Register);
end;

initialization
  RegisterPackage('industrial', @Register);
end.
