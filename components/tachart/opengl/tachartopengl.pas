{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TAChartOpenGL;

{$warn 5023 off : no warning about unused units}
interface

uses
  TADrawerOpenGL, TAOpenGL, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('TAChartOpenGL', @Register);
end.
