{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DateTimeCtrls;

{$warn 5023 off : no warning about unused units}
interface

uses
  DateTimePicker, DBDateTimePicker, DateTimePickerPropEdit, 
  DateTimeControlsReg, calcontrolwrapper, lclcalwrapper, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DateTimeControlsReg', @DateTimeControlsReg.Register);
end;

initialization
  RegisterPackage('DateTimeCtrls', @Register);
end.
