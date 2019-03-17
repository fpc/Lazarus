{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascalscript;

{$warn 5023 off : no warning about unused units}
interface

uses
  uPSR_stdctrls, PascalScript_Core_Reg, uPSC_buttons, uPSC_controls, 
  uPSC_extctrls, uPSC_forms, uPSC_graphics, uPSC_menus, uPSC_stdctrls, 
  uPSComponent_Forms, uPSComponent_StdCtrls, uPSR_buttons, uPSR_controls, 
  uPSR_extctrls, uPSR_forms, uPSR_graphics, uPSR_menus, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PascalScript_Core_Reg', @PascalScript_Core_Reg.Register);
end;

initialization
  RegisterPackage('pascalscript', @Register);
end.
