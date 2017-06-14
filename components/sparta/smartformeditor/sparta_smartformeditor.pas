{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit sparta_SmartFormEditor;

interface

uses
  sparta_reg_SmartFormEditor, sparta_FakeFormBackground, sparta_FakeFormBG, 
  sparta_EDTU_Main, sparta_ComponentPalette, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('sparta_reg_SmartFormEditor', 
    @sparta_reg_SmartFormEditor.Register);
end;

initialization
  RegisterPackage('sparta_SmartFormEditor', @Register);
end.
