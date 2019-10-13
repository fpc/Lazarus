{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit sparta_DockedFormEditor;

{$warn 5023 off : no warning about unused units}
interface

uses
  sparta_reg_DockedFormEditor, sparta_DesignedForm, sparta_Resizer, 
  sparta_ResizerFrame, SpartaAPI, sparta_FakeCustom, sparta_FakeForm, 
  sparta_FakeFrame, sparta_FakeNonControl, sparta_MainIDE, sparta_strconsts, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('sparta_reg_DockedFormEditor', 
    @sparta_reg_DockedFormEditor.Register);
end;

initialization
  RegisterPackage('sparta_DockedFormEditor', @Register);
end.
