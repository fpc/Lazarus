{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit sparta_MDI;

interface

uses
  sparta_BasicResizeFrame, sparta_InterfacesMDI, sparta_BasicResizer, 
  sparta_MDI_StrConsts, sparta_BasicFakeCustom, sparta_FormBackgroundForMDI, 
  sparta_MultiplyResizer, sparta_AbstractResizer, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('sparta_MDI', @Register);
end.
