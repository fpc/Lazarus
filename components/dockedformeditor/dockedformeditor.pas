{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DockedFormEditor;

{$warn 5023 off : no warning about unused units}
interface

uses
  DockedRegister, DockedStrConsts, DockedFormAccesses, DockedMainIDE, 
  DockedResizer, DockedOptionsIDE, DockedOptionsFrame, DockedTools, 
  DockedDesignForm, DockedSourcePageControl, DockedSourceWindow, 
  DockedAnchorDesigner, DockedBasicAnchorDesigner, DockedAnchorControl, 
  DockedGrip, DockedResizeControl, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DockedRegister', @DockedRegister.Register);
end;

initialization
  RegisterPackage('DockedFormEditor', @Register);
end.
