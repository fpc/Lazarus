{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IdeConfig;

{$warn 5023 off : no warning about unused units}
interface

uses
  CompOptsModes, CoolBarOptions, DiffPatch, EditorToolBarOptions, 
  EnvironmentOpts, etMakeMsgParser, IDECmdLine, IdeConfStrConsts, 
  IDEGuiCmdLine, IDEOptionDefs, IDEProcs, IdeXmlConfigProcs, LazConf, 
  ModeMatrixOpts, RecentListProcs, SearchPathProcs, ToolBarOptionsBase, 
  TransferMacros, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('IdeConfig', @Register);
end.
