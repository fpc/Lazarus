{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IdeConfig;

{$warn 5023 off : no warning about unused units}
interface

uses
  ApplicationBundle, BaseBuildManager, Compiler, CompilerOptions, 
  CompOptsModes, CoolBarOptions, DialogProcs, DiffPatch, EditDefineTree, 
  EditorToolBarOptions, EnvironmentOpts, etFPCMsgFilePool, etFPCMsgParser, 
  etMakeMsgParser, etPas2jsMsgParser, ExtTools, ExtToolsConsole, 
  FindProjPackUnit, FppkgHelper, IdeBuilder, IDECmdLine, IdeConfigPckTest, 
  IdeConfStrConsts, IDEGuiCmdLine, IDEOptionDefs, IDEProcs, IdeTransferMacros, 
  IDETranslations, IdeXmlConfigProcs, InitialSetupProc, LazConf, MiscOptions, 
  ModeMatrixOpts, ParsedCompilerOpts, ProjectBuildMode, ProjPackCommon, 
  RecentListProcs, ResourceConvertProc, SearchPathProcs, ToolBarOptionsBase, 
  TransferMacros, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('IdeConfig', @Register);
end.
