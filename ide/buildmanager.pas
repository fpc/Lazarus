{
 /***************************************************************************
                            buildmanager.pas
                            ----------------


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit BuildManager;

{$mode objfpc}{$H+}

{off $DEFINE VerboseFPCSrcScan}

interface

uses
  // RTL + FCL
  Classes, SysUtils, Laz_AVL_Tree,
  // LCL
  InterfaceBase, LCLPlatformDef, Dialogs, Forms, Controls,
  // CodeTools
  ExprEval, BasicCodeTools, CodeToolManager, DefineTemplates, CodeCache,
  FileProcs, CodeToolsCfgScript, LinkScanner,
  // LazUtils
  LConvEncoding, FileUtil, LazFileUtils, LazFileCache, LazUTF8, Laz2_XMLCfg,
  LazUtilities, LazStringUtils, LazMethodList,
  // IDEIntf
  BaseIDEIntf, IDEOptionsIntf, ProjectIntf, MacroIntf, IDEDialogs, IDEExternToolIntf,
  CompOptsIntf, LazIDEIntf, MacroDefIntf, IDEMsgIntf, PackageDependencyIntf,
  // IDE
  IDECmdLine, LazarusIDEStrConsts, DialogProcs, IDEProcs,
  InputHistory, EditDefineTree, ProjectResources, MiscOptions, LazConf,
  EnvironmentOpts, TransferMacros, CompilerOptions, PublishModule,
  ExtTools, etMakeMsgParser, etFPCMsgParser, etPas2jsMsgParser,
  Compiler, FPCSrcScan, PackageDefs, PackageSystem, Project, ProjectIcon,
  ModeMatrixOpts, BaseBuildManager, ApplicationBundle, RunParamsOpts;
  
type

  { TBuildManager }

  TBuildManager = class(TBaseBuildManager)
  private
    FBuildTarget: TProject;
    FUnitSetCache: TFPCUnitSetCache;
    fBuildLazExtraOptions: string; // last build lazarus extra options
    FUnitSetChangeStamp: integer;
    FFPCSrcScans: TFPCSrcScans;
    FProjectNameSpace: string;
    FProjectNameSpaceCode: TCodeBuffer;
    FProjectNameSpaceCodeChgStep: integer;
    // Macro FPCVer
    FFPCVer: string;
    FFPC_FULLVERSION: integer;
    FFPCVerChangeStamp: integer;
    // Macro InstantFPCCache
    FMacroInstantFPCCache: string;
    FMacroInstantFPCCacheValid: boolean;
    // current target
    fTargetOS: string;
    fTargetCPU: string;
    fLCLWidgetType: string;
    procedure DoOnRescanFPCDirectoryCache(Sender: TObject);
    function GetTargetFilename: String;
    procedure OnMacroSubstitution(TheMacro: TTransferMacro;
                               const MacroName: string; var s: string;
                               const {%H-}Data: PtrInt; var Handled, {%H-}Abort: boolean;
                               {%H-}Depth: integer);
    function OnSubstituteCompilerOption({%H-}Options: TParsedCompilerOptions;
                                        const UnparsedValue: string;
                                        PlatformIndependent: boolean): string;
    function MacroFuncBuildMode(const {%H-}Param: string; const {%H-}Data: PtrInt;
                             var {%H-}Abort: boolean): string;
    function MacroFuncEnv(const Param: string; const {%H-}Data: PtrInt;
                          var {%H-}Abort: boolean): string;
    function MacroFuncCompPath(const {%H-}s:string; const {%H-}Data: PtrInt;
                               var {%H-}Abort: boolean): string;
    function MacroFuncFPCMsgFile(const {%H-}Param: string; const {%H-}Data: PtrInt;
                          var {%H-}Abort: boolean): string;
    function MacroFuncFPCVer(const {%H-}Param: string; const {%H-}Data: PtrInt;
                             var {%H-}Abort: boolean): string;
    function MacroFuncFPC_FULLVERSION(const {%H-}Param: string; const {%H-}Data: PtrInt;
                             var {%H-}Abort: boolean): string;
    function MacroFuncLCLWidgetType(const {%H-}Param: string; const Data: PtrInt;
                                    var {%H-}Abort: boolean): string;
    function MacroFuncLazVer(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                    var {%H-}Abort: boolean): string;
    function MacroFuncMake(const {%H-}Param: string; const {%H-}Data: PtrInt;
                           var {%H-}Abort: boolean): string;// make utility
    function MacroFuncMakeExe(const Filename: string; const {%H-}Data: PtrInt;
                              var {%H-}Abort: boolean): string;
    function MacroFuncMakeLib(const Filename: string; const {%H-}Data: PtrInt;
                              var {%H-}Abort: boolean): string;
    function MacroFuncInstantFPCCache(const {%H-}Param: string; const {%H-}Data: PtrInt;
                           var {%H-}Abort: boolean): string;// path of the instantfpc cache
    function MacroFuncParams(const {%H-}Param: string; const {%H-}Data: PtrInt;
                             var {%H-}Abort: boolean): string;
    function MacroFuncProject(const Param: string; const {%H-}Data: PtrInt;
                              var {%H-}Abort: boolean): string;
    function MacroFuncProjFile(const {%H-}Param: string; const {%H-}Data: PtrInt;
                               var {%H-}Abort: boolean): string;
    function MacroFuncProjIncPath(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                  var {%H-}Abort: boolean): string;
    function MacroFuncProjNamespaces(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                   var {%H-}Abort: boolean): string;
    function MacroFuncProjOutDir(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                 var {%H-}Abort: boolean): string;
    function MacroFuncProjPath(const {%H-}Param: string; const {%H-}Data: PtrInt;
                               var {%H-}Abort: boolean): string;
    function MacroFuncProjPublishDir(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                     var {%H-}Abort: boolean): string;
    function MacroFuncProjSrcPath(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                  var {%H-}Abort: boolean): string;
    function MacroFuncProjUnitPath(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                   var {%H-}Abort: boolean): string;
    function MacroFuncRunCmdLine(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                 var {%H-}Abort: boolean): string;
    function MacroFuncSrcOS(const {%H-}Param: string; const Data: PtrInt;
                            var {%H-}Abort: boolean): string;
    function MacroFuncTargetCmdLine(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                    var {%H-}Abort: boolean): string;
    function MacroFuncTargetCPU(const {%H-}Param: string; const Data: PtrInt;
                                var {%H-}Abort: boolean): string;
    function MacroFuncTargetFile(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                 var {%H-}Abort: boolean): string;
    function MacroFuncTargetOS(const {%H-}Param: string; const Data: PtrInt;
                               var {%H-}Abort: boolean): string;
    function MacroFuncIDEBuildOptions(const {%H-}Param: string; const Data: PtrInt;
                               var {%H-}Abort: boolean): string;
    function MacroFuncPrimaryConfigPath(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                        var {%H-}Abort: boolean): string;
    function MacroFuncSecondaryConfigPath(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                          var {%H-}Abort: boolean): string;
    function MacroFuncFallbackOutputRoot(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                         var {%H-}Abort: boolean): string;

    function CTMacroFuncProjectNamespaces(Data: Pointer): boolean;
    function CTMacroFuncProjectUnitPath(Data: Pointer): boolean;
    function CTMacroFuncProjectIncPath(Data: Pointer): boolean;
    function CTMacroFuncProjectSrcPath(Data: Pointer): boolean;
    procedure OnProjectDestroy(Sender: TObject);
    procedure SetUnitSetCache(const AValue: TFPCUnitSetCache);
    function GetProjectDefaultNamespace: string; // read .lpr file
  protected
    // command line overrides
    OverrideTargetOS: string;
    OverrideTargetCPU: string;
    OverrideLCLWidgetType: string;
    DefaultCfgVars: TCTCfgScriptVariables;
    DefaultCfgVarsBuildMacroStamp: integer;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    function OnGetBuildMacroValues(Options: TBaseCompilerOptions;
                                   IncludeSelf: boolean): TCTCfgScriptVariables;
    function GetActiveBuildModeName: string;
    procedure AppendMatrixCustomOption(Sender: TObject;
      var Options: string; Types: TBuildMatrixGroupTypes);
    procedure GetMatrixOutputDirectoryOverride(Sender: TObject;
      var OutDir: string; Types: TBuildMatrixGroupTypes);
    function GetModeMatrixTarget(Sender: TObject): string;
    function EnvironmentOptionsIsGlobalMode(const Identifier: string): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetupTransferMacros;
    procedure TranslateMacros;
    procedure SetupExternalTools(aToolsClass: TExternalToolsClass);
    procedure SetupCompilerInterface;
    procedure SetupInputHistories(aInputHist: TInputHistories);
    procedure EnvOptsChanged;

    function GetBuildMacroOverride(const MacroName: string): string; override;
    function GetBuildMacroOverrides: TStrings; override;
    function GetTargetOS: string; override;
    function GetTargetCPU: string; override;
    function GetLCLWidgetType: string; override;
    function GetRunCommandLine: string; override;

    function GetCompilerFilename: string; override;
    function GetFPCompilerFilename: string; override;
    function GetFPCFrontEndOptions: string; override;
    function GetProjectPublishDir: string; override;
    function GetProjectTargetFilename(aProject: TProject): string; override;
    function GetProjectUsesAppBundle: Boolean; override;
    function GetTestUnitFilename(AnUnitInfo: TUnitInfo): string; override;
    function GetTestBuildDirectory: string; override;
    function IsTestUnitFilename(const AFilename: string): boolean; override;
    function GetTargetUnitFilename(AnUnitInfo: TUnitInfo): string; override;

    procedure UpdateEnglishErrorMsgFilename;
    procedure RescanCompilerDefines(ResetBuildTarget, ClearCaches,
                                    WaitTillDone, Quiet: boolean); override;
    function CompilerOnDiskChanged: boolean; override;
    procedure LoadCompilerDefinesCaches;
    procedure SaveCompilerDefinesCaches;
    property UnitSetCache: TFPCUnitSetCache read FUnitSetCache write SetUnitSetCache;

    function DoCheckIfProjectNeedsCompilation(AProject: TProject;
                    out NeedBuildAllFlag: boolean; var Note: string): TModalResult;
    function CheckAmbiguousSources(const AFilename: string;
                                   Compiling: boolean): TModalResult; override;
    function DeleteAmbiguousFiles(const Filename:string
                                  ): TModalResult; override;
    function CheckUnitPathForAmbiguousPascalFiles(const BaseDir, TheUnitPath,
                                    CompiledExt, ContextDescription: string
                                    ): TModalResult; override;
    function CreateProjectApplicationBundle: Boolean; override;
    function BackupFileForWrite(const Filename: string): TModalResult; override;

    function GetResourceType(AnUnitInfo: TUnitInfo): TResourceType;
    function FindLRSFilename(AnUnitInfo: TUnitInfo;
                             UseDefaultIfNotFound: boolean): string;
    function GetDefaultLRSFilename(AnUnitInfo: TUnitInfo): string;
    function UpdateLRSFromLFM(AnUnitInfo: TUnitInfo; ShowAbort: boolean): TModalResult;
    function UpdateProjectAutomaticFiles(TestDir: string): TModalResult; override;

    // methods for building IDE (will be changed when project groups are there)
    procedure SetBuildTarget(const TargetOS, TargetCPU, LCLWidgetType: string;
                             ScanFPCSrc: TScanModeFPCSources; Quiet: boolean);
    procedure SetBuildTargetProject1; override; overload;
    procedure SetBuildTargetProject1(Quiet: boolean; ScanFPCSrc: TScanModeFPCSources = smsfsBackground); overload;
    procedure SetBuildTargetIDE; override;
    function BuildTargetIDEIsDefault: boolean; override;

    property FPCSrcScans: TFPCSrcScans read FFPCSrcScans;
    property BuildTarget: TProject read FBuildTarget; // TProject or nil
  end;
  
var
  MainBuildBoss: TBuildManager = nil;
  TheCompiler: TCompiler = nil;

procedure RunBootHandlers(ht: TLazarusIDEBootHandlerType);external name 'ideintf_LazIDEIntf_RunBootHandlers';

implementation

type
  TUnitFile = record
    FileUnitName: string;
    Filename: string;
  end;
  PUnitFile = ^TUnitFile;

procedure BMLazConfMacroFunction(var s: string);
begin
  if not GlobalMacroList.SubstituteStr(s) then
    debugln(['BMLazConfMacroFunction failed "',s,'"']);
end;

function CompareUnitFiles(UnitFile1, UnitFile2: PUnitFile): integer;
begin
  Result:=CompareIdentifierPtrs(Pointer(UnitFile1^.FileUnitName),
                                Pointer(UnitFile2^.FileUnitName));
end;

function CompareUnitNameAndUnitFile(UnitName: PChar; UnitFile: PUnitFile): integer;
begin
  Result:=CompareIdentifierPtrs(Pointer(UnitName),Pointer(UnitFile^.FileUnitName));
end;

{ TBuildManager }

procedure TBuildManager.OnProjectDestroy(Sender: TObject);
var
  aProject: TProject;
begin
  if not (Sender is TProjectIDEOptions) then
    exit;
  aProject:=TProjectIDEOptions(Sender).Project;
  if FBuildTarget=aProject then
    FBuildTarget:=nil;
end;

procedure TBuildManager.OnMacroSubstitution(TheMacro: TTransferMacro;
  const MacroName: string; var s: string; const Data: PtrInt; var Handled,
  Abort: boolean; Depth: integer);
begin
  if TheMacro=nil then begin
    if ConsoleVerbosity>=0 then
      DebugLn('Warning: (lazarus) Macro not defined: "'+MacroName+'".');
    {$IFDEF VerboseMacroNotDefined}
    DumpStack;
    {$ENDIF}
    s:='';
    //IDEMessageDialog('Unknown Macro','Macro not defined: "'+s+'".',mtError,[mbAbort],0);
    Handled:=true;
    exit;
  end;
end;

function TBuildManager.OnSubstituteCompilerOption(
  Options: TParsedCompilerOptions; const UnparsedValue: string;
  PlatformIndependent: boolean): string;
begin
  Result:=UnparsedValue;
  if PlatformIndependent then
    GlobalMacroList.SubstituteStr(Result,CompilerOptionMacroPlatformIndependent)
  else
    GlobalMacroList.SubstituteStr(Result,CompilerOptionMacroNormal);
end;

function TBuildManager.MacroFuncBuildMode(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetActiveBuildModeName;
end;

constructor TBuildManager.Create(AOwner: TComponent);
begin
  EnvironmentOptions := TEnvironmentOptions.Create;
  IDEEnvironmentOptions := EnvironmentOptions;
  EnvironmentOptions.IsGlobalMode:=@EnvironmentOptionsIsGlobalMode;
  DefaultCfgVars:=TCTCfgScriptVariables.Create;
  DefaultCfgVarsBuildMacroStamp:=CTInvalidChangeStamp;
  FFPCVerChangeStamp:=CTInvalidChangeStamp;
  MainBuildBoss:=Self;
  inherited Create(AOwner);
  fTargetOS:=GetCompiledTargetOS;
  fTargetCPU:=GetCompiledTargetCPU;
  fLCLWidgetType:=GetLCLWidgetTypeName;
  FUnitSetChangeStamp:=TFPCUnitSetCache.GetInvalidChangeStamp;

  OnBackupFileInteractive:=@BackupFileForWrite;

  GetBuildMacroValues:=@OnGetBuildMacroValues;
  OnAppendCustomOption:=@AppendMatrixCustomOption;
  OnGetOutputDirectoryOverride:=@GetMatrixOutputDirectoryOverride;

  CodeToolBoss.OnRescanFPCDirectoryCache:=@DoOnRescanFPCDirectoryCache;
end;

destructor TBuildManager.Destroy;
begin
  FreeAndNil(ExternalTools);

  GetBuildMacroValues:=nil;
  OnAppendCustomOption:=nil;
  OnBackupFileInteractive:=nil;

  FreeAndNil(FFPCSrcScans);

  LazConfMacroFunc:=nil;
  FreeAndNil(InputHistories);
  FreeAndNil(DefaultCfgVars);

  if CompareMethods(TMethod(CodeToolBoss.OnRescanFPCDirectoryCache), TMethod(@DoOnRescanFPCDirectoryCache)) then
    CodeToolBoss.OnRescanFPCDirectoryCache:=nil;

  inherited Destroy;
  MainBuildBoss:=nil;
end;

procedure TBuildManager.SetupTransferMacros;
begin
  LazConfMacroFunc:=@BMLazConfMacroFunction;
  GlobalMacroList:=TTransferMacroList.Create;
  GlobalMacroList.OnSubstitution:=@OnMacroSubstitution;
  IDEMacros:=TLazIDEMacros.Create;
  CompilerOptions.OnParseString:=@OnSubstituteCompilerOption;

  // environment
  EnvironmentOptions.InitMacros(GlobalMacroList);

  // project
  GlobalMacroList.Add(TTransferMacro.Create('Project','',
                      lisProjectMacroProperties,@MacroFuncProject,[]));
  GlobalMacroList.Add(TTransferMacro.Create('BuildMode','',
                      lisNameOfActiveBuildMode, @MacroFuncBuildMode, []));
  GlobalMacroList.Add(TTransferMacro.Create('LCLWidgetType','',
                      lisLCLWidgetType,@MacroFuncLCLWidgetType,[]));
  GlobalMacroList.Add(TTransferMacro.Create('TargetCPU','',
                      lisTargetCPU,@MacroFuncTargetCPU,[]));
  GlobalMacroList.Add(TTransferMacro.Create('TargetOS','',
                      lisTargetOS,@MacroFuncTargetOS,[]));
  GlobalMacroList.Add(TTransferMacro.Create('SrcOS','',
                      lisSrcOS,@MacroFuncSrcOS,[]));
  GlobalMacroList.Add(TTransferMacro.Create('CompPath','',
                      lisCompilerFilename,@MacroFuncCompPath,[]));
  GlobalMacroList.Add(TTransferMacro.Create('FPCVer','',
                      lisFPCVersionEG222, @MacroFuncFPCVer, []));
  GlobalMacroList.Add(TTransferMacro.Create('FPC_FULLVERSION','',
                      lisFPCFullVersionEG20701, @MacroFuncFPC_FULLVERSION, []));
  GlobalMacroList.Add(TTransferMacro.Create('FPCMsgFile','',
                     dlgFilterFPCMessageFile, @MacroFuncFPCMsgFile, []));
  GlobalMacroList.Add(TTransferMacro.Create('Params','',
                      lisCommandLineParamsOfProgram,@MacroFuncParams,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjFile','',
                      lisProjectFilename,@MacroFuncProjFile,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjPath','',
                      lisProjectDirectory,@MacroFuncProjPath,[]));
  GlobalMacroList.Add(TTransferMacro.Create('TargetFile','',
                      lisTargetFilenameOfProject,@MacroFuncTargetFile,[]));
  GlobalMacroList.Add(TTransferMacro.Create('TargetCmdLine','',
                      lisTargetFilenamePlusParams,@MacroFuncTargetCmdLine,[]));
  GlobalMacroList.Add(TTransferMacro.Create('RunCmdLine','',
                      lisLaunchingCmdLine,@MacroFuncRunCmdLine,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjPublishDir','',
                      lisPublishProjDir,@MacroFuncProjPublishDir,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjNamespaces','',
                      lisProjectNamespaces,@MacroFuncProjNamespaces,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjUnitPath','',
                      lisProjectUnitPath,@MacroFuncProjUnitPath,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjIncPath','',
                      lisProjectIncPath,@MacroFuncProjIncPath,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjSrcPath','',
                      lisProjectSrcPath,@MacroFuncProjSrcPath,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjOutDir','',
                      lisProjectOutDir,@MacroFuncProjOutDir,[]));
  GlobalMacroList.Add(TTransferMacro.Create('Env','',
                     lisEnvironmentVariableNameAsParameter, @MacroFuncEnv, []));
  GlobalMacroList.Add(TTransferMacro.Create('MakeExe','',
                      lisMakeExe,@MacroFuncMakeExe,[]));
  GlobalMacroList.Add(TTransferMacro.Create('MakeLib','',
                      lisMakeExe,@MacroFuncMakeLib,[]));
  GlobalMacroList.Add(TTransferMacro.Create('Make','',
                      lisPathOfTheMakeUtility, @MacroFuncMake, []));
  GlobalMacroList.Add(TTransferMacro.Create('InstantFPCCache','',
                      lisPathOfTheInstantfpcCache, @MacroFuncInstantFPCCache, []));
  GlobalMacroList.Add(TTransferMacro.Create('IDEBuildOptions','',
                      lisIDEBuildOptions, @MacroFuncIDEBuildOptions, []));
  GlobalMacroList.Add(TTransferMacro.Create('PrimaryConfigPath','',
                      lisPrimaryConfigPath, @MacroFuncPrimaryConfigPath, []));
  GlobalMacroList.Add(TTransferMacro.Create('SecondaryConfigPath','',
                    lisSecondaryConfigPath, @MacroFuncSecondaryConfigPath, []));
  GlobalMacroList.Add(TTransferMacro.Create('FallbackOutputRoot','',
                     lisSecondaryConfigPath, @MacroFuncFallbackOutputRoot, []));
  GlobalMacroList.Add(TTransferMacro.Create('LAZVer','',
                     lisLAZVer, @MacroFuncLazVer, []));

  // codetools macro functions
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECTNAMESPACES',nil,@CTMacroFuncProjectNamespaces);
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECTUNITPATH',nil,@CTMacroFuncProjectUnitPath);
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECTINCPATH',nil,@CTMacroFuncProjectIncPath);
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECTSRCPATH',nil,@CTMacroFuncProjectSrcPath);

  RunBootHandlers(libhTransferMacrosCreated);
end;

procedure TBuildManager.TranslateMacros;

  procedure tr(const MacroName, Description: string);
  var
    Macro: TTransferMacro;
  begin
    Macro:=GlobalMacroList.FindByName(MacroName);
    if Macro=nil then exit;
    Macro.Description:=Description;
  end;

begin
  tr('Project',lisProjectMacroProperties);
  tr('BuildMode',lisNameOfActiveBuildMode);
  tr('LCLWidgetType',lisLCLWidgetType);
  tr('TargetCPU',lisTargetCPU);
  tr('TargetOS',lisTargetOS);
  tr('SrcOS',lisSrcOS);
  tr('FPCVer',lisFPCVersionEG222);
  tr('LAZVer',lisLAZVer);
  tr('FPC_FULLVERSION',lisFPCFullVersionEG20701);
  tr('Params',lisCommandLineParamsOfProgram);
  tr('ProjFile',lisProjectFilename);
  tr('ProjPath',lisProjectDirectory);
  tr('TargetFile',lisTargetFilenameOfProject);
  tr('TargetCmdLine',lisTargetFilenamePlusParams);
  tr('RunCmdLine',lisLaunchingCmdLine);
  tr('ProjPublishDir',lisPublishProjDir);
  tr('ProjNamespaces',lisProjectNamespaces);
  tr('ProjUnitPath',lisProjectUnitPath);
  tr('ProjIncPath',lisProjectIncPath);
  tr('ProjSrcPath',lisProjectSrcPath);
  tr('ProjOutDir',lisProjectOutDir);
  tr('Env',lisEnvironmentVariableNameAsParameter);
  tr('FPCMsgFile',dlgFilterFPCMessageFile);
  tr('MakeExe',lisMakeExe);
  tr('MakeLib',lisMakeExe);
  tr('Make',lisPathOfTheMakeUtility);
  tr('InstantFPCCache',lisPathOfTheInstantfpcCache);
  tr('IDEBuildOptions',lisIDEBuildOptions);
  tr('PrimaryConfigPath',lisPrimaryConfigPath);
  tr('SecondaryConfigPath',lisSecondaryConfigPath);
  tr('FallbackOutputRoot',lisSecondaryConfigPath);
  tr('CompPath',lisCompilerFilename);
  tr('FPCSrcDir',lisFreePascalSourceDirectory);
  tr('LazarusDir',lisLazarusDirectory);
  tr('ExeExt',lisFileExtensionOfPrograms);
  tr('LanguageID',lisLazarusLanguageID);
  tr('LanguageName',lisLazarusLanguageName);
  tr('TestDir',lisTestDirectory);
  tr('ConfDir',lisConfigDirectory);
  tr('Home',lisUserSHomeDirectory);
  tr('Ext',lisTMFunctionExtractFileExtension);
  tr('Path',lisTMFunctionExtractFilePath);
  tr('Name',lisTMFunctionExtractFileNameExtension);
  tr('NameOnly',lisTMFunctionExtractFileNameOnly);
  tr('MakeDir',lisTMFunctionAppendPathDelimiter);
  tr('MakeFile',lisTMFunctionChompPathDelimiter);
end;

procedure TBuildManager.SetupExternalTools(aToolsClass: TExternalToolsClass);
begin
  // setup the external tool queue
  ExternalTools:=aToolsClass.Create(Self);
  EnvOptsChanged;
  RegisterFPCParser;
  RegisterPas2jsParser;
  RegisterMakeParser;
  ExternalToolList.RegisterParser(TDefaultParser);

  FPCMsgFilePool:=TFPCMsgFilePool.Create(nil);
  Pas2jsMsgFilePool:=TPas2jsMsgFilePool.Create(nil);
end;

procedure TBuildManager.SetupCompilerInterface;
begin
  TheCompiler := TCompiler.Create;
end;

procedure TBuildManager.SetupInputHistories(aInputHist: TInputHistories);
begin
  aInputHist.SetLazarusDefaultFilename;
  aInputHist.Load;
end;

procedure TBuildManager.EnvOptsChanged;
begin
  if EnvironmentOptions.MaxExtToolsInParallel<=0 then
    ExternalTools.MaxProcessCount:=DefaultMaxProcessCount
  else
    ExternalTools.MaxProcessCount:=EnvironmentOptions.MaxExtToolsInParallel;
end;

function TBuildManager.GetBuildMacroOverride(const MacroName: string): string;
begin
  Result:='';
  if SysUtils.CompareText(MacroName,'TargetOS')=0 then
    Result:=OverrideTargetOS
  else if SysUtils.CompareText(MacroName,'TargetCPU')=0 then
    Result:=OverrideTargetCPU
  else if SysUtils.CompareText(MacroName,'LCLWidgetType')=0 then
    Result:=OverrideLCLWidgetType;
end;

function TBuildManager.GetBuildMacroOverrides: TStrings;
begin
  Result:=TStringList.Create;
  if OverrideTargetOS<>'' then
    Result.Values['TargetOS']:=OverrideTargetOS;
  if OverrideTargetCPU<>'' then
    Result.Values['TargetCPU']:=OverrideTargetCPU;
  if OverrideLCLWidgetType<>'' then
    Result.Values['LCLWidgetType']:=OverrideLCLWidgetType;
end;

function TBuildManager.GetTargetOS: string;
begin
  Result:=fTargetOS;
end;

function TBuildManager.GetTargetCPU: string;
begin
  Result:=fTargetCPU;
  //debugln(['TBuildManager.GetTargetCPU ',Result]);
end;

function TBuildManager.GetLCLWidgetType: string;
begin
  Result:=fLCLWidgetType;
end;

function TBuildManager.GetTargetFilename: String;
begin
  Result := GetProjectTargetFilename(Project1);
  if GetProjectUsesAppBundle then
    // return command line to Application Bundle (darwin only)
    Result := ExtractFileNameWithoutExt(Result) + '.app';
end;

function TBuildManager.GetRunCommandLine: string;
var
  TargetFilename: string;
  AMode: TRunParamsOptionsMode;
begin
  Result := '';
  if Project1=nil then exit;
  AMode := Project1.RunParameterOptions.GetActiveMode;
  if (AMode<>nil) and AMode.UseLaunchingApplication then
    Result := AMode.LaunchingApplicationPathPlusParams;

  if Result='' then
  begin
    if (AMode<>nil) then
      Result := AMode.CmdLineParams;
    if GlobalMacroList.SubstituteStr(Result) then
    begin
      TargetFilename := GetTargetFilename;
      if (TargetFilename <> '')
      and (TargetFilename[Length(TargetFilename)] in AllowDirectorySeparators) then
        TargetFilename += ExtractFileNameOnly(
                         Project1.CompilerOptions.GetDefaultMainSourceFileName);
      TargetFilename := '"'+TargetFilename+'"';
      if Result='' then
        Result:=TargetFilename
      else
        Result:=TargetFilename+' '+Result;
    end else
      Result:='';
  end else begin
    if not GlobalMacroList.SubstituteStr(Result) then Result:='';
  end;
end;

function TBuildManager.GetCompilerFilename: string;
var
  s: string;
  Opts: TProjectCompilerOptions;
  Kind: TPascalCompiler;
begin
  Result:='';
  //debugln(['TBuildManager.GetCompilerFilename START FBuildTarget=',DbgSName(FBuildTarget)]);
  if FBuildTarget<>nil then
  begin
    Opts:=FBuildTarget.CompilerOptions;
    //debugln(['TBuildManager.GetCompilerFilename FBuildTarget=',DbgSName(FBuildTarget),' Path=',Opts.CompilerPath,' Build=',[crCompile,crBuild]*Opts.CompileReasons<>[],' Parsing=',Opts.ParsedOpts.Values[pcosCompilerPath].Parsing]);
    if ([crCompile,crBuild]*Opts.CompileReasons<>[])
    and (Opts.CompilerPath<>'')
    and (not Opts.ParsedOpts.Values[pcosCompilerPath].Parsing) then
    begin
      Result:=Opts.CompilerPath;
      // the compiler filename is resolved twice, once for getting the default
      // compiler target OS/CPU and once with the real values.
      // For easier debugging macros, avoid this double resolve.
      if Result='$(CompPath)' then
        Result:=''
      else if (Pos('$',Result)<1) and (FilenameIsAbsolute(Result)) then
        Result:=TrimFilename(Result)
      else
        Result:=FBuildTarget.GetCompilerFilename;
      //debugln(['TBuildManager.GetFPCompilerFilename project compiler="',Result,'"']);
    end;
  end;
  if not IsCompilerExecutable(Result,s,Kind) then begin
    //debugln(['TBuildManager.GetFPCompilerFilename project compiler IS NOT a pascal compiler: "',Result,'"']);
    Result:=EnvironmentOptions.GetParsedCompilerFilename;
    if Kind=pcFPC then ;
  end;
end;

function TBuildManager.GetFPCompilerFilename: string;
begin
  Result:=GetCompilerFilename;
  if GuessCompilerType(Result)<>pcFPC then
    Result:=EnvironmentOptions.GetParsedCompilerFilename;
end;

function TBuildManager.GetFPCFrontEndOptions: string;
var
  s: String;
begin
  Result:='';
  if FBuildTarget<>nil then
  begin
    s:=ExtractFPCFrontEndParameters(FBuildTarget.CompilerOptions.CustomOptions);
    if GlobalMacroList.SubstituteStr(s) then
    begin
      if s<>'' then
        Result:=s;
    end else begin
      debugln(['Warning: (lazarus) [GetFPCFrontEndOptions] ignoring invalid macros in custom options for fpc frontend: "',ExtractFPCFrontEndParameters(FBuildTarget.CompilerOptions.CustomOptions),'"']);
    end;
  end;
  if LazarusIDE<>nil then
    if not LazarusIDE.CallHandlerGetFPCFrontEndParams(Self,Result) then begin
      debugln(['Warning: TBuildManager.GetFPCFrontEndOptions: LazarusIDE.CallHandlerGetFPCFrontEndParams failed Result="',Result,'"']);
    end;
  Result:=UTF8Trim(Result);
end;

function TBuildManager.GetProjectPublishDir: string;
begin
  if Project1<>nil then
    Result:=RealPublishDir(Project1.PublishOptions)
  else
    Result:='';
end;

function TBuildManager.GetProjectTargetFilename(aProject: TProject): string;
var
  AMode: TRunParamsOptionsMode;
begin
  Result:='';
  if aProject=nil then exit;
  AMode := aProject.RunParameterOptions.GetActiveMode;
  if AMode<>nil then
    Result:=AMode.HostApplicationFilename;
  GlobalMacroList.SubstituteStr(Result);
  if (Result='') and (aProject.MainUnitID>=0) then begin
    Result := aProject.CompilerOptions.CreateTargetFilename;
  end;
end;

function TBuildManager.GetProjectUsesAppBundle: Boolean;
begin
  Result := (Project1<>nil)
    and ( (Project1.RunParameterOptions.GetActiveMode=nil)
          or (Project1.RunParameterOptions.GetActiveMode.HostApplicationFilename = ''))
    and (GetTargetOS = 'darwin') and Project1.UseAppBundle;
end;

function TBuildManager.GetTestUnitFilename(AnUnitInfo: TUnitInfo): string;
var
  TestDir: String;
begin
  Result:='';
  if AnUnitInfo=nil then exit;
  TestDir:=GetTestBuildDirectory;
  if TestDir='' then exit;
  Result:=ExtractFilename(AnUnitInfo.Filename);
  if Result='' then exit;
  Result:=TestDir+Result;
end;

function TBuildManager.GetTestBuildDirectory: string;
begin
  Result:=EnvironmentOptions.GetParsedTestBuildDirectory;
end;

function TBuildManager.IsTestUnitFilename(const AFilename: string): boolean;
var
  TestDir: string;
begin
  Result:=false;
  if (Project1<>nil) and Project1.IsVirtual then begin
    TestDir:=GetTestBuildDirectory;
    Result:=FileIsInPath(AFilename,TestDir);
  end;
end;

function TBuildManager.GetTargetUnitFilename(AnUnitInfo: TUnitInfo): string;
begin
  if Project1.IsVirtual then
    Result:=GetTestUnitFilename(AnUnitInfo)
  else
    Result:=AnUnitInfo.Filename;
end;

procedure TBuildManager.UpdateEnglishErrorMsgFilename;
begin
  if EnvironmentOptions.GetParsedLazarusDirectory<>'' then begin
    CodeToolBoss.DefinePool.EnglishErrorMsgFilename:=
      AppendPathDelim(EnvironmentOptions.GetParsedLazarusDirectory)+
        GetForcedPathDelims('components/codetools/fpc.errore.msg');
    CodeToolBoss.CompilerDefinesCache.ExtraOptions:=
                          '-Fr'+CodeToolBoss.DefinePool.EnglishErrorMsgFilename;
  end;
end;

procedure TBuildManager.RescanCompilerDefines(ResetBuildTarget,
  ClearCaches, WaitTillDone, Quiet: boolean);

  procedure AddTemplate(ADefTempl: TDefineTemplate; AddToPool: boolean;
    const ErrorMsg: string);
  begin
    if ADefTempl = nil then
    begin
      DebugLn('');
      DebugLn(ErrorMsg);
    end else
    begin
      if AddToPool then
        CodeToolBoss.DefinePool.Add(ADefTempl.CreateCopy(false,true,true));
      CodeToolBoss.DefineTree.ReplaceRootSameName(ADefTempl);
    end;
  end;

  function FoundSystemPPU: boolean;
  var
    ConfigCache: TPCTargetConfigCache;
    AFilename: string;
  begin
    Result:=false;
    ConfigCache:=UnitSetCache.GetConfigCache(false);
    if ConfigCache=nil then exit;
    if ConfigCache.Units=nil then exit;
    AFilename:=ConfigCache.Units['system'];
    if AFilename='' then exit;
    if CompareFileExt(AFilename,'.ppu',false)<>0 then exit;
    Result:=true;
  end;

  function PPUFilesAndCompilerMatch: boolean;
  // check if compiler is in another directory than the ppu files
  // for example: a 'make install' installs to /usr/local/lib/fpc
  // while the rpm/deb packages install to /usr/lib
  var
    Cfg: TPCTargetConfigCache;
    Filename: String;
  begin
    Cfg:=UnitSetCache.GetConfigCache(false);
    if Cfg=nil then exit(true);
    if Cfg.Kind=pcFPC then begin
      if Cfg.RealCompiler='' then begin
        if ConsoleVerbosity>=0 then
          debugln(['Error: (lazarus) [PPUFilesAndCompilerMatch] Compiler=',Cfg.Compiler,' RealComp=',Cfg.RealCompiler,' InPath=',Cfg.RealCompilerInPath]);
        IDEMessageDialog(lisCCOErrorCaption, Format(
          lisCompilerDoesNotSupportTarget, [Cfg.Compiler, Cfg.TargetCPU, Cfg.TargetOS]),
          mtError,[mbOk]);
        exit(false);
      end;
      Filename:=GetPhysicalFilenameCached(Cfg.RealCompiler,true);
      if (Filename='') then begin
        IDEMessageDialog('Error','Compiler executable is missing: '+Cfg.RealCompiler,
          mtError,[mbOk]);
        exit(false);
      end;
    end;
    Result:=true;
  end;

var
  TargetOS, TargetCPU, FPCOptions: string;
  CompilerFilename: String;
  FPCSrcDir: string;
  ADefTempl: TDefineTemplate;
  FPCSrcCache: TFPCSourceCache;
  NeedUpdateFPCSrcCache: Boolean;
  IgnorePath: String;
  MsgResult: TModalResult;
  AsyncScanFPCSrcDir: String;
  UnitSetChanged: Boolean;
  HasTemplate: Boolean;
  CompilerErrorMsg: string;
  Msg: String;
  Kind: TPascalCompiler;
begin
  if ClearCaches then begin
    {$IFDEF VerboseFPCSrcScan}
    debugln(['TBuildManager.RescanCompilerDefines clear caches']);
    {$ENDIF}
    CodeToolBoss.CompilerDefinesCache.ConfigCaches.Clear;
    CodeToolBoss.CompilerDefinesCache.SourceCaches.Clear;
  end;
  if ResetBuildTarget then
    SetBuildTarget('','','',smsfsSkip,true);
  
  // start the compiler and ask for his settings
  // provide an english message file
  UpdateEnglishErrorMsgFilename;

  // use current TargetOS, TargetCPU, compilerfilename and FPC source dir
  TargetOS:=GetTargetOS;
  TargetCPU:=GetTargetCPU;
  {$IFDEF VerboseFPCSrcScan}
  debugln(['TBuildManager.RescanCompilerDefines GetParsedFPCSourceDirectory needs FPCVer...']);
  {$ENDIF}
  FPCSrcDir:=EnvironmentOptions.GetParsedFPCSourceDirectory; // needs FPCVer macro
  CompilerFilename:=GetCompilerFilename;
  FPCOptions:=GetFPCFrontEndOptions;

  {$IFDEF VerboseFPCSrcScan}
  debugln(['TMainIDE.RescanCompilerDefines START ',
    ' CompilerFilename=',CompilerFilename,
    ' TargetOS=',TargetOS,
    ' TargetCPU=',TargetCPU,
    ' FPCOptions="',FPCOptions,'"',
    ' EnvFPCSrcDir=',EnvironmentOptions.FPCSourceDirectory,
    ' FPCSrcDir=',FPCSrcDir,
    ' WaitTillDone=',WaitTillDone,
    ' Quiet=',Quiet,
    ' ClearCaches=',ClearCaches,
    '']);
  {$ENDIF}

  // first check the default targetos, targetcpu of the default compiler
  if FileExistsCached(EnvironmentOptions.GetParsedCompilerFilename) then
  begin
    {$IFDEF VerboseFPCSrcScan}
    debugln(['TBuildManager.RescanCompilerDefines reading default compiler settings']);
    {$ENDIF}
    UnitSetCache:=CodeToolBoss.CompilerDefinesCache.FindUnitSet(
      EnvironmentOptions.GetParsedCompilerFilename,'','','',FPCSrcDir,true);
    UnitSetCache.GetConfigCache(true);
  end;

  // then check the project's compiler
  if not IsCompilerExecutable(CompilerFilename,CompilerErrorMsg,Kind) then begin
    Msg:='';
    if (FBuildTarget<>nil)
    and ([crCompile,crBuild]*FBuildTarget.CompilerOptions.CompileReasons<>[])
    and (FBuildTarget.CompilerOptions.CompilerPath<>'')
    then begin
      CompilerFilename:=FBuildTarget.GetCompilerFilename;
      if not IsCompilerExecutable(CompilerFilename,CompilerErrorMsg,Kind) then begin
        Msg+='Project''s compiler: "'+CompilerFilename+'": '+CompilerErrorMsg+#13;
      end;
    end;

    CompilerFilename:=EnvironmentOptions.GetParsedCompilerFilename;
    if not IsFPCExecutable(CompilerFilename,CompilerErrorMsg) then begin
      Msg+='Environment compiler: "'+CompilerFilename+'": '+CompilerErrorMsg+#13;
    end;
    debugln('Warning: (lazarus) [TBuildManager.RescanCompilerDefines]: invalid compiler:');
    debugln(Msg);
    if not Quiet then begin
      IDEMessageDialog('Error','There is no Free Pascal Compiler'
        +' (e.g. fpc'+ExeExt+' or ppc<cpu>'+ExeExt+') configured in the'
        +' environment options. Codetools will not work properly.'#13
        +Msg,mtError,[mbCancel]);
    end;
    UnitSetCache:=nil;
    exit;
  end;

  // create a cache for the current project settings
  {$IFDEF VerboseFPCSrcScan}
  debugln(['TBuildManager.RescanCompilerDefines reading active compiler settings']);
  {$ENDIF}
  //debugln(['TBuildManager.RescanCompilerDefines ',CompilerFilename,' OS=',TargetOS,' CPU=',TargetCPU,' Options="',FPCOptions,'"']);
  UnitSetCache:=CodeToolBoss.CompilerDefinesCache.FindUnitSet(
    CompilerFilename,TargetOS,TargetCPU,FPCOptions,FPCSrcDir,true);

  NeedUpdateFPCSrcCache:=false;
  //debugln(['TBuildManager.RescanCompilerDefines ',DirectoryExistsUTF8(FPCSrcDir),' ',(not WaitTillDone),' ',(not HasGUI)]);
  AsyncScanFPCSrcDir:='';
  if DirectoryExistsUTF8(FPCSrcDir) and ((not WaitTillDone) or (not HasGUI)) then
  begin
    // FPC sources are not needed
    // => disable scan
    FPCSrcCache:=UnitSetCache.GetSourceCache(false);
    if (FPCSrcCache<>nil) and (not FPCSrcCache.Valid) then
    begin
      NeedUpdateFPCSrcCache:=HasGUI;
      FPCSrcCache.Valid:=true;
      if NeedUpdateFPCSrcCache then
      begin
        // start background scan of fpc source directory
        //debugln(['TBuildManager.RescanCompilerDefines background scan: '+FPCSrcCache.Directory]);
        AsyncScanFPCSrcDir:=FPCSrcDir;
      end;
    end;
  end;

  // scan compiler, fpc sources and create indices for quick lookup
  UnitSetCache.Init;

  UnitSetChanged:=(FUnitSetChangeStamp=TFPCUnitSetCache.GetInvalidChangeStamp)
               or (FUnitSetChangeStamp<>UnitSetCache.ChangeStamp);

  {$IFDEF VerboseFPCSrcScan}
  debugln(['TBuildManager.RescanCompilerDefines UnitSet changed=',UnitSetChanged,
    ' ClearCaches=',ClearCaches,
    ' CompilerFilename=',CompilerFilename,
    ' TargetOS=',TargetOS,
    ' TargetCPU=',TargetCPU,
    ' FPCOptions="',FPCOptions,'"',
    ' RealCompiler=',UnitSetCache.GetConfigCache(false).RealCompiler,
    ' EnvFPCSrcDir=',EnvironmentOptions.FPCSourceDirectory,
    ' FPCSrcDir=',FPCSrcDir,
    '']);
  {$ENDIF}

  if UnitSetChanged then begin
    {$IFDEF VerboseFPCSrcScan}
    debugln(['TBuildManager.RescanCompilerDefines UnitSet changed => save scan results']);
    {$ENDIF}
    // save caches
    SaveCompilerDefinesCaches;
    FUnitSetChangeStamp:=UnitSetCache.ChangeStamp;
  end;

  // rebuild the define templates
  HasTemplate:=CodeToolBoss.DefineTree.FindDefineTemplateByName(StdDefTemplFPC,true)<>nil;
  if UnitSetChanged or not HasTemplate then
  begin
    {$IFDEF VerboseFPCSrcScan}
    debugln(['TBuildManager.RescanCompilerDefines updating FPC template UnitSetChanged=',UnitSetChanged,' OldTemplateExists=',HasTemplate]);
    {$ENDIF}
    // create template for FPC settings
    ADefTempl:=CreateFPCTemplate(UnitSetCache,nil);
    AddTemplate(ADefTempl,false,
               'NOTE: Could not create Define Template for Free Pascal Compiler');
  end;

  // create template for FPC source directory
  if HasGUI then
  begin
    HasTemplate:=CodeToolBoss.DefineTree.FindDefineTemplateByName(StdDefTemplFPCSrc,true)<>nil;
    if UnitSetChanged or not HasTemplate then
    begin
      {$IFDEF VerboseFPCSrcScan}
      debugln(['TBuildManager.RescanCompilerDefines updating FPC SRC template UnitSetChanged=',UnitSetChanged,' OldTemplateExists=',HasTemplate]);
      {$ENDIF}
      ADefTempl:=CreateFPCSourceTemplate(UnitSetCache,nil);
      AddTemplate(ADefTempl,false,lisNOTECouldNotCreateDefineTemplateForFreePascal);
    end;

    // create compiler macros for the lazarus sources
    HasTemplate:=CodeToolBoss.DefineTree.FindDefineTemplateByName(StdDefTemplLazarusSources,true)<>nil;
    if (not HasTemplate)
    or (fBuildLazExtraOptions<>MiscellaneousOptions.BuildLazOpts.ExtraOptions)
    then begin
      {$IFDEF VerboseFPCSrcScan}
      debugln(['TBuildManager.RescanCompilerDefines updating Lazarus source template OldTemplateExists=',HasTemplate,' OldExtraOptions="',fBuildLazExtraOptions,'" NewExtraOptions="',MiscellaneousOptions.BuildLazOpts.ExtraOptions,'"']);
      {$ENDIF}
      fBuildLazExtraOptions:=MiscellaneousOptions.BuildLazOpts.ExtraOptions;
      ADefTempl:=CreateLazarusSourceTemplate(
        '$('+ExternalMacroStart+'LazarusDir)',
        '$('+ExternalMacroStart+'LCLWidgetType)',
        fBuildLazExtraOptions,nil);
      AddTemplate(ADefTempl,true,
        lisNOTECouldNotCreateDefineTemplateForLazarusSources);
    end;
  end;

  CodeToolBoss.DefineTree.ClearCache;

  if AsyncScanFPCSrcDir<>'' then begin
    // start scanning the fpc source directory in the background
    {$IFDEF VerboseFPCSrcScan}
    debugln(['TBuildManager.RescanCompilerDefines scanning fpc sources:',AsyncScanFPCSrcDir]);
    {$ENDIF}
    if FPCSrcScans=nil then
      FFPCSrcScans:=TFPCSrcScans.Create(Self);
    FPCSrcScans.Scan(AsyncScanFPCSrcDir);
  end;

  if not Quiet then begin
    // check for common installation mistakes
    if not PPUFilesAndCompilerMatch then exit;
    if (UnitSetCache.GetCompilerKind=pcFPC) then begin
      // check if at least one fpc config is there
      if (UnitSetCache.GetFirstFPCCfg='') then begin
        IgnorePath:='MissingFPCCfg_'+TargetOS+'-'+TargetCPU;
        if (InputHistories<>nil) and (InputHistories.Ignores.Find(IgnorePath)=nil)
        then begin
          MsgResult:=IDEMessageDialog(lisCCOWarningCaption,
            lisTheCurrentFPCHasNoConfigFileItWillProbablyMissSome,
            mtWarning,[mbOk,mbIgnore]);
          if MsgResult=mrIgnore then
            InputHistories.Ignores.Add(IgnorePath,iiidIDERestart);
        end;
      end;
      if not FoundSystemPPU then begin
        // system.ppu is missing
        IDEMessageDialog(lisCCOErrorCaption,
          Format(lisTheProjectUsesTargetOSAndCPUTheSystemPpuForThisTar,
                 [TargetOS, TargetCPU, LineEnding, LineEnding]),
          mtError,[mbOk]);
      end;
    end;
  end;
end;

function TBuildManager.CompilerOnDiskChanged: boolean;
var
  CfgCache: TPCTargetConfigCache;
begin
  Result:=false;
  if UnitSetCache=nil then exit;
  CfgCache:=UnitSetCache.GetConfigCache(false);
  if CfgCache=nil then exit;
  Result:=CfgCache.NeedsUpdate;
end;

procedure TBuildManager.LoadCompilerDefinesCaches;
var
  aFilename: String;
  XMLConfig: TXMLConfig;
begin
  aFilename:=AppendPathDelim(GetPrimaryConfigPath)+'fpcdefines.xml';
  CopySecondaryConfigFile(ExtractFilename(aFilename));
  if not FileExistsUTF8(aFilename) then exit;
  try
    XMLConfig:=TXMLConfig.Create(aFilename);
    try
      CodeToolBoss.CompilerDefinesCache.LoadFromXMLConfig(XMLConfig,'');
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      if ConsoleVerbosity>=0 then
        debugln(['Error: (lazarus) [LoadCompilerDefinesCaches] Error reading file '+aFilename+':'+E.Message]);
    end;
  end;
end;

procedure TBuildManager.SaveCompilerDefinesCaches;
var
  aFilename: String;
  XMLConfig: TXMLConfig;
begin
  aFilename:=AppendPathDelim(GetPrimaryConfigPath)+'fpcdefines.xml';
  //debugln(['TBuildManager.SaveCompilerDefinesCaches check if save needed ...']);
  if FileExistsCached(aFilename)
  and (not CodeToolBoss.CompilerDefinesCache.NeedsSave) then
    exit;
  //debugln(['TBuildManager.SaveCompilerDefinesCaches saving ...']);
  try
    XMLConfig:=TXMLConfig.CreateClean(aFilename);
    try
      CodeToolBoss.CompilerDefinesCache.SaveToXMLConfig(XMLConfig,'');
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      if ConsoleVerbosity>=0 then
        debugln(['Error: (lazarus) [SaveCompilerDefinesCaches] Error writing file '+aFilename+':'+E.Message]);
    end;
  end;
end;

function TBuildManager.DoCheckIfProjectNeedsCompilation(AProject: TProject;
  out NeedBuildAllFlag: boolean; var Note: string): TModalResult;
var
  CompilerFilename, CompilerParams, SrcFilename: string;
  StateFilename: String;
  StateFileAge: LongInt;
  AnUnitInfo: TUnitInfo;
  LFMFilename: String;
  IcoRes: TProjectIcon;
  aTargetFilename: String;
  DbgCap: String;

  function EditorFileHasChanged: boolean;
  begin
    Result:=false;
    if AnUnitInfo.IsPartOfProject or AnUnitInfo.IsVirtual then exit;
    if not FileExistsCached(AnUnitInfo.Filename) then exit;
    if StateFileAge>=FileAgeCached(AnUnitInfo.Filename) then exit;
    if FilenameIsPascalUnit(AnUnitInfo.Filename) then
    begin
      if (SearchDirectoryInSearchPath(AProject.CompilerOptions.GetUnitPath(false),
                                ExtractFilePath(AnUnitInfo.Filename))>0)
      then begin
        Result:=true;
        if ConsoleVerbosity>=0 then
          DebugLn(DbgCap,'Editor Unit in project''s unit path has changed ',AProject.IDAsString,' ',AnUnitInfo.Filename);
        Note+='Editor unit "'+AnUnitInfo.Filename+'" in project''s unit search path is newer than state file:'+LineEnding
          +'  File age="'+FileAgeToStr(FileAgeCached(AnUnitInfo.Filename))+'"'+LineEnding
          +'  State file age="'+FileAgeToStr(StateFileAge)+'"'+LineEnding
          +'  State file='+StateFilename+LineEnding;
        exit(true);
      end;
    end;
    if (SearchDirectoryInSearchPath(AProject.CompilerOptions.GetIncludePath(false),
                              ExtractFilePath(AnUnitInfo.Filename))>0)
    then begin
      Result:=true;
      if ConsoleVerbosity>=0 then
        DebugLn(DbgCap,'Editor Src in project''s include path has changed ',AProject.IDAsString,' ',AnUnitInfo.Filename);
      Note+='Editor file "'+AnUnitInfo.Filename+'" in project''s include search path is newer than state file:'+LineEnding
        +'  File age="'+FileAgeToStr(FileAgeCached(AnUnitInfo.Filename))+'"'+LineEnding
        +'  State file age="'+FileAgeToStr(StateFileAge)+'"'+LineEnding
        +'  State file='+StateFilename+LineEnding;
      exit(true);
    end;
  end;

begin
  NeedBuildAllFlag:=false;
  DbgCap:='Hint: (lazarus) Project needs building: ';

  // get main source filename
  if not AProject.IsVirtual then begin
    SrcFilename:=CreateRelativePath(AProject.MainUnitInfo.Filename,
                                    AProject.Directory);
  end else begin
    SrcFilename:=GetTestUnitFilename(AProject.MainUnitInfo);
  end;

  CompilerFilename:=AProject.GetCompilerFilename;
  //DebugLn([DbgCap,'CompilerFilename="',CompilerFilename,'" CompilerPath="',AProject.CompilerOptions.CompilerPath,'"']);
  // Note: use absolute paths, because some external tools resolve symlinked directories
  CompilerParams :=
    AProject.CompilerOptions.MakeOptionsString([ccloAbsolutePaths])
           + ' ' + PrepareCmdLineOption(SrcFilename);
  //DebugLn(DbgCap,'WorkingDir="',WorkingDir,'" SrcFilename="',SrcFilename,'" CompilerFilename="',CompilerFilename,'" CompilerParams="',CompilerParams,'"');

  // check state file
  StateFilename:=AProject.GetStateFilename;
  Result:=AProject.LoadStateFile(false);
  if Result<>mrOk then exit; // read error and user aborted
  if not (lpsfStateFileLoaded in AProject.StateFlags) then begin
    if ConsoleVerbosity>=0 then
      DebugLn(DbgCap,'No state file for ',AProject.IDAsString);
    Note+='State file "'+StateFilename+'" of '+AProject.IDAsString+' is missing.'+LineEnding;
    NeedBuildAllFlag:=true;
    exit(mrYes);
  end;

  // check if build all (-B) is needed
  if (AProject.LastCompilerFilename<>CompilerFilename)
  or (ExtractFPCParamsForBuildAll(AProject.LastCompilerParams)
     <>ExtractFPCParamsForBuildAll(CompilerParams))
  or ((AProject.LastCompilerFileDate>0)
      and FileExistsCached(CompilerFilename)
      and (FileAgeCached(CompilerFilename)<>AProject.LastCompilerFileDate))
  then
    NeedBuildAllFlag:=true;

  StateFileAge:=FileAgeCached(StateFilename);

  // check main source file
  if FileExistsCached(SrcFilename) and (StateFileAge<FileAgeCached(SrcFilename)) then
  begin
    if ConsoleVerbosity>=0 then
      DebugLn(DbgCap,'SrcFile outdated ',AProject.IDAsString);
    Note+='Source file "'+SrcFilename+'" of '+AProject.IDAsString+' outdated:'+LineEnding
      +'  Source age='+FileAgeToStr(FileAgeCached(SrcFilename))+LineEnding
      +'  State file age='+FileAgeToStr(StateFileAge)+LineEnding
      +'  State file='+StateFilename+LineEnding;
    exit(mrYes);
  end;

  // check compiler and params
  if CompilerFilename<>AProject.LastCompilerFilename then begin
    if ConsoleVerbosity>=0 then begin
      DebugLn(DbgCap,'Compiler filename changed for ',AProject.IDAsString);
      DebugLn('  Old="',AProject.LastCompilerFilename,'"');
      DebugLn('  Now="',CompilerFilename,'"');
    end;
    Note+='Compiler filename changed for '+AProject.IDAsString+':'+LineEnding
      +'  Old="'+AProject.LastCompilerFilename+'"'+LineEnding
      +'  Now="'+CompilerFilename+'"'+LineEnding
      +'  State file='+StateFilename+LineEnding;
    exit(mrYes);
  end;
  if not FileExistsCached(CompilerFilename) then begin
    if ConsoleVerbosity>=0 then begin
      DebugLn(DbgCap,'Compiler file not found for ',AProject.IDAsString);
      DebugLn('  File="',CompilerFilename,'"');
    end;
    Note+='Compiler file "'+CompilerFilename+'" not found for '+AProject.IDAsString+'.'+LineEnding;
    exit(mrYes);
  end;
  if FileAgeCached(CompilerFilename)<>AProject.LastCompilerFileDate then begin
    if ConsoleVerbosity>=0 then begin
      DebugLn(DbgCap,'Compiler file changed for ',AProject.IDAsString);
      DebugLn('  File="',CompilerFilename,'"');
    end;
    Note+='Compiler file "'+CompilerFilename+'" for '+AProject.IDAsString+' changed:'+LineEnding
      +'  Old="'+FileAgeToStr(AProject.LastCompilerFileDate)+'"'+LineEnding
      +'  Now="'+FileAgeToStr(FileAgeCached(CompilerFilename))+'"'+LineEnding
      +'  State file='+StateFilename+LineEnding;
    exit(mrYes);
  end;
  if CompilerParams<>AProject.LastCompilerParams then begin
    if ConsoleVerbosity>=0 then begin
      DebugLn(DbgCap,'Compiler params changed for ',AProject.IDAsString);
      DebugLn('  Old="',AProject.LastCompilerParams,'"');
      DebugLn('  Now="',CompilerParams,'"');
    end;
    Note+='Compiler params changed for '+AProject.IDAsString+':'+LineEnding
      +'  Old="'+AProject.LastCompilerParams+'"'+LineEnding
      +'  Now="'+CompilerParams+'"'+LineEnding
      +'  State file='+StateFilename+LineEnding;
    exit(mrYes);
  end;

  // compiler and parameters are the same
  // => it is possible to quick compile without -B
  NeedBuildAllFlag:=false;

  if not AProject.LastCompileComplete then begin
    if ConsoleVerbosity>=0 then
      DebugLn(DbgCap,'Compile was incomplete for ',AProject.IDAsString);
    Note+='Last compile was incomplete.'+LineEnding
      +'  State file='+StateFilename+LineEnding;
    exit(mrYes);
  end;

  // check all required packages
  Result:=PackageGraph.CheckCompileNeedDueToDependencies(AProject,
                                AProject.FirstRequiredDependency,
                                not (pfUseDesignTimePackages in AProject.Flags),
                                StateFileAge,Note);
  if Result<>mrNo then exit;

  // check project files
  AnUnitInfo:=AProject.FirstPartOfProject;
  while AnUnitInfo<>nil do begin
    if (not AnUnitInfo.IsVirtual) and FileExistsCached(AnUnitInfo.Filename) then
    begin
      if (StateFileAge<FileAgeCached(AnUnitInfo.Filename)) then begin
        if ConsoleVerbosity>=0 then
          DebugLn(DbgCap,'Src has changed ',AProject.IDAsString,' ',AnUnitInfo.Filename);
        Note+='File "'+AnUnitInfo.Filename+'" of '+AProject.IDAsString+' is newer than state file:'+LineEnding
          +'  File age="'+FileAgeToStr(FileAgeCached(AnUnitInfo.Filename))+'"'+LineEnding
          +'  State file age="'+FileAgeToStr(StateFileAge)+'"'+LineEnding
          +'  State file='+StateFilename+LineEnding;
        exit(mrYes);
      end;
      if AnUnitInfo.ComponentName<>'' then begin
        LFMFilename:=ChangeFileExt(AnUnitInfo.Filename,'.lfm');
        if FileExistsCached(LFMFilename)
        and (StateFileAge<FileAgeCached(LFMFilename)) then begin
          if ConsoleVerbosity>=0 then
            DebugLn(DbgCap,'LFM has changed ',AProject.IDAsString,' ',LFMFilename);
          Note+='File "'+LFMFilename+'" of '+AProject.IDAsString+' is newer than state file:'+LineEnding
            +'  File age="'+FileAgeToStr(FileAgeCached(LFMFilename))+'"'+LineEnding
            +'  State file age="'+FileAgeToStr(StateFileAge)+'"'+LineEnding
            +'  State file='+StateFilename+LineEnding;
          exit(mrYes);
        end;
      end;
    end;
    AnUnitInfo:=AnUnitInfo.NextPartOfProject;
  end;

  // check all open editor files in unit/include path (maybe the user forgot
  // to add them to the project)
  AnUnitInfo:=AProject.FirstUnitWithEditorIndex;
  while AnUnitInfo<>nil do begin
    if EditorFileHasChanged then
      exit(mrYes);
    AnUnitInfo:=AnUnitInfo.NextUnitWithEditorIndex;
  end;

  // check project resources
  IcoRes:=TProjectIcon(AProject.ProjResources[TProjectIcon]);
  if (IcoRes<>nil) and (not IcoRes.IsEmpty)
  and FilenameIsAbsolute(IcoRes.IcoFileName)
  and FileExistsCached(IcoRes.IcoFileName)
  and (StateFileAge<FileAgeCached(IcoRes.IcoFileName)) then begin
    if ConsoleVerbosity>=0 then
      debugln([DbgCap,'icon has changed ',
        AProject.IDAsString,' "',IcoRes.IcoFileName,'"']);
    Note+='Project''s ico file "'+IcoRes.IcoFileName+'" is newer than state file:'+LineEnding
      +'  File age="'+FileAgeToStr(FileAgeCached(IcoRes.IcoFileName))+'"'+LineEnding
      +'  State file age="'+FileAgeToStr(StateFileAge)+'"'+LineEnding
      +'  State file='+StateFilename+LineEnding;
    exit(mrYes);
  end;

  // check target file
  aTargetFilename:=AProject.CompilerOptions.CreateTargetFilename;
  //debugln(['TBuildManager.DoCheckIfProjectNeedsCompilation aTargetFilename=',aTargetFilename]);
  if (aTargetFilename<>'') and not FileExistsCached(aTargetFilename) then begin
    if ConsoleVerbosity>=0 then
      debugln([DbgCap,'missing target file "',aTargetFilename,'"']);
    Note+='Project''s target file "'+aTargetFilename+'" is missing.';
    exit(mrYes);
  end;

  if not HasGUI then
    debugln(['Hint: (lazarus) Build Project: nothing to do.']);
  Result:=mrNo;
end;

procedure TBuildManager.DoOnRescanFPCDirectoryCache(Sender: TObject);
var
  Files: TStringList;
  FPCSrcDir: string;
begin
  FPCSrcDir := EnvironmentOptions.GetParsedFPCSourceDirectory;
  Files := GatherFilesInFPCSources(FPCSrcDir, nil);
  if Files<>nil then
    try
      ApplyFPCSrcFiles(FPCSrcDir, Files);
    finally
      Files.Free;
    end;
end;

function TBuildManager.CheckAmbiguousSources(const AFilename: string;
  Compiling: boolean): TModalResult;

  function DeleteAmbiguousFile(const AmbiguousFilename: string): TModalResult;
  begin
    if not DeleteFileUTF8(AmbiguousFilename) then begin
      Result:=IDEMessageDialog(lisErrorDeletingFile,
       Format(lisUnableToDeleteAmbiguousFile, [AmbiguousFilename]),
       mtError,[mbOk,mbAbort]);
    end else
      Result:=mrOk;
  end;

  function RenameAmbiguousFile(const AmbiguousFilename: string): TModalResult;
  var
    NewFilename: string;
  begin
    NewFilename:=AmbiguousFilename+'.ambiguous';
    if not RenameFileUTF8(AmbiguousFilename,NewFilename) then
    begin
      Result:=IDEMessageDialog(lisErrorRenamingFile,
       Format(lisUnableToRenameAmbiguousFileTo,[AmbiguousFilename,LineEnding,NewFilename]),
       mtError,[mbOk,mbAbort]);
    end else
      Result:=mrOk;
  end;

  function AddCompileWarning(const AmbiguousFilename: string): TModalResult;
  begin
    Result:=mrOk;
    if Compiling then begin
      IDEMessagesWindow.AddCustomMessage(mluError,
        Format('ambiguous file found: "%s". Source file is: "%s"',
               [AmbiguousFilename, AFilename]));
    end;
  end;

  function CheckFile(const AmbiguousFilename: string): TModalResult;
  begin
    Result:=mrOk;
    if CompareFilenames(AFilename,AmbiguousFilename)=0 then exit;
    if not FileExistsUTF8(AmbiguousFilename) then exit;
    if Compiling then begin
      Result:=AddCompileWarning(AmbiguousFilename);
      exit;
    end;
    case EnvironmentOptions.AmbiguousFileAction of
    afaAsk:
      begin
        Result:=IDEMessageDialog(lisAmbiguousFileFound,
          Format(lisThereIsAFileWithTheSameNameAndASimilarExtension,
                 [LineEnding, AFilename, LineEnding, AmbiguousFilename, LineEnding+LineEnding]),
          mtWarning,[mbYes,mbIgnore,mbAbort]);
        case Result of
        mrYes:    Result:=DeleteAmbiguousFile(AmbiguousFilename);
        mrIgnore: Result:=mrOk;
        end;
      end;

    afaAutoDelete:
      Result:=DeleteAmbiguousFile(AmbiguousFilename);

    afaAutoRename:
      Result:=RenameAmbiguousFile(AmbiguousFilename);

    afaWarnOnCompile:
      Result:=AddCompileWarning(AmbiguousFilename);

    else
      Result:=mrOk;
    end;
  end;

var
  Ext, LowExt: string;
  i: integer;
begin
  Result:=mrOk;
  if EnvironmentOptions.AmbiguousFileAction=afaIgnore then exit;
  if (EnvironmentOptions.AmbiguousFileAction=afaWarnOnCompile)
  and not Compiling then exit;

  if FilenameIsPascalUnit(AFilename) then begin
    Ext:=ExtractFileExt(AFilename);
    LowExt:=lowercase(Ext);
    for i:=Low(PascalFileExt) to High(PascalFileExt) do begin
      if LowExt<>PascalFileExt[i] then begin
        Result:=CheckFile(ChangeFileExt(AFilename,PascalFileExt[i]));
        if Result<>mrOk then exit;
      end;
    end;
  end;
end;

function TBuildManager.DeleteAmbiguousFiles(const Filename: string): TModalResult;
var
  ADirectory: String;
  FileInfo: TSearchRec;
  ShortFilename: String;
  CurFilename: String;
  IsPascalUnit: Boolean;
  AUnitName: String;
begin
  if EnvironmentOptions.AmbiguousFileAction=afaIgnore then exit(mrOK);
  if EnvironmentOptions.AmbiguousFileAction in [afaAsk,afaAutoDelete,afaAutoRename]
  then begin
    ADirectory:=AppendPathDelim(ExtractFilePath(Filename));
    if FindFirstUTF8(ADirectory+GetAllFilesMask,faAnyFile,FileInfo)=0 then
    begin
      ShortFilename:=ExtractFileName(Filename);
      IsPascalUnit:=FilenameIsPascalUnit(ShortFilename);
      AUnitName:=ExtractFilenameOnly(ShortFilename);
      repeat
        if (FileInfo.Name='.') or (FileInfo.Name='..')
        or (FileInfo.Name='')
        or ((FileInfo.Attr and faDirectory)<>0) then continue;
        if CompareFilenames(ShortFilename,FileInfo.Name)=0 then continue;

        if (SysUtils.CompareText(ShortFilename,FileInfo.Name)=0)
        then begin
          // same name different case => ambiguous
        end else if IsPascalUnit and FilenameIsPascalUnit(FileInfo.Name)
           and (SysUtils.CompareText(AUnitName,ExtractFilenameOnly(FileInfo.Name))=0)
        then begin
          // same unit name => ambiguous
        end else
          continue;

        CurFilename:=ADirectory+FileInfo.Name;
        if EnvironmentOptions.AmbiguousFileAction=afaAsk then begin
          if IDEMessageDialog(lisDeleteAmbiguousFile,
            Format(lisAmbiguousFileFoundThisFileCanBeMistakenWithDelete,
                   [CurFilename, LineEnding, ShortFilename, LineEnding+LineEnding]),
            mtConfirmation,[mbYes,mbNo])=mrNo
          then continue;
        end;
        if EnvironmentOptions.AmbiguousFileAction in [afaAutoDelete,afaAsk]
        then begin
          Result:=DeleteFileInteractive(CurFilename);
          if not (Result in [mrOK,mrIgnore]) then exit(mrCancel);
        end else if EnvironmentOptions.AmbiguousFileAction=afaAutoRename then
        begin
          Result:=BackupFileForWrite(CurFilename);
          if not (Result in [mrOK,mrIgnore]) then exit(mrCancel);
          if FileExistsUTF8(CurFilename) then begin
            Result:=DeleteFileInteractive(CurFilename);
            if not (Result in [mrOK,mrIgnore]) then exit(mrCancel);
          end;
        end;
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);
  end;
  Result:=mrOk;
end;

{-------------------------------------------------------------------------------
  function TBuildManager.CheckUnitPathForAmbiguousPascalFiles(
    const BaseDir, TheUnitPath, CompiledExt, ContextDescription: string
    ): TModalResult;

  Collect all pascal files and all compiled units in the unit path and check
  for ambiguous files. For example: doubles.
-------------------------------------------------------------------------------}
function TBuildManager.CheckUnitPathForAmbiguousPascalFiles(const BaseDir,
  TheUnitPath, CompiledExt, ContextDescription: string): TModalResult;

  procedure FreeUnitTree(var Tree: TAVLTree);
  var
    ANode: TAVLTreeNode;
    AnUnitFile: PUnitFile;
  begin
    if Tree<>nil then begin
      ANode:=Tree.FindLowest;
      while ANode<>nil do begin
        AnUnitFile:=PUnitFile(ANode.Data);
        Dispose(AnUnitFile);
        ANode:=Tree.FindSuccessor(ANode);
      end;
      Tree.Free;
      Tree:=nil;
    end;
  end;

var
  EndPos: Integer;
  StartPos: Integer;
  CurDir: String;
  FileInfo: TSearchRec;
  SourceUnitTree, CompiledUnitTree: TAVLTree;
  ANode: TAVLTreeNode;
  CurUnitName: String;
  CurFilename: String;
  AnUnitFile: PUnitFile;
  CurUnitTree: TAVLTree;
  FileInfoNeedClose: Boolean;
  UnitPath: String;
  IgnoreAll: Boolean;
begin
  Result:=mrOk;
  UnitPath:=TrimSearchPath(TheUnitPath,BaseDir,true);

  SourceUnitTree:=TAVLTree.Create(TListSortCompare(@CompareUnitFiles));
  CompiledUnitTree:=TAVLTree.Create(TListSortCompare(@CompareUnitFiles));
  FileInfoNeedClose:=false;
  try
    // collect all units (.pas, .pp, compiled units)
    EndPos:=1;
    while EndPos<=length(UnitPath) do begin
      StartPos:=EndPos;
      while (StartPos<=length(UnitPath)) and (UnitPath[StartPos]=';') do
        inc(StartPos);
      EndPos:=StartPos;
      while (EndPos<=length(UnitPath)) and (UnitPath[EndPos]<>';') do
        inc(EndPos);
      if EndPos>StartPos then begin
        CurDir:=AppendPathDelim(TrimFilename(copy(UnitPath,StartPos,EndPos-StartPos)));
        FileInfoNeedClose:=true;
        if FindFirstUTF8(CurDir+GetAllFilesMask,faAnyFile,FileInfo)=0 then begin
          IgnoreAll:=false;
          repeat
            if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
            or ((FileInfo.Attr and faDirectory)<>0) then continue;
            if FilenameIsPascalUnit(FileInfo.Name) then
              CurUnitTree:=SourceUnitTree
            else if (CompareFileExt(FileInfo.Name,CompiledExt,false)=0) then
              CurUnitTree:=CompiledUnitTree
            else
              continue;
            CurUnitName:=ExtractFilenameOnly(FileInfo.Name);
            if not LazIsValidIdent(CurUnitName) then
              continue;
            CurFilename:=CurDir+FileInfo.Name;
            //DebugLn(['TBuildManager.CheckUnitPathForAmbiguousPascalFiles ',CurUnitName,' ',CurFilename]);
            // check if unit already found
            ANode:=CurUnitTree.FindKey(PChar(CurUnitName),
                                 TListSortCompare(@CompareUnitNameAndUnitFile));
            if (ANode<>nil) and (not IgnoreAll) then begin
              if ConsoleVerbosity>=0 then
                DebugLn(['Note: (lazarus) [TBuildManager.CheckUnitPathForAmbiguousPascalFiles] CurUnitName="',CurUnitName,'" CurFilename="',CurFilename,'" OtherUnitName="',PUnitFile(ANode.Data)^.FileUnitName,'" OtherFilename="',PUnitFile(ANode.Data)^.Filename,'"']);
              // pascal unit exists twice
              Result:=IDEQuestionDialog(lisAmbiguousUnitFound,
                Format(lisTheUnitExistsTwiceInTheUnitPathOfThe,[CurUnitName,ContextDescription])
                +LineEnding
                +LineEnding
                +'1. "'+PUnitFile(ANode.Data)^.Filename+'"'+LineEnding
                +'2. "'+CurFilename+'"'+LineEnding
                +LineEnding
                +lisHintCheckIfTwoPackagesContainAUnitWithTheSameName,
                mtWarning, [mrIgnore,
                            mrYesToAll, lisIgnoreAll,
                            mrAbort]);
              case Result of
              mrIgnore: ;
              mrYesToAll: IgnoreAll:=true;
              else exit;
              end;
            end;
            // add unit to tree
            New(AnUnitFile);
            AnUnitFile^.FileUnitName:=CurUnitName;
            AnUnitFile^.Filename:=CurFilename;
            CurUnitTree.Add(AnUnitFile);
          until FindNextUTF8(FileInfo)<>0;
        end;
        FindCloseUTF8(FileInfo);
        FileInfoNeedClose:=false;
      end;
    end;
  finally
    // clean up
    if FileInfoNeedClose then FindCloseUTF8(FileInfo);
    FreeUnitTree(SourceUnitTree);
    FreeUnitTree(CompiledUnitTree);
  end;
  Result:=mrOk;
end;

function TBuildManager.CreateProjectApplicationBundle: Boolean;
var
  TargetExeName: string;
begin
  Result := False;
  if Project1.MainUnitInfo = nil then
    Exit;
  TargetExeName := Project1.CompilerOptions.CreateTargetFilename;

  if not (CreateApplicationBundle(TargetExeName, Project1.GetTitle, True) in
    [mrOk, mrIgnore]) then
    Exit;
  if not (CreateAppBundleSymbolicLink(TargetExeName, True) in [mrOk, mrIgnore]) then
    Exit;
  Result := True;
end;

function TBuildManager.BackupFileForWrite(const Filename: string): TModalResult;
var BackupFilename, CounterFilename: string;
  AText,ACaption:string;
  BackupInfo: TBackupInfo;
  FilePath, FileNameOnly, FileExt, SubDir: string;
  i: integer;
  IsPartOfProject: boolean;
begin
  Result:=mrOk;
  SubDir:='';
  BackupFilename:='';
  if not (FileExistsUTF8(Filename)) then exit;
  // check if file in lpi
  IsPartOfProject:=(Project1<>nil)
                  and (Project1.FindFile(Filename,[pfsfOnlyProjectFiles])<>nil);
  // check if file in source directory of project
  if (not IsPartOfProject) and (Project1<>nil)
    and (SearchDirectoryInSearchPath(ExtractFilePath(Filename),
         Project1.SourceDirectories.CreateSearchPathFromAllFiles)>0)
  then
    IsPartOfProject:=true;
  // check options
  if IsPartOfProject then
    BackupInfo:=EnvironmentOptions.BackupInfoProjectFiles
  else
    BackupInfo:=EnvironmentOptions.BackupInfoOtherFiles;
  if (BackupInfo.BackupType=bakNone)
  or ((BackupInfo.BackupType=bakSameName) and (BackupInfo.SubDirectory='')) then
    exit;
  // create backup
  FilePath:=ExtractFilePath(Filename);
  FileExt:=ExtractFileExt(Filename);
  FileNameOnly:=ExtractFilenameOnly(Filename);
  SubDir:=BackupInfo.SubDirectory;
  if BackupInfo.SubDirectory<>'' then
    GlobalMacroList.SubstituteStr(SubDir);
  if SubDir<>'' then begin
    if not FilenameIsAbsolute(SubDir) then
      SubDir:=TrimFilename(FilePath+SubDir);
    Result:=ForceDirectoryInteractive(SubDir,[mbRetry,mbIgnore]);
    if Result=mrCancel then exit;
    if Result=mrIgnore then Result:=mrOk;
  end;
  if BackupInfo.BackupType in
     [bakSymbolInFront,bakSymbolBehind,bakUserDefinedAddExt,bakSameName] then
  begin
    case BackupInfo.BackupType of
      bakSymbolInFront:
        BackupFilename:=FileNameOnly+'.~'+copy(FileExt,2,length(FileExt)-1);
      bakSymbolBehind:
        BackupFilename:=FileNameOnly+FileExt+'~';
      bakUserDefinedAddExt:
        BackupFilename:=FileNameOnly+FileExt+'.'+BackupInfo.AdditionalExtension;
      bakSameName:
        BackupFilename:=FileNameOnly+FileExt;
    end;
    if BackupInfo.SubDirectory<>'' then
      BackupFilename:=AppendPathDelim(SubDir)+BackupFilename
    else
      BackupFilename:=FilePath+BackupFilename;
    // remove old backup file
    repeat
      if FileExistsUTF8(BackupFilename) then begin
        if not DeleteFileUTF8(BackupFilename) then begin
          ACaption:=lisDeleteFileFailed;
          AText:=Format(lisUnableToRemoveOldBackupFile,[BackupFilename]);
          Result:=IDEMessageDialog(ACaption,AText,mtError,[mbAbort,mbRetry,mbIgnore]);
          if Result=mrAbort then exit;
          if Result=mrIgnore then Result:=mrOk;
        end;
      end;
    until Result<>mrRetry;
  end else begin
    // backup with counter
    if BackupInfo.SubDirectory<>'' then
      BackupFilename:=AppendPathDelim(SubDir)+FileNameOnly+FileExt+';'
    else
      BackupFilename:=Filename+';';
    if BackupInfo.MaxCounter<=0 then begin
      // search first non existing backup filename
      i:=1;
      while FileExistsUTF8(BackupFilename+IntToStr(i)) do inc(i);
      BackupFilename:=BackupFilename+IntToStr(i);
    end else begin
      // rename all backup files (increase number)
      i:=1;
      while FileExistsUTF8(BackupFilename+IntToStr(i))
      and (i<=BackupInfo.MaxCounter) do inc(i);
      if i>BackupInfo.MaxCounter then begin
        dec(i);
        CounterFilename:=BackupFilename+IntToStr(BackupInfo.MaxCounter);
        // remove old backup file
        repeat
          if FileExistsUTF8(CounterFilename) then begin
            if not DeleteFileUTF8(CounterFilename) then begin
              ACaption:=lisDeleteFileFailed;
              AText:=Format(lisUnableToRemoveOldBackupFile,[CounterFilename]);
              Result:=IDEMessageDialog(ACaption,AText,mtError,[mbAbort,mbRetry,mbIgnore]);
              if Result=mrAbort then exit;
              if Result=mrIgnore then Result:=mrOk;
            end;
          end;
        until Result<>mrRetry;
      end;
      // rename all old backup files
      dec(i);
      while i>=1 do begin
        repeat
          if not RenameFileUTF8(BackupFilename+IntToStr(i),
                                BackupFilename+IntToStr(i+1)) then
          begin
            ACaption:=lisRenameFileFailed;
            AText:=Format(lisUnableToRenameFileTo,
                     [BackupFilename+IntToStr(i), BackupFilename+IntToStr(i+1)]);
            Result:=IDEMessageDialog(ACaption,AText,mtError,
                               [mbAbort,mbRetry,mbIgnore]);
            if Result=mrAbort then exit;
            if Result=mrIgnore then Result:=mrOk;
          end;
        until Result<>mrRetry;
        dec(i);
      end;
      BackupFilename:=BackupFilename+'1';
    end;
  end;
  // backup file
  repeat
    if not IDEProcs.BackupFileForWrite(Filename, BackupFilename) then
    begin
      ACaption := lisBackupFileFailed;
      AText := Format(lisUnableToBackupFileTo, [Filename, BackupFilename]);
      Result := IDEMessageDialog(ACaption,AText,mterror,[mbabort,mbretry,mbignore]);
      if Result = mrAbort then exit;
      if Result = mrIgnore then Result := mrOk;
    end
    else
      Result := mrOk;
  until Result <> mrRetry;
end;

function TBuildManager.GetResourceType(AnUnitInfo: TUnitInfo): TResourceType;
begin
  if AnUnitInfo.Source = nil then
    AnUnitInfo.Source := CodeToolBoss.LoadFile(AnUnitInfo.Filename, True, False);
  if (AnUnitInfo.Source <> nil) and GuessResourceType(AnUnitInfo.Source, Result) then
  begin
    // guessed from source
  end
  else
  if AnUnitInfo.IsPartOfProject then
  begin
    // use project resource type
    Result := AnUnitInfo.Project.ProjResources.ResourceType;
  end
  else
    Result := rtLRS;
end;

function TBuildManager.FindLRSFilename(AnUnitInfo: TUnitInfo;
  UseDefaultIfNotFound: boolean): string;
begin
  if AnUnitInfo.IsVirtual then begin
    Result:='';
  end else begin
    Result:=ExtractFileNameOnly(AnUnitInfo.Filename)+ResourceFileExt;
    Result:=FileUtil.SearchFileInPath(Result,'',
        CodeToolBoss.GetIncludePathForDirectory(ExtractFilePath(AnUnitInfo.Filename)),
        ';',[sffDontSearchInBasePath,sffSearchLoUpCase]);
  end;
  if (Result='') and UseDefaultIfNotFound then
    Result:=GetDefaultLRSFilename(AnUnitInfo);
end;

function TBuildManager.GetDefaultLRSFilename(AnUnitInfo: TUnitInfo): string;
var
  OutputDir: String;
begin
  if AnUnitInfo.IsPartOfProject
  and (not AnUnitInfo.Project.IsVirtual)
  and (pfLRSFilesInOutputDirectory in Project1.Flags) then begin
    OutputDir:=AnUnitInfo.Project.GetOutputDirectory;
    if OutputDir<>'' then begin
      Result:=AppendPathDelim(OutputDir)
              +ExtractFileNameOnly(AnUnitInfo.Filename)+ResourceFileExt;
      exit;
    end;
  end;
  Result:=ChangeFileExt(AnUnitInfo.Filename,ResourceFileExt);
end;

function TBuildManager.UpdateLRSFromLFM(AnUnitInfo: TUnitInfo;
  ShowAbort: boolean): TModalResult;
var
  LFMFilename: String;
  LRSFilename: String;
  Dir: String;
begin
  Result:=mrOk;
  // check if there is a .lfm file
  if AnUnitInfo.IsVirtual then exit;
  LFMFilename:=ChangeFileExt(AnUnitInfo.Filename,'.lfm');
  if not FileExistsCached(LFMFilename) then exit(mrOk);
  // check if there is a .lrs file
  LRSFilename:=FindLRSFilename(AnUnitInfo,true);
  if LRSFilename=LFMFilename then exit;
  // check if .lrs file is newer than .lfm file
  if FileExistsUTF8(LRSFilename)
  and (FileAgeUTF8(LFMFilename)<=FileAgeUTF8(LRSFilename))
  then exit;
  // the .lrs file does not exist, or is older than the .lfm file
  // -> update .lrs file
  Dir:=ExtractFilePath(LRSFilename);
  Result:=ForceDirectoryInteractive(Dir,[mbRetry]);
  if Result<>mrOk then exit;
  Result:=ConvertLFMToLRSFileInteractive(LFMFilename,LRSFilename,ShowAbort);
end;

function TBuildManager.UpdateProjectAutomaticFiles(TestDir: string): TModalResult;
var
  AnUnitInfo: TUnitInfo;
  Code: TCodeBuffer;
begin
  Result:=mrOk;
  Assert(not Project1.ProjResources.Modified, 'UpdateProjectAutomaticFiles: Resources are modified!');
  // update project resource
  if TestDir<>'' then
    Project1.ProjResources.Regenerate(Project1.MainFileName, False, True, TestDir);
  // Iterate project units
  AnUnitInfo := Project1.FirstPartOfProject;
  while AnUnitInfo<>nil do 
  begin
    if AnUnitInfo.HasResources then begin
      case GetResourceType(AnUnitInfo) of
      rtLRS:
        begin
          Result := UpdateLRSFromLFM(AnUnitInfo,false);
          if Result = mrIgnore then Result:=mrOk;
          if Result <> mrOk then exit;
        end;
      rtRes:  // This fixed encoding of source files but only if rtRes type. Why?
        begin // Source was read in every time although encoding is correct most of times.
        end;
      end;
    end;
    AnUnitInfo := AnUnitInfo.NextPartOfProject;
  end;
end;

function TBuildManager.MacroFuncMakeExe(const Filename: string;
  const Data: PtrInt; var Abort: boolean): string;
var
  CommaPos: SizeInt;
  CurTargetOS: String;
  CurFilename: String;
begin
  CurFilename:=Filename;
  CommaPos:=System.Pos(',',CurFilename);
  CurTargetOS:='';
  if CommaPos>1 then begin
    // makeexe(targetos,filename)
    CurTargetOS:=UTF8LowerCase(LeftStr(CurFilename,CommaPos-1));
    if IsValidIdent(CurTargetOS) then begin
      if CurTargetOS='ide' then
        CurTargetOS:=GetCompiledTargetOS;
      System.Delete(CurFilename,1,CommaPos);
    end;
  end;
  if CurTargetOS='' then
    CurTargetOS:=GetTargetOS;
  Result:=MakeStandardExeFilename(CurTargetOS,CurFilename);
  //DebugLn('TMainIDE.MacroFuncMakeExe A ',Filename,' ',Result);
end;

function TBuildManager.MacroFuncMakeLib(const Filename: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=MakeStandardLibFilename(GetTargetOS,Filename);
end;

function TBuildManager.MacroFuncInstantFPCCache(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
var
  Prog: String;
  List: TStringList;
begin
  if not FMacroInstantFPCCacheValid then begin
    FMacroInstantFPCCache:='';
    FMacroInstantFPCCacheValid:=true;
    Prog:=FindDefaultExecutablePath('instantfpc'+GetExecutableExt);
    if Prog<>'' then begin
      List:=nil;
      try
        if ConsoleVerbosity>0 then
          debugln(['Hint: (lazarus) TBuildManager.MacroFuncInstantFPCCache ',Prog]);
        List:=RunTool(Prog,'--get-cache','',ConsoleVerbosity<1);
        if (List<>nil) and (List.Count>0) then
          FMacroInstantFPCCache:=List[0];
        List.Free;
      except
        on E: Exception do begin
          if ConsoleVerbosity>=0 then
            debugln(['Warning: (lazarus) [TBuildManager.MacroFuncInstantFPCCache] error running '+Prog+': '+E.Message]);
        end;
      end;
    end;
    if ConsoleVerbosity>=1 then
      debugln(['Hint: (lazarus) [TBuildManager.MacroFuncInstantFPCCache] ',FMacroInstantFPCCache]);
  end;
  Result:=FMacroInstantFPCCache;
end;

function TBuildManager.MacroFuncProject(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  if Project1<>nil then begin
    if SysUtils.CompareText(Param,'SrcPath')=0 then
      Result:=Project1.CompilerOptions.GetSrcPath(false)
    else if SysUtils.CompareText(Param,'IncPath')=0 then
      Result:=Project1.CompilerOptions.GetIncludePath(false)
    else if SysUtils.CompareText(Param,'Namespaces')=0 then
      Result:=Project1.CompilerOptions.GetNamespacesParsed
    else if SysUtils.CompareText(Param,'UnitPath')=0 then
      Result:=Project1.CompilerOptions.GetUnitPath(false)
    else if SysUtils.CompareText(Param,'InfoFile')=0 then
      Result:=Project1.ProjectInfoFile
    else if SysUtils.CompareText(Param,'OutputDir')=0 then
      Result:=Project1.CompilerOptions.GetUnitOutPath(false)
    else begin
      Result:='<Invalid parameter for macro Project:'+Param+'>';
      if ConsoleVerbosity>=0 then
        debugln('Warning: (lazarus) [TMainIDE.MacroFuncProject]: ',Result);
    end;
  end else begin
    Result:='';
  end;
end;

function TBuildManager.MacroFuncLCLWidgetType(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Data=CompilerOptionMacroPlatformIndependent then
    Result:='%(LCL_PLATFORM)'
  else
    Result:=GetLCLWidgetType;
end;

function TBuildManager.MacroFuncLazVer(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  Result:=LazarusVersionStr;
end;

function TBuildManager.MacroFuncTargetCPU(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Data=CompilerOptionMacroPlatformIndependent then
    Result:='%(CPU_TARGET)'
  else if SysUtils.CompareText(Param,'IDE')=0 then
    Result:=GetCompiledTargetCPU
  else
    Result:=GetTargetCPU;
end;

function TBuildManager.MacroFuncTargetOS(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Data=CompilerOptionMacroPlatformIndependent then
    Result:='%(OS_TARGET)'
  else if SysUtils.CompareText(Param,'IDE')=0 then
    Result:=GetCompiledTargetOS
  else
    Result:=GetTargetOS;
end;

function TBuildManager.MacroFuncIDEBuildOptions(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Data=CompilerOptionMacroPlatformIndependent then
    Result:=''
  else if (MiscellaneousOptions<>nil)
  and (MiscellaneousOptions.BuildLazOpts<>nil)
  then
    Result:=MiscellaneousOptions.BuildLazOpts.ExtraOptions
  else
    Result:='';
end;

function TBuildManager.MacroFuncPrimaryConfigPath(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetPrimaryConfigPath;
end;

function TBuildManager.MacroFuncSecondaryConfigPath(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetSecondaryConfigPath;
end;

function TBuildManager.MacroFuncFallbackOutputRoot(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=AppendPathDelim(GetPrimaryConfigPath)+'lib';
end;

function TBuildManager.MacroFuncSrcOS(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  if Data=CompilerOptionMacroPlatformIndependent then
    Result:='%(OS_TARGET)'
  else if Param<>'' then
    Result:=GetDefaultSrcOSForTargetOS(Param)
  else
    Result:=GetDefaultSrcOSForTargetOS(GetTargetOS);
end;

function TBuildManager.MacroFuncFPCVer(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;

  procedure Compute;
  var
    TargetOS: String;
    TargetCPU: String;
    CompilerFilename: String;
    ConfigCache: TPCTargetConfigCache;
    s: string;
  begin
    FFPC_FULLVERSION:=0;
    if OverrideFPCVer<>'' then
      FFPCVer:=OverrideFPCVer;
    FFPCVer:={$I %FPCVERSION%};   // Version.Release.Patch
    if CodeToolBoss<>nil then begin
      // fetch the FPC version from the current compiler
      // Not from the fpc.exe, but from the real compiler
      CompilerFilename:=GetFPCompilerFilename;
      if not IsFPCExecutable(CompilerFilename,s) then exit;
      TargetOS:=GetTargetOS;
      TargetCPU:=GetTargetCPU;
      ConfigCache:=CodeToolBoss.CompilerDefinesCache.ConfigCaches.Find(
                                   CompilerFilename,'',TargetOS,TargetCPU,true);
      if ConfigCache=nil then exit;
      if ConfigCache.NeedsUpdate then begin
        // ask compiler
        if not ConfigCache.Update(CodeToolBoss.CompilerDefinesCache.TestFilename,
                                  CodeToolBoss.CompilerDefinesCache.ExtraOptions,nil)
        then
          exit;
      end;
      FFPCVer:=ConfigCache.GetFPCVer;
      FFPC_FULLVERSION:=ConfigCache.GetFPC_FULLVERSION;
    end;
  end;

begin
  if FFPCVerChangeStamp<>CompilerParseStamp then
  begin
    Compute;
    FFPCVerChangeStamp:=CompilerParseStamp;
    {$IFDEF VerboseFPCSrcScan}
    debugln(['TBuildManager.MacroFuncFPCVer FPCVer=',FFPCVer,' FPC_FULLVERSION=',FFPC_FULLVERSION,' Stamp=',FFPCVerChangeStamp]);
    {$ENDIF}
  end;
  Result:=FFPCVer;
end;

function TBuildManager.MacroFuncFPC_FULLVERSION(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if FFPCVerChangeStamp<>CompilerParseStamp then
    MacroFuncFPCVer(Param,Data,Abort);
  Result:=IntToStr(FFPC_FULLVERSION);
end;

function TBuildManager.MacroFuncParams(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  if (Project1<>nil) and (Project1.RunParameterOptions.GetActiveMode<>nil) then
    Result:=Project1.RunParameterOptions.GetActiveMode.CmdLineParams
  else
    Result:='';
end;

function TBuildManager.MacroFuncProjFile(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=Project1.MainFilename
  else
    Result:='';
end;

function TBuildManager.MacroFuncProjPath(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=Project1.Directory
  else
    Result:='';
end;

function TBuildManager.MacroFuncTargetFile(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=GetProjectTargetFilename(Project1)
  else
    Result:='';
end;

function TBuildManager.MacroFuncTargetCmdLine(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:='';
  if (Project1<>nil) then begin
    if (Project1.RunParameterOptions.GetActiveMode<>nil) then
      Result:=Project1.RunParameterOptions.GetActiveMode.CmdLineParams;
    if Result='' then
      Result:=GetProjectTargetFilename(Project1)
    else
      Result:=GetProjectTargetFilename(Project1)+' '+Result;
  end;
end;

function TBuildManager.MacroFuncRunCmdLine(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=GetRunCommandLine
  else
    Result:='';
end;

function TBuildManager.MacroFuncProjPublishDir(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetProjectPublishDir;
end;

function TBuildManager.MacroFuncProjUnitPath(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=Project1.CompilerOptions.GetUnitPath(false)
  else
    Result:='';
end;

function TBuildManager.MacroFuncProjIncPath(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=Project1.CompilerOptions.GetIncludePath(false)
  else
    Result:='';
end;

function TBuildManager.MacroFuncProjNamespaces(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then
    begin
    Result:=MergeWithDelimiter(GetProjectDefaultNamespace,
      Project1.CompilerOptions.GetNamespacesParsed,';');
    end
  else
    Result:='';
end;

function TBuildManager.MacroFuncProjSrcPath(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=Project1.CompilerOptions.GetSrcPath(false)
  else
    Result:='';
end;

function TBuildManager.MacroFuncProjOutDir(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=Project1.CompilerOptions.GetUnitOutPath(false)
  else
    Result:='';
end;

function TBuildManager.MacroFuncEnv(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  Result:=GetEnvironmentVariableUTF8(Param);
end;

function TBuildManager.MacroFuncCompPath(const s: string; const Data: PtrInt;
  var Abort: boolean): string;
// if parameter is 'IDE' return the environment option
// otherwise use active project's compiler
begin
  Result:='';
  if CompareText(s,'IDE')<>0 then
    Result:=GetCompilerFilename;

  if Result='' then
    Result:=EnvironmentOptions.GetParsedCompilerFilename;
end;

function TBuildManager.MacroFuncFPCMsgFile(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=EnvironmentOptions.GetParsedCompilerMessagesFilename;
end;

function TBuildManager.MacroFuncMake(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  Result:=EnvironmentOptions.GetParsedMakeFilename;
  if Result='' then
    Result:=FindDefaultMakePath;
end;

function TBuildManager.CTMacroFuncProjectNamespaces(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  Result:=false;
  if Project1<>nil then begin
    FuncData^.Result:=MergeWithDelimiter(GetProjectDefaultNamespace,
                            Project1.CompilerOptions.GetNamespacesParsed(),';');
    Result:=true;
  end;
end;

function TBuildManager.CTMacroFuncProjectUnitPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  Result:=false;
  if Project1<>nil then begin
    FuncData^.Result:=Project1.CompilerOptions.GetUnitPath(false);
    Result:=true;
  end;
end;

function TBuildManager.CTMacroFuncProjectIncPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  Result:=false;
  if Project1<>nil then begin
    FuncData^.Result:=
                 Project1.CompilerOptions.GetIncludePath(false,coptParsed,true);
    Result:=true;
  end;
end;

function TBuildManager.CTMacroFuncProjectSrcPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  Result:=false;
  if Project1<>nil then begin
    FuncData^.Result:=Project1.CompilerOptions.GetSrcPath(false);
    Result:=true;
  end;
end;

procedure TBuildManager.SetUnitSetCache(const AValue: TFPCUnitSetCache);
begin
  if FUnitSetCache=AValue then exit;
  FUnitSetCache:=AValue;
  if UnitSetCache<>nil then begin
    FreeNotification(UnitSetCache);
    FUnitSetChangeStamp:=UnitSetCache.GetInvalidChangeStamp;
  end;
end;

function TBuildManager.GetProjectDefaultNamespace: string;
// called by codetools *before* parsing
// Important: use only basiccodetools
var
  AnUnitInfo: TUnitInfo;
  NameStart, NameEnd: Integer;
  Code: TCodeBuffer;
  ModuleType, ModuleName: string;
  NestedComments: boolean;
begin
  Result:='';
  if Project1=nil then exit;
  if not (pfMainUnitIsPascalSource in Project1.Flags) then exit;
  AnUnitInfo:=Project1.MainUnitInfo;
  if AnUnitInfo=nil then exit;
  Code:=AnUnitInfo.Source;
  if Code=nil then exit;
  if (Code<>FProjectNameSpaceCode) or (Code.ChangeStep<>FProjectNameSpaceCodeChgStep) then
  begin
    // read namespace
    FProjectNameSpace:='';
    FProjectNameSpaceCode:=Code;
    FProjectNameSpaceCodeChgStep:=Code.ChangeStep;
    NestedComments:=CompareText(Project1.CompilerOptions.SyntaxMode,'delphi')<>0;
    ModuleName:=FindModuleNameInSource(Code.Source,ModuleType,NameStart,
      NameEnd,NestedComments);
    FProjectNameSpace:=ChompDottedIdentifier(ModuleName);
  end;
  Result:=FProjectNameSpace;
end;

procedure TBuildManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    if FUnitSetCache=AComponent then
      FUnitSetCache:=nil;
  end;
end;

function TBuildManager.OnGetBuildMacroValues(Options: TBaseCompilerOptions;
  IncludeSelf: boolean): TCTCfgScriptVariables;
{off $DEFINE VerboseBuildMacros}

  procedure AddAllInherited(FirstDependency: TPkgDependency;
    AddTo: TCTCfgScriptVariables);
  var
    List: TFPList;
    i: Integer;
    APackage: TLazPackage;
    Values: TCTCfgScriptVariables;
    OtherOpts: TPkgCompilerOptions;
    j: Integer;
    Macro: TLazBuildMacro;
    Value: PCTCfgScriptVariable;
  begin
    if FirstDependency=nil then exit;
    List:=nil;
    try
      PackageGraph.GetAllRequiredPackages(nil,FirstDependency,List);
      if List=nil then exit;
      for i:=0 to List.Count-1 do begin
        // add values of build macros of used package
        APackage:=TLazPackage(List[i]);
        OtherOpts:=APackage.CompilerOptions;
        if OtherOpts.BuildMacros=nil then continue;
        Values:=OnGetBuildMacroValues(OtherOpts,true);
        if Values=nil then continue;
        for j:=0 to OtherOpts.BuildMacros.Count-1 do begin
          Macro:=OtherOpts.BuildMacros[j];
          if Macro.Identifier='' then continue;
          Value:=Values.GetVariable(PChar(Macro.Identifier));
          if Value=nil then begin
            //debugln(['AddAllInherited InhPkg=',APackage.Name,' Macro="',Macro.Identifier,'" no value']);
            continue;
          end else begin
            //debugln(['AddAllInherited InhPkg=',APackage.Name,' Macro="',Macro.Identifier,'" Value="',dbgs(Value),'"']);
            AddTo.AddOverride(Value);
          end;
        end;
      end;
    finally
      List.Free;
    end;
  end;

  procedure SetCmdLineOverrides(Values: TCTCfgScriptVariables);
  var
    Overrides: TStrings;
    i: Integer;
  begin
    // set overrides (e.g. command line parameters)
    Overrides:=GetBuildMacroOverrides;
    try
      for i:=0 to Overrides.Count-1 do
        Values.Values[Overrides.Names[i]]:=Overrides.ValueFromIndex[i];
      {$IFDEF VerboseBuildMacros}
      debugln(['TBuildManager.OnGetBuildMacroValues cmd line overrides=',dbgstr(Overrides.Text)]);
      {$ENDIF}
    finally
      Overrides.Free;
    end;
  end;

  procedure SetDefaults(Values: TCTCfgScriptVariables);
  var
    s: String;
  begin
    // add the defaults
    // Note: see also ide/frames/compiler_buildmacro_options.pas procedure TCompOptBuildMacrosFrame.BuildMacrosTreeViewEdited
    // TargetOS
    if not Values.IsDefined('TargetOS') then begin
      s:='';
      if FBuildTarget<>nil then
        s:=FBuildTarget.CompilerOptions.TargetOS;
      if s='' then
        s:=fTargetOS;
      if s='' then begin
        {$IFDEF VerboseDefaultCompilerTarget}
        debugln(['SetDefaults WARNING: no TargetOS']);
        {$ENDIF}
        s:=GetCompiledTargetOS;
      end;
      Values.Values['TargetOS']:=s;
    end;
    // SrcOS
    if not Values.IsDefined('SrcOS') then begin
      s:=GetDefaultSrcOSForTargetOS(Result.Values['TargetOS']);
      Values.Values['SrcOS']:=s;
    end;
    // SrcOS2
    if not Result.IsDefined('SrcOS2') then begin
      s:=GetDefaultSrcOS2ForTargetOS(Result.Values['TargetOS']);
      Values.Values['SrcOS2']:=s;
    end;
    // TargetCPU
    if not Values.IsDefined('TargetCPU') then begin
      s:='';
      if FBuildTarget<>nil then
        s:=FBuildTarget.CompilerOptions.TargetCPU;
      if s='' then
        s:=fTargetCPU;
      Values.Values['TargetCPU']:=s;
      if s='' then begin
        {$IFDEF VerboseDefaultCompilerTarget}
        debugln(['SetDefaults WARNING: no TargetCPU']);
        {$ENDIF}
        s:=GetCompiledTargetCPU;
      end;
    end;
  end;

  procedure ApplyMacroOverrides(Vars: TCTCfgScriptVariables);
  var
    Target: String;
    ActiveMode: String;
  begin
    ActiveMode:=GetActiveBuildModeName;
    Target:=GetModeMatrixTarget(Options);
    if EnvironmentOptions<>nil then
      ApplyBuildMatrixMacros(EnvironmentOptions.BuildMatrixOptions,Target,ActiveMode,Vars);
    if FBuildTarget<>nil then begin
      ApplyBuildMatrixMacros(FBuildTarget.BuildModes.SharedMatrixOptions,Target,ActiveMode,Vars);
      ApplyBuildMatrixMacros(FBuildTarget.BuildModes.SessionMatrixOptions,Target,ActiveMode,Vars);
    end;
    SetCmdLineOverrides(Vars);
    {$IFDEF VerboseBuildMacros}
    Vars.WriteDebugReport('OnGetBuildMacroValues after applying project values');
    {$ENDIF}
    SetDefaults(Vars);
  end;

var
  ParseOpts: TParsedCompilerOptions;
  Values: TCTCfgScriptVariables;
begin
  Result:=nil;

  ParseOpts:=Options.ParsedOpts;
  if ParseOpts=nil then exit;

  if IncludeSelf then begin
    Result:=ParseOpts.MacroValues.Variables;
    if ParseOpts.MacroValuesStamp=BuildMacroChangeStamp then exit;

    // compute macro values

    if ParseOpts.MacroValuesParsing then begin
      if ConsoleVerbosity>=0 then
        debugln(['Warning: (lazarus) [TBuildManager.OnGetBuildMacroValues] cycle computing macros of ',dbgsname(Options.Owner)]);
      exit;
    end;

    ParseOpts.MacroValuesParsing:=true;
    try
      Result.Clear;

      // use inherited as default
      Values:=OnGetBuildMacroValues(Options,false);

      // add macro values of self
      if Values<>nil then
        Result.Assign(Values);
      {$IF defined(VerboseBuildMacros) or defined(DebugLCLBaseConditionals)}
      if (Options.Owner is TLazPackage) and (TLazPackage(Options.Owner).Name='LCLBase') then
        Result.WriteDebugReport('TBuildManager.OnGetBuildMacroValues before execute: Conditionals="'+dbgstr(Options.Conditionals),'"');
      {$ENDIF}
      if not ParseOpts.MacroValues.Execute(Options.Conditionals) then begin
        if ConsoleVerbosity>=0 then
          debugln(['Error: (lazarus) [TBuildManager.OnGetBuildMacroValues] Error: ',ParseOpts.MacroValues.GetErrorStr(0)]);
        debugln(Options.Conditionals);
      end;

      {$IFDEF VerboseBuildMacros}
      if (Options.Owner is TLazPackage) and (TLazPackage(Options.Owner).Name='LCL') then
        Result.WriteDebugReport('TBuildManager.OnGetBuildMacroValues executed: '+dbgstr(Options.Conditionals),'  ');
      {$ENDIF}

      // the macro values of the active project take precedence
      ApplyMacroOverrides(Result);

      ParseOpts.MacroValuesStamp:=BuildMacroChangeStamp;
    finally
      ParseOpts.MacroValuesParsing:=false;
    end;
  end else begin
    Result:=ParseOpts.InheritedMacroValues;
    if ParseOpts.InheritedMacroValuesStamp=BuildMacroChangeStamp then exit;

    // compute inherited values
    if ParseOpts.InheritedMacroValuesParsing then begin
      if ConsoleVerbosity>=0 then
        debugln(['Error: (lazarus) [TBuildManager.OnGetBuildMacroValues] cycle detected computing inherited macros of ',dbgsname(Options.Owner)]);
      exit;
    end;
    ParseOpts.InheritedMacroValuesParsing:=true;
    try
      Result.Clear;

      // add inherited
      if (PackageGraph<>nil) then begin
        if Options.Owner is TProject then
          AddAllInherited(TProject(Options.Owner).FirstRequiredDependency,Result)
        else if Options.Owner is TLazPackage then
          AddAllInherited(TLazPackage(Options.Owner).FirstRequiredDependency,Result);
      end;

      // the macro values of the active project take precedence
      ApplyMacroOverrides(Result);

      ParseOpts.InheritedMacroValuesStamp:=BuildMacroChangeStamp;
    finally
      ParseOpts.InheritedMacroValuesParsing:=false;
    end;
  end;
end;

function TBuildManager.GetActiveBuildModeName: string;
begin
  if FBuildTarget<>nil then
    Result:=FBuildTarget.ActiveBuildMode.Identifier
  else
    Result:='default';
end;

procedure TBuildManager.AppendMatrixCustomOption(Sender: TObject;
  var Options: string; Types: TBuildMatrixGroupTypes);
var
  Target: String;
  ActiveMode: String;
begin
  Target:=GetModeMatrixTarget(Sender);
  ActiveMode:=GetActiveBuildModeName;
  if bmgtEnvironment in Types then
    EnvironmentOptions.BuildMatrixOptions.AppendCustomOptions(Target,ActiveMode,Options);
  if FBuildTarget<>nil then begin
    if bmgtProject in Types then
      FBuildTarget.BuildModes.SharedMatrixOptions.AppendCustomOptions(Target,ActiveMode,Options);
    if bmgtSession in Types then
      FBuildTarget.BuildModes.SessionMatrixOptions.AppendCustomOptions(Target,ActiveMode,Options);
  end;
end;

procedure TBuildManager.GetMatrixOutputDirectoryOverride(Sender: TObject;
  var OutDir: string; Types: TBuildMatrixGroupTypes);
var
  Target: String;
  ActiveMode: String;
begin
  Target:=GetModeMatrixTarget(Sender);
  ActiveMode:=GetActiveBuildModeName;
  if bmgtEnvironment in Types then
    EnvironmentOptions.BuildMatrixOptions.GetOutputDirectory(Target,ActiveMode,OutDir);
  if FBuildTarget<>nil then begin
    if bmgtProject in Types then
      FBuildTarget.BuildModes.SharedMatrixOptions.GetOutputDirectory(Target,ActiveMode,OutDir);
    if bmgtSession in Types then
      FBuildTarget.BuildModes.SessionMatrixOptions.GetOutputDirectory(Target,ActiveMode,OutDir);
  end;
end;

function TBuildManager.GetModeMatrixTarget(Sender: TObject): string;
begin
  Result:='';
  if Sender is TParsedCompilerOptions then
    Sender:=TParsedCompilerOptions(Sender).Owner;
  if Sender is TPkgAdditionalCompilerOptions then
    exit; // matrix options are added only to normal options
  if Sender is TPkgCompilerOptions then
    Sender:=TPkgCompilerOptions(Sender).Owner
  else if Sender is TProjectCompilerOptions then
    Sender:=TProjectCompilerOptions(Sender).Owner;
  if Sender is TProject then begin
    Result:=BuildMatrixProjectName;
  end else if Sender is TLazPackage then begin
    Result:=TLazPackage(Sender).Name;
  end else
    Result:=BuildMatrixIDEName;
  //debugln(['TBuildManager.GetModeMatrixTarget ',DbgSName(Sender),' Target="',Result,'"']);
end;

function TBuildManager.EnvironmentOptionsIsGlobalMode(const Identifier: string
  ): boolean;
begin
  Result:=true;
  if Project1=nil then exit;
  if Project1.BuildModes=nil then exit;
  // do not save enabled states of session modes
  Result:=not Project1.BuildModes.IsSessionMode(Identifier);
end;

procedure TBuildManager.SetBuildTarget(const TargetOS, TargetCPU,
  LCLWidgetType: string; ScanFPCSrc: TScanModeFPCSources; Quiet: boolean);

  function GetEffectiveLCLWidgetType: string;
  begin
    if OverrideLCLWidgetType<>'' then
      Result:=OverrideLCLWidgetType
    else if FBuildTarget<>nil then begin
      Result:=FBuildTarget.CompilerOptions.GetEffectiveLCLWidgetType
    end
    else
      Result:='';
    if (Result='') or (SysUtils.CompareText(Result,'default')=0) then
      Result:=GetLCLWidgetTypeName;
    Result:=lowercase(Result);
  end;

var
  OldTargetOS: String;
  OldTargetCPU: String;
  OldLCLWidgetType: String;
  FPCTargetChanged: Boolean;
  LCLTargetChanged: Boolean;
  CompilerTargetOS: string;
  CompilerTargetCPU: string;
  CompQueryOptions: String;
begin
  {$IFDEF VerboseDefaultCompilerTarget}
  debugln(['TBuildManager.SetBuildTarget TargetOS="',TargetOS,'" TargetCPU="',TargetCPU,'" LCLWidgetType="',LCLWidgetType,'"']);
  {$ENDIF}
  OldTargetOS:=fTargetOS;
  OldTargetCPU:=fTargetCPU;
  OldLCLWidgetType:=fLCLWidgetType;
  OverrideTargetOS:=GetFPCTargetOS(TargetOS);
  OverrideTargetCPU:=GetFPCTargetCPU(TargetCPU);
  OverrideLCLWidgetType:=lowercase(LCLWidgetType);

  // compute new TargetOS
  if OverrideTargetOS<>'' then
    fTargetOS:=OverrideTargetOS
  else if FBuildTarget<>nil then
    fTargetOS:=FBuildTarget.CompilerOptions.TargetOS
  else
    fTargetOS:='';
  if SysUtils.CompareText(fTargetOS,'default')=0 then
    fTargetOS:='';

  // compute new TargetCPU
  if OverrideTargetCPU<>'' then
    fTargetCPU:=OverrideTargetCPU
  else if FBuildTarget<>nil then
    fTargetCPU:=FBuildTarget.CompilerOptions.TargetCPU
  else
    fTargetCPU:='';
  if SysUtils.CompareText(fTargetCPU,'default')=0 then
    fTargetCPU:='';

  if (fTargetOS='') or (fTargetCPU='') then
  begin
    // use compiler default target
    CompQueryOptions:='';
    if fTargetCPU<>'' then
      CompQueryOptions:='-P'+GetFPCTargetCPU(fTargetCPU)
    else if fTargetOS<>'' then
      CompQueryOptions:='-T'+GetFPCTargetOS(fTargetOS);
    // Note: resolving the comiler filename requires macros
    CodeToolBoss.CompilerDefinesCache.ConfigCaches.GetDefaultCompilerTarget(
      GetCompilerFilename,CompQueryOptions,CompilerTargetOS,CompilerTargetCPU);
    if fTargetOS='' then
      fTargetOS:=CompilerTargetOS;
    if fTargetOS='' then
      fTargetOS:=GetCompiledTargetOS;
    if fTargetCPU='' then
      fTargetCPU:=CompilerTargetCPU;
    if fTargetCPU='' then
      fTargetCPU:=GetCompiledTargetCPU;
    // the macros were resolved with default values
    // => invalidate macros so they now use the actual values
    IncreaseBuildMacroChangeStamp;
    if ConsoleVerbosity>1 then
      debugln(['Hint: (lazarus) [TBuildManager.SetBuildTarget] OS=',fTargetOS,' CPU=',fTargetCPU,' CompQueryOptions=',CompQueryOptions,' DefaultOS=',CompilerTargetOS,' DefaultCPU=',CompilerTargetCPU]);
  end;

  fTargetOS:=GetFPCTargetOS(fTargetOS);
  fTargetCPU:=GetFPCTargetCPU(fTargetCPU);

  FPCTargetChanged:=(OldTargetOS<>fTargetOS)
                    or (OldTargetCPU<>fTargetCPU)
                    or (CodeToolBoss.DefineTree.FindDefineTemplateByName(
                         StdDefTemplLazarusSources,true)=nil);
  if FPCTargetChanged then
  begin
    IncreaseBuildMacroChangeStamp;
    CodeToolBoss.DefineTree.ClearCache;
  end;

  // compute new LCLWidgetType
  fLCLWidgetType:=GetEffectiveLCLWidgetType;
  LCLTargetChanged:=(OldLCLWidgetType<>fLCLWidgetType);

  if FPCTargetChanged or LCLTargetChanged then begin
    if ConsoleVerbosity>=0 then
      DebugLn(['Hint: (lazarus) [TBuildManager.SetBuildTarget] Old=',OldTargetCPU,'-',OldTargetOS,'-',OldLCLWidgetType,' New=',fTargetCPU,'-',fTargetOS,'-',fLCLWidgetType,' FPC=',FPCTargetChanged,' LCL=',LCLTargetChanged]);
  end;
  if LCLTargetChanged then
    CodeToolBoss.SetGlobalValue(ExternalMacroStart+'LCLWidgetType',fLCLWidgetType);
  if ScanFPCSrc<>smsfsSkip then
    RescanCompilerDefines(false,false,ScanFPCSrc=smsfsWaitTillDone,Quiet);
  //if (PackageGraph<>nil) and (PackageGraph.CodeToolsPackage<>nil) then debugln(['TBuildManager.SetBuildTarget CODETOOLS OUTDIR=',PackageGraph.CodeToolsPackage.CompilerOptions.GetUnitOutPath(true,coptParsed),' ',PackageGraph.CodeToolsPackage.CompilerOptions.ParsedOpts.ParsedStamp[pcosOutputDir],' ',CompilerParseStamp]);
end;

procedure TBuildManager.SetBuildTargetProject1;
begin
  SetBuildTargetProject1(true);
end;

procedure TBuildManager.SetBuildTargetProject1(Quiet: boolean;
  ScanFPCSrc: TScanModeFPCSources);
begin
  //debugln(['TBuildManager.SetBuildTargetProject1 START']);
  FBuildTarget:=Project1;
  if FBuildTarget<>nil then
    FBuildTarget.IDEOptions.AddHandlerDestroy(@OnProjectDestroy);
  SetBuildTarget('','','',ScanFPCSrc,Quiet);
end;

procedure TBuildManager.SetBuildTargetIDE;
var
  NewTargetOS: String;
  NewTargetCPU: String;
  NewLCLWidgetSet: String;
begin
  //debugln(['TBuildManager.SetBuildTargetIDE START']);
  FBuildTarget:=nil;
  with MiscellaneousOptions do begin
    NewTargetOS:=BuildLazOpts.TargetOS;
    NewTargetCPU:=BuildLazOpts.TargetCPU;
    NewLCLWidgetSet:=LCLPlatformDirNames[BuildLazOpts.TargetPlatform];
  end;
  if ConsoleVerbosity>=1 then
    debugln(['Hint: (lazarus) [TBuildManager.SetBuildTargetIDE] OS=',NewTargetOS,' CPU=',NewTargetCPU,' WS=',NewLCLWidgetSet]);
  SetBuildTarget(NewTargetOS,NewTargetCPU,NewLCLWidgetSet,smsfsBackground,false);
end;

function TBuildManager.BuildTargetIDEIsDefault: boolean;
var
  NewTargetOS: String;
  NewTargetCPU: String;
  NewLCLWidgetSet: TLCLPlatform;
begin
  with MiscellaneousOptions do begin
    NewTargetOS:=LowerCase(BuildLazOpts.TargetOS);
    NewTargetCPU:=LowerCase(BuildLazOpts.TargetCPU);
    NewLCLWidgetSet:=BuildLazOpts.TargetPlatform;
  end;
  //debugln(['TBuildManager.BuildTargetIDEIsDefault NewTargetOS=',NewTargetOS,' Default=',GetDefaultTargetOS,' NewTargetCPU=',NewTargetCPU,' default=',GetDefaultTargetCPU,' ws=',LCLPlatformDisplayNames[NewLCLWidgetSet],' default=',LCLPlatformDisplayNames[GetDefaultLCLWidgetType]]);
  Result:=((NewTargetOS='') or (NewTargetOS=GetCompiledTargetOS))
      and ((NewTargetCPU='') or (NewTargetCPU=GetCompiledTargetCPU))
      and (NewLCLWidgetSet<>lpNoGUI)
      and (GetCompilerFilename=EnvironmentOptions.GetParsedCompilerFilename);
end;

end.

