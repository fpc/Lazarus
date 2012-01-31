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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit BuildManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree,
  // LCL
  LConvEncoding, InterfaceBase, LCLProc, Dialogs, FileUtil, Forms, Controls,
  // codetools
  ExprEval, BasicCodeTools, CodeToolManager, DefineTemplates, CodeCache,
  FileProcs, CodeToolsCfgScript, Laz_XMLCfg, CodeToolsStructs,
  // IDEIntf
  SrcEditorIntf, ProjectIntf, MacroIntf, IDEDialogs, IDEExternToolIntf,
  CompOptsIntf, LazIDEIntf,
  // IDE
  LazarusIDEStrConsts, DialogProcs, IDEProcs, CodeToolsOptions, InputHistory,
  EditDefineTree, ProjectResources, MiscOptions, LazConf, EnvironmentOpts,
  TransferMacros, CompilerOptions, OutputFilter, Compiler, FPCSrcScan,
  PackageDefs, PackageSystem, Project,
  BaseBuildManager, ApplicationBundle;
  
type
  TBMScanFPCSources = (
    bmsfsSkip,
    bmsfsWaitTillDone, // scan now and wait till finished
    bmsfsBackground    // start in background
    );

  { TBuildManager }

  TBuildManager = class(TBaseBuildManager)
  private
    CurrentParsedCompilerOption: TParsedCompilerOptions;
    FUnitSetCache: TFPCUnitSetCache;
    function OnSubstituteCompilerOption(Options: TParsedCompilerOptions;
                                        const UnparsedValue: string;
                                        PlatformIndependent: boolean): string;
    function MacroFuncEnv(const Param: string; const Data: PtrInt;
                          var Abort: boolean): string;
    function MacroFuncFPCMsgFile(const Param: string; const Data: PtrInt;
                          var Abort: boolean): string;
    function MacroFuncFPCVer(const Param: string; const Data: PtrInt;
                             var Abort: boolean): string;
    function MacroFuncLCLWidgetType(const Param: string; const Data: PtrInt;
                                    var Abort: boolean): string;
    function MacroFuncMake(const Param: string; const Data: PtrInt;
                           var Abort: boolean): string;// make utility
    function MacroFuncMakeExe(const Filename: string; const Data: PtrInt;
                              var Abort: boolean): string;
    function MacroFuncMakeLib(const Filename: string; const Data: PtrInt;
                              var Abort: boolean): string;
    function MacroFuncInstantFPCCache(const Param: string; const Data: PtrInt;
                           var Abort: boolean): string;// path of the instantfpc cache
    function MacroFuncParams(const Param: string; const Data: PtrInt;
                             var Abort: boolean): string;
    function MacroFuncProject(const Param: string; const Data: PtrInt;
                              var Abort: boolean): string;
    function MacroFuncProjFile(const Param: string; const Data: PtrInt;
                               var Abort: boolean): string;
    function MacroFuncProjIncPath(const Param: string; const Data: PtrInt;
                                  var Abort: boolean): string;
    function MacroFuncProjOutDir(const Param: string; const Data: PtrInt;
                                 var Abort: boolean): string;
    function MacroFuncProjPath(const Param: string; const Data: PtrInt;
                               var Abort: boolean): string;
    function MacroFuncProjPublishDir(const Param: string; const Data: PtrInt;
                                     var Abort: boolean): string;
    function MacroFuncProjSrcPath(const Param: string; const Data: PtrInt;
                                  var Abort: boolean): string;
    function MacroFuncProjUnitPath(const Param: string; const Data: PtrInt;
                                   var Abort: boolean): string;
    function MacroFuncRunCmdLine(const Param: string; const Data: PtrInt;
                                 var Abort: boolean): string;
    function MacroFuncSrcOS(const Param: string; const Data: PtrInt;
                            var Abort: boolean): string;
    function MacroFuncTargetCmdLine(const Param: string; const Data: PtrInt;
                                    var Abort: boolean): string;
    function MacroFuncTargetCPU(const Param: string; const Data: PtrInt;
                                var Abort: boolean): string;
    function MacroFuncTargetFile(const Param: string; const Data: PtrInt;
                                 var Abort: boolean): string;
    function MacroFuncTargetOS(const Param: string; const Data: PtrInt;
                               var Abort: boolean): string;
    function MacroFuncIDEBuildOptions(const Param: string; const Data: PtrInt;
                               var Abort: boolean): string;
    function MacroFuncPrimaryConfigPath(const Param: string; const Data: PtrInt;
                                        var Abort: boolean): string;
    function MacroFuncSecondaryConfigPath(const Param: string; const Data: PtrInt;
                                          var Abort: boolean): string;
    function MacroFuncFallbackOutputRoot(const Param: string; const Data: PtrInt;
                                         var Abort: boolean): string;

    function CTMacroFuncProjectUnitPath(Data: Pointer): boolean;
    function CTMacroFuncProjectIncPath(Data: Pointer): boolean;
    function CTMacroFuncProjectSrcPath(Data: Pointer): boolean;
    procedure OnCmdLineCreate(var CmdLine: string; var Abort: boolean);
    function OnRunCompilerWithOptions(ExtTool: TIDEExternalToolOptions;
                           CompOptions: TBaseCompilerOptions): TModalResult;
    procedure SetUnitSetCache(const AValue: TFPCUnitSetCache);
  protected
    fTargetOS: string;
    fTargetCPU: string;
    fLCLWidgetType: string;
    OverrideTargetOS: string;
    OverrideTargetCPU: string;
    OverrideLCLWidgetType: string;
    FUnitSetChangeStamp: integer;
    FFPCSrcScans: TFPCSrcScans;
    DefaultCfgVars: TCTCfgScriptVariables;
    DefaultCfgVarsBuildMacroStamp: integer;
    // Macro FPCVer
    FFPCVer: string;
    FFPCVerChangeStamp: integer;
    // Macro InstantFPCCache
    FMacroInstantFPCCache: string;
    FMacroInstantFPCCacheValid: boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    function OnGetBuildMacroValues(Options: TBaseCompilerOptions;
                                   IncludeSelf: boolean): TCTCfgScriptVariables;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetupTransferMacros;
    procedure SetupCompilerInterface;
    procedure SetupInputHistories;

    function GetBuildMacroOverride(const MacroName: string): string; override;
    function GetBuildMacroOverrides: TStrings; override;
    function GetTargetOS: string; override;
    function GetTargetCPU: string; override;
    function GetLCLWidgetType: string; override;
    function GetRunCommandLine: string; override;

    function GetProjectPublishDir: string; override;
    function GetProjectTargetFilename(aProject: TProject): string; override;
    function GetProjectUsesAppBundle: Boolean; override;
    function GetTestProjectFilename(aProject: TProject): string; override;
    function GetTestUnitFilename(AnUnitInfo: TUnitInfo): string; override;
    function GetTestBuildDirectory: string; override;
    function IsTestUnitFilename(const AFilename: string): boolean; override;
    function GetTargetUnitFilename(AnUnitInfo: TUnitInfo): string; override;

    procedure UpdateEnglishErrorMsgFilename;
    procedure RescanCompilerDefines(ResetBuildTarget, ClearCaches,
                                    WaitTillDone, Quiet: boolean); override;
    function CompilerOnDiskChanged: boolean; override;
    procedure LoadFPCDefinesCaches;
    procedure SaveFPCDefinesCaches;
    property UnitSetCache: TFPCUnitSetCache read FUnitSetCache write SetUnitSetCache;

    function CheckAmbiguousSources(const AFilename: string;
                                   Compiling: boolean): TModalResult; override;
    function DeleteAmbiguousFiles(const Filename:string
                                  ): TModalResult; override;
    function CheckUnitPathForAmbiguousPascalFiles(const BaseDir, TheUnitPath,
                                    CompiledExt, ContextDescription: string
                                    ): TModalResult; override;
    function CreateProjectApplicationBundle: Boolean; override;
    function BackupFile(const Filename: string): TModalResult; override;

    function GetResourceType(AnUnitInfo: TUnitInfo): TResourceType;
    function FindLRSFilename(AnUnitInfo: TUnitInfo;
                             UseDefaultIfNotFound: boolean): string;
    function GetDefaultLRSFilename(AnUnitInfo: TUnitInfo): string;
    function UpdateLRSFromLFM(AnUnitInfo: TUnitInfo; ShowAbort: boolean): TModalResult;
    function UpdateProjectAutomaticFiles(TestDir: string): TModalResult; override;

    // methods for building IDE (will be changed when project groups are there)
    procedure SetBuildTarget(const TargetOS, TargetCPU, LCLWidgetType: string;
                             ScanFPCSrc: TBMScanFPCSources; Quiet: boolean);
    procedure SetBuildTargetProject1(Quiet: boolean;
                               ScanFPCSrc: TBMScanFPCSources = bmsfsBackground);
    procedure SetBuildTargetIDE;
    function BuildTargetIDEIsDefault: boolean;

    property FPCSrcScans: TFPCSrcScans read FFPCSrcScans;
  end;
  
var
  MainBuildBoss: TBuildManager = nil;
  TheCompiler: TCompiler = nil;
  TheOutputFilter: TOutputFilter = nil;

implementation

type
  TUnitFile = record
    FileUnitName: string;
    Filename: string;
  end;
  PUnitFile = ^TUnitFile;

procedure BMLazConfMacroFunction(var s: string);
begin
  GlobalMacroList.SubstituteStr(s);
end;

function CompareUnitFiles(UnitFile1, UnitFile2: PUnitFile): integer;
begin
  Result:=CompareIdentifierPtrs(Pointer(UnitFile1^.FileUnitName),
                                Pointer(UnitFile2^.FileUnitName));
end;

function CompareUnitNameAndUnitFile(UnitName: PChar;
  UnitFile: PUnitFile): integer;
begin
  Result:=CompareIdentifierPtrs(Pointer(UnitName),Pointer(UnitFile^.FileUnitName));
end;

{ TBuildManager }

function TBuildManager.OnSubstituteCompilerOption(
  Options: TParsedCompilerOptions; const UnparsedValue: string;
  PlatformIndependent: boolean): string;
begin
  CurrentParsedCompilerOption:=Options;
  Result:=UnparsedValue;
  if PlatformIndependent then
    GlobalMacroList.SubstituteStr(Result,CompilerOptionMacroPlatformIndependent)
  else
    GlobalMacroList.SubstituteStr(Result,CompilerOptionMacroNormal);
end;

constructor TBuildManager.Create(AOwner: TComponent);
begin
  DefaultCfgVars:=TCTCfgScriptVariables.Create;
  DefaultCfgVarsBuildMacroStamp:=InvalidParseStamp;
  FFPCVerChangeStamp:=InvalidParseStamp;
  MainBuildBoss:=Self;
  inherited Create(AOwner);
  fTargetOS:=GetDefaultTargetOS;
  fTargetCPU:=GetDefaultTargetCPU;
  fLCLWidgetType:=LCLPlatformDirNames[GetDefaultLCLWidgetType];
  FUnitSetChangeStamp:=TFPCUnitSetCache.GetInvalidChangeStamp;

  OnBackupFileInteractive:=@BackupFile;
  RunCompilerWithOptions:=@OnRunCompilerWithOptions;

  GetBuildMacroValues:=@OnGetBuildMacroValues;
end;

destructor TBuildManager.Destroy;
begin
  FreeAndNil(FFPCSrcScans);

  LazConfMacroFunc:=nil;
  OnBackupFileInteractive:=nil;
  FreeAndNil(InputHistories);
  FreeAndNil(DefaultCfgVars);

  inherited Destroy;
  MainBuildBoss:=nil;
end;

procedure TBuildManager.SetupTransferMacros;
begin
  LazConfMacroFunc:=@BMLazConfMacroFunction;
  GlobalMacroList:=TTransferMacroList.Create;
  IDEMacros:=TLazIDEMacros.Create;
  CompilerOptions.OnParseString:=@OnSubstituteCompilerOption;

  // environment
  EnvironmentOptions.InitMacros(GlobalMacroList);

  // project
  GlobalMacroList.Add(TTransferMacro.Create('Project','',
                      lisProjectMacroProperties,@MacroFuncProject,[]));
  GlobalMacroList.Add(TTransferMacro.Create('LCLWidgetType','',
                      lisLCLWidgetType,@MacroFuncLCLWidgetType,[]));
  GlobalMacroList.Add(TTransferMacro.Create('TargetCPU','',
                      lisTargetCPU,@MacroFuncTargetCPU,[]));
  GlobalMacroList.Add(TTransferMacro.Create('TargetOS','',
                      lisTargetOS,@MacroFuncTargetOS,[]));
  GlobalMacroList.Add(TTransferMacro.Create('SrcOS','',
                      lisSrcOS,@MacroFuncSrcOS,[]));
  GlobalMacroList.Add(TTransferMacro.Create('FPCVer','',
                      lisFPCVersionEG222, @MacroFuncFPCVer, []));
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
  GlobalMacroList.Add(TTransferMacro.Create('FPCMsgFile','',
                     lisFPCMessageFile, @MacroFuncFPCMsgFile, []));
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

  // codetools macro functions
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECTUNITPATH',nil,@CTMacroFuncProjectUnitPath);
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECTINCPATH',nil,@CTMacroFuncProjectIncPath);
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECTSRCPATH',nil,@CTMacroFuncProjectSrcPath);
end;

procedure TBuildManager.SetupCompilerInterface;
begin
  TheCompiler := TCompiler.Create;
  with TheCompiler do begin
    OnCommandLineCreate:=@OnCmdLineCreate;
    OutputFilter:=TheOutputFilter;
  end;
end;

procedure TBuildManager.SetupInputHistories;
begin
  if InputHistories<>nil then exit;
  InputHistories:=TInputHistories.Create;
  with InputHistories do begin
    SetLazarusDefaultFilename;
    Load;
  end;
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

function TBuildManager.GetRunCommandLine: string;
var
  TargetFileName: string;
  
  function GetTargetFilename: String;
  begin
    Result := GetProjectTargetFilename(Project1);
    
    if GetProjectUsesAppBundle then
    begin
      // return command line to Application Bundle (darwin only)
      Result := ExtractFileNameWithoutExt(Result) + '.app';
    end;
  end;
  
begin
  if Project1.RunParameterOptions.UseLaunchingApplication then
    Result := Project1.RunParameterOptions.LaunchingApplicationPathPlusParams
  else
    Result := '';

  if Result=''
  then begin
    Result:=Project1.RunParameterOptions.CmdLineParams;
    if GlobalMacroList.SubstituteStr(Result) then begin
      TargetFileName:='"'+GetTargetFilename+'"';
      if Result='' then
        Result:=TargetFileName
      else
        Result:=TargetFilename+' '+Result;
    end else
      Result:='';
  end else begin
    if not GlobalMacroList.SubstituteStr(Result) then Result:='';
  end;
end;

function TBuildManager.GetProjectPublishDir: string;
begin
  if Project1=nil then begin
    Result:='';
    exit;
  end;
  Result:=Project1.PublishOptions.DestinationDirectory;
  if GlobalMacroList.SubstituteStr(Result) then begin
    if FilenameIsAbsolute(Result) then begin
      Result:=AppendPathDelim(TrimFilename(Result));
    end else begin
      Result:='';
    end;
  end else begin
    Result:='';
  end;
end;

function TBuildManager.GetProjectTargetFilename(aProject: TProject): string;
begin
  Result:='';
  if aProject=nil then exit;
  Result:=aProject.RunParameterOptions.HostApplicationFilename;
  GlobalMacroList.SubstituteStr(Result);
  if Result='' then begin
    if aProject.IsVirtual then
      Result:=GetTestProjectFilename(aProject)
    else begin
      if aProject.MainUnitID>=0 then begin
        Result :=
          aProject.CompilerOptions.CreateTargetFilename(aProject.MainFilename);
      end;
    end;
  end;
end;

function TBuildManager.GetProjectUsesAppBundle: Boolean;
begin
  Result := (Project1.RunParameterOptions.HostApplicationFilename = '') and
    (GetTargetOS = 'darwin') and Project1.UseAppBundle;
end;

function TBuildManager.GetTestProjectFilename(aProject: TProject): string;
var
  TestDir: String;
begin
  Result:='';
  if aProject=nil then exit;
  if (aProject.MainUnitID<0) then exit;
  Result:=GetTestUnitFilename(aProject.MainUnitInfo);
  if Result='' then exit;
  Result:=aProject.CompilerOptions.CreateTargetFilename(Result);
  if Result='' then exit;
  if (not FilenameIsAbsolute(Result)) then begin
    TestDir:=GetTestBuildDirectory;
    if TestDir='' then exit;
    Result:=TestDir+Result;
  end;
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
  Result:=EnvironmentOptions.GetTestBuildDirectory;
end;

function TBuildManager.IsTestUnitFilename(const AFilename: string): boolean;
var
  TestDir: string;
begin
  Result:=false;
  if Project1.IsVirtual then begin
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
  if EnvironmentOptions.LazarusDirectory<>'' then begin
    CodeToolBoss.DefinePool.EnglishErrorMsgFilename:=
      AppendPathDelim(EnvironmentOptions.LazarusDirectory)+
        SetDirSeparators('components/codetools/fpc.errore.msg');
    CodeToolBoss.FPCDefinesCache.ExtraOptions:=
                          '-Fr'+CodeToolBoss.DefinePool.EnglishErrorMsgFilename;
  end;
end;

procedure TBuildManager.RescanCompilerDefines(ResetBuildTarget,
  ClearCaches, WaitTillDone, Quiet: boolean);
var
  TargetOS, TargetCPU: string;
  CompilerFilename: String;
  FPCSrcDir: string;
  ADefTempl: TDefineTemplate;
  FPCSrcCache: TFPCSourceCache;
  NeedUpdateFPCSrcCache: Boolean;
  IgnorePath: String;
  MsgResult: TModalResult;
  AsyncScanFPCSrcDir: String;

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
    ConfigCache: TFPCTargetConfigCache;
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
    Cfg: TFPCTargetConfigCache;
    Filename: String;
  begin
    Cfg:=UnitSetCache.GetConfigCache(false);
    if Cfg=nil then exit(true);
    if Cfg.RealCompiler='' then begin
      debugln(['PPUFilesAndCompilerMatch Compiler=',Cfg.Compiler,' RealComp=',Cfg.RealCompiler,' InPath=',Cfg.RealCompilerInPath]);
      IDEMessageDialog(lisCCOErrorCaption, Format(
        lisCompilerDoesNotSupportTarget, [Cfg.Compiler, Cfg.TargetCPU,
        Cfg.TargetOS]),
        mtError,[mbOk]);
      exit(false);
    end;
    Filename:=ReadAllLinks(Cfg.RealCompiler,false);
    if (Filename='') then begin
      IDEMessageDialog('Error','Compiler executable is missing: '+Cfg.RealCompiler,
        mtError,[mbOk]);
      exit(false);
    end;
    Result:=true;
  end;

begin
  if ClearCaches then begin
    { $IFDEF VerboseFPCSrcScan}
    debugln(['TBuildManager.RescanCompilerDefines clear caches']);
    { $ENDIF}
    CodeToolBoss.FPCDefinesCache.ConfigCaches.Clear;
    CodeToolBoss.FPCDefinesCache.SourceCaches.Clear;
  end;
  if ResetBuildTarget then
    SetBuildTarget('','','',bmsfsSkip,true);
  
  // start the compiler and ask for his settings
  // provide an english message file
  UpdateEnglishErrorMsgFilename;

  // use current TargetOS, TargetCPU, compilerfilename and FPC source dir
  TargetOS:=GetTargetOS;
  TargetCPU:=GetTargetCPU;
  CompilerFilename:=EnvironmentOptions.GetCompilerFilename;
  FPCSrcDir:=EnvironmentOptions.GetFPCSourceDirectory; // needs FPCVer macro

  {$IFDEF VerboseFPCSrcScan}
  debugln(['TMainIDE.RescanCompilerDefines A ',
    ' CompilerFilename=',CompilerFilename,
    ' TargetOS=',TargetOS,
    ' TargetCPU=',TargetCPU,
    ' EnvFPCSrcDir=',EnvironmentOptions.FPCSourceDirectory,
    ' FPCSrcDir=',FPCSrcDir,
    ' WaitTillDone=',WaitTillDone,
    ' Quiet=',Quiet,
    '']);
  {$ENDIF}

  if CompilerFilename='' then begin
    UnitSetCache:=nil;
    exit;
  end;

  UnitSetCache:=CodeToolBoss.FPCDefinesCache.FindUnitSet(
    CompilerFilename,TargetOS,TargetCPU,'',FPCSrcDir,true);

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
        //debugln(['TBuildManager.RescanCompilerDefines background scan '+FPCSrcCache.Directory]);
        AsyncScanFPCSrcDir:=FPCSrcDir;
      end;
    end;
  end;

  // scan compiler, fpc sources and create indices for quick lookup
  UnitSetCache.Init;

  if (FUnitSetChangeStamp=TFPCUnitSetCache.GetInvalidChangeStamp)
  or (FUnitSetChangeStamp<>UnitSetCache.ChangeStamp) then begin
    {$IFDEF VerboseFPCSrcScan}
    debugln(['TBuildManager.RescanCompilerDefines nothing changed']);
    {$ENDIF}
    // save caches
    SaveFPCDefinesCaches;
  end;
  FUnitSetChangeStamp:=UnitSetCache.ChangeStamp;

  {$IFDEF VerboseFPCSrcScan}
  debugln(['TBuildManager.RescanCompilerDefines UnitSet changed => rebuilding defines',
    ' ClearCaches=',ClearCaches,
    ' CompilerFilename=',CompilerFilename,
    ' TargetOS=',TargetOS,
    ' TargetCPU=',TargetCPU,
    ' RealCompiler=',UnitSetCache.GetConfigCache(false).RealCompiler,
    ' EnvFPCSrcDir=',EnvironmentOptions.FPCSourceDirectory,
    ' FPCSrcDir=',FPCSrcDir,
    '']);
  {$ENDIF}

  // rebuild the define templates
  // create template for FPC settings
  ADefTempl:=CreateFPCTemplate(UnitSetCache,nil);
  AddTemplate(ADefTempl,false,
             'NOTE: Could not create Define Template for Free Pascal Compiler');
  // create template for FPC source directory
  ADefTempl:=CreateFPCSourceTemplate(UnitSetCache,nil);
  AddTemplate(ADefTempl,false,lisNOTECouldNotCreateDefineTemplateForFreePascal);

  // create compiler macros for the lazarus sources
  if CodeToolBoss.DefineTree.FindDefineTemplateByName(StdDefTemplLazarusSrcDir,
    true)=nil
  then begin
    ADefTempl:=CreateLazarusSourceTemplate(
      '$('+ExternalMacroStart+'LazarusDir)',
      '$('+ExternalMacroStart+'LCLWidgetType)',
      MiscellaneousOptions.BuildLazOpts.ExtraOptions,nil);
    AddTemplate(ADefTempl,true,
      lisNOTECouldNotCreateDefineTemplateForLazarusSources);
  end;

  CodeToolBoss.DefineTree.ClearCache;

  if AsyncScanFPCSrcDir<>'' then begin
    // start scanning the fpc source directory in the background
    if FPCSrcScans=nil then
      FFPCSrcScans:=TFPCSrcScans.Create(Self);
    FPCSrcScans.Scan(AsyncScanFPCSrcDir);
  end;

  if not Quiet then begin
    // check for common installation mistakes
    if not PPUFilesAndCompilerMatch then exit;
    if (UnitSetCache<>nil) then begin
      // check if at least one fpc config is there
      if UnitSetCache.GetFirstFPCCfg='' then begin
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
    end else if not FoundSystemPPU then begin
      // system.ppu is missing
      IDEMessageDialog(lisCCOErrorCaption,
        Format(lisTheProjectUsesTargetOSAndCPUTheSystemPpuForThisTar, [
          TargetOS, TargetCPU, #13, #13]),
        mtError,[mbOk]);
    end;
  end;
end;

function TBuildManager.CompilerOnDiskChanged: boolean;
var
  CfgCache: TFPCTargetConfigCache;
begin
  Result:=false;
  if UnitSetCache=nil then exit;
  CfgCache:=UnitSetCache.GetConfigCache(false);
  if CfgCache=nil then exit;
  Result:=CfgCache.NeedsUpdate;
end;

procedure TBuildManager.LoadFPCDefinesCaches;
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
      CodeToolBoss.FPCDefinesCache.LoadFromXMLConfig(XMLConfig,'');
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      debugln(['LoadFPCDefinesCaches Error loadinf file '+aFilename+':'+E.Message]);
    end;
  end;
end;

procedure TBuildManager.SaveFPCDefinesCaches;
var
  aFilename: String;
  XMLConfig: TXMLConfig;
begin
  aFilename:=AppendPathDelim(GetPrimaryConfigPath)+'fpcdefines.xml';
  //debugln(['TBuildManager.SaveFPCDefinesCaches check if save needed ...']);
  if FileExistsCached(aFilename)
  and (not CodeToolBoss.FPCDefinesCache.NeedsSave) then
    exit;
  //debugln(['TBuildManager.SaveFPCDefinesCaches saving ...']);
  try
    XMLConfig:=TXMLConfig.CreateClean(aFilename);
    try
      CodeToolBoss.FPCDefinesCache.SaveToXMLConfig(XMLConfig,'');
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      debugln(['LoadFPCDefinesCaches Error loading file '+aFilename+':'+E.Message]);
    end;
  end;
end;

function TBuildManager.CheckAmbiguousSources(const AFilename: string;
  Compiling: boolean): TModalResult;

  function DeleteAmbiguousFile(const AmbiguousFilename: string): TModalResult;
  begin
    if not DeleteFileUTF8(AmbiguousFilename) then begin
      Result:=IDEMessageDialog(lisErrorDeletingFile,
       Format(lisUnableToDeleteAmbiguousFile, ['"', AmbiguousFilename, '"']),
       mtError,[mbOk,mbAbort]);
    end else
      Result:=mrOk;
  end;

  function RenameAmbiguousFile(const AmbiguousFilename: string): TModalResult;
  var
    NewFilename: string;
  begin
    NewFilename:=AmbiguousFilename+'.ambiguous';
    if not FileProcs.RenameFileUTF8(AmbiguousFilename,NewFilename) then
    begin
      Result:=IDEMessageDialog(lisErrorRenamingFile,
       Format(lisUnableToRenameAmbiguousFileTo, ['"', AmbiguousFilename, '"',
         #13, '"', NewFilename, '"']),
       mtError,[mbOk,mbAbort]);
    end else
      Result:=mrOk;
  end;

  function AddCompileWarning(const AmbiguousFilename: string): TModalResult;
  begin
    Result:=mrOk;
    if Compiling then begin
      TheOutputFilter.ReadConstLine(
        Format(lisWarningAmbiguousFileFoundSourceFileIs,
        ['"', AmbiguousFilename, '"', '"', AFilename, '"']), true);
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
          Format(lisThereIsAFileWithTheSameNameAndASimilarExtension, [#13,
            AFilename, #13, AmbiguousFilename, #13, #13]),
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
  Result:=mrOk;
  if EnvironmentOptions.AmbiguousFileAction=afaIgnore then exit;
  if EnvironmentOptions.AmbiguousFileAction
    in [afaAsk,afaAutoDelete,afaAutoRename]
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
            Format(lisAmbiguousFileFoundThisFileCanBeMistakenWithDelete, ['"',
              CurFilename, '"', #13, '"', ShortFilename, '"', #13, #13]),
            mtConfirmation,[mbYes,mbNo])=mrNo
          then continue;
        end;
        if EnvironmentOptions.AmbiguousFileAction in [afaAutoDelete,afaAsk]
        then begin
          if not DeleteFileUTF8(CurFilename) then begin
            IDEMessageDialog(lisDeleteFileFailed,
              Format(lisPkgMangUnableToDeleteFile, ['"', CurFilename, '"']),
              mtError,[mbOk]);
          end;
        end else if EnvironmentOptions.AmbiguousFileAction=afaAutoRename then
        begin
          Result:=BackupFile(CurFilename);
          if Result=mrAbort then exit;
          Result:=mrOk;
        end;
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);
  end;
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
        CurDir:=AppendPathDelim(TrimFilename(copy(
                                             UnitPath,StartPos,EndPos-StartPos)));
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
            if (CurUnitName='') or (not IsValidIdent(CurUnitName)) then
              continue;
            CurFilename:=CurDir+FileInfo.Name;
            //DebugLn(['TBuildManager.CheckUnitPathForAmbiguousPascalFiles ',CurUnitName,' ',CurFilename]);
            // check if unit already found
            ANode:=CurUnitTree.FindKey(PChar(CurUnitName),
                                 TListSortCompare(@CompareUnitNameAndUnitFile));
            if (ANode<>nil) and (not IgnoreAll) then begin
              DebugLn(['TBuildManager.CheckUnitPathForAmbiguousPascalFiles CurUnitName="',CurUnitName,'" CurFilename="',CurFilename,'" OtherUnitName="',PUnitFile(ANode.Data)^.FileUnitName,'" OtherFilename="',PUnitFile(ANode.Data)^.Filename,'"']);
              // pascal unit exists twice
              Result:=QuestionDlg(lisAmbiguousUnitFound2,
                Format(lisTheUnitExistsTwiceInTheUnitPathOfThe, [CurUnitName,
                  ContextDescription])
                +#13
                +#13
                +'1. "'+PUnitFile(ANode.Data)^.Filename+'"'#13
                +'2. "'+CurFilename+'"'#13
                +#13
                +lisHintCheckIfTwoPackagesContainAUnitWithTheSameName,
                mtWarning, [mrIgnore, mrYesToAll, lisIgnoreAll, mrAbort], 0);
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
  if Project1.IsVirtual then
    TargetExeName := GetTestBuildDirectory +
      ExtractFilename(Project1.MainUnitInfo.Filename)
  else
    TargetExeName := Project1.CompilerOptions.CreateTargetFilename(
      Project1.MainFilename);

  if not (CreateApplicationBundle(TargetExeName, Project1.GetTitle, True) in
    [mrOk, mrIgnore]) then
    Exit;
  if not (CreateAppBundleSymbolicLink(TargetExeName, True) in [mrOk, mrIgnore]) then
    Exit;
  Result := True;
end;

function TBuildManager.BackupFile(const Filename: string): TModalResult;
var BackupFilename, CounterFilename: string;
  AText,ACaption:string;
  BackupInfo: TBackupInfo;
  FilePath, FileNameOnly, FileExt, SubDir: string;
  i: integer;
  IsPartOfProject: boolean;
begin
  Result:=mrOk;
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
  if BackupInfo.SubDirectory<>'' then begin
    SubDir:=FilePath+BackupInfo.SubDirectory;
    repeat
      if not DirPathExists(SubDir) then begin
        if not CreateDirUTF8(SubDir) then begin
          Result:=IDEMessageDialog(lisCCOWarningCaption,
                   Format(lisUnableToCreateBackupDirectory, ['"',SubDir, '"'])
                   ,mtWarning,[mbAbort,mbRetry,mbIgnore]);
          if Result=mrAbort then exit;
          if Result=mrIgnore then Result:=mrOk;
        end;
      end;
    until Result<>mrRetry;
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
      BackupFilename:=SubDir+PathDelim+BackupFilename
    else
      BackupFilename:=FilePath+BackupFilename;
    // remove old backup file
    repeat
      if FileExistsUTF8(BackupFilename) then begin
        if not DeleteFileUTF8(BackupFilename) then begin
          ACaption:=lisDeleteFileFailed;
          AText:=Format(lisUnableToRemoveOldBackupFile, ['"', BackupFilename,
            '"']);
          Result:=IDEMessageDialog(ACaption,AText,mtError,
                                   [mbAbort,mbRetry,mbIgnore]);
          if Result=mrAbort then exit;
          if Result=mrIgnore then Result:=mrOk;
        end;
      end;
    until Result<>mrRetry;
  end else begin
    // backup with counter
    if BackupInfo.SubDirectory<>'' then
      BackupFilename:=SubDir+PathDelim+FileNameOnly+FileExt+';'
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
              AText:=Format(lisUnableToRemoveOldBackupFile, ['"',
                CounterFilename, '"']);
              Result:=MessageDlg(ACaption,AText,mtError,
                                 [mbAbort,mbRetry,mbIgnore],0);
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
          if not FileProcs.RenameFileUTF8(BackupFilename+IntToStr(i),
             BackupFilename+IntToStr(i+1)) then
          begin
            ACaption:=lisRenameFileFailed;
            AText:=Format(lisUnableToRenameFileTo, ['"', BackupFilename+IntToStr
              (i), '"', '"', BackupFilename+IntToStr(i+1), '"']);
            Result:=MessageDlg(ACaption,AText,mtError,
                               [mbAbort,mbRetry,mbIgnore],0);
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
    if not IDEProcs.BackupFile(Filename, BackupFilename) then
    begin
      ACaption := lisBackupFileFailed;
      AText := Format(lisUnableToBackupFileTo, ['"', Filename, '"', '"',
        BackupFilename, '"']);
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
    Result := Project1.ProjResources.ResourceType;
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
  and (not Project1.IsVirtual)
  and (pfLRSFilesInOutputDirectory in Project1.Flags) then begin
    OutputDir:=Project1.GetOutputDirectory;
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
  // update project resource
  Project1.ProjResources.Regenerate(Project1.MainFileName, False, True, TestDir);
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
      rtRes:
        if (AnUnitInfo.Source=nil) and (not AnUnitInfo.IsVirtual) then begin
          AnUnitInfo.Source:=CodeToolBoss.LoadFile(AnUnitInfo.Filename,true,false);
          Code:=AnUnitInfo.Source;
          if (Code<>nil) and (Code.DiskEncoding<>EncodingUTF8) then begin
            DebugLn(['TBuildManager.UpdateProjectAutomaticFiles fixing encoding of ',Code.Filename,' from ',Code.DiskEncoding,' to ',EncodingUTF8]);
            Code.DiskEncoding:=EncodingUTF8;
            if not Code.Save then begin
              DebugLn(['TBuildManager.UpdateProjectAutomaticFiles failed to save file ',Code.Filename]);
            end;
          end;
        end;
      end;
    end;
    AnUnitInfo := AnUnitInfo.NextPartOfProject;
  end;
end;

function TBuildManager.MacroFuncMakeExe(const Filename: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=MakeStandardExeFilename(GetTargetOS,Filename);
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
        debugln(['TBuildManager.MacroFuncInstantFPCCache ',Prog]);
        List:=RunTool(Prog,'--get-cache');
        if (List<>nil) and (List.Count>0) then
          FMacroInstantFPCCache:=List[0];
        List.Free;
      except
        on E: Exception do begin
          debugln(['TBuildManager.MacroFuncInstantFPCCache error running '+Prog+': '+E.Message]);
        end;
      end;
    end;
    debugln(['TBuildManager.MacroFuncInstantFPCCache ',FMacroInstantFPCCache]);
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
    else if SysUtils.CompareText(Param,'UnitPath')=0 then
      Result:=Project1.CompilerOptions.GetUnitPath(false)
    else if SysUtils.CompareText(Param,'InfoFile')=0 then
      Result:=Project1.ProjectInfoFile
    else if SysUtils.CompareText(Param,'OutputDir')=0 then
      Result:=Project1.CompilerOptions.GetUnitOutPath(false)
    else begin
      Result:='<Invalid parameter for macro Project:'+Param+'>';
      debugln('WARNING: TMainIDE.MacroFuncProject: ',Result);
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

function TBuildManager.MacroFuncTargetCPU(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Data=CompilerOptionMacroPlatformIndependent then
    Result:='%(CPU_TARGET)'
  else
    Result:=GetTargetCPU;
end;

function TBuildManager.MacroFuncTargetOS(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Data=CompilerOptionMacroPlatformIndependent then
    Result:='%(OS_TARGET)'
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
  else
    Result:=GetDefaultSrcOSForTargetOS(GetTargetOS);
end;

function TBuildManager.MacroFuncFPCVer(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;

  function Compute: string;
  var
    TargetOS: String;
    TargetCPU: String;
    CompilerFilename: String;
    ConfigCache: TFPCTargetConfigCache;
  begin
    Result:={$I %FPCVERSION%};   // Version.Release.Patch
    if CodeToolBoss<>nil then begin
      // fetch the FPC version from the current compiler
      // Not from the fpc.exe, but from the real compiler
      CompilerFilename:=EnvironmentOptions.GetCompilerFilename;
      if CompilerFilename='' then exit;
      TargetOS:=GetTargetOS;
      TargetCPU:=GetTargetCPU;
      ConfigCache:=CodeToolBoss.FPCDefinesCache.ConfigCaches.Find(
                                   CompilerFilename,'',TargetOS,TargetCPU,true);
      if ConfigCache=nil then exit;
      if ConfigCache.NeedsUpdate then begin
        // ask compiler
        if not ConfigCache.Update(CodeToolBoss.FPCDefinesCache.TestFilename,
                                  CodeToolBoss.FPCDefinesCache.ExtraOptions,nil)
        then
          exit;
      end;
      Result:=ConfigCache.GetFPCVer;
    end;
  end;

begin
  if FFPCVerChangeStamp<>CompilerParseStamp then
  begin
    FFPCVer:=Compute;
    FFPCVerChangeStamp:=CompilerParseStamp;
    {$IFDEF VerboseFPCSrcScan}
    debugln(['TBuildManager.MacroFuncFPCVer ',FFPCVer]);
    {$ENDIF}
  end;
  Result:=FFPCVer;
end;

function TBuildManager.MacroFuncParams(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=Project1.RunParameterOptions.CmdLineParams
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
    Result:=Project1.ProjectDirectory
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
  if Project1<>nil then begin
    Result:=Project1.RunParameterOptions.CmdLineParams;
    if Result='' then
      Result:=GetProjectTargetFilename(Project1)
    else
      Result:=GetProjectTargetFilename(Project1)+' '+Result;
  end else
    Result:='';
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
  if Project1<>nil then
    Result:=GetProjectPublishDir
  else
    Result:='';
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

function TBuildManager.MacroFuncFPCMsgFile(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=EnvironmentOptions.CompilerMessagesFilename;
end;

function TBuildManager.MacroFuncMake(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  Result:=EnvironmentOptions.MakeFilename;
  if (Result<>'') and (not FilenameIsAbsolute(Result)) then
    Result:=FindDefaultExecutablePath(Result);
  if Result='' then
    Result:=FindDefaultMakePath;
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

procedure TBuildManager.OnCmdLineCreate(var CmdLine: string; var Abort: boolean);
// replace all transfer macros in command line
begin
  Abort:=not GlobalMacroList.SubstituteStr(CmdLine);
end;

function TBuildManager.OnRunCompilerWithOptions(
  ExtTool: TIDEExternalToolOptions; CompOptions: TBaseCompilerOptions): TModalResult;
begin
  if SourceEditorManagerIntf<>nil then
    SourceEditorManagerIntf.ClearErrorLines;
  Result:=EnvironmentOptions.ExternalTools.Run(ExtTool,GlobalMacroList,false,
                                               CompOptions);
  if LazarusIDE<>nil then
    LazarusIDE.DoCheckFilesOnDisk;
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
      PackageGraph.GetAllRequiredPackages(FirstDependency,List);
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

  procedure SetProjectMacroValues;
  var
    Values: TCTCfgScriptVariables;
  begin
    Values:=OnGetBuildMacroValues(nil,false);
    if Values<>nil then
      OnGetBuildMacroValues.AddOverrides(Values);
  end;

var
  ParseOpts: TParsedCompilerOptions;
  Values: TCTCfgScriptVariables;
  Overrides: TStrings;
  i: Integer;
  s: String;
  aName: string;
  aValue: String;
  MainMacroValues: TProjectBuildMacros;
begin
  Result:=nil;
  if Options=nil then begin
    // return the values of the active project
    if (Project1<>nil) and (Project1.MacroValues<>nil) then begin
      MainMacroValues:=Project1.MacroValues;
      Result:=MainMacroValues.CfgVars;
      if MainMacroValues.CfgVarsBuildMacroStamp=BuildMacroChangeStamp then
        exit;
      // rebuild main macros
      Result.Clear;
      for i:=0 to MainMacroValues.Count-1 do begin
        aName:=MainMacroValues.Names[i];
        aValue:=MainMacroValues.ValueFromIndex(i);
        //debugln(['TBuildManager.OnGetBuildMacroValues project override: ',aName,'="',aValue,'"']);
        Result.Define(PChar(aName),aValue);
      end;
      {$IFDEF VerboseBuildMacros}
      Result.WriteDebugReport('OnGetBuildMacroValues project overrides');
      {$ENDIF}
      MainMacroValues.CfgVarsBuildMacroStamp:=BuildMacroChangeStamp;
    end else if DefaultCfgVars<>nil then begin
      // there is no project => use defaults
      Result:=DefaultCfgVars;
      if DefaultCfgVarsBuildMacroStamp=BuildMacroChangeStamp then
        exit;
      // rebuild main macros
      Result.Clear;
      DefaultCfgVarsBuildMacroStamp:=BuildMacroChangeStamp;
    end else
      exit;

    // set overrides
    Overrides:=GetBuildMacroOverrides;
    try
      for i:=0 to Overrides.Count-1 do
        Result.Values[Overrides.Names[i]]:=Overrides.ValueFromIndex[i];
      {$IFDEF VerboseBuildMacros}
      debugln(['TBuildManager.OnGetBuildMacroValues cmd line overrides=',dbgstr(Overrides.Text)]);
      {$ENDIF}
    finally
      Overrides.Free;
    end;

    // add the defaults
    // Note: see also ide/frames/compiler_buildmacro_options.pas procedure TCompOptBuildMacrosFrame.BuildMacrosTreeViewEdited
    if not Result.IsDefined('TargetOS') then begin
      s:='';
      if Project1<>nil then
        s:=Project1.CompilerOptions.TargetOS;
      if s='' then
        s:=GetDefaultTargetOS;
      Result.Values['TargetOS']:=s;
    end;
    if not Result.IsDefined('SrcOS') then begin
      s:=GetDefaultSrcOSForTargetOS(Result.Values['TargetOS']);
      Result.Values['SrcOS']:=s;
    end;
    if not Result.IsDefined('SrcOS2') then begin
      s:=GetDefaultSrcOS2ForTargetOS(Result.Values['TargetOS']);
      Result.Values['SrcOS2']:=s;
    end;
    if not Result.IsDefined('TargetCPU') then begin
      s:='';
      if Project1<>nil then
        s:=Project1.CompilerOptions.TargetCPU;
      if s='' then
        s:=GetDefaultTargetCPU;
      Result.Values['TargetCPU']:=s;
    end;
    if Result.Values['LCLWidgetType']='' then begin
      s:='';
      if Project1<>nil then
        s:=Project1.CompilerOptions.GetEffectiveLCLWidgetType;
      if s='' then
        s:=LCLPlatformDirNames[GetDefaultLCLWidgetType];
      Result.Values['LCLWidgetType']:=s;
    end;

    {$IFDEF VerboseBuildMacros}
    Result.WriteDebugReport('OnGetBuildMacroValues project values');
    {$ENDIF}
    exit;
  end;

  ParseOpts:=Options.ParsedOpts;
  if ParseOpts=nil then exit;

  if IncludeSelf then begin
    Result:=ParseOpts.MacroValues.Variables;

    if ParseOpts.MacroValuesStamp<>BuildMacroChangeStamp then begin
      // compute macro values

      if ParseOpts.MacroValuesParsing then begin
        debugln(['TBuildManager.OnGetBuildMacroValues circle computing macros of ',dbgsname(Options.Owner)]);
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
        {$IFDEF VerboseBuildMacros}
        Result.WriteDebugReport('TPkgManager.OnGetBuildMacroValues before execute: '+dbgstr(Options.Conditionals),'  ');
        {$ENDIF}
        if not ParseOpts.MacroValues.Execute(Options.Conditionals) then begin
          debugln(['TPkgManager.OnGetBuildMacroValues Error: ',ParseOpts.MacroValues.GetErrorStr(0)]);
          debugln(Options.Conditionals);
        end;

        {$IFDEF VerboseBuildMacros}
        Result.WriteDebugReport('TPkgManager.OnGetBuildMacroValues executed: '+dbgstr(Options.Conditionals),'  ');
        {$ENDIF}

        // the macro values of the active project take precedence
        SetProjectMacroValues;

        ParseOpts.MacroValuesStamp:=BuildMacroChangeStamp;
      finally
        ParseOpts.MacroValuesParsing:=false;
      end;
    end;
  end else begin
    Result:=ParseOpts.InheritedMacroValues;

    if ParseOpts.InheritedMacroValuesStamp<>BuildMacroChangeStamp then begin
      // compute inherited values
      if ParseOpts.InheritedMacroValuesParsing then begin
        debugln(['TPkgManager.OnGetBuildMacroValues circle computing inherited macros of ',dbgsname(Options.Owner)]);
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
        SetProjectMacroValues;

        ParseOpts.InheritedMacroValuesStamp:=BuildMacroChangeStamp;
      finally
        ParseOpts.InheritedMacroValuesParsing:=false;
      end;
    end;
  end;
end;

procedure TBuildManager.SetBuildTarget(const TargetOS, TargetCPU,
  LCLWidgetType: string; ScanFPCSrc: TBMScanFPCSources; Quiet: boolean);
var
  OldTargetOS: String;
  OldTargetCPU: String;
  OldLCLWidgetType: String;
  FPCTargetChanged: Boolean;
  LCLTargetChanged: Boolean;
begin
  OldTargetOS:=fTargetOS;
  OldTargetCPU:=fTargetCPU;
  OldLCLWidgetType:=fLCLWidgetType;
  OverrideTargetOS:=GetFPCTargetOS(TargetOS);
  OverrideTargetCPU:=GetFPCTargetCPU(TargetCPU);
  OverrideLCLWidgetType:=lowercase(LCLWidgetType);

  // compute new TargetOS
  if OverrideTargetOS<>'' then
    fTargetOS:=OverrideTargetOS
  else if Project1<>nil then
    fTargetOS:=Project1.CompilerOptions.TargetOS
  else
    fTargetOS:='';
  if (fTargetOS='') or (SysUtils.CompareText(fTargetOS,'default')=0) then
    fTargetOS:=GetDefaultTargetOS;
  fTargetOS:=GetFPCTargetOS(fTargetOS);

  // compute new TargetCPU
  if OverrideTargetCPU<>'' then
    fTargetCPU:=OverrideTargetCPU
  else if Project1<>nil then
    fTargetCPU:=Project1.CompilerOptions.TargetCPU
  else
    fTargetCPU:='';
  if (fTargetCPU='') or (SysUtils.CompareText(fTargetCPU,'default')=0) then
    fTargetCPU:=GetDefaultTargetCPU;
  fTargetCPU:=GetFPCTargetCPU(fTargetCPU);

  // compute new LCLWidgetType
  if OverrideLCLWidgetType<>'' then
    fLCLWidgetType:=OverrideLCLWidgetType
  else if Project1<>nil then
    fLCLWidgetType:=Project1.CompilerOptions.GetEffectiveLCLWidgetType
  else
    fLCLWidgetType:='';
  if (fLCLWidgetType='') or (SysUtils.CompareText(fLCLWidgetType,'default')=0) then
    fLCLWidgetType:=LCLPlatformDirNames[GetDefaultLCLWidgetType];
  fLCLWidgetType:=lowercase(fLCLWidgetType);

  FPCTargetChanged:=(OldTargetOS<>fTargetOS)
                    or (OldTargetCPU<>fTargetCPU)
                    or (CodeToolBoss.DefineTree.FindDefineTemplateByName(
                         StdDefTemplLazarusSrcDir,true)=nil);
  LCLTargetChanged:=(OldLCLWidgetType<>fLCLWidgetType);

  if FPCTargetChanged or LCLTargetChanged then begin
    //DebugLn('TMainIDE.SetBuildTarget Old=',OldTargetCPU,'-',OldTargetOS,'-',OldLCLWidgetType,
    //  ' New=',fTargetCPU,'-',fTargetOS,'-',fLCLWidgetType,' FPC=',dbgs(FPCTargetChanged),' LCL=',dbgs(LCLTargetChanged));
    IncreaseBuildMacroChangeStamp;
  end;
  if LCLTargetChanged then
    CodeToolBoss.SetGlobalValue(ExternalMacroStart+'LCLWidgetType',fLCLWidgetType);
  if FPCTargetChanged and (ScanFPCSrc<>bmsfsSkip) then
    RescanCompilerDefines(false,false,ScanFPCSrc=bmsfsWaitTillDone,Quiet);
  //if (PackageGraph<>nil) and (PackageGraph.CodeToolsPackage<>nil) then debugln(['TBuildManager.SetBuildTarget CODETOOLS OUTDIR=',PackageGraph.CodeToolsPackage.CompilerOptions.GetUnitOutPath(true,coptParsed),' ',PackageGraph.CodeToolsPackage.CompilerOptions.ParsedOpts.ParsedStamp[pcosOutputDir],' ',CompilerParseStamp]);
end;

procedure TBuildManager.SetBuildTargetProject1(Quiet: boolean;
  ScanFPCSrc: TBMScanFPCSources);
begin
  SetBuildTarget('','','',ScanFPCSrc,Quiet);
end;

procedure TBuildManager.SetBuildTargetIDE;
var
  NewTargetOS: String;
  NewTargetCPU: String;
  NewLCLWidgetSet: String;
begin
  with MiscellaneousOptions do begin
    NewTargetOS:=BuildLazOpts.TargetOS;
    NewTargetCPU:=BuildLazOpts.TargetCPU;
    NewLCLWidgetSet:=LCLPlatformDirNames[BuildLazOpts.TargetPlatform];
  end;
  if (NewTargetOS='') or (NewTargetOS='default') then
    NewTargetOS:=GetDefaultTargetOS;
  if (NewTargetCPU='') or (NewTargetCPU='default') then
    NewTargetCPU:=GetDefaultTargetCPU;
  debugln(['TBuildManager.SetBuildTargetIDE OS=',NewTargetOS,' CPU=',NewTargetCPU,' WS=',NewLCLWidgetSet]);
  SetBuildTarget(NewTargetOS,NewTargetCPU,NewLCLWidgetSet,bmsfsBackground,false);
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
  Result:=((NewTargetOS='') or (NewTargetOS=GetDefaultTargetOS))
      and ((NewTargetCPU='') or (NewTargetCPU=GetDefaultTargetCPU))
      and (NewLCLWidgetSet<>lpNoGUI);
end;

end.

