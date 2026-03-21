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
  Classes, SysUtils, AVL_Tree, System.UITypes,
  // LCL
  InterfaceBase, LCLPlatformDef,
  // CodeTools
  ExprEval, BasicCodeTools, CodeToolManager, DefineTemplates, CodeCache,
  FileProcs, CodeToolsCfgScript, LinkScanner,
  // LazUtils
  FPCAdds, LConvEncoding, FileUtil, LazFileUtils, LazFileCache, LazUTF8,
  Laz2_XMLCfg, LazUtilities, LazMethodList, LazVersion,
  AvgLvlTree,
  // BuildIntf
  BaseIDEIntf, IDEOptionsIntf, ProjectIntf, ProjectResourcesIntf,
  PublishModuleIntf, IDEExternToolIntf, CompOptsIntf, MacroDefIntf,
  // IDEIntf
  IDEDialogs, LazIDEIntf, IDEMsgIntf, SrcEditorIntf,
  // IdeUtils
  InputHistory,
  // IdeConfig
  LazConf, EnvironmentOpts, ModeMatrixOpts, TransferMacros, IdeConfStrConsts,
  IDEProcs, DialogProcs, etMakeMsgParser, etFPCMsgFilePool, EditDefineTree,
  ParsedCompilerOpts, CompilerOptions, Compiler, SearchPathProcs, BaseBuildManager,
  // IdePackager
  IdePackagerStrConsts, PackageDefs, PackageSystem,
  // IdeProject
  Project, ProjectResources, ProjectIcon,
  // IDE
  LazarusIDEStrConsts, LfmUnitResource, MiscOptions, etFPCMsgParser, etPas2jsMsgParser,
  FPCSrcScan, ApplicationBundle, IdeTransferMacros, ExtTools;

type

  { TBuildManager }

  TBuildManager = class(TBaseBuildManager)
  private
    FUnitSetCache: TFPCUnitSetCache;
    fBuildLazExtraOptions: string; // last build lazarus extra options
    FUnitSetChangeStamp: integer;
    FFPCSrcScans: TFPCSrcScans;
    procedure DoOnRescanFPCDirectoryCache(Sender: TObject);
    function GetBuildTarget: TProject;
    function MacroFuncBuildModeCaption(const {%H-}Param: string; const {%H-}Data: PtrInt;
                             var {%H-}Abort: boolean): string;
    function MacroFuncIDEBuildOptions(const {%H-}Param: string; const Data: PtrInt;
                               var {%H-}Abort: boolean): string;
    function MacroFuncProjVer(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                   var {%H-}Abort: boolean): string;
    function MacroFuncProjPublishDir(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                     var {%H-}Abort: boolean): string;
    procedure ProjectDestroy(Sender: TObject);
    procedure SetUnitSetCache(const AValue: TFPCUnitSetCache);
    procedure WriteError(const Msg: string);
  protected
    // command line overrides
    OverrideTargetOS: string;
    OverrideTargetCPU: string;
    OverrideSubtarget: string;
    OverrideLCLWidgetType: string;
    DefaultCfgVars: TCTCfgScriptVariables;
    DefaultCfgVarsBuildMacroStamp: integer;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetBuildMacroValuesHandler(Options: TLazCompilerOptions;
                                   IncludeSelf: boolean): TCTCfgScriptVariables;
    procedure AppendMatrixCustomOption(Sender: TObject;
      var Options: string; Types: TBuildMatrixGroupTypes);
    procedure GetMatrixOutputDirectoryOverride(Sender: TObject;
      var OutDir: string; Types: TBuildMatrixGroupTypes);
    function GetModeMatrixTarget(Sender: TObject): string;
    function EnvironmentOptionsIsGlobalMode(const Identifier: string): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetupTransferMacros; override;
    procedure SetupExternalTools(aToolsClass: TExternalToolsClass);
    procedure SetupCompilerInterface;
    procedure SetupInputHistories(aInputHist: TInputHistories);
    procedure EnvOptsChanged;

    function GetBuildMacroOverride(const MacroName: string): string; override;
    function GetBuildMacroOverrides: TStrings; override;
    function GetRunWorkingDir: string; override;
    procedure WriteDebug_RunCommandLine; override;

    function GetFPCFrontEndOptions: string; override;
    function GetProjectPublishDir: string; override;
    function GetTestUnitFilename(AnUnitInfo: TLazProjectFile): string; override;
    function GetTestBuildDirectory: string; override;
    function IsTestUnitFilename(const AFilename: string): boolean; override;
    function GetTargetUnitFilename(AnUnitInfo: TLazProjectFile): string; override;

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
    function DeleteAmbiguousFiles(const Filename:string): TModalResult; override;
    function CheckUnitPathForAmbiguousPascalFiles(const BaseDir, TheUnitPath,
                CompiledExt, ContextDescription: string): TModalResult; override;
    function CreateProjectApplicationBundle: Boolean; override;
    function BackupFileForWrite(const Filename: string): TModalResult; override;

    function GetResourceType(AnUnitInfo: TUnitInfo): TProjResourceType;
    function FindLRSFilename(AnUnitInfo: TUnitInfo; UseDefaultIfNotFound: boolean): string;
    function GetDefaultLRSFilename(AnUnitInfo: TUnitInfo): string;
    function UpdateLRSFromLFM(AnUnitInfo: TUnitInfo; ShowAbort: boolean): TModalResult;
    function UpdateProjectAutomaticFiles(TestDir: string): TModalResult; override;

    // methods for building IDE (will be changed when project groups are there)
    procedure SetBuildTarget(const TargetOS, TargetCPU, Subtarget, LCLWidgetType: string;
                             ScanFPCSrc: TScanModeFPCSources; Quiet: boolean);
    procedure SetBuildTargetProject1; override; overload;
    procedure SetBuildTargetProject1(Quiet: boolean; ScanFPCSrc: TScanModeFPCSources = smsfsBackground); overload;
    procedure SetBuildTargetIDE(aQuiet: boolean = false); override;
    function BuildTargetIDEIsDefault: boolean; override;

    property FPCSrcScans: TFPCSrcScans read FFPCSrcScans;
    property BuildTarget: TProject read GetBuildTarget; // TProject or nil
  end;

var
  MainBuildBoss: TBuildManager = nil;
  TheCompiler: TCompiler = nil;

procedure RunBootHandlers(ht: TLazarusIDEBootHandlerType); external name 'ideintf_LazIDEIntf_RunBootHandlers';


implementation

type
  TUnitFile = record
    FileUnitName: string;
    Filename: string;
  end;
  PUnitFile = ^TUnitFile;

function CompareUnitFiles(UnitFile1, UnitFile2: PUnitFile): integer;
begin
  Result:=CompareIdentifiers(PChar(UnitFile1^.FileUnitName),
                             PChar(UnitFile2^.FileUnitName));
end;

function CompareUnitNameAndUnitFile(UnitName: PChar; UnitFile: PUnitFile): integer;
begin
  Result:=CompareIdentifiers(PChar(UnitName),PChar(UnitFile^.FileUnitName));
end;

procedure OnCompilerParseStampIncreased;
begin
  CodeToolBoss.DefineTree.ClearCache;
end;

{ TBuildManager }

constructor TBuildManager.Create(AOwner: TComponent);
begin
  DefaultCfgVars:=TCTCfgScriptVariables.Create;
  DefaultCfgVarsBuildMacroStamp:=CTInvalidChangeStamp;
  MainBuildBoss:=Self;
  inherited Create(AOwner);
  EnvironmentOptions.IsGlobalMode:=@EnvironmentOptionsIsGlobalMode;
  FUnitSetChangeStamp:=TFPCUnitSetCache.GetInvalidChangeStamp;
  OnBackupFileInteractive:=@BackupFileForWrite;
  GetBuildMacroValues:=@GetBuildMacroValuesHandler;
  OnAppendCustomOption:=@AppendMatrixCustomOption;
  OnGetMatrixOutputDirectoryOverride:=@GetMatrixOutputDirectoryOverride;
  CodeToolBoss.OnRescanFPCDirectoryCache:=@DoOnRescanFPCDirectoryCache;
end;

destructor TBuildManager.Destroy;
begin
  ExternalToolList.Free; // sets ExternalToolList to nil, do not use FreeAndNil!

  GetBuildMacroValues:=nil;
  OnAppendCustomOption:=nil;
  OnBackupFileInteractive:=nil;

  FreeAndNil(FFPCSrcScans);
  LazConfMacroFunc:=nil;
  FreeAndNil(InputHistories);
  FreeAndNil(DefaultCfgVars);

  if SameMethod(TMethod(CodeToolBoss.OnRescanFPCDirectoryCache),
                TMethod(@DoOnRescanFPCDirectoryCache)) then
    CodeToolBoss.OnRescanFPCDirectoryCache:=nil;

  inherited Destroy;
  MainBuildBoss:=nil;
end;

procedure TBuildManager.SetupTransferMacros;
begin
  inherited SetupTransferMacros;
  // Cannot do these in the base class.
  GlobalMacroList.Add(TTransferMacro.Create('BuildModeCaption','',
                      lisCaptionOfActiveBuildMode, @MacroFuncBuildModeCaption, []));
  GlobalMacroList.Add(TTransferMacro.Create('ProjVer','',
                      lisProjectVer,@MacroFuncProjVer,[]));
  GlobalMacroList.Add(TTransferMacro.Create('IDEBuildOptions','',
                      lisIDEBuildOptions, @MacroFuncIDEBuildOptions, []));
  GlobalMacroList.Add(TTransferMacro.Create('ProjPublishDir','',
                      lisPublishProjDir,@MacroFuncProjPublishDir,[]));
  // Can be done in inherited method later.
  TIdeTransferMarcros.InitMacros(GlobalMacroList);
  RunBootHandlers(libhTransferMacrosCreated);
end;

function TBuildManager.MacroFuncBuildModeCaption(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  if (Project1 <> nil) and (Project1.BuildModes.Count > 1) then
    Result := Project1.ActiveBuildMode.GetCaption
  else
    Result:='';
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

function TBuildManager.MacroFuncProjVer(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
const
  cParamNames: array of string = ('', 'major', 'minor', 'rev', 'build');
  cParamDefVals: array of string = ('0.0', '0', '0', '0', '0');
var
  i: integer;
begin
  for i := 0 to high(cParamNames) do
    if CompareText(Param, cParamNames[i]) = 0 then
    begin
      // check the project and whether the version is used
      Result := cParamDefVals[i];
      if Project1 = nil then exit;
      if Project1.ProjResources = nil then exit;
      if Project1.ProjResources.VersionInfo = nil then exit;
      if Project1.ProjResources.VersionInfo.UseVersionInfo = false then exit;

      // return version or specified number
      with Project1.ProjResources.VersionInfo do
        case i of
          1: exit(IntToStr(MajorVersionNr));
          2: exit(IntToStr(MinorVersionNr));
          3: exit(IntToStr(RevisionNr    ));
          4: exit(IntToStr(BuildNr       ));
        else
          // return the full version number, discarding the zero revision and build
          if BuildNr <> 0 then
            exit(Format('%d.%d.%d.%d', [MajorVersionNr, MinorVersionNr, RevisionNr, BuildNr]))
          else if RevisionNr <> 0 then
            exit(Format('%d.%d.%d'   , [MajorVersionNr, MinorVersionNr, RevisionNr]))
          else
            exit(Format('%d.%d'      , [MajorVersionNr, MinorVersionNr]));
        end;
    end;
  Result := ''; // invalid parameter
end;

function TBuildManager.MacroFuncProjPublishDir(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetProjectPublishDir;
end;

procedure TBuildManager.ProjectDestroy(Sender: TObject);
var
  aProject: TProject;
begin
  if not (Sender is TProjectIDEOptions) then
    exit;
  aProject:=TProjectIDEOptions(Sender).Project;
  if FBuildTarget=aProject then
    FBuildTarget:=nil;
end;

procedure TBuildManager.WriteError(const Msg: string);
begin
  DebugLn(Msg,' [TBuildManager.WriteError]');
  if IDEMessagesWindow<>nil then
    IDEMessagesWindow.AddCustomMessage(mluError,Msg);
end;

procedure TBuildManager.SetupExternalTools(aToolsClass: TExternalToolsClass);
var
  Tools: TExternalTools;
begin
  // setup the external tool queue
  Tools:=aToolsClass.Create(Self);
  if Tools<>ExternalToolList then
    raise Exception.Create('TBuildManager.SetupExternalTools ExternalTools='+DbgSName(ExternalToolList));
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
  TheCompiler.OnWriteError := @WriteError;
end;

procedure TBuildManager.SetupInputHistories(aInputHist: TInputHistories);
begin
  aInputHist.SetLazarusDefaultFilename;
  aInputHist.Load;
end;

procedure TBuildManager.EnvOptsChanged;
begin
  if EnvironmentOptions.MaxExtToolsInParallel<=0 then
    ExternalToolsRef.MaxProcessCount:=DefaultMaxProcessCount
  else
    ExternalToolsRef.MaxProcessCount:=EnvironmentOptions.MaxExtToolsInParallel;
end;

function TBuildManager.GetBuildMacroOverride(const MacroName: string): string;
begin
  Result:='';
  if SysUtils.CompareText(MacroName,'TargetOS')=0 then
    Result:=OverrideTargetOS
  else if SysUtils.CompareText(MacroName,'TargetCPU')=0 then
    Result:=OverrideTargetCPU
  else if SysUtils.CompareText(MacroName,'Subtarget')=0 then
    Result:=OverrideSubtarget
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
  if OverrideSubtarget<>'' then
    Result.Values['Subtarget']:=OverrideSubtarget;
  if OverrideLCLWidgetType<>'' then
    Result.Values['LCLWidgetType']:=OverrideLCLWidgetType;
end;

function TBuildManager.GetRunWorkingDir: string;
var
  AMode: TAbstractRunParamsOptionsMode;
begin
  Result := '';
  if Project1=nil then exit;

  // first take the WorkDir from the active run parameters
  AMode := Project1.RunParameterOptions.GetActiveMode;
  if AMode<>nil then
    Result := AMode.WorkingDirectory;
  if not GlobalMacroList.SubstituteStr(Result) then
    Result := '';
  if (Result <> '') and not FilenameIsAbsolute(Result) then
    Result := CreateAbsolutePath(Result, Project1.Directory);

  // then use the directory of the produced exe
  if Result='' then begin
    Result := ExtractFilePath(BuildBoss.GetProjectTargetFilename(Project1));
    if (Result <> '') and not FilenameIsAbsolute(Result) then
      Result := CreateAbsolutePath(Result, Project1.Directory);
  end;

  // finally use the project directory
  if (Result='') and (not Project1.IsVirtual) then
    Result := ChompPathDelim(Project1.Directory);
end;

procedure TBuildManager.WriteDebug_RunCommandLine;
var
  AMode: TAbstractRunParamsOptionsMode; //TRunParamsOptionsMode;
  s, TargetFilename: String;
begin
  s:='';
  if Project1=nil then
  begin
    debugln(['Note: (lazarus) [TBuildManager.WriteDebug_RunCommandLine] Project1=nil RunCmdLine=[',GetRunCommandLine,']']);
  end else begin
    AMode := Project1.RunParameterOptions.GetActiveMode;
    if AMode<>nil then
      debugln(['Note: (lazarus) [TBuildManager.WriteDebug_RunCommandLine] AMode="',AMode.Name,'" AMode.WorkingDirectory=[',AMode.WorkingDirectory,']'])
    else
      debugln(['Note: (lazarus) [TBuildManager.WriteDebug_RunCommandLine] AMode=nil']);
    if (AMode<>nil) and AMode.UseLaunchingApplication then
    begin
      s := AMode.LaunchingApplicationPathPlusParams;
      debugln(['Note: (lazarus) [TBuildManager.WriteDebug_RunCommandLine] LaunchingApplicationPathPlusParams=[',s,']']);
    end;

    if s='' then
    begin
      // no launching app
      debugln(['Note: (lazarus) [TBuildManager.WriteDebug_RunCommandLine] no LaunchingApplication']);
      if (AMode<>nil) then
      begin
        s := AMode.CmdLineParams;
        if s<>'' then
          debugln(['Note: (lazarus) [TBuildManager.WriteDebug_RunCommandLine] AMode.CmdLineParams=[',s,']']);
      end;
      TargetFilename := GetTargetFilename;
      if (TargetFilename <> '')
      and (TargetFilename[Length(TargetFilename)] in AllowDirectorySeparators) then
        TargetFilename += ExtractFileNameOnly(
                       Project1.CompilerOptions.GetDefaultMainSourceFileName);

      debugln(['Note: (lazarus) [TBuildManager.WriteDebug_RunCommandLine] TargetFilename=[',TargetFilename,']']);
    end;
    debugln(['Note: (lazarus) [TBuildManager.WriteDebug_RunCommandLine] Project1<>nil RunCmdLine=[',GetRunCommandLine,']']);
  end;
end;

function TBuildManager.GetFPCFrontEndOptions: string;
var
  s, CfgFilename: String;
  Opts: TBaseCompilerOptions; //TProjectCompilerOptions;
begin
  Result:='';
  if FBuildTarget<>nil then
  begin
    Opts:=TBaseCompilerOptions(FBuildTarget.LazCompilerOptions);
    s:=ExtractFPCFrontEndParameters(Opts.CustomOptions);
    if GlobalMacroList.SubstituteStr(s) then
    begin
      if s<>'' then
        Result:=s;
    end else begin
      debugln(['Warning: (lazarus) [GetFPCFrontEndOptions] ignoring invalid macros in custom options for fpc frontend: "',
               ExtractFPCFrontEndParameters(Opts.CustomOptions),'"']);
    end;
    if Opts.CustomConfigFile and (Opts.ConfigFilePath<>'') then
    begin
      CfgFilename:=Opts.ParsedOpts.DoParseOption(Opts.ConfigFilePath, pcosCustomConfigFilePath, false);
      if CfgFilename<>'' then
      begin
        if Result<>'' then Result+=' ';
        Result+='@'+CfgFilename;
      end;
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

function TBuildManager.GetTestUnitFilename(AnUnitInfo: TLazProjectFile): string;
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

function TBuildManager.GetTargetUnitFilename(AnUnitInfo: TLazProjectFile): string;
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
               AnsiQuotedStr('-Fr'+CodeToolBoss.DefinePool.EnglishErrorMsgFilename,'"');
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
    if not FilenameExtIs(AFilename,'ppu',true) then exit;
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
          debugln(['Error: (lazarus) [PPUFilesAndCompilerMatch] Compiler=',Cfg.Compiler,' RealComp=',Cfg.RealCompiler,' InPath=',Cfg.RealTargetCPUCompiler]);
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
  TargetOS, TargetCPU, Subtarget, FPCOptions: string;
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
  Msg, DefCompilerFilename, ProjCompilerFilename, ProjCompilerErrorMsg,
    DefCompilerErrorMsg, WorkDir: String;
  CompilerKind, ProjCompilerKind, DefCompilerKind: TPascalCompiler;
  Opts: TProjectCompilerOptions;
begin
  if ClearCaches then begin
    {$IFDEF VerboseFPCSrcScan}
    debugln(['TBuildManager.RescanCompilerDefines clear caches']);
    {$ENDIF}
    CodeToolBoss.CompilerDefinesCache.ConfigCaches.Clear;
    CodeToolBoss.CompilerDefinesCache.SourceCaches.Clear;
    CodeToolBoss.SourceCache.ClearIncludedBy_FPCNamespaced('*');
  end;
  if ResetBuildTarget then
    SetBuildTarget('','','','',smsfsSkip,true);

  // start the compiler and ask for his settings
  // provide an english message file
  UpdateEnglishErrorMsgFilename;

  // use current TargetOS, TargetCPU, compilerfilename and FPC source dir
  TargetOS:=GetTargetOS;
  TargetCPU:=GetTargetCPU;
  Subtarget:=GetSubtarget;
  {$IFDEF VerboseFPCSrcScan}
  debugln(['TBuildManager.RescanCompilerDefines GetParsedFPCSourceDirectory needs FPCVer...']);
  {$ENDIF}
  CompilerFilename:=GetCompilerFilename;
  IsCompilerExecutable(CompilerFilename,CompilerErrorMsg,CompilerKind,{$IFDEF VerboseFPCSrcScan}true{$ELSE}false{$ENDIF});
  FPCSrcDir:=EnvironmentOptions.GetParsedFPCSourceDirectory; // needs FPCVer macro
  FPCOptions:=GetFPCFrontEndOptions;

  {$IFDEF VerboseFPCSrcScan}
  debugln(['TMainIDE.RescanCompilerDefines START ',
    ' CompilerFilename=',CompilerFilename,
    ' Kind=',PascalCompilerNames[CompilerKind],
    ' TargetOS=',TargetOS,
    ' TargetCPU=',TargetCPU,
    ' Subtarget=',Subtarget,
    ' FPCOptions="',FPCOptions,'"',
    ' EnvFPCSrcDir=',EnvironmentOptions.FPCSourceDirectory,
    ' FPCSrcDir=',FPCSrcDir,
    ' WaitTillDone=',WaitTillDone,
    ' Quiet=',Quiet,
    ' ClearCaches=',ClearCaches,
    '']);
  {$ENDIF}

  // first check the default targetos, targetcpu of the default compiler
  DefCompilerFilename:=EnvironmentOptions.GetParsedCompilerFilename;
  if FileExistsCached(DefCompilerFilename) then
  begin
    {$IFDEF VerboseFPCSrcScan}
    debugln(['TBuildManager.RescanCompilerDefines reading default compiler settings']);
    {$ENDIF}
    UnitSetCache:=CodeToolBoss.CompilerDefinesCache.FindUnitSet(
      DefCompilerFilename,'','','','',FPCSrcDir,'',true);
    UnitSetCache.GetConfigCache(true);
  end;

  if CompilerFilename<>DefCompilerFilename then
    IsCompilerExecutable(CompilerFilename,CompilerErrorMsg,CompilerKind,true);

  // then check the project's compiler
  if (CompilerErrorMsg<>'') then begin
    Msg:='';
    if FBuildTarget<>nil then begin
      Opts:=TProjectCompilerOptions(FBuildTarget.LazCompilerOptions);
      if ([crCompile,crBuild]*Opts.CompileReasons<>[]) and (Opts.CompilerPath<>'')
      then begin
        ProjCompilerFilename:=Opts.ParsedOpts.GetParsedValue(pcosCompilerPath);
        if not IsCompilerExecutable(ProjCompilerFilename,ProjCompilerErrorMsg,ProjCompilerKind,true)
        then begin
          Msg+='Project''s compiler: "'+ProjCompilerFilename+'": '+ProjCompilerErrorMsg+LineEnding;
        end;
      end;
    end;

    if not IsCompilerExecutable(DefCompilerFilename,DefCompilerErrorMsg,DefCompilerKind,true)
    then begin
      Msg+='Environment compiler: "'+DefCompilerFilename+'": '+DefCompilerErrorMsg+LineEnding;
    end;
    if Msg='' then
      Msg+='Compiler: "'+CompilerFilename+'": '+CompilerErrorMsg+LineEnding;

    debugln('Warning: (lazarus) [TBuildManager.RescanCompilerDefines]: invalid compiler:');
    debugln(Msg);
    if not Quiet then begin
      IDEMessageDialog(lisCCOErrorCaption, Format(
        lisThereIsNoFreePascalCompilerEGFpcOrPpcCpuConfigured, [ExeExt,
        LineEnding, Msg]), mtError, [mbOk]);
    end;
    UnitSetCache:=nil;
    exit;
  end;

  // create a cache for the current project settings
  {$IFDEF VerboseFPCSrcScan}
  debugln(['TBuildManager.RescanCompilerDefines reading active compiler settings']);
  {$ENDIF}
  WorkDir:='';
  if (FBuildTarget<>nil) and (not FBuildTarget.IsVirtual)
      and HasFPCParamsRelativeFilename(FPCOptions) then
    WorkDir:=FBuildTarget.Directory;

  //debugln(['TBuildManager.RescanCompilerDefines ',CompilerFilename,' OS=',TargetOS,' CPU=',TargetCPU,' Subtarget=',Subtarget,' Options="',FPCOptions,'" WorkDir="',WorkDir,'"']);
  UnitSetCache:=CodeToolBoss.CompilerDefinesCache.FindUnitSet(
    CompilerFilename,TargetOS,TargetCPU,Subtarget,FPCOptions,FPCSrcDir,WorkDir,true);

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
    ' CompilerFilename=',UnitSetCache.CompilerFilename,
    ' TargetOS=',UnitSetCache.TargetOS,
    ' TargetCPU=',UnitSetCache.TargetCPU,
    ' Subtarget=',UnitSetCache.Subtarget,
    ' WorkDir=',UnitSetCache.WorkingDir,
    ' FPCOptions="',UnitSetCache.CompilerOptions,'"',
    ' RealCompiler=',UnitSetCache.GetConfigCache(false).RealCompiler,
    ' EnvFPCSrcDir=',EnvironmentOptions.FPCSourceDirectory,
    ' FPCSrcDir=',UnitSetCache.FPCSourceDirectory,
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
        if Subtarget<>'' then
          IgnorePath+='-'+Subtarget;
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
  DbgCap: String;
  StateFilename: String;
  StateFileAge: LongInt;

  function EditorUnitInfoModified(AnUnitInfo: TUnitInfo): boolean;
  var
    EditComp: TSourceEditorInterface;
  begin
    Result:=false;
    if AnUnitInfo=nil then exit;
    if AnUnitInfo.EditorInfoCount=0 then exit;
    EditComp:=AnUnitInfo.EditorInfo[0].EditorComponent;
    Result:=(EditComp<>nil) and EditComp.Modified;
  end;

  function CheckNonProjectEditorFile(AnUnitInfo: TUnitInfo): boolean;
  begin
    Result:=false;
    if AnUnitInfo.IsPartOfProject or AnUnitInfo.IsVirtual then exit;

    if EditorUnitInfoModified(AnUnitInfo) then
    begin
      if ConsoleVerbosity>=0 then
        DebugLn(DbgCap,'Editor unit modified in source editor ',AProject.IDAsString,' ',AnUnitInfo.Filename);
      Note+='Editor unit "'+AnUnitInfo.Filename+'" has been modified in source editor.'+LineEnding;
      exit(true);
    end;

    if not FileExistsCached(AnUnitInfo.Filename) then exit;
    if StateFileAge>=FileAgeCached(AnUnitInfo.Filename) then exit;
    if FilenameHasPascalExt(AnUnitInfo.Filename) then
    begin
      if (SearchDirectoryInMaskedSearchPath(AProject.CompilerOptions.GetUnitPath(false),
                                ExtractFilePath(AnUnitInfo.Filename))>0)
      then begin
        Result:=true;
        if ConsoleVerbosity>=0 then
          DebugLn(DbgCap,'Editor unit in project''s unit path has changed ',AProject.IDAsString,' ',AnUnitInfo.Filename);
        Note+='Editor unit "'+AnUnitInfo.Filename+'" in project''s unit search path is newer than state file:'+LineEnding
          +'  File age="'+FileAgeToStr(FileAgeCached(AnUnitInfo.Filename))+'"'+LineEnding
          +'  State file age="'+FileAgeToStr(StateFileAge)+'"'+LineEnding
          +'  State file='+StateFilename+LineEnding;
        exit(true);
      end;
    end;
    if (SearchDirectoryInMaskedSearchPath(AProject.CompilerOptions.GetIncludePath(false),
                              ExtractFilePath(AnUnitInfo.Filename))>0)
    then begin
      Result:=true;
      if ConsoleVerbosity>=0 then
        DebugLn(DbgCap,'Editor file in project''s include path has changed ',AProject.IDAsString,' ',AnUnitInfo.Filename);
      Note+='Editor file "'+AnUnitInfo.Filename+'" in project''s include search path is newer than state file:'+LineEnding
        +'  File age="'+FileAgeToStr(FileAgeCached(AnUnitInfo.Filename))+'"'+LineEnding
        +'  State file age="'+FileAgeToStr(StateFileAge)+'"'+LineEnding
        +'  State file='+StateFilename+LineEnding;
      exit(true);
    end;
  end;

var
  CompilerFilename, SrcFilename, LFMFilename, aTargetFilename: string;
  AnUnitInfo: TUnitInfo;
  IcoRes: TProjectIcon;
  CompilerParams: TStrings;
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
    AProject.CompilerOptions.MakeCompilerParams([ccloAbsolutePaths]);
  try
    CompilerParams.Add(SrcFilename);

    //DebugLn(DbgCap,'WorkingDir="',WorkingDir,'" SrcFilename="',SrcFilename,'" CompilerFilename="',CompilerFilename,'" CompilerParams="',MergeCmdLineParams(CompilerParams,TLazCompilerOptions.ConsoleParamsMax),'"');

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
    or FPCParamForBuildAllHasChanged(AProject.LastCompilerParams,CompilerParams)
    or ((AProject.LastCompilerFileDate>0)
        and FileExistsCached(CompilerFilename)
        and (FileAgeCached(CompilerFilename)<>AProject.LastCompilerFileDate))
    then
      NeedBuildAllFlag:=true;

    StateFileAge:=FileAgeCached(StateFilename);

    // check main source file
    AnUnitInfo:=AProject.MainUnitInfo;
    if EditorUnitInfoModified(AnUnitInfo) then
    begin
      if ConsoleVerbosity>=0 then
        DebugLn(DbgCap,'Main src modified in source editor ',AProject.IDAsString,' ',AnUnitInfo.Filename);
      Note+='Main source "'+AnUnitInfo.Filename+'" has been modified in source editor.'+LineEnding;
      exit(mrYes);
    end;
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
    if not CompilerParams.Equals(AProject.LastCompilerParams) then begin
      if ConsoleVerbosity>=0 then begin
        DebugLn(DbgCap,'Compiler params changed for ',AProject.IDAsString);
        DebugLn('  Old="',MergeCmdLineParams(AProject.LastCompilerParams,TLazCompilerOptions.ConsoleParamsMax),'"');
        DebugLn('  Now="',MergeCmdLineParams(CompilerParams,TLazCompilerOptions.ConsoleParamsMax),'"');
      end;
      Note+='Compiler params changed for '+AProject.IDAsString+':'+LineEnding
        +'  Old="'+MergeCmdLineParams(AProject.LastCompilerParams,TLazCompilerOptions.ConsoleParamsMax)+'"'+LineEnding
        +'  Now="'+MergeCmdLineParams(CompilerParams,TLazCompilerOptions.ConsoleParamsMax)+'"'+LineEnding
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

    // check all direct required packages (indirect required were already compiled above)
    Result:=PackageGraph.CheckCompileNeedDueToDependencies(AProject,
                                  AProject.FirstRequiredDependency,
                                  not (pfUseDesignTimePackages in AProject.Flags),
                                  StateFileAge,Note);
    if Result<>mrNo then exit;

    // check project files
    for TLazProjectFile(AnUnitInfo) in AProject.UnitsBelongingToProject do begin
      if EditorUnitInfoModified(AnUnitInfo) then
      begin
        if ConsoleVerbosity>=0 then
          DebugLn(DbgCap,'Project file modified in source editor ',AProject.IDAsString,' ',AnUnitInfo.Filename);
        Note+='Project file "'+AnUnitInfo.Filename+'" has been modified in source editor.'+LineEnding;
        exit(mrYes);
      end;
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
    end;

    // check all open editor files in unit/include path (maybe the user forgot
    // to add them to the project)
    for TLazProjectFile(AnUnitInfo) in AProject.UnitsWithEditorIndex do begin
      if CheckNonProjectEditorFile(AnUnitInfo) then
        exit(mrYes);
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

  finally
    CompilerParams.Free;
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

function TBuildManager.GetBuildTarget: TProject;
begin
  Result := TProject(FBuildTarget);
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
    if not FileExistsCached(AmbiguousFilename) then exit;
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
  LowExt: string;
  i: integer;
begin
  Result:=mrOk;
  if EnvironmentOptions.AmbiguousFileAction=afaIgnore then exit;
  if (EnvironmentOptions.AmbiguousFileAction=afaWarnOnCompile)
  and not Compiling then exit;

  if FilenameHasPascalExt(AFilename) then begin
    LowExt:=lowercase(ExtractFileExt(AFilename));
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
      try
        ShortFilename:=ExtractFileName(Filename);
        IsPascalUnit:=FilenameHasPascalExt(ShortFilename);
        AUnitName:=ExtractFilenameOnly(ShortFilename);
        repeat
          if (FileInfo.Name='.') or (FileInfo.Name='..')
          or (FileInfo.Name='')
          or ((FileInfo.Attr and faDirectory)<>0) then continue;
          if CompareFilenames(ShortFilename,FileInfo.Name)=0 then continue;

          if (SysUtils.CompareText(ShortFilename,FileInfo.Name)=0)
          then begin
            // same name different case => ambiguous
          end else if IsPascalUnit and FilenameHasPascalExt(FileInfo.Name)
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
              mtConfirmation,[mbYes,mbNo])<>mrYes
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
      finally
        FindCloseUTF8(FileInfo);
      end;
    end;
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
  SourceUnitTree, CompiledUnitTree: TAVLTree;
  ANode: TAVLTreeNode;
  CurUnitName: String;
  CurFilename: String;
  AnUnitFile: PUnitFile;
  CurUnitTree: TAVLTree;
  UnitPath: String;
  IgnoreAll: Boolean;
  Files: TFilenameToStringTree;
  Item: PStringToStringItem;
begin
  Result:=mrOk;
  UnitPath:=TrimSearchPath(TheUnitPath,BaseDir,true);

  SourceUnitTree:=TAVLTree.Create(TListSortCompare(@CompareUnitFiles));
  CompiledUnitTree:=TAVLTree.Create(TListSortCompare(@CompareUnitFiles));
  Files:=TFilenameToStringTree.Create(true);
  try
    // collect all units (.pas, .pp, compiled units)
    CollectFilesInSearchPath(UnitPath,Files,'Unit');
    IgnoreAll:=false;
    for Item in Files do
    begin
      CurFilename:=Item^.Name;
      if FilenameHasPascalExt(CurFilename) then
        CurUnitTree:=SourceUnitTree
      else if FilenameExtIs(CurFilename,CompiledExt,true) then
        CurUnitTree:=CompiledUnitTree
      else
        continue;
      CurUnitName:=ExtractFilenameOnly(CurFilename);
      if not IsValidIdent(CurUnitName) then
        continue;
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
    end;
  finally
    // clean up
    Files.Free;
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
  if LazProject1.MainFile = nil then
    Exit;
  TargetExeName := LazProject1.LazCompilerOptions.CreateTargetFilename;

  if not (CreateApplicationBundle(TargetExeName, LazProject1.GetTitle, True, Project1) in
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
  IsPartOfProject:=(LazProject1<>nil)
               and (LazProject1.FindFile(Filename,[pfsfOnlyProjectFiles])<>nil);
  // check if file in source directory of project
  if (not IsPartOfProject) and (Project1<>nil)
    and (SearchDirectoryInMaskedSearchPath(Project1.SourceDirectories.CreateSearchPathFromAllFiles,
      ExtractFilePath(Filename))>0)
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

function TBuildManager.GetResourceType(AnUnitInfo: TUnitInfo): TProjResourceType;
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
    Result:=SearchFileInSearchPath(Result,'',
        CodeToolBoss.GetIncludePathForDirectory(ExtractFilePath(AnUnitInfo.Filename)),
        [TSPSearchFileFlag.SearchLoUpCase]);
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
begin
  Result:=mrOk;
  // update project resource
  if Project1.MainUnitID>=0 then
    Project1.ProjResources.Regenerate(Project1.MainFileName, False, True, TestDir);
  for TLazProjectFile(AnUnitInfo) in Project1.UnitsBelongingToProject do begin
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

procedure TBuildManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    if FUnitSetCache=AComponent then
      FUnitSetCache:=nil;
  end;
end;

function TBuildManager.GetBuildMacroValuesHandler(Options: TLazCompilerOptions;
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
        Values:=GetBuildMacroValuesHandler(OtherOpts,true);
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
        s:=FBuildTarget.LazCompilerOptions.TargetOS;
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
        s:=FBuildTarget.LazCompilerOptions.TargetCPU;
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
    // Subtarget
    if not Values.IsDefined('Subtarget') then begin
      s:='';
      if FBuildTarget<>nil then
        s:=FBuildTarget.LazCompilerOptions.Subtarget;
      if s='' then
        s:=fSubtarget;
      Values.Values['Subtarget']:=s;
    end;
    // Laz_FullVersion
    if not Values.IsDefined('Laz_FullVersion') then begin
      SetCTCSVariableAsNumber(Values.GetVariable('Laz_FullVersion',true),laz_fullversion);
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
      ApplyBuildMatrixMacros(BuildTarget.BuildModes.SharedMatrixOptions,Target,ActiveMode,Vars);
      ApplyBuildMatrixMacros(BuildTarget.BuildModes.SessionMatrixOptions,Target,ActiveMode,Vars);
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

  ParseOpts:=TBaseCompilerOptions(Options).ParsedOpts;
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
      Values:=GetBuildMacroValuesHandler(Options,false);

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
      BuildTarget.BuildModes.SharedMatrixOptions.AppendCustomOptions(Target,ActiveMode,Options);
    if bmgtSession in Types then
      BuildTarget.BuildModes.SessionMatrixOptions.AppendCustomOptions(Target,ActiveMode,Options);
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
      BuildTarget.BuildModes.SharedMatrixOptions.GetOutputDirectory(Target,ActiveMode,OutDir);
    if bmgtSession in Types then
      BuildTarget.BuildModes.SessionMatrixOptions.GetOutputDirectory(Target,ActiveMode,OutDir);
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

procedure TBuildManager.SetBuildTarget(const TargetOS, TargetCPU, Subtarget,
  LCLWidgetType: string; ScanFPCSrc: TScanModeFPCSources; Quiet: boolean);

  function GetEffectiveLCLWidgetType: string;
  begin
    if OverrideLCLWidgetType<>'' then
      Result:=OverrideLCLWidgetType
    else if FBuildTarget<>nil then
      Result:=FBuildTarget.LazCompilerOptions.GetEffectiveLCLWidgetType
    else
      Result:='';
    if (Result='') or (SysUtils.CompareText(Result,'default')=0) then
      Result:=GetLCLWidgetTypeName;
    Result:=lowercase(Result);
  end;

var
  OldTargetOS: String;
  OldTargetCPU: String;
  OldSubtarget: String;
  OldLCLWidgetType: String;
  PCTargetChanged: Boolean;
  LCLTargetChanged: Boolean;
  CompilerTargetOS: string;
  CompilerTargetCPU: string;
  CompQueryOptions, CompilerFilename: String;
begin
  {$IFDEF VerboseDefaultCompilerTarget}
  debugln(['TBuildManager.SetBuildTarget TargetOS="',TargetOS,'" TargetCPU="',TargetCPU,'" Subtarget="',Subtarget,'" LCLWidgetType="',LCLWidgetType,'"']);
  {$ENDIF}
  OldTargetOS:=fTargetOS;
  OldTargetCPU:=fTargetCPU;
  OldSubtarget:=fSubtarget;
  OldLCLWidgetType:=fLCLWidgetType;
  OverrideTargetOS:=GetFPCTargetOS(TargetOS);
  OverrideTargetCPU:=GetFPCTargetCPU(TargetCPU);
  OverrideSubtarget:=GetFPCSubtarget(Subtarget);
  OverrideLCLWidgetType:=lowercase(LCLWidgetType);

  // compute new TargetOS
  if OverrideTargetOS<>'' then
    fTargetOS:=OverrideTargetOS
  else if FBuildTarget<>nil then
    fTargetOS:=FBuildTarget.LazCompilerOptions.TargetOS
  else
    fTargetOS:='';
  if SysUtils.CompareText(fTargetOS,'default')=0 then
    fTargetOS:='';

  // compute new TargetCPU
  if OverrideTargetCPU<>'' then
    fTargetCPU:=OverrideTargetCPU
  else if FBuildTarget<>nil then
    fTargetCPU:=FBuildTarget.LazCompilerOptions.TargetCPU
  else
    fTargetCPU:='';
  if SysUtils.CompareText(fTargetCPU,'default')=0 then
    fTargetCPU:='';

  // compute new Subtarget
  if OverrideSubtarget<>'' then
    fSubtarget:=OverrideSubtarget
  else if FBuildTarget<>nil then
    fSubtarget:=FBuildTarget.LazCompilerOptions.Subtarget
  else
    fSubtarget:='';
  if SysUtils.CompareText(fSubtarget,'default')=0 then
    fSubtarget:='';

  if (fTargetOS='') or (fTargetCPU='') then
  begin
    // use compiler default target
    CompQueryOptions:='';
    if fTargetCPU<>'' then
      CompQueryOptions:='-P'+GetFPCTargetCPU(fTargetCPU)
    else if fTargetOS<>'' then
      CompQueryOptions:='-T'+GetFPCTargetOS(fTargetOS);
    // Note: resolving the compiler filename requires macros
    CompilerFilename:=GetCompilerFilename;
    if CompilerFilename=cInvalidCompiler then
      exit;

    CodeToolBoss.CompilerDefinesCache.ConfigCaches.GetDefaultCompilerTarget(
      CompilerFilename,CompQueryOptions,CompilerTargetOS,CompilerTargetCPU);
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

  PCTargetChanged:=(OldTargetOS<>fTargetOS)
                    or (OldTargetCPU<>fTargetCPU)
                    or (OldSubtarget<>fSubtarget)
                    or (CodeToolBoss.DefineTree.FindDefineTemplateByName(
                         StdDefTemplLazarusSources,true)=nil);
  if PCTargetChanged then
  begin
    IncreaseBuildMacroChangeStamp;
    CodeToolBoss.DefineTree.ClearCache;
  end;

  // compute new LCLWidgetType
  fLCLWidgetType:=GetEffectiveLCLWidgetType;
  LCLTargetChanged:=(OldLCLWidgetType<>fLCLWidgetType);

  if PCTargetChanged or LCLTargetChanged then begin
    if ConsoleVerbosity>=0 then
      DebugLn(['Hint: (lazarus) [TBuildManager.SetBuildTarget]',
          ' Old=',OldTargetCPU,'-',OldTargetOS,'-',OldSubtarget,'-',OldLCLWidgetType,
          ' New=',fTargetCPU,'-',fTargetOS,'-',fSubtarget,'-',fLCLWidgetType,
          ' Changed: OS/CPU=',PCTargetChanged,' LCL=',LCLTargetChanged]);
  end;
  if LCLTargetChanged then
    CodeToolBoss.SetGlobalValue(ExternalMacroStart+'LCLWidgetType',fLCLWidgetType);
  if ScanFPCSrc<>smsfsSkip then
    RescanCompilerDefines(false,false,ScanFPCSrc=smsfsWaitTillDone,Quiet);
  //if (PackageGraph<>nil) and (PackageGraph.CodeToolsPackage<>nil) then
  //  debugln(['TBuildManager.SetBuildTarget CODETOOLS OUTDIR=',PackageGraph.CodeToolsPackage.CompilerOptions.GetUnitOutPath(true,coptParsed),
  //           ' ',PackageGraph.CodeToolsPackage.CompilerOptions.ParsedOpts.ParsedStamp[pcosOutputDir],' ',CompilerParseStamp]);
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
    Project1.IDEOptions.AddHandlerDestroy(@ProjectDestroy);
  SetBuildTarget('','','','',ScanFPCSrc,Quiet);
end;

procedure TBuildManager.SetBuildTargetIDE(aQuiet: boolean);
var
  NewTargetOS: String;
  NewTargetCPU: String;
  NewLCLWidgetSet, NewSubtarget: String;
begin
  //debugln(['TBuildManager.SetBuildTargetIDE START']);
  FBuildTarget:=nil;
  with MiscellaneousOptions do begin
    NewTargetOS:=BuildLazOpts.TargetOS;
    NewTargetCPU:=BuildLazOpts.TargetCPU;
    NewSubtarget:=BuildLazOpts.Subtarget;
    NewLCLWidgetSet:=LCLPlatformDirNames[BuildLazOpts.TargetPlatform];
  end;
  if ConsoleVerbosity>=1 then
    debugln(['Hint: (lazarus) [TBuildManager.SetBuildTargetIDE] OS=',NewTargetOS,' CPU=',NewTargetCPU,' Subtarget=',NewSubtarget,' WS=',NewLCLWidgetSet]);
  SetBuildTarget(NewTargetOS,NewTargetCPU,NewSubtarget,NewLCLWidgetSet,smsfsBackground,aQuiet);
end;

function TBuildManager.BuildTargetIDEIsDefault: boolean;
// check if current BuildLazarus creates the normal lazarus exe
// aka not some cross compile
var
  NewTargetOS: String;
  NewTargetCPU: String;
  NewLCLWidgetSet: TLCLPlatform;
begin
  with MiscellaneousOptions do begin
    NewTargetOS:=BuildLazOpts.TargetOS;
    NewTargetCPU:=BuildLazOpts.TargetCPU;
    NewLCLWidgetSet:=BuildLazOpts.TargetPlatform;
  end;
  //debugln(['TBuildManager.BuildTargetIDEIsDefault NewTargetOS=',NewTargetOS,' Default=',GetDefaultTargetOS,' NewTargetCPU=',NewTargetCPU,' default=',GetDefaultTargetCPU,' ws=',LCLPlatformDisplayNames[NewLCLWidgetSet],' default=',LCLPlatformDisplayNames[GetDefaultLCLWidgetType]]);
  Result:=((NewTargetOS='') or (CompareText(NewTargetOS, GetCompiledTargetOS)=0))
      and ((NewTargetCPU='') or (CompareText(NewTargetCPU, GetCompiledTargetCPU)=0))
      and (NewLCLWidgetSet<>lpNoGUI);
      // Note: no need to check if CompilerFilename is the default
end;

initialization
  EnvironmentOpts.GroupEnvironmentI18NCaption := @dlgGroupEnvironment;

end.

