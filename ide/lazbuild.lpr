{***************************************************************************
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
 
  Command line utility to compile lazarus projects and packages.
}
program lazbuild;

{$mode objfpc}{$H+}

uses
  {$IF defined(HASAMIGA)}
  athreads,
  {$ENDIF}
  {$IF defined(UNIX)}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Math, CustApp, System.UITypes,
  Interfaces, // this includes the NoGUI widgetset
  // LazUtils
  Masks, LConvEncoding, FileUtil, LazFileUtils, LazLoggerBase, LazUtilities,
  LazUTF8, Laz2_XMLCfg, LazStringUtils, FPCAdds,
  // LCL
  LCLPlatformDef,
  // Codetools
  CodeCache, CodeToolManager, DefineTemplates, FileProcs,
  // BuildIntf
  BaseIDEIntf, MacroIntf, PackageIntf, LazMsgWorker, ProjectIntf, IDEExternToolIntf,
  CompOptsIntf, IDEOptionsIntf, PackageDependencyIntf,
  // IdeConfig
  LazConf, IDECmdLine, TransferMacros, EnvironmentOpts, ParsedCompilerOpts, CompilerOptions,
  // IdePackager
  IdePackagerStrConsts,
  // IDE
  InitialSetupProc, ExtToolsConsole, ApplicationBundle,
  IDETranslations, LazarusIDEStrConsts, MiscOptions, Project, PackageDefs,
  PackageLinks, PackageSystem, InterPkgConflictFiles, BuildLazDialog,
  BuildProfileManager, BuildManager, BaseBuildManager, ModeMatrixOpts,
  ColorTTY;

type
  TPkgAction = (
    lpaBuild, // build package, default
    lpaInstall, // install package
    lpaAddPkgLinks // register, no build
    );

  { TLazBuildApplication }

  TLazBuildApplication = class(TCustomApplication)
  private
    FBuildAll: boolean;
    FBuildIDE: boolean;
    FBuildIDEOptions: string;
    FBuildModeOverride: String;
    FBuildRecursive: boolean;
    FBuildTwice: boolean;
    fCompilerInCfg: string;
    fCompilerOverride: String;
    fCPUOverride: String;
    FCreateMakefile: boolean;
    fInitialized: boolean;
    fInitResult: boolean;
    fLazarusDirInCfg: string;
    fLazarusDirOverride : String;
    FMaxProcessCount: integer;
    FNoWriteProject: Boolean;
    fOSOverride: String;
    FPackageAction: TPkgAction;
    FPkgGraphVerbosity: TPkgVerbosityFlags;
    FSkipDependencies: boolean;
    FSubtargetOverride: boolean;
    FSubtargetOverrideValue: String;
    fWidgetsetOverride: String;

    function HasLongOptIgnoreCase(const S: String; out aValue: String): Boolean;
    function HasShortOrLongOpt(const C: Char; const S: String): Boolean;

    // codetools
    procedure CodeBufferDecodeLoaded({%H-}Code: TCodeBuffer;
         const {%H-}Filename: string; var Source, DiskEncoding, MemEncoding: string);
    procedure CodeBufferEncodeSaving(Code: TCodeBuffer;
                                    const {%H-}Filename: string; var Source: string);

    // global package functions
    procedure GetDependencyOwnerDescription(Dependency: TPkgDependency;
                                            out Description: string);
    procedure GetDependencyOwnerDirectory(Dependency: TPkgDependency;
                                          out Directory: string);
    // Event procedure that adds every package added to the package graph to the (user) package links
    procedure PackageGraphAddPackage(Pkg: TLazPackage);
    function PackageGraphCheckInterPkgFiles(IDEObject: TObject;
                          PkgList: TFPList; out FilesChanged: boolean): boolean;

    // project
    procedure ProjectInfoFileChanged(TheProject: TProject);

    // dialogs
    function IDEMessageDialogHandler(const aCaption, aMsg: string;
                         {%H-}DlgType: TMsgDlgType; {%H-}Buttons: TMsgDlgButtons;
                         const {%H-}HelpKeyword: string): Integer;
    function IDEQuestionDialogHandler(const aCaption, aMsg: string;
                         {%H-}DlgType: TMsgDlgType; {%H-}Buttons: array of const;
                         const {%H-}HelpKeyword: string): Integer;
  protected
    function GetParams(Index: Integer): String; override;
    function GetParamCount: Integer; override;
    function HasCustomCompilerOpts(out aValue: string): boolean;

    // Builds project or package, depending on extension.
    // Packages can also be specified by package name if they are known to the IDE.
    function BuildFile(Filename: string): boolean;

    // packages
    // Build a package identified by filename and return build result
    function BuildPackage(const AFilename: string): boolean;
    // Load package file into loaded packages (package graph), overwriting any package with the same name
    function LoadPackage(const AFilename: string): TLazPackage;
    procedure CompilePackage(APackage: TLazPackage; Flags: TPkgCompileFlags);
    procedure DoCreateMakefile(APackage: TLazPackage);
    procedure CheckPackageGraphForCompilation(APackage: TLazPackage;
                                 FirstDependency: TPkgDependency);

    // projects
    function BuildProject(const AFilename: string): boolean;
    function LoadProject(const AFilename: string): TProject;
    procedure CloseProject(var AProject: TProject);

    // Adding packages to list of to-be-installed packages in the IDE.
    // The packages can then be installed by recompiling the IDE (because we're using static packages)
    function AddPackagesToInstallList(const PackageNamesOrFiles: TStringList): boolean;
    function AddCmdLinePackageLinks(const PackageNamesOrFiles: TStringList): boolean;

    // IDE
    function BuildLazarusIDE: boolean;
    function CompileAutoInstallPackages(Clean: boolean): boolean;

    function Init: boolean;
    procedure LoadEnvironmentOptions;
    procedure LoadMiscellaneousOptions;
    procedure SetupMacros;
    procedure SetupCodetools;
    procedure SetupPackageSystem;
    procedure SetupDialogs;
    procedure StoreBaseSettings;
    function RepairedCheckOptions(Const ShortOptions : String;
                   Const Longopts : TStrings; Opts,NonOpts : TStrings) : String;
  public
    // Files (or package names) passed by the user to Lazbuild:
    Files: TStringList;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Run;
    function ParseParameters: boolean;
    procedure WriteUsage;
    procedure PrintInfo(const Msg: string);
    procedure PrintHint(const Msg: string);
    procedure PrintWarning(const Msg: string);
    procedure PrintErrorAndHalt(Code: Byte; const Msg: string);

    property PackageAction: TPkgAction read FPackageAction write FPackageAction;
    property BuildAll: boolean read FBuildAll write FBuildAll;// build all files of project/package
    property BuildTwice: boolean read FBuildTwice write FBuildTwice;// build all packages twice
    property BuildRecursive: boolean read FBuildRecursive // apply BuildAll flag to dependencies
                                     write FBuildRecursive;
    property SkipDependencies: boolean read FSkipDependencies
                                            write FSkipDependencies;
    property BuildIDE: boolean read FBuildIDE write FBuildIDE; // build IDE (as opposed to a project/package etc)
    property BuildIDEOptions: string read FBuildIDEOptions write FBuildIDEOptions;
    property CreateMakefile: boolean read FCreateMakefile write FCreateMakefile;
    property WidgetSetOverride: String read FWidgetsetOverride write FWidgetsetOverride;
    property OSOverride: String read fOSOverride write fOSOverride;
    property CPUOverride: String read fCPUOverride write fCPUOverride;
    property SubtargetOverride: boolean read FSubtargetOverride write FSubtargetOverride;
    property SubtargetOverrideValue: String read FSubtargetOverrideValue write FSubtargetOverrideValue;
    property CompilerOverride: String read fCompilerOverride write fCompilerOverride;
    property LazarusDirOverride: String read fLazarusDirOverride write fLazarusDirOverride;
    property BuildModeOverride: String read FBuildModeOverride write FBuildModeOverride;
    property MaxProcessCount: integer read FMaxProcessCount write FMaxProcessCount;
    property NoWriteProject: boolean read FNoWriteProject write FNoWriteProject;
    property PkgGraphVerbosity: TPkgVerbosityFlags read FPkgGraphVerbosity write FPkgGraphVerbosity;
  end;

var
  LazBuildApp: TLazBuildApplication = nil;

const
  ErrorFileNotFound       = 1;
  ErrorBuildFailed        = 2;
  ErrorLoadPackageFailed  = 3;
  ErrorPackageNameInvalid = 4;
  ErrorLoadProjectFailed  = 5;
  ErrorInvalidSyntax      = 6;
  ErrorInitialization     = 7;
  ErrorExpandMacro        = 8;
  VersionStr = {$I packages/ideconfig/version.inc};

procedure FilterConfigFileContent;
var
  l: TStrings;
  i: Integer;
  LowerOpt: String;
begin
  ResetParamsAndCfg;
  l := GetCfgFileContent;
  if l = nil then exit;
  i := l.Count - 1;
  while i >= 0 do begin
    LowerOpt := LowerCase(l[i]);
    if not(
        LazStartsStr('--primary-config-path=', LowerOpt) or
        LazStartsStr('--secondary-config-path=', LowerOpt) or
        LazStartsStr('--pcp=', LowerOpt) or
        LazStartsStr('--scp=', LowerOpt)
       )
    then
      l.Delete(i);
    dec(i);
  end;
end;

Function ToolParamCount: Integer;
begin
  Result := GetParamsAndCfgFile.Count - 1;
end;

Function ToolParamStr(Param : Integer) : Ansistring;
begin
  if Param >= GetParamsAndCfgFile.Count then
    Result := ''
  else
    Result := GetParamsAndCfgFile[Param];
end;

procedure GetDescriptionOfDependencyOwner(Dependency: TPkgDependency;
  out Description: string);
var
  DepOwner: TObject;
begin
  DepOwner:=Dependency.Owner;
  if (DepOwner<>nil) then begin
    if DepOwner is TLazPackage then begin
      Description:=Format(lisPkgMangPackage, [TLazPackage(DepOwner).IDAsString]);
    end else if DepOwner is TProject then begin
      Description:=Format(lisPkgMangProject,
                          [ExtractFileNameOnly(TProject(DepOwner).ProjectInfoFile)]);
    end else begin
      Description:=dbgsName(DepOwner)
    end;
  end else begin
    Description:=Format(lisPkgMangDependencyWithoutOwner, [Dependency.AsString]);
  end;
end;

procedure GetDirectoryOfDependencyOwner(Dependency: TPkgDependency;
  out Directory: string);
var
  DepOwner: TObject;
begin
  DepOwner:=Dependency.Owner;
  if (DepOwner<>nil) then begin
    if DepOwner is TLazPackage then begin
      Directory:=TLazPackage(DepOwner).Directory;
    end else if DepOwner is TProject then begin
      Directory:=TProject(DepOwner).Directory;
    end else begin
      Directory:=''
    end;
  end else begin
    Directory:=''
  end;
end;

procedure ShowBuildModes;
var
  i: Integer;
begin
  // first print the active build mode
  for i := 0 to Project1.BuildModes.Count - 1 do
    if Project1.BuildModes[i] = Project1.ActiveBuildMode then
      WriteLn(Project1.BuildModes[i].Identifier);
  // print other build modes
  for i := 0 to Project1.BuildModes.Count - 1 do
    if Project1.BuildModes[i] <> Project1.ActiveBuildMode then
      WriteLn(Project1.BuildModes[i].Identifier);
end;

procedure ShowBuildModeError(const aBuildModeOverride: string);
begin
  WriteLn(Format(lisERRORInvalidBuildMode, [aBuildModeOverride]));
  if Project1.BuildModes.Count>1 then
  begin
    WriteLn(lisAvailableProjectBuildModes);
    ShowBuildModes;
  end else
    WriteLn(lisThisProjectHasOnlyTheDefaultBuildMode);
  WriteLn;
  Halt(ErrorBuildFailed);
end;

{ TLazBuildApplication }

procedure TLazBuildApplication.CodeBufferEncodeSaving(Code: TCodeBuffer;
  const Filename: string; var Source: string);
begin
  if (Code.DiskEncoding<>'') and (Code.MemEncoding<>'')
  and (Code.DiskEncoding<>Code.MemEncoding) then begin
    {$IFDEF VerboseIDEEncoding}
    DebugLn(['TLazBuildApplication.OnCodeBufferEncodeSaving Filename=',Code.Filename,' Mem=',Code.MemEncoding,' to Disk=',Code.DiskEncoding]);
    {$ENDIF}
    Source:=ConvertEncoding(Source,Code.MemEncoding,Code.DiskEncoding);
  end;
end;

procedure TLazBuildApplication.CodeBufferDecodeLoaded(Code: TCodeBuffer;
  const Filename: string; var Source, DiskEncoding, MemEncoding: string);
begin
  //DebugLn(['TLazBuildApplication.OnCodeBufferDecodeLoaded Filename=',Filename,' Encoding=',GuessEncoding(Source)]);
  DiskEncoding:='';
  if DiskEncoding='' then
    DiskEncoding:=GuessEncoding(Source);
  MemEncoding:=EncodingUTF8;
  if (DiskEncoding<>MemEncoding) then begin
    {$IFDEF VerboseIDEEncoding}
    DebugLn(['TLazBuildApplication.OnCodeBufferDecodeLoaded Filename=',Filename,' Disk=',DiskEncoding,' to Mem=',MemEncoding]);
    {$ENDIF}
    Source:=ConvertEncoding(Source,DiskEncoding,MemEncoding);
    //DebugLn(['TLazBuildApplication.OnCodeBufferDecodeLoaded ',Source]);
  end;
end;

procedure TLazBuildApplication.GetDependencyOwnerDescription(
  Dependency: TPkgDependency; out Description: string);
begin
  GetDescriptionOfDependencyOwner(Dependency,Description);
end;

procedure TLazBuildApplication.GetDependencyOwnerDirectory(
  Dependency: TPkgDependency; out Directory: string);
begin
  GetDirectoryOfDependencyOwner(Dependency,Directory);
end;

procedure TLazBuildApplication.PackageGraphAddPackage(Pkg: TLazPackage);
begin
  if FileExistsUTF8(Pkg.FileName) then LazPackageLinks.AddUserLink(Pkg);
end;

function TLazBuildApplication.PackageGraphCheckInterPkgFiles(
  IDEObject: TObject; PkgList: TFPList; out FilesChanged: boolean): boolean;
begin
  Result:=CheckInterPkgFiles(IDEObject,PkgList,FilesChanged);
end;

procedure TLazBuildApplication.ProjectInfoFileChanged(TheProject: TProject);
begin
  if TheProject<>Project1 then exit;
  if TheProject.IsVirtual then
    CodeToolBoss.SetGlobalValue(ExternalMacroStart+'ProjPath',VirtualDirectory)
  else
    CodeToolBoss.SetGlobalValue(ExternalMacroStart+'ProjPath',Project1.Directory)
end;

function TLazBuildApplication.IDEMessageDialogHandler(const aCaption, aMsg: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; const HelpKeyword: string): Integer;
begin
  DumpStack;
  PrintErrorAndHalt(ErrorBuildFailed, Format(lisLazbuildIsNonInteractiveAbortingNow, [
    aCaption, LineEnding, aMsg, LineEnding]));
  Result:=mrCancel;
end;

function TLazBuildApplication.IDEQuestionDialogHandler(const aCaption, aMsg: string;
  DlgType: TMsgDlgType; Buttons: array of const; const HelpKeyword: string): Integer;
begin
  DumpStack;
  PrintErrorAndHalt(ErrorBuildFailed, Format(lisLazbuildIsNonInteractiveAbortingNow, [
    aCaption, LineEnding, aMsg, LineEnding]));
  Result:=mrCancel;
end;

function TLazBuildApplication.GetParams(Index: Integer): String;
begin
  Result := ToolParamStr(Index);
end;

function TLazBuildApplication.GetParamCount: Integer;
begin
  Result := ToolParamCount;
end;

function TLazBuildApplication.HasCustomCompilerOpts(out aValue: string): boolean;
var
  p: string; // current parameter
  v: string; // value of current parameter
  i: Integer;
begin
  aValue := '';
  for i := 1 to GetParamCount do
  begin
    p := GetParams(i);
    if LazStartsText('--opt=', p) then
    begin
      // get value
      v := copy(p, length('--opt=') + 1, length(p));
      // remove quotes
      if length(v) >= 2 then
        if ((v[1] = '"' ) and (v[length(v)] = '"' )) or
           ((v[1] = '''') and (v[length(v)] = ''''))
        then
          v := copy(v, 2, length(v) - 2);
      // append
      aValue := MergeCustomOptions(aValue, v);
    end;
  end;
  result := aValue <> '';
end;

function TLazBuildApplication.BuildFile(Filename: string): boolean;
var
  OriginalFilename: string;
  Package: TLazPackageLink;
begin
  Result:=false;
  OriginalFilename:=FileName;

  Filename:=CleanAndExpandFilename(Filename);
  if not FileExistsUTF8(Filename) then
  begin
    // File doesn't exist.

    // Check for packages if the specified name is a valid identifier
    //debugln(['TLazBuildApplication.BuildFile ',OriginalFilename]);
    if IsValidPkgName(OriginalFileName) then begin
      if PackageAction=lpaAddPkgLinks then
        PrintErrorAndHalt(ErrorFileNotFound, 'lpk file expected, but "' + OriginalFilename + '" found');
      // Initialize package graph with base packages etc:
      if not Init then exit;
      // Could be a known but not installed package
      // so try and get package filename from all other known packages
      Package:=TLazPackageLink(LazPackageLinks.FindLinkWithPkgName(OriginalFileName));
      if Package=nil then begin
        // Not found after everything we tried
        if FilenameExtIs(Filename,'lpi', false) then
          PrintErrorAndHalt(ErrorFileNotFound, 'File not found: "' + OriginalFilename + '"')
        else
          PrintErrorAndHalt(ErrorFileNotFound, 'Package not found: "' + OriginalFilename + '"');
      end
      else begin
        // We found a package link
        case PackageAction of
        lpaBuild:
          exit(BuildPackage(Package.LPKFilename));
        lpaInstall:
          exit(true); // this is handled in AddPackagesToInstallList
        end;
      end;
    end;

    PrintErrorAndHalt(ErrorFileNotFound, 'File not found: "' + OriginalFilename + '"');
  end
  else if DirPathExists(Filename) then
    PrintErrorAndHalt(ErrorFileNotFound, '"' + Filename + '" is a directory')
  else begin
    // File exists:
    if FilenameExtIs(Filename,'lpk',true) then begin
      case PackageAction of
      lpaBuild: Result:=BuildPackage(Filename);
      lpaInstall: Result:=true; // this is handled in AddPackagesToInstallList
      lpaAddPkgLinks: Result:=true;
      end;
    end else if FilenameExtIs(Filename,'lpi',false) then
      Result:=BuildProject(Filename)
    else if FilenameExtIs(Filename,'lpr',false) then begin
      Filename:=ChangeFileExt(Filename,'.lpi');
      if FileExists(Filename) then
        Result:=BuildProject(Filename)
      else
        PrintErrorAndHalt(ErrorFileNotFound, 'File not found: "' + Filename + '"');
    end else
      PrintErrorAndHalt(ErrorBuildFailed, 'Don''t know how to build: "' + Filename + '"');
  end;
end;

function TLazBuildApplication.BuildPackage(const AFilename: string): boolean;
var
  APackage: TLazPackage;
  Flags: TPkgCompileFlags;
  S: String;
begin
  Result:=false;
  
  if not Init then exit;

  PrintHint('Compile package "' + AFilename + '"');

  APackage:=LoadPackage(AFilename);
  if APackage=nil then
    PrintErrorAndHalt(ErrorLoadPackageFailed, 'Unable to load package "' + AFilename + '"');
    
  Flags:=[];
  if BuildAll then
    Include(Flags,pcfCleanCompile)
  else
    Include(Flags,pcfOnlyIfNeeded);
  if BuildTwice then
    Include(Flags,pcfCompileTwice);
  if BuildRecursive and BuildAll then
    Include(Flags,pcfCompileDependenciesClean);
  if SkipDependencies then
    Include(Flags,pcfDoNotCompileDependencies);

  if (Length(OSOverride) <> 0) then
    APackage.CompilerOptions.TargetOS:=OSOverride;
  if (Length(CPUOverride) <> 0) then
    APackage.CompilerOptions.TargetCPU:=CPUOverride;
  if SubtargetOverride then
    APackage.CompilerOptions.Subtarget:=SubtargetOverrideValue;
  if HasCustomCompilerOpts(S) then
    with APackage.CompilerOptions do
      CustomOptions := MergeCustomOptions(CustomOptions, S);

  if CreateMakefile then
    DoCreateMakefile(APackage)
  else
    CompilePackage(APackage,Flags);

  LazPackageLinks.SaveUserLinks(true);

  Result:=true;
end;

function TLazBuildApplication.LoadPackage(const AFilename: string): TLazPackage;
var
  XMLConfig: TXMLConfig;
  ConflictPkg: TLazPackage;
begin
  // check if package is already loaded
  Result:=PackageGraph.FindPackageWithFilename(AFilename);
  if (Result<>nil) then exit;
  if not FileExists(AFilename) then
    PrintErrorAndHalt(ErrorLoadPackageFailed, 'Package file not found: "' + AFilename + '"');

  Result:=TLazPackage.Create;
  // load the package file
  XMLConfig:=TXMLConfig.Create(AFilename);
  try
    Result.Filename:=AFilename;
    Result.LoadFromXMLConfig(XMLConfig,'Package/');
  finally
    XMLConfig.Free;
  end;
  // check Package Name
  if not IsValidPkgName(Result.Name) then
    PrintErrorAndHalt(ErrorPackageNameInvalid,
      Format(lisPkgMangThePackageNameOfTheFileIsInvalid, [Result.Name, LineEnding, Result.Filename]));

  // check if Package with same name is already loaded
  ConflictPkg:=PackageGraph.FindPackageWithName(Result.Name,nil);
  if ConflictPkg<>nil then begin
    // replace package
    if not PackageGraph.PackageCanBeReplaced(ConflictPkg,Result) then
      PrintErrorAndHalt(ErrorLoadPackageFailed,
        'Cannot replace loaded package '+ConflictPkg.IDAsString+' with '+Result.IDAsString+' from "'+Result.Filename+'"');
    PackageGraph.ReplacePackage(ConflictPkg,Result);
  end else begin
    // add to graph
    PackageGraph.AddPackage(Result);
  end;
  // save package file links
  LazPackageLinks.SaveUserLinks;
end;

function TLazBuildApplication.BuildLazarusIDE: boolean;
var
  Flags: TBuildLazarusFlags;
  CurResult: TModalResult;
  BuildLazProfiles: TBuildLazarusProfiles;
  CurProf: TBuildLazarusProfile;
  InheritedOptionStrings: TInheritedCompOptsStrings;
  TargetDir: String;
  i: Integer;
  s: String;
  Builder: TLazarusBuilder;
begin
  Result:=false;
  if not Init then exit;

  LoadMiscellaneousOptions;
  BuildLazProfiles:=MiscellaneousOptions.BuildLazProfiles;
  CurProf:=BuildLazProfiles.Current;
  if BuildModeOverride<>'' then
  begin
    i:=BuildLazProfiles.IndexByName(BuildModeOverride);
    if i<0 then
    begin
      debugln(['Error: (lazbuild) IDE build mode "' + BuildModeOverride + '" not found']);
      if ConsoleVerbosity>=0 then begin
        debugln;
        debugln('Available IDE build modes:');
        for i:=0 to BuildLazProfiles.Count-1 do
        begin
          if BuildLazProfiles[i]=CurProf then
            dbgout('* ')
          else
            dbgout('  ');
          debugln(BuildLazProfiles[i].Name);
        end;
      end;
      Halt(ErrorBuildFailed);
    end;
    CurProf:=BuildLazProfiles[i];
    BuildLazProfiles.CurrentIndex:=i;
  end;
  PrintHint('Building Lazarus IDE with profile "' + CurProf.Name + '"');

  if (Length(OSOverride) <> 0) then
    CurProf.TargetOS:=OSOverride;
  if (Length(CPUOverride) <> 0) then
    CurProf.TargetCPU:=CPUOverride;
  if SubtargetOverride then
    CurProf.Subtarget:=SubtargetOverrideValue;

  if WidgetSetOverride<>'' then
    CurProf.TargetPlatform:=DirNameToLCLPlatform(WidgetSetOverride)
  else
    CurProf.TargetPlatform:=GetBuildLCLWidgetType;

  // add custom options from --build-ide after build mode options
  if BuildIDEOptions <> '' then
    CurProf.ExtraOptions := MergeCustomOptions(CurProf.ExtraOptions, BuildIDEOptions);
  // add parameters from the --opt option after --build-ide, as higher priority
  if HasCustomCompilerOpts(s) then
    CurProf.ExtraOptions := MergeCustomOptions(CurProf.ExtraOptions, s);

  if BuildAll then
    CurProf.IdeBuildMode:=bmCleanAllBuild;
  MainBuildBoss.SetBuildTargetIDE;
  Flags:=[];

  // try loading install packages
  PackageGraph.LoadAutoInstallPackages(BuildLazProfiles.StaticAutoInstallPackages);

  // create target directory
  TargetDir:=CurProf.TargetDirectory;
  IDEMacros.SubstituteMacros(TargetDir);
  if not ForceDirectory(TargetDir) then
    PrintErrorAndHalt(ErrorBuildFailed, 'Failed creating IDE target directory "' + TargetDir + '"');

  // clean
  Builder:=TLazarusBuilder.Create;
  try
    Builder.ProfileChanged:=false;

    if BuildLazProfiles.Current.IdeBuildMode=bmCleanAllBuild then begin
      Builder.PackageOptions:='';
      CurResult:=Builder.MakeLazarus(BuildLazProfiles.Current,
                  Flags+[blfDontBuild]);
      if CurResult<>mrOk then
        PrintErrorAndHalt(ErrorBuildFailed, 'Building IDE: Clean all failed');
    end;

    // save list of install packages
    CurResult:=PackageGraph.SaveAutoInstallConfig;
    if CurResult<>mrOk then
      PrintErrorAndHalt(ErrorBuildFailed, 'Building IDE: Failed saving IDE make config files');

    // compile auto install static packages
    if not CompileAutoInstallPackages(BuildLazProfiles.Current.IdeBuildMode<>bmBuild) then
      PrintErrorAndHalt(ErrorBuildFailed, 'Building IDE: Compile AutoInstall Packages failed');

    // create inherited compiler options
    Builder.PackageOptions:=PackageGraph.GetIDEInstallPackageOptions(InheritedOptionStrings{%H-});

    // save idemake.cfg
    CurResult:=Builder.SaveIDEMakeOptions(BuildLazProfiles.Current,Flags+[blfBackupOldExe]);
    if CurResult<>mrOk then
      PrintErrorAndHalt(ErrorBuildFailed, 'Building IDE: Failed saving "idemake.cfg"');

    // compile IDE
    CurResult:=Builder.MakeLazarus(BuildLazProfiles.Current,
                           Flags+[blfUseMakeIDECfg,blfOnlyIDE]);
    if CurResult<>mrOk then
      PrintErrorAndHalt(ErrorBuildFailed, 'Building IDE: Building IDE failed');

    Result:=true;
  finally
    Builder.Free;
  end;
end;

function TLazBuildApplication.CompileAutoInstallPackages(Clean: boolean): boolean;
var
  Dependency: TPkgDependency;
  OldDependency: TPkgDependency;
  CurResult: TModalResult;
  CompilePolicy: TPackageUpdatePolicy;
begin
  Result:=false;
  PackageGraph.BeginUpdate(false);
  try
    Dependency:=PackageGraph.FirstInstallDependency;
    while Dependency<>nil do begin
      OldDependency:=Dependency;
      Dependency:=Dependency.NextRequiresDependency;
      if OldDependency.LoadPackageResult<>lprSuccess then begin
        raise Exception.Create(Format(
            lisPkgMangThePackageIsMarkedForInstallationButCanNotBeFound, [
            OldDependency.AsString, LineEnding]));
      end;
    end;

    // check consistency
    CheckPackageGraphForCompilation(nil,
                      PackageGraph.FirstInstallDependency);

    // compile all auto install dependencies
    CompilePolicy:=pupAsNeeded;
    if (BuildRecursive and BuildAll) or Clean then
      CompilePolicy:=pupOnRebuildingAll;
    CurResult:=PackageGraph.CompileRequiredPackages(nil,
                   PackageGraph.FirstInstallDependency,false,BuildTwice,CompilePolicy);
    if CurResult<>mrOk then exit;

  finally
    PackageGraph.EndUpdate;
  end;
  Result:=true;
end;

procedure TLazBuildApplication.CompilePackage(APackage: TLazPackage;
  Flags: TPkgCompileFlags);
begin
  if APackage.Missing then
    PrintErrorAndHalt(ErrorBuildFailed, '"' + APackage.IDAsString + '": lpk file missing');

  // check graph for circles and broken dependencies
  if not (pcfDoNotCompileDependencies in Flags) then begin
    CheckPackageGraphForCompilation(APackage,nil);
  end;

  if PackageGraph.CompilePackage(APackage,Flags,false)<>mrOk then
    PrintErrorAndHalt(ErrorBuildFailed, '"' + APackage.IDAsString + '": compilation failed');
end;

procedure TLazBuildApplication.DoCreateMakefile(APackage: TLazPackage);
begin
  PrintHint('Create makefile for package "' + APackage.Filename + '"');
  PackageGraph.WriteMakeFile(APackage);
end;

procedure TLazBuildApplication.CheckPackageGraphForCompilation(
  APackage: TLazPackage; FirstDependency: TPkgDependency);
  
  function PathListToString(PathList: TFPList): string;
  var
    i: Integer;
    Item: TObject;
  begin
    Result:='';
    for i:=0 to PathList.Count-1 do begin
      Item:=TObject(PathList[i]);
      if Result<>'' then
        Result:=Result+'->';
      if Item is TPkgDependency then begin
        Result:=Result+TPkgDependency(Item).AsString;
      end else if Item is TProject then begin
        Result:=Result
                +'Project:'+ExtractFileNameOnly(TProject(Item).ProjectInfoFile);
      end else if Item is TLazPackage then begin
        Result:=Result+TLazPackage(Item).IDAsString;
      end else begin
        Result:=Result+'Unknown:'+dbgsName(Item);
      end;
    end;
  end;
  
var
  PathList: TFPList;
begin
  PathList:=nil;
  try
    // check for broken dependencies
    PathList:=PackageGraph.FindBrokenDependencyPath(APackage,FirstDependency);
    if PathList<>nil then
      PrintErrorAndHalt(ErrorLoadPackageFailed,'Broken dependency: '+PathListToString(PathList));

    // check for circle dependencies
    PathList:=PackageGraph.FindCycleDependencyPath(APackage,FirstDependency);
    if PathList<>nil then
      PrintErrorAndHalt(ErrorLoadPackageFailed,'Circle dependency: '+PathListToString(PathList));
  finally
    PathList.Free;
  end;
end;

function TLazBuildApplication.BuildProject(const AFilename: string): boolean;
var
  SrcFilename: String;
  CompReason: TCompileReason;

  function StartBuilding : boolean;
  var
    i: integer;
    HasMacro: boolean;
    NeedBuildAllFlag: Boolean;
    CfgCode: TCodeBuffer;
    CfgFilename: String;
    CompilerParams, CmdLineParams: TStrings;
    CompilerFilename, CompileHint: String;
    S, TargetExeName, WorkingDir: String;
    MatrixOption: TBuildMatrixOption;
    CompilePolicy: TPackageUpdatePolicy;
    ToolBefore, ToolAfter: TProjectCompilationToolOptions;
  begin
    Result := false;

    // override specific options
    if (OSOverride<>'') then
      Project1.CompilerOptions.TargetOS:=OSOverride;
    if (CPUOverride<>'') then
      Project1.CompilerOptions.TargetCPU:=CPUOverride;
    if SubtargetOverride then
      Project1.CompilerOptions.Subtarget:=SubtargetOverrideValue;
    if (WidgetSetOverride<>'') then begin
      MatrixOption:=Project1.BuildModes.SessionMatrixOptions.Add(bmotIDEMacro);
      MatrixOption.Modes:=Project1.ActiveBuildMode.Identifier;
      MatrixOption.MacroName:='LCLWidgetType';
      MatrixOption.Value:=WidgetSetOverride;
    end;
    if HasCustomCompilerOpts(S) then
      with Project1.CompilerOptions do
        CustomOptions := MergeCustomOptions(CustomOptions, S);

    // apply options
    MainBuildBoss.SetBuildTargetProject1(true,smsfsSkip);

    if HasLongOptIgnoreCase('get',S) or
       HasLongOptIgnoreCase('get-expand-text',S) then begin
      // check for empty text
      if S = '' then
      begin
        writeln('');            // print empty text as well
        halt(ErrorExpandMacro); // exit with error
      end;

      // check for macros
      HasMacro := false;
      for i := 1 to length(S) - 1 do // skip last char
        if (S[i] = '$') and (S[i + 1] <> '$') then begin // skip escaped '$'
          HasMacro := true;
          break;
        end;
      // if a macro is not specified, its name is assumed
      if not HasMacro then
        S := '$(' + S + ')';
      // expand
      Project1.MacroEngine.MarkUnhandledMacros := false;
      if not Project1.MacroEngine.SubstituteStr(S) then
      begin
        writeln(S);             // print partially expanded text
        halt(ErrorExpandMacro); // exit with error
      end;
      // print result
      WriteLn(S);
      exit(true);
    end;

    CompilerParams:=nil;
    CmdLineParams:=nil;
    try
      if not SkipDependencies then
      begin
        // compile required packages
        CheckPackageGraphForCompilation(nil,Project1.FirstRequiredDependency);
        PackageGraph.BeginUpdate(false);
        // automatically compile required packages
        CompilePolicy:=pupAsNeeded;
        if BuildRecursive and BuildAll then
          CompilePolicy:=pupOnRebuildingAll;
        if PackageGraph.CompileRequiredPackages(nil,Project1.FirstRequiredDependency,
                                  not (pfUseDesignTimePackages in Project1.Flags),
                                  BuildTwice,CompilePolicy)<>mrOk
        then
          PrintErrorAndHalt(ErrorBuildFailed, 'Project dependencies of "' + AFilename + '"');
      end;

      WorkingDir:=Project1.Directory;
      SrcFilename:=CreateRelativePath(Project1.MainUnitInfo.Filename,WorkingDir);

      NeedBuildAllFlag:=false;
      CompileHint:='';
      if (CompReason in Project1.CompilerOptions.CompileReasons) then begin
        // only check if NeedBuildAllFlag will be set
        MainBuildBoss.DoCheckIfProjectNeedsCompilation(Project1, NeedBuildAllFlag,CompileHint);
      end;

      // execute compilation tool 'Before'
      ToolBefore:=TProjectCompilationToolOptions(Project1.CompilerOptions.ExecuteBefore);
      if (CompReason in ToolBefore.CompileReasons) then begin
        if ToolBefore.Execute(Project1.Directory,
          lisProject2+lisExecutingCommandBefore, CompileHint)<>mrOk
        then
          PrintErrorAndHalt(ErrorBuildFailed, 'Failed "tool before" of project "' + AFilename + '"');
      end;

      // create unit output directory
      S:=Project1.CompilerOptions.GetUnitOutPath(false);
      if not ForceDirectory(S) then
        PrintErrorAndHalt(ErrorBuildFailed, 'Unable to create project unit output directory "' + S + '"');

      // create target output directory
      TargetExeName := Project1.CompilerOptions.CreateTargetFilename;
      S := ExtractFileDir(TargetExeName);
      if not ForceDirectory(S) then
        PrintErrorAndHalt(ErrorBuildFailed, 'Unable to create project target directory "' + S + '"');

      // create LazBuildApp bundle
      if Project1.UseAppBundle and (Project1.MainUnitID>=0)
      and ((MainBuildBoss.GetLCLWidgetType=LCLPlatformDirNames[lpCarbon])
        or (MainBuildBoss.GetLCLWidgetType=LCLPlatformDirNames[lpCocoa]))
      then begin
        if not (CreateApplicationBundle(TargetExeName, Project1.Title) in [mrOk,mrIgnore]) then
          PrintErrorAndHalt(ErrorBuildFailed, 'Unable to create application bundle for "' + TargetExeName + '"');
        if not (CreateAppBundleSymbolicLink(TargetExeName) in [mrOk,mrIgnore]) then
          PrintErrorAndHalt(ErrorBuildFailed, 'Unable to create application bundle symbolic link for "' + TargetExeName + '"');
      end;

      // update all lrs files
      MainBuildBoss.UpdateProjectAutomaticFiles('');

      // regenerate resources
      if not Project1.ProjResources.Regenerate(SrcFileName, False, True, '') then
        PrintWarning('Project1.Resources.Regenerate failed of "' + SrcFilename + '"');

      // get compiler parameters
      if CompilerOverride <> '' then
        CompilerFilename := CompilerOverride
      else
        CompilerFilename:=Project1.GetCompilerFilename;
      if CompilerFilename='' then
        PrintErrorAndHalt(ErrorBuildFailed, 'Invalid compiler "' + Project1.CompilerOptions.CompilerPath + '"');

      //DebugLn(['TLazBuildApplication.BuildProject CompilerFilename="',CompilerFilename,'" CompilerPath="',Project1.CompilerOptions.CompilerPath,'"']);
      // CompileHint: use absolute paths, same as TBuildManager.DoCheckIfProjectNeedsCompilation
      CompilerParams:=Project1.CompilerOptions.MakeCompilerParams([ccloAbsolutePaths]);
      CompilerParams.Add(SrcFilename);
      CmdLineParams:=CompilerParams;

      if (CompReason in Project1.CompilerOptions.CompileReasons) then begin
        // compile

        if Project1.CompilerOptions.WriteConfigFile then
        begin
          CfgFilename:=Project1.GetWriteConfigFilePath;
          CfgCode:=WriteCompilerCfgFile(CfgFilename,CompilerParams,CmdLineParams);
          if CfgCode=nil then
            PrintErrorAndHalt(ErrorBuildFailed, 'Unable to read "' + CfgFilename + '"');
          if CfgCode.FileOnDiskNeedsUpdate and (not CfgCode.Save) then
            PrintErrorAndHalt(ErrorBuildFailed, 'Unable to write "' + CfgFilename + '"');
        end;

        // write state file to avoid building clean every time
        if Project1.SaveStateFile(CompilerFilename,CompilerParams,false)<>mrOk then
          PrintErrorAndHalt(ErrorBuildFailed, 'Failed saving statefile of project "' + AFilename + '"');
        if TheCompiler.Compile(Project1,WorkingDir,CompilerFilename,CmdLineParams,
                               BuildAll or NeedBuildAllFlag,false,false,false,
                               CompileHint)<>mrOk
        then
          PrintErrorAndHalt(ErrorBuildFailed, 'Failed compiling of project "' + AFilename + '"');
        // compilation succeded -> write state file
        if Project1.SaveStateFile(CompilerFilename,CompilerParams,true)<>mrOk then
          PrintErrorAndHalt(ErrorBuildFailed, 'Failed saving statefile of project "' + AFilename + '"');
      end;

      // execute compilation tool 'After'
      ToolAfter:=TProjectCompilationToolOptions(Project1.CompilerOptions.ExecuteAfter);
      if (CompReason in ToolAfter.CompileReasons) then begin
        if ToolAfter.Execute(Project1.Directory,
          lisProject2+lisExecutingCommandAfter,CompileHint)<>mrOk
        then
          PrintErrorAndHalt(ErrorBuildFailed, 'Failed "tool after" of project "' + AFilename + '"');
      end;

      // no need to check for mrOk, we are exit if it wasn't
      Result:=true;
    finally
      if CmdLineParams<>CompilerParams then
        CmdLineParams.Free;
      CompilerParams.Free;
      if not SkipDependencies then
        PackageGraph.EndUpdate;
    end;
  end;

var
  i, MatchCount: Integer;
  ModeMask: TMask;
  CurResult: Boolean;
  S: string;
begin
  Result:=false;
  CloseProject(Project1);

  if not Init then exit;

  Project1:=LoadProject(AFilename);
  
  if Project1.MainUnitInfo=nil then
    PrintErrorAndHalt(ErrorBuildFailed, 'Project has no main unit');

  if BuildAll then
    CompReason:= crBuild
  else
    CompReason:= crCompile;

  if HasLongOptIgnoreCase('get-build-modes',S) then begin
    ShowBuildModes;
    exit(true);
  end;

  // first override build mode
  if BuildModeOverride<>'' then
  begin
    CurResult := true;
    MatchCount := 0;
    ModeMask := TMask.Create(BuildModeOverride);
    for i := 0 to Project1.BuildModes.Count-1 do
    begin
      if ModeMask.Matches(Project1.BuildModes[i].Identifier) then
      begin
        inc(MatchCount);
        Project1.ActiveBuildMode := Project1.BuildModes[i];
        CurResult := CurResult and StartBuilding;
      end;
    end;
    ModeMask.Free;
    if MatchCount=0 then // No matches
      ShowBuildModeError(BuildModeOverride);
    Result := CurResult;
  end
  else
    Result := StartBuilding;

  // Auto increment build number
  if Result and BuildAll and not NoWriteProject
  and Project1.ProjResources.VersionInfo.UseVersionInfo
  and Project1.ProjResources.VersionInfo.AutoIncrementBuild
  then begin
    if FileIsWritable(AFilename) then begin
      Project1.ProjResources.DoAfterBuild(CompReason, Project1.IsVirtual);
      Project1.WriteProject(Project1.PublishOptions.WriteFlags,AFileName,EnvironmentOptions.BuildMatrixOptions)
    end else
      PrintWarning('Project1.WriteProject skipped for read-only "' + SrcFilename + '"');
  end;
end;

function TLazBuildApplication.LoadProject(const AFilename: string): TProject;
var
  ProjectDesc: TProjectDescriptor;
begin
  ProjectDesc:=TProjectDescriptor.Create;
  try
    Result:=TProject.Create(ProjectDesc);
    // custom initialization
    Result.BeginUpdate(true);
    if ProjectDesc.InitProject(Result)<>mrOk then begin
      Result.EndUpdate;
      Result.Free;
      exit(nil);
    end;
    Result.EndUpdate;

    Result.MainProject:=true;
    Result.OnFileBackup:=@BuildBoss.BackupFileForWrite;
    Result.OnChangeProjectInfoFile:=@ProjectInfoFileChanged;

  finally
    ProjectDesc.Free;
  end;

  Result.BeginUpdate(true);
  try
    // read project info file
    if Result.ReadProject(AFilename,EnvironmentOptions.BuildMatrixOptions)<>mrOk then
      PrintErrorAndHalt(ErrorLoadProjectFailed,'Project '+AFilename);
    //BuildBoss.RescanCompilerDefines(true);

    // load required packages
    PackageGraph.OpenRequiredDependencyList(Result.FirstRequiredDependency);
  finally
    Result.EndUpdate;
  end;
  IncreaseCompilerParseStamp;
end;

procedure TLazBuildApplication.CloseProject(var AProject: TProject);
begin
  // free project, if it is still there
  FreeThenNil(AProject);
end;

function TLazBuildApplication.AddPackagesToInstallList(
  const PackageNamesOrFiles: TStringList): boolean;
var
  i: integer;
  Package: TLazPackage;
  PackageLink: TLazPackageLink;
  PackageName: String;
  PkgFilename: String;
  ErrorMsg: String;
  ErrCode: Byte;
begin
  Result:=false;
  if not Init then exit;

  LoadMiscellaneousOptions;

  ErrorMsg:='';
  ErrCode:=ErrorPackageNameInvalid;
  for i:=0 to PackageNamesOrFiles.Count -1 do
  begin
    // Look for package name in all known packages
    PackageName:='';
    PkgFilename:='';
    if pvPkgSearch in fPkgGraphVerbosity then
      debugln(['Info: (lazbuild) [TLazBuildApplication.AddPackagesToInstallList] "',PackageNamesOrFiles[i],'"']);
    if FilenameExtIs(PackageNamesOrFiles[i],'lpk',true) then
      PkgFilename:=ExpandFileNameUTF8(PackageNamesOrFiles[i])
    else if IsValidPkgName(PackageNamesOrFiles[i]) then begin
      PackageLink:=TLazPackageLink(LazPackageLinks.FindLinkWithPkgName(PackageNamesOrFiles[i]));
      if PackageLink=nil then
      begin
        ErrorMsg+='Can not find package "'+PackageNamesOrFiles[i]+'", so it is not marked for installation.'+LineEnding;
        continue;
      end;
      PkgFilename:=PackageLink.LPKFilename;
    end else begin
      ErrorMsg+='"'+PackageNamesOrFiles[i]+'" is not a package, so it is not marked for installation.'+LineEnding;
      continue;
    end;
    Package:=LoadPackage(PkgFilename);
    if Package=nil then
    begin
      ErrorMsg+='Could not load "'+PackageNamesOrFiles[i]+'", so it is not marked for installation.'+LineEnding;
      ErrCode:=ErrorLoadPackageFailed;
      continue;
    end;
    if Package.PackageType in [lptRunTime,lptRunTimeOnly] then
    begin
      ErrorMsg+='Package "'+PackageNamesOrFiles[i]+'" is only for runtime.'+LineEnding;
      continue;
    end;
    PackageName:=Package.Name;
    // set it as (static) autoinstall: select for installation
    PrintHint('Adding package "' + PkgFilename + '" to install list of IDE');
    if MiscellaneousOptions.BuildLazProfiles.StaticAutoInstallPackages.IndexOf(PackageName)<0 then
      MiscellaneousOptions.BuildLazProfiles.StaticAutoInstallPackages.Add(PackageName);
  end;
  if ErrorMsg<>'' then
    PrintErrorAndHalt(ErrCode,UTF8Trim(ErrorMsg));

  // save list
  MiscellaneousOptions.Save;
  LazPackageLinks.SaveUserLinks(true);

  Result:=true;
end;

function TLazBuildApplication.AddCmdLinePackageLinks(
  const PackageNamesOrFiles: TStringList): boolean;
var
  ErrorMsg, PkgFilename: String;
  i, ErrCode: Integer;
  Package: TLazPackage;
begin
  Result:=false;
  if not Init then exit;

  ErrorMsg:='';
  ErrCode:=ErrorLoadPackageFailed;
  for i:=0 to PackageNamesOrFiles.Count -1 do
  begin
    // Look for package name in all known packages
    PkgFilename:=PackageNamesOrFiles[i];
    if not FilenameExtIs(PkgFilename,'lpk',true) then begin
      ErrorMsg+='"' + PkgFilename+'" is not a package (.lpk), so it is not registered.'+LineEnding;
      continue;
    end;
    PkgFilename:=ExpandFileNameUTF8(PkgFilename);

    Package:=LoadPackage(PkgFilename);
    if Package=nil then
    begin
      ErrorMsg+='Could not load "'+PkgFilename+'", so it is not registered.'+LineEnding;
      continue;
    end;
    PrintHint('Registering package link "' + PkgFilename + '"');
    LazPackageLinks.AddUserLink(Package);
  end;
  if ErrorMsg<>'' then
    PrintErrorAndHalt(ErrCode,UTF8Trim(ErrorMsg));

  LazPackageLinks.SaveUserLinks(true);
  Result:=true;
end;

function TLazBuildApplication.Init: boolean;
begin
  if fInitialized then exit(fInitResult);
  fInitResult:=false;
  fInitialized:=true;

  try
    PrintHint('Primary config path: "' + GetPrimaryConfigPath + '"');
    CreatePrimaryConfigPath;

    MainBuildBoss:=TBuildManager.Create(nil);
    SetupMacros;
    LoadEnvironmentOptions;
    if Terminated then exit(false);
    LoadMiscellaneousOptions;
    SetupLazarusDirectory;
    SetupCodetools;
    SetupFPCExeFilename;
    SetupPackageSystem;
    MainBuildBoss.SetupExternalTools(TExternalToolsConsole);
    ExtToolConsole:=TLazExtToolConsole.Create(nil);
    MainBuildBoss.SetupCompilerInterface;

    StoreBaseSettings;

    // load static base packages
    PackageGraph.LoadStaticBasePackages;

    MainBuildBoss.SetBuildTarget(OSOverride,CPUOverride,SubtargetOverrideValue,
                                 WidgetSetOverride,smsfsSkip,true);
  except
    PrintErrorAndHalt(ErrorInitialization, 'Initialization error');
  end;

  fInitResult:=true;
  Result:=fInitResult;
end;

procedure TLazBuildApplication.LoadEnvironmentOptions;
var
  Note, Lang: string;
begin
  EnvironmentOptions.CreateConfig;
  EnvironmentOptions.Load(false);
  fCompilerInCfg:=EnvironmentOptions.CompilerFilename;
  fLazarusDirInCfg:=EnvironmentOptions.LazarusDirectory;

  if HasLongOptIgnoreCase('language',Lang) then
    EnvironmentOptions.LanguageID:=Lang;
  TranslateResourceStrings(EnvironmentOptions.GetParsedLazarusDirectory,
                           EnvironmentOptions.LanguageID);
  if CompilerOverride<>'' then
    EnvironmentOptions.CompilerFilename:=CompilerOverride;
  //debugln(['TLazBuildApplication.LoadEnvironmentOptions LazarusDirectory="',LazarusDirectory,'"']);
  if LazarusDirOverride<>'' then
    EnvironmentOptions.LazarusDirectory:=CleanAndExpandDirectory(LazarusDirOverride);
  if MaxProcessCount>=0 then
    // set command line override
    EnvironmentOptions.MaxExtToolsInParallel:=MaxProcessCount;
  if not FileExistsUTF8(EnvironmentOptions.GetParsedLazarusDirectory
                        +GetForcedPathDelims('packager/registration/fcl.lpk'))
  then begin
    CheckLazarusDirectoryQuality(EnvironmentOptions.GetParsedLazarusDirectory,Note);
    PrintErrorAndHalt(ErrorInitialization, 'Invalid Lazarus directory "' + EnvironmentOptions.LazarusDirectory + '": ' + Note);
  end;
end;

procedure TLazBuildApplication.LoadMiscellaneousOptions;
begin
  if MiscellaneousOptions<>nil then exit;
  MiscellaneousOptions:=TMiscellaneousOptions.Create;
  MiscellaneousOptions.Load;
end;

procedure TLazBuildApplication.SetupMacros;
begin
  MainBuildBoss.SetupTransferMacros;
  (IDEMacros as TLazIDEMacros).LoadLazbuildMacros;
end;

procedure TLazBuildApplication.SetupCodetools;
begin
  // create a test unit needed to get from the compiler all macros and search paths
  CodeToolBoss.CompilerDefinesCache.TestFilename:=CreateCompilerTestPascalFilename;
  CodeToolBoss.SourceCache.OnEncodeSaving:=@CodeBufferEncodeSaving;
  CodeToolBoss.SourceCache.OnDecodeLoaded:=@CodeBufferDecodeLoaded;
  CodeToolBoss.SourceCache.DefaultEncoding:=EncodingUTF8;

  MainBuildBoss.LoadCompilerDefinesCaches;
  // create a test unit needed to get from the compiler all macros and search paths
  CodeToolBoss.CompilerDefinesCache.TestFilename:=CreateCompilerTestPascalFilename;
end;

procedure TLazBuildApplication.SetupPackageSystem;
begin
  OnGetDependencyOwnerDescription:=@GetDependencyOwnerDescription;
  OnGetDependencyOwnerDirectory:=@GetDependencyOwnerDirectory;

  // package links
  LazPackageLinks:=TLazPackageLinks.Create;
  LazPackageLinks.UpdateAll;

  // package graph
  PackageGraph:=TLazPackageGraph.Create;
  PackageGraphInterface:=PackageGraph;
  PackageGraph.OnAddPackage:=@PackageGraphAddPackage;
  PackageGraph.OnCheckInterPkgFiles:=@PackageGraphCheckInterPkgFiles;
  PackageGraph.Verbosity:=PkgGraphVerbosity;
end;

procedure TLazBuildApplication.SetupDialogs;
begin
  LazMessageWorker:=@IDEMessageDialogHandler;
  LazQuestionWorker:=@IDEQuestionDialogHandler;
end;

procedure TLazBuildApplication.StoreBaseSettings;
var
  StoreLazDir: Boolean;
  StoreCompPath: Boolean;
  Cfg: TXMLConfig;
begin
  StoreLazDir:=(fLazarusDirInCfg='') and (EnvironmentOptions.LazarusDirectory<>'');
  StoreCompPath:=(fCompilerInCfg='') and (EnvironmentOptions.CompilerFilename<>'');
  if (not StoreLazDir) and (not StoreCompPath) then exit;

  try
    PrintInfo('Storing in "' + EnvironmentOptions.Filename + '":');
    if StoreLazDir then
      PrintInfo('  Lazarus directory: "' + EnvironmentOptions.LazarusDirectory + '"');
    if StoreCompPath then
      PrintInfo('  Compiler path: "' + EnvironmentOptions.CompilerFilename + '"');

    Cfg:=TXMLConfig.Create(EnvironmentOptions.Filename);
    try
      if StoreLazDir then
        Cfg.SetValue('EnvironmentOptions/LazarusDirectory/Value',
                     EnvironmentOptions.LazarusDirectory);
      if StoreCompPath then
        Cfg.SetValue('EnvironmentOptions/CompilerFilename/Value',
                     EnvironmentOptions.CompilerFilename);
      Cfg.Flush;
    finally
      Cfg.Free;
    end;
  except
    PrintWarning('Unable to edit file "' + EnvironmentOptions.Filename + '"');
  end;
end;

function TLazBuildApplication.RepairedCheckOptions(const ShortOptions: String;
  const Longopts: TStrings; Opts, NonOpts: TStrings): String;

  Function FindLongOpt(S : String) : boolean;
  Var
    I : integer;
  begin
    I:=LongOpts.Count-1;
    While (I>=0) and (LongOpts[I]<>S) do
      Dec(I);
    Result:=(I<>-1);
  end;

Var
  I,J,L,P : Integer;
  O,OV : String;
  HaveArg : Boolean;
  NeedArg: Boolean;
begin
  Result:='';
  I:=1;
  While (I<=ToolParamCount) and (Result='') do
    begin
    O:=ToolParamStr(I);
    If (Length(O)=0) or (O[1]<>OptionChar) then
      begin
      If Assigned(NonOpts) then
        NonOpts.Add(O)
      end
    else
      begin
      If (Length(O)<2) then
        Result:=Format(lisErrInvalidOption,[i,O])
      else
        begin
        HaveArg:=False;
        OV:='';
        // Long option ?
        If (O[2]=OptionChar) then
          begin
          Delete(O,1,2);
          J:=Pos('=',O);
          If J<>0 then
            begin
            HaveArg:=true;
            OV:=O;
            Delete(OV,1,J);
            O:=Copy(O,1,J-1);
            end;
          O:=LowerCase(O); // Long options are case-insensitive.
          // Switch Option
          If FindLongopt(O) then
            begin
            If HaveArg then
              Result:=Format(lisErrNoOptionAllowed,[I,O]);
            end
          else
            begin // Required argument
            If FindLongOpt(O+':') then
              begin
              If Not HaveArg then
                Result:=Format(lisErrOptionNeeded,[I,O]);
              end
            else
              begin // Optional Argument.
              If not FindLongOpt(O+'::') then
                Result:=Format(lisErrInvalidOption,[I,O]);
              end;
            end;
          end
        else // Short Option.
          begin
          HaveArg:=(I<ToolParamCount) and (Length(ToolParamStr(I+1))>0)
                   and (ToolParamStr(I+1)[1]<>OptionChar);
          If HaveArg then
            OV:=ToolParamStr(I+1);
          L:=Length(O);
          J:=2;
          NeedArg:=false;
          While (result='') and (J<=L) do
            begin
            P:=Pos(O[J],ShortOptions);
            If (P=0) or (O[j]=':') then
              Result:=Format(lisErrInvalidOption,[I,O[J]])
            else
              begin
              If (P<Length(ShortOptions)) and (Shortoptions[P+1]=':') then
                begin
                // Required argument
                NeedArg:=true;
                PrintInfo( Format('P %d J %d %s %d Havearg %s', [P, J, O[J], l, BoolToStr(HaveArg, true)]) );
                If ((P+1)=Length(ShortOptions)) or (Shortoptions[P+2]<>':') Then
                  If (J<L) or not haveArg then // Must be last in multi-opt !!
                    Result:=Format(lisErrOptionNeeded,[I,O[J]]);
                O:=O[j]; // O is added to arguments.
                end;
              end;
            Inc(J);
            end;
          if not NeedArg then HaveArg:=false;
          If HaveArg then
            begin
            Inc(I); // Skip argument.
            O:=O[Length(O)]; // O is added to arguments !
            end;
          end;
        If HaveArg and (Result='') then
          If Assigned(Opts) then
            Opts.Add(O+'='+OV);
        end;
      end;
    Inc(I);
    end;
end;

constructor TLazBuildApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SetupDialogs;
  Files:=TStringList.Create;
  fMaxProcessCount:=-1;
end;

destructor TLazBuildApplication.Destroy;
begin
  CloseProject(Project1);

  if Assigned(PackageGraph) then
  begin
    PackageGraph.FreeAutoInstallDependencies;
    FreeThenNil(PackageGraph);
  end;

  FreeThenNil(LazPackageLinks);
  FreeThenNil(TheCompiler);
  FreeAndNil(ExtToolConsole);
  FreeThenNil(GlobalMacroList);
  FreeThenNil(IDEMacros);
  FreeThenNil(MiscellaneousOptions);
  FreeThenNil(EnvironmentOptions);
  FreeThenNil(MainBuildBoss);

  FreeAndNil(Files);
  inherited Destroy;
end;

procedure TLazBuildApplication.Run;
var
  i: Integer;
begin
  if not ParseParameters then exit;

  // Build all projects/packages specified by the user...
  // except packages to be added the IDE install list.
  for i:=0 to Files.Count-1 do
    if not BuildFile(Files[i]) then
      PrintErrorAndHalt(ErrorBuildFailed, 'Building failed: "' + Files[i] + '"');

  // Add user-requested packages to IDE install list:
  case PackageAction of
  lpaInstall:
    if not AddPackagesToInstallList(Files) then
      PrintErrorAndHalt(ErrorBuildFailed, 'Installing package(s) failed: "' + Files.Text + '"');
  lpaAddPkgLinks:
    if not AddCmdLinePackageLinks(Files) then
      PrintErrorAndHalt(ErrorBuildFailed, 'Adding package(s) links failed: "' + Files.Text + '"');
  end;

  if BuildIDE then
    if not BuildLazarusIDE then
      PrintErrorAndHalt(ErrorBuildFailed, '');
end;

function TLazBuildApplication.HasLongOptIgnoreCase(const S: String; out
  aValue: String): Boolean;
// Check existence of a long option case-insensitively.
begin
  CaseSensitiveOptions:=False;
  Result:=HasOption(S);
  if Result then
    aValue:=GetOptionValue(S)
  else
    aValue:='';
  CaseSensitiveOptions:=True;
end;

function TLazBuildApplication.HasShortOrLongOpt(const C: Char; const S: String
  ): Boolean;
// Check existence of a short option casesensitively and long option case-insensitively.
begin
  Result:=HasOption(C);
  CaseSensitiveOptions:=False;
  Result:=Result or HasOption(S);
  CaseSensitiveOptions:=True;
end;

function TLazBuildApplication.ParseParameters: boolean;
var
  Options: TStringList;
  NonOptions: TStringList;
  LongOptions: TStringList;
  i: Integer;
  p, ErrorMsg: String;
  FilesNeeded: Boolean;
begin
  Result:=False;
  if (ToolParamCount<=0)
   or (CompareText(ToolParamStr(1),'--help')=0)
   or (CompareText(ToolParamStr(1),'-help')=0)
   or (CompareText(ToolParamStr(1),'-?')=0)
   or (CompareText(ToolParamStr(1),'-h')=0)
  then begin
    WriteUsage;
    exit;
  end;
  if HasShortOrLongOpt('h','help') or HasOption('?') then begin
    WriteUsage;
    exit;
  end;
  if HasShortOrLongOpt('v','version') then begin
    writeln(VersionStr);
    exit;
  end;

  // ConsoleVerbosity
  if HasLongOptIgnoreCase('get-build-modes', p) or
     HasLongOptIgnoreCase('get-expand-text', p) or
     HasLongOptIgnoreCase('get', p)
  then
    ConsoleVerbosity := -100 // do not output anything other than the result
  else
    for i:=1 to ToolParamCount do begin
      p:=ToolParamStr(i);
      if CompareText(p, '--verbose') = 0 then
        ConsoleVerbosity:=Max(+1,ConsoleVerbosity+1)
      else if (p='-q') or (CompareText(p, '--quiet') = 0) then
        ConsoleVerbosity:=Min(-1,ConsoleVerbosity-1);
    end;
  CTConsoleVerbosity:=ConsoleVerbosity;

  Options:=TStringList.Create;
  NonOptions:=TStringList.Create;
  LongOptions:=TStringList.Create;
  try
    LongOptions.Add('quiet');
    LongOptions.Add('verbose');
    LongOptions.Add('verbose-pkgsearch');
    LongOptions.Add('primary-config-path:');
    LongOptions.Add('pcp:');
    LongOptions.Add('secondary-config-path:');
    LongOptions.Add('scp:');
    LongOptions.Add('language:');
    LongOptions.Add('add-package');
    LongOptions.Add('add-package-link');
    LongOptions.Add('build-all');
    LongOptions.Add('build-ide::'); // value is optional
    LongOptions.Add('build-twice');
    LongOptions.Add('recursive');
    LongOptions.Add('skip-dependencies');
    LongOptions.Add('widgetset:');
    LongOptions.Add('ws:');
    LongOptions.Add('operating-system:');
    LongOptions.Add('os:');
    LongOptions.Add('cpu:');
    LongOptions.Add('subtarget:');
    LongOptions.Add('bm:');
    LongOptions.Add('build-mode:');
    LongOptions.Add('opt:');
    LongOptions.Add('compiler:');
    LongOptions.Add('lazarusdir:');
    LongOptions.Add('create-makefile');
    LongOptions.Add('max-process-count:');
    LongOptions.Add('no-write-project');
    LongOptions.Add('get-expand-text:');
    LongOptions.Add('get:');
    LongOptions.Add('get-build-modes');
    ErrorMsg:=RepairedCheckOptions('lBrdq',LongOptions,Options,NonOptions);
    if ErrorMsg<>'' then
      PrintErrorAndHalt(ErrorInvalidSyntax, ErrorMsg);

    // lazarus config
    if FileExistsUTF8(GetCfgFileName) then
      PrintInfo('Using config file: "' + GetCfgFileName + '"');

    FilesNeeded:=true;

    if HasLongOptIgnoreCase('verbose-pkgsearch',p) then
      Include(fPkgGraphVerbosity,pvPkgSearch);

    // PackageAction: register lpk files
    if HasLongOptIgnoreCase('add-package-link',p) then begin
      PrintInfo('Parameter: --add-package-link');
      if PackageAction<>lpaBuild then
        PrintErrorAndHalt(ErrorInvalidSyntax, 'Invalid combination of package actions');
      FilesNeeded:=false;
      PackageAction:=lpaAddPkgLinks;
    end;

    // PackageAction: install lpk files
    if HasLongOptIgnoreCase('add-package',p) then begin
      PrintInfo('Parameter: --add-package');
      if PackageAction<>lpaBuild then
        PrintErrorAndHalt(ErrorInvalidSyntax, 'Invalid combination of package actions');
      PackageAction:=lpaInstall;
      FilesNeeded:=false;
    end;

    // building IDE
    if HasLongOptIgnoreCase('build-ide', FBuildIDEOptions) then begin
      BuildIDE:=true;
      FilesNeeded:=false;
      PrintInfo('Parameter: --build-ide="' + BuildIDEOptions + '"');
    end;

    // files
    Files.Assign(NonOptions);
    if FilesNeeded and (Files.Count=0) then
      PrintErrorAndHalt(ErrorInvalidSyntax, 'Missing file');

    // primary config path
    if HasLongOptIgnoreCase('primary-config-path',p) then begin
      SetPrimaryConfigPath(p);
      PrintInfo('Parameter: --primary-config-path="' + GetPrimaryConfigPath + '"');
    end else if HasLongOptIgnoreCase('pcp',p) then begin
      SetPrimaryConfigPath(p);
      PrintInfo('Parameter: --pcp="' + GetPrimaryConfigPath + '"');
    end;

    // secondary config path
    if HasLongOptIgnoreCase('secondary-config-path',p) then begin
      SetPrimaryConfigPath(p);
      PrintInfo('Parameter: --secondary-config-path="' + GetSecondaryConfigPath + '"');
    end else if HasLongOptIgnoreCase('scp',p) then begin
      SetSecondaryConfigPath(p);
      PrintInfo('Parameter: --scp="' + GetSecondaryConfigPath + '"');
    end;

    // build all
    if HasShortOrLongOpt('B','build-all') then begin
      BuildAll:=true;
      PrintInfo('Parameter: --build-all');
    end;
    if HasOption('build-twice') then begin
      BuildTwice:=true;
      PrintInfo('Parameter: --build-twice');
    end;
    if HasShortOrLongOpt('r','recursive') then begin
      BuildAll:=true;
      BuildRecursive:=true;
      PrintInfo('Parameter: --recursive');
    end;
    if HasShortOrLongOpt('d','skip-dependencies') then begin
      SkipDependencies:=true;
      PrintInfo('Parameter: --skip-dependencies');
    end;
    if BuildRecursive and SkipDependencies then
      PrintErrorAndHalt(ErrorInvalidSyntax, '"--recursive" and "--skip-dependencies" options are incompatible');

    { Overrides }

    // widgetset
    if HasLongOptIgnoreCase('ws',FWidgetSetOverride) then
      PrintInfo('Parameter: --ws=' + WidgetSetOverride)
    else if HasLongOptIgnoreCase('widgetset',FWidgetSetOverride) then
      PrintInfo('Parameter: --widgetset=' + WidgetSetOverride);

    // operating system
    if HasLongOptIgnoreCase('os',FOSOverride) then
      PrintInfo('Parameter: --os=' + OSOverride)
    else if HasLongOptIgnoreCase('operating-system',FOSOverride) then
      PrintInfo('Parameter: --operating-system=' + OSOverride);

    // cpu
    if HasLongOptIgnoreCase('cpu',FCPUOverride) then
      PrintInfo('Parameter: --cpu=' + CPUOverride);

    // subtarget
    if HasLongOptIgnoreCase('subtarget',FSubtargetOverrideValue) then begin
      FSubtargetOverride:=true;
      PrintInfo('Parameter: --subtarget=' + FSubtargetOverrideValue);
    end;

    // build mode
    if HasLongOptIgnoreCase('bm',FBuildModeOverride) then
      PrintInfo('Parameter: --bm="' + BuildModeOverride + '"')
    else if HasLongOptIgnoreCase('build-mode',FBuildModeOverride) then
      PrintInfo('Parameter: --build-mode="' + BuildModeOverride + '"');

    // compiler
    if HasLongOptIgnoreCase('compiler',FCompilerOverride) then
      PrintInfo('Parameter: --compiler="' + CompilerOverride + '"');

    // lazarusdir
    if HasLongOptIgnoreCase('lazarusdir',FLazarusDirOverride) then
      PrintInfo('Parameter: --lazarusdir="' + LazarusDirOverride + '"');

    // language
    if HasLongOptIgnoreCase('language',p) then
      PrintInfo('Parameter: --language=' + p);

    // max-process-count
    if HasLongOptIgnoreCase('max-process-count',p) then begin
      MaxProcessCount:=StrToInt(p);
      PrintInfo('Parameter: --max-process-count=' + p);
    end;

    if HasLongOptIgnoreCase('no-write-project',p) then begin
      NoWriteProject := true;
      PrintInfo('Parameter: --no-write-project');
    end;

    if HasLongOptIgnoreCase('create-makefile',p) then
    begin
      CreateMakefile := true;
      PrintInfo('Parameter: --create-makefile');
      if PackageAction<>lpaBuild then
        PrintErrorAndHalt(ErrorInvalidSyntax, '"--create-makefile" and "--add-package" options are incompatible');
    end;
  finally
    Options.Free;
    NonOptions.Free;
    LongOptions.Free;
  end;
  Result:=true;
end;

procedure TLazBuildApplication.WriteUsage;
var
  CustomLang: string;
const
  cDescrIndent = 16;
  cMaxLength = 80;

  procedure w(Msg: string);
  begin
    writeln(UTF8ToConsole(StringOfChar(' ', cDescrIndent) + UTF8WrapText(Msg, LineEnding, [' ', #9], cMaxLength, cDescrIndent)));
  end;

begin
  // Obtain 'language' option value if present.
  // HasLongOptIgnoreCase correctly handles all cases, so no need to check its result:
  // empty CustomLang means using default language.
  HasLongOptIgnoreCase('language',CustomLang);
  TranslateResourceStrings(ProgramDirectoryWithBundle,CustomLang);
  writeln('');
  writeln(lisLazbuildOptionsSyntax);
  writeln('');
  writeln(lisEdtExtToolParameters);
  writeln('');
  writeln('-?, -h, --help');
  w(lisThisHelpMessage);
  writeln('');
  writeln('-B, --build-all');
  w(lisBuildAllFilesOfProjectPackageIDE);
  writeln('');
  writeln('--build-twice');
  w(lisCompilePackageTwiceAndCheckIfAnyUnitWasCompiledAga);
  writeln('');
  writeln('-r, --recursive');
  w(lisApplyBuildFlagsBToDependenciesToo);
  writeln('');
  writeln('-d, --skip-dependencies');
  w(lisDoNotCompileDependencies);
  writeln('');
  writeln('--build-ide=<options>');
  w(lisBuildIDEWithPackages);
  writeln('');
  writeln('-v, --version');
  w(lisShowVersionAndExit);
  writeln('');
  writeln('-q, --quiet');
  w(lisBeLessVerboseCanBeGivenMultipleTimes);
  w(lisPassingQuietTwoTimesWillP);
  writeln('');
  writeln('--verbose');
  w(lisBeMoreVerboseCanBeGivenMultipleTimes);
  writeln('');
  writeln('--verbose-pkgsearch');
  w(lisWriteWhatPackageFilesAreS);
  writeln('');

  writeln('--add-package');
  w(lisAddPackageSToListOfInstalledPackagesCombineWithBui);
  writeln('');
  writeln('--add-package-link=<.lpk file>');
  w(lisOnlyRegisterTheLazarusPackageFilesLpkDoNotBuild);
  writeln('');
  writeln('--create-makefile');
  w(lisInsteadOfCompilePackageCreateASimpleMakefile);
  writeln('');

  writeln(PrimaryConfPathOptShort,'<path>, ',PrimaryConfPathOptLong,'<path>');
  w(Format(lisprimaryConfigDirectoryWhereLazarusStoresItsConfig, [LazConf.GetPrimaryConfigPath]));
  writeln('');
  writeln(SecondaryConfPathOptShort,'<path>, ',SecondaryConfPathOptLong,'<path>');
  w(Format(lissecondaryConfigDirectoryWhereLazarusSearchesFor, [LazConf.GetSecondaryConfigPath]));
  writeln('');
  writeln('--os=<operating-system>, --operating-system=<operating-system>');
  w(Format(lisOverrideTheProjectOperatingSystemEGWin32LinuxDefau, [FPCAdds.GetCompiledTargetOS]));
  writeln('');
  writeln('--ws=<widgetset>, --widgetset=<widgetset>');
  w(Format(lisOverrideTheProjectWidgetsetEGDefault, ['gtk2 qt qt5 qt6 win32 cocoa', LCLPlatformDirNames[GetBuildLCLWidgetType]]));
  writeln('');
  writeln('--cpu=<cpu>');
  w(Format(lisOverrideTheProjectCpuEGI386X86_64PowerpcPowerpc_64, [FPCAdds.GetCompiledTargetCPU]));
  writeln('');
  writeln('--subtarget=<subtarget>');
  w(lisOverrideTheProjectSubtarg);
  writeln('');
  writeln('--bm=<project/IDE build mode>, --build-mode=<project/IDE build mode>');
  w(lisOverrideTheProjectBuildMode);
  writeln('');
  writeln('--opt=<extra-options>');
  w(lisExtraOpts);
  writeln('');
  writeln('--compiler=<ppcXXX>');
  w(lisOverrideTheDefaultCompilerEGPpc386Ppcx64PpcppcEtcD);
  writeln('');
  writeln(LanguageOpt,'<Language ID>');
  w(lisOverrideLanguage);
  writeln('');
  writeln('--lazarusdir=<Lazarus directory>');
  w(lisLazarusDirOverride);
  writeln('');
  writeln('--max-process-count=<count>');
  w(lisMaximumNumberOfThreadsForCompilingInParallelDefaul);
  writeln('');
  writeln('--no-write-project');
  w(lisDoNotWriteUpdatedProjectInfoAfterBuild);
  writeln('');
  writeln('--get=<text>, --get-expand-text=<text>');
  w(lisGetExpandText);
  writeln('');
  writeln('--get-build-modes');
  w(lisGetBuildModes);
  writeln('');
end;

procedure TLazBuildApplication.PrintInfo(const Msg: string);
begin
  if ConsoleVerbosity >= 1 then
    debugln('Info: (lazbuild) ', LineBreaksToSystemLineBreaks(Msg));
end;

procedure TLazBuildApplication.PrintHint(const Msg: string);
begin
  if ConsoleVerbosity >= 0 then
    debugln('Hint: (lazbuild) ', LineBreaksToSystemLineBreaks(Msg));
end;

procedure TLazBuildApplication.PrintWarning(const Msg: string);
begin
  if ConsoleVerbosity >= -1 then
    debugln('Warning: (lazbuild) ', LineBreaksToSystemLineBreaks(Msg));
end;

procedure TLazBuildApplication.PrintErrorAndHalt(Code: Byte; const Msg: string);
begin
  if Msg <> '' then
    debugln('Error: (lazbuild) ', LineBreaksToSystemLineBreaks(Msg));
  halt(Code);
end;

begin
  // When quick rebuilding lazbuild, FPC rebuilds only the lazbuild.lpr, so any
  // flag that should work with quick build must be set here.
  // At the moment there is no flag

  HasGUI:=false;
  FilterConfigFileContent;
  // start our own LazBuildApp
  LazBuildApp:=TLazBuildApplication.Create(nil);
  LazBuildApp.Run;
  LazBuildApp.Free;
end.

