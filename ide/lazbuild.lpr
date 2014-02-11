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
  {$IFDEF unix}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, LCLProc, Dialogs, Forms, Controls, FileUtil,
  Interfaces, InterfaceBase, UTF8Process, LConvEncoding,
  // codetools
  CodeCache, CodeToolManager, DefineTemplates, Laz2_XMLCfg, LazUTF8,
  // IDEIntf
  MacroIntf, PackageIntf, IDEDialogs, ProjectIntf, IDEExternToolIntf,
  CompOptsIntf, LazIDEIntf,
  // IDE
  IDEProcs, InitialSetupDlgs, OutputFilter, CompilerOptions, ApplicationBundle,
  TransferMacros, EnvironmentOpts, IDETranslations, LazarusIDEStrConsts,
  IDECmdLine, ExtToolDialog, MiscOptions, Project, LazConf, PackageDefs,
  PackageLinks, PackageSystem, BuildLazDialog, BuildProfileManager,
  BuildManager, BaseBuildManager, ModeMatrixOpts;
  
type

  { TLazBuildApplication }

  TLazBuildApplication = class(TCustomApplication)
  private
    FAddPackage: boolean;
    FBuildAll: boolean;
    FBuildIDE: boolean;
    FBuildIDEOptions: string;
    FBuildModeOverride: String;
    FBuildRecursive: boolean;
    fCompilerOverride: String;
    fCompilerInCfg: string;
    FCreateMakefile: boolean;
    fLazarusDirOverride : String;
    fLazarusDirInCfg: string;
    fCPUOverride: String;
    fOSOverride: String;
    FSkipDependencies: boolean;
    fInitialized: boolean;
    fInitResult: boolean;
    fWidgetsetOverride: String;
    // external tools
    procedure OnExtToolFreeOutputFilter({%H-}OutputFilter: TOutputFilter;
                                        ErrorOccurred: boolean);
    procedure OnExtToolNeedsOutputFilter(var OutputFilter: TOutputFilter;
                                         var {%H-}Abort: boolean);

    // codetools
    procedure OnCodeBufferDecodeLoaded({%H-}Code: TCodeBuffer;
         const {%H-}Filename: string; var Source, DiskEncoding, MemEncoding: string);
    procedure OnCodeBufferEncodeSaving(Code: TCodeBuffer;
                                    const {%H-}Filename: string; var Source: string);

    // global package functions
    procedure GetDependencyOwnerDescription(Dependency: TPkgDependency;
                                            out Description: string);
    procedure GetDependencyOwnerDirectory(Dependency: TPkgDependency;
                                          out Directory: string);
    // Event procedure that adds every package added to the package graph to the (user) package links
    procedure PackageGraphAddPackage(Pkg: TLazPackage);

    // project
    procedure OnProjectChangeInfoFile(TheProject: TProject);
    procedure OnProjectGetTestDirectory({%H-}TheProject: TProject; out
      TestDir: string);

    // dialogs
    function OnIDEMessageDialog(const aCaption, aMsg: string;
                                {%H-}DlgType: TMsgDlgType; {%H-}Buttons: TMsgDlgButtons;
                                const {%H-}HelpKeyword: string): Integer;
    function OnIDEQuestionDialog(const aCaption, aMsg: string;
                                 {%H-}DlgType: TMsgDlgType; {%H-}Buttons: array of const;
                                 const {%H-}HelpKeyword: string): Integer;
  protected
    function GetParams(Index: Integer): String; override;
    function GetParamCount: Integer; override;

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

    // IDE
    function BuildLazarusIDE: boolean;
    function CompileAutoInstallPackages(Clean: boolean): boolean;

    function Init: boolean;
    procedure LoadEnvironmentOptions;
    procedure LoadMiscellaneousOptions;
    procedure SetupOutputFilter;
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
    procedure Error(ErrorCode: Byte; const ErrorMsg: string);
    function OnRunExternalTool(Tool: TIDEExternalToolOptions): TModalResult;

    property AddPackage: boolean read FAddPackage write FAddPackage; // add package to installed pacakge in IDE (UserIDE)
    property BuildAll: boolean read FBuildAll write FBuildAll;// build all files of project/package
    property BuildRecursive: boolean read FBuildRecursive // apply BuildAll flag to dependencies
                                     write FBuildRecursive;
    property SkipDependencies: boolean read FSkipDependencies
                                            write FSkipDependencies;
    property BuildIDE: boolean read FBuildIDE write FBuildIDE; // build IDE (as opposed to a project/package etc)
    property BuildIDEOptions: string read FBuildIDEOptions write FBuildIDEOptions;
    property CreateMakefile: boolean read FCreateMakefile write FCreateMakefile;
    property WidgetSetOverride: String read fWidgetsetOverride
                                            write fWidgetsetOverride;
    property OSOverride: String read fOSOverride write fOSOverride;
    property CPUOverride: String read fCPUOverride write fCPUOverride;
    property CompilerOverride: String read fCompilerOverride write fCompilerOverride;
    property LazarusDirOverride: String read fLazarusDirOverride write fLazarusDirOverride;
    property BuildModeOverride: String read FBuildModeOverride write FBuildModeOverride;
  end;

var
  Application: TLazBuildApplication = nil;

const
  ErrorFileNotFound = 1;
  ErrorBuildFailed = 2;
  ErrorLoadPackageFailed = 3;
  ErrorPackageNameInvalid = 4;
  ErrorLoadProjectFailed = 5;
  VersionStr = {$I version.inc};

procedure FilterConfigFileContent;
var
  l: TStrings;
  i: Integer;
begin
  ResetParamsAndCfg;
  l := GetCfgFileContent;
  if l = nil then exit;
  i := l.Count - 1;
  while i >= 0 do begin
    if not(
        (copy(l[i], 1, 22) = '--primary-config-path=') or
        (copy(l[i], 1, 24) = '--secondary-config-path=') or
        (copy(l[i], 1,  6) = '--pcp=') or
        (copy(l[i], 1,  6) = '--scp=')
       )
    then
      l.Delete(i);
    dec(i);
  end;
end;

Function ParamCount: Integer;
begin
  Result := GetParamsAndCfgFile.Count - 1;
end;

Function Paramstr(Param : Integer) : Ansistring;
begin
  if Param >= GetParamsAndCfgFile.Count then
    Result := ''
  else
    Result := GetParamsAndCfgFile[Param];
end;

Function ParamStrUTF8(Param : Integer) : Ansistring;
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
      Description:=Format(lisPkgMangPackage, [TLazPackage(DepOwner).IDAsString]
        );
    end else if DepOwner is TProject then begin
      Description:=Format(lisPkgMangProject, [ExtractFileNameOnly(TProject(
        DepOwner).ProjectInfoFile)]);
    end else begin
      Description:=dbgsName(DepOwner)
    end;
  end else begin
    Description:=Format(lisPkgMangDependencyWithoutOwner, [Dependency.AsString]
      );
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
      Directory:=TProject(DepOwner).ProjectDirectory;
    end else begin
      Directory:=''
    end;
  end else begin
    Directory:=''
  end;
end;

{ TLazBuildApplication }

procedure TLazBuildApplication.OnExtToolFreeOutputFilter(
  OutputFilter: TOutputFilter; ErrorOccurred: boolean);
begin
  if ErrorOccurred then Error(ErrorBuildFailed,'tool reported error');
end;

procedure TLazBuildApplication.OnExtToolNeedsOutputFilter(
  var OutputFilter: TOutputFilter; var Abort: boolean);
begin
  OutputFilter:=TheOutputFilter;
end;

procedure TLazBuildApplication.OnCodeBufferEncodeSaving(Code: TCodeBuffer;
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

procedure TLazBuildApplication.OnCodeBufferDecodeLoaded(Code: TCodeBuffer;
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
  if FileExists(Pkg.FileName) then PkgLinks.AddUserLink(Pkg);
end;

procedure TLazBuildApplication.OnProjectChangeInfoFile(TheProject: TProject);
begin
  if TheProject<>Project1 then exit;
  if TheProject.IsVirtual then
    CodeToolBoss.SetGlobalValue(ExternalMacroStart+'ProjPath',VirtualDirectory)
  else
    CodeToolBoss.SetGlobalValue(ExternalMacroStart+'ProjPath',
                                Project1.ProjectDirectory)
end;

procedure TLazBuildApplication.OnProjectGetTestDirectory(TheProject: TProject;
  out TestDir: string);
begin
  TestDir:=BuildBoss.GetTestBuildDirectory;
end;

function TLazBuildApplication.OnIDEMessageDialog(const aCaption, aMsg: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; const HelpKeyword: string
  ): Integer;
begin
  DumpStack;
  Error(ErrorBuildFailed, Format(lisLazbuildIsNonInteractiveAbortingNow, [
    aCaption, #13, aMsg, #13]));
  Result:=mrCancel;
end;

function TLazBuildApplication.OnIDEQuestionDialog(const aCaption, aMsg: string;
  DlgType: TMsgDlgType; Buttons: array of const; const HelpKeyword: string
  ): Integer;
begin
  DumpStack;
  Error(ErrorBuildFailed, Format(lisLazbuildIsNonInteractiveAbortingNow, [
    aCaption, #13, aMsg, #13]));
  Result:=mrCancel;
end;

function TLazBuildApplication.GetParams(Index: Integer): String;
begin
  if Index >= GetParamsAndCfgFile.Count then
    Result := ''
  else
    Result := GetParamsAndCfgFile[Index];
end;

function TLazBuildApplication.GetParamCount: Integer;
begin
  Result := GetParamsAndCfgFile.Count - 1;
end;

function TLazBuildApplication.BuildFile(Filename: string): boolean;
var
  OriginalFilename: string;
  Package: TPackageLink;
begin
  Result:=false;
  OriginalFilename:=FileName;
  Filename:=CleanAndExpandFilename(Filename);
  if not FileExistsUTF8(Filename) then
  begin
    // Check for packages if the specified name is a valid identifier
    if IsValidIdent(OriginalFileName) then begin
      // Initialize package graph with base packages etc:
      if not Init then exit;
      // Apparently not found, could be a known but not installed package
      // so try and get package filename from all other known packages
      Package:=PkgLinks.FindLinkWithPkgName(OriginalFileName);
      if Package=nil then begin
        // Not found after everything we tried
        Error(ErrorFileNotFound,'package not found: '+OriginalFilename);
      end
      else begin
        // We found a package link
        if AddPackage then begin
          // this is handled in AddPackagesToInstallList
          Result:=true;
        end
        else
          Result:=BuildPackage(Package.LPKFilename)
      end;
    end
    else begin
      // File is not an identifier and doesn't exist.
      Error(ErrorFileNotFound, 'package not found: '+OriginalFilename);
      Exit;
    end;
  end
  else begin
    // File exists:
    if CompareFileExt(Filename,'.lpk')=0 then
      if AddPackage then begin
        // this is handled in AddPackagesToInstallList
        Result:=true;
      end
      else
        Result:=BuildPackage(Filename)
    else if CompareFileExt(Filename,'.lpi')=0 then
        Result:=BuildProject(Filename)
    else if CompareFileExt(Filename,'.lpr')=0 then begin
      Filename:=ChangeFileExt(Filename,'.lpi');
      if FileExists(Filename) then
        Result:=BuildProject(Filename)
      else
        Error(ErrorFileNotFound,'file not found: '+Filename);
    end else
      Error(ErrorBuildFailed,'don''t know how to build: '+Filename);
  end;
end;

function TLazBuildApplication.BuildPackage(const AFilename: string): boolean;
var
  APackage: TLazPackage;
  Flags: TPkgCompileFlags;
begin
  Result:=false;
  
  if not Init then exit;

  APackage:=LoadPackage(AFilename);
  if APackage=nil then
    Error(ErrorLoadPackageFailed, 'unable to load package "'+AFilename+'"');
    
  Flags:=[];
  if BuildAll then
    Include(Flags,pcfCleanCompile)
  else
    Include(Flags,pcfOnlyIfNeeded);
  if BuildRecursive and BuildAll then
    Include(Flags,pcfCompileDependenciesClean);
  if SkipDependencies then
    Include(Flags,pcfDoNotCompileDependencies);

  if (Length(OSOverride) <> 0) then
    APackage.CompilerOptions.TargetOS:=OSOverride;
  if (Length(CPUOverride) <> 0) then
    APackage.CompilerOptions.TargetCPU:=CPUOverride;

  if CreateMakefile then
    DoCreateMakefile(APackage)
  else
    CompilePackage(APackage,Flags);
  
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
  if not FileExistsUTF8(AFilename) then
    Error(ErrorLoadPackageFailed,'Package file not found "'+AFilename+'"');

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
  if (Result.Name='') or (not IsValidIdent(Result.Name)) then begin
    Error(ErrorPackageNameInvalid,
          Format(lisPkgMangThePackageNameOfTheFileIsInvalid,
           ['"', Result.Name,'"', #13, '"', Result.Filename, '"']));
  end;
  // check if Package with same name is already loaded
  ConflictPkg:=PackageGraph.FindPackageWithName(Result.Name,nil);
  if ConflictPkg<>nil then begin
    // replace package
    PackageGraph.ReplacePackage(ConflictPkg,Result);
  end else begin
    // add to graph
    PackageGraph.AddPackage(Result);
  end;
  // save package file links
  PkgLinks.SaveUserLinks;
end;

function TLazBuildApplication.BuildLazarusIDE: boolean;
var
  Flags: TBuildLazarusFlags;
  CurResult: TModalResult;
  BuildLazProfiles: TBuildLazarusProfiles;
  CurProf: TBuildLazarusProfile;
  PkgOptions: String;
  InheritedOptionStrings: TInheritedCompOptsStrings;
  TargetDir: String;
  i: Integer;
  s: String;
  ProfileChanged: boolean;
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
      debugln(['ERROR: IDE build mode "'+BuildModeOverride+'" not found']);
      if ConsoleVerbosity>=-2 then begin
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
        debugln;
      end;
      Halt(ErrorBuildFailed);
    end;
    CurProf:=BuildLazProfiles[i];
    BuildLazProfiles.CurrentIndex:=i;
  end;
  if ConsoleVerbosity>=0 then
    debugln(['Building Lazarus IDE with profile "',CurProf.Name,'"']);

  if (Length(OSOverride) <> 0) then
    CurProf.TargetOS:=OSOverride;
  if (Length(CPUOverride) <> 0) then
    CurProf.TargetCPU:=CPUOverride;

  if WidgetSetOverride<>'' then
    CurProf.TargetPlatform:=DirNameToLCLPlatform(WidgetSetOverride)
  else
    CurProf.TargetPlatform:=GetDefaultLCLWidgetType;
  if BuildIDEOptions<>'' then
  begin
    s:=CurProf.ExtraOptions;
    if s<>'' then
      s+=' ';
    s+=BuildIDEOptions;
    CurProf.ExtraOptions:=s;
  end;
  if BuildAll then
    CurProf.IdeBuildMode:=bmCleanAllBuild;
  MainBuildBoss.SetBuildTargetIDE;
  Flags:=[];

  // try loading install packages
  PackageGraph.LoadAutoInstallPackages(BuildLazProfiles.StaticAutoInstallPackages);

  // save target directory
  TargetDir:=CurProf.TargetDirectory;
  IDEMacros.SubstituteMacros(TargetDir);
  if not ForceDirectory(TargetDir) then begin
    if ConsoleVerbosity>=-1 then
      DebugLn('WARNING: failed creating IDE target directory "',TargetDir,'" (TLazBuildApplication.BuildLazarusIDE)');
    exit;
  end;

  // clean
  ProfileChanged:=false;
  if BuildLazProfiles.Current.IdeBuildMode=bmCleanAllBuild then begin
    CurResult:=MakeLazarus(BuildLazProfiles.Current,
                EnvironmentOptions.ExternalTools,GlobalMacroList,
                '',EnvironmentOptions.GetParsedCompilerFilename,
                EnvironmentOptions.GetParsedMakeFilename,
                Flags+[blfDontBuild],
                ProfileChanged);
    if CurResult<>mrOk then begin
      if ConsoleVerbosity>=-1 then
        DebugLn('TLazBuildApplication.BuildLazarusIDE: Clean all failed.');
      exit;
    end;
  end;

  // save configs for 'make'
  CurResult:=PackageGraph.SaveAutoInstallConfig;
  if CurResult<>mrOk then begin
    if ConsoleVerbosity>=-1 then
      DebugLn('TLazBuildApplication.BuildLazarusIDE: failed saving IDE make config files.');
    exit;
  end;

  // compile auto install static packages
  if not CompileAutoInstallPackages(BuildLazProfiles.Current.IdeBuildMode<>bmBuild)
  then begin
    if ConsoleVerbosity>=-1 then
      DebugLn('TLazBuildApplication.BuildLazarusIDE: Compile AutoInstall Packages failed.');
    exit;
  end;
  
  // create inherited compiler options
  PkgOptions:=PackageGraph.GetIDEInstallPackageOptions(
           PackageGraph.FirstAutoInstallDependency,InheritedOptionStrings{%H-});

  // save
  CurResult:=SaveIDEMakeOptions(BuildLazProfiles.Current,GlobalMacroList,
                                PkgOptions,Flags+[blfBackupOldExe]);
  if CurResult<>mrOk then begin
    if ConsoleVerbosity>=-1 then
      DebugLn('TLazBuildApplication.BuildLazarusIDE: failed saving idemake.cfg');
    exit;
  end;

  // compile IDE
  CurResult:=MakeLazarus(BuildLazProfiles.Current,
                         EnvironmentOptions.ExternalTools,GlobalMacroList,
                         PkgOptions,EnvironmentOptions.GetParsedCompilerFilename,
                         EnvironmentOptions.GetParsedMakeFilename,
                         Flags+[blfUseMakeIDECfg,blfOnlyIDE],
                         ProfileChanged);
  if CurResult<>mrOk then begin
    if ConsoleVerbosity>=-1 then
      DebugLn('TLazBuildApplication.BuildLazarusIDE: Building IDE failed.');
    exit;
  end;

  Result:=true;
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
    Dependency:=PackageGraph.FirstAutoInstallDependency;
    while Dependency<>nil do begin
      OldDependency:=Dependency;
      Dependency:=Dependency.NextRequiresDependency;
      if OldDependency.LoadPackageResult<>lprSuccess then begin
        raise Exception.Create(Format(
            lisPkgMangThePackageIsMarkedForInstallationButCanNotBeFound, [
            '"', OldDependency.AsString, '"', #13]));
      end;
    end;

    // check consistency
    CheckPackageGraphForCompilation(nil,
                      PackageGraph.FirstAutoInstallDependency);

    // compile all auto install dependencies
    CompilePolicy:=pupAsNeeded;
    if (BuildRecursive and BuildAll) or Clean then
      CompilePolicy:=pupOnRebuildingAll;
    CurResult:=PackageGraph.CompileRequiredPackages(nil,
                   PackageGraph.FirstAutoInstallDependency,false,CompilePolicy);
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
    Error(ErrorBuildFailed,APackage.IDAsString+' lpk file missing');

  // check graph for circles and broken dependencies
  if not (pcfDoNotCompileDependencies in Flags) then begin
    CheckPackageGraphForCompilation(APackage,nil);
  end;

  if PackageGraph.CompilePackage(APackage,Flags,false)<>mrOk then
    Error(ErrorBuildFailed,APackage.IDAsString+' compilation failed');
end;

procedure TLazBuildApplication.DoCreateMakefile(APackage: TLazPackage);
begin
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
      Item:=TObject(PathList[0]);
      if Item is TPkgDependency then begin
        if Result<>'' then
          Result:=Result+'>';
        Result:=Result+TPkgDependency(Item).AsString;
      end else if Item is TProject then begin
        if Result<>'' then
          Result:=Result+'>';
        Result:=Result
                +'Project:'+ExtractFileNameOnly(TProject(Item).ProjectInfoFile);
      end else if Item is TLazPackage then begin
        if Result<>'' then
          Result:=Result+'>';
        Result:=Result+TLazPackage(Item).IDAsString;
      end else begin
        if Result<>'' then
          Result:=Result+'>';
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
      Error(ErrorLoadPackageFailed,'Broken dependency: '+PathListToString(PathList));

    // check for circle dependencies
    PathList:=PackageGraph.FindCycleDependencyPath(APackage,FirstDependency);
    if PathList<>nil then
      Error(ErrorLoadPackageFailed,'Circle dependency: '+PathListToString(PathList));
  finally
    PathList.Free;
  end;
end;

function TLazBuildApplication.BuildProject(const AFilename: string): boolean;
var
  CompilerFilename: String;
  WorkingDir: String;
  SrcFilename: String;
  CompilerParams: String;
  ToolBefore: TProjectCompilationToolOptions;
  ToolAfter: TProjectCompilationToolOptions;
  UnitOutputDirectory: String;
  TargetExeName: String;
  TargetExeDir: String;
  NewBuildMode: TProjectBuildMode;
  CompilePolicy: TPackageUpdatePolicy;
  i: Integer;
  Note: String;
  NeedBuildAllFlag: Boolean;
  SubResult: TModalResult;
  MatrixOption: TBuildMatrixOption;
begin
  Result:=false;
  CloseProject(Project1);

  if not Init then exit;

  Project1:=LoadProject(AFilename);
  
  if Project1.MainUnitInfo=nil then
    Error(ErrorBuildFailed,'project has no main unit');

  // first override build mode
  if (BuildModeOverride<>'') then begin
    NewBuildMode:=Project1.BuildModes.Find(BuildModeOverride);
    if NewBuildMode=nil then
    begin
      debugln([Format(lisERRORInvalidBuildMode, [BuildModeOverride])]);
      if ConsoleVerbosity>=0 then begin
        debugln;
        if Project1.BuildModes.Count>1 then
        begin
          debugln(lisAvailableProjectBuildModes);
          for i:=0 to Project1.BuildModes.Count-1 do
          begin
            if Project1.BuildModes[i]=Project1.ActiveBuildMode then
              dbgout('* ')
            else
              dbgout('  ');
            debugln(Project1.BuildModes[i].Identifier);
          end;
        end else begin
          debugln(lisThisProjectHasOnlyTheDefaultBuildMode);
        end;
        debugln;
      end;
      Halt(ErrorBuildFailed);
    end;
    Project1.ActiveBuildMode:=NewBuildMode;
  end;
  // then override specific options
  if (OSOverride<>'') then
    Project1.CompilerOptions.TargetOS:=OSOverride;
  if (CPUOverride<>'') then
    Project1.CompilerOptions.TargetCPU:=CPUOverride;
  if (WidgetSetOverride<>'') then begin
    MatrixOption:=Project1.BuildModes.SessionMatrixOptions.Add(bmotIDEMacro);
    MatrixOption.Modes:=Project1.ActiveBuildMode.Identifier;
    MatrixOption.MacroName:='LCLWidgetType';
    MatrixOption.Value:=WidgetSetOverride;
  end;
  // apply options
  MainBuildBoss.SetBuildTargetProject1(true,smsfsSkip);

  if not SkipDependencies then begin
    // compile required packages
    CheckPackageGraphForCompilation(nil,Project1.FirstRequiredDependency);

    PackageGraph.BeginUpdate(false);
    try
      // automatically compile required packages
      CompilePolicy:=pupAsNeeded;
      if BuildRecursive and BuildAll then
        CompilePolicy:=pupOnRebuildingAll;
      if PackageGraph.CompileRequiredPackages(nil,
                                Project1.FirstRequiredDependency,
                                not (pfUseDesignTimePackages in Project1.Flags),
                                CompilePolicy)<>mrOk
      then
        Error(ErrorBuildFailed,'Project dependencies of '+AFilename);
    finally
      PackageGraph.EndUpdate;
    end;
  end;
  
  WorkingDir:=Project1.ProjectDirectory;
  SrcFilename:=CreateRelativePath(Project1.MainUnitInfo.Filename,WorkingDir);

  // create unit output directory
  UnitOutputDirectory:=Project1.CompilerOptions.GetUnitOutPath(false);
  if not ForceDirectory(UnitOutputDirectory) then
    Error(ErrorBuildFailed,'Unable to create project unit output directory '+UnitOutputDirectory);

  // create target output directory
  TargetExeName := Project1.CompilerOptions.CreateTargetFilename(Project1.MainFilename);
  TargetExeDir := ExtractFilePath(TargetExeName);
  if not ForceDirectory(TargetExeDir) then
    Error(ErrorBuildFailed,'Unable to create project target directory '+TargetExeDir);

  // update all lrs files
  MainBuildBoss.UpdateProjectAutomaticFiles('');

  // create application bundle
  if Project1.UseAppBundle and (Project1.MainUnitID>=0)
  and (MainBuildBoss.GetLCLWidgetType=LCLPlatformDirNames[lpCarbon])
  then begin
    if not (CreateApplicationBundle(TargetExeName, Project1.Title) in [mrOk,mrIgnore]) then
      Error(ErrorBuildFailed,'Unable to create application bundle for '+TargetExeName);
    if not (CreateAppBundleSymbolicLink(TargetExeName) in [mrOk,mrIgnore]) then
      Error(ErrorBuildFailed,'Unable to create application bundle symbolic link for '+TargetExeName);
  end;

  // regenerate resources
  if not Project1.ProjResources.Regenerate(SrcFileName, False, True, '') then
  begin
    if ConsoleVerbosity>=-1 then
      DebugLn('TLazBuildApplication.BuildProject Project1.Resources.Regenerate failed');
  end;

  // get compiler parameters
  if CompilerOverride <> '' then
    CompilerFilename := CompilerOverride
  else
    CompilerFilename:=Project1.GetCompilerFilename;
  //DebugLn(['TLazBuildApplication.BuildProject CompilerFilename="',CompilerFilename,'" CompilerPath="',Project1.CompilerOptions.CompilerPath,'"']);
  // Note: use absolute paths, same as TBuildManager.DoCheckIfProjectNeedsCompilation
  CompilerParams:=Project1.CompilerOptions.MakeOptionsString(SrcFilename,[ccloAbsolutePaths])
                                         +' '+PrepareCmdLineOption(SrcFilename);

  NeedBuildAllFlag:=false;
  if (crCompile in Project1.CompilerOptions.CompileReasons) then begin
    // check if project is already uptodate
    Note:='';
    SubResult:=MainBuildBoss.DoCheckIfProjectNeedsCompilation(Project1,
                                                         NeedBuildAllFlag,Note);
    if (not BuildAll)
    and (not (pfAlwaysBuild in Project1.Flags)) then begin
      if SubResult=mrNo then begin
        if ConsoleVerbosity>=0 then
          debugln(['TLazBuildApplication.BuildProject MainBuildBoss.DoCheckIfProjectNeedsCompilation nothing to be done']);
        exit(true);
      end;
      if SubResult<>mrYes then
      begin
        if ConsoleVerbosity>=0 then
          debugln(['TLazBuildApplication.BuildProject MainBuildBoss.DoCheckIfProjectNeedsCompilation failed']);
        exit(false);
      end;
    end;
  end;

  // execute compilation tool 'Before'
  ToolBefore:=TProjectCompilationToolOptions(
                                    Project1.CompilerOptions.ExecuteBefore);
  if (crCompile in ToolBefore.CompileReasons) then begin
    if ToolBefore.Execute(
                     Project1.ProjectDirectory,lisExecutingCommandBefore)<>mrOk
    then
      Error(ErrorBuildFailed,'failed "tool before" of project '+AFilename);
  end;

  if (crCompile in Project1.CompilerOptions.CompileReasons) then begin
    // compile
    // write state file to avoid building clean every time
    if Project1.SaveStateFile(CompilerFilename,CompilerParams,false)<>mrOk then
      Error(ErrorBuildFailed,'failed saving statefile of project '+AFilename);
    if TheCompiler.Compile(Project1,
                            WorkingDir,CompilerFilename,CompilerParams,
                            BuildAll or NeedBuildAllFlag,false,false)<>mrOk
    then
      Error(ErrorBuildFailed,'failed compiling of project '+AFilename);
    // compilation succeded -> write state file
    if Project1.SaveStateFile(CompilerFilename,CompilerParams,true)<>mrOk then
      Error(ErrorBuildFailed,'failed saving statefile of project '+AFilename);
  end;

  // execute compilation tool 'After'
  ToolAfter:=TProjectCompilationToolOptions(
                                     Project1.CompilerOptions.ExecuteAfter);
  if (crCompile in ToolAfter.CompileReasons) then begin
    if ToolAfter.Execute(
                      Project1.ProjectDirectory,lisExecutingCommandAfter)<>mrOk
    then
      Error(ErrorBuildFailed,'failed "tool after" of project '+AFilename);
  end;

  // no need to check for mrOk, we are exit if it wasn't
  Result:=true;
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
      Result:=nil;
    end;
    Result.EndUpdate;

    Result.MainProject:=true;
    Result.OnFileBackup:=@BuildBoss.BackupFile;
    Result.OnGetTestDirectory:=@OnProjectGetTestDirectory;
    Result.OnChangeProjectInfoFile:=@OnProjectChangeInfoFile;

  finally
    ProjectDesc.Free;
  end;

  Result.BeginUpdate(true);
  try
    // read project info file
    if Result.ReadProject(AFilename,EnvironmentOptions.BuildMatrixOptions)<>mrOk then
      Error(ErrorLoadProjectFailed,'Project '+AFilename);
    //BuildBoss.RescanCompilerDefines(true);

    // load required packages
    PackageGraph.OpenRequiredDependencyList(Result.FirstRequiredDependency);

    //Result.DefineTemplates.AllChanged;
    //Result.DefineTemplates.Active:=true;
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
  PackageLink: TPackageLink;
  PackageName:string;
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
    if CompareFileExt(PackageNamesOrFiles[i],'.lpk')=0 then
      PkgFilename:=PackageNamesOrFiles[i]
    else if IsValidIdent(PackageNamesOrFiles[i]) then begin
      PackageLink:=PkgLinks.FindLinkWithPkgName(PackageNamesOrFiles[i]);
      if PackageLink=nil then
      begin
        ErrorMsg+='Can not find package '+PackageNamesOrFiles[i]+', so it is not marked for installation.'+LineEnding;
        continue;
      end;
      PkgFilename:=PackageLink.LPKFilename;
    end else begin
      ErrorMsg+=PackageNamesOrFiles[i]+' is not a package, so it is not marked for installation.'+LineEnding;
      continue;
    end;
    Package:=LoadPackage(PkgFilename);
    if Package=nil then
    begin
      ErrorMsg+='Could not load '+PackageNamesOrFiles[i]+', so it is not marked for installation.'+LineEnding;
      ErrCode:=ErrorLoadPackageFailed;
      continue;
    end;
    if Package.PackageType in [lptRunTime,lptRunTimeOnly] then
    begin
      ErrorMsg+='Package '+PackageNamesOrFiles[i]+' is only for runtime.'+LineEnding;
      continue;
    end;
    PackageName:=Package.Name;
    // set it as (static) autoinstall: select for installation
    if ConsoleVerbosity>=0 then
      debugln(['adding package "'+PkgFilename+'" to install list of IDE']);
    if MiscellaneousOptions.BuildLazProfiles.StaticAutoInstallPackages.IndexOf(PackageName)<0 then
      MiscellaneousOptions.BuildLazProfiles.StaticAutoInstallPackages.Add(PackageName);
  end;
  if ErrorMsg<>'' then begin
    ErrorMsg:=UTF8Trim(ErrorMsg);
    Error(ErrCode,ErrorMsg);
    exit;
  end;
  // save list
  MiscellaneousOptions.Save;

  Result:=true;
end;

function TLazBuildApplication.Init: boolean;
begin
  if fInitialized then exit(fInitResult);
  fInitResult:=false;
  fInitialized:=true;

  if ConsoleVerbosity>=0 then
    debugln(['primary config path: ',GetPrimaryConfigPath]);
  CreatePrimaryConfigPath;

  MainBuildBoss:=TBuildManager.Create(nil);
  MainBuildBoss.HasGUI:=false;
  SetupMacros;
  LoadEnvironmentOptions;
  if Terminated then exit(false);
  LoadMiscellaneousOptions;
  SetupLazarusDirectory;
  SetupCodetools;
  SetupCompilerFilename;
  SetupPackageSystem;
  SetupOutputFilter;
  MainBuildBoss.SetupCompilerInterface;

  StoreBaseSettings;

  // load static base packages
  PackageGraph.LoadStaticBasePackages;

  MainBuildBoss.SetBuildTarget(OSOverride,CPUOverride,WidgetSetOverride,smsfsSkip,true);

  fInitResult:=true;
  Result:=fInitResult;
end;

procedure TLazBuildApplication.LoadEnvironmentOptions;
var
  Note: string;
begin
  with EnvironmentOptions do begin
    CreateConfig;
    Load(false);
    fCompilerInCfg:=CompilerFilename;
    fLazarusDirInCfg:=LazarusDirectory;

    if Application.HasOption('language') then begin
      if ConsoleVerbosity>=0 then
        debugln('Note: overriding language with command line: ',
          Application.GetOptionValue('language'));
      EnvironmentOptions.LanguageID:=Application.GetOptionValue('language');
    end;
    TranslateResourceStrings(EnvironmentOptions.GetParsedLazarusDirectory,
                             EnvironmentOptions.LanguageID);
    TExternalToolList(ExternalTools).OnNeedsOutputFilter:=@OnExtToolNeedsOutputFilter;
    TExternalToolList(ExternalTools).OnFreeOutputFilter:=@OnExtToolFreeOutputFilter;
    if CompilerOverride<>'' then
      CompilerFilename:=CompilerOverride;
    //debugln(['TLazBuildApplication.LoadEnvironmentOptions LazarusDirectory="',LazarusDirectory,'"']);
    if LazarusDirOverride<>'' then
      LazarusDirectory:=CleanAndExpandDirectory(LazarusDirOverride);
  end;
  if not FileExistsUTF8(EnvironmentOptions.GetParsedLazarusDirectory
    +SetDirSeparators('packager/registration/fcl.lpk'))
  then begin
    CheckLazarusDirectoryQuality(EnvironmentOptions.GetParsedLazarusDirectory,Note);
    if ConsoleVerbosity>=-1 then
      debugln(['Error: invalid Lazarus directory "'+EnvironmentOptions.LazarusDirectory+'": '+Note]);
    Terminate;
  end;
end;

procedure TLazBuildApplication.LoadMiscellaneousOptions;
begin
  if MiscellaneousOptions<>nil then exit;
  MiscellaneousOptions:=TMiscellaneousOptions.Create;
  MiscellaneousOptions.Load;
end;

procedure TLazBuildApplication.SetupOutputFilter;
begin
  TheOutputFilter:=TOutputFilter.Create;
  TheOutputFilter.OnGetIncludePath:=@CodeToolBoss.GetIncludePathForDirectory;
end;

procedure TLazBuildApplication.SetupMacros;
begin
  MainBuildBoss.SetupTransferMacros;
end;

procedure TLazBuildApplication.SetupCodetools;
begin
  // create a test unit needed to get from the compiler all macros and search paths
  CodeToolBoss.FPCDefinesCache.TestFilename:=CreateCompilerTestPascalFilename;
  CodeToolBoss.SourceCache.OnEncodeSaving:=@OnCodeBufferEncodeSaving;
  CodeToolBoss.SourceCache.OnDecodeLoaded:=@OnCodeBufferDecodeLoaded;
  CodeToolBoss.SourceCache.DefaultEncoding:=EncodingUTF8;

  MainBuildBoss.LoadFPCDefinesCaches;
  // create a test unit needed to get from the compiler all macros and search paths
  CodeToolBoss.FPCDefinesCache.TestFilename:=CreateCompilerTestPascalFilename;
end;

procedure TLazBuildApplication.SetupPackageSystem;
begin
  OnGetDependencyOwnerDescription:=@GetDependencyOwnerDescription;
  OnGetDependencyOwnerDirectory:=@GetDependencyOwnerDirectory;

  // package links
  PkgLinks:=TPackageLinks.Create;
  PkgLinks.UpdateAll;

  // package graph
  PackageGraph:=TLazPackageGraph.Create;
  PackageGraph.OnAddPackage:=@PackageGraphAddPackage;
end;

procedure TLazBuildApplication.SetupDialogs;
begin
  IDEMessageDialog:=@OnIDEMessageDialog;
  IDEQuestionDialog:=@OnIDEQuestionDialog;
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
    if ConsoleVerbosity>=-1 then
    begin
      dbgout('storing');
      if StoreLazDir then
        dbgout(' Lazarus directory "',EnvironmentOptions.LazarusDirectory,'"');
      if StoreCompPath then
        dbgout(' Compiler path "',EnvironmentOptions.CompilerFilename,'"');
      debugln(' in "',EnvironmentOptions.Filename,'"');
    end;
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
  end;
end;

function TLazBuildApplication.RepairedCheckOptions(const ShortOptions: String;
  const Longopts: TStrings; Opts, NonOpts: TStrings): String;

Var
  I,J,L,P : Integer;
  O,OV : String;
  HaveArg : Boolean;
  NeedArg: Boolean;

  Function FindLongOpt(S : String) : boolean;

  Var
    I : integer;

  begin
    If CaseSensitiveOptions then
      begin
      I:=LongOpts.Count-1;
      While (I>=0) and (LongOpts[i]<>S) do
        Dec(i);
      end
    else
      begin
      S:=UpperCase(S);
      I:=LongOpts.Count-1;
      While (I>=0) and (UpperCase(LongOpts[i])<>S) do
        Dec(i);
      end;
    Result:=(I<>-1);
  end;

begin
  Result:='';
  I:=1;
  While (I<=ParamCount) and (Result='') do
    begin
    O:=Paramstr(I);
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
          HaveArg:=(I<ParamCount) and (Length(ParamStr(I+1))>0)
                   and (ParamStr(I+1)[i]<>OptionChar);
          If HaveArg then
            OV:=Paramstr(I+1);
          If Not CaseSensitiveOptions then
            O:=LowerCase(O);
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
                if ConsoleVerbosity>=-1 then
                  debugln(['P ',P,' J ',J,' ',O[J],' ',l,' Havearg ',HaveArg]);
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
  TOutputFilterProcess:=TProcessUTF8;
  Files:=TStringList.Create;
  RunExternalTool := @OnRunExternalTool;
end;

destructor TLazBuildApplication.Destroy;
begin
  CloseProject(Project1);

  if Assigned(PackageGraph) then
  begin
    PackageGraph.FreeAutoInstallDependencies;
    FreeThenNil(PackageGraph);
  end;

  FreeThenNil(PkgLinks);
  FreeThenNil(TheCompiler);
  FreeThenNil(TheOutputFilter);
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
  for i:=0 to Files.Count-1 do begin
    if not BuildFile(Files[i]) then begin
      if ConsoleVerbosity>=-1 then
        debugln('Failed building ',Files[i]);
      ExitCode := ErrorBuildFailed;
      exit;
    end;
  end;

  // Add user-requested packages to IDE install list:
  if AddPackage then begin
    if not AddPackagesToInstallList(Files) then begin
      if ConsoleVerbosity>=-1 then
        debugln('Failed adding package(s) ',Files.Text);
      ExitCode := ErrorBuildFailed;
      exit;
    end;
  end;

  if BuildIDE then begin
    if not BuildLazarusIDE then begin
      if ConsoleVerbosity>=-1 then
        debugln('Failed building Lazarus IDE');
      ExitCode := ErrorBuildFailed;
      exit;
    end;
  end;
end;

function TLazBuildApplication.ParseParameters: boolean;
var
  Options: TStringList;
  NonOptions: TStringList;
  ErrorMsg: String;
  LongOptions: TStringList;
  i: Integer;
  p: String;
begin
  Result:=false;
  if (ParamCount<=0)
   or (CompareText(ParamStr(1),'--help')=0)
   or (CompareText(ParamStr(1),'-help')=0)
   or (CompareText(ParamStr(1),'-?')=0)
   or (CompareText(ParamStr(1),'-h')=0)
  then begin
    WriteUsage;
    exit;
  end;
  if HasOption('h','help') or HasOption('?') then begin
    WriteUsage;
    exit;
  end;
  if HasOption('v','version') then begin
    writeln(VersionStr);
    exit;
  end;

  // ConsoleVerbosity
  for i:=1 to ParamCount do begin
    p:=ParamStr(i);
    if p='--verbose' then
      inc(ConsoleVerbosity)
    else if (p='-q') or (p='--quiet') then
      dec(ConsoleVerbosity);
  end;

  Options:=TStringList.Create;
  NonOptions:=TStringList.Create;
  LongOptions:=TStringList.Create;
  try
    LongOptions.Add('quiet');
    LongOptions.Add('verbose');
    LongOptions.Add('primary-config-path:');
    LongOptions.Add('pcp:');
    LongOptions.Add('secondary-config-path:');
    LongOptions.Add('scp:');
    LongOptions.Add('language:');
    LongOptions.Add('add-package');
    LongOptions.Add('build-all');
    LongOptions.Add('build-ide:');
    LongOptions.Add('recursive');
    LongOptions.Add('skip-dependencies');
    LongOptions.Add('widgetset:');
    LongOptions.Add('ws:');
    LongOptions.Add('operating-system:');
    LongOptions.Add('os:');
    LongOptions.Add('cpu:');
    LongOptions.Add('bm:');
    LongOptions.Add('build-mode:');
    LongOptions.Add('compiler:');
    LongOptions.Add('lazarusdir:');
    LongOptions.Add('create-makefile');
    ErrorMsg:=RepairedCheckOptions('lBrdq',LongOptions,Options,NonOptions);
    if ErrorMsg<>'' then begin
      writeln(ErrorMsg);
      writeln('');
      exit;
    end;

    // building IDE
    if HasOption('build-ide') then begin
      BuildIDE:=true;
      BuildIDEOptions:=GetOptionValue('build-ide');
    end;

    // Add package to list of to be installed packages)
    AddPackage:=HasOption('add-package');

    // files
    Files.Assign(NonOptions);
    if (Files.Count=0) and (not BuildIDE) then begin
      writeln('Error: missing file');
      WriteUsage;
      exit;
    end;

    // primary config path
    if HasOption('primary-config-path') then
      SetPrimaryConfigPath(GetOptionValue('primary-config-path'))
    else if HasOption('pcp') then
      SetPrimaryConfigPath(GetOptionValue('pcp'));

    // secondary config path
    if HasOption('secondary-config-path') then
      SetPrimaryConfigPath(GetOptionValue('secondary-config-path'))
    else if HasOption('scp') then
      SetSecondaryConfigPath(GetOptionValue('scp'));
      
    // build all
    if HasOption('B','build-all') then
      BuildAll:=true;
    if HasOption('r','recursive') then
      BuildRecursive:=true;
    if HasOption('d','skip-dependencies') then
      SkipDependencies:=true;

    // overides
    // widgetset
    if HasOption('ws') then
      WidgetSetOverride := GetOptionValue('ws')
    else if HasOption('widgetset') then
      WidgetSetOverride := GetOptionValue('widgetset');
      
    // operating system
    if HasOption('os') then
      OSOverride := GetOptionValue('os')
    else if HasOption('operating-system') then
      OSOverride := GetOptionValue('operating-system');

    // cpu
    if HasOption('cpu') then
      CPUOverride := GetOptionValue('cpu');

    // build mode
    if HasOption('bm') then
      BuildModeOverride := GetOptionValue('bm')
    else if HasOption('build-mode') then
      BuildModeOverride := GetOptionValue('build-mode');

    // compiler
    if HasOption('compiler') then
      CompilerOverride := GetOptionValue('compiler');

    if HasOption('lazarusdir') then
      LazarusDirOverride := GetOptionValue('lazarusdir');

    if HasOption('create-makefile') then
    begin
      CreateMakefile := true;
      if AddPackage then
        Error(ErrorPackageNameInvalid,'Can you combine --create-makefile and --add-package');
    end;
  finally
    Options.Free;
    NonOptions.Free;
    LongOptions.Free;
  end;
  Result:=true;
end;

procedure TLazBuildApplication.WriteUsage;
const
  space = '                      ';

  function LongToConsole(s: string): string;
  begin
    Result:=UTF8ToConsole(BreakString(s,75, length(space)))
  end;
begin
  TranslateResourceStrings(ProgramDirectory(true),'');
  writeln('');
  writeln('lazbuild [options] <project/package filename or package name>');
  writeln('');
  writeln(UTF8ToConsole(lisEdtExtToolParameters));
  writeln('');
  writeln('--help or -?              ', UTF8ToConsole(listhisHelpMessage));
  writeln('');
  writeln('-B or --build-all         ', UTF8ToConsole(lisBuildAllFilesOfProjectPackageIDE));
  writeln('-r or --recursive         ', UTF8ToConsole(lisApplyBuildFlagsBToDependenciesToo));
  writeln('-d or --skip-dependencies ', UTF8ToConsole(lisDoNotCompileDependencies));
  writeln('--build-ide=<options>     ', UTF8ToConsole(lisBuildIDEWithPackages));
  writeln('-v or --version           ', UTF8ToConsole(lisShowVersionAndExit));
  writeln('-q or --quiet             ', UTF8ToConsole('be less verbose, can be given multiple times'));
  writeln('--verbose                 ', UTF8ToConsole('be more verbose, can be given multiple times'));
  writeln('');

  writeln('--add-package');
  writeln(LongToConsole(space+lisAddPackageSToListOfInstalledPackagesCombineWithBui));
  writeln('--create-makefile');
  writeln(LongToConsole(space+lisInsteadOfCompilePackageCreateASimpleMakefile));
  writeln('');

  writeln(PrimaryConfPathOptLong,'<path>');
  writeln('or ',PrimaryConfPathOptShort,'<path>');
  writeln(LongToConsole(space+lisprimaryConfigDirectoryWhereLazarusStoresItsConfig+LazConf.GetPrimaryConfigPath));
  writeln('');
  writeln(SecondaryConfPathOptLong,'<path>');
  writeln('or ',SecondaryConfPathOptShort,'<path>');
  writeln(LongToConsole(space+lissecondaryConfigDirectoryWhereLazarusSearchesFor+LazConf.GetSecondaryConfigPath));
  writeln('');
  writeln('--operating-system=<operating-system>');
  writeln('or --os=<operating-system>');
  writeln(LongToConsole(Format(
    lisOverrideTheProjectOperatingSystemEGWin32LinuxDefau, [space,
    GetCompiledTargetOS])));
  writeln('');
  writeln('--widgetset=<widgetset>');
  writeln('or --ws=<widgetset>');
  writeln(LongToConsole(Format(
    lisOverrideTheProjectWidgetsetEGGtkGtk2QtWin32CarbonD, [space,
    LCLPlatformDirNames[LazConf.GetDefaultLCLWidgetType]])));
  writeln('');
  writeln('--cpu=<cpu>');
  writeln(LongToConsole(Format(
    lisOverrideTheProjectCpuEGI386X86_64PowerpcPowerpc_64, [space,
    GetCompiledTargetCPU])));
  writeln('');
  writeln('--build-mode=<project/ide build mode>');
  writeln('or --bm=<project/ide build mode>');
  writeln(LongToConsole(Format(lisOverrideTheProjectBuildMode,[space])));
  writeln('');
  writeln('--compiler=<ppcXXX>');
  writeln(LongToConsole(Format(
    lisOverrideTheDefaultCompilerEGPpc386Ppcx64PpcppcEtcD, [space])));
  writeln('');
  writeln(LanguageOpt);
  writeln(LongToConsole(space+lisOverrideLanguage));
  writeln('');
  writeln('--lazarusdir=<Lazarus directory>');
  writeln(LongToConsole(space+lisLazarusDirOverride));
  writeln('');
end;

procedure TLazBuildApplication.Error(ErrorCode: Byte; const ErrorMsg: string);
begin
  writeln('ERROR: ',LineBreaksToSystemLineBreaks(ErrorMsg));
  Halt(ErrorCode);
end;

function TLazBuildApplication.OnRunExternalTool(Tool: TIDEExternalToolOptions
  ): TModalResult;
begin
  Result:=EnvironmentOptions.ExternalTools.Run(Tool,GlobalMacroList,false);
end;

begin
  // When quick rebuilding lazbuild, FPC rebuilds only the lazbuild.lpr, so any
  // flag that should work with quick build must be set here.

  {$IFDEF BuildWidgetSetWin32}  Result:=lpWin32;  {$ENDIF}
  {$IFDEF BuildWidgetSetWinCE}  Result:=lpWinCE;  {$ENDIF}
  {$IFDEF BuildWidgetSetGTK}    Result:=lpGtk;    {$ENDIF}
  {$IFDEF BuildWidgetSetGTK2}   Result:=lpGtk2;   {$ENDIF}
  {$IFDEF BuildWidgetSetQT}     Result:=lpQT;     {$ENDIF}
  {$IFDEF BuildWidgetSetFPGui}  Result:=lpfpGUI;  {$ENDIF}
  {$IFDEF BuildWidgetSetCarbon} Result:=lpCarbon; {$ENDIF}
  {$IFDEF BuildWidgetSetCocoa}  Result:=lpCocoa;  {$ENDIF}
  {$IFDEF BuildWidgetSetNoGui}  Result:=lpNoGUI;  {$ENDIF}

  FilterConfigFileContent;
  // free LCL application
  FreeAndNil(Forms.Application);
  // start our own application
  Application:=TLazBuildApplication.Create(nil);
  Application.Run;
  Application.Free;
end.

