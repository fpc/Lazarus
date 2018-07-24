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
  Classes, SysUtils, math, CustApp,
  Interfaces, // this includes the NoGUI widgetset
  // LazUtils
  Masks, LConvEncoding, FileUtil, LazFileUtils, LazLoggerBase, LazUtilities,
  LazUTF8, Laz2_XMLCfg, UITypes,
  // LCL
  LCLPlatformDef, Forms,
  // Codetools
  CodeCache, CodeToolManager, DefineTemplates, FileProcs,
  // IDEIntf
  BaseIDEIntf, MacroIntf, PackageIntf, LazMsgDialogs, ProjectIntf, IDEExternToolIntf,
  CompOptsIntf, IDEOptionsIntf, LazIDEIntf, PackageDependencyIntf,
  // IDE
  IDEProcs, InitialSetupProc, ExtTools, CompilerOptions,
  ApplicationBundle, TransferMacros, EnvironmentOpts, IDETranslations,
  LazarusIDEStrConsts, IDECmdLine, MiscOptions, Project, LazConf, PackageDefs,
  PackageLinks, PackageSystem, InterPkgConflictFiles, BuildLazDialog,
  BuildProfileManager, BuildManager, BaseBuildManager, ModeMatrixOpts;

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
    fWidgetsetOverride: String;

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
    function PackageGraphCheckInterPkgFiles(IDEObject: TObject;
                          PkgList: TFPList; out FilesChanged: boolean): boolean;

    // project
    procedure OnProjectChangeInfoFile(TheProject: TProject);

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
    procedure Error(ErrorCode: Byte; const ErrorMsg: string);

    property PackageAction: TPkgAction read FPackageAction write FPackageAction;
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
    property MaxProcessCount: integer read FMaxProcessCount write FMaxProcessCount;
    property NoWriteProject: boolean read FNoWriteProject write FNoWriteProject;
    property PkgGraphVerbosity: TPkgVerbosityFlags read FPkgGraphVerbosity write FPkgGraphVerbosity;
  end;

var
  LazBuildApp: TLazBuildApplication = nil;

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

{ TLazBuildApplication }

procedure TLazBuildApplication.OnCodeBufferEncodeSaving(Code: TCodeBuffer;
  const Filename: string; var Source: string);
begin
  if (Code.DiskEncoding<>'') and (Code.MemEncoding<>'')
  and (Code.DiskEncoding<>Code.MemEncoding) then begin
    {$IFDEF VerboseIDEEncoding}
    DebugLn(['TLazBuildApplication.OnCodeBufferEncodeSaving Filename=',Code.Filename,' Mem=',Code.MemEncoding,' to Disk=',Code.DiskEncoding]);
    {$ENDIF}
    Source:=ConvertEncoding(Source,Code.MemEncoding,Code.DiskEncoding);
    {$IF FPC_FULLVERSION >= 20701}
    //SetCodePage(Source,CP_ACP);
    {$ENDIF}
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
  if FileExistsUTF8(Pkg.FileName) then LazPackageLinks.AddUserLink(Pkg);
end;

function TLazBuildApplication.PackageGraphCheckInterPkgFiles(
  IDEObject: TObject; PkgList: TFPList; out FilesChanged: boolean): boolean;
begin
  Result:=CheckInterPkgFiles(IDEObject,PkgList,FilesChanged);
end;

procedure TLazBuildApplication.OnProjectChangeInfoFile(TheProject: TProject);
begin
  if TheProject<>Project1 then exit;
  if TheProject.IsVirtual then
    CodeToolBoss.SetGlobalValue(ExternalMacroStart+'ProjPath',VirtualDirectory)
  else
    CodeToolBoss.SetGlobalValue(ExternalMacroStart+'ProjPath',
                                Project1.Directory)
end;

function TLazBuildApplication.OnIDEMessageDialog(const aCaption, aMsg: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; const HelpKeyword: string
  ): Integer;
begin
  DumpStack;
  Error(ErrorBuildFailed, Format(lisLazbuildIsNonInteractiveAbortingNow, [
    aCaption, LineEnding, aMsg, LineEnding]));
  Result:=mrCancel;
end;

function TLazBuildApplication.OnIDEQuestionDialog(const aCaption, aMsg: string;
  DlgType: TMsgDlgType; Buttons: array of const; const HelpKeyword: string
  ): Integer;
begin
  DumpStack;
  Error(ErrorBuildFailed, Format(lisLazbuildIsNonInteractiveAbortingNow, [
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
    // Check for packages if the specified name is a valid identifier
    if LazIsValidIdent(OriginalFileName) then begin
      if PackageAction=lpaAddPkgLinks then begin
        Error(ErrorFileNotFound,'lpk file expected, but '+OriginalFilename+' found');
        Exit;
      end;

      // Initialize package graph with base packages etc:
      if not Init then exit;
      // Could be a known but not installed package
      // so try and get package filename from all other known packages
      Package:=TLazPackageLink(LazPackageLinks.FindLinkWithPkgName(OriginalFileName));
      if Package=nil then begin
        // Not found after everything we tried
        if CompareFileExt(Filename,'.lpi')=0 then
          Error(ErrorFileNotFound,'file not found: '+OriginalFilename)
        else
          Error(ErrorFileNotFound,'package not found: '+OriginalFilename);
      end
      else begin
        // We found a package link
        case PackageAction of
        lpaBuild: Result:=BuildPackage(Package.LPKFilename);
        lpaInstall: Result:=true; // this is handled in AddPackagesToInstallList
        end;
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
    if CompareFileExt(Filename,'.lpk')=0 then begin
      case PackageAction of
      lpaBuild: Result:=BuildPackage(Filename);
      lpaInstall: Result:=true; // this is handled in AddPackagesToInstallList
      lpaAddPkgLinks: Result:=true;
      end;
    end else if CompareFileExt(Filename,'.lpi')=0 then
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

  if ConsoleVerbosity>=0 then
    debugln(['Hint: (lazarus) compile package "',AFilename,'"']);

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
  if (Result.Name='') or (not LazIsValidIdent(Result.Name)) then begin
    Error(ErrorPackageNameInvalid,
          Format(lisPkgMangThePackageNameOfTheFileIsInvalid,
                 [Result.Name, LineEnding, Result.Filename]));
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
      debugln(['Error: (lazarus) IDE build mode "'+BuildModeOverride+'" not found']);
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
    debugln(['Hint: (lazarus) Building Lazarus IDE with profile "',CurProf.Name,'"']);

  if (Length(OSOverride) <> 0) then
    CurProf.TargetOS:=OSOverride;
  if (Length(CPUOverride) <> 0) then
    CurProf.TargetCPU:=CPUOverride;

  if WidgetSetOverride<>'' then
    CurProf.TargetPlatform:=DirNameToLCLPlatform(WidgetSetOverride)
  else
    CurProf.TargetPlatform:=GetBuildLCLWidgetType;
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

  // create target directory
  TargetDir:=CurProf.TargetDirectory;
  IDEMacros.SubstituteMacros(TargetDir);
  if not ForceDirectory(TargetDir) then begin
    if ConsoleVerbosity>=-1 then
      DebugLn('Warning: (lazarus) failed creating IDE target directory "',TargetDir,'" (TLazBuildApplication.BuildLazarusIDE)');
    exit;
  end;

  // clean
  Builder:=TLazarusBuilder.Create;
  try
    Builder.ProfileChanged:=false;

    if BuildLazProfiles.Current.IdeBuildMode=bmCleanAllBuild then begin
      Builder.PackageOptions:='';
      CurResult:=Builder.MakeLazarus(BuildLazProfiles.Current,
                  Flags+[blfDontBuild]);
      if CurResult<>mrOk then begin
        if ConsoleVerbosity>=-1 then
          DebugLn('Error: (lazarus) Building IDE: Clean all failed.');
        exit;
      end;
    end;

    // save list of install packages
    CurResult:=PackageGraph.SaveAutoInstallConfig;
    if CurResult<>mrOk then begin
      if ConsoleVerbosity>=-1 then
        DebugLn('Error: (lazarus) Building IDE: failed saving IDE make config files.');
      exit;
    end;

    // compile auto install static packages
    if not CompileAutoInstallPackages(BuildLazProfiles.Current.IdeBuildMode<>bmBuild)
    then begin
      if ConsoleVerbosity>=-1 then
        DebugLn('Error: (lazarus) Building IDE: Compile AutoInstall Packages failed.');
      exit;
    end;

    // create inherited compiler options
    Builder.PackageOptions:=PackageGraph.GetIDEInstallPackageOptions(InheritedOptionStrings{%H-});

    // save idemake.cfg
    CurResult:=Builder.SaveIDEMakeOptions(BuildLazProfiles.Current,Flags+[blfBackupOldExe]);
    if CurResult<>mrOk then begin
      if ConsoleVerbosity>=-1 then
        DebugLn('Error: (lazarus) Building IDE: failed saving idemake.cfg');
      exit;
    end;

    // compile IDE
    CurResult:=Builder.MakeLazarus(BuildLazProfiles.Current,
                           Flags+[blfUseMakeIDECfg,blfOnlyIDE]);
    if CurResult<>mrOk then begin
      if ConsoleVerbosity>=-1 then
        DebugLn('Error: (lazarus) Building IDE: Building IDE failed.');
      exit;
    end;

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
    Dependency:=PackageGraph.FirstAutoInstallDependency;
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
  if ConsoleVerbosity>0 then
    debugln(['Hint: (lazarus) [TLazBuildApplication.DoCreateMakefile] ',APackage.Filename]);
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
  CompilePolicy: TPackageUpdatePolicy;
  i,MatchCount: Integer;
  CompileHint: String;
  CompReason: TCompileReason;
  NeedBuildAllFlag: Boolean;
  MatrixOption: TBuildMatrixOption;
  ModeMask: TMask;
  CurResult: Boolean;

  function StartBuilding : boolean;
  begin
    Result := false;

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
                                  CompilePolicy)<>mrOk
        then
          Error(ErrorBuildFailed,'Project dependencies of '+AFilename);
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
          Error(ErrorBuildFailed,'failed "tool before" of project '+AFilename);
      end;

      // create unit output directory
      UnitOutputDirectory:=Project1.CompilerOptions.GetUnitOutPath(false);
      if not ForceDirectory(UnitOutputDirectory) then
        Error(ErrorBuildFailed,'Unable to create project unit output directory '+UnitOutputDirectory);

      // create target output directory
      TargetExeName := Project1.CompilerOptions.CreateTargetFilename;
      TargetExeDir := ExtractFilePath(TargetExeName);
      if not ForceDirectory(TargetExeDir) then
        Error(ErrorBuildFailed,'Unable to create project target directory '+TargetExeDir);

      // create LazBuildApp bundle
      if Project1.UseAppBundle and (Project1.MainUnitID>=0)
      and (MainBuildBoss.GetLCLWidgetType=LCLPlatformDirNames[lpCarbon])
      then begin
        if not (CreateApplicationBundle(TargetExeName, Project1.Title) in [mrOk,mrIgnore]) then
          Error(ErrorBuildFailed,'Unable to create application bundle for '+TargetExeName);
        if not (CreateAppBundleSymbolicLink(TargetExeName) in [mrOk,mrIgnore]) then
          Error(ErrorBuildFailed,'Unable to create application bundle symbolic link for '+TargetExeName);
      end;

      // update all lrs files
      MainBuildBoss.UpdateProjectAutomaticFiles('');

      // regenerate resources
      if not Project1.ProjResources.Regenerate(SrcFileName, False, True, '') then
      begin
        if ConsoleVerbosity>=-1 then
          DebugLn('Error: (lazarus) Project1.Resources.Regenerate failed of ',SrcFilename);
      end;

      // get compiler parameters
      if CompilerOverride <> '' then
        CompilerFilename := CompilerOverride
      else
        CompilerFilename:=Project1.GetCompilerFilename;
      //DebugLn(['TLazBuildApplication.BuildProject CompilerFilename="',CompilerFilename,'" CompilerPath="',Project1.CompilerOptions.CompilerPath,'"']);
      // CompileHint: use absolute paths, same as TBuildManager.DoCheckIfProjectNeedsCompilation
      CompilerParams:=Project1.CompilerOptions.MakeOptionsString([ccloAbsolutePaths])
                                             +' '+PrepareCmdLineOption(SrcFilename);

      if (CompReason in Project1.CompilerOptions.CompileReasons) then begin
        // compile

        // write state file to avoid building clean every time
        if Project1.SaveStateFile(CompilerFilename,CompilerParams,false)<>mrOk then
          Error(ErrorBuildFailed,'failed saving statefile of project '+AFilename);
        if TheCompiler.Compile(Project1,WorkingDir,CompilerFilename,CompilerParams,
                               BuildAll or NeedBuildAllFlag,false,false,false,
                               CompileHint)<>mrOk
        then
          Error(ErrorBuildFailed,'failed compiling of project '+AFilename);
        // compilation succeded -> write state file
        if Project1.SaveStateFile(CompilerFilename,CompilerParams,true)<>mrOk then
          Error(ErrorBuildFailed,'failed saving statefile of project '+AFilename);
      end;

      // execute compilation tool 'After'
      ToolAfter:=TProjectCompilationToolOptions(Project1.CompilerOptions.ExecuteAfter);
      if (CompReason in ToolAfter.CompileReasons) then begin
        if ToolAfter.Execute(Project1.Directory,
          lisProject2+lisExecutingCommandAfter,CompileHint)<>mrOk
        then
          Error(ErrorBuildFailed,'failed "tool after" of project '+AFilename);
      end;

      // no need to check for mrOk, we are exit if it wasn't
      Result:=true;
    finally
      if not SkipDependencies then
        PackageGraph.EndUpdate;
    end;
  end;

begin
  Result:=false;
  CloseProject(Project1);

  if not Init then exit;

  Project1:=LoadProject(AFilename);
  
  if Project1.MainUnitInfo=nil then
    Error(ErrorBuildFailed,'project has no main unit');

  if BuildAll then
    CompReason:= crBuild
  else
    CompReason:= crCompile;

  // first override build mode
  if (BuildModeOverride<>'') then
  begin
    CurResult := true;

    MatchCount := 0;
    ModeMask := TMask.Create(BuildModeOverride,false);
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
    begin
      debugln([Format(lisERRORInvalidBuildMode, [BuildModeOverride])]);
      if ConsoleVerbosity>=0 then
      begin
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

    Result := CurResult;
  end
  else
    Result := StartBuilding;

  // Auto increment build number
  if Result and not NoWriteProject
  and Project1.ProjResources.VersionInfo.UseVersionInfo
  and Project1.ProjResources.VersionInfo.AutoIncrementBuild
  then begin
    if FileIsWritable(AFilename) then
    begin
      Project1.ProjResources.DoAfterBuild(CompReason, Project1.IsVirtual);
      Project1.WriteProject(Project1.PublishOptions.WriteFlags,AFileName,EnvironmentOptions.BuildMatrixOptions)
    end
    else
    begin
      if ConsoleVerbosity>=-1 then
        DebugLn('Error: (lazarus) Project1.WriteProject skipped for read-only ',SrcFilename);
    end;
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
      Result:=nil;
    end;
    Result.EndUpdate;

    Result.MainProject:=true;
    Result.OnFileBackup:=@BuildBoss.BackupFileForWrite;
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
    if CompareFileExt(PackageNamesOrFiles[i],'.lpk')=0 then
      PkgFilename:=ExpandFileNameUTF8(PackageNamesOrFiles[i])
    else if LazIsValidIdent(PackageNamesOrFiles[i]) then begin
      PackageLink:=TLazPackageLink(LazPackageLinks.FindLinkWithPkgName(PackageNamesOrFiles[i]));
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
      debugln(['Hint: (lazarus) adding package "'+PkgFilename+'" to install list of IDE']);
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
    if CompareFileExt(PkgFilename,'.lpk')<>0 then begin
      ErrorMsg+=PkgFilename+' is not a package (.lpk), so it is not registered.'+LineEnding;
      continue;
    end;
    PkgFilename:=ExpandFileNameUTF8(PkgFilename);

    Package:=LoadPackage(PkgFilename);
    if Package=nil then
    begin
      ErrorMsg+='Could not load '+PkgFilename+', so it is not registered.'+LineEnding;
      continue;
    end;
    if ConsoleVerbosity>=0 then
      debugln(['Hint: (lazarus) registering package link "'+PkgFilename+'".']);
    LazPackageLinks.AddUserLink(Package);
  end;
  if ErrorMsg<>'' then begin
    ErrorMsg:=UTF8Trim(ErrorMsg);
    Error(ErrCode,ErrorMsg);
    exit;
  end;

  LazPackageLinks.SaveUserLinks(true);
  Result:=true;
end;

function TLazBuildApplication.Init: boolean;
begin
  if fInitialized then exit(fInitResult);
  fInitResult:=false;
  fInitialized:=true;

  if ConsoleVerbosity>=0 then
    debugln(['Hint: (lazarus) primary config path: ',GetPrimaryConfigPath]);
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
  MainBuildBoss.SetupExternalTools;
  ExtToolConsole:=TLazExtToolConsole.Create(nil);
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

    if LazBuildApp.HasOption('language') then begin
      if ConsoleVerbosity>=0 then
        debugln('Note: (lazarus) overriding language with command line: ',
          LazBuildApp.GetOptionValue('language'));
      EnvironmentOptions.LanguageID:=LazBuildApp.GetOptionValue('language');
    end;
    TranslateResourceStrings(EnvironmentOptions.GetParsedLazarusDirectory,
                             EnvironmentOptions.LanguageID);
    if CompilerOverride<>'' then
      CompilerFilename:=CompilerOverride;
    //debugln(['TLazBuildApplication.LoadEnvironmentOptions LazarusDirectory="',LazarusDirectory,'"']);
    if LazarusDirOverride<>'' then
      LazarusDirectory:=CleanAndExpandDirectory(LazarusDirOverride);
    if MaxProcessCount>=0 then
      // set command line override
      MaxExtToolsInParallel:=MaxProcessCount;
  end;
  if not FileExistsUTF8(EnvironmentOptions.GetParsedLazarusDirectory
    +GetForcedPathDelims('packager/registration/fcl.lpk'))
  then begin
    CheckLazarusDirectoryQuality(EnvironmentOptions.GetParsedLazarusDirectory,Note);
    if ConsoleVerbosity>=-1 then
      debugln(['Error: (lazarus) invalid Lazarus directory "'+EnvironmentOptions.LazarusDirectory+'": '+Note]);
    Terminate;
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
end;

procedure TLazBuildApplication.SetupCodetools;
begin
  // create a test unit needed to get from the compiler all macros and search paths
  CodeToolBoss.CompilerDefinesCache.TestFilename:=CreateCompilerTestPascalFilename;
  CodeToolBoss.SourceCache.OnEncodeSaving:=@OnCodeBufferEncodeSaving;
  CodeToolBoss.SourceCache.OnDecodeLoaded:=@OnCodeBufferDecodeLoaded;
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
  LazMessageDialog:=@OnIDEMessageDialog;
  LazQuestionDialog:=@OnIDEQuestionDialog;
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
      dbgout('Hint: (lazarus) storing');
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
    on E: Exception do
      debugln(['Error: (lazarus) unable to edit file ',EnvironmentOptions.Filename]);
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
                   and (ToolParamStr(I+1)[i]<>OptionChar);
          If HaveArg then
            OV:=ToolParamStr(I+1);
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
                if ConsoleVerbosity>0 then
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
  for i:=0 to Files.Count-1 do begin
    if not BuildFile(Files[i]) then begin
      if ConsoleVerbosity>=-1 then
        debugln('Error: (lazarus) Building failed: ',Files[i]);
      ExitCode := ErrorBuildFailed;
      exit;
    end;
  end;

  // Add user-requested packages to IDE install list:
  case PackageAction of
  lpaInstall:
    if not AddPackagesToInstallList(Files) then begin
      if ConsoleVerbosity>=-1 then
        debugln('Error: (lazarus) Installing package(s) failed: ',Files.Text);
      ExitCode := ErrorBuildFailed;
      exit;
    end;
  lpaAddPkgLinks:
    if not AddCmdLinePackageLinks(Files) then begin
      if ConsoleVerbosity>=-1 then
        debugln('Error: (lazarus) Adding package(s) links failed: ',Files.Text);
      ExitCode := ErrorBuildFailed;
      exit;
    end;
  end;

  if BuildIDE then begin
    if not BuildLazarusIDE then begin
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
  FilesNeeded: Boolean;
begin
  Result:=false;
  if (ToolParamCount<=0)
   or (CompareText(ToolParamStr(1),'--help')=0)
   or (CompareText(ToolParamStr(1),'-help')=0)
   or (CompareText(ToolParamStr(1),'-?')=0)
   or (CompareText(ToolParamStr(1),'-h')=0)
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
  for i:=1 to ToolParamCount do begin
    p:=ToolParamStr(i);
    if p='--verbose' then
      ConsoleVerbosity:=Max(1,ConsoleVerbosity+1)
    else if (p='-q') or (p='--quiet') then
      ConsoleVerbosity:=Min(0,ConsoleVerbosity-1);
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
    LongOptions.Add('max-process-count:');
    LongOptions.Add('no-write-project');
    ErrorMsg:=RepairedCheckOptions('lBrdq',LongOptions,Options,NonOptions);
    if ErrorMsg<>'' then begin
      writeln(ErrorMsg);
      writeln('');
      exit;
    end;

    FilesNeeded:=true;

    if HasOption('verbose-pkgsearch') then
      Include(fPkgGraphVerbosity,pvPkgSearch);

    // PackageAction: register lpk files
    if HasOption('add-package-link') then begin
      if ConsoleVerbosity>=0 then
        writeln('Parameter: add-package-link');
      if PackageAction<>lpaBuild then begin
        writeln('Error: invalid combination of package actions');
        WriteUsage;
        exit;
      end;
      FilesNeeded:=false;
      PackageAction:=lpaAddPkgLinks;
    end;

    // PackageAction: install lpk files
    if HasOption('add-package') then begin
      if ConsoleVerbosity>=0 then
        writeln('Parameter: add-package');
      if PackageAction<>lpaBuild then begin
        writeln('Error: invalid combination of package actions');
        WriteUsage;
        exit;
      end;
      PackageAction:=lpaInstall;
      FilesNeeded:=false;
    end;

    // building IDE
    if HasOption('build-ide') then begin
      BuildIDE:=true;
      BuildIDEOptions:=GetOptionValue('build-ide');
      FilesNeeded:=false;
      if ConsoleVerbosity>=0 then
        writeln('Parameter: build-ide=',BuildIDEOptions);
    end;

    // files
    Files.Assign(NonOptions);
    if FilesNeeded and (Files.Count=0) then begin
      writeln('Error: missing file');
      WriteUsage;
      exit;
    end;

    // primary config path
    if HasOption('primary-config-path') then begin
      SetPrimaryConfigPath(GetOptionValue('primary-config-path'));
      if ConsoleVerbosity>=0 then
        writeln('Parameter: primary-config-path=',GetPrimaryConfigPath);
    end else if HasOption('pcp') then begin
      SetPrimaryConfigPath(GetOptionValue('pcp'));
      if ConsoleVerbosity>=0 then
        writeln('Parameter: pcp=',GetPrimaryConfigPath);
    end;

    // secondary config path
    if HasOption('secondary-config-path') then begin
      SetPrimaryConfigPath(GetOptionValue('secondary-config-path'));
      if ConsoleVerbosity>=0 then
        writeln('Parameter: secondary-config-path=',GetSecondaryConfigPath);
    end else if HasOption('scp') then begin
      SetSecondaryConfigPath(GetOptionValue('scp'));
      if ConsoleVerbosity>=0 then
        writeln('Parameter: scp=',GetSecondaryConfigPath);
    end;

    // build all
    if HasOption('B','build-all') then begin
      BuildAll:=true;
      if ConsoleVerbosity>=0 then
        writeln('Parameter: build-all');
    end;
    if HasOption('r','recursive') then begin
      BuildAll:=true;
      BuildRecursive:=true;
      if ConsoleVerbosity>=0 then
        writeln('Parameter: recursive');
    end;
    if HasOption('d','skip-dependencies') then begin
      SkipDependencies:=true;
      if ConsoleVerbosity>=0 then
        writeln('Parameter: skip-dependencies');
    end;
    if BuildRecursive and SkipDependencies then begin
      writeln('Error: --recursive and --skip-dependencies. You have to make up your mind.');
      WriteUsage;
      exit;
    end;

    // overides
    // widgetset
    if HasOption('ws') then begin
      WidgetSetOverride := GetOptionValue('ws');
      if ConsoleVerbosity>=0 then
        writeln('Parameter: ws=',WidgetSetOverride);
    end else if HasOption('widgetset') then begin
      WidgetSetOverride := GetOptionValue('widgetset');
      if ConsoleVerbosity>=0 then
        writeln('Parameter: widgetset=',WidgetSetOverride);
    end;

    // operating system
    if HasOption('os') then begin
      OSOverride := GetOptionValue('os');
      if ConsoleVerbosity>=0 then
        writeln('Parameter: os=',OSOverride);
    end else if HasOption('operating-system') then begin
      OSOverride := GetOptionValue('operating-system');
      if ConsoleVerbosity>=0 then
        writeln('Parameter: operating-system=',OSOverride);
    end;

    // cpu
    if HasOption('cpu') then begin
      CPUOverride := GetOptionValue('cpu');
      if ConsoleVerbosity>=0 then
        writeln('Parameter: cpu=',CPUOverride);
    end;

    // build mode
    if HasOption('bm') then begin
      BuildModeOverride := GetOptionValue('bm');
      if ConsoleVerbosity>=0 then
        writeln('Parameter: bm=',BuildModeOverride);
    end else if HasOption('build-mode') then begin
      BuildModeOverride := GetOptionValue('build-mode');
      if ConsoleVerbosity>=0 then
        writeln('Parameter: build-mode=',BuildModeOverride);
    end;

    // compiler
    if HasOption('compiler') then begin
      CompilerOverride := GetOptionValue('compiler');
      if ConsoleVerbosity>=0 then
        writeln('Parameter: compiler=',CompilerOverride);
    end;

    // lazarusdir
    if HasOption('lazarusdir') then begin
      LazarusDirOverride := GetOptionValue('lazarusdir');
      if ConsoleVerbosity>=0 then
        writeln('Parameter: lazarusdir=',LazarusDirOverride);
    end;

    // max-process-count
    if HasOption('max-process-count') then begin
      MaxProcessCount:=StrToInt(GetOptionValue('max-process-count'));
      if ConsoleVerbosity>=0 then
        writeln('Parameter: max-process-count=',MaxProcessCount);
    end;

    if HasOption('no-write-project') then
    begin
      NoWriteProject := true;
      if ConsoleVerbosity>=0 then
        writeln('Parameter: no-write-project');
    end;

    if HasOption('create-makefile') then
    begin
      CreateMakefile := true;
      if ConsoleVerbosity>=0 then
        writeln('Parameter: create-makefile');
      if PackageAction<>lpaBuild then
        Error(ErrorPackageNameInvalid,'You can not combine --create-makefile and --add-package');
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

  procedure w(Msg: string);
  begin
    writeln(LongToConsole(Msg));
  end;

begin
  TranslateResourceStrings(ProgramDirectoryWithBundle,'');
  writeln('');
  writeln('lazbuild [options] <project/package filename or package name>');
  writeln('');
  w(lisEdtExtToolParameters);
  writeln('');
  writeln('--help or -?');
  w(space+listhisHelpMessage);
  writeln('-B or --build-all');
  w(space+lisBuildAllFilesOfProjectPackageIDE);
  writeln('-r or --recursive');
  w(space+lisApplyBuildFlagsBToDependenciesToo);
  writeln('-d or --skip-dependencies');
  w(space+lisDoNotCompileDependencies);
  writeln('--build-ide=<options>');
  w(space+lisBuildIDEWithPackages);
  writeln('-v or --version');
  w(space+lisShowVersionAndExit);
  writeln('-q or --quiet');
  w(space+lisBeLessVerboseCanBeGivenMultipleTimes);
  w(space+'Passing quiet two times, will pass -vw-n-h-i-l-d-u-t-p-c-x- to the compiler');
  writeln('--verbose');
  w(space+lisBeMoreVerboseCanBeGivenMultipleTimes);
  writeln('--verbose-pkgsearch');
  w(space+'Write what package files are searched and found');
  writeln('');

  writeln('--add-package');
  w(space+lisAddPackageSToListOfInstalledPackagesCombineWithBui);
  writeln('--add-package-link=<.lpk file>');
  w(space+lisOnlyRegisterTheLazarusPackageFilesLpkDoNotBuild);
  writeln('--create-makefile');
  w(space+lisInsteadOfCompilePackageCreateASimpleMakefile);
  writeln('');

  writeln(PrimaryConfPathOptLong,'<path>');
  writeln('or ',PrimaryConfPathOptShort,'<path>');
  w(space+lisprimaryConfigDirectoryWhereLazarusStoresItsConfig+LazConf.GetPrimaryConfigPath);
  writeln('');
  writeln(SecondaryConfPathOptLong,'<path>');
  writeln('or ',SecondaryConfPathOptShort,'<path>');
  w(space+lissecondaryConfigDirectoryWhereLazarusSearchesFor+LazConf.GetSecondaryConfigPath);
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
    LCLPlatformDirNames[GetBuildLCLWidgetType]])));
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
  w(space+lisOverrideLanguage);
  writeln('');
  writeln('--lazarusdir=<Lazarus directory>');
  w(space+lisLazarusDirOverride);
  writeln('');
  writeln('--max-process-count=<count>');
  w(space+lisMaximumNumberOfThreadsForCompilingInParallelDefaul);
  writeln('');
  writeln('--no-write-project');
  w(space+lisDoNotWriteUpdatedProjectInfoAfterBuild);
  writeln('');
end;

procedure TLazBuildApplication.Error(ErrorCode: Byte; const ErrorMsg: string);
begin
  writeln('Error: (lazbuild) ',LineBreaksToSystemLineBreaks(ErrorMsg));
  Halt(ErrorCode);
end;

begin
  // When quick rebuilding lazbuild, FPC rebuilds only the lazbuild.lpr, so any
  // flag that should work with quick build must be set here.
  // At the moment there is no flag

  HasGUI:=false;
  ConsoleVerbosity:=-1;
  FilterConfigFileContent;
  // free LCL Application to help debugging nogui issues
  Application.Free;
  // start our own LazBuildApp
  LazBuildApp:=TLazBuildApplication.Create(nil);
  LazBuildApp.Run;
  LazBuildApp.Free;
end.

