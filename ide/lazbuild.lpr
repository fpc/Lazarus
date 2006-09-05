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
  
  !!! Under construction. !!!
  
  ToDo:
    Separate the visual parts in the IDE from the package and build system.
}
program lazbuild;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, LCLProc, Forms, Controls, FileUtil,
  // codetools
  CodeToolManager, Laz_XMLCfg,
  // IDEIntf
  MacroIntf, PackageIntf,
  // IDE
  IDEProcs, InitialSetupDlgs, OutputFilter, Compiler, CompilerOptions,
  TransferMacros, EnvironmentOpts, IDETranslations, LazarusIDEStrConsts,
  LazConf, BasePkgManager, PackageDefs, PackageLinks, PackageSystem,
  BuildManager, BaseBuildManager;
  
type

  { TLazBuildApplication }

  TLazBuildApplication = class(TCustomApplication)
  private
    FBuildAll: boolean;
    FBuildRecursive: boolean;
    fInitialized: boolean;
    fInitResult: boolean;
    TheOutputFilter: TOutputFilter;
    TheCompiler: TCompiler;
    // external tools
    procedure OnExtToolFreeOutputFilter(OutputFilter: TOutputFilter;
                                        ErrorOccurred: boolean);
    procedure OnExtToolNeedsOutputFilter(var OutputFilter: TOutputFilter;
                                         var Abort: boolean);

    // global package functions
    procedure GetDependencyOwnerDescription(Dependency: TPkgDependency;
                                            var Description: string);
    procedure GetDependencyOwnerDirectory(Dependency: TPkgDependency;
                                          var Directory: string);
    procedure GetWritablePkgOutputDirectory(APackage: TLazPackage;
                                            var AnOutDirectory: string);
    // package graph
    procedure PackageGraphAddPackage(Pkg: TLazPackage);
  protected
    function BuildFile(Filename: string): boolean;
    function BuildPackage(const AFilename: string): boolean;
    function LoadPackage(const AFilename: string): TLazPackage;
    procedure CompilePackage(APackage: TLazPackage; Flags: TPkgCompileFlags);
    procedure CheckPackageGraphForCompilation(APackage: TLazPackage;
                                 FirstDependency: TPkgDependency);
    function Init: boolean;
    procedure LoadEnvironmentOptions;
    procedure SetupOutputFilter;
    procedure SetupMacros;
    procedure SetupPackageSystem;
  public
    Files: TStringList;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run;
    function ParseParameters: boolean;
    procedure WriteUsage;
    procedure Error(ErrorCode: Byte; const ErrorMsg: string);
    property BuildAll: boolean read FBuildAll write FBuildAll;// build all files of project/package
    property BuildRecursive: boolean read FBuildRecursive // apply BuildAll flag to dependencies
                                     write FBuildRecursive;
  end;

const
  ErrorFileNotFound = 1;
  ErrorBuildFailed = 2;
  ErrorLoadPackageFailed = 3;
  ErrorPackageNameInvalid = 4;

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

procedure TLazBuildApplication.GetDependencyOwnerDescription(
  Dependency: TPkgDependency; var Description: string);
begin
  GetDescriptionOfDependencyOwner(Dependency,Description);
end;

procedure TLazBuildApplication.GetDependencyOwnerDirectory(
  Dependency: TPkgDependency; var Directory: string);
begin
  GetDirectoryOfDependencyOwner(Dependency,Directory);
end;

procedure TLazBuildApplication.GetWritablePkgOutputDirectory(
  APackage: TLazPackage; var AnOutDirectory: string);
var
  NewOutDir: String;
begin
  if DirectoryIsWritableCached(AnOutDirectory) then exit;

  ForceDirectory(AnOutDirectory);
  InvalidateFileStateCache;
  if DirectoryIsWritableCached(AnOutDirectory) then exit;
  //debugln('TPkgManager.GetWritablePkgOutputDirectory AnOutDirectory=',AnOutDirectory,' ',dbgs(DirectoryIsWritable(AnOutDirectory)));

  // output directory is not writable
  // -> redirect to config directory
  NewOutDir:=SetDirSeparators('/$(TargetCPU)-$(TargetOS)');
  IDEMacros.SubstituteMacros(NewOutDir);
  NewOutDir:=TrimFilename(GetPrimaryConfigPath+PathDelim+'lib'+PathDelim
                          +APackage.Name+NewOutDir);
  AnOutDirectory:=NewOutDir;
  //debugln('TPkgManager.GetWritablePkgOutputDirectory APackage=',APackage.IDAsString,' AnOutDirectory="',AnOutDirectory,'"');
end;

procedure TLazBuildApplication.PackageGraphAddPackage(Pkg: TLazPackage);
begin
  if FileExists(Pkg.FileName) then PkgLinks.AddUserLink(Pkg);
end;

function TLazBuildApplication.BuildFile(Filename: string): boolean;
begin
  Result:=false;
  Filename:=CleanAndExpandFilename(Filename);
  if not FileExists(Filename) then begin
    Error(ErrorFileNotFound, 'File not found: '+Filename);
    Exit;
  end;
  
  if CompareFileExt(Filename,'.lpk')=0 then
    Result:=BuildPackage(Filename);
end;

function TLazBuildApplication.BuildPackage(const AFilename: string): boolean;
var
  APackage: TLazPackage;
  Flags: TPkgCompileFlags;
begin
  Result:=false;
  Init;
  APackage:=LoadPackage(AFilename);
  if APackage=nil then
    Error(ErrorLoadPackageFailed, 'unable to load package "'+AFilename+'"');
  Flags:=[];
  if BuildAll then
    Include(Flags,pcfCleanCompile);
  if BuildRecursive and BuildAll then
    Include(Flags,pcfCompileDependenciesClean);
  CompilePackage(APackage,Flags);
end;

function TLazBuildApplication.LoadPackage(const AFilename: string): TLazPackage;
var
  XMLConfig: TXMLConfig;
  ConflictPkg: TLazPackage;
begin
  // check if package is already loaded
  Result:=PackageGraph.FindPackageWithFilename(AFilename,true);
  if (Result<>nil) then exit;
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
  ConflictPkg:=PackageGraph.FindAPackageWithName(Result.Name,nil);
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

procedure TLazBuildApplication.CompilePackage(APackage: TLazPackage;
  Flags: TPkgCompileFlags);
begin
  if APackage.AutoCreated then
    Error(ErrorBuildFailed,APackage.IDAsString+' is an auto created package');

  // check graph for circles and broken dependencies
  if not (pcfDoNotCompileDependencies in Flags) then begin
    CheckPackageGraphForCompilation(APackage,nil);
  end;

  {$NOTE TODO: move code from package manager to packagegraph and use it here}
end;

procedure TLazBuildApplication.CheckPackageGraphForCompilation(
  APackage: TLazPackage; FirstDependency: TPkgDependency);
  
  function PathListToString(PathList: TFPList): string;
  var
    Dependency: TPkgDependency;
    i: Integer;
  begin
    Result:='';
    for i:=0 to PathList.Count-1 do begin
      Dependency:=TPkgDependency(PathList[0]);
      if Dependency is TPkgDependency then begin
        if Result<>'' then
          Result:=Result+'>';
        Result:=Result+Dependency.AsString;
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
    PathList:=PackageGraph.FindCircleDependencyPath(APackage,FirstDependency);
    if PathList<>nil then
      Error(ErrorLoadPackageFailed,'Circle dependency: '+PathListToString(PathList));
  finally
    PathList.Free;
  end;
end;

function TLazBuildApplication.Init: boolean;
var
  InteractiveSetup: Boolean;
begin
  if fInitialized then exit(fInitResult);
  fInitResult:=false;
  fInitialized:=true;
  
  CreatePrimaryConfigPath;

  MainBuildBoss:=TBuildManager.Create;
  LoadEnvironmentOptions;
  InteractiveSetup:=false;
  SetupCompilerFilename(InteractiveSetup);
  SetupLazarusDirectory(InteractiveSetup);
  SetupMacros;
  SetupPackageSystem;
  SetupOutputFilter;
  MainBuildBoss.SetupCompilerInterface;

  fInitResult:=true;
end;

procedure TLazBuildApplication.LoadEnvironmentOptions;
begin
  EnvironmentOptions:=TEnvironmentOptions.Create;
  with EnvironmentOptions do begin
    SetLazarusDefaultFilename;
    Load(false);
    if Application.HasOption('language') then begin
      debugln('TLazBuildApplication.Init overriding language with command line: ',
        Application.GetOptionValue('language'));
      EnvironmentOptions.LanguageID:=Application.GetOptionValue('language');
    end;
    TranslateResourceStrings(EnvironmentOptions.LazarusDirectory,
                             EnvironmentOptions.LanguageID);
    ExternalTools.OnNeedsOutputFilter:=@OnExtToolNeedsOutputFilter;
    ExternalTools.OnFreeOutputFilter:=@OnExtToolFreeOutputFilter;
  end;
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

procedure TLazBuildApplication.SetupPackageSystem;
begin
  OnGetDependencyOwnerDescription:=@GetDependencyOwnerDescription;
  OnGetDependencyOwnerDirectory:=@GetDependencyOwnerDirectory;
  OnGetWritablePkgOutputDirectory:=@GetWritablePkgOutputDirectory;

  // package links
  PkgLinks:=TPackageLinks.Create;
  PkgLinks.UpdateAll;

  // package graph
  PackageGraph:=TLazPackageGraph.Create;
  PackageGraph.OnAddPackage:=@PackageGraphAddPackage;
end;

constructor TLazBuildApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Files:=TStringList.Create;
end;

destructor TLazBuildApplication.Destroy;
begin
  // free project, if it is still there
  //FreeThenNil(Project1);
  
  FreeThenNil(PackageGraph);
  FreeThenNil(PkgLinks);
  FreeThenNil(TheCompiler);
  FreeThenNil(TheOutputFilter);
  FreeThenNil(GlobalMacroList);
  FreeThenNil(IDEMacros);
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
  
  for i:=0 to Files.Count-1 do begin
    if not BuildFile(Files[i]) then begin
      writeln('Failed building ',Files[i]);
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
  Options:=TStringList.Create;
  NonOptions:=TStringList.Create;
  LongOptions:=TStringList.Create;
  try
    LongOptions.Add('primary-config-path');
    LongOptions.Add('pcp');
    LongOptions.Add('secondary-config-path');
    LongOptions.Add('scp');
    LongOptions.Add('language');
    LongOptions.Add('build-all');
    LongOptions.Add('recursive');
    ErrorMsg:=CheckOptions('lBR',LongOptions,Options,NonOptions);
    if ErrorMsg<>'' then begin
      writeln(ErrorMsg);
      writeln('');
      exit;
    end;

    // files
    Files.Assign(NonOptions);
    if Files.Count=0 then begin
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
    if HasOption('R','recursive') then
      BuildRecursive:=true;
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
begin
  TranslateResourceStrings(ProgramDirectory,'');
  writeln('');
  writeln('lazbuild [options] <project or package-filename>');
  writeln('');
  writeln('Options:');
  writeln('');
  writeln('--help or -?             ', listhisHelpMessage);
  writeln('');
  writeln('-B or --buildall         ','build all files of project/package');
  writeln('-R or --recursive        ','apply build flags (-B) to dependencies too.');
  writeln('');
  writeln(PrimaryConfPathOptLong,' <path>');
  writeln('or ',PrimaryConfPathOptShort,' <path>');
  writeln(BreakString(space+lisprimaryConfigDirectoryWhereLazarusStoresItsConfig,
                      75, 22), LazConf.GetPrimaryConfigPath);
  writeln('');
  writeln(SecondaryConfPathOptLong,' <path>');
  writeln('or ',SecondaryConfPathOptShort,' <path>');
  writeln(BreakString(space+lissecondaryConfigDirectoryWhereLazarusSearchesFor,
                      75, 22), LazConf.GetSecondaryConfigPath);
  writeln('');
  writeln(LanguageOpt);
  writeln(BreakString(space+lisOverrideLanguage,75, 22));
end;

procedure TLazBuildApplication.Error(ErrorCode: Byte; const ErrorMsg: string);
begin
  writeln('ERROR: ',ErrorMsg);
  Halt(ErrorCode);
end;

var
  Application: TLazBuildApplication;
begin
  Application:=TLazBuildApplication.Create(nil);
  Application.Run;
  Application.Free;
end.

