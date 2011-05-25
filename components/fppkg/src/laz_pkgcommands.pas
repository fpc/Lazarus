unit laz_pkgcommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,pkghandler,laz_pkghandler;

implementation

uses
  zipper,
  pkgmessages, pkgglobals, pkgoptions, pkgdownload, pkgrepos, fprepos,
  laz_pkgrepos, pkgfpmake, SvnCommand;

type
  { TCommandLazAddConfig }

  TCommandLazAddConfig = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandLazUpdate }

  TCommandLazUpdate = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandLazListPackages }

  TCommandLazListPackages = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandLazScanPackages }

  TCommandLazScanPackages = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandLazDownload }

  TCommandLazDownload = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandLazUnzip }

  TCommandLazUnzip = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandLazCompile }

  TCommandLazCompile = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandLazBuild }

  TCommandLazBuild = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandLazInstall }

  TCommandLazInstall = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandLazClean }

  TCommandLazClean = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandLazArchive }

  TCommandLazArchive = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandLazInstallDependencies }

  TCommandLazInstallDependencies = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandLazFixBroken }

  TCommandLazFixBroken = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandLazListSettings }

  TCommandLazListSettings = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

var
  DependenciesDepth: integer;

{ TCommandLazListSettings }

procedure TCommandLazListSettings.Execute;
begin
  GlobalOptions.LogValues(vlProgres);
  CompilerOptions.LogValues(vlProgres,'');
  FPMakeCompilerOptions.LogValues(vlProgres,'fpmake-building ');
end;


procedure TCommandLazAddConfig.Execute;
begin
{
  Log(vlInfo,SLogGeneratingCompilerConfig,[S]);
  Options.InitCompilerDefaults(Args[2]);
  Options.SaveCompilerToFile(S);
}
end;


procedure TCommandLazUpdate.Execute;
var
  PackagesURL :  String;
begin
  // Download and load mirrors.xml
  // This can be skipped when a custom RemoteRepository is configured
  if (GlobalOptions.RemoteMirrorsURL<>'') and
     (GlobalOptions.RemoteRepository='auto') then
    begin
      Log(vlCommands,SLogDownloading,[GlobalOptions.RemoteMirrorsURL,GlobalOptions.LocalMirrorsFile]);
      DownloadFile(GlobalOptions.RemoteMirrorsURL,GlobalOptions.LocalMirrorsFile);
      LoadLocalAvailableMirrors;
    end;
  // Download packages.xml
  PackagesURL:=GetRemoteRepositoryURL(PackagesFileName);
  Log(vlCommands,SLogDownloading,[PackagesURL,GlobalOptions.LocalPackagesFile]);
  DownloadFile(PackagesURL,GlobalOptions.LocalPackagesFile);
  // Read the repository again
  LoadLocalAvailableRepository;
  // no need to log errors again
  FindInstalledPackages(CompilerOptions,False);
end;


procedure TCommandLazListPackages.Execute;
begin
  Laz_ListPackages;
end;


procedure TCommandLazScanPackages.Execute;
begin
  { nothing, already handled in fppkg.pp as special case
    before the local fppkg directory is processed }
end;


procedure TCommandLazDownload.Execute;
var
  P : TFPPackage;
begin
  if PackageName='' then
  begin
    Error(SErrNoPackageSpecified);
    exit;
  end;
  P:=AvailableRepository.PackageByName(PackageName);
  if not FileExists(PackageLocalArchive(P)) then
    Laz_ExecuteAction(PackageName,'downloadpackage');
end;


procedure TCommandLazUnzip.Execute;
Var
  BuildDir : string;
  ArchiveFile : String;
  P : TFPPackage;
  fDownload: boolean = False;
  M: TFPMirror;
begin
  if PackageName='' then
    Error(SErrNoPackageSpecified);
  P:=AvailableRepository.PackageByName(PackageName);
  BuildDir:=PackageBuildPath(P);

  M := AvailableMirrors.FindMirror(P.Mirror);

  if not Assigned(M) or (M.Protocol <> 'SVN') then
  begin
    ArchiveFile:=PackageLocalArchive(P);
    if not FileExists(ArchiveFile) then
      Laz_ExecuteAction(PackageName,'downloadpackage');
    { Create builddir, remove it first if needed }
    if DirectoryExists(BuildDir) then
      DeleteDir(BuildDir);
    ForceDirectories(BuildDir);
    SetCurrentDir(BuildDir);
    { Unzip Archive }
    With TUnZipper.Create do
      try
        Log(vlCommands,SLogUnzippping,[ArchiveFile]);
        OutputPath:=PackageBuildPath(P);
        UnZipAllFiles(ArchiveFile);
      Finally
        Free;
      end;
  end
  else
    { Create builddir }
    if not DirectoryExists(BuildDir) then
      ExecuteSvnCommand('export ' + M.URL + P.FileName + ' "' + BuildDir + '"');
end;


procedure TCommandLazCompile.Execute;
begin
  if PackageName<>'' then
    begin
      // For local files we need the information inside the zip to get the
      // dependencies
      if (PackageName=CmdLinePackageName) then
        begin
          Laz_ExecuteAction(PackageName,'laz_unzip');
          Laz_ExecuteAction(PackageName,'laz_installdependencies');
        end
      else
        if (PackageName=CurrentDirPackageName) then
          begin
            Laz_ExecuteAction(PackageName,'laz_installdependencies');
          end
      else
        begin
          Laz_ExecuteAction(PackageName,'laz_installdependencies');
          Laz_ExecuteAction(PackageName,'laz_unzip');
        end;
    end;
  Laz_ExecuteAction(PackageName,'fpmakecompile');
end;


procedure TCommandLazBuild.Execute;
var
  P: TFPPackage;
begin
  if PackageName<>'' then
    begin
      // For local files we need the information inside the zip to get the
      // dependencies
      if (PackageName=CmdLinePackageName) then
        begin
          Laz_ExecuteAction(PackageName,'laz_unzip');
          Laz_ExecuteAction(PackageName,'laz_installdependencies');
        end
      else
        if (PackageName=CurrentDirPackageName) then
          begin
            Laz_ExecuteAction(PackageName,'laz_installdependencies');
          end
      else
        begin
          Laz_ExecuteAction(PackageName,'laz_installdependencies');
          // Check if the package is not installed but being recompiled because of changed
          // dependencies while the original source is still available.
          P := AvailableRepository.FindPackage(PackageName);
          if not (assigned(P) and P.RecompileBroken and (P.SourcePath<>'')) then
            // The package is not available locally, download and unzip it.
            Laz_ExecuteAction(PackageName,'laz_unzip');
        end;
    end;
  Laz_ExecuteAction(PackageName,'fpmakebuild');
end;


procedure TCommandLazInstall.Execute;
var
  UFN,S : String;
  P   : TFPPackage;
begin
  if PackageName<>'' then
    begin
      Laz_ExecuteAction(PackageName,'laz_build');
      Laz_ExecuteAction(PackageName,'fpmakeinstall');
      if (PackageName=CmdLinePackageName) or (PackageName=CurrentDirPackageName) then
        begin
          // Load package name from manifest
          if not FileExists(ManifestFileName) then
            Laz_ExecuteAction(PackageName,'fpmakemanifest');
          P:=LoadManifestFromFile(ManifestFileName);
          S:=P.Name;
          FreeAndNil(P);
        end
      else
        S:=PackageName;
      P:=InstalledRepository.FindPackage(S);
      if not assigned(P) then
        P:=InstalledRepository.AddPackage(S);
      if P.RecompileBroken then
        begin
          // If the package is recompiled, the installation-location is dependent on where
          // the package was installed originally.
          if P.InstalledLocally then
            UFN:=CompilerOptions.LocalUnitDir
          else
            UFN:=CompilerOptions.GlobalUnitDir;
          // Setting RecompileBroken to false is in a strict sense not needed. But it is better
          // to clean this temporary flag, to avoid problems with changes in the future
          P.RecompileBroken := false;
          AvailableRepository.FindPackage(P.Name).RecompileBroken:=false;
        end
      else
        begin
          if (IsSuperUser or GlobalOptions.InstallGlobal) then
            UFN:=CompilerOptions.GlobalUnitDir
          else
            UFN:=CompilerOptions.LocalUnitDir;
        end;
      UFN:=IncludeTrailingPathDelimiter(UFN)+S+PathDelim+UnitConfigFileName;
      LoadUnitConfigFromFile(P,UFN);
      if P.IsFPMakeAddIn then
        AddFPMakeAddIn(P);
    end
  else
    Laz_ExecuteAction(PackageName,'fpmakeinstall');
end;


procedure TCommandLazClean.Execute;
begin
  Laz_ExecuteAction(PackageName,'fpmakeclean');
end;


procedure TCommandLazArchive.Execute;
begin
  Laz_ExecuteAction(PackageName,'fpmakearchive');
end;


procedure TCommandLazInstallDependencies.Execute;
var
  i : Integer;
  MissingDependency,
  D : TFPDependency;
  P,
  InstalledP,
  AvailP : TFPPackage;
  L : TStringList;
  status : string;
  FreeManifest : boolean;
begin
  if PackageName='' then
    Error(SErrNoPackageSpecified);
  FreeManifest:=false;
  // Load dependencies for local packages
  if (PackageName=CmdLinePackageName) or (PackageName=CurrentDirPackageName) then
    begin
      Laz_ExecuteAction(PackageName,'fpmakemanifest');
      P:=LoadManifestFromFile(ManifestFileName);
      FreeManifest:=true;
    end
  else
    P:=AvailableRepository.PackageByName(PackageName);
  // Find and List dependencies
  MissingDependency:=nil;
  L:=TStringList.Create;
  for i:=0 to P.Dependencies.Count-1 do
    begin
      D:=P.Dependencies[i];
      if (CompilerOptions.CompilerOS in D.OSes) and
         (CompilerOptions.CompilerCPU in D.CPUs) then
        begin
          InstalledP:=InstalledRepository.FindPackage(D.PackageName);
          // Need installation?
          if not assigned(InstalledP) or
             (InstalledP.Version.CompareVersion(D.MinVersion)<0) then
            begin
              AvailP:=AvailableRepository.FindPackage(D.PackageName);
              if not assigned(AvailP) or
                 (AvailP.Version.CompareVersion(D.MinVersion)<0) then
                begin
                  status:='Not Available!';
                  MissingDependency:=D;
                end
              else
                begin
                  status:='Updating';
                  L.Add(D.PackageName);
                end;
            end
          else
            begin
              if PackageIsBroken(InstalledP, True) then
                begin
                  status:='Broken, recompiling';
                  L.Add(D.PackageName);
                end
              else
                status:='OK';
            end;
          Log(vlInfo,SLogPackageDependency,
              [D.PackageName,D.MinVersion.AsString,PackageInstalledVersionStr(D.PackageName),
               PackageAvailableVersionStr(D.PackageName),status]);
        end
      else
        Log(vlDebug,SDbgPackageDependencyOtherTarget,[D.PackageName,MakeTargetString(CompilerOptions.CompilerCPU,CompilerOptions.CompilerOS)]);
    end;
  // Give error on first missing dependency
  if assigned(MissingDependency) then
    Error(SErrNoPackageAvailable,[MissingDependency.PackageName,MissingDependency.MinVersion.AsString]);
  // Install needed updates
  if L.Count > 0 then
    begin
      if DependenciesDepth=0 then
        pkgglobals.Log(vlProgres,SProgrInstallDependencies);
      inc(DependenciesDepth);

      for i:=0 to L.Count-1 do
        Laz_ExecuteAction(L[i],'laz_install');

      dec(DependenciesDepth);
      if DependenciesDepth=0 then
        pkgglobals.Log(vlProgres,SProgrDependenciesInstalled);
    end;
  FreeAndNil(L);
  if FreeManifest then
    FreeAndNil(P);
end;


procedure TCommandLazFixBroken.Execute;
var
  i : integer;
  SL : TStringList;
begin
  SL:=TStringList.Create;
  repeat
    FindBrokenPackages(SL);
    if SL.Count=0 then
      break;
    pkgglobals.Log(vlProgres,SProgrReinstallDependent);
    for i:=0 to SL.Count-1 do
      begin
        Laz_ExecuteAction(SL[i],'laz_build');
        Laz_ExecuteAction(SL[i],'laz_install');
      end;
  until false;
  FreeAndNil(SL);
end;


initialization
  DependenciesDepth:=0;
  RegisterPkgHandler('laz_update',TCommandLazUpdate);
  RegisterPkgHandler('laz_list',TCommandLazListPackages);
  RegisterPkgHandler('laz_scan',TCommandLazScanPackages);
  RegisterPkgHandler('laz_download',TCommandLazDownload);
  RegisterPkgHandler('laz_unzip',TCommandLazUnzip);
  RegisterPkgHandler('laz_compile',TCommandLazCompile);
  RegisterPkgHandler('laz_build',TCommandLazBuild);
  RegisterPkgHandler('laz_install',TCommandLazInstall);
  RegisterPkgHandler('laz_clean',TCommandLazClean);
  RegisterPkgHandler('laz_archive',TCommandLazArchive);
  RegisterPkgHandler('laz_installdependencies',TCommandLazInstallDependencies);
  RegisterPkgHandler('laz_fixbroken',TCommandLazFixBroken);
  RegisterPkgHandler('laz_listsettings',TCommandLazListSettings);
end.
