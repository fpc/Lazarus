unit FindProjPackUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazFileUtils,
  // Codetools
  DefineTemplates, CodeToolManager,
  // BuildIntf
  ProjectIntf, PackageIntf,
  // IdeConfig
  EnvironmentOpts;


function FindProjPackUnitFile(const AFilename: string; TheOwner: TObject;
  IgnoreUninstallPkgs: boolean = false): string;

implementation

function FindProjPackUnitFile(const AFilename: string; TheOwner: TObject;
  IgnoreUninstallPkgs: boolean): string;
// If TheOwner=nil, search in base IDE + installed packages.

  function FindInBaseIDE: string;
  var
    BaseDir, AnUnitName, UnitInFilename: String;
  begin
    AnUnitName:=ExtractFileNameOnly(AFilename);
    BaseDir:=EnvironmentOptions.GetParsedLazarusDirectory+PathDelim+'ide';
    UnitInFilename:='';
    Result:=CodeToolBoss.DirectoryCachePool.FindUnitSourceInCompletePath(
                                       BaseDir, AnUnitName,UnitInFilename, true);
  end;

  function FindInProject(AProject: TLazProject): string;
  var
    AnUnitInfo: TLazProjectFile; //TUnitInfo;
    BaseDir, AnUnitName, UnitInFilename: String;
  begin
    // search in virtual (unsaved) files
    AnUnitInfo:=AProject.UnitInfoWithFilename(AFilename,
                                     [pfsfOnlyProjectFiles,pfsfOnlyVirtualFiles]);
    if AnUnitInfo<>nil then
      exit(AnUnitInfo.Filename);
    // search in search path of project
    AnUnitName:=ExtractFileNameOnly(AFilename);
    BaseDir:=AProject.Directory;
    UnitInFilename:='';
    Result:=CodeToolBoss.DirectoryCachePool.FindUnitSourceInCompletePath(
                                       BaseDir,AnUnitName,UnitInFilename,true);
  end;

  function FindInPackage(APackage: TIDEPackage): string;
  var
    BaseDir, AnUnitName, UnitInFilename: String;
  begin
    Result:='';
    BaseDir:=APackage.Directory;
    if not FilenameIsAbsolute(BaseDir) then exit;
    // search in search path of package
    AnUnitName:=ExtractFileNameOnly(AFilename);
    UnitInFilename:='';
    Result:=CodeToolBoss.DirectoryCachePool.FindUnitSourceInCompletePath(
                                       BaseDir,AnUnitName,UnitInFilename,true);
  end;

var
  i: Integer;
begin
  if FilenameIsAbsolute(AFilename) then
    exit(AFilename);
  Result:='';
  // project
  if TheOwner is TLazProject then begin
    Result:=FindInProject(TLazProject(TheOwner));
    if Result<>'' then exit;
  end;
  // package
  if TheOwner is TIDEPackage then begin
    Result:=FindInPackage(TIDEPackage(TheOwner));
    if Result<>'' then exit;
  end;

  if TheOwner=nil then begin
    // search in base IDE
    Result:=FindInBaseIDE;
    if Result<>'' then exit;

    // search in installed packages
    for i:=0 to PackageGraphInterface.Count-1 do
      if (PackageGraphInterface.IdePkgs[i].Installed<>pitNope)
      and ( (not IgnoreUninstallPkgs) or (PackageGraphInterface.IdePkgs[i].AutoInstall<>pitNope) )
      then begin
        Result:=FindInPackage(PackageGraphInterface.IdePkgs[i]);
        if Result<>'' then exit;
      end;
    // search in auto install packages
    for i:=0 to PackageGraphInterface.Count-1 do
      if (PackageGraphInterface.IdePkgs[i].Installed=pitNope)
      and (PackageGraphInterface.IdePkgs[i].AutoInstall<>pitNope) then begin
        Result:=FindInPackage(PackageGraphInterface.IdePkgs[i]);
        if Result<>'' then exit;
      end;
    // then search in all other open packages
    for i:=0 to PackageGraphInterface.Count-1 do
      if (PackageGraphInterface.IdePkgs[i].Installed=pitNope)
      and (PackageGraphInterface.IdePkgs[i].AutoInstall=pitNope) then begin
        Result:=FindInPackage(PackageGraphInterface.IdePkgs[i]);
        if Result<>'' then exit;
      end;
  end;
  Result:='';
end;

end.

