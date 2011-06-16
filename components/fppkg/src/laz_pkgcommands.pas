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
  { TCommandLazListPackages }

  TCommandLazListPackages = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandLazUnzip }

  TCommandLazUnzip = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

procedure TCommandLazListPackages.Execute;
begin
  Laz_ListPackages;
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

  {$ifdef SVNProtocol}
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
  {$endif}
    { Create builddir }
    if not DirectoryExists(BuildDir) then
      ExecuteSvnCommand('export ' + M.URL + P.FileName + ' "' + BuildDir + '"');
end;

initialization
  RegisterPkgHandler('laz_list',TCommandLazListPackages);
  RegisterPkgHandler('laz_unzip',TCommandLazUnzip);
end.
