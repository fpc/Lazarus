{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for IdeHelpUtils 0.0

   This file was generated on 24/07/2026
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_IdeHelpUtils(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('idehelputils');
    P.Version:='<none>';

    P.Directory:=ADirectory;


    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('fcl');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('idehelputils.pas');


    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('idehelputils.compiled');
    P.InstallFiles.Add('idehelputils.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_IdeHelpUtils('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
