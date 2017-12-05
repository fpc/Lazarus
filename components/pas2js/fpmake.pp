{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for Pas2JSDsgn 1.0.1

   This file was generated on 06.12.2017
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_Pas2JSDsgn(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPackage('pas2jsdsgn');
    P.Version:='1.0.1';

    P.Directory:=ADirectory;

    P.Flags.Add('LazarusDsgnPkg');

    P.Dependencies.Add('ideintf');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Schi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCLWidgetType)');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('pas2jsdsgn.pas');
    t.Dependencies.AddUnit('pjsdsgnregister');

    T:=P.Targets.AddUnit('pjsdsgnregister.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('Pas2JSDsgn.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_Pas2JSDsgn('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
