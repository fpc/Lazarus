{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for SynEditSpellChecker 0.1

   This file was generated on 12/01/2026
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_SynEditSpellChecker(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('syneditspellchecker');
    P.Version:='0.1.0-0';

    P.Directory:=ADirectory;

    P.Author:='M Friebe';
    P.License:='Either of: Modified LGPL-2 (as LCL) / Modified LGPL-3 / MPL 1.1 or 2.0'#13#10''#13#10'Used "uses" units may have to be amended in case of chosing any subset of the Licenses.';
    P.Description:='SynEditSpellChecker - spell check add-on for SynEdit';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('ideintf');
    D := P.Dependencies.Add('synedit');
    D := P.Dependencies.Add('fcl');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCLWidgetType)');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('syneditspellchecker.pas');
    t.Dependencies.AddUnit('synspellcheckplugin');
    t.Dependencies.AddUnit('synaspelldef');
    t.Dependencies.AddUnit('synspelldictionary');

    T:=P.Targets.AddUnit('synspellcheckplugin.pas');
    T:=P.Targets.AddUnit('synaspelldef.pas');
    T:=P.Targets.AddUnit('synspelldictionary.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('syneditspellchecker.compiled');
    P.InstallFiles.Add('syneditspellchecker.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_SynEditSpellChecker('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
