{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for SynEditSpellCheckerDsgn 0.0

   This file was generated on 12/01/2026
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_SynEditSpellCheckerDsgn(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('syneditspellcheckerdsgn');
    P.Version:='<none>';

    P.Directory:=ADirectory;

    P.Author:='M Friebe';
    P.License:='Either of: Modified LGPL-2 (as LCL) / Modified LGPL-3 / MPL 1.1 or 2.0'#13#10''#13#10'Used "uses" units may have to be amended in case of chosing any subset of the Licenses.';
    P.Description:='IDE registration for: SynEditSpellChecker - spell check add-on for SynEdit';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('lazcontroldsgn');
    D := P.Dependencies.Add('ideintf');
    D := P.Dependencies.Add('syneditspellchecker');
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
    T:=P.Targets.AddUnit('syneditspellcheckerdsgn.pas');
    t.Dependencies.AddUnit('synspellcheckdsgnregister');
    t.Dependencies.AddUnit('syn_spell_options');
    t.Dependencies.AddUnit('synspellcheckdsgnstrings');
    t.Dependencies.AddUnit('synspellcheckdsgnoptions');

    T:=P.Targets.AddUnit('synspellcheckdsgnregister.pas');
    T:=P.Targets.AddUnit('syn_spell_options.pas');
    T:=P.Targets.AddUnit('synspellcheckdsgnstrings.pas');
    T:=P.Targets.AddUnit('synspellcheckdsgnoptions.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('syneditspellcheckerdsgn.compiled');
    P.InstallFiles.Add('syneditspellcheckerdsgn.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_SynEditSpellCheckerDsgn('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
