{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for FCL 1.0.1

   This file was generated on 08/21/12
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_FCL;

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPAckage('fcl');
    P.Version:='1.0.1';

{$ifdef ALLPACKAGES}
    P.Directory:='packager/registration/';
{$endif ALLPACKAGES}
    P.SupportBuildModes := [bmBuildUnit];

    P.Dependencies.Add('fcl-process');
    P.Dependencies.Add('fcl-db');

    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-vewnhi');
    P.Options.Add('-l');
    P.Options.Add('-Fu.');
    T:=P.Targets.AddUnit('fcllaz.pas');
    t.Dependencies.AddUnit('registerfcl');

    //P.Sources.AddSrc('lazaruspackageintf.pas');
    T:=P.Targets.AddUnit('registerfcl.pas');
    T:=P.Targets.AddUnit('lazaruspackageintf.pas');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_FCL;
  Installer.Run;
end.
{$endif ALLPACKAGES}
