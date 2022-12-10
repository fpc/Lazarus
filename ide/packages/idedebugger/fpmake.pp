{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for IdeDebugger 0.0.1

   This file was generated on 10/12/2022
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_IdeDebugger(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('idedebugger');
    P.Version:='0.0.1-0';

    P.Directory:=ADirectory;

    P.Author:='Lazarus Team';
    P.License:='GPL-2';
    P.Description:='GUI Debugger Frontend for the IDE';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('ideconfig');
    D := P.Dependencies.Add('lazdebuggerfp');
    D := P.Dependencies.Add('lazcontroldsgn');
    D := P.Dependencies.Add('laz.virtualtreeview_package');
    D := P.Dependencies.Add('debuggerintf');
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
    P.UnitPath.Add('frames');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('idedebuggerpackage.pas');
    t.Dependencies.AddUnit('idedebuggerbase');
    t.Dependencies.AddUnit('debugger');
    t.Dependencies.AddUnit('processdebugger');
    t.Dependencies.AddUnit('processlist');
    t.Dependencies.AddUnit('debuggertreeview');
    t.Dependencies.AddUnit('idedebuggerutils');
    t.Dependencies.AddUnit('idedebuggerwatchresult');
    t.Dependencies.AddUnit('idedebuggerwatchresprinter');
    t.Dependencies.AddUnit('idedebuggerwatchresutils');
    t.Dependencies.AddUnit('arraynavigationframe');
    t.Dependencies.AddUnit('idedebuggerstringconstants');
    t.Dependencies.AddUnit('idedebuggerbackendvalueconv');
    t.Dependencies.AddUnit('idedbgvalueconvertersettingsframe');
    t.Dependencies.AddUnit('idedebugger_valconv_options');
    t.Dependencies.AddUnit('idedebuggeropts');
    t.Dependencies.AddUnit('idedebuggerwatchresultjson');
    t.Dependencies.AddUnit('watchinspecttoolbar');

    T:=P.Targets.AddUnit('idedebuggerbase.pas');
    T:=P.Targets.AddUnit('debugger.pp');
    T:=P.Targets.AddUnit('processdebugger.pp');
    T:=P.Targets.AddUnit('processlist.pas');
    T:=P.Targets.AddUnit('debuggertreeview.pas');
    T:=P.Targets.AddUnit('idedebuggerutils.pas');
    T:=P.Targets.AddUnit('idedebuggerwatchresult.pas');
    T:=P.Targets.AddUnit('idedebuggerwatchresprinter.pas');
    T:=P.Targets.AddUnit('idedebuggerwatchresutils.pas');
    T:=P.Targets.AddUnit('arraynavigationframe.pas');
    T:=P.Targets.AddUnit('idedebuggerstringconstants.pas');
    T:=P.Targets.AddUnit('idedebuggerbackendvalueconv.pas');
    T:=P.Targets.AddUnit('idedbgvalueconvertersettingsframe.pas');
    T:=P.Targets.AddUnit('frames\idedebugger_valconv_options.pas');
    T:=P.Targets.AddUnit('idedebuggeropts.pas');
    T:=P.Targets.AddUnit('idedebuggerwatchresultjson.pas');
    T:=P.Targets.AddUnit('frames\watchinspecttoolbar.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('IdeDebugger.compiled');
    P.InstallFiles.Add('IdeDebugger.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_IdeDebugger('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
