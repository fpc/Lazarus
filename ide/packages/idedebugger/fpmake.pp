{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for IdeDebugger 0.0.1

   This file was generated on 12/04/2024
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

    D := P.Dependencies.Add('ideproject');
    D := P.Dependencies.Add('lazdebuggergdbmi');
    D := P.Dependencies.Add('synedit');
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
    t.Dependencies.AddUnit('basedebugmanager');
    t.Dependencies.AddUnit('watchpropertydlg');
    t.Dependencies.AddUnit('debuggerdlg');
    t.Dependencies.AddUnit('watchesdlg');
    t.Dependencies.AddUnit('callstackdlg');
    t.Dependencies.AddUnit('localsdlg');
    t.Dependencies.AddUnit('threaddlg');
    t.Dependencies.AddUnit('breakpropertydlggroups');
    t.Dependencies.AddUnit('historydlg');
    t.Dependencies.AddUnit('pseudoterminaldlg');
    t.Dependencies.AddUnit('registersdlg');
    t.Dependencies.AddUnit('debugoutputform');
    t.Dependencies.AddUnit('exceptiondlg');
    t.Dependencies.AddUnit('feedbackdlg');
    t.Dependencies.AddUnit('debugattachdialog');
    t.Dependencies.AddUnit('breakpropertydlg');
    t.Dependencies.AddUnit('evaluatedlg');
    t.Dependencies.AddUnit('inspectdlg');
    t.Dependencies.AddUnit('breakpointsdlg');
    t.Dependencies.AddUnit('assemblerdlg');
    t.Dependencies.AddUnit('dbgtreeviewwatchdata');
    t.Dependencies.AddUnit('envdebuggeroptions');
    t.Dependencies.AddUnit('breakpointgroupframe');
    t.Dependencies.AddUnit('idedbgvalueformattersettingsframe');
    t.Dependencies.AddUnit('idedebuggervalueformatter');
    t.Dependencies.AddUnit('idedebugger_valformatter_options');
    t.Dependencies.AddUnit('idedebuggervalueformatterdatetime');
    t.Dependencies.AddUnit('idedebuggervalueformattercolor');
    t.Dependencies.AddUnit('idedebuggervalueformattersetup');
    t.Dependencies.AddUnit('idedebuggervalueformattercurrency');
    t.Dependencies.AddUnit('displayformatconfigframe');
    t.Dependencies.AddUnit('displayformatdefaultsconfigframe');
    t.Dependencies.AddUnit('idedebuggerdisplayformats');
    t.Dependencies.AddUnit('idedebugger_displayformat_options');
    t.Dependencies.AddUnit('projectdebuglink');

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
    T:=P.Targets.AddUnit('basedebugmanager.pas');
    T:=P.Targets.AddUnit('watchpropertydlg.pp');
    T:=P.Targets.AddUnit('debuggerdlg.pp');
    T:=P.Targets.AddUnit('watchesdlg.pp');
    T:=P.Targets.AddUnit('callstackdlg.pp');
    T:=P.Targets.AddUnit('localsdlg.pp');
    T:=P.Targets.AddUnit('threaddlg.pp');
    T:=P.Targets.AddUnit('breakpropertydlggroups.pas');
    T:=P.Targets.AddUnit('historydlg.pp');
    T:=P.Targets.AddUnit('pseudoterminaldlg.pp');
    T:=P.Targets.AddUnit('registersdlg.pp');
    T:=P.Targets.AddUnit('debugoutputform.pp');
    T:=P.Targets.AddUnit('exceptiondlg.pas');
    T:=P.Targets.AddUnit('feedbackdlg.pp');
    T:=P.Targets.AddUnit('debugattachdialog.pas');
    T:=P.Targets.AddUnit('breakpropertydlg.pas');
    T:=P.Targets.AddUnit('evaluatedlg.pp');
    T:=P.Targets.AddUnit('inspectdlg.pas');
    T:=P.Targets.AddUnit('breakpointsdlg.pp');
    T:=P.Targets.AddUnit('assemblerdlg.pp');
    T:=P.Targets.AddUnit('dbgtreeviewwatchdata.pas');
    T:=P.Targets.AddUnit('envdebuggeroptions.pas');
    T:=P.Targets.AddUnit('breakpointgroupframe.pas');
    T:=P.Targets.AddUnit('idedbgvalueformattersettingsframe.pas');
    T:=P.Targets.AddUnit('idedebuggervalueformatter.pas');
    T:=P.Targets.AddUnit('frames\idedebugger_valformatter_options.pas');
    T:=P.Targets.AddUnit('idedebuggervalueformatterdatetime.pas');
    T:=P.Targets.AddUnit('idedebuggervalueformattercolor.pas');
    T:=P.Targets.AddUnit('idedebuggervalueformattersetup.pas');
    T:=P.Targets.AddUnit('idedebuggervalueformattercurrency.pas');
    T:=P.Targets.AddUnit('frames\displayformatconfigframe.pas');
    T:=P.Targets.AddUnit('frames\displayformatdefaultsconfigframe.pas');
    T:=P.Targets.AddUnit('idedebuggerdisplayformats.pas');
    T:=P.Targets.AddUnit('idedebugger_displayformat_options.pas');
    T:=P.Targets.AddUnit('projectdebuglink.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('idedebugger.compiled');
    P.InstallFiles.Add('idedebugger.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_IdeDebugger('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
