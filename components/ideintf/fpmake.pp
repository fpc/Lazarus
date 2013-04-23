{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for IDEIntf 1.0

   This file was generated on 09/07/12
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_IDEIntf;

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPAckage('ideintf');
    P.Version:='1.0';

{$ifdef ALLPACKAGES}
    // when this is part of a meta package, set here the sub directory
    P.Directory:='ideintf';
{$endif ALLPACKAGES}

    P.Dependencies.Add('lazcontrols');
    P.Dependencies.Add('fcl-res');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-vew');
    P.Options.Add('-l');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCL_PLATFORM)');
    P.IncludePath.Add('images');
    P.Options.Add('-Fu../components/lazcontrols/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../lcl/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../lcl/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../components/lazutils/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../packager/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu.');
    T:=P.Targets.AddUnit('ideintf.pas');
    t.Dependencies.AddUnit('actionseditor');
    t.Dependencies.AddUnit('actionseditorstd');
    t.Dependencies.AddUnit('baseideintf');
    t.Dependencies.AddUnit('checkgroupeditordlg');
    t.Dependencies.AddUnit('checklistboxeditordlg');
    t.Dependencies.AddUnit('collectionpropeditform');
    t.Dependencies.AddUnit('columndlg');
    t.Dependencies.AddUnit('componenteditors');
    t.Dependencies.AddUnit('componentreg');
    t.Dependencies.AddUnit('componenttreeview');
    t.Dependencies.AddUnit('dbpropedits');
    t.Dependencies.AddUnit('fieldseditor');
    t.Dependencies.AddUnit('fieldslist');
    t.Dependencies.AddUnit('formeditingintf');
    t.Dependencies.AddUnit('frmselectprops');
    t.Dependencies.AddUnit('graphicpropedit');
    t.Dependencies.AddUnit('graphpropedits');
    t.Dependencies.AddUnit('headercontrolpropedit');
    t.Dependencies.AddUnit('helpfpdoc');
    t.Dependencies.AddUnit('idecommands');
    t.Dependencies.AddUnit('idedialogs');
    t.Dependencies.AddUnit('ideexterntoolintf');
    t.Dependencies.AddUnit('idehelpintf');
    t.Dependencies.AddUnit('ideimagesintf');
    t.Dependencies.AddUnit('idemsgintf');
    t.Dependencies.AddUnit('ideoptionsintf');
    t.Dependencies.AddUnit('idetextconverter');
    t.Dependencies.AddUnit('idewindowintf');
    t.Dependencies.AddUnit('imagelisteditor');
    t.Dependencies.AddUnit('lazideintf');
    t.Dependencies.AddUnit('lazstringgridedit');
    t.Dependencies.AddUnit('listviewpropedit');
    t.Dependencies.AddUnit('macrointf');
    t.Dependencies.AddUnit('maskpropedit');
    t.Dependencies.AddUnit('menuintf');
    t.Dependencies.AddUnit('newfield');
    t.Dependencies.AddUnit('newitemintf');
    t.Dependencies.AddUnit('objectinspector');
    t.Dependencies.AddUnit('objinspstrconsts');
    t.Dependencies.AddUnit('oifavoriteproperties');
    t.Dependencies.AddUnit('packageintf');
    t.Dependencies.AddUnit('projectintf');
    t.Dependencies.AddUnit('projectresourcesintf');
    t.Dependencies.AddUnit('propedits');
    t.Dependencies.AddUnit('propeditutils');
    t.Dependencies.AddUnit('srceditorintf');
    t.Dependencies.AddUnit('statusbarpropedit');
    t.Dependencies.AddUnit('stringspropeditdlg');
    t.Dependencies.AddUnit('texttools');
    t.Dependencies.AddUnit('treeviewpropedit');
    t.Dependencies.AddUnit('compoptsintf');

    T:=P.Targets.AddUnit('actionseditor.pas');
    T:=P.Targets.AddUnit('actionseditorstd.pas');
    T:=P.Targets.AddUnit('baseideintf.pas');
    T:=P.Targets.AddUnit('checkgroupeditordlg.pas');
    T:=P.Targets.AddUnit('checklistboxeditordlg.pas');
    T:=P.Targets.AddUnit('collectionpropeditform.pas');
    T:=P.Targets.AddUnit('columndlg.pp');
    T:=P.Targets.AddUnit('componenteditors.pas');
    T:=P.Targets.AddUnit('componentreg.pas');
    T:=P.Targets.AddUnit('componenttreeview.pas');
    T:=P.Targets.AddUnit('dbpropedits.pas');
    T:=P.Targets.AddUnit('fieldseditor.pas');
    T:=P.Targets.AddUnit('fieldslist.pas');
    T:=P.Targets.AddUnit('formeditingintf.pas');
    T:=P.Targets.AddUnit('frmselectprops.pas');
    T:=P.Targets.AddUnit('graphicpropedit.pas');
    T:=P.Targets.AddUnit('graphpropedits.pas');
    T:=P.Targets.AddUnit('headercontrolpropedit.pp');
    T:=P.Targets.AddUnit('helpfpdoc.pas');
    T:=P.Targets.AddUnit('idecommands.pas');
    T:=P.Targets.AddUnit('idedialogs.pas');
    T:=P.Targets.AddUnit('ideexterntoolintf.pas');
    T:=P.Targets.AddUnit('idehelpintf.pas');
    T:=P.Targets.AddUnit('ideimagesintf.pas');
    T:=P.Targets.AddUnit('idemsgintf.pas');
    T:=P.Targets.AddUnit('ideoptionsintf.pas');
    T:=P.Targets.AddUnit('idetextconverter.pas');
    T:=P.Targets.AddUnit('idewindowintf.pas');
    T:=P.Targets.AddUnit('imagelisteditor.pp');
    T:=P.Targets.AddUnit('lazideintf.pas');
    T:=P.Targets.AddUnit('lazstringgridedit.pas');
    T:=P.Targets.AddUnit('listviewpropedit.pp');
    T:=P.Targets.AddUnit('macrointf.pas');
    T:=P.Targets.AddUnit('maskpropedit.pas');
    T:=P.Targets.AddUnit('menuintf.pas');
    T:=P.Targets.AddUnit('newfield.pas');
    T:=P.Targets.AddUnit('newitemintf.pas');
    T:=P.Targets.AddUnit('objectinspector.pp');
    T:=P.Targets.AddUnit('objinspstrconsts.pas');
    T:=P.Targets.AddUnit('oifavoriteproperties.pas');
    T:=P.Targets.AddUnit('packageintf.pas');
    T:=P.Targets.AddUnit('projectintf.pas');
    T:=P.Targets.AddUnit('projectresourcesintf.pas');
    T:=P.Targets.AddUnit('propedits.pp');
    T:=P.Targets.AddUnit('propeditutils.pp');
    T:=P.Targets.AddUnit('srceditorintf.pas');
    T:=P.Targets.AddUnit('statusbarpropedit.pp');
    T:=P.Targets.AddUnit('stringspropeditdlg.pas');
    T:=P.Targets.AddUnit('texttools.pas');
    T:=P.Targets.AddUnit('treeviewpropedit.pas');
    T:=P.Targets.AddUnit('compoptsintf.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('IDEIntf.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_IDEIntf;
  Installer.Run;
end.
{$endif ALLPACKAGES}
