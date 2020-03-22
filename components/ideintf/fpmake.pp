{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for IDEIntf 1.0

   This file was generated on 21.03.2020
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_IDEIntf(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('ideintf');
    P.Version:='1.0';

    P.Directory:=ADirectory;

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('buildintf');
    D := P.Dependencies.Add('lazcontrols');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCLWidgetType)');
    P.IncludePath.Add('images');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('ideintf.pas');
    t.Dependencies.AddUnit('actionseditor');
    t.Dependencies.AddUnit('actionseditorstd');
    t.Dependencies.AddUnit('changeparentdlg');
    t.Dependencies.AddUnit('checkgroupeditordlg');
    t.Dependencies.AddUnit('checklistboxeditordlg');
    t.Dependencies.AddUnit('collectionpropeditform');
    t.Dependencies.AddUnit('columndlg');
    t.Dependencies.AddUnit('componenteditors');
    t.Dependencies.AddUnit('componentreg');
    t.Dependencies.AddUnit('componenttreeview');
    t.Dependencies.AddUnit('dbgridcolumnspropeditform');
    t.Dependencies.AddUnit('dbpropedits');
    t.Dependencies.AddUnit('editorsyntaxhighlighterdef');
    t.Dependencies.AddUnit('fieldseditor');
    t.Dependencies.AddUnit('fieldslist');
    t.Dependencies.AddUnit('filefilterpropeditor');
    t.Dependencies.AddUnit('formeditingintf');
    t.Dependencies.AddUnit('frmselectprops');
    t.Dependencies.AddUnit('graphicpropedit');
    t.Dependencies.AddUnit('graphpropedits');
    t.Dependencies.AddUnit('headercontrolpropedit');
    t.Dependencies.AddUnit('helpfpdoc');
    t.Dependencies.AddUnit('idecommands');
    t.Dependencies.AddUnit('idedialogs');
    t.Dependencies.AddUnit('idehelpintf');
    t.Dependencies.AddUnit('ideimagesintf');
    t.Dependencies.AddUnit('idemsgintf');
    t.Dependencies.AddUnit('ideopteditorintf');
    t.Dependencies.AddUnit('idetextconverter');
    t.Dependencies.AddUnit('ideutils');
    t.Dependencies.AddUnit('idewindowintf');
    t.Dependencies.AddUnit('imagelisteditor');
    t.Dependencies.AddUnit('keyvalpropeditdlg');
    t.Dependencies.AddUnit('lazideintf');
    t.Dependencies.AddUnit('lazstringgridedit');
    t.Dependencies.AddUnit('listviewpropedit');
    t.Dependencies.AddUnit('maskpropedit');
    t.Dependencies.AddUnit('menuintf');
    t.Dependencies.AddUnit('newfield');
    t.Dependencies.AddUnit('objectinspector');
    t.Dependencies.AddUnit('objinspstrconsts');
    t.Dependencies.AddUnit('oifavoriteproperties');
    t.Dependencies.AddUnit('projectgroupintf');
    t.Dependencies.AddUnit('propedits');
    t.Dependencies.AddUnit('propeditutils');
    t.Dependencies.AddUnit('srceditorintf');
    t.Dependencies.AddUnit('statusbarpropedit');
    t.Dependencies.AddUnit('stringspropeditdlg');
    t.Dependencies.AddUnit('texttools');
    t.Dependencies.AddUnit('toolbarintf');
    t.Dependencies.AddUnit('treeviewpropedit');
    t.Dependencies.AddUnit('unitresources');

    T:=P.Targets.AddUnit('actionseditor.pas');
    T:=P.Targets.AddUnit('actionseditorstd.pas');
    T:=P.Targets.AddUnit('changeparentdlg.pas');
    T:=P.Targets.AddUnit('checkgroupeditordlg.pas');
    T:=P.Targets.AddUnit('checklistboxeditordlg.pas');
    T:=P.Targets.AddUnit('collectionpropeditform.pas');
    T:=P.Targets.AddUnit('columndlg.pp');
    T:=P.Targets.AddUnit('componenteditors.pas');
    T:=P.Targets.AddUnit('componentreg.pas');
    T:=P.Targets.AddUnit('componenttreeview.pas');
    T:=P.Targets.AddUnit('dbgridcolumnspropeditform.pas');
    T:=P.Targets.AddUnit('dbpropedits.pas');
    T:=P.Targets.AddUnit('editorsyntaxhighlighterdef.pas');
    T:=P.Targets.AddUnit('fieldseditor.pas');
    T:=P.Targets.AddUnit('fieldslist.pas');
    T:=P.Targets.AddUnit('filefilterpropeditor.pas');
    T:=P.Targets.AddUnit('formeditingintf.pas');
    T:=P.Targets.AddUnit('frmselectprops.pas');
    T:=P.Targets.AddUnit('graphicpropedit.pas');
    T:=P.Targets.AddUnit('graphpropedits.pas');
    T:=P.Targets.AddUnit('headercontrolpropedit.pp');
    T:=P.Targets.AddUnit('helpfpdoc.pas');
    T:=P.Targets.AddUnit('idecommands.pas');
    T:=P.Targets.AddUnit('idedialogs.pas');
    T:=P.Targets.AddUnit('idehelpintf.pas');
    T:=P.Targets.AddUnit('ideimagesintf.pas');
    T:=P.Targets.AddUnit('idemsgintf.pas');
    T:=P.Targets.AddUnit('ideopteditorintf.pas');
    T:=P.Targets.AddUnit('idetextconverter.pas');
    T:=P.Targets.AddUnit('ideutils.pas');
    T:=P.Targets.AddUnit('idewindowintf.pas');
    T:=P.Targets.AddUnit('imagelisteditor.pp');
    T:=P.Targets.AddUnit('keyvalpropeditdlg.pas');
    T:=P.Targets.AddUnit('lazideintf.pas');
    T:=P.Targets.AddUnit('lazstringgridedit.pas');
    T:=P.Targets.AddUnit('listviewpropedit.pp');
    T:=P.Targets.AddUnit('maskpropedit.pas');
    T:=P.Targets.AddUnit('menuintf.pas');
    T:=P.Targets.AddUnit('newfield.pas');
    T:=P.Targets.AddUnit('objectinspector.pp');
    T:=P.Targets.AddUnit('objinspstrconsts.pas');
    T:=P.Targets.AddUnit('oifavoriteproperties.pas');
    T:=P.Targets.AddUnit('projectgroupintf.pp');
    T:=P.Targets.AddUnit('propedits.pp');
    T:=P.Targets.AddUnit('propeditutils.pp');
    T:=P.Targets.AddUnit('srceditorintf.pas');
    T:=P.Targets.AddUnit('statusbarpropedit.pp');
    T:=P.Targets.AddUnit('stringspropeditdlg.pas');
    T:=P.Targets.AddUnit('texttools.pas');
    T:=P.Targets.AddUnit('toolbarintf.pas');
    T:=P.Targets.AddUnit('treeviewpropedit.pas');
    T:=P.Targets.AddUnit('unitresources.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('IDEIntf.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_IDEIntf('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
