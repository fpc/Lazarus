{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for LCLBase 2.1

   This file was generated on 29-12-18
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_LCLBase(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPackage('lclbase');
    P.Version:='2.1';

    P.Directory:=ADirectory;

    P.Flags.Add('LazarusDsgnPkg');

    P.Dependencies.Add('chm');
    P.Dependencies.Add('lazutils');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewibq');
    P.Options.Add('-vn-h-');
    P.IncludePath.Add('include');
    P.UnitPath.Add('forms');
    P.UnitPath.Add('widgetset');
    P.UnitPath.Add('nonwin32');
    P.UnitPath.Add('.');
    P.InstallFiles.Add('cursors.res', '$(unitinstalldir)');
    P.InstallFiles.Add('btn_icons.res', '$(unitinstalldir)');
    P.InstallFiles.Add('dialog_icons.res', '$(unitinstalldir)');
    P.InstallFiles.Add('lcl_grid_images.res', '$(unitinstalldir)');
    P.InstallFiles.Add('lcl_edbtnimg.res', '$(unitinstalldir)');
    P.InstallFiles.Add('lcl_dock_images.res', '$(unitinstalldir)');
    T:=P.Targets.AddUnit('alllclunits.pp');
    t.Dependencies.AddUnit('CheckLst');
    t.Dependencies.AddUnit('Clipbrd');
    t.Dependencies.AddUnit('ColorBox');
    t.Dependencies.AddUnit('ComCtrls');
    t.Dependencies.AddUnit('Controls');
    t.Dependencies.AddUnit('CustomTimer');
    t.Dependencies.AddUnit('DBActns');
    t.Dependencies.AddUnit('DBCtrls');
    t.Dependencies.AddUnit('DBGrids');
    t.Dependencies.AddUnit('DefaultTranslator');
    t.Dependencies.AddUnit('Dialogs');
    t.Dependencies.AddUnit('ExtCtrls');
    t.Dependencies.AddUnit('ExtDlgs');
    t.Dependencies.AddUnit('ExtGraphics');
    t.Dependencies.AddUnit('FileCtrl');
    t.Dependencies.AddUnit('Forms');
    t.Dependencies.AddUnit('Graphics');
    t.Dependencies.AddUnit('GraphMath');
    t.Dependencies.AddUnit('GraphUtil');
    t.Dependencies.AddUnit('Grids');
    t.Dependencies.AddUnit('HelpIntfs');
    t.Dependencies.AddUnit('IcnsTypes');
    t.Dependencies.AddUnit('ImageListCache');
    t.Dependencies.AddUnit('ImgList');
    t.Dependencies.AddUnit('IniPropStorage');
    t.Dependencies.AddUnit('InterfaceBase');
    t.Dependencies.AddUnit('IntfGraphics');
    t.Dependencies.AddUnit('LazHelpHTML');
    t.Dependencies.AddUnit('LazHelpIntf');
    t.Dependencies.AddUnit('LCLClasses');
    t.Dependencies.AddUnit('LCLIntf');
    t.Dependencies.AddUnit('LCLMemManager');
    t.Dependencies.AddUnit('LCLMessageGlue');
    t.Dependencies.AddUnit('LCLProc');
    t.Dependencies.AddUnit('LCLResCache');
    t.Dependencies.AddUnit('LCLStrConsts');
    t.Dependencies.AddUnit('LCLType');
    t.Dependencies.AddUnit('Menus');
    t.Dependencies.AddUnit('LCLUnicodeData');
    t.Dependencies.AddUnit('LCLVersion');
    t.Dependencies.AddUnit('LMessages');
    t.Dependencies.AddUnit('LResources');
    t.Dependencies.AddUnit('MaskEdit');
    t.Dependencies.AddUnit('PairSplitter');
    t.Dependencies.AddUnit('PopupNotifier');
    t.Dependencies.AddUnit('PostScriptCanvas');
    t.Dependencies.AddUnit('PostScriptPrinter');
    t.Dependencies.AddUnit('PostScriptUnicode');
    t.Dependencies.AddUnit('Printers');
    t.Dependencies.AddUnit('PropertyStorage');
    t.Dependencies.AddUnit('RubberBand');
    t.Dependencies.AddUnit('ShellCtrls');
    t.Dependencies.AddUnit('Spin');
    t.Dependencies.AddUnit('StdActns');
    t.Dependencies.AddUnit('StdCtrls');
    t.Dependencies.AddUnit('Themes');
    t.Dependencies.AddUnit('TmSchema');
    t.Dependencies.AddUnit('Toolwin');
    t.Dependencies.AddUnit('UTrace');
    t.Dependencies.AddUnit('XMLPropStorage');
    t.Dependencies.AddUnit('CalendarPopup');
    t.Dependencies.AddUnit('TimePopup');
    t.Dependencies.AddUnit('Messages');
    t.Dependencies.AddUnit('WSButtons');
    t.Dependencies.AddUnit('WSCalendar');
    t.Dependencies.AddUnit('WSCheckLst');
    t.Dependencies.AddUnit('WSComCtrls');
    t.Dependencies.AddUnit('WSControls');
    t.Dependencies.AddUnit('WSDesigner');
    t.Dependencies.AddUnit('WSDialogs');
    t.Dependencies.AddUnit('WSExtCtrls');
    t.Dependencies.AddUnit('WSExtDlgs');
    t.Dependencies.AddUnit('WSFactory');
    t.Dependencies.AddUnit('WSForms');
    t.Dependencies.AddUnit('WSGrids');
    t.Dependencies.AddUnit('WSImgList');
    t.Dependencies.AddUnit('WSLCLClasses');
    t.Dependencies.AddUnit('WSMenus');
    t.Dependencies.AddUnit('WSPairSplitter');
    t.Dependencies.AddUnit('WSProc');
    t.Dependencies.AddUnit('WSReferences');
    t.Dependencies.AddUnit('WSSpin');
    t.Dependencies.AddUnit('WSStdCtrls');
    t.Dependencies.AddUnit('WSToolwin');
    t.Dependencies.AddUnit('ActnList');
    t.Dependencies.AddUnit('AsyncProcess');
    t.Dependencies.AddUnit('ButtonPanel');
    t.Dependencies.AddUnit('Buttons');
    t.Dependencies.AddUnit('Calendar');
    t.Dependencies.AddUnit('RegisterLCL');
    t.Dependencies.AddUnit('ValEdit');
    t.Dependencies.AddUnit('LazCanvas');
    t.Dependencies.AddUnit('LazDialogs');
    t.Dependencies.AddUnit('LazRegions');
    t.Dependencies.AddUnit('CustomDrawn_Common');
    t.Dependencies.AddUnit('CustomDrawnControls');
    t.Dependencies.AddUnit('CustomDrawnDrawers');
    t.Dependencies.AddUnit('LazDeviceApis');
    t.Dependencies.AddUnit('LDockTree');
    t.Dependencies.AddUnit('LazFreeTypeIntfDrawer');
    t.Dependencies.AddUnit('CustomDrawn_WinXP');
    t.Dependencies.AddUnit('CustomDrawn_Android');
    t.Dependencies.AddUnit('Arrow');
    t.Dependencies.AddUnit('EditBtn');
    t.Dependencies.AddUnit('ComboEx');
    t.Dependencies.AddUnit('DBExtCtrls');
    t.Dependencies.AddUnit('CustomDrawn_Mac');
    t.Dependencies.AddUnit('CalcForm');
    t.Dependencies.AddUnit('LCLTranslator');
    t.Dependencies.AddUnit('GroupedEdit');
    t.Dependencies.AddUnit('LCLTaskDialog');
    t.Dependencies.AddUnit('WSLazDeviceAPIS');
    t.Dependencies.AddUnit('LCLPlatformDef');
    t.Dependencies.AddUnit('IndustrialBase');
    t.Dependencies.AddUnit('JSONPropStorage');
    t.Dependencies.AddUnit('DBLogDlg');
    t.Dependencies.AddUnit('LCLExceptionStackTrace');
    T := P.Targets.AddImplicitUnit('checklst.pas');
    T := P.Targets.AddImplicitUnit('clipbrd.pp');
    T := P.Targets.AddImplicitUnit('colorbox.pas');
    T := P.Targets.AddImplicitUnit('comctrls.pp');
    T := P.Targets.AddImplicitUnit('controls.pp');
    T := P.Targets.AddImplicitUnit('customtimer.pas');
    T := P.Targets.AddImplicitUnit('dbactns.pp');
    T := P.Targets.AddImplicitUnit('dbctrls.pp');
    T := P.Targets.AddImplicitUnit('dbgrids.pas');
    T := P.Targets.AddImplicitUnit('defaulttranslator.pas');
    T := P.Targets.AddImplicitUnit('dialogs.pp');
    T := P.Targets.AddImplicitUnit('extctrls.pp');
    T := P.Targets.AddImplicitUnit('extdlgs.pas');
    T := P.Targets.AddImplicitUnit('extgraphics.pas');
    T := P.Targets.AddImplicitUnit('filectrl.pp');
    T := P.Targets.AddImplicitUnit('forms.pp');
    T := P.Targets.AddImplicitUnit('graphics.pp');
    T := P.Targets.AddImplicitUnit('graphmath.pp');
    T := P.Targets.AddImplicitUnit('graphutil.pp');
    T := P.Targets.AddImplicitUnit('grids.pas');
    T := P.Targets.AddImplicitUnit('helpintfs.pas');
    T := P.Targets.AddImplicitUnit('icnstypes.pas');
    T := P.Targets.AddImplicitUnit('imagelistcache.pas');
    T := P.Targets.AddImplicitUnit('imglist.pp');
    T := P.Targets.AddImplicitUnit('inipropstorage.pas');
    T := P.Targets.AddImplicitUnit('interfacebase.pp');
    T := P.Targets.AddImplicitUnit('intfgraphics.pas');
    T := P.Targets.AddImplicitUnit('lazhelphtml.pas');
    T := P.Targets.AddImplicitUnit('lazhelpintf.pas');
    T := P.Targets.AddImplicitUnit('lclclasses.pp');
    T := P.Targets.AddImplicitUnit('lclintf.pas');
    T := P.Targets.AddImplicitUnit('lclmemmanager.pas');
    T := P.Targets.AddImplicitUnit('lclmessageglue.pas');
    T := P.Targets.AddImplicitUnit('lclproc.pas');
    T := P.Targets.AddImplicitUnit('lclrescache.pas');
    T := P.Targets.AddImplicitUnit('lclstrconsts.pas');
    T := P.Targets.AddImplicitUnit('lcltype.pp');
    T := P.Targets.AddImplicitUnit('menus.pp');
    T := P.Targets.AddImplicitUnit('lclunicodedata.pas');
    T := P.Targets.AddImplicitUnit('lclversion.pas');
    T := P.Targets.AddImplicitUnit('lmessages.pp');
    T := P.Targets.AddImplicitUnit('lresources.pp');
    T := P.Targets.AddImplicitUnit('maskedit.pp');
    T := P.Targets.AddImplicitUnit('pairsplitter.pas');
    T := P.Targets.AddImplicitUnit('popupnotifier.pas');
    T := P.Targets.AddImplicitUnit('postscriptcanvas.pas');
    T := P.Targets.AddImplicitUnit('postscriptprinter.pas');
    T := P.Targets.AddImplicitUnit('postscriptunicode.pas');
    T := P.Targets.AddImplicitUnit('printers.pas');
    T := P.Targets.AddImplicitUnit('propertystorage.pas');
    T := P.Targets.AddImplicitUnit('rubberband.pas');
    T := P.Targets.AddImplicitUnit('shellctrls.pas');
    T := P.Targets.AddImplicitUnit('spin.pp');
    T := P.Targets.AddImplicitUnit('stdactns.pas');
    T := P.Targets.AddImplicitUnit('stdctrls.pp');
    T := P.Targets.AddImplicitUnit('themes.pas');
    T := P.Targets.AddImplicitUnit('tmschema.pas');
    T := P.Targets.AddImplicitUnit('toolwin.pp');
    T := P.Targets.AddImplicitUnit('utrace.pp');
    T := P.Targets.AddImplicitUnit('xmlpropstorage.pas');
    T := P.Targets.AddImplicitUnit('forms/calendarpopup.pas');
    T := P.Targets.AddImplicitUnit('forms/timepopup.pas');
    T := P.Targets.AddImplicitUnit('nonwin32/messages.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wsbuttons.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wscalendar.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wschecklst.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wscomctrls.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wscontrols.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wsdesigner.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wsdialogs.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wsextctrls.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wsextdlgs.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wsfactory.pas');
    T := P.Targets.AddImplicitUnit('widgetset/wsforms.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wsgrids.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wsimglist.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wslclclasses.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wsmenus.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wspairsplitter.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wsproc.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wsreferences.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wsspin.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wsstdctrls.pp');
    T := P.Targets.AddImplicitUnit('widgetset/wstoolwin.pp');
    T := P.Targets.AddImplicitUnit('actnlist.pas');
    T := P.Targets.AddImplicitUnit('asyncprocess.pp');
    T := P.Targets.AddImplicitUnit('buttonpanel.pas');
    T := P.Targets.AddImplicitUnit('buttons.pp');
    T := P.Targets.AddImplicitUnit('calendar.pp');
    T := P.Targets.AddImplicitUnit('registerlcl.pas');
    T := P.Targets.AddImplicitUnit('valedit.pas');
    T := P.Targets.AddImplicitUnit('lazcanvas.pas');
    T := P.Targets.AddImplicitUnit('lazdialogs.pas');
    T := P.Targets.AddImplicitUnit('lazregions.pas');
    T := P.Targets.AddImplicitUnit('customdrawn_common.pas');
    T := P.Targets.AddImplicitUnit('customdrawncontrols.pas');
    T := P.Targets.AddImplicitUnit('customdrawndrawers.pas');
    T := P.Targets.AddImplicitUnit('lazdeviceapis.pas');
    T := P.Targets.AddImplicitUnit('ldocktree.pas');
    T := P.Targets.AddImplicitUnit('lazfreetypeintfdrawer.pas');
    T := P.Targets.AddImplicitUnit('customdrawn_winxp.pas');
    T := P.Targets.AddImplicitUnit('customdrawn_android.pas');
    T := P.Targets.AddImplicitUnit('arrow.pp');
    T := P.Targets.AddImplicitUnit('editbtn.pas');
    T := P.Targets.AddImplicitUnit('comboex.pas');
    T := P.Targets.AddImplicitUnit('dbextctrls.pp');
    T := P.Targets.AddImplicitUnit('customdrawn_mac.pas');
    T := P.Targets.AddImplicitUnit('forms/calcform.pas');
    T := P.Targets.AddImplicitUnit('lcltranslator.pas');
    T := P.Targets.AddImplicitUnit('groupededit.pp');
    T := P.Targets.AddImplicitUnit('lcltaskdialog.pas');
    T := P.Targets.AddImplicitUnit('widgetset/wslazdeviceapis.pas');
    T := P.Targets.AddImplicitUnit('lclplatformdef.pas');
    T := P.Targets.AddImplicitUnit('industrialbase.pp');
    T := P.Targets.AddImplicitUnit('jsonpropstorage.pas');
    T := P.Targets.AddImplicitUnit('forms/dblogdlg.pas');
    T := P.Targets.AddImplicitUnit('lclexceptionstacktrace.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('LCLBase.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_LCLBase('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
