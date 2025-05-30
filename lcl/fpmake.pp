{
   File generated automatically by Lazarus Package Manager
   Created with the Fppkgpackagemanager package installed

   fpmake.pp for LCLBase 2.3

   This file was generated on 28-08-21
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
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('lclbase');
    P.Version:='2.3.0-0';

    P.Directory:=ADirectory;

    P.Author:='Lazarus';
    P.License:='modified LGPL-2';
    P.Description:='The platform independent units of the LCL.';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('freetypelaz');
    D := P.Dependencies.Add('chm');
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
    D := T.Dependencies.AddUnit('CheckLst');
    D := T.Dependencies.AddUnit('Clipbrd');
    D := T.Dependencies.AddUnit('ColorBox');
    D := T.Dependencies.AddUnit('ComCtrls');
    D := T.Dependencies.AddUnit('Controls');
    D := T.Dependencies.AddUnit('CustomTimer');
    D := T.Dependencies.AddUnit('DBActns');
    D := T.Dependencies.AddUnit('DBCtrls');
    D := T.Dependencies.AddUnit('DBGrids');
    D := T.Dependencies.AddUnit('DefaultTranslator');
    D := T.Dependencies.AddUnit('Dialogs');
    D := T.Dependencies.AddUnit('ExtCtrls');
    D := T.Dependencies.AddUnit('ExtDlgs');
    D := T.Dependencies.AddUnit('ExtGraphics');
    D := T.Dependencies.AddUnit('FileCtrl');
    D := T.Dependencies.AddUnit('Forms');
    D := T.Dependencies.AddUnit('Graphics');
    D := T.Dependencies.AddUnit('GraphUtil');
    D := T.Dependencies.AddUnit('Grids');
    D := T.Dependencies.AddUnit('HelpIntfs');
    D := T.Dependencies.AddUnit('IcnsTypes');
    D := T.Dependencies.AddUnit('ImageListCache');
    D := T.Dependencies.AddUnit('ImgList');
    D := T.Dependencies.AddUnit('IniPropStorage');
    D := T.Dependencies.AddUnit('InterfaceBase');
    D := T.Dependencies.AddUnit('IntfGraphics');
    D := T.Dependencies.AddUnit('LazHelpHTML');
    D := T.Dependencies.AddUnit('LazHelpIntf');
    D := T.Dependencies.AddUnit('LCLClasses');
    D := T.Dependencies.AddUnit('LCLIntf');
    D := T.Dependencies.AddUnit('LCLMemManager');
    D := T.Dependencies.AddUnit('LCLMessageGlue');
    D := T.Dependencies.AddUnit('LCLProc');
    D := T.Dependencies.AddUnit('LCLResCache');
    D := T.Dependencies.AddUnit('LCLStrConsts');
    D := T.Dependencies.AddUnit('LCLType');
    D := T.Dependencies.AddUnit('Menus');
    D := T.Dependencies.AddUnit('LCLUnicodeData');
    D := T.Dependencies.AddUnit('LCLVersion');
    D := T.Dependencies.AddUnit('LMessages');
    D := T.Dependencies.AddUnit('LResources');
    D := T.Dependencies.AddUnit('MaskEdit');
    D := T.Dependencies.AddUnit('PairSplitter');
    D := T.Dependencies.AddUnit('PopupNotifier');
    D := T.Dependencies.AddUnit('PostScriptCanvas');
    D := T.Dependencies.AddUnit('PostScriptPrinter');
    D := T.Dependencies.AddUnit('PostScriptUnicode');
    D := T.Dependencies.AddUnit('Printers');
    D := T.Dependencies.AddUnit('PropertyStorage');
    D := T.Dependencies.AddUnit('RubberBand');
    D := T.Dependencies.AddUnit('ShellCtrls');
    D := T.Dependencies.AddUnit('Spin');
    D := T.Dependencies.AddUnit('StdActns');
    D := T.Dependencies.AddUnit('StdCtrls');
    D := T.Dependencies.AddUnit('Themes');
    D := T.Dependencies.AddUnit('TmSchema');
    D := T.Dependencies.AddUnit('Toolwin');
    D := T.Dependencies.AddUnit('UTrace');
    D := T.Dependencies.AddUnit('XMLPropStorage');
    D := T.Dependencies.AddUnit('CalendarPopup');
    D := T.Dependencies.AddUnit('TimePopup');
    D := T.Dependencies.AddInclude('include/actionlink.inc');
    D := T.Dependencies.AddInclude('include/application.inc');
    D := T.Dependencies.AddInclude('include/applicationproperties.inc');
    D := T.Dependencies.AddInclude('include/bevel.inc');
    D := T.Dependencies.AddInclude('include/bitbtn.inc');
    D := T.Dependencies.AddInclude('include/bitmap.inc');
    D := T.Dependencies.AddInclude('include/bitmapcanvas.inc');
    D := T.Dependencies.AddInclude('include/boundlabel.inc');
    D := T.Dependencies.AddInclude('include/brush.inc');
    D := T.Dependencies.AddInclude('include/buttoncontrol.inc');
    D := T.Dependencies.AddInclude('include/buttonglyph.inc');
    D := T.Dependencies.AddInclude('include/buttons.inc');
    D := T.Dependencies.AddInclude('include/canvas.inc');
    D := T.Dependencies.AddInclude('include/checkbox.inc');
    D := T.Dependencies.AddInclude('include/clipbrd.inc');
    D := T.Dependencies.AddInclude('include/colorbutton.inc');
    D := T.Dependencies.AddInclude('include/commondialog.inc');
    D := T.Dependencies.AddInclude('include/containedaction.inc');
    D := T.Dependencies.AddInclude('include/control.inc');
    D := T.Dependencies.AddInclude('include/controlactionlink.inc');
    D := T.Dependencies.AddInclude('include/controlcanvas.inc');
    D := T.Dependencies.AddInclude('include/controlscrollbar.inc');
    D := T.Dependencies.AddInclude('include/controlsproc.inc');
    D := T.Dependencies.AddInclude('include/cursorimage.inc');
    D := T.Dependencies.AddInclude('include/customaction.inc');
    D := T.Dependencies.AddInclude('include/customactionlist.inc');
    D := T.Dependencies.AddInclude('include/custombitmap.inc');
    D := T.Dependencies.AddInclude('include/customcheckbox.inc');
    D := T.Dependencies.AddInclude('include/customcheckgroup.inc');
    D := T.Dependencies.AddInclude('include/customcombobox.inc');
    D := T.Dependencies.AddInclude('include/customcontrol.inc');
    D := T.Dependencies.AddInclude('include/customdbcombobox.inc');
    D := T.Dependencies.AddInclude('include/customdblistbox.inc');
    D := T.Dependencies.AddInclude('include/customdockform.inc');
    D := T.Dependencies.AddInclude('include/customedit.inc');
    D := T.Dependencies.AddInclude('include/customform.inc');
    D := T.Dependencies.AddInclude('include/customframe.inc');
    D := T.Dependencies.AddInclude('include/customgroupbox.inc');
    D := T.Dependencies.AddInclude('include/customimage.inc');
    D := T.Dependencies.AddInclude('include/customlabel.inc');
    D := T.Dependencies.AddInclude('include/customlabelededit.inc');
    D := T.Dependencies.AddInclude('include/customlistbox.inc');
    D := T.Dependencies.AddInclude('include/customlistview.inc');
    D := T.Dependencies.AddInclude('include/custommemo.inc');
    D := T.Dependencies.AddInclude('include/customnotebook.inc');
    D := T.Dependencies.AddInclude('include/custompage.inc');
    D := T.Dependencies.AddInclude('include/custompanel.inc');
    D := T.Dependencies.AddInclude('include/customsplitter.inc');
    D := T.Dependencies.AddInclude('include/customstatictext.inc');
    D := T.Dependencies.AddInclude('include/customtrayicon.inc');
    D := T.Dependencies.AddInclude('include/customupdown.inc');
    D := T.Dependencies.AddInclude('include/dbcalendar.inc');
    D := T.Dependencies.AddInclude('include/dbcheckbox.inc');
    D := T.Dependencies.AddInclude('include/dbcombobox.inc');
    D := T.Dependencies.AddInclude('include/dbcustomnavigator.inc');
    D := T.Dependencies.AddInclude('include/dbedit.inc');
    D := T.Dependencies.AddInclude('include/dbgroupbox.inc');
    D := T.Dependencies.AddInclude('include/dbimage.inc');
    D := T.Dependencies.AddInclude('include/dblistbox.inc');
    D := T.Dependencies.AddInclude('include/dblookup.inc');
    D := T.Dependencies.AddInclude('include/dblookupcombobox.inc');
    D := T.Dependencies.AddInclude('include/dblookuplistbox.inc');
    D := T.Dependencies.AddInclude('include/dbmemo.inc');
    D := T.Dependencies.AddInclude('include/dbradiogroup.inc');
    D := T.Dependencies.AddInclude('include/dbtext.inc');
    D := T.Dependencies.AddInclude('include/docktree.inc');
    D := T.Dependencies.AddInclude('include/dockzone.inc');
    D := T.Dependencies.AddInclude('include/dragdock.inc');
    D := T.Dependencies.AddInclude('include/dragimagelist.inc');
    D := T.Dependencies.AddInclude('include/dragmanager.inc');
    D := T.Dependencies.AddInclude('include/dragobject.inc');
    D := T.Dependencies.AddInclude('include/filedialog.inc');
    D := T.Dependencies.AddInclude('include/finddialog.inc');
    D := T.Dependencies.AddInclude('include/font.inc');
    D := T.Dependencies.AddInclude('include/fontdialog.inc');
    D := T.Dependencies.AddInclude('include/fpimagebitmap.inc');
    D := T.Dependencies.AddInclude('include/gifimage.inc');
    D := T.Dependencies.AddInclude('include/glyphlist.inc');
    D := T.Dependencies.AddInclude('include/graphic.inc');
    D := T.Dependencies.AddInclude('include/graphiccontrol.inc');
    D := T.Dependencies.AddInclude('include/graphicsobject.inc');
    D := T.Dependencies.AddInclude('include/headercontrol.inc');
    D := T.Dependencies.AddInclude('include/hintwindow.inc');
    D := T.Dependencies.AddInclude('include/icnsicon.inc');
    D := T.Dependencies.AddInclude('include/icon.inc');
    D := T.Dependencies.AddInclude('include/idletimer.inc');
    D := T.Dependencies.AddInclude('include/imglist.inc');
    D := T.Dependencies.AddInclude('include/inputdialog.inc');
    D := T.Dependencies.AddInclude('include/interfacebase.inc');
    D := T.Dependencies.AddInclude('include/intfbaselcl.inc');
    D := T.Dependencies.AddInclude('include/intfbasewinapi.inc');
    D := T.Dependencies.AddInclude('include/jpegimage.inc');
    D := T.Dependencies.AddInclude('include/lclaction.inc');
    D := T.Dependencies.AddInclude('include/lclintf.inc');
    D := T.Dependencies.AddInclude('include/lclintfh.inc');
    D := T.Dependencies.AddInclude('include/listcolumn.inc');
    D := T.Dependencies.AddInclude('include/listcolumns.inc');
    D := T.Dependencies.AddInclude('include/listitem.inc');
    D := T.Dependencies.AddInclude('include/listitems.inc');
    D := T.Dependencies.AddInclude('include/mainmenu.inc');
    D := T.Dependencies.AddInclude('include/memoscrollbar.inc');
    D := T.Dependencies.AddInclude('include/menu.inc');
    D := T.Dependencies.AddInclude('include/menuactionlink.inc');
    D := T.Dependencies.AddInclude('include/menuitem.inc');
    D := T.Dependencies.AddInclude('include/messagedialogs.inc');
    D := T.Dependencies.AddInclude('include/monitor.inc');
    D := T.Dependencies.AddInclude('include/mouse.inc');
    D := T.Dependencies.AddInclude('include/notebook.inc');
    D := T.Dependencies.AddInclude('include/page.inc');
    D := T.Dependencies.AddInclude('include/pagecontrol.inc');
    D := T.Dependencies.AddInclude('include/paintbox.inc');
    D := T.Dependencies.AddInclude('include/pen.inc');
    D := T.Dependencies.AddInclude('include/picture.inc');
    D := T.Dependencies.AddInclude('include/pixmap.inc');
    D := T.Dependencies.AddInclude('include/png.inc');
    D := T.Dependencies.AddInclude('include/pnm.inc');
    D := T.Dependencies.AddInclude('include/popupmenu.inc');
    D := T.Dependencies.AddInclude('include/progressbar.inc');
    D := T.Dependencies.AddInclude('include/promptdialog.inc');
    D := T.Dependencies.AddInclude('include/radiobutton.inc');
    D := T.Dependencies.AddInclude('include/radiogroup.inc');
    D := T.Dependencies.AddInclude('include/rasterimage.inc');
    D := T.Dependencies.AddInclude('include/reginifile.inc');
    D := T.Dependencies.AddInclude('include/region.inc');
    D := T.Dependencies.AddInclude('include/replacedialog.inc');
    D := T.Dependencies.AddInclude('include/rubberband.inc');
    D := T.Dependencies.AddInclude('include/screen.inc');
    D := T.Dependencies.AddInclude('include/scrollbar.inc');
    D := T.Dependencies.AddInclude('include/scrollbox.inc');
    D := T.Dependencies.AddInclude('include/scrollingwincontrol.inc');
    D := T.Dependencies.AddInclude('include/shape.inc');
    D := T.Dependencies.AddInclude('include/sharedcustombitmap.inc');
    D := T.Dependencies.AddInclude('include/sharedimage.inc');
    D := T.Dependencies.AddInclude('include/sharedrasterimage.inc');
    D := T.Dependencies.AddInclude('include/shortcutlist.inc');
    D := T.Dependencies.AddInclude('include/sizeconstraints.inc');
    D := T.Dependencies.AddInclude('include/speedbutton.inc');
    D := T.Dependencies.AddInclude('include/spinedit.inc');
    D := T.Dependencies.AddInclude('include/statusbar.inc');
    D := T.Dependencies.AddInclude('include/statuspanel.inc');
    D := T.Dependencies.AddInclude('include/statuspanels.inc');
    D := T.Dependencies.AddInclude('include/tabcontrol.inc');
    D := T.Dependencies.AddInclude('include/tabsheet.inc');
    D := T.Dependencies.AddInclude('include/tiffimage.inc');
    D := T.Dependencies.AddInclude('include/togglebox.inc');
    D := T.Dependencies.AddInclude('include/toolbar.inc');
    D := T.Dependencies.AddInclude('include/toolbutton.inc');
    D := T.Dependencies.AddInclude('include/toolwindow.inc');
    D := T.Dependencies.AddInclude('include/trackbar.inc');
    D := T.Dependencies.AddInclude('include/treeview.inc');
    D := T.Dependencies.AddInclude('include/winapi.inc');
    D := T.Dependencies.AddInclude('include/winapih.inc');
    D := T.Dependencies.AddInclude('include/wincontrol.inc');
    D := T.Dependencies.AddUnit('Messages');
    D.OSes := AllOSes - [win32,win64,wince];
    D := T.Dependencies.AddUnit('WSButtons');
    D := T.Dependencies.AddUnit('WSCalendar');
    D := T.Dependencies.AddUnit('WSCheckLst');
    D := T.Dependencies.AddUnit('WSComCtrls');
    D := T.Dependencies.AddUnit('WSControls');
    D := T.Dependencies.AddUnit('WSDesigner');
    D := T.Dependencies.AddUnit('WSDialogs');
    D := T.Dependencies.AddUnit('WSExtCtrls');
    D := T.Dependencies.AddUnit('WSExtDlgs');
    D := T.Dependencies.AddUnit('WSFactory');
    D := T.Dependencies.AddUnit('WSForms');
    D := T.Dependencies.AddUnit('WSGrids');
    D := T.Dependencies.AddUnit('WSImgList');
    D := T.Dependencies.AddUnit('WSLCLClasses');
    D := T.Dependencies.AddUnit('WSMenus');
    D := T.Dependencies.AddUnit('WSPairSplitter');
    D := T.Dependencies.AddUnit('WSProc');
    D := T.Dependencies.AddUnit('WSReferences');
    D := T.Dependencies.AddUnit('WSSpin');
    D := T.Dependencies.AddUnit('WSStdCtrls');
    D := T.Dependencies.AddUnit('WSToolwin');
    D := T.Dependencies.AddUnit('ActnList');
    D := T.Dependencies.AddUnit('AsyncProcess');
    D := T.Dependencies.AddUnit('ButtonPanel');
    D := T.Dependencies.AddUnit('Buttons');
    D := T.Dependencies.AddUnit('Calendar');
    D := T.Dependencies.AddUnit('RegisterLCL');
    D := T.Dependencies.AddInclude('include/lclcolordialog.inc');
    D := T.Dependencies.AddUnit('ValEdit');
    D := T.Dependencies.AddUnit('LazCanvas');
    D := T.Dependencies.AddUnit('LazDialogs');
    D := T.Dependencies.AddUnit('LazRegions');
    D := T.Dependencies.AddUnit('CustomDrawn_Common');
    D := T.Dependencies.AddUnit('CustomDrawnControls');
    D := T.Dependencies.AddUnit('CustomDrawnDrawers');
    D := T.Dependencies.AddUnit('LazDeviceApis');
    D := T.Dependencies.AddUnit('LDockTree');
    D := T.Dependencies.AddUnit('LazFreeTypeIntfDrawer');
    D := T.Dependencies.AddUnit('CustomDrawn_WinXP');
    D := T.Dependencies.AddUnit('CustomDrawn_Android');
    D := T.Dependencies.AddInclude('include/sysenvapis_win.inc');
    D := T.Dependencies.AddInclude('include/sysenvapis.inc');
    D := T.Dependencies.AddInclude('include/sysenvapis_mac.inc');
    D := T.Dependencies.AddInclude('include/sysenvapis_unix.inc');
    D := T.Dependencies.AddInclude('include/lcl_defines.inc');
    D := T.Dependencies.AddUnit('Arrow');
    D := T.Dependencies.AddUnit('EditBtn');
    D := T.Dependencies.AddUnit('ComboEx');
    D := T.Dependencies.AddUnit('DBExtCtrls');
    D := T.Dependencies.AddUnit('CustomDrawn_Mac');
    D := T.Dependencies.AddUnit('CalcForm');
    D := T.Dependencies.AddUnit('LCLTranslator');
    D := T.Dependencies.AddInclude('include/customflowpanel.inc');
    D := T.Dependencies.AddInclude('include/clipbrd_html.inc');
    D := T.Dependencies.AddUnit('GroupedEdit');
    D := T.Dependencies.AddUnit('LCLTaskDialog');
    D := T.Dependencies.AddInclude('include/taskdialog.inc');
    D := T.Dependencies.AddUnit('WSLazDeviceAPIS');
    D := T.Dependencies.AddInclude('include/customdesigncontrol.inc');
    D := T.Dependencies.AddUnit('LCLPlatformDef');
    D := T.Dependencies.AddUnit('IndustrialBase');
    D := T.Dependencies.AddInclude('include/patternbitmap.inc');
    D := T.Dependencies.AddUnit('JSONPropStorage');
    D := T.Dependencies.AddInclude('include/comboex.inc');
    D := T.Dependencies.AddUnit('LCLExceptionStackTrace');
    D := T.Dependencies.AddUnit('WSShellCtrls');
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
    T.ResourceFiles.Add('calendarpopup.lfm');
    T := P.Targets.AddImplicitUnit('forms/timepopup.pas');
    T.ResourceFiles.Add('timepopup.lfm');
    T := P.Targets.AddImplicitUnit('nonwin32/messages.pp');
    T.OSes := AllOSes - [win32,win64,wince];
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
    T := P.Targets.AddImplicitUnit('lclexceptionstacktrace.pas');
    T := P.Targets.AddImplicitUnit('widgetset/wsshellctrls.pp');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('LCLBase.compiled');
    P.InstallFiles.Add('LCLBase.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_LCLBase('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
