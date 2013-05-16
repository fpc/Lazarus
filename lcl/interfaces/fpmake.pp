{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for LCL 1.0.1

   This file was generated on 08/21/12
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_LCL;

var
  P : TPackage;
  T : TTarget;
  WidgetSetVariant: TPackageVariants;

begin
  with Installer do
    begin
    P:=AddPAckage('lcl');
    P.Version:='1.0.1';

    WidgetSetVariant := P.AddPackageVariant('widgetset',true);
    WidgetSetVariant.Add('carbon');
    WidgetSetVariant.Add('gtk2');
    WidgetSetVariant.Add('win');


    //GlobalDictionary.AddVariable('LCL_PLATFORM','carbon');
    //P.SetUnitsOutputDir('units'+PathDelim+'$(target)'+PathDelim+'$(LCL_PLATFORM)');
    //P.SetPackageUnitInstallDir('$(LCL_PLATFORM)');

{$ifdef ALLPACKAGES}
    P.Directory:='lcl/interfaces/';
{$endif ALLPACKAGES}
    P.SupportBuildModes := [bmBuildUnit];


    P.Dependencies.Add('lclbase');
    P.Dependencies.Add('fcl');
    P.Dependencies.Add('univint');
    P.Dependencies.Add('cocoaint');
    P.Dependencies.Add('opengl');

    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-vewnhi');
    P.Options.Add('-l');
    P.Options.Add('-vm5044');
    P.Options.Add('-dcarbon');
    P.IncludePath.Add('$(widgetset)');
    P.IncludePath.Add('carbon/objc');
    P.IncludePath.Add('carbon/pascocoa/appkit');
    P.IncludePath.Add('carbon/pascocoa/foundation');
    P.SourcePath.Add('$(widgetset)');
    P.SourcePath.Add('carbon/objc',[darwin]);
    P.SourcePath.Add('carbon/pascocoa/appkit',[darwin]);
    P.SourcePath.Add('carbon/pascocoa/foundation',[darwin]);
    P.Options.Add('-Fu../units/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../components/lazutils/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../packager/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu.');
    T:=P.Targets.AddUnit('lcl.pas');
    t.Dependencies.AddUnit('alllclintfunits');


    T:=P.Targets.AddUnit('foundation.pas',[darwin]);
    T:=P.Targets.AddUnit('lobjc.pas',[darwin]);
    T:=P.Targets.AddUnit('appkit.pas',[darwin]);


    P.Targets.AddUnit('agl.pp');
    P.Targets.AddUnit('alllclintfunits.pas');
    P.Targets.AddUnit('carbonbars.pp');
    P.Targets.AddUnit('carbonbuttons.pp');
    P.Targets.AddUnit('carboncalendar.pas');
    P.Targets.AddUnit('carboncalendarview.pas');
    P.Targets.AddUnit('carboncanvas.pp');
    P.Targets.AddUnit('carboncaret.pas');
    P.Targets.AddUnit('carbonclipboard.pp');
    P.Targets.AddUnit('carbondbgconsts.pp');
    P.Targets.AddUnit('carbondebug.pp');
    P.Targets.AddUnit('carbondef.pp');
    P.Targets.AddUnit('carbonedits.pp');
    P.Targets.AddUnit('carbongdiobjects.pp');
    P.Targets.AddUnit('carbonint.pas');
    P.Targets.AddUnit('carbonlistviews.pp');
    P.Targets.AddUnit('carbonmenus.pp');
    P.Targets.AddUnit('carbonprivate.pp');
    P.Targets.AddUnit('carbonproc.pp');
    P.Targets.AddUnit('carbonstrings.pp');
    P.Targets.AddUnit('carbontabs.pp');
    P.Targets.AddUnit('carbonthemes.pas');
    P.Targets.AddUnit('carbonutils.pas');
    P.Targets.AddUnit('carbonwsbuttons.pp');
    P.Targets.AddUnit('carbonwscalendar.pp');
    P.Targets.AddUnit('carbonwschecklst.pp');
    P.Targets.AddUnit('carbonwscomctrls.pp');
    P.Targets.AddUnit('carbonwscontrols.pp');
    P.Targets.AddUnit('carbonwsdialogs.pp');
    P.Targets.AddUnit('carbonwsextctrls.pp');
    P.Targets.AddUnit('carbonwsextdlgs.pp');
    P.Targets.AddUnit('carbonwsfactory.pas');
    P.Targets.AddUnit('carbonwsforms.pp');
    P.Targets.AddUnit('carbonwsgrids.pp');
    P.Targets.AddUnit('carbonwsimglist.pp');
    P.Targets.AddUnit('carbonwsmenus.pp');
    P.Targets.AddUnit('carbonwspairsplitter.pp');
    P.Targets.AddUnit('carbonwsspin.pp');
    P.Targets.AddUnit('carbonwsstdctrls.pp');
    P.Targets.AddUnit('glgrab.pas');
    P.Targets.AddUnit('interfaces.pas');
    P.Targets.AddUnit('opengl.pas');


    P.Sources.AddSrc('carbon/agl.pp');
    P.Sources.AddSrc('carbon/alllclintfunits.pas');
    P.Sources.AddSrc('carbon/carbonbars.pp');
    P.Sources.AddSrc('carbon/carbonbuttons.pp');
    P.Sources.AddSrc('carbon/carboncalendar.pas');
    P.Sources.AddSrc('carbon/carboncalendarview.pas');
    P.Sources.AddSrc('carbon/carboncanvas.pp');
    P.Sources.AddSrc('carbon/carboncaret.pas');
    P.Sources.AddSrc('carbon/carbonclipboard.pp');
    P.Sources.AddSrc('carbon/carbondbgconsts.pp');
    P.Sources.AddSrc('carbon/carbondebug.pp');
    P.Sources.AddSrc('carbon/carbondef.pp');
    P.Sources.AddSrc('carbon/carbonedits.pp');
    P.Sources.AddSrc('carbon/carbongdiobjects.pp');
    P.Sources.AddSrc('carbon/carbonint.pas');
    P.Sources.AddSrc('carbon/carbonlistviews.pp');
    P.Sources.AddSrc('carbon/carbonmenus.pp');
    P.Sources.AddSrc('carbon/carbonprivate.pp');
    P.Sources.AddSrc('carbon/carbonproc.pp');
    P.Sources.AddSrc('carbon/carbonstrings.pp');
    P.Sources.AddSrc('carbon/carbontabs.pp');
    P.Sources.AddSrc('carbon/carbonthemes.pas');
    P.Sources.AddSrc('carbon/carbonutils.pas');
    P.Sources.AddSrc('carbon/carbonwsbuttons.pp');
    P.Sources.AddSrc('carbon/carbonwscalendar.pp');
    P.Sources.AddSrc('carbon/carbonwschecklst.pp');
    P.Sources.AddSrc('carbon/carbonwscomctrls.pp');
    P.Sources.AddSrc('carbon/carbonwscontrols.pp');
    P.Sources.AddSrc('carbon/carbonwsdialogs.pp');
    P.Sources.AddSrc('carbon/carbonwsextctrls.pp');
    P.Sources.AddSrc('carbon/carbonwsextdlgs.pp');
    P.Sources.AddSrc('carbon/carbonwsfactory.pas');
    P.Sources.AddSrc('carbon/carbonwsforms.pp');
    P.Sources.AddSrc('carbon/carbonwsgrids.pp');
    P.Sources.AddSrc('carbon/carbonwsimglist.pp');
    P.Sources.AddSrc('carbon/carbonwsmenus.pp');
    P.Sources.AddSrc('carbon/carbonwspairsplitter.pp');
    P.Sources.AddSrc('carbon/carbonwsspin.pp');
    P.Sources.AddSrc('carbon/carbonwsstdctrls.pp');
    P.Sources.AddSrc('carbon/glgrab.pas');
    P.Sources.AddSrc('carbon/interfaces.pas');
    P.Sources.AddSrc('carbon/opengl.pas');
    P.Sources.AddSrc('cocoa/alllclintfunits.pas');
    P.Sources.AddSrc('cocoa/cocoagdiobjects.pas');
    P.Sources.AddSrc('cocoa/cocoaint.pas');
    P.Sources.AddSrc('cocoa/cocoaprivate.pp');
    P.Sources.AddSrc('cocoa/cocoautils.pas');
    P.Sources.AddSrc('cocoa/cocoawscommon.pas');
    P.Sources.AddSrc('cocoa/cocoawsextctrls.pas');
    P.Sources.AddSrc('cocoa/cocoawsfactory.pas');
    P.Sources.AddSrc('cocoa/cocoawsforms.pp');
    P.Sources.AddSrc('cocoa/cocoawsmenus.pas');
    P.Sources.AddSrc('cocoa/cocoawsstdctrls.pp');
    P.Sources.AddSrc('cocoa/interfaces.pas');
    P.Sources.AddSrc('fpgui/alllclintfunits.pas');
    P.Sources.AddSrc('fpgui/fpguiint.pp');
    P.Sources.AddSrc('fpgui/fpguiobjects.pas');
    P.Sources.AddSrc('fpgui/fpguiproc.pas');
    P.Sources.AddSrc('fpgui/fpguiwsbuttons.pp');
    P.Sources.AddSrc('fpgui/fpguiwscalendar.pp');
    P.Sources.AddSrc('fpgui/fpguiwscomctrls.pp');
    P.Sources.AddSrc('fpgui/fpguiwscontrols.pp');
    P.Sources.AddSrc('fpgui/fpguiwsdialogs.pp');
    P.Sources.AddSrc('fpgui/fpguiwsextctrls.pp');
    P.Sources.AddSrc('fpgui/fpguiwsextdlgs.pp');
    P.Sources.AddSrc('fpgui/fpguiwsfactory.pas');
    P.Sources.AddSrc('fpgui/fpguiwsforms.pp');
    P.Sources.AddSrc('fpgui/fpguiwsgrids.pp');
    P.Sources.AddSrc('fpgui/fpguiwsimglist.pp');
    P.Sources.AddSrc('fpgui/fpguiwsmenus.pp');
    P.Sources.AddSrc('fpgui/fpguiwspairsplitter.pp');
    P.Sources.AddSrc('fpgui/fpguiwsprivate.pp');
    P.Sources.AddSrc('fpgui/fpguiwsstdctrls.pp');
    P.Sources.AddSrc('fpgui/interfaces.pp');
    P.Sources.AddSrc('gtk/alllclintfunits.pas');
    P.Sources.AddSrc('gtk/gtk1def.pp');
    P.Sources.AddSrc('gtk/gtk1int.pp');
    P.Sources.AddSrc('gtk/gtk1wsprivate.pp');
    P.Sources.AddSrc('gtk/gtkdebug.pp');
    P.Sources.AddSrc('gtk/gtkdef.pp');
    P.Sources.AddSrc('gtk/gtkextra.pp');
    P.Sources.AddSrc('gtk/gtkfontcache.pas');
    P.Sources.AddSrc('gtk/gtkglobals.pp');
    P.Sources.AddSrc('gtk/gtkint.pp');
    P.Sources.AddSrc('gtk/gtkmsgqueue.pp');
    P.Sources.AddSrc('gtk/gtkproc.pp');
    P.Sources.AddSrc('gtk/gtkthemes.pas');
    P.Sources.AddSrc('gtk/gtkwinapiwindow.pp');
    P.Sources.AddSrc('gtk/gtkwsbuttons.pp');
    P.Sources.AddSrc('gtk/gtkwscalendar.pp');
    P.Sources.AddSrc('gtk/gtkwschecklst.pp');
    P.Sources.AddSrc('gtk/gtkwscomctrls.pp');
    P.Sources.AddSrc('gtk/gtkwscontrols.pp');
    P.Sources.AddSrc('gtk/gtkwsdialogs.pp');
    P.Sources.AddSrc('gtk/gtkwsextctrls.pp');
    P.Sources.AddSrc('gtk/gtkwsextdlgs.pp');
    P.Sources.AddSrc('gtk/gtkwsfactory.pas');
    P.Sources.AddSrc('gtk/gtkwsforms.pp');
    P.Sources.AddSrc('gtk/gtkwsgrids.pp');
    P.Sources.AddSrc('gtk/gtkwsimglist.pp');
    P.Sources.AddSrc('gtk/gtkwsmenus.pp');
    P.Sources.AddSrc('gtk/gtkwspairsplitter.pp');
    P.Sources.AddSrc('gtk/gtkwsprivate.pp');
    P.Sources.AddSrc('gtk/gtkwsspin.pp');
    P.Sources.AddSrc('gtk/gtkwsstdctrls.pp');
    P.Sources.AddSrc('gtk/interfaces.pp');
    P.Sources.AddSrc('gtk2/alllclintfunits.pas');
    P.Sources.AddSrc('gtk2/gtk2cellrenderer.pas');
    P.Sources.AddSrc('gtk2/gtk2debug.pp');
    P.Sources.AddSrc('gtk2/gtk2def.pp');
    P.Sources.AddSrc('gtk2/gtk2extra.pp');
    P.Sources.AddSrc('gtk2/gtk2fontcache.pas');
    P.Sources.AddSrc('gtk2/gtk2globals.pp');
    P.Sources.AddSrc('gtk2/gtk2int.pas');
    P.Sources.AddSrc('gtk2/gtk2listviewtreemodel.pas');
    P.Sources.AddSrc('gtk2/gtk2msgqueue.pp');
    P.Sources.AddSrc('gtk2/gtk2proc.pp');
    P.Sources.AddSrc('gtk2/gtk2themes.pas');
    P.Sources.AddSrc('gtk2/gtk2winapiwindow.pp');
    P.Sources.AddSrc('gtk2/gtk2windows.pas');
    P.Sources.AddSrc('gtk2/gtk2wsbuttons.pp');
    P.Sources.AddSrc('gtk2/gtk2wscalendar.pp');
    P.Sources.AddSrc('gtk2/gtk2wschecklst.pp');
    P.Sources.AddSrc('gtk2/gtk2wscomctrls.pp');
    P.Sources.AddSrc('gtk2/gtk2wscontrols.pp');
    P.Sources.AddSrc('gtk2/gtk2wsdialogs.pp');
    P.Sources.AddSrc('gtk2/gtk2wsextctrls.pp');
    P.Sources.AddSrc('gtk2/gtk2wsextdlgs.pp');
    P.Sources.AddSrc('gtk2/gtk2wsfactory.pas');
    P.Sources.AddSrc('gtk2/gtk2wsforms.pp');
    P.Sources.AddSrc('gtk2/gtk2wsgrids.pp');
    P.Sources.AddSrc('gtk2/gtk2wsimglist.pp');
    P.Sources.AddSrc('gtk2/gtk2wsmenus.pp');
    P.Sources.AddSrc('gtk2/gtk2wspairsplitter.pp');
    P.Sources.AddSrc('gtk2/gtk2wsprivate.pp');
    P.Sources.AddSrc('gtk2/gtk2wsspin.pp');
    P.Sources.AddSrc('gtk2/gtk2wsstdctrls.pp');
    P.Sources.AddSrc('gtk2/interfaces.pas');
    P.Sources.AddSrc('win32/alllclintfunits.pas');
    P.Sources.AddSrc('win32/interfaces.pp');
    P.Sources.AddSrc('win32/win32debug.pp');
    P.Sources.AddSrc('win32/win32def.pp');
    P.Sources.AddSrc('win32/win32extra.pas');
    P.Sources.AddSrc('win32/win32int.pp');
    P.Sources.AddSrc('win32/win32proc.pp');
    P.Sources.AddSrc('win32/win32themes.pas');
    P.Sources.AddSrc('win32/win32wsbuttons.pp');
    P.Sources.AddSrc('win32/win32wscalendar.pp');
    P.Sources.AddSrc('win32/win32wschecklst.pp');
    P.Sources.AddSrc('win32/win32wscomctrls.pp');
    P.Sources.AddSrc('win32/win32wscontrols.pp');
    P.Sources.AddSrc('win32/win32wsdialogs.pp');
    P.Sources.AddSrc('win32/win32wsextctrls.pp');
    P.Sources.AddSrc('win32/win32wsextdlgs.pp');
    P.Sources.AddSrc('win32/win32wsfactory.pas');
    P.Sources.AddSrc('win32/win32wsforms.pp');
    P.Sources.AddSrc('win32/win32wsgrids.pp');
    P.Sources.AddSrc('win32/win32wsimglist.pp');
    P.Sources.AddSrc('win32/win32wsmenus.pp');
    P.Sources.AddSrc('win32/win32wspairsplitter.pp');
    P.Sources.AddSrc('win32/win32wsspin.pp');
    P.Sources.AddSrc('win32/win32wsstdctrls.pp');
    P.Sources.AddSrc('win32/win32wstoolwin.pp');
    P.Sources.AddSrc('wince/interfaces.pp');
    P.Sources.AddSrc('wince/win32compat.pas');
    P.Sources.AddSrc('wince/wincedef.pp');
    P.Sources.AddSrc('wince/winceextra.pp');
    P.Sources.AddSrc('wince/winceint.pp');
    P.Sources.AddSrc('wince/winceproc.pp');
    P.Sources.AddSrc('wince/wincewsbuttons.pp');
    P.Sources.AddSrc('wince/wincewscalendar.pp');
    P.Sources.AddSrc('wince/wincewschecklst.pp');
    P.Sources.AddSrc('wince/wincewscomctrls.pp');
    P.Sources.AddSrc('wince/wincewscontrols.pp');
    P.Sources.AddSrc('wince/wincewsdialogs.pp');
    P.Sources.AddSrc('wince/wincewsextctrls.pp');
    P.Sources.AddSrc('wince/wincewsfactory.pas');
    P.Sources.AddSrc('wince/wincewsforms.pp');
    P.Sources.AddSrc('wince/wincewsgrids.pp');
    P.Sources.AddSrc('wince/wincewsimglist.pp');
    P.Sources.AddSrc('wince/wincewsmenus.pp');
    P.Sources.AddSrc('wince/wincewsspin.pp');
    P.Sources.AddSrc('wince/wincewsstdctrls.pp');
    P.Sources.AddSrc('wince/winext.pas');
    P.Sources.AddSrc('wince/alllclintfunits.pas');
    P.Sources.AddSrc('qt/alllclintfunits.pas');
    P.Sources.AddSrc('qt/interfaces.pp');
    P.Sources.AddSrc('qt/qt4.pas');
    P.Sources.AddSrc('qt/qt45.pas');
    P.Sources.AddSrc('qt/qtcaret.pas');
    P.Sources.AddSrc('qt/qtint.pp');
    P.Sources.AddSrc('qt/qtobjects.pas');
    P.Sources.AddSrc('qt/qtprivate.pp');
    P.Sources.AddSrc('qt/qtproc.pp');
    P.Sources.AddSrc('qt/qtthemes.pas');
    P.Sources.AddSrc('qt/qtwidgets.pas');
    P.Sources.AddSrc('qt/qtwsbuttons.pp');
    P.Sources.AddSrc('qt/qtwscalendar.pp');
    P.Sources.AddSrc('qt/qtwschecklst.pp');
    P.Sources.AddSrc('qt/qtwscomctrls.pp');
    P.Sources.AddSrc('qt/qtwscontrols.pp');
    P.Sources.AddSrc('qt/qtwsdesigner.pp');
    P.Sources.AddSrc('qt/qtwsdialogs.pp');
    P.Sources.AddSrc('qt/qtwsextctrls.pp');
    P.Sources.AddSrc('qt/qtwsextdlgs.pp');
    P.Sources.AddSrc('qt/qtwsfactory.pas');
    P.Sources.AddSrc('qt/qtwsforms.pp');
    P.Sources.AddSrc('qt/qtwsgrids.pp');
    P.Sources.AddSrc('qt/qtwsimglist.pp');
    P.Sources.AddSrc('qt/qtwsmenus.pp');
    P.Sources.AddSrc('qt/qtwspairsplitter.pp');
    P.Sources.AddSrc('qt/qtwsspin.pp');
    P.Sources.AddSrc('qt/qtwsstdctrls.pp');
    P.Sources.AddSrc('cocoa/cocoawsbuttons.pp');
    P.Sources.AddSrc('customdrawn/customdrawn_winproc.pas');
    P.Sources.AddSrc('customdrawn/alllclintfunits.pas');
    P.Sources.AddSrc('customdrawn/customdrawnint.pas');
    P.Sources.AddSrc('customdrawn/customdrawnwscontrols.pp');
    P.Sources.AddSrc('customdrawn/customdrawnwsfactory.pas');
    P.Sources.AddSrc('customdrawn/customdrawnwsforms.pp');
    P.Sources.AddSrc('customdrawn/interfaces.pas');
    P.Sources.AddSrc('customdrawn/customdrawnwsforms_win.inc');
    P.Sources.AddSrc('customdrawn/customdrawnwsforms_x11.inc');
    P.Sources.AddSrc('customdrawn/customdrawnobject_x11.inc');
    P.Sources.AddSrc('customdrawn/cocoagdiobjects.pas');
    P.Sources.AddSrc('customdrawn/customdrawn_cocoaproc.pas');
    P.Sources.AddSrc('customdrawn/cocoautils.pas');
    P.Sources.AddSrc('customdrawn/customdrawnproc.pas');
    P.Sources.AddSrc('customdrawn/customdrawn_x11proc.pas');
    P.Sources.AddSrc('customdrawn/customdrawn_androidproc.pas');
    P.Sources.AddSrc('customdrawn/android/android_native_app_glue.pas');
    P.Sources.AddSrc('customdrawn/android/asset_manager.pas');
    P.Sources.AddSrc('customdrawn/android/configuration.pas');
    P.Sources.AddSrc('customdrawn/android/egl.pas');
    P.Sources.AddSrc('customdrawn/android/gles.pas');
    P.Sources.AddSrc('customdrawn/android/input.pas');
    P.Sources.AddSrc('customdrawn/android/jni.pas');
    P.Sources.AddSrc('customdrawn/android/keycodes.pas');
    P.Sources.AddSrc('customdrawn/android/log.pas');
    P.Sources.AddSrc('customdrawn/android/looper.pas');
    P.Sources.AddSrc('customdrawn/android/native_activity.pas');
    P.Sources.AddSrc('customdrawn/android/native_window.pas');
    P.Sources.AddSrc('customdrawn/android/rect.pas');
    P.Sources.AddSrc('customdrawn/android/bitmap.pas');
    P.Sources.AddSrc('customdrawn/customdrawnwsstdctrls.pas');
    P.Sources.AddSrc('customdrawn/customdrawnwscomctrls.pas');
    P.Sources.AddSrc('customdrawn/customdrawnwsextctrls.pas');
    P.Sources.AddSrc('customdrawn/customdrawnprivate.pas');
    P.Sources.AddSrc('cocoa/cocoacaret.pas');
    P.Sources.AddSrc('customdrawn/customdrawnwslazdeviceapis.pas');
    P.Sources.AddSrc('nogui/alllclintfunits.pas');
    P.Sources.AddSrc('nogui/interfaces.pp');
    P.Sources.AddSrc('nogui/noguiint.pp');
    P.Sources.AddSrc('nogui/noguiwsfactory.pas');
    P.Sources.AddSrc('customdrawn/customdrawnwsspin.pas');
    P.Sources.AddSrc('customdrawn/customdrawnwsbuttons.pas');
    P.Sources.AddSrc('customdrawn/customdrawnwsdialogs.pas');
    P.Sources.AddSrc('customdrawn/customdrawnwsmenus.pas');
    P.Sources.AddSrc('gtk2/gtk2disableliboverlay.pas');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_LCL;
  Installer.Run;
end.
{$endif ALLPACKAGES}
