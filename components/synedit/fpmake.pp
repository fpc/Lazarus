{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for SynEdit 1.0

   This file was generated on 29-12-18
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_SynEdit(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPackage('synedit');
    P.Version:='1.0';

    P.Directory:=ADirectory;

    P.Flags.Add('LazarusDsgnPkg');

    P.Dependencies.Add('fcl-registry');
    P.Dependencies.Add('regexpr');
    P.Dependencies.Add('lcl');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-CR');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.Options.Add('-vm5024,4055');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCLWidgetType)');
    P.Options.Add('-CR');
    P.Options.Add('-dgc');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('allsynedit.pas');
    t.Dependencies.AddUnit('SynBeautifier');
    t.Dependencies.AddUnit('SynCompletion');
    t.Dependencies.AddUnit('LazSynIMM');
    t.Dependencies.AddUnit('SynEdit');
    t.Dependencies.AddUnit('SynEditAutoComplete');
    t.Dependencies.AddUnit('SynEditExport');
    t.Dependencies.AddUnit('SynEditFoldedView');
    t.Dependencies.AddUnit('SynEditHighlighter');
    t.Dependencies.AddUnit('SynEditHighlighterFoldBase');
    t.Dependencies.AddUnit('SynEditHighlighterXMLBase');
    t.Dependencies.AddUnit('SynEditKeyCmds');
    t.Dependencies.AddUnit('LazSynEditMouseCmdsTypes');
    t.Dependencies.AddUnit('SynHighlighterPo');
    t.Dependencies.AddUnit('SynEditLines');
    t.Dependencies.AddUnit('SynEditMarks');
    t.Dependencies.AddUnit('SynEditMarkup');
    t.Dependencies.AddUnit('SynEditMarkupBracket');
    t.Dependencies.AddUnit('SynEditMarkupCtrlMouseLink');
    t.Dependencies.AddUnit('SynEditMarkupHighAll');
    t.Dependencies.AddUnit('SynEditMarkupSelection');
    t.Dependencies.AddUnit('SynEditMarkupSpecialLine');
    t.Dependencies.AddUnit('SynEditMarkupWordGroup');
    t.Dependencies.AddUnit('SynEditMiscClasses');
    t.Dependencies.AddUnit('SynEditMiscProcs');
    t.Dependencies.AddUnit('SynEditMouseCmds');
    t.Dependencies.AddUnit('SynEditPlugins');
    t.Dependencies.AddUnit('SynEditPointClasses');
    t.Dependencies.AddUnit('SynEditRegexSearch');
    t.Dependencies.AddUnit('SynEditSearch');
    t.Dependencies.AddUnit('SynEditStrConst');
    t.Dependencies.AddUnit('SynEditTextBase');
    t.Dependencies.AddUnit('SynEditTextBuffer');
    t.Dependencies.AddUnit('SynEditTextBidiChars');
    t.Dependencies.AddUnit('SynEditTextTabExpander');
    t.Dependencies.AddUnit('SynEditTextTrimmer');
    t.Dependencies.AddUnit('SynEditTypes');
    t.Dependencies.AddUnit('SynExportHTML');
    t.Dependencies.AddUnit('SynGutter');
    t.Dependencies.AddUnit('SynGutterBase');
    t.Dependencies.AddUnit('SynGutterChanges');
    t.Dependencies.AddUnit('SynGutterCodeFolding');
    t.Dependencies.AddUnit('SynGutterLineNumber');
    t.Dependencies.AddUnit('SynGutterLineOverview');
    t.Dependencies.AddUnit('SynGutterMarks');
    t.Dependencies.AddUnit('SynHighlighterAny');
    t.Dependencies.AddUnit('SynHighlighterCpp');
    t.Dependencies.AddUnit('SynHighlighterCss');
    t.Dependencies.AddUnit('SynHighlighterDiff');
    t.Dependencies.AddUnit('SynHighlighterHashEntries');
    t.Dependencies.AddUnit('SynHighlighterHTML');
    t.Dependencies.AddUnit('SynHighlighterJava');
    t.Dependencies.AddUnit('SynHighlighterJScript');
    t.Dependencies.AddUnit('SynHighlighterLFM');
    t.Dependencies.AddUnit('SynHighlighterMulti');
    t.Dependencies.AddUnit('SynHighlighterPas');
    t.Dependencies.AddUnit('SynHighlighterPerl');
    t.Dependencies.AddUnit('SynHighlighterPHP');
    t.Dependencies.AddUnit('SynHighlighterPosition');
    t.Dependencies.AddUnit('SynHighlighterPython');
    t.Dependencies.AddUnit('SynHighlighterSQL');
    t.Dependencies.AddUnit('SynHighlighterTeX');
    t.Dependencies.AddUnit('synhighlighterunixshellscript');
    t.Dependencies.AddUnit('SynHighlighterVB');
    t.Dependencies.AddUnit('SynHighlighterXML');
    t.Dependencies.AddUnit('SynMacroRecorder');
    t.Dependencies.AddUnit('SynMemo');
    t.Dependencies.AddUnit('SynPluginSyncroEdit');
    t.Dependencies.AddUnit('SynPluginSyncronizedEditBase');
    t.Dependencies.AddUnit('SynPluginTemplateEdit');
    t.Dependencies.AddUnit('LazSynEditText');
    t.Dependencies.AddUnit('LazSynTextArea');
    t.Dependencies.AddUnit('SynTextDrawer');
    t.Dependencies.AddUnit('SynEditMarkupGutterMark');
    t.Dependencies.AddUnit('SynHighlighterBat');
    t.Dependencies.AddUnit('SynHighlighterIni');
    t.Dependencies.AddUnit('SynEditMarkupSpecialChar');
    t.Dependencies.AddUnit('SynEditTextDoubleWidthChars');
    t.Dependencies.AddUnit('SynEditTextSystemCharWidth');
    t.Dependencies.AddUnit('SynEditMarkupIfDef');
    t.Dependencies.AddUnit('SynPluginMultiCaret');
    t.Dependencies.AddUnit('synhighlighterpike');
    t.Dependencies.AddUnit('SynEditMarkupFoldColoring');
    T := P.Targets.AddImplicitUnit('synbeautifier.pas');
    T := P.Targets.AddImplicitUnit('syncompletion.pas');
    T := P.Targets.AddImplicitUnit('lazsynimm.pas');
    T.OSes := [win32,win64];
    T := P.Targets.AddImplicitUnit('synedit.pp');
    T := P.Targets.AddImplicitUnit('syneditautocomplete.pp');
    T := P.Targets.AddImplicitUnit('syneditexport.pas');
    T := P.Targets.AddImplicitUnit('syneditfoldedview.pp');
    T := P.Targets.AddImplicitUnit('synedithighlighter.pp');
    T := P.Targets.AddImplicitUnit('synedithighlighterfoldbase.pas');
    T := P.Targets.AddImplicitUnit('synedithighlighterxmlbase.pas');
    T := P.Targets.AddImplicitUnit('syneditkeycmds.pp');
    T := P.Targets.AddImplicitUnit('lazsyneditmousecmdstypes.pp');
    T := P.Targets.AddImplicitUnit('synhighlighterpo.pp');
    T := P.Targets.AddImplicitUnit('syneditlines.pas');
    T := P.Targets.AddImplicitUnit('syneditmarks.pp');
    T := P.Targets.AddImplicitUnit('syneditmarkup.pp');
    T := P.Targets.AddImplicitUnit('syneditmarkupbracket.pp');
    T := P.Targets.AddImplicitUnit('syneditmarkupctrlmouselink.pp');
    T := P.Targets.AddImplicitUnit('syneditmarkuphighall.pp');
    T := P.Targets.AddImplicitUnit('syneditmarkupselection.pp');
    T := P.Targets.AddImplicitUnit('syneditmarkupspecialline.pp');
    T := P.Targets.AddImplicitUnit('syneditmarkupwordgroup.pp');
    T := P.Targets.AddImplicitUnit('syneditmiscclasses.pp');
    T := P.Targets.AddImplicitUnit('syneditmiscprocs.pp');
    T := P.Targets.AddImplicitUnit('syneditmousecmds.pp');
    T := P.Targets.AddImplicitUnit('syneditplugins.pas');
    T := P.Targets.AddImplicitUnit('syneditpointclasses.pas');
    T := P.Targets.AddImplicitUnit('syneditregexsearch.pas');
    T := P.Targets.AddImplicitUnit('syneditsearch.pp');
    T := P.Targets.AddImplicitUnit('syneditstrconst.pp');
    T := P.Targets.AddImplicitUnit('synedittextbase.pas');
    T := P.Targets.AddImplicitUnit('synedittextbuffer.pp');
    T := P.Targets.AddImplicitUnit('synedittextbidichars.pas');
    T := P.Targets.AddImplicitUnit('synedittexttabexpander.pas');
    T := P.Targets.AddImplicitUnit('synedittexttrimmer.pas');
    T := P.Targets.AddImplicitUnit('synedittypes.pp');
    T := P.Targets.AddImplicitUnit('synexporthtml.pas');
    T := P.Targets.AddImplicitUnit('syngutter.pp');
    T := P.Targets.AddImplicitUnit('syngutterbase.pp');
    T := P.Targets.AddImplicitUnit('syngutterchanges.pas');
    T := P.Targets.AddImplicitUnit('synguttercodefolding.pp');
    T := P.Targets.AddImplicitUnit('syngutterlinenumber.pp');
    T := P.Targets.AddImplicitUnit('syngutterlineoverview.pp');
    T := P.Targets.AddImplicitUnit('synguttermarks.pp');
    T := P.Targets.AddImplicitUnit('synhighlighterany.pas');
    T := P.Targets.AddImplicitUnit('synhighlightercpp.pp');
    T := P.Targets.AddImplicitUnit('synhighlightercss.pas');
    T := P.Targets.AddImplicitUnit('synhighlighterdiff.pas');
    T := P.Targets.AddImplicitUnit('synhighlighterhashentries.pas');
    T := P.Targets.AddImplicitUnit('synhighlighterhtml.pp');
    T := P.Targets.AddImplicitUnit('synhighlighterjava.pas');
    T := P.Targets.AddImplicitUnit('synhighlighterjscript.pas');
    T := P.Targets.AddImplicitUnit('synhighlighterlfm.pas');
    T := P.Targets.AddImplicitUnit('synhighlightermulti.pas');
    T := P.Targets.AddImplicitUnit('synhighlighterpas.pp');
    T := P.Targets.AddImplicitUnit('synhighlighterperl.pas');
    T := P.Targets.AddImplicitUnit('synhighlighterphp.pas');
    T := P.Targets.AddImplicitUnit('synhighlighterposition.pas');
    T := P.Targets.AddImplicitUnit('synhighlighterpython.pas');
    T := P.Targets.AddImplicitUnit('synhighlightersql.pas');
    T := P.Targets.AddImplicitUnit('synhighlightertex.pas');
    T := P.Targets.AddImplicitUnit('synhighlighterunixshellscript.pas');
    T := P.Targets.AddImplicitUnit('synhighlightervb.pas');
    T := P.Targets.AddImplicitUnit('synhighlighterxml.pas');
    T := P.Targets.AddImplicitUnit('synmacrorecorder.pas');
    T := P.Targets.AddImplicitUnit('synmemo.pas');
    T := P.Targets.AddImplicitUnit('synpluginsyncroedit.pp');
    T := P.Targets.AddImplicitUnit('synpluginsyncronizededitbase.pp');
    T := P.Targets.AddImplicitUnit('synplugintemplateedit.pp');
    T := P.Targets.AddImplicitUnit('lazsynedittext.pas');
    T := P.Targets.AddImplicitUnit('lazsyntextarea.pp');
    T := P.Targets.AddImplicitUnit('syntextdrawer.pp');
    T := P.Targets.AddImplicitUnit('syneditmarkupguttermark.pp');
    T := P.Targets.AddImplicitUnit('synhighlighterbat.pas');
    T := P.Targets.AddImplicitUnit('synhighlighterini.pas');
    T := P.Targets.AddImplicitUnit('syneditmarkupspecialchar.pp');
    T := P.Targets.AddImplicitUnit('synedittextdoublewidthchars.pas');
    T := P.Targets.AddImplicitUnit('synedittextsystemcharwidth.pas');
    T := P.Targets.AddImplicitUnit('syneditmarkupifdef.pp');
    T := P.Targets.AddImplicitUnit('synpluginmulticaret.pp');
    T := P.Targets.AddImplicitUnit('synhighlighterpike.pas');
    T := P.Targets.AddImplicitUnit('syneditmarkupfoldcoloring.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('SynEdit.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_SynEdit('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
