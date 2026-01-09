{
 *****************************************************************************
  This file is part of the SynEditSpellCheckerDsgn package from the Lazarus IDE.

  This content of this file is licensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 or 2.0 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
  Used units may have to be amended according to the choice.
 *****************************************************************************
}
unit SynSpellCheckDsgnRegister;

{$mode objfpc}{$H+}

interface

uses
  // FCL, LCL
  Classes, SysUtils, Graphics,
  // IdeIntf
  SrcEditorIntf, EditorSyntaxHighlighterDef,
  // SynEdit, LazEdit
  LazEditTextAttributes,
  SynEdit, SynEditMiscClasses, SynEditHighlighter,
  // SynSpellChecker
  SynSpellCheckPlugin, SynSpellCheckWordBreaker, SynSpellDictionary,
  // SynSpellCheckerDsgn
  syn_spell_options, SynSpellCheckDsgnStrings, SynSpellCheckDsgnOptions;

procedure Register;

implementation

type

 { TSynSpellIdeEventsHandler }

 TSynSpellIdeEventsHandler = class
  procedure DoNewEditor(Sender: TObject);
  procedure DoEditorConf(Sender: TObject);
  procedure DoRegisterAttribs(Sender: TObject);
end;

var
  SpellErrorAttrib: TSynSelectedColor;

procedure Register;
begin
  syn_spell_options.Register; // load config
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorCreate, @TSynSpellIdeEventsHandler(nil).DoNewEditor);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorCloned, @TSynSpellIdeEventsHandler(nil).DoNewEditor);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorReConfigured, @TSynSpellIdeEventsHandler(nil).DoEditorConf);
end;

procedure RegisterAttribs;
var
  AttribGroupIdx, i: Integer;
begin
  SpellErrorAttrib  := TSynSelectedColor.Create('', '', []);
  SpellErrorAttrib.Clear;
  SpellErrorAttrib.FrameColor := clRed;
  SpellErrorAttrib.FrameEdges := sfeBottom;
  SpellErrorAttrib.FrameStyle := slsWaved;
  SpellErrorAttrib.Features  := [];
  SpellErrorAttrib.InternalSaveDefaultValues;

  // Register colors before the IDE loads them
  AttribGroupIdx := IdeColorSchemeList.RegisterAttributeGroup(@SynSpellOptSpellChecking);
  for i := 0 to IdeSyntaxHighlighters.Count - 1 do
    IdeColorSchemeList.AddAttribute(AttribGroupIdx, i, 'SynSpellerMarkupError',
      @AttribNameSpellError, [hafBackColor..hafFrameEdges], SpellErrorAttrib);
end;

procedure FreeAttribs;
begin
  SpellErrorAttrib.Free;
end;

function TheDictionary: TSynSpellDictionaryASpell;
const
  GlobalDict: TSynSpellDictionaryASpell = nil;
begin
  if GlobalDict = nil then
    GlobalDict := TSynSpellDictionaryASpell.Create;
  Result := GlobalDict;
end;

procedure ApplySettings(ASrcEdit: TSourceEditorInterface);
var
  hl: TSynCustomHighlighter;
  cs: IColorScheme;
  csl: IColorSchemeLanguage;
  attr: IColorSchemeAttribute;
  p: TSynCustomPluginSpellCheck;
  i: Integer;
begin
  TheDictionary.Assign(SynSpellOptions.ASpellOpts);

  hl := TSynEdit(ASrcEdit.EditorControl).Highlighter;
  attr := nil;
  if hl <> nil then begin
    cs := IdeColorSchemeList.GetCurrentSchemeForHighlighter(hl);
    csl := cs.GetLanguageForHighlighter(hl);
    attr := csl.GetAttributeIntf('SynSpellerMarkupError');
  end;

  i := TSynEdit(ASrcEdit.EditorControl).PluginCount - 1;
  while (i >= 0) and not (TSynEdit(ASrcEdit.EditorControl).Plugin[i] is TSynCustomPluginSpellCheck) do
    dec(i);
  if i >= 0 then begin
    p := TSynCustomPluginSpellCheck(TSynEdit(ASrcEdit.EditorControl).Plugin[i]);
    if attr <> nil then
      attr.ApplyTo(p.MarkupInfo);

    p.MouseActions.Clear;
    if SynSpellOptions.EnablePopupMenu then begin
      p.MouseActions.AddCmdSuggestion(SynSpellOptions.MouseButton,
        SynSpellOptions.MouseShift, SynSpellOptions.MouseShiftMask);
    end;
    p.WordChecker.Assign(SynSpellOptions.CheckerOpts);
    (p.WordBreaker as TSynSpellWordBreakerSimpleUtf8).SpecialLetters :=
      SynSpellOptions.CheckerOpts.SpecialUpperLetters + SynSpellOptions.CheckerOpts.SpecialLowerLetters;
  end;
end;

{ TSynSpellIdeEventsHandler }

procedure TSynSpellIdeEventsHandler.DoNewEditor(Sender: TObject);
var
  p: TSynCustomPluginSpellCheck;
begin
  p := TSynCustomPluginSpellCheck.Create(TSourceEditorInterface(Sender).EditorControl as TSynEdit);
  p.Setup(TSynSpellWordBreakerSimpleUtf8, TSynSpellWordCheckerSourceCode, TheDictionary);
  ApplySettings(TSourceEditorInterface(Sender));
end;

procedure TSynSpellIdeEventsHandler.DoEditorConf(Sender: TObject);
begin
  ApplySettings(TSourceEditorInterface(Sender));
end;

procedure TSynSpellIdeEventsHandler.DoRegisterAttribs(Sender: TObject);
begin
  RegisterAttribs;
end;

initialization
  RegisterOnIdeColorSchemeListCreated(@TSynSpellIdeEventsHandler(nil).DoRegisterAttribs);

finalization
  FreeAttribs;
  TheDictionary.ReleaseReference;

end.

