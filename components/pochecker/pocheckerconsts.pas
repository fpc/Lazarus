unit pocheckerconsts;

{$mode objfpc}{$H+}

interface

Uses Controls;

resourcestring
  //Main form
  rsPoChecker = 'Check PO Files ...';
  sSelectAllTests = 'Select &All';
  sUnselectAllTests = '&Unselect All';
  sGUIPoFileCheckingTool = 'GUI Po-file checking tool';
  sSelectTestTypes = 'Select test types';
  sScanDir = 'Scan a folder';
  sRunSelectedTests = '&Run Selected Tests';
  sClearListBox = 'Clear';
  sUnselectListBox = 'Unselect all files';
  sSelectAllListBox = 'Select all files';
  sAllLanguages = 'All Languages';
  sFilesNotFoundAndRemoved = 'The following non-existent files were removed from the list:' + LineEnding + '%s';
  sNoFilesLeftToCheck = 'There are no files left to check.';
  sErrorOnCreate = 'Error creating an instance of TPoFamily:' + LineEnding + '%s';
  sErrorOnCleanup = 'An unrecoverable error occurred' + LineEnding + '%s' + LineEnding + 'Please close the program';

  sLastSearchPath = 'Last search path:';
  sLanguage = 'Language:';

  sTotalErrors = 'Total errors: %d';
  sTotalErrorsNonFuzzy = 'Total errors: %d (formatting errors in non-fuzzy strings: %d)';
  sTotalWarnings = 'Total warnings: %d';
  sTotalUntranslatedStrings = 'Total untranslated strings: %s';
  sTotalFuzzyStrings = 'Total fuzzy strings: %s';
  sTotalTranslatedStrings = 'Total translated strings: %s (%.1f%%)';
  sCurrentTest = 'Test: %s on %s';
  sNoTestSelected = 'There are no tests selected.';
  sScanningInProgress = 'Scanning in progress, please wait ...';

  //Result form
  sSaveError = 'Error saving file:' + LineEnding + '%s';
  sResults = 'Results';
  sShowStatGraph = 'Show statistics graph';
  sGeneralInfo = 'General Info';
  sTranslationStatistics = 'Translation Statistics';
  sDuplicateOriginalsTab = 'Duplicate Originals';
  sCopy = '&Copy';
  sSaveAs ='Save &As ...';

  //Graphical summary form
  sGrapStatFormCaption = 'Graphical summary';
  sTranslatedStringsTotal = 'Translated strings (total: %s [%.1f%%])';
  sUntranslatedStringsTotal = 'Untranslated strings (total: %s)';
  sFuzzyStringsTotal = 'Fuzzy strings (total: %s)';
  sTranslatedStrings = 'Translated strings';
  sUntranslatedStrings = 'Untranslated strings';
  sFuzzyStrings = 'Fuzzy strings';
  sStatHint = 'Translated: %d (%.1f%%)' + LineEnding +
              'Untranslated: %d (%.1f%%)' + LineEnding +
              'Fuzzy: %d (%.1f%%)' + LineEnding +
              'Errors in selected tests: %d';
  SOpenFail = 'Unable to open file:' + LineEnding +  '"%s"';
  SOpenFailExternal = 'Unable to open file' + LineEnding +
                      '"%s"' + LineEnding + 'in external editor' + LineEnding + '"%s"';
  sCreatingIconXofY = 'Creating icon nr. %d of %d';
  sOpenFileInSystemPOEditor = 'Open file in system PO editor';
  sOpenFileInExternalEditor = 'Open file in external editor';
  sOpenFileInIDEEditor = 'Open file in IDE editor';
  sRefreshCurrentTranslationFamily = 'Refresh current translation family';
  sRefreshAllTranslationFamilies = 'Refresh all translation families';
  sProcessingTranslationFamilyOf = 'Processing translation family %s of %s';
  sProcessingTranslationFamily = 'Processing translation family...';
  sCannotWriteFileYouCanPressRetryToRefreshThisTransl = 'Cannot write file%'
    +'s%s%s%sYou can press "Retry" to refresh this translation family again and '
    +'continue.';

  //PoFamiles
  sOriginal = 'Original';
  sTranslation = 'Translation';
  sErrorsByTest = 'Errors / warnings reported by %s for:';
  sFile = 'File %s';
  sCheckNumberOfItems = 'Check number of items';
  sCheckForIncompatibleFormatArguments = 'Check for incompatible format '
    +'arguments';
  sCheckMissingIdentifiers = 'Check missing identifiers';
  sCheckForMismatchesInUntranslatedStrings = 'Check for mismatches of originals';
  sIncompatibleFormatArgs = '[Line: %d] Incompatible and/or invalid format() arguments for:' ;

  sNrErrorsFound = 'Found %d errors.';
  sNrWarningsFound = 'Found %d warnings.';
  sLineInFileName = '[Line %d] in %s:';
  sIdentifierNotFoundIn = 'Identifier [%s] not found in %s';
  sMissingMasterIdentifier = 'Identifier [%s] found in %s, but it does not exist in %s';
  sLineNr = '[Line: %d]';
  sNoteTranslationIsFuzzy = 'Note: translation is fuzzy';


  sNrOfItemsMisMatch = 'Mismatch in number of items for master and child';
  sNrOfItemsMismatchD = '%s: %d items';

  sDuplicateOriginals = 'Original value "%s" is used for more than 1 entry:';

  sDuplicateLineNrWithValue = '[Line %d] %s';
  sPercTranslated = '%s: %5.1f%% translated strings.';
  sPercUntranslated = '%s: %5.1f%% untranslated strings.';
  sPercFuzzy = '%s: %5.1f%% fuzzy strings.';

  sTheFollowingOrphanedPoFileSFound = 'The following %s orphaned .po file(s) found:';
  sTheFollowingMissingMasterPoFileSWereRemoved = 'The following %s '
    +'missing master .po file(s) were removed from the list:';
  sTroublesomeFiles = 'Troublesome files';

const
  mrOpenEditorFile = mrNone+100;


type
  {
   Currently supported lanuages in Lazarus.
   Please keep alphabetically when introducing a new one
  }
  TLangID = (
    lang_all,    {All languages}
    lang_af_ZA,  {Afrikaans}
    lang_ar,     {Arabic}
    lang_ca,     {Catalan}
    lang_cs,     {Czech}
    lang_de,     {German}
    lang_en,     {English}
    lang_es,     {Spanish}
    lang_fi,     {Finnish}
    lang_fr,     {French}
    lang_he,     {Hebrew}
    lang_hu,     {Hungarian}
    lang_id,     {Indonesian}
    lang_it,     {Italian}
    lang_ja,     {Japanese}
    lang_lt,     {Lithuanian}
    lang_nl,     {Dutch}
    lang_pl,     {Polish}
    lang_pt,     {Portuguese}
    lang_pt_BR,  {Brazilian Portuguese}
    lang_ru,     {Russian}
    lang_sk,     {Slovak}
    lang_tr,     {Turkish}
    lang_uk,     {Ukrainian}
    lang_zh_CN   {Chinese, simplified}
    );

resourcestring
  rs_lang_all =    'All languages';
  rs_lang_af_ZA =  'Afrikaans';
  rs_lang_ar =     'Arabic';
  rs_lang_ca =     'Catalan';
  rs_lang_cs =     'Czech';
  rs_lang_de =     'German';
  rs_lang_en =     'English';
  rs_lang_es =     'Spanish';
  rs_lang_fi =     'Finnish';
  rs_lang_fr =     'French';
  rs_lang_he =     'Hebrew';
  rs_lang_hu =     'Hungarian';
  rs_lang_id =     'Indonesian';
  rs_lang_it =     'Italian';
  rs_lang_ja =     'Japanese';
  rs_lang_lt =     'Lithuanian';
  rs_lang_nl =     'Dutch';
  rs_lang_pl =     'Polish';
  rs_lang_pt =     'Portuguese';
  rs_lang_pt_BR =  'Brazilian Portuguese';
  rs_lang_ru =     'Russian';
  rs_lang_sk =     'Slovak';
  rs_lang_tr =     'Turkish';
  rs_lang_uk =     'Ukrainian';
  rs_lang_zh_CN =  'Chinese, simplified';

const
  LanguageNames: Array[TLangID] of String = (
    rs_lang_all ,
    rs_lang_af_ZA,
    rs_lang_ar ,
    rs_lang_ca ,
    rs_lang_cs ,
    rs_lang_de ,
    rs_lang_en ,
    rs_lang_es ,
    rs_lang_fi ,
    rs_lang_fr ,
    rs_lang_he ,
    rs_lang_hu ,
    rs_lang_id ,
    rs_lang_it ,
    rs_lang_ja ,
    rs_lang_lt ,
    rs_lang_nl ,
    rs_lang_pl ,
    rs_lang_pt ,
    rs_lang_pt_BR,
    rs_lang_ru ,
    rs_lang_sk ,
    rs_lang_tr ,
    rs_lang_uk ,
    rs_lang_zh_CN
    );

  LanguageAbbr: Array[TLangID] of String = (
     '',       {Defaut language} //in LangFilter: All Languages
     'af_ZA',  {Afrikaans}
     'ar',     {Arabic}
     'ca',     {Catalan}
     'cs',     {Czech}
     'de',     {German}
     'en',     {English}
     'es',     {Spanish}
     'fi',     {Finnish}
     'fr',     {French}
     'he',     {Hebrew}
     'hu',     {Hungarian}
     'id',     {Indonesian}
     'it',     {Italian}
     'ja',     {Japanese}
     'lt',     {Lithuanian}
     'nl',     {Dutch}
     'pl',     {Polish}
     'pt',     {Portuguese}
     'pt_BR',  {Brazilian Portuguese}
     'ru',     {Russian}
     'sk',     {Slovak}
     'tr',     {Turkish}
     'uk',     {Ukrainian}
     'zh_CN'   {Chinese, simplified}
     );

function LangAbbrToLangId(const Abbr: String): TLangID;
procedure LocalizeLanguageNames;

implementation

function LangAbbrToLangId(const Abbr: String): TLangID;
var
  ID: TLangID;
begin
  Result := lang_all;
  for ID := Low(TLangID) to High(TLangID) do
  begin
    if LanguageAbbr[ID] = Abbr then
      Exit(ID);
  end;
end;

procedure LocalizeLanguageNames;
begin
  LanguageNames[lang_all]:=rs_lang_all;
  LanguageNames[lang_af_ZA]:=rs_lang_af_ZA;
  LanguageNames[lang_ar]:=rs_lang_ar;
  LanguageNames[lang_ca]:=rs_lang_ca;
  LanguageNames[lang_cs]:=rs_lang_cs;
  LanguageNames[lang_de]:=rs_lang_de;
  LanguageNames[lang_en]:=rs_lang_en;
  LanguageNames[lang_es]:=rs_lang_es;
  LanguageNames[lang_fi]:=rs_lang_fi;
  LanguageNames[lang_fr]:=rs_lang_fr;
  LanguageNames[lang_he]:=rs_lang_he;
  LanguageNames[lang_hu]:=rs_lang_hu;
  LanguageNames[lang_id]:=rs_lang_id;
  LanguageNames[lang_it]:=rs_lang_it;
  LanguageNames[lang_ja]:=rs_lang_ja;
  LanguageNames[lang_lt]:=rs_lang_lt;
  LanguageNames[lang_nl]:=rs_lang_nl;
  LanguageNames[lang_pl]:=rs_lang_pl;
  LanguageNames[lang_pt]:=rs_lang_pt;
  LanguageNames[lang_pt_BR]:=rs_lang_pt_BR;
  LanguageNames[lang_ru]:=rs_lang_ru;
  LanguageNames[lang_sk]:=rs_lang_sk;
  LanguageNames[lang_tr]:=rs_lang_tr;
  LanguageNames[lang_uk]:=rs_lang_uk;
  LanguageNames[lang_zh_CN]:=rs_lang_zh_CN;
end;

end.

