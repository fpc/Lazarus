unit pocheckerconsts;

{$mode objfpc}{$H+}

interface

resourcestring
  //Main form
  rsPoChecker = 'PO File Checker';
  sSelectBasicTests = 'Select basic tests';
  sGUIPoFileCheckingTool = 'GUI Po-file checking tool';
  sSelectTestTypes = 'Select test types';
  sOpenAPoFile = '&Open a po-file';
  sRunSelectedTests = '&Run Selected Tests';
  sCannotFindMaster = 'Cannot find master po file:' + LineEnding + '%s' + LineEnding + 'for selected file' + LineEnding + '%s';
  sNotAProperFileName = 'Selected filename' + LineEnding + '%s' + LineEnding + 'does not seem to be a proper name for a po-file';
  sErrorOnCreate = 'Error creating an instance of TPoFamily:' + LineEnding + '%s';
  sErrorOnCleanup = 'An unrecoverable error occurred' + LineEnding + '%s' + LineEnding + 'Please close the program';

  sTotalErrors = 'Total errors / warnings found: %d';
  sTotalWarnings = 'Total warnings found: %d';
  sNoErrorsFound = 'No errors found';
  sCurrentTest = 'Current Test:';
  sCurrentPoFile = 'Current po-file:';
  sNoTestSelected = 'There are no tests selected.';

  //Result form
  sSaveError = 'Error saving file:' + LineEnding + '%s';
  sSaveCaption = 'Save to file';
  sResults = 'Results';
  sCopyCaption = 'Copy to clipboard';

  //PoFamiles
  sOriginal = 'Original';
  sTranslation = 'Translation';
  sErrorsByTest = 'Errors / warnings reported by %s for:';
  sTranslationStatistics = 'Translation statistics for:';
  sCheckNumberOfItems = 'Check number of items';
  sCheckForIncompatibleFormatArguments = 'Check for incompatible format '
    +'arguments';
  sCheckMissingIdentifiers = 'Check missing identifiers';
  sCheckForMismatchesInUntranslatedStrings = 'Check for mismatches in '
    +'untranslated strings';
  sCheckForDuplicateUntranslatedValues = 'Check for duplicate untranslated '
    +'values';
  sCheckStatistics = 'Check percentage of (un)translated and fuzzy strings';
  sFindAllTranslatedPoFiles = 'Find all translated po-files';
  sIncompatibleFormatArgs = '[Line: %d] Incompatible and/or invalid format() arguments for:' ;

  sNrErrorsFound = 'Found %d errors.';
  sNrWarningsFound = 'Found %d warnings.';
  sLineInFileName = '[Line %d] in %s:';
  sIdentifierNotFoundIn = 'Identifier [%s] not found in %s';
  sMissingMasterIdentifier = 'Identifier [%s] found in %s, but it does not exist in %s';
  sLineNr = '[Line: %d]';

  sNrOfItemsMisMatch = 'Mismatch in number of items for master and child';
  sNrOfItemsMismatchD = '%s: %d items';

  sDuplicateOriginals = 'The (untranslated) value "%s" is used for more than 1 entry:';

  sDuplicateLineNrWithValue = '[Line %d] %s';
  sPercTranslated = 'Translated strings: %2.0f%%.';
  sPercUntranslated = 'Untranslated strings: %2.0f%%.';
  sPercFuzzy = 'Fuzzy strings: %2.0f%%.';
implementation

end.

