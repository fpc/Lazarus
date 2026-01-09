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
unit SynSpellCheckDsgnStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  SynSpellOptSpellChecking = 'Spell Checking';
  AttribNameSpellError = 'Misspelled token';

  SynSpellOptDictionary = 'Dictionary';
  SynSpellOptLibraryNameAndPath = 'Library name (and path):';
  SynSpellOptDictionaryPath = 'Dictionary path:';
  SynSpellOptLanguage = 'Language:';
  SynSpellOptCurrentLoadedLibraryIsSRe = 'Current loaded library is "%s". Restart the IDE to '
    +'change.';
  SynSpellOptPersonalDictionaryFile = 'Personal dictionary file';
  SynSpellOptMouse = 'Mouse';
  SynSpellOptSuggestionAndAddToPersona = 'Suggestion and add to personal dictionary';
  SynSpellOptLeft = 'Left';
  SynSpellOptMiddle = 'Middle';
  SynSpellOptRight = 'Right';
  SynSpellOptSyntaxRules = 'Syntax rules';
  SynSpellOptWordsAndWordPartsSplitAtC = 'Words and word-parts (split at CamelCase, change to '
    +'uppercase';
  SynSpellOptUnicodeUpperChars = 'Unicode upper chars';
  SynSpellOptUpperCharsOtherThanAZUsed = 'Upper chars other than A-Z. Used to determine word '
    +'boundaries. Must be known by the dictionary.';
  SynSpellOptUnicodeLowerChars = 'Unicode lower chars';
  SynSpellOptLowerCharsOtherThanAZUsed = 'Lower chars other than a-z. Used to determine word '
    +'boundaries. Must be known by the dictionary.';
  SynSpellOptAlsoCheckTheWholeWordEven = 'Also check the whole word even if it has parts';
  SynSpellOptCheckWordsWithSeveralLead = 'Check words with several leading capital chars';
  SynSpellOptIgnoreWordsUpToChars = 'Ignore words up to chars';
  SynSpellOptAlsoIgnoreWordsWithSevera = 'Also ignore words with several leading capital chars';
  SynSpellOptIgnoreWordPartsUpToChars = 'Ignore word parts up to chars';
  SynSpellOptAlsoCheckWordPartsWithSev =
    'Also check word parts with several leading capital chars';
  SynSpellOptAlsoIgnoreWordPartsWithSe = 'Also ignore word parts with several leading capital '
    +'chars';
  SynSpellOptIgnoreLeadingLowercaseCha = 'Ignore leading lowercase chars (up to len)';
  SynSpellOptSubPartsSOMEThingUpperLea = 'Sub-parts: SOMEThing (upper lead + capitalized word)';
  SynSpellOptTryToFindValidWordsBySpli = 'Try to find valid words by splitting before the last '
    +'upper-char. Then spell check the "UPPER LEAD" and "Capitalized Word".';
  SynSpellOptRequireMinimumLenForLEAD = 'Require minimum len for LEAD';
  SynSpellOptIgnoreLEADUpToLen = 'Ignore LEAD up to len';
  SynSpellOptOnlyIgnoreLEADIfCapitaliz = 'Only ignore LEAD, if Capitalize has min len';
  SynSpellOptRequireMinimumLenForCapit = 'Require minimum len for Capitalized Word';
  SynSpellOptIgnoreCapitalizedUpToLen = 'Ignore Capitalized up to len';
  SynSpellOptOnlyIgnoreCapitalizedIfLE = 'Only ignore Capitalized, if LEAD has min len';
  SynSpellOptSubPartsUPPERlower = 'Sub-Parts: UPPERlower';
  SynSpellOptTryToFindValidWordsBySpli2 = 'Try to find valid words by splitting after the last '
    +'upper-char. Then spell check the "UPPER WORD" and "lower word".';
  SynSpellOptRequireMinimumLenForUPPER = 'Require minimum len for UPPER PART';
  SynSpellOptIgnoreUPPERUpToLen = 'Ignore UPPER up to len';
  SynSpellOptOnlyIgnoreUPPERIfLowerHas = 'Only ignore UPPER, if lower has min len';
  SynSpellOptRequireMinimumLenForLower = 'Require minimum len for lower part';
  SynSpellOptIgnoreLowerUpToLen = 'Ignore lower up to len';
  SynSpellOptOnlyIgnoreLowerIfUPPERHas = 'Only ignore lower, if UPPER has min len';
  SynSpellOptStripPrefix = 'Strip prefix';
  SynSpellOptIgnoreArticleAAnIfStartin = 'Ignore article (A/An), if starting uppercase ';
  SynSpellOptOnlyIgnoreUpperArticlesIf = 'Only ignore upper articles if next char is also upper';
  SynSpellOptIgnoreArticleAAnIfStartin2 = 'Ignore article (a/an), if starting lowercase';
  SynSpellOptOnlyIgnoreLowerArticlesIf = 'Only ignore lower articles if next char is also lower';
  SynSpellOptListOfCharsPrefixToIgnore = 'List of chars (prefix) to ignore at the start of the '
    +'word';
  SynSpellOptOnlyIgnoreUpperPrefixChar = 'Only ignore upper prefix chars, if next char is also '
    +'upper';
  SynSpellOptOnlyIgnoreLowerPrefixChar = 'Only ignore lower prefix chars, if next char is also '
    +'lower';
  SynSpellOptOnlyRemovePrefixArticleIf = 'Only remove prefix/article if the first word-part has '
    +'min len';


implementation

end.

