{ Copyright (C) 2006 Mattias Gaertner

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit H2PasStrConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  h2pCHeaderFileConverter = 'C header file converter';
  h2pH2Pas = 'H2Pas ...';
  h2pCreateUnitsFromCHeaderFiles = 'Create units from C header files';
  h2pH2PasTool = 'H2PasTool';
  h2pCHeaderFiles = 'C header files';
  h2pAddHFiles = 'Add .h files ...';
  h2pDeleteSelectedHFiles = 'Delete selected .h files';
  h2pEnableAllHFiles = 'Enable all .h files';
  h2pDisableAllHFiles = 'Disable all .h files';
  h2pMoveFileDown = 'Move file down';
  h2pMoveFileUp = 'Move file up';
  h2pFileInformation = 'File information';
  h2pAddIncludedHFiles = 'Add included .h files';
  h2pMergeAllButThis = 'Merge all but this';
  h2pMergeFile = 'Merge file';
  h2pH2pasOptions = 'H2Pas Options';
  h2pOptions = 'Options';
  h2pDUseExternalForAllProcedures = '-d  Use external; for all procedures';
  h2pDUseExternalLibnameNameFunc__nameForFunctions = '-D  Use external libname name "func__name" for functions';
  h2pEConstantsInsteadOfEnumerationTypeForCEnums = '-e  constants instead of enumeration type for C enums';
  h2pCCompactOutputmodeLessSpacesAndEmptyLines = '-c  Compact outputmode, less spaces and empty lines';
  h2pICreateAnIncludeFileInsteadOfAUnit = '-i  Create an include file instead of a unit';
  h2pPUseLetterPForPointerTypesInsteadOf = '-p  Use letter P for pointer types instead of "^"';
  h2pPrPackAllRecords1ByteAlignment = '-pr Pack all records (1 byte alignment)';
  h2pPUseProcVarsForImports = '-P  use proc. vars for imports';
  h2pSStripComments = '-s  Strip comments';
  h2pSStripCommentsAndInfo = '-S  Strip comments and info';
  h2pTPrependTypedefTypesWithT = '-t  Prepend  typedef  types with T';
  h2pTPrependTypedefTypesWithTAndRemove__ = '-T  Prepend  typedef  types with T, and remove __';
  h2pVReplacePointerParametersByVar = '-v  Replace pointer parameters by  var';
  h2pWHandleSpecialWin32Macros = '-w  Handle special win32 macros';
  h2pXHandleSYS__TRAPOfThePalmOSHeaderFiles = '-x  Handle SYS__TRAP of the PalmOS header files';
  h2pCUseTypesInCtypesUnit = '-C  Use types in ctypes unit';
  h2pOutputExtensionOfNewFile = 'Output extension of new file';
  h2pOutputDirectory = 'Output directory';
  h2pLLibraryName = '-l Library name';
  h2pBeforeH2pas = 'Before H2Pas';
  h2pConversionsBeforeRunningH2pas = 'Conversions before running H2Pas';
  h2pAfterH2pas = 'After H2Pas';
  h2pConversionsAfterRunningH2pas = 'Conversions after running H2Pas';
  h2pSettings = 'Settings';
  h2pH2pasProgramPath = 'H2Pas program path';
  h2pOpenLastSettingsOnStart = 'Open last settings on start';
  h2pSaveSettingsAs = 'Save settings as ...';
  h2pNewClearSettings = 'New/Clear settings';
  h2pOpenSettings = '&Open Settings';
  h2pSaveSettings = '&Save Settings';
  h2pRunH2pas = 'Run H2Pas';
  h2pRunH2pasAndCompile = 'Run H2Pas and compile';
  h2pClose = '&Close';
  h2pSaveChanges = 'Save changes?';
  h2pSaveSettings2 = 'Save settings?';
  h2pSaveAndExit = 'Save and exit';
  h2pDiscardChangesAndExit = 'Discard changes and exit';
  h2pDoNotExit = 'Do not exit';
  h2pConfirmRemoval = 'Confirm removal';
  h2pRemoveAllFiles = 'Remove all files';
  h2pAddHFiles2 = 'Add *.h files ...';
  h2pAddHFiles3 = 'Add .h files?';
  h2pAddTheseHFilesToH2pasProject = 'Add these .h files to H2Pas project?%s%s%s';
  h2pDeleteTheseHFilesFromList = 'Delete these .h files from list?%s%s%s';
  h2pFile = 'File: %s';
  h2pERRORFileNotFound2 = 'ERROR: file not found';
  h2pMergedInto = 'Merged into: %s';
  h2pOutput = 'Output: %s';
  h2pIncludes = 'Includes:';
  h2pIncludedBy = 'Included by:';
  h2pNoFileSelected = 'No file selected.';
  h2pAddSearchAndReplaceToolBeforeH2pas = 'Add "search and replace" tool before H2Pas';
  h2pIncludedBy2 = '%s (included by %s)';
  h2pNothingToDo = 'Nothing to do';
  h2pPleaseEnableAtLeastOneCHeaderFileThatIsNotMerged = 'Please enable at least one c header file that is not merged.';
  h2pWriteError = 'Write error';
  h2pErrorWritingGlobalConfig = 'Error writing global config:%s%s';
  h2pReadError = 'Read error';
  h2pErrorReadingGlobalConfig = 'Error reading global config:%s%s';
  h2pSaveProjectAsH2p = 'Save project as ... (*.h2p)';
  h2pH2pasProjectH2pH2pAllFiles = 'H2Pas project (*.h2p)|*.h2p|All files (*.*)|%s';
  h2pReplaceFile = 'Replace file?';
  h2pTheFileAlreadyExists = 'The file "%s"%salready exists.';
  h2pFileNotFound = 'File not found';
  h2pFileNotFound2 = 'File not found: "%s"';
  h2pErrorReadingProjectFromFile = 'Error reading project from file%s%s%s%s%s';
  h2pFilenameOfH2pasProgram = 'Filename of H2Pas program';
  h2pOpenProjectH2p = 'Open project (*.h2p) ...';
  h2pCHeaderFileHHAllFiles = 'C header file (*.h)|*.h|All files (*.*)|%s';
  h2pTextConversionToolsEditor = 'Text conversion tools editor';
  h2pTools = 'Tools:';
  h2pAddNewTool = 'Add new tool';
  h2pAddACopy = 'Add a copy';
  h2pAddFromClipboard = 'Add from clipboard';
  h2pCopyToolToClipboard = 'Copy tool to clipboard';
  h2pMoveDown = 'Move down';
  h2pMoveUp = 'Move up';
  h2pDeleteTool = 'Delete tool';
  h2pNotATCustomTextConverterTool = 'not a TCustomTextConverterTool';
  h2pError = 'Error';
  h2pErrorConvertingClipboardTextToTextTool = 'Error converting clipboard text to text tool:%s%s';
  h2pErrorConvertingPuttingToolOntoClipboard = 'Error converting putting tool onto clipboard:%s%s';
  h2pConfirmDelete = 'Confirm delete';
  h2pDoYouReallyWantToDelete = 'Do you really want to delete "%s"?';
  h2pDelete = 'Delete';
  h2pTH2PasDialogH2pasOptionsCheckGroupItemClickUnknown = 'TH2PasDialog.h2pasOptionsCheckGroupItemClick: Unknown option %s';
  h2pH2pasProject = 'H2Pas project';
  h2pOverwrite = 'Overwrite';
  h2pCancel = 'Cancel';
  h2pSelectAClass = '&Select a class';
  h2pAdd = '&Add';
  h2pCancel2 = '&Cancel';
  h2pInvalidClass = 'Invalid class';
  h2pAddMissingBracketsAroundMacroValues = 'Add missing brackets around macro values';
  h2pTAddToUsesSectionExecuteInvalidUnitname = 'TAddToUsesSection.Execute invalid unitname "%s"';
  h2pAddUnitsToUsesSection = 'Add units to uses section';
  h2pTAddToUsesSectionExecuteFileIsNotPascal = 'TAddToUsesSection.Execute file is not pascal: ';
  h2pCommentFunctionsThatAreTooComplexForH2pas = 'Comment functions that are too complex for H2Pas';
  h2pCommentMacrosThatAreTooComplexForH2pas = 'Comment macros that are too complex for H2Pas';
  h2pGiveAnonymousCEnumsATypedefName = 'Give anonymous C enums a typedef name';
  h2pAddMissingPointerTypesLikePPPChar = 'Add missing pointer types like PPPChar';
  h2pRemoveDoubleSemicolons = 'Remove double semicolons';
  h2pFixForwardDefinitionsByReordering = 'Fix forward definitions by reordering';
  h2pConvertFunctionTypesToPointers = 'Convert function types to pointers';
  h2pRemoveAllIncludeDirectives = 'Remove all include directives';
  h2pTPostH2PasToolsExecuteAddToUsesSectionInvalidUnitn = 'TPostH2PasTools.Execute.AddToUsesSection invalid unitname "%s"';
  h2pPostH2PasASetOfCommonToolsToRunAfterH2pasPhReplace = 'Post H2Pas - a set of common tools to run after H2Pas%sphReplaceUnitFilenameWithUnitName - Replace "unit filename;" with "unit name;"%sphRemoveIncludeDirectives - Remove include directives%sphRemoveSystemTypes - Remove type redefinitons like PLongint%sphFixH2PasMissingIFDEFsInUnit - Add missing IFDEFs for function bodies%sphReduceCompilerDirectivesInUnit - Removes unneeded directives%sphRemoveRedefinedPointerTypes - Remove redefined pointer types%sphRemoveEmptyTypeVarConstSections - Remove empty type/var/const sections%sphReplaceImplicitTypes - Search implicit types in parameters and add types for them%sphFixArrayOfParameterType - Replace "array of )" with "array of const)"%sphRemoveRedefinitionsInUnit - Removes redefinitions of types, variables, constants and resourcestrings%sphFixAliasDefinitionsInUnit - Fix section type of alias definitions%sphReplaceConstFunctionsInUnit - Replace simple assignment functions with constants%'
    +'sphReplaceTypeCastFunctionsInUnit - Replace simple type cast functions with types%sphFixForwardDefinitions - Fix forward definitions by reordering%sphAddUnitsToUsesSection - Add units to uses section%s';
  h2pPreH2PasASetOfCommonToolsToRunBeforeH2pasPhRemoveC = 'Pre H2Pas - a set of common tools to run before H2Pas%sphRemoveCPlusPlusExternCTool - Remove C++ ''extern "C"'' lines%sphRemoveEmptyCMacrosTool - Remove empty C macros%sphReplaceEdgedBracketPairWithStar - Replace [] with *%sphReplace0PointerWithNULL - Replace macro values 0 pointer like (char *)0%sphConvertFunctionTypesToPointers - Convert function types to pointers%sphConvertEnumsToTypeDef - Convert anonymous enums to typedef enums%sphCommentComplexCMacros - Comment macros too complex for H2Pas%sphCommentComplexCFunctions - Comment functions too complex for H2Pas%s';
  h2pReplaceSimpleFunctionsWithTypeCasts = 'Replace simple functions with type casts';
  h2pReplaceSimpleFunctionsWithConstants = 'Replace simple functions with constants';
  h2pReduceCompilerDirectivesInPascalFileShortensExpres = 'Reduce compiler directives in pascal file%sShortens expressions in $IF directives%sand removes unneeded $IFDEF and $DEFINE directives.';
  h2pAddMissingH2pasIFDEFsForFunctionBodies = 'Add missing H2Pas IFDEFs for function bodies';
  h2pFixesSectionTypeOfAliasDefinitionsInPascalUnitChec = 'Fixes section type of alias definitions in pascal unit%sChecks all alias definitions of the form%sconst LeftSide = RightSide;%slooks up RightSide in the unit and if RightSide is a type or var, changes the section accordingly';
  h2pRemoveRedefinitionsInPascalUnit = 'Remove redefinitions in pascal unit';
  h2pFixOpenArraysReplaceArrayOfWithArrayOfConst = 'Fix open arrays%sReplace "array of )" with "array of const)"';
  h2pFindExplicitTypesFAILEDRereadingTypeDefinition = 'FindExplicitTypes FAILED Rereading type definition %s';
  h2pFindExplicitTypesFAILEDReadingConstSection = 'FindExplicitTypes FAILED reading const section';
  h2pFindExplicitTypesFAILEDReadingTypeDefinition = 'FindExplicitTypes FAILED reading type definition %s';
  h2pReplaceImplicitTypesForExampleProcedureProcNameAAr = 'Replace implicit types%sFor example:%s    procedure ProcName(a: array[0..2] of char)%s  is replaced with%s    procedure ProcName(a: Tarray_0to2_of_char)%s  and a new type is added%s    Tarray_0to2_of_char = array[0..2] of char';
  h2pRemoveEmptyTypeVarConstSections = 'Remove empty type/var/const sections';
  h2pRemoveRedefinedPointerTypes = 'Remove redefined pointer types';
  h2pReplacingOfNULLWithNilFailed = 'replacing of NULL with nil failed';
  h2pDeletionOfFailed = 'deletion of "%s" failed';
  h2pRemoveTypeRedefinitionsLikePLongint = 'Remove type redefinitions like PLongint';
  h2pReplaceUnitFilenameWithUnitName = 'Replace "unit filename;" with "unit name;"';
  h2pReplaceWith = 'Replace [] with *';
  h2pReplaceMacroValues0PointerLikeChar0WithNULL = 'Replace macro values 0 pointer like (char *)0 with NULL';
  h2pRemoveEmptyCMacros = 'Remove empty C macros';
  h2pRemoveCExternCLines = 'Remove C++ ''extern "C"'' lines';
  h2pUnableToMergeFileInto = 'Unable to merge file "%s" into "%s"';
  h2pWarning = 'Warning';
  h2pAmbiguousMerges = 'Ambiguous merges:%s%s';
  h2pWarningTheFileWillBeMergedIntoMultipleFiles = '%sWarning: the file "%s"%swill be merged into multiple files:%s';
  h2pCopyingFileFailed = 'Copying file failed';
  h2pUnableToCopyFileTo = 'Unable to copy file "%s"%sto "%s"';
  h2pCHeaderFileNotFound = 'C header file "%s" not found';

implementation

end.

