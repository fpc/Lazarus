{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    The resource strings of the package.
}
unit CodyStrConsts;

{$mode objfpc}{$H+}

interface

resourcestring
  crsNoProject = 'No project';
  crsPleaseOpenAProjectFirst = 'Please open a project first.';
  crsPPUFilesOfProject = 'PPU files of project "%s"';
  crsUses = 'Uses';
  crsCOGeneral = 'General';
  crsUsedBy = 'Used by';
  crsCOUsesPath = 'Uses path';
  crsProjectHasNoMainSourceFile = 'Project has no main source file.';
  crsMainSourceFile = 'Main source file: %s';
  crsSizeOfPpuFile = 'Size of .ppu file';
  crsSizeOfOFile = 'Size of .o file';
  crsTotal = 'Total';
  crsSearching = 'searching ...';
  crsMissing = 'missing ...';
  crsUnit = 'Unit';
  crsUnits = 'Units';
  crsLinkedFiles = 'Linked files';
  crsType = 'Type';
  crsWhere = 'Where';
  crsFile = 'File';
  crsFlags = 'Flags';
  crsPackage = 'Package';
  crsKbytes = 'kbytes';
  crsMbytes = 'Mbytes';
  crsGbytes = 'Gbytes';
  crsByFpcCfg = 'by fpc.cfg';
  crsNoUnitSelected = 'No unit selected';
  crsUnit2 = 'Unit: %s';
  crsSource = 'Source: %s';
  crsPPU = 'PPU: %s';
  crsUnitObjectFiles = 'Unit object files';
  crsUnitStaticLibraries = 'Unit static libraries';
  crsUnitSharedLibraries = 'Unit shared libraries';
  crsOtherObjectFiles = 'Other object files';
  crsOtherStaticLibraries = 'Other static libraries';
  crsOtherSharedLibraries = 'Other shared libraries';
  crsFrameworks = 'Frameworks';
  crsShowUsedPpuFiles = 'Show Used .ppu Files ...';
  crsInsertFileAtCursor = 'Insert File Contents at Cursor ...';
  crsVirtualUnit = 'Virtual Unit';
  crsProjectOutput = 'Project Output';
  crsHelp = '&Help';
  crsClose = '&Close';
  crsLinks = 'Links';
  crsCodeBuffers = 'CodeBuffers';
  crsAddAssignMethod = 'Add Assign Method';
  crsShowCodeToolsNodeInfo = 'Show CodeTools Node Info ...';
  crsShowUnitIdentifierDictionary = 'Show Unit / Identifier Dictionary ...';
  crsFindProcedureMethodOverloads = 'Find Procedure/Method Overloads';
  crsAddAssignMethod2 = 'Add Assign Method ...';
  crsCopyDeclarationToClipboard = 'Copy declaration to Clipboard';
  crsCutDeclarationToClipboard = 'Cut declaration to Clipboard';
  crsExplodeAWithBlock = 'Explode a "With" Block';
  crsAddAWithBlock = 'Add a "With" Block';

  crsCWError = 'Error';
  crsPleaseSelectSomeCodeInTheSourceEditorForANewWithBl = 'Please select some '
    +'code in the Source Editor for a new "With" block. No candidate for a '
    +'with expression was found.';
  crsPleaseSelectSomeCodeInTheSourceEditor = 'Please select some code in the '
    +'Source Editor.';
  crsAddWithBlock = 'Add "With" Block';
  crsSelectExpression = 'Select expression';
  crsExpression = 'Expression';
  crsCount = 'Count';
  crsPleasePlaceTheCursorOfTheSourceEditorOnAnIdentifie = 'Please place the '
    +'cursor of the source editor on an identifier of a declaration.';
  crsPleaseSpecifyAType = 'Please specify a type';
  crsPleaseSpecifyALocation = 'Please specify a location';
  crsPleasePlaceTheCursorOfTheSourceEditorAtAnIdentifie = 'Please place the '
    +'cursor of the source editor at an identifier in a statement.'
    +'%sFor example:'
    +'%sMyVar:=3;';
  crsDeclareVariable3 = 'Declare variable "%s"';
  crsAlreadyDefined = 'Already defined';
  crsInInterface = 'In interface';
  crsInImplementation = 'In implementation';
  crsDVIsAKeyword = '"%s" is a keyword.';
  crsOnClipboard = 'On clipboard';
  crsLocalVariableOf = 'Local variable of %s';
  crsAlreadyDefinedAt = 'Already defined at %s';
  crsPrivate = 'Private';
  crsProtected = 'Protected';
  crsPublic = 'Public';
  crsPublished = 'Published';
  crsMemberOf = '%s member of %s %s';
  crsTheIdentifierIsAlreadyDefined = 'The identifier "%s" is already defined.';
  crsCWPleasePlaceTheCursorOfTheSourceEditorOnAWithVariab = 'Please place the '
    +'cursor of the source editor on a With variable.';

  crsCAMPleasePositionTheCursorOfTheSourceEditorInAPascalC = 'Please position '
    +'the cursor of the source editor in a pascal class declaration before '
    +'invoking "Add Assign method".';
  crsCAMAddAssignMethodToClass = 'Add Assign method to class %s';
  crsCAMNewMethod = 'New method:';
  crsCAMMethodName = 'Method name:';
  crsCAMInvalidIdentifier = 'invalid identifier';
  crsCAMCursorIsNotInAPascalClassDeclaration = 'cursor is not in a pascal '
    +'class declaration';
  crsCAMExistsAlready = 'exists already';
  crsCAMParameterName = 'Parameter name:';
  crsCAMParameterType = 'Parameter type:';
  crsCAMInherited = 'Inherited:';
  crsCAMOverride = 'Override';
  crsCAMCallInherited = 'Call inherited';
  crsCAMCallInheritedOnlyIfWrongClass = 'Call inherited only if wrong class';
  crsCAMMethod = 'Method:';
  crsCAMThereIsNoInheritedMethod = 'There is no inherited method.';
  crsCAMSelectMembersToAssign = 'Select members to assign:';
  crsCAMPrivate = 'private';
  crsCAMProtected = 'protected';
  crsCAMPublic = 'public';
  crsCAMPublished = 'published';
  crsCAMVisibility = '?visibility?';
  crsCAMVar = '%s var';
  crsCAMProperty = '%s property';
  crsCAMWrittenByProperty = '%s, written by property %s';
  crsBTNOK = '&OK';
  crsBTNCancel = 'Cancel';

  crsDeclareVariable = 'Declare Variable';
  crsDeclareVariable2 = 'Declare Variable ...';
  crsInsertCallInherited = 'Insert Call Inherited';

  crsCUSelectFileToInsertAtCursor = 'Select file to insert at cursor';
  crsCUPascalPasPpPasPp = 'Pascal (*.pas;*.pp)|*.pas;*.pp';
  crsCUAllFiles = '%s|All files (%s)|%s';
  crsCUWarning = 'Warning';
  crsCUTheFileSeemsToBeABinaryProceed = 'The file seems to be a binary. '
    +'Proceed?';
  crsCUUnableToLoadFile = 'Unable to load file "%s"%s%s';
  crsCUPleasePlaceTheCursorOfTheSourceEditorInAnImplement = 'Please place the '
    +'cursor of the source editor in an implementation of an overridden method.';
  crsCodeNodeInformation = 'Code Node Information';
  crsReport = 'Report';
  crsOptions = 'Options';
  crsRefresh = 'Refresh';
  crsFilter = '(Filter)';
  crsHideUnitsOfOtherProjects = 'Hide units of other projects';
  crsAddUnitToImplementationUsesSection = 'Add unit to implementation uses '
    +'section';
  crsIfIdentifierIsAddedToTheImplementationSectionAndNe = 'If identifier is '
    +'added to the implementation section and needed unit needs to be added, '
    +'add to the uses section of the implementation';
  crsJumpTo = '&Jump to';
  crsStarts = 'Starts';
  crsShowOnlyIdentifiersStartingWithFilterText = 'Show only identifiers '
    +'starting with filter text';
  crsContains = 'Contains';
  crsShowOnlyIdentifiersContainingFilterText = 'Show only identifiers '
    +'containing filter text';
  crsUseIdentifier = '&Use identifier';
  crsAndMoreIdentifiers = '... and %s more identifiers';
  crsCodyIdentifierDictionary = 'Identifier Dictionary';
  crsReallyDeleteTheUnitFromTheDatabaseNoteThisDoesNo = 'Really delete the '
    +'unit from the database?%sNote: This does not change the source or any'
    +' configuration.%s%sunit: "%s"';
  crsReallyDeleteThePackageFromTheDatabaseNoteThisDoe = 'Really delete the '
    +'package from the database?%sNote: This does not change the source or '
    +'any configuration.%s%s"%s"';
  crsDeletePackage = 'Delete package?';
  crsDeleteUnit = 'Delete unit?';
  crsIn = 'in "%s"';
  crsUnableToRenameTo = 'unable to rename "%s" to "%s"';
  crsUnableToDelete = 'Unable to delete "%s"';
  crsShowCodyDict = 'Show Cody Dictionary for "%s"';
  crsPackagesUnitsIdentifiersFile = 'Packages: %s, Units: %s, Identifiers: %s%'
    +'sDatabase: %s';
  crsNoneSelected = 'none selected';
  crsError = 'Error: %s';
  crsPackageNotFound = 'Package not found';
  crsPackageNotFoundItShouldBeIn = 'Package "%s" not found. It should be in "%'
    +'s".';
  crsUnitNameClash = 'Unit name clash';
  crsTheTargetUnitHasTheSameNameAsTheCurrentUnitFreePas = 'The target unit has'
    +' the same name as the current unit.%s Free Pascal does not support that.';
  crsFPCUnitWithoutPpu = 'FPC unit without ppu';
  crsThisUnitIsLocatedInTheFreePascalSourcesButNoPpuFil = 'This unit is '
    +'located in the Free Pascal sources, but no ppu file is installed. Maybe '
    +'this unit is not available for this target platform.';
  crsExtendUnitPath = 'Extend unit path';
  crsImpossibleDependency = 'Impossible dependency';
  crsTheUnitIsPartOfItCanNotUseAnotherPackageWithTheSam = 'The unit "%s"'
    +'%sis part of "%s".'
    +'%sIt can not use another package with the same name:'
    +'%s"%s"';
  crsPackageWithSameName = 'Package with same name';
  crsThereIsAlreadyAnotherPackageLoadedWithTheSameNameO = 'There is already '
    +'another package loaded with the same name.%sOpen package: %s%sNew '
    +'package: %s%sOnly one package can be loaded at a time.';
  crsCloseOtherPackageAndOpenNew = 'Close other package and open new';
  crsFileNotFound = 'File not found';
  crsFileDoesNotExistAnymore = 'File "%s" does not exist anymore.';
  crsFileReadError = 'File read error';
  crsUnableToReadFile = 'Unable to read file "%s".';
  crsIdentifierNotFound = 'Identifier not found';
  crsIdentifierNotFoundInUnit = 'Identifier "%s" not found in unit "%s".'
    +' Maybe the identifier does not exist for this platform or maybe the'
    +' identifier was deleted/renamed.';
  crsPackage2 = 'Package: %s';

  crsIDEIntegration = 'IDE Integration';
  crsMinutes = '%s minutes';
  crsSeconds = '%s seconds';
  crsLoadDictionaryAfter = 'Load dictionary after %s';
  crsSaveDictionaryEvery = 'Save dictionary every %s';
  crsUnitIdentifierDictionary = 'Unit / Identifier Dictionary';
  crsTheDictionaryIsLoadedOnDemandOrAfterThisTime = 'The dictionary is loaded '
    +'on demand or after this time';
  crsTheDictionaryIsSavedInIntervals = 'The dictionary is saved in intervals';
  crsSaveDictionaryNow = 'Save dictionary now';
  crsSaveToFile = 'Save to file %s';
  crsConfigureNewIDEWindow = 'Configure new IDE window';
  crsNoteTheNameOfTheFormMustBeAValidPascalIdentifierAn = 'Note: The name of '
    +'the form must be a valid pascal identifier and unique across the whole '
    +'IDE, which includes all packages. Prepend the name with an abbreviation '
    +'of your package. The prefixes "IDE", "Laz", "Pkg" are already used by '
    +'the IDE itself.';
  crsMenuItemCaption = 'Menu item caption:';
  crsCheetahEditor = 'Cheetah Editor';
  crsContinue = 'Continue';
  crsIDEWindowDockable = 'IDE window, dockable';
  crsAWindowForTheLazarusIDEItCanBeDockedLikeTheCodeExp = 'A window for the '
    +'Lazarus IDE. It can be docked like the Code Explorer or the FPDoc Editor'
    +'. This also creates a menu item in the View menu and a short cut.';
  crsFindGDBBacktraceLine = 'Find GDB backtrace line';
  crsFindSourceOfGDBBacktrace = 'Find source of GDB backtrace';
  crsPasteLinesOfAGdbBacktrace = 'Paste lines of a gdb backtrace:';
  crsJump = 'Jump';
  crsHideAbstractMethodsAndMethodsOfClassInterfaces = 'Hide abstract methods '
    +'and methods of class interfaces';
  crsRelations = 'Relations:';
  crsIfUncheckedListAlsoProceduresWithSameNameAndIncomp = 'If unchecked list '
    +'also procedures with same name and incompatible parameter lists.';
  crsName = 'Name';
  crsCompatibility = 'Compatibility';
  crsDistance = 'Distance';
  crsExact = 'exact';
  crsCompatible = 'compatible';
  crsIncompatible = 'incompatible';
  crsCodyFindOverloads = 'Cody - Find Overloads';
  crsOnlyMethods = 'Only methods';
  crsScanningSUnits = 'Scanning: %s units ...';
  crsOnlyDescendantsOf = 'Only descendants of %s';
  crsScanning = 'Scanning ...';
  crsNoOverloadsFoundInProjectUnits = 'no overloads found in project units';
  crsUnitsS = 'Units: %s';
  crsErrorCursorIsNotInAProjectUnit = 'Error: cursor is not in a project unit';
  crsOnlyNonMethods = 'Only non methods';
  crsAny = 'Any';
  crsErrorNeedSourceEditorAtProcedureCallOrDeclaration = 'Error: Need source '
    +'editor at procedure call or declaration';
  crsParseError = 'Parse error';
  crsFilter2 = 'Filter';
  crsOnlyProceduresWithCompatibleParameters = 'Only procedures with compatible'
    +' parameters';
  crsJumpTo2 = 'Jump to';

implementation

end.

