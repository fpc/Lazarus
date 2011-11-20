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
  lisCOGeneral = 'General';
  crsUsedBy = 'Used by';
  lisCOUsesPath = 'Uses path';
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
  crsInsertFileAtCursor = 'Insert File at Cursor ...';
  crsVirtualUnit = 'Virtual Unit';
  crsProjectOutput = 'Project Output';
  crsHelp = '&Help';
  crsClose = '&Close';
  crsAddAssignMethod = 'Add Assign Method';
  crsShowCodeToolsNodeInfo = 'Show CodeTools Node Info ...';
  crsShowUnitIdentifierDictionary = 'Show Unit / Identifier Dictionary ...';
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
    +'cursor of the source editor at an identifier in a statement.%sFor '
    +'example:%sMyVar:=3;';
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
  crsAddCallInherited = 'Add call inherited';

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
  crsAndMoreIdentifiers = '... and %s more identifiers';
  crsCodyIdentifierDictionary = 'Cody Identifier Dictionary';
  crsPackagesUnitsIdentifiersFile = 'Packages: %s, Units: %s, Identifiers: %s%'
    +'sFile: %s';
  crsError = 'Error: %s';

implementation

end.

