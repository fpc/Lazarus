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
    This unit contains all resource strings for the codetools.

}
unit CodeToolsStrConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

ResourceString
  // codetree
  ctsUnknownSubDescriptor = '(unknown subdescriptor %s)';
  ctsForward = 'Forward';
  ctsUnparsed = 'Unparsed';
  ctsHasError = 'HasError';

  // linkscanner
  ctsInvalidFlagValueForDirective = 'invalid flag value "%s" for directive %s';
  ctsInvalidMode = 'invalid mode "%s"';
  ctsInvalidModeSwitch = 'invalid mode switch "%s"';
  ctsAwithoutB = '%s without %s';
  ctsIncludeFileNotFound = 'include file not found "%s"';
  ctsErrorInDirectiveExpression = 'error in directive expression';
  ctsIncludeCircleDetected = 'Include circle detected';
  ctsfileIsReadOnly = 'file is read only';
  ctsCommentEndNotFound = 'Comment end not found';

  // customcodetool
  ctsIdentExpectedButAtomFound = 'identifier expected, but %s found';
  ctsIdentExpectedButKeyWordFound = 'identifier expected, but keyword %s found';
  ctsStrExpectedButAtomFound = '%s expected, but %s found';
  ctsEndOfFile = 'end of file';
  ctsPositionNotInSource = 'Position not in source';
  ctsInvalidClassName = 'invalid class name=%s%s%s';
  ctsSourceOfUnitNotFound = 'source of unit not found: %s';
  ctsclassNotFound = 'class %s%s%s not found';
  ctsinvalidClassName2 = 'invalid class name %s%s%s';
  ctsinvalidVariableName = 'invalid variable name %s%s%s';
  ctsinvalidVariableType = 'invalid variable type %s%s%s';
  ctsClassSNotFound = 'Class %s not found';
  ctsIdentExpectedButEOFFound = 'unexpected end of file (identifier expected)';
  ctsOperatorExpectedButEOFFound = 'unexpected end of file (operator expected)';
  ctsOperatorExpectedButAtomFound = 'operator expected, but %s found';
  ctsBracketOpenExpectedButAtomFound = 'bracket open expected, but %s found';
  ctsBracketCloseExpectedButAtomFound = 'bracket close expected, but %s found';
  ctsBracketNotFound = 'bracket %s not found';
  ctsUnknownMainFilename = '(unknown mainfilename)';
  
  // pascal parser
  ctsUnexpectedKeyword = 'unexpected keyword "%s"';
  ctsNoPascalCodeFound = 'no pascal code found (first token is %s)';
  ctsStringConstant = 'string constant';
  crsClosingBracketNotFound = 'closing bracket not found';
  crsBracketNotFound = 'bracket ) not found';
  crsBracketNotFound2 = 'bracket ] not found';
  crsRecordEndNotFound = 'record end not found';
  crsFoundUnexpectedAt = '%s, found unexpected %s at %s';
  ctsAnonymDefinitionsAreNotAllowed = 'Anonymous %s definitions are not allowed';
  ctsNestedDefinitionsAreNotAllowed = 'Nested %s definitions are not allowed';
  ctsEndForRecordNotFound = 'end for record not found';
  ctsMissingEnumList = 'missing enum list';
  ctsMissingTypeIdentifier = 'missing type identifier';
  ctsProcedureOrFunctionOrConstructorOrDestructor = 'procedure or function or constructor or destructor';
  ctsMethodName = 'method name';
  ctsIdentifier = 'identifier';
  ctsSemicolonNotFound = 'semicolon not found';
  ctsKeyword = 'keyword';
  ctsKeywordExampleExpectedButAtomFound = 'keyword (e.g. %s) expected, but %s found';
  ctsConstant = 'constant';
  ctsSemicolonAfterPropSpecMissing =
    '; expected after "%s" property specifier, but %s found';
  ctsUnknownSectionKeyword = 'unknown section keyword %s found';
  ctsIllegalQualifier = 'illegal qualifier %s found';
  ctsMethodSignatureSNotFoundInClass = 'Method signature %s not found in class';
  ctsUnexpectedEndOfSource = 'unexpected end of source';
  ctsEndofSourceExpectedButAtomFound = 'expected end., but %s found';
  ctsPointStartAt = '. start at ';
  ctsUnexpectedKeywordInAsmBlock = 'unexpected keyword "%s" in asm block found';
  ctsUnexpectedKeywordInBeginEndBlock =
    'unexpected keyword "%s" in begin..end found';
  ctsUnexpectedKeywordWhileReadingBackwards =
    'unexpected keyword "%s" found while reading blocks backwards';
  ctsWordNotFound = '"%s" not found';
  ctsPointHintProcStartAt = '.  Hint: proc start at ';
  ctsMissingPointAfterEnd = 'missing . after end';
  ctsEndForClassNotFound = '"end" for class/object not found';
  ctsVarIsOnlyAllowedInGenericsEndOfClassNotFound = 'var is only allowed in '
    +'generics, end of class not found';
  ctsTypeIsOnlyAllowedInGenericsEndOfClassNotFound = 'type is only allowed in '
    +'generics, end of class not found';
  ctsUnexpectedSubRangeOperatorFound = 'unexpected subrange operator ''..'' found';
  ctsInvalidSubrange = 'invalid subrange';
  ctsExportsClauseOnlyAllowedInLibraries = 'exports clause only allowed in libraries';

  // find declaration
  ctsUnitNotFound = 'unit not found: %s';
  ctsSourceNotFoundUnit = 'source not found: unit %s';
  ctsIdentifierNotFound = 'identifier not found: %s';
  ctsNotEnoughGenParams = 'Not enough actual generic parameters';
  ctsExpected = '"%s" expected';
  ctsNoContextNodeFoundAtCursor = 'no context node found at cursor';
  ctsInheritedKeywordOnlyAllowedInMethods =
     'inherited keyword only allowed in methods';
  ctsCircleInDefinitions = 'circle in definitions';
  ctsForwardClassDefinitionNotResolved = 'Forward class definition not resolved: %s';
  ctsClassOfDefinitionNotResolved = '"class of" definition not resolved: %s';
  ctsTypeIdentifier = 'type identifier';
  ctsGenericIdentifier = 'generic identifier';
  ctsAncestorIsNotProperty = 'ancestor of untyped property is not a property';
  ctsBaseTypeOfNotFound = 'base type of "%s" not found';
  ctsClassIdentifierExpected = 'class identifier expected';
  ctsDefaultClassAncestorTObjectNotFound = 'default class ancestor TObject not found';
  ctsDefaultInterfaceAncestorIInterfaceNotFound =
    'default interface ancestor IInterface not found';
  ctsDefaultDispinterfaceAncestorIDispatchNotFound = 'default dispinterface '
    +'ancestor IDispatch not found';
  ctsDefaultJavaClassAncestorJLObjectNotFound = 'default java class ancestor '
    +'JLObject not found';
  ctsDefaultAncestorNotFound = 'default ancestor %s not found';
  ctsExprTypeMustBeClassOrRecord = 'expression type must be class or record type';
  ctsClassWithoutName = 'class without name';
  ctsBinaryOperator = 'binary operator';
  ctsInvalidType = 'invalid type';
  ctsKeywordIn = 'keyword "in"';
  ctsIllegalCircleInUsedUnits = 'illegal circle using unit: %s';
  ctsSourceIsNotUnit = 'source is not unit';
  ctsInterfaceSectionNotFound = 'interface section not found';
  ctsUsedUnitIsNotAPascalUnit = 'used unit is not a pascal unit';
  ctsDuplicateIdentifier = 'duplicate identifier: %s';
  ctsQualifierExpectedButAtomFound = 'qualifier expected but %s found';
  ctsIncompatibleTypesGotExpected = 'incompatibles types: expected "%s" but got "%s"';
  ctsDefaultPropertyNotFound = 'default property not found';
  ctsFunctionGetEnumeratorNotFoundInThisClass = 'function GetEnumerator not '
    +'found in this class';
  ctsFunctionGetEnumeratorNotFoundInThisClass2 = 'function GetEnumerator not '
    +'found in this class';
  ctsResultTypeOfFunctionGetEnumeratorNotFound = 'result type of function '
    +'GetEnumerator not found';
  ctsPropertyCurrentNotFound = 'property Current not found';
  ctsEnumerationType = 'enumeration type';
  ctsType = 'type';
  ctsExpectedStatementButFound = 'expected statement, but found %s';
  ctsUnexpectedKeyword2 = 'unexpected keyword %s';
  ctsBeginAtWithoutEnd = 'begin at %s without end';
  ctsThenExpectedButFound = 'then expected, but %s found';
  ctsExpectedButFound = 'expected (, but found %s';
  ctsExpectedIdentifierButFound = 'expected identifier, but found %s';
  ctsExpectedButFound2 = 'expected ), but found %s';
  ctsMissing = 'missing :=';
  ctsExpectedButFound3 = 'expected :=, but %s found';
  ctsCharacterConstantOutOfRange = 'character constant out of range';
  ctsOperatorExpectedButFound = 'operator expected but %s found';
  ctsOperandExpectedButFound = 'operand expected but %s found';
  ctsOperandExpectedButFound2 = 'operand expected, but %s found';
  ctsInvalidOperator = 'invalid operator %s';
  ctsOperatorExpectedButFound2 = 'operator expected, but %s found';

  // codecompletion
  ctsPropertySpecifierAlreadyDefined = 'property specifier already defined: %s';
  ctsErrorInParamList = 'error in paramlist';
  ctsPropertTypeExpectedButAtomFound = 'property type expected, but %s found';
  ctsIndexSpecifierRedefined = 'index specifier redefined';
  ctsIndexParameterExpectedButAtomFound = 'index parameter expected, but %s found';
  ctsDefaultSpecifierRedefined = 'default specifier redefined';
  ctsDefaultParameterExpectedButAtomFound = 'default parameter expected, but %s found';
  ctsNodefaultSpecifierDefinedTwice = 'nodefault specifier defined twice';
  ctsImplementationNodeNotFound = 'implementation node not found';
  ctsDispidSpecifierRedefined = 'dispid specifier redefined';
  ctsDispidParameterExpectedButAtomFound = 'dispid parameter expected, but %s found';
  ctsClassNodeWithoutParentNode = 'class node without parent node';
  ctsTypeSectionOfClassNotFound = 'type section of class not found';
  ctsUnableToCompleteProperty = 'unable to complete property';
  ctsErrorDuringInsertingNewClassParts = 'error during inserting new class parts';
  ctsErrorDuringCreationOfNewProcBodies = 'error during creation of new proc bodies';
  ctsErrorDuringInsertingNewUsesSection = 'error during inserting new units to the main uses section';
  ctsUnableToApplyChanges = 'unable to apply changes';
  ctsEndOfSourceNotFound = 'End of source not found.';
  ctsCursorPosOutsideOfCode = 'cursor pos outside of code';
  ctsNewProcBodyNotFound = 'new proc body not found';
  ctsIdentifierAlreadyDefined = 'Identifier %s already defined';
  ctsTermNotSimple = 'Term has no simple type';
  
  // codetoolsmanager
  ctsNoScannerFound = 'No scanner found for "%s".'
      +' If this is an include file, please open the main source first.';
  ctsNoScannerAvailable = 'No scanner available';
  
  // definetemplates
  ctsUnknownFunction = 'Unknown function %s';
  ctsSyntaxErrorInExpr = 'Syntax Error in expression "%s"';
  ctsDefaultFPCSymbol = 'Default fpc symbol';
  ctsDefaultFPCTargetOperatingSystem = 'Default fpc target Operating System';
  ctsDefaultFPCSourceOperatingSystem = 'Default fpc source Operating System';
  ctsDefaultFPCSource2OperatingSystem = 'Default fpc source Operating System 2';
  ctsDefaultFPCTargetProcessor = 'Default fpc target processor';
  ctsFreePascalCompilerInitialMacros = 'Free Pascal Compiler initial macros';
  ctsScannedFiles = 'Scanned files: %s';
  ctsFreePascalSourcesPlusDesc = 'Free Pascal Sources, %s';
  ctsSourceFilenamesForStandardFPCUnits =
                                  'Source filenames for the standard fpc units';
  ctsFreePascalSourceDir = 'Free Pascal Source Directory';
  ctsSrcPathInitialization = 'SrcPath Initialization';
  ctsUnitPathInitialization = 'UnitPath Initialization';
  ctsDefineLCL = 'Define LCL';
  ctsDefineLCLWidgetset = 'Define LCLwidgetset, e.g. LCLgtk';
  ctsFPDocSystemOn = 'enable FPDocSystem';
  ctsCompiler = 'Compiler';
  ctsDefineProcessorType = 'Define processor type';
  ctsRuntimeLibrary = 'Runtime library';
  ctsProcessorSpecific = 'processor specific';
  ctsFreePascalComponentLibrary = 'Free Pascal Component Library';
  ctsIncludeDirectoriesPlusDirs = 'include directories: %s';
  ctsPackageDirectories = 'Package directories';
  ctsUtilsDirectories = 'Utils directories';
  ctsIfTargetOSIsNotSrcOS = 'If TargetOS<>SrcOS';
  ctsIfTargetOSIsNotSrcOS2 = 'If TargetOS<>SrcOS2';
  ctsInstallerDirectories = 'Installer directories';
  ctsDefsForLazarusSources = 'Definitions for the Lazarus Sources';
  ctsAddsDirToSourcePath = 'adds %s to SrcPath';
  ctsIfDefWindows = 'IfDef Windows';
  ctsIfDefLinux = 'IfDef Linux';
  ctsIfDefDarwin = 'IfDef Darwin';
  ctsAddsDirToIncludePath = 'adds %s to IncPath';
  ctsSetsIncPathTo = 'sets IncPath to %s';
  ctsSetsSrcPathTo = 'sets SrcPath to %s';
  ctsNamedDirectory = '%s Directory';
  ctsWidgetDirectory = 'Widget Directory';
  ctsIntfDirectory = 'interface directory';
  ctsIfLCLWidgetTypeEqualsGtk2 = 'If LCLWidgetType=gtk2 then';
  ctsElse = 'else';
  ctsIfTargetOSIsNotWin32 = 'If TargetOS<>win32 then';
  ctsDefineMacroGTK1 = 'Define macro gtk1';
  ctsDefineMacroGTK2 = 'Define macro gtk2';
  ctsDefineMacroCarbon1 = 'Define macro carbon1';
  ctsDefineMacroQT1 = 'Define macro qt1';
  ctsGtk2IntfDirectory = 'gtk2 interface directory';
  ctsDefineMacroWinCE1 = 'Define macro wince1';
  ctsComponentsDirectory = 'Components Directory';
  ctsCustomComponentsDirectory = 'Custom Components Directory';
  ctsToolsDirectory = 'Tools Directory';
  ctsInstallDirectory = 'Install Directory';
  ctsDesignerDirectory = 'Designer Directory';
  ctsIDEIntfDirectory = 'IDEIntf Directory';
  ctsJITFormDirectory = 'JITForm Directory';
  ctsDesignerUnitsDirectory = 'Designer Units';
  ctsCompiledSrcPath = 'Compiled SrcPath';
  ctsPackagerDirectory = 'Packager Directory';
  ctsPackagerRegistrationDirectory = 'Packager Registration Directory';
  ctsPackagerUnitsDirectory = 'Packager Units Directory';
  ctsCPUDirectory = 'CPU directory';
  ctsLazarusMainDirectory = 'lazarus main directory';
  ctsDebuggerDirectory = 'Debugger Directory';
  ctsConverterDirectory = 'Converter Directory';
  ctsIDEDirectory = 'IDE Directory';
  ctsDocEditorDirectory = 'Doc Editor Directory';
  ctsLazarusSources = 'Lazarus Sources';
  ctsAnLCLProject = 'an LCL project';
  ctsOtherCompilerDefines = '%s Compiler Defines';
  ctsResetAllDefines = 'Reset all defines';
  ctsDefineMacroName = 'Define Macro %s';
  ctsNamedProject = '%s Project';
  ctsDefine = 'Define ';
  ctsUndefine = 'Undefine ';
  ctsCommandLineParameters = 'Command line parameters';

  // eventcodetool
  ctsMethodTypeDefinitionNotFound = 'method type definition not found';
  ctsOldMethodNotFound = 'old method not found: %s';
  
  // fileprocs
  ctsExecuteAccessDeniedForFile = 'execute access denied for %s';
  ctsDirComponentDoesNotExistsOrIsDanglingSymLink =
    'a directory component in %s does not exist or is a dangling symlink';
  ctsDirComponentIsNotDir = 'a directory component in %s is not a directory';
  ctsInsufficientMemory = 'insufficient memory';
  ctsFileHasCircularSymLink = '%s has a circular symbolic link';

  // misc
  ctsSrcPathForCompiledUnits = 'src path for compiled units';
  ctsTCodeToolManagerConsistencyCheck = 'TCodeToolManager.ConsistencyCheck=%d';

implementation

end.

