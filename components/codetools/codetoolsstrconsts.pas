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
  
  // linkscanner
  ctsInvalidFlagValueForDirective = 'invalid flag value "%s" for directive %s';
  ctsInvalidMode = 'invalid mode "%s"';
  ctsAwithoutB = '%s without %s';
  ctsIncludeFileNotFound = 'include file not found "%s"';
  ctsErrorInDirectiveExpression = 'error in directive expression';
  ctsIncludeCircleDetected = 'Include circle detected';
  ctsCommentEndNotFound = 'Comment end not found';

  // customcodetool
  ctsIdentExpectedButAtomFound = 'identifier expected, but %s found';
  ctsIdentExpectedButKeyWordFound = 'identifier expected, but keyword %s found';
  ctsStrExpectedButAtomFound = '%s expected, but %s found';
  ctsIdentExpectedButEOFFound = 'unexpected end of file (identifier expected)';
  ctsBracketOpenExpectedButAtomFound = 'bracket open expected, but %s found';
  ctsBracketCloseExpectedButAtomFound = 'bracket close expected, but %s found';
  ctsBracketNotFound = 'bracket %s not found';
  ctsNoNodeFoundAtCursor = 'no pascal node found at cursor (i.e. in unparsed code)';
  ctsUnknownMainFilename = '(unknown mainfilename)';
  
  // pascal parser
  ctsUnexpectedKeyword = 'unexpected keyword "%s"';
  ctsNoPascalCodeFound = 'no pascal code found (first token is %s)';
  ctsStringConstant = 'string constant';
  ctsAnoymDefinitionsAreNotAllowed = 'Anonym %s definitions are not allowed';
  ctsEndForRecordNotFound = 'end for record not found';
  ctsMissingEnumList = 'missing enum list';
  ctsMissingTypeIdentifier = 'missing type identifier';
  ctsProcedureOrFunction = 'procedure or function';
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
  ctsUnexpectedEndOfSource = 'unexpected end of source';
  ctsPointStartAt = '. start at ';
  ctsUnexpectedKeywordInAsmBlock = 'unexpected keyword "%s" in asm block found';
  ctsUnexpectedKeywordWhileReadingBackwards =
    'unexpected keyword "%s" found while reading blocks backwards';
  ctsWordNotFound = '"%s" not found';
  ctsPointHintProcStartAt = '.  Hint: proc start at ';
  ctsMissingPointAfterEnd = 'missing . after end';
  ctsEndForClassNotFound = '"end" for class/object not found';
  ctsUnexpectedSubRangeOperatorFound = 'unexpected subrange operator ''..'' found';
  ctsInvalidSubrange = 'invalid subrange';

  // find declaration
  ctsUnitNotFound = 'unit not found: %s';
  ctsIdentifierNotFound = 'identifier not found: %s';
  ctsNoContextNodeFoundAtCursor = 'no context node found at cursor';
  ctsInheritedKeywordOnlyAllowedInMethods =
     'inherited keyword only allowed in methods';
  ctsCircleInDefinitions = 'circle in definitions';
  ctsForwardClassDefinitionNotResolved = 'Forward class definition not resolved: %s';
  ctsTypeIdentifier = 'type identifier';
  ctsAncestorIsNotProperty = 'ancestor of untyped property is not a property';
  ctsBaseTypeOfNotFound = 'base type of "%s" not found';
  ctsClassIdentifierExpected = 'class identifier expected';
  ctsDefaultClassAncestorTObjectNotFound = 'default class ancestor TObject not found';
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
  ctsClassNodeWithoutParentNode = 'class node without parent node';
  ctsTypeSectionOfClassNotFound = 'type section of class not found';
  ctsUnableToCompleteProperty = 'unable to complete property';
  ctsErrorDuringInsertingNewClassParts = 'error during inserting new class parts';
  ctsErrorDuringCreationOfNewProcBodies = 'error during creation of new proc bodies';
  ctsUnableToApplyChanges = 'unable to apply changes';
  ctsEndOfSourceNotFound = 'End of source not found';
  ctsCursorPosOutsideOfCode = 'cursor pos outside of code';
  ctsNewProcBodyNotFound = 'new proc body not found';
  
  // codetoolsmanager
  ctsNoScannerFound = 'No scanner found for "%s".'
      +' If this is an include file, please open the main source first.';
  ctsNoScannerAvailable = 'No scanner available';
  
  // definetemplates
  ctsUnknownFunction = 'Unknown function %s';
  ctsSyntaxErrorInExpr = 'Syntax Error in expression "%s"';
  ctsDefaultppc386Macro = 'Default ppc386 macro';
  ctsDefaultppc386TargetOperatingSystem = 'Default ppc386 target Operating System';
  ctsDefaultppc386SourceOperatingSystem = 'Default ppc386 source Operating System';
  ctsDefaultppc386TargetProcessor = 'Default ppc386 target processor';
  ctsFreePascalCompilerInitialMacros = 'Free Pascal Compiler initial makros';
  ctsFreePascalSourcesPlusDesc = 'Free Pascal Sources, %s';
  ctsSourceFilenamesForStandardFPCUnits =
                                  'Source filenames for the standard fpc units';
  ctsFreePascalSourceDir = 'Free Pascal Source Directory';
  ctsSrcPathInitialization = 'SrcPath Initialization';
  ctsCompiler = 'Compiler';
  ctsRuntimeLibrary = 'Runtime library';
  ctsProcessorSpecific = 'processor specific';
  ctsFreePascalComponentLibrary = 'Free Pascal Component Library';
  ctsIncludeDirectoriesPlusDirs = 'include directories: %s';
  ctsPackageDirectories = 'Package directories';
  ctsDefsForLazarusSources = 'Definitions for the Lazarus Sources';
  ctsAddsDirToSourcePath = 'adds %s to SrcPath';
  ctsSetsIncPathTo = 'sets IncPath to %s';
  ctsSetsSrcPathTo = 'sets SrcPath to %s';
  ctsNamedDirectory = '%s Directory';
  ctsAbstractWidgetPath = 'abstract widget path';
  ctsWidgetDirectory = 'Widget Directory';
  ctsComponentsDirectory = 'Components Directory';
  ctsToolsDirectory = 'Tools Directory';
  ctsDesignerDirectory = 'Designer Directory';
  ctsLazarusMainDirectory = 'lazarus main directory';
  ctsDebuggerDirectory = 'Debugger Directory';
  ctsLazarusSources = 'Lazarus Sources';
  ctsAnLCLProject = 'an LCL project';
  ctsOtherCompilerDefines = '%s Compiler Defines';
  ctsResetAllDefines = 'Reset all defines';
  ctsDefineMakroName = 'Define Makro %s';
  ctsNamedProject = '%s Project';
  
  // eventcodetool
  ctsMethodTypeDefinitionNotFound = 'method type definition not found';
  ctsOldMethodNotFound = 'old method not found: %s';
  
  // fileprocs
  ctsFileDoesNotExists = 'file "%s" does not exist';
  ctsExecuteAccessDeniedForFile = 'execute access denied for %s';
  ctsDirComponentDoesNotExistsOrIsDanglingSymLink =
    'a directory component in %s does not exist or is a dangling symlink';
  ctsDirComponentIsNotDir = 'a directory component in %s is not a directory';
  ctsInsufficientMemory = 'insufficient memory';
  ctsFileHasCircularSymLink = '%s has a circular symbolic link';
  ctsFileIsNotExecutable = '%s is not executable';

implementation

end.

