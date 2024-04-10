{
 /***************************************************************************
                          IdeUtilsPkgStrConsts.pas
                          -----------------------
              This unit contains resource strings of the IDE


 ***************************************************************************/

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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit IdePackagerStrConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  lisUnit = 'Unit';
  lisPkgFileTypeVirtualUnit = 'Virtual Unit';
  lisPkgFileTypeMainUnit = 'Main Unit';
  lisPkgFileTypeLFM = 'LFM - Lazarus form text';
  lisPkgFileTypeLRS = 'LRS - Lazarus resource';
  lisPkgFileTypeInclude = 'Include file';
  lisPkgFileTypeIssues = 'Issues xml file';
  lisPkgFileTypeText = 'Text';
  lisPkgFileTypeBinary = 'Binary';
  lisCEIn = '%s in %s';
  lisPckEditDefault = '%s, default: %s';
  lisPckEditFPMakePackage = '(fpmake)';
  lisPckOptsPackageOptions = 'Package Options';
  lisExtendUnitPath = 'Extend unit path?';
  lisExtendUnitSearchPathOfPackageWith = 'Extend unit search path of package "'
    +'%s" with%s"%s"?';
  lisExtendIncludePath = 'Extend include path?';
  lisExtendIncludeFilesSearchPathOfProjectWith = 'Extend include files search '
    +'path of project with%s"%s"?';
  lisExtendIncludeFileSearchPathOfPackageWith = 'Extend include file search '
    +'path of package "%s" with%s"%s"?';
  dlgCompilerOptions = 'Compiler Options';
  lisPkgDefsUnitPath = 'Unit Path';
  lisPkgDefsOutputDirectory = 'Output directory';
  lisPkgDefsCompiledSrcPathAddition = 'CompiledSrcPath addition';
  lisPkgDefsSrcDirMark = 'Package Source Directory Mark';

  lisPassingCompilerOptionTwiceWithDifferentValues = 'passing compiler option '
    +'-%s twice with different values';
  lisPassingCompilerDefineTwiceWithDifferentValues = 'passing compiler define '
    +'"%s" twice with different values';
  lisPkgMangUnableToCreateOutputDirectoryForPackage = 'Unable to create '
    +'output directory "%s"%sfor package %s.';

  lisPkgMacroPackageDirectoryParameterIsPackageID = 'Package directory. '
    +'Parameter is package ID, e.g. "Name" or "Name 1.0"';
  lisPkgMacroPackageSourceSearchPathParameterIsPackageID = 'Package source '
    +'search path. Parameter is package ID, e.g. "Name" or "Name 1.0"';
  lisPkgMacroPackageUnitSearchPathParameterIsPackageID = 'Package unit search '
    +'path. Parameter is package ID, e.g. "Name" or "Name 1.0"';
  lisPkgMacroPackageIncludeFilesSearchPathParameterIsPackageID = 'Package '
    +'include files search path. Parameter is package ID, e.g. "Name" or "Name 1.0"';
  lisPkgMacroPackageNameParameterIsPackageID = 'Package name. Parameter is package ID, e.g. "Name" or "Name 1.0"';
  lisPkgMacroPackageOutputDirectoryParameterIsPackageID = 'Package output directory. Parameter is package ID, e.g. "Name" or "Name 1.0"';

  lispIgnoreAll = 'Ignore all';

  // package system
  lisPkgSysInvalidUnitname = 'Invalid Unitname: %s';
  lisPkgSysUnitWasNotFoundInTheLpkFileProbablyThisLpkFileWasN =
     'Unit "%s" was not found in the lpk file.'
    +'%sProbably this lpk file was not used for building this IDE. '
    +'Or the package misuses the procedure RegisterUnit.';
  lisPkgSysUnitWasRemovedFromPackageLpk = 'Unit "%s" was removed from package (lpk)';
  lisPkgSysCanNotRegisterComponentsWithoutUnit = 'Cannot register components without unit';
  lisPkgSysInvalidComponentClass = 'Invalid component class';
  lisPkgSysComponentClassAlreadyDefined = 'Component Class "%s" already defined';
  lisPkgSysRegisterUnitWasCalledButNoPackageIsRegistering = 'RegisterUnit was '
    +'called but no package is registering.';
  lisPkgSysUnitName = '%s%sUnit Name: "%s"';
  lisPkgSysFileName = '%s%sFile Name: "%s"';
  lisPkgSysLPKFilename = '%s%slpk file: "%s"';
  lisPkgSysTheLpkFileWasNotFound = '%s%sThe lpk file was not found.';
  lisPkgSysPackageRegistrationError = 'Package registration error';
  lisUpdatingPoFilesFailedForPackage = 'Updating PO files failed for package %s';
  lisPkgSysThisIsTheDefaultPackageUsedOnlyForComponents = 'This is the '
    +'default package. Used only for components without a package. These '
    +'components are outdated.';
  lisPkgMangUnableToLoadPackage = 'Unable to load package';
  lisPkgMangUnableToOpenThePackage = 'Unable to open the package "%s".'
    +'%sThis package was marked for installation.';
  lisPkgMangstaticPackagesConfigFile = 'static packages config file';
  lisPkgMangErrorWritingFile = 'Error writing file';
  lisPkgMangUnableToWriteStateFileOfPackageError = 'Unable to write state '
    +'file "%s"%sof package %s.%sError: %s';
  lisPkgMangErrorReadingFile = 'Error reading file';
  lisPkgMangUnableToReadStateFileOfPackageError = 'Unable to read state file '
    +'"%s"%sof package %s.%sError: %s';
  lisPkgMangCompilePackage = 'Compile package %s';
  lisIDEInfoCreatingMakefileForPackage = 'Creating Makefile for package %s';
  lisPkgSysThisPackageIsInstalledButTheLpkFileWasNotFound = 'This package is installed '
    +'but the lpk file was not found. All its components are deactivated. Please fix this.';
  lisPkgSysPackageFileNotFound = 'Package file not found';
  lisPkgMangTheFileOfPackageWasNotFound = 'The file "%s" of package %s was not found.';
  lisPkgSysThePackageIsInstalledButNoValidPackageFileWasFound =
     'The package "%s" is installed but no valid package file (.lpk) was found.'
    +'%sA broken dummy package was created.';
  lisSkipTheseWarnings = 'Skip these warnings';


  lisExecutingCommandBefore = 'Executing command before';
  lisExecutingCommandAfter = 'Executing command after';

  lisPkgMangUnableToDeleteFilename = 'Unable to delete file';
  lisPkgMangUnableToCreatePackageSourceDirectoryForPackage = 'Unable to '
    +'create package source directory "%s"%sfor package %s.';
  lisPkgMangDeleteFailed = 'Delete failed';
  lisTheFileWasFoundInOneOfTheSourceDirectoriesOfThePac = 'The file "%s"'
    +'%swas found in one of the source directories of the package %s and looks '
    +'like a compiled unit. Compiled units must be in the output directory of '
    +'the package, otherwise other packages can get problems using this package.'
    +'%sDelete ambiguous file?';
  lisAmbiguousUnitFound = 'Ambiguous unit found';
  lisDeletingOfFileFailed = 'Deleting of file "%s" failed.';
  lisPkgMangpackageMainSourceFile = 'package main source file';
  lisPkgMangUnableToDeleteOldStateFileForPackage = 'Unable to delete old '
    +'state file "%s"%sfor package %s.';
  lisPkgSysRegisterProcedureIsNil = 'Register procedure is nil';

implementation

end.

