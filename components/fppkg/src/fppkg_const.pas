{ Constants for the lazarus package manager

  Copyright (C) 2011 Darius Blaszyk

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit fppkg_const;

{$mode objfpc}{$H+}

interface

const
  COMMUNITY_SUPPORTED = -1;
  FPC_SUPPORTED = 0;
  LAZARUS_SUPPORTED = 1;

resourcestring
  rsAboutForm = 'About form';
  rsArchivesDirectory = 'Archives directory';
  rsBuildDirectory = 'Build directory';
  rsCompiler = 'Compiler';
  rsCompilerConfigDirectory = 'Compiler config directory';
  rsCompilerTarget = 'Compiler target';
  rsCompilerVersion = 'Compiler version';
  rsCustomFpmakeOptions = 'Custom fpmake options';
  rsDefaultCompilerConfig = 'Default compiler config';
  rsDownloader = 'Downloader';
  rsFpmakeCompilerConfig = 'fpmake compiler config';
  rsFppkgOptions = 'fppkg options';
  rsFppkgSFailedResultDS = 'Fppkg %s failed: Result %d%s%s';
  rsFreePascalPackageManagerForLazarus = 'Free Pascal package manager for Lazarus';
  rsGlobalInstallDir = 'Global install dir';
  rsGlobalPrefix = 'Global prefix';
  rsLocalInstallDir = 'Local install dir';
  rsLocalPrefix = 'Local prefix';
  rsLocalRepository = 'Local repository';
  rsNoFppkgExecutableFound = 'No fppkg executable found';
  rsOptions = 'Options';
  rsRemoteMirrorsURL = 'Remote mirrors URL';
  rsRemoteRepository = 'Remote repository';
  rsShowFppkgPackageManager = 'Show Fppkg Package Manager';
  rsFppkgEnvironmentOptions = 'Fppkg';
  rsFppkgEnvironmentOptionsCaption = 'Compilation';
  rsPackageFileOptionsTitle = 'FPMake';

  SInstalled = 'Installed';
  SAvailable = 'Available';
  SUnknown   = 'Unknown';
  SAuthor    = 'Author';
  SLicense   = 'License';
  SHomepage  = 'Homepage';
  SDownload  = 'Download';
  SFilename  = 'Filename';
  SEmail     = 'Email';
  SOS        = 'OS';
  SCPU       = 'CPU';
  SSupport   = 'Support';
  SKeywords  = 'Keywords';
  SCategory  = 'Category';

  lisFppkgPckOptsTitle = 'Fppkg';
  lisFppkgPckOptsBuildMethod = 'Supported build methods';
  lisFppkgBuildMethodFPMake = 'FPMake';
  lisFppkgBuildMethodLazarus = 'Lazbuild';
  lisFppkgBuildMethodBoth = 'Both';

  SErrActionFailed    = 'Failed to %s: ' + sLineBreak + sLineBreak + '%s';
  SMsgActionSucceeded = '%s succeeded.';
  SMsgFppkgRunning    = 'A prior command is still in progress.';
  SActFixBroken       = 'fix broken packages';
  SActCleanPackages   = 'clean package(s)';
  SActCompilePackages = 'compile packages';
  SActDownloadPackages= 'download packages';
  SActArchivePackages = 'create archive(s) for package(s)';
  SActBuildPackages   = 'build package(s)';
  SActInstallPackages = 'install package(s)';
  SActUnInstPackages  = 'uninstall package(s)';
  SActUpdate          = 'update repository';
  SActInitializeFppkg = 'initialize fppkg';

implementation

end.

