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

 Author: Balázs Székely
 Abstract:
   Constants, resource strings for the online package manager.
}
unit opkman_const;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  cRemoteJSONFile = 'packagelist.json';
  cLocalRepository =  'onlinepackagemanager';
  cLocalRepositoryPackages = 'packages';
  cLocalRepositoryArchive = 'archive';
  cLocalRepositoryUpdate = 'update';
  cLocalRepositoryConfig = 'config';
  cLocalRepositoryConfigFile = 'options.xml';

resourcestring
  //package manager
  rsLazarusPackageManager = 'Online Package Manager';

  //main form
  rsMainFrmCaption = 'packages found';
  rsMainFrmVSTHeaderColumnPackageName = 'Packages';
  rsMainFrmVSTHeaderColumnPackageFile = 'Package file(.lpk)';
  rsMainFrmVSTHeaderColumnAvailable = 'Available';
  rsMainFrmVSTHeaderColumnInstalled = 'Installed';
  rsMainFrmVSTHeaderColumnData = 'Status/Data';
  rsMainFrmVSTTextPackageCategory = 'Package category';
  rsMainFrmVSTTextPackageStatus = 'Package status';
  rsMainFrmVSTTextVersion = 'Version';
  rsMainFrmVSTTextDescription = 'Description';
  rsMainFrmVSTTextAuthor = 'Author';
  rsMainFrmVSTTextLazCompatibility = 'Lazarus compatibility';
  rsMainFrmVSTTextFPCCompatibility = 'FPC compatibility';
  rsMainFrmVSTTextSupportedWidgetsets = 'Supported widget sets';
  rsMainFrmVSTTextPackagetype = 'Package type';
  rsMainFrmVSTTextDependecies = 'Dependencies';
  rsMainFrmVSTTextLicense = 'License';
  rsMainFrmVSTTextPackageInfo = 'Package info';
  rsMainFrmVSTTextCategory = 'Category';
  rsMainFrmVSTTextRepositoryFilename = 'Repository filename';
  rsMainFrmVSTTextRepositoryFileSize = 'Repository filesize';
  rsMainFrmVSTTextRepositoryFileHash = 'Repository filehash';
  rsMainFrmVSTTextRepositoryFileDate = 'Available since';
  rsMainFrmVSTTextHomePageURL = 'Home page';
  rsMainFrmVSTTextDownloadURL = 'Update link';
  rsMainFrmVSTTextSVNURL = 'SVN';
  rsMainFrmVSTTextPackageType0 = 'Designtime and runtime';
  rsMainFrmVSTTextPackageType1 = 'Designtime';
  rsMainFrmVSTTextPackageType2 = 'Runtime';
  rsMainFrmVSTTextPackageType3 = 'Runtime only, cannot be installed in IDE';
  rsMainFrmVSTTextPackageState0 = 'Repository';
  rsMainFrmVSTTextPackageState1 = 'Downloaded';
  rsMainFrmVSTTextPackageState2 = 'Extracted';
  rsMainFrmVSTTextPackageState3 = 'Installed';
  rsMainFrmVSTTextPackageState4 = 'Up to date';
  rsMainFrmVSTTextPackageCategory0 = 'Cryptography';
  rsMainFrmVSTTextPackageCategory1 = 'DataControls';
  rsMainFrmVSTTextPackageCategory2 = 'Graphics';
  rsMainFrmVSTTextPackageCategory3 = 'GUIContainers';
  rsMainFrmVSTTextPackageCategory4 = 'LazIDEPlugins';
  rsMainFrmVSTTextPackageCategory5 = 'Multimedia';
  rsMainFrmVSTTextPackageCategory6 = 'Networking';
  rsMainFrmVSTTextPackageCategory7 = 'Reporting';
  rsMainFrmVSTTextPackageCategory8 = 'Other';
  rsMainFrmcbAllCaption = 'All/None';
  rsMainFrmcbAllHint = 'Check/Uncheck packages';
  rsMainFrmlbFilterCaption = 'Filter by:';
  rsMainFrmcbFilterByHint = 'Filter package list by:';
  rsMainFrmedFilterHint = 'Type filter text';
  rsMainFrmspClearHint = 'Clear filter text';
  rsMainFrmspExpandHint = 'Expand package tree';
  rsMainFrmspCollapseHint = 'Collapse package tree';
  rsMainFrmTBRefreshCaption = 'Refresh';
  rsMainFrmTBRefreshHint = 'Refresh package list';
  rsMainFrmTBDownloadCaption = 'Download';
  rsMainFrmTBDownloadHint = 'Download packages to...';
  rsMainFrmTBInstallCaption = 'Install';
  rsMainFrmTBInstallHint = 'Install packages';
  rsMainFrmTBUpdateCaption = 'Update';
  rsMainFrmTBUpdateHint = 'Update packages from external URL';
  rsMainFrmTBCleanUpCaption = 'Cleanup';
  rsMainFrmTBCleanUpHint = 'Cleanup local repository';
  rsMainFrmTBRepositoryCaption = 'Create';
  rsMainFrmTBRepositoryHint = 'Create package or repository';
  rsMainFrmTBOptionsCaption = 'Options';
  rsMainFrmTBOptionsHint = 'Show options dialog';
  rsMainFrmmiCreateRepositoryPackage = 'Create repository package';
  rsMainFrmmiCreateRepository = 'Create repository';
  rsMainFrmmiJSONShow =  'Show JSON';
  rsMainFrmmiJSONHide = 'Hide JSON';
  rsMainFrmPackagenameAlreadyExists = 'A package with the same name already exists!';
  rsMainFrmFilenameAlreadyExists = 'A package with the same zip file already exists!';
  rsMainFrmPackageAlreadyInstalled = 'The following packages are alrady installed. Continue with install?';
  rsMainFrmPackageAlreadyDownloaded = 'The following repository packages already exists in the target folder. Continue?';
  rsMainFrmPackageUpdateWarning = 'Updating packages from external link is not without a risk!' + sLineBreak + 'Only update if you trust the package maintainer. Continue?';
  rsMainFrmPackageUpdate0 = 'The following repository packages don''t have a valid external download link. The packages will be skipped. Continue?';
  rsMainFrmPackageUpdate1 = 'None of the checked repository package(s) has a valid external download link.';

  //progress form
  rsProgressFrmCaption0 = 'Downloading packages';
  rsProgressFrmCaption1 = 'Extracting packages';
  rsProgressFrmCaption2 = 'Installing packages';
  rsProgressFrmCaption3 = 'Updating packages';
  rsProgressFrmCaption4 = '. Please wait...';
  rsProgressFrmCaption5 = 'Unknown';
  rsProgressFrmlbPackageCaption = 'Package:';
  rsProgressFrmlbSpeedCaption = 'Speed:';
  rsProgressFrmlbSpeedCalcCaption = 'Estimating. Please wait...';
  rsProgressFrmlbEllapsedCaption = 'Ellapsed:';
  rsProgressFrmlbRemainingCaption = 'Remaining:';
  rsProgressFrmlbReceivedCaption0 = 'Received:';
  rsProgressFrmlbReceivedCaption1 = 'Unzipped:';
  rsProgressFrmlbReceivedTotalCaption0 = 'Received(total):';
  rsProgressFrmlbReceivedTotalCaption1 = 'Unzipped(total):';
  rsProgressFrmcbExtractOpenCaption0 = 'Extract after download';
  rsProgressFrmcbExtractOpenCaption1 = 'Open containing folder';
  rsProgressFrmCancelDownload = 'Cancel download?';
  rsProgressFrmCancelZip = 'Cancel zip?';
  rsProgressFrmError0 = 'Cannot download package:';
  rsProgressFrmError1 = 'Error message:';
  rsProgressFrmError2 = 'Cannot extract package:';
  rsProgressFrmError3 = 'Cannot install package:';
  rsProgressFrmError4 = 'Dependecy';
  rsProgressFrmError5 = 'not found!';
  rsProgressFrmError6 = 'Unknown error.';
  rsProgressFrmError7 = 'Cannot contact download site';
  rsProgressFrmError8 = 'No valid download link found.';
  rsProgressFrmError9 = 'Cannot add package to the IDE.';
  rsProgressFrmConfirm0 = 'Continue with next one?';
  rsProgressFrmInfo0 = 'Installing package:';
  rsProgressFrmInfo1 = 'Success.';
  rsProgressFrmInfo2 = 'Contacting download site for';
  rsProgressFrmInfo3 = 'Preparing to download. Please wait...';
  rsProgressFrmInfo4 = 'Canceling. Please wait...';

  //options form
  rsOptionsFrmCaption = 'Options';
  rsOptionedRemoteRepository = 'Please enter the remote repository address!';
  rsOptionedProxyServer = 'Please enter the proxy server address!';
  rsOptionedProxyPort = 'Please enter the proxy server port!';

  //packagelist form
  rsPackageListFrmCaption0 = 'Installed package list';
  rsPackageListFrmCaption1 = 'Downloaded package list';
  rsPackageListFrmCaption2 = 'Update package list';
  rsPackageListFrmbYes = 'Yes';
  rsPackageListFrmbNo = 'No';
  rsPackageListFrmbOk = 'Ok';

  //createrepositorypackage form
  rsCreateRepositoryPackage = 'Create repository package';
  rsCreateRepositoryPackagepnMessage = 'Please wait...';
  rsCreateRepositoryPackagecbOpenCaption = 'After create open containing folder';
  rsCreateRepositoryPackagelbPackageDir = 'Select package directory:';
  rsCreateRepositoryPackagepnCaption0 = 'Available packages';
  rsCreateRepositoryPackagepnCaption1 = 'Description';
  rsCreateRepositoryPackagepnCaption2 = 'Data';
  rsCreateRepositoryPackageNoPackage = 'No packages found!';
  rsCreateRepositoryPackagelbCategory = 'Category:';
  rsCreateRepositoryPackagelbDisplayName = 'Display name';
  rsCreateRepositoryPackagelbLazCompatibility = 'Lazarus compatibility:';
  rsCreateRepositoryPackagelbFPCCompatibility = 'FPC compatibility:';
  rsCreateRepositoryPackagelbSupportedWidgetset = 'Supported widgetset:';
  rsCreateRepositoryPackagelbHomePageURL = 'Home page:';
  rsCreateRepositoryPackagelbDownloadURL = 'Download link:';
  rsCreateRepositoryPackagelbSVNURL = 'SVN';
  rsCreateRepositorySDDTitleSrc = 'Select package directory';
  rsCreateRepositorySDDTitleDst = 'Save repository package to...';
  rsCreateRepositoryPackageError0 = 'Error reading package';
  rsCreateRepositoryPackageError1 = 'Cannot creating zip file:';
  rsCreateRepositoryPackageError2 = 'Cannot create JSON file:';
  rsCreateRepositoryPackageMessage0 = 'Please select a category for package:';
  rsCreateRepositoryPackageMessage1 = 'Please enter supported lazarus versions for package:';
  rsCreateRepositoryPackageMessage2 = 'Please enter supported FPC versions for package:';
  rsCreateRepositoryPackageMessage3 = 'Please enter supported widgetsets for package:';
  rsCreateRepositoryPackageMessage4 = 'Compressing package. Please wait...';
  rsCreateRepositoryPackageMessage5 = 'Creating JSON. Please wait...';
  rsCreateRepositoryPackageMessage6 = 'Repository package successfully created.';

  //messages
  rsMessageNoPackage = 'No packages to show.';
  rsMessageDownload = 'Downloading package list. Please wait...';
  rsMessageNoRepository0 = 'Remote package repository not configured.';
  rsMessageNoRepository1 = 'Do you wish to configure it now?';
  rsMessageError0 = 'Cannot download package list. Error message:';
  rsMessageError1 = 'Invalid JSON file.';
  rsMessageError2 = 'Remote server unreachable.';
  rsNoPackageToDownload = 'Please check one or more packages!';
  rsRepositoryCleanup0 = 'This will delete all non-installed packages from local repository. Continue?';
  rsRepositoryCleanup1 = 'packages deleted!';
  rsPackageDependency0 = 'depends on package:';
  rsPackageDependency1 = 'Resolve dependency?';
  rsPackageDependency2 = 'Not resolving dependencies might lead to install failure!';
  rsPackageDependency3 = 'is in';
  rsPackageDependency4 = 'dependency list. Unchecking it might lead to installing failure. Do you wish to continue?';
  rsInstallInfoOpenPackage = 'Opening package:';
  rsInstallInfoOpenPackageSuccess = 'Success.';
  rsInstallInfoCompilePackage = 'Compiling package:';
  rsInstallInfoCompilePackageSuccess = 'Success.';
  rsInstallInfoInstallPackage = 'Installing package:';
  rsInstallInfoInstallPackageSuccess = 'Success.';
  rsInstallInfoCompleted = 'Done';
  rsInstallErrorOpenPackage = 'Cannot open package file.';
  rsInstallErrorCompilePackge = 'Cannot compile package.';
  rsInstallErrorInstallPackage = 'Cannot install package.';
  rsInstallErrorDependency0 = 'Dependecy';
  rsInstallErrorDependency1 = 'not found!';


implementation

end.









