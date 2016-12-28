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
  cRemoteRepository = 'http://packages.lazarus-ide.org/';
  cRemoteJSONFile = 'packagelist.json';
  cLocalRepository =  'onlinepackagemanager';
  cLocalRepositoryPackages = 'packages';
  cLocalRepositoryArchive = 'archive';
  cLocalRepositoryUpdate = 'update';
  cLocalRepositoryConfig = 'config';
  cLocalRepositoryConfigFile = 'options.xml';
  cLocalRepositoryUpdatesFile = 'updates.xml';
  cExcludedFilesDef = '*.,*.a,*.o,*.ppu,*.compiled,*.bak,*.or,*.rsj,*.~,*.exe,*.dbg,*.zip,*.json';
  cExcludedFoldersDef = 'lib,backup,updates,compiled,.git,.svn';
  cHelpPage = 'http://wiki.freepascal.org/Online_Package_Manager';
  cHelpPage_CreateRepositoryPackage = 'http://wiki.freepascal.org/Online_Package_Manager#Create_repository_package';
  cHelpPage_CreateExternalJSON = 'http://wiki.freepascal.org/Online_Package_Manager#Create_JSON_for_updates';
  {$ifdef win64}
  cOpenSSLURL = 'http://packages.lazarus-ide.org/openssl-1.0.2j-x64_86-win64.zip';
  {$endif}
  {$ifdef win32}
  cOpenSSLURL = 'http://packages.lazarus-ide.org/openssl-1.0.2j-i386-win32.zip';
  {$endif}
  cExtractDir = 'ExtractDir';

resourcestring
  //package manager
  rsLazarusPackageManager = 'Online Package Manager';

  //main form
  rsPackagesFound = '(%s packages found)';
  rsMainFrm_VSTHeaderColumn_PackageName = 'Packages';
  rsMainFrm_VSTHeaderColumn_PackageFile = 'Package file (.lpk)';
  rsMainFrm_VSTHeaderColumn_Repository = 'Repository';
  rsMainFrm_VSTHeaderColumn_Installed = 'Installed';
  rsMainFrm_VSTHeaderColumn_Update = 'Update';
  rsMainFrm_VSTHeaderColumn_Data = 'Status/Data';
  rsMainFrm_VSTHeaderColumn_Button = '';
  rsMainFrm_VSTHeaderColumn_Rating = 'Rating';
  rsMainFrm_VSTText_PackageCategory = 'Package category';
  rsMainFrm_VSTText_PackageStatus = 'Package status';
  rsMainFrm_VSTText_Version = 'Version';
  rsMainFrm_VSTText_Description = 'Description';
  rsMainFrm_VSTText_Author = 'Author';
  rsMainFrm_VSTText_LazCompatibility = 'Lazarus compatibility';
  rsMainFrm_VSTText_FPCCompatibility = 'FPC compatibility';
  rsMainFrm_VSTText_SupportedWidgetsets = 'Supported widgetsets';
  rsMainFrm_VSTText_Packagetype = 'Package type';
  rsMainFrm_VSTText_Dependecies = 'Dependencies';
  rsMainFrm_VSTText_License = 'License';
  rsMainFrm_VSTText_PackageInfo = 'Package info';
  rsMainFrm_VSTText_Category = 'Category';
  rsMainFrm_VSTText_RepositoryFilename = 'Repository filename';
  rsMainFrm_VSTText_RepositoryFileSize = 'Repository filesize';
  rsMainFrm_VSTText_RepositoryFileHash = 'Repository filehash';
  rsMainFrm_VSTText_RepositoryFileDate = 'Available since';
  rsMainFrm_VSTText_HomePageURL = 'Home page';
  rsMainFrm_VSTText_DownloadURL = 'Update link (JSON)';
  rsMainFrm_VSTText_SVNURL = 'SVN';
  rsMainFrm_VSTText_Install0 = 'No';
  rsMainFrm_VSTText_Install1 = 'Yes';
  rsMainFrm_VSTText_Install2 = 'Partially';
  rsMainFrm_VSTText_PackageType0 = 'Designtime and runtime';
  rsMainFrm_VSTText_PackageType1 = 'Designtime';
  rsMainFrm_VSTText_PackageType2 = 'Runtime';
  rsMainFrm_VSTText_PackageType3 = 'Runtime only, cannot be installed in IDE';
  rsMainFrm_VSTText_PackageState0 = 'Repository';
  rsMainFrm_VSTText_PackageState1 = 'Downloaded';
  rsMainFrm_VSTText_PackageState2 = 'Extracted';
  rsMainFrm_VSTText_PackageState3 = 'Installed';
  rsMainFrm_VSTText_PackageState4 = 'Up to date';
  rsMainFrm_VSTText_PackageState5 = 'New version available (repository)';
  rsMainFrm_VSTText_PackageState6 = 'New updates available';
  rsMainFrm_VSTText_PackageCategory0  = 'Charts and Graphs';
  rsMainFrm_VSTText_PackageCategory1  = 'Cryptography';
  rsMainFrm_VSTText_PackageCategory2  = 'DataControls';
  rsMainFrm_VSTText_PackageCategory3  = 'Date and Time';
  rsMainFrm_VSTText_PackageCategory4  = 'Dialogs';
  rsMainFrm_VSTText_PackageCategory5  = 'Edit and Memos';
  rsMainFrm_VSTText_PackageCategory6  = 'Files and Drives';
  rsMainFrm_VSTText_PackageCategory7  = 'GUIContainers';
  rsMainFrm_VSTText_PackageCategory8  = 'Graphics';
  rsMainFrm_VSTText_PackageCategory9  = 'Grids';
  rsMainFrm_VSTText_PackageCategory10 = 'Indicators and Gauges';
  rsMainFrm_VSTText_PackageCategory11 = 'Labels';
  rsMainFrm_VSTText_PackageCategory12 = 'LazIDEPlugins';
  rsMainFrm_VSTText_PackageCategory13 = 'List and Combo Boxes';
  rsMainFrm_VSTText_PackageCategory14 = 'ListViews and TreeViews';
  rsMainFrm_VSTText_PackageCategory15 = 'Menus';
  rsMainFrm_VSTText_PackageCategory16 = 'Multimedia';
  rsMainFrm_VSTText_PackageCategory17 = 'Networking';
  rsMainFrm_VSTText_PackageCategory18 = 'Panels';
  rsMainFrm_VSTText_PackageCategory19 = 'Reporting';
  rsMainFrm_VSTText_PackageCategory20 = 'Science';
  rsMainFrm_VSTText_PackageCategory21 = 'Security';
  rsMainFrm_VSTText_PackageCategory22 = 'Shapes';
  rsMainFrm_VSTText_PackageCategory23 = 'Sizers and Scrollers';
  rsMainFrm_VSTText_PackageCategory24 = 'System';
  rsMainFrm_VSTText_PackageCategory25 = 'Tabbed Components';
  rsMainFrm_VSTText_PackageCategory26 = 'Other';
  rsMainFrm_VSTText_Desc = 'Description for package';
  rsMainFrm_VSTText_Lic = 'License info for package';
  rsMainFrm_cbAll_Caption = 'All/None';
  rsMainFrm_cbAll_Hint = 'Check/Uncheck packages';
  rsMainFrm_lbFilter_Caption = 'Filter by:';
  rsMainFrm_cbFilterBy_Hint = 'Filter package list by:';
  rsMainFrm_edFilter_Hint = 'Type filter text';
  rsMainFrm_spClear_Hint = 'Clear filter text';
  rsMainFrm_spExpand_Hint = 'Expand package tree';
  rsMainFrm_spCollapse_Hint = 'Collapse package tree';
  rsMainFrm_TBRefresh_Caption = 'Refresh';
  rsMainFrm_TBRefresh_Hint = 'Refresh package list';
  rsMainFrm_TBDownload_Caption = 'Download';
  rsMainFrm_TBDownload_Hint = 'Download packages';
  rsMainFrm_TBInstall_Caption = 'Install';
  rsMainFrm_TBInstall_Hint = 'Install packages';
  rsMainFrm_TBUpdate_Caption = 'Update';
  rsMainFrm_TBUpdate_Hint = 'Update packages from external URL';
  rsMainFrm_TBCleanUp_Caption = 'Cleanup';
  rsMainFrm_TBCleanUp_Hint = 'Cleanup local repository';
  rsMainFrm_TBRepository_Caption = 'Create';
  rsMainFrm_TBRepository_Hint = 'Create package or repository';
  rsMainFrm_TBOptions_Caption = 'Options';
  rsMainFrm_TBOptions_Hint = 'Show options dialog';
  rsMainFrm_TBHelp_Caption = 'Help';
  rsMainFrm_TBHelp_Hint = 'Help (' + cHelpPage + ')';
  rsMainFrm_miCreateRepositoryPackage = 'Create repository package';
  rsMainFrm_miCreateJSONForUpdates = 'Create JSON for updates';
  rsMainFrm_miJSONShow =  'Show JSON';
  rsMainFrm_miJSONHide = 'Hide JSON';
  rsMainFrm_miCopyToClpBrd = 'Copy to clipboard';
  rsMainFrm_miResetRating = 'Reset rating';
  rsMainFrm_PackagenameAlreadyExists = 'A package with the same name already exists!';
  rsMainFrm_PackageAlreadyInstalled = 'The following packages are already installed. Continue anyway?';
  rsMainFrm_PackageAlreadyDownloaded = 'The following repository packages already exist in the target folder. Continue?';
  rsMainFrm_PackageUpdateWarning = 'Updating packages from external link is not without a risk!' + sLineBreak + 'Only update if you trust the package maintainer. Continue?';
  rsMainFrm_PackageUpdate0 = 'The following repository packages are not installed or don''t have a valid external download link. The packages will be skipped. Continue?';
  rsMainFrm_PackageUpdate1 = 'None of the checked repository packages is installed or has a valid external download link.';
  rsMainFrm_rsMessageNoPackage = 'No packages to show.';
  rsMainFrm_rsMessageParsingJSON = 'Parsing JSON. Please wait...';
  rsMainFrm_rsMessageDownload = 'Downloading package list. Please wait...';
  rsMainFrm_rsMessageNoRepository0 = 'Remote package repository not configured.' + sLineBreak + 'Do you wish to configure it now?';
  rsMainFrm_rsMessageError0 = 'Cannot download package list. Error message:';
  rsMainFrm_rsMessageError1 = 'Invalid JSON file.';
  rsMainFrm_rsMessageError2 = 'Remote server unreachable.';
  rsMainFrm_rsNoPackageToDownload = 'Please check one or more packages!';
  rsMainFrm_rsRepositoryCleanup0 = 'This will delete all non-installed packages from local repository. Continue?';
  rsMainFrm_rsRepositoryCleanup1 = '%s packages deleted!';
  rsMainFrm_rsPackageDependency0 = 'Package "%s" depends on package "%s". '
    +'Resolve dependency?';
  rsMainFrm_rsPackageDependency1 = 'Not resolving dependencies might lead to install failure!';
  rsMainFrm_rsPackageRating = 'Your vote for package "%s" is: %s. Thank you for voting!';

  //progress form
  rsProgressFrm_Caption0 = 'Downloading packages';
  rsProgressFrm_Caption1 = 'Extracting packages';
  rsProgressFrm_Caption2 = 'Installing packages';
  rsProgressFrm_Caption3 = 'Updating packages';
  rsProgressFrm_Caption4 = '. Please wait...';
  rsProgressFrm_Caption5 = 'Unknown';
  rsProgressFrm_lbPackage_Caption = 'Package:';
  rsProgressFrm_lbSpeed_Caption = 'Speed:';
  rsProgressFrm_lbSpeedCalc_Caption = 'Estimating. Please wait...';
  rsProgressFrm_lbElapsed_Caption = 'Elapsed:';
  rsProgressFrm_lbRemaining_Caption = 'Remaining:';
  rsProgressFrm_lbReceived_Caption0 = 'Received:';
  rsProgressFrm_lbReceived_Caption1 = 'Unzipped:';
  rsProgressFrm_lbReceivedTotal_Caption0 = 'Received (total):';
  rsProgressFrm_lbReceivedTotal_Caption1 = 'Unzipped (total):';
  rsProgressFrm_cbExtractOpen_Caption0 = 'Extract after download';
  rsProgressFrm_cbExtractOpen_Caption1 = 'Open containing folder';
  rsProgressFrm_Error0 = 'Cannot download package:';
  rsProgressFrm_Error1 = 'Error message:';
  rsProgressFrm_Error2 = 'Cannot extract package:';
  rsProgressFrm_Error3 = 'Cannot install package:';
  rsProgressFrm_Error4 = 'Dependency "%s" not found!';
  rsProgressFrm_Error5 = 'Cannot contact download site';
  rsProgressFrm_Error6 = 'No valid download link found.';
  rsProgressFrm_Error7 = 'Cannot open package file.';
  rsProgressFrm_Error8 = 'Cannot compile package.';
  rsProgressFrm_Error9 = 'Cannot install package.';
  rsProgressFrm_Conf0 = 'Continue with next one?';
  rsProgressFrm_Info0 = 'Installing package:';
  rsProgressFrm_Info1 = 'Success.';
  rsProgressFrm_Info2 = 'Contacting download site for "%s" (%s)';
  rsProgressFrm_Info3 = 'Preparing to download. Please wait...';
  rsProgressFrm_Info4 = 'Canceling. Please wait...';
  rsProgressFrm_Info5 = 'Opening package:';
  rsProgressFrm_Info6 = 'Compiling package:';

  //options form
  rsOptions_FrmCaption = 'Options';
  rsOptions_tsGeneral_Caption = 'General';
  rsOptions_tsProxy_Caption = 'Proxy';
  rsOptions_tsFolders_Caption = 'Folders';
  rsOptions_tsProfiles_Caption = 'Profiles';
  rsOptions_lbRemoteRepository_Caption = 'Remote repository';
  rsOptions_cbForceDownloadExtract_Caption = 'Always force download and extract';
  rsOptions_cbForceDownloadExtract_Hint = 'If this option is checked the packages are always re-downloaded/extracted before install';
  rsOptions_lbSelectProfile_Caption = 'Select profile';
  rsOptions_cbSelectProfile_Item0 = 'Regular user';
  rsOptions_cbSelectProfile_Item1 = 'Package maintainer';
  rsOptions_cbSelectProfile_Hint = 'Choose a profile that best fits you';
  rsOptions_cbDelete_Caption = 'Delete downloaded zip files after installation/update';
  rsOptions_cbDelete_Hint = 'If this option is checked the downloaded zip file is always deleted after installation';
  rsOptions_cbProxy_Caption = 'Use proxy';
  rsOptions_gbProxySettings_Caption = 'Proxy settings';
  rsOptions_lbServer_Caption = 'Server';
  rsOptions_lbPort_Caption = 'Port';
  rsOptions_lbUsername_Caption = 'Username';
  rsOptions_lbPassword_Caption = 'Password';
  rsOptions_lbLocalRepositoryPackages_Caption = 'Local repository';
  rsOptions_edLocalRepositoryPackages_Hint = 'The folder where the repository packages are extracted/installed';
  rsOptions_lbLocalRepositoryArchive_Caption = 'Archive directory';
  rsOptions_edLocalRepositoryArchive_Hint = 'The folder where the zip files are downloaded from the remote repository';
  rsOptions_lbLocalRepositoryUpdate_Caption = 'Update directory';
  rsOptions_edLocalRepositoryUpdate_Hint = 'The folder where the zip files are downloaded from the package maintainer webpage';
  rsOptions_RemoteRepository_Information = 'Please enter the remote repository address!';
  rsOptions_ProxyServer_Info = 'Please enter the proxy server address!';
  rsOptions_ProxyPort_Info = 'Please enter the proxy server port!';
  rsOptions_InvalidDirectory_Info = 'Please enter a valid directory!';
  rsOptions_RestoreDefaults_Conf = 'This will restore the default settings. Continue?';
  rsOptions_lbCheckForUpdates_Caption = 'Check for package updates';
  rsOptions_cbCheckForUpdates_Item0 = 'Every few minutes';
  rsOptions_cbCheckForUpdates_Item1 = 'Every hour';
  rsOptions_cbCheckForUpdates_Item2 = 'Once per day';
  rsOptions_cbCheckForUpdates_Item3 = 'Weekly';
  rsOptions_cbCheckForUpdates_Item4 = 'Montly';
  rsOptions_cbCheckForUpdates_Item5 = 'Never';
  rsOptions_lbLastUpdate_Caption = 'Last update: ';
  rsOptions_LastUpdate_Never = 'never';
  rsOptions_lbFilterFiles_Caption = 'Excluded files (packages)';
  rsOptions_lbFilterDirs_Caption = 'Excluded folders (packages)';
  rsOptions_bAdd_Caption = 'Add';
  rsOptions_bEdit_Caption = 'Edit';
  rsOptions_bDelete_Caption = 'Delete';
  rsOptions_lbExcludeFiles_Hint = 'These files will be excluded from repository packages (see: "Create repository package")';
  rsOptions_lbExcludeFolders_Hint = 'These folders will be excluded from repository packages (see: "Create repository package")';
  rsOptions_InputBox_Caption = 'Add new exclusion';
  rsOptions_InputBox_Text0 = 'Type the extension name:';
  rsOptions_InputBox_Text1 = 'Type the folder name:';
  rsOptions_InputBox_Info0 = 'Please select a file extension!';
  rsOptions_InputBox_Info1 = 'Please select a folder!';
  rsOptions_InputBox_Conf0 = 'Delete selected extension ("%s")?';
  rsOptions_InputBox_Conf1 = 'Delete selected folder ("%s")?';


  //packagelist form
  rsPackageListFrm_Caption0 = 'Installed package list';
  rsPackageListFrm_Caption1 = 'Downloaded package list';
  rsPackageListFrm_Caption2 = 'Update package list';
  rsPackageListFrm_bYes_Caption = 'Yes';
  rsPackageListFrm_bNo_Caption = 'No';
  rsPackageListFrm_bOk_Caption = 'OK';

  //createrepositorypackage form
  rsCreateRepositoryPackageFrm_Caption = 'Create repository package';
  rsCreateRepositoryPackageFrm_pnMessage_Caption = 'Please wait...';
  rsCreateRepositoryPackageFrm_cbOpen_Caption = 'Open containing folder after creation';
  rsCreateRepositoryPackageFrm_lbPackageDir_Caption = 'Package directory:';
  rsCreateRepositoryPackageFrm_pnCaption_Caption0 = 'Available packages';
  rsCreateRepositoryPackageFrm_pnCaption_Caption1 = 'Description';
  rsCreateRepositoryPackageFrm_pnCaption_Caption2 = 'Data';
  rsCreateRepositoryPackageFrm_NoPackage = 'No packages found!';
  rsCreateRepositoryPackageFrm_lbCategory_Caption = 'Category:';
  rsCreateRepositoryPackageFrm_lbDisplayName_Caption = 'Display name:';
  rsCreateRepositoryPackageFrm_lbLazCompatibility_Caption = 'Lazarus compatibility:';
  rsCreateRepositoryPackageFrm_lbFPCCompatibility_Caption = 'FPC compatibility:';
  rsCreateRepositoryPackageFrm_lbSupportedWidgetset_Caption = 'Supported widgetsets:';
  rsCreateRepositoryPackageFrm_lbHomePageURL_Caption = 'Home page:';
  rsCreateRepositoryPackageFrm_lbDownloadURL_Caption = 'Update link (JSON):';
  rsCreateRepositoryPackageFrm_lbSVNURL_Caption = 'SVN:';
  rsCreateRepositoryPackageFrm_SDDTitleSrc = 'Select package directory';
  rsCreateRepositoryPackageFrm_SDDTitleDst = 'Save repository package to...';
  rsCreateRepositoryPackageFrm_Error0 = 'Error reading package';
  rsCreateRepositoryPackageFrm_Error1 = 'Cannot create zip file:';
  rsCreateRepositoryPackageFrm_Error2 = 'Cannot create JSON file:';
  rsCreateRepositoryPackageFrm_Message0 = 'Please select a category for package:';
  rsCreateRepositoryPackageFrm_Message1 = 'Please enter supported Lazarus versions for package:';
  rsCreateRepositoryPackageFrm_Message2 = 'Please enter supported FPC versions for package:';
  rsCreateRepositoryPackageFrm_Message3 = 'Please enter supported widgetsets for package:';
  rsCreateRepositoryPackageFrm_Message4 = 'Compressing package. Please wait...';
  rsCreateRepositoryPackageFrm_Message5 = 'Creating JSON. Please wait...';
  rsCreateRepositoryPackageFrm_Message6 = 'Repository package successfully created.';
  rsCreateRepositoryPackageFrm_bHelp_Caption = 'Help';
  rsCreateRepositoryPackageFrm_bOptions_Caption = 'Options';
  rsCreateRepositoryPackageFrm_bCreate_Caption = 'Create';
  rsCreateRepositoryPackageFrm_bCancel_Caption = 'Cancel';

  //createupdatejson
  rsCreateJSONForUpdatesFrm_Caption = 'Create update JSON for package:';
  rsCreateJSONForUpdatesFrm_bHelp_Caption = 'Help';
  rsCreateJSONForUpdatesFrm_bCreate_Caption = 'Create';
  rsCreateJSONForUpdatesFrm_bClose_Caption = 'Cancel';
  rsCreateJSONForUpdatesFrm_cbOpen_Caption = 'Open containing folder after creation';
  rsCreateJSONForUpdatesFrm_lbLinkToZip_Caption = 'Link to the package zip file';
  rsCreateJSONForUpdatesFrm_bTest_Caption = 'Test';
  rsCreateJSONForUpdatesFrm_Column0_Text = 'PackageFileName';
  rsCreateJSONForUpdatesFrm_Column1_Text = 'Version';
  rsCreateJSONForUpdatesFrm_Column2_Text = 'Force notify';
  rsCreateJSONForUpdatesFrm_Column3_Text = 'Internal version';
  rsCreateJSONForUpdatesFrm_Message0 = 'Please check a repository package!';
  rsCreateJSONForUpdatesFrm_Message1 = 'Please check only one repository package!';
  rsCreateJSONForUpdatesFrm_Message2 = 'Please enter a valid URL!';
  rsCreateJSONForUpdatesFrm_Message3 = 'Please check at least one package file!';
  rsCreateJSONForUpdatesFrm_Message4 = 'JSON for updates successfully created.';
  rsCreateJSONForUpdatesFrm_Error1 = 'Cannot create JSON for updates! Error message:';


  //categories form
  rsCategoriesFrm_Caption = 'List with categories';
  rsCategoriesFrm_lbMessage_Caption = 'Please select (check) one or more categories';
  rsCategoriesFrm_bYes_Caption = 'OK';
  rsCategoriesFrm_bCancel_Caption = 'Cancel';


implementation

end.

































