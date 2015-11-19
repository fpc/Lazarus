unit ProjectGroupStrConst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Resourcestring
  lisErrTargetDoesNotExist      = 'Target does not exist. Remove ?';
  lisErrNoSuchFile              = 'Could not find target file'+sLineBreak+
                                  '"%s"'+sLineBreak+
                                  'What do you want to do ?';
  lisRemoveTarget                = 'Remove target';
  lisAbortLoadingProjectGroup    = 'Abort loading project group';
  lisSkipAllTargets              = 'Remove all invalid targets';
  lisErrOnlyProjectGroupAllowed  = 'Only target type "projectgroup" is allowed for root project group';
  lisLazarusProjectGroup = 'Lazarus project group';
  lisAllFiles = 'All files';
  lisProjectGroupModified        = 'Project group modified';
  lisProjectGroupModifiedConfirm = 'Project group "%s" is modified.'+sLineBreak+
                                   'what do you want to do?';

  lisSavePG  = 'Save project group';
  lisDiscard = 'Discard changes';
  lisAbort   = 'Abort';

  lisTargetAdd          = 'Add target';
  lisTargetRemove       = 'Remove target';
  lisTargetCompile      = 'Compile';
  lisTargetCompileClean = 'Compile clean';
  lisTargetCompileFromHere = 'Compile from here';
  lisTargetInstall      = 'Install';
  lisTargetUnInstall    = 'Uninstall';
  lisTargetActivate     = 'Activate target';
  lisTargetOpen         = 'Open Target';
  lisTargetRun          = 'Run Target';
  lisTargetProperties   = 'Target properties';
  lisTargetLater        = 'Compile target later';
  lisTargetEarlier      = 'Compile target earlier';
  lisNewProjectGroup    = 'New project group';
  lisOpenProjectGroup   = 'Open project group';
  lisOpenRecentProjectGroup = 'Open recent project group';
  lisSaveProjectGroup   = 'Save project group';
  lisSaveProjectGroupAs = 'Save project group as';
  lisTargetCopyFilename = 'Copy file name';

  lisProjectGroup            = 'Project group %s';
  lisNodeTargets             = 'Targets';
  lisNodeRemovedTargets      = 'Removed targets';
  lisNodeBuildModes          = 'Build Modes';
  lisNodeFiles               = 'Files';
  lisNodeRemovedFiles        = 'Removed files';
  lisNodeDependencies        = 'Dependencies';
  lisNodeRemovedDependencies = 'Removed dependencies';
  lisTargetCount             = '%d targets';
  lisActiveTarget            = 'Target: %s';

  lisProjectGroupSaveCaption   = 'Save';
  lisProjectGroupSaveHint      = 'Save project group';
  lisProjectGroupSaveAsCaption = 'Save As';
  lisProjectGroupSaveAsHint    = 'Save project group with a new name';
  lisProjectGroupAddExistingCaption = 'Add';
  lisProjectGroupAddExistingHint    = 'Add existing target to project group';
  lisProjectGroupDeleteCaption = 'Remove';
  lisProjectGroupDeleteHint    = 'Remove target from project group';
  lisProjectGroupAddNewCaption = 'New';
  lisProjectGroupAddNewHint    = 'Add new target to project group';
  lisTargetEarlierCaption      = 'Earlier';
  lisTargetEarlierHint         = 'Build target earlier';
  lisTargetLaterCaption        = 'Later';
  lisTargetLaterHint           = 'Build target later';
  lisTargetCompileCaption      = 'Compile';
  lisTargetCompileHint         = 'Compile selected target';
  lisTargetCompileCleanCaption = 'Compile clean';
  lisTargetCompileCleanHint    = 'Compile selected target clean';
  lisTargetPropertiesCaption   = 'Properties';
  lisTargetPropertiesHint      = 'Show property dialog for selected target';
  lisTargetRunCaption          = 'Run';
  lisTargetRunHint             = 'Run selected target';
  lisTargetInstallCaption      = 'Install';
  lisTargetInstallHint         = 'Install selected target';
  lisTargetUninstallCaption    = 'Uninstall';
  lisTargetUninstallHint       = 'Uninstall selected target';
  lisTargetActivateCaption     = 'Activate';
  lisTargetActivateHint        = 'Activate selected target';
  lisTargetOpenCaption         = 'Open';
  lisTargetOpenHint            = 'Open selected target';

implementation

end.

