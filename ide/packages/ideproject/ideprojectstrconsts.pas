unit IdeProjectStrConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  lisFileNotFound2 = 'File "%s" not found.';

  lisCouldNotAddToMainSource = 'Could not add "%s" to main source!';
  lisCouldNotRemoveRFromMainSource = 'Could not remove "{$R %s}" from main source!';
  lisCouldNotAddRToMainSource = 'Could not add "{$R %s}" to main source!';
  lisCouldNotRemoveIFromMainSource = 'Could not remove "{$I %s}" from main source!';
  lisCouldNotAddIToMainSource = 'Could not add "{$I %s}" to main source!';

  lisCodeToolsDefsReadError = 'Read error';
  dlgProjectOptions = 'Project Options';
  lisStrangeLpiFile = 'Strange lpi file';
  lisTheFileDoesNotLookLikeALpiFile = 'The file %s does not look like a lpi file.';
  lisUnableToReadTheProjectInfoFile = 'Unable to read the project info file%s"%s".';
  lisUnableToReadLpi = 'Unable to read lpi';
  lisUnableToWriteTheProjectInfoFileError = 'Unable to write the project info '
    +'file%s"%s".%sError: %s';
  lisUnableToWriteTheProjectSessionFileError = 'Unable to write the project session '
    +'file%s"%s".%sError: %s';
  lisExtendUnitSearchPathOfProjectWith = 'Extend unit search path of project '
    +'with%s"%s"?';
  lisProjMangUnableToReadStateFileOfProjectError = 'Unable to read state '
    +'file %s of project %s%sError: %s';
  lisProjMangUnableToWriteStateFileForProjectError = 'Unable to write state '
    +'file for project %s%sError: %s';

  // Save icon in application options
  lisSaveIconToFile = 'Save icon to file';
  lisMultipleImagesInfo = 'An icon can contain images at several sizes, ' +
    'but the selected file format allows only a single image per file.';
  lisHowToProceed = 'How to proceed?';
  lisSaveOnlyCurrentImageSize = 'Save only current image size';
  lisSaveAllImageSizesToIndividualFiles = 'Save all image sizes to individual files';

  // package defs
  lisPkgDefsNamespaces = 'Namespaces';
  lisProjProjectSourceDirectoryMark = 'Project Source Directory Mark';

implementation

end.

