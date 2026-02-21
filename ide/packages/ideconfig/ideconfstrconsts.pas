{
*****************************************************************************
 See the file COPYING.modifiedLGPL.txt, included in this distribution,
 for details about the license.
*****************************************************************************
}
unit IdeConfStrConsts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  // compiler
  lisCompilerErrorInvalidCompiler = 'Error: invalid compiler: %s';
  lisCompilerHintYouCanSetTheCompilerPath = 'Hint: you can set the compiler '
    +'executable in Tools -> Options-> Files -> Compiler executable';
  lisCompileProject = 'Compile Project';
  lisMode = ', Mode: %s';
  lisOS = ', OS: %s';
  lisCPU = ', CPU: %s';
  lisTarget2 = ', Target: %s';
  // Transfer Macros
  lisTMFunctionExtractFileExtension = 'Function: extract file extension';
  lisTMFunctionExtractFilePath = 'Function: extract file path';
  lisTMFunctionExtractFileNameExtension = 'Function: extract file name+extension';
  lisTMFunctionExtractFileNameOnly = 'Function: extract file name only';
  lisTMFunctionAppendPathDelimiter = 'Function: append path delimiter';
  lisTMFunctionChompPathDelimiter = 'Function: remove trailing path delimiter';
  lisTMFunctionEncloseBrackets = 'Function: enclose in ()';
  lisTMunknownMacro = '(unknown macro: %s)';
  lisEndlessLoopInMacros = 'Endless loop in macros';
  // Component Name Validity
  lisComponentNameIsNotAValidIdentifier = 'Component name "%s" is not a valid identifier';
  lisComponentNameIsAPascalKeyword = 'Component name "%s" is a Pascal keyword.';
  // edit define tree
  lisEdtDefAllPackages = 'All packages';
  lisEdtDefsAllProjects = 'All projects';
  lisEdtDefsetFPCModeToDELPHI = 'set FPC mode to DELPHI';
  lisEdtDefsetFPCModeToTP = 'set FPC mode to TP';
  lisEdtDefsetFPCModeToMacPas = 'set FPC mode to MacPas';
  lisEdtDefsetFPCModeToFPC = 'set FPC mode to FPC';
  lisEdtDefsetIOCHECKSOn = 'set IOCHECKS on';
  lisEdtDefsetRANGECHECKSOn = 'set RANGECHECKS on';
  lisEdtDefsetOVERFLOWCHECKSOn = 'set OVERFLOWCHECKS on';
  lisEdtDefuseLineInfoUnit = 'use LineInfo unit';
  lisEdtDefuseHeapTrcUnit = 'use HeapTrc unit';
  // Fppkg checks
  lisFppkgRtlNotFound = 'Fppkg reports that the RTL is not installed.';
  lisFppkgCompilerNotExists = 'The compiler [%s] configured for Fppkg does not exist.';
  lisFppkgCompilerNotExecutable = 'The compiler [%s] configured for Fppkg is not an executable.';
  lisFppkgCompilerNotFound = 'Could not find the compiler [%s] configured for Fppkg.';
  // Mostly DialogProcs
  lisUnableToRenameFile = 'Unable to rename file';
  lisUnableToRenameFileTo = 'Unable to rename file "%s" to "%s"!';
  lisUnableToRenameFileTo2= 'Unable to rename file "%s"%sto "%s".';
  lisUnableToCopyFile = 'Unable to copy file';
  lisUnableToCopyFileTo = 'Unable to copy file "%s"%sto "%s"';
  lisSourceAndDestinationAreTheSame = 'Source and Destination are the same:%s%s';
  lisFileNotText = 'File not text';
  lisFileDoesNotLookLikeATextFileOpenItAnyway = 'File "%s"'
    +'%sdoes not look like a text file.'
    +'%sOpen it anyway?';
  lisWriteError = 'Write Error';
  lisReadError = 'Read Error';
  lisUnableToReadFile = 'Unable to read file';
  lisUnableToReadFile2 = 'Unable to read file "%s".';
  lisUnableToReadFileError = 'Unable to read file "%s"%sError: %s';
  lisCodeToolsDefsWriteError = 'Write error';
  lisUnableToWrite2 = 'Unable to write "%s"';
  lisUnableToWriteFile = 'Unable to write file';
  lisUnableToWriteFile2 = 'Unable to write file "%s".';
  lisUnableToWriteToFile2 = 'Unable to write to file "%s".';
  lisCCOErrorCaption = 'Error';
  lisErrorLoadingFrom = 'Error loading %s from%s%s%s%s';
  lisErrorSavingTo = 'Error saving %s to%s%s%s%s';
  lisXMLError = 'XML Error';
  lisXMLParserErrorInFileError = 'XML parser error in file %s%sError: %s';
  lisUnableToWriteXmlStreamToError = 'Unable to write xml stream to %s%sError: %s';
  lisUnableToCreateFile = 'Unable to create file';
  lisUnableToCreateFile2 = 'Unable to create file "%s"';
  lisUnableToCreateFile3 = 'Unable to create file%s"%s"';
  lisOverwriteFile = 'Overwrite file?';
  lisOverwriteFileOnDisk = 'Overwrite file on disk';
  lisAFileAlreadyExistsReplaceIt = 'A file "%s" already exists.'
    +'%sReplace it?';
  lisUnableToWriteFileError = 'Unable to write file "%s"%sError: %s';
  lisFileIsNotWritable = 'File is not writable';
  lisFileIsSymlink = 'File is symlink';
  lisTheFileIsASymlinkOpenInstead = 'The file "%s" is a symlink.'
    +'%sOpen "%s" instead?';
  lisOpenTarget = 'Open target';
  lisOpenSymlink = 'Open symlink';
  lisUnableToCreateLinkWithTarget = 'Unable to create link "%s" with target "%s"';
  lisPkgMangUnableToCreateDirectory = 'Unable to create directory';
  lisUnableToCreateDirectory = 'Unable to create directory "%s".';
  lisDirectoryNotWritable = 'Directory not writable';
  lisTheDirectoryIsNotWritable = 'The directory "%s" is not writable.';
  lisPkgMangUnableToDeleteFile = 'Unable to delete file "%s".';
  lisDeleteFileFailed = 'Delete file failed';
  lisWriteErrorFile = 'Write error: %s%sFile: %s%s%s';
  lisNotImplemented = 'Not implemented';
  lisNotImplementedYet = 'Not implemented yet:%s%s';
  lisEnvOptDlgDirectoryNotFound = 'Directory not found';
  lisIgnoreAndContinue = 'Ignore and continue';
  lisTheCodetoolsFoundAnError = 'The Codetools found an error:%s%s';

implementation

end.

