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
unit IdeUtilsPkgStrConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  lisFileNotText = 'File not text';
  lisUnableToRenameFile = 'Unable to rename file';
  lisUnableToCopyFile = 'Unable to copy file';
  lisWriteError = 'Write Error';
  lisFileDoesNotLookLikeATextFileOpenItAnyway = 'File "%s"'
    +'%sdoes not look like a text file.'
    +'%sOpen it anyway?';
  lisSourceAndDestinationAreTheSame = 'Source and Destination are the same:%s%s';
  lisUnableToWriteFileError = 'Unable to write file "%s"%sError: %s';
  lisOverwriteFile = 'Overwrite file?';
  lisAFileAlreadyExistsReplaceIt = 'A file "%s" already exists.'
    +'%sReplace it?';
  lisOverwriteFileOnDisk = 'Overwrite file on disk';
  lisReadError = 'Read Error';
  lisUnableToReadFile = 'Unable to read file';
  lisUnableToReadFile2 = 'Unable to read file "%s".';
  lisUnableToReadFileError = 'Unable to read file "%s"%sError: %s';
  lisUnableToRenameFileTo = 'Unable to rename file "%s" to "%s"!';
  lisUnableToRenameFileTo2= 'Unable to rename file "%s"%sto "%s".';
  lisUnableToCreateFile = 'Unable to create file';
  lisUnableToCreateFile2 = 'Unable to create file "%s"';
  lisUnableToCreateFile3 = 'Unable to create file%s"%s"';
  lisUnableToCopyFileTo = 'Unable to copy file "%s"%sto "%s"';
  lisUnableToWrite2 = 'Unable to write "%s"';
  lisUnableToWriteFile = 'Unable to write file';
  lisUnableToWriteFile2 = 'Unable to write file "%s".';
  lisUnableToWriteToFile2 = 'Unable to write to file "%s".';
  lisFileIsNotWritable = 'File is not writable';
  lisCodeToolsDefsWriteError = 'Write error';
  lisCCOErrorCaption = 'Error';
  lisErrorLoadingFrom = 'Error loading %s from%s%s%s%s';
  lisErrorSavingTo = 'Error saving %s to%s%s%s%s';
  lisXMLError = 'XML Error';
  lisXMLParserErrorInFileError = 'XML parser error in file %s%sError: %s';
  lisUnableToWriteXmlStreamToError = 'Unable to write xml stream to %s%sError: %s';
  lisWriteErrorFile = 'Write error: %s%sFile: %s%s%s';
  lisFileIsSymlink = 'File is symlink';
  lisTheFileIsASymlinkOpenInstead = 'The file "%s" is a symlink.'
    +'%sOpen "%s" instead?';
  lisOpenTarget = 'Open target';
  lisOpenSymlink = 'Open symlink';
  lisDirectoryNotWritable = 'Directory not writable';
  lisTheDirectoryIsNotWritable = 'The directory "%s" is not writable.';
  lisPkgMangUnableToCreateDirectory = 'Unable to create directory';
  lisUnableToCreateDirectory = 'Unable to create directory "%s".';
  lisPkgMangUnableToDeleteFile = 'Unable to delete file "%s".';
  lisDeleteFileFailed = 'Delete file failed';
  lisTheCodetoolsFoundAnError = 'The Codetools found an error:%s%s';
  lisNotImplemented = 'Not implemented';
  lisNotImplementedYet = 'Not implemented yet:%s%s';
  lisEnvOptDlgDirectoryNotFound = 'Directory not found';
  lisIgnoreAndContinue = 'Ignore and continue';
  lisUnableToCreateLinkWithTarget = 'Unable to create link "%s" with target "%s"';

  // Languages
  rsLanguageAutomatic    = 'Automatic (or English)';
  rsLanguageEnglish      = 'English';
  rsLanguageGerman       = 'German';
  rsLanguageSpanish      = 'Spanish';
  rsLanguageFrench       = 'French';
  rsLanguageRussian      = 'Russian';
  rsLanguagePolish       = 'Polish';
  rsLanguageItalian      = 'Italian';
  rsLanguageCatalan      = 'Catalan';
  rsLanguageFinnish      = 'Finnish';
  rsLanguageHebrew       = 'Hebrew';
  rsLanguageArabic       = 'Arabic';
  rsLanguagePortugueseBr = 'Brazilian Portuguese';
  rsLanguagePortuguese   = 'Portuguese';
  rsLanguageUkrainian    = 'Ukrainian';
  rsLanguageDutch        = 'Dutch';
  rsLanguageJapanese     = 'Japanese';
  rsLanguageChinese      = 'Chinese';
  rsLanguageIndonesian   = 'Indonesian';
  rsLanguageAfrikaans    = 'Afrikaans';
  rsLanguageLithuanian   = 'Lithuanian';
  rsLanguageSlovak       = 'Slovak';
  rsLanguageTurkish      = 'Turkish';
  rsLanguageCzech        = 'Czech';
  rsLanguageHungarian    = 'Hungarian';
  rsLanguageCorsican     = 'Corsican';
  rsLanguageSinhala      = 'Sinhala';

implementation

end.

