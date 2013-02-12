{
 /***************************************************************************
                            LazUtilsStrConsts.pas
                            ----------------
     This unit contains all resource strings from LazUtils


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit LazUtilsStrConsts;

{$mode objfpc}{$H+}

interface

resourceString
  lrsModified = '  modified ';
  lrsInvalidCharSet = 'The char set in mask "%s" is not valid!';
  lrsSize = '  size ';
  lrsFileDoesNotExist = 'file "%s" does not exist';
  lrsFileIsADirectoryAndNotAnExecutable = 'file "%s" is a directory and not an'
    +' executable';
  lrsReadAccessDeniedFor = 'read access denied for %s';
  lrsADirectoryComponentInDoesNotExistOrIsADanglingSyml2 = 'a directory '
    +'component in %s does not exist or is a dangling symlink';
  lrsADirectoryComponentInIsNotADirectory2 = 'a directory component in %s is '
    +'not a directory';
  lrsADirectoryComponentInDoesNotExistOrIsADanglingSyml = 'a directory '
    +'component in %s does not exist or is a dangling symlink';
  lrsADirectoryComponentInIsNotADirectory = 'a directory component in %s is '
    +'not a directory';
  lrsInsufficientMemory = 'insufficient memory';
  lrsHasACircularSymbolicLink = '%s has a circular symbolic link';
  lrsIsNotASymbolicLink = '%s is not a symbolic link';
  lrsIsNotExecutable = '%s is not executable';
  lrsUnableToCreateConfigDirectoryS = 'Unable to create config directory "%s"';
  lrsProgramFileNotFound = 'program file not found %s';
  lrsCanNotExecute = 'can not execute %s';

implementation

end.

