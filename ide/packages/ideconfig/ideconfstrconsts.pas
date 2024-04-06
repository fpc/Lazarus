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
  // Transfer Macros
  lisTMFunctionExtractFileExtension = 'Function: extract file extension';
  lisTMFunctionExtractFilePath = 'Function: extract file path';
  lisTMFunctionExtractFileNameExtension = 'Function: extract file name+extension';
  lisTMFunctionExtractFileNameOnly = 'Function: extract file name only';
  lisTMFunctionAppendPathDelimiter = 'Function: append path delimiter';
  lisTMFunctionChompPathDelimiter = 'Function: remove trailing path delimiter';
  lisTMunknownMacro = '(unknown macro: %s)';
  lisComponentNameIsNotAValidIdentifier = 'Component name "%s" is not a valid identifier';
  lisComponentNameIsAPascalKeyword = 'Component name "%s" is a Pascal keyword.';
  lisEndlessLoopInMacros = 'Endless loop in macros';


implementation

end.

