{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Abstract:
   Useful functions for IDE add ons.
}
unit IDEUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  StdCtrls, ImgList,
  LazStringUtils,
  IDEIntfUtils;

// Moved and deprecated in Lazarus 4.99 in April 2026. Delete the whole unit later.
type
  TCmpStrType = LazStringUtils.TCmpStrType deprecated 'Use from unit LazStringUtils instead.';

function IndexInStringList(List: TStrings; Cmp: TCmpStrType; s: string): integer;
                              deprecated 'Use from unit LazStringUtils instead.';
procedure SetComboBoxText(AComboBox: TComboBox; const AText: String;
  Cmp: TCmpStrType; MaxCount: integer = 1000); deprecated 'Use from unit IDEIntfUtils instead.';
function LoadProjectIconIntoImages(const ProjFile: string;
  const Images: TCustomImageList; const Index: TStringList): Integer; deprecated 'Use from unit IDEIntfUtils instead.';

{ Tests aFileDirStr and try to return the longest best path match on system.
  Returns longest valid path. aoFileName contains the remainder of the supplied
  aFileDirStr that couldn't be included in result  }
function GetValidDirectory(const aFileDirStr: string;
  out aoFileName : string): string; deprecated 'Use from unit IDEIntfUtils instead.';

{ Tests aFileDirStr and try to return best path/filename on system. }
function GetValidDirectoryAndFilename(const aFileDirStr: string;
  out aoFileName : string): string; deprecated 'Use from unit IDEIntfUtils instead.';


implementation

function IndexInStringList(List: TStrings; Cmp: TCmpStrType; s: string): integer;
begin
  Result := LazStringUtils.IndexInStringList(List, Cmp, s);
end;

procedure SetComboBoxText(AComboBox:TComboBox; const AText: String;
  Cmp: TCmpStrType; MaxCount: integer);
begin
  IDEIntfUtils.SetComboBoxText(AComboBox, AText, Cmp, MaxCount);
end;

function LoadProjectIconIntoImages(const ProjFile: string;
  const Images: TCustomImageList; const Index: TStringList): Integer;
begin
  Result := IDEIntfUtils.LoadProjectIconIntoImages(ProjFile, Images, Index);
end;

function GetValidDirectory(const aFileDirStr: string; out aoFileName: string): string;
begin
  Result := IDEIntfUtils.GetValidDirectory(aFileDirStr, aoFileName);
end;

function GetValidDirectoryAndFilename(const aFileDirStr: string;
  out aoFileName: string): string;
begin
  Result := IDEIntfUtils.GetValidDirectoryAndFilename(aFileDirStr, aoFileName);
end;

end.

