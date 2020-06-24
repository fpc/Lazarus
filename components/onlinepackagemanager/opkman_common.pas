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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

 Author: Balázs Székely
 Abstract:
   Common functions, procedures.
}
unit opkman_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  // LCL
  Dialogs, Forms, Controls,
  // LazUtils
  LazFileUtils,
  // IdeIntf
  LazIDEIntf, PackageIntf,
  // OpkMan
  opkman_const, opkman_options;

type
  TPackageAction = (paDownloadTo, paInstall, paUpdate);

  TPackageData = class(TObject)
    FName: String;
    FPackageBaseDir: String;
    FPackageRelativePath: String;
    FFullPath: String;
  end;

const
  MaxCategories = 28;
  Categories: array[0..MaxCategories - 1] of String = (
    rsMainFrm_VSTText_PackageCategory0,
    rsMainFrm_VSTText_PackageCategory1,
    rsMainFrm_VSTText_PackageCategory2,
    rsMainFrm_VSTText_PackageCategory3,
    rsMainFrm_VSTText_PackageCategory4,
    rsMainFrm_VSTText_PackageCategory5,
    rsMainFrm_VSTText_PackageCategory6,
    rsMainFrm_VSTText_PackageCategory7,
    rsMainFrm_VSTText_PackageCategory8,
    rsMainFrm_VSTText_PackageCategory9,
    rsMainFrm_VSTText_PackageCategory10,
    rsMainFrm_VSTText_PackageCategory11,
    rsMainFrm_VSTText_PackageCategory12,
    rsMainFrm_VSTText_PackageCategory13,
    rsMainFrm_VSTText_PackageCategory14,
    rsMainFrm_VSTText_PackageCategory15,
    rsMainFrm_VSTText_PackageCategory16,
    rsMainFrm_VSTText_PackageCategory17,
    rsMainFrm_VSTText_PackageCategory18,
    rsMainFrm_VSTText_PackageCategory19,
    rsMainFrm_VSTText_PackageCategory20,
    rsMainFrm_VSTText_PackageCategory21,
    rsMainFrm_VSTText_PackageCategory22,
    rsMainFrm_VSTText_PackageCategory23,
    rsMainFrm_VSTText_PackageCategory24,
    rsMainFrm_VSTText_PackageCategory25,
    rsMainFrm_VSTText_PackageCategory26,
    rsMainFrm_VSTText_PackageCategory27);
  //needed for localized filter, since the JSON contains only english text
  CategoriesEng: array[0..MaxCategories - 1] of String = (
    'Charts and Graphs',
    'Cryptography',
    'DataControls',
    'Date and Time',
    'Dialogs',
    'Edit and Memos',
    'Files and Drives',
    'GUIContainers',
    'Graphics',
    'Grids',
    'Indicators and Gauges',
    'Labels',
    'LazIDEPlugins',
    'List and Combo Boxes',
    'ListViews and TreeViews',
    'Menus',
    'Multimedia',
    'Networking',
    'Panels',
    'Reporting',
    'Science',
    'Security',
    'Shapes',
    'Sizers and Scrollers',
    'System',
    'Tabbed Components',
    'Other',
    'Games and Game Engines');

  MaxLazVersions = 11;
  LazVersions: array [0..MaxLazVersions - 1] of String = (
    '1.8.0', '1.8.2', '1.8.4', '1.8.5',
    '2.0.0', '2.0.2', '2.0.4', '2.0.6', '2.0.8', '2.0.10',
    'Trunk');
  LazDefVersions = '2.0.0, 2.0.2, 2.0.4, 2.0.6, 2.0.8, 2.0.10';
  LazTrunk = '2.1.0';

  MaxFPCVersions = 5;
  FPCVersions: array [0..MaxFPCVersions - 1] of String = (
    '3.0.0', '3.0.2', '3.0.4', '3.2.0',
    'Trunk');
  FPCDefVersion = '3.0.0, 3.0.2, 3.0.4, 3.2.0';
  FPCTrunk = '3.3.1';

  DefWidgetSets = 'gtk2, win32/win64';

var
  LocalRepositoryConfigFile: String;
  LocalRepositoryUpdatesFile: String;
  PackageAction: TPackageAction;
  InstallPackageList: TObjectList;
  CriticalSection: TRTLCriticalSection;
  CurLazVersion: String;
  CurFPCVersion: String;
  CurWidgetSet: String;

function MessageDlgEx(const AMsg: String; ADlgType: TMsgDlgType;  AButtons:
  TMsgDlgButtons; AParent: TForm): TModalResult;
procedure InitLocalRepository;
function SecToHourAndMin(const ASec: LongInt): String;
function FormatSize(Size: Int64): String;
function FormatSpeed(Speed: LongInt): String;
function GetPackageTypeString(aPackageType: TLazPackageType): String;
function GetDirSize(const ADirName: String; var AFileCnt, ADirCnt: Integer): Int64;
procedure FindPackages(const ADirName: String; APackageList: TStrings);
procedure FindAllFilesEx(const ADirName: String; AFileList: TStrings);
function FixProtocol(const AURL: String): String;
function IsDirectoryEmpty(const ADirectory: String): Boolean;

implementation

function MessageDlgEx(const AMsg: string; ADlgType: TMsgDlgType;
  AButtons: TMsgDlgButtons; AParent: TForm): TModalResult;
var
  MsgFrm: TForm;
begin
  MsgFrm := CreateMessageDialog(AMsg, ADlgType, AButtons);
  try
    MsgFrm.FormStyle := fsSystemStayOnTop;
    if AParent <> nil then
    begin
      MsgFrm.Position := poDefaultSizeOnly;
      MsgFrm.Left := AParent.Left + (AParent.Width - MsgFrm.Width) div 2;
      MsgFrm.Top := AParent.Top + (AParent.Height - MsgFrm.Height) div 2;
    end
    else
      MsgFrm.Position := poWorkAreaCenter;
    Result := MsgFrm.ShowModal;
  finally
    MsgFrm.Free
  end;
end;

procedure InitLocalRepository;
var
  LocalRepo, LocalRepoConfig: String;
begin
  LocalRepo := AppendPathDelim(AppendPathDelim(LazarusIDE.GetPrimaryConfigPath) + cLocalRepository);
  if not DirectoryExists(LocalRepo) then
    CreateDir(LocalRepo);

  LocalRepoConfig := AppendPathDelim(LocalRepo + cLocalRepositoryConfig);
  if not DirectoryExists(LocalRepoConfig) then
    CreateDir(LocalRepoConfig);
  LocalRepositoryConfigFile := LocalRepoConfig + cLocalRepositoryConfigFile;
  LocalRepositoryUpdatesFile := LocalRepoConfig + cLocalRepositoryUpdatesFile;
end;

function SecToHourAndMin(const ASec: LongInt): String;
var
  Hour, Min, Sec: LongInt;
begin
   Hour := Trunc(ASec/3600);
   Min  := Trunc((ASec - Hour*3600)/60);
   Sec  := ASec - Hour*3600 - 60*Min;
   Result := IntToStr(Hour) + 'h: ' + IntToStr(Min) + 'm: ' + IntToStr(Sec) + 's';
end;

function FormatSize(Size: Int64): String;
const
  KB = 1024;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if Size < KB then
    Result := FormatFloat('#,##0 Bytes', Size)
  else if Size < MB then
    Result := FormatFloat('#,##0.0 KB', Size / KB)
  else if Size < GB then
    Result := FormatFloat('#,##0.0 MB', Size / MB)
  else
    Result := FormatFloat('#,##0.0 GB', Size / GB);
end;

function FormatSpeed(Speed: LongInt): String;
const
  KB = 1024;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if Speed < KB then
    Result := FormatFloat('#,##0 bits/s', Speed)
  else if Speed < MB then
    Result := FormatFloat('#,##0.0 kB/s', Speed / KB)
  else if Speed < GB then
    Result := FormatFloat('#,##0.0 MB/s', Speed / MB)
  else
    Result := FormatFloat('#,##0.0 GB/s', Speed / GB);
end;

function GetPackageTypeString(aPackageType: TLazPackageType): String;
begin
  case aPackageType of
    lptRunAndDesignTime: Result := rsMainFrm_VSTText_PackageType0;
    lptDesignTime:       Result := rsMainFrm_VSTText_PackageType1;
    lptRunTime:          Result := rsMainFrm_VSTText_PackageType2;
    lptRunTimeOnly:      Result := rsMainFrm_VSTText_PackageType3;
  end;
end;

function GetDirSize(const ADirName: String; var AFileCnt, ADirCnt: Integer): Int64;
var
  DirSize: Int64;

  procedure GetSize(const ADirName: String);
  var
    SR: TSearchRec;
    DirName: String;
  begin
    DirName := AppendPathDelim(ADirName);
    if FindFirst(DirName + '*', faAnyFile - faDirectory, SR) = 0 then
    begin
      try
        repeat
          Inc(AFileCnt);
          DirSize:= DirSize + SR.Size;
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;
    if FindFirst(DirName + '*', faAnyFile, SR) = 0 then
    begin
      try
        repeat
          if ((SR.Attr and faDirectory) <> 0)  and (SR.Name <> '.') and (SR.Name <> '..') then
           begin
             Inc(ADirCnt);
             GetSize(DirName + SR.Name);
           end;
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;
  end;
begin
  DirSize := 0;
  AFileCnt := 0;
  ADirCnt := 0;
  GetSize(ADirName);
  Result := DirSize;
end;

procedure FindPackages(const ADirName: String; APackageList: TStrings);
var
  BaseDir, BasePath: String;
  SLExcludedFolders: TStringList;

  function IsAllowed(AName: String): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to SLExcludedFolders.Count - 1 do
    begin
      if UpperCase(SLExcludedFolders.Strings[I]) = UpperCase(AName) then
        begin
          Result := False;
          Break;
        end;
    end;
  end;

  procedure FindFiles(const ADirName: String);
  var
    SR: TSearchRec;
    Path: String;
    PackageData: TPackageData;
  begin
    Path := AppendPathDelim(ADirName);
    if FindFirst(Path + '*', faAnyFile - faDirectory, SR) = 0 then
    begin
      try
        repeat
          if (UpperCase(ExtractFileExt(SR.Name)) = UpperCase('.lpk')) then
          begin
            PackageData := TPackageData.Create;
            PackageData.FName := SR.Name;
            PackageData.FPackageBaseDir := BaseDir;
            PackageData.FPackageRelativePath := StringReplace(Path, BasePath, '', [rfIgnoreCase, rfReplaceAll]);
            if Trim(PackageData.FPackageRelativePath) <> '' then
            begin
              if PackageData.FPackageRelativePath[Length(PackageData.FPackageRelativePath)] = PathDelim then
                PackageData.FPackageRelativePath := Copy(PackageData.FPackageRelativePath, 1, Length(PackageData.FPackageRelativePath) - 1);
              PackageData.FPackageRelativePath := PackageData.FPackageRelativePath;
            end;
            PackageData.FFullPath := Path + SR.Name;
            APackageList.AddObject(PackageData.FName, PackageData);
          end;
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;

    if FindFirst(Path + '*', faAnyFile, SR) = 0 then
    begin
      try
        repeat
          if ((SR.Attr and faDirectory) <> 0) and (SR.Name <> '.') and (SR.Name <> '..') then
            if IsAllowed(SR.Name) then
              FindFiles(Path + SR.Name);
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;
  end;

begin
  BasePath := AppendPathDelim(ADirName);
  if ADirName[Length(ADirName)] = PathDelim then
    BaseDir := ExtractFileName(Copy(ADirName, 1, Length(ADirName) - 1))
  else
    BaseDir := ExtractFileName(ADirName);

  SLExcludedFolders := TStringList.Create;
  try
    SLExcludedFolders.Delimiter := ',';
    SLExcludedFolders.StrictDelimiter := True;
    SLExcludedFolders.DelimitedText := Options.ExcludedFolders;
    FindFiles(ADirName);
  finally
    SLExcludedFolders.Free;
  end;
end;

procedure FindAllFilesEx(const ADirName: String; AFileList: TStrings);
var
  SLExcludedFiles: TStringList;
  SLExcludedFolders: TStringList;

  function IsAllowed(const AName: String; const AIsDir: Boolean): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    if not AIsDir then
    begin
      for I := 0 to SLExcludedFiles.Count - 1 do
      begin
        if UpperCase(SLExcludedFiles.Strings[I]) = UpperCase(ExtractFileExt(AName)) then
        begin
          Result := False;
          Break;
        end;
      end;
    end
    else
    begin
      for I := 0 to SLExcludedFolders.Count - 1 do
        if UpperCase(SLExcludedFolders.Strings[I]) = UpperCase(AName) then
        begin
          Result := False;
          Break;
        end;
    end;
  end;

  procedure FindFiles(const ADirName: String);
  var
    SR: TSearchRec;
    Path: String;
  begin
    Path := AppendPathDelim(ADirName);
    if FindFirst(Path + '*', faAnyFile - faDirectory, SR) = 0 then
    begin
      try
        repeat
           if IsAllowed(SR.Name, False) then
             AFileList.Add(Path + SR.Name);
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;

    if FindFirst(Path + '*', faAnyFile, SR) = 0 then
    begin
      try
        repeat
          if ((SR.Attr and faDirectory) <> 0) and (SR.Name <> '.') and (SR.Name <> '..') then
            if IsAllowed(SR.Name, True) then
              FindFiles(Path + SR.Name);
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;
  end;

var
  I, P: Integer;
  Ext: String;
begin
  SLExcludedFiles := TStringList.Create;
  try
    SLExcludedFiles.Delimiter := ',';
    SLExcludedFiles.StrictDelimiter := True;
    SLExcludedFiles.DelimitedText := Options.ExcludedFiles;
    for I := 0 to SLExcludedFiles.Count - 1 do
    begin
      P := Pos('*.', SLExcludedFiles.Strings[I]);
      if P > 0 then
      begin
        Ext := Copy(SLExcludedFiles.Strings[I], 2, Length(SLExcludedFiles.Strings[I]));
        if Trim(Ext) = '.' then
          Ext := '';
      end
      else
        Ext := '.' + SLExcludedFiles.Strings[I];
      SLExcludedFiles.Strings[I] := Ext;
    end;
    SLExcludedFolders := TStringList.Create;
    try
      SLExcludedFolders.Delimiter := ',';
      SLExcludedFolders.StrictDelimiter := True;
      SLExcludedFolders.DelimitedText := Options.ExcludedFolders;
      FindFiles(ADirName);
    finally
      SLExcludedFolders.Free;
    end;
  finally
    SLExcludedFiles.Free;
  end;
end;

function FixProtocol(const AURL: String): String;
begin
  Result := AURL;
  if (Pos('http://', Result) = 0) and (Pos('https://', Result) = 0) then
    Result := 'https://' + Result;
end;

function IsDirectoryEmpty(const ADirectory: String): Boolean;
var
  SearchRec: TSearchRec;
  SearchRes: Longint;
begin
  Result := true;
  SearchRes := FindFirst(IncludeTrailingPathDelimiter(ADirectory) + AllFilesMask, faAnyFile, SearchRec);
  try
    while SearchRes = 0 do
    begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        Result := False;
        Break;
      end;
      SearchRes := FindNext(SearchRec);
    end;
  finally
    SysUtils.FindClose(SearchRec);
  end;
end;

end.

