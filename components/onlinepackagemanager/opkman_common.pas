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
   Common functions, procedures.
}
unit opkman_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Forms, Controls, LazIDEIntf, LazFileUtils, contnrs,
  opkman_const;

type
  TPackageAction = (paDownloadTo, paInstall, paUpdate);

  TPackageData = class(TObject)
    FName: String;
    FPackageBaseDir: String;
    FPackageRelativePath: String;
    FFullPath: String;
  end;

const
  MaxCategories = 9;
  Categories: array[0..MaxCategories - 1] of String = (
    rsMainFrm_VSTText_PackageCategory0,
    rsMainFrm_VSTText_PackageCategory1,
    rsMainFrm_VSTText_PackageCategory2,
    rsMainFrm_VSTText_PackageCategory3,
    rsMainFrm_VSTText_PackageCategory4,
    rsMainFrm_VSTText_PackageCategory5,
    rsMainFrm_VSTText_PackageCategory6,
    rsMainFrm_VSTText_PackageCategory7,
    rsMainFrm_VSTText_PackageCategory8);

var
  LocalRepositoryConfigFile: String;
  PackageAction: TPackageAction;
  InstallPackageList: TObjectList;

function MessageDlgEx(const AMsg: String; ADlgType: TMsgDlgType;  AButtons:
  TMsgDlgButtons; AParent: TForm): TModalResult;
procedure InitLocalRepository;
function SecToHourAndMin(const ASec: LongInt): String;
function FormatSize(Size: Int64): String;
function FormatSpeed(Speed: LongInt): String;
function GetDirSize(const ADirName: String; var AFileCnt, ADirCnt: Integer): Int64;
procedure FindPackages(const ADirName: String; APackageList: TStrings);
procedure FindAllFilesEx(const ADirName: String; AFileList: TStrings);

implementation

function MessageDlgEx(const AMsg: string; ADlgType: TMsgDlgType;
  AButtons: TMsgDlgButtons; AParent: TForm): TModalResult;
var
  MsgFrm: TForm;
begin
  MsgFrm := CreateMessageDialog(AMsg, ADlgType, AButtons);
  try
    MsgFrm.Position := poDefaultSizeOnly;
    MsgFrm.FormStyle := fsSystemStayOnTop;
    MsgFrm.Left := AParent.Left + (AParent.Width - MsgFrm.Width) div 2;
    MsgFrm.Top := AParent.Top + (AParent.Height - MsgFrm.Height) div 2;
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
  if not DirectoryExistsUTF8(LocalRepo) then
    CreateDirUTF8(LocalRepo);

  LocalRepoConfig := AppendPathDelim(LocalRepo + cLocalRepositoryConfig);
  if not DirectoryExists(LocalRepoConfig) then
    CreateDirUTF8(LocalRepoConfig);
  LocalRepositoryConfigFile := LocalRepoConfig + cLocalRepositoryConfigFile;
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
          if UpperCase(ExtractFileExt(SR.Name)) = UpperCase('.lpk') then
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
  FindFiles(ADirName);
end;

procedure FindAllFilesEx(const ADirName: String; AFileList: TStrings);

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
            FindFiles(Path + SR.Name);
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;
  end;

begin
  FindFiles(ADirName);
end;

end.

