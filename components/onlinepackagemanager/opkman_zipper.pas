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
  Implementation of the package zipper class.
}
unit opkman_zipper;

{$mode objfpc}{$H+}

{$INCLUDE opkman_fpcdef.inc}

interface

uses
  Classes, SysUtils, strutils,
  // LazUtils
  FileUtil, LazFileUtils,
  // OpkMan
  opkman_serializablepackages, opkman_common,
  {$IFDEF FPC320}zipper{$ELSE}opkman_zip{$ENDIF};

type
  TOnProgressEx = procedure(Sender : TObject; const ATotPos, ATotSize: Int64);
  TOnZipProgress = procedure(Sender: TObject; AZipfile: String; ACnt, ATotCnt: Integer; ACurPos, ACurSize, ATotPos, ATotSize: Int64;
    AElapsed, ARemaining, ASpeed: LongInt) of object;
  TOnZipError = procedure(Sender: TObject; APackageName: String; const AErrMsg: String) of object;
  TOnZipCompleted = TNotifyEvent;

  { TPackageUnzipper }

  TPackageUnzipper = class(TThread)
  private
    FSrcDir: String;
    FDstDir: String;
    FStarted: Boolean;
    FNeedToBreak: Boolean;
    FZipFile: String;
    FCnt: Integer;
    FTotCnt: Integer;
    FCurPos: Int64;
    FCurSize: Int64;
    FTotPos: Int64;
    FTotPosTmp: Int64;
    FTotSize: Int64;
    FElapsed: QWord;
    FStartTime: QWord;
    FRemaining: Integer;
    FSpeed: Integer;
    FErrMsg: String;
    FIsUpdate: Boolean;
    FUnZipper: TUnZipper;
    FOnZipProgress: TOnZipProgress;
    FOnZipError: TOnZipError;
    FOnZipCompleted: TOnZipCompleted;
    procedure DoOnProgressEx(Sender : TObject; const ATotPos, {%H-}ATotSize: Int64);
    procedure DoOnZipProgress;
    procedure DoOnZipError;
    procedure DoOnZipCompleted;
    function GetZipSize(var AIsDirZipped: Boolean; var ABaseDir: String): Int64;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartUnZip(const ASrcDir, ADstDir: String; const AIsUpdate: Boolean = False);
    procedure StopUnZip;
  published
    property OnZipProgress: TOnZipProgress read FOnZipProgress write FOnZipProgress;
    property OnZipError: TOnZipError read FOnZipError write FOnZipError;
    property OnZipCompleted: TOnZipCompleted read FOnZipCompleted write FOnZipCompleted;
  end;

  { TPackageZipper }

  TPackageZipper = class(TThread)
  private
    FZipper: TZipper;
    FSrcDir: String;
    FZipFile: String;
    FStarted: Boolean;
    FNeedToBreak: Boolean;
    FErrMsg: String;
    FOnZipError: TOnZipError;
    FOnZipCompleted: TOnZipCompleted;
    procedure DoOnZipError;
    procedure DoOnZipCompleted;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartZip(const ASrcDir, AZipFile: String);
    procedure StopZip;
  published
    property OnZipError: TOnZipError read FOnZipError write FOnZipError;
    property OnZipCompleted: TOnZipCompleted read FOnZipCompleted write FOnZipCompleted;
  end;

var
  PackageUnzipper: TPackageUnzipper = nil;

implementation

{ TPackageUnZipper }

procedure TPackageUnzipper.DoOnZipProgress;
begin
  if Assigned(FOnZipProgress) then
    FOnZipProgress(Self, FZipfile, FCnt, FTotCnt, FCurPos, FCurSize, FTotPosTmp, FTotSize, FElapsed, FRemaining, FSpeed);
end;

procedure TPackageUnzipper.DoOnZipError;
begin
  if Assigned(FOnZipError) then
    FOnZipError(Self, FZipFile, FErrMsg);
end;

procedure TPackageUnzipper.DoOnZipCompleted;
begin
  if Assigned(FOnZipCompleted) then
    FOnZipCompleted(Self);
end;

procedure TPackageUnzipper.Execute;
var
  I: Integer;
  DelDir: String;
begin
  Sleep(50);
  FCnt := 0;
  FStartTime := GetTickCount64;
  for I := 0 to SerializablePackages.Count - 1 do
  begin
    if SerializablePackages.Items[I].IsExtractable then
    begin
      if FNeedToBreak then
        Break;
      Inc(FCnt);
      FCurPos := 0;
      FZipFile := SerializablePackages.Items[I].RepositoryFileName;
      DelDir := FDstDir + SerializablePackages.Items[I].PackageBaseDir;
      if FIsUpdate then
        if DirectoryExists(DelDir) then
          DeleteDirectory(DelDir, False);
      try
        FUnZipper.Clear;
        FUnZipper.FileName := FSrcDir + SerializablePackages.Items[I].RepositoryFileName;
        if SerializablePackages.Items[I].IsDirZipped then
          FUnZipper.OutputPath := FDstDir
        else
          FUnZipper.OutputPath := FDstDir + SerializablePackages.Items[I].PackageBaseDir;
        FUnZipper.OnProgressEx := @DoOnProgressEx;
        FUnZipper.Examine;
        FUnZipper.UnZipAllFiles;
        SerializablePackages.Items[I].ChangePackageStates(ctAdd, psExtracted);
        if (SerializablePackages.Items[I].IsDirZipped ) and (UpperCase(SerializablePackages.Items[I].PackageBaseDir) <> UpperCase(SerializablePackages.Items[I].ZippedBaseDir)) then
        begin
          CopyDirTree(FUnZipper.OutputPath + SerializablePackages.Items[I].ZippedBaseDir, DelDir, [cffOverwriteFile]);
          DeleteDirectory(FUnZipper.OutputPath + SerializablePackages.Items[I].ZippedBaseDir, False);
        end;
        Synchronize(@DoOnZipProgress);
        FTotPos := FTotPos + FCurSize;
      except
        on E: Exception do
        begin
          FErrMsg := E.Message;
          SerializablePackages.Items[I].ChangePackageStates(ctRemove, psExtracted);
          SerializablePackages.Items[I].ChangePackageStates(ctAdd, psError);
          DeleteDirectory(DelDir, False);
          Synchronize(@DoOnZipError);
        end;
      end;
    end;
  end;
  if (FNeedToBreak) then
  begin
    if DirectoryExists(DelDir) then
      DeleteDirectory(DelDir, False)
  end
  else
  begin
    SerializablePackages.MarkRuntimePackages;
    Synchronize(@DoOnZipCompleted);
  end;
end;

constructor TPackageUnzipper.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FUnZipper := TUnZipper.Create;
end;

destructor TPackageUnzipper.Destroy;
begin
  FUnZipper.Free;
  inherited Destroy;
end;

procedure TPackageUnzipper.DoOnProgressEx(Sender : TObject; const ATotPos, ATotSize: Int64);
begin
  FElapsed := GetTickCount64 - FStartTime;
  if FElapsed < 1000 then
    Exit;
  FElapsed := FElapsed div 1000;

  FCurPos := ATotPos;
  FCurSize := ATotSize;
  FTotPosTmp := FTotPos + FCurPos;
  FSpeed := Round(FTotPosTmp/FElapsed);
  if FSpeed > 0 then
    FRemaining := Round((FTotSize - FTotPosTmp)/FSpeed);
  Synchronize(@DoOnZipProgress);
  Sleep(5);
end;

function TPackageUnzipper.GetZipSize(var AIsDirZipped: Boolean; var ABaseDir: String): Int64;
var
  I: Integer;
  Item: TFullZipFileEntry;
  AllFiles: Boolean;
  P: Integer;
begin
  FUnZipper.Examine;
  AllFiles := (FUnZipper.Files.Count = 0);
  Result := 0;
  if FUnZipper.Entries.Count > 0 then
  begin
    P := Pos('/', TZipFileEntry(FUnZipper.Entries.Items[0]).ArchiveFileName);
    if P = 0 then
      P := Pos('\', TZipFileEntry(FUnZipper.Entries.Items[0]).ArchiveFileName);
    if P <> 0 then
      ABaseDir := Copy(TZipFileEntry(FUnZipper.Entries.Items[0]).ArchiveFileName, 1, P);
  end;
  for I := 0 to FUnZipper.Entries.Count-1 do
  begin
      Item := FUnZipper.Entries[i];
      if AllFiles or (FUnZipper.Files.IndexOf(Item.ArchiveFileName)<>-1) then
      begin
        Result := Result + TZipFileEntry(Item).Size;
        if AIsDirZipped then
          if Pos(ABaseDir, Item.ArchiveFileName) = 0 then
            AIsDirZipped := False;
      end;
  end;
  if not AIsDirZipped then
    ABaseDir := ''
  else
    ABaseDir := Copy(ABaseDir, 1, Length(ABaseDir) - 1);
end;


procedure TPackageUnzipper.StartUnZip(const ASrcDir, ADstDir: String;
  const AIsUpdate: Boolean);
var
  I: Integer;
  IsDirZipped: Boolean;
  BaseDir: String;
begin
  if FStarted then
    Exit;
  FDstDir := ADstDir;
  FSrcDir := ASrcDir;
  FTotCnt := 0;
  FTotSize := 0;
  FIsUpdate := AIsUpdate;
  for I := 0 to SerializablePackages.Count - 1 do
  begin
    if SerializablePackages.Items[I].IsExtractable then
    begin
      try
        FUnZipper.Clear;
        FUnZipper.FileName := FSrcDir + SerializablePackages.Items[I].RepositoryFileName;
        FUnZipper.Examine;
        IsDirZipped := True;
        BaseDir := '';
        FTotSize := FTotSize + GetZipSize(IsDirZipped, BaseDir);
        SerializablePackages.Items[I].IsDirZipped := IsDirZipped;
        if BaseDir <> '' then
          BaseDir := AppendPathDelim(BaseDir);
        SerializablePackages.Items[I].ZippedBaseDir := BaseDir;
        Inc(FTotCnt);
      except
        on E: Exception do
        begin
          FZipFile := SerializablePackages.Items[I].RepositoryFileName;
          FErrMsg := E.Message;
          Synchronize(@DoOnZipError);
        end;
      end
    end;
  end;
  FStarted := True;
  Start;
end;

procedure TPackageUnzipper.StopUnZip;
begin
  if Assigned(FUnZipper) then
    FUnZipper.Terminate;
  FNeedToBreak := True;
  FStarted := False;
end;


{ TPackageZipper }

procedure TPackageZipper.DoOnZipError;
begin
  if Assigned(FOnZipError) then
    FOnZipError(Self, FZipFile, FErrMsg);
end;

procedure TPackageZipper.DoOnZipCompleted;
begin
  if Assigned(FOnZipCompleted) then
    FOnZipCompleted(Self);
end;

procedure TPackageZipper.Execute;
var
  PathEntry: String;
  ZipFileEntries: TZipFileEntries;
  FileList: TStringList;
  SrcDir: String;
  P, I: Integer;
  CanGo: Boolean;
begin
  CanGo := True;
  FZipper.Filename := FZipFile;
  SrcDir := FSrcDir;
  FZipper.Clear;
  ZipFileEntries := TZipFileEntries.Create(TZipFileEntry);
  try
    if DirPathExists(SrcDir) then
    begin
      P := RPos(PathDelim, ChompPathDelim(SrcDir));
      PathEntry := LeftStr(SrcDir, P);
      FileList := TStringList.Create;
      try
        FindAllFilesEx(SrcDir, FileList);
        for I := 0 to FileList.Count - 1 do
          ZipFileEntries.AddFileEntry(FileList[I], CreateRelativePath(FileList[I], PathEntry));
      finally
        FileList.Free;
      end;

      if (ZipFileEntries.Count > 0) then
      begin
        try
          FZipper.ZipFiles(ZipFileEntries);
        except
          on E: EZipError do
          begin
            CanGo := False;
            FErrMsg := E.Message;
            Synchronize(@DoOnZipError);
          end;
        end;
      end;
    end;
  finally
    ZipFileEntries.Free;
  end;
  if CanGo then
    Synchronize(@DoOnZipCompleted);
end;

constructor TPackageZipper.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FZipper := TZipper.Create;
end;

destructor TPackageZipper.Destroy;
begin
  FZipper.Free;
  inherited Destroy;
end;

procedure TPackageZipper.StartZip(const ASrcDir, AZipFile: String);
begin
  if FStarted then
    Exit;
  FSrcDir := ASrcDir;
  FZipFile := AZipFile;
  FStarted := True;
  Start;
end;

procedure TPackageZipper.StopZip;
begin
  if Assigned(FZipper) then
    FZipper.Terminate;
  FNeedToBreak := True;
  FStarted := False;
end;

end.

