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

 Abstract:
   form for showing the diffs of editor files changed on disk
}
unit DiskDiffsDialog;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL
  Classes, SysUtils,
  // LCL
  LCLProc, LCLType, Forms, StdCtrls, ExtCtrls, CheckLst, ButtonPanel, Buttons,
  // CodeTools
  FileProcs, CodeCache,
  // LazUtils
  LazFileUtils, UITypes, LazFileCache,
  // IdeIntf
  IDEImagesIntf,
  // SynEdit
  SynEdit, SynHighlighterDiff,
  // IDE
  Project, PackageDefs, DiffPatch, LazarusIDEStrConsts, EnvironmentOpts, EditorOptions;

type
  PDiffItem = ^TDiffItem;
  TDiffItem = record
    Valid: boolean;
    Code: TCodeBuffer;
    Owner: TObject;
    Diff: string;
    TxtOnDisk: string;
  end;

  { TDiskDiffsDlg }

  TDiskDiffsDlg = class(TForm)
    BtnPanel: TButtonPanel;
    CheckDiskChangesWithLoadingCheckBox: TCheckBox;
    DiffSynEdit: TSynEdit;
    FilesListBox: TCheckListBox;
    WarnImage: TPaintBox;
    WarnLabel: TLabel;
    Splitter: TSplitter;
    SynDiffSyn1: TSynDiffSyn;
    procedure FilesListBoxClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure WarnImagePaint(Sender: TObject);
  private
    FIgnoreList: TFPList;
    FPackageList: TStringList;
    FCodeList: TFPList;
    FHasLocalModifications: Boolean;
    FHasExistingFiles: Boolean;
    FCachedDiffs: TFPList; // List of PDiffItem
    function ShortenFilename(const aFilename: string): string;
    procedure AddFile2Box(AInfo: TObject; AFileName: string; AModified: Boolean);
    procedure FillFilesListBox;
    procedure ApplyChecks;
    procedure ShowDiff;
    function GetCachedDiff(FileOwner: TObject; AltFilename: string): PDiffItem;
    procedure ClearCache;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property CodeList: TFPList read FCodeList write FCodeList; // list of TCodeBuffer
    property PackageList: TStringList read FPackageList write FPackageList; // list of alternative filename and TLazPackage
    property IgnoreList: TFPList read FIgnoreList write FIgnoreList; // TCodeBuffer of TLazPackage
  end;

function ShowDiskDiffsDialog(
  ACodeList: TFPList; // list of TCodeBuffer
  APackageList: TStringList; // list of TLazPackage
  AnIgnoreList: TFPList): TModalResult;

implementation

{$R *.lfm}

var
  DiskDiffsDlg: TDiskDiffsDlg = nil;

procedure CheckUnits(ACodeList, AnIgnoreList: TFPList);

  function AddChangedBuffer(Code: TCodeBuffer): boolean;
  var
    fs: TFileStream;
    s, DiskEncoding, MemEncoding, aFilename: string;
  begin
    if (Code=nil) or Code.IsVirtual then
      exit(false);

    aFilename:=Code.Filename;
    if EnvironmentOptions.CheckDiskChangesWithLoading then
    begin
      // load and compare
      try
        fs := TFileStream.Create(aFilename, fmOpenRead or fmShareDenyNone);
        try
          SetLength(s{%H-}, fs.Size);
          if s <> '' then
            fs.Read(s[1], length(s));
          DiskEncoding := '';
          MemEncoding := '';
          Code.CodeCache.OnDecodeLoaded(Code,aFilename,
            s,DiskEncoding,MemEncoding);
          //debugln(['CheckUnitsWithLoading ',aFilename,
          //  ' ',length(s),'=',Code.SourceLength]);
          if (MemEncoding=Code.MemEncoding)
          and (DiskEncoding=Code.DiskEncoding)
          and (length(s)=Code.SourceLength)
          and (s=Code.Source) then begin
            // same content -> no need to bother user
            exit(false);
          end;
        finally
          fs.Free;
        end;
      except
        // unable to load, e.g. no longer exists or permission denied
      end;
    end;

    // file has changed
    Result:=true;
  end;

var
  i: Integer;
begin
  for i:=ACodeList.Count-1 downto 0 do
  begin
    if not AddChangedBuffer(TCodeBuffer(ACodeList[i])) then
      AnIgnoreList.Add(ACodeList[i]);
  end;
end;

procedure CheckPackages(APackageList: TStringList; AnIgnoreList: TFPList);
var
  i: Integer;
  CurPackage: TLazPackage;
  PackageOk: Boolean;
  fs: TFileStream;
  CurSource, DiskSource: string;
  AltFilename, LPKFilename: String;
begin
  for i:=APackageList.Count-1 downto 0 do
  begin
    AltFilename:=APackageList[i];
    CurPackage:=TLazPackage(APackageList.Objects[i]);
    LPKFilename:=CurPackage.Filename;
    PackageOk:=false;
    if CurPackage.LPKSource=nil then
      continue; // this package was not loaded/saved
    if CompareFilenames(LPKFilename,AltFilename)<>0 then
      continue; // lpk has vanished, an alternative lpk was found => show
    try
      CurPackage.SaveToString(CurSource);
      fs:=TFileStream.Create(LPKFilename,fmOpenRead);
      try
        if fs.Size=length(CurSource) then begin
          // size has not changed => load to see difference
          SetLength(DiskSource{%H-},fs.Size);
          fs.Read(DiskSource[1],length(DiskSource));
          if DiskSource=CurSource then
            PackageOk:=true;
        end;
      finally
        fs.Free;
      end;
    except
      // unable to load
      on E: Exception do
        DebugLn(['CheckPackagesWithLoading Filename=',CurPackage.Filename,' Error=',E.Message]);
    end;
    if PackageOk then
      AnIgnoreList.Add(CurPackage);
  end;
end;

function ShowDiskDiffsDialog(ACodeList: TFPList; APackageList: TStringList;
  AnIgnoreList: TFPList): TModalResult;

  function ListsAreEmpty: boolean;
  var
    i: Integer;
  begin
    if ACodeList<>nil then
      for i:=0 to ACodeList.Count-1 do
        if AnIgnoreList.IndexOf(ACodeList[i])<0 then
          exit(false);
    if APackageList<>nil then
      for i:=0 to APackageList.Count-1 do
        if AnIgnoreList.IndexOf(APackageList.Objects[i])<0 then
          exit(false);
    Result:=true;
  end;

begin
  if (DiskDiffsDlg<>nil) or ListsAreEmpty then
    exit(mrIgnore);
  if Assigned(ACodeList) then
    CheckUnits(ACodeList,AnIgnoreList);
  if Assigned(APackageList) then
    CheckPackages(APackageList,AnIgnoreList);
  if ListsAreEmpty then
    exit(mrIgnore);
  DiskDiffsDlg:=TDiskDiffsDlg.Create(nil);
  try
    DiskDiffsDlg.CodeList:=ACodeList;
    DiskDiffsDlg.PackageList:=APackageList;
    DiskDiffsDlg.IgnoreList:=AnIgnoreList;
    DiskDiffsDlg.FillFilesListBox;
    Result:=DiskDiffsDlg.ShowModal;
    case Result of
      mrOK : DiskDiffsDlg.ApplyChecks;
      mrCancel : Result:=mrIgnore;
    end;
  finally
    DiskDiffsDlg.Free;
    DiskDiffsDlg:=nil;
  end;
  Assert(Result in [mrOK,mrIgnore], 'ShowDiskDiffsDialog: Invalid result '+IntToStr(Result));
end;

{ TDiskDiffsDlg }

procedure TDiskDiffsDlg.FilesListBoxClick(Sender: TObject);
begin
  ShowDiff;
end;

procedure TDiskDiffsDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  EnvironmentOptions.CheckDiskChangesWithLoading:=CheckDiskChangesWithLoadingCheckBox.Checked;
end;

procedure TDiskDiffsDlg.WarnImagePaint(Sender: TObject);
var
  ppi: Integer;
  imgIndex: Integer;
  paintbx: TPaintBox;
begin
  paintbx := Sender as TPaintbox;
  ppi := Font.PixelsPerInch;
  imgIndex := IDEImages.GetImageIndex('state_warning');
  IDEImages.Images_16.DrawForPPI(paintbx.Canvas, 0, 0, imgIndex, 16, ppi, GetCanvasScaleFactor);
end;

function TDiskDiffsDlg.ShortenFilename(const aFilename: string): string;
begin
  Result:=Project1.RemoveProjectPathFromFilename(aFilename);
end;

procedure TDiskDiffsDlg.AddFile2Box(AInfo: TObject; AFileName: string; AModified: Boolean);
var
  i: Integer;
begin
  if AModified then
    AFileName:='*'+AFileName;
  i:=FilesListBox.Items.AddObject(AFileName,AInfo);
  if AModified then
    FHasLocalModifications:=True
  else
    FilesListBox.Checked[i]:=True;
end;

procedure TDiskDiffsDlg.FillFilesListBox;
var
  i: integer;
  CurUnit: TUnitInfo;
  APackage: TLazPackage;
  aCode: TCodeBuffer;
  aFilename, AltFilename: String;
  CurModified: Boolean;
begin
  FHasLocalModifications:=False;
  FHasExistingFiles:=False;
  FilesListBox.Items.BeginUpdate;
  FilesListBox.Items.Clear;
  if CodeList<>nil then
  begin
    for i:=0 to CodeList.Count-1 do begin
      aCode:=TCodeBuffer(CodeList[i]);
      if IgnoreList.IndexOf(aCode)>=0 then continue;
      aFilename:=aCode.Filename;
      CurUnit:=Project1.UnitInfoWithFilename(aFilename);
      if CurUnit=nil then
        CurUnit:=Project1.UnitInfoWithLFMFilename(aFilename);
      CurModified:=(CurUnit<>nil) and CurUnit.Modified;
      AddFile2Box(aCode, ShortenFilename(aFilename), CurModified);
      if (not FHasExistingFiles) and FileExistsCached(aFilename) then
        FHasExistingFiles:=true;
    end;
  end;
  if PackageList<>nil then
  begin
    for i:=0 to PackageList.Count-1 do begin
      APackage:=TLazPackage(PackageList.Objects[i]);
      if IgnoreList.IndexOf(APackage)>=0 then continue;
      AddFile2Box(APackage, APackage.Filename, APackage.Modified);
      AltFilename:=PackageList[i];
      if not FHasExistingFiles then
      begin
        if FileExistsCached(APackage.Filename)
            or ((CompareFilenames(AltFilename,APackage.Filename)<>0)
              and FileExistsCached(AltFilename)) then
          FHasExistingFiles:=true;
      end;
    end;
  end;
  FilesListBox.Items.EndUpdate;
  WarnImage.Visible:=FHasLocalModifications;
  WarnLabel.Visible:=FHasLocalModifications;
  BtnPanel.OkButton.Visible:=FHasExistingFiles;
end;

procedure TDiskDiffsDlg.ShowDiff;
var
  i: integer;
  DiffItem: PDiffItem;
  AInfo: TObject;
  aFilename: String;
  aCode: TCodeBuffer;
begin
  DiffItem:=nil;
  i:=FilesListBox.ItemIndex;
  if i>=0 then
  begin
    aFilename:=FilesListBox.Items[i];
    if aFilename[1]='*' then
      Delete(aFilename,1,1);
    AInfo:=FilesListBox.Items.Objects[i];
    if AInfo is TCodeBuffer then
    begin
      aCode:=TCodeBuffer(AInfo);
      DiffItem:=GetCachedDiff(aCode,'');
    end else if AInfo is TLazPackage then begin
      for i:=0 to PackageList.Count-1 do
        if PackageList.Objects[i]=AInfo then
        begin
          DiffItem:=GetCachedDiff(TLazPackage(AInfo),PackageList[i]);
          break;
        end;
    end;
  end;
  if DiffItem<>nil then begin
    DiffSynEdit.Lines.Text:=DiffItem^.Diff;
  end else begin
    DiffSynEdit.Lines.Clear;
  end;
end;

function TDiskDiffsDlg.GetCachedDiff(FileOwner: TObject; AltFilename: string
  ): PDiffItem;
var
  i: integer;
  fs: TFileStream;
  Filename: String;
  APackage: TLazPackage;
  Source: String;
  DiffOutput: TDiffOutput;
  Code: TCodeBuffer;
begin
  if FCachedDiffs=nil then
    FCachedDiffs:=TFPList.Create;
  for i:=0 to FCachedDiffs.Count-1 do begin
    Result:=PDiffItem(FCachedDiffs[i]);
    if (Result<>nil) and (Result^.Owner=FileOwner) then exit;
  end;
  New(Result);
  Result^.Owner:=FileOwner;
  try
    if FileOwner is TCodeBuffer then begin
      // compare disk and codetools
      Code:=TCodeBuffer(FileOwner);
      Filename:=Code.Filename;
      Source:=Code.Source;
    end else if FileOwner is TLazPackage then begin
      // compare disk and package
      APackage:=TLazPackage(FileOwner);
      if AltFilename<>'' then begin
        if CompareFilenames(AltFilename,APackage.Filename)<>0 then
          Result^.Diff+=Format(lisLpkHasVanishedOnDiskUsingAsAlternative,
                               [LineEnding+AltFilename+LineEnding]);
        Filename:=AltFilename;
      end
      else if APackage.LPKSource<>nil then
        Filename:=APackage.LPKSource.Filename
      else
        Filename:=APackage.GetFullFilename(true);
      APackage.SaveToString(Source);
    end else begin
      Filename:='';
      Source:='';
    end;
    fs:=TFileStream.Create(Filename,fmOpenRead);
    SetLength(Result^.TxtOnDisk,fs.Size);
    if Result^.TxtOnDisk<>'' then
      fs.Read(Result^.TxtOnDisk[1],length(Result^.TxtOnDisk));
    fs.Free;

    DiffOutput:=TDiffOutput.Create(Source,Result^.TxtOnDisk, []);
    try
      Result^.Diff+=DiffOutput.CreateTextDiff;
    finally
      DiffOutput.Free;
    end;
  except
    On E: Exception do
      Result^.Diff+='\ '+Format(lisDiskDiffErrorReadingFile, [E.Message]);
  end;
  FCachedDiffs.Add(Result);
end;

procedure TDiskDiffsDlg.ClearCache;
var
  i: integer;
  DiffItem: PDiffItem;
begin
  if FCachedDiffs=nil then exit;
  for i:=0 to FCachedDiffs.Count-1 do begin
    DiffItem:=PDiffItem(FCachedDiffs[i]);
    if DiffItem<>nil then begin
      DiffItem^.TxtOnDisk:='';
      DiffItem^.Diff:='';
      Dispose(DiffItem);
    end;
  end;
  FCachedDiffs.Clear;
end;

constructor TDiskDiffsDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Caption:=lisDiskDiffSomeFilesHaveChangedOnDisk;
  EditorOpts.GetSynEditSettings(DiffSynEdit);
  DiffSynEdit.Lines.Text:=lisDiskDiffClickOnOneOfTheAboveItemsToSeeTheDiff;

  BtnPanel.OkButton.Caption:=lisDiskDiffReloadCheckedFilesFromDisk;
  Assert(BtnPanel.OKButton.ModalResult=mrOK, 'OKButton.ModalResult<>mrOK');
  // Cancel button now means Ignore All Disk Changes
  BtnPanel.CancelButton.Caption:=lisDiskDiffIgnoreAllDiskChanges;

  WarnLabel.Caption:=lisDiskDiffSomeFilesHaveLocalChanges;
  WarnLabel.Visible:=False;
  WarnImage.Visible:=False;

  CheckDiskChangesWithLoadingCheckBox.Caption:=lisCheckForDiskFileChangesViaContent;
  CheckDiskChangesWithLoadingCheckBox.Checked:=EnvironmentOptions.CheckDiskChangesWithLoading;
end;

procedure TDiskDiffsDlg.ApplyChecks;
var
  i: Integer;
begin
  for i := 0 to FilesListBox.Count-1 do
    if not FilesListBox.Checked[i] then
      FIgnoreList.Add(FilesListBox.Items.Objects[i]);
end;

destructor TDiskDiffsDlg.Destroy;
begin
  ClearCache;
  FCachedDiffs.Free;
  inherited Destroy;
end;

end.

