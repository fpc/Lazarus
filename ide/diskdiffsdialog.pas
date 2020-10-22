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
  LazUTF8Classes, LazFileUtils, UITypes,
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
    WarnLabel: TLabel;
    WarnSpeedButton: TSpeedButton;
    Splitter: TSplitter;
    SynDiffSyn1: TSynDiffSyn;
    procedure FilesListBoxClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
  private
    FIgnoreList: TFPList;
    FPackageList: TStringList;
    FUnitList: TFPList;
    FHasLocalModifications: Boolean;
    FCachedDiffs: TFPList; // List of PDiffItem
    procedure AddFile2Box(AInfo: TObject; AFileName: string; AModified: Boolean);
    procedure FillFilesListBox;
    procedure ApplyChecks;
    procedure ShowDiff;
    function GetCachedDiff(FileOwner: TObject; AltFilename: string): PDiffItem;
    procedure ClearCache;
  public
    property UnitList: TFPList read FUnitList write FUnitList; // list of TUnitInfo
    property PackageList: TStringList read FPackageList write FPackageList; // list of alternative filename and TLazPackage
    property IgnoreList: TFPList read FIgnoreList write FIgnoreList;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

function ShowDiskDiffsDialog(AnUnitList: TFPList;
  APackageList: TStringList; AnIgnoreList: TFPList): TModalResult;

implementation

{$R *.lfm}

var
  DiskDiffsDlg: TDiskDiffsDlg = nil;

// Procedures used by ShowDiskDiffsDialog

procedure CheckUnitsWithLoading(AnUnitList: TFPList);
var
  i: Integer;
  CurUnit: TUnitInfo;
  CodeOk: Boolean;
  MemCode: TCodeBuffer;
  s, DiskEncoding, MemEncoding: String;
  fs: TFileStreamUTF8;
begin
  for i:=AnUnitList.Count-1 downto 0 do
  begin
    CurUnit:=TUnitInfo(AnUnitList[i]);
    MemCode:=CurUnit.Source;
    CodeOk:=false;
    try
      fs := TFileStreamUTF8.Create(MemCode.Filename, fmOpenRead or fmShareDenyNone);
      try
        SetLength(s, fs.Size);
        if s <> '' then
          fs.Read(s[1], length(s));
        DiskEncoding := '';
        MemEncoding := '';
        MemCode.CodeCache.OnDecodeLoaded(MemCode,MemCode.Filename,
          s,DiskEncoding,MemEncoding);
        //debugln(['CheckUnitsWithLoading ',MemCode.Filename,
        //  ' ',length(s),'=',MemCode.SourceLength]);
        if (MemEncoding=MemCode.MemEncoding)
        and (DiskEncoding=MemCode.DiskEncoding)
        and (length(s)=MemCode.SourceLength)
        and (s=MemCode.Source) then begin
          CodeOk:=true;
        end;
      finally
        fs.Free;
      end;
    except
      // unable to load
    end;
    if CodeOk then begin
      if CurUnit.Source<>nil then
        CurUnit.Source.MakeFileDateValid;
      AnUnitList.Delete(i);
    end;
  end;
end;

procedure CheckPackagesWithLoading(APackageList: TStringList);
var
  i: Integer;
  CurPackage: TLazPackage;
  PackageOk: Boolean;
  fs: TFileStreamUTF8;
  CurSource, DiskSource: string;
  AltFilename: String;
begin
  for i:=APackageList.Count-1 downto 0 do
  begin
    AltFilename:=APackageList[i];
    CurPackage:=TLazPackage(APackageList.Objects[i]);
    PackageOk:=false;
    if CurPackage.LPKSource=nil then
      continue; // this package was not loaded/saved
    if CompareFilenames(CurPackage.Filename,AltFilename)<>0 then
      continue; // lpk has vanished, an alternative lpk was found => show
    try
      CurPackage.SaveToString(CurSource);
      fs:=TFileStreamUTF8.Create(CurPackage.Filename,fmOpenRead);
      try
        if fs.Size=length(CurSource) then begin
          // size has not changed => load to see difference
          SetLength(DiskSource,fs.Size);
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
      APackageList.Delete(i);
  end;
end;

function ShowDiskDiffsDialog(AnUnitList: TFPList; APackageList: TStringList;
  AnIgnoreList: TFPList): TModalResult;

  function ListsAreEmpty: boolean;
  begin
    Result:=((AnUnitList=nil) or (AnUnitList.Count=0))
        and ((APackageList=nil) or (APackageList.Count=0));
  end;

begin
  if (DiskDiffsDlg<>nil) or ListsAreEmpty then
    exit(mrIgnore);
  if EnvironmentOptions.CheckDiskChangesWithLoading then begin
    if Assigned(AnUnitList) then
      CheckUnitsWithLoading(AnUnitList);
    if Assigned(APackageList) then
      CheckPackagesWithLoading(APackageList);
    if ListsAreEmpty then exit(mrIgnore);
  end;
  DiskDiffsDlg:=TDiskDiffsDlg.Create(nil);
  DiskDiffsDlg.UnitList:=AnUnitList;
  DiskDiffsDlg.PackageList:=APackageList;
  DiskDiffsDlg.IgnoreList:=AnIgnoreList;
  DiskDiffsDlg.FillFilesListBox;
  Result:=DiskDiffsDlg.ShowModal;
  case Result of
    mrOK : DiskDiffsDlg.ApplyChecks;
    mrCancel : Result:=mrIgnore;
  end;
  DiskDiffsDlg.Free;
  DiskDiffsDlg:=nil;
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
  UInfo: TUnitInfo;
  APackage: TLazPackage;
begin
  FHasLocalModifications:=False;
  FilesListBox.Items.BeginUpdate;
  FilesListBox.Items.Clear;
  if UnitList<>nil then
  begin
    for i:=0 to UnitList.Count-1 do begin
      UInfo:=TUnitInfo(UnitList[i]);
      AddFile2Box(UInfo, UInfo.ShortFilename, UInfo.Modified);
    end;
  end;
  if PackageList<>nil then
  begin
    for i:=0 to PackageList.Count-1 do begin
      APackage:=TLazPackage(PackageList.Objects[i]);
      AddFile2Box(APackage, APackage.Filename, APackage.Modified);
    end;
  end;
  FilesListBox.Items.EndUpdate;
  WarnSpeedButton.Visible:=FHasLocalModifications;
  WarnLabel.Visible:=FHasLocalModifications;
end;

procedure TDiskDiffsDlg.ShowDiff;
var
  i: integer;
  DiffItem: PDiffItem;
begin
  i:=FilesListBox.ItemIndex;
  DiffItem:=nil;
  if (i>=0) and (UnitList<>nil) then begin
    if i<UnitList.Count then
      DiffItem:=GetCachedDiff(TUnitInfo(UnitList[i]),'');
    dec(i,UnitList.Count);
  end;
  if (i>=0) and (PackageList<>nil) then begin
    if i<PackageList.Count then
      DiffItem:=GetCachedDiff(TLazPackage(PackageList.Objects[i]),PackageList[i]);
    dec(i,PackageList.Count);
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
  fs: TFileStreamUTF8;
  Filename: String;
  AnUnitInfo: TUnitInfo;
  APackage: TLazPackage;
  Source: String;
  DiffOutput: TDiffOutput;
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
    if FileOwner is TUnitInfo then begin
      // compare disk and codetools
      AnUnitInfo:=TUnitInfo(FileOwner);
      Filename:=AnUnitInfo.Source.Filename;
      Source:=AnUnitInfo.Source.Source;
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
    fs:=TFileStreamUTF8.Create(Filename,fmOpenRead);
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

  IDEImages.AssignImage(WarnSpeedButton, 'state_warning');
  WarnLabel.Caption:=lisDiskDiffSomeFilesHaveLocalChanges;
  WarnLabel.Visible:=False;
  WarnSpeedButton.Visible:=False;

  CheckDiskChangesWithLoadingCheckBox.Caption:=lisCheckForDiskFileChangesViaContent;
  CheckDiskChangesWithLoadingCheckBox.Checked:=EnvironmentOptions.CheckDiskChangesWithLoading;
end;

procedure TDiskDiffsDlg.ApplyChecks;
var
  i: Integer;
begin
  FIgnoreList.Clear;
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

