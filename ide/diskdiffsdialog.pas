{
 /***************************************************************************
   diskdiffsdialog.pas
     -  form for showing the diffs of editor files changed on disk

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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

}
unit DiskDiffsDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Buttons, StdCtrls,
  Laz2_XMLWrite, lazutf8classes,
  SynEdit, SynHighlighterDiff, LCLType, ComCtrls, ExtCtrls,
  FileProcs, CodeToolManager, CodeCache,
  Project, DiffPatch, LazarusIDEStrConsts, EnvironmentOpts, EditorOptions,
  PackageDefs;

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
    BtnPanel: TPanel;
    CheckDiskChangesWithLoadingCheckBox: TCheckBox;
    DiffSynEdit: TSynEdit;
    FilesListBox: TListBox;
    RevertAllButton: TButton;
    IgnoreDiskChangesButton: TButton;
    Splitter: TSplitter;
    SynDiffSyn1: TSynDiffSyn;
    procedure DiskDiffsDlgKeyDown(Sender: TObject; var Key: Word;
          Shift: TShiftState);
    procedure FilesListBoxMouseUp(Sender: TOBject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FPackageList: TStringList;
    FUnitList: TFPList;
    FCachedDiffs: TFPList; // List of PDiffItem
    procedure FillFilesListBox;
    procedure ShowDiff;
    function GetCachedDiff(FileOwner: TObject; AltFilename: string): PDiffItem;
    procedure ClearCache;
  public
    property UnitList: TFPList read FUnitList write FUnitList; // list of TUnitInfo
    property PackageList: TStringList read FPackageList write FPackageList; // list of alternative filename and TLazPackage
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
function ShowDiskDiffsDialog(AnUnitList: TFPList; APackageList: TStringList): TModalResult;


implementation

{$R *.lfm}

var
  DiskDiffsDlg: TDiskDiffsDlg = nil;

function ShowDiskDiffsDialog(AnUnitList: TFPList; APackageList: TStringList): TModalResult;

  function ListsAreEmpty: boolean;
  begin
    Result:=((AnUnitList=nil) or (AnUnitList.Count=0))
        and ((APackageList=nil) or (APackageList.Count=0));
  end;

  procedure CheckUnitsWithLoading;
  var
    i: Integer;
    CurUnit: TUnitInfo;
    fs: TFileStreamUTF8;
    UnitDidNotChange: Boolean;
    s: string;
  begin
    if AnUnitList=nil then exit;
    for i:=AnUnitList.Count-1 downto 0 do begin
      CurUnit:=TUnitInfo(AnUnitList[i]);
      UnitDidNotChange:=false;
      try
        fs:=TFileStreamUTF8.Create(CurUnit.Filename,fmOpenRead);
        try
          if fs.Size=CurUnit.Source.SourceLength then begin
            // size has not changed => load to see difference
            SetLength(s,fs.Size);
            fs.Read(s[1],length(s));
            if s=CurUnit.Source.Source then
              UnitDidNotChange:=true;
          end;
        finally
          fs.Free;
        end;
      except
        // unable to load
      end;
      if UnitDidNotChange then begin
        if (CurUnit.Source<>nil) then CurUnit.Source.MakeFileDateValid;
        AnUnitList.Delete(i);
      end;
    end;
  end;
  
  procedure CheckPackagesWithLoading;
  var
    i: Integer;
    CurPackage: TLazPackage;
    PackageDidNotChange: Boolean;
    fs: TFileStreamUTF8;
    CurSource, DiskSource: string;
    AltFilename: String;
  begin
    if APackageList=nil then exit;
    for i:=APackageList.Count-1 downto 0 do begin
      AltFilename:=APackageList[i];
      CurPackage:=TLazPackage(APackageList.Objects[i]);
      PackageDidNotChange:=false;
      if CurPackage.LPKSource=nil then
        continue;// this package was not loaded/saved
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
              PackageDidNotChange:=true;
          end;
        finally
          fs.Free;
        end;
      except
        // unable to load
        on E: Exception do begin
          DebugLn(['CheckPackagesWithLoading Filename=',CurPackage.Filename,' Error=',E.Message]);
        end;
      end;
      if PackageDidNotChange then begin
        APackageList.Delete(i);
      end;
    end;
  end;
  
begin
  if (DiskDiffsDlg<>nil) or ListsAreEmpty then begin
    Result:=mrIgnore;
    exit;
  end;
  if EnvironmentOptions.CheckDiskChangesWithLoading then begin
    CheckUnitsWithLoading;
    CheckPackagesWithLoading;
    if ListsAreEmpty then exit(mrIgnore);
  end;
  DiskDiffsDlg:=TDiskDiffsDlg.Create(nil);
  DiskDiffsDlg.UnitList:=AnUnitList;
  DiskDiffsDlg.PackageList:=APackageList;
  DiskDiffsDlg.FillFilesListBox;
  Result:=DiskDiffsDlg.ShowModal;
  DiskDiffsDlg.Free;
  DiskDiffsDlg:=nil;
end;

{ TDiskDiffsDlg }

procedure TDiskDiffsDlg.DiskDiffsDlgKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Escape then
    ModalResult := mrIgnore
  else
  if Key = VK_Return then
    ModalResult := mrYesToAll;
end;

procedure TDiskDiffsDlg.FilesListBoxMouseUp(Sender: TOBject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowDiff;
end;

procedure TDiskDiffsDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  EnvironmentOptions.CheckDiskChangesWithLoading:=CheckDiskChangesWithLoadingCheckBox.Checked;
end;

procedure TDiskDiffsDlg.FillFilesListBox;
var i: integer;
  AnUnitInfo: TUnitInfo;
  APackage: TLazPackage;
begin
  FilesListBox.Items.BeginUpdate;
  FilesListBox.Items.Clear;
  if UnitList<>nil then
    for i:=0 to UnitList.Count-1 do begin
      AnUnitInfo:=TUnitInfo(UnitList[i]);
      FilesListBox.Items.AddObject(AnUnitInfo.ShortFilename,AnUnitInfo);
    end;
  if PackageList<>nil then
    for i:=0 to PackageList.Count-1 do begin
      APackage:=TLazPackage(PackageList.Objects[i]);
      FilesListBox.Items.AddObject(APackage.Filename,APackage);
    end;
  FilesListBox.Items.EndUpdate;
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
      if AltFilename<>'' then
        Filename:=AltFilename
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

    DiffOutput:=TDiffOutput.Create(Source,Result^.TxtOnDisk, [], nil);
    try
      Result^.Diff:=DiffOutput.CreateTextDiff;
    finally
      DiffOutput.Free;
    end;
  except
    On E: Exception do
      Result^.Diff:='\ '+Format(lisDiskDiffErrorReadingFile, [E.Message]);
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
  RevertAllButton.Caption:=lisDiskDiffRevertAll;
  IgnoreDiskChangesButton.Caption:=lisDiskDiffIgnoreDiskChanges;
  CheckDiskChangesWithLoadingCheckBox.Caption:=lisCheckForDiskFileChangesViaContentRatherThanTimesta;
  CheckDiskChangesWithLoadingCheckBox.Checked:=EnvironmentOptions.CheckDiskChangesWithLoading;
end;

destructor TDiskDiffsDlg.Destroy;
begin
  ClearCache;
  FCachedDiffs.Free;
  inherited Destroy;
end;

end.

