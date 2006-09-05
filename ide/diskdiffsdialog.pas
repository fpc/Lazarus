{  $Id$  }
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
  Classes, SysUtils, Forms, Controls, Buttons, StdCtrls, LResources, Project,
  SynEdit, LCLType, DiffPatch, LazarusIDEStrConsts, ComCtrls, ExtCtrls,
  EnvironmentOpts;

type
  PDiffItem = ^TDiffItem;
  TDiffItem = record
    Valid: boolean;
    UnitInfo: TUnitInfo;
    Diff: string;
    TxtOnDisk: string;
  end;

  { TDiskDiffsDlg }

  TDiskDiffsDlg = class(TForm)
    CheckDiskChangesWithLoadingCheckBox: TCheckBox;
    DiffSynEdit: TSynEdit;
    FilesListBox: TListBox;
    RevertAllButton: TButton;
    IgnoreDiskChangesButton: TButton;
    Splitter: TSplitter;
    procedure DiskDiffsDlgKeyDown(Sender: TObject; var Key: Word;
          Shift: TShiftState);
    procedure FilesListBoxMouseUp(Sender: TOBject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FUnitList: TFPList;
    FCachedDiffs: TFPList; // List of PDiffItem
    procedure FillFilesListBox;
    procedure SetUnitList(const AValue: TFPList);
    procedure ShowDiff;
    function GetCachedDiff(AnUnitInfo: TUnitInfo): PDiffItem;
    procedure ClearCache;
  public
    property UnitList: TFPList read FUnitList write SetUnitList; // list of TUnitInfo
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
function ShowDiskDiffsDialog(AnUnitList: TFPList): TModalResult;

implementation

var
  DiskDiffsDlg: TDiskDiffsDlg = nil;

function ShowDiskDiffsDialog(AnUnitList: TFPList): TModalResult;

  procedure CheckWithLoading;
  var
    i: Integer;
    CurUnit: TUnitInfo;
    fs: TFileStream;
    UnitDidNotChange: Boolean;
    s: string;
  begin
    for i:=AnUnitList.Count-1 downto 0 do begin
      CurUnit:=TUnitInfo(AnUnitList[i]);
      UnitDidNotChange:=false;
      try
        fs:=TFileStream.Create(CurUnit.Filename,fmOpenRead);
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
  
begin
  if (DiskDiffsDlg<>nil) or (AnUnitList=nil) then begin
    Result:=mrIgnore;
    exit;
  end;
  if EnvironmentOptions.CheckDiskChangesWithLoading then begin
    CheckWithLoading;
    if AnUnitList.Count=0 then exit;
  end;

  DiskDiffsDlg:=TDiskDiffsDlg.Create(nil);
  DiskDiffsDlg.UnitList:=AnUnitList;
  Result:=DiskDiffsDlg.ShowModal;
  DiskDiffsDlg.Free;
  DiskDiffsDlg:=nil;
end;

{ TDiskDiffsDlg }

procedure TDiskDiffsDlg.DiskDiffsDlgKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_Escape then
    ModalResult:=mrCancel;
end;

procedure TDiskDiffsDlg.FilesListBoxMouseUp(Sender: TOBject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowDiff;
end;

procedure TDiskDiffsDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  EnvironmentOptions.CheckDiskChangesWithLoading:=
                                    CheckDiskChangesWithLoadingCheckBox.Checked;
end;

procedure TDiskDiffsDlg.FillFilesListBox;
var i: integer;
begin
  FilesListBox.Items.BeginUpdate;
  FilesListBox.Items.Clear;
  for i:=0 to UnitList.Count-1 do
    FilesListBox.Items.Add(TUnitInfo(UnitList[i]).ShortFilename);
  FilesListBox.Items.EndUpdate;
end;

procedure TDiskDiffsDlg.SetUnitList(const AValue: TFPList);
begin
  FUnitList:=AValue;
  FillFilesListBox;
end;

procedure TDiskDiffsDlg.ShowDiff;
var
  i: integer;
  DiffItem: PDiffItem;
begin
  i:=FilesListBox.ItemIndex;
  if i>=0 then begin
    DiffItem:=GetCachedDiff(TUnitInfo(FUnitList[i]));
    DiffSynEdit.Lines.Text:=DiffItem^.Diff;
  end else begin
    DiffSynEdit.Lines.Clear;
  end;
end;

function TDiskDiffsDlg.GetCachedDiff(AnUnitInfo: TUnitInfo): PDiffItem;
var
  i: integer;
  fs: TFileStream;
begin
  if FCachedDiffs=nil then
    FCachedDiffs:=TFPList.Create;
  for i:=0 to FCachedDiffs.Count-1 do begin
    Result:=PDiffItem(FCachedDiffs[i]);
    if (Result<>nil) and (Result^.UnitInfo=AnUnitInfo) then exit;
  end;
  New(Result);
  Result^.UnitInfo:=AnUnitInfo;
  try
    fs:=TFileStream.Create(AnUnitInfo.Filename,fmOpenRead);
    SetLength(Result^.TxtOnDisk,fs.Size);
    if Result^.TxtOnDisk<>'' then
      fs.Read(Result^.TxtOnDisk[1],length(Result^.TxtOnDisk));
    fs.Free;
    Result^.Diff:=CreateTextDiff(AnUnitInfo.Source.Source,Result^.TxtOnDisk,[],
                                 tdoContext);
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
  DiffSynEdit.Lines.Text:=lisDiskDiffClickOnOneOfTheAboveItemsToSeeTheDiff;
  RevertAllButton.Caption:=lisDiskDiffRevertAll;
  IgnoreDiskChangesButton.Caption:=lisDiskDiffIgnoreDiskChanges;
  CheckDiskChangesWithLoadingCheckBox.Caption:=lisCheckChangesOnDiskWithLoading;
  
  CheckDiskChangesWithLoadingCheckBox.Checked:=
                                 EnvironmentOptions.CheckDiskChangesWithLoading;
end;

destructor TDiskDiffsDlg.Destroy;
begin
  ClearCache;
  FCachedDiffs.Free;
  inherited Destroy;
end;

initialization
  {$I diskdiffsdialog.lrs}

end.

