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
  Classes, SysUtils, IDEProcs, Forms, Controls, Buttons, ExtCtrls, StdCtrls,
  LResources, Project, SynEdit, LCLType, DiffPatch;

type
  PDiffItem = ^TDiffItem;
  TDiffItem = record
    Valid: boolean;
    UnitInfo: TUnitInfo;
    Diff: string;
    TxtOnDisk: string;
  end;

  TDiskDiffsDlg = class(TForm)
    MainGroupBox: TGroupBox;
    FilesListBox: TListBox;
    DiffSynEdit: TSynEdit;
    RevertAllButton: TButton;
    IgnoreDiskChangesButton: TButton;
    procedure DiskDiffsDlgKeyDown(Sender: TObject; var Key: Word;
          Shift: TShiftState);
    procedure DiskDiffsDlgResize(Sender: TObject);
    procedure FilesListBoxMouseUp(Sender: TOBject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
    procedure MainGroupBoxResize(Sender: TObject);
  private
    FUnitList: TList;
    FCachedDiffs: TList; // List of PDiffItem
    procedure FillFilesListBox;
    procedure SetUnitList(const AValue: TList);
    procedure ShowDiff;
    function GetCachedDiff(AnUnitInfo: TUnitInfo): PDiffItem;
    procedure ClearCache;
  public
    property UnitList: TList read FUnitList write SetUnitList; // list of TUnitInfo
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;
  

var DiskDiffsDlg: TDiskDiffsDlg;


function ShowDiskDiffsDialog(AnUnitList: TList): TModalResult;


implementation


function ShowDiskDiffsDialog(AnUnitList: TList): TModalResult;
begin
  if DiskDiffsDlg<>nil then begin
    Result:=mrIgnore;
    exit;
  end;
  DiskDiffsDlg:=TDiskDiffsDlg.Create(Application);
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

procedure TDiskDiffsDlg.DiskDiffsDlgResize(Sender: TObject);
begin
  with MainGroupBox do begin
    Width:=Self.ClientWidth-2*Left;
    Height:=Self.ClientHeight-50-Top;
  end;
  
  with RevertAllButton do begin
    Top:=Self.ClientHeight-40;
  end;

  with IgnoreDiskChangesButton do begin
    Left:=RevertAllButton.Left+RevertAllButton.Width+10;
    Top:=RevertAllButton.Top;
  end;
end;

procedure TDiskDiffsDlg.FilesListBoxMouseUp(Sender: TOBject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowDiff;
end;

procedure TDiskDiffsDlg.MainGroupBoxResize(Sender: TObject);
begin
  with DiffSynEdit do begin
    Width:=MainGroupBox.ClientWidth;
    Height:=MainGroupBox.ClientHeight-Top;
  end;
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

procedure TDiskDiffsDlg.SetUnitList(const AValue: TList);
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
    FCachedDiffs:=TList.Create;
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
    Result^.Diff:=CreateTextDiff(AnUnitInfo.Source.Source,Result^.TxtOnDisk,[]);
  except
    On E: Exception do
      Result^.Diff:='\ Error reading file: '+E.Message;
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
  if LazarusResources.Find(ClassName)=nil then begin
    Caption:='Some files have changed on disk:';
    Position:=poScreenCenter;
    Width:=600;
    Height:=300;
  
    MainGroupBox:=TGroupBox.Create(Self);
    with MainGroupBox do begin
      Name:='MainGroupBox';
      Parent:=Self;
      Left:=5;
      Top:=5;
      Width:=Self.ClientWidth-2*Left;
      Height:=Self.ClientHeight-50-Top;
      Caption:='Changed files:';
      Visible:=true;
      OnResize:=@MainGroupBoxResize;
    end;
    
    FilesListBox:=TListBox.Create(Self);
    with FilesListBox do begin
      Name:='FilesListBox';
      Parent:=MainGroupBox;
      Left:=0;
      Top:=0;
      Height:=60;
      Align:=alTop;
      Visible:=true;
      OnMouseUp:=@FilesListBoxMouseUp;
    end;
    
    DiffSynEdit:=TSynEdit.Create(Self);
    with DiffSynEdit do begin
      Name:='DiffSynEdit';
      Parent:=MainGroupBox;
      Left:=0;
      Top:=FilesListBox.Height+2;
      Width:=MainGroupBox.ClientWidth;
      Height:=MainGroupBox.ClientHeight-Top;
      ReadOnly:=true;
      Gutter.Visible:=false;
      Lines.Text:='Click on one of the above items to see the diff';
      Visible:=true;
    end;
    
    RevertAllButton:=TButton.Create(Self);
    with RevertAllButton do begin
      Name:='RevertAllButton';
      Parent:=Self;
      Left:=50;
      Top:=Self.ClientHeight-40;
      Width:=150;
      Caption:='Revert All';
      ModalResult:=mrYesToAll;
      Default:=true;
      Visible:=true;
    end;
    
    IgnoreDiskChangesButton:=TButton.Create(Self);
    with IgnoreDiskChangesButton do begin
      Name:='IgnoreDiskChangesButton';
      Parent:=Self;
      Left:=RevertAllButton.Left+RevertAllButton.Width+10;
      Top:=RevertAllButton.Top;
      Width:=150;
      Caption:='Ignore disk changes';
      ModalResult:=mrIgnore;
      Visible:=true;
    end;

    OnResize:=@DiskDiffsDlgResize;
    OnKeyDown:=@DiskDiffsDlgKeyDown;
  end;
end;

destructor TDiskDiffsDlg.Destroy;
begin
  ClearCache;
  FCachedDiffs.Free;
  inherited Destroy;
end;

initialization
  DiskDiffsDlg:=nil;

end.

