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
  TDiskDiffsDlg = class(TForm)
    MainGroupBox: TGroupBox;
    FilesListBox: TListBox;
    DiffSynEdit: TSynEdit;
    RevertAllButton: TButton;
    IgnoreDiskChangesButton: TButton;
    procedure DiskDiffsDlgKeyDown(Sender: TObject; var Key: Word;
          Shift: TShiftState);
    procedure DiskDiffsDlgResize(Sender: TObject);
    procedure MainGroupBoxResize(Sender: TObject);
  private
    FUnitList: TList;
    procedure FillFilesListBox;
    procedure SetUnitList(const AValue: TList);
  public
    property UnitList: TList read FUnitList write SetUnitList; // list of TUnitInfo
    constructor Create(TheOwner: TComponent); override;
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
    end;
    
    DiffSynEdit:=TSynEdit.Create(Self);
    with DiffSynEdit do begin
      Name:='DiffSynEdit';
      Parent:=MainGroupBox;
      Left:=0;
      Top:=FilesListBox.Height+2;
      Width:=MainGroupBox.ClientWidth;
      Height:=MainGroupBox.ClientHeight-Top;
      Lines.Text:='The Diff View is not implemented yet.';
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

initialization
  DiskDiffsDlg:=nil;

end.

