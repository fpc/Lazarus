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
}
unit ConfirmPkgListDlg;

{$mode objfpc}{$H+}

interface

uses
  Buttons, Classes, ExtCtrls, Grids, Forms, Controls;

type

  { TConfirmPackageListDialog }

  TConfirmPackageListDialog = class(TForm)
    BtnPanel: TPanel;
    CancelButton: TBitBtn;
    OkButton: TBitBtn;
    PackagesGrid: TStringGrid;
  private
    function Confirm(AChangesReport: TStrings): Boolean;
  end; 

function ConfirmPackageList(AChangesReport: TStrings): Boolean;

implementation

{$R *.lfm}

uses
  LazarusIDEStrConsts, Math, StrUtils;

function ConfirmPackageList(AChangesReport: TStrings): Boolean;
var
  dlg: TConfirmPackageListDialog;
begin
  dlg := TConfirmPackageListDialog.Create(nil);
  try
    Result := dlg.Confirm(AChangesReport);
  finally
    dlg.Free;
  end;
end;

{ TConfirmPackageListDialog }

function TConfirmPackageListDialog.Confirm(AChangesReport: TStrings): Boolean;
var
  s: String;
  i, j, p, np, d: Integer;
begin
  with PackagesGrid do begin
    RowCount := AChangesReport.Count + 1;
    Cells[0, 0] := lisConfirmPackageNewPackageSet;
    Cells[1, 0] := lisConfirmPackageAction;
    Cells[2, 0] := lisConfirmPackageOldPackageSet;
    d := RowCount * DefaultRowHeight + GridLineWidth - Height;
  end;
  // Auto-grow dialog up to 3/4 of the screen height.
  d := Min(d, Screen.Height * 3 div 4 - Height);
  Height := Height + d;
  Caption := lisConfirmNewPackageSetForTheIDE;
  OkButton.Caption := lisContinue;
  CancelButton.Caption := lisCancel;

  for i := 1 to AChangesReport.Count do begin
    s := AChangesReport[i - 1];
    p := 1;
    for j := 0 to PackagesGrid.ColCount - 1 do begin
      np := PosEx('|', s, p);
      if np = 0 then np := Length(s) + 1;
      PackagesGrid.Cells[j, i] := Copy(s, p, np - p);
      if np > Length(s) then break;
      p := np + 1;
    end;
  end;
  PackagesGrid.AutoSizeColumns;
  Result := ShowModal = mrOK;
end;

end.

