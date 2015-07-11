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
unit MissingPkgFilesDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComCtrls, FileProcs,
  PackageDefs, LazarusIDEStrConsts;

type

  { TMissingPkgFilesDialog }

  TMissingPkgFilesDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    FilesTreeView: TTreeView;
    procedure ButtonPanel1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FLazPackage: TLazPackage;
    procedure SetLazPackage(const AValue: TLazPackage);
    procedure UpdateFilesList;
  public
    property LazPackage: TLazPackage read FLazPackage write SetLazPackage;
  end; 

function ShowMissingPkgFilesDialog(APackage: TLazPackage): TModalResult;

implementation

function ShowMissingPkgFilesDialog(APackage: TLazPackage): TModalResult;
var
  Dlg: TMissingPkgFilesDialog;
begin
  Dlg:=TMissingPkgFilesDialog.Create(nil);
  try
    Dlg.LazPackage:=APackage;
    Result:=Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{$R *.lfm}

{ TMissingPkgFilesDialog }

procedure TMissingPkgFilesDialog.FormCreate(Sender: TObject);
begin
  ButtonPanel1.CancelButton.Caption:=lisCancel;
end;

procedure TMissingPkgFilesDialog.ButtonPanel1Click(Sender: TObject);
begin
  if LazPackage<>nil then
    LazPackage.RemoveNonExistingFiles;
end;

procedure TMissingPkgFilesDialog.SetLazPackage(const AValue: TLazPackage);
begin
  if FLazPackage=AValue then exit;
  FLazPackage:=AValue;
  if LazPackage<>nil then
    Caption:=Format(lisPEMissingFilesOfPackage, [LazPackage.IDAsString]);
  UpdateFilesList;
end;

procedure TMissingPkgFilesDialog.UpdateFilesList;
var
  i: Integer;
  j: Integer;
  PkgFile: TPkgFile;
  RealFilename: String;
  s: String;
begin
  FilesTreeView.BeginUpdate;
  i:=0;
  if LazPackage<>nil then begin
    for j:=0 to LazPackage.FileCount-1 do begin
      PkgFile:=LazPackage.Files[j];
      RealFilename:=PkgFile.GetResolvedFilename;
      if (RealFilename<>'') and FileExistsCached(RealFilename) then continue;
      s:=PkgFile.Filename;
      if FilesTreeView.Items.TopLvlCount>i then
        FilesTreeView.Items.TopLvlItems[i].Text:=s
      else
        FilesTreeView.Items.Add(nil,s);
      inc(i);
    end;
  end;
  if i=0 then begin
    s:=lisPENoFilesMissingAllFilesExist;
    if FilesTreeView.Items.TopLvlCount>i then
      FilesTreeView.Items.TopLvlItems[i].Text:=s
    else
      FilesTreeView.Items.Add(nil,s);
  end;
  while FilesTreeView.Items.TopLvlCount>i do
    FilesTreeView.Items.TopLvlItems[FilesTreeView.Items.TopLvlCount-1].Free;
  FilesTreeView.EndUpdate;
  if i>0 then begin
    ButtonPanel1.OKButton.Caption:=lisPERemoveFiles;
  end else begin
    ButtonPanel1.OKButton.Caption:=lisMenuOk;
  end;
end;

end.

