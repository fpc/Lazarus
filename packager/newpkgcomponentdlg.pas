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

  Author: Mattias Gaertner

  Abstract:
    Dialog to select the package where to create the new component.
}
unit NewPkgComponentDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, PackageSystem, PackageDefs, LazarusIDEStrConsts;

type

  { TNewPkgComponentDialog }

  TNewPkgComponentDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    PkgsGroupBox: TGroupBox;
    PkgsListBox: TListBox;
    procedure FormCreate(Sender: TObject);
  private
    procedure FillPkgsListBox;
  public
    function GetPackageName: string;
  end;

function ShowNewPkgComponentDialog(out aPackage: TLazPackage): TModalResult;

implementation

{$R *.lfm}

function ShowNewPkgComponentDialog(out aPackage: TLazPackage): TModalResult;
var
  NewPkgComponentDialog: TNewPkgComponentDialog;
  PkgName: String;
begin
  aPackage:=nil;
  NewPkgComponentDialog:=TNewPkgComponentDialog.Create(nil);
  try
    Result:=NewPkgComponentDialog.ShowModal;
    if Result<>mrOk then exit;
    PkgName:=NewPkgComponentDialog.GetPackageName;
    if PkgName<>'' then
      aPackage:=PackageGraph.FindPackageWithName(PkgName,nil);
  finally
    NewPkgComponentDialog.Free;
  end;
end;

{ TNewPkgComponentDialog }

procedure TNewPkgComponentDialog.FormCreate(Sender: TObject);
begin
  Caption:=lisCreateNewPackageComponent;

  PkgsGroupBox.Caption:=lisPkgSelectAPackage;
  FillPkgsListBox;
end;

procedure TNewPkgComponentDialog.FillPkgsListBox;
var
  sl: TStringList;
  Pkg: TLazPackage;
  i: Integer;
begin
  sl:=TStringList.Create;
  try
    for i:=0 to PackageGraph.Count-1 do begin
      Pkg:=PackageGraph[i];
      if (not (Pkg.PackageType in [lptRunAndDesignTime,lptDesignTime]))
      or Pkg.ReadOnly or Pkg.AutoCreated
      then continue;
      sl.Add(Pkg.Name);
    end;
    sl.Sort;
    // add as first item '(create new)'
    sl.Insert(0,lisCreateNewPackage);
    PkgsListBox.Items.Assign(sl);
    PkgsListBox.ItemIndex:=0;
  finally
    sl.Free;
  end;
end;

function TNewPkgComponentDialog.GetPackageName: string;
var
  i: Integer;
begin
  i:=PkgsListBox.ItemIndex;
  if i<1 then
    Result:=''
  else
    Result:=PkgsListBox.Items[i];
end;

end.

