{  $Id$  }
{
 /***************************************************************************
                            openinstalledpkgdlg.pas
                            -----------------------


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

  Author: Mattias Gaertner

  Abstract:
    Defines TOpenInstalledPackagesDlg - The dialog let the user choose one of
    the installed packages.
}
unit OpenInstalledPkgDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons, ComCtrls, StdCtrls,
  FileCtrl, Dialogs, LCLProc, IDEContextHelpEdit,
  PackageDefs, LazarusIDEStrConsts, IDEWindowIntf, PackageSystem, ExtCtrls;

type

  { TOpenInstalledPackagesDlg }

  TOpenInstalledPackagesDlg = class(TForm)
    HelpButton: TBitBtn;
    CancelButton: TBitBtn;
    OpenButton: TBitBtn;
    PkgListView: TListView;
    HintMemo: TMemo;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure PkgListViewDblClick(Sender: TObject);
    procedure PkgListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    function PkgStateToString(APackage: TLazPackage): string;
  public
    procedure UpdateSelection;
    procedure UpdatePackageList;
    function GetSelectedPackage: TLazPackage;
  end;
  
function ShowOpenInstalledPkgDlg(var OpenPackage: TLazPackage): TModalResult;

implementation

{$R *.lfm}

function ShowOpenInstalledPkgDlg(var OpenPackage: TLazPackage): TModalResult;
var
  OpenInstalledPackagesDlg: TOpenInstalledPackagesDlg;
begin
  OpenInstalledPackagesDlg:=TOpenInstalledPackagesDlg.Create(nil);
  OpenInstalledPackagesDlg.UpdatePackageList;
  OpenInstalledPackagesDlg.UpdateSelection;
  Result:=OpenInstalledPackagesDlg.ShowModal;
  OpenPackage:=OpenInstalledPackagesDlg.GetSelectedPackage;
  OpenInstalledPackagesDlg.Free;
end;

{ TOpenInstalledPackagesDlg }

procedure TOpenInstalledPackagesDlg.PkgListViewDblClick(Sender: TObject);
begin
  OpenButtonClick(Sender);
end;

procedure TOpenInstalledPackagesDlg.PkgListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateSelection;
end;

procedure TOpenInstalledPackagesDlg.UpdateSelection;
var
  CurPkg: TLazPackage;
  LI: TListItem;
  HintStr: String;
begin
  LI:=PkgListView.Selected;
  if LI<>nil then begin
    CurPkg:=TLazPackage(LI.Data);
    HintStr:=
       Format(lisOIPFilename, [CurPkg.Filename]);
    if CurPkg.AutoCreated then
      HintStr:=Format(lisOIPThisPackageWasAutomaticallyCreated, [HintStr+
        LineEnding]);
    if CurPkg.Missing then
      HintStr:=Format(lisOIPThisPackageIsInstalledButTheLpkFileWasNotFound, [
        HintStr+LineEnding]);
    HintStr:=Format(lisOIPDescriptionDescription, [HintStr+LineEnding,
      BreakString(CurPkg.Description, 60, length(lisOIPDescription))]);
    HintMemo.Text:=HintStr;
  end else begin
    HintMemo.Text:=lisOIPPleaseSelectAPackage;
  end;
end;

procedure TOpenInstalledPackagesDlg.OpenButtonClick(Sender: TObject);
begin
  if PkgListView.Selected=nil then begin
    MessageDlg(lisOIPNoPackageSelected,
      lisOIPPleaseSelectAPackageToOpen,
      mtInformation,[mbCancel],0);
    exit;
  end;
  ModalResult:=mrOk;
end;

procedure TOpenInstalledPackagesDlg.FormCreate(Sender: TObject);
var
  NewColumn: TListColumn;
begin
  Caption:=lisOIPOpenLoadedPackage;
  IDEDialogLayoutList.ApplyLayout(Self,500,350);

  with PkgListView do begin
    ViewStyle:=vsReport;
    NewColumn:=Columns.Add;
    NewColumn.Caption:=lisOIPPackageName;
    NewColumn.Width:=150;
    NewColumn:=Columns.Add;
    NewColumn.Caption:=lisVersion;
    NewColumn.Width:=80;
    NewColumn:=Columns.Add;
    NewColumn.Caption:=lisOIPState;
    NewColumn.Width:=300;
  end;

  OpenButton.Caption:=lisMenuOpen;
  CancelButton.Caption:=dlgCancel;
  HelpButton.Caption:=dlgGroupHelp;
end;

procedure TOpenInstalledPackagesDlg.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
end;

function TOpenInstalledPackagesDlg.PkgStateToString(APackage: TLazPackage
  ): string;
  
  procedure AddState(const s: string);
  begin
    if Result='' then
      Result:=Result+s
    else
      Result:=Result+', '+s;
  end;
  
begin
  Result:='';
  if APackage.Modified then AddState(lisOIPmodified);
  if APackage.Missing then AddState(lisOIPmissing);
  case APackage.Installed of
  pitStatic: AddState(lisOIPinstalledStatic);
  pitDynamic: AddState(lisOIPinstalledDynamic);
  end;
  case APackage.AutoInstall of
  pitStatic: AddState(lisOIPautoInstallStatic);
  pitDynamic: AddState(lisOIPautoInstallDynamic);
  end;
  if APackage.ReadOnly then AddState(lisOIPreadonly);
end;

procedure TOpenInstalledPackagesDlg.UpdatePackageList;
var
  Cnt: Integer;
  i: Integer;
  CurPkg: TLazPackage;
  CurListItem: TListItem;
begin
  PkgListView.BeginUpdate;
  Cnt:=PackageGraph.Count;
  for i:=0 to Cnt-1 do begin
    CurPkg:=PackageGraph[i];
    //if not (CurPkg.Installed in [pitStatic,pitDynamic]) then continue;
    if PkgListView.Items.Count>i then begin
      CurListItem:=PkgListView.Items[i];
      CurListItem.SubItems[0]:=CurPkg.Version.AsString;
      CurListItem.SubItems[1]:=PkgStateToString(CurPkg);
    end else begin
      CurListItem:=PkgListView.Items.Add;
      CurListItem.SubItems.Add(CurPkg.Version.AsString);
      CurListItem.SubItems.Add(PkgStateToString(CurPkg));
    end;
    CurListItem.Caption:=CurPkg.Name;
    CurListItem.Data:=CurPkg;
  end;
  PkgListView.EndUpdate;
end;

function TOpenInstalledPackagesDlg.GetSelectedPackage: TLazPackage;
var
  LI: TListItem;
begin
  Result:=nil;
  LI:=PkgListView.Selected;
  if LI=nil then exit;
  Result:=TLazPackage(LI.Data);
end;

end.

