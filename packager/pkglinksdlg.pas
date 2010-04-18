{
 /***************************************************************************
                            pkgmanager.pas
                            --------------


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
    Dialog showing the package links of the IDE package systems.
}
unit PkgLinksDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, Grids, AVL_Tree,
  FileProcs, PackageIntf,
  LazarusIDEStrConsts, PackageDefs, PackageLinks;

type

  { TPkgLinkInfo }

  TPkgLinkInfo = class(TPackageLink)
  public
    procedure Assign(Source: TLazPackageID);
    property Origin;
  end;

  { TPackageLinksDialog }

  TPackageLinksDialog = class(TForm)
    CloseBitBtn: TBitBtn;
    ShowUserLinksCheckBox: TCheckBox;
    ShowGlobalLinksCheckBox: TCheckBox;
    FileMustExistCheckBox: TCheckBox;
    ScopeGroupBox: TGroupBox;
    PkgStringGrid: TStringGrid;
    procedure FileMustExistCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShowGlobalLinksCheckBoxChange(Sender: TObject);
    procedure ShowUserLinksCheckBoxChange(Sender: TObject);
  private
    FLinks: TAVLTree;// tree of TPkgLinkInfo sorted for names
    FCollectingOrigin: TPkgLinkOrigin;
    procedure UpdateAll;
    procedure UpdatePackageList;
    procedure ClearLinks;
    procedure IteratePackages(APackage: TLazPackageID);
  public
    destructor Destroy; override;
  end; 

function ShowPackageLinks: TModalResult;

implementation

{$R *.lfm}

function ShowPackageLinks: TModalResult;
var
  PackageLinksDialog: TPackageLinksDialog;
begin
  PackageLinksDialog:=TPackageLinksDialog.Create(nil);
  try
    Result:=PackageLinksDialog.ShowModal;
  finally
    PackageLinksDialog.Free;
  end;
end;

{ TPackageLinksDialog }

procedure TPackageLinksDialog.FormCreate(Sender: TObject);
begin
  Caption:=lisPLDPackageLinks;
  CloseBitBtn.Caption:=lisMenuClose;
  ScopeGroupBox.Caption:=dlgScope;
  FileMustExistCheckBox.Caption:=lisPLDOnlyExistingFiles;
  ShowGlobalLinksCheckBox.Caption:=lisPLDShowGlobalLinks
                                 +' ('+PkgLinks.GetGlobalLinkDirectory+'*.lpl)';
  ShowUserLinksCheckBox.Caption:=lisPLDShowUserLinks
                                      +' ('+PkgLinks.GetUserLinkFile+')';
  UpdateAll;
end;

procedure TPackageLinksDialog.ShowGlobalLinksCheckBoxChange(Sender: TObject);
begin
  UpdatePackageList;
end;

procedure TPackageLinksDialog.ShowUserLinksCheckBoxChange(Sender: TObject);
begin
  UpdatePackageList;
end;

procedure TPackageLinksDialog.FileMustExistCheckBoxChange(Sender: TObject);
begin
  UpdatePackageList;
end;

procedure TPackageLinksDialog.UpdateAll;
begin
  UpdatePackageList;
end;

procedure TPackageLinksDialog.UpdatePackageList;
var
  Node: TAVLTreeNode;
  Link: TPkgLinkInfo;
  i: Integer;
  OriginStr: String;
begin
  ClearLinks;
  
  FLinks:=TAVLTree.Create(@ComparePackageLinks);
  if ShowGlobalLinksCheckBox.Checked then begin
    FCollectingOrigin:=ploGlobal;
    PkgLinks.IteratePackages(FileMustExistCheckBox.Checked,@IteratePackages,[ploGlobal]);
  end;
  if ShowUserLinksCheckBox.Checked then begin
    FCollectingOrigin:=ploUser;
    PkgLinks.IteratePackages(FileMustExistCheckBox.Checked,@IteratePackages,[ploUser]);
  end;

  PkgStringGrid.ColCount:=5;
  PkgStringGrid.RowCount:=FLinks.Count+1;
  PkgStringGrid.Cells[0, 0]:=lisDebugOptionsFrmName;
  PkgStringGrid.Cells[1, 0]:=lisVersion;
  PkgStringGrid.Cells[2, 0]:=dlgPLDPackageGroup;
  PkgStringGrid.Cells[3, 0]:=lisPLDExists;
  PkgStringGrid.Cells[4, 0]:=lisA2PFilename2;

  i:=1;
  Node:=FLinks.FindLowest;
  while Node<>nil do begin
    Link:=TPkgLinkInfo(Node.Data);
    PkgStringGrid.Cells[0,i]:=Link.Name;
    PkgStringGrid.Cells[1,i]:=Link.Version.AsString;
    if Link.Origin=ploGlobal then
      OriginStr:=lisPLDGlobal
    else
      OriginStr:=lisPLDUser;
    PkgStringGrid.Cells[2,i]:=OriginStr;
    PkgStringGrid.Cells[3,i]:=dbgs(FileExistsCached(Link.GetEffectiveFilename));
    PkgStringGrid.Cells[4,i]:=Link.GetEffectiveFilename;
    inc(i);
    Node:=FLinks.FindSuccessor(Node);
  end;
  
  PkgStringGrid.AutoAdjustColumns;
end;

procedure TPackageLinksDialog.ClearLinks;
begin
  if FLinks<>nil then begin
    FLinks.FreeAndClear;
    FreeAndNil(FLinks);
  end;
end;

procedure TPackageLinksDialog.IteratePackages(APackage: TLazPackageID);
var
  NewLink: TPkgLinkInfo;
begin
  NewLink:=TPkgLinkInfo.Create;
  NewLink.Assign(APackage);
  NewLink.Origin:=FCollectingOrigin;
  FLinks.Add(NewLink);
end;

destructor TPackageLinksDialog.Destroy;
begin
  ClearLinks;
  inherited Destroy;
end;

{ TPkgLinkInfo }

procedure TPkgLinkInfo.Assign(Source: TLazPackageID);
var
  Link: TPackageLink;
begin
  AssignID(Source);
  if Source is TPackageLink then begin
    Link:=TPackageLink(Source);
    Origin:=Link.Origin;
    Filename:=Link.Filename;
    AutoCheckExists:=Link.AutoCheckExists;
    NotFoundCount:=Link.NotFoundCount;
    LastCheckValid:=Link.LastCheckValid;
    LastCheck:=Link.LastCheck;
    FileDateValid:=Link.FileDateValid;
    FileDate:=Link.FileDate;
  end;
end;

end.

