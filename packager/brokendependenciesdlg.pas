{  $Id$  }
{
 /***************************************************************************
                          brokendependenciesdlg.pas
                          -------------------------


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
  Form resource added: Alexey Torgashin

  Abstract:
    TBrokenDependenciesDialog is the dialog showing, which dependencies are broken.
}
unit BrokenDependenciesDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, ComCtrls, ButtonPanel,
  LazarusIDEStrConsts, PackageDefs;


type
  { TBrokenDependenciesDialog }

  TBrokenDependenciesDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    DependencyListView: TListView;
    NoteLabel: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    DependencyList: TFPList;
    procedure UpdateDependencyList;
  end;
  

function ShowBrokenDependencies(DependencyList: TFPList): TModalResult;


implementation

{$R *.lfm}

function ShowBrokenDependencies(DependencyList: TFPList): TModalResult;
var
  Dlg: TBrokenDependenciesDialog;
begin
  Dlg:=TBrokenDependenciesDialog.Create(nil);
  try
    Dlg.DependencyList:=DependencyList;
    Dlg.UpdateDependencyList;
    Result:=Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{ TBrokenDependenciesDialog }

procedure TBrokenDependenciesDialog.FormCreate(Sender: TObject);
begin
  Caption:=lisA2PBrokenDependencies;
  ButtonPanel1.OKButton.Caption:=lisYes;
  ButtonPanel1.CloseButton.Caption:=lisIgnore;

  NoteLabel.Caption:=Format(lisBDDChangingThePackageNameOrVersionBreaksDependencies,
                            [LineEnding, LineEnding]);

  DependencyListView.Columns[0].Caption:=lisA2PPackageOrProject;
  DependencyListView.Columns[1].Caption:=lisA2PDependency;
end;


procedure TBrokenDependenciesDialog.UpdateDependencyList;
var
  i: Integer;
  Dependency: TPkgDependency;
  li: TListItem;
begin
  if DependencyList=nil then begin
    DependencyListView.Items.Clear;
    exit;
  end;
  for i:=0 to DependencyList.Count-1 do begin
    Dependency:=TPkgDependency(DependencyList[i]);
    if i>=DependencyListView.Items.Count then begin
      li:=DependencyListView.Items.Add;
      li.SubItems.Add('');
    end else
      li:=DependencyListView.Items[i];
    li.Caption:=GetDependencyOwnerAsString(Dependency);
    li.SubItems[0]:=Dependency.AsString;
  end;
end;

end.

