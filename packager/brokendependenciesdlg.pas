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

  Abstract:
    TBrokenDependenciesDialog is the dialog showing, which dependencies are
    broken.
}
unit BrokenDependenciesDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons, LResources, ExtCtrls, StdCtrls,
  ComCtrls, FileCtrl, Dialogs, IDEProcs, IDEOptionDefs, LazarusIDEStrConsts,
  Project, PackageDefs, PackageSystem;

type
  TBrokenDependenciesDialog = class(TForm)
    DependencyListView: TListView;
    NoteLabel: TLabel;
    procedure BrokenDependenciesDialogClose(Sender: TObject;
      var Action: TCloseAction);
    procedure BrokenDependenciesDialogResize(Sender: TObject);
  private
    fButtons: TList; // list of TBitBtn
    fButtonSet: TMsgDlgButtons;
    function GetButtons(Btn: TMsgDlgBtn): TBitBtn;
    procedure SetupComponents;
    procedure ClearButtons;
  public
    DependencyList: TList;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Buttons[Btn: TMsgDlgBtn]: TBitBtn read GetButtons;
    procedure CreateButtons(BtnSet: TMsgDlgButtons);
    procedure UpdateDependencyList;
  end;
  
const
  DefaultBrokenDepButtons = [mbYes,mbIgnore,mbCancel,mbAbort];


function ShowBrokenDependencies(DependencyList: TList;
  BtnSet: TMsgDlgButtons): TModalResult;
function GetDependencyOwnerAsString(Dependency: TPkgDependency): string;


implementation


function ShowBrokenDependencies(DependencyList: TList;
  BtnSet: TMsgDlgButtons): TModalResult;
var
  BrokenDependenciesDialog: TBrokenDependenciesDialog;
begin
  BrokenDependenciesDialog:=TBrokenDependenciesDialog.Create(Application);
  BrokenDependenciesDialog.DependencyList:=DependencyList;
  with BrokenDependenciesDialog do begin
    CreateButtons(BtnSet);
    UpdateDependencyList;
    Result:=ShowModal;
    Free;
  end;
end;

function GetDependencyOwnerAsString(Dependency: TPkgDependency): string;
var
  DepOwner: TObject;
begin
  DepOwner:=Dependency.Owner;
  if (DepOwner<>nil) then begin
    if DepOwner is TLazPackage then begin
      Result:='Package: '+TLazPackage(DepOwner).IDAsString;
    end else if DepOwner is TProject then begin
      Result:='Project: '
                       +ExtractFileNameOnly(TProject(DepOwner).ProjectInfoFile);
    end else begin
      Result:=DepOwner.ClassName
    end;
  end else begin
    Result:='Dependency without Owner: '+Dependency.AsString;
  end;
end;

{ TBrokenDependenciesDialog }

procedure TBrokenDependenciesDialog.BrokenDependenciesDialogResize(
  Sender: TObject);
var
  i: Integer;
  y: Integer;
  x: Integer;
  CurButton: TBitBtn;
begin
  x:=ClientWidth;
  NoteLabel.SetBounds(5,5,x-10,100);
  y:=NoteLabel.Top+NoteLabel.Height+2;
  with DependencyListView do
    SetBounds(0,y,x,ClientHeight-y-40);
  y:=ClientHeight-35;
  for i:=fButtons.Count-1 downto 0 do begin
    CurButton:=TBitBtn(fButtons[i]);
    dec(x,CurButton.Width+10);
    CurButton.SetBounds(x,y,Width,Height);
  end;
end;

function TBrokenDependenciesDialog.GetButtons(Btn: TMsgDlgBtn): TBitBtn;
var
  CurBtn: TMsgDlgBtn;
  i: Integer;
begin
  if not (Btn in fButtonSet) then begin
    Result:=nil;
    exit;
  end;
  i:=0;
  for CurBtn:=Low(TMsgDlgButtons) to High(TMsgDlgButtons) do begin
    if CurBtn=Btn then break;
    if Btn in fButtonSet then inc(i);
  end;
  Result:=TBitBtn(fButtons[i]);
end;

procedure TBrokenDependenciesDialog.BrokenDependenciesDialogClose(
  Sender: TObject; var Action: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TBrokenDependenciesDialog.SetupComponents;
var
  NewColumn: TListColumn;
begin
  DependencyListView:=TListView.Create(Self);
  with DependencyListView do begin
    Name:='DependencyListView';
    Parent:=Self;
    ViewStyle:=vsReport;
    NewColumn:=Columns.Add;
    NewColumn.Width:=170;
    NewColumn.Caption:='Package/Project';
    NewColumn:=Columns.Add;
    NewColumn.Caption:='Dependency';
  end;
  
  NoteLabel:=TLabel.Create(Self);
  with NoteLabel do begin
    Name:='NoteLabel';
    Parent:=Self;
    WordWrap:=true;
    Caption:='Changing the package name or version would result in breaking'
      +' the dependencies listed below. Should they be changed as well?'#13
      +'Select Yes to change all listed dependencies.'#13
      +'Select Ignore to keep the dependencies untouched and continue.'#13
      +'Select Cancel to cancel the renaming and/or changing the version.'#13
      +'Select Abort to abort the current command.';
  end;
end;

procedure TBrokenDependenciesDialog.ClearButtons;
var
  i: Integer;
begin
  for i:=0 to fButtons.Count-1 do TBitBtn(fButtons[i]).Free;
  fButtons.Clear;
end;

constructor TBrokenDependenciesDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Name:='BrokenDependenciesDialog';
  Caption:='Broken Dependencies';
  fButtons:=TList.Create;
  SetupComponents;
  OnResize:=@BrokenDependenciesDialogResize;
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,350,300);
  OnResize(Self);
  OnClose:=@BrokenDependenciesDialogClose;
end;

destructor TBrokenDependenciesDialog.Destroy;
begin
  ClearButtons;
  fButtons.Free;
  inherited Destroy;
end;

procedure TBrokenDependenciesDialog.CreateButtons(BtnSet: TMsgDlgButtons);
var
  Btn: TMsgDlgBtn;
  NewBitBtn: TBitBtn;
begin
  ClearButtons;
  fButtonSet:=BtnSet;
  for Btn:=Low(TMsgDlgButtons) to High(TMsgDlgButtons) do begin
    if Btn in fButtonSet then begin
      NewBitBtn:=TBitBtn.Create(Self);
      NewBitBtn.Name:='BitBtn'+IntToStr(fButtons.Count+1);
      NewBitBtn.Kind:=MsgDlgBtnToBitBtnKind[Btn];
      NewBitBtn.Parent:=Self;
      fButtons.Add(NewBitBtn);
    end;
  end;
  OnResize(Self);
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

