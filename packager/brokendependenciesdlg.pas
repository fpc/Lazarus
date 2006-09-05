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
  Classes, SysUtils, Forms, Controls, Buttons, LResources, StdCtrls, ComCtrls,
  FileCtrl, Dialogs,
  IDEWindowIntf, LazarusIDEStrConsts, Project, PackageDefs, PackageSystem;

type
  TBrokenDependenciesDialog = class(TForm)
    NoteLabel: TLabel;
    DependencyListView: TListView;
    procedure BrokenDependenciesDialogClose(Sender: TObject;
      var CloseAction: TCloseAction);
    procedure BrokenDependenciesDialogResize(Sender: TObject);
  private
    fButtons: TList; // list of TBitBtn
    fButtonSet: TMsgDlgButtons;
    function GetButtons(Btn: TMsgDlgBtn): TBitBtn;
    procedure SetupComponents;
    procedure ClearButtons;
  public
    DependencyList: TFPList;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Buttons[Btn: TMsgDlgBtn]: TBitBtn read GetButtons;
    procedure CreateButtons(BtnSet: TMsgDlgButtons);
    procedure UpdateDependencyList;
  end;
  
const
  DefaultBrokenDepButtons = [mbYes,mbIgnore,mbCancel,mbAbort];


function ShowBrokenDependencies(DependencyList: TFPList;
  BtnSet: TMsgDlgButtons): TModalResult;


implementation


function ShowBrokenDependencies(DependencyList: TFPList;
  BtnSet: TMsgDlgButtons): TModalResult;
var
  BrokenDependenciesDialog: TBrokenDependenciesDialog;
begin
  BrokenDependenciesDialog:=TBrokenDependenciesDialog.Create(nil);
  BrokenDependenciesDialog.DependencyList:=DependencyList;
  with BrokenDependenciesDialog do begin
    CreateButtons(BtnSet);
    UpdateDependencyList;
    Result:=ShowModal;
    Free;
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
  NoteLabel.SetBounds(5,5,x-10,80);
  y:=NoteLabel.Top+NoteLabel.Height+2;
  with DependencyListView do
    SetBounds(0,y,x,Parent.ClientHeight-y-40);
  y:=ClientHeight-35;
  for i:=fButtons.Count-1 downto 0 do begin
    CurButton:=TBitBtn(fButtons[i]);
    dec(x,CurButton.Width+10);
    with CurButton do
      SetBounds(x,y,80,Height);
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
  Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TBrokenDependenciesDialog.SetupComponents;
var
  NewColumn: TListColumn;
begin
  NoteLabel:=TLabel.Create(Self);
  with NoteLabel do begin
    Name:='NoteLabel';
    Parent:=Self;
    WordWrap:=true;
    Caption:=Format(lisBDDChangingThePackageNameOrVersionBreaksDependencies, [
      #13, #13]);
  end;

  DependencyListView:=TListView.Create(Self);
  with DependencyListView do begin
    Name:='DependencyListView';
    Parent:=Self;
    ViewStyle:=vsReport;
    NewColumn:=Columns.Add;
    NewColumn.Width:=200;
    NewColumn.Caption:='Package/Project';
    NewColumn:=Columns.Add;
    NewColumn.Caption:=lisA2PDependency;
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
  Caption:=lisA2PBrokenDependencies;
  fButtons:=TList.Create;
  SetupComponents;
  OnResize:=@BrokenDependenciesDialogResize;
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,500,300);
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
      if Btn=mbYes then NewBitBtn.Default:=true;
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

