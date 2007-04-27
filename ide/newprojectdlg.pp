{
  Author: Mattias Gaertner

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
 
  Abstract:
    The new project dialog for lazarus.

}
unit NewProjectDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Controls, LResources, Project, Buttons,
  StdCtrls, ProjectIntf, LazarusIDEStrConsts;

type

{ TNewProjectDialog }

  TNewProjectDialog = class(TForm)
    CreateButton: TButton;
    CancelButton: TButton;
    ListBox: TListBox;
    HelpLabel: TLabel;
    procedure CreateButtonClick(Sender:TObject);
    procedure CancelButtonClick(Sender:TObject);
    procedure ListBoxDblClick(Sender: TObject);
    procedure ListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    procedure FillHelpLabel;
    procedure SetupComponents;
  public
    constructor Create(AOwner: TComponent); override;
    function GetProjectDescriptor: TProjectDescriptor;
  end;

function ChooseNewProject(var ProjectDesc: TProjectDescriptor): TModalResult;

implementation

function ChooseNewProject(var ProjectDesc: TProjectDescriptor):TModalResult;
var
  NewProjectDialog: TNewProjectDialog;
begin
  ProjectDesc:=nil;
  NewProjectDialog:=TNewProjectDialog.Create(nil);
  try
    Result:=NewProjectDialog.ShowModal;
    if Result=mrOk then
      ProjectDesc:=NewProjectDialog.GetProjectDescriptor;
  finally
    NewProjectDialog.Free;
  end;
end;

{ NewProjectDialog }

constructor TNewProjectDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width:=390;
  Height:=240;
  Position:=poScreenCenter;
  Caption:=lisNPCreateANewProject;
  SetupComponents;
  FillHelpLabel;
end;

function TNewProjectDialog.GetProjectDescriptor: TProjectDescriptor;
var
  i: LongInt;
  s: string;
begin
  Result:=ProjectDescriptorApplication;
  i:=ListBox.ItemIndex;
  if (i<0) then exit;
  s:=ListBox.Items[i];
  for i:=0 to ProjectDescriptors.Count-1 do
    if ProjectDescriptors[i].GetLocalizedName=s then
      exit(ProjectDescriptors[i]);
end;

procedure TNewProjectDialog.FillHelpLabel;
begin
  HelpLabel.Caption:=GetProjectDescriptor.GetLocalizedDescription;
  HelpLabel.Width:=Self.ClientWidth-HelpLabel.Left-10;
end;

procedure TNewProjectDialog.SetupComponents;
var
  i: integer;
  MaxX, MaxY: integer;
begin
  MaxX:=386;
  MaxY:=238;

  ListBox:=TListBox.Create(Self);
  with ListBox do begin
    Parent:=Self;
    Name:='ListBox';
    Left:=5;
    Top:=5;
    Width:=MaxX-200;
    Height:=MaxY-50;
    Anchors := [akTop,akLeft,akRight,akBottom];
    with Items do begin
      BeginUpdate;
      for i:=0 to ProjectDescriptors.Count-1 do begin
        if ProjectDescriptors[i].VisibleInNewDialog then
          Add(ProjectDescriptors[i].GetLocalizedName);
      end;
      EndUpdate;
    end;
    ItemIndex:=0;
    OnDblClick:=@ListBoxDblClick;
    OnSelectionChange:=@ListBoxSelectionChange;
  end;

  HelpLabel:=TLabel.Create(Self);
  with HelpLabel do begin
    Parent:=Self;
    Name:='HelpLabel';
    Anchors := [akTop,akRight,akBottom];
    WordWrap:=true;
    Caption:=lisNPSelectAProjectType;
    AnchorToCompanion(akLeft,6,ListBox);
    AnchorParallel(akRight,6,Parent);
  end;

  CreateButton:=TButton.Create(Self);
  with CreateButton do begin
    Parent:=Self;
    Name:='CreateButton';
    Width:=80;
    Height:=23;
    Left:=Self.ClientWidth-Width*2-2*15;
    Top:=Self.ClientHeight-40;
    Anchors := [akRight,akBottom];
    OnClick:=@CreateButtonClick;
    Caption:=lisNPCreate;
    Default:=true;
  end;

  CancelButton:=TButton.Create(Self);
  with CancelButton do begin
    Parent:=Self;
    Name:='CancelButton';
    Width:=80;
    Height:=23;
    Left:=Self.ClientWidth-Width-15;
    Top:=CreateButton.Top;
    Anchors := [akRight,akBottom];
    OnClick:=@CancelButtonClick;
    Caption:=dlgCancel;
    Cancel:=true;
  end;
end;

procedure TNewProjectDialog.CreateButtonClick(Sender:TObject);
begin
  ModalResult:=mrOk;
end;

procedure TNewProjectDialog.CancelButtonClick(Sender:TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TNewProjectDialog.ListBoxDblClick(Sender: TObject);
begin
  if ListBox.GetIndexAtY(ListBox.ScreenToClient(Mouse.CursorPos).Y) >= 0
  then CreateButtonClick(Self);
end;

procedure TNewProjectDialog.ListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  FillHelpLabel;
end;

end.
