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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Juha Manninen

  Abstract:
    Defines build profiles for "Build Lazarus" function, and has a simple GUI
    for managing them.
}
unit BuildProfileManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, System.UITypes,
  // LazUtils
  LazLoggerBase, LazFileUtils,
  // LCL
  Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, ButtonPanel,
  InterfaceBase, LCLPlatformDef,
  // Codetools
  DefineTemplates,
  // IdeIntf
  IdeIntfStrConsts, IDEImagesIntf, IDEDialogs,
  // IdeConfig
  EnvironmentOpts, TransferMacros, RecentListProcs, MiscOptions,
  // IDE
  LazarusIDEStrConsts;

type
  { TBuildProfileManagerForm }

  TBuildProfileManagerForm = class(TForm)
    AddButton: TToolButton;
    ButtonPanel:TButtonPanel;
    EditButton: TToolButton;
    MoveDownButton: TToolButton;
    MoveUpButton: TToolButton;
    ProfilesListBox: TListBox;
    ProfilesPanel: TPanel;
    ProfilesToolBar: TToolBar;
    RemoveButton: TToolButton;
    tbSeparator: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ProfilesListboxClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure MoveUpButtonClick(Sender: TObject);
    procedure MoveDownButtonClick(Sender: TObject);
  private
    fProfsToManage: TBuildLazarusProfiles;
    procedure EnableButtons;
  public
    procedure Prepare(AProfiles: TBuildLazarusProfiles);
    // Assigned by caller when opening/closing this form.
    property  ProfsToManage: TBuildLazarusProfiles read fProfsToManage;

  end; 

var
  BuildProfileManagerForm: TBuildProfileManagerForm;


implementation

{$R *.lfm}

{ TBuildProfileManagerForm }

procedure TBuildProfileManagerForm.FormCreate(Sender: TObject);
begin
  Caption := lisLazBuildManageProfiles;

  ProfilesToolBar.Images := IDEImages.Images_16;
  AddButton.ImageIndex     :=IDEImages.LoadImage('laz_add');
  RemoveButton.ImageIndex  :=IDEImages.LoadImage('laz_delete');
  EditButton.ImageIndex    :=IDEImages.LoadImage('laz_edit');
  MoveUpButton.ImageIndex  :=IDEImages.LoadImage('arrow_up');
  MoveDownButton.ImageIndex:=IDEImages.LoadImage('arrow_down');

  AddButton.Caption:=lisAdd;
  RemoveButton.Caption:=lisRemove;
  EditButton.Caption:=lisRename;
  MoveUpButton.Caption:=lisUp;
  MoveDownButton.Caption:=lisDown;

  ButtonPanel.OKButton.Caption:=lisBtnOk;
  ButtonPanel.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel.CancelButton.Caption:=lisCancel;

  fProfsToManage:=TBuildLazarusProfiles.Create;
end;

procedure TBuildProfileManagerForm.FormDestroy(Sender: TObject);
begin
  fProfsToManage.Free;
end;

procedure TBuildProfileManagerForm.Prepare(AProfiles: TBuildLazarusProfiles);
var
  i: Integer;
begin
  fProfsToManage.Assign(AProfiles);
  for i:=0 to fProfsToManage.Count-1 do
    ProfilesListBox.Items.Add(fProfsToManage[i].TranslatedName);
  ProfilesListBox.ItemIndex:=fProfsToManage.CurrentIndex;
end;

procedure TBuildProfileManagerForm.ProfilesListboxClick(Sender: TObject);
begin
  if fProfsToManage.Count>0 then begin
    fProfsToManage.CurrentIndex:=(Sender as TListbox).ItemIndex;
    EnableButtons;
  end;
end;

procedure TBuildProfileManagerForm.AddButtonClick(Sender: TObject);
var
  NewProfile: TBuildLazarusProfile;
  Str: string;
begin
  Str:= '';
  if not InputQuery(lisLazBuildNewProf, lisLazBuildNewProfInfo, Str) then Exit;
  if Str='' then Exit;

  // Update ProfsToManage collection.
  NewProfile:=TBuildLazarusProfile.Create(fProfsToManage,Str);
  NewProfile.Assign(fProfsToManage.Current, False);
  fProfsToManage.Add(NewProfile);
  fProfsToManage.CurrentIndex:=fProfsToManage.Count-1; // Select the new profile.
  // Update ListBox
  ProfilesListbox.Items.Add(Str);
  ProfilesListbox.ItemIndex:=ProfilesListbox.Count-1;
  EnableButtons;
end;

procedure TBuildProfileManagerForm.RemoveButtonClick(Sender: TObject);
var
  i, SelI, NewI: integer;
begin
  i := ProfilesListbox.ItemIndex;
  if i<0 then exit;
  // Remove the item from selected list.
  if IDEMessageDialog(lisLazBuildConfirmDeletion,
    lisLazBuildAreYouSureYouWantToDeleteThisBuildProfile, mtConfirmation,
    [mbYes, mbNo])=mrYes then
  begin
    SelI:=fProfsToManage.Selected.IndexOf(fProfsToManage[i].TranslatedName);
    if SelI>-1 then
      fProfsToManage.Selected.Delete(SelI);
    // New last item index.
    NewI:=i;
    if i=ProfilesListbox.Items.Count-1 then
      Dec(NewI);
    // Update ProfsToManage collection.
    fProfsToManage.Delete(i);
    fProfsToManage.CurrentIndex:=NewI;
    // Update ListBox
    ProfilesListBox.Items.Delete(i);
    ProfilesListBox.ItemIndex:=NewI;
    EnableButtons;
  end;
end;

procedure TBuildProfileManagerForm.EditButtonClick(Sender: TObject);
var
  i, SelI: integer;
  Str: string;
begin
  i:=ProfilesListbox.ItemIndex;
  if i<0 then exit;

  Str:= ProfilesListbox.Items[i];
  if not InputQuery(lisLazBuildRenameProf, lisLazBuildRenameProfInfo, Str) then Exit;
  if (Str='') or (Str=ProfilesListbox.Items[i]) then Exit;

  // Update ProfsToManage collection.
  fProfsToManage[i].Name:=Str;
  // Update selected list.
  SelI:=fProfsToManage.Selected.IndexOf(ProfilesListbox.Items[i]);
  if SelI>-1 then
    fProfsToManage.Selected[SelI]:=Str;
  // Update ListBox
  ProfilesListbox.Items[i]:=Str;
  EnableButtons;
end;

procedure TBuildProfileManagerForm.MoveUpButtonClick(Sender: TObject);
var
  i: integer;
begin
  i:=ProfilesListbox.ItemIndex;
  if i<1 then exit;
  fProfsToManage.Move(i,i-1);
  ProfilesListbox.Items.Move(i,i-1);
  ProfilesListbox.ItemIndex:=i-1;
  EnableButtons;
end;

procedure TBuildProfileManagerForm.MoveDownButtonClick(Sender: TObject);
var
  i: integer;
begin
  i:=ProfilesListbox.ItemIndex;
  if (i<0) or (i>=ProfilesListbox.Items.Count-1) then exit;
  fProfsToManage.Move(i,i+1);
  ProfilesListbox.Items.Move(i,i+1);
  ProfilesListbox.ItemIndex:=i+1;
  EnableButtons;
end;

procedure TBuildProfileManagerForm.EnableButtons;
var
  i: integer;
begin
  i:=ProfilesListbox.ItemIndex;
  AddButton.Enabled:=True;
  RemoveButton.Enabled:=(i>=0) and (ProfilesListbox.Items.Count>1);
  EditButton.Enabled:=(i>=0);
  MoveUpButton.Enabled:=(i>0);
  MoveDownButton.Enabled:=(i>=0) and (i<ProfilesListbox.Items.Count-1);
end;

end.

