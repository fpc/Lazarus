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

  Author: Juha Manninen

  Abstract:
    Defines build profiles for "Build Lazarus" function, and has a simple GUI
    for managing them.
}
unit BuildProfileManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls, ComCtrls, Contnrs, ButtonPanel,
  Laz_XMLCfg, DefineTemplates,
  IDEImagesIntf, IDEMsgIntf, IDEHelpIntf,
  LazarusIDEStrConsts, LazConf, InterfaceBase, IDEProcs,
  IDEContextHelpEdit, CompilerOptions;

type

  TIdeBuildMode = (
    bmBuild,
    bmCleanBuild,
    bmCleanAllBuild
  );

  TBuildLazarusProfiles = class;

  { TBuildLazarusProfile }

  TBuildLazarusProfile = class
  private
    fOwnerCnt: TBuildLazarusProfiles;
    fName: string;
    fTargetOS: string;
    fTargetDirectory: string;
    fTargetCPU: string;
    fTargetPlatform: TLCLPlatform;
    fIdeBuildMode: TIdeBuildMode;
    fUpdateRevisionInc: boolean;
    fOptions: TStringList;      // User defined options.
    fDefines: TStringList;      // Defines selected for this profile.

    function GetExtraOptions: string;
    procedure SetExtraOptions(const AValue: string);
  public
    constructor Create(AOwnerCnt: TBuildLazarusProfiles; AName: string);
    destructor Destroy; override;
    procedure Assign(Source: TBuildLazarusProfile; ACopyName: Boolean=True);
    procedure Load(XMLConfig: TXMLConfig; const Path: string);
    procedure Save(XMLConfig: TXMLConfig; const Path: string);
    function FPCTargetOS: string;
    function FPCTargetCPU: string;
  public
    property Name: string read fName;
    property ExtraOptions: string read GetExtraOptions write SetExtraOptions;
    property TargetOS: string read fTargetOS write fTargetOS;
    property TargetDirectory: string read fTargetDirectory write fTargetDirectory;
    property TargetCPU: string read fTargetCPU write fTargetCPU;
    property TargetPlatform: TLCLPlatform read fTargetPlatform write fTargetPlatform;
    property IdeBuildMode: TIdeBuildMode read fIdeBuildMode write fIdeBuildMode;
    property UpdateRevisionInc: boolean read fUpdateRevisionInc write fUpdateRevisionInc;
    property OptionsLines: TStringList read fOptions;
    property Defines: TStringList read fDefines;
  end;

  { TBuildLazarusProfiles }

  TBuildLazarusProfiles = class(TObjectList)
  private
    fRestartAfterBuild: boolean;
    fConfirmBuild: boolean;
    fAllDefines: TStringList;
    fSelected: TStringList;
    fStaticAutoInstallPackages: TStringList;
    fCurrentIndex: integer;
    function GetCurrentProfile: TBuildLazarusProfile;
    function GetItems(Index: integer): TBuildLazarusProfile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TBuildLazarusProfiles);
    function IndexByName(AName: string): integer;
    function CreateDefaults: integer;
    procedure Load(XMLConfig: TXMLConfig; const Path: string; const FileVersion: integer);
    procedure Save(XMLConfig: TXMLConfig; const Path: string);
    procedure Move(CurIndex, NewIndex: Integer); // Replaces TList.Move
  public
    property RestartAfterBuild: boolean read fRestartAfterBuild write fRestartAfterBuild;
    property ConfirmBuild: boolean read fConfirmBuild write fConfirmBuild;
    property AllDefines: TStringList read fAllDefines;
    property Selected: TStringList read fSelected;
    property StaticAutoInstallPackages: TStringList read fStaticAutoInstallPackages;
    property CurrentIndex: integer read fCurrentIndex write fCurrentIndex;
    property Current: TBuildLazarusProfile read GetCurrentProfile;
    property Items[Index: integer]: TBuildLazarusProfile read GetItems; default;
  end;

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
    procedure HelpButtonClick(Sender: TObject);
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

uses
  AddProfileDialog;

const
  DefaultTargetDirectory = ''; // empty will be replaced by '$(ConfDir)/bin';


function IdeBuildModeToStr(BuildMode: TIdeBuildMode): string;
begin
  case BuildMode of
    bmBuild:         Result:='Build';
    bmCleanBuild:    Result:='Clean + Build';
    bmCleanAllBuild: Result:='Clean All + Build';
  end;
end;

function StrToIdeBuildMode(const s: string): TIdeBuildMode;
begin
  Result:=bmBuild;
  if s='Clean + Build' then
    Result:=bmCleanBuild
  else if s='Clean All + Build' then
    Result:=bmCleanAllBuild;
end;


{ TBuildLazarusProfile }

constructor TBuildLazarusProfile.Create(AOwnerCnt: TBuildLazarusProfiles;
                                        AName: string);
begin
  inherited Create;
  fOwnerCnt:=AOwnerCnt;
  fName:=AName;
  fOptions:=TStringList.Create;
  fDefines:=TStringList.Create;
end;

destructor TBuildLazarusProfile.Destroy;
begin
  fDefines.Free;
  fOptions.Free;
  inherited Destroy;
end;

procedure TBuildLazarusProfile.Load(XMLConfig: TXMLConfig; const Path: string);
var
  LCLPlatformStr: string;
begin
  TargetOS      :=XMLConfig.GetValue(Path+'TargetOS/Value','');
  TargetCPU     :=XMLConfig.GetValue(Path+'TargetCPU/Value','');
  LCLPlatformStr:=XMLConfig.GetValue(Path+'LCLPlatform/Value','');
  if LCLPlatformStr='' then
    fTargetPlatform:=GetDefaultLCLWidgetType
  else
    fTargetPlatform  :=DirNameToLCLPlatform(LCLPlatformStr);
  FTargetDirectory:=AppendPathDelim(SetDirSeparators(
      XMLConfig.GetValue(Path+'TargetDirectory/Value', DefaultTargetDirectory)));
  IdeBuildMode:=StrToIdeBuildMode(XMLConfig.GetValue(Path+'IdeBuildMode/Value',''));
  FUpdateRevisionInc :=XMLConfig.GetValue(Path+'UpdateRevisionInc/Value',true);
  LoadStringList(XMLConfig,fOptions,Path+'Options/');
  if fOptions.Count=0 then     // Support a syntax used earlier by profiles.
    fOptions.Text:=XMLConfig.GetValue(Path+'ExtraOptions/Value','');
  LoadStringList(XMLConfig,fDefines,Path+'Defines/');
end;

procedure TBuildLazarusProfile.Save(XMLConfig: TXMLConfig; const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'TargetOS/Value',TargetOS,'');
  XMLConfig.SetDeleteValue(Path+'TargetCPU/Value',TargetCPU,'');
  XMLConfig.SetDeleteValue(Path+'LCLPlatform/Value',
                           LCLPlatformDirNames[fTargetPlatform],
                           ''); //LCLPlatformDirNames[GetDefaultLCLWidgetType]
  XMLConfig.SetDeleteValue(Path+'TargetDirectory/Value',
                           FTargetDirectory,DefaultTargetDirectory);
  XMLConfig.SetDeleteValue(Path+'IdeBuildMode/Value',IdeBuildModeToStr(IdeBuildMode),'');
  XMLConfig.SetDeleteValue(Path+'UpdateRevisionInc/Value',FUpdateRevisionInc,true);
  SaveStringList(XMLConfig,fOptions,Path+'Options/');
  SaveStringList(XMLConfig,fDefines,Path+'Defines/');
end;

procedure TBuildLazarusProfile.Assign(Source: TBuildLazarusProfile; ACopyName: Boolean);
begin
  if (Source=nil) or (Source=Self) then exit;
  if ACopyName then
    fName           :=Source.Name;
  TargetOS          :=Source.TargetOS;
  TargetDirectory   :=Source.TargetDirectory;
  TargetCPU         :=Source.TargetCPU;
  TargetPlatform    :=Source.TargetPlatform;
  IdeBuildMode      :=Source.IdeBuildMode;
  UpdateRevisionInc :=Source.UpdateRevisionInc;
  fOptions.Assign(Source.fOptions);
  fDefines.Assign(Source.fDefines);
end;

function TBuildLazarusProfile.FPCTargetOS: string;
begin
  Result:=GetFPCTargetOS(TargetOS);
end;

function TBuildLazarusProfile.FPCTargetCPU: string;
begin
  Result:=GetFPCTargetCPU(TargetCPU);
end;

function TBuildLazarusProfile.GetExtraOptions: string;
var
  i: Integer;
begin
  Result:='';
  for i:=0 to fOptions.Count-1 do
    Result:=Result+' '+fOptions[i];
  Result:=Trim(Result);
  for i:=0 to fDefines.Count-1 do
    Result:=Result+' -d'+fDefines[i];
  Result:=Trim(Result);
end;

procedure TBuildLazarusProfile.SetExtraOptions(const AValue: string);
begin
  fOptions.Text:=AValue;
end;


{ TBuildLazarusProfiles }

constructor TBuildLazarusProfiles.Create;
begin
  inherited Create;
  fRestartAfterBuild:=True;
  fConfirmBuild:=True;
  fAllDefines:=TStringList.Create;
  fSelected:=TStringList.Create;
  fStaticAutoInstallPackages:=TStringList.Create;
end;

destructor TBuildLazarusProfiles.Destroy;
begin
  inherited Destroy;
  // Clear is called by inherited Destroy. Must be freed later.
  fStaticAutoInstallPackages.Free;
  fSelected.Free;
  fAllDefines.Free;
end;

procedure TBuildLazarusProfiles.Clear;
begin
  fAllDefines.Clear;
  fSelected.Clear;
  fStaticAutoInstallPackages.Clear;
  inherited Clear;
end;

procedure TBuildLazarusProfiles.Assign(Source: TBuildLazarusProfiles);
var
  i: Integer;
  SrcItem, NewItem: TBuildLazarusProfile;
begin
  Clear;
  RestartAfterBuild :=Source.RestartAfterBuild;
  ConfirmBuild:=Source.ConfirmBuild;
  fAllDefines.Assign(Source.fAllDefines);
  fSelected.Assign(Source.fSelected);
  fStaticAutoInstallPackages.Assign(Source.fStaticAutoInstallPackages);
  fCurrentIndex:=Source.fCurrentIndex;
  for i:=0 to Source.Count-1 do begin
    SrcItem:=Source.Items[i];
    NewItem:=TBuildLazarusProfile.Create(Self, SrcItem.Name);
    NewItem.Assign(SrcItem);
    Add(NewItem);
  end;
end;

function TBuildLazarusProfiles.IndexByName(AName: string): integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to Count-1 do
    if Items[i].Name=AName then begin
      Result:=i;
      break;
    end;
end;

function TBuildLazarusProfiles.CreateDefaults: integer;
// Create a set of default profiles when none are saved.
// Returns index for the default selected profile.
var
  Profile: TBuildLazarusProfile;
  Platfrm: TLCLPlatform;
begin
  Platfrm:=GetDefaultLCLWidgetType;

  // Build Normal IDE
  Profile:=TBuildLazarusProfile.Create(Self, lisLazBuildNormalIDE);
  with Profile, fOwnerCnt do begin
    fTargetPlatform:=Platfrm;
    fIdeBuildMode:=bmBuild;
    fUpdateRevisionInc:=True;
  end;
  // Return this one as default. Needed when building packages without saved profiles.
  Result:=Add(Profile);

  // Build Debug IDE
  Profile:=TBuildLazarusProfile.Create(Self, lisLazBuildDebugIDE);
  with Profile, fOwnerCnt do begin
    fTargetPlatform:=Platfrm;
    fIdeBuildMode:=bmBuild;
    fUpdateRevisionInc:=True;
    {$IFDEF Darwin}
    // FPC on darwin has a bug with -Cr
    fOptions.Add('-gw -gl -godwarfsets -gh -gt -Co -Ci -Sa');
    {$ELSE}
    fOptions.Add('-gw -gl -godwarfsets -gh -gt -Co -Cr -Ci -Sa');
    {$ENDIF}
  end;
  Add(Profile);

  // Build Optimised IDE
  Profile:=TBuildLazarusProfile.Create(Self, lisLazBuildOptimizedIDE);
  with Profile, fOwnerCnt do begin
    fTargetPlatform:=Platfrm;
    fIdeBuildMode:=bmBuild;
    fUpdateRevisionInc:=True;
    fOptions.Add('-O2 -g- -Xs');
  end;
  Add(Profile);

  // Clean Up + Build all
  Profile:=TBuildLazarusProfile.Create(Self, lisLazCleanUpBuildAll);
  with Profile, fOwnerCnt do begin
    fTargetPlatform:=Platfrm;
    fIdeBuildMode:=bmCleanBuild;
    fUpdateRevisionInc:=True;
  end;
  Add(Profile);

  // Defines to test.
  if fAllDefines.Count = 0 then begin
    fAllDefines.Add('Debug');
    fAllDefines.Add('Verbose');
  end;
end;

procedure TBuildLazarusProfiles.Load(XMLConfig: TXMLConfig; const Path: string;
                                     const FileVersion: integer);
var
  i, ProfCount, ProfInd: Integer;
  ProfPath, ProfName: string;
  Profile: TBuildLazarusProfile;
begin
  Clear;
  case FileVersion of
    // Older config file version.
    1: begin
      CreateDefaults;         // Only one profile saved, create defaults always.
      // Then create MyProfile.
      Profile:=TBuildLazarusProfile.Create(Self, 'MyProfile');
      Profile.Load(XMLConfig, Path);
      Add(Profile);
      FRestartAfterBuild:=XMLConfig.GetValue(Path+'RestartAfterBuild/Value',true);
      FConfirmBuild     :=XMLConfig.GetValue(Path+'ConfirmBuild/Value',true);
      ProfInd:=Count-1;       // Go to last MyProfile.
    end;
    // Latest config file version.
    2: begin
      ProfCount:=XMLConfig.GetValue(Path+'Profiles/Count',0);
      if ProfCount = 0 then
        ProfInd:=CreateDefaults    // No saved profiles were found, use defaults.
      else begin
        // Load list of profiles.
        for i:=0 to ProfCount-1 do begin
          ProfPath:=Path+'Profiles/Profile'+IntToStr(i)+'/';
          ProfName:=XMLConfig.GetValue(ProfPath+'Name','Unknown');
          Profile:=TBuildLazarusProfile.Create(Self, ProfName);
          Profile.Load(XMLConfig, ProfPath);
          Add(Profile);
        end;
        // Current profile ItemIndex.
        ProfInd:=XMLConfig.GetValue(Path+'ProfileIndex/Value',0);
        // Other global build values.
        FRestartAfterBuild:=XMLConfig.GetValue(Path+'RestartAfterBuild/Value',true);
        FConfirmBuild     :=XMLConfig.GetValue(Path+'ConfirmBuild/Value',true);
      end
    end;
    // Invalid config file.
    else
      ProfInd:=CreateDefaults;
  end;
  // Load defines, selected profiles and auto install packages.
  LoadStringList(XMLConfig,fAllDefines,Path+'AllDefines/');
  LoadStringList(XMLConfig,fSelected,Path+'SelectedProfiles/');
  LoadStringList(XMLConfig,fStaticAutoInstallPackages,Path+'StaticAutoInstallPackages/');
  // Defines to test.
  if fAllDefines.Count = 0 then begin
    fAllDefines.Add('Debug');
    fAllDefines.Add('Verbose');
  end;
  fCurrentIndex:=ProfInd;
end;

procedure TBuildLazarusProfiles.Save(XMLConfig: TXMLConfig; const Path: string);
var
  i: Integer;
  ProfPath, n: string;
begin
  // Save list of profiles.
  XMLConfig.SetDeleteValue(Path+'Profiles/Count',Count,0);
  for i:=0 to Count-1 do begin
    ProfPath:=Path+'Profiles/Profile'+IntToStr(i)+'/';
    n:=Items[i].Name;
    XMLConfig.SetDeleteValue(ProfPath+'Name',n,'');
    Items[i].Save(XMLConfig, ProfPath);
  end;
  // Current profile ItemIndex.
  XMLConfig.SetDeleteValue(Path+'ProfileIndex/Value',CurrentIndex,0);
  // Other global build values.
  XMLConfig.SetDeleteValue(Path+'RestartAfterBuild/Value',FRestartAfterBuild,true);
  XMLConfig.SetDeleteValue(Path+'ConfirmBuild/Value',FConfirmBuild,true);
  // Save defines, selected profiles and auto install packages.
  SaveStringList(XMLConfig,fAllDefines,Path+'AllDefines/');
  SaveStringList(XMLConfig,fSelected,Path+'SelectedProfiles/');
  SaveStringList(XMLConfig,fStaticAutoInstallPackages,Path+'StaticAutoInstallPackages/');
end;

procedure TBuildLazarusProfiles.Move(CurIndex, NewIndex: Integer);
begin
  inherited Move(CurIndex, NewIndex);
  fCurrentIndex:=NewIndex;
end;

function TBuildLazarusProfiles.GetCurrentProfile: TBuildLazarusProfile;
begin
  Result:=Items[fCurrentIndex];
end;

function TBuildLazarusProfiles.GetItems(Index: integer): TBuildLazarusProfile;
begin
  Result:=TBuildLazarusProfile(inherited Items[Index]);
end;


{ TBuildProfileManagerForm }

procedure TBuildProfileManagerForm.FormCreate(Sender: TObject);
begin
  Caption := lisLazBuildManageProfiles;

  ProfilesToolBar.Images := IDEImages.Images_16;
  AddButton.ImageIndex     :=IDEImages.LoadImage(16, 'laz_add');
  RemoveButton.ImageIndex  :=IDEImages.LoadImage(16, 'laz_delete');
  EditButton.ImageIndex    :=IDEImages.LoadImage(16, 'laz_edit');
  MoveUpButton.ImageIndex  :=IDEImages.LoadImage(16, 'arrow_up');
  MoveDownButton.ImageIndex:=IDEImages.LoadImage(16, 'arrow_down');

  AddButton.Caption:=lisLazBuildAdd;
  RemoveButton.Caption:=lisLazBuildRemove;
  EditButton.Caption:=lisLazBuildRename;
  MoveUpButton.Caption:=lisExtToolMoveUp;
  MoveDownButton.Caption:=lisExtToolMoveDown;

  ButtonPanel.OKButton.Caption:=lisOk;
  ButtonPanel.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel.CancelButton.Caption:=dlgCancel;

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
    ProfilesListBox.Items.Add(fProfsToManage[i].Name);
  ProfilesListBox.ItemIndex:=fProfsToManage.CurrentIndex;
end;

procedure TBuildProfileManagerForm.ProfilesListboxClick(Sender: TObject);
begin
  if fProfsToManage.Count>0 then begin
    fProfsToManage.fCurrentIndex:=(Sender as TListbox).ItemIndex;
    EnableButtons;
  end;
end;

procedure TBuildProfileManagerForm.AddButtonClick(Sender: TObject);
var
  NewProfile: TBuildLazarusProfile;
begin
  with TAddProfileForm.Create(nil) do
  try
    Caption:=lisLazBuildNewProf;
    ProfileHeaderLabel.Caption:=lisLazBuildNewProfInfo;
    if (ShowModal=mrOk) and (NameEdit.Text<>'') then begin
      // Update ProfsToManage collection.
      NewProfile:=TBuildLazarusProfile.Create(fProfsToManage,NameEdit.Text);
      NewProfile.Assign(fProfsToManage.Current, False);
      fProfsToManage.Add(NewProfile);
      fProfsToManage.fCurrentIndex:=fProfsToManage.Count-1; // Select the new profile.
      // Update ListBox
      ProfilesListbox.Items.Add(NameEdit.Text);
      ProfilesListbox.ItemIndex:=ProfilesListbox.Count-1;
      EnableButtons;
    end;
  finally
    Free;
  end;
end;

procedure TBuildProfileManagerForm.RemoveButtonClick(Sender: TObject);
var
  i, SelI, NewI: integer;
begin
  i := ProfilesListbox.ItemIndex;
  if i<0 then exit;
  // Remove the item from selected list.
  if MessageDlg(lisLazBuildConfirmDeletion,
    lisLazBuildAreYouSureYouWantToDeleteThisBuildProfile, mtConfirmation,
    [mbYes, mbNo], 0)=mrYes then
  begin
    SelI:=fProfsToManage.Selected.IndexOf(fProfsToManage[i].fName);
    if SelI>-1 then
      fProfsToManage.Selected.Delete(SelI);
    // New last item index.
    NewI:=i;
    if i=ProfilesListbox.Items.Count-1 then
      Dec(NewI);
    // Update ProfsToManage collection.
    fProfsToManage.Delete(i);
    fProfsToManage.fCurrentIndex:=NewI;
    // Update ListBox
    ProfilesListBox.Items.Delete(i);
    ProfilesListBox.ItemIndex:=NewI;
    EnableButtons;
  end;
end;

procedure TBuildProfileManagerForm.EditButtonClick(Sender: TObject);
var
  i, SelI: integer;
begin
  i:=ProfilesListbox.ItemIndex;
  if i<0 then exit;
  with TAddProfileForm.Create(nil) do
  try
    Caption:=lisLazBuildRenameProf;
    ProfileHeaderLabel.Caption:=lisLazBuildRenameProfInfo;
    NameEdit.Text:=ProfilesListbox.Items[i];
    if (ShowModal=mrOk) and (NameEdit.Text<>'') then begin
      // Update ProfsToManage collection.
      fProfsToManage[i].fName:=NameEdit.Text;
      // Update selected list.
      SelI:=fProfsToManage.Selected.IndexOf(ProfilesListbox.Items[i]);
      if SelI>-1 then
        fProfsToManage.Selected[SelI]:=NameEdit.Text;
      // Update ListBox
      ProfilesListbox.Items[i]:=NameEdit.Text;
      EnableButtons;
    end;
  finally
    Free;
  end;
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

procedure TBuildProfileManagerForm.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

end.

