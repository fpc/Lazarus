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
  IDEImagesIntf, IDEMsgIntf,
  LazarusIDEStrConsts, LazConf, InterfaceBase, IDEProcs,
  IDEContextHelpEdit, CompilerOptions;

type

  TMakeMode = (
    mmNone,
    mmBuild,
    mmCleanBuild
  );
  TMakeModes = set of TMakeMode;
  // Used for the actual mode settings in profiles.
  TMakeModeSettings = array of TMakeMode;

  { TMakeModeDef }

  TMakeModeDef = class
  private
    fName: string;
    fDescription: string;
    fDirectory: string;
    fDefaultMakeMode: TMakeMode;
    fCommands: array[TMakeMode] of string;
    function GetCommands(Mode: TMakeMode): string;
    procedure SetCommands(Mode: TMakeMode; const AValue: string);
  public
    constructor Create;
    constructor Create(const NewName, NewDescription, NewDirectory: string;
                       const NewMakeMode: TMakeMode);
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TMakeModeDef);
  public
    property Name: string read fName write fName;
    property Description: string read fDescription write fDescription;
    property Directory: string read fDirectory write fDirectory;
    property DefaultMakeMode: TMakeMode read fDefaultMakeMode write fDefaultMakeMode;
    property Commands[Mode: TMakeMode]: string read GetCommands write SetCommands;
  end;

  { TMakeModeDefs }

  TMakeModeDefs = class(TObjectList)
  private
    fItemLCL: TMakeModeDef;
    fItemPkgReg: TMakeModeDef;
    fItemIDEIntf: TMakeModeDef;
    fItemSynEdit: TMakeModeDef;
    fItemIDE: TMakeModeDef;
    fItemExamples: TMakeModeDef;
    fItemIDEIndex: integer;
    function GetItems(Index: integer): TMakeModeDef;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TMakeModeDefs);
    // Show the permanant description part in ListBox.
    function FindName(const Name: string): TMakeModeDef;
  public
    property ItemLCL: TMakeModeDef read fItemLCL;
    property ItemPkgReg: TMakeModeDef read fItemPkgReg;
    property ItemIDEIntf: TMakeModeDef read fItemIDEIntf;
    property ItemSynEdit: TMakeModeDef read fItemSynEdit;
    property ItemIDE: TMakeModeDef read fItemIDE;
    property ItemExamples: TMakeModeDef read fItemExamples;
    property Items[Index: integer]: TMakeModeDef read GetItems; default;
  end;

  TBuildLazarusProfiles = class;

  { TBuildLazarusProfile }

  TBuildLazarusProfile = class
  private
    fOwnerCnt: TBuildLazarusProfiles;
    fName: string;
    fCleanAll: boolean;
    fTargetOS: string;
    fTargetDirectory: string;
    fTargetCPU: string;
    fTargetPlatform: TLCLPlatform;
    fWithStaticPackages: boolean;
    fUpdateRevisionInc: boolean;
    // User defined options.
    fOptions: TStringList;
    // Defines selected for this profile.
    fDefines: TStringList;
    // MakeModeSettings is Synchronised with TMakeModeDefs, same indexes.
    fMakeModes: TMakeModeSettings;

    function GetExtraOptions: string;
    function GetTargetPlatform: TLCLPlatform;
    procedure SetExtraOptions(const AValue: string);
    procedure SetTargetCPU(const AValue: string);
    procedure SetTargetOS(const AValue: string);
    procedure SetTargetPlatform(const AValue: TLCLPlatform);
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
    property CleanAll: boolean read fCleanAll write fCleanAll;
    property TargetOS: string read fTargetOS write SetTargetOS;
    property TargetDirectory: string read fTargetDirectory write fTargetDirectory;
    property TargetCPU: string read fTargetCPU write SetTargetCPU;
    property TargetPlatform: TLCLPlatform read GetTargetPlatform write SetTargetPlatform;
    property WithStaticPackages: boolean read fWithStaticPackages write fWithStaticPackages;
    property UpdateRevisionInc: boolean read fUpdateRevisionInc write fUpdateRevisionInc;
    property OptionsLines: TStringList read fOptions;
    property Defines: TStringList read fDefines;
    property MakeModes: TMakeModeSettings read fMakeModes;
  end;

  { TBuildLazarusProfiles }

  TBuildLazarusProfiles = class(TObjectList)
  private
    fGlobals: TGlobalCompilerOptions;
    fMakeModeDefs: TMakeModeDefs;
    fRestartAfterBuild: boolean;
    fConfirmBuild: boolean;
    fAllDefines: TStringList;
    fSelected: TStringList;
    fStaticAutoInstallPackages: TStringList;
    fCurrentIndex: integer;
    function GetCurrentIdeMode: TMakeMode;
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
    procedure UpdateGlobals;
  public
    property Globals: TGlobalCompilerOptions read fGlobals;
    property MakeModeDefs: TMakeModeDefs read fMakeModeDefs;
    property RestartAfterBuild: boolean read fRestartAfterBuild write fRestartAfterBuild;
    property ConfirmBuild: boolean read fConfirmBuild write fConfirmBuild;
    property AllDefines: TStringList read fAllDefines;
    property Selected: TStringList read fSelected;
    property StaticAutoInstallPackages: TStringList read fStaticAutoInstallPackages;
    property CurrentIndex: integer read fCurrentIndex write fCurrentIndex;
    property Current: TBuildLazarusProfile read GetCurrentProfile;
    property CurrentIdeMode: TMakeMode read GetCurrentIdeMode;
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

const
  MakeModeNames: array[TMakeMode] of string = ('None', 'Build', 'Clean+Build' );

var
  BuildProfileManagerForm: TBuildProfileManagerForm;


implementation

{$R *.lfm}

uses
  AddProfileDialog; // , BuildLazDialog

const
  DefaultTargetDirectory = ''; // empty will be replaced by '$(ConfDir)/bin';


function StrToMakeMode(const s: string): TMakeMode;
begin
  for Result:=Succ(mmNone) to High(TMakeMode) do
    if CompareText(s,MakeModeNames[Result])=0 then exit;
  Result:=mmNone;
end;


{ TMakeModeDef }

function TMakeModeDef.GetCommands(Mode: TMakeMode): string;
begin
  Result:=fCommands[Mode];
end;

procedure TMakeModeDef.SetCommands(Mode: TMakeMode; const AValue: string);
begin
  fCommands[Mode]:=AValue;
end;

constructor TMakeModeDef.Create;
begin
  inherited Create;
  Clear;
end;

constructor TMakeModeDef.Create(const NewName, NewDescription,
  NewDirectory: string; const NewMakeMode: TMakeMode);
begin
  inherited Create;
  Clear;
  Name:=NewName;
  Description:=NewDescription;
  Directory:=NewDirectory;
  DefaultMakeMode:=NewMakeMode;
end;

destructor TMakeModeDef.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TMakeModeDef.Clear;
begin
  FCommands[mmNone]:='';
  FCommands[mmBuild]:='all';
  FCommands[mmCleanBuild]:='clean all';
  FDirectory:='';
  FName:='';
end;

procedure TMakeModeDef.Assign(Source: TMakeModeDef);
var
  mm: TMakeMode;
begin
  if (Source=nil) or (Source=Self) then exit;
  Name:=Source.Name;
  Description:=Source.Description;
  Directory:=Source.Directory;
  DefaultMakeMode:=Source.DefaultMakeMode;
  for mm:=Low(TMakeMode) to High(TMakeMode) do
    Commands[mm]:=Source.Commands[mm];
end;

{ TMakeModeDefs }

function TMakeModeDefs.GetItems(Index: integer): TMakeModeDef;
begin
  Result:=TMakeModeDef(inherited Items[Index]);
end;

constructor TMakeModeDefs.Create;
begin
  inherited Create;
  // Hard-coded build values.
  // LCL
  FItemLCL:=TMakeModeDef.Create('LCL',lisLCL,'lcl',mmCleanBuild);
  Add(FItemLCL);
  // package registration units
  FItemPkgReg:=TMakeModeDef.Create(
    'PackageRegistration',lisPkgReg,'packager/registration', mmBuild);
  Add(FItemPkgReg);
  // IDE Interface
  FItemIDEIntf:=TMakeModeDef.Create('IDEIntf',lisIDEIntf,'ideintf',mmBuild);
  Add(FItemIDEIntf);
  // SynEdit
  FItemSynEdit:=TMakeModeDef.Create(
    'SynEdit',lisSynEdit,'components/synedit',mmBuild);
  Add(FItemSynEdit);
  // IDE
  FItemIDE:=TMakeModeDef.Create('IDE',lisIDE,'',mmBuild);
  FItemIDE.Commands[mmBuild]:='ide';
  FItemIDE.Commands[mmCleanBuild]:='cleanide ide';
  fItemIDEIndex:=Add(FItemIDE);
  // Examples
  FItemExamples:=TMakeModeDef.Create(
    'Examples',lisExamples,'examples',mmBuild);
  Add(FItemExamples);
end;

destructor TMakeModeDefs.Destroy;
begin
  inherited Destroy;        // Items are owned by ObjectList and are freed here.
end;

procedure TMakeModeDefs.Clear;
begin
  FItemLCL:=nil;
  FItemSynEdit:=nil;
  FItemPkgReg:=nil;
  FItemIDEIntf:=nil;
  FItemIDE:=nil;
  FItemExamples:=nil;
  inherited Clear;          // Items are freed here, too.
end;

procedure TMakeModeDefs.Assign(Source: TMakeModeDefs);
var
  i: Integer;
  SrcItem, NewItem: TMakeModeDef;
begin
  Clear;
  for i:=0 to Source.Count-1 do begin
    SrcItem:=Source.Items[i];
    NewItem:=TMakeModeDef.Create;
    NewItem.Assign(SrcItem);
    Add(NewItem);
  end;
  fItemLCL     :=FindName('LCL');
  fItemPkgReg  :=FindName('PkgReg');
  fItemIDEIntf :=FindName('IDEIntf');
  fItemSynEdit :=FindName('SynEdit');
  fItemIDE     :=FindName('IDE');
  fItemExamples:=FindName('Examples');
  fItemIDEIndex:=Source.fItemIDEIndex;
end;

function TMakeModeDefs.FindName(const Name: string): TMakeModeDef;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to Count-1 do
    if CompareText(Name,Items[i].Name)=0 then begin
      Result:=Items[i];
      exit;
    end;
end;

{ TBuildLazarusProfile }

constructor TBuildLazarusProfile.Create(AOwnerCnt: TBuildLazarusProfiles;
                                        AName: string);
var
  i: Integer;
begin
  inherited Create;
  fOwnerCnt:=AOwnerCnt;
  fName:=AName;
  fOptions:=TStringList.Create;
  fDefines:=TStringList.Create;
  // Set default values for MakeModes.
  SetLength(fMakeModes, fOwnerCnt.fMakeModeDefs.Count);
  for i:=0 to fOwnerCnt.fMakeModeDefs.Count-1 do
    fMakeModes[i]:=fOwnerCnt.fMakeModeDefs[i].DefaultMakeMode;
end;

destructor TBuildLazarusProfile.Destroy;
begin
  fDefines.Free;
  fOptions.Free;
  inherited Destroy;
end;

procedure TBuildLazarusProfile.Load(XMLConfig: TXMLConfig; const Path: string);
var
  i: Integer;
  LCLPlatformStr: string;
begin
  // fBuildItems and fMakeModes are synchronized, can use the same index.
  with fOwnerCnt do
    for i:=0 to fMakeModeDefs.Count-1 do
      fMakeModes[i]:=StrToMakeMode(XMLConfig.GetValue(
          Path+'Build'+fMakeModeDefs[i].Name+'/Value',
          MakeModeNames[fMakeModeDefs[i].DefaultMakeMode]));
  FCleanAll          :=XMLConfig.GetValue(Path+'CleanAll/Value',false);
  TargetOS           :=XMLConfig.GetValue(Path+'TargetOS/Value','');
  TargetCPU          :=XMLConfig.GetValue(Path+'TargetCPU/Value','');
  LCLPlatformStr     :=XMLConfig.GetValue(Path+'LCLPlatform/Value','');
  if LCLPlatformStr='' then
    fTargetPlatform  :=GetDefaultLCLWidgetType
  else
    fTargetPlatform  :=DirNameToLCLPlatform(LCLPlatformStr);
  FTargetDirectory:=AppendPathDelim(SetDirSeparators(
      XMLConfig.GetValue(Path+'TargetDirectory/Value', DefaultTargetDirectory)));
  FWithStaticPackages:=XMLConfig.GetValue(Path+'WithStaticPackages/Value',true);
  FUpdateRevisionInc :=XMLConfig.GetValue(Path+'UpdateRevisionInc/Value',true);
  LoadStringList(XMLConfig,fOptions,Path+'Options/');
  if fOptions.Count=0 then     // Support a syntax used earlier by profiles.
    fOptions.Text:=XMLConfig.GetValue(Path+'ExtraOptions/Value','');
  LoadStringList(XMLConfig,fDefines,Path+'Defines/');
end;

procedure TBuildLazarusProfile.Save(XMLConfig: TXMLConfig; const Path: string);
var
  i: Integer;
begin
  with fOwnerCnt do
    for i:=0 to fMakeModeDefs.Count-1 do begin
      XMLConfig.SetDeleteValue(Path+'Build'+fMakeModeDefs[i].Name+'/Value',
                               MakeModeNames[fMakeModes[i]],
                               MakeModeNames[fMakeModeDefs[i].DefaultMakeMode]);
  end;
  XMLConfig.SetDeleteValue(Path+'CleanAll/Value',FCleanAll,false);
  XMLConfig.SetDeleteValue(Path+'TargetOS/Value',TargetOS,'');
  XMLConfig.SetDeleteValue(Path+'TargetCPU/Value',TargetCPU,'');
  XMLConfig.SetDeleteValue(Path+'LCLPlatform/Value',
                           LCLPlatformDirNames[fTargetPlatform],
                           ''); //LCLPlatformDirNames[GetDefaultLCLWidgetType]
  XMLConfig.SetDeleteValue(Path+'TargetDirectory/Value',
                           FTargetDirectory,DefaultTargetDirectory);
  XMLConfig.SetDeleteValue(Path+'WithStaticPackages/Value',FWithStaticPackages,true);
  XMLConfig.SetDeleteValue(Path+'UpdateRevisionInc/Value',FUpdateRevisionInc,true);
  SaveStringList(XMLConfig,fOptions,Path+'Options/');
  SaveStringList(XMLConfig,fDefines,Path+'Defines/');
end;

procedure TBuildLazarusProfile.Assign(Source: TBuildLazarusProfile; ACopyName: Boolean);
var
  i: Integer;
begin
  if (Source=nil) or (Source=Self) then exit;
  if ACopyName then
    fName           :=Source.Name;
  CleanAll          :=Source.CleanAll;
  TargetOS          :=Source.TargetOS;
  TargetDirectory   :=Source.TargetDirectory;
  TargetCPU         :=Source.TargetCPU;
  TargetPlatform    :=Source.TargetPlatform;
  WithStaticPackages:=Source.WithStaticPackages;
  UpdateRevisionInc :=Source.UpdateRevisionInc;
  fOptions.Assign(Source.fOptions);
  fDefines.Assign(Source.fDefines);
  for i:=0 to Length(fMakeModes)-1 do
    fMakeModes[i]:=Source.MakeModes[i];
end;

function TBuildLazarusProfile.FPCTargetOS: string;
begin
  Result:=GetFPCTargetOS(TargetOS);
end;

function TBuildLazarusProfile.FPCTargetCPU: string;
begin
  Result:=GetFPCTargetCPU(TargetCPU);
end;

procedure TBuildLazarusProfile.SetTargetCPU(const AValue: string);
begin
  if FTargetCPU=AValue then exit;
  FTargetCPU:=AValue;
end;

procedure TBuildLazarusProfile.SetTargetOS(const AValue: string);
begin
  if fTargetOS=AValue then exit;
  fTargetOS:=AValue;
end;

function TBuildLazarusProfile.GetTargetPlatform: TLCLPlatform;
begin
  Result:=fTargetPlatform;
//  if Result=lpDefault then
//    Result:=GetDefaultLCLWidgetType;
end;

procedure TBuildLazarusProfile.SetTargetPlatform(const AValue: TLCLPlatform);
begin
  fTargetPlatform:=AValue;
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
  fGlobals:=TGlobalCompilerOptions.Create;
  fMakeModeDefs:=TMakeModeDefs.Create;
  fRestartAfterBuild:=True;
  fConfirmBuild:=True;
  fAllDefines:=TStringList.Create;
  fSelected:=TStringList.Create;
  fStaticAutoInstallPackages:=TStringList.Create;
end;

destructor TBuildLazarusProfiles.Destroy;
begin
  fMakeModeDefs.Free;
  fGlobals.Free;
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
  fGlobals.TargetCPU:=Source.fGlobals.TargetCPU;
  fGlobals.TargetOS:=Source.fGlobals.TargetOS;
  fMakeModeDefs.Assign(Source.MakeModeDefs);
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
  i: Integer;
  Profile: TBuildLazarusProfile;
  Platfrm: TLCLPlatform;
begin
  Platfrm:=GetDefaultLCLWidgetType;

  // Build LCL
  Profile:=TBuildLazarusProfile.Create(Self, lisLazBuildQBOBuildLCL);
  with Profile, fOwnerCnt do begin
    fCleanAll:=False;
    fTargetPlatform:=Platfrm;
    fWithStaticPackages:=False;
    for i:=0 to fMakeModeDefs.Count-1 do
      if fMakeModeDefs[i].Description=lisLCL then
        fMakeModes[i]:=mmBuild
      else
        fMakeModes[i]:=mmNone;
  end;
  Add(Profile);

  // Build IDE with Packages
  Profile:=TBuildLazarusProfile.Create(Self, lisLazBuildQBOBuildIDEwPackages);
  with Profile, fOwnerCnt do begin
    fCleanAll:=False;
    fTargetPlatform:=Platfrm;
    fWithStaticPackages:=True;
    for i:=0 to fMakeModeDefs.Count-1 do
      if fMakeModeDefs[i].Description=lisIDE then
        fMakeModes[i]:=mmBuild
      else
        fMakeModes[i]:=mmNone;
  end;
  // Return this one as default for fixes branch, when user clicks on build Lazarus.
  Result:=Add(Profile);

  // Build IDE without Packages
  Profile:=TBuildLazarusProfile.Create(Self, lisLazBuildQBOBuildIDEwithoutPackages);
  with Profile, fOwnerCnt do begin
    fCleanAll:=False;
    fTargetPlatform:=Platfrm;
    fWithStaticPackages:=False;
    for i:=0 to fMakeModeDefs.Count-1 do
      if fMakeModeDefs[i].Description=lisIDE then
        fMakeModes[i]:=mmBuild
      else
        fMakeModes[i]:=mmNone;
  end;
  Add(Profile);

  // Build All
  Profile:=TBuildLazarusProfile.Create(Self, lisLazBuildQBOBuildAll);
  with Profile, fOwnerCnt do begin
    fCleanAll:=False;
    fTargetPlatform:=Platfrm;
    fWithStaticPackages:=True;
    for i:=0 to fMakeModeDefs.Count-1 do
      if fMakeModeDefs[i].Description=lisExamples then
        fMakeModes[i]:=mmNone // All exept for examples.
      else
        fMakeModes[i]:=mmBuild;
  end;
  Add(Profile);

  // Clean Up + Build all
  Profile:=TBuildLazarusProfile.Create(Self, lisLazBuildQBOCleanUpBuildAll);
  with Profile, fOwnerCnt do begin
    fCleanAll:=False;
    fTargetPlatform:=Platfrm;
    fWithStaticPackages:=True;
    for i:=0 to fMakeModeDefs.Count-1 do
      fMakeModes[i]:=mmCleanBuild;
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

procedure TBuildLazarusProfiles.UpdateGlobals;
begin
  Globals.TargetOS:=Current.FPCTargetOS;
  Globals.TargetCPU:=Current.FPCTargetCPU;
end;

function TBuildLazarusProfiles.GetCurrentProfile: TBuildLazarusProfile;
begin
  Result:=Items[fCurrentIndex];
end;

function TBuildLazarusProfiles.GetCurrentIdeMode: TMakeMode;
begin
  Result:=Current.fMakeModes[fMakeModeDefs.fItemIDEIndex]
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
  // Update ProfsToManage collection.
  fProfsToManage.Move(i,i-1);
  // Update ListBox
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
  // Update ProfsToManage collection.
  fProfsToManage.Move(i,i+1);
  // Update ListBox
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
  ShowContextHelpForIDE(Self);
end;

end.

