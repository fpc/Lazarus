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

  Author: Mattias Gaertner

  Abstract:
    Miscellaneous options of the lazarus IDE.
}
unit MiscOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  // LazUtils
  LazUtilities, LazFileUtils, LazStringUtils, Laz2_XMLCfg, LazFileCache,
  LazLoggerBase, LazVersion,
  // CodeTools
  CodeToolsStructs, DefineTemplates,
  // IdeConfig
  LazConf, IDEProcs, TransferMacros, RecentListProcs, EnvironmentOpts,
  IdeXmlConfigProcs, IdeConfStrConsts;

type
  TIdeBuildMode = (
    bmBuild,
    bmCleanBuild, // obsolete since 2.3.0
    bmCleanAllBuild
  );

  TBuildLazarusProfiles = class;

  { TBuildLazarusProfile }

  TBuildLazarusProfile = class
  private
    FCleanOnce: boolean;
    fOwner: TBuildLazarusProfiles;
    fName: string;
    FSubtarget: string;
    fTargetOS: string;
    fTargetDirectory: string;
    fTargetCPU: string;
    fTargetPlatform: TLCLPlatform;
    fIdeBuildMode: TIdeBuildMode;
    fUpdateRevisionInc: boolean;
    fOptions: TStringList;      // User defined options.
    fDefines: TStringList;      // Defines selected for this profile.
    fTargetDirHistory: TStringList; // History
    function GetExtraOptions: string;
    function GetTranslatedName: string;
    procedure SetExtraOptions(const AValue: string);
  public
    constructor Create(AOwnerCnt: TBuildLazarusProfiles; AName: string);
    destructor Destroy; override;
    procedure Assign(Source: TBuildLazarusProfile; ACopyName: Boolean=True);
    procedure Load(XMLConfig: TXMLConfig; const Path: string);
    procedure Save(XMLConfig: TXMLConfig; const Path: string);
    function FPCTargetOS: string;
    function FPCTargetCPU: string;
    function GetParsedTargetDirectory(Macros: TTransferMacroList): string;
  public
    property Name: string read fName write fName;
    property TranslatedName: string read GetTranslatedName;
    property ExtraOptions: string read GetExtraOptions write SetExtraOptions;
    property TargetOS: string read fTargetOS write fTargetOS;
    property TargetDirectory: string read fTargetDirectory write fTargetDirectory;
    property TargetDirHistory: TStringList read fTargetDirHistory;
    property TargetCPU: string read fTargetCPU write fTargetCPU;
    property Subtarget: string read FSubtarget write FSubtarget;
    property TargetPlatform: TLCLPlatform read fTargetPlatform write fTargetPlatform;
    property IdeBuildMode: TIdeBuildMode read fIdeBuildMode write fIdeBuildMode;
    property CleanOnce: boolean read FCleanOnce write FCleanOnce;
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
    fTranslatedProfileNames: array[0..3] of string;
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TBuildLazarusProfiles);
    function IndexByName(AName: string): integer;
    function CreateDefaults: integer;
    procedure Load(XMLConfig: TXMLConfig; const Path: string; const FileVersion: integer);
    procedure Save(XMLConfig: TXMLConfig; const Path: string);
    procedure Move(CurIndex, NewIndex: Integer); // Replaces TList.Move
    function Default2TranslatedProfile(aName: string): string;
    //function Translated2DefaultProfile(aName: string): string;
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


  { TFindRenameIdentifierOptions }

  TFindRenameScope = (
    frCurrentUnit,
    frOwnerProjectPackage, // the project/package the current unit beongs to
    frProject,
    frAllOpenProjectsAndPackages
    );

  TFindRenameIdentifierOptions = class
  private
    FChangeStamp: integer;
    FExtraFiles: TStrings;
    FIdentifierFilename: string;
    FIdentifierPosition: TPoint;
    FOverrides: boolean;
    FIncludeLFMs: boolean;
    FRename: boolean;
    FRenameShowResult: boolean;
    FRenameTo: string;
    FScope: TFindRenameScope;
    FSearchInComments: boolean;
    fSavedStamp: integer;
    function GetModified: boolean;
    procedure SetExtraFiles(AValue: TStrings);
    procedure SetIdentifierFilename(AValue: string);
    procedure SetIdentifierPosition(AValue: TPoint);
    procedure SetOverrides(const AValue: boolean);
    procedure SetModified(AValue: boolean);
    procedure SetRename(AValue: boolean);
    procedure SetRenameShowResult(AValue: boolean);
    procedure SetRenameTo(AValue: string);
    procedure SetScope(AValue: TFindRenameScope);
    procedure SetSearchInComments(AValue: boolean);
    procedure SetIncludingLFMs(AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    property ChangeStamp: integer read FChangeStamp;
    procedure IncreaseChangeStamp; inline;
    property Modified: boolean read GetModified write SetModified;
    property IdentifierFilename: string read FIdentifierFilename write SetIdentifierFilename;
    property IdentifierPosition: TPoint read FIdentifierPosition write SetIdentifierPosition;
    property Rename: boolean read FRename write SetRename;
    property RenameTo: string read FRenameTo write SetRenameTo;
    property SearchInComments: boolean read FSearchInComments write SetSearchInComments;
    property Overrides: boolean read FOverrides write SetOverrides;
    property IncludeLFMs: boolean read FIncludeLFMs write SetIncludingLFMs;
    property RenameShowResult: boolean read FRenameShowResult write SetRenameShowResult;
    property Scope: TFindRenameScope read FScope write SetScope;
    property ExtraFiles: TStrings read FExtraFiles write SetExtraFiles;
  end;
  
  
  { TMiscellaneousOptions }

  TMiscellaneousOptions = class
  private
    fBuildLazProfiles: TBuildLazarusProfiles;
    FChangeStamp: integer;
    FExtractProcName: string;
    fFilename: string;
    FFindRenameIdentifierOptions: TFindRenameIdentifierOptions;
    FMakeResourceStringInsertPolicy: TResourcestringInsertPolicy;
    FShowCompOptFullFilenames: boolean;
    FShowCompOptMultiLine: boolean;
    FSortSelDirection: TSortDirection;
    FSortSelDomain: TSortDomain;
    fSavedStamp: integer;
    function GetBuildLazOpts: TBuildLazarusProfile;
    function GetFilename: string;
    function GetModified: boolean;
    procedure SetExtractProcName(AValue: string);
    procedure SetMakeResourceStringInsertPolicy(
      AValue: TResourcestringInsertPolicy);
    procedure SetModified(AValue: boolean);
    procedure SetShowCompOptFullFilenames(AValue: boolean);
    procedure SetShowCompOptMultiLine(const AValue: boolean);
    procedure SetSortSelDirection(AValue: TSortDirection);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    property Filename: string read GetFilename;
    property ChangeStamp: integer read FChangeStamp;
    procedure IncreaseChangeStamp; inline;
    property Modified: boolean read GetModified write SetModified;

    property BuildLazProfiles: TBuildLazarusProfiles read fBuildLazProfiles;
    property BuildLazOpts: TBuildLazarusProfile read GetBuildLazOpts;
    property ExtractProcName: string read FExtractProcName write SetExtractProcName;
    property SortSelDirection: TSortDirection read FSortSelDirection
                                              write SetSortSelDirection;
    property SortSelDomain: TSortDomain read FSortSelDomain write FSortSelDomain;
    property MakeResourceStringInsertPolicy: TResourcestringInsertPolicy
                                          read FMakeResourceStringInsertPolicy
                                          write SetMakeResourceStringInsertPolicy;
    property FindRenameIdentifierOptions: TFindRenameIdentifierOptions
                                              read FFindRenameIdentifierOptions;
    property ShowCompOptFullFilenames: boolean read FShowCompOptFullFilenames
                                              write SetShowCompOptFullFilenames;
    property ShowCompOptMultiLine: boolean read FShowCompOptMultiLine
                                              write SetShowCompOptMultiLine;
  end;

const
  SortDirectionNames: array[TSortDirection] of string = (
    'Ascending', 'Descending');
  SortDomainNames: array[TSortDomain] of string = (
    'Words', 'Lines', 'Paragraphs');
  ResourcestringInsertPolicyNames: array[TResourcestringInsertPolicy] of string
    = ('None', 'Append', 'Alphabetically', 'Context');
  FindRenameScopeNames: array[TFindRenameScope] of string = (
    'CurrentUnit', 'Project', 'OwnerProjectPackage',
    'AllOpenProjectsAndPackages'
    );

var MiscellaneousOptions: TMiscellaneousOptions = nil;

//function SortDirectionNameToType(const s: string): TSortDirection;
//function SortDomainNameToType(const s: string): TSortDomain;
//function ResourcestringInsertPolicyNameToType(const s: string): TResourcestringInsertPolicy;
//function FindRenameScopeNameToScope(const s: string): TFindRenameScope;


implementation

const
  DefaultTargetDirectory = ''; // empty will be replaced by '$(ConfDir)/bin';
  MiscOptsFilename = 'miscellaneousoptions.xml';
  MiscOptsVersion = 4;

  // Same names as in resourcestrings but not translated.
  // Works together with TBuildLazarusProfiles.fTranslatedProfileNames.
  DefaultProfileNames: array[0..3] of string = (
    'Normal IDE',           // Matches lisLazBuildNormalIDE
    'Debug IDE',            // Matches lisLazBuildDebugIDE
    'Optimized IDE',        // Matches lisLazBuildOptimizedIDE
    'Clean Up + Build all'  // Matches lisLazCleanUpBuildAll
  );

function IdeBuildModeToStr(BuildMode: TIdeBuildMode): string;
begin
  Result:='';
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

function SortDirectionNameToType(const s: string): TSortDirection;
begin
  for Result:=Low(TSortDirection) to High(TSortDirection) do
    if CompareText(SortDirectionNames[Result],s)=0 then exit;
  Result:=sdAscending;
end;

function SortDomainNameToType(const s: string): TSortDomain;
begin
  for Result:=Low(TSortDomain) to High(TSortDomain) do
    if CompareText(SortDomainNames[Result],s)=0 then exit;
  Result:=sdLines;
end;

function ResourcestringInsertPolicyNameToType(const s: string): TResourcestringInsertPolicy;
begin
  for Result:=Low(TResourcestringInsertPolicy)
  to High(TResourcestringInsertPolicy) do
    if CompareText(ResourcestringInsertPolicyNames[Result],s)=0 then exit;
  Result:=rsipAppend;
end;

function FindRenameScopeNameToScope(const s: string): TFindRenameScope;
begin
  for Result:=Low(TFindRenameScope) to High(TFindRenameScope) do
    if CompareText(FindRenameScopeNames[Result],s)=0 then exit;
  Result:=frAllOpenProjectsAndPackages;
end;

{ TBuildLazarusProfile }

constructor TBuildLazarusProfile.Create(AOwnerCnt: TBuildLazarusProfiles;
                                        AName: string);
begin
  inherited Create;
  fOwner:=AOwnerCnt;
  fName:=AName;
  fOptions:=TStringList.Create;
  fDefines:=TStringList.Create;
  fTargetDirHistory:=TStringList.Create;
end;

destructor TBuildLazarusProfile.Destroy;
begin
  fDefines.Free;
  fOptions.Free;
  fTargetDirHistory.Free;
  inherited Destroy;
end;

procedure TBuildLazarusProfile.Load(XMLConfig: TXMLConfig; const Path: string);
var
  LCLPlatformStr: string;
begin
  TargetOS      :=XMLConfig.GetValue(Path+'TargetOS/Value','');
  TargetCPU     :=XMLConfig.GetValue(Path+'TargetCPU/Value','');
  Subtarget     :=XMLConfig.GetValue(Path+'Subtarget/Value','');
  LCLPlatformStr:=XMLConfig.GetValue(Path+'LCLPlatform/Value','');
  if LCLPlatformStr='' then
    fTargetPlatform:=GetLCLWidgetType
  else
    fTargetPlatform:=DirNameToLCLPlatform(LCLPlatformStr);
  FTargetDirectory:=AppendPathDelim(SetDirSeparators(
      XMLConfig.GetValue(Path+'TargetDirectory/Value', DefaultTargetDirectory)));
  LoadRecentList(XMLConfig,fTargetDirHistory,Path+'TargetDirectory/History/',rltFile);

  IdeBuildMode:=StrToIdeBuildMode(XMLConfig.GetValue(Path+'IdeBuildMode/Value',''));
  CleanOnce:=XMLConfig.GetValue(Path+'CleanOnce/Value',false);
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
  XMLConfig.SetDeleteValue(Path+'Subtarget/Value',Subtarget,'');
  XMLConfig.SetDeleteValue(Path+'LCLPlatform/Value',
                           LCLPlatformDirNames[fTargetPlatform],
                           '');
  XMLConfig.SetDeleteValue(Path+'TargetDirectory/Value',
                           FTargetDirectory,DefaultTargetDirectory);
  SaveRecentList(XMLConfig,fTargetDirHistory,Path+'TargetDirectory/History/');

  XMLConfig.SetDeleteValue(Path+'IdeBuildMode/Value',IdeBuildModeToStr(IdeBuildMode),'');
  XMLConfig.SetDeleteValue(Path+'CleanOnce/Value',CleanOnce,false);
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
  fTargetDirHistory.Assign(Source.fTargetDirHistory);
  TargetCPU         :=Source.TargetCPU;
  Subtarget         :=Source.Subtarget;
  TargetPlatform    :=Source.TargetPlatform;
  IdeBuildMode      :=Source.IdeBuildMode;
  CleanOnce         :=Source.CleanOnce;
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

function TBuildLazarusProfile.GetParsedTargetDirectory(
  Macros: TTransferMacroList): string;
begin
  Result:=TargetDirectory;
  if Result='' then exit;
  if not Macros.SubstituteStr(Result) then begin
    DebugLn('TBuildLazarusProfile.GetParsedTargetDirectory macro aborted Options.TargetDirectory=',TargetDirectory);
    exit('');
  end;
  Result:=TrimAndExpandDirectory(Result,EnvironmentOptions.GetParsedLazarusDirectory);
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

function TBuildLazarusProfile.GetTranslatedName: string;
begin
  Result:=fOwner.Default2TranslatedProfile(fName);
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
  // Works together with DefaultProfileNames
  fTranslatedProfileNames[0]:=lisLazBuildNormalIDE;
  fTranslatedProfileNames[1]:=lisLazBuildDebugIDE;
  fTranslatedProfileNames[2]:=lisLazBuildOptimizedIDE;
  fTranslatedProfileNames[3]:=lisLazCleanUpBuildAll;
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
begin
  Result:=Count-1;
  while (Result>=0) and (AnsiCompareText(Items[Result].Name,AName)<>0) do
    dec(Result);
end;

function TBuildLazarusProfiles.CreateDefaults: integer;
// Create a set of default profiles when none are saved.
// Returns index for the default selected profile.
var
  Profile: TBuildLazarusProfile;
  Platfrm: TLCLPlatform;
begin
  Platfrm:=GetLCLWidgetType;
  // Build Normal IDE
  Profile:=TBuildLazarusProfile.Create(Self, DefaultProfileNames[0]); //lisLazBuildNormalIDE
  with Profile, fOwner do begin
    fTargetPlatform:=Platfrm;
    fIdeBuildMode:=bmBuild;
    fUpdateRevisionInc:=True;
  end;
  // Return this one as default. Needed when building packages without saved profiles.
  Result:=Add(Profile);

  // Build Debug IDE
  Profile:=TBuildLazarusProfile.Create(Self, DefaultProfileNames[1]); //lisLazBuildDebugIDE
  with Profile, fOwner do begin
    fTargetPlatform:=Platfrm;
    fIdeBuildMode:=bmBuild;
    fUpdateRevisionInc:=True;
    {$IFDEF Darwin}
    // FPC on darwin has a bug with -Cr
    fOptions.Add('-gw -gl -godwarfsets -gh -gt -Co -Ci -Sa');
    {$ELSE}
    fOptions.Add('-gw3 -gl -gh -gt -Co -Cr -Ci -Sa');
    {$ENDIF}
  end;
  Add(Profile);

  // Build Optimised IDE
  Profile:=TBuildLazarusProfile.Create(Self, DefaultProfileNames[2]); //lisLazBuildOptimizedIDE
  with Profile, fOwner do begin
    fTargetPlatform:=Platfrm;
    fIdeBuildMode:=bmBuild;
    fUpdateRevisionInc:=True;
    fOptions.Add('-O3 -g- -Xs');
  end;
  Add(Profile);

  // Clean Up + Build all
  Profile:=TBuildLazarusProfile.Create(Self, DefaultProfileNames[3]); //lisLazCleanUpBuildAll
  with Profile, fOwner do begin
    fTargetPlatform:=Platfrm;
    fIdeBuildMode:=bmCleanAllBuild;
    fUpdateRevisionInc:=True;
  end;
  Add(Profile);
end;

procedure TBuildLazarusProfiles.Load(XMLConfig: TXMLConfig; const Path: string;
                                     const FileVersion: integer);
var
  i, ProfCount, ProfInd: Integer;
  ProfPath, ProfName: string;
  Profile: TBuildLazarusProfile;
begin
  Clear;
  if FileVersion<1 then
  begin
    ProfInd:=CreateDefaults;
  end else if FileVersion=1 then
  begin
    // Older config file version.
    CreateDefaults;         // Only one profile saved, create defaults always.
    // Then create MyProfile.
    Profile:=TBuildLazarusProfile.Create(Self, 'MyProfile');
    Profile.Load(XMLConfig, Path);
    Add(Profile);
    FRestartAfterBuild:=XMLConfig.GetValue(Path+'RestartAfterBuild/Value',true);
    FConfirmBuild     :=XMLConfig.GetValue(Path+'ConfirmBuild/Value',true);
    ProfInd:=Count-1;       // Go to last MyProfile.
  end else begin
    // Latest config file version.
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
  // Load defines, selected profiles and auto install packages.
  LoadStringList(XMLConfig,fAllDefines,Path+'AllDefines/');
  LoadStringList(XMLConfig,fSelected,Path+'SelectedProfiles/');

  LoadStringList(XMLConfig,fStaticAutoInstallPackages,Path+'StaticAutoInstallPackages/');
  if FileVersion<3 then begin
    // the IDE part of synedit was split into a new package syneditdsgn
    fStaticAutoInstallPackages.Add('syneditdsgn');
  end;
  fCurrentIndex:=ProfInd;
end;

procedure TBuildLazarusProfiles.Save(XMLConfig: TXMLConfig; const Path: string);
var
  i: Integer;
  ProfPath: string;
begin
  // Save list of profiles.
  XMLConfig.SetDeleteValue(Path+'Profiles/Count',Count,0);
  for i:=0 to Count-1 do begin
    ProfPath:=Path+'Profiles/Profile'+IntToStr(i)+'/';
    XMLConfig.SetDeleteValue(ProfPath+'Name', Items[i].Name ,'');
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

function TBuildLazarusProfiles.Default2TranslatedProfile(aName: string): string;
var
  i: Integer;
begin
  for i:=Low(DefaultProfileNames) to High(DefaultProfileNames) do
    if DefaultProfileNames[i]=aName then
      exit(fTranslatedProfileNames[i]);
  Result:=aName; // No translation, not a preconfigured profile.
end;
{
function TBuildLazarusProfiles.Translated2DefaultProfile(aName: string): string;
var
  i: Integer;
begin
  for i:=Low(fTranslatedProfileNames) to High(fTranslatedProfileNames) do
    if fTranslatedProfileNames[i]=aName then
      exit(DefaultProfileNames[i]);
  Result:=aName;  // Not a preconfigured profile.
end;
}
function TBuildLazarusProfiles.GetCurrentProfile: TBuildLazarusProfile;
begin
  Result:=Items[fCurrentIndex];
end;

function TBuildLazarusProfiles.GetItems(Index: integer): TBuildLazarusProfile;
begin
  Result:=TBuildLazarusProfile(inherited Items[Index]);
end;

{ TMiscellaneousOptions }

// inline
procedure TMiscellaneousOptions.IncreaseChangeStamp;
begin
  LUIncreaseChangeStamp(fChangeStamp);
end;

constructor TMiscellaneousOptions.Create;
begin
  inherited Create;
  fSavedStamp:=LUInvalidChangeStamp;
  fBuildLazProfiles:=TBuildLazarusProfiles.Create;
  FExtractProcName:='NewProc';
  fSortSelDirection:=sdAscending;
  fSortSelDomain:=sdLines;
  fMakeResourceStringInsertPolicy:=rsipAppend;
  FFindRenameIdentifierOptions:=TFindRenameIdentifierOptions.Create;
end;

destructor TMiscellaneousOptions.Destroy;
begin
  fBuildLazProfiles.Free;
  FFindRenameIdentifierOptions.Free;
  inherited Destroy;
end;

function TMiscellaneousOptions.GetFilename: string;
var
  ConfFileName: string;
begin
  if fFilename='' then begin
    ConfFileName:=AppendPathDelim(GetPrimaryConfigPath)+MiscOptsFilename;
    CopySecondaryConfigFile(MiscOptsFilename);
    if (not FileExistsUTF8(ConfFileName)) then begin
      //DebugLn('Note: miscellaneous options file not found - using defaults');
    end;
    FFilename:=ConfFilename;
  end;
  Result:=fFilename;
end;

function TMiscellaneousOptions.GetModified: boolean;
begin
  Result:=(ChangeStamp<>fSavedStamp) or FindRenameIdentifierOptions.Modified;
end;

procedure TMiscellaneousOptions.SetExtractProcName(AValue: string);
begin
  if FExtractProcName=AValue then Exit;
  FExtractProcName:=AValue;
  IncreaseChangeStamp;
end;

procedure TMiscellaneousOptions.SetMakeResourceStringInsertPolicy(
  AValue: TResourcestringInsertPolicy);
begin
  if FMakeResourceStringInsertPolicy=AValue then Exit;
  FMakeResourceStringInsertPolicy:=AValue;
  IncreaseChangeStamp;
end;

procedure TMiscellaneousOptions.SetModified(AValue: boolean);
begin
  if AValue then
    IncreaseChangeStamp
  else begin
    fSavedStamp:=ChangeStamp;
    FindRenameIdentifierOptions.Modified:=false;
  end;
end;

procedure TMiscellaneousOptions.SetShowCompOptFullFilenames(AValue: boolean);
begin
  if FShowCompOptFullFilenames=AValue then Exit;
  FShowCompOptFullFilenames:=AValue;
  IncreaseChangeStamp;
end;

procedure TMiscellaneousOptions.SetShowCompOptMultiLine(const AValue: boolean);
begin
  if FShowCompOptMultiLine=AValue then Exit;
  FShowCompOptMultiLine:=AValue;
  IncreaseChangeStamp;
end;

procedure TMiscellaneousOptions.SetSortSelDirection(AValue: TSortDirection);
begin
  if FSortSelDirection=AValue then Exit;
  FSortSelDirection:=AValue;
  IncreaseChangeStamp;
end;

function TMiscellaneousOptions.GetBuildLazOpts: TBuildLazarusProfile;
begin
  Result:=BuildLazProfiles.Current;
end;

procedure TMiscellaneousOptions.Load;
var XMLConfig: TXMLConfig;
  FileVersion: integer;
  Path: String;
begin
  try
    XMLConfig:=TXMLConfig.Create(GetFilename);
  except
    DebugLn('ERROR: unable to open miscellaneous options "',GetFilename,'"');
    exit;
  end;
  try
    try
      Path:='MiscellaneousOptions/';
      FileVersion:=XMLConfig.GetValue(Path+'Version/Value',0);
      BuildLazProfiles.Load(XMLConfig,Path+'BuildLazarusOptions/',FileVersion);
      SortSelDirection:=SortDirectionNameToType(XMLConfig.GetValue(
           Path+'SortSelection/Direction',SortDirectionNames[sdAscending]));
      SortSelDomain:=SortDomainNameToType(XMLConfig.GetValue(
           Path+'SortSelection/Domain',SortDomainNames[sdLines]));
      MakeResourceStringInsertPolicy:=ResourcestringInsertPolicyNameToType(
           XMLConfig.GetValue(Path+'MakeResourcestringInsertPolicy/Value',
                              ResourcestringInsertPolicyNames[rsipAppend]));
      ExtractProcName:=XMLConfig.GetValue(Path+'ExtractProcName/Value','NewProc');
      FindRenameIdentifierOptions.LoadFromXMLConfig(XMLConfig,
                                                  Path+'FindRenameIdentifier/');
      ShowCompOptFullFilenames:=XMLConfig.GetValue(Path+'ShowCompOpts/Filenames/Full',false);
      ShowCompOptMultiLine:=XMLConfig.GetValue(Path+'ShowCompOpts/MultiLine',true);
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('ERROR: unable read miscellaneous options from "',GetFilename,'": ',E.Message);
    end;
  end;
  Modified:=false;
end;

procedure TMiscellaneousOptions.Save;
var XMLConfig: TXMLConfig;
  Path: String;
  XMLFilename: String;
begin
  if not Modified then exit;
  XMLFilename:=GetFilename;
  try
    XMLConfig:=TXMLConfig.CreateClean(XMLFilename);
  except
    on E: Exception do begin
      DebugLn('ERROR: unable to open miscellaneous options "',XMLFilename,'":',E.Message);
      exit;
    end;
  end;
  try
    try
      Path:='MiscellaneousOptions/';
      XMLConfig.SetValue(Path+'Version/Value',MiscOptsVersion);

      BuildLazProfiles.Save(XMLConfig,Path+'BuildLazarusOptions/');
      XMLConfig.SetDeleteValue(Path+'SortSelection/Direction',
           SortDirectionNames[SortSelDirection],
           SortDirectionNames[sdAscending]);
      XMLConfig.SetDeleteValue(Path+'SortSelection/Domain',
           SortDomainNames[SortSelDomain],SortDomainNames[sdLines]);
      XMLConfig.SetDeleteValue(Path+'MakeResourcestringInsertPolicy/Value',
           ResourcestringInsertPolicyNames[MakeResourceStringInsertPolicy],
           ResourcestringInsertPolicyNames[rsipAppend]);
      XMLConfig.SetDeleteValue(Path+'ExtractProcName/Value',ExtractProcName,
                               'NewProc');
      FindRenameIdentifierOptions.SaveToXMLConfig(XMLConfig,
                                                  Path+'FindRenameIdentifier/');
      XMLConfig.SetDeleteValue(Path+'ShowCompOpts/MultLine',ShowCompOptMultiLine,true);
      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('ERROR: unable read miscellaneous options from "',XMLFilename,'": ',E.Message);
    end;
  end;
  Modified:=false;
end;

{ TFindRenameIdentifierOptions }

// inline
procedure TFindRenameIdentifierOptions.IncreaseChangeStamp;
begin
  LUIncreaseChangeStamp(fChangeStamp);
end;

procedure TFindRenameIdentifierOptions.SetExtraFiles(AValue: TStrings);
begin
  if (FExtraFiles=AValue) or FExtraFiles.Equals(AValue) then Exit;
  FExtraFiles.Assign(AValue);
  IncreaseChangeStamp;
end;

function TFindRenameIdentifierOptions.GetModified: boolean;
begin
  Result:=fSavedStamp=ChangeStamp;
end;

procedure TFindRenameIdentifierOptions.SetIdentifierFilename(AValue: string);
begin
  if FIdentifierFilename=AValue then Exit;
  FIdentifierFilename:=AValue;
  IncreaseChangeStamp;
end;

procedure TFindRenameIdentifierOptions.SetIdentifierPosition(AValue: TPoint);
begin
  if ComparePoints(FIdentifierPosition,AValue)=0 then Exit;
  FIdentifierPosition:=AValue;
  IncreaseChangeStamp;
end;

procedure TFindRenameIdentifierOptions.SetOverrides(const AValue: boolean);
begin
  if FOverrides=AValue then Exit;
  FOverrides:=AValue;
  IncreaseChangeStamp;
end;

procedure TFindRenameIdentifierOptions.SetModified(AValue: boolean);
begin
  if AValue then
    IncreaseChangeStamp
  else
    fSavedStamp:=ChangeStamp;
end;

procedure TFindRenameIdentifierOptions.SetRename(AValue: boolean);
begin
  if FRename=AValue then Exit;
  FRename:=AValue;
  IncreaseChangeStamp;
end;

procedure TFindRenameIdentifierOptions.SetRenameShowResult(AValue: boolean);
begin
  if FRenameShowResult=AValue then Exit;
  FRenameShowResult:=AValue;
  IncreaseChangeStamp;
end;

procedure TFindRenameIdentifierOptions.SetRenameTo(AValue: string);
begin
  if FRenameTo=AValue then Exit;
  FRenameTo:=AValue;
  IncreaseChangeStamp;
end;

procedure TFindRenameIdentifierOptions.SetScope(AValue: TFindRenameScope);
begin
  if FScope=AValue then Exit;
  FScope:=AValue;
  IncreaseChangeStamp;
end;

procedure TFindRenameIdentifierOptions.SetSearchInComments(AValue: boolean);
begin
  if FSearchInComments=AValue then Exit;
  FSearchInComments:=AValue;
  IncreaseChangeStamp;
end;

procedure TFindRenameIdentifierOptions.SetIncludingLFMs(AValue: boolean);
begin
  if FIncludeLFMs=AValue then Exit;
  FIncludeLFMs:=AValue;
  IncreaseChangeStamp;
end;

constructor TFindRenameIdentifierOptions.Create;
begin
  inherited;
  fSavedStamp:=LUInvalidChangeStamp;
  fExtraFiles:=TStringList.Create;
end;

destructor TFindRenameIdentifierOptions.Destroy;
begin
  FreeAndNil(FExtraFiles);
  inherited Destroy;
end;

procedure TFindRenameIdentifierOptions.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  fIdentifierFilename:=XMLConfig.GetValue(Path+'Identifier/Filename','');
  LoadPoint(XMLConfig,Path+'Identifier/',fIdentifierPosition,Point(0,0));
  fRename:=XMLConfig.GetValue(Path+'Rename/Value',false);
  fRenameTo:=XMLConfig.GetValue(Path+'Rename/Identifier','');
  fSearchInComments:=XMLConfig.GetValue(Path+'SearchInComments/Value',true);
  FOverrides:=XMLConfig.GetValue(Path+'Overrides/Value',true);
  FIncludeLFMs:=XMLConfig.GetValue(Path+'IncludeLFMs/Value',true);
  fRenameShowResult:=XMLConfig.GetValue(Path+'RenameShowResult/Value',false);
  fScope:=FindRenameScopeNameToScope(XMLConfig.GetValue(Path+'Scope/Value',
                           FindRenameScopeNames[frAllOpenProjectsAndPackages]));
  LoadStringList(XMLConfig,fExtraFiles,Path+'ExtraFiles/');
  Modified:=false;
end;

procedure TFindRenameIdentifierOptions.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'Identifier/Filename',IdentifierFilename,'');
  SavePoint(XMLConfig,Path+'Identifier/',IdentifierPosition,Point(0,0));
  XMLConfig.SetDeleteValue(Path+'Rename/Value',Rename,false);
  XMLConfig.SetDeleteValue(Path+'Rename/Identifier',RenameTo,'');
  XMLConfig.SetDeleteValue(Path+'SearchInComments/Value',SearchInComments,true);
  XMLConfig.SetDeleteValue(Path+'Overrides/Value',Overrides,true);
  XMLConfig.SetDeleteValue(Path+'IncludeLFMs/Value',IncludeLFMs,true);
  XMLConfig.SetDeleteValue(Path+'RenameShowResult/Value',RenameShowResult,false);
  XMLConfig.SetDeleteValue(Path+'Scope/Value',FindRenameScopeNames[Scope],
                            FindRenameScopeNames[frAllOpenProjectsAndPackages]);
  SaveStringList(XMLConfig,ExtraFiles,Path+'ExtraFiles/');
  Modified:=false;
end;

end.


