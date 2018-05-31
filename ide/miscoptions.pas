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
  Classes, SysUtils, LCLProc, BuildProfileManager, CodeToolsStructs, TextTools,
  LazFileUtils, Laz2_XMLCfg, LazFileCache, LazConf, IDEProcs;

type
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
    procedure SetModified(AValue: boolean);
    procedure SetRename(AValue: boolean);
    procedure SetRenameShowResult(AValue: boolean);
    procedure SetRenameTo(AValue: string);
    procedure SetScope(AValue: TFindRenameScope);
    procedure SetSearchInComments(AValue: boolean);
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

function SortDirectionNameToType(const s: string): TSortDirection;
function SortDomainNameToType(const s: string): TSortDomain;
function ResourcestringInsertPolicyNameToType(
  const s: string): TResourcestringInsertPolicy;
function FindRenameScopeNameToScope(const s: string): TFindRenameScope;


implementation


const
  MiscOptsFilename = 'miscellaneousoptions.xml';
  MiscOptsVersion = 3;

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

function ResourcestringInsertPolicyNameToType(
  const s: string): TResourcestringInsertPolicy;
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
  XMLConfig.SetDeleteValue(Path+'RenameShowResult/Value',RenameShowResult,false);
  XMLConfig.SetDeleteValue(Path+'Scope/Value',FindRenameScopeNames[Scope],
                            FindRenameScopeNames[frAllOpenProjectsAndPackages]);
  SaveStringList(XMLConfig,ExtraFiles,Path+'ExtraFiles/');
  Modified:=false;
end;

end.


