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

  Author: Mattias Gaertner

  Abstract:
    Miscellaneous options of the lazarus IDE.
}
unit MiscOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, BuildLazDialog, BuildProfileManager,
  CodeToolsStructs, TextTools, FileUtil, Laz_XMLCfg, LazConf, IDEProcs;

type
  { TFindRenameIdentifierOptions }

  TFindRenameScope = (
    frCurrentUnit,
    frOwnerProjectPackage, // the project/package the current unit beongs to
    frProject,
    frAllOpenProjectsAndPackages
    );

  TFindRenameIdentifierOptions = class
  public
    IdentifierFilename: string;
    IdentifierPosition: TPoint;
    Rename: boolean;
    RenameTo: string;
    SearchInComments: boolean;
    Scope: TFindRenameScope;
    ExtraFiles: TStrings;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  end;
  
  
  { TMiscellaneousOptions }

  TMiscellaneousOptions = class
  private
    fBuildLazProfiles: TBuildLazarusProfiles;
    FExtractProcName: string;
    fFilename: string;
    FFindRenameIdentifierOptions: TFindRenameIdentifierOptions;
    FMakeResourceStringInsertPolicy: TResourcestringInsertPolicy;
    FSortSelDirection: TSortDirection;
    FSortSelDomain: TSortDomain;
    function GetBuildLazOpts: TBuildLazarusProfile;
    function GetFilename: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    property Filename: string read GetFilename;

    property BuildLazProfiles: TBuildLazarusProfiles read fBuildLazProfiles;
    property BuildLazOpts: TBuildLazarusProfile read GetBuildLazOpts;
    property ExtractProcName: string read FExtractProcName write FExtractProcName;
    property SortSelDirection: TSortDirection read FSortSelDirection
                                              write FSortSelDirection;
    property SortSelDomain: TSortDomain read FSortSelDomain write FSortSelDomain;
    property MakeResourceStringInsertPolicy: TResourcestringInsertPolicy
                                          read FMakeResourceStringInsertPolicy
                                          write FMakeResourceStringInsertPolicy;
    property FindRenameIdentifierOptions: TFindRenameIdentifierOptions
                                              read FFindRenameIdentifierOptions;
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
  MiscOptsVersion = 2;

function SortDirectionNameToType(const s: string): TSortDirection;
begin
  for Result:=Low(TSortDirection) to High(TSortDirection) do
    if AnsiCompareText(SortDirectionNames[Result],s)=0 then exit;
  Result:=sdAscending;
end;

function SortDomainNameToType(const s: string): TSortDomain;
begin
  for Result:=Low(TSortDomain) to High(TSortDomain) do
    if AnsiCompareText(SortDomainNames[Result],s)=0 then exit;
  Result:=sdLines;
end;

function ResourcestringInsertPolicyNameToType(
  const s: string): TResourcestringInsertPolicy;
begin
  for Result:=Low(TResourcestringInsertPolicy)
  to High(TResourcestringInsertPolicy) do
    if AnsiCompareText(ResourcestringInsertPolicyNames[Result],s)=0 then exit;
  Result:=rsipAppend;
end;

function FindRenameScopeNameToScope(const s: string): TFindRenameScope;
begin
  for Result:=Low(TFindRenameScope) to High(TFindRenameScope) do
    if AnsiCompareText(FindRenameScopeNames[Result],s)=0 then exit;
  Result:=frAllOpenProjectsAndPackages;
end;

{ TMiscellaneousOptions }

constructor TMiscellaneousOptions.Create;
begin
  inherited Create;
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
    ConfFileName:=SetDirSeparators(GetPrimaryConfigPath+'/'+MiscOptsFilename);
    CopySecondaryConfigFile(MiscOptsFilename);
    if (not FileExistsUTF8(ConfFileName)) then begin
      DebugLn('NOTE: miscellaneous options file not found - using defaults');
    end;
    FFilename:=ConfFilename;
  end;
  Result:=fFilename;
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
//      if (FileVersion<MiscOptsVersion) and (FileVersion<>0) then
//        DebugLn('NOTE: converting old miscellaneous options ...');
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
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('ERROR: unable read miscellaneous options from "',GetFilename,'": ',E.Message);
    end;
  end;
end;

procedure TMiscellaneousOptions.Save;
var XMLConfig: TXMLConfig;
  Path: String;
  XMLFilename: String;
begin
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
      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('ERROR: unable read miscellaneous options from "',XMLFilename,'": ',E.Message);
    end;
  end;
end;

{ TFindRenameIdentifierOptions }

constructor TFindRenameIdentifierOptions.Create;
begin
  ExtraFiles:=TStringList.Create;
end;

destructor TFindRenameIdentifierOptions.Destroy;
begin
  ExtraFiles.Free;
  inherited Destroy;
end;

procedure TFindRenameIdentifierOptions.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  IdentifierFilename:=XMLConfig.GetValue(Path+'Identifier/Filename','');
  LoadPoint(XMLConfig,Path+'Identifier/',IdentifierPosition,Point(0,0));
  Rename:=XMLConfig.GetValue(Path+'Rename/Value',false);
  RenameTo:=XMLConfig.GetValue(Path+'Rename/Identifier','');
  SearchInComments:=XMLConfig.GetValue(Path+'SearchInComments/Value',true);
  Scope:=FindRenameScopeNameToScope(XMLConfig.GetValue(Path+'Scope/Value',
                           FindRenameScopeNames[frAllOpenProjectsAndPackages]));
  LoadStringList(XMLConfig,ExtraFiles,Path+'ExtraFiles/');
end;

procedure TFindRenameIdentifierOptions.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'Identifier/Filename',IdentifierFilename,'');
  SavePoint(XMLConfig,Path+'Identifier/',IdentifierPosition,Point(0,0));
  XMLConfig.SetDeleteValue(Path+'Rename/Value',Rename,false);
  XMLConfig.SetDeleteValue(Path+'Rename/Identifier',RenameTo,'');
  XMLConfig.SetDeleteValue(Path+'SearchInComments/Value',SearchInComments,true);
  XMLConfig.SetDeleteValue(Path+'Scope/Value',FindRenameScopeNames[Scope],
                            FindRenameScopeNames[frAllOpenProjectsAndPackages]);
  SaveStringList(XMLConfig,ExtraFiles,Path+'ExtraFiles/');
end;

end.


