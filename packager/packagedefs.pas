{  $Id$  }
{
 /***************************************************************************
                            packagedefs.pas
                            ---------------


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

  What is a package:
  A lazarus package is a collection of units and components, containing
  information how they can be compiled and how they can be used by projects or
  other packages or the IDE. In contrary to Delphi, packages are not limited
  to libraries and they can be OS independent.
  (Delphi: a package is a specially compiled library used by applications,
  the IDE or both. Delphi packages require compiler magic, which fpc is not
  capable of at the moment and of course this magic is not OS independent.)

}
unit PackageDefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_XMLCfg, IDEProcs, CompilerOptions;

type
  { TPkgVersion }

  TPkgVersion = class
  public
    Major: integer;
    Minor: integer;
    Build: integer;
    Release: integer;
    procedure Clear;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
      FileVersion: integer);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  end;
  
  
  { TPkgFile }

  TPkgFileType = (
    pftUnit,   // file is pascal unit
    pftText    // file is text (e.g. copyright or install notes)
    );
  TPkgFileTypes = set of TPkgFileType;

  TPkgFileFlag = (
    pffHasRegisterProc // file is unit and has a 'register' procedure
    );
  TPkgFileFlags = set of TPkgFileFlag;
  
  TPkgFile = class
  private
    FFilename: string;
    FFileType: TPkgFileType;
    FFlags: TPkgFileFlags;
    function GetHasRegisteredProc: boolean;
    procedure SetFilename(const AValue: string);
    procedure SetFileType(const AValue: TPkgFileType);
    procedure SetFlags(const AValue: TPkgFileFlags);
    procedure SetHasRegisteredProc(const AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
      FileVersion: integer);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  public
    property Filename: string read FFilename write SetFilename;
    property FileType: TPkgFileType read FFileType write SetFileType;
    property Flags: TPkgFileFlags read FFlags write SetFlags;
    property HasRegisteredProc: boolean
      read GetHasRegisteredProc write SetHasRegisteredProc;
  end;
  
  
  { TPkgDependency }
  
  TPkgDependencyFlag = (
    pdfMinVersion, // >= MinVersion
    pdfMaxVersion  // <= MaxVersion
    );
  TPkgDependencyFlags = set of TPkgDependencyFlag;
  
  TPkgDependency = class
  private
    FFlags: TPkgDependencyFlags;
    FMaxVersion: TPkgVersion;
    FMinVersion: TPkgVersion;
    FPackageName: string;
    procedure SetFlags(const AValue: TPkgDependencyFlags);
    procedure SetMaxVersion(const AValue: TPkgVersion);
    procedure SetMinVersion(const AValue: TPkgVersion);
    procedure SetPackageName(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
      FileVersion: integer);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  public
    property PackageName: string read FPackageName write SetPackageName;
    property Flags: TPkgDependencyFlags read FFlags write SetFlags;
    property MinVersion: TPkgVersion read FMinVersion write SetMinVersion;
    property MaxVersion: TPkgVersion read FMaxVersion write SetMaxVersion;
  end;
  
  
  { TPkgCompilerOptions }
  
  TPkgCompilerOptions = class(TBaseCompilerOptions)
  end;
  
  
  { TLazPackage }
  
  TLazPackageType = (
    lptRunTime,         // RunTime packages can't register anything in the IDE.
    lptDesignTime,      // DesignTime packages can register anything in the IDE
                        // and should not be compiled into projects.
                        // The IDE calls the 'register' procedures of each unit.
    lptRunAndDesignTime // RunAndDesignTime packages can do anything.
    );
    
  TLazPackageFlag = (
    lpfAutoIncrementVersionOnBuild, // increment version before
    lpfModified,           // package needs saving
    lpfAutoUpdate          // auto compile, if this package
                           // or any required package has been modified
    );
  TLazPackageFlags = set of TLazPackageFlag;

  TLazPackage = class
  private
    FAuthor: string;
    FConflictPkgs: TList; // TList of TPkgDependency
    FCompilerOptions: TPkgCompilerOptions;
    FDescription: string;
    FDirectory: string;
    FVersion: TPkgVersion;
    FFilename: string;
    FFiles: TList; // TList of TPkgFile
    FFlags: TLazPackageFlags;
    FIconFile: string;
    FName: string;
    FModifiedLock: integer;
    FPackageType: TLazPackageType;
    FRequiredPkgs: TList; // TList of TPkgDependency
    FTitle: string;
    FUsageOptions: TAdditionalCompilerOptions;
    function GetAutoIncrementVersionOnBuild: boolean;
    function GetAutoUpdate: boolean;
    function GetConflictPkgCount: integer;
    function GetConflictPkgs(Index: integer): TPkgDependency;
    function GetDirectory: string;
    function GetFileCount: integer;
    function GetFiles(Index: integer): TPkgFile;
    function GetModified: boolean;
    function GetRequiredPkgCount: integer;
    function GetRequiredPkgs(Index: integer): TPkgDependency;
    procedure SetAuthor(const AValue: string);
    procedure SetAutoIncrementVersionOnBuild(const AValue: boolean);
    procedure SetAutoUpdate(const AValue: boolean);
    procedure SetDescription(const AValue: string);
    procedure SetFilename(const AValue: string);
    procedure SetFlags(const AValue: TLazPackageFlags);
    procedure SetIconFile(const AValue: string);
    procedure SetModified(const AValue: boolean);
    procedure SetName(const AValue: string);
    procedure SetPackageType(const AValue: TLazPackageType);
    procedure SetTitle(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LockModified;
    procedure UnlockModified;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  public
    property Name: string read FName write SetName;
    property Title: string read FTitle write SetTitle;
    property Author: string read FAuthor write SetAuthor;
    property Description: string read FDescription write SetDescription;
    property Version: TPkgVersion read FVersion;
    property Filename: string read FFilename write SetFilename; // the .lpk filename
    property Directory: string read GetDirectory; // the path of the .lpk file
    property IconFile: string read FIconFile write SetIconFile;
    property PackageType: TLazPackageType
      read FPackageType write SetPackageType;
    property Flags: TLazPackageFlags read FFlags write SetFlags;
    property Files[Index: integer]: TPkgFile read GetFiles;
    property FileCount: integer read GetFileCount;
    property RequiredPkgs[Index: integer]: TPkgDependency read GetRequiredPkgs;
    property RequiredPkgCount: integer read GetRequiredPkgCount;
    property ConflictPkgs[Index: integer]: TPkgDependency read GetConflictPkgs;
    property ConflictPkgCount: integer read GetConflictPkgCount;
    property Modified: boolean read GetModified write SetModified;
    property CompilerOptions: TPkgCompilerOptions
      read FCompilerOptions;
    property AutoIncrementVersionOnBuild: boolean
      read GetAutoIncrementVersionOnBuild write SetAutoIncrementVersionOnBuild;
    property AutoUpdate: boolean read GetAutoUpdate write SetAutoUpdate;
    property UsageOptions: TAdditionalCompilerOptions
      read FUsageOptions;
  end;

const
  LazPkgXMLFileVersion = 1;

  PkgFileTypeNames: array[TPkgFileType] of string = (
    'pftUnit', 'pftText' );
  PkgFileTypeIdents: array[TPkgFileType] of string = (
    'Unit', 'Text' );
  PkgFileFlag: array[TPkgFileFlag] of string = (
    'pffHasRegisterProc');
  PkgDependencyFlagNames: array[TPkgDependencyFlag] of string = (
    'pdfMinVersion', 'pdfMaxVersion');
  LazPackageTypeNames: array[TLazPackageType] of string = (
    'lptRunTime', 'lptDesignTime', 'lptRunAndDesignTime');
  LazPackageTypeIdents: array[TLazPackageType] of string = (
    'RunTime', 'DesignTime', 'RunAndDesignTime');
  LazPackageFlagNames: array[TLazPackageFlag] of string = (
    'lpfAutoIncrementVersionOnBuild', 'lpfModified', 'lpfAutoUpdate');
    

function PkgFileTypeIdentToType(const s: string): TPkgFileType;
function LazPackageTypeIdentToType(const s: string): TLazPackageType;


implementation


function PkgFileTypeIdentToType(const s: string): TPkgFileType;
begin
  for Result:=Low(TPkgFileType) to High(TPkgFileType) do
    if AnsiCompareText(s,PkgFileTypeIdents[Result])=0 then exit;
  Result:=pftUnit;
end;

function LazPackageTypeIdentToType(const s: string): TLazPackageType;
begin
  for Result:=Low(TLazPackageType) to High(TLazPackageType) do
    if AnsiCompareText(s,LazPackageTypeIdents[Result])=0 then exit;
  Result:=lptRunTime;
end;

{ TPkgFile }

procedure TPkgFile.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
end;

function TPkgFile.GetHasRegisteredProc: boolean;
begin
  Result:=pffHasRegisterProc in FFlags;
end;

procedure TPkgFile.SetFileType(const AValue: TPkgFileType);
begin
  if FFileType=AValue then exit;
  FFileType:=AValue;
end;

procedure TPkgFile.SetFlags(const AValue: TPkgFileFlags);
begin
  if FFlags=AValue then exit;
  FFlags:=AValue;
end;

procedure TPkgFile.SetHasRegisteredProc(const AValue: boolean);
begin
  if HasRegisteredProc=AValue then exit;
  if AValue then
    Include(FFlags,pffHasRegisterProc)
  else
    Exclude(FFlags,pffHasRegisterProc);
end;

constructor TPkgFile.Create;
begin
  Clear;
end;

destructor TPkgFile.Destroy;
begin
  inherited Destroy;
end;

procedure TPkgFile.Clear;
begin
  FFilename:='';
  FFlags:=[];
  FFileType:=pftUnit;
end;

procedure TPkgFile.LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  FileVersion: integer);
begin
  if FileVersion=1 then ;
  Clear;
  FFilename:=XMLConfig.GetValue(Path+'Filename/Value','');
  HasRegisteredProc:=XMLConfig.GetValue(Path+'HasRegisteredProc/Value',false);
  FileType:=PkgFileTypeIdentToType(XMLConfig.GetValue(Path+'Type/Value',''));
end;

procedure TPkgFile.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'Filename/Value',Filename,'');
  XMLConfig.SetDeleteValue(Path+'HasRegisteredProc/Value',HasRegisteredProc,
                           false);
  XMLConfig.SetDeleteValue(Path+'Type/Value',PkgFileTypeIdents[FileType],
                           PkgFileTypeIdents[pftUnit]);
end;

{ TPkgDependency }

procedure TPkgDependency.SetFlags(const AValue: TPkgDependencyFlags);
begin
  if FFlags=AValue then exit;
  FFlags:=AValue;
end;

procedure TPkgDependency.SetMaxVersion(const AValue: TPkgVersion);
begin
  if FMaxVersion=AValue then exit;
  FMaxVersion:=AValue;
end;

procedure TPkgDependency.SetMinVersion(const AValue: TPkgVersion);
begin
  if FMinVersion=AValue then exit;
  FMinVersion:=AValue;
end;

procedure TPkgDependency.SetPackageName(const AValue: string);
begin
  if FPackageName=AValue then exit;
  FPackageName:=AValue;
end;

constructor TPkgDependency.Create;
begin
  MinVersion:=TPkgVersion.Create;
  MaxVersion:=TPkgVersion.Create;
  Clear;
end;

destructor TPkgDependency.Destroy;
begin
  FreeAndNil(fMinVersion);
  FreeAndNil(fMaxVersion);
  inherited Destroy;
end;

procedure TPkgDependency.Clear;
begin
  FFlags:=[];
  FMaxVersion.Clear;
  FMinVersion.Clear;
  FPackageName:='';
end;

procedure TPkgDependency.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; FileVersion: integer);
begin
  if FileVersion=1 then ;
  Clear;
  FPackageName:=XMLConfig.GetValue(Path+'PackageName/Value','');
  MaxVersion.LoadFromXMLConfig(XMLConfig,Path+'MaxVersion/',FileVersion);
  MinVersion.LoadFromXMLConfig(XMLConfig,Path+'MinVersion/',FileVersion);
  if XMLConfig.GetValue(Path+'MaxVersion/Valid',false) then
    Include(FFlags,pdfMaxVersion);
  if XMLConfig.GetValue(Path+'MinVersion/Valid',false) then
    Include(FFlags,pdfMinVersion);
end;

procedure TPkgDependency.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'PackageName/Value',PackageName,'');
  MaxVersion.SaveToXMLConfig(XMLConfig,Path+'MaxVersion/');
  MinVersion.SaveToXMLConfig(XMLConfig,Path+'MinVersion/');
  XMLConfig.SetDeleteValue(Path+'MaxVersion/Value',pdfMaxVersion in FFlags,false);
  XMLConfig.SetDeleteValue(Path+'MinVersion/Value',pdfMinVersion in FFlags,false);
end;

{ TPkgVersion }

procedure TPkgVersion.Clear;
begin
  Major:=0;
  Minor:=0;
  Build:=0;
  Release:=0;
end;

procedure TPkgVersion.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; FileVersion: integer);
begin
  if FileVersion=1 then ;
  Clear;
  Major:=XMLConfig.GetValue(Path+'Major',0);
  Minor:=XMLConfig.GetValue(Path+'Minor',0);
  Build:=XMLConfig.GetValue(Path+'Build',0);
  Release:=XMLConfig.GetValue(Path+'Release',0);
end;

procedure TPkgVersion.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string
  );
begin
  XMLConfig.SetDeleteValue(Path+'Major',Major,0);
  XMLConfig.SetDeleteValue(Path+'Minor',Minor,0);
  XMLConfig.SetDeleteValue(Path+'Build',Build,0);
  XMLConfig.SetDeleteValue(Path+'Release',Release,0);
end;

{ TLazPackage }

function TLazPackage.GetAutoIncrementVersionOnBuild: boolean;
begin
  Result:=lpfAutoIncrementVersionOnBuild in FFlags;
end;

function TLazPackage.GetAutoUpdate: boolean;
begin
  Result:=lpfAutoUpdate in FFlags;
end;

function TLazPackage.GetConflictPkgCount: integer;
begin
  Result:=FConflictPkgs.Count;
end;

function TLazPackage.GetConflictPkgs(Index: integer): TPkgDependency;
begin
  Result:=TPkgDependency(FConflictPkgs[Index]);
end;

function TLazPackage.GetDirectory: string;
begin
  Result:=FDirectory;
end;

function TLazPackage.GetFileCount: integer;
begin
  Result:=FFiles.Count;
end;

function TLazPackage.GetFiles(Index: integer): TPkgFile;
begin
  Result:=TPkgFile(FFiles[Index]);
end;

function TLazPackage.GetModified: boolean;
begin
  Result:=lpfModified in FFlags;
end;

function TLazPackage.GetRequiredPkgCount: integer;
begin
  Result:=FRequiredPkgs.Count;
end;

function TLazPackage.GetRequiredPkgs(Index: integer): TPkgDependency;
begin
  Result:=TPkgDependency(FRequiredPkgs[Index]);
end;

procedure TLazPackage.SetAuthor(const AValue: string);
begin
  if FAuthor=AValue then exit;
  FAuthor:=AValue;
  Modified:=true;
end;

procedure TLazPackage.SetAutoIncrementVersionOnBuild(const AValue: boolean);
begin
  if AutoIncrementVersionOnBuild=AValue then exit;
  if AValue then
    Include(FFlags,lpfAutoIncrementVersionOnBuild)
  else
    Exclude(FFlags,lpfAutoIncrementVersionOnBuild);
  Modified:=true;
end;

procedure TLazPackage.SetAutoUpdate(const AValue: boolean);
begin
  if AValue=AutoUpdate then exit;
  if AValue then
    Include(FFlags,lpfAutoUpdate)
  else
    Exclude(FFlags,lpfAutoUpdate);
  Modified:=true;
end;

procedure TLazPackage.SetDescription(const AValue: string);
begin
  if FDescription=AValue then exit;
  FDescription:=AValue;
  Modified:=true;
end;

procedure TLazPackage.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
  FDirectory:=ExtractFilePath(FFilename);
  Modified:=true;
end;

procedure TLazPackage.SetFlags(const AValue: TLazPackageFlags);
begin
  if FFlags=AValue then exit;
  FFlags:=AValue;
  Modified:=true;
end;

procedure TLazPackage.SetIconFile(const AValue: string);
begin
  if FIconFile=AValue then exit;
  FIconFile:=AValue;
  Modified:=true;
end;

procedure TLazPackage.SetModified(const AValue: boolean);
begin
  if FModifiedLock>0 then exit;
  if AValue then
    Include(FFlags,lpfModified)
  else
    Exclude(FFlags,lpfModified);
end;

procedure TLazPackage.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
  Modified:=true;
end;

procedure TLazPackage.SetPackageType(const AValue: TLazPackageType);
begin
  if FPackageType=AValue then exit;
  FPackageType:=AValue;
  Modified:=true;
end;

procedure TLazPackage.SetTitle(const AValue: string);
begin
  if FTitle=AValue then exit;
  FTitle:=AValue;
  Modified:=true;
end;

constructor TLazPackage.Create;
begin
  FVersion:=TPkgVersion.Create;
  FConflictPkgs:=TList.Create;
  FRequiredPkgs:=TList.Create;
  FFiles:=TList.Create;
end;

destructor TLazPackage.Destroy;
begin
  Clear;
  FreeAndNil(FFiles);
  FreeAndNil(FConflictPkgs);
  FreeAndNil(FRequiredPkgs);
  FreeAndNil(FVersion);
  inherited Destroy;
end;

procedure TLazPackage.Clear;
var
  i: Integer;
begin
  FAuthor:='';
  for i:=0 to FConflictPkgs.Count-1 do ConflictPkgs[i].Free;
  FConflictPkgs.Clear;
  FCompilerOptions.Clear;
  FDescription:='';
  FDirectory:='';
  FVersion.Clear;
  FFilename:='';
  for i:=0 to FFiles.Count-1 do Files[i].Free;
  FFiles.Clear;
  FFlags:=[lpfAutoIncrementVersionOnBuild,lpfAutoUpdate];
  FIconFile:='';
  FName:='';
  FPackageType:=lptRunTime;
  for i:=0 to FRequiredPkgs.Count-1 do RequiredPkgs[i].Free;
  FRequiredPkgs.Clear;
  FTitle:='';
  FUsageOptions.Clear;
end;

procedure TLazPackage.LockModified;
begin
  inc(FModifiedLock);
end;

procedure TLazPackage.UnlockModified;
begin
  if FModifiedLock<=0 then
    RaiseException('TLazPackage.UnlockModified');
  dec(FModifiedLock);
end;

procedure TLazPackage.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  FileVersion: integer;

  procedure LoadPkgDependencyList(const ThePath: string; List: TList);
  var
    i: Integer;
    PkgDependency: TPkgDependency;
    NewCount: Integer;
  begin
    NewCount:=XMLConfig.GetValue(ThePath+'Count',0);
    for i:=0 to NewCount-1 do begin
      PkgDependency:=TPkgDependency.Create;
      PkgDependency.LoadFromXMLConfig(XMLConfig,ThePath+'Item'+IntToStr(i)+'/',
                                      FileVersion);
      List.Add(PkgDependency);
    end;
  end;

  procedure LoadFiles(const ThePath: string; List: TList);
  var
    i: Integer;
    NewCount: Integer;
    PkgFile: TPkgFile;
  begin
    NewCount:=XMLConfig.GetValue(ThePath+'Count',0);
    for i:=0 to NewCount-1 do begin
      PkgFile:=TPkgFile.Create;
      PkgFile.LoadFromXMLConfig(XMLConfig,ThePath+'Item'+IntToStr(i)+'/',
                                FileVersion);
      List.Add(PkgFile);
    end;
  end;
  
  procedure LoadFlags(const ThePath: string);
  begin
    if XMLConfig.GetValue(ThePath+'AutoIncrementVersionOnBuild/Value',true) then
      Include(FFlags,lpfAutoIncrementVersionOnBuild)
    else
      Exclude(FFlags,lpfAutoIncrementVersionOnBuild);
    if XMLConfig.GetValue(ThePath+'AutoUpdate/Value',true) then
      Include(FFlags,lpfAutoUpdate)
    else
      Exclude(FFlags,lpfAutoUpdate);
  end;

begin
  FileVersion:=XMLConfig.GetValue(Path+'Version',0);
  if FileVersion=1 then ;
  Clear;
  LockModified;
  FAuthor:=XMLConfig.GetValue(Path+'Author/Value','');
  LoadPkgDependencyList(Path+'ConflictPkgs/',FConflictPkgs);
  FCompilerOptions.LoadFromXMLConfig(XMLConfig,Path+'CompilerOptions/');
  FDescription:=XMLConfig.GetValue(Path+'Description','');
  FVersion.LoadFromXMLConfig(XMLConfig,Path+'Version/',FileVersion);
  LoadFiles(Path+'Files/',FFiles);
  LoadFlags(Path);
  FIconFile:=XMLConfig.GetValue(Path+'IconFile/Value','');
  FName:=XMLConfig.GetValue(Path+'Name/Value','');
  FPackageType:=LazPackageTypeIdentToType(XMLConfig.GetValue(Path+'Type/Value',
                                          LazPackageTypeIdents[lptRunTime]));
  LoadPkgDependencyList(Path+'RequiredPkgs/',FRequiredPkgs);
  FTitle:=XMLConfig.GetValue(Path+'Title/Value','');
  FUsageOptions.LoadFromXMLConfig(XMLConfig,Path+'UsageOptions/');
  UnlockModified;
end;

procedure TLazPackage.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string
  );
  
  procedure SavePkgDependencyList(const ThePath: string; List: TList);
  var
    i: Integer;
    PkgDependency: TPkgDependency;
  begin
    XMLConfig.SetDeleteValue(Path+'Count',List.Count,0);
    for i:=0 to List.Count-1 do begin
      PkgDependency:=TPkgDependency(List[i]);
      PkgDependency.SaveToXMLConfig(XMLConfig,ThePath+'Item'+IntToStr(i)+'/');
    end;
  end;

  procedure SaveFiles(const ThePath: string; List: TList);
  var
    i: Integer;
    PkgFile: TPkgFile;
  begin
    XMLConfig.SetDeleteValue(Path+'Count',List.Count,0);
    for i:=0 to List.Count-1 do begin
      PkgFile:=TPkgFile(List[i]);
      PkgFile.SaveToXMLConfig(XMLConfig,ThePath+'Item'+IntToStr(i)+'/');
    end;
  end;
  
  procedure SaveFlags(const ThePath: string);
  begin
    XMLConfig.SetDeleteValue(ThePath+'AutoIncrementVersionOnBuild/Value',
      AutoIncrementVersionOnBuild,true);
    XMLConfig.SetDeleteValue(ThePath+'AutoUpdate/Value',AutoUpdate,true);
  end;

begin
  XMLConfig.SetDeleteValue(Path+'Author/Value',FAuthor,'');
  SavePkgDependencyList(Path+'ConflictPkgs/',FConflictPkgs);
  FCompilerOptions.SaveToXMLConfig(XMLConfig,Path+'CompilerOptions/');
  XMLConfig.SetDeleteValue(Path+'Description',FDescription,'');
  FVersion.SaveToXMLConfig(XMLConfig,Path+'Version/');
  SaveFiles(Path+'Files/',FFiles);
  SaveFlags(Path);
  XMLConfig.SetDeleteValue(Path+'IconFile/Value',FIconFile,'');
  XMLConfig.SetDeleteValue(Path+'Name/Value',FName,'');
  XMLConfig.SetDeleteValue(Path+'Type/Value',LazPackageTypeIdents[FPackageType],
                           LazPackageTypeIdents[lptRunTime]);
  SavePkgDependencyList(Path+'RequiredPkgs/',FRequiredPkgs);
  XMLConfig.SetDeleteValue(Path+'Title/Value',FTitle,'');
  FUsageOptions.SaveToXMLConfig(XMLConfig,Path+'UsageOptions/');
end;

end.

