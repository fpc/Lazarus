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
  Classes, SysUtils, LCLProc, Laz_XMLCfg, CompilerOptions, Forms, FileCtrl,
  IDEProcs;

type
  TLazPackage = class;
  TPkgFile = class;
  TBasePackageEditor = class;
  TPkgDependency = class;


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
    function Compare(Version2: TPkgVersion): integer;
    procedure Assign(Source: TPkgVersion);
    function AsString: string;
  end;
  
  
  { TPkgComponent }
  
  TPkgComponent = class
  private
    FComponentClass: TComponentClass;
    FDefaultCompClassName: string;
    FPkgFile: TPkgFile;
    procedure SetDefaultCompClassName(const AValue: string);
  public
    constructor Create(ThePkgFile: TPkgFile; TheComponentClass: TComponentClass;
      const TheDefCompClassName: string);
    destructor Destroy; override;
    function GetComponentClassName: string;
    procedure ConsistencyCheck;
  public
    property ComponentClass: TComponentClass read FComponentClass;
    property DefaultCompClassName: string read FDefaultCompClassName
                                          write SetDefaultCompClassName;
    property PkgFile: TPkgFile read FPkgFile;
  end;
  
  
  { TPkgFile }

  TPkgFileType = (
    pftUnit,   // file is pascal unit
    pftText,   // file is text (e.g. copyright or install notes)
    pftBinary  // file is something else
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
    FPackage: TLazPackage;
    FUnitName: string;
    function GetHasRegisteredProc: boolean;
    procedure SetFilename(const AValue: string);
    procedure SetFileType(const AValue: TPkgFileType);
    procedure SetFlags(const AValue: TPkgFileFlags);
    procedure SetHasRegisteredProc(const AValue: boolean);
    procedure UpdateUnitName;
  public
    constructor Create(ThePackage: TLazPackage);
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
      FileVersion: integer);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure ConsistencyCheck;
    function IsVirtual: boolean;
    function UnitName: string;
  public
    property Filename: string read FFilename write SetFilename;
    property FileType: TPkgFileType read FFileType write SetFileType;
    property Flags: TPkgFileFlags read FFlags write SetFlags;
    property HasRegisteredProc: boolean
      read GetHasRegisteredProc write SetHasRegisteredProc;
    property LazPackage: TLazPackage read FPackage;
  end;
  
  
  { TPkgDependency }
  
  TPkgDependencyFlag = (
    pdfMinVersion, // >= MinVersion
    pdfMaxVersion, // <= MaxVersion
    pdfActive
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
    function MakeSense: boolean;
    function IsCompatible(const Version: TPkgVersion): boolean;
    function IsCompatible(const PkgName: string;
      const Version: TPkgVersion): boolean;
    function Compare(Dependency2: TPkgDependency): integer;
    procedure ConsistencyCheck;
    function IsCompatible(Pkg: TLazPackage): boolean;
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
    lpfAutoUpdate,         // auto compile, if this package
                           // or any required package has been modified
    lpfOpenInIDE           // open packageeditor in IDE
    );
  TLazPackageFlags = set of TLazPackageFlag;
  
  TIterateComponentClassesEvent =
    procedure(PkgComponent: TPkgComponent) of object;

  TLazPackage = class
  private
    FAuthor: string;
    FAutoLoad: boolean;
    FComponentCount: integer;
    FConflictPkgs: TList; // TList of TPkgDependency
    FCompilerOptions: TPkgCompilerOptions;
    FComponents: TList; // TList of TPkgComponent
    FDependingPkgs: TList; // TList of TLazPackage
    FDescription: string;
    FDirectory: string;
    FEditorRect: TRect;
    FInstalled: boolean;
    FLoaded: boolean;
    FPackageEditor: TBasePackageEditor;
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
    FUsedPkgs: TList; // list of TLazPackage
    function GetAutoIncrementVersionOnBuild: boolean;
    function GetAutoUpdate: boolean;
    function GetComponents(Index: integer): TPkgComponent;
    function GetConflictPkgCount: integer;
    function GetConflictPkgs(Index: integer): TPkgDependency;
    function GetDependingPkgCount: integer;
    function GetDependingPkgs(Index: integer): TLazPackage;
    function GetDirectory: string;
    function GetFileCount: integer;
    function GetFiles(Index: integer): TPkgFile;
    function GetModified: boolean;
    function GetOpen: boolean;
    function GetRequiredPkgCount: integer;
    function GetRequiredPkgs(Index: integer): TPkgDependency;
    function GetUsedPkgCount: integer;
    function GetUsedPkgs(Index: integer): TLazPackage;
    procedure SetAuthor(const AValue: string);
    procedure SetAutoIncrementVersionOnBuild(const AValue: boolean);
    procedure SetAutoLoad(const AValue: boolean);
    procedure SetAutoUpdate(const AValue: boolean);
    procedure SetDescription(const AValue: string);
    procedure SetEditorRect(const AValue: TRect);
    procedure SetFilename(const AValue: string);
    procedure SetFlags(const AValue: TLazPackageFlags);
    procedure SetIconFile(const AValue: string);
    procedure SetInstalled(const AValue: boolean);
    procedure SetLoaded(const AValue: boolean);
    procedure SetModified(const AValue: boolean);
    procedure SetName(const AValue: string);
    procedure SetOpen(const AValue: boolean);
    procedure SetPackageEditor(const AValue: TBasePackageEditor);
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
    function IsVirtual: boolean;
    procedure CheckInnerDependencies;
    function Compare(Package2: TLazPackage): integer;
    procedure ShortenFilename(var ExpandedFilename: string);
    procedure LongenFilename(var ExpandedFilename: string);
    procedure IterateComponentClasses(Event: TIterateComponentClassesEvent;
                                      WithRequiredPackages: boolean);
    procedure ConsistencyCheck;
    function IndexOfPkgComponent(PkgComponent: TPkgComponent): integer;
    function FindUnit(const TheUnitName: string): TPkgFile;
    function NameAndVersion: string;
  public
    property Author: string read FAuthor write SetAuthor;
    property AutoIncrementVersionOnBuild: boolean
      read GetAutoIncrementVersionOnBuild write SetAutoIncrementVersionOnBuild;
    property AutoLoad: boolean read FAutoLoad write SetAutoLoad; { dynamic: load package on next IDE start
                                  static: compile package into IDE }
    property AutoUpdate: boolean read GetAutoUpdate write SetAutoUpdate;
    property CompilerOptions: TPkgCompilerOptions
      read FCompilerOptions;
    property ComponentCount: integer read FComponentCount;
    property Components[Index: integer]: TPkgComponent read GetComponents;
    property ConflictPkgCount: integer read GetConflictPkgCount;
    property ConflictPkgs[Index: integer]: TPkgDependency read GetConflictPkgs;
    property DependingPkgCount: integer read GetDependingPkgCount;
    property DependingPkgs[Index: integer]: TLazPackage read GetDependingPkgs;
    property Description: string read FDescription write SetDescription;
    property Directory: string read GetDirectory; // the path of the .lpk file
    property FileCount: integer read GetFileCount;
    property Filename: string read FFilename write SetFilename; // the .lpk filename
    property Files[Index: integer]: TPkgFile read GetFiles;
    property Flags: TLazPackageFlags read FFlags write SetFlags;
    property IconFile: string read FIconFile write SetIconFile;
    property Installed: boolean read FInstalled write SetInstalled; // is in component palette (can be set by projects)
    property Loaded: boolean read FLoaded write SetLoaded; // package is available for runtime installation
    property Modified: boolean read GetModified write SetModified;
    property Name: string read FName write SetName;
    property PackageType: TLazPackageType
      read FPackageType write SetPackageType;
    property RequiredPkgCount: integer read GetRequiredPkgCount;
    property RequiredPkgs[Index: integer]: TPkgDependency read GetRequiredPkgs;
    property UsedPkgCount: integer read GetUsedPkgCount;
    property UsedPkgs[Index: integer]: TLazPackage read GetUsedPkgs;
    property Title: string read FTitle write SetTitle;
    property UsageOptions: TAdditionalCompilerOptions
      read FUsageOptions;
    property Version: TPkgVersion read FVersion;
    property Open: boolean read GetOpen write SetOpen; // a packageeditor is open in the IDE
    property Editor: TBasePackageEditor read FPackageEditor write SetPackageEditor;
    property EditorRect: TRect read FEditorRect write SetEditorRect;
  end;
  
  
  { TBasePackageEditor }
  
  TBasePackageEditor = class(TForm)
  public
  end;
  

const
  LazPkgXMLFileVersion = 1;

  PkgFileTypeNames: array[TPkgFileType] of string = (
    'pftUnit', 'pftText', 'pftBinary');
  PkgFileTypeIdents: array[TPkgFileType] of string = (
    'Unit', 'Text', 'Binary');
  PkgFileFlag: array[TPkgFileFlag] of string = (
    'pffHasRegisterProc');
  PkgDependencyFlagNames: array[TPkgDependencyFlag] of string = (
    'pdfMinVersion', 'pdfMaxVersion', 'pdfActive');
  LazPackageTypeNames: array[TLazPackageType] of string = (
    'lptRunTime', 'lptDesignTime', 'lptRunAndDesignTime');
  LazPackageTypeIdents: array[TLazPackageType] of string = (
    'RunTime', 'DesignTime', 'RunAndDesignTime');
  LazPackageFlagNames: array[TLazPackageFlag] of string = (
    'lpfAutoIncrementVersionOnBuild', 'lpfModified', 'lpfAutoUpdate',
    'lpfOpenInIDE');
    

function PkgFileTypeIdentToType(const s: string): TPkgFileType;
function LazPackageTypeIdentToType(const s: string): TLazPackageType;
procedure SortDependencyList(Dependencies: TList);
function CompareLazPackage(Data1, Data2: Pointer): integer;
function CompareNameWithPackage(Key, Data: Pointer): integer;

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

procedure SortDependencyList(Dependencies: TList);
var
  Count: Integer;
  i, j: Integer;
  Dependency1: TPkgDependency;
  Dependency2: TPkgDependency;
  Sorted: Boolean;
begin
  if (Dependencies=nil) or (Dependencies.Count<2) then exit;
  // check if already sorted
  Count:=Dependencies.Count;
  Sorted:=true;
  for i:=0 to Count-2 do begin
    Dependency1:=TPkgDependency(Dependencies[i]);
    Dependency2:=TPkgDependency(Dependencies[i+1]);
    if Dependency1.Compare(Dependency2)>0 then begin
      Sorted:=false;
      break;
    end;
  end;
  if Sorted then exit;
  // bubble sort (slow, but dependency lists are normally sorted)
  for i:=0 to Count-2 do begin
    Dependency1:=TPkgDependency(Dependencies[i]);
    for j:=i+1 to Count-1 do begin
      Dependency2:=TPkgDependency(Dependencies[j]);
      if Dependency1.Compare(Dependency2)>0 then begin
        Dependencies.Exchange(i,j);
        Dependency1:=TPkgDependency(Dependencies[i]);
      end;
    end;
  end;
end;

function CompareLazPackage(Data1, Data2: Pointer): integer;
var
  Pkg1: TLazPackage;
  Pkg2: TLazPackage;
begin
  Pkg1:=TLazPackage(Data1);
  Pkg2:=TLazPackage(Data2);
  Result:=Pkg1.Compare(Pkg2);
end;

function CompareNameWithPackage(Key, Data: Pointer): integer;
var
  Name: String;
  Pkg: TLazPackage;
begin
  if Key<>nil then begin
    Name:=AnsiString(Key);
    Pkg:=TLazPackage(Data);
    Result:=AnsiCompareText(Name,Pkg.Name);
  end else
    Result:=-1;
end;

{ TPkgFile }

procedure TPkgFile.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
  UpdateUnitName;
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

procedure TPkgFile.UpdateUnitName;
begin
  if FilenameIsPascalUnit(FFilename) then
    FUnitName:=ExtractFileNameOnly(FFilename)
  else
    FUnitName:='';
end;

constructor TPkgFile.Create(ThePackage: TLazPackage);
begin
  Clear;
  FPackage:=ThePackage;
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
  FPackage.LongenFilename(FFilename);
  UpdateUnitName;
  HasRegisteredProc:=XMLConfig.GetValue(Path+'HasRegisteredProc/Value',false);
  FileType:=PkgFileTypeIdentToType(XMLConfig.GetValue(Path+'Type/Value',''));
end;

procedure TPkgFile.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
var
  TmpFilename: String;
begin
  TmpFilename:=Filename;
  FPackage.ShortenFilename(TmpFilename);
  XMLConfig.SetDeleteValue(Path+'Filename/Value',TmpFilename,'');
  XMLConfig.SetDeleteValue(Path+'HasRegisteredProc/Value',HasRegisteredProc,
                           false);
  XMLConfig.SetDeleteValue(Path+'Type/Value',PkgFileTypeIdents[FileType],
                           PkgFileTypeIdents[pftUnit]);
end;

procedure TPkgFile.ConsistencyCheck;
begin
  if FPackage=nil then
    RaiseGDBException('TPkgFile.ConsistencyCheck FPackage=nil');
  if FFilename='' then
    RaiseGDBException('TPkgFile.ConsistencyCheck FFilename=""');
end;

function TPkgFile.IsVirtual: boolean;
begin
  Result:=FilenameIsAbsolute(FFilename);
end;

function TPkgFile.UnitName: string;
begin
  Result:=FUnitName;
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

function TPkgDependency.MakeSense: boolean;
begin
  Result:=(pdfActive in FFlags) and IsValidIdent(PackageName);
  if Result
  and (pdfMinVersion in FFlags) and (pdfMaxVersion in FFlags)
  and (MinVersion.Compare(MaxVersion)>0) then
    Result:=false;
end;

function TPkgDependency.IsCompatible(const Version: TPkgVersion): boolean;
begin
  if ((pdfMinVersion in FFlags) and (MinVersion.Compare(Version)>0))
  or ((pdfMaxVersion in FFlags) and (MaxVersion.Compare(Version)<0)) then
    Result:=false
  else
    Result:=true;
end;

function TPkgDependency.IsCompatible(const PkgName: string;
  const Version: TPkgVersion): boolean;
begin
  Result:=(AnsiCompareText(PkgName,PackageName)=0) and IsCompatible(Version);
end;

function TPkgDependency.Compare(Dependency2: TPkgDependency): integer;
begin
  Result:=AnsiCompareText(PackageName,Dependency2.PackageName);
  if Result<>0 then exit;
  Result:=MinVersion.Compare(Dependency2.MinVersion);
  if Result<>0 then exit;
  Result:=MaxVersion.Compare(Dependency2.MaxVersion);
end;

procedure TPkgDependency.ConsistencyCheck;
begin

end;

function TPkgDependency.IsCompatible(Pkg: TLazPackage): boolean;
begin
  Result:=IsCompatible(Pkg.Name,Pkg.Version);
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

function TPkgVersion.Compare(Version2: TPkgVersion): integer;
begin
  Result:=Major-Version2.Major;
  if Result=0 then Result:=Minor-Version2.Minor;
  if Result=0 then Result:=Build-Version2.Build;
  if Result=0 then Result:=Release-Version2.Release;
end;

procedure TPkgVersion.Assign(Source: TPkgVersion);
begin
  Major:=Source.Major;
  Minor:=Source.Minor;
  Build:=Source.Build;
  Release:=Source.Release;
end;

function TPkgVersion.AsString: string;
begin
  Result:=IntToStr(Major)+'.'+IntToStr(Minor)+'.'+IntToStr(Build)+'.'
          +IntToStr(Release)+'.';
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

function TLazPackage.GetComponents(Index: integer): TPkgComponent;
begin
  Result:=TPkgComponent(FComponents[Index]);
end;

function TLazPackage.GetConflictPkgCount: integer;
begin
  Result:=FConflictPkgs.Count;
end;

function TLazPackage.GetConflictPkgs(Index: integer): TPkgDependency;
begin
  Result:=TPkgDependency(FConflictPkgs[Index]);
end;

function TLazPackage.GetDependingPkgCount: integer;
begin
  Result:=FDependingPkgs.Count;
end;

function TLazPackage.GetDependingPkgs(Index: integer): TLazPackage;
begin
  Result:=TLazPackage(FDependingPkgs[Index]);
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

function TLazPackage.GetOpen: boolean;
begin
  Result:=lpfOpenInIDE in FFLags;
end;

function TLazPackage.GetRequiredPkgCount: integer;
begin
  Result:=FRequiredPkgs.Count;
end;

function TLazPackage.GetRequiredPkgs(Index: integer): TPkgDependency;
begin
  Result:=TPkgDependency(FRequiredPkgs[Index]);
end;

function TLazPackage.GetUsedPkgCount: integer;
begin
  Result:=FUsedPkgs.Count;
end;

function TLazPackage.GetUsedPkgs(Index: integer): TLazPackage;
begin
  Result:=TLazPackage(FUsedPkgs[Index]);
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

procedure TLazPackage.SetAutoLoad(const AValue: boolean);
begin
  if FAutoLoad=AValue then exit;
  FAutoLoad:=AValue;
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

procedure TLazPackage.SetEditorRect(const AValue: TRect);
begin
  FEditorRect:=AValue;
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

procedure TLazPackage.SetInstalled(const AValue: boolean);
begin
  if FInstalled=AValue then exit;
  FInstalled:=AValue;
end;

procedure TLazPackage.SetLoaded(const AValue: boolean);
begin
  if FLoaded=AValue then exit;
  FLoaded:=AValue;
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

procedure TLazPackage.SetOpen(const AValue: boolean);
begin
  if Open=AValue then exit;
  if AValue then
    Include(FFlags,lpfOpenInIDE)
  else
    Exclude(FFlags,lpfOpenInIDE);
end;

procedure TLazPackage.SetPackageEditor(const AValue: TBasePackageEditor);
begin
  if FPackageEditor=AValue then exit;
  FPackageEditor:=AValue;
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
  FComponents:=TList.Create;
  FDependingPkgs:=Tlist.Create;
  FRequiredPkgs:=TList.Create;
  FFiles:=TList.Create;
  FCompilerOptions:=TPkgCompilerOptions.Create;
  FUsageOptions:=TAdditionalCompilerOptions.Create;
  FUsedPkgs:=TList.Create;
end;

destructor TLazPackage.Destroy;
begin
  Clear;
  FreeAndNil(FFiles);
  FreeAndNil(FComponents);
  FreeAndNil(FCompilerOptions);
  FreeAndNil(FConflictPkgs);
  FreeAndNil(FDependingPkgs);
  FreeAndNil(FUsedPkgs);
  FreeAndNil(FRequiredPkgs);
  FreeAndNil(FVersion);
  FreeAndNil(FUsageOptions);
  inherited Destroy;
end;

procedure TLazPackage.Clear;
var
  i: Integer;
begin
  FAuthor:='';
  for i:=0 to FConflictPkgs.Count-1 do ConflictPkgs[i].Free;
  FConflictPkgs.Clear;
  for i:=0 to FComponents.Count-1 do Components[i].Free;
  FComponents.Clear;
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
  FUsedPkgs.Clear;
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
    SortDependencyList(List);
  end;

  procedure LoadFiles(const ThePath: string; List: TList);
  var
    i: Integer;
    NewCount: Integer;
    PkgFile: TPkgFile;
  begin
    NewCount:=XMLConfig.GetValue(ThePath+'Count',0);
    for i:=0 to NewCount-1 do begin
      PkgFile:=TPkgFile.Create(Self);
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
    if XMLConfig.GetValue(ThePath+'OpenInIDE/Value',true) then
      Include(FFlags,lpfOpenInIDE)
    else
      Exclude(FFlags,lpfOpenInIDE);
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

function TLazPackage.IsVirtual: boolean;
begin
  Result:=not FilenameIsAbsolute(Filename);
end;

procedure TLazPackage.CheckInnerDependencies;
begin
  // ToDo: make some checks like deactivating double requirements
end;

function TLazPackage.Compare(Package2: TLazPackage): integer;
begin
  Result:=AnsiCompareText(Name,Package2.Name);
  if Result<>0 then exit;
  Result:=Version.Compare(Package2.Version);
end;

procedure TLazPackage.ShortenFilename(var ExpandedFilename: string);
var
  PkgDir: String;
  CurPath: String;
begin
  if IsVirtual then exit;
  PkgDir:=Directory;
  CurPath:=ExtractFilePath(ExpandedFilename);
  if CompareFilenames(PkgDir,CurPath)=0 then begin
    ExpandedFilename:=copy(ExpandedFilename,length(CurPath)+1,
                           length(ExpandedFilename)-length(CurPath));
  end;
end;

procedure TLazPackage.LongenFilename(var ExpandedFilename: string);
begin
  if IsVirtual then exit;
  if not FilenameIsAbsolute(ExpandedFilename) then
    ExpandedFilename:=TrimFilename(Directory+ExpandedFilename);
end;

procedure TLazPackage.IterateComponentClasses(
  Event: TIterateComponentClassesEvent; WithRequiredPackages: boolean);
var
  Cnt: Integer;
  i: Integer;
begin
  // iterate through components in this package
  Cnt:=ComponentCount;
  for i:=0 to Cnt-1 do Event(Components[i]);
  // iterate through all used packages
  if WithRequiredPackages then begin
    // ToDo
  end;
end;

procedure TLazPackage.ConsistencyCheck;
begin
  CheckList(FUsedPkgs,true,true,true);
  CheckList(FDependingPkgs,true,true,true);
  CheckList(FRequiredPkgs,true,true,true);
  CheckList(FConflictPkgs,true,true,true);
  CheckList(FFiles,true,true,true);
  CheckList(FComponents,true,true,true);
  CheckEmptyListCut(FDependingPkgs,FUsedPkgs);
  
end;

function TLazPackage.IndexOfPkgComponent(PkgComponent: TPkgComponent): integer;
begin
  Result:=FComponents.IndexOf(PkgComponent);
end;

function TLazPackage.FindUnit(const TheUnitName: string): TPkgFile;
var
  Cnt: Integer;
  i: Integer;
begin
  if TheUnitName<>'' then begin
    Cnt:=FileCount;
    for i:=0 to Cnt-1 do begin
      Result:=Files[i];
      if AnsiCompareText(Result.UnitName,TheUnitName)=0 then exit;
    end;
  end;
  Result:=nil;
end;

function TLazPackage.NameAndVersion: string;
begin
  Result:=Name+' '+Version.AsString;
end;

{ TPkgComponent }

procedure TPkgComponent.SetDefaultCompClassName(const AValue: string);
begin
  if FDefaultCompClassName=AValue then exit;
  FDefaultCompClassName:=AValue;
end;

constructor TPkgComponent.Create(ThePkgFile: TPkgFile;
  TheComponentClass: TComponentClass; const TheDefCompClassName: string);
begin
  FPkgFile:=ThePkgFile;
  FComponentClass:=TheComponentClass;
  if FComponentClass<>nil then
    FDefaultCompClassName:=FComponentClass.ClassName
  else
    FDefaultCompClassName:=TheDefCompClassName;
end;

destructor TPkgComponent.Destroy;
begin
  inherited Destroy;
end;

function TPkgComponent.GetComponentClassName: string;
begin
  if ComponentClass<>nil then
    Result:=ComponentClass.ClassName
  else
    Result:=DefaultCompClassName;
end;

procedure TPkgComponent.ConsistencyCheck;
begin
  if (FComponentClass<>nil)
  and (AnsiCompareText(FComponentClass.ClassName,FDefaultCompClassName)<>0) then
    RaiseGDBException('TPkgComponent.ConsistencyCheck FComponentClass.ClassName<>FDefaultCompClassName');
  if FPkgFile=nil then
    RaiseGDBException('TPkgComponent.ConsistencyCheck FPkgFile=nil');
  if FPkgFile.LazPackage=nil then
    RaiseGDBException('TPkgComponent.ConsistencyCheck FPkgFile.LazPackage=nil');
  if FPkgFile.LazPackage.IndexOfPkgComponent(Self)<0 then
    RaiseGDBException('TPkgComponent.ConsistencyCheck FPkgFile.LazPackage.IndexOfPkgComponent(Self)<0');
  if not IsValidIdent(FDefaultCompClassName) then
    RaiseGDBException('TPkgComponent.ConsistencyCheck not IsValidIdent(FDefaultCompClassName)');
end;

end.

