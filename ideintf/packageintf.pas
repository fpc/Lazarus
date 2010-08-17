{  $Id$  }
{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Methods and Types to access the IDE packages.
}
unit PackageIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, LazConfigStorage, NewItemIntf, AvgLvlTree;
  
const
  PkgDescGroupName = 'Package';
  PkgDescNameStandard = 'Standard Package';

type
  { TPkgVersion }

  TPkgVersionValid = (
    pvtNone,
    pvtMajor,
    pvtMinor,
    pvtRelease,
    pvtBuild
    );

  TPkgVersion = class
  public
    Major: integer;
    Minor: integer;
    Release: integer;
    Build: integer;
    Valid: TPkgVersionValid;
    OnChange: TNotifyEvent;
    procedure Clear;
    function Compare(Version2: TPkgVersion): integer;
    function CompareMask(ExactVersion: TPkgVersion): integer;
    procedure Assign(Source: TPkgVersion);
    function AsString: string;
    function AsWord: string;
    function ReadString(const s: string): boolean;
    procedure SetValues(NewMajor, NewMinor, NewRelease, NewBuild: integer;
                        NewValid: TPkgVersionValid = pvtBuild);
    function VersionBound(v: integer): integer;
  end;

  TIDEPackage = class;

  { TLazPackageFile }

  TLazPackageFile = class
  private
    FFilename: string;
    FRemoved: boolean;
  protected
    procedure SetFilename(const AValue: string); virtual;
    function GetIDEPackage: TIDEPackage; virtual; abstract;
    procedure SetRemoved(const AValue: boolean); virtual;
  public
    function GetFullFilename: string; virtual; abstract;
    function GetShortFilename(UseUp: boolean): string; virtual; abstract;
  public
    property Filename: string read FFilename write SetFilename; // can contain macros if package was auto created
    property LazPackage: TIDEPackage read GetIDEPackage;
    property Removed: boolean read FRemoved write SetRemoved;
  end;

  { TLazPackageID }

  TLazPackageID = class
  private
    FIDAsWord: string;
  protected
    FName: string;
    FVersion: TPkgVersion;
    FIDAsString: string;
    procedure SetName(const AValue: string); virtual;
    procedure UpdateIDAsString;
    procedure VersionChanged(Sender: TObject); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function StringToID(const s: string): boolean;
    function Compare(PackageID2: TLazPackageID): integer;
    function CompareMask(ExactPackageID: TLazPackageID): integer;
    procedure AssignID(Source: TLazPackageID); virtual;
  public
    property Name: string read FName write SetName;
    property Version: TPkgVersion read FVersion;
    property IDAsString: string read FIDAsString;
    property IDAsWord: string read FIDAsWord;
  end;

  { TIDEPackage }

  TIDEPackage = class(TLazPackageID)
  protected
    FCustomOptions: TConfigStorage;
    FFilename: string;
    FChangeStamp: integer;
    function GetDirectoryExpanded: string; virtual; abstract;
    function GetFileCount: integer; virtual; abstract;
    function GetPkgFiles(Index: integer): TLazPackageFile; virtual; abstract;
    function GetModified: boolean; virtual; abstract;
    procedure SetFilename(const AValue: string); virtual; abstract;
    procedure SetModified(const AValue: boolean); virtual; abstract;
    function GetRemovedCount: integer; virtual; abstract;
    function GetRemovedPkgFiles(Index: integer): TLazPackageFile; virtual; abstract;
  public
    function IsVirtual: boolean; virtual; abstract;
    function ReadOnly: boolean; virtual; abstract;
    constructor Create;
    destructor Destroy; override;
    procedure ClearCustomOptions;
  public
    property Filename: string read FFilename write SetFilename;//the .lpk filename
    property Modified: boolean read GetModified write SetModified;
    property DirectoryExpanded: string read GetDirectoryExpanded;
    property CustomOptions: TConfigStorage read FCustomOptions;
    property ChangeStamp: integer read FChangeStamp;
    property FileCount: integer read GetFileCount;
    property Files[Index: integer]: TLazPackageFile read GetPkgFiles;
    property RemovedFilesCount: integer read GetRemovedCount;
    property RemovedFiles[Index: integer]: TLazPackageFile read GetRemovedPkgFiles;
  end;

type
  TPkgSaveFlag = (
    psfSaveAs,
    psfAskBeforeSaving
    );
  TPkgSaveFlags = set of TPkgSaveFlag;

  TPkgOpenFlag = (
    pofAddToRecent,   // add file to recent files
    pofRevert,        // reload file if already open
    pofConvertMacros, // replace macros in filename
    pofMultiOpen,     // set during loading multiple files, shows 'Cancel all' button using mrAbort
    pofDoNotOpenEditor// do not open packageeditor
    );
  TPkgOpenFlags = set of TPkgOpenFlag;

  TPkgCompileFlag = (
    pcfCleanCompile,  // append -B to the compiler options
    pcfDoNotCompileDependencies,
    pcfDoNotCompilePackage,
    pcfCompileDependenciesClean,
    pcfOnlyIfNeeded,
    pcfDoNotSaveEditorFiles,
    pcfCreateMakefile
    );
  TPkgCompileFlags = set of TPkgCompileFlag;

const
  PkgCompileFlagNames: array[TPkgCompileFlag] of string = (
    'pcfCleanCompile',
    'pcfDoNotCompileDependencies',
    'pcfDoNotCompilePackage',
    'pcfCompileDependenciesClean',
    'pcfOnlyIfNeeded',
    'pcfDoNotSaveEditorFiles',
    'pcfCreateMakefile'
    );

type
  TPkgIntfOwnerSearchFlag = (
    piosfExcludeOwned, // file must not be marked as part of project/package
    piosfIncludeSourceDirectories
    );
  TPkgIntfOwnerSearchFlags = set of TPkgIntfOwnerSearchFlag;

  TPkgIntfHandlerType = (
    pihtGraphChanged, // called after loading/saving packages, changing dependencies
    pihtPackageFileLoaded  { called after loading a lpk,
           before the package is initialized and the dependencies are resolved }
    );

  { TPackageEditingInterface }

  TPackageEditingInterface = class(TComponent)
  protected
    FHandlers: array[TPkgIntfHandlerType] of TMethodList;
    procedure AddHandler(HandlerType: TPkgIntfHandlerType;
                         const AMethod: TMethod; AsLast: boolean = false);
    procedure RemoveHandler(HandlerType: TPkgIntfHandlerType;
                            const AMethod: TMethod);
    procedure DoCallNotifyHandler(HandlerType: TPkgIntfHandlerType; Sender: TObject);
  public
    function DoOpenPackageWithName(const APackageName: string;
                         Flags: TPkgOpenFlags;
                         ShowAbort: boolean): TModalResult; virtual; abstract;
    function DoOpenPackageFile(AFilename: string;
                         Flags: TPkgOpenFlags; ShowAbort: boolean
                         ): TModalResult; virtual; abstract;
    function DoSaveAllPackages(Flags: TPkgSaveFlags): TModalResult; virtual; abstract;

    function AddUnitDependenciesForComponentClasses(const UnitFilename: string;
                         ComponentClassnames: TStrings;
                         Quiet: boolean = false): TModalResult; virtual; abstract;
    function GetOwnersOfUnit(const UnitFilename: string): TFPList; virtual; abstract;
    procedure ExtendOwnerListWithUsedByOwners(OwnerList: TFPList); virtual; abstract;
    function GetSourceFilesOfOwners(OwnerList: TFPList): TStrings; virtual; abstract;
    function GetPossibleOwnersOfUnit(const UnitFilename: string;
                                     Flags: TPkgIntfOwnerSearchFlags): TFPList; virtual; abstract;

    function GetPackageCount: integer; virtual; abstract;
    function GetPackages(Index: integer): TIDEPackage; virtual; abstract;
    function FindPackageWithName(const PkgName: string): TIDEPackage; virtual; abstract;

    // package editors
    function GetPackageOfEditorItem(Sender: TObject): TIDEPackage; virtual; abstract;

    // events
    procedure RemoveAllHandlersOfObject(AnObject: TObject);
    procedure AddHandlerOnGraphChanged(const OnGraphChanged: TNotifyEvent;
                                       AsLast: boolean = false);
    procedure RemoveHandlerOnGraphChanged(const OnGraphChanged: TNotifyEvent);
    procedure AddHandlerOnPackageFileLoaded(const OnPkgLoaded: TNotifyEvent;
                                        AsLast: boolean = false);
    procedure RemoveHandlerOnPackageFileLoaded(const OnPkgLoaded: TNotifyEvent);
  end;
  
var
  PackageEditingInterface: TPackageEditingInterface; // will be set by the IDE


type
  { TPackageDescriptor }
  
  TPackageDescriptor = class(TPersistent)
  private
    FName: string;
    FReferenceCount: integer;
    FVisibleInNewDialog: boolean;
  protected
    procedure SetName(const AValue: string); virtual;
  public
    constructor Create; virtual;
    function GetLocalizedName: string; virtual;
    function GetLocalizedDescription: string; virtual;
    procedure Release;
    procedure Reference;
    // TODO: procedure InitPackage(APackage: TLazPackage); virtual;
    // TODO: procedure CreateStartFiles(APackage: TLazPackage); virtual;
  public
    property Name: string read FName write SetName;
    property VisibleInNewDialog: boolean read FVisibleInNewDialog write FVisibleInNewDialog;
  end;
  TPackageDescriptorClass = class of TPackageDescriptor;


  { TNewItemPackage - a new item for package descriptors }

  TNewItemPackage = class(TNewIDEItemTemplate)
  private
    FDescriptor: TPackageDescriptor;
  public
    function LocalizedName: string; override;
    function Description: string; override;
    procedure Assign(Source: TPersistent); override;
  public
    property Descriptor: TPackageDescriptor read FDescriptor write FDescriptor;
  end;


  { TPackageDescriptors }

  TPackageDescriptors = class(TPersistent)
  protected
    function GetItems(Index: integer): TPackageDescriptor; virtual; abstract;
  public
    function Count: integer; virtual; abstract;
    function GetUniqueName(const Name: string): string; virtual; abstract;
    function IndexOf(const Name: string): integer; virtual; abstract;
    function FindByName(const Name: string): TPackageDescriptor; virtual; abstract;
    procedure RegisterDescriptor(Descriptor: TPackageDescriptor); virtual; abstract;
    procedure UnregisterDescriptor(Descriptor: TPackageDescriptor); virtual; abstract;
  public
    property Items[Index: integer]: TPackageDescriptor read GetItems; default;
  end;

var
  PackageDescriptors: TPackageDescriptors; // will be set by the IDE


procedure RegisterPackageDescriptor(PkgDesc: TPackageDescriptor);
function PackageDescriptorStd: TPackageDescriptor;

function PkgCompileFlagsToString(Flags: TPkgCompileFlags): string;


implementation


function PkgCompileFlagsToString(Flags: TPkgCompileFlags): string;
var
  f: TPkgCompileFlag;
begin
  Result:='';
  for f:=Low(TPkgCompileFlag) to High(TPkgCompileFlag) do begin
    if not (f in Flags) then continue;
    if Result<>'' then Result:=Result+',';
    Result:=Result+PkgCompileFlagNames[f];
  end;
  Result:='['+Result+']';
end;

procedure RegisterPackageDescriptor(PkgDesc: TPackageDescriptor);
var
  NewItemPkg: TNewItemPackage;
begin
  PackageDescriptors.RegisterDescriptor(PkgDesc);
  if PkgDesc.VisibleInNewDialog then begin
    NewItemPkg:=TNewItemPackage.Create(PkgDesc.Name,niifCopy,[niifCopy]);
    NewItemPkg.Descriptor:=PkgDesc;
    RegisterNewDialogItem(PkgDescGroupName,NewItemPkg);
  end;
end;

function PackageDescriptorStd: TPackageDescriptor;
begin
  Result:=PackageDescriptors.FindByName(PkgDescNameStandard);
end;

{ TPackageDescriptor }

procedure TPackageDescriptor.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
end;

constructor TPackageDescriptor.Create;
begin
  FReferenceCount:=1;
  fVisibleInNewDialog:=true;
end;

function TPackageDescriptor.GetLocalizedName: string;
begin
  Result:=Name;
end;

function TPackageDescriptor.GetLocalizedDescription: string;
begin
  Result:=GetLocalizedName;
end;

procedure TPackageDescriptor.Release;
begin
  //debugln('TPackageDescriptor.Release A ',Name,' ',dbgs(FReferenceCount));
  if FReferenceCount=0 then
    raise Exception.Create('');
  dec(FReferenceCount);
  if FReferenceCount=0 then Free;
end;

procedure TPackageDescriptor.Reference;
begin
  inc(FReferenceCount);
end;

{ TNewItemPackage }

function TNewItemPackage.LocalizedName: string;
begin
  Result:=Descriptor.GetLocalizedName;
end;

function TNewItemPackage.Description: string;
begin
  Result:=Descriptor.GetLocalizedDescription;
end;

procedure TNewItemPackage.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TNewItemPackage then
    FDescriptor:=TNewItemPackage(Source).Descriptor;
end;

{ TPkgVersion }

procedure TPkgVersion.Clear;
begin
  SetValues(0,0,0,0,pvtBuild);
end;

function TPkgVersion.Compare(Version2: TPkgVersion): integer;
begin
  Result:=Major-Version2.Major;
  if Result<>0 then exit;
  Result:=Minor-Version2.Minor;
  if Result<>0 then exit;
  Result:=Release-Version2.Release;
  if Result<>0 then exit;
  Result:=Build-Version2.Build;
end;

function TPkgVersion.CompareMask(ExactVersion: TPkgVersion): integer;
begin
  if Valid=pvtNone then exit(0);
  Result:=Major-ExactVersion.Major;
  if Result<>0 then exit;
  if Valid=pvtMajor then exit;
  Result:=Minor-ExactVersion.Minor;
  if Result<>0 then exit;
  if Valid=pvtMinor then exit;
  Result:=Release-ExactVersion.Release;
  if Result<>0 then exit;
  if Valid=pvtRelease then exit;
  Result:=Build-ExactVersion.Build;
end;

procedure TPkgVersion.Assign(Source: TPkgVersion);
begin
  SetValues(Source.Major,Source.Minor,Source.Release,Source.Build,Source.Valid);
end;

function TPkgVersion.AsString: string;
begin
  Result:=IntToStr(Major)+'.'+IntToStr(Minor);
  if (Build<>0) then
    Result:=Result+'.'+IntToStr(Release)+'.'+IntToStr(Build)
  else if (Release<>0) then
    Result:=Result+'.'+IntToStr(Release)
end;

function TPkgVersion.AsWord: string;
begin
  Result:=IntToStr(Major)+'_'+IntToStr(Minor);
  if (Build<>0) then
    Result:=Result+'_'+IntToStr(Release)+'_'+IntToStr(Build)
  else if (Release<>0) then
    Result:=Result+'_'+IntToStr(Release)
end;

function TPkgVersion.ReadString(const s: string): boolean;
var
  ints: array[1..4] of integer;
  i: integer;
  CurPos: Integer;
  StartPos: Integer;
  NewValid: TPkgVersionValid;
begin
  Result:=false;
  CurPos:=1;
  NewValid:=pvtNone;
  for i:=1 to 4 do begin
    ints[i]:=0;
    if CurPos<length(s) then begin
      if i>Low(ints) then begin
        // read point
        if s[CurPos]<>'.' then exit;
        inc(CurPos);
      end;
      // read int
      StartPos:=CurPos;
      while (CurPos<=length(s)) and (i<=9999)
      and (s[CurPos] in ['0'..'9']) do begin
        ints[i]:=ints[i]*10+ord(s[CurPos])-ord('0');
        inc(CurPos);
      end;
      if (StartPos=CurPos) then exit;
      NewValid:=succ(NewValid);
    end;
  end;
  if CurPos<=length(s) then exit;
  SetValues(ints[1],ints[2],ints[3],ints[4],NewValid);

  Result:=true;
end;

procedure TPkgVersion.SetValues(NewMajor, NewMinor, NewRelease,
  NewBuild: integer; NewValid: TPkgVersionValid);
begin
  NewMajor:=VersionBound(NewMajor);
  NewMinor:=VersionBound(NewMinor);
  NewRelease:=VersionBound(NewRelease);
  NewBuild:=VersionBound(NewBuild);
  if (NewMajor=Major) and (NewMinor=Minor) and (NewRelease=Release)
  and (NewBuild=Build) and (NewValid=Valid) then exit;
  Major:=NewMajor;
  Minor:=NewMinor;
  Release:=NewRelease;
  Build:=NewBuild;
  Valid:=NewValid;
  if Assigned(OnChange) then OnChange(Self);
end;

function TPkgVersion.VersionBound(v: integer): integer;
begin
  if v>9999 then
    Result:=9999
  else if v<0 then
    Result:=0
  else
    Result:=v;
end;

{ TLazPackageID }

procedure TLazPackageID.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
  UpdateIDAsString;
end;

constructor TLazPackageID.Create;
begin
  FVersion:=TPkgVersion.Create;
  FVersion.OnChange:=@VersionChanged;
end;

destructor TLazPackageID.Destroy;
begin
  FreeAndNil(FVersion);
  inherited Destroy;
end;

procedure TLazPackageID.UpdateIDAsString;
begin
  FIDAsString:=Version.AsString;
  if FIDAsString<>'' then
    FIDAsString:=Name+' '+FIDAsString
  else
    FIDAsString:=FIDAsString;
  FIDAsWord:=Version.AsWord;
  if FIDAsWord<>'' then
    FIDAsWord:=Name+FIDAsWord
  else
    FIDAsWord:=FIDAsWord;
end;

procedure TLazPackageID.VersionChanged(Sender: TObject);
begin
  UpdateIDAsString;
end;

function TLazPackageID.StringToID(const s: string): boolean;
var
  IdentEndPos: Integer;
  StartPos: Integer;
begin
  Result:=false;
  IdentEndPos:=1;
  while (IdentEndPos<=length(s))
  and (s[IdentEndPos] in ['a'..'z','A'..'Z','0'..'9','_'])
  do
    inc(IdentEndPos);
  if IdentEndPos=1 then exit;
  Name:=copy(s,1,IdentEndPos-1);
  StartPos:=IdentEndPos;
  while (StartPos<=length(s)) and (s[StartPos]=' ') do inc(StartPos);
  if StartPos=IdentEndPos then begin
    Version.Clear;
    Version.Valid:=pvtNone;
  end else begin
    if not Version.ReadString(copy(s,StartPos,length(s))) then exit;
  end;
  Result:=true;
end;

function TLazPackageID.Compare(PackageID2: TLazPackageID): integer;
begin
  if PackageID2 <> nil then
  begin
    Result:=CompareText(Name,PackageID2.Name);
    if Result<>0 then exit;
    Result:=Version.Compare(PackageID2.Version);
  end
  else
    Result := -1;
end;

function TLazPackageID.CompareMask(ExactPackageID: TLazPackageID): integer;
begin
  Result:=CompareText(Name,ExactPackageID.Name);
  if Result<>0 then exit;
  Result:=Version.CompareMask(ExactPackageID.Version);
end;

procedure TLazPackageID.AssignID(Source: TLazPackageID);
begin
  Name:=Source.Name;
  Version.Assign(Source.Version);
end;

{ TIDEPackage }

constructor TIDEPackage.Create;
begin
  inherited Create;
  FCustomOptions:=TConfigMemStorage.Create('',false);
end;

destructor TIDEPackage.Destroy;
begin
  FreeAndNil(FCustomOptions);
  inherited Destroy;
end;

procedure TIDEPackage.ClearCustomOptions;
begin
  TConfigMemStorage(FCustomOptions).Clear;
end;

{ TPackageEditingInterface }

procedure TPackageEditingInterface.AddHandler(HandlerType: TPkgIntfHandlerType;
  const AMethod: TMethod; AsLast: boolean);
begin
  if FHandlers[HandlerType]=nil then
    FHandlers[HandlerType]:=TMethodList.Create;
  FHandlers[HandlerType].Add(AMethod);
end;

procedure TPackageEditingInterface.RemoveHandler(
  HandlerType: TPkgIntfHandlerType; const AMethod: TMethod);
begin
  FHandlers[HandlerType].Remove(AMethod);
end;

procedure TPackageEditingInterface.DoCallNotifyHandler(
  HandlerType: TPkgIntfHandlerType; Sender: TObject);
begin
  FHandlers[HandlerType].CallNotifyEvents(Sender);
end;

procedure TPackageEditingInterface.RemoveAllHandlersOfObject(AnObject: TObject
  );
var
  HandlerType: TPkgIntfHandlerType;
begin
  for HandlerType:=Low(HandlerType) to High(HandlerType) do
    FHandlers[HandlerType].RemoveAllMethodsOfObject(AnObject);
end;

procedure TPackageEditingInterface.AddHandlerOnGraphChanged(
  const OnGraphChanged: TNotifyEvent; AsLast: boolean);
begin
  AddHandler(pihtGraphChanged,TMethod(OnGraphChanged));
end;

procedure TPackageEditingInterface.RemoveHandlerOnGraphChanged(
  const OnGraphChanged: TNotifyEvent);
begin
  RemoveHandler(pihtGraphChanged,TMethod(OnGraphChanged));
end;

procedure TPackageEditingInterface.AddHandlerOnPackageFileLoaded(
  const OnPkgLoaded: TNotifyEvent; AsLast: boolean);
begin
  AddHandler(pihtPackageFileLoaded,TMethod(OnPkgLoaded));
end;

procedure TPackageEditingInterface.RemoveHandlerOnPackageFileLoaded(
  const OnPkgLoaded: TNotifyEvent);
begin
  RemoveHandler(pihtPackageFileLoaded,TMethod(OnPkgLoaded));
end;

{ TLazPackageFile }

procedure TLazPackageFile.SetFilename(const AValue: string);
begin
  FFilename:=AValue;
end;

procedure TLazPackageFile.SetRemoved(const AValue: boolean);
begin
  FRemoved:=AValue;
end;

initialization
  PackageEditingInterface:=nil;

end.

