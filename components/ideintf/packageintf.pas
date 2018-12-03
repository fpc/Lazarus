{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Methods and Types to access the IDE packages.
}
unit PackageIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  // LazUtils
  LazConfigStorage, LazMethodList, LazLoggerBase, UITypes,
  // IdeIntf
  NewItemIntf, ProjPackIntf, PackageDependencyIntf, IDEOptionsIntf;
  
const
  PkgDescGroupName = 'Package';
  PkgDescNameStandard = 'Standard Package';

type
  TPkgFileType = (
    pftUnit,    // file is pascal unit
    pftVirtualUnit,// file is virtual pascal unit
    pftMainUnit, // file is the auto created main pascal unit
    pftLFM,     // lazarus form text file
    pftLRS,     // lazarus resource file
    pftInclude, // include file
    pftIssues,  // file is issues xml file
    pftText,    // file is text (e.g. copyright or install notes)
    pftBinary   // file is something else
    );
  TPkgFileTypes = set of TPkgFileType;

const
  PkgFileUnitTypes = [pftUnit,pftVirtualUnit,pftMainUnit];
  PkgFileRealUnitTypes = [pftUnit,pftMainUnit];
  PkgFileTypeIdents: array[TPkgFileType] of string = (
    'Unit', 'Virtual Unit', 'Main Unit',
    'LFM', 'LRS', 'Include', 'Issues', 'Text', 'Binary');

type
  TIDEPackage = class;

  { TLazPackageFile }

  TLazPackageFile = class(TIDEOwnedFile)
  private
    FDisableI18NForLFM: boolean;
    FFileType: TPkgFileType;
    FRemoved: boolean;
  protected
    function GetInUses: boolean; virtual; abstract;
    procedure SetInUses(AValue: boolean); virtual; abstract;
    function GetIDEPackage: TIDEPackage; virtual; abstract;
    procedure SetRemoved(const AValue: boolean); virtual;
    procedure SetDisableI18NForLFM(AValue: boolean); virtual;
    procedure SetFileType(const AValue: TPkgFileType); virtual;
  public
    property LazPackage: TIDEPackage read GetIDEPackage;
    property Removed: boolean read FRemoved write SetRemoved;
    property DisableI18NForLFM: boolean read FDisableI18NForLFM write SetDisableI18NForLFM;
    property FileType: TPkgFileType read FFileType write SetFileType;
    property InUses: boolean read GetInUses write SetInUses; // added to uses section of package
  end;

  { PkgDependency flags }

  TLoadPackageResult = (
    lprUndefined,
    lprSuccess,
    lprNotFound,
    lprLoadError
    );

  { TPkgDependencyID }

  TPkgDependencyID = class(TPkgDependencyBase)
  private
    procedure SetRequiredPackage(const AValue: TIDEPackage);
  protected
    FLoadPackageResult: TLoadPackageResult;
    FRequiredPackage: TIDEPackage;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
  public
    property LoadPackageResult: TLoadPackageResult read FLoadPackageResult write FLoadPackageResult;
    property RequiredIDEPackage: TIDEPackage read FRequiredPackage write SetRequiredPackage;
  end;

  { TLazPackageID }

  TLazPackageID = class(TIDEProjPackBase)
  private
    FIDAsString: string;
    FIDAsWord: string;
    function GetIDAsString: string;
    function GetIDAsWord: string;
  protected
    FVersion: TPkgVersion;
    procedure SetName(const NewName: TComponentName); override;
    procedure UpdateIDAsString;
    procedure VersionChanged(Sender: TObject); virtual;
    function GetDirectory: string; override;
  public
    procedure AssignOptions(Source: TPersistent); virtual;
    constructor Create; virtual; reintroduce;
    destructor Destroy; override;
    function StringToID(const s: string): boolean;
    function Compare(PackageID2: TLazPackageID): integer;
    function CompareMask(ExactPackageID: TLazPackageID): integer;
    procedure AssignID(Source: TLazPackageID); virtual;
  public
    property Version: TPkgVersion read FVersion;
    property IDAsString: string read GetIDAsString;
    property IDAsWord: string read GetIDAsWord;
  end;

  TIteratePackagesEvent = procedure(APackage: TLazPackageID) of object;

  TLazPackageType = (
    lptRunTime, // Cannot register anything in the IDE. Can be used by designtime packages.
    lptDesignTime, // Can register anything in the IDE but is not compiled into projects.
                   // The IDE calls the 'register' procedures of each unit.
    lptRunAndDesignTime, // Can do anything.
    lptRunTimeOnly // As lptRunTime but cannot be installed in the IDE, not even indirectly.
    );
  TLazPackageTypes = set of TLazPackageType;

const
  LazPackageTypeIdents: array[TLazPackageType] of string = (
    'RunTime', 'DesignTime', 'RunAndDesignTime', 'RunTimeOnly');

type
  TPackageInstallType = (
    pitNope,
    pitStatic,
    pitDynamic
    );

// FPMake/Lazarus build mode
  TBuildMethod = (
    bmLazarus,
    bmFPMake,
    bmBoth
    );

  { TIDEPackage }

  TIDEPackage = class(TLazPackageID)
  protected
    FAutoInstall: TPackageInstallType;
    FFilename: string;
    FChangeStamp: integer;
    FCustomOptions: TConfigStorage;
    FPackageType: TLazPackageType;
    FBuildMethod: TBuildMethod;
    function GetDirectoryExpanded: string; virtual; abstract;
    function GetFileCount: integer; virtual; abstract;
    function GetPkgFiles(Index: integer): TLazPackageFile; virtual; abstract;
    function GetModified: boolean; virtual; abstract;
    procedure SetFilename(const AValue: string); virtual; abstract;
    procedure SetModified(const AValue: boolean); virtual; abstract;
    function GetRemovedCount: integer; virtual; abstract;
    function GetRemovedPkgFiles(Index: integer): TLazPackageFile; virtual; abstract;
    procedure SetAutoInstall(AValue: TPackageInstallType); virtual; abstract;
  public
    procedure AssignOptions(Source: TPersistent); override;
    function IsVirtual: boolean; virtual; abstract;
    function ReadOnly: boolean; virtual; abstract;
    constructor Create; override;
    destructor Destroy; override;
    procedure ClearCustomOptions;
    // used by dependencies
    procedure AddUsedByDependency(Dependency: TPkgDependencyBase); virtual; abstract;
    procedure RemoveUsedByDependency(Dependency: TPkgDependencyBase); virtual; abstract;
  public
    property AutoInstall: TPackageInstallType read FAutoInstall write SetAutoInstall;
    property Filename: string read FFilename write SetFilename;//the .lpk filename
    property ChangeStamp: integer read FChangeStamp;
    property CustomOptions: TConfigStorage read FCustomOptions;
    property PackageType: TLazPackageType read FPackageType;
    property DirectoryExpanded: string read GetDirectoryExpanded;
    property FileCount: integer read GetFileCount;
    property Files[Index: integer]: TLazPackageFile read GetPkgFiles;
    property Modified: boolean read GetModified write SetModified;
    property RemovedFilesCount: integer read GetRemovedCount;
    property RemovedFiles[Index: integer]: TLazPackageFile read GetRemovedPkgFiles;
    property BuildMethod: TBuildMethod read FBuildMethod write FBuildMethod;
  end;

type
  TPkgSaveFlag = (
    psfSaveAs
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
    pcfOnlyIfNeeded,
    pcfCleanCompile,  // append -B to the compiler options
    pcfGroupCompile,
    pcfDoNotCompileDependencies,
    pcfDoNotCompilePackage,
    pcfCompileDependenciesClean,
    pcfSkipDesignTimePackages,
    pcfDoNotSaveEditorFiles,
    pcfCreateMakefile,
    pcfCreateFpmakeFile
    );
  TPkgCompileFlags = set of TPkgCompileFlag;

type
  TPkgInstallInIDEFlag = (
    piiifQuiet,
    piiifClear, // replace, clear the old list
    piiifRebuildIDE,
    piiifSkipChecks,
    piiifRemoveConflicts
    );
  TPkgInstallInIDEFlags = set of TPkgInstallInIDEFlag;

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

  TPkgIntfRequiredFlag = (
    pirNotRecursive, // return the list of direct dependencies, not sorted topologically
    pirSkipDesignTimeOnly,
    pirCompileOrder // start with packages that do not depend on other packages
    );
  TPkgIntfRequiredFlags = set of TPkgIntfRequiredFlag;

  TPkgIntfGatherUnitType = (
    piguListed, // unit is in list of given Owner, i.e. in lpi, lpk file, this may contain platform specific units
    piguUsed, // unit is used directly or indirectly by the start module and no currently open package/project is associated with it
    piguAllUsed // as pigyUsed, except even units associated with another package/project are returned
    );
  TPkgIntfGatherUnitTypes = set of TPkgIntfGatherUnitType;

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
    destructor Destroy; override;
    function DoOpenPackageWithName(const APackageName: string;
                         Flags: TPkgOpenFlags;
                         ShowAbort: boolean): TModalResult; virtual; abstract;
    function DoOpenPackageFile(AFilename: string;
                         Flags: TPkgOpenFlags; ShowAbort: boolean
                         ): TModalResult; virtual; abstract;
    function DoSaveAllPackages(Flags: TPkgSaveFlags): TModalResult; virtual; abstract;

    function GetOwnersOfUnit(const UnitFilename: string): TFPList; virtual; abstract;
    procedure ExtendOwnerListWithUsedByOwners(OwnerList: TFPList); virtual; abstract;
    function GetSourceFilesOfOwners(OwnerList: TFPList): TStrings; virtual; abstract;
    function GetUnitsOfOwners(OwnerList: TFPList; Flags: TPkgIntfGatherUnitTypes): TStrings; virtual; abstract;
    function GetPossibleOwnersOfUnit(const UnitFilename: string;
                                     Flags: TPkgIntfOwnerSearchFlags): TFPList; virtual; abstract;
    function GetPackageOfSourceEditor(out APackage: TIDEPackage; ASrcEdit: TObject): TLazPackageFile; virtual; abstract;

    function GetPackageCount: integer; virtual; abstract;
    function GetPackages(Index: integer): TIDEPackage; virtual; abstract;
    function FindPackageWithName(const PkgName: string; IgnorePackage: TIDEPackage = nil): TIDEPackage; virtual; abstract;
    function FindInstalledPackageWithUnit(const AnUnitName: string): TIDEPackage; virtual; abstract;
    function IsPackageInstalled(const PkgName: string): TIDEPackage; virtual; abstract;

    // dependencies
    function IsOwnerDependingOnPkg(AnOwner: TObject; const PkgName: string;
                                   out DependencyOwner: TObject): boolean; virtual; abstract;
    procedure GetRequiredPackages(AnOwner: TObject; // a TLazProject or TIDEPackage
      out PkgList: TFPList; // list of TIDEPackage
      Flags: TPkgIntfRequiredFlags = []) virtual; abstract;
    function AddDependencyToOwners(OwnerList: TFPList; APackage: TIDEPackage;
                   OnlyTestIfPossible: boolean = false): TModalResult; virtual; abstract; // mrOk or mrIgnore for already connected
    function AddUnitDependenciesForComponentClasses(const UnitFilename: string;
                         ComponentClassnames: TStrings;
                         Quiet: boolean = false): TModalResult; virtual; abstract;
    function RedirectPackageDependency(APackage: TIDEPackage): TIDEPackage; virtual; abstract;

    // package editors
    function GetPackageOfEditorItem(Sender: TObject): TIDEPackage; virtual; abstract;

    // package compilation
    function DoCompilePackage(APackage: TIDEPackage; Flags: TPkgCompileFlags;
                              ShowAbort: boolean): TModalResult; virtual; abstract;

    // install
    function CheckInstallPackageList(PkgIDList: TObjectList;
                 Flags: TPkgInstallInIDEFlags = []): boolean; virtual; abstract;
    function InstallPackages(PkgIdList: TObjectList;
                  Flags: TPkgInstallInIDEFlags = []): TModalResult; virtual; abstract;

    //uninstall
    function UninstallPackage(APackage: TIDEPackage; ShowAbort: boolean): TModalResult; virtual; abstract;

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

  TPackageGraphInterface = class
  protected
    FChangeStamp: Int64;
  protected
    procedure IncChangeStamp; virtual;
  public
    property ChangeStamp: Int64 read FChangeStamp;
  end;

  TAbstractPackageIDEOptions = class(TAbstractIDEOptions)
  protected
    function GetPackage: TIDEPackage; virtual; abstract;
  public
    property Package: TIDEPackage read GetPackage;
  end;


var
  PackageDescriptors: TPackageDescriptors; // will be set by the IDE
  PackageGraphInterface: TPackageGraphInterface; // must be set along with PackageSystem.PackageGraph


function PkgFileTypeIdentToType(const s: string): TPkgFileType;
function LazPackageTypeIdentToType(const s: string): TLazPackageType;
function PackageDescriptorStd: TPackageDescriptor;
function PkgCompileFlagsToString(Flags: TPkgCompileFlags): string;
procedure RegisterPackageDescriptor(PkgDesc: TPackageDescriptor);


implementation


function PkgFileTypeIdentToType(const s: string): TPkgFileType;
begin
  for Result:=Low(TPkgFileType) to High(TPkgFileType) do
    if SysUtils.CompareText(s,PkgFileTypeIdents[Result])=0 then exit;
  Result:=pftUnit;
end;

function LazPackageTypeIdentToType(const s: string): TLazPackageType;
begin
  for Result:=Low(TLazPackageType) to High(TLazPackageType) do
    if SysUtils.CompareText(s,LazPackageTypeIdents[Result])=0 then exit;
  Result:=lptRunTime;
end;

function PackageDescriptorStd: TPackageDescriptor;
begin
  Result:=PackageDescriptors.FindByName(PkgDescNameStandard);
end;

function PkgCompileFlagsToString(Flags: TPkgCompileFlags): string;
var
  f: TPkgCompileFlag;
  s: string;
begin
  Result:='';
  for f:=Low(TPkgCompileFlag) to High(TPkgCompileFlag) do begin
    if not (f in Flags) then continue;
    WriteStr(s, f);
    if Result<>'' then
      Result:=Result+',';
    Result:=Result+s;
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


{ TPackageGraphInterface }

procedure TPackageGraphInterface.IncChangeStamp;
begin
  {$push}{$R-}  // range check off
  Inc(FChangeStamp);
  {$pop}
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

{ TLazPackageID }

constructor TPkgDependencyID.Create;
begin
  inherited Create;
end;

destructor TPkgDependencyID.Destroy;
begin
  RequiredIDEPackage:=nil;
  inherited Destroy;
end;

procedure TPkgDependencyID.Clear;
begin
  inherited Clear;
  RequiredIDEPackage:=nil;
end;

// Setters

procedure TPkgDependencyID.SetRequiredPackage(const AValue: TIDEPackage);
begin
  if FRequiredPackage=AValue then exit;
  if FRequiredPackage<>nil then
    FRequiredPackage.RemoveUsedByDependency(Self);
  FLoadPackageResult:=lprUndefined;
  FRequiredPackage:=AValue;
  if FRequiredPackage<>nil then
    FRequiredPackage.AddUsedByDependency(Self);
end;

{ TLazPackageID }

constructor TLazPackageID.Create;
begin
  inherited Create(nil);
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
    FIDAsString:=Name+' '+FIDAsString;
  FIDAsWord:=Version.AsWord;
  if FIDAsWord<>'' then
    FIDAsWord:=Name+FIDAsWord;
end;

procedure TLazPackageID.VersionChanged(Sender: TObject);
begin
  UpdateIDAsString;
end;

function TLazPackageID.GetDirectory: string;
begin
  raise Exception.Create(''); // just an ID, no file
  Result:='';
end;

procedure TLazPackageID.AssignOptions(Source: TPersistent);
var
  aSource: TLazPackageID;
begin
  if Source is TLazPackageID then
  begin
    aSource:=TLazPackageID(Source);
    FVersion.Assign(aSource.Version);
    Name:=aSource.Name;
    UpdateIDAsString;
  end else
    raise Exception.Create('TLazPackageID.AssignOptions: can not copy from '+DbgSName(Source));
end;

function TLazPackageID.StringToID(const s: string): boolean;
var
  IdentEndPos: PChar;
  StartPos: PChar;

  function ReadIdentifier: boolean;
  begin
    Result:=false;
    while IdentEndPos^ in ['a'..'z','A'..'Z','0'..'9','_'] do begin
      inc(IdentEndPos);
      Result:=true;
    end;
  end;

begin
  Result:=false;
  if s='' then exit;
  IdentEndPos:=PChar(s);
  repeat
    if not ReadIdentifier then exit;
    if IdentEndPos^<>'.' then break;
    inc(IdentEndPos);
  until false;
  Name:=copy(s,1,IdentEndPos-PChar(s));
  StartPos:=IdentEndPos;
  while StartPos^=' ' do inc(StartPos);
  if StartPos=IdentEndPos then begin
    Version.Clear;
    Version.Valid:=pvtNone;
  end else begin
    if not Version.ReadString(StartPos) then exit;
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

function TLazPackageID.GetIDAsString: string;
begin
  Result := FIDAsString;
end;

function TLazPackageID.GetIDAsWord: string;
begin
  Result := FIDAsWord;
end;

procedure TLazPackageID.SetName(const NewName: TComponentName);
begin
  if Name=NewName then exit;
  ChangeName(NewName);
  UpdateIDAsString;
end;

{ TIDEPackage }

procedure TIDEPackage.AssignOptions(Source: TPersistent);
var
  aSource: TIDEPackage;
begin
  inherited AssignOptions(Source);
  if Source is TIDEPackage then
  begin
    aSource:=TIDEPackage(Source);
    LazCompilerOptions.Assign(aSource.LazCompilerOptions);
    // ToDo:
    //FCustomOptions:=aSource.FCustomOptions;
  end;
end;

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
  FHandlers[HandlerType].Add(AMethod,AsLast);
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

destructor TPackageEditingInterface.Destroy;
var
  h: TPkgIntfHandlerType;
begin
  for h:=Low(FHandlers) to high(FHandlers) do
    FreeAndNil(FHandlers[h]);
  inherited Destroy;
end;

procedure TPackageEditingInterface.RemoveAllHandlersOfObject(AnObject: TObject);
var
  HandlerType: TPkgIntfHandlerType;
begin
  for HandlerType:=Low(HandlerType) to High(HandlerType) do
    FHandlers[HandlerType].RemoveAllMethodsOfObject(AnObject);
end;

procedure TPackageEditingInterface.AddHandlerOnGraphChanged(
  const OnGraphChanged: TNotifyEvent; AsLast: boolean);
begin
  AddHandler(pihtGraphChanged,TMethod(OnGraphChanged),AsLast);
end;

procedure TPackageEditingInterface.RemoveHandlerOnGraphChanged(
  const OnGraphChanged: TNotifyEvent);
begin
  RemoveHandler(pihtGraphChanged,TMethod(OnGraphChanged));
end;

procedure TPackageEditingInterface.AddHandlerOnPackageFileLoaded(
  const OnPkgLoaded: TNotifyEvent; AsLast: boolean);
begin
  AddHandler(pihtPackageFileLoaded,TMethod(OnPkgLoaded),AsLast);
end;

procedure TPackageEditingInterface.RemoveHandlerOnPackageFileLoaded(
  const OnPkgLoaded: TNotifyEvent);
begin
  RemoveHandler(pihtPackageFileLoaded,TMethod(OnPkgLoaded));
end;

{ TLazPackageFile }

procedure TLazPackageFile.SetDisableI18NForLFM(AValue: boolean);
begin
  FDisableI18NForLFM:=AValue;
end;

procedure TLazPackageFile.SetFileType(const AValue: TPkgFileType);
begin
  FFileType:=AValue;
end;

procedure TLazPackageFile.SetRemoved(const AValue: boolean);
begin
  FRemoved:=AValue;
end;

initialization
  PackageEditingInterface:=nil;

end.

