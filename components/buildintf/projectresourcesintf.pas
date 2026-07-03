{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit ProjectResourcesIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, resource,
  // LazUtils
  Laz2_XMLCfg, LazMethodList,
  // BuildIntf
  BuildStrConsts, CompOptsIntf;

type
  TProjResourceType = (
    rtLRS,   // lazarus resources
    rtRes    // fpc resources
  );

  TUserResourceType = (
    rtIcon,    // maps to RT_GROUP_ICON
    rtCursor,  // maps to RT_GROUP_CURSOR
    rtBitmap,  // maps to RT_BITMAP
    rtHTML,    // maps to RT_HTML
    rtRCData   // maps to RT_RCDATA
  );

  TProjectUserResourceInfo = record
    FileName: string;            // may contain IDE macros / be project-relative
    ResType: TUserResourceType;
    ResName: string;
  end;

  EProjectUserResourceError = class(Exception);

  TAbstractProjectResources = class;
  TAbstractProjectUserResources = class;

  { TAbstractProjectResource }

  TAbstractProjectResource = class
  private
    FModified: boolean;
    FOnModified: TNotifyEvent;
    procedure SetModified(const AValue: boolean);
  protected
    // This resource is used when reading project default options.
    FIsDefaultOption: Boolean;
  public
    constructor Create; virtual;

    procedure DoAfterBuild({%H-}AResources: TAbstractProjectResources; {%H-}AReason: TCompileReason; {%H-}SaveToTestDir: boolean); virtual;
    procedure DoBeforeBuild({%H-}AResources: TAbstractProjectResources; {%H-}AReason: TCompileReason; {%H-}SaveToTestDir: boolean); virtual;
    function UpdateResources(AResources: TAbstractProjectResources; const MainFilename: string): Boolean; virtual; abstract;
    procedure WriteToProjectFile(AConfig: TXMLConfig; const Path: String); virtual; abstract;
    procedure ReadFromProjectFile(AConfig: TXMLConfig; const Path: String); virtual; abstract;

    property Modified: boolean read FModified write SetModified;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property IsDefaultOption: Boolean read FIsDefaultOption;
  end;

  TAbstractProjectResourceClass = class of TAbstractProjectResource;

  { TAbstractProjectResources }

  TAbstractProjectResources = class
  private
    FResourceType: TProjResourceType;
  protected
    FMessages: TStringList;
    procedure SetResourceType(const AValue: TProjResourceType); virtual;
    function GetProjectResource(AIndex: TAbstractProjectResourceClass): TAbstractProjectResource; virtual; abstract;
    function GetUserResources: TAbstractProjectUserResources; virtual; abstract;
    class function GetRegisteredResources: TList;
  public
    constructor Create; virtual;
    // Deprecated in Lazarus 4.99, March 2026
    constructor Create(AProject: TObject); virtual; deprecated 'Call Create without parameters';
    destructor Destroy; override;

    procedure AddSystemResource(AResource: TAbstractResource); virtual; abstract;
    procedure AddLazarusResource(AResource: TStream;
                   const AResourceName, AResourceType: String); virtual; abstract;

    property Messages: TStringList read FMessages;
    property ResourceType: TProjResourceType read FResourceType write SetResourceType;
    property Resource[AIndex: TAbstractProjectResourceClass]: TAbstractProjectResource
                                                      read GetProjectResource; default;
    // Public access to the project's user resource files (list/add/remove/rename).
    property UserResources: TAbstractProjectUserResources read GetUserResources;
  end;

  { TAbstractProjectUserResources
    Public interface to the project's user resource *files* - the list edited
    under Project Options > Resources. Designtime packages reach it via
    LazarusIDE.ActiveProject.Resources.UserResources. }
  TAbstractProjectUserResources = class(TAbstractProjectResource)
  private
    FUpdateCount: integer;
    FChangedPending: boolean;
    FChangeHandlers: TMethodList;
    procedure DoChanged;
  protected
    function GetCount: integer; virtual; abstract;
    function GetInfo(AIndex: integer): TProjectUserResourceInfo; virtual; abstract;
    // Descendants call these after mutating the list.
    procedure Changed;
    // Raise EProjectUserResourceError on a duplicate file name or resource name.
    procedure CheckCanAdd(const AFileName, AResName: string);
    procedure CheckCanRename(AIndex: integer; const ANewResName: string);
  public
    constructor Create; override;
    destructor Destroy; override;

    // Batch several edits, so observers are notified only once at EndUpdate.
    procedure BeginUpdate;
    procedure EndUpdate;

    // Observers (e.g. the open Project Options dialog).
    procedure AddChangeHandler(const AHandler: TNotifyEvent);
    procedure RemoveChangeHandler(const AHandler: TNotifyEvent);

    // Enumerate.
    property Count: integer read GetCount;
    property Items[AIndex: integer]: TProjectUserResourceInfo read GetInfo; default;
    function IndexOfFileName(const AFileName: string): integer; virtual; abstract;
    function IndexOfResName(const AResName: string): integer; virtual; abstract;
    // Resolve item AIndex's stored file name (which may be project-relative or
    // contain IDE macros) to an absolute path, mirroring how the file is located
    // at build time. The user resources always belong to the active project,
    // whose directory is used to expand relative names.
    function GetRealFileName(AIndex: integer): string; virtual; abstract;

    // Add / remove.
    function AddFile(const AFileName: string; AResType: TUserResourceType;
                     const AResName: string): integer; virtual; abstract;
    procedure Delete(AIndex: integer); virtual; abstract;
    function RemoveFile(const AFileName: string): boolean; virtual; abstract;

    // Modify.
    procedure SetFileName(AIndex: integer; const ANewFileName: string); virtual; abstract;
    procedure SetResName(AIndex: integer; const ANewResName: string); virtual; abstract;
    procedure SetResType(AIndex: integer; ANewResType: TUserResourceType); virtual; abstract;
  end;

const
  ResourceTypeToStr: array[TUserResourceType] of String = (
  'ICON',
  'CURSOR',
  'BITMAP',
  'HTML',
  'RCDATA'
  );

function StrToResourceType(const AStr: String): TUserResourceType;

procedure RegisterProjectResource(AResource: TAbstractProjectResourceClass);

implementation

var
  FRegisteredProjectResources: TList = nil;

function StrToResourceType(const AStr: String): TUserResourceType;
begin
  case AStr of
    'ICON': Result := rtIcon;
    'CURSOR': Result := rtCursor;
    'BITMAP': Result := rtBitmap;
    'HTML': Result := rtHTML;
  else
    Result := rtRCData;
  end;
end;

procedure RegisterProjectResource(AResource: TAbstractProjectResourceClass);
begin
  if FRegisteredProjectResources = nil then
    FRegisteredProjectResources := TList.Create;
  FRegisteredProjectResources.Add(AResource);
end;

{ TAbstractProjectResource }

procedure TAbstractProjectResource.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
  if Assigned(OnModified) then OnModified(Self);
end;

constructor TAbstractProjectResource.Create;
begin
  FModified := False;
end;

procedure TAbstractProjectResource.DoAfterBuild(AResources: TAbstractProjectResources; 
  AReason: TCompileReason; SaveToTestDir: boolean);
begin
  // nothing
end;

procedure TAbstractProjectResource.DoBeforeBuild(AResources: TAbstractProjectResources; 
  AReason: TCompileReason; SaveToTestDir: boolean);
begin
  // nothing
end;

{ TAbstractProjectResources }

procedure TAbstractProjectResources.SetResourceType(const AValue: TProjResourceType);
begin
  FResourceType := AValue;
end;

class function TAbstractProjectResources.GetRegisteredResources: TList;
begin
  Result := FRegisteredProjectResources;
end;

constructor TAbstractProjectResources.Create;
begin
  inherited Create;
  FMessages := TStringList.Create;
end;

constructor TAbstractProjectResources.Create(AProject: TObject);
begin
  Create;
end;

destructor TAbstractProjectResources.Destroy;
begin
  FreeAndNil(FMessages);
  inherited Destroy;
end;

{ TAbstractProjectUserResources }

constructor TAbstractProjectUserResources.Create;
begin
  inherited Create;
  FChangeHandlers := TMethodList.Create;
end;

destructor TAbstractProjectUserResources.Destroy;
begin
  FreeAndNil(FChangeHandlers);
  inherited Destroy;
end;

procedure TAbstractProjectUserResources.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TAbstractProjectUserResources.EndUpdate;
begin
  if FUpdateCount = 0 then
    raise EProjectUserResourceError.Create('EndUpdate without BeginUpdate');
  Dec(FUpdateCount);
  if (FUpdateCount = 0) and FChangedPending then
    DoChanged;
end;

procedure TAbstractProjectUserResources.Changed;
begin
  FChangedPending := True;
  if FUpdateCount = 0 then
    DoChanged;
end;

procedure TAbstractProjectUserResources.DoChanged;
begin
  FChangedPending := False;
  Modified := True;                        // -> OnModified -> project marked dirty
  FChangeHandlers.CallNotifyEvents(Self);  // -> live observers (options dialog)
end;

procedure TAbstractProjectUserResources.AddChangeHandler(const AHandler: TNotifyEvent);
begin
  FChangeHandlers.Add(TMethod(AHandler));
end;

procedure TAbstractProjectUserResources.RemoveChangeHandler(const AHandler: TNotifyEvent);
begin
  FChangeHandlers.Remove(TMethod(AHandler));
end;

procedure TAbstractProjectUserResources.CheckCanAdd(const AFileName, AResName: string);
begin
  if IndexOfFileName(AFileName) >= 0 then
    raise EProjectUserResourceError.CreateFmt(lisResFileAlreadyExists, [AFileName]);
  if IndexOfResName(AResName) >= 0 then
    raise EProjectUserResourceError.CreateFmt(lisResNameAlreadyExists, [AResName]);
end;

procedure TAbstractProjectUserResources.CheckCanRename(AIndex: integer;
  const ANewResName: string);
var
  j: integer;
begin
  j := IndexOfResName(ANewResName);
  if (j >= 0) and (j <> AIndex) then
    raise EProjectUserResourceError.CreateFmt(lisResNameAlreadyExists, [ANewResName]);
end;

finalization
  FRegisteredProjectResources.Free;
end.
