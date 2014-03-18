{ $Id$ }
{                        ----------------------------------------
                           Debugger.pp  -  Debugger base classes
                         ----------------------------------------

 @created(Wed Feb 25st WET 2001)
 @author(Marc Weustink <marc@@dommelstein.net>)

 This unit contains the base class definitions of the debugger. These
 classes are only definitions. Implemented debuggers should be
 derived from these.

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
}
unit Debugger;

{$mode objfpc}{$H+}

{$IFDEF linux} {$DEFINE DBG_ENABLE_TERMINAL} {$ENDIF}

interface

uses
  TypInfo, Classes, SysUtils, Laz2_XMLCfg, math, FileUtil, LazLoggerBase,
  LCLProc, LazConfigStorage, LazClasses, maps,
  DbgIntfBaseTypes, DbgIntfMiscClasses, DbgIntfDebuggerBase;

const
  XMLBreakPointsNode = 'BreakPoints';
  XMLBreakPointGroupsNode = 'BreakPointGroups';
  XMLWatchesNode = 'Watches';
  XMLExceptionsNode = 'Exceptions';

type

  { TDebuggerConfigStore }
  (* TODO: maybe revert relations. Create this in Debugger, and call environmentoptions for the configstore only? *)

  { TDebuggerConfigStoreBase }

  TDebuggerConfigStoreBase = class(TPersistent)
  private
    FConfigStore: TConfigStorage;
  public
    property ConfigStore: TConfigStorage read FConfigStore write FConfigStore;
    procedure Init; virtual;
    procedure Load; virtual;
    procedure Save; virtual;
  end;

  { TDebuggerWatchesDlgConfig }

  TDebuggerWatchesDlgConfig = class(TDebuggerConfigStoreBase)
  private
    FColumnNameWidth: Integer;
    FColumnValueWidth: Integer;
  public
    constructor Create;
    procedure Init; override;
  published
    property ColumnNameWidth: Integer read FColumnNameWidth write FColumnNameWidth;
    property ColumnValueWidth: Integer read FColumnValueWidth write FColumnValueWidth;
  end;

  TDebuggerConfigStore = class(TDebuggerConfigStoreBase)
  private
    FDebuggerClass: String;
    FTDebuggerWatchesDlgConfig: TDebuggerWatchesDlgConfig;
  public
    procedure Load; override;
    procedure Save; override;
  public
    constructor Create;
    destructor Destroy; override;
    property DebuggerClass: String read FDebuggerClass write FDebuggerClass;
    property DlgWatchesConfig: TDebuggerWatchesDlgConfig read FTDebuggerWatchesDlgConfig;
  published
  end;


  TDebuggerLocationType = (dltUnknown,        // not jet looked up
                           dltUnresolvable,   // lookup failed
                           dltProject,
                           dltPackage
                          );
  TDebuggerLocationFlag =  (dlfLoadError,  // resolved but failed to load
                            dlfSearchByFunctionName
                           );
  TDebuggerLocationFlags = set of TDebuggerLocationFlag;

  { TDebuggerUnitInfo }

  TDebuggerUnitInfo = class(TRefCountedObject)
  private
    FFunctionArgs: String;
    FSrcClassName: String;
    FFileName, FDbgFullName: String;
    FFlags: TDebuggerLocationFlags;
    FFunctionName: String;
    FLocationName, FLocationOwnerName, FLocationFullFile: String;
    FLocationType: TDebuggerLocationType;
    FUnitName: String;
    function GetFileName: String;
    function GetDbgFullName: String;
    function GetLocationFullFile: String;
    function GetLocationName: String;
    function GetLocationOwnerName: String;
    function GetLocationType: TDebuggerLocationType;
    procedure SetLocationFullFile(AValue: String);
    procedure SetLocationType(AValue: TDebuggerLocationType);
  public
    constructor Create(const AFileName: String; const AFullFileName: String);
    constructor Create(const AUnitName, AClassName, AFunctionName, AFunctionArgs: String);
    function DebugText: String;
    function IsEqual(const AFileName: String; const AFullFileName: String): boolean;
    function IsEqual(const AUnitName, AClassName, AFunctionName, AFunctionArgs: String): boolean;
    function IsEqual(AnOther: TDebuggerUnitInfo): boolean;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                    const APath: string); virtual;
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                                  const APath: string); virtual;
    property FileName: String read GetFileName;
    property DbgFullName: String read GetDbgFullName;
    property LocationType: TDebuggerLocationType read GetLocationType write SetLocationType;
    property LocationOwnerName: String read GetLocationOwnerName;
    property LocationName: String read GetLocationName;
    property LocationFullFile: String read GetLocationFullFile write SetLocationFullFile;
    property Flags: TDebuggerLocationFlags read FFlags write FFlags;
    property UnitName: String read FUnitName;
    property SrcClassName: String read FSrcClassName;
    property FunctionName: String read FFunctionName;
    property FunctionArgs: String read FFunctionArgs; // comma separated list of types. e.g. "integer, boolean"
  end;

  { TDebuggerUnitInfoList }

  TDebuggerUnitInfoList = class(TRefCntObjList)
  private
    function GetInfo(Index: Integer): TDebuggerUnitInfo;
    procedure PutInfo(Index: Integer; AValue: TDebuggerUnitInfo);
  public
    property Items[Index: Integer]: TDebuggerUnitInfo read GetInfo write PutInfo; default;
  end;

  { TDebuggerUnitInfoProvider }

  TDebuggerUnitInfoProvider = class
  private
    FList: TDebuggerUnitInfoList;
    FLoader: TDebuggerUnitInfo;
    function GetInfo(Index: Integer): TDebuggerUnitInfo;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetUnitInfoFor(const AFileName: String; const AFullFileName: String): TDebuggerUnitInfo;
    function GetUnitInfoByFunction(const AUnitName, AClassName, AFunctionName, AFunctionArgs: String): TDebuggerUnitInfo;
    function IndexOf(AnInfo: TDebuggerUnitInfo; AddIfNotExists: Boolean = False): Integer;
    function Count: integer;
    property Items[Index: Integer]: TDebuggerUnitInfo read GetInfo; default;
  public
    // Load/Save all entries with ID
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                    const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                                  const APath: string);
  end;

{ ---------------------------------------------------------<br>
  TDebuggerNotification is a reference counted baseclass
  for handling notifications for locals, watches, breakpoints etc.<br>
  ---------------------------------------------------------}

  TDebuggerNotification = class(TRefCountedObject)
  end;

  TDebuggerChangeNotification = class(TDebuggerNotification)
  private
    FOnChange: TNotifyEvent;
    FOnCurrent: TNotifyEvent;
  protected
    property OnCurrent: TNotifyEvent read FOnCurrent write FOnCurrent;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TDebuggerNotificationList }

  TDebuggerNotificationList = class(TObject)
  private
    FList: TList;
    function GetItem(AIndex: Integer): TDebuggerNotification;
  protected
    function NextDownIndex(var Index: integer): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ANotification: TDebuggerNotification);
    procedure Remove(const ANotification: TDebuggerNotification);
    function Count: Integer;
    procedure Clear;
    property Items[AIndex: Integer]: TDebuggerNotification read GetItem; default;
  end;

  { TDebuggerChangeNotificationList }

  TDebuggerChangeNotificationList = class(TDebuggerNotificationList)
  private
    function GetItem(AIndex: Integer): TDebuggerChangeNotification; reintroduce;
  public
    procedure NotifyChange(Sender: TObject);
    procedure NotifyCurrent(Sender: TObject);
    property Items[AIndex: Integer]: TDebuggerChangeNotification read GetItem; default;
  end;

  TIDEBreakPoints = class;
  TIDEBreakPointGroup = class;
  TIDEBreakPointGroups = class;
  TWatch = class;
  TIdeWatches = class;
  TCurrentWatch = class;
  TCurrentWatches = class;
  TIdeWatchesMonitor = class;
  TIdeLocalsMonitor = class;
  TCurrentLocals = class;
  TIDELineInfo = class;
  TCallStack = class;
  TIdeCallStackMonitor = class;
  TIdeThreadsMonitor = class;
  TSnapshotManager = class;
  TDebugger = class;

  TOnSaveFilenameToConfig = procedure(var Filename: string) of object;
  TOnLoadFilenameFromConfig = procedure(var Filename: string) of object;
  TOnGetGroupByName = function(const GroupName: string): TIDEBreakPointGroup of object;

  TNullableBool = (nbUnknown, nbTrue, nbFalse);

  { TDebuggerDataSnapShot }

  TDebuggerDataSnapShot = class
  public
    destructor Destroy; override;
  public
    DataObject: TObject;
    SnapShotId: Pointer;
  end;

  { TDebuggerDataSnapShotList }

  TDebuggerDataSnapShotList = class
  private
    FList: TList;
    function GetSnapShot(AnID: Pointer): TObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddSnapShot(AnID: Pointer; AnObject: TObject);
    procedure RemoveSnapShot(AnID: Pointer);
    property  SnapShot[AnID: Pointer]: TObject read GetSnapShot;
  end;

{$region Breakpoints **********************************************************}
(******************************************************************************)
(**                                                                          **)
(**   B R E A K P O I N T S                                                  **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

  { TIDEBreakPoint }

  // The TBaseBreakpoint family is the common ancestor for the "public" available
  // TIDEBreakPoint through the DebugBoss as well as the "private" TDBGBreakPoint
  // used by the debugboss itself.
  // The BreakPointGroups are no longer part of the debugger, but they are now
  // managed by the debugboss.

  TIDEBreakPointAction = (
    bpaStop,
    bpaEnableGroup,
    bpaDisableGroup,
    bpaLogMessage,
    bpaEValExpression,
    bpaLogCallStack,
    bpaTakeSnapshot
    );
  TIDEBreakPointActions = set of TIDEBreakPointAction;

  TIDEBreakPoint = class;

  { TIDEBreakPointGroupList }

  TIDEBreakPointGroupList = class
  private
    FList: TFPList;
    FOwner: TIDEBreakPoint;
    function GetItem(AIndex: Integer): TIDEBreakPointGroup;
  public
    constructor Create(AOwner: TIDEBreakPoint);
    destructor Destroy; override;
    procedure Assign(ASrc: TIDEBreakPointGroupList);
    procedure Clear;
    function  Add(const AGroup: TIDEBreakPointGroup): Integer;
    procedure Remove(const AGroup: TIDEBreakPointGroup);
    function  IndexOf(const AGroup: TIDEBreakPointGroup): Integer;
    function  Count: Integer;
    property Items[AIndex: Integer]: TIDEBreakPointGroup read GetItem; default;
  end;

  TIDEBreakPoint = class(TBaseBreakPoint)
  private
    FLogEvalExpression: String;
    FMaster: TDBGBreakPoint;
    FAutoContinueTime: Cardinal;
    FActions: TIDEBreakPointActions;
    FDisableGroupList: TIDEBreakPointGroupList;
    FEnableGroupList: TIDEBreakPointGroupList;
    FGroup: TIDEBreakPointGroup;
    FLoading: Boolean;
    FLogMessage: String;
    FLogCallStackLimit: Integer;
    FUserModified: Boolean;
  protected
    procedure AssignLocationTo(Dest: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoChanged; override;
    procedure DoUserChanged;  // User changed settings
    function GetHitCount: Integer; override;
    function GetValid: TValidState; override;
    procedure SetBreakHitCount(const AValue: Integer); override;
    procedure SetEnabled(const AValue: Boolean); override;
    procedure SetInitialEnabled(const AValue: Boolean); override;
    procedure SetExpression(const AValue: String); override;
    function  DebugExeLine: Integer; virtual;  // Same as line, but in Subclass: the line in the compiled exe

    procedure DisableGroups;
    procedure DoActionChange; virtual;
    procedure DoHit(const ACount: Integer; var AContinue: Boolean); override;
    procedure EnableGroups;
    procedure ClearAllGroupLists;
    {$IFDEF DBG_BREAKPOINT}
    function  DebugText: string;
    {$ENDIF}
  protected
    // virtual properties
    function GetActions: TIDEBreakPointActions; virtual;
    function GetGroup: TIDEBreakPointGroup; virtual;
    function GetAutoContinueTime: Cardinal; virtual;
    function GetLogMessage: String; virtual;
    function GetLogCallStackLimit: Integer;
    procedure SetActions(const AValue: TIDEBreakPointActions); virtual;
    procedure SetGroup(const AValue: TIDEBreakPointGroup); virtual;
    procedure SetAutoContinueTime(const AValue: Cardinal); virtual;
    procedure SetLogEvalExpression(AValue: String);
    procedure SetLogMessage(const AValue: String); virtual;
    procedure SetLogCallStackLimit(const AValue: Integer);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                      const OnLoadFilename: TOnLoadFilenameFromConfig;
                      const OnGetGroup: TOnGetGroupByName); virtual;
    procedure SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string;
                      const OnSaveFilename: TOnSaveFilenameToConfig); virtual;
    procedure SetAddress(const AValue: TDBGPtr); override;
    procedure SetLocation(const ASource: String; const ALine: Integer); override;
    procedure SetWatch(const AData: String; const AScope: TDBGWatchPointScope;
                       const AKind: TDBGWatchPointKind); override;
    procedure ResetMaster;
    property UserModified: Boolean read FUserModified write FUserModified; // Indicator for DoChanged
  public
    property Actions: TIDEBreakPointActions read GetActions write SetActions;
    property AutoContinueTime: Cardinal read GetAutoContinueTime write SetAutoContinueTime;
    property Group: TIDEBreakPointGroup read GetGroup write SetGroup;
    property DisableGroupList: TIDEBreakPointGroupList read FDisableGroupList;
    property EnableGroupList: TIDEBreakPointGroupList read FEnableGroupList;
    property LogEvalExpression: String read FLogEvalExpression write SetLogEvalExpression;
    property Loading: Boolean read FLoading;
    property LogMessage: String read GetLogMessage write SetLogMessage;
    property LogCallStackLimit: Integer read GetLogCallStackLimit write SetLogCallStackLimit;
  end;
  TIDEBreakPointClass = class of TIDEBreakPoint;

  { TIDEBreakPoints }

  TIDEBreakPointsEvent = procedure(const ASender: TIDEBreakPoints;
                                   const ABreakpoint: TIDEBreakPoint) of object;

  TIDEBreakPointsNotification = class(TDebuggerNotification)
  private
    FOnAdd:    TIDEBreakPointsEvent;
    FOnUpdate: TIDEBreakPointsEvent;//Item will be nil in case all items need to be updated
    FOnRemove: TIDEBreakPointsEvent;
  public
    property OnAdd:    TIDEBreakPointsEvent read FOnAdd    write FOnAdd;
    property OnUpdate: TIDEBreakPointsEvent read FOnUpdate write FOnUpdate;
    property OnRemove: TIDEBreakPointsEvent read FOnRemove write FonRemove;
  end;

  TIDEBreakPoints = class(TBaseBreakPoints)
  private
    FNotificationList: TList;
    FMaster: TDBGBreakPoints;
    procedure SetMaster(const AValue: TDBGBreakPoints);
    function GetItem(const AnIndex: Integer): TIDEBreakPoint;
    procedure SetItem(const AnIndex: Integer; const AValue: TIDEBreakPoint);
  protected
    procedure NotifyAdd(const ABreakPoint: TIDEBreakPoint); virtual;    // called when a breakpoint is added
    procedure NotifyRemove(const ABreakpoint: TIDEBreakPoint); virtual; // called by breakpoint when destructed
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(const ABreakPointClass: TIDEBreakPointClass);
    destructor Destroy; override;
    function Add(const ASource: String; const ALine: Integer): TIDEBreakPoint; overload;
    function Add(const AAddress: TDBGPtr): TIDEBreakPoint; overload;
    function Add(const AData: String; const AScope: TDBGWatchPointScope;
                 const AKind: TDBGWatchPointKind): TIDEBreakPoint; overload;
    function Find(const ASource: String; const ALine: Integer): TIDEBreakPoint; overload;
    function Find(const ASource: String; const ALine: Integer; const AIgnore: TIDEBreakPoint): TIDEBreakPoint; overload;
    function Find(const AAddress: TDBGPtr): TIDEBreakPoint; overload;
    function Find(const AAddress: TDBGPtr; const AIgnore: TIDEBreakPoint): TIDEBreakPoint; overload;
    function Find(const AData: String; const AScope: TDBGWatchPointScope;
                  const AKind: TDBGWatchPointKind): TIDEBreakPoint; overload;
    function Find(const AData: String; const AScope: TDBGWatchPointScope;
                  const AKind: TDBGWatchPointKind; const AIgnore: TIDEBreakPoint): TIDEBreakPoint; overload;
    procedure AddNotification(const ANotification: TIDEBreakPointsNotification);
    procedure RemoveNotification(const ANotification: TIDEBreakPointsNotification);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                      const OnLoadFilename: TOnLoadFilenameFromConfig;
                      const OnGetGroup: TOnGetGroupByName); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                      const OnSaveFilename: TOnSaveFilenameToConfig); virtual;
    property Master: TDBGBreakPoints read FMaster write SetMaster;
  public
    property Items[const AnIndex: Integer]: TIDEBreakPoint read GetItem
                                                         write SetItem; default;
  end;


  { TIDEBreakPointGroup }

  TIDEBreakPointGroup = class(TCollectionItem)
  private
    FEnabled: Boolean;
    FInitialEnabled: Boolean;
    FName: String;
    FBreakpoints: TList;// A list of breakpoints that member
    FReferences: TList; // A list of breakpoints that refer to us through En/disable group
    function GetBreakpoint(const AIndex: Integer): TIDEBreakPoint;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetInitialEnabled(const AValue: Boolean);
    procedure SetName(const AValue: String);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AddReference(const ABreakPointList: TIDEBreakPointGroupList);
    procedure RemoveReference(const ABreakPointList: TIDEBreakPointGroupList);
  public
    function Add(const ABreakPoint: TIDEBreakPoint): Integer;
    function Count: Integer;
    constructor Create(ACollection: TCollection); override;
    procedure Delete(const AIndex: Integer);
    destructor Destroy; override;
    function Remove(const ABreakPoint: TIDEBreakPoint): Integer;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig;
                                const Path: string); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig;
                              const Path: string); virtual;
    class function CheckName(const AName: String): Boolean;
  public
    property Breakpoints[const AIndex: Integer]: TIDEBreakPoint read GetBreakpoint;
    property Enabled: Boolean read FEnabled write SetEnabled;
    //property InitialEnabled: Boolean read FInitialEnabled write SetInitialEnabled;
    property Name: String read FName write SetName;
  end;


  { TIDEBreakPointGroups }

  TIDEBreakPointGroups = class(TCollection)
  private
    function GetItem(const AnIndex: Integer): TIDEBreakPointGroup;
    procedure SetItem(const AnIndex: Integer; const AValue: TIDEBreakPointGroup);
  protected
  public
    constructor Create;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig;
                                const Path: string); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig;
                              const Path: string); virtual;
    function GetGroupByName(const GroupName: string): TIDEBreakPointGroup;
    function FindGroupByName(const GroupName: string;
                             Ignore: TIDEBreakPointGroup): TIDEBreakPointGroup;
    function IndexOfGroupWithName(const GroupName: string;
                                  Ignore : TIDEBreakPointGroup): integer;
    procedure InitTargetStart; virtual;
//    procedure Regroup(SrcGroups: TIDEBreakPointGroups;
//                      SrcBreakPoints, DestBreakPoints: TIDEBreakPoints);
  public
    property Items[const AnIndex: Integer]: TIDEBreakPointGroup
                                            read GetItem write SetItem; default;
  end;

{%endregion   ^^^^^  Breakpoints  ^^^^^   }

{%region Watches **************************************************************
 ******************************************************************************
 **                                                                          **
 **   W A T C H E S                                                          **
 **                                                                          **
 ******************************************************************************
 ******************************************************************************}

const
  TWatchDisplayFormatNames: array [TWatchDisplayFormat] of string =
    ('wdfDefault',
     'wdfStructure',
     'wdfChar', 'wdfString',
     'wdfDecimal', 'wdfUnsigned', 'wdfFloat', 'wdfHex',
     'wdfPointer',
     'wdfMemDump'
    );

type

  TWatchesEvent =
       procedure(const ASender: TIdeWatches; const AWatch: TWatch) of object;

  TWatchesNotification = class(TDebuggerNotification)
  private
    FOnAdd:    TWatchesEvent;
    FOnUpdate: TWatchesEvent;//Item will be nil in case all items need to be updated
    FOnRemove: TWatchesEvent;
  public
    property OnAdd:    TWatchesEvent read FOnAdd    write FOnAdd;
    property OnUpdate: TWatchesEvent read FOnUpdate write FOnUpdate;
    property OnRemove: TWatchesEvent read FOnRemove write FonRemove;
  end;

  { TWatchesNotificationList }

  TWatchesNotificationList = class(TDebuggerNotificationList)
  private
    function GetItem(AIndex: Integer): TWatchesNotification;
  public
    procedure NotifyAdd(const ASender: TCurrentWatches; const AWatch: TCurrentWatch);
    procedure NotifyUpdate(const ASender: TCurrentWatches; const AWatch: TCurrentWatch);
    procedure NotifyRemove(const ASender: TCurrentWatches; const AWatch: TCurrentWatch);
    property Items[AIndex: Integer]: TWatchesNotification read GetItem; default;
  end;

  { TWatchValue }

  TWatchValue = class(TWatchValueBase)
  private
    FWatch: TWatch;
    FDisplayFormat: TWatchDisplayFormat;
    FEvaluateFlags: TDBGEvaluateFlags;
    FRepeatCount: Integer;
    FStackFrame: Integer;
    FThreadId: Integer;

  protected
    function GetDisplayFormat: TWatchDisplayFormat; override;
    function GetEvaluateFlags: TDBGEvaluateFlags; override;
    function GetExpression: String; override;
    function GetRepeatCount: Integer; override;
    function GetStackFrame: Integer; override;
    function GetThreadId: Integer; override;
    function GetTypeInfo: TDBGType; override;
    function GetValue: String; override;
    function GetWatchBase: TWatchBase; override;

    procedure RequestData; virtual;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                              const APath: string);
  public
    constructor Create; virtual;  overload;
    constructor Create(AOwnerWatch: TWatch);  overload;
    constructor Create(AOwnerWatch: TWatch;
                       const AThreadId: Integer;
                       const AStackFrame: Integer
                      );  overload;
    procedure Assign(AnOther: TWatchValueBase); override;

    property Watch: TWatch read FWatch;
  end;

  { TWatchValueList }

  TWatchValueList = class
  private
    FList: TList;
    FWatch: TWatch;
    function GetEntry(const AThreadId: Integer; const AStackFrame: Integer): TWatchValue;
    function GetEntryByIdx(AnIndex: integer): TWatchValue;
  protected
    function CreateEntry(const {%H-}AThreadId: Integer; const {%H-}AStackFrame: Integer): TWatchValue; virtual;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                              APath: string);
  public
    procedure Assign(AnOther: TWatchValueList);
    constructor Create(AOwnerWatch: TWatch);
    destructor Destroy; override;
    procedure Add(AnEntry: TWatchValue);
    procedure Clear;
    function Count: Integer;
    property EntriesByIdx[AnIndex: integer]: TWatchValue read GetEntryByIdx;
    property Entries[const AThreadId: Integer; const AStackFrame: Integer]: TWatchValue
             read GetEntry; default;
    property Watch: TWatch read FWatch;
  end;

  { TWatch }

  TWatch = class(TWatchBase)
  private
    FEnabled: Boolean;
    FEvaluateFlags: TDBGEvaluateFlags;
    FExpression: String;
    FDisplayFormat: TWatchDisplayFormat;
    FRepeatCount: Integer;
    FValueList: TWatchValueList;
  protected
    function GetDisplayFormat: TWatchDisplayFormat; override;
    function GetEnabled: Boolean; override;
    function GetEvaluateFlags: TDBGEvaluateFlags; override;
    function GetExpression: String; override;
    function GetRepeatCount: Integer; override;
    function GetValueBase(const AThreadId: Integer; const AStackFrame: Integer): TWatchValueBase; override;
    procedure SetDisplayFormat(AValue: TWatchDisplayFormat); override;
    procedure SetEnabled(AValue: Boolean); override;
    procedure SetEvaluateFlags(AValue: TDBGEvaluateFlags); override;
    procedure SetExpression(AValue: String); override;
    procedure SetRepeatCount(AValue: Integer); override;
    function GetValue(const AThreadId: Integer; const AStackFrame: Integer): TWatchValue;
    procedure AssignTo(Dest: TPersistent); override;
    function CreateValueList: TWatchValueList; virtual;
    procedure DoModified; virtual;  // user-storable data: expression, enabled, display-format
    procedure DoEnableChange; virtual;
    procedure DoExpressionChange; virtual;
    procedure DoDisplayFormatChanged; virtual;
  protected
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                              const APath: string);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure ClearValues; override;
  public
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Expression: String read GetExpression write SetExpression;
    property DisplayFormat: TWatchDisplayFormat read GetDisplayFormat write SetDisplayFormat;
    property EvaluateFlags: TDBGEvaluateFlags read FEvaluateFlags write SetEvaluateFlags;
    property RepeatCount: Integer read FRepeatCount write SetRepeatCount;
  public
    property Values[const AThreadId: Integer; const AStackFrame: Integer]: TWatchValue
             read GetValue;
  end;

  { TIdeWatches }

  TIdeWatches = class(TWatches)
  private
    function GetItem(const AnIndex: Integer): TWatch;
    procedure SetItem(const AnIndex: Integer; const AValue: TWatch);
  protected
    function WatchClass: TBaseWatchClass; override;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                              APath: string);
  public
    function Add(const AExpression: String): TWatch;
    function Find(const AExpression: String): TWatch; override;
    property Items[const AnIndex: Integer]: TWatch read GetItem write SetItem; default;
    procedure ClearValues; override;
  end;

  { TCurrentWatchValue }

  TCurrentWatchValue = class(TWatchValue)
  private
    FSnapShot: TWatchValue;
    procedure SetSnapShot(const AValue: TWatchValue);
  protected
    procedure RequestData; override;
    procedure DoDataValidityChanged(AnOldValidity: TDebuggerDataState); override;
  public
    property SnapShot: TWatchValue read FSnapShot write SetSnapShot;
  end;

  { TCurrentWatchValueList }

  TCurrentWatchValueList = class(TWatchValueList)
  private
    FSnapShot: TWatchValueList;
    procedure SetSnapShot(const AValue: TWatchValueList);
  protected
    function CreateEntry(const AThreadId: Integer; const AStackFrame: Integer): TWatchValue; override;
    property SnapShot: TWatchValueList read FSnapShot write SetSnapShot;
  end;

  { TCurrentWatch }

  TCurrentWatch = class(TWatch)
  private
    FSnapShot: TWatch;
    procedure SetSnapShot(const AValue: TWatch);
  protected
    function CreateValueList: TWatchValueList; override;
    procedure DoChanged; override;
    procedure DoModified; override;
    procedure RequestData(AWatchValue: TCurrentWatchValue);
    property SnapShot: TWatch read FSnapShot write SetSnapShot;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure LoadFromXMLConfig(const AConfig: TXMLConfig;
                                const APath: string); virtual;
    procedure SaveToXMLConfig(const AConfig: TXMLConfig;
                              const APath: string); virtual;
  end;
  TIDEWatchClass = class of TCurrentWatch;

  { TCurrentWatches }

  TCurrentWatches = class(TIdeWatches)
  private
    FMonitor: TIdeWatchesMonitor;
    FSnapShot: TIdeWatches;
    FDestroying: Boolean;
    procedure SetSnapShot(const AValue: TIdeWatches);
    procedure WatchesChanged(Sender: TObject);
  protected
    function GetItem(const AnIndex: Integer): TCurrentWatch;
    procedure SetItem(const AnIndex: Integer; const AValue: TCurrentWatch);
  protected
    function WatchClass: TBaseWatchClass; override;
    procedure NotifyAdd(const AWatch: TCurrentWatch); virtual;    // called when a watch is added
    procedure NotifyRemove(const AWatch: TCurrentWatch); virtual; // called by watch when destructed
    procedure DoModified;
    procedure Update(Item: TCollectionItem); override;
    procedure RequestData(AWatchValue: TCurrentWatchValue);
    property SnapShot: TIdeWatches read FSnapShot write SetSnapShot;
  public
    constructor Create(AMonitor: TIdeWatchesMonitor);
    destructor Destroy; override;
    // Watch
    function Add(const AExpression: String): TCurrentWatch;
    function Find(const AExpression: String): TCurrentWatch; override;
    // IDE
    procedure LoadFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
    procedure SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string);
  public
    property Items[const AnIndex: Integer]: TCurrentWatch read GetItem
                                                      write SetItem; default;
  end;

  { TIdeWatchesMonitor }

  TIdeWatchesMonitor = class(TWatchesMonitor)
  private
    FSnapshots: TDebuggerDataSnapShotList;
    FOnModified: TNotifyEvent;
    FIgnoreModified: Integer;
    FNotificationList: TWatchesNotificationList;
    function GetCurrentWatches: TCurrentWatches;
    function GetSnapshot(AnID: Pointer): TIdeWatches;
    function  GetSupplier: TWatchesSupplier;
    procedure SetSupplier(const AValue: TWatchesSupplier);
  protected
    procedure DoStateEnterPause; override;
    procedure DoStateLeavePause; override;
    procedure DoStateLeavePauseClean; override;
    procedure DoNewSupplier; override;
    procedure DoModified; override;
    //procedure NotifyChange
    procedure NotifyAdd(const AWatches: TCurrentWatches; const AWatch: TCurrentWatch);
    procedure NotifyRemove(const AWatches: TCurrentWatches; const AWatch: TCurrentWatch);
    procedure NotifyUpdate(const AWatches: TCurrentWatches; const AWatch: TCurrentWatch);
    procedure RequestData(AWatchValue: TCurrentWatchValue);
    function CreateWatches: TWatches; override;
    function CreateSnapshot(CreateEmpty: Boolean = False): TObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotification(const ANotification: TWatchesNotification);
    procedure RemoveNotification(const ANotification: TWatchesNotification);
    procedure NewSnapshot(AnID: Pointer; CreateEmpty: Boolean = False);
    procedure RemoveSnapshot(AnID: Pointer);
    property CurrentWatches: TCurrentWatches read GetCurrentWatches;// FCurrentWatches;
    property Snapshots[AnID: Pointer]: TIdeWatches read GetSnapshot;
    property Supplier: TWatchesSupplier read GetSupplier write SetSupplier;
  public
    procedure Clear;
    procedure LoadFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
    procedure SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string);

    procedure BeginIgnoreModified;
    procedure EndIgnoreModified;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;       // user-modified / xml-storable data modified
  end;

  {%endregion   ^^^^^  Watches  ^^^^^   }

{%region Locals ***************************************************************
 ******************************************************************************
 **                                                                          **
 **   L O C A L S                                                            **
 **                                                                          **
 ******************************************************************************
 ******************************************************************************}

  TLocalsNotification = class(TDebuggerChangeNotification)
  public
    property OnChange;
  end;

  { TIDELocals }

  TIDELocals = class(TLocals)
  protected
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                              APath: string);
  public
    constructor CreateFromXMLConfig(const AConfig: TXMLConfig; APath: string);
    procedure SetDataValidity(AValidity: TDebuggerDataState); override;
  end;

  { TCurrentLocals }

  TCurrentLocals = class(TIDELocals)
  private
    FMonitor: TIdeLocalsMonitor;
    FSnapShot: TIDELocals;
    FDataValidity: TDebuggerDataState;
    procedure SetSnapShot(const AValue: TIDELocals);
  protected
    property SnapShot: TIDELocals read FSnapShot write SetSnapShot;
  public
    constructor Create(AMonitor: TIdeLocalsMonitor; AThreadId, AStackFrame: Integer);
    function Count: Integer; override;
    procedure SetDataValidity(AValidity: TDebuggerDataState); override;
  end;

  { TLocalsList }

  { TIDELocalsList }

  TIDELocalsList = class(TLocalsList)
  private
    function GetEntry(const AThreadId: Integer; const AStackFrame: Integer): TIDELocals;
    function GetEntryByIdx(const AnIndex: Integer): TIDELocals;
  protected
    function CreateEntry(AThreadId, AStackFrame: Integer): TDbgEntityValuesList; override;
    procedure DoAssign(AnOther: TDbgEntitiesThreadStackList); override;
    procedure DoAdded(AnEntry: TDbgEntityValuesList); override;

    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                              APath: string);
  public
    property EntriesByIdx[const AnIndex: Integer]: TIDELocals read GetEntryByIdx;
    property Entries[const AThreadId: Integer; const AStackFrame: Integer]: TIDELocals
             read GetEntry; default;
  end;

  { TCurrentLocalsList }

  TCurrentLocalsList = class(TIDELocalsList)
  private
    FMonitor: TIdeLocalsMonitor;
    FSnapShot: TIDELocalsList;
    procedure SetSnapShot(const AValue: TIDELocalsList);
  protected
    procedure DoCleared; override;
    procedure DoAdded(AnEntry: TDbgEntityValuesList); override;
    function CreateEntry(AThreadId, AStackFrame: Integer): TIDELocals; override;
    property SnapShot: TIDELocalsList read FSnapShot write SetSnapShot;
  public
    constructor Create(AMonitor: TIdeLocalsMonitor);
  end;

  { TIdeLocalsMonitor }

  TIdeLocalsMonitor = class(TLocalsMonitor)
  private
    FSnapshots: TDebuggerDataSnapShotList;
    FNotificationList: TDebuggerChangeNotificationList;
    function GetCurrentLocalsList: TCurrentLocalsList;
    function GetSnapshot(AnID: Pointer): TIDELocalsList;
    function GetSupplier: TLocalsSupplier;
    procedure SetSupplier(const AValue: TLocalsSupplier);
  protected
    procedure DoStateEnterPause; override;
    procedure DoStateLeavePause; override;
    procedure DoStateLeavePauseClean; override;
    procedure NotifyChange(ALocals: TCurrentLocals);
    procedure DoNewSupplier; override;
    procedure RequestData(ALocals: TCurrentLocals);
    function CreateSnapshot(CreateEmpty: Boolean = False): TObject;
    function CreateLocalsList: TLocalsList; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddNotification(const ANotification: TLocalsNotification);
    procedure RemoveNotification(const ANotification: TLocalsNotification);
    procedure NewSnapshot(AnID: Pointer; CreateEmpty: Boolean = False);
    procedure RemoveSnapshot(AnID: Pointer);
    property  CurrentLocalsList: TCurrentLocalsList read GetCurrentLocalsList;
    property  Snapshots[AnID: Pointer]: TIDELocalsList read GetSnapshot;
    property  Supplier: TLocalsSupplier read GetSupplier write SetSupplier;
  end;

  {%endregion   ^^^^^  Locals  ^^^^^   }


{%region Line Info ************************************************************
 ******************************************************************************
 **                                                                          **
 **   L I N E   I N F O                                                      **
 **                                                                          **
 ******************************************************************************
 ******************************************************************************}

  { TIDELineInfo }

  TIDELineInfoNotification = class(TDebuggerNotification)
  private
    FOnChange: TIDELineInfoEvent;
  public
    property OnChange: TIDELineInfoEvent read FOnChange write FOnChange;
  end;

  TIDELineInfo = class(TBaseLineInfo)
  private
    FNotificationList: TList;
    FMaster: TDBGLineInfo;
    procedure LineInfoChanged(const {%H-}ASender: TObject; const ASource: String);
    procedure SetMaster(const AMaster: TDBGLineInfo);
  protected
    function GetSource(const AIndex: Integer): String; override;
  protected
    procedure NotifyChange(ASource: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotification(const ANotification: TIDELineInfoNotification);
    procedure RemoveNotification(const ANotification: TIDELineInfoNotification);
    function Count: Integer; override;
    function GetAddress(const AIndex: Integer; const ALine: Integer): TDbgPtr; override;
    function GetInfo(AAdress: TDbgPtr; out ASource, ALine, AOffset: Integer): Boolean; override;
    function IndexOf(const ASource: String): integer; override;
    procedure Request(const ASource: String); override;
    procedure Cancel(const ASource: String); override;
    property Master: TDBGLineInfo read FMaster write SetMaster;
  end;

  {%endregion   ^^^^^  Line Info  ^^^^^   }

{%region Register *************************************************************
 ******************************************************************************
 **                                                                          **
 **   R E G I S T E R S                                                      **
 **                                                                          **
 ******************************************************************************
 ******************************************************************************}

  TIdeRegistersMonitor = class;

  TRegistersNotification = class(TDebuggerChangeNotification)
  public
    property OnChange;
  end;

  { TIDERegisterValue }

  TIDERegisterValue = class(TRegisterValue)
  protected
    procedure DoDataValidityChanged(AnOldValidity: TDebuggerDataState); override;
    procedure DoDisplayFormatChanged(AnOldFormat: TRegisterDisplayFormat); override;
  end;

  { TIDERegisters }

  TIDERegisters = class(TRegisters)
  protected
    function CreateEntry: TDbgEntityValue; override;
  end;

  { TCurrentIDERegisters }

  TCurrentIDERegisters = class(TIDERegisters)
  private
    FMonitor: TIdeRegistersMonitor;
  protected
    procedure DoDataValidityChanged(AnOldValidity: TDebuggerDataState); override;
  public
    constructor Create(AMonitor: TIdeRegistersMonitor; AThreadId, AStackFrame: Integer);
    function Count: Integer; override;
  end;


  TIDERegistersList = class(TRegistersList)
  private
    //function GetEntry(const AThreadId: Integer; const AStackFrame: Integer): TIDERegisters;
    //function GetEntryByIdx(const AnIndex: Integer): TIDERegisters;
  protected
    //function CreateEntry(AThreadId, AStackFrame: Integer): TDbgEntityValuesList; override; // TIDERegisters
    //procedure DoAssign(AnOther: TDbgEntitiesThreadStackList); override; // Immutable
    // XML
  public
    //property EntriesByIdx[const AnIndex: Integer]: TIDERegisters read GetEntryByIdx;
    //property Entries[const AThreadId: Integer; const AStackFrame: Integer]: TIDERegisters
    //         read GetEntry; default;
  end;

  { TCurrentIDERegistersList }

  TCurrentIDERegistersList = class(TIDERegistersList)
  private
    FMonitor: TIdeRegistersMonitor;
  protected
    procedure DoCleared; override;
    function CreateEntry(AThreadId, AStackFrame: Integer): TRegisters; override; // TIDERegisters
  public
    constructor Create(AMonitor: TIdeRegistersMonitor);
  end;

  { TIdeRegistersMonitor }

  TIdeRegistersMonitor = class(TRegistersMonitor)
  private
    FNotificationList: TDebuggerChangeNotificationList;
    FFlags: set of (rmNeedNotifyChange);
    function GetCurrentRegistersList: TCurrentIDERegistersList;
    function GetSupplier: TRegisterSupplier;
    procedure SetSupplier(const AValue: TRegisterSupplier);
  protected
    procedure DoStateEnterPause; override;
    //procedure DoStateLeavePause; override;
    procedure DoStateLeavePauseClean; override;
    procedure DoEndUpdate; override;
    procedure NotifyChange(ARegisters: TCurrentIDERegisters);
    procedure DoNewSupplier; override;
    procedure RequestData(ARegisters: TCurrentIDERegisters);
    //function CreateSnapshot(CreateEmpty: Boolean = False): TObject; override;
    function CreateRegistersList: TRegistersList; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddNotification(const ANotification: TRegistersNotification);
    procedure RemoveNotification(const ANotification: TRegistersNotification);
    property  CurrentRegistersList: TCurrentIDERegistersList read GetCurrentRegistersList;
    //property  Snapshots[AnID: Pointer]: TIDERegistersList read GetSnapshot;
    property  Supplier: TRegisterSupplier read GetSupplier write SetSupplier;
  end;

  {%endregion   ^^^^^  Register  ^^^^^   }

{%region Callstack ************************************************************
 ******************************************************************************
 **                                                                          **
 **   C A L L S T A C K                                                      **
 **                                                                          **
 ******************************************************************************
 ******************************************************************************
 * The entries for the callstack are created on demand. This way when the     *
 * first entry is needed, it isn't required to create the whole stack         *
 *                                                                            *
 * TCallStackEntry needs to stay a readonly object so its data can be shared  *
 ******************************************************************************}

  { TCallStackNotification }

  TCallStackNotification = class(TDebuggerChangeNotification)
  public
    property OnChange;
    property OnCurrent;
  end;

  { TCallStackEntry }

  TCallStackEntry = class(TCallStackEntryBase)
  private
    FOwner: TCallStack;
    FIndex: Integer;
    FAdress: TDbgPtr;
    FFunctionName: String;
    FLine: Integer;
    FArguments: TStrings;
    FUnitInfo: TDebuggerUnitInfo;
    FState: TDebuggerDataState;
    procedure SetUnitInfo(AUnitInfo: TDebuggerUnitInfo);
  protected
    // for use in TThreadEntry ONLY
    function GetThreadId: Integer; override;
    function GetThreadName: String; override;
    function GetThreadState: String; override;
    procedure SetThreadState(AValue: String); override;
    function GetUnitInfoProvider: TDebuggerUnitInfoProvider; virtual;
  protected
    function GetAddress: TDbgPtr; override;
    function GetArgumentCount: Integer; override;
    function GetArgumentName(const AnIndex: Integer): String; override;
    function GetArgumentValue(const AnIndex: Integer): String; override;
    function GetFunctionName: String; override;
    function GetIndex: Integer; override;
    function GetLine: Integer; override;
    function GetSource: String; override;
    function GetState: TDebuggerDataState; override;
    procedure SetState(AValue: TDebuggerDataState); override;

    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                    const APath: string;
                                    AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                   );
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                                  APath: string;
                                  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                 );
    procedure ClearLocation; // TODO need a way to call Changed on TCallStack or TThreads // corrently done in SetThreadState
  public
    constructor Create;
    constructor Create(const AIndex:Integer; const AnAdress: TDbgPtr;
                       const AnArguments: TStrings; const AFunctionName: String;
                       const AUnitInfo: TDebuggerUnitInfo;
                       const ALine: Integer; AState: TDebuggerDataState = ddsValid);
    constructor CreateCopy(const ASource: TCallStackEntry);
    destructor Destroy; override;
    procedure Init(const AnAdress: TDbgPtr;
                   const AnArguments: TStrings; const AFunctionName: String;
                   const AUnitName, AClassName, AProcName, AFunctionArgs: String;
                   const ALine: Integer; AState: TDebuggerDataState = ddsValid); override;
    procedure Init(const AnAdress: TDbgPtr;
                   const AnArguments: TStrings; const AFunctionName: String;
                   const FileName, FullName: String;
                   const ALine: Integer; AState: TDebuggerDataState = ddsValid); override;
    function GetFunctionWithArg: String; override;
    function IsCurrent: Boolean;
    procedure MakeCurrent;
    property Address: TDbgPtr read FAdress;
    property ArgumentCount: Integer read GetArgumentCount;
    property ArgumentNames[const AnIndex: Integer]: String read GetArgumentName;
    property ArgumentValues[const AnIndex: Integer]: String read GetArgumentValue;
    property FunctionName: String read GetFunctionName;
    property Index: Integer read FIndex;
    property Line: Integer read FLine;
    property Source: String read GetSource;
    property UnitInfo: TDebuggerUnitInfo read FUnitInfo;
    property State: TDebuggerDataState read FState write FState;
  end;

  { TCallStack }

  TCallStack = class(TCallStackBase)
  private
    FThreadId: Integer;
    FCurrent: Integer;
    FList: TList;
  protected
    function IndexError(AIndex: Integer): TCallStackEntry;

    function GetEntryBase(AIndex: Integer): TCallStackEntryBase; override;
    function  GetCurrent: Integer; override;
    procedure SetCurrent(AValue: Integer); override;
    function GetThreadId: Integer; override;
    procedure SetThreadId(AValue: Integer); override;
    function GetRawEntries: TMap; override;
    function GetNewCurrentIndex: Integer; override;

    procedure Clear; virtual;
    function  GetCount: Integer; override;
    procedure SetCount({%H-}ACount: Integer); override;
    function  GetEntry(AIndex: Integer): TCallStackEntry; virtual;
    procedure AddEntry(AnEntry: TCallStackEntry); virtual; // must be added in correct order
    procedure AssignEntriesTo(AnOther: TCallStack); virtual;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                    APath: string;
                                    AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                   );
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                                  APath: string;
                                  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                 );
  public
    procedure DoEntriesCreated; override;
    procedure DoEntriesUpdated; override;
    procedure SetCountValidity(AValidity: TDebuggerDataState); override;
    procedure SetHasAtLeastCountInfo(AValidity: TDebuggerDataState; AMinCount: Integer = - 1);
      override;
    procedure SetCurrentValidity(AValidity: TDebuggerDataState); override;
  public
    constructor Create;
    constructor CreateCopy(const ASource: TCallStack);
    destructor Destroy; override;
    procedure Assign(AnOther: TCallStack);
    procedure PrepareRange({%H-}AIndex, {%H-}ACount: Integer); override;
    procedure ChangeCurrentIndex(ANewIndex: Integer); virtual;
    function HasAtLeastCount(ARequiredMinCount: Integer): TNullableBool; virtual; // Can be faster than getting the full count
    function CountLimited(ALimit: Integer): Integer; override;
    property Count: Integer read GetCount write SetCount;
    property CurrentIndex: Integer read GetCurrent write SetCurrent;
    property Entries[AIndex: Integer]: TCallStackEntry read GetEntry;
  end;

  { TCallStackList }

  TIdeCallStackList = class(TCallStackList)
  private
    FList: TList;
    function GetEntry(const AIndex: Integer): TCallStack;
  protected
    function  GetEntryForThread(const AThreadId: Integer): TCallStack; virtual;
    function GetEntryBase(const AIndex: Integer): TCallStackBase; override;
    function GetEntryForThreadBase(const AThreadId: Integer): TCallStackBase; override;
    procedure Add(ACallStack: TCallStack);
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                    APath: string;
                                    AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                   );
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                                  APath: string;
                                  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                 );
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(AnOther: TIdeCallStackList);
    procedure Clear; override;
    function Count: Integer; override; // Count of already requested CallStacks (via ThreadId)
    property Entries[const AIndex: Integer]: TCallStack read GetEntry; default;
    property EntriesForThreads[const AThreadId: Integer]: TCallStack read GetEntryForThread;
  end;

  { TCurrentCallStack }

  TCurrentCallStack = class(TCallStack)
  private
    FMonitor: TIdeCallStackMonitor;
    FCountValidity, FAtLeastCountValidity: TDebuggerDataState;
    FCurrentValidity: TDebuggerDataState;
    FNewCurrentIndex: Integer;
    FPreparing: Boolean;
    FSnapShot: TCallStack;
    FEntries: TMap;        // list of created entries
    FCount, FAtLeastCount, FAtLeastCountOld: Integer;
    FLowestUnknown, FHighestUnknown: Integer;
    procedure SetSnapShot(const AValue: TCallStack);
  protected
    function  GetCurrent: Integer; override;
    procedure SetCurrent(AValue: Integer); override;

    procedure Clear; override;
    function  GetCount: Integer; override;
    procedure SetCount(ACount: Integer); override;
    function GetEntry(AIndex: Integer): TCallStackEntry; override;
    procedure AddEntry(AnEntry: TCallStackEntry); override;
    procedure AssignEntriesTo(AnOther: TCallStack); override;
    function GetRawEntries: TMap; override;
    function GetLowestUnknown: Integer; override;
    function GetHighestUnknown: Integer; override;
    function GetNewCurrentIndex: Integer; override;
  public
    constructor Create(AMonitor: TIdeCallStackMonitor);
    destructor Destroy; override;
    procedure Assign(AnOther: TCallStack);
    procedure PrepareRange(AIndex, ACount: Integer); override;
    procedure ChangeCurrentIndex(ANewIndex: Integer); override;
    procedure DoEntriesCreated; override;
    procedure DoEntriesUpdated; override;
    function HasAtLeastCount(ARequiredMinCount: Integer): TNullableBool; override;
    property NewCurrentIndex: Integer read FNewCurrentIndex;
    property SnapShot: TCallStack read FSnapShot write SetSnapShot;
  public
    procedure SetCountValidity(AValidity: TDebuggerDataState); override;
    procedure SetHasAtLeastCountInfo(AValidity: TDebuggerDataState; AMinCount: Integer = -1); override;
    procedure SetCurrentValidity(AValidity: TDebuggerDataState); override;
  end;

  { TCurrentCallStackList }

  TCurrentCallStackList = class(TIdeCallStackList)
  private
    FMonitor: TIdeCallStackMonitor;
    FSnapShot: TIdeCallStackList;
    procedure SetSnapShot(const AValue: TIdeCallStackList);
  protected
    function GetEntryForThread(const AThreadId: Integer): TCallStack; override;
    property SnapShot: TIdeCallStackList read FSnapShot write SetSnapShot;
  public
    constructor Create(AMonitor: TIdeCallStackMonitor);
  end;

  { TIdeCallStackMonitor }

  TIdeCallStackMonitor = class(TCallStackMonitor)
  private
    FSnapshots: TDebuggerDataSnapShotList;
    FNotificationList: TDebuggerChangeNotificationList;
    FUnitInfoProvider: TDebuggerUnitInfoProvider;
    procedure CallStackClear(Sender: TObject);
    function GetCurrentCallStackList: TCurrentCallStackList;
    function GetSnapshot(AnID: Pointer): TIdeCallStackList;
    function  GetSupplier: TCallStackSupplier;
    procedure SetSupplier(const AValue: TCallStackSupplier);
  protected
    procedure DoStateEnterPause; override;
    procedure DoStateLeavePause; override;
    procedure DoStateLeavePauseClean; override;
    procedure DoModified; override;
    procedure RequestCount(ACallstack: TCallStack);
    procedure RequestAtLeastCount(ACallstack: TCallStack; ARequiredMinCount: Integer);
    procedure RequestCurrent(ACallstack: TCallStack);
    procedure RequestEntries(ACallstack: TCallStack);
    procedure UpdateCurrentIndex;
    procedure DoNewSupplier; override;
    function  CreateSnapshot(CreateEmpty: Boolean = False): TObject;
    function CreateCallStackList: TCallStackList; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotification(const ANotification: TCallStackNotification);
    procedure RemoveNotification(const ANotification: TCallStackNotification);
    procedure NewSnapshot(AnID: Pointer; CreateEmpty: Boolean = False);
    procedure RemoveSnapshot(AnID: Pointer);
    procedure NotifyChange; // (sender)
    procedure NotifyCurrent;
    property CurrentCallStackList: TCurrentCallStackList read GetCurrentCallStackList;
    property Snapshots[AnID: Pointer]: TIdeCallStackList read GetSnapshot;
    property Supplier: TCallStackSupplier read GetSupplier write SetSupplier;
    property UnitInfoProvider: TDebuggerUnitInfoProvider                        // Provided by DebugBoss, to map files to packages or project
             read FUnitInfoProvider write FUnitInfoProvider;
  end;

  {%endregion   ^^^^^  Callstack  ^^^^^   }

{%region      *****  Disassembler  *****   }
(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   D I S A S S E M B L E R                                                **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

  { TIDEDisassemblerNotification }

  TIDEDisassemblerNotification = class(TDebuggerNotification)
  private
    FOnChange: TNotifyEvent;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TIDEDisassembler = class(TBaseDisassembler)
  private
    FNotificationList: TList;
    FMaster: TDBGDisassembler;
    procedure DisassemblerChanged(Sender: TObject);
    procedure SetMaster(AMaster: TDBGDisassembler);
  protected
    procedure DoChanged; override;
    function  InternalGetEntry(AIndex: Integer): TDisassemblerEntry; override;
    function  InternalGetEntryPtr(AIndex: Integer): PDisassemblerEntry; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotification(const ANotification: TIDEDisassemblerNotification);
    procedure RemoveNotification(const ANotification: TIDEDisassemblerNotification);
    procedure Clear; override;
    function PrepareRange(AnAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer): Boolean; override;
    property Master: TDBGDisassembler read FMaster write SetMaster;
  end;

{%endregion   ^^^^^  Disassembler  ^^^^^   }

{%region Threads **************************************************************
 ******************************************************************************
 **                                                                          **
 **   T H R E A D S                                                          **
 **                                                                          **
 ******************************************************************************
 ******************************************************************************}

  { TThreadsNotification }

  TThreadsNotification = class(TDebuggerChangeNotification)
  public
    property OnChange; // fires for all changes (incl OnCurrent)
    property OnCurrent;
  end;

  TIdeThreads = class;

  { TThreadEntry }

  TThreadEntry = class(TCallStackEntry)
  private
    FThreadOwner: TIdeThreads;
    FThreadId: Integer;
    FThreadName: String;
    FThreadState: String;
  protected
    function GetUnitInfoProvider: TDebuggerUnitInfoProvider; override;
    function GetThreadId: Integer; override;
    function GetThreadName: String; override;
    function GetThreadState: String; override;
    procedure SetThreadState(AValue: String); override;

    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                    const APath: string;
                                    AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                   ); reintroduce;
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                                  const APath: string;
                                  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                 ); reintroduce;
  public
    constructor Create(const AIndex:Integer; const AnAdress: TDbgPtr;
                       const AnArguments: TStrings; const AFunctionName: String;
                       const FileName, FullName: String;
                       const ALine: Integer;
                       const AThreadId: Integer; const AThreadName: String;
                       const AThreadState: String;
                       AState: TDebuggerDataState = ddsValid); overload;
    constructor CreateCopy(const ASource: TThreadEntry);
  end;

  { TIdeThreads }

  TIdeThreads = class(TThreads)
  private
    FCurrentThreadId: Integer;
    FList: TList;
    function GetEntry(const AnIndex: Integer): TThreadEntry;
    function GetEntryById(const AnID: Integer): TThreadEntry;
  protected
    procedure SetCurrentThreadId(AValue: Integer); override;
    function GetCurrentThreadId: Integer; override;
    function GetEntryBase(const AnIndex: Integer): TCallStackEntryBase; override;
    function GetEntryByIdBase(const AnID: Integer): TCallStackEntryBase; override;
    procedure Assign(AOther: TIdeThreads);
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                    APath: string;
                                    AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                   );
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                                  APath: string;
                                  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                 );
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer; override;
    procedure Clear; override;
    procedure Add(AThread: TCallStackEntryBase); override;
    procedure Remove(AThread: TCallStackEntryBase); override;
    function CreateEntry(const AIndex:Integer; const AnAdress: TDbgPtr;
                       const AnArguments: TStrings; const AFunctionName: String;
                       const FileName, FullName: String;
                       const ALine: Integer;
                       const AThreadId: Integer; const AThreadName: String;
                       const AThreadState: String;
                       AState: TDebuggerDataState = ddsValid): TCallStackEntryBase; override;
    procedure SetValidity(AValidity: TDebuggerDataState); override;
    property Entries[const AnIndex: Integer]: TThreadEntry read GetEntry; default;
    property EntryById[const AnID: Integer]: TThreadEntry read GetEntryById;
  end;

  { TCurrentThreads }

  TCurrentThreads = class(TIdeThreads)
  private
    FMonitor: TIdeThreadsMonitor;
    FDataValidity: TDebuggerDataState;
    FSnapShot: TIdeThreads;
    procedure SetSnapShot(const AValue: TIdeThreads);
  protected
    Paused: Boolean; // Todo: introduce Supplie.ReadyForRequest
    procedure SetCurrentThreadId(AValue: Integer); override;
    property SnapShot: TIdeThreads read FSnapShot write SetSnapShot;
  public
    constructor Create(AMonitor: TIdeThreadsMonitor);
    function  Count: Integer; override;
    procedure Clear; override;
    function CreateEntry(const AIndex:Integer; const AnAdress: TDbgPtr;
                       const AnArguments: TStrings; const AFunctionName: String;
                       const FileName, FullName: String;
                       const ALine: Integer;
                       const AThreadId: Integer; const AThreadName: String;
                       const AThreadState: String;
                       AState: TDebuggerDataState = ddsValid): TCallStackEntryBase; override;
    procedure SetValidity(AValidity: TDebuggerDataState); override;
  end;

  { TIdeThreadsMonitor }

  TIdeThreadsMonitor = class(TThreadsMonitor)
  private
    FSnapshots: TDebuggerDataSnapShotList;
    FUnitInfoProvider: TDebuggerUnitInfoProvider;
    FNotificationList: TDebuggerChangeNotificationList;
    function GetCurrentThreads: TCurrentThreads;
    function GetSnapshot(AnID: Pointer): TIdeThreads;
    function GetSupplier: TThreadsSupplier;
    procedure SetSupplier(const AValue: TThreadsSupplier);
  protected
    procedure DoModified; override;
    procedure DoStateEnterPause; override;
    procedure DoStateLeavePause; override;
    procedure DoStateLeavePauseClean; override;
    procedure DoNewSupplier; override;
    procedure Changed;
    procedure RequestData;
    function  CreateSnapshot(CreateEmpty: Boolean = False): TObject;
    function CreateThreads: TThreads; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddNotification(const ANotification: TThreadsNotification);
    procedure RemoveNotification(const ANotification: TThreadsNotification);
    procedure NewSnapshot(AnID: Pointer; CreateEmpty: Boolean = False);
    procedure RemoveSnapshot(AnID: Pointer);
    procedure ChangeCurrentThread(ANewId: Integer);
    procedure CurrentChanged;
    property  CurrentThreads: TCurrentThreads read GetCurrentThreads;
    property  Snapshots[AnID: Pointer]: TIdeThreads read GetSnapshot;
    property  Supplier: TThreadsSupplier read GetSupplier write SetSupplier;
    property UnitInfoProvider: TDebuggerUnitInfoProvider                        // Provided by DebugBoss, to map files to packages or project
             read FUnitInfoProvider write FUnitInfoProvider;
  end;

{%endregion   ^^^^^  Threads  ^^^^^   }

{%region   *****  Snapshots  *****   }

  TSnapshotNotification = class(TDebuggerChangeNotification)
  public
    property OnChange; // fires for all changes (incl OnCurrent)
    property OnCurrent;
  end;

  { TSnapshot }

  TSnapshot = class(TRefCountedObject)
  private
    FLocation: TDBGLocationRec;
    FTimeStamp: TDateTime;
    FSnapMgr: TSnapshotManager;
    function GetLocationAsText: String;
  protected
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                    APath: string;
                                    AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                   );
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                                  APath: string;
                                  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                 );
  public
    constructor Create(ASnapMgr: TSnapshotManager);
    destructor Destroy; override;
    property TimeStamp: TDateTime read FTimeStamp;
    property Location: TDBGLocationRec read FLocation write FLocation;
    property LocationAsText: String read GetLocationAsText;
  public
    procedure AddToSnapshots;
    procedure AddToHistory;
    procedure RemoveFromSnapshots;
    procedure RemoveFromHistory;
    function IsCurrent: Boolean;
    function IsHistory: Boolean;
    function IsSnapshot: Boolean;
  end;

  { TSnapshotList }

  TSnapshotList = class(TRefCntObjList)
  private
    function Get(Index: Integer): TSnapshot;
    procedure Put(Index: Integer; const AValue: TSnapshot);
  public
    property Items[Index: Integer]: TSnapshot read Get write Put; default;
  end;

  { TSnapshotManager }
  TSnapshotManagerRequestedFlags = set of
    (smrThreads, smrCallStackCnt, smrCallStack, smrLocals, smrWatches);

  TSnapshotManager = class
  private
    FDebugger: TDebuggerIntf;
    FNotificationList: TDebuggerChangeNotificationList;
    FLocals: TIdeLocalsMonitor;
    FWatches: TIdeWatchesMonitor;
    FCallStack: TIdeCallStackMonitor;
    FCallStackNotification: TCallStackNotification;
    FThreads: TIdeThreadsMonitor;
    procedure SetCallStack(AValue: TIdeCallStackMonitor);
    procedure DoCallStackChanged(Sender: TObject);
  private
    FActive: Boolean;
    FForcedIdle: Boolean;
    FUnitInfoProvider: TDebuggerUnitInfoProvider;
    FUpdateLock: Integer;
    FUpdateFlags: set of (ufSnapChanged, ufSnapCurrent, ufInDebuggerIdle);
    FCurrentState: TDBGState;
    FRequestsDone: TSnapshotManagerRequestedFlags;
    FCurrentSnapshot: TSnapshot; // snapshot for current pause. Not yet in list
    procedure SetActive(const AValue: Boolean);
    procedure SetDebugger(AValue: TDebuggerIntf);
  protected
    FHistoryCapacity: Integer;
    FHistoryIndex: Integer;
    FHistoryList: TSnapshotList;
    FHistorySelected: Boolean;
    function  GetHistoryEntry(AIndex: Integer): TSnapshot;
    procedure SetHistoryIndex(const AValue: Integer);
    procedure SetHistorySelected(AValue: Boolean);
    procedure CreateHistoryEntry;
    procedure RemoveHistoryEntry(AIndex: Integer);
    procedure RemoveHistoryEntry(ASnapShot: TSnapshot);
    procedure RemoveHistoryEntryFromMonitors(AnEntry: TSnapshot);
  protected
    FSnapshotIndex: Integer;
    FSnapshotList: TSnapshotList;
    FSnapshotSelected: Boolean;
    function  GetSnapshotEntry(AIndex: Integer): TSnapshot;
    procedure SetSnapshotIndex(const AValue: Integer);
    procedure SetSnapshotSelected(AValue: Boolean);
    procedure AddSnapshotEntry(ASnapShot: TSnapshot);
    procedure RemoveSnapshotEntry(ASnapShot: TSnapshot);
    procedure AddHistoryEntry(ASnapShot: TSnapshot);
  protected
    procedure DoSnapShotDestroy(ASnapShot: TSnapshot);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure DoChanged;
    procedure DoCurrent;
  protected
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                    APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                                  APath: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotification(const ANotification: TSnapshotNotification);
    procedure RemoveNotification(const ANotification: TSnapshotNotification);
    procedure DoStateChange(const AOldState: TDBGState);
    procedure DoDebuggerIdle(AForce: Boolean = False);
    property Active: Boolean read FActive write SetActive;
  public
    function SelectedId: Pointer;
    function SelectedEntry: TSnapshot;
    procedure Clear;
    procedure ClearHistory;
    procedure ClearSnapshots;
    function  GetAsXML: String;
    procedure SetFromXML(aXML: String);
    property Current: TSnapshot read FCurrentSnapshot;
  public
    property HistoryIndex: Integer read FHistoryIndex write SetHistoryIndex;
    property HistoryCapacity: Integer read FHistoryCapacity write FHistoryCapacity;
    property HistorySelected: Boolean read FHistorySelected write SetHistorySelected;
    property History: TSnapshotList read FHistoryList;
  public
    property SnapshotIndex: Integer read FSnapshotIndex write SetSnapshotIndex;
    property SnapshotSelected: Boolean read FSnapshotSelected write SetSnapshotSelected;
    property Snapshots: TSnapshotList read FSnapshotList;
  public
    property Locals: TIdeLocalsMonitor read FLocals write FLocals;
    property Watches: TIdeWatchesMonitor read FWatches write FWatches;
    property CallStack: TIdeCallStackMonitor read FCallStack write SetCallStack;
    property Threads: TIdeThreadsMonitor read FThreads write FThreads;
    property Debugger: TDebuggerIntf read FDebugger write SetDebugger;
    property UnitInfoProvider: TDebuggerUnitInfoProvider read FUnitInfoProvider write FUnitInfoProvider;
  end;
{%endregion   ^^^^^  Snapshots  ^^^^^   }

{%region Signals / Exceptions *************************************************}
(******************************************************************************)
(**                                                                          **)
(**   S I G N A L S  and  E X C E P T I O N S                                **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

  { TIDESignal }

  TIDESignal = class(TBaseSignal)
  private
    FMaster: TDBGSignal;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromXMLConfig(const {%H-}AXMLConfig: TXMLConfig;
                                const {%H-}APath: string);
    procedure SaveToXMLConfig(const {%H-}AXMLConfig: TXMLConfig;
                              const {%H-}APath: string);
    procedure ResetMaster;
  end;

  { TIDESignals }

  TIDESignals = class(TBaseSignals)
  private
    FMaster: TDBGSignals;
    procedure SetMaster(const AValue: TDBGSignals);
    function GetItem(const AIndex: Integer): TIDESignal;
    procedure SetItem(const AIndex: Integer; const AValue: TIDESignal);
  protected
    procedure AddDefault;
  public
    constructor Create;
    procedure Reset; override;
    function Add(const AName: String; AID: Integer): TIDESignal;
    function Find(const AName: String): TIDESignal;
    property Master: TDBGSignals read FMaster write SetMaster;
  public
    procedure LoadFromXMLConfig(const {%H-}AXMLConfig: TXMLConfig;
                                const {%H-}APath: string);
    procedure SaveToXMLConfig(const {%H-}AXMLConfig: TXMLConfig;
                              const {%H-}APath: string);
    property Items[const AIndex: Integer]: TIDESignal read GetItem
                                                      write SetItem; default;
  end;


  { TIDEException }
  TIDEException = class(TBaseException)
  private
    FMaster: TDBGException;
  public
    constructor Create(ACollection: TCollection); override;
    procedure LoadFromXMLConfig(const AXMLConfig: TXMLConfig;
                                const APath: string);
    procedure SaveToXMLConfig(const AXMLConfig: TXMLConfig;
                              const APath: string);
    procedure ResetMaster;
  end;

  { TIDEExceptions }

  TIDEExceptions = class(TBaseExceptions)
  private
    function GetItem(const AIndex: Integer): TIDEException;
    procedure SetItem(const AIndex: Integer; const AValue: TIDEException);
  protected
    procedure AddDefault;
  public
    function Add(const AName: String): TIDEException;
    function Find(const AName: String): TIDEException;
  public
    constructor Create;
    procedure LoadFromXMLConfig(const AXMLConfig: TXMLConfig;
                                const APath: string);
    procedure SaveToXMLConfig(const AXMLConfig: TXMLConfig;
                              const APath: string);
    procedure AddIfNeeded(AName: string);
    procedure Reset; override;
    property Items[const AIndex: Integer]: TIDEException read GetItem
                                                        write SetItem; default;
  end;
{%endregion   ^^^^^  Signals / Exceptions  ^^^^^   }

(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   D E B U G G E R                                                        **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

  TDBGEventRec = packed record
    case Boolean of
      False: (
       Category: Word;
       EventType: Word);
      True: (Ptr: Pointer);
  end;


  { TDebugger }

  TDebugger = class(TDebuggerIntf)
  end;

const
  DBGCommandNames: array[TDBGCommand] of string = (
    'Run',
    'Pause',
    'Stop',
    'StepOver',
    'StepInto',
    'StepOut',
    'RunTo',
    'Jumpto',
    'Attach',
    'Detach',
    'Break',
    'Watch',
    'Local',
    'Evaluate',
    'Modify',
    'Environment',
    'SetStackFrame',
    'Disassemble',
    'StepOverInstr',
    'StepIntoInstr',
    'SendConsoleInput'
    );

  DBGStateNames: array[TDBGState] of string = (
    'None',
    'Idle',
    'Stop',
    'Pause',
    'InternalPause',
    'Init',
    'Run',
    'Error',
    'Destroying'
    );

  DBGBreakPointActionNames: array[TIDEBreakPointAction] of string = (
    'Stop',
    'EnableGroup',
    'DisableGroup',
    'LogMessage',
    'EvalExpression',
    'LogCallStack',
    'TakeSnapshot'
    );

function DBGCommandNameToCommand(const s: string): TDBGCommand;
function DBGStateNameToState(const s: string): TDBGState; deprecated;
function DBGBreakPointActionNameToAction(const s: string): TIDEBreakPointAction;

function dbgs(AFlag: TDebuggerLocationFlag): String; overload;
function dbgs(AFlags: TDebuggerLocationFlags): String; overload;

function HasConsoleSupport: Boolean;
(******************************************************************************)
(******************************************************************************)
(******************************************************************************)
(******************************************************************************)

implementation

var
  DBG_DATA_MONITORS, DBG_LOCATION_INFO: PLazLoggerLogGroup;

function dbgs(AFlag: TDebuggerLocationFlag): String;
begin
  writestr(Result{%H-}, AFlag);
end;

function dbgs(AFlags: TDebuggerLocationFlags): String;
var
  i: TDebuggerLocationFlag;
begin
  Result:='';
  for i := low(TDebuggerLocationFlags) to high(TDebuggerLocationFlags) do
    if i in AFlags then begin
      if Result <> '' then Result := Result + ', ';
      Result := Result + dbgs(i);
    end;
  if Result <> '' then Result := '[' + Result + ']';
end;

function HasConsoleSupport: Boolean;
begin
  {$IFDEF DBG_ENABLE_TERMINAL}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function DBGCommandNameToCommand(const s: string): TDBGCommand;
begin
  for Result:=Low(TDBGCommand) to High(TDBGCommand) do
    if AnsiCompareText(s,DBGCommandNames[Result])=0 then exit;
  Result:=dcStop;
end;

function DBGStateNameToState(const s: string): TDBGState;
begin
  for Result:=Low(TDBGState) to High(TDBGState) do
    if AnsiCompareText(s,DBGStateNames[Result])=0 then exit;
  Result:=dsNone;
end;

function DBGBreakPointActionNameToAction(const s: string): TIDEBreakPointAction;
begin
  for Result:=Low(TIDEBreakPointAction) to High(TIDEBreakPointAction) do
    if AnsiCompareText(s,DBGBreakPointActionNames[Result])=0 then exit;
  Result:=bpaStop;
end;

{ TIDEBreakPointGroupList }

function TIDEBreakPointGroupList.GetItem(AIndex: Integer): TIDEBreakPointGroup;
begin
  Result := TIDEBreakPointGroup(FList[AIndex]);
end;

constructor TIDEBreakPointGroupList.Create(AOwner: TIDEBreakPoint);
begin
  FList := TFPList.Create;
  FOwner := AOwner;
end;

destructor TIDEBreakPointGroupList.Destroy;
begin
  inherited Destroy;
  FList.Free;
end;

procedure TIDEBreakPointGroupList.Assign(ASrc: TIDEBreakPointGroupList);
var
  i: Integer;
begin
  Clear;
  for i := 0 to ASrc.Count - 1 do
    Add(ASrc[i]);
end;

procedure TIDEBreakPointGroupList.Clear;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    Items[i].RemoveReference(Self);
  FList.Clear;
end;

function TIDEBreakPointGroupList.Add(const AGroup: TIDEBreakPointGroup): Integer;
begin
  if (AGroup = nil) or (IndexOf(AGroup) >= 0) then exit;
  Result := FList.Add(AGroup);
  AGroup.AddReference(Self);
  FOwner.DoChanged;
end;

procedure TIDEBreakPointGroupList.Remove(const AGroup: TIDEBreakPointGroup);
begin
  if (AGroup = nil) then exit;
  AGroup.RemoveReference(Self);
  if (IndexOf(AGroup) < 0) then exit;
  FList.Remove(AGroup);
  FOwner.DoChanged;
end;

function TIDEBreakPointGroupList.IndexOf(const AGroup: TIDEBreakPointGroup): Integer;
begin
  Result := FList.IndexOf(AGroup);
end;

function TIDEBreakPointGroupList.Count: Integer;
begin
  Result := FList.Count;
end;

{ TDebuggerWatchesDlgConfig }

constructor TDebuggerWatchesDlgConfig.Create;
begin
  Init;
end;

procedure TDebuggerWatchesDlgConfig.Init;
begin
  FColumnNameWidth := -1;
  FColumnValueWidth := -1;
end;

{ TDebuggerConfigStoreBase }

procedure TDebuggerConfigStoreBase.Init;
begin
  //
end;

procedure TDebuggerConfigStoreBase.Load;
begin
  Init;
  ConfigStore.ReadObject('', self);
end;

procedure TDebuggerConfigStoreBase.Save;
begin
  ConfigStore.WriteObject('', self);
end;

{ TDebuggerConfigStore }

procedure TDebuggerConfigStore.Load;
const
  OLD_GDB_DBG_NAME = 'GNU debugger (gdb)';
  OLD_SSH_DBG_NAME = 'GNU debugger through SSH (gdb)';
var
  s: String;
begin
  inherited;
  FDebuggerClass := ConfigStore.GetValue('Class', '');
  if FDebuggerClass='' then begin
    // try old format
    s := ConfigStore.GetValue('Type', '');
    if s = OLD_GDB_DBG_NAME then FDebuggerClass:='TGDBMIDEBUGGER';
    if s = OLD_SSH_DBG_NAME then FDebuggerClass:='TSSHGDBMIDEBUGGER';
  end;
  ConfigStore.AppendBasePath('WatchesDlg/');
  try
    FTDebuggerWatchesDlgConfig.ConfigStore := ConfigStore;
    FTDebuggerWatchesDlgConfig.Load;
  finally
    ConfigStore.UndoAppendBasePath;
  end;
end;

procedure TDebuggerConfigStore.Save;
begin
  inherited;
  ConfigStore.SetDeleteValue('Class', FDebuggerClass, '');
  ConfigStore.DeletePath('Type');
  ConfigStore.AppendBasePath('WatchesDlg/');
  try
    FTDebuggerWatchesDlgConfig.ConfigStore := ConfigStore;
    FTDebuggerWatchesDlgConfig.Save;
  finally
    ConfigStore.UndoAppendBasePath;
  end;
end;

constructor TDebuggerConfigStore.Create;
begin
  FTDebuggerWatchesDlgConfig := TDebuggerWatchesDlgConfig.Create;
end;

destructor TDebuggerConfigStore.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FTDebuggerWatchesDlgConfig);
end;

{ TDebuggerUnitInfoProvider }

function TDebuggerUnitInfoProvider.GetInfo(Index: Integer): TDebuggerUnitInfo;
begin
  Result := FList.Items[Index];
end;

constructor TDebuggerUnitInfoProvider.Create;
begin
  FList := TDebuggerUnitInfoList.Create;
  FLoader := TDebuggerUnitInfo.Create('', '');
end;

destructor TDebuggerUnitInfoProvider.Destroy;
begin
  FList.Clear;
  inherited Destroy;
  FreeAndNil(FLoader);
  FreeAndNil(FList);
end;

procedure TDebuggerUnitInfoProvider.Clear;
begin
  FList.Clear;
end;

function TDebuggerUnitInfoProvider.GetUnitInfoFor(const AFileName: String;
  const AFullFileName: String): TDebuggerUnitInfo;
var
  i: Integer;
begin
  i := FList.Count - 1;
  while i >= 0 do begin
    if (not(dlfSearchByFunctionName in FList[i].Flags)) and
       FList[i].IsEqual(AFileName, AFullFileName)
    then begin
      debugln(DBG_LOCATION_INFO, ['TDebuggerLocationProvider.GetLocationInfoFor  Found entry for: ', AFileName, ' / ', AFullFileName]);
      exit(FList[i]);
    end;
    dec(i);
  end;
  Result := TDebuggerUnitInfo.Create(AFileName, AFullFileName);
  FList.Add(Result);
  debugln(DBG_LOCATION_INFO, ['TDebuggerLocationProvider.GetLocationInfoFor  Created new entry (Cnt=',FList.Count,') for: ', AFileName, ' / ', AFullFileName]);
end;

function TDebuggerUnitInfoProvider.GetUnitInfoByFunction(const AUnitName,
  AClassName, AFunctionName, AFunctionArgs: String): TDebuggerUnitInfo;
var
  i: Integer;
begin
  i := FList.Count - 1;
  while i >= 0 do begin
    if (dlfSearchByFunctionName in FList[i].Flags) and
       FList[i].IsEqual(AUnitName, AClassName, AFunctionName, AFunctionArgs)
    then begin
      debugln(DBG_LOCATION_INFO, ['TDebuggerLocationProvider.GetLocationInfoFor  Found entry for: ', AUnitName, ' / ', AClassName, ' / ', AFunctionName]);
      exit(FList[i]);
    end;
    dec(i);
  end;
  Result := TDebuggerUnitInfo.Create(AUnitName, AClassName, AFunctionName, AFunctionArgs);
  FList.Add(Result);
  debugln(DBG_LOCATION_INFO, ['TDebuggerLocationProvider.GetLocationInfoFor  Created new entry (Cnt=',FList.Count,') for: ', AUnitName, ' / ', AClassName, ' / ', AFunctionName]);
end;

function TDebuggerUnitInfoProvider.IndexOf(AnInfo: TDebuggerUnitInfo;
  AddIfNotExists: Boolean): Integer;
begin
  Result := FList.Count - 1;
  while Result >= 0 do begin
    if FList[Result].IsEqual(AnInfo) then begin
      exit;
    end;
    dec(Result);
  end;
  if AddIfNotExists then
    Result := FList.Add(AnInfo);
end;

function TDebuggerUnitInfoProvider.Count: integer;
begin
  Result := FList.Count;
end;

procedure TDebuggerUnitInfoProvider.LoadDataFromXMLConfig(const AConfig: TXMLConfig;
  const APath: string);
var
  i, c: Integer;
  Item: TDebuggerUnitInfo;
begin
  c := AConfig.GetValue(APath + 'UnitInfoCount', 0);
  for i := 0 to c - 1 do begin
    Item := TDebuggerUnitInfo.Create('', '');
    Item.LoadDataFromXMLConfig(AConfig, APath + 'UnitInfo_' + IntToStr(i) + '/');
    FList.Add(Item);
  end;
end;

procedure TDebuggerUnitInfoProvider.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  const APath: string);
var
  i: Integer;
begin
  AConfig.SetValue(APath + 'UnitInfoCount', FList.Count);
  for i := 0 to FList.Count - 1 do
    FList[i].SaveDataToXMLConfig(AConfig, APath + 'UnitInfo_' + IntToStr(i) + '/');
end;

{ TDebuggerUnitInfoList }

function TDebuggerUnitInfoList.GetInfo(Index: Integer): TDebuggerUnitInfo;
begin
  Result := TDebuggerUnitInfo(inherited Items[Index]);
end;

procedure TDebuggerUnitInfoList.PutInfo(Index: Integer; AValue: TDebuggerUnitInfo);
begin
  inherited Items[Index] := AValue;
end;

{ TDebuggerUnitInfo }

function TDebuggerUnitInfo.GetFileName: String;
begin
  Result := FFileName;
end;

function TDebuggerUnitInfo.GetDbgFullName: String;
begin
  Result := FDbgFullName;
end;

function TDebuggerUnitInfo.GetLocationFullFile: String;
begin
  Result := FLocationFullFile;;
end;

function TDebuggerUnitInfo.GetLocationName: String;
begin
  Result := FLocationName;
end;

function TDebuggerUnitInfo.GetLocationOwnerName: String;
begin
  Result := FLocationOwnerName;
end;

function TDebuggerUnitInfo.GetLocationType: TDebuggerLocationType;
begin
  Result := FLocationType;
end;

procedure TDebuggerUnitInfo.SetLocationFullFile(AValue: String);
begin
  FLocationFullFile := AValue;
end;

procedure TDebuggerUnitInfo.SetLocationType(AValue: TDebuggerLocationType);
begin
  FLocationType := AValue;
end;

constructor TDebuggerUnitInfo.Create(const AFileName: String; const AFullFileName: String);
begin
  FFileName := AFileName;
  FDbgFullName := TrimFilename(AFullFileName);
  FLocationType := dltUnknown;
end;

constructor TDebuggerUnitInfo.Create(const AUnitName, AClassName,
  AFunctionName, AFunctionArgs: String);
begin
  include(FFlags, dlfSearchByFunctionName);
  FUnitName := AUnitName;
  FSrcClassName := AClassName;
  FFunctionName := AFunctionName;
  FFunctionArgs := AFunctionArgs;
  FLocationType := dltUnknown;
end;

function TDebuggerUnitInfo.DebugText: String;
var s: String;
begin
  writestr(s{%H-}, FLocationType);
  Result
    := ' FileName="'+FFileName+'" '
    +  'DbgFullName="' + FDbgFullName+'" '
    +  'UnitName="' + FUnitName+'" '
    +  'ClassName="' + FSrcClassName+'" '
    +  'FunctionName="' + FFunctionName+'" '
    +  'Flags="' + dbgs(FFlags)+'" '
    +  'LocationName="' + FLocationName+'" '
    +  'LocationOwnerName="' + FLocationOwnerName+'" '
    +  'LocationFullFile="' + FLocationFullFile+'" '
    +  'LocationType="' + s+'"'
    +  'FunctionArgs"' + FFunctionArgs +'" ';
end;

function TDebuggerUnitInfo.IsEqual(const AFileName: String;
  const AFullFileName: String): boolean;
begin
  Result := (FFileName = AFileName) and
            (FDbgFullName = AFullFileName);
end;

function TDebuggerUnitInfo.IsEqual(const AUnitName, AClassName, AFunctionName,
  AFunctionArgs: String): boolean;
begin
  Result := (FUnitName = AUnitName) and
            (FSrcClassName = AClassName) and
            (FFunctionName = AFunctionName) and
            (FFunctionArgs = AFunctionArgs);
end;

function TDebuggerUnitInfo.IsEqual(AnOther: TDebuggerUnitInfo): boolean;
begin
  Result := (FFileName = AnOther.FFileName);
  if not Result then exit;

  case LocationType of
    dltUnknown, dltUnresolvable:
      Result := Result and (FDbgFullName = AnOther.FDbgFullName);
    dltProject, dltPackage:
      Result := Result and
                (FLocationType = AnOther.FLocationType) and
                (FLocationOwnerName = AnOther.FLocationOwnerName) and
                (FLocationName = AnOther.FLocationName);
  end;
end;

procedure TDebuggerUnitInfo.LoadDataFromXMLConfig(const AConfig: TXMLConfig;
  const APath: string);
begin
  try
    ReadStr(AConfig.GetValue(APath + 'Type', 'dltUnknown'), FLocationType);
    if LocationType = dltUnresolvable
    then LocationType := dltUnknown;
  except
    FLocationType := dltUnknown;
  end;

  if AConfig.GetValue(APath + 'ByFunction', False) then
    include(FFlags, dlfSearchByFunctionName)
  else
    exclude(FFlags, dlfSearchByFunctionName);
  FFileName          := AConfig.GetValue(APath + 'File', '');
  FLocationOwnerName := AConfig.GetValue(APath + 'UnitOwner', '');
  FLocationName      := AConfig.GetValue(APath + 'UnitFile',  '');
  FDbgFullName       := AConfig.GetValue(APath + 'DbgFile',  '');
  FLocationFullFile := '';
  FUnitName := AConfig.GetValue(APath + 'UnitName', '');
  FSrcClassName := AConfig.GetValue(APath + 'SrcClassName', '');
  FFunctionName := AConfig.GetValue(APath + 'FunctionName', '');
  FFunctionArgs := AConfig.GetValue(APath + 'FunctionArgs', '');
end;

procedure TDebuggerUnitInfo.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  const APath: string);
var
  s: String;
begin
  WriteStr(s{%H-}, LocationType);
  AConfig.SetValue(APath + 'Type', s);
  AConfig.SetValue(APath + 'File', FileName);
  AConfig.SetDeleteValue(APath + 'ByFunction',  dlfSearchByFunctionName in FFlags, False);

  AConfig.SetValue(APath + 'UnitOwner', LocationOwnerName);
  AConfig.SetValue(APath + 'UnitFile',  LocationName);
  AConfig.SetValue(APath + 'DbgFile',   FDbgFullName);
  AConfig.SetDeleteValue(APath + 'UnitName',   FUnitName, '');
  AConfig.SetDeleteValue(APath + 'SrcClassName',   FSrcClassName, '');
  AConfig.SetDeleteValue(APath + 'FunctionName',   FFunctionName, '');
  AConfig.SetDeleteValue(APath + 'FunctionArgs',   FFunctionArgs, '');
end;

{ TSnapshotList }

function TSnapshotList.Get(Index: Integer): TSnapshot;
begin
  Result := TSnapshot(inherited Items[Index])
end;

procedure TSnapshotList.Put(Index: Integer; const AValue: TSnapshot);
begin
  inherited Items[Index] := AValue;
end;

{ TDebuggerDataSnapShot }

destructor TDebuggerDataSnapShot.Destroy;
begin
  inherited Destroy;
  DataObject.Free;
end;

function TSnapshot.GetLocationAsText: String;
begin
  if FLocation.SrcFile <> ''
  then Result := FLocation.SrcFile + ' ' + IntToStr(FLocation.SrcLine)
  else Result := ':' + IntToHex(FLocation.Address, 8);
  if FLocation.FuncName <> ''
  then Result := FLocation.FuncName + ' (' + Result + ')';
end;

constructor TSnapshot.Create(ASnapMgr: TSnapshotManager);
begin
  FTimeStamp := Now;
  FSnapMgr := ASnapMgr;
  AddReference;
end;

destructor TSnapshot.Destroy;
begin
  FSnapMgr.DoSnapShotDestroy(Self);
  inherited Destroy;
end;

procedure TSnapshot.LoadDataFromXMLConfig(const AConfig: TXMLConfig; APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
begin
  FLocation.Address     := StrToQWordDef(AConfig.GetValue(APath + 'LocationAddress', '0'), 0);
  FLocation.FuncName    := AConfig.GetValue(APath + 'LocationFuncName', '');
  FLocation.SrcFile     := AConfig.GetValue(APath + 'LocationSrcFile', '');
  FLocation.SrcFullName := AConfig.GetValue(APath + 'LocationSrcFullName', '');
  FLocation.SrcLine     := AConfig.GetValue(APath + 'LocationSrcLine', -1);
  try
    FTimeStamp := StrToDouble(AConfig.GetValue(APath + 'TimeStamp', '0'));
  except
    FTimeStamp := 0;
  end;
  if FSnapMgr.Threads.Snapshots[Pointer(Self)] <> nil then
    FSnapMgr.Threads.Snapshots[Pointer(Self)].LoadDataFromXMLConfig(AConfig, APath + 'SnapThreads/', AUnitInvoPrv);
  if FSnapMgr.CallStack.Snapshots[Pointer(Self)] <> nil then
    FSnapMgr.CallStack.Snapshots[Pointer(Self)].LoadDataFromXMLConfig(AConfig, APath + 'SnapCallstack/', AUnitInvoPrv);
  if FSnapMgr.Locals.Snapshots[Pointer(Self)] <> nil then
    FSnapMgr.Locals.Snapshots[Pointer(Self)].LoadDataFromXMLConfig(AConfig, APath + 'SnapLocals/');
  if FSnapMgr.Watches.Snapshots[Pointer(Self)] <> nil then
    FSnapMgr.Watches.Snapshots[Pointer(Self)].LoadDataFromXMLConfig(AConfig, APath + 'SnapWatches/');

  if AConfig.GetValue(APath + 'IsSnapshot', False) then AddToSnapshots;
  if AConfig.GetValue(APath + 'IsHistory', True)  then AddToHistory;
end;

procedure TSnapshot.SaveDataToXMLConfig(const AConfig: TXMLConfig; APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
begin
  AConfig.SetValue(APath + 'LocationAddress', IntToStr(FLocation.Address));
  AConfig.SetValue(APath + 'LocationFuncName', FLocation.FuncName);
  AConfig.SetValue(APath + 'LocationSrcFile', FLocation.SrcFile);
  AConfig.SetValue(APath + 'LocationSrcFullName', FLocation.SrcFullName);
  AConfig.SetValue(APath + 'LocationSrcLine', FLocation.SrcLine);
  AConfig.SetValue(APath + 'TimeStamp', FloatToStr(FTimeStamp));
  AConfig.SetValue(APath + 'IsHistory', IsHistory);
  AConfig.SetValue(APath + 'IsSnapshot', IsSnapshot);

  if FSnapMgr.Threads.Snapshots[Pointer(Self)] <> nil then
    FSnapMgr.Threads.Snapshots[Pointer(Self)].SaveDataToXMLConfig(AConfig, APath + 'SnapThreads/', AUnitInvoPrv);
  if FSnapMgr.CallStack.Snapshots[Pointer(Self)] <> nil then
    FSnapMgr.CallStack.Snapshots[Pointer(Self)].SaveDataToXMLConfig(AConfig, APath + 'SnapCallstack/', AUnitInvoPrv);
  if FSnapMgr.Locals.Snapshots[Pointer(Self)] <> nil then
    FSnapMgr.Locals.Snapshots[Pointer(Self)].SaveDataToXMLConfig(AConfig, APath + 'SnapLocals/');
  if FSnapMgr.Watches.Snapshots[Pointer(Self)] <> nil then
    FSnapMgr.Watches.Snapshots[Pointer(Self)].SaveDataToXMLConfig(AConfig, APath + 'SnapWatches/');
end;

procedure TSnapshot.AddToSnapshots;
begin
  FSnapMgr.AddSnapshotEntry(Self);
end;

procedure TSnapshot.AddToHistory;
begin
  FSnapMgr.AddHistoryEntry(Self);
end;

procedure TSnapshot.RemoveFromSnapshots;
begin
  FSnapMgr.RemoveSnapshotEntry(Self);
end;

procedure TSnapshot.RemoveFromHistory;
begin
  FSnapMgr.RemoveHistoryEntry(Self);
end;

function TSnapshot.IsCurrent: Boolean;
begin
  Result := Self = FSnapMgr.Current;
end;

function TSnapshot.IsHistory: Boolean;
begin
  Result := FSnapMgr.FHistoryList.IndexOf(Self) >= 0;
end;

function TSnapshot.IsSnapshot: Boolean;
begin
  Result := FSnapMgr.FSnapshotList.IndexOf(Self) >= 0;
end;

{ TSnapshotManager }

function TSnapshotManager.GetHistoryEntry(AIndex: Integer): TSnapshot;
begin
  Result := FHistoryList[AIndex];
end;

procedure TSnapshotManager.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then exit;
  FActive := AValue;

  if Active and (FCurrentState = dsPause)
  then DoDebuggerIdle;
end;

procedure TSnapshotManager.SetDebugger(AValue: TDebuggerIntf);
begin
  if FDebugger = AValue then Exit;
  FDebugger := AValue;
  FCurrentState := dsNone;
end;

procedure TSnapshotManager.DoCallStackChanged(Sender: TObject);
begin
  if FForcedIdle then
    DoDebuggerIdle(True);
end;

procedure TSnapshotManager.SetCallStack(AValue: TIdeCallStackMonitor);
begin
  if FCallStack = AValue then Exit;

  if (FCallStackNotification <> nil) and (FCallStack <> nil) then begin
    FCallStack.RemoveNotification(FCallStackNotification);
  end;

  FCallStack := AValue;

  if (FCallStack <> nil) then begin
    if FCallStackNotification = nil then begin
      FCallStackNotification := TCallStackNotification.Create;
      FCallStackNotification.AddReference;
      FCallStackNotification.OnChange  := @DoCallStackChanged;
    end;
    FCallStack.AddNotification(FCallStackNotification);
  end;

end;

procedure TSnapshotManager.SetHistoryIndex(const AValue: Integer);
begin
  if FHistoryindex = AValue then exit;
  FHistoryindex := AValue;
  if FHistorySelected then DoCurrent;
end;

procedure TSnapshotManager.SetHistorySelected(AValue: Boolean);
begin
  if FHistoryList.Count = 0 then AValue := False;
  if FHistorySelected = AValue then exit;
  FHistorySelected := AValue;
  if AValue then SnapshotSelected := False;
  DoCurrent;
end;

function TSnapshotManager.GetSnapshotEntry(AIndex: Integer): TSnapshot;
begin
  Result := FSnapshotList[AIndex];
end;

procedure TSnapshotManager.SetSnapshotIndex(const AValue: Integer);
begin
  if FSnapshotIndex = AValue then exit;
  FSnapshotIndex := AValue;
  if FSnapshotSelected then DoCurrent;
end;

procedure TSnapshotManager.SetSnapshotSelected(AValue: Boolean);
begin
  if FSnapshotList.Count = 0 then AValue := False;
  if FSnapshotSelected = AValue then exit;
  FSnapshotSelected := AValue;
  if AValue then HistorySelected := False;
  DoCurrent;
end;

procedure TSnapshotManager.DoSnapShotDestroy(ASnapShot: TSnapshot);
begin
  FHistoryList.Remove(ASnapShot);
  RemoveHistoryEntryFromMonitors(ASnapShot);

  if FHistoryList.Count = 0
  then HistorySelected := False;
  if FSnapshotList.Count = 0
  then SnapshotSelected := False;
end;

procedure TSnapshotManager.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TSnapshotManager.EndUpdate;
begin
  Assert(FUpdateLock > 0, 'TSnapshotManager.EndUpdate no locked');
  if FUpdateLock > 0
  then dec(FUpdateLock);
  if FUpdateLock = 0 then begin
    if ufSnapChanged in FUpdateFlags then DoChanged;
    if ufSnapCurrent in FUpdateFlags then DoCurrent;
  end;
end;

procedure TSnapshotManager.DoChanged;
begin
  if FUpdateLock > 0 then begin
    Include(FUpdateFlags, ufSnapChanged);
    exit;
  end;
  Exclude(FUpdateFlags, ufSnapChanged);
  FNotificationList.NotifyChange(Self);
end;

procedure TSnapshotManager.DoCurrent;
begin
  if FUpdateLock > 0 then begin
    Include(FUpdateFlags, ufSnapCurrent);
    exit;
  end;
  Exclude(FUpdateFlags, ufSnapCurrent);
  FNotificationList.NotifyCurrent(Self);
end;

procedure TSnapshotManager.LoadDataFromXMLConfig(const AConfig: TXMLConfig; APath: string);
var
  c, i: Integer;
  NewSnap: TSnapshot;
  UIProv: TDebuggerUnitInfoProvider;
begin
  Clear;
  UIProv := TDebuggerUnitInfoProvider.Create;
  UIProv.LoadDataFromXMLConfig(AConfig, APath + 'UnitInfos/');

  c := AConfig.GetValue(APath + 'SnapCount', 0);
  for i := 0 to c - 1 do begin
    NewSnap := TSnapshot.Create(Self);
    FThreads.NewSnapshot(NewSnap, True);
    FCallStack.NewSnapshot(NewSnap, True);
    FLocals.NewSnapshot(NewSnap, True);
    FWatches.NewSnapshot(NewSnap, True);
    NewSnap.LoadDataFromXMLConfig(AConfig, APath + 'SnapEntry' + IntToStr(i) + '/', UIProv);
    if not(NewSnap.IsHistory or NewSnap.IsSnapshot) then begin
      RemoveHistoryEntryFromMonitors(NewSnap); // TODO: add user feedback / warning
      debugln(['************** Snapshot loaded, but not kept']);
    end;
    NewSnap.ReleaseReference;
  end;

  c := AConfig.GetValue(APath + 'HistCount', 0);
  for i := 0 to c - 1 do begin
    NewSnap := TSnapshot.Create(Self);
    FThreads.NewSnapshot(NewSnap, True);
    FCallStack.NewSnapshot(NewSnap, True);
    FLocals.NewSnapshot(NewSnap, True);
    FWatches.NewSnapshot(NewSnap, True);
    NewSnap.LoadDataFromXMLConfig(AConfig, APath + 'HistEntry' + IntToStr(i) + '/', UIProv);
    if not(NewSnap.IsHistory or NewSnap.IsSnapshot) then begin
      RemoveHistoryEntryFromMonitors(NewSnap); // TODO: add user feedback / warning
      debugln(['************** Snapshot loaded, but not kept']);
    end;
    NewSnap.ReleaseReference;
  end;

  UIProv.Free;

  //FThreads.CurrentThreads.SnapShot := nil;
  //FCallStack.CurrentCallStackList.SnapShot := nil;
  //FLocals.CurrentLocalsList.SnapShot := nil;
  //FWatches.CurrentWatches.SnapShot := nil;
  DoChanged;
  DoCurrent;
end;

procedure TSnapshotManager.SaveDataToXMLConfig(const AConfig: TXMLConfig; APath: string);
var
  c, i: Integer;
  UIProv: TDebuggerUnitInfoProvider;
begin
  UIProv := TDebuggerUnitInfoProvider.Create;

  c := 0;
  for i := 0 to FSnapshotList.Count - 1 do begin
    if FSnapshotList[i].IsHistory then continue;
    FSnapshotList[i].SaveDataToXMLConfig(AConfig, APath + 'SnapEntry' + IntToStr(i) + '/', UIProv);
    inc(c);
  end;
  AConfig.SetValue(APath + 'SnapCount', c);

  c := 0;
  for i := 0 to FHistoryList.Count - 1 do begin
    FHistoryList[i].SaveDataToXMLConfig(AConfig, APath + 'HistEntry' + IntToStr(i) + '/', UIProv);
    inc(c);
  end;
  AConfig.SetValue(APath + 'HistCount', c);

  UIProv.SaveDataToXMLConfig(AConfig, APath + 'UnitInfos/');
  UIProv.Free;
end;

procedure TSnapshotManager.ClearHistory;
begin
  FHistoryList.Clear;
  HistorySelected := False;
end;

procedure TSnapshotManager.ClearSnapshots;
begin
  FSnapshotList.Clear;
  SnapshotSelected := False;
end;

function TSnapshotManager.GetAsXML: String;
var
  XmlConf: TXMLConfig;
  s: TStringStream;
begin
  XmlConf := TXMLConfig.CreateClean('');
  XmlConf.Clear;
  SaveDataToXMLConfig(XmlConf, 'History/');
  s := TStringStream.Create('');
  XmlConf.WriteToStream(s);
  Result := s.DataString;
  s.WriteAnsiString(Result);
  XmlConf.Free;
  s.Free;
end;

procedure TSnapshotManager.SetFromXML(aXML: String);
var
  XmlConf: TXMLConfig;
  s: TStringStream;
begin
  XmlConf := TXMLConfig.CreateClean('');
  XmlConf.Clear;
  s := TStringStream.Create(aXML);
  XmlConf.ReadFromStream(s);
  LoadDataFromXMLConfig(XmlConf, 'History/');
  XmlConf.Free;
  s.Free;
end;

procedure TSnapshotManager.CreateHistoryEntry;
var
  t: LongInt;
begin
  ReleaseRefAndNil(FCurrentSnapshot); // should be nil already
  FCurrentSnapshot := TSnapshot.Create(Self);
  FCurrentSnapshot.Location := Debugger.GetLocation;

  FThreads.NewSnapshot(FCurrentSnapshot);
  FCallStack.NewSnapshot(FCurrentSnapshot);
  FLocals.NewSnapshot(FCurrentSnapshot);
  FWatches.NewSnapshot(FCurrentSnapshot);

  // acces them , so they will be present
  t := FThreads.CurrentThreads.CurrentThreadId;
  FCallStack.CurrentCallStackList.EntriesForThreads[t];

  DoDebuggerIdle;
  DoChanged;
end;

procedure TSnapshotManager.RemoveHistoryEntry(AIndex: Integer);
begin
  BeginUpdate;
  try
    FHistoryList.Delete(AIndex);
    if FHistoryList.Count = 0
    then HistorySelected := False;
    DoChanged;
  finally
    EndUpdate;
  end;
end;

procedure TSnapshotManager.RemoveHistoryEntry(ASnapShot: TSnapshot);
begin
  BeginUpdate;
  try
    FHistoryList.Remove(ASnapShot);
    if FHistoryList.Count = 0
    then HistorySelected := False;
    DoChanged;
  finally
    EndUpdate;
  end;
end;

procedure TSnapshotManager.RemoveHistoryEntryFromMonitors(AnEntry: TSnapshot);
begin
  if FThreads <> nil then   FThreads.RemoveSnapshot(AnEntry);
  if FCallStack <> nil then FCallStack.RemoveSnapshot(AnEntry);
  if FLocals <> nil then    FLocals.RemoveSnapshot(AnEntry);
  if FWatches <> nil then   FWatches.RemoveSnapshot(AnEntry);
end;

procedure TSnapshotManager.AddSnapshotEntry(ASnapShot: TSnapshot);
begin
  FSnapshotList.Add(ASnapShot);
  DoChanged;
end;

procedure TSnapshotManager.RemoveSnapshotEntry(ASnapShot: TSnapshot);
begin
  BeginUpdate;
  try
    FSnapshotList.Remove(ASnapShot);
    if FSnapshotList.Count = 0
    then SnapshotSelected := False;
    DoChanged;
  finally
    EndUpdate;
  end;
end;

procedure TSnapshotManager.AddHistoryEntry(ASnapShot: TSnapshot);
begin
  FHistoryList.Add(ASnapShot);
  DoChanged;
end;

constructor TSnapshotManager.Create;
begin
  FNotificationList := TDebuggerChangeNotificationList.Create;
  FActive := True;
  FHistorySelected := False;
  FHistoryList := TSnapshotList.Create;
  FHistoryCapacity := 25;
  FSnapshotList := TSnapshotList.Create;
  inherited Create;
end;

destructor TSnapshotManager.Destroy;
begin
  FCallStackNotification.OnChange := nil;
  FNotificationList.Clear;
  ReleaseRefAndNil(FCurrentSnapshot);
  Clear;
  CallStack := nil;
  ReleaseRefAndNil(FCallStackNotification);
  inherited Destroy;
  FreeAndNil(FHistoryList);
  FreeAndNil(FSnapshotList);
  FreeAndNil(FNotificationList);
end;

procedure TSnapshotManager.AddNotification(const ANotification: TSnapshotNotification);
begin
  FNotificationList.Add(ANotification);
end;

procedure TSnapshotManager.RemoveNotification(const ANotification: TSnapshotNotification);
begin
  FNotificationList.Remove(ANotification);
end;

procedure TSnapshotManager.DoStateChange(const AOldState: TDBGState);
begin
  if FDebugger = nil then exit;
  FCurrentState := Debugger.State;
  FForcedIdle := False;
  DebugLnEnter(DBG_DATA_MONITORS, ['DebugDataMonitor: >>ENTER: TSnapshotManager.DoStateChange  New-State=', DBGStateNames[FCurrentState]]);

  BeginUpdate;
  try
    if FDebugger.State in [dsPause, dsInternalPause] then begin
      Exclude(FUpdateFlags, ufInDebuggerIdle);
      FRequestsDone := [];
      CreateHistoryEntry;
      HistorySelected := False;
      SnapshotSelected := False;
    end
    else begin
      if (FCurrentSnapshot <> nil) and (FActive or (AOldState = dsInternalPause)) then begin
        HistoryIndex := FHistoryList.Add(FCurrentSnapshot);
        ReleaseRefAndNil(FCurrentSnapshot);
        while FHistoryList.Count > HistoryCapacity do RemoveHistoryEntry(0);
        DoChanged;
      end;
    end;
    if (FDebugger.State = dsInit) then begin
      Clear;
    end;
  finally
    EndUpdate;
  end;
  DebugLnExit(DBG_DATA_MONITORS, ['DebugDataMonitor: <<EXIT: TSnapshotManager.DoStateChange']);
end;

procedure TSnapshotManager.DoDebuggerIdle(AForce: Boolean = False);
var
  i, j, k: LongInt;
  w: TCurrentWatches;
  CurSnap: TSnapshot;
begin
  if ufInDebuggerIdle in FUpdateFlags then exit;
  if (not FActive) and (not AForce) then exit;
  if not(FCurrentState in [dsPause, dsInternalPause]) then exit;
  if (not Debugger.IsIdle) and (not AForce) then exit;
  Include(FUpdateFlags, ufInDebuggerIdle);
  CurSnap := FCurrentSnapshot;
  DebugLnEnter(DBG_DATA_MONITORS, ['DebugDataMonitor: >>ENTER: TSnapshotManager.DoDebuggerIdle  New-State=', DBGStateNames[FCurrentState]]);
  try

    if not(smrThreads in FRequestsDone) then begin
      include(FRequestsDone, smrThreads);
      FThreads.CurrentThreads.Count;
      if (not(FCurrentState in [dsPause, dsInternalPause])) or
         (Debugger = nil) or ( (not Debugger.IsIdle) and (not AForce) )
      then exit;
      if CurSnap <> FCurrentSnapshot then exit; // Debugger did "run" in between
    end;

    if not(smrCallStack in FRequestsDone) then begin
      i := FThreads.CurrentThreads.CurrentThreadId;
      k := FCallStack.CurrentCallStackList.EntriesForThreads[i].CountLimited(5);
      if CurSnap <> FCurrentSnapshot then exit; // Debugger did "run" in between
      if (k > 0) or (smrCallStackCnt in FRequestsDone) then begin
        // Since DoDebuggerIdle was re-entered
        // and smrCallStackCnt is set, the count should be valid
        include(FRequestsDone, smrCallStack);
        if k > 0
        then FCallStack.CurrentCallStackList.EntriesForThreads[i].PrepareRange(0, Min(5, k));
        if (not(FCurrentState in [dsPause, dsInternalPause])) or
           (Debugger = nil) or ( (not Debugger.IsIdle) and (not AForce) )
        then exit;
        if CurSnap <> FCurrentSnapshot then exit; // Debugger did "run" in between
      end
      else
      if AForce then // request re-entry, even if not idle
        FForcedIdle := True;
    end;

    if not(smrCallStackCnt in FRequestsDone) then begin
      include(FRequestsDone, smrCallStackCnt);
      i := FThreads.CurrentThreads.CurrentThreadId;
      FCallStack.CurrentCallStackList.EntriesForThreads[i].CountLimited(5);
      if (not(FCurrentState in [dsPause, dsInternalPause])) or
         (Debugger = nil) or ( (not Debugger.IsIdle) and (not AForce) )
      then exit;
      if CurSnap <> FCurrentSnapshot then exit; // Debugger did "run" in between
    end;

    if not(smrLocals in FRequestsDone) then begin
      include(FRequestsDone, smrLocals);
      i := FThreads.CurrentThreads.CurrentThreadId;
      j := FCallStack.CurrentCallStackList.EntriesForThreads[i].CurrentIndex;
      FLocals.CurrentLocalsList.Entries[i, j].Count;
      if (not(FCurrentState in [dsPause, dsInternalPause])) or
         (Debugger = nil) or ( (not Debugger.IsIdle) and (not AForce) )
      then exit;
      if CurSnap <> FCurrentSnapshot then exit; // Debugger did "run" in between
    end;

    if not(smrWatches in FRequestsDone) then begin
      include(FRequestsDone, smrWatches);
      i := FThreads.CurrentThreads.CurrentThreadId;
      j := FCallStack.CurrentCallStackList.EntriesForThreads[i].CurrentIndex;
      w := FWatches.CurrentWatches;
      k := 0;
      while k < w.Count do begin
        if CurSnap <> FCurrentSnapshot then exit; // Debugger did "run" in between
        w[k].Values[i, j].Value;
        inc(k);
      end;
      if (not(FCurrentState in [dsPause, dsInternalPause])) or
         (Debugger = nil) or ( (not Debugger.IsIdle) and (not AForce) )
      then exit;
      if CurSnap <> FCurrentSnapshot then exit; // Debugger did "run" in between
    end;
  finally
    Exclude(FUpdateFlags, ufInDebuggerIdle);
    DebugLnExit(DBG_DATA_MONITORS, ['DebugDataMonitor: <<EXIT: TSnapshotManager.DoDebuggerIdle']);
  end;
end;

function TSnapshotManager.SelectedId: Pointer;
begin
  Result := nil;
  if (HistoryIndex >= 0) and (HistoryIndex < FHistoryList.Count) and (FHistorySelected)
  then Result := FHistoryList[HistoryIndex];
  if (SnapshotIndex >= 0) and (SnapshotIndex < FSnapshotList.Count) and (FSnapshotSelected)
  then Result := FSnapshotList[HistoryIndex];
end;

function TSnapshotManager.SelectedEntry: TSnapshot;
begin
  Result := nil;
  if (HistoryIndex >= 0) and (HistoryIndex < FHistoryList.Count) and (FHistorySelected)
  then Result := FHistoryList[HistoryIndex];
  if (SnapshotIndex >= 0) and (SnapshotIndex < FSnapshotList.Count) and (FSnapshotSelected)
  then Result := FSnapshotList[SnapshotIndex];
end;

procedure TSnapshotManager.Clear;
begin
  BeginUpdate;
  try
    ClearHistory;
    ClearSnapshots;
    DoChanged;
    DoCurrent;
  finally
    EndUpdate;
  end;
end;

{ TDebuggerDataSnapShotList }

function TDebuggerDataSnapShotList.GetSnapShot(AnID: Pointer): TObject;
var
  i: Integer;
begin
  i := FList.Count - 1;
  while i >= 0 do begin
    Result := TObject(FList[i]);
    if TDebuggerDataSnapShot(Result).SnapShotId = AnID
    then exit(TDebuggerDataSnapShot(Result).DataObject);
    dec(i);
  end;
  Result := nil;
end;

constructor TDebuggerDataSnapShotList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TDebuggerDataSnapShotList.Destroy;
begin
  Clear;
  inherited Destroy;
  FreeAndNil(FList);
end;

procedure TDebuggerDataSnapShotList.Clear;
begin
  while FList.Count > 0 do begin
    TDebuggerDataSnapShot(FList[0]).Free;
    FList.Delete(0);
  end;
end;

procedure TDebuggerDataSnapShotList.AddSnapShot(AnID: Pointer; AnObject: TObject);
var
  NewSn: TDebuggerDataSnapShot;
begin
  NewSn := TDebuggerDataSnapShot.Create;
  NewSn.SnapShotId := AnID;
  NewSn.DataObject := AnObject;
  FList.Add(NewSn);
end;

procedure TDebuggerDataSnapShotList.RemoveSnapShot(AnID: Pointer);
var
  R: TDebuggerDataSnapShot;
  i: Integer;
begin
  i := FList.Count - 1;
  while i >= 0 do begin
    R := TDebuggerDataSnapShot(FList[i]);
    if TDebuggerDataSnapShot(R).SnapShotId = AnID
    then break;
    dec(i);
  end;
  if i >= 0 then begin
    FList.Delete(i);
    R.Free;
  end;
end;

{ TCurrentLocalsList }

procedure TCurrentLocalsList.SetSnapShot(const AValue: TIDELocalsList);
var
  i: Integer;
  E, R: TIDELocals;
begin
  assert((FSnapShot=nil) or (AValue=nil), 'TCurrentLocalsList already have snapshot');
  if FSnapShot = AValue then exit;

  if FSnapShot <> nil then
    FSnapShot.Immutable := True;

  FSnapShot := AValue;

  if FSnapShot = nil then begin
    for i := 0 to Count-1 do
      TCurrentLocals(EntriesByIdx[i]).SnapShot := nil;
  end else begin
    //FSnapShot.Assign(Self);
    FSnapShot.Clear;
    for i := 0 to Count-1 do begin
      E := EntriesByIdx[i];
      R := TIDELocals.Create(e.ThreadId, e.StackFrame);
      FSnapShot.Add(R);
      TCurrentLocals(E).SnapShot := R;
    end;

  end;
end;

procedure TCurrentLocalsList.DoCleared;
begin
  FMonitor.NotifyChange(nil);
end;

procedure TCurrentLocalsList.DoAdded(AnEntry: TDbgEntityValuesList);
var
  R: TIDELocals;
begin
  Assert(AnEntry is TCurrentLocals, 'TCurrentLocalsList.DoAdded');
  inherited DoAdded(AnEntry);
  if FSnapShot <> nil
  then begin
    R := TIDELocals.Create(AnEntry.ThreadId, AnEntry.StackFrame);
    FSnapShot.Add(R);
    TCurrentLocals(AnEntry).SnapShot := R;
  end;
end;

function TCurrentLocalsList.CreateEntry(AThreadId, AStackFrame: Integer): TIDELocals;
begin
  Result := TCurrentLocals.Create(FMonitor, AThreadId, AStackFrame);
end;

constructor TCurrentLocalsList.Create(AMonitor: TIdeLocalsMonitor);
begin
  FMonitor := AMonitor;
  inherited Create;
end;

{ TLocalsList }

function TIDELocalsList.GetEntry(const AThreadId: Integer; const AStackFrame: Integer): TIDELocals;
begin
  Result := TIDELocals(inherited Entries[AThreadId, AStackFrame]);
end;

function TIDELocalsList.GetEntryByIdx(const AnIndex: Integer): TIDELocals;
begin
  Result := TIDELocals(inherited EntriesByIdx[AnIndex]);
end;

function TIDELocalsList.CreateEntry(AThreadId, AStackFrame: Integer): TDbgEntityValuesList;
begin
  Result := TIDELocals.Create(AThreadId, AStackFrame);
end;

procedure TIDELocalsList.DoAssign(AnOther: TDbgEntitiesThreadStackList);
begin
  inherited DoAssign(AnOther);
  Immutable := not(Self is TCurrentLocalsList);
end;

procedure TIDELocalsList.DoAdded(AnEntry: TDbgEntityValuesList);
begin
  inherited DoAdded(AnEntry);
  //AnEntry.Immutable := not(Self is TCurrentLocalsList);
end;

procedure TIDELocalsList.LoadDataFromXMLConfig(const AConfig: TXMLConfig; APath: string);
var
  e: TIDELocals;
  c, i: Integer;
begin
  Clear;
  c := AConfig.GetValue(APath + 'Count', 0);
  APath := APath + 'LocalsEntry';
  for i := 0 to c - 1 do begin
    e := TIDELocals.CreateFromXMLConfig(AConfig, APath + IntToStr(i) + '/');
    Add(e);
  end;
end;

procedure TIDELocalsList.SaveDataToXMLConfig(const AConfig: TXMLConfig; APath: string);
var
  i: Integer;
begin
  AConfig.SetDeleteValue(APath + 'Count', Count, 0);
  APath := APath + 'LocalsEntry';
  for i := 0 to Count - 1 do
    EntriesByIdx[i].SaveDataToXMLConfig(AConfig, APath + IntToStr(i) + '/');
end;

{ TIdeLocalsMonitor }

function TIdeLocalsMonitor.GetSupplier: TLocalsSupplier;
begin
  Result := TLocalsSupplier(inherited Supplier);
end;

function TIdeLocalsMonitor.GetSnapshot(AnID: Pointer): TIDELocalsList;
begin
  Result := TIDELocalsList(FSnapshots.SnapShot[AnID]);
end;

function TIdeLocalsMonitor.GetCurrentLocalsList: TCurrentLocalsList;
begin
  Result := TCurrentLocalsList(LocalsList);;
end;

procedure TIdeLocalsMonitor.SetSupplier(const AValue: TLocalsSupplier);
begin
  inherited Supplier := AValue;
end;

procedure TIdeLocalsMonitor.DoStateEnterPause;
begin
  inherited DoStateEnterPause;
  if (CurrentLocalsList = nil) then Exit;
  Clear;
end;

procedure TIdeLocalsMonitor.DoStateLeavePause;
begin
  inherited DoStateLeavePause;
  if (CurrentLocalsList = nil) then Exit;
  CurrentLocalsList.SnapShot := nil;
end;

procedure TIdeLocalsMonitor.DoStateLeavePauseClean;
begin
  inherited DoStateLeavePauseClean;
  if (CurrentLocalsList = nil) then Exit;
  CurrentLocalsList.SnapShot := nil;
  Clear;
end;

procedure TIdeLocalsMonitor.NotifyChange(ALocals: TCurrentLocals);
begin
  FNotificationList.NotifyChange(ALocals);
end;

procedure TIdeLocalsMonitor.DoNewSupplier;
begin
  inherited DoNewSupplier;
  NotifyChange(nil);
  if Supplier <> nil then
    Supplier.CurrentLocalsList := CurrentLocalsList;
end;

procedure TIdeLocalsMonitor.RequestData(ALocals: TCurrentLocals);
begin
  if Supplier <> nil
  then Supplier.RequestData(ALocals)
  else ALocals.SetDataValidity(ddsInvalid);
end;

function TIdeLocalsMonitor.CreateSnapshot(CreateEmpty: Boolean = False): TObject;
begin
  Result := TIDELocalsList.Create;
  if not CreateEmpty
  then CurrentLocalsList.SnapShot := TIDELocalsList(Result);
end;

function TIdeLocalsMonitor.CreateLocalsList: TLocalsList;
begin
  Result := TCurrentLocalsList.Create(Self);
end;

constructor TIdeLocalsMonitor.Create;
begin
  FSnapshots := TDebuggerDataSnapShotList.Create;
  inherited;
  FNotificationList := TDebuggerChangeNotificationList.Create;
end;

destructor TIdeLocalsMonitor.Destroy;
begin
  FSnapshots.Clear;
  FNotificationList.Clear;
  inherited Destroy;
  FreeAndNil(FNotificationList);
  FreeAndNil(FSnapshots);
end;

procedure TIdeLocalsMonitor.Clear;
begin
  CurrentLocalsList.Clear;
end;

procedure TIdeLocalsMonitor.AddNotification(const ANotification: TLocalsNotification);
begin
  FNotificationList.Add(ANotification);
end;

procedure TIdeLocalsMonitor.RemoveNotification(const ANotification: TLocalsNotification);
begin
  FNotificationList.Remove(ANotification);
end;

procedure TIdeLocalsMonitor.NewSnapshot(AnID: Pointer; CreateEmpty: Boolean);
var
  S: TObject;
begin
  S := CreateSnapshot(CreateEmpty);
  FSnapshots.AddSnapShot(AnID, S);
end;

procedure TIdeLocalsMonitor.RemoveSnapshot(AnID: Pointer);
begin
  FSnapshots.RemoveSnapShot(AnID);
end;

{ TCurrentWatchValue }

procedure TCurrentWatchValue.SetSnapShot(const AValue: TWatchValue);
begin
  assert((FSnapShot=nil) or (AValue=nil), 'TCurrentWatchValue already have snapshot');
  if FSnapShot = AValue then exit;
  FSnapShot := AValue;
  if FSnapShot <> nil
  then FSnapShot.Assign(self);
end;

procedure TCurrentWatchValue.RequestData;
begin
  TCurrentWatch(FWatch).RequestData(self);
end;

procedure TCurrentWatchValue.DoDataValidityChanged(AnOldValidity: TDebuggerDataState);
begin
  if Validity = ddsRequested then exit;
  TCurrentWatches(TCurrentWatch(FWatch).Collection).Update(FWatch);
  if FSnapShot <> nil
  then FSnapShot.Assign(self);
end;

{ TCurrentWatchValueList }

procedure TCurrentWatchValueList.SetSnapShot(const AValue: TWatchValueList);
var
  R: TWatchValue;
  i: Integer;
begin
  assert((FSnapShot=nil) or (AValue=nil), 'TCurrentWatchValueList already have snapshot');
  if FSnapShot = AValue then exit;
  FSnapShot := AValue;

  if FSnapShot = nil then begin
    for i := 0 to Count - 1 do
      TCurrentWatchValue(EntriesByIdx[i]).SnapShot := nil;
  end
  else begin
    // Assign
    FSnapShot.Clear;
    for i := 0 to Count - 1 do begin
      R := TWatchValue.Create(FSnapShot.FWatch);
      R.Assign(EntriesByIdx[i]);
      FSnapShot.Add(R);
      TCurrentWatchValue(EntriesByIdx[i]).SnapShot := R;
    end;
  end;

end;

function TCurrentWatchValueList.CreateEntry(const AThreadId: Integer;
  const AStackFrame: Integer): TWatchValue;
var
  R: TWatchValue;
begin
  try DebugLnEnter(DBG_DATA_MONITORS, ['DebugDataMonitor: >>ENTER: TCurrentWatchValueList.CreateEntry  AThreadId=', AThreadId, '  AStackFrame=',AStackFrame, ' Expr=', FWatch.Expression]);
  Result := TCurrentWatchValue.Create(FWatch, AThreadId, AStackFrame);
  Add(Result);
  if FSnapShot <> nil then begin
    R := TWatchValue.Create(FSnapShot.FWatch);
    FSnapShot.Add(R);
    TCurrentWatchValue(Result).SnapShot := R;
  end;
  finally DebugLnExit(DBG_DATA_MONITORS, ['DebugDataMonitor: <<EXIT: TCurrentWatchValueList.CreateEntry']); end;
end;

{ TWatchValueList }

function TWatchValueList.GetEntry(const AThreadId: Integer;
  const AStackFrame: Integer): TWatchValue;
var
  i: Integer;
begin
  i := FList.Count - 1;
  while i >= 0 do begin
    Result := TWatchValue(FList[i]);
    if (Result.ThreadId = AThreadId) and (Result.StackFrame = AStackFrame) and
       (Result.DisplayFormat = FWatch.DisplayFormat) and
       (Result.RepeatCount = FWatch.RepeatCount) and
       (Result.EvaluateFlags = FWatch.EvaluateFlags)
    then
      exit;
    dec(i);
  end;
  Result := CreateEntry(AThreadId, AStackFrame);
end;

function TWatchValueList.GetEntryByIdx(AnIndex: integer): TWatchValue;
begin
  Result := TWatchValue(FList[AnIndex]);
end;

function TWatchValueList.CreateEntry(const AThreadId: Integer;
  const AStackFrame: Integer): TWatchValue;
begin
  Result := nil;
end;

procedure TWatchValueList.LoadDataFromXMLConfig(const AConfig: TXMLConfig;
  APath: string);
var
  e: TWatchValue;
  c, i: Integer;
begin
  Clear;
  c := AConfig.GetValue(APath + 'Count', 0);
  APath := APath + 'Entry';
  for i := 0 to c - 1 do begin
    e := TWatchValue.Create(FWatch);
    e.LoadDataFromXMLConfig(AConfig, APath + IntToStr(i) + '/');
    Add(e);
  end;
end;

procedure TWatchValueList.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  APath: string);
var
  i: Integer;
begin
  AConfig.SetDeleteValue(APath + 'Count', Count, 0);
  APath := APath + 'Entry';
  for i := 0 to Count - 1 do
    EntriesByIdx[i].SaveDataToXMLConfig(AConfig, APath + IntToStr(i) + '/');
end;

procedure TWatchValueList.Assign(AnOther: TWatchValueList);
var
  i: Integer;
  v: TWatchValue;
begin
  Clear;
  for i := 0 to AnOther.FList.Count - 1 do begin
    v := TWatchValue.Create(FWatch);
    v.Assign(TWatchValue(AnOther.FList[i]));
    FList.Add(v);
  end;
end;

constructor TWatchValueList.Create(AOwnerWatch: TWatch);
begin
  assert(AOwnerWatch <> nil, 'TWatchValueList.Create without owner');
  assert(((Self is TCurrentWatchValueList) and (AOwnerWatch is TCurrentWatch)) or ((not(Self is TCurrentWatchValueList)) and not(AOwnerWatch is TCurrentWatch)),
         'TWatchValueList.Create: Watch and list differ (current and none current)');
  FList := TList.Create;
  FWatch := AOwnerWatch;
  inherited Create;
end;

destructor TWatchValueList.Destroy;
begin
  Clear;
  inherited Destroy;
  FreeAndNil(FList);
end;

procedure TWatchValueList.Add(AnEntry: TWatchValue);
begin
  Flist.Add(AnEntry);
end;

procedure TWatchValueList.Clear;
begin
  while FList.Count > 0 do begin
    TObject(FList[0]).Free;
    FList.Delete(0);
  end;
end;

function TWatchValueList.Count: Integer;
begin
  Result := FList.Count;
end;

{ TWatchValue }

function TWatchValue.GetValue: String;
var
  i: Integer;
begin
  if not FWatch.Enabled then begin
    Result := '<disabled>';
    exit;
  end;
  i := DbgStateChangeCounter;  // workaround for state changes during TWatchValue.GetValue
  if Validity = ddsUnknown then begin
    Result := '<evaluating>';
    Validity := ddsRequested;
    RequestData;
    if i <> DbgStateChangeCounter then exit; // in case the debugger did run.
    // TODO: The watch can also be deleted by the user
  end;
  case Validity of
    ddsRequested, ddsEvaluating: Result := '<evaluating>';
    ddsValid:                    Result := inherited GetValue;
    ddsInvalid:                  Result := '<invalid>';
    ddsError:                    Result := '<Error: '+ (inherited GetValue) +'>';
  end;

end;

function TWatchValue.GetWatchBase: TWatchBase;
begin
  Result := FWatch;
end;

function TWatchValue.GetDisplayFormat: TWatchDisplayFormat;
begin
  Result := FDisplayFormat;
end;

function TWatchValue.GetEvaluateFlags: TDBGEvaluateFlags;
begin
  Result := FEvaluateFlags;
end;

function TWatchValue.GetExpression: String;
begin
  Result := FWatch.Expression;
end;

function TWatchValue.GetRepeatCount: Integer;
begin
  Result := FRepeatCount;
end;

function TWatchValue.GetStackFrame: Integer;
begin
  Result := FStackFrame;
end;

function TWatchValue.GetThreadId: Integer;
begin
  Result := FThreadId;
end;

function TWatchValue.GetTypeInfo: TDBGType;
var
  i: Integer;
begin
  Result := nil;
  if not FWatch.Enabled then
    exit;
  i := DbgStateChangeCounter;  // workaround for state changes during TWatchValue.GetValue
  if Validity = ddsUnknown then begin
    Validity := ddsRequested;
    RequestData;
    if i <> DbgStateChangeCounter then exit;
  end;
  case Validity of
    ddsRequested,
    ddsEvaluating: Result := nil;
    ddsValid:      Result := inherited GetTypeInfo;
    ddsInvalid,
    ddsError:      Result := nil;
  end;
end;

procedure TWatchValue.RequestData;
begin
  Validity := ddsInvalid;
end;

procedure TWatchValue.LoadDataFromXMLConfig(const AConfig: TXMLConfig;
  const APath: string);
var
  NewValidity: TDebuggerDataState;
begin
  FThreadId   := AConfig.GetValue(APath + 'ThreadId', -1);
  FStackFrame := AConfig.GetValue(APath + 'StackFrame', -1);
  Value      := AConfig.GetValue(APath + 'Value', '');
  if AConfig.GetValue(APath + 'ClassAutoCast', False)
  then Include(FEvaluateFlags, defClassAutoCast)
  else Exclude(FEvaluateFlags, defClassAutoCast);
  FRepeatCount := AConfig.GetValue(APath + 'RepeatCount', 0);
  try    ReadStr(AConfig.GetValue(APath + 'DisplayFormat', 'wdfDefault'), FDisplayFormat);
  except FDisplayFormat := wdfDefault; end;
  try
    ReadStr(AConfig.GetValue(APath + 'Validity', 'ddsValid'), NewValidity);
    Validity := NewValidity;
  except
    Validity := ddsUnknown;
  end;
end;

procedure TWatchValue.SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string);
var
  s: String;
begin
  AConfig.SetValue(APath + 'ThreadId', FThreadId);
  AConfig.SetValue(APath + 'StackFrame', FStackFrame);
  AConfig.SetValue(APath + 'Value', Value);
  WriteStr(s{%H-}, FDisplayFormat);
  AConfig.SetDeleteValue(APath + 'DisplayFormat', s, 'wdfDefault');
  WriteStr(s, Validity);
  AConfig.SetDeleteValue(APath + 'Validity', s, 'ddsValid');
  AConfig.SetDeleteValue(APath + 'ClassAutoCast', defClassAutoCast in FEvaluateFlags, False);
  AConfig.SetDeleteValue(APath + 'RepeatCount', FRepeatCount, 0);
end;

constructor TWatchValue.Create;
begin
  assert(FWatch <> nil, 'TwatchValue without owner');
  assert(((Self is TCurrentWatchValue) and (FWatch is TCurrentWatch)) or ((not(Self is TCurrentWatchValue)) and not(FWatch is TCurrentWatch)),
         'TWatchValue.Create: Watch and self differ (current and none current)');
  inherited Create;
end;

constructor TWatchValue.Create(AOwnerWatch: TWatch);
begin
  Validity := ddsUnknown;
  FWatch := AOwnerWatch;
  FDisplayFormat := FWatch.DisplayFormat;
  FEvaluateFlags := FWatch.EvaluateFlags;
  FRepeatCount   := FWatch.RepeatCount;
  Create;
end;

constructor TWatchValue.Create(AOwnerWatch: TWatch; const AThreadId: Integer;
  const AStackFrame: Integer);
begin
  Create(AOwnerWatch);
  FThreadId := AThreadId;
  FStackFrame := AStackFrame;
end;

procedure TWatchValue.Assign(AnOther: TWatchValueBase);
begin
  inherited Assign(AnOther);
  FThreadId      := TWatchValue(AnOther).FThreadId;
  FStackFrame    := TWatchValue(AnOther).FStackFrame;
  FDisplayFormat := TWatchValue(AnOther).FDisplayFormat;
end;

{ TIdeWatchesMonitor }

function TIdeWatchesMonitor.GetSupplier: TWatchesSupplier;
begin
  Result := TWatchesSupplier(inherited Supplier);
end;

function TIdeWatchesMonitor.GetSnapshot(AnID: Pointer): TIdeWatches;
begin
  Result := TIdeWatches(FSnapshots.SnapShot[AnID]);
end;

function TIdeWatchesMonitor.GetCurrentWatches: TCurrentWatches;
begin
  Result := TCurrentWatches(Watches);
end;

procedure TIdeWatchesMonitor.SetSupplier(const AValue: TWatchesSupplier);
begin
  inherited Supplier := AValue;
end;

procedure TIdeWatchesMonitor.DoStateEnterPause;
begin
  inherited DoStateEnterPause;
  if (CurrentWatches = nil) then Exit;
  CurrentWatches.ClearValues;
  NotifyUpdate(CurrentWatches, nil);
end;

procedure TIdeWatchesMonitor.DoStateLeavePause;
begin
  inherited DoStateLeavePause;
  if (CurrentWatches = nil) then Exit;
  CurrentWatches.SnapShot := nil;
end;

procedure TIdeWatchesMonitor.DoStateLeavePauseClean;
begin
  inherited DoStateLeavePauseClean;
  if (CurrentWatches = nil) then Exit;
  CurrentWatches.SnapShot := nil;
  CurrentWatches.ClearValues;  // TODO: block the update calls, update will be done for all on next line
  NotifyUpdate(CurrentWatches, nil);
end;

procedure TIdeWatchesMonitor.DoNewSupplier;
begin
  inherited DoNewSupplier;
  if Supplier <> nil then
    Supplier.CurrentWatches := CurrentWatches;
end;

procedure TIdeWatchesMonitor.DoModified;
begin
  if (FIgnoreModified = 0) and Assigned(FOnModified) then
    FOnModified(Self);
end;

procedure TIdeWatchesMonitor.NotifyAdd(const AWatches: TCurrentWatches; const AWatch: TCurrentWatch);
begin
  FNotificationList.NotifyAdd(AWatches, AWatch);
end;

procedure TIdeWatchesMonitor.NotifyRemove(const AWatches: TCurrentWatches; const AWatch: TCurrentWatch);
begin
  FNotificationList.NotifyRemove(AWatches, AWatch);
end;

procedure TIdeWatchesMonitor.NotifyUpdate(const AWatches: TCurrentWatches; const AWatch: TCurrentWatch);
begin
  FNotificationList.NotifyUpdate(AWatches, AWatch);
end;

procedure TIdeWatchesMonitor.RequestData(AWatchValue: TCurrentWatchValue);
begin
  if Supplier <> nil
  then Supplier.RequestData(AWatchValue)
  else AWatchValue.Validity := ddsInvalid;
end;

function TIdeWatchesMonitor.CreateWatches: TWatches;
begin
  Result := TCurrentWatches.Create(Self);
end;

function TIdeWatchesMonitor.CreateSnapshot(CreateEmpty: Boolean = False): TObject;
begin
  Result := TIdeWatches.Create;
  if not CreateEmpty
  then CurrentWatches.SnapShot := TIdeWatches(Result);
end;

constructor TIdeWatchesMonitor.Create;
begin
  FSnapshots := TDebuggerDataSnapShotList.Create;
  FIgnoreModified := 0;
  FNotificationList := TWatchesNotificationList.Create;
  inherited;
end;

destructor TIdeWatchesMonitor.Destroy;
begin
  FSnapshots.Clear;
  FNotificationList.Clear;
  inherited Destroy;
  FreeAndNil(FNotificationList);
  FreeAndNil(FSnapshots);
end;

procedure TIdeWatchesMonitor.AddNotification(const ANotification: TWatchesNotification);
begin
  FNotificationList.Add(ANotification);
end;

procedure TIdeWatchesMonitor.RemoveNotification(const ANotification: TWatchesNotification);
begin
  FNotificationList.Remove(ANotification);
end;

procedure TIdeWatchesMonitor.NewSnapshot(AnID: Pointer; CreateEmpty: Boolean);
var
  S: TObject;
begin
  S := CreateSnapshot(CreateEmpty);
  FSnapshots.AddSnapShot(AnID, S);
end;

procedure TIdeWatchesMonitor.RemoveSnapshot(AnID: Pointer);
begin
  FSnapshots.RemoveSnapShot(AnID);
end;

procedure TIdeWatchesMonitor.Clear;
begin
  CurrentWatches.Clear;
end;

procedure TIdeWatchesMonitor.LoadFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
begin
  CurrentWatches.LoadFromXMLConfig(AConfig, APath);
end;

procedure TIdeWatchesMonitor.SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string);
begin
  CurrentWatches.SaveToXMLConfig(AConfig, APath);
end;

procedure TIdeWatchesMonitor.BeginIgnoreModified;
begin
  inc(FIgnoreModified);
end;

procedure TIdeWatchesMonitor.EndIgnoreModified;
begin
  dec(FIgnoreModified);
end;

{ TWatchesNotificationList }

function TWatchesNotificationList.GetItem(AIndex: Integer): TWatchesNotification;
begin
  Result := TWatchesNotification(FList[AIndex]);
end;

procedure TWatchesNotificationList.NotifyAdd(const ASender: TCurrentWatches;
  const AWatch: TCurrentWatch);
var
  i: LongInt;
begin
  i := Count;
  while NextDownIndex(i) do
    if Assigned(Items[i].OnAdd) then
      Items[i].OnAdd(ASender, AWatch);
end;

procedure TWatchesNotificationList.NotifyUpdate(const ASender: TCurrentWatches;
  const AWatch: TCurrentWatch);
var
  i: LongInt;
begin
  i := Count;
  while NextDownIndex(i) do
    if Assigned(Items[i].OnUpdate) then
      Items[i].OnUpdate(ASender, AWatch);
end;

procedure TWatchesNotificationList.NotifyRemove(const ASender: TCurrentWatches;
  const AWatch: TCurrentWatch);
var
  i: LongInt;
begin
  i := Count;
  while NextDownIndex(i) do
    if Assigned(Items[i].OnRemove) then
      Items[i].OnRemove(ASender, AWatch);
end;

procedure TCurrentCallStack.SetCurrent(AValue: Integer);
begin
  inherited SetCurrent(AValue);
  FMonitor.NotifyCurrent;
end;

function TCurrentCallStack.GetCurrent: Integer;
begin
  case FCurrentValidity of
    ddsUnknown:   begin
        Result := 0;
        FCurrentValidity := ddsRequested;
        FMonitor.RequestCurrent(self);
        if FCurrentValidity = ddsValid then
          Result := inherited GetCurrent();
      end;
    ddsRequested, ddsEvaluating: Result := 0;
    ddsValid:                    Result := inherited GetCurrent;
    ddsInvalid, ddsError:        Result := 0;
  end;
end;

procedure TCurrentCallStack.Clear;
var
  Iterator: TMapIterator;
begin
  Iterator:= TMapIterator.Create(FEntries);
  while not Iterator.EOM do
  begin
    TObject(Iterator.DataPtr^).Free;
    Iterator.Next;
  end;
  Iterator.Free;
  FEntries.Clear;

  FCount := -1;
  FAtLeastCount := -1;
  FAtLeastCountOld := -1;
end;

constructor TCurrentCallStack.Create(AMonitor: TIdeCallStackMonitor);
begin
  FCount := 0;
  FAtLeastCount := 0;
  FAtLeastCountOld := -1;
  FEntries:= TMap.Create(its4, SizeOf(TCallStackEntry));
  FMonitor := AMonitor;
  FPreparing := False;
  FCountValidity := ddsUnknown;
  FAtLeastCountValidity := ddsUnknown;
  FCurrentValidity := ddsUnknown;
  FLowestUnknown :=  -1;
  FHighestUnknown := -1;
  inherited Create;
end;

destructor TCurrentCallStack.Destroy;
begin
  Clear;
  inherited Destroy;
  FreeAndNil(FEntries);
end;

procedure TCurrentCallStack.Assign(AnOther: TCallStack);
begin
  inherited Assign(AnOther);
  if AnOther is TCurrentCallStack then begin
    FCount := TCurrentCallStack(AnOther).FCount;
    FCountValidity := TCurrentCallStack(AnOther).FCountValidity;
    FAtLeastCount := TCurrentCallStack(AnOther).FAtLeastCount;
    FAtLeastCountOld := TCurrentCallStack(AnOther).FAtLeastCountOld;
  end
  else begin
    FCount := AnOther.Count;
    FAtLeastCount := -1;
    FAtLeastCountOld := -1;
  end;
end;

procedure TCurrentCallStack.SetSnapShot(const AValue: TCallStack);
begin
  assert((FSnapShot=nil) or (AValue=nil), 'TCurrentCallStack already have snapshot');
  if FSnapShot = AValue then exit;

  if (FSnapShot <> nil) and (AValue = nil)
  then FSnapShot.Assign(Self);

  FSnapShot := AValue;
end;

function TCurrentCallStack.GetCount: Integer;
begin
  case FCountValidity of
    ddsUnknown:   begin
        Result := 0;
        FCountValidity := ddsRequested;
        FMonitor.RequestCount(self);
        if FCountValidity = ddsValid then
          Result := FCount;
      end;
    ddsRequested, ddsEvaluating: Result := 0;
    ddsValid:                    Result := FCount;
    ddsInvalid, ddsError:        Result := 0;
  end;
end;

procedure TCurrentCallStack.SetCount(ACount: Integer);
begin
  if FCount = ACount then exit;
  FCount := ACount;
  FAtLeastCount := ACount;
  if FCountValidity = ddsValid then
    FMonitor.NotifyChange;
end;

function TCurrentCallStack.GetEntry(AIndex: Integer): TCallStackEntry;
begin
  if (AIndex < 0)
  or (AIndex >= CountLimited(AIndex+1)) then IndexError(Aindex);

  Result := nil;
  if FEntries.GetData(AIndex, Result) then Exit;

  Result := TCallStackEntry.Create(AIndex, 0, nil, '', nil, 0, ddsRequested);
  if Result = nil then Exit;
  FEntries.Add(AIndex, Result);
  Result.FOwner := Self;

  if (FLowestUnknown < 0) or (FLowestUnknown > AIndex)
  then FLowestUnknown := AIndex;
  if (FHighestUnknown < AIndex)
  then FHighestUnknown := AIndex;

  DoEntriesCreated;
end;

procedure TCurrentCallStack.AddEntry(AnEntry: TCallStackEntry);
begin
  FEntries.Add(AnEntry.Index, AnEntry);
  AnEntry.FOwner := Self;
end;

procedure TCurrentCallStack.AssignEntriesTo(AnOther: TCallStack);
var
  It: TMapIterator;
begin
  It := TMapIterator.Create(FEntries);
  It.First;
  while (not IT.EOM)
  do begin
    AnOther.AddEntry(TCallStackEntry.CreateCopy(TCallStackEntry(It.DataPtr^)));
    It.Next;
  end;
  It.Free;
end;

function TCurrentCallStack.GetRawEntries: TMap;
begin
  Result := FEntries;
end;

function TCurrentCallStack.GetLowestUnknown: Integer;
begin
  Result := FLowestUnknown;
end;

function TCurrentCallStack.GetHighestUnknown: Integer;
begin
  Result := FHighestUnknown;
end;

function TCurrentCallStack.GetNewCurrentIndex: Integer;
begin
  Result := FNewCurrentIndex;
end;

procedure TCurrentCallStack.PrepareRange(AIndex, ACount: Integer);
var
  It: TMapIterator;
  EndIndex: Integer;
begin
  It := TMapIterator.Create(FEntries);
  if It.Locate(AIndex)
  then repeat
    // start searching for the first unavailable
    Inc(AIndex);
    Dec(ACount);
    It.Next;
  until It.EOM or (ACount <= 0) or (TCallStackEntry(It.DataPtr^).Index <> AIndex);

  if ACount > 1
  then begin
    EndIndex := AIndex + ACount - 1;
    if It.Locate(EndIndex)
    then repeat
      // start searching for the last unavailable
      Dec(EndIndex);
      Dec(ACount);
      It.Previous;
    until It.BOM or (ACount <= 0) or (TCallStackEntry(It.DataPtr^).Index <> EndIndex);
  end;
  It.Free;
  if ACount <= 0 then Exit;

  FPreparing := True;
  while ACount > 0 do begin
    Entries[AIndex]; // Request unknown entries: will set LowesUnknown / HighesUnknown
    inc(AIndex);
    dec(ACount);
  end;
  FPreparing := False;
  DoEntriesCreated;
end;

procedure TCurrentCallStack.ChangeCurrentIndex(ANewIndex: Integer);
begin
  FNewCurrentIndex := ANewIndex;
  FMonitor.UpdateCurrentIndex;
end;

procedure TCurrentCallStack.DoEntriesCreated;
begin
  if not FPreparing
  then FMonitor.RequestEntries(Self);
end;

procedure TCurrentCallStack.DoEntriesUpdated;
begin
  FLowestUnknown := -1;
  FHighestUnknown := -1;
  FMonitor.NotifyChange;
end;

function TCurrentCallStack.HasAtLeastCount(ARequiredMinCount: Integer): TNullableBool;
begin
  if FCountValidity = ddsValid then begin
    Result := inherited HasAtLeastCount(ARequiredMinCount);
    exit;
  end;

  if FAtLeastCountOld >= ARequiredMinCount then begin
    Result := nbTrue;
    exit;
  end;

  if (FAtLeastCountValidity = ddsValid) and (FAtLeastCount < ARequiredMinCount) then begin
    FAtLeastCountOld := FAtLeastCount;
    FAtLeastCountValidity := ddsUnknown;
  end;

  case FAtLeastCountValidity of
    ddsUnknown:   begin
        Result := nbUnknown;
        if FCountValidity in [ddsRequested, ddsEvaluating] then
          exit;

        FAtLeastCountValidity := ddsRequested;
        FMonitor.RequestAtLeastCount(self, ARequiredMinCount);
        if FCountValidity = ddsValid then
          Result := inherited HasAtLeastCount(ARequiredMinCount)
        else
        if FAtLeastCountValidity = ddsValid then begin
          if ARequiredMinCount <= FAtLeastCount then
            Result := nbTrue
          else
            Result := nbFalse;
        end;
      end;
    ddsRequested, ddsEvaluating: Result := nbUnknown;
    ddsValid: begin
        if ARequiredMinCount <= FAtLeastCount then
          Result := nbTrue
        else
          Result := nbFalse;
      end;
    ddsInvalid, ddsError:        Result := nbFalse;
  end;
end;

procedure TCurrentCallStack.SetCountValidity(AValidity: TDebuggerDataState);
begin
  if FCountValidity = AValidity then exit;
  DebugLn(DBG_DATA_MONITORS, ['DebugDataMonitor: TCurrentCallStack.SetCountValidity: FThreadId=', FThreadId, ' AValidity=',dbgs(AValidity)]);
  FCountValidity := AValidity;
  FMonitor.NotifyChange;
end;

procedure TCurrentCallStack.SetHasAtLeastCountInfo(AValidity: TDebuggerDataState;
  AMinCount: Integer);
begin
  if (FAtLeastCountValidity = AValidity) then exit;
  DebugLn(DBG_DATA_MONITORS, ['DebugDataMonitor: TCurrentCallStack.SetCountMinValidity: FThreadId=', FThreadId, ' AValidity=',dbgs(AValidity)]);
  FAtLeastCountOld := -1;
  FAtLeastCountValidity := AValidity;
  FAtLeastCount := AMinCount;
  FMonitor.NotifyChange;
end;

procedure TCurrentCallStack.SetCurrentValidity(AValidity: TDebuggerDataState);
begin
  if FCurrentValidity = AValidity then exit;
  DebugLn(DBG_DATA_MONITORS, ['DebugDataMonitor: TCurrentCallStack.SetCurrentValidity: FThreadId=', FThreadId, ' AValidity=',dbgs(AValidity)]);
  FCurrentValidity := AValidity;
  if FCurrentValidity = ddsValid then
    FMonitor.NotifyChange;
  FMonitor.NotifyCurrent;
end;

{ TCurrentCallStackList }

constructor TCurrentCallStackList.Create(AMonitor: TIdeCallStackMonitor);
begin
  FMonitor := AMonitor;
  inherited Create;
end;

procedure TCurrentCallStackList.SetSnapShot(const AValue: TIdeCallStackList);
var
  R: TCallStack;
  i: Integer;
begin
  assert((FSnapShot=nil) or (AValue=nil), 'Callstack already have snapshot');
  if FSnapShot = AValue then exit;
  FSnapShot := AValue;

  if FSnapShot = nil then begin
    for i := 0 to Count - 1 do
      TCurrentCallStack(Entries[i]).SnapShot := nil;
  end
  else begin
    // Assign
    FSnapShot.Clear;
    for i := 0 to Count - 1 do begin
      R := TCallStack.Create;
      R.ThreadId := Entries[i].ThreadId;
      FSnapShot.Add(R);
      TCurrentCallStack(Entries[i]).SnapShot := R;
    end;
  end;
end;

function TCurrentCallStackList.GetEntryForThread(const AThreadId: Integer): TCallStack;
var
  R: TCallStack;
begin
  Result := inherited GetEntryForThread(AThreadId);
  if Result = nil then begin
    try DebugLnEnter(DBG_DATA_MONITORS, ['DebugDataMonitor: >>ENTER: TCurrentCallStackList.GetEntryForThread: ThreadId=', AThreadId]);
    Result := TCurrentCallStack.Create(FMonitor);
    Result.ThreadId := AThreadId;
    Add(Result);
    if FSnapShot <> nil then begin
      R := TCallStack.Create;
      R.ThreadId := AThreadId;
      FSnapShot.Add(R);
      TCurrentCallStack(Result).SnapShot := R;
    end;
    finally DebugLnExit(DBG_DATA_MONITORS, ['DebugDataMonitor: <<EXIT: TCurrentCallStackList.GetEntryForThread' ]) end;
  end;
end;

{ TCallStackList }

function TIdeCallStackList.GetEntry(const AIndex: Integer): TCallStack;
begin
  Result := TCallStack(FList[AIndex]);
end;

function TIdeCallStackList.GetEntryForThread(const AThreadId: Integer): TCallStack;
var
  i: Integer;
begin
  i := Count - 1;
  while (i >= 0) and (TCallStack(FList[i]).ThreadId <> AThreadId) do dec(i);
  if i >= 0
  then Result := TCallStack(FList[i])
  else Result := nil;
end;

function TIdeCallStackList.GetEntryBase(const AIndex: Integer): TCallStackBase;
begin
  Result := TCallStackBase(GetEntry(AIndex));
end;

function TIdeCallStackList.GetEntryForThreadBase(const AThreadId: Integer): TCallStackBase;
begin
  Result := TCallStackBase(GetEntryForThread(AThreadId));
end;

procedure TIdeCallStackList.Add(ACallStack: TCallStack);
begin
  assert(((Self is TCurrentCallStackList) and (ACallStack is TCurrentCallStack)) or ((not(Self is TCurrentCallStackList)) and not(ACallStack is TCurrentCallStack)),
         'TCallStackList.Add: entry and list differ (current and none current)');
  FList.Add(ACallStack);
end;

procedure TIdeCallStackList.LoadDataFromXMLConfig(const AConfig: TXMLConfig;
  APath: string; AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
var
  c, i: Integer;
  e: TCallStack;
begin
  Clear;
  c := AConfig.GetValue(APath + 'Count', 0);
  APath := APath + 'Entry';
  for i := 0 to c - 1 do begin
    e := TCallStack.Create;
    e.LoadDataFromXMLConfig(AConfig, APath + IntToStr(i) + '/', AUnitInvoPrv);
    Add(e);
  end;
end;

procedure TIdeCallStackList.SaveDataToXMLConfig(const AConfig: TXMLConfig; APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
var
  i: Integer;
begin
  AConfig.SetDeleteValue(APath + 'Count', Count, 0);
  APath := APath + 'Entry';
  for i := 0 to Count - 1 do
    Entries[i].SaveDataToXMLConfig(AConfig, APath + IntToStr(i) + '/', AUnitInvoPrv);
end;

procedure TIdeCallStackList.Assign(AnOther: TIdeCallStackList);
var
  i: Integer;
begin
  Clear;
  for i := 0 to AnOther.FList.Count-1 do
    FList.Add(TCallStack.CreateCopy(TCallStack(AnOther.FList[i])));
end;

constructor TIdeCallStackList.Create;
begin
  FList := TList.Create;
end;

destructor TIdeCallStackList.Destroy;
begin
  inherited Destroy;
  Clear;
  FreeAndNil(FList);
end;

procedure TIdeCallStackList.Clear;
begin
  while FList.Count > 0 do begin
    TObject(FList[0]).Free;
    FList.Delete(0);
  end;
end;

function TIdeCallStackList.Count: Integer;
begin
  Result := FList.Count;
end;

{ TCurrentThreads }

procedure TCurrentThreads.SetValidity(AValidity: TDebuggerDataState);
begin
  if FDataValidity = AValidity then exit;
  DebugLn(DBG_DATA_MONITORS, ['DebugDataMonitor: TCurrentThreads.SetValidity ', dbgs(AValidity)]);

  // Assign snapshot, if old data wasn't final
  if (FDataValidity in [ddsUnknown, ddsEvaluating, ddsRequested]) and (FSnapShot <> nil)
  then FSnapShot.Assign(self);

  FDataValidity := AValidity;

  if FDataValidity = ddsUnknown then Clear;
  FMonitor.Changed;
end;

procedure TCurrentThreads.SetCurrentThreadId(AValue: Integer);
begin
  if FCurrentThreadId = AValue then exit;
  DebugLn(DBG_DATA_MONITORS, ['DebugDataMonitor: TCurrentThreads.SetCurrentThreadId ', AValue]);
  inherited SetCurrentThreadId(AValue);
  FMonitor.CurrentChanged; // TODO ChangedSelection
end;

procedure TCurrentThreads.SetSnapShot(const AValue: TIdeThreads);
begin
  assert((FSnapShot=nil) or (AValue=nil), 'Threads already have snapshot');
  if FSnapShot = AValue then exit;
  FSnapShot := AValue;
  if FSnapShot <> nil
  then FSnapShot.Assign(self);
end;

constructor TCurrentThreads.Create(AMonitor: TIdeThreadsMonitor);
begin
  FMonitor := AMonitor;
  FDataValidity := ddsUnknown;
  inherited Create;
end;

function TCurrentThreads.Count: Integer;
begin
  if (FDataValidity = ddsUnknown) and Paused then begin
    FDataValidity := ddsRequested;
    Paused := False;
    FMonitor.RequestData;
  end;

  Result := inherited Count;

  //case FDataValidity of
  //  ddsUnknown:   begin
  //      Result := 0;
  //      FDataValidity := ddsRequested;
  //      FMonitor.RequestData;
  //      if FDataValidity = ddsValid then Result := inherited Count();
  //    end;
  //  ddsRequested, ddsEvaluating: Result := 0;
  //  ddsValid:                    Result := inherited Count;
  //  ddsInvalid, ddsError:        Result := 0;
  //end;
end;

procedure TCurrentThreads.Clear;
begin
  FDataValidity := ddsUnknown;
  inherited Clear;
end;

function TCurrentThreads.CreateEntry(const AIndex: Integer; const AnAdress: TDbgPtr;
  const AnArguments: TStrings; const AFunctionName: String; const FileName, FullName: String;
  const ALine: Integer; const AThreadId: Integer; const AThreadName: String;
  const AThreadState: String; AState: TDebuggerDataState): TCallStackEntryBase;
begin
  Result := inherited CreateEntry(AIndex, AnAdress, AnArguments, AFunctionName, FileName,
    FullName, ALine, AThreadId, AThreadName, AThreadState, AState);
  TThreadEntry(Result).FThreadOwner := self;
end;

{ TIdeThreadsMonitor }

function TIdeThreadsMonitor.GetSupplier: TThreadsSupplier;
begin
  Result := TThreadsSupplier(inherited Supplier);
end;

function TIdeThreadsMonitor.GetSnapshot(AnID: Pointer): TIdeThreads;
begin
  Result := TIdeThreads(FSnapshots.SnapShot[AnID]);
end;

function TIdeThreadsMonitor.GetCurrentThreads: TCurrentThreads;
begin
  Result :=TCurrentThreads(Threads);
end;

procedure TIdeThreadsMonitor.SetSupplier(const AValue: TThreadsSupplier);
begin
  inherited Supplier := AValue;
end;

procedure TIdeThreadsMonitor.DoModified;
begin
  Changed;
end;

procedure TIdeThreadsMonitor.DoStateEnterPause;
begin
  inherited DoStateEnterPause;
  if (CurrentThreads = nil) then Exit;
  CurrentThreads.SetValidity(ddsUnknown);
  CurrentThreads.Paused := True;
end;

procedure TIdeThreadsMonitor.DoStateLeavePause;
begin
  inherited DoStateLeavePause;
  if (CurrentThreads = nil) then Exit;
  CurrentThreads.SnapShot := nil;
end;

procedure TIdeThreadsMonitor.DoStateLeavePauseClean;
begin
  inherited DoStateLeavePauseClean;
  if (CurrentThreads = nil) then Exit;
  CurrentThreads.SnapShot := nil;
end;

procedure TIdeThreadsMonitor.DoNewSupplier;
begin
  inherited DoNewSupplier;
  if CurrentThreads <> nil then
    CurrentThreads.SetValidity(ddsUnknown);
  if Supplier <> nil then
    Supplier.CurrentThreads := CurrentThreads;
end;

procedure TIdeThreadsMonitor.RequestData;
begin
  if Supplier <> nil
  then Supplier.RequestMasterData;
end;

function TIdeThreadsMonitor.CreateSnapshot(CreateEmpty: Boolean = False): TObject;
begin
  Result := TIdeThreads.Create;
  if not CreateEmpty
  then CurrentThreads.SnapShot := TIdeThreads(Result);
end;

function TIdeThreadsMonitor.CreateThreads: TThreads;
begin
  Result := TCurrentThreads.Create(self);
end;

procedure TIdeThreadsMonitor.Changed;
begin
  FNotificationList.NotifyChange(Self);
end;

procedure TIdeThreadsMonitor.CurrentChanged;
begin
  FNotificationList.NotifyChange(Self); // TODO: is this required?? It should not
  FNotificationList.NotifyCurrent(Self);
end;

constructor TIdeThreadsMonitor.Create;
begin
  FSnapshots := TDebuggerDataSnapShotList.Create;
  inherited;
  FNotificationList := TDebuggerChangeNotificationList.Create;
end;

destructor TIdeThreadsMonitor.Destroy;
begin
  FSnapshots.Clear;
  FNotificationList.Clear;
  inherited Destroy;
  FreeAndNil(FNotificationList);
  FreeAndNil(FSnapshots);
end;

procedure TIdeThreadsMonitor.Clear;
begin
  DebugLn(DBG_DATA_MONITORS, ['DebugDataMonitor: TIdeThreadsMonitor.Clear']);
  CurrentThreads.Clear;
  Changed;
end;

procedure TIdeThreadsMonitor.AddNotification(const ANotification: TThreadsNotification);
begin
  FNotificationList.Add(ANotification);
end;

procedure TIdeThreadsMonitor.RemoveNotification(const ANotification: TThreadsNotification);
begin
  FNotificationList.Remove(ANotification);
end;

procedure TIdeThreadsMonitor.NewSnapshot(AnID: Pointer; CreateEmpty: Boolean);
var
  S: TObject;
begin
  S := CreateSnapshot(CreateEmpty);
  FSnapshots.AddSnapShot(AnID, S);
end;

procedure TIdeThreadsMonitor.RemoveSnapshot(AnID: Pointer);
begin
  FSnapshots.RemoveSnapShot(AnID);
end;

procedure TIdeThreadsMonitor.ChangeCurrentThread(ANewId: Integer);
begin
  if Supplier <> nil
  then Supplier.ChangeCurrentThread(ANewId);
end;

{ TDebuggerChangeNotificationList }

function TDebuggerChangeNotificationList.GetItem(AIndex: Integer): TDebuggerChangeNotification;
begin
  Result := TDebuggerChangeNotification(FList[AIndex]);
end;

procedure TDebuggerChangeNotificationList.NotifyChange(Sender: TObject);
var
  i: LongInt;
begin
  i := Count;
  while NextDownIndex(i) do
    if Assigned(Items[i].OnChange) then
      Items[i].OnChange(Sender);
end;

procedure TDebuggerChangeNotificationList.NotifyCurrent(Sender: TObject);
var
  i: LongInt;
begin
  i := Count;
  while NextDownIndex(i) do
    if Assigned(Items[i].OnCurrent) then
      Items[i].OnCurrent(Sender);
end;

{ TDebuggerNotificationList }

function TDebuggerNotificationList.GetItem(AIndex: Integer): TDebuggerNotification;
begin
  Result := TDebuggerNotification(FList[AIndex]);
end;

function TDebuggerNotificationList.NextDownIndex(var Index: integer): boolean;
begin
  dec(Index);
  if (Index >= FList.Count) then
    Index := FList.Count-1;
  Result := Index >= 0;
end;

function TDebuggerNotificationList.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TDebuggerNotificationList.Clear;
begin
  while Count > 0 do
    Remove(Items[0]);
end;

constructor TDebuggerNotificationList.Create;
begin
  FList := TList.Create;
end;

destructor TDebuggerNotificationList.Destroy;
begin
  inherited Destroy;
  Clear;
  FreeAndNil(FList);
end;

procedure TDebuggerNotificationList.Add(const ANotification: TDebuggerNotification);
begin
  FList.Add(ANotification);
  ANotification.AddReference;
end;

procedure TDebuggerNotificationList.Remove(const ANotification: TDebuggerNotification);
begin
  ANotification.ReleaseReference;
  FList.Remove(ANotification);
end;

{ TThreadEntry }

function TThreadEntry.GetUnitInfoProvider: TDebuggerUnitInfoProvider;
begin
  if FThreadOwner = nil then
    Result := nil
  else
    Result := (FThreadOwner as TCurrentThreads).FMonitor.UnitInfoProvider;
end;

function TThreadEntry.GetThreadId: Integer;
begin
  Result := FThreadId;
end;

function TThreadEntry.GetThreadName: String;
begin
  Result := FThreadName;
end;

function TThreadEntry.GetThreadState: String;
begin
  Result := FThreadState;
end;

procedure TThreadEntry.SetThreadState(AValue: String);
begin
  if FThreadState = AValue then Exit;
  FThreadState := AValue;
  ClearLocation;
end;

procedure TThreadEntry.LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath, AUnitInvoPrv);
  FThreadId    := AConfig.GetValue(APath + 'ThreadId', -1);
  FThreadName  := AConfig.GetValue(APath + 'ThreadName', '');
  FThreadState := AConfig.GetValue(APath + 'ThreadState', '');
end;

procedure TThreadEntry.SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
begin
  inherited SaveDataToXMLConfig(AConfig, APath, AUnitInvoPrv);
  AConfig.SetValue(APath + 'ThreadId', FThreadId);
  AConfig.SetValue(APath + 'ThreadName', FThreadName);
  AConfig.SetValue(APath + 'ThreadState', FThreadState);
end;

constructor TThreadEntry.Create(const AIndex: Integer; const AnAdress: TDbgPtr;
  const AnArguments: TStrings; const AFunctionName: String; const FileName, FullName: String;
  const ALine: Integer; const AThreadId: Integer; const AThreadName: String;
  const AThreadState: String; AState: TDebuggerDataState);
var
  loc: TDebuggerUnitInfo;
begin
  if GetUnitInfoProvider = nil then
    loc := nil
  else
    loc := GetUnitInfoProvider.GetUnitInfoFor(FileName, FullName);
  inherited Create(AIndex, AnAdress, AnArguments, AFunctionName,
                   loc, ALine, AState);
  FThreadId    := AThreadId;
  FThreadName  := AThreadName;
  FThreadState := AThreadState;
end;

constructor TThreadEntry.CreateCopy(const ASource: TThreadEntry);
begin
  inherited CreateCopy(ASource);
  FThreadId    := ASource.FThreadId;
  FThreadName  := ASource.FThreadName;
  FThreadState := ASource.FThreadState;
end;

{ TIdeThreads }

function TIdeThreads.GetEntry(const AnIndex: Integer): TThreadEntry;
begin
  if (AnIndex < 0) or (AnIndex >= Count) then exit(nil);
  Result := TThreadEntry(FList[AnIndex]);
end;

function TIdeThreads.GetEntryById(const AnID: Integer): TThreadEntry;
var
  i: Integer;
begin
  i := Count - 1;
  while i >= 0 do begin
    Result := Entries[i];
    if Result.ThreadId = AnID then
      exit;
    dec(i);
  end;
  Result := nil;
end;

procedure TIdeThreads.SetCurrentThreadId(AValue: Integer);
begin
  if FCurrentThreadId = AValue then exit;
  FCurrentThreadId := AValue;
end;

function TIdeThreads.GetCurrentThreadId: Integer;
begin
  Result := FCurrentThreadId;
end;

function TIdeThreads.GetEntryBase(const AnIndex: Integer): TCallStackEntryBase;
begin
  Result := TCallStackEntryBase(GetEntry(AnIndex));
end;

function TIdeThreads.GetEntryByIdBase(const AnID: Integer): TCallStackEntryBase;
begin
  Result := TCallStackEntryBase(GetEntryById(AnID));
end;

procedure TIdeThreads.Assign(AOther: TIdeThreads);
var
  i: Integer;
begin
  Clear;
  FCurrentThreadId := AOther.FCurrentThreadId;
  for i := 0 to AOther.FList.Count-1 do
    FList.Add(TThreadEntry.CreateCopy(TThreadEntry(AOther.FList[i])));
end;

procedure TIdeThreads.LoadDataFromXMLConfig(const AConfig: TXMLConfig; APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
var
  c, i: Integer;
  e: TThreadEntry;
begin
  Clear;
  FCurrentThreadId   := AConfig.GetValue(APath + 'CurrentThreadId', -1);
  c := AConfig.GetValue(APath + 'Count', 0);
  APath := APath + 'Entry';
  for i := 0 to c - 1 do begin
    e := TThreadEntry.Create;
    e.LoadDataFromXMLConfig(AConfig, APath + IntToStr(i) + '/', AUnitInvoPrv);
    FList.Add(e);
  end;
end;

procedure TIdeThreads.SaveDataToXMLConfig(const AConfig: TXMLConfig; APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
var
  i: Integer;
begin
  AConfig.SetValue(APath + 'CurrentThreadId', FCurrentThreadId);
  AConfig.SetDeleteValue(APath + 'Count', Count, 0);
  APath := APath + 'Entry';
  for i := 0 to Count - 1 do
    Entries[i].SaveDataToXMLConfig(AConfig, APath + IntToStr(i) + '/', AUnitInvoPrv);
end;

constructor TIdeThreads.Create;
begin
  FList := TList.Create;
end;

destructor TIdeThreads.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TIdeThreads.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TIdeThreads.Clear;
begin
  while FList.Count > 0 do begin
    TThreadEntry(Flist[0]).Free;
    FList.Delete(0);
  end;
end;

procedure TIdeThreads.Add(AThread: TCallStackEntryBase);
begin
  FList.Add(TThreadEntry.CreateCopy(AThread as TThreadEntry));
  if FList.Count = 1 then
    FCurrentThreadId := (AThread as TThreadEntry).ThreadId;
end;

procedure TIdeThreads.Remove(AThread: TCallStackEntryBase);
begin
  FList.Remove(AThread);
  if FCurrentThreadId = (AThread as TThreadEntry).ThreadId then begin
    if FList.Count > 0 then
      FCurrentThreadId := Entries[0].ThreadId
    else
      FCurrentThreadId := 0;
  end;
  AThread.Free;
end;

function TIdeThreads.CreateEntry(const AIndex: Integer; const AnAdress: TDbgPtr;
  const AnArguments: TStrings; const AFunctionName: String; const FileName, FullName: String;
  const ALine: Integer; const AThreadId: Integer; const AThreadName: String;
  const AThreadState: String; AState: TDebuggerDataState): TCallStackEntryBase;
begin
  Result := TThreadEntry.Create(AIndex, AnAdress, AnArguments, AFunctionName, FileName,
    FullName, ALine, AThreadId, AThreadName, AThreadState, AState);
  TThreadEntry(Result).FThreadOwner := self;
end;

procedure TIdeThreads.SetValidity(AValidity: TDebuggerDataState);
begin
  assert(false, 'TIdeThreads.SetValidity');
end;

(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   B R E A K P O I N T S                                                  **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

{ =========================================================================== }
{ TIDEBreakPoint }
{ =========================================================================== }

function TIDEBreakPoint.GetAutoContinueTime: Cardinal;
begin
  Result := FAutoContinueTime;
end;

procedure TIDEBreakPoint.SetAutoContinueTime(const AValue: Cardinal);
begin
  if FAutoContinueTime = AValue then Exit;
  FAutoContinueTime := AValue;
  //Changed;
  DoUserChanged;
end;

procedure TIDEBreakPoint.SetLogEvalExpression(AValue: String);
begin
  if FLogEvalExpression <> AValue then
  begin
    FLogEvalExpression := AValue;
    //Changed;
    DoUserChanged;
  end;
end;

procedure TIDEBreakPoint.SetLogMessage(const AValue: String);
begin
  if FLogMessage <> AValue then
  begin
    FLogMessage := AValue;
    //Changed;
    DoUserChanged;
  end;
end;

function TIDEBreakPoint.GetLogMessage: String;
begin
  Result := FLogMessage;
end;

function TIDEBreakPoint.GetLogCallStackLimit: Integer;
begin
  Result := FLogCallStackLimit;
end;

procedure TIDEBreakPoint.SetLogCallStackLimit(const AValue: Integer);
begin
  if FLogCallStackLimit <> AValue then
  begin
    FLogCallStackLimit := AValue;
    //Changed;
    DoUserChanged;
  end;
end;

procedure TIDEBreakPoint.AssignLocationTo(Dest: TPersistent);
var
  DestBreakPoint: TBaseBreakPoint absolute Dest;
begin
  if DestBreakPoint is TDBGBreakPoint then
    DestBreakPoint.SetLocation(Source, DebugExeLine)
  else
    inherited;
end;

procedure TIDEBreakPoint.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TIDEBreakPoint
  then begin
    TIDEBreakPoint(Dest).Actions := FActions;
    TIDEBreakPoint(Dest).AutoContinueTime := FAutoContinueTime;
    TIDEBreakPoint(Dest).Group := FGroup;
    TIDEBreakPoint(Dest).LogEvalExpression := FLogEvalExpression;
    TIDEBreakPoint(Dest).LogMessage := FLogMessage;
    TIDEBreakPoint(Dest).LogCallStackLimit := FLogCallStackLimit;
    TIDEBreakPoint(Dest).EnableGroupList.Assign(FEnableGroupList);
    TIDEBreakPoint(Dest).DisableGroupList.Assign(FDisableGroupList);
  end;

  if (Collection <> nil) and (TIDEBreakPoints(Collection).FMaster <> nil)
  and (Dest is TDBGBreakPoint)
  then begin
    Assert(FMaster=nil, 'TManagedBreakPoint.AssignTO already has Master');
    if FMaster <> nil then FMaster.Slave := nil;
    FMaster := TDBGBreakPoint(Dest);
    FMaster.Slave := Self;
  end;
end;

procedure TIDEBreakPoint.DoChanged;
begin
  if (FMaster <> nil)
  and (FMaster.Slave = nil)
  then FMaster := nil;

  inherited DoChanged;
end;

procedure TIDEBreakPoint.DoUserChanged;
begin
  FUserModified := True;
  DoChanged;
end;

function TIDEBreakPoint.GetHitCount: Integer;
begin
  if FMaster = nil
  then Result := 0
  else Result := FMaster.HitCount;
end;

function TIDEBreakPoint.GetValid: TValidState;
begin
  if FMaster = nil
  then Result := vsUnknown
  else Result := FMaster.Valid;
end;

procedure TIDEBreakPoint.SetBreakHitCount(const AValue: Integer);
begin
  if BreakHitCount = AValue then exit;
  inherited SetBreakHitCount(AValue);
  DoUserChanged;
  if FMaster <> nil then FMaster.BreakHitCount := AValue;
end;

procedure TIDEBreakPoint.SetEnabled(const AValue: Boolean);
begin
  if Enabled = AValue then exit;
  inherited SetEnabled(AValue);
  InitialEnabled:=Enabled;
  if FMaster <> nil then FMaster.Enabled := AValue;
end;

procedure TIDEBreakPoint.SetInitialEnabled(const AValue: Boolean);
begin
  if InitialEnabled = AValue then exit;
  inherited SetInitialEnabled(AValue);
  DoUserChanged;
  if FMaster <> nil then FMaster.InitialEnabled := AValue;
end;

procedure TIDEBreakPoint.SetExpression(const AValue: String);
begin
  if AValue=Expression then exit;
  inherited SetExpression(AValue);
  DoUserChanged;
  if FMaster <> nil then FMaster.Expression := AValue;
end;

function TIDEBreakPoint.DebugExeLine: Integer;
begin
  Result := Line;
end;

procedure TIDEBreakPoint.ClearAllGroupLists;
begin
  FDisableGroupList.Clear;
  FEnableGroupList.Clear;
end;

{$IFDEF DBG_BREAKPOINT}
function TIDEBreakPoint.DebugText: string;
var
  s: String;
begin
  WriteStr(s, FKind);
  Result := dbgs(self) + ' ' + s + ' at ' + Source +':' + IntToStr(Line);
end;
{$ENDIF}

constructor TIDEBreakPoint.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FGroup := nil;
  FActions := [bpaStop];
  FDisableGroupList := TIDEBreakPointGroupList.Create(Self);
  FEnableGroupList := TIDEBreakPointGroupList.Create(Self);
end;

destructor TIDEBreakPoint.Destroy;
var
  Grp: TIDEBreakPointGroup;
begin
  if FMaster <> nil
  then begin
    FMaster.Slave := nil;
    ReleaseRefAndNil(FMaster);
  end;

  if (TIDEBreakPoints(Collection) <> nil)
  then TIDEBreakPoints(Collection).NotifyRemove(Self);

  Grp := FGroup;
  FGroup := nil;
  if Grp <> nil
  then Grp.Remove(Self);

  ClearAllGroupLists;

  inherited;
  FreeAndNil(FDisableGroupList);
  FreeAndNil(FEnableGroupList);
end;

procedure TIDEBreakPoint.DisableGroups;
var
  n: Integer;
begin
  {$IFDEF DBG_BREAKPOINT}
  DebugLn(['DisableGroups: ', DebugText, ' Cnt=',  FDisableGroupList.Count]);
  {$ENDIF}
  for n := 0 to FDisableGroupList.Count - 1 do
    FDisableGroupList[n].Enabled := False;
end;

procedure TIDEBreakPoint.DoActionChange;
begin
  //Changed;
  DoUserChanged;
end;

procedure TIDEBreakPoint.DoHit(const ACount: Integer; var AContinue: Boolean);
begin
  inherited DoHit(ACount, AContinue);
  AContinue := AContinue or not (bpaStop in Actions);
  if bpaLogMessage in Actions
  then FMaster.DoLogMessage(FLogMessage);
  if (bpaEValExpression in Actions) and (Trim(FLogEvalExpression) <> '')
  then FMaster.DoLogExpression(Trim(FLogEvalExpression));
  if bpaLogCallStack in Actions
  then FMaster.DoLogCallStack(FLogCallStackLimit);
  // SnapShot is taken in TDebugManager.DebuggerChangeState
  if bpaEnableGroup in Actions
  then EnableGroups;
  if bpaDisableGroup in Actions
  then DisableGroups;
end;

procedure TIDEBreakPoint.EnableGroups;
var
  n: Integer;
begin
  {$IFDEF DBG_BREAKPOINT}
  DebugLn(['EnableGroups: ', DebugText, ' Cnt=',  FEnableGroupList.Count]);
  {$ENDIF}

  for n := 0 to FEnableGroupList.Count - 1 do
    FEnableGroupList[n].Enabled := True;
end;

function TIDEBreakPoint.GetActions: TIDEBreakPointActions;
begin
  Result := FActions;
end;

function TIDEBreakPoint.GetGroup: TIDEBreakPointGroup;
begin
  Result := FGroup;
end;

procedure TIDEBreakPoint.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; const OnLoadFilename: TOnLoadFilenameFromConfig;
  const OnGetGroup: TOnGetGroupByName);

  procedure LoadGroupList(GroupList: TIDEBreakPointGroupList; const ListPath: string);
  var
    i: Integer;
    CurGroup: TIDEBreakPointGroup;
    NewCount: Integer;
    GroupName: String;
  begin
    GroupList.Clear;
    NewCount:=XMLConfig.GetValue(ListPath+'Count',0);
    for i:=0 to NewCount-1 do begin
      GroupName:=XMLConfig.GetValue(ListPath+'Group'+IntToStr(i+1)+'/Name','');
      if GroupName='' then continue;
      CurGroup:=OnGetGroup(GroupName);
      if CurGroup=nil then continue;
      GroupList.Add(CurGroup);
    end;
  end;

var
  Filename: String;
  GroupName: String;
  NewActions: TIDEBreakPointActions;
  CurAction: TIDEBreakPointAction;
begin
  FLoading:=true;
  try
    Kind:=TDBGBreakPointKind(GetEnumValueDef(TypeInfo(TDBGBreakPointKind),XMLConfig.GetValue(Path+'Kind/Value',''),0));
    GroupName:=XMLConfig.GetValue(Path+'Group/Name','');
    Group:=OnGetGroup(GroupName);
    Expression:=XMLConfig.GetValue(Path+'Expression/Value','');
    AutoContinueTime:=XMLConfig.GetValue(Path+'AutoContinueTime/Value',0);
    BreakHitCount := XMLConfig.GetValue(Path+'BreakHitCount/Value',0);

    Address:=XMLConfig.GetValue(Path+'Address/Value',0);

    FWatchData := XMLConfig.GetValue(Path+'WatchData/Value', '');
    try ReadStr(XMLConfig.GetValue(Path+'WatchScope/Value', 'wpsGlobal'), FWatchScope);
    except FWatchScope := wpsGlobal; end;
    try ReadStr(XMLConfig.GetValue(Path+'WatchKind/Value', 'wpkWrite'), FWatchKind);
    except FWatchKind:= wpkWrite; end;

    Filename:=XMLConfig.GetValue(Path+'Source/Value','');
    if Assigned(OnLoadFilename) then OnLoadFilename(Filename);
    FSource:=Filename;

    InitialEnabled:=XMLConfig.GetValue(Path+'InitialEnabled/Value',true);
    Enabled:=FInitialEnabled;
    FLine:=XMLConfig.GetValue(Path+'Line/Value',-1);
    FLogEvalExpression := XMLConfig.GetValue(Path+'LogEvalExpression/Value', '');
    FLogMessage:=XMLConfig.GetValue(Path+'LogMessage/Value','');
    FLogCallStackLimit:=XMLConfig.GetValue(Path+'LogCallStackLimit/Value',0);
    NewActions:=[];
    for CurAction:=Low(TIDEBreakPointAction) to High(TIDEBreakPointAction) do
      if XMLConfig.GetValue(
          Path+'Actions/'+DBGBreakPointActionNames[CurAction],
          CurAction in [bpaStop])
      then Include(NewActions,CurAction);
    Actions:=NewActions;
    LoadGroupList(FDisableGroupList,Path+'DisableGroups/');
    LoadGroupList(FEnableGroupList,Path+'EnableGroups/');
  finally
    FLoading:=false;
  end;
end;

procedure TIDEBreakPoint.SaveToXMLConfig(const AConfig: TXMLConfig;
  const APath: string; const OnSaveFilename: TOnSaveFilenameToConfig);

  procedure SaveGroupList(const AList: TIDEBreakPointGroupList; const AListPath: string);
  var
    i: Integer;
    CurGroup: TIDEBreakPointGroup;
  begin
    AConfig.SetDeleteValue(AListPath + 'Count', AList.Count,0);
    for i := 0 to AList.Count - 1 do
    begin
      CurGroup := AList[i];
      AConfig.SetDeleteValue(AListPath+'Group'+IntToStr(i+1)+'/Name', CurGroup.Name, '');
    end;
  end;

var
  s, Filename: String;
  CurAction: TIDEBreakPointAction;
begin
  AConfig.SetDeleteValue(APath+'Kind/Value',GetEnumName(TypeInfo(TDBGBreakPointKind), Ord(Kind)), '');
  AConfig.SetDeleteValue(APath+'Address/Value',Address,0);

  AConfig.SetDeleteValue(APath+'WatchData/Value', FWatchData, '');
  WriteStr(s{%H-}, FWatchScope);
  AConfig.SetDeleteValue(APath+'WatchScope/Value', s, '');
  WriteStr(s, FWatchKind);
  AConfig.SetDeleteValue(APath+'WatchKind/Value', s, '');

  if Group <> nil
  then AConfig.SetDeleteValue(APath+'Group/Name',Group.Name,'');

  AConfig.SetDeleteValue(APath+'Expression/Value',Expression,'');
  AConfig.SetDeleteValue(APath+'AutoContinueTime/Value',AutoContinueTime,0);
  AConfig.SetDeleteValue(APath+'BreakHitCount/Value',BreakHitCount,0);

  Filename := Source;
  if Assigned(OnSaveFilename) then OnSaveFilename(Filename);

  AConfig.SetDeleteValue(APath+'Source/Value',Filename,'');
  AConfig.SetDeleteValue(APath+'InitialEnabled/Value',InitialEnabled,true);
  AConfig.SetDeleteValue(APath+'Line/Value',Line,-1);
  AConfig.SetDeleteValue(APath+'LogEvalExpression/Value', FLogEvalExpression,'');
  AConfig.SetDeleteValue(APath+'LogMessage/Value',LogMessage,'');
  AConfig.SetDeleteValue(APath+'LogCallStackLimit/Value',LogCallStackLimit,0);

  for CurAction := Low(TIDEBreakPointAction) to High(TIDEBreakPointAction) do
  begin
    AConfig.SetDeleteValue(
        APath+'Actions/'+DBGBreakPointActionNames[CurAction],
        CurAction in Actions, CurAction in [bpaStop]);
  end;
  SaveGroupList(FDisableGroupList, APath + 'DisableGroups/');
  SaveGroupList(FEnableGroupList, APath + 'EnableGroups/');
end;

procedure TIDEBreakPoint.SetAddress(const AValue: TDBGPtr);
begin
  inherited SetAddress(AValue);
  if FMaster<>nil then FMaster.Address := Address;
end;

procedure TIDEBreakPoint.SetLocation(const ASource: String; const ALine: Integer);
begin
  inherited SetLocation(ASource, ALine);
  if FMaster<>nil then FMaster.SetLocation(ASource, DebugExeLine);
end;

procedure TIDEBreakPoint.SetWatch(const AData: String; const AScope: TDBGWatchPointScope;
  const AKind: TDBGWatchPointKind);
begin
  inherited SetWatch(AData, AScope, AKind);
  if FMaster<>nil then FMaster.SetWatch(AData, AScope, AKind);
end;

procedure TIDEBreakPoint.ResetMaster;
begin
  if FMaster <> nil then FMaster.Slave := nil;
  FMaster := nil;
  Changed;
end;

procedure TIDEBreakPoint.SetActions(const AValue: TIDEBreakPointActions);
begin
  if FActions <> AValue
  then begin
    FActions := AValue;
    DoActionChange;
  end;
end;

procedure TIDEBreakPoint.SetGroup(const AValue: TIDEBreakPointGroup);
var
  Grp: TIDEBreakPointGroup;
begin
  if FGroup <> AValue
  then begin

    if FGroup <> nil
    then begin
      Grp := FGroup;
      FGroup := nil;  //  avoid second entrance
      Grp.Remove(Self);
    end;
    FGroup := AValue;
    if FGroup <> nil
    then begin
      FGroup.Add(Self);
    end;
    //Changed;
    DoUserChanged;
  end;
end;

(*
procedure TIDEBreakPoint.CopyGroupList(SrcGroupList, DestGroupList: TIDEBreakPointGroupList;
  DestGroups: TIDEBreakPointGroups);
var
  i: Integer;
  CurGroup: TIDEBreakPointGroup;
  NewGroup: TIDEBreakPointGroup;
begin
  DestGroupList.clear;
  for i:=0 to SrcGroupList.Count-1 do begin
    CurGroup:=TIDEBreakPointGroup(SrcGroupList[i]);
    NewGroup:=DestGroups.GetGroupByName(CurGroup.Name);
    DestGroupList.Add(NewGroup);
  end;
end;

procedure TIDEBreakPoint.CopyAllGroupLists(SrcBreakPoint: TIDEBreakPoint;
  DestGroups: TIDEBreakPointGroups);
begin
  CopyGroupList(SrcBreakPoint.FEnableGroupList,FEnableGroupList,DestGroups);
  CopyGroupList(SrcBreakPoint.FDisableGroupList,FDisableGroupList,DestGroups);
end;
*)

{ =========================================================================== }
{ TIDEBreakPoints }
{ =========================================================================== }

function TIDEBreakPoints.Add(const ASource: String;
  const ALine: Integer): TIDEBreakPoint;
begin
  Result := TIDEBreakPoint(inherited Add(ASource, ALine));
  NotifyAdd(Result);
end;

function TIDEBreakPoints.Add(const AAddress: TDBGPtr): TIDEBreakPoint;
begin
  Result := TIDEBreakPoint(inherited Add(AAddress));
  NotifyAdd(Result);
end;

function TIDEBreakPoints.Add(const AData: String; const AScope: TDBGWatchPointScope;
  const AKind: TDBGWatchPointKind): TIDEBreakPoint;
begin
  Result := TIDEBreakPoint(inherited Add(AData, AScope, AKind));
  NotifyAdd(Result);
end;

procedure TIDEBreakPoints.AddNotification(
  const ANotification: TIDEBreakPointsNotification);
begin
  FNotificationList.Add(ANotification);
  ANotification.AddReference;
end;

constructor TIDEBreakPoints.Create(const ABreakPointClass: TIDEBreakPointClass);
begin
  FMaster := nil;
  FNotificationList := TList.Create;
  inherited Create(ABreakPointClass);
end;

destructor TIDEBreakPoints.Destroy;
var
  n: Integer;
begin
  for n := FNotificationList.Count - 1 downto 0 do
    TDebuggerNotification(FNotificationList[n]).ReleaseReference;

  inherited;

  FreeAndNil(FNotificationList);
end;

function TIDEBreakPoints.Find(const ASource: String;
  const ALine: Integer): TIDEBreakPoint;
begin
  Result := TIDEBreakPoint(inherited Find(ASource, ALine, nil));
end;

function TIDEBreakPoints.Find(const ASource: String;
  const ALine: Integer; const AIgnore: TIDEBreakPoint): TIDEBreakPoint;
begin
  Result := TIDEBreakPoint(inherited Find(ASource, ALine, AIgnore));
end;

function TIDEBreakPoints.Find(const AAddress: TDBGPtr): TIDEBreakPoint;
begin
  Result := TIDEBreakPoint(inherited Find(AAddress));
end;

function TIDEBreakPoints.Find(const AAddress: TDBGPtr; const AIgnore: TIDEBreakPoint): TIDEBreakPoint;
begin
  Result := TIDEBreakPoint(inherited Find(AAddress, AIgnore));
end;

function TIDEBreakPoints.Find(const AData: String; const AScope: TDBGWatchPointScope;
  const AKind: TDBGWatchPointKind): TIDEBreakPoint;
begin
  Result := TIDEBreakPoint(inherited Find(AData, AScope, AKind));
end;

function TIDEBreakPoints.Find(const AData: String; const AScope: TDBGWatchPointScope;
  const AKind: TDBGWatchPointKind; const AIgnore: TIDEBreakPoint): TIDEBreakPoint;
begin
  Result := TIDEBreakPoint(inherited Find(AData, AScope, AKind, AIgnore));
end;

procedure TIDEBreakPoints.SetMaster(const AValue: TDBGBreakPoints);
var
  n: Integer;
begin
  if FMaster = AValue then Exit;

  FMaster := AValue;
  if FMaster = nil
  then begin
    for n := 0 to Count - 1 do
      Items[n].ResetMaster;
  end
  else begin
    FMaster.Assign(Self);
  end;
end;

function TIDEBreakPoints.GetItem(const AnIndex: Integer): TIDEBreakPoint;
begin
  Result := TIDEBreakPoint(inherited GetItem(AnIndex));
end;

procedure TIDEBreakPoints.NotifyAdd(const ABreakPoint: TIDEBreakPoint);
var
  n: Integer;
  Notification: TIDEBreakPointsNotification;
  BP: TBaseBreakPoint;
begin
  ABreakpoint.InitialEnabled := True;
  ABreakpoint.Enabled := True;

  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TIDEBreakPointsNotification(FNotificationList[n]);
    if Assigned(Notification.FOnAdd)
    then Notification.FOnAdd(Self, ABreakPoint);
  end;

  if FMaster <> nil
  then begin
    // create without source. it will be set in assign (but during Begin/EndUpdate)
    BP := FMaster.Add('', 0);
    BP.Assign(ABreakPoint); // will set ABreakPoint.FMaster := BP;
  end;
end;

procedure TIDEBreakPoints.NotifyRemove(const ABreakpoint: TIDEBreakPoint);
var
  n: Integer;
  Notification: TIDEBreakPointsNotification;
begin
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TIDEBreakPointsNotification(FNotificationList[n]);
    if Assigned(Notification.FOnRemove)
    then Notification.FOnRemove(Self, ABreakpoint);
  end;
end;

procedure TIDEBreakPoints.RemoveNotification(
  const ANotification: TIDEBreakPointsNotification);
begin
  FNotificationList.Remove(ANotification);
  ANotification.ReleaseReference;
end;

procedure TIDEBreakPoints.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; const OnLoadFilename: TOnLoadFilenameFromConfig;
  const OnGetGroup: TOnGetGroupByName);
var
  NewCount: Integer;
  i: Integer;
  LoadBreakPoint: TIDEBreakPoint;
  BreakPoint: TIDEBreakPoint;
begin
  Clear;
  NewCount:=XMLConfig.GetValue(Path+'Count',0);

  for i:=0 to NewCount-1 do
  begin
    LoadBreakPoint := TIDEBreakPoint.Create(nil);
    LoadBreakPoint.LoadFromXMLConfig(XMLConfig,
      Path+'Item'+IntToStr(i+1)+'/',OnLoadFilename,OnGetGroup);

    case LoadBreakPoint.Kind of
      bpkSource:
        begin
          BreakPoint := Find(LoadBreakPoint.Source, LoadBreakPoint.Line, LoadBreakPoint);
          if BreakPoint = nil then
            BreakPoint := Add(LoadBreakPoint.Source, LoadBreakPoint.Line);
        end;
      bpkAddress:
        begin
          BreakPoint := Find(LoadBreakPoint.Address, LoadBreakPoint);
          if BreakPoint = nil then
            BreakPoint := Add(LoadBreakPoint.Address);
        end;
      bpkData:
        begin
          BreakPoint := Find(LoadBreakPoint.WatchData, LoadBreakPoint.WatchScope, LoadBreakPoint.WatchKind, LoadBreakPoint);
          if BreakPoint = nil then
            BreakPoint := Add(LoadBreakPoint.WatchData, LoadBreakPoint.WatchScope, LoadBreakPoint.WatchKind);
        end;
    end;

    BreakPoint.Assign(LoadBreakPoint);
    ReleaseRefAndNil(LoadBreakPoint)
  end;
end;

procedure TIDEBreakPoints.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; const OnSaveFilename: TOnSaveFilenameToConfig);
var
  Cnt: Integer;
  i: Integer;
  CurBreakPoint: TIDEBreakPoint;
begin
  Cnt:=Count;
  XMLConfig.SetDeleteValue(Path+'Count',Cnt,0);
  for i:=0 to Cnt-1 do begin
    CurBreakPoint:=Items[i];
    CurBreakPoint.SaveToXMLConfig(XMLConfig,
      Path+'Item'+IntToStr(i+1)+'/',OnSaveFilename);
  end;
end;

procedure TIDEBreakPoints.SetItem(const AnIndex: Integer;
  const AValue: TIDEBreakPoint);
begin
  inherited SetItem(AnIndex, AValue);
end;

procedure TIDEBreakPoints.Update(Item: TCollectionItem);
var
  n: Integer;
  Notification: TIDEBreakPointsNotification;
begin
  // Note: Item will be nil in case all items need to be updated
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TIDEBreakPointsNotification(FNotificationList[n]);
    if Assigned(Notification.FOnUpdate)
    then Notification.FOnUpdate(Self, TIDEBreakPoint(Item));
  end;
end;

{ =========================================================================== }
{ TIDEBreakPointGroup }
{ =========================================================================== }

function TIDEBreakPointGroup.Add(const ABreakPoint: TIDEBreakPoint): Integer;
begin
  Result := FBreakpoints.IndexOf(ABreakPoint); //avoid dups
  if Result = -1
  then begin
    Result := FBreakpoints.Add(ABreakPoint);
    ABreakpoint.Group := Self;
  end;
end;

procedure TIDEBreakPointGroup.AddReference(const ABreakPointList: TIDEBreakPointGroupList);
begin
  FReferences.Add(ABreakPointList);
end;

function TIDEBreakPointGroup.Count: Integer;
begin
  Result := FBreakpoints.Count;
end;

constructor TIDEBreakPointGroup.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FBreakpoints := TList.Create;
  FReferences := TList.Create;
  FEnabled := True;
end;

procedure TIDEBreakPointGroup.Delete(const AIndex: Integer);
begin
  Remove(TIDEBreakPoint(FBreakPoints[AIndex]));
end;

destructor TIDEBreakPointGroup.Destroy;
var
  n: Integer;
begin
  for n := FBreakpoints.Count - 1 downto 0 do
    TIDEBreakPoint(FBreakpoints[n]).Group := nil;
  for n := FReferences.Count - 1 downto 0 do
    TIDEBreakPointGroupList(FReferences[n]).Remove(Self);

  inherited Destroy;
  FreeAndNil(FBreakpoints);
  FreeAndNil(FReferences);
end;

function TIDEBreakPointGroup.GetBreakpoint(const AIndex: Integer): TIDEBreakPoint;
begin
  Result := TIDEBreakPoint(FBreakPoints[AIndex]);
end;

function TIDEBreakPointGroup.Remove(const ABreakPoint: TIDEBreakPoint): Integer;
begin
  Result := FBreakpoints.Remove(ABreakPoint);
  if ABreakpoint.Group = Self
  then ABreakpoint.Group := nil;
end;

procedure TIDEBreakPointGroup.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  Name:=XMLConfig.GetValue(Path+'Name/Value','');
  // the breakpoints of this group are not loaded here.
  // They are loaded by the TIDEBreakPoints object.
  FInitialEnabled:=XMLConfig.GetValue(Path+'InitialEnabled/Value',true);
  FEnabled:=FInitialEnabled;
end;

procedure TIDEBreakPointGroup.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'Name/Value',Name,'');
  // the breakpoints of this group are not saved here.
  // They are saved by the TIDEBreakPoints object.
  XMLConfig.SetDeleteValue(Path+'InitialEnabled/Value',FInitialEnabled,true);
end;

class function TIDEBreakPointGroup.CheckName(const AName: String): Boolean;
var
  i: Integer;
begin
  for i := 1 to Length(AName) do
    if not (AName[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
      Exit(False);
  Result := True;
end;

procedure TIDEBreakPointGroup.RemoveReference(const ABreakPointList: TIDEBreakPointGroupList);
begin
  FReferences.Remove(ABreakPointList);
end;

procedure TIDEBreakPointGroup.SetEnabled(const AValue: Boolean);
var
  n: Integer;
begin
  for n := 0 to FBreakPoints.Count - 1 do
    TIDEBreakPoint(FBreakPoints[n]).Enabled := AValue;
end;

procedure TIDEBreakPointGroup.SetInitialEnabled(const AValue: Boolean);
begin
  if FInitialEnabled=AValue then exit;
  FInitialEnabled:=AValue;
end;

procedure TIDEBreakPointGroup.SetName(const AValue: String);
begin
  if FName = AValue then Exit;

  FName := AValue;
  Changed(False);
end;

procedure TIDEBreakPointGroup.AssignTo(Dest: TPersistent);
var
  DestGroup: TIDEBreakPointGroup;
begin
  if Dest is TIDEBreakPointGroup then begin
    DestGroup:=TIDEBreakPointGroup(Dest);
    DestGroup.Name:=Name;
    //DestGroup.InitialEnabled:=InitialEnabled;
    DestGroup.Enabled:=Enabled;
  end else
    inherited AssignTo(Dest);
end;

{ =========================================================================== }
{ TIDEBreakPointGroups }
{ =========================================================================== }

constructor TIDEBreakPointGroups.Create;
begin
  inherited Create(TIDEBreakPointGroup);
end;

procedure TIDEBreakPointGroups.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  NewCount: integer;
  NewGroup: TIDEBreakPointGroup;
  i: Integer;
  OldGroup: TIDEBreakPointGroup;
begin
  Clear;
  NewCount := XMLConfig.GetValue(Path+'Count', 0);
  for i := 0 to NewCount - 1 do
  begin
    NewGroup := TIDEBreakPointGroup(inherited Add);
    NewGroup.LoadFromXMLConfig(XMLConfig,
                               Path+'Item'+IntToStr(i+1)+'/');
    OldGroup := FindGroupByName(NewGroup.Name, NewGroup);
    if OldGroup <> nil then
      NewGroup.Free;
  end;
end;

procedure TIDEBreakPointGroups.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  Cnt: Integer;
  CurGroup: TIDEBreakPointGroup;
  i: Integer;
begin
  Cnt:=Count;
  XMLConfig.SetDeleteValue(Path+'Count',Cnt,0);
  for i := 0 to Cnt - 1 do
  begin
    CurGroup := Items[i];
    CurGroup.SaveToXMLConfig(XMLConfig,
                             Path+'Item'+IntToStr(i+1)+'/');
  end;
end;

function TIDEBreakPointGroups.GetGroupByName(const GroupName: string): TIDEBreakPointGroup;
begin
  Result := FindGroupByName(GroupName, nil);
end;

function TIDEBreakPointGroups.FindGroupByName(const GroupName: string;
  Ignore: TIDEBreakPointGroup): TIDEBreakPointGroup;
var
  i: Integer;
begin
  i := Count - 1;
  while i >= 0 do
  begin
    Result := Items[i];
    if (AnsiCompareText(Result.Name, GroupName) = 0) and (Ignore <> Result) then
      Exit;
    Dec(i);
  end;
  Result := nil;
end;

function TIDEBreakPointGroups.IndexOfGroupWithName(const GroupName: string;
  Ignore : TIDEBreakPointGroup): integer;
begin
  Result:=Count-1;
  while (Result>=0)
  and ((AnsiCompareText(Items[Result].Name,GroupName)<>0)
    or (Items[Result]=Ignore))
  do
    dec(Result);
end;

procedure TIDEBreakPointGroups.InitTargetStart;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    Items[i].Enabled:=Items[i].fInitialEnabled;
end;

function TIDEBreakPointGroups.GetItem(const AnIndex: Integer
  ): TIDEBreakPointGroup;
begin
  Result := TIDEBreakPointGroup(inherited GetItem(AnIndex));
end;

procedure TIDEBreakPointGroups.SetItem(const AnIndex: Integer;
  const AValue: TIDEBreakPointGroup);
begin
  inherited SetItem(AnIndex, AValue);
end;

(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   W A T C H E S                                                          **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

{ =========================================================================== }
{ TWatch }
{ =========================================================================== }

procedure TWatch.AssignTo(Dest: TPersistent);
begin
  if Dest is TWatch
  then begin
    TWatch(Dest).FExpression := FExpression;
    TWatch(Dest).FEnabled := FEnabled;
    TWatch(Dest).FDisplayFormat := FDisplayFormat;
    TWatch(Dest).FValueList.Assign(FValueList);
  end
  else inherited;
end;

function TWatch.CreateValueList: TWatchValueList;
begin
  Result := TWatchValueList.Create(Self);
end;

procedure TWatch.DoModified;
begin
  //
end;

constructor TWatch.Create(ACollection: TCollection);
begin
  assert(((Self is TCurrentWatch) and (ACollection is TCurrentWatches)) or ((not(Self is TCurrentWatch)) and not(ACollection is TCurrentWatches)),
         'Twatch.Create: Watch and collection differ (current and none current)');
  FEnabled := False;
  FValueList := CreateValueList;
  inherited Create(ACollection);
end;

destructor TWatch.Destroy;
begin
  FValueList.Clear;
  inherited Destroy;
  FreeAndNil(FValueList);
end;

procedure TWatch.ClearValues;
begin
  FValueList.Clear;
  TCurrentWatches(Collection).Update(Self);
end;


procedure TWatch.DoEnableChange;
begin
  Changed;
  DoModified;
end;

procedure TWatch.DoExpressionChange;
begin
  Changed;
  DoModified;
end;

procedure TWatch.DoDisplayFormatChanged;
begin
  Changed;
  DoModified;
end;

function TWatch.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TWatch.GetEvaluateFlags: TDBGEvaluateFlags;
begin
  Result := FEvaluateFlags;
end;

function TWatch.GetValue(const AThreadId: Integer; const AStackFrame: Integer): TWatchValue;
begin
  Result := FValueList[AThreadId, AStackFrame];
end;

procedure TWatch.SetEvaluateFlags(AValue: TDBGEvaluateFlags);
begin
  if FEvaluateFlags = AValue then Exit;
  FEvaluateFlags := AValue;
  Changed;
  DoModified;
end;

procedure TWatch.SetRepeatCount(AValue: Integer);
begin
  if FRepeatCount = AValue then Exit;
  FRepeatCount := AValue;
  Changed;
  DoModified;
end;

function TWatch.GetDisplayFormat: TWatchDisplayFormat;
begin
  Result := FDisplayFormat;
end;

procedure TWatch.SetDisplayFormat(AValue: TWatchDisplayFormat);
begin
  if AValue = FDisplayFormat then exit;
  FDisplayFormat := AValue;
  DoDisplayFormatChanged;
end;

procedure TWatch.LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
begin
  FEnabled    := AConfig.GetValue(APath + 'Enabled', True);
  FExpression := AConfig.GetValue(APath + 'Expression', '');
  if AConfig.GetValue(APath + 'ClassAutoCast', False)
  then Include(FEvaluateFlags, defClassAutoCast)
  else Exclude(FEvaluateFlags, defClassAutoCast);
  try    ReadStr(AConfig.GetValue(APath + 'DisplayFormat', 'wdfDefault'), FDisplayFormat);
  except FDisplayFormat := wdfDefault; end;
  FRepeatCount := AConfig.GetValue(APath + 'RepeatCount', 0);

  FValueList.LoadDataFromXMLConfig(AConfig, APath + 'ValueList/');
end;

procedure TWatch.SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string);
var
  s: String;
begin
  AConfig.SetDeleteValue(APath + 'Enabled', FEnabled, True);
  AConfig.SetDeleteValue(APath + 'Expression', FExpression, '');
  WriteStr(s{%H-}, FDisplayFormat);
  AConfig.SetDeleteValue(APath + 'DisplayFormat', s, 'wdfDefault');
  AConfig.SetDeleteValue(APath + 'ClassAutoCast', defClassAutoCast in FEvaluateFlags, False);
  AConfig.SetDeleteValue(APath + 'RepeatCount', FRepeatCount, 0);

  FValueList.SaveDataToXMLConfig(AConfig, APath + 'ValueList/');
end;

function TWatch.GetExpression: String;
begin
  Result := FExpression;
end;

function TWatch.GetRepeatCount: Integer;
begin
  Result := FRepeatCount;
end;

function TWatch.GetValueBase(const AThreadId: Integer;
  const AStackFrame: Integer): TWatchValueBase;
begin
  Result := TWatchValueBase(FValueList[AThreadId, AStackFrame]);
end;

procedure TWatch.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue
  then begin
    FEnabled := AValue;
    DoEnableChange;
  end;
end;

procedure TWatch.SetExpression(AValue: String);
begin
  if AValue <> FExpression
  then begin
    FExpression := AValue;
    FValueList.Clear;
    DoExpressionChange;
  end;
end;

{ =========================================================================== }
{ TCurrentWatch }
{ =========================================================================== }

procedure TCurrentWatch.SetSnapShot(const AValue: TWatch);
begin
  assert((FSnapShot=nil) or (AValue=nil), 'TCurrentWatch already have snapshot');
  if FSnapShot = AValue then exit;
  FSnapShot := AValue;
  if FSnapShot = nil then begin
    TCurrentWatchValueList(FValueList).SnapShot := nil;
  end else begin
    // TODO: FValueList is copied twice ?
    FSnapShot.Assign(self);
    FSnapShot.Enabled := True; // Snapshots are always enabled
    TCurrentWatchValueList(FValueList).SnapShot := FSnapShot.FValueList;
  end;
end;

function TCurrentWatch.CreateValueList: TWatchValueList;
begin
  Result := TCurrentWatchValueList.Create(Self);
end;

procedure TCurrentWatch.DoChanged;
begin
  inherited DoChanged;
  if Collection <> nil
  then TCurrentWatches(Collection).Update(Self);
end;

procedure TCurrentWatch.DoModified;
begin
  inherited DoModified;
  TCurrentWatches(Collection).DoModified;
end;

procedure TCurrentWatch.RequestData(AWatchValue: TCurrentWatchValue);
begin
  if Collection <> nil
  then TCurrentWatches(Collection).RequestData(AWatchValue)
  else AWatchValue.Validity := ddsInvalid;
end;

constructor TCurrentWatch.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

destructor TCurrentWatch.Destroy;
begin
  if (TCurrentWatches(Collection) <> nil)
  then begin
    TCurrentWatches(Collection).NotifyRemove(Self);
    TCurrentWatches(Collection).DoModified;
  end;
  inherited Destroy;
end;

procedure TCurrentWatch.LoadFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
var
  i: Integer;
begin
  Expression := AConfig.GetValue(APath + 'Expression/Value', '');
  Enabled := AConfig.GetValue(APath + 'Enabled/Value', true);
  if AConfig.GetValue(APath + 'ClassAutoCast', False)
  then Include(FEvaluateFlags, defClassAutoCast)
  else Exclude(FEvaluateFlags, defClassAutoCast);
  i := StringCase
    (AConfig.GetValue(APath + 'DisplayStyle/Value', TWatchDisplayFormatNames[wdfDefault]),
    TWatchDisplayFormatNames);
  if i >= 0
  then DisplayFormat := TWatchDisplayFormat(i)
  else DisplayFormat := wdfDefault;
  FRepeatCount := AConfig.GetValue(APath + 'RepeatCount', 0);
end;

procedure TCurrentWatch.SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string);
begin
  AConfig.SetDeleteValue(APath + 'Expression/Value', Expression, '');
  AConfig.SetDeleteValue(APath + 'Enabled/Value', Enabled, true);
  AConfig.SetDeleteValue(APath + 'DisplayStyle/Value',
    TWatchDisplayFormatNames[DisplayFormat], TWatchDisplayFormatNames[wdfDefault]);
  AConfig.SetDeleteValue(APath + 'ClassAutoCast', defClassAutoCast in FEvaluateFlags, False);
  AConfig.SetDeleteValue(APath + 'RepeatCount', FRepeatCount, 0);
end;

{ =========================================================================== }
{ TIdeWatches }
{ =========================================================================== }

function TIdeWatches.Add(const AExpression: String): TWatch;
begin
  BeginUpdate;
  Result := TWatch(inherited Add);
  Result.Expression := AExpression;
  EndUpdate;
end;

function TIdeWatches.GetItem(const AnIndex: Integer): TWatch;
begin
  Result := TWatch(inherited Items[AnIndex]);
end;

procedure TIdeWatches.SetItem(const AnIndex: Integer; const AValue: TWatch);
begin
  inherited Items[AnIndex] := AValue;
end;

function TIdeWatches.WatchClass: TBaseWatchClass;
begin
  Result := TWatch;
end;

procedure TIdeWatches.LoadDataFromXMLConfig(const AConfig: TXMLConfig; APath: string);
var
  c, i: Integer;
begin
  Clear;
  c := AConfig.GetValue(APath + 'Count', 0);
  APath := APath + 'Entry';
  for i := 0 to c - 1 do
    Add('').LoadDataFromXMLConfig(AConfig, APath + IntToStr(i) + '/');
end;

procedure TIdeWatches.SaveDataToXMLConfig(const AConfig: TXMLConfig; APath: string);
var
  i: Integer;
begin
  AConfig.SetDeleteValue(APath + 'Count', Count, 0);
  APath := APath + 'Entry';
  for i := 0 to Count - 1 do
    Items[i].SaveDataToXMLConfig(AConfig, APath + IntToStr(i) + '/');
end;

function TIdeWatches.Find(const AExpression: String): TWatch;
var
  n: Integer;
  S: String;
begin
  S := UpperCase(AExpression);
  for n := 0 to Count - 1 do
  begin
    Result := TWatch(GetItem(n));
    if UpperCase(Result.Expression) = S
    then Exit;
  end;
  Result := nil;
end;

procedure TIdeWatches.ClearValues;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
    TWatch(GetItem(n)).ClearValues;
end;

{ =========================================================================== }
{ TCurrentWatches }
{ =========================================================================== }

function TCurrentWatches.Add(const AExpression: String): TCurrentWatch;
var
  R: TWatch;
begin
  // if this is modified, then also update LoadFromXMLConfig
  Result := TCurrentWatch(inherited Add(AExpression));
  if FSnapShot <> nil then begin
    R := FSnapShot.Add(AExpression);
    Result.SnapShot := R;
  end;
  NotifyAdd(Result);
  DoModified;
end;

constructor TCurrentWatches.Create(AMonitor: TIdeWatchesMonitor);
begin
  FDestroying := False;
  FMonitor := AMonitor;
  inherited Create;
end;

destructor TCurrentWatches.Destroy;
begin
  FDestroying := True;
  inherited Destroy;
end;

function TCurrentWatches.Find(const AExpression: String): TCurrentWatch;
begin
  Result := TCurrentWatch(inherited Find(AExpression));
end;

procedure TCurrentWatches.WatchesChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCurrentWatches.SetSnapShot(const AValue: TIdeWatches);
var
  R: TWatch;
  i: Integer;
begin
  assert((FSnapShot=nil) or (AValue=nil), 'TCurrentWatches already have snapshot');
  if FSnapShot = AValue then exit;
  FSnapShot := AValue;

  if FSnapShot = nil then begin
    for i := 0 to Count - 1 do
      Items[i].SnapShot := nil;
  end
  else begin
    // FSnapShot.Assign(Self);
    FSnapShot.Clear;
    for i := 0 to Count - 1 do begin
      R := FSnapShot.Add('');
      R.Assign(Items[i]);
      Items[i].SnapShot := R;
    end;
  end;
end;

function TCurrentWatches.GetItem(const AnIndex: Integer): TCurrentWatch;
begin
  Result := TCurrentWatch(inherited GetItem(AnIndex));
end;

procedure TCurrentWatches.LoadFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
var
  NewCount: Integer;
  i: Integer;
  Watch: TCurrentWatch;
begin
  if FMonitor <> nil then
    FMonitor.BeginIgnoreModified;
  try
    Clear;
    NewCount := AConfig.GetValue(APath + 'Count', 0);
    for i := 0 to NewCount-1 do
    begin
      // Call inherited Add, so NotifyAdd can be send, after the Watch was loaded
      Watch := TCurrentWatch(inherited Add(''));
      Watch.LoadFromXMLConfig(AConfig, Format('%sItem%d/', [APath, i + 1]));
      NotifyAdd(Watch);
    end;
  finally
    if FMonitor <> nil then
      FMonitor.EndIgnoreModified;
  end;
end;

procedure TCurrentWatches.NotifyAdd(const AWatch: TCurrentWatch);
begin
  FMonitor.NotifyAdd(Self, AWatch);
end;

procedure TCurrentWatches.NotifyRemove(const AWatch: TCurrentWatch);
begin
  FMonitor.NotifyRemove(Self, AWatch);
end;

procedure TCurrentWatches.DoModified;
begin
  if (FMonitor <> nil) and (not FDestroying) then
    FMonitor.DoModified;
end;

procedure TCurrentWatches.SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string);
var
  Cnt: Integer;
  i: Integer;
  Watch: TCurrentWatch;
begin
  Cnt := Count;
  AConfig.SetDeleteValue(APath + 'Count', Cnt, 0);
  for i := 0 to Cnt - 1 do
  begin
    Watch := Items[i];
    Watch.SaveToXMLConfig(AConfig, Format('%sItem%d/', [APath, i + 1]));
  end;
end;

procedure TCurrentWatches.SetItem(const AnIndex: Integer; const AValue: TCurrentWatch);
begin
  inherited SetItem(AnIndex, AValue);
end;

function TCurrentWatches.WatchClass: TBaseWatchClass;
begin
  Result := TCurrentWatch;
end;

procedure TCurrentWatches.Update(Item: TCollectionItem);
var
  m, c: Integer;
begin
  if Item <> nil then begin
    FMonitor.NotifyUpdate(Self, TCurrentWatch(Item));
  end else begin
    m := 0;
    c := Count;
    while m < c do begin
      FMonitor.NotifyUpdate(Self, Items[m]);
      if c <> Count then begin
        m := Max(0, m - Max(0, Count - c));
        c := Count;
      end;
      inc(m);
    end;
  end;
end;

procedure TCurrentWatches.RequestData(AWatchValue: TCurrentWatchValue);
begin
  FMonitor.RequestData(AWatchValue);
end;


(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   L O C A L S                                                            **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

{ =========================================================================== }
{ TLocals }
{ =========================================================================== }

procedure TIDELocals.SetDataValidity(AValidity: TDebuggerDataState);
begin
  assert(Self is TCurrentLocals, 'TLocals.SetDataValidity');
end;

procedure TIDELocals.LoadDataFromXMLConfig(const AConfig: TXMLConfig; APath: string);
var
  c, i: Integer;
begin
  c := AConfig.GetValue(APath + 'Count', 0);
  APath := APath + 'Entry';
  for i := 0 to c - 1 do begin
    Add(AConfig.GetValue(APath + IntToStr(i) + '/Expression', ''),
        AConfig.GetValue(APath + IntToStr(i) + '/Value', ''));
  end;
end;

procedure TIDELocals.SaveDataToXMLConfig(const AConfig: TXMLConfig; APath: string);
var
  i: Integer;
begin
  AConfig.SetValue(APath + 'ThreadId', ThreadId);
  AConfig.SetValue(APath + 'StackFrame', StackFrame);
  AConfig.SetDeleteValue(APath + 'Count', Count, 0);
  APath := APath + 'Entry';
  for i := 0 to Count - 1 do begin
    AConfig.SetValue(APath + IntToStr(i) + '/Expression', Names[i]);
    AConfig.SetValue(APath + IntToStr(i) + '/Value', Values[i]);
  end;
end;

constructor TIDELocals.CreateFromXMLConfig(const AConfig: TXMLConfig; APath: string);
var
  LoadThreadId, LoadStackFrame: Integer;
begin
  LoadThreadId := AConfig.GetValue(APath + 'ThreadId', -1);
  LoadStackFrame := AConfig.GetValue(APath + 'StackFrame', -1);
  Create(LoadThreadId, LoadStackFrame);
  LoadDataFromXMLConfig(AConfig, APath);
end;

{ =========================================================================== }
{ TCurrentLocals }
{ =========================================================================== }

procedure TCurrentLocals.SetSnapShot(const AValue: TIDELocals);
begin
  assert((FSnapShot=nil) or (AValue=nil), 'TCurrentLocals already have snapshot');
  if FSnapShot = AValue then exit;
  FSnapShot := AValue;
  if FSnapShot <> nil
  then FSnapShot.Assign(Self);
end;

constructor TCurrentLocals.Create(AMonitor: TIdeLocalsMonitor; AThreadId, AStackFrame: Integer);
begin
  FMonitor := AMonitor;
  FDataValidity := ddsUnknown;
  inherited Create(AThreadId, AStackFrame);
end;

function TCurrentLocals.Count: Integer;
begin
  case FDataValidity of
    ddsUnknown:   begin
        AddReference;
        try
          Result := 0;
          FDataValidity := ddsRequested;
          FMonitor.RequestData(Self);  // Locals can be cleared, if debugger is "run" again
          if FDataValidity = ddsValid then Result := inherited Count();
        finally
          ReleaseReference;
        end;
      end;
    ddsRequested, ddsEvaluating: Result := 0;
    ddsValid:                    Result := inherited Count;
    ddsInvalid, ddsError:        Result := 0;
  end;
end;

procedure TCurrentLocals.SetDataValidity(AValidity: TDebuggerDataState);
begin
  if FDataValidity = AValidity then exit;

  if (FDataValidity in [ddsUnknown, ddsEvaluating, ddsRequested]) and (FSnapShot <> nil)
  then FSnapShot.Assign(Self);

  FDataValidity := AValidity;
  FMonitor.NotifyChange(Self);
end;

(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   R E G I S T E R S                                                      **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

{ TIDERegisterValue }

procedure TIDERegisterValue.DoDataValidityChanged(AnOldValidity: TDebuggerDataState);
begin
  if (Owner <> nil) and (Owner is TCurrentIDERegisters) then
    TCurrentIDERegisters(Owner).DoDataValidityChanged(AnOldValidity);
end;

procedure TIDERegisterValue.DoDisplayFormatChanged(AnOldFormat: TRegisterDisplayFormat);
begin
  if not HasValueFormat[DisplayFormat] then begin
    DataValidity := ddsRequested;
    if (Owner <> nil) and (Owner is TCurrentIDERegisters) then
      TCurrentIDERegisters(Owner).FMonitor.RequestData(TCurrentIDERegisters(Owner));
  end
  else
  if (Owner <> nil) and (Owner is TCurrentIDERegisters) then
    TCurrentIDERegisters(Owner).FMonitor.NotifyChange(TCurrentIDERegisters(Owner));
end;

{ TIDERegisters }

function TIDERegisters.CreateEntry: TDbgEntityValue;
begin
  Result := TIDERegisterValue.Create;
end;

{ TCurrentIDERegisters }

procedure TCurrentIDERegisters.DoDataValidityChanged(AnOldValidity: TDebuggerDataState);
begin
  inherited DoDataValidityChanged(AnOldValidity);
  if not( (DataValidity in [ddsRequested, ddsEvaluating]) and
          (AnOldValidity in [ddsUnknown, ddsRequested, ddsEvaluating]) )
  then
    FMonitor.NotifyChange(Self);
end;

constructor TCurrentIDERegisters.Create(AMonitor: TIdeRegistersMonitor; AThreadId,
  AStackFrame: Integer);
begin
  FMonitor := AMonitor;
  inherited Create(AThreadId, AStackFrame);
end;

function TCurrentIDERegisters.Count: Integer;
begin
  case DataValidity of
    ddsUnknown:   begin
        AddReference;
        try
          Result := 0;
          DataValidity := ddsRequested;
          FMonitor.RequestData(Self);  // Locals can be cleared, if debugger is "run" again
          if DataValidity = ddsValid then Result := inherited Count();
        finally
          ReleaseReference;
        end;
      end;
    ddsRequested, ddsEvaluating: Result := 0;
    ddsValid:                    Result := inherited Count;
    ddsInvalid, ddsError:        Result := 0;
  end;
end;

{ TCurrentIDERegistersList }

procedure TCurrentIDERegistersList.DoCleared;
begin
  inherited DoCleared;
  FMonitor.NotifyChange(nil);
end;

function TCurrentIDERegistersList.CreateEntry(AThreadId, AStackFrame: Integer): TRegisters;
begin
  Result := TCurrentIDERegisters.Create(FMonitor, AThreadId, AStackFrame);
end;

constructor TCurrentIDERegistersList.Create(AMonitor: TIdeRegistersMonitor);
begin
  FMonitor := AMonitor;
  inherited Create;
end;

{ TIdeRegistersMonitor }

function TIdeRegistersMonitor.GetSupplier: TRegisterSupplier;
begin
  Result := TRegisterSupplier(inherited Supplier);
end;

function TIdeRegistersMonitor.GetCurrentRegistersList: TCurrentIDERegistersList;
begin
  Result := TCurrentIDERegistersList(RegistersList);
end;

procedure TIdeRegistersMonitor.SetSupplier(const AValue: TRegisterSupplier);
begin
  inherited Supplier := AValue;
end;

procedure TIdeRegistersMonitor.DoStateEnterPause;
begin
  inherited DoStateEnterPause;
  if CurrentRegistersList = nil then exit;
  Clear;
end;

procedure TIdeRegistersMonitor.DoStateLeavePauseClean;
begin
  inherited DoStateLeavePauseClean;
  if CurrentRegistersList = nil then exit;
  Clear;
end;

procedure TIdeRegistersMonitor.DoEndUpdate;
begin
  inherited DoEndUpdate;
  if rmNeedNotifyChange in FFlags then
    NotifyChange(nil);
end;

procedure TIdeRegistersMonitor.NotifyChange(ARegisters: TCurrentIDERegisters);
begin
  if IsUpdating then begin
    Include(FFlags, rmNeedNotifyChange);
    exit;
  end;
  Exclude(FFlags, rmNeedNotifyChange);
  FNotificationList.NotifyChange(ARegisters);
end;

procedure TIdeRegistersMonitor.DoNewSupplier;
begin
  inherited DoNewSupplier;
  NotifyChange(nil);
  if Supplier <> nil then
    Supplier.CurrentRegistersList := CurrentRegistersList;
end;

procedure TIdeRegistersMonitor.RequestData(ARegisters: TCurrentIDERegisters);
begin
  if Supplier <> nil
  then Supplier.RequestData(ARegisters)
  else ARegisters.DataValidity := ddsInvalid;
end;

function TIdeRegistersMonitor.CreateRegistersList: TRegistersList;
begin
  Result := TCurrentIDERegistersList.Create(Self);
end;

constructor TIdeRegistersMonitor.Create;
begin
  inherited Create;
  FNotificationList := TDebuggerChangeNotificationList.Create;
end;

destructor TIdeRegistersMonitor.Destroy;
begin
  FNotificationList.Clear;
  inherited Destroy;
  FreeAndNil(FNotificationList);
end;

procedure TIdeRegistersMonitor.Clear;
begin
  CurrentRegistersList.Clear;
end;

procedure TIdeRegistersMonitor.AddNotification(const ANotification: TRegistersNotification);
begin
  FNotificationList.Add(ANotification);
end;

procedure TIdeRegistersMonitor.RemoveNotification(const ANotification: TRegistersNotification);
begin
  FNotificationList.Remove(ANotification);
end;


(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   C A L L S T A C K                                                      **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

{ =========================================================================== }
{ TDBGCallStackEntry }
{ =========================================================================== }

constructor TCallStackEntry.Create(const AIndex: Integer;
  const AnAdress: TDbgPtr; const AnArguments: TStrings;
  const AFunctionName: String; const AUnitInfo: TDebuggerUnitInfo;
  const ALine: Integer; AState: TDebuggerDataState = ddsValid);
begin
  inherited Create;
  FIndex := AIndex;
  FAdress := AnAdress;
  FArguments := TStringlist.Create;
  if AnArguments <> nil
  then FArguments.Assign(AnArguments);
  FFunctionName := AFunctionName;
  SetUnitInfo(AUnitInfo);
  FLine := ALine;
  FState := AState;
end;

constructor TCallStackEntry.CreateCopy(const ASource: TCallStackEntry);
begin
  Create(ASource.FIndex, ASource.FAdress, ASource.FArguments,
         ASource.FFunctionName, ASource.FUnitInfo,
         ASource.FLine, ASource.FState);
end;

destructor TCallStackEntry.Destroy;
begin
  inherited;
  if FUnitInfo <> nil then FUnitInfo.ReleaseReference;
  FreeAndNil(FArguments);
end;

procedure TCallStackEntry.Init(const AnAdress: TDbgPtr; const AnArguments: TStrings;
  const AFunctionName: String; const AUnitName, AClassName, AProcName, AFunctionArgs: String;
  const ALine: Integer; AState: TDebuggerDataState);
var
  loc: TDebuggerUnitInfo;
begin
  assert(FOwner is TCurrentCallStack, 'FOwner is TCurrentCallStack');
  if GetUnitInfoProvider = nil then
    loc := nil
  else
    loc := GetUnitInfoProvider.GetUnitInfoByFunction(AUnitName, AClassName, AProcName, AFunctionArgs);
  FAdress := AnAdress;
  if AnArguments <> nil
  then FArguments.Assign(AnArguments);
  FFunctionName := AFunctionName;

  SetUnitInfo(loc);
  FLine := ALine;
  FState := AState;
end;

procedure TCallStackEntry.Init(const AnAdress: TDbgPtr; const AnArguments: TStrings;
  const AFunctionName: String; const FileName, FullName: String; const ALine: Integer;
  AState: TDebuggerDataState);
var
  loc: TDebuggerUnitInfo;
begin
  assert(FOwner is TCurrentCallStack, 'FOwner is TCurrentCallStack');
  if GetUnitInfoProvider = nil then
    loc := nil
  else
    loc := GetUnitInfoProvider.GetUnitInfoFor(FileName, FullName);
  FAdress := AnAdress;
  if AnArguments <> nil
  then FArguments.Assign(AnArguments);
  FFunctionName := AFunctionName;

  SetUnitInfo(loc);
  FLine := ALine;
  FState := AState;
end;

function TCallStackEntry.GetFunctionWithArg: String;
var
  S: String;
  m: Integer;
begin
  S := '';
  for m := 0 to ArgumentCount - 1 do
  begin
    if S <> '' then
      S := S + ', ';
    S := S + ArgumentValues[m];
  end;
  if S <> '' then
    S := '(' + S + ')';
  Result := FunctionName + S;
end;

function TCallStackEntry.IsCurrent: Boolean;
begin
  Result := (FOwner <> nil) and (FOwner.CurrentIndex = Self.Index);
  //TODO: check current thread
end;

procedure TCallStackEntry.MakeCurrent;
begin
  if FOwner = nil then Exit;
  if IsCurrent then exit;
  FOwner.ChangeCurrentIndex(Self.Index);
end;

function TCallStackEntry.GetArgumentCount: Integer;
begin
  Result := FArguments.Count;
end;

function TCallStackEntry.GetArgumentName(const AnIndex: Integer): String;
begin
  Result := FArguments.Names[AnIndex];
end;

function TCallStackEntry.GetArgumentValue(const AnIndex: Integer): String;
begin
  Result := FArguments[AnIndex];
  Result := GetPart('=', '', Result);
end;

function TCallStackEntry.GetFunctionName: String;
begin
  case FState of
    ddsValid:     Result := FFunctionName;
    ddsError:     Result := '<Error: '+FFunctionName+'>';
    ddsInvalid:   Result := '<invalid>';
    ddsRequested, ddsEvaluating: Result := '<evaluating>';
    ddsUnknown:                  Result := '<unknown>';
  end;
end;

function TCallStackEntry.GetIndex: Integer;
begin
  Result := FIndex;
end;

function TCallStackEntry.GetLine: Integer;
begin
  Result := FLine;
end;

function TCallStackEntry.GetSource: String;
begin
  if (FState = ddsValid)  and (FUnitInfo <> nil)
  then Result := FUnitInfo.FileName
  else Result := '';
end;

function TCallStackEntry.GetState: TDebuggerDataState;
begin
  Result := FState;
end;

procedure TCallStackEntry.SetState(AValue: TDebuggerDataState);
begin
  FState := AValue;
end;

procedure TCallStackEntry.SetUnitInfo(AUnitInfo: TDebuggerUnitInfo);
begin
  if FUnitInfo <> nil then FUnitInfo.ReleaseReference;
  FUnitInfo := AUnitInfo;
  if FUnitInfo <> nil then FUnitInfo.AddReference;
end;

function TCallStackEntry.GetThreadId: Integer;
begin
  Assert(false, 'thread only');
  Result := 0;
end;

function TCallStackEntry.GetThreadName: String;
begin
  Assert(false, 'thread only');
  Result := '';
end;

function TCallStackEntry.GetThreadState: String;
begin
  Assert(false, 'thread only');
  Result := '';
end;

procedure TCallStackEntry.SetThreadState(AValue: String);
begin
  Assert(false, 'thread only');
end;

function TCallStackEntry.GetUnitInfoProvider: TDebuggerUnitInfoProvider;
begin
  Result := (FOwner as TCurrentCallStack).FMonitor.UnitInfoProvider;
end;

function TCallStackEntry.GetAddress: TDbgPtr;
begin
  Result := FAdress;
end;

procedure TCallStackEntry.LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
var
  UInfo: TDebuggerUnitInfo;
  i: Integer;
begin
  FIndex          := AConfig.GetValue(APath + 'Index', 0);
  FAdress         := StrToQWordDef(AConfig.GetValue(APath + 'Address', '0'), 0);
  FFunctionName   := AConfig.GetValue(APath + 'FunctionName', '');
  FLine           := AConfig.GetValue(APath + 'Line', 0);
  FArguments.Text := AConfig.GetValue(APath + 'Arguments', '');

  i := AConfig.GetValue(APath + 'UnitInfoRef', -1);
  UInfo := nil;
  if (i >= 0) and (AUnitInvoPrv <> nil) then begin
    if i < AUnitInvoPrv.Count then
      UInfo := AUnitInvoPrv[i];
  end
  else begin
    UInfo := TDebuggerUnitInfo.Create('','');
    UInfo.LoadDataFromXMLConfig(AConfig, APath + 'UnitInfo/');
  end;
  SetUnitInfo(UInfo);
  try
    ReadStr(AConfig.GetValue(APath + 'State', 'ddsUnknown'), FState);
  except
    FState := ddsUnknown;
  end;
end;

procedure TCallStackEntry.SaveDataToXMLConfig(const AConfig: TXMLConfig; APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
var
  s: string;
  i: Integer;
begin
  AConfig.SetValue(APath + 'Index', FIndex);
  AConfig.SetValue(APath + 'Address', IntToStr(FAdress));
  AConfig.SetValue(APath + 'FunctionName', FFunctionName);
  AConfig.SetValue(APath + 'Line', FLine);
  AConfig.SetValue(APath + 'Arguments', FArguments.Text);
  if FUnitInfo <> nil then begin
    if AUnitInvoPrv <> nil
    then begin
      i := AUnitInvoPrv.IndexOf(FUnitInfo, True);
      AConfig.SetValue(APath + 'UnitInfoRef', i);
    end
    else
      FUnitInfo.SaveDataToXMLConfig(AConfig, APath + 'UnitInfo/');
  end;
  WriteStr(s{%H-}, FState);
  AConfig.SetValue(APath + 'State', s);
end;

procedure TCallStackEntry.ClearLocation;
begin
  FIndex := 0;
  FAdress := 0;
  FFunctionName := '';
  FLine := 0;
  if FArguments <> nil then
    FArguments.Clear;
  SetUnitInfo(TDebuggerUnitInfo.Create('',''));
end;

constructor TCallStackEntry.Create;
begin
  FArguments := TStringlist.Create;
end;

{ =========================================================================== }
{ TCallStack }
{ =========================================================================== }

procedure TCallStack.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TObject(FList[i]).Free;
  FList.Clear;
end;

function TCallStack.GetCount: Integer;
begin
  Result := FList.Count;
end;

destructor TCallStack.Destroy;
begin
  Clear;
  inherited Destroy;
  FreeAndNil(FList);
end;

function TCallStack.GetCurrent: Integer;
begin
  Result := FCurrent;
end;

function TCallStack.GetEntry(AIndex: Integer): TCallStackEntry;
begin
  if (AIndex < 0)
  or (AIndex >= CountLimited(AIndex+1)) then IndexError(Aindex);

  Result := TCallStackEntry(FList[AIndex]);
end;

procedure TCallStack.AddEntry(AnEntry: TCallStackEntry);
begin
  // must be added in correct order
  Flist.Add(AnEntry);
  AnEntry.FOwner := Self;
end;

procedure TCallStack.AssignEntriesTo(AnOther: TCallStack);
var
  i: Integer;
begin
  for i := 0 to FList.Count-1 do begin
    AnOther.AddEntry(TCallStackEntry.CreateCopy(TCallStackEntry(FList[i])));
  end;
end;

procedure TCallStack.LoadDataFromXMLConfig(const AConfig: TXMLConfig; APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
var
  c, i: Integer;
  e: TCallStackEntry;
begin
  Clear;
  FThreadId := AConfig.GetValue(APath + 'ThreadId', -1);
  FCurrent  := AConfig.GetValue(APath + 'Current', -1);

  c := AConfig.GetValue(APath + 'Count', 0);
  APath := APath + 'Entry';
  for i := 0 to c - 1 do begin
    e := TCallStackEntry.Create();
    e.FOwner := self;
    e.LoadDataFromXMLConfig(AConfig, APath + IntToStr(i) + '/', AUnitInvoPrv);
    FList.Add(e);
  end;
end;

procedure TCallStack.SaveDataToXMLConfig(const AConfig: TXMLConfig; APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
var
  i: Integer;
begin
  AConfig.SetValue(APath + 'ThreadId', FThreadId);
  AConfig.SetValue(APath + 'Current', FCurrent);

  AConfig.SetDeleteValue(APath + 'Count', FList.Count, 0);
  APath := APath + 'Entry';
  for i := 0 to FList.Count - 1 do
    TCallStackEntry(FList[i]).SaveDataToXMLConfig(AConfig, APath + IntToStr(i) + '/', AUnitInvoPrv);
end;

procedure TCallStack.DoEntriesCreated;
begin
  assert(False, 'TCallStack.DoEntriesCreated');
end;

procedure TCallStack.DoEntriesUpdated;
begin
  assert(False, 'TCallStack.DoEntriesUpdated');
end;

procedure TCallStack.SetCountValidity(AValidity: TDebuggerDataState);
begin
  assert(False, 'TCallStack.SetCountValidity');
end;

procedure TCallStack.SetHasAtLeastCountInfo(AValidity: TDebuggerDataState; AMinCount: Integer);
begin
  assert(False, 'TCallStack.SetHasAtLeastCountInfo');
end;

procedure TCallStack.SetCurrentValidity(AValidity: TDebuggerDataState);
begin
  assert(False, 'TCallStack.SetCurrentValidity');
end;

function TCallStack.IndexError(AIndex: Integer): TCallStackEntry;
begin
  Result:=nil;
  raise EInvalidOperation.CreateFmt('Index out of range (%d)', [AIndex]);
end;

function TCallStack.GetEntryBase(AIndex: Integer): TCallStackEntryBase;
begin
  Result := TCallStackEntryBase(GetEntry(AIndex));
end;

procedure TCallStack.PrepareRange(AIndex, ACount: Integer);
begin
end;

procedure TCallStack.ChangeCurrentIndex(ANewIndex: Integer);
begin
  CurrentIndex := ANewIndex;
end;

function TCallStack.HasAtLeastCount(ARequiredMinCount: Integer): TNullableBool;
begin
  if ARequiredMinCount <= Count then
    Result := nbTrue
  else
    Result := nbFalse;
end;

function TCallStack.CountLimited(ALimit: Integer): Integer;
begin
  case HasAtLeastCount(ALimit) of
    nbUnknown: Result := 0;
    nbTrue:    Result := ALimit;
    nbFalse:   Result := Count;
  end;
end;

procedure TCallStack.SetCount(ACount: Integer);
begin
  // can not set count
  assert(False, 'TCallStack.SetCount should not be called')
end;

procedure TCallStack.Assign(AnOther: TCallStack);
begin
  Clear;
  ThreadId := AnOther.ThreadId;
  FCurrent := AnOther.FCurrent;
  AnOther.AssignEntriesTo(Self);
end;

constructor TCallStack.Create;
begin
  FThreadId := -1;
  FCurrent := -1;
  FList := TList.Create;
  inherited;
end;

constructor TCallStack.CreateCopy(const ASource: TCallStack);
begin
  Create;
  Assign(ASource);
end;

procedure TCallStack.SetCurrent(AValue: Integer);
begin
  FCurrent := AValue;
end;

function TCallStack.GetThreadId: Integer;
begin
  Result := FThreadId;
end;

procedure TCallStack.SetThreadId(AValue: Integer);
begin
  FThreadId := AValue;
end;

function TCallStack.GetRawEntries: TMap;
begin
  assert(False, 'TCallStack.GetRawEntries');
  Result := nil;
end;

function TCallStack.GetNewCurrentIndex: Integer;
begin
  assert(False, 'TCallStack.GetNewCurrentIndex');
  Result := 0;
end;


{ =========================================================================== }
{ TIdeCallStackMonitor }
{ =========================================================================== }

procedure TIdeCallStackMonitor.AddNotification(const ANotification: TCallStackNotification);
begin
  FNotificationList.Add(ANotification);
end;

constructor TIdeCallStackMonitor.Create;
begin
  FSnapshots := TDebuggerDataSnapShotList.Create;
  FNotificationList := TDebuggerChangeNotificationList.Create;
  inherited Create;
end;

destructor TIdeCallStackMonitor.Destroy;
begin
  FSnapshots.Clear;
  FNotificationList.Clear;
  inherited;
  FreeAndNil(FNotificationList);
  FreeAndNil(FSnapshots);
end;

procedure TIdeCallStackMonitor.SetSupplier(const AValue: TCallStackSupplier);
begin
  inherited Supplier := AValue;
end;

procedure TIdeCallStackMonitor.DoStateEnterPause;
begin
  inherited DoStateEnterPause;
  if (CurrentCallStackList = nil) then Exit;
  CurrentCallStackList.Clear;
  DoModified;
end;

procedure TIdeCallStackMonitor.DoStateLeavePause;
begin
  inherited DoStateLeavePause;
  if (CurrentCallStackList = nil) then Exit;
  CurrentCallStackList.SnapShot := nil;
end;

procedure TIdeCallStackMonitor.DoStateLeavePauseClean;
begin
  inherited DoStateLeavePauseClean;
  if (CurrentCallStackList = nil) then Exit;
  CurrentCallStackList.SnapShot := nil;
  CurrentCallStackList.Clear;
  CallStackClear(Self);
end;

procedure TIdeCallStackMonitor.DoModified;
begin
  NotifyChange;
end;

procedure TIdeCallStackMonitor.RequestCount(ACallstack: TCallStack);
begin
  if (Supplier <> nil) and (ACallstack is TCurrentCallStack)
  then Supplier.RequestCount(TCurrentCallStack(ACallstack));
end;

procedure TIdeCallStackMonitor.RequestAtLeastCount(ACallstack: TCallStack;
  ARequiredMinCount: Integer);
begin
  if (Supplier <> nil) and (ACallstack is TCurrentCallStack)
  then Supplier.RequestAtLeastCount(TCurrentCallStack(ACallstack), ARequiredMinCount);
end;

procedure TIdeCallStackMonitor.RequestCurrent(ACallstack: TCallStack);
begin
  if (Supplier <> nil) and (ACallstack is TCurrentCallStack)
  then Supplier.RequestCurrent(TCurrentCallStack(ACallstack));
end;

procedure TIdeCallStackMonitor.RequestEntries(ACallstack: TCallStack);
begin
  if (Supplier <> nil) and (ACallstack is TCurrentCallStack)
  then Supplier.RequestEntries(TCurrentCallStack(ACallstack));
end;

procedure TIdeCallStackMonitor.UpdateCurrentIndex;
begin
  DebugLn(DBG_DATA_MONITORS, ['DebugDataMonitor: TIdeCallStackMonitor.UpdateCurrentIndex']);
  if Supplier <> nil then Supplier.UpdateCurrentIndex;
  NotifyCurrent;
end;

procedure TIdeCallStackMonitor.DoNewSupplier;
begin
  inherited DoNewSupplier;
  NotifyChange;
  if Supplier <> nil then
    Supplier.CurrentCallStackList := CurrentCallStackList;
end;

procedure TIdeCallStackMonitor.CallStackClear(Sender: TObject);
begin
  DebugLn(DBG_DATA_MONITORS, ['DebugDataMonitor: TIdeCallStackMonitor.CallStackClear']);
  // Don't clear, set it to 0 so there are no entries shown
  //SetCount(0);
  NotifyChange;
end;

function TIdeCallStackMonitor.GetCurrentCallStackList: TCurrentCallStackList;
begin
  Result := TCurrentCallStackList(CallStackList);
end;

function TIdeCallStackMonitor.GetSnapshot(AnID: Pointer): TIdeCallStackList;
begin
  Result := TIdeCallStackList(FSnapshots.SnapShot[AnID]);
end;

function TIdeCallStackMonitor.GetSupplier: TCallStackSupplier;
begin
  Result := TCallStackSupplier(inherited Supplier);
end;

procedure TIdeCallStackMonitor.NotifyChange;
begin
  FNotificationList.NotifyChange(Self);
end;

procedure TIdeCallStackMonitor.NotifyCurrent;
begin
  FNotificationList.NotifyCurrent(Self);
end;

function TIdeCallStackMonitor.CreateSnapshot(CreateEmpty: Boolean = False): TObject;
begin
  Result := TIdeCallStackList.Create;
  if not CreateEmpty
  then CurrentCallStackList.SnapShot := TIdeCallStackList(Result);
end;

function TIdeCallStackMonitor.CreateCallStackList: TCallStackList;
begin
  Result := TCurrentCallStackList.Create(Self);
end;

procedure TIdeCallStackMonitor.RemoveNotification(const ANotification: TCallStackNotification);
begin
  FNotificationList.Remove(ANotification);
end;

procedure TIdeCallStackMonitor.NewSnapshot(AnID: Pointer; CreateEmpty: Boolean);
var
  S: TObject;
begin
  S := CreateSnapshot(CreateEmpty);
  FSnapshots.AddSnapShot(AnID, S);
end;

procedure TIdeCallStackMonitor.RemoveSnapshot(AnID: Pointer);
begin
  FSnapshots.RemoveSnapShot(AnID);
end;


(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   S I G N A L S  and  E X C E P T I O N S                                **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

{ =========================================================================== }
{ TIDESignal }
{ =========================================================================== }

procedure TIDESignal.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if (TIDESignals(Collection).FMaster <> nil)
  and (Dest is TDBGSignal)
  then begin
    FMaster := TDBGSignal(Dest);
  end;
end;

procedure TIDESignal.LoadFromXMLConfig (const AXMLConfig: TXMLConfig; const APath: string );
begin
  // TODO
end;

procedure TIDESignal.SaveToXMLConfig (const AXMLConfig: TXMLConfig; const APath: string );
begin
  // TODO
end;

procedure TIDESignal.ResetMaster;
begin
  FMaster := nil;
end;

{ =========================================================================== }
{ TIDESignals }
{ =========================================================================== }

function TIDESignals.Add(const AName: String; AID: Integer): TIDESignal;
begin
  Result := TIDESignal(inherited Add(AName, AID));
end;

function TIDESignals.Find(const AName: String): TIDESignal;
begin
  Result := TIDESignal(inherited Find(AName));
end;

procedure TIDESignals.SetMaster(const AValue: TDBGSignals);
var
  n: Integer;
begin
  if FMaster = AValue then Exit;
  FMaster := AValue;
  if FMaster = nil
  then begin
    for n := 0 to Count - 1 do
      Items[n].ResetMaster;
  end
  else begin
    FMaster.Assign(Self);
  end;
end;

function TIDESignals.GetItem(const AIndex: Integer): TIDESignal;
begin
  Result := TIDESignal(inherited GetItem(AIndex));
end;

procedure TIDESignals.LoadFromXMLConfig(const AXMLConfig: TXMLConfig; const APath: string);
begin
  // TODO
end;

procedure TIDESignals.SaveToXMLConfig(const AXMLConfig: TXMLConfig; const APath: string);
begin
  // TODO
end;

procedure TIDESignals.SetItem(const AIndex: Integer; const AValue: TIDESignal);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TIDESignals.AddDefault;
begin
  // todo: add default signals
end;

constructor TIDESignals.Create;
begin
  FMaster := nil;
  inherited Create(TIDESignal);
  AddDefault;
end;

procedure TIDESignals.Reset;
begin
  inherited Reset;
  AddDefault;
end;

{ =========================================================================== }
{ TIDEException }
{ =========================================================================== }

constructor TIDEException.Create (ACollection: TCollection );
begin
  FEnabled := True;
  inherited Create(ACollection);
end;

procedure TIDEException.LoadFromXMLConfig(const AXMLConfig: TXMLConfig;
  const APath: string);
begin
  FName:=AXMLConfig.GetValue(APath+'Name/Value','');
  FEnabled:=AXMLConfig.GetValue(APath+'Enabled/Value',true);
end;

procedure TIDEException.SaveToXMLConfig(const AXMLConfig: TXMLConfig;
  const APath: string);
begin
  AXMLConfig.SetDeleteValue(APath+'Name/Value',FName,'');
  AXMLConfig.SetDeleteValue(APath+'Enabled/Value',FEnabled,true);
end;

procedure TIDEException.ResetMaster;
begin
  FMaster := nil;
end;

{ =========================================================================== }
{ TIDEExceptions }
{ =========================================================================== }

function TIDEExceptions.Add(const AName: String): TIDEException;
begin
  Result := TIDEException(inherited Add(AName));
end;

function TIDEExceptions.Find(const AName: String): TIDEException;
begin
  Result := TIDEException(inherited Find(AName));
end;

constructor TIDEExceptions.Create;
begin
  inherited Create(TIDEException);
  AddDefault;
end;

function TIDEExceptions.GetItem(const AIndex: Integer): TIDEException;
begin
  Result := TIDEException(inherited GetItem(AIndex));
end;

procedure TIDEExceptions.LoadFromXMLConfig (const AXMLConfig: TXMLConfig;
  const APath: string);
var
  NewCount: Integer;
  i: Integer;
  IDEException: TIDEException;
begin
  Clear;
  NewCount := AXMLConfig.GetValue(APath + 'Count', 0);
  FIgnoreAll := AXMLConfig.GetValue(APath + 'IgnoreAll', False);
  for i := 0 to NewCount-1 do
  begin
    IDEException := TIDEException(inherited Add(''));
    IDEException.LoadFromXMLConfig(AXMLConfig,
                                    Format('%sItem%d/', [APath, i + 1]));
  end;
end;

procedure TIDEExceptions.SaveToXMLConfig (const AXMLConfig: TXMLConfig;
  const APath: string);
var
  Cnt: Integer;
  i: Integer;
  IDEException: TIDEException;
begin
  Cnt := Count;
  AXMLConfig.SetDeleteValue(APath + 'Count', Cnt, 0);
  AXMLConfig.SetDeleteValue(APath + 'IgnoreAll', IgnoreAll, False);
  for i := 0 to Cnt - 1 do
  begin
    IDEException := Items[i];
    IDEException.SaveToXMLConfig(AXMLConfig,
                                  Format('%sItem%d/', [APath, i + 1]));
  end;
end;

procedure TIDEExceptions.AddIfNeeded(AName: string);
begin
  if Find(AName) = nil then
    Add(AName);
end;

procedure TIDEExceptions.Reset;
begin
  inherited Reset;
  AddDefault;
end;

procedure TIDEExceptions.SetItem(const AIndex: Integer;
  const AValue: TIDEException);
begin
  inherited SetItem(Aindex, AValue);
end;

procedure TIDEExceptions.AddDefault;
begin
  AddIfNeeded('EAbort');
  AddIfNeeded('ECodetoolError');
  AddIfNeeded('EFOpenError');
end;

{ TIDELineInfo }

procedure TIDELineInfo.LineInfoChanged(const ASender: TObject; const ASource: String);
begin
  NotifyChange(ASource);
end;

procedure TIDELineInfo.SetMaster(const AMaster: TDBGLineInfo);
begin
  if FMaster = AMaster then Exit;

  if FMaster <> nil
  then begin
    FMaster.OnChange := nil;
  end;

  FMaster := AMaster;

  if FMaster <> nil
  then begin
    FMaster.OnChange := @LineInfoChanged;
  end;
end;

function TIDELineInfo.GetSource(const AIndex: Integer): String;
begin
  if Master = nil
  then Result := inherited GetSource(AIndex)
  else Result := Master.Sources[AIndex];
end;

procedure TIDELineInfo.NotifyChange(ASource: String);
var
  n: Integer;
  Notification: TIDELineInfoNotification;
begin
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TIDELineInfoNotification(FNotificationList[n]);
    if Assigned(Notification.FOnChange)
    then Notification.FOnChange(Self, ASource);
  end;
end;

constructor TIDELineInfo.Create;
begin
  FNotificationList := TList.Create;
  inherited Create;
end;

destructor TIDELineInfo.Destroy;
var
  n: Integer;
begin
  for n := FNotificationList.Count - 1 downto 0 do
    TDebuggerNotification(FNotificationList[n]).ReleaseReference;

  inherited;

  FreeAndNil(FNotificationList);
end;

procedure TIDELineInfo.AddNotification(const ANotification: TIDELineInfoNotification);
begin
  FNotificationList.Add(ANotification);
  ANotification.AddReference;
end;

procedure TIDELineInfo.RemoveNotification(const ANotification: TIDELineInfoNotification);
begin
  if FNotificationList.IndexOf(ANotification) >= 0 then
  begin
    FNotificationList.Remove(ANotification);
    ANotification.ReleaseReference;
  end;
end;

function TIDELineInfo.Count: Integer;
begin
  if Master = nil
  then Result := inherited Count
  else Result := Master.Count;
end;

function TIDELineInfo.GetAddress(const AIndex: Integer; const ALine: Integer): TDbgPtr;
begin
  if Master = nil
  then Result := inherited GetAddress(AIndex, ALine)
  else Result := Master.GetAddress(AIndex, ALine);
end;

function TIDELineInfo.GetInfo(AAdress: TDbgPtr; out ASource, ALine,
  AOffset: Integer): Boolean;
begin
  if Master = nil
  then Result := inherited GetInfo(AAdress, ASource, ALine, AOffset)
  else Result := Master.GetInfo(AAdress, ASource, ALine, AOffset);
end;

function TIDELineInfo.IndexOf(const ASource: String): integer;
begin
  if Master = nil
  then Result := inherited IndexOf(ASource)
  else Result := Master.IndexOf(ASource);
end;

procedure TIDELineInfo.Request(const ASource: String);
begin
  if Master = nil
  then inherited Request(ASource)
  else Master.Request(ASource);
end;

procedure TIDELineInfo.Cancel(const ASource: String);
begin
  if Master = nil
  then inherited Cancel(ASource)
  else Master.Cancel(ASource);
end;

{ TIDEDisassembler }

procedure TIDEDisassembler.DisassemblerChanged(Sender: TObject);
begin
  Changed;
end;

procedure TIDEDisassembler.SetMaster(AMaster: TDBGDisassembler);
begin
  if FMaster = AMaster then Exit;

  if FMaster <> nil
  then FMaster.OnChange := nil;

  FMaster := AMaster;

  if FMaster <> nil
  then FMaster.OnChange := @DisassemblerChanged;

  Changed;
end;

procedure TIDEDisassembler.DoChanged;
var
  n: Integer;
  Notification: TIDEDisassemblerNotification;
begin
  if FMaster <> nil
  then begin
    SetCountBefore(FMaster.CountBefore);
    SetCountAfter(FMaster.CountAfter);
    SetBaseAddr(FMaster.BaseAddr);
  end
  else Clear;

  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TIDEDisassemblerNotification(FNotificationList[n]);
    if Assigned(Notification.FOnChange)
    then Notification.FOnChange(Self);
  end;
end;

function TIDEDisassembler.InternalGetEntry(AIndex: Integer): TDisassemblerEntry;
begin
  if FMaster <> nil
  then Result := FMaster.Entries[AIndex]
  else Result := inherited InternalGetEntry(AIndex);
end;

function TIDEDisassembler.InternalGetEntryPtr(AIndex: Integer): PDisassemblerEntry;
begin
  if FMaster <> nil
  then Result := FMaster.EntriesPtr[AIndex]
  else Result := inherited InternalGetEntryPtr(AIndex);
end;

constructor TIDEDisassembler.Create;
begin
  FNotificationList := TList.Create;
  inherited Create;
end;

destructor TIDEDisassembler.Destroy;
var
  n: Integer;
begin
  if FMaster <> nil
  then FMaster.OnChange := nil;
  FMaster := nil;
  for n := FNotificationList.Count - 1 downto 0 do
    TDebuggerNotification(FNotificationList[n]).ReleaseReference;

  inherited;
  FreeAndNil(FNotificationList);
end;

procedure TIDEDisassembler.AddNotification(const ANotification: TIDEDisassemblerNotification);
begin
  FNotificationList.Add(ANotification);
  ANotification.AddReference;
end;

procedure TIDEDisassembler.RemoveNotification(const ANotification: TIDEDisassemblerNotification);
begin
  FNotificationList.Remove(ANotification);
  ANotification.ReleaseReference;
end;

procedure TIDEDisassembler.Clear;
begin
  if FMaster <> nil
  then FMaster.Clear
  else inherited Clear;
end;

function TIDEDisassembler.PrepareRange(AnAddr: TDbgPtr; ALinesBefore,
  ALinesAfter: Integer): Boolean;
begin
  if (AnAddr = BaseAddr) and (ALinesBefore < CountBefore) and (ALinesAfter < CountAfter)
  then exit(True);

  if FMaster <> nil
  then Result := FMaster.PrepareRange(AnAddr, ALinesBefore, ALinesAfter)
  else Result := inherited PrepareRange(AnAddr, ALinesBefore, ALinesAfter);
end;

initialization
  DBG_DATA_MONITORS := DebugLogger.FindOrRegisterLogGroup('DBG_DATA_MONITORS' {$IFDEF DBG_DATA_MONITORS} , True {$ENDIF} );
  DBG_LOCATION_INFO := DebugLogger.FindOrRegisterLogGroup('DBG_LOCATION_INFO' {$IFDEF DBG_LOCATION_INFO} , True {$ENDIF} );

end.
