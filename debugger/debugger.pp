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

{$IFDEF DBG_STATE}  {$DEFINE DBG_STATE_EVENT} {$ENDIF}
{$IFDEF DBG_EVENTS} {$DEFINE DBG_STATE_EVENT} {$ENDIF}

interface

uses
  TypInfo, Classes, SysUtils, Laz_XMLCfg, math, FileUtil,
  LCLProc, LazConfigStorage, IDEProcs, DebugUtils, maps;

type
  // datatype pointing to data on the target
  TDBGPtr = type QWord;

  TDBGLocationRec = record
    Address: TDBGPtr;
    FuncName: String;
    SrcFile: String;
    SrcFullName: String;
    SrcLine: Integer;
  end;

  TDBGCommand = (
    dcRun,
    dcPause,
    dcStop,
    dcStepOver,
    dcStepInto,
    dcStepOut,
    dcRunTo,
    dcJumpto,
    dcBreak,
    dcWatch,
    dcLocal,
    dcEvaluate,
    dcModify,
    dcEnvironment,
    dcSetStackFrame,
    dcDisassemble,
    dcStepOverInstr,
    dcStepIntoInstr,
    dcSendConsoleInput
    );
  TDBGCommands = set of TDBGCommand;

  TDBGState = (
    dsNone,
    dsIdle,
    dsStop,
    dsPause,
    dsInternalPause,
    dsInit,
    dsRun,
    dsError,
    dsDestroying
    );

  TDBGExceptionType = (
    deInternal,
    deExternal,
    deRunError
  );

{
  Debugger states
  --------------------------------------------------------------------------
  dsNone:
    The debug object is created, but no instance of an external debugger
    exists.
    Initial state, leave with Init, enter with Done

  dsIdle:
    The external debugger is started, but no filename (or no other params
    required to start) were given.

  dsStop:
    (Optional) The execution of the target is stopped
    The external debugger is loaded and ready to (re)start the execution
    of the target.
    Breakpoints, watches etc can be defined

  dsPause:
    The debugger has paused the target. Target variables can be examined

  dsInternalPause:
    Pause, not visible to user.
    For examble auto continue breakpoint: Allow collection of Snapshot data

  dsInit:
    (Optional, Internal) The debugger is about to run

  dsRun:
    The target is running.

  dsError:
    Something unforseen has happened. A shutdown of the debugger is in
    most cases needed.

  -dsDestroying
    The debugger is about to be destroyed.
    Should normally happen immediate on calling Release.
    But the debugger may be in nested calls, and has to exit them first.
  --------------------------------------------------------------------------

}

  TValidState = (vsUnknown, vsValid, vsInvalid);

  TDBGEvaluateFlag =
    (defNoTypeInfo,        // No Typeinfo object will be returned
     defSimpleTypeInfo,    // Returns: Kind (skSimple, skClass, ..); TypeName (but does make no attempt to avoid an alias)
     defFullTypeInfo,      // Get all typeinfo, resolve all anchestors
     defClassAutoCast      // Find real class of instance, and use, instead of declared class of variable
    );
  TDBGEvaluateFlags = set of TDBGEvaluateFlag;

const
//  dcRunCommands = [dcRun,dcStepInto,dcStepOver,dcRunTo];
//  dsRunStates = [dsRun];

  XMLBreakPointsNode = 'BreakPoints';
  XMLBreakPointGroupsNode = 'BreakPointGroups';
  XMLWatchesNode = 'Watches';
  XMLExceptionsNode = 'Exceptions';

type
  EDebuggerException = class(Exception);
  EDBGExceptions = class(EDebuggerException);

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
    FWarnedAboutBreakGroup: Boolean;
  public
    procedure Init; override;
    procedure Load; override;
    procedure Save; override;
  public
    constructor Create;
    destructor Destroy; override;
    property DebuggerClass: String read FDebuggerClass write FDebuggerClass;
    property DlgWatchesConfig: TDebuggerWatchesDlgConfig read FTDebuggerWatchesDlgConfig;
  published
    {$IFdef MSWindows}
    property WarnedAboutBreakGroup: Boolean read FWarnedAboutBreakGroup write FWarnedAboutBreakGroup;
    {$ENDIF}
  end;

  { TFreeNotifyingObject }

  TFreeNotifyingObject = class
  private
    FFreeNotificationList: TMethodList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFreeeNotification(ANotification: TNotifyEvent);
    procedure RemoveFreeeNotification(ANotification: TNotifyEvent);
  end;

  { TRefCountedObject }

  TRefCountedObject = class(TFreeNotifyingObject)
  private
    FRefCount: Integer;
  protected
    procedure DoFree; virtual;
    property  RefCount: Integer read FRefCount;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure AddReference;
    procedure ReleaseReference;
  end;

  { TRefCntObjList }

  TRefCntObjList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

procedure ReleaseRefAndNil(var ARefCountedObject);

type

  TDebuggerLocationType = (dltUnknown,        // not jet looked up
                           dltUnresolvable,   // lookup failed
                           dltProject,
                           dltPackage
                          );
  TDebuggerLocationFlags = set of (dlfLoadError  // resolved but failed to load
                                  );

  { TDebuggerUnitInfo }

  TDebuggerUnitInfo = class(TRefCountedObject)
  private
    FFileName, FDbgFullName: String;
    FFlags: TDebuggerLocationFlags;
    FLocationName, FLocationOwnerName, FLocationFullFile: String;
    FLocationType: TDebuggerLocationType;
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
    function IsEqual(const AFileName: String; const AFullFileName: String): boolean;
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

  TDebuggerDataMonitor = class;
  TDebuggerDataSupplier = class;
  TIDEBreakPoints = class;
  TIDEBreakPointGroup = class;
  TIDEBreakPointGroups = class;
  TWatch = class;
  TWatches = class;
  TCurrentWatch = class;
  TCurrentWatches = class;
  TWatchesMonitor = class;
  TWatchesSupplier = class;
  TLocalsMonitor = class;
  TLocalsSupplier = class;
  TCurrentLocals = class;
  TIDELineInfo = class;
  TCallStack = class;
  TCallStackMonitor = class;
  TCallStackSupplier = class;
  TThreadsMonitor = class;
  TThreadsSupplier = class;
  TSnapshotManager = class;
  TDebugger = class;

  TOnSaveFilenameToConfig = procedure(var Filename: string) of object;
  TOnLoadFilenameFromConfig = procedure(var Filename: string) of object;
  TOnGetGroupByName = function(const GroupName: string): TIDEBreakPointGroup of object;

  TDebuggerDataState = (ddsUnknown,                    //
                        ddsRequested, ddsEvaluating,   //
                        ddsValid,                      // Got a valid value
                        ddsInvalid,                    // Does not have a value
                        ddsError                       // Error, but got some Value to display (e.g. error msg)
                       );

  { TDebuggerDataMonitor }

  TDebuggerDataMonitor = class
  private
    FSupplier: TDebuggerDataSupplier;
    procedure SetSupplier(const AValue: TDebuggerDataSupplier);
  protected
    procedure DoNewSupplier; virtual;
    property  Supplier: TDebuggerDataSupplier read FSupplier write SetSupplier;
  public
    destructor Destroy; override;
  end;

  { TDebuggerDataSupplier }

  TDebuggerDataSupplier = class
  private
    FDebugger: TDebugger;
    FMonitor: TDebuggerDataMonitor;
    procedure SetMonitor(const AValue: TDebuggerDataMonitor);
  protected
    property Debugger: TDebugger read FDebugger write FDebugger;
    property Monitor: TDebuggerDataMonitor read FMonitor write SetMonitor;
  public
    constructor Create(const ADebugger: TDebugger);
    destructor  Destroy; override;
  end;

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

  { TDebuggerDataMonitorEx }

  TDebuggerDataMonitorEx = class(TDebuggerDataMonitor)
  private
    FSnapshots: TDebuggerDataSnapShotList;
  protected
    function CreateSnapshot(CreateEmpty: Boolean = False): TObject; virtual;
    function GetSnapshotObj(AnID: Pointer): TObject; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure NewSnapshot(AnID: Pointer; CreateEmpty: Boolean = False);
    procedure RemoveSnapshot(AnID: Pointer);
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

  TDBGBreakPointKind = (
    bpkSource,  // source breakpoint
    bpkAddress, // address breakpoint
    bpkData     // data/watchpoint
  );

  TDBGWatchPointScope = (
    wpsLocal,
    wpsGlobal
  );

  TDBGWatchPointKind = (
    wpkWrite,
    wpkRead,
    wpkReadWrite
  );

  TBaseBreakPoint = class;
  TDBGBreakPoint = class;

  { TIDEBreakPointGroupList }

  TIDEBreakPointGroupList = class
  private
    FList: TFPList;
    FOwner: TBaseBreakPoint;
    function GetItem(AIndex: Integer): TIDEBreakPointGroup;
  public
    constructor Create(AOwner: TBaseBreakPoint);
    destructor Destroy; override;
    procedure Assign(ASrc: TIDEBreakPointGroupList);
    procedure Clear;
    function  Add(const AGroup: TIDEBreakPointGroup): Integer;
    procedure Remove(const AGroup: TIDEBreakPointGroup);
    function  IndexOf(const AGroup: TIDEBreakPointGroup): Integer;
    function  Count: Integer;
    property Items[AIndex: Integer]: TIDEBreakPointGroup read GetItem; default;
  end;

  { TBaseBreakPoint }

  TBaseBreakPoint = class(TDelayedUdateItem)
  private
    FAddress: TDBGPtr;
    FWatchData: String;
    FEnabled: Boolean;
    FExpression: String;
    FHitCount: Integer;
    FBreakHitCount: Integer;
    FKind: TDBGBreakPointKind;
    FLine: Integer;
    FWatchScope: TDBGWatchPointScope;
    FWatchKind: TDBGWatchPointKind;
    FSource: String;
    FValid: TValidState;
    FInitialEnabled: Boolean;
  protected
    procedure AssignLocationTo(Dest: TPersistent); virtual;
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoBreakHitCountChange; virtual;
    procedure DoExpressionChange; virtual;
    procedure DoEnableChange; virtual;
    procedure DoHit(const ACount: Integer; var AContinue: Boolean); virtual;
    procedure SetHitCount(const AValue: Integer);
    procedure DoKindChange; virtual;
    procedure SetValid(const AValue: TValidState);
  protected
    // virtual properties
    function GetAddress: TDBGPtr; virtual;
    function GetBreakHitCount: Integer; virtual;
    function GetEnabled: Boolean; virtual;
    function GetExpression: String; virtual;
    function GetHitCount: Integer; virtual;
    function GetKind: TDBGBreakPointKind; virtual;
    function GetLine: Integer; virtual;
    function GetSource: String; virtual;
    function GetWatchData: String; virtual;
    function GetWatchScope: TDBGWatchPointScope; virtual;
    function GetWatchKind: TDBGWatchPointKind; virtual;
    function GetValid: TValidState; virtual;

    procedure SetAddress(const AValue: TDBGPtr); virtual;
    procedure SetBreakHitCount(const AValue: Integer); virtual;
    procedure SetEnabled(const AValue: Boolean); virtual;
    procedure SetExpression(const AValue: String); virtual;
    procedure SetInitialEnabled(const AValue: Boolean); virtual;
    procedure SetKind(const AValue: TDBGBreakPointKind); virtual;
  public
    constructor Create(ACollection: TCollection); override;
    // PublicProtectedFix ide/debugmanager.pas(867,32) Error: identifier idents no member "SetLocation"
    property BreakHitCount: Integer read GetBreakHitCount write SetBreakHitCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Expression: String read GetExpression write SetExpression;
    property HitCount: Integer read GetHitCount;
    property InitialEnabled: Boolean read FInitialEnabled write SetInitialEnabled;
    property Kind: TDBGBreakPointKind read GetKind write SetKind;
    property Valid: TValidState read GetValid;
  public
    procedure SetLocation(const ASource: String; const ALine: Integer); virtual;
    procedure SetWatch(const AData: String; const AScope: TDBGWatchPointScope;
                       const AKind: TDBGWatchPointKind); virtual;
    // bpkAddress
    property Address: TDBGPtr read GetAddress write SetAddress;
    // bpkSource
    //   TDBGBreakPoint: Line is the line-number as stored in the debug info
    //   TIDEBreakPoint: Line is the location in the Source (potentially modified Source)
    property Line: Integer read GetLine;
    property Source: String read GetSource;
    // bpkData
    property WatchData: String read GetWatchData;
    property WatchScope: TDBGWatchPointScope read GetWatchScope;
    property WatchKind: TDBGWatchPointKind read GetWatchKind;
  end;
  TBaseBreakPointClass = class of TBaseBreakPoint;

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
  protected
    procedure AssignLocationTo(Dest: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoChanged; override;
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

  { TDBGBreakPoint }

  TDBGBreakPoint = class(TBaseBreakPoint)
  private
    FSlave: TBaseBreakPoint;
    function GetDebugger: TDebugger;
    procedure SetSlave(const ASlave : TBaseBreakPoint);
  protected
    procedure SetEnabled(const AValue: Boolean); override;
    procedure DoChanged; override;
    procedure DoStateChange(const AOldState: TDBGState); virtual;
    procedure DoLogMessage(const AMessage: String); virtual;
    procedure DoLogCallStack(const Limit: Integer); virtual;
    procedure DoLogExpression(const AnExpression: String); virtual; // implemented in TGDBMIBreakpoint
    property  Debugger: TDebugger read GetDebugger;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Hit(var ACanContinue: Boolean);
    property Slave: TBaseBreakPoint read FSlave write SetSlave;
  end;
  TDBGBreakPointClass = class of TDBGBreakPoint;

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

  { TBaseBreakPoints }

  TBaseBreakPoints = class(TCollection)
  private
  protected
  public
    constructor Create(const ABreakPointClass: TBaseBreakPointClass);
    function Add(const ASource: String; const ALine: Integer): TBaseBreakPoint; overload;
    function Add(const AAddress: TDBGPtr): TBaseBreakPoint; overload;
    function Add(const AData: String; const AScope: TDBGWatchPointScope;
                 const AKind: TDBGWatchPointKind): TBaseBreakPoint; overload;
    function Find(const ASource: String; const ALine: Integer): TBaseBreakPoint; overload;
    function Find(const ASource: String; const ALine: Integer; const AIgnore: TBaseBreakPoint): TBaseBreakPoint; overload;
    function Find(const AAddress: TDBGPtr): TBaseBreakPoint; overload;
    function Find(const AAddress: TDBGPtr; const AIgnore: TBaseBreakPoint): TBaseBreakPoint; overload;
    function Find(const AData: String; const AScope: TDBGWatchPointScope;
                  const AKind: TDBGWatchPointKind): TBaseBreakPoint; overload;
    function Find(const AData: String; const AScope: TDBGWatchPointScope;
                  const AKind: TDBGWatchPointKind; const AIgnore: TBaseBreakPoint): TBaseBreakPoint; overload;
    // no items property needed, it is "overridden" anyhow
  end;

  TDBGBreakPoints = class;

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

  { TDBGBreakPoints }

  TDBGBreakPoints = class(TBaseBreakPoints)
  private
    FDebugger: TDebugger;  // reference to our debugger
    function GetItem(const AnIndex: Integer): TDBGBreakPoint;
    procedure SetItem(const AnIndex: Integer; const AValue: TDBGBreakPoint);
  protected
    procedure DoStateChange(const AOldState: TDBGState); virtual;
    property  Debugger: TDebugger read FDebugger;
  public
    constructor Create(const ADebugger: TDebugger;
                       const ABreakPointClass: TDBGBreakPointClass);
    function Add(const ASource: String; const ALine: Integer): TDBGBreakPoint; overload;
    function Add(const AAddress: TDBGPtr): TDBGBreakPoint; overload;
    function Add(const AData: String; const AScope: TDBGWatchPointScope;
                 const AKind: TDBGWatchPointKind): TDBGBreakPoint; overload;
    function Find(const ASource: String; const ALine: Integer): TDBGBreakPoint; overload;
    function Find(const ASource: String; const ALine: Integer; const AIgnore: TDBGBreakPoint): TDBGBreakPoint; overload;
    function Find(const AAddress: TDBGPtr): TDBGBreakPoint; overload;
    function Find(const AAddress: TDBGPtr; const AIgnore: TDBGBreakPoint): TDBGBreakPoint; overload;
    function Find(const AData: String; const AScope: TDBGWatchPointScope;
                  const AKind: TDBGWatchPointKind): TDBGBreakPoint; overload;
    function Find(const AData: String; const AScope: TDBGWatchPointScope;
                  const AKind: TDBGWatchPointKind; const AIgnore: TDBGBreakPoint): TDBGBreakPoint; overload;

    property Items[const AnIndex: Integer]: TDBGBreakPoint read GetItem write SetItem; default;
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


{$region Debug Info ***********************************************************}
(******************************************************************************)
(**                                                                          **)
(**   D E B U G   I N F O R M A T I O N                                      **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

  type
  TDBGSymbolKind = (skClass, skRecord, skEnum, skSet, skProcedure, skFunction, skSimple, skPointer, skVariant);
  TDBGSymbolAttribute = (saRefParam,        // var, const, constref passed by reference
                         saInternalPointer, // PointerToObject
                         saArray, saDynArray
                        );
  TDBGSymbolAttributes = set of TDBGSymbolAttribute;
  TDBGFieldLocation = (flPrivate, flProtected, flPublic, flPublished);
  TDBGFieldFlag = (ffVirtual,ffConstructor,ffDestructor);
  TDBGFieldFlags = set of TDBGFieldFlag;

  TDBGType = class;

  TDBGValue = record
    AsString: ansistring;
    case integer of
      0: (As8Bits: BYTE);
      1: (As16Bits: WORD);
      2: (As32Bits: DWORD);
      3: (As64Bits: QWORD);
      4: (AsSingle: Single);
      5: (AsDouble: Double);
      6: (AsPointer: Pointer);
  end;

  { TDBGField }

  TDBGField = class(TObject)
  private
    FRefCount: Integer;
  protected
    FName: String;
    FFlags: TDBGFieldFlags;
    FLocation: TDBGFieldLocation;
    FDBGType: TDBGType;
    FClassName: String;
    procedure IncRefCount;
    procedure DecRefCount;
    property RefCount: Integer read FRefCount;
  public
    constructor Create(const AName: String; ADBGType: TDBGType;
                       ALocation: TDBGFieldLocation; AFlags: TDBGFieldFlags = [];
                       AClassName: String = '');
    destructor Destroy; override;
    property Name: String read FName;
    property DBGType: TDBGType read FDBGType;
    property Location: TDBGFieldLocation read FLocation;
    property Flags: TDBGFieldFlags read FFlags;
    property ClassName: String read FClassName; // the class in which the field was declared
  end;

  { TDBGFields }

  TDBGFields = class(TObject)
  private
    FList: TList;
    function GetField(const AIndex: Integer): TDBGField;
    function GetCount: Integer;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[const AIndex: Integer]: TDBGField read GetField; default;
    procedure Add(const AField: TDBGField);
  end;

  TDBGTypes = class(TObject)
  private
    function GetType(const AIndex: Integer): TDBGType;
    function GetCount: Integer;
  protected
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[const AIndex: Integer]: TDBGType read GetType; default;
  end;

  { TDBGType }

  TDBGType = class(TObject)
  protected
    FAncestor: String;
    FResult: TDBGType;
    FResultString: String;
    FArguments: TDBGTypes;
    FAttributes: TDBGSymbolAttributes;
    FFields: TDBGFields;
    FKind: TDBGSymbolKind;
    FMembers: TStrings;
    FTypeName: String;
    FTypeDeclaration: String;
    FDBGValue: TDBGValue;
    procedure Init; virtual;
  public
    Value: TDBGValue;
    constructor Create(AKind: TDBGSymbolKind; const ATypeName: String);
    constructor Create(AKind: TDBGSymbolKind; const AArguments: TDBGTypes; AResult: TDBGType = nil);
    destructor Destroy; override;
    property Ancestor: String read FAncestor;
    property Arguments: TDBGTypes read FArguments;
    property Fields: TDBGFields read FFields;
    property Kind: TDBGSymbolKind read FKind;
    property Attributes: TDBGSymbolAttributes read FAttributes;
    property TypeName: String read FTypeName;               // Name/Alias as in type section. One pascal token, or empty
    property TypeDeclaration: String read FTypeDeclaration; // Declaration (for array, set, enum, ..)
    property Members: TStrings read FMembers;
    property Result: TDBGType read FResult;
  end;
{%endregion   ^^^^^  Debug Info  ^^^^^   }

{%region Watches **************************************************************
 ******************************************************************************
 **                                                                          **
 **   W A T C H E S                                                          **
 **                                                                          **
 ******************************************************************************
 ******************************************************************************}

  TWatchDisplayFormat =
    (wdfDefault,
     wdfStructure,
     wdfChar, wdfString,
     wdfDecimal, wdfUnsigned, wdfFloat, wdfHex,
     wdfPointer,
     wdfMemDump
    );

  TRegisterDisplayFormat =
    (rdDefault, rdHex, rdBinary, rdOctal, rdDecimal, rdRaw
    );

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
       procedure(const ASender: TWatches; const AWatch: TWatch) of object;

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

  TWatchValue = class(TFreeNotifyingObject)
  private
    FDisplayFormat: TWatchDisplayFormat;
    FStackFrame: Integer;
    FThreadId: Integer;
    FValidity: TDebuggerDataState;
    FWatch: TWatch;
    function GetTypeInfo: TDBGType;
    function GetValue: String;
    procedure SetValidity(const AValue: TDebuggerDataState);
  protected
    FTypeInfo: TDBGType;
    FValue: String;
    procedure RequestData; virtual;
    procedure ValidityChanged; virtual;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                              const APath: string);
  public
    constructor Create; virtual;  overload;
    constructor Create(AOwnerWatch: TWatch);  overload;
    constructor Create(AOwnerWatch: TWatch;
                       const AThreadId: Integer;
                       const AStackFrame: Integer;
                       const ADisplayFormat: TWatchDisplayFormat);  overload;
    destructor Destroy; override;
    procedure Assign(AnOther: TWatchValue);
    property DisplayFormat: TWatchDisplayFormat read FDisplayFormat;
    property ThreadId: Integer read FThreadId;
    property StackFrame: Integer read FStackFrame;
    property Watch: TWatch read FWatch;
  public
    property Validity: TDebuggerDataState read FValidity write SetValidity;
    property Value: String read GetValue;
    property TypeInfo: TDBGType read GetTypeInfo;
  end;

  { TWatchValueList }

  TWatchValueList = class
  private
    FList: TList;
    FWatch: TWatch;
    function GetEntry(const AThreadId: Integer; const AStackFrame: Integer;
                     const ADisplayFormat: TWatchDisplayFormat): TWatchValue;
    function GetEntryByIdx(AnIndex: integer): TWatchValue;
  protected
    function CreateEntry(const AThreadId: Integer; const AStackFrame: Integer;
                         const ADisplayFormat: TWatchDisplayFormat): TWatchValue; virtual;
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
    property Entries[const AThreadId: Integer; const AStackFrame: Integer;
                     const ADisplayFormat: TWatchDisplayFormat]: TWatchValue
             read GetEntry; default;
    property Watch: TWatch read FWatch;
  end;

  { TWatch }

  TWatch = class(TDelayedUdateItem)
  private
    FEnabled: Boolean;
    FExpression: String;
    FDisplayFormat: TWatchDisplayFormat;
    FValueList: TWatchValueList;
    function GetEnabled: Boolean;
    function GetValue(const AThreadId: Integer; const AStackFrame: Integer): TWatchValue;
    function GetValueEx(const AThreadId: Integer; const AStackFrame: Integer;
                    const ADisplayFormat: TWatchDisplayFormat): TWatchValue;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function CreateValueList: TWatchValueList; virtual;
    procedure DoEnableChange; virtual;
    procedure DoExpressionChange; virtual;
    procedure DoDisplayFormatChanged; virtual;
  protected
    // virtual properties
    function GetExpression: String; virtual;
    function GetDisplayFormat: TWatchDisplayFormat; virtual;

    procedure SetEnabled(const AValue: Boolean); virtual;
    procedure SetExpression(const AValue: String); virtual;
    procedure SetDisplayFormat(const AValue: TWatchDisplayFormat); virtual;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                              const APath: string);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure ClearValues;
  public
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Expression: String read GetExpression write SetExpression;
    property DisplayFormat: TWatchDisplayFormat read GetDisplayFormat write SetDisplayFormat;
  public
    property Values[const AThreadId: Integer; const AStackFrame: Integer]: TWatchValue
             read GetValue;
    property ValuesEx[const AThreadId: Integer; const AStackFrame: Integer;
                    const ADisplayFormat: TWatchDisplayFormat]: TWatchValue
             read GetValueEx;
  end;
  TBaseWatchClass = class of TWatch;

  { TWatches }

  TWatches = class(TCollection)
  private
    function GetItem(const AnIndex: Integer): TWatch;
    procedure SetItem(const AnIndex: Integer; const AValue: TWatch);
  protected
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                              APath: string);
  public
    constructor Create;
    constructor Create(const AWatchClass: TBaseWatchClass);
    function Add(const AExpression: String): TWatch;
    function Find(const AExpression: String): TWatch;
    property Items[const AnIndex: Integer]: TWatch read GetItem write SetItem; default;
    procedure ClearValues;
  end;

  { TCurrentWatchValue }

  TCurrentWatchValue = class(TWatchValue)
  private
    FSnapShot: TWatchValue;
    procedure SetSnapShot(const AValue: TWatchValue);
  protected
    procedure RequestData; override;
    procedure ValidityChanged; override;
  public
    property SnapShot: TWatchValue read FSnapShot write SetSnapShot;
  public
    procedure SetTypeInfo(const AValue: TDBGType);
    procedure SetValue(const AValue: String);
  end;

  { TCurrentWatchValueList }

  TCurrentWatchValueList = class(TWatchValueList)
  private
    FSnapShot: TWatchValueList;
    procedure SetSnapShot(const AValue: TWatchValueList);
  protected
    function CreateEntry(const AThreadId: Integer; const AStackFrame: Integer;
      const ADisplayFormat: TWatchDisplayFormat): TWatchValue; override;
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

  TCurrentWatches = class(TWatches)
  private
    FMonitor: TWatchesMonitor;
    FSnapShot: TWatches;
    procedure SetSnapShot(const AValue: TWatches);
    procedure WatchesChanged(Sender: TObject);
  protected
    function GetItem(const AnIndex: Integer): TCurrentWatch;
    procedure SetItem(const AnIndex: Integer; const AValue: TCurrentWatch);
  protected
    procedure NotifyAdd(const AWatch: TCurrentWatch); virtual;    // called when a watch is added
    procedure NotifyRemove(const AWatch: TCurrentWatch); virtual; // called by watch when destructed
    procedure Update(Item: TCollectionItem); override;
    procedure RequestData(AWatchValue: TCurrentWatchValue);
    property SnapShot: TWatches read FSnapShot write SetSnapShot;
  public
    constructor Create(AMonitor: TWatchesMonitor);
    // Watch
    function Add(const AExpression: String): TCurrentWatch;
    function Find(const AExpression: String): TCurrentWatch;
    // IDE
    procedure LoadFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
    procedure SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string);
  public
    property Items[const AnIndex: Integer]: TCurrentWatch read GetItem
                                                      write SetItem; default;
  end;

  { TWatchesMonitor }

  TWatchesMonitor = class(TDebuggerDataMonitorEx)
  private
    FNotificationList: TWatchesNotificationList;
    FCurrentWatches: TCurrentWatches;
    function GetSnapshot(AnID: Pointer): TWatches;
    function  GetSupplier: TWatchesSupplier;
    procedure SetSupplier(const AValue: TWatchesSupplier);
  protected
    //procedure NotifyChange
    procedure NotifyAdd(const AWatches: TCurrentWatches; const AWatch: TCurrentWatch);
    procedure NotifyRemove(const AWatches: TCurrentWatches; const AWatch: TCurrentWatch);
    procedure NotifyUpdate(const AWatches: TCurrentWatches; const AWatch: TCurrentWatch);
    procedure RequestData(AWatchValue: TCurrentWatchValue);
    function CreateSnapshot(CreateEmpty: Boolean = False): TObject; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotification(const ANotification: TWatchesNotification);
    procedure RemoveNotification(const ANotification: TWatchesNotification);
    property CurrentWatches: TCurrentWatches read FCurrentWatches;
    property Snapshots[AnID: Pointer]: TWatches read GetSnapshot;
    property Supplier: TWatchesSupplier read GetSupplier write SetSupplier;
  public
    procedure Clear;
    procedure LoadFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
    procedure SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string);
  end;

  { TWatchesSupplier }

  TWatchesSupplier = class(TDebuggerDataSupplier)
  private
    FNotifiedState: TDBGState;
    function GetCurrentWatches: TCurrentWatches;
    function GetMonitor: TWatchesMonitor;
    procedure SetMonitor(const AValue: TWatchesMonitor);
  protected
    procedure RequestData(AWatchValue: TCurrentWatchValue);
    procedure InternalRequestData(AWatchValue: TCurrentWatchValue); virtual;
    procedure DoStateChange(const AOldState: TDBGState); virtual;
  public
    constructor Create(const ADebugger: TDebugger);
    property Monitor: TWatchesMonitor read GetMonitor write SetMonitor;
    property CurrentWatches: TCurrentWatches read GetCurrentWatches;
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

  { TLocals }

  TLocals = class(TRefCountedObject)
  private
    function GetName(const AnIndex: Integer): String;
    function GetValue(const AnIndex: Integer): String;
  protected
    FLocals: TStringList;
    FStackFrame: Integer;
    FThreadId: Integer;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                              APath: string);
  public
    procedure Assign(AnOther: TLocals);
    constructor Create;
    constructor Create(AThreadId, AStackFrame: Integer);
    constructor CreateCopy(const ASource: TLocals);
    destructor Destroy; override;
    function Count: Integer; virtual;
  public
    property Names[const AnIndex: Integer]: String read GetName;
    property Values[const AnIndex: Integer]: String read GetValue;
    property ThreadId: Integer read FThreadId;
    property StackFrame: Integer read FStackFrame;
  end;

  { TLocalsList }

  TLocalsList = class
  private
    FList: TList;
    function GetEntry(const AThreadId: Integer; const AStackFrame: Integer): TLocals;
    function GetEntryByIdx(const AnIndex: Integer): TLocals;
  protected
    function CreateEntry(const AThreadId: Integer; const AStackFrame: Integer): TLocals; virtual;
    procedure Add(AnEntry: TLocals);
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                              APath: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(AnOther: TLocalsList);
    procedure Clear;
    function Count: Integer;
    property EntriesByIdx[const AnIndex: Integer]: TLocals read GetEntryByIdx;
    property Entries[const AThreadId: Integer; const AStackFrame: Integer]: TLocals
             read GetEntry; default;
  end;

  { TCurrentLocals }

  TCurrentLocals = class(TLocals)
  private
    FMonitor: TLocalsMonitor;
    FDataValidity: TDebuggerDataState;
    FSnapShot: TLocals;
    procedure SetSnapShot(const AValue: TLocals);
  protected
    property SnapShot: TLocals read FSnapShot write SetSnapShot;
  public
    constructor Create(AMonitor: TLocalsMonitor; AThreadId, AStackFrame: Integer);
    function Count: Integer; override;
    procedure Clear;
    procedure Add(const AName, AValue: String);
    procedure SetDataValidity(AValidity: TDebuggerDataState);
  end;

  { TCurrentLocalsList }

  TCurrentLocalsList = class(TLocalsList)
  private
    FMonitor: TLocalsMonitor;
    FSnapShot: TLocalsList;
    procedure SetSnapShot(const AValue: TLocalsList);
  protected
    function CreateEntry(const AThreadId: Integer; const AStackFrame: Integer): TLocals; override;
    property SnapShot: TLocalsList read FSnapShot write SetSnapShot;
  public
    constructor Create(AMonitor: TLocalsMonitor);
  end;

  { TLocalsMonitor }

  TLocalsMonitor = class(TDebuggerDataMonitorEx)
  private
    FCurrentLocalsList: TCurrentLocalsList;
    FNotificationList: TDebuggerChangeNotificationList;
    function GetSnapshot(AnID: Pointer): TLocalsList;
    function GetSupplier: TLocalsSupplier;
    procedure SetSupplier(const AValue: TLocalsSupplier);
  protected
    procedure NotifyChange(ALocals: TCurrentLocals);
    procedure DoNewSupplier; override;
    procedure RequestData(ALocals: TCurrentLocals);
    function CreateSnapshot(CreateEmpty: Boolean = False): TObject; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddNotification(const ANotification: TLocalsNotification);
    procedure RemoveNotification(const ANotification: TLocalsNotification);
    property  CurrentLocalsList: TCurrentLocalsList read FCurrentLocalsList;
    property  Snapshots[AnID: Pointer]: TLocalsList read GetSnapshot;
    property  Supplier: TLocalsSupplier read GetSupplier write SetSupplier;
  end;

  { TLocalsSupplier }

  TLocalsSupplier = class(TDebuggerDataSupplier)
  private
    function GetCurrentLocalsList: TCurrentLocalsList;
    function GetMonitor: TLocalsMonitor;
    procedure SetMonitor(const AValue: TLocalsMonitor);
  protected
    procedure RequestData(ALocals: TCurrentLocals); virtual;
  public
    procedure DoStateChange(const AOldState: TDBGState); virtual;
    property  CurrentLocalsList: TCurrentLocalsList read GetCurrentLocalsList;
    property  Monitor: TLocalsMonitor read GetMonitor write SetMonitor;
  end;

  {%endregion   ^^^^^  Locals  ^^^^^   }


{%region Line Info ************************************************************
 ******************************************************************************
 **                                                                          **
 **   L I N E   I N F O                                                      **
 **                                                                          **
 ******************************************************************************
 ******************************************************************************}

  TIDELineInfoEvent = procedure(const ASender: TObject; const ASource: String) of object;
  { TBaseLineInfo }

  TBaseLineInfo = class(TObject)
  protected
    function GetSource(const AnIndex: integer): String; virtual;
  public
    constructor Create;
    function Count: Integer; virtual;
    function GetAddress(const AIndex: Integer; const ALine: Integer): TDbgPtr; virtual;
    function GetAddress(const ASource: String; const ALine: Integer): TDbgPtr;
    function GetInfo(AAdress: TDbgPtr; out ASource, ALine, AOffset: Integer): Boolean; virtual;
    function IndexOf(const ASource: String): integer; virtual;
    procedure Request(const ASource: String); virtual;
  public
    property Sources[const AnIndex: Integer]: String read GetSource;
  end;

  { TIDELineInfo }

  TIDELineInfoNotification = class(TDebuggerNotification)
  private
    FOnChange: TIDELineInfoEvent;
  public
    property OnChange: TIDELineInfoEvent read FOnChange write FOnChange;
  end;

  TDBGLineInfo = class;
  TIDELineInfo = class(TBaseLineInfo)
  private
    FNotificationList: TList;
    FMaster: TDBGLineInfo;
    procedure LineInfoChanged(const ASender: TObject; const ASource: String);
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
    property Master: TDBGLineInfo read FMaster write SetMaster;
  end;

  { TDBGLineInfo }

  TDBGLineInfo = class(TBaseLineInfo)
  private
    FDebugger: TDebugger;  // reference to our debugger
    FOnChange: TIDELineInfoEvent;
  protected
    procedure Changed(ASource: String); virtual;
    procedure DoChange(ASource: String);
    procedure DoStateChange(const AOldState: TDBGState); virtual;
    property Debugger: TDebugger read FDebugger;
  public
    constructor Create(const ADebugger: TDebugger);
    property OnChange: TIDELineInfoEvent read FOnChange write FOnChange;
  end;
  {%endregion   ^^^^^  Line Info  ^^^^^   }

{%region Register *************************************************************
 ******************************************************************************
 **                                                                          **
 **   R E G I S T E R S                                                      **
 **                                                                          **
 ******************************************************************************
 ******************************************************************************}

  { TBaseRegisters }

  TBaseRegisters = class(TObject)
  protected
    FUpdateCount: Integer;
    function GetModified(const AnIndex: Integer): Boolean; virtual;
    function GetName(const AnIndex: Integer): String; virtual;
    function GetValue(const AnIndex: Integer): String; virtual;
    function GetFormat(const AnIndex: Integer): TRegisterDisplayFormat; virtual;
    procedure SetFormat(const AnIndex: Integer; const AValue: TRegisterDisplayFormat); virtual;
    procedure ChangeUpdating; virtual;
    function  Updating: Boolean;
  public
    constructor Create;
    function Count: Integer; virtual;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    property Modified[const AnIndex: Integer]: Boolean read GetModified;
    property Names[const AnIndex: Integer]: String read GetName;
    property Values[const AnIndex: Integer]: String read GetValue;
    property Formats[const AnIndex: Integer]: TRegisterDisplayFormat
             read GetFormat write SetFormat;
  end;

  { TIDERegisters }

  TIDERegistersNotification = class(TDebuggerNotification)
  private
    FOnChange: TNotifyEvent;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TDBGRegisters = class;

  TIDERegisters = class(TBaseRegisters)
  private
    FNotificationList: TList;
    FMaster: TDBGRegisters;
    procedure RegistersChanged(Sender: TObject);
    procedure SetMaster(const AMaster: TDBGRegisters);
  protected
    function GetModified(const AnIndex: Integer): Boolean; override;
    function GetName(const AnIndex: Integer): String; override;
    function GetValue(const AnIndex: Integer): String; override;
    function GetFormat(const AnIndex: Integer): TRegisterDisplayFormat; override;
    procedure SetFormat(const AnIndex: Integer; const AValue: TRegisterDisplayFormat); override;
  protected
    procedure NotifyChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotification(const ANotification: TIDERegistersNotification);
    procedure RemoveNotification(const ANotification: TIDERegistersNotification);
    function Count: Integer; override;
    property Master: TDBGRegisters read FMaster write SetMaster;
  end;

  { TDBGRegisters }

  TDBGRegisters = class(TBaseRegisters)
  private
    FDebugger: TDebugger;  // reference to our debugger
    FOnChange: TNotifyEvent;
    FChanged: Boolean;
  protected
    procedure Changed; virtual;
    procedure DoChange;
    procedure DoStateChange(const AOldState: TDBGState); virtual;
    function GetCount: Integer; virtual;
    procedure SetFormat(const AnIndex: Integer; const AValue: TRegisterDisplayFormat); override;
    procedure ChangeUpdating; override;
    property Debugger: TDebugger read FDebugger;
  public
    function Count: Integer; override;
    constructor Create(const ADebugger: TDebugger);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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

  TCallStackEntry = class(TObject)
  private
    FOwner: TCallStack;
    FIndex: Integer;
    FAdress: TDbgPtr;
    FFunctionName: String;
    FLine: Integer;
    FArguments: TStrings;
    FUnitInfo: TDebuggerUnitInfo;
    FState: TDebuggerDataState;
    function GetArgumentCount: Integer;
    function GetArgumentName(const AnIndex: Integer): String;
    function GetArgumentValue(const AnIndex: Integer): String;
    function GetFunctionName: String;
    function GetSource: String;
    procedure SetUnitInfo(AUnitInfo: TDebuggerUnitInfo);
  protected
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                    const APath: string;
                                    AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                   );
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                                  APath: string;
                                  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                 );
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
                   const AUnitInfo: TDebuggerUnitInfo;
                   const ALine: Integer; AState: TDebuggerDataState = ddsValid);
    function GetFunctionWithArg: String;
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

  TCallStack = class(TFreeNotifyingObject)
  private
    FThreadId: Integer;
    FCurrent: Integer;
    FList: TList;
  protected
    function IndexError(AIndex: Integer): TCallStackEntry;

    function  GetCurrent: Integer; virtual;
    procedure SetCurrent(AValue: Integer); virtual;

    procedure Clear; virtual;
    function  GetCount: Integer; virtual;
    procedure SetCount(ACount: Integer); virtual;
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
    constructor Create;
    constructor CreateCopy(const ASource: TCallStack);
    destructor Destroy; override;
    procedure Assign(AnOther: TCallStack);
    procedure PrepareRange(AIndex, ACount: Integer); virtual;
    procedure ChangeCurrentIndex(ANewIndex: Integer); virtual;
    property Count: Integer read GetCount write SetCount;
    property CurrentIndex: Integer read GetCurrent write SetCurrent;
    property Entries[AIndex: Integer]: TCallStackEntry read GetEntry;
    property ThreadId: Integer read FThreadId write FThreadId;
  end;

  { TCallStackList }

  TCallStackList = class
  private
    FList: TList;
    function GetEntry(const AIndex: Integer): TCallStack;
  protected
    function  GetEntryForThread(const AThreadId: Integer): TCallStack; virtual;
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
    procedure Assign(AnOther: TCallStackList);
    procedure Clear;
    function Count: Integer; // Count of already requested CallStacks (via ThreadId)
    property Entries[const AIndex: Integer]: TCallStack read GetEntry; default;
    property EntriesForThreads[const AThreadId: Integer]: TCallStack read GetEntryForThread;
  end;

  { TCurrentCallStack }

  TCurrentCallStack = class(TCallStack)
  private
    FMonitor: TCallStackMonitor;
    FCountValidity: TDebuggerDataState;
    FCurrentValidity: TDebuggerDataState;
    FNewCurrentIndex: Integer;
    FPreparing: Boolean;
    FSnapShot: TCallStack;
    FEntries: TMap;        // list of created entries
    FCount: Integer;
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
  public
    constructor Create(AMonitor: TCallStackMonitor);
    destructor Destroy; override;
    procedure Assign(AnOther: TCallStack);
    procedure PrepareRange(AIndex, ACount: Integer); override;
    procedure ChangeCurrentIndex(ANewIndex: Integer); override;
    procedure DoEntriesCreated;
    procedure DoEntriesUpdated;
    property LowestUnknown: Integer read FLowestUnknown;
    property HighestUnknown: Integer read FHighestUnknown;
    property RawEntries: TMap read FEntries;
    property NewCurrentIndex: Integer read FNewCurrentIndex;
    property SnapShot: TCallStack read FSnapShot write SetSnapShot;
  public
    procedure SetCountValidity(AValidity: TDebuggerDataState);
    procedure SetCurrentValidity(AValidity: TDebuggerDataState);
  end;

  { TCurrentCallStackList }

  TCurrentCallStackList = class(TCallStackList)
  private
    FMonitor: TCallStackMonitor;
    FSnapShot: TCallStackList;
    procedure SetSnapShot(const AValue: TCallStackList);
  protected
    function GetEntryForThread(const AThreadId: Integer): TCallStack; override;
    property SnapShot: TCallStackList read FSnapShot write SetSnapShot;
  public
    constructor Create(AMonitor: TCallStackMonitor);
  end;

  { TCallStackMonitor }

  TCallStackMonitor = class(TDebuggerDataMonitorEx)
  private
    FCurrentCallStackList: TCurrentCallStackList;
    FNotificationList: TDebuggerChangeNotificationList;
    procedure CallStackClear(Sender: TObject);
    function GetSnapshot(AnID: Pointer): TCallStackList;
    function  GetSupplier: TCallStackSupplier;
    procedure SetSupplier(const AValue: TCallStackSupplier);
  protected
    procedure RequestCount(ACallstack: TCallStack);
    procedure RequestCurrent(ACallstack: TCallStack);
    procedure RequestEntries(ACallstack: TCallStack);
    procedure UpdateCurrentIndex;
    procedure DoNewSupplier; override;
    function  CreateSnapshot(CreateEmpty: Boolean = False): TObject; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotification(const ANotification: TCallStackNotification);
    procedure RemoveNotification(const ANotification: TCallStackNotification);
    procedure NotifyChange; // (sender)
    procedure NotifyCurrent;
    property CurrentCallStackList: TCurrentCallStackList read FCurrentCallStackList;
    property Snapshots[AnID: Pointer]: TCallStackList read GetSnapshot;
    property Supplier: TCallStackSupplier read GetSupplier write SetSupplier;
  end;

  { TCallStackSupplier }

  TCallStackSupplier = class(TDebuggerDataSupplier)
  private
    function GetCurrentCallStackList: TCurrentCallStackList;
    function GetMonitor: TCallStackMonitor;
    procedure SetMonitor(const AValue: TCallStackMonitor);
  protected
    procedure RequestCount(ACallstack: TCurrentCallStack); virtual;
    procedure RequestCurrent(ACallstack: TCurrentCallStack); virtual;
    procedure RequestEntries(ACallstack: TCurrentCallStack); virtual;
    procedure CurrentChanged;
    procedure Changed;
    procedure DoStateChange(const AOldState: TDBGState); virtual;
    procedure UpdateCurrentIndex; virtual;
  public
    property Monitor: TCallStackMonitor read GetMonitor write SetMonitor;
    property CurrentCallStackList: TCurrentCallStackList read GetCurrentCallStackList;
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

  PDisassemblerEntry = ^TDisassemblerEntry;
  TDisassemblerEntry = record
    Addr: TDbgPtr;                   // Address
    Dump: String;                    // Raw Data
    Statement: String;               // Asm
    FuncName: String;                // Function, if avail
    Offset: Integer;                 // Byte-Offest in Fonction
    SrcFileName: String;             // SrcFile if avail
    SrcFileLine: Integer;            // Line in SrcFile
    SrcStatementIndex: SmallInt;     // Index of Statement, within list of Stmnt of the same SrcLine
    SrcStatementCount: SmallInt;     // Count of Statements for this SrcLine
  end;

  { TBaseDisassembler }

  TBaseDisassembler = class(TObject)
  private
    FBaseAddr: TDbgPtr;
    FCountAfter: Integer;
    FCountBefore: Integer;
    FChangedLockCount: Integer;
    FIsChanged: Boolean;
    function GetEntryPtr(AIndex: Integer): PDisassemblerEntry;
    function IndexError(AIndex: Integer): TCallStackEntry;
    function GetEntry(AIndex: Integer): TDisassemblerEntry;
  protected
    function  InternalGetEntry(AIndex: Integer): TDisassemblerEntry; virtual;
    function  InternalGetEntryPtr(AIndex: Integer): PDisassemblerEntry; virtual;
    procedure DoChanged; virtual;
    procedure Changed;
    procedure LockChanged;
    procedure UnlockChanged;
    procedure InternalIncreaseCountBefore(ACount: Integer);
    procedure InternalIncreaseCountAfter(ACount: Integer);
    procedure SetCountBefore(ACount: Integer);
    procedure SetCountAfter(ACount: Integer);
    procedure SetBaseAddr(AnAddr: TDbgPtr);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    // Returns "True", if the range is valid, if not a ChangeNotification will be triggered later
    function PrepareRange(AnAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer): Boolean; virtual;
    property BaseAddr: TDbgPtr read FBaseAddr;
    property CountAfter: Integer read FCountAfter;
    property CountBefore: Integer read FCountBefore;
    property Entries[AIndex: Integer]: TDisassemblerEntry read GetEntry;
    property EntriesPtr[Index: Integer]: PDisassemblerEntry read GetEntryPtr;
  end;

  { TIDEDisassemblerNotification }

  TIDEDisassemblerNotification = class(TDebuggerNotification)
  private
    FOnChange: TNotifyEvent;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TDBGDisassembler = class;

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

  { TDBGDisassemblerEntryRange }

  TDBGDisassemblerEntryRange = class
  private
    FCount: Integer;
    FEntries: array of TDisassemblerEntry;
    FLastEntryEndAddr: TDBGPtr;
    FRangeEndAddr: TDBGPtr;
    FRangeStartAddr: TDBGPtr;
    function GetCapacity: Integer;
    function GetEntry(Index: Integer): TDisassemblerEntry;
    function GetEntryPtr(Index: Integer): PDisassemblerEntry;
    procedure SetCapacity(const AValue: Integer);
    procedure SetCount(const AValue: Integer);
  public
    procedure Clear;
    function Append(const AnEntryPtr: PDisassemblerEntry): Integer;
    procedure Merge(const AnotherRange: TDBGDisassemblerEntryRange);
    // Actual addresses on the ranges
    function FirstAddr: TDbgPtr;
    function LastAddr: TDbgPtr;
    function ContainsAddr(const AnAddr: TDbgPtr; IncludeNextAddr: Boolean = False): Boolean;
    function IndexOfAddr(const AnAddr: TDbgPtr): Integer;
    function IndexOfAddrWithOffs(const AnAddr: TDbgPtr): Integer;
    function IndexOfAddrWithOffs(const AnAddr: TDbgPtr; out AOffs: Integer): Integer;
    property Count: Integer read FCount write SetCount;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Entries[Index: Integer]: TDisassemblerEntry read GetEntry;
    property EntriesPtr[Index: Integer]: PDisassemblerEntry read GetEntryPtr;
    // The first address behind last entry
    property LastEntryEndAddr: TDBGPtr read FLastEntryEndAddr write FLastEntryEndAddr;
    // The addresses for which the range was requested
    // The range may bo more, than the entries, if there a gaps that cannot be retrieved.
    property RangeStartAddr: TDBGPtr read FRangeStartAddr write FRangeStartAddr;
    property RangeEndAddr: TDBGPtr read FRangeEndAddr write FRangeEndAddr;
  end;

  { TDBGDisassemblerEntryMap }

  TDBGDisassemblerEntryMapMergeEvent
    = procedure(MergeReceiver, MergeGiver: TDBGDisassemblerEntryRange) of object;

  { TDBGDisassemblerEntryMapIterator }
  TDBGDisassemblerEntryMap = class;

  TDBGDisassemblerEntryMapIterator = class(TMapIterator)
  public
    function GetRangeForAddr(AnAddr: TDbgPtr; IncludeNextAddr: Boolean = False): TDBGDisassemblerEntryRange;
    function NextRange: TDBGDisassemblerEntryRange;
    function PreviousRange: TDBGDisassemblerEntryRange;
  end;

  TDBGDisassemblerEntryMap = class(TMap)
  private
    FIterator: TDBGDisassemblerEntryMapIterator;
    FOnDelete: TNotifyEvent;
    FOnMerge: TDBGDisassemblerEntryMapMergeEvent;
    FFreeItemLock: Boolean;
  protected
    procedure ReleaseData(ADataPtr: Pointer); override;
  public
    constructor Create(AIdType: TMapIdType; ADataSize: Cardinal);
    destructor Destroy; override;
    // AddRange, may destroy the object
    procedure AddRange(const ARange: TDBGDisassemblerEntryRange);
    function GetRangeForAddr(AnAddr: TDbgPtr; IncludeNextAddr: Boolean = False): TDBGDisassemblerEntryRange;
    property OnDelete: TNotifyEvent read FOnDelete write FOnDelete;
    property OnMerge: TDBGDisassemblerEntryMapMergeEvent
             read FOnMerge write FOnMerge;
  end;

  { TDBGDisassembler }

  TDBGDisassembler = class(TBaseDisassembler)
  private
    FDebugger: TDebugger;
    FOnChange: TNotifyEvent;

    FEntryRanges: TDBGDisassemblerEntryMap;
    FCurrentRange: TDBGDisassemblerEntryRange;
    procedure EntryRangesOnDelete(Sender: TObject);
    procedure EntryRangesOnMerge(MergeReceiver, MergeGiver: TDBGDisassemblerEntryRange);
    function FindRange(AnAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer): Boolean;
  protected
    procedure DoChanged; override;
    procedure DoStateChange(const AOldState: TDBGState); virtual;
    function  InternalGetEntry(AIndex: Integer): TDisassemblerEntry; override;
    function  InternalGetEntryPtr(AIndex: Integer): PDisassemblerEntry; override;
    // PrepareEntries returns True, if it already added some entries
    function  PrepareEntries(AnAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer): boolean; virtual;
    function  HandleRangeWithInvalidAddr(ARange: TDBGDisassemblerEntryRange;AnAddr:
                 TDbgPtr; var ALinesBefore, ALinesAfter: Integer): boolean; virtual;
    property Debugger: TDebugger read FDebugger;
    property EntryRanges: TDBGDisassemblerEntryMap read FEntryRanges;
  public
    constructor Create(const ADebugger: TDebugger);
    destructor Destroy; override;
    procedure Clear; override;
    function PrepareRange(AnAddr: TDbgPtr; ALinesBefore, ALinesAfter: Integer): Boolean; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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

  { TThreadEntry }

  TThreadEntry = class(TCallStackEntry)
  private
    FThreadId: Integer;
    FThreadName: String;
    FThreadState: String;
  protected
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
                       const ALocationInfo: TDebuggerUnitInfo;
                       const ALine: Integer;
                       const AThreadId: Integer; const AThreadName: String;
                       const AThreadState: String;
                       AState: TDebuggerDataState = ddsValid); overload;
    constructor CreateCopy(const ASource: TThreadEntry);
    property ThreadId: Integer read FThreadId;
    property ThreadName: String read FThreadName;
    property ThreadState: String read FThreadState;
  end;

  { TThreads }

  TThreads = class(TObject)
  private
    FCurrentThreadId: Integer;
    FList: TList;
    function GetEntry(const AnIndex: Integer): TThreadEntry;
    procedure SetCurrentThreadId(const AValue: Integer); virtual;
  protected
    procedure Assign(AOther: TThreads);
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
    function Count: Integer; virtual;
    procedure Clear; virtual;
    procedure Add(AThread: TThreadEntry);
    property Entries[const AnIndex: Integer]: TThreadEntry read GetEntry; default;
    property CurrentThreadId: Integer read FCurrentThreadId write SetCurrentThreadId;
  end;

  { TCurrentThreads }

  TCurrentThreads = class(TThreads)
  private
    FMonitor: TThreadsMonitor;
    FDataValidity: TDebuggerDataState;
    FSnapShot: TThreads;
    procedure SetCurrentThreadId(const AValue: Integer); override;
    procedure SetSnapShot(const AValue: TThreads);
  protected
    property SnapShot: TThreads read FSnapShot write SetSnapShot;
  public
    constructor Create(AMonitor: TThreadsMonitor);
    function  Count: Integer; override;
    procedure Clear; override;
    procedure SetValidity(AValidity: TDebuggerDataState);
  end;

  { TThreadsMonitor }

  TThreadsMonitor = class(TDebuggerDataMonitorEx)
  private
    FCurrentThreads: TCurrentThreads;
    FNotificationList: TDebuggerChangeNotificationList;
    function GetSnapshot(AnID: Pointer): TThreads;
    function GetSupplier: TThreadsSupplier;
    procedure SetSupplier(const AValue: TThreadsSupplier);
  protected
    procedure DoNewSupplier; override;
    procedure Changed;
    procedure RequestData;
    function  CreateSnapshot(CreateEmpty: Boolean = False): TObject; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddNotification(const ANotification: TThreadsNotification);
    procedure RemoveNotification(const ANotification: TThreadsNotification);
    procedure ChangeCurrentThread(ANewId: Integer);
    procedure CurrentChanged;
    property  CurrentThreads: TCurrentThreads read FCurrentThreads;
    property  Snapshots[AnID: Pointer]: TThreads read GetSnapshot;
    property  Supplier: TThreadsSupplier read GetSupplier write SetSupplier;
  end;

  { TThreadsSupplier }

  TThreadsSupplier = class(TDebuggerDataSupplier)
  private
    function  GetCurrentThreads: TCurrentThreads;
    function  GetMonitor: TThreadsMonitor;
    procedure SetMonitor(const AValue: TThreadsMonitor);
  protected
    procedure ChangeCurrentThread(ANewId: Integer); virtual;
    procedure RequestMasterData; virtual;
  public
    procedure DoStateChange(const AOldState: TDBGState); virtual;
    property  CurrentThreads: TCurrentThreads read GetCurrentThreads;
    property  Monitor: TThreadsMonitor read GetMonitor write SetMonitor;
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
    FDebugger: TDebugger;
    FNotificationList: TDebuggerChangeNotificationList;
    FLocals: TLocalsMonitor;
    FWatches: TWatchesMonitor;
    FCallStack: TCallStackMonitor;
    FThreads: TThreadsMonitor;
  private
    FActive: Boolean;
    FUnitInfoProvider: TDebuggerUnitInfoProvider;
    FUpdateLock: Integer;
    FUpdateFlags: set of (ufSnapChanged, ufSnapCurrent, ufInDebuggerIdle);
    FCurrentState: TDBGState;
    FRequestsDone: TSnapshotManagerRequestedFlags;
    FCurrentSnapshot: TSnapshot; // snapshot for current pause. Not yet in list
    procedure SetActive(const AValue: Boolean);
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
    property Locals: TLocalsMonitor read FLocals write FLocals;
    property Watches: TWatchesMonitor read FWatches write FWatches;
    property CallStack: TCallStackMonitor read FCallStack write FCallStack;
    property Threads: TThreadsMonitor read FThreads write FThreads;
    property Debugger: TDebugger read FDebugger write FDebugger;
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

  { TBaseSignal }

  TBaseSignal = class(TDelayedUdateItem)
  private
    FHandledByDebugger: Boolean;
    FID: Integer;
    FName: String;
    FResumeHandled: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetHandledByDebugger(const AValue: Boolean); virtual;
    procedure SetID(const AValue: Integer); virtual;
    procedure SetName(const AValue: String); virtual;
    procedure SetResumeHandled(const AValue: Boolean); virtual;
  public
    constructor Create(ACollection: TCollection); override;
    property ID: Integer read FID write SetID;
    property Name: String read FName write SetName;
    property HandledByDebugger: Boolean read FHandledByDebugger write SetHandledByDebugger;
    property ResumeHandled: Boolean read FResumeHandled write SetResumeHandled;
  end;
  TBaseSignalClass = class of TBaseSignal;

  { TDBGSignal }

  TDBGSignal = class(TBaseSignal)
  private
    function GetDebugger: TDebugger;
  protected
    property Debugger: TDebugger read GetDebugger;
  public
  end;
  TDBGSignalClass = class of TDBGSignal;

  { TIDESignal }

  TIDESignal = class(TBaseSignal)
  private
    FMaster: TDBGSignal;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromXMLConfig(const AXMLConfig: TXMLConfig;
                                const APath: string);
    procedure SaveToXMLConfig(const AXMLConfig: TXMLConfig;
                              const APath: string);
    procedure ResetMaster;
  end;

  { TBaseSignals }
  TBaseSignals = class(TCollection)
  private
    function Add(const AName: String; AID: Integer): TBaseSignal;
    function Find(const AName: String): TBaseSignal;
  protected
  public
    constructor Create(const AItemClass: TBaseSignalClass);
    procedure Reset; virtual;
  end;

  { TDBGSignals }

  TDBGSignals = class(TBaseSignals)
  private
    FDebugger: TDebugger;  // reference to our debugger
    function GetItem(const AIndex: Integer): TDBGSignal;
    procedure SetItem(const AIndex: Integer; const AValue: TDBGSignal);
  protected
  public
    constructor Create(const ADebugger: TDebugger;
                       const ASignalClass: TDBGSignalClass);
    function Add(const AName: String; AID: Integer): TDBGSignal;
    function Find(const AName: String): TDBGSignal;
  public
    property Items[const AIndex: Integer]: TDBGSignal read GetItem
                                                      write SetItem; default;
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
    procedure LoadFromXMLConfig(const AXMLConfig: TXMLConfig;
                                const APath: string);
    procedure SaveToXMLConfig(const AXMLConfig: TXMLConfig;
                              const APath: string);
    property Items[const AIndex: Integer]: TIDESignal read GetItem
                                                      write SetItem; default;
  end;

  { TBaseException }
  TBaseException = class(TDelayedUdateItem)
  private
    FName: String;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetName(const AValue: String); virtual;
  public
    constructor Create(ACollection: TCollection); override;
    procedure LoadFromXMLConfig(const AXMLConfig: TXMLConfig;
                                const APath: string); virtual;
    procedure SaveToXMLConfig(const AXMLConfig: TXMLConfig;
                              const APath: string); virtual;
  public
    property Name: String read FName write SetName;
  end;
  TBaseExceptionClass = class of TBaseException;

  { TDBGException }
  TDBGException = class(TBaseException)
  private
  protected
  public
  end;
  TDBGExceptionClass = class of TDBGException;


  { TIDEException }
  TIDEException = class(TBaseException)
  private
    FEnabled: Boolean;
    FMaster: TDBGException;
  protected
    procedure DoChanged; override;
    procedure SetEnabled(const AValue: Boolean);
  protected
  public
    constructor Create(ACollection: TCollection); override;
    procedure LoadFromXMLConfig(const AXMLConfig: TXMLConfig;
                                const APath: string); override;
    procedure SaveToXMLConfig(const AXMLConfig: TXMLConfig;
                              const APath: string); override;
    procedure ResetMaster;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  { TBaseExceptions }
  TBaseExceptions = class(TCollection)
  private
    FIgnoreAll: Boolean;
    function Add(const AName: String): TBaseException;
    function Find(const AName: String): TBaseException;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure ClearExceptions; virtual;
    procedure SetIgnoreAll(const AValue: Boolean); virtual;
  public
    constructor Create(const AItemClass: TBaseExceptionClass);
    destructor Destroy; override;
    procedure Reset; virtual;
    property IgnoreAll: Boolean read FIgnoreAll write SetIgnoreAll;
  end;

  { TDBGExceptions }

  TDBGExceptions = class(TBaseExceptions)
  private
    FDebugger: TDebugger;  // reference to our debugger
    function GetItem(const AIndex: Integer): TDBGException;
    procedure SetItem(const AIndex: Integer; const AValue: TDBGException);
  protected
  public
    constructor Create(const ADebugger: TDebugger;
                       const AExceptionClass: TDBGExceptionClass);
    function Add(const AName: String): TDBGException;
    function Find(const AName: String): TDBGException;
  public
    property Items[const AIndex: Integer]: TDBGException read GetItem
                                                        write SetItem; default;
  end;

  { TIDEExceptions }

  TIDEExceptions = class(TBaseExceptions)
  private
    FMaster: TDBGExceptions;
    procedure SetMaster(const AValue: TDBGExceptions);
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
    property Master: TDBGExceptions read FMaster write SetMaster;
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

  { TDebugger }

  TDBGEventCategory = (
    ecBreakpoint, // Breakpoint hit
    ecProcess,    // Process start, process stop
    ecThread,     // Thread creation, destruction, start, etc.
    ecModule,     // Library load and unload
    ecOutput,     // DebugOutput calls
    ecWindows,    // Windows events
    ecDebugger);  // debugger errors and warnings
  TDBGEventCategories = set of TDBGEventCategory;

  TDBGEventType = (
    etDefault,
    // ecBreakpoint category
    etBreakpointEvaluation,
    etBreakpointHit,
    etBreakpointMessage,
    etBreakpointStackDump,
    etExceptionRaised,
    // ecModule category
    etModuleLoad,
    etModuleUnload,
    // ecOutput category
    etOutputDebugString,
    // ecProcess category
    etProcessExit,
    etProcessStart,
    // ecThread category
    etThreadExit,
    etThreadStart,
    // ecWindows category
    etWindowsMessagePosted,
    etWindowsMessageSent
  );

  TDBGEventRec = packed record
    case Boolean of
      False: (
       Category: Word;
       EventType: Word);
      True: (Ptr: Pointer);
  end;

  TDBGFeedbackType = (ftInformation, ftWarning, ftError);
  TDBGFeedbackResult = (frOk, frStop);
  TDBGFeedbackResults = set of TDBGFeedbackResult;

  TDBGEventNotify = procedure(Sender: TObject;
                              const ACategory: TDBGEventCategory;
                              const AEventType: TDBGEventType;
                              const AText: String) of object;

  TDebuggerStateChangedEvent = procedure(ADebugger: TDebugger;
                                         AOldState: TDBGState) of object;
  TDebuggerBreakPointHitEvent = procedure(ADebugger: TDebugger; ABreakPoint: TBaseBreakPoint;
                                          var ACanContinue: Boolean) of object;
  TDBGOutputEvent = procedure(Sender: TObject; const AText: String) of object;
  TDBGCurrentLineEvent = procedure(Sender: TObject;
                                   const ALocation: TDBGLocationRec) of object;
  TDBGExceptionEvent = procedure(Sender: TObject; const AExceptionType: TDBGExceptionType; 
                                 const AExceptionClass: String;
                                 const AExceptionLocation: TDBGLocationRec;
                                 const AExceptionText: String;
                                 out AContinue: Boolean) of object;

  TDBGFeedbackEvent = function(Sender: TObject; const AText, AInfo: String;
                               AType: TDBGFeedbackType; AButtons: TDBGFeedbackResults
                              ): TDBGFeedbackResult of object;


  TDebuggerNotifyReason = (dnrDestroy);

  { TDebuggerProperties }

  TDebuggerProperties = class(TPersistent)
  private
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
  end;
  TDebuggerPropertiesClass= class of TDebuggerProperties;

  TDebugger = class(TObject)
  private
    FArguments: String;
    FBreakPoints: TDBGBreakPoints;
    FDebuggerEnvironment: TStrings;
    FCurEnvironment: TStrings;
    FDisassembler: TDBGDisassembler;
    FEnvironment: TStrings;
    FErrorStateInfo: String;
    FErrorStateMessage: String;
    FExceptions: TDBGExceptions;
    FExitCode: Integer;
    FExternalDebugger: String;
    //FExceptions: TDBGExceptions;
    FFileName: String;
    FLocals: TLocalsSupplier;
    FLineInfo: TDBGLineInfo;
    FUnitInfoProvider, FInternalUnitInfoProvider: TDebuggerUnitInfoProvider;
    FOnBeforeState: TDebuggerStateChangedEvent;
    FOnConsoleOutput: TDBGOutputEvent;
    FOnFeedback: TDBGFeedbackEvent;
    FOnIdle: TNotifyEvent;
    FRegisters: TDBGRegisters;
    FShowConsole: Boolean;
    FSignals: TDBGSignals;
    FState: TDBGState;
    FCallStack: TCallStackSupplier;
    FWatches: TWatchesSupplier;
    FThreads: TThreadsSupplier;
    FOnCurrent: TDBGCurrentLineEvent;
    FOnException: TDBGExceptionEvent;
    FOnOutput: TDBGOutputEvent;
    FOnDbgOutput: TDBGOutputEvent;
    FOnDbgEvent: TDBGEventNotify;
    FOnState: TDebuggerStateChangedEvent;
    FOnBreakPointHit: TDebuggerBreakPointHitEvent;
    FWorkingDir: String;
    FDestroyNotificationList: array [TDebuggerNotifyReason] of TMethodList;
    procedure DebuggerEnvironmentChanged(Sender: TObject);
    procedure EnvironmentChanged(Sender: TObject);
    function GetUnitInfoProvider: TDebuggerUnitInfoProvider;
    function  GetState: TDBGState;
    function  ReqCmd(const ACommand: TDBGCommand;
                     const AParams: array of const): Boolean;
    procedure SetDebuggerEnvironment (const AValue: TStrings );
    procedure SetEnvironment(const AValue: TStrings);
    procedure SetFileName(const AValue: String);
  protected
    function  CreateBreakPoints: TDBGBreakPoints; virtual;
    function  CreateLocals: TLocalsSupplier; virtual;
    function  CreateLineInfo: TDBGLineInfo; virtual;
    function  CreateRegisters: TDBGRegisters; virtual;
    function  CreateCallStack: TCallStackSupplier; virtual;
    function  CreateDisassembler: TDBGDisassembler; virtual;
    function  CreateWatches: TWatchesSupplier; virtual;
    function  CreateThreads: TThreadsSupplier; virtual;
    function  CreateSignals: TDBGSignals; virtual;
    function  CreateExceptions: TDBGExceptions; virtual;
    procedure DoCurrent(const ALocation: TDBGLocationRec);
    procedure DoDbgOutput(const AText: String);
    procedure DoDbgEvent(const ACategory: TDBGEventCategory; const AEventType: TDBGEventType; const AText: String);
    procedure DoException(const AExceptionType: TDBGExceptionType;
                          const AExceptionClass: String;
                          const AExceptionLocation: TDBGLocationRec;
                          const AExceptionText: String;
                          out AContinue: Boolean);
    procedure DoOutput(const AText: String);
    procedure DoBreakpointHit(const ABreakPoint: TBaseBreakPoint; var ACanContinue: Boolean);
    procedure DoBeforeState(const OldState: TDBGState); virtual;
    procedure DoState(const OldState: TDBGState); virtual;
    function  ChangeFileName: Boolean; virtual;
    function  GetCommands: TDBGCommands;
    function  GetSupportedCommands: TDBGCommands; virtual;
    function  GetTargetWidth: Byte; virtual;
    function  GetWaiting: Boolean; virtual;
    function  GetIsIdle: Boolean; virtual;
    function  RequestCommand(const ACommand: TDBGCommand;
                             const AParams: array of const): Boolean;
                             virtual; abstract; // True if succesful
    procedure SetExitCode(const AValue: Integer);
    procedure SetState(const AValue: TDBGState);
    procedure SetErrorState(const AMsg: String; const AInfo: String = '');
    procedure DoRelease; virtual;
  public
    class function Caption: String; virtual;         // The name of the debugger as shown in the debuggeroptions
    class function ExePaths: String; virtual;        // The default locations of the exe
    class function HasExePath: boolean; virtual;        // If the debugger needs to have an exe path

    // debugger properties
    class function CreateProperties: TDebuggerProperties; virtual;         // Creates debuggerproperties
    class function GetProperties: TDebuggerProperties;                     // Get the current properties
    class procedure SetProperties(const AProperties: TDebuggerProperties); // Set the current properties

  public
    constructor Create(const AExternalDebugger: String); virtual;
    destructor Destroy; override;

    procedure Init; virtual;                         // Initializes the debugger
    procedure Done; virtual;                         // Kills the debugger
    procedure Release;                               // Free/Destroy self
    procedure Run;                                   // Starts / continues debugging
    procedure Pause;                                 // Stops running
    procedure Stop;                                  // quit debugging
    procedure StepOver;
    procedure StepInto;
    procedure StepOverInstr;
    procedure StepIntoInstr;
    procedure StepOut;
    procedure RunTo(const ASource: String; const ALine: Integer);                // Executes til a certain point
    procedure JumpTo(const ASource: String; const ALine: Integer);               // No execute, only set exec point
    procedure SendConsoleInput(AText: String);
    function  Evaluate(const AExpression: String; var AResult: String;
                       var ATypeInfo: TDBGType;
                       EvalFlags: TDBGEvaluateFlags = []): Boolean;                     // Evaluates the given expression, returns true if valid
    function  Modify(const AExpression, AValue: String): Boolean;                // Modifies the given expression, returns true if valid
    function  Disassemble(AAddr: TDbgPtr; ABackward: Boolean; out ANextAddr: TDbgPtr;
                          out ADump, AStatement, AFile: String; out ALine: Integer): Boolean; deprecated;
    function GetLocation: TDBGLocationRec; virtual;
    procedure LockCommandProcessing; virtual;
    procedure UnLockCommandProcessing; virtual;
    procedure AddNotifyEvent(AReason: TDebuggerNotifyReason; AnEvent: TNotifyEvent);
    procedure RemoveNotifyEvent(AReason: TDebuggerNotifyReason; AnEvent: TNotifyEvent);
  public
    property Arguments: String read FArguments write FArguments;                 // Arguments feed to the program
    property BreakPoints: TDBGBreakPoints read FBreakPoints;                     // list of all breakpoints
    property CallStack: TCallStackSupplier read FCallStack;
    property Disassembler: TDBGDisassembler read FDisassembler;
    property Commands: TDBGCommands read GetCommands;                            // All current available commands of the debugger
    property DebuggerEnvironment: TStrings read FDebuggerEnvironment
                                           write SetDebuggerEnvironment;         // The environment passed to the debugger process
    property Environment: TStrings read FEnvironment write SetEnvironment;       // The environment passed to the debuggee
    property Exceptions: TDBGExceptions read FExceptions;                        // A list of exceptions we should ignore
    property ExitCode: Integer read FExitCode;
    property ExternalDebugger: String read FExternalDebugger;                    // The name of the debugger executable
    property FileName: String read FFileName write SetFileName;                  // The name of the exe to be debugged
    property Locals: TLocalsSupplier read FLocals;                                    // list of all localvars etc
    property LineInfo: TDBGLineInfo read FLineInfo;                              // list of all source LineInfo
    property Registers: TDBGRegisters read FRegisters;                           // list of all registers
    property Signals: TDBGSignals read FSignals;                                 // A list of actions for signals we know
    property ShowConsole: Boolean read FShowConsole write FShowConsole;          // Indicates if the debugger should create a console for the debuggee
    property State: TDBGState read FState;                                       // The current state of the debugger
    property SupportedCommands: TDBGCommands read GetSupportedCommands;          // All available commands of the debugger
    property TargetWidth: Byte read GetTargetWidth;                              // Currently only 32 or 64
    property Waiting: Boolean read GetWaiting;                                   // Set when the debugger is wating for a command to complete
    property Watches: TWatchesSupplier read FWatches;                                 // list of all watches etc
    property Threads: TThreadsSupplier read FThreads;
    property WorkingDir: String read FWorkingDir write FWorkingDir;              // The working dir of the exe being debugged
    property IsIdle: Boolean read GetIsIdle;                                     // Nothing queued
    property ErrorStateMessage: String read FErrorStateMessage;
    property ErrorStateInfo: String read FErrorStateInfo;
    property UnitInfoProvider: TDebuggerUnitInfoProvider                        // Provided by DebugBoss, to map files to packages or project
             read GetUnitInfoProvider write FUnitInfoProvider;
    // Events
    property OnCurrent: TDBGCurrentLineEvent read FOnCurrent write FOnCurrent;   // Passes info about the current line being debugged
    property OnDbgOutput: TDBGOutputEvent read FOnDbgOutput write FOnDbgOutput;  // Passes all debuggeroutput
    property OnDbgEvent: TDBGEventNotify read FOnDbgEvent write FOnDbgEvent;     // Passes recognized debugger events, like library load or unload
    property OnException: TDBGExceptionEvent read FOnException write FOnException;  // Fires when the debugger received an exeption
    property OnOutput: TDBGOutputEvent read FOnOutput write FOnOutput;           // Passes all output of the debugged target
    property OnBeforeState: TDebuggerStateChangedEvent read FOnBeforeState write FOnBeforeState;   // Fires when the current state of the debugger changes
    property OnState: TDebuggerStateChangedEvent read FOnState write FOnState;   // Fires when the current state of the debugger changes
    property OnBreakPointHit: TDebuggerBreakPointHitEvent read FOnBreakPointHit write FOnBreakPointHit;   // Fires when the program is paused at a breakpoint
    property OnConsoleOutput: TDBGOutputEvent read FOnConsoleOutput write FOnConsoleOutput;  // Passes Application Console Output
    property OnFeedback: TDBGFeedbackEvent read FOnFeedback write FOnFeedback;
    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;                    // Called if all outstanding requests are processed (queue empty)
  end;
  TDebuggerClass = class of TDebugger;

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
function DBGStateNameToState(const s: string): TDBGState;
function DBGBreakPointActionNameToAction(const s: string): TIDEBreakPointAction;

function dbgs(AState: TDBGState): String; overload;
function dbgs(ADisassRange: TDBGDisassemblerEntryRange): String; overload;
function dbgs(ADataState: TDebuggerDataState): String; overload;

function HasConsoleSupport: Boolean;
(******************************************************************************)
(******************************************************************************)
(******************************************************************************)
(******************************************************************************)

implementation

const
  COMMANDMAP: array[TDBGState] of TDBGCommands = (
  {dsNone } [],
  {dsIdle } [dcEnvironment],
  {dsStop } [dcRun, dcStepOver, dcStepInto, dcStepOverInstr, dcStepIntoInstr,
             dcStepOut, dcRunTo, dcJumpto, dcBreak, dcWatch, dcEvaluate, dcEnvironment,
             dcSendConsoleInput],
  {dsPause} [dcRun, dcStop, dcStepOver, dcStepInto, dcStepOverInstr, dcStepIntoInstr,
             dcStepOut, dcRunTo, dcJumpto, dcBreak, dcWatch, dcLocal, dcEvaluate, dcModify,
             dcEnvironment, dcSetStackFrame, dcDisassemble, dcSendConsoleInput],
  {dsInternalPause} // same as run, so not really used
            [dcStop, dcBreak, dcWatch, dcEnvironment, dcSendConsoleInput],
  {dsInit } [],
  {dsRun  } [dcPause, dcStop, dcBreak, dcWatch, dcEnvironment, dcSendConsoleInput],
  {dsError} [dcStop],
  {dsDestroying} []
  );

var
  MDebuggerPropertiesList: TStringlist;

function dbgs(AState: TDBGState): String; overload;
begin
  Result := DBGStateNames[AState];
end;

function dbgs(ADataState: TDebuggerDataState): String;
begin
  writestr(Result, ADataState);
end;

function dbgs(ADisassRange: TDBGDisassemblerEntryRange): String; overload;
var
  fo: Integer;
begin
  if (ADisassRange = nil)
  then begin
    Result := 'Range(nil)'
  end
  else begin
    if (ADisassRange.Count > 0)
    then fo := ADisassRange.EntriesPtr[0]^.Offset
    else fo := 0;
    with ADisassRange do
      Result := Format('Range(%u)=[[ Cnt=%d, Capac=%d, [0].Addr=%u, RFirst=%u, [Cnt].Addr=%u, RLast=%u, REnd=%u, FirstOfs=%d ]]',
        [PtrUInt(ADisassRange), Count, Capacity, FirstAddr, RangeStartAddr, LastAddr, RangeEndAddr, LastEntryEndAddr, fo]);
  end;
end;

function HasConsoleSupport: Boolean;
begin
  {$IFDEF DBG_ENABLE_TERMINAL}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

procedure ReleaseRefAndNil(var ARefCountedObject);
begin
  Assert((Pointer(ARefCountedObject) = nil) or (TObject(ARefCountedObject) is TRefCountedObject), 'ReleaseRefAndNil requires TRefCountedObject');
  if Pointer(ARefCountedObject) <> nil
  then TRefCountedObject(ARefCountedObject).ReleaseReference;
  Pointer(ARefCountedObject) := nil;
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

constructor TIDEBreakPointGroupList.Create(AOwner: TBaseBreakPoint);
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

{ TFreeNotifyingObject }

constructor TFreeNotifyingObject.Create;
begin
  FFreeNotificationList := TMethodList.Create;
  inherited Create;
end;

destructor TFreeNotifyingObject.Destroy;
begin
  FFreeNotificationList.CallNotifyEvents(Self);
  inherited Destroy;
  FreeAndNil(FFreeNotificationList);
end;

procedure TFreeNotifyingObject.AddFreeeNotification(ANotification: TNotifyEvent);
begin
  FFreeNotificationList.Add(TMethod(ANotification));
end;

procedure TFreeNotifyingObject.RemoveFreeeNotification(ANotification: TNotifyEvent);
begin
  FFreeNotificationList.Remove(TMethod(ANotification));
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

procedure TDebuggerConfigStore.Init;
begin
  inherited Init;
  {$IFdef MSWindows}
  WarnedAboutBreakGroup := False;
  {$ENDIF}
end;

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
    if FList[i].IsEqual(AFileName, AFullFileName) then begin
      {$IFDEF DBG_LOCATION_INFO}
      debugln(['TDebuggerLocationProvider.GetLocationInfoFor  Found entry for: ', AFileName, ' / ', AFullFileName]);
      {$ENDIF}
      exit(FList[i])
    end;
    dec(i);
  end;
  Result := TDebuggerUnitInfo.Create(AFileName, AFullFileName);
  FList.Add(Result);
  {$IFDEF DBG_LOCATION_INFO}
  debugln(['TDebuggerLocationProvider.GetLocationInfoFor  Created new entry (Cnt=',FList.Count,') for: ', AFileName, ' / ', AFullFileName]);
  {$ENDIF}
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

function TDebuggerUnitInfo.IsEqual(const AFileName: String;
  const AFullFileName: String): boolean;
begin
  Result := (FFileName = AFileName) and
            (FDbgFullName = AFullFileName);
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

  FFileName          := AConfig.GetValue(APath + 'File', '');
  FLocationOwnerName := AConfig.GetValue(APath + 'UnitOwner', '');
  FLocationName      := AConfig.GetValue(APath + 'UnitFile',  '');
  FDbgFullName       := AConfig.GetValue(APath + 'DbgFile',  '');
  FLocationFullFile := '';
end;

procedure TDebuggerUnitInfo.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  const APath: string);
var
  s: String;
begin
  WriteStr(s, LocationType);
  AConfig.SetValue(APath + 'Type', s);
  AConfig.SetValue(APath + 'File', FileName);

  AConfig.SetValue(APath + 'UnitOwner', LocationOwnerName);
  AConfig.SetValue(APath + 'UnitFile',  LocationName);
  AConfig.SetValue(APath + 'DbgFile',   FDbgFullName);
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

{ TRefCntObjList }

procedure TRefCntObjList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  case Action of
    lnAdded:   TRefCountedObject(Ptr).AddReference;
    lnExtracted,
    lnDeleted: TRefCountedObject(Ptr).ReleaseReference;
  end;
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

procedure TSnapshotManager.SetHistoryindex(const AValue: Integer);
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
  FThreads.RemoveSnapshot(AnEntry);
  FCallStack.RemoveSnapshot(AnEntry);
  FLocals.RemoveSnapshot(AnEntry);
  FWatches.RemoveSnapshot(AnEntry);
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
  FNotificationList.Clear;
  ReleaseRefAndNil(FCurrentSnapshot);
  Clear;
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
  {$IFDEF DBG_DATA_MONITORS} DebugLnEnter(['DebugDataMonitor: >>ENTER: TSnapshotManager.DoStateChange  New-State=', DBGStateNames[FCurrentState]]); {$ENDIF}

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
  {$IFDEF DBG_DATA_MONITORS} DebugLnExit(['DebugDataMonitor: <<EXIT: TSnapshotManager.DoStateChange']); {$ENDIF}
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
  {$IFDEF DBG_DATA_MONITORS} DebugLnEnter(['DebugDataMonitor: >>ENTER: TSnapshotManager.DoDebuggerIdle  New-State=', DBGStateNames[FCurrentState]]); {$ENDIF}
  try

    if not(smrThreads in FRequestsDone) then begin
      include(FRequestsDone, smrThreads);
      FThreads.CurrentThreads.Count;
      if (not Debugger.IsIdle) and (not AForce) then exit;
      if CurSnap <> FCurrentSnapshot then exit; // Debugger did "run" in between
    end;
    if not(smrCallStackCnt in FRequestsDone) then begin
      include(FRequestsDone, smrCallStackCnt);
      i := FThreads.CurrentThreads.CurrentThreadId;
      FCallStack.CurrentCallStackList.EntriesForThreads[i].Count;
      if (not Debugger.IsIdle) and (not AForce) then exit;
      if CurSnap <> FCurrentSnapshot then exit; // Debugger did "run" in between
    end;
    if not(smrCallStack in FRequestsDone) then begin
      include(FRequestsDone, smrCallStack);
      i := FThreads.CurrentThreads.CurrentThreadId;
      k := FCallStack.CurrentCallStackList.EntriesForThreads[i].Count;
      if k > 0
      then FCallStack.CurrentCallStackList.EntriesForThreads[i].PrepareRange(0, Min(5, k));
      if (not Debugger.IsIdle) and (not AForce) then exit;
      if CurSnap <> FCurrentSnapshot then exit; // Debugger did "run" in between
    end;
    if not(smrLocals in FRequestsDone) then begin
      include(FRequestsDone, smrLocals);
      i := FThreads.CurrentThreads.CurrentThreadId;
      j := FCallStack.CurrentCallStackList.EntriesForThreads[i].CurrentIndex;
      FLocals.CurrentLocalsList.Entries[i, j].Count;
      if (not Debugger.IsIdle) and (not AForce) then exit;
      if CurSnap <> FCurrentSnapshot then exit; // Debugger did "run" in between
    end;
    if not(smrWatches in FRequestsDone) then begin
      include(FRequestsDone, smrWatches);
      i := FThreads.CurrentThreads.CurrentThreadId;
      j := FCallStack.CurrentCallStackList.EntriesForThreads[i].CurrentIndex;
      w := FWatches.CurrentWatches;
      for k := 0 to w.Count - 1 do w[k].Values[i, j].Value;
      if (not Debugger.IsIdle) and (not AForce) then exit;
      if CurSnap <> FCurrentSnapshot then exit; // Debugger did "run" in between
    end;
  finally
    Exclude(FUpdateFlags, ufInDebuggerIdle);
    {$IFDEF DBG_DATA_MONITORS} DebugLnExit(['DebugDataMonitor: <<EXIT: TSnapshotManager.DoDebuggerIdle']); {$ENDIF}
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

{ TDebuggerDataMonitorEx }

function TDebuggerDataMonitorEx.CreateSnapshot(CreateEmpty: Boolean = False): TObject;
begin
  Result := nil;
end;

function TDebuggerDataMonitorEx.GetSnapshotObj(AnID: Pointer): TObject;
begin
  Result := FSnapshots.SnapShot[AnID];
end;

constructor TDebuggerDataMonitorEx.Create;
begin
  FSnapshots := TDebuggerDataSnapShotList.Create;
  inherited Create;
end;

destructor TDebuggerDataMonitorEx.Destroy;
begin
  FSnapshots.Clear;
  inherited Destroy;
  FreeAndNil(FSnapshots);
end;

procedure TDebuggerDataMonitorEx.NewSnapshot(AnID: Pointer; CreateEmpty: Boolean = False);
var
  S: TObject;
begin
  S := CreateSnapshot(CreateEmpty);
  FSnapshots.AddSnapShot(AnID, S);
end;

procedure TDebuggerDataMonitorEx.RemoveSnapshot(AnID: Pointer);
begin
  FSnapshots.RemoveSnapShot(AnID);
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

procedure TCurrentLocalsList.SetSnapShot(const AValue: TLocalsList);
var
  i: Integer;
  R: TLocals;
begin
  assert((FSnapShot=nil) or (AValue=nil), 'TCurrentLocalsList already have snapshot');
  if FSnapShot = AValue then exit;
  FSnapShot := AValue;

  if FSnapShot = nil then begin
    for i := 0 to Count-1 do
      TCurrentLocals(EntriesByIdx[i]).SnapShot := nil;
  end else begin
    //FSnapShot.Assign(Self);
    FSnapShot.Clear;
    for i := 0 to Count-1 do begin
      R := TLocals.Create;
      FSnapShot.Add(R);
      TCurrentLocals(EntriesByIdx[i]).SnapShot := R;
    end;

  end;
end;

function TCurrentLocalsList.CreateEntry(const AThreadId: Integer;
  const AStackFrame: Integer): TLocals;
var
  R: TLocals;
begin
  Result := TCurrentLocals.Create(FMonitor, AThreadId, AStackFrame);
  Add(Result);
  if FSnapShot <> nil
  then begin
    R := TLocals.Create(AThreadId, AStackFrame);
    FSnapShot.Add(R);
    TCurrentLocals(Result).SnapShot := R;
  end;
end;

constructor TCurrentLocalsList.Create(AMonitor: TLocalsMonitor);
begin
  FMonitor := AMonitor;
  inherited Create;
end;

{ TLocalsList }

function TLocalsList.GetEntry(const AThreadId: Integer; const AStackFrame: Integer): TLocals;
var
  i: Integer;
begin
  i := FList.Count - 1;
  while i >= 0 do begin
    Result := TLocals(FList[i]);
    if (Result.ThreadId = AThreadId) and (Result.StackFrame = AStackFrame)
    then exit;
    dec(i);
  end;
  Result := CreateEntry(AThreadId, AStackFrame);
end;

function TLocalsList.GetEntryByIdx(const AnIndex: Integer): TLocals;
begin
  Result := TLocals(FList[AnIndex]);
end;

function TLocalsList.CreateEntry(const AThreadId: Integer;
  const AStackFrame: Integer): TLocals;
begin
  Result := nil;
end;

procedure TLocalsList.Add(AnEntry: TLocals);
begin
  assert(((Self is TCurrentLocalsList) and (AnEntry is TCurrentLocals)) or ((not(Self is TCurrentLocalsList)) and not(AnEntry is TCurrentLocals)),
         'TLocalsList.Add: entry and list differ (current and none current)');
  FList.add(AnEntry);
end;

procedure TLocalsList.LoadDataFromXMLConfig(const AConfig: TXMLConfig; APath: string);
var
  e: TLocals;
  c, i: Integer;
begin
  Clear;
  c := AConfig.GetValue(APath + 'Count', 0);
  APath := APath + 'LocalsEntry';
  for i := 0 to c - 1 do begin
    e := TLocals.Create;
    e.LoadDataFromXMLConfig(AConfig, APath + IntToStr(i) + '/');
    Add(e);
  end;
end;

procedure TLocalsList.SaveDataToXMLConfig(const AConfig: TXMLConfig; APath: string);
var
  i: Integer;
begin
  AConfig.SetDeleteValue(APath + 'Count', Count, 0);
  APath := APath + 'LocalsEntry';
  for i := 0 to Count - 1 do
    EntriesByIdx[i].SaveDataToXMLConfig(AConfig, APath + IntToStr(i) + '/');
end;

procedure TLocalsList.Assign(AnOther: TLocalsList);
var
  i: Integer;
begin
  Clear;
  for i := 0 to AnOther.FList.Count-1 do
    FList.Add(TLocals.CreateCopy(TLocals(AnOther.FList[i])));
end;

constructor TLocalsList.Create;
begin
  FList := TList.Create;
  inherited Create;
end;

destructor TLocalsList.Destroy;
begin
  Clear;
  inherited Destroy;
  FList.Free;
end;

procedure TLocalsList.Clear;
begin
  while FList.Count > 0 do begin
    TRefCountedObject(FList[0]).ReleaseReference;
    FList.Delete(0);
  end;
end;

function TLocalsList.Count: Integer;
begin
  Result := FList.Count;
end;

{ TLocalsSupplier }

function TLocalsSupplier.GetCurrentLocalsList: TCurrentLocalsList;
begin
  if Monitor <> nil
  then Result := Monitor.CurrentLocalsList
  else Result := nil;
end;

function TLocalsSupplier.GetMonitor: TLocalsMonitor;
begin
  Result := TLocalsMonitor(inherited Monitor);
end;

procedure TLocalsSupplier.SetMonitor(const AValue: TLocalsMonitor);
begin
  inherited Monitor := AValue;
end;

procedure TLocalsSupplier.RequestData(ALocals: TCurrentLocals);
begin
  ALocals.SetDataValidity(ddsInvalid)
end;

procedure TLocalsSupplier.DoStateChange(const AOldState: TDBGState);
begin
  if (Debugger = nil) or (CurrentLocalsList = nil) then Exit;
  {$IFDEF DBG_DATA_MONITORS} DebugLnEnter(['DebugDataMonitor: >>ENTER: TLocalsSupplier.DoStateChange  New-State=', DBGStateNames[Debugger.State ]]); {$ENDIF}

  if FDebugger.State in [dsPause, dsInternalPause]
  then begin
    if Monitor<> nil
    then Monitor.Clear;
  end
  else begin
    CurrentLocalsList.SnapShot := nil;

    if (AOldState in [dsPause, dsInternalPause]) or (AOldState = dsNone) { Force clear on initialisation }
    then begin
      if Monitor<> nil
      then Monitor.Clear;
    end;
  end;
  {$IFDEF DBG_DATA_MONITORS} DebugLnExit(['DebugDataMonitor: <<EXIT: TLocalsSupplier.DoStateChange']); {$ENDIF}
end;

{ TLocalsMonitor }

function TLocalsMonitor.GetSupplier: TLocalsSupplier;
begin
  Result := TLocalsSupplier(inherited Supplier);
end;

function TLocalsMonitor.GetSnapshot(AnID: Pointer): TLocalsList;
begin
  Result := TLocalsList(GetSnapshotObj(AnID));
end;

procedure TLocalsMonitor.SetSupplier(const AValue: TLocalsSupplier);
begin
  inherited Supplier := AValue;
end;

procedure TLocalsMonitor.NotifyChange(ALocals: TCurrentLocals);
begin
  FNotificationList.NotifyChange(ALocals);
end;

procedure TLocalsMonitor.DoNewSupplier;
begin
  inherited DoNewSupplier;
  NotifyChange(nil);
end;

procedure TLocalsMonitor.RequestData(ALocals: TCurrentLocals);
begin
  if Supplier <> nil
  then Supplier.RequestData(ALocals)
  else ALocals.SetDataValidity(ddsInvalid);
end;

function TLocalsMonitor.CreateSnapshot(CreateEmpty: Boolean = False): TObject;
begin
  Result := TLocalsList.Create;
  if not CreateEmpty
  then CurrentLocalsList.SnapShot := TLocalsList(Result);
end;

constructor TLocalsMonitor.Create;
begin
  inherited;
  FNotificationList := TDebuggerChangeNotificationList.Create;
  FCurrentLocalsList := TCurrentLocalsList.Create(Self);
end;

destructor TLocalsMonitor.Destroy;
begin
  FNotificationList.Clear;
  inherited Destroy;
  FreeAndNil(FCurrentLocalsList);
  FreeAndNil(FNotificationList);
end;

procedure TLocalsMonitor.Clear;
begin
  FCurrentLocalsList.Clear;
  NotifyChange(nil);
end;

procedure TLocalsMonitor.AddNotification(const ANotification: TLocalsNotification);
begin
  FNotificationList.Add(ANotification);
end;

procedure TLocalsMonitor.RemoveNotification(const ANotification: TLocalsNotification);
begin
  FNotificationList.Remove(ANotification);
end;

{ TCurrentWatchValue }

procedure TCurrentWatchValue.SetTypeInfo(const AValue: TDBGType);
begin
  FreeAndNil(FTypeInfo);
  FTypeInfo := AValue;
end;

procedure TCurrentWatchValue.SetValue(const AValue: String);
begin
  FValue := AValue;
end;

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

procedure TCurrentWatchValue.ValidityChanged;
begin
  inherited;
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
  const AStackFrame: Integer; const ADisplayFormat: TWatchDisplayFormat): TWatchValue;
var
  R: TWatchValue;
begin
  {$IFDEF DBG_DATA_MONITORS} try DebugLnEnter(['DebugDataMonitor: >>ENTER: TCurrentWatchValueList.CreateEntry  AThreadId=', AThreadId, '  AStackFrame=',AStackFrame, ' Expr=', FWatch.Expression]); {$ENDIF}
  Result := TCurrentWatchValue.Create(FWatch, AThreadId, AStackFrame, ADisplayFormat);
  Add(Result);
  if FSnapShot <> nil then begin
    R := TWatchValue.Create(FSnapShot.FWatch);
    FSnapShot.Add(R);
    TCurrentWatchValue(Result).SnapShot := R;
  end;
  {$IFDEF DBG_DATA_MONITORS} finally DebugLnExit(['DebugDataMonitor: <<EXIT: TCurrentWatchValueList.CreateEntry']); end; {$ENDIF}
end;

{ TWatchValueList }

function TWatchValueList.GetEntry(const AThreadId: Integer; const AStackFrame: Integer;
                     const ADisplayFormat: TWatchDisplayFormat): TWatchValue;
var
  i: Integer;
begin
  i := FList.Count - 1;
  while i >= 0 do begin
    Result := TWatchValue(FList[i]);
    if (Result.ThreadId = AThreadId) and (Result.StackFrame = AStackFrame) and
       (Result.DisplayFormat = ADisplayFormat)
    then
      exit;
    dec(i);
  end;
  Result := CreateEntry(AThreadId, AStackFrame, ADisplayFormat);
end;

function TWatchValueList.GetEntryByIdx(AnIndex: integer): TWatchValue;
begin
  Result := TWatchValue(FList[AnIndex]);
end;

function TWatchValueList.CreateEntry(const AThreadId: Integer; const AStackFrame: Integer;
  const ADisplayFormat: TWatchDisplayFormat): TWatchValue;
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
begin
  if not FWatch.Enabled then begin
    Result := '<disabled>';
    exit;
  end;
  case FValidity of
    ddsUnknown:   begin
        Result := '<evaluating>';
        FValidity := ddsRequested;
        RequestData;
        if FValidity in [ddsValid, ddsInvalid, ddsError]
        then Result := GetValue();
      end;
    ddsRequested, ddsEvaluating: Result := '<evaluating>';
    ddsValid:                    Result := FValue;
    ddsInvalid:                  Result := '<invalid>';
    ddsError:                    Result := '<Error: '+FValue+'>';
  end;

end;

function TWatchValue.GetTypeInfo: TDBGType;
begin
  if not FWatch.Enabled then begin
    Result := nil;
    exit;
  end;
  case FValidity of
    ddsUnknown: begin
      Result := nil;
      FValidity := ddsRequested;
      RequestData;
      if FValidity in [ddsValid, ddsInvalid, ddsError]
      then Result := GetTypeInfo();
    end;
    ddsRequested,
    ddsEvaluating: Result := nil;
    ddsValid:      Result := FTypeInfo;
    ddsInvalid,
    ddsError:      Result := nil;
  end;
end;

procedure TWatchValue.SetValidity(const AValue: TDebuggerDataState);
begin
  if FValidity = AValue then exit;
  {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataMonitor: TWatchValue.SetValidity: FThreadId=', FThreadId, '  FStackFrame=',FStackFrame, ' Expr=', FWatch.Expression, ' AValidity=',dbgs(AValue)]); {$ENDIF}
  FValidity := AValue;
  ValidityChanged;
end;

procedure TWatchValue.RequestData;
begin
  FValidity := ddsInvalid;
end;

procedure TWatchValue.ValidityChanged;
begin
  //
end;

procedure TWatchValue.LoadDataFromXMLConfig(const AConfig: TXMLConfig;
  const APath: string);
begin
  FThreadId   := AConfig.GetValue(APath + 'ThreadId', -1);
  FStackFrame := AConfig.GetValue(APath + 'StackFrame', -1);
  FValue      := AConfig.GetValue(APath + 'Value', '');
  try    ReadStr(AConfig.GetValue(APath + 'DisplayFormat', 'wdfDefault'), FDisplayFormat);
  except FDisplayFormat := wdfDefault; end;
  try    ReadStr(AConfig.GetValue(APath + 'Validity', 'ddsValid'), FValidity);
  except FValidity := ddsUnknown; end;
end;

procedure TWatchValue.SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string);
var
  s: String;
begin
  AConfig.SetValue(APath + 'ThreadId', FThreadId);
  AConfig.SetValue(APath + 'StackFrame', FStackFrame);
  AConfig.SetValue(APath + 'Value', FValue);
  WriteStr(s, FDisplayFormat);
  AConfig.SetDeleteValue(APath + 'DisplayFormat', s, 'wdfDefault');
  WriteStr(s, FValidity);
  AConfig.SetDeleteValue(APath + 'Validity', s, 'ddsValid');
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
  FValidity := ddsUnknown;
  FWatch := AOwnerWatch;
  Create;
end;

constructor TWatchValue.Create(AOwnerWatch: TWatch; const AThreadId: Integer;
  const AStackFrame: Integer; const ADisplayFormat: TWatchDisplayFormat);
begin
  Create(AOwnerWatch);
  FThreadId := AThreadId;
  FStackFrame := AStackFrame;
  FDisplayFormat := ADisplayFormat;
end;

destructor TWatchValue.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FTypeInfo);
end;

procedure TWatchValue.Assign(AnOther: TWatchValue);
begin
  FreeAndNil(FTypeInfo);
  FValue := AnOther.FValue;
  FValidity := AnOther.FValidity;
  //FTypeInfo := AnOther.FTypeInfo.cre;
  FThreadId := AnOther.FThreadId;
  FStackFrame := AnOther.FStackFrame;
  FDisplayFormat := AnOther.FDisplayFormat;
end;

{ TWatchesSupplier }

function TWatchesSupplier.GetCurrentWatches: TCurrentWatches;
begin
  if Monitor <> nil
  then Result := Monitor.CurrentWatches
  else Result := nil;
end;

function TWatchesSupplier.GetMonitor: TWatchesMonitor;
begin
  Result := TWatchesMonitor(inherited Monitor);
end;

procedure TWatchesSupplier.SetMonitor(const AValue: TWatchesMonitor);
begin
  inherited Monitor := AValue;
end;

procedure TWatchesSupplier.RequestData(AWatchValue: TCurrentWatchValue);
begin
  if FNotifiedState  in [dsPause, dsInternalPause]
  then InternalRequestData(AWatchValue)
  else AWatchValue.SetValidity(ddsInvalid);
end;

procedure TWatchesSupplier.InternalRequestData(AWatchValue: TCurrentWatchValue);
begin
  AWatchValue.SetValidity(ddsInvalid);
end;

procedure TWatchesSupplier.DoStateChange(const AOldState: TDBGState);
begin
  if (Debugger = nil) or (CurrentWatches = nil) then Exit;
  FNotifiedState := Debugger.State;
  {$IFDEF DBG_DATA_MONITORS} DebugLnEnter(['DebugDataMonitor: >>ENTER: TWatchesSupplier.DoStateChange  New-State=', DBGStateNames[FNotifiedState]]); {$ENDIF}

  if FDebugger.State  in [dsPause, dsInternalPause]
  then begin
    CurrentWatches.ClearValues;
    Monitor.NotifyUpdate(CurrentWatches, nil);
  end
  else begin
    CurrentWatches.SnapShot := nil;

    if (AOldState  in [dsPause, dsInternalPause]) or (AOldState = dsNone) { Force clear on initialisation }
    then begin
      CurrentWatches.ClearValues;  // TODO: block the update calls, update will be done for all on next line
      Monitor.NotifyUpdate(CurrentWatches, nil);
    end;
  end;
  {$IFDEF DBG_DATA_MONITORS} DebugLnExit(['DebugDataMonitor: <<EXIT: TWatchesSupplier.DoStateChange']); {$ENDIF}
end;

constructor TWatchesSupplier.Create(const ADebugger: TDebugger);
begin
  inherited Create(ADebugger);
  FNotifiedState := dsNone;
end;

{ TWatchesMonitor }

function TWatchesMonitor.GetSupplier: TWatchesSupplier;
begin
  Result := TWatchesSupplier(inherited Supplier);
end;

function TWatchesMonitor.GetSnapshot(AnID: Pointer): TWatches;
begin
  Result := TWatches(GetSnapshotObj(AnID));
end;

procedure TWatchesMonitor.SetSupplier(const AValue: TWatchesSupplier);
begin
  inherited Supplier := AValue;
end;

procedure TWatchesMonitor.NotifyAdd(const AWatches: TCurrentWatches; const AWatch: TCurrentWatch);
begin
  FNotificationList.NotifyAdd(AWatches, AWatch);
end;

procedure TWatchesMonitor.NotifyRemove(const AWatches: TCurrentWatches; const AWatch: TCurrentWatch);
begin
  FNotificationList.NotifyRemove(AWatches, AWatch);
end;

procedure TWatchesMonitor.NotifyUpdate(const AWatches: TCurrentWatches; const AWatch: TCurrentWatch);
begin
  FNotificationList.NotifyUpdate(AWatches, AWatch);
end;

procedure TWatchesMonitor.RequestData(AWatchValue: TCurrentWatchValue);
begin
  if Supplier <> nil
  then Supplier.RequestData(AWatchValue)
  else AWatchValue.SetValidity(ddsInvalid);
end;

function TWatchesMonitor.CreateSnapshot(CreateEmpty: Boolean = False): TObject;
begin
  Result := TWatches.Create;
  if not CreateEmpty
  then CurrentWatches.SnapShot := TWatches(Result);
end;

constructor TWatchesMonitor.Create;
begin
  FNotificationList := TWatchesNotificationList.Create;
  FCurrentWatches := TCurrentWatches.Create(Self);
  inherited;
end;

destructor TWatchesMonitor.Destroy;
begin
  FNotificationList.Clear;
  inherited Destroy;
  FreeAndNil(FCurrentWatches);
  FreeAndNil(FNotificationList);
end;

procedure TWatchesMonitor.AddNotification(const ANotification: TWatchesNotification);
begin
  FNotificationList.Add(ANotification);
end;

procedure TWatchesMonitor.RemoveNotification(const ANotification: TWatchesNotification);
begin
  FNotificationList.Remove(ANotification);
end;

procedure TWatchesMonitor.Clear;
begin
  FCurrentWatches.Clear;
end;

procedure TWatchesMonitor.LoadFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
begin
  FCurrentWatches.LoadFromXMLConfig(AConfig, APath);
end;

procedure TWatchesMonitor.SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string);
begin
  FCurrentWatches.SaveToXMLConfig(AConfig, APath);
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
end;

constructor TCurrentCallStack.Create(AMonitor: TCallStackMonitor);
begin
  FCount := 0;
  FEntries:= TMap.Create(its4, SizeOf(TCallStackEntry));
  FMonitor := AMonitor;
  FPreparing := False;
  FCountValidity := ddsUnknown;
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
  FCount := AnOther.Count;
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
  FMonitor.NotifyChange;
end;

function TCurrentCallStack.GetEntry(AIndex: Integer): TCallStackEntry;
begin
  if (AIndex < 0)
  or (AIndex >= Count) then IndexError(Aindex);

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
    Entries[AIndex];
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

procedure TCurrentCallStack.SetCountValidity(AValidity: TDebuggerDataState);
begin
  if FCountValidity = AValidity then exit;
  {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataMonitor: TCurrentCallStack.SetCountValidity: FThreadId=', FThreadId, ' AValidity=',dbgs(AValidity)]); {$ENDIF}
  FCountValidity := AValidity;
  FMonitor.NotifyChange;
end;

procedure TCurrentCallStack.SetCurrentValidity(AValidity: TDebuggerDataState);
begin
  if FCurrentValidity = AValidity then exit;
  {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataMonitor: TCurrentCallStack.SetCurrentValidity: FThreadId=', FThreadId, ' AValidity=',dbgs(AValidity)]); {$ENDIF}
  FCurrentValidity := AValidity;
  FMonitor.NotifyCurrent;
end;

{ TCurrentCallStackList }

constructor TCurrentCallStackList.Create(AMonitor: TCallStackMonitor);
begin
  FMonitor := AMonitor;
  inherited Create;
end;

procedure TCurrentCallStackList.SetSnapShot(const AValue: TCallStackList);
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
    {$IFDEF DBG_DATA_MONITORS} try DebugLnEnter(['DebugDataMonitor: >>ENTER: TCurrentCallStackList.GetEntryForThread: ThreadId=', AThreadId]); {$ENDIF}
    Result := TCurrentCallStack.Create(FMonitor);
    Result.ThreadId := AThreadId;
    Add(Result);
    if FSnapShot <> nil then begin
      R := TCallStack.Create;
      R.ThreadId := AThreadId;
      FSnapShot.Add(R);
      TCurrentCallStack(Result).SnapShot := R;
    end;
    {$IFDEF DBG_DATA_MONITORS} finally DebugLnExit(['DebugDataMonitor: <<EXIT: TCurrentCallStackList.GetEntryForThread' ]) end; {$ENDIF}
  end;
end;

{ TCallStackList }

function TCallStackList.GetEntry(const AIndex: Integer): TCallStack;
begin
  Result := TCallStack(FList[AIndex]);
end;

function TCallStackList.GetEntryForThread(const AThreadId: Integer): TCallStack;
var
  i: Integer;
begin
  i := Count - 1;
  while (i >= 0) and (TCallStack(FList[i]).ThreadId <> AThreadId) do dec(i);
  if i >= 0
  then Result := TCallStack(FList[i])
  else Result := nil;
end;

procedure TCallStackList.Add(ACallStack: TCallStack);
begin
  assert(((Self is TCurrentCallStackList) and (ACallStack is TCurrentCallStack)) or ((not(Self is TCurrentCallStackList)) and not(ACallStack is TCurrentCallStack)),
         'TCallStackList.Add: entry and list differ (current and none current)');
  FList.Add(ACallStack);
end;

procedure TCallStackList.LoadDataFromXMLConfig(const AConfig: TXMLConfig;
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

procedure TCallStackList.SaveDataToXMLConfig(const AConfig: TXMLConfig; APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
var
  i: Integer;
begin
  AConfig.SetDeleteValue(APath + 'Count', Count, 0);
  APath := APath + 'Entry';
  for i := 0 to Count - 1 do
    Entries[i].SaveDataToXMLConfig(AConfig, APath + IntToStr(i) + '/', AUnitInvoPrv);
end;

procedure TCallStackList.Assign(AnOther: TCallStackList);
var
  i: Integer;
begin
  Clear;
  for i := 0 to AnOther.FList.Count-1 do
    FList.Add(TCallStack.CreateCopy(TCallStack(AnOther.FList[i])));
end;

constructor TCallStackList.Create;
begin
  FList := TList.Create;
end;

destructor TCallStackList.Destroy;
begin
  inherited Destroy;
  Clear;
  FreeAndNil(FList);
end;

procedure TCallStackList.Clear;
begin
  while FList.Count > 0 do begin
    TObject(FList[0]).Free;
    FList.Delete(0);
  end;
end;

function TCallStackList.Count: Integer;
begin
  Result := FList.Count;
end;

{ TDebuggerDataSupplier }

procedure TDebuggerDataSupplier.SetMonitor(const AValue: TDebuggerDataMonitor);
begin
  if FMonitor = AValue then exit;
  Assert((FMonitor=nil) or (AValue=nil), 'TDebuggerDataSupplier.Monitor already set');
  FMonitor := AValue;
end;

constructor TDebuggerDataSupplier.Create(const ADebugger: TDebugger);
begin
  FDebugger := ADebugger;
  inherited Create;
end;

destructor TDebuggerDataSupplier.Destroy;
begin
  if FMonitor <> nil then FMonitor.Supplier := nil;
  inherited Destroy;
end;

{ TDebuggerDataMonitor }

procedure TDebuggerDataMonitor.SetSupplier(const AValue: TDebuggerDataSupplier);
begin
  if FSupplier = AValue then exit;
  Assert((FSupplier=nil) or (AValue=nil), 'TDebuggerDataMonitor.Supplier already set');
  if FSupplier <> nil then FSupplier.Monitor := nil;
  FSupplier := AValue;
  if FSupplier <> nil then FSupplier.Monitor:= self;

  DoNewSupplier;
end;

procedure TDebuggerDataMonitor.DoNewSupplier;
begin
  //
end;

destructor TDebuggerDataMonitor.Destroy;
begin
  Supplier := nil;
  inherited Destroy;
end;

{ TCurrentThreads }

procedure TCurrentThreads.SetValidity(AValidity: TDebuggerDataState);
begin
  if FDataValidity = AValidity then exit;
  {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataMonitor: TCurrentThreads.SetValidity ', dbgs(AValidity)]); {$ENDIF}

  // Assign snapshot, if old data wasn't final
  if (FDataValidity in [ddsUnknown, ddsEvaluating, ddsRequested]) and (FSnapShot <> nil)
  then FSnapShot.Assign(self);

  FDataValidity := AValidity;

  if FDataValidity = ddsUnknown then Clear;
  FMonitor.Changed;
end;

procedure TCurrentThreads.SetCurrentThreadId(const AValue: Integer);
begin
  if FCurrentThreadId = AValue then exit;
  {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataMonitor: TCurrentThreads.SetCurrentThreadId ', AValue]); {$ENDIF}
  inherited SetCurrentThreadId(AValue);
  FMonitor.CurrentChanged; // TODO ChangedSelection
end;

procedure TCurrentThreads.SetSnapShot(const AValue: TThreads);
begin
  assert((FSnapShot=nil) or (AValue=nil), 'Threads already have snapshot');
  if FSnapShot = AValue then exit;
  FSnapShot := AValue;
  if FSnapShot <> nil
  then FSnapShot.Assign(self);
end;

constructor TCurrentThreads.Create(AMonitor: TThreadsMonitor);
begin
  FMonitor := AMonitor;
  FDataValidity := ddsUnknown;
  inherited Create;
end;

function TCurrentThreads.Count: Integer;
begin
  case FDataValidity of
    ddsUnknown:   begin
        Result := 0;
        FDataValidity := ddsRequested;
        FMonitor.RequestData;
        if FDataValidity = ddsValid then Result := inherited Count();
      end;
    ddsRequested, ddsEvaluating: Result := 0;
    ddsValid:                    Result := inherited Count;
    ddsInvalid, ddsError:        Result := 0;
  end;
end;

procedure TCurrentThreads.Clear;
begin
  FDataValidity := ddsUnknown;
  inherited Clear;
end;

{ TThreadsSupplier }

function TThreadsSupplier.GetCurrentThreads: TCurrentThreads;
begin
  if Monitor <> nil
  then Result := Monitor.CurrentThreads
  else Result := nil;
end;

function TThreadsSupplier.GetMonitor: TThreadsMonitor;
begin
  Result := TThreadsMonitor(inherited Monitor);
end;

procedure TThreadsSupplier.SetMonitor(const AValue: TThreadsMonitor);
begin
  Inherited Monitor := AValue;
end;

procedure TThreadsSupplier.ChangeCurrentThread(ANewId: Integer);
begin
  //
end;

procedure TThreadsSupplier.RequestMasterData;
begin
  //
end;

procedure TThreadsSupplier.DoStateChange(const AOldState: TDBGState);
begin
  if (Debugger = nil) or (CurrentThreads = nil) then Exit;
  {$IFDEF DBG_DATA_MONITORS} DebugLnEnter(['DebugDataMonitor: >>ENTER: TThreadsSupplier.DoStateChange  New-State=', DBGStateNames[Debugger.State ]]); {$ENDIF}

  if Debugger.State in [dsPause, dsInternalPause]
  then begin
    CurrentThreads.SetValidity(ddsUnknown);
  end
  else begin
    CurrentThreads.SnapShot := nil;

    if (AOldState in [dsPause, dsInternalPause]) or (AOldState = dsNone) { Force clear on initialisation }
    then begin
      if Monitor <> nil
      then Monitor.Clear;
    end;
  end;
  {$IFDEF DBG_DATA_MONITORS} DebugLnExit(['DebugDataMonitor: <<EXIT: TThreadsSupplier.DoStateChange']); {$ENDIF}
end;

{ TThreadsMonitor }

function TThreadsMonitor.GetSupplier: TThreadsSupplier;
begin
  Result := TThreadsSupplier(inherited Supplier);
end;

function TThreadsMonitor.GetSnapshot(AnID: Pointer): TThreads;
begin
  Result := TThreads(GetSnapshotObj(AnID));
end;

procedure TThreadsMonitor.SetSupplier(const AValue: TThreadsSupplier);
begin
  inherited Supplier := AValue;
end;

procedure TThreadsMonitor.DoNewSupplier;
begin
  inherited DoNewSupplier;
  if CurrentThreads <> nil
  then CurrentThreads.SetValidity(ddsUnknown);
end;

procedure TThreadsMonitor.RequestData;
begin
  if Supplier <> nil
  then Supplier.RequestMasterData;
end;

function TThreadsMonitor.CreateSnapshot(CreateEmpty: Boolean = False): TObject;
begin
  Result := TThreads.Create;
  if not CreateEmpty
  then CurrentThreads.SnapShot := TThreads(Result);
end;

procedure TThreadsMonitor.Changed;
begin
  FNotificationList.NotifyChange(Self);
end;

procedure TThreadsMonitor.CurrentChanged;
begin
  FNotificationList.NotifyChange(Self); // TODO: is this required?? It should not
  FNotificationList.NotifyCurrent(Self);
end;

constructor TThreadsMonitor.Create;
begin
  inherited;
  FNotificationList := TDebuggerChangeNotificationList.Create;
  FCurrentThreads := TCurrentThreads.Create(self);
end;

destructor TThreadsMonitor.Destroy;
begin
  FNotificationList.Clear;
  inherited Destroy;
  FreeAndNil(FNotificationList);
  FreeAndNil(FCurrentThreads);
end;

procedure TThreadsMonitor.Clear;
begin
  {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataMonitor: TThreadsMonitor.Clear']); {$ENDIF}
  FCurrentThreads.Clear;
  Changed;
end;

procedure TThreadsMonitor.AddNotification(const ANotification: TThreadsNotification);
begin
  FNotificationList.Add(ANotification);
end;

procedure TThreadsMonitor.RemoveNotification(const ANotification: TThreadsNotification);
begin
  FNotificationList.Remove(ANotification);
end;

procedure TThreadsMonitor.ChangeCurrentThread(ANewId: Integer);
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
  const AnArguments: TStrings; const AFunctionName: String;
  const ALocationInfo: TDebuggerUnitInfo; const ALine: Integer; const AThreadId: Integer;
  const AThreadName: String; const AThreadState: String;
  AState: TDebuggerDataState);
begin
  inherited Create(AIndex, AnAdress, AnArguments, AFunctionName, ALocationInfo,
                   ALine, AState);
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

{ TThreads }

function TThreads.GetEntry(const AnIndex: Integer): TThreadEntry;
begin
  if (AnIndex < 0) or (AnIndex >= Count) then exit(nil);
  Result := TThreadEntry(FList[AnIndex]);
end;

procedure TThreads.SetCurrentThreadId(const AValue: Integer);
begin
  if FCurrentThreadId = AValue then exit;
  FCurrentThreadId := AValue;
end;

procedure TThreads.Assign(AOther: TThreads);
var
  i: Integer;
begin
  Clear;
  FCurrentThreadId := AOther.FCurrentThreadId;
  for i := 0 to AOther.FList.Count-1 do
    FList.Add(TThreadEntry.CreateCopy(TThreadEntry(AOther.FList[i])));
end;

procedure TThreads.LoadDataFromXMLConfig(const AConfig: TXMLConfig; APath: string;
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

procedure TThreads.SaveDataToXMLConfig(const AConfig: TXMLConfig; APath: string;
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

constructor TThreads.Create;
begin
  FList := TList.Create;
end;

destructor TThreads.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TThreads.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TThreads.Clear;
begin
  while FList.Count > 0 do begin
    TThreadEntry(Flist[0]).Free;
    FList.Delete(0);
  end;
end;

procedure TThreads.Add(AThread: TThreadEntry);
begin
  FList.Add(TThreadEntry.CreateCopy(AThread));
end;

{ TDebuggerProperties }

constructor TDebuggerProperties.Create;
begin
  //
end;

procedure TDebuggerProperties.Assign(Source: TPersistent);
begin
  //
end;

{ =========================================================================== }
{ TRefCountedObject }
{ =========================================================================== }

procedure TRefCountedObject.AddReference;
begin
  Inc(FRefcount);
end;

procedure TRefCountedObject.DoFree;
begin
  Self.Free;
end;

constructor TRefCountedObject.Create;
begin
  FRefCount := 0;
  inherited;
end;

destructor TRefCountedObject.Destroy;
begin
  Assert(FRefcount = 0, 'Destroying referenced object');
  inherited;
end;

procedure TRefCountedObject.ReleaseReference;
begin
  Assert(FRefCount > 0, 'TRefCountedObject.ReleaseReference  RefCount > 0');
  Dec(FRefCount);
  if FRefCount = 0 then DoFree;
end;

(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   D E B U G G E R                                                        **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)


{ =========================================================================== }
{ TDebugger }
{ =========================================================================== }

class function TDebugger.Caption: String;
begin
  Result := 'No caption set';
end;

function TDebugger.ChangeFileName: Boolean;
begin
  Result := True;
end;

constructor TDebugger.Create(const AExternalDebugger: String);
var
  list: TStringList;
  nr: TDebuggerNotifyReason;
begin
  inherited Create;
  for nr := low(TDebuggerNotifyReason) to high(TDebuggerNotifyReason) do
    FDestroyNotificationList[nr] := TMethodList.Create;
  FOnState := nil;
  FOnCurrent := nil;
  FOnOutput := nil;
  FOnDbgOutput := nil;
  FState := dsNone;
  FArguments := '';
  FFilename := '';
  FExternalDebugger := AExternalDebugger;

  list := TStringList.Create;
  list.Sorted := True;
  list.Duplicates := dupIgnore;
  list.OnChange := @DebuggerEnvironmentChanged;
  FDebuggerEnvironment := list;

  list := TStringList.Create;
  list.Sorted := True;
  list.Duplicates := dupIgnore;
  list.OnChange := @EnvironmentChanged;
  FEnvironment := list;
  FCurEnvironment := TStringList.Create;
  FInternalUnitInfoProvider := TDebuggerUnitInfoProvider.Create;

  FBreakPoints := CreateBreakPoints;
  FLocals := CreateLocals;
  FLineInfo := CreateLineInfo;
  FRegisters := CreateRegisters;
  FCallStack := CreateCallStack;
  FDisassembler := CreateDisassembler;
  FWatches := CreateWatches;
  FThreads := CreateThreads;
  FExceptions := CreateExceptions;
  FSignals := CreateSignals;
  FExitCode := 0;
end;

function TDebugger.CreateBreakPoints: TDBGBreakPoints;
begin
  Result := TDBGBreakPoints.Create(Self, TDBGBreakPoint);
end;

function TDebugger.CreateCallStack: TCallStackSupplier;
begin
  Result := TCallStackSupplier.Create(Self);
end;

function TDebugger.CreateDisassembler: TDBGDisassembler;
begin
  Result := TDBGDisassembler.Create(Self);
end;

function TDebugger.CreateExceptions: TDBGExceptions;
begin
  Result := TDBGExceptions.Create(Self, TDBGException);
end;

function TDebugger.CreateLocals: TLocalsSupplier;
begin
  Result := TLocalsSupplier.Create(Self);
end;

function TDebugger.CreateLineInfo: TDBGLineInfo;
begin
  Result := TDBGLineInfo.Create(Self);
end;

class function TDebugger.CreateProperties: TDebuggerProperties;
begin
  Result := TDebuggerProperties.Create;
end;

function TDebugger.CreateRegisters: TDBGRegisters;
begin
  Result := TDBGRegisters.Create(Self);
end;

function TDebugger.CreateSignals: TDBGSignals;
begin
  Result := TDBGSignals.Create(Self, TDBGSignal);
end;

function TDebugger.CreateWatches: TWatchesSupplier;
begin
  Result := TWatchesSupplier.Create(Self);
end;

function TDebugger.CreateThreads: TThreadsSupplier;
begin
  Result := TThreadsSupplier.Create(Self);
end;

procedure TDebugger.DebuggerEnvironmentChanged (Sender: TObject );
begin
end;

destructor TDebugger.Destroy;
var
  nr: TDebuggerNotifyReason;
begin
  FDestroyNotificationList[dnrDestroy].CallNotifyEvents(Self);
  for nr := low(TDebuggerNotifyReason) to high(TDebuggerNotifyReason) do
    FreeAndNil(FDestroyNotificationList[nr]);
  // don't call events
  FOnState := nil;
  FOnCurrent := nil;
  FOnOutput := nil;
  FOnDbgOutput := nil;

  if FState <> dsNone
  then Done;

  FBreakPoints.FDebugger := nil;
  FLocals.FDebugger := nil;
  FLineInfo.FDebugger := nil;
  FRegisters.FDebugger := nil;
  FCallStack.FDebugger := nil;
  FDisassembler.FDebugger := nil;
  FWatches.Debugger := nil;
  FThreads.Debugger := nil;

  FreeAndNil(FInternalUnitInfoProvider);
  FreeAndNil(FExceptions);
  FreeAndNil(FBreakPoints);
  FreeAndNil(FLocals);
  FreeAndNil(FLineInfo);
  FreeAndNil(FRegisters);
  FreeAndNil(FCallStack);
  FreeAndNil(FDisassembler);
  FreeAndNil(FWatches);
  FreeAndNil(FThreads);
  FreeAndNil(FDebuggerEnvironment);
  FreeAndNil(FEnvironment);
  FreeAndNil(FCurEnvironment);
  FreeAndNil(FSignals);
  inherited;
end;

function TDebugger.Disassemble(AAddr: TDbgPtr; ABackward: Boolean; out ANextAddr: TDbgPtr; out ADump, AStatement, AFile: String; out ALine: Integer): Boolean;
begin
  Result := ReqCmd(dcDisassemble, [AAddr, ABackward, @ANextAddr, @ADump, @AStatement, @AFile, @ALine]);
end;

function TDebugger.GetLocation: TDBGLocationRec;
begin
  Result.Address := 0;
  Result.SrcLine := 0;
end;

procedure TDebugger.LockCommandProcessing;
begin
  // nothing
end;

procedure TDebugger.UnLockCommandProcessing;
begin
  // nothing
end;

procedure TDebugger.AddNotifyEvent(AReason: TDebuggerNotifyReason; AnEvent: TNotifyEvent);
begin
  FDestroyNotificationList[AReason].Add(TMethod(AnEvent));
end;

procedure TDebugger.RemoveNotifyEvent(AReason: TDebuggerNotifyReason; AnEvent: TNotifyEvent);
begin
  FDestroyNotificationList[AReason].Remove(TMethod(AnEvent));
end;

procedure TDebugger.Done;
begin
  SetState(dsNone);
  FEnvironment.Clear;
  FCurEnvironment.Clear;
end;

procedure TDebugger.Release;
begin
  if Self <> nil
  then Self.DoRelease;
end;

procedure TDebugger.DoCurrent(const ALocation: TDBGLocationRec);
begin
  {$IFDEF DBG_EVENTS} DebugLnEnter(['DebugEvent: Enter >> DoCurrent (Location)  >>  State=', DBGStateNames[FState]]); {$ENDIF}
  if Assigned(FOnCurrent) then FOnCurrent(Self, ALocation);
  {$IFDEF DBG_EVENTS} DebugLnExit(['DebugEvent: Exit  << DoCurrent (Location)  <<']); {$ENDIF}
end;

procedure TDebugger.DoDbgOutput(const AText: String);
begin
  // WriteLN(' [TDebugger] ', AText);
  if Assigned(FOnDbgOutput) then FOnDbgOutput(Self, AText);
end;

procedure TDebugger.DoDbgEvent(const ACategory: TDBGEventCategory; const AEventType: TDBGEventType; const AText: String);
{$IFDEF DBG_EVENTS} var s: String; {$ENDIF}
begin
  {$IFDEF DBG_EVENTS} writestr(s, ACategory); DebugLnEnter(['DebugEvent: Enter >> DoDbgEvent >>  State=', DBGStateNames[FState], ' Category=', s]); {$ENDIF}
  if Assigned(FOnDbgEvent) then FOnDbgEvent(Self, ACategory, AEventType, AText);
  {$IFDEF DBG_EVENTS} DebugLnExit(['DebugEvent: Exit  << DoDbgEvent <<']);  {$ENDIF}
end;

procedure TDebugger.DoException(const AExceptionType: TDBGExceptionType;
  const AExceptionClass: String; const AExceptionLocation: TDBGLocationRec; const AExceptionText: String; out AContinue: Boolean);
begin
  {$IFDEF DBG_EVENTS} DebugLnEnter(['DebugEvent: Enter >> DoException >>  State=', DBGStateNames[FState]]); {$ENDIF}
  if AExceptionType = deInternal then
    DoDbgEvent(ecDebugger, etExceptionRaised,
               Format('Exception class "%s" at $%.' + IntToStr(TargetWidth div 4) + 'x with message "%s"',
                      [AExceptionClass, AExceptionLocation.Address, AExceptionText]));
  if Assigned(FOnException) then
    FOnException(Self, AExceptionType, AExceptionClass, AExceptionLocation, AExceptionText, AContinue)
  else
    AContinue := True;
  {$IFDEF DBG_EVENTS} DebugLnExit(['DebugEvent: Exit  << DoException <<']);  {$ENDIF}
end;

procedure TDebugger.DoOutput(const AText: String);
begin
  if Assigned(FOnOutput) then FOnOutput(Self, AText);
end;

procedure TDebugger.DoBreakpointHit(const ABreakPoint: TBaseBreakPoint; var ACanContinue: Boolean);
begin
  {$IFDEF DBG_EVENTS} DebugLnEnter(['DebugEvent: Enter >> DoBreakpointHit <<  State=', DBGStateNames[FState]]); {$ENDIF}
  if Assigned(FOnBreakpointHit)
  then FOnBreakpointHit(Self, ABreakPoint, ACanContinue);
  {$IFDEF DBG_EVENTS} DebugLnExit(['DebugEvent: Exit  >> DoBreakpointHit <<']);  {$ENDIF}
end;

procedure TDebugger.DoBeforeState(const OldState: TDBGState);
begin
  {$IFDEF DBG_STATE_EVENT} DebugLnEnter(['DebugEvent: Enter >> DoBeforeState <<  State=', DBGStateNames[FState]]); {$ENDIF}
  if Assigned(FOnBeforeState) then FOnBeforeState(Self, OldState);
  {$IFDEF DBG_STATE_EVENT} DebugLnExit(['DebugEvent: Exit  >> DoBeforeState <<']);  {$ENDIF}
end;

procedure TDebugger.DoState(const OldState: TDBGState);
begin
  {$IFDEF DBG_STATE_EVENT} DebugLnEnter(['DebugEvent: Enter >> DoState <<  State=', DBGStateNames[FState]]); {$ENDIF}
  if Assigned(FOnState) then FOnState(Self, OldState);
  {$IFDEF DBG_STATE_EVENT} DebugLnExit(['DebugEvent: Exit  >> DoState <<']);  {$ENDIF}
end;

procedure TDebugger.EnvironmentChanged(Sender: TObject);
var
  n, idx: integer;
  S: String;
  Env: TStringList;
begin
  // Createe local copy
  if FState <> dsNone then
  begin
    Env := TStringList.Create;
    try
      Env.Assign(Environment);

      // Check for nonexisting and unchanged vars
      for n := 0 to FCurEnvironment.Count - 1 do
      begin
        S := FCurEnvironment[n];
        idx := Env.IndexOfName(GetPart([], ['='], S, False, False));
        if idx = -1
        then ReqCmd(dcEnvironment, [S, False])
        else begin
          if Env[idx] = S
          then Env.Delete(idx);
        end;
      end;

      // Set the remaining
      for n := 0 to Env.Count - 1 do
      begin
        S := Env[n];
        //Skip functions etc.
        if Pos('=()', S) <> 0 then Continue;
        ReqCmd(dcEnvironment, [S, True]);
      end;
    finally
      Env.Free;
    end;
  end;
  FCurEnvironment.Assign(FEnvironment);
end;

function TDebugger.GetUnitInfoProvider: TDebuggerUnitInfoProvider;
begin
  Result := FUnitInfoProvider;
  if Result = nil then
    Result := FInternalUnitInfoProvider;
end;

function TDebugger.GetIsIdle: Boolean;
begin
  Result := False;
end;

function TDebugger.Evaluate(const AExpression: String; var AResult: String;
  var ATypeInfo: TDBGType; EvalFlags: TDBGEvaluateFlags = []): Boolean;
begin
  FreeAndNIL(ATypeInfo);
  Result := ReqCmd(dcEvaluate, [AExpression, @AResult, @ATypeInfo, Integer(EvalFlags)]);
end;

class function TDebugger.ExePaths: String;
begin
  Result := '';
end;

class function TDebugger.HasExePath: boolean;
begin
  Result := true; // most debugger are external and have an exe path
end;

function TDebugger.GetCommands: TDBGCommands;
begin
  Result := COMMANDMAP[State] * GetSupportedCommands;
end;

class function TDebugger.GetProperties: TDebuggerProperties;
var
  idx: Integer;
begin
  if MDebuggerPropertiesList = nil
  then MDebuggerPropertiesList := TStringList.Create;
  idx := MDebuggerPropertiesList.IndexOf(ClassName);
  if idx = -1
  then begin
    Result := CreateProperties;
    MDebuggerPropertiesList.AddObject(ClassName, Result)
  end
  else begin
    Result := TDebuggerProperties(MDebuggerPropertiesList.Objects[idx]);
  end;
end;

function TDebugger.GetState: TDBGState;
begin
  Result := FState;
end;

function TDebugger.GetSupportedCommands: TDBGCommands;
begin
  Result := [];
end;

function TDebugger.GetTargetWidth: Byte;
begin
  Result := SizeOf(PtrInt)*8;
end;

function TDebugger.GetWaiting: Boolean;
begin
  Result := False;
end;

procedure TDebugger.Init;
begin
  FExitCode := 0;
  FErrorStateMessage := '';
  FErrorStateInfo := '';
  SetState(dsIdle);
end;

procedure TDebugger.JumpTo(const ASource: String; const ALine: Integer);
begin
  ReqCmd(dcJumpTo, [ASource, ALine]);
end;

procedure TDebugger.SendConsoleInput(AText: String);
begin
  ReqCmd(dcSendConsoleInput, [AText]);
end;

function TDebugger.Modify(const AExpression, AValue: String): Boolean;
begin
  Result := ReqCmd(dcModify, [AExpression, AValue]);
end;

procedure TDebugger.Pause;
begin
  ReqCmd(dcPause, []);
end;

function TDebugger.ReqCmd(const ACommand: TDBGCommand;
  const AParams: array of const): Boolean;
begin
  if FState = dsNone then Init;
  if ACommand in Commands
  then begin
    Result := RequestCommand(ACommand, AParams);
    if not Result then begin
      DebugLn('TDebugger.ReqCmd failed: ',DBGCommandNames[ACommand]);
    end;
  end
  else begin
    DebugLn('TDebugger.ReqCmd Command not supported: ',
            DBGCommandNames[ACommand],' ClassName=',ClassName);
    Result := False;
  end;
end;

procedure TDebugger.Run;
begin
  ReqCmd(dcRun, []);
end;

procedure TDebugger.RunTo(const ASource: String; const ALine: Integer);
begin
  ReqCmd(dcRunTo, [ASource, ALine]);
end;

procedure TDebugger.SetDebuggerEnvironment (const AValue: TStrings );
begin
  FDebuggerEnvironment.Assign(AValue);
end;

procedure TDebugger.SetEnvironment(const AValue: TStrings);
begin
  FEnvironment.Assign(AValue);
end;

procedure TDebugger.SetExitCode(const AValue: Integer);
begin
  FExitCode := AValue;
end;

procedure TDebugger.SetFileName(const AValue: String);
begin
  if FFileName <> AValue
  then begin
    DebugLn('[TDebugger.SetFileName] "', AValue, '"');
    if FState in [dsRun, dsPause]
    then begin
      Stop;
      // check if stopped
      if FState <> dsStop
      then SetState(dsError);
    end;

    if FState = dsStop
    then begin
      // Reset state
      FFileName := '';
      SetState(dsIdle);
      ChangeFileName;
    end;

    FFileName := AValue;
    if  (FFilename <> '') and (FState = dsIdle) and ChangeFileName
    then SetState(dsStop);
  end;
end;

class procedure TDebugger.SetProperties(const AProperties: TDebuggerProperties);
var
  Props: TDebuggerProperties;
begin
  if AProperties = nil then Exit;
  Props := GetProperties;
  if Props = AProperties then Exit;

  if Props = nil then Exit; // they weren't created ?
  Props.Assign(AProperties);
end;

procedure TDebugger.SetState(const AValue: TDBGState);
var
  OldState: TDBGState;
begin
  // dsDestroying is final, do not unset
  if FState = dsDestroying
  then exit;

  // dsDestroying must be silent. The ide believes the debugger is gone already
  if AValue = dsDestroying
  then begin
    FState := AValue;
    exit;
  end;

  if AValue <> FState
  then begin
    {$IFDEF DBG_STATE}
    DebugLnEnter(['DebuggerState: Setting to ', DBGStateNames[AValue],', from ', DBGStateNames[FState]]);
    {$ENDIF}
    OldState := FState;
    FState := AValue;
    LockCommandProcessing;
    try
      DoBeforeState(OldState);
      try
        FThreads.DoStateChange(OldState);
        FCallStack.DoStateChange(OldState);
        FBreakpoints.DoStateChange(OldState);
        FLocals.DoStateChange(OldState);
        FLineInfo.DoStateChange(OldState);
        FRegisters.DoStateChange(OldState);
        FDisassembler.DoStateChange(OldState);
        FWatches.DoStateChange(OldState);
      finally
        DoState(OldState);
      end;
    finally
      UnLockCommandProcessing;
      {$IFDEF DBG_STATE}
      DebugLnExit(['DebuggerState: Finished ', DBGStateNames[AValue]]);
      {$ENDIF}
    end;
  end;
end;

procedure TDebugger.SetErrorState(const AMsg: String; const AInfo: String = '');
begin
  if FErrorStateMessage = ''
  then FErrorStateMessage := AMsg;
  if FErrorStateInfo = ''
  then FErrorStateInfo := AInfo;
  SetState(dsError);
end;

procedure TDebugger.DoRelease;
begin
  Self.Free;
end;

procedure TDebugger.StepInto;
begin
  if ReqCmd(dcStepInto, []) then exit;
  DebugLn('TDebugger.StepInto Class=',ClassName,' failed.');
end;

procedure TDebugger.StepOverInstr;
begin
  if ReqCmd(dcStepOverInstr, []) then exit;
  DebugLn('TDebugger.StepOverInstr Class=',ClassName,' failed.');
end;

procedure TDebugger.StepIntoInstr;
begin
  if ReqCmd(dcStepIntoInstr, []) then exit;
  DebugLn('TDebugger.StepIntoInstr Class=',ClassName,' failed.');
end;

procedure TDebugger.StepOut;
begin
  if ReqCmd(dcStepOut, []) then exit;
  DebugLn('TDebugger.StepOut Class=', ClassName, ' failed.');
end;

procedure TDebugger.StepOver;
begin
  if ReqCmd(dcStepOver, []) then exit;
  DebugLn('TDebugger.StepOver Class=',ClassName,' failed.');
end;

procedure TDebugger.Stop;
begin
  if ReqCmd(dcStop,[]) then exit;
  DebugLn('TDebugger.Stop Class=',ClassName,' failed.');
end;

(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   B R E A K P O I N T S                                                  **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

{ ===========================================================================
  TBaseBreakPoint
  =========================================================================== }

function TBaseBreakPoint.GetAddress: TDBGPtr;
begin
  Result := FAddress;
end;

function TBaseBreakPoint.GetKind: TDBGBreakPointKind;
begin
  Result := FKind;
end;

procedure TBaseBreakPoint.SetKind(const AValue: TDBGBreakPointKind);
begin
  if FKind <> AValue
  then begin
    FKind := AValue;
    DoKindChange;
  end;
end;

procedure TBaseBreakPoint.SetAddress(const AValue: TDBGPtr);
begin
  if FAddress <> AValue then
  begin
    FAddress := AValue;
    Changed;
  end;
end;

function TBaseBreakPoint.GetWatchData: String;
begin
  Result := FWatchData;
end;

function TBaseBreakPoint.GetWatchScope: TDBGWatchPointScope;
begin
  Result := FWatchScope;
end;

function TBaseBreakPoint.GetWatchKind: TDBGWatchPointKind;
begin
  Result := FWatchKind;
end;

procedure TBaseBreakPoint.AssignLocationTo(Dest: TPersistent);
var
  DestBreakPoint: TBaseBreakPoint absolute Dest;
begin
  DestBreakPoint.SetLocation(FSource, FLine);
end;

procedure TBaseBreakPoint.AssignTo(Dest: TPersistent);
var
  DestBreakPoint: TBaseBreakPoint absolute Dest;
begin
  // updatelock is set in source.assignto
  if Dest is TBaseBreakPoint
  then begin
    DestBreakPoint.SetKind(FKind);
    DestBreakPoint.SetWatch(FWatchData, FWatchScope, FWatchKind);
    DestBreakPoint.SetAddress(FAddress);
    AssignLocationTo(DestBreakPoint);
    DestBreakPoint.SetBreakHitCount(FBreakHitCount);
    DestBreakPoint.SetExpression(FExpression);
    DestBreakPoint.SetEnabled(FEnabled);
    DestBreakPoint.InitialEnabled := FInitialEnabled;
  end
  else inherited;
end;

constructor TBaseBreakPoint.Create(ACollection: TCollection);
begin
  FAddress := 0;
  FSource := '';
  FLine := -1;
  FValid := vsUnknown;
  FEnabled := False;
  FHitCount := 0;
  FBreakHitCount := 0;
  FExpression := '';
  FInitialEnabled := False;
  FKind := bpkSource;
  inherited Create(ACollection);
end;

procedure TBaseBreakPoint.DoBreakHitCountChange;
begin
  Changed;
end;

procedure TBaseBreakPoint.DoEnableChange;
begin
  Changed;
end;

procedure TBaseBreakPoint.DoExpressionChange;
begin
  Changed;
end;

procedure TBaseBreakPoint.DoHit(const ACount: Integer; var AContinue: Boolean );
begin
  SetHitCount(ACount);
end;

function TBaseBreakPoint.GetBreakHitCount: Integer;
begin
  Result := FBreakHitCount;
end;

function TBaseBreakPoint.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TBaseBreakPoint.GetExpression: String;
begin
  Result := FExpression;
end;

function TBaseBreakPoint.GetHitCount: Integer;
begin
  Result := FHitCount;
end;

function TBaseBreakPoint.GetLine: Integer;
begin
  Result := FLine;
end;

function TBaseBreakPoint.GetSource: String;
begin
  Result := FSource;
end;

function TBaseBreakPoint.GetValid: TValidState;
begin
  Result := FValid;
end;

procedure TBaseBreakPoint.SetBreakHitCount(const AValue: Integer);
begin
  if FBreakHitCount <> AValue
  then begin
    FBreakHitCount := AValue;
    DoBreakHitCountChange;
  end;
end;

procedure TBaseBreakPoint.SetEnabled (const AValue: Boolean );
begin
  if FEnabled <> AValue
  then begin
    FEnabled := AValue;
    DoEnableChange;
  end;
end;

procedure TBaseBreakPoint.SetExpression (const AValue: String );
begin
  if FExpression <> AValue
  then begin
    FExpression := AValue;
    DoExpressionChange;
  end;
end;

procedure TBaseBreakPoint.SetHitCount (const AValue: Integer );
begin
  if FHitCount <> AValue
  then begin
    FHitCount := AValue;
    Changed;
  end;
end;

procedure TBaseBreakPoint.DoKindChange;
begin
  Changed;
end;

procedure TBaseBreakPoint.SetInitialEnabled(const AValue: Boolean);
begin
  if FInitialEnabled=AValue then exit;
  FInitialEnabled:=AValue;
end;

procedure TBaseBreakPoint.SetLocation (const ASource: String; const ALine: Integer );
begin
  if (FSource = ASource) and (FLine = ALine) then exit;
  FSource := ASource;
  FLine := ALine;
  Changed;
end;

procedure TBaseBreakPoint.SetWatch(const AData: String; const AScope: TDBGWatchPointScope;
  const AKind: TDBGWatchPointKind);
begin
  if (AData = FWatchData) and (AScope = FWatchScope) and (AKind = FWatchKind) then exit;
  FWatchData := AData;
  FWatchScope := AScope;
  FWatchKind := AKind;
  Changed;
end;

procedure TBaseBreakPoint.SetValid(const AValue: TValidState );
begin
  if FValid <> AValue
  then begin
    FValid := AValue;
    Changed;
  end;
end;

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
  Changed;
end;

procedure TIDEBreakPoint.SetLogEvalExpression(AValue: String);
begin
  if FLogEvalExpression <> AValue then
  begin
    FLogEvalExpression := AValue;
    Changed;
  end;
end;

procedure TIDEBreakPoint.SetLogMessage(const AValue: String);
begin
  if FLogMessage <> AValue then
  begin
    FLogMessage := AValue;
    Changed;
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
    Changed;
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
  if FMaster <> nil then FMaster.InitialEnabled := AValue;
end;

procedure TIDEBreakPoint.SetExpression(const AValue: String);
begin
  if AValue=Expression then exit;
  inherited SetExpression(AValue);
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
    FreeAndNil(FMaster);
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
  Changed;
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
  if Actions * [bpaDisableGroup, bpaEnableGroup] <> []
  then sleep(2500);
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
  WriteStr(s, FWatchScope);
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
  if FMaster<>nil then FMaster.SetAddress(Address);
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
    Changed;
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
{ TDBGBreakPoint }
{ =========================================================================== }

constructor TDBGBreakPoint.Create (ACollection: TCollection );
begin
  FSlave := nil;
  inherited Create(ACollection);
end;

destructor TDBGBreakPoint.Destroy;
var
  SBP: TBaseBreakPoint;
begin
  SBP := FSlave;
  FSlave := nil;
  if SBP <> nil
  then SBP.DoChanged;   // In case UpdateCount  0

  inherited Destroy;
end;

procedure TDBGBreakPoint.Hit(var ACanContinue: Boolean);
var
  cnt: Integer;
begin
  cnt := HitCount + 1;
  if BreakHitcount > 0
  then ACanContinue := cnt < BreakHitcount;
  DoHit(cnt, ACanContinue);
  if Assigned(FSlave)
  then FSlave.DoHit(cnt, ACanContinue);
  Debugger.DoBreakpointHit(Self, ACanContinue)
end;

procedure TDBGBreakPoint.DoChanged;
begin
  inherited DoChanged;
  if FSlave <> nil
  then FSlave.Changed;
end;

procedure TDBGBreakPoint.DoStateChange(const AOldState: TDBGState);
begin
  if Debugger.State <> dsStop then Exit;
  if not (AOldState in [dsIdle, dsNone]) then Exit;

  BeginUpdate;
  try
    SetLocation(FSource, Line);
    Enabled := InitialEnabled;
    SetHitCount(0);
  finally
    EndUpdate;
  end;
end;

procedure TDBGBreakPoint.DoLogMessage(const AMessage: String);
begin
  Debugger.DoDbgEvent(ecBreakpoint, etBreakpointMessage, 'Breakpoint Message: ' + AMessage);
end;

procedure TDBGBreakPoint.DoLogCallStack(const Limit: Integer);
const
  Spacing = '    ';
var
  CallStack: TCallStack;
  I, Count: Integer;
  Entry: TCallStackEntry;
  StackString: String;
begin
  Debugger.SetState(dsInternalPause);
  CallStack := Debugger.CallStack.CurrentCallStackList.EntriesForThreads[Debugger.Threads.CurrentThreads.CurrentThreadId];
  if Limit = 0 then
  begin
    Debugger.DoDbgEvent(ecBreakpoint, etBreakpointMessage, 'Breakpoint Call Stack: Log all stack frames');
    Count := CallStack.Count;
    CallStack.PrepareRange(0, Count);
  end
  else
  begin
    Debugger.DoDbgEvent(ecBreakpoint, etBreakpointMessage, Format('Breakpoint Call Stack: Log %d stack frames', [Limit]));
    Count := Min(CallStack.Count, Limit);
    CallStack.PrepareRange(0, Count);
  end;

  for I := 0 to Count - 1 do
  begin
    Entry := CallStack.Entries[I];
    StackString := Spacing + Entry.Source;
    if Entry.Source = '' then // we do not have a source file => just show an adress
      StackString := Spacing + ':' + IntToHex(Entry.Address, 8);
    StackString := StackString + ' ' + Entry.GetFunctionWithArg;
    if line > 0 then
      StackString := StackString + ' line ' + IntToStr(Entry.Line);

    Debugger.DoDbgEvent(ecBreakpoint, etBreakpointStackDump, StackString);
  end;
end;

procedure TDBGBreakPoint.DoLogExpression(const AnExpression: String);
begin
  // will be called while Debgger.State = dsRun => can not call Evaluate
end;

function TDBGBreakPoint.GetDebugger: TDebugger;
begin
  Result := TDBGBreakPoints(Collection).FDebugger;
end;

procedure TDBGBreakPoint.SetSlave(const ASlave : TBaseBreakPoint);
begin
  Assert((FSlave = nil) or (ASlave = nil), 'TDBGBreakPoint.SetSlave already has a slave');
  FSlave := ASlave;
end;

procedure TDBGBreakPoint.SetEnabled(const AValue: Boolean);
begin
  if Enabled = AValue then exit;
  inherited SetEnabled(AValue);
  // feedback to IDEBreakPoint
  if FSlave <> nil then FSlave.Enabled := AValue;
end;

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
    BP.Assign(ABreakPoint);
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
    FreeAndNil(LoadBreakPoint)
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
{ TDBGBreakPoints }
{ =========================================================================== }

function TDBGBreakPoints.Add (const ASource: String; const ALine: Integer ): TDBGBreakPoint;
begin
  Result := TDBGBreakPoint(inherited Add(ASource, ALine));
end;

function TDBGBreakPoints.Add(const AAddress: TDBGPtr): TDBGBreakPoint;
begin
  Result := TDBGBreakPoint(inherited Add(AAddress));
end;

function TDBGBreakPoints.Add(const AData: String; const AScope: TDBGWatchPointScope;
  const AKind: TDBGWatchPointKind): TDBGBreakPoint;
begin
  Result := TDBGBreakPoint(inherited Add(AData, AScope, AKind));
end;

constructor TDBGBreakPoints.Create (const ADebugger: TDebugger; const ABreakPointClass: TDBGBreakPointClass );
begin
  FDebugger := ADebugger;
  inherited Create(ABreakPointClass);
end;

procedure TDBGBreakPoints.DoStateChange(const AOldState: TDBGState);
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
    GetItem(n).DoStateChange(AOldState);
end;

function TDBGBreakPoints.Find(const ASource: String; const ALine: Integer): TDBGBreakPoint;
begin
  Result := TDBGBreakPoint(inherited Find(Asource, ALine, nil));
end;

function TDBGBreakPoints.Find (const ASource: String; const ALine: Integer; const AIgnore: TDBGBreakPoint ): TDBGBreakPoint;
begin
  Result := TDBGBreakPoint(inherited Find(ASource, ALine, AIgnore));
end;

function TDBGBreakPoints.Find(const AAddress: TDBGPtr): TDBGBreakPoint;
begin
  Result := TDBGBreakPoint(inherited Find(AAddress));
end;

function TDBGBreakPoints.Find(const AAddress: TDBGPtr; const AIgnore: TDBGBreakPoint): TDBGBreakPoint;
begin
  Result := TDBGBreakPoint(inherited Find(AAddress, nil));
end;

function TDBGBreakPoints.Find(const AData: String; const AScope: TDBGWatchPointScope;
  const AKind: TDBGWatchPointKind): TDBGBreakPoint;
begin
  Result := TDBGBreakPoint(inherited Find(AData, AScope, AKind, nil));
end;

function TDBGBreakPoints.Find(const AData: String; const AScope: TDBGWatchPointScope;
  const AKind: TDBGWatchPointKind; const AIgnore: TDBGBreakPoint): TDBGBreakPoint;
begin
  Result := TDBGBreakPoint(inherited Find(AData, AScope, AKind, AIgnore));
end;

function TDBGBreakPoints.GetItem (const AnIndex: Integer ): TDBGBreakPoint;
begin
  Result := TDBGBreakPoint(inherited GetItem(AnIndex));
end;

procedure TDBGBreakPoints.SetItem (const AnIndex: Integer; const AValue: TDBGBreakPoint );
begin
  inherited SetItem(AnIndex, AValue);
end;

{ =========================================================================== }
{ TBaseBreakPoints }
{ =========================================================================== }

function TBaseBreakPoints.Add(const ASource: String; const ALine: Integer): TBaseBreakPoint;
begin
  Result := TBaseBreakPoint(inherited Add);
  Result.SetKind(bpkSource);
  Result.SetLocation(ASource, ALine);
end;

function TBaseBreakPoints.Add(const AAddress: TDBGPtr): TBaseBreakPoint;
begin
  Result := TBaseBreakPoint(inherited Add);
  Result.SetKind(bpkAddress);
  Result.SetAddress(AAddress);
end;

function TBaseBreakPoints.Add(const AData: String; const AScope: TDBGWatchPointScope;
  const AKind: TDBGWatchPointKind): TBaseBreakPoint;
begin
  Result := TBaseBreakPoint(inherited Add);
  Result.SetKind(bpkData);
  Result.SetWatch(AData, AScope, AKind);
end;

constructor TBaseBreakPoints.Create(const ABreakPointClass: TBaseBreakPointClass);
begin
  inherited Create(ABreakPointClass);
end;

function TBaseBreakPoints.Find(const ASource: String; const ALine: Integer): TBaseBreakPoint;
begin
  Result := Find(ASource, ALine, nil);
end;

function TBaseBreakPoints.Find(const ASource: String; const ALine: Integer; const AIgnore: TBaseBreakPoint): TBaseBreakPoint;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
  begin
    Result := TBaseBreakPoint(GetItem(n));
    if  (Result.Kind = bpkSource)
    and (Result.Line = ALine)
    and (AIgnore <> Result)
    and (CompareFilenames(Result.Source, ASource) = 0)
    then Exit;
  end;
  Result := nil;
end;

function TBaseBreakPoints.Find(const AAddress: TDBGPtr): TBaseBreakPoint;
begin
  Result := Find(AAddress, nil);
end;

function TBaseBreakPoints.Find(const AAddress: TDBGPtr; const AIgnore: TBaseBreakPoint): TBaseBreakPoint;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
  begin
    Result := TBaseBreakPoint(GetItem(n));
    if  (Result.Kind = bpkAddress)
    and (Result.Address = AAddress)
    and (AIgnore <> Result)
    then Exit;
  end;
  Result := nil;
end;

function TBaseBreakPoints.Find(const AData: String; const AScope: TDBGWatchPointScope;
  const AKind: TDBGWatchPointKind): TBaseBreakPoint;
begin
  Result := Find(AData, AScope, AKind, nil);
end;

function TBaseBreakPoints.Find(const AData: String; const AScope: TDBGWatchPointScope;
  const AKind: TDBGWatchPointKind; const AIgnore: TBaseBreakPoint): TBaseBreakPoint;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
  begin
    Result := TBaseBreakPoint(GetItem(n));
    if  (Result.Kind = bpkData)
    and (Result.WatchData = AData)
    and (Result.WatchScope = AScope)
    and (Result.WatchKind = AKind)
    and (AIgnore <> Result)
    then Exit;
  end;
  Result := nil;
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
  FName := AValue;
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
(**   D E B U G   I N F O R M A T I O N                                      **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

{ TDBGField }

procedure TDBGField.IncRefCount;
begin
  inc(FRefCount);
end;

procedure TDBGField.DecRefCount;
begin
  dec(FRefCount);
  if FRefCount <= 0
  then Self.Free;
end;

constructor TDBGField.Create(const AName: String; ADBGType: TDBGType;
  ALocation: TDBGFieldLocation; AFlags: TDBGFieldFlags; AClassName: String = '');
begin
  inherited Create;
  FName := AName;
  FLocation := ALocation;
  FDBGType := ADBGType;
  FFlags := AFlags;
  FRefCount := 0;
  FClassName := AClassName;
end;

destructor TDBGField.Destroy;
begin
  FreeAndNil(FDBGType);
  inherited Destroy;
end;

{ TDBGFields }

constructor TDBGFields.Create;
begin
  FList := TList.Create;
  inherited;
end;

destructor TDBGFields.Destroy;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
    Items[n].DecRefCount;

  FreeAndNil(FList);
  inherited;
end;

procedure TDBGFields.Add(const AField: TDBGField);
begin
  AField.IncRefCount;
  FList.Add(AField);
end;

function TDBGFields.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDBGFields.GetField(const AIndex: Integer): TDBGField;
begin
  Result := TDBGField(FList[AIndex]);
end;

{ TDBGPType }

procedure TDBGType.Init;
begin
  //
end;

constructor TDBGType.Create(AKind: TDBGSymbolKind; const ATypeName: String);
begin
  FKind := AKind;
  FTypeName := ATypeName;
  Init;
  inherited Create;
end;

constructor TDBGType.Create(AKind: TDBGSymbolKind; const AArguments: TDBGTypes; AResult: TDBGType);
begin
  FKind := AKind;
  FArguments := AArguments;
  FResult := AResult;
  Init;
  inherited Create;
end;

destructor TDBGType.Destroy;
begin
  FreeAndNil(FResult);
  FreeAndNil(FArguments);
  FreeAndNil(FFields);
  FreeAndNil(FMembers);
  inherited;
end;

{ TDBGPTypes }

constructor TDBGTypes.Create;
begin
  FList := TList.Create;
  inherited;
end;

destructor TDBGTypes.Destroy;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
    Items[n].Free;

  FreeAndNil(FList);
  inherited;
end;

function TDBGTypes.GetCount: Integer;
begin
  Result := Flist.Count;
end;

function TDBGTypes.GetType(const AIndex: Integer): TDBGType;
begin
  Result := TDBGType(FList[AIndex]);
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
end;

procedure TWatch.DoExpressionChange;
begin
  Changed;
end;

procedure TWatch.DoDisplayFormatChanged;
begin
  Changed;
end;

function TWatch.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TWatch.GetValue(const AThreadId: Integer; const AStackFrame: Integer): TWatchValue;
begin
  Result := FValueList[AThreadId, AStackFrame, FDisplayFormat];
end;

function TWatch.GetValueEx(const AThreadId: Integer; const AStackFrame: Integer;
                    const ADisplayFormat: TWatchDisplayFormat): TWatchValue;
begin
  Result := FValueList[AThreadId, AStackFrame, ADisplayFormat];
end;

function TWatch.GetDisplayFormat: TWatchDisplayFormat;
begin
  Result := FDisplayFormat;
end;

procedure TWatch.SetDisplayFormat(const AValue: TWatchDisplayFormat);
begin
  if AValue = FDisplayFormat then exit;
  FDisplayFormat := AValue;
  DoDisplayFormatChanged;
end;

procedure TWatch.LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
begin
  FEnabled    := AConfig.GetValue(APath + 'Enabled', True);
  FExpression := AConfig.GetValue(APath + 'Expression', '');
  try    ReadStr(AConfig.GetValue(APath + 'DisplayFormat', 'wdfDefault'), FDisplayFormat);
  except FDisplayFormat := wdfDefault; end;

  FValueList.LoadDataFromXMLConfig(AConfig, APath + 'ValueList/');
end;

procedure TWatch.SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string);
var
  s: String;
begin
  AConfig.SetDeleteValue(APath + 'Enabled', FEnabled, True);
  AConfig.SetDeleteValue(APath + 'Expression', FExpression, '');
  WriteStr(s, FDisplayFormat);
  AConfig.SetDeleteValue(APath + 'DisplayFormat', s, 'wdfDefault');

  FValueList.SaveDataToXMLConfig(AConfig, APath + 'ValueList/');
end;

function TWatch.GetExpression: String;
begin
  Result := FExpression;
end;

procedure TWatch.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue
  then begin
    FEnabled := AValue;
    DoEnableChange;
  end;
end;

procedure TWatch.SetExpression(const AValue: String);
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

procedure TCurrentWatch.RequestData(AWatchValue: TCurrentWatchValue);
begin
  if Collection <> nil
  then TCurrentWatches(Collection).RequestData(AWatchValue)
  else AWatchValue.SetValidity(ddsInvalid);
end;

constructor TCurrentWatch.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

destructor TCurrentWatch.Destroy;
begin
  if (TCurrentWatches(Collection) <> nil)
  then TCurrentWatches(Collection).NotifyRemove(Self);
  inherited Destroy;
end;

procedure TCurrentWatch.LoadFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
var
  i: Integer;
begin
  Expression := AConfig.GetValue(APath + 'Expression/Value', '');
  Enabled := AConfig.GetValue(APath + 'Enabled/Value', true);
  i := StringCase
    (AConfig.GetValue(APath + 'DisplayStyle/Value', TWatchDisplayFormatNames[wdfDefault]),
    TWatchDisplayFormatNames);
  if i >= 0
  then DisplayFormat := TWatchDisplayFormat(i)
  else DisplayFormat := wdfDefault;
end;

procedure TCurrentWatch.SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string);
begin
  AConfig.SetDeleteValue(APath + 'Expression/Value', Expression, '');
  AConfig.SetDeleteValue(APath + 'Enabled/Value', Enabled, true);
  AConfig.SetDeleteValue(APath + 'DisplayStyle/Value',
    TWatchDisplayFormatNames[DisplayFormat], TWatchDisplayFormatNames[wdfDefault]);
end;

{ =========================================================================== }
{ TWatches }
{ =========================================================================== }

function TWatches.Add(const AExpression: String): TWatch;
begin
  BeginUpdate;
  Result := TWatch(inherited Add);
  Result.Expression := AExpression;
  EndUpdate;
end;

function TWatches.GetItem(const AnIndex: Integer): TWatch;
begin
  Result := TWatch(inherited Items[AnIndex]);
end;

procedure TWatches.SetItem(const AnIndex: Integer; const AValue: TWatch);
begin
  inherited Items[AnIndex] := AValue;
end;

procedure TWatches.LoadDataFromXMLConfig(const AConfig: TXMLConfig; APath: string);
var
  c, i: Integer;
begin
  Clear;
  c := AConfig.GetValue(APath + 'Count', 0);
  APath := APath + 'Entry';
  for i := 0 to c - 1 do
    Add('').LoadDataFromXMLConfig(AConfig, APath + IntToStr(i) + '/');
end;

procedure TWatches.SaveDataToXMLConfig(const AConfig: TXMLConfig; APath: string);
var
  i: Integer;
begin
  AConfig.SetDeleteValue(APath + 'Count', Count, 0);
  APath := APath + 'Entry';
  for i := 0 to Count - 1 do
    Items[i].SaveDataToXMLConfig(AConfig, APath + IntToStr(i) + '/');
end;

constructor TWatches.Create;
begin
  Create(TWatch);
end;

constructor TWatches.Create(const AWatchClass: TBaseWatchClass);
begin
  inherited Create(AWatchClass);
end;

function TWatches.Find(const AExpression: String): TWatch;
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

procedure TWatches.ClearValues;
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
end;

constructor TCurrentWatches.Create(AMonitor: TWatchesMonitor);
begin
  FMonitor := AMonitor;
  inherited Create(TCurrentWatch);
end;

function TCurrentWatches.Find(const AExpression: String): TCurrentWatch;
begin
  Result := TCurrentWatch(inherited Find(AExpression));
end;

procedure TCurrentWatches.WatchesChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCurrentWatches.SetSnapShot(const AValue: TWatches);
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
  Clear;
  NewCount := AConfig.GetValue(APath + 'Count', 0);
  for i := 0 to NewCount-1 do
  begin
    // Call inherited Add, so NotifyAdd can be send, after the Watch was loaded
    Watch := TCurrentWatch(inherited Add(''));
    Watch.LoadFromXMLConfig(AConfig, Format('%sItem%d/', [APath, i + 1]));
    NotifyAdd(Watch);
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

function TLocals.Count: Integer;
begin
  Result := FLocals.Count;
end;

constructor TLocals.Create;
begin
  FLocals := TStringList.Create;
  inherited Create;
  AddReference;
end;

constructor TLocals.Create(AThreadId, AStackFrame: Integer);
begin
  Create;
  FThreadId := AThreadId;
  FStackFrame := AStackFrame;
end;

constructor TLocals.CreateCopy(const ASource: TLocals);
begin
  Create;
  Assign(ASource);
end;

destructor TLocals.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FLocals);
end;

function TLocals.GetName(const AnIndex: Integer): String;
begin
  Result := FLocals.Names[AnIndex];
end;

function TLocals.GetValue(const AnIndex: Integer): String;
begin
  Result := FLocals[AnIndex];
  Result := GetPart('=', '', Result);
end;

procedure TLocals.LoadDataFromXMLConfig(const AConfig: TXMLConfig; APath: string);
var
  c, i: Integer;
begin
  FLocals.Clear;
  FThreadId := AConfig.GetValue(APath + 'ThreadId', -1);
  FStackFrame := AConfig.GetValue(APath + 'StackFrame', -1);
  c := AConfig.GetValue(APath + 'Count', 0);
  APath := APath + 'Entry';
  for i := 0 to c - 1 do begin
    FLocals.Add(
      AConfig.GetValue(APath + IntToStr(i) + '/Expression', '')
      + '=' +
      AConfig.GetValue(APath + IntToStr(i) + '/Value', '')
    );
  end;
end;

procedure TLocals.SaveDataToXMLConfig(const AConfig: TXMLConfig; APath: string);
var
  i: Integer;
begin
  AConfig.SetValue(APath + 'ThreadId', FThreadId);
  AConfig.SetValue(APath + 'StackFrame', FStackFrame);
  AConfig.SetDeleteValue(APath + 'Count', Count, 0);
  APath := APath + 'Entry';
  for i := 0 to Count - 1 do begin
    AConfig.SetValue(APath + IntToStr(i) + '/Expression', Names[i]);
    AConfig.SetValue(APath + IntToStr(i) + '/Value', Values[i]);
  end;
end;

procedure TLocals.Assign(AnOther: TLocals);
begin
  FThreadId := AnOther.FThreadId;
  FStackFrame := AnOther.FStackFrame;
  FLocals.Assign(AnOther.FLocals);
end;

{ =========================================================================== }
{ TCurrentLocals }
{ =========================================================================== }

procedure TCurrentLocals.SetSnapShot(const AValue: TLocals);
begin
  assert((FSnapShot=nil) or (AValue=nil), 'TCurrentLocals already have snapshot');
  if FSnapShot = AValue then exit;
  FSnapShot := AValue;
  if FSnapShot <> nil
  then FSnapShot.Assign(Self);
end;

constructor TCurrentLocals.Create(AMonitor: TLocalsMonitor; AThreadId, AStackFrame: Integer);
begin
  FMonitor := AMonitor;
  FDataValidity := ddsUnknown;
  FThreadId := AThreadId;
  FStackFrame := AStackFrame;
  inherited Create;
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

procedure TCurrentLocals.Clear;
begin
  FLocals.Clear;
end;

procedure TCurrentLocals.Add(const AName, AValue: String);
begin
  FLocals.Add(AName + '=' + AValue);
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

{ =========================================================================== }
{ TBaseRegisters }
{ =========================================================================== }

function TBaseRegisters.Count: Integer;
begin
  Result := 0;
end;

procedure TBaseRegisters.BeginUpdate;
begin
  inc(FUpdateCount);
  if FUpdateCount = 1 then ChangeUpdating;
end;

procedure TBaseRegisters.EndUpdate;
begin
  dec(FUpdateCount);
  if FUpdateCount = 0 then ChangeUpdating;
end;

constructor TBaseRegisters.Create;
begin
  inherited Create;
end;

function TBaseRegisters.GetFormat(const AnIndex: Integer): TRegisterDisplayFormat;
begin
  Result := rdDefault;
end;

procedure TBaseRegisters.SetFormat(const AnIndex: Integer;
  const AValue: TRegisterDisplayFormat);
begin
  //
end;

procedure TBaseRegisters.ChangeUpdating;
begin
  //
end;

function TBaseRegisters.Updating: Boolean;
begin
  Result := FUpdateCount <> 0;
end;

function TBaseRegisters.GetModified(const AnIndex: Integer): Boolean;
begin
  Result := False;
end;

function TBaseRegisters.GetName(const AnIndex: Integer): String;
begin
  Result := '';
end;

function TBaseRegisters.GetValue(const AnIndex: Integer): String;
begin
  Result := '';
end;

{ =========================================================================== }
{ TIDERegisters }
{ =========================================================================== }

procedure TIDERegisters.AddNotification(const ANotification: TIDERegistersNotification);
begin
  FNotificationList.Add(ANotification);
  ANotification.AddReference;
end;

constructor TIDERegisters.Create;
begin
  FNotificationList := TList.Create;
  inherited Create;
end;

destructor TIDERegisters.Destroy;
var
  n: Integer;
begin
  for n := FNotificationList.Count - 1 downto 0 do
    TDebuggerNotification(FNotificationList[n]).ReleaseReference;

  inherited;

  FreeAndNil(FNotificationList);
end;

procedure TIDERegisters.RegistersChanged(Sender: TObject);
begin
  NotifyChange;
end;

procedure TIDERegisters.SetMaster(const AMaster: TDBGRegisters);
var
  DoNotify: Boolean;
begin
  if FMaster = AMaster then Exit;

  if FMaster <> nil
  then begin
    FMaster.OnChange := nil;
    DoNotify := FMaster.Count <> 0;
  end
  else DoNotify := False;

  FMaster := AMaster;

  if FMaster <> nil
  then begin
    FMaster.OnChange := @RegistersChanged;
    DoNotify := DoNotify or (FMaster.Count <> 0);
  end;

  if DoNotify
  then NotifyChange;
end;

function TIDERegisters.GetModified(const AnIndex: Integer): Boolean;
begin
  if Master = nil
  then Result := inherited GetModified(AnIndex)
  else Result := Master.Modified[AnIndex];
end;

function TIDERegisters.GetName(const AnIndex: Integer): String;
begin
  if Master = nil
  then Result := inherited GetName(AnIndex)
  else Result := Master.Names[AnIndex];
end;

function TIDERegisters.GetValue(const AnIndex: Integer): String;
begin
  if Master = nil
  then Result := inherited GetValue(AnIndex)
  else Result := Master.Values[AnIndex];
end;

function TIDERegisters.GetFormat(const AnIndex: Integer): TRegisterDisplayFormat;
begin
  if Master = nil
  then Result := inherited GetFormat(AnIndex)
  else Result := Master.Formats[AnIndex];
end;

procedure TIDERegisters.SetFormat(const AnIndex: Integer;
  const AValue: TRegisterDisplayFormat);
begin
  if Master = nil
  then inherited SetFormat(AnIndex, AValue)
  else Master.Formats[AnIndex] := AValue;
end;

procedure TIDERegisters.NotifyChange;
var
  n: Integer;
  Notification: TIDERegistersNotification;
begin
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TIDERegistersNotification(FNotificationList[n]);
    if Assigned(Notification.FOnChange)
    then Notification.FOnChange(Self);
  end;
end;

procedure TIDERegisters.RemoveNotification(const ANotification: TIDERegistersNotification);
begin
  FNotificationList.Remove(ANotification);
  ANotification.ReleaseReference;
end;

function TIDERegisters.Count: Integer;
begin
  if Master = nil
  then Result := 0
  else Result := Master.Count;
end;

{ =========================================================================== }
{ TDBGRegisters }
{ =========================================================================== }

function TDBGRegisters.Count: Integer;
begin
  if  (FDebugger <> nil)
  and (FDebugger.State  in [dsPause, dsInternalPause])
  then Result := GetCount
  else Result := 0;
end;

constructor TDBGRegisters.Create(const ADebugger: TDebugger);
begin
  FChanged := False;
  inherited Create;
  FDebugger := ADebugger;
end;

procedure TDBGRegisters.DoChange;
begin
  if Updating then begin
    FChanged := True;
    exit;
  end;
  FChanged := False;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TDBGRegisters.DoStateChange(const AOldState: TDBGState);
begin
end;

procedure TDBGRegisters.Changed;
begin
  DoChange;
end;

function TDBGRegisters.GetCount: Integer;
begin
  Result := 0;
end;

procedure TDBGRegisters.SetFormat(const AnIndex: Integer;
  const AValue: TRegisterDisplayFormat);
begin
  inherited SetFormat(AnIndex, AValue);
  Changed;
end;

procedure TDBGRegisters.ChangeUpdating;
begin
  inherited ChangeUpdating;
  if (not Updating) and FChanged then DoChange;
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

procedure TCallStackEntry.Init(const AnAdress: TDbgPtr;
  const AnArguments: TStrings; const AFunctionName: String;
  const AUnitInfo: TDebuggerUnitInfo; const ALine: Integer; AState: TDebuggerDataState);
begin
  FAdress := AnAdress;
  if AnArguments <> nil
  then FArguments.Assign(AnArguments);
  FFunctionName := AFunctionName;

  SetUnitInfo(AUnitInfo);
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
  FOwner.CurrentIndex := self.Index;
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

function TCallStackEntry.GetSource: String;
begin
  if (FState = ddsValid)  and (FUnitInfo <> nil)
  then Result := FUnitInfo.FileName
  else Result := '';
end;

procedure TCallStackEntry.SetUnitInfo(AUnitInfo: TDebuggerUnitInfo);
begin
  if FUnitInfo <> nil then FUnitInfo.ReleaseReference;
  FUnitInfo := AUnitInfo;
  if FUnitInfo <> nil then FUnitInfo.AddReference;
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
  WriteStr(s, FState);
  AConfig.SetValue(APath + 'State', s);
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
  or (AIndex >= Count) then IndexError(Aindex);

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

function TCallStack.IndexError(AIndex: Integer): TCallStackEntry;
begin
  Result:=nil;
  raise EInvalidOperation.CreateFmt('Index out of range (%d)', [AIndex]);
end;

procedure TCallStack.PrepareRange(AIndex, ACount: Integer);
begin
end;

procedure TCallStack.ChangeCurrentIndex(ANewIndex: Integer);
begin
  CurrentIndex := ANewIndex;
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


{ =========================================================================== }
{ TCallStackMonitor }
{ =========================================================================== }

procedure TCallStackMonitor.AddNotification(const ANotification: TCallStackNotification);
begin
  FNotificationList.Add(ANotification);
end;

constructor TCallStackMonitor.Create;
begin
  FNotificationList := TDebuggerChangeNotificationList.Create;
  FCurrentCallStackList := TCurrentCallStackList.Create(Self);
  inherited Create;
end;

destructor TCallStackMonitor.Destroy;
begin
  FNotificationList.Clear;
  inherited;
  FreeAndNil(FNotificationList);
  FreeAndNil(FCurrentCallStackList);
end;

procedure TCallStackMonitor.SetSupplier(const AValue: TCallStackSupplier);
begin
  inherited Supplier := AValue;
end;

procedure TCallStackMonitor.RequestCount(ACallstack: TCallStack);
begin
  if (Supplier <> nil) and (ACallstack is TCurrentCallStack)
  then Supplier.RequestCount(TCurrentCallStack(ACallstack));
end;

procedure TCallStackMonitor.RequestCurrent(ACallstack: TCallStack);
begin
  if (Supplier <> nil) and (ACallstack is TCurrentCallStack)
  then Supplier.RequestCurrent(TCurrentCallStack(ACallstack));
end;

procedure TCallStackMonitor.RequestEntries(ACallstack: TCallStack);
begin
  if (Supplier <> nil) and (ACallstack is TCurrentCallStack)
  then Supplier.RequestEntries(TCurrentCallStack(ACallstack));
end;

procedure TCallStackMonitor.UpdateCurrentIndex;
begin
  {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataMonitor: TCallStackMonitor.UpdateCurrentIndex']); {$ENDIF}
  if Supplier <> nil then Supplier.UpdateCurrentIndex;
  NotifyCurrent;
end;

procedure TCallStackMonitor.DoNewSupplier;
begin
  inherited DoNewSupplier;
  NotifyChange;
end;

procedure TCallStackMonitor.CallStackClear(Sender: TObject);
begin
  {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataMonitor: TCallStackMonitor.CallStackClear']); {$ENDIF}
  // Don't clear, set it to 0 so there are no entries shown
  //SetCount(0);
  NotifyChange;
end;

function TCallStackMonitor.GetSnapshot(AnID: Pointer): TCallStackList;
begin
  Result := TCallStackList(GetSnapshotObj(AnID));
end;

function TCallStackMonitor.GetSupplier: TCallStackSupplier;
begin
  Result := TCallStackSupplier(inherited Supplier);
end;

procedure TCallStackMonitor.NotifyChange;
begin
  FNotificationList.NotifyChange(Self);
end;

procedure TCallStackMonitor.NotifyCurrent;
begin
  FNotificationList.NotifyCurrent(Self);
end;

function TCallStackMonitor.CreateSnapshot(CreateEmpty: Boolean = False): TObject;
begin
  Result := TCallStackList.Create;
  if not CreateEmpty
  then CurrentCallStackList.SnapShot := TCallStackList(Result);
end;

procedure TCallStackMonitor.RemoveNotification(const ANotification: TCallStackNotification);
begin
  FNotificationList.Remove(ANotification);
end;


{ =========================================================================== }
{ TCallStackSupplier }
{ =========================================================================== }

procedure TCallStackSupplier.Changed;
begin
  {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataMonitor: TCallStackSupplier.Changed']); {$ENDIF}
  Monitor.NotifyChange;
end;

function TCallStackSupplier.GetMonitor: TCallStackMonitor;
begin
  Result := TCallStackMonitor(inherited Monitor);
end;

function TCallStackSupplier.GetCurrentCallStackList: TCurrentCallStackList;
begin
  if Monitor <> nil
  then Result := Monitor.CurrentCallStackList
  else Result := nil;
end;

procedure TCallStackSupplier.SetMonitor(const AValue: TCallStackMonitor);
begin
  inherited Monitor := AValue;
end;

procedure TCallStackSupplier.RequestCount(ACallstack: TCurrentCallStack);
begin
  ACallstack.SetCountValidity(ddsInvalid);
end;

procedure TCallStackSupplier.RequestCurrent(ACallstack: TCurrentCallStack);
begin
  ACallstack.SetCurrentValidity(ddsInvalid);
end;

procedure TCallStackSupplier.RequestEntries(ACallstack: TCurrentCallStack);
var
  e: TCallStackEntry;
  It: TMapIterator;
begin
  {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataMonitor: TCallStackSupplier.RequestEntries']); {$ENDIF}
  It := TMapIterator.Create(ACallstack.FEntries);

  if not It.Locate(ACallstack.LowestUnknown )
  then if not It.EOM
  then It.Next;

  while (not IT.EOM) and (TCallStackEntry(It.DataPtr^).Index < ACallstack.HighestUnknown)
  do begin
    e := TCallStackEntry(It.DataPtr^);
    if e.State = ddsRequested then e.State := ddsInvalid;
    It.Next;
  end;
  It.Free;

  if Monitor <> nil
  then Monitor.NotifyChange;
end;

procedure TCallStackSupplier.CurrentChanged;
begin
  {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataMonitor: TCallStackSupplier.CurrentChanged']); {$ENDIF}
  if Monitor <> nil
  then Monitor.NotifyCurrent;
end;

procedure TCallStackSupplier.DoStateChange(const AOldState: TDBGState);
begin
  if (Debugger = nil) or (CurrentCallStackList = nil) then Exit;
  {$IFDEF DBG_DATA_MONITORS} DebugLnEnter(['DebugDataMonitor: >>ENTER: TCallStackSupplier.DoStateChange  New-State=', DBGStateNames[Debugger.State ]]); {$ENDIF}

  if FDebugger.State in [dsPause, dsInternalPause]
  then begin
    CurrentCallStackList.Clear;
    Changed;
  end
  else begin
    CurrentCallStackList.SnapShot := nil;

    if (AOldState in [dsPause, dsInternalPause]) or (AOldState = dsNone) { Force clear on initialisation }
    then begin
      CurrentCallStackList.Clear;
      Monitor.CallStackClear(Self);
    end;
  end;
  {$IFDEF DBG_DATA_MONITORS} DebugLnExit(['DebugDataMonitor: <<EXIT: TCallStackSupplier.DoStateChange']); {$ENDIF}
end;

procedure TCallStackSupplier.UpdateCurrentIndex;
begin
  //
end;

(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   S I G N A L S  and  E X C E P T I O N S                                **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

{ =========================================================================== }
{ TBaseSignal }
{ =========================================================================== }

procedure TBaseSignal.AssignTo(Dest: TPersistent);
begin
  if Dest is TBaseSignal
  then begin
    TBaseSignal(Dest).Name := FName;
    TBaseSignal(Dest).ID := FID;
    TBaseSignal(Dest).HandledByDebugger := FHandledByDebugger;
    TBaseSignal(Dest).ResumeHandled := FResumeHandled;
  end
  else inherited AssignTo(Dest);
end;

constructor TBaseSignal.Create(ACollection: TCollection);
begin
  FID := 0;
  FHandledByDebugger := False;
  FResumeHandled := True;
  inherited Create(ACollection);
end;

procedure TBaseSignal.SetHandledByDebugger(const AValue: Boolean);
begin
  if AValue = FHandledByDebugger then Exit;
  FHandledByDebugger := AValue;
  Changed;
end;

procedure TBaseSignal.SetID (const AValue: Integer );
begin
  if FID = AValue then Exit;
  FID := AValue;
  Changed;
end;

procedure TBaseSignal.SetName (const AValue: String );
begin
  if FName = AValue then Exit;
  FName := AValue;
  Changed;
end;

procedure TBaseSignal.SetResumeHandled(const AValue: Boolean);
begin
  if FResumeHandled = AValue then Exit;
  FResumeHandled := AValue;
  Changed;
end;

{ =========================================================================== }
{ TDBGSignal }
{ =========================================================================== }

function TDBGSignal.GetDebugger: TDebugger;
begin
  Result := TDBGSignals(Collection).FDebugger;
end;

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
{ TBaseSignals }
{ =========================================================================== }

function TBaseSignals.Add (const AName: String; AID: Integer ): TBaseSignal;
begin
  Result := TBaseSignal(inherited Add);
  Result.BeginUpdate;
  try
    Result.Name := AName;
    Result.ID := AID;
  finally
    Result.EndUpdate;
  end;
end;

constructor TBaseSignals.Create (const AItemClass: TBaseSignalClass );
begin
  inherited Create(AItemClass);
end;

procedure TBaseSignals.Reset;
begin
  Clear;
end;

function TBaseSignals.Find(const AName: String): TBaseSignal;
var
  n: Integer;
  S: String;
begin
  S := UpperCase(AName);
  for n := 0 to Count - 1 do
  begin
    Result := TBaseSignal(GetItem(n));
    if UpperCase(Result.Name) = S
    then Exit;
  end;
  Result := nil;
end;

{ =========================================================================== }
{ TDBGSignals }
{ =========================================================================== }

function TDBGSignals.Add(const AName: String; AID: Integer): TDBGSignal;
begin
  Result := TDBGSignal(inherited Add(AName, AID));
end;

constructor TDBGSignals.Create(const ADebugger: TDebugger;
                               const ASignalClass: TDBGSignalClass);
begin
  FDebugger := ADebugger;
  inherited Create(ASignalClass);
end;

function TDBGSignals.Find(const AName: String): TDBGSignal;
begin
  Result := TDBGSignal(inherited Find(ANAme));
end;

function TDBGSignals.GetItem(const AIndex: Integer): TDBGSignal;
begin
  Result := TDBGSignal(inherited GetItem(AIndex));
end;

procedure TDBGSignals.SetItem(const AIndex: Integer; const AValue: TDBGSignal);
begin
  inherited SetItem(AIndex, AValue);
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
{ TBaseException }
{ =========================================================================== }

procedure TBaseException.AssignTo(Dest: TPersistent);
begin
  if Dest is TBaseException
  then begin
    TBaseException(Dest).Name := FName;
  end
  else inherited AssignTo(Dest);
end;

constructor TBaseException.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

procedure TBaseException.LoadFromXMLConfig(const AXMLConfig: TXMLConfig;
  const APath: string);
begin
  FName:=AXMLConfig.GetValue(APath+'Name/Value','');
end;

procedure TBaseException.SaveToXMLConfig(const AXMLConfig: TXMLConfig;
  const APath: string);
begin
  AXMLConfig.SetDeleteValue(APath+'Name/Value',FName,'');
end;

procedure TBaseException.SetName(const AValue: String);
begin
  if FName = AValue then exit;

  if TBaseExceptions(GetOwner).Find(AValue) <> nil
  then raise EDBGExceptions.Create('Duplicate name: ' + AValue);

  FName := AValue;
  Changed;
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
  inherited LoadFromXMLConfig(AXMLConfig, APath);
  FEnabled:=AXMLConfig.GetValue(APath+'Enabled/Value',true);
end;

procedure TIDEException.SaveToXMLConfig(const AXMLConfig: TXMLConfig;
  const APath: string);
begin
  inherited SaveToXMLConfig(AXMLConfig, APath);
  AXMLConfig.SetDeleteValue(APath+'Enabled/Value',FEnabled,true);
end;

procedure TIDEException.ResetMaster;
begin
  FMaster := nil;
end;

procedure TIDEException.DoChanged;
var
  E: TDBGExceptions;
begin
  E := TIDEExceptions(Collection).FMaster;
  if ((FMaster = nil) = Enabled) and (E <> nil)
  then begin
    if Enabled then
    begin
      FMaster := E.Find(Name);
      if FMaster = nil then
        FMaster := E.Add(Name);
    end
    else FreeAndNil(FMaster);
  end;

  inherited DoChanged;
end;

procedure TIDEException.SetEnabled(const AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  Changed;
end;

{ =========================================================================== }
{ TBaseExceptions }
{ =========================================================================== }

function TBaseExceptions.Add(const AName: String): TBaseException;
begin
  Result := TBaseException(inherited Add);
  Result.Name := AName;
end;

constructor TBaseExceptions.Create(const AItemClass: TBaseExceptionClass);
begin
  inherited Create(AItemClass);
  FIgnoreAll := False;
end;

destructor TBaseExceptions.Destroy;
begin
  ClearExceptions;
  inherited Destroy;
end;

procedure TBaseExceptions.Reset;
begin
  ClearExceptions;
  FIgnoreAll := False;
end;

function TBaseExceptions.Find(const AName: String): TBaseException;
var
  n: Integer;
  S: String;
begin
  S := UpperCase(AName);
  for n := 0 to Count - 1 do
  begin
    Result := TBaseException(GetItem(n));
    if UpperCase(Result.Name) = S
    then Exit;
  end;
  Result := nil;
end;

procedure TBaseExceptions.ClearExceptions;
begin
  while Count>0 do
    TBaseException(GetItem(Count-1)).Free;
end;

procedure TBaseExceptions.SetIgnoreAll(const AValue: Boolean);
begin
  if FIgnoreAll = AValue then exit;
  FIgnoreAll := AValue;
  Changed;
end;

procedure TBaseExceptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TBaseExceptions
  then begin
    TBaseExceptions(Dest).IgnoreAll := IgnoreAll;
  end
  else inherited AssignTo(Dest);
end;

{ =========================================================================== }
{ TDBGExceptions }
{ =========================================================================== }

function TDBGExceptions.Add(const AName: String): TDBGException;
begin
  Result := TDBGException(inherited Add(AName));
end;

constructor TDBGExceptions.Create(const ADebugger: TDebugger; const AExceptionClass: TDBGExceptionClass);
begin
  FDebugger := ADebugger;
  inherited Create(AExceptionClass);
end;

function TDBGExceptions.Find(const AName: String): TDBGException;
begin
  Result := TDBGException(inherited Find(AName));
end;

function TDBGExceptions.GetItem(const AIndex: Integer): TDBGException;
begin
  Result := TDBGException(inherited GetItem(AIndex));
end;

procedure TDBGExceptions.SetItem(const AIndex: Integer; const AValue: TDBGException);
begin
  inherited SetItem(AIndex, AValue);
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
  FMaster := nil;
  inherited Create(TIDEException);
  AddDefault;
end;

procedure TIDEExceptions.SetMaster(const AValue: TDBGExceptions);
var
  n: Integer;
  Item: TIDEException;
begin
  if FMaster = AValue then Exit;
  Assert((FMaster=nil) or (AValue=nil), 'TManagedExceptions already has a Master');
  FMaster := AValue;
  if FMaster = nil
  then begin
    for n := 0 to Count - 1 do
      Items[n].ResetMaster;
  end
  else begin
    // Do not assign, add only enabled exceptions
    for n := 0 to Count - 1 do
    begin
      Item := Items[n];
      if Item.Enabled and (FMaster.Find(Item.Name) = nil)
      then FMaster.Add(Item.Name);
    end;
    FMaster.IgnoreAll := IgnoreAll;
  end;
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

procedure DoFinalization;
var
  n: Integer;
begin
  if MDebuggerPropertiesList <> nil
  then begin
    for n := 0 to MDebuggerPropertiesList.Count - 1 do
      MDebuggerPropertiesList.Objects[n].Free;
    FreeAndNil(MDebuggerPropertiesList);
  end;
end;

{ TBaseLineInfo }

function TBaseLineInfo.GetSource(const AnIndex: integer): String;
begin
  Result := '';
end;

function TBaseLineInfo.IndexOf(const ASource: String): integer;
begin
  Result := -1;
end;

constructor TBaseLineInfo.Create;
begin
  inherited Create;
end;

function TBaseLineInfo.GetAddress(const AIndex: Integer; const ALine: Integer): TDbgPtr;
begin
  Result := 0;
end;

function TBaseLineInfo.GetAddress(const ASource: String; const ALine: Integer): TDbgPtr;
var
  idx: Integer;
begin
  idx := IndexOf(ASource);
  if idx = -1
  then Result := 0
  else Result := GetAddress(idx, ALine);
end;

function TBaseLineInfo.GetInfo(AAdress: TDbgPtr; out ASource, ALine, AOffset: Integer): Boolean;
begin
  Result := False;
end;

procedure TBaseLineInfo.Request(const ASource: String);
begin
end;

function TBaseLineInfo.Count: Integer;
begin
  Result := 0;
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

{ TDBGLineInfo }

procedure TDBGLineInfo.Changed(ASource: String);
begin
  DoChange(ASource);
end;

procedure TDBGLineInfo.DoChange(ASource: String);
begin
  if Assigned(FOnChange) then FOnChange(Self, ASource);
end;

procedure TDBGLineInfo.DoStateChange(const AOldState: TDBGState);
begin
end;

constructor TDBGLineInfo.Create(const ADebugger: TDebugger);
begin
  inherited Create;
  FDebugger := ADebugger;
end;

{ TBaseDisassembler }

function TBaseDisassembler.IndexError(AIndex: Integer): TCallStackEntry;
begin
  Result:=nil;
  raise EInvalidOperation.CreateFmt('Index out of range (%d)', [AIndex]);
end;

function TBaseDisassembler.GetEntryPtr(AIndex: Integer): PDisassemblerEntry;
begin
  if (AIndex < -FCountBefore)
  or (AIndex >= FCountAfter) then IndexError(Aindex);

  Result := InternalGetEntryPtr(AIndex);
end;

function TBaseDisassembler.GetEntry(AIndex: Integer): TDisassemblerEntry;
begin
  if (AIndex < -FCountBefore)
  or (AIndex >= FCountAfter) then IndexError(Aindex);

  Result := InternalGetEntry(AIndex);
end;

function TBaseDisassembler.InternalGetEntry(AIndex: Integer): TDisassemblerEntry;
begin
  Result.Addr := 0;
  Result.Offset := 0;
  Result.SrcFileLine := 0;
  Result.SrcStatementIndex := 0;
  Result.SrcStatementCount := 0;
end;

function TBaseDisassembler.InternalGetEntryPtr(AIndex: Integer): PDisassemblerEntry;
begin
  Result := nil;
end;

procedure TBaseDisassembler.DoChanged;
begin
  // nothing
end;

procedure TBaseDisassembler.Changed;
begin
  if FChangedLockCount > 0
  then begin
    FIsChanged := True;
    exit;
  end;
  FIsChanged := False;
  DoChanged;
end;

procedure TBaseDisassembler.LockChanged;
begin
  inc(FChangedLockCount);
end;

procedure TBaseDisassembler.UnlockChanged;
begin
  dec(FChangedLockCount);
  if FIsChanged and (FChangedLockCount = 0)
  then Changed;
end;

procedure TBaseDisassembler.InternalIncreaseCountBefore(ACount: Integer);
begin
  // increase count withou change notification
  if ACount < FCountBefore
  then begin
    {$IFDEF DBG_VERBOSE}
    debugln(['WARNING: TBaseDisassembler.InternalIncreaseCountBefore will decrease was ', FCountBefore , ' new=',ACount]);
    {$ENDIF}
    SetCountBefore(ACount);
  end
  else FCountBefore := ACount;
end;

procedure TBaseDisassembler.InternalIncreaseCountAfter(ACount: Integer);
begin
  // increase count withou change notification
  if ACount < FCountAfter
  then begin
    {$IFDEF DBG_VERBOSE}
    debugln(['WARNING: TBaseDisassembler.InternalIncreaseCountAfter will decrease was ', FCountAfter , ' new=',ACount]);
    {$ENDIF}
    SetCountAfter(ACount)
  end
  else FCountAfter := ACount;
end;

procedure TBaseDisassembler.SetCountBefore(ACount: Integer);
begin
  if FCountBefore = ACount
  then exit;
  FCountBefore := ACount;
  Changed;
end;

procedure TBaseDisassembler.SetCountAfter(ACount: Integer);
begin
  if FCountAfter = ACount
  then exit;
  FCountAfter := ACount;
  Changed;
end;

procedure TBaseDisassembler.SetBaseAddr(AnAddr: TDbgPtr);
begin
  if FBaseAddr = AnAddr
  then exit;
  FBaseAddr := AnAddr;
  Changed;
end;

constructor TBaseDisassembler.Create;
begin
  Clear;
  FChangedLockCount := 0;
end;

destructor TBaseDisassembler.Destroy;
begin
  inherited Destroy;
  Clear;
end;

procedure TBaseDisassembler.Clear;
begin
  FCountAfter := 0;
  FCountBefore := 0;
  FBaseAddr := 0;
end;

function TBaseDisassembler.PrepareRange(AnAddr: TDbgPtr; ALinesBefore,
  ALinesAfter: Integer): Boolean;
begin
  Result := False;
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

{ TDBGDisassemblerEntryRange }

function TDBGDisassemblerEntryRange.GetEntry(Index: Integer): TDisassemblerEntry;
begin
  if (Index < 0) or (Index >= FCount)
  then raise Exception.Create('Illegal Index');
  Result := FEntries[Index];
end;

function TDBGDisassemblerEntryRange.GetCapacity: Integer;
begin
  Result := length(FEntries);
end;

function TDBGDisassemblerEntryRange.GetEntryPtr(Index: Integer): PDisassemblerEntry;
begin
  if (Index < 0) or (Index >= FCount)
  then raise Exception.Create('Illegal Index');
  Result := @FEntries[Index];
end;

procedure TDBGDisassemblerEntryRange.SetCapacity(const AValue: Integer);
begin
  SetLength(FEntries, AValue);
  if FCount >= AValue
  then FCount := AValue - 1;
end;

procedure TDBGDisassemblerEntryRange.SetCount(const AValue: Integer);
begin
  if FCount = AValue then exit;
  if AValue >= Capacity
  then Capacity := AValue + Max(20, AValue div 4);

  FCount := AValue;
end;

procedure TDBGDisassemblerEntryRange.Clear;
begin
  SetCapacity(0);
  FCount := 0;
end;

function TDBGDisassemblerEntryRange.Append(const AnEntryPtr: PDisassemblerEntry): Integer;
begin
  if FCount >= Capacity
  then Capacity := FCount + Max(20, FCount div 4);

  FEntries[FCount] := AnEntryPtr^;
  Result := FCount;
  inc(FCount);
end;

procedure TDBGDisassemblerEntryRange.Merge(const AnotherRange: TDBGDisassemblerEntryRange);
var
  i, j: Integer;
  a: TDBGPtr;
begin
  if AnotherRange.RangeStartAddr < RangeStartAddr then
  begin
    // merge before
    i := AnotherRange.Count - 1;
    a := FirstAddr;
    while (i >= 0) and (AnotherRange.EntriesPtr[i]^.Addr >= a)
    do dec(i);
    inc(i);
    {$IFDEF DBG_VERBOSE}
    debugln(['INFO: TDBGDisassemblerEntryRange.Merge: Merged to START:   Other=', dbgs(AnotherRange), '  To other index=', i, ' INTO self=', dbgs(self) ]);
    {$ENDIF}
    if Capacity < Count + i
    then Capacity := Count + i;
    for j := Count-1 downto 0 do
      FEntries[j+i] := FEntries[j];
    for j := 0 to i - 1 do
      FEntries[j] := AnotherRange.FEntries[j];
    FCount := FCount + i;
    FRangeStartAddr := AnotherRange.FRangeStartAddr;
  end
  else begin
    // merge after
    a:= LastAddr;
    i := 0;
    while (i < AnotherRange.Count) and (AnotherRange.EntriesPtr[i]^.Addr <= a)
    do inc(i);
    {$IFDEF DBG_VERBOSE}
    debugln(['INFO: TDBGDisassemblerEntryRange.Merge to END:   Other=', dbgs(AnotherRange), '  From other index=', i, ' INTO self=', dbgs(self) ]);
    {$ENDIF}
    if Capacity < Count + AnotherRange.Count - i
    then Capacity := Count + AnotherRange.Count - i;
    for j := 0 to AnotherRange.Count - i - 1 do
      FEntries[Count + j] := AnotherRange.FEntries[i + j];
    FCount := FCount + AnotherRange.Count - i;
    FRangeEndAddr := AnotherRange.FRangeEndAddr;
    FLastEntryEndAddr := AnotherRange.FLastEntryEndAddr;
  end;
  {$IFDEF DBG_VERBOSE}
  debugln(['INFO: TDBGDisassemblerEntryRange.Merge AFTER MERGE: ', dbgs(self) ]);
  {$ENDIF}
end;

function TDBGDisassemblerEntryRange.FirstAddr: TDbgPtr;
begin
  if FCount = 0
  then exit(0);
  Result := FEntries[0].Addr;
end;

function TDBGDisassemblerEntryRange.LastAddr: TDbgPtr;
begin
  if FCount = 0
  then exit(0);
  Result := FEntries[FCount-1].Addr;
end;

function TDBGDisassemblerEntryRange.ContainsAddr(const AnAddr: TDbgPtr;
  IncludeNextAddr: Boolean = False): Boolean;
begin
  if IncludeNextAddr
  then  Result := (AnAddr >= RangeStartAddr) and (AnAddr <= RangeEndAddr)
  else  Result := (AnAddr >= RangeStartAddr) and (AnAddr < RangeEndAddr);
end;

function TDBGDisassemblerEntryRange.IndexOfAddr(const AnAddr: TDbgPtr): Integer;
begin
  Result := FCount - 1;
  while Result >= 0 do begin
    if FEntries[Result].Addr = AnAddr
    then exit;
    dec(Result);
  end;
end;

function TDBGDisassemblerEntryRange.IndexOfAddrWithOffs(const AnAddr: TDbgPtr): Integer;
var
  O: Integer;
begin
  Result := IndexOfAddrWithOffs(AnAddr, O);
end;

function TDBGDisassemblerEntryRange.IndexOfAddrWithOffs(const AnAddr: TDbgPtr; out
  AOffs: Integer): Integer;
begin
  Result := FCount - 1;
  while Result >= 0 do begin
    if FEntries[Result].Addr <= AnAddr
    then break;
    dec(Result);
  end;
  If Result < 0
  then AOffs := 0
  else AOffs := AnAddr - FEntries[Result].Addr;
end;

{ TDBGDisassemblerEntryMapIterator }

function TDBGDisassemblerEntryMapIterator.GetRangeForAddr(AnAddr: TDbgPtr;
  IncludeNextAddr: Boolean): TDBGDisassemblerEntryRange;
begin
  Result := nil;
  if not Locate(AnAddr)
  then if not BOM
  then Previous;

  if BOM
  then exit;

  GetData(Result);
  if not Result.ContainsAddr(AnAddr, IncludeNextAddr)
  then Result := nil;
end;

function TDBGDisassemblerEntryMapIterator.NextRange: TDBGDisassemblerEntryRange;
begin
  Result := nil;
  if EOM
  then exit;

  Next;
  if not EOM
  then GetData(Result);
end;

function TDBGDisassemblerEntryMapIterator.PreviousRange: TDBGDisassemblerEntryRange;
begin
  Result := nil;
  if BOM
  then exit;

  Previous;
  if not BOM
  then GetData(Result);
end;

{ TDBGDisassemblerEntryMap }

procedure TDBGDisassemblerEntryMap.ReleaseData(ADataPtr: Pointer);
type
  PDBGDisassemblerEntryRange = ^TDBGDisassemblerEntryRange;
begin
  if FFreeItemLock
  then exit;
  if Assigned(FOnDelete)
  then FOnDelete(PDBGDisassemblerEntryRange(ADataPtr)^);
  PDBGDisassemblerEntryRange(ADataPtr)^.Free;
end;

constructor TDBGDisassemblerEntryMap.Create(AIdType: TMapIdType; ADataSize: Cardinal);
begin
  inherited;
  FIterator := TDBGDisassemblerEntryMapIterator.Create(Self);
end;

destructor TDBGDisassemblerEntryMap.Destroy;
begin
  FreeAndNil(FIterator);
  inherited Destroy;
end;

procedure TDBGDisassemblerEntryMap.AddRange(const ARange: TDBGDisassemblerEntryRange);
var
  MergeRng, MergeRng2: TDBGDisassemblerEntryRange;
  OldId: TDBGPtr;
begin
  {$IFDEF DBG_VERBOSE}
  debugln(['INFO: TDBGDisassemblerEntryMap.AddRange ', dbgs(ARange), ' to map with count=', Count ]);
  {$ENDIF}
  if ARange.Count = 0 then exit;

  MergeRng := GetRangeForAddr(ARange.RangeStartAddr, True);
  if MergeRng <> nil then begin
    // merge to end ( ARange.RangeStartAddr >= MergeRng.RangeStartAddr )
    // MergeRng keeps it's ID;
    MergeRng.Merge(ARange);
    if assigned(FOnMerge)
    then FOnMerge(MergeRng, ARange);
    ARange.Free;

    MergeRng2 := GetRangeForAddr(MergeRng.RangeEndAddr, True);
    if (MergeRng2 <> nil) and (MergeRng2 <> MergeRng) then begin
      // MergeRng is located before MergeRng2
      // MergeRng2 merges to end of MergeRng ( No ID changes )
      MergeRng.Merge(MergeRng2);
      if assigned(FOnMerge)
      then FOnMerge(MergeRng, MergeRng2);
      Delete(MergeRng2.RangeStartAddr);
    end;
    exit;
  end;

  MergeRng := GetRangeForAddr(ARange.RangeEndAddr, True);
  if MergeRng <> nil then begin
    // merge to start ( ARange.RangeEndAddr is in MergeRng )
    if MergeRng.ContainsAddr(ARange.RangeStartAddr)
    then begin
      debugln(['ERROR: New Range is completely inside existing ', dbgs(MergeRng)]);
      exit;
    end;
    // MergeRng changes ID
    OldId := MergeRng.RangeStartAddr;
    MergeRng.Merge(ARange);
    if assigned(FOnMerge)
    then FOnMerge(ARange, MergeRng);
    FFreeItemLock := True; // prevent destruction of MergeRng
    Delete(OldId);
    FFreeItemLock := False;
    Add(MergeRng.RangeStartAddr, MergeRng);
    ARange.Free;
    exit;
  end;

  Add(ARange.RangeStartAddr, ARange);
end;

function TDBGDisassemblerEntryMap.GetRangeForAddr(AnAddr: TDbgPtr;
  IncludeNextAddr: Boolean = False): TDBGDisassemblerEntryRange;
begin
  Result := FIterator.GetRangeForAddr(AnAddr, IncludeNextAddr);
end;

{ TDBGDisassembler }

procedure TDBGDisassembler.EntryRangesOnDelete(Sender: TObject);
begin
  if FCurrentRange <> Sender
  then exit;
  LockChanged;
  FCurrentRange := nil;
  SetBaseAddr(0);
  SetCountBefore(0);
  SetCountAfter(0);
  UnlockChanged;
end;

procedure TDBGDisassembler.EntryRangesOnMerge(MergeReceiver,
  MergeGiver: TDBGDisassemblerEntryRange);
var
  i: LongInt;
  lb, la: Integer;
begin
  // no need to call changed, will be done by whoever triggered this
  if FCurrentRange = MergeGiver
  then FCurrentRange := MergeReceiver;

  if FCurrentRange = MergeReceiver
  then begin
    i := FCurrentRange.IndexOfAddrWithOffs(BaseAddr);
    if i >= 0
    then begin
      InternalIncreaseCountBefore(i);
      InternalIncreaseCountAfter(FCurrentRange.Count - 1 - i);
      exit;
    end
    else if FCurrentRange.ContainsAddr(BaseAddr)
    then begin
      {$IFDEF DBG_VERBOSE}
      debugln(['WARNING: TDBGDisassembler.OnMerge: Address at odd offset ',BaseAddr, ' before=',CountBefore, ' after=', CountAfter]);
      {$ENDIF}
      lb := CountBefore;
      la := CountAfter;
      if HandleRangeWithInvalidAddr(FCurrentRange, BaseAddr, lb, la)
      then begin
        InternalIncreaseCountBefore(lb);
        InternalIncreaseCountAfter(la);
        exit;
      end;
    end;

    LockChanged;
    SetBaseAddr(0);
    SetCountBefore(0);
    SetCountAfter(0);
    UnlockChanged;
  end;
end;

function TDBGDisassembler.FindRange(AnAddr: TDbgPtr; ALinesBefore,
  ALinesAfter: Integer): Boolean;
var
  i: LongInt;
  NewRange: TDBGDisassemblerEntryRange;
begin
  LockChanged;
  try
    Result := False;
    NewRange := FEntryRanges.GetRangeForAddr(AnAddr);

    if (NewRange <> nil)
    and ( (NewRange.RangeStartAddr > AnAddr) or (NewRange.RangeEndAddr < AnAddr) )
    then
      NewRange := nil;

    if NewRange = nil
    then begin
      {$IFDEF DBG_VERBOSE}
      debugln(['INFO: TDBGDisassembler.FindRange: Address not found ', AnAddr, ' wanted-before=',ALinesBefore,' wanted-after=',ALinesAfter,' in map with count=', FEntryRanges.Count ]);
      {$ENDIF}
      exit;
    end;

    i := NewRange.IndexOfAddr(AnAddr);
    if i < 0
    then begin
      // address at incorrect offset
      Result := HandleRangeWithInvalidAddr(NewRange, AnAddr, ALinesBefore, ALinesAfter);
      {$IFDEF DBG_VERBOSE}
      debugln(['WARNING: TDBGDisassembler.FindRange: Address at odd offset ',AnAddr,'  Result=', dbgs(result), ' before=',CountBefore, ' after=', CountAfter, ' wanted-before=',ALinesBefore,' wanted-after=',ALinesAfter,' in map with count=', FEntryRanges.Count]);
      {$ENDIF}
      if Result
      then begin
        FCurrentRange := NewRange;
        SetBaseAddr(AnAddr);
        SetCountBefore(ALinesBefore);
        SetCountAfter(ALinesAfter);
      end;
      exit;
    end;

    FCurrentRange := NewRange;
    SetBaseAddr(AnAddr);
    SetCountBefore(i);
    SetCountAfter(NewRange.Count - 1 - i);
    Result := (i >= ALinesBefore) and (CountAfter >= ALinesAfter);
    {$IFDEF DBG_VERBOSE}
    debugln(['INFO: TDBGDisassembler.FindRange: Address found ',AnAddr,' Result=', dbgs(result), ' before=',CountBefore, ' after=', CountAfter, ' wanted-before=',ALinesBefore,' wanted-after=',ALinesAfter,' in map with count=', FEntryRanges.Count]);
    {$ENDIF}
  finally
    UnlockChanged;
  end;
end;

procedure TDBGDisassembler.DoChanged;
begin
  inherited DoChanged;
  if assigned(FOnChange)
  then FOnChange(Self);
end;

procedure TDBGDisassembler.Clear;
begin
  {$IFDEF DBG_VERBOSE}
  debugln(['INFO: TDBGDisassembler.Clear:  map had count=', FEntryRanges.Count ]);
  {$ENDIF}
  FCurrentRange := nil;
  FEntryRanges.Clear;
  inherited Clear;
  Changed;
end;

procedure TDBGDisassembler.DoStateChange(const AOldState: TDBGState);
begin
  if FDebugger.State = dsPause
  then begin
    Changed;
  end
  else begin
    if (AOldState = dsPause) or (AOldState = dsNone) { Force clear on initialisation }
    then Clear;
  end;
end;

function TDBGDisassembler.InternalGetEntry(AIndex: Integer): TDisassemblerEntry;
begin
  Result := FCurrentRange.Entries[AIndex + CountBefore];
end;

function TDBGDisassembler.InternalGetEntryPtr(AIndex: Integer): PDisassemblerEntry;
begin
  Result := FCurrentRange.EntriesPtr[AIndex + CountBefore];
end;

function TDBGDisassembler.PrepareEntries(AnAddr: TDbgPtr; ALinesBefore,
  ALinesAfter: Integer): Boolean;
begin
  Result := False;
end;

function TDBGDisassembler.HandleRangeWithInvalidAddr(ARange: TDBGDisassemblerEntryRange;
  AnAddr: TDbgPtr; var ALinesBefore, ALinesAfter: Integer): boolean;
begin
  Result := False;
  if ARange <> nil then
    FEntryRanges.Delete(ARange.RangeStartAddr);
end;

constructor TDBGDisassembler.Create(const ADebugger: TDebugger);
begin
  FDebugger := ADebugger;
  FEntryRanges := TDBGDisassemblerEntryMap.Create(itu8, SizeOf(TDBGDisassemblerEntryRange));
  FEntryRanges.OnDelete   := @EntryRangesOnDelete;
  FEntryRanges.OnMerge   := @EntryRangesOnMerge;
  inherited Create;
end;

destructor TDBGDisassembler.Destroy;
begin
  inherited Destroy;
  FEntryRanges.OnDelete := nil;
  Clear;
  FreeAndNil(FEntryRanges);
end;

function TDBGDisassembler.PrepareRange(AnAddr: TDbgPtr; ALinesBefore,
  ALinesAfter: Integer): Boolean;
begin
  Result := False;
  if (Debugger = nil) or (Debugger.State <> dsPause) or (AnAddr = 0)
  then exit;
  if (ALinesBefore < 0) or (ALinesAfter < 0)
  then raise Exception.Create('invalid PrepareRange request');

  // Do not LockChange, if FindRange changes something, then notification must be send to syncronize counts on IDE-object
  Result:= FindRange(AnAddr, ALinesBefore, ALinesAfter);
  {$IFDEF DBG_VERBOSE}
  if result then debugln(['INFO: TDBGDisassembler.PrepareRange  found existing data  Addr=', AnAddr,' before=', ALinesBefore, ' After=', ALinesAfter ]);
  {$ENDIF}
  if Result
  then exit;

  {$IFDEF DBG_VERBOSE}
  if result then debugln(['INFO: TDBGDisassembler.PrepareRange  calling PrepareEntries Addr=', AnAddr,' before=', ALinesBefore, ' After=', ALinesAfter ]);
  {$ENDIF}
  if PrepareEntries(AnAddr, ALinesBefore, ALinesAfter)
  then Result:= FindRange(AnAddr, ALinesBefore, ALinesAfter);
  {$IFDEF DBG_VERBOSE}
  if result then debugln(['INFO: TDBGDisassembler.PrepareRange  found data AFTER PrepareEntries Addr=', AnAddr,' before=', ALinesBefore, ' After=', ALinesAfter ]);
  {$ENDIF}
end;

initialization
  MDebuggerPropertiesList := nil;

finalization
  DoFinalization;


end.
