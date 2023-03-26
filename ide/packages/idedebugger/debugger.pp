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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit Debugger;

{$mode objfpc}{$H+}

{$IFDEF linux} {$DEFINE DBG_ENABLE_TERMINAL} {$ENDIF}

interface

uses
  TypInfo, Classes, SysUtils, math, Types, fgl,
  // LazUtils
  Laz2_XMLCfg, LazFileUtils, LazStringUtils, LazUtilities, LazLoggerBase,
  LazClasses, Maps, LazMethodList, laz2_XMLWrite,
  // DebuggerIntf
  DbgIntfBaseTypes, DbgIntfMiscClasses, DbgIntfDebuggerBase, LazDebuggerIntf,
  LazDebuggerIntfBaseTypes, LazDebuggerValueConverter, LazDebuggerTemplate,
  IdeDebuggerBase, IdeDebuggerWatchResult, IdeDebuggerOpts,
  IdeDebuggerBackendValueConv, IdeDebuggerUtils;

const
  XMLBreakPointsNode = 'BreakPoints';
  XMLBreakPointGroupsNode = 'BreakPointGroups';
  XMLWatchesNode = 'Watches';
  XMLExceptionsNode = 'Exceptions';

type

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
    FSrcLine: Integer;
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
    property SrcLine: Integer read FSrcLine write FSrcLine;
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
                                                      // functions have result type at end, after ",,"
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
  TIdeWatch = class;
  TIdeWatches = class;
  TCurrentWatch = class;
  TCurrentWatches = class;
  TIdeWatchesMonitor = class;
  TIdeLocalsMonitor = class;
  TCurrentLocals = class;
  TIDELineInfo = class;
  TIdeCallStack = class;
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

  TIDEBreakPoint = class(TIDEBreakPointBase)
  private
    FLogEvalExpression: String;
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
    procedure SetKind(const AValue: TDBGBreakPointKind);
    function  DebugExeLine: Integer; virtual;  // Same as line, but in Subclass: the line in the compiled exe

    procedure DisableGroups;
    procedure DoActionChange; virtual;
    procedure DoHit(const ACount: Integer; var AContinue, ANeedInternalPause: Boolean); override;
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
    function Add(const ASource: String; const ALine: Integer; AnUpdating: Boolean = False): TIDEBreakPoint; overload; reintroduce;
    function Add(const AAddress: TDBGPtr; AnUpdating: Boolean = False): TIDEBreakPoint; overload; reintroduce;
    function Add(const AData: String; const AScope: TDBGWatchPointScope;
                 const AKind: TDBGWatchPointKind; AnUpdating: Boolean = False): TIDEBreakPoint; overload; reintroduce;
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
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string; const ALegacyList: Boolean;
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
      const Path: string; const ALegacyList: Boolean); virtual;
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
     'wdfMemDump', 'wdfBinary'
    );

type

  TWatchesEvent =
       procedure(const ASender: TIdeWatches; const AWatch: TIdeWatch) of object;

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

  { TIdeWatchValue }

  TIdeWatchValue = class(TWatchValue, IWatchAbleResultIntf)
  private
    function GetChildrenByNameAsArrayEntry(AName: Int64): TObject; // TIdeWatch;
    function GetChildrenByNameAsField(AName, AClassName: String): TObject; // TIdeWatch;
    function GetWatch: TIdeWatch;
    function GetEnabled: Boolean;
  protected
    function GetTypeInfo: TDBGType; override;
    function GetValue: String; override;
    function GetResultData: TWatchResultData; override;
    function GetValidity: TDebuggerDataState; override;

    procedure RequestData; virtual;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                              const APath: string);
  public
    constructor Create(AOwnerWatch: TWatch); override;
    constructor Create(AOwnerWatch: TIdeWatch;
                       const AThreadId: Integer;
                       const AStackFrame: Integer
                      );
    procedure Assign(AnOther: TWatchValue); override;
    property Watch: TIdeWatch read GetWatch;

    function ExpressionForChildField(AName: String; AClassName: String = ''): String;
    function ExpressionForChildEntry(AnIndex: Int64): String;
    function ExpressionForChildEntry(AnIndex: String): String;

    function MaybeCopyResult(ASourceWatch: TIdeWatch): boolean;

    property ChildrenByNameAsField[AName, AClassName: String]: TObject read GetChildrenByNameAsField;
    property ChildrenByNameAsArrayEntry[AName: Int64]: TObject read GetChildrenByNameAsArrayEntry;
  end;

  { TIdeWatchValueList }

  TIdeWatchValueList = class(TWatchValueList)
  private
    function GetEntry(const AThreadId: Integer; const AStackFrame: Integer): TIdeWatchValue;
    function GetEntryByIdx(AnIndex: integer): TIdeWatchValue;
    function GetWatch: TIdeWatch;
  protected
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                              APath: string);
  public
    constructor Create(AOwnerWatch: TIdeWatch);
    property EntriesByIdx[AnIndex: integer]: TIdeWatchValue read GetEntryByIdx;
    property Entries[const AThreadId: Integer; const AStackFrame: Integer]: TIdeWatchValue
             read GetEntry; default;
    property Watch: TIdeWatch read GetWatch;
  end;

  { TIdeWatch }

  TIdeWatch = class(TWatch, IWatchAbleDataIntf, IFreeNotifyingIntf)
  private
    FChildWatches: TIdeWatches;
    FDisplayName: String;
    FParentWatch: TIdeWatch;

    function GetChildWatch(ADispName, AnExpr: String): TIdeWatch;
    function GetChildrenByNameAsArrayEntry(AName: Int64): TIdeWatch;
    function GetChildrenByNameAsField(AName, AClassName: String): TIdeWatch;
    function GetTopParentWatch: TIdeWatch;
    function GetValue(const AThreadId: Integer; const AStackFrame: Integer): TIdeWatchValue;
    function GetAnyValidParentWatchValue(AThreadId: Integer; AStackFrame: Integer): TIdeWatchValue;
    function GetWatchDisplayName: String;
    procedure SetDisplayName(AValue: String); reintroduce;
    function GetEnabled: Boolean;
    function GetExpression: String;
  protected
    procedure InitChildWatches;
    function CreateChildWatches: TIdeWatches; virtual;
    procedure SetParentWatch(AValue: TIdeWatch); virtual;
    procedure AssignTo(Dest: TPersistent); override;

    function CreateValueList: TWatchValueList; override;
    procedure DoEnableChange; override;
    procedure DoExpressionChange; override;
    procedure DoDisplayFormatChanged; override;
  protected
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                              const APath: string);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure ClearValues; override;

    procedure BeginChildUpdate;
    procedure EndChildUpdate;
    procedure LimitChildWatchCount(AMaxCnt: Integer; AKeepIndexEntriesBelow: Int64 = low(Int64));
    property ChildrenByNameAsField[AName, AClassName: String]: TIdeWatch read GetChildrenByNameAsField;
    property ChildrenByNameAsArrayEntry[AName: Int64]: TIdeWatch read GetChildrenByNameAsArrayEntry;
    function HasAllValidParents(AThreadId: Integer; AStackFrame: Integer): boolean;
    property ParentWatch: TIdeWatch read FParentWatch;
    property TopParentWatch: TIdeWatch read GetTopParentWatch;
    property DisplayName: String read GetWatchDisplayName write SetDisplayName;
  public
    property Values[const AThreadId: Integer; const AStackFrame: Integer]: TIdeWatchValue
             read GetValue;
  end;

  { TIdeWatches }

  TIdeWatches = class(TWatches)
  private
    //FParentWatches: TIdeWatches;

    function GetItem(const AnIndex: Integer): TIdeWatch;
    procedure SetItem(const AnIndex: Integer; const AValue: TIdeWatch);
  protected
    function WatchClass: TWatchClass; override;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                              APath: string);
  public
{$IfOpt C+}
    function Add: TCollectionItem; reintroduce;
{$EndIF}
    function Add(const AExpression: String): TIdeWatch; virtual;
    function Find(const AExpression: String): TIdeWatch; reintroduce;
    property Items[const AnIndex: Integer]: TIdeWatch read GetItem write SetItem; default;
  end;

  { TCurrentResData }

  TCurrentResData = class(TObject, IDbgWatchDataIntf)
  private type
    TCurrentResDataFlag = (
      crfDone, crfWasDone, crfSubDataCreated,
      crfFreeResData, crfFreeErrResData,
      crfIsArrayEntry, crfArrayProtoSet,
      crfIsAnchestor
    );
    TCurrentResDataFlags = set of TCurrentResDataFlag;
    TCurrentResDataList = specialize TFPGObjectList<TCurrentResData>;
  private
    FNewResultData: TWatchResultDataEx;
    FStoredResultData, FStoredErrorResultData: TWatchResultDataEx;

    FSubCurrentData,       // Deref, Array-Element, PChar(in PCharOrString)
    FSubCurrentDataSecond, // String(in PCharOrString)
    FAnchestorCurrentData,
    FOwnerCurrentData: TCurrentResData;
    FCurrentFields: TCurrentResDataList;
    FFLags: TCurrentResDataFlags;
    FCurrentIdx, FArrayCount: Integer;

    procedure BeforeCreateValue; inline; // Before creating any non-error
    procedure BeforeCreateError; inline;
    procedure AfterDataCreated; virtual;
    procedure AfterSubDataCreated(ASubData: TCurrentResData);
    procedure FinishCurrentArrayElement;
    function  InternalPCharShouldBeStringValue(APCharResult: TCurrentResData): IDbgWatchDataIntf;
    procedure WriteFieldsToRes(AStartIdx: Integer; AClassResData: TWatchResultDataEx);

    function  CreateSubCurrentResData: TCurrentResData; inline;
    procedure InitSubCurrentResData(ANewCurData: TCurrentResData); inline;
    procedure MarkResDataAsUsedByOwner;
    procedure FreeResultAndSubData;
    procedure FreeResultAndSubDataAndDestroy;
  public
    destructor Destroy; override;
    procedure Done;
    function  RootResultData: TCurrentResData;
    property  NewResultData: TWatchResultDataEx read FNewResultData;

    procedure DebugPrint(AText: String);
  public
    {%region ***** IDbgWatchDataIntf ***** }
    procedure CreatePrePrinted(AVal: String);  virtual; // ATypes: TLzDbgWatchDataTypes);
    procedure CreateString(AVal: String);  virtual;// AnEncoding // "pchar data"
    procedure CreateWideString(AVal: WideString); virtual;
    procedure CreateCharValue(ACharValue: QWord; AByteSize: Integer = 0); virtual;
    procedure CreateNumValue(ANumValue: QWord; ASigned: Boolean; AByteSize: Integer = 0); virtual;
    procedure CreatePointerValue(AnAddrValue: TDbgPtr); virtual;
    procedure CreateFloatValue(AFloatValue: Extended; APrecission: TLzDbgFloatPrecission); virtual;
    function  CreateProcedure(AVal: TDBGPtr; AnIsFunction: Boolean; ALoc, ADesc: String): IDbgWatchDataIntf;
    function  CreateProcedureRef(AVal: TDBGPtr; AnIsFunction: Boolean; ALoc, ADesc: String): IDbgWatchDataIntf;
    function  CreateArrayValue(AnArrayType: TLzDbgArrayType;
                               ATotalCount: Integer = 0;
                               ALowIdx: Integer = 0
                              ): IDbgWatchDataIntf; virtual;
    procedure CreateBoolValue(AnOrdBoolValue: QWord; AByteSize: Integer = 0);
    procedure CreateEnumValue(ANumValue: QWord; AName: String; AByteSize: Integer = 0; AnIsEnumIdent: Boolean = False);
//    //procedure CreateEnumValue(ANumValue: QWord; const ANames: TStringDynArray; const AOrdValues: TIntegerDynArray);
    procedure CreateSetValue(const ANames: TStringDynArray);
    //procedure CreateSetValue(const ASetVal: TLzDbgSetData; const ANames: TStringDynArray); //; const AOrdValues: array of Integer);
    function CreateVariantValue(AName: String = ''; AVisibility: TLzDbgFieldVisibility = dfvUnknown): IDbgWatchDataIntf;
    procedure CreateStructure(AStructType: TLzDbgStructType;
                              ADataAddress: TDBGPtr = 0
                              //AOwnFieldCount: Integer = 0;    // Fields declared in this structure (no anchestors)
                              //ARecurseFieldCount: Integer = 0 // Fields including anchestors
                             );
    function CreateValueHandlerResult(AValueHandler: ILazDbgValueConverterIntf): IDbgWatchDataIntf;

    procedure CreateError(AVal: String); virtual;

    function  SetPCharShouldBeStringValue: IDbgWatchDataIntf;
    procedure SetTypeName(ATypeName: String);

    function  SetDerefData: IDbgWatchDataIntf;
    procedure SetDataAddress(AnAddr: TDbgPtr);
    function  SetNextArrayData: IDbgWatchDataIntf;

    function  SetAnchestor(ATypeName: String): IDbgWatchDataIntf;
    function  AddField(AFieldName: String;
                       AVisibility: TLzDbgFieldVisibility;
                       AFlags: TLzDbgFieldFlags
//                       AnAnchestor: IDbgWatchDataIntf  // nil => unknown
                      ): IDbgWatchDataIntf;
    {%endregion ***** IDbgWatchDataIntf ***** }
  end;

  { TCurrentWatchValue }

  TCurrentWatchValue = class(specialize TDbgDataRequestTemplateBase<TIdeWatchValue, IDbgWatchValueIntf>, IDbgWatchValueIntf)
  private
    FCurrentResData: TCurrentResData;
    FCurrentBackEndExpression: String;
//    FUpdateCount: Integer;
    FDbgBackendConverter: TIdeDbgValueConvertSelector;

  protected
  (* IDbgWatchValueIntf *)
    procedure DoBeginUpdating; override;
    procedure DoEndUpdating; override;
    function ResData: IDbgWatchDataIntf;
    function GetDbgValConverter: ILazDbgValueConvertSelectorIntf;
  private
    FOnValidityChanged: TNotifyEvent;
    FSnapShot: TIdeWatchValue;
    procedure SetSnapShot(const AValue: TIdeWatchValue);
  protected
    procedure SetValue(AValue: String); override; // TODO: => one per DisplayFormat???
    procedure SetWatch(AValue: TWatch); override;
    function GetBackendExpression: String; reintroduce;
    function GetValidity: TDebuggerDataState; override;
    procedure RequestData; override;
    procedure CancelRequestData;
    procedure DoDataValidityChanged({%H-}AnOldValidity: TDebuggerDataState); override;
    function IDbgWatchValueIntf.GetExpression = GetBackendExpression;
  public
    destructor Destroy; override;
    property SnapShot: TIdeWatchValue read FSnapShot write SetSnapShot;
    property OnValidityChanged: TNotifyEvent read FOnValidityChanged write FOnValidityChanged;
  end;

  { TCurrentWatchValueList }

  TCurrentWatchValueList = class(TIdeWatchValueList)
  private
    FSnapShot: TIdeWatchValueList;
    procedure SetSnapShot(const AValue: TIdeWatchValueList);
  protected
    function CreateEntry(const AThreadId: Integer; const AStackFrame: Integer): TIdeWatchValue; override;
    property SnapShot: TIdeWatchValueList read FSnapShot write SetSnapShot;
  public
    procedure Clear; override;
  end;

  { TCurrentWatch }

  TCurrentWatch = class(TIdeWatch)
  private
    FSnapShot: TIdeWatch;
    FAdded: Boolean;
    procedure SetSnapShot(const AValue: TIdeWatch);
  protected
    function CreateChildWatches: TIdeWatches; override;
    procedure SetParentWatch(AValue: TIdeWatch); override;
    function CreateValueList: TWatchValueList; override;
    procedure DoChanged; override;
    procedure DoModified; override;
    procedure RequestData(AWatchValue: TCurrentWatchValue);
    property SnapShot: TIdeWatch read FSnapShot write SetSnapShot;
  public
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
    FSkipUpdatedNotification: integer;
    procedure SetSnapShot(const AValue: TIdeWatches);
    procedure WatchesChanged(Sender: TObject);
  protected
    function GetItem(const AnIndex: Integer): TCurrentWatch;
    procedure SetItem(const AnIndex: Integer; const AValue: TCurrentWatch);
  protected
    function WatchClass: TWatchClass; override;
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
    function Add(const AExpression: String): TCurrentWatch; override;
    function Find(const AExpression: String): TCurrentWatch; reintroduce;
    // IDE
    procedure LoadFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
    procedure SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string;
      const ALegacyList: Boolean);
  public
    property Items[const AnIndex: Integer]: TCurrentWatch read GetItem
                                                      write SetItem; default;
  end;

  { TIdeWatchesMonitor }

  TIdeWatchesMonitor = class(TWatchesMonitor)
  private
    FWatches: TWatches;
    FSnapshots: TDebuggerDataSnapShotList;
    FOnModified: TNotifyEvent;
    FOnWatchesInvalidated: TNotifyEvent;
    FIgnoreModified: Integer;
    FNotificationList: TWatchesNotificationList;
    function GetCurrentWatches: TCurrentWatches;
    function GetSnapshot(AnID: Pointer): TIdeWatches;
  protected
    procedure DoStateEnterPause; override;
    procedure DoStateLeavePause; override;
    procedure DoStateLeavePauseClean; override;
    procedure InvalidateWatchValues; override;
    //procedure NotifyChange
    procedure NotifyAdd(const AWatches: TCurrentWatches; const AWatch: TCurrentWatch);
    procedure NotifyRemove(const AWatches: TCurrentWatches; const AWatch: TCurrentWatch);
    procedure NotifyUpdate(const AWatches: TCurrentWatches; const AWatch: TCurrentWatch);
    procedure RequestData(AWatchValue: TCurrentWatchValue);
    function CreateWatches: TWatches; virtual;
    function CreateSnapshot(CreateEmpty: Boolean = False): TObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotification(const ANotification: TWatchesNotification);
    procedure RemoveNotification(const ANotification: TWatchesNotification);
    procedure NewSnapshot(AnID: Pointer; CreateEmpty: Boolean = False);
    procedure RemoveSnapshot(AnID: Pointer);
    property Watches: TWatches read FWatches;
    property CurrentWatches: TCurrentWatches read GetCurrentWatches;// FCurrentWatches;
    property Snapshots[AnID: Pointer]: TIdeWatches read GetSnapshot;
  public
    procedure Clear;
    procedure DoModified; override;
    procedure LoadFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
    procedure SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string;
      const ALegacyList: Boolean);

    procedure BeginIgnoreModified;
    procedure EndIgnoreModified;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;       // user-modified / xml-storable data modified
    property OnWatchesInvalidated: TNotifyEvent read FOnWatchesInvalidated write FOnWatchesInvalidated;       // user-modified / xml-storable data modified
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

  TSubLocals = class;

  { TIdeLocalsValue }

  TIdeLocalsValue = class(TLocalsValue, IWatchAbleResultIntf, IWatchAbleDataIntf, IFreeNotifyingIntf)
  private
    FSubLocals: TSubLocals;
    FDisplayName: String;
  private
    // TWatchAble
    procedure LimitChildWatchCount(AMaxCnt: Integer; AKeepIndexEntriesBelow: Int64 = low(Int64)); virtual;
    procedure ClearDisplayData;  // Clear any cached display-data / keep only what's needed for the snapshot

    function GetDisplayName: String;
    function GetExpression: String;
    function GetEnabled: Boolean;
    function GetValidity: TDebuggerDataState; virtual;
    function GetDisplayFormat: TWatchDisplayFormat;
    function GetTypeInfo: TDBGType; deprecated;

    function GetChildrenByNameAsArrayEntry(AName: Int64): TObject;
    function GetChildrenByNameAsField(AName, AClassName: String): TObject;

  private
    procedure CreateSubLocals; virtual;
    function GetSubLocal(ADispName, AnExpr: String): TIdeLocalsValue;
  protected
    procedure SetDisplayName(AValue: String); virtual;
    procedure DoAssign(AnOther: TDbgEntityValue); override;
  public
    destructor Destroy; override;
    property DisplayName: String read GetDisplayName write SetDisplayName;
  end;

  { TIDELocals }

  TIDELocals = class(TLocals)
  protected
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                              APath: string);
    function CreateEntry: TDbgEntityValue; override;
  public
    constructor CreateFromXMLConfig(const AConfig: TXMLConfig; APath: string);
    procedure SetDataValidity({%H-}AValidity: TDebuggerDataState); override;
  end;

  { THistoryLocalValue }

  THistoryLocalValue = class(TIdeLocalsValue)
  protected
    FSnapShot: TIdeLocalsValue;
    procedure SetSnapShot(const AValue: TIdeLocalsValue); virtual;
    procedure CreateSubLocals; override;
    procedure SetDisplayName(AValue: String); override;
  end;

  { THistoryLocals }

  THistoryLocals = class(TIDELocals)
  protected
    FSnapShot: TIDELocals;
    procedure SetSnapShot(const AValue: TIDELocals); virtual;
  end;

  { TSubLocalsValue }

  TSubLocalsValue = class(specialize TDbgDataRequestTemplateBase<THistoryLocalValue, IDbgWatchValueIntf>, IDbgWatchValueIntf)
  private
    FValidity: TDebuggerDataState;
    FCurrentResData: TCurrentResData;
  private
    FOnChange: TNotifyEvent;
    // IDbgWatchValueIntf
    function GetEvaluateFlags: TWatcheEvaluateFlags;
    function GetDbgValConverter: ILazDbgValueConvertSelectorIntf;
    function GetFirstIndexOffs: Int64;
    function GetRepeatCount: Integer;
    function GetValidity: TDebuggerDataState; override;
    procedure SetTypeInfo(AValue: TDBGTypeBase);
    procedure SetValidity(AValue: TDebuggerDataState);
    procedure SetValue(AValue: String);

    function ResData: IDbgWatchDataIntf; // new ResData for debugger to fill in
    procedure DoBeginUpdating; override;
    procedure DoEndUpdating; override;
    procedure RequestData;

  protected
    function GetResultData: TWatchResultData; override;
    function GetValue: String; override;
  public
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TSubLocals }

  TSubLocals = class(THistoryLocals)
  private
    FOwnerLocals: TIDELocals;
  protected
    function CreateEntry: TDbgEntityValue; override;
    function Add(const AName: String): TIdeLocalsValue; overload;
    procedure SetSnapShot(const AValue: TIDELocals); override;
    function TopOwner: TIDELocals;
  public
    constructor Create(AThreadId, AStackFrame: Integer; AOwnerLocals: TIDELocals);
  end;

  TCurrentLocalValue = class(THistoryLocalValue)
  end;

  { TCurrentLocals }

  TCurrentLocals = class(specialize TDbgDataRequestTemplateBase<THistoryLocals, IDbgLocalsListIntf>, IDbgLocalsListIntf)
  private
    FMonitor: TIdeLocalsMonitor;
    FDataValidity: TDebuggerDataState;
  private
  (* IDbgLocalsListIntf *)
    FCurrentResName: String;
    FCurrentResData: TCurrentResData;
    FCurrentResList: TRefCntObjList;
    FCurrentValidity: TDebuggerDataState;
    function GetStackFrame: Integer;
    function GetThreadId: Integer;
    procedure DoBeginUpdating; override;
    procedure DoEndUpdating; override;
    procedure SetValidity(AValue: TDebuggerDataState);
    function Add(AName: String): IDbgWatchDataIntf; overload;
  protected
    procedure FinishCurrentRes(AnInUpdate: Boolean = False);
    function CreateEntry: TDbgEntityValue; override;
    procedure SetSnapShot(const AValue: TIDELocals); override;
    property SnapShot: TIDELocals read FSnapShot write SetSnapShot;
  public
    constructor Create(AMonitor: TIdeLocalsMonitor; AThreadId, AStackFrame: Integer);
    destructor Destroy; override;
    function Count: Integer; override;
    procedure Add(AnEntry: TDbgEntityValue); override;
    function Add(const AName: String; AValue: TWatchResultData): TLocalsValue; override; overload;
    procedure SetDataValidity(AValidity: TDebuggerDataState); override;
    property Validity: TDebuggerDataState read FDataValidity;
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
    FLocalsList: TLocalsList;
    FSnapshots: TDebuggerDataSnapShotList;
    FNotificationList: TDebuggerChangeNotificationList;
    function GetCurrentLocalsList: TCurrentLocalsList;
    function GetSnapshot(AnID: Pointer): TIDELocalsList;
  protected
    procedure DoStateEnterPause; override;
    procedure DoStateLeavePause; override;
    procedure DoStateLeavePauseClean; override;
    procedure InvalidateLocalValues; override;
    procedure NotifyChange(ALocals: TCurrentLocals);
    procedure DoNewSupplier; override;
    procedure RequestData(ALocals: TCurrentLocals);
    function CreateSnapshot(CreateEmpty: Boolean = False): TObject;
    function CreateLocalsList: TLocalsList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddNotification(const ANotification: TLocalsNotification);
    procedure RemoveNotification(const ANotification: TLocalsNotification);
    procedure NewSnapshot(AnID: Pointer; CreateEmpty: Boolean = False);
    procedure RemoveSnapshot(AnID: Pointer);
    property  LocalsList: TLocalsList read FLocalsList;
    property  CurrentLocalsList: TCurrentLocalsList read GetCurrentLocalsList;
    property  Snapshots[AnID: Pointer]: TIDELocalsList read GetSnapshot;
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
    function HasAddress(const AIndex: Integer; const ALine: Integer): Boolean; override;
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
    procedure DoDisplayFormatChanged({%H-}AnOldFormat: TRegisterDisplayFormat); override;
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
    procedure InvalidateItems;
    procedure AddNotification(const ANotification: TRegistersNotification);
    procedure RemoveNotification(const ANotification: TRegistersNotification);
    property  CurrentRegistersList: TCurrentIDERegistersList read GetCurrentRegistersList;
    //property  Snapshots[AnID: Pointer]: TIDERegistersList read GetSnapshot;
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

  { TIdeCallStackEntry }

  TIdeCallStackEntry = class(TCallStackEntry)
  private
    FOwner: TIdeCallStack;
    FUnitInfo: TDebuggerUnitInfo;
    procedure SetUnitInfo(AUnitInfo: TDebuggerUnitInfo);
  protected
    function GetUnitInfoProvider: TDebuggerUnitInfoProvider; virtual;
  protected
    function GetFunctionName: String; override;
    function GetSource: String; override;

    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                    const APath: string;
                                    AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                   );
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                                  APath: string;
                                  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                 );
  public
    constructor Create(const AIndex:Integer; const AnAdress: TDbgPtr;
                       const AnArguments: TStrings; const AFunctionName: String;
                       const AUnitInfo: TDebuggerUnitInfo;
                       const ALine: Integer; AState: TDebuggerDataState = ddsValid); overload;
    function CreateCopy: TCallStackEntry; override;
    procedure Assign(AnOther: TCallStackEntry); override;
    destructor Destroy; override;
    procedure Init(const AnAdress: TDbgPtr;
                   const AnArguments: TStrings; const AFunctionName: String;
                   const AUnitName, AClassName, AProcName, AFunctionArgs: String;
                   const ALine: Integer; AState: TDebuggerDataState = ddsValid); override;
    procedure Init(const AnAdress: TDbgPtr;
                   const AnArguments: TStrings; const AFunctionName: String;
                   const FileName, FullName: String;
                   const ALine: Integer; AState: TDebuggerDataState = ddsValid); override;
    procedure ClearLocation; override; // TODO need a way to call Changed on TCallStack or TThreads // corrently done in SetThreadState
    function IsCurrent: Boolean;
    procedure MakeCurrent;
    property UnitInfo: TDebuggerUnitInfo read FUnitInfo;
  end;

  { TIdeCallStack }

  TIdeCallStack = class(TCallStackBase)
  private
    FList: TList;
  protected
    function IndexError(AIndex: Integer): TIdeCallStackEntry;

    function GetEntryBase(AIndex: Integer): TCallStackEntry; override;
    function GetRawEntries: TMap; override;
    function GetNewCurrentIndex: Integer; override;

    procedure Clear; virtual;
    function GetCountValidity: TDebuggerDataState; virtual;
    function  GetCount: Integer; override;
    procedure SetCount({%H-}ACount: Integer); override;
    function  GetEntry(AIndex: Integer): TIdeCallStackEntry; virtual;
    procedure AddEntry(AnEntry: TIdeCallStackEntry); virtual; // must be added in correct order
    procedure AssignEntriesTo(AnOther: TIdeCallStack); virtual;
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
    procedure SetCountValidity({%H-}AValidity: TDebuggerDataState); override;
    procedure SetHasAtLeastCountInfo({%H-}AValidity: TDebuggerDataState; {%H-}AMinCount: Integer = - 1);
      override;
    procedure SetCurrentValidity({%H-}AValidity: TDebuggerDataState); override;
  public
    constructor Create;
    function CreateCopy: TCallStackBase; override;
    destructor Destroy; override;
    procedure Assign(AnOther: TCallStackBase); override;
    procedure PrepareRange({%H-}AIndex, {%H-}ACount: Integer); override;
    procedure ChangeCurrentIndex(ANewIndex: Integer); virtual;
    function HasAtLeastCount(ARequiredMinCount: Integer): TNullableBool; virtual; // Can be faster than getting the full count
    function CountLimited(ALimit: Integer): Integer; override;
    property Entries[AIndex: Integer]: TIdeCallStackEntry read GetEntry;
    property CountValidity: TDebuggerDataState read GetCountValidity;
  end;

  { TCallStackList }

  TIdeCallStackList = class(TCallStackList)
  private
    function GetEntry(const AIndex: Integer): TIdeCallStack;
    function GetEntryForThread(const AThreadId: Integer): TIdeCallStack;
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
    property Entries[const AIndex: Integer]: TIdeCallStack read GetEntry; default;
    property EntriesForThreads[const AThreadId: Integer]: TIdeCallStack read GetEntryForThread;
  end;

  { TCurrentCallStack }

  TCurrentCallStack = class(TIdeCallStack)
  private
    FMonitor: TIdeCallStackMonitor;
    FCountValidity, FAtLeastCountValidity: TDebuggerDataState;
    FCurrentValidity: TDebuggerDataState;
    FNewCurrentIndex: Integer;
    FPreparing: Boolean;
    FSnapShot: TIdeCallStack;
    FEntries: TMap;        // list of created entries
    FCount, FAtLeastCount, FAtLeastCountOld: Integer;
    FLowestUnknown, FHighestUnknown: Integer;
    procedure SetSnapShot(const AValue: TIdeCallStack);
  protected
    function  GetCurrent: Integer; override;
    procedure SetCurrent(AValue: Integer); override;

    procedure Clear; override;
    function  GetCount: Integer; override;
    function GetCountValidity: TDebuggerDataState; override;
    procedure SetCount(ACount: Integer); override;
    function GetEntry(AIndex: Integer): TIdeCallStackEntry; override;
    procedure AddEntry(AnEntry: TIdeCallStackEntry); override;
    procedure AssignEntriesTo(AnOther: TIdeCallStack); override;
    function GetRawEntries: TMap; override;
    function GetLowestUnknown: Integer; override;
    function GetHighestUnknown: Integer; override;
    function GetNewCurrentIndex: Integer; override;
  public
    constructor Create(AMonitor: TIdeCallStackMonitor);
    destructor Destroy; override;
    procedure Assign(AnOther: TCallStackBase); override;
    procedure PrepareRange(AIndex, ACount: Integer); override;
    procedure ChangeCurrentIndex(ANewIndex: Integer); override;
    procedure DoEntriesCreated; override;
    procedure DoEntriesUpdated; override;
    function HasAtLeastCount(ARequiredMinCount: Integer): TNullableBool; override;
    property NewCurrentIndex: Integer read FNewCurrentIndex;
    property SnapShot: TIdeCallStack read FSnapShot write SetSnapShot;
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
    function NewEntryForThread(const AThreadId: Integer): TCallStackBase; override;
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
  protected
    procedure DoStateEnterPause; override;
    procedure DoStateLeavePause; override;
    procedure DoStateLeavePauseClean; override;
    procedure DoModified; override;
    procedure RequestCount(ACallstack: TIdeCallStack);
    procedure RequestAtLeastCount(ACallstack: TIdeCallStack; ARequiredMinCount: Integer);
    procedure RequestCurrent(ACallstack: TIdeCallStack);
    procedure RequestEntries(ACallstack: TIdeCallStack);
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

  TIdeThreadEntry = class;
  TIdeThreads = class;

  { TIdeThreadFrameEntry }

  TIdeThreadFrameEntry = class(TIdeCallStackEntry)
  private
    FThread: TIdeThreadEntry;
  protected
    function GetUnitInfoProvider: TDebuggerUnitInfoProvider; override;
  public
    function CreateCopy: TCallStackEntry; override;
  end;

  { TThreadEntry }

  { TIdeThreadEntry }

  TIdeThreadEntry = class(TThreadEntry)
  private
    FThreadOwner: TIdeThreads;
    function GetTopFrame: TIdeThreadFrameEntry;
  protected
    function CreateStackEntry: TCallStackEntry; override;
    function GetUnitInfoProvider: TDebuggerUnitInfoProvider;
    procedure SetThreadState(AValue: TDbgThreadState); override;

    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig;
                                    const APath: string;
                                    AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                   ); reintroduce;
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig;
                                  const APath: string;
                                  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil
                                 ); reintroduce;
  public
    procedure SetThreadStateOnly(AValue: TDbgThreadState); override;
    function CreateCopy: TThreadEntry; override;
    property TopFrame: TIdeThreadFrameEntry read GetTopFrame;
  end;

  { TIdeThreads }

  TIdeThreads = class(TThreads)
  private
    function GetEntry(const AnIndex: Integer): TIdeThreadEntry;
    function GetEntryById(const AnID: Integer): TIdeThreadEntry;
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
    function CreateEntry(const AnAdress: TDbgPtr;
                       const AnArguments: TStrings; const AFunctionName: String;
                       const FileName, FullName: String;
                       const ALine: Integer;
                       const AThreadId: Integer; const AThreadName: String;
                       const AThreadState: TDbgThreadState;
                       AState: TDebuggerDataState = ddsValid): TThreadEntry; override;
    procedure SetValidity({%H-}AValidity: TDebuggerDataState); override;
    property Entries[const AnIndex: Integer]: TIdeThreadEntry read GetEntry; default;
    property EntryById[const AnID: Integer]: TIdeThreadEntry read GetEntryById;
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
    function CreateEntry(const AnAdress: TDbgPtr;
                       const AnArguments: TStrings; const AFunctionName: String;
                       const FileName, FullName: String;
                       const ALine: Integer;
                       const AThreadId: Integer; const AThreadName: String;
                       const AThreadState: TDbgThreadState;
                       AState: TDebuggerDataState = ddsValid): TThreadEntry; override;
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
    (smrThreads, smrCallStack, smrLocals, smrWatches);

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
                              const APath: string; const ALegacyList: Boolean);
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
    'StepTo',
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
//    'SendSignal'
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
  DBG_VERBOSE, DBG_BREAKPOINT, DBG_DATA_MONITORS, DBG_LOCATION_INFO: PLazLoggerLogGroup;

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

function DBGBreakPointActionNameToAction(const s: string): TIDEBreakPointAction;
begin
  for Result:=Low(TIDEBreakPointAction) to High(TIDEBreakPointAction) do
    if AnsiCompareText(s,DBGBreakPointActionNames[Result])=0 then exit;
  Result:=bpaStop;
end;

{ TIdeThreadFrameEntry }

function TIdeThreadFrameEntry.GetUnitInfoProvider: TDebuggerUnitInfoProvider;
begin
  assert(FThread <> nil, 'FThread <> nil');
  Result := FThread.GetUnitInfoProvider;
end;

function TIdeThreadFrameEntry.CreateCopy: TCallStackEntry;
begin
  Result := TIdeThreadFrameEntry.Create;
  Result.Assign(Self);
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
  if (AGroup = nil) or (IndexOf(AGroup) >= 0) then exit(-1);
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
  Result := FLocationFullFile;
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
  inherited Create;
  FFileName := AFileName;
  FDbgFullName := TrimFilename(AFullFileName);
  FLocationType := dltUnknown;
end;

constructor TDebuggerUnitInfo.Create(const AUnitName, AClassName,
  AFunctionName, AFunctionArgs: String);
begin
  inherited Create;
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
                (FLocationName = AnOther.FLocationName) and
                (FSrcLine = AnOther.FSrcLine);
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
  FSrcLine          := AConfig.GetValue(APath + 'SrcLine', 0);
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
  AConfig.SetValue(APath + 'SrcLine', FSrcLine);
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
  inherited Create;
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
      debugln(DBG_VERBOSE, ['************** Snapshot loaded, but not kept']);
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
      debugln(DBG_VERBOSE, ['************** Snapshot loaded, but not kept']);
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
  XmlConf.WriteFlags := [xwfAllowNullCharsInAttributeValue];
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
      if (not(FCurrentState in [dsPause, dsInternalPause])) or
         (Debugger = nil) or ( (not Debugger.IsIdle) and (not AForce) )
      then exit;
      if CurSnap <> FCurrentSnapshot then exit; // Debugger did "run" in between

      if (k > 0) then begin
        include(FRequestsDone, smrCallStack);
        FCallStack.CurrentCallStackList.EntriesForThreads[i].PrepareRange(0, Min(5, k));
        if (not(FCurrentState in [dsPause, dsInternalPause])) or
           (Debugger = nil) or ( (not Debugger.IsIdle) and (not AForce) )
        then exit;
        if CurSnap <> FCurrentSnapshot then exit; // Debugger did "run" in between
      end
      else
      if AForce then // request re-entry, even if not idle
        FForcedIdle := True;
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

function TIdeLocalsMonitor.GetSnapshot(AnID: Pointer): TIDELocalsList;
begin
  Result := TIDELocalsList(FSnapshots.SnapShot[AnID]);
end;

function TIdeLocalsMonitor.GetCurrentLocalsList: TCurrentLocalsList;
begin
  Result := TCurrentLocalsList(LocalsList);
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

procedure TIdeLocalsMonitor.InvalidateLocalValues;
begin
  inherited InvalidateLocalValues;
  if FLocalsList <> nil then
    FLocalsList.Clear;
end;

procedure TIdeLocalsMonitor.NotifyChange(ALocals: TCurrentLocals);
begin
  if FNotificationList <> nil then
    FNotificationList.NotifyChange(ALocals);
end;

procedure TIdeLocalsMonitor.DoNewSupplier;
begin
  inherited DoNewSupplier;
  NotifyChange(nil);
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
  FLocalsList := CreateLocalsList;
  FLocalsList.AddReference;
  FSnapshots := TDebuggerDataSnapShotList.Create;
  inherited;
  FNotificationList := TDebuggerChangeNotificationList.Create;
end;

destructor TIdeLocalsMonitor.Destroy;
begin
  FSnapshots.Clear;
  FNotificationList.Clear;
  inherited Destroy;
  ReleaseRefAndNil(FLocalsList);
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

{ TCurrentResData }

procedure TCurrentResData.BeforeCreateValue;
begin
  if (FNewResultData <> nil) and (FNewResultData.ValueKind = rdkError) then begin
    assert(crfIsArrayEntry in FFLags, 'TCurrentResData.BeforeCreateValue: crfIsArrayEntry in FFLags');
    assert((FStoredErrorResultData=nil) or (FStoredErrorResultData=FNewResultData), 'TCurrentResData.BeforeCreateValue: (FStoredErrorResultData=nil) or (FStoredErrorResultData=FNewResultData)');
    FStoredErrorResultData := FNewResultData;
    FNewResultData := FStoredResultData
  end;
end;

procedure TCurrentResData.BeforeCreateError;
begin
  if crfIsArrayEntry in FFLags then begin
    if (FNewResultData <> nil) and (FNewResultData.ValueKind <> rdkError) then begin
      assert((FStoredResultData=nil) or (FStoredResultData=FNewResultData), 'TCurrentResData.BeforeCreateError: (FStoredResultData=nil) or (FStoredResultData=FNewResultData)');
      FStoredResultData := FNewResultData;
      FNewResultData := FStoredErrorResultData;
    end;
  end
  else
  if (FNewResultData <> nil) and (FNewResultData.ValueKind <> rdkError) then begin
    FreeResultAndSubData;
  end;
end;

procedure TCurrentResData.AfterDataCreated;
begin
  if crfIsArrayEntry in FFLags then
    Exclude(FFLags, crfDone);
  assert(FFLags * [crfWasDone, crfIsArrayEntry] <> [crfWasDone], 'TCurrentResData.AfterDataCreated: FFLags * [crfWasDone, crfIsArrayEntry] <> [crfWasDone]');
  assert((not (crfIsAnchestor in FFLags)) or (FNewResultData.ValueKind=rdkStruct), 'TCurrentResData.AfterDataCreated: (not (crfIsAnchestor in FFLags)) or (FNewResultData.ValueKind=rdkStruct)');

  if FOwnerCurrentData <> nil then
    FOwnerCurrentData.AfterSubDataCreated(Self);
end;

procedure TCurrentResData.AfterSubDataCreated(ASubData: TCurrentResData);
begin
  assert(FNewResultData <> nil, 'TCurrentResData.AfterSubDataCreated: FNewResultData <> nil');
  Include(FFLags, crfSubDataCreated);
end;

procedure TCurrentResData.FinishCurrentArrayElement;
begin
  assert((FNewResultData<>nil) and (FNewResultData.ValueKind=rdkArray), 'TCurrentResData.FinishCurrentArrayElement: (FNewResultData<>nil) and (FNewResultData.ValueKind=rdkArray)');
  assert((FSubCurrentData<>nil) and (FSubCurrentData is TCurrentResData), 'TCurrentResData.FinishCurrentArrayElement: (FSubCurrentData<>nil) and (FSubCurrentData is TCurrentResData)');

  FSubCurrentData.Done;

  if not (crfSubDataCreated in FFLags) then begin
    // Empty array / No type-info set.
    FNewResultData.SetEntryCount(0);
    assert(FCurrentIdx < 0, 'TCurrentResData.FinishCurrentArrayElement: FCurrentIdx < 0');
    exit;
  end;

  if (not(crfArrayProtoSet in FFLags)) and
     (FSubCurrentData.FNewResultData <> nil)
  then begin
    assert(FCurrentIdx <= 0, 'TCurrentResData.FinishCurrentArrayElement: FCurrentIdx <= 0');
    Include(FFLags, crfArrayProtoSet);

    FNewResultData.SetEntryPrototype(FSubCurrentData.FNewResultData);
  end;

  if (FCurrentIdx = 0) and (FArrayCount > 0) then
    FNewResultData.SetEntryCount(Min(FArrayCount, 1000));


  if FCurrentIdx >= 0 then begin
    if FCurrentIdx >= FNewResultData.Count then begin // XXXXX entrycount
      FNewResultData.SetEntryCount(
        FCurrentIdx +
        Max(50, Min(FNewResultData.Count div 8, 500))
      );
    end;

      FNewResultData.WriteValueToStorage(FCurrentIdx, FSubCurrentData.FNewResultData)
  end;

  Exclude(FFLags, crfSubDataCreated);
end;

function TCurrentResData.InternalPCharShouldBeStringValue(
  APCharResult: TCurrentResData): IDbgWatchDataIntf;
begin
  assert(FNewResultData = nil, 'TCurrentResData.InternalPCharShouldBeStringValue: FNewResultData = nil');

  FNewResultData := TWatchResultDataPCharOrString.Create;
  // AfterDataCreated;

  FSubCurrentData := APCharResult;
  InitSubCurrentResData(FSubCurrentData);

  FSubCurrentDataSecond := CreateSubCurrentResData;
  Result := FSubCurrentDataSecond;
end;

procedure TCurrentResData.WriteFieldsToRes(AStartIdx: Integer;
  AClassResData: TWatchResultDataEx);
var
  i: Integer;
begin
  if FCurrentFields = nil then
    exit;

  FNewResultData.SetFieldCount(FCurrentFields.Count);
  for i := 0 to FCurrentFields.Count - 1 do begin
    AClassResData.SetFieldData(AStartIdx + i, TCurrentResData(FCurrentFields[i]).FNewResultData);
    TCurrentResData(FCurrentFields[i]).Done;
    TCurrentResData(FCurrentFields[i]).MarkResDataAsUsedByOwner;
  end;
end;

function TCurrentResData.CreateSubCurrentResData: TCurrentResData;
begin
  Result := TCurrentResData.Create;
  InitSubCurrentResData(Result);
end;

procedure TCurrentResData.InitSubCurrentResData(ANewCurData: TCurrentResData);
begin
  ANewCurData.FOwnerCurrentData := Self;
  ANewCurData.FFLags := ANewCurData.FFLags +
                        FFLags * [crfIsArrayEntry] +
                        [crfFreeResData, crfFreeErrResData];
  if (FNewResultData <> nil) and (FNewResultData.ValueKind = rdkArray) then
    Include(ANewCurData.FFLags, crfIsArrayEntry);
end;

procedure TCurrentResData.MarkResDataAsUsedByOwner;
begin
  if FNewResultData = nil then
    exit;
  if FNewResultData.ValueKind = rdkError then begin
    Exclude(FFLags, crfFreeErrResData);
    Include(FFLags, crfFreeResData);
  end
  else begin
    Exclude(FFLags, crfFreeResData);
    Include(FFLags, crfFreeErrResData);
  end;
end;

procedure TCurrentResData.FreeResultAndSubData;
begin
  if Self = nil then
    exit;

  if FStoredResultData = FNewResultData then
    FStoredResultData := nil;
  if FStoredErrorResultData = FNewResultData then
    FStoredErrorResultData := nil;

  FSubCurrentData.FreeResultAndSubDataAndDestroy;
  FSubCurrentData := nil;
  FSubCurrentDataSecond.FreeResultAndSubDataAndDestroy;
  FSubCurrentDataSecond := nil;
  FAnchestorCurrentData.FreeResultAndSubDataAndDestroy;
  FAnchestorCurrentData := nil;
  FreeAndNil(FCurrentFields);

  if (FOwnerCurrentData = nil) or (crfFreeResData in FFLags) then
    FreeAndNil(FStoredResultData);
  if (FOwnerCurrentData = nil) or (crfFreeErrResData in FFLags) then
    FreeAndNil(FStoredErrorResultData);

  if ( (FNewResultData <> nil) and
       ( (FNewResultData.ValueKind = rdkError) and (crfFreeErrResData in FFLags) or
         (FNewResultData.ValueKind <> rdkError) and (crfFreeResData in FFLags)
       )
     ) or (FOwnerCurrentData = nil)
  then
  FreeAndNil(FNewResultData);
end;

procedure TCurrentResData.FreeResultAndSubDataAndDestroy;
begin
  if Self = nil then
    exit;
  FreeResultAndSubData;
  Destroy;
end;

destructor TCurrentResData.Destroy;
begin
  if FStoredResultData = FNewResultData then
    FStoredResultData := nil;
  if FStoredErrorResultData = FNewResultData then
    FStoredErrorResultData := nil;

  FSubCurrentData.Free;
  FSubCurrentDataSecond.Free;
  FAnchestorCurrentData.Free;
  FCurrentFields.Free;

  if crfFreeResData in FFLags then
    FreeAndNil(FStoredResultData);
  if crfFreeErrResData in FFLags then
    FreeAndNil(FStoredErrorResultData);

  if (FNewResultData <> nil) and
     ( (FNewResultData.ValueKind = rdkError) and (crfFreeErrResData in FFLags) or
       (FNewResultData.ValueKind <> rdkError) and (crfFreeResData in FFLags)
     )
  then
    FreeAndNil(FNewResultData);

  inherited Destroy;
end;

procedure TCurrentResData.Done;
var
  i: Integer;
begin
  if crfDone in FFLags then
    exit;

  Include(FFLags, crfDone);

  if FAnchestorCurrentData <> nil then
    FAnchestorCurrentData.Done;
  if FCurrentFields <> nil then
    for i := 0 to FCurrentFields.Count-1 do
      FCurrentFields[i].Done;

  if (FNewResultData <> nil) then begin
    Include(FFLags, crfWasDone);

    if (FNewResultData is TWatchResultDataPCharOrString) then begin
      FSubCurrentData.Done;
      FSubCurrentDataSecond.Done;

      TWatchResultDataPCharOrString(FNewResultData).SetEntryPrototype(FSubCurrentData.FNewResultData);
      TWatchResultDataPCharOrString(FNewResultData).SetEntryCount(2);
      TWatchResultDataPCharOrString(FNewResultData).WriteValueToStorage(0, FSubCurrentData.FNewResultData);

      TWatchResultDataPCharOrString(FNewResultData).WriteValueToStorage(1, FSubCurrentDataSecond.FNewResultData);
//      FreeAndNil(FSubCurrentData.FNewResultData);
//      FreeAndNil(FSubCurrentDataSecond.FNewResultData);

      FNewResultData.SetSelectedIndex(0);
    end
    else
    if FNewResultData is TWatchResultDataPointer then begin
      if FSubCurrentData <> nil then begin
        FSubCurrentData.Done;
        FNewResultData.SetDerefData(FSubCurrentData.FNewResultData);
        FSubCurrentData.MarkResDataAsUsedByOwner;

        //if not (crfIsArrayEntry in FSubCurrentData.FFLags) then
        //  FSubCurrentData.FNewResultData := nil;
      end;
    end
    else
    if (FNewResultData.ValueKind = rdkArray) then begin
      FinishCurrentArrayElement;
      if (FCurrentIdx >= 0) and (FNewResultData.Count > FCurrentIdx + 1) then
        FNewResultData.SetEntryCount(FCurrentIdx + 1);
    end
    else
    if (FNewResultData.ValueKind = rdkVariant) then begin
      FSubCurrentData.Done;
      FNewResultData.SetDerefData(FSubCurrentData.FNewResultData);
      FSubCurrentData.FNewResultData := nil;
      FreeAndNil(FSubCurrentData);
    end
    else
    if (FNewResultData.ValueKind in [rdkStruct, rdkConvertRes]) then begin
        WriteFieldsToRes(0, FNewResultData);
    end;
  end;
end;

function TCurrentResData.RootResultData: TCurrentResData;
begin
  Result := Self;
  if Result = nil then
    exit;
  while (Result.FOwnerCurrentData <> nil) do
    Result := Result.FOwnerCurrentData;
end;

procedure TCurrentResData.DebugPrint(AText: String);
begin
  if Self = nil then exit;
  DebugLnEnter(['>> ',AText]);
  debugln(['Flags: ', dbghex(longint(FFLags))]);
  if FNewResultData <> nil then
  debugln(['NewRes: ', dbghex(ptrint(FNewResultData)),' ',DbgSName(FNewResultData), ' >> ', FNewResultData.TypeName, ' > ',FNewResultData.AsString]);
  if FStoredResultData <> nil then
  debugln(['Stored: ', dbghex(ptrint(FStoredResultData)),' ',DbgSName(FStoredResultData), ' >> ', FStoredResultData.TypeName, ' > ',FStoredResultData.AsString]);
  if FStoredErrorResultData <> nil then
  debugln(['St-Err: ', dbghex(ptrint(FStoredErrorResultData)),' ',DbgSName(FNewResultData), ' >> ', FNewResultData.TypeName, ' > ',FNewResultData.AsString]);
  debugln(['Owner:  ', dbghex(ptrint(FOwnerCurrentData))]);
  FSubCurrentData.DebugPrint('SubCurrent');
  FSubCurrentDataSecond.DebugPrint('SubCurrent Second');
  FAnchestorCurrentData.DebugPrint('Anchestor');
  DebugLnExit(['<<']);
end;

procedure TCurrentResData.CreatePrePrinted(AVal: String);
begin
  //// TEMP: fallback for unsuported types  // This frees: FOwnerCurrentData.FNewResultData.DerefData
  if FOwnerCurrentData = nil then  FreeResultAndSubData;

  BeforeCreateValue;
  assert((FNewResultData=nil) or (FNewResultData.ValueKind=rdkPrePrinted), 'TCurrentResData.CreatePrePrinted: (FNewResultData=nil) or (FNewResultData.ValueKind=rdkPrePrinted)');
  if FNewResultData = nil then
    FNewResultData := TWatchResultDataPrePrinted.Create(AVal)
  else
    TWatchResultDataPrePrinted(FNewResultData).Create(AVal);
  AfterDataCreated;
end;

procedure TCurrentResData.CreateString(AVal: String);
begin
  BeforeCreateValue;
  assert((FNewResultData=nil) or (FNewResultData.ValueKind=rdkString), 'TCurrentResData.CreateString: (FNewResultData=nil) or (FNewResultData.ValueKind=rdkString)');
  if FNewResultData = nil then
    FNewResultData := TWatchResultDataString.Create(AVal)
  else
    TWatchResultDataString(FNewResultData).Create(AVal);
  AfterDataCreated;
end;

procedure TCurrentResData.CreateWideString(AVal: WideString);
begin
  BeforeCreateValue;
  assert((FNewResultData=nil) or (FNewResultData.ValueKind=rdkWideString), 'TCurrentResData.CreateWideString: (FNewResultData=nil) or (FNewResultData.ValueKind=rdkString)');
  if FNewResultData = nil then
    FNewResultData := TWatchResultDataWideString.Create(AVal)
  else
    TWatchResultDataWideString(FNewResultData).Create(AVal);
  AfterDataCreated;
end;

procedure TCurrentResData.CreateCharValue(ACharValue: QWord; AByteSize: Integer);
begin
  BeforeCreateValue;
  assert((FNewResultData=nil) or (FNewResultData.ValueKind=rdkChar), 'TCurrentResData.CreateCharValue: (FNewResultData=nil) or (FNewResultData.ValueKind=rdkString)');
  if FNewResultData = nil then
    FNewResultData := TWatchResultDataChar.Create(ACharValue, AByteSize)
  else
    TWatchResultDataChar(FNewResultData).Create(ACharValue, AByteSize);
  AfterDataCreated;
end;

procedure TCurrentResData.CreateNumValue(ANumValue: QWord; ASigned: Boolean;
  AByteSize: Integer);
begin
  BeforeCreateValue;
  assert((FNewResultData = nil) or (FNewResultData.ValueKind in [rdkSignedNumVal, rdkUnsignedNumVal]), 'TCurrentResData.CreateNumValue: (FNewResultData = nil) or (FNewResultData.ValueKind in [rdkSignedNumVal, rdkUnsignedNumVal])');
  if FNewResultData = nil then begin
    if ASigned then
      FNewResultData := TWatchResultDataSignedNum.Create(Int64(ANumValue), AByteSize)
    else
      FNewResultData := TWatchResultDataUnSignedNum.Create(ANumValue, AByteSize);
  end
  else begin
    if ASigned then
      TWatchResultDataSignedNum(FNewResultData).Create(Int64(ANumValue), AByteSize)
    else
      TWatchResultDataUnSignedNum(FNewResultData).Create(ANumValue, AByteSize);
  end;
  AfterDataCreated;
end;

procedure TCurrentResData.CreatePointerValue(AnAddrValue: TDbgPtr);
begin
  BeforeCreateValue;
  assert((FNewResultData=nil) or (FNewResultData.ValueKind=rdkPointerVal), 'TCurrentResData.CreatePointerValue: (FNewResultData=nil) or (FNewResultData.ValueKind=rdkPointerVal)');
  if FNewResultData = nil then
    FNewResultData := TWatchResultDataPointer.Create(AnAddrValue)
  else
    TWatchResultDataPointer(FNewResultData).Create(AnAddrValue);
  AfterDataCreated;
end;

procedure TCurrentResData.CreateFloatValue(AFloatValue: Extended;
  APrecission: TLzDbgFloatPrecission);
begin
  BeforeCreateValue;
  assert((FNewResultData=nil) or (FNewResultData.ValueKind=rdkFloatVal), 'TCurrentResData.CreateFloatValue: (FNewResultData=nil) or (FNewResultData.ValueKind=rdkString)');
  if FNewResultData = nil then
    FNewResultData := TWatchResultDataFloat.Create(AFloatValue, APrecission)
  else
    TWatchResultDataFloat(FNewResultData).Create(AFloatValue, APrecission);
  AfterDataCreated;
end;

function TCurrentResData.CreateProcedure(AVal: TDBGPtr; AnIsFunction: Boolean;
  ALoc, ADesc: String): IDbgWatchDataIntf;
begin
  BeforeCreateValue;
  if AnIsFunction then begin
    assert((FNewResultData=nil) or (FNewResultData.ValueKind = rdkFunction), 'TCurrentResData.CreateProcedure: (FNewResultData=nil) or (FNewResultData.ValueKind = rdkFunction]');
    if FNewResultData = nil then
      FNewResultData := TWatchResultDataFunc.Create(AVal, ALoc, ADesc)
    else
      TWatchResultDataFunc(FNewResultData).Create(AVal, ALoc, ADesc);
  end
  else begin
    assert((FNewResultData=nil) or (FNewResultData.ValueKind = rdkProcedure), 'TCurrentResData.CreateProcedure: (FNewResultData=nil) or (FNewResultData.ValueKind = rdkProcedure]');
    if FNewResultData = nil then
      FNewResultData := TWatchResultDataProc.Create(AVal, ALoc, ADesc)
    else
      TWatchResultDataProc(FNewResultData).Create(AVal, ALoc, ADesc);
  end;
  AfterDataCreated;

  Result := nil;
end;

function TCurrentResData.CreateProcedureRef(AVal: TDBGPtr;
  AnIsFunction: Boolean; ALoc, ADesc: String): IDbgWatchDataIntf;
begin
  BeforeCreateValue;
  if AnIsFunction then begin
    assert((FNewResultData=nil) or (FNewResultData.ValueKind = rdkFunctionRef), 'TCurrentResData.CreateProcedureRef: (FNewResultData=nil) or (FNewResultData.ValueKind = rdkFunctionRef]');
    if FNewResultData = nil then
      FNewResultData := TWatchResultDataFuncRef.Create(AVal, ALoc, ADesc)
    else
      TWatchResultDataFuncRef(FNewResultData).Create(AVal, ALoc, ADesc);
  end
  else begin
    assert((FNewResultData=nil) or (FNewResultData.ValueKind = rdkProcedureRef), 'TCurrentResData.CreateProcedureRef: (FNewResultData=nil) or (FNewResultData.ValueKind = rdkProcedureRef]');
    if FNewResultData = nil then
      FNewResultData := TWatchResultDataProcRef.Create(AVal, ALoc, ADesc)
    else
      TWatchResultDataProcRef(FNewResultData).Create(AVal, ALoc, ADesc);
  end;
  AfterDataCreated;


  Result := nil;
end;

procedure TCurrentResData.CreateBoolValue(AnOrdBoolValue: QWord;
  AByteSize: Integer);
begin
  BeforeCreateValue;
  assert((FNewResultData=nil) or (FNewResultData.ValueKind=rdkBool), 'TCurrentResData.CreateBoolValue: (FNewResultData=nil) or (FNewResultData.ValueKind=rdkString)');
  if FNewResultData = nil then
    FNewResultData := TWatchResultDataBoolean.Create(AnOrdBoolValue, AByteSize)
  else
    TWatchResultDataBoolean(FNewResultData).Create(AnOrdBoolValue, AByteSize);
  AfterDataCreated;
end;

procedure TCurrentResData.CreateEnumValue(ANumValue: QWord; AName: String;
  AByteSize: Integer; AnIsEnumIdent: Boolean);
begin
  BeforeCreateValue;
  if AnIsEnumIdent then begin
    assert((FNewResultData=nil) or (FNewResultData.ValueKind = rdkEnumVal), 'TCurrentResData.CreateEnumValue: (FNewResultData=nil) or (FNewResultData.ValueKind = rdkEnumVal]');
    if FNewResultData = nil then
      FNewResultData := TWatchResultDataEnumVal.Create(ANumValue, AName, AByteSize)
    else
      TWatchResultDataEnumVal(FNewResultData).Create(ANumValue, AName, AByteSize);
  end
  else begin
    assert((FNewResultData=nil) or (FNewResultData.ValueKind = rdkEnum), 'TCurrentResData.CreateEnumValue: (FNewResultData=nil) or (FNewResultData.ValueKind = rdkEnum]');
    if FNewResultData = nil then
      FNewResultData := TWatchResultDataEnum.Create(ANumValue, AName, AByteSize)
    else
      TWatchResultDataEnum(FNewResultData).Create(ANumValue, AName, AByteSize);
  end;
  AfterDataCreated;
end;

procedure TCurrentResData.CreateSetValue(const ANames: TStringDynArray);
begin
  BeforeCreateValue;
  assert((FNewResultData=nil) or (FNewResultData.ValueKind=rdkSet), 'TCurrentResData.CreateSetValue: (FNewResultData=nil) or (FNewResultData.ValueKind=rdkPointerVal)');
  if FNewResultData = nil then
    FNewResultData := TWatchResultDataSet.Create(ANames)
  else
    TWatchResultDataSet(FNewResultData).Create(ANames);
  AfterDataCreated;
end;

function TCurrentResData.CreateVariantValue(AName: String;
  AVisibility: TLzDbgFieldVisibility): IDbgWatchDataIntf;
begin
  BeforeCreateValue;
  assert((FNewResultData=nil) or (FNewResultData.ValueKind=rdkVariant), 'TCurrentResData.CreateVariantValue: (FNewResultData=nil) or (FNewResultData.ValueKind=rdkVariant)');
  if FNewResultData = nil then
    FNewResultData := TWatchResultDataVariant.Create(AName, AVisibility)
  else
    TWatchResultDataVariant(FNewResultData).Create(AName, AVisibility);

  if FSubCurrentData <> nil then
    FSubCurrentData.FreeResultAndSubDataAndDestroy;
  // Don't set the FOwnerCurrentData
  FSubCurrentData := TCurrentResData.Create;
  FSubCurrentData.FFLags := FSubCurrentData.FFLags + [crfFreeResData, crfFreeErrResData];
  Result := FSubCurrentData;
  AfterDataCreated;
end;

procedure TCurrentResData.CreateStructure(AStructType: TLzDbgStructType;
  ADataAddress: TDBGPtr);
begin
  BeforeCreateValue;
  assert((FNewResultData=nil) or (FNewResultData.ValueKind=rdkStruct), 'TCurrentResData.CreateStructure: (FNewResultData=nil) or (FNewResultData.ValueKind=rdkPointerVal)');
  if FNewResultData = nil then begin
    if AStructType in [dstClass, dstInterface] then
      FNewResultData := TWatchResultDataRefStruct.Create(AStructType, ADataAddress)
    else
      FNewResultData := TWatchResultDataStruct.Create(AStructType);
  end
  else begin
    if AStructType in [dstClass, dstInterface] then
      TWatchResultDataRefStruct(FNewResultData).Create(AStructType, ADataAddress)
    else
      TWatchResultDataStruct(FNewResultData).Create(AStructType);
  end;
  FCurrentIdx := 0;
  AfterDataCreated;
end;

function TCurrentResData.CreateValueHandlerResult(
  AValueHandler: ILazDbgValueConverterIntf): IDbgWatchDataIntf;
begin
  BeforeCreateValue;
  assert((FNewResultData=nil) or (FNewResultData.ValueKind=rdkConvertRes), 'TCurrentResData.CreateVariantValue: (FNewResultData=nil) or (FNewResultData.ValueKind=rdkConvertRes)');
  if FNewResultData = nil then
    FNewResultData := TWatchResultDataConverted.Create(AValueHandler)
  else
    TWatchResultDataConverted(FNewResultData).Create(AValueHandler);

  FCurrentIdx := 0;
  AfterDataCreated;

  Result := AddField('', dfvUnknown, []);
end;

function TCurrentResData.CreateArrayValue(AnArrayType: TLzDbgArrayType;
  ATotalCount: Integer; ALowIdx: Integer): IDbgWatchDataIntf;
begin
  BeforeCreateValue;
  assert((FNewResultData=nil) or ((FNewResultData=nil) or (FNewResultData.ValueKind=rdkArray)), 'TCurrentResData.CreateArrayValue: (FNewResultData=nil) or ((FNewResultData=nil) or (FNewResultData.ValueKind=rdkArray))');
  if FNewResultData = nil then begin
    assert(FSubCurrentData=nil, 'TCurrentResData.CreateArrayValue: FSubCurrentData=nil');
    assert(FFLags*[crfSubDataCreated]=[], 'TCurrentResData.CreateArrayValue: FFLags*[crfSubDataCreated]=[]');
    assert((AnArrayType<>datDynArray) or (ALowIdx=0), 'TCurrentResData.CreateArrayValue: (AnArrayType<>datDynArray) or (ALowIdx=0)');

    case AnArrayType of
      datUnknown:   FNewResultData := TWatchResultDataArray.Create(ATotalCount, ALowIdx);
      datDynArray:  FNewResultData := TWatchResultDataDynArray.Create(ATotalCount);
      datStatArray: FNewResultData := TWatchResultDataStatArray.Create(ATotalCount, ALowIdx);
    end;

    FCurrentIdx := -1; // xxxxxxxxx lowbound
    FArrayCount := ATotalCount;

    FSubCurrentData := CreateSubCurrentResData;
  end

  else begin

    case AnArrayType of
      datUnknown:   begin
        assert(FNewResultData is TWatchResultDataArray, 'TCurrentResData.CreateArrayValue: FNewResultData is TWatchResultDataArray');
        TWatchResultDataArray(FNewResultData).Create(ATotalCount, ALowIdx);
      end;
      datDynArray:  begin
        assert(FNewResultData is TWatchResultDataDynArray, 'TCurrentResData.CreateArrayValue: FNewResultData is TWatchResultDataDynArray');
        TWatchResultDataDynArray(FNewResultData).Create(ATotalCount);
      end;
      datStatArray: begin
        assert(FNewResultData is TWatchResultDataStatArray, 'TCurrentResData.CreateArrayValue: FNewResultData is TWatchResultDataStatArray');
        assert(ATotalCount=FArrayCount, 'TCurrentResData.CreateArrayValue: ATotalCount=FArrayCount');
        TWatchResultDataStatArray(FNewResultData).Create(ATotalCount, ALowIdx);
      end;
    end;

    FNewResultData.SetEntryCount(0);
    FCurrentIdx := -1; // xxxxxxxxx lowbound
    FArrayCount := ATotalCount;
  end;

  AfterDataCreated;
  Result := FSubCurrentData;
end;

procedure TCurrentResData.CreateError(AVal: String);
begin
  BeforeCreateError;
  if FNewResultData = nil then
    FNewResultData := TWatchResultDataError.Create(AVal)
  else
    TWatchResultDataError(FNewResultData).Create(AVal);
  if FStoredResultData <> nil then
    FNewResultData.SetTypeName(FStoredResultData.TypeName);
  AfterDataCreated;
end;

function TCurrentResData.SetPCharShouldBeStringValue: IDbgWatchDataIntf;
begin
  assert(FNewResultData<>nil, 'TCurrentResData.SetPCharShouldBeStringValue: FNewResultData<>nil');
  assert(FOwnerCurrentData=nil, 'TCurrentResData.SetPCharShouldBeStringValue: FOwnerCurrentData=nil');

  FOwnerCurrentData := TCurrentResData.Create;
  Result := FOwnerCurrentData.InternalPCharShouldBeStringValue(Self);
end;

procedure TCurrentResData.SetTypeName(ATypeName: String);
begin
  assert(FNewResultData<>nil, 'TCurrentResData.SetTypeName: FNewResultData<>nil');
  FNewResultData.SetTypeName(ATypeName);
end;

function TCurrentResData.SetDerefData: IDbgWatchDataIntf;
begin
  if (FNewResultData<> nil) and (FNewResultData.ValueKind = rdkConvertRes) then begin
    Result := AddField('', dfvUnknown, []);
    exit;
  end;

  assert((FNewResultData<>nil) and (FNewResultData is TWatchResultDataPointer), 'TCurrentResData.SetDerefData: (FNewResultData<>nil) and (FNewResultData is TWatchResultDataPointer)');
  if FSubCurrentData = nil then
    FSubCurrentData := CreateSubCurrentResData;
  Result := FSubCurrentData;
end;

procedure TCurrentResData.SetDataAddress(AnAddr: TDbgPtr);
begin
  assert((FNewResultData<>nil) and (FNewResultData.ValueKind in [rdkArray, rdkString, rdkWideString]), 'TCurrentResData.SetDataAddress: (FNewResultData<>nil) and (FNewResultData.ValueKind in [rdkArray, rdkString, rdkWideString])');
  FNewResultData.SetDataAddress(AnAddr);
end;

function TCurrentResData.SetNextArrayData: IDbgWatchDataIntf;
begin
  assert((FNewResultData<>nil) and (FNewResultData.ValueKind=rdkArray), 'TCurrentResData.SetNextArrayData: (FNewResultData<>nil) and (FNewResultData.ValueKind=rdkArray)');

  FinishCurrentArrayElement;
  inc(FCurrentIdx);
  Result := FSubCurrentData;
end;

function TCurrentResData.SetAnchestor(ATypeName: String): IDbgWatchDataIntf;
begin
  assert((FNewResultData<>nil) and (FNewResultData.ValueKind in  [rdkStruct]), 'TCurrentResData.SetAnchestor: (FNewResultData<>nil) and (FNewResultData.ValueKind in  [rdkStruct])');
  assert(FSubCurrentData=nil, 'TCurrentResData.SetAnchestor: FSubCurrentData=nil');

  if (FAnchestorCurrentData <> nil) then begin
    assert(crfIsArrayEntry in FFLags, 'TCurrentResData.BeforeCreateValue: crfIsArrayEntry in FFLags');
    FAnchestorCurrentData.CreateStructure(FNewResultData.StructType);
    Result := FAnchestorCurrentData;
    exit;
  end;

  assert(FAnchestorCurrentData=nil, 'TCurrentResData.SetAnchestor: FAnchestorCurrentData=nil');

  FAnchestorCurrentData := CreateSubCurrentResData;
  Include(FAnchestorCurrentData.FFLags, crfIsAnchestor);
  if crfIsAnchestor in FFLags then
    FAnchestorCurrentData.FOwnerCurrentData := FOwnerCurrentData; // Top level parent
  FAnchestorCurrentData.CreateStructure(FNewResultData.StructType);

  FAnchestorCurrentData.SetTypeName(ATypeName);
  FNewResultData.SetAnchestor(FAnchestorCurrentData.FNewResultData);
  FAnchestorCurrentData.MarkResDataAsUsedByOwner;

  Result := FAnchestorCurrentData;
end;

function TCurrentResData.AddField(AFieldName: String;
  AVisibility: TLzDbgFieldVisibility; AFlags: TLzDbgFieldFlags
  ): IDbgWatchDataIntf;
var
  NewField: TCurrentResData;
begin
  Result := nil;
  assert((FNewResultData<>nil) and (FNewResultData.ValueKind in [rdkStruct, rdkConvertRes]), 'TCurrentResData.AddField: (FNewResultData<>nil) and (FNewResultData.ValueKind in [rdkStruct])');

  if FCurrentFields = nil then begin
    FCurrentFields := TCurrentResDataList.Create(True);
    Exclude(FFLags, crfWasDone);
  end;

  if FCurrentIdx < FCurrentFields.Count then begin
    Result := FCurrentFields[FCurrentIdx];
    inc(FCurrentIdx);

    exit;
  end;

  NewField := CreateSubCurrentResData;
  FCurrentFields.Add(NewField);

  if FCurrentIdx >= FNewResultData.DirectFieldCount then
    FNewResultData.SetFieldCount(FCurrentIdx+50);

  FNewResultData.SetField(FCurrentIdx, AFieldName, AVisibility, AFlags, nil);
  inc(FCurrentIdx);

  Result := NewField;
end;

{ TCurrentWatchValue }

procedure TCurrentWatchValue.DoBeginUpdating;
begin
  AddReference;
  FCurrentBackEndExpression := inherited GetBackendExpression;
end;

procedure TCurrentWatchValue.DoEndUpdating;
var
  NewValid: TDebuggerDataState;
begin
  //assert(Validity = ddsRequested, 'TCurrentWatchValue.EndUpdate: Validity = ddsRequested');
  NewValid := ddsValid;

  FCurrentResData := FCurrentResData.RootResultData;
  if (FCurrentResData <> nil) and (FCurrentResData.FNewResultData <> nil) then begin
    FCurrentResData.Done;
    SetResultData(FCurrentResData.FNewResultData);

    if ResultData.ValueKind = rdkError then
      NewValid := ddsError;

    FreeAndNil(FCurrentResData);
  end
  else
    NewValid := ddsInvalid;

  if Validity = ddsRequested then
    SetValidity(NewValid)
  else
    DoDataValidityChanged(ddsRequested);

  ReleaseReference; // Last statemnet, may call Destroy
end;

function TCurrentWatchValue.ResData: IDbgWatchDataIntf;
begin
  assert(UpdateCount > 0, 'TCurrentWatchValue.ResData: FUpdateCount > 0');
  if FCurrentResData = nil then
    FCurrentResData := TCurrentResData.Create;
  Result := FCurrentResData;
end;

function TCurrentWatchValue.GetDbgValConverter: ILazDbgValueConvertSelectorIntf;
begin
  Result := FDbgBackendConverter;
end;

procedure TCurrentWatchValue.SetSnapShot(const AValue: TIdeWatchValue);
begin
  assert((FSnapShot=nil) or (AValue=nil), 'TCurrentWatchValue already have snapshot');
  if FSnapShot = AValue then exit;
  FSnapShot := AValue;
  if FSnapShot <> nil
  then FSnapShot.Assign(self);
end;

procedure TCurrentWatchValue.SetValue(AValue: String);
begin
  BeginUpdate;
  ResData.CreatePrePrinted(AValue);
  EndUpdate;
end;

procedure TCurrentWatchValue.SetWatch(AValue: TWatch);
begin
  CancelRequestData;
  inherited SetWatch(AValue);
end;

function TCurrentWatchValue.GetBackendExpression: String;
begin
  if UpdateCount > 0 then
    Result := FCurrentBackEndExpression
  else
    Result := inherited GetBackendExpression;
end;

function TCurrentWatchValue.GetValidity: TDebuggerDataState;
begin
  if UpdateCount > 0 then
    Result := ddsRequested  // prevent reading FValue
  else
    Result := inherited GetValidity;
end;

procedure TCurrentWatchValue.RequestData;
begin
  FreeAndNil(FDbgBackendConverter);
  if Watch.DbgBackendConverter <> nil then
    FDbgBackendConverter := TIdeDbgValueConvertSelector(Watch.DbgBackendConverter.CreateCopy);

  if (Watch.ParentWatch <> nil) and (Watch.ParentWatch.DbgBackendConverter = Watch.DbgBackendConverter) then
    if MaybeCopyResult(Watch.ParentWatch) then
      exit;

  TCurrentWatch(Watch).RequestData(self);
end;

procedure TCurrentWatchValue.CancelRequestData;
begin
  CallNotifications(weeCancel, default(TDbgDataRequestEventData));
end;

procedure TCurrentWatchValue.DoDataValidityChanged(AnOldValidity: TDebuggerDataState);
begin
  if UpdateCount > 0 then
    exit;
  if Validity = ddsRequested then exit;
  if Watch <> nil then
    TCurrentWatches(TCurrentWatch(Watch).Collection).Update(Watch);
  if FSnapShot <> nil
  then FSnapShot.Assign(self);

  if FOnValidityChanged <> nil then
    FOnValidityChanged(Self);
end;

destructor TCurrentWatchValue.Destroy;
var
  e: TMethodList;
begin
  assert(UpdateCount=0, 'TCurrentWatchValue.Destroy: FUpdateCount=0');
  FCurrentResData := FCurrentResData.RootResultData;
  if (FCurrentResData <> nil) and (FResultData = nil) then
    FCurrentResData.FreeResultAndSubData;
  FCurrentResData.Free;
  DoDestroy;
  FDbgBackendConverter.Free;
  inherited Destroy;
end;

{ TCurrentWatchValueList }

procedure TCurrentWatchValueList.SetSnapShot(const AValue: TIdeWatchValueList);
var
  R: TIdeWatchValue;
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
      R := TIdeWatchValue.Create(FSnapShot.Watch);
      FSnapShot.Add(R);
      TCurrentWatchValue(EntriesByIdx[i]).SnapShot := R;
    end;
  end;

end;

function TCurrentWatchValueList.CreateEntry(const AThreadId: Integer;
  const AStackFrame: Integer): TIdeWatchValue;
var
  R: TIdeWatchValue;
begin
  try DebugLnEnter(DBG_DATA_MONITORS, ['DebugDataMonitor: >>ENTER: TCurrentWatchValueList.CreateEntry  AThreadId=', AThreadId, '  AStackFrame=',AStackFrame, ' Expr=', Watch.Expression]);
  Result := TCurrentWatchValue.Create(Watch, AThreadId, AStackFrame);
  Add(Result);
  if FSnapShot <> nil then begin
    R := TIdeWatchValue.Create(FSnapShot.Watch);
    FSnapShot.Add(R);
    TCurrentWatchValue(Result).SnapShot := R;
  end;
  finally DebugLnExit(DBG_DATA_MONITORS, ['DebugDataMonitor: <<EXIT: TCurrentWatchValueList.CreateEntry']); end;
end;

procedure TCurrentWatchValueList.Clear;
begin
  inherited Clear;
  if FSnapShot <> nil then
    FSnapShot.Clear;
end;

{ TWatchValueList }

function TIdeWatchValueList.GetEntry(const AThreadId: Integer;
  const AStackFrame: Integer): TIdeWatchValue;
begin
  Result := TIdeWatchValue(inherited Entries[AThreadId, AStackFrame]);
end;

function TIdeWatchValueList.GetEntryByIdx(AnIndex: integer): TIdeWatchValue;
begin
  Result := TIdeWatchValue(inherited EntriesByIdx[AnIndex]);
end;

function TIdeWatchValueList.GetWatch: TIdeWatch;
begin
  Result := TIdeWatch(inherited Watch);
end;

procedure TIdeWatchValueList.LoadDataFromXMLConfig(const AConfig: TXMLConfig;
  APath: string);
var
  e: TIdeWatchValue;
  c, i: Integer;
begin
  Clear;
  c := AConfig.GetValue(APath + 'Count', 0);
  APath := APath + 'Entry';
  for i := 0 to c - 1 do begin
    e := TIdeWatchValue.Create(Watch);
    e.LoadDataFromXMLConfig(AConfig, APath + IntToStr(i) + '/');
    Add(e);
  end;
end;

procedure TIdeWatchValueList.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  APath: string);
var
  i: Integer;
begin
  AConfig.SetDeleteValue(APath + 'Count', Count, 0);
  APath := APath + 'Entry';
  for i := 0 to Count - 1 do
    EntriesByIdx[i].SaveDataToXMLConfig(AConfig, APath + IntToStr(i) + '/');
end;

constructor TIdeWatchValueList.Create(AOwnerWatch: TIdeWatch);
begin
  assert(((Self is TCurrentWatchValueList) and (AOwnerWatch is TCurrentWatch)) or ((not(Self is TCurrentWatchValueList)) and not(AOwnerWatch is TCurrentWatch)),
         'TWatchValueList.Create: Watch and list differ (current and none current)');
  inherited Create(AOwnerWatch);
end;

{ TWatchValue }

function TIdeWatchValue.GetValue: String;
var
  i: Integer;
begin
  Result := '';
  if Watch = nil then
    exit;
  if not Watch.Enabled then
    exit('<disabled>');
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

function TIdeWatchValue.GetResultData: TWatchResultData;
var
  i: Integer;
begin
  Result := inherited GetResultData;
  if (Watch = nil) or (not Watch.Enabled) then
    exit;
  i := DbgStateChangeCounter;  // workaround for state changes during TWatchValue.GetResultData
  if Validity = ddsUnknown then begin
    Validity := ddsRequested;
    RequestData;
    if i <> DbgStateChangeCounter then exit; // in case the debugger did run.
    // TODO: The watch can also be deleted by the user
    Result := inherited GetResultData;
  end;
end;

function TIdeWatchValue.GetValidity: TDebuggerDataState;
begin
  if (Watch = nil) or (Watch.HasAllValidParents(ThreadId, StackFrame)) then
    Result := inherited GetValidity
  else
    Result := ddsEvaluating; // ddsWaitForParent;
end;

function TIdeWatchValue.GetChildrenByNameAsArrayEntry(AName: Int64): TObject;
begin
  Result := nil;
  if FWatch = nil then
    exit;

  if FResultDataContent = rdcJSon then begin
    Result := Watch.GetChildWatch(IntToStr(AName), Expression + '{' + IntToStr(AName) + '}');
    exit;
  end;

  Result := Watch.ChildrenByNameAsArrayEntry[AName];
end;

function TIdeWatchValue.GetChildrenByNameAsField(AName, AClassName: String
  ): TObject;
begin
  Result := nil;
  if FWatch = nil then
    exit;

  if FResultDataContent = rdcJSon then begin
    Result := Watch.GetChildWatch(AName, Expression + '{"' + AName + '"}');
    exit;
  end;

  Result := Watch.ChildrenByNameAsField[AName, AClassName];
end;

function TIdeWatchValue.GetWatch: TIdeWatch;
begin
  Result := TIdeWatch(inherited Watch);
end;

function TIdeWatchValue.GetEnabled: Boolean;
begin
  Result := Watch <> nil;
  if Result then
    Result := Watch.Enabled;
end;

function TIdeWatchValue.GetTypeInfo: TDBGType;
var
  i: Integer;
begin
  Result := nil;
  if (Watch = nil) or (not Watch.Enabled) then
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

procedure TIdeWatchValue.RequestData;
begin
  Validity := ddsInvalid;
end;

procedure TIdeWatchValue.LoadDataFromXMLConfig(const AConfig: TXMLConfig;
  const APath: string);
begin
  FThreadId   := AConfig.GetValue(APath + 'ThreadId', -1);
  FStackFrame := AConfig.GetValue(APath + 'StackFrame', -1);
  AConfig.GetValue(APath + 'Validity', int64(ord(ddsValid)), FValidity, system.TypeInfo(TDebuggerDataState));
  if AConfig.GetValue(APath + 'ClassAutoCast', False)
  then Include(FEvaluateFlags, defClassAutoCast)
  else Exclude(FEvaluateFlags, defClassAutoCast);
  FRepeatCount := AConfig.GetValue(APath + 'RepeatCount', 0);
  AConfig.GetValue(APath + 'DisplayFormat', int64(ord(wdfDefault)), FDisplayFormat, system.TypeInfo(TWatchDisplayFormat));

  // Defaults to PrePrinted
  FResultData := TWatchResultData.CreateFromXMLConfig(AConfig, APath);
  if ResultData = nil then
    FValidity := ddsUnknown
  else
  if ResultData.ValueKind = rdkError then
    FValidity := ddsError;
end;

procedure TIdeWatchValue.SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string);
begin
  AConfig.SetValue(APath + 'ThreadId', ThreadId);
  AConfig.SetValue(APath + 'StackFrame', StackFrame);
  AConfig.SetDeleteValue(APath + 'Validity', Validity, int64(ord(ddsValid)), system.TypeInfo(TDebuggerDataState));
  AConfig.SetDeleteValue(APath + 'ClassAutoCast', defClassAutoCast in EvaluateFlags, False);
  AConfig.SetDeleteValue(APath + 'RepeatCount', RepeatCount, 0);

  if (Watch <> nil) and (FDisplayFormat <> wdfMemDump) and
     (FResultData <> nil) and
     (FResultData.ValueKind <> rdkPrePrinted)
  then begin
    // Use same path => "Value" will be readable for older IDE (read as wdDefault)
    // ResultData does not write any "path/value" conflicting with the above fields
    ResultData.SaveDataToXMLConfig(AConfig, APath);
  end
  else begin
    AConfig.SetDeleteValue(APath + 'DisplayFormat', DisplayFormat, int64(ord(wdfDefault)), system.TypeInfo(TWatchDisplayFormat));
    AConfig.SetValue(APath + 'Value', Value);
  end;
end;

constructor TIdeWatchValue.Create(AOwnerWatch: TWatch);
begin
  inherited Create(AOwnerWatch);
  Validity := ddsUnknown;
  FDisplayFormat := Watch.DisplayFormat;
  FEvaluateFlags := Watch.EvaluateFlags;
  FRepeatCount   := Watch.RepeatCount;
  FFirstIndexOffs    := Watch.FirstIndexOffs;
end;

constructor TIdeWatchValue.Create(AOwnerWatch: TIdeWatch; const AThreadId: Integer;
  const AStackFrame: Integer);
begin
  Create(AOwnerWatch);
  FThreadId := AThreadId;
  FStackFrame := AStackFrame;
end;

procedure TIdeWatchValue.Assign(AnOther: TWatchValue);
begin
  inherited Assign(AnOther);
  FThreadId      := TIdeWatchValue(AnOther).FThreadId;
  FStackFrame    := TIdeWatchValue(AnOther).FStackFrame;
  FDisplayFormat := TIdeWatchValue(AnOther).FDisplayFormat;
end;

function TIdeWatchValue.ExpressionForChildField(AName: String;
  AClassName: String): String;
begin
  Result := '';
  if FResultDataContent = rdcJSon then
    Result := Expression + '{"' + AName + '"}'
  else
  if ResultData.ValueKind = rdkStruct then begin
    Result := Expression;
    if AClassName <> '' then
      Result := AClassName + '(' + Result + ')';
    Result := Result + '.' + AName;
  end;
end;

function TIdeWatchValue.ExpressionForChildEntry(AnIndex: Int64): String;
begin
  Result := ExpressionForChildEntry(IntToStr(AnIndex));
end;

function TIdeWatchValue.ExpressionForChildEntry(AnIndex: String): String;
begin
  Result := '';
  if FResultDataContent = rdcJSon then
    Result := Expression + '{' + AnIndex + '}'
  else
  if ResultData.ValueKind = rdkArray then
    Result := Expression + '[' + AnIndex + ']';
end;

function TIdeWatchValue.MaybeCopyResult(ASourceWatch: TIdeWatch): boolean;
var
  ASrcValue: TIdeWatchValue;
begin
  Result := (ASourceWatch.GetBackendExpression = GetBackendExpression) and
            (ASourceWatch.DisplayFormat  = FWatch.DisplayFormat) and
            (ASourceWatch.RepeatCount    = FWatch.RepeatCount) and
            (ASourceWatch.FirstIndexOffs = FWatch.FirstIndexOffs) and
            (ASourceWatch.EvaluateFlags  = FWatch.EvaluateFlags);
  if not Result then
    exit;

  ASrcValue := TIdeWatchValue(ASourceWatch.FValueList.ExistingEntries[ThreadId, StackFrame]);
  Result := (ASrcValue <> nil) and (ASrcValue.Validity = ddsValid) and (ASrcValue.FResultData <> nil);
  if not Result then
    exit;

  FResultData := ASrcValue.FResultData.CreateCopy;
  SetValidity(ddsValid);
end;

{ TIdeWatchesMonitor }

function TIdeWatchesMonitor.GetSnapshot(AnID: Pointer): TIdeWatches;
begin
  Result := TIdeWatches(FSnapshots.SnapShot[AnID]);
end;

function TIdeWatchesMonitor.GetCurrentWatches: TCurrentWatches;
begin
  Result := TCurrentWatches(Watches);
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

procedure TIdeWatchesMonitor.DoModified;
begin
  if (FIgnoreModified = 0) and Assigned(FOnModified) then
    FOnModified(Self);
end;

procedure TIdeWatchesMonitor.InvalidateWatchValues;
begin
  inherited InvalidateWatchValues;
  if Watches <> nil then
    Watches.ClearValues;
  if FOnWatchesInvalidated <> nil then
    FOnWatchesInvalidated(Self);
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
  then Supplier.RequestData(IDbgWatchValueIntf(AWatchValue))
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
  FWatches := CreateWatches;
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
  FreeAndNil(FWatches);
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

procedure TIdeWatchesMonitor.SaveToXMLConfig(const AConfig: TXMLConfig;
  const APath: string; const ALegacyList: Boolean);
begin
  CurrentWatches.SaveToXMLConfig(AConfig, APath, ALegacyList);
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
  Result := 0;
  case FCurrentValidity of
    ddsUnknown:   begin
        FCurrentValidity := ddsRequested;
        FMonitor.RequestCurrent(self);
        if FCurrentValidity = ddsValid then
          Result := inherited GetCurrent();
      end;
    ddsValid:                    Result := inherited GetCurrent;
    //ddsRequested, ddsEvaluating: Result := 0;
    //ddsInvalid, ddsError:        Result := 0;
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
  FEntries:= TMap.Create(its4, SizeOf(TIdeCallStackEntry));
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

procedure TCurrentCallStack.Assign(AnOther: TCallStackBase);
begin
  inherited Assign(AnOther);
  if AnOther is TCurrentCallStack then begin
    FCount := TCurrentCallStack(AnOther).FCount;
    FCountValidity := TCurrentCallStack(AnOther).FCountValidity;
    FAtLeastCountValidity := TCurrentCallStack(AnOther).FAtLeastCountValidity;
    FAtLeastCount := TCurrentCallStack(AnOther).FAtLeastCount;
    FAtLeastCountOld := TCurrentCallStack(AnOther).FAtLeastCountOld;
  end
  else begin
    FCount := AnOther.Count;
    FAtLeastCount := -1;
    FAtLeastCountOld := -1;
  end;
end;

procedure TCurrentCallStack.SetSnapShot(const AValue: TIdeCallStack);
begin
  assert((FSnapShot=nil) or (AValue=nil), 'TCurrentCallStack already have snapshot');
  if FSnapShot = AValue then exit;

  if (FSnapShot <> nil) and (AValue = nil)
  then FSnapShot.Assign(Self);

  FSnapShot := AValue;
end;

function TCurrentCallStack.GetCountValidity: TDebuggerDataState;
begin
  Result := FCountValidity;
end;

function TCurrentCallStack.GetCount: Integer;
begin
  Result := 0;
  case FCountValidity of
    ddsUnknown:   begin
        FCountValidity := ddsRequested;
        FMonitor.RequestCount(self);
        if FCountValidity = ddsValid then
          Result := FCount;
      end;
    ddsValid:                    Result := FCount;
    //ddsRequested, ddsEvaluating: Result := 0;
    //ddsInvalid, ddsError:        Result := 0;
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

function TCurrentCallStack.GetEntry(AIndex: Integer): TIdeCallStackEntry;
begin
  if (AIndex < 0)
  or (AIndex >= CountLimited(AIndex+1)) then IndexError(Aindex);

  Result := nil;
  if FEntries.GetData(AIndex, Result) then Exit;

  Result := TIdeCallStackEntry.Create(AIndex, 0, nil, '', nil, 0, ddsRequested);
  if Result = nil then Exit;
  FEntries.Add(AIndex, Result);
  Result.FOwner := Self;

  if (FLowestUnknown < 0) or (FLowestUnknown > AIndex)
  then FLowestUnknown := AIndex;
  if (FHighestUnknown < AIndex)
  then FHighestUnknown := AIndex;

  DoEntriesCreated;
end;

procedure TCurrentCallStack.AddEntry(AnEntry: TIdeCallStackEntry);
begin
  FEntries.Add(AnEntry.Index, AnEntry);
  AnEntry.FOwner := Self;
end;

procedure TCurrentCallStack.AssignEntriesTo(AnOther: TIdeCallStack);
var
  It: TMapIterator;
begin
  It := TMapIterator.Create(FEntries);
  It.First;
  while (not IT.EOM)
  do begin
    AnOther.AddEntry(TIdeCallStackEntry(It.DataPtr^).CreateCopy as TIdeCallStackEntry);
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
  until It.EOM or (ACount <= 0) or (TIdeCallStackEntry(It.DataPtr^).Index <> AIndex);

  if ACount > 1
  then begin
    EndIndex := AIndex + ACount - 1;
    if It.Locate(EndIndex)
    then repeat
      // start searching for the last unavailable
      Dec(EndIndex);
      Dec(ACount);
      It.Previous;
    until It.BOM or (ACount <= 0) or (TIdeCallStackEntry(It.DataPtr^).Index <> EndIndex);
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
  if FCountValidity = ddsValid then
    exit(inherited HasAtLeastCount(ARequiredMinCount));
  if FAtLeastCountOld >= ARequiredMinCount then
    exit(nbTrue);
  if (FAtLeastCountValidity = ddsValid) and (FAtLeastCount < ARequiredMinCount) then begin
    FAtLeastCountOld := FAtLeastCount;
    FAtLeastCountValidity := ddsUnknown;
  end;

  Result := nbUnknown;
  case FAtLeastCountValidity of
    ddsUnknown:   begin
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
  if (FAtLeastCountValidity = AValidity) and
     ( (AValidity <> ddsValid) or (FAtLeastCount >= AMinCount) )
  then
    exit;
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
  R: TIdeCallStack;
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
      R := TIdeCallStack.Create;
      R.ThreadId := Entries[i].ThreadId;
      FSnapShot.Add(R);
      TCurrentCallStack(Entries[i]).SnapShot := R;
    end;
  end;
end;

function TCurrentCallStackList.NewEntryForThread(const AThreadId: Integer): TCallStackBase;
var
  R: TIdeCallStack;
begin
  try DebugLnEnter(DBG_DATA_MONITORS, ['DebugDataMonitor: >>ENTER: TCurrentCallStackList.GetEntryForThread: ThreadId=', AThreadId]);
  Result := TCurrentCallStack.Create(FMonitor);
  Result.ThreadId := AThreadId;
  Add(Result);
  if FSnapShot <> nil then begin
    R := TIdeCallStack.Create;
    R.ThreadId := AThreadId;
    FSnapShot.Add(R);
    TCurrentCallStack(Result).SnapShot := R;
  end;
  finally DebugLnExit(DBG_DATA_MONITORS, ['DebugDataMonitor: <<EXIT: TCurrentCallStackList.GetEntryForThread' ]) end;
end;

{ TCallStackList }

function TIdeCallStackList.GetEntry(const AIndex: Integer): TIdeCallStack;
begin
  Result := TIdeCallStack(inherited Entries[AIndex]);
end;

function TIdeCallStackList.GetEntryForThread(const AThreadId: Integer): TIdeCallStack;
begin
  Result := TIdeCallStack(inherited EntriesForThreads[AThreadId]);
end;

procedure TIdeCallStackList.LoadDataFromXMLConfig(const AConfig: TXMLConfig;
  APath: string; AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
var
  c, i: Integer;
  e: TIdeCallStack;
begin
  Clear;
  c := AConfig.GetValue(APath + 'Count', 0);
  APath := APath + 'Entry';
  for i := 0 to c - 1 do begin
    e := TIdeCallStack.Create;
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
  if CurrentThreadId = AValue then exit;
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

function TCurrentThreads.CreateEntry(const AnAdress: TDbgPtr;
  const AnArguments: TStrings; const AFunctionName: String; const FileName,
  FullName: String; const ALine: Integer; const AThreadId: Integer;
  const AThreadName: String; const AThreadState: TDbgThreadState;
  AState: TDebuggerDataState): TThreadEntry;
begin
  Result := inherited CreateEntry(AnAdress, AnArguments, AFunctionName, FileName,
    FullName, ALine, AThreadId, AThreadName, AThreadState, AState);
  TIdeThreadEntry(Result).FThreadOwner := self;
end;

{ TIdeThreadsMonitor }

function TIdeThreadsMonitor.GetSnapshot(AnID: Pointer): TIdeThreads;
begin
  Result := TIdeThreads(FSnapshots.SnapShot[AnID]);
end;

function TIdeThreadsMonitor.GetCurrentThreads: TCurrentThreads;
begin
  Result :=TCurrentThreads(Threads);
end;

procedure TIdeThreadsMonitor.DoModified;
begin
  Changed;
end;

procedure TIdeThreadsMonitor.DoStateEnterPause;
begin
  inherited DoStateEnterPause;
  if (CurrentThreads = nil) then Exit;
  CurrentThreads.SetValidity(ddsUnknown); // TODO: this may be wrong, for any debugger that keeps threads updated while running
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

function TIdeThreadEntry.GetTopFrame: TIdeThreadFrameEntry;
begin
  Result := TIdeThreadFrameEntry(inherited TopFrame);
end;

function TIdeThreadEntry.CreateStackEntry: TCallStackEntry;
begin
  Result := TIdeThreadFrameEntry.Create;
  TIdeThreadFrameEntry(Result).FThread := Self;
end;

function TIdeThreadEntry.GetUnitInfoProvider: TDebuggerUnitInfoProvider;
begin
  if FThreadOwner = nil then
    Result := nil
  else
    Result := (FThreadOwner as TCurrentThreads).FMonitor.UnitInfoProvider;
end;

procedure TIdeThreadEntry.SetThreadState(AValue: TDbgThreadState);
begin
  if ThreadState = AValue then Exit;
  inherited SetThreadState(AValue);
  TopFrame.ClearLocation;
end;

procedure TIdeThreadEntry.SetThreadStateOnly(AValue: TDbgThreadState);
begin
  inherited SetThreadState(AValue);
end;

procedure TIdeThreadEntry.LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
begin
  TIdeCallStackEntry(TopFrame).LoadDataFromXMLConfig(AConfig, APath, AUnitInvoPrv);
  FThreadId    := AConfig.GetValue(APath + 'ThreadId', -1);
  FThreadName  := AConfig.GetValue(APath + 'ThreadName', '');
  AConfig.GetValue(APath + 'ThreadState', FThreadState, TypeInfo(TDbgThreadState));
end;

procedure TIdeThreadEntry.SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
begin
  TIdeCallStackEntry(TopFrame).SaveDataToXMLConfig(AConfig, APath, AUnitInvoPrv);
  AConfig.SetValue(APath + 'ThreadId', ThreadId);
  AConfig.SetValue(APath + 'ThreadName', ThreadName);
  AConfig.SetValue(APath + 'ThreadState', ThreadState, TypeInfo(TDbgThreadState));
end;

function TIdeThreadEntry.CreateCopy: TThreadEntry;
begin
  Result := TIdeThreadEntry.Create;
  Result.Assign(Self);
end;

{ TIdeThreads }

function TIdeThreads.GetEntry(const AnIndex: Integer): TIdeThreadEntry;
begin
  Result := TIdeThreadEntry(inherited Entries[AnIndex]);
end;

function TIdeThreads.GetEntryById(const AnID: Integer): TIdeThreadEntry;
begin
  Result := TIdeThreadEntry(inherited EntryById[AnID]);
end;

procedure TIdeThreads.LoadDataFromXMLConfig(const AConfig: TXMLConfig; APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
var
  c, i: Integer;
  e: TIdeThreadEntry;
  NewCurrentThreadId: Integer;
begin
  Clear;
  NewCurrentThreadId  := AConfig.GetValue(APath + 'CurrentThreadId', -1);
  inherited SetCurrentThreadId(NewCurrentThreadId);
  c := AConfig.GetValue(APath + 'Count', 0);
  APath := APath + 'Entry';
  for i := 0 to c - 1 do begin
    e := TIdeThreadEntry.Create;
    e.LoadDataFromXMLConfig(AConfig, APath + IntToStr(i) + '/', AUnitInvoPrv);
    List.Add(e);
  end;
end;

procedure TIdeThreads.SaveDataToXMLConfig(const AConfig: TXMLConfig; APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
var
  i: Integer;
begin
  AConfig.SetValue(APath + 'CurrentThreadId', CurrentThreadId);
  AConfig.SetDeleteValue(APath + 'Count', Count, 0);
  APath := APath + 'Entry';
  for i := 0 to Count - 1 do
    Entries[i].SaveDataToXMLConfig(AConfig, APath + IntToStr(i) + '/', AUnitInvoPrv);
end;

function TIdeThreads.CreateEntry(const AnAdress: TDbgPtr;
  const AnArguments: TStrings; const AFunctionName: String; const FileName,
  FullName: String; const ALine: Integer; const AThreadId: Integer;
  const AThreadName: String; const AThreadState: TDbgThreadState;
  AState: TDebuggerDataState): TThreadEntry;
begin
  Result := TIdeThreadEntry.Create(AnAdress, AnArguments, AFunctionName, FileName,
    FullName, ALine, AThreadId, AThreadName, AThreadState, AState);
  TIdeThreadEntry(Result).FThreadOwner := self;
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
    Assert(Master=nil, 'TManagedBreakPoint.AssignTO already has Master');
    if Master <> nil then Master.Slave := nil;
    Master := TDBGBreakPoint(Dest);
    Master.Slave := Self;
  end;
end;

procedure TIDEBreakPoint.DoChanged;
begin
  if (Master <> nil)
  and (Master.Slave = nil)
  then Master := nil;

  inherited DoChanged;
end;

procedure TIDEBreakPoint.DoUserChanged;
begin
  FUserModified := True;
  DoChanged;
end;

function TIDEBreakPoint.GetHitCount: Integer;
begin
  if Master = nil
  then Result := 0
  else Result := Master.HitCount;
end;

function TIDEBreakPoint.GetValid: TValidState;
begin
  if Master = nil
  then Result := vsUnknown
  else Result := Master.Valid;
end;

procedure TIDEBreakPoint.SetBreakHitCount(const AValue: Integer);
begin
  if BreakHitCount = AValue then exit;
  inherited SetBreakHitCount(AValue);
  DoUserChanged;
  if Master <> nil then Master.BreakHitCount := AValue;
end;

procedure TIDEBreakPoint.SetEnabled(const AValue: Boolean);
begin
  if Enabled = AValue then exit;
  inherited SetEnabled(AValue);
  InitialEnabled:=Enabled;
  if Master <> nil then Master.Enabled := AValue;
end;

procedure TIDEBreakPoint.SetInitialEnabled(const AValue: Boolean);
begin
  if InitialEnabled = AValue then exit;
  inherited SetInitialEnabled(AValue);
  DoUserChanged;
  if Master <> nil then Master.InitialEnabled := AValue;
end;

procedure TIDEBreakPoint.SetExpression(const AValue: String);
begin
  if AValue=Expression then exit;
  inherited SetExpression(AValue);
  DoUserChanged;
  if Master <> nil then Master.Expression := AValue;
end;

procedure TIDEBreakPoint.SetKind(const AValue: TDBGBreakPointKind);
begin
  if AValue=Kind then exit;
  inherited SetKind(AValue);
  DoUserChanged;
  if Master <> nil then Master.Kind := AValue;
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
  //WriteStr(s, FKind);
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
  ReleaseMaster;

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
  DebugLn(DBG_BREAKPOINT, ['DisableGroups: ', DebugText, ' Cnt=',  FDisableGroupList.Count]);
{$ENDIF}
  for n := 0 to FDisableGroupList.Count - 1 do
    FDisableGroupList[n].Enabled := False;
end;

procedure TIDEBreakPoint.DoActionChange;
begin
  //Changed;
  DoUserChanged;
end;

procedure TIDEBreakPoint.DoHit(const ACount: Integer; var AContinue,
  ANeedInternalPause: Boolean);
begin
  inherited DoHit(ACount, AContinue, ANeedInternalPause);
  AContinue := AContinue or not (bpaStop in Actions);
  if bpaLogMessage in Actions
  then Master.DoLogMessage(FLogMessage);
  if (bpaEValExpression in Actions) and (Trim(FLogEvalExpression) <> '')
  then Master.DoLogExpression(Trim(FLogEvalExpression));
  if bpaLogCallStack in Actions
  then Master.DoLogCallStack(FLogCallStackLimit);
  // SnapShot is taken in TDebugManager.DebuggerChangeState
  if bpaTakeSnapshot in Actions then
    ANeedInternalPause := True;
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
  DebugLn(DBG_BREAKPOINT, ['EnableGroups: ', DebugText, ' Cnt=',  FEnableGroupList.Count]);
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
    SetKind(TDBGBreakPointKind(GetEnumValueDef(TypeInfo(TDBGBreakPointKind),XMLConfig.GetValue(Path+'Kind/Value',''),0)));
    GroupName:=XMLConfig.GetValue(Path+'Group/Name','');
    Group:=OnGetGroup(GroupName);
    Expression:=XMLConfig.GetValue(Path+'Expression/Value','');
    AutoContinueTime:=XMLConfig.GetValue(Path+'AutoContinueTime/Value',0);
    BreakHitCount := XMLConfig.GetValue(Path+'BreakHitCount/Value',0);

    Address:=XMLConfig.GetValue(Path+'Address/Value',Int64(0)); // shouldb TDBGPtr;

    FWatchData := XMLConfig.GetValue(Path+'WatchData/Value', '');
    try ReadStr(XMLConfig.GetValue(Path+'WatchScope/Value', 'wpsGlobal'), FWatchScope);
    except FWatchScope := wpsGlobal; end;
    try ReadStr(XMLConfig.GetValue(Path+'WatchKind/Value', 'wpkWrite'), FWatchKind);
    except FWatchKind:= wpkWrite; end;

    Filename:=XMLConfig.GetValue(Path+'Source/Value','');
    if Assigned(OnLoadFilename) then OnLoadFilename(Filename);
    FSource:=Filename;

    InitialEnabled:=XMLConfig.GetValue(Path+'InitialEnabled/Value',true);
    Enabled:=InitialEnabled;
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
  AConfig.SetDeleteValue(APath+'Address/Value',Address,Int64(0)); // shouldb TDBGPtr;

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
  if Master<>nil then Master.Address := Address;

  //TODO: Why not DoUserChanged; ?
end;

procedure TIDEBreakPoint.SetLocation(const ASource: String; const ALine: Integer);
begin
  inherited SetLocation(ASource, ALine);
  if Master<>nil then Master.SetLocation(ASource, DebugExeLine);
  //TODO: Why not DoUserChanged; ?
end;

procedure TIDEBreakPoint.SetWatch(const AData: String; const AScope: TDBGWatchPointScope;
  const AKind: TDBGWatchPointKind);
begin
  inherited SetWatch(AData, AScope, AKind);
  if Master<>nil then Master.SetWatch(AData, AScope, AKind);
  //TODO: Why not DoUserChanged; ?
end;

procedure TIDEBreakPoint.ResetMaster;
begin
  if Master <> nil then Master.Slave := nil;
  Master := nil;
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

function TIDEBreakPoints.Add(const ASource: String; const ALine: Integer;
  AnUpdating: Boolean): TIDEBreakPoint;
begin
  Result := TIDEBreakPoint(inherited Add(ASource, ALine, AnUpdating));
  NotifyAdd(Result);
end;

function TIDEBreakPoints.Add(const AAddress: TDBGPtr; AnUpdating: Boolean
  ): TIDEBreakPoint;
begin
  Result := TIDEBreakPoint(inherited Add(AAddress, AnUpdating));
  NotifyAdd(Result);
end;

function TIDEBreakPoints.Add(const AData: String;
  const AScope: TDBGWatchPointScope; const AKind: TDBGWatchPointKind;
  AnUpdating: Boolean): TIDEBreakPoint;
begin
  Result := TIDEBreakPoint(inherited Add(AData, AScope, AKind, AnUpdating));
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
    // create without data. it will be set in assign (but during Begin/EndUpdate)
    BP :=  TBaseBreakPoint(FMaster.Add);
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
  IsLegacyList: Boolean;
  BreakPointPath: string;
begin
  Clear;
  IsLegacyList := XMLConfig.IsLegacyList(Path);
  NewCount := XMLConfig.GetListItemCount(Path, 'Item', IsLegacyList);

  for i:=0 to NewCount-1 do
  begin
    LoadBreakPoint := TIDEBreakPoint.Create(nil);
    BreakPointPath := Path+XMLConfig.GetListItemXPath('Item', i, IsLegacyList, True)+'/';
    LoadBreakPoint.LoadFromXMLConfig(XMLConfig, BreakPointPath, OnLoadFilename, OnGetGroup);

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
  const Path: string; const ALegacyList: Boolean;
  const OnSaveFilename: TOnSaveFilenameToConfig);
var
  Cnt: Integer;
  i: Integer;
  CurBreakPoint: TIDEBreakPoint;
  BreakPointPath: string;
begin
  Cnt:=Count;
  XMLConfig.SetListItemCount(Path,Cnt,ALegacyList);
  for i:=0 to Cnt-1 do begin
    BreakPointPath := Path+XMLConfig.GetListItemXPath('Item', i, ALegacyList, True)+'/';
    CurBreakPoint:=Items[i];
    CurBreakPoint.SaveToXMLConfig(XMLConfig, BreakPointPath, OnSaveFilename);
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
  ItemPath: string;
  IsLegacyList: Boolean;
begin
  Clear;
  IsLegacyList := XMLConfig.IsLegacyList(Path);
  NewCount := XMLConfig.GetListItemCount(Path, 'Item',IsLegacyList);
  for i := 0 to NewCount - 1 do
  begin
    NewGroup := TIDEBreakPointGroup(inherited Add);
    ItemPath := Path+XMLConfig.GetListItemXPath('Item', i, IsLegacyList, True)+'/';
    NewGroup.LoadFromXMLConfig(XMLConfig, ItemPath);
    OldGroup := FindGroupByName(NewGroup.Name, NewGroup);
    if OldGroup <> nil then
      NewGroup.Free;
  end;
end;

procedure TIDEBreakPointGroups.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; const ALegacyList: Boolean);
var
  Cnt: Integer;
  CurGroup: TIDEBreakPointGroup;
  i: Integer;
  ItemPath: string;
begin
  Cnt:=Count;
  XMLConfig.SetListItemCount(Path,Cnt,ALegacyList);
  for i := 0 to Cnt - 1 do
  begin
    CurGroup := Items[i];
    ItemPath := Path+XMLConfig.GetListItemXPath('Item', i, ALegacyList, True)+'/';
    CurGroup.SaveToXMLConfig(XMLConfig, ItemPath);
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
    if (Ignore <> Result) and (AnsiCompareText(Result.Name, GroupName) = 0) then
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
{ TIdeWatch }
{ =========================================================================== }

function TIdeWatch.CreateValueList: TWatchValueList;
begin
  Result := TIdeWatchValueList.Create(Self);
end;

constructor TIdeWatch.Create(ACollection: TCollection);
begin
  assert(((Self is TCurrentWatch) and (ACollection is TCurrentWatches)) or ((not(Self is TCurrentWatch)) and not(ACollection is TCurrentWatches)),
         'TIdewatch.Create: Watch and collection differ (current and none current)');
  inherited Create(ACollection);
end;

destructor TIdeWatch.Destroy;
begin
  FreeAndNil(FChildWatches);
  inherited Destroy;
end;

procedure TIdeWatch.ClearValues;
begin
  if FChildWatches <> nil then
    FChildWatches.ClearValues;

  inherited ClearValues;
  TCurrentWatches(Collection).Update(Self);
end;

procedure TIdeWatch.BeginChildUpdate;
begin
  if FChildWatches = nil then
    InitChildWatches;
  FChildWatches.BeginUpdate;
end;

procedure TIdeWatch.EndChildUpdate;
begin
  if FChildWatches <> nil then
    FChildWatches.EndUpdate;
end;

procedure TIdeWatch.LimitChildWatchCount(AMaxCnt: Integer;
  AKeepIndexEntriesBelow: Int64);
var
  w: TIdeWatch;
  x: int64;
  i: Integer;
begin
  i := 0;
  while (FChildWatches.Count > AMaxCnt) and (i < FChildWatches.Count) do begin
    w := FChildWatches[i];
    if TryStrToInt64(w.Expression, x) and (x < AKeepIndexEntriesBelow) then
      inc(i)
    else
      FChildWatches.Delete(0);
  end;
end;

function TIdeWatch.HasAllValidParents(AThreadId: Integer; AStackFrame: Integer
  ): boolean;
begin
  Result := FParentWatch = nil;
  if Result then
    exit;

  Result := (GetAnyValidParentWatchValue(AThreadId, AStackFrame) <> nil) and
            FParentWatch.HasAllValidParents(AThreadId, AStackFrame);
end;

procedure TIdeWatch.DoEnableChange;
begin
  Changed;
  DoModified;
end;

procedure TIdeWatch.DoExpressionChange;
begin
  Changed;
  DoModified;
end;

procedure TIdeWatch.DoDisplayFormatChanged;
begin
  Changed;
  DoModified;
end;

function TIdeWatch.GetValue(const AThreadId: Integer; const AStackFrame: Integer): TIdeWatchValue;
begin
  Result := TIdeWatchValue(inherited Values[AThreadId, AStackFrame]);
end;

function TIdeWatch.GetAnyValidParentWatchValue(AThreadId: Integer;
  AStackFrame: Integer): TIdeWatchValue;
var
  i: Integer;
  vl: TWatchValueList;
begin
  Result := nil;
  if FParentWatch = nil then
    exit;
  vl := FParentWatch.FValueList;
  i := vl.Count - 1;
  while (i >= 0) and (
    (vl.EntriesByIdx[i].Validity <> ddsValid) or
    (vl.EntriesByIdx[i].ThreadId <> AThreadId) or
    (vl.EntriesByIdx[i].StackFrame <> AStackFrame)
  ) do
    dec(i);
  if i >= 0 then
    Result := TIdeWatchValue(vl.EntriesByIdx[i]);
end;
function TIdeWatch.GetWatchDisplayName: String;
begin
  if FDisplayName <> '' then
    Result := FDisplayName
  else
    Result := FExpression;
end;

procedure TIdeWatch.SetDisplayName(AValue: String);
begin
  FDisplayName := AValue;
  Changed;
  DoModified;
end;

function TIdeWatch.GetEnabled: Boolean;
begin
  Result := inherited Enabled;
end;

function TIdeWatch.GetExpression: String;
begin
  Result := inherited Expression;
end;

procedure TIdeWatch.SetParentWatch(AValue: TIdeWatch);
begin
  if FParentWatch = AValue then Exit;
  FParentWatch := AValue;
end;

procedure TIdeWatch.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TIdeWatch then
    TIdeWatch(Dest).FDisplayName := FDisplayName;
end;

procedure TIdeWatch.InitChildWatches;
begin
  if FChildWatches <> nil then
    exit;

  FChildWatches := CreateChildWatches;
  //FChildWatches.FParentWatches := TIdeWatches(Collection);
end;

function TIdeWatch.GetTopParentWatch: TIdeWatch;
begin
  Result := Self;
  while Result.FParentWatch <> nil do
    Result := Result.FParentWatch;
end;

function TIdeWatch.GetChildWatch(ADispName, AnExpr: String): TIdeWatch;
begin
  if FChildWatches <> nil then begin
    Result := FChildWatches.Find(AnExpr);
    if Result <> nil then
      exit;
  end;

  BeginChildUpdate;
  Result := FChildWatches.Add(AnExpr);
  Result.SetParentWatch(Self);
  Result.Enabled       := Enabled;
  Result.DisplayFormat := DisplayFormat;
  Result.DbgBackendConverter := DbgBackendConverter;
  Result.FDisplayName := ADispName;
  if (defClassAutoCast in EvaluateFlags) then
    Result.EvaluateFlags := Result.EvaluateFlags + [defClassAutoCast];

  EndChildUpdate;
end;

function TIdeWatch.GetChildrenByNameAsArrayEntry(AName: Int64): TIdeWatch;
begin
  Result := GetChildWatch(IntToStr(AName),
    GetExpressionForArrayElement(Expression, AName)
  );
end;

function TIdeWatch.GetChildrenByNameAsField(AName, AClassName: String
  ): TIdeWatch;
var
  Expr: String;
begin
  Expr := Expression;
  if AClassName <> '' then
    Expr := AClassName + '(' + Expr + ')';
  Expr := Expr + '.' + AName;
  Result := GetChildWatch(AName, Expr);
end;

function TIdeWatch.CreateChildWatches: TIdeWatches;
begin
  Result := TIdeWatches.Create;
end;

procedure TIdeWatch.LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
var
  s: String;
begin
  FEnabled    := AConfig.GetValue(APath + 'Enabled', True);
  FExpression := AConfig.GetValue(APath + 'Expression', '');
  if AConfig.GetValue(APath + 'ClassAutoCast', False)
  then Include(FEvaluateFlags, defClassAutoCast)
  else Exclude(FEvaluateFlags, defClassAutoCast);
  if AConfig.GetValue(APath + 'AllowFunctionCall', False)
  then Include(FEvaluateFlags, defAllowFunctionCall)
  else Exclude(FEvaluateFlags, defAllowFunctionCall);
  if AConfig.GetValue(APath + 'AllowFunctionThreads', False)
  then Include(FEvaluateFlags, defFunctionCallRunAllThreads)
  else Exclude(FEvaluateFlags, defFunctionCallRunAllThreads);
  try    ReadStr(AConfig.GetValue(APath + 'DisplayFormat', 'wdfDefault'), FDisplayFormat);
  except FDisplayFormat := wdfDefault; end;
  FRepeatCount := AConfig.GetValue(APath + 'RepeatCount', 0);

  if AConfig.GetValue(APath + 'SkipFpDbgConv', False)
  then Include(FEvaluateFlags, defSkipValConv)
  else Exclude(FEvaluateFlags, defSkipValConv);

  s := AConfig.GetValue(APath + 'FpDbgConv', '');
  if s <> '' then
    DbgBackendConverter := DebuggerOptions.BackendConverterConfig.IdeItemByName(s);

  TIdeWatchValueList(FValueList).LoadDataFromXMLConfig(AConfig, APath + 'ValueList/');
end;

procedure TIdeWatch.SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string);
var
  s: String;
begin
  AConfig.SetDeleteValue(APath + 'Enabled', FEnabled, True);
  AConfig.SetDeleteValue(APath + 'Expression', FExpression, '');
  WriteStr(s{%H-}, FDisplayFormat);
  AConfig.SetDeleteValue(APath + 'DisplayFormat', s, 'wdfDefault');
  AConfig.SetDeleteValue(APath + 'ClassAutoCast', defClassAutoCast in FEvaluateFlags, False);
  AConfig.SetDeleteValue(APath + 'AllowFunctionCall', defAllowFunctionCall in FEvaluateFlags, False);
  AConfig.SetDeleteValue(APath + 'AllowFunctionThreads', defFunctionCallRunAllThreads in FEvaluateFlags, False);
  AConfig.SetDeleteValue(APath + 'RepeatCount', FRepeatCount, 0);

  AConfig.SetDeleteValue(APath + 'SkipFpDbgConv', defSkipValConv in FEvaluateFlags, False);
  if DbgBackendConverter <> nil then
    AConfig.SetDeleteValue(APath + 'FpDbgConv', DbgBackendConverter.Name, '');

  TIdeWatchValueList(FValueList).SaveDataToXMLConfig(AConfig, APath + 'ValueList/');
end;

{ =========================================================================== }
{ TCurrentWatch }
{ =========================================================================== }

procedure TCurrentWatch.SetSnapShot(const AValue: TIdeWatch);
begin
  assert((FSnapShot=nil) or (AValue=nil), 'TCurrentWatch already have snapshot');
  if FSnapShot = AValue then exit;
  FSnapShot := AValue;
  if FSnapShot = nil then begin
    TCurrentWatchValueList(FValueList).SnapShot := nil;
    if FChildWatches <> nil then
      TCurrentWatches(FChildWatches).SnapShot := nil;
  end else begin
    // TODO: FValueList is copied twice ?
    FSnapShot.Assign(self);
    FSnapShot.Enabled := True; // Snapshots are always enabled
    TCurrentWatchValueList(FValueList).SnapShot := TIdeWatchValueList(FSnapShot.FValueList);

    if FParentWatch <> nil then
      FSnapShot.SetParentWatch(TCurrentWatch(FParentWatch).SnapShot);

    if FChildWatches <> nil then begin
      FSnapShot.InitChildWatches;
      TCurrentWatches(FChildWatches).SnapShot := FSnapShot.FChildWatches;
    end;
  end;
end;

function TCurrentWatch.CreateChildWatches: TIdeWatches;
begin
  Result := TCurrentWatches.Create(TCurrentWatches(Collection).FMonitor);
  if FSnapShot <> nil then begin
    FSnapShot.InitChildWatches;
    TCurrentWatches(Result).SnapShot := FSnapShot.FChildWatches;
  end;
end;

procedure TCurrentWatch.SetParentWatch(AValue: TIdeWatch);
begin
  inherited SetParentWatch(AValue);

  if (SnapShot <> nil) and (FParentWatch <> nil) then
    SnapShot.SetParentWatch(TCurrentWatch(FParentWatch).SnapShot);
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
  if FParentWatch <> nil then begin
    if not (FParentWatch is TCurrentWatch) then
      exit;
    TCurrentWatch(FParentWatch).RequestData(AWatchValue);
    exit;
  end;

  if Collection <> nil
  then TCurrentWatches(Collection).RequestData(AWatchValue)
  else AWatchValue.Validity := ddsInvalid;
end;

destructor TCurrentWatch.Destroy;
var
  w: TCurrentWatches;
  s: TIdeWatch;
begin
  if FSnapShot <> nil then begin
    s := FSnapShot;
    SnapShot := Nil;
    FreeAndNil(s);
  end;

  if (TCurrentWatches(Collection) <> nil)
  then begin
    TCurrentWatches(Collection).NotifyRemove(Self);
    TCurrentWatches(Collection).DoModified;
  end;
  w := TCurrentWatches(Collection);
  inc(w.FSkipUpdatedNotification);
  inherited Destroy;
  dec(w.FSkipUpdatedNotification);
end;

procedure TCurrentWatch.LoadFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
var
  i: Integer;
  s: String;
begin
  Expression := AConfig.GetValue(APath + 'Expression/Value', '');
  Enabled := AConfig.GetValue(APath + 'Enabled/Value', true);
  if AConfig.GetValue(APath + 'ClassAutoCast', False)
  then Include(FEvaluateFlags, defClassAutoCast)
  else Exclude(FEvaluateFlags, defClassAutoCast);
  if AConfig.GetValue(APath + 'AllowFunctionCall', False)
  then Include(FEvaluateFlags, defAllowFunctionCall)
  else Exclude(FEvaluateFlags, defAllowFunctionCall);
  if AConfig.GetValue(APath + 'AllowFunctionThreads', False)
  then Include(FEvaluateFlags, defFunctionCallRunAllThreads)
  else Exclude(FEvaluateFlags, defFunctionCallRunAllThreads);
  i := StringCase
    (AConfig.GetValue(APath + 'DisplayStyle/Value', TWatchDisplayFormatNames[wdfDefault]),
    TWatchDisplayFormatNames);
  if i >= 0
  then DisplayFormat := TWatchDisplayFormat(i)
  else DisplayFormat := wdfDefault;
  FRepeatCount := AConfig.GetValue(APath + 'RepeatCount', 0);

  if AConfig.GetValue(APath + 'SkipFpDbgConv', False)
  then Include(FEvaluateFlags, defSkipValConv)
  else Exclude(FEvaluateFlags, defSkipValConv);

  s := AConfig.GetValue(APath + 'FpDbgConv', '');
  if s <> '' then
    DbgBackendConverter := DebuggerOptions.BackendConverterConfig.IdeItemByName(s);
end;

procedure TCurrentWatch.SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string);
begin
  AConfig.SetDeleteValue(APath + 'Expression/Value', Expression, '');
  AConfig.SetDeleteValue(APath + 'Enabled/Value', Enabled, true);
  AConfig.SetDeleteValue(APath + 'DisplayStyle/Value',
    TWatchDisplayFormatNames[DisplayFormat], TWatchDisplayFormatNames[wdfDefault]);
  AConfig.SetDeleteValue(APath + 'ClassAutoCast', defClassAutoCast in FEvaluateFlags, False);
  AConfig.SetDeleteValue(APath + 'AllowFunctionCall', defAllowFunctionCall in FEvaluateFlags, False);
  AConfig.SetDeleteValue(APath + 'AllowFunctionThreads', defFunctionCallRunAllThreads in FEvaluateFlags, False);
  AConfig.SetDeleteValue(APath + 'RepeatCount', FRepeatCount, 0);

  AConfig.SetDeleteValue(APath + 'SkipFpDbgConv', defSkipValConv in FEvaluateFlags, False);
  if DbgBackendConverter <> nil then
    AConfig.SetDeleteValue(APath + 'FpDbgConv', DbgBackendConverter.Name, '');
end;

{ =========================================================================== }
{ TIdeWatches }
{ =========================================================================== }

function TIdeWatches.Add(const AExpression: String): TIdeWatch;
begin
  Result := TIdeWatch(inherited Add); // calls update
  Result.Expression := AExpression;
end;

function TIdeWatches.GetItem(const AnIndex: Integer): TIdeWatch;
begin
  Result := TIdeWatch(inherited Items[AnIndex]);
end;

procedure TIdeWatches.SetItem(const AnIndex: Integer; const AValue: TIdeWatch);
begin
  inherited Items[AnIndex] := AValue;
end;

function TIdeWatches.WatchClass: TWatchClass;
begin
  Result := TIdeWatch;
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

{$IfOpt C+}
function TIdeWatches.Add: TCollectionItem;
begin
  assert(False, 'TIdeWatches.Add: False');
end;
{$EndIf}

function TIdeWatches.Find(const AExpression: String): TIdeWatch;
begin
  Result := TIdeWatch(inherited Find(AExpression));
end;

{ =========================================================================== }
{ TCurrentWatches }
{ =========================================================================== }

function TCurrentWatches.Add(const AExpression: String): TCurrentWatch;
var
  R: TIdeWatch;
begin
  // if this is modified, then also update LoadFromXMLConfig
  inc(FSkipUpdatedNotification);
  Result := TCurrentWatch(inherited Add(AExpression));
  dec(FSkipUpdatedNotification);
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
  R: TIdeWatch;
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
  IsLegacyList: Boolean;
  ItemPath: string;
begin
  if FMonitor <> nil then
    FMonitor.BeginIgnoreModified;
  try
    Clear;
    IsLegacyList := AConfig.IsLegacyList(APath);
    NewCount := AConfig.GetListItemCount(APath, 'Item', IsLegacyList);
    for i := 0 to NewCount-1 do
    begin
      // Call inherited Add, so NotifyAdd can be send, after the Watch was loaded
      Watch := TCurrentWatch(inherited Add(''));
      ItemPath := APath+AConfig.GetListItemXPath('Item', i, IsLegacyList, True)+'/';
      Watch.LoadFromXMLConfig(AConfig, ItemPath);
      NotifyAdd(Watch);
    end;
  finally
    if FMonitor <> nil then
      FMonitor.EndIgnoreModified;
  end;
end;

procedure TCurrentWatches.NotifyAdd(const AWatch: TCurrentWatch);
begin
  if UpdateCount > 0 then begin
    AWatch.FAdded := True;
    exit;
  end;
  AWatch.FAdded := False;
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

procedure TCurrentWatches.SaveToXMLConfig(const AConfig: TXMLConfig;
  const APath: string; const ALegacyList: Boolean);
var
  Cnt: Integer;
  i: Integer;
  Watch: TCurrentWatch;
  ItemPath: string;
begin
  Cnt := Count;
  AConfig.SetListItemCount(APath, Cnt, ALegacyList);
  for i := 0 to Cnt - 1 do
  begin
    Watch := Items[i];
    ItemPath := APath+AConfig.GetListItemXPath('Item', i, ALegacyList, True)+'/';
    Watch.SaveToXMLConfig(AConfig, ItemPath);
  end;
end;

procedure TCurrentWatches.SetItem(const AnIndex: Integer; const AValue: TCurrentWatch);
begin
  inherited SetItem(AnIndex, AValue);
end;

function TCurrentWatches.WatchClass: TWatchClass;
begin
  Result := TCurrentWatch;
end;

procedure TCurrentWatches.Update(Item: TCollectionItem);
var
  m, c: Integer;
begin
  if (UpdateCount > 0) or (FSkipUpdatedNotification > 0) then
    exit;

  if Item <> nil then begin
    FMonitor.NotifyUpdate(Self, TCurrentWatch(Item));
  end else begin
    m := 0;
    c := Count;
    while m < c do begin
      if Items[m].FAdded then
        NotifyAdd(Items[m]);
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

{ TIdeLocalsValue }

procedure TIdeLocalsValue.LimitChildWatchCount(AMaxCnt: Integer;
  AKeepIndexEntriesBelow: Int64);
begin
  //
end;

procedure TIdeLocalsValue.ClearDisplayData;
begin
  //
end;

function TIdeLocalsValue.GetDisplayName: String;
begin
  Result := FDisplayName;
  if Result = '' then
    Result := Name;
end;

function TIdeLocalsValue.GetExpression: String;
begin
  Result := Name;
end;

function TIdeLocalsValue.GetEnabled: Boolean;
begin
  Result := True;
end;

function TIdeLocalsValue.GetValidity: TDebuggerDataState;
begin
  if ResultData <> nil then
    Result := ddsValid
  else
    Result := ddsRequested;
end;

function TIdeLocalsValue.GetDisplayFormat: TWatchDisplayFormat;
begin
  Result := wdfDefault;
end;

function TIdeLocalsValue.GetTypeInfo: TDBGType;
begin
  Result := nil;
end;

function TIdeLocalsValue.GetChildrenByNameAsArrayEntry(AName: Int64): TObject;
begin
  Result := GetSubLocal(IntToStr(AName),
    GetExpressionForArrayElement(Name, AName)
  );
end;

function TIdeLocalsValue.GetChildrenByNameAsField(AName, AClassName: String
  ): TObject;
var
  Expr: String;
begin
  Expr := Name;
  if AClassName <> '' then
    Expr := AClassName + '(' + Expr + ')';
  Expr := Expr + '.' + AName;
  Result := GetSubLocal(AName, Expr);
end;

procedure TIdeLocalsValue.CreateSubLocals;
begin
  if FSubLocals = nil then
    FSubLocals := TSubLocals.Create(ThreadId, StackFrame, TIDELocals(Owner));
end;

function TIdeLocalsValue.GetSubLocal(ADispName, AnExpr: String
  ): TIdeLocalsValue;
begin
  if FSubLocals = nil then begin
    CreateSubLocals;
  end
  else begin
    Result := TIdeLocalsValue(FSubLocals.Find(AnExpr));
    if Result <> nil then
      exit;
  end;

  Result := FSubLocals.Add(AnExpr);
  Result.DisplayName := ADispName;
end;

procedure TIdeLocalsValue.SetDisplayName(AValue: String);
begin
  FDisplayName := AValue;
end;

procedure TIdeLocalsValue.DoAssign(AnOther: TDbgEntityValue);
begin
  inherited DoAssign(AnOther);
  if AnOther is TIdeLocalsValue then begin
    FDisplayName := TIdeLocalsValue(AnOther).FDisplayName;
    // skip SubLocals
  end;
end;

destructor TIdeLocalsValue.Destroy;
begin
  inherited Destroy;
  FSubLocals.Free;
end;

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
        TWatchResultData.CreateFromXMLConfig(AConfig, APath + IntToStr(i) + '/')
       );

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
    Values[i].SaveDataToXMLConfig(AConfig, APath + IntToStr(i) + '/');
  end;
end;

function TIDELocals.CreateEntry: TDbgEntityValue;
begin
  Result := TIdeLocalsValue.Create;
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

{ THistoryLocalValue }

procedure THistoryLocalValue.SetSnapShot(const AValue: TIdeLocalsValue);
begin
  FSnapShot := AValue;
  FSnapShot.Assign(Self);

  if FSubLocals <> nil then begin
    FSnapShot.CreateSubLocals;
    FSubLocals.SetSnapShot(FSnapShot.FSubLocals);
  end;
end;

procedure THistoryLocalValue.CreateSubLocals;
begin
  inherited CreateSubLocals;
  if FSnapShot <> nil then begin
    FSnapShot.CreateSubLocals;
    FSubLocals.SetSnapShot(FSnapShot.FSubLocals);
  end;
end;

procedure THistoryLocalValue.SetDisplayName(AValue: String);
begin
  inherited SetDisplayName(AValue);
  if FSnapShot <> nil then
    FSnapShot.DisplayName := AValue;
end;

{ THistoryLocals }

procedure THistoryLocals.SetSnapShot(const AValue: TIDELocals);
begin
  FSnapShot := AValue;
end;

{ TSubLocalsValue }

function TSubLocalsValue.ResData: IDbgWatchDataIntf;
begin
  if FCurrentResData = nil then
    FCurrentResData := TCurrentResData.Create;
  Result := FCurrentResData;
end;

procedure TSubLocalsValue.DoBeginUpdating;
begin
  AddReference;
  FValidity := ddsEvaluating;
end;

procedure TSubLocalsValue.DoEndUpdating;
begin
  FCurrentResData := FCurrentResData.RootResultData;
  // TODO: maybe create an error entry, if only FNewResultData is missing
  if (FCurrentResData <> nil) then begin
    if (FCurrentResData.FNewResultData = nil) then begin
      FreeAndNil(FCurrentResData);
      FValidity := ddsInvalid;
      ReleaseReference;
      exit;
    end;

    FCurrentResData.Done;
    FValue := FCurrentResData.FNewResultData;
    FreeAndNil(FCurrentResData);

    if FValidity = ddsEvaluating then
      FValidity := ddsValid;
  end
  else
    FValidity := ddsInvalid;

  if FSnapShot <> nil then begin
    TSubLocalsValue(FSnapShot).FValidity := FValidity;
    TSubLocalsValue(FSnapShot).FValue := FValue.CreateCopy;
  end;

  if FOnChange <> nil then
    FOnChange(Self);

  ReleaseReference;
end;

procedure TSubLocalsValue.RequestData;
begin
  if(DebugBossManager <> nil) and
     (FValidity = ddsUnknown) and
     (TSubLocals(Owner).TopOwner is TCurrentLocals) and
     (inherited GetResultData = nil)
  then begin
    FValidity := ddsRequested;
    DebugBossManager.RequestWatchData(Self);
  end;
end;

function TSubLocalsValue.GetResultData: TWatchResultData;
begin
  RequestData;
  Result := inherited GetResultData;
end;

function TSubLocalsValue.GetValue: String;
begin
  RequestData;
  case FValidity of
    ddsRequested, ddsEvaluating: Result := '<evaluating>';
    ddsValid:                    Result := inherited GetValue;
    ddsInvalid:                  Result := '<invalid>';
    ddsError:                    Result := '<Error: '+ (inherited GetValue) +'>';
    else Result := '<not evaluated>';
  end;
end;

destructor TSubLocalsValue.Destroy;
begin
  inherited Destroy;
  DoDestroy;
end;

function TSubLocalsValue.GetEvaluateFlags: TWatcheEvaluateFlags;
begin
  Result := [];
end;

function TSubLocalsValue.GetDbgValConverter: ILazDbgValueConvertSelectorIntf;
begin
  Result := nil;
end;

function TSubLocalsValue.GetFirstIndexOffs: Int64;
begin
  Result := 0;
end;

function TSubLocalsValue.GetRepeatCount: Integer;
begin
  Result := 0;
end;

function TSubLocalsValue.GetValidity: TDebuggerDataState;
begin
  RequestData;
  Result := FValidity;
end;

procedure TSubLocalsValue.SetTypeInfo(AValue: TDBGTypeBase);
begin
  //assert(False, 'TSubLocalsValue.SetTypeInfo: False');
end;

procedure TSubLocalsValue.SetValidity(AValue: TDebuggerDataState);
begin
  FValidity := AValue;
  if not IsUpdating then begin
    AddReference;
    DoEndUpdating;
  end;
end;

procedure TSubLocalsValue.SetValue(AValue: String);
begin
  FValue.Free;
  FValue := TWatchResultDataPrePrinted.Create(AValue);
end;

{ TSubLocals }

function TSubLocals.CreateEntry: TDbgEntityValue;
begin
  Result := TSubLocalsValue.Create;
end;

function TSubLocals.Add(const AName: String): TIdeLocalsValue;
var
  V: TSubLocalsValue;
begin
  Result := TIdeLocalsValue(CreateEntry);
  Result.FName := AName;
  Add(Result);

  if FSnapShot <> nil then begin
    V := TSubLocalsValue(FSnapShot.Add('', nil));
    assert(V is TSubLocalsValue, 'TSubLocals.Add: V is TSubLocalsValue');
    assert(Result is TSubLocalsValue, 'TSubLocals.Add: Result is TSubLocalsValue');
    TSubLocalsValue(Result).SetSnapShot(V);
  end;
end;

procedure TSubLocals.SetSnapShot(const AValue: TIDELocals);
var
  V: TSubLocalsValue;
  i: Integer;
begin
  assert((FSnapShot=nil) or (AValue=nil), 'TSubLocals.SetSnapShot: (FSnapShot=nil) or (AValue=nil)');
  inherited SetSnapShot(AValue);

  if FSnapShot <> nil then begin
    assert(FSnapShot is TSubLocals, 'TSubLocals.SetSnapShot: FSnapShot is TSubLocals');
    FSnapShot.Clear;
    for i := 0 to Count - 1 do begin
      V := TSubLocalsValue(FSnapShot.Add('', nil));
      TSubLocalsValue(Entries[i]).SetSnapShot(V);
    end;
  end;
end;

function TSubLocals.TopOwner: TIDELocals;
begin
  Result := FOwnerLocals;
  while (Result <> nil) and (Result is TSubLocals) do
    Result := TSubLocals(Result).FOwnerLocals;
end;

constructor TSubLocals.Create(AThreadId, AStackFrame: Integer;
  AOwnerLocals: TIDELocals);
begin
  inherited Create(AThreadId, AStackFrame);
  FOwnerLocals := AOwnerLocals;
end;

{ =========================================================================== }
{ TCurrentLocals }
{ =========================================================================== }

procedure TCurrentLocals.SetSnapShot(const AValue: TIDELocals);
var
  V: TIdeLocalsValue;
  i: Integer;
begin
  assert((FSnapShot=nil) or (AValue=nil), 'TCurrentLocals already have snapshot');
  if FSnapShot = AValue then exit;
  inherited SetSnapShot(AValue);

  if FSnapShot <> nil then begin
    FSnapShot.Clear;
    for i := 0 to Count - 1 do begin
      V := TIdeLocalsValue(FSnapShot.Add('', nil));
      TCurrentLocalValue(Entries[i]).SetSnapShot(V);
    end;
  end;
end;

function TCurrentLocals.GetStackFrame: Integer;
begin
  Result := StackFrame;
end;

function TCurrentLocals.GetThreadId: Integer;
begin
  Result := ThreadId;
end;

procedure TCurrentLocals.DoBeginUpdating;
begin
  AddReference;
  Clear;
  if (FCurrentResList = nil) then
    FCurrentResList := TRefCntObjList.Create
  else
    FCurrentResList.Clear;
  FCurrentValidity := ddsValid;
end;

procedure TCurrentLocals.DoEndUpdating;
var
  i: Integer;
begin
  FinishCurrentRes(True);

  for i := 0 to FCurrentResList.Count - 1 do
    Add(TLocalsValue(FCurrentResList[i]));
  FCurrentResList.Clear;

  SetDataValidity(FCurrentValidity);
  ReleaseReference;
end;

procedure TCurrentLocals.SetValidity(AValue: TDebuggerDataState);
begin
  if UpdateCount > 0 then begin
    FCurrentValidity := AValue;
  end
  else begin
    if AValue <> ddsValid then
      Clear
    else
    if FCurrentResData <> nil then
      FinishCurrentRes;
    SetDataValidity(AValue);
  end;
end;

function TCurrentLocals.Add(AName: String): IDbgWatchDataIntf;
begin
  FinishCurrentRes;
  FCurrentResName := AName;
  FCurrentResData := TCurrentResData.Create;
  Result := FCurrentResData;
end;

procedure TCurrentLocals.FinishCurrentRes(AnInUpdate: Boolean);
var
  v: TLocalsValue;
begin
  FCurrentResData := FCurrentResData.RootResultData;
  // TODO: maybe create an error entry, if only FNewResultData is missing
  if (FCurrentResData = nil) then
    exit;
  if (FCurrentResData.FNewResultData = nil) then begin
    FreeAndNil(FCurrentResData);
    exit;
  end;

  FCurrentResData.Done;

  v := TLocalsValue(CreateEntry);
  v.Init(FCurrentResName, FCurrentResData.FNewResultData);

  if IsUpdating or AnInUpdate then
    FCurrentResList.Add(v)
  else
    Add(v);

  FreeAndNil(FCurrentResData);
end;

function TCurrentLocals.CreateEntry: TDbgEntityValue;
begin
  Result := TCurrentLocalValue.Create;
end;

constructor TCurrentLocals.Create(AMonitor: TIdeLocalsMonitor; AThreadId, AStackFrame: Integer);
begin
  FMonitor := AMonitor;
  FDataValidity := ddsUnknown;
  inherited Create(AThreadId, AStackFrame);
end;

destructor TCurrentLocals.Destroy;
begin
  inherited Destroy;
  DoDestroy;

  FCurrentResData := FCurrentResData.RootResultData;
  if (FCurrentResData <> nil) {and (FResultData = nil)} then
    FCurrentResData.FreeResultAndSubData;
  FCurrentResData.Free;

  FCurrentResList.Free;

  DoDestroy;
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

procedure TCurrentLocals.Add(AnEntry: TDbgEntityValue);
var
  V: TIdeLocalsValue;
begin
  inherited Add(AnEntry);
  assert(AnEntry is TCurrentLocalValue, 'TCurrentLocals.Add: AnEntry is TCurrentLocalValue');
  if FSnapShot <> nil then begin
    V := TIdeLocalsValue(FSnapShot.Add('', nil));
    TCurrentLocalValue(AnEntry).SetSnapShot(V);
  end;
end;

function TCurrentLocals.Add(const AName: String; AValue: TWatchResultData
  ): TLocalsValue;
var
  V: TIdeLocalsValue;
begin
  Result := inherited Add(AName, AValue);
  if FSnapShot <> nil then begin
    V := TIdeLocalsValue(FSnapShot.Add('', nil));
    TCurrentLocalValue(Result).SetSnapShot(V);
  end;
end;

procedure TCurrentLocals.SetDataValidity(AValidity: TDebuggerDataState);
begin
  if FDataValidity = AValidity then exit;

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
  if Owner is TCurrentIDERegisters then
    TCurrentIDERegisters(Owner).DoDataValidityChanged(AnOldValidity);
end;

procedure TIDERegisterValue.DoDisplayFormatChanged(AnOldFormat: TRegisterDisplayFormat);
begin
  if not HasValueFormat[DisplayFormat] then begin
    DataValidity := ddsRequested;
    if Owner is TCurrentIDERegisters then
      TCurrentIDERegisters(Owner).FMonitor.RequestData(TCurrentIDERegisters(Owner));
  end
  else
  if Owner is TCurrentIDERegisters then
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
  if DataValidity = ddsUnknown then begin
    AddReference;
    try
      Result := 0;
      DataValidity := ddsRequested;
      FMonitor.RequestData(Self);  // Locals can be cleared, if debugger is "run" again
    finally
      ReleaseReference;
    end;
  end;
  result := TDbgEntityValuesList(self).Count;
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

function TIdeRegistersMonitor.GetCurrentRegistersList: TCurrentIDERegistersList;
begin
  Result := TCurrentIDERegistersList(RegistersList);
end;

procedure TIdeRegistersMonitor.DoStateEnterPause;
begin
  inherited DoStateEnterPause;
  if CurrentRegistersList = nil then exit;
  InvalidateItems;
end;

procedure TIdeRegistersMonitor.DoStateLeavePauseClean;
begin
  inherited DoStateLeavePauseClean;
  if CurrentRegistersList = nil then exit;
  InvalidateItems;
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

procedure TIdeRegistersMonitor.InvalidateItems;
begin
  CurrentRegistersList.InvalidateItems;
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

constructor TIdeCallStackEntry.Create(const AIndex: Integer;
  const AnAdress: TDbgPtr; const AnArguments: TStrings;
  const AFunctionName: String; const AUnitInfo: TDebuggerUnitInfo;
  const ALine: Integer; AState: TDebuggerDataState = ddsValid);
begin
  inherited Create;
  SetUnitInfo(AUnitInfo);
  InitFields(AIndex, AnAdress, AnArguments, AFunctionName, ALine, AState);
end;

function TIdeCallStackEntry.CreateCopy: TCallStackEntry;
begin
  Result := TIdeCallStackEntry.Create;
  Result.Assign(Self);
end;

procedure TIdeCallStackEntry.Assign(AnOther: TCallStackEntry);
begin
  FUnitInfo.ReleaseReference;
  inherited Assign(AnOther);
  if AnOther is TIdeCallStackEntry then begin
    FUnitInfo := TIdeCallStackEntry(AnOther).FUnitInfo;
    if FUnitInfo <> nil then
      FUnitInfo.AddReference;
  end;
end;

destructor TIdeCallStackEntry.Destroy;
begin
  inherited;
  if FUnitInfo <> nil then FUnitInfo.ReleaseReference;
end;

procedure TIdeCallStackEntry.Init(const AnAdress: TDbgPtr; const AnArguments: TStrings;
  const AFunctionName: String; const AUnitName, AClassName, AProcName, AFunctionArgs: String;
  const ALine: Integer; AState: TDebuggerDataState);
var
  loc: TDebuggerUnitInfo;
begin
  assert((FOwner = nil) or (FOwner is TCurrentCallStack), 'FOwner is TCurrentCallStack');
  inherited Init(AnAdress, AnArguments, AFunctionName, AUnitName, AClassName, AProcName,
      AFunctionArgs, ALine, AState);

  if GetUnitInfoProvider = nil then
    loc := nil
  else
    loc := GetUnitInfoProvider.GetUnitInfoByFunction(AUnitName, AClassName, AProcName, AFunctionArgs);

  SetUnitInfo(loc);
end;

procedure TIdeCallStackEntry.Init(const AnAdress: TDbgPtr; const AnArguments: TStrings;
  const AFunctionName: String; const FileName, FullName: String; const ALine: Integer;
  AState: TDebuggerDataState);
var
  loc: TDebuggerUnitInfo;
begin
  assert((FOwner = nil) or (FOwner is TCurrentCallStack), 'FOwner is TCurrentCallStack');
  inherited Init(AnAdress, AnArguments, AFunctionName, FileName, FullName, ALine, AState);

  if GetUnitInfoProvider = nil then
    loc := nil
  else
    loc := GetUnitInfoProvider.GetUnitInfoFor(FileName, FullName);

  SetUnitInfo(loc);
end;

function TIdeCallStackEntry.IsCurrent: Boolean;
begin
  Result := (FOwner <> nil) and (FOwner.CurrentIndex = Self.Index);
  //TODO: check current thread
end;

procedure TIdeCallStackEntry.MakeCurrent;
begin
  if FOwner = nil then Exit;
  if IsCurrent then exit;
  FOwner.ChangeCurrentIndex(Self.Index);
end;

function TIdeCallStackEntry.GetFunctionName: String;
begin
  case Validity of
    ddsValid:     Result := inherited GetFunctionName;
    ddsError:     Result := '<Error: '+(inherited GetFunctionName)+'>';
    ddsInvalid:   Result := '<invalid>';
    ddsRequested, ddsEvaluating: Result := '<evaluating>';
    ddsUnknown:                  Result := '<unknown>';
  end;
end;

function TIdeCallStackEntry.GetSource: String;
begin
  if (Validity = ddsValid)  and (FUnitInfo <> nil)
  then Result := FUnitInfo.FileName
  else Result := '';
end;

procedure TIdeCallStackEntry.SetUnitInfo(AUnitInfo: TDebuggerUnitInfo);
begin
  if FUnitInfo <> nil then FUnitInfo.ReleaseReference;
  FUnitInfo := AUnitInfo;
  if FUnitInfo <> nil then FUnitInfo.AddReference;
end;

function TIdeCallStackEntry.GetUnitInfoProvider: TDebuggerUnitInfoProvider;
begin
  assert(FOwner <> nil, 'FOwner <> nil');
  Result := (FOwner as TCurrentCallStack).FMonitor.UnitInfoProvider;
end;

procedure TIdeCallStackEntry.LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
var
  UInfo: TDebuggerUnitInfo;
  i: Integer;
  AState: TDebuggerDataState;
  NewIndex, NewLine: Integer;
  NewAddress: QWord;
  NewFunctionName: String;
begin
  NewIndex          := AConfig.GetValue(APath + 'Index', 0);
  NewAddress        := StrToQWordDef(AConfig.GetValue(APath + 'Address', '0'), 0);
  NewFunctionName   := AConfig.GetValue(APath + 'FunctionName', '');
  NewLine           := AConfig.GetValue(APath + 'Line', 0);
  InitFields(NewIndex, NewAddress, nil, NewFunctionName, NewLine, ddsUnknown);

  Arguments.Text  := AConfig.GetValue(APath + 'Arguments', '');

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
    ReadStr(AConfig.GetValue(APath + 'State', 'ddsUnknown'), AState);
    Validity := AState;
  except
    Validity := ddsUnknown;
  end;
end;

procedure TIdeCallStackEntry.SaveDataToXMLConfig(const AConfig: TXMLConfig; APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
var
  s: string;
  i: Integer;
begin
  AConfig.SetValue(APath + 'Index', Index);
  AConfig.SetValue(APath + 'Address', IntToStr(Address));
  AConfig.SetValue(APath + 'FunctionName', FunctionName);
  AConfig.SetValue(APath + 'Line', Line);
  AConfig.SetValue(APath + 'Arguments', Arguments.Text);
  if FUnitInfo <> nil then begin
    if AUnitInvoPrv <> nil
    then begin
      i := AUnitInvoPrv.IndexOf(FUnitInfo, True);
      AConfig.SetValue(APath + 'UnitInfoRef', i);
    end
    else
      FUnitInfo.SaveDataToXMLConfig(AConfig, APath + 'UnitInfo/');
  end;
  WriteStr(s{%H-}, Validity);
  AConfig.SetValue(APath + 'State', s);
end;

procedure TIdeCallStackEntry.ClearLocation;
begin
  inherited ClearLocation;
  SetUnitInfo(TDebuggerUnitInfo.Create('',''));
end;

{ =========================================================================== }
{ TCallStack }
{ =========================================================================== }

procedure TIdeCallStack.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TObject(FList[i]).Free;
  FList.Clear;
end;

function TIdeCallStack.GetCount: Integer;
begin
  Result := FList.Count;
end;

destructor TIdeCallStack.Destroy;
begin
  Clear;
  inherited Destroy;
  FreeAndNil(FList);
end;

function TIdeCallStack.GetEntry(AIndex: Integer): TIdeCallStackEntry;
begin
  if (AIndex < 0)
  or (AIndex >= CountLimited(AIndex+1)) then IndexError(Aindex);

  Result := TIdeCallStackEntry(FList[AIndex]);
end;

procedure TIdeCallStack.AddEntry(AnEntry: TIdeCallStackEntry);
begin
  // must be added in correct order
  Flist.Add(AnEntry);
  AnEntry.FOwner := Self;
end;

procedure TIdeCallStack.AssignEntriesTo(AnOther: TIdeCallStack);
var
  i: Integer;
begin
  for i := 0 to FList.Count-1 do begin
    AnOther.AddEntry(TIdeCallStackEntry(FList[i]).CreateCopy as TIdeCallStackEntry);
  end;
end;

procedure TIdeCallStack.LoadDataFromXMLConfig(const AConfig: TXMLConfig; APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
var
  c, i: Integer;
  e: TIdeCallStackEntry;
begin
  Clear;
  FThreadId := AConfig.GetValue(APath + 'ThreadId', -1);
  FCurrent  := AConfig.GetValue(APath + 'Current', -1);

  c := AConfig.GetValue(APath + 'Count', 0);
  APath := APath + 'Entry';
  for i := 0 to c - 1 do begin
    e := TIdeCallStackEntry.Create();
    e.FOwner := self;
    e.LoadDataFromXMLConfig(AConfig, APath + IntToStr(i) + '/', AUnitInvoPrv);
    FList.Add(e);
  end;
end;

procedure TIdeCallStack.SaveDataToXMLConfig(const AConfig: TXMLConfig; APath: string;
  AUnitInvoPrv: TDebuggerUnitInfoProvider = nil);
var
  i: Integer;
begin
  AConfig.SetValue(APath + 'ThreadId', FThreadId);
  AConfig.SetValue(APath + 'Current', FCurrent);

  AConfig.SetDeleteValue(APath + 'Count', FList.Count, 0);
  APath := APath + 'Entry';
  for i := 0 to FList.Count - 1 do
    TIdeCallStackEntry(FList[i]).SaveDataToXMLConfig(AConfig, APath + IntToStr(i) + '/', AUnitInvoPrv);
end;

procedure TIdeCallStack.DoEntriesCreated;
begin
  assert(False, 'TCallStack.DoEntriesCreated');
end;

procedure TIdeCallStack.DoEntriesUpdated;
begin
  assert(False, 'TCallStack.DoEntriesUpdated');
end;

procedure TIdeCallStack.SetCountValidity(AValidity: TDebuggerDataState);
begin
  assert(False, 'TCallStack.SetCountValidity');
end;

procedure TIdeCallStack.SetHasAtLeastCountInfo(AValidity: TDebuggerDataState; AMinCount: Integer);
begin
  assert(False, 'TCallStack.SetHasAtLeastCountInfo');
end;

procedure TIdeCallStack.SetCurrentValidity(AValidity: TDebuggerDataState);
begin
  assert(False, 'TCallStack.SetCurrentValidity');
end;

function TIdeCallStack.GetCountValidity: TDebuggerDataState;
begin
  Result := ddsValid;
end;

function TIdeCallStack.IndexError(AIndex: Integer): TIdeCallStackEntry;
begin
  Result:=nil;
  raise EInvalidOperation.CreateFmt('Index out of range (%d)', [AIndex]);
end;

function TIdeCallStack.GetEntryBase(AIndex: Integer): TCallStackEntry;
begin
  Result := TCallStackEntry(GetEntry(AIndex));
end;

procedure TIdeCallStack.PrepareRange(AIndex, ACount: Integer);
begin
end;

procedure TIdeCallStack.ChangeCurrentIndex(ANewIndex: Integer);
begin
  CurrentIndex := ANewIndex;
end;

function TIdeCallStack.HasAtLeastCount(ARequiredMinCount: Integer): TNullableBool;
begin
  if ARequiredMinCount <= Count then
    Result := nbTrue
  else
    Result := nbFalse;
end;

function TIdeCallStack.CountLimited(ALimit: Integer): Integer;
begin
  case HasAtLeastCount(ALimit) of
    nbTrue:    Result := ALimit;
    nbFalse:   Result := Count;
    else       Result := 0;
  end;
end;

procedure TIdeCallStack.SetCount(ACount: Integer);
begin
  // can not set count
  assert(False, 'TCallStack.SetCount should not be called')
end;

procedure TIdeCallStack.Assign(AnOther: TCallStackBase);
begin
  Clear;
  inherited Assign(AnOther);
  TIdeCallStack(AnOther).AssignEntriesTo(Self);
end;

constructor TIdeCallStack.Create;
begin
  FList := TList.Create;
  inherited;
end;

function TIdeCallStack.CreateCopy: TCallStackBase;
begin
  Result := TIdeCallStack.Create;
  Result.Assign(Self);
end;

function TIdeCallStack.GetRawEntries: TMap;
begin
  assert(False, 'TCallStack.GetRawEntries');
  Result := nil;
end;

function TIdeCallStack.GetNewCurrentIndex: Integer;
begin
  assert(False, 'TCallStack.GetNewCurrentIndex');
  Result := inherited GetNewCurrentIndex;
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

procedure TIdeCallStackMonitor.RequestCount(ACallstack: TIdeCallStack);
begin
  if (Supplier <> nil) and (ACallstack is TCurrentCallStack)
  then Supplier.RequestCount(TCurrentCallStack(ACallstack));
end;

procedure TIdeCallStackMonitor.RequestAtLeastCount(ACallstack: TIdeCallStack;
  ARequiredMinCount: Integer);
begin
  if (Supplier <> nil) and (ACallstack is TCurrentCallStack)
  then Supplier.RequestAtLeastCount(TCurrentCallStack(ACallstack), ARequiredMinCount);
end;

procedure TIdeCallStackMonitor.RequestCurrent(ACallstack: TIdeCallStack);
begin
  if (Supplier <> nil) and (ACallstack is TCurrentCallStack)
  then Supplier.RequestCurrent(TCurrentCallStack(ACallstack));
end;

procedure TIdeCallStackMonitor.RequestEntries(ACallstack: TIdeCallStack);
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

procedure TIDEExceptions.LoadFromXMLConfig(const AXMLConfig: TXMLConfig;
  const APath: string);
var
  NewCount: Integer;
  i: Integer;
  IDEException: TIDEException;
  IsLegacyList: Boolean;
  ItemPath: string;
begin
  Clear;
  IsLegacyList := AXMLConfig.IsLegacyList(APath);
  NewCount := AXMLConfig.GetListItemCount(APath, 'Item', IsLegacyList);
  FIgnoreAll := AXMLConfig.GetValue(APath + 'IgnoreAll', False);
  for i := 0 to NewCount-1 do
  begin
    IDEException := TIDEException(inherited Add(''));
    ItemPath := APath+AXMLConfig.GetListItemXPath('Item', i, IsLegacyList, True)+'/';
    IDEException.LoadFromXMLConfig(AXMLConfig, ItemPath);
  end;
end;

procedure TIDEExceptions.SaveToXMLConfig(const AXMLConfig: TXMLConfig;
  const APath: string; const ALegacyList: Boolean);
var
  Cnt: Integer;
  i: Integer;
  IDEException: TIDEException;
  ItemPath: string;
begin
  Cnt := Count;
  AXMLConfig.SetListItemCount(APath, Cnt, ALegacyList);
  AXMLConfig.SetDeleteValue(APath + 'IgnoreAll', IgnoreAll, False);
  for i := 0 to Cnt - 1 do
  begin
    IDEException := Items[i];
    ItemPath := APath+AXMLConfig.GetListItemXPath('Item', i, ALegacyList, True)+'/';
    IDEException.SaveToXMLConfig(AXMLConfig, ItemPath);
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

function TIDELineInfo.HasAddress(const AIndex: Integer; const ALine: Integer
  ): Boolean;
begin
  if Master = nil
  then Result := inherited HasAddress(AIndex, ALine)
  else Result := Master.HasAddress(AIndex, ALine);
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
  if (AnAddr = BaseAddr) and (ALinesBefore <= CountBefore) and (ALinesAfter <= CountAfter)
  then exit(True);

  if FMaster <> nil
  then Result := FMaster.PrepareRange(AnAddr, ALinesBefore, ALinesAfter)
  else Result := inherited PrepareRange(AnAddr, ALinesBefore, ALinesAfter);
end;

initialization
  DBG_VERBOSE       := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_BREAKPOINT    := DebugLogger.FindOrRegisterLogGroup('DBG_BREAKPOINT' {$IFDEF DBG_BREAKPOINT} , True {$ENDIF} );
  DBG_DATA_MONITORS := DebugLogger.FindOrRegisterLogGroup('DBG_DATA_MONITORS' {$IFDEF DBG_DATA_MONITORS} , True {$ENDIF} );
  DBG_LOCATION_INFO := DebugLogger.FindOrRegisterLogGroup('DBG_LOCATION_INFO' {$IFDEF DBG_LOCATION_INFO} , True {$ENDIF} );

end.
