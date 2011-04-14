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

interface

uses
  Classes, SysUtils, Laz_XMLCfg, math,
  LCLProc, IDEProcs, DebugUtils, maps;

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
    dcStepIntoInstr
    );
  TDBGCommands = set of TDBGCommand;

  TDBGState = (
    dsNone,
    dsIdle,
    dsStop,
    dsPause,
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
{ ---------------------------------------------------------<br>
  TDebuggerNotification is a reference counted baseclass
  for handling notifications for locals, watches, breakpoints etc.<br>
  ---------------------------------------------------------}
  TDebuggerNotification = class(TObject)
  private
    FRefCount: Integer;
  public
    procedure AddReference;
    constructor Create;
    destructor Destroy; override;
    procedure ReleaseReference;
  end;


  TIDEBreakPoints = class;
  TIDEBreakPointGroup = class;
  TIDEBreakPointGroups = class;
  TIDEWatches = class;
  TIDELocals = class;
  TIDELineInfo = class;
  TDebugger = class;

  TOnSaveFilenameToConfig = procedure(var Filename: string) of object;
  TOnLoadFilenameFromConfig = procedure(var Filename: string) of object;
  TOnGetGroupByName = function(const GroupName: string): TIDEBreakPointGroup of object;

(******************************************************************************)
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
    bpaDisableGroup
    );
  TIDEBreakPointActions = set of TIDEBreakPointAction;

  { TBaseBreakPoint }

  TBaseBreakPoint = class(TDelayedUdateItem)
  private
    FEnabled: Boolean;
    FExpression: String;
    FHitCount: Integer;
    FBreakHitCount: Integer;
    FLine: Integer;
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
    procedure SetValid(const AValue: TValidState);
  protected
    // virtual properties
    function GetBreakHitCount: Integer; virtual;
    function GetEnabled: Boolean; virtual;
    function GetExpression: String; virtual;
    function GetHitCount: Integer; virtual;
    function GetLine: Integer; virtual;
    function GetSource: String; virtual;
    function GetValid: TValidState; virtual;

    procedure SetBreakHitCount(const AValue: Integer); virtual;
    procedure SetEnabled(const AValue: Boolean); virtual;
    procedure SetExpression(const AValue: String); virtual;
    procedure SetInitialEnabled(const AValue: Boolean); virtual;
  public
    constructor Create(ACollection: TCollection); override;
    procedure SetLocation(const ASource: String; const ALine: Integer); virtual;// PublicProtectedFix ide/debugmanager.pas(867,32) Error: identifier idents no member "SetLocation"
    property BreakHitCount: Integer read GetBreakHitCount write SetBreakHitCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Expression: String read GetExpression write SetExpression;
    property HitCount: Integer read GetHitCount;
    property InitialEnabled: Boolean read FInitialEnabled write SetInitialEnabled;
    // TDBGBreakPoint: Line is the line-number as stored in the debug info
    // TIDEBreakPoint: Line is the location in the Source (potentially modified Source)
    property Line: Integer read GetLine;
    property Source: String read GetSource;
    property Valid: TValidState read GetValid;
  end;
  TBaseBreakPointClass = class of TBaseBreakPoint;

  TIDEBreakPoint = class(TBaseBreakPoint)
  private
    FAutoContinueTime: Cardinal;
    FActions: TIDEBreakPointActions;
    FDisableGroupList: TList;
    FEnableGroupList: TList;
    FGroup: TIDEBreakPointGroup;
    FLoading: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DisableGroups;
    procedure DoActionChange; virtual;
    procedure DoHit(const ACount: Integer; var AContinue: Boolean); override;
    procedure EnableGroups;
    procedure RemoveFromGroupList(const AGroup: TIDEBreakPointGroup;
                                  const AGroupList: TList);
    procedure ClearGroupList(const AGroupList: TList);
    procedure ClearAllGroupLists;
  protected
    // virtual properties
    function GetActions: TIDEBreakPointActions; virtual;
    function GetGroup: TIDEBreakPointGroup; virtual;
    function GetAutoContinueTime: Cardinal; virtual;
    procedure SetActions(const AValue: TIDEBreakPointActions); virtual;
    procedure SetGroup(const AValue: TIDEBreakPointGroup); virtual;
    procedure SetAutoContinueTime(const AValue: Cardinal); virtual;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure AddDisableGroup(const AGroup: TIDEBreakPointGroup);
    procedure AddEnableGroup(const AGroup: TIDEBreakPointGroup);
    procedure RemoveDisableGroup(const AGroup: TIDEBreakPointGroup);
    procedure RemoveEnableGroup(const AGroup: TIDEBreakPointGroup);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                      const OnLoadFilename: TOnLoadFilenameFromConfig;
                      const OnGetGroup: TOnGetGroupByName); virtual;
    procedure SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string;
                      const OnSaveFilename: TOnSaveFilenameToConfig); virtual;
  public
    property Actions: TIDEBreakPointActions read GetActions write SetActions;
    property AutoContinueTime: Cardinal read GetAutoContinueTime write SetAutoContinueTime;
    property Group: TIDEBreakPointGroup read GetGroup write SetGroup;
    property Loading: Boolean read FLoading;
  end;
  TIDEBreakPointClass = class of TIDEBreakPoint;

  { TDBGBreakPoint }

  TDBGBreakPoint = class(TBaseBreakPoint)
  private
    FSlave: TBaseBreakPoint;
    function GetDebugger: TDebugger;
    procedure SetSlave(const ASlave : TBaseBreakPoint);
  protected
    procedure DoChanged; override;
    procedure DoStateChange(const AOldState: TDBGState); virtual;
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

  TBaseBreakPoints = class(TCollection)
  private
  protected
  public
    constructor Create(const ABreakPointClass: TBaseBreakPointClass);
    function Add(const ASource: String; const ALine: Integer): TBaseBreakPoint;
    function Find(const ASource: String; const ALine: Integer): TBaseBreakPoint; overload;
    function Find(const ASource: String; const ALine: Integer; const AIgnore: TBaseBreakPoint): TBaseBreakPoint; overload;
    // no items property needed, it is "overridden" anyhow
  end;

  TIDEBreakPoints = class(TBaseBreakPoints)
  private
    FNotificationList: TList;
    function GetItem(const AnIndex: Integer): TIDEBreakPoint;
    procedure SetItem(const AnIndex: Integer; const AValue: TIDEBreakPoint);
  protected
    procedure NotifyAdd(const ABreakPoint: TIDEBreakPoint); virtual;    // called when a breakpoint is added
    procedure NotifyRemove(const ABreakpoint: TIDEBreakPoint); virtual; // called by breakpoint when destructed
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(const ABreakPointClass: TIDEBreakPointClass);
    destructor Destroy; override;
    function Add(const ASource: String; const ALine: Integer): TIDEBreakPoint;
    function Find(const ASource: String; const ALine: Integer): TIDEBreakPoint; overload;
    function Find(const ASource: String; const ALine: Integer; const AIgnore: TIDEBreakPoint): TIDEBreakPoint; overload;
    procedure AddNotification(const ANotification: TIDEBreakPointsNotification);
    procedure RemoveNotification(const ANotification: TIDEBreakPointsNotification);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                      const OnLoadFilename: TOnLoadFilenameFromConfig;
                      const OnGetGroup: TOnGetGroupByName); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                      const OnSaveFilename: TOnSaveFilenameToConfig); virtual;
  public
    property Items[const AnIndex: Integer]: TIDEBreakPoint read GetItem
                                                         write SetItem; default;
  end;

  TDBGBreakPoints = class(TBaseBreakPoints)
  private
    FDebugger: TDebugger;  // reference to our debugger
    function GetItem(const AnIndex: Integer): TDBGBreakPoint;
    procedure SetItem(const AnIndex: Integer; const AValue: TDBGBreakPoint);
  protected
    procedure DoStateChange(const AOldState: TDBGState); virtual;
    property  Debugger: TDebugger read FDebugger;
  public
    function Add(const ASource: String; const ALine: Integer): TDBGBreakPoint;
    constructor Create(const ADebugger: TDebugger;
                       const ABreakPointClass: TDBGBreakPointClass);
    function Find(const ASource: String; const ALine: Integer): TDBGBreakPoint; overload;
    function Find(const ASource: String; const ALine: Integer; const AIgnore: TDBGBreakPoint): TDBGBreakPoint; overload;

    property Items[const AnIndex: Integer]: TDBGBreakPoint read GetItem
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
    procedure AddReference(const ABreakPoint: TIDEBreakPoint);
    procedure RemoveReference(const ABreakPoint: TIDEBreakPoint);
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
    property InitialEnabled: Boolean read FInitialEnabled write SetInitialEnabled;
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



(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   D E B U G   I N F O R M A T I O N                                      **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

  type
  TDBGSymbolKind = (skClass, skRecord, skEnum, skSet, skProcedure, skFunction, skSimple, skPointer, skVariant);
  TDBGSymbolAttribute = (saRefParam,        // var, const, constref passed by reference
                         saInternalPointer // PointerToObject
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
  protected
    FName: String;
    FFlags: TDBGFieldFlags;
    FLocation: TDBGFieldLocation;
    FDBGType: TDBGType;
  public
    constructor Create(const AName: String; ADBGType: TDBGType; ALocation: TDBGFieldLocation; AFlags: TDBGFieldFlags = []);
    destructor Destroy; override;
    property Name: String read FName;
    property DBGType: TDBGType read FDBGType;
    property Location: TDBGFieldLocation read FLocation;
    property Flags: TDBGFieldFlags read FFlags;
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
    FDBGValue: TDBGValue;
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
    property TypeName: String read FTypeName;
    property Members: TStrings read FMembers;
    property Result: TDBGType read FResult;
  end;


(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   W A T C H E S                                                          **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

  TWatchDisplayFormat =
    (wdfDefault,
     wdfStructure,
     wdfChar, wdfString,
     wdfDecimal, wdfUnsigned, wdfFloat, wdfHex,
     wdfPointer,
     wdfMemDump
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

  { TBaseWatch }

  TBaseWatch = class(TDelayedUdateItem)
  private
    FEnabled: Boolean;
    FExpression: String;
    FDisplayFormat: TWatchDisplayFormat;
    FValid: TValidState;
    function GetEnabled: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoEnableChange; virtual;
    procedure DoExpressionChange; virtual;
    procedure DoDisplayFormatChanged; virtual;
    procedure SetValid(const AValue: TValidState);

  protected
    // virtual properties
    function GetExpression: String; virtual;
    function GetDisplayFormat: TWatchDisplayFormat; virtual;
    function GetValid: TValidState; virtual;
    function GetValue: String; virtual;
    function GetTypeInfo: TDBGType; virtual;

    procedure SetEnabled(const AValue: Boolean); virtual;
    procedure SetExpression(const AValue: String); virtual;
    procedure SetDisplayFormat(const AValue: TWatchDisplayFormat); virtual;
  public
    constructor Create(ACollection: TCollection); override;
  public
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Expression: String read GetExpression write SetExpression;
    property DisplayFormat: TWatchDisplayFormat read GetDisplayFormat write SetDisplayFormat;
    property Valid: TValidState read GetValid;
    property Value: String read GetValue;
    property TypeInfo: TDBGType read GetTypeInfo;
  end;
  TBaseWatchClass = class of TBaseWatch;

  TIDEWatch = class(TBaseWatch)
  private
  protected
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure LoadFromXMLConfig(const AConfig: TXMLConfig;
                                const APath: string); virtual;
    procedure SaveToXMLConfig(const AConfig: TXMLConfig;
                              const APath: string); virtual;
  end;
  TIDEWatchClass = class of TIDEWatch;

  { TDBGWatch }

  TDBGWatch = class(TBaseWatch)
  private
    FSlave: TBaseWatch;
    function GetDebugger: TDebugger;
    procedure SetSlave(const ASlave : TBaseWatch);
  protected
    procedure DoChanged; override;
    procedure DoChange; virtual;
    procedure DoStateChange(const AOldState: TDBGState); virtual;
    property Debugger: TDebugger read GetDebugger;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property Slave: TBaseWatch read FSlave write SetSlave;
  end;
  TDBGWatchClass = class of TDBGWatch;


  { TBaseWatches }

  TIDEWatchesEvent =
       procedure(const ASender: TIDEWatches; const AWatch: TIDEWatch) of object;

  TIDEWatchesNotification = class(TDebuggerNotification)
  private
    FOnAdd:    TIDEWatchesEvent;
    FOnUpdate: TIDEWatchesEvent;//Item will be nil in case all items need to be updated
    FOnRemove: TIDEWatchesEvent;
  public
    property OnAdd:    TIDEWatchesEvent read FOnAdd    write FOnAdd;
    property OnUpdate: TIDEWatchesEvent read FOnUpdate write FOnUpdate;
    property OnRemove: TIDEWatchesEvent read FOnRemove write FonRemove;
  end;

  TBaseWatches = class(TCollection)
  private
  protected
  public
    constructor Create(const AWatchClass: TBaseWatchClass);
    function Add(const AExpression: String): TBaseWatch;
    function Find(const AExpression: String): TBaseWatch;
    // no items property needed, it is "overridden" anyhow
  end;

  TIDEWatches = class(TBaseWatches)
  private
    FNotificationList: TList;
    function GetItem(const AnIndex: Integer): TIDEWatch;
    procedure SetItem(const AnIndex: Integer; const AValue: TIDEWatch);
  protected
    procedure NotifyAdd(const AWatch: TIDEWatch); virtual;    // called when a watch is added
    procedure NotifyRemove(const AWatch: TIDEWatch); virtual; // called by watch when destructed
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(const AWatchClass: TIDEWatchClass);
    destructor Destroy; override;
    // Watch
    function Add(const AExpression: String): TIDEWatch;
    function Find(const AExpression: String): TIDEWatch;
    // IDE
    procedure AddNotification(const ANotification: TIDEWatchesNotification);
    procedure RemoveNotification(const ANotification: TIDEWatchesNotification);
    procedure LoadFromXMLConfig(const AConfig: TXMLConfig; const APath: string); virtual;
    procedure SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string); virtual;
  public
    property Items[const AnIndex: Integer]: TIDEWatch read GetItem
                                                      write SetItem; default;
  end;

  { TDBGWatches }

  TDBGWatches = class(TBaseWatches)
  private
    FDebugger: TDebugger;  // reference to our debugger
    FOnChange: TNotifyEvent;
    function GetItem(const AnIndex: Integer): TDBGWatch;
    procedure SetItem(const AnIndex: Integer; const AValue: TDBGWatch);
  protected
    procedure DoStateChange(const AOldState: TDBGState); virtual;
    procedure Update(Item: TCollectionItem); override;
    property  Debugger: TDebugger read FDebugger;
  public
    constructor Create(const ADebugger: TDebugger;
                       const AWatchClass: TDBGWatchClass);
    // Watch
    function Add(const AExpression: String): TDBGWatch;
    function Find(const AExpression: String): TDBGWatch;
  public
    property Items[const AnIndex: Integer]: TDBGWatch read GetItem
                                                      write SetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   L O C A L S                                                            **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

  { TBaseLocals }

  TBaseLocals = class(TObject)
  private
  protected
    function GetName(const AnIndex: Integer): String; virtual;
    function GetValue(const AnIndex: Integer): String; virtual;
  public
    constructor Create;
    function Count: Integer; virtual;
  public
    property Names[const AnIndex: Integer]: String read GetName;
    property Values[const AnIndex: Integer]: String read GetValue;
  end;

  { TIDELocals }

  TIDELocalsNotification = class(TDebuggerNotification)
  private
    FOnChange: TNotifyEvent;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TIDELocals = class(TBaseLocals)
  private
    FNotificationList: TList;
  protected
    procedure NotifyChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotification(const ANotification: TIDELocalsNotification);
    procedure RemoveNotification(const ANotification: TIDELocalsNotification);
  end;

  { TDBGLocals }

  TDBGLocals = class(TBaseLocals)
  private
    FDebugger: TDebugger;  // reference to our debugger
    FOnChange: TNotifyEvent;
  protected
    procedure Changed; virtual;
    procedure DoChange;
    procedure DoStateChange(const AOldState: TDBGState); virtual;
    function GetCount: Integer; virtual;
    property Debugger: TDebugger read FDebugger;
  public
    function Count: Integer; override;
    constructor Create(const ADebugger: TDebugger);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   L I N E   I N F O                                                      **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

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

  TIDELineInfo = class(TBaseLineInfo)
  private
    FNotificationList: TList;
  protected
    procedure NotifyChange(ASource: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotification(const ANotification: TIDELineInfoNotification);
    procedure RemoveNotification(const ANotification: TIDELineInfoNotification);
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


(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   R E G I S T E R S                                                      **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

  { TBaseRegisters }

  TBaseRegisters = class(TObject)
  private
  protected
    function GetModified(const AnIndex: Integer): Boolean; virtual;
    function GetName(const AnIndex: Integer): String; virtual;
    function GetValue(const AnIndex: Integer): String; virtual;
  public
    constructor Create;
    function Count: Integer; virtual;
  public
    property Modified[const AnIndex: Integer]: Boolean read GetModified;
    property Names[const AnIndex: Integer]: String read GetName;
    property Values[const AnIndex: Integer]: String read GetValue;
  end;

  { TIDERegisters }

  TIDERegistersNotification = class(TDebuggerNotification)
  private
    FOnChange: TNotifyEvent;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TIDERegisters = class(TBaseRegisters)
  private
    FNotificationList: TList;
  protected
    procedure NotifyChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotification(const ANotification: TIDERegistersNotification);
    procedure RemoveNotification(const ANotification: TIDERegistersNotification);
  end;

  { TDBGRegisters }

  TDBGRegisters = class(TBaseRegisters)
  private
    FDebugger: TDebugger;  // reference to our debugger
    FOnChange: TNotifyEvent;
  protected
    procedure Changed; virtual;
    procedure DoChange;
    procedure DoStateChange(const AOldState: TDBGState); virtual;
    function GetCount: Integer; virtual;
    property Debugger: TDebugger read FDebugger;
  public
    function Count: Integer; override;
    constructor Create(const ADebugger: TDebugger);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   C A L L S T A C K                                                      **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)
(* The entries for the callstack are created on demand. This way when the     *)
(* first entry is needed, it isn't required to create the whole stack         *)
(*                                                                            *)
(* TCallStackEntry needs to stay a readonly object so its data can be shared  *)
(******************************************************************************)

  TBaseCallStack = class;

  { TCallStackEntry }

  TCallStackEntryState = (cseValid, cseRequested, cseInvalid);

  TCallStackEntry = class(TObject)
  private
    FOwner: TBaseCallStack;
    FIndex: Integer;
    FAdress: TDbgPtr;
    FFunctionName: String;
    FLine: Integer;
    FArguments: TStrings;
    FSource: String;
    FFullFileName: String;
    FState: TCallStackEntryState;
    function GetArgumentCount: Integer;
    function GetArgumentName(const AnIndex: Integer): String;
    function GetArgumentValue(const AnIndex: Integer): String;
    function GetCurrent: Boolean;
    function GetFullFileName: String;
    function GetFunctionName: String;
    function GetSource: String;
    procedure SetCurrent(const AValue: Boolean);
  public
    constructor Create(const AIndex:Integer; const AnAdress: TDbgPtr;
                       const AnArguments: TStrings; const AFunctionName: String;
                       const ASource: String; const AFullFileName: String;
                       const ALine: Integer; AState: TCallStackEntryState = cseValid);
    constructor CreateCopy(const ASource: TCallStackEntry);
    destructor Destroy; override;
    property Address: TDbgPtr read FAdress;
    property ArgumentCount: Integer read GetArgumentCount;
    property ArgumentNames[const AnIndex: Integer]: String read GetArgumentName;
    property ArgumentValues[const AnIndex: Integer]: String read GetArgumentValue;
    property Current: Boolean read GetCurrent write SetCurrent;
    property FunctionName: String read GetFunctionName;
    property Index: Integer read FIndex;
    property Line: Integer read FLine;
    property Source: String read GetSource;
    property FullFileName: String read GetFullFileName;
    property State: TCallStackEntryState read FState write FState;
  end;

  { TBaseCallStack }

  TBaseCallStack = class(TObject)
  private
    FCount: Integer;
    function IndexError(AIndex: Integer): TCallStackEntry;
    function GetEntry(AIndex: Integer): TCallStackEntry;
  protected
    function CheckCount: Boolean; virtual;
    procedure Clear; virtual;
    function GetCurrent: TCallStackEntry; virtual;
    function InternalGetEntry(AIndex: Integer): TCallStackEntry; virtual;
    procedure SetCurrent(AValue: TCallStackEntry); virtual;
    procedure SetCount(ACount: Integer); virtual;
  public
    function Count: Integer;
    destructor Destroy; override;
    procedure PrepareRange(AIndex, ACount: Integer); virtual;
    property Current: TCallStackEntry read GetCurrent write SetCurrent;
    property Entries[AIndex: Integer]: TCallStackEntry read GetEntry;
  end;

  { TIDECallStackNotification }

  TIDECallStackNotification = class(TDebuggerNotification)
  private
    FOnChange: TNotifyEvent;
    FOnCurrent: TNotifyEvent;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnCurrent: TNotifyEvent read FOnCurrent write FOnCurrent;
  end;

  { TIDECallStack }

  TIDECallStack = class(TBaseCallStack)
  private
    FNotificationList: TList;
  protected
    procedure NotifyChange;
    procedure NotifyCurrent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotification(const ANotification: TIDECallStackNotification);
    procedure RemoveNotification(const ANotification: TIDECallStackNotification);
  end;

  { TDBGCallStack }

  TDBGCallStack = class(TBaseCallStack)
  private
    FDebugger: TDebugger;  // reference to our debugger
    FEntries: TMap;        // list of created entries
    FOldState: TDBGState;
    FOnChange: TNotifyEvent;
    FOnClear: TNotifyEvent;
    FOnCurrent: TNotifyEvent;
  protected
    function  CreateStackEntry(AIndex: Integer): TCallStackEntry; virtual;
    procedure CurrentChanged;
    procedure Changed;
    function  CheckCount: Boolean; override;
    procedure Clear; override;
    procedure DoStateChange(const AOldState: TDBGState); virtual;
    function  InternalGetEntry(AIndex: Integer): TCallStackEntry; override;
    procedure InternalSetEntry(AIndex: Integer; AEntry: TCallStackEntry);
    procedure PrepareEntries(AStartIndex, AEndIndex: Integer); virtual;
    property Debugger: TDebugger read FDebugger;
  public
    constructor Create(const ADebugger: TDebugger);
    destructor Destroy; override;
  public
    procedure PrepareRange(AIndex, ACount: Integer); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
    property OnCurrent: TNotifyEvent read FOnCurrent write FOnCurrent;
  end;

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
    SrcFileName: String;             // SrcFile if avai;
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

  TIDEDisassembler = class(TBaseDisassembler)
  private
    FNotificationList: TList;
  protected
    procedure DoChanged; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotification(const ANotification: TIDEDisassemblerNotification);
    procedure RemoveNotification(const ANotification: TIDEDisassemblerNotification);
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
    property Count: Integer read FCount;
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

(******************************************************************************)
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

  TIDESignal = class(TBaseSignal)
  private
  protected
  public
    procedure LoadFromXMLConfig(const AXMLConfig: TXMLConfig;
                                const APath: string);
    procedure SaveToXMLConfig(const AXMLConfig: TXMLConfig;
                              const APath: string);
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
    function GetItem(const AIndex: Integer): TIDESignal;
    procedure SetItem(const AIndex: Integer; const AValue: TIDESignal);
  protected
  public
    function Add(const AName: String; AID: Integer): TIDESignal;
    function Find(const AName: String): TIDESignal;
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
    procedure SetEnabled(const AValue: Boolean);
  protected
  public
    constructor Create(ACollection: TCollection); override;
    procedure LoadFromXMLConfig(const AXMLConfig: TXMLConfig;
                                const APath: string); override;
    procedure SaveToXMLConfig(const AXMLConfig: TXMLConfig;
                              const APath: string); override;
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
    function GetItem(const AIndex: Integer): TIDEException;
    procedure SetItem(const AIndex: Integer; const AValue: TIDEException);
  protected
  public
    function Add(const AName: String): TIDEException;
    function Find(const AName: String): TIDEException;
  public
    procedure LoadFromXMLConfig(const AXMLConfig: TXMLConfig;
                                const APath: string);
    procedure SaveToXMLConfig(const AXMLConfig: TXMLConfig;
                              const APath: string);
    property Items[const AIndex: Integer]: TIDEException read GetItem
                                                        write SetItem; default;
  end;

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
    ecProcess,
    ecThread,     // Thread creation, destruction, start, etc.
    ecModule,     // Library load and unload
    ecOutput,     // DebugOutput calls
    ecWindow,
    ecDebugger);  // debugger errors and warnings
  TDBGEventCategories = set of TDBGEventCategory;

  TDBGEventNotify = procedure(Sender: TObject; const ACategory: TDBGEventCategory;
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
                                 const AExceptionText: String;
                                 out AContinue: Boolean) of object;

  TDebuggerNotifyReason = (dnrDestroy);

  TDebuggerProperties = class(TPersistent)
  private
  public
  published
  end;

  TDebugger = class(TObject)
  private
    FArguments: String;
    FBreakPoints: TDBGBreakPoints;
    FDebuggerEnvironment: TStrings;
    FCurEnvironment: TStrings;
    FDisassembler: TDBGDisassembler;
    FEnvironment: TStrings;
    FExceptions: TDBGExceptions;
    FExitCode: Integer;
    FExternalDebugger: String;
    //FExceptions: TDBGExceptions;
    FFileName: String;
    FLocals: TDBGLocals;
    FLineInfo: TDBGLineInfo;
    FRegisters: TDBGRegisters;
    FShowConsole: Boolean;
    FSignals: TDBGSignals;
    FState: TDBGState;
    FCallStack: TDBGCallStack;
    FWatches: TDBGWatches;
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
    function  GetState: TDBGState;
    function  ReqCmd(const ACommand: TDBGCommand;
                     const AParams: array of const): Boolean;
    procedure SetDebuggerEnvironment (const AValue: TStrings );
    procedure SetEnvironment(const AValue: TStrings);
    procedure SetFileName(const AValue: String);
  protected
    function  CreateBreakPoints: TDBGBreakPoints; virtual;
    function  CreateLocals: TDBGLocals; virtual;
    function  CreateLineInfo: TDBGLineInfo; virtual;
    function  CreateRegisters: TDBGRegisters; virtual;
    function  CreateCallStack: TDBGCallStack; virtual;
    function  CreateDisassembler: TDBGDisassembler; virtual;
    function  CreateWatches: TDBGWatches; virtual;
    function  CreateSignals: TDBGSignals; virtual;
    function  CreateExceptions: TDBGExceptions; virtual;
    procedure DoCurrent(const ALocation: TDBGLocationRec);
    procedure DoDbgOutput(const AText: String);
    procedure DoDbgEvent(const ACategory: TDBGEventCategory; const AText: String);
    procedure DoException(const AExceptionType: TDBGExceptionType; const AExceptionClass: String; const AExceptionText: String; out AContinue: Boolean);
    procedure DoOutput(const AText: String);
    procedure DoBreakpointHit(const ABreakPoint: TBaseBreakPoint; var ACanContinue: Boolean);
    procedure DoState(const OldState: TDBGState); virtual;
    function  ChangeFileName: Boolean; virtual;
    function  GetCommands: TDBGCommands;
    function  GetSupportedCommands: TDBGCommands; virtual;
    function  GetTargetWidth: Byte; virtual;
    function  GetWaiting: Boolean; virtual;
    function  RequestCommand(const ACommand: TDBGCommand;
                             const AParams: array of const): Boolean;
                             virtual; abstract; // True if succesful
    procedure SetExitCode(const AValue: Integer);
    procedure SetState(const AValue: TDBGState);
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
    function  Evaluate(const AExpression: String; var AResult: String;
                          var ATypeInfo: TDBGType): Boolean;                     // Evaluates the given expression, returns true if valid
    function  Modify(const AExpression, AValue: String): Boolean;                // Modifies the given expression, returns true if valid
    function  Disassemble(AAddr: TDbgPtr; ABackward: Boolean; out ANextAddr: TDbgPtr;
                          out ADump, AStatement, AFile: String; out ALine: Integer): Boolean; deprecated;
    procedure LockCommandProcessing; virtual;
    procedure UnLockCommandProcessing; virtual;
    procedure AddNotifyEvent(AReason: TDebuggerNotifyReason; AnEvent: TNotifyEvent);
    procedure RemoveNotifyEvent(AReason: TDebuggerNotifyReason; AnEvent: TNotifyEvent);
  public
    property Arguments: String read FArguments write FArguments;                 // Arguments feed to the program
    property BreakPoints: TDBGBreakPoints read FBreakPoints;                     // list of all breakpoints
    property CallStack: TDBGCallStack read FCallStack;
    property Disassembler: TDBGDisassembler read FDisassembler;
    property Commands: TDBGCommands read GetCommands;                            // All current available commands of the debugger
    property DebuggerEnvironment: TStrings read FDebuggerEnvironment
                                           write SetDebuggerEnvironment;         // The environment passed to the debugger process
    property Environment: TStrings read FEnvironment write SetEnvironment;       // The environment passed to the debuggee
    property Exceptions: TDBGExceptions read FExceptions;                        // A list of exceptions we should ignore
    property ExitCode: Integer read FExitCode;
    property ExternalDebugger: String read FExternalDebugger;                    // The name of the debugger executable
    property FileName: String read FFileName write SetFileName;                  // The name of the exe to be debugged
    property Locals: TDBGLocals read FLocals;                                    // list of all localvars etc
    property LineInfo: TDBGLineInfo read FLineInfo;                              // list of all source LineInfo
    property Registers: TDBGRegisters read FRegisters;                           // list of all registers
    property Signals: TDBGSignals read FSignals;                                 // A list of actions for signals we know
    property ShowConsole: Boolean read FShowConsole write FShowConsole;          // Indicates if the debugger should create a console for the debuggee
    property State: TDBGState read FState;                                       // The current state of the debugger
    property SupportedCommands: TDBGCommands read GetSupportedCommands;          // All available commands of the debugger
    property TargetWidth: Byte read GetTargetWidth;                              // Currently only 32 or 64
    property Waiting: Boolean read GetWaiting;                                   // Set when the debugger is wating for a command to complete
    property Watches: TDBGWatches read FWatches;                                 // list of all watches etc
    property WorkingDir: String read FWorkingDir write FWorkingDir;              // The working dir of the exe being debugged
    // Events
    property OnCurrent: TDBGCurrentLineEvent read FOnCurrent write FOnCurrent;   // Passes info about the current line being debugged
    property OnDbgOutput: TDBGOutputEvent read FOnDbgOutput write FOnDbgOutput;  // Passes all debuggeroutput
    property OnDbgEvent: TDBGEventNotify read FOnDbgEvent write FOnDbgEvent;     // Passes recognized debugger events, like library load or unload
    property OnException: TDBGExceptionEvent read FOnException write FOnException;  // Fires when the debugger received an exeption
    property OnOutput: TDBGOutputEvent read FOnOutput write FOnOutput;           // Passes all output of the debugged target
    property OnState: TDebuggerStateChangedEvent read FOnState write FOnState;   // Fires when the current state of the debugger changes
    property OnBreakPointHit: TDebuggerBreakPointHitEvent read FOnBreakPointHit write FOnBreakPointHit;   // Fires when the program is paused at a breakpoint
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
    'StepIntoInstr'
    );

  DBGStateNames: array[TDBGState] of string = (
    'None',
    'Idle',
    'Stop',
    'Pause',
    'Init',
    'Run',
    'Error',
    'Destroying'
    );

  DBGBreakPointActionNames: array[TIDEBreakPointAction] of string = (
    'Stop',
    'EnableGroup',
    'DisableGroup'
    );

function DBGCommandNameToCommand(const s: string): TDBGCommand;
function DBGStateNameToState(const s: string): TDBGState;
function DBGBreakPointActionNameToAction(const s: string): TIDEBreakPointAction;

function dbgs(AState: TDBGState): String; overload;
function dbgs(ADisassRange: TDBGDisassemblerEntryRange): String; overload;

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
             dcStepOut, dcRunTo, dcJumpto, dcBreak, dcWatch, dcEvaluate, dcEnvironment],
  {dsPause} [dcRun, dcStop, dcStepOver, dcStepInto, dcStepOverInstr, dcStepIntoInstr,
             dcStepOut, dcRunTo, dcJumpto, dcBreak, dcWatch, dcLocal, dcEvaluate, dcModify,
             dcEnvironment, dcSetStackFrame, dcDisassemble],
  {dsInit } [],
  {dsRun  } [dcPause, dcStop, dcBreak, dcWatch, dcEnvironment],
  {dsError} [dcStop],
  {dsDestroying} []
  );

var
  MDebuggerPropertiesList: TStringlist;

function dbgs(AState: TDBGState): String; overload;
begin
  Result := DBGStateNames[AState];
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
      Result := Format('Range(%u)=[[ Cnt=%d, Capac=%d, First=%u, RFirst=%u, Last=%u, RLast=%u, REnd=%u, FirstOfs=%d ]]',
        [PtrUInt(ADisassRange), Count, Capacity, FirstAddr, RangeStartAddr, LastAddr, RangeEndAddr, LastEntryEndAddr, fo]);
  end;
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

{ =========================================================================== }
{ TDebuggerNotification }
{ =========================================================================== }

procedure TDebuggerNotification.AddReference;
begin
  Inc(FRefcount);
end;

constructor TDebuggerNotification.Create;
begin
  FRefCount := 0;
  inherited;
end;

destructor TDebuggerNotification.Destroy;
begin
  Assert(FRefcount = 0, 'Destroying referenced object');
  inherited;
end;

procedure TDebuggerNotification.ReleaseReference;
begin
  Dec(FRefCount);
  if FRefCount = 0 then Free;
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

  FBreakPoints := CreateBreakPoints;
  FLocals := CreateLocals;
  FLineInfo := CreateLineInfo;
  FRegisters := CreateRegisters;
  FCallStack := CreateCallStack;
  FDisassembler := CreateDisassembler;
  FWatches := CreateWatches;
  FExceptions := CreateExceptions;
  FSignals := CreateSignals;
  FExitCode := 0;
end;

function TDebugger.CreateBreakPoints: TDBGBreakPoints;
begin
  Result := TDBGBreakPoints.Create(Self, TDBGBreakPoint);
end;

function TDebugger.CreateCallStack: TDBGCallStack;
begin
  Result := TDBGCallStack.Create(Self);
end;

function TDebugger.CreateDisassembler: TDBGDisassembler;
begin
  Result := TDBGDisassembler.Create(Self);
end;

function TDebugger.CreateExceptions: TDBGExceptions;
begin
  Result := TDBGExceptions.Create(Self, TDBGException);
end;

function TDebugger.CreateLocals: TDBGLocals;
begin
  Result := TDBGLocals.Create(Self);
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

function TDebugger.CreateWatches: TDBGWatches;
begin
  Result := TDBGWatches.Create(Self, TDBGWatch);
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
  FWatches.FDebugger := nil;

  FreeAndNil(FExceptions);
  FreeAndNil(FBreakPoints);
  FreeAndNil(FLocals);
  FreeAndNil(FLineInfo);
  FreeAndNil(FRegisters);
  FreeAndNil(FCallStack);
  FreeAndNil(FDisassembler);
  FreeAndNil(FWatches);
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
  if Assigned(FOnCurrent) then FOnCurrent(Self, ALocation);
end;

procedure TDebugger.DoDbgOutput(const AText: String);
begin
  // WriteLN(' [TDebugger] ', AText);
  if Assigned(FOnDbgOutput) then FOnDbgOutput(Self, AText);
end;

procedure TDebugger.DoDbgEvent(const ACategory: TDBGEventCategory; const AText: String);
begin
  if Assigned(FOnDbgEvent) then FOnDbgEvent(Self, ACategory, AText);
end;

procedure TDebugger.DoException(const AExceptionType: TDBGExceptionType; const AExceptionClass: String;
  const AExceptionText: String; out AContinue: Boolean);
begin
  if Assigned(FOnException) then
    FOnException(Self, AExceptionType, AExceptionClass, AExceptionText, AContinue)
  else
    AContinue := True;
end;

procedure TDebugger.DoOutput(const AText: String);
begin
  if Assigned(FOnOutput) then FOnOutput(Self, AText);
end;

procedure TDebugger.DoBreakpointHit(const ABreakPoint: TBaseBreakPoint; var ACanContinue: Boolean);
begin
  if Assigned(FOnBreakpointHit)
  then FOnBreakpointHit(Self, ABreakPoint, ACanContinue);
end;

procedure TDebugger.DoState(const OldState: TDBGState);
begin
  if Assigned(FOnState) then FOnState(Self, OldState);
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

function TDebugger.Evaluate(const AExpression: String; var AResult: String;
  var ATypeInfo: TDBGType): Boolean;
begin
  FreeAndNIL(ATypeInfo);
  Result := ReqCmd(dcEvaluate, [AExpression, @AResult, @ATypeInfo]);
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
  SetState(dsIdle);
end;

procedure TDebugger.JumpTo(const ASource: String; const ALine: Integer);
begin
  ReqCmd(dcJumpTo, [ASource, ALine]);
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
    OldState := FState;
    FState := AValue;
    FBreakpoints.DoStateChange(OldState);
    FLocals.DoStateChange(OldState);
    FLineInfo.DoStateChange(OldState);
    FRegisters.DoStateChange(OldState);
    FCallStack.DoStateChange(OldState);
    FDisassembler.DoStateChange(OldState);
    FWatches.DoStateChange(OldState);
    DoState(OldState);
  end;
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
  FSource := '';
  FLine := -1;
  FValid := vsUnknown;
  FEnabled := False;
  FHitCount := 0;
  FBreakHitCount := 0;
  FExpression := '';
  FInitialEnabled := False;
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

procedure TIDEBreakPoint.AddDisableGroup(const AGroup: TIDEBreakPointGroup);
begin
  if AGroup = nil then Exit;
  FDisableGroupList.Add(AGroup);
  AGroup.AddReference(Self);
  Changed;
end;

procedure TIDEBreakPoint.AddEnableGroup(const AGroup: TIDEBreakPointGroup);
begin
  if AGroup = nil then Exit;
  FEnableGroupList.Add(AGroup);
  AGroup.AddReference(Self);
  Changed;
end;

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

procedure TIDEBreakPoint.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TIDEBreakPoint
  then begin
    TIDEBreakPoint(Dest).Actions := FActions;
    TIDEBreakPoint(Dest).AutoContinueTime := FAutoContinueTime;
  end;
end;

procedure TIDEBreakPoint.ClearAllGroupLists;
begin
  ClearGroupList(FDisableGroupList);
  ClearGroupList(FEnableGroupList);
end;

procedure TIDEBreakPoint.ClearGroupList(const AGroupList: TList);
var
  i: Integer;
  AGroup: TIDEBreakPointGroup;
begin
  for i:=0 to AGroupList.Count-1 do begin
    AGroup:=TIDEBreakPointGroup(AGroupList[i]);
    AGroup.RemoveReference(Self);
  end;
  AGroupList.Clear;
end;

constructor TIDEBreakPoint.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FGroup := nil;
  FActions := [bpaStop];
  FDisableGroupList := TList.Create;
  FEnableGroupList := TList.Create;
end;

destructor TIDEBreakPoint.Destroy;
begin
  if (TIDEBreakPoints(Collection) <> nil)
  then TIDEBreakPoints(Collection).NotifyRemove(Self);

  if FGroup <> nil
  then FGroup.Remove(Self);

  ClearAllGroupLists;

  inherited;
  FreeAndNil(FDisableGroupList);
  FreeAndNil(FEnableGroupList);
end;

procedure TIDEBreakPoint.DisableGroups;
var
  n: Integer;
begin
  for n := 0 to FDisableGroupList.Count - 1 do
    TIDEBreakPointGroup(FDisableGroupList[n]).Enabled := False;
end;

procedure TIDEBreakPoint.DoActionChange;
begin
  Changed;
end;

procedure TIDEBreakPoint.DoHit (const ACount: Integer; var AContinue: Boolean );
begin
  inherited DoHit(ACount, AContinue);
  if bpaEnableGroup in Actions
  then EnableGroups;
  if bpaDisableGroup in Actions
  then DisableGroups;
end;

procedure TIDEBreakPoint.EnableGroups;
var
  n: Integer;
begin
  for n := 0 to FDisableGroupList.Count - 1 do
    TIDEBreakPointGroup(FDisableGroupList[n]).Enabled := True;
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

  procedure LoadGroupList(GroupList: TList; const ListPath: string);
  var
    i: Integer;
    CurGroup: TIDEBreakPointGroup;
    NewCount: Integer;
    GroupName: String;
  begin
    ClearGroupList(GroupList);
    NewCount:=XMLConfig.GetValue(ListPath+'Count',0);
    for i:=0 to NewCount-1 do begin
      GroupName:=XMLConfig.GetValue(ListPath+'Group'+IntToStr(i+1)+'/Name','');
      if GroupName='' then continue;
      CurGroup:=OnGetGroup(GroupName);
      if CurGroup=nil then continue;
      if GroupList=FDisableGroupList then
        AddDisableGroup(CurGroup)
      else if GroupList=FEnableGroupList then
        AddEnableGroup(CurGroup);
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
    GroupName:=XMLConfig.GetValue(Path+'Group/Name','');
    Group:=OnGetGroup(GroupName);
    Expression:=XMLConfig.GetValue(Path+'Expression/Value','');
    AutoContinueTime:=XMLConfig.GetValue(Path+'AutoContinueTime/Value',0);
    BreakHitCount := XMLConfig.GetValue(Path+'BreakHitCount/Value',0);
    Filename:=XMLConfig.GetValue(Path+'Source/Value','');
    if Assigned(OnLoadFilename) then OnLoadFilename(Filename);
    FSource:=Filename;
    InitialEnabled:=XMLConfig.GetValue(Path+'InitialEnabled/Value',true);
    Enabled:=FInitialEnabled;
    FLine:=XMLConfig.GetValue(Path+'Line/Value',-1);
    NewActions:=[];
    for CurAction:=Low(TIDEBreakPointAction) to High(TIDEBreakPointAction) do
      if XMLConfig.GetValue(
          Path+'Actions/'+DBGBreakPointActionNames[CurAction],
          CurAction in [bpaStop])
      then
        Include(NewActions,CurAction);
    Actions:=NewActions;
    LoadGroupList(FDisableGroupList,Path+'DisableGroups/');
    LoadGroupList(FEnableGroupList,Path+'EnableGroups/');
  finally
    FLoading:=false;
  end;
end;

procedure TIDEBreakPoint.RemoveDisableGroup(const AGroup: TIDEBreakPointGroup);
begin
  RemoveFromGroupList(AGroup,FDisableGroupList);
end;

procedure TIDEBreakPoint.RemoveEnableGroup(const AGroup: TIDEBreakPointGroup);
begin
  RemoveFromGroupList(AGroup,FEnableGroupList);
end;

procedure TIDEBreakPoint.RemoveFromGroupList(const AGroup: TIDEBreakPointGroup;
  const AGroupList: TList);
begin
  if (AGroup = nil) then Exit;
  AGroupList.Remove(AGroup);
  AGroup.RemoveReference(Self);
end;

procedure TIDEBreakPoint.SaveToXMLConfig(const AConfig: TXMLConfig;
  const APath: string; const OnSaveFilename: TOnSaveFilenameToConfig);

  procedure SaveGroupList(const AList: TList; const AListPath: string);
  var
    i: Integer;
    CurGroup: TIDEBreakPointGroup;
  begin
    AConfig.SetDeleteValue(AListPath + 'Count', AList.Count,0);
    for i := 0 to AList.Count - 1 do
    begin
      CurGroup := TIDEBreakPointGroup(AList[i]);
      AConfig.SetDeleteValue(Format('$%sGroup%d/Name', [AListPath, i+1]),
        CurGroup.Name, '');
    end;
  end;

var
  Filename: String;
  CurAction: TIDEBreakPointAction;
begin
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

  for CurAction := Low(TIDEBreakPointAction) to High(TIDEBreakPointAction) do
  begin
    AConfig.SetDeleteValue(
        APath+'Actions/'+DBGBreakPointActionNames[CurAction],
        CurAction in Actions, CurAction in [bpaStop]);
  end;
  SaveGroupList(FDisableGroupList, APath + 'DisableGroups/');
  SaveGroupList(FEnableGroupList, APath + 'EnableGroups/');
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
procedure TIDEBreakPoint.CopyGroupList(SrcGroupList, DestGroupList: TList;
  DestGroups: TIDEBreakPointGroups);
var
  i: Integer;
  CurGroup: TIDEBreakPointGroup;
  NewGroup: TIDEBreakPointGroup;
begin
  ClearGroupList(DestGroupList);
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

function TDBGBreakPoint.GetDebugger: TDebugger;
begin
  Result := TDBGBreakPoints(Collection).FDebugger;
end;

procedure TDBGBreakPoint.SetSlave(const ASlave : TBaseBreakPoint);
begin
  Assert(FSlave = nil, 'TDBGBreakPoint.SetSlave already has a slave');
  FSlave := ASlave;
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

procedure TIDEBreakPoints.AddNotification(
  const ANotification: TIDEBreakPointsNotification);
begin
  FNotificationList.Add(ANotification);
  ANotification.AddReference;
end;

constructor TIDEBreakPoints.Create(const ABreakPointClass: TIDEBreakPointClass);
begin
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

function TIDEBreakPoints.GetItem(const AnIndex: Integer): TIDEBreakPoint;
begin
  Result := TIDEBreakPoint(inherited GetItem(AnIndex));
end;

procedure TIDEBreakPoints.NotifyAdd(const ABreakPoint: TIDEBreakPoint);
var
  n: Integer;
  Notification: TIDEBreakPointsNotification;
begin
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TIDEBreakPointsNotification(FNotificationList[n]);
    if Assigned(Notification.FOnAdd)
    then Notification.FOnAdd(Self, ABreakPoint);
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

    BreakPoint := Find(LoadBreakPoint.Source, LoadBreakPoint.Line, LoadBreakPoint);

    if BreakPoint = nil
    then BreakPoint := Add(LoadBreakPoint.Source, LoadBreakPoint.Line);
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
  Result.SetLocation(ASource, ALine);
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
    if  (Result.Line = ALine)
    and (AIgnore <> Result)
    and (CompareFilenames(Result.Source, ASource) = 0)
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

procedure TIDEBreakPointGroup.AddReference(const ABreakPoint: TIDEBreakPoint);
begin
  FReferences.Add(ABreakPoint);
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
    TIDEBreakPoint(FReferences[n]).RemoveDisableGroup(Self);
  for n := FReferences.Count - 1 downto 0 do
    TIDEBreakPoint(FReferences[n]).RemoveEnableGroup(Self);

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
  InitialEnabled:=XMLConfig.GetValue(Path+'InitialEnabled/Value',true);
  FEnabled:=InitialEnabled;
end;

procedure TIDEBreakPointGroup.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'Name/Value',Name,'');
  // the breakpoints of this group are not saved here.
  // They are saved by the TIDEBreakPoints object.
  XMLConfig.SetDeleteValue(Path+'InitialEnabled/Value',InitialEnabled,true);
end;

procedure TIDEBreakPointGroup.RemoveReference(const ABreakPoint: TIDEBreakPoint);
begin
  FReferences.Remove(ABreakPoint);
end;

procedure TIDEBreakPointGroup.SetEnabled(const AValue: Boolean);
var
  n: Integer;
begin
  if FEnabled <> AValue
  then begin
    FEnabled := AValue;
    for n := 0 to FBreakPoints.Count - 1 do
      TIDEBreakPoint(FBreakPoints[n]).Enabled := FEnabled;
  end;
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
    DestGroup.InitialEnabled:=InitialEnabled;
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
  NewCount:=XMLConfig.GetValue(Path+'Count',0);
  for i:=0 to NewCount-1 do begin
    NewGroup:=TIDEBreakPointGroup(inherited Add);
    NewGroup.LoadFromXMLConfig(XMLConfig,
                               Path+'Item'+IntToStr(i+1)+'/');
    OldGroup:=FindGroupByName(NewGroup.Name,NewGroup);
    if OldGroup<>nil then
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
  for i:=0 to Cnt-1 do begin
    CurGroup:=Items[i];
    CurGroup.SaveToXMLConfig(XMLConfig,
                             Path+'Item'+IntToStr(i+1)+'/');
  end;
end;

function TIDEBreakPointGroups.GetGroupByName(const GroupName: string
  ): TIDEBreakPointGroup;
begin
  Result:=FindGroupByName(GroupName,nil);
end;

function TIDEBreakPointGroups.FindGroupByName(const GroupName: string;
  Ignore: TIDEBreakPointGroup): TIDEBreakPointGroup;
var
  i: Integer;
begin
  i:=Count-1;
  while i>=0 do begin
    Result:=Items[i];
    if (AnsiCompareText(Result.Name,GroupName)=0)
    and (Ignore<>Result) then
      exit;
    dec(i);
  end;
  Result:=nil;
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
    Items[i].Enabled:=Items[i].InitialEnabled;
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

constructor TDBGField.Create(const AName: String; ADBGType: TDBGType; ALocation: TDBGFieldLocation; AFlags: TDBGFieldFlags);
begin
  inherited Create;
  FName := AName;
  FLocation := ALocation;
  FDBGType := ADBGType;
  FFlags := AFlags;
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
    Items[n].Free;

  FreeAndNil(FList);
  inherited;
end;

procedure TDBGFields.Add(const AField: TDBGField);
begin
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

constructor TDBGType.Create(AKind: TDBGSymbolKind; const ATypeName: String);
begin
  FKind := AKind;
  FTypeName := ATypeName;
  inherited Create;
end;

constructor TDBGType.Create(AKind: TDBGSymbolKind; const AArguments: TDBGTypes; AResult: TDBGType);
begin
  FKind := AKind;
  FArguments := AArguments;
  FResult := AResult;
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
{ TBaseWatch }
{ =========================================================================== }

procedure TBaseWatch.AssignTo(Dest: TPersistent);
begin
  if Dest is TBaseWatch
  then begin
    TBaseWatch(Dest).SetExpression(FExpression);
    TBaseWatch(Dest).SetEnabled(FEnabled);
    TBaseWatch(Dest).SetDisplayFormat(FDisplayFormat);
  end
  else inherited;
end;

constructor TBaseWatch.Create(ACollection: TCollection);
begin
  FEnabled := False;
  FValid := vsUnknown;
  inherited Create(ACollection);
end;


procedure TBaseWatch.DoEnableChange;
begin
  Changed;
end;

procedure TBaseWatch.DoExpressionChange;
begin
  Changed;
end;

procedure TBaseWatch.DoDisplayFormatChanged;
begin
  Changed;
end;

function TBaseWatch.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TBaseWatch.GetDisplayFormat: TWatchDisplayFormat;
begin
  Result := FDisplayFormat;
end;

procedure TBaseWatch.SetDisplayFormat(const AValue: TWatchDisplayFormat);
begin
  if AValue = FDisplayFormat then exit;
  FDisplayFormat := AValue;
  DoDisplayFormatChanged;
end;

function TBaseWatch.GetExpression: String;
begin
  Result := FExpression;
end;

function TBaseWatch.GetValid: TValidState;
begin
  Result := FValid;
end;

function TBaseWatch.GetValue: String;
begin
  if not Enabled
  then Result := '<disabled>'
  else begin
    case Valid of
      vsValid:   Result := '<valid>';
      vsInvalid: Result := '<invalid>';
    else
    {vsUnknown:}Result := '<unknown>';
    end;
  end;
end;

function TBaseWatch.GetTypeInfo: TDBGType;
begin
  Result:=nil;
end;

procedure TBaseWatch.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue
  then begin
    FEnabled := AValue;
    DoEnableChange;
  end;
end;

procedure TBaseWatch.SetExpression(const AValue: String);
begin
  if AValue <> FExpression
  then begin
    FExpression := AValue;
    DoExpressionChange;
  end;
end;

procedure TBaseWatch.SetValid(const AValue: TValidState);
begin
  if FValid <> AValue
  then begin
    FValid := AValue;
    Changed;
  end;
end;

{ =========================================================================== }
{ TIDEWatch }
{ =========================================================================== }

constructor TIDEWatch.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

destructor TIDEWatch.Destroy;
begin
  if (TIDEWatches(Collection) <> nil)
  then TIDEWatches(Collection).NotifyRemove(Self);
  inherited Destroy;
end;

procedure TIDEWatch.LoadFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
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

procedure TIDEWatch.SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string);
begin
  AConfig.SetDeleteValue(APath + 'Expression/Value', Expression, '');
  AConfig.SetDeleteValue(APath + 'Enabled/Value', Enabled, true);
  AConfig.SetDeleteValue(APath + 'DisplayStyle/Value',
    TWatchDisplayFormatNames[DisplayFormat], TWatchDisplayFormatNames[wdfDefault]);
end;


{ =========================================================================== }
{ TDBGWatch }
{ =========================================================================== }

constructor TDBGWatch.Create(ACollection: TCollection);
begin
  FSlave := nil;
  inherited Create(ACollection);
end;

destructor TDBGWatch.Destroy;
var
  SW: TBaseWatch;
begin
  SW := FSlave;
  FSlave := nil;
  if SW <> nil
  then SW.DoChanged; // in case UpDateCount was 0
  inherited Destroy;
end;

procedure TDBGWatch.DoChanged;
begin
  inherited DoChanged;
  if FSlave <> nil
  then FSlave.Changed;
end;

procedure TDBGWatch.DoChange;
begin
end;

procedure TDBGWatch.DoStateChange(const AOldState: TDBGState);
begin
end;

function TDBGWatch.GetDebugger: TDebugger;
begin
  Result := TDBGWatches(Collection).FDebugger;
end;

procedure TDBGWatch.SetSlave(const ASlave : TBaseWatch);
begin
  Assert((FSlave = nil) or (ASlave = nil), 'TDBGWatch.SetSlave already has a slave');
  FSlave := ASlave;
end;

{ =========================================================================== }
{ TBaseWatches }
{ =========================================================================== }

function TBaseWatches.Add(const AExpression: String): TBaseWatch;
begin
  Result := TBaseWatch(inherited Add);
  Result.Expression := AExpression;
end;

constructor TBaseWatches.Create(const AWatchClass: TBaseWatchClass);
begin
  inherited Create(AWatchClass);
end;

function TBaseWatches.Find(const AExpression: String): TBaseWatch;
var
  n: Integer;
  S: String;
begin
  S := UpperCase(AExpression);
  for n := 0 to Count - 1 do
  begin
    Result := TBaseWatch(GetItem(n));
    if UpperCase(Result.Expression) = S
    then Exit;
  end;
  Result := nil;
end;

{ =========================================================================== }
{ TIDEWatches }
{ =========================================================================== }

function TIDEWatches.Add(const AExpression: String): TIDEWatch;
begin
  // if this is modified, then also update LoadFromXMLConfig
  Result := TIDEWatch(inherited Add(AExpression));
  NotifyAdd(Result);
end;

procedure TIDEWatches.AddNotification(const ANotification: TIDEWatchesNotification);
begin
  FNotificationList.Add(ANotification);
  ANotification.AddReference;
end;

constructor TIDEWatches.Create(const AWatchClass: TIDEWatchClass);
begin
  FNotificationList := TList.Create;
  inherited Create(AWatchClass);
end;

destructor TIDEWatches.Destroy;
var
  n: Integer;
begin
  for n := FNotificationList.Count - 1 downto 0 do
    TDebuggerNotification(FNotificationList[n]).ReleaseReference;

  inherited;

  FreeAndNil(FNotificationList);
end;


function TIDEWatches.Find(const AExpression: String): TIDEWatch;
begin
  Result := TIDEWatch(inherited Find(AExpression));
end;

function TIDEWatches.GetItem(const AnIndex: Integer): TIDEWatch;
begin
  Result := TIDEWatch(inherited GetItem(AnIndex));
end;

procedure TIDEWatches.LoadFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
var
  NewCount: Integer;
  i: Integer;
  Watch: TIDEWatch;
begin
  Clear;
  NewCount := AConfig.GetValue(APath + 'Count', 0);
  for i := 0 to NewCount-1 do
  begin
    // Call inherited Add, so NotifyAdd can be send, after the Watch was loaded
    Watch := TIDEWatch(inherited Add(''));
    Watch.LoadFromXMLConfig(AConfig, Format('%sItem%d/', [APath, i + 1]));
    NotifyAdd(Watch);
  end;
end;

procedure TIDEWatches.NotifyAdd(const AWatch: TIDEWatch);
var
  n: Integer;
  Notification: TIDEWatchesNotification;
begin
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TIDEWatchesNotification(FNotificationList[n]);
    if Assigned(Notification.FOnAdd)
    then Notification.FOnAdd(Self, AWatch);
  end;
end;

procedure TIDEWatches.NotifyRemove(const AWatch: TIDEWatch);
var
  n: Integer;
  Notification: TIDEWatchesNotification;
begin
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TIDEWatchesNotification(FNotificationList[n]);
    if Assigned(Notification.FOnRemove)
    then Notification.FOnRemove(Self, AWatch);
  end;
end;

procedure TIDEWatches.RemoveNotification(const ANotification: TIDEWatchesNotification);
begin
  FNotificationList.Remove(ANotification);
  ANotification.ReleaseReference;
end;

procedure TIDEWatches.SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string);
var
  Cnt: Integer;
  i: Integer;
  Watch: TIDEWatch;
begin
  Cnt := Count;
  AConfig.SetDeleteValue(APath + 'Count', Cnt, 0);
  for i := 0 to Cnt - 1 do
  begin
    Watch := Items[i];
    Watch.SaveToXMLConfig(AConfig, Format('%sItem%d/', [APath, i + 1]));
  end;
end;

procedure TIDEWatches.SetItem(const AnIndex: Integer; const AValue: TIDEWatch);
begin
  inherited SetItem(AnIndex, AValue);
end;

procedure TIDEWatches.Update(Item: TCollectionItem);
var
  n, m, c: Integer;
  Notification: TIDEWatchesNotification;
begin
  // Note: Item will be nil in case all items need to be updated
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TIDEWatchesNotification(FNotificationList[n]);
    if not Assigned(Notification.FOnUpdate) then Continue;

    if Item = nil
    then begin
      m := 0;
      c := Count;
      while m < c do begin;
        Notification.FOnUpdate(Self, Items[m]);
        if c <> Count then begin
          m := Max(0, m - Max(0, Count - c));
          c := Count;
        end;
        inc(m);
      end;
    end
    else begin
      Notification.FOnUpdate(Self, TIDEWatch(Item));
    end;
  end;
end;

{ =========================================================================== }
{ TDBGWatches }
{ =========================================================================== }

function TDBGWatches.Add(const AExpression: String): TDBGWatch;
begin
  Result := TDBGWatch(inherited Add(AExpression));
end;

constructor TDBGWatches.Create(const ADebugger: TDebugger; const AWatchClass: TDBGWatchClass);
begin
  FDebugger := ADebugger;
  inherited Create(AWatchClass);
end;

procedure TDBGWatches.DoStateChange(const AOldState: TDBGState);
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
    GetItem(n).DoStateChange(AOldState);
end;

function TDBGWatches.Find(const AExpression: String): TDBGWatch;
begin
  Result := TDBGWatch(inherited Find(AExpression));
end;

function TDBGWatches.GetItem(const AnIndex: Integer): TDBGWatch;
begin
  Result := TDBGWatch(inherited GetItem(AnIndex));
end;

procedure TDBGWatches.SetItem(const AnIndex: Integer; const AValue: TDBGWatch);
begin
  inherited SetItem(AnIndex, AValue);
end;

procedure TDBGWatches.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  // notyfy only if collection is changed
  if (Item = nil) and Assigned(FOnChange)
  then FOnChange(Self);
end;


(******************************************************************************)
(******************************************************************************)
(**                                                                          **)
(**   L O C A L S                                                            **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

{ =========================================================================== }
{ TBaseLocals }
{ =========================================================================== }

function TBaseLocals.Count: Integer;
begin
  Result := 0;
end;

constructor TBaseLocals.Create;
begin
  inherited Create;
end;

function TBaseLocals.GetName(const AnIndex: Integer): String;
begin
  Result := '';
end;

function TBaseLocals.GetValue(const AnIndex: Integer): String;
begin
  Result := '';
end;

{ =========================================================================== }
{ TIDELocals }
{ =========================================================================== }

procedure TIDELocals.AddNotification(const ANotification: TIDELocalsNotification);
begin
  FNotificationList.Add(ANotification);
  ANotification.AddReference;
end;

constructor TIDELocals.Create;
begin
  FNotificationList := TList.Create;
  inherited Create;
end;

destructor TIDELocals.Destroy;
var
  n: Integer;
begin
  for n := FNotificationList.Count - 1 downto 0 do
    TDebuggerNotification(FNotificationList[n]).ReleaseReference;

  inherited;

  FreeAndNil(FNotificationList);
end;

procedure TIDELocals.NotifyChange;
var
  n: Integer;
  Notification: TIDELocalsNotification;
begin
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TIDELocalsNotification(FNotificationList[n]);
    if Assigned(Notification.FOnChange)
    then Notification.FOnChange(Self);
  end;
end;

procedure TIDELocals.RemoveNotification(const ANotification: TIDELocalsNotification);
begin
  FNotificationList.Remove(ANotification);
  ANotification.ReleaseReference;
end;

{ =========================================================================== }
{ TDBGLocals }
{ =========================================================================== }

function TDBGLocals.Count: Integer;
begin
  if  (FDebugger <> nil)
  and (FDebugger.State = dsPause)
  then Result := GetCount
  else Result := 0;
end;

constructor TDBGLocals.Create(const ADebugger: TDebugger);
begin
  inherited Create;
  FDebugger := ADebugger;
end;

procedure TDBGLocals.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TDBGLocals.DoStateChange(const AOldState: TDBGState);
begin
end;

procedure TDBGLocals.Changed;
begin
  DoChange;
end;

function TDBGLocals.GetCount: Integer;
begin
  Result := 0;
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

constructor TBaseRegisters.Create;
begin
  inherited Create;
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

{ =========================================================================== }
{ TDBGRegisters }
{ =========================================================================== }

function TDBGRegisters.Count: Integer;
begin
  if  (FDebugger <> nil)
  and (FDebugger.State = dsPause)
  then Result := GetCount
  else Result := 0;
end;

constructor TDBGRegisters.Create(const ADebugger: TDebugger);
begin
  inherited Create;
  FDebugger := ADebugger;
end;

procedure TDBGRegisters.DoChange;
begin
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
  const AFunctionName: String; const ASource: String; const AFullFileName: String;
  const ALine: Integer; AState: TCallStackEntryState = cseValid);
begin
  inherited Create;
  FIndex := AIndex;
  FAdress := AnAdress;
  FArguments := TStringlist.Create;
  if AnArguments <> nil
  then FArguments.Assign(AnArguments);
  FFunctionName := AFunctionName;
  FSource := ASource;
  FFullFileName := AFullFileName;
  FLine := ALine;
  FState := AState;
end;

constructor TCallStackEntry.CreateCopy(const ASource: TCallStackEntry);
begin
  Create(ASource.FIndex, ASource.FAdress, ASource.FArguments,
         ASource.FFunctionName, ASource.FSource, ASource.FFullFileName,
         ASource.FLine, ASource.FState);
end;

destructor TCallStackEntry.Destroy;
begin
  inherited;
  FreeAndNil(FArguments);
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

function TCallStackEntry.GetCurrent: Boolean;
begin
  Result := (FOwner <> nil) and (FOwner.GetCurrent = Self)
end;

function TCallStackEntry.GetFullFileName: String;
begin
  if FState = cseValid
  then Result := FFullFileName
  else Result := '';
end;

function TCallStackEntry.GetFunctionName: String;
begin
  case FState of
    cseValid:     Result := FFunctionName;
    cseRequested: Result := '<evaluating>';
    cseInvalid:   Result := '<unknown>';
  end;
end;

function TCallStackEntry.GetSource: String;
begin
  if FState = cseValid
  then Result := FSource
  else Result := '';
end;

procedure TCallStackEntry.SetCurrent(const AValue: Boolean);
begin
  if FOwner = nil then Exit;
  if GetCurrent = AValue then Exit;

  if AValue
  then FOwner.SetCurrent(self)
  else FOwner.SetCurrent(nil);
end;

{ =========================================================================== }
{ TBaseCallStack }
{ =========================================================================== }

function TBaseCallStack.CheckCount: Boolean;
begin
  Result := False;
end;

procedure TBaseCallStack.Clear;
begin
  FCount := -1;
end;

function TBaseCallStack.Count: Integer;
begin
  if (FCount = -1) and not CheckCount
  then Result := 0
  else Result := FCount;
end;

destructor TBaseCallStack.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TBaseCallStack.GetCurrent: TCallStackEntry;
begin
  Result := nil;
end;

function TBaseCallStack.GetEntry(AIndex: Integer): TCallStackEntry;
begin
  if (AIndex < 0)
  or (AIndex >= Count) then IndexError(Aindex);

  Result := InternalGetEntry(AIndex);
end;

function TBaseCallStack.IndexError(AIndex: Integer): TCallStackEntry;
begin
  Result:=nil;
  raise EInvalidOperation.CreateFmt('Index out of range (%d)', [AIndex]);
end;

function TBaseCallStack.InternalGetEntry(AIndex: Integer): TCallStackEntry;
begin
  Result := nil;
end;

procedure TBaseCallStack.PrepareRange(AIndex, ACount: Integer);
begin
end;

procedure TBaseCallStack.SetCount(ACount: Integer);
  procedure Error;
  begin
    raise EInvalidOperation.CreateFmt('Illegal count (%d < 0)', [ACount]);
  end;

begin
  if ACount < 0 then Error;
  FCount := ACount;
end;

procedure TBaseCallStack.SetCurrent(AValue: TCallStackEntry);
begin
end;


{ =========================================================================== }
{ TIDECallStack }
{ =========================================================================== }

procedure TIDECallStack.AddNotification(const ANotification: TIDECallStackNotification);
begin
  FNotificationList.Add(ANotification);
  ANotification.AddReference;
end;

constructor TIDECallStack.Create;
begin
  FNotificationList := TList.Create;
  inherited Create;
end;

destructor TIDECallStack.Destroy;
var
  n: Integer;
begin
  for n := FNotificationList.Count - 1 downto 0 do
    TDebuggerNotification(FNotificationList[n]).ReleaseReference;

  inherited;

  FreeAndNil(FNotificationList);
end;

procedure TIDECallStack.NotifyChange;
var
  n: Integer;
  Notification: TIDECallStackNotification;
begin
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TIDECallStackNotification(FNotificationList[n]);
    if Assigned(Notification.FOnChange)
    then Notification.FOnChange(Self);
  end;
end;

procedure TIDECallStack.NotifyCurrent;
var
  n: Integer;
  Notification: TIDECallStackNotification;
begin
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TIDECallStackNotification(FNotificationList[n]);
    if Assigned(Notification.FOnCurrent)
    then Notification.FOnCurrent(Self);
  end;
end;

procedure TIDECallStack.RemoveNotification(const ANotification: TIDECallStackNotification);
begin
  FNotificationList.Remove(ANotification);
  ANotification.ReleaseReference;
end;


{ =========================================================================== }
{ TDBGCallStack }
{ =========================================================================== }

procedure TDBGCallStack.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TDBGCallStack.CheckCount: Boolean;
begin
  Result := (FDebugger <> nil)
        and (FDebugger.State = dsPause);
  if Result then SetCount(0);
end;

procedure TDBGCallStack.Clear;
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

  inherited Clear;
end;

constructor TDBGCallStack.Create(const ADebugger: TDebugger);
begin
  FDebugger := ADebugger;
  FOldState := FDebugger.State;
  FEntries:= TMap.Create(its4, SizeOf(TCallStackEntry));
  inherited Create;
end;

function TDBGCallStack.CreateStackEntry(AIndex: Integer): TCallStackEntry;
begin
  Result := nil;
end;

procedure TDBGCallStack.CurrentChanged;
begin
  if Assigned(FOnCurrent) then FOnCurrent(Self);
end;

destructor TDBGCallStack.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FEntries);
end;

procedure TDBGCallStack.DoStateChange(const AOldState: TDBGState);
begin
  if FDebugger.State = dsPause
  then begin
    Changed;
  end
  else begin
    if (AOldState = dsPause) or (AOldState = dsNone) { Force clear on initialisation }
    then begin
      Clear;
      if Assigned(FOnClear) then FOnClear(Self);
    end;
  end;
end;

function TDBGCallStack.InternalGetEntry(AIndex: Integer): TCallStackEntry;
begin
  Result := nil;
  if FEntries.GetData(AIndex, Result) then Exit;

  Result := CreateStackEntry(AIndex);
  if Result = nil then Exit;
  FEntries.Add(AIndex, Result);
  Result.FOwner := Self;
end;

procedure TDBGCallStack.InternalSetEntry(AIndex: Integer; AEntry: TCallStackEntry);
var
  Dummy: TCallStackEntry;
begin
  if FEntries.GetData(AIndex, Dummy) then begin
    //debugln(['TDBGCallStack.InternalSetEntry: replacing existing entry ', Dummy.Line]);
    FEntries.Delete(AIndex);
    Dummy.Free;
  end;
  AEntry.FOwner := Self;
  FEntries.Add(AIndex, AEntry);
end;

procedure TDBGCallStack.PrepareEntries(AStartIndex, AEndIndex: Integer);
begin
end;

procedure TDBGCallStack.PrepareRange(AIndex, ACount: Integer);
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
  PrepareEntries(AIndex, ACount);
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

procedure TIDESignal.LoadFromXMLConfig (const AXMLConfig: TXMLConfig; const APath: string );
begin
  // TODO
end;

procedure TIDESignal.SaveToXMLConfig (const AXMLConfig: TXMLConfig; const APath: string );
begin
  // TODO
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

procedure TIDEExceptions.SetItem(const AIndex: Integer;
  const AValue: TIDEException);
begin
  inherited SetItem(Aindex, AValue);
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

procedure TIDEDisassembler.DoChanged;
var
  n: Integer;
  Notification: TIDEDisassemblerNotification;
begin
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TIDEDisassemblerNotification(FNotificationList[n]);
    if Assigned(Notification.FOnChange)
    then Notification.FOnChange(Self);
  end;
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
    while (i >= 0) and (AnotherRange.EntriesPtr[i]^.Addr >= RangeStartAddr)
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
    a:= RangeEndAddr;
    if LastAddr > a
    then a := LastAddr;
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
  AOffs := AnAddr - FEntries[Result].Addr;
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
