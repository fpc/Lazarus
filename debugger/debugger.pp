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
  Classes, SysUtils, Laz_XMLCfg,
  LCLProc, IDEProcs, DBGUtils;

type
  // datatype pointing to data on the target
  TDBGPtr = type QWord;

  TDBGLocationRec = record
    Address: TDBGPtr;
    FuncName: String;
    SrcFile: String;
    SrcLine: Integer;
  end;

  TDBGCommand = (
    dcRun,
    dcPause,
    dcStop,
    dcStepOver,
    dcStepInto,
    dcRunTo,
    dcJumpto,
    dcBreak,
    dcWatch,
    dcLocal,
    dcEvaluate,
    dcModify,
    dcEnvironment
    );
  TDBGCommands = set of TDBGCommand;
  
  TDBGState = (
    dsNone,
    dsIdle,
    dsStop,
    dsPause,
    dsInit,
    dsRun,
    dsError
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
  
  TBaseBreakPoint = class(TDelayedUdateItem)
  private
    FEnabled: Boolean;
    FExpression: String;
    FHitCount: Integer;
    FLine: Integer;
    FSource: String;
    FValid: TValidState;
    FInitialEnabled: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoExpressionChange; virtual;
    procedure DoEnableChange; virtual;
    procedure DoHit(const ACount: Integer; var AContinue: Boolean); virtual;
    procedure SetHitCount(const AValue: Integer);
    procedure SetValid(const AValue: TValidState);

  protected
    // virtual properties
    function GetEnabled: Boolean; virtual;
    function GetExpression: String; virtual;
    function GetHitCount: Integer; virtual;
    function GetLine: Integer; virtual;
    function GetSource: String; virtual;
    function GetSourceLine: Integer; virtual;
    function GetValid: TValidState; virtual;

    procedure SetEnabled(const AValue: Boolean); virtual;
    procedure SetExpression(const AValue: String); virtual;
    procedure SetInitialEnabled(const AValue: Boolean); virtual;
  public
    constructor Create(ACollection: TCollection); override;
    procedure SetLocation(const ASource: String; const ALine: Integer); virtual;// PublicProtectedFix ide/debugmanager.pas(867,32) Error: identifier idents no member "SetLocation"
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Expression: String read GetExpression write SetExpression;
    property HitCount: Integer read GetHitCount;
    property InitialEnabled: Boolean read FInitialEnabled write SetInitialEnabled;
    property Line: Integer read GetLine;
    property Source: String read GetSource;
    property SourceLine: Integer read GetSourceLine; // the current line of this breakpoint in the source
                                                     // this may differ from th location set
                                                     // todo: move to manager ?
    property Valid: TValidState read GetValid;
  end;
  TBaseBreakPointClass = class of TBaseBreakPoint;

  TIDEBreakPoint = class(TBaseBreakPoint)
  private
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
    procedure SetActions(const AValue: TIDEBreakPointActions); virtual;
    procedure SetGroup(const AValue: TIDEBreakPointGroup); virtual;
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
    property Group: TIDEBreakPointGroup read GetGroup write SetGroup;
    property Loading: Boolean read FLoading;
  end;
  TIDEBreakPointClass = class of TIDEBreakPoint;

  TDBGBreakPoint = class(TBaseBreakPoint)
  private
    FSlave: TBaseBreakPoint;
    function GetDebugger: TDebugger;
  protected
    procedure DoChanged; override;
    procedure DoStateChange(const AOldState: TDBGState); virtual;
    property  Debugger: TDebugger read GetDebugger;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    function GetSourceLine: integer; override;
    property Slave: TBaseBreakPoint read FSlave write FSlave;
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
(**   W A T C H E S                                                          **)
(**                                                                          **)
(******************************************************************************)
(******************************************************************************)

  { TBaseWatch }

  TBaseWatch = class(TDelayedUdateItem)
  private
    FEnabled: Boolean;
    FExpression: String;
    FValid: TValidState;
    function GetEnabled: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoEnableChange; virtual;
    procedure DoExpressionChange; virtual;
    procedure SetValid(const AValue: TValidState);
    
  protected
    // virtual properties
    function GetExpression: String; virtual;
    function GetValid: TValidState; virtual;
    function GetValue: String; virtual;

    procedure SetEnabled(const AValue: Boolean); virtual;
    procedure SetExpression(const AValue: String); virtual;
  public
    constructor Create(ACollection: TCollection); override;
  public
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Expression: String read GetExpression write SetExpression;
    property Valid: TValidState read GetValid;
    property Value: String read GetValue;
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

  TDBGWatch = class(TBaseWatch)
  private
    FSlave: TBaseWatch;
    function GetDebugger: TDebugger;
  protected
    procedure DoChanged; override;
    procedure DoStateChange(const AOldState: TDBGState); virtual;
    property Debugger: TDebugger read GetDebugger;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property Slave: TBaseWatch read FSlave write FSlave;
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

  TDBGWatches = class(TBaseWatches)
  private
    FDebugger: TDebugger;  // reference to our debugger
    function GetItem(const AnIndex: Integer): TDBGWatch;
    procedure SetItem(const AnIndex: Integer; const AValue: TDBGWatch);
  protected
    procedure DoStateChange(const AOldState: TDBGState); virtual;
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

  { TCallStackEntry }

  TCallStackEntry = class(TObject)
  private
    FIndex: Integer;
    FAdress: TDbgPtr;
    FFunctionName: String;
    FLine: Integer;
    FArguments: TStrings;
    FSource: String;
    function GetArgumentCount: Integer; 
    function GetArgumentName(const AnIndex: Integer): String;
    function GetArgumentValue(const AnIndex: Integer): String;
  protected
  public
    constructor Create(const AIndex:Integer; const AnAdress: TDbgPtr;
                       const AnArguments: TStrings; const AFunctionName: String;
                       const ASource: String; const ALine: Integer);
    constructor CreateCopy(const ASource: TCallStackEntry);
    destructor Destroy; override;
    property Adress: TDbgPtr read FAdress;
    property ArgumentCount: Integer read GetArgumentCount;
    property ArgumentNames[const AnIndex: Integer]: String read GetArgumentName;
    property ArgumentValues[const AnIndex: Integer]: String read GetArgumentValue;
    property FunctionName: String read FFunctionName;
    property Line: Integer read FLine;
    property Source: String read FSource;
  end;
  
  { TBaseCallStack }

  TBaseCallStack = class(TObject)
  private
    FEntries: TList;       // list of created entries
    FEntryIndex: TList;    // index to created entries
    FCount: Integer;
    function GetEntry(const AIndex: Integer): TCallStackEntry;
  protected
    function CheckCount: Boolean; virtual;
    procedure Clear;
    function CreateStackEntry(const AIndex: Integer): TCallStackEntry; virtual;
    procedure SetCount(const ACount: Integer); virtual;
  public
    function Count: Integer;
    constructor Create;
    destructor Destroy; override;
    function GetStackEntry(const AIndex: Integer): TCallStackEntry; virtual;
    property Entries[const AIndex: Integer]: TCallStackEntry read GetEntry;
  end;
  
  { TIDECallStack }

  TIDECallStackNotification = class(TDebuggerNotification)
  private
    FOnChange: TNotifyEvent;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TIDECallStack = class(TBaseCallStack)
  private
    FNotificationList: TList;
  protected
    procedure NotifyChange;
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
    FOldState: TDBGState;
    FOnChange: TNotifyEvent;
    FOnClear: TNotifyEvent;
  protected
    function CheckCount: Boolean; override;
    procedure DoStateChange(const AOldState: TDBGState); virtual;
    property Debugger: TDebugger read FDebugger;
  public
    constructor Create(const ADebugger: TDebugger);
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
  end;

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
    function Add(const AName: String): TBaseException;
    function Find(const AName: String): TBaseException;
  protected
    procedure ClearExceptions; virtual;
  public
    constructor Create(const AItemClass: TBaseExceptionClass);
    destructor Destroy; override;
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

  TDebuggerStateChangedEvent = procedure(ADebugger: TDebugger;
                                         AOldState: TDBGState) of object;
  TDBGOutputEvent = procedure(Sender: TObject; const AText: String) of object;
  TDBGCurrentLineEvent = procedure(Sender: TObject;
                                   const ALocation: TDBGLocationRec) of object;
  TDBGExceptionEvent = procedure(Sender: TObject; const AExceptionClass: String;
                                 const AExceptionText: String) of object;

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
    FEnvironment: TStrings;
    FExceptions: TDBGExceptions;
    FExitCode: Integer;
    FExternalDebugger: String;
    //FExceptions: TDBGExceptions;
    FFileName: String;
    FLocals: TDBGLocals;
    FShowConsole: Boolean;
    FSignals: TDBGSignals;
    FState: TDBGState;
    FCallStack: TDBGCallStack;
    FWatches: TDBGWatches;
    FOnCurrent: TDBGCurrentLineEvent;
    FOnException: TDBGExceptionEvent;
    FOnOutput: TDBGOutputEvent;
    FOnDbgOutput: TDBGOutputEvent;
    FOnState: TDebuggerStateChangedEvent;
    FWorkingDir: String;
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
    function  CreateCallStack: TDBGCallStack; virtual;
    function  CreateWatches: TDBGWatches; virtual;
    function  CreateSignals: TDBGSignals; virtual;
    function  CreateExceptions: TDBGExceptions; virtual;
    procedure DoCurrent(const ALocation: TDBGLocationRec);
    procedure DoDbgOutput(const AText: String);
    procedure DoException(const AExceptionClass: String; const AExceptionText: String);
    procedure DoOutput(const AText: String);
    procedure DoState(const OldState: TDBGState); virtual;
    function  ChangeFileName: Boolean; virtual;
    function  GetCommands: TDBGCommands;
    function  GetSupportedCommands: TDBGCommands; virtual;
    function  GetTargetWidth: Byte; virtual;
    function  RequestCommand(const ACommand: TDBGCommand;
                             const AParams: array of const): Boolean;
                             virtual; abstract; // True if succesful
    procedure SetExitCode(const AValue: Integer);
    procedure SetState(const AValue: TDBGState);
  public
    class function Caption: String; virtual;         // The name of the debugger as shown in the debuggeroptions
    class function ExePaths: String; virtual;        // The default locations of the exe
    class function HasExePath: boolean; virtual;        // If the debugger needs to have an exe path

    // debugger properties
    class function CreateProperties: TDebuggerProperties; virtual;         // Creates debuggerproperties
    class function GetProperties: TDebuggerProperties;                     // Get the current properties
    class procedure SetProperties(const AProperties: TDebuggerProperties); // Set the current properties

  public
    constructor Create(const AExternalDebugger: String); virtual; {Virtual constructor makes no sense}
                        //MWE: there will be a day that they do make sense :-)
                        // MG: there will be a day that they do make troubles :)
                        //MWE: do they ?
                        //MWE: Now they do make sense !
    destructor Destroy; override;

    procedure Init; virtual;                         // Initializes the debugger
    procedure Done; virtual;                         // Kills the debugger
    procedure Run;                                   // Starts / continues debugging
    procedure Pause;                                 // Stops running
    procedure Stop;                                  // quit debugging
    procedure StepOver;
    procedure StepInto;
    procedure RunTo(const ASource: String; const ALine: Integer);                // Executes til a certain point
    procedure JumpTo(const ASource: String; const ALine: Integer);               // No execute, only set exec point

    function  Evaluate(const AExpression: String; var AResult: String): Boolean; // Evaluates the given expression, returns true if valid
    function  Modify(const AExpression, AValue: String): Boolean;                // Modifies the given expression, returns true if valid

  public 
    property Arguments: String read FArguments write FArguments;                 // Arguments feed to the program
    property BreakPoints: TDBGBreakPoints read FBreakPoints;                     // list of all breakpoints
    property CallStack: TDBGCallStack read FCallStack;
    property Commands: TDBGCommands read GetCommands;                            // All current available commands of the debugger
    property DebuggerEnvironment: TStrings read FDebuggerEnvironment
                                           write SetDebuggerEnvironment;         // The environment passed to the debugger process
    property Environment: TStrings read FEnvironment write SetEnvironment;       // The environment passed to the debuggee
    property Exceptions: TDBGExceptions read FExceptions;                        // A list of exceptions we should ignore
    property ExitCode: Integer read FExitCode;
    property ExternalDebugger: String read FExternalDebugger;                    // The name of the debugger executable
    property FileName: String read FFileName write SetFileName;                  // The name of the exe to be debugged
    property Locals: TDBGLocals read FLocals;                                    // list of all localvars etc
    property Signals: TDBGSignals read FSignals;                                 // A list of actions for signals we know
    property ShowConsole: Boolean read FShowConsole write FShowConsole;          // Indicates if the debugger should create a console for the debuggee
    property State: TDBGState read FState;                                       // The current state of the debugger
    property SupportedCommands: TDBGCommands read GetSupportedCommands;          // All available commands of the debugger
    property TargetWidth: Byte read GetTargetWidth;                              // Currently only 32 or 64
    property Watches: TDBGWatches read FWatches;                                 // list of all watches etc
    property WorkingDir: String read FWorkingDir write FWorkingDir;              // The working dir of the exe being debugged
    // Events
    property OnCurrent: TDBGCurrentLineEvent read FOnCurrent write FOnCurrent;   // Passes info about the current line being debugged
    property OnDbgOutput: TDBGOutputEvent read FOnDbgOutput write FOnDbgOutput;  // Passes all debuggeroutput
    property OnException: TDBGExceptionEvent read FOnException write FOnException;  // Fires when the debugger received an exeption
    property OnOutput: TDBGOutputEvent read FOnOutput write FOnOutput;           // Passes all output of the debugged target
    property OnState: TDebuggerStateChangedEvent read FOnState write FOnState;   // Fires when the current state of the debugger changes
  end;
  TDebuggerClass = class of TDebugger;
  
const
  DBGCommandNames: array[TDBGCommand] of string = (
    'Run',
    'Pause',
    'Stop',
    'StepOver',
    'StepInto',
    'RunTo',
    'Jumpto',
    'Break',
    'Watch',
    'Local',
    'Evaluate',
    'Modify',
    'Environment'
    );
    
  DBGStateNames: array[TDBGState] of string = (
    'None',
    'Idle',
    'Stop',
    'Pause',
    'Init',
    'Run',
    'Error'
    );
    
  DBGBreakPointActionNames: array[TIDEBreakPointAction] of string = (
    'Stop',
    'EnableGroup',
    'DisableGroup'
    );
    
function DBGCommandNameToCommand(const s: string): TDBGCommand;
function DBGStateNameToState(const s: string): TDBGState;
function DBGBreakPointActionNameToAction(const s: string): TIDEBreakPointAction;

(******************************************************************************)
(******************************************************************************)
(******************************************************************************)
(******************************************************************************)

implementation

const
  INTERNAL_STATES = [dsInit];

  COMMANDMAP: array[TDBGState] of TDBGCommands = (
  {dsNone } [],
  {dsIdle } [dcEnvironment],
  {dsStop } [dcRun, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak, dcWatch,
             dcEvaluate, dcEnvironment],
  {dsPause} [dcRun, dcStop, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak,
             dcWatch, dcLocal, dcEvaluate, dcModify, dcEnvironment],
  {dsInit } [],
  {dsRun  } [dcPause, dcStop, dcBreak, dcWatch, dcEnvironment],
  {dsError} [dcStop]
  );
  
var
  MDebuggerPropertiesList: TStringlist;

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
begin
  inherited Create;
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
  FCallStack := CreateCallStack;
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

function TDebugger.CreateExceptions: TDBGExceptions;
begin
  Result := TDBGExceptions.Create(Self, TDBGException);
end;

function TDebugger.CreateLocals: TDBGLocals;
begin
  Result := TDBGLocals.Create(Self);
end;

class function TDebugger.CreateProperties: TDebuggerProperties; 
begin
  Result := TDebuggerProperties.Create;
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
begin
  // don't call events
  FOnState := nil;
  FOnCurrent := nil;
  FOnOutput := nil;
  FOnDbgOutput := nil;

  if FState <> dsNone
  then Done;

  FBreakPoints.FDebugger := nil;
  FLocals.FDebugger := nil;
  FCallStack.FDebugger := nil;
  FWatches.FDebugger := nil;

  FreeAndNil(FExceptions);
  FreeAndNil(FBreakPoints);
  FreeAndNil(FLocals);
  FreeAndNil(FCallStack);
  FreeAndNil(FWatches);
  FreeAndNil(FDebuggerEnvironment);
  FreeAndNil(FEnvironment);
  FreeAndNil(FCurEnvironment);
  FreeAndNil(FSignals);
  inherited;
end;

procedure TDebugger.Done;
begin
  SetState(dsNone);
  FEnvironment.Clear;
  FCurEnvironment.Clear;
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

procedure TDebugger.DoException(const AExceptionClass: String;
  const AExceptionText: String);
begin
  if Assigned(FOnException) then
    FOnException(Self, AExceptionClass, AExceptionText);
end;

procedure TDebugger.DoOutput(const AText: String);
begin
  if Assigned(FOnOutput) then FOnOutput(Self, AText);
end;

procedure TDebugger.DoState(const OldState: TDBGState);
begin
  if State in INTERNAL_STATES then Exit;
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

function TDebugger.Evaluate(const AExpression: String;
  var AResult: String): Boolean;
begin
  Result := ReqCmd(dcEvaluate, [AExpression, @AResult]);
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
  Result := False;
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
  if AValue <> FState
  then begin
    OldState := FState;            
    FState := AValue;
    FBreakpoints.DoStateChange(OldState);
    FLocals.DoStateChange(OldState);
    FCallStack.DoStateChange(OldState);
    FWatches.DoStateChange(OldState);
    DoState(OldState);
  end;
end;

procedure TDebugger.StepInto;
begin
  if ReqCmd(dcStepInto, []) then exit;
  DebugLn('TDebugger.StepInto Class=',ClassName,' failed.');
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

procedure TBaseBreakPoint.AssignTo(Dest: TPersistent);
var
  DestBreakPoint: TBaseBreakPoint;
begin
  // updatelock is set in source.assignto
  if Dest is TBaseBreakPoint
  then begin
    DestBreakPoint:=TBaseBreakPoint(Dest);
    DestBreakPoint.SetLocation(FSource, FLine);
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
  FExpression := '';
  FInitialEnabled := False;
  inherited Create(ACollection);
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

function TBaseBreakPoint.GetSourceLine: Integer;
begin
  Result := Line;
end;

function TBaseBreakPoint.GetValid: TValidState;
begin
  Result := FValid;
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

procedure TIDEBreakPoint.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TIDEBreakPoint
  then begin
    TIDEBreakPoint(Dest).Actions := FActions;
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
  then SBP.Changed;
  inherited Destroy;
end;

function TDBGBreakPoint.GetSourceLine: integer;
begin
  if Slave<>nil then
    Result:=Slave.GetSourceLine
  else
    Result:=inherited GetSourceLine;
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
    SetLocation(FSource, SourceLine);
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

function TBaseWatch.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TBaseWatch.GetExpression: String;
begin
  Result := FExpression;
end;

function TBaseWatch.GetValid: TValidState;
begin
  Result := vsUnknown;
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
begin
  Expression := AConfig.GetValue(APath + 'Expression/Value', '');
  Enabled := AConfig.GetValue(APath + 'Enabled/Value', true);
end;

procedure TIDEWatch.SaveToXMLConfig(const AConfig: TXMLConfig; const APath: string);
begin
  AConfig.SetDeleteValue(APath + 'Expression/Value', Expression, '');
  AConfig.SetDeleteValue(APath + 'Enabled/Value', Enabled, true);
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
  then SW.Changed;
  inherited Destroy;
end;

procedure TDBGWatch.DoChanged;
begin
  inherited DoChanged;
  if FSlave <> nil
  then FSlave.Changed;
end;

procedure TDBGWatch.DoStateChange(const AOldState: TDBGState);
begin
end;

function TDBGWatch.GetDebugger: TDebugger;
begin
  Result := TDBGWatches(Collection).FDebugger;
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
    Watch := TIDEWatch(inherited Add(''));
    Watch.LoadFromXMLConfig(AConfig, Format('%sItem%d/', [APath, i + 1]));
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
  n: Integer;
  Notification: TIDEWatchesNotification;
begin
  // Note: Item will be nil in case all items need to be updated
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TIDEWatchesNotification(FNotificationList[n]);
    if Assigned(Notification.FOnUpdate)
    then Notification.FOnUpdate(Self, TIDEWatch(Item));
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

function TDBGLocals.GetCount: Integer;
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
  const AFunctionName: String; const ASource: String; const ALine: Integer);
begin
  inherited Create;   
  FIndex := AIndex;
  FAdress := AnAdress;
  FArguments := TStringlist.Create;
  FArguments.Assign(AnArguments);
  FFunctionName := AFunctionName;
  FSource := ASource;
  FLine := ALine;
end;

constructor TCallStackEntry.CreateCopy(const ASource: TCallStackEntry);
begin
  Create(ASource.FIndex, ASource.FAdress, ASource.FArguments,
         ASource.FunctionName, ASource.FSource, ASource.FLine);
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

{ =========================================================================== }
{ TBaseCallStack }
{ =========================================================================== }

function TBaseCallStack.CheckCount: Boolean;
begin
  Result := False;
end;

procedure TBaseCallStack.Clear;
var
  n:Integer;
begin
  for n := 0 to FEntries.Count - 1 do
    TObject(FEntries[n]).Free;

  FEntries.Clear;
  FEntryIndex.Clear;
  FCount := -1;
end;

function TBaseCallStack.CreateStackEntry(const AIndex: Integer): TCallStackEntry;
begin
  Result := nil;
end;

function TBaseCallStack.Count: Integer;
begin
  if (FCount = -1)
  and not CheckCount
  then Result := 0
  else Result := FCount;
end;

constructor TBaseCallStack.Create;
begin
  FEntries := TList.Create;
  FEntryIndex := TList.Create;
  inherited Create;
end;

destructor TBaseCallStack.Destroy;
begin
  Clear;
  inherited Destroy;
  FreeAndNil(FEntries);
  FreeAndNil(FEntryIndex);
end;

function TBaseCallStack.GetEntry(const AIndex: Integer): TCallStackEntry;
begin
  if (AIndex < 0)
  or (AIndex >= Count)
  then raise EInvalidOperation.CreateFmt('Index out of range (%d)', [AIndex]);

  Result := GetStackEntry(AIndex);
end;

function TBaseCallStack.GetStackEntry(const AIndex: Integer): TCallStackEntry;
var
  idx: PtrInt;
begin
  idx := PtrInt(FEntryIndex[AIndex]);
  if idx = -1
  then begin
    // not created yet
    Result := CreateStackEntry(AIndex);
    if Result = nil then Exit;
    idx := FEntries.Add(Result);
    FEntryIndex[AIndex] := Pointer(idx);
  end
  else begin
    Result := TCallStackEntry(FEntries[idx]);
  end;
end;

procedure TBaseCallStack.SetCount(const ACount: Integer);
var
  n: integer;
begin
  if FCount = ACount then Exit;
  Assert(ACount >= 0);

  FEntryIndex.Count := ACount;
  if FCount < 0 then FCount := 0;
  for n := FCount to ACount - 1 do
    FEntryIndex[n] := Pointer(-1);
  
  FCount := ACount;
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

procedure TIDECallStack.RemoveNotification(const ANotification: TIDECallStackNotification);
begin
  FNotificationList.Remove(ANotification);
  ANotification.ReleaseReference;
end;

{ =========================================================================== }
{ TDBGCallStack }
{ =========================================================================== }

function TDBGCallStack.CheckCount: Boolean;
begin
  Result := (FDebugger <> nil)
        and (FDebugger.State = dsPause);
  if Result then SetCount(0);
end;

constructor TDBGCallStack.Create(const ADebugger: TDebugger);
begin
  FDebugger := ADebugger;
  FOldState := FDebugger.State;
  inherited Create;
end;

procedure TDBGCallStack.DoStateChange(const AOldState: TDBGState);
begin
  if FDebugger.State = dsPause
  then begin
    if Assigned(FOnChange) then FOnChange(Self);
  end
  else begin
    if (AOldState = dsPause) or (AOldState = dsNone) { Force clear on initialisation } 
    then begin 
      Clear;
      if Assigned(FOnClear) then FOnClear(Self);
    end;
  end;          
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
  then raise EDBGExceptions.Create('Duplicate name');

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
end;

destructor TBaseExceptions.Destroy;
begin
  ClearExceptions;
  inherited Destroy;
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

initialization
  MDebuggerPropertiesList := nil;

finalization
  DoFinalization;


end.
