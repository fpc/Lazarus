{ $Id$ }
{                        ----------------------------------------
                           Debugger.pp  -  Debugger base classes
                         ----------------------------------------

 @created(Wed Feb 25st WET 2001)
 @lastmod($Date$)
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
  Classes, SysUtils, Laz_XMLCfg, IDEProcs, DBGUtils;

type
  TDBGLocationRec = record
    Adress: Pointer;
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
    dcModify
    );
  TDBGCommands = set of TDBGCommand;
  
  TDBGState = (
    dsNone,
    dsIdle,
    dsStop,
    dsPause,
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

  dsRun:
    The target is running.

  dsError:
    Something unforseen has happened. A shutdown of the debugger is in
    most cases needed.
  --------------------------------------------------------------------------

}

  TValidState = (vsUnknown, vsValid, vsInvalid);


const
  dcRunCommands = [dcRun,dcStepInto,dcStepOver,dcRunTo];
  dsRunStates = [dsRun];

  XMLBreakPointsNode = 'BreakPoints';
  XMLBreakPointGroupsNode = 'BreakPointGroups';
  XMLWatchesNode = 'Watches';

type
{ ---------------------------------------------------------
  TDebuggerNotification is a reference counted baseclass
  for handling notifications for locals, watches, breakpoints etc.
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

  TDBGBreakPoints = class;
  TDBGBreakPointGroup = class;
  TDBGBreakPointGroups = class;
  TDBGWatches = class;
  TDebugger = class;

  TOnSaveFilenameToConfig = procedure(var Filename: string) of object;
  TOnLoadFilenameFromConfig = procedure(var Filename: string) of object;
  TOnGetGroupByName = function(const GroupName: string): TDBGBreakPointGroup of object;

  { TDBGBreakPoint }

  TDBGBreakPointAction = (
    bpaStop,
    bpaEnableGroup,
    bpaDisableGroup
    );
  TDBGBreakPointActions = set of TDBGBreakPointAction;

  TDBGBreakPoint = class(TCollectionItem)
  private
    FGroup: TDBGBreakPointGroup;
    FInitialEnabled: Boolean;
    FLoading: Boolean;
    FValid: TValidState;
    FEnabled: Boolean;
    FHitCount: Integer;
    FExpression: String;
    FSource: String;
    FLine: Integer;
    FFirstRun: Boolean;
    FActions: TDBGBreakPointActions;
    FDisableGroupList: TList;
    FEnableGroupList: TList;                                  
    function  GetDebugger: TDebugger;
    procedure SetActions(const AValue: TDBGBreakPointActions);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetExpression(const AValue: String);
    procedure SetGroup(const AValue: TDBGBreakPointGroup);
    procedure SetInitialEnabled(const AValue: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DisableGroups;
    procedure DoActionChange; virtual;
    procedure DoEnableChange; virtual;
    procedure DoExpressionChange; virtual;
    procedure DoStateChange; virtual;
    procedure EnableGroups;
    procedure SetHitCount(const AValue: Integer);
    procedure SetLocation(const ASource: String; const ALine: Integer); virtual;
    procedure SetValid(const AValue: TValidState);
    property  Debugger: TDebugger read GetDebugger;
    procedure RemoveFromGroupList(const AGroup: TDBGBreakPointGroup;
                                  const AGroupList: TList);
    procedure ClearGroupList(const AGroupList: TList);
    procedure CopyGroupList(SrcGroupList, DestGroupList: TList;
                            DestGroups: TDBGBreakPointGroups);
    procedure CopyAllGroupLists(SrcBreakPoint: TDBGBreakPoint;
                                DestGroups: TDBGBreakPointGroups);
    procedure ClearAllGroupLists;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure AddDisableGroup(const AGroup: TDBGBreakPointGroup);
    procedure AddEnableGroup(const AGroup: TDBGBreakPointGroup);
    procedure RemoveDisableGroup(const AGroup: TDBGBreakPointGroup);
    procedure RemoveEnableGroup(const AGroup: TDBGBreakPointGroup);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                      const OnLoadFilename: TOnLoadFilenameFromConfig;
                      const OnGetGroup: TOnGetGroupByName); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                      const OnSaveFilename: TOnSaveFilenameToConfig); virtual;
  public
    property Actions: TDBGBreakPointActions read FActions write SetActions;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Expression: String read FExpression write SetExpression;
    property Group: TDBGBreakPointGroup read FGroup write SetGroup;
    property HitCount: Integer read FHitCount;
    property InitialEnabled: Boolean read FInitialEnabled write SetInitialEnabled;
    property Line: Integer read FLine;
    property Source: String read FSource;
    property Valid: TValidState read FValid;
    property Loading: Boolean read FLoading;
  end;
  TDBGBreakPointClass = class of TDBGBreakPoint;


  { TDBGBreakPoints }

  TDBGBreakPointsEvent = procedure(const ASender: TDBGBreakPoints;
                                   const ABreakpoint: TDBGBreakPoint) of object;

  TDBGBreakPointsNotification = class(TDebuggerNotification)
  private
    FOnAdd:    TDBGBreakPointsEvent;
    FOnUpdate: TDBGBreakPointsEvent;//Item will be nil in case all items need to be updated
    FOnRemove: TDBGBreakPointsEvent;
  public
    property OnAdd:    TDBGBreakPointsEvent read FOnAdd    write FOnAdd;
    property OnUpdate: TDBGBreakPointsEvent read FOnUpdate write FOnUpdate;
    property OnRemove: TDBGBreakPointsEvent read FOnRemove write FonRemove;
  end;

  TDBGBreakPoints = class(TCollection)
  private
    FDebugger: TDebugger;  // reference to our debugger
    FNotificationList: TList;
    function GetItem(const AnIndex: Integer): TDBGBreakPoint;
    procedure SetItem(const AnIndex: Integer; const AValue: TDBGBreakPoint);
    procedure NotifyRemove(const ABreakpoint: TDBGBreakPoint); // called by breakpoint when destructed
    procedure NotifyAdd(const ABreakPoint: TDBGBreakPoint);    // called when a breakpoint is added
  protected
    procedure DoStateChange; virtual;
    procedure Update(Item: TCollectionItem); override;
    property  Debugger: TDebugger read FDebugger;
  public
    constructor Create(const ADebugger: TDebugger;
                       const ABreakPointClass: TDBGBreakPointClass);
    destructor Destroy; override;
    function Add(const ASource: String; const ALine: Integer): TDBGBreakPoint;
    function Find(const ASource: String; const ALine: Integer): TDBGBreakPoint;
    function FindBreakPoint(const ASource: String; const ALine: Integer;
                            Ignore: TDBGBreakPoint): TDBGBreakPoint;
    procedure AddNotification(const ANotification: TDBGBreakPointsNotification);
    procedure RemoveNotification(const ANotification: TDBGBreakPointsNotification);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                      const OnLoadFilename: TOnLoadFilenameFromConfig;
                      const OnGetGroup: TOnGetGroupByName); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                      const OnSaveFilename: TOnSaveFilenameToConfig); virtual;
    procedure InitTargetStart; virtual;
  public
    property Items[const AnIndex: Integer]: TDBGBreakPoint read GetItem
                                                         write SetItem; default;
  end;
  
  
  { TDBGBreakPointGroup }

  TDBGBreakPointGroup = class(TCollectionItem)
  private
    FEnabled: Boolean;
    FInitialEnabled: Boolean;
    FName: String;
    FBreakpoints: TList;// A list of breakpoints that member
    FReferences: TList; // A list of breakpoints that refer to us through En/disable group
    function GetBreakpoint(const AIndex: Integer): TDBGBreakPoint;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetInitialEnabled(const AValue: Boolean);
    procedure SetName(const AValue: String);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AddReference(const ABreakPoint: TDBGBreakPoint);
    procedure RemoveReference(const ABreakPoint: TDBGBreakPoint);
  public
    function Add(const ABreakPoint: TDBGBreakPoint): Integer;
    function Count: Integer;
    constructor Create(ACollection: TCollection); override;
    procedure Delete(const AIndex: Integer);
    destructor Destroy; override;
    function Remove(const ABreakPoint: TDBGBreakPoint): Integer;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig;
                                const Path: string); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig;
                              const Path: string); virtual;
  public
    property Breakpoints[const AIndex: Integer]: TDBGBreakPoint read GetBreakpoint;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property InitialEnabled: Boolean read FInitialEnabled write SetInitialEnabled;
    property Name: String read FName write SetName;
  end;


  { TDBGBreakPointGroups }

  TDBGBreakPointGroups = class(TCollection)
  private
    function GetItem(const AnIndex: Integer): TDBGBreakPointGroup;
    procedure SetItem(const AnIndex: Integer; const AValue: TDBGBreakPointGroup);
  protected
  public
    constructor Create;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig;
                                const Path: string); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig;
                              const Path: string); virtual;
    function GetGroupByName(const GroupName: string): TDBGBreakPointGroup;
    function FindGroupByName(const GroupName: string;
                             Ignore: TDBGBreakPointGroup): TDBGBreakPointGroup;
    function IndexOfGroupWithName(const GroupName: string;
                                  Ignore : TDBGBreakPointGroup): integer;
    procedure InitTargetStart; virtual;
    procedure Regroup(SrcGroups: TDBGBreakPointGroups;
                      SrcBreakPoints, DestBreakPoints: TDBGBreakPoints);
  public
    property Items[const AnIndex: Integer]: TDBGBreakPointGroup
                                            read GetItem write SetItem; default;
  end;
  
  
  { TDBGWatch }

  TDBGWatch = class(TCollectionItem)
  private
    FEnabled: Boolean;
    FExpression: String;
    FInitialEnabled: Boolean;
    FValid: TValidState;
    function  GetDebugger: TDebugger;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetExpression(const AValue: String);
    procedure SetInitialEnabled(const AValue: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoEnableChange; virtual;
    procedure DoExpressionChange; virtual;
    procedure DoStateChange; virtual;
    function  GetValue: String; virtual;
    function  GetValid: TValidState; virtual;
    procedure SetValid(const AValue: TValidState);
    property  Debugger: TDebugger read GetDebugger;
  public
    constructor Create(ACollection: TCollection); override;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig;
                                const Path: string); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig;
                              const Path: string); virtual;
  public
    property Enabled: Boolean read FEnabled write SetEnabled;
    property InitialEnabled: Boolean read FInitialEnabled write SetInitialEnabled;
    property Expression: String read FExpression write SetExpression;
    property Valid: TValidState read GetValid;
    property Value: String read GetValue;
  end;

  TDBGWatchClass = class of TDBGWatch;


  { TDBGWatches }

  TDBGWatchesEvent =
       procedure(const ASender: TDBGWatches; const AWatch: TDBGWatch) of object;
       
  TDBGWatchesNotification = class(TDebuggerNotification)
  private
    FOnAdd:    TDBGWatchesEvent;
    FOnUpdate: TDBGWatchesEvent;//Item will be nil in case all items need to be updated
    FOnRemove: TDBGWatchesEvent;
  public
    property OnAdd:    TDBGWatchesEvent read FOnAdd    write FOnAdd;
    property OnUpdate: TDBGWatchesEvent read FOnUpdate write FOnUpdate;
    property OnRemove: TDBGWatchesEvent read FOnRemove write FonRemove;
  end;

  TDBGWatches = class(TCollection)
  private
    FDebugger: TDebugger;  // reference to our debugger
    FNotificationList: TList;
    function GetItem(const AnIndex: Integer): TDBGWatch;
    procedure SetItem(const AnIndex: Integer; const AValue: TDBGWatch);
    procedure Removed(const AWatch: TDBGWatch); // called by watch when destructed
  protected
    procedure DoStateChange; virtual;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(const ADebugger: TDebugger;
                       const AWatchClass: TDBGWatchClass);
    destructor Destroy; override;
    function Add(const AExpression: String): TDBGWatch;
    function Find(const AExpression: String): TDBGWatch;
    procedure AddNotification(const ANotification: TDBGWatchesNotification);
    procedure RemoveNotification(const ANotification: TDBGWatchesNotification);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig;
                                const Path: string); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig;
                              const Path: string); virtual;
    procedure InitTargetStart; virtual;
  public
    property Items[const AnIndex: Integer]: TDBGWatch read GetItem
                                                      write SetItem; default;
  end;
  
  
  { TDBGLocals }

  TDBGLocals = class(TObject)
  private
    FDebugger: TDebugger;  // reference to our debugger
    FOnChange: TNotifyEvent;
  protected
    procedure DoChange;
    procedure DoStateChange; virtual;
    function GetName(const AnIndex: Integer): String; virtual;
    function GetValue(const AnIndex: Integer): String; virtual;
    property Debugger: TDebugger read FDebugger;
  public
    constructor Create(const ADebugger: TDebugger);
    function Count: Integer; virtual;
  public
    property Names[const AnIndex: Integer]: String read GetName;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Values[const AnIndex: Integer]: String read GetValue;
  end;
  
  
  { TDBGCallStackEntry }

  TDBGCallStackEntry = class(TObject)
  private
    FIndex: Integer;
    FAdress: Pointer;
    FFunctionName: String;
    FLine: Integer;
    FArguments: TStrings;
    FSource: String;
    function GetArgumentCount: Integer; 
    function GetArgumentName(const AnIndex: Integer): String;
    function GetArgumentValue(const AnIndex: Integer): String;
  protected
  public
    constructor Create(const AIndex:Integer; const AnAdress: Pointer;
                       const AnArguments: TStrings; const AFunctionName: String;
                       const ASource: String; const ALine: Integer);
    destructor Destroy; override;
    property Adress: Pointer read FAdress;
    property ArgumentCount: Integer read GetArgumentCount;
    property ArgumentNames[const AnIndex: Integer]: String read GetArgumentName;
    property ArgumentValues[const AnIndex: Integer]: String read GetArgumentValue;
    property FunctionName: String read FFunctionName;
    property Line: Integer read FLine;
    property Source: String read FSource;
  end;
  
  
  { TDBGCallStack }

  TDBGCallStack = class(TObject)
  private
    FDebugger: TDebugger;  // reference to our debugger
    FEntries: TList;       // list of created entries
    FOldState: TDBGState;  // records the previous debugger state 
    FOnChange: TNotifyEvent;
    procedure Clear;
    function GetStackEntry(const AIndex: Integer): TDBGCallStackEntry;
  protected
    procedure DoChange;
    function CreateStackEntry(const AIndex: Integer): TDBGCallStackEntry; virtual; 
    procedure DoStateChange; virtual;
    function GetCount: Integer; virtual;
    property Debugger: TDebugger read FDebugger;
  public   
    function Count: Integer;
    constructor Create(const ADebugger: TDebugger); 
    destructor Destroy; override;
    property Entries[const AIndex: Integer]: TDBGCallStackEntry read GetStackEntry;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  { TDebugger }

  TDebuggerStateChangedEvent = procedure(ADebugger: TDebugger;
                                         OldState: TDBGState) of object;
  TDBGOutputEvent = procedure(Sender: TObject; const AText: String) of object;
  TDBGCurrentLineEvent = procedure(Sender: TObject;
                                   const ALocation: TDBGLocationRec) of object;
  TDBGExceptionEvent = procedure(Sender: TObject; const AExceptionID: Integer;
                                 const AExceptionText: String) of object;

  TDebugger = class(TObject)
  private
    FArguments: String;
    FBreakPoints: TDBGBreakPoints;
    FBreakPointGroups: TDBGBreakPointGroups;
    FEnvironment: TStrings;
    FExitCode: Integer;
    FExternalDebugger: String;
    FFileName: String;
    FLocals: TDBGLocals;
    FState: TDBGState;
    FCallStack: TDBGCallStack;
    FWatches: TDBGWatches;
    FOnCurrent: TDBGCurrentLineEvent;
    FOnException: TDBGExceptionEvent;
    FOnOutput: TDBGOutputEvent;
    FOnDbgOutput: TDBGOutputEvent;
    FOnState: TDebuggerStateChangedEvent;
    function  GetState: TDBGState;
    function  ReqCmd(const ACommand: TDBGCommand;
                     const AParams: array of const): Boolean;
    procedure SetEnvironment(const AValue: TStrings);
    procedure SetFileName(const AValue: String);
  protected
    function  CreateBreakPoints: TDBGBreakPoints; virtual;
    function  CreateLocals: TDBGLocals; virtual;
    function  CreateCallStack: TDBGCallStack; virtual;
    function  CreateWatches: TDBGWatches; virtual;
    procedure DoCurrent(const ALocation: TDBGLocationRec);
    procedure DoDbgOutput(const AText: String);
    procedure DoException(const AExceptionID: Integer; const AExceptionText: String);
    procedure DoOutput(const AText: String);
    procedure DoState(const OldState: TDBGState);
    function  ChangeFileName: Boolean; virtual;
    function  GetCommands: TDBGCommands;
    function  GetSupportedCommands: TDBGCommands; virtual;
    function  RequestCommand(const ACommand: TDBGCommand;
                             const AParams: array of const): Boolean;
                             virtual; abstract; // True if succesful
    procedure SetExitCode(const AValue: Integer);
    procedure SetState(const AValue: TDBGState);
    procedure InitTargetStart; virtual;
  public
    constructor Create(const AExternalDebugger: String); {virtual; Virtual constructor makes no sense}
                        //MWE: there will be a day that they do make sense :-)
                        // MG: there will be a day that they do make troubles :)
                        //MWE: do they ?
    destructor Destroy; override;

    procedure Init; virtual;                         // Initializes the debugger
    procedure Done; virtual;                         // Kills the debugger
    procedure Run;                                   // Starts / continues debugging
    procedure Pause;                                 // Stops running
    procedure Stop;                                  // quit debugging
    procedure StepOver;
    procedure StepInto;
    procedure RunTo(const ASource: String; const ALine: Integer); virtual;       // Executes til a certain point
    procedure JumpTo(const ASource: String; const ALine: Integer); virtual;      // No execute, only set exec point

    function  Evaluate(const AExpression: String; var AResult: String): Boolean;  // Evaluates the given expression, returns true if valid
    function  Modify(const AExpression, AValue: String): Boolean;                 // Modifies the given expression, returns true if valid
    function  TargetIsStarted: boolean; virtual;

    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                      const OnLoadFilename: TOnLoadFilenameFromConfig); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                      const OnSaveFilename: TOnSaveFilenameToConfig); virtual;

  public
    property Arguments: String read FArguments write FArguments;                 // Arguments feed to the program
    property BreakPointGroups: TDBGBreakPointGroups read FBreakPointGroups;      // list of all breakpointgroups
    property BreakPoints: TDBGBreakPoints read FBreakPoints;                     // list of all breakpoints
    property CallStack: TDBGCallStack read FCallStack;
    property Commands: TDBGCommands read GetCommands;                            // All current available commands of the debugger
    property Environment: TStrings read FEnvironment write SetEnvironment;
    property ExitCode: Integer read FExitCode;
    property ExternalDebugger: String read FExternalDebugger;
    property FileName: String read FFileName write SetFileName;                  // The name of the exe to be debugged
    property Locals: TDBGLocals read FLocals;
    property OnCurrent: TDBGCurrentLineEvent read FOnCurrent write FOnCurrent;   // Passes info about the current line being debugged
    property OnDbgOutput: TDBGOutputEvent read FOnDbgOutput write FOnDbgOutput;  // Passes all debuggeroutput
    property OnException: TDBGExceptionEvent read FOnException write FOnException;  // Fires when the debugger received an exeption
    property OnOutput: TDBGOutputEvent read FOnOutput write FOnOutput;           // Passes all output of the debugged target
    property OnState: TDebuggerStateChangedEvent read FOnState write FOnState;                 // Fires when the current state of the debugger changes
    property State: TDBGState read FState;                                       // The current state of the debugger
    property SupportedCommands: TDBGCommands read GetSupportedCommands;          // All available commands of the debugger
    property Watches: TDBGWatches read FWatches;                                 // list of all watches localvars etc
  end;
  
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
    'Modify'
    );
    
  DBGStateNames: array[TDBGState] of string = (
    'None',
    'Idle',
    'Stop',
    'Pause',
    'Run',
    'Error'
    );
    
  DBGBreakPointActionNames: array[TDBGBreakPointAction] of string = (
    'Stop',
    'EnableGroup',
    'DisableGroup'
    );
    
function DBGCommandNameToCommand(const s: string): TDBGCommand;
function DBGStateNameToState(const s: string): TDBGState;
function DBGBreakPointActionNameToAction(const s: string): TDBGBreakPointAction;

implementation

const
  COMMANDMAP: array[TDBGState] of TDBGCommands = (
  {dsNone } [],
  {dsIdle } [],
  {dsStop } [dcRun, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak, dcWatch,
             dcEvaluate],
  {dsPause} [dcRun, dcStop, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak,
             dcWatch, dcLocal, dcEvaluate, dcModify],
  {dsRun  } [dcPause, dcStop, dcBreak, dcWatch],
  {dsError} [dcStop]
  );

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

function DBGBreakPointActionNameToAction(const s: string): TDBGBreakPointAction;
begin
  for Result:=Low(TDBGBreakPointAction) to High(TDBGBreakPointAction) do
    if AnsiCompareText(s,DBGBreakPointActionNames[Result])=0 then exit;
  Result:=bpaStop;
end;

{ =========================================================================== }
{ TDebugger }
{ =========================================================================== }

function TDebugger.ChangeFileName: Boolean;
begin
  Result := True;
end;

constructor TDebugger.Create(const AExternalDebugger: String);
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
  FBreakPoints := CreateBreakPoints;
  FLocals := CreateLocals;
  FCallStack := CreateCallStack;
  FWatches := CreateWatches;
  FBreakPointGroups := TDBGBreakPointGroups.Create;
  FExitCode := 0;
  FEnvironment:=TStringList.Create;
end;

function TDebugger.CreateBreakPoints: TDBGBreakPoints;
begin
  Result := TDBGBreakPoints.Create(Self, TDBGBreakPoint);
end;

function TDebugger.CreateCallStack: TDBGCallStack; 
begin
  Result := TDBGCallStack.Create(Self);
end;

function TDebugger.CreateLocals: TDBGLocals;
begin
  Result := TDBGLocals.Create(Self);
end;

function TDebugger.CreateWatches: TDBGWatches;
begin
  Result := TDBGWatches.Create(Self, TDBGWatch);
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

  FreeAndNil(FBreakPoints);
  FreeAndNil(FBreakPointGroups);
  FreeAndNil(FLocals);     
  FreeAndNil(FCallStack);
  FreeAndNil(FWatches);
  FreeAndNil(FEnvironment);
  inherited;
end;

procedure TDebugger.Done;
begin
  SetState(dsNone);
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

procedure TDebugger.DoException(const AExceptionID: Integer;
  const AExceptionText: String);
begin
  if Assigned(FOnException) then
    FOnException(Self, AExceptionID, AExceptionText);
end;

procedure TDebugger.DoOutput(const AText: String);
begin
  if Assigned(FOnOutput) then FOnOutput(Self, AText);
end;

procedure TDebugger.DoState(const OldState: TDBGState);
begin
  if Assigned(FOnState) then FOnState(Self,OldState);
end;

function TDebugger.Evaluate(const AExpression: String;
  var AResult: String): Boolean;
begin
  Result := ReqCmd(dcEvaluate, [AExpression, @AResult]);
end;

function TDebugger.GetCommands: TDBGCommands;
begin
  Result := COMMANDMAP[State] * GetSupportedCommands;
end;

function TDebugger.GetState: TDBGState;
begin
  Result := FState;
end;

function TDebugger.GetSupportedCommands: TDBGCommands;
begin
  Result := [];
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

function TDebugger.TargetIsStarted: boolean;
begin
  Result:=FState in [dsRun,dsPause];
end;

procedure TDebugger.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; const OnLoadFilename: TOnLoadFilenameFromConfig);
begin
  Arguments:=XMLConfig.GetValue(Path+'Arguments/Value','');
  BreakPointGroups.LoadFromXMLConfig(XMLConfig,Path+XMLBreakPointGroupsNode+'/');
  BreakPoints.LoadFromXMLConfig(XMLConfig,Path+XMLBreakPointsNode+'/',
                               OnLoadFilename,@BreakPointGroups.GetGroupByName);
  Watches.LoadFromXMLConfig(XMLConfig,Path+XMLWatchesNode+'/');
  // the environment is controlled by the run parameters, so don't load it
  Environment.Clear;
end;

procedure TDebugger.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  const OnSaveFilename: TOnSaveFilenameToConfig);
begin
  XMLConfig.SetDeleteValue(Path+'Arguments/Value',Arguments,'');
  BreakPointGroups.SaveToXMLConfig(XMLConfig,Path+XMLBreakPointGroupsNode+'/');
  BreakPoints.SaveToXMLConfig(XMLConfig,Path+XMLBreakPointsNode+'/',
                              OnSaveFilename);
  Watches.SaveToXMLConfig(XMLConfig,Path+XMLWatchesNode+'/');
  // the environment is controlled by the run parameters, so don't save it
end;

procedure TDebugger.Pause;
begin
  ReqCmd(dcPause, []);
end;

function TDebugger.ReqCmd(const ACommand: TDBGCommand;
  const AParams: array of const): Boolean;
begin
  if FState = dsNone then Init;
  if ACommand in Commands then begin
    if (not TargetIsStarted) and (ACommand in dcRunCommands) then
      InitTargetStart;
    Result := RequestCommand(ACommand, AParams);
  end
  else Result := False;
end;

procedure TDebugger.SetEnvironment(const AValue: TStrings);
begin
  FEnvironment.Assign(AValue);
end;

procedure TDebugger.Run;
begin
  ReqCmd(dcRun, []);
end;

procedure TDebugger.RunTo(const ASource: String; const ALine: Integer);
begin
  ReqCmd(dcRunTo, [ASource, ALine]);
end;

procedure TDebugger.SetExitCode(const AValue: Integer);
begin
  FExitCode := AValue;
end;

procedure TDebugger.SetFileName(const AValue: String);
begin
  if FFileName <> AValue
  then begin
    WriteLN('[TDebugger.SetFileName] ', AValue);
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
    end;

    FFileName := AValue;
    if (FFilename <> '') and (FState = dsIdle) and ChangeFileName
    then SetState(dsStop);
  end;
end;

procedure TDebugger.SetState(const AValue: TDBGState);
var
  OldState: TDBGState;
begin
  if AValue <> FState
  then begin
    OldState := FState;
    FState := AValue;
    FBreakpoints.DoStateChange;
    FLocals.DoStateChange;
    FCallStack.DoStateChange;
    FWatches.DoStateChange;
    DoState(OldState);
  end;
end;

procedure TDebugger.InitTargetStart;
begin
  FBreakPoints.InitTargetStart;
  FBreakPointGroups.InitTargetStart;
  FWatches.InitTargetStart;
end;

procedure TDebugger.StepInto;
begin
  ReqCmd(dcStepInto, []);
end;

procedure TDebugger.StepOver;
begin
  ReqCmd(dcStepOver, []);
end;

procedure TDebugger.Stop;
begin
  ReqCmd(dcStop, []);
end;

{ =========================================================================== }
{ TDBGBreakPoint }
{ =========================================================================== }

procedure TDBGBreakPoint.AddDisableGroup(const AGroup: TDBGBreakPointGroup);
begin
  if AGroup = nil then Exit;
  FDisableGroupList.Add(AGroup);
  AGroup.AddReference(Self);
  Changed(False);
end;

procedure TDBGBreakPoint.AddEnableGroup(const AGroup: TDBGBreakPointGroup);
begin
  if AGroup = nil then Exit;
  FEnableGroupList.Add(AGroup);
  AGroup.AddReference(Self);
  Changed(False);
end;

procedure TDBGBreakPoint.AssignTo(Dest: TPersistent);
var
  DestBreakPoint: TDBGBreakPoint;
begin
  if Dest is TDBGBreakPoint
  then begin
    DestBreakPoint:=TDBGBreakPoint(Dest);
    writeln('TDBGBreakPoint.AssignTo Src=',ClassName,' Dest=',Dest.ClassName,' File="',FSource,'" Line=',FLine);
    DestBreakPoint.SetLocation(FSource, FLine);
    DestBreakPoint.SetExpression(FExpression);
    DestBreakPoint.SetActions(FActions);
    DestBreakPoint.SetInitialEnabled(FInitialEnabled);
    DestBreakPoint.SetEnabled(FEnabled);
  end
  else inherited;
end;

constructor TDBGBreakPoint.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FSource := '';
  FLine := -1;
  FValid := vsUnknown;
  FEnabled := False;
  FInitialEnabled:=false;
  FHitCount := 0;
  FExpression := '';
  FGroup := nil;
  FFirstRun := True;
  FActions := [bpaStop];
  FDisableGroupList := TList.Create;
  FEnableGroupList := TList.Create;
end;

destructor TDBGBreakPoint.Destroy;
begin
  if (TDBGBreakPoints(Collection) <> nil)
  then TDBGBreakPoints(Collection).NotifyRemove(Self);

  if FGroup <> nil
  then FGroup.Remove(Self);

  ClearAllGroupLists;

  inherited;
  FreeAndNil(FDisableGroupList);
  FreeAndNil(FEnableGroupList);
end;

procedure TDBGBreakPoint.DisableGroups;
var
  n: Integer;
begin
  for n := 0 to FDisableGroupList.Count - 1 do
    TDBGBreakPointGroup(FDisableGroupList[n]).Enabled := False;
end;

procedure TDBGBreakPoint.DoActionChange;
begin
  Changed(False);
end;

procedure TDBGBreakPoint.DoEnableChange;
begin
  Changed(False);
end;

procedure TDBGBreakPoint.DoExpressionChange;
begin
  Changed(False);
end;

procedure TDBGBreakPoint.DoStateChange;
begin
  case Debugger.State of
    dsStop, dsIdle: begin
      FFirstRun := True;
    end;
    dsRun: begin
      if FFirstRun
      then begin
        FHitCount := 0;
        FFirstRun := False;
      end;
    end;
  end;
end;

procedure TDBGBreakPoint.EnableGroups;
var
  n: Integer;
begin
  for n := 0 to FDisableGroupList.Count - 1 do
    TDBGBreakPointGroup(FDisableGroupList[n]).Enabled := True;
end;

function  TDBGBreakPoint.GetDebugger: TDebugger;
begin
  Result := TDBGBreakPoints(Collection).FDebugger;
end;

procedure TDBGBreakPoint.RemoveDisableGroup(const AGroup: TDBGBreakPointGroup);
begin
  RemoveFromGroupList(AGroup,FDisableGroupList);
end;

procedure TDBGBreakPoint.RemoveEnableGroup(const AGroup: TDBGBreakPointGroup);
begin
  RemoveFromGroupList(AGroup,FEnableGroupList);
end;

procedure TDBGBreakPoint.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; const OnLoadFilename: TOnLoadFilenameFromConfig;
  const OnGetGroup: TOnGetGroupByName);

  procedure LoadGroupList(GroupList: TList; const ListPath: string);
  var
    i: Integer;
    CurGroup: TDBGBreakPointGroup;
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
  NewActions: TDBGBreakPointActions;
  CurAction: TDBGBreakPointAction;
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
    for CurAction:=Low(TDBGBreakPointAction) to High(TDBGBreakPointAction) do
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

procedure TDBGBreakPoint.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; const OnSaveFilename: TOnSaveFilenameToConfig);
  
  procedure SaveGroupList(GroupList: TList; const ListPath: string);
  var
    i: Integer;
    CurGroup: TDBGBreakPointGroup;
  begin
    XMLConfig.SetDeleteValue(ListPath+'Count',GroupList.Count,0);
    for i:=0 to GroupList.Count-1 do begin
      CurGroup:=TDBGBreakPointGroup(GroupList[i]);
      XMLConfig.SetDeleteValue(ListPath+'Group'+IntToStr(i+1)+'/Name',
        CurGroup.Name,'');
    end;
  end;
  
var
  Filename: String;
  CurAction: TDBGBreakPointAction;
begin
  if Group<>nil then
    XMLConfig.SetDeleteValue(Path+'Group/Name',Group.Name,'');
  XMLConfig.SetDeleteValue(Path+'Expression/Value',Expression,'');
  Filename:=Source;
  if Assigned(OnSaveFilename) then OnSaveFilename(Filename);
  XMLConfig.SetDeleteValue(Path+'Source/Value',Filename,'');
  XMLConfig.SetDeleteValue(Path+'InitialEnabled/Value',InitialEnabled,true);
  XMLConfig.SetDeleteValue(Path+'Line/Value',Line,-1);
  for CurAction:=Low(TDBGBreakPointAction) to High(TDBGBreakPointAction) do
    XMLConfig.SetDeleteValue(
        Path+'Actions/'+DBGBreakPointActionNames[CurAction],
        CurAction in Actions,CurAction in [bpaStop]);
  SaveGroupList(FDisableGroupList,Path+'DisableGroups/');
  SaveGroupList(FEnableGroupList,Path+'EnableGroups/');
end;

procedure TDBGBreakPoint.SetActions(const AValue: TDBGBreakPointActions);
begin
  if FActions <> AValue
  then begin
    FActions := AValue;
    DoActionChange;
    Changed(False);
  end;
end;

procedure TDBGBreakPoint.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue
  then begin
    FEnabled := AValue;
    DoEnableChange;
  end;
end;

procedure TDBGBreakPoint.SetExpression(const AValue: String);
begin
  if FExpression <> AValue
  then begin
    FExpression := AValue;
    DoExpressionChange;
  end;
end;

procedure TDBGBreakPoint.SetGroup(const AValue: TDBGBreakPointGroup);
var
  Grp: TDBGBreakPointGroup;
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
    Changed(False);
  end;
end;

procedure TDBGBreakPoint.SetInitialEnabled(const AValue: Boolean);
begin
  if FInitialEnabled=AValue then exit;
  FInitialEnabled:=AValue;
end;

procedure TDBGBreakPoint.SetHitCount(const AValue: Integer);
begin
  if FHitCount <> AValue
  then begin
    FHitCount := AValue;
    Changed(False);
  end;
end;

procedure TDBGBreakPoint.SetLocation(const ASource: String;
  const ALine: Integer);
begin
  if (FSource = ASource) and (FLine = ALine) then exit;
  FSource := ASource;
  FLine := ALine;
  Changed(False);
end;

procedure TDBGBreakPoint.SetValid(const AValue: TValidState);
begin
  if FValid <> AValue
  then begin
    FValid := AValue;
    Changed(False);
  end;
end;

procedure TDBGBreakPoint.RemoveFromGroupList(const AGroup: TDBGBreakPointGroup;
  const AGroupList: TList);
begin
  if (AGroup = nil) then Exit;
  AGroupList.Remove(AGroup);
  AGroup.RemoveReference(Self);
end;

procedure TDBGBreakPoint.ClearGroupList(const AGroupList: TList);
var
  i: Integer;
  AGroup: TDBGBreakPointGroup;
begin
  for i:=0 to AGroupList.Count-1 do begin
    AGroup:=TDBGBreakPointGroup(AGroupList[i]);
    AGroup.RemoveReference(Self);
  end;
  AGroupList.Clear;
end;

procedure TDBGBreakPoint.CopyGroupList(SrcGroupList, DestGroupList: TList;
  DestGroups: TDBGBreakPointGroups);
var
  i: Integer;
  CurGroup: TDBGBreakPointGroup;
  NewGroup: TDBGBreakPointGroup;
begin
  ClearGroupList(DestGroupList);
  for i:=0 to SrcGroupList.Count-1 do begin
    CurGroup:=TDBGBreakPointGroup(SrcGroupList[i]);
    NewGroup:=DestGroups.GetGroupByName(CurGroup.Name);
    DestGroupList.Add(NewGroup);
  end;
end;

procedure TDBGBreakPoint.CopyAllGroupLists(SrcBreakPoint: TDBGBreakPoint;
  DestGroups: TDBGBreakPointGroups);
begin
  CopyGroupList(SrcBreakPoint.FEnableGroupList,FEnableGroupList,DestGroups);
  CopyGroupList(SrcBreakPoint.FDisableGroupList,FDisableGroupList,DestGroups);
end;

procedure TDBGBreakPoint.ClearAllGroupLists;
begin
  ClearGroupList(FDisableGroupList);
  ClearGroupList(FEnableGroupList);
end;

{ =========================================================================== }
{ TDBGBreakPoints }
{ =========================================================================== }

function TDBGBreakPoints.Add(const ASource: String;
  const ALine: Integer): TDBGBreakPoint;
begin
  Result := TDBGBreakPoint(inherited Add);
  writeln('TDBGBreakPoints.Add ',Result.ClassName,' ',ASource,' ',ALine);
  Result.SetLocation(ASource, ALine);
  NotifyAdd(Result);
end;

procedure TDBGBreakPoints.AddNotification(
  const ANotification: TDBGBreakPointsNotification);
begin
  FNotificationList.Add(ANotification);
  ANotification.AddReference;
end;

constructor TDBGBreakPoints.Create(const ADebugger: TDebugger;
  const ABreakPointClass: TDBGBreakPointClass);
begin
  FDebugger := ADebugger;
  FNotificationList := TList.Create;
  inherited Create(ABreakPointClass);
end;

destructor TDBGBreakPoints.Destroy;
var
  n: Integer;
begin
  for n := FNotificationList.Count - 1 downto 0 do
    TDebuggerNotification(FNotificationList[n]).ReleaseReference;

  inherited;

  FreeAndNil(FNotificationList);
end;

procedure TDBGBreakPoints.DoStateChange;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
    GetItem(n).DoStateChange;
end;

function TDBGBreakPoints.Find(const ASource: String;
  const ALine: Integer): TDBGBreakPoint;
begin
  Result := FindBreakPoint(ASource,ALine,nil);
end;

function TDBGBreakPoints.FindBreakPoint(const ASource: String;
  const ALine: Integer; Ignore: TDBGBreakPoint): TDBGBreakPoint;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
  begin
    Result := GetItem(n);
    if  (Result.Line = ALine)
    and (CompareFilenames(Result.Source,ASource)=0)
    and (Ignore<>Result)
    then exit;
  end;
  Result := nil;
end;

function TDBGBreakPoints.GetItem(const AnIndex: Integer): TDBGBreakPoint;
begin
  Result := TDBGBreakPoint(inherited GetItem(AnIndex));
end;

procedure TDBGBreakPoints.NotifyAdd(const ABreakPoint: TDBGBreakPoint);
var
  n: Integer;
  Notification: TDBGBreakPointsNotification;
begin
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TDBGBreakPointsNotification(FNotificationList[n]);
    if Assigned(Notification.FOnAdd)
    then Notification.FOnAdd(Self, ABreakPoint);
  end;
end;

procedure TDBGBreakPoints.NotifyRemove(const ABreakpoint: TDBGBreakPoint);
var
  n: Integer;
  Notification: TDBGBreakPointsNotification;
begin
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TDBGBreakPointsNotification(FNotificationList[n]);
    if Assigned(Notification.FOnRemove)
    then Notification.FOnRemove(Self, ABreakpoint);
  end;
end;

procedure TDBGBreakPoints.RemoveNotification(
  const ANotification: TDBGBreakPointsNotification);
begin
  FNotificationList.Remove(ANotification);
  ANotification.ReleaseReference;
end;

procedure TDBGBreakPoints.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; const OnLoadFilename: TOnLoadFilenameFromConfig;
  const OnGetGroup: TOnGetGroupByName);
var
  NewCount: Integer;
  i: Integer;
  NewBreakPoint: TDBGBreakPoint;
  OldBreakPoint: TDBGBreakPoint;
begin
  Clear;
  NewCount:=XMLConfig.GetValue(Path+'Count',0);
  writeln('TDBGBreakPoints.LoadFromXMLConfig NewCount=',NewCount);
  for i:=0 to NewCount-1 do begin
    NewBreakPoint:=TDBGBreakPoint(inherited Add);
    NewBreakPoint.LoadFromXMLConfig(XMLConfig,
      Path+'Item'+IntToStr(i+1)+'/',OnLoadFilename,OnGetGroup);
    OldBreakPoint:=FindBreakPoint(NewBreakPoint.Source,NewBreakPoint.Line,
                                  NewBreakPoint);
    writeln('TDBGBreakPoints.LoadFromXMLConfig i=',i,' ',
      NewBreakPoint.InitialEnabled,' ',NewBreakPoint.Source,' ',NewBreakPoint.Line,
      ' OldBreakPoint=',OldBreakPoint<>nil);
    if OldBreakPoint <> nil
    then NewBreakPoint.Free
    else NotifyAdd(NewBreakPoint);
  end;
end;

procedure TDBGBreakPoints.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; const OnSaveFilename: TOnSaveFilenameToConfig);
var
  Cnt: Integer;
  i: Integer;
  CurBreakPoint: TDBGBreakPoint;
begin
  Cnt:=Count;
  XMLConfig.SetDeleteValue(Path+'Count',Cnt,0);
  for i:=0 to Cnt-1 do begin
    CurBreakPoint:=Items[i];
    CurBreakPoint.SaveToXMLConfig(XMLConfig,
      Path+'Item'+IntToStr(i+1)+'/',OnSaveFilename);
  end;
end;

procedure TDBGBreakPoints.InitTargetStart;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    Items[i].Enabled:=Items[i].InitialEnabled;
end;

procedure TDBGBreakPoints.SetItem(const AnIndex: Integer;
  const AValue: TDBGBreakPoint);
begin
  inherited SetItem(AnIndex, AValue);
end;

procedure TDBGBreakPoints.Update(Item: TCollectionItem);
var
  n: Integer;
  Notification: TDBGBreakPointsNotification;
begin
  // Note: Item will be nil in case all items need to be updated
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TDBGBreakPointsNotification(FNotificationList[n]);
    if Assigned(Notification.FOnUpdate)
    then Notification.FOnUpdate(Self, TDBGBreakPoint(Item));
  end;
end;

{ =========================================================================== }
{ TDBGBreakPointGroup }
{ =========================================================================== }

function TDBGBreakPointGroup.Add(const ABreakPoint: TDBGBreakPoint): Integer;
begin
  Result := FBreakpoints.IndexOf(ABreakPoint); //avoid dups
  if Result = -1
  then begin
    Result := FBreakpoints.Add(ABreakPoint);
    ABreakpoint.Group := Self;
  end;
end;

procedure TDBGBreakPointGroup.AddReference(const ABreakPoint: TDBGBreakPoint);
begin
  FReferences.Add(ABreakPoint);
end;

function TDBGBreakPointGroup.Count: Integer;
begin
  Result := FBreakpoints.Count;
end;

constructor TDBGBreakPointGroup.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FBreakpoints := TList.Create;
  FReferences := TList.Create;
  FEnabled := True;
end;

procedure TDBGBreakPointGroup.Delete(const AIndex: Integer);
begin
  Remove(TDBGBreakPoint(FBreakPoints[AIndex]));
end;

destructor TDBGBreakPointGroup.Destroy;
var
  n: Integer;
begin
  for n := FBreakpoints.Count - 1 downto 0 do
    TDBGBreakPoint(FBreakpoints[n]).Group := nil;
  for n := FReferences.Count - 1 downto 0 do
    TDBGBreakPoint(FReferences[n]).RemoveDisableGroup(Self);
  for n := FReferences.Count - 1 downto 0 do
    TDBGBreakPoint(FReferences[n]).RemoveEnableGroup(Self);

  inherited Destroy;
  FreeAndNil(FBreakpoints);
  FreeAndNil(FReferences);
end;

function TDBGBreakPointGroup.GetBreakpoint(const AIndex: Integer): TDBGBreakPoint;
begin
  Result := TDBGBreakPoint(FBreakPoints[AIndex]);
end;

function TDBGBreakPointGroup.Remove(const ABreakPoint: TDBGBreakPoint): Integer;
begin
  Result := FBreakpoints.Remove(ABreakPoint);
  if ABreakpoint.Group = Self
  then ABreakpoint.Group := nil;
end;

procedure TDBGBreakPointGroup.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  Name:=XMLConfig.GetValue(Path+'Name/Value','');
  // the breakpoints of this group are not loaded here.
  // They are loaded by the TDBGBreakPoints object.
  InitialEnabled:=XMLConfig.GetValue(Path+'InitialEnabled/Value',true);
  FEnabled:=InitialEnabled;
end;

procedure TDBGBreakPointGroup.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'Name/Value',Name,'');
  // the breakpoints of this group are not saved here.
  // They are saved by the TDBGBreakPoints object.
  XMLConfig.SetDeleteValue(Path+'InitialEnabled/Value',InitialEnabled,true);
end;

procedure TDBGBreakPointGroup.RemoveReference(const ABreakPoint: TDBGBreakPoint);
begin
  FReferences.Remove(ABreakPoint);
end;

procedure TDBGBreakPointGroup.SetEnabled(const AValue: Boolean);
var
  n: Integer;
begin
  if FEnabled <> AValue
  then begin
    FEnabled := AValue;
    for n := 0 to FBreakPoints.Count - 1 do
      TDBGBreakpoint(FBreakPoints[n]).Enabled := FEnabled;
  end;
end;

procedure TDBGBreakPointGroup.SetInitialEnabled(const AValue: Boolean);
begin
  if FInitialEnabled=AValue then exit;
  FInitialEnabled:=AValue;
end;

procedure TDBGBreakPointGroup.SetName(const AValue: String);
begin
  FName := AValue;
end;

procedure TDBGBreakPointGroup.AssignTo(Dest: TPersistent);
var
  DestGroup: TDBGBreakPointGroup;
begin
  if Dest is TDBGBreakPointGroup then begin
    DestGroup:=TDBGBreakPointGroup(Dest);
    DestGroup.Name:=Name;
    DestGroup.InitialEnabled:=InitialEnabled;
    DestGroup.Enabled:=Enabled;
  end else
    inherited AssignTo(Dest);
end;

{ =========================================================================== }
{ TDBGBreakPointGroups }
{ =========================================================================== }

constructor TDBGBreakPointGroups.Create;
begin
  inherited Create(TDBGBreakPointGroup);
end;

procedure TDBGBreakPointGroups.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  NewCount: integer;
  NewGroup: TDBGBreakPointGroup;
  i: Integer;
  OldGroup: TDBGBreakPointGroup;
begin
  Clear;
  NewCount:=XMLConfig.GetValue(Path+'Count',0);
  writeln('TDBGBreakPointGroups.LoadFromXMLConfig Count=',NewCount);
  for i:=0 to NewCount-1 do begin
    NewGroup:=TDBGBreakPointGroup(inherited Add);
    NewGroup.LoadFromXMLConfig(XMLConfig,
                               Path+'Item'+IntToStr(i+1)+'/');
    OldGroup:=FindGroupByName(NewGroup.Name,NewGroup);
    writeln('TDBGBreakPointGroups.LoadFromXMLConfig i=',i,' ',NewGroup.Name,' OldGroup=',OldGroup<>nil);
    if OldGroup<>nil then
      NewGroup.Free;
  end;
end;

procedure TDBGBreakPointGroups.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  Cnt: Integer;
  CurGroup: TDBGBreakPointGroup;
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

function TDBGBreakPointGroups.GetGroupByName(const GroupName: string
  ): TDBGBreakPointGroup;
begin
  Result:=FindGroupByName(GroupName,nil);
end;

function TDBGBreakPointGroups.FindGroupByName(const GroupName: string;
  Ignore: TDBGBreakPointGroup): TDBGBreakPointGroup;
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

function TDBGBreakPointGroups.IndexOfGroupWithName(const GroupName: string;
  Ignore : TDBGBreakPointGroup): integer;
begin
  Result:=Count-1;
  while (Result>=0)
  and ((AnsiCompareText(Items[Result].Name,GroupName)<>0)
    or (Items[Result]=Ignore))
  do
    dec(Result);
end;

procedure TDBGBreakPointGroups.InitTargetStart;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    Items[i].Enabled:=Items[i].InitialEnabled;
end;

procedure TDBGBreakPointGroups.Regroup(SrcGroups: TDBGBreakPointGroups;
  SrcBreakPoints, DestBreakPoints: TDBGBreakPoints);
var
  BreakPointCnt: Integer;
  i: Integer;
  SrcBreakPoint: TDBGBreakPoint;
  DestBreakPoint: TDBGBreakPoint;
begin
  // copy the groups
  Assign(SrcGroups);
  // copy the groups of the SrcBreakPoints to the DestBreakPoints by using
  // the new groups
  BreakPointCnt:=SrcBreakPoints.Count;
  if BreakPointCnt<>DestBreakPoints.Count then
    RaiseException('TDBGBreakPointGroups.Regroup Src<>Dest breakpoints');
  for i:=0 to BreakPointCnt-1 do begin
    SrcBreakPoint:=SrcBreakPoints[i];
    DestBreakPoint:=DestBreakPoints[i];
    // copy group of breakpoint
    if SrcBreakPoint.Group<>nil then
      DestBreakPoint.Group:=GetGroupByName(SrcBreakPoint.Group.Name)
    else
      DestBreakPoint.Group:=nil;
    // copy group lists of breakpoint
    DestBreakPoint.CopyAllGroupLists(SrcBreakPoint,Self);
  end;
end;

function TDBGBreakPointGroups.GetItem(const AnIndex: Integer
  ): TDBGBreakPointGroup;
begin
  Result := TDBGBreakPointGroup(inherited GetItem(AnIndex));
end;

procedure TDBGBreakPointGroups.SetItem(const AnIndex: Integer;
  const AValue: TDBGBreakPointGroup);
begin
  inherited SetItem(AnIndex, AValue);
end;

{ =========================================================================== }
{ TDBGWatch }
{ =========================================================================== }

procedure TDBGWatch.AssignTo(Dest: TPersistent);
begin
  if Dest is TDBGWatch
  then begin
    TDBGWatch(Dest).SetExpression(FExpression);
    TDBGWatch(Dest).SetEnabled(FEnabled);
  end
  else inherited;
end;

constructor TDBGWatch.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FEnabled := False;
end;

procedure TDBGWatch.LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string
  );
begin
  Expression:=XMLConfig.GetValue(Path+'Expression/Value','');
  InitialEnabled:=XMLConfig.GetValue(Path+'InitialEnabled/Value',true);
  FEnabled:=FInitialEnabled;
end;

procedure TDBGWatch.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'Expression/Value',Expression,'');
  XMLConfig.SetDeleteValue(Path+'InitialEnabled/Value',InitialEnabled,true);
end;

procedure TDBGWatch.DoEnableChange;
begin
  Changed(False);
end;

procedure TDBGWatch.DoExpressionChange;
begin
  Changed(False);
end;

procedure TDBGWatch.DoStateChange;
begin    
end;

function TDBGWatch.GetDebugger: TDebugger;
begin
  Result := TDBGWatches(Collection).FDebugger;
end;

function TDBGWatch.GetValid: TValidState;
begin
  Result := vsUnknown;
end;

function TDBGWatch.GetValue: String;
begin       
  if not Enabled
  then Result := '<disabled>'
  else
    case Valid of
    vsValid:   Result := '<valid>';
    vsInvalid: Result := '<invalid>';
    else
    {vsUnknown:}Result := '<unknown>';
    end;
end;

procedure TDBGWatch.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue
  then begin
    FEnabled := AValue;
    DoEnableChange;
  end;
end;

procedure TDBGWatch.SetExpression(const AValue: String);
begin
  if AValue <> FExpression
  then begin
    FExpression := AValue;
    DoExpressionChange;
  end;
end;

procedure TDBGWatch.SetInitialEnabled(const AValue: Boolean);
begin
  if FInitialEnabled=AValue then exit;
  FInitialEnabled:=AValue;
end;

procedure TDBGWatch.SetValid(const AValue: TValidState);
begin
  if FValid <> AValue
  then begin
    FValid := AValue;
    Changed(False);
  end;
end;

{ =========================================================================== }
{ TDBGWatches }
{ =========================================================================== }

function TDBGWatches.Add(const AExpression: String): TDBGWatch;
var
  n: Integer;
  Notification: TDBGWatchesNotification;
begin
  Result := Find(AExpression);
  if Result <> nil then Exit;

  Result := TDBGWatch(inherited Add);
  Result.Expression := AExpression;
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TDBGWatchesNotification(FNotificationList[n]);
    if Assigned(Notification.FOnAdd)
    then Notification.FOnAdd(Self, Result);
  end;
end;

procedure TDBGWatches.AddNotification(
  const ANotification: TDBGWatchesNotification);
begin
  FNotificationList.Add(ANotification);
  ANotification.AddReference;
end;

constructor TDBGWatches.Create(const ADebugger: TDebugger;
  const AWatchClass: TDBGWatchClass);
begin
  FDebugger := ADebugger;
  FNotificationList := TList.Create;
  inherited Create(AWatchClass);
end;

destructor TDBGWatches.Destroy;
var
  n: Integer;
begin
  for n := FNotificationList.Count - 1 downto 0 do
    TDebuggerNotification(FNotificationList[n]).ReleaseReference;

  inherited;

  FreeAndNil(FNotificationList);
end;

procedure TDBGWatches.DoStateChange;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
    GetItem(n).DoStateChange;
end;

function TDBGWatches.Find(const AExpression: String): TDBGWatch;
var
  n: Integer;
  S: String;
begin
  S := UpperCase(AExpression);
  for n := 0 to Count - 1 do
  begin
    Result := GetItem(n);
    if UpperCase(Result.Expression) = S
    then Exit;
  end;
  Result := nil;
end;

function TDBGWatches.GetItem(const AnIndex: Integer): TDBGWatch;
begin
  Result := TDBGWatch(inherited GetItem(AnIndex));
end;

procedure TDBGWatches.Removed(const AWatch: TDBGWatch);
var
  n: Integer;
  Notification: TDBGWatchesNotification;
begin
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TDBGWatchesNotification(FNotificationList[n]);
    if Assigned(Notification.FOnRemove)
    then Notification.FOnRemove(Self, AWatch);
  end;
end;

procedure TDBGWatches.RemoveNotification(
  const ANotification: TDBGWatchesNotification);
begin
  FNotificationList.Remove(ANotification);
  ANotification.ReleaseReference;
end;

procedure TDBGWatches.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  NewCount: Integer;
  i: Integer;
  NewWatch: TDBGWatch;
begin
  Clear;
  NewCount:=XMLConfig.GetValue(Path+'Count',0);
  writeln('TDBGWatches.LoadFromXMLConfig Count=',NewCount);
  for i:=0 to NewCount-1 do begin
    NewWatch:=TDBGWatch(inherited Add);
    NewWatch.LoadFromXMLConfig(XMLConfig,Path+'Item'+IntToStr(i+1)+'/');
    writeln('TDBGWatches.LoadFromXMLConfig i=',i,' ',NewWatch.Expression);
  end;
end;

procedure TDBGWatches.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string
  );
var
  Cnt: Integer;
  i: Integer;
  CutWatch: TDBGWatch;
begin
  Cnt:=Count;
  XMLConfig.SetDeleteValue(Path+'Count',Cnt,0);
  for i:=0 to Cnt-1 do begin
    CutWatch:=Items[i];
    CutWatch.SaveToXMLConfig(XMLConfig,Path+'Item'+IntToStr(i+1)+'/');
  end;
end;

procedure TDBGWatches.InitTargetStart;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    Items[i].Enabled:=Items[i].InitialEnabled;
end;

procedure TDBGWatches.SetItem(const AnIndex: Integer; const AValue: TDBGWatch);
begin
  inherited SetItem(AnIndex, AValue);
end;

procedure TDBGWatches.Update(Item: TCollectionItem);
var
  n: Integer;
  Notification: TDBGWatchesNotification;
begin
  // Note: Item will be nil in case all items need to be updated
  for n := 0 to FNotificationList.Count - 1 do
  begin
    Notification := TDBGWatchesNotification(FNotificationList[n]);
    if Assigned(Notification.FOnUpdate)
    then Notification.FOnUpdate(Self, TDBGWatch(Item));
  end;
end;

{ =========================================================================== }
{ TDBGLocals }
{ =========================================================================== }

function TDBGLocals.Count: Integer;
begin
  Result := 0;
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

procedure TDBGLocals.DoStateChange;
begin
end;

function TDBGLocals.GetName(const AnIndex: Integer): String;
begin
  Result := '';
end;

function TDBGLocals.GetValue(const AnIndex: Integer): String;
begin
  Result := '';
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

{ =========================================================================== }
{ TDBGCallStackEntry }
{ =========================================================================== }

constructor TDBGCallStackEntry.Create(const AIndex: Integer;
  const AnAdress: Pointer; const AnArguments: TStrings;
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

destructor TDBGCallStackEntry.Destroy;
begin
  inherited;
  FreeAndNil(FArguments);
end;

function TDBGCallStackEntry.GetArgumentCount: Integer; 
begin
  Result := FArguments.Count;
end;

function TDBGCallStackEntry.GetArgumentName(const AnIndex: Integer): String;
begin
  Result := FArguments.Names[AnIndex];
end;

function TDBGCallStackEntry.GetArgumentValue(const AnIndex: Integer): String;
begin                        
  Result := FArguments[AnIndex];
  Result := GetPart('=', '', Result);
end;

{ =========================================================================== }
{ TDBGCallStack }
{ =========================================================================== }

procedure TDBGCallStack.Clear;
var
  n:Integer;
begin
  for n := 0 to FEntries.Count - 1 do 
    TObject(FEntries[n]).Free;
    
  FEntries.Clear;  
end;

function TDBGCallStack.Count: Integer;
begin
  if  (FDebugger <> nil) 
  and (FDebugger.State = dsPause)
  then Result := GetCount
  else Result := 0;
end;

constructor TDBGCallStack.Create(const ADebugger: TDebugger);
begin
  FDebugger := ADebugger;
  FEntries := TList.Create;
  FOldState := FDebugger.State;
  inherited Create;
end;

function TDBGCallStack.CreateStackEntry(
  const AIndex: Integer): TDBGCallStackEntry;
begin
  Result := nil;
end;

destructor TDBGCallStack.Destroy;
begin
  Clear;
  inherited;
  FreeAndNil(FEntries);
end;

procedure TDBGCallStack.DoChange; 
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TDBGCallStack.DoStateChange; 
begin
  if FDebugger.State = dsPause
  then DoChange
  else begin
    if FOldState = dsPause
    then begin 
      Clear;
      DoChange;
    end;
  end;          
  FOldState := FDebugger.State;
end;

function TDBGCallStack.GetCount: Integer;
begin
  Result := 0;
end;

function TDBGCallStack.GetStackEntry(const AIndex: Integer): TDBGCallStackEntry;
var
  n: Integer;
begin
  if (AIndex < 0) 
  or (AIndex >= Count)
  then raise EInvalidOperation.CreateFmt('Index out of range (%d)', [AIndex]);
  
  for n := 0 to FEntries.Count - 1 do
  begin
    Result := TDBGCallStackEntry(FEntries[n]);
    if Result.FIndex = AIndex 
    then Exit;
  end;
  
  Result := CreateStackEntry(AIndex);
  if Result <> nil 
  then FEntries.Add(Result);
end;

end.
{ =============================================================================
  $Log$
  Revision 1.28  2003/05/27 08:01:31  marc
  MWE: + Added exception break
       * Reworked adding/removing breakpoints
       + Added Unknown breakpoint type

  Revision 1.27  2003/05/26 20:05:21  mattias
  made compiling gtk2 interface easier

  Revision 1.26  2003/05/26 11:08:20  mattias
  fixed double breakpoints

  Revision 1.25  2003/05/26 10:34:47  mattias
  implemented search, fixed double loading breakpoints

  Revision 1.24  2003/05/23 16:46:13  mattias
  added message, that debugger is readonly while running

  Revision 1.23  2003/05/23 14:12:51  mattias
  implemented restoring breakpoints

  Revision 1.22  2003/05/22 23:08:19  marc
  MWE: = Moved and renamed debuggerforms so that they can be
         modified by the ide
       + Added some parsing to evaluate complex expressions
         not understood by the debugger

  Revision 1.21  2003/05/22 17:06:49  mattias
  implemented InitialEnabled for breakpoints and watches

  Revision 1.20  2003/05/21 16:19:12  mattias
  implemented saving breakpoints and watches

  Revision 1.19  2003/05/21 08:09:04  mattias
  started loading/saving watches

  Revision 1.18  2003/05/20 21:41:07  mattias
  started loading/saving breakpoints

  Revision 1.17  2003/02/28 19:10:25  mattias
  added new ... dialog

  Revision 1.16  2002/08/28 10:44:44  lazarus
  MG: implemented run param environment variables

  Revision 1.15  2002/05/10 06:57:47  lazarus
  MG: updated licenses

  Revision 1.14  2002/04/30 15:57:39  lazarus
  MWE:
    + Added callstack object and dialog
    + Added checks to see if debugger = nil
    + Added dbgutils

  Revision 1.13  2002/04/24 20:42:29  lazarus
  MWE:
    + Added watches
    * Updated watches and watchproperty dialog to load as resource
    = renamed debugger resource files from *.lrc to *.lrs
    * Temporary fixed language problems on GDB (bug #508)
    * Made Debugmanager dialog handling more generic

  Revision 1.12  2002/03/25 22:38:29  lazarus
  MWE:
    + Added invalidBreakpoint image
    * Reorganized uniteditor so that breakpoints can be added erternal
    * moved breakpoints events to notification object

  Revision 1.11  2002/03/23 15:54:30  lazarus
  MWE:
    + Added locals dialog
    * Modified breakpoints dialog (load as resource)
    + Added generic debuggerdlg class
    = Reorganized main.pp, all debbugger relater routines are moved
      to include/ide_debugger.inc

  Revision 1.10  2002/03/12 23:55:36  lazarus
  MWE:
    * More delphi compatibility added/updated to TListView
    * Introduced TDebugger.locals
    * Moved breakpoints dialog to debugger dir
    * Changed breakpoints dialog to read from resource

  Revision 1.9  2002/03/09 02:03:59  lazarus
  MWE:
    * Upgraded gdb debugger to gdb/mi debugger
    * Set default value for autpopoup
    * Added Clear popup to debugger output window

  Revision 1.8  2002/02/20 23:33:24  lazarus
  MWE:
    + Published OnClick for TMenuItem
    + Published PopupMenu property for TEdit and TMemo (Doesn't work yet)
    * Fixed debugger running twice
    + Added Debugger output form
    * Enabled breakpoints

  Revision 1.7  2002/02/06 08:58:29  lazarus
  MG: fixed compiler warnings and asking to create non existing files

  Revision 1.6  2002/02/05 23:16:48  lazarus
  MWE: * Updated tebugger
       + Added debugger to IDE

  Revision 1.5  2001/11/12 19:28:23  lazarus
  MG: fixed create, virtual constructors makes no sense

  Revision 1.4  2001/11/06 23:59:13  lazarus
  MWE: + Initial breakpoint support
       + Added exeption handling on process.free

  Revision 1.3  2001/11/05 00:12:51  lazarus
  MWE: First steps of a debugger.

  Revision 1.2  2001/10/18 13:01:31  lazarus
  MG: fixed speedbuttons numglyphs>1 and started IDE debugging

  Revision 1.1  2001/02/28 22:09:15  lazarus
  MWE:
    * Renamed DBGDebugger to Debugger

  Revision 1.2  2001/02/25 16:44:57  lazarus
  MWE:
    + Added header and footer

}
