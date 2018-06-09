{
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

  Author: Mattias Gaertner

  Abstract:
    Running external programs and parsing their output lines.
}
unit ExtTools;

{$mode objfpc}{$H+}

{off $DEFINE VerboseExtToolErrors}
{off $DEFINE VerboseExtToolAddOutputLines}
{off $DEFINE VerboseExtToolThread}

interface

uses
  // RTL + FCL
  Classes, SysUtils, math, process, Pipes, Laz_AVL_Tree,
  // LCL
  LCLIntf, Forms, Dialogs,
  {$IFDEF VerboseExtToolThread}
  LCLProc,
  {$ENDIF}
  // CodeTools
  FileProcs,
  // LazUtils
  FileUtil, LazFileUtils, LazUtilities, UTF8Process, LazUTF8, AvgLvlTree,
  // IDEIntf
  IDEExternToolIntf, BaseIDEIntf, MacroIntf, IDEMsgIntf, IDEDialogs,
  PackageIntf, LazIDEIntf,
  // IDE
  IDECmdLine, TransferMacros, LazarusIDEStrConsts;

type
  TLMVToolState = (
    lmvtsRunning,
    lmvtsSuccess,
    lmvtsFailed
    );
  TLMVToolStates = set of TLMVToolState;

  { TLazExtToolView }

  TLazExtToolView = class(TExtToolView)
  private
    FAsyncQueued: boolean;
    FToolState: TLMVToolState;
  protected
    procedure SetToolState(AValue: TLMVToolState); virtual;
    procedure CallOnChangedInMainThread({%H-}Data: PtrInt); // (main thread)
    procedure QueueAsyncOnChanged; override; // (worker thread)
    procedure RemoveAsyncOnChanged; override; // (worker thread)
  public
    property ToolState: TLMVToolState read FToolState write SetToolState;
  end;

  { TLazExtToolConsoleView }

  TLazExtToolConsoleView = class(TLazExtToolView)
  protected
    fWrittenLineCount: integer;
    procedure ToolExited; override; // (main thread)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InputClosed; override; // (main thread)
    procedure ProcessNewMessages({%H-}AThread: TThread); override; // (worker thread, Tool is in Critical section)
    procedure OnNewOutput(Sender: TObject; {%H-}FirstNewMsgLine: integer); // (main thread)
  end;

  { TLazExtToolConsole }

  TLazExtToolConsole = class(TComponent)
  private
    FTerminating: boolean;
    fViews: TFPList; // list of TLazExtToolConsoleView
    function GetViews(Index: integer): TLazExtToolConsoleView;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function CreateView(Tool: TAbstractExternalTool): TLazExtToolConsoleView;
    function FindUnfinishedView: TLazExtToolConsoleView;
    property Views[Index: integer]: TLazExtToolConsoleView read GetViews;
    function Count: integer; inline;
    property Terminating: boolean read FTerminating write FTerminating;
  end;

var
  ExtToolConsole: TLazExtToolConsole = nil; // set by lazbuild

type
  TExternalTool = class;

  { TExternalToolThread }

  TExternalToolThread = class(TThread)
  private
    fLines: TStringList;
    FTool: TExternalTool;
    procedure SetTool(AValue: TExternalTool);
  public
    property Tool: TExternalTool read FTool write SetTool;
    procedure Execute; override;
    destructor Destroy; override;
  end;

  { TExternalTool }

  TExternalTool = class(TAbstractExternalTool)
  private
    FThread: TExternalToolThread;
    fExecuteAfter: TFPList; // list of TExternalTool
    fExecuteBefore: TFPList; // list of TExternalTool
    fNeedAfterSync: boolean;
    fOutputCountNotified: integer;
    procedure ProcessRunning; // (worker thread) after Process.Execute
    procedure ProcessStopped; // (worker thread) when process stopped
    procedure AddOutputLines(Lines: TStringList); // (worker thread) when new output arrived
    procedure NotifyHandlerStopped; // (main thread) called by ProcessStopped
    procedure NotifyHandlerNewOutput; // (main thread) called by AddOutputLines
    procedure SetThread(AValue: TExternalToolThread); // main or worker thread
    procedure SynchronizedImproveMessages; // (main thread) called by AddOutputLines
    procedure DoTerminate; // (main thread)
    procedure SyncAutoFree({%H-}aData: PtrInt); // (main thread)
  protected
    procedure DoExecute; override; // (main thread)
    procedure DoStart; // (main thread)
    procedure CreateView; // (main thread)
    function GetExecuteAfter(Index: integer): TAbstractExternalTool; override;
    function GetExecuteBefore(Index: integer): TAbstractExternalTool; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    function CanFree: boolean; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property Thread: TExternalToolThread read FThread write SetThread;
    procedure Execute; override;
    procedure Terminate; override;
    procedure WaitForExit; override;
    function ResolveMacros: boolean; override;

    function ExecuteAfterCount: integer; override;
    function ExecuteBeforeCount: integer; override;
    procedure RemoveExecuteBefore(Tool: TAbstractExternalTool); override;
    function IsExecutedBefore(Tool: TAbstractExternalTool): Boolean; override;
    procedure AddExecuteBefore(Tool: TAbstractExternalTool); override;
    function CanStart: boolean;
    function GetLongestEstimatedLoad: int64;
  end;

  { TExternalTools }

  TExternalTools = class(TIDEExternalTools)
  private
    FCritSec: TRTLCriticalSection;
    fRunning: TFPList; // list of TExternalTool, needs Enter/LeaveCriticalSection
    FMaxProcessCount: integer;
    fParsers: TFPList; // list of TExtToolParserClass
    function GetRunningTools(Index: integer): TExternalTool;
    procedure AddRunningTool(Tool: TExternalTool); // (worker thread)
    procedure RemoveRunningTool(Tool: TExternalTool); // (worker thread)
    function OnRunExternalTool(ToolOptions: TIDEExternalToolOptions): boolean; // (main thread)
  protected
    function GetParsers(Index: integer): TExtToolParserClass; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function Add(Title: string): TAbstractExternalTool; override;
    function IndexOf(Tool: TAbstractExternalTool): integer; override;
    property MaxProcessCount: integer read FMaxProcessCount write FMaxProcessCount;
    procedure Work;
    function FindNextToolToStart: TExternalTool;
    procedure Terminate(Tool: TExternalTool);
    procedure TerminateAll; override;
    procedure Clear; override;
    function RunningCount: integer;
    property RunningTools[Index: integer]: TExternalTool read GetRunningTools;
    procedure EnterCriticalSection; override;
    procedure LeaveCriticalSection; override;
    function GetIDEObject(ToolData: TIDEExternalToolData): TObject; override;
    procedure HandleMesages; override;
    // parsers
    function ParserCount: integer; override;
    procedure RegisterParser(Parser: TExtToolParserClass); override;
    procedure UnregisterParser(Parser: TExtToolParserClass); override;
    function FindParserForTool(const SubTool: string): TExtToolParserClass; override;
    function FindParserWithName(const ParserName: string): TExtToolParserClass;
      override;
    function GetMsgTool(Msg: TMessageLine): TAbstractExternalTool; override;
  end;

var
  ExternalTools: TExternalTools = nil;

implementation

{ TLazExtToolConsole }

// inline
function TLazExtToolConsole.Count: integer;
begin
  Result:=fViews.Count;
end;

function TLazExtToolConsole.GetViews(Index: integer): TLazExtToolConsoleView;
begin
  Result:=TLazExtToolConsoleView(fViews[Index]);
end;

constructor TLazExtToolConsole.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fViews:=TFPList.Create;
  ExtToolConsole:=Self;
end;

destructor TLazExtToolConsole.Destroy;
begin
  Clear;
  FreeAndNil(fViews);
  ExtToolConsole:=nil;
  inherited Destroy;
end;

procedure TLazExtToolConsole.Clear;
var
  i: Integer;
begin
  while FindUnfinishedView<>nil do begin
    if Application<>nil then
      Application.ProcessMessages
    else
      CheckSynchronize;
    Sleep(10);
  end;
  for i:=Count-1 downto 0 do begin
    if i>=Count then continue;
    Views[i].Free;
  end;
  if Count>0 then
    raise Exception.Create('TLazExtToolConsole.Clear: some views failed to free');
end;

function TLazExtToolConsole.CreateView(Tool: TAbstractExternalTool
  ): TLazExtToolConsoleView;
begin
  Result:=TLazExtToolConsoleView.Create(Self);
  Result.Caption:=Tool.Title;
  Tool.AddHandlerOnNewOutput(@Result.OnNewOutput);
  fViews.Add(Result);
end;

function TLazExtToolConsole.FindUnfinishedView: TLazExtToolConsoleView;
var
  i: Integer;
begin
  for i:=0 to fViews.Count-1 do begin
    Result:=Views[i];
    if not Result.HasFinished then exit;
  end;
  Result:=nil;
end;

{ TLazExtToolConsoleView }

procedure TLazExtToolConsoleView.ToolExited;
begin
  inherited ToolExited;
  if Tool.Terminated then begin
    ToolState:=lmvtsFailed;
    debugln('Error: (lazarus) ',Caption,': terminated');
  end else if (ExitStatus<>0) then begin
    ToolState:=lmvtsFailed;
    debugln('Error: (lazarus) ',Caption,': stopped with exit code '+IntToStr(ExitStatus));
  end else if Tool.ErrorMessage<>'' then begin
    ToolState:=lmvtsFailed;
    debugln('Error: (lazarus) ',Caption,': ',Tool.ErrorMessage);
  end else begin
    ToolState:=lmvtsSuccess;
  end;
end;

procedure TLazExtToolConsoleView.ProcessNewMessages(AThread: TThread);
begin

end;

procedure TLazExtToolConsoleView.OnNewOutput(Sender: TObject;
  FirstNewMsgLine: integer);
begin
  if (ExtToolConsole<>nil) and ExtToolConsole.Terminating then
    exit;
  while fWrittenLineCount<Tool.WorkerOutput.Count do begin
    debugln(Tool.WorkerOutput[fWrittenLineCount]);
    inc(fWrittenLineCount);
  end;
end;

constructor TLazExtToolConsoleView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TLazExtToolConsoleView.Destroy;
begin
  if Owner is TLazExtToolConsole then
    TLazExtToolConsole(Owner).fViews.Remove(Self);
  inherited Destroy;
end;

procedure TLazExtToolConsoleView.InputClosed;
begin
  inherited InputClosed;
  Free;
end;

{ TLazExtToolView }

procedure TLazExtToolView.SetToolState(AValue: TLMVToolState);
begin
  if FToolState=AValue then Exit;
  FToolState:=AValue;
end;

procedure TLazExtToolView.CallOnChangedInMainThread(Data: PtrInt);
begin
  FAsyncQueued:=false;
  if csDestroying in ComponentState then exit;
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TLazExtToolView.QueueAsyncOnChanged;
begin
  if FAsyncQueued then exit;
  FAsyncQueued:=true;
  if Application<>nil then
    Application.QueueAsyncCall(@CallOnChangedInMainThread,0);
end;

procedure TLazExtToolView.RemoveAsyncOnChanged;
begin
  if not FAsyncQueued then exit;
  FAsyncQueued:=false;
  if Application<>nil then
    Application.RemoveAsyncCalls(Self);
end;

{ TExternalTool }

procedure TExternalTool.ProcessRunning;
var
  i: Integer;
begin
  EnterCriticalSection;
  try
    if FStage<>etsStarting then exit;
    FStage:=etsRunning;
  finally
    LeaveCriticalSection;
  end;
  for i:=0 to ParserCount-1 do
    Parsers[i].InitReading;
end;

procedure TExternalTool.ProcessStopped;
var
  i: Integer;
begin
  {$IFDEF VerboseExtToolErrors}
  if ErrorMessage<>'' then
    DebuglnThreadLog(['TExternalTool.ThreadStopped ',Title,' ErrorMessage=',ErrorMessage]);
  {$ENDIF}
  EnterCriticalSection;
  try
    if (not Terminated) and (ExitStatus<>0) and (ErrorMessage='') then
      ErrorMessage:=Format(lisExitCode, [IntToStr(ExitStatus)]);
    if FStage>=etsStopped then exit;
    FStage:=etsStopped;
  finally
    LeaveCriticalSection;
  end;
  for i:=0 to ParserCount-1 do begin
    try
      Parsers[i].Done;
    except
      on E: Exception do begin
        {$IFDEF VerboseExtToolErrors}
        DebuglnThreadLog(['TExternalTool.ProcessStopped ',Title,' Error in ',DbgSName(Parsers[i]),': ',E.Message]);
        {$ENDIF}
      end;
    end;
  end;
  try
    if Tools<>nil then
      TExternalTools(Tools).RemoveRunningTool(Self);
    Thread.Synchronize(Thread,@NotifyHandlerStopped);
  finally
    fThread:=nil;
  end;
end;

procedure TExternalTool.AddOutputLines(Lines: TStringList);
var
  i: Integer;
  Handled: Boolean;
  Line: LongInt;
  OldOutputCount: LongInt;
  OldMsgCount: LongInt;
  Parser: TExtToolParser;
  NeedSynchronize: Boolean;
  MsgLine: TMessageLine;
  LineStr: String;
begin
  {$IFDEF VerboseExtToolAddOutputLines}
  DebuglnThreadLog(['TExternalTool.AddOutputLines ',Title,' Tick=',IntToStr(GetTickCount64),' Lines=',Lines.Count]);
  {$ENDIF}
  if (Lines=nil) or (Lines.Count=0) then exit;
  NeedSynchronize:=false;
  EnterCriticalSection;
  try
    OldOutputCount:=WorkerOutput.Count;
    OldMsgCount:=WorkerMessages.Count;
    WorkerOutput.AddStrings(Lines);
    for i:=0 to ParserCount-1 do
      Parsers[i].NeedSynchronize:=false;

    // feed new lines into all parsers, converting raw lines into messages
    for Line:=OldOutputCount to WorkerOutput.Count-1 do begin
      Handled:=false;
      LineStr:=WorkerOutput[Line];
      for i:=0 to ParserCount-1 do begin
        {$IFDEF VerboseExtToolAddOutputLines}
        DebuglnThreadLog(['TExternalTool.AddOutputLines ',DbgSName(Parsers[i]),' Line="',WorkerOutput[Line],'" READLINE ...']);
        {$ENDIF}
        Parsers[i].ReadLine(LineStr,Line,Handled);
        if Handled then break;
      end;
      if (not Handled) then begin
        MsgLine:=WorkerMessages.CreateLine(Line);
        MsgLine.Msg:=LineStr; // use raw output as default msg
        MsgLine.Urgency:=mluDebug;
        WorkerMessages.Add(MsgLine);
      end;
    end;

    // let all parsers improve the new messages
    if OldMsgCount<WorkerMessages.Count then begin
      for i:=0 to ParserCount-1 do begin
        Parser:=Parsers[i];
        Parser.NeedSynchronize:=false;
        Parser.NeedAfterSync:=false;
        {$IFDEF VerboseExtToolAddOutputLines}
        DebuglnThreadLog(['TExternalTool.AddOutputLines ',DbgSName(Parser),' IMPROVE after ReadLine ...']);
        {$ENDIF}
        Parser.ImproveMessages(etpspAfterReadLine);
        if Parser.NeedSynchronize then
          NeedSynchronize:=true;
      end;
    end;
  finally
    LeaveCriticalSection;
  end;

  // let all parsers improve the new messages
  if NeedSynchronize then begin
    {$IFDEF VerboseExtToolAddOutputLines}
    DebuglnThreadLog(['TExternalTool.AddOutputLines SynchronizedImproveMessages ...']);
    {$ENDIF}
    Thread.Synchronize(Thread,@SynchronizedImproveMessages);
  end;

  EnterCriticalSection;
  try
    if fNeedAfterSync then begin
      for i:=0 to ParserCount-1 do begin
        Parser:=Parsers[i];
        if not Parser.NeedAfterSync then continue;
        {$IFDEF VerboseExtToolAddOutputLines}
        DebuglnThreadLog(['TExternalTool.AddOutputLines ',DbgSName(Parser),' IMPROVE after sync ...']);
        {$ENDIF}
        Parser.ImproveMessages(etpspAfterSync);
      end;
    end;

    // feed new messages into all viewers
    if OldMsgCount<WorkerMessages.Count then begin
      for i:=0 to ViewCount-1 do begin
        {$IFDEF VerboseExtToolAddOutputLines}
        DebuglnThreadLog(['TExternalTool.AddOutputLines ',DbgSName(Views[i]),' "',Views[i].Caption,'" ProcessNewMessages ...']);
        {$ENDIF}
        Views[i].ProcessNewMessages(Thread);
      end;
    end;
  finally
    LeaveCriticalSection;
  end;

  // notify main thread handlers for new output
  // Note: The IDE itself does not set such a handler
  if {$IFDEF VerboseExtToolAddOutputLines}true{$ELSE}FHandlers[ethNewOutput].Count>0{$ENDIF}
  then begin
    {$IFDEF VerboseExtToolAddOutputLines}
    DebuglnThreadLog(['TExternalTool.AddOutputLines NotifyHandlerNewOutput ...']);
    {$ENDIF}
    Thread.Synchronize(Thread,@NotifyHandlerNewOutput);
  end;
  fOutputCountNotified:=WorkerOutput.Count;
  {$IFDEF VerboseExtToolAddOutputLines}
  DebuglnThreadLog(['TExternalTool.AddOutputLines END']);
  {$ENDIF}
end;

procedure TExternalTool.NotifyHandlerStopped;
var
  i: Integer;
  View: TExtToolView;
begin
  DoCallNotifyHandler(ethStopped);

  EnterCriticalSection;
  try
    for i:=ViewCount-1 downto 0 do begin
      if i>=ViewCount then continue;
      View:=Views[i];
      if ErrorMessage<>'' then
        View.SummaryMsg:=ErrorMessage
      else
        View.SummaryMsg:=lisSuccess;
      View.InputClosed; // this might delete the view
    end;
  finally
    LeaveCriticalSection;
  end;

  if Group<>nil then
    Group.ToolExited(Self);

  // process stopped => start next
  if Tools<>nil then
    TExternalTools(Tools).Work;
end;

procedure TExternalTool.NotifyHandlerNewOutput;
var
  i: integer;
begin
  if fOutputCountNotified>=WorkerOutput.Count then exit;
  {$IFDEF VerboseExtToolAddOutputLines}
  for i:=fOutputCountNotified to WorkerOutput.Count-1 do
    debugln('IDE-DEBUG: ',WorkerOutput[i]);
  {$ENDIF}
  i:=FHandlers[ethNewOutput].Count;
  while FHandlers[ethNewOutput].NextDownIndex(i) do
    TExternalToolNewOutputEvent(FHandlers[ethNewOutput][i])(Self,fOutputCountNotified);
end;

procedure TExternalTool.SetThread(AValue: TExternalToolThread);
var
  CallAutoFree: Boolean;
begin
  // Note: in lazbuild ProcessStopped sets FThread:=nil, so SetThread is not called.
  EnterCriticalSection;
  try
    if FThread=AValue then Exit;
    FThread:=AValue;
    CallAutoFree:=CanFree;
  finally
    LeaveCriticalSection;
  end;
  if CallAutoFree then begin
    if MainThreadID=GetCurrentThreadId then
      AutoFree
    else if (Application<>nil) then
      Application.QueueAsyncCall(@SyncAutoFree,0)
    else
    begin
      debugln(['WARNING: (lazarus) TExternalTool.SetThread can not call AutoFree from other thread']);
    end;
  end;
end;

procedure TExternalTool.SynchronizedImproveMessages;
var
  i: Integer;
  Parser: TExtToolParser;
begin
  EnterCriticalSection;
  try
    fNeedAfterSync:=false;
    for i:=0 to ParserCount-1 do begin
      Parser:=Parsers[i];
      if not Parser.NeedSynchronize then continue;
      {$IFDEF VerboseExtToolAddOutputLines}
      //debugln(['TExternalTool.SynchronizedImproveMessages ',DbgSName(Parser),' ...']);
      {$ENDIF}
      Parser.ImproveMessages(etpspSynchronized);
      Parser.NeedSynchronize:=false;
      if Parser.NeedAfterSync then
        fNeedAfterSync:=true;
    end;
  finally
    LeaveCriticalSection;
  end;
end;

constructor TExternalTool.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FWorkerOutput:=TStringList.Create;
  FProcess:=TProcessUTF8.Create(nil);
  FProcess.Options:= [poUsePipes{$IFDEF Windows},poStderrToOutPut{$ENDIF}];
  FProcess.ShowWindow := swoHide;
  fExecuteBefore:=TFPList.Create;
  fExecuteAfter:=TFPList.Create;
end;

destructor TExternalTool.Destroy;
begin
  //debugln(['TExternalTool.Destroy ',Title]);
  EnterCriticalSection;
  try
    FStage:=etsDestroying;
    if Thread is TExternalToolThread then
      TExternalToolThread(Thread).Tool:=nil;
    FreeAndNil(FProcess);
    FreeAndNil(FWorkerOutput);
    FreeAndNil(fExecuteBefore);
    FreeAndNil(fExecuteAfter);
  finally
    LeaveCriticalSection;
  end;
  inherited Destroy;
end;

procedure TExternalTool.DoExecute;
// in main thread

  function CheckError: boolean;
  begin
    if (FStage>=etsStopped) then exit(true);
    if (ErrorMessage='') then exit(false);
    debugln(['Error: (lazarus) [TExternalTool.DoExecute.CheckError] Error=',ErrorMessage]);
    EnterCriticalSection;
    try
      if FStage>=etsStopped then exit(true);
      FStage:=etsStopped;
    finally
      LeaveCriticalSection;
    end;
    CreateView;
    NotifyHandlerStopped;

    Result:=true;
  end;

var
  ExeFile: String;
  i: Integer;
  aParser: TExtToolParser;
begin
  if Terminated then exit;

  // set Stage to etsInitializing
  EnterCriticalSection;
  try
    if Stage<>etsInit then
      raise Exception.Create('TExternalTool.Execute: already initialized');
    FStage:=etsInitializing;
  finally
    LeaveCriticalSection;
  end;

  // resolve macros
  if ResolveMacrosOnExecute then
  begin
    if not ResolveMacros then begin
      if ErrorMessage='' then
        ErrorMessage:=lisFailedToResolveMacros;
      if CheckError then exit;
    end;
  end;

  // init CurrentDirectory
  Process.CurrentDirectory:=TrimFilename(Process.CurrentDirectory);
  if not FilenameIsAbsolute(Process.CurrentDirectory) then
    Process.CurrentDirectory:=AppendPathDelim(GetCurrentDirUTF8)+Process.CurrentDirectory;

  // init Executable
  Process.Executable:=TrimFilename(Process.Executable);
  {$IFDEF VerboseExtToolThread}
  debugln(['TExternalTool.DoExecute Exe=',Process.Executable]);
  {$ENDIF}
  if not FilenameIsAbsolute(Process.Executable) then begin
    if ExtractFilePath(Process.Executable)<>'' then
      Process.Executable:=AppendPathDelim(GetCurrentDirUTF8)+Process.Executable
    else if Process.Executable='' then begin
      ErrorMessage:=Format(lisToolHasNoExecutable, [Title]);
      CheckError;
      exit;
    end else begin
      ExeFile:=FindDefaultExecutablePath(Process.Executable,GetCurrentDirUTF8);
      if ExeFile='' then begin
        ErrorMessage:=Format(lisCanNotFindExecutable, [Process.Executable]);
        CheckError;
        exit;
      end;
      Process.Executable:=ExeFile;
    end;
  end;
  ExeFile:=Process.Executable;
  if not FileExistsUTF8(ExeFile) then begin
    ErrorMessage:=Format(lisMissingExecutable, [ExeFile]);
    CheckError;
    exit;
  end;
  if DirectoryExistsUTF8(ExeFile) then begin
    ErrorMessage:=Format(lisExecutableIsADirectory, [ExeFile]);
    CheckError;
    exit;
  end;
  if not FileIsExecutable(ExeFile) then begin
    ErrorMessage:=Format(lisExecutableLacksThePermissionToRun, [ExeFile]);
    CheckError;
    exit;
  end;

  // init misc
  WorkerMessages.BaseDirectory:=Process.CurrentDirectory;
  WorkerDirectory:=WorkerMessages.BaseDirectory;
  if EnvironmentOverrides.Count>0 then
    AssignEnvironmentTo(Process.Environment,EnvironmentOverrides);

  // init parsers
  for i:=0 to ParserCount-1 do begin
    aParser:=Parsers[i];
    try
      aParser.Init;
    except
      on E: Exception do begin
        ErrorMessage:=Format(lisParser, [DbgSName(aParser), E.Message]);
        CheckError;
        exit;
      end;
    end;
  end;

  // set Stage to etsWaitingForStart
  EnterCriticalSection;
  try
    if Stage<>etsInitializing then
      raise Exception.Create('TExternalTool.Execute: bug in initialization');
    FStage:=etsWaitingForStart;
  finally
    LeaveCriticalSection;
  end;
end;

procedure TExternalTool.DoStart;
var
  i: Integer;
begin
  // set Stage to etsStarting
  EnterCriticalSection;
  try
    if Stage<>etsWaitingForStart then
      raise Exception.Create('TExternalTool.Execute: already started');
    FStage:=etsStarting;
  finally
    LeaveCriticalSection;
  end;

  CreateView;

  // mark running
  if Tools<>nil then
    TExternalTools(Tools).AddRunningTool(Self);

  // start thread
  if Thread=nil then begin
    FThread:=TExternalToolThread.Create(true);
    Thread.Tool:=Self;
    FThread.FreeOnTerminate:=true;
  end;
  if ConsoleVerbosity>=-1 then begin
    debugln(['Info: (lazarus) Execute Title="',Title,'"']);
    debugln(['Info: (lazarus) Working Directory="',Process.CurrentDirectory,'"']);
    debugln(['Info: (lazarus) Executable="',Process.Executable,'"']);
    for i:=0 to Process.Parameters.Count-1 do
      debugln(['Info: (lazarus) Param[',i,']="',Process.Parameters[i],'"']);
  end;
  Thread.Start;
end;

procedure TExternalTool.CreateView;
var
  View: TExtToolView;
begin
  if ViewCount>0 then exit;
  View:=nil;
  if ExtToolConsole<>nil then begin
    // in console mode (lazbuild) all output goes unparsed to console
    ClearParsers;
    View:=ExtToolConsole.CreateView(Self);
  end else if (ViewCount=0) and (ParserCount>0) then begin
    // this tool generates parsed output => auto create view
    if IDEMessagesWindow<>nil then
      View:=IDEMessagesWindow.CreateView(Title);
  end;
  if View<>nil then
    AddView(View);
end;

function TExternalTool.ExecuteBeforeCount: integer;
begin
  Result:=fExecuteBefore.Count;
end;

function TExternalTool.ExecuteAfterCount: integer;
begin
  Result:=fExecuteAfter.Count;
end;

function TExternalTool.GetExecuteAfter(Index: integer): TAbstractExternalTool;
begin
  Result:=TAbstractExternalTool(fExecuteAfter[Index]);
end;

function TExternalTool.GetExecuteBefore(Index: integer): TAbstractExternalTool;
begin
  Result:=TAbstractExternalTool(fExecuteBefore[Index]);
end;

procedure TExternalTool.DoTerminate;
var
  NeedProcTerminate: Boolean;
begin
  NeedProcTerminate:=false;
  EnterCriticalSection;
  try
    //debugln(['TExternalTool.DoTerminate ',Title,' Terminated=',Terminated,' Stage=',dbgs(Stage)]);
    if Terminated then exit;
    if Stage=etsStopped then exit;

    if ErrorMessage='' then
      ErrorMessage:=lisAborted;
    fTerminated:=true;
    if Stage=etsRunning then
      NeedProcTerminate:=true;
    if Stage<etsStarting then
      FStage:=etsStopped
    else if Stage<=etsRunning then
      FStage:=etsWaitingForStop;
  finally
    LeaveCriticalSection;
  end;
  if NeedProcTerminate and (Process<>nil) then
    Process.Terminate(AbortedExitCode);
end;

procedure TExternalTool.SyncAutoFree(aData: PtrInt);
begin
  AutoFree;
end;

procedure TExternalTool.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    if fExecuteBefore<>nil then
      fExecuteBefore.Remove(AComponent);
    if fExecuteAfter<>nil then
      fExecuteAfter.Remove(AComponent);
  end;
end;

function TExternalTool.CanFree: boolean;
begin
  Result:=(FThread=nil)
       and inherited CanFree;
end;

function TExternalTool.IsExecutedBefore(Tool: TAbstractExternalTool): Boolean;
var
  Visited: TFPList;

  function Search(CurTool: TAbstractExternalTool): Boolean;
  var
    i: Integer;
  begin
    if CurTool=Tool then exit(true);
    if Visited.IndexOf(CurTool)>=0 then exit(false);
    Visited.Add(CurTool);
    for i:=0 to CurTool.ExecuteBeforeCount-1 do
      if Search(CurTool.ExecuteBefore[i]) then exit(true);
    Result:=false;
  end;

begin
  Result:=false;
  if Tool=Self then exit;
  Visited:=TFPList.Create;
  try
    Result:=Search(Self);
  finally
    Visited.Free;
  end;
end;

procedure TExternalTool.AddExecuteBefore(Tool: TAbstractExternalTool);
begin
  //debugln(['TExternalTool.AddExecuteBefore Self=',Title,' Tool=',Tool.Title]);
  if (Tool=Self) or (Tool.IsExecutedBefore(Self)) then
    raise Exception.Create('TExternalTool.AddExecuteBefore: that would create a circle');
  if (fExecuteBefore<>nil) and (fExecuteBefore.IndexOf(Tool)<0) then
    fExecuteBefore.Add(Tool);
  if (TExternalTool(Tool).fExecuteAfter<>nil)
  and (TExternalTool(Tool).fExecuteAfter.IndexOf(Self)<=0) then
    TExternalTool(Tool).fExecuteAfter.Add(Self);
end;

function TExternalTool.CanStart: boolean;
var
  i: Integer;
  ExecBefore: TAbstractExternalTool;
begin
  Result:=false;
  //debugln(['TExternalTool.CanStart ',Title,' ',dbgs(Stage)]);
  if Stage<>etsWaitingForStart then exit;
  if Terminated then exit;
  for i:=0 to ExecuteBeforeCount-1 do begin
    ExecBefore:=ExecuteBefore[i];
    if ord(ExecBefore.Stage)<ord(etsStopped) then exit;
    if ExecBefore.ErrorMessage<>'' then exit;
  end;
  Result:=true;
end;

function TExternalTool.GetLongestEstimatedLoad: int64;
type
  TInfo = record
    Load: int64;
  end;
  PInfo = ^TInfo;
var
  ToolToInfo: TPointerToPointerTree;

  function GetLoad(Tool: TExternalTool): int64;
  var
    Info: PInfo;
    i: Integer;
  begin
    Info:=PInfo(ToolToInfo[Tool]);
    if Info<>nil then
      Result:=Info^.Load
    else begin
      New(Info);
      Info^.Load:=1;
      ToolToInfo[Tool]:=Info;
      Result:=0;
      for i:=0 to Tool.ExecuteAfterCount-1 do
        Result:=Max(Result,GetLoad(TExternalTool(Tool.ExecuteAfter[i])));
      inc(Result,Tool.EstimatedLoad);
      Info^.Load:=Result;
    end;
  end;

var
  Node: TAvlTreeNode;
  Item: PPointerToPointerItem;
  Info: PInfo;
begin
  ToolToInfo:=TPointerToPointerTree.Create;
  try
    Result:=GetLoad(Self);
  finally
    Node:=ToolToInfo.Tree.FindLowest;
    while Node<>nil do begin
      Item:=PPointerToPointerItem(Node.Data);
      Info:=PInfo(Item^.Value);
      Dispose(Info);
      Node:=ToolToInfo.Tree.FindSuccessor(Node);
    end;
    ToolToInfo.Free;
  end;
end;

procedure TExternalTool.Execute;
begin
  if Stage<>etsInit then
    raise Exception.Create('TExternalTool.Execute "'+Title+'" already started');
  DoExecute;
  if Stage<>etsWaitingForStart then
    exit;

  if Tools<>nil then
    TExternalTools(Tools).Work
  else
    DoStart;
end;

procedure TExternalTool.Terminate;
begin
  if Tools<>nil then
    TExternalTools(Tools).Terminate(Self)
  else
    DoTerminate;
end;

procedure TExternalTool.WaitForExit;
var
  MyTools: TIDEExternalTools;
begin
  MyTools:=Tools;
  repeat
    EnterCriticalSection;
    try
      if Stage=etsDestroying then exit;
      if (Stage=etsStopped) and (FindUnfinishedView=nil) then exit;
    finally
      LeaveCriticalSection;
    end;
    // call synchronized tasks, this might free this tool
    if MainThreadID=ThreadID then
      if Application<>nil then
        Application.ProcessMessages
      else
        CheckSynchronize;
    // check if this tool still exists
    if MyTools.IndexOf(Self)<0 then exit;
    // still running => wait
    Sleep(10);
  until false;
end;

function TExternalTool.ResolveMacros: boolean;

  function Resolve(const aValue: string; out NewValue: string): boolean;
  begin
    NewValue:=aValue;
    Result:=IDEMacros.SubstituteMacros(NewValue);
    if Result then exit;
    if ErrorMessage='' then
      ErrorMessage:=Format(lisInvalidMacrosIn, [aValue]);
    IDEMessageDialog(lisCCOErrorCaption, Format(lisInvalidMacrosInExternalTool,
      [aValue, Title]),
      mtError,[mbCancel]);
  end;

var
  i: Integer;
  s: string;
begin
  if IDEMacros=nil then exit(true);
  Result:=false;

  if not Resolve(Process.CurrentDirectory,s) then exit;
  Process.CurrentDirectory:=s;

  if not Resolve(Process.Executable,s) then exit;
  Process.Executable:=s;

  for i:=0 to Process.Parameters.Count-1 do begin
    if not Resolve(Process.Parameters[i],s) then exit;
    Process.Parameters[i]:=s;
  end;

  for i:=0 to EnvironmentOverrides.Count-1 do begin
    if not Resolve(EnvironmentOverrides[i],s) then exit;
    EnvironmentOverrides[i]:=s;
  end;

  Result:=true;
end;

procedure TExternalTool.RemoveExecuteBefore(Tool: TAbstractExternalTool);
begin
  if fExecuteBefore<>nil then
    fExecuteBefore.Remove(Tool);
  if TExternalTool(Tool).fExecuteAfter<>nil then
    TExternalTool(Tool).fExecuteAfter.Remove(Self);
end;

{ TExternalTools }

function TExternalTools.OnRunExternalTool(ToolOptions: TIDEExternalToolOptions
  ): boolean;
var
  Tool: TAbstractExternalTool;
  i: Integer;
  Proc: TProcessUTF8;
  s: String;
  sl: TStringList;
  Path: String;
begin
  {$IFDEF VerboseExtToolThread}
  debugln(['TExternalTools.OnRunExternalTool ',ToolOptions.Title,' exe="',ToolOptions.Executable,'" params="',ToolOptions.CmdLineParams,'"']);
  {$ENDIF}
  Result:=false;

  if (ToolOptions.Parsers.Count=0) and (ExtToolConsole=nil) then begin
    // simply run and detach
    Proc:=TProcessUTF8.Create(nil);
    try
      Proc.InheritHandles:=false;
      // working directory
      s:=ToolOptions.WorkingDirectory;
      if ToolOptions.ResolveMacros then begin
        if not GlobalMacroList.SubstituteStr(s) then begin
          debugln(['Error: (lazarus) [TExternalTools.OnRunExternalTool] ',ToolOptions.Title,' failed: macros of WorkerDirectory: "',ToolOptions.WorkingDirectory,'"']);
          exit;
        end;
      end;
      s:=ChompPathDelim(CleanAndExpandDirectory(s));
      if not DirectoryExistsUTF8(s) then begin
        debugln(['Error: (lazarus) [TExternalTools.OnRunExternalTool] ',ToolOptions.Title,' failed: missing directory "',s,'"']);
        exit;
      end;
      Proc.CurrentDirectory:=s;

      // environment
      if ToolOptions.EnvironmentOverrides.Count>0 then
        AssignEnvironmentTo(Proc.Environment,ToolOptions.EnvironmentOverrides);
      if ToolOptions.ResolveMacros then begin
        for i:=0 to Proc.Environment.Count-1 do begin
          s:=Proc.Environment[i];
          if not GlobalMacroList.SubstituteStr(s) then begin
            debugln(['Error: (lazarus) [TExternalTools.OnRunExternalTool] ',ToolOptions.Title,' failed: environment override "',Proc.Environment,'"']);
            exit;
          end;
          Proc.Environment[i]:=s;
        end;
      end;

      // executable
      s:=ToolOptions.Executable;
      if ToolOptions.ResolveMacros then begin
        if not GlobalMacroList.SubstituteStr(s) then begin
          debugln(['Error: (lazarus) [TExternalTools.OnRunExternalTool] ',ToolOptions.Title,' failed: macros of Executable: "',ToolOptions.Executable,'"']);
          exit;
        end;
      end;
      if not FilenameIsAbsolute(s) then begin
        // search in PATH
        if Proc.Environment.Count>0 then
          Path:=Proc.Environment.Values['PATH']
        else
          Path:=GetEnvironmentVariableUTF8('PATH');
        s:=SearchFileInPath(s,Proc.CurrentDirectory,
                                 Path, PathSeparator,
                                 []);
        {$IFDEF Windows}
        if (s='') and (ExtractFileExt(s)='') then begin
          s:=SearchFileInPath(s+'.exe',Proc.CurrentDirectory,
                                   Path, PathSeparator,
                                   []);
        end;
        {$ENDIF}
        if s='' then begin
          debugln(['Error: (lazarus) [TExternalTools.OnRunExternalTool] ',ToolOptions.Title,' failed: missing executable "',ToolOptions.Executable,'"']);
          exit;
        end;
      end;
      if (not FilenameIsAbsolute(s))
      or (not FileExistsUTF8(s)) then begin
        debugln(['Error: (lazarus) [TExternalTools.OnRunExternalTool] ',ToolOptions.Title,'  failed: missing executable: "',s,'"']);
        exit;
      end;
      if DirectoryExistsUTF8(s) then begin
        debugln(['Error: (lazarus) [TExternalTools.OnRunExternalTool] ',ToolOptions.Title,'  failed: executable is a directory: "',s,'"']);
        exit;
      end;
      if not FileIsExecutable(s) then begin
        debugln(['Error: (lazarus) [TExternalTools.OnRunExternalTool] ',ToolOptions.Title,'  failed: executable lacks permission to run: "',s,'"']);
        exit;
      end;
      Proc.Executable:=s;

      // params
      s:=ToolOptions.CmdLineParams;
      if ToolOptions.ResolveMacros then begin
        if not GlobalMacroList.SubstituteStr(s) then begin
          debugln(['Error: (lazarus) [TExternalTools.OnRunExternalTool] ',ToolOptions.Title,' failed: macros in cmd line params "',ToolOptions.CmdLineParams,'"']);
          exit;
        end;
      end;
      sl:=TStringList.Create;
      try
        SplitCmdLineParams(s,sl);
        Proc.Parameters:=sl;
      finally
        sl.Free;
      end;

      // run and detach
      {$IF FPC_FULLVERSION<20604}
      Proc.InheritHandles:=false;
      {$ENDIF}
      if ToolOptions.ShowConsole then
        Proc.Options:=Proc.Options+[poNewConsole]-[poNoConsole]
      else
        Proc.Options:=Proc.Options-[poNewConsole]+[poNoConsole];
      if ToolOptions.HideWindow then
        Proc.ShowWindow:=swoHide
      else
        Proc.ShowWindow:=swoShow;
      try
        Proc.Execute;
      except
      end;
    finally
      Proc.Free;
    end;
  end else begin
    // run with parsers and messages
    {$IFDEF VerboseExtToolThread}
    debugln(['TExternalTools.OnRunExternalTool run with scanners ...']);
    {$ENDIF}
    Tool:=Add(ToolOptions.Title);
    Tool.Reference(Self,ClassName);
    try
      Tool.Hint:=ToolOptions.Hint;
      Tool.Process.CurrentDirectory:=ToolOptions.WorkingDirectory;
      Tool.Process.Executable:=ToolOptions.Executable;
      Tool.CmdLineParams:=ToolOptions.CmdLineParams;
      Tool.EnvironmentOverrides:=ToolOptions.EnvironmentOverrides;
      if ExtToolConsole=nil then
        for i:=0 to ToolOptions.Parsers.Count-1 do
          Tool.AddParsers(ToolOptions.Parsers[i]);
      if ToolOptions.ShowConsole then
        Tool.Process.Options:=Tool.Process.Options+[poNewConsole]-[poNoConsole]
      else
        Tool.Process.Options:=Tool.Process.Options-[poNewConsole]+[poNoConsole];
      if ToolOptions.HideWindow then
        Tool.Process.ShowWindow:=swoHide
      else
        Tool.Process.ShowWindow:=swoShow;
      if ToolOptions.ResolveMacros then begin
        if not Tool.ResolveMacros then begin
          debugln(['Error: (lazarus) [TExternalTools.OnRunExternalTool] failed to resolve macros']);
          exit;
        end;
      end;
      {$IFDEF VerboseExtToolThread}
      debugln(['TExternalTools.OnRunExternalTool Execute ',Tool.Title,' WD="',Tool.Process.CurrentDirectory,'" Exe="',Tool.Process.Executable,'" Params="',Tool.CmdLineParams,'" ...']);
      {$ENDIF}
      Tool.Execute;
      {$IFDEF VerboseExtToolThread}
      debugln(['TExternalTools.OnRunExternalTool WaitForExit ',Tool.Title,' ...']);
      {$ENDIF}
      Tool.WaitForExit;
      {$IFDEF VerboseExtToolThread}
      debugln(['TExternalTools.OnRunExternalTool Done ',Tool.Title]);
      {$ENDIF}
      Result:=(Tool.ErrorMessage='') and (not Tool.Terminated) and (Tool.ExitStatus=0);
    finally
      Tool.Release(Self);
    end;
  end;
end;

function TExternalTools.GetRunningTools(Index: integer): TExternalTool;
begin
  EnterCriticalSection;
  try
    Result:=TExternalTool(fRunning[Index]);
  finally
    LeaveCriticalSection;
  end;
end;

procedure TExternalTools.AddRunningTool(Tool: TExternalTool);
begin
  EnterCriticalSection;
  try
    if fRunning.IndexOf(Tool)<0 then
      fRunning.Add(Tool);
  finally
    LeaveCriticalSection;
  end;
end;

procedure TExternalTools.RemoveRunningTool(Tool: TExternalTool);
begin
  EnterCriticalSection;
  try
    fRunning.Remove(Tool);
  finally
    LeaveCriticalSection;
  end;
end;

function TExternalTools.GetParsers(Index: integer): TExtToolParserClass;
begin
  Result:=TExtToolParserClass(fParsers[Index]);
end;

procedure TExternalTools.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    EnterCriticalSection;
    try
      if fItems<>nil then
        fItems.Remove(AComponent);
      if fRunning<>nil then
        fRunning.Remove(AComponent);
    finally
      LeaveCriticalSection;
    end;
  end;
end;

constructor TExternalTools.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  InitCriticalSection(FCritSec);
  fRunning:=TFPList.Create;
  fParsers:=TFPList.Create;
  MaxProcessCount:=DefaultMaxProcessCount;
  if ExternalToolList=nil then
    ExternalToolList:=Self;
  if ExternalTools=nil then
    ExternalTools:=Self;
  RunExternalTool:=@OnRunExternalTool;
end;

destructor TExternalTools.Destroy;
begin
  RunExternalTool:=nil;
  TerminateAll;
  EnterCriticalSection;
  try
    if fRunning.Count>0 then
      raise Exception.Create('TExternalTools.Destroy some tools still running');
    if ExternalToolList=Self then
      ExternalToolList:=nil;
    if ExternalTools=Self then
      ExternalTools:=nil;
    inherited Destroy;
    FreeAndNil(fRunning);
    FreeAndNil(fParsers);
  finally
    LeaveCriticalSection;
  end;
  DoneCriticalsection(FCritSec);
end;

function TExternalTools.Add(Title: string): TAbstractExternalTool;
begin
  Result:=TExternalTool.Create(Self);
  Result.Title:=Title;
  fItems.Add(Result);
end;

function TExternalTools.IndexOf(Tool: TAbstractExternalTool): integer;
begin
  Result:=fItems.IndexOf(Tool);
end;

function TExternalTools.ParserCount: integer;
begin
  Result:=fParsers.Count;
end;

procedure TExternalTools.Work;
var
  Tool: TExternalTool;
begin
  while RunningCount<MaxProcessCount do begin
    Tool:=FindNextToolToStart;
    if Tool=nil then exit;
    Tool.DoStart;
  end;
end;

function TExternalTools.FindNextToolToStart: TExternalTool;
var
  Tool: TExternalTool;
  CurLoad: Int64;
  Load: Int64;
  i: Integer;
begin
  Result:=nil;
  Load:=0;
  for i:=0 to Count-1 do begin
    Tool:=TExternalTool(Items[i]);
    //debugln(['TExternalTools.FindNextToolToExec ',Tool.Title,' ',Tool.CanStart]);
    if not Tool.CanStart then continue;
    CurLoad:=Tool.GetLongestEstimatedLoad;
    if (Result<>nil) and (Load>=CurLoad) then Continue;
    Result:=Tool;
    Load:=CurLoad;
  end;
end;

procedure TExternalTools.Terminate(Tool: TExternalTool);
begin
  if Tool=nil then exit;
  Tool.DoTerminate;
end;

procedure TExternalTools.TerminateAll;
var
  i: Integer;
begin
  // terminate all current tools
  if ExtToolConsole<>nil then
    ExtToolConsole.Terminating:=true;
  for i:=Count-1 downto 0 do begin
    if i>=Count then continue;
    Terminate(Items[i] as TExternalTool);
  end;
  if ExtToolConsole<>nil then
    ExtToolConsole.Terminating:=false;
end;

procedure TExternalTools.Clear;
begin
  TerminateAll;
  while Count>0 do
    Items[0].Free;
end;

function TExternalTools.RunningCount: integer;
begin
  Result:=fRunning.Count;
end;

procedure TExternalTools.EnterCriticalSection;
begin
  System.EnterCriticalsection(FCritSec);
end;

procedure TExternalTools.LeaveCriticalSection;
begin
  System.LeaveCriticalsection(FCritSec);
end;

function TExternalTools.GetIDEObject(ToolData: TIDEExternalToolData): TObject;
begin
  Result:=nil;
  if ToolData=nil then exit;
  if ToolData.Kind=IDEToolCompileProject then begin
    Result:=LazarusIDE.ActiveProject;
  end else if ToolData.Kind=IDEToolCompilePackage then begin
    Result:=PackageEditingInterface.FindPackageWithName(ToolData.ModuleName);
  end else if ToolData.Kind=IDEToolCompileIDE then begin
    Result:=LazarusIDE;
  end;
end;

procedure TExternalTools.HandleMesages;
begin
  if Application<>nil then
    Application.ProcessMessages
  else
    CheckSynchronize;
end;

procedure TExternalTools.RegisterParser(Parser: TExtToolParserClass);
begin
  if fParsers.IndexOf(Parser)>=0 then exit;
  fParsers.Add(Parser);
end;

procedure TExternalTools.UnregisterParser(Parser: TExtToolParserClass);
begin
  if fParsers=nil then exit;
  fParsers.Remove(Parser);
end;

function TExternalTools.FindParserForTool(const SubTool: string
  ): TExtToolParserClass;
var
  i: Integer;
begin
  for i:=0 to fParsers.Count-1 do begin
    Result:=TExtToolParserClass(fParsers[i]);
    if Result.CanParseSubTool(SubTool) then exit;
  end;
  Result:=nil;
end;

function TExternalTools.FindParserWithName(const ParserName: string
  ): TExtToolParserClass;
var
  i: Integer;
begin
  for i:=0 to fParsers.Count-1 do begin
    Result:=TExtToolParserClass(fParsers[i]);
    if SameText(Result.GetParserName,ParserName) then exit;
  end;
  Result:=nil;
end;

function TExternalTools.GetMsgTool(Msg: TMessageLine): TAbstractExternalTool;
var
  CurOwner: TObject;
  View: TExtToolView;
begin
  Result:=nil;
  if (Msg=nil) or (Msg.Lines=nil) then exit;
  CurOwner:=Msg.Lines.Owner;
  if CurOwner=nil then exit;
  if CurOwner is TAbstractExternalTool then
    Result:=TAbstractExternalTool(CurOwner)
  else if CurOwner is TExtToolView then begin
    View:=TExtToolView(CurOwner);
    Result:=View.Tool;
  end;
end;

{ TExternalToolThread }

procedure TExternalToolThread.SetTool(AValue: TExternalTool);
begin
  if FTool=AValue then Exit;
  if FTool<>nil then
    FTool.Thread:=nil;
  FTool:=AValue;
  if FTool<>nil then
    FTool.Thread:=Self;
end;

procedure TExternalToolThread.Execute;
type
  TErrorFrame = record
    Addr: Pointer;
    Line: shortstring;
  end;
  PErrorFrame = ^TErrorFrame;

var
  ErrorFrames: array[0..30] of TErrorFrame;
  ErrorFrameCount: integer;

  function GetExceptionStackTrace: string;
  var
    FrameCount: LongInt;
    Frames: PPointer;
    Cnt: LongInt;
    f: PErrorFrame;
    i: Integer;
  begin
    Result:='';
    FrameCount:=ExceptFrameCount;
    Frames:=ExceptFrames;
    ErrorFrames[0].Addr:=ExceptAddr;
    ErrorFrames[0].Line:='';
    ErrorFrameCount:=1;
    Cnt:=FrameCount;
    for i:=1 to Cnt do begin
      ErrorFrames[i].Addr:=Frames[i-1];
      ErrorFrames[i].Line:='';
      ErrorFrameCount:=i+1;
    end;
    for i:=0 to ErrorFrameCount-1 do begin
      f:=@ErrorFrames[i];
      try
        f^.Line:=copy(BackTraceStrFunc(f^.Addr),1,255);
      except
        f^.Line:=copy(SysBackTraceStr(f^.Addr),1,255);
      end;
    end;
    for i:=0 to ErrorFrameCount-1 do begin
      Result+=ErrorFrames[i].Line+LineEnding;
    end;
  end;

var
  Buf: string;

  function ReadInputPipe(aStream: TInputPipeStream; var LineBuf: string): boolean;
  // true if some bytes have been read
  var
    Count: DWord;
    StartPos: Integer;
    i: DWord;
  begin
    Result:=false;
    if aStream=nil then exit;
    Count:=aStream.NumBytesAvailable;
    if Count=0 then exit;
    Count:=aStream.Read(Buf[1],Min(length(Buf),Count));
    if Count=0 then exit;
    Result:=true;
    StartPos:=1;
    i:=1;
    while i<=Count do begin
      if Buf[i] in [#10,#13] then begin
        LineBuf:=LineBuf+copy(Buf,StartPos,i-StartPos);
        fLines.Add(LineBuf);
        LineBuf:='';
        if (i<Count) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1])
        then
          inc(i);
        StartPos:=i+1;
      end;
      inc(i);
    end;
    LineBuf:=LineBuf+copy(Buf,StartPos,Count-StartPos+1);
  end;

const
  UpdateTimeDiff = 1000 div 5; // update five times a second, even if there is still work
var
  {$IFDEF VerboseExtToolThread}
  Title: String;
  {$ENDIF}
  OutputLine, StdErrLine: String;
  LastUpdate: QWord;
  ErrMsg: String;
  ok: Boolean;
  HasOutput: Boolean;
begin
  {$IFDEF VerboseExtToolThread}
  Title:=Tool.Title;
  {$ENDIF}
  SetLength(Buf,4096);
  ErrorFrameCount:=0;
  fLines:=TStringList.Create;
  try
    try
      if Tool.Stage<>etsStarting then begin
        {$IFDEF VerboseExtToolThread}
        DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' Tool.Stage=',dbgs(Tool.Stage),' aborting']);
        {$ENDIF}
        exit;
      end;

      {$IFDEF VerboseExtToolThread}
      DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' check executing "',Tool.Process.Executable,'" ...']);
      {$ENDIF}

      if not FileIsExecutable(Tool.Process.Executable) then begin
        Tool.ErrorMessage:=Format(lisCanNotExecute, [Tool.Process.Executable]);
        Tool.ProcessStopped;
        exit;
      end;
      if not DirectoryExistsUTF8(ChompPathDelim(Tool.Process.CurrentDirectory))
      then begin
        Tool.ErrorMessage:=Format(lisMissingDirectory, [Tool.Process.
          CurrentDirectory]);
        Tool.ProcessStopped;
        exit;
      end;

      // Under Unix TProcess uses fpFork, which means the current thread is
      // duplicated. One is the old thread and one runs fpExecve.
      // If fpExecve runs, then it will not return.
      // If fpExecve fails it returns via an exception and this thread runs twice.
      ok:=false;
      try
        {$IFDEF VerboseExtToolThread}
        DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' execute ...']);
        {$ENDIF}
        // now execute
        Tool.Process.PipeBufferSize:=Max(Tool.Process.PipeBufferSize,64*1024);
        Tool.Process.Execute;
        {$IFDEF VerboseExtToolThread}
        DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' executing ...']);
        {$ENDIF}
        ok:=true;
      except
        on E: Exception do begin
          // BEWARE: we are now either in the normal thread or in the failed forked thread
          {$IFDEF VerboseExtToolThread}
          DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' execute failed: ',E.Message]);
          {$ENDIF}
          if Tool.ErrorMessage='' then
            Tool.ErrorMessage:=Format(lisUnableToExecute, [E.Message]);
        end;
      end;
      // BEWARE: we are now either in the normal thread or in the failed forked thread
      if not ok then begin
        Tool.ProcessStopped;
        exit;
      end;
      // we are now in the normal thread
      if Tool.Stage>=etsStopped then
        exit;
      {$IFDEF VerboseExtToolThread}
      DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' ProcessRunning ...']);
      {$ENDIF}
      Tool.ProcessRunning;
      if Tool.Stage>=etsStopped then
        exit;
      {$IFDEF VerboseExtToolThread}
      DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' reading ...']);
      {$ENDIF}

      OutputLine:='';
      StdErrLine:='';
      LastUpdate:=GetTickCount64;
      while (Tool<>nil) and (Tool.Stage=etsRunning) do begin
        if Tool.ReadStdOutBeforeErr then begin
          HasOutput:=ReadInputPipe(Tool.Process.Output,OutputLine)
                  or ReadInputPipe(Tool.Process.Stderr,StdErrLine);
        end else begin
          HasOutput:=ReadInputPipe(Tool.Process.Stderr,StdErrLine)
                  or ReadInputPipe(Tool.Process.Output,OutputLine);
        end;
        if (not HasOutput) then begin
          // no more pending output
          if not Tool.Process.Running then break;
        end;
        if (fLines.Count>0)
        and (Abs(int64(GetTickCount64)-LastUpdate)>UpdateTimeDiff) then begin
          {$IFDEF VerboseExtToolThread}
          DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' ',TimeToStr(Now),' ',IntToStr(GetTickCount),' AddOutputLines ...']);
          {$ENDIF}
          Tool.AddOutputLines(fLines);
          {$IFDEF VerboseExtToolThread}
          DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' AddOutputLines ok']);
          {$ENDIF}
          fLines.Clear;
          LastUpdate:=GetTickCount64;
        end;
        if (not HasOutput) then begin
          // no more pending output and process is still running
          // => tool needs some time
          Sleep(10);
        end;
      end;
      {$IFDEF VerboseExtToolThread}
      DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' end reading']);
      {$ENDIF}
      // add rest of output
      if (OutputLine<>'') then
        fLines.Add(OutputLine);
      if (StdErrLine<>'') then
        fLines.Add(StdErrLine);
      if (Tool<>nil) and (fLines.Count>0) then begin
        {$IFDEF VerboseExtToolThread}
        DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' final AddOutputLines ...']);
        {$ENDIF}
        Tool.AddOutputLines(fLines);
        {$IFDEF VerboseExtToolThread}
        DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' final AddOutputLines ok']);
        {$ENDIF}
        fLines.Clear;
      end;
      try
        if Tool.Stage>=etsStopped then begin
          {$IFDEF VerboseExtToolThread}
          DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' not reading exit status, because already stopped']);
          {$ENDIF}
          exit;
        end;
        {$IFDEF VerboseExtToolThread}
        DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' reading exit status ...']);
        {$ENDIF}
        Tool.ExitStatus:=Tool.Process.ExitStatus;
      except
        Tool.ErrorMessage:=lisUnableToReadProcessExitStatus;
      end;
    except
      on E: Exception do begin
        {$IFDEF VerboseExtToolThread}
        DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' run: ',E.Message]);
        {$ENDIF}
        if (Tool<>nil) and (Tool.ErrorMessage='') then begin
          Tool.ErrorMessage:=E.Message;
          ErrMsg:=GetExceptionStackTrace;
          {$IFDEF VerboseExtToolErrors}
          DebuglnThreadLog(ErrMsg);
          {$ENDIF}
          Tool.ErrorMessage:=E.Message+LineEnding+ErrMsg;
        end;
      end;
    end;
  finally
    {$IFDEF VerboseExtToolThread}
    if fLines<>nil then
      DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' cleaning up']);
    {$ENDIF}
    // clean up
    try
      FreeAndNil(fLines);
    except
      on E: Exception do begin
        {$IFDEF VerboseExtToolThread}
        DebuglnThreadLog(['TExternalToolThread.Execute adding pending messages: ',E.Message]);
        {$ENDIF}
        if Tool<>nil then
          Tool.ErrorMessage:=Format(lisFreeingBufferLines, [E.Message]);
      end;
    end;
  end;
  if Tool.Stage>=etsStopped then begin
    {$IFDEF VerboseExtToolThread}
    DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' not cleaning up']);
    {$ENDIF}
    exit;
  end;
  {$IFDEF VerboseExtToolThread}
  DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' ProcessStopped ...']);
  {$ENDIF}
  if Tool<>nil then
    Tool.ProcessStopped;
  {$IFDEF VerboseExtToolThread}
  DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' Thread END']);
  {$ENDIF}
end;

destructor TExternalToolThread.Destroy;
begin
  Tool:=nil;
  inherited Destroy;
end;

end.

