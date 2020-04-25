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
  // LazUtils
  FileUtil, LazFileUtils, LazUtilities, LazLoggerBase, UTF8Process, LazUTF8,
  UITypes, AvgLvlTree,
  // IDEIntf
  IDEExternToolIntf, BaseIDEIntf, MacroIntf, LazMsgDialogs,
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
    FToolState: TLMVToolState;
  protected
    procedure SetToolState(AValue: TLMVToolState); virtual;
  public
    property ToolState: TLMVToolState read FToolState write SetToolState;
  end;

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
    procedure DebuglnThreadLog(const Args: array of const);
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
  protected
    procedure DoExecute; override;           // (main thread)
    procedure DoStart;                       // (main thread)
    procedure CreateView; virtual; abstract; // (main thread)
    function GetExecuteAfter(Index: integer): TAbstractExternalTool; override;
    function GetExecuteBefore(Index: integer): TAbstractExternalTool; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CanFree: boolean; override;
    procedure QueueAsyncAutoFree; virtual; abstract;
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

  TExternalToolClass = class of TExternalTool;

  { TExternalTools }

  TExternalTools = class(TExternalToolsBase)
  private
    FCritSec: TRTLCriticalSection;
    fRunning: TFPList; // list of TExternalTool, needs Enter/LeaveCriticalSection
    FMaxProcessCount: integer;
    fParsers: TFPList; // list of TExtToolParserClass
    function GetRunningTools(Index: integer): TExternalTool;
    procedure AddRunningTool(Tool: TExternalTool); // (worker thread)
    procedure RemoveRunningTool(Tool: TExternalTool); // (worker thread)
    function RunExtToolHandler(ToolOptions: TIDEExternalToolOptions): boolean;
    function RunToolAndDetach(ToolOptions: TIDEExternalToolOptions): boolean;
    function RunToolWithParsers(ToolOptions: TIDEExternalToolOptions): boolean;
  protected
    FToolClass: TExternalToolClass;
    function GetParsers(Index: integer): TExtToolParserClass; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
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
    // parsers
    function ParserCount: integer; override;
    procedure RegisterParser(Parser: TExtToolParserClass); override;
    procedure UnregisterParser(Parser: TExtToolParserClass); override;
    function FindParserForTool(const SubTool: string): TExtToolParserClass; override;
    function FindParserWithName(const ParserName: string): TExtToolParserClass; override;
    function GetMsgTool(Msg: TMessageLine): TAbstractExternalTool; override;
  end;

  TExternalToolsClass = class of TExternalTools;

function ExternalToolsRef: TExternalTools;


implementation

function ExternalToolsRef: TExternalTools;
begin
  Result := ExternalToolList as TExternalTools;
end;

{$IF defined(VerboseExtToolErrors) or defined(VerboseExtToolThread) or defined(VerboseExtToolAddOutputLines)}
function ArgsToString(Args: array of const): string;
var
  i: Integer;
begin
  Result := '';
  for i:=Low(Args) to High(Args) do begin
    case Args[i].VType of
      vtInteger:    Result := Result + dbgs(Args[i].vinteger);
      vtInt64:      Result := Result + dbgs(Args[i].VInt64^);
      vtQWord:      Result := Result + dbgs(Args[i].VQWord^);
      vtBoolean:    Result := Result + dbgs(Args[i].vboolean);
      vtExtended:   Result := Result + dbgs(Args[i].VExtended^);
      vtString:     Result := Result + Args[i].VString^;
      vtAnsiString: Result := Result + AnsiString(Args[i].VAnsiString);
      vtChar:       Result := Result + Args[i].VChar;
      vtPChar:      Result := Result + Args[i].VPChar;
      vtPWideChar:  Result := {%H-}Result {%H-}+ Args[i].VPWideChar;
      vtWideChar:   Result := Result + AnsiString(Args[i].VWideChar);
      vtWidestring: Result := Result + AnsiString(WideString(Args[i].VWideString));
      vtObject:     Result := Result + DbgSName(Args[i].VObject);
      vtClass:      Result := Result + DbgSName(Args[i].VClass);
      vtPointer:    Result := Result + Dbgs(Args[i].VPointer);
      else          Result := Result + '?unknown variant?';
    end;
  end;
end;

procedure DebuglnThreadLog(const args: array of const);
var
  s, Filename: string;
  fs: TFileStream;
begin
  if GetCurrentThreadId=MainThreadID then
    debugln(args)
  else
  begin
    s:=ArgsToString(args)+sLineBreak;
    Filename:='lazdbg'+IntToStr(GetCurrentThreadId)+'.log';
    if FileExistsUTF8(Filename) then
      fs:=TFileStream.Create(Filename,fmOpenWrite or fmShareDenyNone)
    else
      fs:=TFileStream.Create(Filename,fmCreate);
    try
      try
        fs.Seek(0,soEnd);
        fs.Write(s[1],length(s));
      except
      end;
    finally
      fs.Free;
    end;
  end;
end;
{$ENDIF}

{ TLazExtToolView }

procedure TLazExtToolView.SetToolState(AValue: TLMVToolState);
begin
  if FToolState=AValue then Exit;
  FToolState:=AValue;
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
    if (not Terminated) and (ErrorMessage='') then
    begin
      if ExitCode<>0 then
        ErrorMessage:=Format(lisExitCode, [IntToStr(ExitCode)])
      else if ExitStatus<>0 then
        ErrorMessage:='ExitStatus '+IntToStr(ExitStatus);
    end;
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
  NeedSynchronize, IsStdErr: Boolean;
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
      IsStdErr:=WorkerOutput.Objects[Line]<>nil;
      for i:=0 to ParserCount-1 do begin
        {$IFDEF VerboseExtToolAddOutputLines}
        DebuglnThreadLog(['TExternalTool.AddOutputLines ',DbgSName(Parsers[i]),' Line="',WorkerOutput[Line],'" READLINE ...']);
        {$ENDIF}
        Parsers[i].ReadLine(LineStr,Line,IsStdErr,Handled);
        if Handled then break;
      end;
      if (not Handled) then begin
        MsgLine:=WorkerMessages.CreateLine(Line);
        MsgLine.Msg:=LineStr; // use raw output as default msg
        MsgLine.Urgency:=mluDebug;
        if IsStdErr then
          MsgLine.Flags:=MsgLine.Flags+[mlfStdErr];
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
    else
      QueueAsyncAutoFree;
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
  if ConsoleVerbosity>=0 then begin
    debugln(['Info: (lazarus) Execute Title="',Title,'"']);
    debugln(['Info: (lazarus) Working Directory="',Process.CurrentDirectory,'"']);
    debugln(['Info: (lazarus) Executable="',Process.Executable,'"']);
    for i:=0 to Process.Parameters.Count-1 do
      debugln(['Info: (lazarus) Param[',i,']="',Process.Parameters[i],'"']);
  end;
  Thread.Start;
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

procedure TExternalTool.Notification(AComponent: TComponent; Operation: TOperation);
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
  MyTools: TExternalToolsBase;
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
    begin
      Assert(Owner is TExternalToolsBase, 'TExternalTool.WaitForExit: Owner is not TExternalToolsBase.');
      TExternalToolsBase(Owner).HandleMesages;
    end;
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
    LazMessageDialog(lisCCOErrorCaption, Format(lisInvalidMacrosInExternalTool,
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

function TExternalTools.RunExtToolHandler(ToolOptions: TIDEExternalToolOptions): boolean;
begin
  {$IFDEF VerboseExtToolThread}
  debugln(['TExternalTools.RunExtToolHandler ',ToolOptions.Title,
           ' exe="',ToolOptions.Executable,'" params="',ToolOptions.CmdLineParams,'"']);
  {$ENDIF}
  if ToolOptions.Parsers.Count=0 then
    Result := RunToolAndDetach(ToolOptions)
  else
    Result := RunToolWithParsers(ToolOptions)
end;

function TExternalTools.RunToolAndDetach(ToolOptions: TIDEExternalToolOptions): boolean;
// simply run and detach
var
  i: Integer;
  Proc: TProcessUTF8;
  sl: TStringList;
  s, Path: String;
begin
  Result:=false;
  Proc:=TProcessUTF8.Create(nil);
  try
    Proc.InheritHandles:=false;
    // working directory
    s:=ToolOptions.WorkingDirectory;
    if ToolOptions.ResolveMacros then begin
      if not GlobalMacroList.SubstituteStr(s) then begin
        debugln(['Error: (lazarus) [TExternalTools.RunToolAndDetach] ',ToolOptions.Title,' failed: macros of WorkerDirectory: "',ToolOptions.WorkingDirectory,'"']);
        exit;
      end;
    end;
    s:=ChompPathDelim(CleanAndExpandDirectory(s));
    if not DirectoryExistsUTF8(s) then begin
      debugln(['Error: (lazarus) [TExternalTools.RunToolAndDetach] ',ToolOptions.Title,' failed: missing directory "',s,'"']);
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
          debugln(['Error: (lazarus) [TExternalTools.RunToolAndDetach] ',ToolOptions.Title,' failed: environment override "',Proc.Environment,'"']);
          exit;
        end;
        Proc.Environment[i]:=s;
      end;
    end;

    // executable
    s:=ToolOptions.Executable;
    if ToolOptions.ResolveMacros then begin
      if not GlobalMacroList.SubstituteStr(s) then begin
        debugln(['Error: (lazarus) [TExternalTools.RunToolAndDetach] ',ToolOptions.Title,' failed: macros of Executable: "',ToolOptions.Executable,'"']);
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
                               Path, PathSeparator, sffFindProgramInPath);
      {$IFDEF Windows}
      if (s='') and (ExtractFileExt(s)='') then begin
        s:=SearchFileInPath(s+'.exe',Proc.CurrentDirectory,
                                 Path, PathSeparator,
                                 sffFindProgramInPath);
      end;
      {$ENDIF}
      if s='' then begin
        debugln(['Error: (lazarus) [TExternalTools.RunToolAndDetach] ',ToolOptions.Title,' failed: missing executable "',ToolOptions.Executable,'"']);
        exit;
      end;
    end;
    if not ( FilenameIsAbsolute(s) and FileExistsUTF8(s) ) then begin
      debugln(['Error: (lazarus) [TExternalTools.RunToolAndDetach] ',ToolOptions.Title,'  failed: missing executable: "',s,'"']);
      exit;
    end;
    if DirectoryExistsUTF8(s) {$IFDEF DARWIN}and (ExtractFileExt(s)<>'.app'){$ENDIF} then begin
      debugln(['Error: (lazarus) [TExternalTools.RunToolAndDetach] ',ToolOptions.Title,'  failed: executable is a directory: "',s,'"']);
      exit;
    end;
    if {$IFDEF DARWIN}(ExtractFileExt(s)<>'.app') and{$ENDIF} not FileIsExecutable(s) then begin
      debugln(['Error: (lazarus) [TExternalTools.RunToolAndDetach] ',ToolOptions.Title,'  failed: executable lacks permission to run: "',s,'"']);
      exit;
    end;

    {$IFDEF DARWIN}
    if DirectoryExistsUTF8(s) then
    begin
      Proc.Executable:='/usr/bin/open';
      s:=s+LineEnding+ToolOptions.CmdLineParams;
    end
    else
    {$ENDIF}
    begin
      Proc.Executable:=s;
      s:=ToolOptions.CmdLineParams;
    end;

    // params
    if ToolOptions.ResolveMacros and not GlobalMacroList.SubstituteStr(s) then begin
      debugln(['Error: (lazarus) [TExternalTools.RunToolAndDetach] ',ToolOptions.Title,
               ' failed: macros in cmd line params "',ToolOptions.CmdLineParams,'"']);
      exit;
    end;
    sl:=TStringList.Create;
    try
      SplitCmdLineParams(s,sl);
      Proc.Parameters:=sl;
    finally
      sl.Free;
    end;

    // run and detach
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
      Result:=true;
    except
    end;
  finally
    Proc.Free;
  end;
end;

function TExternalTools.RunToolWithParsers(ToolOptions: TIDEExternalToolOptions): boolean;
// run with parsers and messages
var
  Tool: TAbstractExternalTool;
  i: Integer;
begin
  {$IFDEF VerboseExtToolThread}
  debugln(['TExternalTools.RunToolWithParsers run with scanners ...']);
  {$ENDIF}
  Result:=false;
  Tool:=Add(ToolOptions.Title);
  Tool.Reference(Self,ClassName);
  try
    Tool.Hint:=ToolOptions.Hint;
    Tool.Process.CurrentDirectory:=ToolOptions.WorkingDirectory;
    Tool.Process.Executable:=ToolOptions.Executable;
    Tool.CmdLineParams:=ToolOptions.CmdLineParams;
    Tool.EnvironmentOverrides:=ToolOptions.EnvironmentOverrides;
    Assert(Assigned(ToolOptions.Parsers), 'TExternalTools.RunToolWithParsers: Parsers=Nil.');
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
    if ToolOptions.ResolveMacros and not Tool.ResolveMacros then begin
      debugln(['Error: (lazarus) [TExternalTools.RunToolWithParsers] failed to resolve macros']);
      exit;
    end;
    {$IFDEF VerboseExtToolThread}
    debugln(['TExternalTools.RunToolWithParsers Execute ',Tool.Title,' WD="',Tool.Process.CurrentDirectory,'" Exe="',Tool.Process.Executable,'" Params="',Tool.CmdLineParams,'" ...']);
    {$ENDIF}
    Tool.Execute;
    {$IFDEF VerboseExtToolThread}
    debugln(['TExternalTools.RunToolWithParsers WaitForExit ',Tool.Title,' ...']);
    {$ENDIF}
    Tool.WaitForExit;
    {$IFDEF VerboseExtToolThread}
    debugln(['TExternalTools.RunToolWithParsers Done ',Tool.Title]);
    {$ENDIF}
    Result:=(Tool.ErrorMessage='') and (not Tool.Terminated) and (Tool.ExitStatus=0);
  finally
    Tool.Release(Self);
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

procedure TExternalTools.Notification(AComponent: TComponent; Operation: TOperation);
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
  RunExternalTool := @RunExtToolHandler;
end;

destructor TExternalTools.Destroy;
begin
  RunExternalTool:=nil;
  TerminateAll;
  EnterCriticalSection;
  try
    if fRunning.Count>0 then
      raise Exception.Create('TExternalTools.Destroy some tools still running');
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
  Result:=FToolClass.Create(Self);
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
// terminate all current tools
var
  i: Integer;
begin
  for i:=Count-1 downto 0 do
  begin
    Assert(i<Count, 'TExternalTools.TerminateAll: xxx'); // if i>=Count then continue;  <- why was this?
    Terminate(Items[i] as TExternalTool);
  end;
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

function TExternalTools.FindParserForTool(const SubTool: string): TExtToolParserClass;
var
  i: Integer;
begin
  for i:=0 to fParsers.Count-1 do begin
    Result:=TExtToolParserClass(fParsers[i]);
    if Result.CanParseSubTool(SubTool) then exit;
  end;
  Result:=nil;
end;

function TExternalTools.FindParserWithName(const ParserName: string): TExtToolParserClass;
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

  function ReadInputPipe(aStream: TInputPipeStream; var LineBuf: string;
    IsStdErr: boolean): boolean;
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
        if IsStdErr then
          fLines.AddObject(LineBuf,fLines)
        else
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
          HasOutput:=ReadInputPipe(Tool.Process.Output,OutputLine,false)
                  or ReadInputPipe(Tool.Process.Stderr,StdErrLine,true);
        end else begin
          HasOutput:=ReadInputPipe(Tool.Process.Stderr,StdErrLine,true)
                  or ReadInputPipe(Tool.Process.Output,OutputLine,false);
        end;
        if (not HasOutput) then begin
          // no more pending output
          if not Tool.Process.Running then break;
        end;
        if (fLines.Count>0)
        and (Abs(int64(GetTickCount64)-LastUpdate)>UpdateTimeDiff) then begin
          {$IFDEF VerboseExtToolThread}
          DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' ',TimeToStr(Now),' ',IntToStr(GetTickCount64),' AddOutputLines ...']);
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
          Sleep(50);
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
        Tool.ExitCode:=Tool.Process.ExitCode;
        {$IFDEF VerboseExtToolThread}
        if Tool.ExitStatus<>0 then
          DebuglnThreadLog(['TExternalToolThread.Execute ',Title,' exit status=',Tool.ExitStatus,' ExitCode=',Tool.ExitCode]);
        {$ENDIF}
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

procedure TExternalToolThread.DebuglnThreadLog(const Args: array of const);
begin
  debugln(Args);
end;

destructor TExternalToolThread.Destroy;
begin
  Tool:=nil;
  inherited Destroy;
end;

end.

