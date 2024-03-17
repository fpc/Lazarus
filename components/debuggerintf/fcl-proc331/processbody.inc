{%MainUnit process.pp}
{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Type
   {$ifdef processunicodestring}
   TProcessString = Unicodestring;
   TprocessChar   = UnicodeChar;

   { TProcessStrings }

   TProcessStrings = Class(TPersistent)
                private
                   name   : array of unicodestring;
                   function getcount: Integer;
                   function getname( index: integer): Unicodestring;
                public
                   procedure AssignTo(Dest: TPersistent); override;
                   procedure add(const s : Unicodestring);
                   procedure Clear;
                   procedure Delete(i:integer);

                   property Names[ index:integer]:Unicodestring read getname; default;
                   property Count : Integer read getcount;
   end;
   TProcessStringList = TProcessStrings;
   {$else}
   TprocessChar   = Char;
   TProcessString = String;
   TProcessStrings = TStrings;
   TProcessStringList = TStringList;
   {$endif}

   TFileWriteMode = (fwmTruncate, fwmAppend, fwmAtstart);


   TIODescriptor = class(TPersistent)
   private
     FAfterAllocateHandle: TAfterAllocateHandleEvent;
     FCloseHandleOnExecute: Boolean;
     FCustomHandle: THandle;
     FAutoCloseCustomHandle: Boolean;
     FCustomHandleIsInheritable: Boolean;
     FFileWriteMode: TFileWriteMode;
     FHandleType: TProcessHandleType;
     FFileName: TFileName;
     FIOType: TIOType;
     FOnGetHandle: TGetHandleEvent;
     FOwnerProcess: TProcess;
     FPipeBufferSize: cardinal;
     FProcess: TProcess;
     FTheirHandle : THandle;
     FTheirHandleIOType: TIOType;
     FHandleValid : Boolean;
     FStream : THandleStream;
     FOurHandle : THandle;
     procedure SetFileName(AValue: TFileName);
     procedure SetFileWriteMode(AValue: TFileWriteMode);
     procedure SetIOType(AValue: TIOType);
     procedure SetProcess(AValue: TProcess);
     function SysCreatePipeHandle: THandle;
     function SysPrepareCreatedHandleForProcess(aHandle: THandle): THandle;
     Function SysCreateFileNameHandle(const aFileName : string) : THandle;
     function SysNullFileName : string;
     function SysIsTypeSupported(AValue: TIOType) : Boolean;
   protected
     Procedure CheckNotRunning; virtual;
     // Create handles for new process
     Function PrepareCreatedHandleForProcess(aHandle : THandle) : THandle; virtual;
     Function CreateStandardHandle : THandle; virtual;
     Function CreatePipeHandle : THandle; virtual;
     Function CreateFileNameHandle : THandle; virtual;
     Function CreateNullFileHandle : THandle; virtual;
     Function CreateCustomHandle : THandle; virtual;
     Function CreateProcessHandle : THandle; virtual;
     Function ResolveProcessHandle : THandle; virtual;
     Function ResolveStream : THandleStream; virtual;
     Procedure CloseOurHandle; virtual;
     Procedure CloseTheirHandle(aForceClose : Boolean = false);virtual;
     Procedure PrepareHandles;virtual;
     Procedure ResetHandles;virtual;
     Property OwnerProcess : TProcess Read FOwnerProcess;
     Property PipeBufferSize : cardinal read FPipeBufferSize write FPipeBufferSize;
     Property OurHandle: THandle Read FOurHandle;
     Property HandleValid : Boolean Read FHandleValid;
     Property CloseHandleOnExecute : Boolean Read FCloseHandleOnExecute Write FCloseHandleOnExecute;
   public
     Constructor Create(aOwnerProcess : TProcess; aType : TProcessHandleType);
     Destructor Destroy; override;
     Property ProcessHandleType : TProcessHandleType Read FHandleType;
     Property CustomHandle : THandle Read FCustomHandle Write FCustomHandle;
     Property AutoCloseCustomHandle: Boolean Read FAutoCloseCustomHandle Write FAutoCloseCustomHandle;
     Property CustomHandleIsInheritable: Boolean Read FCustomHandleIsInheritable Write FCustomHandleIsInheritable; platform;

   Published
     Property IOType : TIOType Read FIOType Write SetIOType;
     Property FileName : TFileName Read FFileName Write SetFileName;
     Property OnGetHandle : TGetHandleEvent Read FOnGetHandle Write FOnGetHandle;
     Property AfterAllocateHandle : TAfterAllocateHandleEvent Read FAfterAllocateHandle Write FAfterAllocateHandle;
     Property Process : TProcess Read FProcess Write SetProcess;
     Property FileWriteMode : TFileWriteMode Read FFileWriteMode Write SetFileWriteMode;
   end;


  { TProcess }

  TProcess = Class (TComponent)
  Private
    FOnRunCommandEvent: TOnRunCommandEvent;
    FProcessOptions : TProcessOptions;
    FRunCommandSleepTime: Integer;
    FStartupOptions : TStartupOptions;
    FFillAttribute : Cardinal;
    FApplicationName : TProcessString;
    FConsoleTitle : TProcessString;
    FCommandLine : TProcessString;
    FCurrentDirectory : TProcessString;
    FDesktop : String;
    FEnvironment : TProcessStrings;
    FExecutable : TProcessString;
    FParameters : TProcessStrings;
    FShowWindow : TShowWindowOptions;
    FInherithandles : Boolean;
    {$ifdef UNIX}
    FForkEvent : TProcessForkEvent;
    {$endif UNIX}
    FProcessPriority : TProcessPriority;
    dwXCountchars,
    dwXSize,
    dwYsize,
    dwx,
    dwYcountChars,
    dwy : Cardinal;
    FXTermProgram: String;
    FPipeBufferSize : cardinal;
    FDescriptors: Array [TProcessHandleType] of TIODescriptor;
    function GetDescriptor(AIndex: Integer): TIODescriptor;
    Function  GetExitStatus : Integer;
    Function  GetExitCode : Integer;
    function GetInputStream: TOutputPipeStream;
    function GetOutputStream: TInputPipeStream;
    Function  GetRunning : Boolean;
    function GetStderrStream: TinputPipeStream;
    Function  GetWindowRect : TRect;
    procedure SetCommandLine(const AValue: TProcessString); deprecated;
    procedure SetDescriptor(AIndex: Integer; AValue: TIODescriptor);
    procedure SetParameters(const AValue: TProcessStrings);
    procedure SetPipeBufferSize(AValue: cardinal);
    Procedure SetWindowRect (Value : TRect);
    Procedure SetShowWindow (Value : TShowWindowOptions);
    Procedure SetWindowColumns (Value : Cardinal);
    Procedure SetWindowHeight (Value : Cardinal);
    Procedure SetWindowLeft (Value : Cardinal);
    Procedure SetWindowRows (Value : Cardinal);
    Procedure SetWindowTop (Value : Cardinal);
    Procedure SetWindowWidth (Value : Cardinal);
    procedure SetApplicationName(const Value: TProcessString);
    procedure SetProcessOptions(const Value: TProcessOptions);
    procedure SetActive(const Value: Boolean);
    procedure SetEnvironment(const Value: TProcessStrings);
    Procedure ConvertCommandLine;
    function  PeekExitStatus: Boolean;
    Procedure IntOnIdleSleep(Sender,Context : TObject;Status:TRunCommandEventCode;const Message:String);
  Protected
    FRunning : Boolean;
    FExitCode : Cardinal;
    FProcessID : Integer;
    FThreadID : Integer;
    FProcessHandle : Thandle;
    FThreadHandle : Thandle;
    procedure CloseProcessHandles; virtual;
    procedure Loaded; override;
    Procedure SysExecute; virtual;
    function CreateIODescriptor(aOwner: TProcess; aHandleType: TProcessHandleType): TIODescriptor; virtual;
  Public
    Constructor Create (AOwner : TComponent);override;
    Destructor Destroy; override;
    Procedure Execute; virtual;
    procedure CloseInput; virtual;
    procedure CloseOutput; virtual;
    procedure CloseStderr; virtual;
    Function Resume : Integer; virtual;
    Function Suspend : Integer; virtual;
    Function Terminate (AExitCode : Integer): Boolean; virtual;
    Function WaitOnExit : Boolean;
    Function WaitOnExit(Timeout : DWord) : Boolean;
    function ReadInputStream(p:TInputPipeStream;var BytesRead:integer;var DataLength:integer;var Data:string;MaxLoops:integer=10):boolean; virtual;
    function ReadInputStream(p:TInputPipeStream;data:TStream;MaxLoops:integer=10):boolean; virtual;
    function RunCommandLoop(out outputstring:string;out stderrstring:string; out anexitstatus:integer):integer; virtual;

    Property WindowRect : Trect Read GetWindowRect Write SetWindowRect;
    Property Handle : THandle Read FProcessHandle;
    Property ProcessHandle : THandle Read FProcessHandle;
    Property ThreadHandle : THandle Read FProcessHandle;
    Property ProcessID : Integer Read FProcessID;
    Property ThreadID : Integer Read FThreadID;
    Property Input  : TOutputPipeStream Read GetInputStream;
    Property Output : TInputPipeStream  Read GetOutputStream;
    Property Stderr : TinputPipeStream  Read GetStderrStream;
    Property ExitStatus : Integer Read GetExitStatus;
    Property ExitCode : Integer Read GetExitCode;
    Property InheritHandles : Boolean Read FInheritHandles Write FInheritHandles;
    Property OnRunCommandEvent : TOnRunCommandEvent Read FOnRunCommandEvent Write FOnRunCommandEvent;
    Property RunCommandSleepTime : Integer read FRunCommandSleepTime write FRunCommandSleepTime;
    {$ifdef UNIX}
    property OnForkEvent : TProcessForkEvent Read FForkEvent Write FForkEvent;
    {$endif UNIX}
  Published
    property PipeBufferSize : cardinal read FPipeBufferSize write SetPipeBufferSize default 1024;
    Property Active : Boolean Read GetRunning Write SetActive;
    Property ApplicationName : TProcessString Read FApplicationName Write SetApplicationName; deprecated;
    Property CommandLine : TProcessString Read FCommandLine Write SetCommandLine ; deprecated;
    Property Executable : TProcessString Read FExecutable Write FExecutable;
    Property Parameters : TProcessStrings Read FParameters Write SetParameters;
    Property ConsoleTitle : TProcessString Read FConsoleTitle Write FConsoleTitle;
    Property CurrentDirectory : TProcessString Read FCurrentDirectory Write FCurrentDirectory;
    Property Desktop : String Read FDesktop Write FDesktop;
    Property Environment : TProcessStrings Read FEnvironment Write SetEnvironment;
    Property Options : TProcessOptions Read FProcessOptions Write SetProcessOptions;
    Property Priority : TProcessPriority Read FProcessPriority Write FProcessPriority;
    Property StartupOptions : TStartupOptions Read FStartupOptions Write FStartupOptions;
    Property Running : Boolean Read GetRunning;
    Property ShowWindow : TShowWindowOptions Read FShowWindow Write SetShowWindow;
    Property WindowColumns : Cardinal Read dwXCountChars Write SetWindowColumns;
    Property WindowHeight : Cardinal Read dwYSize Write SetWindowHeight;
    Property WindowLeft : Cardinal Read dwX Write SetWindowLeft;
    Property WindowRows : Cardinal Read dwYCountChars Write SetWindowRows;
    Property WindowTop : Cardinal Read dwY Write SetWindowTop ;
    Property WindowWidth : Cardinal Read dwXSize Write SetWindowWidth;
    Property FillAttribute : Cardinal read FFillAttribute Write FFillAttribute;
    Property XTermProgram : String Read FXTermProgram Write FXTermProgram;
    Property InputDescriptor : TIODescriptor index Ord(phtInput) Read GetDescriptor Write SetDescriptor;
    Property OutputDescriptor : TIODescriptor Index Ord(phtOutput) Read GetDescriptor Write SetDescriptor;
    Property ErrorDescriptor : TIODescriptor Index Ord(phtError) Read GetDescriptor Write SetDescriptor;
  end;

  TProcessClass = Class of TProcess;

Procedure CommandToList(S : TProcessString; List : TProcessStrings);

{$ifdef unix}
Var
  TryTerminals : Array of string;
  XTermProgram : String;
  SignalWaitTime : Integer = 20; // Wait time in ms. after sending SIGTERM
  Function DetectXTerm : String;
{$endif unix}

function RunCommandIndir(const curdir:TProcessString;const exename:TProcessString;const commands:array of TProcessString;out outputstring:string; out exitstatus:integer; Options : TProcessOptions = [];SWOptions:TShowWindowOptions=swoNone):integer;
function RunCommandIndir(const curdir:TProcessString;const exename:TProcessString;const commands:array of TProcessString;out outputstring:string; Options : TProcessOptions = [];SWOptions:TShowWindowOptions=swoNone):boolean;
function RunCommand(const exename:TProcessString;const commands:array of TProcessString;out outputstring:string; Options : TProcessOptions = [];SWOptions:TShowWindowOptions=swoNone):boolean;

function RunCommandInDir(const curdir,cmdline:TProcessString;out outputstring:string):boolean; deprecated;
function RunCommand(const cmdline:TProcessString;out outputstring:string):boolean; deprecated;

// Allows override of the class instantiated for RunCommand*.

var DefaultTProcess : TProcessClass = TProcess;

const
  SErrCannotCreatePipes = 'Failed to create pipes';

{$IFDEF LINUX}
  INVALID_HANDLE_VALUE = DWORD(-1);
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
{$i win_process.inc}
{$ELSE}
{$i unix_process.inc}
{$ENDIF}

{$IFNDEF SKIPHELPERS}
function TIOTypeHelper.ToString : string;

begin
  WriteStr(Result,Self);
end;

{ TProcessHandleTypeHelper }

function TProcessHandleTypeHelper.ToString: string;
begin
  WriteStr(Result,Self);
end;


{ TProcessOption }

function TProcessOptionHelper.ToString: String;
begin
  WriteStr(Result,Self);
end;

{ TShowWindowOptionsHelper }

function TShowWindowOptionsHelper.ToString: String;
begin
  WriteStr(Result,Self);
end;

{ TStartupOptionHelper }

function TStartupOptionHelper.ToString: String;
begin
  WriteStr(Result,Self);
end;

{ TProcessPriorityhelper }

function TProcessPriorityhelper.ToString: String;
begin
  WriteStr(Result,Self);
end;

{ TRunCommandEventCodeHelper }

function TRunCommandEventCodeHelper.ToString: string;
begin
  WriteStr(Result,Self);
end;

{$ENDIF SKIPHELPERS}

Procedure CommandToList(S : TProcessString; List : TProcessStrings);

  Function GetNextWord : TProcessString;

  Const
    WhiteSpace = [' ',#9,#10,#13];
    Literals = ['"',''''];

  Var
    Wstart,wend : Integer;
    InLiteral : Boolean;
    LastLiteral : TProcessChar;

  begin
    WStart:=1;
    While (WStart<=Length(S)) and charinset(S[WStart],WhiteSpace) do
      Inc(WStart);
    WEnd:=WStart;
    InLiteral:=False;
    LastLiteral:=#0;
    While (Wend<=Length(S)) and (Not charinset(S[Wend],WhiteSpace) or InLiteral) do
      begin
      if charinset(S[Wend],Literals) then
        If InLiteral then
          InLiteral:=Not (S[Wend]=LastLiteral)
        else
          begin
          InLiteral:=True;
          LastLiteral:=S[Wend];
          end;
       inc(wend);
       end;

     Result:=Copy(S,WStart,WEnd-WStart);

     if  (Length(Result) > 0)
     and (Result[1] = Result[Length(Result)]) // if 1st AnsiChar = last AnsiChar and..
     and (Result[1] in Literals) then // it's one of the literals, then
       Result:=Copy(Result, 2, Length(Result) - 2); //delete the 2 (but not others in it)

     While (WEnd<=Length(S)) and (S[Wend] in WhiteSpace) do
       inc(Wend);
     Delete(S,1,WEnd-1);

  end;

Var
  W : TProcessString;

begin
  While Length(S)>0 do
    begin
    W:=GetNextWord;
    If (W<>'') then
      List.Add(W);
    end;
end;

Constructor TProcess.Create (AOwner : TComponent);

Var
  HT : TProcessHandleType;

begin
  Inherited;
  FProcessPriority:=ppNormal;
  FShowWindow:=swoNone;
  FInheritHandles:=True;
  {$ifdef UNIX}
  FForkEvent:=nil;
  {$endif UNIX}
  FPipeBufferSize := 1024;
  FEnvironment:=TProcessStringList.Create;
  FParameters:=TProcessStringList.Create;
  FRunCommandSleepTime:=100;
  FOnRunCommandEvent:=@IntOnIdleSleep;
  For HT in TProcessHandleType do
    FDescriptors[HT]:=CreateIODescriptor(Self,HT)
end;

Destructor TProcess.Destroy;

Var
  HT : TProcessHandleType;

begin
  FParameters.Free;
  FEnvironment.Free;
  CloseProcessHandles;
  For HT in TProcessHandleType do
    FreeAndNil(FDescriptors[HT]);
  Inherited Destroy;
end;

Function TProcess.CreateIODescriptor(aOwner : TProcess; aHandleType : TProcessHandleType) :TIODescriptor;

begin
  Result:=TIODescriptor.Create(aOwner,aHandleType);
end;

function TProcess.GetDescriptor(AIndex: Integer): TIODescriptor;
begin
  Result:=FDescriptors[TProcessHandleType(aIndex)];
end;

function TProcess.GetInputStream: TOutputPipeStream;
begin
  Result:=FDescriptors[phtInput].ResolveStream as TOutputPipeStream;
end;

function TProcess.GetOutputStream: TInputPipeStream;
begin
  Result:=FDescriptors[phtOutput].ResolveStream as TInputPipeStream;
end;



Function TProcess.GetExitStatus : Integer;

begin
  GetRunning;
  Result:=Integer(FExitCode);
end;

{$IFNDEF OS_HASEXITCODE}
Function TProcess.GetExitCode : Integer;

begin
  if Not Running then
    Result:=GetExitStatus
  else
    Result:=0
end;
{$ENDIF}

Function TProcess.GetRunning : Boolean;

begin
  IF FRunning then
    FRunning:=Not PeekExitStatus;
  Result:=FRunning;
end;

function TProcess.GetStderrStream: TinputPipeStream;
begin
  Result:=FDescriptors[phtError].ResolveStream as TInputPipeStream;
end;

procedure TProcess.Loaded;
begin
  inherited Loaded;
  If (csDesigning in ComponentState) and (FCommandLine<>'') then
    ConvertCommandLine;
end;

Procedure TProcess.Execute;

Var
  HT : TProcessHandleType;

begin
  for HT in TProcessHandleType do
    FDescriptors[HT].ResetHandles;
  SysExecute;
end;


procedure TProcess.CloseInput;
begin
  FDescriptors[phtInput].CloseOurHandle;
end;

procedure TProcess.CloseOutput;
begin
  FDescriptors[phtOutput].CloseOurHandle;
end;

procedure TProcess.CloseStderr;
begin
  FDescriptors[phtError].CloseOurHandle;
end;

Procedure TProcess.SetWindowColumns (Value : Cardinal);

begin
  if Value<>0 then
    Include(FStartupOptions,suoUseCountChars);
  dwXCountChars:=Value;
end;


Procedure TProcess.SetWindowHeight (Value : Cardinal);

begin
  if Value<>0 then
    include(FStartupOptions,suoUsePosition);
  dwYSize:=Value;
end;

Procedure TProcess.SetWindowLeft (Value : Cardinal);

begin
  if Value<>0 then
    Include(FStartupOptions,suoUseSize);
  dwx:=Value;
end;

Procedure TProcess.SetWindowTop (Value : Cardinal);

begin
  if Value<>0 then
    Include(FStartupOptions,suoUsePosition);
  dwy:=Value;
end;

Procedure TProcess.SetWindowWidth (Value : Cardinal);
begin
  If (Value<>0) then
    Include(FStartupOptions,suoUseSize);
  dwXSize:=Value;
end;

Function TProcess.GetWindowRect : TRect;
begin
  With Result do
    begin
    Left:=dwx;
    Right:=dwx+dwxSize;
    Top:=dwy;
    Bottom:=dwy+dwysize;
    end;
end;

procedure TProcess.SetCommandLine(const AValue: TProcessString);
begin
  if FCommandLine=AValue then exit;
  FCommandLine:=AValue;
  If Not (csLoading in ComponentState) then
    ConvertCommandLine;
end;

procedure TProcess.SetDescriptor(AIndex: Integer; AValue: TIODescriptor);
begin
  FDescriptors[TProcessHandleType(aIndex)].Assign(AValue);
end;

procedure TProcess.SetParameters(const AValue: TProcessStrings);
begin
  FParameters.Assign(AValue);
end;

procedure TProcess.SetPipeBufferSize(AValue: cardinal);
var
  HT: TProcessHandleType;
begin
  if FPipeBufferSize = AValue then Exit;
  FPipeBufferSize := AValue;
  for HT in TProcessHandleType do
    FDescriptors[HT].PipeBufferSize:=AValue;
end;

Procedure TProcess.SetWindowRect (Value : Trect);
begin
  Include(FStartupOptions,suoUseSize);
  Include(FStartupOptions,suoUsePosition);
  With Value do
    begin
    dwx:=Left;
    dwxSize:=Right-Left;
    dwy:=Top;
    dwySize:=Bottom-top;
    end;
end;


Procedure TProcess.SetWindowRows (Value : Cardinal);

begin
  if Value<>0 then
    Include(FStartupOptions,suoUseCountChars);
  dwYCountChars:=Value;
end;

procedure TProcess.SetApplicationName(const Value: TProcessString);
begin
  FApplicationName := Value;
  If (csDesigning in ComponentState) and
     (FCommandLine='') then
    FCommandLine:=Value;
end;

procedure TProcess.SetProcessOptions(const Value: TProcessOptions);

var
  HT : TProcessHandleType;

begin
  FProcessOptions := Value;
  If poNewConsole in FProcessOptions then
    Exclude(FProcessOptions,poNoConsole);
  if poRunSuspended in FProcessOptions then
    Exclude(FProcessOptions,poWaitOnExit);
  if poUsePipes in FProcessOptions then
    for HT in TProcessHandleType do
      FDescriptors[HT].IOType:=iotPipe;
  if poStderrToOutPut in FProcessOptions then
    FDescriptors[phtError].IOType:=iotDefault;
  if poPassInput in FProcessOptions then
    FDescriptors[phtInput].IOType:=iotDefault;
end;

procedure TProcess.SetActive(const Value: Boolean);
begin
  if (Value<>GetRunning) then
    If Value then
      Execute
    else
      Terminate(0);
end;

procedure TProcess.SetEnvironment(const Value: TProcessStrings);
begin
  FEnvironment.Assign(Value);
end;

procedure TProcess.ConvertCommandLine;
begin
  FParameters.Clear;
  CommandToList(FCommandLine,FParameters);
  If FParameters.Count>0 then
    begin
    Executable:=FParameters[0];
    FParameters.Delete(0);
    end;
end;

Const
  READ_BYTES = 65536; // not too small to avoid fragmentation when reading large files.

function TProcess.ReadInputStream(p:TInputPipeStream;var BytesRead:integer;var DataLength:integer;var data:string;MaxLoops:integer=10):boolean;
var Available, NumBytes: integer;
begin
    Available:=P.NumBytesAvailable;
    result:=Available>0;
    if not result then
     exit;
    while (available > 0) and (MaxLoops>0) do
      begin
        if (BytesRead + available > DataLength) then
          begin
            DataLength:=BytesRead + max(READ_BYTES,available);
            Setlength(Data,DataLength);
          end;
        NumBytes := p.Read(data[1+BytesRead], Available);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes);
        Available:=P.NumBytesAvailable;
        dec(MaxLoops);
      end;
end;

function TProcess.ReadInputStream(p:TInputPipeStream;data:TStream;MaxLoops:integer=10):boolean;
const
  BufSize = 4096;
var
  Buffer: array[0..BufSize - 1] of byte;
  ReadBytes: integer;
  Available : integer;
begin
  Available:=P.NumBytesAvailable;
  result:=Available>0;
  if not result then
    Exit;
  while (available > 0) and (MaxLoops>0) do
  begin
    ReadBytes := Output.Read({%H-}Buffer, min(BufSize,Available));
    data.Write(Buffer, ReadBytes);
    Available:=P.NumBytesAvailable;
    dec(MaxLoops);
  end;
end;

procedure TProcess.IntOnIdleSleep(Sender,Context : TObject;status:TRunCommandEventCode;const message:string);
begin
  if status=RunCommandIdle then
    sleep(FRunCommandSleepTime);
end;

// helperfunction that does the bulk of the work.
// We need to also collect stderr output in order to avoid
// lock out if the stderr pipe is full.
function TProcess.RunCommandLoop(out outputstring:string;
                            out stderrstring:string; out anexitstatus:integer):integer;
var
    bytesread : integer;
    outputlength, stderrlength : integer;
    stderrbytesread : integer;
    gotoutput,gotoutputstderr : boolean;
begin
  result:=-1;
    try
    Options := Options + [poUsePipes];
    bytesread:=0;
    outputlength:=0;
    stderrbytesread:=0;
    stderrlength:=0;
    Execute;
    while Running do
      begin
        // Only call ReadFromStream if Data from corresponding stream
        // is already available, otherwise, on  linux, the read call
        // is blocking, and thus it is not possible to be sure to handle
        // big data amounts bboth on output and stderr pipes. PM.
        gotoutput:=ReadInputStream(output,BytesRead,OutputLength,OutputString,1);
        // The check for assigned(P.stderr) is mainly here so that
        // if we use poStderrToOutput in p.Options, we do not access invalid memory.
        gotoutputstderr:=false;
        if assigned(stderr) then
            gotoutputstderr:=ReadInputStream(StdErr,StdErrBytesRead,StdErrLength,StdErrString,1);
 
        if (porunidle in options) and not gotoutput and not gotoutputstderr and Assigned(FOnRunCommandEvent) Then
          FOnRunCommandEvent(self,Nil,RunCommandIdle,'');
      end;
    // Get left output after end of execution
    ReadInputStream(output,BytesRead,OutputLength,OutputString,250);
    setlength(outputstring,BytesRead);
    if assigned(stderr) then
      ReadInputStream(StdErr,StdErrBytesRead,StdErrLength,StdErrString,250);
    setlength(stderrstring,StderrBytesRead);
    anexitstatus:=exitstatus;
    result:=0; // we came to here, document that.
    if Assigned(FOnRunCommandEvent) then          // allow external apps to react to that and finish GUI
      FOnRunCommandEvent(self,Nil,RunCommandFinished,'');

    except
      on e : Exception do
         begin
           result:=1;
           setlength(outputstring,BytesRead);
           setlength(stderrstring,StderrBytesRead);
           if Assigned(FOnRunCommandEvent) then
             FOnRunCommandEvent(self,Nil,RunCommandException,e.Message);
         end;
     end;
end;

{ Functions without StderrString }

Const
  ForbiddenOptions = [poRunSuspended,poWaitOnExit];

function RunCommandIndir(const curdir:TProcessString;const exename:TProcessString;const commands:array of TProcessString;out outputstring:string;out exitstatus:integer; Options : TProcessOptions = [];SWOptions:TShowWindowOptions=swoNone):integer;
Var
    p : TProcess;
    i : integer;
    ErrorString : String;
begin
  p:=DefaultTProcess.create(nil);
  if Options<>[] then
    P.Options:=Options - ForbiddenOptions;
  P.ShowWindow:=SwOptions;
  p.Executable:=exename;
  if curdir<>'' then
    p.CurrentDirectory:=curdir;
  if high(commands)>=0 then
   for i:=low(commands) to high(commands) do
     p.Parameters.add(commands[i]);
  try
    result:=p.RunCommandLoop(outputstring,errorstring,exitstatus);
  finally
    p.free;
  end;
end;

function RunCommandInDir(const curdir,cmdline:TProcessString;out outputstring:string):boolean; deprecated;
Var
    p : TProcess;
    exitstatus : integer;
    ErrorString : String;
begin
  p:=DefaultTProcess.create(nil);
  p.setcommandline(cmdline);
  if curdir<>'' then
    p.CurrentDirectory:=curdir;
  try
    result:=p.RunCommandLoop(outputstring,errorstring,exitstatus)=0;
  finally
    p.free;
  end;
  if exitstatus<>0 then result:=false;
end;

function RunCommandIndir(const curdir:TProcessString;const exename:TProcessString;const commands:array of TProcessString;out outputstring:string; Options : TProcessOptions = [];SWOptions:TShowWindowOptions=swoNone):boolean;
Var
    p : TProcess;
    i,
    exitstatus : integer;
    ErrorString : String;
begin
  p:=DefaultTProcess.create(nil);
  if Options<>[] then
    P.Options:=Options - ForbiddenOptions;
  P.ShowWindow:=SwOptions;
  p.Executable:=exename;
  if curdir<>'' then
    p.CurrentDirectory:=curdir;
  if high(commands)>=0 then
   for i:=low(commands) to high(commands) do
     p.Parameters.add(commands[i]);
  try
    result:=p.RunCommandLoop(outputstring,errorstring,exitstatus)=0;
  finally
    p.free;
  end;
  if exitstatus<>0 then result:=false;
end;

function RunCommand(const cmdline:TProcessString;out outputstring:String):boolean; deprecated;
Var
    p : TProcess;
    exitstatus : integer;
    ErrorString : String;
begin
  p:=DefaultTProcess.create(nil);
  p.setcommandline(cmdline);
  try
    result:=p.RunCommandLoop(outputstring,errorstring,exitstatus)=0;
  finally
    p.free;
  end;
  if exitstatus<>0 then result:=false;
end;

function RunCommand(const exename:TProcessString;const commands:array of TProcessString;out outputstring:string; Options : TProcessOptions = [];SWOptions:TShowWindowOptions=swoNone):boolean;
Var
    p : TProcess;
    i,
    exitstatus : integer;
    ErrorString : String;
begin
  p:=DefaultTProcess.create(nil);
  if Options<>[] then
    P.Options:=Options - ForbiddenOptions;
  P.ShowWindow:=SwOptions;
  p.Executable:=exename;
  if high(commands)>=0 then
   for i:=low(commands) to high(commands) do
     p.Parameters.add(commands[i]);
  try
    result:=p.RunCommandLoop(outputstring,errorstring,exitstatus)=0;
  finally
    p.free;
  end;
  if exitstatus<>0 then result:=false;
end;

{$ifdef processunicodestring}
// dummy subset of tstrings.
{ TProcessStrings }

function TProcessStrings.getname( index: integer): Unicodestring;
begin
  if index<length(name) then
     result:=name[index]
  else
     result:='';
end;

function TProcessStrings.getcount: Integer;
begin
  result:=length(name);
end;

procedure TProcessStrings.AssignTo(Dest: TPersistent);
var i : integer;
begin
  inherited assign(dest);
  if dest is TStrings then
    begin
      setlength(name,tstrings(dest).count);
      for i:=0 to length(name)-1 do
        name[i]:=tstrings(dest)[i];
    end;
  if dest is tprocessstrings then
     name:=copy(tprocessstrings(dest).name);
end;

procedure TProcessStrings.add(const s: Unicodestring);
var len : integer;
begin
  len:=length(name);
  setlength(name, len+1);
  name[len]:=s;
end;

procedure TProcessStrings.Clear;
begin
 setlength(name,0);
end;

procedure TProcessStrings.Delete(i: integer);
var len,j : integer;
begin
  len:=length(name);
  if len=0 then exit;
  if (i<>len-1) and (len<>1) then
     begin
       for j:=i+1 to len-1 do
         name[j-1]:=name[j];
       setlength(name,len-1)
     end
  else
    setlength(name,len-1)
end;
{$endif}

{ TIODescriptor }

procedure TIODescriptor.SetFileName(AValue: TFileName);
begin
  if FileName=AValue then Exit;
  CheckNotRunning;
  FFileName:=AValue;
  if aValue<>'' then
    FIOType:=iotFile;
end;

procedure TIODescriptor.SetFileWriteMode(AValue: TFileWriteMode);
begin
  if FFileWriteMode=AValue then Exit;
  CheckNotRunning;
  FFileWriteMode:=AValue;
end;

procedure TIODescriptor.SetIOType(AValue: TIOType);

var
  S : String;

begin
  if FIOType=AValue then Exit;
  CheckNotRunning;
  if not SysIsTypeSupported(aValue) then
    begin
    WriteStr(S,aValue);
    Raise EProcess.CreateFmt('I/O Type "%s" not supported on this platform',[S]);
    end;
  FIOType:=AValue;
  // Some cleanup
  if aValue<>iotProcess then
    FProcess:=Nil;
  if aValue<>iotFile then
    FFileName:='';
end;

procedure TIODescriptor.SetProcess(AValue: TProcess);

begin
  if FProcess=AValue then Exit;
  CheckNotRunning;
  if (FOwnerProcess=FProcess) then
    Raise EProcess.Create('Remote process cannot refer to self process');
  if Assigned(FOwnerProcess) and Assigned(FProcess) then
    FProcess.RemoveComponent(FOwnerProcess);
  if (aValue<>Nil) then
    FIOType:=iotProcess;
  FProcess:=AValue;
  if Assigned(FOwnerProcess) and Assigned(FProcess) then
    FProcess.RemoveComponent(FOwnerProcess);
  if Self.ProcessHandleType=phtInput then
    FProcess.OutputDescriptor.IOType:=iotPipe
  else
    FProcess.InputDescriptor.IOType:=iotPipe;
end;


procedure TIODescriptor.CheckNotRunning;
begin
  If Assigned(FOwnerProcess) then
    if FOwnerProcess.Active then
       Raise EProcess.Create('Cannot perform operation while process is running');
end;

function TIODescriptor.PrepareCreatedHandleForProcess(aHandle: THandle): THandle;
begin
  Result:=SysPrepareCreatedHandleForProcess(aHandle);
end;

Function TIODescriptor.CreateStandardHandle : THandle;

begin
  case ProcessHandleType of
    phtInput:  Result:=StdInputHandle;
    phtOutput:  Result:=StdOutputHandle;
    phtError:  Result:=StdErrorHandle;
  end;
end;

Function TIODescriptor.CreatePipeHandle : THandle;

begin
  Result:=SysCreatePipeHandle;
end;

{$IFNDEF OS_HASCREATEPIPE}
Function TIODescriptor.SysCreatePipeHandle : THandle;

var
  HIn,HOut : Thandle;
begin
  HIn:=THandle(INVALID_HANDLE_VALUE);
  HOut:=HIn;
  If not CreatePipeHandles(Hin,HOut) then
    Raise EProcess.Create(SErrCannotCreatePipes);
  case ProcessHandleType of
    phtInput:
      begin
      Result:=HIn;
      FOurHandle:=hOut;
      end;
    phtOutput,
    phtError:
      begin
      Result:=hOut;
      FOurHandle:=HIn;
      end;
  end;
end;
{$ENDIF}

Function TIODescriptor.CreateFileNameHandle : THandle;

begin
  Result:=SysCreateFileNameHandle(FileName);
  if (ProcessHandleType<>phtInput) then
    case FFileWriteMode of
      fwmAtstart: ;
      fwmTruncate : FileTruncate(Result,0);
      fwmAppend : FileSeek(Result,0,soFromEnd);
    end;
end;

function TIODescriptor.CreateNullFileHandle: THandle;
begin
  Result:=SysCreateFileNameHandle(SysNullFileName);
end;

Function TIODescriptor.CreateCustomHandle : THandle;

begin
  Result:=FCustomHandle;
  if Assigned(FOnGetHandle) then
    FOnGetHandle(Self,Result,FCloseHandleOnExecute);
  if FCustomHandle=THandle(INVALID_HANDLE_VALUE) then
    Raise EProcess.Create('Cannot get custom handle. No handle set');
end;

Function TIODescriptor.CreateProcessHandle : THandle;

begin
  if Not Assigned(Process) then
    Raise EProcess.Create('Cannot get handle. No process assigned');
  case ProcessHandleType of
    phtInput:  Result:=Process.OutputDescriptor.OurHandle;
    phtOutput:  Result:=Process.InputDescriptor.OurHandle;
    phtError:  Result:=Process.InputDescriptor.OurHandle;
  end;
  if Result=THandle(INVALID_HANDLE_VALUE) then
    Raise EProcess.Create('Cannot get handle. Process not active');
end;

function TIODescriptor.ResolveStream: THandleStream;
begin
  if (FStream=Nil) and (FHandleValid) and (FTheirHandleIOType=iotPipe) then
    begin
    // Writeln(ProcessHandleType,' creating stream for stream ',IOType,': ',OurHandle);
    Case FHandleType of
      phtInput : FStream:=TOutputPipeStream.Create(OurHandle);
      phtError,
      phtOutput : FStream:=TInputPipeStream.Create(OurHandle);
    end;
    end;
  FOurHandle:=THandle(INVALID_HANDLE_VALUE);
  Result:=FStream;
end;


procedure TIODescriptor.CloseOurHandle;

var
  H : THandle;

begin
  if Not FHandleValid then
     exit;
  H:=OurHandle;
  // Writeln(StdErr, GetProcessID ,' : ',ProcessHandleType,' closing our handle ',IOType,': ',FOurHandle);
  FOurHandle:=THandle(INVALID_HANDLE_VALUE) ;
  if H<>THandle(INVALID_HANDLE_VALUE) then
    FileClose(H);
end;

procedure TIODescriptor.CloseTheirHandle(aForceClose: Boolean);
var
  H : THandle;

begin
  if Not FHandleValid then
     exit;
  If (FTheirHandleIOType=iotDefault) or not (CloseHandleOnExecute or aForceClose) then
    begin
    FTheirHandle:=THandle(INVALID_HANDLE_VALUE);
    exit;
    end;
  H:=ResolveProcessHandle;
  // Writeln(StdErr,GetProcessID,' : ',ProcessHandleType,' closing their handle ',IOType,': ',H);
  FTheirHandle:=THandle(INVALID_HANDLE_VALUE);
  if H<>THandle(INVALID_HANDLE_VALUE) then
    begin
    FileClose(H);
    end;
end;

procedure TIODescriptor.PrepareHandles;

var
  H : THandle;
  S : String;

begin
  WriteStr(S,IOType);
  H:=ResolveProcessHandle;
  // Writeln('PReparing handle ',S,' : ',H,' (ours: ',OurHandle,')');
  if H=THandle(INVALID_HANDLE_VALUE) then
     Raise EProcess.CreateFmt('Failed to prepare process handle for %s',[S]);
end;

procedure TIODescriptor.ResetHandles;

begin
  CloseOurHandle;
  CloseTheirHandle(True);
  FreeAndNil(FStream);
  FHandleValid:=False;
end;



function TIODescriptor.ResolveProcessHandle: THandle;

var
  H : THandle;

begin
  if not FHandleValid then
    begin
    FTheirHandleIOType := IOType;
    FOurHandle:=THAndle(INVALID_HANDLE_VALUE);
    FCloseHandleOnExecute:=(IOType<>iotDefault);
    if IOType = iotHandle then
      FCloseHandleOnExecute:=FAutoCloseCustomHandle;
    Case IOType of
      iotDefault : H:=CreateStandardHandle;
      iotPipe : H:=CreatePipeHandle;
      iotFile : H:=CreateFileNameHandle;
      iotProcess : H:=CreateProcessHandle;
      iotHandle : H:=CreateCustomHandle;
      iotNull : H:=CreateNullFileHandle;
    end;
    FTheirHandle:=PrepareCreatedHandleForProcess(H);
    if Assigned(FAfterAllocateHandle) then
      FAfterAllocateHandle(Self,FTheirHandle,FCloseHandleOnExecute);
    FHandleValid:=True;
    end;
  Result:=FTheirHandle;
end;

constructor TIODescriptor.Create(aOwnerProcess: TProcess; aType: TProcessHandleType);
begin
  FOwnerProcess:=aOwnerProcess;
  FHandleType:=aType;
  FCustomHandle:=THandle(INVALID_HANDLE_VALUE);
  FTheirHandle:=THandle(INVALID_HANDLE_VALUE);
  FOurHandle:=THandle(INVALID_HANDLE_VALUE);
  FPipeBufferSize := 1024;
  FAutoCloseCustomHandle := True;
end;

destructor TIODescriptor.Destroy;
begin
  FreeAndNil(FStream);
  ResetHandles;
  inherited Destroy;
end;

end.
