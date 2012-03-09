{
 /***************************************************************************
                        outputfilter.pas  -  Lazarus IDE unit
                        -------------------------------------
             TOutputFilter is responsible for parsing output of external
             tools and to filter important messages.

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit OutputFilter;

{$mode objfpc}
{$H+}

{off $DEFINE VerboseOFExecute}

interface

uses
  Classes, Math, SysUtils, Forms, Controls, Dialogs, CompilerOptions,
  AsyncProcess, LCLProc, DynQueue, FileUtil, UTF8Process,
  CodeCache, CodeToolManager,
  IDEDialogs, IDEMsgIntf, IDEExternToolIntf,
  IDEProcs, LazConf;

type

  { TMessageScanners }

  TMessageScanners = class(TIDEMsgScanners)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TOnOutputString = procedure(Line: TIDEScanMessageLine) of object;
  TOnAddFilteredLine = procedure(const Msg, Directory: String;
                             OriginalIndex: integer; Parts: TStrings) of object;
  TOnGetIncludePath = function(const Directory: string;
                               UseCache: boolean): string of object;

  TOuputFilterOption = (
    ofoShowAll,              // don't filter
    ofoSearchForFPCMessages, // scan for freepascal compiler messages
    ofoSearchForMakeMessages,// scan for make/gmake messages
    ofoExceptionOnError,     // raise exception on panic, fatal errors
    ofoMakeFilenamesAbsolute // convert relative filenames to absolute ones
    );
  TOuputFilterOptions = set of TOuputFilterOption;
  
  TOutputMessageType = (omtNone, omtFPC, omtLinker, omtMake);

  { TFilteredOutputLines
    A TStringList maintaining an original index for each string.
    TOutputFilter creates an instance of this class as a result of filtering
    output.
  }
  TFilteredOutputLines = class(TStringList)
  private
    FOriginalIndices: PInteger;
    function GetOriginalIndices(Index: integer): integer;
    procedure SetOriginalIndices(Index: integer; const AValue: integer);
  protected
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure InsertItem(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    property OriginalIndices[Index: integer]: integer read GetOriginalIndices write SetOriginalIndices;
  end;
  
  TOFOnEndReading = procedure(Sender: TObject; Lines: TIDEMessageLineList)
                              of object;

  TOutputFilter = class;

  { TOFScanLine }

  TOFScanLine = class(TIDEScanMessageLine)
  private
    FFilter: TOutputFilter;
  public
    constructor Create(TheFilter: TOutputFilter);
    procedure Init(const aLine, aWorkDir: string; const aLineNumber: integer);
    procedure LineChanged(const OldValue: string); override;
    procedure WorkingDirectoryChanged(const OldValue: string); override;
    property Filter: TOutputFilter read FFilter;
  end;

  TOutputFilterState = (ofsNone, ofsRunning, ofsSucceded, ofsFailed, ofsAborted);

  { TOutputFilter }

  TOutputFilter = class
  private
    FAsyncDataAvailable: boolean;
    FAsyncProcessTerminated: boolean;
    FLastAsyncExecuteTime: TDateTime;
    FCaller: TObject;
    FFinishedCallback: TNotifyEvent;
    FCompilerOptions: TBaseCompilerOptions;
    FBufferingOutputLock: integer;
    fCurrentDirectory: string;
    fFilteredOutput: TFilteredOutputLines;
    FOnBeginUpdate: TNotifyEvent;
    FOnEndReading: TOFOnEndReading;
    FOnEndUpdate: TNotifyEvent;
    fOnReadLine: TOnOutputString;
    fOutput: TIDEMessageLineList;
    fLastErrorType: TFPCErrorType;
    fLastMessageType: TOutputMessageType;
    fCompilingHistory: TStringList;
    fMakeDirHistory: TStringList;
    fOnGetIncludePath: TOnGetIncludePath;
    fOnAddFilteredLine: TOnAddFilteredLine;
    fOptions: TOuputFilterOptions;
    FScanLine: TOFScanLine;
    FState: TOutputFilterState;
    FHasReadErrorLine: Boolean;
    FHasRaisedException: boolean;
    FStopExecute: boolean;
    FLasTOutputLineParts: integer;
    fLastOutputTime: TDateTime;
    fLastSearchedShortIncFilename: string;
    fLastSearchedIncFilename: string;
    fProcess: TProcessUTF8;
    FAsyncProcess: TAsyncProcess;
    FAsyncOutput: TDynamicDataQueue;
    FScanners: TFPList; // list of TIDEMsgScanner
    FTool: TIDEExternalToolOptions;
    DarwinLinkerMultiline: Boolean;
    DarwinLinkerLine : String;
    FErrorNames : array [TFPCErrorType] of string; {customizable error names}
    fLastBuffer: TCodeBuffer;
    procedure DoAddFilteredLine(const s: string; OriginalIndex: integer = -1);
    procedure DoAddLastLinkerMessages(SkipLastLine: boolean);
    procedure DoAddLastAssemblerMessages;
    function GetCurrentMessageParts: TStrings;
    function GetScanners(Index: integer): TIDEMsgScanner;
    function GetScannerCount: integer;
    function SearchIncludeFile(const ShortIncFilename: string): string;
    procedure SetStopExecute(const AValue: boolean);
    procedure InternalSetCurrentDirectory(const Dir: string);
    procedure OnAsyncTerminate(Sender: TObject);
    procedure OnAsyncReadData(Sender: TObject);
    function CreateScanners(ScannerOptions: TStrings): boolean;
    procedure ClearScanners;
    procedure InitExecute;
    procedure CleanUpExecute;
    procedure ContinueAsyncExecute(Data: PtrInt);
  protected
    procedure SetErrorName(errtype: TFPCErrorType; const AValue: String );
    function GetErrorName(errtype: TFPCErrorType): string;
  public
    ErrorExists: boolean;
    Aborted: boolean;
    function Execute(TheProcess: TProcessUTF8; aCaller: TObject = nil;
                     aTool: TIDEExternalToolOptions = nil): boolean;
    function ExecuteAsyncron(TheProcess: TProcessUTF8;
                             aFinishedCallback: TNotifyEvent;
                             aCaller: TObject = nil;
                             aTool: TIDEExternalToolOptions = nil): boolean;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    function IsHintForUnusedUnit(const OutputLine,
                                 MainSrcFile: string): boolean;
    function IsHintForParameterSenderNotUsed(const OutputLine: string): boolean;
    function IsParsing: boolean;
    procedure ReadLine(var s: string; DontFilterLine: boolean);
    procedure ReadConstLine(const s: string; DontFilterLine: boolean);
    function ReadFPCompilerLine(const s: string): boolean;
    function ReadMakeLine(const s: string): boolean;
    procedure WriteOutput(Flush: boolean);
    procedure BeginBufferingOutput;
    procedure EndBufferingOutput;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure RaiseOutputFilterError(const Msg: string);
    function HasHideDirective(Filename: string; Line,Column: integer): boolean;
  public
    property CurrentDirectory: string read fCurrentDirectory
                                      write fCurrentDirectory;
    property FilteredLines: TFilteredOutputLines read fFilteredOutput;
    property StopExecute: boolean read FStopExecute write SetStopExecute;
    property Lines: TIDEMessageLineList read fOutput;
    property LastErrorType: TFPCErrorType read fLastErrorType;
    property LastMessageType: TOutputMessageType read fLastMessageType;
    property OnGetIncludePath: TOnGetIncludePath
                                 read fOnGetIncludePath write fOnGetIncludePath;
    property OnReadLine: TOnOutputString read fOnReadLine write fOnReadLine;// used by the messages window
    property OnAddFilteredLine: TOnAddFilteredLine
                               read fOnAddFilteredLine write fOnAddFilteredLine;// used by the messages window
    property Options: TOuputFilterOptions read fOptions write fOptions;
    property CompilerOptions: TBaseCompilerOptions read FCompilerOptions
                                               write FCompilerOptions;
    property CurrentMessageParts: TStrings read GetCurrentMessageParts;
    property AsyncProcessTerminated: boolean read FAsyncProcessTerminated;
    property OnEndReading: TOFOnEndReading read FOnEndReading write FOnEndReading;
    property OnBeginUpdate: TNotifyEvent read FOnBeginUpdate write FOnBeginUpdate;
    property OnEndUpdate: TNotifyEvent read FOnEndUpdate write FOnEndUpdate;
    property Caller: TObject read FCaller;
    property ScanLine: TOFScanLine read FScanLine;
    property Tool: TIDEExternalToolOptions read FTool;
    property ScannerCount: integer read GetScannerCount;
    property Scanners[Index: integer]: TIDEMsgScanner read GetScanners;
    property ErrorTypeName[errType: TFPCErrorType]: string read GetErrorName write SetErrorName;
    property State: TOutputFilterState read FState;
  end;
  
  EOutputFilterError = class(Exception)
  end;

type
  TProcessClass = class of TProcessUTF8;

var
  TOutputFilterProcess: TProcessClass = nil;
  MessageScanners: TMessageScanners = nil;


implementation


{ TOutputFilter }

constructor TOutputFilter.Create;
begin
  inherited Create;
  FState := ofsNone;
  fFilteredOutput:=TFilteredOutputLines.Create;
  fOutput:=TIDEMessageLineList.Create;
  fOptions:=[ofoSearchForFPCMessages,ofoSearchForMakeMessages,
             ofoMakeFilenamesAbsolute];
  Clear;
end;

procedure TOutputFilter.Clear;
begin
  fOutput.Clear;
  FAsyncDataAvailable:=false;
  FAsyncProcessTerminated:=false;
  FLasTOutputLineParts:=-1;
  fFilteredOutput.Clear;
  if fCompilingHistory<>nil then fCompilingHistory.Clear;
  if fMakeDirHistory<>nil then fMakeDirHistory.Clear;
  FStopExecute:=false;
  fLastSearchedShortIncFilename:='';
  fLastSearchedIncFilename:='';
end;

procedure TOutputFilter.InitExecute;
begin
  //debugln('TOutputFilter.Execute A CurrentDirectory="',TheProcess.CurrentDirectory,'"');
  fCurrentDirectory:=TrimFilename(fProcess.CurrentDirectory);
  if fCurrentDirectory='' then fCurrentDirectory:=GetCurrentDirUTF8;
  fCurrentDirectory:=AppendPathDelim(fCurrentDirectory);

  ErrorExists:=true;
  Aborted:=false;

  //Darwin linker features
  DarwinLinkerMultiline:=false;
  DarwinLinkerLine:='';
  FreeAndNil(FAsyncOutput);
end;

procedure TOutputFilter.CleanUpExecute;
begin
  // workaround for missing TProcess error handling
  {$IFDEF VerboseOFExecute}
  WriteLn('TOutputFilter.Execute W2');
  {$ENDIF}
  EndBufferingOutput;
  fProcess:=nil;
  FAsyncProcess:= nil;
  {$IFDEF VerboseOFExecute}
  WriteLn('TOutputFilter.Execute W3');
  {$ENDIF}
  FreeAndNil(FAsyncOutput);
  {$IFDEF VerboseOFExecute}
  WriteLn('TOutputFilter.Execute W4');
  {$ENDIF}
  if Assigned(OnEndReading) then OnEndReading(Self,fOutput);
  {$IFDEF VerboseOFExecute}
  WriteLn('TOutputFilter.Execute W5');
  {$ENDIF}
  FreeAndNil(FScanLine);
  {$IFDEF VerboseOFExecute}
  WriteLn('TOutputFilter.Execute W6');
  {$ENDIF}
  FTool:=nil;
  FCaller:=nil;
  ClearScanners;
  {$IFDEF VerboseOFExecute}
  WriteLn('TOutputFilter.Execute W7');
  {$ENDIF}
end;

function TOutputFilter.Execute(TheProcess: TProcessUTF8; aCaller: TObject;
  aTool: TIDEExternalToolOptions): boolean;
const
  BufSize = 4096;
  NormalWait = ((double(1)/86400)/15); // 15 times per second
  LongWait = ((double(1)/86400)/4); // 4 times per second
var
  i, Count, LineStart : longint;
  OutputLine, Buf : String;
  TheAsyncProcess: TAsyncProcess;
  LastProcessMessages: TDateTime;
  EndUpdateNeeded: Boolean;
  ExceptionMsg: String;
  Wait: double;
begin
  Result:=true;
  FHasRaisedException := False;
  if FState = ofsRunning then RaiseGDBException('OutputFilter already running');

  Clear;
  fProcess:=TheProcess;
  FCaller:=aCaller;
  FTool:=aTool;
  FScanLine:=TOFScanLine.Create(Self);

  InitExecute;

  SetLength(Buf,BufSize);
  OutputLine:='';
  TheAsyncProcess:=nil;
  EndUpdateNeeded:=false;
  ExceptionMsg:='';

  try
    BeginBufferingOutput;

    // create custom scanners
    ClearScanners;
    if (Tool<>nil) and (Tool.Scanners<>nil)
    and (not CreateScanners(Tool.Scanners)) then
      exit;

    //debugln(['TOutputFilter.Execute ',dbgsname(fProcess)]);
    if fProcess is TAsyncProcess then begin
      TheAsyncProcess:=TAsyncProcess(fProcess);
      TheAsyncProcess.OnReadData:=@OnAsyncReadData;
      TheAsyncProcess.OnTerminate:=@OnAsyncTerminate;
      FAsyncOutput:=TDynamicDataQueue.Create;
    end;

    fProcess.Execute;

    LastProcessMessages:=Now-1;// force one update at start
    Wait:=NormalWait;
    repeat
      if (Application<>nil) and (abs(LastProcessMessages-Now)>Wait)
      then begin
        LastProcessMessages:=Now;
        if EndUpdateNeeded then begin
          EndUpdateNeeded:=false;
          EndUpdate;
        end;
        Application.ProcessMessages;
        Application.Idle(false);
        BeginUpdate;
        EndUpdateNeeded:=true;
      end;
      if StopExecute then begin
        fProcess.Terminate(0);
        Aborted:=true;
        Result:=false;
        ReadConstLine('aborted',true);
        break;
      end;

      Count:=0;
      if (TheAsyncProcess<>nil) then begin
        // using non blocking TAsyncProcess
        Count:=FAsyncOutput.Size;
        //DebugLn(['TOutputFilter.Execute Count=',Count,' AsyncProcessTerminated=',AsyncProcessTerminated,' ',TheAsyncProcess.NumBytesAvailable]);
        if Count>0 then
          Count:=FAsyncOutput.Pop(Buf[1],Min(Count,length(Buf)))
        else if AsyncProcessTerminated then begin
          Count:=TheAsyncProcess.NumBytesAvailable;
          if Count>0 then begin
            Count:=fProcess.Output.Read(Buf[1],Min(Count,length(Buf)));
          end else begin
            break;
          end;
        end else begin
          // no new input, but process still running
          Sleep(30);
        end;
      end;
      if (TheAsyncProcess=nil) and (fProcess.Output<>nil) then begin
        // using a blocking TProcess
        Count:=fProcess.Output.Read(Buf[1],length(Buf));
        if Count=0 then begin
          // no output on blocking means, process has ended
          break;
        end;
      end;
      //DebugLn('TOutputFilter.Execute Count=',dbgs(Count));

      LineStart:=1;
      i:=1;
      while i<=Count do begin
        if Buf[i] in [#10,#13] then begin
          OutputLine:=OutputLine+copy(Buf,LineStart,i-LineStart);
          ReadLine(OutputLine,false);
          if fLastErrorType in [etFatal, etPanic, etError] then begin
            Result:=false;
          end;
          OutputLine:='';
          if (i<Count) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1])
          then
            inc(i);
          LineStart:=i+1;
        end;
        inc(i);
      end;
      if Count=length(Buf) then begin
        // the buffer is full => process more and update the view less
        Wait:=LongWait;
      end else begin
        // the buffer was not full  => update more often
        Wait:=NormalWait;
      end;
      OutputLine:=OutputLine+copy(Buf,LineStart,Count-LineStart+1);
    until false;
    //DebugLn('TOutputFilter.Execute After Loop');
    if not fProcess.WaitOnExit then begin
      // process was terminated by signal or OS
      if ErrorExists and (ofoExceptionOnError in Options) then
        ExceptionMsg:='process terminated with errors';
    end else begin
      //DebugLn('TOutputFilter.Execute fProcess.ExitStatus=',dbgs(fProcess.ExitStatus));
      if fProcess.ExitStatus=0 then
        ErrorExists:=false;
      if ErrorExists and (ofoExceptionOnError in Options) then
        ExceptionMsg:='the process exited with error code '+dbgs(fProcess.ExitStatus);
    end;
  finally
    {$IFDEF VerboseOFExecute}
    WriteLn('TOutputFilter.Execute W1');
    {$ENDIF}
    if EndUpdateNeeded then
      EndUpdate;
    CleanUpExecute;
  end;
  if ExceptionMsg<>'' then
    RaiseOutputFilterError(ExceptionMsg);
end;

function TOutputFilter.ExecuteAsyncron(TheProcess: TProcessUTF8;
  aFinishedCallback: TNotifyEvent; aCaller: TObject; aTool: TIDEExternalToolOptions): boolean;
begin
  Result := False;
  if FState = ofsRunning then RaiseGDBException('OutputFilter already running');
  FState := ofsRunning;
  FHasReadErrorLine := False;
  FHasRaisedException := False;

  Clear;
  fProcess:=TheProcess;
  FCaller:=aCaller;
  FFinishedCallback := aFinishedCallback;
  FTool:=aTool;
  FScanLine:=TOFScanLine.Create(Self);

  InitExecute;

  try
    BeginBufferingOutput;

    // create custom scanners
    ClearScanners;
    if (Tool<>nil) and (Tool.Scanners<>nil)
    and (not CreateScanners(Tool.Scanners)) then
      exit;

    //debugln(['TOutputFilter.Execute ',dbgsname(fProcess)]);
    if fProcess is TAsyncProcess then begin
      FAsyncProcess:=TAsyncProcess(fProcess);
      FAsyncProcess.OnReadData:=@OnAsyncReadData;
      FAsyncProcess.OnTerminate:=@OnAsyncTerminate;
      FAsyncOutput:=TDynamicDataQueue.Create;
    end;

    fProcess.Execute;
    Result := True;
    FLastAsyncExecuteTime := Now;
  finally
    if Result then
      Application.QueueAsyncCall(@ContinueAsyncExecute, 0)
    else begin
      CleanUpExecute;
      FState := ofsFailed;
      FFinishedCallback(Self);
    end;
  end;
end;

procedure TOutputFilter.ContinueAsyncExecute(Data: PtrInt);
  procedure CheckTermintedProcess;
  begin
    if fProcess.WaitOnExit then begin
      //DebugLn('TOutputFilter.Execute fProcess.ExitStatus=',dbgs(fProcess.ExitStatus));
      if fProcess.ExitStatus=0 then
        ErrorExists:=false;
    end;
  end;

const
  BufSize = 4096;
var
  i, Count, LineStart : longint;
  OutputLine, Buf : String;
  AsyncExecuteTime: TDateTime;
begin
  try
    if FState <> ofsRunning then exit;

    if StopExecute then begin
      try
        fProcess.Terminate(0);
        Aborted:=true;
        ReadConstLine('aborted',true);
        CheckTermintedProcess;
      finally
        FState := ofsAborted;
        CleanUpExecute;
        FFinishedCallback(Self);
      end;
      exit;
    end;

    AsyncExecuteTime:=Now;
    SetLength(Buf,BufSize);
    OutputLine:='';
    try
      BeginUpdate;

      repeat
        // Read data
        Count:=0;
        if (FAsyncProcess<>nil) then begin
          // using non blocking TAsyncProcess
          Count:=FAsyncOutput.Size;
          if Count>0 then
            Count:=FAsyncOutput.Pop(Buf[1],Min(Count,length(Buf)))
          else if AsyncProcessTerminated then begin
            Count:=FAsyncProcess.NumBytesAvailable;
            if Count>0 then begin
              Count:=fProcess.Output.Read(Buf[1],Min(Count,length(Buf)));
            end else begin
              FState := ofsSucceded;
              break;
            end;
          end else begin
            // no new input, but process still running
            Sleep(30);
          end;
        end;
        if (FAsyncProcess=nil) and (fProcess.Output<>nil) then begin
          // using a blocking TProcess
          Count:=fProcess.Output.Read(Buf[1],length(Buf));
          if Count=0 then begin
            // no output on blocking means, process has ended
            FState := ofsSucceded;
            break;
          end;
        end;
        //DebugLn('TOutputFilter.Execute Count=',dbgs(Count));

        LineStart:=1;
        i:=1;
        while i<=Count do begin
          if Buf[i] in [#10,#13] then begin
            OutputLine:=OutputLine+copy(Buf,LineStart,i-LineStart);
            ReadLine(OutputLine,false);
            if fLastErrorType in [etFatal, etPanic, etError] then begin
              FHasReadErrorLine := True;
            end;
            OutputLine:='';
            if (i<Count) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1])
            then
              inc(i);
            LineStart:=i+1;
          end;
          inc(i);
        end;
        OutputLine:=OutputLine+copy(Buf,LineStart,Count-LineStart+1);

      // Repeat, if we did have something to read
      until (Count = 0) or  (abs(Now - FLastAsyncExecuteTime) > ((1/86400)/15));

      if not(FState = ofsRunning) then
        CheckTermintedProcess;
    finally
      FLastAsyncExecuteTime := AsyncExecuteTime;
      if (FState = ofsSucceded) and FHasReadErrorLine then
        FState := ofsFailed;
      EndUpdate;
      if FState = ofsRunning then
        Application.QueueAsyncCall(@ContinueAsyncExecute, 0)
      else begin
        CleanUpExecute;
        FFinishedCallback(Self);
      end;
    end;

  except
    try
      fProcess.Terminate(0);
    except end;
    FFinishedCallback(Self);
  end;
end;

procedure TOutputFilter.ReadLine(var s: string; DontFilterLine: boolean);
// this is called for every line written by the external tool (=Output)
// it parses the output
var
  i: Integer;
begin
  //debugln('TOutputFilter: "',s,'"');
  fLastMessageType:=omtNone;
  fLastErrorType:=etNone;
  fOutput.Add(s);
  WriteOutput(false);
  if FScanLine<>nil then begin
    FScanLine.Init(s,fCurrentDirectory,fOutput.Count-1);
    if Tool<>nil then begin
      Tool.ParseLine(Self,FScanLine);
      s:=FScanLine.Line;
    end;
    if Assigned(OnReadLine) then begin
      OnReadLine(FScanLine);
      s:=FScanLine.Line;
    end;
    if FScanners<>nil then begin
      for i:=0 to ScannerCount-1 do begin
        //Scanners[i].ParseLine();
      end;
    end;
  end;

  if DontFilterLine then begin
    DoAddFilteredLine(s);
  end else if (ofoSearchForFPCMessages in Options) and (ReadFPCompilerLine(s))
  then begin
    exit;
  end else if (ofoSearchForMakeMessages in Options) and (ReadMakeLine(s))
  then begin
    exit;
  end else if (ofoShowAll in Options) then begin
    DoAddFilteredLine(s);
  end;
end;

procedure TOutputFilter.ReadConstLine(const s: string; DontFilterLine: boolean);
var
  Line: String;
begin
  Line:=s;
  ReadLine(Line,DontFilterLine);
end;

function TOutputFilter.ReadFPCompilerLine(const s: string): boolean;
{ returns true, if it is a compiler message
   Examples for freepascal compiler messages:
     Compiling <filename>
     Assembling <filename>
     Fatal: <some text>
     <filename>(123,45) <ErrorType>: <some text>
     <filename>(123) <ErrorType>: <some text>
     <filename>(456) <ErrorType>: <some text> in line (123)
}
const
  AsmError = 'Error while assembling';
var i, j, FilenameEndPos: integer;
  MsgTypeName, Filename, Msg: string;
  MsgType: TFPCErrorType;
  SkipMessage: boolean;
  CurCompHistory: string;
  CurFilenameLen: Integer;
  CurCompHistLen: Integer;
  MainSrcFilename: String;
  LineNumberStartPos: LongInt;
  ColumnNumberStartPos: LongInt;
  ColumnNumberEndPos: LongInt;
  MessageStartPos: Integer;
  LineNumberEndPos: LongInt;
  AbsFilename: String;
  LineNumberStr: String;
  Line: LongInt;
  Col: LongInt;
  ColNumberStr: String;

  function CompStr(const SubStr, s: string; Position: integer): boolean;
  begin
    Result:=(SubStr<>'') and (length(s)>=(Position+length(SubStr)-1))
          and (strlcomp(PChar(Pointer(@s[Position])),
                        PChar(Pointer(SubStr)),length(SubStr))=0);
  end;
  
  function CheckForCompilingState(p: integer): boolean;
  var
    AFilename: string;
  begin
    Result:=false;
    if CompStr('Compiling ',s,p) then begin
      // for example 'Compiling ./subdir/unit1.pas'
      fLastMessageType:=omtFPC;
      fLastErrorType:=etNone;
      // add path to history
      if fCompilingHistory=nil then fCompilingHistory:=TStringList.Create;
      inc(p,length('Compiling '));
      if (length(s)>=p+1) and (s[p]='.') and (s[p+1] in ['/','\']) then
        inc(p,2);
      AFilename:=TrimFilename(SetDirSeparators(copy(s,p,length(s))));
      fCompilingHistory.Add(AFilename);
      CurrentMessageParts.Values['Stage']:='FPC';
      CurrentMessageParts.Values['Type']:='Compiling';
      CurrentMessageParts.Values['Filename']:=AFilename;
      Result:=true;
    end;
  end;
  
  function CheckForAssemblingState(p: integer): boolean;
  begin
    Result:=false;
    if CompStr('Assembling ',s,p) then begin
      fLastMessageType:=omtFPC;
      fLastErrorType:=etNone;
      CurrentMessageParts.Values['Stage']:='FPC';
      CurrentMessageParts.Values['Type']:='Assembling';
      Result:=true;
    end;
  end;
  
  function CheckForPPULoading(p: integer): boolean;
  begin
    Result:=false;
    if CompStr('PPU ',s,p) then begin
      fLastMessageType:=omtFPC;
      fLastErrorType:=etNone;
      if CompStr('PPU Invalid Version',s,p) then
        fLastErrorType:=etFatal;
      CurrentMessageParts.Values['Stage']:='FPC';
      CurrentMessageParts.Values['Type']:=FPCErrorTypeNames[fLastErrorType];
      DoAddFilteredLine(s);
      Result:=true;
    end;
  end;

  function CheckForUrgentMessages(p: integer): boolean;
  const
    UnableToOpen = 'Fatal: Unable to open file ';
  var
    NewLine: String;
    LastFile: string;
    FullFilename: String;
  begin
    Result:=false;
    if CompStr('Fatal: ',s,p)
    or CompStr('Panic',s,p)
    or CompStr('Error: ',s,p)
    or CompStr(FErrorNames[etFatal], s, p)
    or CompStr(FErrorNames[etError], s, p)
    or CompStr('Closing script ppas.sh',s,p)
    then begin
      // always show fatal, panic and linker errors
      fLastMessageType:=omtFPC;
      if CompStr('Panic',s,p) then
        fLastErrorType:=etPanic
      else if CompStr('Fatal: ',s,p) or CompStr(FErrorNames[etFatal], s, p) then
        fLastErrorType:=etFatal
      else if CompStr('Error: ',s,p) or CompStr(FErrorNames[etError], s, p) then
        fLastErrorType:=etError
      else if CompStr('Closing script ppas.sh',s,p) then begin
        // linker error
        fLastMessageType:=omtLinker;
        fLastErrorType:=etFatal;
      end;
      if fLastMessageType=omtLinker then
        CurrentMessageParts.Values['Stage']:='Linker'
      else
        CurrentMessageParts.Values['Stage']:='FPC';
      CurrentMessageParts.Values['Type']:=FPCErrorTypeNames[fLastErrorType];

      NewLine:=copy(s,p,length(s));
      if fLastErrorType in [etPanic,etFatal] then begin
        // fatal and panic errors are not very informative
        // -> prepend current file
        if CompStr(UnableToOpen, NewLine, 1) then
          CurrentMessageParts.Values['Filename']:=
            TrimFilename(SetDirSeparators(Copy(NewLine,Length(UnableToOpen)+1,Length(NewLine))))
        else if (fCompilingHistory<>nil) and (fCompilingHistory.Count>0) then begin
          LastFile:=fCompilingHistory[fCompilingHistory.Count-1];
          if not FilenameIsAbsolute(LastFile) then
            FullFilename:=TrimFilename(fCurrentDirectory+LastFile)
          else
            FullFilename:=LastFile;
          if (ofoMakeFilenamesAbsolute in Options)
          and (not FilenameIsAbsolute(LastFile)) then begin
            if FileExistsUTF8(FullFilename) then
              LastFile:=FullFilename;
          end;
          if FileExistsUTF8(FullFilename) then begin
            CurrentMessageParts.Values['Filename']:=FullFilename;
            NewLine:=LastFile+'(1,1) '+NewLine;
          end;
        end;
      end;
      DoAddFilteredLine(NewLine);
      if (ofoExceptionOnError in Options) then
        RaiseOutputFilterError(NewLine);
      Result:=true;
      exit;
    end;
  end;
  
  function CheckForNoteMessages(p: integer): boolean;
  begin
    Result:=false;
    if CompStr('Note: ',s,p) or CompStr(FErrorNames[etNote], s, p) then begin
      DoAddFilteredLine(copy(s,p,length(s)));
      fLastErrorType:=etNote;
      CurrentMessageParts.Values['Stage']:='FPC';
      CurrentMessageParts.Values['Type']:=FPCErrorTypeNames[fLastErrorType];
      Result:=true;
      exit;
    end;
  end;

  function CheckForNumber(const Str: string; var p: integer): boolean;
  var
    OldP: Integer;
  begin
    OldP:=p;
    while (p<=length(Str)) and (Str[p] in ['0'..'9']) do inc(p);
    Result:=OldP<>p;
  end;
  
  function CheckForChar(const Str: string; var p: integer; c: char): boolean;
  begin
    Result:=(p<=length(Str)) and (Str[p]=c);
    if Result then inc(p);
  end;

  function CheckForString(const Str: string; var p: integer;
    const Find: string): boolean;
  begin
    Result:=CompStr(Find,Str,p);
    if Result then inc(p,length(Find));
  end;

  function CheckForLineProgress(p: integer): boolean;
  begin
    Result:=false;
    if not CheckForNumber(s,p) then exit;
    if not CheckForChar(s,p,' ') then exit;
    if not CheckForNumber(s,p) then exit;
    if not CheckForChar(s,p,'/') then exit;
    if not CheckForNumber(s,p) then exit;
    if not CheckForChar(s,p,' ') then exit;
    Result:=true;
    // I don't think it should be shown in filtered lines: DoAddFilteredLine(s);
  end;
  
  function CheckForLinesCompiled(p: integer): boolean;
  var
    OldStart: LongInt;
  begin
    Result:=false;
    OldStart:=p;
    if not CheckForNumber(s,p) then exit;
    if not CheckForString(s,p,' lines compiled, ') then exit;
    if not CheckForNumber(s,p) then exit;
    if not CheckForChar(s,p,'.') then exit;
    if not CheckForNumber(s,p) then exit;
    if not CheckForChar(s,p,' ') then exit;
    Result:=true;
    if (CompilerOptions<>nil)
    and (CompilerOptions.ShowAll or CompilerOptions.ShowSummary) then
      DoAddFilteredLine(copy(s,OldStart,length(s)));
  end;

  { For example:
  Size of Code: 1184256 bytes
  Size of initialized data: 519168 bytes
  Size of uninitialized data: 83968 bytes
  Stack space reserved: 262144 bytes
  Stack space committed: 4096 bytes
  }
  function CheckForExecutableInfo(p: integer): boolean;
  var
    OldStart: LongInt;
  begin
    Result:=false;
    OldStart:=p;
    if not (CheckForString(s,p,'Size of Code: ') or
            CheckForString(s,p,'Size of initialized data: ') or
            CheckForString(s,p,'Size of uninitialized data: ') or
            CheckForString(s,p,'Stack space reserved: ') or
            CheckForString(s,p,'Stack space committed: ') or // message contains typo
            CheckForString(s,p,'Stack space committed: ')) then exit;
    if not CheckForNumber(s,p) then exit;
    if not CheckForString(s,p,' bytes') then exit;
    Result:=true;
    if (CompilerOptions<>nil)
    and (CompilerOptions.ShowAll or CompilerOptions.ShowExecInfo) then
      DoAddFilteredLine(copy(s,OldStart,length(s)));
  end;
  
  { For example:
    linkerror.o(.text$_main+0x9):linkerror.pas: undefined reference to `NonExistingFunction'

    Mac OS X linker example:
    ld: framework not found Cocoas

    Multiline Mac OS X linker example:
    Undefined symbols:
      "_exterfunc", referenced from:
          _PASCALMAIN in testld.o
      "_exterfunc2", referenced from:
          _PASCALMAIN in testld.o
    ld: symbol(s) not found
  }
  function CheckForLinkingErrors(p: integer): boolean;
  var
    OldStart: LongInt;
    DarwinSymbs:Boolean;
  const
    DarwinPrefixLvl1 = '  ';
    DarwinPrefixLvl2 = '      ';
  begin
    Result:=false;
    OldStart:=p;
    while (p<=length(s)) and (s[p] in ['0'..'9','a'..'z','A'..'Z','_']) do
      inc(p);
    if not CompStr('.o(',s,p) then begin
      p := OldStart;
      if CompStr('ld: ',s,p) then begin
        inc(p, 4);
        DarwinSymbs := CompStr('symbol(s) not found',s,p);
        Result := DarwinSymbs or (Pos('not found', s) > 0);
        if DarwinSymbs then begin
          if DarwinLinkerLine <> '' then DoAddFilteredLine(DarwinLinkerLine);
          DarwinLinkerMultiline:=false;
        end;
      end else if CompStr('Undefined symbols:', s, OldStart) or DarwinLinkerMultiline then begin
        DarwinLinkerMultiline:=true;
        if CompStr(DarwinPrefixLvl2, s, OldStart) then begin
          DarwinLinkerLine := DarwinLinkerLine + ' ' +
            Copy(s, length(DarwinPrefixLvl2)+1, length(s)-length(DarwinPrefixLvl2));
        end else if CompStr(DarwinPrefixLvl1, s, OldStart) then begin
          if DarwinLinkerLine <> '' then DoAddFilteredLine(DarwinLinkerLine);
          DarwinLinkerLine := s;
        end else begin
          if DarwinLinkerLine <> '' then DoAddFilteredLine(DarwinLinkerLine);
          DoAddFilteredLine(copy(s,OldStart,length(s)));
        end;
      end;
      if not Result then Exit;
      p := OldStart;
    end;
    Result:=true;
    DoAddFilteredLine(copy(s,OldStart,length(s)));
  end;
  
  { example:
    Recompiling GtkInt, checksum changed for gdk2x
  }
  function CheckForRecompilingChecksumChangedMessages(p: integer): boolean;
  var
    OldStart: LongInt;
  begin
    Result:=false;
    OldStart:=p;
    if not CompStr('Recompiling ',s,p) then exit;
    while (p<=length(s)) and (s[p]<>',') do
      inc(p);
    if not CompStr(', checksum changed for ',s,p) then exit;
    Result:=true;
    DoAddFilteredLine(copy(s,OldStart,length(s)));
  end;
  
  { example:
    ...\windres.exe: warning: ...
  }
  function CheckForWindresErrors(p: integer): boolean;
  var
    wPos: integer;
    tempStr: String;
  begin
    Result := false;
    tempStr := LowerCase(s);
    wPos := Pos('windres', tempStr);
    Result := (wPos > p);
    if Result then
    begin
      p := wPos + 7;
      if CompStr('.exe', s, p) then
        inc(p, 4);
      DoAddFilteredLine('windres' + copy(s, p, length(s)));
    end;
  end;

  function CheckForUnitUsed(p: integer): Boolean;
  var
    pp  : Integer;
  begin
    pp := Pos('Load from ', s);
    Result := (pp > 0)  and (s[p] = '(') and (Pos(' unit ', s)>0);
    if Result then DoAddFilteredLine(Copy(s, pp, length(s)-pp+1));
  end;

begin
  Result:=false;
  if s='' then exit;
  i:=1;
  // skip time [0.000]
  if (s<>'') and (s[1]='[') then begin
    inc(i);
    while (i<=length(s)) and (s[i] in ['0'..'9','.']) do inc(i);
    if (i<=length(s)) and (s[i]=']') then inc(i);
    while (i<=length(s)) and (s[i] in [' ']) do inc(i);
    // the user enabled extreme verbosity
    // show all
    DoAddFilteredLine(Copy(s, i, length(s)));
    exit(true);
  end;
  
  // check for 'Compiling <filename>'
  Result:=CheckForCompilingState(i);
  if Result then exit;
  // check for 'Assembling <filename>'
  Result:=CheckForAssemblingState(i);
  if Result then exit;
  // check for 'PPU Loading <filename>' and 'PPU Invalid Version <number>'
  Result:=CheckForPPULoading(i);
  if Result then exit;
  // check for 'Fatal: ', 'Panic: ', 'Error: ', 'Closing script ppas.sh'
  Result:=CheckForUrgentMessages(i);
  if Result then exit;
  // check for 'Note: '
  Result:=CheckForNoteMessages(i);
  if Result then exit;
  // check for '<line> <kb>/<kb>'...
  Result:=CheckForLineProgress(i);
  if Result then exit;
  // check for '<int> Lines compiled, <int>.<int> sec'
  Result:=CheckForLinesCompiled(i);
  if Result then exit;
  // check for -vx output
  Result:=CheckForExecutableInfo(i);
  if Result then exit;
  // check for linking errors
  Result:=CheckForLinkingErrors(i);
  if Result then exit;
  // check for Recompiling, checksum changed
  Result:=CheckForRecompilingChecksumChangedMessages(i);
  if Result then exit;
  // check for windres errors
  Result := CheckForWindresErrors(i);
  if Result then Exit;
  // check for Load from unit
  Result := Assigned(CompilerOptions) and CompilerOptions.ShowUsedFiles
            and CheckForUnitUsed(i);
  if Result then Exit;

  // search for round bracket open
  i:=1;
  while (i<=length(s)) and (s[i]<>'(') do inc(i);
  FilenameEndPos:=i-1;
  inc(i);
  // search for number
  LineNumberStartPos:=i;
  if not CheckForNumber(s,i) then exit;
  LineNumberEndPos:=i;
  if (i<length(s)) and (s[i]=',') and (s[i+1] in ['0'..'9']) then begin
    // skip second number
    inc(i);
    ColumnNumberStartPos:=i;
    while (i<=length(s)) and (s[i] in ['0'..'9']) do inc(i);
    ColumnNumberEndPos:=i;
  end else begin
    ColumnNumberStartPos:=i;
    ColumnNumberEndPos:=i;
  end;
  // search for ') <ErrorType>: '
  if not CheckForChar(s,i,')') then exit;
  if not CheckForChar(s,i,' ') then exit;
  if (i>=length(s)) or (not (s[i] in ['A'..'Z',#128..#255])) then exit; {#128..#255 - utf8 encoded strings}
  j:=i+1;
  while (j<=length(s)) and (s[j] in ['a'..'z',#128..#255]) do inc(j); 
  if (j+1>length(s)) or (s[j]<>':') or (s[j+1]<>' ') then exit;
  MessageStartPos:=j+2;
  MsgTypeName:=copy(s,i,j-i);
  for MsgType:=Succ(etNone) to High(TFPCErrorType) do begin
    // FPCErrorTypeNames is checked, in case of badly formed message file
    if (FErrorNames[MsgType]=MsgTypeName) or (FPCErrorTypeNames[MsgType]=MsgTypeName) then begin
      // this is a freepascal compiler message
      // -> filter message
      fLastErrorType:=MsgType;
      fLastMessageType:=omtFPC;
      CurrentMessageParts.Values['Stage']:='FPC';
      CurrentMessageParts.Values['Type']:=FPCErrorTypeNames[fLastErrorType];
      LineNumberStr:=copy(s,LineNumberStartPos,LineNumberEndPos-LineNumberStartPos);
      CurrentMessageParts.Values['Line']:=LineNumberStr;
      ColNumberStr:=copy(s,ColumnNumberStartPos,ColumnNumberEndPos-ColumnNumberStartPos);
      CurrentMessageParts.Values['Column']:=ColNumberStr;
      CurrentMessageParts.Values['Message']:=copy(s,MessageStartPos,length(s));

      SkipMessage:=true;
      
      case MsgType of

      etHint:
        begin
          SkipMessage:=(CompilerOptions<>nil)
                       and (not (CompilerOptions.ShowHints
                            or CompilerOptions.ShowAll));
          if (not SkipMessage)
          and (CompilerOptions<>nil)
          and (not CompilerOptions.ShowAll) then begin
            if (not CompilerOptions.ShowHintsForSenderNotUsed) and
              (IsHintForParameterSenderNotUsed(s)) then
              SkipMessage:=true
            else if (not CompilerOptions.ShowHintsForUnusedUnitsInMainSrc) then
            begin
              MainSrcFilename:=CompilerOptions.GetDefaultMainSourceFileName;
              if (MainSrcFilename<>'')
              and (IsHintForUnusedUnit(s,MainSrcFilename)) then
                SkipMessage:=true;
            end;
          end;
        end;

      etNote:
        begin
          SkipMessage:=(CompilerOptions<>nil)
               and (not (CompilerOptions.ShowNotes or CompilerOptions.ShowAll));
        end;

      etError:
        begin
          SkipMessage:=(CompilerOptions<>nil)
              and (not (CompilerOptions.ShowErrors or CompilerOptions.ShowAll));
          if copy(s,j+2,length(s)-j-1)='Error while linking' then begin
            DoAddLastLinkerMessages(true);
          end
          else if copy(s,j+2,length(AsmError))=AsmError then begin
            DoAddLastAssemblerMessages;
          end;
        end;

      etWarning:
        begin
          SkipMessage:=(CompilerOptions<>nil)
                and (not (CompilerOptions.ShowWarn or CompilerOptions.ShowAll));
        end;

      etPanic, etFatal:
        SkipMessage:=false;

      end;

      // beautify compiler message
      
      // the compiler always gives short filenames, even if it went into a
      // subdirectory
      // -> prepend the current subdirectory
      Msg:=s;
      Filename:=TrimFilename(copy(Msg,1,FilenameEndPos));
      if not FilenameIsAbsolute(Filename) then begin
        // filename is relative
        i:=-1;
        if (fCompilingHistory<>nil) then begin
          // the compiler writes a line compiling ./subdir/unit.pas
          // and then writes the messages without any path
          // -> prepend this subdirectory
          i:=fCompilingHistory.Count-1;
          while (i>=0) do begin
            CurCompHistory:=fCompilingHistory[i];
            CurCompHistLen:=length(CurCompHistory);
            CurFilenameLen:=length(Filename);
            j:=CurCompHistLen-CurFilenameLen;
            if (j>1) and (CurCompHistory[j]=PathDelim)
            and (CompareFilenames(
              copy(CurCompHistory,j+1,CurFilenameLen),Filename)=0) then
            begin
              Msg:=copy(CurCompHistory,1,j)+Msg;
              inc(FilenameEndPos,j);
              break;
            end;
            dec(i);
          end;
        end;
        if i<0 then begin
          // this file is not a compiled pascal source
          // -> search for include files
          Filename:=SearchIncludeFile(Filename);
          Msg:=Filename+copy(Msg,FileNameEndPos+1,length(Msg)-FileNameEndPos);
          FileNameEndPos:=length(Filename);
        end;
      end;
      
      // make filenames absolute if wanted
      Filename:=TrimFilename(SetDirSeparators(copy(Msg,1,FilenameEndPos)));
      if FilenameIsAbsolute(Filename) then begin
        AbsFilename:=Filename;
        CurrentMessageParts.Values['Filename']:=AbsFilename;
      end else begin
        AbsFilename:=TrimFilename(fCurrentDirectory+Filename);
        if not FileExistsCached(AbsFilename) then begin
          AbsFilename:='';
        end;
      end;
      if (ofoMakeFilenamesAbsolute in Options) then begin
        if (AbsFilename<>'') and (AbsFilename<>Filename) then begin
          Filename:=AbsFilename;
          Msg:=Filename+TrimFilename(SetDirSeparators(
                        copy(Msg,FilenameEndPos+1,length(Msg)-FilenameEndPos)));
        end;
      end else begin
        if FileIsInPath(Filename,fCurrentDirectory) then begin
          Filename:=CreateRelativePath(Filename,fCurrentDirectory);
          Msg:=Filename+TrimFilename(SetDirSeparators(
                        copy(Msg,FilenameEndPos+1,length(Msg)-FilenameEndPos)));
        end;
      end;
      //DebugLn('TOutputFilter.ReadFPCompilerLine AbsFilename=',AbsFilename,' Filename=',Filename,' Dir=',fCurrentDirectory);
      if AbsFilename<>'' then begin
        AbsFilename:=CodeToolBoss.DirectoryCachePool.FindDiskFilename(AbsFilename);
        CurrentMessageParts.Values['Filename']:=AbsFilename;
        Line:=StrToIntDef(LineNumberStr,1);
        Col:=StrToIntDef(ColNumberStr,1);
        if (MsgType in [etHint,etNone,etWarning])
        and HasHideDirective(AbsFilename,Line,Col) then
          SkipMessage:=true;
      end else
        CurrentMessageParts.Values['Filename']:=Filename;

      // add line
      if not SkipMessage then
        DoAddFilteredLine(Msg);
        
      if (ofoExceptionOnError in Options) and (MsgType in [etPanic, etFatal])
      then
        RaiseOutputFilterError(Msg);
      Result:=true;
      exit;
    end;
  end;
end;

function TOutputFilter.IsHintForUnusedUnit(const OutputLine,
  MainSrcFile: string): boolean;
//todo: multilingual?
{ recognizes hints of the form

  mainprogram.pp(5,35) Hint: Unit UNUSEDUNIT not used in mainprogram
}
var Filename: string;
begin
  Result:=false;
  Filename:=ExtractFilename(MainSrcFile);
  if CompareFilenames(Filename,copy(OutputLine,1,length(Filename)))<>0 then
    exit;
  if (pos(') Hint: Unit ',OutputLine)<>0)
  and (pos(' not used in ',OutputLine)<>0) then
    Result:=true;
end;

function TOutputFilter.IsHintForParameterSenderNotUsed(const OutputLine: string): boolean;
//todo: multilingual?
{ recognizes hints of the form

  Unit1.pas(15,28) Hint: Parameter "Sender" not used

}
const
  SenderNotUsed= ') Hint: Parameter "Sender" not used';
begin
  Result:=
    pos(SenderNotUsed, OutputLine)=Length(OutputLine)-Length(SenderNotUsed)+1;
end;

procedure TOutputFilter.DoAddFilteredLine(const s: string;
  OriginalIndex: integer);
begin
  if OriginalIndex=-1 then
    OriginalIndex:=fOutput.Count-1;
  fFilteredOutput.Add(s);
  fFilteredOutput.OriginalIndices[fFilteredOutput.Count-1]:=OriginalIndex;
  if Assigned(OnAddFilteredLine) then
    OnAddFilteredLine(s,fCurrentDirectory,OriginalIndex,CurrentMessageParts);
end;

procedure TOutputFilter.DoAddLastLinkerMessages(SkipLastLine: boolean);
var i: integer;
begin
  // read back to 'Linking' message
  i:=fOutput.Count-1;
  while (i>=0) and (LeftStr(fOutput[i].Msg,length('Linking '))<>'Linking ') do
    dec(i);
  inc(i);
  // output skipped messages
  while (i<fOutput.Count) do begin
    if (fOutput[i].Msg<>'')
    and ((i<fOutput.Count-1) or (not SkipLastLine)) then
      DoAddFilteredLine(fOutput[i].Msg,i);
    inc(i);
  end;
end;

procedure TOutputFilter.DoAddLastAssemblerMessages;
const
  AsmStartMsg = 'Assembler messages:';
var i: integer;
begin
  // read back to 'Assembler messages:' message
  i:=fOutput.Count-1;
  while (i>=0) and (RightStr(fOutput[i].Msg,length(AsmStartMsg))<>AsmStartMsg) do
    dec(i);
  if i<0 then exit;
  while (i<fOutput.Count-1) do begin
    if (fOutput[i].Msg<>'') then
      DoAddFilteredLine(fOutput[i].Msg,i);
    inc(i);
  end;
end;

function TOutputFilter.GetCurrentMessageParts: TStrings;
var
  Cnt: LongInt;
  Line: TIDEMessageLine;
begin
  Result:=nil;
  if (fOutput=nil) then exit;
  Cnt:=fOutput.Count;
  if (Cnt=0) then exit;
  Result:=fOutput.Parts[Cnt-1];
  if Result=nil then begin
    Result:=TStringList.Create;
    Line:=fOutput[Cnt-1];
    Line.Directory:=fCurrentDirectory;
    Line.Parts:=Result;
  end;
end;

function TOutputFilter.GetScanners(Index: integer): TIDEMsgScanner;
begin
  Result:=TIDEMsgScanner(fScanners[Index]);
end;

function TOutputFilter.GetScannerCount: integer;
begin
  if FScanners<>nil then
    Result:=FScanners.Count
  else
    Result:=0;
end;

function TOutputFilter.SearchIncludeFile(const ShortIncFilename: string): string;
// search the include file and make it relative to the current start directory
var
  AlreadySearchedPaths: string;
  AlreadySearchedIncPaths: string;
  FullDir, RelativeDir, IncludePath: string;
  p: integer;
begin
  if fCompilingHistory=nil then begin
    Result:=ShortIncFilename;
    exit;
  end;
  // try cache
  if (fLastSearchedShortIncFilename<>'')
  and (fLastSearchedShortIncFilename=ShortIncFilename)
  then begin
    Result:=fLastSearchedIncFilename;
    exit;
  end;
  
  try
    AlreadySearchedPaths:='';
    AlreadySearchedIncPaths:='';
    // try every compiled pascal source
    for p:=fCompilingHistory.Count-1 downto 0 do begin
      RelativeDir:=AppendPathDelim(ExtractFilePath(fCompilingHistory[p]));
      FullDir:=RelativeDir;
      if not FilenameIsAbsolute(FullDir) then
        FullDir:=fCurrentDirectory+FullDir;
      FullDir:=TrimFilename(FullDir);
      if SearchDirectoryInSearchPath(AlreadySearchedPaths,FullDir,1)>0 then
        continue;
      // new directory start a search
      Result:=FullDir+ShortIncFilename;
      if FileExistsUTF8(Result) then begin
        // file found in search dir
        exit;
      end;
      AlreadySearchedPaths:=MergeSearchPaths(AlreadySearchedPaths,FullDir);
      // search with include path of directory
      if Assigned(OnGetIncludePath) then begin
        IncludePath:=TrimSearchPath(OnGetIncludePath(FullDir,false),FullDir,true);
        IncludePath:=RemoveSearchPaths(IncludePath,AlreadySearchedIncPaths);
        if IncludePath<>'' then begin
          Result:=SearchFileInPath(ShortIncFilename,FullDir,IncludePath,';',[]);
          if Result<>'' then begin
            if LeftStr(Result,length(fCurrentDirectory))=fCurrentDirectory then
              Result:=TrimFilename(
                     RightStr(Result,length(Result)-length(fCurrentDirectory)));
            exit;
          end;
          AlreadySearchedIncPaths:=MergeSearchPaths(AlreadySearchedIncPaths,
                                                    IncludePath);
        end;
      end;
    end;
    Result:=ShortIncFilename;
  finally
    // cache result, it will probably be used several consecutive times
    fLastSearchedShortIncFilename:=ShortIncFilename;
    fLastSearchedIncFilename:=Result;
  end;
end;

procedure TOutputFilter.SetStopExecute(const AValue: boolean);
begin
  FStopExecute:=AValue;
end;

procedure TOutputFilter.InternalSetCurrentDirectory(const Dir: string);
begin
  fCurrentDirectory:=TrimFilename(AppendPathDelim(SetDirSeparators(Dir)));
end;

procedure TOutputFilter.OnAsyncTerminate(Sender: TObject);
begin
  if fProcess=nil then exit;
  FAsyncProcessTerminated:=true;
end;

procedure TOutputFilter.OnAsyncReadData(Sender: TObject);
var
  Count: LongWord;
begin
  if fProcess=nil then exit;
  Count:=TAsyncProcess(fProcess).NumBytesAvailable;
  if Count>0 then
    FAsyncOutput.Push(TStream(TAsyncProcess(fProcess).Output),Count);
end;

function TOutputFilter.CreateScanners(ScannerOptions: TStrings): boolean;
var
  i: Integer;
  ScannerName: string;
  ScannerType: TIDEMsgScannerType;
  MsgResult: TModalResult;
  CurScanner: TIDEMsgScanner;
begin
  if (ScannerOptions=nil) or (ScannerOptions.Count=0) then exit(true);
  
  // first check if all scanners are there
  for i:=0 to ScannerOptions.Count-1 do begin
    ScannerName:=ScannerOptions[i];
    ScannerType:=MessageScanners.TypeOfName(ScannerName);
    if ScannerType=nil then begin
      MsgResult:=IDEMessageDialog('Unknown Scanner',
        'Scanner "'+ScannerName+'" not found. Maybe you forgot to install a package?',
        mtError,[mbCancel]);
      if MsgResult<>mrIgnore then begin
        Result:=false;
        exit;
      end;
    end;
  end;
  
  // create scanners
  Result:=false;
  ScannerName:='';
  try
    for i:=0 to ScannerOptions.Count-1 do begin
      ScannerName:=ScannerOptions[i];
      ScannerType:=MessageScanners.TypeOfName(ScannerName);
      if ScannerType=nil then continue;
      CurScanner:=ScannerType.StartScan(nil);
      if CurScanner=nil then
        raise Exception.Create('TOutputFilter.CreateScanners failed to create scanner: '+dbgsName(ScannerType));
    end;
    Result:=true;
  except
    on E: Exception do begin
      MsgResult:=IDEMessageDialog('Scanner creation failed',
        'Failed to create scanner "'+ScannerName+'":'#13
        +E.Message,
        mtError,[mbCancel]);
    end;
  end;
end;

procedure TOutputFilter.ClearScanners;
var
  i: Integer;
begin
  if FScanners<>nil then begin
    for i:=0 to FScanners.Count-1 do TObject(FScanners[i]).Free;
    FreeAndNil(FScanners);
  end;
end;

procedure TOutputFilter.SetErrorName(errType: TFPCErrorType; const AValue: String);
begin
  FErrorNames[errType]:=AValue;
end;

function TOutputFilter.GetErrorName(errType: TFPCErrorType): string; 
begin
  Result:=FErrorNames[errType];
end;

destructor TOutputFilter.Destroy;
begin
  ClearScanners;
  FreeAndNil(fFilteredOutput);
  FreeAndNil(fOutput);
  FreeAndNil(fMakeDirHistory);
  FreeAndNil(fCompilingHistory);
  inherited Destroy;
end;

function TOutputFilter.IsParsing: boolean;
begin
  Result:=([ofoSearchForFPCMessages,ofoSearchForMakeMessages]*Options)<>[];
end;

function TOutputFilter.ReadMakeLine(const s: string): boolean;
{ returns true, if it is a make/gmake message
   Examples for make messages:
     make[1]: Entering directory `<filename>'
     make[1]: Leaving directory `<filename>'
     make[1]: *** [<filename>] Killed
}
const
  EnterDirPattern = ']: Entering directory `';
  LeavingDirPattern = ']: Leaving directory `';
  MakeMsgPattern = ']: *** [';
var
  MakeBeginPattern: string;
  i: integer;
  BracketEnd: Integer;
  MsgStartPos: Integer;
  MakeMsg: String;
begin
  Result:=false;
  MakeBeginPattern:= 'make' + GetExecutableExt + '[';
  i:=length(MakeBeginPattern);
  if copy(s,1,i)=MakeBeginPattern then begin
    Result:=true;
    CurrentMessageParts.Values['Stage']:='make';

    inc(i);
    if (i>length(s)) or (not (s[i] in ['0'..'9'])) then exit;
    while (i<=length(s)) and (s[i] in ['0'..'9']) do inc(i);
    if (i>length(s)) or (s[i]<>']') then exit;
    // check for enter directory
    if copy(s,i,length(EnterDirPattern))=EnterDirPattern then
    begin
      CurrentMessageParts.Values['Type']:='entering directory';
      inc(i,length(EnterDirPattern));
      if (fCurrentDirectory<>'') then begin
        if (fMakeDirHistory=nil) then fMakeDirHistory:=TStringList.Create;
        fMakeDirHistory.Add(fCurrentDirectory);
      end;
      // the make tool uses unix paths under windows
      InternalSetCurrentDirectory(SetDirSeparators(copy(s,i,length(s)-i)));
      exit;
    end;
    // check for leaving directory
    if copy(s,i,length(LeavingDirPattern))=LeavingDirPattern then
    begin
      CurrentMessageParts.Values['Type']:='leaving directory';
      if (fMakeDirHistory<>nil) and (fMakeDirHistory.Count>0) then begin
        InternalSetCurrentDirectory(fMakeDirHistory[fMakeDirHistory.Count-1]);
        fMakeDirHistory.Delete(fMakeDirHistory.Count-1);
        exit;
      end else begin
        // leaving which directory???
        InternalSetCurrentDirectory('');
      end;
    end;
    // check for make message
    if copy(s,i,length(MakeMsgPattern))=MakeMsgPattern then
    begin
      BracketEnd:=i+length(MakeMsgPattern);
      while (BracketEnd<=length(s)) and (s[BracketEnd]<>']') do inc(BracketEnd);
      MsgStartPos:=BracketEnd+1;
      while (MsgStartPos<=length(s)) and (s[MsgStartPos]=' ') do inc(MsgStartPos);
      MakeMsg:=copy(s,MsgStartPos,length(s)-MsgStartPos+1);
      DoAddFilteredLine(SetDirSeparators(s));
      if CompareText(copy(MakeMsg,1,5),'Error')=0 then
        if (ofoExceptionOnError in Options) then
          RaiseOutputFilterError(s);
      exit;
    end;
  end
  else begin
    // TODO: under MacOS X and probably BSD too the make does not write
    // entering and leaving directory without the -w option
  end;
end;

procedure TOutputFilter.WriteOutput(Flush: boolean);
// write output in blocks. This way slow terminals don't slow down the IDE.
var
  CurTime: Double;
  s: String;
  i: LongInt;
  NewLen: Integer;
  LineEnd: string = LineEnding;
  CurLine: String;
const
  HalfASecond = 0.5/(24*60*60); // 0.5 divided by the number of seconds per day
begin
  CurTime:=Now;
  if ((CurTime-fLastOutputTime)>HalfASecond)
  or Flush or (FBufferingOutputLock<=0) then begin
    if FLastOutputLineParts<fOutput.Count-1 then begin
      i:=FLastOutputLineParts;
      NewLen:=0;
      while i<fOutput.Count-1 do begin
        inc(i);
        inc(NewLen,length(fOutput[i].Msg));
        inc(NewLen,length(LineEnd));
      end;
      SetLength(s,NewLen);
      i:=1;
      while FLastOutputLineParts<fOutput.Count-1 do begin
        inc(FLastOutputLineParts);
        CurLine:=fOutput[FLastOutputLineParts].Msg;
        if CurLine<>'' then begin
          System.Move(CurLine[1],s[i],length(CurLine));
          inc(i,length(CurLine));
        end;
        System.Move(LineEnd[1],s[i],length(LineEnd));
        inc(i,length(LineEnd));
      end;
      DbgOut(s);
    end;
    fLastOutputTime:=CurTime;
  end;
end;

procedure TOutputFilter.BeginBufferingOutput;
begin
  inc(FBufferingOutputLock);
end;

procedure TOutputFilter.EndBufferingOutput;
begin
  if FBufferingOutputLock<=0 then RaiseException('');
  dec(FBufferingOutputLock);
  if FBufferingOutputLock=0 then
    WriteOutput(true);
end;

procedure TOutputFilter.BeginUpdate;
begin
  if Assigned(OnBeginUpdate) then OnBeginUpdate(Self);
end;

procedure TOutputFilter.EndUpdate;
begin
  if Assigned(OnEndUpdate) then OnEndUpdate(Self);
end;

procedure TOutputFilter.RaiseOutputFilterError(const Msg: string);
begin
  if FHasRaisedException then exit;
  FHasRaisedException:=true;
  raise EOutputFilterError.Create(Msg);
end;

function TOutputFilter.HasHideDirective(Filename: string;
  Line,Column: integer): boolean;
var
  Buf: TCodeBuffer;
  p: Integer;
  Src: String;
begin
  Result:=false;
  if (Line<1) or (Column<1) then exit;
  if not FilenameIsAbsolute(Filename) then exit;
  Filename:=TrimFilename(Filename);
  if (fLastBuffer=nil) or (CompareFilenames(fLastBuffer.Filename,Filename)<>0)
  then begin
    Buf:=CodeToolBoss.LoadFile(Filename,true,false);
    if Buf=nil then exit;
    fLastBuffer:=Buf;
  end;
  // search for an IDE directive in front
  if Line>fLastBuffer.LineCount then exit;
  fLastBuffer.LineColToPosition(Line,Column,p);
  Src:=fLastBuffer.Source;
  if p>length(Src) then
    p:=length(Src);
  if (p>1) and (Src[p] in [',',';']) then
    dec(p);
  while (p>1) do begin
    if Src[p] in [#10,#13,',',';'] then exit;
    if Src[p]='}' then begin
      // could be a IDE directive
      while (p>0) do begin
        case Src[p] of
        #10,#13: exit;
        '{':
          begin
            if (Src[p+1]='%')
            and (Src[p+2] in ['h','H'])
            and (Src[p+3]='-') then begin
              exit(true);
            end;
          end;
        end;
        dec(p);
      end;
    end;
    dec(p);
  end;
end;

{ TFilteredOutputLines }

function TFilteredOutputLines.GetOriginalIndices(Index: integer): integer;
begin
  Result:=FOriginalIndices[Index];
end;

procedure TFilteredOutputLines.SetOriginalIndices(Index: integer;
  const AValue: integer);
begin
  FOriginalIndices[Index]:=AValue;
end;

procedure TFilteredOutputLines.SetCapacity(NewCapacity: Integer);
begin
  ReAllocMem(FOriginalIndices,SizeOf(Integer)*NewCapacity);
  inherited SetCapacity(NewCapacity);
end;

procedure TFilteredOutputLines.InsertItem(Index: Integer; const S: string);
begin
  inherited InsertItem(Index, S);
  if Index<Count-1 then
    System.Move(FOriginalIndices[Index],FOriginalIndices[Index+1],
                (Count-Index)*SizeOf(Integer));
end;

procedure TFilteredOutputLines.InsertItem(Index: Integer; const S: string;
  O: TObject);
begin
  inherited InsertItem(Index, S, O);
  if Index<Count-1 then
    System.Move(FOriginalIndices[Index],FOriginalIndices[Index+1],
                (Count-Index)*SizeOf(Integer));
end;

procedure TFilteredOutputLines.Delete(Index: Integer);
begin
  if (Index>=0) and (Index<Count-1) then
    System.Move(FOriginalIndices[Index+1],
                FOriginalIndices[Index],
                (Count-Index)*SizeOf(TStringItem));
  inherited Delete(Index);
end;

procedure TFilteredOutputLines.Exchange(Index1, Index2: Integer);
var
  i: LongInt;
begin
  inherited Exchange(Index1, Index2);
  i:=FOriginalIndices[Index1];
  FOriginalIndices[Index1]:=FOriginalIndices[Index2];
  FOriginalIndices[Index2]:=i;
end;

{ TOFScanLine }

constructor TOFScanLine.Create(TheFilter: TOutputFilter);
begin
  FFilter:=TheFilter;
  inherited Create(Filter.Caller,Filter.Tool);
end;

procedure TOFScanLine.Init(const aLine, aWorkDir: string;
  const aLineNumber: integer);
begin
  Line:=aLine;
  WorkingDirectory:=aWorkDir;
  SetLineNumber(aLineNumber);
end;

procedure TOFScanLine.LineChanged(const OldValue: string);
begin

end;

procedure TOFScanLine.WorkingDirectoryChanged(const OldValue: string);
begin

end;

{ TMessageScanners }

constructor TMessageScanners.Create;
begin
  MessageScanners:=Self;
  inherited Create;
end;

destructor TMessageScanners.Destroy;
begin
  MessageScanners:=nil;
  inherited Destroy;
end;

end.

