{  $Id$  }
{
 /***************************************************************************
                        outputfilter.pas  -  Lazarus IDE unit
                        -------------------------------------
             TOutputFilter is responsible for parsing output of external
             tools and to filter important messages.

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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

interface

uses
  Classes, Math, SysUtils, Forms, Controls, CompilerOptions, Project, Process,
  AsyncProcess, LCLProc, DynQueue, FileUtil,
  IDEMsgIntf, IDEExternToolIntf,
  IDEProcs, LazConf;

type
  TOnOutputString = procedure(Line: TIDEScanMessageLine) of object;
  TOnAddFilteredLine = procedure(const Msg, Directory: String;
                                 OriginalIndex: integer) of object;
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

  TErrorType = (etNone, etHint, etNote, etWarning, etError, etFatal, etPanic);
  
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

  { TOutputFilter }

  TOutputFilter = class
  private
    FAsyncDataAvailable: boolean;
    FAsyncProcessTerminated: boolean;
    FCaller: TObject;
    FCompilerOptions: TBaseCompilerOptions;
    FBufferingOutputLock: integer;
    fCurrentDirectory: string;
    fFilteredOutput: TFilteredOutputLines;
    FOnEndReading: TOFOnEndReading;
    fOnReadLine: TOnOutputString;
    fOutput: TIDEMessageLineList;
    fLastErrorType: TErrorType;
    fLastMessageType: TOutputMessageType;
    fCompilingHistory: TStringList;
    fMakeDirHistory: TStringList;
    fOnGetIncludePath: TOnGetIncludePath;
    fOnAddFilteredLine: TOnAddFilteredLine;
    fOptions: TOuputFilterOptions;
    FScanLine: TOFScanLine;
    FStopExecute: boolean;
    FLasTOutputLineParts: integer;
    fLastOutputTime: TDateTime;
    fLastSearchedShortIncFilename: string;
    fLastSearchedIncFilename: string;
    fProcess: TProcess;
    FAsyncOutput: TDynamicDataQueue;
    FTool: TIDEExternalToolOptions;
    procedure DoAddFilteredLine(const s: string; OriginalIndex: integer = -1);
    procedure DoAddLastLinkerMessages(SkipLastLine: boolean);
    procedure DoAddLastAssemblerMessages;
    function GetCurrentMessageParts: TStrings;
    function SearchIncludeFile(const ShortIncFilename: string): string;
    procedure SetStopExecute(const AValue: boolean);
    procedure InternalSetCurrentDirectory(const Dir: string);
    procedure OnAsyncTerminate(Sender: TObject);
    procedure OnAsyncReadData(Sender: TObject);
  public
    ErrorExists: boolean;
    Aborted: boolean;
    function Execute(TheProcess: TProcess; aCaller: TObject = nil;
                     aTool: TIDEExternalToolOptions = nil): boolean;
    function GetSourcePosition(const Line: string; var Filename:string;
      var CaretXY: TPoint; var MsgType: TErrorType): boolean;
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
  public
    property CurrentDirectory: string read fCurrentDirectory
                                      write fCurrentDirectory;
    property FilteredLines: TFilteredOutputLines read fFilteredOutput;
    property StopExecute: boolean read FStopExecute write SetStopExecute;
    property Lines: TIDEMessageLineList read fOutput;
    property LastErrorType: TErrorType read fLastErrorType;
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
    property Caller: TObject read FCaller;
    property ScanLine: TOFScanLine read FScanLine;
    property Tool: TIDEExternalToolOptions read FTool;
  end;
  
  EOutputFilterError = class(Exception)
  end;

type
  TProcessClass = class of TProcess;

var
  TOutputFilterProcess: TProcessClass = nil;

const
  ErrorTypeNames : array[TErrorType] of string = (
      'None','Hint','Note','Warning','Error','Fatal','Panic'
    );

function ErrorTypeNameToType(const Name:string): TErrorType;


implementation


function ErrorTypeNameToType(const Name:string): TErrorType;
begin
  for Result:=Succ(etNone) to High(TErrorType) do
    if CompareText(ErrorTypeNames[Result],Name)=0 then exit;
  Result:=etNone;
end;

{ TOutputFilter }

constructor TOutputFilter.Create;
begin
  inherited Create;
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

function TOutputFilter.Execute(TheProcess: TProcess; aCaller: TObject;
  aTool: TIDEExternalToolOptions): boolean;
const
  BufSize = 4096;
var
  i, Count, LineStart : longint;
  OutputLine, Buf : String;
  TheAsyncProcess: TAsyncProcess;
  LastProcessMessages: TDateTime;
begin
  Result:=true;
  Clear;
  fProcess:=TheProcess;
  FCaller:=aCaller;
  FTool:=aTool;
  FScanLine:=TOFScanLine.Create(Self);
  
  //debugln('TOutputFilter.Execute A CurrentDirectory="',TheProcess.CurrentDirectory,'"');
  fCurrentDirectory:=TrimFilename(fProcess.CurrentDirectory);
  if fCurrentDirectory='' then fCurrentDirectory:=GetCurrentDir;
  fCurrentDirectory:=AppendPathDelim(fCurrentDirectory);
  SetLength(Buf,BufSize);

  OutputLine:='';
  ErrorExists:=true;
  Aborted:=false;
  try
    BeginBufferingOutput;
    
    if fProcess is TAsyncProcess then begin
      TheAsyncProcess:=TAsyncProcess(fProcess);
      TheAsyncProcess.OnReadData:=@OnAsyncReadData;
      TheAsyncProcess.OnTerminate:=@OnAsyncTerminate;
      FAsyncOutput:=TDynamicDataQueue.Create;
    end else
      TheAsyncProcess:=nil;

    fProcess.Execute;
    LastProcessMessages:=Now-1;// force one at start
    repeat
      if (Application<>nil) and (abs(LastProcessMessages-Now)>((1/86400)/3))
      then begin
        LastProcessMessages:=Now;
        Application.ProcessMessages;
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
      OutputLine:=OutputLine+copy(Buf,LineStart,Count-LineStart+1);
    until false;
    //DebugLn('TOutputFilter.Execute After Loop');
    fProcess.WaitOnExit;
    //DebugLn('TOutputFilter.Execute fProcess.ExitStatus=',dbgs(fProcess.ExitStatus));
    if fProcess.ExitStatus=0 then
      ErrorExists:=false;
    if ErrorExists and (ofoExceptionOnError in Options) then
      raise EOutputFilterError.Create('there was an error');
  finally
    EndBufferingOutput;
    fProcess:=nil;
    FreeAndNil(FAsyncOutput);
    if Assigned(OnEndReading) then OnEndReading(Self,fOutput);
    FreeAndNil(FScanLine);
    FTool:=nil;
    FCaller:=nil;
  end;
end;

procedure TOutputFilter.ReadLine(var s: string; DontFilterLine: boolean);
// this is called for every line written by the external tool (=Output)
// it parses the output
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

procedure TOutputFilter.ReadConstLine(const s: string; DontFilterLine: boolean
  );
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
  MsgType: TErrorType;
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
  
  function CheckForCompilingState: boolean;
  var
    AFilename: string;
  begin
    Result:=false;
    if ('Compiling '=copy(s,1,length('Compiling '))) then begin
      // for example 'Compiling ./subdir/unit1.pas'
      fLastMessageType:=omtFPC;
      fLastErrorType:=etNone;
      // add path to history
      if fCompilingHistory=nil then fCompilingHistory:=TStringList.Create;
      i:=length('Compiling ');
      if (length(s)>=i+2) and (s[i+1]='.') and (s[i+2]=PathDelim) then
        inc(i,2);
      AFilename:=TrimFilename(copy(s,i+1,length(s)-i));
      fCompilingHistory.Add(AFilename);
      CurrentMessageParts.Values['Stage']:='FPC';
      CurrentMessageParts.Values['Type']:='Compiling';
      CurrentMessageParts.Values['Filename']:=AFilename;
      Result:=true;
    end;
  end;
  
  function CheckForAssemblingState: boolean;
  begin
    Result:=false;
    if ('Assembling '=copy(s,1,length('Assembling ')))
    then begin
      fLastMessageType:=omtFPC;
      fLastErrorType:=etNone;
      CurrentMessageParts.Values['Stage']:='FPC';
      CurrentMessageParts.Values['Type']:='Assembling';
      Result:=true;
    end;
  end;
  
  function CheckForUrgentMessages: boolean;
  var
    NewLine: String;
    LastFile: string;
    FullFilename: String;
  begin
    Result:=false;
    if ('Fatal: '=copy(s,1,length('Fatal: ')))
    or ('Panic'=copy(s,1,length('Panic')))
    or ('Error: '=copy(s,1,length('Error: ')))
    or ('Closing script ppas.sh'=s)
    then begin
      // always show fatal, panic and linker errors
      fLastMessageType:=omtFPC;
      if ('Panic'=copy(s,1,length('Panic'))) then
        fLastErrorType:=etPanic
      else if ('Fatal: '=copy(s,1,length('Fatal: '))) then
        fLastErrorType:=etFatal
      else if ('Error: '=copy(s,1,length('Error: '))) then
        fLastErrorType:=etError
      else if ('Closing script ppas.sh'=s) then begin
        // linker error
        fLastMessageType:=omtLinker;
        fLastErrorType:=etFatal;
      end;
      if fLastMessageType=omtLinker then
        CurrentMessageParts.Values['Stage']:='Linker'
      else
        CurrentMessageParts.Values['Stage']:='FPC';
      CurrentMessageParts.Values['Type']:=ErrorTypeNames[fLastErrorType];

      NewLine:=s;
      if fLastErrorType in [etPanic,etFatal] then begin
        // fatal and panic errors are not very informative
        // -> prepend current file
        if (fCompilingHistory<>nil) and (fCompilingHistory.Count>0) then begin
          LastFile:=fCompilingHistory[fCompilingHistory.Count-1];
          if not FilenameIsAbsolute(LastFile) then
            FullFilename:=TrimFilename(fCurrentDirectory+LastFile)
          else
            FullFilename:=LastFile;
          if (ofoMakeFilenamesAbsolute in Options)
          and (not FilenameIsAbsolute(LastFile)) then begin
            if FileExists(FullFilename) then
              LastFile:=FullFilename;
          end;
          if FileExists(FullFilename) then begin
            CurrentMessageParts.Values['Filename']:=FullFilename;
            NewLine:=LastFile+'(1,1) '+NewLine;
          end;
        end;
      end;
      DoAddFilteredLine(NewLine);
      if (ofoExceptionOnError in Options) then
        raise EOutputFilterError.Create(NewLine);
      Result:=true;
      exit;
    end;
  end;
  
  function CheckForNoteMessages: boolean;
  begin
    Result:=false;
    if ('Note: '=copy(s,1,length('Note: '))) then begin
      DoAddFilteredLine(s);
      fLastErrorType:=etNote;
      CurrentMessageParts.Values['Stage']:='FPC';
      CurrentMessageParts.Values['Type']:=ErrorTypeNames[fLastErrorType];
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
    Result:=(p+length(Find)-1<=length(Str))
            and (CompareText(Find,copy(s,p,length(Find)))=0);
    if Result then inc(p,length(Find));
  end;

  function CheckForLineProgress: boolean;
  var
    p: Integer;
  begin
    Result:=false;
    p:=1;
    if not CheckForNumber(s,p) then exit;
    if not CheckForChar(s,p,' ') then exit;
    if not CheckForNumber(s,p) then exit;
    if not CheckForChar(s,p,'/') then exit;
    if not CheckForNumber(s,p) then exit;
    if not CheckForChar(s,p,' ') then exit;
    Result:=true;
    // I don't think it should be shown in filtered lines: DoAddFilteredLine(s);
  end;
  
  function CheckForLinesCompiled: boolean;
  var
    p: Integer;
  begin
    Result:=false;
    p:=1;
    if not CheckForNumber(s,p) then exit;
    if not CheckForString(s,p,' Lines compiled, ') then exit;
    if not CheckForNumber(s,p) then exit;
    if not CheckForChar(s,p,'.') then exit;
    if not CheckForNumber(s,p) then exit;
    if not CheckForChar(s,p,' ') then exit;
    Result:=true;
    if (CompilerOptions<>nil)
    and (CompilerOptions.ShowAll or CompilerOptions.ShowSummary) then
      DoAddFilteredLine(s);
  end;

  { For example:
  Size of Code: 1184256 bytes
  Size of initialized data: 519168 bytes
  Size of uninitialized data: 83968 bytes
  Stack space reserved: 262144 bytes
  Stack space commited: 4096 bytes
  }
  function CheckForExecutableInfo: boolean;
  var
    p: Integer;
  begin
    Result:=false;
    p:=1;
    if not (CheckForString(s,p,'Size of Code: ') or
            CheckForString(s,p,'Size of initialized data: ') or
            CheckForString(s,p,'Size of uninitialized data: ') or
            CheckForString(s,p,'Stack space reserved: ') or
            CheckForString(s,p,'Stack space commited: ') or // message contains typo
            CheckForString(s,p,'Stack space committed: ')) then exit;
    if not CheckForNumber(s,p) then exit;
    if not CheckForString(s,p,' bytes') then exit;
    Result:=true;
    if (CompilerOptions<>nil)
    and (CompilerOptions.ShowAll or CompilerOptions.ShowExecInfo) then
      DoAddFilteredLine(s);
  end;
  
  { For example:
    linkerror.o(.text$_main+0x9):linkerror.pas: undefined reference to `NonExistingFunction'
  }
  function CheckForLinkingErrors: boolean;
  begin
  
    Result:=false;
  end;

begin
  Result:=false;
  if s='' then exit;
  // check for 'Compiling <filename>'
  Result:=CheckForCompilingState;
  if Result then exit;
  // check for 'Assembling <filename>'
  Result:=CheckForAssemblingState;
  if Result then exit;
  // check for 'Fatal: ', 'Panic: ', 'Error: ', 'Closing script ppas.sh'
  Result:=CheckForUrgentMessages;
  if Result then exit;
  // check for 'Note: '
  Result:=CheckForNoteMessages;
  if Result then exit;
  // check for '<line> <kb>/<kb>'...
  Result:=CheckForLineProgress;
  if Result then exit;
  // check for '<int> Lines compiled, <int>.<int> sec'
  Result:=CheckForLinesCompiled;
  if Result then exit;
  // check for -vx output
  Result:=CheckForExecutableInfo;
  if Result then exit;
  // check for linking errors
  Result:=CheckForLinkingErrors;
  if Result then exit;

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
  if (i>=length(s)) or (not (s[i] in ['A'..'Z'])) then exit;
  j:=i+1;
  while (j<=length(s)) and (s[j] in ['a'..'z']) do inc(j);
  if (j+1>length(s)) or (s[j]<>':') or (s[j+1]<>' ') then exit;
  MessageStartPos:=j+2;
  MsgTypeName:=copy(s,i,j-i);
  for MsgType:=Succ(etNone) to High(TErrorType) do begin
    if ErrorTypeNames[MsgType]=MsgTypeName then begin
      // this is a freepascal compiler message
      // -> filter message
      fLastErrorType:=MsgType;
      fLastMessageType:=omtFPC;
      CurrentMessageParts.Values['Stage']:='FPC';
      CurrentMessageParts.Values['Type']:=ErrorTypeNames[fLastErrorType];
      CurrentMessageParts.Values['Line']:=
                 copy(s,LineNumberStartPos,LineNumberEndPos-LineNumberStartPos);
      CurrentMessageParts.Values['Column']:=
           copy(s,ColumnNumberStartPos,ColumnNumberEndPos-ColumnNumberStartPos);
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
      Filename:=copy(Msg,1,FilenameEndPos);
      if FilenameIsAbsolute(Filename) then begin
        AbsFilename:=Filename;
      end else begin
        AbsFilename:=TrimFilename(fCurrentDirectory+Filename);
        if not FileExists(AbsFilename) then begin
          AbsFilename:='';
        end;
      end;
      if (ofoMakeFilenamesAbsolute in Options) then begin
        if (AbsFilename<>'') and (AbsFilename<>Filename) then begin
          Filename:=AbsFilename;
          Msg:=Filename+copy(Msg,FilenameEndPos+1,length(Msg)-FilenameEndPos);
        end;
      end else begin
        if FileIsInPath(Filename,fCurrentDirectory) then begin
          Filename:=CreateRelativePath(Filename,fCurrentDirectory);
          Msg:=Filename+copy(Msg,FilenameEndPos+1,length(Msg)-FilenameEndPos);
        end;
      end;
      //DebugLn('TOutputFilter.ReadFPCompilerLine AbsFilename=',AbsFilename,' Filename=',Filename,' Dir=',fCurrentDirectory);
      if AbsFilename<>'' then
        CurrentMessageParts.Values['Filename']:=AbsFilename
      else
        CurrentMessageParts.Values['Filename']:=Filename;

      // add line
      if not SkipMessage then
        DoAddFilteredLine(Msg);
        
      if (ofoExceptionOnError in Options) and (MsgType in [etPanic, etFatal])
      then
        raise EOutputFilterError.Create(Msg);
        
      Result:=true;
      exit;
    end;
  end;
end;

function TOutputFilter.GetSourcePosition(const Line: string; var Filename:string;
  var CaretXY: TPoint; var MsgType: TErrorType): boolean;
{ This assumes the line has one of the following formats
<filename>(123,45) <ErrorType>: <some text>
<filename>(123) <ErrorType>: <some text>
<filename>(456) <ErrorType>: <some text> in line (123)
Fatal: <some text>
}
var StartPos, EndPos: integer;
begin
  Result:=false;
  if copy(Line,1,7)='Fatal: ' then begin
    Result:=true;
    Filename:='';
    MsgType:=etFatal;
    exit;
  end;
  StartPos:=1;
  // find filename
  EndPos:=StartPos;
  while (EndPos<=length(Line)) and (Line[EndPos]<>'(') do inc(EndPos);
  if EndPos>length(Line) then exit;
  FileName:=copy(Line,StartPos,EndPos-StartPos);
  // read linenumber
  StartPos:=EndPos+1;
  EndPos:=StartPos;
  while (EndPos<=length(Line)) and (Line[EndPos] in ['0'..'9']) do inc(EndPos);
  if EndPos>length(Line) then exit;
  CaretXY.X:=1;
  CaretXY.Y:=StrToIntDef(copy(Line,StartPos,EndPos-StartPos),-1);
  if Line[EndPos]=',' then begin
    // format: <filename>(123,45) <ErrorType>: <some text>
    // read column
    StartPos:=EndPos+1;
    EndPos:=StartPos;
    while (EndPos<=length(Line)) and (Line[EndPos] in ['0'..'9']) do inc(EndPos);
    if EndPos>length(Line) then exit;
    CaretXY.X:=StrToIntDef(copy(Line,StartPos,EndPos-StartPos),-1);
    // read error type
    StartPos:=EndPos+2;
    while (EndPos<=length(Line)) and (Line[EndPos]<>':') do inc(EndPos);
    if EndPos>length(Line) then exit;
    MsgType:=ErrorTypeNameToType(copy(Line,StartPos,EndPos-StartPos));
    Result:=true;
  end else if Line[EndPos]=')' then begin
    // <filename>(123) <ErrorType>: <some text>
    // <filename>(456) <ErrorType>: <some text> in line (123)
    // read error type
    StartPos:=EndPos+2;
    while (EndPos<=length(Line)) and (Line[EndPos]<>':') do inc(EndPos);
    if EndPos>length(Line) then exit;
    MsgType:=ErrorTypeNameToType(copy(Line,StartPos,EndPos-StartPos));
    // read second linenumber (more useful)
    while (EndPos<=length(Line)) and (Line[EndPos]<>'(') do inc(EndPos);
    if EndPos>length(Line) then begin
      // format: <filename>(123) <ErrorType>: <some text>
      Result:=true;
      exit;
    end;
    StartPos:=EndPos+1;
    EndPos:=StartPos;
    while (EndPos<=length(Line)) and (Line[EndPos] in ['0'..'9']) do inc(EndPos);
    if EndPos>length(Line) then exit;
    CaretXY.Y:=StrToIntDef(copy(Line,StartPos,EndPos-StartPos),-1);
    Result:=true;
  end;
end;

function TOutputFilter.IsHintForUnusedUnit(const OutputLine,
  MainSrcFile: string): boolean;
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
    OnAddFilteredLine(s,fCurrentDirectory,OriginalIndex);
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

function TOutputFilter.SearchIncludeFile(const ShortIncFilename: string
  ): string;
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
      if FileExists(Result) then begin
        // file found in search dir
        Result:=CleanAndExpandFilename(Result);
        exit;
      end;
      AlreadySearchedPaths:=MergeSearchPaths(AlreadySearchedPaths,FullDir);
      // search with include path of directory
      if Assigned(OnGetIncludePath) then begin
        IncludePath:=TrimSearchPath(OnGetIncludePath(FullDir,false),FullDir);
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

destructor TOutputFilter.Destroy;
begin
  fFilteredOutput.Free;
  fOutput.Free;
  fMakeDirHistory.Free;
  fCompilingHistory.Free;
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
      InternalSetCurrentDirectory(copy(s,i,length(s)-i));
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
      DoAddFilteredLine(s);
      if CompareText(copy(MakeMsg,1,5),'Error')=0 then
        if (ofoExceptionOnError in Options) then
          raise EOutputFilterError.Create(s);
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
  HalfASecond =0.5/(24*60*60); // 0.5 divided by the number of seconds per day
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
  dec(FBufferingOutputLock);
  if FBufferingOutputLock<0 then RaiseException('');
  if FBufferingOutputLock=0 then
    WriteOutput(true);
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

end.

