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

{$DEFINE ShowTriedFiles}

uses
  Classes, SysUtils, Forms, Controls, CompilerOptions, Project, Process,
  IDEProcs, FileCtrl, LazConf;

type
  TOnOutputString = procedure(const Msg, Directory: String) of Object;
  TOnGetIncludePath = function(const Directory: string): string of object;
  
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

  TOutputFilter = class
  private
    FCompilerOptions: TBaseCompilerOptions;
    FBufferingOutputLock: integer;
    fCurrentDirectory: string;
    fFilteredOutput: TStringList;
    fOnReadLine: TOnOutputString;
    fOutput: TStringList;
    fLastErrorType: TErrorType;
    fLastMessageType: TOutputMessageType;
    fCompilingHistory: TStringList;
    fMakeDirHistory: TStringList;
    fOnGetIncludePath: TOnGetIncludePath;
    fOnOutputString: TOnOutputString;
    fOptions: TOuputFilterOptions;
    FStopExecute: boolean;
    FLastOutputLine: integer;
    fLastOutputTime: TDateTime;
    procedure DoAddFilteredLine(const s: string);
    procedure DoAddLastLinkerMessages(SkipLastLine: boolean);
    procedure DoAddLastAssemblerMessages;
    function SearchIncludeFile(const ShortIncFilename: string): string;
    procedure SetStopExecute(const AValue: boolean);
    procedure InternalSetCurrentDirectory(const Dir: string);
  public
    ErrorExists: boolean;
    Aborted: boolean;
    function Execute(TheProcess: TProcess): boolean;
    function GetSourcePosition(const Line: string; var Filename:string;
      var CaretXY: TPoint; var MsgType: TErrorType): boolean;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    function IsHintForUnusedUnit(const OutputLine,
      MainSrcFile: string): boolean;
    function IsParsing: boolean;
    procedure ReadLine(const s: string; DontFilterLine: boolean);
    function ReadFPCompilerLine(const s: string): boolean;
    function ReadMakeLine(const s: string): boolean;
    procedure WriteOutput(Flush: boolean);
    procedure BeginBufferingOutput;
    procedure EndBufferingOutput;
  public
    property CurrentDirectory: string read fCurrentDirectory;
    property FilteredLines: TStringList read fFilteredOutput;
    property StopExecute: boolean read FStopExecute write SetStopExecute;
    property Lines: TStringList read fOutput;
    property LastErrorType: TErrorType read fLastErrorType;
    property LastMessageType: TOutputMessageType read fLastMessageType;
    property OnGetIncludePath: TOnGetIncludePath
                                 read fOnGetIncludePath write fOnGetIncludePath;
    property OnReadLine: TOnOutputString read fOnReadLine write fOnReadLine;
    property OnOutputString: TOnOutputString
                                     read fOnOutputString write fOnOutputString;
    property Options: TOuputFilterOptions read fOptions write fOptions;
    property CompilerOptions: TBaseCompilerOptions read FCompilerOptions
                                               write FCompilerOptions;
  end;
  
  EOutputFilterError = class(Exception)
  end;

const
  ErrorTypeNames : array[TErrorType] of string = (
      'None','Hint','Note','Warning','Error','Fatal','Panic'
    );

function ErrorTypeNameToType(const Name:string): TErrorType;


implementation


function ErrorTypeNameToType(const Name:string): TErrorType;
begin
  for Result:=Succ(etNone) to High(TErrorType) do
    if AnsiCompareText(ErrorTypeNames[Result],Name)=0 then exit;
  Result:=etNone;
end;


{ TOutputFilter }

constructor TOutputFilter.Create;
begin
  inherited Create;
  fFilteredOutput:=TStringList.Create;
  fOutput:=TStringList.Create;
  Clear;
end;

procedure TOutputFilter.Clear;
begin
  fOutput.Clear;
  FLastOutputLine:=-1;
  fFilteredOutput.Clear;
  if fCompilingHistory<>nil then fCompilingHistory.Clear;
  if fMakeDirHistory<>nil then fMakeDirHistory.Clear;
  FStopExecute:=false;
end;

function TOutputFilter.Execute(TheProcess: TProcess): boolean;
const
  BufSize = 1000;
var
  i, Count, LineStart : longint;
  OutputLine, Buf : String;
begin
  Result:=true;
  Clear;
  TheProcess.Execute;
  fCurrentDirectory:=TrimFilename(TheProcess.CurrentDirectory);
  if fCurrentDirectory='' then fCurrentDirectory:=GetCurrentDir;
  fCurrentDirectory:=AppendPathDelim(fCurrentDirectory);
  SetLength(Buf,BufSize);

  OutputLine:='';
  ErrorExists:=true;
  Aborted:=false;
  try
    BeginBufferingOutput;
    repeat
      Application.ProcessMessages;
      if StopExecute then begin
        TheProcess.Terminate(0);
        Aborted:=true;
        Result:=false;
        ReadLine('aborted',true);
        break;
      end;

      if TheProcess.Output<>nil then
        Count:=TheProcess.Output.Read(Buf[1],length(Buf))
      else
        Count:=0;
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
    until Count=0;
    TheProcess.WaitOnExit;
    if TheProcess.ExitStatus=0 then
      ErrorExists:=false;
    if ErrorExists and (ofoExceptionOnError in Options) then
      raise EOutputFilterError.Create('there was an error');
  finally
    EndBufferingOutput;
  end;
end;

procedure TOutputFilter.ReadLine(const s: string; DontFilterLine: boolean);
begin
  //writeln('TOutputFilter: "',s,'"');
  fLastMessageType:=omtNone;
  fLastErrorType:=etNone;
  fOutput.Add(s);
  WriteOutput(false);
  if Assigned(OnReadLine) then
    OnReadLine(s,fCurrentDirectory);

  if DontFilterLine or (ofoShowAll in Options) then begin
    DoAddFilteredLine(s);
  end else if (ofoSearchForFPCMessages in Options) and (ReadFPCompilerLine(s))
  then begin
    exit;
  end else if (ofoSearchForMakeMessages in Options) and (ReadMakeLine(s))
  then begin
    exit;
  end;
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
  NewFilename: String;
  
  function CheckForCompilingState: boolean;
  var
    AFilename: string;
  begin
    Result:=false;
    if ('Compiling '=copy(s,1,length('Compiling '))) then begin
      // for example 'Compiling ./subdir/unit1.pas'
      fLastMessageType:=omtFPC;
      fLastErrorType:=etNone;
      Result:=true;
      // add path to history
      if fCompilingHistory=nil then fCompilingHistory:=TStringList.Create;
      i:=length('Compiling ');
      if (length(s)>=i+2) and (s[i+1]='.') and (s[i+2]=PathDelim) then
        inc(i,2);
      AFilename:=TrimFilename(copy(s,i+1,length(s)-i));
      fCompilingHistory.Add(AFilename);
    end;
  end;
  
  function CheckForAssemblingState: boolean;
  begin
    Result:=false;
    if ('Assembling '=copy(s,1,length('Assembling ')))
    then begin
      fLastMessageType:=omtFPC;
      fLastErrorType:=etNone;
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
    or ('Closing script ppas.sh'=s)
    then begin
      // always show fatal, panic and linker errors
      fLastMessageType:=omtFPC;
      if ('Panic'=copy(s,1,length('Panic'))) then
        fLastErrorType:=etPanic
      else if ('Fatal: '=copy(s,1,length('Fatal: '))) then
        fLastErrorType:=etFatal
      else if ('Closing script ppas.sh'=s) then begin
        // linker error
        fLastMessageType:=omtLinker;
        fLastErrorType:=etFatal;
      end;
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
          if FileExists(FullFilename) then
            NewLine:=LastFile+'(1,1) '+NewLine;
        end;
      end;
      DoAddFilteredLine(NewLine);
      if (ofoExceptionOnError in Options) then
        raise EOutputFilterError.Create(NewLine);
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
  // check for 'Fatal: ', 'Panic: ', 'Closing script ppas.sh'
  Result:=CheckForUrgentMessages;
  if Result then exit;
  // check for '<line> <kb>/<kb> Kb Free'
  Result:=CheckForLineProgress;
  if Result then exit;
  
  // search for round bracket open
  i:=1;
  while (i<=length(s)) and (s[i]<>'(') do inc(i);
  FilenameEndPos:=i-1;
  inc(i);
  // search for number
  if not CheckForNumber(s,i) then exit;
  if (i<length(s)) and (s[i]=',') and (s[i+1] in ['0'..'9']) then begin
    // skip second number
    inc(i);
    while (i<=length(s)) and (s[i] in ['0'..'9']) do inc(i);
  end;
  // search for ') <ErrorType>: '
  if not CheckForChar(s,i,')') then exit;
  if not CheckForChar(s,i,' ') then exit;
  if (i>=length(s)) or (not (s[i] in ['A'..'Z'])) then exit;
  j:=i+1;
  while (j<=length(s)) and (s[j] in ['a'..'z']) do inc(j);
  if (j+1>length(s)) or (s[j]<>':') or (s[j+1]<>' ') then exit;
  MsgTypeName:=copy(s,i,j-i);
  for MsgType:=Succ(etNone) to High(TErrorType) do begin
    if ErrorTypeNames[MsgType]=MsgTypeName then begin
      // this is a freepascal compiler message
      // -> filter message
      fLastErrorType:=MsgType;
      fLastMessageType:=omtFPC;
      
      SkipMessage:=true;
      
      case MsgType of

      etHint:
        begin
          SkipMessage:=(CompilerOptions<>nil)
                       and (not (CompilerOptions.ShowHints
                            or CompilerOptions.ShowAll));
          if (not SkipMessage)
          and (CompilerOptions<>nil)
          and (not CompilerOptions.ShowAll)
          and (not CompilerOptions.ShowHintsForUnusedUnitsInMainSrc) then
          begin
            MainSrcFilename:=CompilerOptions.GetDefaultMainSourceFileName;
            if (MainSrcFilename<>'')
            and (IsHintForUnusedUnit(s,MainSrcFilename)) then
              SkipMessage:=true;
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
      if (ofoMakeFilenamesAbsolute in Options) then begin
        if not FilenameIsAbsolute(Filename) then begin
          NewFilename:=TrimFilename(fCurrentDirectory+Filename);
          if FileExists(NewFilename) then
            Msg:=NewFilename
                 +copy(Msg,FilenameEndPos+1,length(Msg)-FilenameEndPos);
        end;
      end else begin
        if FileIsInPath(Filename,fCurrentDirectory) then begin
          Filename:=CreateRelativePath(Filename,fCurrentDirectory);
          Msg:=Filename+copy(Msg,FilenameEndPos+1,length(Msg)-FilenameEndPos);
        end;
      end;

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

procedure TOutputFilter.DoAddFilteredLine(const s: string);
begin
  fFilteredOutput.Add(s);
  if Assigned(OnOutputString) then
    OnOutputString(s,fCurrentDirectory);
end;

procedure TOutputFilter.DoAddLastLinkerMessages(SkipLastLine: boolean);
var i: integer;
begin
  // read back to 'Linking' message
  i:=fOutput.Count-1;
  while (i>=0) and (LeftStr(fOutput[i],length('Linking '))<>'Linking ') do
    dec(i);
  inc(i);
  // output skipped messages
  while (i<fOutput.Count) do begin
    if (fOutput[i]<>'')
    and ((i<fOutput.Count-1) or (not SkipLastLine)) then
      DoAddFilteredLine(fOutput[i]);
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
  while (i>=0) and (RightStr(fOutput[i],length(AsmStartMsg))<>AsmStartMsg) do
    dec(i);
  if i<0 then exit;
  while (i<fOutput.Count-1) do begin
    if (fOutput[i]<>'') then
      DoAddFilteredLine(fOutput[i]);
    inc(i);
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
      IncludePath:=TrimSearchPath(OnGetIncludePath(FullDir),FullDir);
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
end;

procedure TOutputFilter.SetStopExecute(const AValue: boolean);
begin
  FStopExecute:=AValue;
end;

procedure TOutputFilter.InternalSetCurrentDirectory(const Dir: string);
begin
  fCurrentDirectory:=TrimFilename(AppendPathDelim(Dir));
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
  MakeBeginPattern:= 'make' + GetDefaultExecutableExt + '[';
  i:=length(MakeBeginPattern);
  if copy(s,1,i)<>MakeBeginPattern then exit;
  Result:=true;
  
  inc(i);
  if (i>length(s)) or (not (s[i] in ['0'..'9'])) then exit;
  while (i<=length(s)) and (s[i] in ['0'..'9']) do inc(i);
  if (i>length(s)) or (s[i]<>']') then exit;
  // check for enter directory
  if copy(s,i,length(EnterDirPattern))=EnterDirPattern then
  begin
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
    if (fMakeDirHistory<>nil) and (fMakeDirHistory.Count>0) then begin
      InternalSetCurrentDirectory(fMakeDirHistory[fMakeDirHistory.Count-1]);
      fMakeDirHistory.Delete(fMakeDirHistory.Count-1);
      exit;
    end else begin
      // leaving what directory???
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
    if AnsiCompareText(copy(MakeMsg,1,5),'Error')=0 then
      if (ofoExceptionOnError in Options) then
        raise EOutputFilterError.Create(s);
    exit;
  end;
end;

procedure TOutputFilter.WriteOutput(Flush: boolean);
// write output in blocks. This way slow terminals don't slow down the IDE.
var
  CurTime: Double;
  s: String;
begin
  CurTime:=Now;
  if ((CurTime-fLastOutputTime)>500) or Flush or (FBufferingOutputLock<=0) then
  begin
    s:='';
    while FLastOutputLine<fOutput.Count-1 do begin
      inc(FLastOutputLine);
      s:=s+fOutput[FLastOutputLine]+LineEnding;
    end;
    if s<>'' then write(s);
  end;
  fLastOutputTime:=CurTime;
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


end.


