{  $Id$  }
{
 /***************************************************************************
                        outputfilter.pas  -  Lazarus IDE unit
                        -------------------------------------
                   TOutputFilter is responsible for parsing output of external
                   tools and to filter important messages.

 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
unit OutputFilter;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, CompilerOptions, Project, Process,
  IDEProcs;

type
  TOnOutputString = procedure (const Value: String) of Object;
  
  TOuputFilterOption = (
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
    fCurrentDirectory: string;
    fFilteredOutput: TStringList;
    fLastErrorType: TErrorType;
    fLastMessageType: TOutputMessageType;
    fMakeDirHistory: TStringList;
    fOnOutputString: TOnOutputString;
    fOptions: TOuputFilterOptions;
    fProject: TProject;
    fPrgSourceFilename: string;
    procedure DoAddFilteredLine(const s: string);
  public
    procedure Execute(TheProcess: TProcess);
    function GetSourcePosition(const Line: string; var Filename:string;
      var CaretXY: TPoint; var MsgType: TErrorType): boolean;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    function IsHintForUnusedProjectUnit(const OutputLine,
      ProgramSrcFile: string): boolean;
    function IsParsing: boolean;
    procedure ReadLine(const s: string; DontFilterLine: boolean);
    function ReadFPCompilerLine(const s: string): boolean;
    function ReadMakeLine(const s: string): boolean;
    property CurrentDirectory: string read fCurrentDirectory;
    property FilteredLines: TStringList read fFilteredOutput;
    property LastErrorType: TErrorType read fLastErrorType;
    property LastMessageType: TOutputMessageType read fLastMessageType;
    property PrgSourceFilename: string
      read fPrgSourceFilename write fPrgSourceFilename;
    property OnOutputString: TOnOutputString
      read fOnOutputString write fOnOutputString;
    property Options: TOuputFilterOptions read fOptions write fOptions;
    property Project: TProject read fProject write fProject;
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
  Clear;
end;

procedure TOutputFilter.Clear;
begin
  fFilteredOutput.Clear;
end;

procedure TOutputFilter.Execute(TheProcess: TProcess);
const
  BufSize = 1024;
var
  i, Count, LineStart : longint;
  OutputLine, Buf : String;
begin
  TheProcess.Execute;
  fCurrentDirectory:=TheProcess.CurrentDirectory;
  if fCurrentDirectory='' then fCurrentDirectory:=GetCurrentDir;
  if (fCurrentDirectory<>'')
  and (fCurrentDirectory[length(fCurrentDirectory)]<>PathDelim) then
    fCurrentDirectory:=fCurrentDirectory+PathDelim;
  if fMakeDirHistory<>nil then fMakeDirHistory.Clear;
  SetLength(Buf,BufSize);
  Application.ProcessMessages;

  fFilteredOutput.Clear;
  OutputLine:='';
  repeat
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
        OutputLine:='';
        if (i<Count) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1])
        then
          inc(i);
        LineStart:=i+1;
      end;
      inc(i);
    end;
    OutputLine:=copy(Buf,LineStart,Count-LineStart+1);
  until Count=0;
  TheProcess.WaitOnExit;
end;

procedure TOutputFilter.ReadLine(const s: string; DontFilterLine: boolean);
begin
  writeln('TOutputFilter: "',s,'"');
  fLastMessageType:=omtNone;
  fLastErrorType:=etNone;
  if DontFilterLine then begin
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
var i, j, FilenameEndPos: integer;
  MsgTypeName, Filename, Msg: string;
  MsgType: TErrorType;
  SkipMessage: boolean;
begin
  Result:=false;
  if ('Compiling '=copy(s,1,length('Compiling ')))
  or ('Assembling '=copy(s,1,length('Assembling ')))
  then begin
    fLastMessageType:=omtFPC;
    fLastErrorType:=etNone;
    Result:=true;
    exit;
  end;
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
      fLastMessageType:=omtLinker;
      fLastErrorType:=etFatal;
    end;
    fFilteredOutput.Add(s);
    if (ofoExceptionOnError in Options) then
      raise EOutputFilterError.Create(s);
    Result:=true;
    exit;
  end;
  // search for round bracket open
  i:=1;
  while (i<=length(s)) and (s[i]<>'(') do inc(i);
  FilenameEndPos:=i-1;
  inc(i);
  // search for number
  if (i>=length(s)) or (not (s[i] in ['0'..'9'])) then exit;
  while (i<=length(s)) and (s[i] in ['0'..'9']) do inc(i);
  if (i<length(s)) and (s[i]=',') and (s[i+1] in ['0'..'9']) then begin
    // skip second number
    inc(i);
    while (i<=length(s)) and (s[i] in ['0'..'9']) do inc(i);
  end;
  // search for ') <ErrorType>: '
  inc(i,2);
  if (i>=length(s)) or (s[i-2]<>')') or (S[i-1]<>' ')
  or (not (s[i] in ['A'..'Z'])) then exit;
  j:=i+1;
  while (j<=length(s)) and (s[j] in ['a'..'z']) do inc(j);
  if (j+1>length(s)) or (s[j]<>':') or (s[j+1]<>' ') then exit;
  MsgTypeName:=copy(s,i,j-i);
  for MsgType:=Succ(etNone) to High(TErrorType) do begin
    if ErrorTypeNames[MsgType]=MsgTypeName then begin
      // this is a freepascal compiler message
      // -> filter message
      fLastErrorType:=MsgType;
      SkipMessage:=true;
      if Project<>nil then begin
        case MsgType of

        etHint:
          begin
            SkipMessage:=not (Project.CompilerOptions.ShowHints
                              or Project.CompilerOptions.ShowAll);
            if (not SkipMessage)
            and (not Project.CompilerOptions.ShowAll)
            and (not Project.CompilerOptions.ShowHintsForUnusedProjectUnits)
            and (PrgSourceFilename<>'')
            and (IsHintForUnusedProjectUnit(s,PrgSourceFilename)) then
              SkipMessage:=true;
          end;

        etNote:
          begin
            SkipMessage:=not (Project.CompilerOptions.ShowNotes
                              or Project.CompilerOptions.ShowAll);
          end;

        etError:
          begin
            SkipMessage:=not (Project.CompilerOptions.ShowErrors
                              or Project.CompilerOptions.ShowAll);
          end;

        etWarning:
          begin
            SkipMessage:=not (Project.CompilerOptions.ShowWarn
                              or Project.CompilerOptions.ShowAll);
          end;

        etPanic, etFatal:
          SkipMessage:=false;

        end;
      end else
        SkipMessage:=false;
      Msg:=s;
      if (ofoMakeFilenamesAbsolute in Options) then begin
        Filename:=copy(s,1,FilenameEndPos);
        if not FilenameIsAbsolute(Filename) then
          Msg:=fCurrentDirectory+Msg;
      end;
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
    // <filename>(456) <ErrorType>: <some text> in line (123)
    // read error type
    StartPos:=EndPos+2;
    while (EndPos<=length(Line)) and (Line[EndPos]<>':') do inc(EndPos);
    if EndPos>length(Line) then exit;
    MsgType:=ErrorTypeNameToType(copy(Line,StartPos,EndPos-StartPos));
    // read second linenumber (more useful)
    while (EndPos<=length(Line)) and (Line[EndPos]<>'(') do inc(EndPos);
    if EndPos>length(Line) then exit;
    StartPos:=EndPos+1;
    EndPos:=StartPos;
    while (EndPos<=length(Line)) and (Line[EndPos] in ['0'..'9']) do inc(EndPos);
    if EndPos>length(Line) then exit;
    CaretXY.Y:=StrToIntDef(copy(Line,StartPos,EndPos-StartPos),-1);
    Result:=true;
  end;
end;

function TOutputFilter.IsHintForUnusedProjectUnit(const OutputLine,
  ProgramSrcFile: string): boolean;
{ recognizes hints of the form

  mainprogram.pp(5,35) Hint: Unit UNUSEDUNIT not used in mainprogram
}
var Filename: string;
begin
  Result:=false;
  Filename:=ExtractFilename(ProgramSrcFile);
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
    OnOutputString(s);
end;

destructor TOutputFilter.Destroy;
begin
  fFilteredOutput.Free;
  fMakeDirHistory.Free;
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
}
var i: integer;
begin
  Result:=false;
  i:=length('make[');
  if copy(s,1,i)<>'make[' then exit;
  inc(i);
  if (i>length(s)) or (not (s[i] in ['0'..'9'])) then exit;
  while (i<=length(s)) and (s[i] in ['0'..'9']) do inc(i);
  if (i>length(s)) or (s[i]<>']') then exit;
  if copy(s,i,length(']: Leaving directory `'))=']: Leaving directory `' then
  begin
    if (fMakeDirHistory<>nil) and (fMakeDirHistory.Count>0) then begin
      fCurrentDirectory:=fMakeDirHistory[fMakeDirHistory.Count-1];
      fMakeDirHistory.Delete(fMakeDirHistory.Count-1);
      Result:=true;
      exit;
    end else begin
      // leaving what directory???
      fCurrentDirectory:='';
    end;
  end;
  if copy(s,i,length(']: Entering directory `'))=']: Entering directory `' then
  begin
    inc(i,length(']: Entering directory `'));
    if (fCurrentDirectory<>'') then begin
      if (fMakeDirHistory=nil) then fMakeDirHistory:=TStringList.Create;
      fMakeDirHistory.Add(fCurrentDirectory);
    end;
    fCurrentDirectory:=copy(s,i,length(s)-i);
    if (fCurrentDirectory<>'')
    and (fCurrentDirectory[length(fCurrentDirectory)]<>PathDelim) then
      fCurrentDirectory:=fCurrentDirectory+PathDelim;
    Result:=true;
    exit;
  end;
end;


end.


