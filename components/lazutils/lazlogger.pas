unit LazLogger;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, types, math, LazLoggerBase, LazClasses;

type

  //TLazLoggerLogGroupFlag = LazLoggerBase.TLazLoggerLogGroupFlag;
  //TLazLoggerLogGroupFlags = LazLoggerBase.TLazLoggerLogGroupFlags;
  //TLazLoggerLogGroup = LazLoggerBase.TLazLoggerLogGroup;
  PLazLoggerLogGroup = LazLoggerBase.PLazLoggerLogGroup;

  //TLazLoggerWriteEvent = LazLoggerBase.LazLoggerBase.;

  //TLazLogger = LazLoggerBase.TLazLogger;

{$DEFINE USED_BY_LAZLOGGER}
{$I LazLoggerIntf.inc}


function DbgStr(const StringWithSpecialChars: string): string; overload;
function DbgStr(const StringWithSpecialChars: string; StartPos, Len: PtrInt): string; overload;
function DbgStr(const p: PChar; Len: PtrInt): string; overload;
function DbgWideStr(const StringWithSpecialChars: widestring): string; overload;

function ConvertLineEndings(const s: string): string;
procedure ReplaceSubstring(var s: string; StartPos, Count: SizeInt;
                           const Insertion: string);

type

  { TLazLoggerFileHandle }

  TLazLoggerFileHandle = class
  private
    FActiveLogText: PText; // may point to stdout
    FCloseLogFileBetweenWrites: Boolean;
    FLogName: String;
    FLogText: Text;
    FLogTextInUse, FLogTextFailed: Boolean;
    FUseStdOut: Boolean;
    procedure DoOpenFile;
    procedure DoCloseFile;
    procedure SetCloseLogFileBetweenWrites(AValue: Boolean);
    procedure SetLogName(AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure OpenFile;
    procedure CloseFile;

    procedure WriteToFile(const s: string); inline;
    procedure WriteLnToFile(const s: string); inline;

    property  LogName: String read FLogName write SetLogName;
    property  UseStdOut: Boolean read FUseStdOut write FUseStdOut;
    property  CloseLogFileBetweenWrites: Boolean read FCloseLogFileBetweenWrites write SetCloseLogFileBetweenWrites;
  end;

  { TLazLoggerFile }

  TLazLoggerFile = class(TLazLoggerWithGroupParam)
  private
    FFileHandle: TLazLoggerFileHandle;
    FOnDbgOut: TLazLoggerWriteEvent;
    FOnDebugLn: TLazLoggerWriteEvent;


    FEnvironmentForLogFileName: String;
    //FLogName: String;

    FParamForLogFileName: String;
    FGetLogFileNameDone: Boolean;

    FDebugNestLvl: Integer;
    FDebugIndent: String;
    FDebugNestAtBOL: Boolean;

    function  GetFileHandle: TLazLoggerFileHandle;
    procedure SetEnvironmentForLogFileName(AValue: String);
    procedure SetFileHandle(AValue: TLazLoggerFileHandle);
    procedure SetParamForLogFileName(AValue: String);
    function  GetLogFileName: string;
  private
    // forward to TLazLoggerFileHandle
    function  GetCloseLogFileBetweenWrites: Boolean;
    function  GetLogName: String;
    function  GetUseStdOut: Boolean;
    procedure SetCloseLogFileBetweenWrites(AValue: Boolean);
    procedure SetLogName(AValue: String);
    procedure SetUseStdOut(AValue: Boolean);
  protected
    procedure DoInit; override;
    procedure DoFinsh; override;

    procedure IncreaseIndent; overload; override;
    procedure DecreaseIndent; overload; override;
    procedure IncreaseIndent(LogGroup: PLazLoggerLogGroup); overload; override;
    procedure DecreaseIndent(LogGroup: PLazLoggerLogGroup); overload; override;
    procedure IndentChanged; override;
    procedure CreateIndent; virtual;

    procedure DoDbgOut(const s: string); override;
    procedure DoDebugLn(const s: string); override;
    procedure DoDebuglnStack(const s: string); override;

    property FileHandle: TLazLoggerFileHandle read GetFileHandle write SetFileHandle;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Src: TLazLogger); override;
    // A param on the commandline, that may contain the name (if not already set)
    // example/default: --debug-log=
    property  ParamForLogFileName: String read FParamForLogFileName write SetParamForLogFileName;
    // Environment to specify log file name (* replaced by param(0))
    // example/default: *_debuglog
    property  EnvironmentForLogFileName: String read FEnvironmentForLogFileName write SetEnvironmentForLogFileName; // "*" will be replaced by appname

    property  OnDebugLn: TLazLoggerWriteEvent read FOnDebugLn write FOnDebugLn;
    property  OnDbgOut:  TLazLoggerWriteEvent read FOnDbgOut write FOnDbgOut;

    // forward to TLazLoggerFileHandle
    property  LogName: String read GetLogName write SetLogName;
    property  UseStdOut: Boolean read GetUseStdOut write SetUseStdOut;
    property  CloseLogFileBetweenWrites: Boolean read GetCloseLogFileBetweenWrites write SetCloseLogFileBetweenWrites;
  end;

function GetDebugLogger: TLazLoggerFile; inline;
procedure SetDebugLogger(ALogger: TLazLoggerFile);

property DebugLogger: TLazLoggerFile read GetDebugLogger write SetDebugLogger;

implementation

{$I LazLoggerImpl.inc}

{$ifdef wince}
const
  Str_LCL_Debug_File = 'lcldebug.log';
{$endif}

(* Creation / Access *)

function CreateDebugLogger: TRefCountedObject;
begin
  Result := TLazLoggerFile.Create;
  TLazLoggerFile(Result).Assign(GetExistingDebugLogger);
end;

function GetDebugLogger: TLazLoggerFile; inline;
begin
  Result := TLazLoggerFile(LazLoggerBase.DebugLogger);
end;

procedure SetDebugLogger(ALogger: TLazLoggerFile);
begin
  LazLoggerBase.DebugLogger := ALogger;
end;

(* ArgV *)


{ TLazLoggerFileHandle }

procedure TLazLoggerFileHandle.DoOpenFile;
var
  fm: Byte;
begin
  if FActiveLogText <> nil then exit;

  if (not FLogTextFailed) and (length(FLogName)>0)
     {$ifNdef WinCE}
     and (DirPathExists(ExtractFileDir(FLogName)))
     {$endif}
  then begin
    fm:=Filemode;
    try
      {$ifdef WinCE}
        Assign(FLogText, FLogName);
        {$I-}
        Append(FLogText);
        if IOResult <> 0 then
          Rewrite(FLogText);
        {$I+}
      {$else}
        Filemode:=fmShareDenyNone;
        Assign(FLogText, FLogName);
        if FileExistsUTF8(FLogName) then
          Append(FLogText)
        else
          Rewrite(FLogText);
      {$endif}
      FActiveLogText := @FLogText;
      FLogTextInUse := true;
    except
      FLogTextInUse := false;
      FActiveLogText := nil;
      FLogTextFailed := True;
      // Add extra line ending: a dialog will be shown in windows gui application
      writeln(StdOut, 'Cannot open file: ', FLogName+LineEnding);
    end;
    Filemode:=fm;
  end;

  if (not FLogTextInUse) and (FUseStdOut) then
  begin
    if not(TextRec(Output).Mode=fmClosed) then
      FActiveLogText := @Output;
  end;
end;

procedure TLazLoggerFileHandle.DoCloseFile;
begin
  if FLogTextInUse then begin
    try
      Close(FLogText);
    except
    end;
    FLogTextInUse := false;
  end;
  FActiveLogText := nil;
end;

procedure TLazLoggerFileHandle.SetCloseLogFileBetweenWrites(AValue: Boolean);
begin
  if FCloseLogFileBetweenWrites = AValue then Exit;
  FCloseLogFileBetweenWrites := AValue;
  if FCloseLogFileBetweenWrites then
    DoCloseFile;
end;

procedure TLazLoggerFileHandle.SetLogName(AValue: String);
begin
  if FLogName = AValue then Exit;
  DoCloseFile;

  FLogName := AValue;

  FLogTextFailed := False;
end;

constructor TLazLoggerFileHandle.Create;
begin
  FLogTextInUse := False;
  FLogTextFailed := False;
  {$ifdef WinCE}
  FLogName := ExtractFilePath(ParamStr(0)) + Str_LCL_Debug_File;
  FUseStdOut := False;
  FCloseLogFileBetweenWrites := True;
  {$else}
  FLogName := '';
  FUseStdOut := True;
  FCloseLogFileBetweenWrites := False;
  {$endif}
end;

destructor TLazLoggerFileHandle.Destroy;
begin
  inherited Destroy;
  DoCloseFile;
end;

procedure TLazLoggerFileHandle.OpenFile;
begin
  if not CloseLogFileBetweenWrites then
    DoOpenFile;
end;

procedure TLazLoggerFileHandle.CloseFile;
begin
  DoCloseFile;
  FLogTextFailed := False;
end;

procedure TLazLoggerFileHandle.WriteToFile(const s: string);
begin
  DoOpenFile;
  if FActiveLogText = nil then exit;

  Write(FActiveLogText^, s);

  if FCloseLogFileBetweenWrites then
    DoCloseFile;
end;

procedure TLazLoggerFileHandle.WriteLnToFile(const s: string);
begin
  DoOpenFile;
  if FActiveLogText = nil then exit;

  WriteLn(FActiveLogText^, s);

  if FCloseLogFileBetweenWrites then
    DoCloseFile;
end;

{ TLazLoggerFile }

function TLazLoggerFile.GetFileHandle: TLazLoggerFileHandle;
begin
  if FFileHandle = nil then
    FFileHandle := TLazLoggerFileHandle.Create;
  Result := FFileHandle;
end;

procedure TLazLoggerFile.SetEnvironmentForLogFileName(AValue: String);
begin
  if FEnvironmentForLogFileName = AValue then Exit;
  Finish;
  FGetLogFileNameDone := False;
  FEnvironmentForLogFileName := AValue;
end;

procedure TLazLoggerFile.SetFileHandle(AValue: TLazLoggerFileHandle);
begin
  if FFileHandle = AValue then Exit;
  Finish;
  FreeAndNil(FFileHandle);
  FFileHandle := AValue;
end;

procedure TLazLoggerFile.SetParamForLogFileName(AValue: String);
begin
  if FParamForLogFileName = AValue then Exit;
  Finish;
  FGetLogFileNameDone := False;
  FParamForLogFileName := AValue;
end;

function TLazLoggerFile.GetCloseLogFileBetweenWrites: Boolean;
begin
  Result := FileHandle.CloseLogFileBetweenWrites;
end;

function TLazLoggerFile.GetLogName: String;
begin
  Result := FileHandle.LogName;
end;

function TLazLoggerFile.GetUseStdOut: Boolean;
begin
  Result := FileHandle.UseStdOut;
end;

procedure TLazLoggerFile.SetCloseLogFileBetweenWrites(AValue: Boolean);
begin
  FileHandle.CloseLogFileBetweenWrites := AValue;
end;

procedure TLazLoggerFile.SetLogName(AValue: String);
begin
  if FileHandle.LogName = AValue then Exit;
  Finish;
  FileHandle.LogName := AValue;
end;

procedure TLazLoggerFile.SetUseStdOut(AValue: Boolean);
begin
  FileHandle.UseStdOut := AValue;
end;

procedure TLazLoggerFile.DoInit;
begin
  inherited DoInit;

  FDebugNestLvl := 0;
  FDebugNestAtBOL := True;
  if (LogName = '') and not FGetLogFileNameDone then
    LogName := GetLogFileName;

  FileHandle.OpenFile;
end;

procedure TLazLoggerFile.DoFinsh;
begin
  inherited DoFinsh;

  FileHandle.CloseFile;
end;

procedure TLazLoggerFile.IncreaseIndent;
begin
  inc(FDebugNestLvl);
  CreateIndent;
end;

procedure TLazLoggerFile.DecreaseIndent;
begin
  if not FDebugNestAtBOL then DebugLn;

  if FDebugNestLvl > 0 then
    dec(FDebugNestLvl);
  CreateIndent;
end;

procedure TLazLoggerFile.IncreaseIndent(LogGroup: PLazLoggerLogGroup);
begin
  if (LogGroup <> nil) then begin
    if (not LogGroup^.Enabled) then exit;
    inc(LogGroup^.FOpenedIndents);
    IncreaseIndent;
  end
  else
    IncreaseIndent;
end;

procedure TLazLoggerFile.DecreaseIndent(LogGroup: PLazLoggerLogGroup);
begin
  if (LogGroup <> nil) then begin
    // close what was opened, even if now disabled
    // only close, if opened by this group
    if (LogGroup^.FOpenedIndents <= 0) then exit;
    dec(LogGroup^.FOpenedIndents);
    DecreaseIndent;
  end
  else
    DecreaseIndent;
end;

procedure TLazLoggerFile.IndentChanged;
begin
  CreateIndent;
end;

procedure TLazLoggerFile.CreateIndent;
var
  s: String;
  NewLen: Integer;
begin
  NewLen := FDebugNestLvl * NestLvlIndent;
  if NewLen < 0 then NewLen := 0;
  if (NewLen >= MaxNestPrefixLen) then begin
    s := IntToStr(FDebugNestLvl);
    NewLen := MaxNestPrefixLen - Length(s);
    if NewLen < 1 then
      NewLen := 1;
  end else
    s := '';

  if NewLen <> Length(FDebugIndent) then
    FDebugIndent := s + StringOfChar(' ', NewLen);
end;

procedure TLazLoggerFile.DoDbgOut(const s: string);
var
  Handled: Boolean;
begin
  if not IsInitialized then Init;

  if OnDbgOut <> nil then
  begin
    Handled := False;
    if FDebugNestAtBOL and (s <> '') then
      OnDbgOut(Self, FDebugIndent + s, Handled)
    else
      OnDbgOut(Self, s, Handled);
    if Handled then
      Exit;
  end;

  if OnWidgetSetDbgOut <> nil then
  begin
    Handled := False;
    if FDebugNestAtBOL and (s <> '') then
      OnWidgetSetDbgOut(Self, FDebugIndent + s, Handled)
    else
      OnWidgetSetDbgOut(Self, s, Handled);
    if Handled then
      Exit;
  end;

  if FDebugNestAtBOL and (s <> '') then
    FileHandle.WriteToFile(FDebugIndent + s)
  else
    FileHandle.WriteToFile(s);
  FDebugNestAtBOL := (s = '') or (s[length(s)] in [#10,#13]);
end;

procedure TLazLoggerFile.DoDebugLn(const s: string);
var
  Handled: Boolean;
begin
  if not IsInitialized then Init;

  if OnDebugLn <> nil then
  begin
    Handled := False;
    if FDebugNestAtBOL and (s <> '') then
      OnDebugLn(Self, FDebugIndent + s, Handled)
    else
      OnDebugLn(Self, s, Handled);
    if Handled then
      Exit;
  end;

  if OnWidgetSetDebugLn <> nil then
  begin
    Handled := False;
    if FDebugNestAtBOL and (s <> '') then
      OnWidgetSetDebugLn(Self, FDebugIndent + s, Handled)
    else
      OnWidgetSetDebugLn(Self, s, Handled);
    if Handled then
      Exit;
  end;

  if FDebugNestAtBOL and (s <> '') then
    FileHandle.WriteLnToFile(FDebugIndent + ConvertLineEndings(s))
  else
    FileHandle.WriteLnToFile(ConvertLineEndings(s));
  FDebugNestAtBOL := True;
end;

procedure TLazLoggerFile.DoDebuglnStack(const s: string);
begin
  DebugLn(s);
  FileHandle.DoOpenFile;
  if FileHandle.FActiveLogText = nil then exit;

  Dump_Stack(FileHandle.FActiveLogText^, get_frame);

  if CloseLogFileBetweenWrites then
    FileHandle.DoCloseFile;
end;

constructor TLazLoggerFile.Create;
begin
  inherited;
  FDebugNestLvl := 0;

  {$ifdef WinCE}
  FParamForLogFileName := '';
  FEnvironmentForLogFileName := '';
  {$else}
  FParamForLogFileName := '--debug-log=';
  FEnvironmentForLogFileName   := '*_debuglog';
  {$endif}
end;

destructor TLazLoggerFile.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FFileHandle);
end;

procedure TLazLoggerFile.Assign(Src: TLazLogger);
begin
  inherited Assign(Src);
  if (Src <> nil) and (Src is TLazLoggerFile) then begin
    FOnDbgOut  := TLazLoggerFile(Src).FOnDbgOut;
    FOnDebugLn := TLazLoggerFile(Src).FOnDebugLn;;

    FEnvironmentForLogFileName := TLazLoggerFile(Src).FEnvironmentForLogFileName;
    FParamForLogFileName       := TLazLoggerFile(Src).FParamForLogFileName;
    FGetLogFileNameDone        := TLazLoggerFile(Src).FGetLogFileNameDone;

    LogName   := TLazLoggerFile(Src).LogName;
    UseStdOut := TLazLoggerFile(Src).UseStdOut;
    CloseLogFileBetweenWrites := TLazLoggerFile(Src).CloseLogFileBetweenWrites;
  end;
end;

function TLazLoggerFile.GetLogFileName: string;
var
  EnvVarName: string;
begin
  Result := '';
  FGetLogFileNameDone := True;
  if FParamForLogFileName <> '' then begin
    // first try to find the log file name in the command line parameters
    Result := GetParamByName(FParamForLogFileName, 0);
  end;
  if FEnvironmentForLogFileName <> '' then begin;
    // if not found yet, then try to find in the environment variables
    if (length(result)=0) then begin
      EnvVarName:= ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'') + FEnvironmentForLogFileName;
      Result := GetEnvironmentVariableUTF8(EnvVarName);
    end;
  end;
  if (length(result)>0) then
    Result := ExpandFileNameUTF8(Result);
end;


function DbgStr(const StringWithSpecialChars: string): string;
var
  i: Integer;
  s: String;
  l: Integer;
begin
  Result:=StringWithSpecialChars;
  i:=1;
  while (i<=length(Result)) do begin
    case Result[i] of
    ' '..#126: inc(i);
    else
      s:='#'+HexStr(ord(Result[i]),2);
      // Note: do not use copy, fpc might change broken UTF-8 characters to '?'
      l:=length(Result)-i;
      SetLength(Result,length(Result)-1+length(s));
      if l>0 then
        system.Move(Result[i+1],Result[i+length(s)],l);
      system.Move(s[1],Result[i],length(s));
      inc(i,length(s));
    end;
  end;
end;

function DbgStr(const StringWithSpecialChars: string; StartPos, Len: PtrInt
  ): string;
begin
  Result:=dbgstr(copy(StringWithSpecialChars,StartPos,Len));
end;

function DbgStr(const p: PChar; Len: PtrInt): string;
const
  Hex: array[0..15] of char='0123456789ABCDEF';
var
  UsedLen: PtrUInt;
  ResultLen: PtrUInt;
  Src: PChar;
  Dest: PChar;
  c: Char;
begin
  if (p=nil) or (p^=#0) or (Len<=0) then exit('');
  UsedLen:=0;
  ResultLen:=0;
  Src:=p;
  while Src^<>#0 do begin
    inc(UsedLen);
    if Src^ in [' '..#126] then
      inc(ResultLen)
    else
      inc(ResultLen,3);
    if UsedLen>=Len then break;
    inc(Src);
  end;
  SetLength(Result,ResultLen);
  Src:=p;
  Dest:=PChar(Result);
  while UsedLen>0 do begin
    dec(UsedLen);
    c:=Src^;
    if c in [' '..#126] then begin
      Dest^:=c;
      inc(Dest);
    end else begin
      Dest^:='#';
      inc(Dest);
      Dest^:=Hex[ord(c) shr 4];
      inc(Dest);
      Dest^:=Hex[ord(c) and $f];
      inc(Dest);
    end;
    inc(Src);
  end;
end;

function DbgWideStr(const StringWithSpecialChars: widestring): string;
var
  s: String;
  SrcPos: Integer;
  DestPos: Integer;
  i: Integer;
begin
  SetLength(Result,length(StringWithSpecialChars));
  SrcPos:=1;
  DestPos:=1;
  while SrcPos<=length(StringWithSpecialChars) do begin
    i:=ord(StringWithSpecialChars[SrcPos]);
    case i of
    32..126:
      begin
        Result[DestPos]:=chr(i);
        inc(SrcPos);
        inc(DestPos);
      end;
    else
      s:='#'+HexStr(i,4);
      inc(SrcPos);
      Result:=copy(Result,1,DestPos-1)+s+copy(Result,DestPos+1,length(Result));
      inc(DestPos,length(s));
    end;
  end;
end;

function ConvertLineEndings(const s: string): string;
var
  i: Integer;
  EndingStart: LongInt;
begin
  Result:=s;
  i:=1;
  while (i<=length(Result)) do begin
    if Result[i] in [#10,#13] then begin
      EndingStart:=i;
      inc(i);
      if (i<=length(Result)) and (Result[i] in [#10,#13])
      and (Result[i]<>Result[i-1]) then begin
        inc(i);
      end;
      if (length(LineEnding)<>i-EndingStart)
      or (LineEnding<>copy(Result,EndingStart,length(LineEnding))) then begin
        // line end differs => replace with current LineEnding
        Result:=
          copy(Result,1,EndingStart-1)+LineEnding+copy(Result,i,length(Result));
        i:=EndingStart+length(LineEnding);
      end;
    end else
      inc(i);
  end;
end;

procedure ReplaceSubstring(var s: string; StartPos, Count: SizeInt;
  const Insertion: string);
var
  MaxCount: SizeInt;
  InsertionLen: SizeInt;
  SLen: SizeInt;
  RestLen: SizeInt;
  p: PByte;
begin
  SLen:=length(s);
  if StartPos>SLen then
    StartPos:=SLen;
  if StartPos<1 then StartPos:=1;
  if Count<0 then Count:=0;
  MaxCount:=SLen-StartPos+1;
  if Count>MaxCount then
    Count:=MaxCount;
  InsertionLen:=length(Insertion);
  if (Count=0) and (InsertionLen=0) then
    exit; // nothing to do
  if (Count=InsertionLen) then begin
    if CompareMem(PByte(s)+StartPos-1,Pointer(Insertion),Count) then
      // already the same content
      exit;
    UniqueString(s);
  end else begin
    RestLen:=SLen-StartPos-Count+1;
    if InsertionLen<Count then begin
      // shorten
      if RestLen>0 then begin
        UniqueString(s);
        p:=PByte(s)+StartPos-1;
        System.Move((p+Count)^,(p+InsertionLen)^,RestLen);
      end;
      Setlength(s,SLen-Count+InsertionLen);
    end else begin
      // longen
      Setlength(s,SLen-Count+InsertionLen);
      if RestLen>0 then begin
        p:=PByte(s)+StartPos-1;
        System.Move((p+Count)^,(p+InsertionLen)^,RestLen);
      end;
    end;
  end;
  if InsertionLen>0 then
    System.Move(PByte(Insertion)^,(PByte(s)+StartPos-1)^,InsertionLen);
end;

initialization
  LazDebugLoggerCreator := @CreateDebugLogger;
  RecreateDebugLogger
end.

