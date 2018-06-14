{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit LazLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, math,
  // LazUtils
  LazLoggerBase, LazClasses, LazFileUtils, LazUtilities, LazUTF8;

type

  PLazLoggerLogGroup = LazLoggerBase.PLazLoggerLogGroup;

{$DEFINE USED_BY_LAZLOGGER}
{$I LazLoggerIntf.inc}


function DbgStr(const StringWithSpecialChars: string): string; overload;
function DbgStr(const StringWithSpecialChars: string; StartPos, Len: PtrInt): string; overload;
function DbgStr(const p: PChar; Len: PtrInt): string; overload;
function DbgWideStr(const StringWithSpecialChars: widestring): string; overload;

type

  { TLazLoggerFileHandle }

  TLazLoggerFileHandle = class
  private
    FActiveLogText: PText; // may point to stdout
    FCloseLogFileBetweenWrites: Boolean;
    FLastWriteFailed: Boolean;
    FLogName: String;
    FLogText: Text;
    FLogTextInUse, FLogTextFailed: Boolean;
    FUseStdOut: Boolean;
    FWriteFailedCount: Integer;
    procedure DoOpenFile;
    procedure DoCloseFile;
    function GetWriteTarget: TLazLoggerWriteTarget;
    procedure SetCloseLogFileBetweenWrites(AValue: Boolean);
    procedure SetLogName(AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure OpenFile;
    procedure CloseFile;
    procedure ResetWriteFailedCounter;

    procedure WriteToFile(const s: string); virtual;
    procedure WriteLnToFile(const s: string); virtual;

    property  LogName: String read FLogName write SetLogName;
    property  UseStdOut: Boolean read FUseStdOut write FUseStdOut;
    property  CloseLogFileBetweenWrites: Boolean read FCloseLogFileBetweenWrites write SetCloseLogFileBetweenWrites;
    property  WriteTarget: TLazLoggerWriteTarget read GetWriteTarget;
    property  ActiveLogText: PText read FActiveLogText;
    property  WriteFailedCount: Integer read FWriteFailedCount;
    property  LastWriteFailed: Boolean read FLastWriteFailed;
  end;

  { TLazLoggerFileHandleThreadSave
    file operations in critical section
  }

  TLazLoggerFileHandleThreadSave = class (TLazLoggerFileHandle)
  private
    FWriteToFileLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteToFile(const s: string); override;
    procedure WriteLnToFile(const s: string); override;
  end;

  { TLazLoggerFileHandleMainThread
    file operations queued for main thread
  }

  TLazLoggerFileHandleMainThread = class (TLazLoggerFileHandle)
  private
  type
    PWriteListEntry = ^TWriteListEntry;
    TWriteListEntry = record
      Next: PWriteListEntry;
      Data: String;
      Ln: Boolean;
    end;
  private
    FWriteToFileLock: TRTLCriticalSection;
    FFirst, FLast: PWriteListEntry;

    procedure MainThreadWrite;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteToFile(const s: string); override;
    procedure WriteLnToFile(const s: string); override;
  end;


  { TLazLoggerFile }

  TLazLoggerFile = class(TLazLoggerWithGroupParam)
  private
    FFileHandle: TLazLoggerFileHandle;
    FOnDbgOut: TLazLoggerWriteEvent;
    FOnDebugLn: TLazLoggerWriteEvent;
    FBlockHandler: TList;


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
    function GetBlockHandler(AIndex: Integer): TLazLoggerBlockHandler; override;
    procedure ClearAllBlockHandler;


    procedure DoDbgOut(const s: string); override;
    procedure DoDebugLn(const s: string); override;
    procedure DoDebuglnStack(const s: string); override;

    property FileHandle: TLazLoggerFileHandle read GetFileHandle write SetFileHandle;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Src: TLazLogger); override;
    function CurrentIndentLevel: Integer; override;
    // A param on the commandline, that may contain the name (if not already set)
    // example/default: --debug-log=
    property  ParamForLogFileName: String read FParamForLogFileName write SetParamForLogFileName;
    // Environment variable used to specify log file name 
    // * is replaced by param(0) - the application name without extension
    // example/default: *_debuglog
    property  EnvironmentForLogFileName: String read FEnvironmentForLogFileName write SetEnvironmentForLogFileName; 

    property  OnDebugLn: TLazLoggerWriteEvent read FOnDebugLn write FOnDebugLn;
    property  OnDbgOut:  TLazLoggerWriteEvent read FOnDbgOut write FOnDbgOut;

    procedure AddBlockHandler(AHandler: TLazLoggerBlockHandler); override;
    procedure RemoveBlockHandler(AHandler: TLazLoggerBlockHandler); override;
    function BlockHandlerCount: Integer; override;

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

{ TLazLoggerFileHandleMainThread }

procedure TLazLoggerFileHandleMainThread.MainThreadWrite;
var
  Data, NextData: PWriteListEntry;
begin
  EnterCriticalsection(FWriteToFileLock);
  try
    Data := FFirst;
    FFirst := nil;
    FLast := nil;
  finally
    LeaveCriticalsection(FWriteToFileLock);
  end;

  while Data <> nil do begin
    NextData := Data^.Next;
    if Data^.Ln
    then inherited WriteLnToFile(Data^.Data)
    else inherited WriteToFile(Data^.Data);
    Dispose(Data);
    Data := NextData;
  end;
end;

constructor TLazLoggerFileHandleMainThread.Create;
begin
  InitCriticalSection(FWriteToFileLock);
  inherited;
end;

destructor TLazLoggerFileHandleMainThread.Destroy;
begin
  inherited Destroy;
  DoneCriticalsection(FWriteToFileLock);
end;

procedure TLazLoggerFileHandleMainThread.WriteToFile(const s: string);
var
  Data: PWriteListEntry;
begin
  New(Data);
  Data^.Data := s;
  Data^.Ln := False;
  Data^.Next := nil;
  EnterCriticalsection(FWriteToFileLock);
  try
    if FLast = nil then
      FFirst := Data
    else
      FLast^.Next := Data;
    FLast := Data;
  finally
    LeaveCriticalsection(FWriteToFileLock);
  end;
  TThread.Queue(nil, @MainThreadWrite);
end;

procedure TLazLoggerFileHandleMainThread.WriteLnToFile(const s: string);
var
  Data: PWriteListEntry;
begin
  New(Data);
  Data^.Data := s;
  Data^.Ln := True;
  Data^.Next := nil;
  EnterCriticalsection(FWriteToFileLock);
  try
    if FLast = nil then
      FFirst := Data
    else
      FLast^.Next := Data;
    FLast := Data;
  finally
    LeaveCriticalsection(FWriteToFileLock);
  end;
  TThread.Queue(nil, @MainThreadWrite);
end;

{ TLazLoggerFileHandleThreadSave }

constructor TLazLoggerFileHandleThreadSave.Create;
begin
  InitCriticalSection(FWriteToFileLock);
  inherited;
end;

destructor TLazLoggerFileHandleThreadSave.Destroy;
begin
  inherited Destroy;
  DoneCriticalsection(FWriteToFileLock);
end;

procedure TLazLoggerFileHandleThreadSave.WriteToFile(const s: string);
begin
  EnterCriticalsection(FWriteToFileLock);
  try
    inherited WriteToFile(s);
  finally
    LeaveCriticalsection(FWriteToFileLock);
  end;
end;

procedure TLazLoggerFileHandleThreadSave.WriteLnToFile(const s: string);
begin
  EnterCriticalsection(FWriteToFileLock);
  try
    inherited WriteLnToFile(s);
  finally
    LeaveCriticalsection(FWriteToFileLock);
  end;
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

function TLazLoggerFileHandle.GetWriteTarget: TLazLoggerWriteTarget;
begin
  Result := lwtNone;
  if FActiveLogText = @Output then
    Result := lwtStdOut
  else
  if FLogTextInUse then
    Result := lwtTextFile;
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

  FLogName := CleanAndExpandFilename(AValue);

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

procedure TLazLoggerFileHandle.ResetWriteFailedCounter;
begin
  FWriteFailedCount := 0;
end;

procedure TLazLoggerFileHandle.WriteToFile(const s: string);
begin
  try
  DoOpenFile;
  if FActiveLogText = nil then exit;

  Write(FActiveLogText^, s);

  if FCloseLogFileBetweenWrites then
    DoCloseFile;
    FLastWriteFailed := False;
  except
    inc(FWriteFailedCount);
    FLastWriteFailed := True;
  end;
end;

procedure TLazLoggerFileHandle.WriteLnToFile(const s: string);
begin
  try
  DoOpenFile;
  if FActiveLogText = nil then exit;

  WriteLn(FActiveLogText^, s);

  if FCloseLogFileBetweenWrites then
    DoCloseFile;
    FLastWriteFailed := False;
  except
    inc(FWriteFailedCount);
    FLastWriteFailed := True;
  end;
end;

{ TLazLoggerFile }

function TLazLoggerFile.GetFileHandle: TLazLoggerFileHandle;
begin
  if FFileHandle = nil then
    FFileHandle := TLazLoggerFileHandleMainThread.Create;
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
var
  i: Integer;
begin
  inc(FDebugNestLvl);
  CreateIndent;
  for i := 0 to BlockHandlerCount - 1 do
    BlockHandler[i].EnterBlock(Self, FDebugNestLvl);
end;

procedure TLazLoggerFile.DecreaseIndent;
var
  i: Integer;
begin
  if not FDebugNestAtBOL then DebugLn;

  if FDebugNestLvl > 0 then begin
    for i := 0 to BlockHandlerCount - 1 do
      BlockHandler[i].ExitBlock(Self, FDebugNestLvl);
    dec(FDebugNestLvl);
  end;
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

function TLazLoggerFile.GetBlockHandler(AIndex: Integer): TLazLoggerBlockHandler;
begin
  Result := TLazLoggerBlockHandler(FBlockHandler[AIndex]);
end;

procedure TLazLoggerFile.ClearAllBlockHandler;
begin
  while BlockHandlerCount > 0 do RemoveBlockHandler(BlockHandler[0]);
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
      OnWidgetSetDbgOut(Self, FDebugIndent + s, Handled,
                        FileHandle.WriteTarget, FileHandle.ActiveLogText)
    else
      OnWidgetSetDbgOut(Self, s, Handled, FileHandle.WriteTarget, FileHandle.ActiveLogText);
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
      OnWidgetSetDebugLn(Self, FDebugIndent + s, Handled,
                         FileHandle.WriteTarget, FileHandle.ActiveLogText)
    else
      OnWidgetSetDebugLn(Self, s, Handled, FileHandle.WriteTarget, FileHandle.ActiveLogText);
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
  FBlockHandler := TList.Create;

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
  ClearAllBlockHandler;
  inherited Destroy;
  FreeAndNil(FFileHandle);
  FreeAndNil(FBlockHandler);
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

function TLazLoggerFile.CurrentIndentLevel: Integer;
begin
  Result := FDebugNestLvl;
end;

procedure TLazLoggerFile.AddBlockHandler(AHandler: TLazLoggerBlockHandler);
begin
  FBlockHandler.Add(AHandler);
  AHandler.AddReference;
end;

procedure TLazLoggerFile.RemoveBlockHandler(AHandler: TLazLoggerBlockHandler);
begin
  FBlockHandler.Remove(AHandler);
  AHandler.ReleaseReference;
end;

function TLazLoggerFile.BlockHandlerCount: Integer;
begin
  Result := FBlockHandler.Count;
end;

function TLazLoggerFile.GetLogFileName: string;
var
  EnvVarName: string;
  i: Integer;
begin
  Result := '';
  FGetLogFileNameDone := True;
  if FParamForLogFileName <> '' then begin
    // first try to find the log file name in the command line parameters
    i := GetParamByNameCount(FParamForLogFileName) - 1;
    if i >= 0 then
      Result := GetParamByName(FParamForLogFileName, i);
  end;
  if FEnvironmentForLogFileName <> '' then begin;
    // if not found yet, then try to find in the environment variables
    if (length(result)=0) then begin
      // Substitute * with executable filename without extension
      EnvVarName:=StringReplace(FEnvironmentForLogFileName,
        '*',
        ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),''),
        [rfReplaceAll,rfIgnoreCase]);
      Result := GetEnvironmentVariableUTF8(EnvVarName);
    end;
  end;
  if (length(result)>0) then
    Result := ExpandFileNameUTF8(Result);
end;


function DbgStr(const StringWithSpecialChars: string): string;
begin
  Result := LazLoggerBase.DbgStr(StringWithSpecialChars);
end;

function DbgStr(const StringWithSpecialChars: string; StartPos, Len: PtrInt
  ): string;
begin
  Result := LazLoggerBase.DbgStr(StringWithSpecialChars, StartPos, Len);
end;

function DbgStr(const p: PChar; Len: PtrInt): string;
begin
  Result := LazLoggerBase.DbgStr(p, Len);
end;

function DbgWideStr(const StringWithSpecialChars: widestring): string;
begin
  Result := LazLoggerBase.DbgWideStr(StringWithSpecialChars);
end;

initialization
  LazDebugLoggerCreator := @CreateDebugLogger;
  RecreateDebugLogger;
end.

