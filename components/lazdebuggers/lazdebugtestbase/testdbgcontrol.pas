unit TestDbgControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestDbgConfig, TTestDbgExecuteables;

type
  TWriteLogConfig = (wlNever, wlAlways, wlOnError);

  TCanCpuBits = function(b: TCpuBitType): Boolean;
  TCanSymType = function(s: TSymbolType): Boolean;
  TCanCompiler = function(name: string): Boolean;
  TCanDebugger = function(name: string): Boolean;
  TCanTest = function(id: Pointer): Boolean;
  TGetTestPattern = function: string;

  TSetLogPath = procedure(path: string);
  TGetLogPath = function: string;
  TGetWriteLog = function: TWriteLogConfig;


  TRegisterCompiler = procedure(name: string);
  TRegisterDebugger = procedure(name: string);
  TRegisterTest = function(Name: String; Parent: Pointer = nil): Pointer;

var
  CanCpuBitsProc: TCanCpuBits;
  CanSymTypeProc: TCanSymType;
  CanCompilerProc: TCanCompiler;
  CanDebuggerProc: TCanDebugger;
  CanTestProc: TCanTest;
  GetTestPatternProc: TGetTestPattern;
  SetLogPathProc: TSetLogPath;
  GetLogPathProc: TGetLogPath;
  GetWriteLogProc: TGetWriteLog;
  RegisterCompilerProc: TRegisterCompiler;
  RegisterDebuggerProc: TRegisterDebugger;
  RegisterTestProc: TRegisterTest;

function TestControlCanCpuBits(b: TCpuBitType): Boolean;
function TestControlCanSymType(s: TSymbolType): Boolean;
function TestControlCanCompiler(name: string): Boolean;
function TestControlCanDebugger(name: string): Boolean;
function TestControlCanTest(id: Pointer): Boolean;
function TestControlGetTestPattern: string;

procedure TestControlRegisterCompiler(name: string);
procedure TestControlRegisterDebugger(name: string);
function TestControlRegisterTest(Name: String; Parent: Pointer = nil): Pointer;

procedure TestControlSetLogPath(path: string);
function TestControlGetLogPath: string;
function TestControlGetWriteLog: TWriteLogConfig;

procedure TestControlRegisterCompilers(c: TBaseList);
procedure TestControlRegisterDebuggers(d: TBaseList);

implementation

var
  LogPath: String;

function TestControlCanCpuBits(b: TCpuBitType): Boolean;
begin
  Result := True;
  if CanCpuBitsProc <> nil then
    Result := CanCpuBitsProc(b);
end;

function TestControlCanSymType(s: TSymbolType): Boolean;
begin
  Result := True;
  if CanSymTypeProc <> nil then
    Result := CanSymTypeProc(s);
end;

function TestControlCanCompiler(name: string): Boolean;
begin
  Result := True;
  if CanCompilerProc <> nil then
    Result := CanCompilerProc(name);
end;

function TestControlCanDebugger(name: string): Boolean;
begin
  Result := True;
  if CanDebuggerProc <> nil then
    Result := CanDebuggerProc(name);
end;

function TestControlCanTest(id: Pointer): Boolean;
begin
  Result := True;
  if id = nil then
    exit;
  if CanTestProc <> nil then
    Result := CanTestProc(id);
end;

function TestControlGetTestPattern: string;
begin
  Result := '';
  if GetTestPatternProc <> nil then
    Result := GetTestPatternProc();
end;

procedure TestControlRegisterCompiler(name: string);
begin
  if RegisterCompilerProc <> nil then
    RegisterCompilerProc(name);
end;

procedure TestControlRegisterDebugger(name: string);
begin
  if RegisterDebuggerProc <> nil then
    RegisterDebuggerProc(name);
end;

function TestControlRegisterTest(Name: String; Parent: Pointer): Pointer;
begin
  Result := nil;
  if RegisterTestProc <> nil then
    Result := RegisterTestProc(Name, Parent);
end;

procedure TestControlSetLogPath(path: string);
begin
  if SetLogPathProc <> nil then
    SetLogPathProc(path)
  else
    LogPath := path;
end;

function TestControlGetLogPath: string;
begin
  Result := LogPath;
  if GetLogPathProc <> nil then
    Result := GetLogPathProc();
end;

function TestControlGetWriteLog: TWriteLogConfig;
begin
  Result := wlNever;
  if GetWriteLogProc <> nil then
    Result := GetWriteLogProc()
  else
  if TestControlGetLogPath <> '' then
    Result := wlAlways;
end;

procedure TestControlRegisterCompilers(c: TBaseList);
var
  i: Integer;
begin
  for i := 0 to c.Count - 1 do
    TestControlRegisterCompiler(c.Name[i]);
end;

procedure TestControlRegisterDebuggers(d: TBaseList);
var
  i: Integer;
begin
  for i := 0 to d.Count - 1 do
    TestControlRegisterDebugger(d.Name[i]);
end;

end.

