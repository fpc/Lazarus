unit TestBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fpcunit, testutils, testregistry,
  EnvironmentOpts, LCLProc, CompileHelpers, Dialogs;

(*
  fpclist.txt contains lines of format:
    [Name]
    exe=/path/fpc.exe
    symbols=gs,gw,gw3


  gdblist.txt contains lines of format:
    [Name]
    exe=/path/fpc.exe
    symbols=gs,gw,gw3

*)

type
  TSymbolType = (stStabs, stDwarf, stDwarf3);
  TSymbolTypes = set of TSymbolType;

const
  SymbolTypeNames: Array [TSymbolType] of String = ('Stabs', 'Dwarf', 'Dwarf3');
  SymbolTypeSwitches: Array [TSymbolType] of String = ('-gs', '-gw', '-gw3');

type

  TCompilerInfo = record
        Name: string;
        ExeName: string;
        SymbolTypes: TSymbolTypes;
      end;

  TDebuggerInfo = record
        Name: string;
        ExeName: string;
        SymbolTypes: TSymbolTypes;
      end;

  { TBaseList }

  TBaseList = class
  protected
    function  AddName(const AName: string): Integer; virtual; abstract;
    procedure SetAttribute(AIndex: Integer; const AAttr, AValue: string); virtual; abstract;
  public
    procedure LoadFromFile(const AFileName: string);
  end;

  { TCompilerList }

  TCompilerList = class(TBaseList)
  private
    FList: array of TCompilerInfo;
    function GetCompilerInfo(Index: Integer): TCompilerInfo;
    function GetExeName(Index: Integer): string;
    function GetName(Index: Integer): string;
    function GetSymbolTypes(Index: Integer): TSymbolTypes;
  protected
    function  AddName(const AName: string): Integer; override;
    procedure SetAttribute(AIndex: Integer; const AAttr, AValue: string); override;
  public
    procedure Add(Name, Exe: string);
    function Count: Integer;
    property CompilerInfo[Index: Integer]: TCompilerInfo read GetCompilerInfo;
    property Name[Index: Integer]: string read GetName;
    property ExeName[Index: Integer]: string read GetExeName;
    property SymbolTypes[Index: Integer]: TSymbolTypes read GetSymbolTypes;
  end;

  { TDebuggerList }

  TDebuggerList = class(TBaseList)
  private
    FList: array of TDebuggerInfo;
    function GetDebuggerInfo(Index: Integer): TDebuggerInfo;
    function GetExeName(Index: Integer): string;
    function GetName(Index: Integer): string;
    function GetSymbolTypes(Index: Integer): TSymbolTypes;
  protected
    function  AddName(const AName: string): Integer; override;
    procedure SetAttribute(AIndex: Integer; const AAttr, AValue: string); override;
  public
    procedure Add(Name, Exe: string);
    function Count: Integer;
    property DebuggerInfo[Index: Integer]: TDebuggerInfo read GetDebuggerInfo;
    property Name[Index: Integer]: string read GetName;
    property ExeName[Index: Integer]: string read GetExeName;
    property SymbolTypes[Index: Integer]: TSymbolTypes read GetSymbolTypes;
  end;


  { TCompilerSuite }

  TCompilerSuite = class(TTestSuite)
  private
    FCompilerInfo: TCompilerInfo;
  public
    constructor Create(ACompilerInfo: TCompilerInfo; ADebuggerList: TDebuggerList);
    procedure RegisterDbgTest(ATestClass: TTestCaseClass);
  public
    property CompilerInfo: TCompilerInfo read FCompilerInfo;
  end;

  { TCompilerOptionsSuite }

  TCompilerOptionsSuite = class(TTestSuite)
  private
    FParent: TCompilerSuite;
    FSymbolSwitch: String;
    FSymbolType: TSymbolType;
    FFileNameExt: String;
    FCompiledList: TStringList;
    FInRun: Boolean;
    function GetCompilerInfo: TCompilerInfo;
  protected
    procedure Clear;
  public
    constructor Create(AParent: TCompilerSuite; ASymbolType: TSymbolType; ADebuggerList: TDebuggerList);
    destructor Destroy; override;
    procedure Run(AResult: TTestResult); override;
    procedure RunTest(ATest: TTest; AResult: TTestResult); override;
    procedure RegisterDbgTest(ATestClass: TTestCaseClass);
    Procedure TestCompile(const PrgName: string; out ExeName: string);
  public
    property Parent: TCompilerSuite read FParent;
    property SymbolType: TSymbolType read FSymbolType;
    property SymbolSwitch: String read FSymbolSwitch;
    property CompilerInfo: TCompilerInfo read GetCompilerInfo;
  end;

  { TDebuggerSuite }

  TDebuggerSuite = class(TTestSuite)
  private
    FDebuggerInfo: TDebuggerInfo;
    FParent: TCompilerOptionsSuite;
    function GetCompilerInfo: TCompilerInfo;
    function GetSymbolType: TSymbolType;
  public
    constructor Create(AParent: TCompilerOptionsSuite; ADebuggerInfo: TDebuggerInfo);
    procedure RegisterDbgTest(ATestClass: TTestCaseClass);
    Procedure TestCompile(const PrgName: string; out ExeName: string);
  public
    property Parent: TCompilerOptionsSuite read FParent;
    property DebuggerInfo: TDebuggerInfo read FDebuggerInfo;
    property SymbolType: TSymbolType read GetSymbolType;
    property CompilerInfo: TCompilerInfo read GetCompilerInfo;
  end;

  { TGDBTestsuite }

  TGDBTestsuite = class(TTestSuite)
  private
    FParent: TDebuggerSuite;
    function GetCompilerInfo: TCompilerInfo;
    function GetDebuggerInfo: TDebuggerInfo;
    function GetSymbolType: TSymbolType;
  public
    constructor Create(AParent: TDebuggerSuite; AClass: TClass);
    procedure AddTest(ATest: TTest); overload; override;
    Procedure TestCompile(const PrgName: string; out ExeName: string);
  public
    property Parent: TDebuggerSuite read FParent;
    property DebuggerInfo: TDebuggerInfo read GetDebuggerInfo;
    property SymbolType: TSymbolType read GetSymbolType;
    property CompilerInfo: TCompilerInfo read GetCompilerInfo;
  end;

  { TGDBTestCase }

  TGDBTestCase = class(TTestCase)
  private
    FParent: TGDBTestsuite;
    function GetCompilerInfo: TCompilerInfo;
    function GetDebuggerInfo: TDebuggerInfo;
    function GetSymbolType: TSymbolType;
  public
    Procedure TestCompile(const PrgName: string; out ExeName: string);
  public
    property Parent: TGDBTestsuite read FParent write FParent;
    property DebuggerInfo: TDebuggerInfo read GetDebuggerInfo;
    property SymbolType: TSymbolType read GetSymbolType;
    property CompilerInfo: TCompilerInfo read GetCompilerInfo;
  end;


function GetCompilers: TCompilerList;
function GetDebuggers: TDebuggerList;

procedure RegisterDbgTest(ATestClass: TTestCaseClass);

var
  AppDir: String;

implementation

var
  Compilers: TCompilerList = nil;
  Debuggers: TDebuggerList = nil;

function StrToSymbolTypes(s: string): TSymbolTypes;
var
  s2: string;
begin
  Result := [];
  while (s <> '') do begin
    while (s <> '') and (s[1] in [' ', ',', #9, #10, #13]) do delete(s,1, 1);
    s2 := '';
    while (s <> '') and not (s[1] in [' ', ',', #9, #10, #13]) do begin
      s2 := s2 + s[1];
      delete(s,1, 1);
    end;
    if s2 = 'gs' then Result := Result + [stStabs];
    if s2 = 'gw' then Result := Result + [stDwarf];
    if s2 = 'gw3' then Result := Result + [stDwarf3];
  end;
end;


function GetCompilers: TCompilerList;
var
  AppDir: String;
begin
  if Compilers <> nil then exit(Compilers);

  AppDir := ExtractFilePath(Paramstr(0));
  Result := TCompilerList.Create;
  if FileExists(AppDir + 'fpclist.txt') then
    Result.LoadFromFile(AppDir + 'fpclist.txt');
  if (Result.Count = 0) and (EnvironmentOptions.CompilerFilename <> '') then
    Result.Add('fpc from conf', EnvironmentOptions.CompilerFilename);
  Compilers := Result;
end;

function GetDebuggers: TDebuggerList;
var
  AppDir: String;
begin
  if Debuggers <> nil then exit(Debuggers);

  AppDir := ExtractFilePath(Paramstr(0));
  Result := TDebuggerList.Create;
  if FileExists(AppDir + 'gdblist.txt') then
    Result.LoadFromFile(AppDir + 'gdblist.txt');
  if (Result.Count = 0) and (EnvironmentOptions.DebuggerFilename <> '') then
    Result.Add('gdb from conf', EnvironmentOptions.DebuggerFilename);
  Debuggers := Result;
end;

{ TGDBTestCase }

function TGDBTestCase.GetCompilerInfo: TCompilerInfo;
begin
  Result := Parent.CompilerInfo;
end;

function TGDBTestCase.GetDebuggerInfo: TDebuggerInfo;
begin
  Result := Parent.DebuggerInfo;
end;

function TGDBTestCase.GetSymbolType: TSymbolType;
begin
  Result := Parent.SymbolType;
end;

procedure TGDBTestCase.TestCompile(const PrgName: string; out ExeName: string);
begin
  Parent.TestCompile(PrgName, ExeName);
end;

{ TBaseList }

procedure TBaseList.LoadFromFile(const AFileName: string);
var
  txt: TStringList;
  s: string;
  i, j, k: Integer;
begin
  txt := TStringList.Create;
  txt.LoadFromFile(AFileName);
  j := -1;
  for i := 0 to txt.Count - 1 do begin
    s := txt[i];
    if Trim(s) = '' then continue;
    if copy(s, 1, 1) = '[' then begin
      j  := AddName(GetPart(['['], [']'], s));
      continue;
    end;
    if j < 0 then continue;
    k := pos('=', s);
    SetAttribute(j, copy(s, 1, k-1), copy(s, k + 1, length(s)));
  end;
end;

{ TCompilerList }

function TCompilerList.GetExeName(Index: Integer): string;
begin
  Result := FList[Index].ExeName;
end;

function TCompilerList.GetCompilerInfo(Index: Integer): TCompilerInfo;
begin
  Result := FList[Index];
end;

function TCompilerList.GetName(Index: Integer): string;
begin
  Result := FList[Index].Name;
end;

function TCompilerList.GetSymbolTypes(Index: Integer): TSymbolTypes;
begin
  Result := FList[Index].SymbolTypes;
end;

function TCompilerList.AddName(const AName: string): Integer;
begin
  Result := length(FList);
  SetLength(FList, Result + 1);
  FList[Result].Name := AName;
  FList[Result].SymbolTypes := [];
end;

procedure TCompilerList.SetAttribute(AIndex: Integer; const AAttr, AValue: string);
begin
  case StringCase(AAttr, ['exe', 'symbols'], True, False) of
    0: begin // exe
        FList[AIndex].ExeName := AValue;
      end;
    1: begin // symbols
        FList[AIndex].SymbolTypes := StrToSymbolTypes(AValue);
      end;
  end;
end;

procedure TCompilerList.Add(Name, Exe: string);
var
  i: LongInt;
begin
  i := AddName(Name);
  FList[i].ExeName := Exe;
  FList[i].SymbolTypes := [stStabs, stDwarf];
end;

function TCompilerList.Count: Integer;
begin
  Result := length(FList);
end;

{ TDebuggerList }

function TDebuggerList.GetExeName(Index: Integer): string;
begin
  Result := FList[Index].ExeName;
end;

function TDebuggerList.GetDebuggerInfo(Index: Integer): TDebuggerInfo;
begin
  Result := FList[Index];
end;

function TDebuggerList.GetName(Index: Integer): string;
begin
  Result := FList[Index].Name;
end;

function TDebuggerList.GetSymbolTypes(Index: Integer): TSymbolTypes;
begin
  Result := FList[Index].SymbolTypes;
end;

function TDebuggerList.AddName(const AName: string): Integer;
begin
  Result := length(FList);
  SetLength(FList, Result + 1);
  FList[Result].Name := AName;
  FList[Result].SymbolTypes := [];
end;

procedure TDebuggerList.SetAttribute(AIndex: Integer; const AAttr, AValue: string);
begin
  case StringCase(AAttr, ['exe', 'symbols'], True, False) of
    0: begin // exe
        FList[AIndex].ExeName := AValue;
      end;
    1: begin // symbols
        FList[AIndex].SymbolTypes := StrToSymbolTypes(AValue);
      end;
  end;
end;

procedure TDebuggerList.Add(Name, Exe: string);
var
  i: LongInt;
begin
  i := AddName(Name);
  FList[i].ExeName := Exe;
  FList[i].SymbolTypes := [stStabs, stDwarf];
end;

function TDebuggerList.Count: Integer;
begin
  Result := length(FList);
end;

{ TCompilerSuite }

constructor TCompilerSuite.Create(ACompilerInfo: TCompilerInfo; ADebuggerList: TDebuggerList);
var
  st: TSymbolType;
  SubSuite: TCompilerOptionsSuite;
begin
  inherited Create(ACompilerInfo.Name);
  FCompilerInfo := ACompilerInfo;

  for st := low(TSymbolType) to high(TSymbolType) do begin
    if not (st in FCompilerInfo.SymbolTypes) then
      continue;

    SubSuite := TCompilerOptionsSuite.Create(Self, st, ADebuggerList);
    Self.AddTest(SubSuite);
  end;
end;

procedure TCompilerSuite.RegisterDbgTest(ATestClass: TTestCaseClass);
var
  i: Integer;
begin
  for i := 0 to Tests.Count - 1 do
    if Test[i] is TCompilerOptionsSuite then
      TCompilerOptionsSuite(Test[i]).RegisterDbgTest(ATestClass);
end;

{ TCompilerOptionsSuite }

function TCompilerOptionsSuite.GetCompilerInfo: TCompilerInfo;
begin
  Result := Parent.CompilerInfo;
end;

procedure TCompilerOptionsSuite.Clear;
var
  i: Integer;
begin
  for i := 0 to FCompiledList.Count - 1 do
    DeleteFile(FCompiledList[i]);
  FCompiledList.Clear;
end;

constructor TCompilerOptionsSuite.Create(AParent: TCompilerSuite; ASymbolType: TSymbolType;
  ADebuggerList: TDebuggerList);
var
  i: Integer;
  SubSuite: TDebuggerSuite;
begin
  inherited Create(SymbolTypeNames[ASymbolType]);
  FParent := AParent;
  FSymbolType := ASymbolType;

  FCompiledList := TStringList.Create;
  FSymbolSwitch := SymbolTypeSwitches[FSymbolType];
  FInRun := False;

  FFileNameExt := SymbolTypeNames[FSymbolType] + '_';
  for i := 1 to length(CompilerInfo.Name) do begin
    if CompilerInfo.Name[i] in ['a'..'z', 'A'..'Z', '0'..'9', '.', '-'] then
      FFileNameExt := FFileNameExt + CompilerInfo.Name[i]
    else if CompilerInfo.Name[i] = ' ' then
      FFileNameExt := FFileNameExt +  '__'
    else
      FFileNameExt := FFileNameExt + '_' + IntToHex(ord(CompilerInfo.Name[i]), 2);
  end;

  for i := 0 to ADebuggerList.Count - 1 do begin
    if not (FSymbolType in ADebuggerList.SymbolTypes[i]) then
      continue;
    SubSuite := TDebuggerSuite.Create(Self, ADebuggerList.DebuggerInfo[i]);
    Self.AddTest(SubSuite);
  end;
end;

destructor TCompilerOptionsSuite.Destroy;
begin
  inherited Destroy;
  Clear;
  FreeAndNil(FCompiledList);
end;

procedure TCompilerOptionsSuite.Run(AResult: TTestResult);
begin
  FInRun := True;
  try
    inherited Run(AResult);
  finally
    FInRun := False;
    Clear;
  end;
end;

procedure TCompilerOptionsSuite.RunTest(ATest: TTest; AResult: TTestResult);
begin
  try
    inherited RunTest(ATest, AResult);
  finally
    if not FInRun then Clear;
  end;
end;

procedure TCompilerOptionsSuite.RegisterDbgTest(ATestClass: TTestCaseClass);
var
  i: Integer;
begin
  for i := 0 to Tests.Count - 1 do
    if Test[i] is TDebuggerSuite then
      TDebuggerSuite(Test[i]).RegisterDbgTest(ATestClass);
end;

procedure TCompilerOptionsSuite.TestCompile(const PrgName: string; out ExeName: string);
var
  ExePath, ErrMsg: String;
begin
  ExePath := ExtractFileNameWithoutExt(PrgName);
  ExeName := ExtractFileNameOnly(ExePath);
  ExePath := AppendPathDelim(copy(ExePath, 1, length(ExePath) - length(ExeName)));
  if DirectoryExistsUTF8(ExePath + 'lib') then
    ExePath := AppendPathDelim(ExePath + 'lib');
  ExeName := ExePath + ExeName + FFileNameExt + GetExeExt;

  if FCompiledList.IndexOf(ExeName) < 0 then begin
    if FileExists(ExeName) then
      raise EAssertionFailedError.Create('Found existing file before compiling: ' + ExeName);
    FCompiledList.Add(ExeName);
    ErrMsg := CompileHelpers.TestCompile(PrgName, FSymbolSwitch, ExeName, CompilerInfo.ExeName);
    if ErrMsg <> '' then
      raise EAssertionFailedError.Create('Compilation Failed: ' + ExeName + LineEnding + ErrMsg);
  end;

  if not FileExists(ExeName) then
    raise EAssertionFailedError.Create('Missing compiled exe ' + ExeName);
end;

{ TDebuggerSuite }

function TDebuggerSuite.GetCompilerInfo: TCompilerInfo;
begin
  Result := Parent.CompilerInfo;
end;

function TDebuggerSuite.GetSymbolType: TSymbolType;
begin
  Result := Parent.SymbolType;
end;

constructor TDebuggerSuite.Create(AParent: TCompilerOptionsSuite;
  ADebuggerInfo: TDebuggerInfo);
begin
  inherited Create(ADebuggerInfo.Name);
  FParent := AParent;
  FDebuggerInfo := ADebuggerInfo;
end;

procedure TDebuggerSuite.RegisterDbgTest(ATestClass: TTestCaseClass);
var
  NewTest: TGDBTestsuite;
begin
  NewTest := TGDBTestsuite.Create(Self, ATestClass);
  AddTest(NewTest);
end;

procedure TDebuggerSuite.TestCompile(const PrgName: string; out ExeName: string);
begin
  Parent.TestCompile(PrgName, ExeName);
end;

{ TGDBTestsuite }

function TGDBTestsuite.GetCompilerInfo: TCompilerInfo;
begin
  Result := Parent.CompilerInfo;
end;

function TGDBTestsuite.GetDebuggerInfo: TDebuggerInfo;
begin
  Result := Parent.DebuggerInfo;
end;

function TGDBTestsuite.GetSymbolType: TSymbolType;
begin
  Result := Parent.SymbolType;
end;

constructor TGDBTestsuite.Create(AParent: TDebuggerSuite; AClass: TClass);
begin
  inherited Create(AClass);
  FParent := AParent;
end;

procedure TGDBTestsuite.AddTest(ATest: TTest);
begin
  inherited AddTest(ATest);
  if ATest is TGDBTestCase then
    TGDBTestCase(ATest).Parent := Self;
end;

procedure TGDBTestsuite.TestCompile(const PrgName: string; out ExeName: string);
begin
  Parent.TestCompile(PrgName, ExeName);
end;

{ --- }

procedure RegisterDbgTest(ATestClass: TTestCaseClass);
var
  Suite: TTestSuite;
  i: Integer;
begin
  Suite := GetTestRegistry;
  for i := 0 to Suite.Tests.Count - 1 do
    if Suite.Test[i] is TCompilerSuite then
      TCompilerSuite(Suite.Test[i]).RegisterDbgTest(ATestClass);
end;


procedure BuildTestSuites;
var
  FpcList: TCompilerList;
  GdbList: TDebuggerList;
  CompilerSuite: TCompilerSuite;
  i: Integer;
begin
  FpcList := GetCompilers;
  GdbList := GetDebuggers;

  for i := 0 to FpcList.Count - 1 do begin
    CompilerSuite := TCompilerSuite.Create(FpcList.CompilerInfo[i], GdbList);
    GetTestRegistry.AddTest(CompilerSuite);
  end;
end;

function CheckAppDir(var AppDir: string): Boolean;
begin
  Result := DirectoryExistsUTF8(AppDir + 'TestApps');
  if Result then
    AppDir := AppendPathDelim(AppDir + 'TestApps');
end;

function CheckAppDirLib(var AppDir: string): Boolean;
var
  s: string;
begin
  Result := False;
  if RightStr(AppDir, length('lib' + DirectorySeparator)) = 'lib' + DirectorySeparator
  then begin
    s := copy(AppDir, 1, length(AppDir) - length('lib' + DirectorySeparator));
    Result :=  DirectoryExistsUTF8(s + 'TestApps');
    if Result then
      AppDir := AppendPathDelim(s + 'TestApps');
  end;
end;

function AppDirStripAppBundle(AppDir: string): String;
var
  p: LongInt;
begin
  Result := AppDir;
  p := pos('.app' + DirectorySeparator, AppDir);
  while (p > 1) and (AppDir[p-1] <> DirectorySeparator) do
    dec(p);
  if p > 1 then
    Result := Copy(AppDir, 1, p - 1);
end;

initialization
  AppDir := AppendPathDelim(ExtractFilePath(Paramstr(0)));
  if  not(CheckAppDir(AppDir))
  and not(CheckAppDirLib(AppDir))
  then begin
    AppDir := AppDirStripAppBundle(AppDir);
    if  not(CheckAppDir(AppDir))
    and not(CheckAppDirLib(AppDir))
    then
      with TSelectDirectoryDialog.Create(nil) do begin
        if Execute then AppDir := AppendPathDelim(FileName);
        Free;
      end;
  end;


  EnvironmentOptions := TEnvironmentOptions.Create;
  with EnvironmentOptions do
  begin
    SetLazarusDefaultFilename;
    Load(false);
  end;
  BuildTestSuites;

finalization
  FreeAndNil(Compilers);
  FreeAndNil(Debuggers);

end.

