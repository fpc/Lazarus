unit TestDbgConfig;

{$mode objfpc}{$H+}

(*
  fpclist.txt contains lines of format:
    [Name]
    exe=/path/fpc.exe
    symbols=none,gs,gw,gwset,gw3


  optional
  gdblist.txt contains lines of format:
    [Name]
    exe=/path/fpc.exe
    version=070201
    symbols=none,gs,gw,gwset,gw3

*)

interface

uses
  Classes, SysUtils, LCLProc;

var
  AppDir, ConfDir: String;

type
  TSymbolType = (stNone, stStabs, stDwarf, stDwarfSet, stDwarf3);
  TSymbolTypes = set of TSymbolType;

  TCpuBitType = (cpu32, cpu64);
  TCpuBitTypes = set of TCpuBitType;

const
  SymbolTypeNames: Array [TSymbolType] of String = ('No_Dbg', 'Stabs', 'Dwarf', 'Dwarf+Sets', 'Dwarf3');
  SymbolTypeSwitches: Array [TSymbolType] of String = ('', '-gs', '-gw', '-gw -godwarfsets', '-gw3');
  CpuBitNames: Array [TCpuBitType] of String = ('32Bit', '64Bit');

type
  TExternalExeInfo = record
    Name: string;
    Version: Integer;
    ExeName: string;
    SymbolTypes: TSymbolTypes;
    CpuBitTypes: TCpuBitTypes;
    ExtraOpts: string;
  end;

type
  { TBaseList }

  TBaseList = class
  private
    FList: array of TExternalExeInfo;
    function GetCpuBitTypes(Index: Integer): TCpuBitTypes;
    function GetExeName(Index: Integer): string;
    function GetExtraOpts(Index: Integer): string;
    function GetFullInfo(Index: Integer): TExternalExeInfo;
    function GetName(Index: Integer): string;
    function GetSymbolTypes(Index: Integer): TSymbolTypes;
    function GetVersion(Index: Integer): Integer;

  protected
    function  AddName(const AName: string): Integer;
    procedure SetAttribute(AIndex: Integer; const AAttr, AValue: string); virtual;
  public
    procedure Add(Name, Exe: string; Opts: String = '');
    procedure Add(Info: TExternalExeInfo);
    procedure LoadFromFile(const AFileName: string);
    function Count: Integer;
    property Name[Index: Integer]: string read GetName;
    property Version[Index: Integer]: Integer read GetVersion;
    property ExeName[Index: Integer]: string read GetExeName;
    property SymbolTypes[Index: Integer]: TSymbolTypes read GetSymbolTypes;
    property CpuBitTypes[Index: Integer]: TCpuBitTypes read GetCpuBitTypes;
    property ExtraOpts[Index: Integer]: string read GetExtraOpts;
    property FullInfo[Index: Integer]: TExternalExeInfo read GetFullInfo;
  end;

function LoadConfig(FileName: String): TBaseList;

implementation

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
    if s2 = 'none' then Result := Result + [stNone];
    if s2 = 'gs' then Result := Result + [stStabs];
    if s2 = 'gw' then Result := Result + [stDwarf];
    if s2 = 'gwset' then Result := Result + [stDwarfSet];
    if s2 = 'gw3' then Result := Result + [stDwarf3];
  end;
end;

function StrToCpuBitTypes(s: string): TCpuBitTypes;
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
    if s2 = '32' then Result := Result + [cpu32];
    if s2 = '64' then Result := Result + [cpu64];
  end;
end;

{ TBaseList }

function TBaseList.GetExeName(Index: Integer): string;
begin
  Result := FList[Index].ExeName;
end;

function TBaseList.GetCpuBitTypes(Index: Integer): TCpuBitTypes;
begin
  Result := FList[Index].CpuBitTypes;
end;

function TBaseList.GetExtraOpts(Index: Integer): string;
begin
  Result := FList[Index].ExtraOpts;
end;

function TBaseList.GetFullInfo(Index: Integer): TExternalExeInfo;
begin
  Result := FList[Index];
end;

function TBaseList.GetName(Index: Integer): string;
begin
  Result := FList[Index].Name;
end;

function TBaseList.GetSymbolTypes(Index: Integer): TSymbolTypes;
begin
  Result := FList[Index].SymbolTypes;
end;

function TBaseList.GetVersion(Index: Integer): Integer;
begin
  Result := FList[Index].Version;
end;

function TBaseList.AddName(const AName: string): Integer;
begin
  Result := length(FList);
  SetLength(FList, Result + 1);
  FList[Result].Name := AName;
  FList[Result].Version := -1;
  FList[Result].ExeName := '';
  FList[Result].SymbolTypes := [];
  FList[Result].CpuBitTypes := [low(TCpuBitType)..high(TCpuBitType)]; // default: all
  FList[Result].ExtraOpts := '';
end;

procedure TBaseList.SetAttribute(AIndex: Integer; const AAttr, AValue: string);
begin
  case StringCase(AAttr, ['exe', 'symbols', 'opts', 'vers', 'version', 'bittype', 'bits'], True, False) of
    0: begin // exe
        FList[AIndex].ExeName := AValue;
      end;
    1: begin // symbols
        FList[AIndex].SymbolTypes := StrToSymbolTypes(AValue);
      end;
    2: begin //opts
        FList[AIndex].ExtraOpts := AValue;
      end;
    3,4: begin
        FList[AIndex].Version := StrToIntDef(AValue,-1);
      end;
    5,6: begin
        FList[AIndex].CpuBitTypes := StrToCpuBitTypes(AValue);
      end;
  end;
end;

procedure TBaseList.Add(Name, Exe: string; Opts: String);
var
  i: LongInt;
begin
  i := AddName(Name);
  FList[i].ExeName := Exe;
  FList[i].SymbolTypes := [low(TSymbolTypes)..high(TSymbolTypes)];
  FList[i].ExtraOpts := Opts;
end;

procedure TBaseList.Add(Info: TExternalExeInfo);
var
  i: Integer;
begin
  i := AddName('');
  FList[i] := Info;
end;

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
  txt.Free;
end;

function TBaseList.Count: Integer;
begin
  Result := length(FList);
end;

function LoadConfig(FileName: String): TBaseList;
begin
  Result := TBaseList.Create;
  if FileExists(FileName) then
    Result.LoadFromFile(FileName);
end;

end.

