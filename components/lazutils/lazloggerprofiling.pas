unit LazLoggerProfiling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazLoggerBase, lazutf8sysutils;

type

  { TLazLoggerBlockTimer }

  TLazLoggerBlockTimer = class(TLazLoggerBlockHandler)
  private
    FMaxDepth: Integer;
    FTimes: Array of QWord;
    FNested: Array of QWord;
    function GetNested(ALevel: Integer): QWord;
    function GetTimeDiff(ALevel: Integer): QWord;
    procedure SetMaxDepth(AValue: Integer);
  public
    constructor Create;
    procedure EnterBlock(Sender: TLazLogger; Level: Integer); override;
    procedure ExitBlock(Sender: TLazLogger; Level: Integer); override;
    property  MaxDepth: Integer read FMaxDepth write SetMaxDepth;
    property  TimeDiff[ALevel: Integer]: QWord read GetTimeDiff;
    property  Nested[ALevel: Integer]: QWord read GetNested;
  end;

  { TLazLoggerBlockMemWatch }

  TLazLoggerBlockMemWatch = class(TLazLoggerBlockHandler)
  private
    FMaxDepth: Integer;
    FMem: Array of Int64;
    FNested: Array of Int64;
    function GetMemDiff(ALevel: Integer): Int64;
    function GetNested(ALevel: Integer): Int64;
    procedure SetMaxDepth(AValue: Integer);
  public
    constructor Create;
    procedure EnterBlock(Sender: TLazLogger; Level: Integer); override;
    procedure ExitBlock(Sender: TLazLogger; Level: Integer); override;
    property  MaxDepth: Integer read FMaxDepth write SetMaxDepth;
    property  MemDiff[ALevel: Integer]: Int64 read GetMemDiff;
    property  Nested[ALevel: Integer]: Int64 read GetNested;
  end;

// %0:s Current block, since enter
// %1:s Sum of Nested blocks frame
// %2:s Paren block, since enter
// %4:s Sum of Nested blocks in parent frame
function DbgsMemUsed(AFormat: String = '%0:d'): string;
function DbgsTimeUsed(AFormat: String = '%0:n'): string;

implementation

function GetMemWatch: TLazLoggerBlockMemWatch;
var
  i: Integer;
begin
  Result := nil;
  i := DebugLogger.BlockHandlerCount - 1;
  while (i >= 0) and (Result = nil) do
    if DebugLogger.BlockHandler[i] is TLazLoggerBlockMemWatch then
      Result := DebugLogger.BlockHandler[i] as TLazLoggerBlockMemWatch
    else
      dec(i);
end;

function GetTimer: TLazLoggerBlockTimer;
var
  i: Integer;
begin
  Result := nil;
  i := DebugLogger.BlockHandlerCount - 1;
  while (i >= 0) and (Result = nil) do
    if DebugLogger.BlockHandler[i] is TLazLoggerBlockTimer then
      Result := DebugLogger.BlockHandler[i] as TLazLoggerBlockTimer
    else
      dec(i);
end;

function DbgsMemUsed(AFormat: String): string;
var
  l: TLazLoggerBlockMemWatch;
  i: Integer;
begin
  Result := '';
  i := DebugLogger.NestLvlIndent;
  l := GetMemWatch;
  if l = nil then exit;
  try
    Result := Format(AFormat, [l.MemDiff[i], l.Nested[i], l.MemDiff[i-1], l.Nested[i-1]]);
  except
    Result := Format('%0:d %1:d', [l.MemDiff[i], l.Nested[i], l.MemDiff[i-1], l.Nested[i-1]]);
  end;
end;

function DbgsTimeUsed(AFormat: String): string;
var
  l: TLazLoggerBlockTimer;
  i: Integer;
begin
  Result := '';
  i := DebugLogger.NestLvlIndent;
  l := GetTimer;
  if l = nil then exit;
  try
    Result := Format(AFormat, [l.TimeDiff[i]/1000, l.Nested[i]/1000, l.TimeDiff[i-1]/1000, l.Nested[i-1]/1000]);
  except
    Result := Format('%0:n %1:n', [l.TimeDiff[i]/1000, l.Nested[i]/1000, l.TimeDiff[i-1]/1000, l.Nested[i-1]/1000]);
  end;
end;

{ TLazLoggerBlockMemWatch }

procedure TLazLoggerBlockMemWatch.SetMaxDepth(AValue: Integer);
begin
  if FMaxDepth = AValue then Exit;
  SetLength(FMem, AValue+1);
  SetLength(FNested, AValue);

  if (FMaxDepth = 0) and (AValue > 0) then begin
    FMem[0] := GetHeapStatus.TotalAllocated;
    FNested[0] := 0;
  end;

  FMaxDepth := AValue;
end;

function TLazLoggerBlockMemWatch.GetMemDiff(ALevel: Integer): Int64;
var
  t: Int64;
begin
  Result := 0;
  if (ALevel < 0) or (ALevel >= FMaxDepth + 1) then exit;
  t := GetHeapStatus.TotalAllocated;
  Result := t - FMem[ALevel];
end;

function TLazLoggerBlockMemWatch.GetNested(ALevel: Integer): Int64;
begin
  Result := 0;
  if (ALevel < 0) or (ALevel >= FMaxDepth) then exit;
  Result := FNested[ALevel];
end;

constructor TLazLoggerBlockMemWatch.Create;
begin
  MaxDepth := 100;
  FMem[0] := GetHeapStatus.TotalAllocated;
  FNested[0] := 0;
end;

procedure TLazLoggerBlockMemWatch.EnterBlock(Sender: TLazLogger; Level: Integer);
begin
  if (Level < 0) or (Level >= FMaxDepth + 1) then exit;
  FMem[Level] := GetHeapStatus.TotalAllocated;
  if (Level >= FMaxDepth) then exit;
  FNested[Level] := 0;
end;

procedure TLazLoggerBlockMemWatch.ExitBlock(Sender: TLazLogger; Level: Integer);
begin
  if (Level < 1) or (Level >= FMaxDepth + 1) then exit;
  FMem[Level - 1] := FNested[Level - 1] + GetMemDiff(Level);
end;

{ TLazLoggerBlockTimer }

procedure TLazLoggerBlockTimer.SetMaxDepth(AValue: Integer);
begin
  if FMaxDepth = AValue then Exit;
  SetLength(FTimes, AValue+1);
  SetLength(FNested, AValue);

  if (FMaxDepth = 0) and (AValue > 0) then begin
    FTimes[0] := GetTickCount64;
    FNested[0] := 0;
  end;

  FMaxDepth := AValue;
end;

function TLazLoggerBlockTimer.GetTimeDiff(ALevel: Integer): QWord;
var
  t: QWord;
begin
  Result := 0;
  if (ALevel < 0) or (ALevel >= FMaxDepth + 1) then exit;
  t := GetTickCount64;
  if t >= FTimes[ALevel] then
    Result := t - FTimes[ALevel]
  else // timer overflow
    Result := high(t) - FTimes[ALevel] + t;
end;

function TLazLoggerBlockTimer.GetNested(ALevel: Integer): QWord;
begin
  Result := 0;
  if (ALevel < 0) or (ALevel >= FMaxDepth) then exit;
  Result := FNested[ALevel];
end;

constructor TLazLoggerBlockTimer.Create;
begin
  MaxDepth := 100;
  FTimes[0] := GetTickCount64;
  FNested[0] := 0;
end;

procedure TLazLoggerBlockTimer.EnterBlock(Sender: TLazLogger; Level: Integer);
begin
  if (Level < 0) or (Level >= FMaxDepth + 1) then exit;
  FTimes[Level] := GetTickCount64;
  if (Level >= FMaxDepth) then exit;
  FNested[Level] := 0;
end;

procedure TLazLoggerBlockTimer.ExitBlock(Sender: TLazLogger; Level: Integer);
begin
  if (Level < 1) or (Level >= FMaxDepth + 1) then exit;
  FNested[Level - 1] := FNested[Level - 1] + GetTimeDiff(Level);
end;

initialization
  DebugLogger.AddBlockHandler(TLazLoggerBlockTimer.Create);
  DebugLogger.AddBlockHandler(TLazLoggerBlockMemWatch.Create);

end.

