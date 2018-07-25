{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit LazLoggerProfiling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazLoggerBase, LazSysUtils;

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
    procedure EnterBlock({%H-}Sender: TLazLogger; Level: Integer); override;
    procedure ExitBlock({%H-}Sender: TLazLogger; Level: Integer); override;
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
    procedure EnterBlock({%H-}Sender: TLazLogger; Level: Integer); override;
    procedure ExitBlock({%H-}Sender: TLazLogger; Level: Integer); override;
    property  MaxDepth: Integer read FMaxDepth write SetMaxDepth;
    property  MemDiff[ALevel: Integer]: Int64 read GetMemDiff;
    property  Nested[ALevel: Integer]: Int64 read GetNested;
  end;

// %0:s Current block, since enter
// %1:s Sum of Nested blocks frame
// %2:s Parent block, since enter
// %4:s Sum of Nested blocks in parent frame
function DbgsMemUsed(AFormat: String = '%0:d'): string;
function DbgsTimeUsed(AFormat: String = '%0:n'): string;

procedure DbgStartTimer(AName: String);
procedure DbgStopTimer(AName: String);
procedure DbgStartMemWatch(AName: String);
procedure DbgStopMemWatch(AName: String);

function DbgsMemUsed(AFormat: String; AName: String): string;
function DbgsTimeUsed(AFormat: String; AName: String): string;

implementation

var
  NamedMemWatches: TStringList = nil;
  NamedTimer: TStringList = nil;
  NamedMemWatchesData: Array of record Sum, Last: Int64; end;
  NamedTimerData: array of record Sum, Last: QWord; end;


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
  i := DebugLogger.CurrentIndentLevel;
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
  i := DebugLogger.CurrentIndentLevel;
  l := GetTimer;
  if l = nil then exit;
  try
    Result := Format(AFormat, [l.TimeDiff[i]/1000, l.Nested[i]/1000, l.TimeDiff[i-1]/1000, l.Nested[i-1]/1000]);
  except
    Result := Format('%0:n %1:n', [l.TimeDiff[i]/1000, l.Nested[i]/1000, l.TimeDiff[i-1]/1000, l.Nested[i-1]/1000]);
  end;
end;

procedure DbgStartTimer(AName: String);
var
  idx: Integer;
begin
  if NamedTimer = nil then begin
    NamedTimer := TStringList.Create;
    NamedTimer.Sorted := True;
    NamedTimer.Duplicates := dupError;
  end;
  idx := NamedTimer.IndexOf(AName);
  if idx < 0 then begin
    idx := NamedTimer.AddObject(AName, TObject(length(NamedTimerData)));
    SetLength(NamedTimerData, length(NamedTimerData) + 1);
    NamedTimerData[length(NamedTimerData)-1].Sum := 0;
  end;
  idx := PtrInt(NamedTimer.Objects[idx]);
  NamedTimerData[idx].Last := GetTickCount64;
end;

procedure DbgstopTimer(AName: String);
var
  idx: Integer;
  t: QWord;
begin
  if NamedTimer = nil then exit;
  idx := NamedTimer.IndexOf(AName);
  if idx < 0 then exit;
  idx := PtrInt(NamedTimer.Objects[idx]);
  t := GetTickCount64;
  if t >= NamedTimerData[idx].Last then
    t := t - NamedTimerData[idx].Last
  else // timer overflow
    t := high(t) - NamedTimerData[idx].Last + t;
  NamedTimerData[idx].Sum := NamedTimerData[idx].Last + t;
end;

procedure DbgStartMemWatch(AName: String);
var
  idx: Integer;
begin
  if NamedMemWatches = nil then begin
    NamedMemWatches := TStringList.Create;
    NamedMemWatches.Sorted := True;
    NamedMemWatches.Duplicates := dupError;
  end;
  idx := NamedMemWatches.IndexOf(AName);
  if idx < 0 then begin
    idx := NamedMemWatches.AddObject(AName, TObject(length(NamedMemWatchesData)));
    SetLength(NamedMemWatchesData, length(NamedMemWatchesData) + 1);
    NamedMemWatchesData[length(NamedMemWatchesData)-1].Sum := 0;
  end;
  idx := PtrInt(NamedMemWatches.Objects[idx]);
  NamedMemWatchesData[idx].Last := GetHeapStatus.TotalAllocated;
end;

procedure DbgStopMemWatch(AName: String);
var
  idx: Integer;
begin
  if NamedMemWatches = nil then exit;
  idx := NamedMemWatches.IndexOf(AName);
  if idx < 0 then exit;
  idx := PtrInt(NamedMemWatches.Objects[idx]);
  NamedMemWatchesData[idx].Sum := NamedMemWatchesData[idx].Sum + (GetHeapStatus.TotalAllocated - NamedMemWatchesData[idx].Last);
  NamedMemWatchesData[idx].Last := 0;
end;

function DbgsMemUsed(AFormat: String; AName: String): string;
var
  idx: Integer;
begin
  Result := '';
  if NamedMemWatches = nil then exit;
  idx := NamedMemWatches.IndexOf(AName);
  if idx < 0 then exit;
  idx := PtrInt(NamedMemWatches.Objects[idx]);
  try
    Result := Format(AFormat, [NamedMemWatchesData[idx].Sum]);
  except
    Result := Format('%d', [NamedMemWatchesData[idx].Sum]);
  end;
end;

function DbgsTimeUsed(AFormat: String; AName: String): string;
var
  idx: Integer;
begin
  Result := '';
  if NamedTimer = nil then exit;
  idx := NamedTimer.IndexOf(AName);
  if idx < 0 then exit;
  idx := PtrInt(NamedTimer.Objects[idx]);
  try
    Result := Format(AFormat, [NamedTimerData[idx].Sum/1000]);
  except
    Result := Format('%n', [NamedTimerData[idx].Sum /1000]);
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

finalization
  FreeAndNil(NamedTimer);
  FreeAndNil(NamedMemWatches);
  NamedTimerData := nil;
  NamedMemWatchesData := nil;

end.

