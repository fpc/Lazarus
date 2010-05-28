unit leakinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

type
  { TStackLine }

  TStackLine = record
    LineNum   : Integer;  // -1 is line is uknown
    FileName  : string;   // should be empty if file is unknown
    Addr      : Int64;    // -1 if address is unknown
    RawLineData: string;
  end;

  { TStackTrace }

  TStackTrace = class(TObject)
  public
    Lines      : array of TStackLine;
    LinesCount : integer;
    BlockSize  : integer;
    Addr       : Int64;
    LeakCount  : Integer;
    RawStackData: string;
    constructor Create;
  end;

  TLeakStatus = record
    TotalMem    : Int64; // total mem used (-1) if unavailable
    LeakedMem   : Int64; // leaked mem size (0) if none
    LeakCount   : Int64; // number of unfreed pointers
  end;


  // abstract class

  { TLeakInfo }

  TLeakInfo = class(TObject)
    // returns True, if information has been succesfully received, False otherwise
    // Fills LeakData record
    // if Traces is not nil, fill the list with TStackTrace object. User is responsible for freeing them
    function GetLeakInfo(var LeakData: TLeakStatus; var Traces: TList): Boolean; virtual; abstract;
  end;

  // this file can be (should be?) hidden in the other unit, or to the implementation section
  // but it's hear for debugging purposes yet.

  // heap trc class

  { THeapTrcInfo }

  THeapTraceInfo = record
    ExeName       : string;
    AllocSize     : Int64;
    FreedSize     : Int64;
    UnfreedSize   : Int64;
    AllocBlocks   : Int64;
    FreedBlocks   : Int64;
    Unfreedblocks : Int64;
    HeapSize      : Int64;
    HeapFreed     : Int64;
    HeapShouldbe  : Int64;
    StartupUsed   : Int64;
  end;

  THeapTrcInfo = class(TLeakInfo)
  protected
    fTRCFile  : string;
    fTRCText  : string;
    Trc       : TStringList;
    TrcIndex  : integer;
    fSummary  : string;

    fParsed   : Boolean;

    function PosInTrc(const SubStr: string; CaseSensetive: Boolean = false): Boolean;
    function IsTraceLine(const SubStr: string): Boolean;
    function TrcNumberAfter(var Num: Int64; const AfterSub: string): Boolean;
    function TrcNumberAfter(var Num: Integer; const AfterSub: string): Boolean;
    function TrcNumFirstAndAfter(var FirstNum, AfterNum: Int64; const AfterSub: string): Boolean;

    procedure ParseStackTrace(trace: TStackTrace);
    procedure DoParseTrc(traces: TList);

  public
    TraceInfo : THeapTraceInfo;
    constructor Create(const ATRCFile: string);
    constructor CreateFromTxt(const AText: string);
    function GetLeakInfo(var LeakData: TLeakStatus; var Traces: TList): Boolean; override;
  end;

function AllocHeapTraceInfo(const TrcFile: string): TLeakInfo;
function AllocHeapTraceInfoFromText(const TrcText: string): TLeakInfo;

resourcestring
  CallTracePrefix = 'Call trace for block ';
  RawTracePrefix = 'Stack trace:';
  rsStackTrace = 'Stack trace';

implementation

function AllocHeapTraceInfo(const TrcFile: string): TLeakInfo;
begin
  Result := THeapTrcInfo.Create(TrcFile);
end;

function AllocHeapTraceInfoFromText(const TrcText: string): TLeakInfo;
begin
  Result := THeapTrcInfo.CreateFromTxt(TrcText);
end;

// heap trace parsing implementation

procedure ClearTraceInfo(var TraceInfo: THeapTraceInfo);
begin
  with TraceInfo do begin
    ExeName       := '';
    AllocSize     := -1;
    FreedSize     := -1;
    UnfreedSize   := 0;
    AllocBlocks   := -1;
    FreedBlocks   := -1;
    Unfreedblocks := 0;
    HeapSize      := -1;
    HeapFreed     := -1;
    HeapShouldbe  := -1;
    StartupUsed   := -1;
  end;
end;

function ExtractNumberStr(const s: string; Offset: Integer): string;
var
  i : integer;
begin
  for i := Offset to length(s) do
    if not (s[i] in ['0'..'9']) then begin
      Result := Copy(s, Offset, i - Offset);
      Exit;
    end;
  Result := Copy(s, Offset, length(s)-Offset+1);
end;

function ExtractHexNumberStr(const s: string; Offset: Integer): string;
var
  i : integer;
begin
  Result := '';
  if s[Offset] = '$' then i := Offset + 1
  else i := Offset;

  for i := i to length(s) do
    if not (s[i] in ['0'..'9','A'..'F', 'a'..'f']) then begin
      Result := Copy(s, Offset, i - Offset);
      Exit;
    end;
  Result := Copy(s, Offset, length(s)-Offset+1);
end;

function StrToInt(const s: string; var Num: int64): Boolean;
var
  err : Integer;
begin
  if s = '' then Result := false
  else begin
    Val(s, Num, err);
    Result := err = 0;
  end;
end;

function GetNumberAfter(const s: string; var Num: int64; const AfterStr: string): Boolean; overload;
var
  i : integer;
  sub : string;
begin
  i := Pos(AfterStr, s);
  Result := i > 0;
  if not Result then Exit;

  inc(i, length(AfterStr));
  sub := ExtractNumberStr(s, i);
  Result := sub <> '';
  if not Result then Exit;
  Result := StrToInt(sub, num);
end;

function GetNumberAfter(const s: string; var Num: integer; const AfterStr: string): Boolean; overload;
var
  i64 : Int64;
begin
  i64 := Num;
  Result := GetNumberAfter(s, i64, AfterStr);
  Num := i64;
end;

procedure GetNumFirstAndAfter(const s: string; var FirstNum, AfterNum: Int64; const AfterStr: string);
begin
  StrToInt(ExtractNumberStr(s, 1), FirstNum);
  GetNumberAfter(s, AfterNum, AfterStr);
end;

procedure ParseTraceLine(const s: string; var line: TStackLine);
var
  i   : integer;
  err : Integer;
  hex : string;
begin
  i := Pos('$', s);
  if i <= 0 then Exit;
  hex := ExtractHexNumberStr(s, i);
  Val(hex, line.Addr, err);

  if not GetNumberAfter(s, line.LineNum, 'line ') then begin
    line.LineNum := -1;
    line.FileName := ''
  end else begin
    i := Pos(' of ', s);
    if i <= 0 then Exit;
    inc(i, 4);
    line.FileName := Copy(s, i, length(s) - i + 1);
  end;
end;



{ THeapTrcInfo }

function THeapTrcInfo.PosInTrc(const SubStr: string; CaseSensetive: Boolean): Boolean;
begin
  Result := TrcIndex<Trc.Count;
  if not Result then Exit;

  if CaseSensetive then
    Result := Pos(SubStr, Trc[TrcIndex])>0
  else // slow?
    Result := Pos(UpperCase(SubStr), UpperCase(Trc[TrcIndex]))>0;
end;

function THeapTrcInfo.IsTraceLine(const SubStr: string): Boolean;
var
  i, l: integer;
begin
  Result := False;
  i := 1;
  l := length(SubStr);
  while (i <= l) and (SubStr[i] = ' ') do inc(i);
  if (i > l) or (SubStr[i] <> '$') then exit;
  inc(i);
  while (i <= l) and
        ((SubStr[i] in ['0'..'9']) or ((SubStr[i] in ['A'..'F'])) or ((SubStr[i] in ['a'..'f'])))
  do inc(i);
  if (i > l) or (SubStr[i] <> ' ') then exit;
  Result := Pos('line', SubStr) > 0;
end;

function THeapTrcInfo.TrcNumberAfter(var Num: Int64; const AfterSub: string): Boolean;
begin
  Result := TrcIndex<Trc.Count;
  if not Result then Exit;
  GetNumberAfter(Trc[TrcIndex], Num, AfterSub);
end;

function THeapTrcInfo.TrcNumberAfter(var Num: Integer; const AfterSub: string): Boolean;
var
  i : Int64;
begin
  i := Num;
  Result := TrcNumberAfter(i, AfterSub);
  Num := i;
end;

function THeapTrcInfo.TrcNumFirstAndAfter(var FirstNum, AfterNum: Int64; const AfterSub: string): Boolean;
begin
  Result := TrcIndex<Trc.Count;
  if not Result then Exit;
  GetNumFirstAndAfter(Trc[TrcIndex], FirstNum, AfterNum, AfterSub);
end;

procedure THeapTrcInfo.DoParseTrc(traces: TList);
var
  st : TStackTrace;
begin
  ClearTraceInfo(TraceInfo);
  if TrcIndex >= Trc.COunt then Exit;
  TraceInfo.ExeName := Trc[TrcIndex];

  while (TrcIndex < Trc.Count)
    and (not (PosInTrc('Heap dump') or  PosInTrc(RawTracePrefix) or
              PosInTrc(CallTracePrefix) or IsTraceLine(Trc[TrcIndex]) ))
  do
    inc(TrcIndex);

  if TrcIndex >= Trc.Count then Exit;

  if PosInTrc(RawTracePrefix) or IsTraceLine(Trc[TrcIndex]) then begin
    if not Assigned(traces) then Exit;
    st := TStackTrace.Create;
    ParseStackTrace(st); // changes TrcIndex
    Traces.Add(st);
    exit;
  end;

  if not PosInTrc(CallTracePrefix) then begin
    inc(TrcIndex);
    with TraceInfo do begin
      TrcNumFirstAndAfter(AllocBlocks, AllocSize, ': '); inc(TrcIndex);
      TrcNumFirstAndAfter(FreedBlocks, FreedSize, ': '); inc(TrcIndex);
      TrcNumFirstAndAfter(UnfreedBlocks, UnfreedSize, ': ');  inc(TrcIndex);
      TrcNumberAfter(HeapSize, ': ');
      TrcNumberAfter(StartupUsed, '('); inc(TrcIndex);
      TrcNumberAfter(HeapFreed, ': '); inc(TrcIndex);
      if PosInTrc('Should be') then begin
        TrcNumberAfter(HeapShouldBe, ': ');
        inc(TrcIndex);
      end;
    end;
  end;

  if not Assigned(traces) then Exit;

  while TrcIndex < Trc.Count do begin
    if PosInTrc(CallTracePrefix) then begin
      st := TStackTrace.Create;
      ParseStackTrace(st); // changes TrcIndex
      Traces.Add(st);
    end else
      inc(TrcIndex);
  end;

end;

constructor THeapTrcInfo.Create(const ATRCFile: string);
begin
  fTrcFile := ATrcFile;
  fTRCText := '';
  inherited Create;
end;

constructor THeapTrcInfo.CreateFromTxt(const AText: string);
begin
  fTRCText := AText;
end;

procedure THeapTrcInfo.ParseStackTrace(trace: TStackTrace);
var
  i   : integer;
  err : integer;
  hex : string;
begin
  i := Pos(RawTracePrefix, Trc[TrcIndex]);
  if (i <= 0) and not IsTraceLine(Trc[TrcIndex]) then begin
    i := Pos(CallTracePrefix, Trc[TrcIndex]);
    if i <= 0 then Exit;

    trace.RawStackData := Trc[TrcIndex]; // raw stack trace data

    inc(i, length(CallTracePrefix));
    hex := ExtractHexNumberStr(Trc[TrcIndex], i);

    Val(hex, trace.Addr, err);
    GetNumberAfter(Trc[TrcIndex], trace.BlockSize, 'size ');
  end else begin
    trace.RawStackData := rsStackTrace;
    trace.Addr := 0;
    trace.BlockSize := 0;
  end;

  inc(TrcIndex);
  while (TrcIndex < Trc.Count) and (Pos(CallTracePrefix, Trc[TrcIndex]) = 0) and
        (Pos(RawTracePrefix, Trc[TrcIndex]) = 0)
  do begin
    if trace.LinesCount = length(trace.Lines) then begin
      if trace.LinesCount = 0 then SetLength(trace.Lines, 4)
      else SetLength(trace.Lines, trace.LinesCount * 2);
    end;
    ParseTraceLine(Trc[Trcindex], trace.Lines[trace.LinesCount]);
    trace.Lines[trace.LinesCount].RawLineData := Trc[Trcindex]; // raw stack line data
    inc(trace.LinesCount);
    inc(Trcindex);
  end;
end;


function THeapTrcInfo.GetLeakInfo(var LeakData: TLeakStatus; var Traces: TList): Boolean;
begin
  Result := false;
  if (not FileExistsUTF8(fTRCFile)) and (fTRCText = '') then
    Exit;
  try
    Trc := TStringList.Create;
    try
      if fTRCText <> '' then
        Trc.Text := fTRCText
      else
        Trc.LoadFromFile(fTrcFile);
      TrcIndex := 0;

      DoParseTrc(Traces);
      LeakData.LeakCount := TraceInfo.UnfreedBlocks;
      LeakData.LeakedMem := TraceInfo.UnfreedSize;
      LeakData.TotalMem := TraceInfo.AllocSize;
      Result := true;
    finally
      Trc.Free;
      Trc := nil;
    end;
  except
    Result := false;
  end;
end;


{ TStackTrace }

constructor TStackTrace.Create;
begin
  LeakCount := 1;
end;

end.

