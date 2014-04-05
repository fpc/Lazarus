unit leakinfo;
(*
  Testdata at aend of file
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazClasses, DbgInfoReader;

type
  { TStackLine }

  TStackLine = class(TRefCountedObject)
    LineNum   : Integer;  // -1 is line is uknown
    FileName  : string;   // should be empty if file is unknown
    Addr      : Int64;    // -1 if address is unknown
    RawLineData: string;
    function Equals(ALine: TStackLine): boolean; reintroduce;
    procedure Assign(ALine: TStackLine);
    procedure AssignLineAndFile(ALine: TStackLine);
  end;

  { TStackLines }

  TStackLines = class(TObject)
  private
    FLines: TRefCntObjList;
    function GetLine(Index: Integer): TStackLine;
  public
    constructor Create;
    destructor  Destroy; override;
    function  Count: Integer;
    procedure Clear;
    function  Add(ALine: TStackLine): Integer;
    function  IndexOfAddr(AnAddr: Int64): Integer;
    function  FindAddr(AnAddr: Int64): TStackLine;
    procedure CopyLineInfoByAddr(AnOtherLines: TStackLines);
    property  Lines[Index: Integer]: TStackLine read GetLine;
  end;

  { TStackTrace }

  TStackTrace = class(TStackLines)
  public
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
    function GetLeakInfo(out LeakData: TLeakStatus; var Traces: TList): Boolean; virtual; abstract;
    function ResolveLeakInfo(AFileName: string; Traces: TList): Boolean; virtual; abstract;
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
    FKnownAddresses: TStackLines;

    fParsed   : Boolean;

    function PosInTrc(const SubStr: string; CaseSensetive: Boolean = false): Boolean;
    function IsTraceLine(const SubStr: string; CheckOnlyLineStart: Boolean = False): Boolean;
    function IsHeaderLine(const SubStr: string): Boolean;
    function TrcNumberAfter(var Num: Int64; const AfterSub: string): Boolean;
    function TrcNumberAfter(var Num: Integer; const AfterSub: string): Boolean;
    function TrcNumFirstAndAfter(var FirstNum, AfterNum: Int64; const AfterSub: string): Boolean;

    procedure ParseStackTrace(trace: TStackTrace);
    procedure DoParseTrc(traces: TList);

  public
    TraceInfo : THeapTraceInfo;
    constructor Create(const ATRCFile: string);
    constructor CreateFromTxt(const AText: string);
    destructor Destroy; override;
    function GetLeakInfo(out LeakData: TLeakStatus; var Traces: TList): Boolean; override;
    function ResolveLeakInfo(AFileName: string; Traces: TList): Boolean; override;
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
  else
  if (Offset < length(s)) and (s[Offset] = '0') and (s[Offset+1] = 'x') then i := Offset + 2
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

procedure ParseTraceLine(s: string; var line: TStackLine);
var
  i   : integer;
  {%H-}err : Integer;
  hex : string;
begin
  s := TrimLeft(s);
  i := Pos('$', s);
  if i > 0 then begin
    hex := ExtractHexNumberStr(s, i);
    Val(hex, line.Addr, err);

    if not GetNumberAfter(s, line.LineNum, 'line ') then begin
      line.LineNum := -1;
      line.FileName := '';
    end else begin
      i := Pos(' of ', s);
      if i <= 0 then Exit;
      inc(i, 4);
      line.FileName := Copy(s, i, length(s) - i + 1);
    end;
  end;

  // gdb line?
  i := Pos('#', s);
  if (i < 1) and (copy(s,1,4) = '0000') then i := 4; // mantis mangled line
  if (i > 0) and (i < 5) and (i < Length(s)) and (s[i+1] in ['0'..'9'])
  then begin
    inc(i);
    while (i <= Length(s)) and (s[i] in ['0'..'9']) do inc(i);
    while (i <= Length(s)) and (s[i] in [' ', #9]) do inc(i);
    if (s[i] = '0') and (i < Length(s)) and (s[i+1] = 'x') then begin
      hex := ExtractHexNumberStr(s, i);
      Val(hex, line.Addr, err);
    end;
    line.LineNum := -1;
    line.FileName := '';

    i := pos (' at ', s);
    if i < 1 then
      exit;
    while i > 0 do begin // find last " at "
      delete(s,1,i);
      i := pos (' at ', s);
    end;
    i := pos (':', s);
    line.FileName := Copy(s, 4, i - 4);
    GetNumberAfter(s, line.LineNum, ':')

  end;
end;

{ TStackLine }

function TStackLine.Equals(ALine: TStackLine): boolean;
begin
  Result :=
    (LineNum     = ALine.LineNum) and
    (FileName    = ALine.FileName) and
    (Addr        = ALine.Addr) and
    (RawLineData = ALine.RawLineData);
end;

procedure TStackLine.Assign(ALine: TStackLine);
begin
  LineNum     := ALine.LineNum;
  FileName    := ALine.FileName;
  Addr        := ALine.Addr;
  RawLineData := ALine.RawLineData;
end;

procedure TStackLine.AssignLineAndFile(ALine: TStackLine);
begin
  LineNum     := ALine.LineNum;
  FileName    := ALine.FileName;
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

function THeapTrcInfo.IsTraceLine(const SubStr: string; CheckOnlyLineStart: Boolean): Boolean;
var
  i, l: integer;
  s: String;
begin
  Result := False;

  s := TrimLeft(SubStr);
  i := pos('#', s);
  if (i = 1) or (pos('~"#', s) = 1) then begin
    // gdb
    Result := (i < Length(s)) and (s[i+1] in ['0'..'9']) and
              ( (pos(' at ', s) > 1) or (pos(' from ', s) > 1) );
    Result := Result or CheckOnlyLineStart;
    exit;
  end;
  if copy(s,1,4) = '0000' then begin // leave 3 digits for pos
    // mantis mangled gdb ?
    i := pos(':', s);
    Result := (  ((i > 1) and (i < Length(s)) and (s[i+1] in ['0'..'9'])) or
                 (pos(' in ', s) > 1)  ) and
              ( (pos(' at ', s) > 1) or (pos(' from ', s) > 1) );
    Result := Result or CheckOnlyLineStart;
    exit;
  end;

  // heaptrc line?
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
  Result := Result or CheckOnlyLineStart;
end;

function THeapTrcInfo.IsHeaderLine(const SubStr: string): Boolean;
var
  i: Integer;
begin
  i := 1;
  while (i < length(SubStr)) and (SubStr[i] in [' ', #9, '#', '~', '"', '''']) do inc(i);
  Result := (pos(UpperCase(CallTracePrefix), UpperCase(SubStr)) = i) or
            (pos(UpperCase(RawTracePrefix), UpperCase(SubStr)) = i);
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
  FKnownAddresses := TStackLines.Create;
  fTrcFile := ATrcFile;
  fTRCText := '';
  inherited Create;
end;

constructor THeapTrcInfo.CreateFromTxt(const AText: string);
begin
  FKnownAddresses := TStackLines.Create;
  fTRCText := AText;
end;

destructor THeapTrcInfo.Destroy;
begin
  FreeAndNil(FKnownAddresses);
  inherited Destroy;
end;

procedure THeapTrcInfo.ParseStackTrace(trace: TStackTrace);
var
  i   : integer;
  {%H-}err : integer;
  hex : string;
  NewLine: TStackLine;
  s: String;
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

  if not IsTraceLine(Trc[TrcIndex]) then
    inc(TrcIndex);
  while (TrcIndex < Trc.Count) and (Pos(CallTracePrefix, Trc[TrcIndex]) = 0) and
        (Pos(RawTracePrefix, Trc[TrcIndex]) = 0)
  do begin
    NewLine := TStackLine.Create; // No reference
    trace.Add(NewLine);
    s := Trc[Trcindex];
    if (TrcIndex < Trc.Count-1) and (not IsTraceLine(Trc[Trcindex+1], True)) and
       (not IsHeaderLine(Trc[Trcindex+1]))
    then begin
      // join next line, as there may be a linewrap
      while (length(s) > 0) and (s[length(s)] in [#10,#13]) do delete(s, length(s), 1);
      s := s + Trc[Trcindex+1];
    end;
    ParseTraceLine(s, NewLine);
    NewLine.RawLineData := Trc[Trcindex]; // raw stack line data
    inc(Trcindex);

    if (NewLine.FileName <> '') then begin
      i := FKnownAddresses.IndexOfAddr(NewLine.Addr);
      // Todo: compare addr, to detect inconsistencies
      if i < 0 then
        FKnownAddresses.Add(NewLine);
    end;
  end;
end;


function THeapTrcInfo.GetLeakInfo(out LeakData: TLeakStatus; var Traces: TList): Boolean;
var
  i: Integer;
begin
  Result := false;
  FKnownAddresses.Clear;
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

    for i := 0 to Traces.Count - 1 do
      TStackTrace(Traces[i]).CopyLineInfoByAddr(FKnownAddresses);
  except
    Result := false;
  end;
end;

function THeapTrcInfo.ResolveLeakInfo(AFileName: string; Traces: TList): Boolean;
var
  trace: TStackTrace;
  i, j, k: Integer;
  CurLine: TStackLine;
  FuncName, SrcName: shortstring;
  SrcLine: longint;
  BadAddresses: TStackLines;
begin
  Result := False;
  if not OpenSymbolFile(AFileName) then
    exit;
  BadAddresses := TStackLines.Create;
  try
    for i := 0 to Traces.Count - 1 do begin
      trace := TStackTrace(Traces[i]);
      for j := 0 to trace.Count - 1 do begin
        CurLine := trace.Lines[j];
        if (CurLine.FileName = '') then begin
          k := FKnownAddresses.IndexOfAddr(CurLine.Addr);
          if k >= 0 then
            CurLine.Assign(FKnownAddresses.Lines[k])
          else
          if BadAddresses.IndexOfAddr(CurLine.Addr) < 0 then begin
            if GetLineInfo(CurLine.Addr, FuncName, SrcName, SrcLine) then begin
              CurLine.FileName := SrcName;
              CurLine.LineNum := SrcLine;
              FKnownAddresses.Add(CurLine);
            end
            else begin
              BadAddresses.Add(CurLine);
            end;
          end;
        end;
      end;
    end;
  finally
    CloseSymbolFile;
    FreeAndNil(BadAddresses);
    Result := True;
  end;
end;


{ TStackLines }

function TStackLines.GetLine(Index: Integer): TStackLine;
begin
  Result := TStackLine(FLines[Index]);
end;

constructor TStackLines.Create;
begin
  FLines := TRefCntObjList.Create;
end;

destructor TStackLines.Destroy;
begin
  FLines.Clear;
  FreeAndNil(FLines);
  inherited Destroy;
end;

function TStackLines.Count: Integer;
begin
  Result := FLines.Count;
end;

procedure TStackLines.Clear;
begin
  FLines.Clear;
end;

function TStackLines.Add(ALine: TStackLine): Integer;
begin
  Result := FLines.Add(ALine);
end;

function TStackLines.IndexOfAddr(AnAddr: Int64): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (Lines[Result].Addr <> AnAddr) do
    dec(Result);
end;

function TStackLines.FindAddr(AnAddr: Int64): TStackLine;
var
  i: Integer;
begin
  i := IndexOfAddr(AnAddr);
  if i < 0 then
    Result := nil
  else
    Result := Lines[i];
end;

procedure TStackLines.CopyLineInfoByAddr(AnOtherLines: TStackLines);
var
  i, j: Integer;
  CurLine: TStackLine;
begin
  for i := 0 to Count - 1 do begin
    CurLine := Lines[i];
    if (CurLine.FileName = '') and (CurLine.Addr <> 0) then begin
      j := AnOtherLines.IndexOfAddr(CurLine.Addr);
      if J >= 0 then
        CurLine.AssignLineAndFile(AnOtherLines.Lines[j]);
    end;
  end;
end;

{ TStackTrace }

constructor TStackTrace.Create;
begin
  LeakCount := 1;
  inherited Create;
end;


end.

(*
  Testdata

  full leak trace

  <sample missing>


  fpc trace

   $00C86CF3 line 4015 of ../debugger/gdbmidebugger.pp
  $00CA4EC7 line 11321 of ../debugger/gdbmidebugger.pp
  $00C95663 line 6914 of ../debugger/gdbmidebugger.pp
  $00C95EAE line 7053 of ../debugger/gdbmidebugger.pp
  $00C96E43 line 7491 of ../debugger/gdbmidebugger.pp
  $00C97DCA line 7793 of ../debugger/gdbmidebugger.pp
  $0081FFBA line 6472 of ../debugger/debugger.pp
  $008208DF line 6626 of ../debugger/debugger.pp
  $0080FC76 line 2347 of debugmanager.pas
  $008101AE line 2418 of debugmanager.pas
  $004562D1 line 3290 of main.pp
  $00C73C00 line 411 of ../debugger/assemblerdlg.pp


  GDB -i mi (with line broken by output on shell)

bt
&"bt\n"
~"#0  DOHANDLEMOUSEACTION (this=0x14afae00, ANACTIONLIST=0x14a96af8, ANINFO=...) at synedit.pp:3000\n"
~"#1  0x00aea3e9 in FINDANDHANDLEMOUSEACTION (this=0x14afae00, ABUTTON=MBLEFT, ASHIFT=..., X=233, Y=241, ACCOUNT=CCSINGLE, ADIR=CDDOWN, ANAC
TIONRESULT=..., AWHEELDELTA=0) at synedit.pp:3307\n"
~"#2  0x00aea914 in MOUSEDOWN (this=0x14afae00, BUTTON=MBLEFT, SHIFT=..., X=233, Y=241) at synedit.pp:3374\n"
~"#3  0x005e083b in DOMOUSEDOWN (this=0x14afae00, MESSAGE=..., BUTTON=MBLEFT, SHIFT=...) at include/control.inc:2135\n"
~"#4  0x005e0e8f in WMLBUTTONDOWN (this=0x14afae00, MESSAGE=...) at include/control.inc:2269\n"
~"#5  0x0040d096 in DISPATCH (this=0xeebf6d4, MESSAGE=0) at ../inc/objpas.inc:592\n"
~"#6  0x005e06e3 in WNDPROC (this=0x14afae00, THEMESSAGE=...) at include/control.inc:2099\n"
~"#7  0x005d1b88 in WNDPROC (this=0x14afae00, MESSAGE=...) at include/wincontrol.inc:5327\n"
~"#8  0x00af3b76 in WNDPROC (this=0x14afae00, MSG=...) at synedit.pp:5740\n"
~"#9  0x006666a0 in DELIVERMESSAGE (TARGET=0x14afae00, AMESSAGE=0) at lclmessageglue.pas:112\n"
~"#10 0x0057ad0e in WINDOWPROC (WINDOW=3934144, MSG=513, WPARAM=1, LPARAM=15794409) at win32/win32callback.inc:2478\n"
~"#11 0x7673fd72 in ?? () from C:\\Windows\\system32\\user32.dll\n"
~"#12 0x7673fe4a in ?? () from C:\\Windows\\system32\\user32.dll\n"
~"#13 0x7674018d in ?? () from C:\\Windows\\system32\\user32.dll\n"
~"#14 0x7674022b in ?? () from C:\\Windows\\system32\\user32.dll\n"
~"#15 0x0057e0b8 in APPPROCESSMESSAGES (this=0x183d58) at win32/win32object.inc:367\n"
~"#16 0x0043d9e1 in HANDLEMESSAGE (this=0x12bf68) at include/application.inc:1257\n"
~"#17 0x0043df56 in RUNLOOP (this=0x12bf68) at include/application.inc:1390\n"
~"#18 0x00490481 in APPRUN (this=0x183d58, ALOOP=...) at include/interfacebase.inc:54\n"
~"#19 0x0043defb in RUN (this=0x12bf68) at include/application.inc:1378\n"
~"#20 0x0040358f in main () at lazarus.pp:128\n"
^done


  GDB mangled by mantis

0000001 gdk_drawable_copy_to_image at :0
0000002 gdk_pixbuf_get_from_drawable at :0
0000003 PAINTWINDOW(0x7fffffffc2d0, 0xd0a480) at gtk2/gtk2wscontrols.pp:1021
0000004 PAINTWIDGET(0x7fffffffc2d0, 0xcf28f0) at gtk2/gtk2wscontrols.pp:1038
0000005 PAINTTO(0x7ffff7fb36f0, 0x7ffff7fb30b0, 140737353635824, 0, 0) at gtk2/gtk2wscontrols.pp:1045
0000006 PAINTTO(0x7ffff7fb30b0, 140737353635824, 0, 0) at include/wincontrol.inc:4968
0000007 PAINTTO(0x7ffff7fb30b0, 0x7ffff04de480, 0, 0) at include/wincontrol.inc:4973
0000008 PAINTBOX1PAINT(0x7ffff7fbd3b0, 0x7ffff7fbdbd0) at r26u01.pas:36
0000009 PAINT(0x7ffff7fbdbd0) at include/graphiccontrol.inc:90
0000010 PAINT(0x7ffff7fbdbd0) at include/paintbox.inc:45
0000011 WMPAINT(0x7ffff7fbdbd0, {MSG = 15, UNUSEDMSG = 1431655765, DC = 140737353635824, PAINTSTRUCT = 0x0, RESULT = 0}) at include/graphiccontrol.inc:58
0000012 SYSTEM_TOBJECT_$__DISPATCH$formal at :0

*)
