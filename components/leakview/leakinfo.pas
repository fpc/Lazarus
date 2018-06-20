unit leakinfo;
(*
  Testdata at aend of file
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // CodeTools
  CodeToolManager, CodeCache,
  // LazUtils
  FileUtil, LazFileUtils, LazClasses,
  // IDEIntf
  TextTools,
  // LeakView
  DbgInfoReader, DbgIntfBaseTypes, LldbHelper;

type
  { TStackLine }

  TStackLine = class(TRefCountedObject)
    LineNum   : Integer;  // -1 is line is unknown
    Column    : Integer;  // 0 is unknown
    FileName  : string;   // should be empty if file is unknown
    Addr      : Int64;    // -1 if address is unknown
    RawLineData: string;
    function Equals(ALine: TStackLine): boolean; reintroduce;
    procedure Assign(ALine: TStackLine);
    procedure AssignSourcePos(ALine: TStackLine);
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
    // returns True, if information has been successfully received, False otherwise
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
  private
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
    function IsHeaderLine(Idx: integer; NoneStrict: Boolean = False): Boolean;
    function TrcNumberAfter(var Num: Int64; const AfterSub: string): Boolean;
    function TrcNumberAfter(var Num: Integer; const AfterSub: string): Boolean;
    function TrcNumFirstAndAfter(var FirstNum, AfterNum: Int64; const AfterSub: string): Boolean;

    procedure ParseTraceLine(s: string; var line: TStackLine);
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

{ TStackLine }

function TStackLine.Equals(ALine: TStackLine): boolean;
begin
  Result :=
    (LineNum     = ALine.LineNum) and
    (Column      = ALine.Column) and
    (FileName    = ALine.FileName) and
    (Addr        = ALine.Addr) and
    (RawLineData = ALine.RawLineData);
end;

procedure TStackLine.Assign(ALine: TStackLine);
begin
  LineNum     := ALine.LineNum;
  Column      := ALine.Column;
  FileName    := ALine.FileName;
  Addr        := ALine.Addr;
  RawLineData := ALine.RawLineData;
end;

procedure TStackLine.AssignSourcePos(ALine: TStackLine);
begin
  LineNum     := ALine.LineNum;
  Column      := ALine.Column;
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

function IsValgrindLine(s: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if pos('==', s) <> 1 then exit;
  i := 3;
  while (i<=Length(s)) and (s[i] in ['0'..'9']) do inc(i);
  if (i+2 < Length(s)) and (s[i] = '=') and (s[i+1] = '=')  and (s[i+2] = ' ') then
    Result := true;
end;

function IsValgrindTraceLine(s: String; RequireAT: boolean = false): Boolean;
var
  i: Integer;
begin
  Result := False;
  if pos('==', s) <> 1 then exit;
  i := 3;
  while (i<=Length(s)) and (s[i] in ['0'..'9']) do inc(i);
  if (i+2 < Length(s)) and (s[i] = '=') and (s[i+1] = '=')  and (s[i+2] = ' ') then i := i + 3;
  while (i<=Length(s)) and (s[i] = ' ') do inc(i);

  if (i+2 < Length(s)) and
     ( (copy(s, i, 3) = 'at ') or
       ( (copy(s, i, 3) = 'by ') and not RequireAT )
     )
  then
    Result := true;
end;

function THeapTrcInfo.IsTraceLine(const SubStr: string; CheckOnlyLineStart: Boolean): Boolean;

  function IsGDBLine(s: string): boolean;
  begin
    Result:=false;
    if REMatches(s,'^#[0-9]+ +0x[0-9a-f]+ in ') then begin
      // gdb
      //   #4  0x007489de in EXTTOOLEDITDLG_TEXTERNALTOOLMENUITEMS_$__LOAD$TCONFIGSTORAGE$$TMODALRESULT ()
      Result:=true;
    end;
    if REMatches(s,'^#[0-9] .* (at|from) ') then begin
      // gdb
      //   #0 DOHANDLEMOUSEACTION (this=0x14afae00, ANACTIONLIST=0x14a96af8,ANINFO=...) at synedit.pp:3000
      Result:=true;
    end;
  end;

var
  i, l: integer;
  s: String;
begin
  Result := False;

  s := Trim(SubStr);
  if s='' then exit;

  if (LeftStr(s,3) = '~"#') then begin
    // gdb
    Delete(s,1,2);
    if s[length(s)]='"' then Delete(s,length(s),1);
    Result:=CheckOnlyLineStart or IsGDBLine(s);
  end else if LeftStr(s,4) = '0000' then begin // leave 3 digits for pos
    // mantis mangled gdb
    //  line format: 0000.*:0.* in .* (at|from) .*
    i := pos(':', s);
    Result := (  ((i > 1) and (i < Length(s)) and (s[i+1] in ['0'..'9'])) or
                 (pos(' in ', s) > 1)  ) and
              ( (pos(' at ', s) > 1) or (pos(' from ', s) > 1) );
    Result := Result or CheckOnlyLineStart;
  end else if IsGDBLine(s) then begin
    Result := true;
  end else if pos('frame #', s) > 0 then begin
    Result := True;
  end else if IsValgrindTraceLine(s) then begin
    Result := True;
  end else begin
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
    Result := (Pos('line', SubStr) > 0)
            or CheckOnlyLineStart;
  end;
end;

function THeapTrcInfo.IsHeaderLine(Idx: integer; NoneStrict: Boolean): Boolean;
var
  i: Integer;
  SubStr: String;
begin
  SubStr := Trc[Idx];
  if (pos('==', SubStr) = 1) and (Idx<Trc.Count-1) and IsValgrindTraceLine(Trc[Idx+1], True) then begin
    Result := True;
    exit;
  end;

  if NoneStrict and (  PosInTrc(RawTracePrefix) or PosInTrc(CallTracePrefix) ) then begin
    Result := True;
    exit;
  end;

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
  if TrcIndex >= Trc.Count then Exit;
  TraceInfo.ExeName := Trc[TrcIndex];

  while (TrcIndex < Trc.Count)
    and not( PosInTrc('Heap dump') or  IsHeaderLine(TrcIndex, True) or IsTraceLine(Trc[TrcIndex]) )
  do
    inc(TrcIndex);

  if TrcIndex >= Trc.Count then Exit;

  if PosInTrc('Heap dump') then begin
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
    if PosInTrc(CallTracePrefix) or
       ( (TrcIndex < Trc.Count-1) and IsHeaderLine(TrcIndex) and IsTraceLine(Trc[TrcIndex+1]) ) or
       IsTraceLine(Trc[TrcIndex])
    then begin
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

procedure THeapTrcInfo.ParseTraceLine(s: string; var line: TStackLine);

  procedure ReadValgrindLine;
  var
    i, SrcX, SrcY, SrcTopLine: Integer;
    fn, TheErrorMsg: String;
    Complete: boolean;
    SrcCode: TCodeBuffer;
  begin
    i := 3;
    while (i<=Length(s)) and (s[i] in ['0'..'9']) do inc(i);
    if (i+2 < Length(s)) and (s[i] = '=') and (s[i+1] = '=')  and (s[i+2] = ' ')
    then i := i + 3
    else exit;
    while (i<=Length(s)) and (s[i] = ' ') do inc(i);
    if (i+2 < Length(s)) and ( (copy(s, i, 3) = 'at ') or (copy(s, i, 3) = 'by ') )
    then i := i + 3
    else exit;
    Delete(s, 1, i-1);

    i := pos(' ', s);
    if i < 3 then exit;
    line.Addr := StrToInt64Def(copy(s,1,i-1), 0);
    Delete(s, 1, i);

    i := pos(' (', s);
    if i < 3 then exit;
    fn := copy(s,1,i-1);
    Delete(s, 1, i);

    i := pos(':', s);
    if i < 3 then exit;
    line.FileName := copy(s,1,i-1);
    Delete(s, 1, i);

    i := pos(')', s);
    if i < 3 then exit;
    line.LineNum := StrToIntDef(copy(s,1,i-1), 0);
    Delete(s, 1, i);

    if (fn <> '') and (line.FileName = '') then begin
      CodeToolBoss.FindFPCMangledIdentifier(fn ,Complete,TheErrorMsg,nil,
         SrcCode,SrcX,SrcY,SrcTopLine);
      if SrcCode<>nil then begin
        line.FileName:=SrcCode.Filename;
        line.LineNum:=SrcY;
        line.Column:=SrcX;
      end;
    end;
  end;

  procedure ReadLLDBLine;
  var
    AnId, SrcX, SrcY, SrcTopLine: Integer;
    AnIsCurrent, Complete: Boolean;
    AFuncName, AFile, AReminder, TheErrorMsg: String;
    AnArgs: TStringList;
    SrcCode: TCodeBuffer;
  begin
    ParseFrameLocation(s, AnId, AnIsCurrent, TDBGPtr(line.Addr), AFuncName,
      AnArgs, line.FileName, line.LineNum, AReminder);
    AnArgs.Free;

    if (line.FileName = '') and (AFuncName <> '') then begin
      CodeToolBoss.FindFPCMangledIdentifier(AFuncName,Complete,TheErrorMsg,nil,
        SrcCode,SrcX,SrcY,SrcTopLine);
      if SrcCode<>nil then begin
        line.FileName:=SrcCode.Filename;
        line.LineNum:=SrcY;
        line.Column:=SrcX;
      end;
    end;
  end;

  procedure ReadGDBLine;
  var
    p: PChar;
    StartP: PChar;
    hex: String;
    {%H-}err : Integer;
    i: SizeInt;
    GDBIdentifier: String;
    Complete: boolean;
    TheErrorMsg: string;
    SrcCode: TCodeBuffer;
    SrcX, SrcY, SrcTopLine: integer;
  begin
    p:=PChar(s);
    // read stack depth decimal number
    if not (p^ in ['0'..'9']) then exit;
    while p^ in ['0'..'9'] do inc(p);
    if p^<>' ' then exit;
    while p^=' ' do inc(p);
    // read address (hex number $000 or 0x000)
    if p^='$' then
      inc(p)
    else if (p^='0') and (p[1]='x') then
      inc(p,2)
    else
      exit;
    StartP:=p;
    while p^ in ['0'..'9','a'..'z'] do inc(p);
    hex:=copy(s,StartP-PChar(s)+1,p-StartP);
    Val(hex, line.Addr, err);

    line.LineNum := -1;
    line.FileName := '';
    Delete(s,1,p-PChar(s));

    i := pos (' at ', s);
    if i > 0 then
    begin
      // ' at <filename>:<line number>'
      while i > 0 do begin // find last " at "
        delete(s,1,i);
        i := pos (' at ', s);
      end;
      i := pos (':', s);
      line.FileName := Copy(s, 4, i - 4);
      GetNumberAfter(s, line.LineNum, ':')
    end else if LeftStr(s,4)=' in ' then begin
      // for example:
      //   #4  0x007489de in EXTTOOLEDITDLG_TEXTERNALTOOLMENUITEMS_$__LOAD$TCONFIGSTORAGE$$TMODALRESULT ()
      Delete(s,1,4);
      i:=1;
      while (i<=length(s)) and (s[i] in ['a'..'z','A'..'Z','0'..'9','_','$','?']) do
        inc(i);
      GDBIdentifier:=copy(s,1,i-1);
      CodeToolBoss.FindFPCMangledIdentifier(GDBIdentifier,Complete,TheErrorMsg,nil,
         SrcCode,SrcX,SrcY,SrcTopLine);
      if SrcCode<>nil then begin
        line.FileName:=SrcCode.Filename;
        line.LineNum:=SrcY;
        line.Column:=SrcX;
      end;
    end;
  end;

var
  i   : integer;
  {%H-}err : Integer;
  hex : string;
begin
  //DebugLn(['ParseTraceLine "',s,'"']);
  s := Trim(s);

  // gdb
  if (LeftStr(s,3) = '~"#') then begin
    // gdb
    Delete(s,1,3);
    if s[length(s)]='"' then Delete(s,length(s),1);
    ReadGDBLine;
  end else if LeftStr(s,4) = '0000' then begin
    // mantis mangled gdb
    ReadGDBLine;
  end else if REMatches(s,'^#[0-9]+ +0x[0-9a-f]+ in ') then begin
    // gdb
    //   #4  0x007489de in EXTTOOLEDITDLG_TEXTERNALTOOLMENUITEMS_$__LOAD$TCONFIGSTORAGE$$TMODALRESULT ()
    Delete(s,1,1);
    ReadGDBLine;
  end
  else
  // lldb
  if pos('frame #', s) > 0 then begin
    if s[1] = '*' then
      s := '  ' + s // restore spaces
    else
      s := '    ' + s; // restore spaces
    ReadLLDBLine;
  end
  else // valgrind
  if IsValgrindTraceLine(s) then begin
    ReadValgrindLine;
  end else begin
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
  end;

  // gdb line?
  {i := Pos('#', s);
  if (i < 1) and (copy(s,1,4) = '0000') then i := 4; // mantis mangled line
  if (i > 0) and (i < 5) and (i < Length(s)) and (s[i+1] in ['0'..'9'])
  then begin
    inc(i);
    // read stack depth (a decimal number)
    while (i <= Length(s)) and (s[i] in ['0'..'9']) do inc(i);
    while (i <= Length(s)) and (s[i] in [' ', #9]) do inc(i);
    // read proc address (a hex number 0x00..00)
    if (s[i] = '0') and (i < Length(s)) and (s[i+1] = 'x') then begin
      hex := ExtractHexNumberStr(s, i);
      Val(hex, line.Addr, err);
    end;
    line.LineNum := -1;
    line.FileName := '';

    i := pos (' at ', s);
    if i > 0 then
    begin
      // ' at <filename>:<line number>'
      while i > 0 do begin // find last " at "
        delete(s,1,i);
        i := pos (' at ', s);
      end;
      i := pos (':', s);
      line.FileName := Copy(s, 4, i - 4);
      GetNumberAfter(s, line.LineNum, ':')
    end else if LeftStr(s,4)=' in ' then begin
      // for example:
      //   #4  0x007489de in EXTTOOLEDITDLG_TEXTERNALTOOLMENUITEMS_$__LOAD$TCONFIGSTORAGE$$TMODALRESULT ()
      Delete(s,1,4);
      i:=1;
      while (i<=length(s)) and (s[i] in ['a'..'z','A'..'Z','0'..'9','_','$','?']) do
        inc(i);
      GDBIdentifier:=copy(s,1,i-1);
      CodeToolBoss.FindFPCMangledIdentifier(GDBIdentifier,Complete,TheErrorMsg,@OnFindCTSource,
         SrcCode,SrcX,SrcY,SrcTopLine);
      if SrcCode<>nil then begin
        line.FileName:=SrcCode.Filename;
        line.LineNum:=SrcY;
        line.Column:=SrcX;
      end;
    end;
  end;}
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
  if (i <= 0) and (not IsTraceLine(Trc[TrcIndex])) and (not IsValgrindLine(Trc[TrcIndex])) then begin
    i := Pos(CallTracePrefix, Trc[TrcIndex]);
    if i <= 0 then begin
      inc(TrcIndex);
      Exit;
    end;

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

  if IsValgrindLine(Trc[TrcIndex]) and IsHeaderLine(TrcIndex)  // add valgrind headers
  then begin
    NewLine := TStackLine.Create; // No reference
    trace.Add(NewLine);
    NewLine.RawLineData := Trc[Trcindex]; // raw stack line data
    inc(Trcindex);
  end
  else
  if (not IsTraceLine(Trc[TrcIndex])) then
    inc(TrcIndex);

  while (TrcIndex < Trc.Count) and (not IsHeaderLine(TrcIndex, True))
  do begin
    NewLine := TStackLine.Create; // No reference
    trace.Add(NewLine);
    s := Trc[Trcindex];
    if (TrcIndex < Trc.Count-1) and (not IsTraceLine(Trc[Trcindex+1], True)) and
       (not IsHeaderLine(Trcindex+1))
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
        CurLine.AssignSourcePos(AnOtherLines.Lines[j]);
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
