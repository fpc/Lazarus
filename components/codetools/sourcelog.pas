{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Defines TSourceLog which manage a source (= an ansistring) and all changes
    like inserting, deleting and moving parts of it.
}
unit SourceLog;

{$ifdef fpc}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils,
  // LazUtils
  LazFileUtils, LazUTF8, LazUTF8Classes, LazDbgLog, LazStringUtils;

type
  TSourceLog = class;

  TSourceLogEntryOperation = (sleoInsert, sleoDelete, sleoMove);
  TOnSourceLogInsert = procedure(Sender: TSourceLog; Pos: integer;
                        const Txt: string) of object;
  TOnSourceLogDelete = procedure(Sender: TSourceLog; Pos, Len: integer)
                        of object;
  TOnSourceLogMove = procedure(Sender: TSourceLog; Pos, Len, MoveTo: integer)
                      of object;
  TOnSourceLogDecodeLoaded = procedure(Sender: TSourceLog; const Filename: string;
                        var Source, DiskEncoding, MemEncoding: string) of object;
  TOnSourceLogEncodeSaving = procedure(Sender: TSourceLog;
                          const Filename: string; var Source: string) of object;

  TSourceLogEntry = class
  private
  public
    Position: integer;
    Len: integer;
    MoveTo: integer;
    LineEnds: integer; // number of line ends in txt
    LengthOfLastLine: integer;
    Txt: string;
    Operation: TSourceLogEntryOperation;
    procedure AdjustPosition(var APosition: integer);
    constructor Create(APos, ALength, AMoveTo: integer; const ATxt: string;
      AnOperation: TSourceLogEntryOperation);
  end;
  
  TOnSourceChange = procedure(Sender: TSourceLog; Entry: TSourceLogEntry)
                         of object;

  { TSourceLogMarker }

  TSourceLogMarker = class
  private
  public
    Position: integer;
    NewPosition: integer;
    Deleted: boolean;
    Data: Pointer;
    Log: TSourceLog;
    destructor Destroy; override;
  end;

  TLineRange = packed record
    StartPos, EndPos: integer;
  end;
  PLineRange = ^TLineRange;

  { TSourceLog }

  TSourceLog = class
  private
    FDiskEncoding: string;
    FDiskLineEnding: string;
    FLineCount: integer;
    FLineRanges: PLineRange;
    FMemEncoding: string;
    FOnDecodeLoaded: TOnSourceLogDecodeLoaded;
    FOnEncodeSaving: TOnSourceLogEncodeSaving;
              // array of TLineRange
    FSrcLen: integer;
    FLog: TFPList; // list of TSourceLogEntry
    FMarkers: TFPList; // list of TSourceLogMarker;
    FModified: boolean;
    FOnInsert: TOnSourceLogInsert;
    FOnDelete: TOnSourceLogDelete;
    FOnMove: TOnSourceLogMove;
    FChangeHooks: {$ifdef fpc}^{$else}array of {$endif}TOnSourceChange;
    FChangeHookCount: integer;
    FChangeHookDelayed: boolean;
    FSource: string;
    FChangeStep: integer;
    FReadOnly: boolean;
    FWriteLock: integer;
    FChangeHookLock: integer;
    procedure SetSource(const NewSrc: string);
    function GetItems(Index: integer): TSourceLogEntry;
    procedure SetItems(Index: integer; AnItem: TSourceLogEntry);
    function GetMarkers(Index: integer): TSourceLogMarker;
    procedure BuildLineRanges;
    procedure SetReadOnly(const Value: boolean);
    function IndexOfChangeHook(AChangeHook: TOnSourceChange): integer;
  protected
    procedure IncreaseChangeStep; virtual; // any change
    procedure DoSourceChanged; virtual; // source change
    procedure DecodeLoaded(const AFilename: string;
                        var ASource, ADiskEncoding, AMemEncoding: string); virtual;
    procedure EncodeSaving(const AFilename: string; var ASource: string); virtual;
  public
    Data: Pointer;
    LastError: string;
    function LineCount: integer;
    function GetLine(Index: integer; WithLineEnd: boolean = true): string; // 0-based
    function GetLineLength(Index: integer): integer; // 0-based
    procedure GetLineRange(Index: integer; out LineRange: TLineRange); // 0-based
    function GetLineStart(Index: integer): integer; // 1-based
    property Items[Index: integer]: TSourceLogEntry
       read GetItems write SetItems; default;
    function Count: integer; // # Items
    property SourceLength: integer read fSrcLen;
    function ClearEntries: boolean;
    property ChangeStep: integer read FChangeStep;
    property Markers[Index: integer]: TSourceLogMarker read GetMarkers;
    function MarkerCount: integer;
    function AddMarker(Position: integer; SomeData: Pointer): TSourceLogMarker;
    function AddMarkerXY(Line, Column: integer; SomeData: Pointer): TSourceLogMarker;
    procedure AdjustPosition(var APosition: integer);
    procedure NotifyHooks(Entry: TSourceLogEntry);
    procedure IncreaseHookLock;
    procedure DecreaseHookLock;
    property Source: string read FSource write SetSource;
    property Modified: boolean read FModified write FModified;
    // Line and Column begin at 1
    procedure LineColToPosition(Line, Column: integer; out Position: integer);
    procedure AbsoluteToLineCol(Position: integer; out Line, Column: integer);
    function LineColIsOutside(Line, Column: integer): boolean;
    function LineColIsSpace(Line, Column: integer): boolean;
    function AbsoluteToLineColStr(Position: integer): string;
    procedure Insert(Pos: integer; const Txt: string);
    procedure Delete(Pos, Len: integer);
    procedure Replace(Pos, Len: integer; const Txt: string);
    procedure Move(Pos, Len, MoveTo: integer);
    function LoadFromFile(const Filename: string): boolean; virtual;
    function SaveToFile(const Filename: string): boolean; virtual;
    function GetLines(StartLine, EndLine: integer): string;
    function IsEqual(sl: TStrings): boolean;
    function OldIsEqual(sl: TStrings): boolean;
    procedure Assign(sl: TStrings);
    procedure AssignTo(sl: TStrings; UseAddStrings: Boolean);
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToStream(aStream: TStream);
    property ReadOnly: boolean read FReadOnly write SetReadOnly;
    property DiskEncoding: string read FDiskEncoding write FDiskEncoding;
    property MemEncoding: string read FMemEncoding write FMemEncoding;
    property DiskLineEnding: string read FDiskLineEnding write FDiskLineEnding;
    property WriteLock: integer read FWriteLock;
    procedure IncWriteLock;
    procedure DecWriteLock;
    procedure Clear; virtual; // clear content, not Encoding, not LineEnding
    function ConsistencyCheck: integer;
    function CalcMemSize: PtrUInt; virtual;
    constructor Create(const ASource: string);
    destructor Destroy; override;
    
    procedure AddChangeHook(AnOnSourceChange: TOnSourceChange);
    procedure RemoveChangeHook(AnOnSourceChange: TOnSourceChange);
    property OnInsert: TOnSourceLogInsert read FOnInsert write FOnInsert;
    property OnDelete: TOnSourceLogDelete read FOnDelete write FOnDelete;
    property OnMove: TOnSourceLogMove read FOnMove write FOnMove;
    property OnDecodeLoaded: TOnSourceLogDecodeLoaded read FOnDecodeLoaded
                                                      write FOnDecodeLoaded;
    property OnEncodeSaving: TOnSourceLogEncodeSaving read FOnEncodeSaving
                                                      write FOnEncodeSaving;
  end;
  
implementation

{ TSourceLogEntry }

constructor TSourceLogEntry.Create(APos, ALength, AMoveTo: integer;
  const ATxt: string; AnOperation: TSourceLogEntryOperation);
begin
  Position:=APos;
  Len:=ALength;
  MoveTo:=AMoveTo;
  Operation:=AnOperation;
  LineEnds:=LineEndingCount(Txt, LengthOfLastLine);
  Txt:=ATxt;
end;

procedure TSourceLogEntry.AdjustPosition(var APosition: integer);
begin
  case Operation of
    sleoInsert:
      if APosition>=Position then inc(APosition,Len);
    sleoDelete:
      if (APosition>=Position) then begin
        if APosition>=Position+Len then
          dec(APosition,Len)
        else
          APosition:=Position;
      end;
    sleoMove:
      if Position<MoveTo then begin
        if APosition>=Position then begin
          if APosition<Position+Len then
            inc(APosition,MoveTo-Position)
          else if APosition<MoveTo then
            dec(APosition,Len);
        end;
      end else begin
        if APosition>=MoveTo then begin
          if APosition<Position then
            inc(APosition,Len)
          else if APosition<Position+Len then
            dec(APosition,Position-MoveTo);
        end;
      end;
  end;
end;


{ TSourceLogMarker }

{ TSourceLog }

constructor TSourceLog.Create(const ASource: string);
begin
  inherited Create;
  FModified:=false;
  FSource:=ASource;
  FSrcLen:=length(FSource);
  FLog:=TFPList.Create;
  FMarkers:=TFPList.Create;
  FLineRanges:=nil;
  FLineCount:=-1;
  FChangeStep:=0;
  Data:=nil;
  FChangeHooks:=nil;
  FChangeHookCount:=0;
  FReadOnly:=false;
end;

destructor TSourceLog.Destroy;
var
  i: Integer;
begin
  if FChangeHooks<>nil then begin
    FreeMem(FChangeHooks);
    FChangeHooks:=nil;
  end;
  Clear;
  for i:=FMarkers.Count-1 downto 0 do begin
    Markers[i].Log:=nil;
    Markers[i].Free;
  end;
  FMarkers.Free;
  FLog.Free;
  inherited Destroy;
end;

function TSourceLog.LineCount: integer;
begin
  if fLineCount<0 then BuildLineRanges;
  Result:=fLineCount;
end;

function TSourceLog.GetLine(Index: integer; WithLineEnd: boolean): string;
var LineLen: integer;
begin
  BuildLineRanges;
  if (Index>=0) and (Index<fLineCount) then begin
    if WithLineEnd then begin
      if Index<fLineCount-1 then
        LineLen:=fLineRanges[Index+1].StartPos-fLineRanges[Index].StartPos
      else
        LineLen:=fSrcLen-fLineRanges[Index].StartPos+1;
    end else begin
      LineLen:=fLineRanges[Index].EndPos-fLineRanges[Index].StartPos
    end;
    SetLength(Result,LineLen);
    if LineLen>0 then
      System.Move(fSource[fLineRanges[Index].StartPos],Result[1],LineLen);
  end else
    Result:='';
end;

function TSourceLog.GetLineLength(Index: integer): integer;
begin
  BuildLineRanges;
  if (Index>=0) and (Index<fLineCount) then
    Result:=fLineRanges[Index].EndPos-fLineRanges[Index].StartPos
  else
    Result:=0;
end;

procedure TSourceLog.GetLineRange(Index: integer; out LineRange: TLineRange);
begin
  BuildLineRanges;
  LineRange:=FLineRanges[Index];
end;

function TSourceLog.GetLineStart(Index: integer): integer;
begin
  BuildLineRanges;
  if Index<FLineCount then
    Result:=FLineRanges[Index].StartPos
  else
    Result:=FSrcLen;
end;

function TSourceLog.ClearEntries: boolean;
var i: integer;
begin
  if (Count=0) and (FLog.Count=0) then exit(false);
  Result:=true;
  for i:=0 to Count-1 do Items[i].Free;
  FLog.Clear;
end;

procedure TSourceLog.Clear;
var i: integer;
  m: TSourceLogMarker;
  SourceChanged: Boolean;
begin
  ClearEntries; // ignore if entries change
  // markers are owned by someone else, do not free them
  for i:=0 to FMarkers.Count-1 do begin
    m:=Markers[i];
    if m.Position>1 then
      m.Deleted:=true;
  end;
  SourceChanged:=FSource<>'';
  FSource:='';
  FSrcLen:=0;
  FModified:=false;
  if FLineRanges<>nil then begin
    FreeMem(FLineRanges);
    FLineRanges:=nil;
  end;
  FLineCount:=-1;
  Data:=nil;
  FReadOnly:=false;
  IncreaseChangeStep;
  if SourceChanged then
    DoSourceChanged;
  NotifyHooks(nil);
end;

function TSourceLog.GetItems(Index: integer): TSourceLogEntry;
begin
  Result:=TSourceLogEntry(FLog[Index]);
end;

procedure TSourceLog.SetItems(Index: integer; AnItem: TSourceLogEntry);
begin
  FLog[Index]:=AnItem;
end;

function TSourceLog.Count: integer;
begin
  Result:=fLog.Count;
end;

function TSourceLog.GetMarkers(Index: integer): TSourceLogMarker;
begin
  Result:=TSourceLogMarker(FMarkers[Index]);
end;

function TSourceLog.MarkerCount: integer;
begin
  Result:=fMarkers.Count;
end;

procedure TSourceLog.NotifyHooks(Entry: TSourceLogEntry);
var i: integer;
begin
  if (FChangeHooks=nil) or (FChangeHookLock>0) then begin
    FChangeHookDelayed:=true;
    exit;
  end;
  FChangeHookDelayed:=false;
  for i:=0 to FChangeHookCount-1 do
    FChangeHooks[i](Self,Entry);
end;

procedure TSourceLog.IncreaseHookLock;
begin
  inc(FChangeHookLock);
end;

procedure TSourceLog.DecreaseHookLock;
begin
  if FChangeHookLock<=0 then exit;
  dec(FChangeHookLock);
  if (FChangeHookLock=0) and FChangeHookDelayed then
    NotifyHooks(nil);
end;

procedure TSourceLog.SetSource(const NewSrc: string);
begin
  //DebugLn('TSourceLog.SetSource A ',length(NewSrc),' LineCount=',fLineCount,' SrcLen=',fSrcLen);
  if NewSrc<>FSource then begin
    inc(FChangeHookLock);
    try
      Clear;
      FSource:=NewSrc;
      FSrcLen:=length(FSource);
      FLineCount:=-1;
      FReadOnly:=false;
      DoSourceChanged;
    finally
      dec(FChangeHookLock);
    end;
    NotifyHooks(nil);
  end;
end;

procedure TSourceLog.Insert(Pos: integer; const Txt: string);
var i: integer;
  NewSrcLogEntry: TSourceLogEntry;
begin
  if Txt='' then exit;
  if Assigned(FOnInsert) then FOnInsert(Self,Pos,Txt);
  NewSrcLogEntry:=TSourceLogEntry.Create(Pos,length(Txt),-1,Txt,sleoInsert);
  FLog.Add(NewSrcLogEntry);
  NotifyHooks(NewSrcLogEntry);
  FSource:=copy(FSource,1,Pos-1)
          +Txt
          +copy(FSource,Pos,length(FSource)-Pos+1);
  FSrcLen:=length(FSource);
  FLineCount:=-1;
  for i:=0 to FMarkers.Count-1 do begin
    if (not Markers[i].Deleted) then
      NewSrcLogEntry.AdjustPosition(Markers[i].NewPosition);
  end;
  FModified:=true;
  DoSourceChanged;
end;

procedure TSourceLog.Delete(Pos, Len: integer);
var i: integer;
  NewSrcLogEntry: TSourceLogEntry;
begin
  if Len=0 then exit;
  if Assigned(FOnDelete) then FOnDelete(Self,Pos,Len);
  NewSrcLogEntry:=TSourceLogEntry.Create(Pos,Len,-1,'',sleoDelete);
  FLog.Add(NewSrcLogEntry);
  NotifyHooks(NewSrcLogEntry);
  System.Delete(FSource,Pos,Len);
  FSrcLen:=length(FSource);
  FLineCount:=-1;
  for i:=0 to FMarkers.Count-1 do begin
    if (Markers[i].Deleted=false) then begin
      if (Markers[i].NewPosition<=Pos) and (Markers[i].NewPosition<Pos+Len) then
        Markers[i].Deleted:=true
      else begin
        NewSrcLogEntry.AdjustPosition(Markers[i].NewPosition);
      end;
    end;
  end;
  FModified:=true;
  DoSourceChanged;
end;

procedure TSourceLog.Replace(Pos, Len: integer; const Txt: string);
var i: integer;
  DeleteSrcLogEntry, InsertSrcLogEntry: TSourceLogEntry;
begin
  if (Len=0) and (Txt='') then exit;
  if Len=length(Txt) then begin
    i:=1;
    while (i<=Len) and (FSource[Pos+i-1]=Txt[i]) do inc(i);
    if i>Len then exit;
  end;
  if Assigned(FOnDelete) then FOnDelete(Self,Pos,Len);
  if Assigned(FOnInsert) then FOnInsert(Self,Pos,Txt);
  DeleteSrcLogEntry:=TSourceLogEntry.Create(Pos,Len,-1,'',sleoDelete);
  FLog.Add(DeleteSrcLogEntry);
  NotifyHooks(DeleteSrcLogEntry);
  InsertSrcLogEntry:=TSourceLogEntry.Create(Pos,length(Txt),-1,Txt,sleoInsert);
  FLog.Add(InsertSrcLogEntry);
  NotifyHooks(InsertSrcLogEntry);
  FSource:=copy(FSource,1,Pos-1)
          +Txt
          +copy(FSource,Pos+Len,length(FSource)-Pos-Len+1);
  FSrcLen:=length(FSource);
  FLineCount:=-1;
  for i:=0 to FMarkers.Count-1 do begin
    if (Markers[i].Deleted=false) then begin
      if (Markers[i].NewPosition<=Pos) and (Markers[i].NewPosition<Pos+Len) then
        Markers[i].Deleted:=true
      else begin
        DeleteSrcLogEntry.AdjustPosition(Markers[i].NewPosition);
        InsertSrcLogEntry.AdjustPosition(Markers[i].NewPosition);
      end;
    end;
  end;
  FModified:=true;
  DoSourceChanged;
end;

procedure TSourceLog.Move(Pos, Len, MoveTo: integer);
var i: integer;
  NewSrcLogEntry: TSourceLogEntry;
begin
  if Assigned(FOnMove) then FOnMove(Self,Pos,Len,MoveTo);
  if (MoveTo>=Pos) and (MoveTo<Pos+Len) then exit;
  NewSrcLogEntry:=TSourceLogEntry.Create(Pos,Len,MoveTo,'',sleoMove);
  FLog.Add(NewSrcLogEntry);
  NotifyHooks(NewSrcLogEntry);
  if MoveTo<Pos then begin
    FSource:=copy(FSource,1,MoveTo-1)
            +copy(FSource,Pos,Len)
            +copy(FSource,MoveTo,Pos-MoveTo)
            +copy(FSource,Pos+Len,length(FSource)-Pos-Len+1);
  end else begin
    FSource:=copy(FSource,1,Pos-1)
            +copy(FSource,Pos+Len,MoveTo-Pos-Len)
            +copy(FSource,Pos,Len)
            +copy(FSource,MoveTo,length(FSource)-MoveTo+1);
  end;
  FSrcLen:=length(FSource);
  FLineCount:=-1;
  for i:=0 to FMarkers.Count-1 do begin
    if (Markers[i].Deleted=false) then
      NewSrcLogEntry.AdjustPosition(Markers[i].NewPosition);
  end;
  FModified:=true;
  DoSourceChanged;
end;

function TSourceLog.AddMarker(Position: integer; SomeData: Pointer
  ): TSourceLogMarker;
begin
  Result:=TSourceLogMarker.Create;
  Result.Position:=Position;
  Result.NewPosition:=Result.Position;
  Result.Data:=SomeData;
  Result.Deleted:=false;
  Result.Log:=Self;
  FMarkers.Add(Result);
end;

function TSourceLog.AddMarkerXY(Line, Column: integer; SomeData: Pointer
  ): TSourceLogMarker;
begin
  Result:=TSourceLogMarker.Create;
  LineColToPosition(Line,Column,Result.Position);
  Result.NewPosition:=Result.Position;
  Result.Data:=SomeData;
  Result.Deleted:=false;
  Result.Log:=Self;
  FMarkers.Add(Result);
end;

procedure TSourceLog.AdjustPosition(var APosition: integer);
var i: integer;
begin
  for i:=0 to Count-1 do
    Items[i].AdjustPosition(APosition);
end;

{$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
{$R-}
procedure TSourceLog.BuildLineRanges;
var
  line:integer;
  Cap: Integer;
  SrcEnd: PChar;
  SrcStart: PChar;
  p: PChar;
begin
  //DebugLn(['[TSourceLog.BuildLineRanges] A Self=',DbgS(Self),',LineCount=',FLineCount,' Len=',SourceLength]);
  if FLineCount>=0 then exit;
  // build line range list
  FLineCount:=0;
  if FSource='' then begin
    ReAllocMem(FLineRanges,0);
    exit;
  end;
  Cap:=FSrcLen div 20+100;
  ReAllocMem(FLineRanges,Cap*SizeOf(TLineRange));
  line:=0;
  FLineRanges[line].StartPos:=1;
  SrcStart:=PChar(FSource);
  SrcEnd:=SrcStart+FSrcLen;
  p:=SrcStart;
  repeat
    if (not (p^ in [#10,#13])) then begin
      if (p^=#0) and (p>=SrcEnd) then break;
      inc(p);
    end else begin
      // new line
      FLineRanges[line].EndPos:=p-SrcStart+1;
      inc(line);
      if line>=Cap then begin
        Cap:=Cap*2;
        ReAllocMem(FLineRanges,Cap*SizeOf(TLineRange));
      end;
      if (p[1] in [#10,#13]) and (p^<>p[1]) then
        inc(p,2)
      else
        inc(p);
      FLineRanges[line].StartPos:=p-SrcStart+1;
    end;
  until false;
  FLineRanges[line].EndPos:=fSrcLen+1;
  FLineCount:=line;
  if not (FSource[FSrcLen] in [#10,#13]) then
    inc(FLineCount);
  ReAllocMem(FLineRanges,FLineCount*SizeOf(TLineRange));
  //DebugLn('[TSourceLog.BuildLineRanges] END ',FLineCount);
end;
{$IFDEF RangeChecking}{$R+}{$ENDIF}

procedure TSourceLog.LineColToPosition(Line, Column: integer;
  out Position: integer);
begin
  BuildLineRanges;
  if (Line>=1) and (Line<=FLineCount) and (Column>=1) then begin
    if (Line<FLineCount) then begin
      // not the last line
      if (Column<=FLineRanges[Line-1].EndPos-FLineRanges[Line-1].StartPos+1)
      then begin
        Position:=FLineRanges[Line-1].StartPos+Column-1;
      end else begin
        Position:=FLineRanges[Line-1].EndPos;
      end;
    end else begin
      // last line
      if (Column<=fSrcLen-FLineRanges[Line-1].StartPos) then begin
        Position:=FLineRanges[Line-1].StartPos+Column-1;
      end else begin
        Position:=FLineRanges[Line-1].EndPos;
      end;
    end;
  end else begin
    Position:=-1;
  end;
end;

procedure TSourceLog.AbsoluteToLineCol(Position: integer;
  out Line, Column: integer);
var l,r,m:integer;
begin
  BuildLineRanges;
  if (FLineCount=0) or (Position<1) or (Position>fSrcLen+1) then begin
    Line:=-1;
    Column:=-1;
    exit;
  end;
  if (Position>=FLineRanges[FLineCount-1].StartPos) then begin
    Line:=FLineCount;
    Column:=Position-FLineRanges[Line-1].StartPos+1;
    exit;
  end;
  // binary search for the line
  l:=0;
  r:=FLineCount-1;
  repeat
    m:=(l+r) shr 1;
    if FLineRanges[m].StartPos>Position then begin
      // too high, search lower
      r:=m-1;
    end else if FLineRanges[m+1].StartPos<=Position then begin
      // too low, search higher
      l:=m+1;
    end else begin
      // line found
      Line:=m+1;
      Column:=Position-FLineRanges[Line-1].StartPos+1;
      exit;
    end;
  until false;
end;

function TSourceLog.LineColIsOutside(Line, Column: integer): boolean;
begin
  BuildLineRanges;
  Result:=true;
  if (Line<1) or (Column<1) then exit;
  if (Line>LineCount+1) then exit;
  if (Line<=fLineCount)
  and (Column>fLineRanges[Line-1].EndPos-fLineRanges[Line-1].StartPos+1) then
    exit;
  // check if on empty last line
  if (Line=FLineCount+1)
  and ((Column>1) or (FSource='') or (not (FSource[FSrcLen] in [#10,#13]))) then
    exit;
  Result:=false;
end;

function TSourceLog.LineColIsSpace(Line, Column: integer): boolean;
// check if there is a non space character in front of or at Line,Column
var
  p: PChar;
  rg: PLineRange;
begin
  BuildLineRanges;
  Result:=true;
  if (Line<1) or (Column<1) or (Line>LineCount) then exit;
  rg:=@fLineRanges[Line-1];
  if (Column>rg^.EndPos-rg^.StartPos+1) then
    exit;
  p:=@fSource[rg^.StartPos];
  if (p[Column-1]>' ') then exit(false);
  if (Column>1) and (p[Column-2]>' ') then exit(false);
end;

function TSourceLog.AbsoluteToLineColStr(Position: integer): string;
var
  Line: integer;
  Column: integer;
begin
  AbsoluteToLineCol(Position,Line,Column);
  Result:='p='+IntToStr(Position)+',line='+IntToStr(Line)+',col='+IntToStr(Column);
end;

function TSourceLog.LoadFromFile(const Filename: string): boolean;
var
  s: string;
  fs: TFileStreamUTF8;
  p: Integer;
begin
  Result := False;
  LastError:='';
  try
    fs := TFileStreamUTF8.Create(Filename, fmOpenRead or fmShareDenyNone);
    try
      SetLength(s, fs.Size);
      if s <> '' then
        fs.Read(s[1], length(s));
      FDiskEncoding := '';
      FMemEncoding := '';
      DecodeLoaded(Filename, s, FDiskEncoding, FMemEncoding);

      // get line ending
      FDiskLineEnding:=LineEnding;
      p:=1;
      while p<=length(s) do begin
        if s[p] in [#10,#13] then begin
          if s[p]=#10 then fDiskLineEnding:=#10
          else if (p<length(s)) and (s[p+1]=#10) then fDiskLineEnding:=#13#10
          else fDiskLineEnding:=#13;
          break;
        end;
        inc(p);
      end;

      Source := s;
    finally
      fs.Free;
    end;
    Result := True;
  except
    on E: Exception do
      LastError:=E.Message;
  end;
end;

procedure TSourceLog.IncreaseChangeStep;
begin
  if FChangeStep<High(FChangeStep) then
    inc(FChangeStep)
  else
    FChangeStep:=low(FChangeStep);
  //DebugLn('[TSourceLog.IncreaseChangeStep] ',FChangeStep,',',DbgS(Self));
end;

procedure TSourceLog.DoSourceChanged;
begin
  IncreaseChangeStep;
  //debugln(['TSourceLog.DoSourceChanged ']);
end;

function TSourceLog.SaveToFile(const Filename: string): boolean;
var 
  fs: TFileStreamUTF8;
  s: String;
begin
  {$IFDEF VerboseCTSave}
  DebugLn(['TSourceLog.SaveToFile Self=',DbgS(Self),' ',Filename,' Size=',length(Source)]);
  CTDumpStack;
  {$ENDIF}
  Result := False;
  LastError:='';
  try
    s := Source;
    // convert encoding
    EncodeSaving(Filename, s);
    // convert line ending to disk line ending
    if (DiskLineEnding<>'') and (LineEnding <> DiskLineEnding) then
      s := ChangeLineEndings(s, DiskLineEnding);

    // keep filename case on disk
    if FileExistsUTF8(Filename) then begin
      InvalidateFileStateCache(Filename);
      fs := TFileStreamUTF8.Create(Filename, fmOpenWrite or fmShareDenyNone);
      fs.Size := 0;
    end else begin
      InvalidateFileStateCache; // invalidate all (samba shares)
      fs := TFileStreamUTF8.Create(Filename, fmCreate);
    end;
    try
      if s <> '' then
        fs.Write(s[1], length(s));
    finally
      fs.Free;
    end;
    Result := True;
  except
    on E: Exception do
      LastError:=E.Message;
  end;
end;

function TSourceLog.GetLines(StartLine, EndLine: integer): string;
var
  StartPos: Integer;
  EndPos: Integer;
begin
  BuildLineRanges;
  if StartLine<1 then StartLine:=1;
  if EndLine>LineCount then EndLine:=LineCount;
  if StartLine<=EndLine then begin
    StartPos:=FLineRanges[StartLine-1].StartPos;
    if EndLine<LineCount then
      EndPos:=FLineRanges[EndLine].StartPos
    else
      EndPos:=FLineRanges[EndLine-1].EndPos;
    SetLength(Result,EndPos-StartPos);
    System.Move(FSource[StartPos],Result[1],length(Result));
  end else
    Result:='';
end;

function TSourceLog.IsEqual(sl: TStrings): boolean;
var
  p: PChar;
  Line: String;
  l: PChar;
  y: Integer;
begin
  Result:=false;
  if sl=nil then exit;
  if (FSrcLen=0) and (sl.Count>0) then exit;
  if (FLineCount>=0) and (sl.Count<>FLineCount) then exit;
  p:=PChar(FSource);
  y:=0;
  while (y<sl.Count) do begin
    Line:=sl[y];
    if (Line<>'') then begin
      l:=PChar(Line);
      while (l^=p^) do begin
        if (l^=#0) then begin
          if l-PChar(Line)=length(Line) then begin
            // end of Line
            if (p-PChar(FSource)=FSrcLen) then begin
              // end of source
              Result:=y=sl.Count-1;
              exit;
            end;
            break;
          end else if p-PChar(FSource)=FSrcLen then begin
            // not at end of Line, end of source
            exit;
          end;
        end;
        inc(p);
        inc(l);
      end;
      if l^<>#0 then exit;
    end;
    // at end of Line
    if not (p^ in [#10,#13]) then begin
      // not between two lines in Source
      Result:=(y=sl.Count-1) and (p-PChar(FSource)=FSrcLen);
      exit;
    end;
    // skip line end
    if (p[1] in [#10,#13]) and (p^<>p[1]) then
      inc(p,2)
    else
      inc(p);
    inc(y);
  end;
  Result:=p-PChar(FSource)=FSrcLen;
end;

function TSourceLog.OldIsEqual(sl: TStrings): boolean;
var x,y,p,LineLen: integer;
  Line: string;
begin
  Result:=false;
  if sl=nil then exit;
  p:=1;
  x:=1;
  y:=0;
  while (y<sl.Count) do begin
    Line:=sl[y];
    LineLen:=length(Line);
    if fSrcLen-p+1<LineLen then exit;
    x:=1;
    while (x<=LineLen) do begin
      if Line[x]<>fSource[p] then exit;
      inc(x);
      inc(p);
    end;
    if (p<=fSrcLen) and (not (fSource[p] in [#10,#13])) then exit;
    inc(p);
    if (p<=fSrcLen) and (fSource[p] in [#10,#13]) and (fSource[p]<>fSource[p-1])
    then inc(p);
    inc(y);
  end;
  if p<FSrcLen then exit;
  Result:=true;
end;

procedure TSourceLog.Assign(sl: TStrings);
begin
  if sl=nil then exit;
  if IsEqual(sl) then exit;
  IncreaseHookLock;
  try
    Clear;
    fSource := sl.Text;
    fSrcLen := Length(fSource);
    DoSourceChanged;
    NotifyHooks(nil);
  finally
    DecreaseHookLock;
  end;
end;

procedure TSourceLog.AssignTo(sl: TStrings; UseAddStrings: Boolean);
var y: integer;
  s: string;
  TempList: TStringList;
begin
  if sl=nil then exit;
  if IsEqual(sl) then exit;
  if UseAddStrings then begin
    TempList:=TStringList.Create;
    AssignTo(TempList,false);
    sl.BeginUpdate;
    sl.Clear;
    sl.AddStrings(TempList);
    sl.EndUpdate;
    TempList.Free;
  end else begin
    sl.BeginUpdate;
    sl.Clear;
    BuildLineRanges;
    sl.Capacity:=fLineCount;
    for y:=0 to fLineCount-1 do begin
      s:='';
      SetLength(s,fLineRanges[y].EndPos-fLineRanges[y].StartPos);
      if s<>'' then
        System.Move(fSource[fLineRanges[y].StartPos],s[1],length(s));
      sl.Add(s);
    end;
    sl.EndUpdate;
  end;
end;

procedure TSourceLog.LoadFromStream(aStream: TStream);
var
  NewSrcLen: integer;
  NewSource: String;
begin
  IncreaseHookLock;
  try
    if aStream=nil then exit;
    aStream.Position:=0;
    NewSrcLen:=aStream.Size-aStream.Position;
    NewSource:='';
    if NewSrcLen>0 then begin
      SetLength(NewSource,NewSrcLen);
      aStream.Read(NewSource[1],NewSrcLen);
    end;
    Source:=NewSource;
  finally
    DecreaseHookLock;
  end;
end;

procedure TSourceLog.SaveToStream(aStream: TStream);
begin
  if fSource<>'' then aStream.Write(fSource[1],fSrcLen);
end;

procedure TSourceLog.SetReadOnly(const Value: boolean);
begin
  FReadOnly := Value;
end;

procedure TSourceLog.IncWriteLock;
begin
  inc(FWriteLock);
end;

procedure TSourceLog.DecWriteLock;
begin
  if FWriteLock>0 then dec(FWriteLock);
end;

function TSourceLog.ConsistencyCheck: integer;
begin
  if fSrcLen<>length(fSource) then begin
    Result:=-1;  exit;
  end;
  Result:=0;
end;

function TSourceLog.CalcMemSize: PtrUInt;
begin
  Result:=PtrUInt(InstanceSize)
    +MemSizeString(FDiskEncoding)
    +MemSizeString(FDiskLineEnding)
    +PtrUint(FLineCount)*SizeOf(TLineRange)
    +MemSizeString(FMemEncoding)
    +PtrUInt(FChangeHookCount)*SizeOf(TOnSourceChange)
    +MemSizeString(FSource)
    +PtrUint(FLog.Count)*SizeOf(TSourceLogEntry)
    +PtrUInt(FMarkers.Count*TSourceLogMarker.InstanceSize);
end;

function TSourceLog.IndexOfChangeHook(AChangeHook: TOnSourceChange): integer;
begin
  Result:=FChangeHookCount-1;
  while (Result>=0) and (FChangeHooks[Result]<>AChangeHook) do dec(Result);
end;

procedure TSourceLog.DecodeLoaded(const AFilename: string;
  var ASource, ADiskEncoding, AMemEncoding: string);
begin
  if Assigned(OnDecodeLoaded) then
    OnDecodeLoaded(Self,AFilename,ASource,ADiskEncoding,AMemEncoding);
end;

procedure TSourceLog.EncodeSaving(const AFilename: string; var ASource: string);
begin
  if Assigned(OnEncodeSaving) then
    OnEncodeSaving(Self,AFilename,ASource);
end;

procedure TSourceLog.AddChangeHook(AnOnSourceChange: TOnSourceChange);
var i: integer;
begin
  i:=IndexOfChangeHook(AnOnSourceChange);
  if i>=0 then exit;
  inc(FChangeHookCount);
  if FChangeHooks=nil then
    GetMem(FChangeHooks, SizeOf(TOnSourceChange))
  else
    ReallocMem(FChangeHooks, SizeOf(TOnSourceChange) * FChangeHookCount);
  FChangeHooks[FChangeHookCount-1]:=AnOnSourceChange;
end;

procedure TSourceLog.RemoveChangeHook(AnOnSourceChange: TOnSourceChange);
var i,j: integer;
begin
  i:=IndexOfChangeHook(AnOnSourceChange);
  if i<0 then exit;
  dec(FChangeHookCount);
  if FChangeHookCount=1 then
    FreeMem(FChangeHooks)
  else begin
    for j:=i to FChangeHookCount-2 do
      FChangeHooks[j]:=FChangeHooks[j+1];
    ReAllocMem(FChangeHooks,SizeOf(TOnSourceChange) * FChangeHookCount);
  end;
end;

{ TSourceLogMarker }

destructor TSourceLogMarker.Destroy;
begin
  if Log<>nil then Log.FMarkers.Remove(Self);
  Log:=nil;
  inherited Destroy;
end;

end.

