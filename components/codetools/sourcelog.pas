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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
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
  Classes, SysUtils, FileProcs;

type
  TSourceLog = class;

  TSourceLogEntryOperation = (sleoInsert, sleoDelete, sleoMove);
  TOnSourceLogInsert = procedure(Sender: TSourceLog; Pos: integer;
                        const Txt: string) of object;
  TOnSourceLogDelete = procedure(Sender: TSourceLog; Pos, Len: integer)
                        of object;
  TOnSourceLogMove = procedure(Sender: TSourceLog; Pos, Len, MoveTo: integer)
                      of object;
                      
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

  TSourceLogMarker = class
  private
  public
    Position: integer;
    NewPosition: integer;
    Deleted: boolean;
    Data: Pointer;
  end;

  TLineRange = record
    StartPos, EndPos: integer;
  end;

  TSourceLog = class
  private
    FLineCount: integer;
    FLineRanges: {$ifdef fpc}^{$else}array of {$endif}TLineRange;
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
    procedure IncreaseChangeStep;
    procedure SetReadOnly(const Value: boolean);
    function IndexOfChangeHook(AChangeHook: TOnSourceChange): integer;
  public
    Data: Pointer;
    function LineCount: integer;
    function GetLine(Index: integer): string;
    procedure GetLineRange(Index: integer; out LineRange: TLineRange);
    property Items[Index: integer]: TSourceLogEntry
       read GetItems write SetItems; default;
    function Count: integer; // # Items
    property SourceLength: integer read fSrcLen;
    procedure ClearEntries;
    property ChangeStep: integer read FChangeStep;
    property Markers[Index: integer]: TSourceLogMarker read GetMarkers;
    function MarkerCount: integer;
    procedure AddMarker(Position: integer; SomeData: Pointer);
    procedure AddMarkerXY(Line, Column: integer; SomeData: Pointer);
    procedure AdjustPosition(var APosition: integer);
    procedure NotifyHooks(Entry: TSourceLogEntry);
    procedure IncreaseHookLock;
    procedure DecreaseHookLock;
    property Source: string read FSource write SetSource;
    property Modified: boolean read FModified write FModified;
    // Line and Column begin at 1
    procedure LineColToPosition(Line, Column: integer; out Position: integer);
    procedure AbsoluteToLineCol(Position: integer; out Line, Column: integer);
    procedure Insert(Pos: integer; const Txt: string);
    procedure Delete(Pos, Len: integer);
    procedure Replace(Pos, Len: integer; const Txt: string);
    procedure Move(Pos, Len, MoveTo: integer);
    function LoadFromFile(const Filename: string): boolean; virtual;
    function SaveToFile(const Filename: string): boolean; virtual;
    function GetLines(StartLine, EndLine: integer): string;
    function IsEqual(sl: TStrings): boolean;
    procedure Assign(sl: TStrings);
    procedure AssignTo(sl: TStrings; UseAddStrings: Boolean);
    procedure LoadFromStream(s: TStream);
    procedure SaveToStream(s: TStream);
    property ReadOnly: boolean read FReadOnly write SetReadOnly;
    property WriteLock: integer read FWriteLock;
    procedure IncWriteLock;
    procedure DecWriteLock;
    procedure Clear;  virtual;
    function ConsistencyCheck: integer;
    constructor Create(const ASource: string);
    destructor Destroy; override;
    
    procedure AddChangeHook(AnOnSourceChange: TOnSourceChange);
    procedure RemoveChangeHook(AnOnSourceChange: TOnSourceChange);
    property OnInsert: TOnSourceLogInsert read FOnInsert write FOnInsert;
    property OnDelete: TOnSourceLogDelete read FOnDelete write FOnDelete;
    property OnMove: TOnSourceLogMove read FOnMove write FOnMove;
  end;


implementation


{ useful function }

function LineEndCount(const Txt: string;var LengthOfLastLine: integer): integer;
var i, LastLineEndPos, TxtLen: integer;
begin
  i:=1;
  LastLineEndPos:=0;
  Result:=0;
  TxtLen:=length(Txt);
  while i<TxtLen do begin
    if (Txt[i] in [#10,#13]) then begin
      inc(Result);
      inc(i);
      if (i<=TxtLen) and (Txt[i] in [#10,#13]) and (Txt[i-1]<>Txt[i]) then
        inc(i);
      LastLineEndPos:=i-1;
    end else
      inc(i);
  end;
  LengthOfLastLine:=TxtLen-LastLineEndPos;
end;


{ TSourceLogEntry }

constructor TSourceLogEntry.Create(APos, ALength, AMoveTo: integer;
  const ATxt: string; AnOperation: TSourceLogEntryOperation);
begin
  Position:=APos;
  Len:=ALength;
  MoveTo:=AMoveTo;
  Operation:=AnOperation;
  LineEnds:=LineEndCount(Txt, LengthOfLastLine);
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
begin
  Clear;
  if FChangeHooks<>nil then begin
    FreeMem(FChangeHooks);
    FChangeHooks:=nil;
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

function TSourceLog.GetLine(Index: integer): string;
var LineLen: integer;
begin
  BuildLineRanges;
  if (Index>=0) and (Index<fLineCount) then begin
    if Index<fLineCount-1 then
      LineLen:=fLineRanges[Index+1].StartPos-fLineRanges[Index].StartPos
    else
      LineLen:=fSrcLen-fLineRanges[Index].StartPos;
    SetLength(Result,LineLen);
    if LineLen>0 then
      System.Move(fSource[fLineRanges[Index].StartPos],Result[1],LineLen);
  end else
    Result:='';
end;

procedure TSourceLog.GetLineRange(Index: integer; out LineRange: TLineRange);
begin
  BuildLineRanges;
  LineRange:=FLineRanges[Index];
end;

procedure TSourceLog.ClearEntries;
var i: integer;
begin
  for i:=0 to Count-1 do Items[i].Free;
  FLog.Clear;
end;

procedure TSourceLog.Clear;
var i: integer;
begin
  ClearEntries;
  for i:=0 to MarkerCount-1 do Markers[i].Free;
  FMarkers.Clear;
  FSource:='';
  FSrcLen:=0;
  FModified:=false;
  if FLineRanges<>nil then begin
    FreeMem(FLineRanges);
    FLineRanges:=nil;
  end;
  FLineCount:=-1;
  IncreaseChangeStep;
  Data:=nil;
  FReadOnly:=false;
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
  if (FChangeHooks=nil) or (FChangeHookLock>0) then exit;
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
  if FChangeHookLock=0 then NotifyHooks(nil);
end;

procedure TSourceLog.SetSource(const NewSrc: string);
begin
  //DebugLn('TSourceLog.SetSource A ',length(NewSrc),' LineCount=',fLineCount,' SrcLen=',fSrcLen);
  if NewSrc<>FSource then begin
    Clear;
    FSource:=NewSrc;
    FSrcLen:=length(FSource);
    FLineCount:=-1;
    FReadOnly:=false;
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
  IncreaseChangeStep;
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
  IncreaseChangeStep;
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
  IncreaseChangeStep;
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
  IncreaseChangeStep;
end;

procedure TSourceLog.AddMarker(Position: integer; SomeData: Pointer);
var NewMarker: TSourceLogMarker;
begin
  NewMarker:=TSourceLogMarker.Create;
  NewMarker.Position:=Position;
  NewMarker.NewPosition:=NewMarker.Position;
  NewMarker.Data:=SomeData;
  NewMarker.Deleted:=false;
  FMarkers.Add(NewMarker);
end;

procedure TSourceLog.AddMarkerXY(Line, Column: integer; SomeData: Pointer);
var NewMarker: TSourceLogMarker;
begin
  NewMarker:=TSourceLogMarker.Create;
  LineColToPosition(Line,Column,NewMarker.Position);
  NewMarker.NewPosition:=NewMarker.Position;
  NewMarker.Data:=SomeData;
  NewMarker.Deleted:=false;
  FMarkers.Add(NewMarker);
end;

procedure TSourceLog.AdjustPosition(var APosition: integer);
var i: integer;
begin
  for i:=0 to Count-1 do
    Items[i].AdjustPosition(APosition);
end;

procedure TSourceLog.BuildLineRanges;
var p,line:integer;
begin
  {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
  {$R-}
  //DebugLn('[TSourceLog.BuildLineRanges] A Self=',DbgS(Self),',LineCount=',FLineCount,' Len=',SourceLength);
  if FLineCount>=0 then exit;
  if FLineRanges<>nil then begin
    FreeMem(FLineRanges);
    FLineRanges:=nil;
  end;
  // count line ends
  FLineCount:=0;
  p:=1;
  while (p<=fSrcLen) do begin
    if (not (FSource[p] in [#10,#13])) then begin
      inc(p);
    end else begin
      // new line
      inc(FLineCount);
      inc(p);
      if (p<=fSrcLen) and (FSource[p] in [#10,#13]) 
      and (FSource[p]<>FSource[p-1]) then
        inc(p);
    end;
  end;
  if (FSource<>'') and (not (FSource[fSrcLen] in [#10,#13])) then 
    inc(FLineCount);
  // build line range list
  if FLineCount>0 then begin
    GetMem(FLineRanges,FLineCount*SizeOf(TLineRange));
    p:=1;
    line:=0;
    FLineRanges[line].StartPos:=1;
    FLineRanges[FLineCount-1].EndPos:=fSrcLen+1;
    while (p<=fSrcLen) do begin
      if (not (FSource[p] in [#10,#13])) then begin
        inc(p);
      end else begin
        // new line
        FLineRanges[line].EndPos:=p;
        inc(line);
        inc(p);
        if (p<=fSrcLen) and (FSource[p] in [#10,#13]) 
        and (FSource[p]<>FSource[p-1]) then
          inc(p);
        if line<FLineCount then
          FLineRanges[line].StartPos:=p;
      end;
    end;
  end;
  //DebugLn('[TSourceLog.BuildLineRanges] END ',FLineCount);
  {$IFDEF RangeChecking}{$R+}{$ENDIF}
end;

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
  if (FLineCount=0) or (Position<1) or (Position>=fSrcLen) then begin
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

function TSourceLog.LoadFromFile(const Filename: string): boolean;
var s: string;
  fs: TFileStream;
begin
  Result:=true;
  try
    fs:=TFileStream.Create(Filename, fmOpenRead);
    try
      SetLength(s,fs.Size);
      if s<>'' then
        fs.Read(s[1],length(s));
      Source:=s;
    finally
      fs.Free;
    end;
  except
    Result:=false;
  end;
end;

procedure TSourceLog.IncreaseChangeStep;
begin
  if FChangeStep<>$7fffffff then
    inc(FChangeStep)
  else
    FChangeStep:=-$7fffffff;
  //DebugLn('[TSourceLog.IncreaseChangeStep] ',FChangeStep,',',DbgS(Self));
end;

function TSourceLog.SaveToFile(const Filename: string): boolean;
var 
  fs: TFileStream;
  TheFilename: String;
begin
  //DebugLn('TSourceLog.SaveToFile Self=',DbgS(Self));
  Result:=true;
  try
    InvalidateFileStateCache;
    // keep filename case on disk
    TheFilename:=FindDiskFilename(Filename);
    fs:=TFileStream.Create(TheFilename, fmCreate);
    try
      if fSrcLen>0 then
        fs.Write(FSource[1],length(FSource));
    finally
      fs.Free;
    end;
  except
    Result:=false;
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
  Clear;
  fSource := sl.Text;
  fSrcLen := Length(fSource);
  DecreaseHookLock;
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

procedure TSourceLog.LoadFromStream(s: TStream);
begin
  IncreaseHookLock;
  Clear;
  if s=nil then exit;
  s.Position:=0;
  fSrcLen:=s.Size-s.Position;
  if fSrcLen>0 then begin
    SetLength(fSource,fSrcLen);
    s.Read(fSource[1],fSrcLen);
  end;
  fLineCount:=-1;
  DecreaseHookLock;
end;

procedure TSourceLog.SaveToStream(s: TStream);
begin
  if fSource<>'' then s.Write(fSource[1],fSrcLen);
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

function TSourceLog.IndexOfChangeHook(AChangeHook: TOnSourceChange): integer;
begin
  Result:=FChangeHookCount-1;
  while (Result>=0) and (FChangeHooks[Result]<>AChangeHook) do dec(Result);
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


end.

