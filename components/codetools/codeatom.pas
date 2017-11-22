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
    An Atom is the smallest unit for a parser. Usually a word or a symbol.
    An Atom is defined by the Start- and Endposition in the code (TAtomPosition)
    
    An TAtomRing is a ring of TAtomPosition
  
}
unit CodeAtom;

{$ifdef FPC}{$mode objfpc}{$endif}{$inline on}{$H+}

interface

{$I codetools.inc}

{ $Define CheckAtomRing}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  SysUtils,
  // Codetools
  FileProcs, KeywordFuncLists;

type
  TCommonAtomFlag = (
    cafNone, // = none of the below
    cafSemicolon, cafEqual, cafColon, cafComma, cafPoint,
    cafRoundBracketOpen, cafRoundBracketClose,
    cafEdgedBracketOpen, cafEdgedBracketClose,
    cafAssignment,
    cafWord, cafEnd,
    cafOtherOperator // = other operator
    );
  TCommonAtomFlags = set of TCommonAtomFlag;
    
const
  AllCommonAtomWords = [cafWord, cafEnd];
  CommonAtomFlagNames: array[TCommonAtomFlag] of shortstring = (
      'None',
      'Semicolon', 'Equal', 'Colon', 'Comma', 'Point',
      'RoundBracketOpen', 'RoundBracketClose',
      'EdgedBracketOpen', 'EdgedBracketClose',
      'Assignment',
      'Word', 'End', 'Operator'
    );
    
type
  TAtomPosition = record
    StartPos: integer; // first char of Atom
    EndPos: integer;   // char behind Atom
    Flag: TCommonAtomFlag;
  end;
  PAtomPosition = ^TAtomPosition;
  
const
  StartAtomPosition: TAtomPosition = (StartPos:1; EndPos:1; Flag:cafNone);
  CleanAtomPosition: TAtomPosition = (StartPos:0; EndPos:0; Flag:cafNone);

type

  { TAtomRing }

  TAtomRing = class
  private
    FMask: integer;
    FSize: integer;
    FCur: integer;
    FFirst: integer;
    FLast: integer;
    FItems: {$ifdef FPC}^{$else}array of {$endif}TAtomPosition;
    FSrcLen: integer;
       // ring of TAtomPosition
    procedure SetSize(NewSize: integer);
    function IndexToRelativePos(Index: integer): integer;
    function RelativeToIndex(RelativePos: integer): integer; inline;
  public
    function Empty: boolean; inline;
    procedure Add(const NewAtom: TAtomPosition); inline;
    function GetCurrent(var Atom: TAtomPosition): boolean;
    function IsCurrent(const Atom: TAtomPosition): boolean;
    function SetCurrent(const Atom: TAtomPosition): boolean;
    function HasPrior: boolean; inline;
    function GoBack(var Atom: TAtomPosition): boolean;
    procedure UndoLastAdd; inline;
    procedure AddReverse(const NewAtom: TAtomPosition); inline; // used when reading backwards
    function GetPriorAtom: TAtomPosition; inline;
    function GetAtomAt(RelativePos:integer): TAtomPosition;
          // 0=current=last added
          // -1=prior current, added before current ...
          // 1=next first undo item, ...
    function GetValueAt(ReverseRelativePos:integer): TAtomPosition; inline; deprecated 'use GetAtomAt(-1-index)'; // Laz 1.9
    function IndexOf(StartPos: integer; out RelativePos: integer): boolean;
    procedure SetIndex(RelativePos: integer);
    function Count: integer; inline; deprecated 'use HasPrior or PriorCount instead'; // Laz 1.9
    function PriorCount: integer; inline;
    function NextCount: integer; inline;
    function HasNext: boolean; inline;
    function MoveToNext(var Atom: TAtomPosition): boolean;
    property Size: integer read FSize write SetSize; // rounded up to next power of 2
    procedure Clear;
    procedure ClearCurrent; // clear current and next, keep previous
    procedure WriteDebugReport;
    procedure ConsistencyCheck;
    property SrcLen: integer read FSrcLen write FSrcLen;
    constructor Create;
    destructor Destroy; override;
    function CalcMemSize: PtrUInt;
  end;
  
  TAtomList = class
  private
    FCapacity: integer;
    FCount: integer;
    FItems: {$ifdef FPC}^{$else}array of {$endif}TAtomPosition;
    function GetItems(Index: integer): TAtomPosition;
    procedure SetCapacity(const AValue: integer);
    procedure SetItems(Index: integer; const AValue: TAtomPosition);
    procedure Grow;
  public
    procedure Add(NewAtom: TAtomPosition);
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    property Capacity: integer read FCapacity write SetCapacity;
    property Count: integer read FCount;
    property Items[Index: integer]: TAtomPosition read GetItems write SetItems; default;
  end;
  
//-----------------------------------------------------------------------------
// useful functions
function AtomPosition(StartPos, EndPos: integer): TAtomPosition; overload;
function AtomPosition(StartPos, EndPos: integer; Flag: TCommonAtomFlag): TAtomPosition; overload;

function dbgs(const a: TAtomPosition): string; overload;


implementation


{ useful functions }

function AtomPosition(StartPos, EndPos: integer): TAtomPosition;
begin
  Result.StartPos:=StartPos;
  Result.EndPos:=EndPos;
  Result.Flag:=cafNone;
end;

function AtomPosition(StartPos, EndPos: integer; Flag: TCommonAtomFlag
  ): TAtomPosition;
begin
  Result.StartPos:=StartPos;
  Result.EndPos:=EndPos;
  Result.Flag:=Flag;
end;

function dbgs(const a: TAtomPosition): string;
begin
  Result:=CommonAtomFlagNames[a.Flag]+'['+dbgs(a.StartPos)+'-'+dbgs(a.EndPos)+']';
end;

{ TAtomRing }

procedure TAtomRing.SetSize(NewSize: integer);
var i: integer;
begin
  Clear;
  if NewSize<2 then NewSize:=2;
  if NewSize>$FFFFFFF then NewSize:=$FFFFFFF;
  i:=0;
  while (i<30) and (NewSize>(1 shl i)) do inc(i);
  NewSize:=(1 shl i);
  if FSize=NewSize then exit;
  FSize:=NewSize;
  FMask:=FSize-1;
  ReAllocMem(FItems,FSize * SizeOf(TAtomPosition));
  Clear;
end;

function TAtomRing.IndexToRelativePos(Index: integer): integer;
begin
  if FCur<0 then
    RaiseCatchableException('IndexToRelativePos');
  if FCur>=FFirst then begin
    if Index>=FFirst then begin
      Result:=Index-FCur;
    end else begin
      Result:=Index+FSize-FCur;
    end;
  end else begin
    if Index>=FFirst then begin
      Result:=Index-FSize-FCur;
    end else begin
      Result:=Index-FCur;
    end;
  end;
end;

function TAtomRing.RelativeToIndex(RelativePos: integer): integer;
begin
  Result:=(FCur+FSize+RelativePos) and FMask;
end;

function TAtomRing.Empty: boolean;
begin
  Result:=FCur>=0;
end;

constructor TAtomRing.Create;
begin
  inherited Create;
  FItems:=nil;
  Size:=16;
end;

destructor TAtomRing.Destroy;
begin
  if FItems<>nil then FreeMem(FItems);
  inherited Destroy;
end;

function TAtomRing.CalcMemSize: PtrUInt;
begin
  Result:=PtrUInt(InstanceSize)
       +PtrUInt(FSize)*SizeOf(TAtomPosition);
end;

procedure TAtomRing.Add(const NewAtom: TAtomPosition);
begin
  if FCur>=0 then begin
    if (FCur=FLast) then begin
      FCur:=(FCur+1) and FMask;
      FLast:=FCur;
      if FFirst=FLast then
        FFirst:=(FFirst+1) and FMask;
    end else
      FCur:=(FCur+1) and FMask;
  end else begin
    FCur:=0;
    FFirst:=0;
    FLast:=0;
  end;
  FItems[FCur]:=NewAtom;
  {$IFDEF CheckAtomRing}ConsistencyCheck;{$ENDIF}
end;

function TAtomRing.GetCurrent(var Atom: TAtomPosition): boolean;
begin
  if FCur>=0 then begin
    Atom:=FItems[FCur];
    Result:=true;
  end else
    Result:=false;
end;

function TAtomRing.IsCurrent(const Atom: TAtomPosition): boolean;
var
  p: PAtomPosition;
begin
  if FCur<0 then exit(false);
  p:=@FItems[FCur];
  Result:=(Atom.StartPos=p^.StartPos) and (Atom.EndPos=p^.EndPos)
    and (Atom.Flag=p^.Flag);
end;

function TAtomRing.SetCurrent(const Atom: TAtomPosition): boolean;
var
  Item: PAtomPosition;
begin
  if Atom.StartPos>=Atom.EndPos then begin
    Clear;
    exit(false);
  end;
  Result:=true;
  if FCur>=0 then begin
    if FCur<>FFirst then begin
      Item:=@FItems[(FCur+FSize-1) and FMask];
      if Item^.EndPos>Atom.StartPos then begin
        Clear;
        Add(Atom);
        exit;
      end;
    end;
    if FCur<>FLast then begin
      Item:=@FItems[(FCur+1) and FMask];
      if Item^.StartPos<Atom.EndPos then begin
        ClearCurrent;
        Add(Atom);
        exit;
      end;
    end;
    FItems[FCur]:=Atom;
  end else begin
    Add(Atom);
  end;
end;

function TAtomRing.HasPrior: boolean;
begin
  Result:=FCur<>FFirst;
end;

function TAtomRing.GoBack(var Atom: TAtomPosition): boolean;
begin
  if FCur<>FFirst then begin
    FCur:=(FCur+FSize-1) and FMask;
    Atom:=FItems[FCur];
    Result:=true;
    {$IFDEF CheckAtomRing}ConsistencyCheck;{$ENDIF}
  end else
    Result:=false;
end;

procedure TAtomRing.AddReverse(const NewAtom: TAtomPosition);
begin
  if FFirst>=0 then begin
    if (FCur=FFirst) then begin
      FCur:=(FCur+FSize-1) and FMask;
      FFirst:=FCur;
      if FFirst=FLast then
        FLast:=(FLast+FSize-1) and FMask;
    end else
      FCur:=(FCur+FSize-1) and FMask;
  end else begin
    FCur:=0;
    FFirst:=0;
    FLast:=0;
  end;
  FItems[FCur]:=NewAtom;
  {$IFDEF CheckAtomRing}ConsistencyCheck;{$ENDIF}
end;

procedure TAtomRing.UndoLastAdd;
begin
  if FCur<>FFirst then begin
    FCur:=(FCur+FSize-1) and FMask;
  end else begin
    Clear;
  end;
  {$IFDEF CheckAtomRing}ConsistencyCheck;{$ENDIF}
end;

function TAtomRing.GetPriorAtom: TAtomPosition;
begin
  if (FCur<>FFirst) then begin
    Result:=FItems[RelativeToIndex(-1)];
    exit;
  end;
  Result:=CleanAtomPosition;
end;

function TAtomRing.GetAtomAt(RelativePos: integer): TAtomPosition;
// 0=current -1=prior current ...
var
  i: Integer;
begin
  if (FCur>=0) then begin
    if RelativePos>=0 then begin
      i:=(FLast+FSize-FCur) and FMask;
      if RelativePos<=i then begin
        Result:=FItems[RelativeToIndex(RelativePos)];
        exit;
      end;
    end else begin
      i:=((FCur+FSize-FFirst) and FMask)+1;
      if -RelativePos<=i then begin
        Result:=FItems[RelativeToIndex(RelativePos)];
        exit;
      end;
    end;
  end;
  Result:=CleanAtomPosition;
end;

function TAtomRing.GetValueAt(ReverseRelativePos: integer): TAtomPosition;
begin
  Result:=GetAtomAt(-1-ReverseRelativePos);
end;

function TAtomRing.IndexOf(StartPos: integer; out RelativePos: integer): boolean;
var
  p, l, r, m: Integer;
begin
  //writeln('TAtomRing.IndexOf StartPos=',StartPos,' FCur=',FCur,' FFirst=',FFirst,' FLast=',FLast);
  if FCur<0 then exit(false);
  if FItems[FFirst].StartPos>StartPos then exit(false);
  if FItems[FLast].StartPos<StartPos then exit(false);

  if FFirst<=FLast then begin
    l:=FFirst;
    r:=FLast;
  end else begin
    l:=FFirst;
    r:=FLast+FSize;
  end;
  //writeln('TAtomRing.IndexOf l=',l,' r=',r);
  while l<=r do begin
    m:=(l+r) shr 1;
    p:=FItems[m and FMask].StartPos;
    if StartPos>p then
      l:=m+1
    else if StartPos<p then
      r:=m-1
    else begin
      // found
      RelativePos:=IndexToRelativePos(m);
      exit(true);
    end;
  end;
  Result:=false;
end;

procedure TAtomRing.SetIndex(RelativePos: integer);
begin
  FCur:=(FCur+FSize+RelativePos) and FMask;
  {$IFDEF CheckAtomRing}ConsistencyCheck;{$ENDIF}
end;

function TAtomRing.Count: integer;
begin
  Result:=PriorCount;
end;

procedure TAtomRing.Clear;
begin
  FFirst:=-1;
  FLast:=-1;
  FCur:=-1;
  {$IFDEF CheckAtomRing}ConsistencyCheck;{$ENDIF}
end;

procedure TAtomRing.ClearCurrent;
begin
  if FCur<0 then exit;
  if FCur=FFirst then
    Clear
  else begin
    FCur:=(FCur+FSize-1) and FMask;
    FLast:=FCur;
  end;
  {$IFDEF CheckAtomRing}ConsistencyCheck;{$ENDIF}
end;

function TAtomRing.PriorCount: integer;
begin
  if FCur>=0 then
    Result:=((FCur+FSize-FFirst) and FMask)+1
  else
    Result:=0;
end;

function TAtomRing.NextCount: integer;
begin
  if FCur>=0 then
    Result:=(FLast+FSize-FCur) and FMask
  else
    Result:=0;
end;

function TAtomRing.HasNext: boolean;
begin
  Result:=FCur<>FLast;
end;

function TAtomRing.MoveToNext(var Atom: TAtomPosition): boolean;
begin
  if FCur<>FLast then begin
    FCur:=(FCur+1) and FMask;
    Atom:=FItems[FCur];
    Result:=true;
    {$IFDEF CheckAtomRing}ConsistencyCheck;{$ENDIF}
  end else
    Result:=false;
end;

procedure TAtomRing.WriteDebugReport;
var i: integer;
  p: TAtomPosition;
begin
  DebugLn(['[TAtomRing.WriteDebugReport] Size=',FSize
    ,' Cur=',FCur,' First=',FFirst,' Last=',FLast,' PriorCount=',PriorCount,' NextCount=',NextCount]);
  if FCur>=0 then begin
    DbgOut('ValuesAt: ');
    for i:=1-PriorCount to NextCount do begin
      p:=GetAtomAt(i);
      DbgOut(' '+dbgs(i)+'='+dbgs(p.StartPos)+'-'+dbgs(p.EndPos));
    end;
    DebugLn('');
  end;
end;

procedure TAtomRing.ConsistencyCheck;

  procedure E(Msg: string);
  begin
    WriteDebugReport;
    Msg:='TAtomRing.ConsistencyCheck: '+Msg;
    debugln('ERROR: ',Msg);
    RaiseCatchableException(Msg);
  end;

var
  i, Next: Integer;
begin
  if FSize<2 then exit;
  if FSize<>FMask+1 then E('invalid FMask');
  if (FCur<-1) or (FCur>FMask) then E('invalid FCur');
  if (FFirst<-1) or (FFirst>FMask) then E('invalid FFirst');
  if (FLast<-1) or (FLast>FMask) then E('invalid FLast');
  if FCur<0 then begin
    if FFirst<>FCur then E('FFirst<>FCur');
    if FLast<>FCur then E('FLast<>FCur');
  end else begin
    if FFirst<0 then E('FFirst<0');
    if FLast<0 then E('FLast<0');
    if FFirst<=FLast then begin
      if FCur<FFirst then E('FCur<FFirst<=FLast');
      if FCur>FLast then E('FCur>FLast>=FFirst');
    end else begin
      if (FCur>FLast) and (FCur<FFirst) then E('FLast<FCur<FFirst');
    end;
    i:=FFirst;
    repeat
      if FItems[i].StartPos>=FItems[i].EndPos then begin
        if (i=FLast) and (FItems[i].StartPos=FItems[i].EndPos)
        and (FItems[i].StartPos>SrcLen) then
          // src end
        else if (i=FFirst) and (FItems[i].StartPos=FItems[i].EndPos)
        and (FItems[i].StartPos<1) then
          // src start
        else
          E('StartPos>=EndPos at '+IntToStr(i));
      end;
      if i=FLast then break;
      Next:=(i+1) and FMask;
      if FItems[i].EndPos>FItems[Next].StartPos then E('FItems['+IntToStr(i)+'].EndPos>FItems['+IntToStr(Next)+'].StartPos');
      i:=Next;
    until false;
  end;
end;

{ TAtomList }

function TAtomList.GetItems(Index: integer): TAtomPosition;
begin
  Result:=FItems[Index];
end;

procedure TAtomList.SetCapacity(const AValue: integer);
begin
  if FCapacity=AValue then exit;
  FCapacity:=AValue;
  if FItems<>nil then begin
    if FCapacity>0 then begin
      ReallocMem(FItems,SizeOf(TAtomPosition)*FCapacity);
    end else begin
      FreeMem(FItems);
      FItems:=nil;
    end;
  end else begin
    if FCapacity>0 then
      GetMem(FItems,SizeOf(TAtomPosition)*FCapacity);
  end;
end;

procedure TAtomList.SetItems(Index: integer; const AValue: TAtomPosition);
begin
  FItems[Index]:=AValue;
end;

procedure TAtomList.Grow;
begin
  Capacity:=Capacity*2+10;
end;

procedure TAtomList.Add(NewAtom: TAtomPosition);
begin
  if FCount=FCapacity then Grow;
  inc(FCount);
  Items[Count-1]:=NewAtom;
end;

procedure TAtomList.Clear;
begin
  FCount:=0;
  Capacity:=0;
end;

constructor TAtomList.Create;
begin
  inherited Create;
end;

destructor TAtomList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

end.

