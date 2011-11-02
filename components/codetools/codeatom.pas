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
    An Atom is the smallest unit for a parser. Usually a word or a symbol.
    An Atom is defined by the Start- and Endposition in the code (TAtomPosition)
    
    An TAtomRing is a ring of TAtomPosition
  
}
unit CodeAtom;

{$ifdef FPC}{$mode objfpc}{$endif}{$inline on}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, FileProcs, AVL_Tree, KeywordFuncLists;

type
  TCommonAtomFlag = (
    cafNone, // = none of the below
    cafSemicolon, cafEqual, cafColon, cafComma, cafPoint,
    cafRoundBracketOpen, cafRoundBracketClose,
    cafEdgedBracketOpen, cafEdgedBracketClose,
    cafWord, cafEnd
    );
    
const
  AllCommonAtomWords = [cafWord, cafEnd];
  CommonAtomFlagNames: array[TCommonAtomFlag] of shortstring = (
      'None',
      'Semicolon', 'Equal', 'Colon', 'Comma', 'Point',
      'RoundBracketOpen', 'RoundBracketClose',
      'EdgedBracketOpen', 'EdgedBracketClose',
      'Word', 'End'
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
    FSize: integer;
    FItems: {$ifdef FPC}^{$else}array of {$endif}TAtomPosition;
       // ring of TAtomPosition
    FStart, FLast: integer;
    procedure SetSize(NewSize: integer);
  public
    procedure Add(NewAtom: TAtomPosition); inline;
    procedure UndoLastAdd; inline;
    function GetValueAt(RelativePos:integer): TAtomPosition; inline;
          // 0=current last 1=prior current ...
          // for LastAtoms: 0 is the last atom
    function Count: integer; inline;
    property Size: integer read FSize write SetSize;
    procedure Clear;
    procedure WriteDebugReport;
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
function AtomPosition(StartPos, EndPos: integer): TAtomPosition;

function dbgs(const a: TAtomPosition): string; overload;


implementation


{ useful functions }

function AtomPosition(StartPos, EndPos: integer): TAtomPosition;
begin
  Result.StartPos:=StartPos;
  Result.EndPos:=EndPos;
  Result.Flag:=cafNone;
end;

function dbgs(const a: TAtomPosition): string;
begin
  Result:=CommonAtomFlagNames[a.Flag]+'['+dbgs(a.StartPos)+'-'+dbgs(a.EndPos)+']';
end;

{ TAtomRing }

constructor TAtomRing.Create;
begin
  inherited Create;
  FItems:=nil;
  Size:=5;
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

procedure TAtomRing.SetSize(NewSize: integer);
var i: integer;
begin
  Clear;
  if NewSize<2 then NewSize:=2;
  if NewSize>$FFFFFFF then NewSize:=$FFFFFFF;
  i:=0;
  while (i<30) and (NewSize>=(1 shl i)) do inc(i);
  NewSize:=(1 shl i)-1;
  if FSize=NewSize then exit;
  if FItems<>nil then FreeMem(FItems);
  FSize:=NewSize;
  GetMem(FItems,(FSize+1) * SizeOf(TAtomPosition));
end;

procedure TAtomRing.Add(NewAtom: TAtomPosition);
begin
  FItems[FStart]:=NewAtom;
  FStart:=(FStart+1) and FSize;
  if (FStart=FLast) then
    FLast:=(FLast+1) and FSize;
end;

procedure TAtomRing.UndoLastAdd;
begin
  if FStart=FLast then exit;
  FStart:=(FStart-1) and FSize;
end;

function TAtomRing.GetValueAt(RelativePos:integer):TAtomPosition;
// 0=current 1=prior current ...
begin
  if RelativePos<Count then
    Result:=FItems[(FStart-RelativePos-1) and FSize]
  else begin
    Result.StartPos:=1;
    Result.EndPos:=1;
    Result.Flag:=cafNone;
  end;
end;

procedure TAtomRing.Clear;
begin
  FStart:=0;
  FLast:=0;
end;

function TAtomRing.Count: integer;
begin
  Result:=FStart-FLast;
  if Result<0 then inc(Result,FSize);
end;

procedure TAtomRing.WriteDebugReport;
var i: integer;
  p: TAtomPosition;
begin
  DebugLn('[TAtomRing.WriteDebugReport] Size=',dbgs(FSize)
    ,' Start=',dbgs(FStart),' Last=',dbgs(FLast),' Count=',dbgs(Count));
  DbgOut('ValuesAt: ');
  for i:=0 to Count-1 do begin
    p:=GetValueAt(i);
    DbgOut(' '+dbgs(i)+'='+dbgs(p.StartPos)+'-'+dbgs(p.EndPos));
  end;
  DebugLn('');
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

