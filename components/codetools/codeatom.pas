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

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeCache;

type
  TCodePosition = record
    P: integer;
    Code: TCodeBuffer;
  end;

  TCodeXYPosition = record
    X, Y: integer;
    Code: TCodeBuffer;
  end;

  TAtomPosition = record
    StartPos: integer; // first char of Atom
    EndPos: integer;   // char behind Atom
  end;

  TAtomRing = class
  private
    FSize: integer;
    FItems: {$ifdef FPC}^{$else}array of {$endif}TAtomPosition;
       // ring of TAtomPosition
    FStart, FLast: integer;
    procedure SetSize(NewSize: integer);
  public
    procedure Add(NewAtom: TAtomPosition);
    procedure UndoLastAdd;
    function GetValueAt(
        RelativePos:integer):TAtomPosition;  // 0=current 1=prior current ...
    function Count: integer;
    property Size: integer read FSize write SetSize;
    procedure Clear;
    procedure WriteDebugReport;
    constructor Create;
    destructor Destroy; override;
  end;


//-----------------------------------------------------------------------------
// useful functions
function AtomPosition(StartPos, EndPos: integer): TAtomPosition;
function CodePosition(P: integer; Code: TCodeBuffer): TCodePosition;


implementation


{ useful functions }

function AtomPosition(StartPos, EndPos: integer): TAtomPosition;
begin
  Result.StartPos:=StartPos;
  Result.EndPos:=EndPos;
end;

function CodePosition(P: integer; Code: TCodeBuffer): TCodePosition;
begin
  Result.P:=P;
  Result.Code:=Code;
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
  writeln('[TAtomRing.WriteDebugReport] Size=',FSize
    ,' Start=',FStart,' Last=',FLast,' Count=',Count);
  write('ValuesAt: ');
  for i:=0 to Count-1 do begin
    p:=GetValueAt(i);
    write(' ',i,'=',p.StartPos,'-',p.EndPos);
  end;
  writeln('');
end;


end.

