{
/***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

  Author: Mattias Gaertner

  Abstract:
    Defines class TExpressionEvaluator
    Used by Code Tools for compiler directives. For example $IF expression.

    This class stores variables (case sensitive) of type string.
    Boolean values are '0' for false and true else (except empty '' which is
    invalid).
    The function Eval evaluates expressions and understands the operators
      AND, OR, XOR, NOT, (, ), =, <, >, <=, >=, <>
}
unit ExprEval;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

{$I codetools.inc}

interface

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, KeyWordFuncLists, FileProcs;

const
  ExternalMacroStart = '#';

type
  TOnValuesChanged = procedure of object;
  
  ArrayOfAnsiString = {$IFDEF FPC}^{$ELSE}array of {$ENDIF}AnsiString;
  

  TExpressionEvaluator = class
  private
    FNames, FValues: ArrayOfAnsiString; // always sorted in FNames and FNames uppercase
    FCount: integer;
    FCapacity: integer;
    Expr: string;
    CurPos, Max, AtomStart, AtomEnd, PriorAtomStart, ErrorPos: integer;
    FOnChange: TOnValuesChanged;
    function ReadTilEndBracket:boolean;
    function CompAtom(const UpperCaseTag:string): boolean;
    function ReadNextAtom:boolean;
    function EvalAtPos:string;
    function CompareValues(const v1, v2: string): integer;
    function GetVariables(const Name: string): string;
    procedure SetVariables(const Name: string; const Value: string);
    function IndexOfName(const VarName: string): integer;
    procedure Expand;
  public
    property Variables[const Name: string]: string
       read GetVariables write SetVariables;  default;
    property Count: integer read FCount;
    procedure Undefine(const Name: string);
    function IsDefined(const Name: string): boolean;
    function Equals(AnExpressionEvaluator: TExpressionEvaluator): boolean;
    procedure Assign(SourceExpressionEvaluator: TExpressionEvaluator);
    procedure AssignTo(SL: TStringList);
    function Eval(const Expression: string):string;
    property ErrorPosition:integer read ErrorPos;
    property OnChange: TOnValuesChanged read FOnChange write FOnChange;
    function Items(Index: integer): string;
    function Names(Index: integer): string;
    function Values(Index: integer): string;
    procedure Append(const Variable, Value: string);
    procedure Prepend(const Variable, Value: string);
    procedure Clear;
    function AsString: string;
    constructor Create;
    destructor Destroy; override;
    function ConsistencyCheck: integer; // 0 = ok
    procedure WriteDebugReport;
  end;

implementation

var
  IsWordChar, IsIdentifierChar, IsNumberBeginChar, IsNumberChar:
    array[#0..#255] of boolean;

procedure InternalInit;
var c:char;
begin
  for c:=#0 to #255 do begin
    IsWordChar[c]:=(c in ['a'..'z','A'..'Z','_']);
    IsNumberBeginChar[c]:=(c in ['0'..'9','$','%']);
    IsNumberChar[c]:=(c in ['0'..'9','.','E','e']);
    IsIdentifierChar[c]:=(c in ['a'..'z','A'..'Z','_','0'..'9']);
  end;
end;

{ TBooleanVariables }

procedure TExpressionEvaluator.Clear;
var i: integer;
begin
  for i:=0 to FCount-1 do begin
    FNames[i]:='';
    FValues[i]:='';
  end;
  if FNames<>nil then begin
    FreeMem(FNames);
    FNames:=nil;
  end;
  if FValues<>nil then begin
    FreeMem(FValues);
    FValues:=nil;
  end;
  FCount:=0;
  FCapacity:=0;
end;

function TExpressionEvaluator.CompareValues(const v1, v2: string): integer;
// -1 : v1<v2
//  0 : v1=v2
//  1 : v1>v2
var len1,len2,a:integer;
  c1: Char;
  c2: Char;
  ValPos1: Integer;
  ValPos2: Integer;
begin
  len1:=length(v1);
  len2:=length(v2);
  ValPos1:=1;
  ValPos2:=1;
  if (len1>1) and (v1[ValPos1]='''') then begin
    inc(ValPos1);
    dec(Len1,2);
  end;
  if (len2>1) and (v2[ValPos2]='''') then begin
    inc(ValPos2);
    dec(Len2,2);
  end;
  if len1<len2 then Result:=-1
  else if len1>len2 then Result:=1
  else begin
    for a:=1 to len1 do begin
      c1:=v1[ValPos1];
      c2:=v2[ValPos2];
      if c1<c2 then begin
        Result:=-1;  exit;
      end;
      if c1>c2 then begin
        Result:=1;  exit;
      end;
      inc(ValPos1);
      inc(ValPos2);
    end;
    Result:=0;
  end;
end;

function TExpressionEvaluator.CompAtom(
  const UpperCaseTag: string): boolean;
// compare uppercase tag with case insensitive atom
var a,len:integer;
begin
  if (AtomEnd>Max+1) then begin
    Result:=false;  exit;
  end;
  len:=AtomEnd-AtomStart;
  if length(UpperCaseTag)<>len then begin
    Result:=false;  exit;
  end;
  for a:=1 to len do begin
    if (UpChars[Expr[AtomStart+a-1]]<>UpperCaseTag[a]) then begin
      Result:=false;  exit;
    end;
  end;
  Result:=true;
end;

constructor TExpressionEvaluator.Create;
begin
  inherited Create;
  FValues:=nil;
  FNames:=nil;
  FCount:=0;
end;

destructor TExpressionEvaluator.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TExpressionEvaluator.Eval(const Expression: string): string;
//  1 = true
//  0 = syntax error
// -1 = false
var s:string;
begin
  Expr:=Expression;
  Max:=length(expr);
  CurPos:=1;
  AtomStart:=-1;  AtomEnd:=-1;  PriorAtomStart:=-1;
  ErrorPos:=-1;
  s:=EvalAtPos;
  if ErrorPos>=0 then begin
    // error
    Result:='';  exit;
  end;
  Result:=s;
end;

function TExpressionEvaluator.Items(Index: integer): string;
begin
  Result:=FNames[Index]+'='+FValues[Index];
end;

function TExpressionEvaluator.Names(Index: integer): string;
begin
  Result:=FNames[Index];
end;

function TExpressionEvaluator.Values(Index: integer): string;
begin
  Result:=FValues[Index];
end;

procedure TExpressionEvaluator.Append(const Variable, Value: string);
begin
  Variables[Variable]:=Variables[Variable]+Value;
end;

procedure TExpressionEvaluator.Prepend(const Variable, Value: string);
begin
  Variables[Variable]:=Value+Variables[Variable];
end;

function TExpressionEvaluator.EvalAtPos: string;
var r: string;   // current result
  c,o1,o2: char;
  OldPos: integer;
  AtomCount: Integer;
begin
  Result:='';
  AtomCount:=0;
  repeat
    if (not ReadNextAtom) then exit;
    inc(AtomCount);
    c:=Expr[AtomStart];
    if IsWordChar[c] then begin
      // identifier or keyword
      if (CompAtom('AND')) then begin
        if (Result='') then ErrorPos:=CurPos
        else if (Result<>'0') then begin
          // true AND ...
          Result:=EvalAtPos();
          if ErrorPos>=0 then exit;
          if (Result='') then ErrorPos:=CurPos;
        end;
        exit;
      end else if (CompAtom('OR')) then begin
        if (Result='0') then begin
          // false OR ...
          Result:=EvalAtPos();
          if ErrorPos>=0 then exit;
          if (Result='') then ErrorPos:=CurPos;
        end else if (AtomCount<=1) then ErrorPos:=CurPos;
        exit;
      end else if (CompAtom('XOR')) then begin
        if (Result='') then begin
          ErrorPos:=CurPos;  exit;
        end;
        r:=Result;
        // true/false XOR ...
        Result:=EvalAtPos();
        if ErrorPos>=0 then exit;
        if (Result='') then begin
          ErrorPos:=CurPos;  exit;
        end;
        if (r='0') then begin
          if (Result='0') then Result:='0' else Result:='1';
        end else begin
          if (Result='0') then Result:='1' else Result:='0';
        end;
        exit;
      end else if (CompAtom('NOT')) then begin
        Result:=EvalAtPos();
        if ErrorPos>=0 then exit;
        if (Result='0') then Result:='1'
        else if (Result='') then ErrorPos:=CurPos
        else Result:='0';
        exit;
      end else if (CompAtom('DEFINED')) then begin
        // read DEFINED(identifier)
        if (Result<>'') or (not ReadNextAtom) or (CompAtom('(')=false)
        or (not ReadNextAtom) then begin
          ErrorPos:=CurPos;
          exit;
        end;
        Result:=Variables[copy(Expr,AtomStart,AtomEnd-AtomStart)];
        if Result<>'' then
          Result:='1'
        else
          Result:='0';
        if (not ReadNextAtom) then begin
          ErrorPos:=CurPos;
          exit;
        end;
      end else if (CompAtom('DECLARED')) then begin
        // read DECLARED(identifier)
        if (Result<>'') or (not ReadNextAtom) or (CompAtom('(')=false)
        or (not ReadNextAtom) then begin
          ErrorPos:=CurPos;
          exit;
        end;
        Result:='0';// this can only be answered by a real compiler
        if (not ReadNextAtom) then begin
          ErrorPos:=CurPos;
          exit;
        end;
      end else begin
        // Identifier
        if (Result<>'') then begin
          ErrorPos:=CurPos;
          exit;
        end else
          Result:=Variables[copy(Expr,AtomStart,AtomEnd-AtomStart)];
      end;
    end else if IsNumberBeginChar[c] then begin
      // number
      if (Result<>'') then begin
        ErrorPos:=CurPos;  exit;
      end else Result:=copy(Expr,AtomStart,AtomEnd-AtomStart);
    end else if c='''' then begin
      Result:=copy(Expr,AtomStart+1,AtomEnd-AtomStart-2);
    end else begin
      // operator
      case c of
      ')':exit;
      '(':begin
          OldPos:=AtomStart;
          // eval in brackets
          Result:=EvalAtPos();
          if ErrorPos>=0 then exit;
          // go behind brackets
          CurPos:=OldPos;
          if (not ReadTilEndBracket) then exit;
          inc(CurPos);
        end;
      '=','>','<':begin
          o1:=c;
          if AtomEnd=AtomStart+1 then begin
            r:=EvalAtPos();
            if ErrorPos>=0 then exit;
            case o1 of
            '=':if CompareValues(Result,r)=0 then Result:='1' else Result:='0';
            '>':if CompareValues(Result,r)=1 then Result:='1' else Result:='0';
            '<':if CompareValues(Result,r)=-1 then Result:='1' else Result:='0';
            end;
          end else begin
            o2:=Expr[AtomStart+1];
            r:=EvalAtPos();
            if ErrorPos>=0 then exit;
            if o1='<' then begin
              if o2='>' then begin
                if CompareValues(Result,r)<>0 then Result:='1' else Result:='0';
              end else if o2='=' then begin
                if CompareValues(Result,r)<=0 then Result:='1' else Result:='0';
              end else ErrorPos:=AtomStart;
            end else if o1='>' then begin
              if o2='=' then begin
                if CompareValues(Result,r)>=0 then Result:='1' else Result:='0';
              end else ErrorPos:=AtomStart;
            end else ErrorPos:=AtomStart;
          end;
          exit;
        end;
      '!':
        begin
          Result:=EvalAtPos();
          if ErrorPos>=0 then exit;
          if (Result='0') then Result:='1'
          else if (Result='') then ErrorPos:=CurPos
          else Result:='0';
          exit;
        end;
      else
        begin
          ErrorPos:=CurPos;
        end;
      end;
    end;
  until (ErrorPos>=0);
end;

procedure TExpressionEvaluator.Expand;
var i, NewCapacity, NewSize: integer;
  NewValues, NewNames: ArrayOfAnsiString;
begin
  NewCapacity:=(FCapacity shl 1)+10;
  NewSize:=SizeOf(AnsiString)*NewCapacity;
  GetMem(NewValues,NewSize);
  GetMem(NewNames,NewSize);
  FillChar(Pointer(NewValues)^,NewSize,0);
  FillChar(Pointer(NewNames)^,NewSize,0);
  for i:=0 to FCount-1 do begin
    NewValues[i]:=FValues[i];
    FValues[i]:='';
    NewNames[i]:=FNames[i];
    FNames[i]:='';
  end;
  if FValues<>nil then FreeMem(FValues);
  if FNames<>nil then FreeMem(FNames);
  FValues:=NewValues;
  FNames:=NewNames;
  FCapacity:=NewCapacity;
end;

function TExpressionEvaluator.IndexOfName(
  const VarName: string): integer;
var l,r,m, cmp: integer;
begin
  if FCount=0 then begin
    Result:=0;
    exit;
  end;
  l:=0;
  r:=FCount-1;
  m:=0;
  while l<=r do begin
    m:=(l+r) shr 1;
    cmp:=CompareText(VarName,FNames[m]);
    if cmp>0 then
      l:=m+1
    else if cmp<0 then
      r:=m-1
    else
      break;
  end;
  if CompareText(VarName,FNames[m])>0 then inc(m);
  Result:=m;
end;

function TExpressionEvaluator.GetVariables(const Name: string): string;
var i: integer;
begin
  i:=IndexOfName(Name);
  if (i>=0) and (i<FCount) and (CompareText(FNames[i],Name)=0) then
    Result:=FValues[i]
  else 
    Result:='';
end;

function TExpressionEvaluator.IsDefined(const Name: string): boolean;
var i: integer;
begin
  i:=IndexOfName(Name);
  Result:=(i>=0) and (i<FCount) and (CompareText(FNames[i],Name)=0);
end;

function TExpressionEvaluator.ReadNextAtom: boolean;
var c,o1,o2:char;
begin
  PriorAtomStart:=AtomStart;
  while (CurPos<=Max) do begin
    c:=Expr[CurPos];
    if (c<=' ') then inc(CurPos)
    else if IsWordChar[c] then begin
      // Identifier
      AtomStart:=CurPos;
      repeat
        inc(CurPos);
      until (CurPos>Max) or (not IsIdentifierChar[Expr[CurPos]]);
      AtomEnd:=CurPos;
      Result:=true;
      exit;
    end else if IsNumberBeginChar[c] then begin
      // Number
      AtomStart:=CurPos;
      repeat
        inc(CurPos);
      until (CurPos>Max) or (IsNumberChar[Expr[CurPos]]=false);
      AtomEnd:=CurPos;
      Result:=true;
      exit;
    end else if c='''' then begin
      // string
      AtomStart:=CurPos;
      repeat
        inc(CurPos);
        if Expr[CurPos]='''' then begin
          inc(CurPos);
          AtomEnd:=CurPos;
          Result:=true;
          exit;
        end;
        if CurPos>Max then begin
          AtomEnd:=CurPos;
          Result:=false;
          exit;
        end;
      until (CurPos>Max);
    end else begin
      // Symbol
      AtomStart:=CurPos;
      inc(CurPos);
      if (CurPos<=Max) then begin
        o1:=c;
        o2:=Expr[CurPos];
        if ((o2='=') and ((o1='<') or (o1='>')))
        or ((o1='<') and (o2='>'))
        then inc(CurPos);
      end;
      AtomEnd:=CurPos;
      Result:=true;
      exit;
    end;
  end;
  Result:=false;
end;

function TExpressionEvaluator.ReadTilEndBracket: boolean;
// true = end bracket found
// false = not found
var lvl:integer;
begin
  lvl:=0;
  while (CurPos<=Max) do begin
    if (Expr[CurPos]='(') then
      inc(lvl)
    else if (Expr[CurPos]=')') then begin
      dec(lvl);
      if (lvl=0) then begin
        Result:=true;  exit;
      end else if (lvl<0) then begin
        ErrorPos:=CurPos;
        Result:=true;  exit;
      end;
    end;
    inc(CurPos);
  end;
  Result:=false;
end;

procedure TExpressionEvaluator.Assign(
  SourceExpressionEvaluator: TExpressionEvaluator);
var i, Size: integer;
begin
  Clear;
  if SourceExpressionEvaluator<>nil then begin
    FCount:=SourceExpressionEvaluator.Count;
    Size:=SizeOf(AnsiString) * FCount;
    GetMem(FNames,Size);
    FillChar(Pointer(FNames)^,Size,0);
    GetMem(FValues,Size);
    FillChar(Pointer(FValues)^,Size,0);
    FCapacity:=FCount;
    for i:=0 to FCount-1 do begin
      FNames[i]:=SourceExpressionEvaluator.FNames[i];
      FValues[i]:=SourceExpressionEvaluator.FValues[i];
    end;
  end;
  if Assigned(FOnChange) then FOnChange;
end;

procedure TExpressionEvaluator.SetVariables(const Name: string;
  const Value: string);
var i, j: integer;
begin
  i:=IndexOfName(Name);
  if (i>=0) and (i<FCount) and (CompareText(FNames[i],Name)=0) then
    // variable already exists -> replace value
    FValues[i]:=Value
  else begin
    // new variable
    if FCount=FCapacity then Expand;
    if i<0 then i:=0;
    for j:=FCount downto i+1 do begin
      FNames[j]:=FNames[j-1];
      FValues[j]:=FValues[j-1];
    end;
    FNames[i]:=UpperCaseStr(Name);
    FValues[i]:=Value;
    inc(FCount);
  end;
end;

procedure TExpressionEvaluator.Undefine(const Name: string);
var i, j: integer;
begin
  i:=IndexOfName(Name);
  if (i>=0) and (i<FCount) and (CompareText(FNames[i],Name)=0) then begin
    for j:=i to FCount-2 do begin
      FNames[j]:=FNames[j+1];
      FValues[j]:=FValues[j+1];
    end;
    dec(FCount);
    FNames[FCount]:='';
    FValues[FCount]:='';
  end;
end;

function TExpressionEvaluator.Equals(
  AnExpressionEvaluator: TExpressionEvaluator): boolean;
var i: integer;
begin
  if (AnExpressionEvaluator=nil) or (AnExpressionEvaluator.Count<>FCount) then
  begin
    Result:=false;
    exit;
  end;
  for i:=0 to FCount-1 do begin
    if (FNames[i]<>AnExpressionEvaluator.FNames[i])
    or (FValues[i]<>AnExpressionEvaluator.FValues[i]) then begin
      Result:=false;
      exit;
    end;
  end;
  Result:=true;
end;

procedure TExpressionEvaluator.AssignTo(SL: TStringList);
var i: integer;
begin
  if SL=nil then exit;
  SL.Clear;
  for i:=0 to FCount-1 do
    SL.Add(FNames[i]+'='+FValues[i]);
end;

function TExpressionEvaluator.AsString: string;
var TxtLen, i, p: integer;
begin
  TxtLen:=FCount*3;
  for i:=0 to FCount-1 do
    inc(TxtLen,length(FNames[i])+length(FValues[i]));
  Setlength(Result,TxtLen);
  p:=1;
  for i:=0 to FCount-1 do begin
    Move(FNames[i][1],Result[p],length(FNames[i]));
    inc(p,length(FNames[i]));
    Result[p]:=' ';
    inc(p);
    if length(FValues[i])>0 then begin
      Move(FValues[i][1],Result[p],length(FValues[i]));
      inc(p,length(FValues[i]));
    end;
    Result[p]:=#13;
    inc(p);
    Result[p]:=#10;
    inc(p);
  end;
end;

function TExpressionEvaluator.ConsistencyCheck: integer;
// 0 = ok
var i: integer;
begin
  if FCapacity<0 then begin
    Result:=-1;  exit;
  end;
  if FCapacity<FCount then begin
    Result:=-2;  exit;
  end;
  if FCount<0 then begin
    Result:=-3;  exit;
  end;
  if (FCapacity=0) and (FNames<>nil) then begin
    Result:=-4;  exit;
  end;
  if (FNames=nil) xor (FValues=nil) then begin
    Result:=-5;  exit;
  end;
  for i:=0 to FCount-1 do begin
    if not IsUpperCaseStr(FNames[i]) then begin
      Result:=-6;  exit;
    end;
    if (i>0) and (FNames[i-1]=FNames[i]) then begin
      Result:=-7;  exit;
    end;
    if (i>0) and (AnsiCompareStr(FNames[i-1],FNames[i])>0) then begin
      Result:=-8;  exit;
    end;
  end;
  for i:=FCount to FCapacity-1 do begin
    if (FNames[i]<>'') then begin
      Result:=-9;  exit;
    end;
    if (FValues[i]<>'') then begin
      Result:=-10;  exit;
    end;
  end;
  Result:=0;
end;

procedure TExpressionEvaluator.WriteDebugReport;
begin
  DebugLn('[TExpressionEvaluator.WriteDebugReport] Consistency=',
    dbgs(ConsistencyCheck));
end;

initialization
  InternalInit;

end.

