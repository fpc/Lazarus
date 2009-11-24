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
      defined()
      not defined V or undefined V
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
  TOnGetSameString = procedure(var s: string) of object;
  ArrayOfAnsiString = ^AnsiString;
  

  { TExpressionEvaluator }

  TExpressionEvaluator = class
  private
    FChangeStamp: integer;
    FErrorMsg: string;
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
    function IndexOfName(const VarName: string; InsertPos: boolean): integer;
    function IndexOfIdentifier(Identifier: PChar; InsertPos: boolean): integer;
    procedure Expand;
  public
    property Variables[const Name: string]: string
       read GetVariables write SetVariables;  default;
    property Count: integer read FCount;
    procedure Undefine(const Name: string);
    function IsDefined(const Name: string): boolean;
    function IsIdentifierDefined(Identifier: PChar): boolean;
    function Equals(AnExpressionEvaluator: TExpressionEvaluator): boolean; reintroduce;
    procedure Assign(SourceExpressionEvaluator: TExpressionEvaluator);
    procedure AssignTo(SL: TStringList);
    function Eval2(const Expression: string):string;
    function EvalPChar(Expression: PChar; ExprLen: PtrInt): string;
    function Eval(const Expression: string):string;
    property ErrorPosition: integer read ErrorPos write ErrorPos;
    property ErrorMsg: string read FErrorMsg write FErrorMsg;
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
    procedure RemoveDoubles(OnGetSameString: TOnGetSameString);
    procedure ConsistencyCheck;
    procedure WriteDebugReport;
    function CalcMemSize(WithNamesAndValues: boolean = true; Original: TExpressionEvaluator = nil): PtrUInt;
    property ChangeStamp: integer read FChangeStamp;
    procedure IncreaseChangeStamp; inline;
  end;

  { TExpressionSolver
    Checks if expression is always true 1, always false 0, or something }

  TExpressionSolver = class
  public
    //Defines: TStringToStringTree;
    //Undefines: TStringToStringTree;
    ErrorMsg: string; // last error message
    ErrorPos: integer;// last error position
    constructor Create;
    destructor Destroy; override;
    function Solve(const Expr: string; out ExprResult: string): boolean;
    function Solve(const Src: string; StartPos, EndPos: integer;
                  out ExprResult: string): boolean;
  end;


implementation

type
  TOperator = (
    opNone,
    opNot,
    opDefined,
    opUndefined,
    opDeclared,
    opLowerThan,
    opLowerOrEqual,
    opEqual,
    opNotEqual,
    opGreaterOrEqual,
    opGreaterThan,
    opAnd,
    opOr,
    opXor,
    opShl
    );
  TOperandValue = record
    Value: PChar;
    Len: PtrInt;
    Data: array[0..3] of char;
    Free: boolean;
  end;
const
  CleanOperandValue: TOperandValue = (Value:nil; Len:0; Data:#0#0#0#0; Free:false);

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

procedure FreeOperandValue(var V: TOperandValue);
begin
  if V.Free then begin
    FreeMem(V.Value);
    V.Value:=nil;
    V.Free:=false;
  end;
end;

procedure SetOperandValueChar(var V: TOperandValue; const c: Char);
begin
  if V.Free then FreeOperandValue(V);
  V.Data[0]:=c;
  V.Value:=@V.Data[0];
  V.Len:=1;
end;

procedure SetOperandValueConst(var V: TOperandValue; const p: PChar);
begin
  if V.Free then FreeOperandValue(V);
  V.Len:=strlen(p);
  V.Value:=p;
end;

function CompareOperand(const Operand: TOperandValue; Value: PChar): integer;
var
  p: PChar;
  l: PtrInt;
begin
  if (Operand.Value<>nil) and (Operand.Len>0) then begin
    if Value<>nil then begin
      p:=Operand.Value;
      l:=Operand.Len;
      while (p^=Value^) and (l>0) do begin
        if Value^=#0 then begin
          // 'aaa'#0'b' 'aaa'
          exit(0);
        end;
        inc(p);
        inc(Value);
        dec(l);
      end;
      if l>0 then begin
        if p^<Value^ then begin
          // 'aaa' 'aab'
          Result:=1;
        end else begin
          // 'aab' 'aaa' or 'aaa' 'aa'
          Result:=-1;
        end;
      end else begin
        if Value=#0 then begin
          // 'aaa' 'aaa'
          Result:=0;
        end else begin
          // 'aa' 'aaa'
          Result:=1;
        end;
      end;
    end else begin
      // 'aaa' nil
      Result:=-1;
    end;
  end else begin
    if Value<>nil then begin
      // nil 'aaa'
      Result:=1;
    end else begin
      // nil nil
      Result:=0;
    end;
  end;
end;

function GetIdentifierLen(Identifier: PChar): integer;
var
  p: PChar;
begin
  Result:=0;
  p:=Identifier;
  if p=nil then exit;
  if not IsIdentStartChar[p^] then exit;
  inc(p);
  while IsIdentChar[p^] do inc(p);
  Result:=p-Identifier;
end;

function CompareIdentifiers(Identifier1, Identifier2: PChar): integer;
begin
  while (UpChars[Identifier1[0]]=UpChars[Identifier2[0]]) do begin
    if (IsIdentChar[Identifier1[0]]) then begin
      inc(Identifier1);
      inc(Identifier2);
    end else begin
      Result:=0; // for example  'aaA;' 'aAa;'
      exit;
    end;
  end;
  if (IsIdentChar[Identifier1[0]]) then begin
    if (IsIdentChar[Identifier2[0]]) then begin
      if UpChars[Identifier1[0]]>UpChars[Identifier2[0]] then
        Result:=-1 // for example  'aab' 'aaa'
      else
        Result:=1; // for example  'aaa' 'aab'
    end else begin
      Result:=-1; // for example  'aaa' 'aa;'
    end;
  end else begin
    if (IsIdentChar[Identifier2[0]]) then
      Result:=1 // for example  'aa;' 'aaa'
    else
      Result:=0; // for example  'aa;' 'aa,'
  end;
end;

function CompareNames(Name1: PChar; Name1Len: PtrInt;
  Name2: PChar; Name2Len: PtrInt): integer;
begin
  while (Name1Len>0) and (Name2Len>0) do begin
    if UpChars[Name1^]=UpChars[Name2^] then begin
      inc(Name1);
      dec(Name1Len);
      inc(Name2);
      dec(Name2Len);
    end else begin
      if UpChars[Name1^]<UpChars[Name2^] then
        Result:=1
      else
        Result:=-1;
      exit;
    end;
  end;
  if Name1Len>Name2Len then
    Result:=-1
  else if Name1Len<Name2Len then
    Result:=1
  else
    Result:=0;
end;

function CompareNames(const Name1, Name2: string): integer; inline;
begin
  Result:=CompareNames(PChar(Name1),length(Name1),PChar(Name2),length(Name2));
end;


{ TBooleanVariables }

procedure TExpressionEvaluator.Clear;
var i: integer;
begin
  if FCount=0 then exit;
  for i:=0 to FCount-1 do begin
    FNames[i]:='';
    FValues[i]:='';
  end;
  FCount:=0;
  if FNames<>nil then begin
    FreeMem(FNames);
    FNames:=nil;
  end;
  if FValues<>nil then begin
    FreeMem(FValues);
    FValues:=nil;
  end;
  FCapacity:=0;
  IncreaseChangeStamp;
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

procedure TExpressionEvaluator.RemoveDoubles(OnGetSameString: TOnGetSameString);
var
  i: Integer;
begin
  for i:=0 to FCount-1 do begin
    OnGetSameString(FNames[i]);
    OnGetSameString(FValues[i]);
  end;
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
  HasBracket: Boolean;
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
        // Note: for Delphi compatibility: "IF not UndefinedVariable" is valid
        if (Result='0') then Result:='1'
        else Result:='0';
        exit;
      end else if (CompAtom('DEFINED')) then begin
        // read DEFINED(identifier) or defined identifier
        if (Result<>'') or (not ReadNextAtom) then begin
          ErrorPos:=CurPos;
          exit;
        end;
        HasBracket:=CompAtom('(');
        if HasBracket and (not ReadNextAtom) then begin
          ErrorPos:=CurPos;
          exit;
        end;
        if IsDefined(copy(Expr,AtomStart,AtomEnd-AtomStart)) then
          Result:='1'
        else
          Result:='0';
        if HasBracket then begin
          if (not ReadNextAtom) or (not CompAtom(')')) then begin
            ErrorPos:=CurPos;
            exit;
          end;
        end;
      end else if (CompAtom('DECLARED')) then begin
        // read DECLARED(identifier)
        if (Result<>'') or (not ReadNextAtom) or (CompAtom('(')=false)
        or (not ReadNextAtom) then begin
          ErrorPos:=CurPos;
          exit;
        end;
        if CompAtom('UNICODESTRING') then begin
          if IsDefined('FPC_HAS_UNICODESTRING') then
            Result:='1'
          else
            Result:='0';
        end else begin
          Result:='0';// this can only be answered by a real compiler
        end;
        if (not ReadNextAtom) or (not CompAtom(')')) then begin
          ErrorPos:=CurPos;
          exit;
        end;
      end else if (CompAtom('UNDEFINED')) then begin
        // read UNDEFINED(identifier) or undefined identifier
        if (Result<>'') or (not ReadNextAtom) then begin
          ErrorPos:=CurPos;
          exit;
        end;
        HasBracket:=CompAtom('(');
        if HasBracket and (not ReadNextAtom) then begin
          ErrorPos:=CurPos;
          exit;
        end;
        Result:=Variables[copy(Expr,AtomStart,AtomEnd-AtomStart)];
        if Result<>'' then
          Result:='0'
        else
          Result:='1';
        if HasBracket then begin
          if (not ReadNextAtom) or (not CompAtom(')')) then begin
            ErrorPos:=CurPos;
            exit;
          end;
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
var
  NewSize: integer;
begin
  FCapacity:=(FCapacity shl 1)+10;
  NewSize:=SizeOf(AnsiString)*FCapacity;
  ReAllocMem(FValues,NewSize);
  ReAllocMem(FNames,NewSize);
end;

function TExpressionEvaluator.IndexOfName(
  const VarName: string; InsertPos: boolean): integer;
var l,r,m, cmp: integer;
begin
  if FCount=0 then begin
    if InsertPos then
      Result:=0
    else
      Result:=-1;
    exit;
  end;
  l:=0;
  r:=FCount-1;
  m:=0;
  cmp:=0;
  while l<=r do begin
    m:=(l+r) shr 1;
    cmp:=CompareNames(VarName,FNames[m]);
    if cmp>0 then
      l:=m+1
    else if cmp<0 then
      r:=m-1
    else begin
      Result:=m;
      exit;
    end;
  end;
  if InsertPos then begin
    if cmp>0 then inc(m);
    Result:=m;
  end else begin
    Result:=-1;
  end;
end;

function TExpressionEvaluator.IndexOfIdentifier(Identifier: PChar;
  InsertPos: boolean): integer;
var l,r,m, cmp: integer;
  IdentLen: Integer;
  CurName: String;
begin
  if FCount=0 then begin
    if InsertPos then
      Result:=0
    else
      Result:=-1;
    exit;
  end;
  l:=0;
  r:=FCount-1;
  m:=0;
  cmp:=0;
  IdentLen:=GetIdentifierLen(Identifier);
  while l<=r do begin
    m:=(l+r) shr 1;
    CurName:=FNames[m];
    cmp:=CompareNames(Identifier,IdentLen,PChar(CurName),length(CurName));
    if cmp>0 then
      l:=m+1
    else if cmp<0 then
      r:=m-1
    else begin
      Result:=m;
      exit;
    end;
  end;
  if InsertPos then begin
    if cmp>0 then inc(m);
    Result:=m;
  end else begin
    Result:=-1;
  end;
end;

function TExpressionEvaluator.GetVariables(const Name: string): string;
var i: integer;
begin
  i:=IndexOfName(Name,false);
  if (i>=0) then
    Result:=FValues[i]
  else 
    Result:='';
end;

function TExpressionEvaluator.IsDefined(const Name: string): boolean;
begin
  Result:=IndexOfName(Name,false)>=0;
end;

function TExpressionEvaluator.IsIdentifierDefined(Identifier: PChar): boolean;
begin
  Result:=IndexOfIdentifier(Identifier,false)>=0;
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
    if Size>0 then begin
      GetMem(FNames,Size);
      FillByte(Pointer(FNames)^,Size,0);
      GetMem(FValues,Size);
      FillByte(Pointer(FValues)^,Size,0);
      FCapacity:=FCount;
      for i:=0 to FCount-1 do begin
        FNames[i]:=SourceExpressionEvaluator.FNames[i];
        FValues[i]:=SourceExpressionEvaluator.FValues[i];
      end;
    end;
    IncreaseChangeStamp;
  end;
  if Assigned(FOnChange) then FOnChange;
end;

procedure TExpressionEvaluator.SetVariables(const Name: string;
  const Value: string);
var i: integer;
  Size: Integer;
begin
  i:=IndexOfName(Name,true);
  if (i>=0) and (i<FCount) and (CompareNames(FNames[i],Name)=0) then begin
    // variable already exists -> replace value
    if FValues[i]<>Value then begin
      FValues[i]:=Value;
      IncreaseChangeStamp;
    end;
  end else begin
    // new variable
    if FCount=FCapacity then Expand;
    if i<0 then i:=0;
    if i<FCount then begin
      Size:=SizeOf(AnsiString)*(FCount-i);
      System.Move(PPointer(FNames)[i],PPointer(FNames)[i+1],Size);
      System.Move(PPointer(FValues)[i],PPointer(FValues)[i+1],Size);
    end;
    PPointer(FNames)[i]:=nil;
    PPointer(FValues)[i]:=nil;
    FNames[i]:=UpperCaseStr(Name);
    FValues[i]:=Value;
    inc(FCount);
    IncreaseChangeStamp;
  end;
end;

procedure TExpressionEvaluator.Undefine(const Name: string);
var i: integer;
  Size: Integer;
begin
  i:=IndexOfName(Name,false);
  if (i>=0) then begin
    FNames[i]:='';
    FValues[i]:='';
    dec(FCount);
    if FCount>i then begin
      Size:=SizeOf(AnsiString)*(FCount-i);
      System.Move(PPointer(FNames)[i+1],PPointer(FNames)[i],Size);
      System.Move(PPointer(FValues)[i+1],PPointer(FValues)[i],Size);
    end;
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

function TExpressionEvaluator.Eval2(const Expression: string): string;
begin
  if Expression='' then exit('0');
  Result:=EvalPChar(PChar(Expression),length(Expression));
end;

function TExpressionEvaluator.EvalPChar(Expression: PChar; ExprLen: PtrInt
  ): string;
{  1 = true
   0 = syntax error
  -1 = false

  brackets ()
  unary operators: not, defined, undefined
  binary operators: < <= = <> => > and or xor shl shr
  functions: defined(), undefined(), declared()
}
{type
  TOperandAndOperator = record
    Operand: TValue;
    theOperator: PChar;
    OperatorLvl: integer;
  end;
  TExprStack = array[0..4] of TOperandAndOperator;

var
  ExprStack: TExprStack;
  StackPtr: integer;}
var
  ExprEnd: Pointer;

  procedure Error(NewErrorPos: PChar; const NewErrorMsg: string);
  begin
    if NewErrorPos<>nil then
      ErrorPos:=NewErrorPos-Expression
    else
      ErrorPos:=0;
    ErrorMsg:=NewErrorMsg;
  end;

  procedure ExpressionMissing(NewErrorPos: PChar);
  begin
    Error(NewErrorPos,'expression missing');
  end;

  procedure IdentifierMissing(NewErrorPos: PChar);
  begin
    Error(NewErrorPos,'identifier missing');
  end;

  procedure CharMissing(NewErrorPos: PChar; c: char);
  begin
    Error(NewErrorPos,c+' missing');
  end;

  procedure StrExpectedAtPos(NewErrorPos, ExpectedStr: PChar);
  var
    s: string;
    f: string;
  begin
    s:=ExpectedStr;
    f:=NewErrorPos^;
    Error(NewErrorPos,'expected '+s+', but found '+f);
  end;

  function ParseDefinedParams(p: PChar; out Operand: TOperandValue): boolean;
  // p is behind defined or undefined keyword
  // Operand: '1' or '-1'
  var
    NeedBracketClose: Boolean;
    IdentStartPos: PChar;
  begin
    Result:=false;
    // skip space
    while IsSpaceChar[p^] do inc(p);
    if p>=ExprEnd then begin
      IdentifierMissing(p);
      exit;
    end;
    NeedBracketClose:=false;
    if p^='(' then begin
      // defined(
      NeedBracketClose:=true;
      // skip space
      while IsSpaceChar[p^] do inc(p);
      if p>=ExprEnd then begin
        IdentifierMissing(p);
        exit;
      end;
    end;
    if not IsIdentifierChar[p^] then begin
      StrExpectedAtPos(p,'macro name');
      exit;
    end;
    IdentStartPos:=p;
    while IsIdentChar[p^] do inc(p);
    if IsIdentifierDefined(IdentStartPos) then begin
      SetOperandValueChar(Operand,'1');
    end else begin
      SetOperandValueConst(Operand,'-1');
    end;
    if NeedBracketClose then begin
      // read bracket close
      // skip space
      while IsSpaceChar[p^] do inc(p);
      if p>=ExprEnd then begin
        CharMissing(ExprEnd,')');
        exit;
      end;
      if p^<>')' then begin
        StrExpectedAtPos(p,')');
        exit;
      end;
    end;
    Result:=true;
  end;

  function ReadOperand(var p: PChar; out Operand: TOperandValue): boolean;
  { Examples:
     Variable
     not Variable
     not not undefined Variable
     defined(Variable)
  }
  var
    IdentStartPos: PChar;
    i: LongInt;
  begin
    Result:=false;
    // skip space
    while IsSpaceChar[p^] do inc(p);
    if p>=ExprEnd then exit;
    case UpChars[p^] of
    'N':
      if CompareIdentifiers(p,'NOT')=0 then begin
        // not
        inc(p,3);
        if not ReadOperand(p,Operand) then exit;
        if (Operand.Len=1) and (Operand.Value^='1') then begin
          SetOperandValueConst(Operand,'-1');
        end else begin
          SetOperandValueChar(Operand,'1');
        end;
        exit(true);
      end;
    'D':
      if CompareIdentifiers(p,'DEFINED')=0 then begin
        // "defined V" or "defined(V)"
        inc(p,length('DEFINED'));
        if not ParseDefinedParams(p,Operand) then exit;
        exit(true);
      end;
    'U':
      if CompareIdentifiers(p,'UNDEFINED')=0 then begin
        // "undefined V" or "undefined(V)"
        inc(p,length('UNDEFINED'));
        if not ParseDefinedParams(p,Operand) then exit;
        if (Operand.Len=1) and (Operand.Value^='1') then begin
          SetOperandValueConst(Operand,'-1');
        end else begin
          SetOperandValueChar(Operand,'1');
        end;
        exit(true);
      end;
    end;
    if IsIdentStartChar[p^] then begin
      // identifier => return current value
      IdentStartPos:=p;
      while IsIdentChar[p^] do inc(p);
      i:=IndexOfIdentifier(IdentStartPos,false);
      if i>=0 then begin
        if Operand.Free then FreeOperandValue(Operand);
        Operand.Value:=PChar(FValues[i]);
        Operand.Len:=length(FValues[i]);
      end;
      exit(true);
    end;
    // invalid operand
    IdentifierMissing(p);
  end;

var
  p: PChar;
  Operand: TOperandValue;
begin
  p:=Expression;
  Result:='0';
  if p=nil then begin
    ExpressionMissing(p);
    exit;
  end;
  ExprEnd:=p+ExprLen;
  // skip space
  while IsSpaceChar[p^] do inc(p);
  if p>=ExprEnd then begin
    ExpressionMissing(p);
    exit;
  end;
  // read operand
  Operand:=CleanOperandValue;
  ReadOperand(p,Operand);

  FreeOperandValue(Operand);
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

procedure TExpressionEvaluator.ConsistencyCheck;
// 0 = ok
var i: integer;
begin
  if FCapacity<0 then
    RaiseCatchableException('');
  if FCapacity<FCount then
    RaiseCatchableException('');
  if FCount<0 then
    RaiseCatchableException('');
  if (FCapacity=0) and (FNames<>nil) then
    RaiseCatchableException('');
  if (FNames=nil) xor (FValues=nil) then
    RaiseCatchableException('');
  for i:=0 to FCount-1 do begin
    if not IsUpperCaseStr(FNames[i]) then
      RaiseCatchableException('');
    if (i>0) and (FNames[i-1]=FNames[i]) then
      RaiseCatchableException('');
    if (i>0) and (CompareNames(FNames[i-1],FNames[i])>0) then
      RaiseCatchableException('');
  end;
end;

procedure TExpressionEvaluator.WriteDebugReport;
begin
  DebugLn('[TExpressionEvaluator.WriteDebugReport] ');
  ConsistencyCheck;
end;

function TExpressionEvaluator.CalcMemSize(WithNamesAndValues: boolean;
  Original: TExpressionEvaluator): PtrUInt;
var
  i: Integer;
  j: LongInt;
begin
  Result:=PtrUInt(InstanceSize)
    +MemSizeString(Expr)
    +SizeOf(Pointer)*PtrUInt(FCount)*2;
  if WithNamesAndValues then begin
    for i:=0 to FCount-1 do begin
      if Original<>nil then begin
        j:=Original.IndexOfName(FNames[i],false);
        if j>=0 then begin
          if Pointer(FNames[i])=Pointer(Original.FNames[j]) then continue;
        end;
      end;
      inc(Result,MemSizeString(FNames[i]));
      inc(Result,MemSizeString(FValues[i]));
    end;
  end;
end;

procedure TExpressionEvaluator.IncreaseChangeStamp;
begin
  if FChangeStamp<High(Integer) then
    inc(FChangeStamp)
  else
    FChangeStamp:=Low(Integer);
end;


{ TExpressionSolver }

constructor TExpressionSolver.Create;
begin

end;

destructor TExpressionSolver.Destroy;
begin
  inherited Destroy;
end;

function TExpressionSolver.Solve(const Expr: string; out
  ExprResult: string): boolean;
begin
  Result:=Solve(Expr,1,length(Expr),ExprResult);
end;

function TExpressionSolver.Solve(const Src: string;
  StartPos, EndPos: integer; out ExprResult: string): boolean;
{ '' -> ''
  true = nonzero, false = zero
  defined(name)
  sizeof(type)
  unary operators: not, !
  binary operators: = <> >= <= > < and or xor shl shr
  round brackets ()
}
var
  AtomStart: LongInt;
  SrcPos: LongInt;

  function AtomIs(const s: shortstring): boolean;
  var
    len: Integer;
    i: Integer;
  begin
    len:=length(s);
    if (len<>SrcPos-AtomStart) then exit(false);
    if SrcPos>EndPos then exit(false);
    for i:=1 to len do
      if Src[AtomStart+i-1]<>s[i] then exit(false);
    Result:=true;
  end;

begin
  if StartPos>=EndPos then begin
    ExprResult:='';
    exit(true);
  end;
  SrcPos:=StartPos;
  AtomStart:=SrcPos;
  //ReadNextCAtom(Source,SrcPos,AtomStart);
  if AtomIs('!') then begin

  end;
end;

initialization
  InternalInit;

end.

