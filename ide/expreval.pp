{
  Author: Mattias Gaertner

  Abstract:
    Defines class TExpressionEvaluator
    Used by Code Tools for compiler directives. For example $IF expression.

    This class stores variables (case sensitive) of type string.
    Boolean values are '0' for false and true else (except empty '' which is
    invalid).
    The function Eval can evaluates expressions and knows the operators
      AND, OR, XOR, NOT, (, ), =, <, >, <=, >=, <>
}
unit ExprEval;

{$MODE OBJFPC}{$H+}

interface

uses Classes, SysUtils;

type
  TExpressionEvaluator = class
  private
    FVars: TStringlist;
    Expr: string;
    Pos, Max, AtomStart, AtomEnd, PriorAtomStart, ErrorPos: integer;
    function ReadTilEndBracket:boolean;
    function CompAtom(const UpperCaseTag:string): boolean;
    function ReadNextAtom:boolean;
    function EvalAtPos:string;
    function CompareValues(v1, v2: string): integer;
    function GetVariables(Name: string): string;
    procedure SetVariables(Name: string; const Value: string);
  public
    procedure Assign(SourceExpressionEvaluator: TExpressionEvaluator);
    property Variables[Name:string]:string
       read GetVariables write SetVariables;  default;
    procedure Undefine(Name:string);
    function IsDefined(Name:string):boolean;
    function ErrorPosition:integer;
    function Eval(Expression:string):string;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

var
  IsWordChar, IsNumberBeginChar, IsNumberChar:array[#0..#255] of boolean;

procedure InternalInit;
var c:char;
begin
  for c:=#0 to #255 do begin
    IsWordChar[c]:=(c in ['a'..'z','A'..'Z','_']);
    IsNumberBeginChar[c]:=(c in ['0'..'9','$','%']);
    IsNumberChar[c]:=(c in ['0'..'9','.','E','e']);
  end;
end;

{ TBooleanVariables }

procedure TExpressionEvaluator.Clear;
begin
  FVars.Clear;
end;

function TExpressionEvaluator.CompareValues(v1, v2: string): integer;
// -1 : v1<v2
//  0 : v1=v2
//  1 : v1>v2
var len1,len2,a:integer;
begin
  len1:=length(v1);
  len2:=length(v2);
  if len1<len2 then Result:=-1
  else if len1=len2 then begin
    for a:=1 to len1 do begin
      if v1[a]<v2[a] then begin
        Result:=-1;  exit;
      end;
      if v1[a]>v2[a] then begin
        Result:=1;  exit;
      end;
    end;
    Result:=0;
  end else Result:=1;
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
    if (upcase(Expr[AtomStart+a-1])<>UpperCaseTag[a]) then begin
      Result:=false;  exit;
    end;
  end;
  Result:=true;
end;

constructor TExpressionEvaluator.Create;
begin
  inherited Create;
  FVars:=TStringList.Create;
end;

destructor TExpressionEvaluator.Destroy;
begin
  FVars.Free;
  inherited Destroy;
end;

function TExpressionEvaluator.ErrorPosition: integer;
begin
  Result:=ErrorPos;
end;

function TExpressionEvaluator.Eval(Expression: string): string;
//  1 = true
//  0 = syntax error
// -1 = false
var s:string;
begin
  Expr:=Expression;
  Max:=length(expr);
  Pos:=1;
  AtomStart:=-1;  AtomEnd:=-1;  PriorAtomStart:=-1;
  ErrorPos:=-1;
  s:=EvalAtPos;
  if ErrorPos>=0 then begin
    // error
    Result:='';  exit;
  end;
  if (AtomStart>=0) and (AtomStart<=Max) and (Expr[AtomStart]=')') then begin
    // too many brackets closed -> error
    ErrorPos:=AtomStart;
    Result:='';  exit;
  end;
  Result:=s;
end;

function TExpressionEvaluator.EvalAtPos: string;
var r:string;   // current result
  c,o1,o2:char;
begin
  Result:='';
  repeat
    if (not ReadNextAtom) then exit;
    c:=Expr[AtomStart];
    if IsWordChar[c] then begin
      // identifier or keyword
      if (CompAtom('AND')) then begin
        if (Result='') then ErrorPos:=Pos
        else if (Result<>'0') then begin
          // true AND ...
          Result:=EvalAtPos;
          if ErrorPos>=0 then exit;
          if (Result='') then ErrorPos:=Pos;
        end;
        exit;
      end else if (CompAtom('OR')) then begin
        if (Result='0') then begin
          // false OR ...
          Result:=EvalAtPos;
          if ErrorPos>=0 then exit;
          if (Result='') then ErrorPos:=Pos;
        end else if (Result='') then ErrorPos:=Pos;
        exit;
      end else if (CompAtom('XOR')) then begin
        if (Result='') then begin
          ErrorPos:=Pos;  exit;
        end;
        r:=Result;
        // true/false XOR ...
        Result:=EvalAtPos;
        if ErrorPos>=0 then exit;
        if (Result='') then begin
          ErrorPos:=Pos;  exit;
        end;
        if (r='0') then begin
          if (Result='0') then Result:='0' else Result:='1';
        end else begin
          if (Result='0') then Result:='1' else Result:='0';
        end;
        exit;
      end else if (CompAtom('NOT')) then begin
        Result:=EvalAtPos;
        if ErrorPos>=0 then exit;
        if (Result='0') then Result:='1'
        else if (Result='') then ErrorPos:=Pos
        else Result:='0';
        exit;
      end else begin
        // Identifier
        if (Result<>'') then begin
          ErrorPos:=Pos;  exit;
        end else Result:=Variables[copy(Expr,AtomStart,AtomEnd-AtomStart)];
      end
    end else if IsNumberBeginChar[c] then begin
      // number
      if (Result<>'') then begin
        ErrorPos:=Pos;  exit;
      end else Result:=copy(Expr,AtomStart,AtomEnd-AtomStart);
    end else begin
      // operator
      case c of
      ')':exit;
      '(':begin
          // eval in brackets
          Result:=EvalAtPos;
          if ErrorPos>=0 then exit;
          // go behind brackets
          if (AtomStart<=Max) and (Expr[AtomStart]<>')') then begin
            if (not ReadTilEndBracket) then exit;
          end;
        end;
      '=','>','<':begin
          o1:=c;
          if AtomEnd=AtomStart+1 then begin
            r:=EvalAtPos;
            if ErrorPos>=0 then exit;
            case o1 of
            '=':if CompareValues(Result,r)=0 then Result:='1' else Result:='0';
            '>':if CompareValues(Result,r)=1 then Result:='1' else Result:='0';
            '<':if CompareValues(Result,r)=-1 then Result:='1' else Result:='0';
            end;
          end else begin
            o2:=Expr[AtomStart+1];
            r:=EvalAtPos;
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
      else
        begin
          ErrorPos:=Pos;
        end;
      end;
    end;
  until (ErrorPos>=0);
end;

function TExpressionEvaluator.GetVariables(Name: string): string;
begin
  Result:=FVars.Values[uppercase(Name)];
end;

function TExpressionEvaluator.IsDefined(Name: string): boolean;
var c:integer;
begin
  for c:=0 to FVars.Count-1 do begin
    if FVars.Names[c]=Name then begin
      Result:=true;  exit;
    end;
  end;
  Result:=false;
end;

function TExpressionEvaluator.ReadNextAtom: boolean;
var c,o1,o2:char;
begin
  PriorAtomStart:=AtomStart;
  while (Pos<=Max) do begin
    c:=Expr[Pos];
    if (c<=' ') then inc(Pos)
    else if IsWordChar[c] then begin
      // Identifier
      AtomStart:=Pos;
      repeat
        inc(Pos);
      until (Pos>Max) or (IsWordChar[Expr[Pos]]=false);
      AtomEnd:=Pos;
      Result:=true;
      exit;
    end else if IsNumberBeginChar[c] then begin
      // Number
      AtomStart:=Pos;
      repeat
        inc(Pos);
      until (Pos>Max) or (IsNumberChar[Expr[Pos]]=false);
      AtomEnd:=Pos;
      Result:=true;
      exit;
    end else begin
      // Symbol
      AtomStart:=Pos;
      inc(Pos);
      if (Pos<=Max) then begin
        o1:=c;
        o2:=Expr[Pos];
        if ((o2='=') and ( (o1='<') or (o1='>') ))
        or ((o1='<') and (o2='>'))
        then inc(Pos);
      end;
      AtomEnd:=Pos;
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
  while (Pos<Max) do begin
    if (Expr[Pos]='(') then inc(lvl);
    if (Expr[Pos]=')') then begin
      dec(lvl);
      if (lvl<0) then begin
        ErrorPos:=Pos;
        Result:=true;  exit;
      end;
    end;
    inc(pos);
  end;
  Result:=false;
end;

procedure TExpressionEvaluator.Assign(
  SourceExpressionEvaluator: TExpressionEvaluator);
begin
  FVars.Clear;
  FVars.AddStrings(SourceExpressionEvaluator.FVars);
end;

procedure TExpressionEvaluator.SetVariables(Name: string;
  const Value: string);
var OldCount:integer;
begin
  OldCount:=FVars.Count;
  FVars.Values[Name]:=Value;
  if FVars.Count=OldCount then FVars.Add(Name+'='+Value);
end;

procedure TExpressionEvaluator.Undefine(Name: string);
var c:integer;
begin
  for c:=0 to FVars.Count-1 do begin
    if FVars.Names[c]=Name then begin
      FVars.Delete(c);  exit;
    end;
  end;
end;

initialization
  InternalInit;

end.
