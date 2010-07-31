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

}
unit CodeToolsCfgScript;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils, BasicCodeTools, AVL_Tree, KeywordFuncLists, FileProcs,
  typinfo;

type
  TCTCSValueType = (
    ctcsvNone,
    ctcsvString,
    ctcsvNumber
    );

  { TCTCfgScriptVariable }

  TCTCfgScriptVariable = record
    Name: PChar;
    ValueType: TCTCSValueType;
    case Integer of
    0: (StrStart: PChar; StrLen: integer);
    1: (Number: int64);
  end;
  PCTCfgScriptVariable = ^TCTCfgScriptVariable;


  { TCTCfgScriptVariables }

  TCTCfgScriptVariables = class
  private
    FItems: TAVLTree; // tree of PCTCfgScriptVariable sorted for name
    function GetValues(const Name: string): string;
    procedure SetValues(const Name: string; const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Equals(Vars: TCTCfgScriptVariables): boolean; reintroduce;
    procedure Assign(Source: TCTCfgScriptVariables);
    function GetVariable(const Name: PChar;
                      CreateIfNotExists: Boolean = false): PCTCfgScriptVariable;
    property Values[const Name: string]: string read GetValues write SetValues; default;
  end;

type
  TCTCfgScriptOperator = (
    ctcsoNone,
    ctcsoNot,
    ctcsoAnd,
    ctcsoOr,
    ctcsoXOr,
    ctcsoShL,
    ctcsoShR,
    ctcsoDiv,
    ctcsoMod,
    ctcsoPlus,
    ctcsoMinus,
    ctcsoMultiply,
    ctcsoDivide,
    ctcsoEqual,
    ctcsoNotEqual,
    ctcsoLowerThan,
    ctcsoLowerOrEqualThan,
    ctcsoGreaterThan,
    ctcsoGreaterOrEqualThan
    );
  TCTCfgScriptOperators = set of TCTCfgScriptOperator;
const
  CTCfgScriptOperatorLvl: array[TCTCfgScriptOperator] of integer = (
    0, //ctcsoNone,
    1, //ctcsoNot,
    1, //ctcsoAnd,
    2, //ctcsoOr,
    2, //ctcsoXOr,
    1, //ctcsoShL,
    1, //ctcsoShR,
    1, //ctcsoDiv,
    1, //ctcsoMod,
    2, //ctcsoPlus,
    2, //ctcsoMinus,
    1, //ctcsoMultiply,
    1, //ctcsoDivide,
    4, //ctcsoEqual,
    4, //ctcsoNotEqual,
    4, //ctcsoLowerThan,
    4, //ctcsoLowerOrEqualThan,
    4, //ctcsoGreaterThan,
    4  //ctcsoGreaterOrEqualThan
    );
type
  TCTCfgScriptStackItemType = (
    ctcssNone,
    ctcssStatement,
    ctcssBegin,
    ctcssIf,
    ctcssIfThen,
    ctcssIfElse,
    ctcssExpression,
    ctcssRoundBracketOpen,
    ctcssOperand,
    ctcssOperator,
    ctcssAssignment
    );
const
  ctcssAllStatementStarts = [ctcssNone,ctcssIfThen,ctcssIfElse,ctcssBegin];
type
  TCTCfgScriptStackItem = record
    Typ: TCTCfgScriptStackItemType;
    StartPos: PChar;
    Operand: TCTCfgScriptVariable;
  end;
  PCTCfgScriptStackItem = ^TCTCfgScriptStackItem;

type
  { TCTCfgScriptStack }

  TCTCfgScriptStack = class
  public
    Items: PCTCfgScriptStackItem;
    Top: integer; // current item, -1 = empty
    TopTyp: TCTCfgScriptStackItemType;
    Capacity: integer;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Push(Typ: TCTCfgScriptStackItemType; const StartPos: PChar);
    procedure Pop;
  end;

  { TCTCfgScriptError }

  TCTCfgScriptError = class
  public
    Msg: string;
    ErrorPos: PChar;
    constructor Create(const aMsg: string; anErrorPos: PChar);
  end;

  { TCTConfigScriptEngine }

  TCTConfigScriptEngine = class
  private
    FVariables: TCTCfgScriptVariables;
    FStack: TCTCfgScriptStack;
    FErrors: TFPList; // list of TCTCfgScriptError
    function GetErrors(Index: integer): TCTCfgScriptError;
    procedure AddError(const aMsg: string; ErrorPos: PChar); overload;
    procedure AddError(const aMsg: string); overload;
    procedure PushNumberConstant;
    procedure PushBooleanValue(b: boolean);
    procedure PushNumberValue(const Number: int64);
    function RunDefined(Negate: boolean): boolean;
    procedure PushStringConstant;
    procedure RunStatement(Skip: boolean);
    procedure RunBegin(Skip: boolean);
    procedure RunIf(Skip: boolean);
    procedure RunAssignment(Skip: boolean);
    function RunExpression(var Value: TCTCfgScriptVariable): boolean;
    function AtomIsKeyWord: boolean;
    function ExecuteStack(MaxLevel: integer): boolean;
    function GetOperatorLevel(P: PChar): integer;
  public
    Src: PChar;
    AtomStart: PChar;
    SrcStart, SrcEnd: PChar;
    MaxErrorCount: integer;
    constructor Create;
    destructor Destroy; override;
    procedure ClearErrors;
    property Variables: TCTCfgScriptVariables read FVariables;
    function Execute(const Source: string; StopAfterErrors: integer = 1): boolean;// true if no errors
    function ErrorCount: integer;
    property Errors[Index: integer]: TCTCfgScriptError read GetErrors;
    function GetAtom: string;
    function GetAtomOrNothing: string;
    function GetAtom(P: PChar): string;
    function PosToLineCol(p: PChar; out Line, Column: integer): boolean;
    function PosToStr(p: PChar): string;
    function GetErrorStr(Index: integer): string;

    procedure WriteDebugReportStack(Title: string);
  end;

function CompareCTCSVariables(Var1, Var2: Pointer): integer;
function ComparePCharWithCTCSVariableName(Name, aVar: Pointer): integer;
function AreCTCSVariablesEqual(const V1, V2: PCTCfgScriptVariable): Boolean;
function AreCTCSVariablesExactEqual(const V1, V2: PCTCfgScriptVariable): Boolean;
function NewCTCSVariable: PCTCfgScriptVariable;
function NewCTCSVariable(CloneName: PChar): PCTCfgScriptVariable;
function CloneCTCSVariable(const V: PCTCfgScriptVariable): PCTCfgScriptVariable;
procedure FreeCTCSVariable(var V: PCTCfgScriptVariable);
procedure ClearCTCSVariable(const V: PCTCfgScriptVariable);
procedure SetCTCSVariableAsString(const V: PCTCfgScriptVariable; const s: string);
procedure SetCTCSVariableAsNumber(const V: PCTCfgScriptVariable; const i: int64);
procedure SetCTCSVariableValue(const Src, Dest: PCTCfgScriptVariable);
function GetCTCSVariableAsString(const V: PCTCfgScriptVariable): string;
function CTCSNumberEqualsString(const Number: int64; const P: PChar): boolean; inline;
function CTCSStringToNumber(P: PChar; out Number: int64): boolean;
function CTCSVariableIsTrue(const V: PCTCfgScriptVariable): boolean; inline;
function CTCSVariableIsFalse(const V: PCTCfgScriptVariable): boolean;

function AtomToCTCfgOperator(p: PChar): TCTCfgScriptOperator;

function dbgs(const t: TCTCfgScriptStackItemType): string; overload;
function dbgs(const t: TCTCSValueType): string; overload;
function dbgs(const t: TCTCfgScriptOperator): string; overload;
function dbgs(const V: PCTCfgScriptVariable): string; overload;


implementation

function CompareCTCSVariables(Var1, Var2: Pointer): integer;
var
  v1: PCTCfgScriptVariable absolute Var1;
  v2: PCTCfgScriptVariable absolute Var2;
begin
  Result:=CompareIdentifiers(v1^.Name,v2^.Name);
end;

function ComparePCharWithCTCSVariableName(Name, aVar: Pointer): integer;
var
  n: PChar absolute Name;
  v: PCTCfgScriptVariable absolute aVar;
begin
  Result:=CompareIdentifiers(n,v^.Name);
end;

function AreCTCSVariablesEqual(const V1, V2: PCTCfgScriptVariable): Boolean;
begin
  Result:=false;
  case V1^.ValueType of
  ctcsvNone:
    if V2^.ValueType<>ctcsvNone then exit;
  ctcsvString:
    case V2^.ValueType of
    ctcsvNone: exit;
    ctcsvString:
      if (V1^.StrLen<>V2^.StrLen)
                 or ((V1^.StrStart<>nil)
                     and (not CompareMem(V1^.StrStart,V2^.StrStart,V1^.StrLen)))
               then exit;
    ctcsvNumber:
      if not CTCSNumberEqualsString(V2^.Number,V1^.StrStart) then exit;
    end;
  ctcsvNumber:
    case V2^.ValueType of
    ctcsvNone: exit;
    ctcsvString:
      if not CTCSNumberEqualsString(V1^.Number,V2^.StrStart) then exit;
    ctcsvNumber:
      if V1^.Number<>V2^.Number then exit;
    end;
  end;
  Result:=true;
end;

function AreCTCSVariablesExactEqual(const V1, V2: PCTCfgScriptVariable
  ): Boolean;
begin
  Result:=false;
  if V1^.ValueType<>V2^.ValueType then exit;
  case V1^.ValueType of
  ctcsvNone: ;
  ctcsvString: if (V1^.StrLen<>V2^.StrLen)
                 or ((V1^.StrStart<>nil)
                     and (not CompareMem(V1^.StrStart,V2^.StrStart,V1^.StrLen)))
               then exit;
  ctcsvNumber: if V1^.Number<>V2^.Number then exit;
  end;
  Result:=true;
end;

function NewCTCSVariable: PCTCfgScriptVariable;
begin
  New(Result);
  FillByte(Result^,SizeOf(Result),0);
end;

function NewCTCSVariable(CloneName: PChar): PCTCfgScriptVariable;
var
  l: LongInt;
begin
  Result:=NewCTCSVariable();
  l:=GetIdentLen(CloneName);
  if l>0 then begin
    Result^.Name:=GetMem(l+1);
    System.Move(CloneName^,Result^.Name^,l);
    Result^.Name[l]:=#0;
  end;
end;

function CloneCTCSVariable(const V: PCTCfgScriptVariable): PCTCfgScriptVariable;
var
  l: LongInt;
begin
  Result:=NewCTCSVariable(V^.Name);
  Result^.ValueType:=V^.ValueType;
  case V^.ValueType of
  ctcsvNone: ;
  ctcsvString:
    begin
      l:=V^.StrLen;
      Result^.StrLen:=l;
      if l>0 then begin
        Result^.StrStart:=GetMem(l+1);
        System.Move(V^.StrStart^,Result^.StrStart^,l);
        Result^.StrStart[l]:=#0;
      end;
    end;
  ctcsvNumber:
    Result^.Number:=V^.Number;
  end;
end;

procedure SetCTCSVariableValue(const Src, Dest: PCTCfgScriptVariable);
var
  l: LongInt;
begin
  if Src=Dest then exit;
  case Src^.ValueType of
  ctcsvNone:
    ClearCTCSVariable(Dest);
  ctcsvString:
    begin
      if Dest^.ValueType<>ctcsvString then begin
        Dest^.ValueType:=ctcsvString;
        Dest^.StrStart:=nil;
      end;
      l:=Src^.StrLen;
      Dest^.StrLen:=l;
      ReAllocMem(Dest^.StrStart,l);
      if l>0 then
        System.Move(Src^.StrStart^,Dest^.StrStart^,l);
    end;
  ctcsvNumber:
    begin
      case Dest^.ValueType of
      ctcsvNone:
        Dest^.ValueType:=ctcsvNumber;
      ctcsvString:
        begin
          Dest^.ValueType:=ctcsvNumber;
          if Dest^.StrStart<>nil then
            Freemem(Dest^.StrStart);
        end;
      ctcsvNumber: ;
      end;
      Dest^.Number:=Src^.Number;
    end;
  end;
end;

procedure FreeCTCSVariable(var V: PCTCfgScriptVariable);
begin
  ClearCTCSVariable(V);
  ReAllocMem(V^.Name,0);
  Dispose(V);
end;

procedure ClearCTCSVariable(const V: PCTCfgScriptVariable);
begin
  if V^.ValueType=ctcsvString then
    ReAllocMem(V^.StrStart,0);
  V^.ValueType:=ctcsvNone;
end;

function CTCSNumberEqualsString(const Number: int64; const P: PChar): boolean;
var
  n: int64;
begin
  Result:=CTCSStringToNumber(P,n) and (n=Number);
end;

function CTCSStringToNumber(P: PChar; out Number: int64): boolean;
var
  n: int64;
  Negated: Boolean;
begin
  Result:=false;
  if (P=nil) or (P^=#0) then exit;
  try
    n:=0;
    if p^='-' then begin
      Negated:=true;
      inc(p);
    end else
      Negated:=false;
    if p^='$' then begin
      // hex
      repeat
        case p^ of
        '0'..'9': n:=n*16+Ord(p^)-Ord('0');
        'a'..'f': n:=n*16+Ord(p^)-Ord('a')+10;
        'A'..'F': n:=n*16+Ord(p^)-Ord('A')+10;
        #0: break;
        else exit;
        end;
        inc(p);
      until false;
    end else if p^='%' then begin
      // binary
      repeat
        case p^ of
        '0': n:=n*2;
        '1': n:=n*2+1;
        #0: break;
        else exit;
        end;
        inc(p);
      until false;
    end else begin
      // decimal
      repeat
        case p^ of
        '0'..'9': n:=n*10+Ord(p^)-Ord('0');
        #0: break;
        else exit;
        end;
        inc(p);
      until false;
    end;
    if Negated then n:=-n;
  except
    exit;
  end;
  Number:=n;
  Result:=true;
end;

function CTCSVariableIsTrue(const V: PCTCfgScriptVariable): boolean;
begin
  Result:=not CTCSVariableIsFalse(V);
end;

function CTCSVariableIsFalse(const V: PCTCfgScriptVariable): boolean;
begin
  case V^.ValueType of
  ctcsvNone:
    Result:=false;
  ctcsvString:
    Result:=(V^.StrLen=1) and (V^.StrStart^='0');
  ctcsvNumber:
    Result:=V^.Number=0;
  end;
end;

function AtomToCTCfgOperator(p: PChar): TCTCfgScriptOperator;
begin
  Result:=ctcsoNone;
  case UpChars[p^] of
  'A':
    if CompareIdentifiers('and',p)=0 then Result:=ctcsoAnd;
  'D':
    if CompareIdentifiers('div',p)=0 then Result:=ctcsoDiv;
  'M':
    if CompareIdentifiers('mod',p)=0 then Result:=ctcsoMod;
  'N':
    if CompareIdentifiers('not',p)=0 then Result:=ctcsoNot;
  'O':
    if CompareIdentifiers('or',p)=0 then Result:=ctcsoOr;
  'S':
    case UpChars[p[1]] of
    'H':
      case UpChars[p[2]] of
      'L': if CompareIdentifiers('shl',p)=0 then Result:=ctcsoShL;
      'R': if CompareIdentifiers('shr',p)=0 then Result:=ctcsoShR;
      end;
    end;
  'X':
    if CompareIdentifiers('xor',p)=0 then Result:=ctcsoXOr;
  '=':
    Result:=ctcsoEqual;
  '<':
    case p[1] of
    '>': Result:=ctcsoNotEqual;
    '=': Result:=ctcsoLowerOrEqualThan;
    else { < lower than } Result:=ctcsoLowerThan;
    end;
  '>':
    case p[1] of
    '=': Result:=ctcsoGreaterOrEqualThan;
    else { > greater than } Result:=ctcsoGreaterThan;
    end;
  '*':
    case p[1] of
    '*': ;
    '=': ;
    else { * multiply } Result:=ctcsoMultiply;
    end;
  '/':
    case p[1] of
    '/': ;
    '=': ;
    else { / divide } Result:=ctcsoDivide;
    end;
  '+':
    case p[1] of
    '=': ;
    else { + plus } Result:=ctcsoPlus;
    end;
  '-':
    case p[1] of
    '=': ;
    else { - minus } Result:=ctcsoMinus;
    end;
  ':':
    case p[1] of
    '=': ;
    else { : colon } ;
    end;
  end;
end;

function dbgs(const t: TCTCfgScriptStackItemType): string;
begin
  Result:=GetEnumName(typeinfo(t),ord(t));
end;

function dbgs(const t: TCTCSValueType): string;
begin
  Result:=GetEnumName(typeinfo(t),ord(t));
end;

function dbgs(const t: TCTCfgScriptOperator): string;
begin
  Result:=GetEnumName(typeinfo(t),ord(t));
end;

function dbgs(const V: PCTCfgScriptVariable): string;
var
  l: Integer;
begin
  Result:=GetIdentifier(V^.Name)+':';
  case V^.ValueType of
  ctcsvNone:
    Result:=Result+'none';
  ctcsvString:
    begin
      Result:=Result+'string=';
      l:=length(Result);
      SetLength(Result,l+V^.StrLen);
      if V^.StrLen>0 then
        System.Move(V^.StrStart^,Result[l+1],V^.StrLen);
    end;
  ctcsvNumber:
    Result:=Result+'int64='+IntToStr(V^.Number);
  end;
end;

function GetCTCSVariableAsString(const V: PCTCfgScriptVariable): string;
begin
  case V^.ValueType of
  ctcsvNone: Result:='';
  ctcsvString:
    begin
      SetLength(Result,V^.StrLen);
      if Result<>'' then
        System.Move(V^.StrStart^,Result[1],length(Result));
    end;
  ctcsvNumber: Result:=IntToStr(V^.Number);
  else Result:='';
  end;
end;

procedure SetCTCSVariableAsString(const V: PCTCfgScriptVariable; const s: string
  );
var
  l: Integer;
begin
  if V^.ValueType<>ctcsvString then begin
    V^.ValueType:=ctcsvString;
    V^.StrLen:=0;
    V^.StrStart:=nil;
  end;
  l:=length(s);
  V^.StrLen:=l;
  ReAllocMem(V^.StrStart,l);
  if l>0 then
    System.Move(s[1],V^.StrStart^,l);
end;

procedure SetCTCSVariableAsNumber(const V: PCTCfgScriptVariable; const i: int64
  );
begin
  if (V^.ValueType=ctcsvString) and (V^.StrStart<>nil) then
    Freemem(V^.StrStart);
  V^.ValueType:=ctcsvNumber;
  V^.Number:=i;
end;

{ TCTCfgScriptVariables }

function TCTCfgScriptVariables.GetValues(const Name: string): string;
var
  v: PCTCfgScriptVariable;
begin
  if Name='' then
    exit('');
  v:=GetVariable(PChar(Name));
  if v=nil then
    exit('');
  Result:=GetCTCSVariableAsString(v);
end;

procedure TCTCfgScriptVariables.SetValues(const Name: string;
  const AValue: string);
var
  v: PCTCfgScriptVariable;
begin
  if Name='' then
    exit;
  v:=GetVariable(PChar(Name),true);
  SetCTCSVariableAsString(v,AValue);
end;

constructor TCTCfgScriptVariables.Create;
begin
  FItems:=TAVLTree.Create(@CompareCTCSVariables);
end;

destructor TCTCfgScriptVariables.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCTCfgScriptVariables.Clear;
var
  Node: TAVLTreeNode;
  Item: PCTCfgScriptVariable;
begin
  Node:=FItems.FindLowest;
  while Node<>nil do begin
    Item:=PCTCfgScriptVariable(Node.Data);
    FreeCTCSVariable(Item);
    Node:=FItems.FindSuccessor(Node);
  end;
  FItems.Clear;
end;

function TCTCfgScriptVariables.Equals(Vars: TCTCfgScriptVariables): boolean;
var
  Node1: TAVLTreeNode;
  Node2: TAVLTreeNode;
  Item1: PCTCfgScriptVariable;
  Item2: PCTCfgScriptVariable;
begin
  Result:=false;
  if Vars=nil then exit;
  if FItems.Count<>Vars.FItems.Count then exit;
  Node1:=FItems.FindLowest;
  Node2:=Vars.FItems.FindLowest;
  while Node1<>nil do begin
    Item1:=PCTCfgScriptVariable(Node1.Data);
    Item2:=PCTCfgScriptVariable(Node2.Data);
    if CompareIdentifiers(Item1^.Name,Item2^.Name)<>0 then exit;
    if Item1^.ValueType<>Item2^.ValueType then exit;
    case Item1^.ValueType of
    ctcsvNone: ;
    ctcsvString: if (Item1^.StrLen<>Item2^.StrLen)
                   or ((Item1^.StrStart<>nil)
                       and (not CompareMem(Item1^.StrStart,Item2^.StrStart,Item1^.StrLen)))
                 then exit;
    ctcsvNumber: if Item1^.Number<>Item2^.Number then exit;
    end;
    Node1:=FItems.FindSuccessor(Node1);
    Node2:=Vars.FItems.FindSuccessor(Node2);
  end;
  Result:=true;
end;

procedure TCTCfgScriptVariables.Assign(Source: TCTCfgScriptVariables);
var
  Node: TAVLTreeNode;
  Item: PCTCfgScriptVariable;
  NewItem: PCTCfgScriptVariable;
begin
  Clear;
  Node:=Source.FItems.FindLowest;
  while Node<>nil do begin
    Item:=PCTCfgScriptVariable(Node.Data);
    NewItem:=CloneCTCSVariable(Item);
    FItems.Add(NewItem);
    Node:=Source.FItems.FindSuccessor(Node);
  end;
end;

function TCTCfgScriptVariables.GetVariable(const Name: PChar;
  CreateIfNotExists: Boolean): PCTCfgScriptVariable;
var
  Node: TAVLTreeNode;
begin
  Node:=FItems.FindKey(Name,@ComparePCharWithCTCSVariableName);
  if Node<>nil then
    Result:=PCTCfgScriptVariable(Node.Data)
  else if CreateIfNotExists then begin
    Result:=NewCTCSVariable(Name);
    FItems.Add(Result);
  end else
    Result:=nil;
end;

{ TCTConfigScriptEngine }

function TCTConfigScriptEngine.GetErrors(Index: integer): TCTCfgScriptError;
begin
  Result:=TCTCfgScriptError(FErrors[Index]);
end;

procedure TCTConfigScriptEngine.AddError(const aMsg: string; ErrorPos: PChar);
var
  Err: TCTCfgScriptError;
begin
  Err:=TCTCfgScriptError.Create(aMsg,ErrorPos);
  FErrors.Add(Err);
  if ErrorCount>=MaxErrorCount then
    raise Exception.Create(GetErrorStr(ErrorCount-1));
end;

procedure TCTConfigScriptEngine.AddError(const aMsg: string);
begin
  AddError(aMsg,AtomStart);
end;

procedure TCTConfigScriptEngine.RunStatement(Skip: boolean);
{ Examples:
    begin..
    if...
    variable:=
}

  procedure ErrorUnexpectedAtom;
  begin
    AddError('expected statement, but found '+GetAtomOrNothing)
  end;

var
  IsKeyword: Boolean;
begin
  debugln(['TCTConfigScriptEngine.ParseStatement Atom=',GetAtom]);
  case AtomStart^ of
  #0: ;
  ';': ; // empty statement
  'a'..'z','A'..'Z':
    begin
      // identifier or keyword
      IsKeyword:=false;
      case UpChars[AtomStart^] of
      'B':
        if CompareIdentifiers('BEGIN',AtomStart)=0 then begin
          IsKeyword:=true;
          RunBegin(Skip);
        end;
      'I':
        if CompareIdentifiers('IF',AtomStart)=0 then begin
          IsKeyword:=true;
          RunIf(Skip);
        end;
      end;
      if (not IsKeyword) and AtomIsKeyWord then begin
        AddError('unexpected keyword '+GetAtom);
        exit;
      end;
      if not IsKeyword then begin
        // parse assignment
        RunAssignment(Skip);
      end;
    end;
  else
    ErrorUnexpectedAtom;
  end;
end;

procedure TCTConfigScriptEngine.RunBegin(Skip: boolean);
{ Examples:
    begin
    end
    begin
      statement statement
    end
}
var
  BeginStart: PChar;
  StartTop: LongInt;

  procedure ErrorMissingEnd;
  begin
    AddError('begin at '+PosToStr(BeginStart)+' without end');
  end;

begin
  BeginStart:=AtomStart;
  StartTop:=FStack.Top;
  FStack.Push(ctcssBegin,AtomStart);
  repeat
    ReadRawNextPascalAtom(Src,AtomStart);
    if (AtomStart=#0) then begin
      ErrorMissingEnd;
      break;
    end else if CompareIdentifiers('END',AtomStart)=0 then begin
      FStack.Pop;
      break;
    end;
    RunStatement(Skip);
  until false;
  // clean up stack
  while FStack.Top>StartTop do FStack.Pop;
end;

procedure TCTConfigScriptEngine.RunIf(Skip: boolean);
{ Examples:
    if expression then statement else statement
}
var
  IfStart: PChar;
  Value: TCTCfgScriptVariable;
  ExprIsTrue: Boolean;
  StartTop: LongInt;
begin
  IfStart:=AtomStart;
  StartTop:=FStack.Top;
  FStack.Push(ctcssIf,IfStart);
  ReadRawNextPascalAtom(Src,AtomStart);
  FillByte(Value,SizeOf(Value),0);
  ExprIsTrue:=RunExpression(Value) and CTCSVariableIsTrue(@Value);
  debugln(['TCTConfigScriptEngine.RunIf expression=',ExprIsTrue]);

  // read then
  if CompareIdentifiers(AtomStart,'then')<>0 then
    AddError('then expected, but '+GetAtomOrNothing+' found');
  // then statement
  ReadRawNextPascalAtom(Src,AtomStart);
  RunStatement(not ExprIsTrue);
  if CompareIdentifiers(AtomStart,'else')=0 then begin
    // else statement
    ReadRawNextPascalAtom(Src,AtomStart);
    RunStatement(ExprIsTrue);
  end;
  // clean up stack
  while FStack.Top>StartTop do FStack.Pop;
end;

procedure TCTConfigScriptEngine.RunAssignment(Skip: boolean);
{ Examples:
    a:=3;
}
var
  VarStart: PChar;
  OperatorStart: PChar;
  Value: TCTCfgScriptVariable;
  Variable: PCTCfgScriptVariable;
  StartTop: TCTCfgScriptStackItemType;
begin
  VarStart:=AtomStart;
  debugln(['TCTConfigScriptEngine.RunAssignment ',GetIdentifier(VarStart)]);
  StartTop:=FStack.TopTyp;
  FStack.Push(ctcssAssignment,VarStart);
  ReadRawNextPascalAtom(Src,AtomStart);
  debugln(['TCTConfigScriptEngine.RunAssignment Operator=',GetAtom]);
  OperatorStart:=AtomStart;
  // read :=
  if AtomStart^=#0 then begin
    AddError('missing :=');
    exit;
  end;
  if (not (AtomStart^ in [':','+','-','*','/'])) or (AtomStart[1]<>'=') then begin
    AddError('expected :=, but '+GetAtom+' found');
    exit;
  end;
  // read expression
  ReadRawNextPascalAtom(Src,AtomStart);
  FillByte(Value,SizeOf(Value),0);
  RunExpression(Value);
  if (not Skip) then begin
    Variable:=Variables.GetVariable(VarStart,true);
    debugln(['TCTConfigScriptEngine.RunAssignment ',GetIdentifier(VarStart),'=(Old=',dbgs(Variable),') := ',dbgs(PCTCfgScriptVariable(@Value))]);
    if OperatorStart=nil then ;
    SetCTCSVariableValue(@Value,Variable);
    debugln(['TCTConfigScriptEngine.RunAssignment ',GetIdentifier(VarStart),' = ',dbgs(Variable)]);
  end;
  // clean up stack
  while FStack.TopTyp>StartTop do FStack.Pop;
end;

procedure TCTConfigScriptEngine.PushNumberValue(const Number: int64);
var
  Item: PCTCfgScriptStackItem;
begin
  FStack.Push(ctcssOperand,AtomStart);
  Item:=@FStack.Items[FStack.Top];
  Item^.Operand.ValueType:=ctcsvNumber;
  Item^.Operand.Number:=Number;
  ExecuteStack(1);
end;

function TCTConfigScriptEngine.RunDefined(Negate: boolean): boolean;
var
  VarStart: PChar;
  b: Boolean;
begin
  Result:=false;
  ReadRawNextPascalAtom(Src,AtomStart);
  if AtomStart^<>'(' then begin
    AddError('expected (, but found '+GetAtomOrNothing);
    exit;
  end;
  ReadRawNextPascalAtom(Src,AtomStart);
  if (not IsIdentStartChar[AtomStart^]) or AtomIsKeyWord then begin
    AddError('expected identifier, but found '+GetAtomOrNothing);
    exit;
  end;
  VarStart:=AtomStart;
  ReadRawNextPascalAtom(Src,AtomStart);
  if AtomStart^<>')' then begin
    AddError('expected (, but found '+GetAtomOrNothing);
    exit;
  end;
  b:=Variables.GetVariable(VarStart)<>nil;
  if Negate then b:=not b;
  PushBooleanValue(b);
  Result:=true;
end;

procedure TCTConfigScriptEngine.PushStringConstant;
var
  Item: PCTCfgScriptStackItem;

  procedure Add(p: PChar; Count: integer);
  var
    OldLen: LongInt;
    NewLen: Integer;
  begin
    if Count=0 then exit;
    OldLen:=Item^.Operand.StrLen;
    NewLen:=OldLen+Count;
    ReAllocMem(Item^.Operand.StrStart,NewLen);
    System.Move(p^,Item^.Operand.StrStart[OldLen],Count);
    Item^.Operand.StrLen:=NewLen;
  end;

var
  p: PChar;
  StartPos: PChar;
  i: Integer;
  c: char;
begin
  FStack.Push(ctcssOperand,AtomStart);
  Item:=@FStack.Items[FStack.Top];
  Item^.Operand.ValueType:=ctcsvString;
  Item^.Operand.StrLen:=0;
  Item^.Operand.StrStart:=nil;
  p:=AtomStart;
  while true do begin
    case p^ of
    #0:
      break;
    '#':
      begin
        inc(p);
        StartPos:=p;
        i:=0;
        while (p^ in ['0'..'9']) do begin
          i:=i*10+ord(p^)-ord('0');
          if (i>255) then begin
            AddError('character constant out of range');
            while (p^ in ['0'..'9']) do inc(p);
            break;
          end;
          inc(p);
        end;
        c:=chr(i);
        Add(@c,1);
      end;
    '''':
      begin
        inc(p);
        StartPos:=p;
        while not (p^ in ['''',#0]) do
          inc(p);
        Add(StartPos,p-StartPos);
        if p^='''' then
          inc(p);
      end;
    else
      break;
    end;
  end;
  ExecuteStack(1);
end;

procedure TCTConfigScriptEngine.PushNumberConstant;
var
  Item: PCTCfgScriptStackItem;
  p: PChar;
  Number: int64;
  l: integer;
  c: Char;
begin
  FStack.Push(ctcssOperand,AtomStart);
  Item:=@FStack.Items[FStack.Top];
  p:=AtomStart;
  c:=p^;
  if not (c in ['0'..'9']) then inc(p);
  Number:=0;
  try
    while true do begin
      case c of
      '%':
        case p^ of
        '0': Number:=Number*2;
        '1': Number:=Number*2+1;
        else break;
        end;
      '&':
        case p^ of
        '0'..'7': Number:=Number*8+ord(p^)-ord('0');
        else break;
        end;
      '$':
        case p^ of
        '0'..'9': Number:=Number*16+ord(p^)-ord('0');
        'a'..'f': Number:=Number*16+ord(p^)-ord('a')+10;
        'A'..'F': Number:=Number*16+ord(p^)-ord('A')+10;
        else break;
        end;
      else
        case p^ of
        '0'..'9': Number:=Number*10+ord(p^)-ord('0');
        else break;
        end;
      end;
      inc(p);
    end;
  except
    p:=AtomStart;
  end;
  if p=Src then begin
    // a number
    Item^.Operand.ValueType:=ctcsvNumber;
    Item^.Operand.Number:=Number;
  end else begin
    // string constant
    Item^.Operand.ValueType:=ctcsvString;
    l:=Src-AtomStart;
    Item^.Operand.StrLen:=l;
    Item^.Operand.StrStart:=GetMem(l);
    System.Move(AtomStart^,Item^.Operand.StrStart^,l);
  end;
  ExecuteStack(1);
end;

procedure TCTConfigScriptEngine.PushBooleanValue(b: boolean);
var
  Item: PCTCfgScriptStackItem;
begin
  FStack.Push(ctcssOperand,AtomStart);
  Item:=@FStack.Items[FStack.Top];
  Item^.Operand.ValueType:=ctcsvNumber;
  if b then
    Item^.Operand.Number:=1
  else
    Item^.Operand.Number:=0;
  ExecuteStack(1);
end;

function TCTConfigScriptEngine.RunExpression(var Value: TCTCfgScriptVariable
  ): boolean;
{ Examples:
    A   is false if A=0 or A='0'
    defined(A)
    (A)
    unary operators:

    binary operators:

}
  procedure ErrorUnexpectedRoundBracketClose;
  begin
    AddError('expression expected, but ) found');
  end;

  function OperandAllowed: boolean;
  begin
    case FStack.TopTyp of
    ctcssExpression,ctcssOperator,ctcssRoundBracketOpen:
      Result:=true;
    else
      debugln(['TCTConfigScriptEngine.RunExpression.OperandAllowed no']);
      AddError('operator expected but '+GetAtom+' found');
      Result:=false;
    end;
  end;

  function BinaryOperatorAllowed: boolean;
  begin
    case FStack.TopTyp of
    ctcssOperand:
      Result:=true;
    else
      debugln(['TCTConfigScriptEngine.RunExpression.BinaryOperatorAllowed no']);
      AddError('operand expected but '+GetAtom+' found');
      Result:=false;
    end;
  end;

  function PushBinaryOperator: boolean;
  begin
    Result:=BinaryOperatorAllowed;
    if not Result then exit;
    ExecuteStack(GetOperatorLevel(AtomStart));
    FStack.Push(ctcssOperator,AtomStart);
  end;

var
  ExprStart: PChar;
  IsKeyword: Boolean;
  Item: PCTCfgScriptStackItem;
  StartTop: LongInt;
  v: PCTCfgScriptVariable;
begin
  Result:=true;
  ExprStart:=AtomStart;
  StartTop:=FStack.Top;
  FStack.Push(ctcssExpression,ExprStart);
  while true do begin
    debugln(['TCTConfigScriptEngine.RunExpression Atom=',GetAtom]);
    case AtomStart^ of
    #0:
      break;
    '(':
      begin
        if not OperandAllowed then break;
        FStack.Push(ctcssRoundBracketOpen,AtomStart);
      end;
    ')':
      begin
        if FStack.TopTyp=ctcssRoundBracketOpen then begin
          ExecuteStack(5);
          FStack.Pop;
        end
        else
          ErrorUnexpectedRoundBracketClose;
      end;
    'a'..'z','A'..'Z':
      begin
        // a keyword or an identifier

        debugln(['TCTConfigScriptEngine.RunExpression StackTop=',dbgs(FStack.TopTyp),' Atom=',GetAtom]);
        // execute
        IsKeyword:=false;
        case UpChars[AtomStart^] of
        'A':
          if CompareIdentifiers('and',AtomStart)=0 then begin
            IsKeyword:=true;
            if not PushBinaryOperator then break;
          end;
        'D':
          case UpChars[AtomStart[1]] of
          'E':
            if CompareIdentifiers('defined',AtomStart)=0 then begin
              IsKeyword:=true;
              if not OperandAllowed then break;
              if not RunDefined(false) then break;
            end;
          'I':
            if CompareIdentifiers('div',AtomStart)=0 then begin
              IsKeyword:=true;
              if not PushBinaryOperator then break;
            end;
          end;
        'E':
          case UpChars[AtomStart[1]] of
          'L':
            if CompareIdentifiers('else',AtomStart)=0 then
              break;
          'N':
            if CompareIdentifiers('end',AtomStart)=0 then
              break;
          end;
        'F':
          if CompareIdentifiers('false',AtomStart)=0 then begin
            IsKeyword:=true;
            if not OperandAllowed then break;
            PushBooleanValue(false);
          end;
        'M':
          if CompareIdentifiers('mod',AtomStart)=0 then begin
            IsKeyword:=true;
            if not PushBinaryOperator then break;
          end;
        'N':
          if CompareIdentifiers('not',AtomStart)=0 then begin
            IsKeyword:=true;
            if not OperandAllowed then break;
            // Note: no execute, "not" is unary operator for the next operand
            FStack.Push(ctcssOperator,AtomStart);
          end;
        'O':
          if CompareIdentifiers('or',AtomStart)=0 then begin
            IsKeyword:=true;
            if not PushBinaryOperator then break;
          end;
        'T':
          case UpChars[AtomStart[1]] of
          'H':
            if CompareIdentifiers('then',AtomStart)=0 then begin
              break;
            end;
          'R':
            if CompareIdentifiers('true',AtomStart)=0 then begin
              IsKeyword:=true;
              if not OperandAllowed then break;
              PushBooleanValue(true);
            end;
          end;
        'U':
          if CompareIdentifiers('undefined',AtomStart)=0 then begin
            IsKeyword:=true;
            if not OperandAllowed then break;
            if not RunDefined(true) then break;
          end;
        'X':
          if CompareIdentifiers('xor',AtomStart)=0 then begin
            IsKeyword:=true;
            if not PushBinaryOperator then break;
          end;
        end;
        if (not IsKeyword) and AtomIsKeyWord then begin
          AddError('unexpected keyword '+GetAtom);
          break;
        end;
        if not IsKeyword then begin
          // a variable
          if not OperandAllowed then break;
          FStack.Push(ctcssOperand,AtomStart);
          Item:=@FStack.Items[FStack.Top];
          v:=Variables.GetVariable(AtomStart);
          if v<>nil then begin
            SetCTCSVariableValue(v,@Item^.Operand);
          end;
          ExecuteStack(1);
        end;
      end;
    '#','''':
      begin
        if not OperandAllowed then break;
        PushStringConstant;
      end;
    '0'..'9','$','%','&':
      begin
        // float, decimal, hex, octal, binary constant
        if not OperandAllowed then break;
        PushNumberConstant;
      end;
    else
      if FStack.TopTyp in [ctcssOperator,ctcssRoundBracketOpen]
      then begin
        AddError('operand expected, but '+GetAtom+' found');
        Result:=false;
      end;
      break;
    end;
    ReadRawNextPascalAtom(Src,AtomStart);
  end;

  if Result then begin
    ExecuteStack(10);
    if FStack.Top=StartTop+1 then begin
      // empty expression
      AddError('operand expected, but '+GetAtom+' found');
    end else if (FStack.TopTyp<>ctcssOperand) or (FStack.Top<>StartTop+2) then begin
      // unfinished expression
      if FStack.TopTyp in [ctcssOperator,ctcssRoundBracketOpen]
      then
        AddError('operand expected, but '+GetAtom+' found')
      else
        AddError('operator expected, but '+GetAtom+' found');
      Result:=false;
    end
    else begin
      // success
      Item:=@FStack.Items[FStack.Top];
      SetCTCSVariableValue(@Item^.Operand,@Value);
      debugln(['TCTConfigScriptEngine.RunExpression Result="',dbgs(PCTCfgScriptVariable(@Value)),'" ',dbgs(@Value)]);
    end;
  end;

  // clean up stack
  while (FStack.Top>StartTop) do FStack.Pop;
end;

function TCTConfigScriptEngine.AtomIsKeyWord: boolean;
begin
  Result:=false;
  case UpChars[AtomStart^] of
  'A':
    if CompareIdentifiers('and',AtomStart)=0 then
      exit(true);
  'B':
    if CompareIdentifiers('begin',AtomStart)=0 then
      exit(true);
  'C':
    if CompareIdentifiers('case',AtomStart)=0 then
      exit(true);
  'D':
    case UpChars[AtomStart[1]] of
    'E':
      if CompareIdentifiers('defined',AtomStart)=0 then
        exit(true);
    'I':
      if CompareIdentifiers('div',AtomStart)=0 then
        exit(true);
    end;
  'E':
    case UpChars[AtomStart[1]] of
    'L':
      if CompareIdentifiers('else',AtomStart)=0 then
        exit(true);
    'N':
      if CompareIdentifiers('end',AtomStart)=0 then
        exit(true);
    end;
  'F':
    case UpChars[AtomStart[1]] of
    'A':
      if CompareIdentifiers('false',AtomStart)=0 then
        exit(true);
    'U':
      if CompareIdentifiers('function',AtomStart)=0 then
        exit(true);
    end;
  'I':
    case UpChars[AtomStart[1]] of
    'F':
      if CompareIdentifiers('if',AtomStart)=0 then
        exit(true);
    'N':
      if CompareIdentifiers('in',AtomStart)=0 then
        exit(true);
    end;
  'M':
    if CompareIdentifiers('mod',AtomStart)=0 then
      exit(true);
  'N':
    if CompareIdentifiers('not',AtomStart)=0 then
      exit(true);
  'O':
    case UpChars[AtomStart[1]] of
    'F':
      if CompareIdentifiers('of',AtomStart)=0 then
        exit(true);
    'R':
      if CompareIdentifiers('or',AtomStart)=0 then
        exit(true);
    end;
  'P':
    if CompareIdentifiers('procedure',AtomStart)=0 then
      exit(true);
  'S':
    case UpChars[AtomStart[1]] of
    'H':
      case UpChars[AtomStart[2]] of
      'L':
        if CompareIdentifiers('shl',AtomStart)=0 then
          exit(true);
      'R':
        if CompareIdentifiers('shr',AtomStart)=0 then
          exit(true);
      end;
    end;
  'T':
    case UpChars[AtomStart[1]] of
    'H':
      if CompareIdentifiers('then',AtomStart)=0 then
        exit(true);
    'R':
      if CompareIdentifiers('true',AtomStart)=0 then
        exit(true);
    end;
  'X':
    if CompareIdentifiers('xor',AtomStart)=0 then
      exit(true);
  'U':
    if CompareIdentifiers('undefined',AtomStart)=0 then
      exit(true);
  end;
end;

function TCTConfigScriptEngine.ExecuteStack(MaxLevel: integer): boolean;
{ execute all operators on stack with level <= maxlevel
}
var
  OperatorItem: PCTCfgScriptStackItem;
  Typ: TCTCfgScriptOperator;
  OperandItem: PCTCfgScriptStackItem;
  b: Boolean;

  procedure ErrorInvalidOperator;
  begin
    raise Exception.Create('TCTConfigScriptEngine.ExecuteStack invalid operator: '+GetAtom(OperatorItem^.StartPos));
  end;

begin
  Result:=true;
  repeat
    WriteDebugReportStack('ExecuteStack MaxLevel='+dbgs(MaxLevel));
    if (FStack.TopTyp<>ctcssOperand) or (FStack.Top<=0) then
      exit;
    OperatorItem:=@FStack.Items[FStack.Top-1];
    if (OperatorItem^.Typ<>ctcssOperator)
    or (GetOperatorLevel(OperatorItem^.StartPos)>MaxLevel) then
      exit;
    OperandItem:=@FStack.Items[FStack.Top];

    // execute operator
    Typ:=AtomToCTCfgOperator(OperatorItem^.StartPos);
    debugln(['TCTConfigScriptEngine.ExecuteStack execute operator "',GetAtom(OperatorItem^.StartPos),'" Typ=',dbgs(Typ)]);
    case Typ of
    ctcsoNot:
      begin
        b:=CTCSVariableIsTrue(@OperandItem^.Operand);
        FStack.Pop;
        FStack.Pop;
        PushBooleanValue(not b);
      end;
    ctcsoAnd:
      begin
        b:=CTCSVariableIsTrue(@OperandItem^.Operand);
        FStack.Pop;
        FStack.Pop;
        if (FStack.Top>=0) then begin
          OperandItem:=@FStack.Items[FStack.Top];
          b:=b and CTCSVariableIsTrue(@OperandItem^.Operand);
          FStack.Pop;
        end;
        PushBooleanValue(b);
      end;
    ctcsoOr:
      begin
        b:=CTCSVariableIsTrue(@OperandItem^.Operand);
        FStack.Pop;
        FStack.Pop;
        if (FStack.Top>=0) then begin
          OperandItem:=@FStack.Items[FStack.Top];
          b:=b or CTCSVariableIsTrue(@OperandItem^.Operand);
          FStack.Pop;
        end;
        PushBooleanValue(b);
      end;
    ctcsoXOr:
      begin
        b:=CTCSVariableIsTrue(@OperandItem^.Operand);
        FStack.Pop;
        FStack.Pop;
        if (FStack.Top>=0) then begin
          OperandItem:=@FStack.Items[FStack.Top];
          b:=b xor CTCSVariableIsTrue(@OperandItem^.Operand);
          FStack.Pop;
        end;
        PushBooleanValue(b);
      end;
    else
      ErrorInvalidOperator;
    end;
  until false;
end;

function TCTConfigScriptEngine.GetOperatorLevel(P: PChar): integer;
begin
  Result:=CTCfgScriptOperatorLvl[AtomToCTCfgOperator(P)];
end;

constructor TCTConfigScriptEngine.Create;
begin
  FVariables:=TCTCfgScriptVariables.Create;
  FStack:=TCTCfgScriptStack.Create;
  FErrors:=TFPList.Create;
end;

destructor TCTConfigScriptEngine.Destroy;
begin
  ClearErrors;
  FreeAndNil(FErrors);
  FreeAndNil(FVariables);
  FreeAndNil(FStack);
  inherited Destroy;
end;

procedure TCTConfigScriptEngine.ClearErrors;
var
  i: Integer;
begin
  for i:=0 to FErrors.Count-1 do
    TObject(FErrors[i]).Free;
  FErrors.Clear;
end;

function TCTConfigScriptEngine.Execute(const Source: string;
  StopAfterErrors: integer): boolean;
begin
  FStack.Clear;
  ClearErrors;
  MaxErrorCount:=StopAfterErrors;
  SrcStart:=#0;
  SrcEnd:=SrcStart;
  Src:=SrcStart;
  AtomStart:=SrcStart;

  if Source='' then exit(true);

  SrcStart:=PChar(Source);
  SrcEnd:=SrcStart+length(Source);
  Src:=SrcStart;
  AtomStart:=Src;

  // execute all statements
  ReadRawNextPascalAtom(Src,AtomStart);
  while Src^<>#0 do begin
    RunStatement(false);
    ReadRawNextPascalAtom(Src,AtomStart);
  end;

  Result:=ErrorCount=0;
end;

function TCTConfigScriptEngine.ErrorCount: integer;
begin
  Result:=FErrors.Count;
end;

function TCTConfigScriptEngine.GetAtom: string;
begin
  if (AtomStart=nil) or (AtomStart>Src) then
    exit('');
  SetLength(Result,Src-AtomStart);
  if Result<>'' then
    System.Move(AtomStart^,Result[1],length(Result));
end;

function TCTConfigScriptEngine.GetAtomOrNothing: string;
begin
  if (AtomStart=nil) or (AtomStart>Src) then
    Result:='nothing'
  else begin
    SetLength(Result,Src-AtomStart);
    if Result<>'' then
      System.Move(AtomStart^,Result[1],length(Result));
  end;
end;

function TCTConfigScriptEngine.GetAtom(P: PChar): string;
var
  StartPos: PChar;
begin
  if P=nil then
    exit('');
  ReadRawNextPascalAtom(P,StartPos);
  SetLength(Result,p-StartPos);
  if Result<>'' then
    System.Move(StartPos^,Result[1],length(Result));
end;

function TCTConfigScriptEngine.PosToLineCol(p: PChar; out Line, Column: integer
  ): boolean;
var
  run: PChar;
begin
  Line:=1;
  Column:=1;
  if (p<SrcStart) or (p>SrcEnd) then exit(false);
  run:=SrcStart;
  while run<p do begin
    if Run^ in [#10,#13] then begin
      inc(Line);
      Column:=1;
      if (Run[1] in [#10,#13]) and (Run^<>Run[1]) then
        inc(Run,2)
      else
        inc(Run);
    end else begin
      inc(Run);
      inc(Column);
    end;
  end;
end;

function TCTConfigScriptEngine.PosToStr(p: PChar): string;
var
  Line: integer;
  Column: integer;
begin
  if PosToLineCol(p,Line,Column) then
    Result:='('+IntToStr(Line)+','+IntToStr(Column)+')'
  else
    Result:='';
end;

function TCTConfigScriptEngine.GetErrorStr(Index: integer): string;
var
  Err: TCTCfgScriptError;
  s: String;
begin
  Err:=Errors[Index];
  Result:='Error: ';
  s:=PosToStr(Err.ErrorPos);
  if s<>'' then
    Result:=Result+s+' ';
  Result:=Result+Err.Msg;
end;

procedure TCTConfigScriptEngine.WriteDebugReportStack(Title: string);
var
  i: Integer;
  Item: PCTCfgScriptStackItem;
begin
  debugln(['TCTConfigScriptEngine.WriteDebugReportStack FStack.Top=',FStack.Top,' ',Title]);
  for i:=0 to FStack.Top do begin
    dbgout(GetIndentStr(i*2+2));
    Item:=@FStack.Items[i];
    dbgout(dbgs(Item^.Typ),' StartPos=',GetAtom(Item^.StartPos));
    if Item^.Typ=ctcssOperator then
      dbgout(' level='+dbgs(GetOperatorLevel(Item^.StartPos)));
    if Item^.Typ=ctcssOperand then
      dbgout(' ',dbgs(PCTCfgScriptVariable(@Item^.Operand)));
    debugln;
  end;
end;

{ TCTCfgScriptStack }

constructor TCTCfgScriptStack.Create;
begin
  Top:=-1;
end;

destructor TCTCfgScriptStack.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCTCfgScriptStack.Clear;
var
  i: Integer;
  Item: PCTCfgScriptStackItem;
begin
  for i:=0 to Top do begin
    Item:=@Items[i];
    ClearCTCSVariable(@Item^.Operand);
    if Item^.Operand.Name<>nil then
      ReAllocMem(Item^.Operand.Name,0);
  end;
  Top:=-1;
  TopTyp:=ctcssNone;
  Capacity:=0;
  ReAllocMem(Items,0);
end;

procedure TCTCfgScriptStack.Push(Typ: TCTCfgScriptStackItemType;
  const StartPos: PChar);
var
  OldCapacity: LongInt;
  Item: PCTCfgScriptStackItem;
begin
  inc(Top);
  if Top>=Capacity then begin
    OldCapacity:=Capacity;
    if Capacity<10 then
      Capacity:=10
    else
      Capacity:=Capacity*2;
    ReAllocMem(Items,Capacity*SizeOf(TCTCfgScriptStackItem));
    FillByte(Items[OldCapacity],(Capacity-OldCapacity)*SizeOf(TCTCfgScriptStackItem),0);
  end;
  Item:=@Items[Top];
  Item^.Typ:=Typ;
  Item^.StartPos:=StartPos;
  TopTyp:=Typ;
end;

procedure TCTCfgScriptStack.Pop;

  procedure RaiseTooManyPop;
  begin
    raise Exception.Create('TCTCfgScriptStack.Pop too many pop');
  end;

var
  Item: PCTCfgScriptStackItem;
begin
  if Top<0 then
    RaiseTooManyPop;
  Item:=@Items[Top];
  ClearCTCSVariable(@Item^.Operand);
  if Item^.Operand.Name<>nil then
    ReAllocMem(Item^.Operand.Name,0);
  dec(Top);
  if Top>=0 then
    TopTyp:=Items[0].Typ
  else
    TopTyp:=ctcssNone;
end;

{ TCTCfgScriptError }

constructor TCTCfgScriptError.Create(const aMsg: string; anErrorPos: PChar);
begin
  Msg:=aMsg;
  ErrorPos:=anErrorPos;
end;

end.

