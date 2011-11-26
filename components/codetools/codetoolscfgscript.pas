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
    TCTConfigScriptEngine implements an interpreter for a simple scripting
    language, enough for configurations.

  Working:
    if, then, else, begin..end, ;
    ()
    boolean operators: not, and, or, xor
    operators: =, <>, >, <, <=, >=, +
    variables
    constants: decimal, hex, octal, binary, string, #decimal
    functions: string(), integer(), int64(), defined(), undefined()
    procedures: undefine()
    assignments: :=, +=

  Not supported:
    - floats
    - types
    - objects
    - loops
    - functions
}
unit CodeToolsCfgScript;

{$mode objfpc}{$H+}
{$inline on}

{off $Define VerboseCTCfgScript}
{off $DEFINE CheckCTCfgVars}

interface

uses
  Classes, SysUtils, BasicCodeTools, AVL_Tree, KeywordFuncLists, FileProcs,
  typinfo, CodeToolsStrConsts;

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
    procedure Assign(Source: TCTCfgScriptVariables); overload;
    procedure Assign(Source: TStrings); overload;
    procedure AddOverrides(Source: TCTCfgScriptVariables);
    procedure AddOverride(Source: PCTCfgScriptVariable);
    function GetVariable(const Name: PChar;
                      CreateIfNotExists: Boolean = false): PCTCfgScriptVariable;
    property Values[const Name: string]: string read GetValues write SetValues; default;
    procedure Undefine(Name: PChar);
    procedure Define(Name: PChar; const Value: string);
    function IsDefined(Name: PChar): boolean;
    property Tree: TAVLTree read FItems;
    procedure WriteDebugReport(const Title: string; const Prefix: string = '');
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
    procedure Pop(Count: integer = 1);
    procedure Delete(Index: integer);
    function TopItem: PCTCfgScriptStackItem;
    function TopItemOperand: PCTCfgScriptVariable;
    {$IFDEF CheckCTCfgVars}
    procedure CheckOperands;
    {$ENDIF}
  end;

  { TCTCfgScriptError }

  TCTCfgScriptError = class
  public
    Msg: string;
    Position: integer;
    Line: integer;
    Column: integer;
    constructor Create(const aMsg: string; aPos, aLine, aCol: integer);
    constructor Create(const aMsg: string);
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
    function RunFunction: boolean;
    procedure PushStringConstant;
    procedure RunStatement(Skip: boolean);
    procedure RunBegin(Skip: boolean);
    procedure RunIf(Skip: boolean);
    procedure RunUndefine(Skip: boolean);
    procedure RunAssignment(Skip: boolean);
    function RunExpression: boolean; // if true the stack top has an operand
    function ExecuteStack(MaxLevel: integer): boolean;
    function GetOperatorLevel(P: PChar): integer;
    function IsKeyWord(P: PChar): boolean;
    function IsFunction(P: PChar): boolean;
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

procedure RenameCTCSVariable(var Src: string; const OldName, NewName: string);

function CompareCTCSVariables(Var1, Var2: Pointer): integer;
function ComparePCharWithCTCSVariableName(Name, aVar: Pointer): integer;
function AreCTCSVariablesEqual(const V1, V2: PCTCfgScriptVariable): Boolean;
function AreCTCSVariablesExactEqual(const V1, V2: PCTCfgScriptVariable): Boolean;
function CompareCTCSVariables(const Left, Right: PCTCfgScriptVariable;
                           out Equal, LeftIsLowerThanRight: boolean): boolean;
function NewCTCSVariable: PCTCfgScriptVariable;
function NewCTCSVariable(CloneName: PChar): PCTCfgScriptVariable;
function CloneCTCSVariable(const V: PCTCfgScriptVariable): PCTCfgScriptVariable;
procedure FreeCTCSVariable(var V: PCTCfgScriptVariable);
procedure ClearCTCSVariable(const V: PCTCfgScriptVariable);
procedure SetCTCSVariableAsString(const V: PCTCfgScriptVariable; const s: string);
procedure SetCTCSVariableAsNumber(const V: PCTCfgScriptVariable; const i: int64);
procedure SetCTCSVariableValue(const Src, Dest: PCTCfgScriptVariable);
function GetCTCSVariableAsString(const V: PCTCfgScriptVariable): string;
procedure MakeCTCSVariableString(const V: PCTCfgScriptVariable);
procedure MakeCTCSVariableInt64(const V: PCTCfgScriptVariable);
procedure MakeCTCSVariableInteger(const V: PCTCfgScriptVariable);
procedure AddCTCSVariables(const AddVar, SumVar: PCTCfgScriptVariable);
function CTCSNumberEqualsString(const Number: int64; const P: PChar): boolean; inline;
function CTCSVariableIsTrue(const V: PCTCfgScriptVariable): boolean; inline;
function CTCSVariableIsFalse(const V: PCTCfgScriptVariable): boolean;

function CTCSStringToNumber(P: PChar; out Number: int64): boolean;
function AtomToCTCfgOperator(p: PChar): TCTCfgScriptOperator;

procedure CheckCTCSVariable(const V: PCTCfgScriptVariable);
function dbgs(const t: TCTCfgScriptStackItemType): string; overload;
function dbgs(const t: TCTCSValueType): string; overload;
function dbgs(const t: TCTCfgScriptOperator): string; overload;
function dbgs(const V: PCTCfgScriptVariable): string; overload;


implementation

procedure RenameCTCSVariable(var Src: string; const OldName, NewName: string);
var
  p: PChar;
  AtomStart: PChar;
  SrcPos: PtrUInt;
begin
  if (Src='') or (OldName='') or (not IsValidIdent(OldName))
  or (NewName='') then exit;
  p:=PChar(Src);
  //debugln(['RenameCTCSVariable START ',dbgstr(Src)]);
  repeat
    ReadRawNextPascalAtom(p,AtomStart);
    if (p=AtomStart) then break;
    if IsIdentStartChar[AtomStart^]
    and (CompareIdentifierPtrs(PChar(OldName),AtomStart)=0)
    then begin
      SrcPos:=PtrUInt(AtomStart-PChar(Src))+1;
      Src:=copy(Src,1,SrcPos-1)+NewName+copy(Src,SrcPos+PtrUInt(length(OldName)),length(Src));
      p:=@Src[SrcPos]+length(NewName);
    end;
  until false;
  //debugln(['RenameCTCSVariable END ',dbgstr(Src)]);
end;

function CompareCTCSVariables(Var1, Var2: Pointer): integer;
var
  v1: PCTCfgScriptVariable absolute Var1;
  v2: PCTCfgScriptVariable absolute Var2;
begin
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(v1);
  CheckCTCSVariable(v2);
  {$ENDIF}
  Result:=CompareIdentifiers(v1^.Name,v2^.Name);
end;

function ComparePCharWithCTCSVariableName(Name, aVar: Pointer): integer;
var
  n: PChar absolute Name;
  v: PCTCfgScriptVariable absolute aVar;
begin
  {$IFDEF CheckCTCfgVars}CheckCTCSVariable(v);{$ENDIF}
  Result:=CompareIdentifiers(n,v^.Name);
end;

function AreCTCSVariablesEqual(const V1, V2: PCTCfgScriptVariable): Boolean;
begin
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(v1);
  CheckCTCSVariable(v2);
  {$ENDIF}
  Result:=false;
  case V1^.ValueType of
  ctcsvNone:
    exit; // invalid is never equal to anything
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
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(v1);
  CheckCTCSVariable(v2);
  {$ENDIF}
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

function CompareCTCSVariables(const Left, Right: PCTCfgScriptVariable; out
  Equal, LeftIsLowerThanRight: boolean): boolean;
{ Rules:
    If one of the values is invalid, return false
    If both are numbers, compare as numbers
    Otherwise compare as string alphabetically case sensitive A<B, A<AA
}

  procedure CompareNumberWithString(Number: int64; p: PChar);
  var
    i: Integer;
    Cnt: integer;
    s: array[0..30] of char;
  begin
    if p=nil then begin
      Equal:=false;
      LeftIsLowerThanRight:=false;
      exit;
    end;
    // convert number to decimal string
    if Number=0 then begin
      Cnt:=1;
      s[0]:='0';
    end else begin
      Cnt:=0;
      if Number<0 then begin
        Cnt:=1;
        s[0]:='-';
        Number:=-Number;
      end;
      while Number>0 do begin
        s[Cnt]:=chr(Number mod 10+ord('0'));
        inc(Cnt);
        Number:=Number div 10;
      end;
    end;
    for i:=0 to Cnt-1 do begin
      if p^<>s[i] then begin
        Equal:=false;
        LeftIsLowerThanRight:=s[i]<p^;
        exit;
      end;
      inc(p);
    end;
    if p^=#0 then begin
      Equal:=true;
      LeftIsLowerThanRight:=false;
    end else begin
      Equal:=False;
      LeftIsLowerThanRight:=true;
    end;
  end;

var
  V1: PChar;
  V2: PChar;
begin
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(Left);
  CheckCTCSVariable(Right);
  {$ENDIF}
  //debugln(['CompareCTCSVariables AAA1 Left=',dbgs(Left),' Right=',dbgs(Right)]);
  Result:=false;
  Equal:=false;
  LeftIsLowerThanRight:=false;
  case Left^.ValueType of
  ctcsvNone:
    exit; // invalid is never equal to anything
  ctcsvString:
    case Right^.ValueType of
    ctcsvNone: exit;
    ctcsvString:
      begin
        // compare two strings
        V1:=Left^.StrStart;
        V2:=Right^.StrStart;
        if V1=nil then begin
          if V2=nil then begin
            Equal:=true;
            LeftIsLowerThanRight:=false;
          end else begin
            Equal:=False;
            LeftIsLowerThanRight:=true; // left is shorter than right
          end;
        end else begin
          if V2=nil then begin
            Equal:=False;
            LeftIsLowerThanRight:=false; // left is longer than right
          end else begin
            repeat
              if V1^=V2^ then begin
                if V1^=#0 then begin
                  Equal:=true;
                  LeftIsLowerThanRight:=false;
                  break;
                end else begin
                  inc(V1);
                  inc(V2);
                end;
              end else begin
                Equal:=false;
                LeftIsLowerThanRight:=V1^<V2^;
                break;
              end;
            until false;
          end;
        end;
      end;
    ctcsvNumber:
      begin
        CompareNumberWithString(Right^.Number,Left^.StrStart);
        LeftIsLowerThanRight:=not LeftIsLowerThanRight;
      end;
    end;
  ctcsvNumber:
    case Right^.ValueType of
    ctcsvNone: exit;
    ctcsvString:
      CompareNumberWithString(Left^.Number,Right^.StrStart);
    ctcsvNumber:
      begin
        Equal:=Left^.Number=Right^.Number;
        LeftIsLowerThanRight:=Left^.Number<Right^.Number;
      end;
    end;
  end;
  Result:=true;
end;

function NewCTCSVariable: PCTCfgScriptVariable;
begin
  New(Result);
  FillByte(Result^,SizeOf(TCTCfgScriptVariable),0);
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(Result);
  {$ENDIF}
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
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(Result);
  {$ENDIF}
end;

function CloneCTCSVariable(const V: PCTCfgScriptVariable): PCTCfgScriptVariable;
var
  l: LongInt;
begin
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(v);
  {$ENDIF}
  Result:=NewCTCSVariable(V^.Name);
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(Result);
  {$ENDIF}
  Result^.ValueType:=V^.ValueType;
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(Result);
  {$ENDIF}
  case V^.ValueType of
  ctcsvNone: ;
  ctcsvString:
    begin
      l:=V^.StrLen;
      if l>0 then begin
        Result^.StrLen:=l;
        Result^.StrStart:=GetMem(l+1);
        System.Move(V^.StrStart^,Result^.StrStart^,l);
        Result^.StrStart[l]:=#0;
      end;
    end;
  ctcsvNumber:
    Result^.Number:=V^.Number;
  end;
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(Result);
  {$ENDIF}
end;

procedure SetCTCSVariableValue(const Src, Dest: PCTCfgScriptVariable);
var
  l: LongInt;
begin
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(Src);
  CheckCTCSVariable(Dest);
  {$ENDIF}
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
      if l>0 then begin
        ReAllocMem(Dest^.StrStart,l+1);
        System.Move(Src^.StrStart^,Dest^.StrStart^,l);
        Dest^.StrStart[l]:=#0;
      end else
        ReAllocMem(Dest^.StrStart,0);
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
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(Src);
  CheckCTCSVariable(Dest);
  {$ENDIF}
end;

procedure FreeCTCSVariable(var V: PCTCfgScriptVariable);
begin
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(v);
  {$ENDIF}
  ClearCTCSVariable(V);
  ReAllocMem(V^.Name,0);
  Dispose(V);
end;

procedure ClearCTCSVariable(const V: PCTCfgScriptVariable);
begin
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(v);
  {$ENDIF}
  if V^.ValueType=ctcsvString then
    ReAllocMem(V^.StrStart,0);
  V^.ValueType:=ctcsvNone;
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(v);
  {$ENDIF}
end;

procedure MakeCTCSVariableString(const V: PCTCfgScriptVariable);
var
  s: String;
begin
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(V);
  {$ENDIF}
  case V^.ValueType of
  ctcsvNone:
    begin
      V^.StrLen:=0;
      V^.StrStart:=nil;
      V^.ValueType:=ctcsvString;
    end;
  ctcsvString: ;
  ctcsvNumber:
    begin
      s:=IntToStr(V^.Number);
      V^.StrLen:=length(s);
      V^.StrStart:=GetMem(length(s)+1);
      System.Move(s[1],V^.StrStart^,length(s)+1);
      V^.ValueType:=ctcsvString;
    end;
  end;
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(V);
  {$ENDIF}
end;

procedure MakeCTCSVariableInt64(const V: PCTCfgScriptVariable);
var
  i: Int64;
begin
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(V);
  {$ENDIF}
  case V^.ValueType of
  ctcsvNone:
    begin
      V^.Number:=0;
      V^.ValueType:=ctcsvNumber;
    end;
  ctcsvString:
    begin
      if V^.StrStart<>nil then begin
        i:=StrToInt64Def(V^.StrStart,0);
        FreeMem(V^.StrStart);
        V^.Number:=i;
      end else
        V^.Number:=0;
      V^.ValueType:=ctcsvNumber;
    end;
  ctcsvNumber: ;
  end;
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(V);
  {$ENDIF}
end;

procedure MakeCTCSVariableInteger(const V: PCTCfgScriptVariable);
var
  i: integer;
begin
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(V);
  {$ENDIF}
  case V^.ValueType of
  ctcsvNone:
    begin
      V^.Number:=0;
      V^.ValueType:=ctcsvNumber;
    end;
  ctcsvString:
    begin
      if V^.StrStart<>nil then begin
        i:=StrToIntDef(V^.StrStart,0);
        FreeMem(V^.StrStart);
        V^.Number:=i;
      end else
        V^.Number:=0;
      V^.ValueType:=ctcsvNumber;
    end;
  ctcsvNumber: ;
  end;
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(V);
  {$ENDIF}
end;

procedure AddCTCSVariables(const AddVar, SumVar: PCTCfgScriptVariable);
{ If one of them is none, then save in sum the other value
  If both are numbers, add them.
  Otherwise concatenate as strings.
}
var
  OldLen: LongInt;
  s: String;
begin
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(AddVar);
  CheckCTCSVariable(SumVar);
  {$ENDIF}
  case SumVar^.ValueType of
  ctcsvNone:
    SetCTCSVariableValue(AddVar,SumVar);
  ctcsvString:
    case AddVar^.ValueType of
    ctcsvNone:
      ;
    ctcsvString:
      if AddVar^.StrLen>0 then begin
        // append
        OldLen:=SumVar^.StrLen;
        SumVar^.StrLen+=AddVar^.StrLen;
        ReAllocMem(SumVar^.StrStart,SumVar^.StrLen+1);
        System.Move(AddVar^.StrStart^,SumVar^.StrStart[OldLen],AddVar^.StrLen+1);
      end;
    ctcsvNumber:
      begin
        // append as string
        s:=IntToStr(AddVar^.Number);
        OldLen:=SumVar^.StrLen;
        SumVar^.StrLen+=length(s);
        ReAllocMem(SumVar^.StrStart,SumVar^.StrLen+1);
        System.Move(s[1],SumVar^.StrStart[OldLen],length(s)+1);
      end;
    end;
  ctcsvNumber:
    case AddVar^.ValueType of
    ctcsvNone:
      ;
    ctcsvString:
      begin
        // convert SumVar from number to string and append
        s:=IntToStr(SumVar^.Number);
        SumVar^.ValueType:=ctcsvString;
        SumVar^.StrLen:=length(s)+AddVar^.StrLen;
        SumVar^.StrStart:=GetMem(SumVar^.StrLen+1);
        System.Move(s[1],SumVar^.StrStart^,length(s));
        if AddVar^.StrStart<>nil then
          System.Move(AddVar^.StrStart^,SumVar^.StrStart[length(s)],AddVar^.StrLen+1)
        else
          SumVar^.StrStart[SumVar^.StrLen]:=#0;
      end;
    ctcsvNumber:
      try
        SumVar^.Number+=AddVar^.Number;
      except
      end;
    end;
  end;
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(AddVar);
  CheckCTCSVariable(SumVar);
  {$ENDIF}
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

procedure CheckCTCSVariable(const V: PCTCfgScriptVariable);
begin
  if V=nil then
    RaiseCatchableException('');
  if (V^.Name<>nil) and (strlen(V^.Name)>255) then
    RaiseCatchableException('');
  case V^.ValueType of
  ctcsvNone: ;
  ctcsvString:
    begin
      if V^.StrLen=0 then begin
        if V^.StrStart<>nil then
          RaiseCatchableException('');
      end else begin
        if V^.StrStart=nil then
          RaiseCatchableException('');
        if strlen(V^.StrStart)<>V^.StrLen then
          RaiseCatchableException('');
      end;
    end;
  ctcsvNumber: ;
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
      if V^.StrLen>0 then begin
        SetLength(Result,l+V^.StrLen);
        System.Move(V^.StrStart^,Result[l+1],V^.StrLen);
      end;
    end;
  ctcsvNumber:
    Result:=Result+'int64='+IntToStr(V^.Number);
  end;
end;

function GetCTCSVariableAsString(const V: PCTCfgScriptVariable): string;
begin
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(V);
  {$ENDIF}
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

procedure SetCTCSVariableAsString(const V: PCTCfgScriptVariable; const s: string);
var
  l: Integer;
begin
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(v);
  {$ENDIF}
  if V^.ValueType<>ctcsvString then begin
    V^.ValueType:=ctcsvString;
    V^.StrLen:=0;
    V^.StrStart:=nil;
  end;
  l:=length(s);
  V^.StrLen:=l;
  if l>0 then begin
    ReAllocMem(V^.StrStart,l+1);
    System.Move(s[1],V^.StrStart^,l+1); // +1 for the #0
  end else
    ReAllocMem(V^.StrStart,0);
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(v);
  {$ENDIF}
end;

procedure SetCTCSVariableAsNumber(const V: PCTCfgScriptVariable; const i: int64);
begin
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(v);
  {$ENDIF}
  if (V^.ValueType=ctcsvString) and (V^.StrStart<>nil) then
    Freemem(V^.StrStart);
  V^.ValueType:=ctcsvNumber;
  V^.Number:=i;
  {$IFDEF CheckCTCfgVars}
  CheckCTCSVariable(v);
  {$ENDIF}
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
  FreeAndNil(FItems);
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
  if Self=Source then exit;
  Clear;
  Node:=Source.FItems.FindLowest;
  while Node<>nil do begin
    Item:=PCTCfgScriptVariable(Node.Data);
    NewItem:=CloneCTCSVariable(Item);
    FItems.Add(NewItem);
    Node:=Source.FItems.FindSuccessor(Node);
  end;
end;

procedure TCTCfgScriptVariables.Assign(Source: TStrings);
var
  Name: string;
  Value: string;
  i: Integer;
begin
  Clear;
  for i:=0 to Source.Count-1 do begin
    Name:=Source.Names[i];
    if (Name='') or not IsValidIdent(Name) then continue;
    Value:=Source.ValueFromIndex[i];
    Define(PChar(Name),Value);
  end;
end;

procedure TCTCfgScriptVariables.AddOverrides(Source: TCTCfgScriptVariables);
var
  Item: PCTCfgScriptVariable;
  Node: TAVLTreeNode;
begin
  Node:=Source.FItems.FindLowest;
  while Node<>nil do begin
    Item:=PCTCfgScriptVariable(Node.Data);
    AddOverride(Item);
    Node:=Source.FItems.FindSuccessor(Node);
  end;
end;

procedure TCTCfgScriptVariables.AddOverride(Source: PCTCfgScriptVariable);
var
  Node: TAVLTreeNode;
  Item: PCTCfgScriptVariable;
begin
  Node:=FItems.Find(Source);
  if Node<>nil then begin
    Item:=PCTCfgScriptVariable(Node.Data);
    SetCTCSVariableValue(Source,Item);
  end else begin
    Item:=CloneCTCSVariable(Source);
    FItems.Add(Item);
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

procedure TCTCfgScriptVariables.Undefine(Name: PChar);
var
  Node: TAVLTreeNode;
  Item: PCTCfgScriptVariable;
begin
  Node:=FItems.FindKey(Name,@ComparePCharWithCTCSVariableName);
  if Node=nil then exit;
  Item:=PCTCfgScriptVariable(Node.Data);
  FreeCTCSVariable(Item);
  FItems.Delete(Node);
end;

procedure TCTCfgScriptVariables.Define(Name: PChar; const Value: string);

  function IsNumber: boolean;
  var
    p: PChar;
  begin
    if Value='' then exit;
    p:=PChar(Value);
    if p^='-' then inc(p);
    while (p^ in ['0'..'9']) do inc(p);
    Result:=(p^=#0) and (p-PChar(Value)=length(Value));
  end;

var
  V: PCTCfgScriptVariable;
  i: Int64;
begin
  V:=GetVariable(Name,true);
  if Value='' then
    ClearCTCSVariable(V)
  else if IsNumber and TryStrToInt64(Value,i) then
    SetCTCSVariableAsNumber(V,i)
  else
    SetCTCSVariableAsString(V,Value);
end;

function TCTCfgScriptVariables.IsDefined(Name: PChar): boolean;
begin
  Result:=GetVariable(Name)<>nil;
end;

procedure TCTCfgScriptVariables.WriteDebugReport(const Title: string;
  const Prefix: string);
var
  Node: TAVLTreeNode;
  V: PCTCfgScriptVariable;
begin
  debugln([Prefix,'TCTCfgScriptVariables.WriteDebugReport Count=',Tree.Count,': ',Title]);
  Node:=FItems.FindLowest;
  while Node<>nil do begin
    V:=PCTCfgScriptVariable(Node.Data);
    debugln([Prefix,'  ',dbgs(V)]);
    Node:=FItems.FindSuccessor(Node);
  end;
end;

{ TCTConfigScriptEngine }

function TCTConfigScriptEngine.GetErrors(Index: integer): TCTCfgScriptError;
begin
  Result:=TCTCfgScriptError(FErrors[Index]);
end;

procedure TCTConfigScriptEngine.AddError(const aMsg: string; ErrorPos: PChar);
var
  Err: TCTCfgScriptError;
  Position: Integer;
  Line: Integer;
  Column: Integer;
begin
  {$IFDEF VerboseCTCfgScript}
  WriteDebugReportStack('ERROR: '+aMsg);
  {$ENDIF}
  Position:=-1;
  Line:=0;
  Column:=0;
  if (ErrorPos<>nil) then begin
    Position:=ErrorPos-Src;
    PosToLineCol(ErrorPos,Line,Column);
  end;
  Err:=TCTCfgScriptError.Create(aMsg,Position,Line,Column);
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
    AddError(Format(ctsExpectedStatementButFound, [GetAtomOrNothing]))
  end;

var
  Handled: Boolean;
  StartTop: LongInt;
begin
  {$IFDEF VerboseCTCfgScript}
  debugln(['TCTConfigScriptEngine.RunStatement Atom=',GetAtom]);
  {$ENDIF}
  StartTop:=FStack.Top;
  case AtomStart^ of
  #0: ;
  ';': ; // empty statement
  'a'..'z','A'..'Z':
    begin
      // identifier or keyword
      Handled:=false;
      case UpChars[AtomStart^] of
      'B':
        if CompareIdentifiers('BEGIN',AtomStart)=0 then begin
          Handled:=true;
          RunBegin(Skip);
        end;
      'I':
        if CompareIdentifiers('IF',AtomStart)=0 then begin
          Handled:=true;
          RunIf(Skip);
        end;
      'U':
        if CompareIdentifiers('Undefine',AtomStart)=0 then begin
          Handled:=true;
          RunUndefine(Skip);
        end;
      end;
      if (not Handled) then begin
        if IsKeyWord(AtomStart) then begin
          AddError(Format(ctsUnexpectedKeyword2, [GetAtom]));
        end else if IsFunction(AtomStart) then begin
          if not RunFunction then exit;
        end else begin
          // parse assignment
          RunAssignment(Skip);
        end;
      end;
    end;
  else
    ErrorUnexpectedAtom;
  end;
  // clean up stack
  while FStack.Top>StartTop do FStack.Pop;
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
    //debugln(['ErrorMissingEnd BeginStart=',BeginStart]);
    AddError(Format(ctsBeginAtWithoutEnd, [PosToStr(BeginStart)]));
  end;

begin
  BeginStart:=AtomStart;
  StartTop:=FStack.Top;
  FStack.Push(ctcssBegin,AtomStart);
  ReadRawNextPascalAtom(Src,AtomStart);
  repeat
    if (AtomStart^=#0) then begin
      ErrorMissingEnd;
      break;
    end else if CompareIdentifiers('END',AtomStart)=0 then begin
      FStack.Pop;
      ReadRawNextPascalAtom(Src,AtomStart);
      break;
    end else if AtomStart^=';' then begin
      // skip
      ReadRawNextPascalAtom(Src,AtomStart);
    end else begin
      RunStatement(Skip);
    end;
  until false;
  // clean up stack (recover from errors)
  while FStack.Top>StartTop do FStack.Pop;
end;

procedure TCTConfigScriptEngine.RunIf(Skip: boolean);
{ Examples:
    if expression then statement else statement
}
var
  IfStart: PChar;
  ExprIsTrue: Boolean;
  StartTop: LongInt;
begin
  IfStart:=AtomStart;
  StartTop:=FStack.Top;
  FStack.Push(ctcssIf,IfStart);
  ReadRawNextPascalAtom(Src,AtomStart);
  ExprIsTrue:=false;
  if RunExpression then begin
    ExprIsTrue:=CTCSVariableIsTrue(FStack.TopItemOperand);
    FStack.Pop;
  end;
  {$IFDEF VerboseCTCfgScript}
  debugln(['TCTConfigScriptEngine.RunIf expression=',ExprIsTrue]);
  {$ENDIF}

  // read then
  if CompareIdentifiers(AtomStart,'then')<>0 then
    AddError(Format(ctsThenExpectedButFound, [GetAtomOrNothing]));
  // then statement
  ReadRawNextPascalAtom(Src,AtomStart);
  RunStatement(Skip or not ExprIsTrue);
  if CompareIdentifiers(AtomStart,'else')=0 then begin
    // else statement
    ReadRawNextPascalAtom(Src,AtomStart);
    RunStatement(Skip or ExprIsTrue);
  end;
  // clean up stack
  while FStack.Top>StartTop do FStack.Pop;
end;

procedure TCTConfigScriptEngine.RunUndefine(Skip: boolean);
var
  VarStart: PChar;
begin
  ReadRawNextPascalAtom(Src,AtomStart);
  if AtomStart^<>'(' then begin
    AddError(Format(ctsExpectedButFound, [GetAtomOrNothing]));
    exit;
  end;
  ReadRawNextPascalAtom(Src,AtomStart);
  if (not IsIdentStartChar[AtomStart^]) or IsKeyWord(AtomStart) then begin
    AddError(Format(ctsExpectedIdentifierButFound, [GetAtomOrNothing]));
    exit;
  end;
  VarStart:=AtomStart;
  ReadRawNextPascalAtom(Src,AtomStart);
  if AtomStart^<>')' then begin
    AddError(Format(ctsExpectedButFound2, [GetAtomOrNothing]));
    exit;
  end;
  if not Skip then
    Variables.Undefine(VarStart);
  ReadRawNextPascalAtom(Src,AtomStart);
end;

procedure TCTConfigScriptEngine.RunAssignment(Skip: boolean);
{ Examples:
    a:=3;
}
var
  VarStart: PChar;
  Variable: PCTCfgScriptVariable;
  OperatorStart: PChar;
  StartTop: LongInt;
begin
  VarStart:=AtomStart;
  {$IFDEF VerboseCTCfgScript}
  debugln(['TCTConfigScriptEngine.RunAssignment ',GetIdentifier(VarStart)]);
  {$ENDIF}
  StartTop:=FStack.Top;
  FStack.Push(ctcssAssignment,VarStart);
  ReadRawNextPascalAtom(Src,AtomStart);
  {$IFDEF VerboseCTCfgScript}
  debugln(['TCTConfigScriptEngine.RunAssignment Operator=',GetAtom]);
  {$ENDIF}
  // read :=
  if AtomStart^=#0 then begin
    AddError(ctsMissing);
    exit;
  end;
  OperatorStart:=AtomStart;
  if (not (AtomStart^ in [':','+'])) or (AtomStart[1]<>'=') then begin
    AddError(Format(ctsExpectedButFound3, [GetAtom]));
    exit;
  end;
  // read expression
  ReadRawNextPascalAtom(Src,AtomStart);
  if RunExpression and (not Skip) then begin
    Variable:=Variables.GetVariable(VarStart,true);
    {$IFDEF VerboseCTCfgScript}
    debugln(['TCTConfigScriptEngine.RunAssignment BEFORE ',GetIdentifier(VarStart),'=(Old=',dbgs(Variable),') ',GetAtom(OperatorStart),' ',dbgs(FStack.TopItemOperand)]);
    {$ENDIF}
    case OperatorStart^ of
    ':': // :=
      SetCTCSVariableValue(FStack.TopItemOperand,Variable);
    '+': // +=
      AddCTCSVariables(FStack.TopItemOperand,Variable);
    end;

    {$IFDEF VerboseCTCfgScript}
    debugln(['TCTConfigScriptEngine.RunAssignment AFTER ',GetIdentifier(VarStart),' = ',dbgs(Variable)]);
    {$ENDIF}
  end;
  // clean up stack
  while FStack.Top>StartTop do FStack.Pop;
end;

procedure TCTConfigScriptEngine.PushNumberValue(const Number: int64);
var
  Operand: PCTCfgScriptVariable;
begin
  FStack.Push(ctcssOperand,AtomStart);
  Operand:=FStack.TopItemOperand;
  Operand^.ValueType:=ctcsvNumber;
  Operand^.Number:=Number;
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
    AddError(Format(ctsExpectedButFound, [GetAtomOrNothing]));
    exit;
  end;
  ReadRawNextPascalAtom(Src,AtomStart);
  if (not IsIdentStartChar[AtomStart^]) or IsKeyWord(AtomStart) then begin
    AddError(Format(ctsExpectedIdentifierButFound, [GetAtomOrNothing]));
    exit;
  end;
  VarStart:=AtomStart;
  ReadRawNextPascalAtom(Src,AtomStart);
  if AtomStart^<>')' then begin
    AddError(Format(ctsExpectedButFound2, [GetAtomOrNothing]));
    exit;
  end;
  b:=Variables.GetVariable(VarStart)<>nil;
  if Negate then b:=not b;
  PushBooleanValue(b);
  Result:=true;
end;

function TCTConfigScriptEngine.RunFunction: boolean;
var
  StartTop: LongInt;
  Value: TCTCfgScriptVariable;
  FunctionName: PChar;
begin
  Result:=false;
  FunctionName:=AtomStart;
  StartTop:=FStack.Top;
  ReadRawNextPascalAtom(Src,AtomStart);
  if AtomStart^<>'(' then begin
    AddError(Format(ctsExpectedButFound, [GetAtomOrNothing]));
    exit;
  end;
  ReadRawNextPascalAtom(Src,AtomStart);

  FStack.Push(ctcssRoundBracketOpen,AtomStart);
  FillByte(Value{%H-},SizeOf(Value),0);
  if RunExpression then
    SetCTCSVariableValue(FStack.TopItemOperand,@Value);
  if AtomStart^<>')' then begin
    AddError(Format(ctsExpectedButFound2, [GetAtomOrNothing]));
    exit;
  end;

  // clean up stack
  while FStack.Top>StartTop do FStack.Pop;

  // execute function
  {$IFDEF VerboseCTCfgScript}
  debugln(['TCTConfigScriptEngine.RunFunction FunctionName="',GetAtom(FunctionName),'" Parameter=',dbgs(PCTCfgScriptVariable(@Value))]);
  {$ENDIF}
  case UpChars[FunctionName^] of
  'I':
    if CompareIdentifiers(FunctionName,'int64')=0 then
      MakeCTCSVariableInt64(@Value)
    else if CompareIdentifiers(FunctionName,'integer')=0 then
      MakeCTCSVariableInteger(@Value);
  'S':
    if CompareIdentifiers(FunctionName,'string')=0 then
      MakeCTCSVariableString(@Value);
  end;

  // put result on stack as operand
  {$IFDEF VerboseCTCfgScript}
  debugln(['TCTConfigScriptEngine.RunFunction FunctionName="',GetAtom(FunctionName),'" Result=',dbgs(PCTCfgScriptVariable(@Value))]);
  {$ENDIF}
  FStack.Push(ctcssOperand,FunctionName);
  SetCTCSVariableValue(@Value,FStack.TopItemOperand);

  Result:=true;
end;

procedure TCTConfigScriptEngine.PushStringConstant;
var
  Operand: PCTCfgScriptVariable;

  procedure Add(p: PChar; Count: integer);
  var
    OldLen: LongInt;
    NewLen: Integer;
  begin
    if Count=0 then exit;
    OldLen:=Operand^.StrLen;
    NewLen:=OldLen+Count;
    ReAllocMem(Operand^.StrStart,NewLen+1);
    System.Move(p^,Operand^.StrStart[OldLen],Count);
    Operand^.StrLen:=NewLen;
    Operand^.StrStart[NewLen]:=#0;
  end;

var
  p: PChar;
  StartPos: PChar;
  i: Integer;
  c: char;
begin
  FStack.Push(ctcssOperand,AtomStart);
  Operand:=FStack.TopItemOperand;
  Operand^.ValueType:=ctcsvString;
  Operand^.StrLen:=0;
  Operand^.StrStart:=nil;
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
            AddError(ctsCharacterConstantOutOfRange);
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
        while not (p^ in ['''',#10,#13,#0]) do
          inc(p);
        if p^<>'''' then
          AddError('missing end apostrophe of string constant');
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
  Item:=FStack.TopItem;
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
        // decimal or float
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
    Item^.Operand.StrStart:=GetMem(l+1);
    System.Move(AtomStart^,Item^.Operand.StrStart^,l);
    Item^.Operand.StrStart[l]:=#0;
  end;
  ExecuteStack(1);
end;

procedure TCTConfigScriptEngine.PushBooleanValue(b: boolean);
var
  Operand: PCTCfgScriptVariable;
begin
  FStack.Push(ctcssOperand,AtomStart);
  Operand:=FStack.TopItemOperand;
  Operand^.ValueType:=ctcsvNumber;
  if b then
    Operand^.Number:=1
  else
    Operand^.Number:=0;
  ExecuteStack(1);
end;

function TCTConfigScriptEngine.RunExpression: boolean;
{ Examples:
    A   is false if A=0 or A='0'
    defined(A)
    (A)
    unary operators:

    binary operators:

}
  function OperandAllowed: boolean;
  begin
    case FStack.TopTyp of
    ctcssExpression,ctcssOperator,ctcssRoundBracketOpen:
      Result:=true;
    else
      {$IFDEF VerboseCTCfgScript}
      debugln(['TCTConfigScriptEngine.RunExpression.OperandAllowed no']);
      {$ENDIF}
      AddError(Format(ctsOperatorExpectedButFound, [GetAtom]));
      Result:=false;
    end;
  end;

  function BinaryOperatorAllowed: boolean;
  begin
    case FStack.TopTyp of
    ctcssOperand:
      Result:=true;
    else
      {$IFDEF VerboseCTCfgScript}
      debugln(['TCTConfigScriptEngine.RunExpression.BinaryOperatorAllowed no']);
      {$ENDIF}
      AddError(Format(ctsOperandExpectedButFound, [GetAtom]));
      Result:=false;
    end;
  end;

  function PushBinaryOperator: boolean;
  begin
    Result:=BinaryOperatorAllowed;
    if not Result then begin
      RunExpression:=false;
      exit;
    end;
    ExecuteStack(GetOperatorLevel(AtomStart));
    FStack.Push(ctcssOperator,AtomStart);
  end;

var
  ExprStart: PChar;
  Handled: Boolean;
  Item: PCTCfgScriptStackItem;
  StartTop: LongInt;
  v: PCTCfgScriptVariable;
begin
  Result:=true;
  ExprStart:=AtomStart;
  StartTop:=FStack.Top;
  FStack.Push(ctcssExpression,ExprStart);
  while true do begin
    {$IFDEF VerboseCTCfgScript}
    debugln(['TCTConfigScriptEngine.RunExpression Atom=',GetAtom]);
    {$ENDIF}
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
        ExecuteStack(5);
        if FStack.TopTyp=ctcssRoundBracketOpen then begin
          // empty ()
          AddError(Format(ctsOperandExpectedButFound2, [GetAtom]));
          Result:=false;
          break;
        end else if (FStack.TopTyp=ctcssOperand)
        and (FStack.Top>0) and (FStack.Items[FStack.Top-1].Typ=ctcssRoundBracketOpen)
        then begin
          FStack.Delete(FStack.Top-1);
        end else
          break;
      end;
    '=':
      if not PushBinaryOperator then break;
    '<':
      if (Src-AtomStart=1) or (AtomStart[1] in ['=','>']) then begin
        if not PushBinaryOperator then break;
      end else begin
        AddError(Format(ctsInvalidOperator, [GetAtom]));
        Result:=false;
        break;
      end;
    '>':
      if (Src-AtomStart=1) or (AtomStart[1] in ['=']) then begin
        if not PushBinaryOperator then break;
      end else begin
        AddError(Format(ctsInvalidOperator, [GetAtom]));
        Result:=false;
        break;
      end;
    '+':
      if (Src-AtomStart=1) then begin
        if not PushBinaryOperator then break;
      end else begin
        AddError(Format(ctsInvalidOperator, [GetAtom]));
        Result:=false;
        break;
      end;
    'a'..'z','A'..'Z':
      begin
        // a keyword or an identifier

        {$IFDEF VerboseCTCfgScript}
        debugln(['TCTConfigScriptEngine.RunExpression StackTop=',dbgs(FStack.TopTyp),' Atom=',GetAtom]);
        {$ENDIF}
        // execute
        Handled:=false;
        case UpChars[AtomStart^] of
        'A':
          if CompareIdentifiers('and',AtomStart)=0 then begin
            Handled:=true;
            if not PushBinaryOperator then break;
          end;
        'D':
          case UpChars[AtomStart[1]] of
          'E':
            if CompareIdentifiers('defined',AtomStart)=0 then begin
              Handled:=true;
              if not OperandAllowed then break;
              if not RunDefined(false) then break;
            end;
          'I':
            if CompareIdentifiers('div',AtomStart)=0 then begin
              Handled:=true;
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
            Handled:=true;
            if not OperandAllowed then break;
            PushBooleanValue(false);
          end;
        'M':
          if CompareIdentifiers('mod',AtomStart)=0 then begin
            Handled:=true;
            if not PushBinaryOperator then break;
          end;
        'N':
          if CompareIdentifiers('not',AtomStart)=0 then begin
            Handled:=true;
            if not OperandAllowed then break;
            // Note: no execute, "not" is unary operator for the next operand
            FStack.Push(ctcssOperator,AtomStart);
          end;
        'O':
          if CompareIdentifiers('or',AtomStart)=0 then begin
            Handled:=true;
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
              Handled:=true;
              if not OperandAllowed then break;
              PushBooleanValue(true);
            end;
          end;
        'U':
          if CompareIdentifiers('undefined',AtomStart)=0 then begin
            Handled:=true;
            if not OperandAllowed then break;
            if not RunDefined(true) then break;
          end;
        'X':
          if CompareIdentifiers('xor',AtomStart)=0 then begin
            Handled:=true;
            if not PushBinaryOperator then break;
          end;
        end;
        if (not Handled) and IsKeyWord(AtomStart) then begin
          AddError(Format(ctsUnexpectedKeyword2, [GetAtom]));
          Result:=false;
          break;
        end;
        if (not Handled) then begin
          if not OperandAllowed then break;
          {$IFDEF VerboseCTCfgScript}
          debugln(['TCTConfigScriptEngine.RunExpression ',GetAtom(AtomStart),' ',IsFunction(AtomStart)]);
          {$ENDIF}
          if IsFunction(AtomStart) then begin
            // a function
            if not RunFunction then begin
              Result:=false;
              break;
            end;
          end else begin
            // a variable
            FStack.Push(ctcssOperand,AtomStart);
            Item:=FStack.TopItem;
            v:=Variables.GetVariable(AtomStart);
            if v<>nil then begin
              SetCTCSVariableValue(v,@Item^.Operand);
            end;
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
        AddError(Format(ctsOperandExpectedButFound2, [GetAtom]));
        Result:=false;
      end;
      break;
    end;
    ReadRawNextPascalAtom(Src,AtomStart);
  end;

  if Result then begin
    if not ExecuteStack(10) then
      Result:=false;
    if FStack.Top=StartTop+1 then begin
      // empty expression
      AddError(Format(ctsOperandExpectedButFound2, [GetAtom]));
    end else if (FStack.TopTyp<>ctcssOperand) or (FStack.Top<>StartTop+2) then begin
      // unfinished expression
      if FStack.TopTyp in [ctcssOperator,ctcssRoundBracketOpen]
      then
        AddError(Format(ctsOperandExpectedButFound2, [GetAtom]))
      else
        AddError(Format(ctsOperatorExpectedButFound2, [GetAtom]));
      Result:=false;
    end
    else if Result then begin
      // success
      // delete ctcssExpression and keep the operand
      FStack.Delete(FStack.Top-1);
      Item:=FStack.TopItem;
      inc(StartTop);
      {$IFDEF VerboseCTCfgScript}
      debugln(['TCTConfigScriptEngine.RunExpression Result="',dbgs(PCTCfgScriptVariable(@Item^.Operand)),'" ']);
      {$ENDIF}
    end;
  end;

  // clean up stack
  while (FStack.Top>StartTop) do FStack.Pop;
end;

function TCTConfigScriptEngine.IsKeyWord(P: PChar): boolean;
begin
  Result:=false;
  if p=nil then exit;
  case UpChars[p^] of
  'A':
    if CompareIdentifiers('and',p)=0 then exit(true);
  'B':
    if CompareIdentifiers('begin',p)=0 then exit(true);
  'C':
    if CompareIdentifiers('case',p)=0 then exit(true);
  'D':
    case UpChars[p[1]] of
    'E':
      if CompareIdentifiers('defined',p)=0 then exit(true);
    'I':
      if CompareIdentifiers('div',p)=0 then exit(true);
    end;
  'E':
    case UpChars[p[1]] of
    'L':
      if CompareIdentifiers('else',p)=0 then exit(true);
    'N':
      if CompareIdentifiers('end',p)=0 then exit(true);
    end;
  'F':
    case UpChars[p[1]] of
    'A':
      if CompareIdentifiers('false',p)=0 then exit(true);
    'U':
      if CompareIdentifiers('function',p)=0 then exit(true);
    end;
  'I':
    case UpChars[p[1]] of
    'F':
      if CompareIdentifiers('if',p)=0 then exit(true);
    'N':
      if (CompareIdentifiers('in',p)=0) then exit(true)
    end;
  'M':
    if CompareIdentifiers('mod',p)=0 then exit(true);
  'N':
    if CompareIdentifiers('not',p)=0 then exit(true);
  'O':
    case UpChars[p[1]] of
    'F':
      if CompareIdentifiers('of',p)=0 then exit(true);
    'R':
      if CompareIdentifiers('or',p)=0 then exit(true);
    end;
  'P':
    if CompareIdentifiers('procedure',p)=0 then exit(true);
  'S':
    case UpChars[p[1]] of
    'H':
      case UpChars[p[2]] of
      'L':
        if CompareIdentifiers('shl',p)=0 then exit(true);
      'R':
        if CompareIdentifiers('shr',p)=0 then exit(true);
      end;
    end;
  'T':
    case UpChars[p[1]] of
    'H':
      if CompareIdentifiers('then',p)=0 then exit(true);
    'R':
      if CompareIdentifiers('true',p)=0 then exit(true);
    end;
  'X':
    if CompareIdentifiers('xor',p)=0 then exit(true);
  'U':
    if CompareIdentifiers('undefined',p)=0 then exit(true);
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
  LeftOperandItem: PCTCfgScriptStackItem;
  OperandsEqual: boolean;
  LeftIsLowerThanRight: boolean;

  procedure ErrorInvalidOperator;
  begin
    raise Exception.Create('TCTConfigScriptEngine.ExecuteStack invalid operator: '+GetAtom(OperatorItem^.StartPos));
  end;

begin
  Result:=true;
  repeat
    {$IFDEF VerboseCTCfgScript}
    WriteDebugReportStack('ExecuteStack MaxLevel='+dbgs(MaxLevel));
    {$ENDIF}
    if (FStack.TopTyp<>ctcssOperand) or (FStack.Top<=0) then
      exit;
    OperatorItem:=@FStack.Items[FStack.Top-1];
    if (OperatorItem^.Typ<>ctcssOperator)
    or (GetOperatorLevel(OperatorItem^.StartPos)>MaxLevel) then
      exit;
    OperandItem:=FStack.TopItem;

    // execute operator
    Typ:=AtomToCTCfgOperator(OperatorItem^.StartPos);
    {$IFDEF VerboseCTCfgScript}
    debugln(['TCTConfigScriptEngine.ExecuteStack execute operator "',GetAtom(OperatorItem^.StartPos),'" Typ=',dbgs(Typ)]);
    {$ENDIF}
    case Typ of

    ctcsoNot:
      begin
        b:=CTCSVariableIsTrue(@OperandItem^.Operand);
        FStack.Pop(2);
        PushBooleanValue(not b);
      end;

    ctcsoAnd,ctcsoOr,ctcsoXOr:
      begin
        b:=CTCSVariableIsTrue(@OperandItem^.Operand);
        FStack.Pop(2);
        if (FStack.Top>=0) then begin
          OperandItem:=FStack.TopItem;
          case Typ of
          ctcsoAnd: b:=b and CTCSVariableIsTrue(@OperandItem^.Operand);
          ctcsoOr:  b:=b or CTCSVariableIsTrue(@OperandItem^.Operand);
          ctcsoXOr: b:=b xor CTCSVariableIsTrue(@OperandItem^.Operand);
          end;
          FStack.Pop;
        end;
        PushBooleanValue(b);
      end;

    ctcsoEqual, ctcsoNotEqual, ctcsoLowerThan, ctcsoLowerOrEqualThan,
    ctcsoGreaterThan, ctcsoGreaterOrEqualThan:
      begin
        b:=false;
        if (FStack.Top>=2) then begin
          LeftOperandItem:=@FStack.Items[FStack.Top-2];
          if not CompareCTCSVariables(@LeftOperandItem^.Operand,@OperandItem^.Operand,
                               OperandsEqual,LeftIsLowerThanRight)
          then begin
            b:=false;
          end else begin
            case Typ of
            ctcsoEqual:
              b:=OperandsEqual;
            ctcsoNotEqual:
              b:=not OperandsEqual;
            ctcsoLowerThan:
              b:=(not OperandsEqual) and LeftIsLowerThanRight;
            ctcsoLowerOrEqualThan:
              b:=OperandsEqual or LeftIsLowerThanRight;
            ctcsoGreaterThan:
              b:=(not OperandsEqual) and not LeftIsLowerThanRight;
            ctcsoGreaterOrEqualThan:
              b:=OperandsEqual or not LeftIsLowerThanRight;
            end;
          end;
          {$IFDEF VerboseCTCfgScript}
          debugln(['TCTConfigScriptEngine.ExecuteStack ',GetCTCSVariableAsString(@LeftOperandItem^.Operand),' ',GetCTCSVariableAsString(@OperandItem^.Operand),' Equal=',OperandsEqual,' LT=',LeftIsLowerThanRight,' Result=',Result]);
          {$ENDIF}
          FStack.Pop(3);
        end else begin
          FStack.Pop(2);
        end;
        PushBooleanValue(b);
      end;

    ctcsoPlus:
      begin
        if (FStack.Top>=2) then begin
          LeftOperandItem:=@FStack.Items[FStack.Top-2];
          // add right operand to left operand on stack
          AddCTCSVariables(@OperandItem^.Operand,@LeftOperandItem^.Operand);
          // remove right operand and +
          FStack.Pop(2);
        end else begin
          // unary operator
          // just remove the +
          FStack.Delete(FStack.Top-1);
        end;
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

function TCTConfigScriptEngine.IsFunction(P: PChar): boolean;
begin
  Result:=false;
  if p=nil then exit;
  case UpChars[p^] of
  'I':
    if (CompareIdentifiers(p,'integer')=0)
    or (CompareIdentifiers(p,'int64')=0)
    then exit(true);
  'S':
    if CompareIdentifiers(p,'string')=0 then exit(true);
  end;
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
var
  Err: TCTCfgScriptError;
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

  try
    // execute all statements
    ReadRawNextPascalAtom(Src,AtomStart);
    while Src^<>#0 do begin
      RunStatement(false);
      ReadRawNextPascalAtom(Src,AtomStart);
    end;
  except
    on E: Exception do begin
      // too many errors
      if ErrorCount=0 then begin
        Err:=TCTCfgScriptError.Create(E.Message);
        FErrors.Add(Err);
      end;
    end;
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
begin
  Err:=Errors[Index];
  Result:='Error: ';
  if Err.Line>0 then
    Result:=Result+'('+IntToStr(Err.Line)+','+IntToStr(Err.Column)+') ';
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
  {$IFDEF CheckCTCfgVars}
  CheckOperands;
  {$ENDIF}
end;

procedure TCTCfgScriptStack.Pop(Count: integer);

  procedure RaiseTooManyPop;
  begin
    raise Exception.Create('TCTCfgScriptStack.Pop too many pop');
  end;

var
  Item: PCTCfgScriptStackItem;
begin
  if Top<Count-1 then
    RaiseTooManyPop;
  while Count>0 do begin
    Item:=@Items[Top];
    ClearCTCSVariable(@Item^.Operand);
    if Item^.Operand.Name<>nil then
      ReAllocMem(Item^.Operand.Name,0);
    dec(Top);
    if Top>=0 then
      TopTyp:=Items[Top].Typ
    else
      TopTyp:=ctcssNone;
    dec(Count);
  end;
  {$IFDEF CheckCTCfgVars}
  CheckOperands;
  {$ENDIF}
end;

procedure TCTCfgScriptStack.Delete(Index: integer);
var
  Item: PCTCfgScriptStackItem;
begin
  if (Index<0) or (Index>Top) then exit;
  Item:=@Items[Index];
  ClearCTCSVariable(@Item^.Operand);
  if Item^.Operand.Name<>nil then
    ReAllocMem(Item^.Operand.Name,0);
  if Index<Top then begin
    System.Move(Items[Index+1],Items[Index],SizeOf(TCTCfgScriptStackItem)*(Top-Index));
    Item:=@Items[Top];
    Item^.Typ:=ctcssNone;
    FillByte(Item^.Operand,SizeOf(Item^.Operand),0);
  end;
  dec(Top);
  {$IFDEF CheckCTCfgVars}
  CheckOperands;
  {$ENDIF}
end;

function TCTCfgScriptStack.TopItem: PCTCfgScriptStackItem;
begin
  if Top<0 then
    Result:=nil
  else
    Result:=@Items[Top];
end;

function TCTCfgScriptStack.TopItemOperand: PCTCfgScriptVariable;
begin
  if Top<0 then
    Result:=nil
  else
    Result:=@Items[Top].Operand;
end;

{$IFDEF CheckCTCfgVars}
procedure TCTCfgScriptStack.CheckOperands;
var
  i: Integer;
begin
  for i:=0 to Top do
    CheckCTCSVariable(@Items[Top].Operand);
end;
{$ENDIF}

{ TCTCfgScriptError }

constructor TCTCfgScriptError.Create(const aMsg: string; aPos, aLine,
  aCol: integer);
begin
  Msg:=aMsg;
  Position:=aPos;
  Line:=aLine;
  Column:=aCol;
end;

constructor TCTCfgScriptError.Create(const aMsg: string);
begin
  Msg:=aMsg;
  Position:=-1;
  Line:=0;
  Column:=0;
end;

end.

