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

{ $DEFINE VerboseExprEval}

interface

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, KeyWordFuncLists, FileProcs, LazDbgLog;

const
  ExternalMacroStart = '#';

//----------------------------------------------------------------------------
// compiler switches
const
  CompilerSwitchesNames: array['A'..'Z'] of shortstring=(
     'ALIGN'          // A align fields
    ,'BOOLEVAL'       // B complete boolean evaluation
    ,'ASSERTIONS'     // C generate code for assertions
    ,'DEBUGINFO'      // D generate debuginfo (debug lines), OR: $description 'text'
    ,'EXTENSION'      // E output file extension
    ,''               // F
    ,'IMPORTEDDATA'   // G
    ,'LONGSTRINGS'    // H String=AnsiString
    ,'IOCHECKS'       // I generate EInOutError
    ,'WRITEABLECONST' // J writable typed const
    ,''               // K
    ,'LOCALSYMBOLS'   // L generate local symbol information (debug, requires $D+)
    ,'TYPEINFO'       // M allow published members OR $M minstacksize,maxstacksize
    ,''               // N
    ,'OPTIMIZATION'   // O enable safe optimizations (-O1)
    ,'OPENSTRINGS'    // P deprecated Delphi directive
    ,'OVERFLOWCHECKS' // Q or $OV
    ,'RANGECHECKS'    // R
    ,''               // S
    ,'TYPEADDRESS'    // T enabled: @variable gives typed pointer, otherwise untyped pointer
    ,'SAFEDIVIDE'     // U
    ,'VARSTRINGCHECKS'// V strict shortstring checking, e.g. cannot pass shortstring[3] to shortstring
    ,'STACKFRAMES'    // W always generate stackframes (debugging)
    ,'EXTENDEDSYNTAX' // X deprecated Delphi directive
    ,'REFERENCEINFO'  // Y store for each identifier the declaration location
    ,''               // Z
    );

type
  TOnValuesChanged = procedure of object;
  TOnGetSameString = procedure(var s: string) of object;
  ArrayOfAnsiString = ^AnsiString;
  
  TEvalOperand = record
    Value: PChar;
    Len: PtrInt;
    Data: array[0..3] of char;
    Free: boolean;
  end;
  PEvalOperand = ^TEvalOperand;

  { TExpressionEvaluator }

  TExpressionEvaluator = class
  private
    FChangeStamp: integer;
    FErrorMsg: string;
    FErrorPos: integer;
    FNames, FValues: ArrayOfAnsiString; // always sorted in FNames and FNames uppercase
    FCount: integer;
    FCapacity: integer;
    OldExpr: string;
    OldCurPos, OldMax, OldAtomStart, OldAtomEnd, OldPriorAtomStart: integer;
    FOnChange: TOnValuesChanged;
    function OldReadTilEndBracket:boolean;
    function CompAtom(const UpperCaseTag:string): boolean;
    function OldReadNextAtom:boolean;
    function EvalAtPos:string;
    function CompareValues(const v1, v2: string): integer;
    function GetVariables(const Name: string): string;
    procedure SetVariables(const Name: string; const Value: string);
    function IndexOfName(VarName: PChar; VarLen: integer; InsertPos: boolean): integer;
    function IndexOfIdentifier(Identifier: PChar; InsertPos: boolean): integer;
    procedure Expand;
  public
    property Variables[const Name: string]: string
       read GetVariables write SetVariables;  default;
    property Count: integer read FCount;
    procedure Undefine(const Name: string);
    function IsDefined(const Name: string): boolean; inline;
    function IsIdentifierDefined(Identifier: PChar): boolean; inline;
    function Equals(AnExpressionEvaluator: TExpressionEvaluator): boolean; reintroduce;
    procedure Assign(SourceExpressionEvaluator: TExpressionEvaluator);
    procedure AssignTo(SL: TStringList);
    function Eval(const Expression: string; AllowExternalMacro: boolean = false):string;
    function EvalPChar(Expression: PChar; ExprLen: PtrInt;
                       out Operand: TEvalOperand; AllowExternalMacro: boolean = false): boolean;// true if expression valid
    function EvalBoolean(Expression: PChar; ExprLen: PtrInt; AllowExternalMacro: boolean = false): boolean;
    function EvalOld(const Expression: string):string;
    property ErrorPosition: integer read FErrorPos write FErrorPos;
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

procedure FreeEvalOperand(var V: TEvalOperand);
procedure ClearEvalOperand(out V: TEvalOperand); inline;
function EvalOperandIsTrue(const V: TEvalOperand): boolean; inline;
function EvalOperandToInt64(const V: TEvalOperand): int64;
function CompareEvalOperand(const Operand: TEvalOperand; Value: PChar): integer;
function CompareNames(Name1: PChar; Name1Len: PtrInt;
                      Name2: PChar; Name2Len: PtrInt): integer;
function CompareNames(const Name1, Name2: string): integer; inline;

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

procedure FreeEvalOperand(var V: TEvalOperand);
begin
  if V.Free then begin
    FreeMem(V.Value);
    V.Free:=false;
    V.Value:=nil;
    V.Len:=0;
  end;
end;

procedure ClearEvalOperand(out V: TEvalOperand); inline;
begin
  V.Free:=false;
  V.Value:=nil;
  V.Len:=0;
end;

function EvalOperandIsTrue(const V: TEvalOperand): boolean; inline;
begin
  Result:=not ((V.Len=1) and (V.Value^='0'));
end;

function EvalOperandToInt64(const V: TEvalOperand): int64;
var
  p: PChar;
  l: PtrInt;
  Negated: Boolean;
  c: Char;
begin
  Result:=0;
  p:=V.Value;
  l:=V.Len;
  if l=0 then exit;
  if p^='-' then begin
    Negated:=true;
    inc(p);
    dec(l);
  end else
    Negated:=false;
  if p^='$' then begin
    // hex number
    if l<15 then begin
      while l>0 do begin
        c:=p^;
        case c of
        '0'..'9': Result:=Result*16+ord(p^)-ord('0');
        'a'..'f': Result:=Result*16+ord(p^)-ord('a')+10;
        'A'..'Z': Result:=Result*16+ord(p^)-ord('A')+10;
        else
          break;
        end;
        inc(p);
        dec(l);
      end;
    end else begin
      try
        while l>0 do begin
          c:=p^;
          case c of
          '0'..'9': Result:=Result*16+ord(p^)-ord('0');
          'a'..'f': Result:=Result*16+ord(p^)-ord('a')+10;
          'A'..'Z': Result:=Result*16+ord(p^)-ord('A')+10;
          else
            break;
          end;
          inc(p);
          dec(l);
        end;
      except
      end;
    end;
  end else begin
    // decimal number
    if l<15 then begin
      while l>0 do begin
        c:=p^;
        if c in ['0'..'9'] then
          Result:=Result*10+ord(c)-ord('0')
        else
          break;
        inc(p);
        dec(l);
      end;
    end else begin
      try
        while l>0 do begin
          c:=p^;
          if c in ['0'..'9'] then
            Result:=Result*10+ord(c)-ord('0')
          else
            break;
          inc(p);
          dec(l);
        end;
      except
      end;
    end;
  end;
  if Negated then Result:=-Result;
end;

procedure SetOperandValueStringConst(var V: TEvalOperand;
  StartPos, EndPos: PChar);
var
  l: PtrInt;
  p: PChar;
  DstPos: PChar;
begin
  l:=0;
  p:=StartPos;
  if p^<>'''' then begin
    if V.Free then FreeEvalOperand(V);
    V.Len:=0;
    V.Value:=nil;
    exit;
  end;
  inc(p);
  while p<EndPos do begin
    if p^='''' then begin
      inc(p);
      if (p^<>'''') or (p=EndPos) then break;
    end;
    inc(p);
    inc(l);
  end;
  if l<5 then begin
    // short string
    if V.Free then FreeEvalOperand(V);
    V.Value:=@V.Data[0];
  end else begin
    // big string
    if V.Free then
      ReAllocMem(V.Value,l)
    else begin
      Getmem(V.Value,l);
      V.Free:=true;
    end;
  end;
  V.Len:=l;
  // copy content
  p:=StartPos+1;
  DstPos:=V.Value;
  while p<EndPos do begin
    if p^='''' then begin
      inc(p);
      if (p^<>'''') or (p=EndPos) then break;
    end;
    DstPos^:=p^;
    inc(p);
    inc(DstPos);
  end;
end;

procedure SetOperandValueChar(var V: TEvalOperand; const c: Char);
begin
  if V.Free then FreeEvalOperand(V);
  V.Data[0]:=c;
  V.Value:=@V.Data[0];
  V.Len:=1;
end;

procedure SetOperandValueConst(var V: TEvalOperand; const p: PChar);
begin
  if V.Free then FreeEvalOperand(V);
  V.Len:=strlen(p);
  V.Value:=p;
end;

procedure SetOperandValueInt64(var V: TEvalOperand; i : int64);
const
  HexChrs: array[0..15] of char = '0123456789ABCDEF';
var
  j: Integer;
  k: Integer;
  i2: Int64;
begin
  if (i>=-999) and (i<=9999) then begin
    // small number => save in data
    if V.Free then FreeEvalOperand(V);
    V.Value:=@V.Data[0];
    V.Len:=0;
    if i<0 then begin
      // sign
      V.Data[0]:='-';
      inc(V.Len);
      i:=-i;
    end;
    if i<10 then
      j:=1
    else if i<100 then
      j:=2
    else if i<1000 then
      j:=3
    else
      j:=4;
    inc(V.Len,j);
    k:=V.Len-1;
    repeat
      V.Data[k]:=HexChrs[i mod 10];
      dec(j);
      if j=0 then break;
      i:=i div 10;
      dec(k);
    until false;
  end else begin
    // big number => save as hex number
    // calculate needed mem
    i2:=i;
    j:=1; // $
    if i2<0 then begin
      i2:=-i2;
      inc(j);
    end;
    while i2>0 do begin
      i2:=i2 shr 4;
      inc(j);
    end;
    V.Len:=j;
    // allocate mem
    if V.Free then begin
      ReAllocMem(V.Value,j);
    end else begin
      V.Free:=true;
      Getmem(V.Value,j);
    end;
    // write number
    if i<0 then i:=-i;
    while i>0 do begin
      i:=i shr 4;
      dec(j);
      V.Value[j]:=HexChrs[i and $f];
    end;
    // write $
    dec(j);
    V.Value[j]:='$';
    // write minus sign
    if j=0 then
      V.Value[j]:='-';
  end;
end;

function CompareEvalOperand(const Operand: TEvalOperand; Value: PChar): integer;
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

function OperandsAreEqual(const Op1, Op2: TEvalOperand): boolean;
var
  i: Integer;
begin
  Result:=false;
  if Op1.Len<>Op2.Len then exit;
  i:=Op1.Len-1;
  while i>=0 do begin
    if Op1.Value[i]<>Op2.Value[i] then exit;
    dec(i);
  end;
  Result:=true;
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
  if (OldAtomEnd>OldMax+1) then begin
    Result:=false;  exit;
  end;
  len:=OldAtomEnd-OldAtomStart;
  if length(UpperCaseTag)<>len then begin
    Result:=false;  exit;
  end;
  for a:=1 to len do begin
    if (UpChars[OldExpr[OldAtomStart+a-1]]<>UpperCaseTag[a]) then begin
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

function TExpressionEvaluator.EvalOld(const Expression: string): string;
//  1 = true
//  0 = syntax error
// -1 = false
var s:string;
begin
  OldExpr:=Expression;
  OldMax:=length(OldExpr);
  OldCurPos:=1;
  OldAtomStart:=-1;  OldAtomEnd:=-1;  OldPriorAtomStart:=-1;
  FErrorPos:=-1;
  s:=EvalAtPos;
  if FErrorPos>=0 then begin
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
    if (not OldReadNextAtom) then exit;
    inc(AtomCount);
    c:=OldExpr[OldAtomStart];
    if IsWordChar[c] then begin
      // identifier or keyword
      if (CompAtom('AND')) then begin
        if (Result='') then FErrorPos:=OldCurPos
        else if (Result<>'0') then begin
          // true AND ...
          Result:=EvalAtPos();
          if FErrorPos>=0 then exit;
          if (Result='') then FErrorPos:=OldCurPos;
        end;
        exit;
      end else if (CompAtom('OR')) then begin
        if (Result='0') then begin
          // false OR ...
          Result:=EvalAtPos();
          if FErrorPos>=0 then exit;
          if (Result='') then FErrorPos:=OldCurPos;
        end else if (AtomCount<=1) then FErrorPos:=OldCurPos;
        exit;
      end else if (CompAtom('XOR')) then begin
        if (Result='') then begin
          FErrorPos:=OldCurPos;  exit;
        end;
        r:=Result;
        // true/false XOR ...
        Result:=EvalAtPos();
        if FErrorPos>=0 then exit;
        if (Result='') then begin
          FErrorPos:=OldCurPos;  exit;
        end;
        if (r='0') then begin
          if (Result='0') then Result:='0' else Result:='1';
        end else begin
          if (Result='0') then Result:='1' else Result:='0';
        end;
        exit;
      end else if (CompAtom('NOT')) then begin
        Result:=EvalAtPos();
        if FErrorPos>=0 then exit;
        // Note: for Delphi compatibility: "IF not UndefinedVariable" is valid
        if (Result='0') then Result:='1'
        else Result:='0';
        exit;
      end else if (CompAtom('DEFINED')) then begin
        // read DEFINED(identifier) or defined identifier
        if (Result<>'') or (not OldReadNextAtom) then begin
          FErrorPos:=OldCurPos;
          exit;
        end;
        HasBracket:=CompAtom('(');
        if HasBracket and (not OldReadNextAtom) then begin
          FErrorPos:=OldCurPos;
          exit;
        end;
        if IsDefined(copy(OldExpr,OldAtomStart,OldAtomEnd-OldAtomStart)) then
          Result:='1'
        else
          Result:='0';
        if HasBracket then begin
          if (not OldReadNextAtom) or (not CompAtom(')')) then begin
            FErrorPos:=OldCurPos;
            exit;
          end;
        end;
      end else if (CompAtom('DECLARED')) then begin
        // read DECLARED(identifier)
        if (Result<>'') or (not OldReadNextAtom) or (CompAtom('(')=false)
        or (not OldReadNextAtom) then begin
          FErrorPos:=OldCurPos;
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
        if (not OldReadNextAtom) or (not CompAtom(')')) then begin
          FErrorPos:=OldCurPos;
          exit;
        end;
      end else if (CompAtom('UNDEFINED')) then begin
        // read UNDEFINED(identifier) or undefined identifier
        if (Result<>'') or (not OldReadNextAtom) then begin
          FErrorPos:=OldCurPos;
          exit;
        end;
        HasBracket:=CompAtom('(');
        if HasBracket and (not OldReadNextAtom) then begin
          FErrorPos:=OldCurPos;
          exit;
        end;
        Result:=Variables[copy(OldExpr,OldAtomStart,OldAtomEnd-OldAtomStart)];
        if Result<>'' then
          Result:='0'
        else
          Result:='1';
        if HasBracket then begin
          if (not OldReadNextAtom) or (not CompAtom(')')) then begin
            FErrorPos:=OldCurPos;
            exit;
          end;
        end;
      end else begin
        // Identifier
        if (Result<>'') then begin
          FErrorPos:=OldCurPos;
          exit;
        end else
          Result:=Variables[copy(OldExpr,OldAtomStart,OldAtomEnd-OldAtomStart)];
      end;
    end else if IsNumberBeginChar[c] then begin
      // number
      if (Result<>'') then begin
        FErrorPos:=OldCurPos;  exit;
      end else Result:=copy(OldExpr,OldAtomStart,OldAtomEnd-OldAtomStart);
    end else if c='''' then begin
      Result:=copy(OldExpr,OldAtomStart+1,OldAtomEnd-OldAtomStart-2);
    end else begin
      // operator
      case c of
      ')':exit;
      '(':begin
          OldPos:=OldAtomStart;
          // eval in brackets
          Result:=EvalAtPos();
          if FErrorPos>=0 then exit;
          // go behind brackets
          OldCurPos:=OldPos;
          if (not OldReadTilEndBracket) then exit;
          inc(OldCurPos);
        end;
      '=','>','<':begin
          o1:=c;
          if OldAtomEnd=OldAtomStart+1 then begin
            r:=EvalAtPos();
            if FErrorPos>=0 then exit;
            case o1 of
            '=':if CompareValues(Result,r)=0 then Result:='1' else Result:='0';
            '>':if CompareValues(Result,r)=1 then Result:='1' else Result:='0';
            '<':if CompareValues(Result,r)=-1 then Result:='1' else Result:='0';
            end;
          end else begin
            o2:=OldExpr[OldAtomStart+1];
            r:=EvalAtPos();
            if FErrorPos>=0 then exit;
            if o1='<' then begin
              if o2='>' then begin
                if CompareValues(Result,r)<>0 then Result:='1' else Result:='0';
              end else if o2='=' then begin
                if CompareValues(Result,r)<=0 then Result:='1' else Result:='0';
              end else FErrorPos:=OldAtomStart;
            end else if o1='>' then begin
              if o2='=' then begin
                if CompareValues(Result,r)>=0 then Result:='1' else Result:='0';
              end else FErrorPos:=OldAtomStart;
            end else FErrorPos:=OldAtomStart;
          end;
          exit;
        end;
      '!':
        begin
          Result:=EvalAtPos();
          if FErrorPos>=0 then exit;
          if (Result='0') then Result:='1'
          else if (Result='') then FErrorPos:=OldCurPos
          else Result:='0';
          exit;
        end;
      else
        begin
          FErrorPos:=OldCurPos;
        end;
      end;
    end;
  until (FErrorPos>=0);
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

function TExpressionEvaluator.IndexOfName(VarName: PChar; VarLen: integer;
  InsertPos: boolean): integer;
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
    cmp:=CompareNames(VarName,VarLen,PChar(FNames[m]),length(FNames[m]));
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
  i:=IndexOfName(PChar(Name),length(Name),false);
  if (i>=0) then
    Result:=FValues[i]
  else 
    Result:='';
end;

function TExpressionEvaluator.IsDefined(const Name: string): boolean;
begin
  Result:=IndexOfName(PChar(Name),length(Name),false)>=0;
end;

function TExpressionEvaluator.IsIdentifierDefined(Identifier: PChar): boolean;
begin
  Result:=IndexOfIdentifier(Identifier,false)>=0;
end;

function TExpressionEvaluator.OldReadNextAtom: boolean;
var c,o1,o2:char;
begin
  OldPriorAtomStart:=OldAtomStart;
  while (OldCurPos<=OldMax) do begin
    c:=OldExpr[OldCurPos];
    if (c<=' ') then inc(OldCurPos)
    else if IsWordChar[c] then begin
      // Identifier
      OldAtomStart:=OldCurPos;
      repeat
        inc(OldCurPos);
      until (OldCurPos>OldMax) or (not IsIdentifierChar[OldExpr[OldCurPos]]);
      OldAtomEnd:=OldCurPos;
      Result:=true;
      exit;
    end else if IsNumberBeginChar[c] then begin
      // Number
      OldAtomStart:=OldCurPos;
      repeat
        inc(OldCurPos);
      until (OldCurPos>OldMax) or (IsNumberChar[OldExpr[OldCurPos]]=false);
      OldAtomEnd:=OldCurPos;
      Result:=true;
      exit;
    end else if c='''' then begin
      // string
      OldAtomStart:=OldCurPos;
      repeat
        inc(OldCurPos);
        if OldExpr[OldCurPos]='''' then begin
          inc(OldCurPos);
          OldAtomEnd:=OldCurPos;
          Result:=true;
          exit;
        end;
        if OldCurPos>OldMax then begin
          OldAtomEnd:=OldCurPos;
          Result:=false;
          exit;
        end;
      until (OldCurPos>OldMax);
    end else begin
      // Symbol
      OldAtomStart:=OldCurPos;
      inc(OldCurPos);
      if (OldCurPos<=OldMax) then begin
        o1:=c;
        o2:=OldExpr[OldCurPos];
        if ((o2='=') and ((o1='<') or (o1='>')))
        or ((o1='<') and (o2='>'))
        then inc(OldCurPos);
      end;
      OldAtomEnd:=OldCurPos;
      Result:=true;
      exit;
    end;
  end;
  Result:=false;
end;

function TExpressionEvaluator.OldReadTilEndBracket: boolean;
// true = end bracket found
// false = not found
var lvl:integer;
begin
  lvl:=0;
  while (OldCurPos<=OldMax) do begin
    if (OldExpr[OldCurPos]='(') then
      inc(lvl)
    else if (OldExpr[OldCurPos]=')') then begin
      dec(lvl);
      if (lvl=0) then begin
        Result:=true;  exit;
      end else if (lvl<0) then begin
        FErrorPos:=OldCurPos;
        Result:=true;  exit;
      end;
    end;
    inc(OldCurPos);
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
  i:=IndexOfName(PChar(Name),length(Name),true);
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
  i:=IndexOfName(PChar(Name),length(Name),false);
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

function TExpressionEvaluator.Eval(const Expression: string;
  AllowExternalMacro: boolean): string;
{  0 = false
   else true }
var
  Operand: TEvalOperand;
begin
  if Expression='' then exit('0');
  if not EvalPChar(PChar(Expression),length(Expression),Operand,AllowExternalMacro) then
    Result:=''
  else begin
    SetLength(Result,Operand.Len);
    if Result<>'' then
      System.Move(Operand.Value^,Result[1],length(Result));
  end;
  FreeEvalOperand(Operand);
end;

function TExpressionEvaluator.EvalPChar(Expression: PChar; ExprLen: PtrInt; out
  Operand: TEvalOperand; AllowExternalMacro: boolean): boolean;
{  0 = false
   else true

  brackets ()
  constants: false, true
  unary operators: not, defined, undefined
  binary operators: + - * / < <= = <> => > div mod and or xor shl shr
  functions: defined(), undefined(), declared(), sizeof()=1, option(),
    high(), low()
}
type
  TOperandAndOperator = record
    Operand: TEvalOperand;
    theOperator: PChar;
    OperatorLvl: integer;
  end;
  TExprStack = array[0..3] of TOperandAndOperator;

var
  ExprStack: TExprStack;
  StackPtr: integer; // -1 = empty
  ExprEnd: PChar;
  p, AtomStart: PChar;

  procedure FreeStack;
  begin
    while StackPtr>=0 do begin
      FreeEvalOperand(ExprStack[StackPtr].Operand);
      dec(StackPtr);
    end;
  end;

  function GetAtom: string;
  begin
    Setlength(Result,p-AtomStart);
    if Result<>'' then
      System.Move(AtomStart^,Result[1],length(Result));
  end;

  procedure ReadNextAtom;
  var
    Float: Boolean;
    Exponent: Boolean;
  begin
    // skip space
    while p^ in [' ',#9,#10,#13] do inc(p);
    if p>=ExprEnd then begin
      p:=ExprEnd;
      AtomStart:=p;
      exit;
    end;
    AtomStart:=p;
    case UpChars[p^] of
    'A'..'Z','_':
      begin
        while IsIdentChar[p^] do inc(p);
        if p>ExprEnd then p:=ExprEnd;
      end;
    '0'..'9':
      begin
        inc(p);
        Float:=false;
        Exponent:=false;
        repeat
          case p^ of
          '0'..'9': inc(p);
          '.':
            if Float then
              break
            else begin
              Float:=true;
              inc(p);
            end;
          'e','E':
            if Exponent or (not Float) then
              break
            else begin
              Exponent:=true;
              inc(p);
            end;
          else
            break;
          end;
        until p>=ExprEnd;
      end;
    '$':
      begin
        inc(p);
        while IsHexNumberChar[p^] do inc(p);
      end;
    '>':
      begin
        inc(p);
        case p^ of
        '=','>': inc(p); // >= >>
        end;
      end;
    '<':
      begin
        inc(p);
        case p^ of
        '<','>','=': inc(p); // <> <= <<
        end;
      end;
    '''':
      begin
        inc(p);
        while (p<=ExprEnd) do begin
          if p^='''' then begin
            inc(p);
            if p^<>'''' then break;
            inc(p);
          end else begin
            inc(p);
          end;
        end;
      end;
    else
      inc(p);
    end;
    {$IFDEF VerboseExprEval}
    DebugLn(['ReadNextAtom ',GetAtom]);
    {$ENDIF}
  end;

  procedure Error(NewErrorPos: PChar; const NewErrorMsg: string);
  begin
    if NewErrorPos<>nil then
      FErrorPos:=NewErrorPos-Expression
    else
      FErrorPos:=0;
    ErrorMsg:=NewErrorMsg;
    {$IFDEF VerboseExprEval}
    DebugLn(['Error ',ErrorMsg,' at ',ErrorPosition]);
    {$ENDIF}
  end;

  procedure Error(NewErrorPos: PChar; E: Exception);
  begin
    Error(NewErrorPos,E.Message);
  end;

  procedure ExpressionMissing(NewErrorPos: PChar);
  begin
    Error(NewErrorPos,'expression missing');
  end;

  procedure IdentifierMissing(NewErrorPos: PChar);
  begin
    Error(NewErrorPos,'identifier missing');
  end;

  procedure OperatorMissing(NewErrorPos: PChar);
  begin
    Error(NewErrorPos,'operator missing');
  end;

  procedure CharMissing(NewErrorPos: PChar; c: char);
  begin
    Error(NewErrorPos,c+' missing');
  end;

  procedure BracketMissing(NewErrorPos: PChar);
  begin
    Error(NewErrorPos,'closing bracket without opening bracket');
  end;

  procedure StrExpectedAtPos(NewErrorPos, ExpectedStr: PChar);
  var
    s: string;
    f: string;
  begin
    s:=ExpectedStr;
    if ExprEnd>NewErrorPos then begin
      SetLength(f,ExprEnd-NewErrorPos);
      System.Move(NewErrorPos^,f[1],ExprEnd-NewErrorPos);
      Error(NewErrorPos,'expected '+s+', but found '+f);
    end else begin
      Error(NewErrorPos,'expected '+s);
    end;
  end;

  function ReadTilEndBracket: boolean;
  // start on bracket open
  // ends on bracket close
  var
    BracketLvl: Integer;
    BracketOpen: PChar;
  begin
    BracketOpen:=AtomStart;
    BracketLvl:=0;
    while AtomStart<ExprEnd do begin
      case AtomStart^ of
      '(': inc(BracketLvl);
      ')':
        begin
          dec(BracketLvl);
          if BracketLvl=0 then exit(true);
        end;
      end;
      ReadNextAtom;
    end;
    BracketMissing(BracketOpen);
    Result:=false;
  end;

  function ParseDefinedParams(var Operand: TEvalOperand): boolean;
  // p is behind defined or undefined keyword
  // Operand: '1' or '-1'
  var
    NameStart: PChar;
  begin
    Result:=false;
    ReadNextAtom;
    if AtomStart>=ExprEnd then begin
      IdentifierMissing(AtomStart);
      exit;
    end;
    if IsIdentifierChar[AtomStart^] then begin
      if IsIdentifierDefined(AtomStart) then begin
        SetOperandValueChar(Operand,'1');
      end else begin
        SetOperandValueConst(Operand,'0');
      end;
    end else if AtomStart^='(' then begin
      ReadNextAtom;
      if p=AtomStart then begin
        StrExpectedAtPos(AtomStart,'macro name');
        exit;
      end;
      if AtomStart^=')' then begin
        SetOperandValueConst(Operand,'0');
        exit(true);
      end;
      NameStart:=AtomStart;
      if (AtomStart^=ExternalMacroStart) and AllowExternalMacro then begin
        inc(AtomStart);
        p:=AtomStart;
      end;
      if not IsIdentStartChar[AtomStart^] then begin
        StrExpectedAtPos(AtomStart,'macro name');
        exit;
      end;
      while IsIdentifierChar[p^] do inc(p);
      if IndexOfName(NameStart,p-NameStart,false)>=0 then begin
        SetOperandValueConst(Operand,'1');
      end else begin
        SetOperandValueConst(Operand,'0');
      end;
      ReadNextAtom;
      if AtomStart^<>')' then begin
        StrExpectedAtPos(AtomStart,')');
        exit;
      end;
    end else begin
      StrExpectedAtPos(AtomStart,'macro name');
      exit;
    end;
    Result:=true;
  end;

  function ParseOptionParams(var Operand: TEvalOperand): boolean;
  // p is behind option keyword
  // Operand: '1' or '-1'
  begin
    Result:=false;
    ReadNextAtom;
    if AtomStart>=ExprEnd then begin
      CharMissing(ExprEnd,'(');
      exit;
    end;
    if AtomStart^<>'(' then begin
      StrExpectedAtPos(AtomStart,'(');
      exit;
    end;
    ReadNextAtom;
    if not IsIdentifierChar[AtomStart^] then begin
      StrExpectedAtPos(AtomStart,'option name');
      exit;
    end;
    SetOperandValueChar(Operand,'1');  // ToDo: check the right flag
    ReadNextAtom;
    if AtomStart>=ExprEnd then begin
      CharMissing(ExprEnd,')');
      exit;
    end;
    if AtomStart^<>')' then begin
      StrExpectedAtPos(AtomStart,')');
      exit;
    end;
    Result:=true;
  end;

  function ReadOperand: boolean;
  { Examples:
     Variable
     not Variable
     not not undefined Variable
     defined(Variable)
     !Variable
     unicodestring
     123
     $45
     'Abc'
     (expression)
  }
  var
    i: LongInt;
    BracketStart: PChar;
  begin
    Result:=false;
    if AtomStart>=ExprEnd then exit;
    {$IFDEF VerboseExprEval}
    DebugLn(['ReadOperand ',GetAtom]);
    {$ENDIF}
    case UpChars[AtomStart^] of
    'N':
      if CompareIdentifiers(AtomStart,'NOT')=0 then begin
        // not
        ReadNextAtom;
        if not ReadOperand() then exit;
        if (Operand.Len=1) and (Operand.Value^='0') then begin
          SetOperandValueChar(Operand,'1');
        end else begin
          SetOperandValueChar(Operand,'0');
        end;
        exit(true);
      end;
    'D':
      if CompareIdentifiers(AtomStart,'DEFINED')=0 then begin
        // "defined V" or "defined(V)"
        if not ParseDefinedParams(Operand) then exit;
        exit(true);
      end
      else if CompareIdentifiers(AtomStart,'DECLARED')=0 then begin
        // should check if a pascal identifier is already declared
        // can not do this here => return always true
        if not ParseDefinedParams(Operand) then exit;
        SetOperandValueChar(Operand,'1');
        exit(true);
      end;
    'H':
      if CompareIdentifiers(AtomStart,'HIGH')=0 then begin
        ReadNextAtom;
        if AtomStart^<>'(' then StrExpectedAtPos(AtomStart,'(');
        if not ReadTilEndBracket then exit;
        SetOperandValueChar(Operand,'0');
        exit(true);
      end;
    'L':
      if CompareIdentifiers(AtomStart,'LOW')=0 then begin
        ReadNextAtom;
        if AtomStart^<>'(' then StrExpectedAtPos(AtomStart,'(');
        if not ReadTilEndBracket then exit;
        SetOperandValueChar(Operand,'0');
        exit(true);
      end;
    'O':
      if CompareIdentifiers(AtomStart,'OPTION')=0 then begin
        ReadNextAtom;
        if not ParseOptionParams(Operand) then exit;
        exit(true);
      end;
    'S':
      if CompareIdentifiers(AtomStart,'SIZEOF')=0 then begin
        ReadNextAtom;
        if AtomStart^<>'(' then StrExpectedAtPos(AtomStart,'(');
        if not ReadTilEndBracket then exit;
        SetOperandValueChar(Operand,'1');
        exit(true);
      end;
    'U':
      if CompareIdentifiers(AtomStart,'UNDEFINED')=0 then begin
        // "undefined V" or "undefined(V)"
        if not ParseDefinedParams(Operand) then exit;
        if (Operand.Len=1) and (Operand.Value^='0') then begin
          SetOperandValueChar(Operand,'1');
        end else begin
          SetOperandValueChar(Operand,'0');
        end;
        exit(true);
      end
      else if CompareIdentifiers(AtomStart,'UNICODESTRING')=0 then begin
        // unicodestring
        if IsIdentifierDefined('FPC_HAS_UNICODESTRING') then begin
          SetOperandValueChar(Operand,'1');
        end else begin
          SetOperandValueChar(Operand,'0');
        end;
        exit(true);
      end;
    '!':
      if p-AtomStart=1 then begin
        // not
        ReadNextAtom;
        if not ReadOperand() then exit;
        if (Operand.Len=1) and (Operand.Value^='0') then begin
          SetOperandValueChar(Operand,'1');
        end else begin
          SetOperandValueChar(Operand,'0');
        end;
        exit(true);
      end;
    '0'..'9','$':
      begin
        // number
        if Operand.Free then FreeEvalOperand(Operand);
        Operand.Value:=AtomStart;
        Operand.Len:=p-AtomStart;
        exit(true);
      end;
    '''':
      begin
        SetOperandValueStringConst(Operand,AtomStart,p);
        exit(true);
      end;
    '(':
      begin
        BracketStart:=AtomStart;
        ReadNextAtom;
        if AtomStart>=ExprEnd then exit;
        {$IFDEF VerboseExprEval}
        DebugLn(['ReadOperand BRACKET OPEN']);
        {$ENDIF}
        if not EvalPChar(AtomStart,ExprLen-(AtomStart-Expression),Operand) then
          exit;
        {$IFDEF VerboseExprEval}
        DebugLn(['ReadOperand BRACKET CLOSED => skip bracket']);
        {$ENDIF}
        AtomStart:=BracketStart;
        p:=AtomStart+1;
        if not ReadTilEndBracket then exit;
        exit(true);
      end;
    end;
    if IsIdentStartChar[AtomStart^] then begin
      // identifier => return current value
      i:=IndexOfIdentifier(AtomStart,false);
      if i>=0 then begin
        if Operand.Free then FreeEvalOperand(Operand);
        Operand.Value:=PChar(FValues[i]);
        Operand.Len:=length(FValues[i]);
      end;
      exit(true);
    end;
    // invalid operand
    IdentifierMissing(AtomStart);
  end;

  function ExecuteStack(LowerOrEqualOperatorLvl: integer): boolean;
  var
    Op: PChar;
    Number1: Int64;
    Number2: Int64;
    NumberResult: Int64;
    StackOperand: PEvalOperand;
  begin
    Result:=true;
    while (StackPtr>=0)
    and (ExprStack[StackPtr].OperatorLvl<=LowerOrEqualOperatorLvl) do begin
      try
        // compute stack item
        Op:=ExprStack[StackPtr].theOperator;
        StackOperand:=@ExprStack[StackPtr].Operand;
        {$IFDEF VerboseExprEval}
        DebugLn(['ExecuteStack Operator^=',ExprStack[StackPtr].theOperator^]);
        {$ENDIF}
        case UpChars[Op^] of
        '*': // multiply
          begin
            Number1:=EvalOperandToInt64(StackOperand^);
            Number2:=EvalOperandToInt64(Operand);
            NumberResult:=Number1*Number2;
            SetOperandValueInt64(Operand,NumberResult);
          end;
        '+': // Add
          begin
            Number1:=EvalOperandToInt64(StackOperand^);
            Number2:=EvalOperandToInt64(Operand);
            NumberResult:=Number1+Number2;
            SetOperandValueInt64(Operand,NumberResult);
          end;
        '-': // subtract
          begin
            Number1:=EvalOperandToInt64(StackOperand^);
            Number2:=EvalOperandToInt64(Operand);
            NumberResult:=Number1-Number2;
            SetOperandValueInt64(Operand,NumberResult);
          end;
        '=':
          if OperandsAreEqual(StackOperand^,Operand) then begin
            SetOperandValueChar(Operand,'1');
          end else begin
            SetOperandValueChar(Operand,'0');
          end;
        '<':
          case Op[1] of
          '>': // <>
            if OperandsAreEqual(StackOperand^,Operand) then begin
              SetOperandValueChar(Operand,'0');
            end else begin
              SetOperandValueChar(Operand,'1');
            end;
          '=':
            begin
              // <=
              Number1:=EvalOperandToInt64(StackOperand^);
              Number2:=EvalOperandToInt64(Operand);
              if Number1<=Number2 then
                SetOperandValueChar(Operand,'1')
              else
                SetOperandValueChar(Operand,'0');
            end;
          '<':
            begin
              // <<
              Number1:=EvalOperandToInt64(StackOperand^);
              Number2:=EvalOperandToInt64(Operand);
              NumberResult:=Number1 shl Number2;
              SetOperandValueInt64(Operand,NumberResult);
            end;
          else
            // <
            Number1:=EvalOperandToInt64(StackOperand^);
            Number2:=EvalOperandToInt64(Operand);
            if Number1<Number2 then
              SetOperandValueChar(Operand,'1')
            else
              SetOperandValueChar(Operand,'0');
          end;
        '>':
          case Op[1] of
          '=':
            begin
              // >=
              Number1:=EvalOperandToInt64(StackOperand^);
              Number2:=EvalOperandToInt64(Operand);
              if Number1>=Number2 then
                SetOperandValueChar(Operand,'1')
              else
                SetOperandValueChar(Operand,'0');
            end;
          '>':
            begin
              // >>
              Number1:=EvalOperandToInt64(StackOperand^);
              Number2:=EvalOperandToInt64(Operand);
              NumberResult:=Number1 shr Number2;
              SetOperandValueInt64(Operand,NumberResult);
            end;
          else
            // >
            Number1:=EvalOperandToInt64(StackOperand^);
            Number2:=EvalOperandToInt64(Operand);
            if Number1>Number2 then
              SetOperandValueChar(Operand,'1')
            else
              SetOperandValueChar(Operand,'0');
          end;
        'A': // AND
          begin
            if EvalOperandIsTrue(StackOperand^) and EvalOperandIsTrue(Operand) then
              SetOperandValueChar(Operand,'1')
            else
              SetOperandValueChar(Operand,'0');
          end;
        'D': // DIV
          begin
            Number1:=EvalOperandToInt64(StackOperand^);
            Number2:=EvalOperandToInt64(Operand);
            NumberResult:=Number1 div Number2;
            SetOperandValueInt64(Operand,NumberResult);
          end;
        'M': // MOD
          begin
            Number1:=EvalOperandToInt64(StackOperand^);
            Number2:=EvalOperandToInt64(Operand);
            NumberResult:=Number1 mod Number2;
            SetOperandValueInt64(Operand,NumberResult);
          end;
        'S':
          case UpChars[Op[1]] of
          'H': // SH
            case UpChars[Op[2]] of
            'L': // SHL
              begin
                Number1:=EvalOperandToInt64(StackOperand^);
                Number2:=EvalOperandToInt64(Operand);
                NumberResult:=Number1 shl Number2;
                SetOperandValueInt64(Operand,NumberResult);
              end;
            'R': // SHR
              begin
                Number1:=EvalOperandToInt64(StackOperand^);
                Number2:=EvalOperandToInt64(Operand);
                NumberResult:=Number1 shr Number2;
                SetOperandValueInt64(Operand,NumberResult);
              end;
            end;
          end;
        'O': // OR
          begin
            if EvalOperandIsTrue(StackOperand^) or EvalOperandIsTrue(Operand) then
              SetOperandValueChar(Operand,'1')
            else
              SetOperandValueChar(Operand,'0');
          end;
        'X': // XOR
          begin
            if EvalOperandIsTrue(StackOperand^) xor EvalOperandIsTrue(Operand) then
              SetOperandValueChar(Operand,'1')
            else
              SetOperandValueChar(Operand,'0');
          end;
        end;

      except
        on E: Exception do begin
          Result:=false;
          Error(AtomStart,E);
        end;
      end;
      if not Result then exit;
      FreeEvalOperand(ExprStack[StackPtr].Operand);
      dec(StackPtr);
    end;
  end;

var
  OperatorLvl: Integer;
begin
  p:=Expression;
  Result:=false;
  ClearEvalOperand(Operand);
  if p=nil then begin
    ExpressionMissing(p);
    exit;
  end;
  ExprEnd:=p+ExprLen;
  ReadNextAtom;
  if AtomStart>=ExprEnd then begin
    ExpressionMissing(AtomStart);
    exit;
  end;
  StackPtr:=-1;
  FErrorPos:=-1;
  fErrorMsg:='';
  try
    while AtomStart<ExprEnd do begin
      // read operand
      if not ReadOperand then
        break;
      // read operator
      ReadNextAtom;
      if AtomStart>=ExprEnd then break;
      // level 0: NOT () DEFINED UNDEFINED DECLARED: handled by ReadOperand
      // level 1: * / DIV MOD AND SHL SHR << >>
      // level 2: + - OR XOR
      // level 3: = < > <> >= <=
      OperatorLvl:=0;
      case UpChars[AtomStart^] of
      ')': break;
      '*','/': if p-AtomStart=1 then OperatorLvl:=1;
      '+','-': if p-AtomStart=1 then OperatorLvl:=2;
      '=': if p-AtomStart=1 then OperatorLvl:=3;
      '<': if (p-AtomStart=1)
           or (AtomStart[1] in ['=','>']) then
             OperatorLvl:=3
           else if AtomStart[1]='<' then
             OperatorLvl:=1;
      '>': if (p-AtomStart=1)
           or (AtomStart[1]='=') then
             OperatorLvl:=3
           else if AtomStart[1]='>' then
             OperatorLvl:=1;
      'A':
        if CompareIdentifiers(AtomStart,'AND')=0 then begin
          OperatorLvl:=1;
          if not EvalOperandIsTrue(Operand) then begin
            SetOperandValueChar(Operand,'0');
            break;
          end;
        end;
      'D': if CompareIdentifiers(AtomStart,'DIV')=0 then OperatorLvl:=1;
      'M': if CompareIdentifiers(AtomStart,'MOD')=0 then OperatorLvl:=1;
      'S':
        case UpChars[AtomStart[1]] of
        'H': // SH
          case UpChars[AtomStart[2]] of
          'L': if p-AtomStart=3 then OperatorLvl:=1; // SHL
          'R': if p-AtomStart=3 then OperatorLvl:=1; // SHR
          end;
        end;
      'O':
        case UpChars[AtomStart[1]] of
        'R':
          if p-AtomStart=2 then begin
            OperatorLvl:=2;
            if EvalOperandIsTrue(Operand) then begin
              SetOperandValueChar(Operand,'1');
              break;
            end;
          end;
        end;
      'X': if CompareIdentifiers(AtomStart,'XOR')=0 then OperatorLvl:=2;
      end;
      if OperatorLvl=0 then begin
        OperatorMissing(AtomStart);
        break;
      end;
      if not ExecuteStack(OperatorLvl) then break;
      // push onto stack
      inc(StackPtr);
      ExprStack[StackPtr].Operand:=Operand;
      ExprStack[StackPtr].OperatorLvl:=OperatorLvl;
      ExprStack[StackPtr].theOperator:=AtomStart;
      ClearEvalOperand(Operand);
      ReadNextAtom;
    end;
    if FErrorPos<0 then begin
      Result:=ExecuteStack(4);
    end;
  finally
    // clean up
    FreeStack;
  end;
end;

function TExpressionEvaluator.EvalBoolean(Expression: PChar; ExprLen: PtrInt;
  AllowExternalMacro: boolean): boolean;
var
  Operand: TEvalOperand;
begin
  Result:=EvalPChar(Expression,ExprLen,Operand,AllowExternalMacro)
       and EvalOperandIsTrue(Operand);
  FreeEvalOperand(Operand);
end;

function TExpressionEvaluator.AsString: string;
var TxtLen, i, p: integer;
  s: String;
begin
  TxtLen:=0;
  for i:=0 to FCount-1 do begin
    inc(TxtLen,length(FNames[i])+2);
    s:=FValues[i];
    if s<>'' then
      inc(TxtLen,length(s)+1);
  end;
  Setlength(Result,TxtLen);
  p:=1;
  for i:=0 to FCount-1 do begin
    Move(FNames[i][1],Result[p],length(FNames[i]));
    inc(p,length(FNames[i]));
    s:=FValues[i];
    if length(s)>0 then begin
      Result[p]:='=';
      inc(p);
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
var
  i: Integer;
begin
  DebugLn('[TExpressionEvaluator.WriteDebugReport] ');
  ConsistencyCheck;
  for i:=0 to Count-1 do
    debugln('  ',Items(i));
end;

function TExpressionEvaluator.CalcMemSize(WithNamesAndValues: boolean;
  Original: TExpressionEvaluator): PtrUInt;
var
  i: Integer;
  j: LongInt;
begin
  Result:=PtrUInt(InstanceSize)
    +MemSizeString(OldExpr)
    +SizeOf(Pointer)*PtrUInt(FCount)*2;
  if WithNamesAndValues then begin
    for i:=0 to FCount-1 do begin
      if Original<>nil then begin
        j:=Original.IndexOfName(PChar(FNames[i]),length(FNames[i]),false);
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


initialization
  InternalInit;

end.

