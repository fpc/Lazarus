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

interface

uses
  Classes, SysUtils, BasicCodeTools, AVL_Tree, KeywordFuncLists, FileProcs;

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
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Equals(Vars: TCTCfgScriptVariables): boolean; reintroduce;
    procedure Assign(Source: TCTCfgScriptVariables);
    function GetVariable(const Name: PChar;
                      CreateIfNotExists: Boolean = false): PCTCfgScriptVariable;
  end;

type
  TCTCfgScriptStackItemType = (
    ctcssNone,
    ctcssStatement,
    ctcssIf,
    ctcssIfThen,
    ctcssIfElse,
    ctcssRoundBracketOpen,
    ctcssBegin
    );
const
  ctcssAllStatementStarts = [ctcssNone,ctcssIfThen,ctcssIfElse,ctcssBegin];
type
  TCTCfgScriptStackItem = record
    Typ: TCTCfgScriptStackItemType;
    StartPos: integer;
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
    procedure Push(Typ: TCTCfgScriptStackItemType; StartPos: integer);
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
    function PosToLineCol(p: PChar; out Line, Column: integer): boolean;
    function PosToStr(p: PChar): string;
    function GetErrorStr(Index: integer): string;
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
function CTCSNumberEqualsString(const Number: int64; const P: PChar): boolean; inline;
function CTCSStringToNumber(P: PChar; out Number: int64): boolean;

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

{ TCTCfgScriptVariables }

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

  procedure ErrorUnexpectedAtom;
  begin
    AddError('expected statement, but found '+GetAtom);
  end;

var
  IsKeyword: Boolean;
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
  while true do begin
    ReadRawNextPascalAtom(Src,AtomStart);
    debugln(['TCTConfigScriptEngine.Execute Atom=',GetAtom]);
    case AtomStart^ of
    #0: break;
    'a'..'z','A'..'Z':
      begin
        // identifier or keyword
        IsKeyword:=false;
        case UpChars[AtomStart^] of
        'B':
          if CompareIdentifiers('BEGIN',AtomStart)=0 then begin
            IsKeyword:=true;
          end;
        'E':
          case UpChars[AtomStart[1]] of
          'L':
            if CompareIdentifiers('ELSE',AtomStart)=0 then begin
              IsKeyword:=true;
            end;
          'N':
            if CompareIdentifiers('END',AtomStart)=0 then begin
              IsKeyword:=true;
            end;
          end;
        'I':
          if CompareIdentifiers('IF',AtomStart)=0 then begin
            IsKeyword:=true;
          end;
        'T':
          if CompareIdentifiers('THEN',AtomStart)=0 then begin
            IsKeyword:=true;
          end;
        end;
        if not IsKeyword then begin
          // parse assignment
          debugln(['TCTConfigScriptEngine.Execute Identifier="',GetAtom,'" Variable exists=',Variables.GetVariable(AtomStart)<>nil]);
        end;
      end;
    else
      ErrorUnexpectedAtom;
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
  StartPos: integer);
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

