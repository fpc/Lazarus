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
  Classes, SysUtils, BasicCodeTools, AVL_Tree;

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

  { TCTConfigScriptEngine }

  TCTConfigScriptEngine = class
  public
    constructor Create;
    destructor Destroy; override;
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

constructor TCTConfigScriptEngine.Create;
begin

end;

destructor TCTConfigScriptEngine.Destroy;
begin
  inherited Destroy;
end;

end.

