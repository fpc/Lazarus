{ /***************************************************************************
                    compoptsmodes.pas  -  Lazarus IDE unit
                    ---------------------------------------
                Conditional compiler options and build modes.

 ***************************************************************************/

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
    - compiler options depending on build modes and environment
}
unit CompOptsModes;

{$mode objfpc}{$H+}

{$i ide.inc}

interface

uses
  Classes, SysUtils, Laz_XMLCfg, ExprEval;

type
  TCOCNodeType = (
    cocntNone,
    cocntIf,
    cocntIfdef,
    cocntIfNdef,
    cocntElseIf,
    cocntElse,
    cocntEndIf,
    cocntAddValue
  );
  TCOCNodeTypes = set of TCOCNodeType;
  TCOCValueType = (
    cocvtNone,
    cocvtUnitPath,
    cocvtSrcPath,
    cocvtIncPath,
    cocvtObjectPath,
    cocvtLibraryPath,
    cocvtLinkerOptions,
    cocvtCustomOptions
    );
  TCOCValueTypes = set of TCOCValueType;

const
  COCNodeTypeNames: array[TCOCNodeType] of string = (
    'None',
    'If',
    'Ifdef',
    'IfNdef',
    'ElseIf',
    'Else',
    'EndIf',
    'AddValue'
    );
  COCValueTypeNames: array[TCOCValueType] of string = (
    'None',
    'UnitPath',
    'SrcPath',
    'IncPath',
    'ObjectPath',
    'LibraryPath',
    'LinkerOptions',
    'CustomOptions'
    );

type
  TCompOptConditionals = class;

  { TCompOptCondNode }

  TCompOptCondNode = class
  private
    FCount: integer;
    fChilds: TFPList;// list of TCompOptCondNode
    fClearing: Boolean;
    FNodeType: TCOCNodeType;
    FOwner: TCompOptConditionals;
    FParent: TCompOptCondNode;
    FValue: string;
    FValueType: TCOCValueType;
    function GetChilds(Index: integer): TCompOptCondNode;
    procedure SetNodeType(const AValue: TCOCNodeType);
    procedure SetParent(const AValue: TCompOptCondNode);
    procedure InternalAddChild(Child: TCompOptCondNode; Index: integer);
    procedure InternalRemoveChild(Child: TCompOptCondNode);
    procedure SetValue(const AValue: string);
    procedure SetValueType(const AValue: TCOCValueType);
    procedure Changed;
  public
    constructor Create(TheOwner: TCompOptConditionals);
    destructor Destroy; override;
    procedure Clear;
    procedure AddLast(Child: TCompOptCondNode);
    procedure Insert(Child: TCompOptCondNode; Index: integer);
    procedure Move(CurIndex, NewIndex: integer);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                DoSwitchPathDelims: boolean); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string); virtual;
    property NodeType: TCOCNodeType read FNodeType write SetNodeType;
    property ValueType: TCOCValueType read FValueType write SetValueType;
    property Value: string read FValue write SetValue;
    property Parent: TCompOptCondNode read FParent write SetParent;
    property Childs[Index: integer]: TCompOptCondNode read GetChilds; default;
    property Count: integer read FCount;
    property Owner: TCompOptConditionals read FOwner;
  end;

  { TCompOptConditionals }

  TCompOptConditionals = class
  private
    FChangeStamp: integer;
    FEvaluator: TExpressionEvaluator;
    FRoot: TCompOptCondNode;
    function GetValues(const ValueType: TCOCValueType): string;
    procedure SetEvaluator(const AValue: TExpressionEvaluator);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure InvalidateValues;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                DoSwitchPathDelims: boolean); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string); virtual;
    property Values[ValueType: TCOCValueType]: string read GetValues;
    property Evaluator: TExpressionEvaluator read FEvaluator write SetEvaluator;
    property ChangeStamp: integer read FChangeStamp;
    property Root: TCompOptCondNode read FRoot;
    procedure IncreaseChangeStamp; inline;
  end;

function COCNodeTypeNameToType(const s: string): TCOCNodeType;
function COCValueTypeNameToType(const s: string): TCOCValueType;

implementation

function COCNodeTypeNameToType(const s: string): TCOCNodeType;
begin
  for Result:=Low(TCOCNodeType) to High(TCOCNodeType) do
    if SysUtils.CompareText(s,COCNodeTypeNames[Result])=0 then exit;
  Result:=cocntNone;
end;

function COCValueTypeNameToType(const s: string): TCOCValueType;
begin
  for Result:=Low(TCOCValueType) to High(TCOCValueType) do
    if SysUtils.CompareText(s,COCValueTypeNames[Result])=0 then exit;
  Result:=cocvtNone;
end;

{ TCompOptCondNode }

function TCompOptCondNode.GetChilds(Index: integer): TCompOptCondNode;
begin
  Result:=TCompOptCondNode(fChilds[Index]);
end;

procedure TCompOptCondNode.SetNodeType(const AValue: TCOCNodeType);
begin
  if FNodeType=AValue then exit;
  FNodeType:=AValue;
  Changed;
end;

procedure TCompOptCondNode.SetParent(const AValue: TCompOptCondNode);
begin
  if FParent=AValue then exit;
  if FParent<>nil then begin
    FParent.InternalRemoveChild(Self);
    FParent.Changed;
  end;
  FParent:=AValue;
  if FParent<>nil then begin
    FParent.InternalAddChild(Self,Count);
    FParent.Changed;
  end;
end;

procedure TCompOptCondNode.InternalAddChild(Child: TCompOptCondNode;
  Index: integer);
begin
  fChilds.Insert(Index,Child);
end;

procedure TCompOptCondNode.InternalRemoveChild(Child: TCompOptCondNode);
begin
  fChilds.Remove(Child);
end;

procedure TCompOptCondNode.SetValue(const AValue: string);
begin
  if FValue=AValue then exit;
  FValue:=AValue;
  Changed;
end;

procedure TCompOptCondNode.SetValueType(const AValue: TCOCValueType);
begin
  if FValueType=AValue then exit;
  FValueType:=AValue;
  Changed;
end;

procedure TCompOptCondNode.Changed;
begin
  if FOwner<>nil then FOwner.InvalidateValues;
end;

constructor TCompOptCondNode.Create(TheOwner: TCompOptConditionals);
begin
  FOwner:=TheOwner;
  fChilds:=TFPList.Create;
end;

destructor TCompOptCondNode.Destroy;
begin
  Parent:=nil;
  Clear;
  FreeAndNil(FChilds);
  inherited Destroy;
end;

procedure TCompOptCondNode.Clear;
var
  i: Integer;
begin
  fClearing:=true;
  for i:=0 to FChilds.Count-1 do
    TObject(fChilds[i]).Free;
  fChilds.Clear;
  fClearing:=false;
end;

procedure TCompOptCondNode.AddLast(Child: TCompOptCondNode);
begin
  Child.Parent:=nil;
  InternalAddChild(Child,Count);
  Child.FParent:=Self;
end;

procedure TCompOptCondNode.Insert(Child: TCompOptCondNode; Index: integer);
begin
  Child.Parent:=nil;
  InternalAddChild(Child,Index);
  Child.FParent:=Self;
end;

procedure TCompOptCondNode.Move(CurIndex, NewIndex: integer);
begin
  fChilds.Move(CurIndex,NewIndex);
end;

procedure TCompOptCondNode.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; DoSwitchPathDelims: boolean);
var
  NewCount: LongInt;
  i: Integer;
  NewChild: TCompOptCondNode;
begin
  Clear;
  FNodeType:=COCNodeTypeNameToType(XMLConfig.GetValue(Path+'NodeType',''));
  FValueType:=COCValueTypeNameToType(XMLConfig.GetValue(Path+'ValueType',''));
  FValue:=XMLConfig.GetValue(Path+'Value','');
  NewCount:=XMLConfig.GetValue(Path+'ChildCount',0);
  for i:=0 to NewCount-1 do begin
    NewChild:=TCompOptCondNode.Create(Owner);
    AddLast(NewChild);
    NewChild.LoadFromXMLConfig(XMLConfig,Path+'Item'+IntToStr(i)+'/',
                               DoSwitchPathDelims);
  end;
end;

procedure TCompOptCondNode.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  i: Integer;
begin
  XMLConfig.SetDeleteValue(Path+'NodeType',COCNodeTypeNames[NodeType],
                           COCNodeTypeNames[cocntNone]);
  XMLConfig.SetDeleteValue(Path+'ValueType',COCValueTypeNames[ValueType],
                           COCValueTypeNames[cocvtNone]);
  XMLConfig.SetDeleteValue(Path+'Value',Value,'');
  XMLConfig.SetDeleteValue(Path+'ChildCount',Count,0);
  for i:=0 to Count-1 do
    Childs[i].SaveToXMLConfig(XMLConfig,Path+'Item'+IntToStr(i)+'/');
end;

{ TCompOptConditionals }

function TCompOptConditionals.GetValues(const ValueType: TCOCValueType): string;
begin

end;

procedure TCompOptConditionals.SetEvaluator(const AValue: TExpressionEvaluator
  );
begin
  if FEvaluator=AValue then exit;
  FEvaluator:=AValue;
end;

constructor TCompOptConditionals.Create;
begin

end;

destructor TCompOptConditionals.Destroy;
begin
  inherited Destroy;
end;

procedure TCompOptConditionals.Clear;
begin

end;

procedure TCompOptConditionals.InvalidateValues;
begin
  if FChangeStamp<High(Integer) then
    inc(FChangeStamp)
  else
    FChangeStamp:=Low(Integer);
end;

procedure TCompOptConditionals.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; DoSwitchPathDelims: boolean);
begin

end;

procedure TCompOptConditionals.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin

end;

procedure TCompOptConditionals.IncreaseChangeStamp; inline;
begin

end;

end.

