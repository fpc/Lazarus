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
    fClearing: Boolean;
    FFirstChild: TCompOptCondNode;
    FLastChild: TCompOptCondNode;
    FNextSibling: TCompOptCondNode;
    FNodeType: TCOCNodeType;
    FOwner: TCompOptConditionals;
    FParent: TCompOptCondNode;
    FPrevSibling: TCompOptCondNode;
    FValue: string;
    FValueType: TCOCValueType;
    procedure SetNodeType(const AValue: TCOCNodeType);
    procedure SetParent(const AValue: TCompOptCondNode);
    procedure SetValue(const AValue: string);
    procedure SetValueType(const AValue: TCOCValueType);
    procedure Changed;
  public
    constructor Create(TheOwner: TCompOptConditionals);
    destructor Destroy; override;
    procedure Clear;
    procedure AddLast(Child: TCompOptCondNode);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                DoSwitchPathDelims: boolean); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string); virtual;
    property NodeType: TCOCNodeType read FNodeType write SetNodeType;
    property ValueType: TCOCValueType read FValueType write SetValueType;
    property Value: string read FValue write SetValue;
    property Owner: TCompOptConditionals read FOwner;
    property Parent: TCompOptCondNode read FParent write SetParent;
    property Count: integer read FCount;
    property FirstChild: TCompOptCondNode read FFirstChild;
    property LastChild: TCompOptCondNode read FLastChild;
    property NextSibling: TCompOptCondNode read FNextSibling;
    property PrevSibling: TCompOptCondNode read FPrevSibling;
  end;

  { TCompOptConditionals }

  TCompOptConditionals = class
  private
    FChangeStamp: integer;
    FErrorNode: TCompOptCondNode;
    FEvaluator: TExpressionEvaluator;
    FRoot: TCompOptCondNode;
    FEvaluatorStamp: integer;
    FValuesValid: boolean;
    FValues: array[TCOCValueType] of string;
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
    property ErrorNode: TCompOptCondNode read FErrorNode write FErrorNode;
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
    // ToDo
    FParent.Changed;
  end;
  FParent:=AValue;
  if FParent<>nil then begin
    // ToDo
    FParent.Changed;
  end;
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
end;

destructor TCompOptCondNode.Destroy;
begin
  Parent:=nil;
  Clear;
  inherited Destroy;
end;

procedure TCompOptCondNode.Clear;
begin
  fClearing:=true;
  while FFirstChild<>nil do
    FFirstChild.Free;
  fClearing:=false;
end;

procedure TCompOptCondNode.AddLast(Child: TCompOptCondNode);
begin
  Child.Parent:=nil;
  Child.fPrevSibling:=FLastChild;
  FLastChild.FNextSibling:=Child;
  if FFirstChild=nil then
    FFirstChild:=Child;
  FLastChild:=Child;
  Child.FParent:=Self;
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
begin
  XMLConfig.SetDeleteValue(Path+'NodeType',COCNodeTypeNames[NodeType],
                           COCNodeTypeNames[cocntNone]);
  XMLConfig.SetDeleteValue(Path+'ValueType',COCValueTypeNames[ValueType],
                           COCValueTypeNames[cocvtNone]);
  XMLConfig.SetDeleteValue(Path+'Value',Value,'');
  XMLConfig.SetDeleteValue(Path+'ChildCount',Count,0);
  // ToDo
end;

{ TCompOptConditionals }

function TCompOptConditionals.GetValues(const ValueType: TCOCValueType): string;

  function ComputeIfNode(Node: TCompOptCondNode;
    out ExprResult: boolean): boolean;
  var
    ResultStr: String;
  begin
    case Node.NodeType of
    cocntIf,cocntElseIf:
      begin
        ResultStr:=FEvaluator.Eval(Node.Value);
        if FEvaluator.ErrorPosition>=0 then begin
          FErrorNode:=Node;
          exit(false);
        end;
        ExprResult:=ResultStr<>'0';
      end;
    cocntIfdef:
      ExprResult:=FEvaluator.IsDefined(Node.Value);
    cocntIfNdef:
      ExprResult:=not FEvaluator.IsDefined(Node.Value);
    else
      exit(false);
    end;
    Result:=true;
  end;

  function ComputeNode(Node: TCompOptCondNode): boolean;
  var
    ExprResult: boolean;
  begin
    Result:=false;
    case Node.NodeType of
    cocntIf,cocntIfdef,cocntIfNdef:
      begin
        if not ComputeIfNode(Node,ExprResult) then exit;
        if ExprResult then begin
          if Node.FirstChild<>nil then
            if not ComputeNode(Node.FirstChild) then exit;
          // skip till EndIf

        end else begin

        end;
      end;

    end;
    Result:=true;
  end;

var
  v: TCOCValueType;
begin
  if (not FValuesValid)
  or (FEvaluator.ChangeStamp<>FEvaluatorStamp) then begin
    for v:=Low(FValues) to High(FValues) do
      FValues[v]:='';
    ComputeNode(Root);
    FValuesValid:=true;
    FEvaluatorStamp:=FEvaluator.ChangeStamp;
  end;
  Result:=FValues[ValueType];
end;

procedure TCompOptConditionals.SetEvaluator(const AValue: TExpressionEvaluator
  );
begin
  if FEvaluator=AValue then exit;
  FEvaluator:=AValue;
  InvalidateValues;
end;

constructor TCompOptConditionals.Create;
begin

end;

destructor TCompOptConditionals.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCompOptConditionals.Clear;
begin
  FErrorNode:=nil;
  FreeAndNil(FRoot);
end;

procedure TCompOptConditionals.InvalidateValues;
begin
  FValuesValid:=false;
  FErrorNode:=nil;
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
  if FChangeStamp<High(Integer) then
    inc(FChangeStamp)
  else
    FChangeStamp:=Low(Integer);
end;

end.

