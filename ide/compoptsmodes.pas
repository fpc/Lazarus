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
  Classes, SysUtils, LCLProc, Laz_XMLCfg, ExprEval,
  IDEProcs, ProjectIntf;

type

  { TCompilerDiffTool
    A tool to create the difference between two option sets }

  TCompilerDiffTool = class
  private
    FDiff: TStrings;
    FDiffer: boolean;
    FPath: string;
    procedure SetDiff(const AValue: TStrings);
    procedure SetDiffer(const AValue: boolean);
    procedure SetPath(const AValue: string);
  public
    constructor Create(DiffList: TStrings);
    procedure AddDiffItem(const PropertyName, Value: string);
    procedure AddDiff(const PropertyName: string; const Old, New: string);
    procedure AddDiff(const PropertyName: string; const Old, New: integer);
    procedure AddDiff(const PropertyName: string; const Old, New: boolean);
    procedure AddStringsDiff(const PropertyName: string; const OldList, NewList: TStrings);
    procedure AddPathsDiff(const PropertyName: string; const Old, New: string);
    procedure AddSetDiff(const PropertyName: string; const Old, New: integer;
                         const EnumNames: PString);
    property Diff: TStrings read FDiff write SetDiff;
    property Path: string read FPath write SetPath;
    property Differ: boolean read FDiffer write SetDiffer;
  end;

  { TCompOptConditionals }

  TCompOptConditionals = class(TLazCompOptConditionals)
  private
    FChangeStamp: integer;
    FErrorMsg: string;
    FErrorNode: TCompOptCondNode;
    FEvaluator: TExpressionEvaluator;
    FEvaluatorStamp: integer;
    FValuesValid: boolean;
    FValues: array[TCOCValueType] of string;
    function GetValues(const ValueType: TCOCValueType): string;
    procedure SetEvaluator(const AValue: TExpressionEvaluator);
    procedure AddValue(const ValueType: TCOCValueType; Value: string);
    procedure SetValue(const ValueType: TCOCValueType; Value: string);
  public
    constructor Create(TheEvaluator: TExpressionEvaluator);
    destructor Destroy; override;
    procedure Clear;
    procedure ClearNodes;
    procedure InvalidateValues; override;
    procedure Assign(Source: TLazCompOptConditionals); override;
    procedure CreateDiff(CompOpts: TLazCompOptConditionals;
                         Tool: TCompilerDiffTool); virtual;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                DoSwitchPathDelims: boolean); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                              UsePathDelim: TPathDelimSwitch); virtual;
    property Values[ValueType: TCOCValueType]: string read GetValues;
    property Evaluator: TExpressionEvaluator read FEvaluator write SetEvaluator;
    property ChangeStamp: integer read FChangeStamp;
    procedure IncreaseChangeStamp; inline;
    procedure WriteDebugReport;
    property ErrorNode: TCompOptCondNode read FErrorNode write FErrorNode;
    property ErrorMsg: string read FErrorMsg write FErrorMsg;
  end;

implementation

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
          FErrorMsg:='error in expression at column '+IntToStr(FEvaluator.ErrorPosition);
          exit(false);
        end;
        ExprResult:=ResultStr<>'0';
      end;
    cocntIfdef:
      ExprResult:=FEvaluator.IsDefined(Node.Value);
    cocntIfNdef:
      ExprResult:=not FEvaluator.IsDefined(Node.Value);
    else
      FErrorNode:=Node;
      FErrorMsg:='unexpected node of type '+COCNodeTypeNames[Node.NodeType];
      exit(false);
    end;
    Result:=true;
  end;

  function ComputeNode(ParentNode: TCompOptCondNode; Index: integer): boolean;
  var
    ExprResult: boolean;
    Node: TCompOptCondNode;
  begin
    Result:=false;
    while Index<ParentNode.Count do begin
      Node:=ParentNode.Childs[Index];
      case Node.NodeType of

      cocntIf,cocntIfdef,cocntIfNdef:
        while true do begin
          if (Node.NodeType=cocntElse) then
            ExprResult:=true
          else if (not ComputeIfNode(Node,ExprResult)) then
            exit;
          if ExprResult then begin
            // execute childs
            if Node.Count>0 then
              if not ComputeNode(Node,0) then exit;
            // skip all else
            inc(Index);
            while (Index<ParentNode.Count) do begin
              Node:=ParentNode.Childs[Index];
              if not (Node.NodeType in [cocntElseIf,cocntElse]) then break;
              if ParentNode.Childs[Index-1].NodeType=cocntElse then begin
                FErrorNode:=Node;
                FErrorMsg:='ElseIf not allowed after Else';
                exit(false);
              end;
              inc(Index);
            end;
            break;
          end else begin
            // skip childs
            inc(Index);
          end;
          if Index>=ParentNode.Count then break;
          Node:=ParentNode.Childs[Index];
        end;

      cocntAddValue:
        begin
          AddValue(Node.ValueType,Node.Value);
          inc(Index);
        end;

      cocntSetValue:
        begin
          SetValue(Node.ValueType,Node.Value);
          inc(Index);
        end;

      else
        fErrorNode:=Node;
        FErrorMsg:='unexpected node of type '+COCNodeTypeNames[Node.NodeType];
        exit(false);
      end;
    end;
    Result:=true;
  end;

var
  v: TCOCValueType;
begin
  if FEvaluator=nil then begin
    Result:='';
    exit;
  end;
  if (not FValuesValid)
  or (FEvaluator.ChangeStamp<>FEvaluatorStamp) then begin
    for v:=Low(FValues) to High(FValues) do
      FValues[v]:='';
    ComputeNode(Root,0);
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

procedure TCompOptConditionals.AddValue(const ValueType: TCOCValueType;
  Value: string);
begin
  Value:=Trim(Value);
  if Value='' then exit;
  case ValueType of
  cocvtUnitPath,cocvtSrcPath,cocvtIncludePath,cocvtObjectPath,cocvtLibraryPath,
  cocvtDebugPath:
    begin
      FValues[ValueType]:=MergeSearchPaths(FValues[ValueType],Value);
    end;
  cocvtLinkerOptions,cocvtCustomOptions:
    begin
      if FValues[ValueType]<>'' then
        FValues[ValueType]:=FValues[ValueType]+' ';
      FValues[ValueType]:=FValues[ValueType]+Value;
    end;
  else
    FValues[ValueType]:=FValues[ValueType]+Value;
  end;
end;

procedure TCompOptConditionals.SetValue(const ValueType: TCOCValueType;
  Value: string);
begin
  FValues[ValueType]:=Trim(Value);
end;

constructor TCompOptConditionals.Create(TheEvaluator: TExpressionEvaluator);
begin
  FEvaluator:=TheEvaluator;
  inherited Create;
end;

destructor TCompOptConditionals.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCompOptConditionals.Clear;
begin
  ClearNodes;
end;

procedure TCompOptConditionals.ClearNodes;
begin
  FValuesValid:=false;
  FErrorNode:=nil;
  FErrorMsg:='';
  Root.ClearNodes;
end;

procedure TCompOptConditionals.InvalidateValues;
begin
  FValuesValid:=false;
  FErrorNode:=nil;
  FErrorMsg:='';
end;

procedure TCompOptConditionals.Assign(Source: TLazCompOptConditionals);
begin
  ClearNodes;
  Root.Assign(Source.Root);
end;

procedure TCompOptConditionals.CreateDiff(CompOpts: TLazCompOptConditionals;
  Tool: TCompilerDiffTool);

  procedure Diff(CurNode, OtherNode: TCompOptCondNode);
  var
    i: Integer;
    OldPath: String;
  begin
    Tool.AddDiff('NodeType',COCNodeTypeNames[CurNode.NodeType],COCNodeTypeNames[OtherNode.NodeType]);
    Tool.AddDiff('ValueType',COCValueTypeNames[CurNode.ValueType],COCValueTypeNames[OtherNode.ValueType]);
    Tool.AddDiff('Value',CurNode.Value,OtherNode.Value);
    if CurNode.Count<>OtherNode.Count then begin
      Tool.AddDiff('Count',IntToStr(CurNode.Count),IntToStr(OtherNode.Count));
      exit;
    end;
    for i:=0 to CurNode.Count-1 do begin
      OldPath:=Tool.Path;
      Tool.Path:=Tool.Path+'Item'+IntToStr(i+1)+'/';
      Diff(CurNode.Childs[i],OtherNode.Childs[i]);
      Tool.Path:=OldPath;
    end;
  end;

begin
  Diff(Root,CompOpts.Root);
end;

procedure TCompOptConditionals.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; DoSwitchPathDelims: boolean);

  procedure LoadNode(Node: TCompOptCondNode; const SubPath: string);
  var
    NewCount: LongInt;
    i: Integer;
    NewChild: TCompOptCondNode;
    s: String;
  begin
    Node.ClearNodes;
    Node.NodeType:=COCNodeTypeNameToType(XMLConfig.GetValue(SubPath+'NodeType',''));
    Node.ValueType:=COCValueTypeNameToType(XMLConfig.GetValue(SubPath+'ValueType',''));
    s:=XMLConfig.GetValue(SubPath+'Value','');
    Node.Value:=SwitchPathDelims(s,DoSwitchPathDelims);
    // load childs
    NewCount:=XMLConfig.GetValue(SubPath+'ChildCount',0);
    for i:=1 to NewCount do begin
      NewChild:=TCompOptCondNode.Create(Node.Owner);
      Node.AddLast(NewChild);
      LoadNode(NewChild,SubPath+'Item'+IntToStr(i)+'/');
    end;
  end;

begin
  LoadNode(Root,Path);
  Root.NodeType:=cocntNone;
  Root.ValueType:=cocvtNone;
  Root.Value:='';
end;

procedure TCompOptConditionals.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; UsePathDelim: TPathDelimSwitch);

  procedure SaveNode(Node: TCompOptCondNode; const SubPath: string);
  var
    i: Integer;
    s: String;
  begin
    XMLConfig.SetDeleteValue(SubPath+'NodeType',COCNodeTypeNames[Node.NodeType],
                             COCNodeTypeNames[cocntNone]);
    XMLConfig.SetDeleteValue(SubPath+'ValueType',COCValueTypeNames[Node.ValueType],
                             COCValueTypeNames[cocvtNone]);
    s:=SwitchPathDelims(Node.Value,UsePathDelim);
    XMLConfig.SetDeleteValue(SubPath+'Value',s,'');
    // save childs
    XMLConfig.SetDeleteValue(SubPath+'ChildCount',Node.Count,0);
    for i:=0 to Node.Count-1 do
      SaveNode(Node.Childs[i],SubPath+'Item'+IntToStr(i+1)+'/');
  end;

begin
  SaveNode(Root,Path);
end;

procedure TCompOptConditionals.IncreaseChangeStamp; inline;
begin
  if FChangeStamp<High(Integer) then
    inc(FChangeStamp)
  else
    FChangeStamp:=Low(Integer);
end;

procedure TCompOptConditionals.WriteDebugReport;

  procedure WriteNode(Prefix: string; Node: TCompOptCondNode);
  var
    i: Integer;
  begin
    DebugLn([Prefix,'Type=',COCNodeTypeNames[Node.NodeType],' Value=',Node.Value]);
    for i:=0 to Node.Count-1 do
      WriteNode(Prefix+'  ',Node.Childs[i]);
  end;

begin
  DebugLn(['TCompOptConditionals.WriteDebugReport ']);
  WriteNode('  ',Root);
end;

{ TCompilerDiffTool }

procedure TCompilerDiffTool.SetDiff(const AValue: TStrings);
begin
  if FDiff=AValue then exit;
  FDiff:=AValue;
end;

procedure TCompilerDiffTool.SetDiffer(const AValue: boolean);
begin
  if FDiffer=AValue then exit;
  FDiffer:=AValue;
end;

procedure TCompilerDiffTool.SetPath(const AValue: string);
begin
  if FPath=AValue then exit;
  FPath:=AValue;
  // ! config path, not file path. Always /, not PathDelim
  if (FPath<>'') and (Path[length(Path)]<>'/') then FPath:=FPath+'/';
end;

constructor TCompilerDiffTool.Create(DiffList: TStrings);
begin
  FDiff:=DiffList;
  if Diff<>nil then
    Diff.Clear;
end;

procedure TCompilerDiffTool.AddDiffItem(const PropertyName, Value: string);
begin
  Differ:=true;
  if Diff<>nil then
    Diff.Add(Path+PropertyName+'='+Value);
end;

procedure TCompilerDiffTool.AddDiff(const PropertyName: string; const Old,
  New: string);
begin
  if Old=New then exit;
  AddDiffItem(PropertyName,New);
end;

procedure TCompilerDiffTool.AddDiff(const PropertyName: string; const Old,
  New: integer);
begin
  if Old=New then exit;
  AddDiffItem(PropertyName,IntToStr(New));
end;

procedure TCompilerDiffTool.AddDiff(const PropertyName: string; const Old,
  New: boolean);
begin
  if Old=New then exit;
  AddDiffItem(PropertyName,dbgs(New));
end;

procedure TCompilerDiffTool.AddStringsDiff(const PropertyName: string;
  const OldList, NewList: TStrings);
var
  i: Integer;
begin
  AddDiff(PropertyName+'/Count',OldList.Count,NewList.Count);
  for i:=0 to OldList.Count-1 do begin
    if (i>=NewList.Count) or (OldList[i]<>NewList[i]) then
      AddDiffItem(PropertyName+'/Item'+IntToStr(i),NewList[i]);
  end;
end;

procedure TCompilerDiffTool.AddPathsDiff(const PropertyName: string; const Old,
  New: string);
begin
  if Old=New then exit;
  AddDiff(PropertyName,Old,New);
end;

procedure TCompilerDiffTool.AddSetDiff(const PropertyName: string; const Old,
  New: integer; const EnumNames: PString);
var
  i: Integer;
  Mask: LongInt;
  s: String;
begin
  if Old=New then exit;
  Mask := 1;
  s:='';
  for i := 0 to 31 do begin
    if (New and Mask) <> (Old and Mask) then begin
      if s<>'' then s:=s+',';
      if (New and Mask) <> 0 then
        s:=s+'+'
      else
        s:=s+'-';
      s:=s+EnumNames[i];
    end;
    Mask := Mask shl 1;
  end;
  AddDiffItem(PropertyName,s);
end;

end.

