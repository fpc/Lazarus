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
    Functions to parse and edit compiler directives.
}
unit DirectivesTree; 

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

{ $DEFINE VerboseDisableUnreachableIFDEFs}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, FileProcs, CodeToolsStructs, BasicCodeTools,
  KeywordFuncLists, LinkScanner, CodeAtom, CodeCache, AVL_Tree,
  ExprEval, CodeTree;

type
  TCompilerDirectiveNodeDesc = word;
  
const
  // descriptors
  cdnBase     = 1000;
  cdnNone     =  0+cdnBase;
  
  cdnRoot     =  1+cdnBase;

  cdnDefine   = 11+cdnBase;
  cdnInclude  = 12+cdnBase;

  cdnIf       = 21+cdnBase;
  cdnElseIf   = 22+cdnBase;
  cdnElse     = 23+cdnBase;
  cdnEnd      = 24+cdnBase;
  
  // sub descriptors
  cdnsBase        = 10000;
  cdnsNone        =  0+cdnsBase;
  
  cdnsIfdef       =  1+cdnsBase;
  cdnsIfC         =  2+cdnsBase;
  cdnsIfndef      =  3+cdnsBase;
  cdnsIf          =  4+cdnsBase;
  cdnsIfOpt       =  5+cdnsBase;
  cdnsEndif       = 11+cdnsBase;
  cdnsEndC        = 12+cdnsBase;
  cdnsIfEnd       = 13+cdnsBase;
  cdnsElse        = 21+cdnsBase;
  cdnsElseC       = 22+cdnsBase;
  cdnsElseIf      = 23+cdnsBase;
  cdnsElIfC       = 24+cdnsBase;
  cdnsDefine      = 31+cdnsBase;
  cdnsUndef       = 32+cdnsBase;
  cdnsSetC        = 33+cdnsBase;
  cdnsInclude     = 41+cdnsBase;
  cdnsIncludePath = 42+cdnsBase;
  cdnsShortSwitch = 51+cdnsBase;
  cdnsLongSwitch  = 52+cdnsBase;
  cdnsMode        = 53+cdnsBase;
  cdnsThreading   = 54+cdnsBase;
  cdnsOther       = 55+cdnsBase;

const
  H2Pas_Function_Prefix = 'H2PAS_FUNCTION_';

type
  TCompilerDirectivesTree = class;

  { ECDirectiveParserException }

  ECDirectiveParserException = class(Exception)
  public
    Sender: TCompilerDirectivesTree;
    constructor Create(ASender: TCompilerDirectivesTree; const AMessage: string);
  end;

  TCompilerMacroStatus = (
    cmsUnknown,   // never seen
    cmsDefined,   // set to a specific value e.g. by $Define or by $IfDef
    cmsUndefined, // undefined e.g. by $Undef
    cmsComplex    // value depends on complex expressions. e.g. {$if A or B}.
    );

  TCompilerMacroStats = class
  public
    Name: string;
    Value: string;
    Status: TCompilerMacroStatus;
    LastDefineNode: TCodeTreeNode;// define or undef node
    LastReadNode: TCodeTreeNode;// if node
  end;

  { TH2PasFunction }

  TH2PasFunction = class
  public
    Name: string;
    HeaderStart: integer;
    HeaderEnd: integer;
    BeginStart: integer;
    BeginEnd: integer;
    IsForward: boolean;
    IsExternal: boolean;
    InInterface: boolean;
    DefNode: TH2PasFunction;// the corresponding node
    function NeedsBody: boolean;
    procedure AdjustPositionsAfterInsert(FromPos, ToPos, DiffPos: integer);
  end;

  { TCompilerDirectivesTree }

  TCompilerDirectivesTree = class
    FChangeStep: integer;
  private
    FDefaultDirectiveFuncList: TKeyWordFunctionList;
    FDisableUnusedDefines: boolean;
    FRemoveDisabledDirectives: boolean;
    FSimplifyExpressions: boolean;
    FUndefH2PasFunctions: boolean;
    function IfdefDirective: boolean;
    function IfCDirective: boolean;
    function IfndefDirective: boolean;
    function IfDirective: boolean;
    function IfOptDirective: boolean;
    function EndifDirective: boolean;
    function EndCDirective: boolean;
    function IfEndDirective: boolean;
    function ElseDirective: boolean;
    function ElseCDirective: boolean;
    function ElseIfDirective: boolean;
    function ElIfCDirective: boolean;
    function DefineDirective: boolean;
    function UndefDirective: boolean;
    function SetCDirective: boolean;
    function IncludeDirective: boolean;
    function IncludePathDirective: boolean;
    function ShortSwitchDirective: boolean;
    function ReadNextSwitchDirective: boolean;
    function LongSwitchDirective: boolean;
    function ModeDirective: boolean;
    function ThreadingDirective: boolean;
    function OtherDirective: boolean;
    procedure InitKeyWordList;

    procedure InitParser;
    procedure CreateChildNode(Desc: TCompilerDirectiveNodeDesc;
                              SubDesc: TCompilerDirectiveNodeDesc = cdnNone);
    procedure EndChildNode;
    procedure EndIFNode(const ErrorMsg: string);

    procedure InternalRemoveNode(Node: TCodeTreeNode);
  public
    Code: TCodeBuffer;
    Src: string;
    SrcLen: integer;
    NestedComments: boolean;
    Tree: TCodeTree;
    CurNode: TCodeTreeNode;
    SrcPos: Integer;
    AtomStart: integer;
    Macros: TAVLTree;// tree of TCompilerMacroStats
    ParseChangeStep: integer;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    
    procedure Parse;
    procedure Parse(aCode: TCodeBuffer; aNestedComments: boolean);
    function UpdateNeeded: boolean;
    procedure ReduceCompilerDirectives(Undefines, Defines: TStrings;
                                       var Changed: boolean);
    procedure GatherH2PasFunctions(out ListOfH2PasFunctions: TFPList;
                                   FindDefNodes: boolean);
    procedure FixMissingH2PasDirectives(var Changed: boolean);
    
    function FindResourceDirective(const Filename: string = '';
                                   StartPos: integer = 1): TCodeTreeNode;
    function IsResourceDirective(Node: TCodeTreeNode;
                                 const Filename: string = ''): boolean;

    function FindIncludeDirective(const Filename: string = '';
                                  StartPos: integer = 1): TCodeTreeNode;
    function IsIncludeDirective(Node: TCodeTreeNode;
                                const Filename: string = ''): boolean;

    function GetDirectiveName(Node: TCodeTreeNode): string;
    function GetDirective(Node: TCodeTreeNode): string;
    function GetIfExpression(Node: TCodeTreeNode;
                             out ExprStart, ExprEnd: integer): boolean;
    function GetIfExpressionString(Node: TCodeTreeNode): string;
    function IsIfExpressionSimple(Node: TCodeTreeNode; out NameStart: integer
                                  ): boolean;
    function FindNameInIfExpression(Node: TCodeTreeNode; Identifier: PChar
                                    ): integer;
    function GetDefineNameAndValue(DefineNode: TCodeTreeNode;
          out NameStart: integer; out HasValue: boolean; out ValueStart: integer
          ): boolean;
    function DefineUsesName(DefineNode: TCodeTreeNode;
                            Identifier: PChar): boolean;
    function NodeIsEmpty(Node: TCodeTreeNode; IgnoreComments: boolean = true): boolean;
    function FindNodeAtPos(p: integer): TCodeTreeNode;
    function NodeStartToCodePos(Node: TCodeTreeNode;
                                out CodePos: TCodeXYPosition): boolean;

    procedure CheckAndImproveExpr_Brackets(Node: TCodeTreeNode;
                                           var Changed: boolean);
    procedure CheckAndImproveExpr_IfDefinedMacro(Node: TCodeTreeNode;
                                                 var Changed: boolean);
    procedure DisableAllUnusedDefines(var Changed: boolean);
    procedure MoveIfNotThenDefsUp(var Changed: boolean);
    procedure DisableUnreachableBlocks(Undefines, Defines: TStrings;
                                       var Changed: boolean);
    procedure DisableNode(Node: TCodeTreeNode; var Changed: boolean;
                          WithContent: boolean);
    procedure DisableDefineNode(Node: TCodeTreeNode; var Changed: boolean);
    procedure DisableIfNode(Node: TCodeTreeNode; WithContent: boolean;
                            var Changed: boolean);
    function InsertDefine(Position: integer; const NewSrc: string;
                          SubDesc: TCompilerDirectiveNodeDesc): TCodeTreeNode;
    procedure RemoveEmptyNodes(var Changed: boolean);

    procedure MoveCursorToPos(p: integer);
    procedure ReadNextAtom;
    function ReadTilBracketClose(CloseBracket: char): boolean;
    function AtomIs(const s: shortstring): boolean;
    function UpAtomIs(const s: shortstring): boolean;
    function AtomIsIdentifier: boolean;
    function GetAtom: string;

    procedure Replace(FromPos, ToPos: integer; const NewSrc: string);

    procedure IncreaseChangeStep;
    procedure ResetMacros;
    procedure ClearMacros;
    procedure WriteDebugReport;
  public
    property SimplifyExpressions: boolean read FSimplifyExpressions
                                          write FSimplifyExpressions;
    property DisableUnusedDefines: boolean read FDisableUnusedDefines
                                           write FDisableUnusedDefines;
    property RemoveDisabledDirectives: boolean read FRemoveDisabledDirectives
                                               write FRemoveDisabledDirectives;
    property UndefH2PasFunctions: boolean read FUndefH2PasFunctions
                                          write FUndefH2PasFunctions;
    property ChangeStep: integer read FChangeStep;
  end;

function CompareCompilerMacroStats(Data1, Data2: Pointer): integer;
function ComparePCharWithCompilerMacroStats(Name, MacroStats: Pointer): integer;
function CompareH2PasFuncByNameAndPos(Data1, Data2: Pointer): integer;
function ComparePCharWithH2PasFuncName(Name, H2PasFunc: Pointer): integer;

function CDNodeDescAsString(Desc: TCompilerDirectiveNodeDesc): string;
function CDNodeSubDescAsString(Desc: TCompilerDirectiveNodeDesc): string;

implementation

type
  TDefineStatus = (
    dsUnknown,
    dsDefined,
    dsNotDefined
    );

  TDefineValue = class
    Name: string;
    Status: TDefineStatus;
    Value: string;
  end;
  
{$IFDEF VerboseDisableUnreachableIFDEFs}
const
  DefineStatusNames: array[TDefineStatus] of string = (
    'dsUnknown','dsDefined','dsNotDefined'
    );
{$ENDIF}
  
function CompareDefineValues(Data1, Data2: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(Pointer(TDefineValue(Data1).Name),
                                Pointer(TDefineValue(Data2).Name));
end;

function ComparePCharWithDefineValue(Name, DefValue: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(Name,
                                Pointer(TDefineValue(DefValue).Name));
end;

function CompareCompilerMacroStats(Data1, Data2: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(Pointer(TCompilerMacroStats(Data1).Name),
                                Pointer(TCompilerMacroStats(Data2).Name));
end;

function ComparePCharWithCompilerMacroStats(Name, MacroStats: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(Name,
                                Pointer(TCompilerMacroStats(MacroStats).Name));
end;

function CompareH2PasFuncByNameAndPos(Data1, Data2: Pointer): integer;
var
  F1: TH2PasFunction;
  F2: TH2PasFunction;
begin
  F1:=TH2PasFunction(Data1);
  F2:=TH2PasFunction(Data2);
  Result:=CompareIdentifierPtrs(Pointer(F1.Name),Pointer(F2.Name));
  if Result<>0 then exit;
  if F1.HeaderStart>F2.HeaderStart then
    exit(1)
  else if F1.HeaderStart<F2.HeaderStart then
    exit(-1)
  else
    exit(0);
end;

function ComparePCharWithH2PasFuncName(Name, H2PasFunc: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(Name,Pointer(TH2PasFunction(H2PasFunc).Name));
end;

function CDNodeDescAsString(Desc: TCompilerDirectiveNodeDesc): string;
begin
  case Desc of
  cdnNone     : Result:='None';

  cdnRoot     : Result:='Root';

  cdnDefine   : Result:='Define';

  cdnIf       : Result:='If';
  cdnElseIf   : Result:='ElseIf';
  cdnElse     : Result:='Else';
  cdnEnd      : Result:='End';
  else          Result:='?';
  end;
end;

function CDNodeSubDescAsString(Desc: TCompilerDirectiveNodeDesc): string;
begin
  case Desc of
  cdnsIfdef       : Result:='IfDef';
  cdnsIfC         : Result:='IfC';
  cdnsIfndef      : Result:='IfNDef';
  cdnsIf          : Result:='If';
  cdnsIfOpt       : Result:='IfOpt';
  cdnsEndif       : Result:='EndIf';
  cdnsEndC        : Result:='EndC';
  cdnsIfEnd       : Result:='IfEnd';
  cdnsElse        : Result:='Else';
  cdnsElseC       : Result:='ElseC';
  cdnsElseIf      : Result:='ElseIf';
  cdnsElIfC       : Result:='ElIfC';
  cdnsDefine      : Result:='Define';
  cdnsUndef       : Result:='UnDef';
  cdnsSetC        : Result:='SetC';
  cdnsInclude     : Result:='Include';
  cdnsIncludePath : Result:='IncludePath';
  cdnsShortSwitch : Result:='ShortSwitch';
  cdnsLongSwitch  : Result:='LongSwitch';
  cdnsMode        : Result:='Mode';
  cdnsThreading   : Result:='Threading';
  cdnsOther       : Result:='Other';
  else              Result:='?';
  end;
end;


{ TCompilerDirectivesTree }

function TCompilerDirectivesTree.IfdefDirective: boolean;
// example: {$IFDEF macroname}
begin
  Result:=true;
  CreateChildNode(cdnIf,cdnsIfdef);
end;

function TCompilerDirectivesTree.IfCDirective: boolean;
// example: {$IFC expression}
begin
  Result:=true;
  CreateChildNode(cdnIf,cdnsIfC);
end;

function TCompilerDirectivesTree.IfndefDirective: boolean;
// example: {$IFNDEF macroname}
begin
  Result:=true;
  CreateChildNode(cdnIf,cdnsIfndef);
end;

function TCompilerDirectivesTree.IfDirective: boolean;
// example: {$IF expression}
begin
  Result:=true;
  CreateChildNode(cdnIf,cdnsIf);
end;

function TCompilerDirectivesTree.IfOptDirective: boolean;
// {$ifopt o+} or {$ifopt o-}
begin
  Result:=true;
  CreateChildNode(cdnIf,cdnsIfOpt);
end;

function TCompilerDirectivesTree.EndifDirective: boolean;
// example: {$ENDIF comment}
begin
  Result:=true;
  EndIFNode('EndIf without IfDef');
  CreateChildNode(cdnEnd,cdnsEndif);
  AtomStart:=SrcPos;
  EndChildNode;
end;

function TCompilerDirectivesTree.EndCDirective: boolean;
// example: {$ENDC comment}
begin
  Result:=true;
  EndIFNode('EndC without IfC');
  CreateChildNode(cdnEnd,cdnsEndC);
  AtomStart:=SrcPos;
  EndChildNode;
end;

function TCompilerDirectivesTree.IfEndDirective: boolean;
// {$IfEnd comment}
begin
  Result:=true;
  EndIFNode('IfEnd without IfDef');
  CreateChildNode(cdnEnd,cdnsIfEnd);
  AtomStart:=SrcPos;
  EndChildNode;
end;

function TCompilerDirectivesTree.ElseDirective: boolean;
// {$Else comment}
begin
  Result:=true;
  EndIFNode('Else without IfDef');
  CreateChildNode(cdnElse,cdnsElse);
end;

function TCompilerDirectivesTree.ElseCDirective: boolean;
// {$elsec comment}
begin
  Result:=true;
  EndIFNode('ElseC without IfC');
  CreateChildNode(cdnElse,cdnsElseC);
end;

function TCompilerDirectivesTree.ElseIfDirective: boolean;
// {$elseif expression}
begin
  Result:=true;
  EndIFNode('ElseIf without IfDef');
  CreateChildNode(cdnElseIf,cdnsElseIf);
end;

function TCompilerDirectivesTree.ElIfCDirective: boolean;
// {$elifc expression}
begin
  Result:=true;
  EndIFNode('ElIfC without IfC');
  CreateChildNode(cdnElseIf,cdnsElIfC);
end;

function TCompilerDirectivesTree.DefineDirective: boolean;
// {$define name} or {$define name:=value}
begin
  Result:=true;
  CreateChildNode(cdnDefine,cdnsDefine);
  AtomStart:=SrcPos;
  EndChildNode;
end;

function TCompilerDirectivesTree.UndefDirective: boolean;
// {$undefine macroname}
begin
  Result:=true;
  CreateChildNode(cdnDefine,cdnsUndef);
  AtomStart:=SrcPos;
  EndChildNode;
end;

function TCompilerDirectivesTree.SetCDirective: boolean;
// {$setc macroname} or {$setc macroname:=value}
begin
  Result:=true;
  CreateChildNode(cdnDefine,cdnsSetC);
  AtomStart:=SrcPos;
  EndChildNode;
end;

function TCompilerDirectivesTree.IncludeDirective: boolean;
begin
  Result:=true;
  CreateChildNode(cdnInclude,cdnsInclude);
  AtomStart:=SrcPos;
  EndChildNode;
end;

function TCompilerDirectivesTree.IncludePathDirective: boolean;
// {$includepath path_addition}
begin
  Result:=true;
end;

function TCompilerDirectivesTree.ShortSwitchDirective: boolean;
// example: {$H+} or {$H+, R- comment}
begin
  Result:=true;
  if Src[AtomStart+3] in ['+','-'] then
    CreateChildNode(cdnDefine,cdnsShortSwitch)
  else begin
    if (Src[AtomStart+2] in ['I','i']) then
      CreateChildNode(cdnInclude,cdnsInclude)
    else
      CreateChildNode(cdnDefine,cdnsOther);
  end;
  AtomStart:=SrcPos;
  EndChildNode;
end;

function TCompilerDirectivesTree.ReadNextSwitchDirective: boolean;
begin
  Result:=true;
end;

function TCompilerDirectivesTree.LongSwitchDirective: boolean;
// example: {$ASSERTIONS ON comment}
begin
  Result:=true;
  CreateChildNode(cdnDefine,cdnsLongSwitch);
  AtomStart:=SrcPos;
  EndChildNode;
end;

function TCompilerDirectivesTree.ModeDirective: boolean;
// example: {$MODE ObjFPC comment}
begin
  Result:=true;
  CreateChildNode(cdnDefine,cdnsMode);
  AtomStart:=SrcPos;
  EndChildNode;
end;

function TCompilerDirectivesTree.ThreadingDirective: boolean;
// example: {$threading on}
begin
  Result:=true;
  CreateChildNode(cdnDefine,cdnsThreading);
  AtomStart:=SrcPos;
  EndChildNode;
end;

function TCompilerDirectivesTree.OtherDirective: boolean;
begin
  Result:=true;
  CreateChildNode(cdnDefine,cdnsOther);
  AtomStart:=SrcPos;
  EndChildNode;
end;

procedure TCompilerDirectivesTree.InitKeyWordList;
var
  c: Char;
begin
  if FDefaultDirectiveFuncList=nil then begin
    FDefaultDirectiveFuncList:=TKeyWordFunctionList.Create;
    with FDefaultDirectiveFuncList do begin
      for c:='A' to 'Z' do begin
        if CompilerSwitchesNames[c]<>'' then begin
          Add(c,{$ifdef FPC}@{$endif}ShortSwitchDirective);
          Add(CompilerSwitchesNames[c],{$ifdef FPC}@{$endif}LongSwitchDirective);
        end;
      end;
      Add('IFDEF',{$ifdef FPC}@{$endif}IfdefDirective);
      Add('IFC',{$ifdef FPC}@{$endif}IfCDirective);
      Add('IFNDEF',{$ifdef FPC}@{$endif}IfndefDirective);
      Add('IF',{$ifdef FPC}@{$endif}IfDirective);
      Add('IFOPT',{$ifdef FPC}@{$endif}IfOptDirective);
      Add('ENDIF',{$ifdef FPC}@{$endif}EndIfDirective);
      Add('ENDC',{$ifdef FPC}@{$endif}EndCDirective);
      Add('ELSE',{$ifdef FPC}@{$endif}ElseDirective);
      Add('ELSEC',{$ifdef FPC}@{$endif}ElseCDirective);
      Add('ELSEIF',{$ifdef FPC}@{$endif}ElseIfDirective);
      Add('ELIFC',{$ifdef FPC}@{$endif}ElIfCDirective);
      Add('IFEND',{$ifdef FPC}@{$endif}IfEndDirective);
      Add('DEFINE',{$ifdef FPC}@{$endif}DefineDirective);
      Add('UNDEF',{$ifdef FPC}@{$endif}UndefDirective);
      Add('SETC',{$ifdef FPC}@{$endif}SetCDirective);
      Add('INCLUDE',{$ifdef FPC}@{$endif}IncludeDirective);
      Add('INCLUDEPATH',{$ifdef FPC}@{$endif}IncludePathDirective);
      Add('MODE',{$ifdef FPC}@{$endif}ModeDirective);
      Add('THREADING',{$ifdef FPC}@{$endif}ThreadingDirective);
      DefaultKeyWordFunction:={$ifdef FPC}@{$endif}OtherDirective;
    end;
  end;
end;

procedure TCompilerDirectivesTree.InitParser;
begin
  ParseChangeStep:=Code.ChangeStep;
  IncreaseChangeStep;
  InitKeyWordList;
  Src:=Code.Source;
  SrcLen:=length(Src);
  if Tree=nil then
    Tree:=TCodeTree.Create
  else
    Tree.Clear;
  SrcPos:=1;
  AtomStart:=1;
  CurNode:=nil;
  CreateChildNode(cdnRoot);
end;

procedure TCompilerDirectivesTree.CreateChildNode(
  Desc: TCompilerDirectiveNodeDesc;
  SubDesc: TCompilerDirectiveNodeDesc);
var NewNode: TCodeTreeNode;
begin
  NewNode:=NodeMemManager.NewNode;
  Tree.AddNodeAsLastChild(CurNode,NewNode);
  NewNode.Desc:=Desc;
  NewNode.SubDesc:=SubDesc;
  CurNode:=NewNode;
  CurNode.StartPos:=AtomStart;
  //DebugLn([GetIndentStr(CurNode.GetLevel*2),'TCompilerDirectivesTree.CreateChildNode ']);
end;

procedure TCompilerDirectivesTree.EndChildNode;
begin
  //DebugLn([GetIndentStr(CurNode.GetLevel*2),'TCompilerDirectivesTree.EndChildNode ']);
  CurNode.EndPos:=AtomStart;
  CurNode:=CurNode.Parent;
end;

procedure TCompilerDirectivesTree.EndIFNode(const ErrorMsg: string);

  procedure RaiseMissingStartNode;
  begin
    WriteDebugReport;
    raise ECDirectiveParserException.Create(Self,ErrorMsg);
  end;

begin
  if (CurNode.Desc<>cdnIf) and (CurNode.Desc<>cdnElse)
  and (CurNode.Desc<>cdnElseIf) then
    RaiseMissingStartNode;
  EndChildNode;
end;

procedure TCompilerDirectivesTree.CheckAndImproveExpr_Brackets(
  Node: TCodeTreeNode; var Changed: boolean);
// improve (MacroName) to MacroName
var
  ExprStart: integer;
  ExprEnd: integer;
  NameStart: LongInt;
  FromPos: LongInt;
  ToPos: LongInt;
begin
  if not SimplifyExpressions then exit;
  if (Node.SubDesc<>cdnsIf) and (Node.SubDesc<>cdnElseIf) then exit;
  if not GetIfExpression(Node,ExprStart,ExprEnd) then exit;

  // improve (MacroName) to MacroName
  MoveCursorToPos(ExprStart);
  repeat
    ReadNextAtom;
    if UpAtomIs('DEFINED') then begin
      // the function defined(): skip keyword and bracket
      ReadNextAtom;
      ReadNextAtom;
    end;
    if AtomIs('(') then begin
      FromPos:=AtomStart;
      ReadNextAtom;
      if AtomIsIdentifier then begin
        NameStart:=AtomStart;
        ReadNextAtom;
        if AtomIs(')') then begin
          ToPos:=SrcPos;
          DebugLn(['TCompilerDirectivesTree.CheckAndImproveExpr_Brackets removing unneeded brackets']);
          Replace(FromPos,ToPos,GetIdentifier(@Src[NameStart]));
          MoveCursorToPos(FromPos);
        end;
      end;
    end;
  until SrcPos>=ExprEnd;
end;

procedure TCompilerDirectivesTree.CheckAndImproveExpr_IfDefinedMacro(
  Node: TCodeTreeNode; var Changed: boolean);
// check if {$IF defined(MacroName)}
//       or {$IF !defined(MacroName)}
//       or {$IF not defined(MacroName)}
//       or {$IF not (defined(MacroName))}
var
  ExprStart: integer;
  ExprEnd: integer;
  MacroNameStart: LongInt;
  Negated: Boolean;
  NewDirective: String;
  BracketLvl: Integer;
begin
  if not SimplifyExpressions then exit;
  if (Node.SubDesc<>cdnsIf) then exit;
  if not GetIfExpression(Node,ExprStart,ExprEnd) then exit;
  Negated:=false;
  MoveCursorToPos(ExprStart);
  ReadNextAtom;
  if UpAtomIs('NOT') or AtomIs('!') then begin
    Negated:=true;
    ReadNextAtom;
  end;
  BracketLvl:=0;
  while AtomIs('(') do begin
    inc(BracketLvl);
    ReadNextAtom;
  end;
  if not UpAtomIs('DEFINED') then exit;
  ReadNextAtom;
  if not AtomIs('(') then exit;
  inc(BracketLvl);
  ReadNextAtom;
  if not AtomIsIdentifier then exit;
  MacroNameStart:=AtomStart;
  ReadNextAtom;
  while AtomIs(')') do begin
    dec(BracketLvl);
    ReadNextAtom;
  end;
  if BracketLvl>0 then exit;
  if SrcPos<=ExprEnd then exit;

  if Negated then
    NewDirective:='IFNDEF'
  else
    NewDirective:='IFDEF';
  NewDirective:='{$'+NewDirective+' '+GetIdentifier(@Src[MacroNameStart])+'}';

  DebugLn(['TCompilerDirectivesTree.CheckAndImproveExpr_IfDefinedMacro simplifying expression']);
  Replace(Node.StartPos,FindCommentEnd(Src,Node.StartPos,NestedComments),NewDirective);
  if Negated then
    Node.SubDesc:=cdnsIfNdef
  else
    Node.SubDesc:=cdnsIfdef;

  Changed:=true;
end;

procedure TCompilerDirectivesTree.DisableAllUnusedDefines(var Changed: boolean);
var
  AVLNode: TAVLTreeNode;
  MacroNode: TCompilerMacroStats;
  NextAVLNode: TAVLTreeNode;
begin
  if Macros=nil then exit;
  if not DisableUnusedDefines then exit;
  AVLNode:=Macros.FindLowest;
  while AVLNode<>nil do begin
    NextAVLNode:=Macros.FindSuccessor(AVLNode);
    MacroNode:=TCompilerMacroStats(AVLNode.Data);
    if (MacroNode.LastDefineNode<>nil)
    and (MacroNode.LastReadNode=nil) then begin
      // this Define/Undef is not used
      DebugLn(['TCompilerDirectivesTree.DisableAllUnusedDefines']);
      DisableDefineNode(MacroNode.LastDefineNode,Changed);
    end;
    AVLNode:=NextAVLNode;
  end;
end;

procedure TCompilerDirectivesTree.MoveIfNotThenDefsUp(var Changed: boolean
  );
(* 1. Search for
    {$IFNDEF Name}
      {$DEFINE Name}
      .. name is not used here ..
    {$ENDIF}

   And move the define behind the IF block

  2. And check for
    {$IFDEF Name}
      .. name is not set here ..
      {$DEFINE Name}
    {$ENDIF}

   And remove the define
*)

  function IdentifierIsReadAfterNode(Identifier: PChar;
    StartNode: TCodeTreeNode): boolean;
  var
    Node: TCodeTreeNode;
    ParentNode: TCodeTreeNode;
  begin
    Node:=StartNode;
    while Node<>nil do begin
      case Node.Desc of
      cdnIf,cdnElseIf:
        if FindNameInIfExpression(Node,Identifier)>0 then begin
          exit(true);
        end;
      cdnDefine:
        if DefineUsesName(Node,Identifier) then begin
          ParentNode:=StartNode;
          while (ParentNode<>nil) do begin
            if ParentNode=Node.Parent then exit(false);
            ParentNode:=ParentNode.Parent;
          end;
        end;
      end;
      Node:=Node.Next;
    end;
    Result:=false;
  end;

var
  Node: TCodeTreeNode;
  NextNode: TCodeTreeNode;
  SubNode: TCodeTreeNode;
  NameStart: integer;
  LastDefineNode: TCodeTreeNode;
  LastIFNode: TCodeTreeNode;
  NextSubNode: TCodeTreeNode;
  EndNode: TCodeTreeNode;
  InsertPos: LongInt;
  NewSrc: String;
  LastChildDefineNode: TCodeTreeNode;
begin
  Node:=Tree.Root;
  while Node<>nil do begin
    NextNode:=Node.Next;
    if ((Node.Desc=cdnIf) or (Node.Desc=cdnElseIf))
    and IsIfExpressionSimple(Node,NameStart) then begin
      // an IF with a single test
      LastIFNode:=nil;
      LastDefineNode:=nil;
      LastChildDefineNode:=nil;
      SubNode:=Node.FirstChild;
      while (SubNode<>nil) and (SubNode.HasAsParent(Node)) do begin
        NextSubNode:=SubNode.Next;
        case SubNode.Desc of
        
        cdnIf, cdnElseIf:
          if FindNameInIfExpression(SubNode,@Src[NameStart])>0 then begin
            // this sub IF block uses the macro
            LastIFNode:=SubNode;
          end;
          
        cdnDefine:
          if ((SubNode.SubDesc=cdnsDefine) or (SubNode.SubDesc=cdnsUndef))
          and DefineUsesName(SubNode,@Src[NameStart]) then begin
            // this sub Define/Undef sets the macro
            if (LastIFNode=nil) and (LastDefineNode=nil) then begin
              (* This is
                {$IF(N)DEF Name}
                  ... Name not used ...
                  {$DEFINE|UNDEF Name}
              *)
              if (Node.SubDesc=cdnsIfndef) = (SubNode.SubDesc=cdnsUndef) then
              begin
                { this is
                     IFNDEF then UNDEF
                 or  IFDEF then DEFINE
                  -> remove define
                }
                NextSubNode:=SubNode.NextSkipChilds;
                DebugLn(['TCompilerDirectivesTree.MoveIfNotThenDefsUp IFDEF + DEFINE => the define is not needed']);
                DisableDefineNode(SubNode,Changed);
                SubNode:=nil;
              end;
            end;
            if SubNode<>nil then begin
              LastDefineNode:=SubNode;
              LastIFNode:=nil;
              if SubNode.Parent=Node then begin
                // this define is valid for end of the IF block
                LastChildDefineNode:=SubNode;
              end else if (LastChildDefineNode<>nil)
              and (LastChildDefineNode.SubDesc<>SubNode.SubDesc) then begin
                // this sub define can cancel the higher level define
                LastChildDefineNode:=nil;
              end;
            end;
          end;
        end;
        SubNode:=NextSubNode;
      end;
      
      if (LastChildDefineNode<>nil) then begin
        (* this is
           {$IFNDEF Name}
             ...
             {$DEFINE Name}
             ... Name only read ...
           {$ENDIF}
           
           or IFDEF and UNDEF
           -> move define behind IF block
        *)
        EndNode:=Node;
        while (EndNode<>nil) and (EndNode.Desc<>cdnEnd) do
          EndNode:=EndNode.NextBrother;
        if (EndNode<>nil)
        and IdentifierIsReadAfterNode(@Src[NameStart],EndNode) then begin
          InsertPos:=FindLineEndOrCodeAfterPosition(Src,EndNode.EndPos,SrcLen,
                                                    NestedComments);
          NewSrc:=LineEnding+GetDirective(LastDefineNode);
          DebugLn(['TCompilerDirectivesTree.MoveIfNotThenDefsUp IFNDEF + DEFINE => add define after block']);
          InsertDefine(InsertPos,NewSrc,LastDefineNode.SubDesc);
          if (LastDefineNode=LastChildDefineNode)
          and (LastIFNode=nil) then begin
            // the name was not read after it was set -> disable the define
            // in the block
            DebugLn(['TCompilerDirectivesTree.MoveIfNotThenDefsUp old DEFINE is not needed anymore']);
            DisableDefineNode(LastDefineNode,Changed);
          end;
        end;
      end;
    end;
    Node:=NextNode;
  end;
end;

procedure TCompilerDirectivesTree.DisableUnreachableBlocks(Undefines,
  Defines: TStrings; var Changed: boolean);
type
  PDefineChange = ^TDefineChange;
  TDefineChange = record
    Name: string;
    OldStatus: TDefineStatus;
    Next: PDefineChange;
  end;
  
var
  CurDefines: TAVLTree;
  Stack: array of PDefineChange;// stack of lists of PDefineChange
  StackPointer: integer;
  
  procedure InitStack;
  begin
    SetLength(Stack,1);
    StackPointer:=0;
    Stack[0]:=nil;
  end;

  procedure FreeStack;
  var
    i: Integer;
    Item: PDefineChange;
    DeleteItem: PDefineChange;
  begin
    for i:=0 to StackPointer do begin
      Item:=Stack[i];
      while Item<>nil do begin
        DeleteItem:=Item;
        Item:=DeleteItem^.Next;
        Dispose(DeleteItem);
      end;
    end;
    Setlength(Stack,0);
  end;

  procedure AddStackChange(const MacroName: string; OldStatus: TDefineStatus);
  var
    Change: PDefineChange;
  begin
    {$IFDEF VerboseDisableUnreachableIFDEFs}
    DebugLn(['AddStackChange ',MacroName,' ',DefineStatusNames[OldStatus]]);
    {$ENDIF}
    // check if MacroName was already changed
    Change:=Stack[StackPointer];
    while (Change<>nil) do begin
      if (CompareIdentifierPtrs(Pointer(MacroName),Pointer(Change^.Name))=0)
      then begin
        // old status is already saved
        exit;
      end;
      Change:=Change^.Next;
    end;
  
    {$IFDEF VerboseDisableUnreachableIFDEFs}
    DebugLn(['AddStackChange ADD ',MacroName,' ',DefineStatusNames[OldStatus]]);
    {$ENDIF}
    New(Change);
    FillChar(Change^,SizeOf(TDefineChange),0);
    Change^.Name:=MacroName;
    Change^.OldStatus:=OldStatus;
    Change^.Next:=Stack[StackPointer];
    Stack[StackPointer]:=Change;
  end;
  
  function GetStatus(Identifier: PChar): TDefineStatus;
  var
    AVLNode: TAVLTreeNode;
  begin
    AVLNode:=CurDefines.FindKey(Identifier,@ComparePCharWithDefineValue);
    if AVLNode<>nil then
      Result:=TDefineValue(AVLNode.Data).Status
    else
      Result:=dsUnknown;
  end;
  
  procedure SetStatus(Identifier: PChar; NewStatus: TDefineStatus;
    SaveOnStack, SetGlobal: boolean);
  var
    AVLNode: TAVLTreeNode;
    DefValue: TDefineValue;
    i: Integer;
    Change: PDefineChange;
  begin
    {$IFDEF VerboseDisableUnreachableIFDEFs}
    DebugLn(['SetStatus ',GetIdentifier(Identifier),' Old=',DefineStatusNames[GetStatus(Identifier)],' New=',DefineStatusNames[NewStatus],' SaveOnStack=',SaveOnStack,' SetGlobal=',SetGlobal]);
    {$ENDIF}
    AVLNode:=CurDefines.FindKey(Identifier,@ComparePCharWithDefineValue);
    if AVLNode=nil then begin
      if NewStatus<>dsUnknown then begin
        DefValue:=TDefineValue.Create;
        DefValue.Name:=GetIdentifier(Identifier);
        DefValue.Status:=NewStatus;
        CurDefines.Add(DefValue);
        if SaveOnStack then
          AddStackChange(DefValue.Name,dsUnknown);
      end else begin
        // no change
      end;
    end else begin
      DefValue:=TDefineValue(AVLNode.Data);
      if NewStatus<>dsUnknown then begin
        if NewStatus<>DefValue.Status then begin
          if SaveOnStack then
            AddStackChange(DefValue.Name,DefValue.Status);
          DefValue.Status:=NewStatus;
        end;
      end else begin
        if SaveOnStack then
          AddStackChange(DefValue.Name,DefValue.Status);
        CurDefines.Delete(AVLNode);
        DefValue.Free;
      end;
    end;
    if SetGlobal then begin
      for i:=StackPointer downto 0 do begin
        Change:=Stack[i];
        while Change<>nil do begin
          if CompareIdentifiers(PChar(Change^.Name),Identifier)=0 then begin
            if (Change^.OldStatus=dsUnknown)
            or (Change^.OldStatus=NewStatus) then begin
              // ok
            end else begin
              Change^.OldStatus:=dsUnknown;
            end;
          end;
          Change:=Change^.Next;
        end;
      end;
    end;
    {$IFDEF VerboseDisableUnreachableIFDEFs}
    DebugLn(['SetStatus ',GetIdentifier(Identifier),' Cur=',DefineStatusNames[GetStatus(Identifier)],' Should=',DefineStatusNames[NewStatus]]);
    {$ENDIF}
  end;

  procedure InitDefines;
  var
    i: Integer;
    CurName: string;
    Node: TCodeTreeNode;
    ExprStart: integer;
    ExprEnd: integer;
  begin
    CurDefines:=TAVLTree.Create(@CompareDefineValues);
    {$IFDEF VerboseDisableUnreachableIFDEFs}
    DebugLn(['InitDefines ',Defines<>nil,' ',Undefines<>nil]);
    {$ENDIF}
    if Undefines<>nil then begin
      for i:=0 to Undefines.Count-1 do
        if Undefines[i]<>'' then
          SetStatus(PChar(Undefines[i]),dsNotDefined,false,false);
    end;
    if Defines<>nil then begin
      for i:=0 to Defines.Count-1 do begin
        CurName:=Defines[i];
        if System.Pos('=',CurName)>0 then
          CurName:=Defines.Names[i];
        if CurName='' then continue;
        SetStatus(PChar(CurName),dsDefined,false,false);
      end;
    end;
    if UndefH2PasFunctions then begin
      Node:=Tree.Root;
      while Node<>nil do begin
        if ((Node.Desc=cdnIf) or (Node.Desc=cdnElseIf)) then begin
          if GetIfExpression(Node,ExprStart,ExprEnd) then begin
            MoveCursorToPos(ExprStart);
            repeat
              ReadNextAtom;
              if AtomStart>=ExprEnd then break;
              if ComparePrefixIdent(H2Pas_Function_Prefix,@Src[AtomStart]) then
                SetStatus(@Src[AtomStart],dsNotDefined,false,false);
            until false;
          end;
        end;
        Node:=Node.Next;
      end;
    end;
  end;
  
  procedure FreeDefines;
  begin
    if CurDefines=nil then exit;
    CurDefines.FreeAndClear;
    FreeAndNil(CurDefines);
  end;
  
  procedure Push;
  begin
    inc(StackPointer);
    if StackPointer=length(Stack) then
      SetLength(Stack,length(Stack)*2+10);
    Stack[StackPointer]:=nil;
  end;
  
  procedure Pop;
  var
    Change: PDefineChange;
  begin
    if StackPointer=0 then
      raise ECDirectiveParserException.Create(Self,'TCompilerDirectivesTree.DisableUnreachableBlocks.Pop without Push');
    // undo all changes
    while Stack[StackPointer]<>nil do begin
      Change:=Stack[StackPointer];
      SetStatus(PChar(Change^.Name),Change^.OldStatus,false,false);
      Stack[StackPointer]:=Change^.Next;
      Dispose(Change);
    end;
    dec(StackPointer);
  end;
  
var
  Node: TCodeTreeNode;
  NextNode: TCodeTreeNode;
  NameStart: integer;
  NewStatus: TDefineStatus;
  Identifier: PChar;
  OldStatus: TDefineStatus;
  HasValue: boolean;
  ValueStart: integer;
  ExprNode: TCodeTreeNode;
  IsIfBlock: Boolean;
  BlockIsAlwaysReached: Boolean;
  BlockIsNeverReached: Boolean;
  BlockIsReachable: Boolean;
begin
  InitDefines;
  InitStack;
  try
    Node:=Tree.Root;
    while Node<>nil do begin
      NextNode:=Node.Next;
      {$IFDEF VerboseDisableUnreachableIFDEFs}
      DebugLn(['TCompilerDirectivesTree.DisableUnreachableBlocks Node=',CDNodeDescAsString(Node.Desc),'=',GetDirective(Node)]);
      {$ENDIF}
      case Node.Desc of
      cdnIf, cdnElse:
        begin
          if Node.Desc=cdnIf then begin
            IsIfBlock:=true;
          end else begin
            IsIfBlock:=false;
            // close prior block
            Pop;
          end;
          // start new block
          Push;
          
          if IsIfBlock then begin
            ExprNode:=Node;
          end else begin
            if Node.PriorBrother.Desc=cdnIf then begin
              ExprNode:=Node.PriorBrother;
            end else begin
              ExprNode:=nil;
            end;
          end;
          {$IFDEF VerboseDisableUnreachableIFDEFs}
          if (ExprNode<>nil) then
            DebugLn(['TCompilerDirectivesTree.DisableUnreachableBlocks Expr=',GetIfExpressionString(ExprNode),' Simple=',IsIfExpressionSimple(ExprNode,NameStart)])
          else
            DebugLn(['TCompilerDirectivesTree.DisableUnreachableBlocks Expr=nil']);
          {$ENDIF}

          if (ExprNode<>nil) and IsIfExpressionSimple(ExprNode,NameStart) then
          begin
            // a simple expression
            Identifier:=@Src[NameStart];
            if (Node.SubDesc=cdnsIfndef)=IsIfBlock then
              NewStatus:=dsNotDefined
            else
              NewStatus:=dsDefined;
            OldStatus:=GetStatus(Identifier);
            BlockIsReachable:=(OldStatus=dsUnknown) or (OldStatus=NewStatus);
            BlockIsAlwaysReached:=OldStatus=NewStatus;
            BlockIsNeverReached:=(OldStatus<>dsUnknown) and (OldStatus<>NewStatus);
            {$IFDEF VerboseDisableUnreachableIFDEFs}
            DebugLn(['TCompilerDirectivesTree.DisableUnreachableBlocks Identifier=',GetIdentifier(Identifier),' Reachable=',BlockIsReachable,' Always=',BlockIsAlwaysReached,' Never=',BlockIsNeverReached,' NewStatus=',DefineStatusNames[NewStatus]]);
            {$ENDIF}
            if BlockIsReachable then
              SetStatus(Identifier,NewStatus,true,false);
            if BlockIsAlwaysReached or BlockIsNeverReached then begin
              // this node can be removed
              if BlockIsNeverReached or (Node.FirstChild=nil) then begin
                NextNode:=Node.NextBrother;
                if (NextNode<>nil) and (NextNode.Desc=cdnEnd) then begin
                  // if the next node is an end node it will be disabled too
                  NextNode:=NextNode.NextSkipChilds;
                end;
              end;
              // we can Pop here, because
              //   this the last block
              //   or this is the first block, then the next block will
              //   become the new first block
              Pop;
              if BlockIsAlwaysReached then
                DebugLn(['TCompilerDirectivesTree.DisableUnreachableBlocks BlockIsAlwaysReached ',GetDirective(Node)]);
              if BlockIsNeverReached then
                DebugLn(['TCompilerDirectivesTree.DisableUnreachableBlocks BlockIsNeverReached ',GetDirective(Node)]);
              DisableIfNode(Node,BlockIsNeverReached,Changed);
            end;
          end else begin
            // a complex expression (If, ElseIf, Else)
            // assume: it is reachable
          end;
        end;
        
      cdnElseIf:
        begin
          // if there is an ElseIf block, then there must be an IF block in front
          // And the IF block in front must be reachable,
          // otherwise it would be disabled
          Pop;
          // If+ElseIf gives a complex expression
          // assume: it is reachable
          Push;
        end;
        
      cdnEnd:
        begin
          Pop;
        end;
        
      cdnDefine:
        if ((Node.SubDesc=cdnsDefine) or (Node.SubDesc=cdnsUndef)
        or (Node.SubDesc=cdnsSetC))
        and GetDefineNameAndValue(Node,NameStart,HasValue,ValueStart) then begin
          if Node.SubDesc=cdnsDefine then
            NewStatus:=dsDefined
          else
            NewStatus:=dsNotDefined;
          if GetStatus(@Src[NameStart])=NewStatus then begin
            // this define is not needed
            NextNode:=NextNode.NextSkipChilds;
            DebugLn(['TCompilerDirectivesTree.DisableUnreachableBlocks DEFINE is already, always set to this value']);
            DisableDefineNode(Node,Changed);
          end else begin
            // set status on all levels
            SetStatus(@Src[NameStart],NewStatus,true,true);
          end;
        end;
      end;
      Node:=NextNode;
    end;
  finally
    FreeStack;
    FreeDefines;
  end;
  {$IFDEF VerboseDisableUnreachableIFDEFs}
  DebugLn(['TCompilerDirectivesTree.DisableUnreachableBlocks END']);
  {$ENDIF}
end;

procedure TCompilerDirectivesTree.DisableNode(Node: TCodeTreeNode;
  var Changed: boolean; WithContent: boolean);
begin
  if Node=nil then exit;
  case Node.Desc of
  cdnDefine, cdnInclude: DisableDefineNode(Node,Changed);
  cdnIf, cdnElseIf, cdnElse: DisableIfNode(Node,WithContent,Changed);
  end;
end;

procedure TCompilerDirectivesTree.DisableDefineNode(Node: TCodeTreeNode;
  var Changed: boolean);
var
  FromPos: LongInt;
  ToPos: LongInt;
  NewSrc: String;
begin
  if not DisableUnusedDefines then exit;
  //DebugLn(['TCompilerDirectivesTree.DisableDefineNode ',GetDirective(Node)]);
  if RemoveDisabledDirectives then begin
    // remove directive (including space+empty lines in front and spaces behind)
    FromPos:=Node.StartPos;
    while (FromPos>1) and (IsSpaceChar[Src[FromPos-1]]) do dec(FromPos);
    ToPos:=FindCommentEnd(Src,Node.StartPos,NestedComments);
    ToPos:=FindLineEndOrCodeAfterPosition(Src,ToPos,SrcLen+1,NestedComments);
    NewSrc:='';
    if (FromPos=1) and (ToPos<SrcLen) and (Src[ToPos] in [#10,#13]) then begin
      inc(ToPos);
      if (ToPos<=SrcLen) and (Src[ToPos] in [#10,#13])
      and (Src[ToPos]<>Src[ToPos-1]) then
        inc(ToPos);
    end;
    Replace(FromPos,ToPos,NewSrc);
  end else begin
    // disable directive -> {off $Define MacroName}
    Replace(Node.StartPos+1,Node.StartPos+1,'off ');
  end;
  Changed:=true;
  InternalRemoveNode(Node);
end;

procedure TCompilerDirectivesTree.DisableIfNode(Node: TCodeTreeNode;
  WithContent: boolean; var Changed: boolean);
  
  procedure RaiseImpossible;
  begin
    raise ECDirectiveParserException.Create(Self,'TCompilerDirectivesTree.DisableIfNode impossible');
  end;
  
  function GetExpr(ExprNode: TCodeTreeNode; out Negated: boolean): string;
  var
    ExprStart: integer;
    ExprEnd: integer;
  begin
    if not GetIfExpression(ExprNode,ExprStart,ExprEnd) then
      RaiseImpossible;
    Result:=copy(Src,ExprStart,ExprEnd-ExprStart);
    Negated:=ExprNode.SubDesc=cdnsIfNdef;
    if (ExprNode.SubDesc=cdnsIfdef) or (ExprNode.SubDesc=cdnsIfNdef) then
      Result:='defined('+Result+')';
  end;
  
  procedure CommentCode(FromPos, ToPos: integer);
  var
    p: LongInt;
    NewSrc: String;
  begin
    p:=FromPos;
    repeat
      // find code
      MoveCursorToPos(p);
      ReadNextAtom;
      if AtomStart>=ToPos then break;
      // there is code to comment
      // = > start comment
      Replace(AtomStart,AtomStart,'(* ');
      p:=AtomStart;
      while (p<FromPos) do begin
        if (Src[p]='(') and (Src[p+1]='*') then
          break;
        inc(p);
      end;
      // end comment
      NewSrc:='*)'+LineEnding;
      Replace(p,p,NewSrc);
      inc(p,length(NewSrc));
    until false;
  end;
  
  procedure DisableContent;
  var
    FromPos: LongInt;
    ToPos: LongInt;
    ChildNode: TCodeTreeNode;
    FirstChild: TCodeTreeNode;
    LastChild: TCodeTreeNode;
  begin
    if not WithContent then begin
      // the content (child nodes) will stay, but the Node will be freed
      // -> move child nodes in front of Node (keep source positions)
      FirstChild:=Node.FirstChild;
      LastChild:=Node.LastChild;
      if FirstChild<>nil then begin
        ChildNode:=FirstChild;
        while ChildNode<>nil do begin
          ChildNode.Parent:=Node.Parent;
          ChildNode:=ChildNode.NextBrother;
        end;
        FirstChild.PriorBrother:=Node.PriorBrother;
        LastChild.NextBrother:=Node;
        if FirstChild.PriorBrother=nil then begin
          if Node.Parent<>nil then
            Node.Parent.FirstChild:=FirstChild;
        end else begin
          FirstChild.PriorBrother.NextBrother:=FirstChild;
        end;
        Node.PriorBrother:=LastChild;
        Node.FirstChild:=nil;
        Node.LastChild:=nil;
      end;
    end else begin
      // free nodes and delete code
      while Node.FirstChild<>nil do
        InternalRemoveNode(Node.FirstChild);
      FromPos:=FindCommentEnd(Src,Node.StartPos,NestedComments);
      ToPos:=Node.NextBrother.StartPos;
      if RemoveDisabledDirectives then begin
        // delete content
        Replace(FromPos,ToPos,'');
      end else begin
        // comment content
        CommentCode(FromPos,ToPos);
      end;
    end;
  end;
  
var
  FromPos: LongInt;
  ToPos: LongInt;
  Expr: String;
  ElseNode: TCodeTreeNode;
  ElseName: String;
  Expr2: String;
  NewSrc: String;
  PrevNode: TCodeTreeNode;
  NewDesc: TCompilerDirectiveNodeDesc;
  NewSubDesc: TCompilerDirectiveNodeDesc;
  Simplified: Boolean;
  ExprNegated: boolean;
  Expr2Negated: boolean;
  p: LongInt;
begin
  if (Node.NextBrother=nil) then
    RaiseImpossible;
  if (Node.Desc<>cdnIf) and (Node.Desc<>cdnElseIf) and (Node.Desc<>cdnElse) then
    RaiseImpossible;
    
  DisableContent;
    
  Changed:=true;
  
  // fix all following elseif and else nodes
  Expr:=GetExpr(Node,ExprNegated);
  ElseNode:=Node.NextBrother;
  while ElseNode<>nil do begin
    if (ElseNode.Desc=cdnElse) or (ElseNode.Desc=cdnElseIf) then begin
      PrevNode:=ElseNode.PriorBrother;
      if (PrevNode.Desc=cdnIf) then begin
        NewDesc:=cdnIf;
        if ElseNode.SubDesc=cdnsIfC then
          NewSubDesc:=cdnsIfC
        else
          NewSubDesc:=cdnsIf; // IFDEF, IF -> IF
      end else begin
        NewDesc:=cdnElseIf;
        if (ElseNode.SubDesc=cdnsElseIf) or (ElseNode.SubDesc=cdnsElse) then
          NewSubDesc:=cdnsElIfC
        else
          NewSubDesc:=cdnsElseIf; // Else, ElseIf -> ElseIF
      end;
      ElseName:=CDNodeSubDescAsString(NewSubDesc);
      // convert {$Else} to {$ElseIf not (Expr)}
      // convert {$ElseIf Expr2} to {$ElseIf (Expr2) and not (Expr)}
      NewSrc:='('+Expr+')';
      if not ExprNegated then
        NewSrc:='not '+NewSrc;
      if ElseNode.Desc=cdnElse then
        NewSrc:='{$'+ElseName+' '+NewSrc+'}'
      else begin
        Expr2:=GetExpr(ElseNode,Expr2Negated);
        NewSrc:='{$'+ElseName+' ('+Expr2+') and '+NewSrc+'}';
      end;
      Replace(ElseNode.StartPos,
              FindCommentEnd(Src,ElseNode.StartPos,NestedComments),NewSrc);
      ElseNode.Desc:=NewDesc;
      ElseNode.SubDesc:=NewSubDesc;
      Simplified:=false;
      CheckAndImproveExpr_Brackets(ElseNode,Simplified);
      CheckAndImproveExpr_IfDefinedMacro(ElseNode,Simplified);
    end else begin
      break;
    end;
    ElseNode:=ElseNode.NextBrother;
  end;
  
  FromPos:=Node.StartPos;
  if RemoveDisabledDirectives then begin
    if (Node.NextBrother.Desc=cdnEnd) and (Node.Desc=cdnIf) then begin
      // remove the whole IF..END block
      ToPos:=FindCommentEnd(Src,Node.NextBrother.StartPos,NestedComments);
      ToPos:=FindLineEndOrCodeAfterPosition(Src,ToPos,SrcLen+1,NestedComments);
    end else begin
      // remove a sub block
      ToPos:=Node.NextBrother.StartPos;
    end;
    if WithContent then begin
      // remove node source with content
      if (FromPos>1) and (Src[FromPos-1] in [#10,#13])
      and (ToPos<=SrcLen) and (Src[ToPos] in [#10,#13]) then begin
        // the directive has a complete line
        // remove the line end too
        inc(ToPos);
        if (ToPos<=SrcLen) and (Src[ToPos] in [#10,#13]) and (Src[ToPos]<>Src[ToPos-1])
        then inc(ToPos);
        if (ToPos<=SrcLen) and (Src[ToPos] in [#10,#13]) then begin
          // there is an empty line behind the directive
          // check if there is an empty line in front of the directive
          p:=FromPos;
          if (p>1) and (Src[p-1] in [#10,#13]) then begin
            dec(p);
            if (p>1) and (Src[p-1] in [#10,#13]) and (Src[p]<>Src[p-1]) then
              dec(p);
            if (p>1) and (Src[p-1] in [#10,#13]) then begin
              // there is an empty line in front of the directive too
              // => remove one empty line
              FromPos:=p;
            end;
          end;
        end;
      end;
      Replace(FromPos,ToPos,'');
    end else begin
      // remove node source keeping content (child node source)
      Replace(FromPos,FindCommentEnd(Src,FromPos,NestedComments),'');
      if Node.NextBrother.Desc=cdnEnd then begin
        ToPos:=FindCommentEnd(Src,Node.NextBrother.StartPos,NestedComments);
        ToPos:=FindLineEndOrCodeAfterPosition(Src,ToPos,SrcLen+1,NestedComments);
        Replace(Node.NextBrother.StartPos,ToPos,'');
      end;
    end;
  end else begin
    // disable directive -> {$off IfDef MacroName}
    Replace(FromPos+1,FromPos+1,'off ');
    if Node.NextBrother.Desc=cdnEnd then
      Replace(Node.NextBrother.StartPos+1,Node.NextBrother.StartPos+1,'off ');
  end;
  
  if Node.NextBrother.Desc=cdnEnd then
    InternalRemoveNode(Node.NextBrother);
  InternalRemoveNode(Node);
end;

procedure TCompilerDirectivesTree.InternalRemoveNode(Node: TCodeTreeNode);
var
  AVLNode: TAVLTreeNode;
  MacroNode: TCompilerMacroStats;
begin
  // clear references
  if Macros<>nil then begin
    AVLNode:=Macros.FindLowest;
    while AVLNode<>nil do begin
      MacroNode:=TCompilerMacroStats(AVLNode.Data);
      if MacroNode.LastDefineNode=Node then
        MacroNode.LastDefineNode:=nil;
      if MacroNode.LastReadNode=Node then
        MacroNode.LastReadNode:=nil;
      AVLNode:=Macros.FindSuccessor(AVLNode);
    end;
  end;

  // free node
  Tree.DeleteNode(Node);
end;

procedure TCompilerDirectivesTree.RemoveEmptyNodes(var Changed: boolean);
var
  Node: TCodeTreeNode;
  NextNode: TCodeTreeNode;
  
  procedure CheckNode;
  begin
    //DebugLn(['CheckNode ',Node.Desc=cdnIf,' ',(Node.NextBrother<>nil),' ',(Node.FirstChild=nil),' ',GetDirective(Node)]);
    case Node.Desc of
    cdnIf,cdnElseIf,cdnElse:
      if (Node.NextBrother<>nil) and (Node.FirstChild=nil) then begin
        case Node.NextBrother.Desc of
        cdnEnd,cdnElseIf,cdnElse:
          begin
            //DebugLn(['CheckNode Checking if empty ...']);
            MoveCursorToPos(Node.StartPos);
            // skip directive
            ReadNextAtom;
            // read the following atom (token or directive)
            ReadNextAtom;
            if AtomStart=Node.NextBrother.StartPos then begin
              // node is empty
              DebugLn(['TCompilerDirectivesTree.RemoveEmptyNodes node only contains spaces and comments ',GetDirective(Node)]);
              DisableIfNode(Node,true,Changed);
            end;
          end;
        end;
      end;
    end;
  end;
  
begin
  //DebugLn(['TCompilerDirectivesTree.RemoveEmptyNodes ']);
  // check nodes from end to start
  Node:=Tree.Root;
  while (Node.NextBrother<>nil) do Node:=Node.NextBrother;
  while (Node.LastChild<>nil) do Node:=Node.LastChild;
  while Node<>nil do begin
    NextNode:=Node.Prior;
    CheckNode;
    Node:=NextNode;
  end;
end;

function TCompilerDirectivesTree.InsertDefine(Position: integer;
  const NewSrc: string; SubDesc: TCompilerDirectiveNodeDesc): TCodeTreeNode;
var
  ParentNode: TCodeTreeNode;
  NextBrotherNode: TCodeTreeNode;
begin
  Replace(Position,Position,NewSrc);
  ParentNode:=FindNodeAtPos(Position);
  if ParentNode=nil then
    ParentNode:=Tree.Root;
  while (ParentNode<>Tree.Root) and (ParentNode.EndPos=Position) do
    ParentNode:=ParentNode.Parent;
  Result:=NodeMemManager.NewNode;
  Result.Desc:=cdnDefine;
  Result.SubDesc:=SubDesc;
  Result.StartPos:=FindNextCompilerDirective(Src,Position,NestedComments);
  Result.EndPos:=FindCommentEnd(Src,Result.StartPos,NestedComments);
  NextBrotherNode:=ParentNode.FirstChild;
  while (NextBrotherNode<>nil) and (NextBrotherNode.StartPos<=Position) do
    NextBrotherNode:=NextBrotherNode.NextBrother;
  if NextBrotherNode<>nil then begin
    Tree.AddNodeInFrontOf(NextBrotherNode,Result);
  end else begin
    Tree.AddNodeAsLastChild(ParentNode,Result);
    if ParentNode.EndPos<Result.EndPos then
      ParentNode.EndPos:=Result.EndPos;
  end;
end;

constructor TCompilerDirectivesTree.Create;
begin
  Tree:=TCodeTree.Create;
  SimplifyExpressions:=true;
  DisableUnusedDefines:=true;
  RemoveDisabledDirectives:=true;
  UndefH2PasFunctions:=true;
end;

destructor TCompilerDirectivesTree.Destroy;
begin
  ClearMacros;
  FreeAndNil(Tree);
  FDefaultDirectiveFuncList.Free;
  inherited Destroy;
end;

procedure TCompilerDirectivesTree.Clear;
begin
  Tree.Clear;
  if Macros<>nil then begin
    Macros.FreeAndClear;
    FreeAndNil(Macros);
  end;
end;

procedure TCompilerDirectivesTree.Parse;
begin
  Parse(Code,NestedComments);
end;

procedure TCompilerDirectivesTree.Parse(aCode: TCodeBuffer;
  aNestedComments: boolean);
  
  procedure RaiseDanglingIFDEF;
  begin
    WriteDebugReport;
    raise ECDirectiveParserException.Create(Self,'missing EndIf');
  end;
  
var
  DirectiveName: PChar;
  Node: TCodeTreeNode;
begin
  {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
  {$R-}
  if (Code=aCode) and (NestedComments=aNestedComments) and (not UpdateNeeded)
  then
    exit;

  Code:=aCode;
  NestedComments:=aNestedComments;
  InitParser;
  
  repeat
    ReadRawNextPascalAtom(Src,SrcPos,AtomStart,NestedComments);
    //DebugLn(['TCompilerDirectivesTree.Parse ',copy(Src,AtomStart,SrcPos-AtomStart)]);
    if SrcPos<=SrcLen then begin
      if (Src[AtomStart]='{') and (Src[AtomStart+1]='$') then begin
        // compiler directive
        DirectiveName:=@Src[AtomStart+2];
        //DebugLn(['ParseCompilerDirectives ',GetIdentifier(DirectiveName)]);
        FDefaultDirectiveFuncList.DoItCaseInsensitive(DirectiveName);
      end;
    end else begin
      break;
    end;
  until false;
  // close nodes
  Node:=CurNode;
  while Node<>nil do begin
    Node.EndPos:=AtomStart;
    Node:=Node.Parent;
  end;
  if CurNode<>Tree.Root then
    RaiseDanglingIFDEF;
  
  {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}
end;

function TCompilerDirectivesTree.UpdateNeeded: boolean;
begin
  Result:=true;
  if (Code=nil) or (Tree=nil) or (Tree.Root=nil) then exit;
  if Code.ChangeStep<>ParseChangeStep then exit;
  Result:=false;
end;

procedure TCompilerDirectivesTree.ReduceCompilerDirectives(
  Undefines, Defines: TStrings; var Changed: boolean);
(*  Check and improve the following cases
  1.  {$DEFINE Name} and Name is never used afterwards -> disable
   
  2.  {$DEFINE Name}
      ... Name is not used here ...
      {$DEFINE Name}
      -> disable first

  3.  {$IFDEF Name}... only comments and spaces ...{$ENDIF}
      -> disable the whole block

  4. {$IFNDEF Name}
       ... only comments and spaces ...
       {$DEFINE Name}
       ... only comments and spaces ...
     {$ENDIF}
     -> disable the IFNDEF and the ENDIF and keep the DEFINE
*)

  function GetMacroNode(p: PChar): TCompilerMacroStats;
  var
    AVLNode: TAVLTreeNode;
  begin
    AVLNode:=Macros.FindKey(p,@ComparePCharWithCompilerMacroStats);
    if AVLNode<>nil then
      Result:=TCompilerMacroStats(AVLNode.Data)
    else
      Result:=nil;
  end;
  
  procedure CheckMacroInExpression(Node: TCodeTreeNode; NameStart: integer;
    Complex: boolean; var Changed: boolean);
  var
    MacroNode: TCompilerMacroStats;
  begin
    MacroNode:=GetMacroNode(@Src[NameStart]);
    if MacroNode=nil then begin
      MacroNode:=TCompilerMacroStats.Create;
      MacroNode.Name:=GetIdentifier(@Src[NameStart]);
      Macros.Add(MacroNode);
    end;
    MacroNode.LastReadNode:=Node;
    
    if not Complex then begin

    end;
  end;
  
  procedure CheckDefine(Node: TCodeTreeNode; var Changed: boolean);
  var
    MacroNode: TCompilerMacroStats;
    NameStart: integer;
    HasValue: boolean;
    ValueStart: integer;
  begin
    if (Node.SubDesc<>cdnsDefine) and (Node.SubDesc<>cdnsUndef)
    and (Node.SubDesc<>cdnsSetC) then exit;
    if not GetDefineNameAndValue(Node,NameStart,HasValue,ValueStart) then exit;
    MacroNode:=GetMacroNode(@Src[NameStart]);
    if MacroNode=nil then begin
      MacroNode:=TCompilerMacroStats.Create;
      MacroNode.Name:=GetIdentifier(@Src[NameStart]);
      Macros.Add(MacroNode);
    end;
    if (MacroNode.LastReadNode=nil) and (MacroNode.LastDefineNode<>nil)
    and (MacroNode.LastDefineNode.Parent=Node.Parent)
    and ((MacroNode.LastDefineNode.SubDesc=cdnsUndef)=(Node.SubDesc=cdnsUndef)) then begin
      // last define was never used -> disable it
      DebugLn(['TCompilerDirectivesTree.ReduceCompilerDirectives this define was already set to this value']);
      DisableDefineNode(MacroNode.LastDefineNode,Changed);
    end;

    MacroNode.LastReadNode:=nil;
    MacroNode.LastDefineNode:=Node;
  end;
  
var
  Node: TCodeTreeNode;
  ExprStart: integer;
  ExprEnd: integer;
  Complex: Boolean;
  AtomCount: Integer;
  NextNode: TCodeTreeNode;
begin
  try
    ResetMacros;
    Node:=Tree.Root;
    while Node<>nil do begin
      NextNode:=Node.Next;

      case Node.Desc of
      cdnIf,cdnElseIf:
        if GetIfExpression(Node,ExprStart,ExprEnd) then begin
          // improve expression
          CheckAndImproveExpr_Brackets(Node,Changed);
          CheckAndImproveExpr_IfDefinedMacro(Node,Changed);
        
          //DebugLn(['TCompilerDirectivesTree.ReduceCompilerDirectives Expr=',copy(Src,ExprStart,ExprEnd-ExprStart)]);
          // check if it is a complex expression or just one macro
          AtomCount:=0;
          if (Node.SubDesc=cdnsIf) or (Node.SubDesc=cdnsIfC)
          or (Node.SubDesc=cdnsElseIf) then begin
            MoveCursorToPos(ExprStart);
            repeat
              ReadNextAtom;
              inc(AtomCount);
            until AtomStart>=ExprEnd;
          end;
          Complex:=AtomCount>1;

          // mark all macros as read
          MoveCursorToPos(ExprStart);
          repeat
            ReadNextAtom;
            if AtomIsIdentifier then begin
              CheckMacroInExpression(Node,AtomStart,Complex,Changed);
            end;
          until AtomStart>=ExprEnd;
        end;
        
      cdnDefine:
        CheckDefine(Node,Changed);
        
      end;
      
      Node:=NextNode;
    end;
    
    DisableAllUnusedDefines(Changed);
    
    MoveIfNotThenDefsUp(Changed);
    
    DisableUnreachableBlocks(Undefines,Defines,Changed);
    
    RemoveEmptyNodes(Changed);
  finally
    ClearMacros;
  end;
end;

procedure TCompilerDirectivesTree.GatherH2PasFunctions(out
  ListOfH2PasFunctions: TFPList; FindDefNodes: boolean);
var
  InInterface: boolean;

  procedure ReadFunction;
  var
    HeaderStart: LongInt;
    HeaderEnd: LongInt;
    FuncName: String;
    IsForward: Boolean;
    BlockLevel: Integer;
    CurH2PasFunc: TH2PasFunction;
    BeginStart: Integer;
    BeginEnd: Integer;
    IsExternal: Boolean;
  begin
    HeaderStart:=AtomStart;
    // read name
    ReadNextAtom;
    if not AtomIsIdentifier then exit;
    FuncName:=GetAtom;
    // read parameter list
    ReadNextAtom;
    if AtomIs('(') then begin
      if not ReadTilBracketClose(')') then exit;
      ReadNextAtom;
    end;
    // read colon
    if not AtomIs(':') then exit;
    // read result type
    ReadNextAtom;
    if not AtomIsIdentifier then exit;
    // read semicolon
    ReadNextAtom;
    if not AtomIs(';') then exit;
    HeaderEnd:=SrcPos;
    // read function modifiers
    IsForward:=false;
    IsExternal:=false;
    repeat
      ReadNextAtom;
      if (AtomStart<=SrcLen)
      and IsKeyWordProcedureSpecifier.DoItCaseInsensitive(@Src[AtomStart])
      then begin
        if UpAtomIs('EXTERNAL') then
          IsExternal:=true;
        if UpAtomIs('FORWARD') then
          IsForward:=true;
        repeat
          ReadNextAtom;
        until (AtomStart>SrcLen) or AtomIs(';');
        HeaderEnd:=SrcPos;
      end else
        break;
    until false;

    // read begin..end block
    BeginStart:=-1;
    BeginEnd:=-1;
    if (not IsForward) and (not InInterface) and (not IsExternal)
    and UpAtomIs('BEGIN') then begin
      BeginStart:=AtomStart;
      BlockLevel:=1;
      repeat
        ReadNextAtom;
        if (AtomStart>SrcLen) then break;
        if UpAtomIs('END') then begin
          dec(BlockLevel);
          if BlockLevel=0 then begin
            BeginEnd:=SrcPos;
            ReadNextAtom;
            if AtomIs(';') then
              BeginEnd:=SrcPos;
            break;
          end;
        end else if UpAtomIs('BEGIN') or UpAtomIs('ASM') then
          inc(BlockLevel);
      until false;
    end else begin
      // undo forward read to make sure that current atom is the last of the function
      MoveCursorToPos(HeaderEnd);
    end;

    // found a function
    //DebugLn(['ReadFunction ',copy(Src,HeaderStart,FuncEnd-HeaderStart)]);
    CurH2PasFunc:=TH2PasFunction.Create;
    CurH2PasFunc.Name:=FuncName;
    CurH2PasFunc.HeaderStart:=HeaderStart;
    CurH2PasFunc.HeaderEnd:=HeaderEnd;
    CurH2PasFunc.BeginStart:=BeginStart;
    CurH2PasFunc.BeginEnd:=BeginEnd;
    CurH2PasFunc.IsForward:=IsForward;
    CurH2PasFunc.InInterface:=InInterface;
    CurH2PasFunc.IsExternal:=IsExternal;
    if ListOfH2PasFunctions=nil then ListOfH2PasFunctions:=TFPList.Create;
    ListOfH2PasFunctions.Add(CurH2PasFunc);
  end;
  
  procedure DoFindDefNodes;
  var
    i: Integer;
    CurH2PasFunc: TH2PasFunction;
    TreeOfForwardFuncs: TAVLTree;
    TreeOfBodyFuncs: TAVLTree;
    AVLNode: TAVLTreeNode;
    BodyAVLNode: TAVLTreeNode;
    BodyFunc: TH2PasFunction;
  begin
    if ListOfH2PasFunctions=nil then exit;
    
    // create a tree of the function definitions
    // and a tree of the function bodies
    TreeOfForwardFuncs:=TAVLTree.Create(@CompareH2PasFuncByNameAndPos);
    TreeOfBodyFuncs:=TAVLTree.Create(@CompareH2PasFuncByNameAndPos);
    for i:=0 to ListOfH2PasFunctions.Count-1 do begin
      CurH2PasFunc:=TH2PasFunction(ListOfH2PasFunctions[i]);
      if CurH2PasFunc.NeedsBody then
        TreeOfForwardFuncs.Add(CurH2PasFunc)
      else if (CurH2PasFunc.BeginStart>0) then
        TreeOfBodyFuncs.Add(CurH2PasFunc);
    end;
    
    // search for every definition the corresponding body
    AVLNode:=TreeOfForwardFuncs.FindLowest;
    while AVLNode<>nil do begin
      CurH2PasFunc:=TH2PasFunction(AVLNode.Data);
      if CurH2PasFunc.DefNode=nil then begin
        BodyAVLNode:=TreeOfBodyFuncs.FindLeftMostKey(Pointer(CurH2PasFunc.Name),
                                                @ComparePCharWithH2PasFuncName);
        if BodyAVLNode<>nil then begin
          // there is at least one body with this name
          repeat
            BodyFunc:=TH2PasFunction(BodyAVLNode.Data);
            if BodyFunc.DefNode=nil then begin
              // this body node with the same name has not yet a definition node
              // => found the corresponding node
              BodyFunc.DefNode:=CurH2PasFunc;
              CurH2PasFunc.DefNode:=BodyFunc;
              break;
            end else begin
              // this body node has already a definition node
              // search next body node with same name
              BodyAVLNode:=TreeOfBodyFuncs.FindSuccessor(BodyAVLNode);
              if (BodyAVLNode=nil)
              or (ComparePCharWithH2PasFuncName(
                                Pointer(CurH2PasFunc.Name),BodyAVLNode.Data)<>0)
              then
                break;
            end;
          until false;
        end;
      end;
      AVLNode:=TreeOfBodyFuncs.FindSuccessor(AVLNode);
    end;
    
    // clean up
    TreeOfForwardFuncs.Free;
    TreeOfBodyFuncs.Free;
  end;

begin
  ListOfH2PasFunctions:=nil;

  InInterface:=false;
  MoveCursorToPos(1);
  repeat
    ReadNextAtom;
    if SrcPos>SrcLen then break;
    if UpAtomIs('FUNCTION') then begin
      ReadFunction;
    end else if UpAtomIs('INTERFACE') then begin
      InInterface:=true;
    end else if UpAtomIs('IMPLEMENTATION') then begin
      InInterface:=false;
    end;
  until false;
  
  if FindDefNodes then
    DoFindDefNodes;
end;

procedure TCompilerDirectivesTree.FixMissingH2PasDirectives(var Changed: boolean
  );
{ Adds the directives around the function bodies, that h2pas forgets to add.

}
type
  TBodyBlock = record
    Definition: TCodeTreeNode;
    FirstBodyFunc: TH2PasFunction;
    LastBodyFunc: TH2PasFunction;
  end;

var
  CurBodyBlock: TBodyBlock;
  MacroNames: TStrings; // the Objects are the TCodeTreeNode
  ListOfH2PasFunctions: TFPList;
  LocalChange: boolean;

  function IsSameDirective(OldNode: TCodeTreeNode; Position: integer;
    out NewNode: TCodeTreeNode): boolean;
  begin
    NewNode:=FindNodeAtPos(Position);
    //if OldNode<>nil then DebugLn(['IsSameDirective OldNode=',OldNode.StartPos,' "',copy(Src,OldNode.StartPos,OldNode.EndPos-OldNode.StartPos),'"']);
    //if NewNode<>nil then DebugLn(['IsSameDirective NewNode=',NewNode.StartPos,' "',copy(Src,NewNode.StartPos,NewNode.EndPos-NewNode.StartPos),'"']);
    Result:=(NewNode<>nil) and (NewNode=OldNode);
  end;
  
  function HasCodeBetween(FromPos, ToPos: integer): boolean;
  begin
    if FromPos<1 then FromPos:=1;
    if FromPos>ToPos then exit(false);
    MoveCursorToPos(FromPos);
    ReadNextAtom;
    Result:=AtomStart<ToPos;
  end;
  
  function GetMacroNameForNode(Node: TCodeTreeNode; out IsNew: boolean): string;
  var
    i: Integer;
  begin
    if MacroNames=nil then
      MacroNames:=TStringList.Create;
    for i:=0 to MacroNames.Count-1 do
      if MacroNames.Objects[i]=Node then begin
        Result:=MacroNames[i];
        IsNew:=false;
        exit;
      end;
    IsNew:=true;
    Result:=H2Pas_Function_Prefix+IntToStr(MacroNames.Count+1);
    MacroNames.AddObject(Result,Node);
  end;
  
  procedure LocalReplace(FromPos, ToPos: integer; const NewSrc: string);
  var
    DiffPos: Integer;
    i: Integer;
    Func: TH2PasFunction;
  begin
    LocalChange:=true;
    Replace(FromPos,ToPos,NewSrc);
    // update positions
    DiffPos:=length(NewSrc)-(ToPos-FromPos);
    if DiffPos<>0 then begin
      for i:=0 to ListOfH2PasFunctions.Count-1 do begin
        Func:=TH2PasFunction(ListOfH2PasFunctions[i]);
        Func.AdjustPositionsAfterInsert(FromPos,ToPos,DiffPos);
      end;
    end;
  end;
  
  procedure StartBodyBlock(BodyFunc: TH2PasFunction; DefNode: TCodeTreeNode);
  begin
    CurBodyBlock.Definition:=DefNode;
    CurBodyBlock.FirstBodyFunc:=BodyFunc;
    CurBodyBlock.LastBodyFunc:=BodyFunc;
  end;
  
  procedure EndBodyBlock;
  var
    MacroName: String;
    InsertPos: LongInt;
    IsNewMacro: boolean;
  begin
    if CurBodyBlock.Definition=nil then exit;
    if CurBodyBlock.Definition<>Tree.Root then begin
      DebugLn(['TCompilerDirectivesTree.FixMissingH2PasDirectives add missing directives']);
      // create unique macro name
      MacroName:=GetMacroNameForNode(CurBodyBlock.Definition,IsNewMacro);
      if IsNewMacro then begin
        // insert $DEFINE
        InsertPos:=FindCommentEnd(Src,CurBodyBlock.Definition.StartPos,NestedComments);
        LocalReplace(InsertPos,InsertPos,LineEnding+'{$DEFINE '+MacroName+'}');
      end;
      // insert $IFDEF
      InsertPos:=FindLineEndOrCodeInFrontOfPosition(Src,
                  CurBodyBlock.FirstBodyFunc.HeaderStart,1,NestedComments,true);
      LocalReplace(InsertPos,InsertPos,LineEnding+'{$IFDEF '+MacroName+'}');
      // insert $ENDIF
      InsertPos:=FindLineEndOrCodeAfterPosition(Src,
                      CurBodyBlock.LastBodyFunc.BeginEnd,1,NestedComments,true);
      LocalReplace(InsertPos,InsertPos,LineEnding+'{$ENDIF '+MacroName+'}');
    end;
    FillChar(CurBodyBlock,SizeOf(TBodyBlock),0);
  end;
  
var
  i: Integer;
  BodyFunc: TH2PasFunction;
  LastDefNode: TCodeTreeNode;
  BodyNode: TCodeTreeNode;
begin
  ListOfH2PasFunctions:=nil;
  MacroNames:=nil;
  LocalChange:=false;
  try
    GatherH2PasFunctions(ListOfH2PasFunctions,true);
    DebugLn(['TCompilerDirectivesTree.FixMissingH2PasDirectives ',ListOfH2PasFunctions=nil]);
    if ListOfH2PasFunctions=nil then exit;
    FillChar(CurBodyBlock,SizeOf(TBodyBlock),0);
    LastDefNode:=nil;
    for i:=0 to ListOfH2PasFunctions.Count-1 do begin
      BodyFunc:=TH2PasFunction(ListOfH2PasFunctions[i]);
      //DebugLn(['TCompilerDirectivesTree.FixMissingH2PasDirectives DefNode=',(BodyFunc.DefNode<>nil),' Body="',copy(Src,BodyFunc.HeaderStart,BodyFunc.HeaderEnd-BodyFunc.HeaderStart),'"']);
      if (BodyFunc.BeginStart<1) or (BodyFunc.DefNode=nil) then
        continue;
      BodyNode:=FindNodeAtPos(BodyFunc.HeaderStart);
      if BodyNode<>Tree.Root then begin
        // this body has already a directive block
        continue;
      end;
      // this function is a body and has a definition
      
      if (CurBodyBlock.LastBodyFunc<>nil)
      and HasCodeBetween(CurBodyBlock.LastBodyFunc.BeginEnd,BodyFunc.HeaderStart)
      then begin
        // there is code between last function body and current function body
        // end last block
        EndBodyBlock;
      end;
      
      if not IsSameDirective(LastDefNode,
        BodyFunc.DefNode.HeaderStart,LastDefNode)
      then begin
        // another directive block => end last block
        EndBodyBlock;
      end;
      
      if (CurBodyBlock.Definition=nil) then begin
        // a new block
        StartBodyBlock(BodyFunc, LastDefNode);
      end else begin
        // continue current block
        CurBodyBlock.LastBodyFunc:=BodyFunc;
      end;
    end;
    // end last block
    EndBodyBlock;
    
  finally
    if ListOfH2PasFunctions<>nil then
      for i:=0 to ListOfH2PasFunctions.Count-1 do
        TObject(ListOfH2PasFunctions[i]).Free;
    ListOfH2PasFunctions.Free;
    MacroNames.Free;
    
    if LocalChange then begin
      Changed:=true;
      Parse(Code,NestedComments);
    end;
  end;
end;

function TCompilerDirectivesTree.NodeStartToCodePos(Node: TCodeTreeNode; out
  CodePos: TCodeXYPosition): boolean;
begin
  CodePos.Code:=nil;
  CodePos.Y:=0;
  CodePos.X:=0;
  if (Node=nil) or (Code=nil) then exit(false);
  CodePos.Code:=Code;
  Code.AbsoluteToLineCol(Node.StartPos,CodePos.Y,CodePos.X);
  Result:=true;
end;

function TCompilerDirectivesTree.FindResourceDirective(const Filename: string;
  StartPos: integer): TCodeTreeNode;
begin
  if Tree=nil then exit(nil);
  Result:=Tree.Root;
  while Result<>nil do begin
    if (Result.StartPos>=StartPos)
    and IsResourceDirective(Result,Filename) then exit;
    Result:=Result.Next;
  end;
end;

function TCompilerDirectivesTree.IsResourceDirective(Node: TCodeTreeNode;
  const Filename: string): boolean;
// search for {$R filename}
// if filename='' then search for any {$R } directive
// Beware: do not find {$R+}
var
  p: LongInt;
begin
  Result:=false;
  if (Node=nil) or (Node.Desc<>cdnDefine) or (Node.SubDesc<>cdnsOther) then exit;
  p:=Node.StartPos;
  if (Node.EndPos-p>=5) and (Src[p]='{') and (Src[p+1]='$') and (Src[p+2]='R')
  and IsSpaceChar[Src[p+3]] then
  begin
    if (Filename='') then exit(true);
    inc(p,4);
    while (p<Node.EndPos) and IsSpaceChar[Src[p]] do inc(p);
    if CompareFilenamesIgnoreCase(Filename,copy(Src,p,Node.EndPos-p-1))=0 then
      exit(true);
  end;
end;

function TCompilerDirectivesTree.FindIncludeDirective(const Filename: string;
  StartPos: integer): TCodeTreeNode;
begin
  if Tree=nil then exit(nil);
  Result:=Tree.Root;
  while Result<>nil do begin
    if (Result.StartPos>=StartPos)
    and IsIncludeDirective(Result,Filename) then exit;
    Result:=Result.Next;
  end;
end;

function TCompilerDirectivesTree.IsIncludeDirective(Node: TCodeTreeNode;
  const Filename: string): boolean;
// search for {$I filename}
// if filename='' then search for any {$I } directive
// Beware: do not find {$I+}
var
  p: LongInt;
  FilenameStartPos: integer;
  FilenameEndPos: integer;
  CommentStart: integer;
  CommentEnd: integer;
begin
  Result:=false;
  //debugln(['TCompilerDirectivesTree.IsIncludeDirective ',CDNodeDescAsString(Node.Desc)]);
  if (Node=nil) or (Node.Desc<>cdnInclude) then exit;
  p:=Node.StartPos;
  if (Node.EndPos-p>=5) and (Src[p]='{') and (Src[p+1]='$') and (Src[p+2]='I')
  then begin
    if (Filename='') then exit(true);
    if FindNextIncludeDirective(Src,p,NestedComments,
      FilenameStartPos,FilenameEndPos,CommentStart,CommentEnd)=p then
    begin;
      if CompareFilenamesIgnoreCase(Filename,
        copy(Src,FilenameStartPos,FilenameEndPos-FilenameStartPos))=0
      then
        exit(true);
    end;
  end;
end;

function TCompilerDirectivesTree.GetDirectiveName(Node: TCodeTreeNode): string;
begin
  Result:=GetIdentifier(@Src[Node.StartPos+2]);
end;

function TCompilerDirectivesTree.GetDirective(Node: TCodeTreeNode): string;
begin
  Result:=copy(Src,Node.StartPos,
               FindCommentEnd(Src,Node.StartPos,NestedComments)-Node.StartPos);
end;

function TCompilerDirectivesTree.GetIfExpression(Node: TCodeTreeNode;
  out ExprStart, ExprEnd: integer): boolean;
var
  p: LongInt;
begin
  Result:=false;
  ExprStart:=-1;
  ExprEnd:=-1;
  p:=Node.StartPos+2;
  if p>SrcLen then exit;
  while (p<=SrcLen) and IsIdentChar[Src[p]] do inc(p);
  if (p>SrcLen) or (not IsSpaceChar[Src[p]]) then exit;
  inc(p);
  ExprStart:=p;
  while (p<=SrcLen) and (Src[p]<>'}') do inc(p);
  ExprEnd:=p;
  Result:=true;
end;

function TCompilerDirectivesTree.GetIfExpressionString(Node: TCodeTreeNode
  ): string;
var
  ExprStart: integer;
  ExprEnd: integer;
begin
  if not GetIfExpression(Node,ExprStart,ExprEnd) then
    Result:=''
  else
    Result:=copy(Src,ExprStart,ExprEnd-ExprStart);
end;

function TCompilerDirectivesTree.IsIfExpressionSimple(Node: TCodeTreeNode; out
  NameStart: integer): boolean;
var
  p: LongInt;
begin
  Result:=false;
  NameStart:=-1;
  // skip {$
  p:=Node.StartPos+2;
  if p>SrcLen then exit;
  // skip directive name
  while (p<=SrcLen) and IsIdentChar[Src[p]] do inc(p);
  // skip space
  if (p>SrcLen) or (not IsSpaceChar[Src[p]]) then exit;
  while (p<=SrcLen) and IsSpaceChar[Src[p]] do inc(p);
  if (p>SrcLen) or (not IsIdentStartChar[Src[p]]) then exit;
  // the expression starts with word
  NameStart:=p;
  if (Node.SubDesc=cdnsIfdef) or (Node.SubDesc=cdnsIfndef) then begin
    // IFDEF and IFNDEF only test the first word
    exit(true);
  end;
  // skip first word
  while (p<=SrcLen) and (IsIdentChar[Src[p]]) do inc(p);
  // skip space
  while (p<=SrcLen) and IsSpaceChar[Src[p]] do inc(p);
  if (p>SrcLen) or (Src[p]='}') then begin
    // the expression only contains one word
    exit(true);
  end;
  Result:=false;
end;

function TCompilerDirectivesTree.FindNameInIfExpression(Node: TCodeTreeNode;
  Identifier: PChar): integer;
var
  p: LongInt;
begin
  Result:=-1;
  // skip {$
  p:=Node.StartPos+2;
  if p>SrcLen then exit;
  // skip directive name
  while (p<=SrcLen) and IsIdentChar[Src[p]] do inc(p);
  // read expression
  while (p<=SrcLen) do begin
    if Src[p]='}' then exit;
    if IsIdentStartChar[Src[p]] then begin
      if CompareIdentifierPtrs(@Src[p],Identifier)=0 then
        exit(p);
      if (Node.SubDesc=cdnsIfdef) or (Node.SubDesc=cdnsIfndef) then begin
        // IFDEF and IFNDEF have only one word
        exit;
      end;
      while (p<=SrcLen) and (IsIdentChar[Src[p]]) do inc(p);
    end else begin
      inc(p);
    end;
  end;
end;

function TCompilerDirectivesTree.GetDefineNameAndValue(
  DefineNode: TCodeTreeNode; out NameStart: integer; out HasValue: boolean; out
  ValueStart: integer): boolean;
var
  p: LongInt;
begin
  Result:=false;
  NameStart:=-1;
  HasValue:=false;
  ValueStart:=-1;
  p:=DefineNode.StartPos+2;
  if p>SrcLen then exit;
  // skip keyword
  while (p<=SrcLen) and (IsIdentChar[Src[p]]) do inc(p);
  while (p<=SrcLen) and (IsSpaceChar[Src[p]]) do inc(p);
  // check name
  if p>SrcLen then exit;
  NameStart:=p;
  if not IsIdentStartChar[Src[p]] then exit;
  Result:=true;
  
  // skip name
  while (p<=SrcLen) and (IsIdentChar[Src[p]]) do inc(p);
  while (p<=SrcLen) and (IsSpaceChar[Src[p]]) do inc(p);
  if p>SrcLen then exit;
  if (Src[p]=':') and (p<SrcLen) and (Src[p+1]='=') then begin
    // has value
    HasValue:=true;
    inc(p,2);
    while (p<=SrcLen) and (IsSpaceChar[Src[p]]) do inc(p);
    ValueStart:=p;
  end;
end;

function TCompilerDirectivesTree.DefineUsesName(DefineNode: TCodeTreeNode;
  Identifier: PChar): boolean;
var
  p: LongInt;
begin
  Result:=false;
  p:=DefineNode.StartPos+2;
  if p>SrcLen then exit;
  // skip keyword
  while (p<=SrcLen) and (IsIdentChar[Src[p]]) do inc(p);
  while (p<=SrcLen) and (IsSpaceChar[Src[p]]) do inc(p);
  // check name
  if p>SrcLen then exit;
  Result:=CompareIdentifierPtrs(@Src[p],Identifier)=0;
end;

function TCompilerDirectivesTree.NodeIsEmpty(Node: TCodeTreeNode;
  IgnoreComments: boolean): boolean;
var
  DirectiveEndPos: LongInt;
begin
  if (Node=nil) then exit(true);
  if Node.FirstChild<>nil then exit(false);
  case Node.Desc of
  cdnNone: exit(true);
  cdnRoot: exit(false); // root is never empty, can not be deleted
  cdnDefine: exit(true);
  cdnIf,
  cdnElseIf,
  cdnElse:
    begin
      if Node.NextBrother=nil then exit(false); // maybe continued in another file
      MoveCursorToPos(Node.StartPos);
      // skip directive
      ReadNextAtom;
      DirectiveEndPos:=SrcPos;
      // read the following atom (token or directive)
      ReadNextAtom;
      if AtomStart=Node.NextBrother.StartPos then begin
        if IgnoreComments then
          exit(true)
        else if FindNextNonSpace(Src,DirectiveEndPos)<AtomStart then
          exit(false)
        else
          exit(true);
      end;
    end;
  cdnEnd: exit(false);
  end;
end;

function TCompilerDirectivesTree.FindNodeAtPos(p: integer): TCodeTreeNode;
begin
  Result:=Tree.Root;
  while Result<>nil do begin
    if Result.StartPos>p then
      exit(Result.Parent);
    if (Result.EndPos>p)
    or  ((Result.EndPos=p) and (Result.NextBrother<>nil)
          and (Result.NextBrother.StartPos>p))
    then begin
      // p is in range of Result => check children
      if (Result.FirstChild=nil)
      or (Result.FirstChild.StartPos>p) then
        exit;
      Result:=Result.FirstChild;
    end else begin
      // p is behind => next
      if Result.NextBrother<>nil then
        Result:=Result.NextBrother
      else
        exit(Result.Parent);
    end;
  end;
end;

procedure TCompilerDirectivesTree.MoveCursorToPos(p: integer);
begin
  SrcPos:=p;
  AtomStart:=p;
end;

procedure TCompilerDirectivesTree.ReadNextAtom;
begin
  //DebugLn(['TCompilerDirectivesTree.ReadNextAtom START ',AtomStart,'-',SrcPos,' ',Src[SrcPos]]);
  ReadRawNextPascalAtom(Src,SrcPos,AtomStart,NestedComments);
  //DebugLn(['TCompilerDirectivesTree.ReadNextAtom END ',AtomStart,'-',SrcPos,' ',copy(Src,AtomStart,SrcPos-AtomStart)]);
end;

function TCompilerDirectivesTree.ReadTilBracketClose(CloseBracket: char
  ): boolean;
begin
  Result:=false;
  repeat
    ReadNextAtom;
    if AtomStart>SrcLen then exit;
    if SrcPos-AtomStart=1 then begin
      if Src[AtomStart]=CloseBracket then
        exit(true)
      else if Src[AtomStart]='(' then
        ReadTilBracketClose(')')
      else if Src[AtomStart]='[' then
        ReadTilBracketClose(']');
    end;
  until false;
end;

function TCompilerDirectivesTree.AtomIs(const s: shortstring): boolean;
var
  len: Integer;
  i: Integer;
begin
  len:=length(s);
  if (len<>SrcPos-AtomStart) then exit(false);
  if SrcPos>SrcLen then exit(false);
  for i:=1 to len do
    if Src[AtomStart+i-1]<>s[i] then exit(false);
  Result:=true;
end;

function TCompilerDirectivesTree.UpAtomIs(const s: shortstring): boolean;
var
  len: Integer;
  i: Integer;
begin
  len:=length(s);
  if (len<>SrcPos-AtomStart) then exit(false);
  if SrcPos>SrcLen then exit(false);
  for i:=1 to len do
    if UpChars[Src[AtomStart+i-1]]<>s[i] then exit(false);
  Result:=true;
end;

function TCompilerDirectivesTree.AtomIsIdentifier: boolean;
var
  p: Integer;
begin
  if (AtomStart>=SrcPos) then exit(false);
  if (SrcPos>SrcLen) or (SrcPos-AtomStart>255) then exit(false);
  if not IsIdentStartChar[Src[AtomStart]] then exit(false);
  p:=AtomStart+1;
  while (p<SrcPos) do begin
    if not IsIdentChar[Src[p]] then exit(false);
    inc(p);
  end;
  Result:=true;
end;

function TCompilerDirectivesTree.GetAtom: string;
begin
  Result:=copy(Src,AtomStart,SrcPos-AtomStart);
end;

procedure TCompilerDirectivesTree.Replace(FromPos, ToPos: integer;
  const NewSrc: string);
var
  Node: TCodeTreeNode;
  DiffPos: Integer;
begin
  //DebugLn(['TCompilerDirectivesTree.Replace ',FromPos,'-',ToPos,' Old="',copy(Src,FromPos,ToPos-FromPos),'" New="',NewSrc,'"']);
  IncreaseChangeStep;
  Code.Replace(FromPos,ToPos-FromPos,NewSrc);
  Src:=Code.Source;
  SrcLen:=length(Src);
  // update positions
  DiffPos:=length(NewSrc)-(ToPos-FromPos);
  if DiffPos<>0 then begin
    Node:=Tree.Root;
    while Node<>nil do begin
      AdjustPositionAfterInsert(Node.StartPos,true,FromPos,ToPos,DiffPos);
      AdjustPositionAfterInsert(Node.EndPos,false,FromPos,ToPos,DiffPos);
      Node:=Node.Next;
    end;
  end;
end;

procedure TCompilerDirectivesTree.IncreaseChangeStep;
begin
  if FChangeStep<>$7fffffff then
    inc(FChangeStep)
  else
    FChangeStep:=-$7fffffff;
end;

procedure TCompilerDirectivesTree.ResetMacros;
begin
  if Macros<>nil then
    Macros.FreeAndClear
  else
    Macros:=TAVLTree.Create(@CompareCompilerMacroStats);
end;

procedure TCompilerDirectivesTree.ClearMacros;
begin
  if Macros<>nil then begin
    Macros.FreeAndClear;
    FreeAndNil(Macros);
  end;
end;

procedure TCompilerDirectivesTree.WriteDebugReport;
var
  Node: TCodeTreeNode;
begin
  DebugLn(['TCompilerDirectivesTree.WriteDebugReport ']);
  if Tree<>nil then begin
    Node:=Tree.Root;
    while Node<>nil do begin
      DebugLn([GetIndentStr(Node.GetLevel*2)+CDNodeDescAsString(Node.Desc),' ',GetDirective(Node)]);
      Node:=Node.Next;
    end;
  end;
end;

{ TH2PasFunction }

function TH2PasFunction.NeedsBody: boolean;
begin
  Result:=(IsForward or InInterface) and (not IsExternal) and (BeginStart<0);
end;

procedure TH2PasFunction.AdjustPositionsAfterInsert(FromPos, ToPos,
  DiffPos: integer);
begin
  AdjustPositionAfterInsert(HeaderStart,true,FromPos,ToPos,DiffPos);
  AdjustPositionAfterInsert(HeaderEnd,false,FromPos,ToPos,DiffPos);
  AdjustPositionAfterInsert(BeginStart,true,FromPos,ToPos,DiffPos);
  AdjustPositionAfterInsert(BeginEnd,false,FromPos,ToPos,DiffPos);
end;

{ ECDirectiveParserException }

constructor ECDirectiveParserException.Create(ASender: TCompilerDirectivesTree;
  const AMessage: string);
begin
  inherited Create(AMessage);
  Sender:=ASender;
end;

end.

