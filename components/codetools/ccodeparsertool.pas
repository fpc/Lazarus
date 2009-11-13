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
    A simple C parser.
    
  Predefined C macros:
    __LINE__  current source file line number as decimal
    __FILE__  current source filename
    __DATE__  current date: Apr 21 1990 or Jan  1 2008  (note the space in front of the 1)
    __TIME__  current time "hh:mm:ss"
    __STDC__  1
    
}
//  Predefined gcc macros:
//    __attribute__((packed))
//      Examples:
//        typedef struct {
//             uint8_t b[6];
//        } __attribute__((packed)) bdaddr_t;
//        struct __attribute__((packed)) {
//                typeof(*(ptr)) __v;
//        } *__p = (void *) (ptr);

//ToDo:

// void * __strong _reserved0;

//struct {
//       NSUInteger _isEmpty:1;
//       NSUInteger _hasSingleRange:1;
//       NSUInteger _cacheValid:1;
//       NSUInteger _reservedArrayBinderController:29;
//   } _indexSetFlags;
//
//_indexSetFlags: bitpacked record
//  _isEmty: 0..1;
//  _hasSingleRange: 0..1;
//  _cacheValid: 0..1;
//  _reservedArrayBinderController: 0..((1 shl 29)-1);
//end;

//union {
//       struct {
//           NSRange _range;
//       } _singleRange;
//       struct {
//           void *__strong _data;
//           void *_reserved;
//       } _multipleRanges;
//   } _internal;
//
//_internal: record
//  case byte of
//    0: (_singleRange:
//          record
//            _range: NSRange;
//          end;
//       );
//    1: (_multipleRanges:
//          record
//            _data: pointer;
//            _reserved: pointer;
//          end;
//       );
//end;

unit CCodeParserTool;

{$mode objfpc}{$H+}

interface

{$I codetools.inc}

{off $DEFINE VerboseCCodeParser}
{off $DEFINE VerboseCDirectives}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, FileProcs, CodeToolsStructs, BasicCodeTools,
  KeywordFuncLists, LinkScanner, CodeAtom, CodeCache, AVL_Tree,
  CodeToolsStrConsts, CodeTree, NonPascalCodeTools;

type
  TCCodeNodeDesc = word;

const
  // descriptors
  ccnBase           = 1000;
  ccnNone           =  0+ccnBase;
  
  ccnRoot           =  1+ccnBase;
  ccnDirective      =  2+ccnBase;// e.g. "#define a" ,can be multiple lines, EndPos at line end
  ccnExtern         =  3+ccnBase;// e.g. extern "C" {}
  ccnEnumBlock      =  4+ccnBase;// e.g. enum {};
  ccnEnumID         =  5+ccnBase;// e.g. name = value;
  ccnConstant       =  6+ccnBase;// e.g. 1
  ccnTypedef        =  7+ccnBase;// e.g. typedef int TInt;
  ccnStruct         =  8+ccnBase;// e.g. struct{}
  ccnStructAlias    =  9+ccnBase;// e.g. struct name
  ccnUnion          = 10+ccnBase;// e.g. union{}
  ccnVariable       = 11+ccnBase;// e.g. int i
  ccnFunction       = 12+ccnBase;// e.g. int i()
  ccnName           = 13+ccnBase;// e.g. i
  ccnFuncParamList  = 14+ccnBase;// e.g. ()
  ccnFuncParameter  = 15+ccnBase;// e.g. ()
  ccnStatementBlock = 16+ccnBase;// e.g. {}

  // values for Node.SubDesc
  ccnsNone             =  0;
  // values for Node.SubDesc if Node.Desc=ccnDirective
  ccnsDirectiveIf      =  1;
  ccnsDirectiveIfDef   =  2;
  ccnsDirectiveIfNDef  =  3;
  ccnsDirectiveElIf    =  4;
  ccnsDirectiveElse    =  5;
  ccnsDirectiveEndIf   =  6;
  ccnsDirectiveDefine  =  7;
  ccnsDirectiveUndef   =  8;
  ccnsDirectiveInclude =  9;
  ccnsDirectiveLine    = 10;
  ccnsDirectiveError   = 11;
  ccnsDirectivePragma  = 12;

type
  TCCodeParserTool = class;

  { ECCodeParserException }

  ECCodeParserException = class(Exception)
  public
    Sender: TCCodeParserTool;
    constructor Create(ASender: TCCodeParserTool; const AMessage: string);
  end;
  
  TCCodeParserIfStackItem = record
    StartPos: integer;
  end;
  PCCodeParserIfStackItem = ^TCCodeParserIfStackItem;

  { TCCodeParserTool }

  TCCodeParserTool = class
  private
    FChangeStep: integer;
    FDefaultTokenList: TKeyWordFunctionList;
    FIfStack: PCCodeParserIfStackItem;
    FIfStackCapacity: integer;

    function OtherToken: boolean;
    function DirectiveToken: boolean;
    function EnumToken: boolean;
    function ExternToken: boolean;
    function CurlyBracketCloseToken: boolean;
    function TypedefToken: boolean;
    function StructToken: boolean;
    procedure InitKeyWordList;

    procedure InitParser;
    procedure CreateChildNode(Desc: TCCodeNodeDesc);
    procedure EndChildNode;
    procedure CloseNodes;
    
    procedure ReadVariable(AsParameter: boolean);
    procedure ReadParameterList;
    procedure ReadEnum;
    procedure ReadStruct;
    procedure ReadUnion;
    procedure ReadConstant;
    procedure Read__attribute__;
    
    procedure IncIfLevel(StartPos: integer);

    procedure RaiseException(const AMessage: string; ReportPos: integer = 0);
    procedure RaiseExpectedButAtomFound(const AToken: string; ReportPos: integer = 0);
  public
    Code: TCodeBuffer;
    Src: string;
    SrcLen: integer;
    Tree: TCodeTree;
    CurNode: TCodeTreeNode;
    SrcPos: Integer;
    AtomStart: integer;
    IfLevel: integer;
    ParseChangeStep: integer;// = Code.ChangeStep at the time of last Parse

    VisibleEditorLines: integer;
    JumpCentered: boolean;
    CursorBeyondEOL: boolean;
    ParseDirectives: boolean;// default is true

    LastSrcPos: integer;
    LastAtomStart: integer;
    
    LastErrorMsg: string;
    LastErrorPos: integer;  // the position where the code does no make sense
    LastErrorReportPos: integer; // if the position that gives a human a clue what went wrong
                             // normally LastErrorReportPos=LastErrorPos
                             // but if a closing bracket is missing LastErrorReportPos points
                             // to ( and ErrorPos to next atom

    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure Parse;
    procedure Parse(aCode: TCodeBuffer);
    function UpdateNeeded: boolean;

    function FindDeepestNodeAtPos(P: integer;
      ExceptionOnNotFound: boolean): TCodeTreeNode; inline;
    function FindDeepestNodeAtPos(StartNode: TCodeTreeNode; P: integer;
      ExceptionOnNotFound: boolean): TCodeTreeNode;
    function CaretToCleanPos(Caret: TCodeXYPosition;
        out CleanPos: integer): integer;  // 0=valid CleanPos
                          //-1=CursorPos was skipped, CleanPos between two links
                          // 1=CursorPos beyond scanned code
                          //-2=X,Y beyond source
    function CleanPosToCodePos(CleanPos: integer;
        out CodePos:TCodePosition): boolean; // true=ok, false=invalid CleanPos
    function CleanPosToCaret(CleanPos: integer;
        out Caret:TCodeXYPosition): boolean; // true=ok, false=invalid CleanPos
    function CleanPosToCaretAndTopLine(CleanPos: integer;
        out Caret:TCodeXYPosition; out NewTopLine: integer): boolean; // true=ok, false=invalid CleanPos
    function CleanPosToStr(CleanPos: integer): string;
    function MainFilename: string;

    procedure MoveCursorToPos(p: integer);
    procedure MoveCursorToNode(Node: TCodeTreeNode);
    procedure ReadNextAtom;
    procedure ReadNextAtomSkipDirectives;
    procedure ReadRawNextAtom;
    procedure UndoReadNextAtom;
    function ReadTilBracketClose(ExceptionOnNotFound: boolean): boolean;
    function AtomIs(const s: shortstring): boolean;
    function AtomIsChar(const c: char): boolean;
    function AtomIsCharOfSet(const s: shortstring): boolean;
    function UpAtomIs(const s: shortstring): boolean;
    function AtomIsIdentifier: boolean;
    function AtomIsStringConstant: boolean;
    function GetAtom: string;
    function LastAtomIs(const s: shortstring): boolean;
    function GetLastAtom: string;
    function ExtractCode(StartPos, EndPos: integer;
                         WithDirectives: boolean = false): string;// extract code without comments

    function GetFirstNameNode(Node: TCodeTreeNode): TCodeTreeNode;
    function ExtractVariableName(VarNode: TCodeTreeNode): string;
    function ExtractVariableType(VarNode: TCodeTreeNode;
                                 WithDirectives: boolean = false): string;
    function ExtractFunctionName(FuncNode: TCodeTreeNode): string;
    function GetFunctionParamListNode(Node: TCodeTreeNode): TCodeTreeNode;
    function ExtractFunctionParamList(FuncNode: TCodeTreeNode): string;
    function ExtractFunctionType(FuncNode: TCodeTreeNode;
                                 WithDirectives: boolean = false): string;
    function ExtractFunctionResultType(FuncNode: TCodeTreeNode;
                                       WithDirectives: boolean = false;
                                       WithBrackets: boolean = true): string;
    function IsPointerToFunction(FuncNode: TCodeTreeNode): boolean;
    function ExtractParameterName(ParamNode: TCodeTreeNode): string;
    function ExtractParameterType(ParamNode: TCodeTreeNode;
                                  WithDirectives: boolean = false): string;
    function ExtractEnumBlockName(EnumBlockNode: TCodeTreeNode): string;
    function ExtractEnumIDName(EnumIDNode: TCodeTreeNode): string;
    function ExtractEnumIDValue(EnumIDNode: TCodeTreeNode;
                                WithDirectives: boolean = false): string;
    function ExtractStructName(StructNode: TCodeTreeNode): string;
    function ExtractUnionName(UnionNode: TCodeTreeNode): string;
    function ExtractTypedefName(TypedefNode: TCodeTreeNode): string;
    function ExtractDirectiveAction(DirectiveNode: TCodeTreeNode): string;
    function ExtractDirectiveFirstAtom(DirectiveNode: TCodeTreeNode): string;
    function ExtractDirectiveParams(DirectiveNode: TCodeTreeNode): string;
    function ExtractDefine(DefineNode: TCodeTreeNode;
                           out MacroName, MacroParamList, MacroValue: string): boolean;

    function FindDirectiveBlockEnd(InnerNode: TCodeTreeNode): TCodeTreeNode;
    function FindElseOrEndIf(IfOrElseNode: TCodeTreeNode): TCodeTreeNode;// find corresponding #EndIf or #Else or #ElIf
    function FindEndIf(IfOrElseNode: TCodeTreeNode): TCodeTreeNode;// find corresponding #EndIf
    function FindEnclosingIFNDEF: TCodeTreeNode;// finds the typical IFNDEF that encloses the whole header source

    procedure Replace(FromPos, ToPos: integer; const NewSrc: string);

    procedure IncreaseChangeStep;
    procedure WriteDebugReport;
    procedure CheckNodeTool(Node: TCodeTreeNode);
    function NodeAsString(Node: TCodeTreeNode): string;

    property ChangeStep: integer read FChangeStep;
  end;
  
function CCNodeDescAsString(Desc: TCCodeNodeDesc; SubDesc: TCCodeNodeDesc = 0): string;
procedure InitCCodeKeyWordLists;

var
  IsCCodeFunctionModifier: TKeyWordFunctionList = nil;
  IsCCodeCustomOperator: TKeyWordFunctionList = nil;

implementation

var
  KeyWordLists: TFPList;

function CCNodeDescAsString(Desc: TCCodeNodeDesc; SubDesc: TCCodeNodeDesc): string;
begin
  case Desc of
  ccnNone          : Result:='None';
  ccnRoot          : Result:='Root';
  ccnDirective     :
    begin
      Result:='Directive';
      case SubDesc of
      ccnsDirectiveIf      : Result:=Result+'.If';
      ccnsDirectiveIfDef   : Result:=Result+'.IfDef';
      ccnsDirectiveIfNDef  : Result:=Result+'.IfNDef';
      ccnsDirectiveElIf    : Result:=Result+'.ElIf';
      ccnsDirectiveElse    : Result:=Result+'.Else';
      ccnsDirectiveEndIf   : Result:=Result+'.EndIf';
      ccnsDirectiveDefine  : Result:=Result+'.Define';
      ccnsDirectiveUndef   : Result:=Result+'.Undef';
      ccnsDirectiveInclude : Result:=Result+'.Include';
      ccnsDirectiveLine    : Result:=Result+'.Line';
      ccnsDirectiveError   : Result:=Result+'.Error';
      ccnsDirectivePragma  : Result:=Result+'.Pragma';
      end;
    end;
  ccnExtern        : Result:='extern-block';
  ccnEnumBlock     : Result:='enum-block';
  ccnEnumID        : Result:='enum-ID';
  ccnConstant      : Result:='constant';
  ccnTypedef       : Result:='typedef';
  ccnStruct        : Result:='struct';
  ccnStructAlias   : Result:='struct-alias';
  ccnUnion         : Result:='union';
  ccnVariable      : Result:='variable';
  ccnFunction      : Result:='function';
  ccnName          : Result:='name';
  ccnFuncParamList : Result:='function-param-list';
  ccnFuncParameter : Result:='function-parameter';
  ccnStatementBlock: Result:='statement-block';
  else          Result:='?('+IntToStr(Desc)+')';
  end;
end;

procedure InitCCodeKeyWordLists;
begin
  if KeyWordLists<>nil then exit;
  KeyWordLists:=TFPList.Create;
  
  IsCCodeFunctionModifier:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(IsCCodeFunctionModifier);
  with IsCCodeFunctionModifier do begin
    Add('static'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('inline'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;

  IsCCodeCustomOperator:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(IsCCodeCustomOperator);
  with IsCCodeCustomOperator do begin
    Add('+'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('-'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('*'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('/'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('|'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('&'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('='     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('++'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('--'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('+='    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('-='    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('*='    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('/='    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('&='    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('|='    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('=='    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('!='    ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
end;

{ ECCodeParserException }

constructor ECCodeParserException.Create(ASender: TCCodeParserTool;
  const AMessage: string);
begin
  inherited Create(AMessage);
  Sender:=ASender;
end;

{ TCCodeParserTool }

function TCCodeParserTool.OtherToken: boolean;
begin
  Result:=true;
  if AtomIsChar(';') then
    // ignore
  else if AtomIsIdentifier then begin
    ReadVariable(false);
  end else
    RaiseException('unexpected token '+GetAtom);
end;

function TCCodeParserTool.DirectiveToken: boolean;

  procedure ReadExpression;
  var
    BracketLevel: Integer;
    NeedBracket: Boolean;
  begin
    BracketLevel:=0;
    repeat
      ReadRawNextCAtom(Src,SrcPos,AtomStart);
      if AtomStart>SrcLen then
        RaiseException('missing expression');
      if Src[AtomStart] in [#10,#13] then begin
        if BracketLevel>0 then
          RaiseException('missing )');
        break;
      end;
      if AtomIsChar('(') then begin
        // in front of a ( there can
        inc(BracketLevel);
      end else if AtomIsChar(')') then begin
        if BracketLevel=0 then
          RaiseException(') without (');
        dec(BracketLevel);
      end else if AtomIsCharOfSet('!+-*/><')
      or AtomIs('!=') or AtomIs('==') or AtomIs('<=') or AtomIs('>=')
      or AtomIs('&&') or AtomIs('||') or AtomIs('<<') or AtomIs('>>')
      then begin
        // valid operator
      end else if IsIdentChar[Src[AtomStart]] then begin
        if AtomIs('defined') then begin
          //    read  defined(macro)
          // or read  defined macro
          ReadRawNextCAtom(Src,SrcPos,AtomStart);
          if AtomIsChar('(') then begin
            NeedBracket:=true;
            ReadRawNextCAtom(Src,SrcPos,AtomStart);
          end else begin
            NeedBracket:=false;
          end;
          if not AtomIsIdentifier then
            RaiseExpectedButAtomFound('macro');
          if NeedBracket then begin
            ReadRawNextCAtom(Src,SrcPos,AtomStart);
            if not AtomIsChar(')') then
              RaiseExpectedButAtomFound(')');
          end;
        end else begin
          // constant
        end;
      end else begin
        RaiseExpectedButAtomFound('constant');
      end;
    until false;
  end;

var
  StartPos: LongInt;
begin
  Result:=true;
  CreateChildNode(ccnDirective);
  if ParseDirectives then begin
    // read directive
    ReadRawNextAtom;
    if AtomIs('include') then begin
      CurNode.SubDesc:=ccnsDirectiveInclude;
      ReadRawNextAtom;
      if AtomIsChar('<') then begin
        // #include <filename>  // search independent of source position
        StartPos:=SrcPos;
        repeat
          ReadRawNextAtom;
          if AtomStart>SrcLen then begin
            MoveCursorToPos(StartPos);
            RaiseExpectedButAtomFound('>');
          end;
        until AtomIsChar('>');
      end else if AtomIsStringConstant then begin
        // #include "filename"  // search dependent on source position
      end else begin
        RaiseExpectedButAtomFound('< or "');
      end;
    end else if AtomIs('define') then begin
      // #define FMAC(a,b) a here, then b
      // #define NONFMAC some text here
      CurNode.SubDesc:=ccnsDirectiveDefine;
      ReadRawNextAtom;
      if not AtomIsIdentifier then
        RaiseExpectedButAtomFound('identifier');
      // if a ( follows immediately (without spaces) then it is a macro function
      if (SrcPos<=SrcLen) and (Src[SrcPos]='(') then begin
        AtomStart:=SrcPos;
        SrcPos:=AtomStart+1;
        ReadTilBracketClose(true);
      end;
    end else if AtomIs('undef') then begin
      CurNode.SubDesc:=ccnsDirectiveUndef;
      ReadRawNextAtom;
      if not AtomIsIdentifier then
        RaiseExpectedButAtomFound('identifier');
    end else if AtomIs('if') then begin
      {$IFDEF VerboseCDirectives}
      DebugLn(['TCCodeParserTool.DirectiveToken ',GetIndentStr(IfLevel*2),GetAtom]);
      {$ENDIF}
      CurNode.SubDesc:=ccnsDirectiveIf;
      IncIfLevel(AtomStart);
      ReadExpression;
    end else if AtomIs('ifdef') then begin
      {$IFDEF VerboseCDirectives}
      DebugLn(['TCCodeParserTool.DirectiveToken ',GetIndentStr(IfLevel*2),GetAtom]);
      {$ENDIF}
      CurNode.SubDesc:=ccnsDirectiveIfDef;
      IncIfLevel(AtomStart);
      ReadRawNextAtom;
      if not AtomIsIdentifier then
        RaiseExpectedButAtomFound('identifier');
    end else if AtomIs('ifndef') then begin
      {$IFDEF VerboseCDirectives}
      DebugLn(['TCCodeParserTool.DirectiveToken ',GetIndentStr(IfLevel*2),GetAtom]);
      {$ENDIF}
      CurNode.SubDesc:=ccnsDirectiveIfNDef;
      IncIfLevel(AtomStart);
      ReadRawNextAtom;
      if not AtomIsIdentifier then
        RaiseExpectedButAtomFound('identifier');
    end else if AtomIs('elif') then begin
      {$IFDEF VerboseCDirectives}
      DebugLn(['TCCodeParserTool.DirectiveToken ',GetIndentStr(IfLevel*2-2),GetAtom]);
      {$ENDIF}
      CurNode.SubDesc:=ccnsDirectiveElIf;
      if IfLevel=0 then
        RaiseException('elif without if');
      ReadExpression;
    end else if AtomIs('else') then begin
      {$IFDEF VerboseCDirectives}
      DebugLn(['TCCodeParserTool.DirectiveToken ',GetIndentStr(IfLevel*2-2),GetAtom]);
      {$ENDIF}
      CurNode.SubDesc:=ccnsDirectiveElse;
      if IfLevel=0 then
        RaiseException('else without if');
    end else if AtomIs('endif') then begin
      CurNode.SubDesc:=ccnsDirectiveEndIf;
      if IfLevel=0 then
        RaiseException('endif without if');
      dec(IfLevel);
      {$IFDEF VerboseCDirectives}
      DebugLn(['TCCodeParserTool.DirectiveToken ',GetIndentStr(IfLevel*2),GetAtom]);
      {$ENDIF}
    end else if AtomIs('line') then begin
      CurNode.SubDesc:=ccnsDirectiveLine;
    end else if AtomIs('error') then begin
      CurNode.SubDesc:=ccnsDirectiveError;
    end else if AtomIs('pragma') then begin
      CurNode.SubDesc:=ccnsDirectivePragma;
    end else begin
      RaiseExpectedButAtomFound('directive')
    end;
  end;
  // read til end of line
  ReadTilCLineEnd(Src,SrcPos);
  AtomStart:=SrcPos;
  //DebugLn(['TCCodeParserTool.DirectiveToken ',copy(Src,CurNode.StartPos,AtomStart-CurNode.Startpos)]);
  EndChildNode;
end;

function TCCodeParserTool.EnumToken: boolean;
begin
  Result:=true;
  ReadEnum;
  // read semicolon
  ReadNextAtom;
  if not AtomIsChar(';') then
    RaiseExpectedButAtomFound(';');
end;

function TCCodeParserTool.ExternToken: boolean;
begin
  Result:=true;
  CreateChildNode(ccnExtern);
  ReadNextAtom;
  if not AtomIsStringConstant then
    RaiseExpectedButAtomFound('string constant');
  ReadNextAtom;
  if not AtomIsChar('{') then
    RaiseExpectedButAtomFound('{');
end;

function TCCodeParserTool.CurlyBracketCloseToken: boolean;
// examples:
//  end of 'extern "C" {'
begin
  Result:=true;
  if CurNode.Desc=ccnExtern then
    EndChildNode
  else
    RaiseException('} without {');
end;

procedure TCCodeParserTool.ReadEnum;
(* For example:
  enum {
    TEST_ENUM1 = 1, /* Enum starts at 1 */
    TEST_ENUM2,
    TEST_ENUM3
  };
  enum e1{dark, light};

*)
begin
  CreateChildNode(ccnEnumBlock);
  ReadNextAtom;
  // read optional name
  if AtomIsIdentifier then begin
    CreateChildNode(ccnName);
    EndChildNode;
    ReadNextAtom;
  end;
  if not AtomIsChar('{') then
    RaiseExpectedButAtomFound('{');
  // read enums. Examples
  // name,
  // name = constant,
  ReadNextAtom;
  repeat
    if AtomIsIdentifier then begin
      // read enum
      CreateChildNode(ccnEnumID);
      CurNode.EndPos:=SrcPos;
      ReadNextAtom;
      if AtomIsChar('=') then begin
        // read value
        ReadNextAtom;
        ReadConstant;
        CurNode.EndPos:=SrcPos;
        ReadNextAtom;
      end;
      EndChildNode;
    end;
    if AtomIsChar(',') then begin
      // next enum
      ReadNextAtom;
      if not AtomIsIdentifier then
        RaiseExpectedButAtomFound('identifier');
    end else if AtomIsChar('}') then begin
      break;
    end else
      RaiseExpectedButAtomFound('}');
  until false;
  EndChildNode;
end;

procedure TCCodeParserTool.ReadStruct;
(*  Examples:

  As typedef:
    typedef struct {
      uint8_t b[6]; // implicit type
    } __attribute__((packed)) bdaddr_t;
    
    typedef struct _sdp_list sdp_list_t;

  As implicit typedef:
    struct hidp_connadd_req {
      int ctrl_sock;
    }
    struct SwsContext; // no content

  As variable:
    struct hidp_conninfo *ci;
*)
//
//  As typecast in macros:
//    struct __attribute__((packed)) {
//            typeof(*(ptr)) __v;
//    } *__p = (void *) (ptr);
//
begin
  CreateChildNode(ccnStruct);
  
  ReadNextAtom;
  if CurNode.Parent.Desc<>ccnTypedef then begin
    // read variable name
    if not AtomIsIdentifier then
      RaiseExpectedButAtomFound('identifier');
    CreateChildNode(ccnName);
    EndChildNode;
    ReadNextAtom;
  end;
  
  // read front attributes
  if AtomIs('__attribute__') then begin
    Read__attribute__;
    ReadNextAtom;
  end;
  if AtomIsChar('{') then begin
    // read block {}
    repeat
      ReadNextAtom;
      // read variables
      if AtomIsIdentifier then begin
        ReadVariable(false);
        ReadNextAtom;
        if AtomIsChar('}') then
          break
        else if AtomIsChar(';') then begin
          // next identifier
        end else
          RaiseExpectedButAtomFound('}');
      end else if AtomIsChar('}') then
        break
      else
        RaiseExpectedButAtomFound('identifier');
    until false;
    // read after attributes
    ReadNextAtom;
    if AtomIs('__attribute__') then begin
      Read__attribute__;
    end else begin
      UndoReadNextAtom;
    end;
  end else if AtomIsIdentifier then begin
    // using another struct
    CreateChildNode(ccnStructAlias);
    EndChildNode;
  end else if AtomIsChar(';') then begin
    // struct without content
  end else
    RaiseExpectedButAtomFound('{');

  // close node
  EndChildNode;
end;

procedure TCCodeParserTool.ReadUnion;
(*  Example
  union {
          uint16_t  uuid16;
          uint32_t  uuid32;
          uint128_t uuid128;
  } value;

*)
begin
  CreateChildNode(ccnUnion);

  ReadNextAtom;

  if AtomIsChar('{') then begin
    // read block {}
    repeat
      ReadNextAtom;
      // read variables
      if AtomIsIdentifier then begin
        ReadVariable(false);
        ReadNextAtom;
        if AtomIsChar('}') then
          break
        else if AtomIsChar(';') then begin
          // next identifier
        end else
          RaiseExpectedButAtomFound('}');
      end else if AtomIsChar('}') then
        break
      else
        RaiseExpectedButAtomFound('identifier');
    until false;
  end else if AtomIsIdentifier then begin
    // using another union
  end else
    RaiseExpectedButAtomFound('{');

  // close node
  EndChildNode;
end;

function TCCodeParserTool.TypedefToken: boolean;
{ examples:
   typedef type name;
}
begin
  Result:=true;
  CreateChildNode(ccnTypedef);
  // read type
  ReadNextAtom;
  if AtomIs('typedef') then
    RaiseExpectedButAtomFound('declaration')
  else if AtomIs('struct') then begin
    ReadStruct;
    ReadNextAtom;
    if not AtomIsIdentifier then
      RaiseExpectedButAtomFound('identifier');
    CreateChildNode(ccnName);
    EndChildNode;
  end else if AtomIs('enum') then begin
    ReadEnum;
    ReadNextAtom;
    if not AtomIsIdentifier then
      RaiseExpectedButAtomFound('identifier');
    CreateChildNode(ccnName);
    EndChildNode;
  end else if SrcPos>SrcLen then
    RaiseException('missing declaration')
  else
    ReadVariable(false);
  // read semicolon
  ReadNextAtom;
  if not AtomIsChar(';') then
    RaiseExpectedButAtomFound(';');
  EndChildNode;
end;

function TCCodeParserTool.StructToken: boolean;
begin
  Result:=true;
  ReadStruct;
end;

procedure TCCodeParserTool.InitKeyWordList;
begin
  if FDefaultTokenList=nil then begin
    FDefaultTokenList:=TKeyWordFunctionList.Create;
    with FDefaultTokenList do begin
      Add('#',{$ifdef FPC}@{$endif}DirectiveToken);
      Add('extern',{$ifdef FPC}@{$endif}ExternToken);
      Add('}',{$ifdef FPC}@{$endif}CurlyBracketCloseToken);
      Add('enum',{$ifdef FPC}@{$endif}EnumToken);
      Add('typedef',{$ifdef FPC}@{$endif}TypedefToken);
      Add('struct',{$ifdef FPC}@{$endif}StructToken);
      DefaultKeyWordFunction:={$ifdef FPC}@{$endif}OtherToken;
    end;
  end;
end;

procedure TCCodeParserTool.InitParser;
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
  CreateChildNode(ccnRoot);
  IfLevel:=0;
end;

procedure TCCodeParserTool.CreateChildNode(Desc: TCCodeNodeDesc);
var
  NewNode: TCodeTreeNode;
begin
  NewNode:=NodeMemManager.NewNode;
  Tree.AddNodeAsLastChild(CurNode,NewNode);
  NewNode.Desc:=Desc;
  CurNode:=NewNode;
  CurNode.StartPos:=AtomStart;
  {$IFDEF VerboseCCodeParser}
  DebugLn([GetIndentStr(CurNode.GetLevel*2),'TCCodeParserTool.CreateChildNode ',CCNodeDescAsString(Desc)]);
  {$ENDIF}
end;

procedure TCCodeParserTool.EndChildNode;
begin
  {$IFDEF VerboseCCodeParser}
  DebugLn([GetIndentStr(CurNode.GetLevel*2),'TCCodeParserTool.EndChildNode ',CCNodeDescAsString(CurNode.Desc)]);
  {$ENDIF}
  if CurNode.EndPos<=0 then
    CurNode.EndPos:=SrcPos;
  CurNode:=CurNode.Parent;
end;

procedure TCCodeParserTool.CloseNodes;
var
  Node: TCodeTreeNode;
begin
  Node:=CurNode;
  while Node<>nil do begin
    Node.EndPos:=AtomStart;
    Node:=Node.Parent;
  end;
end;

procedure TCCodeParserTool.ReadConstant;
// ends on last atom of constant
begin
  if AtomIsChar(',') or AtomIsChar(';') or AtomIsChar(')') then
    RaiseExpectedButAtomFound('identifier');
  CreateChildNode(ccnConstant);
  repeat
    if AtomIsChar('(') or AtomIsChar('[') then
      ReadTilBracketClose(true);
    CurNode.EndPos:=SrcPos;
    ReadNextAtom;
    if AtomIsChar(',') or AtomIsChar(';')
    or AtomIsChar(')') or AtomIsChar(']') or AtomIsChar('}')
    then
      break;
  until false;
  UndoReadNextAtom;
  EndChildNode;
end;

procedure TCCodeParserTool.Read__attribute__;
begin
  ReadNextAtom;
  if not AtomIsChar('(') then
    RaiseExpectedButAtomFound('(');
  ReadTilBracketClose(true);
end;

procedure TCCodeParserTool.IncIfLevel(StartPos: integer);
begin
  inc(IfLevel);
  if FIfStackCapacity<IfLevel then begin
    FIfStackCapacity:=5+FIfStackCapacity*2;
    ReAllocMem(FIfStack,FIfStackCapacity*SizeOf(TCCodeParserIfStackItem));
  end;
  FIfStack[IfLevel-1].StartPos:=StartPos;
end;

procedure TCCodeParserTool.ReadVariable(AsParameter: boolean);
(* Read  type name [specifiers]

  if AsParameter=true then name can be omitted.

  Examples:

  int i
  uint8_t b[6]
  uint8_t lap[MAX_IAC_LAP][3];
  int y = 7;

  static inline int bacmp(const bdaddr_t *ba1, const bdaddr_t *ba2)
  {
        return memcmp(ba1, ba2, sizeof(bdaddr_t));
  }
  bdaddr_t *strtoba(const char *str);

  void av_log(void*, int level, const char *fmt, ...) __attribute__ ((__format__ (__printf__, 3, 4)));

*)
{
  const char* (*item_name)(void* ctx);
  int (*fp)(char*); // pointer to function taking a char* argument; returns an int
  int * f(char*); // function taking a char* argument; returns a pointer to int

  complex operator+(complex, complex);
}
var
  IsFunction: Boolean;
  NeedEnd: Boolean;
  LastIsName: Boolean;
  MainNode: TCodeTreeNode;
begin
  {$IFDEF VerboseCCodeParser}
  DebugLn(['TCCodeParserTool.ReadVariable START ',GetAtom]);
  {$ENDIF}
  if AsParameter then begin
    CreateChildNode(ccnFuncParameter);
    if AtomIs('...') then begin
      EndChildNode;
      exit;
    end;
  end else
    CreateChildNode(ccnVariable);
  MainNode:=CurNode;
  IsFunction:=false;
  if AtomIs('const') then ReadNextAtom;
  if AtomIs('struct') then begin
    // for example: struct structname varname
    ReadNextAtom;
  end else if AtomIs('union') then begin
    ReadUnion;
  end else if IsCCodeFunctionModifier.DoItCaseSensitive(Src,AtomStart,SrcPos-AtomStart)
  then begin
    // read function modifiers
    while IsCCodeFunctionModifier.DoItCaseSensitive(Src,AtomStart,SrcPos-AtomStart)
    do begin
      if AsParameter then
        RaiseException('function modifier not allowed in parameter');
      IsFunction:=true;
      MainNode.Desc:=ccnFunction;
      ReadNextAtom;
      if not AtomIsIdentifier then
        RaiseExpectedButAtomFound('identifier');
    end;
  end;
  if AtomIs('const') then ReadNextAtom;

  // prefixes: signed, unsigned
  // prefixes and/or names long, short

  // int, short int, short signed int
  // char, signed char, unsigned char
  // singed short, unsigned short, short
  // long, long long, signed long, signed long long, unsigned long, unsigned long long
  LastIsName:=false;
  repeat
    if AtomIs('signed') or AtomIs('unsigned') then begin
      LastIsName:=false;
      ReadNextAtom;
    end else if AtomIs('short') or AtomIs('long') then begin
      LastIsName:=true;
      ReadNextAtom;
    end else
      break;
  until false;
  if LastIsName then
    UndoReadNextAtom;

  // read name
  ReadNextAtom;
  while AtomIsChar('*') or AtomIs('const') do begin
    // pointer or const
    ReadNextAtom;
  end;
  if AtomIs('operator') then begin
    if AsParameter then
      RaiseException('operator not allowed as parameter');
    IsFunction:=true;
    MainNode.Desc:=ccnFunction;
    // read operator
    ReadNextAtom;
    if not IsCCodeCustomOperator.DoItCaseSensitive(Src,AtomStart,SrcPos-AtomStart)
    then
      RaiseExpectedButAtomFound('operator');
    CreateChildNode(ccnName);
    CurNode.StartPos:=AtomStart;
    CurNode.EndPos:=SrcPos;
  end else if AtomIsChar('(') then begin
    IsFunction:=true;
    MainNode.Desc:=ccnFunction;
    // example: int (*fp)(char*);
    //   pointer to function taking a char* argument; returns an int
    ReadNextAtom;
    while AtomIsChar('*') or AtomIs('const') do begin
      // pointer or const
      ReadNextAtom;
    end;
    {$IFDEF VerboseCCodeParser}
    DebugLn(['TCCodeParserTool.ReadVariable name=',GetAtom]);
    {$ENDIF}
    if AtomIsIdentifier then begin
      CreateChildNode(ccnName);
      CurNode.StartPos:=AtomStart;
      CurNode.EndPos:=SrcPos;
      ReadNextAtom;
    end else if not AsParameter then
      RaiseExpectedButAtomFound('identifier');
    if not AtomIsChar(')') then
      RaiseExpectedButAtomFound(')');
  end else begin
    {$IFDEF VerboseCCodeParser}
    DebugLn(['TCCodeParserTool.ReadVariable name=',GetAtom]);
    {$ENDIF}
    if AtomIsIdentifier then begin
      CreateChildNode(ccnName);
      CurNode.StartPos:=AtomStart;
      CurNode.EndPos:=SrcPos;
    end else if not AsParameter then begin
      RaiseExpectedButAtomFound('identifier');
    end else begin
      UndoReadNextAtom;
    end;
  end;
  // end of name
  if CurNode.Desc=ccnName then
    EndChildNode;

  ReadNextAtom;
  if IsFunction and (not AtomIsChar('(')) then
    RaiseExpectedButAtomFound('(');
  NeedEnd:=true;
  if AtomIsChar('(') then begin
    // this is a function => read parameter list
    IsFunction:=true;
    MainNode.Desc:=ccnFunction;
    ReadParameterList;
    ReadNextAtom;
    if AtomIs('__attribute__') then begin
      Read__attribute__;
      ReadNextAtom;
    end;
    if (CurNode.Parent.Desc=ccnTypedef) then begin
      if AtomIsChar('{') then
        RaiseException('a typedef can not have a statement block');
    end else if AsParameter then begin
      if AtomIsChar('{') then
        RaiseException('a parameter can not have a statement block');
    end else begin
      if AtomIsChar('{') then begin
        // read statements {}
        CreateChildNode(ccnStatementBlock);
        ReadTilBracketClose(true);
        CurNode.EndPos:=SrcPos;
        EndChildNode;
        ReadNextAtom;
      end else if not AtomIsChar(';') then begin
        // functions without statements are external and must end with a semicolon
        RaiseExpectedButAtomFound(';');
      end;
      NeedEnd:=false;
    end;
  end else if AtomIsChar('[') then begin
    // read array brackets
    while AtomIsChar('[') do begin
      ReadTilBracketClose(true);
      ReadNextAtom;
    end;
  end;
  
  // read initial constant
  if AtomIsChar('=') then begin
    if CurNode.HasParentOfType(ccnTypedef) then
      RaiseException('typedef can not have an initial value');
    ReadNextAtom;
    ReadConstant;
    ReadNextAtom;
    NeedEnd:=true;
  end;
  
  // sanity check
  if (SrcPos<=SrcLen) and NeedEnd
  and not (AtomIsChar(';') or AtomIsChar(',') or AtomIsChar(')')) then
    RaiseExpectedButAtomFound('"end of variable"');
    
  UndoReadNextAtom;

  EndChildNode;
end;

procedure TCCodeParserTool.ReadParameterList;
// start on (, end on )
var
  StartPos: LongInt;
begin
  CreateChildNode(ccnFuncParamList);
  StartPos:=AtomStart;
  repeat
    ReadNextAtom;
    if AtomStart>SrcLen then begin
      // missing closing bracket
      AtomStart:=StartPos;
      SrcPos:=AtomStart+1;
      RaiseException('closing bracket not found');
    end;
    if AtomIsChar(')') then break;
    if AtomIsChar(',') then
      RaiseExpectedButAtomFound('parameter type');
    // read parameter
    ReadVariable(true);
    // read next
    ReadNextAtom;
    if AtomIsChar(')') then break;
    if not AtomIsChar(',') then
      RaiseExpectedButAtomFound(',');
  until false;
  CurNode.EndPos:=SrcPos;
  EndChildNode;
end;

procedure TCCodeParserTool.RaiseException(const AMessage: string; ReportPos: integer);
begin
  LastErrorMsg:=AMessage;
  LastErrorPos:=AtomStart;
  LastErrorReportPos:=LastErrorPos;
  if ReportPos>0 then
    LastErrorReportPos:=ReportPos;
  CloseNodes;
  raise ECCodeParserException.Create(Self,AMessage);
end;

procedure TCCodeParserTool.RaiseExpectedButAtomFound(const AToken: string;
  ReportPos: integer);
begin
  RaiseException(AToken+' expected, but '+GetAtom+' found',ReportPos);
end;

constructor TCCodeParserTool.Create;
begin
  Tree:=TCodeTree.Create;
  InitCCOdeKeyWordLists;
  VisibleEditorLines:=25;
  JumpCentered:=true;
  CursorBeyondEOL:=true;
  ParseDirectives:=true;
end;

destructor TCCodeParserTool.Destroy;
begin
  Clear;
  FreeAndNil(Tree);
  ReAllocMem(FIfStack,0);
  FIfStackCapacity:=0;
  FreeAndNil(FDefaultTokenList);
  inherited Destroy;
end;

procedure TCCodeParserTool.Clear;
begin
  IfLevel:=0;
  if FIfStackCapacity>10 then begin
    ReAllocMem(FIfStack,0);
    FIfStackCapacity:=0;
  end;
  Tree.Clear;
end;

procedure TCCodeParserTool.Parse;
begin
  Parse(Code);
end;

procedure TCCodeParserTool.Parse(aCode: TCodeBuffer);
begin
  if (Code=aCode) and (not UpdateNeeded) then
    exit;
  Code:=aCode;
  InitParser;
  repeat
    ReadNextAtom;
    if SrcPos<=SrcLen then begin
      FDefaultTokenList.DoItCaseSensitive(Src,AtomStart,SrcPos-AtomStart);
    end else begin
      break;
    end;
  until false;
  if (CurNode=nil) or (CurNode.Desc<>ccnRoot) then
    RaiseException('TCCodeParserTool.Parse: internal parser error');
  EndChildNode;
end;

function TCCodeParserTool.UpdateNeeded: boolean;
begin
  Result:=true;
  if (Code=nil) or (Tree=nil) or (Tree.Root=nil) then exit;
  if Code.ChangeStep<>ParseChangeStep then exit;
  Result:=false;
end;

function TCCodeParserTool.FindDeepestNodeAtPos(P: integer;
  ExceptionOnNotFound: boolean): TCodeTreeNode; inline;
begin
  Result:=FindDeepestNodeAtPos(Tree.Root,P,ExceptionOnNotFound);
end;

function TCCodeParserTool.FindDeepestNodeAtPos(StartNode: TCodeTreeNode;
  P: integer; ExceptionOnNotFound: boolean): TCodeTreeNode;

  procedure RaiseNoNodeFoundAtCursor;
  begin
    //DebugLn('RaiseNoNodeFoundAtCursor ',MainFilename);
    RaiseException(ctsNoNodeFoundAtCursor);
  end;

var
  ChildNode: TCodeTreeNode;
  Brother: TCodeTreeNode;
begin
  {$IFDEF CheckNodeTool}CheckNodeTool(StartNode);{$ENDIF}
  Result:=nil;
  while StartNode<>nil do begin
    //DebugLn('SearchInNode ',NodeDescriptionAsString(ANode.Desc),
    //',',ANode.StartPos,',',ANode.EndPos,', p=',p,
    //' "',copy(Src,ANode.StartPos,4),'" - "',copy(Src,ANode.EndPos-5,4),'"');
    if (StartNode.StartPos<=P)
    and ((StartNode.EndPos>P) or (StartNode.EndPos<1)) then begin
      // StartNode contains P
      Result:=StartNode;
      // -> search for a child that contains P
      Brother:=StartNode;
      while (Brother<>nil)
      and (Brother.StartPos<=P) do begin
        // brother also contains P
        if Brother.FirstChild<>nil then begin
          ChildNode:=FindDeepestNodeAtPos(Brother.FirstChild,P,false);
          if ChildNode<>nil then begin
            Result:=ChildNode;
            exit;
          end;
        end;
        Brother:=Brother.NextBrother;
      end;
      break;
    end else begin
      // search in next node
      StartNode:=StartNode.NextBrother;
    end;
  end;
  if (Result=nil) and ExceptionOnNotFound then begin
    MoveCursorToPos(P);
    RaiseNoNodeFoundAtCursor;
  end;
end;

function TCCodeParserTool.CaretToCleanPos(Caret: TCodeXYPosition; out
  CleanPos: integer): integer;
begin
  CleanPos:=0;
  if Caret.Code<>Code then
    exit(-1);
  Code.LineColToPosition(Caret.Y,Caret.X,CleanPos);
  if (CleanPos>=1) then
    Result:=0
  else
    Result:=-2; // x,y beyond source
end;

function TCCodeParserTool.CleanPosToCodePos(CleanPos: integer; out
  CodePos: TCodePosition): boolean;
begin
  CodePos.Code:=Code;
  CodePos.P:=CleanPos;
  Result:=(Code<>nil) and (CleanPos>0) and (CleanPos<Code.SourceLength);
end;

function TCCodeParserTool.CleanPosToCaret(CleanPos: integer; out
  Caret: TCodeXYPosition): boolean;
begin
  Caret.Code:=Code;
  Code.AbsoluteToLineCol(CleanPos,Caret.Y,Caret.X);
  Result:=CleanPos>0;
end;

function TCCodeParserTool.CleanPosToCaretAndTopLine(CleanPos: integer; out
  Caret: TCodeXYPosition; out NewTopLine: integer): boolean;
begin
  Caret:=CleanCodeXYPosition;
  NewTopLine:=0;
  Result:=CleanPosToCaret(CleanPos,Caret);
  if Result then begin
    if JumpCentered then begin
      NewTopLine:=Caret.Y-(VisibleEditorLines shr 1);
      if NewTopLine<1 then NewTopLine:=1;
    end else
      NewTopLine:=Caret.Y;
  end;
end;

function TCCodeParserTool.CleanPosToStr(CleanPos: integer): string;
var
  CodePos: TCodeXYPosition;
begin
  if CleanPosToCaret(CleanPos,CodePos) then
    Result:='y='+IntToStr(CodePos.Y)+',x='+IntToStr(CodePos.X)
  else
    Result:='y=?,x=?';
end;

function TCCodeParserTool.MainFilename: string;
begin
  Result:=Code.Filename;
end;

procedure TCCodeParserTool.MoveCursorToPos(p: integer);
begin
  SrcPos:=p;
  AtomStart:=p;
  LastAtomStart:=0;
  LastSrcPos:=0;
end;

procedure TCCodeParserTool.MoveCursorToNode(Node: TCodeTreeNode);
begin
  MoveCursorToPos(Node.StartPos);
end;

procedure TCCodeParserTool.ReadNextAtom;
begin
  //DebugLn(['TCCodeParserTool.ReadNextAtom START ',AtomStart,'-',SrcPos,' ',Src[SrcPos]]);
  LastSrcPos:=SrcPos;
  LastAtomStart:=AtomStart;
  repeat
    ReadRawNextCAtom(Src,SrcPos,AtomStart);
  until (SrcPos>SrcLen) or (not (Src[AtomStart] in [#10,#13]));
  {$IFDEF VerboseCCodeParser}
  DebugLn(['TCCodeParserTool.ReadNextAtom END ',AtomStart,'-',SrcPos,' "',copy(Src,AtomStart,SrcPos-AtomStart),'"']);
  {$ENDIF}
end;

procedure TCCodeParserTool.ReadNextAtomSkipDirectives;
begin
  //DebugLn(['TCCodeParserTool.ReadNextAtom START ',AtomStart,'-',SrcPos,' ',Src[SrcPos]]);
  LastSrcPos:=SrcPos;
  LastAtomStart:=AtomStart;
  repeat
    ReadRawNextCAtom(Src,SrcPos,AtomStart);
    if (SrcPos>SrcLen) then break;
    if Src[AtomStart]='#' then begin
      ReadTilCLineEnd(Src,SrcPos);
      if (SrcPos>SrcLen) then break;
    end;
  until (not (Src[AtomStart] in [#10,#13]));
  {$IFDEF VerboseCCodeParser}
  DebugLn(['TCCodeParserTool.ReadNextAtom END ',AtomStart,'-',SrcPos,' "',copy(Src,AtomStart,SrcPos-AtomStart),'"']);
  {$ENDIF}
end;

procedure TCCodeParserTool.ReadRawNextAtom;
begin
  LastSrcPos:=SrcPos;
  LastAtomStart:=AtomStart;
  ReadRawNextCAtom(Src,SrcPos,AtomStart);
end;

procedure TCCodeParserTool.UndoReadNextAtom;
begin
  if LastSrcPos>0 then begin
    SrcPos:=LastSrcPos;
    AtomStart:=LastAtomStart;
    LastSrcPos:=0;
    LastAtomStart:=0;
  end else begin
    SrcPos:=AtomStart;
  end;
end;

function TCCodeParserTool.ReadTilBracketClose(
  ExceptionOnNotFound: boolean): boolean;
// AtomStart must be on bracket open
// after reading AtomStart is on closing bracket
var
  CloseBracket: Char;
  StartPos: LongInt;
begin
  case Src[AtomStart] of
  '{': CloseBracket:='}';
  '[': CloseBracket:=']';
  '(': CloseBracket:=')';
  '<': CloseBracket:='>';
  else
    if ExceptionOnNotFound then
      RaiseExpectedButAtomFound('(');
    exit(false);
  end;
  StartPos:=AtomStart;
  {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
  {$R-}
  repeat
    ReadRawNextCAtom(Src,SrcPos,AtomStart);
    if AtomStart>SrcLen then begin
      AtomStart:=StartPos;
      SrcPos:=AtomStart+1;
      if ExceptionOnNotFound then
        RaiseException('closing bracket not found');
      exit;
    end;
    case Src[AtomStart] of
    '{','(','[':
      // skip nested bracketss
      begin
        if not ReadTilBracketClose(ExceptionOnNotFound) then
          exit;
      end;
    else
      if Src[AtomStart]=CloseBracket then exit(true);
    end;
  until false;
  {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}
end;

function TCCodeParserTool.AtomIs(const s: shortstring): boolean;
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

function TCCodeParserTool.AtomIsChar(const c: char): boolean;
begin
  if SrcPos-AtomStart<>1 then exit(false);
  if SrcPos>SrcLen then exit(false);
  if Src[AtomStart]<>c then exit(false);
  Result:=true;
end;

function TCCodeParserTool.AtomIsCharOfSet(const s: shortstring): boolean;
var
  i: Integer;
  c: Char;
begin
  if SrcPos-AtomStart<>1 then exit(false);
  if SrcPos>SrcLen then exit(false);
  c:=Src[AtomStart];
  for i:=1 to length(s) do
    if s[i]=c then exit(true);
  Result:=false;
end;

function TCCodeParserTool.UpAtomIs(const s: shortstring): boolean;
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

function TCCodeParserTool.AtomIsIdentifier: boolean;
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

function TCCodeParserTool.AtomIsStringConstant: boolean;
begin
  Result:=(AtomStart<SrcLen) and (Src[AtomStart]='"');
end;

function TCCodeParserTool.LastAtomIs(const s: shortstring): boolean;
var
  len: Integer;
  i: Integer;
begin
  if LastAtomStart<=LastSrcPos then exit(false);
  len:=length(s);
  if (len<>LastSrcPos-LastAtomStart) then exit(false);
  if LastSrcPos>SrcLen then exit(false);
  for i:=1 to len do
    if Src[LastAtomStart+i-1]<>s[i] then exit(false);
  Result:=true;
end;

function TCCodeParserTool.GetLastAtom: string;
begin
  Result:=copy(Src,LastAtomStart,LastSrcPos-LastAtomStart);
end;

function TCCodeParserTool.ExtractCode(StartPos, EndPos: integer;
  WithDirectives: boolean): string;
var
  s: string;
  p: integer;

  procedure ReadIt;
  var
    l: Integer;
    NextChar: Char;
    LastChar: Char;
  begin
    MoveCursorToPos(StartPos);
    p:=1;
    LastChar:=' ';
    repeat
      // read next token
      if WithDirectives then
        ReadNextAtom
      else
        ReadNextAtomSkipDirectives;
      if (AtomStart>=EndPos) then break;
      
      NextChar:=Src[AtomStart];
      if IsIdentChar[LastChar] and IsIdentStartChar[NextChar] then begin
        // add space
        if s<>'' then
          s[p]:=' ';
        inc(p);
      end;
      // add token
      l:=SrcPos-AtomStart;
      if s<>'' then begin
        // copy token
        System.Move(Src[AtomStart],s[p],SrcPos-AtomStart);
      end;
      inc(p,l);
      // remember last char
      LastChar:=Src[SrcPos-1];
    until false;
  end;

begin
  if EndPos>SrcLen then EndPos:=SrcLen+1;
  // first read and compute needed length
  ReadIt;
  // allocate space and copy tokens
  SetLength(s,p-1);
  ReadIt;
  Result:=s;
end;

function TCCodeParserTool.GetFirstNameNode(Node: TCodeTreeNode): TCodeTreeNode;
begin
  Result:=Node.FirstChild;
  while (Result<>nil) and (Result.Desc<>ccnName) do Result:=Result.NextBrother;
end;

function TCCodeParserTool.ExtractVariableName(VarNode: TCodeTreeNode): string;
var
  NameNode: TCodeTreeNode;
begin
  NameNode:=GetFirstNameNode(VarNode);
  if (NameNode=nil) then
    Result:=''
  else
    Result:=copy(Src,NameNode.StartPos,NameNode.EndPos-NameNode.StartPos);
end;

function TCCodeParserTool.ExtractVariableType(VarNode: TCodeTreeNode;
  WithDirectives: boolean): string;
var
  NameNode: TCodeTreeNode;
  s: String;
begin
  NameNode:=GetFirstNameNode(VarNode);
  if (NameNode=nil) then
    Result:=''
  else begin
    Result:=ExtractCode(VarNode.StartPos,NameNode.StartPos,WithDirectives);
    if (NameNode.NextBrother<>nil)
    and (NameNode.NextBrother.Desc=ccnConstant) then begin
      // a variable with an initial value
      // omit the constant
      s:=ExtractCode(NameNode.EndPos,NameNode.NextBrother.StartPos,
                     WithDirectives);
      s:=copy(s,1,length(s)-1);
      Result:=Result+s;
    end else begin
      Result:=Result+ExtractCode(NameNode.EndPos,VarNode.EndPos,
                                 WithDirectives);
    end;
  end;
end;

function TCCodeParserTool.ExtractFunctionName(FuncNode: TCodeTreeNode): string;
var
  NameNode: TCodeTreeNode;
begin
  NameNode:=GetFirstNameNode(FuncNode);
  if (NameNode=nil) then
    Result:=''
  else
    Result:=copy(Src,NameNode.StartPos,NameNode.EndPos-NameNode.StartPos);
end;

function TCCodeParserTool.GetFunctionParamListNode(Node: TCodeTreeNode
  ): TCodeTreeNode;
begin
  Result:=Node.FirstChild;
  while (Result<>nil) and (Result.Desc<>ccnFuncParamList) do
    Result:=Result.NextBrother;
end;

function TCCodeParserTool.ExtractFunctionParamList(FuncNode: TCodeTreeNode
  ): string;
var
  ParamsNode: TCodeTreeNode;
begin
  ParamsNode:=GetFunctionParamListNode(FuncNode);
  if (ParamsNode=nil) then
    Result:=''
  else
    Result:=ExtractCode(ParamsNode.StartPos,ParamsNode.EndPos);
end;

function TCCodeParserTool.ExtractFunctionType(FuncNode: TCodeTreeNode;
  WithDirectives: boolean): string;
var
  NameNode: TCodeTreeNode;
begin
  NameNode:=GetFirstNameNode(FuncNode);
  if (NameNode=nil) then begin
    Result:='';
    exit;
  end;
  Result:=ExtractCode(FuncNode.StartPos,NameNode.StartPos,WithDirectives);
  if (NameNode.NextBrother<>nil)
  and (NameNode.NextBrother.Desc=ccnFuncParamList) then begin
    // The name is in between.
    // The type is result type + parameter list
    Result:=Result+ExtractCode(NameNode.EndPos,NameNode.NextBrother.EndPos,
                               WithDirectives);
  end else begin
    Result:=Result+ExtractCode(NameNode.EndPos,FuncNode.EndPos,
                               WithDirectives);
  end;
end;

function TCCodeParserTool.ExtractFunctionResultType(FuncNode: TCodeTreeNode;
  WithDirectives: boolean; WithBrackets: boolean): string;
var
  NameNode: TCodeTreeNode;
  p: Integer;
  CurAtomStart: integer;
begin
  NameNode:=GetFirstNameNode(FuncNode);
  if (NameNode=nil) then begin
    Result:='';
    exit;
  end;
  
  // skip function modifiers
  MoveCursorToNode(FuncNode);
  repeat
    ReadNextAtom;
    if AtomStart>=NameNode.StartPos then break;
    if not IsCCodeFunctionModifier.DoItCaseSensitive(Src,AtomStart,SrcPos-AtomStart)
    then
      break;
  until false;
  
  Result:=ExtractCode(AtomStart,NameNode.StartPos,WithDirectives);
  if (NameNode.NextBrother<>nil)
  and (NameNode.NextBrother.Desc=ccnFuncParamList) then begin
    // The name is in between.
    Result:=Result+ExtractCode(NameNode.EndPos,NameNode.NextBrother.StartPos,
                               WithDirectives);
  end else begin
    Result:=Result+ExtractCode(NameNode.EndPos,FuncNode.EndPos,
                               WithDirectives);
  end;
  
  if not WithBrackets then begin
    p:=1;
    repeat
      ReadRawNextCAtom(Result,p,CurAtomStart);
      if CurAtomStart>length(Result) then break;
      if Result[CurAtomStart]='(' then begin
        p:=CurAtomStart;
        if ReadTilCBracketClose(Result,p) then begin
          Result:=copy(Result,1,CurAtomStart-1)
                 +copy(Result,p,length(Result));
        end;
        break;
      end;
    until false;
  end;
end;

function TCCodeParserTool.IsPointerToFunction(FuncNode: TCodeTreeNode
  ): boolean;
// for example: int *(*fp)();
var
  NameNode: TCodeTreeNode;
begin
  NameNode:=FuncNode.FirstChild;
  if (NameNode=nil) or (NameNode.Desc<>ccnName) then exit(false);
  MoveCursorToNode(FuncNode);
  repeat
    ReadNextAtom;
    if AtomStart>SrcLen then exit;
    if AtomIs('(') then exit(true);
    if (IsIdentStartChar[Src[AtomStart]])
    or (AtomIs('*')) then begin
      // skip words and *
    end else begin
      break;
    end;
  until AtomStart>=NameNode.StartPos;
  Result:=false;
end;

function TCCodeParserTool.ExtractParameterName(ParamNode: TCodeTreeNode
  ): string;
var
  NameNode: TCodeTreeNode;
begin
  NameNode:=GetFirstNameNode(ParamNode);
  if (NameNode=nil) then
    Result:=''
  else
    Result:=copy(Src,NameNode.StartPos,NameNode.EndPos-NameNode.StartPos);
end;

function TCCodeParserTool.ExtractParameterType(ParamNode: TCodeTreeNode;
  WithDirectives: boolean): string;
var
  NameNode: TCodeTreeNode;
  s: String;
  ConstantNode: TCodeTreeNode;
begin
  NameNode:=GetFirstNameNode(ParamNode);
  if (ParamNode.LastChild<>nil)
  and (ParamNode.LastChild.Desc=ccnConstant) then
    ConstantNode:=ParamNode.LastChild
  else
    ConstantNode:=nil;
  if (NameNode=nil) then begin
    if ConstantNode<>nil then begin
      // a parameter with an initial value
      // omit the constant
      Result:=ExtractCode(ParamNode.StartPos,ConstantNode.StartPos,WithDirectives);
      Result:=copy(Result,1,length(Result)-1);
    end else begin
      Result:=ExtractCode(ParamNode.StartPos,ParamNode.EndPos,WithDirectives);
    end;
  end else begin
    Result:=ExtractCode(ParamNode.StartPos,NameNode.StartPos,WithDirectives);
    if (NameNode.NextBrother<>nil)
    and (NameNode.NextBrother.Desc=ccnConstant) then begin
      // a parameter with an initial value
      // omit the constant
      s:=ExtractCode(NameNode.EndPos,NameNode.NextBrother.StartPos,
                     WithDirectives);
      s:=copy(s,1,length(s)-1);
      Result:=Result+s;
    end else begin
      Result:=Result+ExtractCode(NameNode.EndPos,ParamNode.EndPos,
                                 WithDirectives);
    end;
  end;
end;

function TCCodeParserTool.ExtractEnumBlockName(EnumBlockNode: TCodeTreeNode
  ): string;
var
  NameNode: TCodeTreeNode;
begin
  if (EnumBlockNode.FirstChild<>nil)
  and (EnumBlockNode.FirstChild.Desc=ccnName) then begin
    NameNode:=EnumBlockNode.FirstChild;
    Result:=copy(Src,NameNode.StartPos,NameNode.EndPos-NameNode.StartPos);
  end else begin
    Result:='';
  end;
end;

function TCCodeParserTool.ExtractEnumIDName(EnumIDNode: TCodeTreeNode): string;
begin
  Result:=GetIdentifier(@Src[EnumIDNode.StartPos]);
end;

function TCCodeParserTool.ExtractEnumIDValue(EnumIDNode: TCodeTreeNode;
  WithDirectives: boolean): string;
var
  ConstantNode: TCodeTreeNode;
begin
  ConstantNode:=EnumIDNode.FirstChild;
  if (ConstantNode=nil)
  or (ConstantNode.Desc<>ccnConstant) then
    Result:=''
  else
    Result:=ExtractCode(ConstantNode.StartPos,ConstantNode.EndPos,
                        WithDirectives);
end;

function TCCodeParserTool.ExtractStructName(StructNode: TCodeTreeNode): string;
var
  NameNode: TCodeTreeNode;
begin
  NameNode:=StructNode.FirstChild;
  if (NameNode<>nil) and (NameNode.Desc=ccnName) then
    Result:=GetIdentifier(@Src[NameNode.StartPos])
  else
    Result:='';
end;

function TCCodeParserTool.ExtractUnionName(UnionNode: TCodeTreeNode): string;
var
  NameNode: TCodeTreeNode;
begin
  NameNode:=UnionNode.FirstChild;
  if (NameNode<>nil) and (NameNode.Desc=ccnName) then
    Result:=GetIdentifier(@Src[NameNode.StartPos])
  else
    Result:='';
end;

function TCCodeParserTool.ExtractTypedefName(TypedefNode: TCodeTreeNode
  ): string;
var
  Node: TCodeTreeNode;
begin
  Node:=TypedefNode.LastChild;
  while (Node<>nil) and (Node.Desc<>ccnName) do
    Node:=Node.PriorBrother;
  if Node=nil then begin
    if (TypedefNode.FirstChild<>nil)
    and (TypedefNode.FirstChild.Desc=ccnFunction) then
      Result:=ExtractFunctionName(TypedefNode.FirstChild)
    else
      Result:='';
  end else
    Result:=GetIdentifier(@Src[Node.StartPos]);
end;

function TCCodeParserTool.ExtractDirectiveAction(DirectiveNode: TCodeTreeNode
  ): string;
begin
  if DirectiveNode.StartPos<SrcLen then
    Result:=GetIdentifier(@Src[DirectiveNode.StartPos+1])
  else
    Result:='';
end;

function TCCodeParserTool.ExtractDirectiveFirstAtom(DirectiveNode: TCodeTreeNode
  ): string;
begin
  MoveCursorToPos(DirectiveNode.StartPos+1);
  // read action
  ReadRawNextAtom;
  // read first atom
  ReadRawNextAtom;
  Result:=GetAtom;
end;

function TCCodeParserTool.ExtractDirectiveParams(DirectiveNode: TCodeTreeNode
  ): string;
begin
  MoveCursorToPos(DirectiveNode.StartPos+1);
  // read action
  ReadRawNextAtom;
  // read first atom
  ReadRawNextAtom;
  Result:=ExtractCode(AtomStart,DirectiveNode.EndPos);
end;

function TCCodeParserTool.ExtractDefine(DefineNode: TCodeTreeNode; out
  MacroName, MacroParamList, MacroValue: string): boolean;
var
  StartPos: LongInt;
  EndPos: LongInt;
begin
  Result:=false;
  MacroName:='';
  MacroParamList:='';
  MacroValue:='';
  MoveCursorToPos(DefineNode.StartPos+1);
  // read action
  ReadRawNextAtom;
  if not AtomIs('define') then exit;
  // read first atom
  ReadRawNextAtom;
  MacroName:=GetAtom;
  StartPos:=SrcPos;
  // read param list
  if (SrcPos<=SrcLen) and (Src[SrcPos]='(') then begin
    StartPos:=SrcPos;
    AtomStart:=SrcPos;
    SrcPos:=AtomStart+1;
    if not ReadTilBracketClose(false) then exit;
    EndPos:=SrcPos;
    MacroParamList:=ExtractCode(StartPos,EndPos);
    StartPos:=EndPos;
  end;
  // read value
  while (StartPos<=SrcLen) and (IsSpaceChar[Src[StartPos]]) do
    inc(StartPos);
  EndPos:=DefineNode.EndPos;
  while (EndPos>StartPos) and (IsSpaceChar[Src[EndPos-1]]) do
    dec(EndPos);
  MacroValue:=copy(Src,StartPos,EndPos-StartPos);
  Result:=true;
end;

function TCCodeParserTool.FindDirectiveBlockEnd(InnerNode: TCodeTreeNode
  ): TCodeTreeNode;
// find the end of block that contains InnerNode
begin
  Result:=InnerNode;
  if Result=nil then exit;
  Result:=Result.Next;
  IfLevel:=0;
  while Result<>nil do begin
    if Result.Desc=ccnDirective then begin
      //DebugLn(['TCCodeParserTool.FindDirectiveBlockEnd ',NodeAsString(Result),' ',IfLevel]);
      case Result.SubDesc of
      ccnsDirectiveIf,ccnsDirectiveIfDef,ccnsDirectiveIfNDef:
        IncIfLevel(Result.StartPos);
      ccnsDirectiveElse,ccnsDirectiveElIf:
        begin
          if IfLevel=0 then
            exit;
          dec(IfLevel);
          IncIfLevel(Result.StartPos);
        end;
      ccnsDirectiveEndIf:
        begin
          if IfLevel=0 then
            exit;
          dec(IfLevel);
        end;
      end;
    end;
    Result:=Result.Next;
  end;
end;

function TCCodeParserTool.FindElseOrEndIf(IfOrElseNode: TCodeTreeNode
  ): TCodeTreeNode;
// find corresponding #EndIf or #Else or #ElIf
begin
  Result:=IfOrElseNode;
  // check if IfOrElseNode is valid
  if (Result=nil) or (Result.Desc<>ccnDirective)
  or (not (Result.SubDesc in [ccnsDirectiveIf,ccnsDirectiveIfDef,
                      ccnsDirectiveIfNDef,ccnsDirectiveElIf,ccnsDirectiveElse]))
  then
    exit(nil);
  // check if next node is the end of the block
  Result:=Result.Next;
  if Result=nil then exit;
  if (Result.Desc=ccnDirective)
  and (Result.SubDesc in [ccnsDirectiveElIf,ccnsDirectiveElse,ccnsDirectiveEndIf])
  then
    exit;
  // Result is a node in the block => search end of block
  Result:=FindDirectiveBlockEnd(Result);
end;

function TCCodeParserTool.FindEndIf(IfOrElseNode: TCodeTreeNode): TCodeTreeNode;
// find corresponding #EndIf
begin
  Result:=IfOrElseNode;
  repeat
    Result:=FindElseOrEndIf(Result);
    if (Result=nil)
    or ((Result.Desc=ccnDirective) and (Result.SubDesc=ccnsDirectiveEndIf))
    then
      exit;
  until false;
end;

function TCCodeParserTool.FindEnclosingIFNDEF: TCodeTreeNode;
{ Search for the typical enclosing IFNDEF of c header file:
   - No code in front
   - #IFNDEF NAME
   - #DEFINE NAME
   - ...
   - #ENDIF
   - No code behind
}
var
  IfNDefNode: TCodeTreeNode;
  MacroName: String;
  DefNode: TCodeTreeNode;
  DefMacroName: String;
  EndIfNode: TCodeTreeNode;
  Node: TCodeTreeNode;
begin
  Result:=nil;
  WriteDebugReport;
  IfNDefNode:=Tree.Root;
  // skip root and extern nodes
  while (IfNDefNode<>nil) do begin
    if (IfNDefNode.Desc<>ccnRoot)
    and (IfNDefNode.Desc<>ccnExtern) then
      break;
    IfNDefNode:=IfNDefNode.Next;
  end;
  if IfNDefNode=nil then exit;
  // check if IfNDefNode is #IFNDEF name
  if (IfNDefNode.Desc<>ccnDirective)
  or (IfNDefNode.SubDesc<>ccnsDirectiveIfNDef) then
    exit;
  MacroName:=ExtractDirectiveFirstAtom(IfNDefNode);
  // check if next node is #DEFINE name
  DefNode:=IfNDefNode.Next;
  if (DefNode=nil) or (DefNode.Desc<>ccnDirective)
  or (DefNode.SubDesc<>ccnsDirectiveDefine) then exit;
  DefMacroName:=ExtractDirectiveFirstAtom(DefNode);
  if CompareIdentifiers(PChar(MacroName),PChar(DefMacroName))<>0 then exit;
  // find #endif
  EndIfNode:=FindEndIf(IfNDefNode);
  if EndIfNode=nil then exit;
  // check that no code comes after the #EndIf
  Node:=EndIfNode;
  while (Node<>nil) do begin
    if (Node.Desc=ccnExtern)
    or ((Node.Desc=ccnDirective)
        and (Node.SubDesc in [ccnsDirectiveIf,ccnsDirectiveIfDef,
          ccnsDirectiveIfNDef,ccnsDirectiveElIf,ccnsDirectiveEndIf]))
    then
      Node:=Node.Next
    else
      exit;
  end;
  Result:=IfNDefNode;
end;

function TCCodeParserTool.GetAtom: string;
begin
  Result:=copy(Src,AtomStart,SrcPos-AtomStart);
end;

procedure TCCodeParserTool.Replace(FromPos, ToPos: integer; const NewSrc: string
  );
var
  Node: TCodeTreeNode;
  DiffPos: Integer;
begin
  {$IFDEF VerboseCCodeParser}
  DebugLn(['TCCodeParserTool.Replace ',FromPos,'-',ToPos,' Old="',copy(Src,FromPos,ToPos-FromPos),'" New="',NewSrc,'"']);
  {$ENDIF}
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

procedure TCCodeParserTool.IncreaseChangeStep;
begin
  if FChangeStep<>$7fffffff then
    inc(FChangeStep)
  else
    FChangeStep:=-$7fffffff;
end;

procedure TCCodeParserTool.WriteDebugReport;
var
  Node: TCodeTreeNode;
begin
  DebugLn(['TCCodeParserTool.WriteDebugReport ']);
  if Tree<>nil then begin
    Node:=Tree.Root;
    while Node<>nil do begin
      DebugLn([GetIndentStr(Node.GetLevel*2)+NodeAsString(Node)]);
      Node:=Node.Next;
    end;
  end;
end;

procedure TCCodeParserTool.CheckNodeTool(Node: TCodeTreeNode);

  procedure RaiseForeignNode;
  begin
    RaiseCatchableException('TCCodeParserTool.CheckNodeTool '+DbgSName(Self)+' '+CCNodeDescAsString(Node.Desc));
  end;

begin
  if Node=nil then exit;
  while Node.Parent<>nil do Node:=Node.Parent;
  while Node.PriorBrother<>nil do Node:=Node.PriorBrother;
  if (Tree=nil) or (Tree.Root<>Node) then
    RaiseForeignNode;
end;

function TCCodeParserTool.NodeAsString(Node: TCodeTreeNode): string;
begin
  if Node=nil then begin
    Result:='nil';
    exit;
  end;
  case Node.Desc of
  ccnName: Result:=copy(Src,Node.StartPos,Node.EndPos-Node.StartPos);
  else Result:='';
  end;
  Result:=Result+'('+CCNodeDescAsString(Node.Desc,Node.SubDesc)
                    +' at '+CleanPosToStr(Node.StartPos)+')';
end;

procedure InternalFinal;
var
  i: Integer;
begin
  if KeyWordLists<>nil then begin
    for i:=0 to KeyWordLists.Count-1 do
      TObject(KeyWordLists[i]).Free;
    FreeAndNil(KeyWordLists);
  end;
end;

finalization
  InternalFinal;

end.

