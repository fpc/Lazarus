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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    TCustomCodeTool is the ancestor class for code tools which parses code
    beginning with the Main Source code. It can parse atoms, the smallest code
    elements in source code, create new code tree nodes and provides several
    useful functions for parsing and changing code.

}
unit CustomCodeTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}
{$inline on}

interface

{$I codetools.inc}

{ $DEFINE ShowIgnoreError}
{ $DEFINE VerboseUpdateNeeded}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, Laz_AVL_Tree,
  // Codetools
  FileProcs, CodeToolsStrConsts, CodeTree, CodeAtom,
  KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache,
  // LazUtils
  LazFileUtils, LazUtilities, LazDbgLog;

type
  TCodeToolPhase = (
    ctpNone,
    ctpScan,
    ctpParse,
    ctpTool
    );

  TCustomCodeTool = class;


  // types for errors

  { ECodeToolError }

  ECodeToolError = class(Exception)
    Sender: TCustomCodeTool;
    Id: int64;
    constructor Create(ASender: TCustomCodeTool; TheId: int64; const AMessage: string);
  end;
  
  ECodeToolErrors = class of ECodeToolError;
  
  { ECodeToolFileNotFound }

  ECodeToolFileNotFound = class(ECodeToolError)
    Filename: string;
    constructor Create(ASender: TCustomCodeTool; TheId: int64;
      const AMessage, AFilename: string);
  end;
  
  
  // types for user aborts
  TOnParserProgress = function(Tool: TCustomCodeTool): boolean of object;
  TCodeTreeChangeEvent = procedure(Tool: TCustomCodeTool;
                                   NodesDeleting: boolean) of object;
  TGetChangeStepEvent = procedure(out ChangeStep: integer) of object;

  EParserAbort = class(ECodeToolError)
  end;

  { TCodeTreeNodeParseError }

  TCodeTreeNodeParseError = class
  public
    Node: TCodeTreeNode;
    CleanPos: integer;
    NicePos: TCodeXYPosition;
    Msg: string;
    constructor Create(ANode: TCodeTreeNode);
  end;
  
  { TCustomCodeTool }

  TCustomCodeTool = class(TObject)
  private
    FLastProgressPos: integer;
    FNodesDeletedChangeStep: integer;
    FOnParserProgress: TOnParserProgress;
    FOnSetGlobalWriteLock: TOnSetWriteLock;
    FScanner: TLinkScanner;
    FOnTreeChange: TCodeTreeChangeEvent;
    FTreeChangeStep: integer;
    FNodeParseErrors: TAVLTree; // tree of TCodeTreeNodeParseError
  protected
    FLastScannerChangeStep: integer;
    FIgnoreErrorAfter: TCodePosition;
    KeyWordFuncList: TKeyWordFunctionList;
    WordIsKeyWordFuncList: TKeyWordFunctionList;
    FRangeValidTill: TLinkScannerRange; // if there was an error, it excludes the last section
    function DefaultKeyWordFunc: boolean;
    procedure BuildDefaultKeyWordFunctions; virtual;
    procedure SetScanner(NewScanner: TLinkScanner); virtual;
    procedure DoDeleteNodes(StartNode: TCodeTreeNode); virtual;
    procedure CloseUnfinishedNodes;
    procedure SaveRaiseIdentExpectedButAtomFound(id: int64);
    procedure SaveRaiseBracketOpenExpectedButAtomFound(id: int64);
    procedure SaveRaiseBracketCloseExpectedButAtomFound(id: int64);
    procedure RaiseUndoImpossible(id: int64);
    procedure SetIgnoreErrorAfter(const AValue: TCodePosition); virtual;
    procedure IncreaseTreeChangeStep(NodesDeleting: boolean);
  protected
    LastErrorMessage: string;
    LastErrorCurPos: TAtomPosition;
    LastErrorValid: boolean;
    LastErrorBehindIgnorePosition: boolean;
    LastErrorCheckedForIgnored: boolean;
    LastErrorNicePosition: TCodeXYPosition;
    LastErrorId: int64;
    procedure ClearLastError;
    procedure RaiseLastError;
    procedure DoProgress; inline;
    procedure NotifyAboutProgress;
    procedure FetchScannerSource; virtual;
    function InternalAtomIsIdentifier: boolean; inline;
  public
    Tree: TCodeTree;

    // current Values, Position, Node ...
    CurPos: TAtomPosition;
    Src: string;
    SrcLen: integer;
    CurNode: TCodeTreeNode;
    LastAtoms: TAtomRing;

    CheckFilesOnDisk: boolean;
    IndentSize: integer;
    VisibleEditorLines: integer;
    JumpSingleLinePos: integer; // in percent 0..100
    JumpCodeBlockPos: integer; // in percent 0..100
    CursorBeyondEOL: boolean;
    
    ErrorPosition: TCodeXYPosition;
    ErrorNicePosition: TCodeXYPosition;// if NiceErrorPosition is set, then it is in front of ErrorPosition

    property Scanner: TLinkScanner read FScanner write SetScanner;
    function MainFilename: string;
    property TreeChangeStep: integer read FTreeChangeStep;
    property NodesDeletedChangeStep: integer read FNodesDeletedChangeStep;
    property OnTreeChange: TCodeTreeChangeEvent read FOnTreeChange
                                                write FOnTreeChange;
    
    function FindDeepestNodeAtPos(P: integer;
      ExceptionOnNotFound: boolean): TCodeTreeNode; {$IFDEF UseInline}inline;{$ENDIF}
    function FindDeepestNodeAtPos(StartNode: TCodeTreeNode; P: integer;
      ExceptionOnNotFound: boolean): TCodeTreeNode;
    function CaretToCleanPos(const Caret: TCodeXYPosition;
        out CleanPos: integer): integer;  // 0=valid CleanPos
                          //-1=CursorPos was skipped, CleanPos between two links
                          // 1=CursorPos beyond scanned code
                          //-2=X,Y beyond source
    function CodePosToCleanPos(const CodePos: TCodePosition;
        out CleanPos: integer): integer;  // 0=valid CleanPos
                          //-1=CursorPos was skipped, CleanPos between two links
                          // 1=CursorPos beyond scanned code
    function CleanPosToCodePos(CleanPos: integer;
        out CodePos:TCodePosition): boolean; // true=ok, false=invalid CleanPos
    function CleanPosToCaret(CleanPos: integer;
        out Caret:TCodeXYPosition): boolean; // true=ok, false=invalid CleanPos
    function CleanPosToCaretAndTopLine(CleanPos: integer;
        out Caret:TCodeXYPosition; out NewTopLine: integer): boolean; // true=ok, false=invalid CleanPos
    function CleanPosToStr(CleanPos: integer; WithFilename: boolean = false): string;
    function CodeXYToStr(const CodePos: TCodeXYPosition; WithFilename: boolean = false): string;
    function CleanPosToRelativeStr(CleanPos: integer;
        const BaseFilename: string): string;
    procedure GetCleanPosInfo(CodePosInFront, CleanPos: integer;
        ResolveComments: boolean; out SameArea: TAtomPosition);
    procedure GetLineInfo(ACleanPos: integer;
        out ALineStart, ALineEnd, AFirstAtomStart, ALastAtomEnd: integer);
    function FindLineEndOrCodeAfterPosition(StartPos: integer;
        SkipEmptyLines: boolean = false; IncludeLineEnd: boolean = false): integer;
    function FindLineEndOrCodeInFrontOfPosition(StartPos: integer;
        StopAtDirectives: boolean = true; SkipEmptyLines: boolean = false): integer;
    function SkipResourceDirective(StartPos: integer): integer;

    function UpdateNeeded(Range: TLinkScannerRange): boolean;
    function UpdateNeeded(OnlyInterfaceNeeded: boolean): boolean; deprecated; // use UpdateNeeded(lsrImplementationStart) or UpdateNeeded(lsrEnd)
    procedure BeginParsing(Range: TLinkScannerRange); virtual;
    procedure BeginParsingAndGetCleanPos(
        Range: TLinkScannerRange; CursorPos: TCodeXYPosition;
        out CleanCursorPos: integer);

    function StringIsKeyWord(const Word: string): boolean;
    
    // cursor moving
    procedure MoveCursorToNodeStart(ANode: TCodeTreeNode); {$IFDEF UseInline}inline;{$ENDIF}
    procedure MoveCursorToCleanPos(ACleanPos: integer);
    procedure MoveCursorToCleanPos(ACleanPos: PChar);
    procedure MoveCursorToAtomPos(const AnAtomPos: TAtomPosition);
    procedure MoveCursorToNearestAtom(ACleanPos: integer);
    procedure MoveCursorToLastNodeAtom(ANode: TCodeTreeNode);
    function IsPCharInSrc(ACleanPos: PChar): boolean;

    // read atoms
    procedure ReadNextAtom;
    procedure UndoReadNextAtom;
    procedure ReadPriorAtom;
    procedure ReadPriorAtomSafe(CleanPos: integer);
    procedure ReadAsStringConstant; // start at CurPos.StartPos

    // read blocks
    function ReadTilBracketClose(ExceptionOnNotFound: boolean): boolean;
    function ReadBackTilBracketOpen(ExceptionOnNotFound: boolean): boolean;
    procedure ReadTillCommentEnd;
    
    // read atoms
    function AtomIs(const AnAtom: shortstring): boolean; // case sensitive
    function UpAtomIs(const AnAtom: shortstring): boolean; // compare to uppercase
    function UpAtomIs(const AtomPos: TAtomPosition; const AnAtom: shortstring): boolean; overload;
    function ReadNextAtomIs(const AnAtom: shortstring): boolean; {$IFDEF UseInline}inline;{$ENDIF}
    function ReadNextAtomIsIdentifier(Identifier: PChar): boolean; {$IFDEF UseInline}inline;{$ENDIF}
    function ReadNextUpAtomIs(const AnAtom: shortstring): boolean; {$IFDEF UseInline}inline;{$ENDIF}
    function ReadNextAtomIsChar(const c: char): boolean; {$IFDEF UseInline}inline;{$ENDIF}
    function AtomIsChar(const c: char): boolean; {$IFDEF UseInline}inline;{$ENDIF}
    function AtomIsKeyWord: boolean;
    function AtomIsNumber: boolean; {$IFDEF UseInline}inline;{$ENDIF}
    function AtomIsRealNumber: boolean;
    function AtomIsStringConstant: boolean; {$IFDEF UseInline}inline;{$ENDIF}
    function AtomIsCharConstant: boolean;
    function AtomIsEmptyStringConstant: boolean;
    function AtomIsIdentifier(Identifier: PChar): boolean;
    function AtomIsIdentifier: boolean;
    procedure AtomIsIdentifierE; overload;
    function AtomIsIdentifierE(ExceptionOnNotFound: boolean): boolean; overload;
    procedure AtomIsIdentifierSaveE(id: int64);
    function AtomIsCustomOperator(AllowIdentifier, ExceptionOnNotFound, SaveE: boolean): boolean;
    function LastAtomIs(BackIndex: integer;
        const AnAtom: shortstring): boolean; // 0=current, 1=prior current, ...
    function LastUpAtomIs(BackIndex: integer;
        const AnAtom: shortstring): boolean; // 0=current, 1=prior current, ...
    function GetAtom: string;
    function GetUpAtom: string;
    function GetAtom(const Atom: TAtomPosition): string;
    function GetUpAtom(const Atom: TAtomPosition): string;
    function FreeUpAtomIs(const FreeAtomPos: TAtomPosition;
        const AnAtom: shortstring): boolean;
    
    // identifiers
    function CompareNodeIdentChars(ANode: TCodeTreeNode;
        const AnUpperIdent: string): integer;
    function CompareSrcIdentifiers(
      CleanStartPos1, CleanStartPos2: integer): boolean;
    function CompareSrcIdentifiers(Identifier1, Identifier2: PChar): boolean;
    function CompareSrcIdentifiers(CleanStartPos: integer;
      AnIdentifier: PChar): boolean;
    function CompareSrcIdentifiersMethod(Identifier1, Identifier2: Pointer): integer;
    function ExtractIdentifier(CleanStartPos: integer): string;
    function ExtractDottedIdentifier(CleanStartPos: integer): string;

    procedure CreateChildNode;
    procedure EndChildNode; {$IFDEF UseInline}inline;{$ENDIF}
    function DoAtom: boolean; virtual;

    // write lock
    procedure ActivateGlobalWriteLock; virtual;
    procedure DeactivateGlobalWriteLock; virtual;
    property OnSetGlobalWriteLock: TOnSetWriteLock
      read FOnSetGlobalWriteLock write FOnSetGlobalWriteLock;
      
    // error handling
    procedure RaiseExceptionInstance(TheException: ECodeToolError;
      ClearNicePos: boolean = true); virtual;
    procedure RaiseExceptionClass(id: int64; const AMessage: string;
      ExceptionClass: ECodeToolErrors; ClearNicePos: boolean); virtual;
    procedure RaiseException(id: int64; const AMessage: string;
      ClearNicePos: boolean = true); virtual;
    procedure RaiseExceptionFmt(id: int64; const AMessage: string;
      const args: array of const; ClearNicePos: boolean = true);
    procedure RaiseExceptionAtCleanPos(id: int64; const AMessage: string;
      CleanPos: integer); virtual;
    procedure RaiseExceptionAtErrorPos(id: int64; const AMessage: string;
      ClearNicePos: boolean = true); virtual; // set ErrorPosition before calling this
    // permanent errors, that the parser will raise again
    procedure SaveRaiseException(id: int64; const AMessage: string;
      ClearNicePos: boolean = true); virtual;
    procedure SaveRaiseExceptionFmt(id: int64; const AMessage: string;
      const args: array of const; ClearNicePos: boolean = true);
    procedure SetNiceErrorPos(CleanPos: integer);
    property IgnoreErrorAfter: TCodePosition
      read FIgnoreErrorAfter write SetIgnoreErrorAfter;
    procedure ClearIgnoreErrorAfter;
    function IgnoreErrorAfterPositionIsInFrontOfLastErrMessage: boolean;
    function IgnoreErrorAfterValid: boolean;
    function IgnoreErrorAfterCleanedPos: integer;
    function CleanPosIsAfterIgnorePos(CleanPos: integer): boolean;
    function LastErrorIsInFrontOfCleanedPos(ACleanedPos: integer): boolean;
    procedure RaiseLastErrorIfInFrontOfCleanedPos(ACleanedPos: integer);
    function GetNodeParserError(Node: TCodeTreeNode): TCodeTreeNodeParseError;
    function SetNodeParserError(Node: TCodeTreeNode; const ErrorMsg: string;
      const ErrorCleanPos: integer;
      const ErrorNiceCleanPos: TCodeXYPosition
      ): TCodeTreeNodeParseError;
    procedure RaiseNodeParserError(Node: TCodeTreeNode);
    procedure RaiseCursorOutsideCode(CursorPos: TCodeXYPosition);
    property OnParserProgress: TOnParserProgress
      read FOnParserProgress write FOnParserProgress;

    // debugging
    procedure Clear; virtual;
    function NodeDescToStr(Desc: integer): string;
    function NodeSubDescToStr(Desc, SubDesc: integer): string;
    procedure ConsistencyCheck; virtual;
    procedure WriteDebugTreeReport;
    procedure CalcMemSize(Stats: TCTMemStats); virtual;
    procedure CheckNodeTool(Node: TCodeTreeNode);
    constructor Create;
    destructor Destroy; override;
  end;
  
var
  RaiseUnhandableExceptions: boolean;

function CompareCodeTreeNodeParserError(Error1, Error2: Pointer): integer;
function CompareNodeWithCodeTreeNodeParserError(Node, Error: Pointer): integer;

implementation

function CompareCodeTreeNodeParserError(Error1, Error2: Pointer): integer;
var
  AnError1: TCodeTreeNodeParseError absolute Error1;
  AnError2: TCodeTreeNodeParseError absolute Error2;
begin
  Result:=ComparePointers(AnError1.Node,AnError2.Node);
end;

function CompareNodeWithCodeTreeNodeParserError(Node, Error: Pointer): integer;
var
  AnError: TCodeTreeNodeParseError absolute Error;
begin
  Result:=ComparePointers(Node,AnError.Node);
end;

{ TCustomCodeTool }

constructor TCustomCodeTool.Create;
begin
  inherited Create;
  Tree:=TCodeTree.Create;
  KeyWordFuncList:=TKeyWordFunctionList.Create('TCustomCodeTool.KeyWordFuncList');
  BuildDefaultKeyWordFunctions;
  WordIsKeyWordFuncList:=WordIsKeyWord;
  LastAtoms:=TAtomRing.Create;
  IndentSize:=2;
  VisibleEditorLines:=20;
  CursorBeyondEOL:=true;
  Clear;
end;

destructor TCustomCodeTool.Destroy;
begin
  Clear;
  FreeAndNil(LastAtoms);
  FreeAndNil(Tree);
  FreeAndNil(KeyWordFuncList);
  inherited Destroy;
end;

procedure TCustomCodeTool.Clear;
begin
  if Tree<>nil then DoDeleteNodes(Tree.Root);
  CurPos:=StartAtomPosition;
  LastAtoms.Clear;
  ClearLastError;
end;

procedure TCustomCodeTool.RaiseException(id: int64; const AMessage: string;
  ClearNicePos: boolean);
begin
  RaiseExceptionClass(id,AMessage,ECodeToolError,ClearNicePos);
end;

procedure TCustomCodeTool.RaiseExceptionFmt(id: int64; const AMessage: string;
  const args: array of const; ClearNicePos: boolean);
begin
  RaiseException(id,Format(AMessage,args),ClearNicePos);
end;

procedure TCustomCodeTool.RaiseExceptionAtCleanPos(id: int64;
  const AMessage: string; CleanPos: integer);
begin
  MoveCursorToCleanPos(CleanPos);
  RaiseException(id,AMessage);
end;

procedure TCustomCodeTool.RaiseExceptionAtErrorPos(id: int64;
  const AMessage: string; ClearNicePos: boolean);
begin
  if ClearNicePos then
    ErrorNicePosition:=CleanCodeXYPosition;
  // raise the exception
  if not RaiseUnhandableExceptions then
    raise ECodeToolError.Create(Self,id,AMessage)
  else
    RaiseCatchableException('['+IntToStr(id)+'] '+AMessage);
end;

procedure TCustomCodeTool.SaveRaiseException(id: int64; const AMessage: string;
  ClearNicePos: boolean);
var
  Node: TCodeTreeNode;
begin
  LastErrorMessage:=AMessage;
  LastErrorCurPos:=CurPos;
  LastErrorValid:=true;
  LastErrorId:=id;
  if ClearNicePos then begin
    LastErrorNicePosition.Code:=nil;
    LastErrorNicePosition.Y:=-1;
  end else begin
    LastErrorNicePosition:=ErrorNicePosition;
  end;

  Node:=CurNode;
  while (Node<>nil) do begin
    if (ctnsNeedJITParsing and Node.SubDesc)>0 then begin
      SetNodeParserError(Node,AMessage,CurPos.StartPos,
                         ErrorNicePosition);
      break;
    end;
    Node:=Node.Parent;
  end;

  RaiseException(id,AMessage,ClearNicePos);
end;

procedure TCustomCodeTool.SaveRaiseExceptionFmt(id: int64;
  const AMessage: string; const args: array of const; ClearNicePos: boolean);
begin
  SaveRaiseException(id,Format(AMessage,args),ClearNicePos);
end;

procedure TCustomCodeTool.SetNiceErrorPos(CleanPos: integer);
var
  CaretXY: TCodeXYPosition;
begin
  // convert cleanpos to caret pos, which is more human readable
  if (CleanPos>SrcLen) and (SrcLen>0) then CleanPos:=SrcLen;
  if (CleanPosToCaret(CleanPos,CaretXY))
  and (CaretXY.Code<>nil) then begin
    ErrorNicePosition:=CaretXY;
  end else if (Scanner<>nil) and (Scanner.MainCode<>nil) then begin
    ErrorNicePosition.Code:=TCodeBuffer(Scanner.MainCode);
    ErrorNicePosition.Y:=-1;
  end else begin
    ErrorNicePosition.Code:=nil;
    ErrorNicePosition.Y:=-1;
  end;
end;

procedure TCustomCodeTool.ClearLastError;
begin
  LastErrorValid:=false;
  LastErrorCheckedForIgnored:=false;
  LastErrorNicePosition.Code:=nil;
  ErrorNicePosition.Code:=nil;
end;

procedure TCustomCodeTool.RaiseLastError;
begin
  MoveCursorToCleanPos(LastErrorCurPos.StartPos);
  CurPos:=LastErrorCurPos;
  ErrorNicePosition:=LastErrorNicePosition;
  SaveRaiseException(LastErrorId,LastErrorMessage,false);
end;

procedure TCustomCodeTool.DoProgress;
begin
  // Check every 1.000.000 chars
  if (FLastProgressPos-CurPos.StartPos)<1000000 then exit;
  NotifyAboutProgress;
end;

procedure TCustomCodeTool.NotifyAboutProgress;
begin
  FLastProgressPos:=CurPos.StartPos;

  if Assigned(OnParserProgress) then begin
    if OnParserProgress(Self) then exit;
    // raise the abort exception to stop the parsing
    RaiseExceptionClass(20170421194502,'Abort',EParserAbort,true);
  end;
end;

procedure TCustomCodeTool.FetchScannerSource;
begin
  // update scanned code
  if FLastScannerChangeStep=Scanner.ChangeStep then begin
    if LastErrorValid then
      RaiseLastError;
  end else begin
    // code has changed
    FLastScannerChangeStep:=Scanner.ChangeStep;
    ClearLastError;
    Src:=Scanner.CleanedSrc;
    SrcLen:=length(Src);
    LastAtoms.SrcLen:=SrcLen;
    {$IFDEF VerboseUpdateNeeded}
    DebugLn(['TCustomCodeTool.BeginParsing ',MainFilename]);
    {$ENDIF}
    FRangeValidTill:=lsrInit;
  end;

  // delete nodes
  if Tree<>nil then
    DoDeleteNodes(Tree.Root);
end;

function TCustomCodeTool.InternalAtomIsIdentifier: boolean;
var
  p: PChar;
begin
  if (CurPos.StartPos<=SrcLen) then begin
    p:=@Src[CurPos.StartPos];
    if IsIdentStartChar[p^] then begin
      if not WordIsKeyWordFuncList.DoIdentifier(p) then
        exit(true);
    end else if (p^='&') and (IsIdentChar[p[1]]) then begin
      exit(true);
    end;
  end;
  Result:=false;
end;

procedure TCustomCodeTool.RaiseUndoImpossible(id: int64);
begin
  RaiseException(id,'TCustomCodeTool.UndoReadNextAtom impossible',true);
end;

procedure TCustomCodeTool.SetScanner(NewScanner: TLinkScanner);
begin
  if NewScanner=FScanner then exit;
  LastErrorCheckedForIgnored:=false;
  Clear;
  FScanner:=NewScanner;
  if Scanner<>nil then begin
    FLastScannerChangeStep:=Scanner.ChangeStep;
    Scanner.SetIgnoreErrorAfter(IgnoreErrorAfter.P,IgnoreErrorAfter.Code);
  end;
  {$IFDEF VerboseUpdateNeeded}
  DebugLn(['TCustomCodeTool.SetScanner ',MainFilename]);
  {$ENDIF}
  FRangeValidTill:=lsrNone;
end;

function TCustomCodeTool.NodeDescToStr(Desc: integer): string;
begin
  Result:=NodeDescriptionAsString(TCodeTreeNodeDesc(Desc));
end;

function TCustomCodeTool.NodeSubDescToStr(Desc, SubDesc: integer): string;
begin
  if SubDesc<>0 then
    Result:=Format(ctsUnknownSubDescriptor,[IntToStr(SubDesc)])
  else
    Result:='';
  case Desc of
  ctnProcedure:
    begin
      if (SubDesc and ctnsForwardDeclaration)>0 then Result:=ctsForward;
    end;
  ctnProcedureHead, ctnBeginBlock:
    begin
      if (SubDesc and ctnsNeedJITParsing)>0 then Result:=ctsUnparsed;
    end;
  ctnClass,ctnObject,ctnRecordType,ctnObjCClass,ctnObjCCategory,ctnObjCProtocol,
  ctnCPPClass,ctnClassInterface,ctnDispinterface,
  ctnTypeHelper,ctnRecordHelper,ctnClassHelper:
    begin
      Result:='';
      if (SubDesc and ctnsForwardDeclaration)>0 then Result:=ctsForward;
      if (SubDesc and ctnsNeedJITParsing)>0 then Result:=Result+ctsUnparsed;
    end;
  end;
  if (SubDesc and ctnsHasParseError)>0 then Result:=Result+','+ctsHasError;
end;

function TCustomCodeTool.AtomIs(const AnAtom: shortstring): boolean;
var AnAtomLen,i : integer;
begin
  Result:=false;
  if (CurPos.StartPos<=SrcLen) and (CurPos.EndPos<=SrcLen+1)
  and (CurPos.StartPos>=1) then begin
    AnAtomLen:=length(AnAtom);
    if AnAtomLen=CurPos.EndPos-CurPos.StartPos then begin
      for i:=1 to AnAtomLen do
        if AnAtom[i]<>Src[CurPos.StartPos-1+i] then exit;
      Result:=true;
    end;
  end;
end;

function TCustomCodeTool.UpAtomIs(const AnAtom: shortstring): boolean;
var
  AnAtomLen, i: integer;
  p: PChar;
begin
  Result:=false;
  AnAtomLen:=length(AnAtom);
  if AnAtomLen<>CurPos.EndPos-CurPos.StartPos then exit;
  if (CurPos.EndPos<=SrcLen+1) and (CurPos.StartPos>=1) then begin
    p:=@Src[CurPos.StartPos];
    for i:=1 to AnAtomLen do begin
      if AnAtom[i]<>UpChars[p^] then exit;
      inc(p);
    end;
    Result:=true;
  end;
end;

function TCustomCodeTool.UpAtomIs(const AtomPos: TAtomPosition;
  const AnAtom: shortstring): boolean;
var
  AnAtomLen, i: integer;
  p: PChar;
begin
  Result:=false;
  AnAtomLen:=length(AnAtom);
  if AnAtomLen<>AtomPos.EndPos-AtomPos.StartPos then exit;
  if (AtomPos.EndPos<=SrcLen+1) and (AtomPos.StartPos>=1) then begin
    p:=@Src[AtomPos.StartPos];
    for i:=1 to AnAtomLen do begin
      if AnAtom[i]<>UpChars[p^] then exit;
      inc(p);
    end;
    Result:=true;
  end;
end;

function TCustomCodeTool.AtomIsIdentifier(Identifier: PChar): boolean;
begin
  Result:=(CurPos.StartPos<=SrcLen)
      and (CompareIdentifiers(Identifier,@Src[CurPos.StartPos])=0);
end;

function TCustomCodeTool.ReadNextAtomIs(const AnAtom: shortstring): boolean;
begin
  ReadNextAtom;
  Result:=AtomIs(AnAtom);
end;

function TCustomCodeTool.ReadNextAtomIsIdentifier(Identifier: PChar): boolean;
begin
  ReadNextAtom;
  Result:=AtomIsIdentifier(Identifier);
end;

function TCustomCodeTool.ReadNextAtomIsChar(const c: char): boolean;
begin
  ReadNextAtom;
  Result:=AtomIsChar(c);
end;

function TCustomCodeTool.ReadNextUpAtomIs(const AnAtom: shortstring): boolean;
begin
  ReadNextAtom;
  Result:=UpAtomIs(AnAtom);
end;

function TCustomCodeTool.CompareNodeIdentChars(ANode: TCodeTreeNode;
  const AnUpperIdent: string): integer;
var
  AnIdentLen, i, NodeSrcLen, MinLen: integer;
  p: PChar;
begin
  {$IFDEF CheckNodeTool}CheckNodeTool(ANode);{$ENDIF}
  if (ANode.StartPos<=SrcLen) and (ANode.EndPos<=SrcLen+1)
  and (ANode.StartPos>=1) then begin
    AnIdentLen:=length(AnUpperIdent);
    NodeSrcLen:=ANode.EndPos-ANode.StartPos;
    if AnIdentLen<NodeSrcLen then
      MinLen:=AnIdentLen
    else
      MinLen:=NodeSrcLen;
    i:=1;
    p:=@Src[ANode.StartPos];
    while (i<=MinLen) and (IsIdentChar[p^]) do begin
      if AnUpperIdent[i]<>UpChars[p^] then begin
        // identifiers different in one letter
        if UpChars[p^]>AnUpperIdent[i] then
          Result:=-1
        else
          Result:=1;
        exit;
      end;
      inc(i);
      inc(p);
    end;
    if (i>MinLen) and (i>AnIdentLen) then begin
      // node is longer than AnUpperIdent
      if (i>NodeSrcLen) or (not IsIdentChar[p^]) then
        // node identifier is equal to AnUpperIdent
        Result:=0
      else
        // node Identifier is longer than AnUpperIdent
        Result:=-1;
    end else
      // node identifier is shorter than AnUpperIdent
      Result:=1
  end else
    Result:=1;
end;

function TCustomCodeTool.CompareSrcIdentifiers(
  CleanStartPos1, CleanStartPos2: integer): boolean;
var
  p1: PChar;
  p2: PChar;
begin
  if (CleanStartPos1<1) or (CleanStartPos1>SrcLen)
     or (CleanStartPos2<1) or (CleanStartPos2>SrcLen)
  then
    exit(false);
  p1:=@Src[CleanStartPos1];
  p2:=@Src[CleanStartPos2];
  while IsIdentChar[p1^] do begin
    if (UpChars[p1^]<>UpChars[p2^]) then
      exit(false);
    inc(CleanStartPos1);
    inc(p1);
    inc(p2);
  end;
  Result:=(not IsIdentChar[p2^]);
end;

function TCustomCodeTool.AtomIsChar(const c: char): boolean;
begin
  Result:=(CurPos.StartPos<=SrcLen)
      and (CurPos.EndPos-CurPos.StartPos=1)
      and (Src[CurPos.StartPos]=c);
end;

function TCustomCodeTool.AtomIsKeyWord: boolean;
begin
  Result:=(CurPos.StartPos<=SrcLen)
      and (IsIdentStartChar[Src[CurPos.StartPos]])
      and (WordIsKeyWordFuncList.DoItCaseInsensitive(Src,CurPos.StartPos,
             CurPos.EndPos-CurPos.StartPos));
end;

function TCustomCodeTool.AtomIsIdentifier:boolean;
begin
  Result:=InternalAtomIsIdentifier;
end;

procedure TCustomCodeTool.AtomIsIdentifierE;

  procedure RaiseEOFFound;
  begin
    RaiseExceptionFmt(20170421194604,ctsIdentExpectedButEOFFound,[GetAtom],true);
  end;

  procedure RaiseAtomFound;
  begin
    RaiseExceptionFmt(20170421194607,ctsIdentExpectedButAtomFound,[GetAtom],true);
  end;

begin
  if InternalAtomIsIdentifier then exit;
  if CurPos.StartPos>SrcLen then
    RaiseEOFFound
  else
    RaiseAtomFound;
end;

function TCustomCodeTool.AtomIsIdentifierE(ExceptionOnNotFound: boolean): boolean;
begin
  if InternalAtomIsIdentifier then exit(true);
  Result:=false;
  if not ExceptionOnNotFound then exit;
  AtomIsIdentifierE();
end;

procedure TCustomCodeTool.AtomIsIdentifierSaveE(id: int64);

  procedure SaveRaiseIdentExpectedButEOFFound;
  begin
    SaveRaiseExceptionFmt(id,ctsIdentExpectedButEOFFound,[GetAtom]);
  end;

begin
  if InternalAtomIsIdentifier then exit;
  if CurPos.StartPos>SrcLen then
    SaveRaiseIdentExpectedButEOFFound
  else
    SaveRaiseIdentExpectedButAtomFound(id);
end;

function TCustomCodeTool.AtomIsCustomOperator(AllowIdentifier,
  ExceptionOnNotFound, SaveE: boolean): boolean;

  procedure RaiseOperatorExpected;
  begin
    if CurPos.StartPos>SrcLen then begin
      if SaveE then
        SaveRaiseException(20170421194635,ctsOperatorExpectedButEOFFound)
      else
        RaiseException(20170421194649,ctsOperatorExpectedButEOFFound)
    end else begin
      if SaveE then
        SaveRaiseExceptionFmt(20170421194701,ctsOperatorExpectedButAtomFound,[GetAtom])
      else
        RaiseExceptionFmt(20170421194704,ctsOperatorExpectedButAtomFound,[GetAtom])
    end;
  end;

begin
  Result:=false;
  if (CurPos.StartPos<=SrcLen) then begin
    if WordIsCustomOperator.DoItCaseInsensitive(
      Src,CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
    or AllowIdentifier and AtomIsIdentifier then
      exit(true);
  end;
  if not ExceptionOnNotFound then
    exit(false);
  RaiseOperatorExpected;
end;

function TCustomCodeTool.AtomIsNumber: boolean;
var
  p: PChar;
begin
  if CurPos.StartPos>SrcLen then exit(false);
  p:=@Src[CurPos.StartPos];
  if p^ in ['0'..'9','%','$'] then exit(true);
  if (p^='&') and IsOctNumberChar[p[1]] then exit(true);
  Result:=false;
end;

function TCustomCodeTool.AtomIsRealNumber: boolean;
var i: integer;
begin
  Result:=false;
  i:=CurPos.StartPos;
  if (i<=SrcLen) and (IsNumberChar[Src[i]]) then begin
    while (i<=SrcLen) and (IsNumberChar[Src[i]]) do
      inc(i);
    if (i<SrcLen) and (Src[i]='.') and (IsNumberChar[Src[i+1]]) then
      Result:=true;
  end;
end;

function TCustomCodeTool.AtomIsStringConstant: boolean;
var
  p: PChar;
begin
  if (CurPos.StartPos<1) or (CurPos.StartPos>SrcLen) then exit(false);
  p:=@Src[CurPos.StartPos];
  Result:=(p^ in ['''','#']) or ((p^='^') and (p[1] in ['A'..'Z']));
end;

function TCustomCodeTool.AtomIsCharConstant: boolean;
var
  p: PChar;
begin
  Result:=false;
  if (CurPos.StartPos<=SrcLen) then begin
    p:=@Src[CurPos.StartPos];
    case p^ of
    
    '#':
      begin
        inc(p);
        if IsNumberChar[p^] then begin
          // decimal
          while IsNumberChar[p^] do
            inc(p);
        end else if p^='$' then begin
          // hexadecimal
          Inc(p);
          while IsHexNumberChar[p^] do
            inc(p);
        end;
      end;

    '''':
      begin
        inc(p);
        if p^='''' then begin
          // could be ''''
          if (p[1]<>'''') or (p[2]<>'''') then exit;
          inc(p,3);
        end else begin
          // could be 'a'
          if p[1]<>'''' then exit;
          inc(p,2);
        end;
      end;

    '^':
      begin
        if not (p[1] in ['A'..'Z']) then exit;
        inc(p,2);
      end;

    end;
    // check that no second character is following
    Result:=not (p^ in ['''','#','^']);
  end;
end;

function TCustomCodeTool.AtomIsEmptyStringConstant: boolean;
var
  p: LongInt;
begin
  p:=CurPos.StartPos;
  while (p<=SrcLen) and (Src[p]='''') do inc(p);
  dec(p,CurPos.StartPos);
  Result:=(p>0) and ((p and 1)=0);
end;

function TCustomCodeTool.LastAtomIs(BackIndex: integer;
  const AnAtom: shortstring): boolean;
var ap: TAtomPosition;
  AnAtomLen: integer;
  i: integer;
begin
  ap:=LastAtoms.GetAtomAt(-BackIndex);
  if ap.StartPos>=ap.EndPos then exit(false);
  if (ap.EndPos<=SrcLen+1) then begin
    AnAtomLen:=length(AnAtom);
    if AnAtomLen<>ap.EndPos-ap.StartPos then exit(false);
    for i:=1 to AnAtomLen do
      if AnAtom[i]<>Src[ap.StartPos-1+i] then exit(false);
    exit(true);
  end;
  Result:=false;
end;

function TCustomCodeTool.LastUpAtomIs(BackIndex: integer;
  const AnAtom: shortstring): boolean;
var ap: TAtomPosition;
  AnAtomLen: integer;
  i: integer;
  p: PChar;
begin
  ap:=LastAtoms.GetAtomAt(-BackIndex);
  if ap.StartPos>=ap.EndPos then exit(false);
  if (ap.EndPos<=SrcLen+1) then begin
    AnAtomLen:=length(AnAtom);
    if AnAtomLen<>ap.EndPos-ap.StartPos then exit(false);
    p:=@Src[ap.StartPos];
    for i:=1 to AnAtomLen do begin
      if AnAtom[i]<>UpChars[p^] then exit(false);
      inc(p);
    end;
    exit(true);
  end;
  Result:=false;
end;

function TCustomCodeTool.GetAtom: string;
begin
  Result:=copy(Src,CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
end;

function TCustomCodeTool.GetUpAtom: string;
begin
  Result:=UpperCaseStr(copy(Src,CurPos.StartPos,CurPos.EndPos-CurPos.StartPos));
end;

function TCustomCodeTool.GetAtom(const Atom: TAtomPosition): string;
begin
  Result:=copy(Src,Atom.StartPos,Atom.EndPos-Atom.StartPos);
end;

function TCustomCodeTool.GetUpAtom(const Atom: TAtomPosition): string;
begin
  Result:=UpperCaseStr(copy(Src,Atom.StartPos,Atom.EndPos-Atom.StartPos));
end;

function TCustomCodeTool.FreeUpAtomIs(const FreeAtomPos: TAtomPosition;
  const AnAtom: shortstring): boolean;
var AnAtomLen,i : integer;
  p: PChar;
begin
  Result:=false;
  if (FreeAtomPos.StartPos>SrcLen) or (FreeAtomPos.EndPos>SrcLen+1)
  or (FreeAtomPos.StartPos<1) then
    exit;
  AnAtomLen:=length(AnAtom);
  if AnAtomLen<>FreeAtomPos.EndPos-FreeAtomPos.StartPos then exit;
  p:=@Src[FreeAtomPos.StartPos];
  for i:=1 to AnAtomLen do begin
    if AnAtom[i]<>UpChars[p^] then exit;
    inc(p);
  end;
  Result:=true;
end;

{$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
{$R-}
procedure TCustomCodeTool.ReadNextAtom;
var
  c1, c2: char;
  CommentLvl: integer;
  p: PChar;
begin
  if LastAtoms.HasNext then begin
    //debugln(['TCustomCodeTool.ReadNextAtom HASNEXT ',LastAtoms.NextCount]);
    LastAtoms.MoveToNext(CurPos);
    //debugln(['TCustomCodeTool.ReadNextAtom ',GetAtom,' ',CurPos.StartPos,'-',CurPos.EndPos,' ',SrcLen]);
    exit;
  end;

  CurPos.StartPos:=CurPos.EndPos;
  CurPos.Flag:=cafNone;
  if CurPos.StartPos>SrcLen then begin
    if (not LastAtoms.Empty) and (LastAtoms.GetPriorAtom.StartPos<CurPos.StartPos)
    then
      LastAtoms.Add(CurPos);
    exit;
  end;
  // Skip all spaces and comments
  p:=@Src[CurPos.StartPos];
  while true do begin
    case p^ of
    #0:
      begin
        CurPos.StartPos:=p-PChar(Src)+1;
        if CurPos.StartPos>SrcLen then
          break
        else
          inc(p);
      end;
    #1..#32:
      inc(p);
    #$EF:
      if (p[1]=#$BB)
      and (p[2]=#$BF) then begin
        // skip UTF BOM
        inc(p,3);
      end else begin
        break;
      end;
    '{': // pascal comment
      begin
        inc(p);
        if p^=#3 then begin
          // codetools skip comment {#3 #3}
          repeat
            case p^ of
            #0:
              begin
                CurPos.StartPos:=p-PChar(Src)+1;
                if CurPos.StartPos>SrcLen then break;
              end;
            #3:
              if p[1]='}' then begin
                inc(p,2);
                break;
              end;
            end;
            inc(p);
          until false;
        end else begin
          // pascal comment {}
          CommentLvl:=1;
          while true do begin
            case p^ of
            #0:
              begin
                CurPos.StartPos:=p-PChar(Src)+1;
                if CurPos.StartPos>SrcLen then break;
              end;
            '{':
              if Scanner.NestedComments then
                inc(CommentLvl);
            '}':
              begin
                dec(CommentLvl);
                if CommentLvl=0 then begin
                  inc(p);
                  break;
                end;
              end;
            end;
            inc(p);
          end;
        end;
      end;
    '/':  // Delphi comment
      if p[1]<>'/' then begin
        break;
      end else begin
        inc(p,2);
        while not (p^ in [#10,#13,#0]) do
          inc(p);
      end;
    '(': // old turbo pascal comment
      if p[1]<>'*' then begin
        break;
      end else begin
        inc(p,2);
        CommentLvl:=1;
        while true do begin
          case p^ of
          #0:
            begin
              CurPos.StartPos:=p-PChar(Src)+1;
              if CurPos.StartPos>SrcLen then break;
            end;
          '(':
            if (p[1]='*') and Scanner.NestedComments then begin
              //debugln('TCustomCodeTool.ReadNextAtom ',copy(Src,CurPos.StartPos,CurPos.StartPos-CurPos.EndPos));
              inc(CommentLvl);
              inc(p);
            end;
          '*':
            if p[1]=')' then begin
              dec(CommentLvl);
              if CommentLvl=0 then begin
                inc(p,2);
                break;
              end;
              inc(p);
            end;
          end;
          inc(p);
        end;
      end;
    else
      break;
    end;
  end;
  CurPos.StartPos:=p-PChar(Src)+1;
  CurPos.EndPos:=CurPos.StartPos;
  // read atom
  c1:=p^;
  case c1 of
  #0: ;
  '_','A'..'Z','a'..'z':
    begin
      inc(p);
      while IsIdentChar[p^] do
        inc(p);
      CurPos.Flag:=cafWord;
      CurPos.EndPos:=p-PChar(Src)+1;
      case c1 of
      'e','E':
        if (CurPos.EndPos-CurPos.StartPos=3)
        and (Src[CurPos.StartPos+1] in ['n','N'])
        and (Src[CurPos.StartPos+2] in ['d','D'])
        and ((CurPos.StartPos=1) or (Src[CurPos.StartPos-1]<>'@'))
        then
          CurPos.Flag:=cafEnd;
      end;
    end;
  '''','#':
    begin
      // string constant
      while true do begin
        case p^ of
        '#':
          begin
            inc(p);
            if IsNumberChar[p^] then begin
              // decimal
              repeat
                inc(p);
              until not IsNumberChar[p^];
            end else if p^='$' then begin
              // hexadecimal
              repeat
                inc(p);
              until not IsHexNumberChar[p^];
            end;
          end;
        '''':
          begin
            inc(p);
            while true do begin
              case p^ of
              '''':
                begin
                  inc(p);
                  break;
                end;
              #0,#10,#13:
                break;
              else
                inc(p);
              end;
            end;
          end;
        '^':
          begin
            inc(p);
            if not (p^ in ['A'..'Z']) then break;
            inc(p);
          end;
        else
          break;
        end;
      end;
      CurPos.EndPos:=p-PChar(Src)+1;
    end;
  '0'..'9':
    begin
      inc(p);
      while IsNumberChar[p^] do
        inc(p);
      if (p^='.') and IsAfterFloatPointChar[p[1]] then begin
        // real type number
        inc(p);
        while IsNumberChar[p^] do
          inc(p);
      end;
      if p^ in ['e','E'] then begin
        // read exponent
        inc(p);
        if p^ in ['-','+'] then inc(p);
        while IsNumberChar[p^] do
          inc(p);
      end;
      CurPos.EndPos:=p-PChar(Src)+1;
    end;
  '%': // binary number
    begin
      inc(p);
      while p^ in ['0'..'1'] do
        inc(p);
      CurPos.EndPos:=p-PChar(Src)+1;
    end;
  '&': // octal number or keyword as identifier
    begin
      inc(p);
      if IsOctNumberChar[p^] then begin
        while IsOctNumberChar[p^] do
          inc(p);
      end else if IsIdentStartChar[p^] then begin
        CurPos.Flag:=cafWord;
        while IsIdentChar[p^] do
          inc(p);
      end;
      CurPos.EndPos:=p-PChar(Src)+1;
    end;
  '$': // hex number
    begin
      inc(p);
      while IsHexNumberChar[p^] do
        inc(p);
      CurPos.EndPos:=p-PChar(Src)+1;
    end;
  ';':
    begin
      inc(CurPos.EndPos);
      CurPos.Flag:=cafSemicolon;
    end;
  ',':
    begin
      inc(CurPos.EndPos);
      CurPos.Flag:=cafComma;
    end;
  '=':
    begin
      inc(CurPos.EndPos);
      CurPos.Flag:=cafEqual;
    end;
  '(':
    begin
      inc(CurPos.EndPos);
      CurPos.Flag:=cafRoundBracketOpen;
    end;
  ')':
    begin
      inc(CurPos.EndPos);
      CurPos.Flag:=cafRoundBracketClose;
    end;
  '[':
    begin
      inc(CurPos.EndPos);
      CurPos.Flag:=cafEdgedBracketOpen;
    end;
  ']':
    begin
      inc(CurPos.EndPos);
      CurPos.Flag:=cafEdgedBracketClose;
    end;
  ':':
    begin
      inc(CurPos.EndPos);
      if (Src[CurPos.EndPos]<>'=') then begin
        CurPos.Flag:=cafColon;
      end else begin
        // :=
        inc(CurPos.EndPos);
        CurPos.Flag:=cafAssignment;
      end;
    end;
  '.':
    begin
      inc(CurPos.EndPos);
      if (Src[CurPos.EndPos]<>'.') then begin
        // '.'
        CurPos.Flag:=cafPoint;
      end else begin
        inc(CurPos.EndPos);
        if (Src[CurPos.EndPos]<>'.') then begin
          // '..'
        end else begin
          // '...'
          inc(CurPos.EndPos);
        end;
      end;
    end;
  #192..#255:
    begin
      // read UTF8 character
      inc(p);
      if ((ord(c1) and %11100000) = %11000000) then begin
        // could be 2 byte character
        if (ord(p[0]) and %11000000) = %10000000 then
          inc(p);
      end
      else if ((ord(c1) and %11110000) = %11100000) then begin
        // could be 3 byte character
        if ((ord(p[0]) and %11000000) = %10000000)
        and ((ord(p[1]) and %11000000) = %10000000) then
          inc(p,2);
      end
      else if ((ord(c1) and %11111000) = %11110000) then begin
        // could be 4 byte character
        if ((ord(p[0]) and %11000000) = %10000000)
        and ((ord(p[1]) and %11000000) = %10000000)
        and ((ord(p[2]) and %11000000) = %10000000) then
          inc(p,3);
      end;
      CurPos.EndPos:=p-PChar(Src)+1;
    end;
  else
    inc(CurPos.EndPos);
    c2:=Src[CurPos.EndPos];
    // test for double char operators :=, +=, -=, /=, *=, <>, <=, >=, **, ><
    if ((c2='=') and  (IsEqualOperatorStartChar[c1]))
      or ((c1='<') and (c2='>')) // <> not equal
      or ((c1='>') and (c2='<')) // >< symmetric diff for sets
      or ((c1='.') and (c2='.')) // .. subrange
      or ((c1='*') and (c2='*')) // ** power
      then begin
        // 2 character operator/symbol
        inc(CurPos.EndPos);
    end
    else if ((c1='@') and (c2='@')) then begin
      // @@ label
      repeat
        inc(CurPos.EndPos);
      until (CurPos.EndPos>SrcLen) or (not IsIdentChar[Src[CurPos.EndPos]]);
    end;
  end;
  LastAtoms.Add(CurPos);
end;
{$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}

procedure TCustomCodeTool.ReadPriorAtom;
var
  CommentLvl, PrePos, OldPrePos: integer;
  IsStringConstant: boolean;

  procedure ReadStringConstantBackward;
  var PrePos: integer;
  begin
    while (CurPos.StartPos>1) do begin
      case Src[CurPos.StartPos-1] of
      '''':
        begin
          dec(CurPos.StartPos);
          repeat
            dec(CurPos.StartPos);
          until (CurPos.StartPos<1) or (Src[CurPos.StartPos] in [#0,#10,#13,'''']);
        end;
      '0'..'9','A'..'Z','a'..'z':
        begin
          // test if char constant
          PrePos:=CurPos.StartPos-1;
          while (PrePos>1) and (IsHexNumberChar[Src[PrePos]]) do
            dec(PrePos);
          if (PrePos<1) then break;
          if (Src[PrePos]='$') then begin
            dec(PrePos);
            if (PrePos<1) then break;
          end;
          if (Src[PrePos]='#') then
            CurPos.StartPos:=PrePos
          else
            break;
        end;
      else
        break;
      end;
    end;
  end;
  
  procedure ReadBackTilCodeLineEnd;
  begin
    dec(CurPos.StartPos);
    if (CurPos.StartPos>=1) and (Src[CurPos.StartPos] in [#10,#13])
    and (Src[CurPos.StartPos+1]<>Src[CurPos.StartPos]) then
      dec(CurPos.StartPos);

    // read backwards till line start
    PrePos:=CurPos.StartPos;
    while (PrePos>=1) and (not (Src[PrePos] in [#10,#13])) do
      dec(PrePos);
    // read line forward to find out,
    // if line ends in comment or string constant
    IsStringConstant:=false;
    repeat
      inc(PrePos);
      case Src[PrePos] of

      '/':
        if Src[PrePos+1]='/' then begin
          // this was a delphi comment -> skip comment
          CurPos.StartPos:=PrePos-1;
          break;
        end;

      '{':
        begin
          inc(PrePos);
          if (PrePos<=CurPos.StartPos) and (Src[PrePos]=#3) then begin
            // skip codetools comment
            inc(PrePos);
            while (PrePos<=CurPos.StartPos) do begin
              if (Src[PrePos]=#3) and (PrePos<CurPos.StartPos)
              and (Src[PrePos+1]='}') then begin
                inc(PrePos,2);
                break;
              end;
              inc(PrePos);
            end;
          end else begin
            // skip pascal comment
            CommentLvl:=1;
            while (PrePos<=CurPos.StartPos) do begin
              case Src[PrePos] of
              '{': if Scanner.NestedComments then inc(CommentLvl);
              '}':
                begin
                  dec(CommentLvl);
                  if CommentLvl=0 then break;
                end;
              end;
              inc(PrePos);
            end;
          end;
        end;

      '(':
        if Src[PrePos+1]='*' then begin
          // skip turbo pascal comment
          inc(PrePos,2);
          while (PrePos<CurPos.StartPos)
          and ((Src[PrePos]<>'*') or (Src[PrePos+1]<>')')) do
            inc(PrePos);
          inc(PrePos);
        end;

      '''':
        begin
          // a string constant -> skip it
          OldPrePos:=PrePos;
          while (PrePos<CurPos.StartPos) do begin
            inc(PrePos);
            case Src[PrePos] of
            '''':
              break;

            #0,#10,#13:
              begin
                // string constant right border is the line end
                // -> last atom of line found
                IsStringConstant:=true;
                break;
              end;

            end;
          end;
          if IsStringConstant then break;
        end;

      #10,#13:
        // no comment and no string constant found
        break;

      end;
    until PrePos>=CurPos.StartPos;
  end;

type
  TNumberType = (ntDecimal, ntHexadecimal, ntBinary, ntIdentifier,
    ntCharConstant, ntFloat, ntFloatWithExponent);
  TNumberTypes = set of TNumberType;

const
  AllNumberTypes: TNumberTypes = [ntDecimal, ntHexadecimal, ntBinary,
    ntIdentifier, ntCharConstant, ntFloat, ntFloatWithExponent];

var c1, c2: char;
  ForbiddenNumberTypes: TNumberTypes;
begin
  if LastAtoms.HasPrior then begin
    LastAtoms.GoBack(CurPos);
    exit;
  end;
  // Skip all spaces and comments
  CommentLvl:=0;
  dec(CurPos.StartPos);
  CurPos.Flag:=cafNone;
  IsStringConstant:=false;
  OldPrePos:=0;
  while CurPos.StartPos>=1 do begin
    if IsCommentEndChar[Src[CurPos.StartPos]] then begin
      case Src[CurPos.StartPos] of
      
      '}':
        begin
          dec(CurPos.StartPos);
          if (CurPos.StartPos>=1) and (Src[CurPos.StartPos]=#3) then begin
            // codetools skip comment {#3 #3}
            dec(CurPos.StartPos);
            while (CurPos.StartPos>=1) do begin
              if (Src[CurPos.StartPos]=#3) and (CurPos.StartPos>1)
              and (Src[CurPos.StartPos-1]='{') then begin
                dec(CurPos.StartPos,2);
                break;
              end;
              dec(CurPos.StartPos);
            end;
          end else begin
            // pascal comment {}
            CommentLvl:=1;
            while (CurPos.StartPos>=1) and (CommentLvl>0) do begin
              case Src[CurPos.StartPos] of
              '}': if Scanner.NestedComments then inc(CommentLvl);
              '{': dec(CommentLvl);
              end;
              dec(CurPos.StartPos);
            end;
          end;
        end;
        
      #10,#13: // possible Delphi comment
        ReadBackTilCodeLineEnd;

      ')': // old turbo pascal comment
        if (CurPos.StartPos>1) and (Src[CurPos.StartPos-1]='*') then begin
          dec(CurPos.StartPos,3);
          while (CurPos.StartPos>=1)
          and ((Src[CurPos.StartPos]<>'(') or (Src[CurPos.StartPos+1]<>'*')) do
            dec(CurPos.StartPos);
          dec(CurPos.StartPos);
        end else
          break;
          
      end;
    end else if IsSpaceChar[Src[CurPos.StartPos]] then begin
      repeat
        dec(CurPos.StartPos);
      until (CurPos.StartPos<1) or (Src[CurPos.StartPos] in [#10,#13])
      or (not (IsSpaceChar[Src[CurPos.StartPos]]));
    end else begin
      break;
    end;
  end;
  // CurPos.StartPos now points to the last char of the prior atom
  CurPos.EndPos:=CurPos.StartPos+1;
  if CurPos.StartPos<1 then begin
    CurPos.StartPos:=1;
    CurPos.EndPos:=1;
    LastAtoms.Clear;
    exit;
  end;
  // read atom
  if IsStringConstant then begin
    CurPos.StartPos:=OldPrePos;
    if (CurPos.StartPos>1) and (Src[CurPos.StartPos-1]='''') then begin
      ReadStringConstantBackward;
    end;
    LastAtoms.AddReverse(CurPos);
    exit;
  end;
  c2:=Src[CurPos.StartPos];
  case c2 of
    '_','A'..'Z','a'..'z':
      begin
        // identifier or keyword or hexnumber
        while (CurPos.StartPos>1) do begin
          if (IsIdentChar[Src[CurPos.StartPos-1]]) then
            dec(CurPos.StartPos)
          else begin
            case UpChars[Src[CurPos.StartPos-1]] of
            '@':
              // assembler label
              if (CurPos.StartPos>2)
              and (Src[CurPos.StartPos-2]='@') then
                dec(CurPos.StartPos,2);
            '$':
              // hex number
              dec(CurPos.StartPos);
            else
              case UpChars[Src[CurPos.StartPos]] of
              'E':
                if CompareSrcIdentifiers(CurPos.StartPos,'END') then
                  CurPos.Flag:=cafEnd
                else
                  CurPos.Flag:=cafWord;
              else
                CurPos.Flag:=cafWord;
              end;
            end;
            break;
          end;
        end;
      end;
    '''':
      begin
        inc(CurPos.StartPos);
        ReadStringConstantBackward;
      end;
    '0'..'9':
      begin
        // could be a decimal number, an identifier, a hex number,
        // a binary number, a char constant, a float, a float with exponent
        ForbiddenNumberTypes:=[];
        while true do begin
          case UpChars[Src[CurPos.StartPos]] of
          '0'..'1':
            ;
          '2'..'9':
            ForbiddenNumberTypes:=ForbiddenNumberTypes+[ntBinary];
          'A'..'D','F':
            ForbiddenNumberTypes:=ForbiddenNumberTypes
               +[ntBinary,ntDecimal,ntCharConstant,ntFloat,ntFloatWithExponent];
          'E':
            ForbiddenNumberTypes:=ForbiddenNumberTypes
               +[ntBinary,ntDecimal,ntCharConstant,ntFloat];
          'G'..'Z','_':
            ForbiddenNumberTypes:=AllNumberTypes-[ntIdentifier];
          '.':
            begin
              // could be the point of a float
              if (ntFloat in ForbiddenNumberTypes)
              or (CurPos.StartPos<=1) or (Src[CurPos.StartPos-1]='.') then begin
                inc(CurPos.StartPos);
                break;
              end;
              dec(CurPos.StartPos);
              // this was the part of a float after the point
              //  -> read decimal in front
              ForbiddenNumberTypes:=AllNumberTypes-[ntDecimal];
            end;
          '+','-':
            begin
              // could be part of an exponent
              if (ntFloatWithExponent in ForbiddenNumberTypes)
              or (CurPos.StartPos<=1)
              or (not (Src[CurPos.StartPos-1] in ['e','E']))
              then begin
                inc(CurPos.StartPos);
                break;
              end;
              dec(CurPos.StartPos);
              // this was the exponent of a float -> read the float
              ForbiddenNumberTypes:=AllNumberTypes-[ntFloat];
            end;
          '#': // char constant found
            begin
              if (ntCharConstant in ForbiddenNumberTypes) then
                inc(CurPos.StartPos);
              ReadStringConstantBackward;
              break;
            end;
          '$':
            begin
              // hexadecimal number found
              if (ntHexadecimal in ForbiddenNumberTypes) then
                inc(CurPos.StartPos);
              break;
            end;
          '%':
            begin
              // binary number found
              if (ntBinary in ForbiddenNumberTypes) then
                inc(CurPos.StartPos);
              break;
            end;
          '@':
            begin
              if (CurPos.StartPos=1) or (Src[CurPos.StartPos-1]<>'@')
              or (([ntIdentifier,ntDecimal]*ForbiddenNumberTypes)=[]) then
                // atom start found
                inc(CurPos.StartPos)
              else
                // label found
                dec(CurPos.StartPos);
              break;
            end;
          else
            begin
              inc(CurPos.StartPos);
              break;
            end;
          end;
          if ForbiddenNumberTypes=AllNumberTypes then begin
            inc(CurPos.StartPos);
            break;
          end;
          if CurPos.StartPos<=1 then break;
          dec(CurPos.StartPos);
        end;
        if IsIdentStartChar[Src[CurPos.StartPos]] then begin
          // it is an identifier
          CurPos.Flag:=cafWord;
          case UpChars[Src[CurPos.StartPos]] of
          'E':
            if CompareSrcIdentifiers(CurPos.StartPos,'END') then
              CurPos.Flag:=cafEnd;
          end;
        end;
      end;
      
    ';': CurPos.Flag:=cafSemicolon;
    ':': CurPos.Flag:=cafColon;
    ',': CurPos.Flag:=cafComma;
    '(': CurPos.Flag:=cafRoundBracketOpen;
    ')': CurPos.Flag:=cafRoundBracketClose;
    '[': CurPos.Flag:=cafEdgedBracketOpen;
    ']': CurPos.Flag:=cafEdgedBracketClose;

    else
      begin
        if CurPos.StartPos>1 then begin
          c1:=Src[CurPos.StartPos-1];
          // test for double char operators :=, +=, -=, /=, *=, <>, <=, >=, **, ><
          if ((c2='=') and (c1=':')) then
          begin
            dec(CurPos.StartPos);
            CurPos.Flag:=cafAssignment;
          end else
          if ((c2='=') and (IsEqualOperatorStartChar[c1]))
          or ((c1='<') and (c2='>'))
          or ((c1='>') and (c2='<'))
          or ((c1='.') and (c2='.'))
          or ((c1='*') and (c2='*'))
          or ((c1='@') and (c2='@'))
          then begin
            dec(CurPos.StartPos);
            CurPos.Flag:=cafOtherOperator;
          end else begin
            case c2 of
            '=': CurPos.Flag:=cafEqual;
            '.': CurPos.Flag:=cafPoint;
            end;
          end;
        end else begin
          case c2 of
          '=': CurPos.Flag:=cafEqual;
          '.': CurPos.Flag:=cafPoint;
          end;
        end;
      end;
  end;
  LastAtoms.AddReverse(CurPos);
end;

procedure TCustomCodeTool.ReadPriorAtomSafe(CleanPos: integer);
var
  Node: TCodeTreeNode;
begin
  // find a clean pos in front
  Node:=FindDeepestNodeAtPos(CleanPos,false);
  repeat
    repeat
      if Node=nil then begin
        CurPos:=CleanAtomPosition;
        exit;
      end;
      if Node.EndPos<CleanPos then begin
        MoveCursorToCleanPos(Node.EndPos);
        break;
      end else if Node.StartPos<CleanPos then begin
        MoveCursorToCleanPos(Node.StartPos);
        break;
      end;
      Node:=Node.Prior;
    until false;
    ReadNextAtom;
    if CurPos.StartPos<CleanPos then
      break;
    // first atom of node is behind CleanPos => try prior node
    Node:=Node.Prior;
  until false;
  if CurPos.EndPos>CleanPos then begin
    CurPos:=CleanAtomPosition;
    exit;
  end;
  if CurPos.EndPos=CleanPos then
    exit;
  repeat
    ReadNextAtom;
    if CurPos.EndPos>=CleanPos then begin
      if CurPos.EndPos>CleanPos then
        UndoReadNextAtom;
      exit;
    end;
  until false;
end;

procedure TCustomCodeTool.ReadAsStringConstant;
var
  p: PChar;
begin
  CurPos.Flag:=cafNone;
  if CurPos.StartPos>SrcLen then begin
    LastAtoms.ClearCurrent;
    CurPos.EndPos:=CurPos.StartPos;
    exit;
  end;
  p:=@Src[CurPos.StartPos];
  // string constant
  while true do begin
    case p^ of
    '#':
      begin
        inc(p);
        if IsNumberChar[p^] then begin
          // decimal
          repeat
            inc(p);
          until not IsNumberChar[p^];
        end else if p^='$' then begin
          // hexadecimal
          repeat
            inc(p);
          until not IsHexNumberChar[p^];
        end;
      end;
    '''':
      begin
        inc(p);
        while true do begin
          case p^ of
          '''':
            begin
              inc(p);
              break;
            end;

          #0,#10,#13:
            break;

          else
            inc(p);
          end;
        end;
      end;
    '^':
      begin
        inc(p);
        if not (p^ in ['A'..'Z']) then break;
        inc(p);
      end;
    else
      break;
    end;
  end;
  CurPos.EndPos:=p-PChar(Src)+1;
  LastAtoms.SetCurrent(CurPos);
end;

procedure TCustomCodeTool.UndoReadNextAtom;
begin
  if not LastAtoms.GoBack(CurPos) then
    RaiseUndoImpossible(20170421194733);
end;

function TCustomCodeTool.ReadTilBracketClose(
  ExceptionOnNotFound: boolean): boolean;
// reads code brackets (not comment brackets)
// after call cursor is on the closing bracket
var CloseBracket, AntiCloseBracket: TCommonAtomFlag;
  Start: TAtomPosition;
  
  procedure RaiseBracketNotFound;
  begin
    if CloseBracket=cafRoundBracketClose then
      SaveRaiseExceptionFmt(20170421194736,ctsBracketNotFound,[')'],false)
    else
      SaveRaiseExceptionFmt(20170421194740,ctsBracketNotFound,[']'],false);
  end;
  
begin
  Result:=false;
  if (Curpos.Flag=cafRoundBracketOpen) then begin
    CloseBracket:=cafRoundBracketClose;
    AntiCloseBracket:=cafEdgedBracketClose;
  end else if (Curpos.Flag=cafEdgedBracketOpen) then begin
    CloseBracket:=cafEdgedBracketClose;
    AntiCloseBracket:=cafRoundBracketClose;
  end else begin
    if ExceptionOnNotFound then
      SaveRaiseBracketOpenExpectedButAtomFound(20170421194744);
    exit;
  end;
  Start:=CurPos;
  repeat
    ReadNextAtom;
    if (CurPos.Flag=CloseBracket) then break;
    if (CurPos.StartPos>SrcLen)
    or (CurPos.Flag in [cafEnd,AntiCloseBracket])
    then begin
      SetNiceErrorPos(Start.StartPos);
      if ExceptionOnNotFound then begin
        RaiseBracketNotFound;
      end;
      exit;
    end;
    if (CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen]) then begin
      if not ReadTilBracketClose(ExceptionOnNotFound) then exit;
    end;
  until false;
  Result:=true;
end;

function TCustomCodeTool.ReadBackTilBracketOpen(
  ExceptionOnNotFound: boolean): boolean;
// reads code brackets (not comment brackets)
var OpenBracket, AntiOpenBracket: TCommonAtomFlag;
  Start: TAtomPosition;
  
  procedure RaiseBracketNotFound;
  begin
    if OpenBracket=cafRoundBracketOpen then
      SaveRaiseExceptionFmt(20170421194747,ctsBracketNotFound,['('])
    else
      SaveRaiseExceptionFmt(20170421194749,ctsBracketNotFound,['[']);
  end;
  
begin
  Result:=false;
  if (CurPos.Flag=cafRoundBracketClose) then begin
    OpenBracket:=cafRoundBracketOpen;
    AntiOpenBracket:=cafEdgedBracketOpen;
  end else if (CurPos.Flag=cafEdgedBracketClose) then begin
    OpenBracket:=cafEdgedBracketOpen;
    AntiOpenBracket:=cafRoundBracketOpen;
  end else begin
    if ExceptionOnNotFound then
      SaveRaiseBracketCloseExpectedButAtomFound(20170421194752);
    exit;
  end;
  Start:=CurPos;
  while CurPos.StartPos>1 do begin
    ReadPriorAtom;
    if (CurPos.Flag=OpenBracket) then exit(true);
    if (CurPos.StartPos<1)
    or (CurPos.Flag in [AntiOpenBracket,cafEND])
    or ((CurPos.Flag=cafWord)
        and UnexpectedKeyWordInBrackets.DoItCaseInsensitive(Src,
             CurPos.StartPos,CurPos.EndPos-CurPos.StartPos))
    then begin
      break;
    end;
    if CurPos.Flag in [cafRoundBracketClose,cafEdgedBracketClose] then begin
      if not ReadBackTilBracketOpen(ExceptionOnNotFound) then exit;
    end;
  end;
  CurPos:=Start;
  if ExceptionOnNotFound then
    RaiseBracketNotFound;
  Result:=false;
end;

procedure TCustomCodeTool.ReadTillCommentEnd;
begin
  CurPos.StartPos:=FindCommentEnd(Src,CurPos.StartPos,Scanner.NestedComments);
  LastAtoms.SetCurrent(CurPos);
end;

procedure TCustomCodeTool.BeginParsing(Range: TLinkScannerRange);
begin
  // scan
  FLastProgressPos:=0;
  Scanner.Scan(Range,CheckFilesOnDisk);
  FetchScannerSource;
  // init parsing values
  CurPos:=StartAtomPosition;
  LastAtoms.Clear;
  if CurPos.StartPos<CurPos.EndPos then
    LastAtoms.Add(CurPos);
  CurNode:=nil;
end;

procedure TCustomCodeTool.BeginParsingAndGetCleanPos(Range: TLinkScannerRange;
  CursorPos: TCodeXYPosition; out CleanCursorPos: integer);
var
  Dummy: integer;
begin
  if UpdateNeeded(Range) then
    BeginParsing(Range);
  // find the CursorPos in cleaned source
  Dummy:=CaretToCleanPos(CursorPos, CleanCursorPos);
  if (Dummy<>0) and (Dummy<>-1) then begin
    MoveCursorToCleanPos(1);
    RaiseException(20170421194754,ctsCursorPosOutsideOfCode,true);
  end;
end;

function TCustomCodeTool.IgnoreErrorAfterPositionIsInFrontOfLastErrMessage: boolean;
var
  IgnoreErrorAfterCleanPos: integer;
begin
  //DebugLn('TCustomCodeTool.IgnoreErrorAfterPositionIsInFrontOfLastErrMessage ',
  //  ' LastErrorCheckedForIgnored='+dbgs(LastErrorCheckedForIgnored),
  //  ' LastErrorBehindIgnorePosition='+dbgs(LastErrorBehindIgnorePosition));
  if LastErrorCheckedForIgnored then begin
    Result:=LastErrorBehindIgnorePosition;
  end else begin
    if (Scanner<>nil) then begin
      IgnoreErrorAfterCleanPos:=Scanner.IgnoreErrorAfterCleanedPos;
      //DebugLn(['  IgnoreErrorAfterCleanPos=',IgnoreErrorAfterCleanPos,' "',copy(Src,IgnoreErrorAfterCleanPos-6,6),'"',
      //  ' LastErrorCurPos.StartPos=',LastErrorCurPos.StartPos,' "',copy(Src,LastErrorCurPos.StartPos-6,6),'"',
      //  ' ']);
      if IgnoreErrorAfterCleanPos>0 then begin
        // ignore position in scanned code
        // -> check if last error is behind or equal ignore position
        if (not LastErrorValid)
        or (IgnoreErrorAfterCleanPos<=LastErrorCurPos.StartPos) then
          Result:=true
        else
          Result:=false;
      end else
        Result:=false;
    end else
      Result:=false;
    LastErrorBehindIgnorePosition:=Result;
    LastErrorCheckedForIgnored:=true;
  end;
  {$IFDEF ShowIgnoreErrorAfter}
  DebugLn('TCustomCodeTool.IgnoreErrAfterPositionIsInFrontOfLastErrMessage ',dbgs(Result));
  {$ENDIF}
end;

function TCustomCodeTool.IgnoreErrorAfterValid: boolean;
begin
  Result:=(Scanner<>nil) and (Scanner.IgnoreErrorAfterValid);
  {$IFDEF ShowIgnoreErrorAfter}
  DebugLn('TCustomCodeTool.IgnoreErrorAfterValid ',dbgs(Result));
  {$ENDIF}
end;

function TCustomCodeTool.IgnoreErrorAfterCleanedPos: integer;
begin
  if Scanner<>nil then
    Result:=Scanner.IgnoreErrorAfterCleanedPos
  else
    Result:=-1;
  {$IFDEF ShowIgnoreErrorAfter}
  DebugLn('TCustomCodeTool.IgnoreErrorAfterCleanedPos ',dbgs(Result));
  {$ENDIF}
end;

function TCustomCodeTool.CleanPosIsAfterIgnorePos(CleanPos: integer): boolean;
begin
  Result:=(Scanner<>nil) and Scanner.CleanPosIsAfterIgnorePos(CleanPos);
end;

function TCustomCodeTool.LastErrorIsInFrontOfCleanedPos(ACleanedPos: integer
  ): boolean;
begin
  if (Scanner<>nil) and Scanner.LastErrorIsInFrontOfCleanedPos(ACleanedPos)
  then
    Result:=true
  else if LastErrorValid
  and (LastErrorCurPos.StartPos<ACleanedPos) then
    Result:=true
  else
    Result:=false;
  {$IFDEF ShowIgnoreErrorAfter}
  DebugLn('TCustomCodeTool.LastErrorsInFrontOfCleanedPos ACleanedPos=',dbgs(ACleanedPos),' ',dbgs(Result));
  {$ENDIF}
end;

procedure TCustomCodeTool.RaiseLastErrorIfInFrontOfCleanedPos(
  ACleanedPos: integer);
begin
  {$IFDEF ShowIgnoreErrorAfter}
  DebugLn('TCustomCodeTool.RaiseLastErrorIfInFrontOfCleanedPos A ACleanedPos=',dbgs(ACleanedPos));
  {$ENDIF}
  if Scanner<>nil then Scanner.RaiseLastErrorIfInFrontOfCleanedPos(ACleanedPos);
  //DebugLn('TCustomCodeTool.RaiseLastErrorIfInFrontOfCleanedPos B ',LastErrorCurPos.EndPos);
  if LastErrorValid
  and (LastErrorCurPos.StartPos<ACleanedPos) then
    RaiseLastError;
  //DebugLn('TCustomCodeTool.RaiseLastErrorIfInFrontOfCleanedPos END ');
end;

function TCustomCodeTool.GetNodeParserError(Node: TCodeTreeNode
  ): TCodeTreeNodeParseError;
var
  AVLNode: TAVLTreeNode;
begin
  if (Node=nil) or (FNodeParseErrors=nil) then
    Result:=nil
  else begin
    AVLNode:=FNodeParseErrors.FindKey(Node,@CompareNodeWithCodeTreeNodeParserError);
    if AVLNode<>nil then
      Result:=TCodeTreeNodeParseError(AVLNode.Data)
    else
      Result:=nil;
  end;
end;

function TCustomCodeTool.SetNodeParserError(Node: TCodeTreeNode;
  const ErrorMsg: string; const ErrorCleanPos: integer;
  const ErrorNiceCleanPos: TCodeXYPosition): TCodeTreeNodeParseError;
var
  AVLNode: TAVLTreeNode;
begin
  //debugln(['TCustomCodeTool.SetNodeParserError ',Node.DescAsString,' Msg="',ErrorMsg,'" ',CleanPosToStr(ErrorCleanPos)]);
  if Node=nil then
    RaiseCatchableException('');
  if FNodeParseErrors=nil then
    FNodeParseErrors:=TAVLTree.Create(@CompareCodeTreeNodeParserError);
  AVLNode:=FNodeParseErrors.FindKey(Node,@CompareNodeWithCodeTreeNodeParserError);
  if AVLNode<>nil then begin
    Result:=TCodeTreeNodeParseError(AVLNode.Data)
  end else begin
    Result:=TCodeTreeNodeParseError.Create(Node);
    FNodeParseErrors.Add(Result);
  end;
  Node.SubDesc:=Node.SubDesc or ctnsHasParseError;
  Result.Msg:=ErrorMsg;
  Result.CleanPos:=ErrorCleanPos;
  Result.NicePos:=ErrorNiceCleanPos;
end;

procedure TCustomCodeTool.RaiseNodeParserError(Node: TCodeTreeNode);
var
  NodeError: TCodeTreeNodeParseError;
begin
  //debugln(['TCustomCodeTool.SetNodeParserError ',Node.DescAsString,' ',(ctnsHasParseError and Node.SubDesc)>0]);
  if (ctnsHasParseError and Node.SubDesc)=0 then exit;
  NodeError:=GetNodeParserError(Node);
  if CleanPosIsAfterIgnorePos(NodeError.CleanPos) then exit;
  //debugln(['TCustomCodeTool.RaiseNodeParserError ',Node.DescAsString,' Msg="',NodeError.Msg,'" ',CleanPosToStr(NodeError.CleanPos)]);
  MoveCursorToCleanPos(NodeError.CleanPos);
  ErrorNicePosition:=NodeError.NicePos;
  RaiseException(20170421194759,NodeError.Msg,false);
end;

procedure TCustomCodeTool.RaiseCursorOutsideCode(CursorPos: TCodeXYPosition);
var
  Msg: String;
  p: integer;
  CleanPos: integer;
  r: Integer;
  i: Integer;
  Link: TSourceLink;
  NearestScanPos: Integer;
  Node: TCodeTreeNode;
  LastPos: Integer;
begin
  ErrorPosition:=CursorPos;
  Msg:='';
  // check position in code buffer
  if CursorPos.Code=nil then
    Msg:='cursor position without code buffer'
  else if (CursorPos.Y<1) or (CursorPos.Y>CursorPos.Code.LineCount+1) then
    Msg:='invalid line number '+IntToStr(CursorPos.Y)
  else if (CursorPos.X<1) then
    Msg:='invalid column number '+IntToStr(CursorPos.Y)
  else begin
    CursorPos.Code.LineColToPosition(CursorPos.Y,CursorPos.X,p);
    if p<1 then
      Msg:='cursor position is outside of code'
    else begin
      // check position in scanner
      if Scanner=nil then
        Msg:='missing scanner (called wrong or no pascal)'
      else begin
        r:=Scanner.CursorToCleanPos(p,CursorPos.Code,CleanPos);
        if r<>0 then begin
          NearestScanPos:=0;
          for i:=0 to Scanner.LinkCount-1 do begin
            Link:=Scanner.Links[i];
            if Link.Code=nil then continue;
            if Link.Code<>Pointer(CursorPos.Code) then continue;
            if Link.SrcPos>p then continue;
            NearestScanPos:=Link.SrcPos+Scanner.LinkSize(i);
          end;
          if NearestScanPos=0 then
            Msg:='file was not reached by scanner'
          else begin
            if r=-1 then
              Msg:='cursor position was skipped by scanner'
            else
              Msg:='cursor position is beyond scan range';
            Msg:=Msg+' (last at '+CursorPos.Code.AbsoluteToLineColStr(NearestScanPos)+')'
          end;
        end else begin
          // check position in tree
          if (Tree=nil) or (Tree.Root=nil) then
            Msg:='No pascal found (maybe function was called before BuildTree)'
          else if CleanPos<Tree.Root.StartPos then begin
            // in front of parsed code
            Msg:='In front of code.';
            if Tree.Root.StartPos<=SrcLen then begin
              MoveCursorToCleanPos(Tree.Root.StartPos);
              ReadNextAtom;
              Msg:=Msg+' (pascal code starts with "'+GetAtom+'" at '
                      +CleanPosToStr(Tree.Root.StartPos)+')';
            end else begin
              Node:=Tree.Root;
              while Node.NextBrother<>nil do Node:=Node.NextBrother;
              if (Node.EndPos>0) and (p>Node.EndPos) then
                LastPos:=Node.EndPos
              else
                LastPos:=Node.StartPos;
              if p>LastPos then begin
                Msg:='behind code (last token at '+CleanPosToStr(LastPos)+')';
              end else begin
                Msg:='inconsistency: the position is in code, but caller thinks it is not.'
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  RaiseExceptionAtErrorPos(20170421194801,Msg);
end;

function TCustomCodeTool.StringIsKeyWord(const Word: string): boolean;
begin
  Result:=(Word<>'') and IsIdentStartChar[Word[1]]
                   and WordIsKeyWordFuncList.DoItUpperCase(Word,1,length(Word));
end;

procedure TCustomCodeTool.MoveCursorToNodeStart(ANode: TCodeTreeNode);
begin
  {$IFDEF CheckNodeTool}CheckNodeTool(ANode);{$ENDIF}
  MoveCursorToCleanPos(ANode.StartPos);
  CurNode:=ANode;
end;

procedure TCustomCodeTool.MoveCursorToCleanPos(ACleanPos: integer);
begin
  CurPos.StartPos:=ACleanPos;
  CurPos.EndPos:=ACleanPos;
  CurPos.Flag:=cafNone;
  LastAtoms.Clear;
end;

procedure TCustomCodeTool.MoveCursorToCleanPos(ACleanPos: PChar);

  procedure RaiseSrcEmpty;
  begin
    RaiseException(20170421194805,'[TCustomCodeTool.MoveCursorToCleanPos - PChar] Src empty',true);
  end;
  
  procedure RaiseNotInSrc;
  begin
    RaiseException(20170421194809,'[TCustomCodeTool.MoveCursorToCleanPos - PChar] '
      +'CleanPos not in Src',true);
  end;

var NewPos: integer;
begin
  if Src='' then
    RaiseSrcEmpty;
  NewPos:=PtrInt({%H-}PtrUInt(ACleanPos))-PtrInt({%H-}PtrUInt(@Src[1]))+1;
  if (NewPos<1) or (NewPos>SrcLen) then
    RaiseNotInSrc;
  MoveCursorToCleanPos(NewPos);
end;

procedure TCustomCodeTool.MoveCursorToAtomPos(const AnAtomPos: TAtomPosition);
var
  i: Integer;
begin
  if (AnAtomPos.StartPos<AnAtomPos.EndPos)
  and LastAtoms.IndexOf(AnAtomPos.StartPos,i) then begin
    //debugln(['TCustomCodeTool.MoveCursorToAtomPos jump to index ',i,' Atom="',GetAtom(AnAtomPos),'"(',AnAtomPos.StartPos,'-',AnAtomPos.EndPos,')']);
    //debugln(['TCustomCodeTool.MoveCursorToAtomPos ',LastAtoms.GetAtomAt(0).StartPos,'-',LastAtoms.GetAtomAt(0).EndPos]);
    LastAtoms.SetIndex(i);
  end
  else begin
    //debugln(['TCustomCodeTool.MoveCursorToAtomPos clearing LastAtoms Atom="',GetAtom(AnAtomPos),'"(',AnAtomPos.StartPos,'-',AnAtomPos.EndPos,')']);
    MoveCursorToCleanPos(AnAtomPos.StartPos);
  end;
  CurPos:=AnAtomPos;
end;

procedure TCustomCodeTool.MoveCursorToNearestAtom(ACleanPos: integer);
var
  ANode: TCodeTreeNode;
  BestPos: Integer;
begin
  ANode:=FindDeepestNodeAtPos(ACleanPos,true);
  if ANode=nil then
    RaiseException(20170421194812,'TCustomCodeTool.MoveCursorToNearestAtom internal error',true);
  MoveCursorToNodeStart(ANode);
  BestPos:=CurPos.StartPos;
  while (CurPos.StartPos<=ACleanPos) and (CurPos.StartPos<=SrcLen) do begin
    BestPos:=CurPos.StartPos;
    ReadNextAtom;
  end;
  MoveCursorToCleanPos(BestPos);
end;

procedure TCustomCodeTool.MoveCursorToLastNodeAtom(ANode: TCodeTreeNode);
var
  BestPos: LongInt;
begin
  MoveCursorToNodeStart(ANode);
  BestPos:=CurPos.StartPos;
  while (CurPos.EndPos<=ANode.EndPos) and (CurPos.StartPos<=SrcLen) do begin
    BestPos:=CurPos.StartPos;
    ReadNextAtom;
  end;
  MoveCursorToCleanPos(BestPos);
end;

function TCustomCodeTool.IsPCharInSrc(ACleanPos: PChar): boolean;
// Note: the ending #0 is a valid position
var
  p: PChar;
begin
  Result:=false;
  if Src='' then exit;
  p:=PChar(Src);
  if p>ACleanPos then exit;
  if ACleanPos>p+SrcLen then exit;
  Result:=true;
end;

procedure TCustomCodeTool.CreateChildNode;
var NewNode: TCodeTreeNode;
begin
  NewNode:=TCodeTreeNode.Create;
  Tree.AddNodeAsLastChild(CurNode,NewNode);
  CurNode:=NewNode;
  CurNode.StartPos:=CurPos.StartPos;
end;

procedure TCustomCodeTool.EndChildNode;
begin
  CurNode:=CurNode.Parent;
end;

procedure TCustomCodeTool.BuildDefaultKeyWordFunctions;
begin
  KeyWordFuncList.Clear;
  KeyWordFuncList.DefaultKeyWordFunction:=
    {$ifdef FPC}@{$endif}DefaultKeyWordFunc;
end;

function TCustomCodeTool.DoAtom: boolean;
begin
  if (CurPos.StartPos>SrcLen) or (CurPos.EndPos<=CurPos.StartPos) then
    Result:=false
  else if IsIdentStartChar[Src[CurPos.StartPos]] then
    Result:=KeyWordFuncList.DoItCaseInsensitive(Src,CurPos.StartPos,
            CurPos.EndPos-CurPos.StartPos)
  else
    Result:=true;
end;

procedure TCustomCodeTool.SetIgnoreErrorAfter(const AValue: TCodePosition);
begin
  if (IgnoreErrorAfter.Code=AValue.Code)
  and (IgnoreErrorAfter.P=AValue.P) then exit;
  FIgnoreErrorAfter:=AValue;
  LastErrorCheckedForIgnored:=false;
  {$IFDEF ShowIgnoreErrorAfter}
  DebugLn('TCustomCodeTool.SetIgnoreErrorAfter FIgnoreErrorAfter=',dbgs(FIgnoreErrorAfter));
  {$ENDIF}
  if Scanner<>nil then
    Scanner.SetIgnoreErrorAfter(IgnoreErrorAfter.P,IgnoreErrorAfter.Code);
end;

procedure TCustomCodeTool.IncreaseTreeChangeStep(NodesDeleting: boolean);
begin
  //DebugLn(['TCustomCodeTool.IncreaseTreeChangeStep ',DbgSName(Self),' NodesDeleting=',NodesDeleting]);
  if FTreeChangeStep=High(integer) then
    FTreeChangeStep:=Low(integer)
  else
    inc(FTreeChangeStep);
  if NodesDeleting then begin
    //DebugLn(['TCustomCodeTool.IncreaseTreeChangeStep NodesDeleting ',MainFilename,' ',Tree<>nil,' ',(Tree<>nil) and (Tree.Root<>nil)]);
    if FNodesDeletedChangeStep=High(integer) then
      FNodesDeletedChangeStep:=Low(integer)
    else
      inc(FNodesDeletedChangeStep);
  end;
  if FOnTreeChange<>nil then
    FOnTreeChange(Self,NodesDeleting);
end;

procedure TCustomCodeTool.RaiseExceptionInstance(TheException: ECodeToolError;
  ClearNicePos: boolean);
var
  CaretXY: TCodeXYPosition;
  CursorPos: integer;
begin
  ErrorPosition.Code:=nil;
  CursorPos:=CurPos.StartPos;
  if CursorPos<1 then LastAtoms.Clear;
  //DebugLn('TCustomCodeTool.RaiseExceptionInstance CursorPos=',dbgs(CursorPos),' "',copy(Src,CursorPos-6,6),'"');
  
  if ClearNicePos then begin
    ErrorNicePosition.Code:=nil;
    ErrorNicePosition.Y:=-1;
  end;

  // close all open nodes, so that FindDeepestNodeAtPos works in the code
  // already parsed
  CloseUnfinishedNodes;
  // convert cursor pos to caret pos, which is more human readable
  if (CursorPos>SrcLen) and (SrcLen>0) then CursorPos:=SrcLen;
  if (CleanPosToCaret(CursorPos,CaretXY))
  and (CaretXY.Code<>nil) then begin
    ErrorPosition:=CaretXY;
  end else if (Scanner<>nil) and (Scanner.MainCode<>nil) then begin
    ErrorPosition.Code:=TCodeBuffer(Scanner.MainCode);
    ErrorPosition.Y:=-1;
  end;
  // raise the exception
  if not RaiseUnhandableExceptions then
    raise TheException
  else begin
    TheException.Free;
    RaiseCatchableException(TheException.Message);
  end;
end;

procedure TCustomCodeTool.RaiseExceptionClass(id: int64;
  const AMessage: string; ExceptionClass: ECodeToolErrors; ClearNicePos: boolean
  );
begin
  RaiseExceptionInstance(ExceptionClass.Create(Self,id,AMessage),ClearNicePos);
end;

function TCustomCodeTool.DefaultKeyWordFunc: boolean;
begin
  Result:=true;
end;

procedure TCustomCodeTool.ConsistencyCheck;
begin
  Tree.ConsistencyCheck;
end;

procedure TCustomCodeTool.WriteDebugTreeReport;

  procedure WriteSrcSubString(A,Len: integer);
  var i: integer;
  begin
    DbgOut('"');
    for i:=A to A+Len-1 do begin
      if (i>0) and (i<SrcLen) and (ord(Src[i])>31) then
        DbgOut(Src[i]);
    end;
    DbgOut('"');
  end;

  procedure WriteSubTree(RootNode: TCodeTreeNode; Indent: string);
  begin
    while RootNode<>nil do begin
      DbgOut(Indent);
      with RootNode do begin
        DbgOut(NodeDescToStr(Desc)+'('+NodeSubDescToStr(Desc,SubDesc)+')  ');
        DbgOut(' Start='+DbgS(StartPos),' ');
        WriteSrcSubString(StartPos,5);
        DbgOut(' End='+DbgS(EndPos)+' ');
        WriteSrcSubString(EndPos-5,5);
        {$ifdef fpc}
        DbgOut(' Self=',DbgS(RootNode));
        DbgOut(' P=',DbgS(Parent));
        DbgOut(' NB=',DbgS(NextBrother));
        //write(' PB=',DbgS(PriorBrother));
        //write(' FC=',DbgS(FirstChild));
        //write(' LC=',DbgS(LastChild));
        {$endif}
      end;
      DebugLn('');
      WriteSubTree(RootNode.FirstChild,Indent+'  ');
      RootNode:=RootNode.NextBrother;
    end;
  end;

begin
  DebugLn('[TCustomCodeTool.WriteDebugTreeReport]');
  WriteSubTree(Tree.Root,'  ');
  ConsistencyCheck;
end;

procedure TCustomCodeTool.CalcMemSize(Stats: TCTMemStats);
begin
  Stats.Add(ClassName,InstanceSize);
  Stats.Add('TCustomCodeTool',MemSizeString(LastErrorMessage));
  if FScanner<>nil then
    FScanner.CalcMemSize(Stats);
  if (FScanner=nil) or (Pointer(FScanner.CleanedSrc)<>Pointer(Src)) then
    Stats.Add('TCustomCodeTool.Src',
      MemSizeString(Src));
  if KeyWordFuncList<>nil then
    Stats.Add('TCustomCodeTool.KeyWordFuncList',
      KeyWordFuncList.CalcMemSize);
  // Note: WordIsKeyWordFuncList is using the global list
  if Tree<>nil then
    Stats.Add('TCustomCodeTool.Tree',
      Tree.NodeCount*TCodeTreeNode.InstanceSize);
  if LastAtoms<>nil then
    Stats.Add('TCustomCodeTool.LastAtoms',
      LastAtoms.CalcMemSize);
end;

procedure TCustomCodeTool.CheckNodeTool(Node: TCodeTreeNode);

  procedure RaiseForeignNode;
  begin
    RaiseCatchableException('TCustomCodeTool.CheckNodeTool '+DbgSName(Self)+' '+Node.DescAsString);
  end;

begin
  if Node=nil then exit;
  while Node.Parent<>nil do Node:=Node.Parent;
  while Node.PriorBrother<>nil do Node:=Node.PriorBrother;
  if (Tree=nil) or (Tree.Root<>Node) then
    RaiseForeignNode;
end;

function TCustomCodeTool.FindDeepestNodeAtPos(P: integer;
  ExceptionOnNotFound: boolean): TCodeTreeNode;
begin
  Result:=FindDeepestNodeAtPos(Tree.Root,P,ExceptionOnNotFound);
end;

function TCustomCodeTool.FindDeepestNodeAtPos(StartNode: TCodeTreeNode;
  P: integer; ExceptionOnNotFound: boolean): TCodeTreeNode;
  
  procedure RaiseNoNodeFoundAtCursor;
  var
    Msg: String;
    Node: TCodeTreeNode;
    LastPos: Integer;
  begin
    //DebugLn('RaiseNoNodeFoundAtCursor ',MainFilename);
    MoveCursorToCleanPos(P);
    // check if p is in parsed code
    if (Tree=nil) or (Tree.Root=nil) then begin
      debugln(['TCustomCodeTool.FindDeepestNodeAtPos there are no nodes, maybe you forgot to parse?']);
      CTDumpStack;
      RaiseException(20170421194833,'no pascal code or not yet parsed');
    end;
    if p<Tree.Root.StartPos then begin
      // in front of parsed code
      Msg:='In front of code.';
      if Tree.Root.StartPos<=SrcLen then begin
        MoveCursorToCleanPos(Tree.Root.StartPos);
        ReadNextAtom;
        Msg:=Msg+' (The pascal code starts with "'+GetAtom+'" at '
                +CleanPosToStr(Tree.Root.StartPos)+')';
      end;
      MoveCursorToCleanPos(P);
      RaiseException(20170421194836,Msg);
    end;
    // behind parsed code
    Node:=Tree.Root;
    while Node.NextBrother<>nil do Node:=Node.NextBrother;
    if (Node.EndPos>0) and (p>Node.EndPos) then
      LastPos:=Node.EndPos
    else
      LastPos:=Node.StartPos;
    if p>LastPos then begin
      Msg:='Behind code (last token at '+CleanPosToStr(LastPos)+')';
      {$IFDEF VerboseNoNodeAtCursor}
      debugln(['RaiseNoNodeFoundAtCursor CleanSrcEnd=',CleanPosToStr(SrcLen),' ...',dbgstr(RightStr(Src,50))]);
      debugln(['   Scanner.ScannedRange=',dbgs(Scanner.ScannedRange)]);
      debugln(['   Scanner.CleanedSrc=',dbgstr(RightStr(Scanner.CleanedSrc,50))]);
      debugln(['   Node.StartPos=',CleanPosToStr(Node.StartPos),' EndPos=',CleanPosToStr(Node.EndPos)]);
      Node:=Node.GetLastNode;
      debugln(['   LastNode=',Node.DescAsString,' StartPos=',CleanPosToStr(Node.StartPos),' EndPos=',CleanPosToStr(Node.EndPos)]);
      {$ENDIF}
      RaiseException(20170421194838,Msg);
    end;

    // p is in parsed code, the StartNode is wrong
    CTDumpStack;
    if (StartNode<>nil) then
      RaiseException(20170421194842,'Invalid search. The search for pascal started at '
                     +CleanPosToStr(StartNode.StartPos)+'. Invalid search')
    else
      RaiseException(20170421194846,'Inconsistency error in TCustomCodeTool.FindDeepestNodeAtPos');
  end;
  
var
  ChildNode: TCodeTreeNode;
  Brother: TCodeTreeNode;
  Node: TCodeTreeNode;
begin
  {$IFDEF CheckNodeTool}CheckNodeTool(StartNode);{$ENDIF}
  Result:=nil;
  Node:=StartNode;
  while Node<>nil do begin
    //DebugLn('SearchInNode ',NodeDescriptionAsString(ANode.Desc),
    //',',ANode.StartPos,',',ANode.EndPos,', p=',p,
    //' "',copy(Src,ANode.StartPos,4),'" - "',copy(Src,ANode.EndPos-5,4),'"');
    if (Node.StartPos<=P)
    and ((Node.EndPos>P) or (Node.EndPos<1)) then begin
      // StartNode contains P
      Result:=Node;
      // -> search for a child that contains P
      Brother:=Node;
      while (Brother<>nil)
      and (Brother.StartPos<=P) do begin
        // brother also contains P
        if Brother.FirstChild<>nil then begin
          ChildNode:=FindDeepestNodeAtPos(Brother.FirstChild,P,false);
          if ChildNode<>nil then begin
            Result:=ChildNode;
            exit;
          end else
            Result:=Brother;
        end;
        Brother:=Brother.NextBrother;
      end;
      break;
    end else begin
      // search in next node
      Node:=Node.NextBrother;
    end;
  end;
  if (Result=nil) and (Tree.Root<>nil) then begin
    Node:=Tree.Root;
    while Node.NextBrother<>nil do
      Node:=Node.NextBrother;
    if (Node<>nil) and (Node.EndPos=p) then begin
      // cursor at end of source
      Result:=Node;
      while (Result.LastChild<>nil) and (Result.LastChild.EndPos=p) do
        Result:=Result.LastChild;
      exit;
    end;
  end;

  if (Result=nil) and ExceptionOnNotFound then begin
    RaiseNoNodeFoundAtCursor;
  end;
end;

function TCustomCodeTool.CaretToCleanPos(const Caret: TCodeXYPosition;
  out CleanPos: integer): integer;
begin
  if Caret.Code=nil then begin
    CleanPos:=0;
    exit(-2);
  end;
  //DebugLn(['TCustomCodeTool.CaretToCleanPos A ',Caret.Code.Filename,' ',Caret.Y,',',Caret.X,' SourceLength=',Caret.Code.SourceLength]);
  Caret.Code.LineColToPosition(Caret.Y,Caret.X,CleanPos);
  //DebugLn(['TCustomCodeTool.CaretToCleanPos B Clean=',CleanPos,',Y=',Caret.Y,',X=',Caret.X,' ',dbgstr(copy(Caret.Code.Source,CleanPos-20,20)),'|',dbgstr(copy(Caret.Code.Source,CleanPos,20))]);
  if (CleanPos>=1) then
    Result:=Scanner.CursorToCleanPos(CleanPos,Caret.Code,CleanPos)
  else
    Result:=-2; // x,y beyond source
  //DebugLn(['TCustomCodeTool.CaretToCleanPos C CleanPos=',CleanPos,' Result=',Result]);
end;

function TCustomCodeTool.CodePosToCleanPos(const CodePos: TCodePosition; out
  CleanPos: integer): integer;
begin
  Result:=Scanner.CursorToCleanPos(CodePos.P,CodePos.Code,CleanPos);
end;

function TCustomCodeTool.CleanPosToCodePos(CleanPos: integer;
  out CodePos: TCodePosition): boolean;
var
  ACode: pointer;
begin
  Result:=Scanner.CleanedPosToCursor(CleanPos,CodePos.p,ACode);
  CodePos.Code:=TCodeBuffer(ACode);
end;

function TCustomCodeTool.CleanPosToCaret(CleanPos: integer;
  out Caret:TCodeXYPosition): boolean; // true=ok, false=invalid CleanPos
var p: integer;
  Code: Pointer;
begin
  Caret:=CleanCodeXYPosition;
  Result:=Scanner.CleanedPosToCursor(CleanPos,p,Code);
  if Result then begin
    Caret.Code:=TCodeBuffer(Code);
    if Caret.Code.IsDeleted then begin
      debugln(['TCustomCodeTool.CleanPosToCaret IsDeleted: ',Caret.Code.Filename,' SourceLength=',Caret.Code.SourceLength]);
      exit(false);
    end;
    TCodeBuffer(Code).AbsoluteToLineCol(p,Caret.Y,Caret.X);
    Result:=(Caret.Y>=0);
  end;
end;

function TCustomCodeTool.CleanPosToCaretAndTopLine(CleanPos: integer;
  out Caret:TCodeXYPosition; out NewTopLine: integer): boolean;
// true=ok, false=invalid CleanPos
begin
  Caret:=CleanCodeXYPosition;
  NewTopLine:=0;
  Result:=CleanPosToCaret(CleanPos,Caret);
  if Result then begin
    if JumpSingleLinePos>0 then begin
      NewTopLine:=Caret.Y-(VisibleEditorLines*JumpSingleLinePos div 100);
      if NewTopLine<1 then NewTopLine:=1;
    end else
      NewTopLine:=Caret.Y;
  end;
end;

function TCustomCodeTool.CleanPosToStr(CleanPos: integer;
  WithFilename: boolean): string;
var
  CodePos: TCodeXYPosition;
begin
  if CleanPosToCaret(CleanPos,CodePos) then begin
    Result:=CodeXYToStr(CodePos,WithFilename);
  end else
    Result:='outside scan range, pos='+IntToStr(CleanPos)+'('+dbgstr(copy(Src,CleanPos-5,5)+'|'+copy(Src,CleanPos,5))+')';
end;

function TCustomCodeTool.CodeXYToStr(const CodePos: TCodeXYPosition;
  WithFilename: boolean): string;
begin
  Result:='';
  if WithFilename then
    Result:=ExtractRelativepath(ExtractFilePath(MainFilename),CodePos.Code.Filename)+',';
  Result:=Result+'line '+IntToStr(CodePos.Y)+', column '+IntToStr(CodePos.X);
end;

function TCustomCodeTool.CleanPosToRelativeStr(CleanPos: integer;
  const BaseFilename: string): string;
var
  CodePos: TCodeXYPosition;
begin
  if not CleanPosToCaret(CleanPos,CodePos) then
    Result:='(?)'
  else begin
    if (BaseFilename='') or (not FilenameIsAbsolute(BaseFilename)) then
      Result:=CodePos.Code.Filename
    else if CompareFilenames(CodePos.Code.Filename,BaseFilename)<>0 then
      Result:=CreateRelativePath(CodePos.Code.Filename,
                                 ExtractFilePath(BaseFilename))
    else
      Result:='';
    Result:=Result+'('+IntToStr(CodePos.Y)+','+IntToStr(CodePos.X)+')';
  end;
end;

procedure TCustomCodeTool.GetCleanPosInfo(CodePosInFront, CleanPos: integer;
  ResolveComments: boolean; out SameArea: TAtomPosition);
{ Use this function to safely find out if a position is in a comment,
  a string constant, an atom or is in space

  CodePosInFront: a position in code, where parsing starts
                  If -1 it uses FindDeepestNodeAtPos
  CleanPos: target position
  ResolveComments: if CleanPos is in a comment, parse again in the comment (not recursive)
  SameArea: area around CleanPos, either an atom, comment, directive or space
            if CleanPos<CodePosInFront then CleanAtomPosition
            if CleanPos>SrcLen then SameArea.StartPos>SrcLen
}
var
  ANode: TCodeTreeNode;
begin
  SameArea:=CleanAtomPosition;
  if CodePosInFront<1 then begin
    ANode:=FindDeepestNodeAtPos(CleanPos,True);
    CodePosInFront:=ANode.StartPos;
  end;
  if CodePosInFront>CleanPos then
    exit;
  MoveCursorToCleanPos(CodePosInFront);
  repeat
    ReadNextAtom;
    //DebugLn(['TCustomCodeTool.GetCleanPosInfo A Atom=',GetAtom,' CleanPos=',CleanPos,' CurPos.StartPos=',CurPos.StartPos]);
    if (CleanPos>=CurPos.StartPos) and (CleanPos<CurPos.EndPos) then begin
      // clean pos on token
      SameArea:=CurPos;
      exit;
    end;
    if CleanPos<CurPos.StartPos then begin
      // clean pos between tokens
      SameArea.Flag:=cafNone;
      // get range of space behind last atom
      if LastAtoms.HasPrior then begin
        SameArea.StartPos:=LastAtoms.GetPriorAtom.EndPos;
      end else begin
        SameArea.StartPos:=CodePosInFront;
      end;
      SameArea.EndPos:=SameArea.StartPos;
      repeat
        //DebugLn(['TCustomCodeTool.GetCleanPosInfo B CleanPos=',CleanPos,' SameArea.StartPos=',SameArea.StartPos,' SameArea.EndPos=',SameArea.EndPos]);
        while (SameArea.EndPos<=SrcLen)
        and (IsSpaceChar[Src[SameArea.EndPos]]) do
          inc(SameArea.EndPos);
        if (SameArea.EndPos>CleanPos) or (SameArea.EndPos>SrcLen) then begin
          // cursor is in normal space (i.e. not comment)
          exit;
        end;
        // still between the two tokens, but end of space
        // -> here starts a comment
        SameArea.StartPos:=SameArea.EndPos;
        MoveCursorToCleanPos(SameArea.StartPos);
        //debugln(['TCustomCodeTool.GetCleanPosInfo C "',dbgstr(Src,SameArea.StartPos,Max(3,FindCommentEnd(Src,SameArea.StartPos,Scanner.NestedComments)-SameArea.StartPos)),'"']);
        ReadTillCommentEnd;
        SameArea.EndPos:=CurPos.StartPos;
        //debugln(['TCustomCodeTool.GetCleanPosInfo D "',dbgstr(Src,SameArea.StartPos,SameArea.EndPos-SameArea.StartPos),'"']);
        if (SameArea.StartPos=SameArea.EndPos) then
          // inconsistency: some non space and non comment between two tokens
          RaiseException(20170421194850,'TCustomCodeTool.GetCleanPosInfo Internal Error A');
        if CleanPos<SameArea.EndPos then begin
          // cursor is in comment
          if ResolveComments then begin
            // take comment as normal code and search again
            CodePosInFront:=SameArea.StartPos;
            case Src[CodePosInFront] of
            '{':
              begin
                inc(CodePosInFront);
                if (CodePosInFront<=SrcLen) and (Src[CodePosInFront+1]=#3) then
                  inc(CodePosInFront);
              end;
            '(','/': inc(CodePosInFront,2);
            else
              RaiseException(20170421194855,'TCustomCodeTool.GetCleanPosInfo Internal Error B '
                +dbgstr(Src[CodePosInFront])+' at '+CleanPosToStr(CodePosInFront,true));
            end;
            if CodePosInFront>CleanPos then
              // CleanPos at start of comment => return comment
              exit;
            GetCleanPosInfo(CodePosInFront,CleanPos,true,SameArea);
          end;
          exit;
        end;
        SameArea.StartPos:=SameArea.EndPos;
      until false;
    end;
  until (CurPos.EndPos>CleanPos) or (CurPos.StartPos>SrcLen);
  SameArea:=CurPos;
end;

procedure TCustomCodeTool.GetLineInfo(ACleanPos: integer;
  out ALineStart, ALineEnd, AFirstAtomStart, ALastAtomEnd: integer);
begin
  if ACleanPos>=1 then begin
    if ACleanPos<=SrcLen then begin
      // search line start
      ALineStart:=ACleanPos;
      while (ALineStart>1) and (not (Src[ALineStart-1] in [#10,#13])) do
        dec(ALineStart);
      // search line end
      ALineEnd:=ACleanPos;
      while (ALineEnd<=SrcLen) and (not (Src[ALineEnd] in [#10,#13])) do
        inc(ALineEnd);
      // search first atom in line
      MoveCursorToCleanPos(ALineStart);
      ReadNextAtom;
      AFirstAtomStart:=CurPos.StartPos;
      // search last atom in line
      repeat
        ALastAtomEnd:=CurPos.EndPos;
        ReadNextAtom;
      until (CurPos.EndPos>ALineEnd) or (CurPos.StartPos>SrcLen);
    end else begin
      ALineStart:=Srclen+1;
      ALineEnd:=Srclen+1;
      AFirstAtomStart:=Srclen+1;
      ALastAtomEnd:=Srclen+1;
    end;
  end else begin
    ALineStart:=1;
    ALineEnd:=1;
    AFirstAtomStart:=1;
    ALastAtomEnd:=1;
  end;
end;

function TCustomCodeTool.FindLineEndOrCodeAfterPosition(StartPos: integer;
  SkipEmptyLines: boolean; IncludeLineEnd: boolean): integer;
{ Searches a nice position in the cleaned source after StartPos.
  It will skip any space and comments (not directives) till next
  line end or compiler directive or code or include file end.
}
var
  LinkIndex, LinkEnd: integer;
begin
  Result:=StartPos;
  LinkIndex:=Scanner.LinkIndexAtCleanPos(StartPos);
  if LinkIndex>=0 then begin
    LinkEnd:=Scanner.LinkCleanedEndPos(LinkIndex);
    if LinkEnd>StartPos then
      Result:=BasicCodeTools.FindLineEndOrCodeAfterPosition(Src,
                    StartPos,LinkEnd-1,Scanner.NestedComments,true,SkipEmptyLines,
                    IncludeLineEnd);
  end;
end;

function TCustomCodeTool.FindLineEndOrCodeInFrontOfPosition(StartPos: integer;
  StopAtDirectives: boolean; SkipEmptyLines: boolean): integer;
{ Searches a nice position in the cleaned source in front of StartPos.
  It will skip any space or comments (not directives) till next
  line end or compiler directive or code or include file end.
}
var
  LinkIndex, LinkStart: integer;
begin
  Result:=StartPos;
  LinkIndex:=Scanner.LinkIndexAtCleanPos(StartPos);
  if LinkIndex>=0 then begin
    LinkStart:=Scanner.Links[LinkIndex].CleanedPos;
    Result:=BasicCodeTools.FindLineEndOrCodeInFrontOfPosition(Src,
              StartPos,LinkStart,Scanner.NestedComments,StopAtDirectives,false,
              SkipEmptyLines);
  end;
end;

function TCustomCodeTool.SkipResourceDirective(StartPos: integer): integer;
var
  LinkIndex, LinkEnd: integer;
begin
  Result:=StartPos;
  LinkIndex:=Scanner.LinkIndexAtCleanPos(StartPos);
  if LinkIndex>=0 then begin
    LinkEnd:=Scanner.LinkCleanedEndPos(LinkIndex);
    Result:=BasicCodeTools.SkipResourceDirective(Src,StartPos,LinkEnd,Scanner.NestedComments);
  end;
end;

procedure TCustomCodeTool.ClearIgnoreErrorAfter;
begin
  IgnoreErrorAfter:=CodePosition(0,nil);
end;

function TCustomCodeTool.UpdateNeeded(Range: TLinkScannerRange): boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCustomCodeTool.UpdateNeeded A Range=',dbgs(Range),' ',Scanner.MainFilename);
  {$ENDIF}
  if Range=lsrNone then exit(false);
  if (not LastErrorValid) and (ord(FRangeValidTill)<ord(Range)) then begin
    {$IFDEF VerboseUpdateNeeded}
    DebugLn(['TCustomCodeTool.UpdateNeeded because range increased from ',dbgs(FRangeValidTill),' to ',dbgs(Range),' ',MainFilename]);
    {$ENDIF}
    exit(true);
  end;
  if (FLastScannerChangeStep<>Scanner.ChangeStep) then begin
    {$IFDEF VerboseUpdateNeeded}
    DebugLn(['TCustomCodeTool.UpdateNeeded because FLastScannerChangeStep<>Scanner.ChangeStep ',MainFilename]);
    {$ENDIF}
    FRangeValidTill:=lsrNone;
    Result:=true;
  end else begin
    Result:=Scanner.UpdateNeeded(Range, CheckFilesOnDisk);
    if Result then begin
      {$IFDEF VerboseUpdateNeeded}
      DebugLn(['TCustomCodeTool.UpdateNeeded because Scanner.UpdateNeeded ',MainFilename]);
      {$ENDIF}
      // decrease valid range
      FRangeValidTill:=Pred(Range);
      ClearLastError;
    end;
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCustomCodeTool.UpdateNeeded END  Result=',dbgs(Result));
  {$ENDIF}
end;

function TCustomCodeTool.UpdateNeeded(OnlyInterfaceNeeded: boolean): boolean;
begin
  if OnlyInterfaceNeeded then
    Result:=UpdateNeeded(lsrImplementationStart)
  else
    Result:=UpdateNeeded(lsrEnd);
end;

function TCustomCodeTool.CompareSrcIdentifiers(Identifier1, Identifier2: PChar
  ): boolean;
begin
  Result:=false;
  if (Identifier1=nil) or (Identifier2=nil) then exit;
  while IsIdentChar[Identifier1^] do begin
    if (UpChars[Identifier1^]=UpChars[Identifier2^]) then begin
      inc(Identifier1);
      inc(Identifier2);
    end else
      exit(false);
  end;
  Result:=not IsIdentChar[Identifier2^];
end;

function TCustomCodeTool.CompareSrcIdentifiers(CleanStartPos: integer;
  AnIdentifier: PChar): boolean;
begin
  Result:=false;
  if (AnIdentifier=nil) or (CleanStartPos<1) or (CleanStartPos>SrcLen) then
    exit;
  while IsIdentChar[AnIdentifier^] do begin
    if (UpChars[AnIdentifier^]=UpChars[Src[CleanStartPos]]) then begin
      inc(AnIdentifier);
      inc(CleanStartPos);
      if CleanStartPos>SrcLen then begin
        Result:=not IsIdentChar[AnIdentifier^];
        exit;
      end;
    end else
      exit(false);
  end;
  Result:=not IsIdentChar[Src[CleanStartPos]];
end;

function TCustomCodeTool.CompareSrcIdentifiersMethod(Identifier1,
  Identifier2: Pointer): integer;
begin
  Result:=CompareIdentifiers(Identifier1,Identifier2);
end;

function TCustomCodeTool.ExtractIdentifier(CleanStartPos: integer): string;
var len: integer;
begin
  if (CleanStartPos>=1) then begin
    len:=0;
    while (CleanStartPos<=SrcLen)
    and (IsIdentChar[Src[CleanStartPos+len]]) do
      inc(len);
    SetLength(Result,len);
    if len>0 then
      Move(Src[CleanStartPos],Result[1],len);
  end else
    Result:='';
end;

function TCustomCodeTool.ExtractDottedIdentifier(CleanStartPos: integer): string;
begin
  Result:=GetDottedIdentifier(@Src[CleanStartPos]);
end;

procedure TCustomCodeTool.DoDeleteNodes(StartNode: TCodeTreeNode);
// delete Node and all following nodes
var
  AVLNode: TAVLTreeNode;
  NextAVLNode: TAVLTreeNode;
  Node: TCodeTreeNode;
begin
  if StartNode<>nil then begin
    //debugln(['TCustomCodeTool.DoDeleteNodes Node=',StartNode.DescAsString,' ',MainFilename]);
    //DebugLn(['TCustomCodeTool.DoDeleteNodes ',MainFilename]);
    // first notify, so that references could be deleted clean
    IncreaseTreeChangeStep(true);
    if (CurNode<>nil) and (StartNode.StartPos<=CurNode.StartPos) then
      CurNode:=nil;

    // free errors and nodes
    if StartNode=Tree.Root then begin
      DisposeAVLTree(FNodeParseErrors);
      Tree.Clear;
    end else begin
      if (FNodeParseErrors<>nil) then begin
        AVLNode:=FNodeParseErrors.FindLowest;
        while AVLNode<>nil do begin
          NextAVLNode:=FNodeParseErrors.FindSuccessor(AVLNode);
          if TCodeTreeNodeParseError(AVLNode.Data).Node.StartPos>=StartNode.StartPos
          then
            FNodeParseErrors.FreeAndDelete(AVLNode);
          AVLNode:=NextAVLNode;
        end;
      end;
      Node:=StartNode;
      repeat
        while Node.NextBrother<>nil do
          Tree.DeleteNode(Node.NextBrother);
        Node:=Node.Parent;
      until Node=nil;
      Tree.DeleteNode(StartNode);
    end;
  end;
end;

procedure TCustomCodeTool.CloseUnfinishedNodes;
var
  p: LongInt;
begin
  if CurNode=nil then exit;
  // close all unfinished nodes
  p:=CurPos.StartPos;
  if p>SrcLen then p:=SrcLen+1;
  while CurNode<>nil do begin
    //debugln(['TCustomCodeTool.CloseUnfinishedNodes BEFORE ',CurNode.DescAsString,' CurNode.EndPos=',CurNode.EndPos]);
    if CurNode.EndPos<1 then begin
      if CurNode.StartPos>p then p:=CurNode.StartPos;
      CurNode.EndPos:=p;
    end else if p<CurNode.EndPos then begin
      p:=CurNode.EndPos;
    end else if (CurNode.LastChild<>nil) and (CurNode.EndPos<CurNode.LastChild.EndPos) then
      CurNode.EndPos:=CurNode.LastChild.EndPos;
    //debugln(['TCustomCodeTool.CloseUnfinishedNodes AFTER ',CurNode.DescAsString,' CurNode.EndPos=',CleanPosToStr(CurNode.EndPos)]);
    CurNode:=CurNode.Parent;
  end;
end;

procedure TCustomCodeTool.SaveRaiseIdentExpectedButAtomFound(id: int64);
begin
  SaveRaiseExceptionFmt(id,ctsIdentExpectedButAtomFound,[GetAtom]);
end;

procedure TCustomCodeTool.SaveRaiseBracketOpenExpectedButAtomFound(id: int64);
begin
  SaveRaiseExceptionFmt(id,ctsBracketOpenExpectedButAtomFound,[GetAtom]);
end;

procedure TCustomCodeTool.SaveRaiseBracketCloseExpectedButAtomFound(id: int64);
begin
  if CurPos.StartPos<SrcLen then
    SaveRaiseExceptionFmt(id,ctsBracketCloseExpectedButAtomFound,[GetAtom])
  else
    SaveRaiseExceptionFmt(id,ctsBracketNotFound,[])
end;

procedure TCustomCodeTool.ActivateGlobalWriteLock;
begin
  if Assigned(OnSetGlobalWriteLock) then OnSetGlobalWriteLock(true);
end;

procedure TCustomCodeTool.DeactivateGlobalWriteLock;
begin
  if Assigned(OnSetGlobalWriteLock) then OnSetGlobalWriteLock(false);
end;

function TCustomCodeTool.MainFilename: string;
begin
  if (Scanner<>nil) and (Scanner.MainCode<>nil) then
    Result:=TCodeBuffer(Scanner.MainCode).Filename
  else
    Result:=ctsUnknownMainFilename;
end;

{ ECodeToolError }

constructor ECodeToolError.Create(ASender: TCustomCodeTool; TheId: int64;
  const AMessage: string);
begin
  inherited Create(AMessage);
  Sender:=ASender;
  Id:=TheId;
end;

{ ECodeToolFileNotFound }

constructor ECodeToolFileNotFound.Create(ASender: TCustomCodeTool;
  TheId: int64; const AMessage, AFilename: string);
begin
  inherited Create(ASender,TheId,AMessage);
  Filename:=AFilename;
end;

{ TCodeTreeNodeParseError }

constructor TCodeTreeNodeParseError.Create(ANode: TCodeTreeNode);
begin
  Node:=ANode;
end;

initialization
  RaiseUnhandableExceptions:=false;

end.
