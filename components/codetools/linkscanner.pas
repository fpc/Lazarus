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
    TLinkScanner scans a source file, reacts to compiler directives, replaces
    macros and reads include files. It builds one source and a link list. The
    resulting source is called the cleaned source. A link points from a position
    of the cleaned source to its position in the real source.
    The link list makes it possible to change scanned code in the source files.

  ToDo:
    - macros
}
unit LinkScanner;

{$ifdef FPC} {$mode objfpc} {$endif}{$H+}
{$ifdef UseInline}{$inline on}{$endif}

{$I codetools.inc}

{ $DEFINE ShowIgnoreErrorAfter}

// debugging
{ $DEFINE ShowUpdateCleanedSrc}
{ $DEFINE VerboseIncludeSearch}

interface

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeToolsStrConsts, CodeToolMemManager, FileProcs,
  AVL_Tree, ExprEval, SourceLog, KeywordFuncLists, BasicCodeTools;

const
  PascalCompilerDefine = ExternalMacroStart+'Compiler';
  NestedCompilerDefine = ExternalMacroStart+'NestedComments';

  MissingIncludeFileCode = Pointer(1);

type
  TLinkScanner = class;

//----------------------------------------------------------------------------
  TOnGetSource = function(Sender: TObject; Code: Pointer): TSourceLog
                 of object;
  TOnLoadSource = function(Sender: TObject; const AFilename: string;
                       OnlyIfExists: boolean): pointer of object;
  TOnGetSourceStatus = procedure(Sender: TObject; Code: Pointer;
                 var ReadOnly: boolean) of object;
  TOnDeleteSource = procedure(Sender: TObject; Code: Pointer; Pos, Len: integer)
                    of object;
  TOnGetFileName = function(Sender: TObject; Code: Pointer): string of object;
  TOnCheckFileOnDisk = function(Code: Pointer): boolean of object;
  TOnGetInitValues = function(Code: Pointer;
                       out ChangeStep: integer): TExpressionEvaluator of object;
  TOnIncludeCode = procedure(ParentCode, IncludeCode: Pointer) of object;
  TOnSetWriteLock = procedure(Lock: boolean) of object;
  TOnGetWriteLockInfo = procedure(out WriteLockIsSet: boolean;
    out WriteLockStep: integer) of object;

  { TSourceLink is used to map between the codefiles and the cleaned source }
  PSourceLink = ^TSourceLink;
  TSourceLink = record
    CleanedPos: integer;
    SrcPos: integer;
    Code: Pointer;
    Next: PSourceLink;
  end;

  { TSourceChangeStep is used save the ChangeStep of every used file }
  PSourceChangeStep = ^TSourceChangeStep;
  TSourceChangeStep = record
    Code: Pointer;
    ChangeStep: integer;
    Next: PSourceChangeStep;
  end;
  
  TLinkScannerRange = (
    lsrNone, // undefined
    lsrInit, // init, but do not scan any code
    lsrInterface, // scan only interface
    lsrEnd // scan till 'end.'
    );

  TCommentStyle = (CommentNone, CommentTP, CommentOldTP, CommentDelphi);

  TCompilerMode = (cmFPC, cmDELPHI, cmGPC, cmTP, cmOBJFPC, cmMacPas);
  TPascalCompiler = (pcFPC, pcDelphi);

  { TMissingIncludeFile is a missing include file together with all
    params involved in the search }
  TMissingIncludeFile = class
  public
    IncludePath: string;
    Filename: string;
    constructor Create(const AFilename, AIncludePath: string);
  end;
  
  { TMissingIncludeFiles is a list of TMissingIncludeFile }
  TMissingIncludeFiles = class(TList)
  private
    function GetIncFile(Index: Integer): TMissingIncludeFile;
    procedure SetIncFile(Index: Integer; const AValue: TMissingIncludeFile);
  public
    procedure Clear; override;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TMissingIncludeFile
      read GetIncFile write SetIncFile; default;
  end;
  
  { LinkScanner Token Types }
  TLSTokenType = (
    lsttNone, lsttSrcEnd, lsttIdentifier, lsttEqual, lsttPoint, lsttEnd,
    lsttEndOfInterface);

  { Error handling }
  ELinkScannerError = class(Exception)
    Sender: TLinkScanner;
    constructor Create(ASender: TLinkScanner; const AMessage: string);
  end;
  
  ELinkScannerErrors = class of ELinkScannerError;
  
  TLinkScannerProgress = function(Sender: TLinkScanner): boolean of object;
  
  ELinkScannerAbort = class(ELinkScannerError)
  end;
  
  ELinkScannerEditError = class(ELinkScannerError)
    Buffer: Pointer;
    BufferPos: integer;
    constructor Create(ASender: TLinkScanner; const AMessage: string;
      ABuffer: Pointer; ABufferPos: integer);
  end;

  { TLinkScanner }
  
  TLinkScanner = class(TObject)
  private
    FLinks: PSourceLink; // list of TSourceLink
    FLinkCount: integer;
    FLinkCapacity: integer;
    FCleanedSrc: string;
    FLastCleanedSrcLen: integer;
    FOnGetSource: TOnGetSource;
    FOnGetFileName: TOnGetFileName;
    FOnGetSourceStatus: TOnGetSourceStatus;
    FOnLoadSource: TOnLoadSource;
    FOnDeleteSource: TOnDeleteSource;
    FOnCheckFileOnDisk: TOnCheckFileOnDisk;
    FOnGetInitValues: TOnGetInitValues;
    FOnIncludeCode: TOnIncludeCode;
    FOnProgress: TLinkScannerProgress;
    FIgnoreErrorAfterCode: Pointer;
    FIgnoreErrorAfterCursorPos: integer;
    FInitValues: TExpressionEvaluator;
    FInitValuesChangeStep: integer;
    FSourceChangeSteps: TFPList; // list of PSourceChangeStep sorted with Code
    FChangeStep: integer;
    FMainSourceFilename: string;
    FMainCode: pointer;
    FScanTill: TLinkScannerRange;
    FIgnoreMissingIncludeFiles: boolean;
    FNestedComments: boolean;
    FForceUpdateNeeded: boolean;
    // global write lock
    FLastGlobalWriteLockStep: integer;
    FOnGetGlobalWriteLockInfo: TOnGetWriteLockInfo;
    FOnSetGlobalWriteLock: TOnSetWriteLock;
    function GetLinks(Index: integer): TSourceLink;
    procedure SetLinks(Index: integer; const Value: TSourceLink);
    procedure SetSource(ACode: Pointer); // set current source
    procedure AddSourceChangeStep(ACode: pointer; AChangeStep: integer);
    procedure AddLink(ACleanedPos, ASrcPos: integer; ACode: Pointer);
    procedure IncreaseChangeStep;
    procedure SetMainCode(const Value: pointer);
    procedure SetScanTill(const Value: TLinkScannerRange);
    procedure SetIgnoreMissingIncludeFiles(const Value: boolean);
    function TokenIs(const AToken: shortstring): boolean;
    function UpTokenIs(const AToken: shortstring): boolean;
  private
    // parsing
    CommentStyle: TCommentStyle;
    CommentLevel: integer;
    CommentStartPos: integer;      // position of '{', '(*', '//'
    CommentInnerStartPos: integer; // position after '{', '(*', '//'
    CommentInnerEndPos: integer;   // position of '}', '*)', #10
    CommentEndPos: integer;        // postion after '}', '*)', #10
    LastCleanSrcPos: integer;
    IfLevel: integer;
    KeywordFuncList: TKeyWordFunctionList;
    procedure ReadNextToken;
    function ReturnFromIncludeFileAndIsEnd: boolean;
    function ReadIdentifier: string;
    function ReadUpperIdentifier: string;
    procedure SkipSpace; {$IFDEF UseInline}inline;{$ENDIF}
    procedure SkipComment;
    procedure SkipDelphiComment;
    procedure SkipOldTPComment;
    procedure CommentEndNotFound;
    procedure EndComment;
    procedure IncCommentLevel;
    procedure DecCommentLevel;
    procedure HandleDirectives;
    procedure UpdateCleanedSource(SourcePos: integer);
    function ReturnFromIncludeFile: boolean;
    procedure InitKeyWordList;
    function DoEndToken: boolean; {$IFDEF UseInline}inline;{$ENDIF}
    function DoDefaultIdentToken: boolean; {$IFDEF UseInline}inline;{$ENDIF}
    function DoEndOfInterfaceToken: boolean; {$IFDEF UseInline}inline;{$ENDIF}
  private
    // directives
    FDirectiveName: shortstring;
    FDirectiveFuncList: TKeyWordFunctionList;
    FSkipDirectiveFuncList: TKeyWordFunctionList;
    FMacrosOn: boolean;
    FMissingIncludeFiles: TMissingIncludeFiles;
    FIncludeStack: TFPList; // list of TSourceLink
    FSkippingTillEndif: boolean;
    FSkipIfLevel: integer;
    FCompilerMode: TCompilerMode;
    FPascalCompiler: TPascalCompiler;
    procedure SetCompilerMode(const AValue: TCompilerMode);
    procedure SkipTillEndifElse;
    procedure SkipTillEndCifElse;
    function SkipIfDirective: boolean;
    function IfdefDirective: boolean;
    function IfCDirective: boolean;
    function IfndefDirective: boolean;
    function IfDirective: boolean;
    function IfOptDirective: boolean;
    function EndifDirective: boolean;
    function EndCDirective: boolean;
    function ElseDirective: boolean;
    function ElseCDirective: boolean;
    function ElseIfDirective: boolean;
    function ElIfCDirective: boolean;
    function IfEndDirective: boolean;
    function DefineDirective: boolean;
    function UndefDirective: boolean;
    function SetCDirective: boolean;
    function IncludeDirective: boolean;
    function IncludeFile(const AFilename: string): boolean;
    function IncludePathDirective: boolean;
    function SearchIncludeFile(AFilename: string; var NewCode: Pointer;
      var MissingIncludeFile: TMissingIncludeFile): boolean;
    function ShortSwitchDirective: boolean;
    function ReadNextSwitchDirective: boolean;
    function LongSwitchDirective: boolean;
    function ModeDirective: boolean;
    function ThreadingDirective: boolean;
    procedure BuildDirectiveFuncList;
    procedure PushIncludeLink(ACleanedPos, ASrcPos: integer; ACode: Pointer);
    function PopIncludeLink: TSourceLink;
    function GetIncludeFileIsMissing: boolean;
    function MissingIncludeFilesNeedsUpdate: boolean;
    procedure ClearMissingIncludeFiles;
  protected
    // errors
    LastErrorMessage: string;
    LastErrorSrcPos: integer;
    LastErrorCode: pointer;
    LastErrorIsValid: boolean;
    LastErrorBehindIgnorePosition: boolean;
    LastErrorCheckedForIgnored: boolean;
    CleanedIgnoreErrorAfterPosition: integer;
    procedure RaiseExceptionFmt(const AMessage: string; Args: array of const);
    procedure RaiseException(const AMessage: string);
    procedure RaiseExceptionClass(const AMessage: string;
      ExceptionClass: ELinkScannerErrors);
    procedure RaiseEditException(const AMessage: string; ABuffer: Pointer;
      ABufferPos: integer);
    procedure ClearLastError;
    procedure RaiseLastError;
    procedure DoCheckAbort;
  public
    // current values, positions, source, flags
    CleanedLen: integer;
    Src: string;     // current parsed source
    SrcPos: integer; // current position
    TokenStart: integer; // start position of current token
    TokenType: TLSTokenType;
    SrcLen: integer; // length of current source
    Code: pointer;   // current code object
    Values: TExpressionEvaluator;

    ScannedRange: TLinkScannerRange;

    function MainFilename: string;

    // links
    property Links[Index: integer]: TSourceLink read GetLinks write SetLinks;
    property LinkCount: integer read FLinkCount;
    function LinkIndexAtCleanPos(ACleanPos: integer): integer;
    function LinkIndexAtCursorPos(ACursorPos: integer; ACode: Pointer): integer;
    function LinkSize(Index: integer): integer;
    function LinkCleanedEndPos(Index: integer): integer;
    function FindFirstSiblingLink(LinkIndex: integer): integer;
    function FindParentLink(LinkIndex: integer): integer;
    function LinkIndexNearCursorPos(ACursorPos: integer; ACode: Pointer;
                                    var CursorInLink: boolean): integer;
    function CreateTreeOfSourceCodes: TAVLTree;

    // source mapping (Cleaned <-> Original)
    function CleanedSrc: string;
    function CursorToCleanPos(ACursorPos: integer; ACode: pointer;
                    var ACleanPos: integer): integer; // 0=valid CleanPos
                          //-1=CursorPos was skipped, CleanPos between two links
                          // 1=CursorPos beyond scanned code
    function CleanedPosToCursor(ACleanedPos: integer; var ACursorPos: integer;
                                var ACode: Pointer): boolean;
    function LastErrorIsInFrontOfCleanedPos(ACleanedPos: integer): boolean;
    procedure RaiseLastErrorIfInFrontOfCleanedPos(ACleanedPos: integer);

    // ranges
    function WholeRangeIsWritable(CleanStartPos, CleanEndPos: integer;
                                  ErrorOnFail: boolean): boolean;
    procedure FindCodeInRange(CleanStartPos, CleanEndPos: integer;
                              UniqueSortedCodeList: TFPList);
    procedure DeleteRange(CleanStartPos,CleanEndPos: integer);

    // scanning
    procedure Scan(Range: TLinkScannerRange; CheckFilesOnDisk: boolean);
    function UpdateNeeded(Range: TLinkScannerRange;
                          CheckFilesOnDisk: boolean): boolean;
    procedure SetIgnoreErrorAfter(ACursorPos: integer; ACode: Pointer);
    procedure ClearIgnoreErrorAfter;
    function IgnoreErrAfterPositionIsInFrontOfLastErrMessage: boolean;
    function IgnoreErrorAfterCleanedPos: integer;
    function IgnoreErrorAfterValid: boolean;
    function LoadSourceCaseLoUp(const AFilename: string): pointer;

    function GuessMisplacedIfdefEndif(StartCursorPos: integer;
                                      StartCode: pointer;
                                      var EndCursorPos: integer;
                                      var EndCode: Pointer): boolean;

    property ChangeStep: integer read FChangeStep;

    // global write lock
    procedure ActivateGlobalWriteLock;
    procedure DeactivateGlobalWriteLock;
    property OnGetGlobalWriteLockInfo: TOnGetWriteLockInfo
                 read FOnGetGlobalWriteLockInfo write FOnGetGlobalWriteLockInfo;
    property OnSetGlobalWriteLock: TOnSetWriteLock
                         read FOnSetGlobalWriteLock write FOnSetGlobalWriteLock;

    // properties
    property OnGetSource: TOnGetSource read FOnGetSource write FOnGetSource;
    property OnLoadSource: TOnLoadSource read FOnLoadSource write FOnLoadSource;
    property OnDeleteSource: TOnDeleteSource
                                     read FOnDeleteSource write FOnDeleteSource;
    property OnGetSourceStatus: TOnGetSourceStatus
                               read FOnGetSourceStatus write FOnGetSourceStatus;
    property OnGetFileName: TOnGetFileName
                                       read FOnGetFileName write FOnGetFileName;
    property OnCheckFileOnDisk: TOnCheckFileOnDisk
                               read FOnCheckFileOnDisk write FOnCheckFileOnDisk;
    property OnGetInitValues: TOnGetInitValues
                                   read FOnGetInitValues write FOnGetInitValues;
    property OnIncludeCode: TOnIncludeCode
                                       read FOnIncludeCode write FOnIncludeCode;
    property OnProgress: TLinkScannerProgress
                                             read FOnProgress write FOnProgress;
    property IgnoreMissingIncludeFiles: boolean read FIgnoreMissingIncludeFiles
                                             write SetIgnoreMissingIncludeFiles;
    property InitialValues: TExpressionEvaluator
                                             read FInitValues write FInitValues;
    property MainCode: pointer read FMainCode write SetMainCode;
    property IncludeFileIsMissing: boolean read GetIncludeFileIsMissing;
    property NestedComments: boolean read FNestedComments;
    property CompilerMode: TCompilerMode
                                       read FCompilerMode write SetCompilerMode;
    property PascalCompiler: TPascalCompiler
                                     read FPascalCompiler write FPascalCompiler;
    property ScanTill: TLinkScannerRange read FScanTill write SetScanTill;
        
    procedure Clear;
    function ConsistencyCheck: integer;
    procedure WriteDebugReport;
    constructor Create;
    destructor Destroy; override;
  end;

//----------------------------------------------------------------------------

  // memory system for PSourceLink(s)
  TPSourceLinkMemManager = class(TCodeToolMemManager)
  protected
    procedure FreeFirstItem; override;
  public
    procedure DisposePSourceLink(Link: PSourceLink);
    function NewPSourceLink: PSourceLink;
  end;

  // memory system for PSourceLink(s)
  TPSourceChangeStepMemManager = class(TCodeToolMemManager)
  protected
    procedure FreeFirstItem; override;
  public
    procedure DisposePSourceChangeStep(Step: PSourceChangeStep);
    function NewPSourceChangeStep: PSourceChangeStep;
  end;

//----------------------------------------------------------------------------
// compiler switches
const
  CompilerSwitchesNames: array['A'..'Z'] of shortstring=(
         'ALIGN'          // A
        ,'BOOLEVAL'       // B
        ,'ASSERTIONS'     // C
        ,'DEBUGINFO'      // D
        ,''               // E
        ,''               // F
        ,''               // G
        ,'LONGSTRINGS'    // H
        ,'IOCHECKS'       // I
        ,''               // J
        ,''               // K
        ,'LOCALSYMBOLS'   // L
        ,'TYPEINFO'       // M
        ,''               // N
        ,''               // O
        ,'OPENSTRINGS'    // P
        ,'OVERFLOWCHECKS' // Q
        ,'RANGECHECKS'    // R
        ,''               // S
        ,'TYPEADDRESS'    // T
        ,''               // U
        ,'VARSTRINGCHECKS'// V
        ,'STACKFRAMES'    // W
        ,'EXTENDEDSYNTAX' // X
        ,'REFERENCEINFO'  // Y
        ,''               // Z
     );

const
  CompilerModeNames: array[TCompilerMode] of shortstring=(
        'FPC', 'DELPHI', 'GPC', 'TP', 'OBJFPC', 'MACPAS'
     );
  PascalCompilerNames: array[TPascalCompiler] of shortstring=(
        'FPC', 'DELPHI'
     );

var
  CompilerModeVars: array[TCompilerMode] of shortstring;

  PSourceLinkMemManager: TPSourceLinkMemManager;
  PSourceChangeStepMemManager: TPSourceChangeStepMemManager;


procedure AddCodeToUniqueList(ACode: Pointer; UniqueSortedCodeList: TFPList);
function IndexOfCodeInUniqueList(ACode: Pointer;
                                 UniqueSortedCodeList: TList): integer;
function IndexOfCodeInUniqueList(ACode: Pointer;
                                 UniqueSortedCodeList: TFPList): integer;


implementation


// useful procs ----------------------------------------------------------------

function IndexOfCodeInUniqueList(ACode: Pointer;
  UniqueSortedCodeList: TList): integer;
var l,m,r: integer;
begin
  l:=0;
  r:=UniqueSortedCodeList.Count-1;
  m:=0;
  while r>=l do begin
    m:=(l+r) shr 1;
    if ACode<UniqueSortedCodeList[m] then
      r:=m-1
    else if ACode>UniqueSortedCodeList[m] then
      l:=m+1
    else begin
      Result:=m;
      exit;
    end;
  end;
  Result:=-1;
end;

function IndexOfCodeInUniqueList(ACode: Pointer;
  UniqueSortedCodeList: TFPList): integer;
var l,m,r: integer;
begin
  l:=0;
  r:=UniqueSortedCodeList.Count-1;
  m:=0;
  while r>=l do begin
    m:=(l+r) shr 1;
    if ACode<UniqueSortedCodeList[m] then
      r:=m-1
    else if ACode>UniqueSortedCodeList[m] then
      l:=m+1
    else begin
      Result:=m;
      exit;
    end;
  end;
  Result:=-1;
end;

procedure AddCodeToUniqueList(ACode: Pointer; UniqueSortedCodeList: TFPList);
var l,m,r: integer;
begin
  l:=0;
  r:=UniqueSortedCodeList.Count-1;
  m:=0;
  while r>=l do begin
    m:=(l+r) shr 1;
    if ACode<UniqueSortedCodeList[m] then
      r:=m-1
    else if ACode>UniqueSortedCodeList[m] then
      l:=m+1
    else
      exit;
  end;
  if (m<UniqueSortedCodeList.Count) and (ACode>UniqueSortedCodeList[m]) then
    inc(m);
  UniqueSortedCodeList.Insert(m,ACode);
end;

function CompareUpToken(const UpToken: shortstring; const Txt: string;
  TxtStartPos, TxtEndPos: integer): boolean;
var len, i: integer;
begin
  Result:=false;
  len:=TxtEndPos-TxtStartPos;
  if len<>length(UpToken) then exit;
  i:=1;
  while i<len do begin
    if (UpToken[i]<>UpChars[Txt[TxtStartPos]]) then exit;
    inc(i);
    inc(TxtStartPos);
  end;
  Result:=true;
end;

function CompareUpToken(const UpToken: ansistring; const Txt: string;
  TxtStartPos, TxtEndPos: integer): boolean;
var len, i: integer;
begin
  Result:=false;
  len:=TxtEndPos-TxtStartPos;
  if len<>length(UpToken) then exit;
  i:=1;
  while i<len do begin
    if (UpToken[i]<>UpChars[Txt[TxtStartPos]]) then exit;
    inc(i);
    inc(TxtStartPos);
  end;
  Result:=true;
end;



{ TLinkScanner }

procedure TLinkScanner.AddLink(ACleanedPos, ASrcPos: integer; ACode: pointer);
var
  NewCapacity: Integer;
begin
  if FLinkCount=FLinkCapacity then begin
    NewCapacity:=FLinkCapacity*2;
    if NewCapacity<16 then NewCapacity:=16;
    ReAllocMem(FLinks,NewCapacity*SizeOf(TSourceLink));
    FLinkCapacity:=NewCapacity;
  end;
  with FLinks[FLinkCount] do begin
    CleanedPos:=ACleanedPos;
    SrcPos:=ASrcPos;
    Code:=ACode;
  end;
  inc(FLinkCount);
end;

function TLinkScanner.CleanedSrc: string;
begin
  if length(FCleanedSrc)<>CleanedLen then begin
    SetLength(FCleanedSrc,CleanedLen);
  end;
  Result:=FCleanedSrc;
  if FLastCleanedSrcLen<CleanedLen then FLastCleanedSrcLen:=CleanedLen;
end;

procedure TLinkScanner.Clear;
var i: integer;
  PLink: PSourceLink;
  PStamp: PSourceChangeStep;
begin
  ClearLastError;
  ClearMissingIncludeFiles;
  for i:=0 to FIncludeStack.Count-1 do begin
    PLink:=PSourceLink(FIncludeStack[i]);
    PSourceLinkMemManager.DisposePSourceLink(PLink);
  end;
  FIncludeStack.Clear;
  FLinkCount:=0;
  FCleanedSrc:='';
  for i:=0 to FSourceChangeSteps.Count-1 do begin
    PStamp:=PSourceChangeStep(FSourceChangeSteps[i]);
    PSourceChangeStepMemManager.DisposePSourceChangeStep(PStamp);
  end;
  FSourceChangeSteps.Clear;
  IncreaseChangeStep;
end;

constructor TLinkScanner.Create;
begin
  inherited Create;
  FInitValues:=TExpressionEvaluator.Create;
  Values:=TExpressionEvaluator.Create;
  FChangeStep:=0;
  FSourceChangeSteps:=TFPList.Create;
  FMainCode:=nil;
  FMainSourceFilename:='';
  BuildDirectiveFuncList;
  FIncludeStack:=TFPList.Create;
  FNestedComments:=false;
end;

procedure TLinkScanner.DecCommentLevel;
begin
  if FNestedComments then dec(CommentLevel)
  else CommentLevel:=0;
end;

destructor TLinkScanner.Destroy;
begin
  Clear;
  KeywordFuncList.Free;
  FIncludeStack.Free;
  FSourceChangeSteps.Free;
  Values.Free;
  FInitValues.Free;
  ReAllocMem(FLinks,0);
  FDirectiveFuncList.Free;
  FSkipDirectiveFuncList.Free;
  inherited Destroy;
end;

function TLinkScanner.GetLinks(Index: integer): TSourceLink;
begin
  Result:=FLinks[Index];
end;

function TLinkScanner.LinkSize(Index: integer): integer;

  procedure IndexOutOfBounds;
  begin
    RaiseException('TLinkScanner.LinkSize  index '
       +IntToStr(Index)+' out of bounds: 0-'+IntToStr(LinkCount));
  end;

begin
  if (Index<0) or (Index>=LinkCount) then
    IndexOutOfBounds;
  if Index<LinkCount-1 then
    Result:=FLinks[Index+1].CleanedPos-FLinks[Index].CleanedPos
  else
    Result:=CleanedLen-FLinks[Index].CleanedPos;
end;

function TLinkScanner.LinkCleanedEndPos(Index: integer): integer;
begin
  Result:=FLinks[Index].CleanedPos+LinkSize(Index);
end;

function TLinkScanner.FindFirstSiblingLink(LinkIndex: integer): integer;
{ find link at the start of the code
  e.g. The resulting link SrcPos is always 1
  
   if LinkIndex is in the main code, the result will be 0
   if LinkIndex is in an include file, the result will be the first link of
   the include file. If the include file is included multiple times, it is
   treated as if they are different files.

  ToDo: if include file includes itself, directly or indirectly
}
var
  LastIndex: integer;
begin
  Result:=LinkIndex;
  if LinkIndex>=0 then begin
    LastIndex:=LinkIndex;
    while (Result>=0) do begin
      if FLinks[Result].Code=FLinks[LinkIndex].Code then begin
        if Links[Result].SrcPos>FLinks[LastIndex].SrcPos then begin
          // the include file was (in-)directly included by itself
          // -> skip
          Result:=FindParentLink(Result);
        end else if FLinks[Result].SrcPos=1 then begin
          // start found
          exit;
        end;
        LastIndex:=Result;
      end;
      dec(Result);
    end;
  end;
end;

function TLinkScanner.FindParentLink(LinkIndex: integer): integer;
// a parent link is the link of the include directive
// or in other words: the link in front of the first sibling link
begin
  Result:=FindFirstSiblingLink(LinkIndex);
  if Result>=0 then dec(Result);
end;

function TLinkScanner.LinkIndexNearCursorPos(ACursorPos: integer;
  ACode: Pointer; var CursorInLink: boolean): integer;
// returns the nearest link at cursorpos
// (either covering the cursorpos or in front)
var
  CurLinkSize: integer;
  BestLinkIndex: integer;
begin
  BestLinkIndex:=-1;
  Result:=0;
  CursorInLink:=false;
  while Result<LinkCount do begin
    if (ACode=FLinks[Result].Code) and (ACursorPos>=FLinks[Result].SrcPos) then
    begin
      CurLinkSize:=LinkSize(Result);
      if ACursorPos<FLinks[Result].SrcPos+CurLinkSize then begin
        CursorInLink:=true;
        exit;
      end else begin
        if (BestLinkIndex<0)
        or (FLinks[BestLinkIndex].SrcPos<FLinks[Result].SrcPos) then begin
          BestLinkIndex:=Result;
        end;
      end;
    end;
    inc(Result);
  end;
  Result:=BestLinkIndex;
end;

function TLinkScanner.CreateTreeOfSourceCodes: TAVLTree;
var
  CurCode: Pointer;
  i: Integer;
begin
  Result:=TAVLTree.Create(@ComparePointers);
  for i:=0 to LinkCount-1 do begin
    CurCode:=FLinks[i].Code;
    if Result.Find(CurCode)=nil then
      Result.Add(CurCode);
  end;
end;

function TLinkScanner.LinkIndexAtCleanPos(ACleanPos: integer): integer;

  procedure ConsistencyError1;
  begin
    raise Exception.Create(
      'TLinkScanner.LinkAtCleanPos Consistency-Error 1');
  end;

  procedure ConsistencyError2;
  begin
    raise Exception.Create(
      'TLinkScanner.LinkAtCleanPos Consistency-Error 2');
  end;

var l,r,m: integer;
begin
  Result:=-1;
  if (ACleanPos<1) or (ACleanPos>CleanedLen) then exit;
  // binary search through the links
  l:=0;
  r:=LinkCount-1;
  while l<=r do begin
    m:=(l+r) div 2;
    if m<LinkCount-1 then begin
      if ACleanPos<FLinks[m].CleanedPos then
        r:=m-1
      else if ACleanPos>=FLinks[m+1].CleanedPos then
        l:=m+1
      else begin
        Result:=m;
        exit;
      end;
    end else begin
      if ACleanPos>=FLinks[m].CleanedPos then begin
        Result:=m;
        exit;
      end else
        ConsistencyError2;
    end;
  end;
  ConsistencyError1;
end;

function TLinkScanner.LinkIndexAtCursorPos(ACursorPos: integer; ACode: Pointer
  ): integer;
var
  CurLinkSize: integer;
begin
  Result:=0;
  while Result<LinkCount do begin
    if (ACode=FLinks[Result].Code) and (ACursorPos>=FLinks[Result].SrcPos) then begin
      CurLinkSize:=LinkSize(Result);
      if ACursorPos<FLinks[Result].SrcPos+CurLinkSize then begin
        exit;
      end;
    end;
    inc(Result);
  end;
  Result:=-1;
end;

procedure TLinkScanner.SetSource(ACode: pointer);

  procedure RaiseUnableToGetCode;
  begin
    RaiseException('unable to get source with Code='+DbgS(Code));
  end;

var SrcLog: TSourceLog;
begin
  if Assigned(FOnGetSource) then begin
    SrcLog:=FOnGetSource(Self,ACode);
    if SrcLog=nil then
      RaiseUnableToGetCode;
    AddSourceChangeStep(ACode,SrcLog.ChangeStep);
    Src:=SrcLog.Source;
    Code:=ACode;
    SrcPos:=1;
    TokenStart:=1;
    TokenType:=lsttNone;
    SrcLen:=length(Src);
    LastCleanSrcPos:=0;
  end else begin
    RaiseUnableToGetCode;
  end;
end;

procedure TLinkScanner.HandleDirectives;
var DirStart, DirLen: integer;
begin
  SrcPos:=CommentInnerStartPos+1;
  DirStart:=SrcPos;
  while (SrcPos<=SrcLen) and (IsIdentStartChar[Src[SrcPos]]) do
    inc(SrcPos);
  DirLen:=SrcPos-DirStart;
  if DirLen>255 then DirLen:=255;
  FDirectiveName:=UpperCaseStr(copy(Src,DirStart,DirLen));
  FDirectiveFuncList.DoIt(Src,DirStart,DirLen);
  SrcPos:=CommentEndPos;
end;

procedure TLinkScanner.IncCommentLevel;
begin
  if FNestedComments then inc(CommentLevel)
  else CommentLevel:=1;
end;

procedure TLinkScanner.IncreaseChangeStep;
begin
  if FChangeStep=$7fffffff then FChangeStep:=-$7fffffff
  else inc(FChangeStep);
end;

function TLinkScanner.ReturnFromIncludeFileAndIsEnd: boolean;
begin
  Result:=false;
  if not ReturnFromIncludeFile then begin
    SrcPos:=SrcLen+1; // make sure SrcPos stands somewhere
    TokenStart:=SrcPos;
    TokenType:=lsttSrcEnd;
    Result:=true;
  end;
end;

procedure TLinkScanner.ReadNextToken;
var
  c1: char;
  c2: char;
begin
  // Skip all spaces and comments
  //DebugLn(' TLinkScanner.ReadNextToken SrcPos=',SrcPos,' SrcLen=',SrcLen,' "',copy(Src,SrcPos,5),'"');
  if (SrcPos>SrcLen) and ReturnFromIncludeFileAndIsEnd then exit;
  c1:=Src[SrcPos];
  if IsCommentStartChar[c1] or IsSpaceChar[c1] then begin
    while true do begin
      if IsCommentStartChar[c1] then begin
        case c1 of
        '{' :
          SkipComment;
        '/':
          if (SrcPos<SrcLen) and (Src[SrcPos+1]='/') then
            SkipDelphiComment
          else
            break;
        '(':
          if (SrcPos<SrcLen) and (Src[SrcPos+1]='*') then
            SkipOldTPComment
          else
            break;
        end;
      end else if IsSpaceChar[c1] then begin
        repeat
          inc(SrcPos);
        until (SrcPos>SrcLen) or (not (IsSpaceChar[Src[SrcPos]]));
      end else
        break;
      if (SrcPos>SrcLen) and ReturnFromIncludeFileAndIsEnd then exit;
      c1:=Src[SrcPos];
    end;
  end;
  TokenStart:=SrcPos;
  TokenType:=lsttNone;
  // read token
  case c1 of
    '_','A'..'Z','a'..'z':
      begin
        // identifier
        inc(SrcPos);
        while (SrcPos<=SrcLen)
        and (IsIdentChar[Src[SrcPos]]) do
          inc(SrcPos);
        KeywordFuncList.DoIt(Src,TokenStart,SrcPos-TokenStart);
      end;
    '''','#':
      begin
        while (SrcPos<=SrcLen) do begin
          case (Src[SrcPos]) of
          '#':
            begin
              inc(SrcPos);
              while (SrcPos<=SrcLen)
              and (IsNumberChar[Src[SrcPos]]) do
                inc(SrcPos);
            end;
          '''':
            begin
              inc(SrcPos);
              while (SrcPos<=SrcLen) do begin
                case Src[SrcPos] of
                '''':
                  begin
                    inc(SrcPos);
                    break;
                  end;
                #10,#13:
                  break;
                else
                  inc(SrcPos);
                end;
              end;
            end;
          else
            break;
          end;
        end;
      end;
    '0'..'9':
      begin
        inc(SrcPos);
        while (SrcPos<=SrcLen) and (IsNumberChar[Src[SrcPos]]) do
          inc(SrcPos);
        if (SrcPos<SrcLen) and (Src[SrcPos]='.') and (Src[SrcPos+1]<>'.')
        then begin
          // real type number
          inc(SrcPos);
          while (SrcPos<=SrcLen) and (IsNumberChar[Src[SrcPos]]) do
            inc(SrcPos);
          if (SrcPos<=SrcLen) and (Src[SrcPos] in ['E','e']) then begin
            // read exponent
            inc(SrcPos);
            if (SrcPos<=SrcLen) and (Src[SrcPos] in ['-','+']) then inc(SrcPos);
            while (SrcPos<=SrcLen) and (IsNumberChar[Src[SrcPos]]) do
              inc(SrcPos);
          end;
        end;
      end;
    '%':
      begin
        inc(SrcPos);
        while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'1']) do
          inc(SrcPos);
      end;
    '$':
      begin
        inc(SrcPos);
        while (SrcPos<=SrcLen)
        and (IsHexNumberChar[Src[SrcPos]]) do
          inc(SrcPos);
      end;
    '=':
      begin
        inc(SrcPos);
        TokenType:=lsttEqual;
      end;
    '.':
      begin
        inc(SrcPos);
        TokenType:=lsttPoint;
      end;
    else
      inc(SrcPos);
      if SrcPos<=SrcLen then begin
        c2:=Src[SrcPos];
        // test for double char operators
        //  :=, +=, -=, /=, *=, <>, <=, >=, **, ><, ..
        if ((c2='=') and  (IsEqualOperatorStartChar[c1]))
        or ((c1='<') and (c2='>'))
        or ((c1='>') and (c2='<'))
        or ((c1='.') and (c2='.'))
        or ((c1='*') and (c2='*'))
        then inc(SrcPos);
      end;
  end;
end;

procedure TLinkScanner.Scan(Range: TLinkScannerRange; CheckFilesOnDisk: boolean);
var
  LastTokenType: TLSTokenType;
  cm: TCompilerMode;
  pc: TPascalCompiler;
  s: string;
  LastProgressPos: integer;
  CheckForAbort: boolean;
  NewSrcLen: Integer;
begin
  if (not UpdateNeeded(Range,CheckFilesOnDisk)) then begin
    // input is the same as last time -> output is the same
    // -> if there was an error, raise it again
    if LastErrorIsValid
    and ((not IgnoreErrorAfterValid)
      or (not IgnoreErrAfterPositionIsInFrontOfLastErrMessage))
    then
      RaiseLastError;
    exit;
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TLinkScanner.Scan A -------- TillInterfaceEnd=',dbgs(TillInterfaceEnd));
  {$ENDIF}
  ScanTill:=Range;
  Clear;
  IncreaseChangeStep;
  {$IFDEF CTDEBUG}
  DebugLn('TLinkScanner.Scan B ');
  {$ENDIF}
  SetSource(FMainCode);
  NewSrcLen:=length(Src);
  if NewSrcLen<FLastCleanedSrcLen+1000 then
    NewSrcLen:=FLastCleanedSrcLen+1000;
  SetLength(FCleanedSrc,NewSrcLen);
  CleanedLen:=0;
  {$IFDEF CTDEBUG}
  DebugLn('TLinkScanner.Scan C ',dbgs(SrcLen));
  {$ENDIF}
  ScannedRange:=lsrNone;
  CommentStyle:=CommentNone;
  CommentLevel:=0;
  CompilerMode:=cmFPC;
  PascalCompiler:=pcFPC;
  IfLevel:=0;
  FSkippingTillEndif:=false;
  //DebugLn('TLinkScanner.Scan D --------');
  
  // initialize Defines
  if Assigned(FOnGetInitValues) then
    FInitValues.Assign(FOnGetInitValues(FMainCode,FInitValuesChangeStep));
  Values.Assign(FInitValues);

  // compiler
  s:=FInitValues.Variables[PascalCompilerDefine];
  for pc:=Low(TPascalCompiler) to High(TPascalCompiler) do
    if (s=PascalCompilerNames[pc]) then
      PascalCompiler:=pc;

  // compiler mode
  for cm:=Low(TCompilerMode) to High(TCompilerMode) do
    if FInitValues.IsDefined(CompilerModeVars[cm]) then
      CompilerMode:=cm;

  // nested comments
  FNestedComments:=false;
  if (PascalCompiler=pcFPC) and (CompilerMode in [cmFPC,cmOBJFPC])
  and ((FInitValues.IsDefined(NestedCompilerDefine))
    or (CompareFileExt(MainFilename,'pp',false)=0))
  then
    FNestedComments:=true;
    
  //DebugLn(Values.AsString);
  //DebugLn('TLinkScanner.Scan E --------');
  FMacrosOn:=(Values.Variables['MACROS']<>'0');
  if Src='' then exit;
  // beging scanning
  InitKeyWordList;
  AddLink(1,SrcPos,Code);
  LastTokenType:=lsttNone;
  LastProgressPos:=0;
  CheckForAbort:=Assigned(OnProgress);
  {$IFDEF CTDEBUG}
  DebugLn('TLinkScanner.Scan F ',dbgs(SrcLen));
  {$ENDIF}
  if ScanTill=lsrInit then exit;
  try
    try
      repeat
        // check every 10.000 bytes for abort
        if CheckForAbort and ((LastProgressPos-LastCleanSrcPos)>10000) then begin
          LastProgressPos:=LastCleanSrcPos;
          DoCheckAbort;
        end;
        ReadNextToken;
        //DebugLn('TLinkScanner.Scan G "',copy(Src,TokenStart,SrcPos-TokenStart),'"');
        if (TokenType=lsttEndOfInterface) and (LastTokenType<>lsttEqual) then
        begin
          ScannedRange:=lsrInterface;
          if ScanTill=lsrInterface then break;
        end else if (LastTokenType=lsttEnd) and (TokenType=lsttPoint) then begin
          ScannedRange:=lsrEnd;
          break;
        end else if (SrcPos>SrcLen) and ReturnFromIncludeFileAndIsEnd then
          break;
        LastTokenType:=TokenType;
      until false;
    finally
      if not FSkippingTillEndif then begin
        {$IFDEF ShowUpdateCleanedSrc}
        DebugLn('TLinkScanner.Scan UpdatePos=',DbgS(SrcPos-1));
        {$ENDIF}
        UpdateCleanedSource(SrcPos-1);
      end;
    end;
    IncreaseChangeStep;
    FForceUpdateNeeded:=false;
    FLastCleanedSrcLen:=CleanedLen;
  except
    on E: ELinkScannerError do begin
      if (not IgnoreErrorAfterValid)
      or (not IgnoreErrAfterPositionIsInFrontOfLastErrMessage) then
        raise;
      {$IFDEF ShowIgnoreErrorAfter}
      DebugLn('TLinkScanner.Scan IGNORING ERROR: ',LastErrorMessage);
      {$ENDIF}
    end;
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TLinkScanner.Scan END ',dbgs(CleanedLen));
  {$ENDIF}
end;

procedure TLinkScanner.SetLinks(Index: integer; const Value: TSourceLink);
begin
  FLinks[Index]:=Value;
end;

procedure TLinkScanner.SkipComment;
// a normal pascal {} comment
begin
  CommentStyle:=CommentTP;
  CommentStartPos:=SrcPos;
  IncCommentLevel;
  inc(SrcPos);
  CommentInnerStartPos:=SrcPos;
  { HandleSwitches can dec CommentLevel }
  while (SrcPos<=SrcLen) and (CommentLevel>0) do begin
    case Src[SrcPos] of
      '{' : IncCommentLevel;
      '}' : DecCommentLevel;
    end;
    inc(SrcPos);
  end;
  CommentEndPos:=SrcPos;
  CommentInnerEndPos:=SrcPos-1;
  if (CommentLevel>0) then CommentEndNotFound;
  { handle compiler switches }
  if Src[CommentInnerStartPos]='$' then HandleDirectives;
  EndComment;
end;

procedure TLinkScanner.SkipDelphiComment;
// a  // newline  comment
begin
  CommentStyle:=CommentDelphi;
  CommentStartPos:=SrcPos;
  IncCommentLevel;
  inc(SrcPos,2);
  CommentInnerStartPos:=SrcPos;
  while (SrcPos<=SrcLen) and (Src[SrcPos]<>#10) do inc(SrcPos);
  DecCommentLevel;
  inc(SrcPos);
  CommentEndPos:=SrcPos;
  CommentInnerEndPos:=SrcPos-1;
  { handle compiler switches (ignore) }
  EndComment;
end;

procedure TLinkScanner.SkipOldTPComment;
// a (* *) comment
begin
  CommentStyle:=CommentDelphi;
  CommentStartPos:=SrcPos;
  IncCommentLevel;
  inc(SrcPos,2);
  CommentInnerStartPos:=SrcPos;
  // ToDo: nested comments
  while (SrcPos<SrcLen) do begin
    if ((Src[SrcPos]<>'*') or (Src[SrcPos+1]<>')')) then
      inc(SrcPos)
    else begin
      DecCommentLevel;
      inc(SrcPos,2);
      break;
    end;
  end;
  CommentEndPos:=SrcPos;
  CommentInnerEndPos:=SrcPos-2;
  if (CommentLevel>0) then CommentEndNotFound;
  { handle compiler switches }
  if Src[CommentInnerStartPos]='$' then HandleDirectives;
  EndComment;
end;

procedure TLinkScanner.CommentEndNotFound;
begin
  SrcPos:=CommentStartPos;
  RaiseException(ctsCommentEndNotFound);
end;

procedure TLinkScanner.UpdateCleanedSource(SourcePos: integer);
// add new parsed code to cleaned source string
var AddLen, i: integer;
begin
  if SourcePos=LastCleanSrcPos then exit;
  if SourcePos>SrcLen then SourcePos:=SrcLen;
  AddLen:=SourcePos-LastCleanSrcPos;
  if AddLen>length(FCleanedSrc)-CleanedLen then begin
    // expand cleaned source string by at least OldLen+1024
    i:=length(FCleanedSrc)+1024;
    if AddLen<i then AddLen:=i;
    SetLength(FCleanedSrc,length(FCleanedSrc)+AddLen);
  end;
  for i:=LastCleanSrcPos+1 to SourcePos do begin
    inc(CleanedLen);
    FCleanedSrc[CleanedLen]:=Src[i];
  end;
  {$IFDEF ShowUpdateCleanedSrc}
  DebugLn('TLinkScanner.UpdateCleanedSource A ',
    DbgS(LastCleanSrcPos),'-',DbgS(SourcePos),'="',
    StringToPascalConst(copy(Src,LastCleanSrcPos+1,20)),
    '".."',StringToPascalConst(copy(Src,SourcePos-19,20)),'"');
  {$ENDIF}
  LastCleanSrcPos:=SourcePos;
end;

procedure TLinkScanner.AddSourceChangeStep(ACode: pointer;AChangeStep: integer);

  procedure RaiseCodeNil;
  begin
    RaiseException('TLinkScanner.AddSourceChangeStep ACode=nil');
  end;

var l,r,m: integer;
  NewSrcChangeStep: PSourceChangeStep;
  c: pointer;
begin
  //DebugLn('[TLinkScanner.AddSourceChangeStep] ',DbgS(ACode));
  if ACode=nil then
    RaiseCodeNil;
  l:=0;
  r:=FSourceChangeSteps.Count-1;
  m:=0;
  c:=nil;
  while (l<=r) do begin
    m:=(l+r) shr 1;
    c:=PSourceChangeStep(FSourceChangeSteps[m])^.Code;
    if c<ACode then l:=m+1
    else if c>ACode then r:=m-1
    else exit;
  end;
  NewSrcChangeStep:=PSourceChangeStepMemManager.NewPSourceChangeStep;
  NewSrcChangeStep^.Code:=ACode;
  NewSrcChangeStep^.ChangeStep:=AChangeStep;
  if (FSourceChangeSteps.Count>0) and (c<ACode) then inc(m);
  FSourceChangeSteps.Insert(m,NewSrcChangeStep);
  //DebugLn('   ADDING ',DbgS(ACode),',',FSourceChangeSteps.Count);
end;

function TLinkScanner.TokenIs(const AToken: shortstring): boolean;
var ATokenLen: integer;
  i: integer;
begin
  Result:=false;
  if (SrcPos<=SrcLen+1) and (TokenStart>=1) then begin
    ATokenLen:=length(AToken);
    if ATokenLen=SrcPos-TokenStart then begin
      for i:=1 to ATokenLen do
        if AToken[i]<>Src[TokenStart-1+i] then exit;
      Result:=true;
    end;
  end;
end;

function TLinkScanner.UpTokenIs(const AToken: shortstring): boolean;
var ATokenLen: integer;
  i: integer;
begin
  Result:=false;
  if (SrcPos<=SrcLen+1) and (TokenStart>=1) then begin
    ATokenLen:=length(AToken);
    if ATokenLen=SrcPos-TokenStart then begin
      for i:=1 to ATokenLen do
        if AToken[i]<>UpChars[Src[TokenStart-1+i]] then exit;
      Result:=true;
    end;
  end;
end;

function TLinkScanner.ConsistencyCheck: integer;
var i: integer;
begin
  if (FLinks=nil) xor (FLinkCapacity=0) then begin
    Result:=-1; exit;
  end;
  if FLinks<>nil then begin
    for i:=0 to FLinkCount-1 do begin
      if FLinks[i].Code=nil then begin
        Result:=-2;  exit;
      end;
      if (FLinks[i].CleanedPos<1) or (FLinks[i].CleanedPos>SrcLen) then begin
        Result:=-3;  exit;
      end;
    end;
  end;
  if SrcLen<>length(Src) then begin // length of current source
    Result:=-4;  exit;
  end;
  if Values<>nil then begin
    Result:=Values.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,10);  exit;
    end;
  end;
  Result:=0;
end;

procedure TLinkScanner.WriteDebugReport;
var i: integer;
begin
  // header
  DebugLn('');
  DebugLn('[TLinkScanner.WriteDebugReport]',
     ' ChangeStepCount=',dbgs(FSourceChangeSteps.Count),
     ' LinkCount=',dbgs(LinkCount),
     ' CleanedLen=',dbgs(CleanedLen));
  // time stamps
  for i:=0 to FSourceChangeSteps.Count-1 do begin
    DebugLn('  ChangeStep ',dbgs(i),': '
        ,' Code=',dbgs(PSourceChangeStep(FSourceChangeSteps[i])^.Code)
        ,' ChangeStep=',dbgs(PSourceChangeStep(FSourceChangeSteps[i])^.ChangeStep));
  end;
  // links
  for i:=0 to LinkCount-1 do begin
    DebugLn('  Link ',dbgs(i),':'
        ,' CleanedPos=',dbgs(FLinks[i].CleanedPos)
        ,' SrcPos=',dbgs(FLinks[i].SrcPos)
        ,' Code=',dbgs(FLinks[i].Code)
      );
  end;
end;

function TLinkScanner.UpdateNeeded(
  Range: TLinkScannerRange; CheckFilesOnDisk: boolean): boolean;
{ the clean source must be rebuild if
   1. scanrange changed from only interface to whole source
   2. unit source changed
   3. one of its include files changed
   4. init values changed (e.g. initial compiler defines)
}
var i: integer;
  SrcLog: TSourceLog;
  NewInitValues: TExpressionEvaluator;
  GlobalWriteLockIsSet: boolean;
  GlobalWriteLockStep: integer;
  NewInitValuesChangeStep: integer;
begin
  Result:=true;
  if FForceUpdateNeeded then exit;
  
  // do a quick test: check the GlobalWriteLockStep
  if Assigned(OnGetGlobalWriteLockInfo) then begin
    OnGetGlobalWriteLockInfo(GlobalWriteLockIsSet,GlobalWriteLockStep);
    if GlobalWriteLockIsSet then begin
      // The global write lock is set. That means, input variables and code are
      // frozen
      if (FLastGlobalWriteLockStep=GlobalWriteLockStep) then begin
        // source and values did not change since last UpdateNeeded check
        // -> check only if ScanTill has increased
        if ord(Range)>ord(ScannedRange) then exit;
        Result:=false;
        exit;
      end else begin
        // this is the first check in this GlobalWriteLockStep
        FLastGlobalWriteLockStep:=GlobalWriteLockStep;
        // proceed normally ...
      end;
    end;
  end;
  
  // check if any input has changed ...
  FForceUpdateNeeded:=true;
  
  // check if ScanRange has increased
  if ord(Range)>ord(ScannedRange) then exit;

  // check all used files
  if Assigned(FOnGetSource) then begin
    if CheckFilesOnDisk and Assigned(FOnCheckFileOnDisk) then begin
      // if files changed on disk, reload them
      for i:=0 to FSourceChangeSteps.Count-1 do begin
        SrcLog:=FOnGetSource(Self,
                             PSourceChangeStep(FSourceChangeSteps[i])^.Code);
        FOnCheckFileOnDisk(SrcLog);
      end;
    end;
    for i:=0 to FSourceChangeSteps.Count-1 do begin
      SrcLog:=FOnGetSource(Self,PSourceChangeStep(FSourceChangeSteps[i])^.Code);
      if PSourceChangeStep(FSourceChangeSteps[i])^.ChangeStep<>SrcLog.ChangeStep
      then exit;
    end;
  end;
  
  // check initvalues
  if Assigned(FOnGetInitValues) then begin
    if FInitValues=nil then exit;
    NewInitValues:=FOnGetInitValues(Code,NewInitValuesChangeStep);
    if (NewInitValues<>nil)
    and (NewInitValuesChangeStep<>FInitValuesChangeStep)
    and (not FInitValues.Equals(NewInitValues)) then
      exit;
  end;
  
  // check missing include files
  if MissingIncludeFilesNeedsUpdate then exit;
  
  // no update needed :)
  FForceUpdateNeeded:=false;
  //DebugLn('TLinkScanner.UpdateNeeded END');
  Result:=false;
end;

procedure TLinkScanner.SetIgnoreErrorAfter(ACursorPos: integer; ACode: Pointer
  );
begin
  if (FIgnoreErrorAfterCode=ACode)
  and (FIgnoreErrorAfterCursorPos=ACursorPos) then exit;
  FIgnoreErrorAfterCode:=ACode;
  FIgnoreErrorAfterCursorPos:=ACursorPos;
  LastErrorCheckedForIgnored:=false;
  {$IFDEF ShowIgnoreErrorAfter}
  DbgOut('TLinkScanner.SetIgnoreErrorAfter ');
  if FIgnoreErrorAfterCode<>nil then
    DbgOut(OnGetFileName(Self,FIgnoreErrorAfterCode))
  else
    DbgOut('nil');
  DbgOut(' ',dbgs(FIgnoreErrorAfterCursorPos));
  DebugLn('');
  {$ENDIF}
end;

procedure TLinkScanner.ClearIgnoreErrorAfter;
begin
  SetIgnoreErrorAfter(0,nil);
end;

function TLinkScanner.IgnoreErrAfterPositionIsInFrontOfLastErrMessage: boolean;
var
  CleanResult: integer;
begin
  //DebugLn('TLinkScanner.IgnoreErrAfterPositionIsInFrontOfLastErrMessage');
  //DebugLn(['  LastErrorCheckedForIgnored=',LastErrorCheckedForIgnored,
  //  ' LastErrorBehindIgnorePosition=',LastErrorBehindIgnorePosition]);
  if LastErrorCheckedForIgnored then
    Result:=LastErrorBehindIgnorePosition
  else begin
    CleanedIgnoreErrorAfterPosition:=-1;
    if (FIgnoreErrorAfterCode<>nil) and (FIgnoreErrorAfterCursorPos>0) then
    begin
      CleanResult:=CursorToCleanPos(FIgnoreErrorAfterCursorPos,
                         FIgnoreErrorAfterCode,CleanedIgnoreErrorAfterPosition);
      //DebugLn(['  CleanResult=',CleanResult,
      //  ' CleanedIgnoreErrorAfterPosition=',CleanedIgnoreErrorAfterPosition,
      //  ' FIgnoreErrorAfterCursorPos=',FIgnoreErrorAfterCursorPos,
      //  ' CleanedLen=',CleanedLen,
      //  ' LastErrorIsValid=',LastErrorIsValid]);
      if (CleanResult=0) or (CleanResult=-1)
      or (not LastErrorIsValid) then begin
        Result:=true;
      end else begin
        Result:=false;
      end;
    end else begin
      Result:=false;
    end;
    LastErrorBehindIgnorePosition:=Result;
    LastErrorCheckedForIgnored:=true;
  end;
  {$IFDEF ShowIgnoreErrorAfter}
  DebugLn('TLinkScanner.IgnoreErrAfterPositionIsInFrontOfLastErrMessage Result=',dbgs(Result));
  {$ENDIF}
end;

function TLinkScanner.IgnoreErrorAfterCleanedPos: integer;
begin
  if IgnoreErrAfterPositionIsInFrontOfLastErrMessage then
    Result:=CleanedIgnoreErrorAfterPosition
  else
    Result:=-1;
  {$IFDEF ShowIgnoreErrorAfter}
  DebugLn('TLinkScanner.IgnoreErrorAfterCleanedPos Result=',dbgs(Result));
  {$ENDIF}
end;

function TLinkScanner.IgnoreErrorAfterValid: boolean;
begin
  Result:=(FIgnoreErrorAfterCode<>nil);
  {$IFDEF ShowIgnoreErrorAfter}
  DebugLn('TLinkScanner.IgnoreErrorAfterValid Result=',dbgs(Result));
  {$ENDIF}
end;

function TLinkScanner.LastErrorIsInFrontOfCleanedPos(ACleanedPos: integer
  ): boolean;
begin
  Result:=LastErrorIsValid and (CleanedLen>ACleanedPos);
  {$IFDEF ShowIgnoreErrorAfter}
  DebugLn('TLinkScanner.LastErrorIsInFrontOfCleanedPos Result=',dbgs(Result));
  {$ENDIF}
end;

procedure TLinkScanner.RaiseLastErrorIfInFrontOfCleanedPos(ACleanedPos: integer
  );
begin
  if LastErrorIsInFrontOfCleanedPos(ACleanedPos) then
    RaiseLastError;
end;

{-------------------------------------------------------------------------------
  function TLinkScanner.GuessMisplacedIfdefEndif
  Params: StartCursorPos: integer; StartCode: pointer;
          var EndCursorPos: integer; var EndCode: Pointer;
  Result: boolean;


-------------------------------------------------------------------------------}
function TLinkScanner.GuessMisplacedIfdefEndif(StartCursorPos: integer;
  StartCode: pointer;
  var EndCursorPos: integer; var EndCode: Pointer): boolean;
  
  type
    TIf = record
      StartPos: integer; // comment start e.g. {
      EndPos: integer;   // comment end  e.g. the char behind }
      Expression: string;
      HasElse: boolean;
    end;
    PIf = ^TIf;
    
    TTokenType = (ttNone,
                  ttCommentStart, ttCommentEnd, // '{' '}'
                  ttTPCommentStart, ttTPCommentEnd, // '(*' '*)'
                  ttDelphiCommentStart, // '//'
                  ttLineEnd
                  );
                  
    TTokenRange = (trCode, trComment, trTPComment, trDelphiComment);

    TToken = record
      StartPos: integer;
      EndPos: integer;
      TheType: TTokenType;
      Range: TTokenRange;
      NestedComments: boolean;
    end;
    
    TDirectiveType = (dtUnknown, dtIf, dtIfDef, dtIfNDef, dtIfOpt,
                      dtElse, dtEndif);
    
  function FindNextToken(const ASrc: string; var AToken: TToken): boolean;
  var
    ASrcLen: integer;
    OldRange: TTokenRange;
  begin
    Result:=true;
    AToken.StartPos:=AToken.EndPos;
    ASrcLen:=length(ASrc);
    OldRange:=AToken.Range;
    
    while (AToken.StartPos<=ASrcLen) do begin
      case ASrc[AToken.StartPos] of
      '{': // pascal comment start
        begin
          AToken.EndPos:=AToken.StartPos+1;
          AToken.TheType:=ttCommentStart;
          AToken.Range:=trComment;
          if (OldRange=trCode) then
            exit
          else if AToken.NestedComments then begin
            if (not FindNextToken(ASrc,AToken)) then begin
              Result:=false;
              exit;
            end;
            AToken.StartPos:=AToken.EndPos-1;
            AToken.Range:=OldRange;
          end;
        end;
        
      '(': // check if Turbo Pascal comment start
        if (AToken.StartPos<ASrcLen) and (ASrc[AToken.StartPos+1]='*') then
        begin
          AToken.EndPos:=AToken.StartPos+2;
          AToken.TheType:=ttTPCommentStart;
          AToken.Range:=trTPComment;
          if (OldRange=trCode) then
            exit
          else if AToken.NestedComments then begin
            if (not FindNextToken(ASrc,AToken)) then begin
              Result:=false;
              exit;
            end;
            AToken.StartPos:=AToken.EndPos-1;
            AToken.Range:=OldRange;
          end;
        end;
        
      '/': // check if Delphi comment start
        if (AToken.StartPos<ASrcLen) and (ASrc[AToken.StartPos+1]='/') then
        begin
          AToken.EndPos:=AToken.StartPos+2;
          AToken.TheType:=ttDelphiCommentStart;
          AToken.Range:=trDelphiComment;
          if (OldRange=trCode) then
            exit
          else if AToken.NestedComments then begin
            if (not FindNextToken(ASrc,AToken)) then begin
              Result:=false;
              exit;
            end;
            AToken.StartPos:=AToken.EndPos-1;
            AToken.Range:=OldRange;
          end;
        end;
        
      '}': // pascal comment end
        case AToken.Range of
        trComment:
          begin
            AToken.EndPos:=AToken.StartPos+1;
            AToken.TheType:=ttCommentEnd;
            AToken.Range:=trCode;
            exit;
          end;
          
        trCode:
          begin
            // error (comment was never openend)
            // -> skip rest of code
            AToken.StartPos:=ASrcLen;
          end;

        else
          // in different kind of comment -> ignore
        end;
        
      '*': // turbo pascal comment end
        if (AToken.StartPos<ASrcLen) and (ASrc[AToken.StartPos+1]=')') then
        begin
          case AToken.Range of
          trTPComment:
            begin
              AToken.EndPos:=AToken.StartPos+1;
              AToken.TheType:=ttTPCommentEnd;
              AToken.Range:=trCode;
              exit;
            end;

          trCode:
            begin
              // error (comment was never openend)
              // -> skip rest of code
              AToken.StartPos:=ASrcLen;
            end;

          else
            // in different kind of comment -> ignore
          end;
        end;

      #10,#13: // line end
        if AToken.Range in [trDelphiComment] then begin
          AToken.EndPos:=AToken.StartPos+1;
          if (AToken.StartPos<ASrcLen)
          and (ASrc[AToken.StartPos+1] in [#10,#13])
          and (ASrc[AToken.StartPos+1]<>ASrc[AToken.StartPos]) then
            inc(AToken.EndPos);
          AToken.TheType:=ttLineEnd;
          AToken.Range:=trCode;
          exit;
        end else begin
          // in different kind of comment -> ignore
        end;

      '''': // skip string constant
        begin
          inc(AToken.StartPos);
          while (AToken.StartPos<=ASrcLen) do begin
            if (not (ASrc[AToken.StartPos] in ['''',#10,#13])) then begin
              inc(AToken.StartPos);
            end else begin
              break;
            end;
          end;
        end;
        
      end;
      inc(AToken.StartPos);
    end;
    
    // at the end of the code
    AToken.EndPos:=AToken.StartPos;
    AToken.TheType:=ttNone;
    Result:=false;
  end;

  procedure FreeIfStack(var IfStack: TFPList);
  var
    i: integer;
    AnIf: PIf;
  begin
    if IfStack=nil then exit;
    for i:=0 to IfStack.Count-1 do begin
      AnIf:=PIf(IfStack[i]);
      AnIf^.Expression:='';
      Dispose(AnIf);
    end;
    IfStack.Free;
    IfStack:=nil;
  end;
  
  function InitGuessMisplaced(var CurToken: TToken; ACode: Pointer;
    var ASrc: string; var ASrcLen: integer): boolean;
  var
    ASrcLog: TSourceLog;
  begin
    Result:=false;
    
    // get source
    if (FOnGetSource=nil) then exit;
    ASrcLog:=FOnGetSource(Self,ACode);
    if ASrcLog=nil then exit;
    ASrc:=ASrcLog.Source;
    ASrcLen:=length(ASrc);

    CurToken.StartPos:=1;
    CurToken.EndPos:=1;
    CurToken.Range:=trCode;
    CurToken.TheType:=ttNone;
    CurToken.NestedComments:=NestedComments;
    Result:=true;
  end;
  
  function ReadDirectiveType(const ASrc: string;
    AToken: TToken): TDirectiveType;
  const
    DIR_RST: array[0..5] of TDirectiveType = (
      dtIfDef, dtIfNDef, dtIfOpt, dtIf, dtElse, dtEndif
    );
    DIR_TXT: array[0..5] of PChar = (
      'IFDEF', 'IFNDEF', 'IFOPT', 'IF', 'ELSE', 'ENDIF'
    );
  var
    ASrcLen, p: integer;
    n: Integer;
  begin
    Result:=dtUnknown;
    ASrcLen:=length(ASrc);
    p:=AToken.EndPos;
    if (p<ASrcLen) and (ASrc[p]='$') then
    begin
      // compiler directive
      inc(p);
      for n := Low(DIR_TXT) to High(DIR_TXT) do
      begin
        if CompareIdentifiers(@ASrc[p], DIR_TXT[n]) = 0
        then begin
          Result := DIR_RST[n];
          Exit;
        end;
      end;
    end;
  end;
  
  procedure PushIfOnStack(const ASrc: string; AToken: TToken; IfStack: TFPList);
  var
    NewIf: PIf;
  begin
    New(NewIf);
    FillChar(NewIf^,SizeOf(PIf),0);
    NewIf^.StartPos:=AToken.StartPos;
    FindNextToken(ASrc,AToken);
    NewIf^.EndPos:=AToken.EndPos;
    NewIf^.Expression:=copy(ASrc,NewIf^.StartPos+1,
                            AToken.EndPos-NewIf^.StartPos-1);
    NewIf^.HasElse:=false;
    IfStack.Add(NewIf);
  end;
  
  procedure PopIfFromStack(IfStack: TFPList);
  var Topif: PIf;
  begin
    TopIf:=PIf(IfStack[IfStack.Count-1]);
    Dispose(TopIf);
    IfStack.Delete(IfStack.Count-1);
  end;

  function GuessMisplacedIfdefEndifInCode(ACode: Pointer;
    StartCursorPos: integer; StartCode: Pointer;
    var EndCursorPos: integer; var EndCode: Pointer): boolean;
  var
    ASrc: string;
    ASrcLen: integer;
    CurToken: TToken;
    IfStack: TFPList;
    DirectiveType: TDirectiveType;
  begin
    Result:=false;
    if not InitGuessMisplaced(CurToken,ACode,ASrc,ASrcLen) then exit;

    IfStack:=TFPList.Create;
    try
      repeat
        if (not FindNextToken(ASrc,CurToken)) then begin
          exit;
        end;
        if CurToken.Range in [trComment] then begin
          DirectiveType:=ReadDirectiveType(ASrc,CurToken);

          case DirectiveType of

          dtIf, dtIfDef, dtIfNDef, dtIfOpt:
            PushIfOnStack(ASrc,CurToken,IfStack);

          dtElse:
            begin
              if (IfStack.Count=0) or (PIf(IfStack[IfStack.Count-1])^.HasElse)
              then begin
                // this $ELSE has no $IF
                // -> misplaced directive found
                EndCursorPos:=CurToken.EndPos;
                EndCode:=ACode;
                DebugLn('GuessMisplacedIfdefEndif  $ELSE has no $IF');
                Result:=true;
                exit;
              end;
              PIf(IfStack[IfStack.Count-1])^.HasElse:=true;
            end;
            
          dtEndif:
            begin
              if (IfStack.Count=0) then begin
                // this $ENDIF has no $IF
                // -> misplaced directive found
                EndCursorPos:=CurToken.EndPos;
                EndCode:=ACode;
                DebugLn('GuessMisplacedIfdefEndif  $ENDIF has no $IF');
                Result:=true;
                exit;
              end;
              PopIfFromStack(IfStack);
            end;

          end;
        end;
      until CurToken.TheType=ttNone;
      if IfStack.Count>0 then begin
        // there is an $IF without $ENDIF
        // -> misplaced directive found
        EndCursorPos:=PIf(IfStack[IfStack.Count-1])^.StartPos+1;
        EndCode:=ACode;
        DebugLn('GuessMisplacedIfdefEndif  $IF without $ENDIF');
        Result:=true;
        exit;
      end;
    finally
      FreeIfStack(IfStack);
    end;
  end;
        
var
  LinkID, i, BestSrcPos: integer;
  LastCode: Pointer;
  SearchedCodes: TFPList;
begin
  Result:=false;
  
  // search link before start position
  LinkID:=-1;
  BestSrcPos:=0;
  i:=0;
  while i<LinkCount do begin
    if (StartCode=FLinks[i].Code) and (StartCursorPos>=FLinks[i].SrcPos) then begin
      if (LinkID<0) or (BestSrcPos<FLinks[i].SrcPos) then
        LinkID:=i;
    end;
    inc(i);
  end;
  if LinkID<0 then exit;
  
  // go through all following sources and guess misplaced ifdef/endif
  SearchedCodes:=TFPList.Create;
  try
    while LinkId<LinkCount do begin
      Result:=GuessMisplacedIfdefEndifInCode(FLinks[LinkID].Code,
        StartCursorPos,StartCode,EndCursorPos,EndCode);
      if Result then exit;
      // search next code
      LastCode:=FLinks[LinkID].Code;
      SearchedCodes.Add(LastCode);
      repeat
        inc(LinkID);
        if LinkID>=LinkCount then exit;
      until (FLinks[LinkID].Code<>LastCode)
      and (SearchedCodes.IndexOf(FLinks[LinkID].Code)<0);
    end;
  finally
    SearchedCodes.Free;
  end;
end;

procedure TLinkScanner.SetMainCode(const Value: pointer);
begin
  if FMainCode=Value then exit;
  FMainCode:=Value;
  FMainSourceFilename:=FOnGetFileName(Self,FMainCode);
  Clear;
end;

procedure TLinkScanner.SetScanTill(const Value: TLinkScannerRange);
var
  OldScanRange: TLinkScannerRange;
begin
  if FScanTill=Value then exit;
  OldScanRange:=FScanTill;
  FScanTill := Value;
  if ord(OldScanRange)<ord(FScanTill) then Clear;
end;

function TLinkScanner.ShortSwitchDirective: boolean;
begin
  FDirectiveName:=CompilerSwitchesNames[FDirectiveName[1]];
  if FDirectiveName<>'' then begin
    if (SrcPos<=SrcLen) and (Src[SrcPos] in ['-','+']) then begin
      if Src[SrcPos]='-' then
        Values.Variables[FDirectiveName]:='0'
      else
        Values.Variables[FDirectiveName]:='1';
      Result:=ReadNextSwitchDirective;
    end else begin
      if FDirectiveName<>CompilerSwitchesNames['I'] then
        Result:=LongSwitchDirective
      else
        Result:=IncludeDirective;
    end;
  end else
    Result:=true;
end;

procedure TLinkScanner.BuildDirectiveFuncList;
var c: char;
begin
  FDirectiveFuncList:=TKeyWordFunctionList.Create;
  with FDirectiveFuncList do begin
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
  end;
  FSkipDirectiveFuncList:=TKeyWordFunctionList.Create;
  with FSkipDirectiveFuncList do begin
    Add('IFDEF',{$ifdef FPC}@{$endif}SkipIfDirective);
    Add('IFNDEF',{$ifdef FPC}@{$endif}SkipIfDirective);
    Add('IF',{$ifdef FPC}@{$endif}SkipIfDirective);
    Add('IFOPT',{$ifdef FPC}@{$endif}SkipIfDirective);
    Add('IFC',{$ifdef FPC}@{$endif}SkipIfDirective);
    Add('ENDIF',{$ifdef FPC}@{$endif}EndIfDirective);
    Add('ENDC',{$ifdef FPC}@{$endif}EndCDirective);
    Add('ELSE',{$ifdef FPC}@{$endif}ElseDirective);
    Add('ELSEC',{$ifdef FPC}@{$endif}ElseCDirective);
    Add('ELSEIF',{$ifdef FPC}@{$endif}ElseIfDirective);
    Add('ELIFC',{$ifdef FPC}@{$endif}ElIfCDirective);
    Add('IFEND',{$ifdef FPC}@{$endif}IfEndDirective);
  end;
end;

function TLinkScanner.LongSwitchDirective: boolean;
var ValStart: integer;
begin
  SkipSpace;
  ValStart:=SrcPos;
  while (SrcPos<=SrcLen) and IsWordChar[Src[SrcPos]] do
    inc(SrcPos);
  if CompareUpToken('ON',Src,ValStart,SrcPos) then
    Values.Variables[FDirectiveName]:='1'
  else if CompareUpToken('OFF',Src,ValStart,SrcPos) then
    Values.Variables[FDirectiveName]:='0'
  else if CompareUpToken('PRELOAD',Src,ValStart,SrcPos)
  and (FDirectiveName='ASSERTIONS') then
    Values.Variables[FDirectiveName]:='PRELOAD'
  else if (FDirectiveName='LOCALSYMBOLS') then
    // ignore link object directive
  else if (FDirectiveName='RANGECHECKS') then
    // ignore link object directive
  else if (FDirectiveName='ALIGN') then
    // set record align size
  else begin
    RaiseExceptionFmt(ctsInvalidFlagValueForDirective,
        [copy(Src,ValStart,SrcPos-ValStart),FDirectiveName]);
  end;
  Result:=ReadNextSwitchDirective;
end;

function TLinkScanner.ModeDirective: boolean;
// $MODE DEFAULT, OBJFPC, TP, FPC, GPC, DELPHI
var ValStart: integer;
  AMode: TCompilerMode;
  ModeValid: boolean;
begin
  SkipSpace;
  ValStart:=SrcPos;
  while (SrcPos<=SrcLen) and (IsWordChar[Src[SrcPos]]) do
    inc(SrcPos);
  // undefine all mode macros
  for AMode:=Low(TCompilerMode) to High(TCompilerMode) do
    Values.Undefine(CompilerModeVars[AMode]);
  CompilerMode:=cmFPC;
  // define new mode macro
  if CompareUpToken('DEFAULT',Src,ValStart,SrcPos) then begin
    // set mode to initial mode
    for AMode:=Low(TCompilerMode) to High(TCompilerMode) do
      if FInitValues.IsDefined(CompilerModeVars[AMode]) then begin
        CompilerMode:=AMode;
      end;
  end else begin
    ModeValid:=false;
    for AMode:=Low(TCompilerMode) to High(TCompilerMode) do
      if CompareUpToken(CompilerModeNames[AMode],Src,ValStart,SrcPos) then
      begin
        CompilerMode:=AMode;
        Values.Variables[CompilerModeVars[AMode]]:='1';
        ModeValid:=true;
        break;
      end;
    if not ModeValid then
      RaiseExceptionFmt(ctsInvalidMode,[copy(Src,ValStart,SrcPos-ValStart)]);
  end;
  Result:=true;
end;

function TLinkScanner.ThreadingDirective: boolean;
var
  ValStart: integer;
begin
  SkipSpace;
  ValStart:=SrcPos;
  while (SrcPos<=SrcLen) and (IsWordChar[Src[SrcPos]]) do
    inc(SrcPos);
  if CompareUpToken('ON',Src,ValStart,SrcPos) then begin
    // define THREADING
    Values.Variables[ExternalMacroStart+'UseSysThrds']:='1';
  end else begin
    // undefine THREADING
    Values.Undefine(ExternalMacroStart+'UseSysThrds');
  end;
  Result:=true;
end;

function TLinkScanner.ReadNextSwitchDirective: boolean;
var DirStart, DirLen: integer;
begin
  SkipSpace;
  if (SrcPos<=SrcLen) and (Src[SrcPos]=',') then begin
    inc(SrcPos);
    DirStart:=SrcPos;
    while (SrcPos<=SrcLen) and (IsIdentStartChar[Src[SrcPos]]) do
      inc(SrcPos);
    DirLen:=SrcPos-DirStart;
    if DirLen>255 then DirLen:=255;
    FDirectiveName:=UpperCaseStr(copy(Src,DirStart,DirLen));
    Result:=FDirectiveFuncList.DoIt(Src,DirStart,DirLen);
  end else
    Result:=true;
end;

function TLinkScanner.IfdefDirective: boolean;
// {$ifdef name comment}
var VariableName: string;
begin
  inc(IfLevel);
  SkipSpace;
  VariableName:=ReadUpperIdentifier;
  if (VariableName<>'') and (not Values.IsDefined(VariableName)) then
    SkipTillEndifElse;
  Result:=true;
end;

function TLinkScanner.IfCDirective: boolean;
// {$ifc expression} or indirectly called by {$elifc expression}
var Expr, ResultStr: string;
begin
  inc(IfLevel);
  inc(SrcPos);
  Expr:=UpperCaseStr(copy(Src,SrcPos,CommentInnerEndPos-SrcPos));
  ResultStr:=Values.Eval(Expr);
  Result:=true;
  if Values.ErrorPosition>=0 then begin
    inc(SrcPos,Values.ErrorPosition);
    RaiseException(ctsErrorInDirectiveExpression)
  end else if ResultStr='0' then
    SkipTillEndifElse
end;

procedure TLinkScanner.SkipSpace;
begin
  while (SrcPos<=SrcLen) and (IsSpaceChar[Src[SrcPos]]) do inc(SrcPos);
end;

function TLinkScanner.ReadIdentifier: string;
var StartPos: integer;
begin
  StartPos:=SrcPos;
  if (SrcPos<=SrcLen) and (IsIdentStartChar[Src[SrcPos]]) then begin
    inc(SrcPos);
    while (SrcPos<=SrcLen) and (IsIdentChar[Src[SrcPos]]) do
      inc(SrcPos);
    Result:=copy(Src,StartPos,SrcPos-StartPos);
  end else
    Result:='';
end;

function TLinkScanner.ReadUpperIdentifier: string;
var StartPos: integer;
begin
  StartPos:=SrcPos;
  if (SrcPos<=SrcLen) and (IsIdentStartChar[Src[SrcPos]]) then begin
    inc(SrcPos);
    while (SrcPos<=SrcLen) and (IsIdentChar[Src[SrcPos]]) do
      inc(SrcPos);
    Result:=UpperCaseStr(copy(Src,StartPos,SrcPos-StartPos));
  end else
    Result:='';
end;

procedure TLinkScanner.EndComment;
begin
  CommentStyle:=CommentNone;
end;

function TLinkScanner.IfndefDirective: boolean;
// {$ifndef name comment}
var VariableName: string;
begin
  inc(IfLevel);
  SkipSpace;
  VariableName:=ReadUpperIdentifier;
  if (VariableName<>'') and (Values.IsDefined(VariableName)) then
    SkipTillEndifElse;
  Result:=true;
end;

function TLinkScanner.EndifDirective: boolean;
// {$endif comment}

  procedure RaiseAWithoutB;
  begin
    RaiseExceptionFmt(ctsAwithoutB,['$ENDIF','$IF'])
  end;
  
begin
  dec(IfLevel);
  if IfLevel<0 then
    RaiseAWithoutB
  else if IfLevel<FSkipIfLevel then begin
    FSkippingTillEndif:=false;
  end;
  Result:=true;
end;

function TLinkScanner.EndCDirective: boolean;
// {$endc comment}

  procedure RaiseAWithoutB;
  begin
    RaiseExceptionFmt(ctsAwithoutB,['$ENDC','$IFC'])
  end;

begin
  dec(IfLevel);
  if IfLevel<0 then
    RaiseAWithoutB
  else if IfLevel<FSkipIfLevel then begin
    FSkippingTillEndif:=false;
  end;
  Result:=true;
end;

function TLinkScanner.ElseDirective: boolean;
// {$else comment}

  procedure RaiseAWithoutB;
  begin
    RaiseExceptionFmt(ctsAwithoutB,['$ELSE','$IF']);
  end;

begin
  if IfLevel=0 then
    RaiseAWithoutB;
  if not FSkippingTillEndif then
    SkipTillEndifElse
  else if IfLevel=FSkipIfLevel then
    FSkippingTillEndif:=false;
  Result:=true;
end;

function TLinkScanner.ElseCDirective: boolean;
// {$elsec comment}

  procedure RaiseAWithoutB;
  begin
    RaiseExceptionFmt(ctsAwithoutB,['$ELSEC','$IFC']);
  end;

begin
  if IfLevel=0 then
    RaiseAWithoutB;
  if not FSkippingTillEndif then
    SkipTillEndCifElse
  else if IfLevel=FSkipIfLevel then
    FSkippingTillEndif:=false;
  Result:=true;
end;

function TLinkScanner.ElseIfDirective: boolean;
// {$elseif expression}

  procedure RaiseAWithoutB;
  begin
    RaiseExceptionFmt(ctsAwithoutB,['$ELSEIF','$IF']);
  end;

begin
  if IfLevel=0 then
    RaiseAWithoutB;
  if not FSkippingTillEndif then begin
    SkipTillEndifElse;
    Result:=true;
  end else if IfLevel=FSkipIfLevel then
    Result:=IfDirective;
end;

function TLinkScanner.ElIfCDirective: boolean;
// {$elifc expression}

  procedure RaiseAWithoutB;
  begin
    RaiseExceptionFmt(ctsAwithoutB,['$ELIFC','$IFC']);
  end;

begin
  if IfLevel=0 then
    RaiseAWithoutB;
  if not FSkippingTillEndif then begin
    SkipTillEndifElse;
    Result:=true;
  end else if IfLevel=FSkipIfLevel then
    Result:=IfCDirective;
end;

function TLinkScanner.IfEndDirective: boolean;
// {$IfEnd comment}

  procedure RaiseAWithoutB;
  begin
    RaiseExceptionFmt(ctsAwithoutB,['$IfEnd','$ElseIf'])
  end;

begin
  dec(IfLevel);
  if IfLevel<0 then
    RaiseAWithoutB
  else if IfLevel<FSkipIfLevel then begin
    FSkippingTillEndif:=false;
  end;
  Result:=true;
end;

function TLinkScanner.DefineDirective: boolean;
// {$define name} or {$define name:=value}
var VariableName, NewValue: string;
begin
  SkipSpace;
  VariableName:=ReadUpperIdentifier;
  if (VariableName<>'') then begin
    SkipSpace;
    if FMacrosOn and (SrcPos<SrcLen)
    and (Src[SrcPos]=':') and (Src[SrcPos+1]='=')
    then begin
      inc(SrcPos,2);
      SkipSpace;
      NewValue:=copy(Src,SrcPos,CommentInnerEndPos-SrcPos);
      if CompareIdentifiers(PChar(NewValue),'false')=0 then
        NewValue:='0'
      else if CompareIdentifiers(PChar(NewValue),'true')=0 then
        NewValue:='1';
      Values.Variables[VariableName]:=NewValue;
    end else begin
      Values.Variables[VariableName]:='1';
    end;
  end;
  Result:=true;
end;

function TLinkScanner.UndefDirective: boolean;
// {$undefine name}
var VariableName: string;
begin
  SkipSpace;
  VariableName:=ReadUpperIdentifier;
  if (VariableName<>'') then
    Values.Undefine(VariableName);
  Result:=true;
end;

function TLinkScanner.SetCDirective: boolean;
// {$setc name} or {$setc name:=value}
var VariableName, NewValue: string;
begin
  SkipSpace;
  VariableName:=ReadUpperIdentifier;
  if (VariableName<>'') then begin
    SkipSpace;
    if FMacrosOn and (SrcPos<SrcLen)
    and (Src[SrcPos]=':') and (Src[SrcPos+1]='=')
    then begin
      inc(SrcPos,2);
      SkipSpace;
      NewValue:=copy(Src,SrcPos,CommentInnerEndPos-SrcPos);
      if CompareIdentifiers(PChar(NewValue),'false')=0 then
        NewValue:='0'
      else if CompareIdentifiers(PChar(NewValue),'true')=0 then
        NewValue:='1';
      Values.Variables[VariableName]:=NewValue;
    end else begin
      Values.Variables[VariableName]:='1';
    end;
  end;
  Result:=true;
end;

function TLinkScanner.IncludeDirective: boolean;
// {$i filename} or {$include filename}
// filename can be 'filename with spaces'
var IncFilename: string;
begin
  inc(SrcPos);
  if (Src[SrcPos]<>'%') then begin
    IncFilename:=Trim(copy(Src,SrcPos,CommentInnerEndPos-SrcPos));
    if (IncFilename<>'') and (IncFilename[1]='''')
    and (IncFilename[length(IncFilename)]='''') then
      IncFilename:=copy(IncFilename,2,length(IncFilename)-2);
    if PascalCompiler<>pcDelphi then begin
      // default is fpc behaviour (default extension is .pp)
      if ExtractFileExt(IncFilename)='' then
        IncFilename:=IncFilename+'.pp';
    end else begin
      // delphi understands quoted include files and default extension is .pas
      if ExtractFileExt(IncFilename)='' then
        IncFilename:=IncFilename+'.pas';
    end;
    {$IFDEF ShowUpdateCleanedSrc}
    DebugLn('TLinkScanner.IncludeDirective A IncFilename=',IncFilename,' UpdatePos=',DbgS(CommentEndPos-1));
    {$ENDIF}
    UpdateCleanedSource(CommentEndPos-1);
    // put old position on stack
    PushIncludeLink(CleanedLen,CommentEndPos,Code);
    // load include file
    Result:=IncludeFile(IncFilename);
    if Result then begin
      if (SrcPos<=SrcLen) then
        CommentEndPos:=SrcPos
      else
        ReturnFromIncludeFile;
    end else begin
      PopIncludeLink;
    end;
  end;
  //DebugLn('[TLinkScanner.IncludeDirective] END ',CommentEndPos,',',SrcPos,',',SrcLen);
end;

function TLinkScanner.IncludePathDirective: boolean;
// {$includepath path_addition}
var AddPath, PathDivider: string;
begin
  inc(SrcPos);
  AddPath:=Trim(copy(Src,SrcPos,CommentInnerEndPos-SrcPos));
  PathDivider:=':';
  Values.Variables[ExternalMacroStart+'INCPATH']:=
    Values.Variables[ExternalMacroStart+'INCPATH']+PathDivider+AddPath;
  Result:=true;
end;

function TLinkScanner.LoadSourceCaseLoUp(
  const AFilename: string): pointer;
var
  Path, FileNameOnly: string;
  SecondaryFileNameOnly: String;
begin
  Path:=ExtractFilePath(AFilename);
  if (Path<>'') and (not FilenameIsAbsolute(Path)) then
    Path:=ExpandFilename(Path);
  FileNameOnly:=ExtractFilename(AFilename);
  Result:=nil;
  Result:=FOnLoadSource(Self,TrimFilename(Path+FileNameOnly),true);
  if (Result<>nil) then exit;
  SecondaryFileNameOnly:=lowercase(FileNameOnly);
  if (SecondaryFileNameOnly<>FileNameOnly) then begin
    Result:=FOnLoadSource(Self,TrimFilename(Path+SecondaryFileNameOnly),true);
    if (Result<>nil) then exit;
  end;
  SecondaryFileNameOnly:=UpperCaseStr(FileNameOnly);
  if (SecondaryFileNameOnly<>FileNameOnly) then begin
    Result:=FOnLoadSource(Self,TrimFilename(Path+SecondaryFileNameOnly),true);
    if (Result<>nil) then exit;
  end;
end;

function TLinkScanner.SearchIncludeFile(AFilename: string;
  var NewCode: Pointer; var MissingIncludeFile: TMissingIncludeFile): boolean;
var PathStart, PathEnd: integer;
  IncludePath, PathDivider, CurPath: string;
  ExpFilename: string;
  SecondaryFilename: String;
  HasPathDelims: Boolean;

  function SearchPath(const APath: string): boolean;
  begin
    Result:=false;
    if APath='' then exit;
    if APath[length(APath)]<>PathDelim then
      ExpFilename:=APath+PathDelim+AFilename
    else
      ExpFilename:=APath+AFilename;
    if not FilenameIsAbsolute(ExpFilename) then
      ExpFilename:=ExtractFilePath(FMainSourceFilename)+ExpFilename;
    NewCode:=LoadSourceCaseLoUp(ExpFilename);
    Result:=NewCode<>nil;
  end;
  
  procedure SetMissingIncludeFile;
  begin
    if MissingIncludeFile=nil then
      MissingIncludeFile:=TMissingIncludeFile.Create(AFilename,'');
    MissingIncludeFile.IncludePath:=IncludePath;
  end;

begin
  {$IFDEF VerboseIncludeSearch}
  DebugLn('TLinkScanner.SearchIncludeFile Filename="',AFilename,'"');
  {$ENDIF}
  IncludePath:='';
  if not Assigned(FOnLoadSource) then begin
    NewCode:=nil;
    SetMissingIncludeFile;
    Result:=false;
    exit;
  end;
  // if include filename is absolute then load it directly
  if FilenameIsAbsolute(AFilename) then begin
    NewCode:=LoadSourceCaseLoUp(AFilename);
    Result:=(NewCode<>nil);
    if not Result then SetMissingIncludeFile;
    exit;
  end;

  // include filename is relative
  // beware of 'dir/file.inc'
  HasPathDelims:=(System.Pos('/',AFilename)>0) or (System.Pos('\',AFilename)>0);
  if HasPathDelims then
    DoDirSeparators(AFilename);

  // first search include file in the directory of the main source
  {$IFDEF VerboseIncludeSearch}
  DebugLn('TLinkScanner.SearchIncludeFile MainSourceFilename="',FMainSourceFilename,'"');
  {$ENDIF}
  if FilenameIsAbsolute(FMainSourceFilename) then begin
    // main source has absolute filename
    ExpFilename:=ExtractFilePath(FMainSourceFilename)+AFilename;
    NewCode:=LoadSourceCaseLoUp(ExpFilename);
    Result:=(NewCode<>nil);
    if Result then exit;
  end else if (not HasPathDelims) then begin
    // main source has relative filename (= virtual)
    NewCode:=FOnLoadSource(Self,TrimFilename(AFilename),true);
    if NewCode=nil then begin
      SecondaryFilename:=lowercase(AFilename);
      if SecondaryFilename<>AFilename then
        NewCode:=FOnLoadSource(Self,TrimFilename(SecondaryFilename),true);
    end;
    if NewCode=nil then begin
      SecondaryFilename:=UpperCaseStr(AFilename);
      if SecondaryFilename<>AFilename then
        NewCode:=FOnLoadSource(Self,TrimFilename(SecondaryFilename),true);
    end;
    Result:=(NewCode<>nil);
    if Result then exit;
  end;
  
  // then search the include file in the include path
  if not HasPathDelims then begin
    if MissingIncludeFile=nil then
      IncludePath:=Values.Variables[ExternalMacroStart+'INCPATH']
    else
      IncludePath:=MissingIncludeFile.IncludePath;

    if Values.IsDefined('DELPHI') then
      PathDivider:=':'
    else
      PathDivider:=':;';
    {$IFDEF VerboseIncludeSearch}
    DebugLn('TLinkScanner.SearchIncludeFile IncPath="',IncludePath,'" PathDivider="',PathDivider,'"');
    {$ENDIF}
    PathStart:=1;
    PathEnd:=PathStart;
    while PathEnd<=length(IncludePath) do begin
      if ((Pos(IncludePath[PathEnd],PathDivider))>0)
      {$IFDEF MSWindows}
      and (not ((PathEnd-PathStart=1) // ignore colon in drive
            and (IncludePath[PathEnd]=':')
            and (IsWordChar[IncludePath[PathEnd-1]])))
      {$ENDIF}
      then begin
        if PathEnd>PathStart then begin
          CurPath:=TrimFilename(copy(IncludePath,PathStart,PathEnd-PathStart));
          Result:=SearchPath(CurPath);
          if Result then exit;
        end;
        PathStart:=PathEnd+1;
        PathEnd:=PathStart;
      end else
        inc(PathEnd);
    end;
    if PathEnd>PathStart then begin
      CurPath:=TrimFilename(copy(IncludePath,PathStart,PathEnd-PathStart));
      Result:=SearchPath(CurPath);
      if Result then exit;
    end;
  end;
  
  SetMissingIncludeFile;
end;

function TLinkScanner.IncludeFile(const AFilename: string): boolean;
var
  NewCode: Pointer;
  MissingIncludeFile: TMissingIncludeFile;
begin
  MissingIncludeFile:=nil;
  Result:=SearchIncludeFile(AFilename, NewCode, MissingIncludeFile);
  if Result then begin
    // change source
    if Assigned(FOnIncludeCode) then
      FOnIncludeCode(FMainCode,NewCode);
    SetSource(NewCode);
    AddLink(CleanedLen+1,SrcPos,Code);
  end else begin
    if MissingIncludeFile<>nil then begin
      if FMissingIncludeFiles=nil then
        FMissingIncludeFiles:=TMissingIncludeFiles.Create;
      FMissingIncludeFiles.Add(MissingIncludeFile);
    end;
    if (not IgnoreMissingIncludeFiles) then begin
      RaiseExceptionFmt(ctsIncludeFileNotFound,[AFilename])
    end else begin
      // add a dummy link
      AddLink(CleanedLen+1,SrcPos,MissingIncludeFileCode);
      AddLink(CleanedLen+1,SrcPos,Code);
    end;
  end;
end;

function TLinkScanner.IfDirective: boolean;
// {$if expression} or indirectly called by {$elseif expression}
var Expr, ResultStr: string;
begin
  inc(IfLevel);
  inc(SrcPos);
  Expr:=UpperCaseStr(copy(Src,SrcPos,CommentInnerEndPos-SrcPos));
  ResultStr:=Values.Eval(Expr);
  Result:=true;
  if Values.ErrorPosition>=0 then begin
    inc(SrcPos,Values.ErrorPosition);
    RaiseException(ctsErrorInDirectiveExpression)
  end else if ResultStr='0' then
    SkipTillEndifElse
end;

function TLinkScanner.IfOptDirective: boolean;
// {$ifopt o+} or {$ifopt o-}
var Option, c: char;
begin
  inc(IfLevel);
  inc(SrcPos);
  Option:=UpChars[Src[SrcPos]];
  if (IsWordChar[Option]) and (CompilerSwitchesNames[Option]<>'')
  then begin
    inc(SrcPos);
    if (SrcPos<=SrcLen) then begin
      c:=Src[SrcPos];
      if c in ['+','-'] then begin
        if (c='-')<>(Values.Variables[CompilerSwitchesNames[Option]]='0') then
          SkipTillEndifElse;
      end;
    end;
  end;
  Result:=true;
end;

procedure TLinkScanner.SetIgnoreMissingIncludeFiles(const Value: boolean);
begin
  FIgnoreMissingIncludeFiles := Value;
end;

procedure TLinkScanner.PushIncludeLink(ACleanedPos, ASrcPos: integer;
  ACode: pointer);
  
  procedure RaiseIncludeCircleDetected;
  begin
    RaiseException(ctsIncludeCircleDetected);
  end;
  
var NewLink: PSourceLink;
  i: integer;
begin
  for i:=0 to FIncludeStack.Count-1 do
    if PSourceLink(FIncludeStack[i])^.Code=ACode then
      RaiseIncludeCircleDetected;
  NewLink:=PSourceLinkMemManager.NewPSourceLink;
  with NewLink^ do begin
    CleanedPos:=ACleanedPos;
    SrcPos:=ASrcPos;
    Code:=ACode;
  end;
  FIncludeStack.Add(NewLink);
end;

function TLinkScanner.PopIncludeLink: TSourceLink;
var PLink: PSourceLink;
begin
  PLink:=PSourceLink(FIncludeStack[FIncludeStack.Count-1]);
  Result:=PLink^;
  PSourceLinkMemManager.DisposePSourceLink(PLink);
  FIncludeStack.Delete(FIncludeStack.Count-1);
end;

function TLinkScanner.GetIncludeFileIsMissing: boolean;
begin
  Result:=(FMissingIncludeFiles<>nil);
end;

function TLinkScanner.MissingIncludeFilesNeedsUpdate: boolean;
var
  i: integer;
  MissingIncludeFile: TMissingIncludeFile;
  NewCode: Pointer;
begin
  Result:=false;
  if (not IncludeFileIsMissing) or IgnoreMissingIncludeFiles then exit;
  { last scan missed an include file (i.e. was not in searchpath)
    -> Check all missing include files again }
  for i:=0 to FMissingIncludeFiles.Count-1 do begin
    MissingIncludeFile:=FMissingIncludeFiles[i];
    if SearchIncludeFile(MissingIncludeFile.Filename,NewCode,MissingIncludeFile)
    then begin
      Result:=true;
      exit;
    end;
  end;
end;

procedure TLinkScanner.ClearMissingIncludeFiles;
begin
  FreeAndNil(FMissingIncludeFiles);
end;

function TLinkScanner.ReturnFromIncludeFile: boolean;
var OldPos: TSourceLink;
begin
  if not FSkippingTillEndif then begin
    {$IFDEF ShowUpdateCleanedSrc}
    DebugLn('TLinkScanner.ReturnFromIncludeFile A UpdatePos=',DbgS(SrcPos-1));
    {$ENDIF}
    UpdateCleanedSource(SrcPos-1);
  end;
  while SrcPos>SrcLen do begin
    Result:=FIncludeStack.Count>0;
    if not Result then exit;
    OldPos:=PopIncludeLink;
    SetSource(OldPos.Code);
    SrcPos:=OldPos.SrcPos;
    LastCleanSrcPos:=SrcPos-1;
    AddLink(CleanedLen+1,SrcPos,Code);
  end;
  Result:=SrcPos<=SrcLen;
end;

procedure TLinkScanner.InitKeyWordList;
begin
  if KeywordFuncList<>nil then exit;
  KeywordFuncList:=TKeyWordFunctionList.Create;
  with KeywordFuncList do begin
    Add('END'            ,@DoEndToken);
    Add('IMPLEMENTATION' ,@DoEndOfInterfaceToken);
    Add('INITIALIZIATION',@DoEndOfInterfaceToken);
    Add('FINALIZATION'   ,@DoEndOfInterfaceToken);
    DefaultKeyWordFunction:=@DoDefaultIdentToken;
  end;
end;

function TLinkScanner.DoEndToken: boolean;
begin
  TokenType:=lsttEnd;
  Result:=true;
end;

function TLinkScanner.DoDefaultIdentToken: boolean;
begin
  TokenType:=lsttIdentifier;
  Result:=true;
end;

function TLinkScanner.DoEndOfInterfaceToken: boolean;
begin
  TokenType:=lsttEndOfInterface;
  Result:=true;
end;

procedure TLinkScanner.SkipTillEndifElse;
var OldDirectiveFuncList: TKeyWordFunctionList;
  c1: Char;
begin
  SrcPos:=CommentEndPos;
  {$IFDEF ShowUpdateCleanedSrc}
  DebugLn('TLinkScanner.SkipTillEndifElse A UpdatePos=',DbgS(SrcPos-1));
  {$ENDIF}
  UpdateCleanedSource(SrcPos-1);
  OldDirectiveFuncList:=FDirectiveFuncList;
  FDirectiveFuncList:=FSkipDirectiveFuncList;
  try
    // parse till $else, $elseif or $endif without adding the code to FCleanedSrc
    FSkippingTillEndif:=true;
    FSkipIfLevel:=IfLevel;
    if (SrcPos<=SrcLen) then begin
      while true do begin
        c1:=Src[SrcPos];
        if IsCommentStartChar[c1] then begin
          case c1 of
            '{': begin
                   SkipComment;
                   if not FSkippingTillEndif then break;
                 end;
            '/': if (Src[SrcPos+1]='/') then begin
                   SkipDelphiComment;
                   if not FSkippingTillEndif then break;
                 end else
                   inc(SrcPos);
            '(': if (Src[SrcPos+1]='*') then begin
                   SkipOldTPComment;
                   if not FSkippingTillEndif then break;
                 end else
                   inc(SrcPos);
          end;
        end else if c1='''' then begin
          // skip string constant
          inc(SrcPos);
          while (SrcPos<=SrcLen) and (Src[SrcPos]<>'''') do inc(SrcPos);
          inc(SrcPos);
        end else begin
          inc(SrcPos);
          if (SrcPos>SrcLen) and not ReturnFromIncludeFile then
            break;
        end;
      end;
    end;
    LastCleanSrcPos:=CommentStartPos-1;
    AddLink(CleanedLen+1,CommentStartPos,Code);
    {$IFDEF ShowUpdateCleanedSrc}
    DebugLn('TLinkScanner.SkipTillEndifElse B Continuing after: ',
      '"',StringToPascalConst(copy(Src,LastCleanSrcPos+1,20)),'"');
    {$ENDIF}
  finally
    FDirectiveFuncList:=OldDirectiveFuncList;
    FSkippingTillEndif:=false;
  end;
end;

procedure TLinkScanner.SkipTillEndCifElse;
var OldDirectiveFuncList: TKeyWordFunctionList;
  c1: Char;
begin
  SrcPos:=CommentEndPos;
  {$IFDEF ShowUpdateCleanedSrc}
  DebugLn('TLinkScanner.SkipTillEndCifElse A UpdatePos=',DbgS(SrcPos-1));
  {$ENDIF}
  UpdateCleanedSource(SrcPos-1);
  OldDirectiveFuncList:=FDirectiveFuncList;
  FDirectiveFuncList:=FSkipDirectiveFuncList;
  try
    // parse till $elsec, $elifc or $endc without adding the code to FCleanedSrc
    FSkippingTillEndif:=true;
    FSkipIfLevel:=IfLevel;
    if (SrcPos<=SrcLen) then begin
      while true do begin
        c1:=Src[SrcPos];
        if IsCommentStartChar[c1] then begin
          case c1 of
            '{': begin
                   SkipComment;
                   if not FSkippingTillEndif then break;
                 end;
            '/': if (Src[SrcPos+1]='/') then begin
                   SkipDelphiComment;
                   if not FSkippingTillEndif then break;
                 end else
                   inc(SrcPos);
            '(': if (Src[SrcPos+1]='*') then begin
                   SkipOldTPComment;
                   if not FSkippingTillEndif then break;
                 end else
                   inc(SrcPos);
          end;
        end else if c1='''' then begin
          // skip string constant
          inc(SrcPos);
          while (SrcPos<=SrcLen) and (Src[SrcPos]<>'''') do inc(SrcPos);
          inc(SrcPos);
        end else begin
          inc(SrcPos);
          if (SrcPos>SrcLen) and not ReturnFromIncludeFile then
            break;
        end;
      end;
    end;
    LastCleanSrcPos:=CommentStartPos-1;
    AddLink(CleanedLen+1,CommentStartPos,Code);
    {$IFDEF ShowUpdateCleanedSrc}
    DebugLn('TLinkScanner.SkipTillEndifElse B Continuing after: ',
      '"',StringToPascalConst(copy(Src,LastCleanSrcPos+1,20)),'"');
    {$ENDIF}
  finally
    FDirectiveFuncList:=OldDirectiveFuncList;
    FSkippingTillEndif:=false;
  end;
end;

procedure TLinkScanner.SetCompilerMode(const AValue: TCompilerMode);
begin
  if FCompilerMode=AValue then exit;
  FCompilerMode:=AValue;
  FNestedComments:=(PascalCompiler=pcFPC)
                   and (FCompilerMode in [cmFPC,cmOBJFPC]);
end;

function TLinkScanner.SkipIfDirective: boolean;
begin
  inc(IfLevel);
  Result:=true;
end;

function TLinkScanner.CursorToCleanPos(ACursorPos: integer; ACode: pointer;
  var ACleanPos: integer): integer;
// 0=valid CleanPos
//-1=CursorPos was skipped, CleanPos is between two links
// 1=CursorPos beyond scanned code
var
  i, j, SkippedCleanPos: integer;
  SkippedPos: boolean;
begin
  i:=0;
  SkippedPos:=false;
  SkippedCleanPos:=-1;
  while i<LinkCount do begin
    //DebugLn('[TLinkScanner.CursorToCleanPos] A ACursorPos=',ACursorPos,', Code=',Links[i].Code=ACode,', Links[i].SrcPos=',Links[i].SrcPos,', Links[i].CleanedPos=',Links[i].CleanedPos);
    if (FLinks[i].Code=ACode) and (FLinks[i].SrcPos<=ACursorPos) then begin
      // link in same code found
      ACleanPos:=ACursorPos-FLinks[i].SrcPos+FLinks[i].CleanedPos;
      //DebugLn('[TLinkScanner.CursorToCleanPos] B ACleanPos=',ACleanPos);
      if i+1<LinkCount then begin
        // link has successor
        //DebugLn(['[TLinkScanner.CursorToCleanPos] C Links[i+1].CleanedPos=',Links[i+1].CleanedPos]);
        if ACleanPos<FLinks[i+1].CleanedPos then begin
          // link covers the cursor position
          Result:=0;  // valid position
          exit;
        end;
        // set found cleanpos to end of link
        ACleanPos:=FLinks[i].CleanedPos+LinkSize(i);
        // link does not cover the cursor position
        // find the next link in the same code
        j:=i+1;
        while (j<LinkCount) and (FLinks[j].Code<>ACode) do inc(j);
        //DebugLn('[TLinkScanner.CursorToCleanPos] D j=',j);
        if (j<LinkCount) and (FLinks[j].SrcPos>ACursorPos) then begin
          if not SkippedPos then begin
            // CursorPos was skipped, CleanPos is between two links
            // but because include files can be parsed multiple times,
            // search must continue
            SkippedPos:=true;
            SkippedCleanPos:=ACleanPos;
          end;
          // if this is an double included file,
          // this position can be in clean code -> search next
        end;
        // search next
        i:=j-1;
      end else begin
        // in last link
        //DebugLn(['[TLinkScanner.CursorToCleanPos] E ACleanPos=',ACleanPos,' CleanedLen=',CleanedLen]);
        if ACleanPos<=CleanedLen then begin
          Result:=0;  // valid position
          exit;
        end;
        break;
      end;
    end;
    inc(i);
  end;
  if SkippedPos then begin
    Result:=-1;
    ACleanPos:=SkippedCleanPos;
  end else
    Result:=1; // default: CursorPos beyond/outside scanned code
end;

function TLinkScanner.CleanedPosToCursor(ACleanedPos: integer;
  var ACursorPos: integer; var ACode: Pointer): boolean;

  procedure ConsistencyCheckI(i: integer);
  begin
    raise Exception.Create(
      'TLinkScanner.CleanedPosToCursor Consistency-Error '+IntToStr(i));
  end;

var l,r,m: integer;
begin
  Result:=(ACleanedPos>=1) and (ACleanedPos<=CleanedLen);
  if Result then begin
    // ACleanedPos in Cleaned Code -> binary search through the links
    l:=0;
    r:=LinkCount-1;
    while l<=r do begin
      m:=(l+r) div 2;
      if m<LinkCount-1 then begin
        if ACleanedPos<FLinks[m].CleanedPos then
          r:=m-1
        else if ACleanedPos>=FLinks[m+1].CleanedPos then
          l:=m+1
        else begin
          ACode:=FLinks[m].Code;
          ACursorPos:=ACleanedPos-FLinks[m].CleanedPos+FLinks[m].SrcPos;
          exit;
        end;
      end else begin
        if ACleanedPos>=FLinks[m].CleanedPos then begin
          ACode:=FLinks[m].Code;
          ACursorPos:=ACleanedPos-FLinks[m].CleanedPos+FLinks[m].SrcPos;
          exit;
        end else
          ConsistencyCheckI(2);
      end;
    end;
    ConsistencyCheckI(1);
  end;
end;

function TLinkScanner.WholeRangeIsWritable(CleanStartPos, CleanEndPos: integer;
  ErrorOnFail: boolean): boolean;
  
  procedure EditError(const AMessage: string; ACode: Pointer);
  begin
    if ErrorOnFail then
      RaiseEditException(AMessage,ACode,0);
  end;
  
var
  ACode: Pointer;
  LinkIndex: integer;
  CodeIsReadOnly: boolean;
begin
  Result:=false;
  if (CleanStartPos<1) or (CleanStartPos>=CleanEndPos)
  or (CleanEndPos>CleanedLen+1) or (not Assigned(FOnGetSourceStatus)) then begin
    EditError('TLinkScanner.WholeRangeIsWritable: Invalid range',nil);
    exit;
  end;
  LinkIndex:=LinkIndexAtCleanPos(CleanStartPos);
  if LinkIndex<0 then begin
    EditError('TLinkScanner.WholeRangeIsWritable: position out of scan range',nil);
    exit;
  end;
  ACode:=FLinks[LinkIndex].Code;
  FOnGetSourceStatus(Self,ACode,CodeIsReadOnly);
  if CodeIsReadOnly then begin
    EditError(ctsfileIsReadOnly, ACode);
    exit;
  end;
  repeat
    inc(LinkIndex);
    if (LinkIndex>=LinkCount) or (FLinks[LinkIndex].CleanedPos>CleanEndPos) then
    begin
      Result:=true;
      exit;
    end;
    if ACode<>FLinks[LinkIndex].Code then begin
      ACode:=FLinks[LinkIndex].Code;
      FOnGetSourceStatus(Self,ACode,CodeIsReadOnly);
      if CodeIsReadOnly then begin
        EditError(ctsfileIsReadOnly, ACode);
        exit;
      end;
    end;
  until false;
end;

procedure TLinkScanner.FindCodeInRange(CleanStartPos, CleanEndPos: integer;
  UniqueSortedCodeList: TFPList);
var ACode: Pointer;
  LinkIndex: integer;
begin
  if (CleanStartPos<1) or (CleanStartPos>CleanEndPos)
  or (CleanEndPos>CleanedLen+1) or (UniqueSortedCodeList=nil) then exit;
  LinkIndex:=LinkIndexAtCleanPos(CleanStartPos);
  if LinkIndex<0 then exit;
  ACode:=FLinks[LinkIndex].Code;
  AddCodeToUniqueList(ACode,UniqueSortedCodeList);
  repeat
    inc(LinkIndex);
    if (LinkIndex>=LinkCount) or (FLinks[LinkIndex].CleanedPos>CleanEndPos) then
      exit;
    if ACode<>FLinks[LinkIndex].Code then begin
      ACode:=FLinks[LinkIndex].Code;
      AddCodeToUniqueList(ACode,UniqueSortedCodeList);
    end;
  until false;
end;

procedure TLinkScanner.DeleteRange(CleanStartPos,CleanEndPos: integer);
{ delete all code in links (=parsed code) starting with the last link
  before you call this, test with WholeRangeIsWritable

  this can do unexpected things if
    - include files are included twice
    - compiler directives like IFDEF - ENDIF are partially destroyed
    
  ToDo: keep include directives
}
var LinkIndex, StartPos, Len, aLinkSize: integer;
begin
  if (CleanStartPos<1) or (CleanStartPos>=CleanEndPos)
  or (CleanEndPos>CleanedLen+1) or (not Assigned(FOnDeleteSource)) then exit;
  LinkIndex:=LinkIndexAtCleanPos(CleanEndPos-1);
  while LinkIndex>=0 do begin
    StartPos:=CleanStartPos-FLinks[LinkIndex].CleanedPos;
    if StartPos<0 then StartPos:=0;
    aLinkSize:=LinkSize(LinkIndex);
    if CleanEndPos<FLinks[LinkIndex].CleanedPos+aLinkSize then
      Len:=CleanEndPos-FLinks[LinkIndex].CleanedPos-StartPos
    else
      Len:=aLinkSize-StartPos;
    inc(StartPos,FLinks[LinkIndex].SrcPos);
    FOnDeleteSource(Self,FLinks[LinkIndex].Code,StartPos,Len);
    if FLinks[LinkIndex].CleanedPos<=CleanStartPos then break;
    dec(LinkIndex);
  end;
end;

procedure TLinkScanner.ActivateGlobalWriteLock;
begin
  if Assigned(OnSetGlobalWriteLock) then OnSetGlobalWriteLock(true);
end;

procedure TLinkScanner.DeactivateGlobalWriteLock;
begin
  if Assigned(OnSetGlobalWriteLock) then OnSetGlobalWriteLock(false);
end;

procedure TLinkScanner.RaiseExceptionFmt(const AMessage: string;
  args: array of const);
begin
  RaiseException(Format(AMessage,args));
end;

procedure TLinkScanner.RaiseException(const AMessage: string);
begin
  RaiseExceptionClass(AMessage,ELinkScannerError);
end;

procedure TLinkScanner.RaiseExceptionClass(const AMessage: string;
  ExceptionClass: ELinkScannerErrors);
begin
  LastErrorMessage:=AMessage;
  LastErrorSrcPos:=SrcPos;
  LastErrorCode:=Code;
  LastErrorCheckedForIgnored:=false;
  LastErrorIsValid:=true;
  raise ExceptionClass.Create(Self,AMessage);
end;

procedure TLinkScanner.RaiseEditException(const AMessage: string;
  ABuffer: Pointer; ABufferPos: integer);
begin
  raise ELinkScannerEditError.Create(Self,AMessage,ABuffer,ABufferPos);
end;

procedure TLinkScanner.ClearLastError;
begin
  LastErrorIsValid:=false;
  LastErrorCheckedForIgnored:=false;
end;

procedure TLinkScanner.RaiseLastError;
begin
  SrcPos:=LastErrorSrcPos;
  Code:=LastErrorCode;
  RaiseException(LastErrorMessage);
end;

procedure TLinkScanner.DoCheckAbort;
begin
  if not Assigned(OnProgress) then exit;
  if OnProgress(Self) then exit;
  // mark scanning results as invalid
  FForceUpdateNeeded:=true;
  // raise abort exception
  RaiseExceptionClass('Abort',ELinkScannerAbort);
end;

function TLinkScanner.MainFilename: string;
begin
  if Assigned(OnGetFileName) and (Code<>nil) then
    Result:=OnGetFileName(Self,Code)
  else
    Result:='';
end;

{ ELinkScannerError }

constructor ELinkScannerError.Create(ASender: TLinkScanner;
  const AMessage: string);
begin
  inherited Create(AMessage);
  Sender:=ASender;
end;

{ TPSourceLinkMemManager }

procedure TPSourceLinkMemManager.FreeFirstItem;
var Link: PSourceLink;
begin
  Link:=PSourceLink(FFirstFree);
  PSourceLink(FFirstFree):=Link^.Next;
  Dispose(Link);
end;

procedure TPSourceLinkMemManager.DisposePSourceLink(Link: PSourceLink);
begin
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add Link to Free list
    FillChar(Link^,SizeOf(TSourceLink),0);
    Link^.Next:=PSourceLink(FFirstFree);
    PSourceLink(FFirstFree):=Link;
    inc(FFreeCount);
  end else begin
    // free list full -> free Link
    Dispose(Link);
    {$IFDEF DebugCTMemManager}
    inc(FFreedCount);
    {$ENDIF}
  end;
  dec(FCount);
end;

function TPSourceLinkMemManager.NewPSourceLink: PSourceLink;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=PSourceLink(FFirstFree);
    PSourceLink(FFirstFree):=Result^.Next;
    Result^.Next:=nil;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new PSourceLink
    New(Result);
    FillChar(Result^,SizeOf(TSourceLink),0);
    {$IFDEF DebugCTMemManager}
    inc(FAllocatedCount);
    {$ENDIF}
  end;
  inc(FCount);
end;

{ TPSourceChangeStep }

procedure TPSourceChangeStepMemManager.FreeFirstItem;
var Step: PSourceChangeStep;
begin
  Step:=PSourceChangeStep(FFirstFree);
  PSourceChangeStep(FFirstFree):=Step^.Next;
  Dispose(Step);
end;

procedure TPSourceChangeStepMemManager.DisposePSourceChangeStep(
  Step: PSourceChangeStep);
begin
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add Link to Free list
    FillChar(Step^,SizeOf(TSourceChangeStep),0);
    Step^.Next:=PSourceChangeStep(FFirstFree);
    PSourceChangeStep(FFirstFree):=Step;
    inc(FFreeCount);
  end else begin
    // free list full -> free Step
    Dispose(Step);
    {$IFDEF DebugCTMemManager}
    inc(FFreedCount);
    {$ENDIF}
  end;
  dec(FCount);
end;

function TPSourceChangeStepMemManager.NewPSourceChangeStep: PSourceChangeStep;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=PSourceChangeStep(FFirstFree);
    PSourceChangeStep(FFirstFree):=Result^.Next;
    Result^.Next:=nil;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new PSourceChangeStep
    New(Result);
    FillChar(Result^,SizeOf(TSourceChangeStep),0);
    {$IFDEF DebugCTMemManager}
    inc(FAllocatedCount);
    {$ENDIF}
  end;
  inc(FCount);
end;

{ TMissingIncludeFile }

constructor TMissingIncludeFile.Create(const AFilename, AIncludePath: string);
begin
  inherited Create;
  Filename:=AFilename;
  IncludePath:=AIncludePath;
end;

{ TMissingIncludeFiles }

function TMissingIncludeFiles.GetIncFile(Index: Integer): TMissingIncludeFile;
begin
  Result:=TMissingIncludeFile(Get(Index));
end;

procedure TMissingIncludeFiles.SetIncFile(Index: Integer;
  const AValue: TMissingIncludeFile);
begin
  Put(Index,AValue);
end;

procedure TMissingIncludeFiles.Clear;
var i: integer;
begin
  for i:=0 to Count-1 do Items[i].Free;
  inherited Clear;
end;

procedure TMissingIncludeFiles.Delete(Index: Integer);
begin
  Items[Index].Free;
  inherited Delete(Index);
end;


//------------------------------------------------------------------------------
procedure InternalInit;
var
  CompMode: TCompilerMode;
begin
  for CompMode:=Low(TCompilerMode) to High(TCompilerMode) do
    CompilerModeVars[CompMode]:='FPC_'+CompilerModeNames[CompMode];
  PSourceLinkMemManager:=TPSourceLinkMemManager.Create;
  PSourceChangeStepMemManager:=TPSourceChangeStepMemManager.Create;
end;

procedure InternalFinal;
begin
  PSourceChangeStepMemManager.Free;
  PSourceLinkMemManager.Free;
end;

{ ELinkScannerEditError }

constructor ELinkScannerEditError.Create(ASender: TLinkScanner;
  const AMessage: string; ABuffer: Pointer; ABufferPos: integer);
begin
  inherited Create(ASender,AMessage);
  Buffer:=ABuffer;
  BufferPos:=ABufferPos;
end;

initialization
  InternalInit;
  
finalization
  InternalFinal;

end.

