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
    The link list makes it possible to change scanned code in the original
    files.

  ToDo:
    - macros
}
unit LinkScanner;

{$ifdef FPC} {$mode objfpc} {$endif}{$H+}

{$I codetools.inc}

interface

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, ExprEval, SourceLog, KeywordFuncLists;

type
//----------------------------------------------------------------------------
  TOnGetSource = function(Sender: TObject; Code: Pointer): TSourceLog
                 of object;
  TOnLoadSource = function(Sender: TObject; const AFilename: string): pointer
                       of object;
  TOnGetSourceStatus = procedure(Sender: TObject; Code: Pointer;
                 var ReadOnly: boolean) of object;
  TOnDeleteSource = procedure(Sender: TObject; Code: Pointer; Pos, Len: integer)
                    of object;
  TOnGetFileName = function(Sender: TObject; Code: Pointer): string of object;
  TOnCheckFileOnDisk = function(Code: Pointer): boolean of object;
  TOnGetInitValues = function(Code: Pointer): TExpressionEvaluator of object;
  TOnIncludeCode = procedure(ParentCode, IncludeCode: Pointer) of object;

  TSourceLink = record
    CleanedPos: integer;
    SrcPos: integer;
    Code: Pointer;
  end;
  PSourceLink = ^TSourceLink;

  TSourceChangeStep = record
    Code: Pointer;
    ChangeStep: integer;
  end;
  PSourceChangeStep = ^TSourceChangeStep;

  TCommentStyle = (CommentNone, CommentTP, CommentOldTP, CommentDelphi);

  TCompilerMode = (cmFPC, cmDELPHI, cmGPC, cmTP, cmOBJFPC);

  TLinkScanner = class(TObject)
  private
    FLinks: TList; // list of PSourceLink
    FCleanedSrc: string;
    FOnGetSource: TOnGetSource;
    FOnGetFileName: TOnGetFileName;
    FOnGetSourceStatus: TOnGetSourceStatus;
    FOnLoadSource: TOnLoadSource;
    FOnDeleteSource: TOnDeleteSource;
    FOnCheckFileOnDisk: TOnCheckFileOnDisk;
    FOnGetInitValues: TOnGetInitValues;
    FOnIncludeCode: TOnIncludeCode;
    FInitValues: TExpressionEvaluator;
    FSourceChangeSteps: TList; // list of PSourceChangeStep sorted with Code
    FChangeStep: integer;
    FMainSourceFilename: string;
    FMainCode: pointer;
    FScanTillInterfaceEnd: boolean;
    FIgnoreMissingIncludeFiles: boolean;
    FNestedComments: boolean;
    FForceUpdateNeeded: boolean;
    function GetLinks(Index: integer): TSourceLink;
    procedure SetLinks(Index: integer; const Value: TSourceLink);
    procedure SetSource(ACode: Pointer); // set current source
    procedure AddSourceChangeStep(ACode: pointer; AChangeStep: integer);
    procedure AddLink(ACleanedPos, ASrcPos: integer; ACode: Pointer);
    procedure IncreaseChangeStep;
    procedure SetMainCode(const Value: pointer);
    procedure SetScanTillInterfaceEnd(const Value: boolean);
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
    procedure ReadNextToken;
    function ReadIdentifier: string;
    function ReadUpperIdentifier: string;
    procedure SkipSpace;
    procedure SkipComment;
    procedure SkipDelphiComment;
    procedure SkipOldTPComment;
    procedure EndComment;
    procedure IncCommentLevel;
    procedure DecCommentLevel;
    procedure HandleDirectives;
    procedure UpdateCleanedSource(SourcePos: integer);
    function ReturnFromIncludeFile: boolean;
  private
    // directives
    FDirectiveName: shortstring;
    FDirectiveFuncList: TKeyWordFunctionList;
    FSkipDirectiveFuncList: TKeyWordFunctionList;
    FMacrosOn: boolean;
    FIncludeStack: TList; // list of TSourceLink
    FSkippingTillEndif: boolean;
    FSkipIfLevel: integer;
    procedure SkipTillEndifElse;
    function SkipIfDirective: boolean;
    function IfdefDirective: boolean;
    function IfndefDirective: boolean;
    function IfDirective: boolean;
    function IfOptDirective: boolean;
    function EndifDirective: boolean;
    function ElseDirective: boolean;
    function DefineDirective: boolean;
    function UndefDirective: boolean;
    function IncludeDirective: boolean;
    function IncludeFile(const AFilename: string): boolean;
    function IncludePathDirective: boolean;
    function ShortSwitchDirective: boolean;
    function ReadNextSwitchDirective: boolean;
    function LongSwitchDirective: boolean;
    function ModeDirective: boolean;
    procedure BuildDirectiveFuncList;
    procedure PushIncludeLink(ACleanedPos, ASrcPos: integer; ACode: Pointer);
    function PopIncludeLink: TSourceLink;
  public
    // current values, positions, source, flags
    CleanedLen: integer;
    Src: string;     // current parsed source
    UpperSrc: string;// current parsed source in uppercase
    SrcPos: integer; // current position
    TokenStart: integer; // start position of current token
    SrcLen: integer; // length of current source
    Code: pointer;   // current code object
    Values: TExpressionEvaluator;

    EndOfInterfaceFound: boolean;
    EndOfSourceFound: boolean;

    property Links[Index: integer]: TSourceLink read GetLinks write SetLinks;
    function LinkCount: integer;
    function LinkIndexAtCleanPos(ACleanPos: integer): integer;
    function LinkSize(Index: integer): integer;
    function CleanedSrc: string;
    function CursorToCleanPos(ACursorPos: integer; ACode: pointer;
        var ACleanPos: integer): integer; // 0=valid CleanPos
              //-1=CursorPos was skipped, CleanPos between two links
              // 1=CursorPos beyond scanned code
    function CleanedPosToCursor(ACleanedPos: integer; var ACursorPos: integer;
        var ACode: Pointer): boolean;
    function WholeRangeIsWritable(CleanStartPos, CleanEndPos: integer): boolean;
    procedure FindCodeInRange(CleanStartPos, CleanEndPos: integer;
        UniqueSortedCodeList: TList);
    procedure DeleteRange(CleanStartPos,CleanEndPos: integer);
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
    property IgnoreMissingIncludeFiles: boolean
        read FIgnoreMissingIncludeFiles write SetIgnoreMissingIncludeFiles;
    property InitialValues: TExpressionEvaluator
        read FInitValues write FInitValues;
    property MainCode: pointer read FMainCode write SetMainCode;
    property NestedComments: boolean read FNestedComments;
    property ScanTillInterfaceEnd: boolean
        read FScanTillInterfaceEnd write SetScanTillInterfaceEnd;
    procedure Scan(TillInterfaceEnd, CheckFilesOnDisk: boolean);
    function UpdateNeeded(OnlyInterfaceNeeded,
        CheckFilesOnDisk: boolean): boolean;
    property ChangeStep: integer read FChangeStep;
    procedure Clear;
    function ConsistencyCheck: integer;
    procedure WriteDebugReport;
    constructor Create;
    destructor Destroy; override;
  end;

  ELinkScannerError = class(Exception);

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
        'FPC', 'DELPHI', 'GPC', 'TP', 'OBJFPC'
     );

var
  IsSpaceChar, IsLineEndChar, IsIdentStartChar, IsIdentChar, IsNumberChar,
  IsCommentStartChar, IsHexNumberChar, IsEqualOperatorStartChar:
    array[char] of boolean;


implementation


{ TLinkScanner }

procedure TLinkScanner.AddLink(ACleanedPos, ASrcPos: integer; ACode: pointer);
var NewLink: PSourceLink;
begin
  New(NewLink);
  with NewLink^ do begin
    CleanedPos:=ACleanedPos;
    SrcPos:=ASrcPos;
    Code:=ACode;
  end;
  FLinks.Add(NewLink);
end;

function TLinkScanner.CleanedSrc: string;
begin
  Result:=copy(FCleanedSrc,1,CleanedLen);
end;

procedure TLinkScanner.Clear;
var i: integer;
  PLink: PSourceLink;
  PStamp: PSourceChangeStep;
begin
  for i:=0 to FIncludeStack.Count-1 do begin
    PLink:=PSourceLink(FIncludeStack[i]);
    Dispose(PLink);
  end;
  FIncludeStack.Clear;
  for i:=0 to LinkCount-1 do begin
    PLink:=PSourceLink(FLinks[i]);
    Dispose(PLink);
  end;
  FLinks.Clear;
  FCleanedSrc:='';
  for i:=0 to FSourceChangeSteps.Count-1 do begin
    PStamp:=PSourceChangeStep(FSourceChangeSteps[i]);
    Dispose(PStamp);
  end;
  FSourceChangeSteps.Clear;
  IncreaseChangeStep;
end;

constructor TLinkScanner.Create;
begin
  inherited Create;
  FLinks:=TList.Create;
  FInitValues:=TExpressionEvaluator.Create;
  Values:=TExpressionEvaluator.Create;
  FChangeStep:=0;
  FSourceChangeSteps:=TList.Create;
  FMainCode:=nil;
  FMainSourceFilename:='';
  BuildDirectiveFuncList;
  FIncludeStack:=TList.Create;
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
  FIncludeStack.Free;
  FSourceChangeSteps.Free;
  Values.Free;
  FInitValues.Free;
  FLinks.Free;
  FDirectiveFuncList.Free;
  FSkipDirectiveFuncList.Free;
  inherited Destroy;
end;

function TLinkScanner.GetLinks(Index: integer): TSourceLink;
begin
  Result:=PSourceLink(FLinks[Index])^;
end;

function TLinkScanner.LinkSize(Index: integer): integer;
begin
  if (Index<0) or (Index>=LinkCount) then
    raise ELinkScannerError.Create('TLinkScanner.LinkSize  index '
       +IntToStr(Index)+' out of bounds: 0-'+IntToStr(LinkCount));
  if Index<LinkCount-1 then
    Result:=Links[Index+1].CleanedPos-Links[Index].CleanedPos
  else
    Result:=CleanedLen-Links[Index].CleanedPos;
end;

function TLinkScanner.LinkIndexAtCleanPos(ACleanPos: integer): integer;
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
      if ACleanPos<Links[m].CleanedPos then
        r:=m-1
      else if ACleanPos>=Links[m+1].CleanedPos then
        l:=m+1
      else begin
        Result:=m;
        exit;
      end;
    end else begin
      if ACleanPos>=Links[m].CleanedPos then begin
        Result:=m;
        exit;
      end else
        raise Exception.Create(
            'TLinkScanner.LinkAtCleanPos Consistency-Error 2');
    end;
  end;
  raise Exception.Create(
      'TLinkScanner.LinkAtCleanPos Consistency-Error 1');
end;

procedure TLinkScanner.SetSource(ACode: pointer);
var SrcLog: TSourceLog;
begin
  if Assigned(FOnGetSource) then begin
    SrcLog:=FOnGetSource(Self,ACode);
    if SrcLog=nil then
      raise ELinkScannerError.Create('unable to get source with Code='
         +HexStr(Cardinal(Code),8));
    AddSourceChangeStep(ACode,SrcLog.ChangeStep);
    Src:=SrcLog.Source;
    UpperSrc:=UpperCaseStr(SrcLog.Source);
    Code:=ACode;
    SrcPos:=1;
    TokenStart:=1;
    SrcLen:=length(Src);
    LastCleanSrcPos:=0;
  end else begin
    raise ELinkScannerError.Create('unable to get source with Code='
       +HexStr(Cardinal(Code),8));
  end;
end;

procedure TLinkScanner.HandleDirectives;
var DirStart, DirLen: integer;
begin
  SrcPos:=CommentInnerStartPos+1;
  DirStart:=SrcPos;
  while (SrcPos<=SrcLen) and (IsIdentStartChar[UpperSrc[SrcPos]]) do
    inc(SrcPos);
  DirLen:=SrcPos-DirStart;
  if DirLen>255 then DirLen:=255;
  FDirectiveName:=copy(UpperSrc,DirStart,DirLen);
  FDirectiveFuncList.DoIt(FDirectiveName);
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

function TLinkScanner.LinkCount: integer;
begin
  Result:=FLinks.Count;
end;

procedure TLinkScanner.ReadNextToken;
var
  c1, c2: char;
begin
  // Skip all spaces and comments
  if (SrcPos>SrcLen) then ReturnFromIncludeFile;
  while SrcPos<=SrcLen do begin
    if IsCommentStartChar[Src[SrcPos]] then begin
      case Src[SrcPos] of
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
    end else if IsSpaceChar[Src[SrcPos]] then begin
      repeat
        inc(SrcPos);
      until (SrcPos>SrcLen) or (not (IsSpaceChar[Src[SrcPos]]));
    end else
      break;
    if (SrcPos>SrcLen) then ReturnFromIncludeFile;
  end;
  TokenStart:=SrcPos;
  if SrcPos>SrcLen then exit;
  // read token
  c1:=UpperSrc[SrcPos];
  case c1 of
    '_','A'..'Z':
      begin
        inc(SrcPos);
        while (SrcPos<=SrcLen)
        and (IsIdentChar[Src[SrcPos]]) do
          inc(SrcPos);
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
              while (SrcPos<=SrcLen)
              and (Src[SrcPos]<>'''') do
                inc(SrcPos);
              inc(SrcPos);
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
          if (SrcPos<=SrcLen) and (UpperSrc[SrcPos]='E') then begin
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
        and (IsHexNumberChar[UpperSrc[SrcPos]]) do
          inc(SrcPos);
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

procedure TLinkScanner.Scan(TillInterfaceEnd, CheckFilesOnDisk: boolean);
var LastTokenIsEqual, LastTokenIsEnd: boolean;
begin
  if not UpdateNeeded(TillInterfaceEnd,CheckFilesOnDisk) then exit;
{$IFDEF CTDEBUG}
writeln('TLinkScanner.Scan A -------- TillInterfaceEnd=',TillInterfaceEnd);
{$ENDIF}
  ScanTillInterfaceEnd:=TillInterfaceEnd;
  Clear;
  IncreaseChangeStep;
  FCleanedSrc:='';
  CleanedLen:=0;
{$IFDEF CTDEBUG}
writeln('TLinkScanner.Scan B ');
{$ENDIF}
  SetSource(FMainCode);
{$IFDEF CTDEBUG}
writeln('TLinkScanner.Scan C ',SrcLen);
{$ENDIF}
  EndOfInterfaceFound:=false;
  EndOfSourceFound:=false;
  CommentStyle:=CommentNone;
  CommentLevel:=0;
  IfLevel:=0;
  FSkippingTillEndif:=false;
  if Assigned(FOnGetInitValues) then
    FInitValues.Assign(FOnGetInitValues(FMainCode));
//writeln('TLinkScanner.Scan C --------');
  Values.Assign(FInitValues);
//writeln(Values.AsString);
//writeln('TLinkScanner.Scan D --------');
  FMacrosOn:=(Values.Variables['MACROS']<>'0');
  if Src='' then exit;
  AddLink(1,SrcPos,Code);
  LastTokenIsEqual:=false;
  LastTokenIsEnd:=false;
{$IFDEF CTDEBUG}
writeln('TLinkScanner.Scan A ',SrcLen);
{$ENDIF}
  repeat
    ReadNextToken;
    UpdateCleanedSource(SrcPos-1);
    if (SrcPos<=SrcLen+1) then begin
      if (not LastTokenIsEqual) and
        ((UpTokenIs('IMPLEMENTATION')) or (UpTokenIs('INITIALIZATION'))
          or (UpTokenIs('FINALIZATION'))) then
            EndOfInterfaceFound:=true;
      if (LastTokenIsEnd) and (UpTokenIs('.')) then begin
        EndOfInterfaceFound:=true;
        EndOfSourceFound:=true;
        break;
      end;
      LastTokenIsEqual:=TokenIs('=');
      LastTokenIsEnd:=UpTokenIs('END');
    end else
      break;
  until (SrcPos>SrcLen) or (EndOfSourceFound)
  or ((ScanTillInterfaceEnd) and (EndOfInterfaceFound));
  IncreaseChangeStep;
  FForceUpdateNeeded:=false;
{$IFDEF CTDEBUG}
writeln('TLinkScanner.Scan END ',CleanedLen);
{$ENDIF}
end;

procedure TLinkScanner.SetLinks(Index: integer; const Value: TSourceLink);
begin
  PSourceLink(FLinks[Index])^:=Value;
end;

procedure TLinkScanner.SkipComment;
// a normal pascal {} comment
begin
  CommentStyle:=CommentTP;
  CommentStartPos:=SrcPos;
  IncCommentLevel;
  inc(SrcPos);
  CommentInnerStartPos:=SrcPos;
  if SrcPos>SrcLen then exit;
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
  if SrcPos>SrcLen then exit;
  if (Src[SrcPos]='$') then ;
  while (SrcPos<=SrcLen) and (Src[SrcPos]<>#10) do inc(SrcPos);
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
  if SrcPos>SrcLen then exit;
  // ToDo: nested comments
  while (SrcPos<=SrcLen)
  and ((Src[SrcPos-1]<>'*') or (Src[SrcPos]<>')')) do inc(SrcPos);
  inc(SrcPos);
  CommentEndPos:=SrcPos;
  CommentInnerEndPos:=SrcPos-2;
  { handle compiler switches }
  if Src[CommentInnerStartPos]='$' then HandleDirectives;
  EndComment;
end;

procedure TLinkScanner.UpdateCleanedSource(SourcePos: integer);
// add new parsed code to cleaned source string
var AddLen, i: integer;
//  s: string;
begin
  if SourcePos=LastCleanSrcPos then exit;
  if SourcePos>SrcLen then SourcePos:=SrcLen;
  AddLen:=SourcePos-LastCleanSrcPos;
  if AddLen>length(FCleanedSrc)-CleanedLen then begin
    // expand cleaned source string
    i:=length(FCleanedSrc)+1024;
    if AddLen<i then AddLen:=i;
    SetLength(FCleanedSrc,length(FCleanedSrc)+AddLen);
    //SetLength(s,AddLen);
    //FCleanedSrc:=FCleanedSrc+s;
  end;
  for i:=LastCleanSrcPos+1 to SourcePos do begin
    inc(CleanedLen);
    FCleanedSrc[CleanedLen]:=Src[i];
    //write(Src[i]);
  end;
  LastCleanSrcPos:=SourcePos;
end;

procedure TLinkScanner.AddSourceChangeStep(ACode: pointer;AChangeStep: integer);
var l,r,m: integer;
  NewSrcChangeStep: PSourceChangeStep;
  c: pointer;
begin
//writeln('[TLinkScanner.AddSourceChangeStep] ',HexStr(Cardinal(ACode),8));
  if ACode=nil then
    raise ELinkScannerError.Create('TLinkScanner.AddSourceChangeStep ACode=nil');
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
  New(NewSrcChangeStep);
  NewSrcChangeStep^.Code:=ACode;
  NewSrcChangeStep^.ChangeStep:=AChangeStep;
  if (FSourceChangeSteps.Count>0) and (c<ACode) then inc(m);
  FSourceChangeSteps.Insert(m,NewSrcChangeStep);
//writeln('   ADDING ',HexStr(Cardinal(ACode),8),',',FSourceChangeSteps.Count);  
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
        if AToken[i]<>UpperSrc[TokenStart-1+i] then exit;
      Result:=true;
    end;
  end;
end;

function TLinkScanner.ConsistencyCheck: integer;
var i: integer;
  sl: TSourceLink;
begin
  if FLinks<>nil then begin
    for i:=0 to FLinks.Count-1 do begin
      if FLinks[i]=nil then begin
        Result:=-1;  exit;
      end;
      sl:=PSourceLink(FLinks[i])^;
      if sl.Code=nil then begin
        Result:=-2;  exit;
      end;
      if (sl.CleanedPos<1) or (sl.CleanedPos>SrcLen) then begin
        Result:=-3;  exit;
      end;
    end;
  end;
  if SrcLen<>length(Src) then begin // length of current source
    Result:=-4;  exit;
  end;
  if UpperSrc<>UpperCaseStr(Src) then begin
    Result:=-5;  exit;
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
  writeln('');
  writeln('[TLinkScanner.WriteDebugReport]',
     ' ChangeStepCount=',FSourceChangeSteps.Count,
     ' LinkCount=',LinkCount,
     ' CleanedLen=',CleanedLen);
  // time stamps
  for i:=0 to FSourceChangeSteps.Count-1 do begin
    writeln('  ChangeStep ',i,': '
        ,' Code=',HexStr(Cardinal(
                        PSourceChangeStep(FSourceChangeSteps[i])^.Code),8)
        ,' ChangeStep=',PSourceChangeStep(FSourceChangeSteps[i])^.ChangeStep);
  end;
  // links
  for i:=0 to LinkCount-1 do begin
    writeln('  Link ',i,':'
        ,' CleanedPos=',Links[i].CleanedPos
        ,' SrcPos=',Links[i].SrcPos
        ,' Code=',HexStr(Cardinal(Links[i].Code),8)
      );
  end;
end;

function TLinkScanner.UpdateNeeded(
  OnlyInterfaceNeeded, CheckFilesOnDisk: boolean): boolean;
{ the clean source must be rebuild if
   1. scanrange changed from only interface to whole source
   2. unit source changed
   3. one of its include files changed
   4. init values changed (e.g. initial compiler defines)
}
var i: integer;
  SrcLog: TSourceLog;
  NewInitValues: TExpressionEvaluator;
begin
  Result:=true;
  if FForceUpdateNeeded then exit;
  FForceUpdateNeeded:=true;
//writeln('TLinkScanner.UpdateNeeded A OnlyInterface=',OnlyInterfaceNeeded,' EndOfSourceFound=',EndOfSourceFound);
  if LinkCount=0 then exit;
  // check if ScanRange has increased
  if (OnlyInterfaceNeeded=false) and (not EndOfSourceFound) then exit;
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
//writeln('TLinkScanner.UpdateNeeded D ',i,',',PSourceChangeStep(FSourceChangeSteps[i])^.Code<>nil,' ',PSourceChangeStep(FSourceChangeSteps[i])^.ChangeStep,'<>',SrcLog.ChangeStep,'  ',HexStr(Cardinal(SrcLog),8));
      if PSourceChangeStep(FSourceChangeSteps[i])^.ChangeStep<>SrcLog.ChangeStep
      then exit;
    end;
  end;
  // check initvalues
  if Assigned(FOnGetInitValues) then begin
    if FInitValues=nil then exit;
    NewInitValues:=FOnGetInitValues(Code);
    if (NewInitValues<>nil) and (not FInitValues.Equals(NewInitValues)) then
      exit;
  end;
  // no update needed :)
  FForceUpdateNeeded:=false;
//writeln('TLinkScanner.UpdateNeeded END');
  Result:=false;
end;

procedure TLinkScanner.SetMainCode(const Value: pointer);
begin
  if FMainCode=Value then exit;
  FMainCode:=Value;
  FMainSourceFilename:=FOnGetFileName(Self,FMainCode);
  Clear;
end;

procedure TLinkScanner.SetScanTillInterfaceEnd(const Value: boolean);
begin
  if FScanTillInterfaceEnd=Value then exit;
  FScanTillInterfaceEnd := Value;
  if not Value then Clear;
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
    Add('IFNDEF',{$ifdef FPC}@{$endif}IfndefDirective);
    Add('IF',{$ifdef FPC}@{$endif}IfDirective);
    Add('IFOPT',{$ifdef FPC}@{$endif}IfOptDirective);
    Add('ENDIF',{$ifdef FPC}@{$endif}EndIfDirective);
    Add('ELSE',{$ifdef FPC}@{$endif}ElseDirective);
    Add('DEFINE',{$ifdef FPC}@{$endif}DefineDirective);
    Add('UNDEF',{$ifdef FPC}@{$endif}UndefDirective);
    Add('INCLUDE',{$ifdef FPC}@{$endif}IncludeDirective);
    Add('INCLUDEPATH',{$ifdef FPC}@{$endif}IncludePathDirective);
    Add('MODE',{$ifdef FPC}@{$endif}ModeDirective);
  end;
  FSkipDirectiveFuncList:=TKeyWordFunctionList.Create;
  with FSkipDirectiveFuncList do begin
    Add('IFDEF',{$ifdef FPC}@{$endif}SkipIfDirective);
    Add('IFNDEF',{$ifdef FPC}@{$endif}SkipIfDirective);
    Add('IF',{$ifdef FPC}@{$endif}SkipIfDirective);
    Add('IFOPT',{$ifdef FPC}@{$endif}SkipIfDirective);
    Add('ENDIF',{$ifdef FPC}@{$endif}EndIfDirective);
    Add('ELSE',{$ifdef FPC}@{$endif}ElseDirective);
  end;
end;

function TLinkScanner.LongSwitchDirective: boolean;
var ValStart: integer;
  ValueStr: string;
begin
  SkipSpace;
  ValStart:=SrcPos;
  while (SrcPos<=SrcLen) and (UpperSrc[SrcPos] in ['A'..'Z']) do
    inc(SrcPos);
  ValueStr:=copy(UpperSrc,ValStart,SrcPos-ValStart);
  if ValueStr='ON' then
    Values.Variables[FDirectiveName]:='1'
  else if ValueStr='OFF' then
    Values.Variables[FDirectiveName]:='0'
  else if (ValueStr='PRELOAD') and (FDirectiveName='ASSERTIONS') then
    Values.Variables[FDirectiveName]:=ValueStr
  else
    raise ELinkScannerError.Create(
      'invalid flag value "'+ValueStr+'" for directive '+FDirectiveName);
  Result:=ReadNextSwitchDirective;
end;

function TLinkScanner.ModeDirective: boolean;
// $MODE DEFAULT, OBJFPC, TP, FPC, GPC, DELPHI
var ValStart: integer;
  ValueStr: string;
  AMode: TCompilerMode;
  ModeValid: boolean;
begin
  SkipSpace;
  ValStart:=SrcPos;
  while (SrcPos<=SrcLen) and (UpperSrc[SrcPos] in ['A'..'Z']) do
    inc(SrcPos);
  ValueStr:=copy(UpperSrc,ValStart,SrcPos-ValStart);
  // undefine all mode macros
  for AMode:=Low(TCompilerMode) to High(TCompilerMode) do
    Values.Undefine('FPC_'+CompilerModeNames[AMode]);
  // define new mode macro
  if (ValueStr='DEFAULT') then begin
  end else begin
    ModeValid:=false;
    for AMode:=Low(TCompilerMode) to High(TCompilerMode) do
      if CompilerModeNames[AMode]=ValueStr then begin
        Values.Variables['FPC_'+CompilerModeNames[AMode]]:='1';
        ModeValid:=true;
        break;
      end;
    if not ModeValid then
      raise ELinkScannerError.Create('invalid mode "'+ValueStr+'"');
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
    while (SrcPos<=SrcLen) and (IsIdentStartChar[UpperSrc[SrcPos]]) do
      inc(SrcPos);
    DirLen:=SrcPos-DirStart;
    if DirLen>255 then DirLen:=255;
    FDirectiveName:=copy(UpperSrc,DirStart,DirLen);
    Result:=FDirectiveFuncList.DoIt(FDirectiveName);
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

procedure TLinkScanner.SkipSpace;
begin
  while (SrcPos<=SrcLen) and (IsSpaceChar[Src[SrcPos]]) do inc(SrcPos);
end;

function TLinkScanner.ReadIdentifier: string;
var StartPos: integer;
begin
  StartPos:=SrcPos;
  if (SrcPos<=SrcLen) and (IsIdentStartChar[UpperSrc[SrcPos]]) then begin
    inc(SrcPos);
    while (SrcPos<=SrcLen) and (IsIdentChar[UpperSrc[SrcPos]]) do
      inc(SrcPos);
    Result:=copy(Src,StartPos,SrcPos-StartPos);
  end else
    Result:='';
end;

function TLinkScanner.ReadUpperIdentifier: string;
var StartPos: integer;
begin
  StartPos:=SrcPos;
  if (SrcPos<=SrcLen) and (IsIdentStartChar[UpperSrc[SrcPos]]) then begin
    inc(SrcPos);
    while (SrcPos<=SrcLen) and (IsIdentChar[UpperSrc[SrcPos]]) do
      inc(SrcPos);
    Result:=copy(UpperSrc,StartPos,SrcPos-StartPos);
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
begin
  dec(IfLevel);
  if IfLevel<0 then
    raise ELinkScannerError.Create('$ENDIF without $IF')
  else if IfLevel<FSkipIfLevel then
    FSkippingTillEndif:=false;
  Result:=true;
end;

function TLinkScanner.ElseDirective: boolean;
// {$else comment}
begin
  if IfLevel=0 then
    raise ELinkScannerError.Create('$ELSE without $IF');
  if not FSkippingTillEndif then
    SkipTillEndifElse
  else if IfLevel=FSkipIfLevel then
    FSkippingTillEndif:=false;
  Result:=true;
end;

function TLinkScanner.DefineDirective: boolean;
// {$define name} or {$define name:=value}
var VariableName, VariableValue: string;
begin
  SkipSpace;
  VariableName:=ReadUpperIdentifier;
  if (VariableName<>'') then begin
    if FMacrosOn and (SrcPos<SrcLen) and (copy(Src,SrcPos,2)=':=') then begin
      inc(SrcPos,2);
      VariableValue:=copy(Src,SrcPos,CommentInnerEndPos-SrcPos);
      Values.Variables[VariableName]:=VariableValue;
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

function TLinkScanner.IncludeDirective: boolean;
// {$i filename} or {$include filename}
var IncFilename: string;
begin
  inc(SrcPos);
  IncFilename:=Trim(copy(Src,SrcPos,CommentInnerEndPos-SrcPos));
  if Values.IsDefined('DELPHI') then begin
    // delphi understands quoted include files and default extension is .pas
    if (copy(IncFilename,1,1)='''')
    and (copy(IncFilename,length(IncFilename),1)='''') then
      IncFilename:=copy(IncFilename,2,length(IncFilename)-2);
    if ExtractFileExt(IncFilename)='' then
      IncFilename:=IncFilename+'.pas';
  end else begin
    // default is fpc behaviour
    if ExtractFileExt(IncFilename)='' then
      IncFilename:=IncFilename+'.pp';
  end;
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
//writeln('[TLinkScanner.IncludeDirective] END ',CommentEndPos,',',SrcPos,',',SrcLen);
end;

function TLinkScanner.IncludePathDirective: boolean;
// {$includepath path_addition}
var AddPath, PathDivider: string;
begin
  inc(SrcPos);
  AddPath:=Trim(copy(Src,SrcPos,CommentInnerEndPos-SrcPos));
  PathDivider:=':';
  Values.Variables['INCLUDEPATH']:=Values.Variables['INCLUDEPATH']
          +PathDivider+AddPath;
  Result:=true;
end;

function TLinkScanner.IncludeFile(const AFilename: string): boolean;
var
  ExpFilename: string;
  NewCode: pointer;

  function FilenameIsAbsolute(TheFilename: string):boolean;
  begin
    {$ifdef FPC}
    DoDirSeparators(TheFilename);
    {$endif}
    {$IFDEF win32}
    // windows
    Result:=(copy(TheFilename,1,2)='\\') or ((length(TheFilename)>3) and
       (UpChars[TheFilename[1]] in ['A'..'Z']) and (TheFilename[2]=':'));
    {$ELSE}
    Result:=(TheFilename<>'') and (TheFilename[1]='/');
    {$ENDIF}
  end;
  
  function LoadSourceCaseSensitive(const AbsoluteFilename: string): pointer;
  var Path, FileNameOnly: string;
  begin
    Path:=ExtractFilePath(AbsoluteFilename);
    FileNameOnly:=ExtractFilename(AbsoluteFilename);
    Result:=nil;
    if FileExists(Path+FileNameOnly) then
      Result:=FOnLoadSource(Self,Path+FileNameOnly);
    FileNameOnly:=lowercase(FileNameOnly);
    if (Result=nil) and (FileExists(Path+FileNameOnly)) then
      Result:=FOnLoadSource(Self,Path+FileNameOnly);
    FileNameOnly:=UpperCaseStr(FileNameOnly);
    if (Result=nil) and (FileExists(Path+FileNameOnly)) then
      Result:=FOnLoadSource(Self,Path+FileNameOnly);
  end;

  function SearchIncludeFile: boolean;
  var PathStart, PathEnd: integer;
    IncludePath, PathDivider, CurPath: string;

    function SearchPath(const APath: string): boolean;
    begin
      Result:=false;
      if APath='' then exit;
      if APath[length(APath)]<>OSDirSeparator then
        ExpFilename:=APath+OSDirSeparator+AFilename
      else
        ExpFilename:=APath+AFilename;
      if not FilenameIsAbsolute(ExpFilename) then
        ExpFilename:=ExtractFilePath(FMainSourceFilename)+ExpFilename;
      NewCode:=LoadSourceCaseSensitive(ExpFilename);
      Result:=NewCode<>nil;
    end;

  begin
    Result:=true;
    if not Assigned(FOnLoadSource) then begin
      NewCode:=nil;
      Result:=false;
      exit;
    end;
    // if include filename is absolute then load it directly
    if FilenameIsAbsolute(AFilename) then begin
      ExpFilename:=AFilename;
      NewCode:=LoadSourceCaseSensitive(ExpFilename);
      Result:=(NewCode<>nil);
      exit;
    end;
    // filename is relative
    // first search file in the directory of the main source
    if FilenameIsAbsolute(FMainSourceFilename) then begin
      ExpFilename:=ExtractFilePath(FMainSourceFilename)+AFilename;
      NewCode:=LoadSourceCaseSensitive(ExpFilename);
      if NewCode<>nil then exit;
    end;
    // then search the file in the include path
    IncludePath:=Values.Variables['#INCPATH'];
    if Values.IsDefined('DELPHI') then
      PathDivider:=':'
    else
      PathDivider:=':;';
    PathStart:=1;
    PathEnd:=PathStart;
//writeln('[TLinkScanner.IncludePathDirective] IncludePath=',IncludePath);
//Values.WriteDebugReport;
//writeln('');
    while PathEnd<=length(IncludePath) do begin
      if ((Pos(IncludePath[PathEnd],PathDivider))>0) 
      {$IFDEF win32}
      and (not ((PathEnd-PathStart=2) 
            and (IncludePath[PathEnd]=':') 
            and (IncludePath[PathEnd-1] in ['a'..'z','A'..'Z'])))
      {$ENDIF}
      then begin
        CurPath:=Trim(copy(IncludePath,PathStart,PathEnd-PathStart));
        Result:=SearchPath(CurPath);
        if Result then exit;
        PathStart:=PathEnd+1;
        PathEnd:=PathStart;
      end else
        inc(PathEnd);
    end;
    CurPath:=Trim(copy(IncludePath,PathStart,PathEnd-PathStart));
    Result:=SearchPath(CurPath);
    if Result then exit;
    // finally if the MainSource is a relative filename (virtual file) then the
    // include file will also be a relative filename (virtual file)
    if (not FilenameIsAbsolute(FMainSourceFilename)) then begin
      NewCode:=FOnLoadSource(Self,AFilename);
      Result:=(NewCode<>nil);
    end;   
    
  end;

// TLinkScanner.IncludeFile
begin
  Result:=SearchIncludeFile;
  if Result then begin
    // change source
    if Assigned(FOnIncludeCode) then
      FOnIncludeCode(FMainCode,NewCode);
    SetSource(NewCode);
    AddLink(CleanedLen+1,SrcPos,Code);
  end else begin
    if (not IgnoreMissingIncludeFiles) then begin
      raise ELinkScannerError.Create('include file not found "'+AFilename+'"')
    end;
  end;
end;

function TLinkScanner.IfDirective: boolean;
// {$if expression}
var Expr, ResultStr: string;
begin
  inc(IfLevel);
  inc(SrcPos);
  Expr:=UpperCaseStr(copy(Src,SrcPos,CommentInnerEndPos-SrcPos));
  ResultStr:=Values.Eval(Expr);
  if Values.ErrorPosition>=0 then
    raise ELinkScannerError.Create('syntax error in directive expression ')
  else if ResultStr='0' then
    SkipTillEndifElse
  else
  Result:=true;
end;

function TLinkScanner.IfOptDirective: boolean;
// {$ifopt o+} or {$ifopt o-}
var Option, c: char;
begin
  inc(IfLevel);
  inc(SrcPos);
  Option:=UpperSrc[SrcPos];
  if (Option in ['A'..'Z']) and (CompilerSwitchesNames[Option]<>'')
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
var NewLink: PSourceLink;
  i: integer;
begin
  for i:=0 to FIncludeStack.Count-1 do
    if PSourceLink(FIncludeStack[i])^.Code=ACode then
      raise ELinkScannerError.Create('Include circle detected');
  New(NewLink);
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
  Dispose(PLink);
  FIncludeStack.Delete(FIncludeStack.Count-1);
end;

function TLinkScanner.ReturnFromIncludeFile: boolean;
var OldPos: TSourceLink;
begin
  if not FSkippingTillEndif then UpdateCleanedSource(SrcPos-1);
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

procedure TLinkScanner.SkipTillEndifElse;
var OldDirectiveFuncList: TKeyWordFunctionList;
begin
  SrcPos:=CommentEndPos;
  UpdateCleanedSource(SrcPos-1);
  OldDirectiveFuncList:=FDirectiveFuncList;
  FDirectiveFuncList:=FSkipDirectiveFuncList;
  try
    // parse till $else or $endif without adding the code to FCleanedSrc
    FSkippingTillEndif:=true;
    FSkipIfLevel:=IfLevel;
    while (SrcPos<=SrcLen) and (FSkippingTillEndif) do begin
      if IsCommentStartChar[Src[SrcPos]] then begin
        case Src[SrcPos] of
          '{': SkipComment;
          '/': if (Src[SrcPos+1]='/') then
                 SkipDelphiComment
               else
                 inc(SrcPos);
          '(': if (Src[SrcPos+1]='*') then
                 SkipOldTPComment
               else
                 inc(SrcPos);
        end;
      end else begin
        inc(SrcPos);
        if SrcPos>SrcLen then ReturnFromIncludeFile;
      end;
    end;
    LastCleanSrcPos:=CommentStartPos-1;
    AddLink(CleanedLen+1,CommentStartPos,Code);
  finally
    FDirectiveFuncList:=OldDirectiveFuncList;
    FSkippingTillEndif:=false;
  end;
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
var i, j: integer;
begin
  i:=0;
  while i<LinkCount do begin
//writeln('[TLinkScanner.CursorToCleanPos] A ACursorPos=',ACursorPos,', Code=',Links[i].Code=ACode,', Links[i].SrcPos=',Links[i].SrcPos,', Links[i].CleanedPos=',Links[i].CleanedPos);
    if (Links[i].Code=ACode) and (Links[i].SrcPos<=ACursorPos) then begin
      ACleanPos:=ACursorPos-Links[i].SrcPos+Links[i].CleanedPos;
//writeln('[TLinkScanner.CursorToCleanPos] B ACleanPos=',ACleanPos);
      if i+1<LinkCount then begin
//writeln('[TLinkScanner.CursorToCleanPos] C Links[i+1].CleanedPos=',Links[i+1].CleanedPos);
        if ACleanPos<Links[i+1].CleanedPos then begin
          Result:=0;  // valid position
          exit;
        end;
        j:=i+1;
        while (j<LinkCount) and (Links[j].Code<>ACode) do inc(j);
//writeln('[TLinkScanner.CursorToCleanPos] D j=',j);
        if (j<LinkCount) and (Links[j].SrcPos>ACursorPos) then begin
          Result:=-1; // CursorPos was skipped, CleanPos is between two links
          exit;
        end;
        // search next
        i:=j-1;
      end else begin
        // in last link
//writeln('[TLinkScanner.CursorToCleanPos] E length(FCleanedSrc)=',length(FCleanedSrc));
        if ACleanPos<=length(FCleanedSrc) then
          Result:=0  // valid position
        else
          Result:=1; // cursor beyond scanned code
        exit;
      end;
    end;
    inc(i);
  end;
  Result:=1;
end;

function TLinkScanner.CleanedPosToCursor(ACleanedPos: integer;
  var ACursorPos: integer; var ACode: Pointer): boolean;
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
        if ACleanedPos<Links[m].CleanedPos then
          r:=m-1
        else if ACleanedPos>=Links[m+1].CleanedPos then
          l:=m+1
        else begin
          ACode:=Links[m].Code;
          ACursorPos:=ACleanedPos-Links[m].CleanedPos+Links[m].SrcPos;
          exit;
        end;
      end else begin
        if ACleanedPos>=Links[m].CleanedPos then begin
          ACode:=Links[m].Code;
          ACursorPos:=ACleanedPos-Links[m].CleanedPos+Links[m].SrcPos;
          exit;
        end else
          raise Exception.Create(
              'TLinkScanner.CleanedPosToCursor Consistency-Error 2');
      end;
    end;
    raise Exception.Create(
        'TLinkScanner.CleanedPosToCursor Consistency-Error 1');
  end;
end;

function TLinkScanner.WholeRangeIsWritable(CleanStartPos, CleanEndPos: integer
  ): boolean;
var ACode: Pointer;
  LinkIndex: integer;
  CodeIsReadOnly: boolean;
begin
  Result:=false;
  if (CleanStartPos<1) or (CleanStartPos>=CleanEndPos)
  or (CleanEndPos>CleanedLen+1) or (not Assigned(FOnGetSourceStatus)) then exit;
  LinkIndex:=LinkIndexAtCleanPos(CleanStartPos);
  if LinkIndex<0 then exit;
  ACode:=Links[LinkIndex].Code;
  FOnGetSourceStatus(Self,ACode,CodeIsReadOnly);
  if CodeIsReadOnly then exit;
  repeat
    inc(LinkIndex);
    if (LinkIndex>=LinkCount) or (Links[LinkIndex].CleanedPos>CleanEndPos) then
    begin
      Result:=true;
      exit;
    end;
    if ACode<>Links[LinkIndex].Code then begin
      ACode:=Links[LinkIndex].Code;
      FOnGetSourceStatus(Self,ACode,CodeIsReadOnly);
      if CodeIsReadOnly then exit;
    end;
  until false;
end;

procedure TLinkScanner.FindCodeInRange(CleanStartPos, CleanEndPos: integer;
  UniqueSortedCodeList: TList);
  
  procedure AddCodeToList(ACode: Pointer);
  var l,m,r: integer;
  begin
    l:=0;
    r:=UniqueSortedCodeList.Count-1;
    m:=0;
    while r>=l do begin
      m:=(l+r) shr 1;
      if UniqueSortedCodeList[m]<ACode then
        r:=m-1
      else if UniqueSortedCodeList[m]>ACode then
        l:=m+1
      else
        exit;
    end;
    if (m<UniqueSortedCodeList.Count) and (UniqueSortedCodeList[m]<ACode) then
      inc(m);
    UniqueSortedCodeList.Insert(m,ACode);
  end;
  
var ACode: Pointer;
  LinkIndex: integer;
begin
  if (CleanStartPos<1) or (CleanStartPos>CleanEndPos)
  or (CleanEndPos>CleanedLen+1) or (UniqueSortedCodeList=nil) then exit;
  LinkIndex:=LinkIndexAtCleanPos(CleanStartPos);
  if LinkIndex<0 then exit;
  ACode:=Links[LinkIndex].Code;
  AddCodeToList(ACode);
  repeat
    inc(LinkIndex);
    if (LinkIndex>=LinkCount) or (Links[LinkIndex].CleanedPos>CleanEndPos) then
      exit;
    if ACode<>Links[LinkIndex].Code then begin
      ACode:=Links[LinkIndex].Code;
      AddCodeToList(ACode);
    end;
  until false;
end;

procedure TLinkScanner.DeleteRange(CleanStartPos,CleanEndPos: integer);
{ delete all code in links (=parsed code) starting with the last link
  before you call this, test with WholeRangeIsWritable

  this can do unexpected things if
    - include files are included twice
    - comiler directives like IFDEF - ENDIF are partially destroyed
    
  ToDo: keep include directives
}
var LinkIndex, StartPos, Len, aLinkSize: integer;
  Link: TSourceLink;
begin
  if (CleanStartPos<1) or (CleanStartPos>=CleanEndPos)
  or (CleanEndPos>CleanedLen+1) or (not Assigned(FOnDeleteSource)) then exit;
  LinkIndex:=LinkIndexAtCleanPos(CleanEndPos-1);
  while LinkIndex>=0 do begin
    Link:=Links[LinkIndex];
    StartPos:=CleanStartPos-Link.CleanedPos;
    if Startpos<0 then StartPos:=0;
    aLinkSize:=LinkSize(LinkIndex);
    if CleanEndPos<Link.CleanedPos+aLinkSize then
      Len:=CleanEndPos-Link.CleanedPos-StartPos
    else
      Len:=aLinkSize-StartPos;
    inc(StartPos,Link.SrcPos);
    FOnDeleteSource(Self,Links[LinkIndex].Code,StartPos,Len);
    if Link.CleanedPos<=CleanStartPos then break;
    dec(LinkIndex);
  end;
end;


//------------------------------------------------------------------------------
procedure InternalInit;
var c: char;
begin
  for c:=Low(char) to high(char) do begin
    IsLineEndChar[c]:=c in [#10,#13];
    IsSpaceChar[c]:=c in [#0..#32];
    IsIdentStartChar[c]:=c in ['a'..'z','A'..'Z','_'];
    IsIdentChar[c]:=c in ['a'..'z','A'..'Z','_','0'..'9'];
    IsNumberChar[c]:=c in ['0'..'9'];
    IsCommentStartChar[c]:=c in ['/','{','('];
    IsHexNumberChar[c]:=c in ['0'..'9','a'..'f','A'..'F'];
    IsEqualOperatorStartChar[c]:=c in [':','+','-','/','*','<','>'];
  end;
end;

initialization


InternalInit;

end.

