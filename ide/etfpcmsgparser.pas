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
    Parser for Free Pascal Compiler output.
}
unit etFPCMsgParser;

{$mode objfpc}{$H+}

{ $DEFINE VerboseFPCMsgUnitNotFound}

interface

uses
  // RTL
  Classes, SysUtils, strutils, math,
  // CodeTools
  KeywordFuncLists, CodeToolsFPCMsgs, CodeCache, FileProcs,
  CodeToolManager, DirectoryCacher, BasicCodeTools, DefineTemplates, SourceLog,
  // LazUtils
  LConvEncoding, LazUTF8, FileUtil, LazFileUtils, LazFileCache, LazUtilities,
  AvgLvlTree,
  // IDEIntf
  IDEExternToolIntf, PackageIntf, LazIDEIntf, ProjectIntf, MacroIntf, IDEUtils,
  // IDE
  IDECmdLine, LazarusIDEStrConsts, EnvironmentOpts, LazConf, TransferMacros,
  etMakeMsgParser;

const
  FPCMsgIDCompiling = 3104;
  FPCMsgIDLogo = 11023;
  FPCMsgIDCantFindUnitUsedBy = 10022;
  FPCMsgIDLinking = 9015;
  FPCMsgIDErrorWhileLinking = 9013;
  FPCMsgIDErrorWhileCompilingResources = 9029;
  FPCMsgIDCallingResourceCompiler = 9028;
  FPCMsgIDThereWereErrorsCompiling = 10026;
  FPCMsgIDMethodIdentifierExpected = 3047;
  FPCMsgIDIdentifierNotFound = 5000;
  FPCMsgIDChecksumChanged = 10028;
  FPCMsgIDUnitNotUsed = 5023; // Unit "$1" not used in $2
  FPCMsgIDCompilationAborted = 1018;
  FPCMsgIDLinesCompiled = 1008;

  FPCMsgAttrWorkerDirectory = 'WD';
  FPCMsgAttrMissingUnit = 'MissingUnit';
  FPCMsgAttrUsedByUnit = 'UsedByUnit';
type
  TFPCMsgFilePool = class;

  { TFPCMsgFilePoolItem }

  TFPCMsgFilePoolItem = class
  private
    FMsgFile: TFPCMsgFile;
    FFilename: string;
    FPool: TFPCMsgFilePool;
    FLoadedFileAge: integer;
    fUseCount: integer;
  public
    constructor Create(aPool: TFPCMsgFilePool; const aFilename: string);
    destructor Destroy; override;
    property Pool: TFPCMsgFilePool read FPool;
    property Filename: string read FFilename;
    property LoadedFileAge: integer read FLoadedFileAge;
    function GetMsg(ID: integer): TFPCMsgItem;
    property MsgFile: TFPCMsgFile read FMsgFile;
    property UseCount: integer read fUseCount;
  end;

  TETLoadFileEvent = procedure(aFilename: string; out s: string) of object;

  { TFPCMsgFilePool }

  TFPCMsgFilePool = class(TComponent)
  private
    fCritSec: TRTLCriticalSection;
    FDefaultEnglishFile: string;
    FDefaultTranslationFile: string;
    FFiles: TFPList; // list of TFPCMsgFilePoolItem sorted for loaded
    FOnLoadFile: TETLoadFileEvent;
    fPendingLog: TStrings;
    fMsgFileStamp: integer;
    fCurrentEnglishFile: string; // valid only if fMsgFileStamp=CompilerParseStamp
    fCurrentTranslationFile: string; // valid only if fMsgFileStamp=CompilerParseStamp
    procedure Log(Msg: string; AThread: TThread);
    procedure LogSync;
    procedure SetDefaultEnglishFile(AValue: string);
    procedure SetDefaultTranslationFile(AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadCurrentEnglishFile(UpdateFromDisk: boolean;
      AThread: TThread): TFPCMsgFilePoolItem; virtual; // don't forget UnloadFile
    function LoadFile(aFilename: string; UpdateFromDisk: boolean;
      AThread: TThread): TFPCMsgFilePoolItem; // don't forget UnloadFile
    procedure UnloadFile(var aFile: TFPCMsgFilePoolItem);
    procedure EnterCriticalsection;
    procedure LeaveCriticalSection;
    procedure GetMsgFileNames(CompilerFilename, TargetOS, TargetCPU: string;
      out anEnglishFile, aTranslationFile: string); virtual; // (main thread)
    property DefaultEnglishFile: string read FDefaultEnglishFile write SetDefaultEnglishFile;
    property DefaulTranslationFile: string read FDefaultTranslationFile write SetDefaultTranslationFile;
    property OnLoadFile: TETLoadFileEvent read FOnLoadFile write FOnLoadFile; // (main or workerthread)
  end;

  { TPatternToMsgID }

  TPatternToMsgID = class
  public
    Pattern: string;
    MsgID: integer;
    PatternLine: integer; // line index in a multi line pattern, starting at 0
  end;
  PPatternToMsgID = ^TPatternToMsgID;

  { TPatternToMsgIDs }

  TPatternToMsgIDs = class
  private
    fItems: array of TPatternToMsgID;
    function IndexOf(Pattern: PChar; Insert: boolean): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Pattern: string; MsgID: integer; PatternLine: integer = 0);
    procedure AddLines(const Lines: string; MsgID: integer);
    function LineToMsgID(p: PChar): integer; inline; // 0 = not found
    function LineToPattern(p: PChar): PPatternToMsgID;
    procedure WriteDebugReport;
    procedure ConsistencyCheck;
  end;

  { TIDEFPCParser }

  TIDEFPCParser = class(TFPCParser)
  private
    fCurSource: TCodeBuffer;
    fFileExists: TFilenameToPointerTree;
    fIncludePath: string; // only valid if fIncludePathValidForWorkerDir=Tool.WorkerDirectory
    fIncludePathValidForWorkerDir: string;
    fUnitPath: string; // only valid if fUnitPathValidForWorkerDir=Tool.WorkerDirectory
    fUnitPathValidForWorkerDir: string;
    fLastWorkerImprovedMessage: array[TExtToolParserSyncPhase] of integer;
    fLineToMsgID: TPatternToMsgIDs;
    fMissingFPCMsgItem: TFPCMsgItem;
    fMsgID: Integer; // current message id given by ReadLine (-vq)
    fMsgItemCantFindUnitUsedBy: TFPCMsgItem;
    fMsgItemCompilationAborted: TFPCMsgItem;
    fMsgItemErrorWhileCompilingResources: TFPCMsgItem;
    fMsgItemErrorWhileLinking: TFPCMsgItem;
    fMsgItemMethodIdentifierExpected: TFPCMsgItem;
    fMsgItemIdentifierNotFound: TFPCMsgItem;
    fMsgItemThereWereErrorsCompiling: TFPCMsgItem;
    fMsgItemChecksumChanged: TFPCMsgItem;
    fMsgItemUnitNotUsed: TFPCMsgItem;
    fOutputIndex: integer; // current OutputIndex given by ReadLine
    procedure FetchIncludePath(aPhase: TExtToolParserSyncPhase; MsgWorkerDir: String);
    procedure FetchUnitPath(aPhase: TExtToolParserSyncPhase; MsgWorkerDir: String);
    function FileExists(const Filename: string; aSynchronized: boolean): boolean;
    function CheckForMsgId(p: PChar): boolean; // (MsgId) message
    function CheckFollowUpMessage(p: PChar): boolean;
    function CheckForFileLineColMessage(p: PChar): boolean; // the normal messages: filename(y,x): Hint: ..
    function CheckForGeneralMessage(p: PChar): boolean; // Fatal: .., Error: ..., Panic: ..
    function CheckForInfos(p: PChar): boolean; // e.g. Free Pascal Compiler version 2.6.4 [2014/02/26] for i386
    function CheckForCompilingState(p: PChar): boolean; // Compiling ..
    function CheckForAssemblingState(p: PChar): boolean; // Assembling ..
    function CheckForLinesCompiled(p: PChar): boolean; // ..lines compiled..
    function CheckForExecutableInfo(p: PChar): boolean;
    function CheckForLineProgress(p: PChar): boolean; // 600 206.521/231.648 Kb Used
    function CheckForLoadFromUnit(p: PChar): Boolean;
    function CheckForWindresErrors(p: PChar): boolean;
    function CheckForLinkerErrors(p: PChar): boolean;
    function CheckForAssemblerErrors(p: PChar): boolean;
    function CreateMsgLine: TMessageLine;
    procedure AddLinkingMessages;
    procedure AddResourceMessages;
    function NeedSource(aPhase: TExtToolParserSyncPhase; SourceOk: boolean): boolean;
    procedure ImproveMsgHiddenByIDEDirective(aPhase: TExtToolParserSyncPhase;
      MsgLine: TMessageLine; SourceOK: Boolean);
    procedure ImproveMsgSenderNotUsed(aPhase: TExtToolParserSyncPhase; MsgLine: TMessageLine);
    procedure ImproveMsgUnitNotUsed(aPhase: TExtToolParserSyncPhase; MsgLine: TMessageLine);
    procedure ImproveMsgUnitNotFound(aPhase: TExtToolParserSyncPhase;
      MsgLine: TMessageLine);
    procedure ImproveMsgLinkerUndefinedReference(aPhase: TExtToolParserSyncPhase;
      MsgLine: TMessageLine);
    procedure ImproveMsgIdentifierPosition(aPhase: TExtToolParserSyncPhase;
      MsgLine: TMessageLine; SourceOK: boolean);
    function FindSrcViaPPU(aPhase: TExtToolParserSyncPhase; MsgLine: TMessageLine;
      const PPUFilename: string): boolean;
    procedure Translate(p: PChar; MsgItem, TranslatedItem: TFPCMsgItem;
      out TranslatedMsg: String; out MsgType: TMessageLineUrgency);
    function ReverseInstantFPCCacheDir(var aFilename: string; aSynchronized: boolean): boolean;
    function ReverseTestBuildDir(MsgLine: TMessageLine; var aFilename: string): boolean;
    function LongenFilename(MsgLine: TMessageLine; aFilename: string): string; // (worker thread)
  protected
    function GetDefaultPCFullVersion: LongWord; virtual;
    function ToUTF8(const Line: string): string; virtual;
  public
    DirectoryStack: TStrings;
    MsgFilename: string; // e.g. /path/to/fpcsrc/compiler/msg/errore.msg
    MsgFile: TFPCMsgFilePoolItem;
    TranslationFilename: string; // e.g. /path/to/fpcsrc/compiler/msg/errord.msg
    TranslationFile: TFPCMsgFilePoolItem;
    InstantFPCCache: string; // with trailing pathdelim
    TestBuildDir: string; // with trailing pathdelim
    VirtualProjectFiles: TFilenameToPointerTree;
    PC_FullVersion: LongWord;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init; override; // called after macros resolved, before starting thread (main thread)
    procedure InitReading; override; // called when process started, before first line (worker thread)
    procedure Done; override; // called after process stopped (worker thread)
    procedure ReadLine(Line: string; OutputIndex: integer; var Handled: boolean); override;
    procedure AddMsgLine(MsgLine: TMessageLine); override;
    procedure ImproveMessages(aPhase: TExtToolParserSyncPhase); override;
    function GetFPCMsgIDPattern(MsgID: integer): string; override;
    function IsMsgID(MsgLine: TMessageLine; MsgID: integer;
      var Item: TFPCMsgItem): boolean;
    class function CanParseSubTool(const SubTool: string): boolean; override;
    class function DefaultSubTool: string; override;
    class function GetMsgPattern(SubTool: string; MsgID: integer;
      out Urgency: TMessageLineUrgency): string; override;
    class function GetMsgHint(SubTool: string; MsgID: integer): string;
      override;
    class function Priority: integer; override;
    class function MsgLineIsId(Msg: TMessageLine; MsgId: integer;
      out Value1, Value2: string): boolean; override;
    class function GetFPCMsgPattern(Msg: TMessageLine): string; override;
    class function GetFPCMsgValue1(Msg: TMessageLine): string; override;
    class function GetFPCMsgValues(Msg: TMessageLine; out Value1, Value2: string): boolean; override;
    class function MsgFilePool: TFPCMsgFilePool; virtual;
  end;

var
  FPCMsgFilePool: TFPCMsgFilePool = nil;

// thread safe
function FPCMsgToMsgUrgency(Msg: TFPCMsgItem): TMessageLineUrgency;
function FPCMsgTypeToUrgency(const Typ: string): TMessageLineUrgency;
function TranslateFPCMsg(const Src, SrcPattern, TargetPattern: string): string;
function FPCMsgFits(const Msg, Pattern: string;
  VarStarts: PPChar = nil; VarEnds: PPChar = nil // 10 PChars
  ): boolean;
function GetFPCMsgValue1(const Src, Pattern: string; out Value1: string): boolean;
function GetFPCMsgValues2(Src, Pattern: string; out Value1, Value2: string): boolean;

// not thread safe
function IsFileInIDESrcDir(Filename: string): boolean; // (main thread)

procedure RegisterFPCParser;

implementation

function FPCMsgTypeToUrgency(const Typ: string): TMessageLineUrgency;
begin
  Result:=mluNone;
  if (Typ='') or (length(Typ)<>1) then exit;
  case UpChars[Typ[1]] of
  'F': Result:=mluFatal;
  'E': Result:=mluError;
  'W': Result:=mluWarning;
  'N': Result:=mluNote;
  'H': Result:=mluHint;
  'I': Result:=mluVerbose;  // info
  'L': Result:=mluProgress; // line number
  'C': Result:=mluVerbose;  // conditional: like IFDEFs
  'U': Result:=mluVerbose2; // used: found files
  'T': Result:=mluVerbose3; // tried: tried paths, general information
  'D': Result:=mluDebug;
  'X': Result:=mluProgress; // e.g. Size of Code
  'O': Result:=mluProgress; // e.g., "press enter to continue"
  else
    Result:=mluNone;
  end;
end;

function FPCMsgToMsgUrgency(Msg: TFPCMsgItem): TMessageLineUrgency;
begin
  Result:=mluNone;
  if Msg=nil then exit;
  Result:=FPCMsgTypeToUrgency(Msg.ShownTyp);
  if Result<>mluNone then exit;
  Result:=FPCMsgTypeToUrgency(Msg.Typ);
  if Result=mluNone then begin
    //debugln(['FPCMsgToMsgUrgency Msg.ShownTyp="',Msg.ShownTyp,'" Msg.Typ="',Msg.Typ,'"']);
    Result:=mluVerbose3;
  end;
end;

function IsFPCMsgVar(p: PChar): boolean; inline;
begin
  Result:=(p^='$') and (p[1] in ['0'..'9']);
end;

function IsFPCMsgEndOrVar(p: PChar): boolean; inline;
begin
  Result:=(p^=#0) or IsFPCMsgVar(p);
end;

function TranslateFPCMsg(const Src, SrcPattern, TargetPattern: string): string;
{ for example:
  Src='A lines compiled, B sec C'
  SrcPattern='$1 lines compiled, $2 sec $3'
  TargetPattern='$1 Zeilen uebersetzt, $2 Sekunden $3'

  Result='A Zeilen uebersetzt, B Sekunden C'
}
var
  SrcPos: PChar;
  TargetPatPos: PChar;
  TargetPos: PChar;
  SrcVarStarts, SrcVarEnds: array[0..9] of PChar;
  VarUsed: array[0..9] of integer;
  i: Integer;
begin
  Result:='';
  {$IFDEF VerboseFPCTranslate}
  debugln(['TranslateFPCMsg Src="',Src,'" SrcPattern="',SrcPattern,'" TargetPattern="',TargetPattern,'"']);
  {$ENDIF}
  if (Src='') or (SrcPattern='') or (TargetPattern='') then exit;

  if not FPCMsgFits(Src,SrcPattern,@SrcVarStarts[0],@SrcVarEnds[0]) then
    exit;

  for i:=Low(SrcVarStarts) to high(SrcVarStarts) do
    VarUsed[i]:=0;

  // create Target
  SetLength(Result,length(TargetPattern)+length(Src));
  TargetPatPos:=PChar(TargetPattern);
  TargetPos:=PChar(Result);
  while TargetPatPos^<>#0 do begin
    //debugln(['TranslateFPCMsg Target ',dbgs(Pointer(TargetPatPos)),' ',ord(TargetPatPos^),' TargetPatPos="',TargetPatPos,'"']);
    if IsFPCMsgVar(TargetPatPos) then begin
      // insert variable
      inc(TargetPatPos);
      i:=ord(TargetPatPos^)-ord('0');
      inc(TargetPatPos);
      if SrcVarStarts[i]<>nil then begin
        inc(VarUsed[i]);
        if VarUsed[i]>1 then begin
          // variable is used more than once => realloc result
          dec(TargetPos,{%H-}PtrUInt(PChar(Result)));
          SetLength(Result,length(Result)+SrcVarEnds[i]-SrcVarStarts[i]);
          inc(TargetPos,{%H-}PtrUInt(PChar(Result)));
        end;
        SrcPos:=SrcVarStarts[i];
        while SrcPos<SrcVarEnds[i] do begin
          TargetPos^:=SrcPos^;
          inc(TargetPos);
          inc(SrcPos);
        end;
      end;
    end else begin
      // copy text from TargetPattern
      TargetPos^:=TargetPatPos^;
      inc(TargetPatPos);
      inc(TargetPos);
    end;
  end;
  SetLength(Result,TargetPos-PChar(Result));
  if Result<>'' then
    UTF8FixBroken(PChar(Result));

  {$IFDEF VerboseFPCTranslate}
  debugln(['TranslateFPCMsg Result="',Result,'"']);
  {$ENDIF}
end;

function FPCMsgFits(const Msg, Pattern: string; VarStarts: PPChar;
  VarEnds: PPChar): boolean;
{ for example:
  Src='A lines compiled, B sec C'
  SrcPattern='$1 lines compiled, $2 sec $3'

  VarStarts and VarEnds can be nil.
  If you need the boundaries of the parameters allocate VarStarts and VarEnds as
    VarStarts:=GetMem(SizeOf(PChar)*10);
    VarEnds:=GetMem(SizeOf(PChar)*10);
  VarStarts[0] will be $0, VarStarts[1] will be $1 and so forth

}
var
  MsgPos, PatPos: PChar;
  MsgPos2, PatPos2: PChar;
  i: Integer;
begin
  Result:=false;
  {$IFDEF VerboseFPCTranslate}
  debugln(['FPCMsgFits Msg="',Msg,'" Pattern="',Pattern,'"']);
  {$ENDIF}
  if (Msg='') or (Pattern='') then exit;
  MsgPos:=PChar(Msg);
  PatPos:=PChar(Pattern);
  // skip the characters of Msg copied from Pattern
  while not IsFPCMsgEndOrVar(PatPos) do begin
    if (MsgPos^<>PatPos^) then begin
      // Pattern does not fit
      {$IFDEF VerboseFPCTranslate}
      debugln(['FPCMsgFits skipping start of Src and SrcPattern failed']);
      {$ENDIF}
      exit;
    end;
    inc(MsgPos);
    inc(PatPos)
  end;
  {$IFDEF VerboseFPCTranslate}
  debugln(['FPCMsgFits skipped start: SrcPos="',SrcPos,'" SrcPatPos="',SrcPatPos,'"']);
  {$ENDIF}
  if VarStarts<>nil then begin
    FillByte(VarStarts^,SizeOf(PChar)*10,0);
    FillByte(VarEnds^,SizeOf(PChar)*10,0);
  end;
  // find the parameters in Msg and store their boundaries in VarStarts, VarEnds
  while (PatPos^<>#0) do begin
    // read variable number
    inc(PatPos);
    i:=ord(PatPos^)-ord('0');
    inc(PatPos);
    if (VarEnds<>nil) and (VarEnds[i]=nil) then begin
      VarStarts[i]:=MsgPos;
      VarEnds[i]:=nil;
    end;
    // find the end of the parameter in Msg
    // example:  Pattern='$1 found' Msg='Ha found found'
    repeat
      if MsgPos^=PatPos^ then begin
        {$IFDEF VerboseFPCTranslate}
        debugln(['FPCMsgFits candidate for param ',i,' end: SrcPos="',SrcPos,'" SrcPatPos="',SrcPatPos,'"']);
        {$ENDIF}
        MsgPos2:=MsgPos;
        PatPos2:=PatPos;
        while (MsgPos2^=PatPos2^) and not IsFPCMsgEndOrVar(PatPos2) do begin
          inc(MsgPos2);
          inc(PatPos2);
        end;
        if IsFPCMsgEndOrVar(PatPos2) then begin
          {$IFDEF VerboseFPCTranslate}
          debugln(['FPCMsgFits param ',i,' end found: SrcPos2="',SrcPos2,'" SrcPatPos2="',SrcPatPos2,'"']);
          {$ENDIF}
          if (VarEnds<>nil) and (VarEnds[i]=nil) then
            VarEnds[i]:=MsgPos;
          MsgPos:=MsgPos2;
          PatPos:=PatPos2;
          break;
        end;
        {$IFDEF VerboseFPCTranslate}
        debugln(['FPCMsgFits searching further...']);
        {$ENDIF}
      end else if MsgPos^=#0 then begin
        if IsFPCMsgEndOrVar(PatPos) then begin
          // empty parameter at end
          if (VarEnds<>nil) and (VarEnds[i]=nil) then
            VarEnds[i]:=MsgPos;
          break;
        end else begin
          // Pattern does not fit Msg
          {$IFDEF VerboseFPCTranslate}
          debugln(['FPCMsgFits finding end of parameter ',i,' failed']);
          {$ENDIF}
          exit;
        end;
      end;
      inc(MsgPos);
    until false;
  end;
  Result:=true;
end;

function GetFPCMsgValue1(const Src, Pattern: string; out Value1: string
  ): boolean;
{ Pattern: 'Compiling $1'
  Src:     'Compiling fcllaz.pas'
  Value1:  'fcllaz.pas'
}
var
  p: SizeInt;
  l: SizeInt;
begin
  Value1:='';
  Result:=false;
  if length(Src)<length(Pattern)-2 then exit;
  p:=Pos('$1',Pattern);
  if p<1 then exit;
  // check start pattern
  if (p>1) and (not CompareMem(Pointer(Src),Pointer(Pattern),p-1)) then exit;
  // check end pattern
  l:=length(Pattern)-p-2;
  if (l>0)
  and (not CompareMem(Pointer(Src)+length(Src)-l,Pointer(Pattern)+p+2,l)) then exit;
  Value1:=copy(Src,p,length(Src)-length(Pattern)+2);
  Result:=true;
end;

function GetFPCMsgValues2(Src, Pattern: string; out Value1, Value2: string
  ): boolean;
{ Pattern: 'Unit $1 was not found but $2 exists'
  Src:     'Unit dialogprocs was not found but dialogpr exists'
  Value1:  'dialogprocs'
  Value1:  'dialogpr'
  Not supported: '$1$2'
}
var
  p1: SizeInt;
  LastPattern: String;
  p2: SizeInt;
  MiddlePattern: String;
  SrcP1Behind: Integer;
  SrcP2: Integer;
begin
  Result:=false;
  Value1:='';
  Value2:='';
  p1:=Pos('$1',Pattern);
  if p1<1 then exit;
  p2:=Pos('$2',Pattern);
  if p2<=p1+2 then exit;
  if LeftStr(Pattern,p1-1)<>LeftStr(Src,p1-1) then exit;
  LastPattern:=RightStr(Pattern,length(Pattern)-p2-1);
  if RightStr(Src,length(LastPattern))<>LastPattern then exit;
  MiddlePattern:=copy(Pattern,p1+2,p2-p1-2);
  SrcP1Behind:=PosEx(MiddlePattern,Src,p1+2);
  if SrcP1Behind<1 then exit;
  Value1:=copy(Src,p1,SrcP1Behind-p1);
  SrcP2:=SrcP1Behind+length(MiddlePattern);
  Value2:=copy(Src,SrcP2,length(Src)-SrcP2-length(LastPattern)+1);
  Result:=true;
end;

function IsFileInIDESrcDir(Filename: string): boolean;
var
  LazDir: String;
begin
  Filename:=TrimFilename(Filename);
  if not FilenameIsAbsolute(Filename) then exit(false);
  LazDir:=AppendPathDelim(EnvironmentOptions.GetParsedLazarusDirectory);
  Result:=FileIsInPath(Filename,LazDir+'ide')
       or FileIsInPath(Filename,LazDir+'debugger')
       or FileIsInPath(Filename,LazDir+'packager')
       or FileIsInPath(Filename,LazDir+'converter')
       or FileIsInPath(Filename,LazDir+'designer');
end;

procedure RegisterFPCParser;
begin
  ExternalToolList.RegisterParser(TIDEFPCParser);
end;

{ TPatternToMsgIDs }

function TPatternToMsgIDs.IndexOf(Pattern: PChar; Insert: boolean): integer;
var
  l: Integer;
  r: Integer;
  m: Integer;
  ItemP: PChar;
  FindP: PChar;
  cmp: Integer;
begin
  Result:=-1;
  l:=0;
  r:=length(fItems)-1;
  cmp:=0;
  m:=0;
  while (l<=r) do begin
    m:=(l+r) div 2;
    ItemP:=PChar(fItems[m].Pattern);
    FindP:=Pattern;
    while (ItemP^=FindP^) do begin
      if ItemP^=#0 then
        exit(m); // exact match
      inc(ItemP);
      inc(FindP);
    end;
    if ItemP^ in [#0,'$'] then begin
      // Pattern longer than Item
      if not Insert then begin
        if (Result<0) or (length(fItems[m].Pattern)>length(fItems[Result].Pattern))
        then
          Result:=m;
      end;
    end;
    cmp:=ord(ItemP^)-ord(FindP^);
    if cmp<0 then
      l:=m+1
    else
      r:=m-1;
  end;
  if Insert then begin
    if cmp<0 then
      Result:=m+1
    else
      Result:=m;
  end;
end;

constructor TPatternToMsgIDs.Create;
begin

end;

destructor TPatternToMsgIDs.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TPatternToMsgIDs.Clear;
var
  i: Integer;
begin
  for i:=0 to length(fItems)-1 do
    fItems[i].Free;
  SetLength(fItems,0);
end;

procedure TPatternToMsgIDs.Add(Pattern: string; MsgID: integer;
  PatternLine: integer);

  procedure RaiseInvalidMsgID;
  begin
    raise Exception.Create('invalid MsgID: '+IntToStr(MsgID));
  end;

var
  i: Integer;
  Item: TPatternToMsgID;
  Cnt: Integer;
begin
  if MsgID=0 then
    RaiseInvalidMsgID;
  Pattern:=Trim(Pattern);
  if (Pattern='') or (Pattern[1]='$') then exit;
  i:=IndexOf(PChar(Pattern),true);
  Cnt:=length(fItems);
  SetLength(fItems,Cnt+1);
  if Cnt-i>0 then
    Move(fItems[i],fItems[i+1],SizeOf(TPatternToMsgID)*(Cnt-i));
  Item:=TPatternToMsgID.Create;
  fItems[i]:=Item;
  Item.Pattern:=Pattern;
  Item.MsgID:=MsgID;
  Item.PatternLine:=PatternLine;
end;

procedure TPatternToMsgIDs.AddLines(const Lines: string; MsgID: integer);
var
  StartPos: PChar;
  p: PChar;
  PatternLine: Integer;
begin
  PatternLine:=0;
  p:=PChar(Lines);
  while p^<>#0 do begin
    StartPos:=p;
    while not (p^ in [#0,#10,#13]) do inc(p);
    if p>StartPos then begin
      Add(copy(Lines,StartPos-PChar(Lines)+1,p-StartPos),MsgID,PatternLine);
      inc(PatternLine);
    end;
    while p^ in [#10,#13] do inc(p);
  end;
end;

function TPatternToMsgIDs.LineToMsgID(p: PChar): integer;
var
  Item: PPatternToMsgID;
begin
  Item:=LineToPattern(p);
  if Item=nil then
    Result:=0
  else
    Result:=Item^.MsgID;
end;

function TPatternToMsgIDs.LineToPattern(p: PChar): PPatternToMsgID;
var
  i: Integer;
begin
  while p^ in [' ',#9,#10,#13] do inc(p);
  i:=IndexOf(p,false);
  if i<0 then
    Result:=nil
  else
    Result:=@fItems[i];
end;

procedure TPatternToMsgIDs.WriteDebugReport;
var
  i: Integer;
begin
  debugln(['TLineStartToMsgIDs.WriteDebugReport Count=',length(fItems)]);
  for i:=0 to Length(fItems)-1 do begin
    debugln(['  ID=',fItems[i].MsgID,'="',fItems[i].Pattern,'"']);
  end;
  ConsistencyCheck;
end;

procedure TPatternToMsgIDs.ConsistencyCheck;

  procedure E(Msg: string);
  begin
    raise Exception.Create(Msg);
  end;

var
  i: Integer;
  Item: TPatternToMsgID;
begin
  for i:=0 to Length(fItems)-1 do begin
    Item:=fItems[i];
    if Item.MsgID<=0 then
      E('Item.MsgID<=0');
    if Item.Pattern='' then
      E('Item.Pattern empty');
    if IndexOf(PChar(Item.Pattern),false)<>i then
      E('IndexOf '+dbgs(i)+' "'+Item.Pattern+'" IndexOf='+dbgs(IndexOf(PChar(Item.Pattern),false)));
  end;
end;

{ TFPCMsgFilePool }

procedure TFPCMsgFilePool.Log(Msg: string; AThread: TThread);
begin
  EnterCriticalsection;
  try
    fPendingLog.Add(Msg);
  finally
    LeaveCriticalSection;
  end;
  if AThread<>nil then
    LogSync
  else
    TThread.Synchronize(AThread,@LogSync);
end;

procedure TFPCMsgFilePool.LogSync;
begin
  EnterCriticalsection;
  try
    dbgout(fPendingLog.Text);
  finally
    LeaveCriticalSection;
  end;
end;

procedure TFPCMsgFilePool.SetDefaultEnglishFile(AValue: string);
begin
  if FDefaultEnglishFile=AValue then Exit;
  FDefaultEnglishFile:=AValue;
  fMsgFileStamp:=-1;
end;

procedure TFPCMsgFilePool.SetDefaultTranslationFile(AValue: string);
begin
  if FDefaultTranslationFile=AValue then Exit;
  FDefaultTranslationFile:=AValue;
  fMsgFileStamp:=-1;
end;

constructor TFPCMsgFilePool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitCriticalSection(fCritSec);
  FFiles:=TFPList.Create;
  fPendingLog:=TStringList.Create;
  fMsgFileStamp:=-1;
end;

destructor TFPCMsgFilePool.Destroy;
var
  i: Integer;
  Item: TFPCMsgFilePoolItem;
begin
  EnterCriticalsection;
  try
    // free unused files
    for i:=FFiles.Count-1 downto 0 do begin
      Item:=TFPCMsgFilePoolItem(FFiles[i]);
      if Item.fUseCount=0 then begin
        Item.Free;
        FFiles.Delete(i);
      end else begin
        if ExitCode=0 then
          debugln(['TFPCMsgFilePool.Destroy file still used: ',Item.Filename]);
      end;
    end;
    if FFiles.Count>0 then begin
      if ExitCode<>0 then
        exit;
      raise Exception.Create('TFPCMsgFilePool.Destroy some files are still used');
    end;
    FreeAndNil(FFiles);
    if FPCMsgFilePool=Self then
      FPCMsgFilePool:=nil;
    inherited Destroy;
    FreeAndNil(fPendingLog);
  finally
    LeaveCriticalSection;
  end;
  DoneCriticalsection(fCritSec);
end;

function TFPCMsgFilePool.LoadCurrentEnglishFile(UpdateFromDisk: boolean;
  AThread: TThread): TFPCMsgFilePoolItem;
var
  anEnglishFile: string;
  aTranslationFile: string;
begin
  Result:=nil;
  GetMsgFileNames(EnvironmentOptions.GetParsedCompilerFilename,'','',
    anEnglishFile,aTranslationFile);
  //writeln('TFPCMsgFilePool.LoadCurrentEnglishFile ',anEnglishFile);
  if not FilenameIsAbsolute(anEnglishFile) then exit;
  Result:=LoadFile(anEnglishFile,UpdateFromDisk,AThread);
end;

function TFPCMsgFilePool.LoadFile(aFilename: string; UpdateFromDisk: boolean;
  AThread: TThread): TFPCMsgFilePoolItem;
var
  IsMainThread: Boolean;

  procedure ResultOutdated;
  begin
    // cached file needs update
    if Result.fUseCount=0 then begin
      FFiles.Remove(Result);
      Result.Free;
    end;
    Result:=nil;
  end;

  function FileExists: boolean;
  begin
    if IsMainThread then
      Result:=FileExistsCached(aFilename)
    else
      Result:=FileExistsUTF8(aFilename);
  end;

  function FileAge: longint;
  begin
    if IsMainThread then
      Result:=FileAgeCached(aFilename)
    else
      Result:=FileAgeUTF8(aFilename);
  end;

var
  Item: TFPCMsgFilePoolItem;
  i: Integer;
  NewItem: TFPCMsgFilePoolItem;
  FileTxt: string;
  ms: TMemoryStream;
  Encoding: String;
begin
  Result:=nil;
  if aFilename='' then exit;
  aFilename:=TrimAndExpandFilename(aFilename);
  //Log('TFPCMsgFilePool.LoadFile '+aFilename,aThread);

  IsMainThread:=GetThreadID=MainThreadID;
  if UpdateFromDisk then begin
    if not FileExists then begin
      Log('TFPCMsgFilePool.LoadFile file not found: '+aFilename,AThread);
      exit;
    end;
  end;
  NewItem:=nil;
  ms:=nil;
  EnterCriticalsection;
  try
    // search the newest version in cache
    for i:=FFiles.Count-1 downto 0 do begin
      Item:=TFPCMsgFilePoolItem(FFiles[i]);
      if CompareFilenames(Item.Filename,aFilename)<>0 then continue;
      Result:=Item;
      break;
    end;
    if UpdateFromDisk then begin
      if (Result<>nil)
      and (FileAge<>Result.LoadedFileAge) then
        ResultOutdated;
    end else if Result=nil then begin
      // not yet loaded, not yet checked if file exists -> check now
      if not FileExists then
        exit;
    end;

    if Result<>nil then begin
      // share
      inc(Result.fUseCount);
    end else begin
      // load for the first time
      NewItem:=TFPCMsgFilePoolItem.Create(Self,aFilename);
      //Log('TFPCMsgFilePool.LoadFile '+dbgs(NewItem.FMsgFile<>nil)+' '+aFilename,aThread);
      if Assigned(OnLoadFile) then begin
        OnLoadFile(aFilename,FileTxt);
      end else begin
        ms:=TMemoryStream.Create;
        ms.LoadFromFile(aFilename);
        SetLength(FileTxt,ms.Size);
        ms.Position:=0;
        if FileTxt<>'' then
          ms.Read(FileTxt[1],length(FileTxt));
      end;
      // convert encoding
      Encoding:=GetDefaultFPCErrorMsgFileEncoding(aFilename);
      FileTxt:=ConvertEncoding(FileTxt,Encoding,EncodingUTF8);
      // parse
      NewItem.FMsgFile.LoadFromText(FileTxt);
      NewItem.FLoadedFileAge:=FileAge;
      // load successful
      Result:=NewItem;
      NewItem:=nil;
      FFiles.Add(Result);
      inc(Result.fUseCount);
      //log('TFPCMsgFilePool.LoadFile '+Result.Filename+' '+dbgs(Result.fUseCount),aThread);
    end;
  finally
    ms.Free;
    FreeAndNil(NewItem);
    LeaveCriticalSection;
  end;
end;

procedure TFPCMsgFilePool.UnloadFile(var aFile: TFPCMsgFilePoolItem);
var
  i: Integer;
  Item: TFPCMsgFilePoolItem;
  Keep: Boolean;
begin
  EnterCriticalsection;
  try
    if aFile.fUseCount<=0 then
      raise Exception.Create('TFPCMsgFilePool.UnloadFile already freed');
    if FFiles.IndexOf(aFile)<0 then
      raise Exception.Create('TFPCMsgFilePool.UnloadFile unknown, maybe already freed');
    dec(aFile.fUseCount);
    //log('TFPCMsgFilePool.UnloadFile '+aFile.Filename+' UseCount='+dbgs(aFile.fUseCount),aThread);
    if aFile.fUseCount>0 then exit;
    // not used anymore
    if not FileExistsUTF8(aFile.Filename) then begin
      Keep:=false;
    end else begin
      // file still exist on disk
      // => check if it is the newest version
      Keep:=true;
      for i:=FFiles.Count-1 downto 0 do begin
        Item:=TFPCMsgFilePoolItem(FFiles[i]);
        if Item=aFile then break;
        if CompareFilenames(Item.Filename,aFile.Filename)<>0 then continue;
        // there is already a newer version
        Keep:=false;
        break;
      end;
    end;
    if Keep then begin
      // this file is the newest version => keep it in cache
    end else begin
      //log('TFPCMsgFilePool.UnloadFile free: '+aFile.Filename,aThread);
      FFiles.Remove(aFile);
      aFile.Free;
    end;
  finally
    aFile:=nil;
    LeaveCriticalSection;
  end;
end;

procedure TFPCMsgFilePool.EnterCriticalsection;
begin
  System.EnterCriticalsection(fCritSec);
end;

procedure TFPCMsgFilePool.LeaveCriticalSection;
begin
  System.LeaveCriticalsection(fCritSec);
end;

procedure TFPCMsgFilePool.GetMsgFileNames(CompilerFilename, TargetOS,
  TargetCPU: string; out anEnglishFile, aTranslationFile: string);
var
  FPCVer: String;
  FPCSrcDir: String;
  aFilename: String;
  ErrMsg: string;
begin
  if fMsgFileStamp<>CompilerParseStamp then begin
    fCurrentEnglishFile:=DefaultEnglishFile;
    fCurrentTranslationFile:=DefaulTranslationFile;
    // English msg file
    // => use fpcsrcdir/compiler/msg/errore.msg
    // the fpcsrcdir might depend on the FPC version
    if IsFPCExecutable(CompilerFilename,ErrMsg) then
      FPCVer:=CodeToolBoss.CompilerDefinesCache.GetFPCVersion(CompilerFilename,TargetOS,TargetCPU,false)
    else
      FPCVer:='';
    FPCSrcDir:=EnvironmentOptions.GetParsedFPCSourceDirectory(FPCVer);
    if FilenameIsAbsolute(FPCSrcDir) then begin
      // FPCSrcDir exists => use the errore.msg
      aFilename:=AppendPathDelim(FPCSrcDir)+GetForcedPathDelims('compiler/msg/errore.msg');
      if FileExistsCached(aFilename) then
        fCurrentEnglishFile:=aFilename;
    end;
    if not FileExistsCached(fCurrentEnglishFile) then begin
      // as fallback use the copy in the Codetools directory
      aFilename:=EnvironmentOptions.GetParsedLazarusDirectory;
      if FilenameIsAbsolute(aFilename) then begin
        aFilename:=AppendPathDelim(aFilename)+GetForcedPathDelims('components/codetools/fpc.errore.msg');
        if FileExistsCached(aFilename) then
          fCurrentEnglishFile:=aFilename;
      end;
    end;
    // translation msg file
    aFilename:=EnvironmentOptions.GetParsedCompilerMessagesFilename;
    if FilenameIsAbsolute(aFilename) and FileExistsCached(aFilename)
    and (CompareFilenames(aFilename,fCurrentEnglishFile)<>0) then
      fCurrentTranslationFile:=aFilename;
    fMsgFileStamp:=CompilerParseStamp;
  end;
  anEnglishFile:=fCurrentEnglishFile;
  aTranslationFile:=fCurrentTranslationFile;
end;

{ TFPCMsgFilePoolItem }

constructor TFPCMsgFilePoolItem.Create(aPool: TFPCMsgFilePool;
  const aFilename: string);
begin
  inherited Create;
  FPool:=aPool;
  FFilename:=aFilename;
  FMsgFile:=TFPCMsgFile.Create;
end;

destructor TFPCMsgFilePoolItem.Destroy;
begin
  FreeAndNil(FMsgFile);
  FFilename:='';
  inherited Destroy;
end;

function TFPCMsgFilePoolItem.GetMsg(ID: integer): TFPCMsgItem;
begin
  Result:=FMsgFile.FindWithID(ID);
end;

{ TIDEFPCParser }

destructor TIDEFPCParser.Destroy;
begin
  FreeAndNil(VirtualProjectFiles);
  FreeAndNil(FFilesToIgnoreUnitNotUsed);
  FreeAndNil(fFileExists);
  FreeAndNil(fCurSource);
  if TranslationFile<>nil then
    MsgFilePool.UnloadFile(TranslationFile);
  if MsgFile<>nil then
    MsgFilePool.UnloadFile(MsgFile);
  FreeAndNil(DirectoryStack);
  FreeAndNil(fLineToMsgID);
  inherited Destroy;
end;

procedure TIDEFPCParser.Init;

  procedure LoadMsgFile(aFilename: string; var List: TFPCMsgFilePoolItem);
  begin
    //debugln(['TFPCParser.Init load Msg filename=',aFilename]);
    if aFilename='' then
      debugln(['WARNING: TFPCParser.Init missing msg file'])
    else if (aFilename<>'') and (List=nil) then begin
      try
        List:=MsgFilePool.LoadFile(aFilename,true,nil);
        {$IFDEF VerboseExtToolThread}
        debugln(['LoadMsgFile successfully read ',aFilename]);
        {$ENDIF}
      except
        on E: Exception do begin
          debugln(['WARNING: TFPCParser.Init failed to load file '+aFilename+': '+E.Message]);
        end;
      end;
    end;
  end;

var
  i: Integer;
  Param: String;
  p: PChar;
  aTargetOS: String;
  aTargetCPU: String;
  aProject: TLazProject;
  aProjFile: TLazProjectFile;
begin
  inherited Init;

  PC_FullVersion:=GetDefaultPCFullVersion;

  if MsgFilePool<>nil then begin
    aTargetOS:='';
    aTargetCPU:='';
    for i:=0 to Tool.Process.Parameters.Count-1 do begin
      Param:=Tool.Process.Parameters[i];
      if Param='' then continue;
      p:=PChar(Param);
      if p^<>'-' then continue;
      if p[1]='T' then
        aTargetOS:=copy(Param,3,255)
      else if p[1]='P' then
        aTargetCPU:=copy(Param,3,255);
    end;
    MsgFilePool.GetMsgFileNames(Tool.Process.Executable,aTargetOS,aTargetCPU,
      MsgFilename,TranslationFilename);
  end;

  LoadMsgFile(MsgFilename,MsgFile);
  if TranslationFilename<>'' then
    LoadMsgFile(TranslationFilename,TranslationFile);

  // get include search path
  fIncludePathValidForWorkerDir:=Tool.WorkerDirectory;
  fIncludePath:=CodeToolBoss.GetIncludePathForDirectory(
                           ChompPathDelim(fIncludePathValidForWorkerDir));
  // get unit search path
  fUnitPathValidForWorkerDir:=Tool.WorkerDirectory;
  fUnitPath:=CodeToolBoss.GetUnitPathForDirectory(
                           ChompPathDelim(fUnitPathValidForWorkerDir));

  // get instantfpc cache directory
  InstantFPCCache:='$(InstantFPCCache)';
  if IDEMacros.SubstituteMacros(InstantFPCCache) then
    InstantFPCCache:=AppendPathDelim(InstantFPCCache)
  else
    InstantFPCCache:='';

  // get TestBuildDir
  if Tool.CurrentDirectoryIsTestDir then begin
    // source filenames in CurrentDirectory must be reversed back
    // -> store the list of virtual filenames (needed by worker thread)
    TestBuildDir:=AppendPathDelim(ResolveDots(Tool.Process.CurrentDirectory));
    if VirtualProjectFiles=nil then
      VirtualProjectFiles:=TFilenameToPointerTree.Create(true);
    aProject:=LazarusIDE.ActiveProject;
    for i:=0 to aProject.FileCount-1 do begin
      aProjFile:=aProject.Files[i];
      if aProjFile.IsPartOfProject and (not FilenameIsAbsolute(aProjFile.Filename)) then
        VirtualProjectFiles[aProjFile.Filename]:=Tool;
    end;
  end else
    TestBuildDir:='';
end;

procedure TIDEFPCParser.InitReading;

  procedure AddPatternItem(MsgID: integer);
  var
    Item: TFPCMsgItem;
  begin
    Item:=MsgFile.GetMsg(MsgID);
    if Item<>nil then
      fLineToMsgID.AddLines(Item.Pattern,Item.ID);
  end;

var
  p: TExtToolParserSyncPhase;
begin
  inherited InitReading;

  fLineToMsgID.Clear;
  AddPatternItem(FPCMsgIDLogo);
  AddPatternItem(FPCMsgIDLinking);
  AddPatternItem(FPCMsgIDCallingResourceCompiler);
  //fLineToMsgID.WriteDebugReport;

  for p:=low(fLastWorkerImprovedMessage) to high(fLastWorkerImprovedMessage) do
    fLastWorkerImprovedMessage[p]:=-1;

  FreeAndNil(DirectoryStack);
end;

procedure TIDEFPCParser.Done;
begin
  FreeAndNil(fCurSource);
  inherited Done;
end;

function TIDEFPCParser.CheckForCompilingState(p: PChar): boolean;
var
  OldP: PChar;
  AFilename: string;
  aDir: String;
  MsgLine: TMessageLine;
  NewFilename: String;
begin
  OldP:=p;
  // for example 'Compiling ./subdir/unit1.pas'
  if fMsgID=0 then begin
    if not ReadString(p,'Compiling ') then exit(false);
    fMsgID:=FPCMsgIDCompiling;
    Result:=true;
  end else if fMsgID=FPCMsgIDCompiling then begin
    Result:=true;
    if not ReadString(p,'Compiling ') then exit;
  end else begin
    exit(false);
  end;
  // add path to history
  if (p^='.') and (p[1]=PathDelim) then
    inc(p,2); // skip ./
  AFilename:=TrimFilename(p);
  aDir:=ExtractFilePath(AFilename);
  if aDir<>'' then begin
    // make absolute
    if (not FilenameIsAbsolute(aDir)) and (Tool.WorkerDirectory<>'') then begin
      aDir:=TrimFilename(AppendPathDelim(Tool.WorkerDirectory)+aDir);
      AFilename:=aDir+ExtractFileName(AFilename);
    end;
    // reverse instantfpc cache
    if (InstantFPCCache<>'') and (Tool.WorkerDirectory<>'')
    and (FilenameIsAbsolute(aDir))
    and (CompareFilenames(InstantFPCCache,aDir)=0) then
    begin
      NewFilename:=AppendPathDelim(Tool.WorkerDirectory)+ExtractFileName(AFilename);
      if FileExists(NewFilename,false) then begin
        AFilename:=NewFilename;
        aDir:=Tool.WorkerDirectory;
      end;
    end;
    // store directory
    if DirectoryStack=nil then DirectoryStack:=TStringList.Create;
    if (DirectoryStack.Count=0)
    or (DirectoryStack[DirectoryStack.Count-1]<>aDir) then
      DirectoryStack.Add(aDir);
  end;
  MsgLine:=CreateMsgLine;
  MsgLine.Urgency:=mluProgress;
  MsgLine.SubTool:=DefaultSubTool;
  MsgLine.Filename:=AFilename;
  MsgLine.Msg:=OldP;
  inherited AddMsgLine(MsgLine);
  Result:=true;
end;

function TIDEFPCParser.CheckForAssemblingState(p: PChar): boolean;
var
  MsgLine: TMessageLine;
  OldP: PChar;
begin
  Result:=fMsgID=9001;
  if (not Result) and (fMsgID>0) then exit;
  OldP:=p;
  if (not Result) and (not CompStr('Assembling ',p)) then exit;
  MsgLine:=CreateMsgLine;
  MsgLine.Urgency:=mluProgress;
  MsgLine.SubTool:=DefaultSubTool;
  MsgLine.Urgency:=mluProgress;
  MsgLine.Msg:=OldP;
  inherited AddMsgLine(MsgLine);
  Result:=true;
end;

function TIDEFPCParser.CheckForGeneralMessage(p: PChar): boolean;
{ check for
  Fatal: message
  Hint: (11030) Start of reading config file /etc/fpc.cfg
  Error: /usr/bin/ppc386 returned an error exitcode
}
const
  FrontEndFPCExitCodeError = 'returned an error exitcode';
var
  MsgLine: TMessageLine;
  MsgType: TMessageLineUrgency;
  p2: PChar;
  i: Integer;
  TranslatedItem: TFPCMsgItem;
  MsgItem: TFPCMsgItem;
  TranslatedMsg: String;

  procedure CheckFinalNote;
  // check if there was already an error message
  // if yes, then downgrade this message to a mluVerbose
  var
    u: TMessageLineUrgency;
  begin
    for u:=mluError to high(TMessageLineUrgency) do
      if Tool.WorkerMessages.UrgencyCounts[u]>0 then
      begin
        MsgType:=mluVerbose;
        exit;
      end;
  end;

begin
  Result:=false;
  MsgType:=mluNone;
  if ReadString(p,'Fatal: ') then begin
    MsgType:=mluFatal;
    // check for "Fatal: compilation aborted"
    if fMsgItemCompilationAborted=nil then begin
      fMsgItemCompilationAborted:=MsgFile.GetMsg(FPCMsgIDCompilationAborted);
      if fMsgItemCompilationAborted=nil then
        fMsgItemCompilationAborted:=fMissingFPCMsgItem;
    end;
    p2:=p;
    if (fMsgItemCompilationAborted<>fMissingFPCMsgItem)
    and ReadString(p2,fMsgItemCompilationAborted.Pattern) then
      CheckFinalNote;
  end
  else if ReadString(p,'Panic') then
    MsgType:=mluPanic
  else if ReadString(p,'Error: ') then begin
    // check for fpc frontend message "Error: /usr/bin/ppc386 returned an error exitcode"
    TranslatedMsg:=p;
    MsgType:=mluError;
    if Pos(FrontEndFPCExitCodeError,TranslatedMsg)>0 then begin
      fMsgID:=FPCMsgIDCompilationAborted;
      CheckFinalNote;
    end;
  end
  else if ReadString(p,'Warn: ') then
    MsgType:=mluWarning
  else if ReadString(p,'Note: ') then
    MsgType:=mluNote
  else if ReadString(p,'Hint: ') then
    MsgType:=mluHint
  else if ReadString(p,'Debug: ') then
    MsgType:=mluDebug
  else begin
    exit;
  end;
  if MsgType=mluNone then exit;

  Result:=true;
  while p^ in [' ',#9] do inc(p);
  TranslatedMsg:='';
  if (p^='(') and (p[1] in ['0'..'9']) then begin
    p2:=p;
    inc(p2);
    i:=0;
    while (p2^ in ['0'..'9']) and (i<1000000) do begin
      i:=i*10+ord(p2^)-ord('0');
      inc(p2);
    end;
    if p2^=')' then begin
      fMsgID:=i;
      p:=p2+1;
      while p^ in [' ',#9] do inc(p);
      //if Pos('reading',String(p))>0 then
      //  debugln(['TFPCParser.CheckForGeneralMessage ID=',fMsgID,' Msg=',p]);
      if (fMsgID>0) then begin
        TranslatedItem:=nil;
        MsgItem:=nil;
        if (MsgFile<>nil) then
          MsgItem:=MsgFile.GetMsg(fMsgID);
        if (TranslationFile<>nil) then
          TranslatedItem:=TranslationFile.GetMsg(fMsgID);
        Translate(p,MsgItem,TranslatedItem,TranslatedMsg,MsgType);
        if (TranslatedItem=nil) and (MsgItem=nil) then begin
          if ConsoleVerbosity>=1 then
            debugln(['TFPCParser.CheckForGeneralMessage msgid not found: ',fMsgID]);
        end;
      end;

    end;
  end;
  if (MsgType>=mluError) and (fMsgID=FPCMsgIDCompilationAborted) // fatal: Compilation aborted
  then begin
    CheckFinalNote;
  end;
  MsgLine:=CreateMsgLine;
  MsgLine.Urgency:=MsgType;
  MsgLine.SubTool:=DefaultSubTool;
  MsgLine.Msg:=p;
  MsgLine.TranslatedMsg:=TranslatedMsg;
  AddMsgLine(MsgLine);
end;

function TIDEFPCParser.CheckForLineProgress(p: PChar): boolean;
// for example:  600 206.521/231.648 Kb Used
var
  OldP: PChar;
  MsgLine: TMessageLine;
begin
  Result:=false;
  OldP:=p;
  if not ReadNumberWithThousandSep(p) then exit;
  if not ReadChar(p,' ') then exit;
  if not ReadNumberWithThousandSep(p) then exit;
  if not ReadChar(p,'/') then exit;
  if not ReadNumberWithThousandSep(p) then exit;
  if not ReadChar(p,' ') then exit;
  MsgLine:=CreateMsgLine;
  MsgLine.SubTool:=DefaultSubTool;
  MsgLine.Urgency:=mluProgress;
  MsgLine.Msg:=OldP;
  inherited AddMsgLine(MsgLine);
  Result:=true;
end;

function TIDEFPCParser.CheckForLinesCompiled(p: PChar): boolean;
var
  OldStart: PChar;
  MsgLine: TMessageLine;
begin
  Result:=fMsgID=FPCMsgIDLinesCompiled;
  if (not Result) and (fMsgID>0) then exit;
  OldStart:=p;
  if not Result then begin
    if not ReadNumberWithThousandSep(p) then exit;
    if not ReadString(p,' lines compiled, ') then exit;
    if not ReadNumberWithThousandSep(p) then exit;
  end;
  Result:=true;
  MsgLine:=CreateMsgLine;
  MsgLine.SubTool:=DefaultSubTool;
  if ShowLinesCompiled then
    MsgLine.Urgency:=mluImportant
  else
    MsgLine.Urgency:=mluVerbose;
  MsgLine.Msg:=OldStart;
  inherited AddMsgLine(MsgLine);
end;

function TIDEFPCParser.CheckForExecutableInfo(p: PChar): boolean;
{ For example:
Size of Code: 1184256 bytes
Size of initialized data: 519168 bytes
Size of uninitialized data: 83968 bytes
Stack space reserved: 262144 bytes
Stack space commited: 4096 bytes
}
var
  OldStart: PChar;
  MsgLine: TMessageLine;
begin
  Result:=(fMsgID>=9130) and (fMsgID<=9140);
  if (not Result) and (fMsgID>0) then exit;
  OldStart:=p;
  if (not Result) then begin
    if not (ReadString(p,'Size of Code: ') or
            ReadString(p,'Size of initialized data: ') or
            ReadString(p,'Size of uninitialized data: ') or
            ReadString(p,'Stack space reserved: ') or
            ReadString(p,'Stack space commited: ') or // message contains typo
            ReadString(p,'Stack space committed: ')) then exit;
    if not ReadNumberWithThousandSep(p) then exit;
    if not ReadString(p,' bytes') then exit;
  end;
  Result:=true;
  MsgLine:=CreateMsgLine;
  MsgLine.SubTool:=DefaultSubTool;
  MsgLine.Urgency:=mluProgress;
  MsgLine.Msg:=OldStart;
  inherited AddMsgLine(MsgLine);
end;

function TIDEFPCParser.CheckForWindresErrors(p: PChar): boolean;
// example: ...\windres.exe: warning: ...
var
  MsgLine: TMessageLine;
  WPos: PChar;
begin
  Result := false;
  WPos:=FindSubStrI('windres',p);
  if WPos=nil then exit;
  Result:=true;
  MsgLine:=CreateMsgLine;
  MsgLine.SubTool:=SubToolFPCWindRes;
  MsgLine.Urgency:=mluWarning;
  p := wPos + 7;
  if CompStr('.exe', p) then
    inc(p, 4);
  MsgLine.Msg:='windres' + p;
  AddMsgLine(MsgLine);
end;

function TIDEFPCParser.CheckForLinkerErrors(p: PChar): boolean;
const
  patUndefinedSymbol: String = 'Undefined symbols for architecture';
  patLD: String = '/usr/bin/ld: ';
var
  MsgLine: TMessageLine;
begin
  if CompareMem(PChar(patUndefinedSymbol),p,length(patUndefinedSymbol)) then
  begin
    MsgLine:=CreateMsgLine;
    MsgLine.MsgID:=0;
    MsgLine.SubTool:=SubToolFPCLinker;
    MsgLine.Urgency:=mluError;
    MsgLine.Msg:='linker: '+p;
    inherited AddMsgLine(MsgLine);
    exit(true);
  end;
  if CompareMem(PChar(patLD),p,length(patLD)) then
  begin
    MsgLine:=CreateMsgLine;
    MsgLine.MsgID:=0;
    MsgLine.SubTool:=SubToolFPCLinker;
    MsgLine.Urgency:=mluVerbose;
    MsgLine.Msg:='linker: '+p;
    inherited AddMsgLine(MsgLine);
    exit(true);
  end;
  Result:=false;
end;

function TIDEFPCParser.CheckForAssemblerErrors(p: PChar): boolean;
// example:
//   <stdin>:227:9: error: unsupported directive '.stabs'
var
  APos: PChar;
  s: string;
  MsgLine: TMessageLine;
begin
  Result:=false;
  APos:=FindSubStrI('error: unsupported directive',p);
  if APos=nil then exit;
  Result:=true;
  MsgLine:=CreateMsgLine;
  MsgLine.SubTool:=SubToolFPCWindRes;
  MsgLine.Urgency:=mluError;
  s:=APos;
  if Pos('.stabs',s)>0 then
    s+='. Hint: Use another type of debug info.';
  MsgLine.Msg:='assembler: '+s;
  AddMsgLine(MsgLine);
end;

function TIDEFPCParser.CheckForInfos(p: PChar): boolean;

  function ReadFPCLogo(PatternItem: PPatternToMsgID;
    out FPCVersionAsInt: LongWord): boolean;
  var
    Line: string;
    Ranges: TFPCMsgRanges;
    aRange: PFPCMsgRange;
    i: SizeInt;
    aFPCFullVersion: String;
    FPCVersion: integer;
    FPCRelease: integer;
    FPCPatch: integer;
  begin
    Result:=false;
    FPCVersionAsInt:=0;
    i:=Pos('$FPCFULLVERSION',PatternItem^.Pattern);
    if i<1 then exit;
    Line:=p;
    Ranges:=nil;
    try
      ExtractFPCMsgParameters(PatternItem^.Pattern,Line,Ranges);
      if Ranges.Count>0 then begin
        // first is $FPCFULLVERSION
        aRange:=@Ranges.Ranges[0];
        aFPCFullVersion:=copy(Line,aRange^.StartPos+1,aRange^.EndPos-aRange^.StartPos);
        SplitFPCVersion(aFPCFullVersion,FPCVersion,FPCRelease,FPCPatch);
        FPCVersionAsInt:=FPCVersion*10000+FPCRelease*100+FPCPatch;
        Result:=FPCVersionAsInt>0;
      end;
      // second is $FPCDATE
      // third is $FPCCPU
    finally
      Ranges.Free;
    end;
  end;

var
  MsgItem: TFPCMsgItem;
  MsgLine: TMessageLine;
  MsgType: TMessageLineUrgency;
  PatternItem: PPatternToMsgID;
  aFPCVersion: LongWord;
begin
  Result:=false;
  PatternItem:=fLineToMsgID.LineToPattern(p);
  if PatternItem=nil then exit;
  fMsgID:=PatternItem^.MsgID;
  if (fMsgID=FPCMsgIDLogo) and (DirectoryStack<>nil) then begin
    // a new call of the compiler (e.g. when compiling via make)
    // => clear stack
    FreeAndNil(DirectoryStack);
  end;
  MsgItem:=MsgFile.GetMsg(fMsgID);
  if MsgItem=nil then exit;
  Result:=true;
  MsgType:=FPCMsgToMsgUrgency(MsgItem);
  if MsgType=mluNone then
    MsgType:=mluVerbose;
  MsgLine:=CreateMsgLine;
  MsgLine.SubTool:=DefaultSubTool;
  MsgLine.Urgency:=MsgType;
  if (fMsgID=FPCMsgIDLogo) and ReadFPCLogo(PatternItem,aFPCVersion) then begin
    if aFPCVersion<>PC_FullVersion then begin
      // unexpected FPC version => always show
      MsgLine.Urgency:=mluImportant;
      PC_FullVersion:=aFPCVersion;
    end;
  end;
  AddMsgLine(MsgLine);
end;

function TIDEFPCParser.CreateMsgLine: TMessageLine;
begin
  Result:=inherited CreateMsgLine(fOutputIndex);
  Result.MsgID:=fMsgID;
end;

procedure TIDEFPCParser.AddLinkingMessages;
{ Add messages for all output between "Linking ..." and the
  current line "Error while linking"

For example:
  Linking /home/user/project1
  /usr/bin/ld: warning: /home/user/link.res contains output sections; did you forget -T?
  /usr/bin/ld: cannot find -la52
  project1.lpr(20,1) Error: Error while linking

  Examples for linking errors:
  linkerror.o(.text$_main+0x9):linkerror.pas: undefined reference to `NonExistingFunction'

  /path/lib/x86_64-linux/blaunit.o: In function `FORMCREATE':
  /path//blaunit.pas:45: undefined reference to `BLAUNIT_BLABLA'

  Closing script ppas.sh

  Mac OS X linker example:
  ld: framework not found Cocoas
  Note: this comes in stderr, so it might be some lines after corresponding stdout

  Multiline Mac OS X linker example:
  Undefined symbols:
    "_exterfunc", referenced from:
        _PASCALMAIN in testld.o
    "_exterfunc2", referenced from:
        _PASCALMAIN in testld.o
  ld: symbol(s) not found

  Linking project1
  Undefined symbols for architecture x86_64:
    "_GetCurrentEventButtonState", referenced from:
        _COCOAINT_TCOCOAWIDGETSET_$__GETKEYSTATE$LONGINT$$SMALLINT in cocoaint.o
  ld: symbol(s) not found for architecture x86_64
  An error occurred while linking

  Linking IDE:
  (9015) Linking ../lazarus
  /usr/bin/ld: cannot find -lGL
  make[2]: *** [lazarus] Error 1
  make[1]: *** [idepkg] Error 2
  make: *** [idepkg] Error 2
  /home/mattias/pascal/wichtig/lazarus/ide/lazarus.pp(161,1) Error: (9013) Error while linking

}
var
  i: Integer;
  MsgLine: TMessageLine;
begin
  // change all low urgency messages in front of the last message to Important
  i:=Tool.WorkerMessages.Count-1;
  while i>=0 do begin
    MsgLine:=Tool.WorkerMessages[i];
    //debugln(['TIDEFPCParser.AddLinkingMessages ',i,' ',dbgs(MsgLine.Urgency),' ',MsgLine.Msg]);
    if MsgLine.Urgency<mluHint then
      MsgLine.Urgency:=mluImportant
    else
      break;
    dec(i);
  end;

  // add all skipped lines in front of the linking error
  i:=Tool.WorkerMessages.Count-1;
  if i<0 then exit;
  MsgLine:=Tool.WorkerMessages[i];
  //debugln(['TIDEFPCParser.AddLinkingMessages MsgLine.OutputIndex=',MsgLine.OutputIndex,' fOutputIndex=',fOutputIndex]);
  for i:=MsgLine.OutputIndex+1 to fOutputIndex-1 do begin
    MsgLine:=inherited CreateMsgLine(i);
    MsgLine.MsgID:=0;
    MsgLine.SubTool:=SubToolFPCLinker;
    if MsgLine.Msg<>'' then
      MsgLine.Urgency:=mluImportant
    else
      MsgLine.Urgency:=mluVerbose2;
    inherited AddMsgLine(MsgLine);
  end;
end;

procedure TIDEFPCParser.AddResourceMessages;
{  Add messages for all output between "Calling resource compiler " and the
  current line "Error while compiling resources"

For example:
  Calling resource compiler "/usr/bin/fpcres" with "-o /home/user/project1.or -a x86_64 -of elf -v "@/home/user/project1.reslst"" as command line
  Debug: parsing command line parameters
  ...
  Error: Error while compiling resources
}
var
  i: Integer;
  MsgLine: TMessageLine;
begin
  // find message "Calling resource compiler ..."
  i:=Tool.WorkerMessages.Count-1;
  while (i>=0) and (Tool.WorkerMessages[i].MsgID<>FPCMsgIDCallingResourceCompiler) do
    dec(i);
  if i<0 then exit;
  MsgLine:=Tool.WorkerMessages[i];
  for i:=MsgLine.OutputIndex+1 to fOutputIndex-1 do begin
    MsgLine:=inherited CreateMsgLine(i);
    MsgLine.MsgID:=0;
    MsgLine.SubTool:=SubToolFPCRes;
    if MsgLine.Msg<>'' then
      MsgLine.Urgency:=mluHint
    else
      MsgLine.Urgency:=mluVerbose2;
    inherited AddMsgLine(MsgLine);
  end;
end;

function TIDEFPCParser.NeedSource(aPhase: TExtToolParserSyncPhase;
  SourceOk: boolean): boolean;
begin
  if SourceOk then exit(false);
  case aPhase of
  etpspAfterReadLine: NeedSynchronize:=true;
  etpspSynchronized: NeedAfterSync:=true;
  end;
  Result:=true;
end;

function TIDEFPCParser.IsMsgID(MsgLine: TMessageLine; MsgID: integer;
  var Item: TFPCMsgItem): boolean;
begin
  if MsgLine.MsgID=MsgID then exit(true);
  Result:=false;
  if MsgLine.MsgID<>0 then exit;
  if MsgLine.SubTool<>DefaultSubTool then exit;
  if Item=nil then begin
    Item:=MsgFile.GetMsg(MsgID);
    if Item=nil then
      Item:=fMissingFPCMsgItem;
  end;
  if Item=fMissingFPCMsgItem then exit;
  if Item.PatternFits(MsgLine.Msg)<0 then exit;
  MsgLine.MsgID:=MsgID;
  Result:=true;
end;

procedure TIDEFPCParser.ImproveMsgHiddenByIDEDirective(
  aPhase: TExtToolParserSyncPhase; MsgLine: TMessageLine; SourceOK: Boolean);
// check for {%H-}

  function IsH(p: PChar): boolean; inline;
  begin
    Result:=(p^='{') and (p[1]='%') and (p[2]='H') and (p[3]='-');
  end;

var
  p: PChar;
  X: Integer;
  Y: Integer;
  HasDirective: Boolean;
  AbsPos: Integer; // 0-based
  OtherPos: Integer;
  AtomEnd: integer;
begin
  if MsgLine.Urgency>=mluError then exit;
  if mlfHiddenByIDEDirectiveValid in MsgLine.Flags then exit;
  if NeedSource(aPhase,SourceOK) then
    exit;

  X:=MsgLine.Column;
  Y:=MsgLine.Line;
  if (y<=fCurSource.LineCount) and (x-1<=fCurSource.GetLineLength(y-1))
  then begin
    HasDirective:=false;
    AbsPos:=fCurSource.GetLineStart(y-1)+x-2; // 0-based
    p:=PChar(fCurSource.Source)+AbsPos;
    //debugln(['TFPCParser.ImproveMsgHiddenByIDEDirective ',MsgLine.Filename,' ',Y,',',X,' ',copy(fCurSource.GetLine(y-1),1,x-1),'|',copy(fCurSource.GetLine(y-1),x,100),' p=',p[0],p[1],p[2]]);
    if IsH(p) then
      // directive beginning at cursor
      HasDirective:=true
    else if (x>5) and IsH(p-5) then
      // directive ending at cursor
      HasDirective:=true
    else begin
      // different compiler versions report some message positions differently.
      // They changed some message positions from start to end of token.
      // => check other end of token
      //debugln(['TIDEFPCParser.ImproveMsgHiddenByIDEDirective mlfLeftToken=',mlfLeftToken in MsgLine.Flags]);
      if mlfLeftToken in MsgLine.Flags then begin
        if IsIdentChar[p[-1]] then begin
          OtherPos:=AbsPos+1;
          ReadPriorPascalAtom(fCurSource.Source,OtherPos,AtomEnd);
          if (OtherPos>5) and (AtomEnd=AbsPos+1)
          and IsH(@fCurSource.Source[OtherPos-5]) then begin
            // for example: {%H-}identifier|
            HasDirective:=true;
          end;
        end;
      end else begin
        if IsIdentStartChar[p^] then begin
          inc(p,GetIdentLen(p));
          if IsH(p) then
            // for example: |identifier{%H-}
            HasDirective:=true;
        end;
      end;
    end;
    if HasDirective then begin
      MsgLine.Flags:=MsgLine.Flags+[mlfHiddenByIDEDirective,
        mlfHiddenByIDEDirectiveValid];
      exit;
    end;
  end;
  MsgLine.Flags:=MsgLine.Flags+[mlfHiddenByIDEDirectiveValid];
end;

procedure TIDEFPCParser.ImproveMsgSenderNotUsed(
  aPhase: TExtToolParserSyncPhase; MsgLine: TMessageLine);
// FPCMsgIDParameterNotUsed = 5024;  Parameter "$1" not used
begin
  if aPhase<>etpspAfterReadLine then exit;
  if (MsgLine.Urgency<=mluVerbose) then exit;
  // check for Sender not used
  if HideHintsSenderNotUsed
  and (MsgLine.Msg='Parameter "Sender" not used') then begin
    MsgLine.Urgency:=mluVerbose;
  end;
end;

procedure TIDEFPCParser.ImproveMsgUnitNotUsed(aPhase: TExtToolParserSyncPhase;
  MsgLine: TMessageLine);
// check for Unit not used message in main sources
// and change urgency to merely 'verbose'
begin
  if aPhase<>etpspAfterReadLine then exit;
  if (MsgLine.Urgency<=mluVerbose) then exit;
  if not IsMsgID(MsgLine,FPCMsgIDUnitNotUsed,fMsgItemUnitNotUsed) then exit;

  //debugln(['TIDEFPCParser.ImproveMsgUnitNotUsed ',aPhase=etpspSynchronized,' ',MsgLine.Msg]);
  // unit not used
  if IndexInStringList(FilesToIgnoreUnitNotUsed,cstFilename,MsgLine.Filename)>=0 then
  begin
    MsgLine.Urgency:=mluVerbose;
  end else if HideHintsUnitNotUsedInMainSource then begin
    if CompareFileExt(MsgLine.Filename, 'lpr', false)=0 then
      // a lpr does not use a unit => not important
      MsgLine.Urgency:=mluVerbose
    else if FilenameIsAbsolute(MsgLine.Filename)
    and FileExists(ChangeFileExt(MsgLine.Filename, '.lpk'), aPhase=etpspSynchronized)
    then begin
      // a lpk does not use a unit => not important
      MsgLine.Urgency:=mluVerbose;
    end;
  end;
end;

procedure TIDEFPCParser.ImproveMsgUnitNotFound(aPhase: TExtToolParserSyncPhase;
  MsgLine: TMessageLine);

  procedure FixSourcePos(CodeBuf: TCodeBuffer; MissingUnitname: string);
  var
    InPos: Integer;
    NamePos: Integer;
    Tool: TCodeTool;
    Caret: TCodeXYPosition;
    NewFilename: String;
  begin
    {$IFDEF VerboseFPCMsgUnitNotFound}
    debugln(['TIDEFPCParser.ImproveMsgUnitNotFound File=',CodeBuf.Filename]);
    {$ENDIF}
    LazarusIDE.SaveSourceEditorChangesToCodeCache(nil);
    if not CodeToolBoss.FindUnitInAllUsesSections(CodeBuf,MissingUnitname,NamePos,InPos)
    then begin
      DebugLn('TIDEFPCParser.ImproveMsgUnitNotFound FindUnitInAllUsesSections failed due to syntax errors or '+MissingUnitname+' is not used in '+CodeBuf.Filename);
      exit;
    end;
    Tool:=CodeToolBoss.CurCodeTool;
    if Tool=nil then exit;
    if not Tool.CleanPosToCaret(NamePos,Caret) then exit;
    if (Caret.X>0) and (Caret.Y>0) then begin
      //DebugLn('QuickFixUnitNotFoundPosition Line=',dbgs(Line),' Col=',dbgs(Col));
      NewFilename:=Caret.Code.Filename;
      MsgLine.SetSourcePosition(NewFilename,Caret.Y,Caret.X);
    end;
  end;

  procedure FindPPUFiles(MissingUnitname: string; PkgList: TFPList;
    PPUFiles: TStringList // Strings:PPUFilename, Objects:TIDEPackage
    );
  var
    i: Integer;
    Pkg: TIDEPackage;
    DirCache: TCTDirectoryCache;
    PPUFilename: String;
    UnitOutDir: String;
  begin
    if PkgList=nil then exit;
    for i:=0 to PkgList.Count-1 do begin
      Pkg:=TIDEPackage(PkgList[i]);
      UnitOutDir:=Pkg.LazCompilerOptions.GetUnitOutputDirectory(false);
      //debugln(['TQuickFixUnitNotFoundPosition.Execute ',Pkg.Name,' UnitOutDir=',UnitOutDir]);
      if not FilenameIsAbsolute(UnitOutDir) then continue;
      DirCache:=CodeToolBoss.DirectoryCachePool.GetCache(UnitOutDir,true,false);
      PPUFilename:=DirCache.FindFile(MissingUnitname+'.ppu',ctsfcLoUpCase);
      //debugln(['TQuickFixUnitNotFoundPosition.Execute ShortPPU=',PPUFilename]);
      if PPUFilename='' then continue;
      PPUFilename:=AppendPathDelim(DirCache.Directory)+PPUFilename;
      PPUFiles.AddObject(PPUFilename,Pkg);
    end;
  end;

  procedure FindPPUInInstalledPkgs(MissingUnitname: string;
    PPUFiles: TStringList // Strings:PPUFilename, Objects:TIDEPackage
    );
  var
    i: Integer;
    Pkg: TIDEPackage;
    PkgList: TFPList;
  begin
    // search ppu in installed packages
    PkgList:=TFPList.Create;
    try
      for i:=0 to PackageEditingInterface.GetPackageCount-1 do begin
        Pkg:=PackageEditingInterface.GetPackages(i);
        if Pkg.AutoInstall=pitNope then continue;
        PkgList.Add(Pkg);
      end;
      FindPPUFiles(MissingUnitname,PkgList,PPUFiles);
    finally
      PkgList.Free;
    end;
  end;

  procedure FindPPUInModuleAndDeps(MissingUnitname: string; Module: TObject;
    PPUFiles: TStringList // Strings:PPUFilename, Objects:TIDEPackage
    );
  var
    PkgList: TFPList;
  begin
    PkgList:=nil;
    try
      PackageEditingInterface.GetRequiredPackages(Module,PkgList);
      if (Module is TIDEPackage) then begin
        if PkgList=nil then
          PkgList:=TFPList.Create;
        if PkgList.IndexOf(Module)<0 then
          PkgList.Add(Module);
      end;
      FindPPUFiles(MissingUnitname,PkgList,PPUFiles);
    finally
      PkgList.Free;
    end;
  end;

  procedure FindPackage(MissingUnitname: string; OnlyInstalled: boolean;
    out Pkg: TIDEPackage; out PkgName: string; out PkgFile: TLazPackageFile);
  var
    i: Integer;
    j: Integer;
    aFile: TLazPackageFile;
    CurPkg: TIDEPackage;
  begin
    PkgName:='';
    PkgFile:=nil;
    Pkg:=nil;
    // search unit in packages
    for i:=0 to PackageEditingInterface.GetPackageCount-1 do begin
      CurPkg:=PackageEditingInterface.GetPackages(i);
      if OnlyInstalled and (CurPkg.AutoInstall=pitNope) then
        continue;
      if CompareTextCT(CurPkg.Name,MissingUnitname)=0 then begin
        PkgName:=CurPkg.Name;
        Pkg:=CurPkg;
        break;
      end;
      for j:=0 to CurPkg.FileCount-1 do begin
        aFile:=CurPkg.Files[j];
        if not (aFile.FileType in PkgFileRealUnitTypes) then
          continue;
        if CompareTextCT(ExtractFileNameOnly(aFile.Filename),MissingUnitname)<>0
        then continue;
        if (PkgFile=nil) or (aFile.InUses and not PkgFile.InUses) then
        begin
          // a better file was found
          PkgFile:=aFile;
          PkgName:=CurPkg.Name;
          Pkg:=CurPkg;
        end;
      end;
    end;
  end;

var
  MissingUnitName: string;
  UsedByUnit: string;
  Filename: String;
  NewFilename: String;
  CodeBuf: TCodeBuffer;
  Owners: TFPList;
  UsedByOwner: TObject;
  UsedByPkg: TIDEPackage;
  PPUFilename: String;
  OnlyInstalled: Boolean;
  s: String;
  PPUFiles: TStringList; // Strings:PPUFilename, Objects:TIDEPackage
  i: Integer;
  DepOwner: TObject;
  TheOwner: TObject;
  MissingPkg: TIDEPackage;
  MissingPkgName: String;
  MissingPkgFile: TLazPackageFile;
  FPCUnitFilename: String;
begin
  if MsgLine.Urgency<mluError then exit;
  if not IsMsgID(MsgLine,FPCMsgIDCantFindUnitUsedBy,fMsgItemCantFindUnitUsedBy)
  then // Can't find unit $1 used by $2
    exit;
  case aPhase of
  etpspAfterReadLine:
    begin
      NeedSynchronize:=true;
      exit;
    end;
  etpspSynchronized: ;
  etpspAfterSync: exit;
  end;

  // in main thread

  if not GetFPCMsgValues(MsgLine,MissingUnitName,UsedByUnit) then
    exit;
  MsgLine.Attribute[FPCMsgAttrMissingUnit]:=MissingUnitName;
  MsgLine.Attribute[FPCMsgAttrUsedByUnit]:=UsedByUnit;

  {$IFDEF VerboseFPCMsgUnitNotFound}
  debugln(['TIDEFPCParser.ImproveMsgUnitNotFound Missing="',MissingUnitname,'" used by "',UsedByUnit,'"']);
  {$ENDIF}

  CodeBuf:=nil;
  Filename:=MsgLine.GetFullFilename;
  if (CompareFilenames(ExtractFileName(Filename),'staticpackages.inc')=0)
  and ((ExtractFilePath(Filename)='')
    or (CompareFilenames(ExtractFilePath(Filename),AppendPathDelim(GetPrimaryConfigPath))=0))
  then begin
    // common case: when building the IDE a package unit is missing
    // staticpackages.inc(1,1) Fatal: Can't find unit sqldblaz used by Lazarus
    // change to lazarus.pp(1,1)
    Filename:=AppendPathDelim(EnvironmentOptions.GetParsedLazarusDirectory)+'ide'+PathDelim+'lazarus.pp';
    MsgLine.SetSourcePosition(Filename,1,1);
    MsgLine.Msg:=Format(lisCanTFindAValidPpu, [MissingUnitname]);
  end else if SysUtils.CompareText(ExtractFileNameOnly(Filename),UsedByUnit)<>0
  then begin
    // the message belongs to another unit
    NewFilename:='';
    if FilenameIsAbsolute(Filename) then
    begin
      // For example: /path/laz/main.pp(1,1) Fatal: Can't find unit lazreport used by lazarus
      // => search source 'lazarus' in directory
      NewFilename:=CodeToolBoss.DirectoryCachePool.FindUnitInDirectory(
                                     ExtractFilePath(Filename),UsedByUnit,true);
    end;
    if NewFilename='' then begin
      TheOwner:=nil;
      if Tool.Data is TIDEExternalToolData then begin
        TheOwner:=ExternalToolList.GetIDEObject(TIDEExternalToolData(Tool.Data));
      end else if Tool.Data=nil then begin
        {$IFDEF VerboseFPCMsgUnitNotFound}
        debugln(['TIDEFPCParser.ImproveMsgUnitNotFound Tool.Data=nil, ProcDir=',Tool.Process.CurrentDirectory]);
        {$ENDIF}
      end;
      NewFilename:=LazarusIDE.FindUnitFile(UsedByUnit,TheOwner);
      if NewFilename='' then begin
        {$IFDEF VerboseFPCMsgUnitNotFound}
        debugln(['TIDEFPCParser.ImproveMsgUnitNotFound unit not found: ',UsedByUnit]);
        {$ENDIF}
      end;
    end;
    if NewFilename<>'' then
      Filename:=NewFilename;
  end;

  if FilenameIsAbsolute(Filename) or (mlfTestBuildFile in MsgLine.Flags) then begin
    CodeBuf:=CodeToolBoss.LoadFile(Filename,false,false);
    if CodeBuf=nil then begin
      {$IFDEF VerboseFPCMsgUnitNotFound}
      debugln(['TIDEFPCParser.ImproveMsgUnitNotFound unable to load unit: ',Filename]);
      {$ENDIF}
    end;
  end else begin
    {$IFDEF VerboseFPCMsgUnitNotFound}
    debugln(['TIDEFPCParser.ImproveMsgUnitNotFound unable to locate UsedByUnit: ',UsedByUnit,' Filename="',MsgLine.Filename,'" Attr[',FPCMsgAttrWorkerDirectory,']=',MsgLine.Attribute[FPCMsgAttrWorkerDirectory],' Tool.WorkerDirectory=',Tool.WorkerDirectory]);
    {$ENDIF}
  end;

  // fix line and column
  Owners:=nil;
  PPUFiles:=TStringList.Create;
  try
    UsedByOwner:=nil;
    UsedByPkg:=nil;
    if CodeBuf<>nil then begin
      FixSourcePos(CodeBuf,MissingUnitname);
      Owners:=PackageEditingInterface.GetOwnersOfUnit(CodeBuf.Filename);
      if (Owners<>nil) and (Owners.Count>0) then begin
        UsedByOwner:=TObject(Owners[0]);
        if UsedByOwner is TIDEPackage then
          UsedByPkg:=TIDEPackage(UsedByOwner);
      end;
    end;

    // if the ppu exists then improve the message
    if (CodeBuf<>nil) and FilenameIsAbsolute(CodeBuf.Filename) then begin
      {$IFDEF VerboseFPCMsgUnitNotFound}
      debugln(['TIDEFPCParser.ImproveMsgUnitNotFound Filename=',CodeBuf.Filename]);
      {$ENDIF}
      PPUFilename:=CodeToolBoss.DirectoryCachePool.FindCompiledUnitInCompletePath(
                        ExtractFilePath(CodeBuf.Filename),MissingUnitname);
      if (PPUFilename<>'') then begin
        FPCUnitFilename:=CodeToolBoss.DirectoryCachePool.FindUnitInUnitSet(
          ExtractFilePath(CodeBuf.Filename),MissingUnitName);
      end else
        FPCUnitFilename:='';
      {$IFDEF VerboseFPCMsgUnitNotFound}
      debugln(['TIDEFPCParser.ImproveMsgUnitNotFound PPUFilename=',PPUFilename,' IsFileInIDESrcDir=',IsFileInIDESrcDir(CodeBuf.Filename)]);
      {$ENDIF}
      OnlyInstalled:=IsFileInIDESrcDir(CodeBuf.Filename);
      if OnlyInstalled then begin
        FindPPUInInstalledPkgs(MissingUnitname,PPUFiles);
      end else if UsedByOwner<>nil then
        FindPPUInModuleAndDeps(MissingUnitName,UsedByOwner,PPUFiles);
      {$IFDEF VerboseFPCMsgUnitNotFound}
      debugln(['TIDEFPCParser.ImproveMsgUnitNotFound PPUFiles in PPU path=',PPUFiles.Count]);
      {$ENDIF}
      FindPackage(MissingUnitname,OnlyInstalled,MissingPkg,MissingPkgName,MissingPkgFile);
      {$IFDEF VerboseFPCMsgUnitNotFound}
      debugln(['TIDEFPCParser.ImproveMsgUnitNotFound MissingUnitPkg=',MissingPkgName]);
      {$ENDIF}
      s:=Format(lisCannotFind, [MissingUnitname]);
      if UsedByUnit<>'' then
        s+=Format(lisUsedBy, [UsedByUnit]);
      if PPUFiles.Count>0 then begin
        // there is a ppu file in a package output directory, but the compiler
        // didn't like it => change message
        if PPUFilename='' then
          PPUFilename:=PPUFiles[0];
        s+=Format(lisIncompatiblePpu, [PPUFilename]);
        if PPUFiles.Count=1 then
          s+=Format(lisPackage3, [TIDEPackage(PPUFiles.Objects[0]).Name])
        else begin
          s+=lisMultiplePack;
          for i:=0 to PPUFiles.Count-1 do begin
            if i>0 then
              s+=', ';
            s+=TIDEPackage(PPUFiles.Objects[i]).Name;
          end;
        end;
      end else if PPUFilename<>'' then begin
        if CompareFilenames(PPUFilename,FPCUnitFilename)=0 then begin
          // there is ppu in the FPC units, but the compiler does not like it
          // => a) using a wrong compiler version (wrong fpc.cfg)
          //    b) user units in fpc.cfg
          //    c) fpc units not compiled with -Ur
          //    d) wrong target platform
          s+=', ppu='+PPUFilename+', check your fpc.cfg';
        end else begin
          // there is a ppu file in the source path
          if (MissingPkg<>nil) and (MissingPkg.LazCompilerOptions.UnitOutputDirectory='')
          then
            s+='. '+lisPackageNeedsAnOutputDirectory
          else
            s+='. '+lisMakeSureAllPpuFilesOfAPackageAreInItsOutputDirecto;
          s+=' '+Format(lisPpuInWrongDirectory, [PPUFilename]);
          if MissingPkgName<>'' then
            s+=' '+Format(lisCleanUpPackage, [MissingPkgName]);
          s+='.';
        end;
      end
      else if (UsedByPkg<>nil) and (CompareTextCT(UsedByPkg.Name,MissingPkgName)=0)
      then begin
        // two units of a package cannot find each other
        s+=Format(lisCheckSearchPathPackageTryACleanRebuildCheckImpleme, [
          UsedByPkg.Name]);
        s+='.';
      end else if (MissingPkgName<>'')
      and (OnlyInstalled
        or ((UsedByOwner<>nil)
           and PackageEditingInterface.IsOwnerDependingOnPkg(UsedByOwner,MissingPkgName,DepOwner)))
      then begin
        // ppu file of an used package is missing
        if (MissingPkgFile<>nil) and (not MissingPkgFile.InUses) then
          s+=Format(lisEnableFlagUseUnitOfUnitInPackage, [MissingUnitName, MissingPkgName])
        else
          s+=Format(lisCheckIfPackageCreatesPpuCheckNothingDeletesThisFil, [
            MissingPkgName, MissingUnitName]);
        s+='.';
      end else begin
        if MissingPkgName<>'' then
          s+=Format(lisCheckIfPackageIsInTheDependencies, [MissingPkgName]);
        if UsedByOwner is TLazProject then
          s+=lisOfTheProjectInspector
        else if UsedByPkg<>nil then
          s+=Format(lisOfPackage, [UsedByPkg.Name]);
        s+='.';
      end;
      MsgLine.Msg:=s;
      {$IFDEF VerboseFPCMsgUnitNotFound}
      debugln(['TIDEFPCParser.ImproveMsgUnitNotFound Msg.Msg="',MsgLine.Msg,'"']);
      {$ENDIF}
    end;
  finally
    PPUFiles.Free;
    Owners.Free;
  end;
end;

procedure TIDEFPCParser.ImproveMsgLinkerUndefinedReference(
  aPhase: TExtToolParserSyncPhase; MsgLine: TMessageLine);

  function CheckForLinuxLDFileAndLineNumber: boolean;
  { For example:
    /path/lib/x86_64-linux/blaunit.o: In function `FORMCREATE':
    /path//blaunit.pas:45: undefined reference to `BLAUNIT_BLABLA'
  }
  var
    p: PChar;
    Msg: String;
    aFilename: String;
    LineNumber: Integer;
    i: SizeInt;
  begin
    Result:=false;
    if aPhase<>etpspAfterReadLine then exit;
    if MsgLine.HasSourcePosition then exit;
    Msg:=MsgLine.Msg;
    p:=PChar(Msg);
    // check for "filename:decimals: message"
    //  or unit1.o(.text+0x3a):unit1.pas:48: undefined reference to `DoesNotExist'

    // read filename
    repeat
      if p^=#0 then exit;
      inc(p);
    until (p^=':') and (p[1] in ['0'..'9']);
    aFilename:=LeftStr(Msg,p-PChar(Msg));
    // check for something):filename
    i:=Pos('):',aFilename);
    if i>0 then
      Delete(aFilename,1,i+1);
    aFilename:=TrimFilename(aFilename);

    // read line number
    inc(p);
    LineNumber:=0;
    while p^ in ['0'..'9'] do begin
      LineNumber:=LineNumber*10+ord(p^)-ord('0');
      if LineNumber>9999999 then exit;
      inc(p);
    end;
    if p^<>':' then exit;
    inc(p);
    while p^ in [' '] do inc(p);

    Result:=true;
    MsgLine.Msg:=copy(Msg,p-PChar(Msg)+1,length(Msg));
    MsgLine.SetSourcePosition(aFilename,LineNumber,1);
    MsgLine.Urgency:=mluError;
  end;

  function CheckForDarwinLDReferencedFrom: boolean;
  { For example:
     "_UNIT1_GIBTESNICHT", referenced from:
  }
  var
    MangledName: string;
    aComplete: boolean;
    aErrorMsg: string;
    NewCode: TCodeBuffer;
    NewX: integer;
    NewY: integer;
    NewTopLine: integer;
  begin
    Result:=false;
    if MsgLine.HasSourcePosition then exit;
    // check for '  "_FPC-Mangled-Identifier", referenced from:
    if not etFPCMsgParser.GetFPCMsgValue1(MsgLine.Msg,'  "_$1", referenced from:',
      MangledName)
    then exit;
    Result:=true;
    case aPhase of
    etpspAfterReadLine:
      begin
        NeedSynchronize:=true;
        exit;
      end;
    etpspAfterSync: exit;
    end;
    // in main thread
    CodeToolBoss.FindFPCMangledIdentifier(MangledName,aComplete,aErrorMsg,
      nil,NewCode,NewX,NewY,NewTopLine);
    if NewCode=nil then exit;
    Result:=true;
    MsgLine.SetSourcePosition(NewCode.Filename,NewY,NewX);
    MsgLine.Urgency:=mluError;
  end;

  function CheckForDarwinLDMangledInO: boolean;
  { For example:
           _UNIT1_TFORM1_$__FORMCREATE$TOBJECT in unit1.o
  }
  var
    MangledName: string;
    aUnitName: string;
    aComplete: boolean;
    aErrorMsg: string;
    NewCode: TCodeBuffer;
    NewX: integer;
    NewY: integer;
    NewTopLine: integer;
  begin
    Result:=false;
    if MsgLine.HasSourcePosition then exit;
    if not etFPCMsgParser.GetFPCMsgValues2(MsgLine.Msg,'      _$1 in $2.o',
      MangledName,aUnitName)
    then exit;
    Result:=true;
    case aPhase of
    etpspAfterReadLine:
      begin
        NeedSynchronize:=true;
        exit;
      end;
    etpspAfterSync: exit;
    end;
    // in main thread
    CodeToolBoss.FindFPCMangledIdentifier(MangledName,aComplete,aErrorMsg,
      nil,NewCode,NewX,NewY,NewTopLine);
    if NewCode=nil then exit;
    Result:=true;
    MsgLine.SetSourcePosition(NewCode.Filename,NewY,NewX);
    MsgLine.Urgency:=mluError;
  end;

begin
  if MsgLine.SubTool<>SubToolFPCLinker then exit;

  if CheckForLinuxLDFileAndLineNumber then exit;
  if CheckForDarwinLDReferencedFrom then exit;
  if CheckForDarwinLDMangledInO then exit;
end;

procedure TIDEFPCParser.ImproveMsgIdentifierPosition(
  aPhase: TExtToolParserSyncPhase; MsgLine: TMessageLine; SourceOK: boolean);
{ FPC sometimes reports the token after the identifier
  => fix the position
  Examples:
    "  i :="
    unit1.pas(42,5) Error: (5000) Identifier not found "i"

    "procedure TMyClass.DoIt  ;"
    test.pas(7,26) Error: (3047) method identifier expected
}
const
  AttrPosChecked = 'PosChecked';
var
  LineRange: TLineRange;
  Line, Col: Integer;
  p, AtomEnd: integer;
  Src: String;
  Identifier: String;
  NewP: Integer;
begin
  Col:=MsgLine.Column;
  Line:=MsgLine.Line;
  if (Col<1) or (Line<1) then
    exit;
  if (Line=1) and (Col=1) then exit;
  if MsgLine.SubTool<>SubToolFPC then exit;
  if MsgLine.MsgID=0 then begin
    // maybe not compiled with -vq: search patterns of common messages
    if (not IsMsgID(MsgLine,FPCMsgIDIdentifierNotFound,fMsgItemIdentifierNotFound))
    and (not IsMsgID(MsgLine,FPCMsgIDMethodIdentifierExpected,fMsgItemMethodIdentifierExpected))
    then
      exit;
  end;
  if MsgLine.MsgID=FPCMsgIDMethodIdentifierExpected then
    Identifier:=''
  else begin
    Identifier:=GetFPCMsgValue1(MsgLine);
    if not IsValidIdent(Identifier) then exit;
  end;

  if MsgLine.Attribute[AttrPosChecked]<>'' then exit;
  if NeedSource(aPhase,SourceOK) then
    exit;
  MsgLine.Attribute[AttrPosChecked]:=ClassName;

  //DebuglnThreadLog(['Old Line=',Line,' ',MsgLine.Column]);
  if Line>=fCurSource.LineCount then exit;
  fCurSource.GetLineRange(Line-1,LineRange);
  //DebuglnThreadLog(['Old Range=',LineRange.StartPos,'-',LineRange.EndPos,' Str="',copy(fCurSource.Source,LineRange.StartPos,LineRange.EndPos-LineRange.StartPos),'"']);
  Col:=Min(Col,LineRange.EndPos-LineRange.StartPos+1);
  p:=LineRange.StartPos+Col-1;
  Src:=fCurSource.Source;
  if Identifier<>'' then begin
    // message is about a specific identifier
    if CompareIdentifiers(PChar(Identifier),@Src[p])=0 then begin
      // already pointing at the start of the identifier
      exit;
    end;
  end else begin
    // message is about any one identifier
    if IsIdentStartChar[Src[p]] then begin
      // already pointing at an identifier
      exit;
    end;
  end;
  // go to prior token
  //DebuglnThreadLog(['New Line=',Line,' Col=',Col,' p=',p]);
  NewP:=p;
  ReadPriorPascalAtom(Src,NewP,AtomEnd,false);
  if NewP<1 then exit;
  if Identifier<>'' then begin
    // message is about a specific identifier
    if CompareIdentifiers(PChar(Identifier),@Src[NewP])<>0 then begin
      // the prior token is not the identifier neither
      // => don't know
      exit;
    end;
  end else begin
    // message is about any one identifier
    if not IsIdentStartChar[Src[NewP]] then begin
      // the prior token is not an identifier neither
      // => don't know
      exit;
    end;
  end;
  fCurSource.AbsoluteToLineCol(NewP,Line,Col);
  //DebuglnThreadLog(['New Line=',Line,' Col=',Col,' p=',NewP]);
  if (Line<1) or (Col<1) then exit;
  if MsgLine.Urgency>=mluError then begin
    // position errors at start of wrong identifier, nicer for identifier completion
    MsgLine.SetSourcePosition(MsgLine.Filename,Line,Col);
    MsgLine.Flags:=MsgLine.Flags-[mlfLeftToken];
  end else begin
    // position hints at end of identifier, nicer for {%H-}
    MsgLine.SetSourcePosition(MsgLine.Filename,Line,Col+length(Identifier));
    MsgLine.Flags:=MsgLine.Flags+[mlfLeftToken];
  end;
end;

function TIDEFPCParser.FindSrcViaPPU(aPhase: TExtToolParserSyncPhase;
  MsgLine: TMessageLine; const PPUFilename: string): boolean;
{ in main thread
 for example:
   /usr/lib/fpc/3.1.1/units/x86_64-linux/rtl/sysutils.ppu:filutil.inc(481,10) Error: (5088) ...
   PPUFilename=/usr/lib/fpc/3.1.1/units/x86_64-linux/rtl/sysutils.ppu
   Filename=filutil.inc
}
var
  i: Integer;
  PrevMsgLine: TMessageLine;
  aFilename: String;
  MsgWorkerDir: String;
  UnitSrcFilename: String;
  IncPath: String;
  Dir: String;
  ShortFilename: String;
  IncFilename: String;
  AnUnitName: String;
  InFilename: String;
begin
  case aPhase of
  etpspAfterReadLine: exit(false);
  etpspSynchronized: ;
  etpspAfterSync: exit(true);
  end;
  Result:=true;

  // in main thread
  i:=MsgLine.Index;
  aFilename:=MsgLine.Filename;
  //debugln(['TIDEFPCParser.FindSrcViaPPU i=',i,' PPUFilename="',PPUFilename,'" Filename="',aFilename,'"']);
  if (i>0) then begin
    PrevMsgLine:=Tool.WorkerMessages[i-1];
    if (PrevMsgLine.SubTool=DefaultSubTool)
    and (CompareFilenames(PPUFilename,PrevMsgLine.Attribute['PPU'])=0)
    and FilenameIsAbsolute(PrevMsgLine.Filename)
    and (CompareFilenames(ExtractFilename(PrevMsgLine.Filename),ExtractFilename(aFilename))=0)
    then begin
      // same file as previous message => use it
      MsgLine.Filename:=PrevMsgLine.Filename;
      exit;
    end;
  end;

  if not FilenameIsAbsolute(PPUFilename) then
  begin
    exit;
  end;

  ShortFilename:=ExtractFilename(aFilename);
  MsgWorkerDir:=MsgLine.Attribute[FPCMsgAttrWorkerDirectory];
  AnUnitName:=ExtractFilenameOnly(PPUFilename);
  InFilename:='';
  UnitSrcFilename:=CodeToolBoss.DirectoryCachePool.FindUnitSourceInCompletePath(
                              MsgWorkerDir,AnUnitName,InFilename);
  //debugln(['TIDEFPCParser.FindSrcViaPPU MsgWorkerDir="',MsgWorkerDir,'" UnitSrcFilename="',UnitSrcFilename,'"']);
  if UnitSrcFilename<>'' then begin
    if CompareFilenames(ExtractFilename(UnitSrcFilename),ShortFilename)=0 then
    begin
      MsgLine.Filename:=UnitSrcFilename;
      exit;
    end;
    Dir:=ChompPathDelim(TrimFilename(ExtractFilePath(UnitSrcFilename)));
    IncPath:=CodeToolBoss.GetIncludePathForDirectory(Dir);
    IncFilename:=SearchFileInPath(ShortFilename,Dir,IncPath,';',ctsfcDefault);
    //debugln(['TIDEFPCParser.FindSrcViaPPU Dir="',Dir,'" IncPath="',IncPath,'" ShortFilename="',ShortFilename,'" IncFilename="',IncFilename,'"']);
    if IncFilename<>'' then begin
      MsgLine.Filename:=IncFilename;
      exit;
    end;
  end;
end;

procedure TIDEFPCParser.Translate(p: PChar; MsgItem, TranslatedItem: TFPCMsgItem;
  out TranslatedMsg: String; out MsgType: TMessageLineUrgency);
begin
  TranslatedMsg:='';
  MsgType:=mluNone;
  if TranslatedItem<>nil then
    MsgType:=FPCMsgToMsgUrgency(TranslatedItem);
  if (MsgType=mluNone) and (MsgItem<>nil) then
    MsgType:=FPCMsgToMsgUrgency(MsgItem);
  if TranslatedItem<>nil then begin
    if System.Pos('$',TranslatedItem.Pattern)<1 then begin
      TranslatedMsg:=TranslatedItem.Pattern;
      UTF8FixBroken(TranslatedMsg);
    end
    else if MsgItem<>nil then
      TranslatedMsg:=TranslateFPCMsg(p,MsgItem.Pattern,TranslatedItem.Pattern);
    //debugln(['TFPCParser.Translate Translation="',TranslatedMsg,'"']);
  end;
end;

function TIDEFPCParser.ReverseInstantFPCCacheDir(var aFilename: string;
  aSynchronized: boolean): boolean;
var
  Reversed: String;
begin
  Result:=false;
  if (InstantFPCCache='') then exit;
  if (CompareFilenames(ExtractFilePath(aFilename),InstantFPCCache)=0) then begin
    Reversed:=AppendPathDelim(Tool.WorkerDirectory)+ExtractFilename(aFilename);
    if FileExists(Reversed,aSynchronized) then begin
      aFilename:=Reversed;
      Result:=true;
    end;
  end;
end;

function TIDEFPCParser.ReverseTestBuildDir(MsgLine: TMessageLine;
  var aFilename: string): boolean;
var
  Reversed: String;
  l: Integer;
begin
  Result:=false;
  if not Tool.CurrentDirectoryIsTestDir then exit;
  l:=length(TestBuildDir); // Note: TestBuildDir includes trailing PathDelim
  if (length(aFilename)>l) and (aFilename[l]=PathDelim)
  and (CompareFilenames(LeftStr(aFilename,l),TestBuildDir)=0) then begin
    Reversed:=copy(aFilename,l+1,length(aFilename));
    if VirtualProjectFiles.Contains(Reversed) then begin
      MsgLine.Flags:=MsgLine.Flags+[mlfTestBuildFile];
      MsgLine.Attribute[MsgAttrDiskFilename]:=aFilename;
      aFilename:=Reversed;
      Result:=true;
    end
  end;
end;

constructor TIDEFPCParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fMissingFPCMsgItem:=TFPCMsgItem(Pointer(1));
  fLineToMsgID:=TPatternToMsgIDs.Create;
  fFileExists:=TFilenameToPointerTree.Create(false);
  FFilesToIgnoreUnitNotUsed:=TStringList.Create;
  HideHintsSenderNotUsed:=true;
  HideHintsUnitNotUsedInMainSource:=true;
  PC_FullVersion:=GetCompiledFPCVersion;
end;

function TIDEFPCParser.FileExists(const Filename: string; aSynchronized: boolean
  ): boolean;
var
  p: Pointer;
begin
  // check internal cache
  p:=fFileExists[Filename];
  if p=Pointer(Self) then
    Result:=true
  else if p=Pointer(fFileExists) then
    Result:=false
  else begin
    // check disk
    if aSynchronized then
      Result:=FileExistsCached(Filename)
    else
      Result:=FileExistsUTF8(Filename);
    // save result
    if Result then
      fFileExists[Filename]:=Pointer(Self)
    else
      fFileExists[Filename]:=Pointer(fFileExists);
  end;
end;

procedure TIDEFPCParser.FetchIncludePath(aPhase: TExtToolParserSyncPhase;
  MsgWorkerDir: String);
begin
  if MsgWorkerDir='' then
    MsgWorkerDir:=Tool.WorkerDirectory;
  if fIncludePathValidForWorkerDir<>MsgWorkerDir then begin
    // fetch include path from IDE
    case aPhase of
    etpspAfterReadLine:
      NeedSynchronize:=true;
    etpspSynchronized:
      begin
        fIncludePathValidForWorkerDir:=MsgWorkerDir;
        fIncludePath:=CodeToolBoss.GetIncludePathForDirectory(
                                 ChompPathDelim(MsgWorkerDir));
        {$IFDEF VerboseFPCMsgUnitNotFound}
        debugln(['TIDEFPCParser.FetchIncludePath ',fIncludePath]);
        {$ENDIF}
        NeedAfterSync:=true;
      end;
    end;
  end;
end;

procedure TIDEFPCParser.FetchUnitPath(aPhase: TExtToolParserSyncPhase;
  MsgWorkerDir: String);
begin
  if MsgWorkerDir='' then
    MsgWorkerDir:=Tool.WorkerDirectory;
  if fUnitPathValidForWorkerDir<>MsgWorkerDir then begin
    // fetch unit path from IDE
    case aPhase of
    etpspAfterReadLine:
      NeedSynchronize:=true;
    etpspSynchronized:
      begin
        fUnitPathValidForWorkerDir:=MsgWorkerDir;
        fUnitPath:=CodeToolBoss.GetUnitPathForDirectory(
                                 ChompPathDelim(MsgWorkerDir));
        NeedAfterSync:=true;
      end;
    end;
  end;
end;

function TIDEFPCParser.CheckForMsgId(p: PChar): boolean;
var
  MsgItem: TFPCMsgItem;
  TranslatedItem: TFPCMsgItem;
  MsgLine: TMessageLine;
  TranslatedMsg: String;
  MsgUrgency: TMessageLineUrgency;
  Msg: string;
begin
  Result:=false;
  if (fMsgID<1) or (MsgFile=nil) then exit;
  MsgItem:=MsgFile.GetMsg(fMsgID);
  if MsgItem=nil then exit;
  Result:=true;
  TranslatedItem:=nil;
  if (TranslationFile<>nil) then
    TranslatedItem:=TranslationFile.GetMsg(fMsgID);
  Translate(p,MsgItem,TranslatedItem,TranslatedMsg,MsgUrgency);
  Msg:=p;
  case fMsgID of
  FPCMsgIDThereWereErrorsCompiling: // There were $1 errors compiling module, stopping
    MsgUrgency:=mluVerbose;
  FPCMsgIDLinesCompiled: // n lines compiled, m sec
    if ShowLinesCompiled then MsgUrgency:=mluImportant;
  end;
  MsgLine:=CreateMsgLine;
  MsgLine.SubTool:=DefaultSubTool;
  MsgLine.Urgency:=MsgUrgency;
  MsgLine.Msg:=Msg;
  MsgLine.TranslatedMsg:=TranslatedMsg;
  AddMsgLine(MsgLine);
end;

function TIDEFPCParser.CheckFollowUpMessage(p: PChar): boolean;
var
  i: Integer;
  LastMsgLine, MsgLine: TMessageLine;
begin
  Result:=false;
  if (p^=' ') then begin
    i:=Tool.WorkerMessages.Count-1;
    if i<0 then exit;
    LastMsgLine:=Tool.WorkerMessages[i];
    if LastMsgLine.SubTool=SubToolFPCLinker then begin
      // a follow up line of the linker output
      Result:=true;
      MsgLine:=CreateMsgLine;
      MsgLine.MsgID:=0;
      MsgLine.SubTool:=SubToolFPCLinker;
      MsgLine.Urgency:=LastMsgLine.Urgency;
      MsgLine.Msg:='linker: '+p;
      inherited AddMsgLine(MsgLine);
    end;
  end;
end;

function TIDEFPCParser.CheckForFileLineColMessage(p: PChar): boolean;
{ filename(line,column) Hint: message
  filename(line,column) Hint: (msgid) message
  filename(line) Hint: (msgid) message
  B:\file(3)name(line,column) Hint: (msgid) message
  /usr/lib/fpc/3.1.1/units/x86_64-linux/rtl/sysutils.ppu:filutil.inc(481,10) Error: (5088) ...
}
var
  FileStartPos: PChar;
  FileEndPos: PChar;
  LineStartPos: PChar;
  ColStartPos: PChar;
  MsgType: TMessageLineUrgency;
  MsgLine: TMessageLine;
  p2: PChar;
  i: Integer;
  TranslatedItem: TFPCMsgItem;
  MsgItem: TFPCMsgItem;
  TranslatedMsg: String;
  aFilename: String;
  Column: Integer;
  PPUFileStartPos: PChar;
  PPUFileEndPos: PChar;
begin
  Result:=false;
  FileStartPos:=p;
  FileEndPos:=nil;
  PPUFileStartPos:=nil;
  PPUFileEndPos:=nil;
  // search colon and last ( in front of colon
  while true do begin
    case p^ of
    #0: exit;
    '(': FileEndPos:=p;
    ':':
      if (p-FileStartPos>5) and (p[-4]='.') and (p[-3] in ['p','P'])
      and (p[-2] in ['p','P']) and (p[-1] in ['u','U']) then begin
        // e.g. /usr/lib/fpc/3.1.1/units/x86_64-linux/rtl/sysutils.ppu:filutil.inc(481,10) Error: (5088) ...
        if PPUFileStartPos<>nil then exit;
        PPUFileStartPos:=FileStartPos;
        PPUFileEndPos:=p;
        FileStartPos:=p+1;
      end
      else if (DriveSeparator='') or (p-FileStartPos>1) then
        break;
    end;
    inc(p);
  end;
  if (FileEndPos=nil) or (FileEndPos-FileStartPos=0) or (FileEndPos[-1]=' ') then exit;
  p:=FileEndPos;
  inc(p); // skip bracket
  LineStartPos:=p;
  if not ReadDecimal(p) then exit;
  if p^=',' then begin
    if not ReadChar(p,',') then exit;
    ColStartPos:=p;
    if not ReadDecimal(p) then exit;
  end else
    ColStartPos:=nil;
  if not ReadChar(p,')') then exit;
  if not ReadChar(p,' ') then exit;
  MsgType:=mluNote;
  if ReadString(p,'Info:') then begin
    MsgType:=mluVerbose;
  end else if ReadString(p,'Hint:') then begin
    MsgType:=mluHint;
  end else if ReadString(p,'Note:') then begin
    MsgType:=mluNote;
  end else if ReadString(p,'Warn:') then begin
    MsgType:=mluWarning;
  end else if ReadString(p,'Error:') then begin
    MsgType:=mluError;
  end else if ReadString(p,'Fatal:') then begin
    MsgType:=mluError;
  end else begin
    p2:=p;
    while not (p2^ in [':',#0,' ']) do inc(p2);
    if p2^=':' then begin
      // unknown type (maybe a translation?)
      p:=p2+1;
    end;
  end;
  while p^ in [' ',#9] do inc(p);
  Result:=true;
  TranslatedMsg:='';
  if (p^='(') and (p[1] in ['0'..'9']) then begin
    // (msgid)
    p2:=p;
    inc(p2);
    i:=0;
    while (p2^ in ['0'..'9']) and (i<1000000) do begin
      i:=i*10+ord(p2^)-ord('0');
      inc(p2);
    end;
    if p2^=')' then begin
      fMsgID:=i;
      p:=p2+1;
      while p^ in [' ',#9] do inc(p);
      //debugln(['TFPCParser.CheckForFileLineColMessage ID=',fMsgID,' Msg=',FileStartPos]);
      if (fMsgID>0) then begin
        TranslatedItem:=nil;
        MsgItem:=nil;
        if (TranslationFile<>nil) then
          TranslatedItem:=TranslationFile.GetMsg(fMsgID);
        if (MsgFile<>nil) then
          MsgItem:=MsgFile.GetMsg(fMsgID);
        if (TranslatedItem=nil) and (MsgItem=nil) then begin
          if ConsoleVerbosity>=1 then
            debugln(['TFPCParser.CheckForFileLineColMessage msgid not found: ',fMsgID]);
        end else begin
          Translate(p,MsgItem,TranslatedItem,TranslatedMsg,MsgType);
          if MsgType=mluNone then begin
            if ConsoleVerbosity>=1 then
              debugln(['TFPCParser.CheckForFileLineColMessage msgid has no type: ',fMsgID]);
          end;
        end;
      end;
    end;
  end;
  if ColStartPos<>nil then
    Column:=Str2Integer(ColStartPos,0)
  else
    Column:=0;

  MsgLine:=CreateMsgLine;
  MsgLine.SubTool:=DefaultSubTool;
  MsgLine.Urgency:=MsgType;
  aFilename:=GetString(FileStartPos,FileEndPos-FileStartPos);
  if PPUFileStartPos<>nil then
    MsgLine.Attribute['PPU']:=GetString(PPUFileStartPos,PPUFileEndPos-PPUFileStartPos);
  MsgLine.Filename:=LongenFilename(MsgLine,aFilename);
  MsgLine.Line:=Str2Integer(LineStartPos,0);
  MsgLine.Column:=Column;
  MsgLine.Msg:=p;
  MsgLine.TranslatedMsg:=TranslatedMsg;
  //debugln(['TFPCParser.CheckForFileLineColMessage ',dbgs(MsgLine.Urgency)]);

  AddMsgLine(MsgLine);
end;

function TIDEFPCParser.CheckForLoadFromUnit(p: PChar): Boolean;
var
  OldP: PChar;
  MsgLine: TMessageLine;
begin
  Result:=fMsgID=10027;
  if (not Result) and (fMsgID>0) then exit;
  OldP:=p;
  if not Result then begin
    if not ReadString(p,'Load from ') then exit;
    while not (p^ in ['(',#0]) do inc(p);
    if p^<>'(' then exit;
    while not (p^ in [')',#0]) do inc(p);
    if p^<>')' then exit;
    if not ReadString(p,') unit ') then exit;
  end;
  MsgLine:=CreateMsgLine;
  MsgLine.SubTool:=DefaultSubTool;
  MsgLine.Urgency:=mluProgress;
  MsgLine.Msg:=OldP;
  AddMsgLine(MsgLine);
  Result:=true;
end;

procedure TIDEFPCParser.ReadLine(Line: string; OutputIndex: integer;
  var Handled: boolean);
{ returns true, if it is a compiler message
   Examples for freepascal compiler messages:
     Compiling <filename>
     Assembling <filename>
     Fatal: <some text>
     Fatal: (message id) <some text>
     (message id) <some text>
     <filename>(123,45) <ErrorType>: <some text>
     <filename>(123) <ErrorType>: <some text>
     <filename>(456) <ErrorType>: <some text> in line (123)
     [0.000] (3101) Macro defined: CPUAMD64
     <filename>(12,34) <ErrorType>: (5024) <some text>
}
var
  p: PChar;
begin
  if Line='' then exit;
  Line:=ToUTF8(Line);
  p:=PChar(Line);
  fOutputIndex:=OutputIndex;
  fMsgID:=0;

  // skip time [0.000]
  if (p^='[') and (p[1] in ['0'..'9']) then begin
    inc(p,2);
    while p^ in ['0'..'9','.'] do inc(p);
    if p^<>']' then exit; // not a fpc message
    inc(p);
    while p^ in [' '] do inc(p);
  end;

  // read message ID (000)
  if (p^='(') and (p[1] in ['0'..'9']) then begin
    inc(p);
    while p^ in ['0'..'9','.'] do begin
      if fMsgID>1000000 then exit; // not a fpc message
      fMsgID:=fMsgID*10+ord(p^)-ord('0');
      inc(p);
    end;
    if p^<>')' then exit; // not a fpc message
    inc(p);
    while p^=' ' do inc(p);
  end;

  if p^ in [#0..#31,' '] then begin
    CheckFollowUpMessage(p);
    exit; // not a fpc message
  end;

  Handled:=true;

  // check for (msgid) message
  if CheckForMsgId(p) then exit;
  // check for 'filename(line,column) Error: message'
  if CheckForFileLineColMessage(p) then exit;
  // check for 'Compiling <filename>'
  if CheckForCompilingState(p) then exit;
  // check for 'Assembling <filename>'
  if CheckForAssemblingState(p) then exit;
  // check for 'Fatal: ', 'Panic: ', 'Error: ', ...
  if CheckForGeneralMessage(p) then exit;
  // check for '<line> <kb>/<kb>'...
  if CheckForLineProgress(p) then exit;
  // check for '<int> Lines compiled, <int>.<int> sec'
  if CheckForLinesCompiled(p) then exit;
  // check for infos (logo, Linking <Progname>)
  if CheckForInfos(p) then exit;
  // check for -vx output
  if CheckForExecutableInfo(p) then exit;
  // check for Load from unit
  if CheckForLoadFromUnit(p) then exit;
  // check for windres errors
  if CheckForWindresErrors(p) then exit;
  // check for linker errors
  if CheckForLinkerErrors(p) then exit;
  // check for assembler errors
  if CheckForAssemblerErrors(p) then exit;

  {$IFDEF VerboseFPCParser}
  debugln('TFPCParser.ReadLine UNKNOWN: ',Line);
  {$ENDIF}
  Handled:=false;
end;

procedure TIDEFPCParser.AddMsgLine(MsgLine: TMessageLine);
begin
  if IsMsgID(MsgLine,FPCMsgIDErrorWhileCompilingResources,
    fMsgItemErrorWhileCompilingResources)
  then begin
    // Error while compiling resources
    AddResourceMessages;
    MsgLine.Msg:=MsgLine.Msg+' -> '+'Compile with -vd for more details. Check for duplicates.';
    MsgLine.TranslatedMsg:=MsgLine.TranslatedMsg+' -> '+lisCompileWithVdForMoreDetailsCheckForDuplicates;
  end
  else if IsMsgID(MsgLine,FPCMsgIDErrorWhileLinking,fMsgItemErrorWhileLinking) then
    AddLinkingMessages
  else if IsMsgID(MsgLine,FPCMsgIDChecksumChanged,fMsgItemChecksumChanged) then
    MsgLine.Urgency:=mluWarning
  else if IsMsgID(MsgLine,FPCMsgIDThereWereErrorsCompiling,
    fMsgItemThereWereErrorsCompiling)
  then
    MsgLine.Urgency:=mluVerbose;
  inherited AddMsgLine(MsgLine);
end;

function TIDEFPCParser.LongenFilename(MsgLine: TMessageLine; aFilename: string
  ): string;
var
  ShortFilename: String;
  i: Integer;
  LastMsgLine: TMessageLine;
  LastFilename: String;
begin
  Result:=TrimFilename(aFilename);
  if FilenameIsAbsolute(Result) then begin
    if ReverseInstantFPCCacheDir(Result,false) then exit;
    if ReverseTestBuildDir(MsgLine,Result) then exit;
    exit;
  end;
  if MsgLine.Attribute['PPU']<>'' then begin
    MsgLine.Attribute[FPCMsgAttrWorkerDirectory]:=Tool.WorkerDirectory;
    exit;
  end;

  ShortFilename:=Result;
  // check last message line
  LastMsgLine:=Tool.WorkerMessages.GetLastLine;
  if (LastMsgLine<>nil) then begin
    if mlfTestBuildFile in LastMsgLine.Flags then
      LastFilename:=LastMsgLine.Attribute[MsgAttrDiskFilename]
    else
      LastFilename:=LastMsgLine.Filename;
    if FilenameIsAbsolute(LastFilename) then begin
      if (length(LastFilename)>length(ShortFilename))
      and (LastFilename[length(LastFilename)-length(ShortFilename)] in AllowDirectorySeparators)
      and (CompareFilenames(RightStr(LastFilename,length(ShortFilename)),ShortFilename)=0)
      then begin
        if mlfTestBuildFile in LastMsgLine.Flags then begin
          MsgLine.Attribute[MsgAttrDiskFilename]:=LastFilename;
          MsgLine.Flags:=MsgLine.Flags+[mlfTestBuildFile];
          Result:=LastMsgLine.Filename;
        end else begin
          Result:=LastFilename;
          ReverseTestBuildDir(MsgLine,Result);
        end;
        exit;
      end;
    end;
  end;
  // search file in the last compiling directories
  if DirectoryStack<>nil then begin
    for i:=DirectoryStack.Count-1 downto 0 do begin
      Result:=AppendPathDelim(DirectoryStack[i])+ShortFilename;
      if FileExists(Result,false) then begin
        ReverseTestBuildDir(MsgLine,Result);
        exit;
      end;
    end;
  end;
  // search file in worker directory
  if Tool.WorkerDirectory<>'' then begin
    Result:=AppendPathDelim(Tool.WorkerDirectory)+ShortFilename;
    if FileExists(Result,false) then begin
      ReverseTestBuildDir(MsgLine,Result);
      exit;
    end;
  end;

  // file not found
  Result:=ShortFilename;

  // save Tool.WorkerDirectory for ImproveMessage
  MsgLine.Attribute[FPCMsgAttrWorkerDirectory]:=Tool.WorkerDirectory;
end;

function TIDEFPCParser.GetDefaultPCFullVersion: LongWord;
begin
  // get FPC version
  Result:=LongWord(CodeToolBoss.GetPCVersionForDirectory(Tool.WorkerDirectory));
end;

function TIDEFPCParser.ToUTF8(const Line: string): string;
begin
  if PC_FullVersion>=20701 then
    Result:=ConsoleToUTF8(Line)
  else begin
    {$IFDEF MSWINDOWS}
    Result:=WinCPToUTF8(Line);
    {$ELSE}
    Result:=SysToUTF8(Line);
    {$ENDIF}
  end;
end;

procedure TIDEFPCParser.ImproveMessages(aPhase: TExtToolParserSyncPhase);
var
  i: Integer;
  MsgLine: TMessageLine;
  aFilename: String;
  Y: Integer;
  X: Integer;
  Code: TCodeBuffer;
  SourceOK: Boolean;
  MsgWorkerDir: String;
  PrevMsgLine: TMessageLine;
  CmdLineParams: String;
  SrcFilename: String;
  PPUFilename: String;
begin
  //debugln(['TIDEFPCParser.ImproveMessages START ',aSynchronized,' Last=',fLastWorkerImprovedMessage[aSynchronized],' Now=',Tool.WorkerMessages.Count]);
  for i:=fLastWorkerImprovedMessage[aPhase]+1 to Tool.WorkerMessages.Count-1 do
  begin
    MsgLine:=Tool.WorkerMessages[i];
    Y:=MsgLine.Line;
    X:=MsgLine.Column;
    if (Y>0) and (X>0)
    and (MsgLine.SubTool=DefaultSubTool) and (MsgLine.Filename<>'')
    then begin
      if mlfTestBuildFile in MsgLine.Flags then
        aFilename:=MsgLine.Attribute[MsgAttrDiskFilename]
      else
        aFilename:=MsgLine.Filename;
      PPUFilename:='';
      if (not FilenameIsAbsolute(aFilename)) then begin
        PPUFilename:=MsgLine.Attribute['PPU'];
        if PPUFilename<>'' then begin
          // compiler gave ppu file and relative source file
          if not FindSrcViaPPU(aPhase,MsgLine,PPUFilename) then continue;
        end;
      end;
      if (not FilenameIsAbsolute(aFilename)) then begin
        // short file name => 1. search the full file name in previous message
        if i>0 then begin
          PrevMsgLine:=Tool.WorkerMessages[i-1];
          if (PrevMsgLine.SubTool=DefaultSubTool)
          and FilenameIsAbsolute(PrevMsgLine.Filename)
          and (CompareFilenames(ExtractFilename(PrevMsgLine.Filename),ExtractFilename(aFilename))=0)
          then begin
            // same file as previous message => use it
            aFilename:=PrevMsgLine.Filename;
            MsgLine.Filename:=aFilename;
          end;
        end;
      end;
      if (not FilenameIsAbsolute(aFilename)) then begin
        // short file name => 2. search in include path
        MsgWorkerDir:=MsgLine.Attribute[FPCMsgAttrWorkerDirectory];
        FetchIncludePath(aPhase,MsgWorkerDir); // needs Phase etpspAfterReadLine+etpspSynchronized
        {$IFDEF VerboseFPCMsgUnitNotFound}
        if aPhase=etpspSynchronized then
          debugln(['TIDEFPCParser.ImproveMessages IncPath="',fIncludePath,'" aFilename="',aFilename,'" MsgWorkerDir="',MsgWorkerDir,'"']);
        {$ENDIF}
        if (aPhase in [etpspAfterReadLine,etpspAfterSync])
        and (fIncludePathValidForWorkerDir=MsgWorkerDir) then begin
          // include path is valid and in worker thread
          // -> search file
          aFilename:=FileUtil.SearchFileInPath(aFilename,MsgWorkerDir,fIncludePath,';',
                                 [FileUtil.sffSearchLoUpCase]);
          if aFilename<>'' then
            MsgLine.Filename:=aFilename;
        end;
      end;
      if (not FilenameIsAbsolute(aFilename)) and (aPhase=etpspAfterReadLine)
      then begin
        CmdLineParams:=Tool.CmdLineParams;
        if Pos(CmdLineParams,PathDelim+'fpc'+ExeExt+' ')>0 then begin
          // short file name => 3. check the cmd line param source file
          SrcFilename:=GetFPCParameterSrcFile(Tool.CmdLineParams);
          if (SrcFilename<>'')
          and ((CompareFilenames(ExtractFilename(SrcFilename),aFilename)=0)
          or (CompareFilenames(ExtractFileNameOnly(SrcFilename),aFilename)=0))
          then begin
            if not FilenameIsAbsolute(SrcFilename) then begin
              MsgWorkerDir:=MsgLine.Attribute[FPCMsgAttrWorkerDirectory];
              SrcFilename:=ResolveDots(AppendPathDelim(MsgWorkerDir)+SrcFilename);
            end;
            if FilenameIsAbsolute(SrcFilename) then
              MsgLine.Filename:=SrcFilename;
          end;
        end;
      end;

      // get source
      SourceOK:=false;
      aFilename:=MsgLine.Filename;
      if FilenameIsAbsolute(aFilename) or (mlfTestBuildFile in MsgLine.Flags)
      then begin
        if (fCurSource<>nil)
        and (CompareFilenames(aFilename,fCurSource.Filename)=0) then begin
          SourceOK:=true;
        end else begin
          // need source
          case aPhase of
          etpspAfterReadLine:
            NeedSynchronize:=true;
          etpspSynchronized:
            begin
              // load source file
              //debugln(['TFPCParser.ImproveMessages loading ',aFilename]);
              Code:=CodeToolBoss.LoadFile(aFilename,true,false);
              if Code<>nil then begin
                if fCurSource=nil then
                  fCurSource:=TCodeBuffer.Create;
                fCurSource.Filename:=aFilename;
                if Code.FileOnDiskNeedsUpdate then begin
                  // IDE buffer contains changes that are not yet saved to disk
                  // The compiler messages are about the disk file
                  // => load the file
                  fCurSource.LoadFromFile(aFilename);
                end else begin
                  // IDE buffer valid => just copy
                  fCurSource.Source:=Code.Source;
                end;
                SourceOK:=true;
                NeedAfterSync:=true;
              end;
            end;
          end;
        end;
      end;

      ImproveMsgIdentifierPosition(aPhase, MsgLine, SourceOK);
      ImproveMsgHiddenByIDEDirective(aPhase, MsgLine, SourceOK);
      ImproveMsgUnitNotUsed(aPhase, MsgLine);
      ImproveMsgSenderNotUsed(aPhase, MsgLine);
    end else if MsgLine.SubTool=SubToolFPCLinker then begin
      ImproveMsgLinkerUndefinedReference(aPhase, MsgLine);
    end;
    ImproveMsgUnitNotFound(aPhase, MsgLine);
  end;
  fLastWorkerImprovedMessage[aPhase]:=Tool.WorkerMessages.Count-1;
end;

class function TIDEFPCParser.CanParseSubTool(const SubTool: string): boolean;
begin
  Result:=(CompareText(SubTool,SubToolFPC)=0)
       or (CompareText(SubTool,SubToolFPCLinker)=0)
       or (CompareText(SubTool,SubToolFPCRes)=0);
end;

class function TIDEFPCParser.DefaultSubTool: string;
begin
  Result:=SubToolFPC;
end;

class function TIDEFPCParser.GetMsgHint(SubTool: string; MsgID: integer): string;
var
  CurMsgFile: TFPCMsgFilePoolItem;
  MsgItem: TFPCMsgItem;
begin
  Result:='';
  if CompareText(SubTool,DefaultSubTool)=0 then begin
    CurMsgFile:=MsgFilePool.LoadCurrentEnglishFile(false,nil);
    if CurMsgFile=nil then exit;
    try
      MsgItem:=CurMsgFile.GetMsg(MsgID);
      if MsgItem=nil then exit;
      Result:=MsgItem.GetTrimmedComment(false,true);
    finally
      MsgFilePool.UnloadFile(CurMsgFile);
    end;
  end;
end;

class function TIDEFPCParser.GetMsgPattern(SubTool: string; MsgID: integer; out
  Urgency: TMessageLineUrgency): string;
var
  CurMsgFile: TFPCMsgFilePoolItem;
  MsgItem: TFPCMsgItem;
begin
  Result:='';
  Urgency:=mluNone;
  if CompareText(SubTool,DefaultSubTool)=0 then begin
    if MsgFilePool=nil then exit;
    CurMsgFile:=MsgFilePool.LoadCurrentEnglishFile(false,nil);
    if CurMsgFile=nil then exit;
    try
      MsgItem:=CurMsgFile.GetMsg(MsgID);
      if MsgItem=nil then exit;
      Result:=MsgItem.Pattern;
      Urgency:=FPCMsgToMsgUrgency(MsgItem);
    finally
      MsgFilePool.UnloadFile(CurMsgFile);
    end;
  end;
end;

class function TIDEFPCParser.Priority: integer;
begin
  Result:=SubToolFPCPriority;
end;

class function TIDEFPCParser.MsgLineIsId(Msg: TMessageLine; MsgId: integer; out
  Value1, Value2: string): boolean;

  function GetStr(FromPos, ToPos: PChar): string;
  begin
    if (FromPos=nil) or (FromPos=ToPos) then
      Result:=''
    else begin
      SetLength(Result,ToPos-FromPos);
      Move(FromPos^,Result[1],ToPos-FromPos);
    end;
  end;

var
  aFPCParser: TFPCParser;
  Pattern: String;
  VarStarts: PPChar;
  VarEnds: PPChar;
  s: String;
begin
  Value1:='';
  Value2:='';
  if Msg=nil then exit(false);
  if Msg.SubTool<>DefaultSubTool then exit(false);
  if (Msg.MsgID<>MsgId)
  and (Msg.MsgID<>0) then exit(false);
  Result:=true;
  aFPCParser:=GetFPCParser(Msg);
  if aFPCParser=nil then exit;
  Pattern:=aFPCParser.GetFPCMsgIDPattern(MsgId);
  VarStarts:=GetMem(SizeOf(PChar)*10);
  VarEnds:=GetMem(SizeOf(PChar)*10);
  s:=Msg.Msg;
  Result:=FPCMsgFits(s,Pattern,VarStarts,VarEnds);
  if Result then begin
    Value1:=GetStr(VarStarts[1],VarEnds[1]);
    Value2:=GetStr(VarStarts[2],VarEnds[2]);
  end;
  Freemem(VarStarts);
  Freemem(VarEnds);
end;

function TIDEFPCParser.GetFPCMsgIDPattern(MsgID: integer): string;
var
  MsgItem: TFPCMsgItem;
begin
  Result:='';
  if MsgID<=0 then exit;
  if MsgFile=nil then exit;
  MsgItem:=MsgFile.GetMsg(MsgID);
  if MsgItem=nil then exit;
  Result:=MsgItem.Pattern;
end;

class function TIDEFPCParser.GetFPCMsgPattern(Msg: TMessageLine): string;
var
  aFPCParser: TFPCParser;
begin
  Result:='';
  if Msg.MsgID<=0 then exit;
  aFPCParser:=GetFPCParser(Msg);
  if aFPCParser=nil then exit;
  Result:=aFPCParser.GetFPCMsgIDPattern(Msg.MsgID);
end;

class function TIDEFPCParser.GetFPCMsgValue1(Msg: TMessageLine): string;
begin
  Result:='';
  if Msg.MsgID<=0 then exit;
  if Msg.SubTool<>DefaultSubTool then exit;
  if not etFPCMsgParser.GetFPCMsgValue1(Msg.Msg,GetFPCMsgPattern(Msg),Result) then
    Result:='';
end;

class function TIDEFPCParser.GetFPCMsgValues(Msg: TMessageLine; out Value1,
  Value2: string): boolean;
begin
  Result:=false;
  if Msg.MsgID<=0 then exit;
  if Msg.SubTool<>DefaultSubTool then exit;
  Result:=etFPCMsgParser.GetFPCMsgValues2(Msg.Msg,GetFPCMsgPattern(Msg),Value1,Value2);
end;

class function TIDEFPCParser.MsgFilePool: TFPCMsgFilePool;
begin
  Result:=FPCMsgFilePool;
end;

initialization
  IDEFPCParser:=TIDEFPCParser;
finalization
  FreeAndNil(FPCMsgFilePool);

end.

