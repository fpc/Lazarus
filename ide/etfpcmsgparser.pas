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
    Parser for Free Pascal Compiler output.
}
unit etFPCMsgParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, FileProcs, KeywordFuncLists, IDEExternToolIntf,
  PackageIntf, LazIDEIntf, ProjectIntf, CodeToolsFPCMsgs, CodeToolsStructs,
  CodeCache, CodeToolManager, DirectoryCacher, BasicCodeTools, DefineTemplates,
  LazUTF8, FileUtil, etMakeMsgParser, EnvironmentOpts;

type
  TFPCMsgFilePool = class;

  { TFPCMsgFilePoolItem }

  TFPCMsgFilePoolItem = class
  private
    FFile: TFPCMsgFile;
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
    procedure Log(Msg: string; AThread: TThread);
    procedure LogSync;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadFile(aFilename: string; UpdateFromDisk: boolean;
      AThread: TThread): TFPCMsgFilePoolItem;
    procedure UnloadFile(var aFile: TFPCMsgFilePoolItem; AThread: TThread);
    procedure EnterCriticalsection;
    procedure LeaveCriticalSection;
    procedure GetMsgFileNames(CompilerFilename, TargetOS, TargetCPU: string;
      out anEnglishFile, aTranslationFile: string); // (main thread)
    property DefaultEnglishFile: string read FDefaultEnglishFile write FDefaultEnglishFile;
    property DefaulTranslationFile: string read FDefaultTranslationFile write FDefaultTranslationFile;
    property OnLoadFile: TETLoadFileEvent read FOnLoadFile write FOnLoadFile; // (main or workerthread)
  end;

  { TPatternToMsgID }

  TPatternToMsgID = class
  public
    Pattern: string;
    MsgID: integer;
  end;

  { TPatternToMsgIDs }

  TPatternToMsgIDs = class
  private
    fItems: array of TPatternToMsgID;
    function IndexOf(Pattern: PChar; Insert: boolean): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Pattern: string; MsgID: integer);
    procedure AddLines(const Lines: string; MsgID: integer);
    function LineToMsgID(p: PChar): integer; // 0 = not found
    procedure WriteDebugReport;
    procedure ConsistencyCheck;
  end;

  { TIDEFPCParser }

  TIDEFPCParser = class(TFPCParser)
  private
    fMsgID: Integer; // current message id given by ReadLine (-vq)
    fOutputIndex: integer; // current OutputIndex given by ReadLine
    fLineToMsgID: TPatternToMsgIDs;
    fLastWorkerImprovedMessage: array[boolean] of integer;
    fLastSource: TCodeBuffer;
    fFileExists: TFilenameToPointerTree;
    function FileExists(const Filename: string; aSynchronized: boolean): boolean;
    function CheckForMsgId(p: PChar): boolean; // (MsgId) message
    function CheckForFileLineColMessage(p: PChar): boolean; // the normal messages: filename(y,x): Hint: ..
    function CheckForGeneralMessage(p: PChar): boolean; // Fatal: .., Error: ..., Panic: ..
    function CheckForInfos(p: PChar): boolean;
    function CheckForCompilingState(p: PChar): boolean; // Compiling ..
    function CheckForAssemblingState(p: PChar): boolean; // Assembling ..
    function CheckForLinesCompiled(p: PChar): boolean; // ..lines compiled..
    function CheckForExecutableInfo(p: PChar): boolean;
    function CheckForLinkingErrors(p: PChar): boolean;
    function CheckForFollowUpMessages(p: PChar): boolean;
    function CheckForLineProgress(p: PChar): boolean; // 600 206.521/231.648 Kb Used
    function CheckForRecompilingChecksumChangedMessages(p: PChar): boolean;
    function CheckForLoadFromUnit(p: PChar): Boolean;
    function CheckForWindresErrors(p: PChar): boolean;
    function CreateMsgLine: TMessageLine;
    procedure ImproveMsgHiddenByIDEDirective(const SourceOK: Boolean;
      var MsgLine: TMessageLine);
    procedure ImproveMsgSenderNotUsed(const MsgLine: TMessageLine);
    procedure ImproveMsgUnitNotUsed(aSynchronized: boolean;
      const aFilename: String; var MsgLine: TMessageLine);
    procedure ImproveMsgUnitNotFound(aSynchronized: boolean;
      var MsgLine: TMessageLine);
    procedure Translate(p: PChar; MsgItem, TranslatedItem: TFPCMsgItem;
        out TranslatedMsg: String; out MsgType: TMessageLineUrgency);
  public
    DirectoryStack: TStrings;
    MsgFilename: string; // e.g. /path/to/fpcsrc/compiler/msg/errore.msg
    MsgFile: TFPCMsgFilePoolItem;
    TranslationFilename: string; // e.g. /path/to/fpcsrc/compiler/msg/errord.msg
    TranslationFile: TFPCMsgFilePoolItem;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init; override; // called after macros resolved, before starting thread (main thread)
    procedure InitReading; override; // called if process started, before first line (worker thread)
    procedure Done; override; // called after process stopped (worker thread)
    procedure ReadLine(Line: string; OutputIndex: integer; var Handled: boolean); override;
    function LongenFilename(aFilename: string): string;
    procedure ImproveMessages(aSynchronized: boolean); override;
    function GetFPCMsgIDPattern(MsgID: integer): string; override;
    class function IsSubTool(const SubTool: string): boolean; override;
    class function DefaultSubTool: string; override;
    class function GetMsgExample(SubTool: string; MsgID: integer): string;
      override;
    class function GetMsgHint(SubTool: string; MsgID: integer): string;
      override;
    class function Priority: integer; override;
    class function GetFPCMsgPattern(Msg: TMessageLine): string; override;
    class function GetFPCMsgValue1(Msg: TMessageLine): string; override;
    class function GetFPCMsgValues(Msg: TMessageLine; out Value1, Value2: string): boolean; override;
  end;

var
  FPCMsgFilePool: TFPCMsgFilePool = nil;

// thread safe
function FPCMsgToMsgUrgency(Msg: TFPCMsgItem): TMessageLineUrgency;
function FPCMsgTypeToUrgency(const Typ: string): TMessageLineUrgency;
function TranslateFPCMsg(const Src, SrcPattern, TargetPattern: string): string;
function GetFPCMsgValue1(const Src, Pattern: string; out Value1: string): boolean;
function GetFPCMsgValues(Src, Pattern: string; out Value1, Value2: string): boolean;

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

function TranslateFPCMsg(const Src, SrcPattern, TargetPattern: string): string;
{ for example:
  Src='A lines compiled, B sec C'
  SrcPattern='$1 lines compiled, $2 sec $3'
  TargetPattern='$1 Zeilen uebersetzt, $2 Sekunden $3'

  Result='A Zeilen uebersetzt, B Sekunden C'
}
  function IsVar(p: PChar): boolean; inline;
  begin
    Result:=(p^='$') and (p[1] in ['0'..'9']);
  end;

  function IsEndOrVar(p: PChar): boolean; inline;
  begin
    Result:=(p^=#0) or IsVar(p);
  end;

var
  SrcPos: PChar;
  SrcPatPos: PChar;
  TargetPatPos: PChar;
  TargetPos: PChar;
  SrcVarStarts, SrcVarEnds: array[0..9] of PChar;
  VarUsed: array[0..9] of integer;
  i: Integer;
  SrcPos2: PChar;
  SrcPatPos2: PChar;
begin
  Result:='';
  {$IFDEF VerboseFPCTranslate}
  debugln(['TranslateFPCMsg Src="',Src,'" SrcPattern="',SrcPattern,'" TargetPattern="',TargetPattern,'"']);
  {$ENDIF}
  if (Src='') or (SrcPattern='') or (TargetPattern='') then exit;
  SrcPos:=PChar(Src);
  SrcPatPos:=PChar(SrcPattern);
  for i:=Low(SrcVarStarts) to high(SrcVarStarts) do begin
    SrcVarStarts[i]:=nil;
    SrcVarEnds[i]:=nil;
    VarUsed[i]:=0;
  end;
  // skip the characters of Src copied from SrcPattern
  while not IsEndOrVar(SrcPatPos) do begin
    if (SrcPos^<>SrcPatPos^) then begin
      // SrcPattern does not fit
      {$IFDEF VerboseFPCTranslate}
      debugln(['TranslateFPCMsg skipping start of Src and SrcPattern failed']);
      {$ENDIF}
      exit;
    end;
    inc(SrcPos);
    inc(SrcPatPos)
  end;
  {$IFDEF VerboseFPCTranslate}
  debugln(['TranslateFPCMsg skipped start: SrcPos="',SrcPos,'" SrcPatPos="',SrcPatPos,'"']);
  {$ENDIF}
  // find the parameters in Src and store their boundaries in SrcVarStarts, SrcVarEnds
  while (SrcPatPos^<>#0) do begin
    // read variable number
    inc(SrcPatPos);
    i:=ord(SrcPatPos^)-ord('0');
    inc(SrcPatPos);
    SrcVarStarts[i]:=SrcPos;
    SrcVarEnds[i]:=nil;
    // find the end of the parameter in Src
    // example:  SrcPattern='$1 found' Src='Ha found found'
    repeat
      if SrcPos^=SrcPatPos^ then begin
        {$IFDEF VerboseFPCTranslate}
        debugln(['TranslateFPCMsg candidate for param ',i,' end: SrcPos="',SrcPos,'" SrcPatPos="',SrcPatPos,'"']);
        {$ENDIF}
        SrcPos2:=SrcPos;
        SrcPatPos2:=SrcPatPos;
        while (SrcPos2^=SrcPatPos2^) and not IsEndOrVar(SrcPatPos2) do begin
          inc(SrcPos2);
          inc(SrcPatPos2);
        end;
        if IsEndOrVar(SrcPatPos2) then begin
          {$IFDEF VerboseFPCTranslate}
          debugln(['TranslateFPCMsg param ',i,' end found: SrcPos2="',SrcPos2,'" SrcPatPos2="',SrcPatPos2,'"']);
          {$ENDIF}
          SrcVarEnds[i]:=SrcPos;
          SrcPos:=SrcPos2;
          SrcPatPos:=SrcPatPos2;
          break;
        end;
        {$IFDEF VerboseFPCTranslate}
        debugln(['TranslateFPCMsg searching further...']);
        {$ENDIF}
      end else if SrcPos^=#0 then begin
        if IsEndOrVar(SrcPatPos) then begin
          // empty parameter at end
          SrcVarEnds[i]:=SrcPos;
          break;
        end else begin
          // SrcPattern does not fit Src
          {$IFDEF VerboseFPCTranslate}
          debugln(['TranslateFPCMsg finding end of parameter ',i,' failed']);
          {$ENDIF}
          exit;
        end;
      end;
      inc(SrcPos);
    until false;
  end;

  // create Target
  SetLength(Result,length(TargetPattern)+length(Src));
  TargetPatPos:=PChar(TargetPattern);
  TargetPos:=PChar(Result);
  while TargetPatPos^<>#0 do begin
    //debugln(['TranslateFPCMsg Target ',dbgs(Pointer(TargetPatPos)),' ',ord(TargetPatPos^),' TargetPatPos="',TargetPatPos,'"']);
    if IsVar(TargetPatPos) then begin
      // insert variable
      inc(TargetPatPos);
      i:=ord(TargetPatPos^)-ord('0');
      inc(TargetPatPos);
      if SrcVarStarts[i]<>nil then begin
        inc(VarUsed[i]);
        if VarUsed[i]>1 then begin
          // variable is used more than once => increase result
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

function GetFPCMsgValue1(const Src, Pattern: string; out Value1: string
  ): boolean;
{ Pattern: 'Compiling $1'
  Src:     'Compiling fcllaz.pas'
  Value1:  'fcllaz.pas'
}
var
  p: SizeInt;
begin
  p:=Pos('$1',Pattern);
  if p<1 then begin
    Result:=false;
    Value1:='';
  end else begin
    Value1:=copy(Src,p,length(Src)-length(Pattern)+2);
    Result:=true;
  end;
end;

function GetFPCMsgValues(Src, Pattern: string; out Value1, Value2: string
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
  if LeftStr(Pattern,p1)<>LeftStr(Src,p1) then exit;
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

procedure TPatternToMsgIDs.Add(Pattern: string; MsgID: integer);

  procedure RaiseInvalidMsgID;
  var
    s: String;
  begin
    s:='invalid MsgID: '+IntToStr(MsgID);
    raise Exception.Create(s);
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
end;

procedure TPatternToMsgIDs.AddLines(const Lines: string; MsgID: integer);
var
  StartPos: PChar;
  p: PChar;
begin
  p:=PChar(Lines);
  while p^<>#0 do begin
    StartPos:=p;
    while not (p^ in [#0,#10,#13]) do inc(p);
    if p>StartPos then begin
      Add(copy(Lines,StartPos-PChar(Lines)+1,p-StartPos),MsgID);
    end;
    while p^ in [#10,#13] do inc(p);
  end;
end;

function TPatternToMsgIDs.LineToMsgID(p: PChar): integer;
var
  i: Integer;
begin
  while p^ in [' ',#9,#10,#13] do inc(p);
  i:=IndexOf(p,false);
  if i<0 then
    Result:=0
  else
    Result:=fItems[i].MsgID;
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

constructor TFPCMsgFilePool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitCriticalSection(fCritSec);
  FFiles:=TFPList.Create;
  fPendingLog:=TStringList.Create;
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
        debugln(['TFPCMsgFilePool.Destroy file still used: ',Item.Filename]);
      end;
    end;
    if FFiles.Count>0 then
      raise Exception.Create('TFPCMsgFilePool.Destroy some files are still used');
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

var
  Item: TFPCMsgFilePoolItem;
  i: Integer;
  NewItem: TFPCMsgFilePoolItem;
  FileTxt: string;
  Code: TCodeBuffer;
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
  EnterCriticalsection;
  try
    // search the newest version in cache
    for i:=FFiles.Count-1 downto 0 do begin
      Item:=TFPCMsgFilePoolItem(FFiles[i]);
      if CompareFilenames(Item.Filename,aFilename)<>0 then continue;
      Result:=Item;
      break;
    end;
    Code:=nil;
    if UpdateFromDisk then begin
      if IsMainThread then begin
        Code:=CodeToolBoss.LoadFile(aFilename,true,false);
        if (Code<>nil) and (Result<>nil) and (Code.FileDateOnDisk<>Result.LoadedFileAge)
        then
          ResultOutdated;
      end else begin
        if (Result<>nil)
        and (FileAgeUTF8(aFilename)<>Result.LoadedFileAge) then
          ResultOutdated;
      end;
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
      //Log('TFPCMsgFilePool.LoadFile '+dbgs(NewItem.FFile<>nil)+' '+aFilename,aThread);
      if Assigned(OnLoadFile) then begin
        OnLoadFile(aFilename,FileTxt);
        NewItem.FFile.LoadFromText(FileTxt);
        NewItem.FLoadedFileAge:=FileAgeUTF8(aFilename);
      end else begin
        if IsMainThread then begin
          if Code=nil then
            Code:=CodeToolBoss.LoadFile(aFilename,true,false);
          if Code=nil then
            exit;
          NewItem.FFile.LoadFromText(Code.Source);
          NewItem.FLoadedFileAge:=Code.FileDateOnDisk;
        end else begin
          NewItem.FFile.LoadFromFile(aFilename);
          NewItem.FLoadedFileAge:=FileAgeUTF8(aFilename);
        end;
      end;
      // load successful
      Result:=NewItem;
      NewItem:=nil;
      FFiles.Add(Result);
      inc(Result.fUseCount);
      //log('TFPCMsgFilePool.LoadFile '+Result.Filename+' '+dbgs(Result.fUseCount),aThread);
    end;
  finally
    FreeAndNil(NewItem);
    LeaveCriticalSection;
  end;
end;

procedure TFPCMsgFilePool.UnloadFile(var aFile: TFPCMsgFilePoolItem;
  AThread: TThread);
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
  anEnglishFile:=DefaultEnglishFile;
  aTranslationFile:=DefaulTranslationFile;
  if IsFPCExecutable(CompilerFilename,ErrMsg) then
    FPCVer:=CodeToolBoss.FPCDefinesCache.GetFPCVersion(CompilerFilename,TargetOS,TargetCPU,false)
  else
    FPCVer:='';
  FPCSrcDir:=EnvironmentOptions.GetParsedFPCSourceDirectory(FPCVer);
  if FilenameIsAbsolute(FPCSrcDir) then begin
    aFilename:=AppendPathDelim(FPCSrcDir)+SetDirSeparators('compiler/msg/errore.msg');
    if FileExistsCached(aFilename) then
      anEnglishFile:=aFilename;
    // ToDo: translation
  end;
end;

{ TFPCMsgFilePoolItem }

constructor TFPCMsgFilePoolItem.Create(aPool: TFPCMsgFilePool;
  const aFilename: string);
begin
  inherited Create;
  FPool:=aPool;
  FFilename:=aFilename;
  FFile:=TFPCMsgFile.Create;
end;

destructor TFPCMsgFilePoolItem.Destroy;
begin
  FreeAndNil(FFile);
  FFilename:='';
  inherited Destroy;
end;

function TFPCMsgFilePoolItem.GetMsg(ID: integer): TFPCMsgItem;
begin
  Result:=FFile.FindWithID(ID);
end;

{ TIDEFPCParser }

destructor TIDEFPCParser.Destroy;
begin
  FreeAndNil(fFileExists);
  FreeAndNil(fLastSource);
  if TranslationFile<>nil then
    FPCMsgFilePool.UnloadFile(TranslationFile,nil);
  if MsgFile<>nil then
    FPCMsgFilePool.UnloadFile(MsgFile,nil);
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
        List:=FPCMsgFilePool.LoadFile(aFilename,true,nil);
        debugln(['LoadMsgFile successfully read ',aFilename]);
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
begin
  inherited Init;

  if FPCMsgFilePool<>nil then begin
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
    FPCMsgFilePool.GetMsgFileNames(Tool.Process.Executable,aTargetOS,aTargetCPU,
      MsgFilename,TranslationFilename);
  end;

  LoadMsgFile(MsgFilename,MsgFile);
  if TranslationFilename<>'' then
    LoadMsgFile(TranslationFilename,TranslationFile);
end;

procedure TIDEFPCParser.InitReading;
var
  Item: TFPCMsgItem;
begin
  inherited InitReading;

  fLineToMsgID.Clear;
  // FPC logo lines
  Item:=MsgFile.GetMsg(11023);
  if Item<>nil then
    fLineToMsgID.AddLines(Item.Pattern,Item.ID);
  // Linking <progname>
  Item:=MsgFile.GetMsg(9015);
  if Item<>nil then
    fLineToMsgID.AddLines(Item.Pattern,Item.ID);
  //fLineToMsgID.WriteDebugReport;

  fLastWorkerImprovedMessage[false]:=-1;
  fLastWorkerImprovedMessage[true]:=-1;
end;

procedure TIDEFPCParser.Done;
begin
  FreeAndNil(fLastSource);
  inherited Done;
end;

function TIDEFPCParser.CheckForCompilingState(p: PChar): boolean;
var
  AFilename: string;
  MsgLine: TMessageLine;
  OldP: PChar;
begin
  Result:=fMsgID=3104;
  if (fMsgID>0) and not Result then exit;
  OldP:=p;
  if not CompStr('Compiling ',p) then exit;
  // for example 'Compiling ./subdir/unit1.pas'
  // add path to history
  if DirectoryStack=nil then DirectoryStack:=TStringList.Create;
  inc(p,length('Compiling '));
  if (p^='.') and (p[1]=PathDelim) then
    inc(p,2); // skip ./
  AFilename:=TrimFilename(p);
  //DirectoryStack.Add(AFilename);
  MsgLine:=CreateMsgLine;
  MsgLine.Urgency:=mluProgress;
  MsgLine.SubTool:=SubToolFPC;
  MsgLine.Filename:=AFilename;
  MsgLine.Msg:=OldP;
  AddMsgLine(MsgLine);
  Result:=true;
end;

function TIDEFPCParser.CheckForAssemblingState(p: PChar): boolean;
var
  MsgLine: TMessageLine;
  OldP: PChar;
begin
  Result:=fMsgID=9001;
  if (fMsgID>0) and not Result then exit;
  OldP:=p;
  if (not Result) and (not CompStr('Assembling ',p)) then exit;
  MsgLine:=CreateMsgLine;
  MsgLine.Urgency:=mluProgress;
  MsgLine.SubTool:=SubToolFPC;
  MsgLine.Urgency:=mluProgress;
  MsgLine.Msg:=OldP;
  AddMsgLine(MsgLine);
  Result:=true;
end;

function TIDEFPCParser.CheckForGeneralMessage(p: PChar): boolean;
{ check for
  Fatal: message
  Hint: (11030) Start of reading config file /etc/fpc.cfg
  Error: /usr/bin/ppc386 returned an error exitcode
}
const
  MsgIDCompilationAborted = 1018;
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
  // check if there was already a message of this urgency
  // if yes, then downgrade this message to a mluVerbose
  begin
    i:=Tool.WorkerMessages.Count-1;
    if (i>=0) and (Tool.WorkerMessages[i].Urgency>=MsgType) then begin
      MsgType:=mluVerbose;
    end;
  end;

begin
  Result:=false;
  MsgType:=mluNone;
  if ReadString(p,'Fatal: ') then begin
    MsgType:=mluFatal;
    // check for "Fatal: compilation aborted"
    MsgItem:=MsgFile.GetMsg(MsgIDCompilationAborted);
    if (MsgItem<>nil) and ReadString(p,MsgItem.Pattern) then
      CheckFinalNote;
  end
  else if ReadString(p,'Panic') then
    MsgType:=mluPanic
  else if ReadString(p,'Error: ') then begin
    // check for fpc frontend message "Error: /usr/bin/ppc386 returned an error exitcode"
    TranslatedMsg:=p;
    MsgType:=mluError;
    if Pos(FrontEndFPCExitCodeError,TranslatedMsg)>0 then begin
      fMsgID:=MsgIDCompilationAborted;
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
        if (TranslationFile<>nil) then
          TranslatedItem:=TranslationFile.GetMsg(fMsgID);
        if (MsgFile<>nil) then
          MsgItem:=MsgFile.GetMsg(fMsgID);
        Translate(p,MsgItem,TranslatedItem,TranslatedMsg,MsgType);
        if (TranslatedItem=nil) and (MsgItem=nil) then begin
          debugln(['TFPCParser.CheckForGeneralMessage msgid not found: ',fMsgID]);
        end;
      end;

    end;
  end;
  if (MsgType>=mluError) and (fMsgID=MsgIDCompilationAborted) // fatal: Compilation aborted
  then begin
    CheckFinalNote;
  end;
  MsgLine:=CreateMsgLine;
  MsgLine.Urgency:=MsgType;
  MsgLine.SubTool:=SubToolFPC;
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
  MsgLine.SubTool:=SubToolFPC;
  MsgLine.Urgency:=mluProgress;
  MsgLine.Msg:=OldP;
  AddMsgLine(MsgLine);
  Result:=true;
end;

function TIDEFPCParser.CheckForLinesCompiled(p: PChar): boolean;
var
  OldStart: PChar;
  MsgLine: TMessageLine;
begin
  Result:=fMsgID=1008;
  if (fMsgID>0) and not Result then exit;
  OldStart:=p;
  if not Result then begin
    if not ReadNumberWithThousandSep(p) then exit;
    if not ReadString(p,' lines compiled, ') then exit;
    if not ReadNumberWithThousandSep(p) then exit;
  end;
  MsgLine:=CreateMsgLine;
  MsgLine.SubTool:=SubToolFPC;
  MsgLine.Urgency:=mluProgress;
  MsgLine.Msg:=OldStart;
  AddMsgLine(MsgLine);
  Result:=true;
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
  if (fMsgID>0) and not Result then exit;
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
  MsgLine.SubTool:=SubToolFPC;
  MsgLine.Urgency:=mluProgress;
  MsgLine.Msg:=OldStart;
  AddMsgLine(MsgLine);
end;

function TIDEFPCParser.CheckForLinkingErrors(p: PChar): boolean;
{ For example:
  linkerror.o(.text$_main+0x9):linkerror.pas: undefined reference to `NonExistingFunction'

  Closing script ppas.sh

  Mac OS X linker example:
  ld: framework not found Cocoas

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
}
var
  OldStart: PChar;
  MsgLine: TMessageLine;
  i: Integer;
  PrevMsgLine: TMessageLine;
//const
//  DarwinPrefixLvl1 = '  ';
//  DarwinPrefixLvl2 = '      ';
begin
  Result:=false;
  OldStart:=p;

  i:=Tool.WorkerMessages.Count-1;
  if i>=0 then begin
    PrevMsgLine:=Tool.WorkerMessages[i];
    if (PrevMsgLine.SubTool=SubToolFPCLinker)
    or ((PrevMsgLine.SubTool=SubToolFPC) and (PrevMsgLine.MsgID=9015)) // (9015) Linking <progname>
    then begin
      // this is a follow up linker warning/error
      MsgLine:=CreateMsgLine;
      MsgLine.SubTool:=SubToolFPCLinker;
      MsgLine.Urgency:=PrevMsgLine.Urgency;
      MsgLine.Msg:=OldStart;
      AddMsgLine(MsgLine);
      exit(true);
    end;
  end;

  if CompStr('Closing script ppas.sh',p) then begin
    MsgLine:=CreateMsgLine;
    MsgLine.SubTool:=SubToolFPCLinker;
    MsgLine.Urgency:=mluWarning;
    MsgLine.Msg:=OldStart;
    AddMsgLine(MsgLine);
    exit(true);
  end;

  while p^ in ['0'..'9','a'..'z','A'..'Z','_'] do
    inc(p);
  if CompStr('.o(',p) then begin
    MsgLine:=CreateMsgLine;
    MsgLine.SubTool:=SubToolFPCLinker;
    MsgLine.Urgency:=mluWarning;
    MsgLine.Msg:=OldStart;
    AddMsgLine(MsgLine);
    exit(true);
  end;
  p := OldStart;
  if CompStr('ld: ',p) then begin
    Result:=true;
    MsgLine:=CreateMsgLine;
    MsgLine.SubTool:=SubToolFPCLinker;
    MsgLine.Urgency:=mluWarning;
    MsgLine.Msg:=OldStart;
    AddMsgLine(MsgLine);
    exit(true);
  end;
  if CompStr('Undefined symbols:', p) then begin
    Result:=true;
    MsgLine:=CreateMsgLine;
    MsgLine.SubTool:=SubToolFPCLinker;
    MsgLine.Urgency:=mluWarning;
    MsgLine.Msg:=OldStart;
    AddMsgLine(MsgLine);
    exit(true);
  end;
end;

function TIDEFPCParser.CheckForFollowUpMessages(p: PChar): boolean;
var
  i: Integer;
  PrevMsgLine: TMessageLine;
  MsgLine: TMessageLine;
begin
  Result:=false;
  i:=Tool.WorkerMessages.Count-1;
  if i<0 then exit;
  PrevMsgLine:=Tool.WorkerMessages[i];

  if (PrevMsgLine.SubTool=SubToolFPCLinker)
  or ((PrevMsgLine.SubTool=SubToolFPC) and (PrevMsgLine.MsgID=9015)) // (9015) Linking <progname>
  then begin
    // this is a follow up linker warning/error
    MsgLine:=CreateMsgLine;
    MsgLine.SubTool:=SubToolFPCLinker;
    MsgLine.Urgency:=PrevMsgLine.Urgency;
    MsgLine.Msg:=p;
    AddMsgLine(MsgLine);
    exit(true);
  end;

  if (PrevMsgLine.SubTool=SubToolFPCRes)
  or ((PrevMsgLine.SubTool=SubToolFPC)
    and ((PrevMsgLine.MsgID=9022)   // (9022) Compiling resource <resource>
      or (PrevMsgLine.MsgID=9028))) // (9028) Calling resource compiler "/usr/bin/fpcres" with ...
  then begin
    // this is a follow up resource compiler warning/error
    MsgLine:=CreateMsgLine;
    MsgLine.SubTool:=SubToolFPCRes;
    MsgLine.Urgency:=PrevMsgLine.Urgency;
    MsgLine.Msg:=p;
    debugln(['TFPCParser.CheckForCompilingResourceErrors ',MsgLine.Msg,' ',dbgs(MsgLine.Urgency)]);
    AddMsgLine(MsgLine);
    exit(true);
  end;
end;

function TIDEFPCParser.CheckForRecompilingChecksumChangedMessages(p: PChar
  ): boolean;
// example: Recompiling GtkInt, checksum changed for gdk2x
var
  OldStart: PChar;
  MsgLine: TMessageLine;
begin
  Result:=fMsgID=10028;
  if (fMsgID>0) and not Result then exit;
  OldStart:=p;
  if not Result then begin
    if not CompStr('Recompiling ',p) then exit;
    while not (p^ in [',',#0]) do inc(p);
    if not CompStr(', checksum changed for ',p) then exit;
    Result:=true;
  end;
  MsgLine:=CreateMsgLine;
  MsgLine.SubTool :=SubToolFPC;
  MsgLine.Urgency:=mluVerbose;
  MsgLine.Msg:=OldStart;
  AddMsgLine(MsgLine);
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
  MsgLine.SubTool:='windres';
  MsgLine.Urgency:=mluWarning;
  p := wPos + 7;
  if CompStr('.exe', p) then
    inc(p, 4);
  MsgLine.Msg:='windres' + p;
  AddMsgLine(MsgLine);
end;

function TIDEFPCParser.CheckForInfos(p: PChar): boolean;
var
  MsgItem: TFPCMsgItem;
  MsgLine: TMessageLine;
  i: Integer;
  MsgType: TMessageLineUrgency;
begin
  Result:=false;
  i:=fLineToMsgID.LineToMsgID(p);
  if i=0 then exit;
  fMsgID:=i;
  MsgItem:=MsgFile.GetMsg(fMsgID);
  if MsgItem=nil then exit;
  Result:=true;
  MsgType:=FPCMsgToMsgUrgency(MsgItem);
  if MsgType=mluNone then
    MsgType:=mluVerbose;
  MsgLine:=CreateMsgLine;
  MsgLine.SubTool:=SubToolFPC;
  MsgLine.Urgency:=MsgType;
  AddMsgLine(MsgLine);
end;

function TIDEFPCParser.CreateMsgLine: TMessageLine;
begin
  Result:=inherited CreateMsgLine(fOutputIndex);
  Result.MsgID:=fMsgID;
end;

procedure TIDEFPCParser.ImproveMsgHiddenByIDEDirective(const SourceOK: Boolean;
  var MsgLine: TMessageLine);
var
  p: PChar;
  X: Integer;
  Y: Integer;
begin
  // check for {%H-}
  X:=MsgLine.Column;
  Y:=MsgLine.Line;
  if SourceOK and (not (mlfHiddenByIDEDirectiveValid in MsgLine.Flags)) then
  begin
    if (y<=fLastSource.LineCount) and (x-1<=fLastSource.GetLineLength(y-1))
    then begin
      p:=PChar(fLastSource.Source)+fLastSource.GetLineStart(y-1)+x-2;
      //debugln(['TFPCParser.ImproveMessages ',aFilename,' ',Y,',',X,' ',copy(fLastSource.GetLine(y-1),1,x-1),'|',copy(fLastSource.GetLine(y-1),x,100),' p=',p[0],p[1],p[2]]);
      if ((p^='{') and (p[1]='%') and (p[2]='H') and (p[3]='-'))
      or ((x>5) and (p[-5]='{') and (p[-4]='%') and (p[-3]='H') and (p[-2]='-')
        and (p[-1]='}'))
      then begin
        //debugln(['TFPCParser.ImproveMessages HIDDEN ',aFilename,' ',Y,',',X,' ',MsgLine.Msg]);
        MsgLine.Flags:=MsgLine.Flags+[mlfHiddenByIDEDirective,
          mlfHiddenByIDEDirectiveValid];
      end;
    end;
    MsgLine.Flags:=MsgLine.Flags+[mlfHiddenByIDEDirectiveValid];
  end;
end;

procedure TIDEFPCParser.ImproveMsgSenderNotUsed(const MsgLine: TMessageLine);
begin
  // check for Sender not used
  if (MsgLine.MsgID=5024) // parameter $1 not used
  and (MsgLine.Urgency>mluVerbose)
  and (MsgLine.Msg='Parameter "Sender" not used') then begin
    // almost always not important
    MsgLine.Urgency:=mluVerbose;
  end;
end;

procedure TIDEFPCParser.ImproveMsgUnitNotUsed(aSynchronized: boolean;
  const aFilename: String; var MsgLine: TMessageLine);
// check for Unit not used message in main sources
// and change urgency to merely 'verbose'
begin
  if (MsgLine.MsgID<>5023) // Unit $1 not used
  or (MsgLine.Urgency<=mluVerbose) then exit;
  //debugln(['TIDEFPCParser.ImproveMsgUnitNotUsed ',aSynchronized,' ',MsgLine.Msg]);
  // unit not used
  if FilenameIsAbsolute(aFilename)
  and ((CompareFileExt(aFilename, 'lpr', false)=0)
    or FileExists(ChangeFileExt(aFilename, '.lpk'), aSynchronized))
  then begin
    // a lpk/lpr does not use a unit => almost always not important
    MsgLine.Urgency:=mluVerbose;
  end else begin
    if aSynchronized then begin
      // ToDo: check if this is the main unit of a project/package
      MsgLine.Urgency:=mluVerbose;
    end else begin
      NeedSynchronize:=true;
    end;
  end;
end;

procedure TIDEFPCParser.ImproveMsgUnitNotFound(aSynchronized: boolean;
  var MsgLine: TMessageLine);

  procedure FixSourcePos(CodeBuf: TCodeBuffer; MissingUnitname: string);
  var
    InPos: Integer;
    NamePos: Integer;
    Tool: TCodeTool;
    Caret: TCodeXYPosition;
    NewFilename: String;
  begin
    {$IFDEF VerboseQuickFixUnitNotFoundPosition}
    debugln(['TIDEFPCParser.ImproveMsgUnitNotFound File=',CodeBuf.Filename]);
    {$ENDIF}
    LazarusIDE.SaveSourceEditorChangesToCodeCache(nil);
    if not CodeToolBoss.FindUnitInAllUsesSections(CodeBuf,MissingUnitname,NamePos,InPos)
    then begin
      DebugLn('QuickFixUnitNotFoundPosition failed due to syntax errors or '+MissingUnitname+' is not used in '+CodeBuf.Filename);
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

  procedure FindPPUInInstalledPkgs(MissingUnitname: string;
    var PPUFilename, PkgName: string);
  var
    i: Integer;
    Pkg: TIDEPackage;
    DirCache: TCTDirectoryCache;
    UnitOutDir: String;
  begin
    // search ppu in installed packages
    for i:=0 to PackageEditingInterface.GetPackageCount-1 do begin
      Pkg:=PackageEditingInterface.GetPackages(i);
      if Pkg.AutoInstall=pitNope then continue;
      UnitOutDir:=Pkg.LazCompilerOptions.GetUnitOutputDirectory(false);
      //debugln(['TQuickFixUnitNotFoundPosition.Execute ',Pkg.Name,' UnitOutDir=',UnitOutDir]);
      if FilenameIsAbsolute(UnitOutDir) then begin
        DirCache:=CodeToolBoss.DirectoryCachePool.GetCache(UnitOutDir,true,false);
        PPUFilename:=DirCache.FindFile(MissingUnitname+'.ppu',ctsfcLoUpCase);
        //debugln(['TQuickFixUnitNotFoundPosition.Execute ShortPPU=',PPUFilename]);
        if PPUFilename<>'' then begin
          PkgName:=Pkg.Name;
          PPUFilename:=AppendPathDelim(DirCache.Directory)+PPUFilename;
          break;
        end;
      end;
    end;
  end;

  procedure FindPackage(MissingUnitname: string; var PkgName: string;
    OnlyInstalled: boolean);
  var
    i: Integer;
    Pkg: TIDEPackage;
    j: Integer;
    PkgFile: TLazPackageFile;
  begin
    if PkgName='' then begin
      // search unit in installed packages
      for i:=0 to PackageEditingInterface.GetPackageCount-1 do begin
        Pkg:=PackageEditingInterface.GetPackages(i);
        if OnlyInstalled and (Pkg.AutoInstall=pitNope) then continue;
        if CompareTextCT(Pkg.Name,MissingUnitname)=0 then begin
          PkgName:=Pkg.Name;
          break;
        end;
        for j:=0 to Pkg.FileCount-1 do begin
          PkgFile:=Pkg.Files[j];
          if not FilenameIsPascalUnit(PkgFile.Filename) then continue;
          if CompareTextCT(ExtractFileNameOnly(PkgFile.Filename),MissingUnitname)<>0
          then continue;
          PkgName:=Pkg.Name;
          break;
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
  PPUFilename: String;
  PkgName: String;
  OnlyInstalled: Boolean;
  s: String;
begin
  if (not aSynchronized)
  or (MsgLine.MsgID<>10022) // Can't find unit $1 used by $2
  then exit;

  if not TFPCParser.GetFPCMsgValues(MsgLine,MissingUnitName,UsedByUnit) then
    exit;

  {$IFDEF VerboseQuickFixUnitNotFoundPosition}
  debugln(['TIDEFPCParser.ImproveMsgUnitNotFound Missing="',MissingUnitname,'" used by "',UsedByUnit,'"']);
  {$ENDIF}

  CodeBuf:=nil;
  Filename:=MsgLine.GetFullFilename;
  if (CompareFilenames(ExtractFileName(Filename),'staticpackages.inc')=0)
  and IsFileInIDESrcDir(Filename) then begin
    // common case: when building the IDE a package unit is missing
    // staticpackages.inc(1,1) Fatal: Can't find unit sqldblaz used by Lazarus
    // change to lazarus.pp(1,1)
    Filename:=AppendPathDelim(EnvironmentOptions.GetParsedLazarusDirectory)+'ide'+PathDelim+'lazarus.pp';
    MsgLine.SetSourcePosition(Filename,1,1);
    MsgLine.Msg:='Can''t find a valid '+MissingUnitname+'.ppu';
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
      NewFilename:=LazarusIDE.FindUnitFile(UsedByUnit);
      if NewFilename='' then begin
        {$IFDEF VerboseQuickFixUnitNotFoundPosition}
        debugln(['TIDEFPCParser.ImproveMsgUnitNotFound unit not found: ',UsedByUnit);
        {$ENDIF}
      end;
    end;
    if NewFilename<>'' then
      Filename:=NewFilename;
  end;

  if Filename<>'' then begin
    CodeBuf:=CodeToolBoss.LoadFile(Filename,false,false);
    if CodeBuf=nil then begin
      {$IFDEF VerboseQuickFixUnitNotFoundPosition}
      debugln(['TIDEFPCParser.ImproveMsgUnitNotFound unable to load unit: ',Filename);
      {$ENDIF}
    end;
  end else begin
    {$IFDEF VerboseQuickFixUnitNotFoundPosition}
    debugln(['TIDEFPCParser.ImproveMsgUnitNotFound unable to locate UsedByUnit: ',UsedByUnit);
    {$ENDIF}
  end;

  // fix line and column
  Owners:=nil;
  UsedByOwner:=nil;
  try
    if CodeBuf<>nil then begin
      FixSourcePos(CodeBuf,MissingUnitname);
      Owners:=PackageEditingInterface.GetOwnersOfUnit(CodeBuf.Filename);
      if (Owners<>nil) and (Owners.Count>0) then
        UsedByOwner:=TObject(Owners[0]);
    end;

    // if the ppu exists then improve the message
    {$IFDEF VerboseQuickFixUnitNotFoundPosition}
    debugln(['TIDEFPCParser.ImproveMsgUnitNotFound Filename=',CodeBuf.Filename]);
    {$ENDIF}
    if FilenameIsAbsolute(CodeBuf.Filename) then begin
      PPUFilename:=CodeToolBoss.DirectoryCachePool.FindCompiledUnitInCompletePath(
                        ExtractFilePath(CodeBuf.Filename),MissingUnitname);
      {$IFDEF VerboseQuickFixUnitNotFoundPosition}
      debugln(['TQuickFixUnitNotFoundPosition.Execute PPUFilename=',PPUFilename,' IsFileInIDESrcDir=',IsFileInIDESrcDir(Dir+'test')]);
      {$ENDIF}
      PkgName:='';
      OnlyInstalled:=IsFileInIDESrcDir(CodeBuf.Filename);
      if OnlyInstalled and (PPUFilename='') then begin
        FindPPUInInstalledPkgs(MissingUnitname,PPUFilename,PkgName);
      end;

      FindPackage(MissingUnitname,PkgName,OnlyInstalled);
      if PPUFilename<>'' then begin
        // there is a ppu file in the unit path
        if PPUFilename<>'' then begin
          // there is a ppu file, but the compiler didn't like it
          // => change message
          s:='Can not find '+MissingUnitname;
          if UsedByUnit<>'' then
            s+=' used by '+UsedByUnit;
          s+=', ppu='+CreateRelativePath(PPUFilename,ExtractFilePath(CodeBuf.Filename));
          if PkgName<>'' then
            s+=', package '+PkgName;
        end else if PkgName<>'' then begin
          // ppu is missing, but the package is known
          // => change message
          s:='Can''t find ppu of unit '+MissingUnitname;
          if UsedByUnit<>'' then
            s+=' used by '+UsedByUnit;
          s+='. Maybe package '+PkgName+' needs a clean rebuild.';
        end;
      end else begin
        // there is no ppu file in the unit path
        s:='Can not find unit '+MissingUnitname;
        if UsedByUnit<>'' then
          s+=' used by '+UsedByUnit;
        if (UsedByOwner is TIDEPackage)
        and (CompareTextCT(TIDEPackage(UsedByOwner).Name,PkgName)=0) then
        begin
          // two units of a package can not find each other
          s+='. Check search path package '+TIDEPackage(UsedByOwner).Name+', try a clean rebuild, check implementation uses sections.';
        end else begin
          if PkgName<>'' then
            s+='. Check if package '+PkgName+' is in the dependencies';
          if UsedByOwner is TLazProject then
            s+=' of the project inspector'
          else if UsedByOwner is TIDEPackage then
            s+=' of package '+TIDEPackage(UsedByOwner).Name;
        end;
        s+='.';
      end;
      MsgLine.Msg:=s;
      {$IFDEF VerboseQuickFixUnitNotFoundPosition}
      debugln(['TIDEFPCParser.ImproveMsgUnitNotFound Msg.Msg="',Msg.Msg,'"']);
      {$ENDIF}
    end;
  finally
    Owners.Free;
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

constructor TIDEFPCParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fLineToMsgID:=TPatternToMsgIDs.Create;
  fFileExists:=TFilenameToPointerTree.Create(false);
end;

function TIDEFPCParser.FileExists(const Filename: string; aSynchronized: boolean
  ): boolean;
var
  p: Pointer;
begin
  p:=fFileExists[Filename];
  if p=Pointer(Self) then
    Result:=true
  else if p=Pointer(fFileExists) then
    Result:=false
  else begin
    if aSynchronized then
      Result:=FileExistsCached(Filename)
    else
      Result:=FileExistsUTF8(Filename);
    if Result then
      fFileExists[Filename]:=Pointer(Self)
    else
      fFileExists[Filename]:=Pointer(fFileExists);
  end;
end;

function TIDEFPCParser.CheckForMsgId(p: PChar): boolean;
var
  MsgItem: TFPCMsgItem;
  TranslatedItem: TFPCMsgItem;
  MsgLine: TMessageLine;
  TranslatedMsg: String;
  MsgType: TMessageLineUrgency;
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
  Translate(p,MsgItem,TranslatedItem,TranslatedMsg,MsgType);
  Msg:=p;
  case fMsgID of
  9029: // Error while compiling resources
    Msg+=' -> Compile with -vd for more details. Check for duplicates.';
  end;
  MsgLine:=CreateMsgLine;
  MsgLine.SubTool:=SubToolFPC;
  MsgLine.Urgency:=MsgType;
  MsgLine.Msg:=Msg;
  MsgLine.TranslatedMsg:=TranslatedMsg;
  AddMsgLine(MsgLine);
end;

function TIDEFPCParser.CheckForFileLineColMessage(p: PChar): boolean;
{ filename(line,column) Hint: message
  filename(line,column) Hint: (msgid) message
  filename(line) Hint: (msgid) message
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
begin
  Result:=false;
  FileStartPos:=p;
  while not (p^ in ['(',#0]) do inc(p);
  if (p^<>'(') or (p=FileStartPos) or (p[-1]=' ') then exit;
  FileEndPos:=p;
  inc(p); // skip bracket
  LineStartPos:=p;
  //writeln('TFPCParser.CheckForFileLineColMessage ',FileStartPos);
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
  if ReadString(p,'Hint:') then begin
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
        Translate(p,MsgItem,TranslatedItem,TranslatedMsg,MsgType);
        if (TranslatedItem=nil) and (MsgItem=nil) then begin
          debugln(['TFPCParser.CheckForFileLineColMessage msgid not found: ',fMsgID]);
        end else if MsgType=mluNone then begin
          debugln(['TFPCParser.CheckForFileLineColMessage msgid has no type: ',fMsgID]);
        end;
      end;
    end;
  end;
  if ColStartPos<>nil then
    Column:=Str2Integer(ColStartPos,0)
  else
    Column:=0;
  MsgLine:=CreateMsgLine;
  MsgLine.SubTool:=SubToolFPC;
  MsgLine.Urgency:=MsgType;
  aFilename:=GetString(FileStartPos,FileEndPos-FileStartPos);
  aFilename:=LongenFilename(aFilename);
  MsgLine.Filename:=aFilename;
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
  if (fMsgID>0) and not Result then exit;
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
  MsgLine.SubTool:=SubToolFPC;
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
  p:=PChar(Line);
  fOutputIndex:=OutputIndex;
  fMsgID:=0;

  //writeln('TFPCParser.ReadLine ',Line);
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
    while p^ in [' '] do inc(p);
  end;

  if p^ in [#0..#31,' '] then exit; // not a fpc message

  Handled:=true;

  // check for (msgid) message
  if CheckForMsgId(p) then exit;
  // check for 'filename(line,column) Error: message'
  if CheckForFileLineColMessage(p) then exit;
  // check for infos (logo, Linking <Progname>)
  if CheckForInfos(p) then exit;
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
  // check for -vx output
  if CheckForExecutableInfo(p) then exit;
  // check for linking errors
  if CheckForLinkingErrors(p) then exit;
  // check for follow up errors (linker and fpcres messages)
  if CheckForFollowUpMessages(p) then exit;
  // check for Recompiling, checksum changed
  if CheckForRecompilingChecksumChangedMessages(p) then exit;
  // check for Load from unit
  if CheckForLoadFromUnit(p) then exit;
  // check for windres errors
  if CheckForWindresErrors(p) then exit;

  {$IFDEF VerboseFPCParser}
  writeln('TFPCParser.ReadLine UNKNOWN: ',Line);
  {$ENDIF}
  Handled:=false;

  {
            else if (not CompilerOptions.ShowHintsForUnusedUnitsInMainSrc) then
            begin
              MainSrcFilename:=CompilerOptions.GetDefaultMainSourceFileName;
              if (MainSrcFilename<>'')
              and (IsHintForUnusedUnit(s,MainSrcFilename)) then
                SkipMessage:=true;
          if copy(s,j+2,length(s)-j-1)='Error while linking' then begin
            DoAddLastLinkerMessages(true);
          end
          else if copy(s,j+2,length(AsmError))=AsmError then begin
            DoAddLastAssemblerMessages;
          end;
        end;

      // beautify compiler message

      // the compiler always gives short filenames, even if it went into a
      // subdirectory
      // -> prepend the current subdirectory
      Msg:=s;
      Filename:=TrimFilename(copy(Msg,1,FilenameEndPos));
      if not FilenameIsAbsolute(Filename) then begin
        // filename is relative
        i:=-1;
        if (fCompilingHistory<>nil) then begin
          // the compiler writes a line compiling ./subdir/unit.pas
          // and then writes the messages without any path
          // -> prepend this subdirectory
          i:=fCompilingHistory.Count-1;
          while (i>=0) do begin
            CurCompHistory:=fCompilingHistory[i];
            CurCompHistLen:=length(CurCompHistory);
            CurFilenameLen:=length(Filename);
            j:=CurCompHistLen-CurFilenameLen;
            if (j>1) and (CurCompHistory[j]=PathDelim)
            and (CompareFilenames(
              copy(CurCompHistory,j+1,CurFilenameLen),Filename)=0) then
            begin
              Msg:=copy(CurCompHistory,1,j)+Msg;
              inc(FilenameEndPos,j);
              break;
            end;
            dec(i);
          end;
        end;
        if i<0 then begin
          // this file is not a compiled pascal source
          // -> search for include files
          Filename:=SearchIncludeFile(Filename);
          Msg:=Filename+copy(Msg,FileNameEndPos+1,length(Msg)-FileNameEndPos);
          FileNameEndPos:=length(Filename);
        end;
      end;
  }
end;

function TIDEFPCParser.LongenFilename(aFilename: string): string;
begin
  Result:=TrimFilename(aFilename);
  if FilenameIsAbsolute(Result) then exit;
  if Tool.WorkerDirectory<>'' then begin
    Result:=AppendPathDelim(Tool.WorkerDirectory)+Result;
  end;
end;

procedure TIDEFPCParser.ImproveMessages(aSynchronized: boolean);
var
  i: Integer;
  MsgLine: TMessageLine;
  aFilename: String;
  Y: Integer;
  X: Integer;
  Code: TCodeBuffer;
  SourceOK: Boolean;
begin
  //debugln(['TIDEFPCParser.ImproveMessages START ',aSynchronized,' Last=',fLastWorkerImprovedMessage[aSynchronized],' Now=',Tool.WorkerMessages.Count]);
  for i:=fLastWorkerImprovedMessage[aSynchronized]+1 to Tool.WorkerMessages.Count-1 do
  begin
    MsgLine:=Tool.WorkerMessages[i];
    Y:=MsgLine.Line;
    X:=MsgLine.Column;
    if (Y>0) and (X>0)
    and (MsgLine.SubTool=SubToolFPC) and (MsgLine.Filename<>'')
    and (MsgLine.Urgency<mluError)
    then begin
      // get source
      SourceOK:=false;
      aFilename:=MsgLine.GetFullFilename;
      if (fLastSource<>nil)
      and (CompareFilenames(aFilename,fLastSource.Filename)=0) then begin
        SourceOK:=true;
      end else begin
        if aSynchronized then begin
          // load source file
          //debugln(['TFPCParser.ImproveMessages loading ',aFilename]);
          Code:=CodeToolBoss.LoadFile(aFilename,true,false);
          if Code<>nil then begin
            if fLastSource=nil then
              fLastSource:=TCodeBuffer.Create;
            fLastSource.Filename:=aFilename;
            fLastSource.Source:=Code.Source;
            SourceOK:=true;
          end;
        end else begin
          NeedSynchronize:=true;
        end;
      end;

      ImproveMsgHiddenByIDEDirective(SourceOK, MsgLine);
      ImproveMsgUnitNotFound(aSynchronized, MsgLine);
      ImproveMsgUnitNotUsed(aSynchronized, aFilename, MsgLine);
      ImproveMsgSenderNotUsed(MsgLine);
    end;
  end;
  fLastWorkerImprovedMessage[aSynchronized]:=Tool.WorkerMessages.Count-1;
end;

class function TIDEFPCParser.IsSubTool(const SubTool: string): boolean;
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
  if CompareText(SubTool,SubToolFPC)=0 then begin
    if FPCMsgFilePool=nil then exit;
    CurMsgFile:=FPCMsgFilePool.LoadFile(FPCMsgFilePool.DefaultEnglishFile,false,nil);
    if CurMsgFile=nil then exit;
    try
      MsgItem:=CurMsgFile.GetMsg(MsgID);
      if MsgItem=nil then exit;
      Result:=MsgItem.GetTrimmedComment(false,true);
    finally
      FPCMsgFilePool.UnloadFile(CurMsgFile,nil);
    end;
  end;
end;

class function TIDEFPCParser.GetMsgExample(SubTool: string; MsgID: integer
  ): string;
var
  CurMsgFile: TFPCMsgFilePoolItem;
  MsgItem: TFPCMsgItem;
begin
  Result:='';
  if CompareText(SubTool,SubToolFPC)=0 then begin
    if FPCMsgFilePool=nil then exit;
    CurMsgFile:=FPCMsgFilePool.LoadFile(FPCMsgFilePool.DefaultEnglishFile,false,nil);
    if CurMsgFile=nil then exit;
    try
      MsgItem:=CurMsgFile.GetMsg(MsgID);
      if MsgItem=nil then exit;
      Result:=MsgItem.Pattern;
    finally
      FPCMsgFilePool.UnloadFile(CurMsgFile,nil);
    end;
  end;
end;

class function TIDEFPCParser.Priority: integer;
begin
  Result:=SubToolFPCPriority;
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
  if Msg.SubTool<>SubToolFPC then exit;
  if not etFPCMsgParser.GetFPCMsgValue1(Msg.Msg,GetFPCMsgPattern(Msg),Result) then
    Result:='';
end;

class function TIDEFPCParser.GetFPCMsgValues(Msg: TMessageLine; out Value1,
  Value2: string): boolean;
begin
  Result:=false;
  if Msg.MsgID<=0 then exit;
  if Msg.SubTool<>SubToolFPC then exit;
  Result:=etFPCMsgParser.GetFPCMsgValues(Msg.Msg,GetFPCMsgPattern(Msg),Value1,Value2);
end;

finalization
  FreeAndNil(FPCMsgFilePool)

end.

