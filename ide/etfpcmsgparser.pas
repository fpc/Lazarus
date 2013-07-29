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
  Classes, SysUtils, FileProcs, KeywordFuncLists, IDEExternToolIntf,
  CodeToolsFPCMsgs, CodeToolsStructs, CodeCache, CodeToolManager, LazUTF8,
  etMakeMsgParser;

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

  TFPCMsgFilePool = class(TComponent)
  private
    fCritSec: TRTLCriticalSection;
    FDefaultEnglishFile: string;
    FDefaultLocaleFile: string;
    FFiles: TFPList; // list of TFPCMsgFilePoolItem sorted for loaded
    FOnLoadFile: TETLoadFileEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadFile(aFilename: string; UpdateFromDisk: boolean): TFPCMsgFilePoolItem;
    procedure UnloadFile(var aFile: TFPCMsgFilePoolItem);
    procedure EnterCriticalsection;
    procedure LeaveCriticalSection;
    property DefaultEnglishFile: string read FDefaultEnglishFile write FDefaultEnglishFile;
    property DefaultLocaleFile: string read FDefaultLocaleFile write FDefaultLocaleFile;
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
    procedure Init; override; // called before reading the output
    procedure Done; override;
    procedure ReadLine(Line: string; OutputIndex: integer; var Handled: boolean); override;
    function LongenFilename(aFilename: string): string;
    procedure ImproveMessages(aSynchronized: boolean); override;
    class function IsSubTool(const SubTool: string): boolean; override;
    class function DefaultSubTool: string; override;
    class function GetMsgExample(SubTool: string; MsgID: integer): string;
      override;
    class function GetMsgHint(SubTool: string; MsgID: integer): string;
      override;
    class function Priority: integer; override;
  end;

var
  FPCMsgFilePool: TFPCMsgFilePool = nil;

function FPCMsgToMsgUrgency(Msg: TFPCMsgItem): TMessageLineUrgency;
function TranslateFPCMsg(const Src, SrcPattern, TargetPattern: string): string;
function GetFPCMsgValue1(const Src, Pattern: string; out Value1: string): boolean;

procedure RegisterFPCParser;

implementation

function FPCMsgToMsgUrgency(Msg: TFPCMsgItem): TMessageLineUrgency;

  function TypToUrgency(const Typ: string): TMessageLineUrgency;
  begin
    Result:=mluNone;
    if (Typ='') or (length(Typ)<>1) then exit;
    case UpChars[Typ[1]] of
    'F': Result:=mluFatal;
    'E': Result:=mluError;
    'W': Result:=mluWarn;
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

begin
  Result:=mluNone;
  if Msg=nil then exit;
  Result:=TypToUrgency(Msg.ShownTyp);
  if Result<>mluNone then exit;
  Result:=TypToUrgency(Msg.Typ);
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

constructor TFPCMsgFilePool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitCriticalSection(fCritSec);
  FFiles:=TFPList.Create;
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
  finally
    LeaveCriticalSection;
  end;
  DoneCriticalsection(fCritSec);
end;

function TFPCMsgFilePool.LoadFile(aFilename: string; UpdateFromDisk: boolean
  ): TFPCMsgFilePoolItem;
var
  Item: TFPCMsgFilePoolItem;
  i: Integer;
  NewItem: TFPCMsgFilePoolItem;
  FileTxt: string;
begin
  Result:=nil;
  if aFilename='' then exit;
  aFilename:=TrimAndExpandFilename(aFilename);
  //debugln(['TFPCMsgFilePool.LoadFile ',aFilename]);

  if UpdateFromDisk then begin
    // Note: do not use FileExistsCached, called by threads
    if not FileExistsUTF8(aFilename) then begin
      debugln(['TFPCMsgFilePool.LoadFile file not found: ',aFilename]);
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
    if UpdateFromDisk then begin
      if (Result<>nil)
      and (FileAgeUTF8(aFilename)<>Result.LoadedFileAge) then begin
        // cached file needs update
        if Result.fUseCount=0 then begin
          FFiles.Remove(Result);
          Result.Free;
        end;
        Result:=nil;
      end;
    end else if Result=nil then begin
      // not yet loaded, not yet checked if file exists -> check now
      if not FileExistsUTF8(aFilename) then
        exit;
    end;

    if Result<>nil then begin
      // share
      inc(Result.fUseCount);
    end else begin
      // load for the first time
      NewItem:=TFPCMsgFilePoolItem.Create(Self,aFilename);
      //debugln(['TFPCMsgFilePool.LoadFile ',NewItem.FFile<>nil,' ',aFilename]);
      if Assigned(OnLoadFile) then begin
        OnLoadFile(aFilename,FileTxt);
        NewItem.FFile.LoadFromText(FileTxt);
      end else begin
        NewItem.FFile.LoadFromFile(aFilename);
      end;
      NewItem.FLoadedFileAge:=FileAgeUTF8(aFilename);
      // load successful
      Result:=NewItem;
      NewItem:=nil;
      FFiles.Add(Result);
      inc(Result.fUseCount);
      //debugln(['TFPCMsgFilePool.LoadFile ',Result.Filename,' ',Result.fUseCount]);
    end;
  finally
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
    //debugln(['TFPCMsgFilePool.UnloadFile ',aFile.Filename,' UseCount=',aFile.fUseCount]);
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
      //debugln(['TFPCMsgFilePool.UnloadFile free: ',aFile.Filename]);
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
    FPCMsgFilePool.UnloadFile(TranslationFile);
  if MsgFile<>nil then
    FPCMsgFilePool.UnloadFile(MsgFile);
  FreeAndNil(DirectoryStack);
  FreeAndNil(fLineToMsgID);
  inherited Destroy;
end;

procedure TIDEFPCParser.Init;

  procedure LoadMsgFile(aFilename: string; var List: TFPCMsgFilePoolItem);
  begin
    //debugln(['TFPCParser.Init load Msg filename=',aFilename]);
    if (aFilename<>'') and (List=nil) then begin
      try
        List:=FPCMsgFilePool.LoadFile(aFilename,true);
      except
        on E: Exception do begin
          debugln(['TFPCParser.Init failed to load file '+aFilename+': '+E.Message]);
        end;
      end;
    end;
  end;

var
  Item: TFPCMsgItem;
begin
  inherited Init;
  LoadMsgFile(MsgFilename,MsgFile);
  LoadMsgFile(TranslationFilename,TranslationFile);

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
}
var
  MsgLine: TMessageLine;
  MsgType: TMessageLineUrgency;
  p2: PChar;
  i: Integer;
  TranslatedItem: TFPCMsgItem;
  MsgItem: TFPCMsgItem;
  TranslatedMsg: String;
begin
  Result:=false;
  MsgType:=mluNone;
  if ReadString(p,'Fatal: ') then
    MsgType:=mluFatal
  else if ReadString(p,'Panic') then
    MsgType:=mluPanic
  else if ReadString(p,'Error: ') then
    MsgType:=mluError
  else if ReadString(p,'Warn: ') then
    MsgType:=mluWarn
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

      if (fMsgID=1018) // fatal: Compilation aborted
      then begin
        i:=Tool.WorkerMessages.Count-1;
        if (i>=0) and (Tool.WorkerMessages[i].Urgency>=MsgType) then begin
          // the last message already explains that the compilation aborted
          MsgType:=mluVerbose;
        end;
      end;
    end;
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
    MsgLine.Urgency:=mluWarn;
    MsgLine.Msg:=OldStart;
    AddMsgLine(MsgLine);
    exit(true);
  end;

  while p^ in ['0'..'9','a'..'z','A'..'Z','_'] do
    inc(p);
  if CompStr('.o(',p) then begin
    MsgLine:=CreateMsgLine;
    MsgLine.SubTool:=SubToolFPCLinker;
    MsgLine.Urgency:=mluWarn;
    MsgLine.Msg:=OldStart;
    AddMsgLine(MsgLine);
    exit(true);
  end;
  p := OldStart;
  if CompStr('ld: ',p) then begin
    Result:=true;
    MsgLine:=CreateMsgLine;
    MsgLine.SubTool:=SubToolFPCLinker;
    MsgLine.Urgency:=mluWarn;
    MsgLine.Msg:=OldStart;
    AddMsgLine(MsgLine);
    exit(true);
  end;
  if CompStr('Undefined symbols:', p) then begin
    Result:=true;
    MsgLine:=CreateMsgLine;
    MsgLine.SubTool:=SubToolFPCLinker;
    MsgLine.Urgency:=mluWarn;
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
  MsgLine.Urgency:=mluWarn;
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
  if FPCMsgFilePool<>nil then begin
    MsgFilename:=FPCMsgFilePool.DefaultEnglishFile;
    TranslationFilename:=FPCMsgFilePool.DefaultLocaleFile;
  end;
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
    MsgType:=mluWarn;
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
  p: PChar;
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

      // check for {%H-}
      if SourceOK and (not (mlfHiddenByIDEDirectiveValid in MsgLine.Flags)) then
      begin
        if (y<=fLastSource.LineCount) and (x-1<=fLastSource.GetLineLength(y-1))
        then begin
          p:=PChar(fLastSource.Source)+fLastSource.GetLineStart(y-1)+x-2;
          //debugln(['TFPCParser.ImproveMessages ',aFilename,' ',Y,',',X,' ',copy(fLastSource.GetLine(y-1),1,x-1),'|',copy(fLastSource.GetLine(y-1),x,100),' p=',p[0],p[1],p[2]]);
          if ((p^='{') and (p[1]='%') and (p[2]='H') and (p[3]='-'))
          or ((x>5) and (p[-5]='{') and (p[-4]='%') and (p[-3]='H') and (p[-2]='-') and (p[-1]='}'))
          then begin
            //debugln(['TFPCParser.ImproveMessages HIDDEN ',aFilename,' ',Y,',',X,' ',MsgLine.Msg]);
            MsgLine.Flags:=MsgLine.Flags+[mlfHiddenByIDEDirective,mlfHiddenByIDEDirectiveValid];
          end;
        end;
        MsgLine.Flags:=MsgLine.Flags+[mlfHiddenByIDEDirectiveValid];
      end;

      if (MsgLine.MsgID=5023) // Unit $1 not used
      and (MsgLine.Urgency>mluVerbose) then begin
        //debugln(['TIDEFPCParser.ImproveMessages ',aSynchronized,' ',MsgLine.Msg]);
        // unit not used
        if FilenameIsAbsolute(aFilename)
        and ((CompareFileExt(aFilename,'lpr',false)=0)
          or FileExists(ChangeFileExt(aFilename,'.lpk'),aSynchronized))
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

      // check for Sender not used
      if (MsgLine.MsgID=5024) // parameter $1 not used
      and (MsgLine.Urgency>mluVerbose)
      and (MsgLine.Msg='Parameter "Sender" not used') then begin
        // almost always not important
        MsgLine.Urgency:=mluVerbose;
      end;
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
    CurMsgFile:=FPCMsgFilePool.LoadFile(FPCMsgFilePool.DefaultEnglishFile,false);
    if CurMsgFile=nil then exit;
    try
      MsgItem:=CurMsgFile.GetMsg(MsgID);
      if MsgItem=nil then exit;
      Result:=MsgItem.GetTrimmedComment(false,true);
    finally
      FPCMsgFilePool.UnloadFile(CurMsgFile);
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
    CurMsgFile:=FPCMsgFilePool.LoadFile(FPCMsgFilePool.DefaultEnglishFile,false);
    if CurMsgFile=nil then exit;
    try
      MsgItem:=CurMsgFile.GetMsg(MsgID);
      if MsgItem=nil then exit;
      Result:=MsgItem.Pattern;
    finally
      FPCMsgFilePool.UnloadFile(CurMsgFile);
    end;
  end;
end;

class function TIDEFPCParser.Priority: integer;
begin
  Result:=SubToolFPCPriority;
end;

finalization
  FreeAndNil(FPCMsgFilePool)

end.

