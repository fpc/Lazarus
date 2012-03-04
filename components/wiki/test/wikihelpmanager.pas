{ Search engine for wiki pages

  Copyright (C) 2012  Mattias Gaertner  mattias@freepascal.org

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.

}
unit WikiHelpManager;

{$mode objfpc}{$H+}

{ $DEFINE VerboseWikiHelp}
{$DEFINE TestWikiSearch}

interface

uses
  Classes, SysUtils, math, LazFileUtils, LazLogger, LazDbgLog, LazUTF8,
  CodeToolsStructs, BasicCodeTools, KeywordFuncLists, Wiki2HTMLConvert,
  Wiki2XHTMLConvert, WikiFormat, WikiParser, MTProcs;

type
  TWikiHelp = class;

  { TWikiHelpQuery }

  TWikiHelpQuery = class
  public
    Phrases: TStrings;
    LoPhrases: TStrings; // Phrases lowercase
    Languages: string; // comma separated list, '-' means not in the original, 'de' = german
    constructor Create(const SearchText: string; const aLang: string = '');
    destructor Destroy; override;
    function Equals(Obj: TObject): boolean; override;
  end;

  TWHTextNodeType = (
    whnTxt,
    whnHeader,
    whnLink
    );

  { TWHTextNode }

  TWHTextNode = class
  private
    FChildNodes: TFPList; // list of TW2HelpTextNode
    FIndexInParent: integer;
    FParent: TWHTextNode;
    function GetChildNodes(Index: integer): TWHTextNode;
    procedure RemoveChild(Child: TWHTextNode);
  public
    Typ: TWHTextNodeType;
    Txt: string;
    constructor Create(aTyp: TWHTextNodeType; aParent: TWHTextNode);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Node: TWHTextNode);
    function Count: integer;
    property ChildNodes[Index: integer]: TWHTextNode read GetChildNodes; default;
    property IndexInParent: integer read FIndexInParent;
    property Parent: TWHTextNode read FParent;
    function CalcMemSize: SizeInt;
  end;

  TWHFitsCategory = (
    whfcNone,
    whfcLink,
    whfcText,
    whfcHeader,
    whfcPageTitle
    );
  TWHFitsCategories = set of TWHFitsCategory;

  TWHFitsStringFlag = (
    whfsPart,
    whfsWholeWord
    );
  TWHFitsStringFlags = set of TWHFitsStringFlag;

  TWHPhrasePageFit = record
    Category: TWHFitsCategory;
    Quality: TWHFitsStringFlag;
  end;
  PWHPhrasePageFit = ^TWHPhrasePageFit;

  TWHScore = single;
  TWHScoring = class
  public
    Phrases: array[TWHFitsCategory,TWHFitsStringFlag] of TWHScore;
  end;

  { TW2HelpPage }

  TW2HelpPage = class(TW2HTMLPage)
  public
    TextRoot: TWHTextNode;
    CurNode: TWHTextNode;
    Score: single;
    destructor Destroy; override;
    function GetScore(Query: TWikiHelpQuery; Scoring: TWHScoring): TWHScore;
    procedure GetFit(Query: TWikiHelpQuery; Fit: PWHPhrasePageFit);
  end;

  { TWiki2HelpConverter }

  TWiki2HelpConverter = class(TWiki2HTMLConverter)
  private
    FHelp: TWikiHelp;
  protected
    PagesPerThread: integer;
    AvailableImages: TFilenameToStringTree; // existing files in the ImagesDirectory
    procedure SavePage({%H-}Page: TW2XHTMLPage); override;
    function FindImage(const ImgFilename: string): string; override;
    procedure ExtractPageText(Page: TW2HelpPage);
    procedure ExtractTextToken(Token: TWPToken);
    procedure ParallelExtractPageText(Index: PtrInt; {%H-}Data: Pointer; {%H-}Item: TMultiThreadProcItem);
    procedure ParallelLoadPage(Index: PtrInt; {%H-}Data: Pointer; {%H-}Item: TMultiThreadProcItem);
  public
    constructor Create; override;
    procedure Clear; override;
    destructor Destroy; override;
    procedure LoadPages;
    procedure ConvertInit; override;
    procedure ExtractAllTexts;
    procedure Search(Query: TWikiHelpQuery; Scoring: TWHScoring);
    property Help: TWikiHelp read FHelp;
  end;

  { TWikiHelpThread }

  TWikiHelpThread = class(TThread)
  protected
    fLogMsg: string;
    fCompleted: boolean;
    procedure Execute; override;
    procedure MainThreadLog;
    procedure Log({%H-}Msg: string);
    procedure ConverterLog({%H-}Msg: string);
    procedure Scanned; // called in thread at end
  public
    Help: TWikiHelp;
  end;

  { TWikiHelp }

  TWikiHelp = class(TComponent)
  private
    FAbortingLoad: boolean;
    FConverter: TWiki2HelpConverter;
    FOnScanned: TNotifyEvent;
    FQuery: TWikiHelpQuery;
    FLoadComplete: boolean;
    FLoading: boolean;
    FScoring: TWHScoring;
    FXMLDirectory: string;
    FCritSec: TRTLCriticalSection;
    FScanThread: TWikiHelpThread;
    function GetImagesDirectory: string;
    procedure SetImagesDirectory(AValue: string);
    procedure SetQuery(AValue: TWikiHelpQuery);
    procedure SetXMLDirectory(AValue: string);
    procedure EnterCritSect;
    procedure LeaveCritSect;
    procedure Scanned;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // load wiki files
    procedure StartLoading; // returns immediately
    property Loading: boolean read FLoading;
    procedure AbortLoading(Wait: boolean);
    property AbortingLoad: boolean read FAbortingLoad;
    property LoadComplete: boolean read FLoadComplete;

    // search
    procedure Search(const Term: string; const Languages: string = '');
    procedure Search(aQuery: TWikiHelpQuery);
    procedure TestSearch;
    property Query: TWikiHelpQuery read FQuery;
    property Scoring: TWHScoring read FScoring;
  public
    property XMLDirectory: string read FXMLDirectory write SetXMLDirectory; // directory where the wiki xml files are
    property ImagesDirectory: string read GetImagesDirectory write SetImagesDirectory; // directory where the wiki image files are
    property Converter: TWiki2HelpConverter read FConverter;
    property OnScanned: TNotifyEvent read FOnScanned write FOnScanned;
  end;

var
  WikiHelp: TWikiHelp = nil;

function SearchTextToPhrases(Txt: string): TStringList;

implementation

function SearchTextToPhrases(Txt: string): TStringList;
var
  p: PChar;
  StartPos: PChar;
  Phrase: String;
begin
  Result:=TStringList.Create;
  Txt:=UTF8Trim(Txt);
  if Txt='' then exit;
  Result.Add(Txt);
  p:=PChar(Txt);
  Phrase:='';
  while p^<>#0 do begin
    if p^='"' then begin
      // quote
      inc(p);
      StartPos:=p;
      while not (p^ in [#0,'"']) do inc(p);
      Phrase:=Phrase+SubString(StartPos,p-StartPos);
      if p^<>#0 then inc(p);
    end else if p^ in [' ',#9,#10,#13] then begin
      // space => end phrase
      inc(p);
      if Phrase<>'' then begin
        if Result.IndexOf(Phrase)<0 then
          Result.Add(Phrase);
        Phrase:='';
      end;
    end else begin
      // word
      StartPos:=p;
      while not (p^ in [#0,'"',' ',#9,#10,#13]) do inc(p);
      Phrase:=Phrase+SubString(StartPos,p-StartPos);
    end;
  end;
  if (Phrase<>'') and (Result.IndexOf(Phrase)<0) then
    Result.Add(Phrase);
end;

{ TWikiHelpQuery }

constructor TWikiHelpQuery.Create(const SearchText: string; const aLang: string
  );
var
  i: Integer;
begin
  Languages:=aLang;
  Phrases:=SearchTextToPhrases(SearchText);
  LoPhrases:=TStringList.Create;
  for i:=0 to Phrases.Count-1 do
    LoPhrases.Add(UTF8LowerCase(Phrases[i]));
end;

destructor TWikiHelpQuery.Destroy;
begin
  FreeAndNil(Phrases);
  inherited Destroy;
end;

function TWikiHelpQuery.Equals(Obj: TObject): boolean;
var
  Src: TWikiHelpQuery;
begin
  Result:=inherited Equals(Obj);
  if Result then exit;
  Result:=false;
  if not (Obj is TWikiHelpQuery) then exit;
  Src:=TWikiHelpQuery(Obj);
  if not Phrases.Equals(Src.Phrases) then exit;
  // LoPhrases is computed from Phrases
end;

{ TW2HelpPage }

destructor TW2HelpPage.Destroy;
begin
  FreeAndNil(TextRoot);
  inherited Destroy;
end;

function TW2HelpPage.GetScore(Query: TWikiHelpQuery; Scoring: TWHScoring
  ): TWHScore;
var
  PhrasesFit: PWHPhrasePageFit;
  Size: Integer;
  i: Integer;
  Fit: PWHPhrasePageFit;
begin
  Result:=0;
  if (Query=nil) or (Query.LoPhrases.Count=0) then exit;
  if not WikiPageHasLanguage(WikiDocumentName,Query.Languages) then begin
    //debugln(['TW2HelpPage.GetScore lang does not fit ',WikiDocumentName,' "',GetWikiPageLanguage(WikiDocumentName),'" ',Query.Languages]);
    exit;
  end;

  Size:=Query.LoPhrases.Count*SizeOf(TWHPhrasePageFit);
  GetMem(PhrasesFit,Size);
  try
    FillByte(PhrasesFit^,Size,0);
    GetFit(Query,PhrasesFit);
    for i:=0 to Query.LoPhrases.Count-1 do begin
      Fit:=@PhrasesFit[i];
      Result+=Scoring.Phrases[Fit^.Category,Fit^.Quality];
    end;
  finally
    FreeMem(PhrasesFit);
  end;
end;

procedure TW2HelpPage.GetFit(Query: TWikiHelpQuery; Fit: PWHPhrasePageFit);

  procedure CheckTxt(s: string; Category: TWHFitsCategory);
  var
    i: Integer;
    Phrase: String;
    FitsWholeWord: boolean;
    FitsCount: SizeInt;
    Quality: TWHFitsStringFlag;
  begin
    s:=UTF8LowerCase(s);
    for i:=0 to Query.LoPhrases.Count-1 do begin
      if Fit[i].Category>Category then continue;
      if (Fit[i].Category=Category) and (Fit[i].Quality>=whfsWholeWord) then
        continue;
      Phrase:=Query.LoPhrases[i];
      HasTxtWord(PChar(Phrase),PChar(s),FitsWholeWord,FitsCount);
      if FitsCount<=0 then continue;
      if FitsWholeWord then
        Quality:=whfsWholeWord
      else
        Quality:=whfsPart;
      Fit[i].Category:=Category;
      Fit[i].Quality:=Quality;
    end;
  end;

  procedure Traverse(Node: TWHTextNode);
  var
    i: Integer;
    Category: TWHFitsCategory;
  begin
    if Node=nil then exit;
    case Node.Typ of
    whnTxt: Category:=whfcText;
    whnHeader: Category:=whfcHeader;
    whnLink: Category:=whfcLink;
    else exit;
    end;
    CheckTxt(Node.Txt,Category);
    for i:=0 to Node.Count-1 do
      Traverse(Node[i]);
  end;

begin
  CheckTxt(WikiPage.Title,whfcPageTitle);
  Traverse(TextRoot);
end;

{ TWHTextNode }

function TWHTextNode.GetChildNodes(Index: integer): TWHTextNode;
begin
  Result:=TWHTextNode(FChildNodes[Index]);
end;

procedure TWHTextNode.RemoveChild(Child: TWHTextNode);
var
  i: Integer;
begin
  FChildNodes.Delete(Child.IndexInParent);
  for i:=Child.IndexInParent to FChildNodes.Count-1 do
    ChildNodes[i].fIndexInParent:=i;
end;

constructor TWHTextNode.Create(aTyp: TWHTextNodeType; aParent: TWHTextNode);
begin
  Typ:=aTyp;
  if aParent<>nil then
    aParent.Add(Self)
  else
    fIndexInParent:=-1;
end;

destructor TWHTextNode.Destroy;
begin
  Clear;
  if Parent<>nil then
    Parent.RemoveChild(Self);
  inherited Destroy;
end;

procedure TWHTextNode.Clear;
var
  i: Integer;
  Child: TWHTextNode;
begin
  Txt:='';
  if FChildNodes<>nil then begin
    for i:=FChildNodes.Count-1 downto 0 do begin
      Child:=TWHTextNode(FChildNodes[i]);
      Child.fParent:=nil;
      Child.Free;
    end;
    FChildNodes.Clear;
  end;
end;

procedure TWHTextNode.Add(Node: TWHTextNode);
begin
  if Node.Parent=Self then exit;
  if Node.Parent<>nil then
    Node.Parent.RemoveChild(Node);
  if FChildNodes=nil then
    FChildNodes:=TFPList.Create;
  Node.fIndexInParent:=Count;
  FChildNodes.Add(Node);
  Node.fParent:=Self;
end;

function TWHTextNode.Count: integer;
begin
  if FChildNodes<>nil then
    Result:=FChildNodes.Count
  else
    Result:=0;
end;

function TWHTextNode.CalcMemSize: SizeInt;
var
  i: Integer;
begin
  Result:=InstanceSize+SizeInt(MemSizeString(Txt));
  if FChildNodes<>nil then begin
    inc(Result,FChildNodes.InstanceSize+FChildNodes.Count*SizeOf(Pointer));
    for i:=0 to Count-1 do
      inc(Result,ChildNodes[i].CalcMemSize);
  end;
end;

{ TWiki2HelpConverter }

procedure TWiki2HelpConverter.SavePage(Page: TW2XHTMLPage);
begin
  // do not save
end;

function TWiki2HelpConverter.FindImage(const ImgFilename: string): string;
begin
  //Log('AvailableImages='+dbgs(AvailableImages.Tree.Count)+' Img="'+ImgFilename+'"');
  if AvailableImages.Contains(ImgFilename) then
    Result:=ImgFilename
  else
    Result:='';
end;

procedure TWiki2HelpConverter.ExtractTextToken(Token: TWPToken);
var
  Page: TW2HelpPage;
  W: TWikiPage;
  Txt: String;
  CurNode: TWHTextNode;
  StartP, EndP: PChar;
  NodeType: TWHTextNodeType;
  TextToken: TWPTextToken;
  LinkToken: TWPLinkToken;
  Caption: String;
begin
  Page:=TW2HelpPage(Token.UserData);
  W:=Page.WikiPage;
  CurNode:=Page.CurNode;
  if CurNode=nil then CurNode:=Page.TextRoot;
  case Token.Token of
  wptText:
    if Token is TWPTextToken then begin
      TextToken:=TWPTextToken(Token);
      StartP:=PChar(W.Src)+TextToken.StartPos-1;
      EndP:=PChar(W.Src)+TextToken.EndPos-1;
      while (StartP<EndP) and (StartP^ in [#1..#31,' ']) do inc(StartP);
      if StartP<EndP then begin
        // not only space
        Txt:=copy(W.Src,TextToken.StartPos,TextToken.EndPos-TextToken.StartPos);
        CurNode.Txt:=CurNode.Txt+Txt;
        exit;
      end;
    end;

  wptSection,wptHeader:
    if Token.Range=wprOpen then begin
      if Token.Token=wptHeader then
        NodeType:=whnHeader
      else
        NodeType:=whnTxt;
      Page.CurNode:=TWHTextNode.Create(NodeType,CurNode);
      exit;
    end else if Token.Range=wprClose then begin
      Page.CurNode:=CurNode.Parent;
      exit;
    end;

  wptInternLink, wptExternLink:
    if Token is TWPLinkToken then begin
      LinkToken:=TWPLinkToken(Token);
      Caption:=copy(W.Src,LinkToken.CaptionStartPos,
                    LinkToken.CaptionEndPos-LinkToken.CaptionStartPos);
      if Caption<>'' then begin
        CurNode:=TWHTextNode.Create(whnLink,CurNode);
        CurNode.Txt:=Caption;
        // do not exit, append a space to the current node
      end;
    end;
  end;
  // add a space to separate words
  if (CurNode.Txt='') or (not (CurNode.Txt[length(CurNode.Txt)] in [#1..#31,' ']))
  then
    CurNode.Txt:=CurNode.Txt+' ';
end;

procedure TWiki2HelpConverter.ParallelExtractPageText(Index: PtrInt;
  Data: Pointer; Item: TMultiThreadProcItem);
var
  StartIndex: Integer;
  EndIndex: Integer;
  i: Integer;
begin
  StartIndex:=Index*PagesPerThread;
  EndIndex:=Min(StartIndex+PagesPerThread-1,Count-1);
  if Help.AbortingLoad then exit;
  for i:=StartIndex to EndIndex do
    ExtractPageText(TW2HelpPage(Pages[i]));
end;

procedure TWiki2HelpConverter.ParallelLoadPage(Index: PtrInt; Data: Pointer;
  Item: TMultiThreadProcItem);
var
  Page: TW2HelpPage;
  StartIndex: Integer;
  EndIndex: integer;
  i: Integer;
begin
  StartIndex:=Index*PagesPerThread;
  EndIndex:=Min(StartIndex+PagesPerThread-1,Count-1);
  for i:=StartIndex to EndIndex do begin
    if Help.AbortingLoad then exit;
    Page:=TW2HelpPage(Pages[i]);
    try
      Page.ParseWikiDoc(false);
    except
      on E: Exception do begin
        Log('ERROR: '+Page.WikiFilename+': '+E.Message);
      end;
    end;
  end;
end;

procedure TWiki2HelpConverter.ExtractPageText(Page: TW2HelpPage);
begin
  FreeAndNil(Page.TextRoot);
  Page.TextRoot:=TWHTextNode.Create(whnTxt,nil);
  try
    Page.CurNode:=Page.TextRoot;
    if Page.WikiPage<>nil then
      Page.WikiPage.Parse(@ExtractTextToken,Page);
  finally
    Page.CurNode:=nil;
  end;
end;

procedure TWiki2HelpConverter.ConvertInit;
var
  FileInfo: TSearchRec;
begin
  inherited ConvertInit;

  //Log('ImagesDir='+ImagesDir);
  AvailableImages.Clear;
  if FindFirstUTF8(ImagesDir+AllFilesMask,faAnyFile,FileInfo)=0 then begin
    repeat
      if (FileInfo.Name='') or (FileInfo.Name='.') or (FileInfo.Name='..') then
        continue;
      AvailableImages[FileInfo.Name]:='1';
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
  Log('Found '+IntToStr(AvailableImages.Tree.Count)+' wiki images in "'+ImagesDir+'"');
end;

procedure TWiki2HelpConverter.ExtractAllTexts;
begin
  ProcThreadPool.DoParallel(@ParallelExtractPageText,0,(Count-1) div PagesPerThread);
end;

procedure TWiki2HelpConverter.Search(Query: TWikiHelpQuery; Scoring: TWHScoring
  );
var
  i: Integer;
  Page: TW2HelpPage;
begin
  for i:=0 to Count-1 do begin
    Page:=TW2HelpPage(Pages[i]);
    Page.GetScore(Query,Scoring);
  end;
end;

procedure TWiki2HelpConverter.LoadPages;
begin
  ProcThreadPool.DoParallel(@ParallelLoadPage,0,(Count-1) div PagesPerThread);
end;

constructor TWiki2HelpConverter.Create;
begin
  inherited Create;
  AvailableImages:=TFilenameToStringTree.Create(true);
  fPageClass:=TW2HelpPage;
  PagesPerThread:=100;
end;

procedure TWiki2HelpConverter.Clear;
begin
  inherited Clear;
  AvailableImages.Clear;
end;

destructor TWiki2HelpConverter.Destroy;
begin
  inherited Destroy;
  FreeAndNil(AvailableImages);
end;

{ TWikiHelpThread }

procedure TWikiHelpThread.Execute;
var
  FileInfo: TSearchRec;
  Files: TStringList;
  i: Integer;
  Filename: String;
  StartTime: TDateTime;
  EndTime: TDateTime;
begin
  CurrentThread:=Self;
  try
    Files:=nil;
    try
      StartTime:=Now;
      Log('TWikiHelpThread.Execute START XMLDirectory="'+Help.XMLDirectory+'"');

      Files:=TStringList.Create;
      try
        Help.Converter.OnLog:=@ConverterLog;
        // get all wiki xml files
        if FindFirstUTF8(Help.XMLDirectory+AllFilesMask,faAnyFile,FileInfo)=0 then begin
          repeat
            if CompareFileExt(FileInfo.Name,'.xml',false)<>0 then continue;
            {$IFDEF TestWikiSearch}
            //if FileInfo.Name[1]<>'L' then continue;
            {$ENDIF}
            Files.Add(FileInfo.Name);
          until FindNextUTF8(FileInfo)<>0;
        end;
        FindCloseUTF8(FileInfo);

        // add file names to converter
        for i:=0 to Files.Count-1 do begin
          Filename:=Help.XMLDirectory+Files[i];
          Help.Converter.AddWikiPage(Filename,false);
        end;
        if Help.AbortingLoad then exit;

        // load xml files
        Help.Converter.LoadPages;
        if Help.AbortingLoad then exit;

        // extract texts
        Help.Converter.ConvertInit;
        if Help.AbortingLoad then exit;
        Help.Converter.ExtractAllTexts;
        if Help.AbortingLoad then exit;

        fCompleted:=true;
        EndTime:=Now;
        Log('TWikiHelpThread.Execute SCAN complete XMLDirectory="'+Help.XMLDirectory+'" '+dbgs(round(Abs(EndTime-StartTime)*86400000))+'msec');
      finally
        Files.Free;
        Help.Converter.OnLog:=nil;
      end;
    except
      on E: Exception do begin
        Log('TWikiHelpThread.Execute error: '+E.Message);
      end;
    end;
  finally
    Scanned;
    CurrentThread:=nil;
  end;
end;

procedure TWikiHelpThread.MainThreadLog;
// called in main thread
begin
  DebugLn(fLogMsg);
end;

procedure TWikiHelpThread.Log(Msg: string);
begin
  fLogMsg:=Msg;
  CurrentThread.Synchronize(@MainThreadLog);
end;

procedure TWikiHelpThread.ConverterLog(Msg: string);
begin
  {$IFDEF VerboseWikiHelp}
  Log(Msg);
  {$ENDIF}
end;

procedure TWikiHelpThread.Scanned;
// called in this thread
begin
  Help.EnterCritSect;
  try
    Help.FScanThread:=nil;
    Help.FLoading:=false;
    Help.FLoadComplete:=fCompleted;
    Help.FAbortingLoad:=false;
  finally
    Help.LeaveCritSect;
  end;
  Synchronize(@Help.Scanned);
end;

{ TWikiHelp }

procedure TWikiHelp.SetImagesDirectory(AValue: string);
var
  NewDir: String;
begin
  NewDir:=TrimAndExpandDirectory(TrimFilename(AValue));
  if Converter.ImagesDir=NewDir then Exit;
  AbortLoading(true);
  Converter.ImagesDir:=NewDir;
end;

procedure TWikiHelp.SetQuery(AValue: TWikiHelpQuery);
begin
  if FQuery=AValue then Exit;
  FQuery:=AValue;
end;

function TWikiHelp.GetImagesDirectory: string;
begin
  Result:=Converter.ImagesDir;
end;

procedure TWikiHelp.SetXMLDirectory(AValue: string);
var
  NewDir: String;
begin
  NewDir:=TrimAndExpandDirectory(TrimFilename(AValue));
  if FXMLDirectory=NewDir then Exit;
  AbortLoading(true);
  FXMLDirectory:=NewDir;
end;

procedure TWikiHelp.EnterCritSect;
begin
  EnterCriticalsection(FCritSec);
end;

procedure TWikiHelp.LeaveCritSect;
begin
  LeaveCriticalsection(FCritSec);
end;

procedure TWikiHelp.Scanned;
begin
  if Assigned(OnScanned) then
    OnScanned(Self);

  {$IFDEF TestWikiSearch}
  Search('documentation');
  {$ENDIF}
end;

constructor TWikiHelp.Create(AOwner: TComponent);
begin
  InitCriticalSection(FCritSec);
  inherited Create(AOwner);
  FConverter:=TWiki2HelpConverter.Create;
  FConverter.LanguageTags:=WikiCreateCommonLanguageList(true);
  FConverter.FHelp:=Self;
  FScoring:=TWHScoring.Create;
  FScoring.Phrases[whfcPageTitle,whfsWholeWord]:=128;
  FScoring.Phrases[whfcPageTitle,whfsPart]:=64;
  FScoring.Phrases[whfcHeader,whfsWholeWord]:=32;
  FScoring.Phrases[whfcHeader,whfsPart]:=16;
  FScoring.Phrases[whfcText,whfsWholeWord]:=8;
  FScoring.Phrases[whfcText,whfsPart]:=4;
  FScoring.Phrases[whfcLink,whfsWholeWord]:=2;
  FScoring.Phrases[whfcLink,whfsPart]:=1;
end;

destructor TWikiHelp.Destroy;
begin
  AbortLoading(true);
  FConverter.LanguageTags.Free;
  FreeAndNil(FConverter);
  FreeAndNil(FScoring);
  FreeAndNil(FQuery);
  inherited Destroy;
  DoneCriticalsection(FCritSec);
end;

procedure TWikiHelp.StartLoading;
begin
  if not DirPathExists(XMLDirectory) then
    raise Exception.Create('TWikiHelp.StartScan XMLDirectory not found: '+XMLDirectory);
  if not DirPathExists(ImagesDirectory) then
    raise Exception.Create('TWikiHelp.StartScan ImagesDirectory not found: '+ImagesDirectory);
  EnterCritSect;
  try
    if Loading then exit;
    FLoading:=true;
    FLoadComplete:=false;
    FScanThread:=TWikiHelpThread.Create(true);
    FScanThread.FreeOnTerminate:=true;
    FScanThread.Help:=Self;
    {$IF FPC_FULLVERSION<=20403}
    FScanThread.Resume;
    {$ELSE}
    FScanThread.Start;
    {$ENDIF}
  finally
    LeaveCritSect;
  end;
end;

procedure TWikiHelp.AbortLoading(Wait: boolean);
begin
  EnterCritSect;
  try
    if not Loading then exit;
    FAbortingLoad:=true;
  finally
    LeaveCritSect;
  end;
  if not Wait then exit;
  while Loading do
    Sleep(10);
  EnterCritSect;
  try
    FAbortingLoad:=false;
  finally
    LeaveCritSect;
  end;
end;

procedure TWikiHelp.Search(const Term: string; const Languages: string);
begin
  Search(TWikiHelpQuery.Create(Term,Languages));
end;

procedure TWikiHelp.Search(aQuery: TWikiHelpQuery);
begin
  EnterCritSect;
  try
    if (aQuery<>nil) and (FQuery<>nil) and (FQuery.Equals(aQuery)) then begin
      // same query
      FreeAndNil(aQuery);
      exit;
    end;
    FreeAndNil(FQuery);
    FQuery:=aQuery;
  finally
    LeaveCritSect;
  end;
  TestSearch;
end;

procedure TWikiHelp.TestSearch;
var
  StartTime: TDateTime;
  EndTime: TDateTime;
begin
  StartTime:=Now;
  debugln(['TWikiHelp.TestSearch START ',Query.Phrases.Text]);
  Converter.Search(Query,Scoring);
  EndTime:=Now;
  debugln(['TWikiHelp.TestSearch END "',Query.Phrases.Text,'" ',dbgs(round(Abs(EndTime-StartTime)*86400000))+'msec']);
end;

end.

