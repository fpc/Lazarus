unit WikiHelpManager;

{$mode objfpc}{$H+}

{ $DEFINE VerboseWikiHelp}

interface

uses
  Classes, SysUtils, math, LazFileUtils, LazLogger, CodeToolsStructs,
  Wiki2HTMLConvert, Wiki2XHTMLConvert, WikiFormat, WikiParser, MTProcs;

type
  TWikiHelp = class;

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
    property ChildNodes[Index: integer]: TWHTextNode read GetChildNodes;
    property IndexInParent: integer read FIndexInParent;
    property Parent: TWHTextNode read FParent;
  end;

  { TW2HelpPage
    for future extensions and descendants }

  TW2HelpPage = class(TW2HTMLPage)
  public
    TextRoot: TWHTextNode;
    CurNode: TWHTextNode;
    destructor Destroy; override;
  end;

  { TWiki2HelpConverter }

  TWiki2HelpConverter = class(TWiki2HTMLConverter)
  protected
    PagesPerThread: integer;
    AvailableImages: TFilenameToStringTree; // existing files in the ImagesDirectory
    procedure SavePage({%H-}Page: TW2XHTMLPage); override;
    function FindImage(const ImgFilename: string): string; override;
    procedure ExtractPageText(Page: TW2HelpPage);
    procedure ExtractTextToken(Token: TWPToken);
    procedure ParallelExtractPageText(Index: PtrInt; {%H-}Data: Pointer; {%H-}Item: TMultiThreadProcItem);
  public
    constructor Create; override;
    procedure Clear; override;
    destructor Destroy; override;
    procedure ConvertInit; override;
    procedure ExtractAllTexts;
  end;

  { TWikiHelpThread }

  TWikiHelpThread = class(TThread)
  protected
    fLogMsg: string;
    procedure Execute; override;
    procedure MainThreadLog;
    procedure Log({%H-}Msg: string);
    procedure ConverterLog({%H-}Msg: string);
    procedure OnScanComplete; // called in thread at end
    procedure LoadWikiPage(Index: PtrInt; {%H-}Data: Pointer; {%H-}Item: TMultiThreadProcItem);
  public
    Help: TWikiHelp;
  end;

  { TWikiHelp }

  TWikiHelp = class(TComponent)
  private
    FAborting: boolean;
    FConverter: TWiki2HelpConverter;
    FScanning: boolean;
    FXMLDirectory: string;
    FCritSec: TRTLCriticalSection;
    FScanThread: TWikiHelpThread;
    function GetImagesDirectory: string;
    procedure SetImagesDirectory(AValue: string);
    procedure SetXMLDirectory(AValue: string);
    procedure EnterCritSect;
    procedure LeaveCritSect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartScan;
    procedure Abort;
    property Scanning: boolean read FScanning;
    property Aborting: boolean read FAborting;
    property XMLDirectory: string read FXMLDirectory write SetXMLDirectory; // directory where the wiki xml files are
    property ImagesDirectory: string read GetImagesDirectory write SetImagesDirectory; // directory where the wiki image files are
    property Converter: TWiki2HelpConverter read FConverter;
  end;

var
  WikiHelp: TWikiHelp = nil;

implementation

{ TW2HelpPage }

destructor TW2HelpPage.Destroy;
begin
  FreeAndNil(TextRoot);
  inherited Destroy;
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
begin
  Page:=TW2HelpPage(Token.UserData);
  W:=Page.WikiPage;
  case Token.Token of
  wptText:
    if Token is TWPTextToken then begin
      // ToDo
    end;
  wptSection:
    ; // ToDo
  wptHeader:
    ; // ToDo
  wptInternLink, wptExternLink:
    ; // ToDo
  else
    // add a space to separate words
    // ToDo
  end;
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
  for i:=StartIndex to EndIndex do
    ExtractPageText(TW2HelpPage(Pages[i]));
end;

procedure TWiki2HelpConverter.ExtractPageText(Page: TW2HelpPage);
begin
  FreeAndNil(Page.TextRoot);
  Page.TextRoot:=TWHTextNode.Create(whnTxt,nil);
  try
    Page.CurNode:=Page.TextRoot;
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
  ProcThreadPool.DoParallel(@ParallelExtractPageText,0,Count div PagesPerThread);
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
          if CompareFileExt(FileInfo.Name,'.xml',false)=0 then
            Files.Add(FileInfo.Name);
        until FindNextUTF8(FileInfo)<>0;
      end;
      FindCloseUTF8(FileInfo);
      if Help.Aborting then exit;

      // add file names to converter
      for i:=0 to Files.Count-1 do begin
        Filename:=Help.XMLDirectory+Files[i];
        Help.Converter.AddWikiPage(Filename,false);
      end;
      if Help.Aborting then exit;

      // load xml files
      ProcThreadPool.DoParallel(@LoadWikiPage,0,Help.Converter.Count-1);
      if Help.Aborting then exit;

      Help.Converter.ConvertInit;
      Help.Converter.ExtractAllTexts;
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
  Synchronize(@OnScanComplete);
end;

procedure TWikiHelpThread.MainThreadLog;
// called in main thread
begin
  DebugLn(fLogMsg);
end;

procedure TWikiHelpThread.Log(Msg: string);
begin
  fLogMsg:=Msg;
  Synchronize(@MainThreadLog);
end;

procedure TWikiHelpThread.ConverterLog(Msg: string);
begin
  {$IFDEF VerboseWikiHelp}
  Log(Msg);
  {$ENDIF}
end;

procedure TWikiHelpThread.OnScanComplete;
// called in this thread
begin
  Help.EnterCritSect;
  try
    Help.FScanThread:=nil;
    Help.FScanning:=false;
  finally
    Help.LeaveCritSect;
  end;
end;

procedure TWikiHelpThread.LoadWikiPage(Index: PtrInt; Data: Pointer;
  Item: TMultiThreadProcItem);
var
  Page: TW2HelpPage;
begin
  if Help.Aborting then exit;
  Page:=TW2HelpPage(Help.Converter.Pages[Index]);
  Page.ParseWikiDoc;
end;

{ TWikiHelp }

procedure TWikiHelp.SetImagesDirectory(AValue: string);
var
  NewDir: String;
begin
  NewDir:=TrimAndExpandDirectory(TrimFilename(AValue));
  if Converter.ImagesDir=NewDir then Exit;
  Converter.ImagesDir:=NewDir;
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

constructor TWikiHelp.Create(AOwner: TComponent);
begin
  InitCriticalSection(FCritSec);
  inherited Create(AOwner);
  FConverter:=TWiki2HelpConverter.Create;
  FConverter.LanguageTags:=WikiCreateCommonLanguageList(true);
end;

destructor TWikiHelp.Destroy;
begin
  Abort;
  FConverter.LanguageTags.Free;
  FreeAndNil(FConverter);
  inherited Destroy;
  DoneCriticalsection(FCritSec);
end;

procedure TWikiHelp.StartScan;
begin
  if not DirPathExists(XMLDirectory) then
    raise Exception.Create('TWikiHelp.StartScan XMLDirectory not found: '+XMLDirectory);
  if not DirPathExists(ImagesDirectory) then
    raise Exception.Create('TWikiHelp.StartScan ImagesDirectory not found: '+ImagesDirectory);
  EnterCritSect;
  try
    if Scanning then exit;
    FScanning:=true;
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

procedure TWikiHelp.Abort;
begin
  FAborting:=true;
  while Scanning do
    Sleep(10);
  FAborting:=false;
end;

end.

