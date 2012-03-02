unit WikiHelpManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazLogger, CodeToolsStructs,
  Wiki2HTMLConvert, Wiki2XHTMLConvert, WikiFormat, MTProcs;

type
  TWikiHelp = class;

  { TW2HelpPage
    for future extensions and descendants }

  TW2HelpPage = class(TW2HTMLPage)
  public

  end;

  { TWiki2HelpConverter }

  TWiki2HelpConverter = class(TWiki2HTMLConverter)
  protected
    AvailableImages: TFilenameToStringTree; // existing files in the ImagesDirectory
    procedure SavePage({%H-}Page: TW2XHTMLPage); override;
    function FindImage(const ImgFilename: string): string; override;
    procedure ConvertInit; override;
  public
    constructor Create; override;
    procedure Clear; override;
    destructor Destroy; override;
  end;

  { TWikiHelpThread }

  TWikiHelpThread = class(TThread)
  protected
    fLogMsg: string;
    procedure Execute; override;
    procedure MainThreadLog;
    procedure Log(Msg: string);
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

constructor TWiki2HelpConverter.Create;
begin
  inherited Create;
  AvailableImages:=TFilenameToStringTree.Create(true);
  fPageClass:=TW2HelpPage;
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
begin
  Files:=nil;
  try
    Log('TWikiHelpThread.Execute START XMLDirectory="'+Help.XMLDirectory+'"');

    Files:=TStringList.Create;
    try
      Help.Converter.OnLog:=@Log;
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

      Help.Converter.Convert;
    finally
      Files.Free;
      Help.Converter.OnLog:=nil;
    end;
    Log('TWikiHelpThread.Execute SCAN complete XMLDirectory="'+Help.XMLDirectory+'"');
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

