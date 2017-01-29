{ Browse and search form of offline wiki

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
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.

}
unit WikiSearchMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, FileUtil, LazLogger, LazUTF8, LazFileUtils, laz2_DOM,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, LCLIntf,
  Menus, IpHtml, Ipfilebroker, IpMsg, CodeToolManager, CodeCache, SynEdit,
  SynHighlighterHTML, SynEditHighlighter, WikiHelpManager, WikiSearchOptions,
  WikiParser, WikiFormat;

type

  { TWikiIpHtmlDataProvider }

  TWikiIpHtmlDataProvider = class(TIpHtmlDataProvider)
  private
  protected
    function DoGetStream(const URL: string): TStream; override;
  public
  end;

  { TWikiPageHistoryItem }

  TWikiPageHistoryItem = class
  public
    DocumentName: string;
    Anchor: string;
    Title: string;
    constructor Create(TheDocumentName, TheAnchor, TheTitle: string);
  end;

  TWikiPageHistory = class
  private
    FCurrentIndex: integer;
    FItems: TFPList; // list of TWikiPageHistoryItem
    FMaxCount: integer;
    function GetCurrent: TWikiPageHistoryItem;
    function GetItems(Index: integer): TWikiPageHistoryItem;
    procedure SetCurrentIndex(AValue: integer);
    procedure SetMaxCount(AValue: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property CurrentIndex: integer read FCurrentIndex write SetCurrentIndex;
    property Current: TWikiPageHistoryItem read GetCurrent;
    property Items[Index: integer]: TWikiPageHistoryItem read GetItems; default;
    function Count: integer;
    property MaxCount: integer read FMaxCount write SetMaxCount;
    function AddAfterCurrent(DocumentName, Anchor, Title: string): TWikiPageHistoryItem;
    procedure Delete(Index: integer);
  end;

  { TWikiSearchDemoForm }

  TWikiSearchDemoForm = class(TForm)
    HideSearchButton: TButton;
    ImageList1: TImageList;
    ViewWikiSourceMenuItem: TMenuItem;
    ViewHTMLMenuItem: TMenuItem;
    OptionsButton: TButton;
    PagePanel: TPanel;
    PageIpHtmlPanel: TIpHtmlPanel;
    PagePopupMenu: TPopupMenu;
    PageToolBar: TToolBar;
    ProgressLabel: TLabel;
    ResultsIpHtmlPanel: TIpHtmlPanel;
    SearchEdit: TEdit;
    SearchPanel: TPanel;
    SearchLabel: TLabel;
    Splitter1: TSplitter;
    SynHTMLSyn1: TSynHTMLSyn;
    Timer1: TTimer;
    ShowSearchToolButton: TToolButton;
    SearchSepToolButton: TToolButton;
    BackToolButton: TToolButton;
    ForwardToolButton: TToolButton;
    procedure BackToolButtonClick(Sender: TObject);
    function DataProviderCanHandle(Sender: TObject; const {%H-}URL: string): Boolean;
    procedure DataProviderGetImage(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ForwardToolButtonClick(Sender: TObject);
    procedure HideSearchButtonClick(Sender: TObject);
    procedure LanguagesEditChange(Sender: TObject);
    procedure IpHtmlPanelHotClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure PagePopupMenuPopup(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure ViewHTMLMenuItemClick(Sender: TObject);
    procedure ShowSearchToolButtonClick(Sender: TObject);
    procedure ViewWikiSourceMenuItemClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fLastSearchText: string;
    fLastLanguages: string;
    fLastScoring: TWHScoring;
    FIdleConnected: boolean;
    FPageHistory: TWikiPageHistory;
    FURLDataProvider: TWikiIpHtmlDataProvider;
    FPageDocumentName: string;
    FPageAnchor: string;
    FPageSource: string;
    FPageTitle: string;
    procedure GoToHistoryPage(Index: integer);
    procedure QueryChanged;
    procedure SetIdleConnected(AValue: boolean);
    procedure UpdateHistoryButtons;
    procedure UpdateProgress;
    procedure LoadWikiPage(Documentname, Anchor: string; AddToHistory: boolean);
    procedure LoadHTML(Target: TIpHtmlPanel; HTML: string; Anchor: string = ''); overload;
    procedure LoadHTML(Target: TIpHtmlPanel; aStream: TStream; Anchor: string = ''); overload;
    procedure ViewSource(aTitle, aSource: string; aHighlighter: TSynCustomHighlighter);
    procedure WikiSearchOptsWndOptionsChanged(Sender: TObject);
    procedure WikiHelpScanned(Sender: TObject);
    procedure WikiHelpSearched(Sender: TObject);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
  public
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
    procedure ShowOptions;
    function GetLanguages: string;
    property PageHistory: TWikiPageHistory read FPageHistory;
  end;

var
  WikiSearchDemoForm: TWikiSearchDemoForm = nil;

implementation

function TWikiPageHistory.GetItems(Index: integer): TWikiPageHistoryItem;
begin
  Result:=TWikiPageHistoryItem(FItems[Index]);
end;

function TWikiPageHistory.GetCurrent: TWikiPageHistoryItem;
begin
  if (CurrentIndex>=0) and (CurrentIndex<Count) then
    Result:=Items[CurrentIndex]
  else
    Result:=nil;
end;

procedure TWikiPageHistory.SetCurrentIndex(AValue: integer);
var
  NewValue: Integer;
begin
  NewValue:=AValue;
  if NewValue<-1 then NewValue:=-1;
  if NewValue>=Count then NewValue:=Count-1;
  if FCurrentIndex=NewValue then Exit;
  FCurrentIndex:=NewValue;
end;

procedure TWikiPageHistory.SetMaxCount(AValue: integer);
begin
  if FMaxCount=AValue then Exit;
  FMaxCount:=AValue;
end;

constructor TWikiPageHistory.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TWikiPageHistory.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TWikiPageHistory.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do
    TObject(FItems[i]).Free;
  FItems.Clear;
  FCurrentIndex:=-1;
end;

function TWikiPageHistory.Count: integer;
begin
  Result:=FItems.Count;
end;

function TWikiPageHistory.AddAfterCurrent(DocumentName, Anchor, Title: string
  ): TWikiPageHistoryItem;
begin
  while Count>CurrentIndex+1 do
    Delete(Count-1);
  Result:=TWikiPageHistoryItem.Create(DocumentName,Anchor,Title);
  FItems.Add(Result);
  FCurrentIndex:=FItems.Count-1;
end;

procedure TWikiPageHistory.Delete(Index: integer);
begin
  TObject(FItems[Index]).Free;
  FItems.Delete(Index);
end;

{ TWikiPageHistoryItem }

constructor TWikiPageHistoryItem.Create(TheDocumentName, TheAnchor,
  TheTitle: string);
begin
  DocumentName:=TheDocumentName;
  Anchor:=TheAnchor;
  Title:=TheTitle;
end;

{ TWikiIpHtmlDataProvider }

function TWikiIpHtmlDataProvider.DoGetStream(const URL: string): TStream;
begin
  Result:=nil;
  if URL='' then exit;
  if URL=WikiHelp.ResultsCSSURL then begin
    //debugln(['TWikiIpHtmlDataProvider.DoGetStream loading css ']);
    Result:=TMemoryStream.Create;
    if WikiHelp.ResultsCSS<>'' then
      Result.Write(WikiHelp.ResultsCSS[1],length(WikiHelp.ResultsCSS));
    Result.Position:=0;
    exit;
  end;
  debugln(['TWikiIpHtmlDataProvider.DoGetStream ',URL]);
end;

{$R *.lfm}

{ TWikiSearchDemoForm }

procedure TWikiSearchDemoForm.FormCreate(Sender: TObject);
var
  Code: TCodeBuffer;
begin
  FPageHistory:=TWikiPageHistory.Create;

  Caption:='Search Wiki (Proof of concept)';

  // search panel
  SearchLabel.Caption:='Search:';
  SearchEdit.Text:='translations';
  SearchEdit.Hint:='Type one or more words separated by space, use " for phrases with spaces';
  HideSearchButton.Caption:='Hide';
  OptionsButton.Caption:='Options';

  // page panel
  ShowSearchToolButton.Hint:='Search';
  BackToolButton.Hint:='Go back one page';
  ForwardToolButton.Hint:='Go forward one page';
  ViewHTMLMenuItem.Caption:='View HTML Source';
  ViewWikiSourceMenuItem.Caption:='View Wiki Source';

  FURLDataProvider:=TWikiIpHtmlDataProvider.Create(nil);
  ResultsIpHtmlPanel.DataProvider:=FURLDataProvider;
  PageIpHtmlPanel.DataProvider:=FURLDataProvider;
  FURLDataProvider.OnCanHandle:=@DataProviderCanHandle;
  FURLDataProvider.OnGetImage:=@DataProviderGetImage;

  WikiHelp:=TWikiHelp.Create(nil);
  fLastScoring:=TWHScoring.Create;
  fLastScoring.Assign(WikiHelp.DefaultScoring);
  WikiHelp.XMLDirectory:=SetDirSeparators('../wikixml');
  WikiHelp.ImagesDirectory:=SetDirSeparators('../images');
  WikiHelp.Converter.OutputDir:='';
  WikiHelp.Converter.CSSFilename:='wiki.css';
  WikiHelp.Converter.WarnMissingPageLinks:=true;
  WikiHelp.OnScanned:=@WikiHelpScanned;
  WikiHelp.OnSearched:=@WikiHelpSearched;
  WikiHelp.ResultsCSSURL:='wiki.css';
  Code:=CodeToolBoss.LoadFile(TrimAndExpandFilename(SetDirSeparators('../html/wiki.css')),true,false);
  if Code<>nil then
    WikiHelp.ResultsCSS:=Code.Source;

  UpdateHistoryButtons;
  LoadHTML(ResultsIpHtmlPanel,'');
  LoadHTML(PageIpHtmlPanel,'');

  WikiHelp.StartLoading;
  UpdateProgress;
  Timer1.Enabled:=true;
end;

function TWikiSearchDemoForm.DataProviderCanHandle(Sender: TObject;
  const URL: string): Boolean;
begin
  //debugln(['TWikiSearchDemoForm.DataProviderCanHandle URL=',URL]);
  Result:=false;
end;

procedure TWikiSearchDemoForm.BackToolButtonClick(Sender: TObject);
begin
  if PageHistory.CurrentIndex<=0 then exit;
  GoToHistoryPage(PageHistory.CurrentIndex-1);
end;

procedure TWikiSearchDemoForm.DataProviderGetImage(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
var
  Filename: String;
  PicCreated: Boolean;
begin
  //debugln(['TWikiSearchDemoForm.DataProviderGetImage URL=',URL]);
  Filename:=WikiHelp.ImagesDirectory+URL;
  if not FileExistsUTF8(Filename) then begin
    debugln(['TWikiSearchDemoForm.DataProviderGetImage image not found "',Filename,'"']);
    exit;
  end;
  PicCreated := False;
  try
    if Picture=nil then begin
      Picture:=TPicture.Create;
      PicCreated := True;
    end;
    Picture.LoadFromFile(Filename);
  except
    if PicCreated then
      Picture.Free;
    Picture := nil;
  end;
end;

procedure TWikiSearchDemoForm.FormDestroy(Sender: TObject);
begin
  IdleConnected:=false;

  // free tool windows
  FreeAndNil(WikiSearchOptsWnd);

  // free pages before dataprovider
  FreeAndNil(ResultsIpHtmlPanel);
  FreeAndNil(PageIpHtmlPanel);
  FreeAndNil(FURLDataProvider);

  FreeAndNil(FPageHistory);
  FreeAndNil(WikiHelp);
  FreeAndNil(fLastScoring);
end;

procedure TWikiSearchDemoForm.ForwardToolButtonClick(Sender: TObject);
begin
  if PageHistory.CurrentIndex>=PageHistory.Count-1 then exit;
  GoToHistoryPage(PageHistory.CurrentIndex+1);
end;

procedure TWikiSearchDemoForm.HideSearchButtonClick(Sender: TObject);
begin
  DisableAutoSizing;
  try
    SearchPanel.Hide;
    Splitter1.Hide;
    ShowSearchToolButton.Visible:=true;
  finally
    EnableAutoSizing;
  end;
end;

procedure TWikiSearchDemoForm.LanguagesEditChange(Sender: TObject);
begin
  IdleConnected:=true;
end;

procedure TWikiSearchDemoForm.OnIdle(Sender: TObject; var Done: Boolean);
begin
  QueryChanged;
  IdleConnected:=false;
end;

function TWikiSearchDemoForm.GetLanguages: string;
begin
  if WikiSearchOptsWnd<>nil then
    Result:=WikiSearchOptsWnd.Languages
  else
    Result:='';
  //debugln(['TWikiSearchDemoForm.GetLanguages "',Result,'"']);
end;

procedure TWikiSearchDemoForm.IpHtmlPanelHotClick(Sender: TObject);
var
  HotNode: TIpHtmlNode;
  HRef: String;
  Panel: TIpHtmlPanel;
  DocumentName: String;
  p: SizeInt;
  AnchorName: String;
begin
  Panel:=Sender as TIpHtmlPanel;
  HotNode:=Panel.HotNode;
  if HotNode is TIpHtmlNodeA then begin
    HRef := TIpHtmlNodeA(HotNode).HRef;
    //Target := TIpHtmlNodeA(HotNode).Target;
  end else begin
    HRef := TIpHtmlNodeAREA(HotNode).HRef;
    //Target := TIpHtmlNodeAREA(HotNode).Target;
  end;
  debugln(['TWikiSearchDemoForm.ResultsIpHtmlPanelHotClick href=',href]);
  if WikiIsExternalLink(HRef) then begin
    // open external page
    OpenURL(HRef);
    exit;
  end;

  // load wiki page
  DocumentName:=HRef;
  p:=Pos('#',DocumentName);
  if p>0 then begin
    AnchorName:=copy(DocumentName,p+1,length(DocumentName));
    DocumentName:=LeftStr(DocumentName,p-1);
  end;
  LoadWikiPage(DocumentName,AnchorName,true);
end;

procedure TWikiSearchDemoForm.OptionsButtonClick(Sender: TObject);
begin
  ShowOptions;
end;

procedure TWikiSearchDemoForm.PagePopupMenuPopup(Sender: TObject);
begin
  ViewHTMLMenuItem.Enabled:=FPageDocumentName<>'';
  ViewWikiSourceMenuItem.Enabled:=FPageDocumentName<>'';
end;

procedure TWikiSearchDemoForm.SearchEditChange(Sender: TObject);
begin
  IdleConnected:=true;
end;

procedure TWikiSearchDemoForm.ViewHTMLMenuItemClick(Sender: TObject);
begin
  if (FPageDocumentName='') then exit;
  ViewSource('HTML of '+FPageDocumentName,FPageSource,SynHTMLSyn1);
end;

procedure TWikiSearchDemoForm.ShowSearchToolButtonClick(Sender: TObject);
begin
  DisableAutoSizing;
  try
    Splitter1.Show;
    SearchPanel.Show;
    SearchPanel.Width:=Max(SearchPanel.Width,30);
    Splitter1.Left:=Max(Splitter1.Left,SearchPanel.Width);
    ShowSearchToolButton.Visible:=false;
  finally
    EnableAutoSizing;
  end;
end;

procedure TWikiSearchDemoForm.ViewWikiSourceMenuItemClick(Sender: TObject);
var
  Page: TW2FormatPage;
begin
  if FPageDocumentName='' then exit;
  Page:=WikiHelp.Converter.GetPageWithDocumentName(FPageDocumentName);
  if (Page=nil) or (Page.WikiPage=nil) then begin
    MessageDlg('Wiki source not found','Unable to find Wiki source of page "'+FPageDocumentName+'".',mtError,[mbCancel],0);
    exit;
  end;
  ViewSource('Wiki source of '+FPageDocumentName,Page.WikiPage.Src,nil);
end;

procedure TWikiSearchDemoForm.Timer1Timer(Sender: TObject);
begin
  ProgressLabel.Caption:=WikiHelp.GetProgressCaption;
  //debugln(['TWikiSearchDemoForm.Timer1Timer ',ProgressLabel.Caption]);
  Timer1.Enabled:=WikiHelp.Busy;
end;

procedure TWikiSearchDemoForm.WikiSearchOptsWndOptionsChanged(Sender: TObject);
begin
  QueryChanged;
end;

procedure TWikiSearchDemoForm.WikiHelpScanned(Sender: TObject);
begin
  UpdateProgress;
  if WikiSearchOptsWnd<>nil then
    WikiSearchOptsWnd.UpdateAvailableLanguages;
end;

procedure TWikiSearchDemoForm.WikiHelpSearched(Sender: TObject);
var
  HTML: String;
begin
  UpdateProgress;
  HTML:=WikiHelp.ResultsHTML;
  if HTML='' then exit;
  LoadHTML(ResultsIpHtmlPanel,HTML);
end;

procedure TWikiSearchDemoForm.QueryChanged;
var
  NewSearchText: String;
  NewLanguages: String;
begin
  if ComponentState*[csDestroying,csLoading]<>[] then exit;
  NewSearchText:=UTF8Trim(SearchEdit.Text);
  NewLanguages:=GetLanguages;
  if (NewSearchText=fLastSearchText) and (NewLanguages=fLastLanguages)
  and ((WikiSearchOptsWnd=nil) or fLastScoring.Equals(WikiSearchOptsWnd.Scoring))
  then
    exit;
  fLastSearchText:=NewSearchText;
  fLastLanguages:=NewLanguages;
  if WikiSearchOptsWnd<>nil then
    fLastScoring.Assign(WikiSearchOptsWnd.Scoring);
  WikiHelp.Search(NewSearchText,NewLanguages,fLastScoring);
  Timer1.Enabled:=true;
end;

procedure TWikiSearchDemoForm.GoToHistoryPage(Index: integer);
var
  CurPage: TWikiPageHistoryItem;
begin
  PageHistory.CurrentIndex:=Index;
  CurPage:=PageHistory.Current;
  LoadWikiPage(CurPage.DocumentName, CurPage.Anchor, false);
  UpdateHistoryButtons;
end;

procedure TWikiSearchDemoForm.SetIdleConnected(AValue: boolean);
begin
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TWikiSearchDemoForm.UpdateHistoryButtons;
begin
  BackToolButton.Enabled:=PageHistory.CurrentIndex>0;
  ForwardToolButton.Visible:=PageHistory.CurrentIndex+1<PageHistory.Count;
end;

procedure TWikiSearchDemoForm.UpdateProgress;
begin
  ProgressLabel.Caption:=WikiHelp.GetProgressCaption;
  Timer1.Enabled:=WikiHelp.Busy;
end;

procedure TWikiSearchDemoForm.LoadWikiPage(Documentname, Anchor: string;
  AddToHistory: boolean);
var
  ms: TMemoryStream;
  Src: string;
  Page: TW2FormatPage;
begin
  if (FPageDocumentName=Documentname)
  and (Anchor=FPageAnchor) then
    exit;
  if Documentname<>'' then begin
    // open page in PageIpHtmlPanel
    ms:=TMemoryStream.Create;
    try
      try
        WikiHelp.SavePageToStream(DocumentName,ms);
        Page:=WikiHelp.Converter.GetPageWithDocumentName(Documentname);
        ms.Position:=0;
        SetLength(Src,ms.Size);
        if Src<>'' then
          ms.Read(Src[1],length(Src));
        ms.Position:=0;
        LoadHTML(PageIpHtmlPanel,ms,Anchor);
        FPageDocumentName:=DocumentName;
        FPageAnchor:=Anchor;
        FPageSource:=Src;
        if (Page<>nil) and (Page.WikiPage<>nil) then
          FPageTitle:=Page.WikiPage.Title
        else
          FPageTitle:=Documentname;
      except
        on E: Exception do begin
          FPageDocumentName:='';
          FPageAnchor:='';
          FPageSource:=Src;
          FPageTitle:='Error: '+E.Message;
          Src:='<html><body>'+EncodeLesserAndGreaterThan(FPageTitle)+'</body></html>';
          LoadHTML(PageIpHtmlPanel,Src);
        end;
      end;
    finally
      ms.Free;
    end;
  end else if Anchor<>'' then begin
    // same page
    // PageIpHtmlPanel.MakeAnchorVisible(Anchor+'/'); // ipHTML stores anchor names with / at end
    PageIpHtmlPanel.MakeAnchorVisible(Anchor);        // ... not any longer, fixed in r49421
    FPageAnchor := Anchor;
  end;
  if AddToHistory and (FPageDocumentName<>'') then begin
    PageHistory.AddAfterCurrent(FPageDocumentName,FPageAnchor,FPageTitle);
    UpdateHistoryButtons;
  end;
end;

procedure TWikiSearchDemoForm.LoadHTML(Target: TIpHtmlPanel; HTML: string;
  Anchor: string);
var
  ms: TMemoryStream;
begin
  if HTML='' then
    HTML:='<html><body></body></html>';
  ms:=TMemoryStream.Create;
  try
    try
      ms.Write(HTML[1],length(HTML));
      ms.Position:=0;
      LoadHTML(Target,ms,Anchor);
    except
      on E: Exception do begin
        debugln(['TWikiSearchDemoForm.LoadHTML ',E.Message]);
      end;
    end;
  finally
    ms.Free;
  end;
end;

procedure TWikiSearchDemoForm.LoadHTML(Target: TIpHtmlPanel; aStream: TStream;
  Anchor: string);
var
  NewHTML: TIpHtml;
begin
  try
    NewHTML:=TIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanel
    Target.SetHtml(NewHTML);
    NewHTML.LoadFromStream(aStream);
    if Anchor<>'' then
      //Target.MakeAnchorVisible(Anchor+'/'); // ipHTML stores anchor names with / at end
      Target.MakeAnchorVisible(Anchor);       // ... not any longer, fixed in r49421
  except
    on E: Exception do begin
      debugln(['TWikiSearchDemoForm.LoadHTML ',E.Message]);
    end;
  end;
end;

procedure TWikiSearchDemoForm.ShowOptions;
begin
  if WikiSearchOptsWnd=nil then begin
    WikiSearchOptsWnd:=TWikiSearchOptsWnd.Create(Self);
    WikiSearchOptsWnd.OnOptionsChanged:=@WikiSearchOptsWndOptionsChanged;
  end;
  WikiSearchOptsWnd.UpdateAvailableLanguages;
  WikiSearchOptsWnd.ShowOnTop;
end;

procedure TWikiSearchDemoForm.ViewSource(aTitle, aSource: string;
  aHighlighter: TSynCustomHighlighter);
var
  SrcEdit: TSynEdit;
  ViewSrcForm: TForm;
begin
  ViewSrcForm:=TForm.Create(Self);
  ViewSrcForm.DisableAutoSizing;
  try
    ViewSrcForm.Caption:=aTitle;
    ViewSrcForm.Position:=poScreenCenter;
    ViewSrcForm.Width:=500;
    ViewSrcForm.Height:=500;

    SrcEdit:=TSynEdit.Create(ViewSrcForm);
    SrcEdit.Align:=alClient;
    SrcEdit.Text:=aSource;
    SrcEdit.Highlighter:=aHighlighter;
    SrcEdit.Parent:=ViewSrcForm;
    SrcEdit.ReadOnly:=true;

    ViewSrcForm.Show;
  finally
    ViewSrcForm.EnableAutoSizing;
  end;
end;

end.

