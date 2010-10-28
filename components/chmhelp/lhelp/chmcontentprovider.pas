unit chmcontentprovider;

{$mode objfpc}{$H+}

//{$if (fpc_version=2) and (fpc_release>2) ((fpc_version=2) and (fpc_release=2) and (fpc_patch>2))}
{$Note Compiling lhelp with search support}
{$DEFINE CHM_SEARCH}

//{$else}
//{$Note Compiling lhelp *without* search support since your fpc version is not new enough}
//{$endif}
{$if (fpc_version=2) and (fpc_release>4)}
{$Note Compiling lhelp *with* binary index and toc support}
{$DEFINE CHM_BINARY_INDEX_TOC}
{$endif}


{off $DEFINE CHM_DEBUG_TIME}
{off $DEFINE CHM_SEARCH}

interface

uses
  Classes, SysUtils, XMLCfg,
  FileUtil, Forms, StdCtrls, ExtCtrls, ComCtrls, Controls, Buttons, Menus,
  BaseContentProvider, FileContentProvider, IpHtml, ChmReader, ChmDataProvider;

type

  { TChmContentProvider }

  TChmContentProvider = class(TFileContentProvider)
  private
    fTabsControl: TPageControl;
      fContentsTab: TTabSheet;
       fContentsPanel: TPanel;
         fContentsTree: TTreeView;
      fIndexTab: TTabSheet;
        fIndexEdit: TLabeledEdit;
        fIndexView: TTreeView;//TListView;
      fSearchTab: TTabSheet;
        fKeywordLabel: TLabel;
        fKeywordCombo: TComboBox;
        fSearchBtn: TButton;
        fResultsLabel: TLabel;
        fSearchResults: TTreeView;
    fSplitter: TSplitter;
    fHtml: TIpHtmlPanel;
    fPopUp: TPopUpMenu;
    fStatusBar: TStatusBar;
    fContext: THelpContext;
  protected
    fIsUsingHistory: Boolean;
    fChms: TChmFileList;
    fHistory: TStringList;
    fHistoryIndex: Integer;
    fStopTimer: Boolean;
    fFillingToc: Boolean;
    fFillingIndex: Boolean;
    fActiveChmTitle: String;
    FLoadingSearchURL: Boolean; // use this to try to highlight search terms

    function  MakeURI(AUrl: String; AChm: TChmReader): String;

    procedure AddHistory(URL: String);
    procedure DoOpenChm(AFile: String; ACloseCurrent: Boolean = True);
    procedure DoCloseChm;
    procedure DoLoadContext(Context: THelpContext);
    procedure DoLoadUri(Uri: String; AChm: TChmReader = nil);
    procedure DoError(Error: Integer);
    procedure NewChmOpened(ChmFileList: TChmFileList; Index: Integer);
    procedure LoadingHTMLStream(var AStream: TStream);

    procedure QueueFillToc(AChm: TChmReader);
    procedure FillTOC(Data: PtrInt);
    procedure IpHtmlPanelDocumentOpen(Sender: TObject);
    procedure IpHtmlPanelHotChange(Sender: TObject);
    procedure PopupCopyClick(Sender: TObject);
    procedure ContentsTreeSelectionChanged(Sender: TObject);
    procedure IndexViewDblClick(Sender: TObject);
    procedure TreeViewStopCollapse(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
    procedure ViewMenuContentsClick(Sender: TObject);
    procedure UpdateTitle;
    procedure SetTitle(const AValue: String); override;
    procedure SearchEditChange(Sender: TObject);
    procedure TOCExpand(Sender: TObject; Node: TTreeNode);
    procedure TOCCollapse(Sender: TObject; Node: TTreeNode);
    procedure SelectTreeItemFromURL(AUrl: String);
    {$IFDEF CHM_SEARCH}
    procedure SearchButtonClick(Sender: TObject);
    procedure SearchResultsDblClick(Sender: TObject);
    procedure SearchComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GetTreeNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    {$ENDIF}
  public
    procedure LoadPreferences(ACfg: TXMLConfig); override;
    procedure SavePreferences(ACfg: TXMLConfig); override;
  public
    function CanGoBack: Boolean; override;
    function CanGoForward: Boolean; override;
    function GetHistory: TStrings; override;
    function LoadURL(const AURL: String; const AContext: THelpContext=-1): Boolean; override;
    procedure GoHome; override;
    procedure GoBack; override;
    procedure GoForward; override;
    property TabsControl: TPageControl read fTabsControl;
    property Splitter: TSplitter read fSplitter;
    class function GetProperContentProvider(const AURL: String): TBaseContentProviderClass; override;

    constructor Create(AParent: TWinControl; AImageList: TImageList); override;
    destructor Destroy; override;
  end;

implementation

uses ChmSpecialParser{$IFDEF CHM_SEARCH}, chmFIftiMain{$ENDIF}, chmsitemap, LCLType, SAX_HTML, Dom, XMLWrite, DOM_HTML, HTMWrite;

type

  { THTMLWordHighlighter }

  THTMLWordHighlighter = class
  private
    Doc: THTMLDocument;
    Words: TStrings;
    Color: String;
    procedure ScanSubNodes(ADomNode: TDOMNode);
    procedure CheckTextNode(var ATextNode: TDomNode);
  public
    constructor Create(AHTMLDoc: THTMLDocument);
    procedure HighlightWords(AWords: TStrings; AColor: String);
  end;

{ THTMLWordHighlighter }

procedure THTMLWordHighlighter.ScanSubNodes(ADomNode: TDOMNode);

var
  CurNode: TDomNode;
begin
  CurNode := ADomNode;
  while CurNode <> nil do
  begin
    if CurNode.HasChildNodes then
      ScanSubNodes(CurNode.FirstChild);

    if CurNode.NodeType = TEXT_NODE then
      CheckTextNode(CurNode);

    CurNode := CurNode.NextSibling;
  end;
end;

procedure THTMLWordHighlighter.CheckTextNode(var ATextNode: TDomNode);
var
  i: Integer;
  fPos: Integer;
  WordStart,
  After: TDOMText;
  Span: TDomElement;
  aWord: String;
  Parent: TDomNode;
begin
   Parent := AtextNode.ParentNode;
   for i := 0 to Words.Count-1 do
   begin
     aWord := Words[i];
     fPos := Pos(aWord, LowerCase(ATextNode.TextContent));
     while fpos > 0 do
     begin
       WordStart:= TDOMText(ATextNode).SplitText(fPos-1);
       After := WordStart.SplitText(Length(aword));
       Span := doc.CreateElement('span');
       Span.SetAttribute('style', 'color:'+Color+';background-color:lightgray');
       Parent.InsertBefore(Span, After);
       Span.AppendChild(WordStart);

       // or we'll keep finding our new node again and again
       ATextNode := After;

       fPos := Pos(aWord, ATextNode.TextContent);
     end;
   end;
end;

constructor THTMLWordHighlighter.Create(AHTMLDoc: THTMLDocument);
begin
  Doc := AHTMLDoc;
end;

procedure THTMLWordHighlighter.HighlightWords(AWords: TStrings; AColor: String);
var
  Elem: TDOMNode;
begin
  Words := AWords;
  Color := AColor;
  Elem := Doc.DocumentElement.FirstChild;

  ScanSubNodes(Elem);

end;

function GetURIFileName(AURI: String): String;
var
  FileStart,
  FileEnd: Integer;
begin
  FileStart := Pos(':', AURI)+1;
  FileEnd := Pos('::', AURI);

  Result := Copy(AURI, FileStart, FileEnd-FileStart);
end;

function GetURIURL(AURI: String): String;
var
  URLStart: Integer;
begin
  URLStart := Pos('::', AURI) + 2;
  Result := Copy(AURI, URLStart, Length(AURI));
end;

function ChmURI(AUrl: String; AFileName: String): String;
var
  FileNameNoPath: String;
begin
  Result := AUrl;
  if Pos('ms-its:', Result) > 0 then
    Exit;
  FileNameNoPath := ExtractFileName(AFileName);

  Result := 'ms-its:'+FileNameNoPath+'::'+AUrl;
end;

{ TChmContentProvider }

function TChmContentProvider.MakeURI ( AUrl: String; AChm: TChmReader ) : String;
var
  ChmIndex: Integer;
begin
  ChmIndex := fChms.IndexOfObject(AChm);

  Result := ChmURI(AUrl, fChms.FileName[ChmIndex]);
end;

procedure TChmContentProvider.AddHistory(URL: String);
begin
  if fHistoryIndex < fHistory.Count then begin
    while fHistory.Count-1 > fHistoryIndex do
      fHistory.Delete(fHistory.Count-1);
  end;

  fHistory.Add(URL);
  Inc(fHistoryIndex);
end;

type
  TCHMHack = class(TChmFileList)
  end;

procedure TChmContentProvider.DoOpenChm(AFile: String; ACloseCurrent: Boolean = True);
begin
  if (fChms <> nil) and fChms.IsAnOpenFile(AFile) then Exit;
  if ACloseCurrent then DoCloseChm;
  if not FileExistsUTF8(AFile) or DirectoryExistsUTF8(AFile) then
  begin
    Exit;
  end;
  if fChms = nil then
  begin
    try
      fChms := TChmFileList.Create(AFile);
      if Not(fChms.Chm[0].IsValidFile) then begin
        FreeAndNil(fChms);
        //DoError(INVALID_FILE_TYPE);
        Exit;
      end;
      TIpChmDataProvider(fHtml.DataProvider).Chm := fChms;
    except
      FreeAndNil(fChms);
      //DoError(INVALID_FILE_TYPE);
      Exit;
    end;
  end
  else
  begin
    TCHMHack(fChms).OpenNewFile(AFile);
    //WriteLn('Loading new chm: ', AFile);
  end;

  if fChms = nil then Exit;

  fHistoryIndex := -1;
  fHistory.Clear;

  // Code Here has been moved to the OpenFile handler

  UpdateTitle;
end;

procedure TChmContentProvider.DoCloseChm;
var
  i : integer;
begin
  fStopTimer := True;
  if assigned(fChms) then
  begin
    for i := 0 to fChms.Count -1 do
      fChms.Chm[i].Free;
  end;
  FreeAndNil(fChms);
  UpdateTitle;
end;

procedure TChmContentProvider.DoLoadContext(Context: THelpContext);
var
 Str: String;
begin
  if fChms = nil then exit;
  Str := fChms.Chm[0].GetContextUrl(Context);
  if Str <> '' then DoLoadUri(Str, fChms.Chm[0]);
end;

procedure TChmContentProvider.DoLoadUri(Uri: String; AChm: TChmReader = nil);
var
  ChmIndex: Integer;
  NewUrl: String;
  FilteredURL: String;
  fPos: Integer;
  StartTime: TDateTime;
  EndTime: TDateTime;
  Time: String;
begin
  if (fChms = nil) and (AChm = nil) then exit;
  fStatusBar.SimpleText :='Loading: '+Uri;
  Application.ProcessMessages;
  StartTime := Now;

  fPos := Pos('#', Uri);
  if fPos > 0 then
    FilteredURL := Copy(Uri, 1, fPos -1)
  else
    FilteredURL := Uri;

  if fChms.ObjectExists(FilteredURL, AChm) = 0 then begin
    fStatusBar.SimpleText := URI + ' not found!';
    Exit;
  end;
  if (Pos('ms-its', Uri) = 0) and (AChm <> nil) then
  begin
    ChmIndex := fChms.IndexOfObject(AChm);
    NewUrl := ExtractFileName(fChms.FileName[ChmIndex]);
    NewUrl := 'ms-its:'+NewUrl+'::/'+Uri;
    Uri := NewUrl;
  end;

  fIsUsingHistory := True;
  fHtml.OpenURL(Uri);
  TIpChmDataProvider(fHtml.DataProvider).CurrentPath := ExtractFileDir(URI)+'/';

  AddHistory(Uri);
  EndTime := Now;

  Time := INtToStr(DateTimeToTimeStamp(EndTime).Time - DateTimeToTimeStamp(StartTime).Time);
  fStatusBar.SimpleText :='Loaded: '+Uri+' in '+ Time+'ms';
end;


procedure TChmContentProvider.DoError(Error: Integer);
begin
  //what to do with these errors?
  //INVALID_FILE_TYPE;
end;

procedure TChmContentProvider.NewChmOpened(ChmFileList: TChmFileList;
  Index: Integer);
begin
  if Index = 0 then begin
    if fContext > -1 then begin
      DoLoadContext(fContext);
      fContext := -1;
    end
    else if ChmFileList.Chm[Index].DefaultPage <> '' then begin
      DoLoadUri(MakeURI(ChmFileList.Chm[Index].DefaultPage, ChmFileList.Chm[Index]));
    end;
  end;
  if ChmFileList.Chm[Index].Title = '' then
    ChmFileList.Chm[Index].Title := ExtractFileName(ChmFileList.FileName[Index]);

  // Fill the table of contents.
  if Index <> 0 then
    QueueFillToc(ChmFileList.Chm[Index]);
end;

procedure TChmContentProvider.LoadingHTMLStream(var AStream: TStream);
var
  Doc: THTMLDocument;
  NewStream: TMemoryStream;
  Highlighter: THTMLWordHighlighter;
  Words: TStringList;
  UseOrigStream: Boolean;
begin
  if not FLoadingSearchURL then
    Exit;
  // load html and add tags to highlight words then save back to stream
  NewStream := TMemoryStream.Create;

  Words := TStringList.Create;
  Words.Delimiter:=' ';
  Words.DelimitedText:=fKeywordCombo.Text;

  try
    UseOrigStream := True;
    ReadHTMLFile(Doc, AStream);
    Highlighter := THTMLWordHighlighter.Create(Doc);
    Highlighter.HighlightWords(Words, 'red');
    WriteHTMLFile(Doc, NewStream);
    UseOrigStream := False;
  finally
    try
      Doc.Free;
      Highlighter.Free;
    finally
    end;
  end;

  Words.Free;

  if not UseOrigStream then
  begin
    AStream.Free;
    AStream := NewStream;
    NewStream.Position:=0;
  end
  else
    NewStream.Free;

  AStream.Position := 0;
end;

procedure TChmContentProvider.QueueFillToc(AChm: TChmReader);
begin
  fContentsTree.Visible := False;
  fContentsPanel.Caption := 'Table of Contents Loading. Please Wait...';
  fStatusBar.SimpleText:= 'Table of Contents Loading...';
  Application.ProcessMessages;
  Application.QueueAsyncCall(@FillToc, PtrInt(AChm));
end;

procedure TChmContentProvider.FillTOC(Data: PtrInt);
var
 fChm: TChmReader;
 ParentNode: TTreeNode;
 i: Integer;
 SM: TChmSiteMap;
 HasSearchIndex: Boolean = False;
 Stream: TMemoryStream;
begin
  if fFillingToc or fFillingIndex then begin
    Application.QueueAsyncCall(@FillToc, Data);
    exit;
  end;
  fFillingToc := True;
  fContentsTree.BeginUpdate;

  fChm := TChmReader(Data);
  {$IFDEF CHM_DEBUG_TIME}
  writeln('Start: ',FormatDateTime('hh:nn:ss.zzz', Now));
  {$ENDIF}
  if fChm <> nil then begin
    ParentNode := fContentsTree.Items.AddChildObject(nil, fChm.Title, fChm);
    ParentNode.ImageIndex := 0;
    ParentNode.SelectedIndex := 0;
    {$IFDEF CHM_BINARY_INDEX_TOC}
    SM := fChm.GetTOCSitemap;
    {$ELSE}
    SM := nil;
    fFillingIndex := True;
    Stream := TMemoryStream(fchm.GetObject(fChm.TOCFile));
    if Stream <> nil then begin
      SM := TChmSiteMap.Create(stTOC);
      SM.LoadFromStream(Stream);
      Stream.Free;
    end;
    {$ENDIF}
    if SM <> nil then begin
      {$IFDEF CHM_DEBUG_TIME}
      writeln('Stream read: ',FormatDateTime('hh:nn:ss.zzz', Now));
      {$ENDIF}
      with TContentsFiller.Create(fContentsTree, SM, @fStopTimer, fChm) do begin
        DoFill(ParentNode);
        Free;
      end;
      SM.Free;
      if (fContentsTree.Selected = nil) and (fHistory.Count > 0) then
        SelectTreeItemFromURL(fHistory.Strings[fHistoryIndex]);
    end;
    if ParentNode.Index = 0 then ParentNode.Expanded := True;
    fFillingToc := False;
    fContentsTree.EndUpdate;
    fContentsTree.Visible := True;
    fContentsPanel.Caption := '';
    fContentsTab.TabVisible := fContentsTree.Items.Count > 1;
    Application.ProcessMessages;
    fFillingIndex := True;


    // we fill the index here too but only for the main file
    if fChms.IndexOfObject(fChm) < 1 then
    begin
      {$IFDEF CHM_BINARY_INDEX_TOC}
      SM := fChm.GetIndexSitemap;
      {$ELSE}
      SM := nil;
      Stream := TMemoryStream(fchm.GetObject(fChm.IndexFile));
      if Stream <> nil then begin
        SM := TChmSiteMap.Create(stTOC);
        SM.LoadFromStream(Stream);
        Stream.Free;
      end;
      {$ENDIF}
      if SM <> nil then begin
        fStatusBar.SimpleText:= 'Index Loading...';
        Application.ProcessMessages;
        with TContentsFiller.Create(fIndexView, SM, @fStopTimer, fChm) do begin
          DoFill(nil);
          Free;
        end;
        SM.Free;
        fIndexView.FullExpand;

      end;
    end;
  end;
  fFillingIndex := False;
  fIndexTab.TabVisible := fIndexView.Items.Count > 0;

  fStatusBar.SimpleText:= '';

  {$IFDEF CHM_DEBUG_TIME}
  writeln('Eind: ',FormatDateTime('hh:nn:ss.zzz', Now));
  {$ENDIF}



  {$IFDEF CHM_SEARCH}
  i := 0;
  while (HasSearchIndex = False) and (i < fChms.Count) do
  begin
    HasSearchIndex := fChms.Chm[i].ObjectExists('/$FIftiMain') > 0;
    inc(i);
  end;

  fSearchTab.TabVisible := HasSearchIndex;
  {$ENDIF}
end;

procedure TChmContentProvider.IpHtmlPanelDocumentOpen(Sender: TObject);
begin
   // StatusBar1.Panels.Items[1] := fHtml.DataProvider.;
 if fIsUsingHistory = False then
   AddHistory(TIpChmDataProvider(fHtml.DataProvider).CurrentPage)
 else fIsUsingHistory := False;
 SelectTreeItemFromURL(TIpChmDataProvider(fHtml.DataProvider).CurrentPage);
end;

procedure TChmContentProvider.IpHtmlPanelHotChange(Sender: TObject);
begin
  fStatusBar.SimpleText := fHtml.HotURL;
end;

procedure TChmContentProvider.PopupCopyClick(Sender: TObject);
begin
  fHtml.CopyToClipboard;
end;

procedure TChmContentProvider.ContentsTreeSelectionChanged(Sender: TObject);
var
ATreeNode: TContentTreeNode;
ARootNode: TTreeNode;
fChm: TChmReader = nil;
Uri: String;
begin
  if (fContentsTree.Selected = nil) then Exit;
  if fContentsTree.Selected.Parent = nil then
  begin
    fChm := TChmReader(fContentsTree.Selected.Data);
    fActiveChmTitle:= fChm.Title;
    UpdateTitle;
    if fChm.DefaultPage <> '' then
    begin
      Uri := MakeURI(fChm.DefaultPage, fChm);
      if ((fHtml.MasterFrame <> nil) and (MakeURI(fHtml.CurURL, fChm)  = Uri)) = False then
        DoLoadUri(Uri);
    end;
    Exit;

  end;

  ATreeNode := TContentTreeNode(fContentsTree.Selected);

  //find the chm associated with this branch
  ARootNode := ATreeNode.Parent;
  while ARootNode.Parent <> nil do
    ARootNode := ARootNode.Parent;

  fChm := TChmReader(ARootNode.Data);
  try
    fContentsTree.OnSelectionChanged := nil;
    if ATreeNode.Url <> '' then begin
      Uri := MakeURI(ATreeNode.Url, fChm);
      if ((fHtml.MasterFrame <> nil) and (MakeURI(fHtml.CurURL, fChm)  = Uri)) = False then
        DoLoadUri(MakeURI(ATreeNode.Url, fChm));
    end;
  finally
    fContentsTree.OnSelectionChanged := @ContentsTreeSelectionChanged;
  end;
end;

procedure TChmContentProvider.IndexViewDblClick(Sender: TObject);
var
  ATreeNode: TContentTreeNode;
begin
  if fIndexView.Selected = nil then Exit;
  ATreeNode := TContentTreeNode(fIndexView.Selected);

  //find the chm associated with this branch
   DoLoadUri(MakeURI(ATreeNode.Url, TChmReader(ATreeNode.Data)));
end;

procedure TChmContentProvider.TreeViewStopCollapse(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  AllowCollapse:=False;
end;

procedure TChmContentProvider.ViewMenuContentsClick(Sender: TObject);
begin
  //TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  //fSplitter.Visible := TMenuItem(Sender).Checked;
  //TabPanel.Visible := Splitter1.Visible;
end;

procedure TChmContentProvider.UpdateTitle;
var
  Item: TTreeNode;
  NewTitle: String;
begin
  Item := fContentsTree.Items.GetFirstNode;
  NewTitle:=fActiveChmTitle +' [';
  while Item <> nil do
  begin
    if ITem.Text <> fActiveChmTitle then
    begin
      NewTitle:=NewTitle+Item.Text;
      if (Item.GetNextSibling <> nil)
      and ((Item.GetNextSibling.GetNextSibling <> nil) or (Item.GetNextSibling.Text <>  fActiveChmTitle))
      then
        NewTitle:=NewTitle+', ';
    end;
    Item := Item.GetNextSibling;
  end;
  NewTitle:=NewTitle+']';
  Title := NewTitle;
end;

procedure TChmContentProvider.SetTitle(const AValue: String);
begin
  if fHtml.Parent = nil then exit;
  TTabSheet(fHtml.Parent).Caption := AValue;
  inherited SetTitle(AValue);
end;

procedure TChmContentProvider.SearchEditChange(Sender: TObject);
var
  ItemName: String;
  SearchText: String;
  Node: TTreeNode;
begin
if not fIndexEdit.Focused then Exit;
  SearchText := LowerCase(fIndexEdit.Text);
  Node := fIndexView.Items.GetFirstNode;
  while Node<>nil do begin

    ItemName := LowerCase(Copy(Node.Text, 1, Length(SearchText)));
    if ItemName = SearchText then begin
      fIndexView.Items.GetLastNode.MakeVisible;
      Node.MakeVisible;
      Node.Selected:=True;
      Exit;
    end;
    Node := Node.GetNextSibling;
  end;
  fIndexView.Selected:=nil;
end;

procedure TChmContentProvider.TOCExpand(Sender: TObject; Node: TTreeNode);
begin
  if Node.Parent <> nil then
  begin
    Node.ImageIndex := 2;
    Node.SelectedIndex := 2;
  end;
end;

procedure TChmContentProvider.TOCCollapse(Sender: TObject; Node: TTreeNode) ;
begin
  if Node.Parent <> nil then
  begin
    Node.ImageIndex := 1;
    Node.SelectedIndex := 1;
  end;
end;

procedure TChmContentProvider.SelectTreeItemFromURL(AUrl: String);
var
  FileName: String;
  URL: String;
  RootNode,
  FoundNode,
  Node: TTreeNode;
  TmpHolder: TNotifyEvent;
  i: integer;
begin
  if fContentsTree.OnSelectionChanged = nil then
    Exit; // the change was a response to a click and should be ignored
  FileName := GetURIFileName(AUrl);
  URL      := GetURIURL(AUrl);
  FoundNode := nil;
  Node := nil;
  for i := 0 to fChms.Count-1 do
  begin
    if FileName = ExtractFileName(fChms.FileName[i]) then
    begin
      fActiveChmTitle:= fChms.Chm[i].Title;
      UpdateTitle;

      RootNode := fContentsTree.Items.FindNodeWithData(fChms.Chm[i]);
      if URL = fChms.Chm[i].DefaultPage then
      begin
        FoundNode := RootNode;
        Break;
      end;

      if RootNode <> nil then
        Node := RootNode.GetFirstChild;

      Break;
    end;

  end;

  if RootNode = nil then
    Exit;

  TmpHolder := fContentsTree.OnSelectionChanged;
  fContentsTree.OnSelectionChanged := nil;

  while (Node<>nil) and (TContentTreeNode(Node).Url<>Url) do
    Node:=Node.GetNext;

  if (Node <> nil) and (TContentTreeNode(Node).Url = Url) then
    FoundNode := Node;

  if FoundNode <> nil then
  begin
    fContentsTree.Selected := FoundNode;
    if not FoundNode.IsVisible then
      FoundNode.MakeVisible;
  end
  else
    fContentsTree.Selected := nil;

  fContentsTree.OnSelectionChanged := TmpHolder;
end;

{$IFDEF CHM_SEARCH}

procedure TChmContentProvider.SearchComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key of
    VK_RETURN: SearchButtonClick(nil);

  end;
end;

procedure TChmContentProvider.GetTreeNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TContentTreeNode;
end;

procedure TChmContentProvider.LoadPreferences(ACfg: TXMLConfig);
begin
  inherited LoadPreferences(ACfg);
  fTabsControl.Width := ACfg.GetValue(ClassName+'/TabControlWidth/Value', fTabsControl.Width);
end;

procedure TChmContentProvider.SavePreferences(ACfg: TXMLConfig);
begin
  inherited SavePreferences(ACfg);
  ACfg.SetValue(ClassName+'/TabControlWidth/Value', fTabsControl.Width);
end;

procedure TChmContentProvider.SearchButtonClick ( Sender: TObject ) ;
type
  TTopicEntry = record
    Topic:Integer;
    Hits: Integer;
    TitleHits: Integer;
    FoundForThisRound: Boolean;
  end;
  TFoundTopics = array of TTopicEntry;
var
  FoundTopics: TFoundTopics;

  procedure DeleteTopic(ATopicIndex: Integer);
  var
    MoveSize: DWord;
  begin
    //WriteLn('Deleting Topic');
    if ATopicIndex < High(FoundTopics) then
    begin
      MoveSize := SizeOf(TTopicEntry) * (High(FoundTopics) - (ATopicIndex+1));
      Move(FoundTopics[ATopicIndex+1], FoundTopics[ATopicIndex], MoveSize);
    end;
    SetLength(FoundTopics, Length(FoundTopics) -1);
  end;

  function GetTopicIndex(ATopicID: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := 0 to High(FoundTopics) do begin
      if FoundTopics[i].Topic = ATopicID then
        Exit(i);
    end;
  end;

  procedure UpdateTopic(TopicID: Integer; NewHits: Integer; NewTitleHits: Integer; AddNewTopic: Boolean);
  var
    TopicIndex: Integer;
  begin
    //WriteLn('Updating topic');
    TopicIndex := GetTopicIndex(TopicID);
    if TopicIndex = -1 then
    begin
      if AddNewTopic = False then
        Exit;
      SetLength(FoundTopics, Length(FoundTopics)+1);
      TopicIndex := High(FoundTopics);
      FoundTopics[TopicIndex].Topic := TopicID;
    end;

    FoundTopics[TopicIndex].FoundForThisRound := True;
    if NewHits > 0 then
      Inc(FoundTopics[TopicIndex].Hits, NewHits);
    if NewTitleHits > 0 then
      Inc(FoundTopics[TopicIndex].TitleHits, NewTitleHits);
  end;

var
  TopicResults: TChmWLCTopicArray;
  TitleResults: TChmWLCTopicArray;
  FIftiMainStream: TMemoryStream;
  SearchWords: TStringList;
  SearchReader: TChmSearchReader;
  DocTitle: String;
  DocURL: String;
  i: Integer;
  j: Integer;
  k: Integer;
  Item: TContentTreeNode;
begin
  SearchWords := TStringList.Create;
  SearchWords.Delimiter := ' ';
  Searchwords.DelimitedText := fKeywordCombo.Text;
  if fKeywordCombo.Items.IndexOf(fKeywordCombo.Text) = -1 then
    fKeywordCombo.Items.Add(fKeywordCombo.Text);
  fSearchResults.BeginUpdate;
  fSearchResults.Items.Clear;
  //WriteLn('Search words: ', SearchWords.Text);
  for i := 0 to fChms.Count-1 do
  begin
    for j := 0 to SearchWords.Count-1 do
    begin
      if fChms.Chm[i].SearchReader = nil then
      begin
        FIftiMainStream := fchms.Chm[i].GetObject('/$FIftiMain');
        if FIftiMainStream = nil then
          continue;
        SearchReader := TChmSearchReader.Create(FIftiMainStream, True); //frees the stream when done
        fChms.Chm[i].SearchReader := SearchReader;
      end
      else
        SearchReader := fChms.Chm[i].SearchReader;
      TopicResults := SearchReader.LookupWord(SearchWords[j], TitleResults);
      // body results
      for k := 0 to High(TopicResults) do
        UpdateTopic(TopicResults[k].TopicIndex, High(TopicResults[k].LocationCodes), 0, j = 0);
      // title results
      for k := 0 to High(TitleResults) do
        UpdateTopic(TitleResults[k].TopicIndex, 0, High(TitleResults[k].LocationCodes), j = 0);

      // remove documents that don't have results
      k := 0;
      while k <= High(FoundTopics) do
      begin
        if FoundTopics[k].FoundForThisRound = False then
          DeleteTopic(k)
        else
        begin
          FoundTopics[k].FoundForThisRound := False;
          Inc(k);
        end;
      end;
    end;

    // clear out results that don't contain all the words we are looking for

    Item := nil;
    // now lookup titles and urls to add to final search results
    for j := 0 to High(FoundTopics) do
    begin
    try
      DocURL := fChms.Chm[i].LookupTopicByID(FoundTopics[j].Topic, DocTitle);
      if (Length(DocURL) > 0) and (DocURL[1] <> '/') then
        Insert('/', DocURL, 1);
      if DocTitle = '' then
        DocTitle := 'untitled';
      Item := TContentTreeNode(fSearchResults.Items.Add(Item, DocTitle));
      Item.Data:= fChms.Chm[i];
      Item.Url:= DocURL;
    except
      //WriteLn('Exception');
      // :)
    end;
    end;

    SetLength(FoundTopics, 0);
  end;
  SetLength(FoundTopics, 0);

  SearchWords.Free;
  if fSearchResults.Items.Count = 0 then
  begin
    fSearchResults.Items.Add(nil, 'No Results');
  end;
  fSearchResults.EndUpdate;
end;

procedure TChmContentProvider.SearchResultsDblClick ( Sender: TObject ) ;
var
  Item: TContentTreeNode;
begin
  Item := TContentTreeNode(fSearchResults.Selected);
  if (Item = nil) or (Item.Data = nil) then
    Exit;
  FLoadingSearchURL:= True;
  DoLoadUri(MakeURI(Item.Url, TChmReader(Item.Data)));
  FLoadingSearchURL:= False;
end;
{$ENDIF}


function TChmContentProvider.CanGoBack: Boolean;
begin
  Result := fHistoryIndex > 0;
end;

function TChmContentProvider.CanGoForward: Boolean;
begin
  Result := fHistoryIndex < fHistory.Count-1
end;

function TChmContentProvider.GetHistory: TStrings;
begin
  Result:= fHistory;
end;

function TChmContentProvider.LoadURL(const AURL: String; const AContext: THelpContext=-1): Boolean;
var
  fFile: String;
  fURL: String = '';
  fPos: Integer;
  FileIndex: Integer;
  LoadTOC: Boolean;
begin
  Result := False;
  fFile := Copy(AUrl,8, Length(AURL));
  fPos := Pos('://', fFile);
  if fPos > 0 then begin
    fURL := Copy(fFile, fPos+3, Length(fFIle));
    fFile := Copy(fFIle, 1, fPos-1);
  end;

  LoadTOC := (fChms = nil) or (fChms.IndexOf(fFile) < 0);
  DoOpenChm(fFile, False);

  // in case of exception fChms can be still = nil
  if fChms <> nil then
    FileIndex := fChms.IndexOf(fFile)
  else
    Exit;

  if LoadTOC and (FileIndex = 0) then
  begin
    QueueFillToc(fChms.Chm[FileIndex]);
  end;


  if fURL <> '' then
    DoLoadUri(MakeURI(fURL, fChms.Chm[FileIndex]))
  else
    DoLoadUri(MakeURI(fChms.Chm[FileIndex].DefaultPage, fChms.Chm[FileIndex]));
  Result := True;

  fChms.OnOpenNewFile := @NewChmOpened;
end;

procedure TChmContentProvider.GoHome;
begin
  if (fChms <> nil) and (fChms.Chm[0].DefaultPage <> '') then begin
    DoLoadUri(MakeURI(fChms.Chm[0].DefaultPage, fChms.Chm[0]));
  end;
end;

procedure TChmContentProvider.GoBack;
begin
  if CanGoBack then begin
    Dec(fHistoryIndex);
    fIsUsingHistory:=True;
    fHtml.OpenURL(fHistory.Strings[fHistoryIndex]);
  end;
end;

procedure TChmContentProvider.GoForward;
var
  HistoryChm: TChmReader;
begin
  if CanGoForward then begin
    Inc(fHistoryIndex);
    fIsUsingHistory:=True;
    HistoryChm := TChmReader(fHistory.Objects[fHistoryIndex]);
    fChms.ObjectExists(fHistory.Strings[fHistoryIndex], HistoryChm); // this ensures that the correct chm will be found
    fHtml.OpenURL(fHistory.Strings[fHistoryIndex]);
  end;
end;

class function TChmContentProvider.GetProperContentProvider(const AURL: String
  ): TBaseContentProviderClass;
begin
  Result:=TChmContentProvider;
end;

constructor TChmContentProvider.Create(AParent: TWinControl; AImageList: TImageList);
const
  TAB_WIDTH = 215;
begin
  inherited Create(AParent, AImageList);

  fHistory := TStringList.Create;

  fTabsControl := TPageControl.Create(AParent);
  with fTabsControl do begin
    Width := TAB_WIDTH + 12;
    Align := alLeft;
    Parent := AParent;
    Visible := True;
  end;

  fContentsTab := TTabSheet.Create(fTabsControl);
  with fContentsTab do begin
    Caption := 'Contents';
    Parent := fTabsControl;
    //BorderSpacing.Around := 6;
  end;
  fContentsPanel := TPanel.Create(fContentsTab);
  with fContentsPanel do begin
    Parent := fContentsTab;
    Align := alClient;
    BevelOuter := bvNone;
    Caption := '';
    Visible := True;
  end;
  fContentsTree := TTreeView.Create(fContentsPanel);
  with fContentsTree do begin
    Parent := fContentsPanel;
    Align := alClient;
    BorderSpacing.Around := 6;
    ReadOnly := True;
    Visible := True;
    OnSelectionChanged := @ContentsTreeSelectionChanged;
    OnExpanded := @TOCExpand;
    OnCollapsed := @TOCCollapse;
    OnCreateNodeClass:=@GetTreeNodeClass;
    Images := fImageList;
    //StateImages := fImageList;
  end;

  fIndexTab := TTabSheet.Create(fTabsControl);
  with fIndexTab do begin
    Caption := 'Index';
    Parent := fTabsControl;
    //BorderSpacing.Around := 6;
  end;

  fIndexEdit := TLabeledEdit.Create(fIndexTab);
  with fIndexEdit do begin
    Parent := fIndexTab;
    Anchors := [akLeft, akRight, akTop];
    BorderSpacing.Around := 6;
    AnchorSide[akLeft].Control := fIndexTab;
    AnchorSide[akRight].Control := fIndexTab;
    AnchorSide[akRight].Side := asrBottom;
    AnchorSide[akTop].Control := fIndexTab;
    EditLabel.Caption := 'Search';
    EditLabel.AutoSize := True;
    LabelPosition := lpAbove;
    OnChange := @SearchEditChange;
    Visible := True;
  end;

  fIndexView := TTreeView.Create(fIndexTab);
  with fIndexView do begin
    Anchors := [akLeft, akTop, akRight, akBottom];
    BorderSpacing.Around := 6;
    AnchorSide[akLeft].Control := fIndexTab;
    AnchorSide[akRight].Control := fIndexTab;
    AnchorSide[akRight].Side := asrBottom;
    AnchorSide[akTop].Control := fIndexEdit;
    AnchorSide[akTop].Side := asrBottom;
    AnchorSide[akBottom].Control := fIndexTab;
    AnchorSide[akBottom].Side := asrBottom;
    Parent := fIndexTab;
    BorderSpacing.Around := 6;
    ReadOnly := True;
    Visible := True;
    ShowButtons:=False;
    ShowLines:=False;
    ShowRoot:=False;
    OnCollapsing:=@TreeViewStopCollapse;
    OnDblClick := @IndexViewDblClick;
    OnCreateNodeClass:=@GetTreeNodeClass;
  end;


 // {$IFDEF CHM_SEARCH}
  fSearchTab := TTabSheet.Create(fTabsControl);
  with fSearchTab do begin
    Caption := 'Search';
    Parent := fTabsControl;

  end;
  fKeywordLabel := TLabel.Create(fSearchTab);
  with fKeywordLabel do begin
    Parent := fSearchTab;
    Top := 6;
    Caption := 'Keyword:';
    Left := 6;
    AutoSize := True;
  end;
  fKeywordCombo := TComboBox.Create(fSearchTab);
  with fKeywordCombo do begin
    Parent := fSearchTab;
    Anchors := [akLeft, akRight, akTop];
    BorderSpacing.Around := 6;
    AnchorSide[akLeft].Control := fSearchTab;
    AnchorSide[akRight].Control := fSearchTab;
    AnchorSide[akRight].Side := asrBottom;
    AnchorSide[akTop].Control := fKeywordLabel;
    AnchorSide[akTop].Side := asrBottom;
    OnKeyDown  := @SearchComboKeyDown;
  end;

  fSearchBtn := TButton.Create(fSearchTab);
  with fSearchBtn do begin
    Parent := fSearchTab;
    Anchors := [akLeft, akTop];
    BorderSpacing.Around := 6;
    AnchorSide[akLeft].Control := fSearchTab;
    AnchorSide[akTop].Control := fKeywordCombo;
    AnchorSide[akTop].Side := asrBottom;
    Caption := 'Find';
    OnClick := @SearchButtonClick;
  end;
  fResultsLabel := TLabel.Create(fSearchTab);
  with fResultsLabel do begin
    Parent := fSearchTab;
    Anchors := [akLeft, akTop];
    BorderSpacing.Around := 6;
    AnchorSide[akLeft].Control := fSearchTab;
    AnchorSide[akRight].Control := fSearchTab;
    AnchorSide[akRight].Side := asrBottom;
    AnchorSide[akTop].Control := fSearchBtn;
    AnchorSide[akTop].Side := asrBottom;
    Caption := 'Search Results:';
    AutoSize := True;
  end;
  fSearchResults := TTreeView.Create(fSearchTab);
  with fSearchResults do begin
    Parent := fSearchTab;
    Anchors := [akLeft, akTop, akRight, akBottom];
    BorderSpacing.Around := 6;
    AnchorSide[akLeft].Control := fSearchTab;
    AnchorSide[akRight].Control := fSearchTab;
    AnchorSide[akRight].Side := asrBottom;
    AnchorSide[akTop].Control := fResultsLabel;
    AnchorSide[akTop].Side := asrBottom;
    AnchorSide[akBottom].Control := fSearchTab;
    AnchorSide[akBottom].Side := asrBottom;
    ReadOnly := True;
    ShowButtons := False;
    ShowLines := False;
    ShowRoot:=False;
    OnDblClick := @SearchResultsDblClick;
    OnCollapsing:=@TreeViewStopCollapse;
    OnCreateNodeClass:=@GetTreeNodeClass;
  end;
 // {$ENDIF}


  fHtml := TIpHtmlPanel.Create(Parent);
  with fHtml do begin
    DataProvider := TIpChmDataProvider.Create(fHtml, fChms);
    TIpChmDataProvider(DataProvider).OnGetHtmlPage:=@LoadingHTMLStream;
    OnDocumentOpen := @IpHtmlPanelDocumentOpen;
    OnHotChange := @IpHtmlPanelHotChange;
    Parent := AParent;
    Align := alClient;
  end;

  fSplitter := TSplitter.Create(Parent);
  with fSplitter do begin
    //Align  := alLeft;

    AnchorSide[akLeft].Control := fTabsControl;
    AnchorSide[akLeft].Side:= asrRight;
    AnchorSide[akRight].Control := fHtml;
    AnchorSide[akRight].Side := asrLeft;
    Parent := AParent;
  end;


  fPopUp := TPopupMenu.Create(fHtml);
  fPopUp.Items.Add(TMenuItem.Create(fPopup));
  with fPopUp.Items.Items[0] do begin
    Caption := 'Copy';
    OnClick := @PopupCopyClick;
  end;
  fHtml.PopupMenu := fPopUp;

  fStatusBar := TStatusBar.Create(AParent);
  with fStatusBar do begin
    Parent := AParent;
    Align := alBottom;
    SimplePanel := True;
  end;
end;

destructor TChmContentProvider.Destroy;
begin
  DoCloseChm;
  fHistory.Free;
  inherited Destroy;
end;

initialization

  RegisterFileType('.chm', TChmContentProvider);

end.

