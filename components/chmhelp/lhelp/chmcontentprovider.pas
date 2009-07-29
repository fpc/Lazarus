unit chmcontentprovider;

{$mode objfpc}{$H+}

{$if (fpc_version=2) and (fpc_release>2) ((fpc_version=2) and (fpc_release=2) and (fpc_patch>2))}
{$Note Compiling lhelp with search support}
{$DEFINE CHM_SEARCH}
{$else}
{$Note Compiling lhelp *without* search support since your fpc version is not new enough}
{$endif}

{off $DEFINE CHM_DEBUG_TIME}
{off $DEFINE CHM_SEARCH}

interface

uses
  Classes, SysUtils,
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
        fIndexView: TListView;
      fSearchTab: TTabSheet;
        fKeywordLabel: TLabel;
        fKeywordCombo: TComboBox;
        fSearchBtn: TButton;
        fResultsLabel: TLabel;
        fSearchResults: TListView;
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

    function  MakeURI(AUrl: String; AChm: TChmReader): String;

    procedure AddHistory(URL: String);
    procedure DoOpenChm(AFile: String; ACloseCurrent: Boolean = True);
    procedure DoCloseChm;
    procedure DoLoadContext(Context: THelpContext);
    procedure DoLoadUri(Uri: String; AChm: TChmReader = nil);
    procedure DoError(Error: Integer);
    procedure NewChmOpened(ChmFileList: TChmFileList; Index: Integer);

    procedure FillTOC(Data: PtrInt);
    procedure IpHtmlPanelDocumentOpen(Sender: TObject);
    procedure IpHtmlPanelHotChange(Sender: TObject);
    procedure PopupCopyClick(Sender: TObject);
    procedure ContentsTreeSelectionChanged(Sender: TObject);
    procedure IndexViewDblClick(Sender: TObject);
    procedure ViewMenuContentsClick(Sender: TObject);
    procedure SetTitle(const ATitle: String);
    procedure SearchEditChange(Sender: TObject);
    procedure TOCExpand(Sender: TObject; Node: TTreeNode);
    procedure TOCCollapse(Sender: TObject; Node: TTreeNode);
    {$IFDEF CHM_SEARCH}
    procedure SearchButtonClick(Sender: TObject);
    procedure SearchResultsDblClick(Sender: TObject);
    {$ENDIF}
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

uses ChmSpecialParser{$IFDEF CHM_SEARCH}, chmFIftiMain{$ENDIF};

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

  //FileMenuCloseItem.Enabled := True;
  if fChms.Chm[0].Title <> '' then SetTitle(fChms.Chm[0].Title);
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
begin
  if (fChms = nil) and (AChm = nil) then exit;

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
  {WriteLn(iphtml.Aspect);
  iphtml.ScaleFonts := True;
  iphtml.Aspect := 2;}
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
    fContentsTree.Items.Clear;
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
    Application.QueueAsyncCall(@FillToc, PtrInt(ChmFileList.Chm[Index]));
end;

procedure TChmContentProvider.FillTOC(Data: PtrInt);
var
 Stream: TMemoryStream;
 fChm: TChmReader;
 ParentNode: TTreeNode;
 i: Integer;
 HasSearchIndex: Boolean = False;
begin
  if fFillingToc = True then begin
    Application.QueueAsyncCall(@FillToc, Data);
    exit;
  end;
  fFillingToc := True;
  fContentsTree.Visible := False;
  Application.ProcessMessages;
  fChm := TChmReader(Data);
  {$IFDEF CHM_DEBUG_TIME}
  writeln('Start: ',FormatDateTime('hh:nn:ss.zzz', Now));
  {$ENDIF}
  if fChm <> nil then begin
    ParentNode := fContentsTree.Items.AddChildObject(nil, fChm.Title, fChm);
    ParentNode.ImageIndex := 0;
    ParentNode.SelectedIndex := 0;
    Stream := TMemoryStream(fchm.GetObject(fChm.TOCFile));
    if Stream <> nil then begin
      Stream.position := 0;
      {$IFDEF CHM_DEBUG_TIME}
      writeln('Stream read: ',FormatDateTime('hh:nn:ss.zzz', Now));
      {$ENDIF}
      with TContentsFiller.Create(fContentsTree, Stream, @fStopTimer, fChm) do begin
        DoFill(ParentNode);
        Free;
      end;
      Stream.Free;
    end;
    fContentsTab.TabVisible := fContentsTree.Items.Count > 1;

    // we fill the index here too but only for the main file
    if fChms.IndexOfObject(fChm) < 1 then
    begin
      Stream := fchms.GetObject(fChm.IndexFile);
      if Stream <> nil then begin
        Stream.position := 0;
        with TIndexFiller.Create(fIndexView, Stream, fChm) do begin;
          DoFill;
          Free;
        end;
        Stream.Free;
      end;
    end;
  end;
  fIndexTab.TabVisible := fIndexView.Items.Count > 0;
  if ParentNode.Index = 0 then ParentNode.Expanded := True;

  fContentsTree.Visible := True;
  {$IFDEF CHM_DEBUG_TIME}
  writeln('Eind: ',FormatDateTime('hh:nn:ss.zzz', Now));
  {$ENDIF}
  fFillingToc := False;

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
begin
  if (fContentsTree.Selected = nil) then Exit;
  if not(fContentsTree.Selected is TContentTreeNode) then
  begin
    fChm := TChmReader(fContentsTree.Selected.Data);
    if fChm.DefaultPage <> '' then
      DoLoadUri(MakeURI(fChm.DefaultPage, fChm));
    Exit;
  end;
  ATreeNode := TContentTreeNode(fContentsTree.Selected);

  //find the chm associated with this branch
  ARootNode := ATreeNode.Parent;
  while ARootNode.Parent <> nil do
    ARootNode := ARootNode.Parent;

  fChm := TChmReader(ARootNode.Data);
  if ATreeNode.Url <> '' then begin
    DoLoadUri(MakeURI(ATreeNode.Url, fChm));
  end;
end;

procedure TChmContentProvider.IndexViewDblClick(Sender: TObject);
var
  SelectedItem: TListItem;
  RealItem: TIndexItem;
begin
  SelectedItem := fIndexView.Selected;
  if SelectedItem = nil then Exit;

  RealItem := TIndexItem(SelectedItem);
  if not fIndexEdit.Focused then
    fIndexEdit.Text := Trim(RealItem.Caption);
  if RealItem.Url <> '' then begin
    DoLoadUri(MakeURI(RealItem.Url, TChmReader(RealItem.Data)));
  end;
end;

procedure TChmContentProvider.ViewMenuContentsClick(Sender: TObject);
begin
  //TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  //fSplitter.Visible := TMenuItem(Sender).Checked;
  //TabPanel.Visible := Splitter1.Visible;
end;

procedure TChmContentProvider.SetTitle(const ATitle: String);
begin
  if fHtml.Parent = nil then exit;
  TTabSheet(fHtml.Parent).Caption := ATitle;
end;

procedure TChmContentProvider.SearchEditChange(Sender: TObject);
var
  I: Integer;
  ItemName: String;
  SearchText: String;
begin
  if fIndexEdit.Text = '' then Exit;
  if not fIndexEdit.Focused then Exit;
  SearchText := LowerCase(fIndexEdit.Text);
  for I := 0 to fIndexView.Items.Count-1 do begin
    ItemName := LowerCase(Copy(fIndexView.Items.Item[I].Caption, 1, Length(SearchText)));
    if ItemName = SearchText then begin
      fIndexView.Items.Item[fIndexView.Items.Count-1].MakeVisible(False);
      fIndexView.Items.Item[I].MakeVisible(False);
      fIndexView.Items.Item[I].Selected := True;
      Exit;
    end;
  end;
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

{$IFDEF CHM_SEARCH}
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
  ListItem: TListItem;
begin
  SearchWords := TStringList.Create;
  SearchWords.Delimiter := ' ';
  Searchwords.DelimitedText := fKeywordCombo.Text;
  fSearchResults.BeginUpdate;
  fSearchResults.Clear;
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

    // now lookup titles and urls to add to final search results
    for j := 0 to High(FoundTopics) do
    begin
    try
      DocURL := fChms.Chm[i].LookupTopicByID(FoundTopics[j].Topic, DocTitle);
      //WriteLn(Docurl);
      if DocTitle = '' then
        Doctitle := 'untitled';
      ListItem := fSearchResults.Items.Add;
      ListItem.Caption := DocTitle;
      ListItem.Data := fChms.Chm[i];
      ListItem.SubItems.Add(IntToStr(FoundTopics[j].Hits));
      ListItem.SubItems.Add(IntToStr(FoundTopics[j].TitleHits));
      ListITem.SubItems.Add(DocURL);
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
    ListItem := fSearchResults.Items.Add;
    ListItem.Caption := 'No Results';
  end;
 { fSearchResults.SortColumn := 1;        // causes the listview data to be mixed up!
  fSearchResults.SortType := stNone;
  fSearchResults.SortType := stText;
  fSearchResults.SortColumn := 2;
  fSearchResults.SortType := stNone;
  fSearchResults.SortType := stText;}
  fSearchResults.EndUpdate;
  //WriteLn('THE DUDE');
end;

procedure TChmContentProvider.SearchResultsDblClick ( Sender: TObject ) ;
var
  Item: TListItem;
begin
  Item := fSearchResults.Selected;
  if (Item = nil) or (Item.Data = nil) then
    Exit;

  DoLoadUri(MakeURI(Item.SubItems[2], TChmReader(Item.Data)));
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
  //writeln(fURL);
  LoadTOC := (fChms = nil) or (fChms.IndexOf(fFile) < 0);
  DoOpenChm(fFile, False);
  FileIndex := fChms.IndexOf(fFile);
  if fURL <> '' then
    DoLoadUri(MakeURI(fURL, fChms.Chm[FileIndex]))
  else
    GoHome;
  Result := True;

  if LoadTOC and (FileIndex = 0) then
  begin
    Application.ProcessMessages;
    Application.QueueAsyncCall(@FillToc, PtrInt(fChms.Chm[FileIndex]));
  end;
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
    Caption := 'Table of Contents Loading. Please Wait...';
    Visible := True;
  end;
  fContentsTree := TTreeView.Create(fContentsPanel);
  with fContentsTree do begin
    Parent := fContentsPanel;
    Align := alClient;
    BorderSpacing.Around := 6;
    Visible := True;
    OnSelectionChanged := @ContentsTreeSelectionChanged;
    OnExpanded := @TOCExpand;
    OnCollapsed := @TOCCollapse;
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
  fIndexView := TListView.Create(fIndexTab);
  with fIndexView do begin
    Parent := fIndexTab;
    Anchors := [akLeft, akTop, akRight, akBottom];
    BorderSpacing.Around := 6;
    AnchorSide[akLeft].Control := fIndexTab;
    AnchorSide[akRight].Control := fIndexTab;
    AnchorSide[akRight].Side := asrBottom;
    AnchorSide[akTop].Control := fIndexEdit;
    AnchorSide[akTop].Side := asrBottom;
    AnchorSide[akBottom].Control := fIndexTab;
    AnchorSide[akBottom].Side := asrBottom;
    Visible := True;
    OnDblClick := @IndexViewDblClick;
    ReadOnly := True;
  end;

  {$IFDEF CHM_SEARCH}
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
  fSearchResults := TListView.Create(fSearchTab);
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
    ShowColumnHeaders := False;
    {$IFDEF MSWINDOWS}
    ViewStyle := vsReport;
    with Columns.Add do
    begin
      Width := 400;
      AutoSize := True;
    end;
    {$ELSE}
    ViewStyle := vsReport;
    Columns.Add.AutoSize := True; // title
    Columns.Add.Visible := False; // topic hits
    Columns.Add.Visible := False; // title hits
    Columns.Add.Visible := False; // url
    {$ENDIF}
    OnDblClick := @SearchResultsDblClick;
  end;
  {$ENDIF}

  fSplitter := TSplitter.Create(Parent);
  with fSplitter do begin
    Align  := alLeft;
    Parent := AParent
  end;

  fHtml := TIpHtmlPanel.Create(Parent);
  with fHtml do begin
    DataProvider := TIpChmDataProvider.Create(fHtml, fChms);
    OnDocumentOpen := @IpHtmlPanelDocumentOpen;
    OnHotChange := @IpHtmlPanelHotChange;
    Parent := AParent;
    Align := alClient;
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

