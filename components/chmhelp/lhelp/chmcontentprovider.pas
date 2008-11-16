unit chmcontentprovider;

{$mode objfpc}{$H+}

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
        fIndexEdit: TEdit;
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
    fPendingChm: TChmReader;
  protected
    fIsUsingHistory: Boolean;
    fChms: TChmFileList;
    fHistory: TStringList;
    fHistoryIndex: Integer;
    fStopTimer: Boolean;
    fFillingToc: Boolean;

    procedure AddHistory(URL: String);
    procedure DoOpenChm(AFile: String);
    procedure DoCloseChm;
    procedure DoLoadContext(Context: THelpContext);
    procedure DoLoadUrl(Url: String; AChm: TChmReader = nil);
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
    procedure SearchButtonClick(Sender: TObject);
    procedure SearchResultsDblClick(Sender: TObject);
  public
    function CanGoBack: Boolean; override;
    function CanGoForward: Boolean; override;
    function GetHistory: TStrings; override;
    function LoadURL(const AURL: String; const AContext: THelpContext=-1): Boolean; override;
    procedure GoHome; override;
    procedure GoBack; override;
    procedure GoForward; override;
    class function GetProperContentProvider(const AURL: String): TBaseContentProviderClass; override;
    
    constructor Create(AParent: TWinControl); override;
    destructor Destroy; override;
  end;
  
implementation

uses ChmSpecialParser, chmFIftiMain;

{ TChmContentProvider }

procedure TChmContentProvider.AddHistory(URL: String);
begin
  if fHistoryIndex < fHistory.Count then begin
    while fHistory.Count-1 > fHistoryIndex do
      fHistory.Delete(fHistory.Count-1);
  end;
  fHistory.Add(URL);
  Inc(fHistoryIndex);
end;

procedure TChmContentProvider.DoOpenChm(AFile: String);
begin
  if (fChms <> nil) and fChms.IsAnOpenFile(AFile) then Exit;
  DoCloseChm;
  if not FileExistsUTF8(AFile) or DirectoryExistsUTF8(AFile) then
  begin
    Exit;
  end;
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
  if fChms = nil then Exit;
  fChms.OnOpenNewFile := @NewChmOpened;
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
  if Str <> '' then DoLoadUrl(Str);
end;

procedure TChmContentProvider.DoLoadUrl(Url: String; AChm: TChmReader = nil);
begin
  if (fChms = nil) and (AChm = nil) then exit;
  if fChms.ObjectExists(Url, AChm) = 0 then begin
    fStatusBar.SimpleText := URL + ' not found!';
    Exit;
  end;
  fIsUsingHistory := True;
  fHtml.OpenURL(Url);
  TIpChmDataProvider(fHtml.DataProvider).CurrentPath := ExtractFileDir(URL)+'/';
  AddHistory(Url);
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
    fPendingChm := ChmFileList.Chm[Index];
    if fContext > -1 then begin
      DoLoadContext(fContext);
      fContext := -1;
    end
    else if ChmFileList.Chm[Index].DefaultPage <> '' then begin
      DoLoadUrl(ChmFileList.Chm[Index].DefaultPage);
    end;
  end;
  if ChmFileList.Chm[Index].Title = '' then
    ChmFileList.Chm[Index].Title := ExtractFileName(ChmFileList.FileName[Index]);
  // Fill the table of contents. This actually works very well
  fContentsTree.Visible := False;
end;

procedure TChmContentProvider.FillTOC(Data: PtrInt);
var
 Stream: TMemoryStream;
 fChm: TChmReader;
 ParentNode: TTreeNode;
begin
  if fFillingToc = True then begin
    Application.QueueAsyncCall(@FillToc, Data);
    exit;
  end;
  fFillingToc := True;
  fContentsTree.Visible := False;
  fChm := TChmReader(Data);
  writeln('Start: ',FormatDateTime('hh:nn:ss.zzz', Now));
  if fChm <> nil then begin
    ParentNode := fContentsTree.Items.AddChildObject(nil, fChm.Title, fChm);
    Stream := TMemoryStream(fchm.GetObject(fChm.TOCFile));
    if Stream <> nil then begin
      Stream.position := 0;
      writeln('Stream read: ',FormatDateTime('hh:nn:ss.zzz', Now));
      with TContentsFiller.Create(fContentsTree, Stream, @fStopTimer) do begin
        DoFill(ParentNode);
        Free;
      end;
    end;
    Stream.Free;
    // we fill the index here too
    Stream := fchms.GetObject(fChm.IndexFile);
    if Stream <> nil then begin
      Stream.position := 0;
      with TIndexFiller.Create(fIndexView, Stream) do begin;
        DoFill;
        Free;
      end;
      Stream.Free;
    end;
  end;
  if ParentNode.Index = 0 then ParentNode.Expanded := True;



  fContentsTree.Visible := True;
  writeln('Eind: ',FormatDateTime('hh:nn:ss.zzz', Now));
  fFillingToc := False;
end;

procedure TChmContentProvider.IpHtmlPanelDocumentOpen(Sender: TObject);
var
  AChm: TChmReader;
begin
   // StatusBar1.Panels.Items[1] := fHtml.DataProvider.;
 if fIsUsingHistory = False then
   AddHistory(TIpChmDataProvider(fHtml.DataProvider).CurrentPage)
 else fIsUsingHistory := False;
 if fPendingChm<>nil then
 begin
   AChm := fPendingChm;
   fPendingChm := nil;
   Application.QueueAsyncCall(@FillToc, PtrInt(AChm));
 end;
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
      DoLoadUrl(fChm.DefaultPage, fChm);
    Exit;
  end;
  ATreeNode := TContentTreeNode(fContentsTree.Selected);

  //find the chm associated with this branch
  ARootNode := ATreeNode.Parent;
  while ARootNode.Parent <> nil do
    ARootNode := ARootNode.Parent;

  fChm := TChmReader(ARootNode.Data);
  if ATreeNode.Url <> '' then begin
    DoLoadUrl(ATreeNode.Url, fChm);
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
    DoLoadUrl(RealItem.Url);
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
  TitleIndex: Integer = -1;
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
  fSearchResults.SortColumn := 1;
  fSearchResults.SortType := stNone;
  fSearchResults.SortType := stText;
  fSearchResults.SortColumn := 2;
  fSearchResults.SortType := stNone;
  fSearchResults.SortType := stText;
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

  DoLoadUrl(Item.SubItems[2], TChmReader(Item.Data));
end;


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
begin
  Result := False;
  fFile := Copy(AUrl,8, Length(AURL));
  fPos := Pos('://', fFile);
  if fPos > 0 then begin
    fURL := Copy(fFile, fPos+3, Length(fFIle));
    fFile := Copy(fFIle, 1, fPos-1);
  end;
  //writeln(fURL);
  //if fChms = nil then
  DoOpenChm(fFile);
  if fURL <> '' then
    DoLoadUrl(fUrl)
  else
    GoHome;
  Result := True;
end;

procedure TChmContentProvider.GoHome;
begin
  if (fChms <> nil) and (fChms.Chm[0].DefaultPage <> '') then begin
    DoLoadUrl(fChms.Chm[0].DefaultPage);
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
begin
  if CanGoForward then begin
    Inc(fHistoryIndex);
    fIsUsingHistory:=True;
    fHtml.OpenURL(fHistory.Strings[fHistoryIndex]);
  end;
end;

class function TChmContentProvider.GetProperContentProvider(const AURL: String
  ): TBaseContentProviderClass;
begin
  Result:=TChmContentProvider;
end;

constructor TChmContentProvider.Create(AParent: TWinControl);
begin
  inherited Create(AParent);

  fHistory := TStringList.Create;

  fTabsControl := TPageControl.Create(AParent);
  with fTabsControl do begin
    Width := 215;
    Align := alLeft;
    Parent := AParent;
    Visible := True;
  end;

  fContentsTab := TTabSheet.Create(fTabsControl);
  with fContentsTab do begin
    Caption := 'Contents';
    Parent := fTabsControl;
    Visible := True;
  end;
  fContentsPanel := TPanel.Create(fContentsTab);
  with fContentsPanel do begin
    Parent := fContentsTab;
    Align := alClient;
    Caption := 'Table of Contents Loading. Please Wait...';
    Visible := True;
  end;
  fContentsTree := TTreeView.Create(fContentsPanel);
  with fContentsTree do begin
    Parent := fContentsPanel;
    Align := alClient;
    Visible := True;
    OnSelectionChanged := @ContentsTreeSelectionChanged;
  end;

  fIndexTab := TTabSheet.Create(fTabsControl);
  with fIndexTab do begin
    Caption := 'Index';
    Parent := fTabsControl;
    Visible := True;
  end;

  with TLabel.Create(fIndexTab) do begin
    Caption := 'Search';
    Align := alTop;
    Parent := fIndexTab;
    Top := 0;
    Visible := True;
  end;

  fIndexEdit := TEdit.Create(fIndexTab);
  with fIndexEdit do begin
    Parent := fIndexTab;
    Top := 10;
    Align := alTop;
    Visible := True;
    BorderSpacing.Bottom := 15;
    OnChange := @SearchEditChange;
  end;
  fIndexView := TListView.Create(fIndexTab);
  with fIndexView do begin
    Parent := fIndexTab;
    Align := alClient;
    Visible := True;
    OnDblClick := @IndexViewDblClick;
  end;

  fSearchTab := TTabSheet.Create(fTabsControl);
  with fSearchTab do begin
    Caption := 'Search';
    Parent := fTabsControl;
    Visible := True;
    BorderSpacing.Around := 3;
  end;
  fKeywordLabel := TLabel.Create(fSearchTab);
  with fKeywordLabel do begin
    Parent := fSearchTab;
    Top := 5;
    Caption := 'Keyword:';
    Left := 0;
    AutoSize := True;
  end;
  fKeywordCombo := TComboBox.Create(fSearchTab);
  with fKeywordCombo do begin
    Parent := fSearchTab;
    Top := fKeywordLabel.Top + fKeywordLabel.Height + 5;
    Left := 5;
    Width := fSearchTab.ClientWidth - Left;
    Anchors := [akLeft, akRight, akTop];
  end;
  fSearchBtn := TButton.Create(fSearchTab);
  with fSearchBtn do begin
     Parent := fSearchTab;
     Top := fKeywordCombo.Top + fKeywordCombo.Height + 5;
     Width := 105;
     Left := fSearchTab.ClientWidth - Width;
     Anchors := [akTop, akRight];
     Caption := 'Find';
     OnClick := @SearchButtonClick;
  end;
  fResultsLabel := TLabel.Create(fSearchTab);
  with fResultsLabel do begin
    Parent := fSearchTab;
    Top := fSearchBtn.Top + fSearchBtn.Height + 15;
    Caption := 'Search Results:';
    Left := 0;
    AutoSize := True;
  end;
  fSearchResults := TListView.Create(fSearchTab);
  with fSearchResults do begin
    Parent := fSearchTab;
    Top := fResultsLabel.Top + fResultsLabel.Height + 5;
    //Width := fSearchTab.Width - (Left * 2);
    Height := fSearchTab.ClientHeight - Top;
    Anchors := [akTop];
    Align := alBottom;
    ShowColumnHeaders := False;
    ViewStyle := vsReport;
    Columns.Add;                  // title
    Columns.Add.Visible := False; // topic hits
    Columns.Add.Visible := False; // title hits
    Columns.Add.Visible := False; // url
    OnDblClick := @SearchResultsDblClick;
  end;

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

