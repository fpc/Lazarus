unit chmcontentprovider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, ComCtrls, Controls, Buttons, Menus,
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
        fSearchResults: TListBox;
    fSplitter: TSplitter;
    fHtml: TIpHtmlPanel;
    fPopUp: TPopUpMenu;
    fChmDataProvider: TIpChmDataProvider;
    fStatusBar: TStatusBar;
    fContext: THelpContext;
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
    
    procedure FillTOCTimer(Sender: TObject);
    procedure IpHtmlPanelDocumentOpen(Sender: TObject);
    procedure IpHtmlPanelHotChange(Sender: TObject);
    procedure PopupCopyClick(Sender: TObject);
    procedure ContentsTreeSelectionChanged(Sender: TObject);
    procedure IndexViewDblClick(Sender: TObject);
    procedure ViewMenuContentsClick(Sender: TObject);
    procedure SetTitle(const ATitle: String);
    procedure SearchEditChange(Sender: TObject);
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
  
  TTocTimer = class(TIdleTimer)
  private
    fChm: TChmReader;
  end;


implementation

uses ChmSpecialParser;

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
var
Stream: TStream;
Timer: TTimer;
begin
  if (fChms <> nil) and fChms.IsAnOpenFile(AFile) then Exit;
  DoCloseChm;
  if not FileExists(AFile) or DirectoryExists(AFile) then
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
begin
  fStopTimer := True;
  if fChms<>nil then begin
    FreeAndNil(fChms);
  end;
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
var
TImer: TTocTimer;
begin
  if Index = 0 then begin
    fContentsTree.Items.Clear;
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
  Timer := TTocTimer.Create(fHtml);
  if ChmFileList.ObjectExists(ChmFileList.Chm[Index].TOCFile) + ChmFileList.ObjectExists(ChmFileList.Chm[Index].IndexFile)> 25000 then
    Timer.Interval := 500
  else
    Timer.Interval := 5;
  Timer.OnTimer := @FillTOCTimer;
  Timer.fChm := ChmFileList.Chm[Index];
  Timer.Enabled := True;
  fContentsTree.Visible := False;
end;

procedure TChmContentProvider.FillTOCTimer(Sender: TObject);
var
 Stream: TMemoryStream;
 fChm: TChmReader;
 ParentNode: TTreeNode;
begin
  if fFillingToc = True then begin
    TTimer(Sender).Interval := 40;
    exit;
  end;
  fFillingToc := True;
  fStopTimer := False;
  fContentsTree.Visible := False;
  fChm := TTocTimer(Sender).fChm;
  TTocTimer(Sender).Free;
  if fChm <> nil then begin
    ParentNode := fContentsTree.Items.AddChildObject(nil, fChm.Title, fChm);
    Stream := TMemoryStream(fchm.GetObject(fChm.TOCFile));
    if Stream <> nil then begin
      Stream.position := 0;
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
  fFillingToc := False;
  fStopTimer := False;
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
  //Result:=inherited GetHistory;
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
  {
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
    Left := 0;
    Width := fSearchTab.ClientWidth;
    Anchors := [akLeft, akRight, akTop];
  end;
  fSearchBtn := TButton.Create(fSearchTab);
  with fSearchBtn do begin
     Parent := fSearchTab
     Top := fKeywordCombo.Top + fKeywordCombo.Height + 5;
     Width := 105;
     Left := fSearchTab.ClientWidth - Width;
     Anchors := [akTop, akRight]
  end;
  fResultsLabel := TLabel.Create(fSearchTab);
  with fResultsLabel do begin
    Parent := fSearchTab;
    Top := fSearchBtn.Top + fSearchBtn.Height + 15;
    Caption := 'Search Results:';
    Left := 0;
    AutoSize := True;
  end;
  fSearchResults := TListBox.Create(fSearchTab);
  with fSearchResults do begin
    Parent := fSearchTab;
    Top := fResultsLabel.Top + fResultsLabel.Height + 5;
    Height := fSearchTab.ClientHeight - Top;
    Anchors := [akTop, akBottom, akLeft, akRight];
  end;
  }

  fSplitter := TSplitter.Create(Parent);
  with fSplitter do begin
    Align  := alLeft;
    Parent := AParent
  end;
  
  fPopUp := TPopupMenu.Create(fHtml);
  fPopUp.Items.Add(TMenuItem.Create(fPopup));
  with fPopUp.Items.Items[0] do begin
    Caption := 'Copy';
    OnClick := @PopupCopyClick;
  end;
  
  fChmDataProvider := TIpChmDataProvider.Create(fChms);
  fHtml := TIpHtmlPanel.Create(Parent);
  with fHtml do begin
    DataProvider := fChmDataProvider;
    OnDocumentOpen := @IpHtmlPanelDocumentOpen;
    OnHotChange := @IpHtmlPanelHotChange;
    PopupMenu := fPopUp;
    Parent := AParent;
    Align := alClient;
  end;
  
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
  inherited Destroy;
end;

initialization

  RegisterFileType('.chm', TChmContentProvider);

end.

