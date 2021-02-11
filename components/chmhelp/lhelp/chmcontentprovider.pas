{
  Graphical CHM help content provider.
  Responsible for loading TOC, providing search etc.
}

unit ChmContentProvider;

{$mode objfpc}{$H+}

{$Note Compiling lhelp with search support}
{$DEFINE CHM_SEARCH}

{$IF FPC_FULLVERSION>=20400}
{$Note Compiling lhelp *with* binary index and toc support}
// CHMs can have both binary and text Table of Contents and index
{$DEFINE CHM_BINARY_INDEX_TOC}    // internal chm index else external file`s indexes
{$endif}

{off $DEFINE CHM_DEBUG_TIME}

interface

uses
  Classes, SysUtils, ChmReader,
  // LCL
  LCLIntf, Forms, StdCtrls, ExtCtrls, ComCtrls, Controls, Menus,
  // LazUtils
  LazFileUtils, LazStringUtils, LazUTF8, Laz2_XMLCfg, LazLoggerBase,
  // Turbopower IPro
  IpHtml,
  // ChmHelp
  BaseContentProvider, FileContentProvider, ChmDataProvider, lhelpstrconsts;

const
  DefaultCHMContentTitle = '[unknown]';

type

  TAsyncIndexData = record
    CHMReader: TChmReader;
    isUpdate: Boolean;
  end;
  PTAsyncIndexData = ^TAsyncIndexData;

  TAsyncUri = record
    CHMReader: TChmReader;
    Uri: String;
  end;
  PTAsyncUri = ^TAsyncUri;

  { TChmContentProvider }

  TChmContentProvider = class(TFileContentProvider)
  private
    fUpdateURI: String; // last request
    fLastURI: String;   // last showed
    fTabsControl: TPageControl;
      fContentsTab: TTabSheet;
       fContentsPanel: TPanel;
         fContentsTree: TTreeView;
      fIndexTab: TTabSheet;
        fIndexEdit: TLabeledEdit;
        fIndexView: TTreeView;
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
    fFillTOCStack: TFPList;
    function GetShowStatusbar: Boolean;
    procedure SetShowStatusbar(AValue: Boolean);
    procedure CompareIndexNodes(Sender: TObject; Node1, Node2: TTreeNode;
                              var Compare: Integer);
    procedure ProcTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ProcKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ProcTreeKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    fIsUsingHistory: Boolean;
    fChms: TChmFileList;
    fChmDataProvider: TIpChmDataProvider;
    fHistory: TStringList;
    fHistoryIndex: Integer;
    fStopTimer: Boolean;
    fActiveChmTitle: String;
    FLoadingSearchURL: Boolean; // use this to try to highlight search terms

    function  MakeURI(Const AUrl: String; AChm: TChmReader): String;

    procedure AddHistory(Const URL: String);
    procedure DoOpenChm(Const AFile: String; ACloseCurrent: Boolean = True);
    procedure DoLoadContext(Context: THelpContext);
    procedure DoLoadUri(Uri: String; AChm: TChmReader = nil);
    procedure DoError({%H-}Error: Integer);
    function  GetChmReader(Const AFile: String): TChmReader;
    procedure NewChmOpened(ChmFileList: TChmFileList; Index: Integer);
    // Set to queue LoadUri processing
    procedure QueueLoadUriAsync(Uri: String; AChm: TChmReader = nil);
    // Set to queue a filling TOC Index for later processing
    procedure QueueFillToc(AChm: TChmReader);
    // Filling TOC and index for the chm file through Async process
    procedure ProcFillTOC(AData: PtrInt);
    // LoadURI through Async process
    procedure ProcLoadUri(UriData: PtrInt);
    procedure LoadingHTMLStream(var AStream: TStream);
    procedure IpHtmlPanelDocumentOpen(Sender: TObject);
    procedure IpHtmlPanelHotChange(Sender: TObject);
    // text and image resource types
    procedure IpHtmlPanelHotClick(Sender: TObject);
    procedure PopupCopyClick(Sender: TObject);
    procedure PopupCopySourceClick(Sender: TObject);

    procedure ContentsTreeSelectionChanged(Sender: TObject);
    procedure TreeViewStopCollapse(Sender: TObject; {%H-}Node: TTreeNode; var AllowCollapse: Boolean);
    procedure TreeViewShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure ViewMenuContentsClick(Sender: TObject);
    procedure UpdateTitle;
    procedure SetTitle(const AValue: String); override;
    procedure SearchEditChange(Sender: TObject);
    procedure TOCExpand(Sender: TObject; Node: TTreeNode);
    procedure TOCCollapse(Sender: TObject; Node: TTreeNode);
    procedure SelectTreeItemFromURL(Const AUrl: String);
    procedure GetTreeNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    {$IFDEF CHM_SEARCH}
    procedure SearchButtonClick(Sender: TObject);
    procedure SearchComboKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    {$ENDIF}
  public
    procedure ProcGlobalKeyUp(var {%H-}Key: Word; {%H-}Shift: TShiftState);overload;
    procedure LoadPreferences(ACfg: TXMLConfig); override;
    procedure SavePreferences(ACfg: TXMLConfig); override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;

    function CanGoBack: Boolean; override;
    function CanGoForward: Boolean; override;
    function GetHistory: TStrings; override;
    function LoadURL(const AURL: String; const AContext: THelpContext=-1): Boolean; override;
    function HasLoadedData(const AUrl: String): Boolean; override;
    procedure GoHome; override;
    procedure GoBack; override;
    procedure GoForward; override;
    procedure ActivateProvider; override;
    procedure ActivateTOCControl; override;
    procedure ActivateIndexControl; override;
    procedure ActivateSearchControl; override;
    // property
    property TabsControl: TPageControl read fTabsControl;
    property Splitter: TSplitter read fSplitter;
    property ShowStatusbar: Boolean read GetShowStatusbar write SetShowStatusbar;

    class function GetProperContentProvider(const {%H-}AURL: String): TBaseContentProviderClass; override;

    constructor Create(AParent: TWinControl; AImageList: TImageList; AUpdateCount: Integer); override;
    destructor Destroy; override;
  end;

implementation

uses
  clipbrd,
  ChmSpecialParser{$IFDEF CHM_SEARCH}, chmFIftiMain{$ENDIF}, chmsitemap,
  LCLType, SAX_HTML, Dom, DOM_HTML, HTMWrite, LConvEncoding;

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
  i, xPos: Integer;
  WordStart, After: TDOMText;
  Span: TDomElement;
  aWord: DOMString;
  Parent: TDomNode;
begin
   Parent := AtextNode.ParentNode;
   for i := 0 to Words.Count-1 do
   begin
     aWord := LowerCase(DOMString(Words[i]));
     xPos := Pos(aWord, LowerCase(ATextNode.TextContent));
     while xpos > 0 do
     begin
       WordStart:= TDOMText(ATextNode).SplitText(xPos-1);
       After := WordStart.SplitText(Length(aWord));
       Span := doc.CreateElement('span');
       // TODO: lHtml don`t perceive background color :(
       Span.SetAttribute('style', DOMString('color:' + Color +
                        ';font-weight:bold;background:lightgray;padding:3px;'));
       Parent.InsertBefore(Span, After);
       Span.AppendChild(WordStart);

       // or we'll keep finding our new node again and again
       ATextNode := After;

       xPos := Pos(aWord, LowerCase(ATextNode.TextContent));
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

function ChmURI(Const AUrl: String; Const AFileName: String): String;
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

function TChmContentProvider.GetShowStatusbar: Boolean;
begin
  Result := fStatusbar.Visible;
end;

procedure TChmContentProvider.SetShowStatusbar(AValue: Boolean);
begin
  fStatusbar.Visible := AValue;
end;

procedure TChmContentProvider.CompareIndexNodes(Sender: TObject; Node1,
  Node2: TTreeNode; var Compare: Integer);
begin
  Compare:= UTF8CompareLatinTextFast(Node1.Text, Node2.Text);
end;

procedure TChmContentProvider.ProcTreeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if  Sender is TTreeView then
  begin
    if (Key = VK_RETURN) and (Shift = []) then
    begin
      ContentsTreeSelectionChanged(Sender);
      key:= 0;
    end
  end;
  if ((Sender is TTreeView) or (Sender is TIpHtmlPanel)) and (Shift = [ssAlt]) then
  case Key of
    VK_Left: begin
      GoBack; key:= 0;
    end;
    VK_RIGHT: begin
      GoForward; key:= 0;
    end;
    VK_Home: begin
      GoHome; key:= 0;
    end;
  end;
end;

procedure TChmContentProvider.ProcKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift <> []) then Exit;
  if (Sender is TLabeledEdit) and (Sender = fIndexEdit) then
  begin
    if ((Key = VK_DOWN) and ( fIndexView.Items.Count >0 )) then
    begin
      fIndexView.SetFocus();
      if (fIndexView.Selected = nil) then
      begin
        fIndexView.Items.GetFirstNode().MakeVisible;
        fIndexView.Items.GetFirstNode().Selected:=True;
      end;
      Key:= 0;
    end;
  end;
end;

procedure TChmContentProvider.ProcTreeKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Sender is TTreeView then
  begin
    if ((Key = VK_DOWN) or (Key = VK_UP)) and (Shift = []) then
    begin
      ContentsTreeSelectionChanged(Sender);
      Key:= 0;
    end;
  end;
end;

function TChmContentProvider.MakeURI ( const AUrl: String; AChm: TChmReader
  ) : String;
var
  ChmIndex: Integer;
begin
  ChmIndex := fChms.IndexOfObject(AChm);
  Result := ChmURI(AUrl, fChms.FileName[ChmIndex]);
end;

procedure TChmContentProvider.BeginUpdate;
begin
  if not isUpdate then
  begin
    fContentsTree.BeginUpdate;
    fIndexView.BeginUpdate;
  end;
  inherited BeginUpdate;
end;

procedure TChmContentProvider.EndUpdate;
begin
  inherited EndUpdate;
  if not isUpdate then
  begin
    fContentsTree.EndUpdate;
    fIndexView.EndUpdate;
    fContentsPanel.Caption := '';
    fContentsTree.Visible := True;
    UpdateTitle;
  end;
end;

procedure TChmContentProvider.AddHistory ( const URL: String ) ;
begin
  if fHistoryIndex < fHistory.Count then
  begin
    while fHistory.Count-1 > fHistoryIndex do
      fHistory.Delete(fHistory.Count-1);
  end;

  fHistory.Add(URL);
  Inc(fHistoryIndex);
end;

procedure TChmContentProvider.DoOpenChm ( const AFile: String;
  ACloseCurrent: Boolean ) ;
begin
  fChmDataProvider.DoOpenChm(AFile, ACloseCurrent);
 //DebugLn('CHP DoOpenChm() Chm file: ', AFile);
  fHistoryIndex := -1;
  fHistory.Clear;

  // Code here has been moved to the OpenFile handler
end;

procedure TChmContentProvider.DoLoadContext(Context: THelpContext);
var
 Str: String;
begin
  if fChms = nil then exit;
  Str := fChms.Chm[0].GetContextUrl(Context);
  if Str <> '' then DoLoadUri(Str, fChms.Chm[0]);
end;

procedure TChmContentProvider.QueueLoadUriAsync(Uri: String; AChm: TChmReader = nil);
var
  AUriData:PTAsyncUri;
begin
  // https://www.freepascal.org/docs-html/rtl/system/initialize.html
  {$IFDEF DEBUGASYNC}
  DebugLn('CHP QueueLoadUriAsync() URI: ', Uri);
  {$ENDIF}
  GetMem(AUriData,SizeOf(TAsyncUri));
  Initialize(AUriData^);
  AUriData^.CHMReader:= AChm;
  AUriData^.Uri:= Uri;
  Application.ProcessMessages;
  Application.QueueAsyncCall(@ProcLoadUri, PtrInt(AUriData));
end;

procedure TChmContentProvider.DoLoadUri(Uri: String; AChm: TChmReader = nil);
var
  ChmIndex: Integer;
  NewUrl: String;
  FilteredURL: String;
  xPos: Integer;
  StartTime: TDateTime;
  EndTime: TDateTime;
  Time: String;
begin
  if (fChms = nil) and (AChm = nil) then exit;
  fStatusBar.SimpleText := Format(slhelp_Loading, [Uri]);

  StartTime := Now;

  xPos := Pos('#', Uri);
  if xPos > 0 then
    FilteredURL := Copy(Uri, 1, xPos -1)
  else
    FilteredURL := Uri;
  {$IFDEF LDEBUG}
  DebugLn('CHP DoLoadUri() LastURI: '+ fLastURI);
  {$ENDIF}
  if fChms.ObjectExists(FilteredURL, AChm) = 0 then
  begin
    fStatusBar.SimpleText := Format(slhelp_NotFound, [URI]);
    {$IFDEF LDEBUG}
    DebugLn('CHP ERR Chm object is not found - URI: '+ Uri);
    {$ENDIF}
    Exit;
  end;
  if (Pos('ms-its', Uri) = 0) and (AChm <> nil) then
  begin
    ChmIndex := fChms.IndexOfObject(AChm);
    NewUrl := ExtractFileName(fChms.FileName[ChmIndex]);
    NewUrl := 'ms-its:'+NewUrl+'::/'+Uri;
    Uri := NewUrl;
  end;
  Application.ProcessMessages;
  // Already showed

  if fLastURI = Uri then Exit;

  if not isUpdate then
  begin

    fIsUsingHistory := True;
    fChmDataProvider.CurrentPath := ExtractFileDir(URI)+'/';
    {$IFDEF LDEBUG}
    DebugLn('CHP OpenURL URI: '+ Uri);
    {$ENDIF}
    fHtml.BeginUpdateBounds;
    fLastURI:= ''; // TODO: for check it
    fHtml.OpenURL(Uri);
    fUpdateURI:= '';
    fHtml.EndUpdateBounds;
    if Assigned(OnContentComplete) then
      OnContentComplete(Self);

    AddHistory(Uri);
    EndTime := Now;

    Time := INtToStr(DateTimeToTimeStamp(EndTime).Time - DateTimeToTimeStamp(StartTime).Time);
    fStatusBar.SimpleText := Format(slhelp_LoadedInMs, [Uri, Time]);

  end
  else if isUpdateLast then
  begin
    // Do nothing, save URI and use Show for execute request
    fUpdateURI:= Uri;
    // Used to async load URL before enable of Updating
    // QueueLoadUriAsync(Uri, AChm);
    {$IFDEF UPDATE_CNT}
    DebugLn('Lastupdate URI: '+ Uri);
    {$ENDIF}
  end;

end;

procedure TChmContentProvider.ProcLoadUri(UriData: PtrInt);
var
  AUriData: PTAsyncUri;
begin
  AUriData:= PTAsyncUri(UriData);
  {$IFDEF DEBUGASYNC}
  DebugLn('CHP ProcLoadUri() URI: ', AUriData^.Uri);
  {$ENDIF}
  fHtml.BeginUpdateBounds;
  fHtml.OpenURL(AUriData^.Uri);
  fHtml.EndUpdateBounds;
  Finalize(AUriData^);
  FreeMem(AUriData);
end;


procedure TChmContentProvider.DoError(Error: Integer);
begin
  //what to do with these errors?
  //INVALID_FILE_TYPE;
end;

function TChmContentProvider.GetChmReader ( const AFile: String ) : TChmReader;
var
  FileIndex : Integer;
begin
  Result := nil;
  if fChms = nil then Exit;
  FileIndex := fChms.IndexOf(AFile);
  if (fChms <> nil) and (FileIndex >= 0) then
    Result := fChms.Chm[fChms.IndexOf(AFile)];
end;

procedure TChmContentProvider.NewChmOpened(ChmFileList: TChmFileList;
  Index: Integer);
begin
  if ChmFileList.Chm[Index].Title = '' then
    ChmFileList.Chm[Index].Title := ExtractFileName(ChmFileList.FileName[Index]);

  // Fill the table of contents.
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

  Doc:=nil;
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
    except
      UseOrigStream := True;
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
var
  AData:PTAsyncIndexData;
begin
  fContentsTree.Visible := False;
  fContentsPanel.Caption := slhelp_TableOfContentsLoadingPleaseWait;
  fStatusBar.SimpleText := slhelp_TableOfContentsLoading;

  AData:= New(PTAsyncIndexData);
  AData^.CHMReader:= AChm;
  AData^.isUpdate:= self.isUpdate; // save state for Async process

  Application.ProcessMessages;
  Application.QueueAsyncCall(@ProcFillTOC, PtrInt(AData));
end;

procedure TChmContentProvider.ProcFillTOC(AData: PtrInt);
var
  CHMReader: TChmReader;
  ParentNode: TTreeNode;
  i: Integer;
  StackIdx: Integer;
  SM: TChmSiteMap;
  HasSearchIndex: Boolean = False;
  {$IFNDEF CHM_BINARY_INDEX_TOC}
  Stream: TMemoryStream;
  {$ENDIF}
begin
  SM := nil;
  CHMReader := PTAsyncIndexData(AData)^.CHMReader;
  try
    BeginUpdate;
    StackIdx := fFillTOCStack.IndexOf(CHMReader);
    if StackIdx > 0 then Exit;

    fFillTOCStack.Add(CHMReader);
    {$IFDEF CHM_DEBUG_TIME}
    DebugLn('CHT CHM Title: '+CHMReader.Title);
    DebugLn('CHT Start of load: ',FormatDateTime('hh:nn:ss.zzz', Now));
    {$ENDIF}
    if CHMReader <> nil then
    begin
      ParentNode := fContentsTree.Items.AddChildObject(nil, CHMReader.Title, CHMReader);
      ParentNode.ImageIndex := 0;
      ParentNode.SelectedIndex := 0;
      {$IFDEF CHM_BINARY_INDEX_TOC}
      // GetTOCSitemap first tries binary TOC but falls back to text if needed
      {$IFDEF CHM_DEBUG_INDEX}
      DebugLn('CHP GetTOCSitemap: ',FormatDateTime('hh:nn:ss.zzz', Now));
      {$ENDIF}
      {$IFDEF CHM_DEBUG_TIME}
      DebugLn('CHT Load of TOC start: ',FormatDateTime('hh:nn:ss.zzz', Now));
      {$ENDIF}
      SM := CHMReader.GetTOCSitemap;
      {$ELSE}
      SM := nil;
      fFillingIndex := True;
      Stream := TMemoryStream(fChms.GetObject(fChms.TOCFile));
      if Stream <> nil then
      begin
        SM := TChmSiteMap.Create(stTOC);
        SM.LoadFromStream(Stream);
        Stream.Free;
      end;
      {$ENDIF}
      if SM <> nil then
      begin
        with TContentsFiller.Create(fContentsTree, SM, @fStopTimer, CHMReader) do
        try
          DoFill(ParentNode, false);
        finally
          Free;
        end;
        FreeAndNil(SM);
      end;
      if Assigned(ParentNode) and (ParentNode.Index = 0) then ParentNode.Expanded := True;
      {$IFDEF CHM_DEBUG_TIME}
      DebugLn('CHT Load of TOC end: ',FormatDateTime('hh:nn:ss.zzz', Now));
      {$ENDIF}

      // Now we fill the index for all files
      {$IFDEF CHM_DEBUG_TIME}
      DebugLn('CHT oad of INDEX start: ',FormatDateTime('hh:nn:ss.zzz', Now));
      {$ENDIF}
      {$IFDEF CHM_BINARY_INDEX_TOC}
      SM := CHMReader.GetIndexSitemap;
      {$ELSE}
      SM := nil;
      Stream := TMemoryStream(fChms.GetObject(fChms.IndexFile));
      if Stream <> nil then
      begin
        SM := TChmSiteMap.Create(stTOC);
        SM.LoadFromStream(Stream);
        Stream.Free;
      end;
      {$ENDIF}
      if SM <> nil then
      begin
        fStatusBar.SimpleText := slhelp_IndexLoading;
        {$IFDEF CHM_DEBUG_TIME}
        DebugLn('CHT Load of INDEX start: ',FormatDateTime('hh:nn:ss.zzz', Now));
        {$ENDIF}
        with TContentsFiller.Create(fIndexView, SM, @fStopTimer, CHMReader) do
        try
          DoFill(nil, false);
          if fChms.Count > 1 then // FpDoc have to sort an INDEX
            fIndexView.Items.SortTopLevelNodes(@fIndexView.DefaultTreeViewSort);
        finally
          Free;
        end;
        FreeAndNil(SM);
        {$IFDEF CHM_DEBUG_TIME}
        DebugLn('CHT Load of INDEX end: ',FormatDateTime('hh:nn:ss.zzz', Now));
        {$ENDIF}
        fIndexView.FullExpand;
      end;
      {$IFDEF CHM_DEBUG_TIME}
      DebugLn('CHT end of load: ',FormatDateTime('hh:nn:ss.zzz', Now));
      {$ENDIF}
    end;

    {$IFDEF CHM_DEBUG_TIME}
    DebugLn('CHT CHM Title: '+CHMReader.Title);
    DebugLn('CHT End: ',FormatDateTime('hh:nn:ss.zzz', Now));
    {$ENDIF}

    fContentsTab.TabVisible := fContentsTree.Items.Count > 0;
    fIndexTab.TabVisible := fIndexTab.TabVisible or (fIndexView.Items.Count > 0);
    fStatusBar.SimpleText:= '';

    {$IFDEF CHM_SEARCH}
    i := 0;
    while (HasSearchIndex = False) and (i < fChms.Count) do
    begin
      // Look for binary full text search index in CHM file
      HasSearchIndex := fChms.Chm[i].ObjectExists('/$FIftiMain') > 0;
      inc(i);
    end;
    fSearchTab.TabVisible := fSearchTab.TabVisible or HasSearchIndex;
    {$ENDIF}

    if Title=DefaultCHMContentTitle then
      UpdateTitle;
    fFillTOCStack.Remove(CHMReader);
  finally
    Dispose(PTAsyncIndexData(AData));
    EndUpdate;
  end;
end;

procedure TChmContentProvider.IpHtmlPanelDocumentOpen(Sender: TObject);
begin
  if fIsUsingHistory = False then
    AddHistory(fChmDataProvider.CurrentPage)
  else
    fIsUsingHistory := False;
  fLastURI:= fChmDataProvider.CurrentPage;
  SelectTreeItemFromURL(fLastURI);
  // Debugln('CHP Ev IpHtmlPanelDocumentOpen() URL: '+fLastURI);
end;

procedure TChmContentProvider.IpHtmlPanelHotChange(Sender: TObject);
begin
  fStatusBar.SimpleText := fHtml.HotURL;
end;

procedure TChmContentProvider.IpHtmlPanelHotClick(Sender: TObject);
var
  HelpFile: String;
  aPos: integer;
begin
  // chm-links look like: mk:@MSITStore:D:\LazPortable\docs\chm\iPro.chm::/html/lh3zs3.htm
  if LazStartsText('javascript:helppopup(''', fHtml.HotURL) or
     LazStartsText('javascript:popuplink(''', fHtml.HotURL)
  then begin
    HelpFile := Copy(fHtml.HotURL, 23, Length(fHtml.HotURL) - (23-1));
    HelpFile := Copy(HelpFile, 1, Pos('''', HelpFile)-1);

    if (Pos('/',HelpFile)=0) and (Pos('.chm:',HelpFile)=0) then begin //looks like?: 'xyz.htm'
      aPos := LastDelimiter('/', fHtml.CurURL);
      if aPos>0 then HelpFile := Copy(fHtml.CurURL,1,aPos) + HelpFile;
   end
   else if (Pos('.chm:',HelpFile)=0) then begin //looks like?: 'folder/xyz.htm' or '/folder/xyz.htm'
     if HelpFile[1]<>'/' then HelpFile:='/'+HelpFile;
     aPos := LastDelimiter(':', fHtml.CurURL);
     if aPos>0 then HelpFile := Copy(fHtml.CurURL,1,aPos) + HelpFile;
   end;
   DoLoadUri(HelpFile); //open it in current iphtmlpanel.
  end
  else
   OpenURL(fHtml.HotURL);
end;

procedure TChmContentProvider.PopupCopyClick(Sender: TObject);
begin
  fHtml.CopyToClipboard;
end;

procedure TChmContentProvider.PopupCopySourceClick(Sender: TObject);
var
  rbs: rawbytestring;
  s: String;
begin
  rbs := fChmDataProvider.GetHtmlText(fHtml.CurUrl);
  s := ConvertEncoding(rbs, fHtml.MasterFrame.Html.DocCharset, encodingUTF8);
  Clipboard.SetAsHtml(rbs, s);
end;

procedure TChmContentProvider.ContentsTreeSelectionChanged(Sender: TObject);
var
  ATreeNode: TContentTreeNode;
  ARootNode: TTreeNode;
  fChm: TChmReader = nil;
  ActiveTreeView: TTreeView;
  Uri: String;
begin
  // Check Active TreeView
  ActiveTreeView:= nil;
  if fTabsControl.ActivePage = fContentsTab then ActiveTreeView:= fContentsTree;
  if fTabsControl.ActivePage = fIndexTab then ActiveTreeView:= fIndexView;
  if fTabsControl.ActivePage = fSearchTab then ActiveTreeView:= fSearchResults;

  if not (Assigned(ActiveTreeView) and Assigned(ActiveTreeView.Selected)) then Exit;
  // Load root pagefor TOC treeView
  if (ActiveTreeView = fContentsTree) and (ActiveTreeView.Selected.Parent = nil) then
  begin
    fChm := TChmReader(ActiveTreeView.Selected.Data);
    fActiveChmTitle:= fChm.Title;
    //UpdateTitle;
    if fChm.DefaultPage <> '' then
    begin
      Uri := MakeURI(fChm.DefaultPage, fChm);
{$IFDEF TREE_DEBUG}
      WriteLn('CHTR ContentTree changed1 URI: ', URI);
{$ENDIF}
      if ((fHtml.MasterFrame <> nil) and (MakeURI(fHtml.CurURL, fChm)  = Uri)) = False then
      begin
        ActiveTreeView.Tag:=1; // status of request from treeview
        DoLoadUri(Uri);
        ActiveTreeView.Tag:=0;
      end;
    end;
    Exit;
  end;

  ATreeNode := TContentTreeNode(ActiveTreeView.Selected);

  ArootNode:= ATreeNode;
  fChm := TChmReader(ARootNode.Data);
    if ATreeNode.Url <> '' then
    begin
      Uri := MakeURI(ATreeNode.Url, fChm);
{$IFDEF TREE_DEBUG}
      WriteLn('CHTR ContentTree changed1 URI: ', URI);
{$ENDIF}
      if ((fHtml.MasterFrame <> nil) and (MakeURI(fHtml.CurURL, fChm)  = Uri)) = False then
      begin
        if ActiveTreeView = fSearchResults then  FLoadingSearchURL:= True;
        ActiveTreeView.Tag:=1; // status of request from treeview
        DoLoadUri(MakeURI(ATreeNode.Url, fChm));
        ActiveTreeView.Tag:=0;
        if ActiveTreeView = fSearchResults then  FLoadingSearchURL:= False;
      end;
    end;
end;

procedure TChmContentProvider.TreeViewStopCollapse(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  AllowCollapse:=False;
end;

procedure TChmContentProvider.TreeViewShowHint ( Sender: TObject;
  HintInfo: PHintInfo ) ;
var
  Node: TContentTreeNode;
begin
  if HintInfo^.HintControl is TTreeView then
  begin
    Node:= TContentTreeNode(TTreeView(HintInfo^.HintControl).Selected);
    if Assigned(Node) and PtInRect(Node.DisplayRect(True), HintInfo^.CursorPos) then
        HintInfo^.HintStr:= MakeURI(Node.Url, TChmReader(Node.Data));
  end;
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
  Item:=nil;
  if fContentsTree.Items.Count > 0 then
    Item := fContentsTree.Items.GetFirstNode;
  NewTitle := '';
  while (Item <> nil) do
  begin
    if Item.Text <> fActiveChmTitle then
    begin
      NewTitle:=NewTitle+Item.Text;
      if (Item.GetNextSibling <> nil)
      and ((Item.GetNextSibling.GetNextSibling <> nil) or (Item.GetNextSibling.Text <>  fActiveChmTitle))
      then
        NewTitle:=NewTitle+', ';
    end;
    Item := Item.GetNextSibling;
  end;
  if NewTitle <> '' then
    NewTitle := FActiveChmTitle + ' [' + NewTitle + ']'
  else
    NewTitle := FActiveChmTitle;
  if NewTitle = '' then NewTitle := DefaultCHMContentTitle;
  Title := NewTitle;
end;

procedure TChmContentProvider.SetTitle(const AValue: String);
begin
  if (fHtml = nil) or (fHtml.Parent = nil) then exit;
  TTabSheet(fHtml.Parent).Caption := AValue;
  inherited SetTitle(AValue);
end;

procedure TChmContentProvider.SearchEditChange(Sender: TObject);
var
  SearchText: String;
  Node: TTreeNode;
begin
  if fIndexEdit <> Sender then
    Exit;
  SearchText := fIndexEdit.Text;
  Node := fIndexView.Items.GetFirstNode;
  while Node<>nil do
  begin
    if LazStartsText(SearchText, Node.Text) then
    begin
      fIndexView.Items.GetLastNode.MakeVisible;
      Node.MakeVisible;
      Node.Selected:=True;
      //DebugLn('Search edit exit: %s', [SearchText]);
      Exit;
    end;
    Node := Node.GetNextSibling;
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

procedure TChmContentProvider.SelectTreeItemFromURL ( const AUrl: String ) ;
var
  FileName: String;
  URL: String;
  RootNode,
  FoundNode,
  Node: TTreeNode;
  TmpHolder: TNotifyEvent;
  i: integer;
begin
  RootNode:= nil;
  if fContentsTree.Tag = 1 then
    Exit; // the change was a response to a click and should be ignored
  {$IFDEF LDEBUG}
  WriteLn('CHP >> SelectTreeItemFromURL()');
  DebugLn('Input AUrl: '+Aurl);
  {$ENDIF}
  FileName := GetURIFileName(AUrl);
  URL      := GetURIURL(AUrl);
  {$IFDEF LDEBUG}
  DebugLn('CHP Get Url: '+Url + ' Into filename: '+FileName);
  {$ENDIF}
  FoundNode := nil;
  Node := nil;
  for i := 0 to fChms.Count-1 do
  begin
    if FileName = ExtractFileName(fChms.FileName[i]) then
    begin
      fActiveChmTitle:= fChms.Chm[i].Title;
      //UpdateTitle;

      RootNode := fContentsTree.Items.FindNodeWithData(fChms.Chm[i]);
      if URL = fChms.Chm[i].DefaultPage then
      begin
        FoundNode := RootNode;
        {$IFDEF LDEBUG}
        DebugLn('CHP RootNode: '+ RootNode.text);
        {$ENDIF}
        Break;
      end;

      if RootNode <> nil then
      begin
        Node := RootNode.GetFirstChild;
        {$IFDEF LDEBUG}
        DebugLn('CHP RootNode Url : '+ TContentTreeNode(Node).Url);
        {$ENDIF}
      end;
      Break;
    end;

  end;

  if RootNode = nil then
    Exit;

  TmpHolder := fContentsTree.OnClick;
  fContentsTree.OnClick := nil;
  // Todo: clear WoContext compare FIRST
  while (Node<>nil) and (GetUrlWoContext(TContentTreeNode(Node).Url)<>GetUrlWoContext(Url)) do
  begin
    Node:=Node.GetNext;
  end;
  // Todo: clear WoContext compare SECOND
  if (Node <> nil) and (GetUrlWoContext(TContentTreeNode(Node).Url) = GetUrlWoContext(Url)) then
  begin
    FoundNode := Node;
  end;

  if FoundNode <> nil then
  begin
    {$IFDEF LDEBUG}
    DebugLn('CHP Found node: '+ FoundNode.Text);
    DebugLn('CHP Found URL: '+ TContentTreeNode(FoundNode).Url);
    {$ENDIF}
    fContentsTree.Selected := FoundNode;
    if not FoundNode.IsVisible then
      FoundNode.MakeVisible;
  end
  else
    fContentsTree.Selected := nil;

  fContentsTree.OnClick := TmpHolder;
  {$IFDEF LDEBUG}
  DebugLn('CHP << SelectTreeItemFromURL()');
  {$ENDIF}
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

{$IFDEF CHM_SEARCH}

procedure TChmContentProvider.SearchComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift <> []) then Exit;
  case key of
    VK_RETURN: begin
        SearchButtonClick(Sender);
        Key:=0;
      end;
    VK_DOWN:
      if fSearchResults.Items.Count > 0 then
      begin
        fSearchResults.SetFocus();
        if (fSearchResults.Selected = nil) then
        begin
          fSearchResults.Items.GetFirstNode().MakeVisible;
          fSearchResults.Items.GetFirstNode().Selected:=True;
        end;
        Key:= 0;
      end;
    else // hide warning
  end;
end;

procedure TChmContentProvider.ProcGlobalKeyUp(var Key: Word; Shift: TShiftState
  );
begin

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
    //DebugLn('Deleting Topic');
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
    for i := 0 to High(FoundTopics) do
    begin
      if FoundTopics[i].Topic = ATopicID then
        Exit(i);
    end;
  end;

  procedure UpdateTopic(TopicID: Integer; NewHits: Integer; NewTitleHits: Integer; AddNewTopic: Boolean);
  var
    TopicIndex: Integer;
  begin
    //DebugLn('Updating topic');
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
  //  if fKeywordCombo.Text = '' then Exit;
  SearchWords := TStringList.Create;
  try
    SearchWords.Delimiter := ' ';
    Searchwords.DelimitedText := fKeywordCombo.Text;
    if fKeywordCombo.Items.IndexOf(fKeywordCombo.Text) = -1 then
      fKeywordCombo.Items.Add(fKeywordCombo.Text);
    fSearchResults.BeginUpdate;
    fSearchResults.Items.Clear;
    //DebugLn('Search words: ', SearchWords.Text);
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
        // Body results
        for k := 0 to High(TopicResults) do
          UpdateTopic(TopicResults[k].TopicIndex, High(TopicResults[k].LocationCodes), 0, j = 0);
        // Title results
        for k := 0 to High(TitleResults) do
          UpdateTopic(TitleResults[k].TopicIndex, 0, High(TitleResults[k].LocationCodes), j = 0);

        // Remove documents that don't have results
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

      // Clear out results that don't contain all the words we are looking for

      Item := nil;
      // Now lookup titles and urls to add to final search results
      for j := 0 to High(FoundTopics) do
      begin
        try
          DocURL := fChms.Chm[i].LookupTopicByID(FoundTopics[j].Topic, DocTitle);
          if (Length(DocURL) > 0) and (DocURL[1] <> '/') then
            Insert('/', DocURL, 1);
          if DocTitle = '' then
            DocTitle := slhelp_Untitled;
          Item := TContentTreeNode(fSearchResults.Items.Add(Item, DocTitle));
          Item.Data:= fChms.Chm[i];
          Item.Url:= DocURL;
        except
          //DebugLn('Exception');
          // :)
        end;
      end;
      // Sort the result
      fSearchResults.Items.SortTopLevelNodes(@fIndexView.DefaultTreeViewSort);
      SetLength(FoundTopics, 0);
    end;
    SetLength(FoundTopics, 0);
  finally
    SearchWords.Free;
  end;

  if fSearchResults.Items.Count = 0 then
  begin
    fSearchResults.Items.Add(nil, slhelp_NoResults);
  end;
  fSearchResults.EndUpdate;
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
  XFile: String;
  xURL: String = '';
  CurCHM: TChmReader;
  ContextURL: String;
begin
  Result := False;
  XFile := GetUrlFilePath(AUrl);
  xURL := GetUrlFile(AUrl);

  fChmDataProvider.DoOpenChm(XFile, False);

  fHistoryIndex := -1;
  fHistory.Clear;

  CurCHM := GetChmReader(XFile);
  if CurCHM = nil then Exit;

  // Load TOC is executed by TChmContentProvider.NewChmOpened() now

  // AContext will override the URL if it is found
  if AContext <> -1 then
  begin
    ContextURL := CurCHM.GetContextUrl(AContext);
    if (Length(ContextURL) > 0) and not (ContextURL[1] in ['/', '\']) then
      Insert('/', ContextURL , 1);
    if Length(ContextURL) > 0 then
      xURL := ContextURL;
  end;

  if xURL <> '' then
    DoLoadUri(MakeURI(xURL, CurCHM))
  else
    DoLoadUri(MakeURI(CurCHM.DefaultPage, CurCHM));
  Result := True;

end;

function TChmContentProvider.HasLoadedData ( const AUrl: String ) : Boolean;
begin
  Result:= (fChms <> nil) and fChms.IsAnOpenFile(GetUrlFilePath(AUrl));
end;

procedure TChmContentProvider.GoHome;
begin
  if (fChms <> nil) and (fChms.Chm[0].DefaultPage <> '') then
  begin
    DebugLn('CHP GoHome() DefaultPage: ', fChms.Chm[0].DefaultPage);
    DoLoadUri(MakeURI(fChms.Chm[0].DefaultPage, fChms.Chm[0]));
  end;
end;

procedure TChmContentProvider.GoBack;
begin
  if CanGoBack then
  begin
    Dec(fHistoryIndex);
    fIsUsingHistory:=True;
    fHtml.OpenURL(fHistory.Strings[fHistoryIndex]);
  end;
end;

procedure TChmContentProvider.GoForward;
var
  HistoryChm: TChmReader;
begin
  if CanGoForward then
  begin
    Inc(fHistoryIndex);
    fIsUsingHistory:=True;
    HistoryChm := TChmReader(fHistory.Objects[fHistoryIndex]);
    fChms.ObjectExists(fHistory.Strings[fHistoryIndex], HistoryChm); // this ensures that the correct chm will be found
    fHtml.OpenURL(fHistory.Strings[fHistoryIndex]);
  end;
end;

procedure TChmContentProvider.ActivateProvider;
begin
  //DebugLn('CHP ActivateProvider() FLastUri: '+fLastURI);
  // For show Home after load of all chms from Lazarus
  if (fChms.Count >0) and (fLastURI = '') then
    GoHome;
end;

procedure TChmContentProvider.ActivateTOCControl;
begin
  if fContentsTab.TabVisible then
  begin
    fTabsControl.ActivePage:= fContentsTab;
    if fContentsTree.Visible then
      fContentsTree.SetFocus
    else
      fContentsTab.SetFocus;
  end;
end;

procedure TChmContentProvider.ActivateIndexControl;
begin
  if fIndexTab.TabVisible then
  begin
    fTabsControl.ActivePage:= fIndexTab;
    fIndexEdit.SetFocus;
  end;
end;

procedure TChmContentProvider.ActivateSearchControl;
begin
  if fSearchTab.TabVisible then
  begin
    fTabsControl.ActivePage:= fSearchTab;
    fKeywordCombo.SetFocus;
  end;
end;

class function TChmContentProvider.GetProperContentProvider(const AURL: String
  ): TBaseContentProviderClass;
begin
  Result:=TChmContentProvider;
end;

constructor TChmContentProvider.Create(AParent: TWinControl; AImageList: TImageList;
                                 AUpdateCount: Integer);
const
  TAB_WIDTH = 215;
begin
  inherited Create(AParent, AImageList, AUpdateCount);

  fHistory := TStringList.Create;
  fFillTOCStack := TFPList.Create;

  fTabsControl := TPageControl.Create(AParent);
  with fTabsControl do
  begin
    Width := TAB_WIDTH + 12;
    Align := alLeft;
    Parent := AParent;
    Visible := True;
  end;

  fContentsTab := TTabSheet.Create(fTabsControl);
  with fContentsTab do
  begin
    Caption := slhelp_Contents;
    Parent := fTabsControl;
  end;
  fContentsPanel := TPanel.Create(fContentsTab);
  with fContentsPanel do
  begin
    Parent := fContentsTab;
    Align := alClient;
    BevelOuter := bvNone;
    Caption := '';
    Visible := True;
  end;
  fContentsTree := TTreeView.Create(fContentsPanel);
  with fContentsTree do
  begin
    Parent := fContentsPanel;
    Align := alClient;
    BorderSpacing.Around := 6;
    ReadOnly := True;
    Visible := True;
    ShowHint:=True;
    OnShowHint:=@TreeViewShowHint;
    OnExpanded := @TOCExpand;
    OnCollapsed := @TOCCollapse;
    OnCreateNodeClass:= @GetTreeNodeClass;
    OnClick:= @ContentsTreeSelectionChanged;
    //OnKeyUp:= @ProcTreeKeyUp;
    OnKeyDown:= @ProcTreeKeyDown;
    Images := fImageList;
    //StateImages := fImageList;
  end;

  fIndexTab := TTabSheet.Create(fTabsControl);
  with fIndexTab do
  begin
    Caption := slhelp_Index;
    Parent := fTabsControl;
    TabVisible:= False;
  end;

  fIndexEdit := TLabeledEdit.Create(fIndexTab);
  with fIndexEdit do
  begin
    Parent := fIndexTab;
    Anchors := [akLeft, akRight, akTop];
    BorderSpacing.Around := 6;
    AnchorSide[akLeft].Control := fIndexTab;
    AnchorSide[akRight].Control := fIndexTab;
    AnchorSide[akRight].Side := asrBottom;
    AnchorSide[akTop].Control := fIndexTab;
    EditLabel.Caption := slhelp_Search;
    EditLabel.AutoSize := True;
    LabelPosition := lpAbove;
    OnChange := @SearchEditChange;
    OnKeyDown:= @ProcKeyDown;
    Visible := True;
  end;

  fIndexView := TTreeView.Create(fIndexTab);
  with fIndexView do
  begin
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
    ShowHint:=True;
    OnShowHint:=@TreeViewShowHint;
    OnCollapsing:=@TreeViewStopCollapse;
    OnClick:= @ContentsTreeSelectionChanged;
    //OnKeyUp:= @ProcTreeKeyUp;
    OnKeyDown:= @ProcTreeKeyDown;
    OnCreateNodeClass:=@GetTreeNodeClass;
    OnCompare:=@CompareIndexNodes;
  end;

 {$IFDEF CHM_SEARCH}
  fSearchTab := TTabSheet.Create(fTabsControl);
  with fSearchTab do
  begin
    Caption := slhelp_Search;
    Parent := fTabsControl;
    TabVisible:= False;
  end;
  fKeywordLabel := TLabel.Create(fSearchTab);
  with fKeywordLabel do
  begin
    Parent := fSearchTab;
    Top := 6;
    Caption := slhelp_Keyword;
    Left := 6;
    AutoSize := True;
  end;
  fKeywordCombo := TComboBox.Create(fSearchTab);
  with fKeywordCombo do
  begin
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
  with fSearchBtn do
  begin
    Parent := fSearchTab;
    Anchors := [akLeft, akTop];
    BorderSpacing.Around := 6;
    AnchorSide[akLeft].Control := fSearchTab;
    AnchorSide[akTop].Control := fKeywordCombo;
    AnchorSide[akTop].Side := asrBottom;
    Caption := slhelp_Find;
    OnClick := @SearchButtonClick;
  end;
  fResultsLabel := TLabel.Create(fSearchTab);
  with fResultsLabel do
  begin
    Parent := fSearchTab;
    Anchors := [akLeft, akTop];
    BorderSpacing.Around := 6;
    AnchorSide[akLeft].Control := fSearchTab;
    AnchorSide[akRight].Control := fSearchTab;
    AnchorSide[akRight].Side := asrBottom;
    AnchorSide[akTop].Control := fSearchBtn;
    AnchorSide[akTop].Side := asrBottom;
    Caption := slhelp_SearchResults;
    AutoSize := True;
  end;
  fSearchResults := TTreeView.Create(fSearchTab);
  with fSearchResults do
  begin
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
    ShowHint:=True;
    OnShowHint:=@TreeViewShowHint;
    OnClick:= @ContentsTreeSelectionChanged;
    OnKeyDown:= @ProcTreeKeyDown;
    OnCollapsing:=@TreeViewStopCollapse;
    OnCreateNodeClass:=@GetTreeNodeClass;
    OnCompare:=@CompareIndexNodes;
  end;
 {$ENDIF}

  fHtml := TIpHtmlPanel.Create(AParent);
  with fHtml do
  begin
    OnDocumentOpen := @IpHtmlPanelDocumentOpen;
    OnHotChange := @IpHtmlPanelHotChange;
    OnHotClick := @IpHtmlPanelHotClick;
    //OnKeyDown:= @ProcTreeKeyDown;
    DataProvider := TIpChmDataProvider.Create(fHtml);
    Parent := AParent;
    Align := alClient;
  end;

  fChms:= TIpChmDataProvider(fHtml.DataProvider).Chms; // save only pointer for convenience
  fChms.OnOpenNewFile:= @NewChmOpened;
  fChmDataProvider:= TIpChmDataProvider(fHtml.DataProvider); // save only pointer for convenience
  fChmDataProvider.OnGetHtmlPage:=@LoadingHTMLStream;

  fSplitter := TSplitter.Create(AParent);
  with fSplitter do
  begin
    //Align  := alLeft;
    Left := 1;
    AnchorSide[akLeft].Control := fTabsControl;
    AnchorSide[akLeft].Side:= asrRight;
    AnchorSide[akRight].Control := fHtml;
    AnchorSide[akRight].Side := asrLeft;
    Parent := AParent;
  end;

  fPopUp := TPopupMenu.Create(fHtml);
  fPopUp.Items.Add(TMenuItem.Create(fPopup));
  with fPopUp.Items.Items[0] do
  begin
    Caption := slhelp_Copy;
    OnClick := @PopupCopyClick;
  end;
  fPopup.Items.Add(TMenuItem.Create(fPopup));
  with fPopup.Items.Items[1] do
  begin
    Caption := slhelp_CopyHtmlSource;
    OnClick := @PopupCopySourceClick;
  end;
  fHtml.PopupMenu := fPopUp;

  fStatusBar := TStatusBar.Create(AParent);
  with fStatusBar do
  begin
    Parent := AParent;
    Align := alBottom;
    SimplePanel := True;
  end;

  if isUpdate then
  begin
    fContentsTree.BeginUpdate;
    fIndexView.BeginUpdate;
  end;

end;

destructor TChmContentProvider.Destroy;
begin
  fChmDataProvider.DoCloseChms;
  fHistory.Free;
  if fFillTOCStack.Count > 0 then
  begin
    Application.ProcessMessages;
    Sleep(200); // waiting a stop of async TOC creating
  end;
  fFillTOCStack.Free;
  inherited Destroy;
end;

initialization

  RegisterFileType('.chm', TChmContentProvider);

end.

