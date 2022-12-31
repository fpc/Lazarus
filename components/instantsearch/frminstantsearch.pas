unit frmInstantSearch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazLoggerBase, FileUtil, Forms, Controls, Graphics,
  Dialogs, LCLType, ExtCtrls, Grids, StdCtrls, ComCtrls, Menus, IDECommands,
  IDEOPtEditorIntf, IDEWindowIntf, LazIDEIntf, MenuIntf, Types, idemcindexer;

Const
  LocRightMargin = 4;
  LocLeftMargin = 4;
  ResultRightMargin = 8;
  LocOffset = LocLeftMargin+LocRightMargin;
  SrcOffset = 2;

type

  { TInstantSearchForm }

  TInstantSearchForm = class(TForm)
    edtSearch: TEdit;
    grdResults: TDrawGrid;
    ILSearch: TImageList;
    imgSearch: TImage;
    pnlStatus: TPanel;
    pnlNoResults: TPanel;
    pnlTop: TPanel;
    PMTrees: TPopupMenu;
    TBSettings: TToolBar;
    TBTrees: TToolButton;
    tmrIdle: TTimer;
    tmrAuto: TTimer;
    procedure edtSearchChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grdResultsClick(Sender: TObject);
    procedure grdResultsDblClick(Sender: TObject);
    procedure grdResultsDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure tmrAutoTimer(Sender: TObject);
    procedure tmrIdleTimer(Sender: TObject);
  private
    class var _OptionsFrameClass: TAbstractIDEOptionsEditorClass;
    procedure CheckIndexing;
    procedure DoCheckIndexing(sender: TObject);
    function IsConfigured: Boolean;
    procedure ShowHighLights(S: String; aCanvas: TCanvas; aRect: TRect;      aLeft: Integer);
  private
    fLastChange : TDateTime;
    FCurrentSearch : String;
    FResults : TMCSearchResultArray;
    FSourceStyle: TTextStyle;
    FContentStyle: TTextStyle;
    FScrollbarwidth : Integer;
    FLastSearchAt : TDateTime;
    procedure CheckScrollbarwidth;
    procedure CheckColSize;
    procedure CreateStyles;
    procedure DoEnableTree(Sender: TObject);
    Procedure DoSearch(aText : string);
    procedure ShowTrees;

  public
    constructor create(aOwner: TComponent); override;
    // Call this
    Procedure TreesChanged;
    // Call this
    Procedure ActiveProjectChanged;
    // Do we have enough characters to search ?
    function CanSearch : Boolean;
    // Actually search
    procedure Search;
    // Called when the form is shown through user action
    Procedure InitSearch;
    // Properties to show
    Class Property OptionsFrameClass : TAbstractIDEOptionsEditorClass Read _OptionsFrameClass Write _OptionsFrameClass;
      // TIDEInstantSearchOptionsFrame
  end;

var
  InstantSearchForm: TInstantSearchForm;
  InstantSearchFormCreator: TIDEWindowCreator; // set by Register procedure

procedure ShowInstantSearchForm(Sender: TObject);
procedure CreateInstantSearchForm(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);

implementation

uses
  dateutils, LCLIntf, clipbrd, IDEExternToolIntf, idemsgintf, ideinstantsearch, instantsearchstrings;


{$R *.lfm}

procedure ShowInstantSearchForm(Sender: TObject);

begin
  if Assigned(IDEWindowCreators.ShowForm(InstantSearchFormCreator.FormName, true)) then
    InstantSearchForm.InitSearch;
end;

procedure CreateInstantSearchForm(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  // sanity check to avoid clashing with another package that has registered a window with the same name
  if CompareText(aFormName, 'InstantSearchForm')<>0 then begin
    DebugLn(['ERROR: CreateInstantSearchForm: there is already a form with '
      +'this name']);
    exit;
  end;
  IDEWindowCreators.CreateForm(AForm, TInstantSearchForm, DoDisableAutoSizing,
    LazarusIDE.OwningComponent);
  AForm.Name:=aFormName;
  InstantSearchForm:=AForm as TInstantSearchForm;
end;

{ TInstantSearchForm }

procedure TInstantSearchForm.grdResultsDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);

var
  aRes : PMCSearchResult;
  S : String;
  aLeft,W : integer;
  RSource : Trect;

begin
  if (aRow>=Length(FResults)) or (aCol>=1) then
    Exit;
  grdResults.DefaultDrawCell(aCol,aRow,aRect,aState);
  aRes:=@FResults[aRow];
  Case aCol of
  0 :
    begin
    if gdSelected in aState then
      grdResults.Canvas.DrawFocusRect(aRect);
    S:=ExtractFileName(aRes^.FileName);
    S:=Format('%s(%d)',[S,aRes^.LineNo]);
    W:=grdResults.Canvas.TextWidth(S);
    RSource:=aRect;
    RSource.Left:=aRect.Right-W-LocOffset-FScrollbarwidth;
    RSource.Right:=aRect.Right-LocRightMargin-FScrollbarwidth;
    grdResults.Canvas.TextRect(RSource,RSource.Left,RSource.Top,S,FSourceStyle);
    aRect.Right:=aRect.Right-W-LocOffset-FScrollbarwidth-ResultRightMargin;
    S:=Trim(aRes^.Content);
    aLeft:=aRect.Left+SrcOffset;
    ShowHighLights(S,grdResults.Canvas,aRect,aLeft);
    grdResults.Canvas.Brush.Style:=bsClear;
    grdResults.Canvas.TextRect(aRect,aLeft,aRect.Top,S,FContentStyle);
    end;
  end;
end;


procedure TInstantSearchForm.ShowHighLights(S : String; aCanvas : TCanvas; aRect : TRect; aLeft: Integer);

Var
  W,L,P : Integer;
  lRect : TRect;
  C : TColor;

begin
  lRect:=aRect;
  c:=aCanvas.Brush.Color;
  aCanvas.Brush.Color:=clYellow;
  W:=aCanvas.TextWidth(FCurrentSearch);
  P:=Pos(FCurrentSearch,S);
  While P<>0 do
    begin
    L:=aCanvas.TextWidth(Copy(S,1,P-1));
    lRect.Left:=aLeft+L;
    lRect.Right:=aLeft+L+W;
    if lrect.Right>aRect.Right then
      lrect.Right:=aRect.Right;
    aCanvas.FillRect(lRect);
    P:=Pos(FCurrentSearch,S,P+Length(FCurrentSearch));
    end;
  aCanvas.Brush.Color:=C;
end;


procedure TInstantSearchForm.tmrAutoTimer(Sender: TObject);
begin
  if MillisecondsBetween(Now,fLastChange)>TmrAuto.Interval then
     begin
     TmrAuto.Enabled:=False;
     Search;
     end;
end;

procedure TInstantSearchForm.tmrIdleTimer(Sender: TObject);
begin
  if SecondsBetween(Now,FLastSearchAt)> IDEInstantSearchManager.IdleDisconnectTimeOut then
    if IDEInstantSearchManager.Indexer.Connected then
      IDEInstantSearchManager.Indexer.Disconnect;
end;

procedure TInstantSearchForm.CheckScrollbarwidth;
begin
//    h := GetSystemMetrics(SM_CYHSCROLL);
  FScrollBarWidth:=GetSystemMetrics(SM_CXVSCROLL);
end;

procedure TInstantSearchForm.DoSearch(aText: string);
begin
  FCurrentSearch:=aText;
  if Not IDEInstantSearchManager.Indexer.Connected then
    try
      IDEInstantSearchManager.Indexer.Connect;
    except
      On E : Exception do
        begin
        IDEMessagesWindow.AddCustomMessage(mluError,E.Message,'',0,0,lrsInstantSearch);
        MessageDlg(lrsInstantSearch,Format(lrsErrorConnecting,[E.Message]),mtError,[mbOK],'');
        end;
    end;
  try
    FResults:=IDEInstantSearchManager.Search(aText);
  except
    On E : Exception do
      begin
      IDEMessagesWindow.AddCustomMessage(mluError,E.Message,'',0,0,lrsInstantSearch);
      MessageDlg(lrsInstantSearch,Format(lrsErrorSearching,[E.Message]),mtError,[mbOK],'');
      end;
  end;
  grdResults.RowCount:=Length(FResults);
  CheckScrollbarWidth;
  pnlNoResults.Visible:=Length(FResults)=0;
  grdResults.Invalidate;
  FLastSearchAt:=Now;
end;

constructor TInstantSearchForm.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  Caption:=lrsInstantSearch;
  edtSearch.TextHint:=Format(lrsSearchTextHint,[IDEInstantSearchManager.MinSearchLength]);
  edtSearch.Hint:=Format(lrsSearchTextHint,[IDEInstantSearchManager.MinSearchLength]);
  IDEInstantSearchManager.OnIndexStart:=@DoCheckIndexing;
  IDEInstantSearchManager.OnIndexDone:=@DoCheckIndexing;
  CheckIndexing;
  CreateStyles;
  ShowTrees;
end;

procedure TInstantSearchForm.DoCheckIndexing(sender : TObject);
begin
  CheckIndexing;
end;

procedure TInstantSearchForm.CheckIndexing;

begin
  pnlStatus.Visible:=IDEInstantSearchManager.IsIndexing;
end;


procedure TInstantSearchForm.TreesChanged;
begin
  ShowTrees;
end;

procedure TInstantSearchForm.ActiveProjectChanged;
begin
  ShowTrees;
end;

procedure TInstantSearchForm.ShowTrees;

Var
  aTree : TSourceTreeDefinition;
  Mnu : TMenuItem;
  I : Integer;

begin
  PMTrees.Items.Clear;
  Mnu:=TMenuItem.Create(Self);
  Mnu.Tag:=-1;
  Mnu.Caption:=lrsCurrentProject;
  Mnu.AutoCheck:=True;
  Mnu.OnClick:=@DoEnableTree;
  Mnu.Enabled:=(IDEInstantSearchManager.ProjectTreeName<>'');
  Mnu.Checked:=Mnu.Enabled and IDEInstantSearchManager.SearchProject;
  PMTrees.Items.Add(Mnu);
  Mnu:=TMenuItem.Create(Self);
  Mnu.Caption:='-';
  PMTrees.Items.Add(Mnu);
  For I:=0 to IDEInstantSearchManager.SourceTrees.Count-1 do
    begin
    aTree:=IDEInstantSearchManager.SourceTrees[i];
    Mnu:=TMenuItem.Create(Self);
    Mnu.Tag:=I;
    Mnu.Caption:=aTree.Name;
    Mnu.Checked:=aTree.Enabled;
    Mnu.AutoCheck:=True;
    Mnu.OnClick:=@DoEnableTree;
    PMTrees.Items.Add(Mnu);
    end;
end;

procedure TInstantSearchForm.CreateStyles;

begin
  FSourceStyle.Alignment:=taRightJustify;
  FSourceStyle.Clipping:=True;
  FSourceStyle.EndEllipsis:=True;
  FSourceStyle.Layout:=TTextLayout.tlCenter;
  FSourceStyle.Wordbreak:=False;

  FContentStyle.Alignment:=taLeftJustify;
  FContentStyle.Opaque:=True;
  FContentStyle.Clipping:=True;
  FContentStyle.EndEllipsis:=True;
  FContentStyle.Layout:=TTextLayout.tlCenter;
  FContentStyle.Wordbreak:=False;
  FContentStyle.Opaque:=True;
end;

procedure TInstantSearchForm.DoEnableTree(Sender: TObject);

Var
  idx : Integer;
  Mnu : TMenuItem absolute Sender;

begin
  if Not (Sender is TMenuItem) then exit;
  Idx:=Mnu.Tag;
  if Idx=-1 then
    IDEInstantSearchManager.SearchProject:=Mnu.Checked
  else
    IDEInstantSearchManager.SourceTrees[Idx].Enabled:=Mnu.Checked;
end;

function TInstantSearchForm.CanSearch: Boolean;
begin
  Result:=Length(edtSearch.text)>=IDEInstantSearchManager.MinSearchLength;
end;

function TInstantSearchForm.IsConfigured: Boolean;

Var
  ErrMsg : String;

begin
  Result:=False;
  if IDEInstantSearchManager.Configured then
    ErrMsg:=IDEInstantSearchManager.CanSearch
  else
    ErrMsg:=lrsNotConfigured;
  Result:=(ErrMsg='');
  if Not Result then
    if mrOK=QuestionDlg(lrsConfigNeeded,Format(lrsConfigNeededReason,[ErrMsg]),mtWarning,[mrAbort,lrsCancelSearch,mrOK,lrsConfigure],'LazInstantSearch') then
      Result:=LazarusIDE.DoOpenIDEOptions(_OptionsFrameClass) and IDEInstantSearchManager.Configured
    else
      Exit;
end;

procedure TInstantSearchForm.Search;

begin
  if not CanSearch then Exit;
  if not IsConfigured then
    Exit;
  DoSearch(edtSearch.Text);
end;

procedure TInstantSearchForm.InitSearch;

var
  lSearch: String;
  C : Char;
  OK : Boolean;

begin
  if Not IsConfigured then
    exit;
  edtSearch.SetFocus;
  lSearch:=Clipboard.AsText;
  OK:=Length(lSearch)<=IDEInstantSearchManager.MaxStartSearchLength;
  if Not OK then
    exit;
  for C in lSearch do
    begin
    OK:=Not (C in [#10,#13,#12]);
    if Not OK then
      Break;
    end;
  if Not OK then
    exit;
  if OK then
    begin
    edtSearch.Text:=lSearch;
    edtSearch.SelectAll;
    if CanSearch then
      Search;
    end;
end;

procedure TInstantSearchForm.grdResultsClick(Sender: TObject);
begin
  if CanSearch then
    Search
end;

procedure TInstantSearchForm.grdResultsDblClick(Sender: TObject);

Var
  Idx : Integer;
  Res : TMCSearchResult;
  FN,BaseDir : String;
  aTree : TSourceTreeDefinition;
  P : TPoint;

begin
  Idx:=grdResults.Selection.Top;
  if (Idx<0) or (Idx>=Length(FResults)) then exit;
  Res:=FResults[Idx];
  if Res.Tree=IDEInstantSearchManager.ProjectTreeName then
    BaseDir:=ExtractFIlePath(LazarusIDE.ActiveProject.ProjectInfoFile)
  else
    begin
    aTree:=IDEInstantSearchManager.SourceTrees.FindByName(Res.Tree);
    if aTree=Nil then
      begin
      ShowMessage(Format(lrsTreeNotFoundResfresh,[Res.Tree]));
      Exit;
      end
    else
      BaseDir:=IncludeTrailingPathDelimiter(aTree.BaseDir);
    end;
  FN:=ExpandFileName(BaseDir+Res.FileName);
  P.Y:=Res.LineNo;
  P.X:=1;
  LazarusIDE.DoOpenFileAndJumpToPos(FN,P,0,0,0,[ofOnlyIfExists,ofRegularFile,ofDoNotLoadResource,ofAddToRecent]);
end;

procedure TInstantSearchForm.edtSearchChange(Sender: TObject);
begin
  tmrAuto.Enabled:=true;
  fLastChange:=Now;
end;

procedure TInstantSearchForm.CheckColSize;

begin
  grdResults.Columns[0].Width:=grdResults.ClientWidth;
end;

procedure TInstantSearchForm.FormResize(Sender: TObject);
begin
  CheckColSize;
end;

procedure TInstantSearchForm.FormShow(Sender: TObject);
begin
  CheckColSize;
  tmrIdle.Interval:=10 * IDEInstantSearchManager.IdleDisconnectTimeOut;
  FLastSearchAt:=Now;
end;



end.

