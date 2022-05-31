unit iprotest_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  EditBtn, ComCtrls, Buttons, SynEdit, SynHighlighterHTML, IpHtml, Ipfilebroker;

type
  
  { TTestForm }

  TTestForm = class(TForm)
    btnPassed: TBitBtn;
    btnFailed: TBitBtn;
    btnRender: TButton;
    btnShowInBrowser: TButton;
    btnLoadHtmlFromFile: TButton;
    btnSaveResults: TButton;
    FileNameEdit1: TFileNameEdit;
    ImageList1: TImageList;
    IpHtmlDataProvider: TIpHtmlDataProvider;
    IpHtmlPanel1: TIpHtmlPanel;
    Label1: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    SynEdit1: TSynEdit;
    SynHTMLSyn1: TSynHTMLSyn;
    TestTree: TTreeView;
    procedure btnSaveResultsClick(Sender: TObject);
    procedure btnTestResultClick(Sender: TObject);
    procedure btnRenderClick(Sender: TObject);
    procedure btnShowInBrowserClick(Sender: TObject);
    procedure btnLoadHtmlFromFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IpHtmlDataProviderGetImage(Sender: TIpHtmlNode; 
      const URL: string; var Picture: TPicture);
    procedure SynEdit1Change(Sender: TObject);
    procedure TestTreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure TestTreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure TestTreeSelectionChanged(Sender: TObject);
  private
    procedure PopulateTests;
    procedure LoadResults;
    procedure SaveResults;

  public
    procedure AddTest(ANode: TTreeNode; ATitle, ADescription, AHtml: String);

  end;

var
  TestForm: TTestForm;

implementation

{$R *.lfm}

uses
  LCLIntf, md5, ipro_tests;

const  // Values for node imageindex showing test results
  RESULT_UNDEFINED = -1;
  RESULT_FAILED = 0;
  RESULT_PASSED = 1;
  
  RESULTS_FILE = 'test-results.txt';
  
type
  TTestCase = class
    Description: String;
    HTML: String;
    MD5: TMDDigest;
  end;
  

{ TTestForm }

procedure TTestForm.AddTest(ANode: TTreeNode; ATitle, ADescription, AHtml: String);
var
  testcase: TTestCase;
begin
  testcase := TTestCase.Create;
  testcase.Description := ADescription;
  testcase.HTML := AHtml;
  testcase.MD5 := MDString(AHtml, MD_VERSION_5);
  TestTree.Items.AddChildObject(ANode, ATitle, testcase);
end;

procedure TTestForm.FormCreate(Sender: TObject);
begin
  SynEdit1.Font.Quality := fqCleartype;
  PopulateTests;
end;

procedure TTestForm.IpHtmlDataProviderGetImage(Sender: TIpHtmlNode; 
  const URL: string; var Picture: TPicture);
begin
  if FileExists(URL) then
  begin
    Picture := TPicture.Create;
    Picture.LoadFromFile(URL);
  end;
end;

procedure TTestForm.SynEdit1Change(Sender: TObject);
begin
  btnPassed.Enabled := false;
  btnFailed.Enabled := false;
end;

procedure TTestForm.TestTreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if (TObject(Node.Data) is TTestCase) then
    TTestCase(Node.Data).Free;
end;

procedure TTestForm.TestTreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TTestForm.btnRenderClick(Sender: TObject);
begin
  IpHtmlPanel1.SetHtmlFromStr(SynEdit1.Lines.Text);
end;

procedure TTestForm.btnTestResultClick(Sender: TObject);
var
  testcase: TTestCase;
begin
  if TestTree.Selected <> nil then
  begin
    testcase := TTestCase(TestTree.Selected.Data);
    if testcase <> nil then
    begin
      if Sender = btnPassed then
        TestTree.Selected.ImageIndex := RESULT_PASSED
      else if Sender = btnFailed then 
        TestTree.Selected.ImageIndex := RESULT_FAILED;
    end;
  end;
end;

procedure TTestForm.btnSaveResultsClick(Sender: TObject);
begin
  SaveResults;
end;

procedure TTestForm.btnShowInBrowserClick(Sender: TObject);
const
  TEST_FILE = 'test.html';
begin
  SynEdit1.Lines.SaveToFile(TEST_FILE);
  OpenURL(TEST_FILE);
end;

procedure TTestForm.btnLoadHtmlFromFileClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do 
    try
      filter := 'HTML files (*.html; *.htm)|*.html;*.htm';
      if FileName <> '' then
        InitialDir := ExtractFileDir(FileName);
      Options := Options + [ofFileMustExist];
      if Execute then
      begin
        Memo1.Lines.Clear;
        SynEdit1.Lines.LoadFromFile(FileName);
        IpHtmlPanel1.SetHtmlFromStr(SynEdit1.Lines.Text);
        btnPassed.Enabled := false;
        btnFailed.Enabled := false;
      end;
    finally
      Free;
    end;
end;

procedure TTestForm.TestTreeSelectionChanged(Sender: TObject);
var
  testcase: TTestCase;
begin
  if TestTree.Selected = nil then
    exit;
  testCase := TTestCase(TestTree.Selected.Data);
  if testCase <> nil then 
  begin
    Memo1.Lines.Text := testCase.Description;
    Synedit1.Lines.Text := testCase.html;
    IpHtmlPanel1.SetHtmlFromStr(testCase.html);
    btnPassed.Enabled := true;
    btnFailed.Enabled := true;
  end else
  begin
    Memo1.Lines.Clear;
    SynEdit1.Lines.Clear;
    IpHtmlPanel1.SetHtml(nil);
    btnPassed.Enabled := false;
    btnFailed.Enabled := false;
  end;
end;

procedure TTestForm.PopulateTests;
var
  node, node1: TTreeNode;
begin
  TestTree.Items.BeginUpdate;
  try
    TestTree.Items.Clear;
      
    node := TestTree.Items.AddChild(nil, 'Text background');
    AddTest(node, TextWithBackgroundInBODY_title, TextWithBackgroundInBODY_descr, TextWithBackgroundInBODY_html);
    AddTest(node, TextWithBackgroundInBODY_CSS_title, TextWithBackgroundInBODY_CSS_descr, TextWithBackgroundInBODY_CSS_html);
    AddTest(node, TextInColoredTableCell_title, TextInColoredTableCell_descr, TextInColoredTableCell_html);
    node.Expanded := true;
    
    node := TestTree.Items.AddChild(nil, 'Tables');
    node1 := TestTree.Items.AddChild(node, 'Text alignment');
    AddTest(node1, AlignInCell_title, AlignInCell_descr, AlignInCell_html);
    AddTest(node1, AlignInCellBold_title, AlignInCellBold_descr, AlignInCellBold_html);
    AddTest(node1, AlignInCell_CSS_title, AlignInCell_CSS_descr, AlignInCell_CSS_html);
    AddTest(node1, AlignInCellBold_CSS_title, AlignInCellBold_CSS_descr, AlignInCellBold_CSS_html);
    node1.Expanded := true;
    node1 := TestTree.Items.AddChild(node, 'Background colors');
    AddTest(node1, TableCellBkColor_title, TableCellBkColor_descr, TableCellBkColor_html);
    AddTest(node1, TableCellBkColor_style_title, TableCellBkColor_style_descr, TableCellBkColor_style_html);
    AddTest(node1, TableRowBkColor_title, TableRowBkColor_descr, TableRowBkColor_html);
    AddTest(node1, TableRowBkColor_style_title, TableRowBkColor_style_descr, TableRowBkColor_style_html);
    AddTest(node1, TableColBkColor_style_title, TableColBkColor_style_descr, TableColBkColor_style_html);
    node1.Expanded := true;
    node1 := TestTree.Items.AddChild(node, 'Merged cells');
    AddTest(node1, TableColSpan_title, TableColSpan_descr, TableColSpan_html);
    AddTest(node1, TableRowSpan_title, TableRowSpan_descr, TableRowSpan_html);
    node1.Expanded := true;
    node1 := TestTree.Items.AddChild(node, 'Column widths');
    AddTest(node1, ColWidth_auto_title, ColWidth_auto_descr, ColWidth_auto_html);
    AddTest(node1, ColWidth_fixed_title, ColWidth_fixed_descr, ColWidth_fixed_html);
    AddTest(node1, ColWidth_fixed_inline_title, ColWidth_fixed_inline_descr, ColWidth_fixed_inline_html);
    AddTest(node1, ColWidth_fixed_style_title, ColWidth_fixed_style_descr, ColWidth_fixed_style_html);
    AddTest(node1, ColWidth_100perc_title, ColWidth_100perc_descr, ColWidth_100perc_html);
    AddTest(node1, ColWidth_30perc_70perc_title, ColWidth_30perc_70perc_descr, ColWidth_30perc_70perc_html);
    AddTest(node1, ColWidth_200px_total100perc_title, ColWidth_200px_total100perc_descr, ColWidth_200px_total100perc_html);
    AddTest(node1, ColWidth_colspan200px_title, ColWidth_colspan200px_descr, ColWidth_colspan200px_html);
    AddTest(node1, ColWidth_colspan200px_100perc_title, ColWidth_colspan200px_100perc_descr, ColWidth_colspan200px_100perc_html);
    AddTest(node1, ColGroup_ColWidth_200px_total100perc_title, ColGroup_ColWidth_200px_total100perc_descr, ColGroup_ColWidth_200px_total100perc_html);
    node1.Expanded := true;
    node.Expanded := true;
    
    node := TestTree.Items.AddChild(nil, 'Lists');
    node1 := TestTree.Items.AddChild(node, 'Ordered');
    AddTest(node1, OL_title, OL_descr, OL_html);
    AddTest(node1, OL_typeA_title, OL_typeA_descr, OL_typeA_html);
    AddTest(node1, OL_typeA_inline_title, OL_typeA_inline_descr, OL_typeA_inline_html);
    AddTest(node1, OL_typeA_style_title, OL_typeA_style_descr, OL_typeA_style_html);
    AddTest(node1, OL_start_title, OL_start_descr, OL_start_html);
    AddTest(node1, OL_2lev_title, OL_2lev_descr, OL_2lev_html);
    AddTest(node1, OL_3lev_title, OL_3lev_descr, OL_3lev_html);
    node1.Expanded := true;
    node1 := TestTree.Items.AddChild(node, 'Unordered');
    AddTest(node1, UL_title, UL_descr, UL_html);
    AddTest(node1, UL_square_title, UL_square_descr, UL_square_html);
    AddTest(node1, UL_inline_title, UL_inline_descr, UL_inline_html);
    AddTest(node1, UL_style_title, UL_style_descr, UL_style_html);
    AddTest(node1, UL_individual_title, UL_individual_descr, UL_individual_html);
    AddTest(node1, UL_2lev_title, UL_2lev_descr, UL_2lev_html);
    AddTest(node1, UL_3lev_title, UL_3lev_descr, UL_3lev_html);
    node1.Expanded := true;
    node1 := TestTree.Items.AddChild(node, 'Mixed');
    AddTest(node1, OUL_3lev_title, OUL_3lev_descr, OUL_3lev_html);
    node1.Expanded := true;
    node.Expanded := true;
    
    node := TestTree.Items.AddChild(nil, 'CSS');
    AddTest(node, HTMLCommentInCSS_title, HTMLCommentInCSS_descr, HTMLCommentInCSS_html);
    node.Expanded := true;
    
    node := TestTree.Items.AddChild(nil, 'Special tags');
    node1 := TestTree.Items.AddChild(node, '<BR>');
    AddTest(node1, BRinBODY_title, BRinBODY_descr, BRinBODY_html);
    AddTest(node1, TwoBRinBODY_title, TwoBRinBODY_descr, TwoBRinBODY_html);
    AddTest(node1, BRinP_title, BRinP_descr, BRinP_html);
    AddTest(node1, TwoBRinP_title, TwoBRinP_descr, TwoBRinP_html);
    AddTest(node1, BRinTableCell_title, BRinTableCell_descr, BRinTableCell_html);
    AddTest(node1, TwoBRinTableCell_title, TwoBRinTableCell_descr, TwoBRinTableCell_html);
    AddTest(node1, BRbetweenTwoP_title, BRbetweenTwoP_descr, BRbetweenTwoP_html);
    AddTest(node1, BRbetweenTwoTables_title, BRbetweenTwoTables_descr, BRbetweenTwoTables_html);
    node1.Expanded := true;
    node1 := TestTree.Items.AddChild(node, '<PRE>');
    AddTest(node1, PRE_title, PRE_descr, PRE_html);
    node1.Expanded := true;
    node.Expanded := true;

    node := TestTree.Items.AddChild(nil, 'Special cases in file structure');
    AddTest(node, NoHtmlTag_title, NoHtmlTag_descr, NoHtmlTag_html);
    AddTest(node, NoBodyTag_title, NoBodyTag_descr, NoBodyTag_html);
    node.Expanded := true;
    
    node := TestTree.Items.AddChild(nil, 'Right-to-left');
    AddTest(node, Arab_title, Arab_descr, Arab_html);
    AddTest(node, Hebrew_title, Hebrew_descr, Hebrew_html);
    AddTest(node, Hebrew_bodyRTL_title, Hebrew_bodyRTL_descr, Hebrew_bodyRTL_html);
    AddTest(node, Hebrew_divRTL_title, Hebrew_divRTL_descr, Hebrew_divRTL_html);
    AddTest(node, Hebrew_pRTL_title, Hebrew_pRTL_descr, Hebrew_pRTL_html);
    AddTest(node, Table_RTL_title, Table_RTL_descr, Table_RTL_html);
    node.Expanded := true;
    
    LoadResults;
  finally
    TestTree.Items.EndUpdate;
  end;
end;

procedure TTestForm.LoadResults;
var
  L: TStringList;
  
  procedure LoadResult(ANode: TTreeNode);
  var
    testcase: TTestCase;
    idx: Integer;
  begin
    if ANode = nil then
      exit;
    testcase := TTestCase(ANode.Data);
    if testcase <> nil then
    begin
      if L.Find(MD5Print(testcase.MD5), idx) then
        ANode.ImageIndex := PtrInt(L.Objects[idx]);
    end else
      LoadResult(ANode.GetFirstChild);
    LoadResult(ANode.GetNextSibling);
  end;
  
var
  i, p, res: Integer;
  imgidx: PtrInt;
  fn: String;
begin
  fn := Application.Location + RESULTS_FILE;
  if not FileExists(fn) then
    exit;
  L := TStringList.Create;
  try
    L.LoadFromFile(fn);
    for i := 0 to L.Count-1 do begin
      p := pos('|', L[i]);
      val(copy(L[i], p+1), imgidx, res);
      L.Objects[i] := TObject(imgidx);
      L.Strings[i] := Copy(L[i], 1, p-1);
    end;
    L.Sorted := true;
    
    LoadResult(TestTree.Items.GetFirstNode);
  finally
    L.Free;
  end;
end;

procedure TTestForm.SaveResults;
var
  F: TextFile;

  procedure SaveTest(ANode: TTreeNode);
  var
    testcase: TTestCase;
  begin
    if ANode = nil then
      exit;
    testcase := TTestCase(ANode.Data);
    if testcase <> nil then
      WriteLn(F, MDPrint(testcase.MD5) + '|' + IntToStr(ANode.ImageIndex))
    else
      SaveTest(ANode.GetFirstChild);
    SaveTest(ANode.GetNextSibling);
  end;
    
begin
  AssignFile(F, Application.Location + RESULTS_FILE);
  Rewrite(F);
  SaveTest(TestTree.Items.GetFirstNode);
  CloseFile(F);
end;

end.

