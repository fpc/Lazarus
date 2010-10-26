{
  Copyright (C) 2004 Dean Zobec

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  
  Modified:
    Graeme Geldenhuys <graemeg@gmail.com>
    Darius Blaszijk <dhkblaszyk@zeelandnet.nl>
}

unit GuiTestRunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, ComCtrls, ActnList, Menus, Clipbrd, StdCtrls,
  testdecorator, xmltestreport,
  fpcunit, testregistry, SynEdit, SynHighlighterXML, gettext, Translations;

type

  { TGUITestRunner }

  TGUITestRunner = class(TForm, ITestListener)
    actCopy: TAction;
    actCut: TAction;
    ActCloseForm: TAction;
    actCopyErrorMsg: TAction;
    ActCheckCurrentSuite: TAction;
    ActCheckAll: TAction;
    ActRunHighlightedTest: TAction;
    ActUncheckAll: TAction;
    ActUncheckCurrentSuite: TAction;
    btnRunHighlighted: TBitBtn;
    ilNodeStates: TImageList;
    Memo1: TMemo;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    miRunTest: TMenuItem;
    miShowfailureMsg: TMenuItem;
    pbBar: TPaintBox;
    PopupMenu3: TPopupMenu;
    RunAction: TAction;
    ActionList1: TActionList;
    ActionList2: TActionList;
    BtnRun: TBitBtn;
    BtnClose: TBitBtn;
    TestTreeImageList: TImageList;
    ResultsXMLImageList: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SaveDialog: TSaveDialog;
    Splitter1: TSplitter;
    TestTree: TTreeView;
    SynXMLSyn1: TSynXMLSyn;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    XMLToolBar: TToolBar;
    CopyXMLToolButton: TToolButton;
    CutXMLToolButton: TToolButton;
    SaveAsToolButton: TToolButton;
    tsTestTree: TTabSheet;
    tsResultsXML: TTabSheet;
    XMLSynEdit: TSynEdit;
    procedure ActCheckAllExecute(Sender: TObject);
    procedure ActCheckCurrentSuiteExecute(Sender: TObject);
    procedure ActCloseFormExecute(Sender: TObject);
    procedure ActRunHighlightedTestExecute(Sender: TObject);
    procedure ActUncheckAllExecute(Sender: TObject);
    procedure ActRunHighLightedTestUpdate(Sender: TObject);
    procedure ActUncheckCurrentSuiteExecute(Sender: TObject);
    procedure RunExecute(Sender: TObject);
    procedure GUITestRunnerCreate(Sender: TObject);
    procedure GUITestRunnerShow(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure SaveAsToolButtonClick(Sender: TObject);
    procedure TestTreeMouseDown(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TestTreeSelectionChanged(Sender: TObject);
    procedure actCopyErrorMsgExecute(Sender: TObject);
    procedure actCopyErrorMsgUpdate(Sender: TObject);
    procedure pbBarPaint(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
  private
    failureCounter: Integer;
    errorCounter: Integer;
    testsCounter: Integer;
    skipsCounter: Integer;
    barColor: TColor;
    testSuite: TTest;
    procedure BuildTree(rootNode: TTreeNode; aSuite: TTestSuite);
    function  FindNode(aTest: TTest): TTreeNode;
    procedure ResetNodeColors;
    procedure PaintNodeError(aNode: TTreeNode);
    procedure PaintNodeFailure(aNode: TTreeNode);
    procedure PaintNodeNonFailed(aNode: TTreeNode);
    procedure PaintNodeBusy(aNode: TTreeNode);
    procedure MemoLog(LogEntry: string);
    procedure EnableRunActions(AValue: boolean);
  public
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure RunTest(ATest: TTest);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
  end;

var
  TestRunner: TGUITestRunner;

resourcestring
  rsAllTests = 'All Tests';
  rsRun = 'Run ';
  rsRuns = 'Runs: %s/%s';
  rsErrors = '%s    Errors: %s';
  rsFailures = '%s     Failures: %s';
  rsMessage = 'Message: %s';
  rsException = 'Exception: %s';
  rsExceptionMes = 'Exception message: %s';
  rsExceptionCla = 'Exception class: %s';
  rsUnitName = 'Unit name: %s';
  rsMethodName = 'Method name: %s';
  rsLineNumber = 'Line number: %s';
  rsRunning = 'Running %s';
  rsNumberOfExec = 'Number of executed tests: %s  Time elapsed: %s';
  // Visual components captions
  sfrmGUITest = 'FPCUnit - run unit test';
  sbtnRun = 'Run';
  sbtnRunH = 'Run highlighted test';
  sbtnClose = 'Close';
  stshTree = 'Testcase tree';
  stshResults = 'Results XML';
  sactRunAction = '&Run';
  sactRunActionH = 'Run all checked test(s)';
  sactCloseForm = 'Quit';
  sactCloseFormH = 'Quit Testting';
  sactCheckCurrentSuite = 'Check the Current Suite';
  sactUncheckCurrentSuite = 'Uncheck the Current Suite';
  sactCheckAll = 'Check all Tests';
  sactUncheckAll = 'Uncheck all tests';
  sactRunHighlightedTest = 'Run highlighted test';
  smiRunTest = '  &Run all selected (checked) tests';
  smiShowfail= 'Copy message to clipboard';
  smiCopy = '&Copy';
  smiCut = 'C&ut';
  smiCopyClipbrd = 'Copy to clipboard';

implementation

{$R *.lfm}

uses
  xmlwrite
  ;

type

  TTreeNodeState=(tsUnChecked, tsChecked);

function FirstLine(const s: string): string;
var
  NewLinePos: integer;
begin
  NewLinePos := pos(LineEnding, s);
  if NewLinePos > 0 then
    Result := copy(s, 1, NewLinePos-1)
  else
    Result := s;
end;

{ TGUITestRunner }

procedure TGUITestRunner.actCopyExecute(Sender: TObject);
begin
  Clipboard.AsText := XMLSynEdit.Lines.Text;
end;


procedure TGUITestRunner.actCutExecute(Sender: TObject);
begin
  Clipboard.AsText := XMLSynEdit.Lines.Text;
  XMLSynEdit.Lines.Clear;
end;


procedure TGUITestRunner.GUITestRunnerCreate(Sender: TObject);
begin
  barColor := clGreen;
  TestTree.Items.Clear;
  BuildTree(TestTree.Items.AddObject(nil, rsAllTests, GetTestRegistry),
    GetTestRegistry);
  PageControl1.ActivePage := tsTestTree;
  //
  BtnRun.Caption:= sbtnRun;
  btnRunHighlighted.Caption := sbtnRunH;
  BtnClose.Caption:= sbtnClose;
  tsTestTree.Caption:= stshTree;
  tsResultsXML.Caption:= stshResults;
  //
  Caption:= sfrmGUITest;
  RunAction.Caption:= sactRunAction;
  RunAction.Hint:= sactRunActionH;
  ActCloseForm.Caption:= sactCloseForm;
  ActCloseForm.Hint:= sactCloseFormH;
  ActCheckCurrentSuite.Caption:= sactCheckCurrentSuite;
  ActUncheckCurrentSuite.Caption:= sactUncheckCurrentSuite;
  ActCheckAll.Caption:= sactCheckAll;
  ActUncheckAll.Caption:= sactUncheckAll;
  ActRunHighlightedTest.Caption:= sactRunHighlightedTest;
  miRunTest.Caption:= smiRunTest;
  miShowfailureMsg.Caption:= smiShowfail;
  MenuItem1.Caption:= smiCopy;
  MenuItem2.Caption:= smiCut;
  MenuItem3.Caption:= smiCopyClipbrd;
end;

procedure TGUITestRunner.RunExecute(Sender: TObject);
begin
  testSuite := GetTestRegistry;
  TestTree.Selected := TestTree.Items[0];
  RunTest(testSuite);
end;

procedure TGUITestRunner.ActCloseFormExecute(Sender: TObject);
begin
  Close;
end;

procedure TGUITestRunner.ActCheckAllExecute(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to TestTree.Items.Count -1 do
    TestTree.Items[i].StateIndex := ord(tsChecked);
end;

procedure TGUITestRunner.ActCheckCurrentSuiteExecute(Sender: TObject);
var
  i: integer;
begin
  if (TestTree.Selected <> nil) and (TestTree.Selected.Data <> nil) then
  begin
    TestTree.Selected.StateIndex := ord(tsChecked);
    for i := 0 to TestTree.Selected.Count - 1 do
      TestTree.Selected.Items[i].StateIndex := ord(tsChecked);
  end;
end;

procedure TGUITestRunner.ActRunHighlightedTestExecute(Sender: TObject);
begin
  if (TestTree.Selected <> nil) and (TestTree.Selected.Data <> nil) then
  begin
    testSuite := TTest(TestTree.Selected.Data);
    TestTree.Selected.Expand(false);
  end;
  RunTest(testSuite);
end;

procedure TGUITestRunner.ActUncheckAllExecute(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to TestTree.Items.Count -1 do
    TestTree.Items[i].StateIndex := ord(tsUnChecked);
end;


procedure TGUITestRunner.ActRunHighlightedTestUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ((TestTree.Selected <> nil)
    and (TestTree.Selected.Data <> nil));
end;

procedure TGUITestRunner.ActUncheckCurrentSuiteExecute(Sender: TObject);
var
  i: integer;
begin
  if (TestTree.Selected <> nil) and (TestTree.Selected.Data <> nil) then
  begin
    TestTree.Selected.StateIndex := ord(tsUnchecked);
    for i := 0 to TestTree.Selected.Count - 1 do
      TestTree.Selected.Items[i].StateIndex := ord(tsUnChecked);
  end;
end;

procedure TGUITestRunner.GUITestRunnerShow(Sender: TObject);
begin
  if (ParamStrUTF8(1) = '--now') or (ParamStrUTF8(1) = '-n') then
    RunExecute(Self);
end;

procedure TGUITestRunner.MenuItem3Click(Sender: TObject);
begin
  Clipboard.AsText := Memo1.Lines.Text;
end;

procedure TGUITestRunner.SaveAsToolButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    XMLSynEdit.Lines.SaveToFile(UTF8ToSys(SaveDialog.FileName));
end;

procedure TGUITestRunner.TestTreeMouseDown(Sender: TOBject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  procedure ChangeCheck(aNode: TTreeNode; aCheck: TTreeNodeState);
  var
    i: integer;
    n: TTreeNode;
  begin
    if Assigned(aNode) then
    begin
      aNode.StateIndex := ord(aCheck);
      if (TTest(aNode.Data) is TTestSuite) then
        for i := 0 to aNode.Count - 1 do
        begin
          n := aNode.Items[i];
          ChangeCheck(n, aCheck);
        end;
    end;
  end;

var
  ht: THitTests;
  lNode: TTreeNode;
begin
  ht := (Sender as TTreeview).GetHitTestInfoAt(X, Y);
  if htOnStateIcon in ht then
  begin
    lNode := (Sender as TTreeview).GetNodeAt(X, Y);
    case lNode.StateIndex of
        0: ChangeCheck(lNode, tsChecked);
        1: ChangeCheck(lNode, tsUnChecked);
      end;
   end;
end;


procedure TGUITestRunner.TestTreeSelectionChanged(Sender: TObject);
begin
  if ((Sender as TTreeView).Selected <> nil) and
    Assigned((Sender as TTreeview).Selected.Data)  then
  begin
    btnRunHighlighted.Visible := true;
    btnRunHighlighted.Caption := rsRun + (Sender as TTreeview).Selected.Text;
  end
  else
    begin
      btnRunHighlighted.Visible := false;
      btnRunHighlighted.Caption := '';
    end;
end;


procedure TGUITestRunner.actCopyErrorMsgExecute(Sender: TObject);
begin
  ClipBoard.AsText := Copy(TestTree.Selected.text, 10, MaxInt)
end;


procedure TGUITestRunner.actCopyErrorMsgUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(TestTree.selected) and
    (Copy(TestTree.Selected.Text, 1, 9) = 'Message: ');
end;


procedure TGUITestRunner.pbBarPaint(Sender: TObject);
var
  msg: string;
  alltests: integer;
  OldStyle: TBrushStyle;
begin
  with (Sender as TPaintBox) do
  begin
    Canvas.Lock;
    Canvas.Brush.Color := clSilver;
    Canvas.Rectangle(0, 0, Width, Height);
    Canvas.Font.Color := clWhite;
    if Assigned(TestSuite) then
    begin
      if alltests - skipsCounter <> 0 then
      begin
        alltests := TestSuite.CountTestCases;
        if FailureCounter + ErrorCounter = 0 then
          barColor := clGreen;
        Canvas.Brush.Color := barColor;
        Canvas.Rectangle(0, 0, round(TestsCounter / (alltests - skipsCounter) * Width), Height);
        msg := Format(rsRuns, [IntToStr(TestsCounter), IntToStr(alltests -
          skipsCounter)]);
        msg := Format(rsErrors, [msg, IntToStr(ErrorCounter)]);
        msg := Format(rsFailures, [msg, IntToStr(FailureCounter)]);
        OldStyle := Canvas.Brush.Style;
        Canvas.Brush.Style := bsClear;
        Canvas.Textout(10, 10,  msg);
        Canvas.Brush.Style := OldStyle;
      end;
    end;
    Canvas.UnLock;
  end;
end;


procedure TGUITestRunner.BuildTree(rootNode: TTreeNode; aSuite: TTestSuite);
var
  node: TTreeNode;
  i: integer;
begin
  rootNode.StateIndex := Ord(tsChecked);
  for i := 0 to ASuite.Tests.Count - 1 do
  begin
    node := TestTree.Items.AddChildObject(rootNode, ASuite.Test[i].TestName, ASuite.Test[i]);
    if ASuite.Test[i] is TTestSuite then
      BuildTree(Node, TTestSuite(ASuite.Test[i]))
    else
      if TObject(ASuite.Test[i]).InheritsFrom(TTestDecorator) then
        BuildTree(Node, TTestSuite(TTestDecorator(ASuite.Test[i]).Test));
    node.ImageIndex := 12;
    node.SelectedIndex := 12;
    node.StateIndex := ord(tsChecked);
  end;
  rootNode.Expand(False);
  ResetNodeColors;
end;


function TGUITestRunner.FindNode(aTest: TTest): TTreeNode;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to TestTree.Items.Count -1 do
    if (TTest(TestTree.Items[i].data) = aTest) then
    begin
      Result :=  TestTree.Items[i];
      Exit;
    end;
end;


procedure TGUITestRunner.ResetNodeColors;
var
  i: integer;
begin
  for i := 0 to TestTree.Items.Count - 1 do
  begin
    TestTree.Items[i].ImageIndex := 12;
    TestTree.Items[i].SelectedIndex := 12;
  end;
end;


procedure TGUITestRunner.PaintNodeError(aNode: TTreeNode);
begin
  while Assigned(aNode) do
  begin
    aNode.ImageIndex := 2;
    aNode.SelectedIndex := 2;
    aNode.Expand(True);
    aNode := aNode.Parent;
    if Assigned(aNode) and ((aNode.ImageIndex in [0, 3, 12, 13]) or (ANode.ImageIndex = -1)) then
      PaintNodeError(aNode);
  end;
end;


procedure TGUITestRunner.PaintNodeFailure(aNode: TTreeNode);
begin
  while Assigned(aNode) do
  begin
    if ((aNode.ImageIndex in [0, 12, 13]) or (ANode.ImageIndex = -1)) then
    begin
      aNode.ImageIndex := 3;
      aNode.SelectedIndex := 3;
      aNode.Expand(true);
    end;
    aNode := aNode.Parent;
    if Assigned(aNode) and ((aNode.ImageIndex in [0, 12, 13]) or (ANode.ImageIndex = -1)) then
      PaintNodeFailure(aNode);
  end;
end;


procedure TGUITestRunner.PaintNodeNonFailed(aNode: TTreeNode);
var
  noFailedSibling: boolean;
  i: integer;
begin
  if Assigned(aNode) then
  begin
    if ((aNode.ImageIndex in [12, 13]) or (ANode.ImageIndex = -1)) then
    begin
      aNode.ImageIndex := 0;
      aNode.SelectedIndex := 0;
    end;
  end;
  if Assigned(aNode.Parent) then
    if aNode.Index = aNode.Parent.Count -1 then
    begin
    aNode := aNode.Parent;
    noFailedSibling := true;
    for i := 0 to aNode.Count -2 do
    begin
      if aNode.Items[i].ImageIndex <> 0 then
        noFailedSibling := false;;
    end;
    if (aNode.ImageIndex = 13) and noFailedSibling then
      PaintNodeNonFailed(aNode);
    end;
end;


procedure TGUITestRunner.PaintNodeBusy(aNode: TTreeNode);
var
  BusySibling: boolean;
  i: integer;
begin
  if Assigned(aNode) then
  begin
    aNode.ImageIndex := 13;
    aNode.SelectedIndex := 13;
  end;
  if Assigned(aNode.Parent) then
  begin
    if aNode.Index = aNode.Parent.Count -1 then
    begin
      aNode := aNode.Parent;
      BusySibling := true;
      for i := 0 to aNode.Count -2 do
      begin
        if aNode.Items[i].ImageIndex <> 0 then
          BusySibling := false;;
      end;
      if (aNode.ImageIndex = 12) and BusySibling then
        PaintNodeBusy(aNode);
    end;
  end;
end;


procedure TGUITestRunner.MemoLog(LogEntry: string);
begin
  Memo1.Lines.Add(TimeToStr(Now) + ' - ' + LogEntry);
end;

procedure TGUITestRunner.EnableRunActions(AValue: boolean);
begin
  ActRunHighlightedTest.Enabled := AValue;
  RunAction.Enabled := AValue;
end;


procedure TGUITestRunner.AddFailure(ATest: TTest; AFailure: TTestFailure);
var
  FailureNode, node: TTreeNode;
begin
  FailureNode := FindNode(ATest);
  if Assigned(FailureNode) then
  begin
    node := TestTree.Items.AddChild(FailureNode, Format(rsMessage, [
      FirstLine(AFailure.ExceptionMessage)]));
    node.ImageIndex := 4;
    node.SelectedIndex := 4;
    node := TestTree.Items.AddChild(FailureNode, Format(rsException, [
      AFailure.ExceptionClassName]));
    node.ImageIndex := 4;
    node.SelectedIndex := 4;
    PaintNodeFailure(FailureNode);
  end;
  Inc(failureCounter);
  if errorCounter = 0 then
    barColor := clFuchsia;
end;


procedure TGUITestRunner.AddError(ATest: TTest; AError: TTestFailure);
var
  ErrorNode, node: TTreeNode;
begin
  ErrorNode := FindNode(ATest);
  if Assigned(ErrorNode) then
  begin
    node := TestTree.Items.AddChild(ErrorNode, Format(rsExceptionMes, [
      FirstLine(AError.ExceptionMessage)]));
    node.ImageIndex := 4;
    node.SelectedIndex := 4;
    node := TestTree.Items.AddChild(ErrorNode, Format(rsExceptionCla, [
      AError.ExceptionClassName]));
    node.ImageIndex := 4;
    node.SelectedIndex := 4;
    if (AError.SourceUnitName <> '') and
      (AError.FailedMethodName <> '')
    then
    begin
      node := TestTree.Items.AddChild(ErrorNode, Format(rsUnitName, [
        AError.SourceUnitName]));
      node.ImageIndex := 11;
      node.SelectedIndex := 11;
      node := TestTree.Items.AddChild(ErrorNode, Format(rsMethodName, [
        AError.FailedMethodName]));
      node.ImageIndex := 11;
      node.SelectedIndex := 11;
      node := TestTree.Items.AddChild(ErrorNode, Format(rsLineNumber, [IntToStr(
        AError.LineNumber)]));
      node.ImageIndex := 11;
      node.SelectedIndex := 11;
    end;
    PaintNodeError(ErrorNode);
  end;
  Inc(errorCounter);
  barColor := clRed;
end;


procedure TGUITestRunner.StartTest(ATest: TTest);
var
  Node: TTreeNode;
begin
  TestTree.BeginUpdate;
  Node := FindNode(ATest);
  Node.DeleteChildren;
  PaintNodeBusy(Node);
  Node.MakeVisible;
  Application.ProcessMessages;
  TestTree.EndUpdate;
end;


procedure TGUITestRunner.EndTest(ATest: TTest);
var
  Node: TTreeNode;
begin
  TestTree.BeginUpdate;
  Inc(testsCounter);
  Node := FindNode(ATest);
  PaintNodeNonFailed(Node);
  pbbar.Refresh;
  Application.ProcessMessages;
  TestTree.EndUpdate;
end;

procedure TGUITestRunner.RunTest(ATest: TTest);
  procedure SkipUncheckedTests(aResult: TTestResult; aNode: TTreeNode);
  var
    i: integer;
  begin
    if (aNode.StateIndex = ord(tsUnChecked)) and (TTest(aNode.Data) is TTestCase) then
      aResult.AddToSkipList(TTest(aNode.Data) as TTestCase);
    for i := 0 to aNode.Count - 1 do
      SkipUncheckedTests(aResult, aNode.Items[i]);
  end;

var
  TestResult:TTestResult;
  w: TXMLResultsWriter;
  m: TMemoryStream;

begin
  barcolor := clGreen;
  ResetNodeColors;
  failureCounter := 0;
  errorCounter := 0;
  testsCounter := 0;
  skipsCounter := 0;
  EnableRunActions(false);
  TestResult := TTestResult.Create;
  try
    SkipUncheckedTests(TestResult, TestTree.Selected);
    skipsCounter := TestResult.NumberOfSkippedTests;
    TestResult.AddListener(self);
    pbBar.Invalidate;
    w := TXMLResultsWriter.Create(nil);
    w.FileName := 'null'; // prevents output to the console
    TestResult.AddListener(w);

    MemoLog(Format(rsRunning, [TestTree.Selected.Text]));
    aTest.Run(TestResult);
    MemoLog(Format(rsNumberOfExec, [IntToStr(TestResult.RunTests),
      FormatDateTime('hh:nn:ss.zzz', Now - TestResult.StartingTime)]));

    w.WriteResult(TestResult);
    m := TMemoryStream.Create;
    WriteXMLFile(w.Document, m);
    m.Position := 0;
    XMLSynEdit.Lines.LoadFromStream(m);

    pbBar.Invalidate;
   finally
    EnableRunActions(true);

    m.free;
    w.Free;
    TestResult.Free;
  end;
end;

procedure TGUITestRunner.StartTestSuite(ATestSuite: TTestSuite);
begin
  // do nothing
end;

procedure TGUITestRunner.EndTestSuite(ATestSuite: TTestSuite);
begin
  // do nothing
end;

procedure TranslateResStrings;
var
  Lang, FallbackLang, S: String;

begin
  GetLanguageIDs(Lang,FallbackLang); // in unit gettext
  S:=AppendPathDelim(AppendPathDelim(ExtractFileDir(ParamStr(0))) + 'languages');
  if FallbackLang = 'pt' then
     Lang := 'pb';
  TranslateUnitResourceStrings('guitestrunner',S+'guitestrunner.%s.po', Lang,FallbackLang);
end;

initialization
  TranslateResStrings;

end.

