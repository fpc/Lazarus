{ Copyright (C) 2004 Dean Zobec

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
}
unit GuiTestRunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, ActnList, Menus, Clipbrd, StdCtrls,
  testdecorator,
  testreport, fpcunit, testregistry;

type

  { TGUITestRunner }

  TGUITestRunner = class(TForm, ITestListener)
    actCopy: TAction;
    actCut: TAction;
    ActCloseForm: TAction;
    actCopyErrorMsg: TAction;
    miRunTest: TMenuItem;
    miShowfailureMsg: TMenuItem;
    PopupMenu3: TPopupMenu;
    RunAction: TAction;
    ActionList1: TActionList;
    ActionList2: TActionList;
    BtnRun: TBitBtn;
    BtnClose: TBitBtn;
    ImageList1: TImageList;
    ImageList2: TImageList;
    Label1: TLabel;
    lblSelectedTest: TLabel;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Panel7: TPanel;
    Panel8: TPanel;
    pbBar: TPaintBox;
    Panel6: TPanel;
    pbBar1: TPaintBox;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Splitter1: TSplitter;
    TestTree: TTreeView;
    XMLMemo: TMemo;
    Panel4: TPanel;
    Panel5: TPanel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    tsTestTree: TTabSheet;
    tsResultsXML: TTabSheet;
    procedure ActCloseFormExecute(Sender: TObject);
    procedure RunActionUpdate(Sender: TObject);
    procedure RunExecute(Sender: TObject);
    procedure GUITestRunnerCreate(Sender: TObject);
    procedure GUITestRunnerShow(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
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
    barColor: TColor;
    testSuite: TTest;
    procedure BuildTree(rootNode: TTreeNode; aSuite: TTestSuite);
    function FindNode(aTest: TTest): TTreeNode;
    procedure ResetNodeColors;
    procedure PaintNodeError(aNode: TTreeNode);
    procedure PaintNodeFailure(aNode: TTreeNode);
    procedure PaintNodeNonFailed(aNode: TTreeNode);
    procedure MemoLog(LogEntry: string);
  public
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
  end;

var
  TestRunner: TGUITestRunner;

implementation

{ TGUITestRunner }

procedure TGUITestRunner.actCopyExecute(Sender: TObject);
begin
  Clipboard.AsText := XMLMemo.Lines.Text;
end;

procedure TGUITestRunner.actCutExecute(Sender: TObject);
begin
  Clipboard.AsText := XMLMemo.Lines.Text;
  XMLMemo.Lines.Clear;
end;

procedure TGUITestRunner.GUITestRunnerCreate(Sender: TObject);
begin
  barColor := clGreen;
  TestTree.Items.Clear;
  BuildTree(TestTree.Items.AddObject(nil, 'All Tests', GetTestRegistry), GetTestRegistry);
end;

procedure TGUITestRunner.RunExecute(Sender: TObject);
var
  testResult:TTestResult;
  FStopCrono: TDateTime;
  FStartCrono: TDateTime;
begin
  barcolor := clGreen;
  ResetNodeColors;

  if (TestTree.Selected <> nil) and (TestTree.Selected.Data <> nil) then
  begin
    testSuite := TTest(TestTree.Selected.Data);
    TestTree.Selected.Expand(false);
  end
  else
    begin
      testSuite := GetTestRegistry;
      TestTree.Selected := TestTree.Items[0];
    end;
  failureCounter := 0;
  errorCounter := 0;
  testsCounter := 0;
  testResult := TTestResult.Create;
  try
    testResult.AddListener(self);
    MemoLog('Running ' + TestTree.Selected.Text);
    FStartCrono := Now;
    testSuite.Run(testResult);
    FStopCrono := Now;
    MemoLog('Number of executed tests: ' + IntToStr(testResult.RunTests) + '  Time elapsed: ' +
    FormatDateTime('hh:nn:ss.zzz', FStopCrono - FStartCrono));
    XMLMemo.lines.text:= '<TestResults>' + system.sLineBreak +
    TestResultAsXML(testResult) + system.sLineBreak + '</TestResults>';
    pbBar.Invalidate;
    pbBar1.Invalidate;
   finally
    testResult.Free;
  end;
end;

procedure TGUITestRunner.ActCloseFormExecute(Sender: TObject);
begin
  Close;
end;

procedure TGUITestRunner.RunActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ((TestTree.Selected <> nil)
    and (TestTree.Selected.Data <> nil)) or (not TestTree.Focused);
end;

procedure TGUITestRunner.GUITestRunnerShow(Sender: TObject);
begin
  if (ParamStr(1) = '--now') or (ParamStr(1) = '-n') then
    RunExecute(Self);
end;

procedure TGUITestRunner.MenuItem3Click(Sender: TObject);
begin
  Clipboard.AsText := Memo1.Lines.Text;
end;

procedure TGUITestRunner.TestTreeSelectionChanged(Sender: TObject);
begin
  if ((Sender as TTreeView).Selected <> nil) and
    Assigned((Sender as TTreeview).Selected.Data)  then
    lblSelectedTest.Caption := (Sender as TTreeview).Selected.Text
  else
    lblSelectedTest.Caption := '';
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
begin
  with (Sender as TPaintBox) do
  begin
    Canvas.Lock;
    Canvas.Brush.Color := clSilver;
    Canvas.Rectangle(0, 0, Width, Height);
    Canvas.Font.Color := clWhite;
    if Assigned(TestSuite) then
    begin
      alltests := TestSuite.CountTestCases;
      if FailureCounter + ErrorCounter = 0 then
        barColor := clGreen;
      Canvas.Brush.Color := barColor;
      if TestsCounter <> 0 then
      begin
        Canvas.Rectangle(0, 0, round(TestsCounter / alltests * Width), Height);
        msg := 'Runs: ' + IntToStr(TestsCounter) + '/' + IntToStr(alltests);
        msg := msg + '    Errors: ' + IntToStr(ErrorCounter);
        msg := msg + '     Failures: ' + IntToStr(FailureCounter);
        Canvas.Textout(10, 10,  msg)
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
    if Assigned(aNode) and (aNode.ImageIndex in [0, 3, 12, -1]) then
      PaintNodeError(aNode);
  end;
end;

procedure TGUITestRunner.PaintNodeFailure(aNode: TTreeNode);
begin
  while Assigned(aNode) do
  begin
    if aNode.ImageIndex in [0, -1, 12] then
    begin
      aNode.ImageIndex := 3;
      aNode.SelectedIndex := 3;
      aNode.Expand(true);
    end;
    aNode := aNode.Parent;
    if Assigned(aNode) and (aNode.ImageIndex in [0, -1, 12]) then
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
    if aNode.ImageIndex in [12, -1] then
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
    if (aNode.ImageIndex = 12) and noFailedSibling then
      PaintNodeNonFailed(aNode);
    end;
end;

procedure TGUITestRunner.MemoLog(LogEntry: string);
begin
  Memo1.Lines.Add(TimeToStr(Now) + ' - ' + LogEntry);
end;


procedure TGUITestRunner.AddFailure(ATest: TTest; AFailure: TTestFailure);
var
  FailureNode, node: TTreeNode;
begin
  FailureNode := FindNode(ATest);
  if Assigned(FailureNode) then
  begin
    FailureNode.DeleteChildren;
    node := TestTree.Items.AddChild(FailureNode, 'Message: ' + AFailure.ExceptionMessage);
    node.ImageIndex := 4;
    node.SelectedIndex := 4;
    node := TestTree.Items.AddChild(FailureNode, 'Exception: ' + AFailure.ExceptionClassName);
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
    ErrorNode.DeleteChildren;
    node := TestTree.Items.AddChild(ErrorNode, 'Exception message: ' + AError.ExceptionMessage);
    node.ImageIndex := 4;
    node.SelectedIndex := 4;
    node := TestTree.Items.AddChild(ErrorNode, 'Exception class: ' + AError.ExceptionClassName);
    node.ImageIndex := 4;
    node.SelectedIndex := 4;
    if (AError.SourceUnitName <> '') and
      (AError.FailedMethodName <> '')
    then
    begin
      node := TestTree.Items.AddChild(ErrorNode, 'Unit name: ' + AError.SourceUnitName);
      node.ImageIndex := 11;
      node.SelectedIndex := 11;
      node := TestTree.Items.AddChild(ErrorNode, 'Method name: ' +
      AError.FailedMethodName);
      node.ImageIndex := 11;
      node.SelectedIndex := 11;
      node := TestTree.Items.AddChild(ErrorNode, 'Line number: ' + IntToStr(AError.LineNumber));
      node.ImageIndex := 11;
      node.SelectedIndex := 11;
    end;
    PaintNodeError(ErrorNode);
  end;
  Inc(errorCounter);
  barColor := clRed;
end;

procedure TGUITestRunner.StartTest(ATest: TTest);
begin
  if ATest=0 then ;
end;

procedure TGUITestRunner.EndTest(ATest: TTest);
var
  Node: TTreeNode;
begin
  Inc(testsCounter);
  Node := FindNode(ATest);
  PaintNodeNonFailed(Node);
  pbbar.Refresh;
  pbbar1.Refresh;
end;

initialization
  {$I guitestrunner.lrs}

end.

