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
  testreport, fpcunit, testregistry;

const
  S_OK = 0;                             {$EXTERNALSYM S_OK}
  S_FALSE = $00000001;                  {$EXTERNALSYM S_FALSE}
  E_NOINTERFACE = HRESULT($80004002);   {$EXTERNALSYM E_NOINTERFACE}

type

  { TGUITestRunner }

  TGUITestRunner = class(TForm, ITestListener)
    actCopy: TAction;
    actCut: TAction;
    ActionList1: TActionList;
    btnClose: TBitBtn;
    btnRun: TBitBtn;
    ComboBox1: TComboBox;
    ImageList1: TImageList;
    ImageList2: TImageList;
    Label1: TLabel;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    pbBar1: TPaintBox;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    XMLMemo: TMemo;
    pbBar: TPaintBox;
    Panel4: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    tsTestTree: TTabSheet;
    tsResultsXML: TTabSheet;
    TestTree: TTreeView;
    procedure BtnCloseClick(Sender: TObject);
    procedure GUITestRunnerCreate(Sender: TObject);
    procedure GUITestRunnerDestroy(Sender: TObject);
    procedure GUITestRunnerShow(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure TestTreeSelectionChanged(Sender: TObject);
    procedure pbBarPaint(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
  private
    { private declarations }
    suiteList: TStringList;
    currentTestNode: TTreeNode;
    failureCounter: Integer;
    errorCounter: Integer;
    testsCounter: Integer;
    barColor: TColor;
    testSuite: TTest;
  protected
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
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
var
  i: integer;
begin
  suiteList := TStringList.Create;
  barColor := clGray;
  for i := 0 to GetTestRegistry.Tests.Count - 1 do
    ComboBox1.Items.Add(GetTestRegistry.Test[i].TestName);
  ComboBox1.ItemIndex := 0;
end;

procedure TGUITestRunner.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TGUITestRunner.GUITestRunnerDestroy(Sender: TObject);
begin
  suiteList.Free;
end;

procedure TGUITestRunner.GUITestRunnerShow(Sender: TObject);
begin
  if (ParamStr(1) = '--now') or (ParamStr(1) = '-n') then
    BtnRunClick(Self);
end;

procedure TGUITestRunner.MenuItem3Click(Sender: TObject);
begin
  Clipboard.AsText := Memo1.Lines.Text;
end;

procedure TGUITestRunner.TestTreeSelectionChanged(Sender: TObject);
begin
  if (Sender as TTreeView).Selected <> nil then
    Memo1.Lines.Text := (Sender as TTreeview).Selected.Text
end;

procedure TGUITestRunner.pbBarPaint(Sender: TObject);
var
  msg: string;
begin
  with (Sender as TPaintBox) do
  begin
    Canvas.Brush.Color := clSilver;
    Canvas.Rectangle(0, 0, Width, Height);
    if Assigned(TestSuite) then
    begin
      if (FailureCounter = 0) and (ErrorCounter = 0) and
      (TestsCounter = TestSuite.CountTestCases) then
        barColor := clGreen;
      Canvas.Brush.Color := barColor;
      if TestsCounter <> 0 then
      begin
        Canvas.Rectangle(0, 0, round(TestsCounter / TestSuite.CountTestCases * Width), Height);
        Canvas.Font.Color := clWhite;
        msg := 'Runs: ' + IntToStr(TestsCounter);
        if ErrorCounter <> 0 then
          msg := msg + '    Number of test errors: ' + IntToStr(ErrorCounter);
        if (FailureCounter <> 0) then
        msg := msg + '     Number of test failures: ' + IntToStr(FailureCounter);
        Canvas.Textout(10, 10,  msg)
      end;
    end;
  end;
end;

procedure TGUITestRunner.btnRunClick(Sender: TObject);
var
  testResult: TTestResult;
begin
  barcolor := clGray;
  TestTree.items.Clear;
  suiteList.Clear;
  currentTestNode := nil;
  if ComboBox1.ItemIndex = 0 then
    testSuite := GetTestRegistry
  else
    testSuite := GetTestRegistry[ComboBox1.itemindex - 1];
  failureCounter := 0;
  errorCounter := 0;
  testsCounter := 0;
  testResult := TTestResult.Create;
  try
    testResult.AddListener(self);
    testSuite.Run(testResult);
    XMLMemo.lines.text:= '<TestResults>' + system.sLineBreak +
      TestResultAsXML(testResult) + system.sLineBreak + '</TestResults>';
  finally
    testResult.Free;
  end;
  pbBar.invalidate;
  pbBar1.invalidate;
end;

procedure TGUITestRunner.AddFailure(ATest: TTest; AFailure: TTestFailure);
var
  node: TTreeNode;
begin
  node := TestTree.Items.AddChild(currentTestNode, 'Message: ' + AFailure.ExceptionMessage);
  node.ImageIndex := 4;
  node.SelectedIndex := 4;
  node := TestTree.Items.AddChild(currentTestNode, 'Exception: ' + AFailure.ExceptionClassName);
  node.ImageIndex := 4;
  node.SelectedIndex := 4;
  currentTestNode.ImageIndex := 3;
  currentTestNode.SelectedIndex := 3;
  node := TTreeNode(suiteList.Objects[suiteList.IndexOf(ATest.TestSuiteName)]);
  node.ImageIndex := 3;
  node.SelectedIndex := 3;
  Inc(failureCounter);
  if errorCounter = 0 then
    barColor := clFuchsia;
end;

procedure TGUITestRunner.AddError(ATest: TTest; AError: TTestFailure);
var
  node: TTreeNode;
begin
  node := TestTree.Items.AddChild(currentTestNode, 'Exception message: ' + AError.ExceptionMessage);
  node.ImageIndex := 4;
  node.SelectedIndex := 4;
  node := TestTree.Items.AddChild(currentTestNode, 'Exception class: ' + AError.ExceptionClassName);
  node.ImageIndex := 4;
  node.SelectedIndex := 4;
  node := TestTree.Items.AddChild(currentTestNode, 'Unit name: ' + AError.SourceUnitName);
  node.ImageIndex := 11;
  node.SelectedIndex := 11;
  node := TestTree.Items.AddChild(currentTestNode, 'Method name: ' + AError.MethodName);
  node.ImageIndex := 11;
  node.SelectedIndex := 11;
  node := TestTree.Items.AddChild(currentTestNode, 'Line number: ' + IntToStr(AError.LineNumber));
  node.ImageIndex := 11;
  node.SelectedIndex := 11;
  currentTestNode.ImageIndex := 2;
  currentTestNode.SelectedIndex := 2;
  node := TTreeNode(suiteList.Objects[suiteList.IndexOf(ATest.TestSuiteName)]);
  node.ImageIndex := 2;
  node.SelectedIndex := 2;
  Inc(errorCounter);
  barColor := clRed;
end;

procedure TGUITestRunner.StartTest(ATest: TTest);
var
  parentNode: TTreeNode;
begin
  if suiteList.IndexOf(ATest.TestSuiteName) <> -1 then
  begin
    parentNode := TTreeNode(suiteList.Objects[suiteList.IndexOf(ATest.TestSuiteName)]);
  end
  else
    begin
      if TestTree.Items.Count = 0 then
      begin
        parentNode := TestTree.Items.AddFirst(nil, ATest.TestSuiteName);
      end
      else
        parentNode := TestTree.Items.Add(TTreeNode(suiteList.Objects[SuiteList.Count - 1]), ATest.TestSuiteName);
      suiteList.AddObject(ATest.TestSuiteName, parentNode);
    end;
  currentTestNode := TestTree.Items.AddChild(parentNode, ATest.TestName);
  Application.ProcessMessages;
end;

procedure TGUITestRunner.EndTest(ATest: TTest);
begin
  Inc(testsCounter);
  pbBar.invalidate;
  pbBar1.invalidate;
  Application.ProcessMessages;
end;

{ TGUITestRunner.IInterface }

function TGUITestRunner.QueryInterface(const IID: TGUID; out Obj): HResult; StdCall;
begin
  if GetInterface(IID, Obj) then Result := S_OK
    else Result := E_NOINTERFACE
end;

function TGUITestRunner._AddRef: Integer; StdCall;
begin
  Result := -1;
end;

function TGUITestRunner._Release: Integer; StdCall;
begin
  Result := -1;
end;

initialization
  {$I guitestrunner.lrs}

end.

