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

  TGUITestRunner = class(TForm, ITestListener)
    actCopy: TAction;
    actCut: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    btnRun: TBitBtn;
    ComboBox1: TComboBox;
    ImageList1: TImageList;
    ImageList2: TImageList;
    Label1: TLabel;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    XMLMemo: TMemo;
    PaintBox1: TPaintBox;
    Panel4: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    tsTestTree: TTabSheet;
    tsResultsXML: TTabSheet;
    TreeView1: TTreeView;
    procedure BitBtn1Click(Sender: TObject);
    procedure GUITestRunnerCreate(Sender: TObject);
    procedure GUITestRunnerDestroy(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure PaintBox1Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Click(Sender: TObject);
    procedure XMLMemoChange(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
  private
    { private declarations }
    suiteList: TStringList;
    currentTestNode: TTreeNode;
    EnabledTestsCount: Integer;
    failureCounter: Integer;
    errorCounter: Integer;
    testsCounter: Integer;
    barColor: TColor;
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
    procedure DrawBar;
  end; 

var
  TestRunner: TGUITestRunner;

implementation

{ TGUITestRunner }

procedure TGUITestRunner.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin

end;

procedure TGUITestRunner.TreeView1Click(Sender: TObject);
var
  Node: TTreeNode;
begin
  if TreeView1.Selected <> nil then
  begin
     Memo1.Lines.Clear;
     Node := TreeView1.Selected;
     if (Node.Level = 2) then
       if (TObject(Node.Data) is TTestFailure)  then
       begin
         Memo1.Lines.Add('Exception Message: ' + TTestFailure(Node.Data).ExceptionMessage);
         Memo1.Lines.Add('Exception Class Name: ' + TTestFailure(Node.Data).ExceptionClassName);
         if TTestFailure(Node.Data).SourceUnitName <> '' then
         begin
           Memo1.Lines.Add('Unit Name: ' + TTestFailure(Node.Data).SourceUnitName);
           Memo1.Lines.Add('Method Name: ' + TTestFailure(Node.Data).MethodName);
           Memo1.Lines.Add('Line Number: ' + IntToStr(TTestFailure(Node.Data).LineNumber));
         end;
       end;
  end;
end;

procedure TGUITestRunner.XMLMemoChange(Sender: TObject);
begin

end;

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

procedure TGUITestRunner.BitBtn1Click(Sender: TObject);
begin
  Close;
end;


procedure TGUITestRunner.GUITestRunnerDestroy(Sender: TObject);
begin
  suiteList.Free;
end;

procedure TGUITestRunner.MenuItem3Click(Sender: TObject);
begin
  Clipboard.AsText := Memo1.Lines.Text;
end;

procedure TGUITestRunner.PaintBox1Click(Sender: TObject);
begin

end;

procedure TGUITestRunner.PaintBox1Paint(Sender: TObject);
var
  msg: string;
begin
  with PaintBox1 do
  begin
    Canvas.Brush.Color := clGray;
    Canvas.Rectangle(0, 0, Width, Height);
    if (FailureCounter = 0) and (ErrorCounter = 0) then
       barColor := clGreen;
    Canvas.Brush.Color := barColor;
    if TestsCounter <> 0 then
    begin
      Canvas.Rectangle(0, 0, round((TestsCounter{- FailureCounter- ErrorCounter})/EnabledTestsCount*
        Width), Height);
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

procedure TGUITestRunner.btnRunClick(Sender: TObject);
var
  testResult: TTestResult;
  testSuite: TTest;
begin
  TreeView1.items.Clear;
  suiteList.Clear;
  currentTestNode := nil;
  if ComboBox1.ItemIndex = 0 then
    testSuite := GetTestRegistry
  else
    testSuite := GetTestRegistry[ComboBox1.itemindex - 1];
  enabledTestsCount := testSuite.CountTestCases;
  failureCounter := 0;
  errorCounter := 0;
  testsCounter := 0;
  testResult := TTestResult.Create;
  try
    testResult.AddListener(self);
    testSuite.Run(testResult);
    XMLMemo.lines.text:= TestResultAsXML(testResult);
  finally
    testResult.Free;
  end;
  PaintBox1.invalidate;
end;

procedure TGUITestRunner.AddFailure(ATest: TTest; AFailure: TTestFailure);
var
  node: TTreeNode;
begin
  node := TreeView1.Items.AddChildObject(currentTestNode, 'Message: ' + AFailure.ExceptionMessage, AFailure);
  node.ImageIndex := 4;
  node.SelectedIndex := 4;
  node := TreeView1.Items.AddChildObject(currentTestNode, 'Exception: ' + AFailure.ExceptionClassName, AFailure);
  node.ImageIndex := 4;
  node.SelectedIndex := 4;
  currentTestNode.ImageIndex := 3;
  currentTestNode.SelectedIndex := 3;
  node := TTreeNode(suiteList.Objects[suiteList.IndexOf(ATest.TestSuiteName)]);
  node.ImageIndex := 3;
  node.SelectedIndex := 3;
  Inc(failureCounter);
  if BarColor <> clRed then
    barColor := clFuchsia;
end;

procedure TGUITestRunner.AddError(ATest: TTest; AError: TTestFailure);
var
  node: TTreeNode;
begin
  node := TreeView1.Items.AddChildObject(currentTestNode, 'Exception message: ' + AError.ExceptionMessage, AError);
  node.ImageIndex := 4;
  node.SelectedIndex := 4;
  node := TreeView1.Items.AddChildObject(currentTestNode, 'Exception class: ' + AError.ExceptionClassName, AError);
  node.ImageIndex := 4;
  node.SelectedIndex := 4;
  node := TreeView1.Items.AddChildObject(currentTestNode, 'Unit name: ' + AError.SourceUnitName, AError);
  node.ImageIndex := 11;
  node.SelectedIndex := 11;
  node := TreeView1.Items.AddChildObject(currentTestNode, 'Method name: ' + AError.MethodName, AError);
  node.ImageIndex := 11;
  node.SelectedIndex := 11;
  node := TreeView1.Items.AddChildObject(currentTestNode, 'Line number: ' + IntToStr(AError.LineNumber), AError);
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
      if TreeView1.Items.Count = 0 then
      begin
        parentNode := TreeView1.Items.AddFirst(nil, ATest.TestSuiteName);
      end
      else
        parentNode := TreeView1.Items.Add(TTreeNode(suiteList.Objects[SuiteList.Count - 1]), ATest.TestSuiteName);
      suiteList.AddObject(ATest.TestSuiteName, parentNode);
    end;
  currentTestNode := TreeView1.Items.AddChildObject(parentNode, ATest.TestName, ATest);
  Application.ProcessMessages;
end;

procedure TGUITestRunner.EndTest(ATest: TTest);
begin
  Inc(testsCounter);
  PaintBox1.invalidate;
  Application.ProcessMessages;
end;

procedure TGUITestRunner.DrawBar;
begin

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

