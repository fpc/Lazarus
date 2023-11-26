unit frmtestinsight;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, ComCtrls, ActnList, Menus, Clipbrd, StdCtrls,
  LCLProc, AsyncProcess, IniFiles, fpJSON, testinsightprotocol,
  testinsightserver, jsonparser, types, StrUtils, strtestcaseopts;


type
  { TTestInsightForm }

  TTestInsightForm = class(TForm)
    ActCloseForm: TAction;
    ActCopyErrorMsg: TAction;
    ActCheckCurrentSuite: TAction;
    ActCheckAll: TAction;
    ActCopyTextToClipboard: TAction;
    aRefresh: TAction;
    ActRunHighlightedTest: TAction;
    ActUncheckAll: TAction;
    ActUncheckCurrentSuite: TAction;
    pTest: TAsyncProcess;
    ilNodeStates: TImageList;
    MMtestInsight: TMainMenu;
    mDetails: TMemo;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemTestTree: TMenuItem;
    MenuItemActions: TMenuItem;
    miExpandNodes: TMenuItem;
    miCollapseNodes: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    miRunTest: TMenuItem;
    miShowfailureMsg: TMenuItem;
    pbBar: TPaintBox;
    pmResults: TPopupMenu;
    actRunAll: TAction;
    alTestInsight: TActionList;
    sbTestInsight: TStatusBar;
    TestTree: TTreeView;
    TestTreeImageList: TImageList;
    ILMenu: TImageList;
    miRefresh: TMenuItem;
    MenuItemCopyText: TMenuItem;
    pmDetails: TPopupMenu;
    Panel1: TPanel;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    actNextError: TAction;
    MenuItem20: TMenuItem;
    actPrevError: TAction;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    procedure actNextErrorUpdate(Sender: TObject);
    procedure actPrevErrorUpdate(Sender: TObject);
    procedure actRunAllUpdate(Sender: TObject);
    procedure aRefreshExecute(Sender: TObject);
    procedure aRefreshUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure pTestReadData(Sender: TObject);
    procedure TestInsightFormCreate(Sender: TObject);
    procedure TestInsightFormDestroy(Sender: TObject);
    procedure ActCheckAllExecute(Sender: TObject);
    procedure ActCheckCurrentSuiteExecute(Sender: TObject);
    procedure ActCloseFormExecute(Sender: TObject);
    procedure ActCopyTextToClipboardExecute(Sender: TObject);
    procedure ActCopyTextToClipboardUpdate(Sender: TObject);
    procedure ActRunHighlightedTestExecute(Sender: TObject);
    procedure ActUncheckAllExecute(Sender: TObject);
    procedure ActRunHighLightedTestUpdate(Sender: TObject);
    procedure ActUncheckCurrentSuiteExecute(Sender: TObject);
    procedure ActCopyErrorMsgExecute(Sender: TObject);
    procedure ActCopyErrorMsgUpdate(Sender: TObject);
    procedure actNextErrorExecute(Sender: TObject);
    procedure actPrevErrorExecute(Sender: TObject);
    procedure miCollapseNodesClick(Sender: TObject);
    procedure miExpandNodesClick(Sender: TObject);
    procedure RunAllExecute(Sender: TObject);
    procedure TestTreeChange(Sender: TObject; Node: TTreeNode);
    procedure TestTreeCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    procedure TestTreeDblClick(Sender: TObject);
    procedure TestTreeMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure TestTreeSelectionChanged(Sender: TObject);
    procedure pbBarPaint(Sender: TObject);
  private
    FConfStoreFile : String;
    FTestProject : String;
    FTestCount : Integer;
    failureCounter: Integer;
    errorCounter: Integer;
    testsCounter: Integer;
    skipsCounter: Integer;
    barColor: TColor;
    FTestSuite : TTestItem;
    FSelectedTestSuite: TTestItem;
    FFirstFailure: TTreeNode; // reference to first failed test
    FServer : TTestInsightServer;
    FTestsRunning : Boolean;
    procedure AddSelected(N: TTreeNode; var Tests: TStringDynArray; var aLen: Integer);
    // Callbacks for server
    procedure DoClearTests(Sender: TObject);
    procedure DoFinishedTests(Sender: TObject);
    procedure DoGetSelectedTests(Sender: TObject; var Tests: TStringDynArray);
    procedure DoSetTestResult(Sender: TObject; aResult: TTestInsightResultArray);
    procedure DoGetOptions(Sender: TObject; aOptions: TTestInsightOptions);
    procedure DoSetTestSuite(Sender: TObject; Tests: TTestItem);
    procedure DoStartTests(Sender: TObject; aCount: Integer);
    // Tree handling
    function  FindNode(aTest: String): TTreeNode;
    function HaveErrors: Boolean;
    function  MakeTestPath(Node: TTreeNode): string;
    procedure ResetNodeColors;
    procedure PaintNodeError(aNode: TTreeNode);
    procedure PaintNodeFailure(aNode: TTreeNode);
    procedure PaintNodeIgnore(aNode: TTreeNode);
    procedure PaintNodeNonFailed(aNode: TTreeNode);
    procedure PaintNodeBusy(aNode: TTreeNode);
    procedure EnableRunActions(AValue: boolean);
    procedure ShowDetails(const Node: TTreeNode);
  Protected
    procedure ClearDetails;
    procedure CreateTestRunConfigFile(aConfStoreFile: string; aSendNamesOnly: Boolean);
    procedure ShowTestSuite;
    procedure RestoreTree;
    procedure SaveTree;
    procedure BuildTree(rootNode: TTreeNode; aSuite: TTestItem);
    Procedure SetFirstFailure(aNode: TTreeNode);
  public
    procedure AddFailure(aNode : TTreeNode; ATest: TTestItem);
    procedure AddError(aNode : TTreeNode; ATest: TTestItem);
    procedure StartTest(ATest: String);
    procedure EndTest(aNode : TTreeNode; ATest: TTestItem);
    procedure RunTest(ATest: TTestItem); virtual;
    procedure NextError;
    procedure PrevError;
    procedure MaybeStartTestProject;
    // test project handling. Needs to integrate with Lazarus IDE.
    function ShowRefreshTestproject : Boolean; virtual;
    function AutoFetchTests : Boolean; virtual;
    function GetBaseURL: String; virtual;
    function GetTestProject : String; virtual;
    procedure RunTestProject(aExecutable : String; SendNamesOnly : Boolean);  virtual;
    function TestRunning : Boolean; virtual;
    procedure ConfigureServer(aServer: TTestInsightServer); virtual;
    function CreateServer(aOwner : TComponent) : TTestInsightServer; virtual;
    procedure NavigateTo(const {%H-}aClass, {%H-}aMethod,{%H-}aUnit,{%H-}aLocationFile : String; {%H-}aLocationLine : Integer); virtual;
    procedure DoneServer(aServer :TTestInsightServer); virtual;
    procedure ShowMessage(Const Msg : String); virtual;
    procedure HandleServerLog(Sender: TObject; const aMessage: String); virtual;
    Property Server : TTestInsightServer Read FServer;
  public
  end;

implementation

{$R *.lfm}

const
  // TestTreeImageList indexes:
  imgGreenBall = 0;    //success result
  imgRedBall = 2;      // error result
  imgPurpleBall = 3;   //failure result
  imgWarningSign = 4;
  imgInfoSign = 11;
  imgGrayBall = 12;    //default
  imgBlueBall = 13;    //busy


const
  SectionName_TestNodes = 'Tests';

type
  TTreeNodeState=(tsUnChecked, tsChecked);

  { TMessageTreeNode }

  TMessageTreeNode = class(TTreeNode)
  private
    FMessage: string;
  public
    property Message: string read FMessage write FMessage;
  end;


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

{ TTestInsightForm }


function TTestInsightForm.MakeTestPath(Node: TTreeNode): string;
begin
  Result := Node.Text;
  Node := Node.Parent;
  If Node = Nil then Exit;
  // We can now skip the unnecessary "All Tests" text
  while Node.Parent <> nil do
  begin
    Result := Node.Text + '.' + Result;
    Node := Node.Parent;
  end;
end;


procedure TTestInsightForm.SaveTree;

  function SkipNode(const Node: TTreeNode): boolean;
  begin
    Result := Node.Data = nil;
  end;

var
  i: integer;
  NodePath  : String;
begin
  With TMemIniFile.Create(FConfStoreFile) do
    Try
      EraseSection(SectionName_TestNodes);
      for i := 0 to TestTree.Items.Count-1 do
        begin
        if SkipNode(TestTree.Items[i]) then
          continue;
        NodePath:=MakeTestPath(TestTree.Items[i]);
        WriteBool(SectionName_TestNodes,NodePath+ '.Checked',
                  TestTree.Items[i].StateIndex = Ord(tsChecked));
        WriteBool(SectionName_TestNodes,NodePath + '.Expanded',
                  TestTree.Items[i].Expanded);
        end;
      UpdateFile;
   Finally
     Free;
   end;
end;

procedure TTestInsightForm.RestoreTree;

Const
  ImgIdx : Array[Boolean] of Integer = (Ord(tsUnChecked),Ord(tsChecked));

var
  i: integer;
  NodePath : String;
  aIni : TMemIniFile;

begin
  aIni:=TMemIniFile.Create(FConfStoreFile);
  Try
    if not aIni.SectionExists(SectionName_TestNodes) then
     Exit;
    for i := 0 to TestTree.Items.Count - 1 do
     begin
     NodePath:=MakeTestPath(TestTree.Items[i]);
     With TestTree.Items[i] do
       begin
       Expanded:=aIni.ReadBool(SectionName_TestNodes, NodePath+ '.Expanded', Expanded);
       StateIndex:=ImgIdx[aIni.ReadBool(SectionName_TestNodes,NodePath+ '.Checked',True)];
       end;
     end;
  finally
    aIni.Free;
  end;
end;

procedure TTestInsightForm.ShowTestSuite;

begin
  FTestCount:=FTestSuite.CountTestCases;
  TestTree.BeginUpdate;
  try
    TestTree.Items.Clear;
    BuildTree(TestTree.Items.AddObject(nil, rsAllTests, FTestSuite), FTestSuite);
    RestoreTree;
    // Select the first entry in the tree in order to immediately activate the
    // Run All tests button:
    if TestTree.Items.Count>0 then
    begin
      TestTree.Items.SelectOnlyThis(TestTree.Items[0]);
      TestTree.Items[0].Expand(False);
    end;
  finally
    TestTree.EndUpdate;
  end;
end;

procedure TTestInsightForm.TestInsightFormCreate(Sender: TObject);
begin
  barColor := clGreen;
  TestTree.Items.Clear;

  Caption:= sfrmGUITest;
  actRunAll.Caption:= sactRunAction;
  actRunAll.Hint:= sactRunActionH;
  ActCloseForm.Caption:= sactCloseForm;
  ActCloseForm.Hint:= sactCloseFormH;
  ActCheckCurrentSuite.Caption:= sactCheckCurrentSuite;
  ActUncheckCurrentSuite.Caption:= sactUncheckCurrentSuite;
  ActCheckAll.Caption:= sactCheckAll;
  ActUncheckAll.Caption:= sactUncheckAll;
  ActRunHighlightedTest.Caption:= sactRunHighlightedTest;
  ActRunHighlightedTest.Hint := sactRunHighlightedTestH;
  MenuItemActions.Caption := smiActions;
  MenuItemTestTree.Caption := smiTestTree;
  MenuItemEdit.Caption := smiEdit;
  miCollapseNodes.Caption := smiCollapseNodes;
  miExpandNodes.Caption := smiExpandNodes;
  actNextError.Caption := rsNextError;
  actPrevError.Caption := rsPreviousError;
  ActCopyErrorMsg.Caption := sactCopyMessageToClipboard;
  ActCopyTextToClipboard.Caption := sactCopyAllToClipboard;
  ActCopyTextToClipboard.Hint := sactCopyAllToClipboardH;
  FServer:=CreateServer(Self);
  FServer.OnClearTests:=@DoClearTests;
  FServer.OnSetTestNames:=@DoSetTestSuite;
  FServer.OnTestResult:=@DoSetTestResult;
  FServer.OnGetSelectedTests:=@DoGetSelectedTests;
  FServer.OnTestsStarted:=@DoStartTests;
  FServer.OnTestsFinished:=@DoFinishedTests;
  FServer.OnLog:=@HandleServerLog;
  ConfigureServer(FServer);
  FServer.StartServer;
  MaybeStartTestProject;
end;

procedure TTestInsightForm.MaybeStartTestProject;

Var
  ProjectPath : String;

begin
  FTestProject:=GetTestProject;
  if (FTestProject<>'') then
    begin
    sbTestInsight.SimpleText:=FTestProject;
    ProjectPath:=ExtractFilePath(FTestProject);
    FConfStoreFile := ProjectPath + 'TestInsightSettings.ini';
    if AutoFetchTests then
      RunTestProject(FTestProject,True); // Get all tests
    end
  else
    sbTestInsight.SimpleText:=SNoTestProjectConfigured;

end;

procedure TTestInsightForm.ConfigureServer(aServer: TTestInsightServer);

begin
  aServer.Port:=6789;
  aServer.BasePath:='/tests';

end;

function TTestInsightForm.CreateServer(aOwner: TComponent): TTestInsightServer;
begin
  Result:=TTestInsightServer.Create(aOwner)
end;

procedure TTestInsightForm.NavigateTo(const aClass,aMethod, aUnit, aLocationFile: String; aLocationLine: Integer);
begin
  ShowMessage(SNavigationNotAvailable);
end;

procedure TTestInsightForm.DoneServer(aServer: TTestInsightServer);
begin
  aServer.Free;
end;

procedure TTestInsightForm.ShowMessage(const Msg: String);
begin
  Dialogs.ShowMessage(Msg);
end;


procedure TTestInsightForm.pTestReadData(Sender: TObject);

Type
  TBuffer = Array[1..4096] of byte;

Var
  Count : Integer;
  Buf : TBuffer;

begin
  Buf:=Default(TBuffer);
  // We just clear the buffer
  Count:=PTest.NumBytesAvailable;
  While Count>0 do
    Count:=Count-pTest.Output.Read(Buf,SizeOf(Buf));
end;

procedure TTestInsightForm.actRunAllUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Not Self.TestRunning;
end;

function TTestInsightForm.HaveErrors : Boolean;

begin
  Result:=(failureCounter>0) or (errorCounter>0);
end;

procedure TTestInsightForm.actNextErrorUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=HaveErrors;
end;

procedure TTestInsightForm.actPrevErrorUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=HaveErrors;
end;

procedure TTestInsightForm.aRefreshExecute(Sender: TObject);
begin
  MaybeStartTestProject;
end;

procedure TTestInsightForm.aRefreshUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=ShowRefreshTestproject;
end;

procedure TTestInsightForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
  SaveTree;
end;


procedure TTestInsightForm.RunAllExecute(Sender: TObject);
begin
  FFirstFailure := nil;
  FSelectedTestSuite:=Nil;
  RunTestProject(FTestProject,False);
end;

procedure TTestInsightForm.ActCloseFormExecute(Sender: TObject);
begin
  Close;
end;

procedure TTestInsightForm.ActCopyTextToClipboardExecute(Sender: TObject);
begin
  Clipboard.AsText := mDetails.Lines.Text
end;

procedure TTestInsightForm.ActCopyTextToClipboardUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (ActiveControl = mDetails) ;
end;

procedure TTestInsightForm.ActCheckAllExecute(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to TestTree.Items.Count -1 do
    TestTree.Items[i].StateIndex := ord(tsChecked);
end;

procedure TTestInsightForm.ActCheckCurrentSuiteExecute(Sender: TObject);
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

procedure TTestInsightForm.ActRunHighlightedTestExecute(Sender: TObject);

Var
  N : TTreeNode;

begin
  FFirstFailure := nil;
  N:=TestTree.Selected;
  if (N<>nil) then
    begin
    FSelectedTestSuite := TTestItem(N.Data);
    if Assigned(FSelectedTestSuite) then
      RunTest(FSelectedTestSuite);
    end;
  TestTree.MakeSelectionVisible;
end;

procedure TTestInsightForm.ActUncheckAllExecute(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to TestTree.Items.Count -1 do
    TestTree.Items[i].StateIndex := ord(tsUnChecked);
end;


procedure TTestInsightForm.ActRunHighLightedTestUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ((TestTree.Selected <> nil)
    and (TestTree.Selected.Data <> nil)) and not TestRunning;
end;

procedure TTestInsightForm.ActUncheckCurrentSuiteExecute(Sender: TObject);
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

procedure TTestInsightForm.TestInsightFormDestroy(Sender: TObject);
begin
  DoneServer(FServer);
  FreeAndNil(FTestSuite);
end;

procedure TTestInsightForm.miCollapseNodesClick(Sender: TObject);
begin
  if not Assigned(TestTree.Selected) then
    Exit;
  TestTree.Selected.Collapse(True);
end;

procedure TTestInsightForm.miExpandNodesClick(Sender: TObject);
begin
  if not Assigned(TestTree.Selected) then
    Exit;
  TestTree.Selected.Expand(True);
end;

procedure TTestInsightForm.TestTreeChange(Sender: TObject; Node: TTreeNode);
begin
  if not Assigned(Node) then
    Exit;
  //mDetails.Lines.Text := TMessageTreeNode(Node).Message;
end;

procedure TTestInsightForm.TestTreeCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TMessageTreeNode;
end;

procedure TTestInsightForm.TestTreeDblClick(Sender: TObject);

  function ExtractMethod(aPath : string; Out aClass,aMethod: String) : Boolean;

  var
    P : integer;

  begin
    Writeln('Path: ');
    Result:=False;
    P:=RPos('.',aPath);
    if P>0 then
      begin
      aMethod:=Copy(aPath,P+1,Length(aPath)-P);
      aPath:=Copy(aPath,1,P-1);
      P:=RPos('.',aPath);
      if P>0 then
        aClass:=Copy(aPath,P+1,Length(aPath)-P)
      else
        aClass:=aPath;
      Result:=True;
      end;
    Writeln('Path -> Class: ',aClass,' method: ',aMethod,': ',Result);
  end;

var
  Itm : TTestItem;
  N : TTreeNode;
  aClass,aMethod,aUnit : String;
  aLocationFile : String;
  aLocationLine : Integer;

begin
  N:=TestTree.Selected;
  Itm:=Nil;
  While (Itm=Nil) and (N<>Nil) do
    begin
    if Assigned(N.Data) and (TObject(N.Data) is TTestItem) then
      Itm:=TTestItem(N.Data)
    else
      N:=N.Parent;
    end;
  if Not assigned(itm) then
    exit;
  if Itm.TestResult=Nil then
    begin
    If not ExtractMethod(Itm.TestPath,aClass,aMethod) then
      begin
      ShowMessage(Format(rsCouldNotDete, [Itm.TestPath]));
      exit;
      end;
    aUnit:='';
    aLocationFile:='';
    aLocationLine:=-1;
    end
  else
    begin
    With Itm.TestResult do
      begin
      aMethod:=TestMethodName;
      aClass:=TestClassName;
      aUnit:=TestUnitName;
      aLocationFile:=FailureSourceUnitName;
      aLocationLine:=FailureLineNumber;
      end;
    end;
  NavigateTo(aClass, aMethod,aUnit,aLocationFile,aLocationLine);
end;

procedure TTestInsightForm.TestTreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  procedure ChangeCheck(aNode: TTreeNode; aCheck: TTreeNodeState);
  var
    i: integer;
    n: TTreeNode;
  begin
    if Assigned(aNode) then
    begin
      aNode.StateIndex := ord(aCheck);
      if TTestItem(aNode.Data).IsTestSuite then
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

procedure TTestInsightForm.ClearDetails;
begin
  mDetails.Lines.Clear;
end;

procedure TTestInsightForm.DoClearTests(Sender: TObject);
begin
  DoSetTestSuite(Sender,TTestItem.Create(Nil,''));
end;

procedure TTestInsightForm.DoFinishedTests(Sender: TObject);
begin
  FTestsRunning:=False;
  pbBar.Refresh;
end;

procedure TTestInsightForm.AddSelected(N: TTreeNode;
  var Tests: TStringDynArray; var aLen: Integer);

Var
  I : integer;

begin
  if Assigned(N.Data) and (TObject(N.Data) is TTestItem) then
    if (N.StateIndex=Ord(tsChecked)) then
      if TTestItem(N.Data).IsTestCase then
        begin
        Tests[aLen]:=TTestItem(N.Data).TestPath;
        Inc(aLen);
        end
      else if TTestItem(N.Data).IsTestSuite then
        For I:=0 to N.Count-1 do
          AddSelected(N.items[i],Tests,aLen);
end;

procedure TTestInsightForm.DoGetSelectedTests(Sender: TObject; var Tests: TStringDynArray);

Var
  aLen : integer;

begin
  aLen:=0;
  SetLength(Tests,TestTree.Items.Count);
  if TestTree.Items.Count>0 then
    AddSelected(TestTree.Items[0],Tests,aLen);
  SetLength(Tests,aLen);
end;

procedure TTestInsightForm.DoSetTestResult(Sender: TObject; aResult: TTestInsightResultArray);

Var
  I : Integer;
  Res : TTestInsightResult;
  Itm : TTestItem;
  N : TTreeNode;
  aName : string;

begin
  For I:=0 to Length(aResult)-1 do
    begin
    Res:=aResult[i];
    aName:=Res.TestName;
    N:=FindNode(aName);
    if N=Nil then
      Res.Free
    else
      begin
      Itm:=TTestItem(N.Data);
      Itm.TestResult:=Res;
      Case Res.TestResult of
        rtFailed : AddFailure(N,Itm);
        rtError : AddError(N,Itm);
        rtWarning : AddFailure(N,Itm);
        rtPassed : EndTest(N,Itm);
      end;
      end;
    end;
end;

procedure TTestInsightForm.DoGetOptions(Sender: TObject;
  aOptions: TTestInsightOptions);
begin
  // Normally only called during test if server is running
  aOptions.ExecuteTests:=False;
  if Assigned(FSelectedTestSuite) then
    aOptions.TestSuite:=FSelectedTestSuite.TestPath
  else
    aOptions.TestSuite:=''
end;

procedure TTestInsightForm.DoSetTestSuite(Sender: TObject; Tests: TTestItem);
begin
  FreeAndNil(FTestSuite);
  FTestSuite:=Tests;
  ShowTestSuite;
end;

procedure TTestInsightForm.DoStartTests(Sender: TObject; aCount: Integer);
begin
  FTestsRunning:=true;
  if aCount=-1 then
    aCount:=FTestSuite.CountTestCases;
  failureCounter:=0;
  errorCounter:=0;
  testsCounter:=0;
  skipsCounter:=0;
  pbBar.Refresh;
end;

procedure TTestInsightForm.HandleServerLog(Sender: TObject; const aMessage: String);
begin
  ShowMessage(aMessage);
end;



procedure TTestInsightForm.ShowDetails(const Node: TTreeNode);

  procedure AddMessages(const Node: TTreeNode);
  begin
    if (Node is TMessageTreeNode) and (TMessageTreeNode(Node).Message <> '') then
      mDetails.Lines.Add(TMessageTreeNode(Node).Message)
    else
      mDetails.Lines.Add(Node.Text);
  end;

var
  CurrNode: TTreeNode;
begin
  ClearDetails;
  if TTestItem(Node.Data).IsTestCase then
  begin
    CurrNode := Node.GetFirstChild;
    while CurrNode <> nil do
    begin
      AddMessages(CurrNode);
      CurrNode := CurrNode.GetNextSibling;
    end;
  end
  else if Assigned(Node.Parent) and Assigned(Node.Parent.Data) and TTestItem(Node.Parent.Data).IsTestCase then
    AddMessages(Node);
end;


procedure TTestInsightForm.TestTreeSelectionChanged(Sender: TObject);
begin
  if (Sender as TTreeView).Selected <> nil then
    ShowDetails((Sender as TTreeView).Selected);
end;


procedure TTestInsightForm.ActCopyErrorMsgExecute(Sender: TObject);
begin
  ClipBoard.AsText := (TestTree.Selected as TMessageTreeNode).Message;
end;


procedure TTestInsightForm.ActCopyErrorMsgUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(TestTree.selected) and
    (Copy(TestTree.Selected.Text, 1, 9) = 'Message: ');
end;


procedure TTestInsightForm.pbBarPaint(Sender: TObject);

var
  msg: string;
  alltests: integer;
  OldStyle: TBrushStyle;
begin
  with (Sender as TPaintBox) do
  begin
    Canvas.Lock;
    if FTestsRunning then
      Canvas.Brush.Color := clBlue
    else
      Canvas.Brush.Color := clSilver;
    Canvas.Rectangle(0, 0, Width, Height);
    Canvas.Font.Color := clWhite;
    alltests := FTestCount;
    if alltests - skipsCounter <> 0 then
    begin
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
      Canvas.Textout(10, 4,  msg);
      Canvas.Brush.Style := OldStyle;
    end;
    Canvas.UnLock;
  end;
end;

procedure TTestInsightForm.actNextErrorExecute(Sender: TObject);
begin
  NextError;
end;

procedure TTestInsightForm.actPrevErrorExecute(Sender: TObject);
begin
  PrevError;
end;

procedure TTestInsightForm.BuildTree(rootNode: TTreeNode; aSuite: TTestItem);

var
  node: TTreeNode;
  i: integer;
  Itm : TTestItem;
  aName : TJSONStringType;

begin
  rootNode.StateIndex := Ord(tsChecked);
  for i := 0 to Length(ASuite.Tests) - 1 do
    begin
    itm:=ASuite.Tests[i];
    aName:=Itm.TestName;
    node := TestTree.Items.AddChildObject(rootNode, aName , itm);
    if Itm.IsTestSuite then
      BuildTree(Node, Itm);
    node.ImageIndex := imgGrayBall;
    node.SelectedIndex := imgGrayBall;
    node.StateIndex := ord(tsChecked);
    end;
  ResetNodeColors;
end;

procedure TTestInsightForm.SetFirstFailure(aNode: TTreeNode);
begin
  if FFirstFailure<>Nil then
    exit;
  FFirstFailure:=aNode;
  TestTree.Items.SelectOnlyThis(FFirstFailure);
  TestTree.MakeSelectionVisible;
end;


function TTestInsightForm.FindNode(aTest: String): TTreeNode;
var
  i: integer;
  tName : String;
begin
  Result := nil;
  for i := 0 to TestTree.Items.Count -1 do
    begin
    tName:=TTestItem(TestTree.Items[i].Data).TestPath;
    if SameText(tName,aTest) then
      Exit(TestTree.Items[i]);
    end;
end;


procedure TTestInsightForm.ResetNodeColors;
var
  i: integer;
begin
  for i := 0 to TestTree.Items.Count - 1 do
  begin
    TestTree.Items[i].ImageIndex := imgGrayBall;
    TestTree.Items[i].SelectedIndex := imgGrayBall;
  end;
end;


procedure TTestInsightForm.PaintNodeError(aNode: TTreeNode);
begin
  while Assigned(aNode) do
  begin
    aNode.ImageIndex := imgRedBall;
    aNode.SelectedIndex := imgRedBall;
    if aNode.AbsoluteIndex<>0 then begin
      aNode.Expand(True);
    end;
    aNode := aNode.Parent;
  end;
end;


procedure TTestInsightForm.PaintNodeFailure(aNode: TTreeNode);
begin
  while Assigned(aNode) do
  begin
    if ((aNode.ImageIndex in [imgGreenBall, imgGrayBall, imgBlueBall]) or
      (ANode.ImageIndex = -1)) then
    begin
      aNode.ImageIndex := imgPurpleBall;
      aNode.SelectedIndex := imgPurpleBall;
      if aNode.AbsoluteIndex<>0 then begin
        aNode.Expand(true);
      end;
    end;
    aNode := aNode.Parent;
  end;
end;

procedure TTestInsightForm.PaintNodeIgnore(aNode: TTreeNode);
// Test results with Ignore
var
  noFailedSibling: boolean;
  i: integer;
begin
  if Assigned(aNode) then
  begin
    if ((aNode.ImageIndex in [imgGrayBall, imgBlueBall]) or
      (ANode.ImageIndex = -1)) then
    begin
      aNode.ImageIndex := imgGreenBall;
      aNode.SelectedIndex := imgGreenBall;
    end;
  end;
  if Assigned(aNode.Parent) then
    if aNode.Index = aNode.Parent.Count -1 then
    begin
    aNode := aNode.Parent;
    noFailedSibling := true;
    for i := 0 to aNode.Count -2 do
    begin
      if aNode.Items[i].ImageIndex <> imgGreenBall then
        noFailedSibling := false;;
    end;
    if (aNode.ImageIndex = imgBlueBall) and
      noFailedSibling then
      PaintNodeIgnore(aNode);
    end;
end;


procedure TTestInsightForm.PaintNodeNonFailed(aNode: TTreeNode);
var
  noFailedSibling: boolean;
  i: integer;
begin
  if Assigned(aNode) then
  begin
    if ((aNode.ImageIndex in [imgGrayBall, imgBlueBall]) or
      (ANode.ImageIndex = -1)) then
    begin
      aNode.ImageIndex := imgGreenBall;
      aNode.SelectedIndex := imgGreenBall;
    end;
  end;
  if Assigned(aNode.Parent) then
    if aNode.Index = aNode.Parent.Count -1 then   // is aNode the last child
    begin
    aNode := aNode.Parent;
    noFailedSibling := true;
    for i := 0 to aNode.Count -2 do
    begin
      if aNode.Items[i].ImageIndex <> imgGreenBall then
        noFailedSibling := false;
    end;
    if (aNode.ImageIndex = imgBlueBall) and
      noFailedSibling then
      PaintNodeNonFailed(aNode);
    end;
end;


procedure TTestInsightForm.PaintNodeBusy(aNode: TTreeNode);
var
  BusySibling: boolean;
  i: integer;
begin
  if Assigned(aNode) then
  begin
    aNode.ImageIndex := imgBlueBall;
    aNode.SelectedIndex := imgBlueBall;
  end;
  if Assigned(aNode.Parent) then
  begin
    if aNode.Index = aNode.Parent.Count -1 then
    begin
      aNode := aNode.Parent;
      BusySibling := true;
      for i := 0 to aNode.Count -2 do
      begin
        if aNode.Items[i].ImageIndex <> imgGreenBall then
          BusySibling := false;
      end;
      if (aNode.ImageIndex = imgBlueBall) and BusySibling then
        PaintNodeBusy(aNode);
    end;
  end;
end;


procedure TTestInsightForm.EnableRunActions(AValue: boolean);
begin
  ActRunHighlightedTest.Enabled := AValue;
  actRunAll.Enabled := AValue;
end;

procedure TTestInsightForm.AddFailure(aNode : TTreeNode; ATest: TTestItem);
var
  MNode: TMessageTreeNode;
  aClass,aMessage : String;

begin
  if not aTest.TestResult.TestIsIgnored then
    begin
    Inc(failureCounter);
    if errorCounter = 0 then
      barColor := clFuchsia;
    end;
  if Not Assigned(aNode) then
    exit;
  aMessage:=aTest.TestResult.TestExceptionMessage;
  aClass:=aTest.TestResult.TestExceptionClass;
  MNode := TestTree.Items.AddChild(aNode,
    Format(rsMessage, [FirstLine(aMessage)]))
    as TMessageTreeNode;
  if not aTest.TestResult.TestIsIgnored then
    begin
    // Genuine failure
    if not Assigned(FFirstFailure) then
      SetFirstFailure(aNode);
    MNode.Message := aMessage;
    MNode.ImageIndex := imgWarningSign;
    MNode.SelectedIndex := imgWarningSign;
    MNode := TestTree.Items.AddChild(aNode,
      Format(rsException, [aClass])) as TMessageTreeNode;
    MNode.ImageIndex := imgWarningSign;
    MNode.SelectedIndex := imgWarningSign;
    if (aTest.TestResult.FailureLocationInfo<>'') then
      begin
      MNode := TestTree.Items.AddChild(aNode, 'at ' + aTest.TestResult.FailureLocationInfo) as TMessageTreeNode;
      MNode.ImageIndex := imgWarningSign;
      MNode.SelectedIndex := imgWarningSign;
      PaintNodeFailure(aNode);
      end;
    end
  else
    begin
    // Although reported as a failure, the test was set up
    // to be ignored so it is actually a success of sorts
    MNode.Message := aMessage;
    MNode.ImageIndex := imgGreenBall;
    MNode.SelectedIndex := imgGreenBall;
    MNode := TestTree.Items.AddChild(aNode,
      Format(rsException, [aClass])) as TMessageTreeNode;
    MNode.ImageIndex := imgGreenBall;
    MNode.SelectedIndex := imgGreenBall;
    PaintNodeIgnore(aNode);
    end;
  ShowDetails(aNode);
end;


procedure TTestInsightForm.AddError(aNode : TTreeNode; ATest: TTestItem);

var
  MNode: TMessageTreeNode;
  aMessage, aClass,aLocation : String;

begin
  Inc(errorCounter);
  barColor := clRed;
  aMessage:=aTest.TestResult.TestExceptionMessage;
  aClass:=aTest.TestResult.TestExceptionClass;
  aLocation:=aTest.TestResult.FailureLocationInfo;
  if Not Assigned(aNode) then
    exit;
  if not Assigned(FFirstFailure) then
    SetFirstFailure(aNode);
  MNode := TestTree.Items.AddChild(aNode,
    Format(rsExceptionMes, [FirstLine(aMessage)]))
    as TMessageTreeNode;
  MNode.Message := aMessage;
  MNode.ImageIndex := imgWarningSign;
  MNode.SelectedIndex := imgWarningSign;
  MNode:=TestTree.Items.AddChild(aNode, Format(rsExceptionCla,[aClass])) as TMessageTreeNode;
  MNode.ImageIndex := imgWarningSign;
  MNode.SelectedIndex := imgWarningSign;
  if (aLocation<>'') then
     begin
     MNode := TestTree.Items.AddChild(aNode, 'at ' + aLocation) as TMessageTreeNode;
     MNode.ImageIndex := imgInfoSign;
     MNode.SelectedIndex := imgInfoSign;
     end;
  PaintNodeError(aNode);
  ShowDetails(aNode);
end;


procedure TTestInsightForm.StartTest(ATest: String);

var
  Node: TTreeNode;

begin
  TestTree.BeginUpdate;
  try
    Node := FindNode(ATest);
    Node.DeleteChildren;
    PaintNodeBusy(Node);
    if Node.Level=1 then begin
      Node.MakeVisible;
    end;
    if assigned(Node.Parent) and (Node.Parent.Level=1) then begin
      Node.Parent.MakeVisible;
    end;
    Application.ProcessMessages;
  finally
    TestTree.EndUpdate;
  end;
end;


procedure TTestInsightForm.EndTest(aNode : TTreeNode; ATest: TTestItem);
begin
  TestTree.BeginUpdate;
  try
    Inc(testsCounter);
    if not assigned(aNode) then
      Exit;
    PaintNodeNonFailed(aNode);
    pbbar.Refresh;
    Application.ProcessMessages;
  finally
    TestTree.EndUpdate;
  end;
  if aTest=Nil then;
end;

function TTestInsightForm.GetTestProject: String;

Var
  ProjectPath : String;

begin
  ProjectPath:=ExtractFilePath(ParamStr(0))+'testing'+PathDelim;
  Result:= ProjectPath + 'clienttest'; // + extension ?
end;


procedure TTestInsightForm.RunTest(ATest: TTestItem);

begin
  SaveTree;
  ClearDetails;
  barcolor := clGreen;
  ResetNodeColors;
  failureCounter := 0;
  errorCounter := 0;
  testsCounter := 0;
  skipsCounter := 0;
  EnableRunActions(false);
  FSelectedTestSuite:=aTest;
  RunTestProject(FTestProject,False);
end;

function TTestInsightForm.GetBaseURL: String;

begin
   Result:=Format('http://localhost:%d%s',[FServer.Port,FServer.BasePath])
end;

procedure TTestInsightForm.CreateTestRunConfigFile(aConfStoreFile : string; aSendNamesOnly : Boolean);


Var
  aIni:TmemIniFile;

begin
  aIni:=TmemIniFile.Create(aConfStoreFile);
  try
    aIni.WriteString(SConfig,KeyBaseURL,GetBaseURL);
    aIni.WriteBool(sConfig,keyExecuteTests,Not aSendNamesOnly);
    if FSelectedTestSuite<>nil then
      aIni.WriteString(SConfig,KeySuite,FSelectedTestSuite.TestPath)
    else
      aIni.DeleteKey(SConfig,KeySuite);
    aIni.UpdateFile;
  finally
    aIni.Free;
  end;
end;

procedure TTestInsightForm.RunTestProject(aExecutable: String; SendNamesOnly: Boolean);

begin
  if TestRunning then
     begin
     ShowMessage(Format('The test project %s is still running',[aExecutable]));
     Exit;
     end;
  if not FileExists(aExecutable) then
    begin
    ShowMessage(Format(SNoExecutableAvailable,[aExecutable]));
    Exit;
    end;
  CreateTestRunConfigFile(FConfStoreFile,SendNamesOnly);
  PTest.Executable:=aExecutable;
  try
    PTest.Execute;
  except
    On E : Exception do
      ShowMessage(Format('Error %s while running test project %s: %s',[E.ClassName,aExecutable,E.Message]));
  end;
  FSelectedTestSuite:=Nil;
end;

function TTestInsightForm.TestRunning: Boolean;
begin
  Result:=pTest.Running;
end;

procedure TTestInsightForm.NextError;

var
  Node: TTreeNode;

begin
  Node := TestTree.Selected;
  while Assigned(Node) do
  begin
    Node := Node.GetNext;
    if Assigned(Node)
       and (Node.ImageIndex in [imgRedBall, imgPurpleBall])
       and TTestItem(Node.Data).IsTestCase then
    begin
      TestTree.Selected := Node;
      TestTree.MakeSelectionVisible;
      Exit;
    end;
  end;
end;

procedure TTestInsightForm.PrevError;
var
  Node: TTreeNode;
begin
  Node := TestTree.Selected;
  while Assigned(Node) do
  begin
    Node := Node.GetPrev;
    if Assigned(Node) and (Node.ImageIndex in [imgRedBall, imgPurpleBall]) and
      TTestItem(Node.Data).IsTestCase then
    begin
      TestTree.Selected := Node;
      TestTree.MakeSelectionVisible;
      Exit;
    end;
  end;
end;

function TTestInsightForm.ShowRefreshTestproject: Boolean;
begin
  Result:=False;
end;

function TTestInsightForm.AutoFetchTests: Boolean;
begin
  Result:=True;
end;

end.

