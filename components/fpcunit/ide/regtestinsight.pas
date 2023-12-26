unit RegTestInsight;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  lazlogger, Forms, Controls,
  IDECommands, MenuIntf, IDEWindowIntf, LazIDEIntf, IDEMsgIntf,
  IDEOptEditorIntf, IDEOptionsIntf, IDEExternToolIntf,
  Projectintf,
  CodeTree, CodeCache, CodeToolManager, FindDeclarationTool,
  frmTestInsight, TestInsightServer, StrTestCaseOpts,
  TestInsightController, fraTestInsightOpts;

Type

  { TLazTestInsightForm }

  TLazTestInsightForm = class(TTestInsightForm)
  private
  Public
    procedure ShowMessage(aType: TInsightMessageType; Const Msg : String); override;
    Function GetTestProject : String; override;
    procedure RunTestProject(aExecutable : string; SendNamesOnly : Boolean); override;
    procedure NavigateTo(const aClass, aMethod, aUnit, aLocationFile: String; aLocationLine: Integer); override;
    procedure ConfigureServer(aServer: TTestInsightServer); override;
    Function AutoFetchTests: Boolean; override;
    function ShowRefreshTestproject: Boolean; override;
  end;



procedure register;

implementation

const
  TestInsightFormName = 'TestInsightForm';

var
  TestInsightOptionsFrameID: integer = 1001;

Type

  { TFormFreeNotifier }

  TFormFreeNotifier = Class(TComponent)
  Public
  Type
    TFreeNotificationProc = Procedure(aForm : TCustomForm);
  Private
    FOnNotify : TFreeNotificationProc;
    FForm : TCustomForm;
  private
    procedure SetForm(AValue: TCustomForm);
  Protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function DoProjectChanged(Sender: TObject; {%H-}AProject: TLazProject): TModalResult;
  Public
    Constructor CustomCreate(aOwner : TComponent; aOnNotify : TFreeNotificationProc);
    Property Form : TCustomForm Read FForm Write SetForm;
  end;

var
  FreeNotifier : TFormFreeNotifier;
  TestInsightForm: TTestInsightForm;


Function ShowFileAt(aLocationFile : String; aLocationLine : integer) : Boolean;

var
  F : TLazProjectFile;

begin
  F:=LazarusIDE.ActiveProject.FindFile(aLocationFile,[]);
  Result:=Assigned(F);
  if Result then
    Result:=(mrOK=LazarusIDE.DoOpenFileAndJumpToPos(F.GetFullFilename,Point(aLocationLine-1,0),0,0,0,0,0,[ofOnlyIfExists]));
end;


Function ShowMethod(aClass,aMethod, aUnit : String) : Boolean;

var
  F : TLazProjectFile;
  Tool : TCodeTool;
  ClassTool : TFindDeclarationTool;
  C : TCodeBuffer;
  ClassNode,NewNode,Node : TCodeTreeNode;
  NewTopLine : Integer;
  Params: TFindDeclarationParams;
  LProj : TLazProject;
  Ctx : TFindContext;
  caret : TCodeXYPosition;

begin
  Result:=False;
  LProj:=LazarusIDE.ActiveProject;
  if (aUnit<>'') then
    begin
    F:=LProj.FindFile(aunit+'.pp',[pfsfOnlyProjectFiles]);
    if Not Assigned(F) then
      F:=LProj.FindFile(aunit+'.pas',[pfsfOnlyProjectFiles]);
    if Not Assigned(F) then
      F:=LProj.FindFile(aunit+'.p',[pfsfOnlyProjectFiles]);
    Writeln('Found unit "',aunit,'" : ',Assigned(F));
    end;
  // Search in unit, if available
  Tool:=Nil;
  Node:=Nil;
  if Assigned(F) then
    begin
    C:=CodeToolBoss.LoadFile(F.GetFullFilename,False,false);
    if CodetoolBoss.Explore(C,Tool,False,false) then
      Node:=Tool.FindImplementationNode;
    end;
  // If not found
  if not Assigned(Node) then
    begin
    C:=CodeToolBoss.LoadFile(LProj.MainFile.GetFullFilename,False,false);
    if CodetoolBoss.Explore(C,Tool,False,false) then
      Node:=Tool.FindMainBeginEndNode;
    end;
  Writeln('Node found for unit "',aunit,'" : ',Assigned(Node));
  if Node=nil then
    exit;
  Params:=TFindDeclarationParams.Create;
  Params.ContextNode:=Node;
  Params.Flags:=[fdfSearchInParentNodes];
  Params.SetIdentifier(Tool,PChar(aClass),nil);
  if not Tool.FindIdentifierInContext(Params) then
    begin
    Writeln('Class not found for unit "',aunit,'", identifier ',aClass);
    exit;
    end;
  NewNode:=Params.NewNode;
  ClassTool:=Params.NewCodeTool;
  ClassNode:=NewNode.FirstChild;
  if (ClassNode=nil)
     or (NewNode.Desc<>ctnTypeDefinition)
     or (ClassNode.Desc<>ctnClass) then
       begin
       Writeln('Class identifier in unit "',aunit,'", is not a class identifier ',aClass);
       Exit;
       end;
  Ctx:=ClassTool.FindClassMember(ClassNode,aMethod,true);
  if not Assigned(Ctx.Node) then
    begin
    Writeln('Method ',aMethod,' for class ',aClass,' not found in unit "',aunit);
    exit;
    end;
  Ctx.Tool.CleanPosToCaretAndTopLine(Ctx.Node.StartPos,Caret,NewTopLine);
  Result:=(mrOK=LazarusIDE.DoOpenFileAndJumpToPos(Caret.Code.Filename,Point(Caret.X,Caret.Y),NewTopLine,-1,-1,[ofRegularFile]));
end;

Var
  TestInsightWindowCreator: TIDEWindowCreator;
  ViewTestInsightWindowCommand: TIDECommand;
  Controller : TTestInsightController;

procedure ShowTestInsightWindow(Sender: TObject);

var
  F : TCustomForm;

begin
  if TestInsightForm=nil then
    begin
    F:=IDEWindowCreators.ShowForm(TestInsightWindowCreator.FormName,true);
    FreeNotifier.Form:=F;
    end
  else
    IDEWindowCreators.ShowForm(TestInsightForm,true);
end;

procedure CreateTestInsightWindow(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  if not SameText(aFormName,TestInsightFormName) then
    begin
    debugln(['ERROR: TestInsightFormName : there is already a form with this name']);
    exit;
    end;
  IDEWindowCreators.CreateForm(AForm,TLazTestInsightForm,DoDisableAutoSizing,
    LazarusIDE.OwningComponent);
  AForm.Name:=aFormName;
  TestInsightForm:=TTestInsightForm(AForm);
end;

procedure ClearTestInsightForm(aForm: TCustomForm);
begin
  if aForm=TestInsightForm then
    TestInsightForm:=nil;
end;

procedure register;

var
  CmdCatView: TIDECommandCategory;

begin
  Controller:=TTestInsightController.Create;
  Controller.Options.LoadFromFile(TestInsightConfig);
  FreeNotifier:=TFormFreeNotifier.CustomCreate(Nil,@ClearTestInsightForm);

  CmdCatView:=IDECommandList.FindCategoryByName(CommandCategoryViewName);
  if CmdCatView=nil then
    raise Exception.Create('simplewebsrv: command category '+CommandCategoryViewName+' not found');


 // Windows - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  TestInsightWindowCreator:=IDEWindowCreators.Add(TestInsightFormName,
      @CreateTestInsightWindow,nil,'20%','20%','+50%','+20%');


 // Windows - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ViewTestInsightWindowCommand:=RegisterIDECommand(CmdCatView, 'ViewTestInsightWindow',
   rsTestInsightTitle, CleanIDEShortCut, CleanIDEShortCut, nil, @ ShowTestInsightWindow);
 RegisterIDEMenuCommand(itmViewMainWindows, 'ViewTestInsightWindow',
   rsTestInsightTitle, nil, nil,  ViewTestInsightWindowCommand);

 // Options Frame - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  TestInsightOptionsFrameID:=RegisterIDEOptionsEditor(GroupEnvironment,TTestInsightOptionsFrame,TestInsightOptionsFrameID)^.Index;
  LazarusIDE.AddHandlerOnProjectOpened(@FreeNotifier.DoProjectChanged);

end;

{ TFormFreeNotifier }

function TFormFreeNotifier.DoProjectChanged(Sender: TObject; AProject: TLazProject): TModalResult;
begin
  Result:=mrOK;
  if Assigned(FForm) and (FForm is TLazTestInsightForm) then
    TLazTestInsightForm(FForm).MaybeStartTestProject;
end;

procedure TFormFreeNotifier.SetForm(AValue: TCustomForm);
begin
  if FForm=AValue then Exit;
  if Assigned(FForm) then FForm.RemoveFreeNotification(Self);
  FForm:=AValue;
  if Assigned(FForm) then FForm.FreeNotification(Self);
end;

procedure TFormFreeNotifier.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (aComponent=FForm) then
    begin
    FOnNotify(FForm);
    FForm:=Nil;
    end;
end;

constructor TFormFreeNotifier.CustomCreate(aOwner: TComponent; aOnNotify: TFreeNotificationProc);

begin
  Inherited Create(aOwner);
  FForm:=Nil;
  FOnNotify:=aOnNotify;
end;

{ TLazTestInsightForm }

procedure TLazTestInsightForm.ShowMessage(aType: TInsightMessageType; const Msg: String);

Const
  MLU : Array[TInsightMessageType] of TMessageLineUrgency = (TMessageLineUrgency.mluImportant,TMessageLineUrgency.mluError);

begin
  if Assigned(IDEMessagesWindow) then

    IDEMessagesWindow.AddCustomMessage(MLU[aType],Msg,'',0,0,rsTestInsightTitle)
end;

function TLazTestInsightForm.GetTestProject: String;

begin
  // When the IDE restores windows, the ActiveProject is not yet there, so we need to check.
  if Assigned(LazarusIDE)
     and Assigned(LazarusIDE.ActiveProject)
     and assigned(LazarusIDE.ActiveProject.LazCompilerOptions) then
  Result:=LazarusIDE.ActiveProject.LazCompilerOptions.CreateTargetFilename;
end;

procedure TLazTestInsightForm.RunTestProject(aExecutable : string; SendNamesOnly: Boolean);
begin
  // Probably the controller should do this.
  inherited RunTestProject(aExecutable,SendNamesOnly);
end;


procedure TLazTestInsightForm.ConfigureServer(aServer: TTestInsightServer);
begin
  aServer.Port:=Controller.Options.Port;
  aServer.BasePath:=Controller.Options.BasePath;
end;

function TLazTestInsightForm.AutoFetchTests: Boolean;
begin
  Result:=Controller.Options.AutoFetchTests
end;


procedure TLazTestInsightForm.NavigateTo(const aClass,aMethod, aUnit, aLocationFile: String; aLocationLine: Integer);

var
  NavOK : Boolean;

begin
  NavOK:=False;
  if (aLocationFile<>'')  then
    NavOK:=ShowFileAt(aLocationFile,aLocationLine);
  if not NavOK then
    NavOK:=ShowMethod(aClass,aMethod,aUnit);
  if not NavOK then
    ShowMessage(imtError,Format('Failed to navigate to test %s.%s in unit %s',[aClass,aMethod,aUnit]));
end;

function TLazTestInsightForm.ShowRefreshTestproject: Boolean;
begin
  Result:=True;
end;

finalization
  FreeAndNil(Controller);
  FreeAndNil(FreeNotifier);
end.

