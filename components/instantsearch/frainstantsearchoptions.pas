unit frainstantsearchoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, ValEdit, Dialogs,
  ActnList, IDEOptionsIntf, IDEOptEditorIntf, IDEDialogs, idemcindexer, ideinstantsearch;

type

  { TIDEInstantSearchOptionsFrame }

  TIDEInstantSearchOptionsFrame = class(TAbstractIDEOptionsEditor)
    actAdd: TAction;
    actDelete: TAction;
    actIndex: TAction;
    actEdit: TAction;
    actClear: TAction;
    actClearAll: TAction;
    actCreateIndex: TAction;
    actDeleteIndex: TAction;
    actTestConnection: TAction;
    alInstantSearch: TActionList;
    BtnTest: TButton;
    btnCreateIndex: TButton;
    btnDeleteIndex: TButton;
    cbMySQLVersion: TComboBox;
    cbProtocol: TComboBox;
    cbIndexName: TComboBox;
    cbIndexProjectMoment: TComboBox;
    cbIndexProjectStrategy: TComboBox;
    edtIndexDelay: TEdit;
    edtMinSearchTermLength: TEdit;
    EdtMaxClipboardLength: TEdit;
    edtLimit: TEdit;
    edtPort: TEdit;
    edtHostname: TEdit;
    edtIdleDisconnect: TEdit;
    GBManticore: TGroupBox;
    GBIndexing: TGroupBox;
    gbAutomations: TGroupBox;
    ilInstantSearch: TImageList;
    lblEditIndexDelay1: TLabel;
    lblEditIndexDelay2: TLabel;
    lblIndexProjectMoment: TLabel;
    lblIndexProjectstrategy: TLabel;
    lblIndexing: TLabel;
    lblIndexName: TLabel;
    lblMinSearchTermLength: TLabel;
    lblMaxClipbrdSearchTermLength: TLabel;
    lblLimit: TLabel;
    lblDisconnect: TLabel;
    lblProtocol: TLabel;
    lblPort: TLabel;
    lblHost: TLabel;
    lblMySQLversion: TLabel;
    PCOptions: TPageControl;
    TBSourceTrees: TToolBar;
    TBAdd: TToolButton;
    TBEdit: TToolButton;
    TBDelete: TToolButton;
    ToolButton1: TToolButton;
    TBClear: TToolButton;
    TBClearAll: TToolButton;
    tbIndex: TToolButton;
    TSSourceTrees: TTabSheet;
    TSSearchEngine: TTabSheet;
    VLETrees: TValueListEditor;
    procedure actClearAllExecute(Sender: TObject);
    procedure actCreateIndexExecute(Sender: TObject);
    procedure actCreateIndexUpdate(Sender: TObject);
    procedure actDeleteIndexExecute(Sender: TObject);
    procedure actDeleteIndexUpdate(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure actEditUpdate(Sender: TObject);
    procedure actIndexUpdate(Sender: TObject);
    procedure actTestConnectionExecute(Sender: TObject);
    procedure cbIndexProjectStrategyChange(Sender: TObject);
    procedure DoHandleClearUpdate(Sender: TObject);
    procedure HandleClear(Sender: TObject);
    procedure HandleEdit(Sender: TObject);
    procedure HandleAdd(Sender: TObject);
    procedure HandleDelete(Sender: TObject);
    procedure HandleIndex(Sender: TObject);
    procedure VLETreesClick(Sender: TObject);
    procedure VLETreesDblClick(Sender: TObject);
  private
    FDialog : TAbstractOptionsEditorDialog;
    FCurrentRow : Integer;
    FTrees : TSourceTreeDefinitionList;
    FTest : TManticoreSearchSources;
    FOldOnIndexStart,
    FOldOnIndexDone : TNotifyEvent;
    function CheckIndexDoesNotExist(const aIndexName: string): Boolean;
    procedure CheckIndexTime;
    function CheckTestConfig(aSilent: Boolean): boolean;
    procedure CheckTreesInNeedOfIndexing;
    procedure ConfigureTest;
        // test connection and fetch index names

    function ConnectTest(TestIndexExistence : Boolean) : Boolean;
    Function CurrentTree : TSourceTreeDefinition;
    procedure DisplayTree;
    procedure DoIndexDone(Sender: TObject);
    procedure DoIndexStart(Sender: TObject);
    Procedure CheckIndexing;
    function EditTree(aTree: TSourceTreeDefinition): boolean;
    procedure IndexTree;
    // test connection and fetch index names
    function TestConnection(aIndexes: TStrings; aSilent: Boolean): Boolean;
  public
    destructor Destroy; override;
    Procedure EditSourceTree;
    Procedure AddSourceTree;
    Procedure DeleteSourceTree;
    Procedure ClearSourceTree;
    Procedure ClearAllSourceTrees;
    Procedure CreateIndex(const aIndexName : string);
    function GetTitle: String; override;
    function Modified : Boolean;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

uses grids, IDEExternToolIntf, idemsgintf, instantsearchstrings, frmSourceTree;

{$R *.lfm}

{ TIDEInstantSearchOptionsFrame }

procedure TIDEInstantSearchOptionsFrame.VLETreesDblClick(Sender: TObject);
begin
  EditSourceTree;
end;

function TIDEInstantSearchOptionsFrame.CurrentTree: TSourceTreeDefinition;
begin
  if (FCurrentRow>=0) and (FCurrentRow<FTrees.Count) then
    Result:=FTrees[FCurrentRow]
  else
    Result:=Nil;
end;

destructor TIDEInstantSearchOptionsFrame.Destroy;
begin
  IDEInstantSearchManager.onIndexDone:=FOldOnIndexDone;
  IDEInstantSearchManager.onIndexStart:=FOldOnIndexStart;
  FreeAndNil(FTrees);
  inherited Destroy;
end;

function TIDEInstantSearchOptionsFrame.EditTree(aTree: TSourceTreeDefinition
  ): boolean;

begin
  With TSourceTreeDefinitionForm.Create(Application) do
    try
       Tree:=aTree;
       Result:=ShowModal=mrOK;
    finally
      Free;
    end;
end;

procedure TIDEInstantSearchOptionsFrame.EditSourceTree;
begin
  if EditTree(CurrentTree) then
    DisplayTree;
end;

procedure TIDEInstantSearchOptionsFrame.AddSourceTree;

Var
  aTree : TSourceTreeDefinition;

begin
  aTree:=Ftrees.AddTree(Format(lrsNewTree,[FTrees.Count]));
  if EditTree(aTree) then
    DisplayTree
  else
    ATree.Free;
end;

procedure TIDEInstantSearchOptionsFrame.DeleteSourceTree;
begin
  if IDEMessageDialog(lrsConfirm,lrsConfirmDeleteTree,mtWarning,[mbYes,mbNo])=mrYes then
    begin
    CurrentTree.Free;
    DisplayTree;
    end;
end;

procedure TIDEInstantSearchOptionsFrame.ClearSourceTree;
begin
  if not ConnectTest(True) then
    exit;
  FTest.DeleteTree(CurrentTree.Name);
end;

function TIDEInstantSearchOptionsFrame.ConnectTest(TestIndexExistence: Boolean
  ): Boolean;

Var
  L : TStrings;

begin
  Result:=True;
  L:=TStringList.Create;
  try
    TestConnection(L,False);
    if TestIndexExistence and (cbIndexName.Text<>'') then
      if L.IndexOf(cbIndexName.Text)=-1 then
        case QuestionDlg(lrsNoIndex,Format(lrsIndexNotFoundCreate,[cbIndexName.Text]),mtWarning,[mrYes,lrsCreateIndex,mrAbort,lrsAbortOperation],'') of
          mrYes : CreateIndex(cbIndexName.Text);
          mrAbort : Exit(False);
        else
          exit(False);
        end;
  finally
    L.Free;
  end;
end;

procedure TIDEInstantSearchOptionsFrame.ClearAllSourceTrees;
begin
  ConnectTest(True);
  FTest.TruncateIndex;
end;


function TIDEInstantSearchOptionsFrame.CheckIndexDoesNotExist(const aIndexName: string) : Boolean;

var
  L : TStrings;

begin
  L:=TStringList.Create;
  try
    TestConnection(L,False);
    Result:=L.IndexOf(aIndexName)=-1;
  finally
    L.Free;
  end;
end;

procedure TIDEInstantSearchOptionsFrame.CreateIndex(const aIndexName: string);
begin
  // This will set indexname
  try
    ConnectTest(False);
    FTest.CreateIndex;
  except
    On E : Exception do
      ShowMessage(format(lrsCreateIndexFailed,[aIndexName,E.Message]));
  end;
end;

function TIDEInstantSearchOptionsFrame.GetTitle: String;
begin
  Result:=lrsInstantSearch;
end;

function TIDEInstantSearchOptionsFrame.Modified: Boolean;

begin
  Result:=(edtHostname.Text<>IDEInstantSearchManager.Indexer.HostName);
  Result:=Result or (edtPort.Text<>IntToStr(IDEInstantSearchManager.Indexer.Port));
  Result:=Result or (EdtMaxClipboardLength.text<>IntToStr(IDEInstantSearchManager.MaxStartSearchLength));
  Result:=Result or (edtLimit.Text<>IntToStr(IDEInstantSearchManager.Indexer.Limit));
  Result:=Result or (edtMinSearchTermLength.text<>IntToStr(IDEInstantSearchManager.Indexer.MinInfixLen));
  Result:=Result or (cbProtocol.ItemIndex<>Ord(IDEInstantSearchManager.Indexer.Transport));
  Result:=Result or (cbMySQLVersion.ItemIndex<>Ord(IDEInstantSearchManager.Indexer.MySQLVersion));
  Result:=Result or (cbIndexName.Text<>IDEInstantSearchManager.Indexer.IndexName);
end;

procedure TIDEInstantSearchOptionsFrame.HandleAdd(Sender: TObject);
begin
  AddSourceTree;
end;

procedure TIDEInstantSearchOptionsFrame.actDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentTree) and Not CurrentTree.System
end;

procedure TIDEInstantSearchOptionsFrame.actClearAllExecute(Sender: TObject);
begin
  ClearAllSourceTrees;
end;

procedure TIDEInstantSearchOptionsFrame.CheckTreesInNeedOfIndexing;

Var
  L : Tstrings;
  S, NeedIndexing : String;
  I : Integer;
  MR : TModalResult;

begin
  ConnectTest(False);
  L:=TStringList.Create;
  try
    FTest.ListTrees(L);
    if L.Count=0 then
      NeedIndexing:=lrsAllTreesNeedindexing
    else
      begin
      for I:=0 to FTrees.Count-1 do
        begin
        S:=FTrees[i].Name;
        if L.IndexOf(S)=-1 then
          begin
          if NeedIndexing<>'' then
            NeedIndexing:=NeedIndexing+', ';
          NeedIndexing:=NeedIndexing+S;
          end;
        end;
      if NeedIndexing<>'' then
        NeedIndexing:=Format(lrsTheseTreesNeedIndexing,[NeedIndexing]);
      end;
    if NeedIndexing<>'' then
      begin
      NeedIndexing:=NeedIndexing+#10+lrsIndexNow+#10+lrsThisNeedsSavingSettings;
      MR:=QuestionDlg(lrsNeedIndexing,NeedIndexing,mtInformation,[mrYes,lrsSaveAndIndex,mrYesToAll,lrsSaveAndIndexAll,mrNo,lrsIndexLater],0);
      if MR in [mrYesToAll,mrYes] then
        begin
        WriteSettings(Nil);
        IDEInstantSearchManager.CheckRefresh(L,MR=mrYesToall);
        end;
      end;
  finally
    L.free;
  end;
end;

procedure TIDEInstantSearchOptionsFrame.actCreateIndexExecute(Sender: TObject);
begin
  if CheckIndexDoesNotExist(cbIndexName.Text) then
    CreateIndex(cbIndexName.Text)
  else
    ShowMessage(Format(lrsIndexAlreadyExists,[cbIndexName.Text]));
  CheckTreesInNeedOfIndexing;
end;

procedure TIDEInstantSearchOptionsFrame.actCreateIndexUpdate(Sender: TObject);
begin
  ConfigureTest;
  (Sender as TAction).Enabled:=CheckTestConfig(True);
end;

procedure TIDEInstantSearchOptionsFrame.actDeleteIndexExecute(Sender: TObject);

var
  lIndexName : String;
  L : TStrings;

begin
  lIndexName:=cbIndexName.Text;
  if IDEInstantSearchManager.IsIndexing then
    ShowMessage(lrsCannotDeleteIndexWhileIndexing)
  else if InputQuery(lrsDeleteIndex,lrsIndexToDelete,lIndexName) then
    begin
    L:=TStringList.Create;
    try
      if TestConnection(L,False) then
        if L.IndexOf(lIndexName)=-1 then
          ShowMessage(Format(lrsNoSuchIndex,[lIndexName]))
        else
          try
            FTest.DeleteIndex(lIndexName);
            ShowMessage(Format(lrsIndexDeleted,[lIndexName]));
          except
            On E : Exception do
              ShowMessage(Format(lrsDeleteIndexFailed,[lIndexName,E.Message]));
          end;
    finally
      L.Free;
    end;
    end;
end;

procedure TIDEInstantSearchOptionsFrame.actDeleteIndexUpdate(Sender: TObject);
begin
  ConfigureTest;
  (Sender as TAction).Enabled:=CheckTestConfig(True);
end;

procedure TIDEInstantSearchOptionsFrame.actEditUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentTree) and Not CurrentTree.System
end;

procedure TIDEInstantSearchOptionsFrame.actIndexUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=IDEInstantSearchManager.CanConnect and Not IDEInstantSearchManager.IsIndexing
end;

procedure TIDEInstantSearchOptionsFrame.actTestConnectionExecute(Sender: TObject
  );

Var
  L : TStrings;
  msg : string;
begin
  L:=TStringList.Create;
  try
    try
      if not TestConnection(L,False) then
        exit;
    except
      On E: Exception do
        ShowMessage(Format(lrsConnectionTestFailed,[E.Message]));
    end;
    ShowMessage(lrsConnectionTestOK);
    cbIndexName.Items:=L;
    if L.IndexOf(cbIndexName.Text)=-1 then
      begin
      Msg:=format(lrsCreateIndexWithName,[cbIndexName.Text]);
      if QuestionDlg(lrsCreateIndex,msg,mtInformation,[mrYes,lrsYesCreateIndex,mrNo,lrsDoNotCreateIndex],0)=mrYes then
        CreateIndex(cbIndexName.Text);
      end;
  finally
    L.Free;
  end;
end;

procedure TIDEInstantSearchOptionsFrame.cbIndexProjectStrategyChange(
  Sender: TObject);
begin
  CheckIndexTime;
end;

procedure TIDEInstantSearchOptionsFrame.CheckIndexTime;

begin
  edtIndexDelay.Enabled:=cbIndexProjectStrategy.ItemIndex=Ord(ipsTimed);
end;

procedure TIDEInstantSearchOptionsFrame.DoHandleClearUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentTree)
end;

procedure TIDEInstantSearchOptionsFrame.HandleClear(Sender: TObject);
begin
  ClearSourceTree;
end;

procedure TIDEInstantSearchOptionsFrame.HandleEdit(Sender: TObject);
begin
  if Assigned(CurrentTree) then
    EditSourceTree;
end;

procedure TIDEInstantSearchOptionsFrame.HandleDelete(Sender: TObject);
begin
  DeleteSourceTree
end;

procedure TIDEInstantSearchOptionsFrame.HandleIndex(Sender: TObject);
begin
  IndexTree;
end;

procedure TIDEInstantSearchOptionsFrame.IndexTree;

begin
  if IDEInstantSearchManager.IsIndexing then
    begin
    ShowMessage(lrsCannotIndexIndexInProgress);
    exit;
    end;
  if Modified then
    begin
    if QuestionDlg(lrsSaveNeeded,lrsIndexNeedsSave,mtWarning,[mrYes,lrsSave,mrAbort,lrsAbortOperation],'') = mrYes then
      WriteSettings(Nil)
    else
      exit;
    end;
  if not IDEInstantSearchManager.IndexTree(CurrentTree) then
    ShowMessage(lrsIndexOperationFailed);
end;

procedure TIDEInstantSearchOptionsFrame.VLETreesClick(Sender: TObject);
begin
  if VLETrees.SelectedRangeCount>0 then
    FCurrentRow:=VLETrees.Selection.Top-1
  else
    FCurrentRow:=-1;
end;

procedure TIDEInstantSearchOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);

Var
  V : TMCMySQLClientVersion;
  T : TMCTransport;
  M : TIndexProjectMoment;
  S : TIndexProjectStrategy;

begin
  FOldOnIndexDone:=IDEInstantSearchManager.OnIndexDone;
  FOldOnIndexStart:=IDEInstantSearchManager.OnIndexStart;
  IDEInstantSearchManager.OnIndexDone:=@DoIndexDone;
  IDEInstantSearchManager.OnIndexStart:=@DoIndexStart;
  Checkindexing;
  FDialog := ADialog;
  FTrees:=TSourceTreeDefinitionList.Create(Self,TSourceTreeDefinition);
  FTest:=TManticoreSearchSources.Create(Self);
  actTestConnection.Caption:=lrsTestConnection;
  actTestConnection.Hint:=lrsTestButtonHint;
  actAdd.Caption := lrsSourceTreeAdd;
  actAdd.Hint := lrsSourceTreeAddHint;
  actDelete.Caption := lrsSourceTreeDelete;
  actDelete.Hint := lrsSourceTreeDeleteHint;
  actIndex.Caption := lrsSourceTreeIndex;
  actIndex.Hint := lrsCreateIndexButtonHint;
  actEdit.Caption := lrsSourceTreeEdit;
  actEdit.Hint := lrsSourceTreeEditHint;
  actClear.Caption := lrsSourceTreeClear;
  actClear.Hint := lrsSourceTreeClearHint;
  actClearAll.Caption := lrsSourceTreeClearAll;
  actClearAll.Hint := lrsSourceTreeClearAllHint;
  actDeleteIndex.Caption:=lrsDeleteIndex;
  actDeleteIndex.Hint:=lrsDeleteIndexHint;
  GBManticore.Caption:=lrsManticoreOptions;
  GBIndexing.Caption:=lrsIndexingOptions;
  lblMinSearchTermLength.Caption:=lrsMinSearchLength;
  lblMaxClipbrdSearchTermLength.Caption:=lrsMaxClipbrdSearchTermLength;
  lblLimit.Caption:=lrsResultsLimit;
  lblProtocol.Caption:=lrsProtocol;
  lblPort.Caption:=lrsPort;
  lblHost.Caption:=lrsHost;
  lblMySQLversion.Caption:=lrsMysqlversion;
  lblIndexName.Caption:=lrsIndexName;
  TSSearchEngine.Caption:=lrsSearchEngine;
  TSSourceTrees.Caption:=lrsSourceTrees;
  lblIndexProjectStrategy.Caption:=lrsIndexProjectStrategy;
  lblIndexProjectMoment.Caption:=lrsIndexProjectMoment;
  lblEditIndexDelay1.Caption:=lrsIndexProjectDelay1;
  lblEditIndexDelay2.Caption:=lrsIndexProjectDelay2;
  For T in TMCTransport do
    if T<>mctNone then
      cbProtocol.Items.Add(MCTransportNames[T]);
  For V in TMCMySQLClientVersion do
    if V<>mcvNone then
      cbMySQLVersion.Items.Add(MySQLConnNames[V]);
  for M in TIndexProjectMoment do
    cbIndexProjectMoment.Items.Add(M.ToString);
  for S in TIndexProjectStrategy do
    cbIndexProjectStrategy.Items.Add(S.ToString);
end;

procedure TIDEInstantSearchOptionsFrame.DisplayTree;

Var
  aName : string;
  I : Integer;

begin
  aName:='';
  If Assigned(CurrentTree) then
    aName:=CurrentTree.Name;
  FCurrentRow:=-1;
  With VLETrees.Strings  do
    try
      BeginUpdate;
      Clear;
      For I:=0 to FTrees.Count-1 do
        With FTrees[i] do
          begin
          Add(Name+'='+BaseDir);
          if SameText(aName,Name) then
            FCurrentRow:=I;
          end;
    finally
      EndUpdate;
    end;
  if FCurrentRow=-1 then
    FCurrentRow:=0;
  VLETrees.Selection:=Rect(0,FCurrentRow,1,FCurrentRow);
end;

procedure TIDEInstantSearchOptionsFrame.DoIndexDone(Sender: TObject);
begin
  if Assigned(FOldOnIndexDone) then
    FOldOnIndexDone(Sender);
  CheckIndexing;
end;

procedure TIDEInstantSearchOptionsFrame.DoIndexStart(Sender: TObject);
begin
  if Assigned(FOldOnIndexStart) then
    FOldOnIndexStart(Sender);
  CheckIndexing;
end;

procedure TIDEInstantSearchOptionsFrame.CheckIndexing;
begin
  lblIndexing.Visible:=IDEInstantSearchManager.IsIndexing;
end;

procedure TIDEInstantSearchOptionsFrame.ConfigureTest;

begin
  FTest.Disconnect;
  FTest.HostName:=edtHostname.Text;
  FTest.Port:=StrToIntDef(edtPort.Text,0);
  FTest.Transport:=TMCTransport(cbProtocol.ItemIndex+1);
  FTest.MySQLVersion:=TMCMySQLClientVersion(cbMySQLVersion.ItemIndex+1);
  FTest.IndexName:=cbIndexName.Text;
end;

function TIDEInstantSearchOptionsFrame.CheckTestConfig(aSilent : Boolean) : boolean;

begin
  Result:=False;
  if FTest.Transport=mctNone then
    if aSilent then
      exit
    else
      Raise EManticoreSearch.Create(lrsNoTransport);
  if (FTest.Transport=mctMysql) and (FTest.MySQLVersion=mcvNone) then
    if aSilent then
      exit
    else
      Raise EManticoreSearch.Create(lrsNoMysqlVersion);
  Result:=True;
end;

function TIDEInstantSearchOptionsFrame.TestConnection(aIndexes : TStrings; aSilent : Boolean) : Boolean;

Var
  aList : Tstrings;

begin
  Result:=False;
  ConfigureTest;
  Result:=CheckTestConfig(aSilent);
  if not Result then
    exit;
  aList:=aIndexes;
  if aList=Nil then
    aList:=TstringList.Create;
  try
    try
      FTest.Connect;
      FTest.ListIndexes(aList);
      Result:=True;
    except
      if not aSilent then
        Raise;
    end;
  finally
    if aList<>aIndexes then
      FreeAndNil(aList);
  end;
end;

procedure TIDEInstantSearchOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  FTrees.Assign(IDEInstantSearchManager.SourceTrees);
  DisplayTree;
  edtHostname.Text:=IDEInstantSearchManager.Indexer.HostName;
  edtPort.Text:=IntToStr(IDEInstantSearchManager.Indexer.Port);
  edtIdleDisconnect.Text:=IntToStr(IDEInstantSearchManager.IdleDisconnectTimeOut);
  EdtMaxClipboardLength.text:=IntToStr(IDEInstantSearchManager.MaxStartSearchLength);
  edtLimit.Text:=IntToStr(IDEInstantSearchManager.Indexer.Limit);
  edtMinSearchTermLength.text:=IntToStr(IDEInstantSearchManager.Indexer.MinInfixLen);
  cbProtocol.ItemIndex:=Ord(IDEInstantSearchManager.Indexer.Transport)-1;
  cbMySQLVersion.ItemIndex:=Ord(IDEInstantSearchManager.Indexer.MySQLVersion)-1;
  cbIndexName.Text:=IDEInstantSearchManager.Indexer.IndexName;
  cbIndexProjectStrategy.ItemIndex:=Ord(IDEInstantSearchManager.IndexProjectStrategy);
  cbIndexProjectMoment.ItemIndex:=Ord(IDEInstantSearchManager.IndexProjectMoment);
  edtIndexDelay.Text:=IntTostr(IDEInstantSearchManager.IndexProjectDelay);
  if IDEInstantSearchManager.CanConnect then
    TestConnection(cbIndexName.Items,true);
end;

procedure TIDEInstantSearchOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);

Var
  L : TStringList;

begin
  IDEInstantSearchManager.Indexer.Disconnect;
  IDEInstantSearchManager.Indexer.HostName:= edtHostname.Text;
  IDEInstantSearchManager.Indexer.Port:= StrToIntDef(edtPort.Text,0);
  IDEInstantSearchManager.IdleDisconnectTimeOut:=StrToIntDef(edtIdleDisconnect.Text,0);
  IDEInstantSearchManager.MaxStartSearchLength:= StrToIntDef(EdtMaxClipboardLength.text,DefaultMaxStartSearchLength);
  IDEInstantSearchManager.Indexer.Limit:=StrToIntDef(edtLimit.Text,0);
  IDEInstantSearchManager.Indexer.MinInfixLen:=StrToIntDef(edtMinSearchTermLength.text,DefaultMCMinInfixLen);
  IDEInstantSearchManager.Indexer.Transport:=TMCTransport(cbProtocol.ItemIndex+1);
  IDEInstantSearchManager.Indexer.MySQLVersion:= TMCMySQLClientVersion(cbMySQLVersion.ItemIndex+1);
  IDEInstantSearchManager.Indexer.IndexName := cbIndexName.Text;
  IDEInstantSearchManager.IndexProjectStrategy:=TIndexProjectStrategy(cbIndexProjectStrategy.ItemIndex);
  IDEInstantSearchManager.IndexProjectMoment:=TIndexProjectMoment(cbIndexProjectMoment.ItemIndex);
  IDEInstantSearchManager.IndexProjectDelay:=StrToIntDef(edtIndexDelay.Text,DefaultTimedDelay);
  IDEInstantSearchManager.SourceTrees:=Self.FTrees;
  IDEInstantSearchManager.Configured:=True;
  IDEInstantSearchManager.Save;
  L:=TStringList.Create;
  try
   if IDEInstantSearchManager.TestConnect(L) then
     if L.IndexOf(IDEInstantSearchManager.Indexer.IndexName)>=0 then
       begin
       L.Clear;
       IDEInstantSearchManager.Indexer.ListTrees(L);
       IDEInstantSearchManager.CheckRefresh(L,False);
       end;
  except
    On E : Exception do
      IDEMessagesWindow.AddCustomMessage(mluError,Format(lrsNotProperlyConfigured,[E.Message]),'',0,0,lrsInstantSearch);
  end;
  L.Free;
end;

class function TIDEInstantSearchOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

