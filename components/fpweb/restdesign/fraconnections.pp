unit fraconnections;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ActnList, StdCtrls, DB, sqldb, PropertyStorage, sqldbrestschema, sqldbrestbridge,sqldbschemaedittools;

type
  { TfraConnections }

  TfraConnections = class(TFrame)
    AConnectionAdd: TAction;
    AConnectionDelete: TAction;
    AConnectionEdit: TAction;
    AConnectionExpose: TAction;
    AConnectionRefresh: TAction;
    AConnectionsHide: TAction;
    aLConnections: TActionList;
    ILConnections: TImageList;
    Label1: TLabel;
    ToolBar1: TToolBar;
    TBConnectionAdd: TToolButton;
    TBConnectionEdit: TToolButton;
    TBConnectionDelete: TToolButton;
    TBSep1: TToolButton;
    TBConnectionRefresh: TToolButton;
    TBConnectionExpose: TToolButton;
    ToolButton1: TToolButton;
    TBConnectionsHide: TToolButton;
    TVConnections: TTreeView;
    procedure AConnectionAddExecute(Sender: TObject);
    procedure AConnectionDeleteExecute(Sender: TObject);
    procedure AConnectionDeleteUpdate(Sender: TObject);
    procedure AConnectionEditExecute(Sender: TObject);
    procedure AConnectionEditUpdate(Sender: TObject);
    procedure AConnectionExposeExecute(Sender: TObject);
    procedure AConnectionExposeUpdate(Sender: TObject);
    procedure AConnectionRefreshExecute(Sender: TObject);
    procedure AConnectionRefreshUpdate(Sender: TObject);
    procedure AConnectionsHideExecute(Sender: TObject);
    procedure TVConnectionsEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure TVConnectionsEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure TVConnectionsStartDrag(Sender: TObject;  var DragObject: TDragObject);
  private
    FConnections: TSQLDBRestConnectionList;
    FFileName: String;
    FModified: Boolean;
    FOnImportSchema: TNotifyEvent;
    function GetCurrentConnection: TMySQLDBRestConnection;
    procedure SetConnections(AValue: TSQLDBRestConnectionList);
    procedure ShowConnection(TN: TTreeNode; aConnection: TMySQLDBRestConnection; ShowTables: Boolean=True);
    procedure ShowConnectionTables(TN: TTreeNode; aConnection: TMySQLDBRestConnection);
    procedure ShowTable(TN: TTreeNode; aConnection: TMySQLDBRestConnection; const aTableName: string);
  Protected
    procedure ReadConnectionsFromIni(const aFileName, ASection: String);
    procedure WriteConnectionsToIni(const aFileName, ASection: String);
    Procedure Changed;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    procedure DeleteConnection(aConnection: TSQLDBRestConnection);
    procedure LoadConnections(const aFileName: string);
    procedure SaveConnections(const aFileName: String);
    Procedure ShowConnections;
    Procedure LoadSession(aStorage : TCustomPropertyStorage); virtual;
    Procedure SaveSession(aStorage : TCustomPropertyStorage); virtual;
    Property Connections : TSQLDBRestConnectionList read FConnections write SetConnections;
    Property CurrentConnection : TMySQLDBRestConnection Read GetCurrentConnection;
    Property OnImportSchema : TNotifyEvent Read FOnImportSchema Write FOnImportSchema;
    // Last used filename in Load/saveConnections
    Property FileName : String Read FFileName;
    Property Modified : Boolean Read FModified;
  end;

implementation

uses dialogs, strutils, inifiles, sqldbrestini, dlgsqldbrestconnection;

{$R *.lfm}
Const
  KeyConnections = 'Connections';
  KeyLoadOptions = 'LoadOptions';

Resourcestring
  SErrDuplicateConnection = 'Duplicate connection name: %s';

{ TfraConnections }

procedure TfraConnections.AConnectionsHideExecute(Sender: TObject);
begin
  Self.Visible:=False;
end;

procedure TfraConnections.TVConnectionsEdited(Sender: TObject; Node: TTreeNode; var S: string);

Var
  D,C : TMySQLDBRestConnection;

begin
  C:=TObject(Node.Data) as TMySQLDBRestConnection;
  D:=TMySQLDBRestConnection(Connections.FindConnection(S));
  if (D<>Nil) and (D<>C) then
    begin
    ShowMessage(SErrDuplicateConnection);
    S:=C.Name;
    end
  else
    begin
    C.Name:=S;
    Changed;
    end;
end;

procedure TfraConnections.TVConnectionsEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin
  allowEdit:=(Node.ImageIndex=idxConnection);
end;


procedure TfraConnections.TVConnectionsStartDrag(Sender: TObject; var DragObject: TDragObject);

Var
  SDO : TStringsDragObject;
  I : Integer;
  TN : TTreeNode;

begin
  SDO:=TStringsDragObject.Create(TVConnections);
  DragObject:=SDO;
  for I:=0 to TVConnections.Items.Count-1 do
     begin
     TN:=TVConnections.Items[I];
     if TN.Selected and (TN.ImageIndex=idxTable) then
       SDO.Items.AddObject(TN.Text,TObject(TN.Data) as TMySQLDBRestConnection);
     end;
end;

procedure TfraConnections.SetConnections(AValue: TSQLDBRestConnectionList);
begin
  if FConnections=AValue then Exit;
  FConnections.Assign(AValue);
  ShowConnections;
end;


function TfraConnections.GetCurrentConnection: TMySQLDBRestConnection;

Var
  N : TTreeNode;

begin
  N:=TVConnections.Selected;
  While (N<>nil) and (N.ImageIndex<>idxConnection) do
    N:=N.Parent;
  if N=nil then
    Result:=nil
  else
    Result:=TObject(N.Data) as TMySQLDBRestConnection;
end;

constructor TfraConnections.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnections:=TSQLDBRestConnectionList.Create(TMySQLDBRestConnection);
end;

destructor TfraConnections.Destroy;
begin
  FreeAndNil(FConnections);
  inherited Destroy;
end;

procedure TfraConnections.ShowTable(TN: TTreeNode; aConnection: TMySQLDBRestConnection;
  const aTableName: string);

begin
  TN.ImageIndex:=idxTable;
  TN.SelectedIndex:=idxTable;
  TN.Data:=aConnection;
  TN.Text:=aTableName;
end;

procedure TfraConnections.ShowConnectionTables(TN : TTreeNode; aConnection : TMySQLDBRestConnection);

Var
  L : TStringList;
  S : String;
  N : TTreeNode;

begin
  TN.DeleteChildren;
  if aConnection.MyConnection=Nil then
    aConnection.CreateConnection;
  try
    aConnection.MyConnection.Connected:=True;
  except
    On E : EDatabaseError do
      begin
      ShowMessage(Format(SErrShowingTablesConnectingTo,[aConnection.Name,E.Message]));
      exit;
      end;
  end;
  L:=TStringList.Create;
  try
    aConnection.MyConnection.GetTableNames(L);
    L.Sorted:=true;
    For S in L do
      begin
      N:=TVConnections.Items.AddChild(Tn,S);
      ShowTable(N,aConnection,S);
      end;
  finally
    L.Free;
  end;
end;

procedure TfraConnections.ShowConnection(TN: TTreeNode; aConnection: TMySQLDBRestConnection; ShowTables: Boolean);

begin
  TN.Text:=aConnection.Name;
  TN.ImageIndex:=idxConnection;
  TN.SelectedIndex:=idxConnection;
  TN.Data:=aConnection;
  if ShowTables then
    ShowConnectionTables(TN,aConnection);
end;

procedure TfraConnections.ShowConnections;

Var
  TN : TTreeNode;
  C : TMySQLDBRestConnection;
  I : Integer;

begin
  TVConnections.Items.BeginUpdate;
  try
    TVConnections.Items.Clear;
    For I:=0 to Connections.Count-1 do
      begin
      C:=Connections[i] as TMySQLDBRestConnection;
      TN:=TVConnections.Items.AddChild(Nil,C.Name);
      ShowConnection(TN,C);
      end;
  finally
    TVConnections.Items.EndUpdate;
  end;
end;

procedure TfraConnections.LoadSession(aStorage: TCustomPropertyStorage);
begin
  // Do nothing
end;

procedure TfraConnections.SaveSession(aStorage: TCustomPropertyStorage);
begin
  // Do nothing
end;

procedure TfraConnections.DeleteConnection(aConnection : TSQLDBRestConnection);

Var
  TN : TTreeNode;

begin
  TN:=TVConnections.Items.FindNodeWithData(aConnection);
  if Assigned(TN) then
    TVConnections.Items.Delete(TN);
  aConnection.Free;
  Changed;
end;

procedure TfraConnections.AConnectionAddExecute(Sender: TObject);

Var
  C : TMySQLDBRestConnection;
  TN : TTreeNode;

begin
  C:=FConnections.Add as TMySQLDBRestConnection;
  if not EditSQLDBRestConnection(C) then
    C.Free
  else
    begin
    TN:=TVConnections.Items.AddChild(Nil,C.Name);
    ShowConnection(Tn,C,True);
    Changed;
    end;
end;

procedure TfraConnections.AConnectionDeleteExecute(Sender: TObject);
begin
  DeleteConnection(CurrentConnection);
end;

procedure TfraConnections.AConnectionDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentConnection);
end;

procedure TfraConnections.AConnectionEditExecute(Sender: TObject);

Var
  C : TMySQLDBRestConnection;
  TN : TTreeNode;

begin
  C:=CurrentConnection;
  if EditSQLDBRestConnection(C) then
    begin
    TN:=TVConnections.Items.FindNodeWithData(C);
    if Assigned(TN) then
      ShowConnection(Tn,C,True);
    Changed;
    end;
end;

procedure TfraConnections.AConnectionEditUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentConnection);
end;

procedure TfraConnections.AConnectionExposeExecute(Sender: TObject);
begin
  if Assigned(OnImportSchema) then
    OnImportSchema(CurrentConnection);
end;

procedure TfraConnections.AConnectionExposeUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(OnImportSchema);
end;

procedure TfraConnections.AConnectionRefreshExecute(Sender: TObject);

Var
  C : TMySQLDBRestConnection;
  TN : TTreeNode;

begin
  C:=CurrentConnection;
  if Assigned(C) then
    begin
    TN:=TVConnections.Items.FindNodeWithData(C);
    if Assigned(TN) then
      ShowConnectionTables(TN,C);
    end;
end;

procedure TfraConnections.AConnectionRefreshUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentConnection);
end;

procedure TfraConnections.WriteConnectionsToIni(const aFileName,
  ASection: String);

Var
  S,L : String;
  I : Integer;
  aIni : TMemIniFile;

begin
  aIni:=TMemIniFile.Create(aFileName);
  try
    L:='';
    for I:=0 to Connections.Count-1 do
      begin
      if (L<>'') then
        L:=L+',';
      L:=L+Connections[i].Name;
      end;
    aIni.WriteString(aSection,KeyConnections,L);
    for I:=0 to Connections.Count-1 do
      begin
      S:=Connections[i].Name;
      L:=ConnectionIniOptionsToStr([]);
      Connections[i].SaveToIni(aIni,S,[]);
      aIni.WriteString(S,KeyLoadOptions,L);
      end;
  finally
    aIni.Free;
  end;
end;

procedure TfraConnections.Changed;
begin
  FModified:=True;
end;

procedure TfraConnections.ReadConnectionsFromIni(const aFileName,
  ASection: String);

Var
  S,L : String;
  I : Integer;
  C : TSQLDBRestConnection;
  CIO : TConnectionIniOptions;
  aIni : TMemIniFile;

begin
  // Read connections
  aIni:=TMemIniFile.Create(aFileName);
  try
    L:=aIni.ReadString(aSection,KeyConnections,'');
    For I:=1 to WordCount(L,[',']) do
      begin
      S:=ExtractWord(I,L,[',']);
      C:=Connections.AddConnection('','','','','');
      C.Name:=S;
      CIO:=StrToConnectionIniOptions(aIni.ReadString(S,KeyLoadOptions,''));
      C.LoadFromIni(aIni,S,CIO);
      end;
  finally
    aIni.Free;
  end;
end;

procedure TfraConnections.LoadConnections(const aFileName: string);

begin
  case lowercase(ExtractFileExt(aFileName)) of
  '.ini' : ReadConnectionsFromIni(aFileName,'Connections');
  '.json' : Connections.LoadFromFile(aFileName)
  end;
  ShowConnections;
  FModified:=False;
  FFileName:=aFileName;
end;

procedure TfraConnections.SaveConnections(const aFileName: String);

begin
  case lowercase(ExtractFileExt(aFileName)) of
  '.ini' : WriteConnectionsToIni(aFileName,'Connections');
  '.json' : Connections.SaveToFile(aFileName)
  end;
  FModified:=False;
  FFileName:=aFileName;
end;

end.

