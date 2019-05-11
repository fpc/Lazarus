unit fraSQLDBRestSchemaEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls, ActnList, PropertyStorage, sqldbschemaedittools, sqldbrestschema, sqldbrestbridge;

type

  { TSQLDBRestSchemaEditorFrame }

  TSQLDBRestSchemaEditorFrame = class(TFrame)
    AAddField: TAction;
    ADeleteField: TAction;
    AEditField: TAction;
    AResourceEdit: TAction;
    AResourceDelete: TAction;
    AResourceAdd: TAction;
    AShowConnections: TAction;
    alResources: TActionList;
    ILResources: TImageList;
    LFrame: TLabel;
    LResources: TLabel;
    PDock: TPanel;
    PResources: TPanel;
    Splitter1: TSplitter;
    TBResources: TToolBar;
    TBShowConnectionsPane: TToolButton;
    ToolButton1: TToolButton;
    TBResourceAdd: TToolButton;
    TBResourceEdit: TToolButton;
    TBResourceDelete: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    TVResources: TTreeView;
    procedure AAddFieldExecute(Sender: TObject);
    procedure AAddFieldUpdate(Sender: TObject);
    procedure ADeleteFieldExecute(Sender: TObject);
    procedure ADeleteFieldUpdate(Sender: TObject);
    procedure AEditFieldExecute(Sender: TObject);
    procedure AEditFieldUpdate(Sender: TObject);
    procedure AResourceAddExecute(Sender: TObject);
    procedure AResourceDeleteExecute(Sender: TObject);
    procedure AResourceDeleteUpdate(Sender: TObject);
    procedure AResourceEditExecute(Sender: TObject);
    procedure AResourceEditUpdate(Sender: TObject);
    procedure AShowConnectionsExecute(Sender: TObject);
    procedure AShowConnectionsUpdate(Sender: TObject);
    procedure TVResourcesChange(Sender: TObject; Node: TTreeNode);
    procedure TVResourcesDblClick(Sender: TObject);
    procedure TVResourcesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TVResourcesDragOver(Sender, Source: TObject; X, Y: Integer;  State: TDragState; var Accept: Boolean);
    procedure TVResourcesEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure TVResourcesEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
  private
    FConnectionPane: TWinControl;
    FConnections: TSQLDBRestConnectionList;
    FOnChanged: TNotifyEvent;
    FSchema: TMySQLDBRestSchema;
    FImportOpts : TRestFieldOptions;
    FCurrentFrame : TBaseEditFrame;
    FSchemaNode : TTreeNode;
    FModified : Boolean;
    // Data handling
    Procedure Changed;
    procedure DoFieldListChanged(Sender: TObject);
    function GetModified: Boolean;
    Function SelectedResource : TMySQLDBRestResource;
    Function SelectedField : TSQLDBRestField;
    function AddFieldToResource(aResource: TMySQLDBRestResource): TSQLDBRestField;
    function AddResource: TMySQLDBRestResource;
    Procedure DeleteResource(R : TMySQLDBRestResource);
    Procedure DeleteField(F : TSQLDBRestField);
    Function ImportResource(aConnection: TMySQLDBRestConnection; const ATableName: UTF8String; AMinFieldOptions : TRestFieldOptions =  []) : TTreeNode;
    // Various editors
    procedure ConfigFrame(F: TBaseEditFrame; aData: TObject);
    procedure RemoveCurrentFrame(DoRefreshNode: Boolean=True);
    procedure ShowFieldEditor(aField: TSQLDBRestField);
    procedure ShowSchemaEditor(aSchema : TSQLDBRestSchema);
    procedure ShowResourceEditor(aResource: TMySQLDBRestResource);
    procedure ShowFieldsEditor(aResource: TMySQLDBRestResource);
    // Various dialogs
    procedure ShowDialogForObject(aData : TObject);
    function ShowEditFrameInForm(aFrame: TBaseEditFrame; aData : TObject): Boolean;
    procedure ShowResourceDialog(aResource: TMySQLDBRestResource);
    procedure ShowFieldDialog(aField: TSQLDBRestField);
    // Tree node handling/filling
    procedure ChangeResourceName(R: TMySQLDBRestResource; Node: TTreeNode; var S: string);
    procedure ChangeRestFieldName(F: TSQLDBRestField; Node: TTreeNode; var S: string);
    procedure DoOnSelectResource(Sender: TObject);
    procedure DoOnSelectField(Sender: TObject);
    procedure RefreshNode(aData: TObject);
    Function AddResourceToTree(Res: TMySQLDBRestResource) : TTreeNode;
    function FindFieldNode(aResource: TSQLDBRestField): TTreeNode;
    function FindResourceNode(aResource: TSQLDBRestResource): TTreeNode;
    function FindResourceFieldsNode(aResource: TSQLDBRestResource): TTreeNode;
    procedure ShowResource(aNode: TTreeNode; aResource: TMySQLDBRestResource);
    procedure ShowResourceFields(aNode: TTreeNode; aResource: TMySQLDBRestResource);
    procedure ShowRestField(aNode: TTreeNode; aField: TSQLDBRestField);
    Function DoCheckSave : Boolean;
  public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure LoadSession(aStorage : TCustomPropertyStorage); virtual;
    Procedure SaveSession(aStorage : TCustomPropertyStorage); virtual;
    Procedure ClearSchema;
    Function CheckSave : Boolean;
    Property Modified : Boolean Read GetModified;
    Procedure LoadFromFile(const aFileName : String);
    Procedure SaveToFile(const aFileName : String);
    Procedure ShowResources;
    Property ImportOpts : TRestFieldOptions Read FImportOpts;
    Property ConnectionPane : TWinControl Read FConnectionPane Write FConnectionPane;
    Property Schema : TMySQLDBRestSchema Read FSchema;
    Property Connections : TSQLDBRestConnectionList Read FConnections Write FConnections;
    Property OnChanged : TNotifyEvent Read FOnChanged Write FOnChanged;
  end;

implementation

uses typinfo,dialogs, dlgrestfieldoptions, frasqldbrestresourceedit, frasqldbresourcefields,  frasqldbrestfieldedit, fraschematableseditor, frmeditframedialog;

{$R *.lfm}



{ TSQLDBRestSchemaEditorFrame }

procedure TSQLDBRestSchemaEditorFrame.RefreshNode(aData : TObject);

Var
  R : TMySQLDBRestResource;
  F : TSQLDBRestField;
  N : TTreeNode;

begin
  if AData is TSQLDBRestSchema then
    ShowResources
  else if AData is TMySQLDBRestResource then
    begin
    R:=AData as TMySQLDBRestResource;
    N:=FindResourceNode(R);
    if Assigned(N) then
      ShowResource(N,R);
    end
  else if (aData is TSQLDBRestField) then
    begin
    F:=aData as TSQLDBRestField;
    N:=FindFieldNode(F);
    If Assigned(N) then
      ShowRestField(N,F);
    end;
end;

procedure TSQLDBRestSchemaEditorFrame.RemoveCurrentFrame(DoRefreshNode : Boolean = True);
begin
  // Dochecksave does no Changed, changed is called after node is refreshed
  if DoCheckSave then
    begin
    if DoRefreshNode then
      RefreshNode(FCurrentFrame.FrameData);
    Changed;
    end;
  FreeAndNil(FCurrentFrame);
end;

procedure TSQLDBRestSchemaEditorFrame.ConfigFrame(F: TBaseEditFrame; aData : TObject);

begin
  if aData=Nil then
    Raise ESQLDBRest.CreateFmt(0,'Internalerror : no data passed for frame %s',[F.ClassName]);
  With F do
    begin
    Parent:=PDock;
    Align:=alClient;
    Connections:=Self.Connections;
    FrameData:=aData;
    MinFieldOptions:=ImportOpts;
    LFrame.Caption:=F.FrameCaption;
    end;
  FCurrentFrame:=F;
end;



procedure TSQLDBRestSchemaEditorFrame.ShowResourceEditor(aResource: TMySQLDBRestResource);

Var
  F : TSQLDBRestResourceEditFrame;

begin
  RemoveCurrentFrame;
  F:=TSQLDBRestResourceEditFrame.Create(Self);
  F.OnFieldsChanged:=@DoFieldListChanged;
  F.OnSelectField:=@DoOnSelectField;
  ConfigFrame(F,aResource);
end;

procedure TSQLDBRestSchemaEditorFrame.ShowFieldsEditor(aResource: TMySQLDBRestResource);

Var
  F : TResourceFieldsEditFrame;

begin
  RemoveCurrentFrame;
  F:=TResourceFieldsEditFrame.Create(Self);
  F.OnSelectField:=@DoOnSelectField;
  ConfigFrame(F,aResource);
end;


function TSQLDBRestSchemaEditorFrame.ShowEditFrameInForm(aFrame: TBaseEditFrame; aData : TObject): Boolean;

Var
  Frm : TEditFrameForm;

begin
  Frm:=TEditFrameForm.Create(Self);
  try
    AFrame.Connections:=Self.Connections;
    AFrame.FrameData:=aData;
    Frm.EditFrame:=aFrame;
    Result:=frm.ShowModal=mrOK;
    aFrame.Free;
  finally
    Frm.Free;
  end;
end;

procedure TSQLDBRestSchemaEditorFrame.ShowResourceDialog(aResource: TMySQLDBRestResource);

Var
  F : TSQLDBRestResourceEditFrame;
  N  : TTreeNode;
begin
  F:=TSQLDBRestResourceEditFrame.Create(Self);
  if ShowEditFrameInForm(F,aResource) then
    begin
    N:=FindResourceNode(aResource);
    if Assigned(N) then
      begin
      ShowResource(N,aResource);
      TVResources.Selected:=N;
      end;
    Changed;
    end;
end;

procedure TSQLDBRestSchemaEditorFrame.ShowFieldDialog(aField: TSQLDBRestField);

Var
  F : TSQLDBRestFieldEditFrame;
  N  : TTreeNode;

begin
  F:=TSQLDBRestFieldEditFrame.Create(Self);
  if ShowEditFrameInForm(F,aField) then // Frees frame;
    begin
    N:=FindFieldNode(aField);
    if Assigned(N) then
      begin
      ShowRestField(N,aField);
      TVResources.Selected:=N;
      end;
    Changed;
    end;
end;

procedure TSQLDBRestSchemaEditorFrame.ShowFieldEditor(aField: TSQLDBRestField);

Var
  F : TSQLDBRestFieldEditFrame;

begin
  RemoveCurrentFrame;
  F:=TSQLDBRestFieldEditFrame.Create(Self);
  ConfigFrame(F,aField);
  F.Field:=aField;
end;

function TSQLDBRestSchemaEditorFrame.FindResourceNode(
  aResource: TSQLDBRestResource): TTreeNode;

Var
  N : TTreeNode;

begin
  N:=TVResources.Items.FindNodeWithData(aResource);
  if Assigned(N) then
    if N.ImageIndex<>idxTable then
      N:=N.Parent;
  Result:=N;
end;

function TSQLDBRestSchemaEditorFrame.FindResourceFieldsNode(aResource: TSQLDBRestResource): TTreeNode;

begin
  Result:=FindResourceNode(aResource);
  if Result<>Nil then
    begin
    Result:=Result.GetFirstChild;
    While (Result<>Nil) and (Result.ImageIndex<>idxFields) do
      Result:=Result.GetNextSibling;
    end;
end;

procedure TSQLDBRestSchemaEditorFrame.DoOnSelectResource(Sender : TObject);

Var
  N : TTreeNode;

begin
  if (Sender is TSQLDBRestResource) then
    begin
    N:=FindResourceNode(Sender as TSQLDBRestResource);
    if Assigned(N) then
      TVResources.Selected:=N;
    end;
end;

procedure TSQLDBRestSchemaEditorFrame.DoOnSelectField(Sender: TObject);
Var
  N : TTreeNode;

begin
  if (Sender is TSQLDBRestField) then
    begin
    N:=FindFieldNode(Sender as TSQLDBRestField);
    if Assigned(N) then
      TVResources.Selected:=N;
    end;
end;

procedure TSQLDBRestSchemaEditorFrame.ShowSchemaEditor(aSchema: TSQLDBRestSchema);
Var
  F : TSQLDBRestSchemaTablesEditFrame;

begin
  RemoveCurrentFrame;
  F:=TSQLDBRestSchemaTablesEditFrame.Create(Self);
  ConfigFrame(F,aSchema);
  F.OnSelectResource:=@DoOnSelectResource;
end;

procedure TSQLDBRestSchemaEditorFrame.TVResourcesChange(Sender: TObject; Node: TTreeNode);
begin
  Case Node.ImageIndex of
    idxConnection : ShowSchemaEditor(TSQLDBRestSchema(Node.Data));
    idxTableInfo,
    idxTable : ShowResourceEditor(TMySQLDBRestResource(Node.Data));
    idxField : ShowFieldEditor(TSQLDBRestField(Node.Data));
    idxFields : ShowFieldsEditor(TMySQLDBRestResource(Node.Data));
  end;
end;

procedure TSQLDBRestSchemaEditorFrame.ShowDialogForObject(aData : TObject);

begin
  if aData is TMySQLDBRestResource then
    ShowResourceDialog(aData as TMySQLDBRestResource)
  else if aData is TSQLDBRestField then
    ShowFieldDialog(aData as TSQLDBRestField);
end;

procedure TSQLDBRestSchemaEditorFrame.TVResourcesDblClick(Sender: TObject);
begin
  if Assigned(TVResources.Selected) then
    if Assigned(TVResources.Selected.Data) then
      ShowDialogForObject(TObject(TVResources.Selected.Data));
end;

function TSQLDBRestSchemaEditorFrame.ImportResource(
  aConnection: TMySQLDBRestConnection; const ATableName: UTF8String;
  AMinFieldOptions: TRestFieldOptions): TTreeNode;

Var
  Res : TSQLDBRestResource;
  N : String;
  i : Integer;

begin
  N:=aTableName;
  I:=0;
  Res:=Schema.Resources.FindResourceByName(N);
  While Res<>Nil do
    begin
    Inc(i);
    N:=aTableName+IntToStr(I);
    Res:=Schema.Resources.FindResourceByName(N);
    end;
  Res:=Schema.Resources.AddResource(aTableName,N);
  Schema.PopulateResourceFields(aConnection.MyConnection,Res,AMinFieldOptions);
  Result:=AddResourceToTree(Res as TMySQLDBRestResource);
  Changed;
end;


procedure TSQLDBRestSchemaEditorFrame.TVResourcesDragDrop(Sender, Source: TObject; X, Y: Integer);

Var
  I : Integer;
  SDO : TStringsDragObject;
  TN : TTreeNode;

begin
  SDO:=Source as TStringsDragObject;
  TN:=nil;
  // FImportOpts so we reuse them
  if GetRestFieldOptions(FimportOpts) then
    For I:=SDO.Items.Count-1 downto 0 do
      TN:=ImportResource(SDO.Items.Objects[i] as TMySQLDBRestConnection,SDO.Items[i],FimportOpts);
  if TN<>Nil then
    begin
    TN.Expanded:=True;
    TVResources.Selected:=TN;
    end;
end;

procedure TSQLDBRestSchemaEditorFrame.TVResourcesDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=(Source is TStringsDragObject) and (TStringsDragObject(Source).Items.Count>0);
end;

procedure TSQLDBRestSchemaEditorFrame.ChangeResourceName(R : TMySQLDBRestResource; Node: TTreeNode; var S: string);

Var
  DR : TMySQLDBRestResource;

begin
  DR:=TMySQLDBRestResource(Schema.Resources.FindResourceByName(S));
  if (DR<>Nil) and (DR<>R) then
    begin
    ShowMessage(Format(SErrDuplicateResource,[DR.ResourceName]));
    S:=R.ResourceName;
    end
  else
    begin
    if FCurrentFrame is TSQLDBRestResourceEditFrame then
      RemoveCurrentFrame(True);
    R.ResourceName:=S;
    ShowResourceEditor(R);
    end;
end;

procedure TSQLDBRestSchemaEditorFrame.ChangeRestFieldName(F : TSQLDBRestField; Node: TTreeNode; var S: string);

Var
  DF : TSQLDBRestField;

begin
  DF:=(F.Collection as TSQLDBRestFieldList).FindByPublicName(S);
  if (DF<>Nil) and (DF<>F) then
    begin
    ShowMessage(Format(SErrDuplicateField,[DF.PublicName]));
    S:=F.PublicName;
    end
  else
    begin
    if FCurrentFrame is TSQLDBRestFieldEditFrame then
      RemoveCurrentFrame(True);
    F.PublicName:=S;
    ShowFieldEditor(F);
    end;
end;


procedure TSQLDBRestSchemaEditorFrame.TVResourcesEdited(Sender: TObject; Node: TTreeNode; var S: string);

begin
  if TObject(Node.Data) is TMySQLDBRestResource then
    ChangeResourceName(TMySQLDBRestResource(Node.Data),Node,S)
  else if TObject(Node.Data) is TSQLDBRestField then
    ChangeRestFieldName(TSQLDBRestField(Node.Data),Node,S)
end;

procedure TSQLDBRestSchemaEditorFrame.TVResourcesEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
begin
  AllowEdit:=(Node.ImageIndex in [idxTable,idxField]);
end;

procedure TSQLDBRestSchemaEditorFrame.Changed;
begin
  FModified:=True;
  if Assigned(FOnChanged) then
    FOnChanged(FSchema);
end;

procedure TSQLDBRestSchemaEditorFrame.DoFieldListChanged(Sender: TObject);

Var
  N: TTreeNode;
  R : TMySQLDBRestResource;

begin
  // Sender is resource.
  R:=Sender as TMySQLDBRestResource;
  N:=FindResourceNode(R);
  if Assigned(N) then
    ShowResource(N,R);
end;


function TSQLDBRestSchemaEditorFrame.FindFieldNode(aResource: TSQLDBRestField): TTreeNode;

Var
  N : TTreeNode;

begin
  N:=TVResources.Items.FindNodeWithData(aResource);
  if N.ImageIndex=idxField then
    Result:=N
  else
    Result:=Nil;
end;

function TSQLDBRestSchemaEditorFrame.SelectedResource: TMySQLDBRestResource;

Var
  N : TTreeNode;

begin
  N:=TVResources.Selected;
  While (N<>Nil) and (N.ImageIndex<>idxTable) do
    N:=N.Parent;
  if Assigned(N) and (TObject(N.Data) is TMySQLDBRestResource) then
    Result:=TMySQLDBRestResource(N.Data)
  else
    Result:=Nil;
end;

function TSQLDBRestSchemaEditorFrame.GetModified: Boolean;
begin
  Result:=FModified;
end;

function TSQLDBRestSchemaEditorFrame.SelectedField: TSQLDBRestField;

Var
  N : TTreeNode;

begin
  N:=TVResources.Selected;
  While (N<>Nil) and (N.ImageIndex<>idxField) do
    N:=N.Parent;
  if Assigned(N) and (TObject(N.Data) is TSQLDBRestField) then
    Result:=TSQLDBRestField(N.Data)
  else
    Result:=Nil;
end;


constructor TSQLDBRestSchemaEditorFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FSchema:=TMySQLDBRestSchema.Create(Self);
  ShowResources;
  FImportOpts:=[foInInsert,foInUpdate,foFilter,foOrderBy,foOrderByDesc];
end;

destructor TSQLDBRestSchemaEditorFrame.Destroy;
begin
  FreeAndNil(FSchema);
  inherited Destroy;
end;

procedure TSQLDBRestSchemaEditorFrame.LoadSession(aStorage: TCustomPropertyStorage);

Var
  S : String;
  i : Integer;

begin
  With aStorage do
    begin
    PResources.Width:=ReadInteger('ResourceWidth',PResources.Width);
    S:=ReadString('ResourceImportFieldOpts','');
    if S<>'' then
      Try
        I:=StringToSet(PTypeInfo(TypeInfo(TRestFieldOptions)),S);
        FImportOpts:=TRestFieldOptions(I);
      except
        On E : EPropertyError do
          begin
          // Silently Ignore this one
          end;
      end;
    end;
end;

procedure TSQLDBRestSchemaEditorFrame.SaveSession(aStorage: TCustomPropertyStorage);
begin
  With aStorage do
    begin
    WriteInteger('ResourceWidth',PResources.Width);
    WriteString('ResourceImportFieldOpts',SetToString(PTypeInfo(TypeInfo(TRestFieldOptions)),Integer(FImportOpts),False));
    end;
end;

procedure TSQLDBRestSchemaEditorFrame.ClearSchema;
begin
  RemoveCurrentFrame(False);
  Schema.Resources.Clear;
  ShowResources;
end;

function TSQLDBRestSchemaEditorFrame.CheckSave: Boolean;
begin
  Result:=DoCheckSave;
  if Result then
    Changed;
end;

procedure TSQLDBRestSchemaEditorFrame.LoadFromFile(const aFileName: String);
begin
  Schema.LoadFromFile(aFilename);
  ShowResources;
  FModified:=False;
end;

procedure TSQLDBRestSchemaEditorFrame.SaveToFile(const aFileName: String);
begin
  Schema.SaveToFile(aFilename);
  FModified:=False;
end;

procedure TSQLDBRestSchemaEditorFrame.ShowRestField(aNode : TTreeNode; aField : TSQLDBRestField);

begin
  aNode.DeleteChildren;
  aNode.Text:=aField.PublicName;
  aNode.ImageIndex:=idxField;
  aNode.SelectedIndex:=idxField;
  aNode.Data:=aField;
  if (foInKey in aField.Options) then
    aNode.StateIndex:=idxKeyField;
end;

function TSQLDBRestSchemaEditorFrame.DoCheckSave: Boolean;
begin
  Result:=Assigned(FCurrentFrame) and FCurrentFrame.Modified;
  if Result then
    FCurrentFrame.SaveData;
end;

procedure TSQLDBRestSchemaEditorFrame.ShowResourceFields(aNode : TTreeNode; aResource : TMySQLDBRestResource);

Var
  FN,PN : TTreeNode;
  F : TSQLDBRestField;
  I : Integer;

begin
  PN:=aNode.TreeNodes.AddChild(aNode,SFields);
  PN.Data:=aResource;
  PN.ImageIndex:=idxFields;
  PN.SelectedIndex:=idxFields;
  For I:=0 to aResource.Fields.Count-1 do
    begin
    F:=aResource.Fields[i];
    FN:=PN.TreeNodes.AddChild(PN,F.PublicName);
    ShowRestField(FN,F);
    end;
end;

procedure TSQLDBRestSchemaEditorFrame.ShowResource(aNode : TTreeNode; aResource : TMySQLDBRestResource);

Var
  N : TTreeNode;
  S : String;

begin
  aNode.DeleteChildren;
  aNode.Text:=aResource.ResourceName;
  aNode.ImageIndex:=idxTable;
  aNode.SelectedIndex:=idxTable;
  aNode.Data:=aResource;
  S:=Format(SPropTableName,[aResource.TableName]);
  if (aResource.ConnectionName<>'') then
    S:=S+', '+Format(SPropConnection,[aResource.ConnectionName]);
  N:=aNode.TreeNodes.AddChild(aNode,S);
  N.Data:=aResource;
  N.ImageIndex:=idxTableInfo;
  N.SelectedIndex:=idxTableInfo;
  ShowResourceFields(aNode,aResource)
end;

function TSQLDBRestSchemaEditorFrame.AddResourceToTree(Res: TMySQLDBRestResource): TTreeNode;

begin
  Result:=TVResources.Items.AddChild(FSchemaNode,Res.ResourceName);
  ShowResource(Result,Res);
end;

procedure TSQLDBRestSchemaEditorFrame.ShowResources;

Var
  I : integer;
  S : String;

begin
  RemoveCurrentFrame(False);
  TVResources.Selected:=Nil; // Force refresh
  With TVResources.Items do
    try
      BeginUpdate;
      if FSchemaNode=Nil then
        begin
        S:=Schema.Name;
        if S='' then
          S:=SSchema;
        FSchemaNode:=TVResources.Items.AddChild(Nil,SSchema);
        FSchemaNode.Data:=Schema;
        FSchemaNode.ImageIndex:=idxConnection;
        end
      else
        FSchemaNode.DeleteChildren;
      for I:=0 to Schema.Resources.Count-1 do
        AddResourceToTree(Schema.Resources[i] as TMySQLDBRestResource);
      FSchemaNode.Expand(False);
    finally
      EndUpdate;
    end;
  TVResources.Selected:=FSchemaNode;
end;

procedure TSQLDBRestSchemaEditorFrame.AShowConnectionsUpdate(Sender: TObject);
begin
  With (Sender as TAction) do
    begin
    Visible:=Assigned(ConnectionPane);
    if Visible then
      Enabled:=not ConnectionPane.Visible;
    end;
end;

procedure TSQLDBRestSchemaEditorFrame.AShowConnectionsExecute(Sender: TObject);
begin
  if Assigned(ConnectionPane) then
    ConnectionPane.Visible:=True;
end;

procedure TSQLDBRestSchemaEditorFrame.AResourceAddExecute(Sender: TObject);

begin
  AddResource;
end;

procedure TSQLDBRestSchemaEditorFrame.AAddFieldUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(SelectedResource<>Nil)
end;

procedure TSQLDBRestSchemaEditorFrame.ADeleteFieldExecute(Sender: TObject);
Var
  R : TMySQLDBRestResource;
  F : TSQLDBRestField;

begin
  F:=SelectedField;
  R:=SelectedResource;
  if Assigned(F) and (QuestionDlg(SDeleteFieldCaption, Format(SDeleteFieldMsg, [F.PublicName,R.ResourceName, LineEnding]),
                 mtWarning, [mrYes, SYesDelete, mrNo, SNoDoNotDelete], 0) = mrYes) then
    DeleteField(F);
end;

procedure TSQLDBRestSchemaEditorFrame.ADeleteFieldUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=(SelectedField<>Nil)
end;

procedure TSQLDBRestSchemaEditorFrame.AEditFieldExecute(Sender: TObject);

Var
  F : TSQLDBRestField;


begin
  F:=SelectedField;
  if (F<>Nil) then
    ShowFieldDialog(F);
end;

procedure TSQLDBRestSchemaEditorFrame.AEditFieldUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(SelectedField<>Nil);
end;

procedure TSQLDBRestSchemaEditorFrame.AAddFieldExecute(Sender: TObject);
begin
  if (SelectedResource<>Nil) then
    AddFieldToResource(SelectedResource);
end;

function TSQLDBRestSchemaEditorFrame.AddFieldToResource(aResource: TMySQLDBRestResource): TSQLDBRestField;

Var
  N : String;
  F : TSQLDBRestField;
  PN,FN : TTreeNode;

begin
  N:='Field'+IntToStr(aResource.Fields.Count+1);
  Repeat
    F:=Nil;
    // Maybe ask field name ?
    If Not InputQuery(SNewField,Format(SNameForField,[aResource.ResourceName]),N) then
      N:=''
    else
      begin
      F:=aResource.Fields.FindByPublicName(N);
      if F<>Nil then
        ShowMessage(Format(SErrDuplicateField,[N]));
      end;
  Until (F=nil) or (N='');
  if N='' then
    exit;
  Result:=aResource.Fields.AddField(N,rftString,FImportOpts);
  PN:=FindResourceFieldsNode(aResource);
  if Not assigned(PN) then
    Raise ESQLDBRest.CreateFmt(0,'Internal error: Cannot find node for fields for resource %s',[aResource.ResourceName]);
  FN:=TVResources.Items.AddChild(PN,Result.PublicName);
  ShowRestField(FN,Result);
  TVResources.Selected:=FN;
  Changed;
end;

procedure TSQLDBRestSchemaEditorFrame.AResourceDeleteExecute(Sender: TObject);

Var
  R : TMySQLDBRestResource;

begin
  R:=SelectedResource;
  if Assigned(R) and (QuestionDlg(SDeleteResourceCaption, Format(SDeleteResourceMsg, [R.ResourceName, LineEnding]),
                 mtWarning, [mrYes, SYesDelete, mrNo, SNoDoNotDelete], 0) = mrYes) then
    DeleteResource(R);
end;

procedure TSQLDBRestSchemaEditorFrame.AResourceDeleteUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=(SelectedResource<>Nil)
end;

procedure TSQLDBRestSchemaEditorFrame.AResourceEditExecute(Sender: TObject);

Var
  R : TMySQLDBRestResource;

begin
  R:=SelectedResource;
  if (R<>Nil) then
    ShowResourceDialog(R);
end;

procedure TSQLDBRestSchemaEditorFrame.AResourceEditUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=SelectedResource<>Nil;
end;

function TSQLDBRestSchemaEditorFrame.AddResource: TMySQLDBRestResource;

Var
  N : String;
  R : TSQLDBRestResource;

begin
  N:='Resource'+IntToStr(Schema.Resources.Count+1);
  Repeat
    R:=Nil;
    // Maybe ask table name ?
    If Not InputQuery(SNewResource,SNameForResource,N) then
      N:=''
    else
      begin
      R:=Schema.Resources.FindResourceByName(N);
      if R<>Nil then
        ShowMessage(Format(SErrDuplicateResource,[N]));
      end;
  Until (R=nil) or (N='');
  if N='' then
    Result:=nil
  else
    begin
    // Maybe check table name ?
    Result:=Schema.Resources.AddResource(N,N) as TMySQLDBRestResource;
    Changed;
    TVResources.Selected:=AddResourceToTree(Result);
    TVResources.Selected.Expanded:=True;
    end;
end;

procedure TSQLDBRestSchemaEditorFrame.DeleteResource(R: TMySQLDBRestResource);

Var
  NCurrent,NNext : TTreeNode;

begin
  RemoveCurrentFrame(False);
  NCurrent:=FindResourceNode(R);
  if Assigned(NCurrent) then
    begin
    NNext:=NCurrent.GetNextSibling;
    if NNext=Nil then
      NNext:=NCurrent.GetPrevSibling;
    end;
  if NNext=Nil then
    NNext:=FSchemaNode;
  if Assigned(NCurrent) then
    TVResources.Items.Delete(NCurrent);
  TVResources.Selected:=NNext;
  R.Free;
  Changed;
end;

procedure TSQLDBRestSchemaEditorFrame.DeleteField(F: TSQLDBRestField);

Var
  NCurrent,NNext : TTreeNode;

begin
  RemoveCurrentFrame(False);
  NCurrent:=FindFieldNode(F);
  if Assigned(NCurrent) then
    begin
    NNext:=NCurrent.GetNextSibling;
    if NNext=Nil then
      NNext:=NCurrent.GetPrevSibling;
    if NNext=Nil then
      NNext:=NCurrent.Parent;
    end;
  if NNext=Nil then
    begin
    NNext:=TVResources.Selected;
    While (NNext<>Nil) and Not(NNext.ImageIndex in [idxFields,idxTable]) do
      NNext:=NNext.Parent;
    if NNext=Nil then
      NNext:=FSchemaNode;
    end;
  if Assigned(NCurrent) then
    TVResources.Items.Delete(NCurrent);
  TVResources.Selected:=NNext;
  F.Free;
  Changed;
end;

end.

