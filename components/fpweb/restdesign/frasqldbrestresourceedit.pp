unit frasqldbrestresourceedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, ActnList, lresources,
  sqldbrestbridge, sqldbRestSchema, SynEdit, SynHighlighterSQL, sqldbschemaedittools, frasqldbresourcefields;

type

  { TSQLDBRestResourceEditFrame }

  TSQLDBRestResourceEditFrame = class(TBaseEditFrame)
    AUpdateFields: TAction;
    AValidateSQL: TAction;
    AGenerateSQL: TAction;
    aLResource: TActionList;
    BFields1: TButton;
    BGenerate: TButton;
    BValidate: TButton;
    BFields: TButton;
    CBEnabled: TCheckBox;
    CGOperations: TCheckGroup;
    CBConnection: TComboBox;
    CBInMetadata: TCheckBox;
    EName: TEdit;
    ETableName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PageControl1: TPageControl;
    PButtons: TPanel;
    PButtons1: TPanel;
    fraFields: TResourceFieldsEditFrame;
    SESelect: TSynEdit;
    SEInsert: TSynEdit;
    SEupdate: TSynEdit;
    SEDelete: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    TSFields: TTabSheet;
    TSSelect: TTabSheet;
    TSInsert: TTabSheet;
    TabSheet3: TTabSheet;
    TSDelete: TTabSheet;
    procedure AGenerateSQLExecute(Sender: TObject);
    procedure AUpdateFieldsExecute(Sender: TObject);
    procedure AUpdateFieldsUpdate(Sender: TObject);
    procedure AValidateSQLExecute(Sender: TObject);
    procedure AValidateSQLUpdate(Sender: TObject);
    procedure ETableNameEditingDone(Sender: TObject);
  private
    FOnFieldsChanged: TNotifyEvent;
    FResource: TSQLDBRestResource;
    function GetOnFieldSelected: TNotifyEvent;
    function HaveSelectSQL: Boolean;
    procedure SetOnFieldSelected(AValue: TNotifyEvent);
    procedure SetResource(AValue: TSQLDBRestResource);
  Protected
    procedure FieldsChanged;
    Procedure UpdateFieldList;
    procedure SetConnections(AValue: TSQLDBRestConnectionList); override;
    Procedure SetFrameData(aData: TObject); override;
  public
    Function Modified : Boolean; override;
    Procedure SaveData; override;
    procedure ShowConnections;
    Procedure ShowResource;
    Function FrameCaption: String; override;
    Property Resource : TSQLDBRestResource Read FResource Write SetResource;
    Property OnFieldsChanged : TNotifyEvent Read FOnFieldsChanged Write FOnFieldsChanged;
    Property OnSelectField : TNotifyEvent Read GetOnFieldSelected Write SetOnFieldSelected;
  end;

implementation

uses dialogs, sqldb;

{$R *.lfm}


{ TSQLDBRestResourceEditFrame }

procedure TSQLDBRestResourceEditFrame.AGenerateSQLExecute(Sender: TObject);
begin
  SESelect.Lines.Text:=Resource.GenerateDefaultSQL(skSelect);
end;

procedure TSQLDBRestResourceEditFrame.AUpdateFieldsExecute(Sender: TObject);

begin
  if Resource.Fields.Count>0 then
     if QuestionDlg(SResetFields, Format(SResetFieldsPrompt, [LineEnding, LineEnding]), mtWarning, [mrYes, SYesResetFields, mrNo,
       SDoNotResetFields], 0) <> mrYes then exit;
  UpdateFieldList;
end;

function TSQLDBRestResourceEditFrame.HaveSelectSQL: Boolean;

begin
  Result:=(SESelect.Lines.Count>0) and (Trim(SESelect.Lines[0])<>'');
end;

function TSQLDBRestResourceEditFrame.GetOnFieldSelected: TNotifyEvent;
begin
  Result:=FraFields.OnSelectField;
end;

procedure TSQLDBRestResourceEditFrame.SetOnFieldSelected(AValue: TNotifyEvent);
begin
  FraFields.OnSelectField:=aValue;
end;

procedure TSQLDBRestResourceEditFrame.AUpdateFieldsUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=(ETableName.Text<>'') or HaveSelectSQL;
end;

procedure TSQLDBRestResourceEditFrame.AValidateSQLExecute(Sender: TObject);

begin
  With ExecuteSelect(CBConnection.Text,Resource.ProcessSQl(SESelect.Lines.text,'(1=0)','','')) do
    Free;
  ShowMessage(SSQLValidatesOK);
end;

procedure TSQLDBRestResourceEditFrame.AValidateSQLUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=CanGetSQLConnection and HaveSelectSQL;
end;

procedure TSQLDBRestResourceEditFrame.ETableNameEditingDone(Sender: TObject);
begin
  if Not SameText(ETableName.Text,Resource.TableName)
     and (Resource.Fields.Count>0)
     and Not HaveSelectSQL then
    if MessageDlg(Format(STableNameChanged, [LineEnding]), mtWarning, [mbYes, mbNo], 0) = mrYes then
      UpdateFieldList;
end;

procedure TSQLDBRestResourceEditFrame.SetResource(AValue: TSQLDBRestResource);
begin
  if FResource=AValue then Exit;
  FResource:=AValue;
  ShowResource;
end;

procedure TSQLDBRestResourceEditFrame.FieldsChanged;
begin
  FraFields.ShowResource;
  If Assigned(FonFieldsChanged) then
    FOnFieldsChanged(FResource);
end;

procedure TSQLDBRestResourceEditFrame.UpdateFieldList;

Var
  Q : TSQLQuery;
  SQL : String;
  idxFields : TStringArray;

begin
  SQL:=Trim(SESelect.Lines.Text);
  if SQL='' then
    SQL:=Resource.GenerateDefaultSQL(skSelect);
  Q:=ExecuteSelect(CBConnection.Text,Resource.ProcessSQl(SQL,'(1=0)','',''));
  try
    Resource.Fields.Clear;
    idxFields:=TSQLDBRestSchema.GetPrimaryIndexFields(Q);
    Resource.PopulateFieldsFromFieldDefs(Q.FieldDefs,idxFields,Nil,MinFieldOptions);
    FieldsChanged;
  finally
    Q.Free;
  end;
end;

procedure TSQLDBRestResourceEditFrame.ShowConnections;

Var
  I : Integer;

begin
  With CBConnection.Items do
      begin
      BeginUpdate;
      try
        if Not assigned(Connections) then
          For I:=0 to Connections.Count-1 do
            AddObject(Connections[i].Name,Connections[i]);
      finally
        EndUpdate;
      end;
      end;
end;

procedure TSQLDBRestResourceEditFrame.SetConnections(AValue: TSQLDBRestConnectionList);
begin
  inherited SetConnections(AValue);
  ShowConnections;
end;

procedure TSQLDBRestResourceEditFrame.SetFrameData(aData: TObject);
begin
  Resource:=aData as TMySQLDBRestResource;
end;

function TSQLDBRestResourceEditFrame.Modified: Boolean;

  Function Diff(S1,S2 : TStrings) : Boolean;

  begin
    Result:=Trim(S1.Text)<>Trim(S2.Text);
  end;

  Procedure DoOperation(O : TRestOperation);

  begin
    Result:=Result or (CGOperations.Checked[Ord(O)-1] <> (O in Resource.AllowedOperations));
  end;

Var
  O : TRestOperation;

begin
  Result:=False;
  With Resource do
    begin
    Result:=(ResourceName<>eName.Text) Or
            (TableName<>ETableName.Text) Or
            (Enabled<>CBEnabled.Checked) Or
            (InMetadata<>CBInMetadata.Checked) or
            Diff(SQLSelect, SESelect.Lines) Or
            Diff(SQLInsert,SEInsert.Lines) Or
            Diff(SQLUpdate,SEUpdate.Lines) Or
            Diff(SQLDelete,SEDelete.Lines);
    for O in TRestOperation do
      if o<>roUnknown then
        DoOperation(O);
    end;
end;

procedure TSQLDBRestResourceEditFrame.SaveData;

  Procedure DoOperation(O : TRestOperation);

  begin
    if CGOperations.Checked[Ord(O)-1] then
      Resource.AllowedOperations:=Resource.AllowedOperations+[O]
    else
      Resource.AllowedOperations:=Resource.AllowedOperations-[O]
  end;

Var
  O : TRestOperation;

begin
  With Resource do
    begin
    ResourceName := eName.Text;
    TableName    := ETableName.Text;
    SQLSelect    := SESelect.Lines;
    SQLInsert    := SEInsert.Lines;
    SQLUpdate    := SEUpdate.Lines;
    SQLDelete    := SEDelete.Lines;
    Enabled      := CBEnabled.Checked;
    InMetadata   := CBInMetadata.Checked;
    for O in TRestOperation do
      if o<>roUnknown then
        DoOperation(O);
    end;
end;

procedure TSQLDBRestResourceEditFrame.ShowResource;

  Procedure DoOperation(O : TRestOperation);

  begin
    CGOperations.Checked[Ord(O)-1]:=O in Resource.AllowedOperations;
  end;

Var
  O : TRestOperation;

begin
  With Resource do
    begin
    eName.Text:=ResourceName;
    ETableName.Text:=TableName;
    SESelect.Lines:=SQLSelect;
    SEInsert.Lines:=SQLInsert;
    SEUpdate.Lines:=SQLUpdate;
    SEDelete.Lines:=SQLDelete;
    CBEnabled.Checked:=Enabled;
    CBInMetadata.Checked:=InMetadata;
    for O in TRestOperation do
      if o<>roUnknown then
        DoOperation(O);
    end;
end;

function TSQLDBRestResourceEditFrame.FrameCaption: String;
begin
  if FResource=Nil then
    Result:=SUnknownObject
  else
    Result:=FResource.ResourceName;
  Result:=Format(SEditObject,[SResource,Result]);
end;

end.

