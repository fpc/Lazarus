unit dlgeditsqldbrestschema;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, XMLPropStorage, frasqldbfullrestschemaaditor,
  sqldbrestschema, sqldbrestbridge;

type

  { TSQLDBRestSchemaEditorForm }

  TSQLDBRestSchemaEditorForm = class(TForm)
    BPSchema: TButtonPanel;
    fraEditor: TSchemaEditorFrame;
    PSSchema: TXMLPropStorage;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PSSchemaRestoreProperties(Sender: TObject);
    procedure PSSchemaSaveProperties(Sender: TObject);
  private
    procedure DoSchemaChanged(Sender: TObject);
    function GetConnections: TSQLDBRestConnectionList;
    function GetConnectionsModified: Boolean;
    function GetSchema: TSQLDBRestSchema;
    function GetSchemaModified: Boolean;
    procedure SetConnections(AValue: TSQLDBRestConnectionList);
    procedure SetSchema(AValue: TSQLDBRestSchema);
    procedure UpdateCaption;
  public
    Property Schema : TSQLDBRestSchema Read GetSchema Write SetSchema;
    Property Connections : TSQLDBRestConnectionList  Read GetConnections Write SetConnections;
    Property SchemaModified : Boolean Read GetSchemaModified;
    Property ConnectionsModified : Boolean Read GetConnectionsModified;
  end;

var
  SQLDBRestSchemaEditorForm: TSQLDBRestSchemaEditorForm;

implementation

uses lazideintf;

{$R *.lfm}

{ TSQLDBRestSchemaEditorForm }

procedure TSQLDBRestSchemaEditorForm.FormCreate(Sender: TObject);
begin
  PSSchema.FileName:=IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath)+'sqldbrestschema.xml';
  PSSchema.Active:=True;
  PSSchema.Restore;
  fraEditor.OnSchemaChanged:=@DoSchemaChanged;
end;

procedure TSQLDBRestSchemaEditorForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  fraEditor.CheckSave;
end;

procedure TSQLDBRestSchemaEditorForm.FormShow(Sender: TObject);
begin
  UpdateCaption;
end;

procedure TSQLDBRestSchemaEditorForm.PSSchemaRestoreProperties(Sender: TObject);
begin
  fraEditor.LoadSession(PSSchema);
end;

procedure TSQLDBRestSchemaEditorForm.PSSchemaSaveProperties(Sender: TObject);
begin
  fraEditor.SaveSession(PSSchema);
end;

procedure TSQLDBRestSchemaEditorForm.UpdateCaption;

Var
  S : String;

begin
  if Assigned(Schema) then
    S:=Schema.Name
  else
    S:='No schema';
  S:='Editing schema '+S;
  if fraEditor.SchemaModified then
    S:=S+' (modified)';
  Caption:=S;
end;

procedure TSQLDBRestSchemaEditorForm.DoSchemaChanged(Sender: TObject);
begin
  UpdateCaption;
end;

function TSQLDBRestSchemaEditorForm.GetConnections: TSQLDBRestConnectionList;
begin
  Result:=fraEditor.Connections;
end;

function TSQLDBRestSchemaEditorForm.GetConnectionsModified: Boolean;
begin
  Result:=fraEditor.ConnectionsModified;
end;

function TSQLDBRestSchemaEditorForm.GetSchema: TSQLDBRestSchema;
begin
  Result:=fraEditor.Schema;
end;

function TSQLDBRestSchemaEditorForm.GetSchemaModified: Boolean;
begin
  Result:=fraEditor.SchemaModified;
end;

procedure TSQLDBRestSchemaEditorForm.SetConnections(AValue: TSQLDBRestConnectionList);
begin
  fraEditor.Connections:=aValue;
end;

procedure TSQLDBRestSchemaEditorForm.SetSchema(AValue: TSQLDBRestSchema);
begin
  fraEditor.Schema:=aValue;
  UpdateCaption;
end;

end.

