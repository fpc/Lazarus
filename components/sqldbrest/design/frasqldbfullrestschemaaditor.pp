unit frasqldbfullrestschemaaditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, PropertyStorage, fraconnections, sqldb, sqldbrestschema, sqldbschemaedittools,
  fraSQLDBRestSchemaEditor, sqldbrestbridge;

type

  { TSchemaEditorFrame }

  TSchemaEditorFrame = class(TFrame)
    fraConn: TfraConnections;
    splConnection: TSplitter;
    fraSchema: TSQLDBRestSchemaEditorFrame;
  private
    FOnSchemaChanged: TNotifyEvent;
    procedure DoImportSchema(Sender: TObject);
    procedure DoSchemaChanged(Sender: TObject);
    function GetConnections: TSQLDBRestConnectionList;
    function GetConnectionsFileName: String;
    function GetConnectionsModified: Boolean;
    function GetModified: Boolean;
    function GetSchema: TSQLDBRestSchema;
    procedure SetConnections(AValue: TSQLDBRestConnectionList);
    procedure SetSchema(AValue: TSQLDBRestSchema);
  public
     Constructor Create(aOwner : TComponent);override;
     Procedure ClearSchema;
     Function CheckSave : Boolean;
     Procedure LoadSchema(Const aFileName : String);
     Procedure SaveSchema(Const aFileName : String);
     Procedure LoadConnections(Const aFileName : String);
     Procedure SaveConnections(Const aFileName : String);
     Procedure LoadSession(aStorage : TCustomPropertyStorage);
     Procedure SaveSession(aStorage : TCustomPropertyStorage);
     Property ConnectionsFileName : String Read GetConnectionsFileName;
     Property SchemaModified : Boolean Read GetModified;
     Property ConnectionsModified : Boolean Read GetConnectionsModified;
     Property OnSchemaChanged : TNotifyEvent Read FOnSchemaChanged Write FOnSchemaChanged;
     Property Schema : TSQLDBRestSchema Read GetSchema Write SetSchema;
     Property Connections : TSQLDBRestConnectionList Read GetConnections Write SetConnections;
  end;

implementation

uses dlgrestfieldoptions;

{$R *.lfm}

{ TSchemaEditorFrame }

procedure TSchemaEditorFrame.DoImportSchema(Sender: TObject);

Var
  RFO : TRestFieldOptions;
  Conn : TSQLConnection;

begin
  Conn:=(Sender as TMySQLDBRestConnection).MyConnection;
  RFO:=fraSchema.ImportOpts;
  if GetRestFieldOptions(RFO) then
    FraSchema.Schema.PopulateResources(Conn,Nil,RFO);
end;

procedure TSchemaEditorFrame.DoSchemaChanged(Sender: TObject);
begin
  if Assigned(FOnSchemaChanged) then
    FOnSchemaChanged(Sender);
end;

function TSchemaEditorFrame.GetConnections: TSQLDBRestConnectionList;
begin
  Result:=fraConn.Connections;
end;

function TSchemaEditorFrame.GetConnectionsFileName: String;
begin
  Result:=fraConn.FileName;
end;

function TSchemaEditorFrame.GetConnectionsModified: Boolean;
begin
  Result:=fraConn.Modified
end;

function TSchemaEditorFrame.GetModified: Boolean;
begin
  Result:=fraSchema.Modified;
end;

function TSchemaEditorFrame.GetSchema: TSQLDBRestSchema;
begin
  Result:=fraSchema.Schema;
end;

procedure TSchemaEditorFrame.SetConnections(AValue: TSQLDBRestConnectionList);
begin
  FraConn.Connections:=aValue;
end;

procedure TSchemaEditorFrame.SetSchema(AValue: TSQLDBRestSchema);
begin
  if Assigned(aValue) then
    begin
    Schema.Name:=aValue.Name;
    Schema.Resources.Assign(aValue.Resources);
    fraSchema.ShowResources;
    end;
end;


constructor TSchemaEditorFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fraConn.OnImportSchema:=@DoImportSchema;
  fraSchema.ConnectionPane:=fraConn;
  fraSchema.Connections:=fraConn.Connections;
  fraSchema.OnChanged:=@DoSchemaChanged;
end;

procedure TSchemaEditorFrame.ClearSchema;
begin
  FraSchema.ClearSchema;
end;

function TSchemaEditorFrame.CheckSave: Boolean;
begin
  Result:=FraSchema.CheckSave;
end;

procedure TSchemaEditorFrame.LoadSchema(const aFileName: String);
begin
  fraSchema.LoadFromFile(aFileName);
end;

procedure TSchemaEditorFrame.SaveSchema(const aFileName: String);
begin
  fraSchema.SaveToFile(aFileName);
end;

procedure TSchemaEditorFrame.LoadConnections(const aFileName: String);
begin
  fraConn.LoadConnections(aFileName);
end;

procedure TSchemaEditorFrame.SaveConnections(const aFileName: String);
begin
  fraConn.SaveConnections(aFileName);
end;

procedure TSchemaEditorFrame.LoadSession(aStorage: TCustomPropertyStorage);

begin
  fraConn.Width:=aStorage.ReadInteger('Connections_Width',fraConn.Width);
  fraConn.Visible:=aStorage.ReadBoolean('Connections_Visible',fraConn.Visible);
  fraConn.LoadSession(aStorage);
  fraSchema.LoadSession(aStorage);
end;

procedure TSchemaEditorFrame.SaveSession(aStorage: TCustomPropertyStorage);
begin
  aStorage.WriteInteger('Connections_Width',fraConn.Width);
  aStorage.WriteBoolean('Connections_Visible',fraConn.Visible);
  fraConn.SaveSession(aStorage);
  fraSchema.SaveSession(aStorage);
end;

end.

