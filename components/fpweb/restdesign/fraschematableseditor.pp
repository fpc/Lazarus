unit fraschematableseditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, sqldbrestschema, sqldbschemaedittools;

type

  { TSQLDBRestSchemaTablesEditFrame }

  TSQLDBRestSchemaTablesEditFrame = class(TBaseEditFrame)
    ILSchema: TImageList;
    LVSchema: TListView;
    procedure LVSchemaDblClick(Sender: TObject);
  private
    FOnSelectResource: TNotifyEvent;
    FSchema: TSQLDBRestSchema;
    procedure AddResourceToList(Res: TSQLDBRestResource);
    procedure SetSchema(AValue: TSQLDBRestSchema);
    procedure ShowResource(LI: TListItem; Res: TSQLDBRestResource);
  Protected
    procedure SetFrameData(aData: TObject); override;
  public
    Procedure ShowSchema;
    Function Modified : Boolean; override;
    Procedure SaveData; override;
    Function FrameCaption : String; override;
    Property Schema : TSQLDBRestSchema Read FSchema Write SetSchema;
    Property OnSelectResource : TNotifyEvent Read FOnSelectResource Write FOnSelectResource;
  end;

implementation

uses dialogs;

Const
  idxChecked   = 0;
  idxUnChecked = 1;
  idxTable     = 2;

{$R *.lfm}

{ TSQLDBRestSchemaTablesEditFrame }

procedure TSQLDBRestSchemaTablesEditFrame.SetSchema(AValue: TSQLDBRestSchema);
begin
  if FSchema=AValue then Exit;
  FSchema:=AValue;
  ShowSchema;
end;

procedure TSQLDBRestSchemaTablesEditFrame.ShowResource(LI: TListItem;
  Res: TSQLDBRestResource);

  procedure ShowBool(Idx : Integer; B : Boolean);
  begin
    LI.SubItems[Idx]:='';
    if B then
      LI.SubItemImages[Idx]:=idxChecked;
  end;

  Procedure ShowOp(idx : Integer; O : TRestOperation);

  begin
    ShowBool(Idx,O in Res.AllowedOperations);
  end;

Var
  i : Integer;

begin
//  LI.StateIndex:=idxTable;
  LI.Data:=Res;
  LI.ImageIndex:=idxTable;
  LI.Caption:=Res.ResourceName;
  for I:=0 to LVSchema.ColumnCount-1 do
    LI.SubItems.Add('');
  LI.SubItems[0]:=Res.TableName;
  LI.SubItems[1]:=Res.ConnectionName;
  ShowBool(2,Res.Enabled);
  ShowBool(3,Res.InMetadata);
  ShowOp(4,roGet);
  ShowOp(5,roPost);
  ShowOp(6,roPut);
  ShowOp(7,roDelete);
  ShowOp(8,roOptions);
  ShowOp(9,roHead);

end;

procedure TSQLDBRestSchemaTablesEditFrame.SetFrameData(aData: TObject);
begin
  Schema:=aData as TSQLDBRestSchema;
end;

procedure TSQLDBRestSchemaTablesEditFrame.LVSchemaDblClick(Sender: TObject);
begin
  if Assigned(OnSelectResource) and Assigned(LVSchema.Selected) And Assigned(LVSchema.Selected.Data) then
    OnSelectResource(TSQLDBRestResource(LVSchema.Selected.Data));
end;

procedure TSQLDBRestSchemaTablesEditFrame.AddResourceToList(
  Res: TSQLDBRestResource);

Var
  LI : TListItem;

begin
  LI:=LVSchema.Items.Add;
  ShowResource(LI,Res);
end;

procedure TSQLDBRestSchemaTablesEditFrame.ShowSchema;

Var
  I : Integer;

begin
  With LVSchema.Items do
    begin
    BeginUpdate;
    try
      Clear;
      if Not assigned(Schema) then
        exit;
      For I:=0 to Schema.Resources.Count-1 do
        AddResourceToList(Schema.Resources[I]);
    finally
      EndUpdate;
    end;
    end;
end;

function TSQLDBRestSchemaTablesEditFrame.Modified: Boolean;
begin
  Result:=False;
end;

procedure TSQLDBRestSchemaTablesEditFrame.SaveData;
begin
  // nothing to do
end;

function TSQLDBRestSchemaTablesEditFrame.FrameCaption: String;
begin
  if FSchema=Nil then
    Result:=SUnknownObject
  else
    Result:=Schema.Name;
  Result:=Format(SEditObject,[SSchema,Result]);
end;

end.

