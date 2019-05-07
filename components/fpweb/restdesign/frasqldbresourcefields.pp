unit frasqldbresourcefields;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, DB, sqldbrestschema, sqldbschemaedittools;

type
  { TResourceFieldsEditFrame }

  TResourceFieldsEditFrame = class(TBaseEditFrame)
    ILFields: TImageList;
    LVFields: TListView;
    procedure LVFieldsDblClick(Sender: TObject);
  private
    FOnSelectField: TNotifyEvent;
    FResource: TSQLDBRestResource;
    procedure AddFieldToList(Fld: TSQLDBRestField);
    procedure SetResource(AValue: TSQLDBRestResource);
    procedure ShowField(LI: TListItem; Fld: TSQLDBRestField);
  Protected
    procedure SetFrameData(aData: TObject); override;
  public
    Procedure ShowResource;
    Function Modified : Boolean; override;
    Procedure SaveData; override;
    Function FrameCaption : String; override;
    Property Resource : TSQLDBRestResource Read FResource Write SetResource;
    Property OnSelectField : TNotifyEvent Read FOnSelectField Write FOnSelectField;
  end;

implementation

uses typinfo;

{$R *.lfm}
Const
  idxChecked   = 0;
  idxUnChecked = 1;
  idxField     = 2;
  idxKey       = 3;

{ TResourceFieldsEditFrame }

procedure TResourceFieldsEditFrame.SetResource(AValue: TSQLDBRestResource);
begin
  if FResource=AValue then Exit;
  FResource:=AValue;
  ShowResource;
end;

procedure TResourceFieldsEditFrame.SetFrameData(aData: TObject);
begin
  Resource:=aData as TSQLDBRestResource;
end;

procedure TResourceFieldsEditFrame.ShowField(LI: TListItem; Fld: TSQLDBRestField);

  procedure ShowBool(Idx : Integer; B : Boolean; ImgIdx : Integer = idxChecked);


  begin
    LI.SubItems[Idx]:='';
    if B then
      LI.SubItemImages[Idx]:=ImgIdx;
  end;

  Procedure ShowOp(idx : Integer; O : TRestFieldOption);

  begin
    ShowBool(Idx,O in fld.Options);
  end;

Var
  i : Integer;
  S : String;

begin
  LI.Data:=Fld;
  LI.ImageIndex:=idxField;
  LI.Caption:=Fld.PublicName;
  for I:=0 to LVFields.ColumnCount-1 do
    LI.SubItems.Add('');
  LI.SubItems[0]:=Fld.FieldName;
  LI.SubItems[1]:=TypeNames[Fld.FieldType];
  S:=GetEnumName(TypeInfo(TFieldType),Ord(Fld.NativeFieldType));
  LI.SubItems[2]:=S;
  LI.SubItems[3]:=Fld.GeneratorName;
  ShowBool(4,foInKey in Fld.Options,idxKey);
  ShowOp(5,foInInsert);
  ShowOp(6,foInUpdate);
  ShowOp(7,foRequired);
  ShowOp(8,foFilter);
  ShowOp(9,foOrderBy);
  ShowOp(10,foOrderByDesc);
end;

procedure TResourceFieldsEditFrame.LVFieldsDblClick(Sender: TObject);
begin
  if Assigned(OnSelectField) and Assigned(LVFields.Selected) And Assigned(LVFields.Selected.Data) then
    OnSelectField(TSQLDBRestResource(LVFields.Selected.Data));
end;

procedure TResourceFieldsEditFrame.AddFieldToList(Fld: TSQLDBRestField);

Var
  LI : TListItem;

begin
  LI:=LVFields.Items.Add;
  ShowField(LI,Fld);
end;

procedure TResourceFieldsEditFrame.ShowResource;

Var
  I : Integer;

begin
  With LVFields.Items do
    begin
    BeginUpdate;
    try
      Clear;
      if Not assigned(Resource) then
        exit;
      For I:=0 to Resource.Fields.Count-1 do
        AddFieldToList(Resource.Fields[I]);
    finally
      EndUpdate;
    end;
    end;
end;

function TResourceFieldsEditFrame.Modified: Boolean;
begin
  Result:=False;
end;

procedure TResourceFieldsEditFrame.SaveData;
begin
  // nothing to do
end;

function TResourceFieldsEditFrame.FrameCaption: String;
begin
  if FResource=Nil then
    Result:=SUnknownObject
  else
    Result:=Resource.ResourceName;
  Result:=Format(SEditObjectFields,[Result]);
end;

end.

