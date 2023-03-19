unit frasqldbresourceparams;

{$mode objfpc}{$H+}
{}
interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, DB, sqldbrestschema, sqldbschemaedittools;

type
  { TResourceParametersEditFrame }

  TResourceParametersEditFrame = class(TBaseEditFrame)
    ILParameters: TImageList;
    LVParameters: TListView;
    procedure LVParametersDblClick(Sender: TObject);
  private
    FOnSelectParameter: TNotifyEvent;
    FResource: TSQLDBRestResource;
    procedure AddParameterToList(Param: TSQLDBRestParam);
    procedure SetResource(AValue: TSQLDBRestResource);
    procedure ShowParameter(LI: TListItem; Param: TSQLDBRestParam);
  Protected
    procedure SetFrameData(aData: TObject); override;
  public
    Procedure ShowResource;
    Function Modified : Boolean; override;
    Procedure SaveData; override;
    Function FrameCaption : String; override;
    Property Resource : TSQLDBRestResource Read FResource Write SetResource;
    Property OnSelectParameter : TNotifyEvent Read FOnSelectParameter Write FOnSelectParameter;
  end;

implementation

uses typinfo, sqldbrestconst;

{$R *.lfm}

Const
  idxParameters    = 0;
  idxParameter     = 1;

{ TResourceParametersEditFrame }

procedure TResourceParametersEditFrame.SetResource(AValue: TSQLDBRestResource);
begin
  if FResource=AValue then Exit;
  FResource:=AValue;
  ShowResource;
end;

procedure TResourceParametersEditFrame.SetFrameData(aData: TObject);
begin
  Resource:=aData as TSQLDBRestResource;
end;

procedure TResourceParametersEditFrame.ShowParameter(LI: TListItem; Param: TSQLDBRestParam);

Var
  i : Integer;
  S : String;

begin
  LI.Data:=Param;
  LI.ImageIndex:=idxParameter;
  LI.Caption:=Param.Name;
  for I:=0 to LVParameters.ColumnCount-1 do
    LI.SubItems.Add('');
  S:=GetEnumName(TypeInfo(TFieldType),Ord(Param.DataType));
  LI.SubItems[0]:=S;
  LI.SubItems[1]:=Param.DefaultValue;
end;

procedure TResourceParametersEditFrame.LVParametersDblClick(Sender: TObject);
begin
  if Assigned(OnSelectParameter) and Assigned(LVParameters.Selected) And Assigned(LVParameters.Selected.Data) then
    OnSelectParameter(TSQLDBRestResource(LVParameters.Selected.Data));
end;

procedure TResourceParametersEditFrame.AddParameterToList(Param: TSQLDBRestParam
  );

Var
  LI : TListItem;

begin
  LI:=LVParameters.Items.Add;
  ShowParameter(LI,Param);
end;

procedure TResourceParametersEditFrame.ShowResource;

Var
  I : Integer;

begin
  With LVParameters.Items do
    begin
    BeginUpdate;
    try
      Clear;
      if Not assigned(Resource) then
        exit;
      For I:=0 to Resource.Parameters.Count-1 do
        AddParameterToList(Resource.Parameters[I]);
    finally
      EndUpdate;
    end;
    end;
end;

function TResourceParametersEditFrame.Modified: Boolean;
begin
  Result:=False;
end;

procedure TResourceParametersEditFrame.SaveData;
begin
  // nothing to do
end;

function TResourceParametersEditFrame.FrameCaption: String;
begin
  if FResource=Nil then
    Result:=SUnknownObject
  else
    Result:=Resource.ResourceName;
  Result:=Format(SEditObjectParameter,[Result]);
end;

end.

