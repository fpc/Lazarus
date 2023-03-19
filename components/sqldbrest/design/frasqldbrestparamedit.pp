unit frasqldbrestparamedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Spin, ExtCtrls, db,  sqldbschemaedittools, sqldbrestschema;

type

  { TSQLDBRestParameterEditFrame }

  TSQLDBRestParameterEditFrame = class(TBaseEditFrame)
    CBDataType: TComboBox;
    LDefaultValue: TLabel;
    lDatatype: TLabel;
    edtDefaultValue: TEdit;
    EName: TEdit;
    LName: TLabel;
  private
    FParam: TSQLDBRestParam;
    procedure SetParam(AValue: TSQLDBRestParam);
  Protected
    Procedure SetFrameData(aData: TObject); override;
  public
    Constructor Create(aOwner : TComponent); override;
    Function Modified : Boolean; override;
    Procedure SaveData; override;
    Procedure ShowParam;
    Function FrameCaption : String; override;
    Property Param : TSQLDBRestParam Read FParam Write SetParam;
  end;


implementation

uses typinfo;

{$R *.lfm}

{ TSQLDBRestParameterEditFrame }


procedure TSQLDBRestParameterEditFrame.SetParam(AValue: TSQLDBRestParam);
begin
  if FParam=AValue then Exit;
  FParam:=AValue;
  ShowParam;
end;

procedure TSQLDBRestParameterEditFrame.SetFrameData(aData: TObject);
begin
  Param:=aData as TSQLDBRestParam;
end;

constructor TSQLDBRestParameterEditFrame.Create(aOwner: TComponent);

Var
  FT : TFieldType;

begin
  inherited Create(aOwner);
  CBDataType.Items.Clear;
  For FT in TFieldType do
    if FT<>ftUnknown then
      CBDataType.Items.Add(GetEnumName(TypeInfo(TFieldType),Ord(FT)))
end;

function TSQLDBRestParameterEditFrame.Modified: Boolean;

begin
  With FParam do
    begin
    Result:=(Name<>EName.Text) or
            (DefaultValue<>edtDefaultValue.Text) or
            (Ord(DataType)<>(CBDataType.ItemIndex+1));
    end;
end;

procedure TSQLDBRestParameterEditFrame.SaveData;


begin
  With FParam do
    begin
    Name:= EName.Text;
    DefaultValue := edtDefaultValue.Text;
    DataType := TFieldType(CBDataType.ItemIndex+1);
    end;
end;

procedure TSQLDBRestParameterEditFrame.ShowParam;

begin
  With FParam do
    begin
    EName.Text:=Name;
    edtDefaultValue.Text:=DefaultValue;
    CBDataType.ItemIndex:=Ord(DataType)-1;
    end;
end;

function TSQLDBRestParameterEditFrame.FrameCaption: String;
begin
  if FParam=Nil then
    Result:=SUnknownObject
  else
    Result:=FParam.Name;
  Result:=Format(SEditObject,[SParameter,Result]);
  If Assigned(Resource) then
    Result:=SResource+' '+Resource.ResourceName+' - '+Result;
end;

end.

