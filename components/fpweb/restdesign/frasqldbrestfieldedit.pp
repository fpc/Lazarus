unit frasqldbrestfieldedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Spin, ExtCtrls, db,  sqldbschemaedittools, sqldbrestschema;

type

  { TSQLDBRestFieldEditFrame }

  TSQLDBRestFieldEditFrame = class(TBaseEditFrame)
    CBFieldType: TComboBox;
    CBNativeFieldType: TComboBox;
    CGFieldOptions: TCheckGroup;
    LPublicName1: TLabel;
    LPublicName2: TLabel;
    LPublicName3: TLabel;
    LPublicName4: TLabel;
    LSEMaxLen: TLabel;
    CGFilters: TCheckGroup;
    EGeneratorName: TEdit;
    EPublicName: TEdit;
    EFieldName: TEdit;
    LPublicName: TLabel;
    SEMaxLen: TSpinEdit;
    procedure CGFieldOptionsItemClick(Sender: TObject; Index: integer);
  private
    FField: TSQLDBRestField;
    procedure CheckFiltersEnabled;
    procedure SetField(AValue: TSQLDBRestField);
  Protected
    Procedure SetFrameData(aData: TObject); override;
  public
    Constructor Create(aOwner : TComponent); override;
    Function Modified : Boolean; override;
    Procedure SaveData; override;
    Procedure ShowField;
    Function FrameCaption : String; override;
    Property Field : TSQLDBRestField Read FField Write SetField;
  end;


implementation

uses typinfo;

{$R *.lfm}

{ TSQLDBRestFieldEditFrame }

procedure TSQLDBRestFieldEditFrame.CGFieldOptionsItemClick(Sender: TObject;
  Index: integer);
begin
  if Index=ord(foFilter) then
    CheckFiltersEnabled;
end;

procedure TSQLDBRestFieldEditFrame.CheckFiltersEnabled;

begin
  CGFilters.Enabled:=CGFieldOptions.Checked[ord(foFilter)];
end;

procedure TSQLDBRestFieldEditFrame.SetField(AValue: TSQLDBRestField);
begin
  if FField=AValue then Exit;
  FField:=AValue;
  ShowField;
end;

procedure TSQLDBRestFieldEditFrame.SetFrameData(aData: TObject);
begin
  Field:=aData as TSQLDBRestField;
end;

constructor TSQLDBRestFieldEditFrame.Create(aOwner: TComponent);
Var
  T : TRestFieldType;
  FT : TFieldType;

begin
  inherited Create(aOwner);
  CBFieldType.Items.Clear;
  For T in TRestFieldType do
    if T<>rftUnknown then
      CBFieldType.Items.Add(TypeNames[T]);
  CBNativeFieldType.Items.Clear;
  For FT in TFieldType do
    if FT<>ftUnknown then
      CBNativeFieldType.Items.Add(GetEnumName(TypeInfo(TFieldType),Ord(FT)))
end;

function TSQLDBRestFieldEditFrame.Modified: Boolean;

  Procedure DoOption(O : TRestFieldOption);

  begin
    Result:=Result or (CGFieldOptions.Checked[Ord(O)]<> (O in Field.Options))
  end;

  Procedure DoFilter(F : TRestFieldFilter);

  begin
    Result:=Result or (CGFilters.Checked[Ord(F)]<> (F in Field.Filters));
  end;

Var
  O : TRestFieldOption;
  FO : TRestFieldFilter;

begin
  With FField do
    begin
    Result:=(PublicName <> EPublicName.Text) or
            (FieldName <> EFieldName.Text) or
            (GeneratorName <> EGeneratorName.Text) or
            (FieldType <> TRestFieldType(CBFieldType.ItemIndex+1)) or
            (NativeFieldType <> TFieldType(CBNativeFieldType.ItemIndex+1)) or
            (MaxLen<>SEMaxLen.Value);
    For O in TRestFieldOption do
      DoOption(O);
    For FO in TRestFieldFilter do
      DoFilter(FO);
    end;
end;

procedure TSQLDBRestFieldEditFrame.SaveData;

  Procedure DoOption(O : TRestFieldOption);

  begin
    if CGFieldOptions.Checked[Ord(O)] then
      Field.Options:=Field.Options+[O]
    else
      Field.Options:=Field.Options-[O]
  end;

  Procedure DoFilter(F : TRestFieldFilter);

  begin
    if CGFilters.Checked[Ord(F)] and (foFilter in Field.Options) then
      Field.Filters:=Field.Filters+[F]
    else
      Field.Filters:=Field.Filters-[F];
  end;

Var
  O : TRestFieldOption;
  FO : TRestFieldFilter;


begin
  With FField do
    begin
    PublicName    := EPublicName.Text;
    FieldName     := EFieldName.Text;
    GeneratorName := EGeneratorName.Text;
    FieldType := TRestFieldType(CBFieldType.ItemIndex+1);
    NativeFieldType := TFieldType(CBNativeFieldType.ItemIndex+1);
    MaxLen:=SEMaxLen.Value;
    For O in TRestFieldOption do
      DoOption(O);
    For FO in TRestFieldFilter do
      DoFilter(FO);
    end;
end;

procedure TSQLDBRestFieldEditFrame.ShowField;

  Procedure DoOption(O : TRestFieldOption);

  begin
    CGFieldOptions.Checked[Ord(O)]:=O in Field.Options;
  end;

  Procedure DoFilter(F : TRestFieldFilter);

  begin
    CGFilters.Checked[Ord(F)]:=(foFilter in Field.Options) and (F in Field.Filters);
  end;

Var
  O : TRestFieldOption;
  FO : TRestFieldFilter;


begin
  With FField do
    begin
    EPublicName.Text:=PublicName;
    EFieldName.Text:=FieldName;
    EGeneratorName.Text:=GeneratorName;
    CBFieldType.ItemIndex:=Ord(FieldType)-1;
    CBNativeFieldType.ItemIndex:=Ord(NativeFieldType)-1;
    SEMaxLen.Value:=MaxLen;
    For O in TRestFieldOption do
      DoOption(O);
    CheckFiltersEnabled;
    For FO in TRestFieldFilter do
      DoFilter(FO);
    end;
end;

function TSQLDBRestFieldEditFrame.FrameCaption: String;
begin
  if FField=Nil then
    Result:=SUnknownObject
  else
    Result:=FField.PublicName;
  Result:=Format(SEditObject,[SField,Result]);
end;

end.

