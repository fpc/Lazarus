{ Copyright (C) 2005 Alexandru Alexandrov
  Date: 11.06.2005

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
{ 2010-07-15 - New field type option (Data) Marcelo B. Paula
  2010-10-30 - Persistent Name Edit...      Marcelo B. Paula }

unit NewField;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Math, DB,
  // LCL
  Forms, Dialogs, Graphics, Controls, ExtCtrls, StdCtrls, ButtonPanel,
  // IdeIntf
  ObjInspStrConsts, ComponentEditors, PropEdits, IDEWindowIntf;

type

  { TNewFieldFrm }

  TNewFieldFrm = class(TForm)
    ButtonPanel1: TButtonPanel;
    EditCompName: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    NoteLbl: TLabel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EditName: TEdit;
    RadioGroup1: TRadioGroup;
    SelectType: TComboBox;
    EditSize: TEdit;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label10: TLabel;
    SelectKeyFields: TComboBox;
    SelectLookupKeys: TComboBox;
    SelectResultField: TComboBox;
    DataSetsCombo: TComboBox;
    procedure DataSetsComboChange(Sender: TObject);
    procedure EditCompNameChange(Sender: TObject);
    procedure EditNameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure OKBtnClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure SelectKeyFieldsChange(Sender: TObject);
    procedure SelectLookupKeysChange(Sender: TObject);
    procedure SelectResultFieldChange(Sender: TObject);
    procedure SelectTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UpdateLookupDatasets(Sender: TObject);
  private
    function GetPersistentName: string;
    procedure SetPersistentName(const AValue: string);
    function CreateFieldName(BaseName: String): String ;
  private
    LinkDataSet: TDataSet;
    FDesigner: TComponentEditorDesigner;
    AddLookupDatasetProc: TGetStrProc;
    function CreateField(fType: TFieldType; FName: string): TField;
    procedure SetButtons;
    procedure UpdateResultFields;
    procedure UpdateFieldsTypes;
    function GetLookupDataset: TDataset;
    procedure AddLookupDataset(const s:ansistring);
    property PersistentName: string read GetPersistentName write SetPersistentName;
    function SizeEnable:Boolean;
  public
    constructor Create(AOwner: TComponent; ADataset: TDataset;
      ADesigner: TComponentEditorDesigner); reintroduce;
    destructor Destroy; override;
  end ;

var
  NewFieldFrm: TNewFieldFrm;

implementation

{$R *.lfm}

procedure SplitFieldsList(FldList: string; AList: TStrings);
const
   SplitChars: Array[0..2] of Char = ('+',';',':');

  function FirstPos(AString: string): integer;
  var i,j: integer;
  begin
    Result := -1;
    for i := Low(SplitChars) to High(SplitChars) do begin
      j := Pos(SplitChars[i], AString);
      if (j <> 0) then begin
        if Result < 1 then Result := j else
          Result := Min(Result, j);
      end;
    end;
  end;
  
var i: integer;
    f,s: string;
begin
  f := FldList;
  i := FirstPos(f);
  while (i>0)do begin
    s := Copy(F, 1, i-1);
    Delete(F, 1, i);
    AList.Add(s);
    i := FirstPos(F);
  end;
  if F <> '' then AList.Add(F);
end;

{ TNewFieldFrm }

constructor TNewFieldFrm.Create(AOwner: TComponent; ADataset: TDataset;
  ADesigner: TComponentEditorDesigner);
begin
  LinkDataSet := ADataSet;
  FDesigner := ADesigner;
  inherited Create(AOwner);
  AddLookupDatasetProc := @AddLookupDataset;
  UpdateFieldsTypes;
  UpdateLookupDatasets(Self);
  RadioGroup1Click(nil);
end;

destructor TNewFieldFrm.Destroy;
begin
  inherited Destroy;
end;

procedure TNewFieldFrm.FormCreate(Sender: TObject);
var i: integer;
begin
  NoteLbl.Caption := fesNoFieldsNote;

  Caption := fesFormCaption;
  RadioGroup1.Caption := fesFieldType;
  RadioGroup1.Items.Clear;
  RadioGroup1.Items.Add(fesData);
  RadioGroup1.Items.Add(fesCalculated);
  RadioGroup1.Items.Add(fesLookup);
  GroupBox1.Caption := fesFieldProps;
  Label1.Caption := fesName;
  Label2.Caption := fesType;
  Label3.Caption := fesSize;
  GroupBox2.Caption := fesLookupDef;
  Label4.Caption := fesKeyfield;
  Label10.Caption := fesDataset;
  Label5.Caption := fesLookupKeys;
  Label6.Caption := fesResultField;
  Label7.Caption := fesPersistentCompName;
  ButtonPanel1.OKButton.Caption := fesOkBtn;
  ButtonPanel1.OKButton.OnClick:=@OKBtnClick;
  ButtonPanel1.CancelButton.Caption := fesCancelBtn;

  if Assigned(LinkDataSet) then begin
    try
      LinkDataset.FieldDefs.Update;
    except
      on E:Exception do begin
        NoteLbl.visible := true;
        //Panel1.Height := 100;
      end;
    end;
  end;
  for i := 0 to LinkDataSet.FieldDefs.Count - 1 do begin
    SelectKeyFields.Items.Add(LinkDataSet.FieldDefs[i].Name);
  end;

  if LinkDataSet.FieldDefs.Count <> 0 then
     RadioGroup1.ItemIndex := 1
  else
    RadioGroup1.ItemIndex := 0;

  RadioGroup1Click(Nil);
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TNewFieldFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

function TNewFieldFrm.CreateField(fType: TFieldType; FName: string): TField;
begin
  Result := Nil;
  if DefaultFieldClasses[fType] <> Nil then begin
    Result := DefaultFieldClasses[fType].Create(LinkDataSet.Owner);
    Result.FieldName := fName;
    Result.Name := PersistentName;
    try
      if (EditSize.Enabled) and (Trim(EditSize.Text)<> '') then
         Result.Size := StrToInt(EditSize.Text);
    except
    end;
    Result.DataSet := LinkDataSet;
  end;
end;

procedure TNewFieldFrm.DataSetsComboChange(Sender: TObject);
begin
  UpdateResultFields;
  SetButtons;
end ;

procedure TNewFieldFrm.EditCompNameChange(Sender: TObject);
begin
  SetButtons;
end;

procedure TNewFieldFrm.EditNameChange(Sender: TObject);
begin
  if Trim(EditName.Text) <> '' then
    PersistentName := CreateFieldName(LinkDataset.Name + EditName.Text)
  else
    PersistentName := '';
  SetButtons;
end ;

procedure TNewFieldFrm.UpdateLookupDatasets(Sender: TObject);
var
  sText: string;
begin
  sText := SelectLookupKeys.Text;
  DataSetsCombo.Clear;
  FDesigner.PropertyEditorHook.GetComponentNames(GetTypeData(TDataset.ClassInfo),
    AddLookupDatasetProc);
  SelectLookupKeys.Text := sText;
end;

function TNewFieldFrm.GetPersistentName: string;
begin
  Result := EditCompName.Text;
end;

procedure TNewFieldFrm.SetPersistentName(const AValue: string);
begin
  EditCompName.Text := AValue;
end;

procedure TNewFieldFrm.OKBtnClick(Sender: TObject);

  function CheckName(FldName: string): string;
  var i,j: integer;
  begin
    Result := FldName;
    i := 0;
    j := 0;
    while (i < LinkDataSet.Fields.Count) do begin
      if Result = LinkDataSet.Fields[i].FieldName then begin
        inc(j);
        Result := FldName + IntToStr(j);
      end else Inc(i);
    end;
  end;
  
  function GetFieldDef(ADataset: TDataset; Name: string): TFieldDef;
  var i: integer;
  begin
    Result := Nil;
    for i := 0 to ADataset.FieldDefs.Count - 1 do
      if AnsiCompareText(ADataset.FieldDefs[i].Name, Name) = 0 then begin
        Result := ADataset.FieldDefs[i];
        break;
      end;
  end;

var NewField: TField;
    i: integer;
    L: TStrings;
    ADataset: TDataset;
    sActive: boolean;
    fldType: TFieldType;
begin
  NewField := Nil;
  sActive := LinkDataSet.Active;
  LinkDataSet.Active := False;

  try
    case RadioGroup1.ItemIndex of
      0: begin //Create data field
        fldType := TFieldType(PtrUInt(SelectType.Items.Objects[SelectType.ItemIndex]));
        NewField := CreateField(fldType, CheckName(EditName.Text));
        if NewField<>nil then begin
          NewField.Calculated := False;
          NewField.FieldKind := fkData;

          FDesigner.PropertyEditorHook.PersistentAdded(NewField, True);
          FDesigner.Modified;
        end else
          ShowMessage(Format(fesFieldCanTBeC, [EditName.Text]));
      end;
      1: begin //Create calc field
        fldType := TFieldType(PtrUInt(SelectType.Items.Objects[SelectType.ItemIndex]));
        NewField := CreateField(fldType, CheckName(EditName.Text));
        NewField.Calculated := True;
        NewField.FieldKind := fkCalculated;

        FDesigner.PropertyEditorHook.PersistentAdded(NewField, True);
        FDesigner.Modified;
      end;
      else begin //Create lookup fields
        L := TStringList.Create;
        try
          ADataset := GetLookupDataset;
          SplitFieldsList(SelectResultField.Text, L);
          for i := 0 to L.Count - 1 do begin
            NewField := CreateField(GetFieldDef(ADataset, L[i]).DataType, CheckName(L[i]));
            if NewField <> Nil then begin
              if GetFieldDef(ADataset, L[i]).DataType = ftString then
                NewField.Size := GetFieldDef(ADataset, L[i]).Size;
              NewField.FieldKind := fkLookup;
              NewField.KeyFields := SelectKeyFields.Text;
              NewField.LookupDataSet := ADataset;
              NewField.LookupResultField := L[i];
              NewField.LookupKeyFields := SelectLookupKeys.Text;

              FDesigner.PropertyEditorHook.PersistentAdded(NewField, True);
            end else
              ShowMessage(Format(fesFieldCanTBeC, [L[i]]));
          end;
          FDesigner.Modified;
        finally
          L.Free;
        end;
      end;
    end;
  except
    if Assigned(NewField) then NewField.Free;
  end;
  if sActive then LinkDataSet.Active := True;
end;

procedure TNewFieldFrm.RadioGroup1Click(Sender: TObject);
begin
  DisableAlign;
  try
    case RadioGroup1.ItemIndex of
      0..1: begin //data,calculated field
        GroupBox2.Visible := False;
        GroupBox1.Visible := True;
        ClientHeight := RadioGroup1.Height + NoteLbl.Height + GroupBox1.Height + ButtonPanel1.Height;
      end;
      2: begin //lookup field
        GroupBox2.Visible := True;
        GroupBox1.Visible := False;
        ClientHeight := RadioGroup1.Height + NoteLbl.Height + GroupBox2.Height + ButtonPanel1.Height;
      end;
    end;
    SetButtons;
  finally
    EnableAlign;
  end;
end;

procedure TNewFieldFrm.SelectKeyFieldsChange(Sender: TObject);
begin
  UpdateResultFields;
  SetButtons;
end;

procedure TNewFieldFrm.SelectLookupKeysChange(Sender: TObject);
begin
  SetButtons;
end;

procedure TNewFieldFrm.SelectResultFieldChange(Sender: TObject);
begin
  SetButtons;
end;

procedure TNewFieldFrm.SelectTypeChange(Sender: TObject);
begin
  UpdateResultFields;
  SetButtons;
  if Trim(EditSize.Text) <> '' then
     EditSize.Text := '';
end;

procedure TNewFieldFrm.SetButtons;
begin
  if SizeEnable then
    begin
      EditSize.Enabled := True;
      EditSize.Color   := clWindow;
    end
  else
    begin
      EditSize.Enabled := False;
      EditSize.Color   := clBtnFace;
    end;
  //
  case RadioGroup1.ItemIndex of
    0..1: ButtonPanel1.OKButton.Enabled := (Length(EditName.Text) > 0) And
                           (Length(PersistentName) > 0) And
                           (SelectType.ItemIndex > -1);
    2: ButtonPanel1.OKButton.Enabled := (SelectKeyFields.Text <> '') And
                          (DataSetsCombo.ItemIndex > -1) And
                          (SelectLookupKeys.Text <> '') And
                          (SelectResultField.Text <> '');
  end;
end;

procedure TNewFieldFrm.UpdateResultFields;
var i: integer;
    ADataset: TDataset;
begin
  SelectResultField.Clear;
  SelectLookUpKeys.Clear;
  if (DataSetsCombo.ItemIndex > -1) then begin
    ADataset := GetLookupDataset;
    if Assigned(ADataset) then begin
      try
        ADataset.FieldDefs.Update;
        for i := 0 to ADataset.FieldDefs.Count - 1 do begin
          SelectResultField.Items.Add(ADataset.FieldDefs[i].Name);
          SelectLookUpKeys.Items.Add(ADataset.FieldDefs[i].Name);
        end;
      except
        on E:Exception do begin
          ShowMessage(fesNoFields+^M+fesCheckDSet+^M^M+E.Message);
        end;
      end;
    end;
  end;
  SelectLookUpKeys.Enabled := SelectLookUpKeys.Items.Count > 0;
  SelectResultField.Enabled := SelectResultField.Items.Count > 0;
end;


procedure TNewFieldFrm.UpdateFieldsTypes;
var i: TFieldType;
begin
  SelectType.Clear;
  SelectType.Sorted := False;
  for i := Low(Fieldtypenames) to High(Fieldtypenames) do
    SelectType.Items.AddObject(Fieldtypenames[i], Tobject(PtrUInt(i)));
  SelectType.Sorted := True;
end;


function TNewFieldFrm.GetLookupDataset: TDataset;
begin
  if GlobalDesignHook=Nil then
    Result := Nil
  else begin
    Result := GlobalDesignHook.GetComponent( DataSetsCombo.Items[DataSetsCombo.ItemIndex] ) as TDataset;
    if Not Result.InheritsFrom(TDataset) then Result := Nil;
  end;
end;

procedure TNewFieldFrm.AddLookupDataset(const s: ansistring);
begin
  if (AnsiCompareText(s, LinkDataSet.Name) <> 0) then
    DataSetsCombo.Items.Add(s);
end;

function TNewFieldFrm.SizeEnable: Boolean;
begin
  if SelectType.ItemIndex >= 0 then
    case TFieldType(PtrUInt( SelectType.Items.Objects[SelectType.ItemIndex])) of
      ftADT:        Result := True;
      ftArray:      Result := True;
      ftBCD:        Result := True;
      ftBlob:       Result := True;
      ftBytes:      Result := True;
      ftDataSet:    Result := True;
      ftFMTBcd:     Result := True;
      ftGraphic:    Result := True;
      ftMemo:       Result := True;
      ftString:     Result := True;
      ftWideString: Result := True;
      ftVarBytes:   Result := True;
      ftVariant:    Result := True;
    else
      Result := False
    end
  else
    Result := False;
end;

function TNewFieldFrm.CreateFieldName(BaseName: String): String ;
var
  i: integer;
  ExistingComponent, OwnerComponent: TComponent;
begin
  Result:=BaseName;
  OwnerComponent := FDesigner.LookupRoot;
  if (OwnerComponent=nil) or (Result='') then exit;
  i:=1;
  repeat
    ExistingComponent := OwnerComponent.FindComponent(Result);
    if ExistingComponent<>nil then
    begin
       if (BaseName[Length(BaseName)] in ['0'..'9']) then
         Result := BaseName+'_'+IntToStr(i)
       else
         Result := BaseName+IntToStr(i);
       inc(i);
    end ;
  until ExistingComponent=nil;
end;


end.
