{  $Id$  }
{ Copyright (C) 2005 Alexandru Alexandrov
  Date: 11.06.2005

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
{ 2010-07-15 - New field type option (Data) Marcelo B. Paula
  2010-10-30 - Persistent Name Edit...      Marcelo B. Paula }

unit newfield;

{$mode Delphi} {$H+}

interface

uses
  Classes, Math, SysUtils, DBConst, LCLIntf, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, DB, ObjInspStrConsts,
  ComponentEditors, PropEdits, PropEditUtils, TypInfo;

type

  { TNewFieldFrm }

  TNewFieldFrm=class(TForm)
    CancelBtn: TBitBtn;
    EditCompName: TEdit;
    Label7: TLabel;
    NoteLbl: TLabel;
    OKBtn: TBitBtn;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EditName: TEdit;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    SelectType: TComboBox;
    EditSize: TEdit;
    Panel3: TPanel;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label10: TLabel;
    SelectKeyFields: TComboBox;
    SelectLookupKeys: TComboBox;
    SelectResultField: TComboBox;
    DataSetsCombo: TComboBox;
    Panel4: TPanel;
    procedure DataSetsComboChange(Sender: TObject);
    procedure EditCompNameChange(Sender: TObject);
    procedure EditNameChange(Sender: TObject);
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
  private
    { Private declarations }
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
    { Public declarations }
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

constructor TNewFieldFrm.Create(AOwner: TComponent; ADataset: TDataset;
      ADesigner: TComponentEditorDesigner);
begin
  LinkDataSet := ADataSet;
  FDesigner := ADesigner;
  inherited Create(AOwner);
  AddLookupDatasetProc := AddLookupDataset;
  UpdateFieldsTypes;
  UpdateLookupDatasets(Self);
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
     PersistentName := FDesigner.CreateUniqueComponentName(LinkDataset.Name + EditName.Text)
  else
    PersistentName := '';

  SetButtons;
end ;

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
  OkBtn.Caption := fesOkBtn;
  CancelBtn.Caption := fesCancelBtn;

  if Assigned(LinkDataSet) then begin
    try
      LinkDataset.FieldDefs.Update;
    except
      on E:Exception do begin
        NoteLbl.visible := true;
        Panel1.Height := 100;
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
end;

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
        NewField.Calculated := False;
        NewField.FieldKind := fkData;

        FDesigner.PropertyEditorHook.PersistentAdded(NewField, True);
        FDesigner.Modified;
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
  case RadioGroup1.ItemIndex of
    0..1: begin //data,calculated field
      Panel3.Visible := False;
      Panel2.Visible := True;
      ClientHeight := Panel1.Height + Panel2.Height + Panel4.Height;
    end;
    2: begin //lookup field
      Panel3.Visible := True;
      Panel2.Visible := False;
      ClientHeight := Panel1.Height + Panel3.Height + Panel4.Height;
    end;
  end;
  SetButtons;
end ;

procedure TNewFieldFrm.SelectKeyFieldsChange(Sender: TObject);
begin
  UpdateResultFields;
  SetButtons;
end;

procedure TNewFieldFrm.SelectLookupKeysChange(Sender: TObject);
begin
  SetButtons;
end ;

procedure TNewFieldFrm.SelectResultFieldChange(Sender: TObject);
begin
  SetButtons;
end ;

procedure TNewFieldFrm.SelectTypeChange(Sender: TObject);
begin
  UpdateResultFields;
  SetButtons;
  if Trim(EditSize.Text) <> '' then
     EditSize.Text := '';
end ;

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
    0..1: OkBtn.Enabled := (Length(EditName.Text) > 0) And
                           (Length(PersistentName) > 0) And
                           (SelectType.ItemIndex > -1);
    2: OkBtn.Enabled := (SelectKeyFields.Text <> '') And
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
      ADataset.FieldDefs.Update;
      for i := 0 to ADataset.FieldDefs.Count - 1 do begin
        SelectResultField.Items.Add(ADataset.FieldDefs[i].Name);
        SelectLookUpKeys.Items.Add(ADataset.FieldDefs[i].Name);
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
  for i := Low(Fieldtypenames) to High(Fieldtypenames) do begin
    SelectType.Items.AddObject(Fieldtypenames[i], Tobject(PtrUInt(i)));
  end;
  SelectType.Sorted := True;
end;


function TNewFieldFrm.GetLookupDataset: TDataset;
begin
  Result := GlobalDesignHook.GetComponent( DataSetsCombo.Items[DataSetsCombo.ItemIndex] ) as TDataset;
  if Not Result.InheritsFrom(TDataset) then Result := Nil;
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

destructor TNewFieldFrm.Destroy;
begin
  inherited Destroy;
end;

end.
