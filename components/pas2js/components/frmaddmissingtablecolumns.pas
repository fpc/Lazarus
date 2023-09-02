unit frmaddmissingtablecolumns;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls, ValEdit, Grids, StdCtrls,
  db, types, stub.bootstraptablewidget;

type
  TFieldProps = record
    FieldType : TFieldType;
    DisplayName : string;
    Size : integer;
  end;

  TColumnData = record
    FieldName : string;
    Title : string;
    Width : integer;
    Render : TColumnRenderMode
  end;
  TColumnDataArray = Array of TColumnData;

  { TAddMissingTableColumsForm }

  TAddMissingTableColumsForm = class(TForm)
    BPFields: TButtonPanel;
    cbAddAction: TCheckBox;
    cbActionField: TComboBox;
    cbSelectField: TComboBox;
    cbAddSelect: TCheckBox;
    pnlTop: TPanel;
    pnlAction: TPanel;
    sgFields: TStringGrid;
  private
    FTable: TDBBootstrapTableWidget;
    function GetActionColumn: String;
    function GetNewColumns: TColumnDataArray;
    function GetSelectColumn: String;
    procedure MakeRow(aRow: Integer; const aFld: String);
    procedure SetTable(AValue: TDBBootstrapTableWidget);
  public
    Procedure AnalyzeColumns;
    Procedure FillFieldsCombobox;
    procedure FillTypesList;
    class function GetFieldProps(aTable: TDBBootstrapTableWidget; const aFieldName: String; out aProps: TFieldProps): Boolean;
    class function GetFieldNames(aTable : TDBBootstrapTableWidget): TStringDynArray;
    class function GetMissingFields(aTable : TDBBootstrapTableWidget; aFields : TStringDynArray): TStringDynArray;
    class function GetKeyField(aTable: TDBBootstrapTableWidget): String;
    Property Table : TDBBootstrapTableWidget Read FTable Write SetTable;
    Property NewColumns : TColumnDataArray Read GetNewColumns;
    Property ActionColumn : String Read GetActionColumn;
    Property SelectColumn : String Read GetSelectColumn;
  end;

var
  AddMissingTableColumsForm: TAddMissingTableColumsForm;

implementation


uses math,typinfo;

const
  idxField = 0;
  idxAdd   = 1;
  idxType  = 2;
  idxWidth = 3;
  idxTitle = 4;


function RenderModeToString(R : TColumnRenderMode) : String;

begin
  Result:=GetEnumName(TypeInfo(TColumnRenderMode),Ord(R));
end;

function StringToRenderMode(const R : String) : TColumnRenderMode;

var
  I : integer;

begin
  I:=GetEnumValue(TypeInfo(TColumnRenderMode),R);
  if I=-1 then
    Result:=crmText
  else
    Result:=TColumnRenderMode(I);
end;


{$R *.lfm}

{ TAddMissingTableColumsForm }

procedure TAddMissingTableColumsForm.SetTable(AValue: TDBBootstrapTableWidget);
begin
  if FTable=AValue then Exit;
  FTable:=AValue;
  FillFieldsCombobox;
  FillTypesList;
  AnalyzeColumns;
end;

class function TAddMissingTableColumsForm.GetFieldNames(aTable: TDBBootstrapTableWidget): TStringDynArray;
var
  DS : TDataset;
  I : Integer;
  FD : TFieldDef;
  F : TField;

begin
  Result:=[];
  Ds:=Nil;
  if assigned(aTable.Datasource)  then
    ds:=aTable.Datasource.Dataset;
  if ds=nil then
    exit;
  if (ds.Fields.Count=0) then
    begin
    SetLength(Result,ds.FieldDefs.Count);
    for I:=0 to ds.FieldDefs.Count-1 do
      begin
      FD:=DS.FieldDefs[I];
      Result[i]:=FD.Name;
      end;
    end
  else
    begin
    SetLength(Result,ds.Fields.Count);
    for I:=0 to ds.Fields.Count-1 do
      begin
      F:=DS.Fields[I];
      Result[i]:=F.FieldName;
      end;
    end;
end;

class function TAddMissingTableColumsForm.GetMissingFields(aTable: TDBBootstrapTableWidget; aFields: TStringDynArray
  ): TStringDynArray;

var
  Fld: String;
  aCount : Integer;

begin
  Result:=[];
  SetLength(Result,Length(aFields));
  aCount:=0;
  For Fld in aFields do
    if aTable.Columns.IndexOfColumn(Fld)=-1 then
      begin
      Result[aCount]:=Fld;
      Inc(aCount);
      end;
  SetLength(Result,aCount);
end;

class function TAddMissingTableColumsForm.GetKeyField(aTable: TDBBootstrapTableWidget): String;

var
  DS : TDataset;
  I : Integer;
  FD : TFieldDef;
  F : TField;

begin
  Result:='';
  if assigned(aTable.Datasource)  then
    ds:=aTable.Datasource.Dataset;
  if ds=nil then
    exit;
  I:=0;
  While (Result='') and (I<ds.Fields.Count) do
    begin
    F:=DS.Fields[I];
    if (pfInKey in F.ProviderFlags) then
      Result:=F.FieldName;
    inc(I);
    end;
  I:=0;
  While (Result='') and (I<ds.FieldDefs.Count) do
    begin
    FD:=DS.FieldDefs[I];
    if FD.DataType=ftAutoInc then
      Result:=FD.Name;
    inc(I);
    end;
end;



procedure TAddMissingTableColumsForm.MakeRow(aRow : Integer; const aFld : String);

var
  Props : TFieldProps;
  R : TColumnRenderMode;

begin
  if not GetFieldProps(Table,aFld,Props) then
    exit;
  SGFields.Cells[idxField,aRow]:=aFld;
  SGFields.Cells[idxAdd,aRow]:='1';
  R:=crmText;
  // TColumnRenderMode = (crmText, crmNumeric, crmDateTime, crmTransformedValue, crmCheckBox, crmButton, crmCustom);
  case Props.fieldtype of
    ftBoolean : R:=crmCheckBox;
    ftDate, ftDateTime,ftTime,ftTimeStamp : R:=crmDateTime;
    ftWord, ftInteger, ftAutoInc, ftLargeint: R:=crmNumeric;
    ftBlob,
    ftBytes,
    ftOraBlob,
    ftOraClob: R:=crmTransformedValue;
  end;
  SGFields.Cells[idxType,aRow]:=RenderModeToString(R);
  SGFields.Cells[idxWidth,aRow]:=IntToStr(Max(40,Min(Props.Size*8,200)));
  SGFields.Cells[idxTitle,aRow]:=Props.displayname;
end;

function TAddMissingTableColumsForm.GetActionColumn: String;
begin
  Result:='';
  if cbAddAction.Checked then
    Result:=cbActionField.Text;
end;

function TAddMissingTableColumsForm.GetNewColumns: TColumnDataArray;

var
  I,Count : Integer;

begin
  Count:=0;
  For I:=1 to SGFields.RowCount-1 do
    if SGFields.Cells[idxAdd,I]='1' then
      Inc(Count);
  SetLength(Result,Count);
  Count:=0;
  For I:=1 to SGFields.RowCount-1 do
    if SGFields.Cells[idxAdd,I]='1' then
      begin
      Result[Count].FieldName:=SGFields.Cells[idxField,I];
      Result[Count].Width:=StrToIntDef(SGFields.Cells[idxWidth,I],120);
      Result[Count].Render:=StringToRenderMode(SGFields.Cells[idxType,I]);
      Result[Count].Title:=SGFields.Cells[idxTitle,I];
      inc(Count);
      end;
end;

function TAddMissingTableColumsForm.GetSelectColumn: String;
begin
  Result:='';
  if cbAddSelect.Checked then
    Result:=cbSelectField.Text;
end;

procedure TAddMissingTableColumsForm.AnalyzeColumns;

var
  Fields : TStringDynArray;
  MissingFields : TStringDynArray;
  aRow : Integer;
  Fld : String;

begin
  Fields:=GetFieldNames(Table);
  MissingFields:=GetMissingFields(Table,Fields);
  sgFields.RowCount:=Length(MissingFields)+1;
  aRow:=0;
  For Fld in MissingFields do
    begin
    Inc(aRow);
    MakeRow(aRow,Fld);
    end;
end;

procedure TAddMissingTableColumsForm.FillFieldsCombobox;

var
  KeyField : String;

  Procedure DoCB(Cb : TCombobox);
  begin
    CB.Items.AddStrings(GetFieldNames(Table),True);
    if (KeyField<>'') then
      CB.ItemIndex:=CB.Items.IndexOf(KeyField);
  end;

begin
  KeyField:=GetKeyField(Table);
  DoCB(cbActionField);
  DoCB(cbSelectField);
end;

procedure TAddMissingTableColumsForm.FillTypesList;

var
  R : TColumnRenderMode;

begin
  With sgFields.Columns[2].PickList do
    begin
    Clear;
    for r in TColumnRenderMode do
      Add(RenderModeToString(R));
    end;
end;

class function TAddMissingTableColumsForm.GetFieldProps(aTable: TDBBootstrapTableWidget; const aFieldName : String; out aProps: TFieldProps): Boolean;
var
  DS : TDataset;
  I : Integer;
  FD : TFieldDef;
  F : TField;

begin
  Result:=False;
  if assigned(aTable.Datasource)  then
    ds:=aTable.Datasource.Dataset;
  if ds=nil then
    exit;
  if (ds.Fields.Count=0) then
    begin
    FD:=ds.FieldDefs.Find(aFieldName);
    Result:=Assigned(FD);
    if Result then
      begin
      aProps.DisplayName:=aFieldName;
      aProps.FieldType:=FD.DataType;
      aProps.size:=FD.Size;
      end;
    end
  else
    begin
    F:=ds.Fields.FindField(aFieldName);
    Result:=Assigned(F);
    if Result then
      begin
      aProps.DisplayName:=F.DisplayLabel;
      aProps.FieldType:=F.DataType;
      aProps.Size:=FD.Size;
      end;
    end;
end;

end.

