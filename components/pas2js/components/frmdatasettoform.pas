unit frmdatasettoform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, ButtonPanel, EditBtn, SynEdit,
  SynHighlighterHTML, datasettoform, RTTIGrids;

type

  { TfrmDatasetToHTMLForm }

  TfrmDatasetToHTMLForm = class(TForm)
    bpData: TButtonPanel;
    btnSave: TButton;
    cbformat: TComboBox;
    feHTML: TFileNameEdit;
    Label1: TLabel;
    LFEHTML: TLabel;
    LBFields: TListBox;
    pnlSave: TPanel;
    PCHTML: TPageControl;
    pnlTop: TPanel;
    SEHTML: TSynEdit;
    shHTML: TSynHTMLSyn;
    Splitter1: TSplitter;
    tpgEntry: TTIPropertyGrid;
    tpgFormat: TTIPropertyGrid;
    TSGeneral: TTabSheet;
    TSFields: TTabSheet;
    TSPreview: TTabSheet;
    procedure btnSaveClick(Sender: TObject);
    procedure cbformatChange(Sender: TObject);
    procedure cbformatSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LBFieldsSelectionChange(Sender: TObject; User: boolean);
    procedure PCHTMLChange(Sender: TObject);
  private
    FGenerator : TDatasetHTMLGenerator;
    FFormatter : TDataFormGenerator;
    FCurrentField : TDataFieldEntryItem;
    procedure FillFormatCombo;
    procedure FillFields;
    procedure ShowSelectedField;
    procedure SetFormatter;
    procedure SetGenerator(AValue: TDatasetHTMLGenerator);
  public
    function GetHTML: String;
    Property Generator : TDatasetHTMLGenerator Read FGenerator Write SetGenerator;
  end;

var
  frmDatasetToHTMLForm: TfrmDatasetToHTMLForm;

implementation

{$R *.lfm}

{ TfrmDatasetToHTMLForm }

procedure TfrmDatasetToHTMLForm.FillFormatCombo;

begin
  TDatasetHTMLGenerator.GetGeneratorNames(cbformat.items);
  cbFormat.ItemIndex:=0;
  SetFormatter;
end;

procedure TfrmDatasetToHTMLForm.FillFields;

var
  I : Integer;
  FE : TDataFieldEntryItem;

begin
  LBFields.Items.Clear;
  If Not Assigned(FGenerator) then
    exit;
  For I:=0 to FGenerator.FieldEntries.Count-1 do
    begin
    FE:=FGenerator.FieldEntries[I];
    LBFields.Items.AddObject(FE.FieldName,FE);
    end;
  LBFields.ItemIndex:=0;
  ShowSelectedField;
end;

procedure TfrmDatasetToHTMLForm.ShowSelectedField;
begin
  With LBFields do
    FCurrentField:=Items.Objects[ItemIndex] as TDataFieldEntryItem;
  tpgEntry.TIObject:=FCurrentField;
end;

function TfrmDatasetToHTMLForm.GetHTML: String;
begin
  FGenerator.FormGenerator:=FFormatter;
  Result:=FGenerator.GetHTML;
end;

procedure TfrmDatasetToHTMLForm.FormCreate(Sender: TObject);
begin
  FillFormatCombo;
  tpgEntry.SaveOnChangeTIObject:=True;
  tpgFormat.SaveOnChangeTIObject:=True;
  SEHTML.Lines.Clear;
  PCHTML.ActivePage:=TSGeneral;
end;

procedure TfrmDatasetToHTMLForm.btnSaveClick(Sender: TObject);
begin
  if FEHTML.FileName='' then
    exit;
  SEHTML.Lines.SaveToFile(FEHTML.FileName);
end;

procedure TfrmDatasetToHTMLForm.cbformatChange(Sender: TObject);
begin
  SetFormatter;
end;

procedure TfrmDatasetToHTMLForm.cbformatSelect(Sender: TObject);
begin

end;

procedure TfrmDatasetToHTMLForm.LBFieldsSelectionChange(Sender: TObject; User: boolean);
begin
  ShowSelectedField;
  if User then ;
end;

procedure TfrmDatasetToHTMLForm.PCHTMLChange(Sender: TObject);
begin
  if PCHTML.ActivePage=TSPreview then
    SEHTML.Lines.Text:=GetHTML;
end;

procedure TfrmDatasetToHTMLForm.SetFormatter;

var
  C : TDataFormGeneratorClass;

begin
  tpgFormat.TIObject:=nil;
  FreeAndNil(FFormatter);
  C:=TDatasetHTMLGenerator.GetGeneratorClass(CBFormat.Text);
  if C<>Nil then
    begin
    FFormatter:=C.Create(Self);
    if Assigned(FGenerator) then
      FGenerator.FormGenerator:=FFormatter;
    tpgFormat.TIObject:=FFormatter;
    FillFields;
    end;
end;

procedure TfrmDatasetToHTMLForm.SetGenerator(AValue: TDatasetHTMLGenerator);
begin
  if FGenerator=AValue then Exit;
  FGenerator:=AValue;
  if Assigned(FGenerator) and Assigned(FFormatter) then
    FGenerator.FormGenerator:=FFormatter;
  FillFields;
end;


end.

