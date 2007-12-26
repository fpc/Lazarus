unit frmBaseConfigCodeGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, ComCtrls, RTTIGrids, CheckLst, fpddcodegen, Buttons,
  ActnList, ButtonPanel;

type

  { TBaseConfigGeneratorForm }

  TBaseConfigGeneratorForm = class(TForm)
    ADown: TAction;
    AUP: TAction;
    ALList: TActionList;
    PDlgButtons: TButtonPanel;
    CBShowDialog: TCheckBox;
    CLBFields: TCheckListBox;
    FEFile: TFileNameEdit;
    LSave: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PCConf: TPageControl;
    PGenerator: TPanel;
    Panel2: TPanel;
    PFieldList: TPanel;
    PButtons: TPanel;
    SBup: TSpeedButton;
    SBDown: TSpeedButton;
    Splitter1: TSplitter;
    GFieldProps: TTIPropertyGrid;
    GCodeOptions: TTIPropertyGrid;
    TSFields: TTabSheet;
    TSOptions: TTabSheet;
    procedure CLBFieldsClick(Sender: TObject);
    procedure CLBFieldsItemClick(Sender: TObject; Index: integer);
    procedure CLBFieldsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure ADownExecute(Sender: TObject);
    procedure AUpExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FFieldmap : TFieldPropDefs;
    FGen: TDDCustomCodeGenerator;
    FCodeOptions : TCodeGeneratorOptions;
    procedure FormToGenerator;
    Procedure GeneratorToForm;
    function GetExtra: Boolean;
    function GetFileName: String;
    function GetShowResult: Boolean;
    procedure MoveFieldDown;
    function MoveFieldUp: Boolean;
    procedure OnOkClick(Sender: TObject);
    procedure SelectField(F: TFieldPropDef);
    procedure SetExtra(const AValue: Boolean);
    procedure SetFileName(const AValue: String);
    procedure SetGen(const AValue: TDDCustomCodeGenerator);
    procedure SetShowResult(const AValue: Boolean);
    procedure ShowSelectedField;
  public
    { public declarations }
    Property Generator : TDDCustomCodeGenerator Read FGen Write SetGen;
    Property ShowExtra : Boolean Read GetExtra Write SetExtra;
    Property FileName : String Read GetFileName Write SetFileName;
    Property ShowResult: Boolean Read GetShowResult Write SetShowResult;
  end;

var
  BaseConfigGeneratorForm: TBaseConfigGeneratorForm;

implementation

uses typinfo,lcltype;

{ TBaseConfigGeneratorForm }

procedure TBaseConfigGeneratorForm.CLBFieldsClick(Sender: TObject);
begin
  ShowSelectedField;
end;

procedure TBaseConfigGeneratorForm.CLBFieldsItemClick(Sender: TObject;
  Index: integer);
begin
  CLBFields.ItemIndex:=Index;
  ShowSelectedField;
  With CLBFields do
    If (ItemIndex<>-1) then
      begin
      FFieldMap[ItemIndex].Enabled:=Checked[ItemIndex];
      GFieldProps.PropertyEditorHook.RefreshPropertyValues;
      end;
end;

procedure TBaseConfigGeneratorForm.CLBFieldsKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Shift=[ssShift] then
    begin
    If (Key=VK_UP)  then
      MoveFieldUp
    else if (Key=VK_DOWN) then
      MoveFieldDown
    end;
end;

procedure TBaseConfigGeneratorForm.ShowSelectedField;

begin
  If (CLBFields.ItemIndex=-1) then
    SelectField(Nil)
  else
    SelectField(FFieldMap[CLBFields.ItemIndex]);
end;


procedure TBaseConfigGeneratorForm.GeneratorToForm;

Var
  B : Boolean;
  I,J : Integer;
  PD : TFieldPropDef;
  CC : TCodeGeneratorOptionsClass;
  
begin
  { The following construct means that only explicitly added
    can be configured, or all fields. }
  FreeAndNil(FFieldMap);
  FFieldMap:=TFieldPropDefs.Create(FGen.Fields.ItemClass);
  If Not FGen.NeedsFieldDefs then
    begin
    PCConf.ActivePage:=TSOptions;
    TSFields.TabVisible:=False;
    end
  else
    begin
    For I:=0 to FGen.Fields.Count-1 do
      FFieldMap.Add.Assign(FGen.Fields[i]);
    For I:=0 to FFieldMap.Count-1 do
      begin
      PD:=FFieldMap[i];
      J:=CLBFields.Items.AddObject(PD.FieldName,PD);
      CLBFields.Checked[J]:=PD.Enabled;
      end;
    If (CLBFields.Items.Count>0) then
      begin
      CLBFields.ItemIndex:=0;
      SelectField(FFieldMap[0])
      end
    else
      begin
      CLBFields.ItemIndex:=-1;
      SelectField(Nil);
      end;
    end;
  CC:=TCodeGeneratorOptionsClass(FGen.CodeOptions.ClassType);
  FCodeOptions:=CC.Create;
  FCodeOptions.Assign(FGen.CodeOptions);
  GCodeOptions.TIObject:=FCodeOptions;
end;

Procedure TBaseConfigGeneratorForm.SelectField(F : TFieldPropDef);

begin
  GFieldProps.TIObject:=F;
  GFieldProps.Enabled:=(F<>Nil);
end;

function TBaseConfigGeneratorForm.GetExtra: Boolean;
begin
  Result:=PGenerator.Visible;
end;

function TBaseConfigGeneratorForm.GetFileName: String;
begin
  Result:=FEFile.FileName;
end;

function TBaseConfigGeneratorForm.GetShowResult: Boolean;
begin
  Result:=CBShowDialog.Checked
end;

procedure TBaseConfigGeneratorForm.SetExtra(const AValue: Boolean);
begin
  PGenerator.Visible:=AValue;
end;

procedure TBaseConfigGeneratorForm.SetFileName(const AValue: String);
begin
  FEFile.FileName:=AValue;
end;

procedure TBaseConfigGeneratorForm.SetGen(const AValue: TDDCustomCodeGenerator);
begin
  if FGen=AValue then exit;
  FGen:=AValue;
  If Assigned(FGen) then
    GeneratorToForm;
end;

procedure TBaseConfigGeneratorForm.SetShowResult(const AValue: Boolean);
begin
  CBShowDialog.Checked:=AValue;
end;

procedure TBaseConfigGeneratorForm.AUpExecute(Sender: TObject);
begin
  MoveFieldUp;
end;

procedure TBaseConfigGeneratorForm.FormCreate(Sender: TObject);
begin
  PDlgButtons.OKButton.OnClick:=@OnOKClick;
end;

procedure TBaseConfigGeneratorForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFieldMap);
  FreeAndNil(FCodeOPtions);
end;

procedure TBaseConfigGeneratorForm.OnOkClick(Sender: TObject);

begin
  FormToGenerator;
end;


Function TBaseConfigGeneratorForm.MoveFieldUp : Boolean;

begin
  With CLBFields do
    If (ItemIndex>0) then
      begin
      Items.Exchange(ItemIndex,ItemIndex-1);
      FFieldMap.Items[ItemIndex].Index:=ItemIndex-1;
      ItemIndex:=ItemIndex-1;
      end;
end;

procedure TBaseConfigGeneratorForm.ADownExecute(Sender: TObject);
begin
  MoveFieldDown;
end;

procedure TBaseConfigGeneratorForm.MoveFieldDown;

begin
  With CLBFields do
    If (ItemIndex<Items.Count-1) then
      begin
      Items.Exchange(ItemIndex,ItemIndex+1);
      FFieldMap.Items[ItemIndex].Index:=ItemIndex+1;
      ItemIndex:=ItemIndex+1;
      end;
end;

procedure TBaseConfigGeneratorForm.FormToGenerator;

Var
  I : Integer;

begin
  For I:=0 to FFieldMap.Count-1 do
    FGen.Fields[I].Assign(FFieldMap[i]);
  FGen.CodeOptions.Assign(FCodeOptions);
end;
initialization
  {$I frmbaseconfigcodegenerator.lrs}

end.

