unit frmcreatecode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ButtonPanel, Spin, EditBtn, StdCtrls, ExtCtrls, SynEdit,
  SynHighlighterPas, fpjson, fpjsontopas;

type

  { TCreateCodeForm }

  TCreateCodeForm = class(TForm)
    BPCode: TButtonPanel;
    CGOptions: TCheckGroup;
    EConstructorArgs: TEdit;
    EDefaultParentName: TEdit;
    EExtraUnitNames: TEdit;
    EPropertyTypeSuffixEdit: TEdit;
    EUnitName: TEdit;
    EFieldPrefix: TEdit;
    ETopLevelClassName: TEdit;
    FECode: TFileNameEdit;
    LETopLevelClassName: TLabel;
    LParentName: TLabel;
    LExtraUnitNames: TLabel;
    LEConstructorArgsLabel1: TLabel;
    LEPropertyTypeSuffix: TLabel;
    LEunitName: TLabel;
    LEunitName1: TLabel;
    LFECode: TLabel;
    LSEindent: TLabel;
    PCCode: TPageControl;
    SEIndent: TSpinEdit;
    SECode: TSynEdit;
    SynFPC: TSynFreePascalSyn;
    TSOptions: TTabSheet;
    TSCode: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PCCodeChange(Sender: TObject);
  private
    FGenerator : TJSONToPascal;
    procedure FormToGenerator;
    function GetJSON: TJSONData;
    procedure SetJSON(AValue: TJSONData);
  public
    Property JSON : TJSONData Read GetJSON Write SetJSON;
  end;

Procedure CreateCodeFromJSON(aData : TJSONData);

implementation

{$R *.lfm}

uses msgjsonviewer;

Procedure CreateCodeFromJSON(aData : TJSONData);

begin
  With TCreateCodeForm.Create(Application) do
    try
      JSON:=aData;
      ShowModal;
    finally
      Free;
    end;
end;

{ TCreateCodeForm }

procedure TCreateCodeForm.FormCreate(Sender: TObject);
begin
  FGenerator:=TJSONToPascal.Create(Self);
  {$IF FPC_FULLVERSION<=30004}
  ETopLevelClassName.Enabled:=False;
  ETopLevelClassName.Text:='TMyObject';
  {$ENDIF}
end;

procedure TCreateCodeForm.OKButtonClick(Sender: TObject);
begin
  if (FECode.FileName='') then
    begin
    ShowMessage(SEFileNameNeeded);
    exit;
    end;
  FormToGenerator;
  FGenerator.Code.Clear;
  FGenerator.Execute;
  FGenerator.Code.SaveToFile(FECode.FileName);
  ModalResult:=mrOK;
end;

procedure TCreateCodeForm.PCCodeChange(Sender: TObject);
begin
  If PCCode.ActivePage=TSCode then
    begin
    FormToGenerator;
    FGenerator.Code.Clear;
    FGenerator.Execute;
    SECode.Lines:=FGenerator.Code;
    end;
end;

function TCreateCodeForm.GetJSON: TJSONData;
begin
  Result:=FGenerator.JSONData;
end;

procedure TCreateCodeForm.SetJSON(AValue: TJSONData);
begin
  FGenerator.JSONData:=AValue;
end;

procedure TCreateCodeForm.FormToGenerator;

Var
  O : TJSONToPascaloptions;
  T : TJSONToPascaloption;

begin
  O:=[];
  For T in TJSONToPascaloption do
    if CGoptions.Checked[Ord(T)] then
      Include(O,T);
  FGenerator.Options:=O;
  {$IF FPC_FULLVERSION>30004}
  FGenerator.TopLevelObjectClassName:=ETopLevelClassName.Text;
  {$ENDIF}
  FGenerator.DestUnitName:=EUnitName.Text;
  if (FGenerator.DestUnitName='') then
    FGenerator.DestUnitName:=ChangeFileExt(ExtractFileName(FECode.FileName),'');
  FGenerator.IndentSize:=SEIndent.Value;
  if (EFieldPrefix.Text<>'') then
    FGenerator.FieldPrefix:=EFieldPrefix.Text
  else
    FGenerator.FieldPrefix:='F';
  FGenerator.PropertyTypeSuffix:=EPropertyTypeSuffixEdit.Text;
  FGenerator.ObjectConstructorArguments:=EConstructorArgs.Text;
  FGenerator.ExtraUnitNames:=EExtraUnitNames.Text;
  FGenerator.DefaultParentName:=EDefaultParentName.Text;
end;

end.

