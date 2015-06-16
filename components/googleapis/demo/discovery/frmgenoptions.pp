unit frmgenoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, EditBtn;

type

  { TGenCodeFormOptions }

  TGenCodeFormOptions = class(TForm)
    ButtonPanel1: TButtonPanel;
    CBView: TCheckBox;
    EBaseClass: TEdit;
    EUnitName: TEdit;
    EExtraUnits: TEdit;
    EPrefix: TEdit;
    EFileName: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LFileName: TLabel;
    procedure EUnitNameEditingDone(Sender: TObject);
  private
    function GetB: Boolean;
    function GetS(AIndex: Integer): String;
    procedure SetB(AValue: Boolean);
    procedure SetS(AIndex: Integer; AValue: String);
    { private declarations }
  public
    { public declarations }
    Property FileName : String index 0 read GetS Write SetS;
    Property UnitName : String index 1 read GetS Write SetS;
    Property ExtraUnits : String index 2 read GetS Write SetS;
    Property Prefix : String index 3 read GetS Write SetS;
    Property BaseClass : String index 4  read GetS Write SetS;
    Property DoPreview : Boolean Read GetB Write SetB;
  end;

var
  GenCodeFormOptions: TGenCodeFormOptions;

implementation

{$R *.lfm}

{ TGenCodeFormOptions }

procedure TGenCodeFormOptions.EUnitNameEditingDone(Sender: TObject);

Var
  E,FN : String;

begin
  E:=ExtractFileExt(EFileName.FileName);
  if E='' then
    E:=EFileName.DefaultExt;
  FN:=ExtractFilePath(EFileName.FileName);
  if FN='' then
    FN:=IncludeTrailingPathDelimiter(Application.Location);
  FN:=FN+EUnitName.Text+E;
  EFileName.FileName:=FN;
end;

function TGenCodeFormOptions.GetB: Boolean;
begin
  Result:=CBView.Checked;
end;

function TGenCodeFormOptions.GetS(AIndex: Integer): String;
begin
  Case AIndex of
    0 : Result:=EFileName.Text;
    1 : Result:=EUnitName.Text;
    2 : Result:=EExtraUnits.Text;
    3 : Result:=EPRefix.Text;
    4 : Result:=EBaseClass.Text;
  end;
end;

procedure TGenCodeFormOptions.SetB(AValue: Boolean);
begin
  CBView.Checked:=AValue
end;

procedure TGenCodeFormOptions.SetS(AIndex: Integer; AValue: String);
begin
  Case AIndex of
    0 : EFileName.Text:=Avalue;
    1 : begin
        EUnitName.Text:=AValue;
        EUnitNameEditingDone(EUnitName);
        end;
    2 : EExtraUnits.Text:=AValue;
    3 : EPRefix.Text:=AValue;
    4 : EBaseClass.Text:=AValue;
  end;
end;

end.

