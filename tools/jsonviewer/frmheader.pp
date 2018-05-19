unit frmheader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel, IniPropStorage;

type

  { THeaderForm }

  THeaderForm = class(TForm)
    BPHeader: TButtonPanel;
    CBName: TComboBox;
    CBValue: TComboBox;
    PSHeaders: TIniPropStorage;
    LCBName: TLabel;
    LCBValue: TLabel;
    procedure CBNameChange(Sender: TObject);
    procedure CBValueChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    procedure CheckOK;
    function GetComboBox(AIndex: Integer): TCombobox;
    function GetString(AIndex: Integer): String;
    procedure SetString(AIndex: Integer; AValue: String);
    function ValidInput: Boolean;

  public
    Property HeaderName : String Index 1 Read GetString Write SetString;
    Property HeaderValue : String Index 2 Read GetString Write SetString;
  end;

var
  HeaderForm: THeaderForm;

implementation

{$R *.lfm}

{ THeaderForm }
function THeaderForm.ValidInput : Boolean;

begin
  Result:=(CBName.Text<>'')
          and (CBValue.Text<>'')
          and (Pos(':',CBName.Text)=0);
end;

procedure THeaderForm.CheckOK;

begin
  BPHeader.OKButton.Enabled:=ValidInput;
end;

function THeaderForm.GetComboBox(AIndex: Integer): TCombobox;

begin
  Case AIndex Of
    1 : Result:=CBName;
    2 : Result:=CBValue;
  end;
end;

function THeaderForm.GetString(AIndex: Integer): String;
begin
  Result:=GetComboBox(AIndex).Text;
end;

procedure THeaderForm.SetString(AIndex: Integer; AValue: String);
begin
  GetComboBox(AIndex).Text:=AValue;
end;

procedure THeaderForm.CBNameChange(Sender: TObject);
begin
  CheckOK;
end;

procedure THeaderForm.CBValueChange(Sender: TObject);
begin
  CheckOK;
end;

procedure THeaderForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  With CBName do
    if Items.IndexOf(Text)=-1 then
      Items.Add(Text);
  With CBValue do
    if Items.IndexOf(Text)=-1 then
      Items.Add(Text);
end;

procedure THeaderForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=(ModalResult<>mrOK) or ValidInput;
end;

procedure THeaderForm.FormShow(Sender: TObject);
begin
  CheckOK;
end;

end.

