unit frmauthentication;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, IniPropStorage, StdCtrls;

type

  { TAuthenticationForm }

  TAuthenticationForm = class(TForm)
    BPHeader: TButtonPanel;
    CBUserName: TComboBox;
    EPassword: TEdit;
    LCBUserName: TLabel;
    LEPassword: TLabel;
    PSHeaders: TIniPropStorage;
  private
    function GetString(AIndex: Integer): String;
    procedure SetString(AIndex: Integer; AValue: String);

  public
    Property UserName : String Index 1 Read GetString Write SetString;
    Property Password : String Index 2 Read GetString Write SetString;
  end;

var
  AuthenticationForm: TAuthenticationForm;

implementation

{$R *.lfm}

{ TAuthenticationForm }

function TAuthenticationForm.GetString(AIndex: Integer): String;
begin
  Case aIndex of
    1 : Result:=CBUserName.Text;
    2 : Result:=EPassword.Text;
  end;
end;

procedure TAuthenticationForm.SetString(AIndex: Integer; AValue: String);
begin
  Case aIndex of
    1 : CBUserName.Text:=aValue;
    2 : EPassword.Text:=aValue;
  end;
end;

end.

