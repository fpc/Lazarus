unit frmaddtofavourite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel;

type

  { TSaveRequestDataForm }

  TSaveRequestDataForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CBSaveContent: TCheckBox;
    EName: TEdit;
    Label1: TLabel;
  private
    function GetRN: String;
    function GetSC: Boolean;
    procedure SetRN(AValue: String);
    procedure SetSC(AValue: Boolean);
  public
    Property RequestName : String Read GetRN Write SetRN;
    Property SaveContent : Boolean Read GetSC Write SetSC;
  end;

var
  SaveRequestDataForm: TSaveRequestDataForm;

implementation

{$R *.lfm}

{ TSaveRequestDataForm }

function TSaveRequestDataForm.GetRN: String;
begin
  Result:=EName.Text;
end;

function TSaveRequestDataForm.GetSC: Boolean;
begin
  Result:=CBSaveContent.Checked;
end;

procedure TSaveRequestDataForm.SetRN(AValue: String);
begin
  EName.Text:=aValue;
end;

procedure TSaveRequestDataForm.SetSC(AValue: Boolean);
begin
  CBSaveContent.Checked:=AValue;
end;

end.

