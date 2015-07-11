unit testcaseopts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, strtestcaseopts;

type

  { TTestCaseOptionsForm }

  TTestCaseOptionsForm = class(TForm)
    btnAccept: TButton;
    cbSetup: TCheckBox;
    cbTeardown: TCheckBox;
    edDefaultName: TEdit;
    gbFixture: TGroupBox;
    gbNames: TGroupBox;
    Label1: TLabel;
    procedure btnAcceptClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 


implementation

{$R *.lfm}

{ TTestCaseOptionsForm }

procedure TTestCaseOptionsForm.btnAcceptClick(Sender: TObject);
begin
  Close;
end;

procedure TTestCaseOptionsForm.FormCreate(Sender: TObject);
begin
  Caption := sfrmTest;
  gbNames.Caption:= sgrpNames;
  gbFixture.Caption:= sgrpFixture;
  label1.Caption:= slblDefault;
  cbSetup.Caption:= schkSetup;
  cbTeardown.Caption:= schkTear;
  btnAccept.Caption:= sbtnCreate;
end;

end.

