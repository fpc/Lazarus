unit testcaseopts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, LCLType, LCLProc, StrTestCaseOpts;

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
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
  ModalResult := mrOK;
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
  btnAccept.Hint:= '['+ShortCutToText(KeyToShortCut(VK_RETURN, [ssCtrl]))+']';
end;

procedure TTestCaseOptionsForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Shift = []) then
  begin
    ModalResult := mrCancel;
    Key := 0;
  end
  else if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    ModalResult := mrOK;
    Key := 0;
  end;
end;

end.

