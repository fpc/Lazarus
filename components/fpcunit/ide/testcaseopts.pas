unit testcaseopts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type

  { TTestCaseOptionsForm }

  TTestCaseOptionsForm = class(TForm)
    btnAccept: TButton;
    cbSetup: TCheckBox;
    cbTeardown: TCheckBox;
    edDefaultName: TEdit;
    gbNames: TGroupBox;
    gbFixture: TGroupBox;
    Label1: TLabel;
    Panel1: TPanel;
    procedure btnAcceptClick(Sender: TObject);
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

end.

