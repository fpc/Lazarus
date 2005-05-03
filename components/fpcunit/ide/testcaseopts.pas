unit testcaseopts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
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

{ TTestCaseOptionsForm }

procedure TTestCaseOptionsForm.btnAcceptClick(Sender: TObject);
begin
  Close;
end;

initialization
  {$I testcaseopts.lrs}

end.

