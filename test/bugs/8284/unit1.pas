unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Button1: TButton;
    CheckBox1: TCheckBox;
    RadioButton1: TRadioButton;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  if ParamStr(1)='--runtest' then begin
    // button doesn't have parentcolor, but is buttonface
    writeln('Button color: ', ColorToString(Button1.Color));
    // checkbox takes parent color
    writeln('Checkbox color: ', ColorToString(CheckBox1.Color));
    // radiobutton takes parent color
    writeln('RadioButton color: ', ColorToString(RadioButton1.Color));
    Close;
  end;
end;

initialization
  {$I unit1.lrs}

end.

