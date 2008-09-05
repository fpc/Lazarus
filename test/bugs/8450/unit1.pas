unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    ComboBox1: TComboBox;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

var
  InTestRun: boolean;

{ TForm1 }

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  ComboBox1.SelStart := 3;
  ComboBox1.SelLength := 3;
  if InTestRun then
    writeln('ComboBox SelText: ', ComboBox1.SelText);
  ComboBox1.SelText := '1234';

  if InTestRun then begin
    writeln('ComboBox.Text: ', ComboBox1.Text);
    writeln('ComboBox.SelText: ', ComboBox1.SelText);
    Close;
  end;
  ApplicationProperties1.OnIdle := nil;
end;

initialization
  {$I unit1.lrs}
  InTestRun :=  ParamStr(1)='--runtest';

end.

