unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, Buttons, LCLProc;

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    CloseButton: TButton;
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  DecimalSeparator := '.';
  Label1.Caption := FloatSpinEdit1.Caption;
  Label2.Caption := FloatSpinEdit2.Caption;
end;

procedure TForm1.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  if ParamStr(1)='--runtest' then begin
    DebugLn(FloatSpinEdit1.Caption);
    DebugLn(Label1.Caption);
    DebugLn(FloatSpinEdit2.Caption);
    DebugLn(Label2.Caption);
    Close;
  end;
end;

initialization
  {$I unit1.lrs}

end.

