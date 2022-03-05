unit Unit1;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

    { TForm1 }

    TForm1 = class(TForm)
        ButtonHello: TButton;
        ButtonClose: TButton;
        Memo1: TMemo;
        procedure ButtonCloseClick(Sender: TObject);
        procedure ButtonHelloClick(Sender: TObject);
    private

    public

    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonHelloClick(Sender: TObject);
begin
    Memo1.Append('Hello World');
end;

procedure TForm1.ButtonCloseClick(Sender: TObject);
begin
    close;
end;

end.

