unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, RichMemo;

type

  { TForm1 }

  TForm1 = class(TForm)
    RichMemo1: TRichMemo;
    procedure RichMemo1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.RichMemo1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Caption:=IntToStr(RichMemo1.CharAtPos(x,y));
end;

end.

