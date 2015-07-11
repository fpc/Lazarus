unit Unit2; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form2: TForm2; 

implementation

{$R unit2.lfm}

{ TForm2 }

procedure TForm2.FormShow(Sender: TObject);
begin
  memo1.lines.LoadFromFile(UTF8ToSys('notes.txt'));
end;

end.

