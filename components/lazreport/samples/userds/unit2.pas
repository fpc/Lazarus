unit Unit2; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

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

{ TForm2 }

procedure TForm2.FormShow(Sender: TObject);
begin
  memo1.lines.LoadFromFile('notes.txt');
end;

initialization
  {$I unit2.lrs}

end.

