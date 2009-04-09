unit LCLClipboardUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  ClipBrd;

type

  { TForm1 }

  TForm1 = class(TForm)
    GetTargetsButton: TButton;
    procedure GetTargetsButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.GetTargetsButtonClick(Sender: TObject);
var
  s: String;
begin
  writeln('TForm1.GetTargetsButtonClick Getting PrimarySelection ...');
  s:=PrimarySelection.AsText;
  writeln('TForm1.GetTargetsButtonClick PrimarySelection="',s,'"');
end;

initialization
  {$I lclclipboardunit.lrs}

end.

