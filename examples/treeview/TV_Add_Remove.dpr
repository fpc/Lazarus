program TV_Add_Remove;

uses
  Interfaces,
  Forms,
  TV_Add_Remove_U1 in 'TV_Add_Remove_U1.pas' {Form1};

{$ifdef win32}
{$R *.res}
{$endif}

begin
  Application.Title:='TV_Add_Remove';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
