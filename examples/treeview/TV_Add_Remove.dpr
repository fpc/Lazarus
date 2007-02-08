program TV_Add_Remove;

uses
  Interfaces,
  Forms,
  TV_Add_Remove_U1 in 'TV_Add_Remove_U1.pas' {Form1};

{$ifdef windows}
{$R *.res}
{$endif}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
