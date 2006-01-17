program wndtray;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

uses
{$ifdef fpc}
  Interfaces,
{$endif}
  Forms,
  frmtest in 'frmtest.pas', TrayIconLaz;

{ add your units here }

{$R magnifier.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
