program StartLazarus;

{$mode objfpc}{$H+}

uses
  Interfaces, SysUtils,
  Forms,
  Splash, LazarusManager;
  
var
  ALazarusManager: TLazarusManager;
  
procedure ShowSplash;
begin
  SplashForm := TSplashForm.Create(Application);
  with SplashForm do begin
    Show;
    Paint;
    StartTimer;
  end;
  Application.ProcessMessages; // process splash paint message
end;

begin
  Application.Initialize;
  ShowSplash;
  ALazarusManager := TLazarusManager.Create;
  ALazarusManager.Run;
  FreeAndNil(ALazarusManager);
end.

