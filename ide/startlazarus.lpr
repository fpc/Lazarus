{  $Id$  }
program StartLazarus;

{$mode objfpc}{$H+}

uses
  Interfaces, SysUtils,
  Forms,
  LazarusManager;
  
var
  ALazarusManager: TLazarusManager;
  
begin
  Application.Initialize;
  ALazarusManager := TLazarusManager.Create;
  ALazarusManager.Run;
  FreeAndNil(ALazarusManager);
end.
{
  $Log$
  Revision 1.2  2004/09/03 21:14:50  vincents
  fix showing splash screen on restart

}

