{  $Id$  }
program StartLazarus;

{$mode objfpc}{$H+}

{$IFDEF WIN32}
  {$R *.res}
{$ENDIF}

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
  Revision 1.3  2004/10/01 21:33:36  vincents
  Added icon to startlazarus.

  Revision 1.2  2004/09/03 21:14:50  vincents
  fix showing splash screen on restart

}

