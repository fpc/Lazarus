unit reglazvlc;

{$IF FPC_FULLVERSION<20701}
{$ERROR needs at least FPC 2.7.1}
{$ENDIF}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vlc, lclvlc, lresources;

Procedure register;

implementation

Procedure register;
begin
  RegisterComponents('Multimedia',[TLCLVLCPlayer,TVLCMediaListPlayer]);
end;

initialization
{$i lazvlc.res}
end.

