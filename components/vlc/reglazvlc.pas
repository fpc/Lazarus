unit reglazvlc;

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

