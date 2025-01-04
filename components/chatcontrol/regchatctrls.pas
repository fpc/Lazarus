unit regchatctrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chatcontrol, typingindicator;

procedure register;

implementation

uses lresources;

procedure register;
begin
  RegisterComponents('Misc',[TChatControl,TTypingIndicator]);
end;

initialization
{$i chatctrls.inc}
end.

