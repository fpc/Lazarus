unit markdown.regide;

{$mode objfpc}{$H+}

interface

uses
  Classes, markdown.control;

procedure register;

implementation

uses lresources;

procedure register;

begin
  RegisterComponents('Misc',[TMarkDownControl]);
end;


initialization
  {$i markdownres.inc}
end.

