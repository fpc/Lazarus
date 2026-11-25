unit Markdown.RegIDE;

{$mode objfpc}{$H+}

interface

uses
  Classes, Markdown.Control;

procedure register;

implementation

uses LResources;

procedure register;

begin
  RegisterComponents('Misc',[TMarkdownControl]);
end;


initialization
  {$i markdownres.inc}
end.

