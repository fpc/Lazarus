unit regchatctrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, chatcontrol, typingindicator, propedits, GraphPropEdits, PropEditUtils;

procedure register;

implementation

uses lresources;

procedure register;
begin
  RegisterComponents('Misc',[TChatControl,TTypingIndicator]);
  RegisterPropertyEditor(TypeInfo(String),TChatControl,'MonoFontName',TFontNamePropertyEditor);
end;

initialization
{$i chatctrls.inc}
end.

