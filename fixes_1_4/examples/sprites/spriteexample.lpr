program SpriteExample;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, PlayGround;

begin
  Application.Initialize;
  Application.CreateForm(TPlayGroundForm, PlayGroundForm);
  Application.Run;
end.

