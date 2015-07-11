program ChildSizingLayout;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, MainUnit, RunTimeTypeInfoControls;

begin
  Application.Initialize;
  Application.CreateForm(TChildsizingLayoutDemoForm, ChildsizingLayoutDemoForm);
  Application.Run;
end.

