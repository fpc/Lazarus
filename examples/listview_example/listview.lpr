program listview;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, testform;

begin
  Application.Initialize;
  Application.CreateForm ( TForm1, Form1 ) ;
  Application.Run;
end.

