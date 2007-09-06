program calleditorwithpkg;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, maincalleditor;

begin
  Application.Title:='LazReport Designer';
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

