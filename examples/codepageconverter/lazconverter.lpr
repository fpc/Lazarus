program lazconverter;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, mainunit, viewunit;

begin
  Application.Initialize;
  Application.CreateForm(TLazConverterForm, LazConverterForm);
  Application.CreateForm(TViewForm, ViewForm);
  Application.Run;
end.

