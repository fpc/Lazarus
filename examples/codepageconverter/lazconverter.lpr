program lazconverter;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, mainunit;

begin
  Application.Initialize;
  Application.CreateForm(TLazConverterForm, LazConverterForm);
  Application.Run;
end.

