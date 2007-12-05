program lclversionexample;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, Unit1;

begin
  Application.Initialize;
  Application.CreateForm(TVersionForm, VersionForm);
  Application.Run;
end.

