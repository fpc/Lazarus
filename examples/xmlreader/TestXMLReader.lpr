program TestXMLReder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, TestXMLReaderUnit;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TXMLRederForm, XMLRederForm);
  Application.Run;
end.

