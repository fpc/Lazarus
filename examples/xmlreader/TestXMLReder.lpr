program TestXMLReder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, TestXMLReaderUnit, LResources
  { you can add units after this };

{$IFDEF WINDOWS}{$R TestXMLReder.rc}{$ENDIF}

begin
  {$I TestXMLReder.lrs}
  Application.Initialize;
  Application.CreateForm(TXMLRederForm, XMLRederForm);
  Application.Run;
end.

