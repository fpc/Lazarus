program AggPasInLCLDemo1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, LResources, AggPasLCL
  { you can add units after this };

{$IFDEF WINDOWS}{$R AggPasInLCLDemo1.rc}{$ENDIF}

begin
  {$I AggPasInLCLDemo1.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

