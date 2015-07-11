program project1;

{$mode objfpc}{$H+}

{$ifdef Darwin}
{$linklib libglib-2.0.dylib}
{$linklib libgobject-2.0.dylib}
{$linklib libpango-1.0.dylib}
{$linklib libpangocairo-1.0.dylib}
{$endif}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, printer4lazarus
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

