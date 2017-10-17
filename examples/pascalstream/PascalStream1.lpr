program PascalStream1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, XMLStreaming, Unit1;

begin
  StreamAsPasForm:=TStreamAsPasForm.Create(nil);
  StreamAsPasForm.Free;
end.

