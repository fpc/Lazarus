program NonLCL1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, NotLCLDesigner, Interfaces, Unit1;

begin
end.

