program bug28877;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { you can add units after this };

operator := (AValue : Integer) : TGUID;
begin
  Result.Data1{declaration:system.TGUID.Data1};
end;

function Test : TGUID;
begin
  Result.Data1{declaration:system.TGUID.Data1};
end;

begin
end.

