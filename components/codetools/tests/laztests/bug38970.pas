program bug38970;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes
  { you can add units after this };

type
  generic TOption<T> = record
  end;

  generic TIterNestedFunc<T> = function (): specialize TOption{declaration:TOption}<T> is nested;

procedure Test;
begin
end;

begin
end.


