unit fdt_typehelper;

{$mode objfpc}{$H+}
{$ModeSwitch typehelpers}

interface

uses
  Classes, SysUtils;

type

  { TStringTypeHelper }

  TStringTypeHelper = type helper for ShortString
  public
    function MyVar: integer;
  end;

procedure DoIt;

implementation

procedure DoIt;
var
  s: ShortString;
begin
  s:='ABC';
  writeln('DoIt ',s.MyVar{declaration:fdt_typehelper.TStringTypeHelper.MyVar});
end;

{ TStringTypeHelper }

function TStringTypeHelper.MyVar: integer;
begin
  Result:=123;
end;

end.

