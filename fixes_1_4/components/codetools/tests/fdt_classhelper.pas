unit fdt_classhelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TStringsClassHelper }

  TStringsClassHelper = class helper for TStrings
  public
    function MyVar: integer;
  end;

procedure DoIt;

implementation

procedure DoIt;
var
  sl: TStringList;
begin
  sl:=TStringList{declaration:Classes.TStringList}.Create;
  writeln('DoIt ',sl.MyVar{declaration-classhelper:fdt_classhelper.TStringsClassHelper.MyVar});
  sl.Free;
end;

{ TStringsClassHelper }

function TStringsClassHelper.MyVar: integer;
begin
  Result:=123;
end;

end.

