{
 Test with:
   ./finddeclarationtest --format=plain --suite=TestFindDeclaration_ClassHelper
}
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

  { TMyClass }

  TMyClass = class(TObject)
  public
    procedure Hello;
  end;

  { TMyClassHelper }

  TMyClassHelper = class helper for TMyClass
  public
    procedure Hello2;
  end;


procedure DoIt;

implementation

procedure DoIt;
var
  sl: TStringList;
begin
  sl:=TStringList{declaration:Classes.TStringList}.Create;
  writeln('DoIt ',sl.MyVar{declaration:fdt_classhelper.TStringsClassHelper.MyVar});
  sl.Free;
end;

{ TMyClassHelper }

procedure TMyClassHelper.Hello2;
begin

end;

{ TMyClass }

procedure TMyClass.Hello;
begin
  Self.Hello2{declaration:fdt_classhelper.TMyClassHelper.Hello2};
end;

{ TStringsClassHelper }

function TStringsClassHelper.MyVar: integer;
begin
  Result:=123;
end;

end.

