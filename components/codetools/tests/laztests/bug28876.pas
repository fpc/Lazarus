program bug28876;

{$mode objfpc}{$H+}

uses
  Classes;
Var
  AClass : TClass;
begin
  AClass := TObject;
  AClass.ClassName{declaration:system.TObject.ClassName};
end.

