unit tunit_order_test;

{$mode objfpc}{$H+}

interface

uses
  unit_order_a, unit_order_b;

implementation

begin
  unit_order_b.v{declaration:unit_order_b.v}:=3;
  v{declaration:unit_order_b.v}:=3;
  unit_order_a.v{declaration:unit_order_a.v}:='3';
end.

