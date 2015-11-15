unit unit_order_test;

{$mode objfpc}{$H+}

interface

uses
  unit_order_a, unit_order_b;

implementation

begin
  unit_order_b.unit_order_a{declaration:unit_order_b.unit_order_a}:=3;
  unit_order_a.unit_order_b{declaration:unit_order_a.unit_order_b}:='3';
end.

