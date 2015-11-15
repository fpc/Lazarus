unit rt_explodewith;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMyClass }

  TMyClass = class
  public
    Parent: TMyClass;
    Left: string;
    procedure DoSomething;
  end;

implementation

{ TMyClass }

procedure TMyClass.DoSomething;
var
  R1: TRect;
begin
  R1:=Rect(1,2,3,4);
  with R1 do
    {explodewith:R1}Left:=4;
  if R1.Left= 4 then ;
  with Parent do
    {explodewith:Parent}Left:='A';
  with Parent.Parent do
    {explodewith:Parent}Left:='A';
end;

end.

