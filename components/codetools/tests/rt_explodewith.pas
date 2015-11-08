unit rt_explodewith;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMyClass }

  TMyClass = class
  public
    procedure DoSomething;
  end;

implementation

{ TMyClass }

procedure TMyClass.DoSomething;
var
  R: TRect;
begin
  R:=Rect(1,2,3,4);
  with R do
    {explodewith:R}Left:=4;
end;

end.

