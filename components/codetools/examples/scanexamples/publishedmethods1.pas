unit PublishedMethods1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PublishedMethods2;

type
  TPoint = PublishedMethods2.Point;
  TMyMethodWithTPoint = procedure (x: TPoint) of object;

function Point: TPoint; // <- it is not TypeDefinition of "Point"

type
  TMyMethodWithAnInteger = procedure (x:T) of object;
  T = char;
  TMyMethodWithAnChar = procedure (x:T) of object;

  { TMyClass }
  {$M+}
  TMyClass = class
  published
    procedure F(x: T);
    procedure DoPoint(x: TPoint);
  end;
  {$M-}

implementation

function Point: TPoint;
begin

end;

{ TMyClass }

procedure TMyClass.F(x: T);
begin

end;

procedure TMyClass.DoPoint(x: TPoint);
begin

end;

end.

