unit LazEditTypes;

{$mode objfpc}{$H+}

interface

uses Types;

type
  TLinePos = type integer; // 1..high(Integer);
  TLineIdx = type integer; // 0..high(Integer);
  IntPos = type integer; // 1..high(Integer);
  IntIdx = type integer; // 0..high(Integer);

  TLogTokenPos = record
    X, Y: IntPos;
    Len: Integer;
  end;

operator := (p: TPoint): TLogTokenPos;
operator := (p: TLogTokenPos): TPoint;

implementation

operator := (p: TPoint): TLogTokenPos;
begin
  Result.X := p.X;
  Result.Y := p.Y;
end;

operator := (p: TLogTokenPos): TPoint;
begin
  Result.X := p.X;
  Result.Y := p.Y;
end;

end.

