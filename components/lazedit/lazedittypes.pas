{
 *****************************************************************************
  This file is part of the LazEdit package from the Lazarus IDE.

  This content of this file is licensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 or 2.0 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
  Used units may have to be amended according to the choice.
 *****************************************************************************
}
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

