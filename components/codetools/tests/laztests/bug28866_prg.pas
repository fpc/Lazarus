program bug28866_prg;

{$mode objfpc}{$H+}

uses
  bug28866_unit1;

var
  S: String;
  O: TObject;
begin
  S.Twice{declaration:bug28866_unit1.TStringHelper.Twice};
  O.Test{declaration:bug28866_unit1.TObjectHelper.Test};
end.

