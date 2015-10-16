unit bug28861_unit2;

{$mode delphi}

interface

uses
  bug268861_unit1;

implementation

begin
  'Hello'.Twice{declaration:bug268861_unit1.TStringHelper.Twice};
  'Hello'.Thrice{declaration:bug268861_unit1.TStringHelper.Thrice};
  'Hello'.GetTheLength{declaration:bug268861_unit1.TStringHelper.GetTheLength};
  'Hello'.{collectidentifiers:}
end.

