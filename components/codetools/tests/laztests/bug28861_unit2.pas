unit bug28861_unit2;

{$mode delphi}

interface

uses
  bug28861_unit1;

implementation

begin
  'Hello'.Twice{declaration:bug28861_unit1.TStringHelper.Twice};
  'Hello'.Thrice{declaration:bug28861_unit1.TStringHelper.Thrice};
  'Hello'.GetTheLength{declaration:bug28861_unit1.TStringHelper.GetTheLength};
  'Hello'.{collectidentifiers:}
end.

