unit bug28861_unit2;

{$mode delphi}

interface

uses
  bug28861_unit1;

implementation

begin
  'Hello'.Twice{declaration:bug28861_unit1.TStringHelper.Twice};
  'Hello'.Thrice{declaration:bug28861_unit1.TStringHelper.Thrice};
  'Hello'.TheLength{declaration:bug28861_unit1.TStringHelper.TheLength};
end.

