unit bug28866_unit1;

{$mode delphi}

interface

{ TStringHelper }

type
  TStringHelper = record helper for string
  private
    function GetTheLength: Integer;
  public
    function Twice: string;
    function Thrice: string;
    property TheLength: Integer read GetTheLength;
  end;

  TObjectHelper = class helper for TObject
  public
    function Test: Integer;
  end;

implementation

{ TObjectHelper }

function TObjectHelper.Test: Integer;
begin

end;

function TStringHelper.GetTheLength: Integer;
begin
  Result := Length(Self)
end;

function TStringHelper.Twice: string;
begin
  Result := Self + Self;
end;

function TStringHelper.Thrice: string;
begin
  Result := Self + Self + Self;
end;

end.

