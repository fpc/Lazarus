unit bug28861_unit1;

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

implementation

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

