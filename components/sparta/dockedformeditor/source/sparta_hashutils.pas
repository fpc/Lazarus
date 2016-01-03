unit sparta_HashUtils;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  THash_TObject = record
    class function Hash(A: TObject; B: SizeUInt): SizeUInt; static;
  end;

implementation

class function THash_TObject.Hash(A: TObject; B: SizeUInt): SizeUInt;
begin
  if A = nil then
    Exit($2A and (b - 1));

  Result := A.GetHashCode() and (b - 1);
end;

end.

