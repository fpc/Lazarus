program bug34845;
{$mode objfpc}{$h+}
type
  TByteSet = Set of byte;
  TSysCharSet = Set of AnsiChar;
  TEnum = (e1,e2);
  TSet1 = Set of TEnum;
  TSet2 = Set of (s1,s2,s3);

function ReturnBytes: TByteSet;
begin
  Result := [];
end;

function ReturnChars: TSysCharSet;
begin
  Result := [];
end;

function ReturnSet1: TSet1;
begin
  Result := [];
end;

function ReturnSet2: TSet2;
begin
  Result := [];
end;

begin
  for B{guesstype:byte} in ReturnBytes do ;
  for C{guesstype:ansichar} in ReturnChars do ;
  for D{guesstype:TEnum} in ReturnSet1 do ;
end.

