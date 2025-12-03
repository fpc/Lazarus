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
  for E{guesstype:low(TSet2)..high(TSet2)} in ReturnSet2 do ;

  for F{guesstype:byte} := low(TByteSet) to high(TByteSet) do ;
  for G{guesstype:TEnum} := low(TSet1) to high(TSet1) do ;
  for H{guesstype:low(TSet2)..high(TSet2)} := low(TSet2) to high(TSet2) do ;

end.

