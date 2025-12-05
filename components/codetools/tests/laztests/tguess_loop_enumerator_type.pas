program tguess_loop_enumerator_type; // bug #34845
{$mode objfpc}{$h+}
type
  TByteSet = Set of byte;
  TSysCharSet = Set of AnsiChar;
  TEnum = (e1,e2,e3);
  TEnumRange = e1..e2;
  TSet1 = Set of TEnum;
  TSet2 = Set of (s1,s2,s3);
  TSet3 = Set of 1..5;
  TSet4 = Set of TEnumRange;
  TSet5 = Set of e2..e3;

TReturnBytes = function: TByteSet;

function ReturnBytes: TByteSet;    begin Result := [];end;
function ReturnChars: TSysCharSet; begin Result := []; end;
function ReturnSet1:  TSet1;       begin Result := []; end;
function ReturnSet2:  TSet2;       begin Result := []; end;
function ReturnSet3:  TSet3;       begin Result := []; end;
function ReturnSet4:  TSet4;       begin Result := []; end;
function ReturnSet5:  TSet5;       begin Result := []; end;

var
  vReturnBytes: TReturnBytes;
begin
  for A1{guesstype:byte}                    in ReturnBytes do ;
  for A2{guesstype:ansichar}                in ReturnChars do ;
  for A3{guesstype:TEnum}                   in ReturnSet1 do ;
  for A4{guesstype:low(TSet2)..high(TSet2)} in ReturnSet2 do ;
  for A5{guesstype:integer}                 in ReturnSet3 do ;
  for A6{ TODO guesstype:TEnumRange}              in ReturnSet4 do ;
  for A7{guesstype:TEnum}                   in ReturnSet5 do ;

  for D1{ TODO: guesstype:byte}                    in vReturnBytes() do ;
  for F1{ TODO: guesstype:byte}                    in TReturnBytes(nil)() do ;

  for B1{guesstype:byte}                    in TByteSet do ;
  for B2{guesstype:ansichar}                in TSysCharSet do ;
  for B3{guesstype:TEnum}                   in TSet1 do ;
  for B4{guesstype:low(TSet2)..high(TSet2)} in TSet2 do ;
  for B5{guesstype:integer}                 in TSet3 do ;
  for B6{ TODO guesstype:TEnumRange}              in TSet4 do ;
  for B7{guesstype:TEnum}                   in TSet5 do ;
  for B8{guesstype:TEnum}                   in TEnum do ;
  for B9{guesstype:TEnum}                   in TEnumRange do ; // TODO: guess TEnumRange

  for C1{guesstype:byte}                    := low(TByteSet) to high(TByteSet) do ;
  for C3{guesstype:TEnum}                   := low(TSet1) to high(TSet1) do ;
  for C4{guesstype:low(TSet2)..high(TSet2)} := low(TSet2) to high(TSet2) do ;
  for C8{guesstype:TEnum}                   := low(TEnum) to high(TEnum) do ;
  for C9{guesstype:TEnum}                   := low(TEnumRange) to high(TEnumRange) do ;  // TODO: guess TEnumRange
  for CA{guesstype:TEnum}                   := e1 to e2 do ;

end.

